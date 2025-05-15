{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lyrihs.API (
  getLyrics,
  getCachedLyrics,
  getLyricsById,
  searchLyrics,
  requestChallenge,
  Challenge (..),
  GetResponse (..),
  TrackData (..),
  SearchQuery (..),
  publish,
  PublishResponse (..),
  PublishRequest (..),
) where

import Control.Lens ((&), (.~), (^.))
import Data.Aeson qualified as A
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Network.Wreq
import Prelude hiding (id)
import Prelude qualified (id)

data Challenge
  = Challenge
  { prefix :: Text
  , target :: Text
  }
  deriving (Generic, A.FromJSON)

data TrackData
  = TrackData
  { id :: Integer
  , trackName :: Text
  , artistName :: Text
  , albumName :: Text
  , duration :: Integer
  , instrumental :: Bool
  , plainLyrics :: Maybe Text
  , syncedLyrics :: Maybe Text
  }
  deriving (Generic, A.FromJSON)

data PublishRequest
  = PublishRequest
  { pName :: Text
  , pArtist :: Text
  , pAlbum :: Text
  , pDuration :: Integer
  , pLyrics :: Maybe Text
  , pSyncedLyrics :: Maybe Text
  }
  deriving (Generic, A.ToJSON)

data PublishResponse = PublishOK | IncorrectToken

instance Show TrackData where
  show TrackData{id, trackName, artistName, albumName} =
    T.unpack $
      T.concat
        [ "Track, id: "
        , T.pack $ show id
        , ", name: "
        , trackName
        , ", artist: "
        , artistName
        , ", album: "
        , albumName
        ]

data GetResponse
  = NotFound
  | OK TrackData
  deriving (Generic, A.FromJSON, Show)

data SearchQuery
  = TextQuery Text
  | TrackQuery
      { qName :: Text
      , qArtist :: Maybe Text
      , qAlbum :: Maybe Text
      }

type SearchResponse = [TrackData]

decode :: (A.FromJSON a) => ByteString -> a
decode =
  fromMaybe (error "Incorrect reply from server")
    . A.decode

getUrl :: String -> Text -> Text -> Text -> Integer -> IO GetResponse
getUrl url track artist album duration = do
  resp <- getWith opts url
  case resp ^. responseStatus . statusCode of
    404 -> pure NotFound
    200 -> pure $ OK $ decode (resp ^. responseBody)
    _ -> error "Unexpected status code on get endpoint"
 where
  opts =
    defaults
      & param "track_name" .~ [track]
      & param "artist_name" .~ [artist]
      & param "album_name" .~ [album]
      & param "duration" .~ [T.pack $ show duration]

getLyrics, getCachedLyrics :: Text -> Text -> Text -> Integer -> IO GetResponse
getLyrics = getUrl "http://lrclib.net/api/get"
getCachedLyrics = getUrl "http://lrclib.net/api/get-cached"

getLyricsById :: Integer -> IO GetResponse
getLyricsById id' = do
  resp <- get $ "http://lrclib.net/api/get/" ++ show id'
  case resp ^. responseStatus . statusCode of
    404 -> pure NotFound
    200 -> pure $ OK $ decode (resp ^. responseBody)
    _ -> error "Unexpected status code on get endpoint"

maybeParam :: Text -> Maybe Text -> Options -> Options
maybeParam name (Just arg) = param name .~ [arg]
maybeParam _ Nothing = Prelude.id

searchLyrics :: SearchQuery -> IO SearchResponse
searchLyrics q = do
  resp <- getWith opts "http://lrclib.net/api/search"
  pure $ decode (resp ^. responseBody)
 where
  opts =
    defaults & case q of
      TextQuery t -> param "q" .~ [t]
      TrackQuery{qName, qArtist, qAlbum} ->
        (param "track_name" .~ [qName])
          . maybeParam "artist_name" qArtist
          . maybeParam "album_name" qAlbum

requestChallenge :: IO Challenge
requestChallenge = do
  resp <- post "http://lrclib.net/api/search" $ A.toJSON ()
  pure $ decode (resp ^. responseBody)

publish :: Text -> PublishRequest -> IO PublishResponse
publish token request = do
  let body = A.encode request
  res <- postWith opts "http://lrclib.net/api/publish" body
  case res ^. responseStatus . statusCode of
    400 -> pure IncorrectToken
    201 -> pure PublishOK
    _ -> error "Unexpected status code on publish endpoint"
 where
  opts = defaults & header "X-Publish-Token" .~ [encodeUtf8 token]
