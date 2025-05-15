{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lyrihs.API (
  getLyrics,
  getCachedLyrics,
  getLyricsById,
  searchLyrics,
  GetResponse (..),
  TrackData (..),
  SearchQuery (..),
) where

import Control.Lens ((&), (.~), (^.))
import Data.Aeson qualified as A
import Data.ByteString (toStrict)
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics (Generic)
import Network.Wreq
import Prelude hiding (id)
import Prelude qualified (id)

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

getUrl :: String -> Text -> Text -> Text -> Integer -> IO GetResponse
getUrl url track artist album duration = do
  resp <- getWith opts url
  case resp ^. responseStatus . statusCode of
    404 -> pure NotFound
    200 ->
      pure $
        OK $
          fromMaybe (error "Incorrect reply from server on get endpoint") $
            A.decodeStrictText $
              decodeUtf8 $
                toStrict (resp ^. responseBody)
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

decode :: (A.FromJSON a) => ByteString -> a
decode =
  fromMaybe (error "Incorrect reply from server")
    . A.decodeStrictText
    . decodeUtf8
    . toStrict

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
