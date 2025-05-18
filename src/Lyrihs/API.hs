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
  publish',
  solveChallenge,
  publish,
  PublishResponse (..),
  PublishRequest (..),
  runAPI,
) where

import Control.Lens ((&), (&~), (.=), (.~), (^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Crypto.Hash.SHA256 (hash)
import Data.Aeson qualified as A
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy (ByteString)
import Data.Either (fromRight)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Network.Wreq
import Prelude hiding (id)

type Url = String
type API = ReaderT Url IO

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
  deriving (Generic)

instance A.ToJSON PublishRequest where
  toJSON PublishRequest{pName, pArtist, pAlbum, pDuration, pLyrics, pSyncedLyrics} =
    A.object
      [ "trackName" A..= pName
      , "artistName" A..= pArtist
      , "albumName" A..= pAlbum
      , "duration" A..= pDuration
      , "plainLyrics" A..= pLyrics
      , "syncedLyrics" A..= pSyncedLyrics
      ]

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

getUrl :: String -> Text -> Text -> Text -> Integer -> API GetResponse
getUrl url track artist album duration = do
  apiUrl <- ask
  resp <- liftIO $ getWith opts $ apiUrl <> url
  case resp ^. responseStatus . statusCode of
    404 -> pure NotFound
    200 -> pure $ OK $ decode (resp ^. responseBody)
    _ -> error "Unexpected status code on get endpoint"
 where
  opts =
    defaults &~ do
      param "track_name" .= [track]
      param "artist_name" .= [artist]
      param "album_name" .= [album]
      param "duration" .= [T.pack $ show duration]

getLyrics, getCachedLyrics :: Text -> Text -> Text -> Integer -> API GetResponse
getLyrics = getUrl "/get"
getCachedLyrics = getUrl "/get-cached"

getLyricsById :: Integer -> API GetResponse
getLyricsById id' = do
  url <- ask
  resp <- liftIO $ get $ url <> "/get/" <> show id'
  case resp ^. responseStatus . statusCode of
    404 -> pure NotFound
    200 -> pure $ OK $ decode (resp ^. responseBody)
    _ -> error "Unexpected status code on get endpoint"

searchLyrics :: SearchQuery -> API SearchResponse
searchLyrics q = do
  url <- ask
  resp <- liftIO $ getWith opts $ url <> "/search"
  pure $ decode (resp ^. responseBody)
 where
  opts =
    defaults &~ case q of
      TextQuery t -> param "q" .= [t]
      TrackQuery{qName, qArtist, qAlbum} -> do
        param "track_name" .= [qName]
        param "artist_name" .= toList qArtist
        param "album_name" .= toList qAlbum

requestChallenge :: API Challenge
requestChallenge = do
  url <- ask
  resp <- liftIO $ post (url <> "/request-challenge") $ A.toJSON ()
  pure $ decode (resp ^. responseBody)

publish' :: Text -> PublishRequest -> API PublishResponse
publish' token request = do
  url <- ask
  res <- liftIO $ postWith opts (url <> "/publish") body
  case res ^. responseStatus . statusCode of
    400 -> pure IncorrectToken
    201 -> pure PublishOK
    _ -> error "Unexpected status code on publish endpoint"
 where
  body = A.toJSON request
  opts = defaults & header "X-Publish-Token" .~ [encodeUtf8 token]

solveChallenge :: Challenge -> Text
solveChallenge Challenge{prefix, target} = go 0
 where
  prefix' = encodeUtf8 prefix
  target' = fromRight (error "Can't decode target from crypto-challenge") $ B16.decode $ encodeUtf8 target
  go :: Integer -> Text
  go n | hash (prefix' <> BC.pack (show n)) < target' = prefix <> ":" <> T.pack (show n)
  go n = go (n + 1)

publish :: PublishRequest -> API PublishResponse
publish r = do
  c <- requestChallenge
  publish' (solveChallenge c) r

runAPI :: Url -> ReaderT Url IO a -> IO a
runAPI url f = runReaderT f url
