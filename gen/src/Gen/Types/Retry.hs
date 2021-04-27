{-# LANGUAGE TemplateHaskell #-}

-- Module      : Gen.Types.Retry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla xtPublic License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)

module Gen.Types.Retry where

import Control.Applicative
import Control.Lens
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics
import Gen.Text
import Gen.Types.Map

defKey :: Text
defKey = "__default__"

data When
  = WhenStatus (Maybe Text) !Integer
  | WhenCRC32 !Text
  deriving (Eq, Show)

instance FromJSON When where
  parseJSON = withObject "when" (\o -> status o <|> crc o)
    where
      status o =
        WhenStatus
          <$> o .:? "service_error_code"
          <*> o .: "http_status_code"

      crc = fmap WhenCRC32 . (.: "crc32body")

data Policy
  = Socket [Text]
  | When When
  deriving (Eq, Show)

instance FromJSON Policy where
  parseJSON = withObject "ref" $ \o -> sock o <|> resp o
    where
      sock o = Socket <$> (o .: "applies_when" >>= (.: "socket_errors"))
      resp o = When <$> (o .: "applies_when" >>= (.: "response"))

data Delay = Delay
  { _delayType :: !Text,
    _delayBase :: !Rational,
    _delayGrowth :: !Integer
  }
  deriving (Eq, Show, Generic)

makeClassy ''Delay

instance FromJSON Delay where
  parseJSON = withObject "delay" $ \o ->
    Delay <$> upperHead <$> o .: "type"
      <*> (o .: "base" >>= base)
      <*> o .: "growth_factor"
    where
      base = \case
        String "rand" -> pure 0.05
        o -> parseJSON o

data Retry = Retry'
  { _retryAttempts :: !Integer,
    _retryDelay :: !Delay,
    _retryPolicies :: Map Text Policy
  }
  deriving (Eq, Show)

makeLenses ''Retry

instance HasDelay Retry where
  delay = retryDelay

instance FromJSON Retry where
  parseJSON = withObject "default_retry" $ \o ->
    Retry' <$> o .: "max_attempts"
      <*> o .: "delay"
      <*> pure mempty

instance FromJSON (Retry -> Retry) where
  parseJSON = withObject "retry" $ \o -> do
    m <- o .:? "max_attempts"
    d <- o .:? "delay"
    -- FIXME: Currently simply ignoring non '__default__' keys.
    p <-
      (o .: defKey <|> pure mempty)
        >>= (\o' -> o' .: "policies" <|> pure mempty)
    return $ \r ->
      Retry'
        (fromMaybe (r ^. retryAttempts) m)
        (fromMaybe (r ^. retryDelay) d)
        (r ^. retryPolicies <> p)

parseRetry :: Text -> Object -> Parser Retry
parseRetry svc o = do
  p <- o .: "definitions" :: Parser (Map Text Policy)
  r <- o .: "retry" :: Parser (Map Text Object)
  -- Since the __default__ policy is everything in
  -- definitions, just add them all rather than dealing
  -- with references.
  case r ^. at defKey of
    Nothing -> fail $ "Missing: " ++ show defKey
    Just x -> do
      Identity d <- parseJSON (Object x)
      case r ^. at (Text.toLower svc) of
        Nothing -> pure (d & retryPolicies .~ p)
        Just y -> do
          z <- parseJSON (Object y)
          return $! z (d & retryPolicies .~ p)
