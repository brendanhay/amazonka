{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Gen.Types.Retry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : This Source Code Form is subject Lens.to the terms of
--               the Mozilla xtPublic License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Gen.Types.Retry where

import qualified Control.Lens as Lens
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Value (..), (.:), (.:?))
import qualified Data.Aeson.Types as Aeson.Types
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Manipulate as Manipulate
import qualified GHC.Generics as Generics
import Gen.Prelude
import Gen.Types.Map

defKey :: Text
defKey = "__default__"

data When
  = WhenStatus (Maybe Text) !Integer
  | WhenCRC32 !Text
  deriving stock (Eq, Show)

instance FromJSON When where
  parseJSON = Aeson.withObject "when" (\o -> status o <|> crc o)
    where
      status o =
        WhenStatus
          <$> o .:? "service_error_code"
          <*> o .: "http_status_code"

      crc = fmap WhenCRC32 . (.: "crc32body")

data Policy
  = Socket [Text]
  | When When
  deriving stock (Eq, Show)

instance FromJSON Policy where
  parseJSON = Aeson.withObject "ref" $ \o -> sock o <|> resp o
    where
      sock o = Socket <$> (o .: "applies_when" >>= (.: "socket_errors"))
      resp o = When <$> (o .: "applies_when" >>= (.: "response"))

data Delay = Delay
  { _delayType :: !Text,
    _delayBase :: !Rational,
    _delayGrowth :: !Integer
  }
  deriving stock (Eq, Show, Generic)

$(Lens.makeClassy ''Delay)

instance FromJSON Delay where
  parseJSON = Aeson.withObject "delay" $ \o ->
    Delay <$> Manipulate.upperHead <$> o .: "type"
      <*> (o .: "base" >>= base)
      <*> o .: "growth_factor"
    where
      base = \case
        String "rand" -> pure 0.05
        o -> Aeson.parseJSON o

data Retry = Retry'
  { _retryAttempts :: !Integer,
    _retryDelay :: !Delay,
    _retryPolicies :: HashMap Text Policy
  }
  deriving stock (Eq, Show)

$(Lens.makeLenses ''Retry)

instance HasDelay Retry where
  delay = retryDelay

instance FromJSON Retry where
  parseJSON = Aeson.withObject "default_retry" $ \o ->
    Retry' <$> o .: "max_attempts"
      <*> o .: "delay"
      <*> pure mempty

instance FromJSON (Retry -> Retry) where
  parseJSON = Aeson.withObject "retry" $ \o -> do
    m <- o .:? "max_attempts"
    d <- o .:? "delay"
    -- FIXME: Currently simply ignoring non '__default__' keys.
    p <-
      (o .: defKey <|> pure mempty)
        >>= (\o' -> o' .: "policies" <|> pure mempty)
    pure $ \r ->
      Retry'
        (fromMaybe (r ^. retryAttempts) m)
        (fromMaybe (r ^. retryDelay) d)
        (r ^. retryPolicies <> p)

parseRetry :: Text -> Aeson.Types.Object -> Aeson.Types.Parser Retry
parseRetry svc o = do
  p <- o .: "definitions" :: Aeson.Types.Parser (HashMap Text Policy)
  r <- o .: "retry" :: Aeson.Types.Parser (HashMap Text Aeson.Types.Object)
  -- Since the __default__ policy is everything in
  -- definitions, just add them all rather than dealing
  -- with references.
  case r ^. Lens.at defKey of
    Nothing -> fail $ "Missing: " ++ show defKey
    Just x -> do
      Identity d <- Aeson.parseJSON (Object x)

      case r ^. Lens.at (Text.toLower svc) of
        Nothing -> pure (d & retryPolicies .~ p)
        Just y -> do
          z <- Aeson.parseJSON (Object y)
          pure $! z (d & retryPolicies .~ p)
