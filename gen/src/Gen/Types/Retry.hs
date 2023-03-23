{-# LANGUAGE TemplateHaskell #-}

module Gen.Types.Retry where

import qualified Control.Lens as Lens
import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Data.Text as Text
import GHC.Generics ()
import Gen.Prelude
import Gen.Text

defKey :: IsString a => a
defKey = "__default__"

data When
  = WhenStatus (Maybe Text) !Integer
  | WhenCRC32 !Text
  deriving (Eq, Show)

instance FromJSON When where
  parseJSON = Aeson.withObject "When" (\o -> status o <|> crc o)
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
  parseJSON = Aeson.withObject "Policy" $ \o -> sock o <|> resp o
    where
      sock o = Socket <$> (o .: "applies_when" >>= (.: "socket_errors"))
      resp o = When <$> (o .: "applies_when" >>= (.: "response"))

data Delay = Delay
  { _delayType :: Text,
    _delayBase :: Rational,
    _delayGrowth :: Integer
  }
  deriving (Eq, Show, Generic)

$(Lens.makeClassy ''Delay)

instance FromJSON Delay where
  parseJSON =
    Aeson.withObject "Delay" $ \o ->
      Delay <$> upperHead <$> o .: "type"
        <*> (o .: "base" >>= base)
        <*> o .: "growth_factor"
    where
      base = \case
        Aeson.String "rand" -> pure 0.05
        o -> Aeson.parseJSON o

data Retry = Retry'
  { _retryAttempts :: Integer,
    _retryDelay :: Delay,
    _retryPolicies :: HashMap Text Policy
  }
  deriving (Eq, Show)

$(Lens.makeLenses ''Retry)

instance HasDelay Retry where
  delay = retryDelay

instance FromJSON Retry where
  parseJSON =
    Aeson.withObject "default_retry" $ \o ->
      Retry'
        <$> o .: "max_attempts"
        <*> o .: "delay"
        <*> pure mempty

instance FromJSON (Retry -> Retry) where
  parseJSON =
    Aeson.withObject "retry" $ \o -> do
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

parseRetry :: Text -> Aeson.Object -> Aeson.Types.Parser Retry
parseRetry svc o = do
  p <- o .: "definitions" :: Aeson.Types.Parser (HashMap Text Policy)
  r <- o .: "retry" :: Aeson.Types.Parser (HashMap Text Aeson.Object)

  -- Since the __default__ policy is everything in
  -- definitions, just add them all rather than dealing
  -- with references.
  case r ^. Lens.at defKey of
    Nothing -> fail $ "Missing object key: " ++ defKey
    Just x -> do
      Identity d <- Aeson.parseJSON (Aeson.Object x)
      case r ^. Lens.at (Text.toLower svc) of
        Nothing -> pure (d & retryPolicies .~ p)
        Just y -> do
          z <- Aeson.parseJSON (Aeson.Object y)
          pure $! z (d & retryPolicies .~ p)
