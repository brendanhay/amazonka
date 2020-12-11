{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Network.AWS.Data.Time
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Network.AWS.Data.Time
  ( -- * Time
    TimeFormat (..),
    Time (..),
    _Time,

    -- * De/serialisation
    parseTime,
    formatTime,

    -- ** Formats
    ISO8601,
    Timestamp,
    UTCTime,
  )
where

import Control.Applicative ((<|>))
import Control.DeepSeq
import Data.Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Attoparsec.Text as AText
import qualified Data.ByteString.Char8 as BS
import Data.Hashable
import Data.Scientific
import qualified Data.Text as Text
import Data.Time (Day (..), UTCTime (..))
import qualified Data.Time as Time
import qualified Data.Time.Clock.POSIX as Time.POSIX
import GHC.Generics (Generic)
import Network.AWS.Data.ByteString
import Network.AWS.Data.JSON
import Network.AWS.Data.Query
import Network.AWS.Data.Text
import Network.AWS.Data.XML
import Network.AWS.Lens (Iso', iso)

data TimeFormat
  = ISO8601
  | UNIX
  deriving (Eq, Read, Show, Generic)

newtype Time (a :: TimeFormat) = Time {fromTime :: UTCTime}
  deriving (Show, Read, Eq, Ord, Generic, NFData)

instance Hashable (Time a) where
  hashWithSalt salt (Time (UTCTime (ModifiedJulianDay d) t)) =
    salt `hashWithSalt` d
      `hashWithSalt` toRational t

_Time :: Iso' (Time a) UTCTime
_Time = iso fromTime Time

convert :: Time a -> Time b
convert = Time . fromTime

type ISO8601 = Time 'ISO8601

iso8601Format :: String
iso8601Format = Time.iso8601DateFormat (Just "%XZ")

type Timestamp = Time 'UNIX

instance FromText ISO8601 where
  fromText = parseTime iso8601Format

instance FromText Timestamp where
  fromText = parseTime iso8601Format

parseTime :: String -> Text -> Either String (Time a)
parseTime format text =
  parseTimestamp text <|> fmap Time parse
  where
    string = Text.unpack text
    parse =
      case Time.parseTimeM True Time.defaultTimeLocale format string of
        Just x -> Right x
        Nothing ->
          Left $
            "Unable to parse Time format "
              ++ show format
              ++ " from "
              ++ show string

parseTimestamp :: Text -> Either String (Time a)
parseTimestamp =
  parseText $
    fmap
      (Time . Time.POSIX.posixSecondsToUTCTime . realToFrac)
      (AText.double <* AText.endOfInput)
      <|> fail "Failure parsing Unix Timestamp"

instance ToText ISO8601 where
  toText = Text.pack . formatTime iso8601Format

instance ToText Timestamp where
  toText (Time t) =
    toText (truncate (Time.POSIX.utcTimeToPOSIXSeconds t) :: Integer)

formatTime :: String -> Time a -> String
formatTime format = Time.formatTime Time.defaultTimeLocale format . fromTime

instance FromXML ISO8601 where
  parseXML = parseXMLText "ISO8601"

instance FromJSON ISO8601 where
  parseJSON = parseJSONText "ISO8601"

-- This is a somewhat unfortunate hack to support the bizzare apigateway
-- occurence of returning ISO8601 or Timestamp timestamps in unknown scenarios.
--
-- See: https://github.com/brendanhay/amazonka/issues/291
instance FromJSON Timestamp where
  parseJSON value =
    fmap convert (string value) <|> number value
    where
      string :: Value -> Aeson.Parser ISO8601
      string = parseJSON

      number :: Value -> Aeson.Parser Timestamp
      number =
        withScientific "Timestamp" $
          pure
            . Time
            . Time.POSIX.posixSecondsToUTCTime
            . realToFrac

instance ToByteString ISO8601 where
  toBS = BS.pack . formatTime iso8601Format

instance ToQuery ISO8601 where
  toQuery = toQuery . toBS

instance ToQuery Timestamp where
  toQuery (Time t) =
    toQuery (truncate (Time.POSIX.utcTimeToPOSIXSeconds t) :: Integer)

instance ToXML ISO8601 where
  toXML = toXMLText

instance ToJSON ISO8601 where
  toJSON = toJSONText

instance ToJSON Timestamp where
  toJSON (Time t) =
    Number $
      scientific (truncate (Time.POSIX.utcTimeToPOSIXSeconds t) :: Integer) 0
