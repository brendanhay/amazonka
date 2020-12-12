{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.AWS.Data.Time
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Network.AWS.Data.Time
  ( -- * Time
    Time (..),
    _Time,
    toTimestamp,
    fromTimestamp,

    -- * Formats
    basicFormat,
    awsFormat,
    iso8601Format,
    rfc822Format,

    -- * ISO8601 date time
    UTCTime,
    DateTime,
    formatDateTime,
    parseDateTime,

    -- * UNIX timestamps
    POSIXTime,
    Timestamp,
    formatTimestamp,
    parseTimestamp,
  )
where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.Text as AText
import qualified Data.ByteString.Char8 as BS
import Data.Hashable (Hashable (hashWithSalt))
import qualified Data.Text as Text
import Data.Time (Day (..), UTCTime (..))
import qualified Data.Time as Time
import Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.Time.Clock.POSIX as Time.POSIX
import GHC.Generics (Generic)
import Network.AWS.Data.ByteString
import Network.AWS.Data.JSON
import Network.AWS.Data.Query
import Network.AWS.Data.Text
import Network.AWS.Data.XML
import Network.AWS.Lens (Iso', iso)

newtype Time a = Time {fromTime :: a}
  deriving (Show, Read, Eq, Ord, Generic, NFData)

_Time :: Iso' (Time a) a
_Time = iso fromTime Time

toTimestamp :: DateTime -> Timestamp
toTimestamp = Time . Time.POSIX.utcTimeToPOSIXSeconds . fromTime

fromTimestamp :: Timestamp -> DateTime
fromTimestamp = Time . Time.POSIX.posixSecondsToUTCTime . fromTime

-- IS8601 DateTimes

type DateTime = Time UTCTime

instance Hashable DateTime where
  hashWithSalt salt (Time (UTCTime (ModifiedJulianDay day) time)) =
    salt `hashWithSalt` day `hashWithSalt` toRational time

basicFormat, awsFormat, iso8601Format, rfc822Format :: String
basicFormat = "%Y%m%d"
awsFormat = "%Y%m%dT%H%M%SZ"
iso8601Format = Time.iso8601DateFormat (Just "%XZ")
rfc822Format = "%a, %d %b %Y %H:%M:%S GMT"

formatDateTime :: String -> DateTime -> String
formatDateTime format = Time.formatTime Time.defaultTimeLocale format . fromTime

parseDateTime :: String -> Text -> Either String DateTime
parseDateTime format text = fmap Time parse
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

instance ToText DateTime where
  toText = Text.pack . formatDateTime iso8601Format

instance FromText DateTime where
  fromText = parseDateTime iso8601Format

instance ToByteString DateTime where
  toBS = BS.pack . formatDateTime iso8601Format

instance ToQuery DateTime where
  toQuery = toQuery . toBS

instance ToXML DateTime where
  toXML = toXMLText

instance FromXML DateTime where
  parseXML = parseXMLText "DateTime"

instance ToJSON DateTime where
  toJSON = toJSONText

instance FromJSON DateTime where
  parseJSON = parseJSONText "DateTime"

-- UNIX Timestamps

type Timestamp = Time POSIXTime

instance Hashable Timestamp where
  hashWithSalt salt time =
    salt `hashWithSalt` formatTimestamp time

formatTimestamp :: Timestamp -> Integer
formatTimestamp = floor . Time.nominalDiffTimeToSeconds . fromTime

parseTimestamp :: Text -> Either String Timestamp
parseTimestamp =
  let parser = Time . fromInteger . floor <$> AText.scientific
   in parseText
        ( parser <* AText.endOfInput
            <|> fail "Failure parsing unix timestamp"
        )

instance ToText Timestamp where
  toText = toText . formatTimestamp

instance FromText Timestamp where
  fromText = parseTimestamp

instance ToQuery Timestamp where
  toQuery = toQuery . formatTimestamp 

instance ToJSON Timestamp where
  toJSON = Aeson.toJSON . formatTimestamp

-- This is a somewhat unfortunate hack to support the bizzare apigateway
-- occurence of returning DateTime or Timestamp in unknown scenarios.
--
-- See: https://github.com/brendanhay/amazonka/issues/291
instance FromJSON Timestamp where
  parseJSON = \case
      Aeson.String s ->
        toTimestamp <$> Aeson.parseJSON (Aeson.String s)
      Aeson.Number n ->
        pure (Time (fromInteger (floor n)))
      _other ->
        fail "Failure parsing unix timestamp from non-string or non-number"
