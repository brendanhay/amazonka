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
    UTCTime,
    NominalDiffTime,
    -- Time (..),
    -- _Time,

    -- * Formats
    basicFormat,
    awsFormat,
    iso8601Format,
    rfc822Format,

    -- * ISO8601 date time
    formatDateTime,
    parseDateTime,

    -- * UNIX timestamps
    formatTimestamp,
    formatTimestamp,
  )
where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.Text as AText
import qualified Data.ByteString.Char8 as BS
import Data.Hashable (Hashable (hashWithSalt))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (Day (..), NominalDiffTime, UTCTime (..))
import qualified Data.Time as Time
import qualified Data.Time.Clock.POSIX as Time.POSIX
import GHC.Generics (Generic)
import Network.AWS.Lens (Iso', iso)

-- newtype Time a = Time {fromTime :: a}
--   deriving (Show, Read, Eq, Ord, Generic, NFData)

-- _Time :: Iso' (Time a) a
-- _Time = iso fromTime Time

-- toTimestamp :: DateTime -> Timestamp
-- toTimestamp = Time . Time.POSIX.utcTimeToPOSIXSeconds . fromTime

-- IS8601 DateTimes

-- type DateTime = Time UTCTime

-- instance Hashable DateTime where
--   hashWithSalt salt (Time (UTCTime (ModifiedJulianDay day) time)) =
--     salt `hashWithSalt` day `hashWithSalt` toRational time

basicFormat, awsFormat, iso8601Format, rfc822Format :: String
basicFormat = "%Y%m%d"
awsFormat = "%Y%m%dT%H%M%SZ"
iso8601Format = "%Y-%m-%dT%XZ"
rfc822Format = "%a, %d %b %Y %H:%M:%S GMT"

formatDateTime :: String -> UTCTime -> String
formatDateTime format = Time.formatTime Time.defaultTimeLocale format

parseDateTime :: String -> String -> Either String UTCTime
parseDateTime format string =
  case Time.parseTimeM True Time.defaultTimeLocale format string of
    Just x -> Right x
    Nothing ->
      Left $
        "Unable to parse time format "
          ++ show format
          ++ " from "
          ++ show string

-- instance ToText DateTime where
--   toText = Text.pack . formatDateTime iso8601Format

-- instance FromText DateTime where
--   fromText = parseDateTime iso8601Format

-- instance ToByteString DateTime where
--   toBS = BS.pack . formatDateTime iso8601Format

-- instance ToQuery DateTime where
--   toQuery = toQuery . toBS

-- instance ToXML DateTime where
--   toXML = toXMLText

-- instance FromXML DateTime where
--   parseXML = parseXMLText "DateTime"

-- instance ToJSON DateTime where
--   toJSON = toJSONText

-- instance FromJSON DateTime where
--   parseJSON = parseJSONText "DateTime"

-- UNIX Timestamps

-- type Timestamp = Time NominalDiffTime

-- instance Hashable Timestamp where
--   hashWithSalt salt time =
--     salt `hashWithSalt` formatTimestamp time

formatTimestamp :: NominalDiffTime -> Integer
formatTimestamp = floor . Time.nominalDiffTimeToSeconds

parseTimestamp :: Text -> Either String NominalDiffTime
parseTimestamp =
  let parser = fromInteger . floor <$> AText.scientific
   in AText.parseOnly
        ( parser <* AText.endOfInput
            <|> fail "Failure parsing unix timestamp"
        )

-- instance ToText Timestamp where
--   toText = toText . formatTimestamp

-- instance FromText Timestamp where
--   fromText = parseTimestamp

-- instance ToQuery Timestamp where
--   toQuery = toQuery . formatTimestamp

-- instance ToJSON Timestamp where
--   toJSON = Aeson.toJSON . formatTimestamp

-- -- This is a somewhat unfortunate hack to support the bizzare apigateway
-- -- occurence of returning DateTime or Timestamp in unknown scenarios.
-- --
-- -- See: https://github.com/brendanhay/amazonka/issues/291
-- instance FromJSON Timestamp where
--   parseJSON = \case
--     Aeson.String s ->
--       toTimestamp <$> Aeson.parseJSON (Aeson.String s)
--     Aeson.Number n ->
--       pure (Time (fromInteger (floor n)))
--     _other ->
--       fail "Failure parsing unix timestamp from non-string or non-number"
