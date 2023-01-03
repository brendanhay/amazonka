-- |
-- Module      : Amazonka.Data.Time
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Amazonka.Data.Time
  ( -- * Time
    Format (..),
    Time (..),
    _Time,

    -- ** Formats
    UTCTime,
    RFC822,
    ISO8601,
    BasicTime,
    AWSTime,
    POSIX,
  )
where

import Amazonka.Core.Lens.Internal (iso)
import Amazonka.Data.ByteString
import Amazonka.Data.JSON
import Amazonka.Data.Query
import Amazonka.Data.Text
import Amazonka.Data.XML
import Amazonka.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Attoparsec.Text as A
import qualified Data.Attoparsec.Text as AText
import qualified Data.ByteString.Char8 as BS
import qualified Data.Scientific as Scientific
import qualified Data.Text as Text
import qualified Data.Time as Time
import Data.Time.Clock.POSIX
import Data.Time.Format (defaultTimeLocale, formatTime, iso8601DateFormat)

data Format
  = RFC822Format
  | ISO8601Format
  | BasicFormat
  | AWSFormat
  | POSIXFormat
  deriving stock (Eq, Read, Show, Generic)

newtype Time (a :: Format) = Time {fromTime :: UTCTime}
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving newtype (NFData)

instance Hashable (Time a) where
  hashWithSalt salt (Time (Time.UTCTime (Time.ModifiedJulianDay d) t)) =
    salt
      `hashWithSalt` d
      `hashWithSalt` toRational t

_Time :: Iso' (Time a) UTCTime
_Time = iso fromTime Time

convert :: Time a -> Time b
convert = Time . fromTime

type RFC822 = Time 'RFC822Format

type ISO8601 = Time 'ISO8601Format

type BasicTime = Time 'BasicFormat

type AWSTime = Time 'AWSFormat

type POSIX = Time 'POSIXFormat

class TimeFormat a where
  format :: proxy a -> String

instance TimeFormat RFC822 where
  format _ = "%a, %d %b %Y %H:%M:%S %Z"

instance TimeFormat ISO8601 where
  format _ = iso8601DateFormat (Just "%XZ")

instance TimeFormat BasicTime where
  format _ = "%Y%m%d"

instance TimeFormat AWSTime where
  format _ = "%Y%m%dT%H%M%SZ"

instance FromText (Time fmt) where
  fromText = A.parseOnly ((parseUnixTimestamp <|> parseFormattedTime) <* A.endOfInput)

parseFormattedTime :: A.Parser (Time a)
parseFormattedTime = do
  s <- Text.unpack <$> AText.takeText

  let parse :: String -> A.Parser (Time a)
      parse fmt =
        case Time.parseTimeM True defaultTimeLocale fmt s of
          Just x -> pure (Time x)
          Nothing ->
            fail
              ( "Unable to parse Time format "
                  ++ show fmt
                  ++ " from "
                  ++ show s
              )

  parse (format (Proxy @RFC822))
    <|> parse (format (Proxy @ISO8601))
    <|> parse (format (Proxy @BasicTime))
    <|> parse (format (Proxy @AWSTime))
    -- Deprecated ISO8601 format exhibited in the AWS-supplied examples.
    <|> parse "%FT%X%Q%Z"
    -- Exhaustive Failure
    <|> fail ("Failure parsing Time from value: " ++ show s)

parseUnixTimestamp :: A.Parser (Time a)
parseUnixTimestamp =
  Time . posixSecondsToUTCTime . realToFrac
    <$> AText.double
    <* AText.endOfInput
    <|> fail "Failure parsing Unix Timestamp"

instance ToText RFC822 where
  toText = Text.pack . renderFormattedTime

instance ToText ISO8601 where
  toText = Text.pack . renderFormattedTime

instance ToText BasicTime where
  toText = Text.pack . renderFormattedTime

instance ToText AWSTime where
  toText = Text.pack . renderFormattedTime

instance ToText POSIX where
  toText (Time t) = toText (truncate (utcTimeToPOSIXSeconds t) :: Integer)

renderFormattedTime :: forall a. TimeFormat (Time a) => Time a -> String
renderFormattedTime (Time t) =
  formatTime
    defaultTimeLocale
    (format (Proxy @(Time a)))
    -- Convert `t` to a GMT `ZonedTime`, because otherwise the
    -- `FormatTime` instance for `UTCTime` converts to UTC `ZonedTime`
    -- for us. While they are the same offset, a UTC `ZonedTime` emits
    -- `UTC` instead of `GMT` when formatted by `RFC822`'s
    -- `TimeFormat`, which is not a valid `zone` in RFC 822's grammar.
    (Time.utcToZonedTime (read "GMT") t)

instance FromXML RFC822 where
  parseXML = parseXMLText "RFC822"

instance FromXML ISO8601 where
  parseXML = parseXMLText "ISO8601"

instance FromXML AWSTime where
  parseXML = parseXMLText "AWSTime"

instance FromXML BasicTime where
  parseXML = parseXMLText "BasicTime"

instance FromJSON RFC822 where
  parseJSON = parseJSONText "RFC822"

instance FromJSON ISO8601 where
  parseJSON = parseJSONText "ISO8601"

instance FromJSON AWSTime where
  parseJSON = parseJSONText "AWSTime"

instance FromJSON BasicTime where
  parseJSON = parseJSONText "BasicTime"

-- This is a somewhat unfortunate hack to support the bizzare apigateway
-- occurence of returning ISO8601 or POSIX timestamps in unknown scenarios.
--
-- See: https://github.com/brendanhay/amazonka/issues/291
instance FromJSON POSIX where
  parseJSON o = fmap convert (str o) <|> num o
    where
      str :: Value -> Aeson.Parser ISO8601
      str = parseJSON

      num :: Value -> Aeson.Parser POSIX
      num =
        Aeson.withScientific
          "POSIX"
          ( pure
              . Time
              . posixSecondsToUTCTime
              . realToFrac
          )

instance ToByteString RFC822 where
  toBS = BS.pack . renderFormattedTime

instance ToByteString ISO8601 where
  toBS = BS.pack . renderFormattedTime

instance ToByteString BasicTime where
  toBS = BS.pack . renderFormattedTime

instance ToByteString AWSTime where
  toBS = BS.pack . renderFormattedTime

instance ToQuery RFC822 where
  toQuery = toQuery . toBS

instance ToQuery ISO8601 where
  toQuery = toQuery . toBS

instance ToQuery BasicTime where
  toQuery = toQuery . toBS

instance ToQuery AWSTime where
  toQuery = toQuery . toBS

instance ToQuery POSIX where
  toQuery (Time t) = toQuery (truncate (utcTimeToPOSIXSeconds t) :: Integer)

instance ToXML RFC822 where
  toXML = toXMLText

instance ToXML ISO8601 where
  toXML = toXMLText

instance ToXML AWSTime where
  toXML = toXMLText

instance ToXML BasicTime where
  toXML = toXMLText

instance ToJSON RFC822 where
  toJSON = toJSONText

instance ToJSON ISO8601 where
  toJSON = toJSONText

instance ToJSON AWSTime where
  toJSON = toJSONText

instance ToJSON BasicTime where
  toJSON = toJSONText

instance ToJSON POSIX where
  toJSON (Time t) =
    Aeson.Number $
      Scientific.scientific (truncate (utcTimeToPOSIXSeconds t) :: Integer) 0
