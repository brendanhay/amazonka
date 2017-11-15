{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

-- |
-- Module      : Network.AWS.Data.Time
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Data.Time
    (
    -- * Time
      Format (..)
    , Time   (..)
    , _Time
    -- ** Formats
    , UTCTime
    , RFC822
    , ISO8601
    , BasicTime
    , AWSTime
    , POSIX
    ) where

import           Control.Applicative
import           Control.DeepSeq
import           Data.Aeson
import qualified Data.Aeson.Types            as Aeson
import           Data.Attoparsec.Text        (Parser)
import qualified Data.Attoparsec.Text        as AText
import qualified Data.ByteString.Char8       as BS
import           Data.Data                   (Data, Typeable)
import           Data.Hashable
import           Data.Monoid
import           Data.Scientific
import           Data.Tagged
import qualified Data.Text                   as Text
import           Data.Time                   (Day (..), UTCTime (..))
import           Data.Time.Clock.POSIX
import           Data.Time.Format            (formatTime)
import           GHC.Generics                (Generic)
import           Network.AWS.Compat.Locale
import           Network.AWS.Compat.Time
import           Network.AWS.Data.ByteString
import           Network.AWS.Data.JSON
import           Network.AWS.Data.Query
import           Network.AWS.Data.Text
import           Network.AWS.Data.XML
import           Network.AWS.Lens            (Iso', iso)

data Format
    = RFC822Format
    | ISO8601Format
    | BasicFormat
    | AWSFormat
    | POSIXFormat
      deriving (Eq, Read, Show, Data, Typeable, Generic)

deriving instance Typeable 'RFC822Format
deriving instance Typeable 'ISO8601Format
deriving instance Typeable 'BasicFormat
deriving instance Typeable 'AWSFormat
deriving instance Typeable 'POSIXFormat

data Time :: Format -> * where
    Time :: UTCTime -> Time a
      deriving (Data, Typeable, Generic)

deriving instance Eq   (Time a)
deriving instance Ord  (Time a)
deriving instance Read (Time a)
deriving instance Show (Time a)

instance NFData (Time a)

instance Hashable (Time a) where
    hashWithSalt salt (Time (UTCTime (ModifiedJulianDay d) t)) =
        salt `hashWithSalt` d
             `hashWithSalt` toRational t

_Time :: Iso' (Time a) UTCTime
_Time = iso (\(Time t) -> t) Time

convert :: Time a -> Time b
convert (Time t) = Time t

type RFC822    = Time 'RFC822Format
type ISO8601   = Time 'ISO8601Format
type BasicTime = Time 'BasicFormat
type AWSTime   = Time 'AWSFormat
type POSIX     = Time 'POSIXFormat

class TimeFormat a where
    format :: Tagged a String

instance TimeFormat RFC822    where format = Tagged "%a, %d %b %Y %H:%M:%S GMT"
instance TimeFormat ISO8601   where format = Tagged (iso8601DateFormat (Just "%X%QZ"))
instance TimeFormat BasicTime where format = Tagged "%Y%m%d"
instance TimeFormat AWSTime   where format = Tagged "%Y%m%dT%H%M%SZ"

instance FromText BasicTime where parser = parseFormattedTime
instance FromText AWSTime   where parser = parseFormattedTime

instance FromText RFC822 where
    parser = (convert :: ISO8601 -> RFC822) <$> parseFormattedTime
         <|> parseFormattedTime

instance FromText ISO8601 where
    parser = (convert :: RFC822 -> ISO8601) <$> parseFormattedTime
         <|> parseFormattedTime
         -- Deprecated, but ensure compatibility with examples until further investigation can be done
         <|> parseFormattedTime' (Tagged $ iso8601DateFormat (Just "%X%Q%Z"))

parseFormattedTime :: forall a. TimeFormat (Time a) => Parser (Time a)
parseFormattedTime = parseFormattedTime' format

parseFormattedTime' :: Tagged (Time a) String -> Parser (Time a)
parseFormattedTime' f = do
    x <- Text.unpack <$> AText.takeText
    p (parseTime defaultTimeLocale (untag f) x) x
  where
    p :: Maybe UTCTime -> String -> Parser (Time a)
    p (Just x) _ = return (Time x)
    p Nothing  s = fail $ mconcat
        [ "Failure parsing Date format "
        , untag f
        , " from value: '"
        , s
        , "'"
        ]

instance ToText RFC822    where toText = Text.pack . renderFormattedTime
instance ToText ISO8601   where toText = Text.pack . renderFormattedTime
instance ToText BasicTime where toText = Text.pack . renderFormattedTime
instance ToText AWSTime   where toText = Text.pack . renderFormattedTime

instance ToText POSIX where
    toText (Time t) = toText (truncate (utcTimeToPOSIXSeconds t) :: Integer)

renderFormattedTime :: forall a. TimeFormat (Time a) => Time a -> String
renderFormattedTime (Time t) = formatTime defaultTimeLocale (untag f) t
  where
    f :: Tagged (Time a) String
    f = format

instance FromXML RFC822    where parseXML = parseXMLText "RFC822"
instance FromXML ISO8601   where parseXML = parseXMLText "ISO8601"
instance FromXML AWSTime   where parseXML = parseXMLText "AWSTime"
instance FromXML BasicTime where parseXML = parseXMLText "BasicTime"

instance FromJSON RFC822    where parseJSON = parseJSONText "RFC822"
instance FromJSON ISO8601   where parseJSON = parseJSONText "ISO8601"
instance FromJSON AWSTime   where parseJSON = parseJSONText "AWSTime"
instance FromJSON BasicTime where parseJSON = parseJSONText "BasicTime"

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
        num = withScientific "POSIX"
            ( pure
            . Time
            . posixSecondsToUTCTime
            . realToFrac
            )

instance ToByteString RFC822    where toBS = BS.pack . renderFormattedTime
instance ToByteString ISO8601   where toBS = BS.pack . renderFormattedTime
instance ToByteString BasicTime where toBS = BS.pack . renderFormattedTime
instance ToByteString AWSTime   where toBS = BS.pack . renderFormattedTime

instance ToQuery RFC822    where toQuery = toQuery . toBS
instance ToQuery ISO8601   where toQuery = toQuery . toBS
instance ToQuery BasicTime where toQuery = toQuery . toBS
instance ToQuery AWSTime   where toQuery = toQuery . toBS

instance ToQuery POSIX where
    toQuery (Time t) = toQuery (truncate (utcTimeToPOSIXSeconds t) :: Integer)

instance ToXML RFC822    where toXML = toXMLText
instance ToXML ISO8601   where toXML = toXMLText
instance ToXML AWSTime   where toXML = toXMLText
instance ToXML BasicTime where toXML = toXMLText

instance ToJSON RFC822    where toJSON = toJSONText
instance ToJSON ISO8601   where toJSON = toJSONText
instance ToJSON AWSTime   where toJSON = toJSONText
instance ToJSON BasicTime where toJSON = toJSONText

instance ToJSON POSIX where
    toJSON (Time t) =
        Number $ scientific (truncate (utcTimeToPOSIXSeconds t) :: Integer) 0
