{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

-- Module      : Network.AWS.Data.Internal.Time
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Data.Internal.Time
    ( Format (..)
    , Time   (..)
    , _Time

    , UTCTime
    , RFC822
    , ISO8601
    , BasicTime
    , AWSTime
    , POSIX

    , parseTime
    , defaultTimeLocale
    , iso8601DateFormat
    ) where

import           Control.Applicative
import           Control.Lens
import           Data.Aeson
import           Data.Attoparsec.Text                 (Parser)
import qualified Data.Attoparsec.Text                 as AText
import qualified Data.ByteString.Char8                as BS
import           Data.Scientific
import           Data.Tagged
import qualified Data.Text                            as Text
import           Data.Time                            (UTCTime)
import           Data.Time.Clock.POSIX
import           Data.Time.Format                     (formatTime)
import           Network.AWS.Compat.Internal.Locale   (defaultTimeLocale,
                                                       iso8601DateFormat)
import           Network.AWS.Compat.Internal.Time     (parseTime)
import           Network.AWS.Data.Internal.ByteString
import           Network.AWS.Data.Internal.JSON
import           Network.AWS.Data.Internal.Query
import           Network.AWS.Data.Internal.Text
import           Network.AWS.Data.Internal.XML

data Format
    = RFC822Format
    | ISO8601Format
    | BasicFormat
    | AWSFormat
    | POSIXFormat
      deriving (Eq, Read, Show)

data Time :: Format -> * where
    Time :: UTCTime -> Time a

deriving instance Eq   (Time a)
deriving instance Ord  (Time a)
deriving instance Read (Time a)
deriving instance Show (Time a)

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

instance FromText POSIX where
    parser = Time . posixSecondsToUTCTime . realToFrac
        <$> (parser :: Parser Scientific)

parseFormattedTime :: forall a. TimeFormat (Time a) => Parser (Time a)
parseFormattedTime = do
    x <- Text.unpack <$> AText.takeText
    p (parseTime defaultTimeLocale (untag f) x) x
  where
    p :: Maybe UTCTime -> String -> Parser (Time a)
    p Nothing  s = fail   ("Unable to parse " ++ untag f ++ " from " ++ s)
    p (Just x) _ = return (Time x)

    f :: Tagged (Time a) String
    f = format

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
instance FromXML POSIX     where parseXML = parseXMLText "POSIX"

instance FromJSON RFC822    where parseJSON = parseJSONText "RFC822"
instance FromJSON ISO8601   where parseJSON = parseJSONText "ISO8601"
instance FromJSON AWSTime   where parseJSON = parseJSONText "AWSTime"
instance FromJSON BasicTime where parseJSON = parseJSONText "BasicTime"

instance FromJSON POSIX where
    parseJSON = withScientific "POSIX"
        $ pure
        . Time
        . posixSecondsToUTCTime
        . realToFrac

instance ToByteString RFC822    where toBS = BS.pack . renderFormattedTime
instance ToByteString ISO8601   where toBS = BS.pack . renderFormattedTime
instance ToByteString BasicTime where toBS = BS.pack . renderFormattedTime
instance ToByteString AWSTime   where toBS = BS.pack . renderFormattedTime

instance ToQuery RFC822    where toQuery = toQuery . toBS
instance ToQuery ISO8601   where toQuery = toQuery . toBS
instance ToQuery BasicTime where toQuery = toQuery . toBS
instance ToQuery AWSTime   where toQuery = toQuery . toBS

instance ToXML RFC822    where toXML = toXMLText
instance ToXML ISO8601   where toXML = toXMLText
instance ToXML AWSTime   where toXML = toXMLText
instance ToXML BasicTime where toXML = toXMLText
instance ToXML POSIX     where toXML = toXMLText

instance ToJSON RFC822    where toJSON = toJSONText
instance ToJSON ISO8601   where toJSON = toJSONText
instance ToJSON AWSTime   where toJSON = toJSONText
instance ToJSON BasicTime where toJSON = toJSONText

instance ToJSON POSIX where
    toJSON (Time t) = Number $
        scientific (truncate (utcTimeToPOSIXSeconds t) :: Integer) 0
