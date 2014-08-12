{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

-- Module      : Network.AWS.Data.Time
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Data.Time where

import           Control.Applicative
import           Data.Aeson
import           Data.Attoparsec.Text        (Parser)
import qualified Data.Attoparsec.Text        as AText
import qualified Data.ByteString.Char8       as BS
import           Data.Tagged
import qualified Data.Text                   as Text
import           Data.Time
import           Data.Time.Clock.POSIX
import           Network.AWS.Data.ByteString
import           Network.AWS.Data.Text
import           Network.AWS.Data.XML
import           System.Locale

data Format
    = RFC822Format
    | ISO8601Format
    | BasicFormat
    | AWSFormat
    | POSIXFormat
      deriving (Eq, Show)

data Time :: Format -> * where
    Time       :: UTCTime    -> Time a
    LocaleTime :: TimeLocale -> UTCTime -> Time a

deriving instance Show (Time a)
deriving instance Eq   (Time a)

type RFC822    = Time RFC822Format
type ISO8601   = Time ISO8601Format
type BasicTime = Time BasicFormat
type AWSTime   = Time AWSFormat
type POSIX     = Time POSIXFormat

class TimeFormat a where
    format :: Tagged a String

instance TimeFormat RFC822    where format = Tagged "%a, %d %b %Y %H:%M:%S GMT"
instance TimeFormat ISO8601   where format = Tagged (iso8601DateFormat (Just "%XZ"))
instance TimeFormat BasicTime where format = Tagged "%Y%m%d"
instance TimeFormat AWSTime   where format = Tagged "%Y%m%dT%H%M%SZ"

instance ToByteString RFC822    where toBS = BS.pack . renderFormattedTime
instance ToByteString ISO8601   where toBS = BS.pack . renderFormattedTime
instance ToByteString BasicTime where toBS = BS.pack . renderFormattedTime
instance ToByteString AWSTime   where toBS = BS.pack . renderFormattedTime

instance ToText RFC822    where toText = Text.pack . renderFormattedTime
instance ToText ISO8601   where toText = Text.pack . renderFormattedTime
instance ToText BasicTime where toText = Text.pack . renderFormattedTime
instance ToText AWSTime   where toText = Text.pack . renderFormattedTime

instance ToText POSIX where
    toText t = toText time
      where
        time :: Integer
        time = truncate . utcTimeToPOSIXSeconds $
            case t of
                Time         x -> x
                LocaleTime _ x -> x

instance ToXML RFC822 where
    toXMLRoot = toRoot "Date"
    toXML o   = toXML (retag o) . toText

instance ToJSON RFC822 where
    toJSON = toJSON . toText

instance ToXML ISO8601 where
    toXMLRoot = toRoot "Date"
    toXML o   = toXML (retag o) . toText

instance ToJSON ISO8601 where
    toJSON = toJSON . toText

instance ToJSON POSIX where
    toJSON = toJSON . toText

renderFormattedTime :: forall a. TimeFormat (Time a) => Time a -> String
renderFormattedTime x = formatTime l (untag f) t
  where
    (l, t) = case x of
        Time          t' -> (defaultTimeLocale, t')
        LocaleTime l' t' -> (l', t')

    f :: Tagged (Time a) String
    f = format

instance FromText RFC822    where parser = parseFormattedTime
instance FromText ISO8601   where parser = parseFormattedTime
instance FromText BasicTime where parser = parseFormattedTime
instance FromText AWSTime   where parser = parseFormattedTime

instance FromText POSIX where
    parser = Time . posixSecondsToUTCTime . realToFrac
        <$> (parser :: Parser Integer)

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

instance FromXML RFC822 where
    fromXMLRoot = fromRoot "Date"
    fromXML     = const fromNodeContent

instance FromJSON RFC822 where
    parseJSON = withText "Date" (either fail return . fromText)

instance FromXML ISO8601 where
    fromXMLRoot = fromRoot "Date"
    fromXML     = const fromNodeContent

instance FromJSON ISO8601 where
    parseJSON = withText "Date" (either fail return . fromText)

instance FromJSON POSIX where
    parseJSON = withText "Date" (either fail return . fromText)
