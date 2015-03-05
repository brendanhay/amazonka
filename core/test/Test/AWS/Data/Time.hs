{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Test.AWS.Data.Time
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Test.AWS.Data.Time (tests) where

import           Data.Aeson
import           Network.AWS.Data
import           Network.AWS.Prelude
import           Test.AWS.Types
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "time"
    [ testGroup "deserialise xml"
        [ testCase "RFC822" $
            assertXML "<T>Fri, 07 Nov 2014 04:42:13 GMT</T>" (ref :: RFC822)
        , testCase "ISO8601" $
            assertXML "<T>2014-11-07T04:42:13.000Z</T>" (ref :: ISO8601)
        , testCase "AWS" $
            assertXML "<T>20141107T044213Z</T>" (ref :: AWSTime)
        , testCase "POSIX integer" $
            assertXML "<T>1415335333</T>" (ref :: POSIX)
        , testCase "POSIX scientific" $
            assertXML "<T>1.415335333E9</T>" (ref :: POSIX)
        ]
    , testGroup "deserialise json"
        [ testCase "RFC822" $
            assertJSON "{\"time_rfc\":\"Fri, 07 Nov 2014 04:42:13 GMT\"}"
                       (defaultTimeItem {timeRFC = Just ref})
        , testCase "ISO8601" $
            assertJSON "{\"time_iso\":\"2014-11-07T04:42:13.000Z\"}"
                       (defaultTimeItem {timeISO = Just ref})
        , testCase "AWS" $
            assertJSON "{\"time_aws\":\"20141107T044213Z\"}"
                       (defaultTimeItem {timeAWS = Just ref})
        , testCase "POSIX integer" $
            assertJSON "{\"time_posix\":1415335333}"
                       (defaultTimeItem {timePOSIX = Just ref})
        , testCase "POSIX scientific" $
            assertJSON "{\"time_posix\":1.415335333E9}"
                       (defaultTimeItem {timePOSIX = Just ref})
        ]
    , testGroup "serialise json"
        [ testCase "POSIX integer" $
            (@?=) (encode $ defaultTimeItem {timePOSIX = Just ref})
                  "{\"time_aws\":null,\"time_iso\":null,\
                  \\"time_posix\":1415335333,\"time_rfc\":null}"
        ]
    ]

ref :: Time a
ref = Time $ fromMaybe (error "Unable to parse time") (parseTime locale format ts)
  where
    locale = defaultTimeLocale
    format = iso8601DateFormat (Just "%H:%M:%S")
    ts = "2014-11-07T04:42:13"

data TimeItem = TimeItem
     { timeRFC   :: Maybe RFC822
     , timeISO   :: Maybe ISO8601
     , timeAWS   :: Maybe AWSTime
     , timePOSIX :: Maybe POSIX
     } deriving (Eq, Show)

defaultTimeItem :: TimeItem
defaultTimeItem = TimeItem Nothing Nothing Nothing Nothing

instance ToJSON TimeItem where
    toJSON TimeItem{..} = object
        [ "time_rfc"   .= timeRFC
        , "time_iso"   .= timeISO
        , "time_aws"   .= timeAWS
        , "time_posix" .= timePOSIX
        ]

instance FromJSON TimeItem where
    parseJSON = withObject "TimeItem" $ \o -> TimeItem
        <$> o .:? "time_rfc"
        <*> o .:? "time_iso"
        <*> o .:? "time_aws"
        <*> o .:? "time_posix"
