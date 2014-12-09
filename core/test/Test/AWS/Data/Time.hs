{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- Module      : Test.AWS.Data.Time
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Test.AWS.Data.Time (tests) where

import Data.Time
import Network.AWS.Prelude
import Test.AWS.Types
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "time"
    [ testGroup "deserialise xml"
        [ testCase "RFC822" $
            assertXML "<T>Fri, 07 Nov 2014 04:42:13 GMT</T>" (ref :: RFC822)
        , testCase "ISO8601" $
            assertXML "<T>2014-11-07T04:42:13.000Z</T>" (ref :: ISO8601)
        , testCase "AWS" $
            assertXML "<T>20141107T044213Z</T>" (ref :: AWSTime)
        , testCase "POSIX" $
            assertXML "<T>1415335333</T>" (ref :: POSIX)
        ]
    ]

ref :: Time a
ref = Time $ fromMaybe (error "Unable to parse time") (parseTime locale format ts)
  where
    locale = defaultTimeLocale
    format = iso8601DateFormat (Just "%H:%M:%S")

    ts = "2014-11-07T04:42:13"
