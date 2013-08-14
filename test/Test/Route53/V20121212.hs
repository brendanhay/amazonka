{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Test.Route53.V20121212
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Test.Route53.V20121212 (tests) where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Maybe
import Data.Monoid
import Data.String
import Data.Text                           (Text)
import Data.Time
import Network.AWS.Internal
import Network.AWS.Route53.V20121212
import Network.AWS.Route53.V20121212.Types
import Paths_aws_haskell                   (getDataFileName)
import System.Locale
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit                          hiding (Test)
import Test.QuickCheck
import Test.QuickCheck.Monadic

version :: String
version = "V20121212"

tests :: [Test]
tests =
    [ testGroup version
        [ -- testCase "Parse CreateHostedZoneResponse" test_parse_create_hosted_zone_response
        ]
    ]

-- test_parse_create_hosted_zone_response = parse "CreateHostedZoneResponse" $
--     CreateHostedZoneResponse hostedZone changeInfo delegationSet

-- parse :: (Show a, Eq a, FromXML a) => String -> a -> IO ()
-- parse name expected = do
--      ma <- fromXML =<< response name
--      ma @?= Just expected

-- response :: String -> IO String
-- response name = getDataFileName
--     ("test/response/Route53/" <> version <> "/" <> name <> ".xml") >>= readFile

-- hostedZone :: HostedZone
-- hostedZone = HostedZone
--    "hosted-zone-identifier"
--    "hosted-zone-name"
--    "hosted-zone-caller-ref"
--    config
--    302

-- config :: Config
-- config = Config "config-comment"

-- changeInfo :: ChangeInfo
-- changeInfo = ChangeInfo
--     "change-info-identifier"
--     PENDING
--     time

-- delegationSet :: DelegationSet
-- delegationSet = DelegationSet ["ns1.test", "ns2.test"]

-- time :: UTCTime
-- time = readTime defaultTimeLocale "%B %e %Y %l:%M%P %Z" "March 7 2009 7:30pm EST"
