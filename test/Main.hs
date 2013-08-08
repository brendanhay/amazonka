{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Main
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Main where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.XML
import Data.Maybe
import Data.Monoid
import Data.String
import Data.Text                            (Text)
import Network.AWS.Internal
import Network.AWS.Route53.Types
import Paths_aws_haskell
import Test.Framework
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.QuickCheck.Monadic

main :: IO ()
main = defaultMain
    [ route53Properties
    ]

route53Properties :: Test
route53Properties = testGroup "Route53"
    [ testProperty "Parse CreateHostedZoneResponse" prop_parse_create_hosted_zone_response
    ]

prop_parse_create_hosted_zone_response :: Property
prop_parse_create_hosted_zone_response = monadicIO $ isJust <$>
    run (parse "CreateHostedZoneResponse" :: IO (Maybe CreateHostedZoneResponse))

-- Get examples of all route53 responses and test the deserialization

parse name = do
    str <- template name
    fromXML str

template :: String -> IO String
template name = getDataFileName ("test/template/" <> name) >>= readFile
