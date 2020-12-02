{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Test.AWS.APIGateway
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Test.AWS.APIGateway
    ( tests
    , fixtures
    ) where

import           Data.Time
import           Network.AWS.APIGateway
import           Network.AWS.Lens        ((&), (?~))
import           Network.AWS.Prelude
import           Test.AWS.Gen.APIGateway
import           Test.AWS.TH
import           Test.Tasty

tests :: [TestTree]
tests = []

fixtures :: [TestTree]
fixtures =
    [ testGroup "request"
        [ requestCreateRestAPI $
            createRestAPI "testAPI"
        ]

    , testGroup "response"
        [ responseCreateRestAPI $
            restAPI
                & raId          ?~ "4x69hrdz0g"
                & raName        ?~ "testAPI"
                & raCreatedDate ?~ $(mkTime "2016-06-07T17:39:19+02:00")
        ]
    ]
