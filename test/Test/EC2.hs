{-# LANGUAGE TemplateHaskell      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Module      : Test.EC2
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Test.EC2 (tests) where

import Network.AWS.EC2
import Test.Common

tests :: [Test]
tests = (:[]) $ testVersion ec2Version
    [ testGroup "Requests"
        [ testProperty "AllocateAddress" (prop :: Rq AllocateAddress)
        ]

    , testGroup "Responses"
        [ testProperty "AllocateAddressResponse" (prop :: Rs AllocateAddressResponse)
        ]
    ]

$(deriveDependency
    [ ''Domain
    ])

$(deriveProperty "test/resources/EC2"
    [ ''AllocateAddress
    , ''AllocateAddressResponse
    ])
