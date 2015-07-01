{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Test.AWS.EC2
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Test.AWS.EC2
    ( tests
    , fixtures
    ) where

import           Data.Time
import           Network.AWS.EC2
import           Network.AWS.Prelude
import           Test.AWS.Gen.EC2
import           Test.AWS.TH
import           Test.Tasty

tests :: [TestTree]
tests = []

fixtures :: [TestTree]
fixtures =
    [ testDescribeInstancesResponse $
        describeInstancesResponse 200 & dirReservations .~
            [ reservation "r-1a2b3c4d" "123456789012"
                & resGroups .~
                    [ groupIdentifier
                        & giGroupId   ?~ "sg-1a2b3c4d"
                        & giGroupName ?~ "my-security-group"
                    ]

                & resInstances .~
                    [ instance'
                        "i-1a2b3c4d"
                        "ami-1a2b3c4d"
                        0
                        C1Medium
                        $(mkTime "2014-03-18T21:47:02+0000")
                        (placement & plaAvailabilityZone ?~ "us-east-1a"
                                   & plaTenancy          ?~ Default)
                        (monitoring & monState ?~ MSDisabled)
                        X8664
                        EBS
                        HVM
                        Xen
                        (instanceState ISNRunning 16)
                    ]
            ]
    ]
