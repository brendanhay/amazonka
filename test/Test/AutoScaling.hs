{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Test.AutoScaling
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Test.AutoScaling (tests) where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Data.ByteString                             (ByteString)
import qualified Data.ByteString.Char8                       as BS
import           Data.List                                   ((\\), sort)
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Data.Text                                   (Text)
import           Data.Time
import           Network.AWS.AutoScaling.V20110101
import           Network.AWS.AutoScaling.V20110101.Responses
import           Network.AWS.AutoScaling.V20110101.Types
import           Network.AWS.Internal
import           Paths_aws_haskell                           (getDataFileName)
import           System.Locale
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                                  hiding (Test)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

version :: String
version = "V20110101"

tests :: [Test]
tests =
    [ testGroup version
        [ testGroup "Requests" []
        -- [ testCase "Encode CreateAutoScalingGroup Request" test_encode_create_autoscaling_group
        -- ]
        , testGroup "Responses"
            [ testCase "Decode CreateAutoScalingGroup Response" test_create_autoscaling_group_response
            ]
        ]
    ]

test_create_autoscaling_group_response = do
    parse "CreateAutoScalingGroupResponse" $ MetadataResponse "8d798a29-f083-11e1-bdfb-cb223EXAMPLE"

parse :: (Show a, Eq a, IsXML a) => String -> a -> IO ()
parse name expected = do
     ea <- decodeEither <$> response name
     ea @?= Right expected

response :: String -> IO ByteString
response name = getDataFileName
    ("test/response/AutoScaling/" <> version <> "/" <> name <> ".xml") >>= BS.readFile


-- test_encode_create_autoscaling_group = do
--     let asg@CreateAutoScalingGroup{..} = CreateAutoScalingGroup
--             { casgAutoScalingGroupName    = name
--             , casgLaunchConfigurationName = ResourceName name
--             , casgMinSize                 = minSize
--             , casgMaxSize                 = maxSize
--             , casgDesiredCapacity         = Just capacity
--             , casgDefaultCooldown         = Just cooldown
--             , casgAvailabilityZones       = Params zones
--             , casgLoadBalancerNames       = Params elbs
--             , casgHealthCheckType         = Just check
--             , casgHealthCheckGracePeriod  = Just grace
--             , casgPlacementGroup          = Just group
--             , casgVPCZoneIdentifier       = Just vpc
--             , casgTerminationPolicies     = Params policies
--             , casgTags                    = Params [tag]
--             }

--         expected = sort
--             [ ("AutoScalingGroupName", casgAutoScalingGroupName)
--             , ("AvailabilityZones.member.1", zones !! 0)
--             , ("AvailabilityZones.member.2", zones !! 1)
--             , ("DefaultCooldown", toBS cooldown)
--             , ("DesiredCapacity", toBS capacity)
--             , ("HealthCheckGracePeriod", toBS grace)
--             , ("HealthCheckType", toBS check)
--             , ("LaunchConfigurationName", name)
--             , ("LoadBalancerNames.member.1", elbs !! 0)
--             , ("MaxSize", toBS maxSize)
--             , ("MinSize", toBS minSize)
--             , ("PlacementGroup", group)
--             , ("Tags.member.1", fmtQueryString $ queryString tag)
--             , ("TerminationPolicies.member.1", policies !! 0)
--             , ("TerminationPolicies.member.2", policies !! 1)
--             , ("VPCZoneIdentifier", vpc)
--             ]


--     putStrLn ""
--     print . sort $ queryString asg
--     putStrLn ""
--     print $ expected

--     let xs = sort (queryString asg) \\ expected

--     xs @?= []
--   where
--     name     = "test-1c"
--     zones    = ["bam", "boozle"]
--     minSize  = 2
--     maxSize  = 10
--     capacity = 2
--     cooldown = 30
--     elbs     = ["Load-Balancer"]
--     check    = "ELB"
--     grace    = 120
--     group    = "Placement Group"
--     vpc      = "vpcPlacement"
--     policies = ["policy1", "policy2"]
--     tag      = Tag (Just "resource-id") (Just "resource-type") "key" (Just "value") (Just True)
