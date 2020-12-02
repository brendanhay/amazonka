{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Shield
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.Shield where

import Data.Proxy
import Network.AWS.Shield
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.Shield.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateSubscription $
--             createSubscription
--
--         , requestListProtections $
--             listProtections
--
--         , requestDeleteSubscription $
--             deleteSubscription
--
--         , requestDescribeAttack $
--             describeAttack
--
--         , requestDescribeProtection $
--             describeProtection
--
--         , requestListAttacks $
--             listAttacks
--
--         , requestCreateProtection $
--             createProtection
--
--         , requestDeleteProtection $
--             deleteProtection
--
--         , requestGetSubscriptionState $
--             getSubscriptionState
--
--         , requestDescribeSubscription $
--             describeSubscription
--
--           ]

--     , testGroup "response"
--         [ responseCreateSubscription $
--             createSubscriptionResponse
--
--         , responseListProtections $
--             listProtectionsResponse
--
--         , responseDeleteSubscription $
--             deleteSubscriptionResponse
--
--         , responseDescribeAttack $
--             describeAttackResponse
--
--         , responseDescribeProtection $
--             describeProtectionResponse
--
--         , responseListAttacks $
--             listAttacksResponse
--
--         , responseCreateProtection $
--             createProtectionResponse
--
--         , responseDeleteProtection $
--             deleteProtectionResponse
--
--         , responseGetSubscriptionState $
--             getSubscriptionStateResponse
--
--         , responseDescribeSubscription $
--             describeSubscriptionResponse
--
--           ]
--     ]

-- Requests

requestCreateSubscription :: CreateSubscription -> TestTree
requestCreateSubscription = req
    "CreateSubscription"
    "fixture/CreateSubscription.yaml"

requestListProtections :: ListProtections -> TestTree
requestListProtections = req
    "ListProtections"
    "fixture/ListProtections.yaml"

requestDeleteSubscription :: DeleteSubscription -> TestTree
requestDeleteSubscription = req
    "DeleteSubscription"
    "fixture/DeleteSubscription.yaml"

requestDescribeAttack :: DescribeAttack -> TestTree
requestDescribeAttack = req
    "DescribeAttack"
    "fixture/DescribeAttack.yaml"

requestDescribeProtection :: DescribeProtection -> TestTree
requestDescribeProtection = req
    "DescribeProtection"
    "fixture/DescribeProtection.yaml"

requestListAttacks :: ListAttacks -> TestTree
requestListAttacks = req
    "ListAttacks"
    "fixture/ListAttacks.yaml"

requestCreateProtection :: CreateProtection -> TestTree
requestCreateProtection = req
    "CreateProtection"
    "fixture/CreateProtection.yaml"

requestDeleteProtection :: DeleteProtection -> TestTree
requestDeleteProtection = req
    "DeleteProtection"
    "fixture/DeleteProtection.yaml"

requestGetSubscriptionState :: GetSubscriptionState -> TestTree
requestGetSubscriptionState = req
    "GetSubscriptionState"
    "fixture/GetSubscriptionState.yaml"

requestDescribeSubscription :: DescribeSubscription -> TestTree
requestDescribeSubscription = req
    "DescribeSubscription"
    "fixture/DescribeSubscription.yaml"

-- Responses

responseCreateSubscription :: CreateSubscriptionResponse -> TestTree
responseCreateSubscription = res
    "CreateSubscriptionResponse"
    "fixture/CreateSubscriptionResponse.proto"
    shield
    (Proxy :: Proxy CreateSubscription)

responseListProtections :: ListProtectionsResponse -> TestTree
responseListProtections = res
    "ListProtectionsResponse"
    "fixture/ListProtectionsResponse.proto"
    shield
    (Proxy :: Proxy ListProtections)

responseDeleteSubscription :: DeleteSubscriptionResponse -> TestTree
responseDeleteSubscription = res
    "DeleteSubscriptionResponse"
    "fixture/DeleteSubscriptionResponse.proto"
    shield
    (Proxy :: Proxy DeleteSubscription)

responseDescribeAttack :: DescribeAttackResponse -> TestTree
responseDescribeAttack = res
    "DescribeAttackResponse"
    "fixture/DescribeAttackResponse.proto"
    shield
    (Proxy :: Proxy DescribeAttack)

responseDescribeProtection :: DescribeProtectionResponse -> TestTree
responseDescribeProtection = res
    "DescribeProtectionResponse"
    "fixture/DescribeProtectionResponse.proto"
    shield
    (Proxy :: Proxy DescribeProtection)

responseListAttacks :: ListAttacksResponse -> TestTree
responseListAttacks = res
    "ListAttacksResponse"
    "fixture/ListAttacksResponse.proto"
    shield
    (Proxy :: Proxy ListAttacks)

responseCreateProtection :: CreateProtectionResponse -> TestTree
responseCreateProtection = res
    "CreateProtectionResponse"
    "fixture/CreateProtectionResponse.proto"
    shield
    (Proxy :: Proxy CreateProtection)

responseDeleteProtection :: DeleteProtectionResponse -> TestTree
responseDeleteProtection = res
    "DeleteProtectionResponse"
    "fixture/DeleteProtectionResponse.proto"
    shield
    (Proxy :: Proxy DeleteProtection)

responseGetSubscriptionState :: GetSubscriptionStateResponse -> TestTree
responseGetSubscriptionState = res
    "GetSubscriptionStateResponse"
    "fixture/GetSubscriptionStateResponse.proto"
    shield
    (Proxy :: Proxy GetSubscriptionState)

responseDescribeSubscription :: DescribeSubscriptionResponse -> TestTree
responseDescribeSubscription = res
    "DescribeSubscriptionResponse"
    "fixture/DescribeSubscriptionResponse.proto"
    shield
    (Proxy :: Proxy DescribeSubscription)
