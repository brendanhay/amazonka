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
--         [ requestAssociateDRTLogBucket $
--             associateDRTLogBucket
--
--         , requestDisassociateDRTRole $
--             disassociateDRTRole
--
--         , requestCreateSubscription $
--             createSubscription
--
--         , requestListProtections $
--             listProtections
--
--         , requestAssociateDRTRole $
--             associateDRTRole
--
--         , requestUpdateSubscription $
--             updateSubscription
--
--         , requestDisassociateDRTLogBucket $
--             disassociateDRTLogBucket
--
--         , requestDescribeAttack $
--             describeAttack
--
--         , requestUpdateEmergencyContactSettings $
--             updateEmergencyContactSettings
--
--         , requestDescribeProtection $
--             describeProtection
--
--         , requestListAttacks $
--             listAttacks
--
--         , requestDescribeEmergencyContactSettings $
--             describeEmergencyContactSettings
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
--         , requestDescribeDRTAccess $
--             describeDRTAccess
--
--         , requestDescribeSubscription $
--             describeSubscription
--
--           ]

--     , testGroup "response"
--         [ responseAssociateDRTLogBucket $
--             associateDRTLogBucketResponse
--
--         , responseDisassociateDRTRole $
--             disassociateDRTRoleResponse
--
--         , responseCreateSubscription $
--             createSubscriptionResponse
--
--         , responseListProtections $
--             listProtectionsResponse
--
--         , responseAssociateDRTRole $
--             associateDRTRoleResponse
--
--         , responseUpdateSubscription $
--             updateSubscriptionResponse
--
--         , responseDisassociateDRTLogBucket $
--             disassociateDRTLogBucketResponse
--
--         , responseDescribeAttack $
--             describeAttackResponse
--
--         , responseUpdateEmergencyContactSettings $
--             updateEmergencyContactSettingsResponse
--
--         , responseDescribeProtection $
--             describeProtectionResponse
--
--         , responseListAttacks $
--             listAttacksResponse
--
--         , responseDescribeEmergencyContactSettings $
--             describeEmergencyContactSettingsResponse
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
--         , responseDescribeDRTAccess $
--             describeDRTAccessResponse
--
--         , responseDescribeSubscription $
--             describeSubscriptionResponse
--
--           ]
--     ]

-- Requests

requestAssociateDRTLogBucket :: AssociateDRTLogBucket -> TestTree
requestAssociateDRTLogBucket = req
    "AssociateDRTLogBucket"
    "fixture/AssociateDRTLogBucket.yaml"

requestDisassociateDRTRole :: DisassociateDRTRole -> TestTree
requestDisassociateDRTRole = req
    "DisassociateDRTRole"
    "fixture/DisassociateDRTRole.yaml"

requestCreateSubscription :: CreateSubscription -> TestTree
requestCreateSubscription = req
    "CreateSubscription"
    "fixture/CreateSubscription.yaml"

requestListProtections :: ListProtections -> TestTree
requestListProtections = req
    "ListProtections"
    "fixture/ListProtections.yaml"

requestAssociateDRTRole :: AssociateDRTRole -> TestTree
requestAssociateDRTRole = req
    "AssociateDRTRole"
    "fixture/AssociateDRTRole.yaml"

requestUpdateSubscription :: UpdateSubscription -> TestTree
requestUpdateSubscription = req
    "UpdateSubscription"
    "fixture/UpdateSubscription.yaml"

requestDisassociateDRTLogBucket :: DisassociateDRTLogBucket -> TestTree
requestDisassociateDRTLogBucket = req
    "DisassociateDRTLogBucket"
    "fixture/DisassociateDRTLogBucket.yaml"

requestDescribeAttack :: DescribeAttack -> TestTree
requestDescribeAttack = req
    "DescribeAttack"
    "fixture/DescribeAttack.yaml"

requestUpdateEmergencyContactSettings :: UpdateEmergencyContactSettings -> TestTree
requestUpdateEmergencyContactSettings = req
    "UpdateEmergencyContactSettings"
    "fixture/UpdateEmergencyContactSettings.yaml"

requestDescribeProtection :: DescribeProtection -> TestTree
requestDescribeProtection = req
    "DescribeProtection"
    "fixture/DescribeProtection.yaml"

requestListAttacks :: ListAttacks -> TestTree
requestListAttacks = req
    "ListAttacks"
    "fixture/ListAttacks.yaml"

requestDescribeEmergencyContactSettings :: DescribeEmergencyContactSettings -> TestTree
requestDescribeEmergencyContactSettings = req
    "DescribeEmergencyContactSettings"
    "fixture/DescribeEmergencyContactSettings.yaml"

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

requestDescribeDRTAccess :: DescribeDRTAccess -> TestTree
requestDescribeDRTAccess = req
    "DescribeDRTAccess"
    "fixture/DescribeDRTAccess.yaml"

requestDescribeSubscription :: DescribeSubscription -> TestTree
requestDescribeSubscription = req
    "DescribeSubscription"
    "fixture/DescribeSubscription.yaml"

-- Responses

responseAssociateDRTLogBucket :: AssociateDRTLogBucketResponse -> TestTree
responseAssociateDRTLogBucket = res
    "AssociateDRTLogBucketResponse"
    "fixture/AssociateDRTLogBucketResponse.proto"
    shield
    (Proxy :: Proxy AssociateDRTLogBucket)

responseDisassociateDRTRole :: DisassociateDRTRoleResponse -> TestTree
responseDisassociateDRTRole = res
    "DisassociateDRTRoleResponse"
    "fixture/DisassociateDRTRoleResponse.proto"
    shield
    (Proxy :: Proxy DisassociateDRTRole)

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

responseAssociateDRTRole :: AssociateDRTRoleResponse -> TestTree
responseAssociateDRTRole = res
    "AssociateDRTRoleResponse"
    "fixture/AssociateDRTRoleResponse.proto"
    shield
    (Proxy :: Proxy AssociateDRTRole)

responseUpdateSubscription :: UpdateSubscriptionResponse -> TestTree
responseUpdateSubscription = res
    "UpdateSubscriptionResponse"
    "fixture/UpdateSubscriptionResponse.proto"
    shield
    (Proxy :: Proxy UpdateSubscription)

responseDisassociateDRTLogBucket :: DisassociateDRTLogBucketResponse -> TestTree
responseDisassociateDRTLogBucket = res
    "DisassociateDRTLogBucketResponse"
    "fixture/DisassociateDRTLogBucketResponse.proto"
    shield
    (Proxy :: Proxy DisassociateDRTLogBucket)

responseDescribeAttack :: DescribeAttackResponse -> TestTree
responseDescribeAttack = res
    "DescribeAttackResponse"
    "fixture/DescribeAttackResponse.proto"
    shield
    (Proxy :: Proxy DescribeAttack)

responseUpdateEmergencyContactSettings :: UpdateEmergencyContactSettingsResponse -> TestTree
responseUpdateEmergencyContactSettings = res
    "UpdateEmergencyContactSettingsResponse"
    "fixture/UpdateEmergencyContactSettingsResponse.proto"
    shield
    (Proxy :: Proxy UpdateEmergencyContactSettings)

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

responseDescribeEmergencyContactSettings :: DescribeEmergencyContactSettingsResponse -> TestTree
responseDescribeEmergencyContactSettings = res
    "DescribeEmergencyContactSettingsResponse"
    "fixture/DescribeEmergencyContactSettingsResponse.proto"
    shield
    (Proxy :: Proxy DescribeEmergencyContactSettings)

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

responseDescribeDRTAccess :: DescribeDRTAccessResponse -> TestTree
responseDescribeDRTAccess = res
    "DescribeDRTAccessResponse"
    "fixture/DescribeDRTAccessResponse.proto"
    shield
    (Proxy :: Proxy DescribeDRTAccess)

responseDescribeSubscription :: DescribeSubscriptionResponse -> TestTree
responseDescribeSubscription = res
    "DescribeSubscriptionResponse"
    "fixture/DescribeSubscriptionResponse.proto"
    shield
    (Proxy :: Proxy DescribeSubscription)
