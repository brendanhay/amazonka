{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Shield
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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
--         , requestAssociateProactiveEngagementDetails $
--             associateProactiveEngagementDetails
--
--         , requestDescribeAttack $
--             describeAttack
--
--         , requestListProtectionGroups $
--             listProtectionGroups
--
--         , requestEnableProactiveEngagement $
--             enableProactiveEngagement
--
--         , requestUpdateEmergencyContactSettings $
--             updateEmergencyContactSettings
--
--         , requestCreateProtectionGroup $
--             createProtectionGroup
--
--         , requestDisableProactiveEngagement $
--             disableProactiveEngagement
--
--         , requestDisassociateHealthCheck $
--             disassociateHealthCheck
--
--         , requestListResourcesInProtectionGroup $
--             listResourcesInProtectionGroup
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
--         , requestDeleteProtectionGroup $
--             deleteProtectionGroup
--
--         , requestUpdateProtectionGroup $
--             updateProtectionGroup
--
--         , requestDescribeAttackStatistics $
--             describeAttackStatistics
--
--         , requestDescribeDRTAccess $
--             describeDRTAccess
--
--         , requestDescribeSubscription $
--             describeSubscription
--
--         , requestAssociateHealthCheck $
--             associateHealthCheck
--
--         , requestDescribeProtectionGroup $
--             describeProtectionGroup
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
--         , responseAssociateProactiveEngagementDetails $
--             associateProactiveEngagementDetailsResponse
--
--         , responseDescribeAttack $
--             describeAttackResponse
--
--         , responseListProtectionGroups $
--             listProtectionGroupsResponse
--
--         , responseEnableProactiveEngagement $
--             enableProactiveEngagementResponse
--
--         , responseUpdateEmergencyContactSettings $
--             updateEmergencyContactSettingsResponse
--
--         , responseCreateProtectionGroup $
--             createProtectionGroupResponse
--
--         , responseDisableProactiveEngagement $
--             disableProactiveEngagementResponse
--
--         , responseDisassociateHealthCheck $
--             disassociateHealthCheckResponse
--
--         , responseListResourcesInProtectionGroup $
--             listResourcesInProtectionGroupResponse
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
--         , responseDeleteProtectionGroup $
--             deleteProtectionGroupResponse
--
--         , responseUpdateProtectionGroup $
--             updateProtectionGroupResponse
--
--         , responseDescribeAttackStatistics $
--             describeAttackStatisticsResponse
--
--         , responseDescribeDRTAccess $
--             describeDRTAccessResponse
--
--         , responseDescribeSubscription $
--             describeSubscriptionResponse
--
--         , responseAssociateHealthCheck $
--             associateHealthCheckResponse
--
--         , responseDescribeProtectionGroup $
--             describeProtectionGroupResponse
--
--           ]
--     ]

-- Requests

requestAssociateDRTLogBucket :: AssociateDRTLogBucket -> TestTree
requestAssociateDRTLogBucket =
  req
    "AssociateDRTLogBucket"
    "fixture/AssociateDRTLogBucket.yaml"

requestDisassociateDRTRole :: DisassociateDRTRole -> TestTree
requestDisassociateDRTRole =
  req
    "DisassociateDRTRole"
    "fixture/DisassociateDRTRole.yaml"

requestCreateSubscription :: CreateSubscription -> TestTree
requestCreateSubscription =
  req
    "CreateSubscription"
    "fixture/CreateSubscription.yaml"

requestListProtections :: ListProtections -> TestTree
requestListProtections =
  req
    "ListProtections"
    "fixture/ListProtections.yaml"

requestAssociateDRTRole :: AssociateDRTRole -> TestTree
requestAssociateDRTRole =
  req
    "AssociateDRTRole"
    "fixture/AssociateDRTRole.yaml"

requestUpdateSubscription :: UpdateSubscription -> TestTree
requestUpdateSubscription =
  req
    "UpdateSubscription"
    "fixture/UpdateSubscription.yaml"

requestDisassociateDRTLogBucket :: DisassociateDRTLogBucket -> TestTree
requestDisassociateDRTLogBucket =
  req
    "DisassociateDRTLogBucket"
    "fixture/DisassociateDRTLogBucket.yaml"

requestAssociateProactiveEngagementDetails :: AssociateProactiveEngagementDetails -> TestTree
requestAssociateProactiveEngagementDetails =
  req
    "AssociateProactiveEngagementDetails"
    "fixture/AssociateProactiveEngagementDetails.yaml"

requestDescribeAttack :: DescribeAttack -> TestTree
requestDescribeAttack =
  req
    "DescribeAttack"
    "fixture/DescribeAttack.yaml"

requestListProtectionGroups :: ListProtectionGroups -> TestTree
requestListProtectionGroups =
  req
    "ListProtectionGroups"
    "fixture/ListProtectionGroups.yaml"

requestEnableProactiveEngagement :: EnableProactiveEngagement -> TestTree
requestEnableProactiveEngagement =
  req
    "EnableProactiveEngagement"
    "fixture/EnableProactiveEngagement.yaml"

requestUpdateEmergencyContactSettings :: UpdateEmergencyContactSettings -> TestTree
requestUpdateEmergencyContactSettings =
  req
    "UpdateEmergencyContactSettings"
    "fixture/UpdateEmergencyContactSettings.yaml"

requestCreateProtectionGroup :: CreateProtectionGroup -> TestTree
requestCreateProtectionGroup =
  req
    "CreateProtectionGroup"
    "fixture/CreateProtectionGroup.yaml"

requestDisableProactiveEngagement :: DisableProactiveEngagement -> TestTree
requestDisableProactiveEngagement =
  req
    "DisableProactiveEngagement"
    "fixture/DisableProactiveEngagement.yaml"

requestDisassociateHealthCheck :: DisassociateHealthCheck -> TestTree
requestDisassociateHealthCheck =
  req
    "DisassociateHealthCheck"
    "fixture/DisassociateHealthCheck.yaml"

requestListResourcesInProtectionGroup :: ListResourcesInProtectionGroup -> TestTree
requestListResourcesInProtectionGroup =
  req
    "ListResourcesInProtectionGroup"
    "fixture/ListResourcesInProtectionGroup.yaml"

requestDescribeProtection :: DescribeProtection -> TestTree
requestDescribeProtection =
  req
    "DescribeProtection"
    "fixture/DescribeProtection.yaml"

requestListAttacks :: ListAttacks -> TestTree
requestListAttacks =
  req
    "ListAttacks"
    "fixture/ListAttacks.yaml"

requestDescribeEmergencyContactSettings :: DescribeEmergencyContactSettings -> TestTree
requestDescribeEmergencyContactSettings =
  req
    "DescribeEmergencyContactSettings"
    "fixture/DescribeEmergencyContactSettings.yaml"

requestCreateProtection :: CreateProtection -> TestTree
requestCreateProtection =
  req
    "CreateProtection"
    "fixture/CreateProtection.yaml"

requestDeleteProtection :: DeleteProtection -> TestTree
requestDeleteProtection =
  req
    "DeleteProtection"
    "fixture/DeleteProtection.yaml"

requestGetSubscriptionState :: GetSubscriptionState -> TestTree
requestGetSubscriptionState =
  req
    "GetSubscriptionState"
    "fixture/GetSubscriptionState.yaml"

requestDeleteProtectionGroup :: DeleteProtectionGroup -> TestTree
requestDeleteProtectionGroup =
  req
    "DeleteProtectionGroup"
    "fixture/DeleteProtectionGroup.yaml"

requestUpdateProtectionGroup :: UpdateProtectionGroup -> TestTree
requestUpdateProtectionGroup =
  req
    "UpdateProtectionGroup"
    "fixture/UpdateProtectionGroup.yaml"

requestDescribeAttackStatistics :: DescribeAttackStatistics -> TestTree
requestDescribeAttackStatistics =
  req
    "DescribeAttackStatistics"
    "fixture/DescribeAttackStatistics.yaml"

requestDescribeDRTAccess :: DescribeDRTAccess -> TestTree
requestDescribeDRTAccess =
  req
    "DescribeDRTAccess"
    "fixture/DescribeDRTAccess.yaml"

requestDescribeSubscription :: DescribeSubscription -> TestTree
requestDescribeSubscription =
  req
    "DescribeSubscription"
    "fixture/DescribeSubscription.yaml"

requestAssociateHealthCheck :: AssociateHealthCheck -> TestTree
requestAssociateHealthCheck =
  req
    "AssociateHealthCheck"
    "fixture/AssociateHealthCheck.yaml"

requestDescribeProtectionGroup :: DescribeProtectionGroup -> TestTree
requestDescribeProtectionGroup =
  req
    "DescribeProtectionGroup"
    "fixture/DescribeProtectionGroup.yaml"

-- Responses

responseAssociateDRTLogBucket :: AssociateDRTLogBucketResponse -> TestTree
responseAssociateDRTLogBucket =
  res
    "AssociateDRTLogBucketResponse"
    "fixture/AssociateDRTLogBucketResponse.proto"
    shield
    (Proxy :: Proxy AssociateDRTLogBucket)

responseDisassociateDRTRole :: DisassociateDRTRoleResponse -> TestTree
responseDisassociateDRTRole =
  res
    "DisassociateDRTRoleResponse"
    "fixture/DisassociateDRTRoleResponse.proto"
    shield
    (Proxy :: Proxy DisassociateDRTRole)

responseCreateSubscription :: CreateSubscriptionResponse -> TestTree
responseCreateSubscription =
  res
    "CreateSubscriptionResponse"
    "fixture/CreateSubscriptionResponse.proto"
    shield
    (Proxy :: Proxy CreateSubscription)

responseListProtections :: ListProtectionsResponse -> TestTree
responseListProtections =
  res
    "ListProtectionsResponse"
    "fixture/ListProtectionsResponse.proto"
    shield
    (Proxy :: Proxy ListProtections)

responseAssociateDRTRole :: AssociateDRTRoleResponse -> TestTree
responseAssociateDRTRole =
  res
    "AssociateDRTRoleResponse"
    "fixture/AssociateDRTRoleResponse.proto"
    shield
    (Proxy :: Proxy AssociateDRTRole)

responseUpdateSubscription :: UpdateSubscriptionResponse -> TestTree
responseUpdateSubscription =
  res
    "UpdateSubscriptionResponse"
    "fixture/UpdateSubscriptionResponse.proto"
    shield
    (Proxy :: Proxy UpdateSubscription)

responseDisassociateDRTLogBucket :: DisassociateDRTLogBucketResponse -> TestTree
responseDisassociateDRTLogBucket =
  res
    "DisassociateDRTLogBucketResponse"
    "fixture/DisassociateDRTLogBucketResponse.proto"
    shield
    (Proxy :: Proxy DisassociateDRTLogBucket)

responseAssociateProactiveEngagementDetails :: AssociateProactiveEngagementDetailsResponse -> TestTree
responseAssociateProactiveEngagementDetails =
  res
    "AssociateProactiveEngagementDetailsResponse"
    "fixture/AssociateProactiveEngagementDetailsResponse.proto"
    shield
    (Proxy :: Proxy AssociateProactiveEngagementDetails)

responseDescribeAttack :: DescribeAttackResponse -> TestTree
responseDescribeAttack =
  res
    "DescribeAttackResponse"
    "fixture/DescribeAttackResponse.proto"
    shield
    (Proxy :: Proxy DescribeAttack)

responseListProtectionGroups :: ListProtectionGroupsResponse -> TestTree
responseListProtectionGroups =
  res
    "ListProtectionGroupsResponse"
    "fixture/ListProtectionGroupsResponse.proto"
    shield
    (Proxy :: Proxy ListProtectionGroups)

responseEnableProactiveEngagement :: EnableProactiveEngagementResponse -> TestTree
responseEnableProactiveEngagement =
  res
    "EnableProactiveEngagementResponse"
    "fixture/EnableProactiveEngagementResponse.proto"
    shield
    (Proxy :: Proxy EnableProactiveEngagement)

responseUpdateEmergencyContactSettings :: UpdateEmergencyContactSettingsResponse -> TestTree
responseUpdateEmergencyContactSettings =
  res
    "UpdateEmergencyContactSettingsResponse"
    "fixture/UpdateEmergencyContactSettingsResponse.proto"
    shield
    (Proxy :: Proxy UpdateEmergencyContactSettings)

responseCreateProtectionGroup :: CreateProtectionGroupResponse -> TestTree
responseCreateProtectionGroup =
  res
    "CreateProtectionGroupResponse"
    "fixture/CreateProtectionGroupResponse.proto"
    shield
    (Proxy :: Proxy CreateProtectionGroup)

responseDisableProactiveEngagement :: DisableProactiveEngagementResponse -> TestTree
responseDisableProactiveEngagement =
  res
    "DisableProactiveEngagementResponse"
    "fixture/DisableProactiveEngagementResponse.proto"
    shield
    (Proxy :: Proxy DisableProactiveEngagement)

responseDisassociateHealthCheck :: DisassociateHealthCheckResponse -> TestTree
responseDisassociateHealthCheck =
  res
    "DisassociateHealthCheckResponse"
    "fixture/DisassociateHealthCheckResponse.proto"
    shield
    (Proxy :: Proxy DisassociateHealthCheck)

responseListResourcesInProtectionGroup :: ListResourcesInProtectionGroupResponse -> TestTree
responseListResourcesInProtectionGroup =
  res
    "ListResourcesInProtectionGroupResponse"
    "fixture/ListResourcesInProtectionGroupResponse.proto"
    shield
    (Proxy :: Proxy ListResourcesInProtectionGroup)

responseDescribeProtection :: DescribeProtectionResponse -> TestTree
responseDescribeProtection =
  res
    "DescribeProtectionResponse"
    "fixture/DescribeProtectionResponse.proto"
    shield
    (Proxy :: Proxy DescribeProtection)

responseListAttacks :: ListAttacksResponse -> TestTree
responseListAttacks =
  res
    "ListAttacksResponse"
    "fixture/ListAttacksResponse.proto"
    shield
    (Proxy :: Proxy ListAttacks)

responseDescribeEmergencyContactSettings :: DescribeEmergencyContactSettingsResponse -> TestTree
responseDescribeEmergencyContactSettings =
  res
    "DescribeEmergencyContactSettingsResponse"
    "fixture/DescribeEmergencyContactSettingsResponse.proto"
    shield
    (Proxy :: Proxy DescribeEmergencyContactSettings)

responseCreateProtection :: CreateProtectionResponse -> TestTree
responseCreateProtection =
  res
    "CreateProtectionResponse"
    "fixture/CreateProtectionResponse.proto"
    shield
    (Proxy :: Proxy CreateProtection)

responseDeleteProtection :: DeleteProtectionResponse -> TestTree
responseDeleteProtection =
  res
    "DeleteProtectionResponse"
    "fixture/DeleteProtectionResponse.proto"
    shield
    (Proxy :: Proxy DeleteProtection)

responseGetSubscriptionState :: GetSubscriptionStateResponse -> TestTree
responseGetSubscriptionState =
  res
    "GetSubscriptionStateResponse"
    "fixture/GetSubscriptionStateResponse.proto"
    shield
    (Proxy :: Proxy GetSubscriptionState)

responseDeleteProtectionGroup :: DeleteProtectionGroupResponse -> TestTree
responseDeleteProtectionGroup =
  res
    "DeleteProtectionGroupResponse"
    "fixture/DeleteProtectionGroupResponse.proto"
    shield
    (Proxy :: Proxy DeleteProtectionGroup)

responseUpdateProtectionGroup :: UpdateProtectionGroupResponse -> TestTree
responseUpdateProtectionGroup =
  res
    "UpdateProtectionGroupResponse"
    "fixture/UpdateProtectionGroupResponse.proto"
    shield
    (Proxy :: Proxy UpdateProtectionGroup)

responseDescribeAttackStatistics :: DescribeAttackStatisticsResponse -> TestTree
responseDescribeAttackStatistics =
  res
    "DescribeAttackStatisticsResponse"
    "fixture/DescribeAttackStatisticsResponse.proto"
    shield
    (Proxy :: Proxy DescribeAttackStatistics)

responseDescribeDRTAccess :: DescribeDRTAccessResponse -> TestTree
responseDescribeDRTAccess =
  res
    "DescribeDRTAccessResponse"
    "fixture/DescribeDRTAccessResponse.proto"
    shield
    (Proxy :: Proxy DescribeDRTAccess)

responseDescribeSubscription :: DescribeSubscriptionResponse -> TestTree
responseDescribeSubscription =
  res
    "DescribeSubscriptionResponse"
    "fixture/DescribeSubscriptionResponse.proto"
    shield
    (Proxy :: Proxy DescribeSubscription)

responseAssociateHealthCheck :: AssociateHealthCheckResponse -> TestTree
responseAssociateHealthCheck =
  res
    "AssociateHealthCheckResponse"
    "fixture/AssociateHealthCheckResponse.proto"
    shield
    (Proxy :: Proxy AssociateHealthCheck)

responseDescribeProtectionGroup :: DescribeProtectionGroupResponse -> TestTree
responseDescribeProtectionGroup =
  res
    "DescribeProtectionGroupResponse"
    "fixture/DescribeProtectionGroupResponse.proto"
    shield
    (Proxy :: Proxy DescribeProtectionGroup)
