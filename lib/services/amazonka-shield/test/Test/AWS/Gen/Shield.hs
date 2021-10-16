{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Shield
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--         [ requestCreateSubscription $
--             newCreateSubscription
--
--         , requestDescribeProtectionGroup $
--             newDescribeProtectionGroup
--
--         , requestDescribeEmergencyContactSettings $
--             newDescribeEmergencyContactSettings
--
--         , requestAssociateHealthCheck $
--             newAssociateHealthCheck
--
--         , requestDescribeSubscription $
--             newDescribeSubscription
--
--         , requestDescribeDRTAccess $
--             newDescribeDRTAccess
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestTagResource $
--             newTagResource
--
--         , requestDisableProactiveEngagement $
--             newDisableProactiveEngagement
--
--         , requestListResourcesInProtectionGroup $
--             newListResourcesInProtectionGroup
--
--         , requestEnableProactiveEngagement $
--             newEnableProactiveEngagement
--
--         , requestAssociateDRTRole $
--             newAssociateDRTRole
--
--         , requestUpdateSubscription $
--             newUpdateSubscription
--
--         , requestDisassociateDRTLogBucket $
--             newDisassociateDRTLogBucket
--
--         , requestCreateProtection $
--             newCreateProtection
--
--         , requestAssociateDRTLogBucket $
--             newAssociateDRTLogBucket
--
--         , requestDisassociateDRTRole $
--             newDisassociateDRTRole
--
--         , requestListAttacks $
--             newListAttacks
--
--         , requestDescribeProtection $
--             newDescribeProtection
--
--         , requestDescribeAttackStatistics $
--             newDescribeAttackStatistics
--
--         , requestDisassociateHealthCheck $
--             newDisassociateHealthCheck
--
--         , requestCreateProtectionGroup $
--             newCreateProtectionGroup
--
--         , requestDescribeAttack $
--             newDescribeAttack
--
--         , requestDeleteProtectionGroup $
--             newDeleteProtectionGroup
--
--         , requestUpdateEmergencyContactSettings $
--             newUpdateEmergencyContactSettings
--
--         , requestUpdateProtectionGroup $
--             newUpdateProtectionGroup
--
--         , requestListProtectionGroups $
--             newListProtectionGroups
--
--         , requestAssociateProactiveEngagementDetails $
--             newAssociateProactiveEngagementDetails
--
--         , requestListProtections $
--             newListProtections
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDeleteProtection $
--             newDeleteProtection
--
--         , requestGetSubscriptionState $
--             newGetSubscriptionState
--
--           ]

--     , testGroup "response"
--         [ responseCreateSubscription $
--             newCreateSubscriptionResponse
--
--         , responseDescribeProtectionGroup $
--             newDescribeProtectionGroupResponse
--
--         , responseDescribeEmergencyContactSettings $
--             newDescribeEmergencyContactSettingsResponse
--
--         , responseAssociateHealthCheck $
--             newAssociateHealthCheckResponse
--
--         , responseDescribeSubscription $
--             newDescribeSubscriptionResponse
--
--         , responseDescribeDRTAccess $
--             newDescribeDRTAccessResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseDisableProactiveEngagement $
--             newDisableProactiveEngagementResponse
--
--         , responseListResourcesInProtectionGroup $
--             newListResourcesInProtectionGroupResponse
--
--         , responseEnableProactiveEngagement $
--             newEnableProactiveEngagementResponse
--
--         , responseAssociateDRTRole $
--             newAssociateDRTRoleResponse
--
--         , responseUpdateSubscription $
--             newUpdateSubscriptionResponse
--
--         , responseDisassociateDRTLogBucket $
--             newDisassociateDRTLogBucketResponse
--
--         , responseCreateProtection $
--             newCreateProtectionResponse
--
--         , responseAssociateDRTLogBucket $
--             newAssociateDRTLogBucketResponse
--
--         , responseDisassociateDRTRole $
--             newDisassociateDRTRoleResponse
--
--         , responseListAttacks $
--             newListAttacksResponse
--
--         , responseDescribeProtection $
--             newDescribeProtectionResponse
--
--         , responseDescribeAttackStatistics $
--             newDescribeAttackStatisticsResponse
--
--         , responseDisassociateHealthCheck $
--             newDisassociateHealthCheckResponse
--
--         , responseCreateProtectionGroup $
--             newCreateProtectionGroupResponse
--
--         , responseDescribeAttack $
--             newDescribeAttackResponse
--
--         , responseDeleteProtectionGroup $
--             newDeleteProtectionGroupResponse
--
--         , responseUpdateEmergencyContactSettings $
--             newUpdateEmergencyContactSettingsResponse
--
--         , responseUpdateProtectionGroup $
--             newUpdateProtectionGroupResponse
--
--         , responseListProtectionGroups $
--             newListProtectionGroupsResponse
--
--         , responseAssociateProactiveEngagementDetails $
--             newAssociateProactiveEngagementDetailsResponse
--
--         , responseListProtections $
--             newListProtectionsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseDeleteProtection $
--             newDeleteProtectionResponse
--
--         , responseGetSubscriptionState $
--             newGetSubscriptionStateResponse
--
--           ]
--     ]

-- Requests

requestCreateSubscription :: CreateSubscription -> TestTree
requestCreateSubscription =
  req
    "CreateSubscription"
    "fixture/CreateSubscription.yaml"

requestDescribeProtectionGroup :: DescribeProtectionGroup -> TestTree
requestDescribeProtectionGroup =
  req
    "DescribeProtectionGroup"
    "fixture/DescribeProtectionGroup.yaml"

requestDescribeEmergencyContactSettings :: DescribeEmergencyContactSettings -> TestTree
requestDescribeEmergencyContactSettings =
  req
    "DescribeEmergencyContactSettings"
    "fixture/DescribeEmergencyContactSettings.yaml"

requestAssociateHealthCheck :: AssociateHealthCheck -> TestTree
requestAssociateHealthCheck =
  req
    "AssociateHealthCheck"
    "fixture/AssociateHealthCheck.yaml"

requestDescribeSubscription :: DescribeSubscription -> TestTree
requestDescribeSubscription =
  req
    "DescribeSubscription"
    "fixture/DescribeSubscription.yaml"

requestDescribeDRTAccess :: DescribeDRTAccess -> TestTree
requestDescribeDRTAccess =
  req
    "DescribeDRTAccess"
    "fixture/DescribeDRTAccess.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestDisableProactiveEngagement :: DisableProactiveEngagement -> TestTree
requestDisableProactiveEngagement =
  req
    "DisableProactiveEngagement"
    "fixture/DisableProactiveEngagement.yaml"

requestListResourcesInProtectionGroup :: ListResourcesInProtectionGroup -> TestTree
requestListResourcesInProtectionGroup =
  req
    "ListResourcesInProtectionGroup"
    "fixture/ListResourcesInProtectionGroup.yaml"

requestEnableProactiveEngagement :: EnableProactiveEngagement -> TestTree
requestEnableProactiveEngagement =
  req
    "EnableProactiveEngagement"
    "fixture/EnableProactiveEngagement.yaml"

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

requestCreateProtection :: CreateProtection -> TestTree
requestCreateProtection =
  req
    "CreateProtection"
    "fixture/CreateProtection.yaml"

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

requestListAttacks :: ListAttacks -> TestTree
requestListAttacks =
  req
    "ListAttacks"
    "fixture/ListAttacks.yaml"

requestDescribeProtection :: DescribeProtection -> TestTree
requestDescribeProtection =
  req
    "DescribeProtection"
    "fixture/DescribeProtection.yaml"

requestDescribeAttackStatistics :: DescribeAttackStatistics -> TestTree
requestDescribeAttackStatistics =
  req
    "DescribeAttackStatistics"
    "fixture/DescribeAttackStatistics.yaml"

requestDisassociateHealthCheck :: DisassociateHealthCheck -> TestTree
requestDisassociateHealthCheck =
  req
    "DisassociateHealthCheck"
    "fixture/DisassociateHealthCheck.yaml"

requestCreateProtectionGroup :: CreateProtectionGroup -> TestTree
requestCreateProtectionGroup =
  req
    "CreateProtectionGroup"
    "fixture/CreateProtectionGroup.yaml"

requestDescribeAttack :: DescribeAttack -> TestTree
requestDescribeAttack =
  req
    "DescribeAttack"
    "fixture/DescribeAttack.yaml"

requestDeleteProtectionGroup :: DeleteProtectionGroup -> TestTree
requestDeleteProtectionGroup =
  req
    "DeleteProtectionGroup"
    "fixture/DeleteProtectionGroup.yaml"

requestUpdateEmergencyContactSettings :: UpdateEmergencyContactSettings -> TestTree
requestUpdateEmergencyContactSettings =
  req
    "UpdateEmergencyContactSettings"
    "fixture/UpdateEmergencyContactSettings.yaml"

requestUpdateProtectionGroup :: UpdateProtectionGroup -> TestTree
requestUpdateProtectionGroup =
  req
    "UpdateProtectionGroup"
    "fixture/UpdateProtectionGroup.yaml"

requestListProtectionGroups :: ListProtectionGroups -> TestTree
requestListProtectionGroups =
  req
    "ListProtectionGroups"
    "fixture/ListProtectionGroups.yaml"

requestAssociateProactiveEngagementDetails :: AssociateProactiveEngagementDetails -> TestTree
requestAssociateProactiveEngagementDetails =
  req
    "AssociateProactiveEngagementDetails"
    "fixture/AssociateProactiveEngagementDetails.yaml"

requestListProtections :: ListProtections -> TestTree
requestListProtections =
  req
    "ListProtections"
    "fixture/ListProtections.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

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

-- Responses

responseCreateSubscription :: CreateSubscriptionResponse -> TestTree
responseCreateSubscription =
  res
    "CreateSubscriptionResponse"
    "fixture/CreateSubscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSubscription)

responseDescribeProtectionGroup :: DescribeProtectionGroupResponse -> TestTree
responseDescribeProtectionGroup =
  res
    "DescribeProtectionGroupResponse"
    "fixture/DescribeProtectionGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeProtectionGroup)

responseDescribeEmergencyContactSettings :: DescribeEmergencyContactSettingsResponse -> TestTree
responseDescribeEmergencyContactSettings =
  res
    "DescribeEmergencyContactSettingsResponse"
    "fixture/DescribeEmergencyContactSettingsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEmergencyContactSettings)

responseAssociateHealthCheck :: AssociateHealthCheckResponse -> TestTree
responseAssociateHealthCheck =
  res
    "AssociateHealthCheckResponse"
    "fixture/AssociateHealthCheckResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateHealthCheck)

responseDescribeSubscription :: DescribeSubscriptionResponse -> TestTree
responseDescribeSubscription =
  res
    "DescribeSubscriptionResponse"
    "fixture/DescribeSubscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSubscription)

responseDescribeDRTAccess :: DescribeDRTAccessResponse -> TestTree
responseDescribeDRTAccess =
  res
    "DescribeDRTAccessResponse"
    "fixture/DescribeDRTAccessResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDRTAccess)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseDisableProactiveEngagement :: DisableProactiveEngagementResponse -> TestTree
responseDisableProactiveEngagement =
  res
    "DisableProactiveEngagementResponse"
    "fixture/DisableProactiveEngagementResponse.proto"
    defaultService
    (Proxy :: Proxy DisableProactiveEngagement)

responseListResourcesInProtectionGroup :: ListResourcesInProtectionGroupResponse -> TestTree
responseListResourcesInProtectionGroup =
  res
    "ListResourcesInProtectionGroupResponse"
    "fixture/ListResourcesInProtectionGroupResponse.proto"
    defaultService
    (Proxy :: Proxy ListResourcesInProtectionGroup)

responseEnableProactiveEngagement :: EnableProactiveEngagementResponse -> TestTree
responseEnableProactiveEngagement =
  res
    "EnableProactiveEngagementResponse"
    "fixture/EnableProactiveEngagementResponse.proto"
    defaultService
    (Proxy :: Proxy EnableProactiveEngagement)

responseAssociateDRTRole :: AssociateDRTRoleResponse -> TestTree
responseAssociateDRTRole =
  res
    "AssociateDRTRoleResponse"
    "fixture/AssociateDRTRoleResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateDRTRole)

responseUpdateSubscription :: UpdateSubscriptionResponse -> TestTree
responseUpdateSubscription =
  res
    "UpdateSubscriptionResponse"
    "fixture/UpdateSubscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSubscription)

responseDisassociateDRTLogBucket :: DisassociateDRTLogBucketResponse -> TestTree
responseDisassociateDRTLogBucket =
  res
    "DisassociateDRTLogBucketResponse"
    "fixture/DisassociateDRTLogBucketResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateDRTLogBucket)

responseCreateProtection :: CreateProtectionResponse -> TestTree
responseCreateProtection =
  res
    "CreateProtectionResponse"
    "fixture/CreateProtectionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateProtection)

responseAssociateDRTLogBucket :: AssociateDRTLogBucketResponse -> TestTree
responseAssociateDRTLogBucket =
  res
    "AssociateDRTLogBucketResponse"
    "fixture/AssociateDRTLogBucketResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateDRTLogBucket)

responseDisassociateDRTRole :: DisassociateDRTRoleResponse -> TestTree
responseDisassociateDRTRole =
  res
    "DisassociateDRTRoleResponse"
    "fixture/DisassociateDRTRoleResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateDRTRole)

responseListAttacks :: ListAttacksResponse -> TestTree
responseListAttacks =
  res
    "ListAttacksResponse"
    "fixture/ListAttacksResponse.proto"
    defaultService
    (Proxy :: Proxy ListAttacks)

responseDescribeProtection :: DescribeProtectionResponse -> TestTree
responseDescribeProtection =
  res
    "DescribeProtectionResponse"
    "fixture/DescribeProtectionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeProtection)

responseDescribeAttackStatistics :: DescribeAttackStatisticsResponse -> TestTree
responseDescribeAttackStatistics =
  res
    "DescribeAttackStatisticsResponse"
    "fixture/DescribeAttackStatisticsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAttackStatistics)

responseDisassociateHealthCheck :: DisassociateHealthCheckResponse -> TestTree
responseDisassociateHealthCheck =
  res
    "DisassociateHealthCheckResponse"
    "fixture/DisassociateHealthCheckResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateHealthCheck)

responseCreateProtectionGroup :: CreateProtectionGroupResponse -> TestTree
responseCreateProtectionGroup =
  res
    "CreateProtectionGroupResponse"
    "fixture/CreateProtectionGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateProtectionGroup)

responseDescribeAttack :: DescribeAttackResponse -> TestTree
responseDescribeAttack =
  res
    "DescribeAttackResponse"
    "fixture/DescribeAttackResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAttack)

responseDeleteProtectionGroup :: DeleteProtectionGroupResponse -> TestTree
responseDeleteProtectionGroup =
  res
    "DeleteProtectionGroupResponse"
    "fixture/DeleteProtectionGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteProtectionGroup)

responseUpdateEmergencyContactSettings :: UpdateEmergencyContactSettingsResponse -> TestTree
responseUpdateEmergencyContactSettings =
  res
    "UpdateEmergencyContactSettingsResponse"
    "fixture/UpdateEmergencyContactSettingsResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateEmergencyContactSettings)

responseUpdateProtectionGroup :: UpdateProtectionGroupResponse -> TestTree
responseUpdateProtectionGroup =
  res
    "UpdateProtectionGroupResponse"
    "fixture/UpdateProtectionGroupResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateProtectionGroup)

responseListProtectionGroups :: ListProtectionGroupsResponse -> TestTree
responseListProtectionGroups =
  res
    "ListProtectionGroupsResponse"
    "fixture/ListProtectionGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy ListProtectionGroups)

responseAssociateProactiveEngagementDetails :: AssociateProactiveEngagementDetailsResponse -> TestTree
responseAssociateProactiveEngagementDetails =
  res
    "AssociateProactiveEngagementDetailsResponse"
    "fixture/AssociateProactiveEngagementDetailsResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateProactiveEngagementDetails)

responseListProtections :: ListProtectionsResponse -> TestTree
responseListProtections =
  res
    "ListProtectionsResponse"
    "fixture/ListProtectionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListProtections)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseDeleteProtection :: DeleteProtectionResponse -> TestTree
responseDeleteProtection =
  res
    "DeleteProtectionResponse"
    "fixture/DeleteProtectionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteProtection)

responseGetSubscriptionState :: GetSubscriptionStateResponse -> TestTree
responseGetSubscriptionState =
  res
    "GetSubscriptionStateResponse"
    "fixture/GetSubscriptionStateResponse.proto"
    defaultService
    (Proxy :: Proxy GetSubscriptionState)
