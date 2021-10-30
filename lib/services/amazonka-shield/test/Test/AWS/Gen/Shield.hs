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

import qualified Data.Proxy as Proxy
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
--             newAssociateDRTLogBucket
--
--         , requestDisassociateDRTRole $
--             newDisassociateDRTRole
--
--         , requestCreateSubscription $
--             newCreateSubscription
--
--         , requestListProtections $
--             newListProtections
--
--         , requestAssociateDRTRole $
--             newAssociateDRTRole
--
--         , requestUpdateSubscription $
--             newUpdateSubscription
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDisassociateDRTLogBucket $
--             newDisassociateDRTLogBucket
--
--         , requestAssociateProactiveEngagementDetails $
--             newAssociateProactiveEngagementDetails
--
--         , requestDescribeAttack $
--             newDescribeAttack
--
--         , requestListProtectionGroups $
--             newListProtectionGroups
--
--         , requestEnableProactiveEngagement $
--             newEnableProactiveEngagement
--
--         , requestUpdateEmergencyContactSettings $
--             newUpdateEmergencyContactSettings
--
--         , requestCreateProtectionGroup $
--             newCreateProtectionGroup
--
--         , requestDisableProactiveEngagement $
--             newDisableProactiveEngagement
--
--         , requestDisassociateHealthCheck $
--             newDisassociateHealthCheck
--
--         , requestListResourcesInProtectionGroup $
--             newListResourcesInProtectionGroup
--
--         , requestDescribeProtection $
--             newDescribeProtection
--
--         , requestListAttacks $
--             newListAttacks
--
--         , requestDescribeEmergencyContactSettings $
--             newDescribeEmergencyContactSettings
--
--         , requestCreateProtection $
--             newCreateProtection
--
--         , requestDeleteProtection $
--             newDeleteProtection
--
--         , requestGetSubscriptionState $
--             newGetSubscriptionState
--
--         , requestDeleteProtectionGroup $
--             newDeleteProtectionGroup
--
--         , requestUpdateProtectionGroup $
--             newUpdateProtectionGroup
--
--         , requestDescribeAttackStatistics $
--             newDescribeAttackStatistics
--
--         , requestTagResource $
--             newTagResource
--
--         , requestDescribeDRTAccess $
--             newDescribeDRTAccess
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDescribeSubscription $
--             newDescribeSubscription
--
--         , requestAssociateHealthCheck $
--             newAssociateHealthCheck
--
--         , requestDescribeProtectionGroup $
--             newDescribeProtectionGroup
--
--           ]

--     , testGroup "response"
--         [ responseAssociateDRTLogBucket $
--             newAssociateDRTLogBucketResponse
--
--         , responseDisassociateDRTRole $
--             newDisassociateDRTRoleResponse
--
--         , responseCreateSubscription $
--             newCreateSubscriptionResponse
--
--         , responseListProtections $
--             newListProtectionsResponse
--
--         , responseAssociateDRTRole $
--             newAssociateDRTRoleResponse
--
--         , responseUpdateSubscription $
--             newUpdateSubscriptionResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseDisassociateDRTLogBucket $
--             newDisassociateDRTLogBucketResponse
--
--         , responseAssociateProactiveEngagementDetails $
--             newAssociateProactiveEngagementDetailsResponse
--
--         , responseDescribeAttack $
--             newDescribeAttackResponse
--
--         , responseListProtectionGroups $
--             newListProtectionGroupsResponse
--
--         , responseEnableProactiveEngagement $
--             newEnableProactiveEngagementResponse
--
--         , responseUpdateEmergencyContactSettings $
--             newUpdateEmergencyContactSettingsResponse
--
--         , responseCreateProtectionGroup $
--             newCreateProtectionGroupResponse
--
--         , responseDisableProactiveEngagement $
--             newDisableProactiveEngagementResponse
--
--         , responseDisassociateHealthCheck $
--             newDisassociateHealthCheckResponse
--
--         , responseListResourcesInProtectionGroup $
--             newListResourcesInProtectionGroupResponse
--
--         , responseDescribeProtection $
--             newDescribeProtectionResponse
--
--         , responseListAttacks $
--             newListAttacksResponse
--
--         , responseDescribeEmergencyContactSettings $
--             newDescribeEmergencyContactSettingsResponse
--
--         , responseCreateProtection $
--             newCreateProtectionResponse
--
--         , responseDeleteProtection $
--             newDeleteProtectionResponse
--
--         , responseGetSubscriptionState $
--             newGetSubscriptionStateResponse
--
--         , responseDeleteProtectionGroup $
--             newDeleteProtectionGroupResponse
--
--         , responseUpdateProtectionGroup $
--             newUpdateProtectionGroupResponse
--
--         , responseDescribeAttackStatistics $
--             newDescribeAttackStatisticsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseDescribeDRTAccess $
--             newDescribeDRTAccessResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDescribeSubscription $
--             newDescribeSubscriptionResponse
--
--         , responseAssociateHealthCheck $
--             newAssociateHealthCheckResponse
--
--         , responseDescribeProtectionGroup $
--             newDescribeProtectionGroupResponse
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

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

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

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

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
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateDRTLogBucket)

responseDisassociateDRTRole :: DisassociateDRTRoleResponse -> TestTree
responseDisassociateDRTRole =
  res
    "DisassociateDRTRoleResponse"
    "fixture/DisassociateDRTRoleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateDRTRole)

responseCreateSubscription :: CreateSubscriptionResponse -> TestTree
responseCreateSubscription =
  res
    "CreateSubscriptionResponse"
    "fixture/CreateSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSubscription)

responseListProtections :: ListProtectionsResponse -> TestTree
responseListProtections =
  res
    "ListProtectionsResponse"
    "fixture/ListProtectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProtections)

responseAssociateDRTRole :: AssociateDRTRoleResponse -> TestTree
responseAssociateDRTRole =
  res
    "AssociateDRTRoleResponse"
    "fixture/AssociateDRTRoleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateDRTRole)

responseUpdateSubscription :: UpdateSubscriptionResponse -> TestTree
responseUpdateSubscription =
  res
    "UpdateSubscriptionResponse"
    "fixture/UpdateSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSubscription)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseDisassociateDRTLogBucket :: DisassociateDRTLogBucketResponse -> TestTree
responseDisassociateDRTLogBucket =
  res
    "DisassociateDRTLogBucketResponse"
    "fixture/DisassociateDRTLogBucketResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateDRTLogBucket)

responseAssociateProactiveEngagementDetails :: AssociateProactiveEngagementDetailsResponse -> TestTree
responseAssociateProactiveEngagementDetails =
  res
    "AssociateProactiveEngagementDetailsResponse"
    "fixture/AssociateProactiveEngagementDetailsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateProactiveEngagementDetails)

responseDescribeAttack :: DescribeAttackResponse -> TestTree
responseDescribeAttack =
  res
    "DescribeAttackResponse"
    "fixture/DescribeAttackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAttack)

responseListProtectionGroups :: ListProtectionGroupsResponse -> TestTree
responseListProtectionGroups =
  res
    "ListProtectionGroupsResponse"
    "fixture/ListProtectionGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProtectionGroups)

responseEnableProactiveEngagement :: EnableProactiveEngagementResponse -> TestTree
responseEnableProactiveEngagement =
  res
    "EnableProactiveEngagementResponse"
    "fixture/EnableProactiveEngagementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableProactiveEngagement)

responseUpdateEmergencyContactSettings :: UpdateEmergencyContactSettingsResponse -> TestTree
responseUpdateEmergencyContactSettings =
  res
    "UpdateEmergencyContactSettingsResponse"
    "fixture/UpdateEmergencyContactSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEmergencyContactSettings)

responseCreateProtectionGroup :: CreateProtectionGroupResponse -> TestTree
responseCreateProtectionGroup =
  res
    "CreateProtectionGroupResponse"
    "fixture/CreateProtectionGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProtectionGroup)

responseDisableProactiveEngagement :: DisableProactiveEngagementResponse -> TestTree
responseDisableProactiveEngagement =
  res
    "DisableProactiveEngagementResponse"
    "fixture/DisableProactiveEngagementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableProactiveEngagement)

responseDisassociateHealthCheck :: DisassociateHealthCheckResponse -> TestTree
responseDisassociateHealthCheck =
  res
    "DisassociateHealthCheckResponse"
    "fixture/DisassociateHealthCheckResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateHealthCheck)

responseListResourcesInProtectionGroup :: ListResourcesInProtectionGroupResponse -> TestTree
responseListResourcesInProtectionGroup =
  res
    "ListResourcesInProtectionGroupResponse"
    "fixture/ListResourcesInProtectionGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResourcesInProtectionGroup)

responseDescribeProtection :: DescribeProtectionResponse -> TestTree
responseDescribeProtection =
  res
    "DescribeProtectionResponse"
    "fixture/DescribeProtectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProtection)

responseListAttacks :: ListAttacksResponse -> TestTree
responseListAttacks =
  res
    "ListAttacksResponse"
    "fixture/ListAttacksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAttacks)

responseDescribeEmergencyContactSettings :: DescribeEmergencyContactSettingsResponse -> TestTree
responseDescribeEmergencyContactSettings =
  res
    "DescribeEmergencyContactSettingsResponse"
    "fixture/DescribeEmergencyContactSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEmergencyContactSettings)

responseCreateProtection :: CreateProtectionResponse -> TestTree
responseCreateProtection =
  res
    "CreateProtectionResponse"
    "fixture/CreateProtectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProtection)

responseDeleteProtection :: DeleteProtectionResponse -> TestTree
responseDeleteProtection =
  res
    "DeleteProtectionResponse"
    "fixture/DeleteProtectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProtection)

responseGetSubscriptionState :: GetSubscriptionStateResponse -> TestTree
responseGetSubscriptionState =
  res
    "GetSubscriptionStateResponse"
    "fixture/GetSubscriptionStateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSubscriptionState)

responseDeleteProtectionGroup :: DeleteProtectionGroupResponse -> TestTree
responseDeleteProtectionGroup =
  res
    "DeleteProtectionGroupResponse"
    "fixture/DeleteProtectionGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProtectionGroup)

responseUpdateProtectionGroup :: UpdateProtectionGroupResponse -> TestTree
responseUpdateProtectionGroup =
  res
    "UpdateProtectionGroupResponse"
    "fixture/UpdateProtectionGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateProtectionGroup)

responseDescribeAttackStatistics :: DescribeAttackStatisticsResponse -> TestTree
responseDescribeAttackStatistics =
  res
    "DescribeAttackStatisticsResponse"
    "fixture/DescribeAttackStatisticsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAttackStatistics)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseDescribeDRTAccess :: DescribeDRTAccessResponse -> TestTree
responseDescribeDRTAccess =
  res
    "DescribeDRTAccessResponse"
    "fixture/DescribeDRTAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDRTAccess)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseDescribeSubscription :: DescribeSubscriptionResponse -> TestTree
responseDescribeSubscription =
  res
    "DescribeSubscriptionResponse"
    "fixture/DescribeSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSubscription)

responseAssociateHealthCheck :: AssociateHealthCheckResponse -> TestTree
responseAssociateHealthCheck =
  res
    "AssociateHealthCheckResponse"
    "fixture/AssociateHealthCheckResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateHealthCheck)

responseDescribeProtectionGroup :: DescribeProtectionGroupResponse -> TestTree
responseDescribeProtectionGroup =
  res
    "DescribeProtectionGroupResponse"
    "fixture/DescribeProtectionGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProtectionGroup)
