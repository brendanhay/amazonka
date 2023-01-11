{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Shield
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Shield where

import Amazonka.Shield
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.Shield.Internal
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
--         , requestAssociateDRTRole $
--             newAssociateDRTRole
--
--         , requestAssociateHealthCheck $
--             newAssociateHealthCheck
--
--         , requestAssociateProactiveEngagementDetails $
--             newAssociateProactiveEngagementDetails
--
--         , requestCreateProtection $
--             newCreateProtection
--
--         , requestCreateProtectionGroup $
--             newCreateProtectionGroup
--
--         , requestCreateSubscription $
--             newCreateSubscription
--
--         , requestDeleteProtection $
--             newDeleteProtection
--
--         , requestDeleteProtectionGroup $
--             newDeleteProtectionGroup
--
--         , requestDescribeAttack $
--             newDescribeAttack
--
--         , requestDescribeAttackStatistics $
--             newDescribeAttackStatistics
--
--         , requestDescribeDRTAccess $
--             newDescribeDRTAccess
--
--         , requestDescribeEmergencyContactSettings $
--             newDescribeEmergencyContactSettings
--
--         , requestDescribeProtection $
--             newDescribeProtection
--
--         , requestDescribeProtectionGroup $
--             newDescribeProtectionGroup
--
--         , requestDescribeSubscription $
--             newDescribeSubscription
--
--         , requestDisableApplicationLayerAutomaticResponse $
--             newDisableApplicationLayerAutomaticResponse
--
--         , requestDisableProactiveEngagement $
--             newDisableProactiveEngagement
--
--         , requestDisassociateDRTLogBucket $
--             newDisassociateDRTLogBucket
--
--         , requestDisassociateDRTRole $
--             newDisassociateDRTRole
--
--         , requestDisassociateHealthCheck $
--             newDisassociateHealthCheck
--
--         , requestEnableApplicationLayerAutomaticResponse $
--             newEnableApplicationLayerAutomaticResponse
--
--         , requestEnableProactiveEngagement $
--             newEnableProactiveEngagement
--
--         , requestGetSubscriptionState $
--             newGetSubscriptionState
--
--         , requestListAttacks $
--             newListAttacks
--
--         , requestListProtectionGroups $
--             newListProtectionGroups
--
--         , requestListProtections $
--             newListProtections
--
--         , requestListResourcesInProtectionGroup $
--             newListResourcesInProtectionGroup
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateApplicationLayerAutomaticResponse $
--             newUpdateApplicationLayerAutomaticResponse
--
--         , requestUpdateEmergencyContactSettings $
--             newUpdateEmergencyContactSettings
--
--         , requestUpdateProtectionGroup $
--             newUpdateProtectionGroup
--
--         , requestUpdateSubscription $
--             newUpdateSubscription
--
--           ]

--     , testGroup "response"
--         [ responseAssociateDRTLogBucket $
--             newAssociateDRTLogBucketResponse
--
--         , responseAssociateDRTRole $
--             newAssociateDRTRoleResponse
--
--         , responseAssociateHealthCheck $
--             newAssociateHealthCheckResponse
--
--         , responseAssociateProactiveEngagementDetails $
--             newAssociateProactiveEngagementDetailsResponse
--
--         , responseCreateProtection $
--             newCreateProtectionResponse
--
--         , responseCreateProtectionGroup $
--             newCreateProtectionGroupResponse
--
--         , responseCreateSubscription $
--             newCreateSubscriptionResponse
--
--         , responseDeleteProtection $
--             newDeleteProtectionResponse
--
--         , responseDeleteProtectionGroup $
--             newDeleteProtectionGroupResponse
--
--         , responseDescribeAttack $
--             newDescribeAttackResponse
--
--         , responseDescribeAttackStatistics $
--             newDescribeAttackStatisticsResponse
--
--         , responseDescribeDRTAccess $
--             newDescribeDRTAccessResponse
--
--         , responseDescribeEmergencyContactSettings $
--             newDescribeEmergencyContactSettingsResponse
--
--         , responseDescribeProtection $
--             newDescribeProtectionResponse
--
--         , responseDescribeProtectionGroup $
--             newDescribeProtectionGroupResponse
--
--         , responseDescribeSubscription $
--             newDescribeSubscriptionResponse
--
--         , responseDisableApplicationLayerAutomaticResponse $
--             newDisableApplicationLayerAutomaticResponseResponse
--
--         , responseDisableProactiveEngagement $
--             newDisableProactiveEngagementResponse
--
--         , responseDisassociateDRTLogBucket $
--             newDisassociateDRTLogBucketResponse
--
--         , responseDisassociateDRTRole $
--             newDisassociateDRTRoleResponse
--
--         , responseDisassociateHealthCheck $
--             newDisassociateHealthCheckResponse
--
--         , responseEnableApplicationLayerAutomaticResponse $
--             newEnableApplicationLayerAutomaticResponseResponse
--
--         , responseEnableProactiveEngagement $
--             newEnableProactiveEngagementResponse
--
--         , responseGetSubscriptionState $
--             newGetSubscriptionStateResponse
--
--         , responseListAttacks $
--             newListAttacksResponse
--
--         , responseListProtectionGroups $
--             newListProtectionGroupsResponse
--
--         , responseListProtections $
--             newListProtectionsResponse
--
--         , responseListResourcesInProtectionGroup $
--             newListResourcesInProtectionGroupResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateApplicationLayerAutomaticResponse $
--             newUpdateApplicationLayerAutomaticResponseResponse
--
--         , responseUpdateEmergencyContactSettings $
--             newUpdateEmergencyContactSettingsResponse
--
--         , responseUpdateProtectionGroup $
--             newUpdateProtectionGroupResponse
--
--         , responseUpdateSubscription $
--             newUpdateSubscriptionResponse
--
--           ]
--     ]

-- Requests

requestAssociateDRTLogBucket :: AssociateDRTLogBucket -> TestTree
requestAssociateDRTLogBucket =
  req
    "AssociateDRTLogBucket"
    "fixture/AssociateDRTLogBucket.yaml"

requestAssociateDRTRole :: AssociateDRTRole -> TestTree
requestAssociateDRTRole =
  req
    "AssociateDRTRole"
    "fixture/AssociateDRTRole.yaml"

requestAssociateHealthCheck :: AssociateHealthCheck -> TestTree
requestAssociateHealthCheck =
  req
    "AssociateHealthCheck"
    "fixture/AssociateHealthCheck.yaml"

requestAssociateProactiveEngagementDetails :: AssociateProactiveEngagementDetails -> TestTree
requestAssociateProactiveEngagementDetails =
  req
    "AssociateProactiveEngagementDetails"
    "fixture/AssociateProactiveEngagementDetails.yaml"

requestCreateProtection :: CreateProtection -> TestTree
requestCreateProtection =
  req
    "CreateProtection"
    "fixture/CreateProtection.yaml"

requestCreateProtectionGroup :: CreateProtectionGroup -> TestTree
requestCreateProtectionGroup =
  req
    "CreateProtectionGroup"
    "fixture/CreateProtectionGroup.yaml"

requestCreateSubscription :: CreateSubscription -> TestTree
requestCreateSubscription =
  req
    "CreateSubscription"
    "fixture/CreateSubscription.yaml"

requestDeleteProtection :: DeleteProtection -> TestTree
requestDeleteProtection =
  req
    "DeleteProtection"
    "fixture/DeleteProtection.yaml"

requestDeleteProtectionGroup :: DeleteProtectionGroup -> TestTree
requestDeleteProtectionGroup =
  req
    "DeleteProtectionGroup"
    "fixture/DeleteProtectionGroup.yaml"

requestDescribeAttack :: DescribeAttack -> TestTree
requestDescribeAttack =
  req
    "DescribeAttack"
    "fixture/DescribeAttack.yaml"

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

requestDescribeEmergencyContactSettings :: DescribeEmergencyContactSettings -> TestTree
requestDescribeEmergencyContactSettings =
  req
    "DescribeEmergencyContactSettings"
    "fixture/DescribeEmergencyContactSettings.yaml"

requestDescribeProtection :: DescribeProtection -> TestTree
requestDescribeProtection =
  req
    "DescribeProtection"
    "fixture/DescribeProtection.yaml"

requestDescribeProtectionGroup :: DescribeProtectionGroup -> TestTree
requestDescribeProtectionGroup =
  req
    "DescribeProtectionGroup"
    "fixture/DescribeProtectionGroup.yaml"

requestDescribeSubscription :: DescribeSubscription -> TestTree
requestDescribeSubscription =
  req
    "DescribeSubscription"
    "fixture/DescribeSubscription.yaml"

requestDisableApplicationLayerAutomaticResponse :: DisableApplicationLayerAutomaticResponse -> TestTree
requestDisableApplicationLayerAutomaticResponse =
  req
    "DisableApplicationLayerAutomaticResponse"
    "fixture/DisableApplicationLayerAutomaticResponse.yaml"

requestDisableProactiveEngagement :: DisableProactiveEngagement -> TestTree
requestDisableProactiveEngagement =
  req
    "DisableProactiveEngagement"
    "fixture/DisableProactiveEngagement.yaml"

requestDisassociateDRTLogBucket :: DisassociateDRTLogBucket -> TestTree
requestDisassociateDRTLogBucket =
  req
    "DisassociateDRTLogBucket"
    "fixture/DisassociateDRTLogBucket.yaml"

requestDisassociateDRTRole :: DisassociateDRTRole -> TestTree
requestDisassociateDRTRole =
  req
    "DisassociateDRTRole"
    "fixture/DisassociateDRTRole.yaml"

requestDisassociateHealthCheck :: DisassociateHealthCheck -> TestTree
requestDisassociateHealthCheck =
  req
    "DisassociateHealthCheck"
    "fixture/DisassociateHealthCheck.yaml"

requestEnableApplicationLayerAutomaticResponse :: EnableApplicationLayerAutomaticResponse -> TestTree
requestEnableApplicationLayerAutomaticResponse =
  req
    "EnableApplicationLayerAutomaticResponse"
    "fixture/EnableApplicationLayerAutomaticResponse.yaml"

requestEnableProactiveEngagement :: EnableProactiveEngagement -> TestTree
requestEnableProactiveEngagement =
  req
    "EnableProactiveEngagement"
    "fixture/EnableProactiveEngagement.yaml"

requestGetSubscriptionState :: GetSubscriptionState -> TestTree
requestGetSubscriptionState =
  req
    "GetSubscriptionState"
    "fixture/GetSubscriptionState.yaml"

requestListAttacks :: ListAttacks -> TestTree
requestListAttacks =
  req
    "ListAttacks"
    "fixture/ListAttacks.yaml"

requestListProtectionGroups :: ListProtectionGroups -> TestTree
requestListProtectionGroups =
  req
    "ListProtectionGroups"
    "fixture/ListProtectionGroups.yaml"

requestListProtections :: ListProtections -> TestTree
requestListProtections =
  req
    "ListProtections"
    "fixture/ListProtections.yaml"

requestListResourcesInProtectionGroup :: ListResourcesInProtectionGroup -> TestTree
requestListResourcesInProtectionGroup =
  req
    "ListResourcesInProtectionGroup"
    "fixture/ListResourcesInProtectionGroup.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateApplicationLayerAutomaticResponse :: UpdateApplicationLayerAutomaticResponse -> TestTree
requestUpdateApplicationLayerAutomaticResponse =
  req
    "UpdateApplicationLayerAutomaticResponse"
    "fixture/UpdateApplicationLayerAutomaticResponse.yaml"

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

requestUpdateSubscription :: UpdateSubscription -> TestTree
requestUpdateSubscription =
  req
    "UpdateSubscription"
    "fixture/UpdateSubscription.yaml"

-- Responses

responseAssociateDRTLogBucket :: AssociateDRTLogBucketResponse -> TestTree
responseAssociateDRTLogBucket =
  res
    "AssociateDRTLogBucketResponse"
    "fixture/AssociateDRTLogBucketResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateDRTLogBucket)

responseAssociateDRTRole :: AssociateDRTRoleResponse -> TestTree
responseAssociateDRTRole =
  res
    "AssociateDRTRoleResponse"
    "fixture/AssociateDRTRoleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateDRTRole)

responseAssociateHealthCheck :: AssociateHealthCheckResponse -> TestTree
responseAssociateHealthCheck =
  res
    "AssociateHealthCheckResponse"
    "fixture/AssociateHealthCheckResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateHealthCheck)

responseAssociateProactiveEngagementDetails :: AssociateProactiveEngagementDetailsResponse -> TestTree
responseAssociateProactiveEngagementDetails =
  res
    "AssociateProactiveEngagementDetailsResponse"
    "fixture/AssociateProactiveEngagementDetailsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateProactiveEngagementDetails)

responseCreateProtection :: CreateProtectionResponse -> TestTree
responseCreateProtection =
  res
    "CreateProtectionResponse"
    "fixture/CreateProtectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProtection)

responseCreateProtectionGroup :: CreateProtectionGroupResponse -> TestTree
responseCreateProtectionGroup =
  res
    "CreateProtectionGroupResponse"
    "fixture/CreateProtectionGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProtectionGroup)

responseCreateSubscription :: CreateSubscriptionResponse -> TestTree
responseCreateSubscription =
  res
    "CreateSubscriptionResponse"
    "fixture/CreateSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSubscription)

responseDeleteProtection :: DeleteProtectionResponse -> TestTree
responseDeleteProtection =
  res
    "DeleteProtectionResponse"
    "fixture/DeleteProtectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProtection)

responseDeleteProtectionGroup :: DeleteProtectionGroupResponse -> TestTree
responseDeleteProtectionGroup =
  res
    "DeleteProtectionGroupResponse"
    "fixture/DeleteProtectionGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProtectionGroup)

responseDescribeAttack :: DescribeAttackResponse -> TestTree
responseDescribeAttack =
  res
    "DescribeAttackResponse"
    "fixture/DescribeAttackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAttack)

responseDescribeAttackStatistics :: DescribeAttackStatisticsResponse -> TestTree
responseDescribeAttackStatistics =
  res
    "DescribeAttackStatisticsResponse"
    "fixture/DescribeAttackStatisticsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAttackStatistics)

responseDescribeDRTAccess :: DescribeDRTAccessResponse -> TestTree
responseDescribeDRTAccess =
  res
    "DescribeDRTAccessResponse"
    "fixture/DescribeDRTAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDRTAccess)

responseDescribeEmergencyContactSettings :: DescribeEmergencyContactSettingsResponse -> TestTree
responseDescribeEmergencyContactSettings =
  res
    "DescribeEmergencyContactSettingsResponse"
    "fixture/DescribeEmergencyContactSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEmergencyContactSettings)

responseDescribeProtection :: DescribeProtectionResponse -> TestTree
responseDescribeProtection =
  res
    "DescribeProtectionResponse"
    "fixture/DescribeProtectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProtection)

responseDescribeProtectionGroup :: DescribeProtectionGroupResponse -> TestTree
responseDescribeProtectionGroup =
  res
    "DescribeProtectionGroupResponse"
    "fixture/DescribeProtectionGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProtectionGroup)

responseDescribeSubscription :: DescribeSubscriptionResponse -> TestTree
responseDescribeSubscription =
  res
    "DescribeSubscriptionResponse"
    "fixture/DescribeSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSubscription)

responseDisableApplicationLayerAutomaticResponse :: DisableApplicationLayerAutomaticResponseResponse -> TestTree
responseDisableApplicationLayerAutomaticResponse =
  res
    "DisableApplicationLayerAutomaticResponseResponse"
    "fixture/DisableApplicationLayerAutomaticResponseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableApplicationLayerAutomaticResponse)

responseDisableProactiveEngagement :: DisableProactiveEngagementResponse -> TestTree
responseDisableProactiveEngagement =
  res
    "DisableProactiveEngagementResponse"
    "fixture/DisableProactiveEngagementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableProactiveEngagement)

responseDisassociateDRTLogBucket :: DisassociateDRTLogBucketResponse -> TestTree
responseDisassociateDRTLogBucket =
  res
    "DisassociateDRTLogBucketResponse"
    "fixture/DisassociateDRTLogBucketResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateDRTLogBucket)

responseDisassociateDRTRole :: DisassociateDRTRoleResponse -> TestTree
responseDisassociateDRTRole =
  res
    "DisassociateDRTRoleResponse"
    "fixture/DisassociateDRTRoleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateDRTRole)

responseDisassociateHealthCheck :: DisassociateHealthCheckResponse -> TestTree
responseDisassociateHealthCheck =
  res
    "DisassociateHealthCheckResponse"
    "fixture/DisassociateHealthCheckResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateHealthCheck)

responseEnableApplicationLayerAutomaticResponse :: EnableApplicationLayerAutomaticResponseResponse -> TestTree
responseEnableApplicationLayerAutomaticResponse =
  res
    "EnableApplicationLayerAutomaticResponseResponse"
    "fixture/EnableApplicationLayerAutomaticResponseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableApplicationLayerAutomaticResponse)

responseEnableProactiveEngagement :: EnableProactiveEngagementResponse -> TestTree
responseEnableProactiveEngagement =
  res
    "EnableProactiveEngagementResponse"
    "fixture/EnableProactiveEngagementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableProactiveEngagement)

responseGetSubscriptionState :: GetSubscriptionStateResponse -> TestTree
responseGetSubscriptionState =
  res
    "GetSubscriptionStateResponse"
    "fixture/GetSubscriptionStateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSubscriptionState)

responseListAttacks :: ListAttacksResponse -> TestTree
responseListAttacks =
  res
    "ListAttacksResponse"
    "fixture/ListAttacksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAttacks)

responseListProtectionGroups :: ListProtectionGroupsResponse -> TestTree
responseListProtectionGroups =
  res
    "ListProtectionGroupsResponse"
    "fixture/ListProtectionGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProtectionGroups)

responseListProtections :: ListProtectionsResponse -> TestTree
responseListProtections =
  res
    "ListProtectionsResponse"
    "fixture/ListProtectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProtections)

responseListResourcesInProtectionGroup :: ListResourcesInProtectionGroupResponse -> TestTree
responseListResourcesInProtectionGroup =
  res
    "ListResourcesInProtectionGroupResponse"
    "fixture/ListResourcesInProtectionGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResourcesInProtectionGroup)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateApplicationLayerAutomaticResponse :: UpdateApplicationLayerAutomaticResponseResponse -> TestTree
responseUpdateApplicationLayerAutomaticResponse =
  res
    "UpdateApplicationLayerAutomaticResponseResponse"
    "fixture/UpdateApplicationLayerAutomaticResponseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApplicationLayerAutomaticResponse)

responseUpdateEmergencyContactSettings :: UpdateEmergencyContactSettingsResponse -> TestTree
responseUpdateEmergencyContactSettings =
  res
    "UpdateEmergencyContactSettingsResponse"
    "fixture/UpdateEmergencyContactSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEmergencyContactSettings)

responseUpdateProtectionGroup :: UpdateProtectionGroupResponse -> TestTree
responseUpdateProtectionGroup =
  res
    "UpdateProtectionGroupResponse"
    "fixture/UpdateProtectionGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateProtectionGroup)

responseUpdateSubscription :: UpdateSubscriptionResponse -> TestTree
responseUpdateSubscription =
  res
    "UpdateSubscriptionResponse"
    "fixture/UpdateSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSubscription)
