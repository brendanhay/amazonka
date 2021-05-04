{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudFront
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.CloudFront where

import Data.Proxy
import Network.AWS.CloudFront
import Test.AWS.CloudFront.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestUpdatePublicKey $
--             newUpdatePublicKey
--
--         , requestDeletePublicKey $
--             newDeletePublicKey
--
--         , requestListPublicKeys $
--             newListPublicKeys
--
--         , requestGetDistribution $
--             newGetDistribution
--
--         , requestGetKeyGroupConfig $
--             newGetKeyGroupConfig
--
--         , requestCreateFieldLevelEncryptionProfile $
--             newCreateFieldLevelEncryptionProfile
--
--         , requestGetMonitoringSubscription $
--             newGetMonitoringSubscription
--
--         , requestCreateOriginRequestPolicy $
--             newCreateOriginRequestPolicy
--
--         , requestListDistributionsByCachePolicyId $
--             newListDistributionsByCachePolicyId
--
--         , requestListKeyGroups $
--             newListKeyGroups
--
--         , requestListOriginRequestPolicies $
--             newListOriginRequestPolicies
--
--         , requestGetKeyGroup $
--             newGetKeyGroup
--
--         , requestGetDistributionConfig $
--             newGetDistributionConfig
--
--         , requestListDistributions $
--             newListDistributions
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestListDistributionsByWebACLId $
--             newListDistributionsByWebACLId
--
--         , requestGetCloudFrontOriginAccessIdentity $
--             newGetCloudFrontOriginAccessIdentity
--
--         , requestGetPublicKey $
--             newGetPublicKey
--
--         , requestListRealtimeLogConfigs $
--             newListRealtimeLogConfigs
--
--         , requestUpdateFieldLevelEncryptionConfig $
--             newUpdateFieldLevelEncryptionConfig
--
--         , requestCreateCachePolicy $
--             newCreateCachePolicy
--
--         , requestListDistributionsByKeyGroup $
--             newListDistributionsByKeyGroup
--
--         , requestTagResource $
--             newTagResource
--
--         , requestListCachePolicies $
--             newListCachePolicies
--
--         , requestListDistributionsByOriginRequestPolicyId $
--             newListDistributionsByOriginRequestPolicyId
--
--         , requestListFieldLevelEncryptionConfigs $
--             newListFieldLevelEncryptionConfigs
--
--         , requestDeleteFieldLevelEncryptionConfig $
--             newDeleteFieldLevelEncryptionConfig
--
--         , requestDeleteCachePolicy $
--             newDeleteCachePolicy
--
--         , requestGetFieldLevelEncryption $
--             newGetFieldLevelEncryption
--
--         , requestUpdateCachePolicy $
--             newUpdateCachePolicy
--
--         , requestGetInvalidation $
--             newGetInvalidation
--
--         , requestGetPublicKeyConfig $
--             newGetPublicKeyConfig
--
--         , requestGetCloudFrontOriginAccessIdentityConfig $
--             newGetCloudFrontOriginAccessIdentityConfig
--
--         , requestCreateStreamingDistribution $
--             newCreateStreamingDistribution
--
--         , requestDeleteCloudFrontOriginAccessIdentity $
--             newDeleteCloudFrontOriginAccessIdentity
--
--         , requestDeleteStreamingDistribution $
--             newDeleteStreamingDistribution
--
--         , requestGetFieldLevelEncryptionConfig $
--             newGetFieldLevelEncryptionConfig
--
--         , requestGetRealtimeLogConfig $
--             newGetRealtimeLogConfig
--
--         , requestUpdateCloudFrontOriginAccessIdentity $
--             newUpdateCloudFrontOriginAccessIdentity
--
--         , requestUpdateStreamingDistribution $
--             newUpdateStreamingDistribution
--
--         , requestListStreamingDistributions $
--             newListStreamingDistributions
--
--         , requestCreateKeyGroup $
--             newCreateKeyGroup
--
--         , requestUpdateOriginRequestPolicy $
--             newUpdateOriginRequestPolicy
--
--         , requestGetFieldLevelEncryptionProfileConfig $
--             newGetFieldLevelEncryptionProfileConfig
--
--         , requestDeleteOriginRequestPolicy $
--             newDeleteOriginRequestPolicy
--
--         , requestListFieldLevelEncryptionProfiles $
--             newListFieldLevelEncryptionProfiles
--
--         , requestDeleteFieldLevelEncryptionProfile $
--             newDeleteFieldLevelEncryptionProfile
--
--         , requestGetOriginRequestPolicyConfig $
--             newGetOriginRequestPolicyConfig
--
--         , requestUpdateKeyGroup $
--             newUpdateKeyGroup
--
--         , requestDeleteKeyGroup $
--             newDeleteKeyGroup
--
--         , requestCreateStreamingDistributionWithTags $
--             newCreateStreamingDistributionWithTags
--
--         , requestListDistributionsByRealtimeLogConfig $
--             newListDistributionsByRealtimeLogConfig
--
--         , requestUpdateFieldLevelEncryptionProfile $
--             newUpdateFieldLevelEncryptionProfile
--
--         , requestCreateDistribution $
--             newCreateDistribution
--
--         , requestDeleteMonitoringSubscription $
--             newDeleteMonitoringSubscription
--
--         , requestGetFieldLevelEncryptionProfile $
--             newGetFieldLevelEncryptionProfile
--
--         , requestCreateMonitoringSubscription $
--             newCreateMonitoringSubscription
--
--         , requestGetOriginRequestPolicy $
--             newGetOriginRequestPolicy
--
--         , requestUpdateDistribution $
--             newUpdateDistribution
--
--         , requestDeleteDistribution $
--             newDeleteDistribution
--
--         , requestDeleteRealtimeLogConfig $
--             newDeleteRealtimeLogConfig
--
--         , requestGetStreamingDistribution $
--             newGetStreamingDistribution
--
--         , requestCreateInvalidation $
--             newCreateInvalidation
--
--         , requestGetCachePolicyConfig $
--             newGetCachePolicyConfig
--
--         , requestUpdateRealtimeLogConfig $
--             newUpdateRealtimeLogConfig
--
--         , requestCreateRealtimeLogConfig $
--             newCreateRealtimeLogConfig
--
--         , requestCreateDistributionWithTags $
--             newCreateDistributionWithTags
--
--         , requestCreateFieldLevelEncryptionConfig $
--             newCreateFieldLevelEncryptionConfig
--
--         , requestListInvalidations $
--             newListInvalidations
--
--         , requestListCloudFrontOriginAccessIdentities $
--             newListCloudFrontOriginAccessIdentities
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestGetStreamingDistributionConfig $
--             newGetStreamingDistributionConfig
--
--         , requestGetCachePolicy $
--             newGetCachePolicy
--
--         , requestCreateCloudFrontOriginAccessIdentity $
--             newCreateCloudFrontOriginAccessIdentity
--
--         , requestCreatePublicKey $
--             newCreatePublicKey
--
--           ]

--     , testGroup "response"
--         [ responseUpdatePublicKey $
--             newUpdatePublicKeyResponse
--
--         , responseDeletePublicKey $
--             newDeletePublicKeyResponse
--
--         , responseListPublicKeys $
--             newListPublicKeysResponse
--
--         , responseGetDistribution $
--             newGetDistributionResponse
--
--         , responseGetKeyGroupConfig $
--             newGetKeyGroupConfigResponse
--
--         , responseCreateFieldLevelEncryptionProfile $
--             newCreateFieldLevelEncryptionProfileResponse
--
--         , responseGetMonitoringSubscription $
--             newGetMonitoringSubscriptionResponse
--
--         , responseCreateOriginRequestPolicy $
--             newCreateOriginRequestPolicyResponse
--
--         , responseListDistributionsByCachePolicyId $
--             newListDistributionsByCachePolicyIdResponse
--
--         , responseListKeyGroups $
--             newListKeyGroupsResponse
--
--         , responseListOriginRequestPolicies $
--             newListOriginRequestPoliciesResponse
--
--         , responseGetKeyGroup $
--             newGetKeyGroupResponse
--
--         , responseGetDistributionConfig $
--             newGetDistributionConfigResponse
--
--         , responseListDistributions $
--             newListDistributionsResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseListDistributionsByWebACLId $
--             newListDistributionsByWebACLIdResponse
--
--         , responseGetCloudFrontOriginAccessIdentity $
--             newGetCloudFrontOriginAccessIdentityResponse
--
--         , responseGetPublicKey $
--             newGetPublicKeyResponse
--
--         , responseListRealtimeLogConfigs $
--             newListRealtimeLogConfigsResponse
--
--         , responseUpdateFieldLevelEncryptionConfig $
--             newUpdateFieldLevelEncryptionConfigResponse
--
--         , responseCreateCachePolicy $
--             newCreateCachePolicyResponse
--
--         , responseListDistributionsByKeyGroup $
--             newListDistributionsByKeyGroupResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseListCachePolicies $
--             newListCachePoliciesResponse
--
--         , responseListDistributionsByOriginRequestPolicyId $
--             newListDistributionsByOriginRequestPolicyIdResponse
--
--         , responseListFieldLevelEncryptionConfigs $
--             newListFieldLevelEncryptionConfigsResponse
--
--         , responseDeleteFieldLevelEncryptionConfig $
--             newDeleteFieldLevelEncryptionConfigResponse
--
--         , responseDeleteCachePolicy $
--             newDeleteCachePolicyResponse
--
--         , responseGetFieldLevelEncryption $
--             newGetFieldLevelEncryptionResponse
--
--         , responseUpdateCachePolicy $
--             newUpdateCachePolicyResponse
--
--         , responseGetInvalidation $
--             newGetInvalidationResponse
--
--         , responseGetPublicKeyConfig $
--             newGetPublicKeyConfigResponse
--
--         , responseGetCloudFrontOriginAccessIdentityConfig $
--             newGetCloudFrontOriginAccessIdentityConfigResponse
--
--         , responseCreateStreamingDistribution $
--             newCreateStreamingDistributionResponse
--
--         , responseDeleteCloudFrontOriginAccessIdentity $
--             newDeleteCloudFrontOriginAccessIdentityResponse
--
--         , responseDeleteStreamingDistribution $
--             newDeleteStreamingDistributionResponse
--
--         , responseGetFieldLevelEncryptionConfig $
--             newGetFieldLevelEncryptionConfigResponse
--
--         , responseGetRealtimeLogConfig $
--             newGetRealtimeLogConfigResponse
--
--         , responseUpdateCloudFrontOriginAccessIdentity $
--             newUpdateCloudFrontOriginAccessIdentityResponse
--
--         , responseUpdateStreamingDistribution $
--             newUpdateStreamingDistributionResponse
--
--         , responseListStreamingDistributions $
--             newListStreamingDistributionsResponse
--
--         , responseCreateKeyGroup $
--             newCreateKeyGroupResponse
--
--         , responseUpdateOriginRequestPolicy $
--             newUpdateOriginRequestPolicyResponse
--
--         , responseGetFieldLevelEncryptionProfileConfig $
--             newGetFieldLevelEncryptionProfileConfigResponse
--
--         , responseDeleteOriginRequestPolicy $
--             newDeleteOriginRequestPolicyResponse
--
--         , responseListFieldLevelEncryptionProfiles $
--             newListFieldLevelEncryptionProfilesResponse
--
--         , responseDeleteFieldLevelEncryptionProfile $
--             newDeleteFieldLevelEncryptionProfileResponse
--
--         , responseGetOriginRequestPolicyConfig $
--             newGetOriginRequestPolicyConfigResponse
--
--         , responseUpdateKeyGroup $
--             newUpdateKeyGroupResponse
--
--         , responseDeleteKeyGroup $
--             newDeleteKeyGroupResponse
--
--         , responseCreateStreamingDistributionWithTags $
--             newCreateStreamingDistributionWithTagsResponse
--
--         , responseListDistributionsByRealtimeLogConfig $
--             newListDistributionsByRealtimeLogConfigResponse
--
--         , responseUpdateFieldLevelEncryptionProfile $
--             newUpdateFieldLevelEncryptionProfileResponse
--
--         , responseCreateDistribution $
--             newCreateDistributionResponse
--
--         , responseDeleteMonitoringSubscription $
--             newDeleteMonitoringSubscriptionResponse
--
--         , responseGetFieldLevelEncryptionProfile $
--             newGetFieldLevelEncryptionProfileResponse
--
--         , responseCreateMonitoringSubscription $
--             newCreateMonitoringSubscriptionResponse
--
--         , responseGetOriginRequestPolicy $
--             newGetOriginRequestPolicyResponse
--
--         , responseUpdateDistribution $
--             newUpdateDistributionResponse
--
--         , responseDeleteDistribution $
--             newDeleteDistributionResponse
--
--         , responseDeleteRealtimeLogConfig $
--             newDeleteRealtimeLogConfigResponse
--
--         , responseGetStreamingDistribution $
--             newGetStreamingDistributionResponse
--
--         , responseCreateInvalidation $
--             newCreateInvalidationResponse
--
--         , responseGetCachePolicyConfig $
--             newGetCachePolicyConfigResponse
--
--         , responseUpdateRealtimeLogConfig $
--             newUpdateRealtimeLogConfigResponse
--
--         , responseCreateRealtimeLogConfig $
--             newCreateRealtimeLogConfigResponse
--
--         , responseCreateDistributionWithTags $
--             newCreateDistributionWithTagsResponse
--
--         , responseCreateFieldLevelEncryptionConfig $
--             newCreateFieldLevelEncryptionConfigResponse
--
--         , responseListInvalidations $
--             newListInvalidationsResponse
--
--         , responseListCloudFrontOriginAccessIdentities $
--             newListCloudFrontOriginAccessIdentitiesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseGetStreamingDistributionConfig $
--             newGetStreamingDistributionConfigResponse
--
--         , responseGetCachePolicy $
--             newGetCachePolicyResponse
--
--         , responseCreateCloudFrontOriginAccessIdentity $
--             newCreateCloudFrontOriginAccessIdentityResponse
--
--         , responseCreatePublicKey $
--             newCreatePublicKeyResponse
--
--           ]
--     ]

-- Requests

requestUpdatePublicKey :: UpdatePublicKey -> TestTree
requestUpdatePublicKey =
  req
    "UpdatePublicKey"
    "fixture/UpdatePublicKey.yaml"

requestDeletePublicKey :: DeletePublicKey -> TestTree
requestDeletePublicKey =
  req
    "DeletePublicKey"
    "fixture/DeletePublicKey.yaml"

requestListPublicKeys :: ListPublicKeys -> TestTree
requestListPublicKeys =
  req
    "ListPublicKeys"
    "fixture/ListPublicKeys.yaml"

requestGetDistribution :: GetDistribution -> TestTree
requestGetDistribution =
  req
    "GetDistribution"
    "fixture/GetDistribution.yaml"

requestGetKeyGroupConfig :: GetKeyGroupConfig -> TestTree
requestGetKeyGroupConfig =
  req
    "GetKeyGroupConfig"
    "fixture/GetKeyGroupConfig.yaml"

requestCreateFieldLevelEncryptionProfile :: CreateFieldLevelEncryptionProfile -> TestTree
requestCreateFieldLevelEncryptionProfile =
  req
    "CreateFieldLevelEncryptionProfile"
    "fixture/CreateFieldLevelEncryptionProfile.yaml"

requestGetMonitoringSubscription :: GetMonitoringSubscription -> TestTree
requestGetMonitoringSubscription =
  req
    "GetMonitoringSubscription"
    "fixture/GetMonitoringSubscription.yaml"

requestCreateOriginRequestPolicy :: CreateOriginRequestPolicy -> TestTree
requestCreateOriginRequestPolicy =
  req
    "CreateOriginRequestPolicy"
    "fixture/CreateOriginRequestPolicy.yaml"

requestListDistributionsByCachePolicyId :: ListDistributionsByCachePolicyId -> TestTree
requestListDistributionsByCachePolicyId =
  req
    "ListDistributionsByCachePolicyId"
    "fixture/ListDistributionsByCachePolicyId.yaml"

requestListKeyGroups :: ListKeyGroups -> TestTree
requestListKeyGroups =
  req
    "ListKeyGroups"
    "fixture/ListKeyGroups.yaml"

requestListOriginRequestPolicies :: ListOriginRequestPolicies -> TestTree
requestListOriginRequestPolicies =
  req
    "ListOriginRequestPolicies"
    "fixture/ListOriginRequestPolicies.yaml"

requestGetKeyGroup :: GetKeyGroup -> TestTree
requestGetKeyGroup =
  req
    "GetKeyGroup"
    "fixture/GetKeyGroup.yaml"

requestGetDistributionConfig :: GetDistributionConfig -> TestTree
requestGetDistributionConfig =
  req
    "GetDistributionConfig"
    "fixture/GetDistributionConfig.yaml"

requestListDistributions :: ListDistributions -> TestTree
requestListDistributions =
  req
    "ListDistributions"
    "fixture/ListDistributions.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestListDistributionsByWebACLId :: ListDistributionsByWebACLId -> TestTree
requestListDistributionsByWebACLId =
  req
    "ListDistributionsByWebACLId"
    "fixture/ListDistributionsByWebACLId.yaml"

requestGetCloudFrontOriginAccessIdentity :: GetCloudFrontOriginAccessIdentity -> TestTree
requestGetCloudFrontOriginAccessIdentity =
  req
    "GetCloudFrontOriginAccessIdentity"
    "fixture/GetCloudFrontOriginAccessIdentity.yaml"

requestGetPublicKey :: GetPublicKey -> TestTree
requestGetPublicKey =
  req
    "GetPublicKey"
    "fixture/GetPublicKey.yaml"

requestListRealtimeLogConfigs :: ListRealtimeLogConfigs -> TestTree
requestListRealtimeLogConfigs =
  req
    "ListRealtimeLogConfigs"
    "fixture/ListRealtimeLogConfigs.yaml"

requestUpdateFieldLevelEncryptionConfig :: UpdateFieldLevelEncryptionConfig -> TestTree
requestUpdateFieldLevelEncryptionConfig =
  req
    "UpdateFieldLevelEncryptionConfig"
    "fixture/UpdateFieldLevelEncryptionConfig.yaml"

requestCreateCachePolicy :: CreateCachePolicy -> TestTree
requestCreateCachePolicy =
  req
    "CreateCachePolicy"
    "fixture/CreateCachePolicy.yaml"

requestListDistributionsByKeyGroup :: ListDistributionsByKeyGroup -> TestTree
requestListDistributionsByKeyGroup =
  req
    "ListDistributionsByKeyGroup"
    "fixture/ListDistributionsByKeyGroup.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestListCachePolicies :: ListCachePolicies -> TestTree
requestListCachePolicies =
  req
    "ListCachePolicies"
    "fixture/ListCachePolicies.yaml"

requestListDistributionsByOriginRequestPolicyId :: ListDistributionsByOriginRequestPolicyId -> TestTree
requestListDistributionsByOriginRequestPolicyId =
  req
    "ListDistributionsByOriginRequestPolicyId"
    "fixture/ListDistributionsByOriginRequestPolicyId.yaml"

requestListFieldLevelEncryptionConfigs :: ListFieldLevelEncryptionConfigs -> TestTree
requestListFieldLevelEncryptionConfigs =
  req
    "ListFieldLevelEncryptionConfigs"
    "fixture/ListFieldLevelEncryptionConfigs.yaml"

requestDeleteFieldLevelEncryptionConfig :: DeleteFieldLevelEncryptionConfig -> TestTree
requestDeleteFieldLevelEncryptionConfig =
  req
    "DeleteFieldLevelEncryptionConfig"
    "fixture/DeleteFieldLevelEncryptionConfig.yaml"

requestDeleteCachePolicy :: DeleteCachePolicy -> TestTree
requestDeleteCachePolicy =
  req
    "DeleteCachePolicy"
    "fixture/DeleteCachePolicy.yaml"

requestGetFieldLevelEncryption :: GetFieldLevelEncryption -> TestTree
requestGetFieldLevelEncryption =
  req
    "GetFieldLevelEncryption"
    "fixture/GetFieldLevelEncryption.yaml"

requestUpdateCachePolicy :: UpdateCachePolicy -> TestTree
requestUpdateCachePolicy =
  req
    "UpdateCachePolicy"
    "fixture/UpdateCachePolicy.yaml"

requestGetInvalidation :: GetInvalidation -> TestTree
requestGetInvalidation =
  req
    "GetInvalidation"
    "fixture/GetInvalidation.yaml"

requestGetPublicKeyConfig :: GetPublicKeyConfig -> TestTree
requestGetPublicKeyConfig =
  req
    "GetPublicKeyConfig"
    "fixture/GetPublicKeyConfig.yaml"

requestGetCloudFrontOriginAccessIdentityConfig :: GetCloudFrontOriginAccessIdentityConfig -> TestTree
requestGetCloudFrontOriginAccessIdentityConfig =
  req
    "GetCloudFrontOriginAccessIdentityConfig"
    "fixture/GetCloudFrontOriginAccessIdentityConfig.yaml"

requestCreateStreamingDistribution :: CreateStreamingDistribution -> TestTree
requestCreateStreamingDistribution =
  req
    "CreateStreamingDistribution"
    "fixture/CreateStreamingDistribution.yaml"

requestDeleteCloudFrontOriginAccessIdentity :: DeleteCloudFrontOriginAccessIdentity -> TestTree
requestDeleteCloudFrontOriginAccessIdentity =
  req
    "DeleteCloudFrontOriginAccessIdentity"
    "fixture/DeleteCloudFrontOriginAccessIdentity.yaml"

requestDeleteStreamingDistribution :: DeleteStreamingDistribution -> TestTree
requestDeleteStreamingDistribution =
  req
    "DeleteStreamingDistribution"
    "fixture/DeleteStreamingDistribution.yaml"

requestGetFieldLevelEncryptionConfig :: GetFieldLevelEncryptionConfig -> TestTree
requestGetFieldLevelEncryptionConfig =
  req
    "GetFieldLevelEncryptionConfig"
    "fixture/GetFieldLevelEncryptionConfig.yaml"

requestGetRealtimeLogConfig :: GetRealtimeLogConfig -> TestTree
requestGetRealtimeLogConfig =
  req
    "GetRealtimeLogConfig"
    "fixture/GetRealtimeLogConfig.yaml"

requestUpdateCloudFrontOriginAccessIdentity :: UpdateCloudFrontOriginAccessIdentity -> TestTree
requestUpdateCloudFrontOriginAccessIdentity =
  req
    "UpdateCloudFrontOriginAccessIdentity"
    "fixture/UpdateCloudFrontOriginAccessIdentity.yaml"

requestUpdateStreamingDistribution :: UpdateStreamingDistribution -> TestTree
requestUpdateStreamingDistribution =
  req
    "UpdateStreamingDistribution"
    "fixture/UpdateStreamingDistribution.yaml"

requestListStreamingDistributions :: ListStreamingDistributions -> TestTree
requestListStreamingDistributions =
  req
    "ListStreamingDistributions"
    "fixture/ListStreamingDistributions.yaml"

requestCreateKeyGroup :: CreateKeyGroup -> TestTree
requestCreateKeyGroup =
  req
    "CreateKeyGroup"
    "fixture/CreateKeyGroup.yaml"

requestUpdateOriginRequestPolicy :: UpdateOriginRequestPolicy -> TestTree
requestUpdateOriginRequestPolicy =
  req
    "UpdateOriginRequestPolicy"
    "fixture/UpdateOriginRequestPolicy.yaml"

requestGetFieldLevelEncryptionProfileConfig :: GetFieldLevelEncryptionProfileConfig -> TestTree
requestGetFieldLevelEncryptionProfileConfig =
  req
    "GetFieldLevelEncryptionProfileConfig"
    "fixture/GetFieldLevelEncryptionProfileConfig.yaml"

requestDeleteOriginRequestPolicy :: DeleteOriginRequestPolicy -> TestTree
requestDeleteOriginRequestPolicy =
  req
    "DeleteOriginRequestPolicy"
    "fixture/DeleteOriginRequestPolicy.yaml"

requestListFieldLevelEncryptionProfiles :: ListFieldLevelEncryptionProfiles -> TestTree
requestListFieldLevelEncryptionProfiles =
  req
    "ListFieldLevelEncryptionProfiles"
    "fixture/ListFieldLevelEncryptionProfiles.yaml"

requestDeleteFieldLevelEncryptionProfile :: DeleteFieldLevelEncryptionProfile -> TestTree
requestDeleteFieldLevelEncryptionProfile =
  req
    "DeleteFieldLevelEncryptionProfile"
    "fixture/DeleteFieldLevelEncryptionProfile.yaml"

requestGetOriginRequestPolicyConfig :: GetOriginRequestPolicyConfig -> TestTree
requestGetOriginRequestPolicyConfig =
  req
    "GetOriginRequestPolicyConfig"
    "fixture/GetOriginRequestPolicyConfig.yaml"

requestUpdateKeyGroup :: UpdateKeyGroup -> TestTree
requestUpdateKeyGroup =
  req
    "UpdateKeyGroup"
    "fixture/UpdateKeyGroup.yaml"

requestDeleteKeyGroup :: DeleteKeyGroup -> TestTree
requestDeleteKeyGroup =
  req
    "DeleteKeyGroup"
    "fixture/DeleteKeyGroup.yaml"

requestCreateStreamingDistributionWithTags :: CreateStreamingDistributionWithTags -> TestTree
requestCreateStreamingDistributionWithTags =
  req
    "CreateStreamingDistributionWithTags"
    "fixture/CreateStreamingDistributionWithTags.yaml"

requestListDistributionsByRealtimeLogConfig :: ListDistributionsByRealtimeLogConfig -> TestTree
requestListDistributionsByRealtimeLogConfig =
  req
    "ListDistributionsByRealtimeLogConfig"
    "fixture/ListDistributionsByRealtimeLogConfig.yaml"

requestUpdateFieldLevelEncryptionProfile :: UpdateFieldLevelEncryptionProfile -> TestTree
requestUpdateFieldLevelEncryptionProfile =
  req
    "UpdateFieldLevelEncryptionProfile"
    "fixture/UpdateFieldLevelEncryptionProfile.yaml"

requestCreateDistribution :: CreateDistribution -> TestTree
requestCreateDistribution =
  req
    "CreateDistribution"
    "fixture/CreateDistribution.yaml"

requestDeleteMonitoringSubscription :: DeleteMonitoringSubscription -> TestTree
requestDeleteMonitoringSubscription =
  req
    "DeleteMonitoringSubscription"
    "fixture/DeleteMonitoringSubscription.yaml"

requestGetFieldLevelEncryptionProfile :: GetFieldLevelEncryptionProfile -> TestTree
requestGetFieldLevelEncryptionProfile =
  req
    "GetFieldLevelEncryptionProfile"
    "fixture/GetFieldLevelEncryptionProfile.yaml"

requestCreateMonitoringSubscription :: CreateMonitoringSubscription -> TestTree
requestCreateMonitoringSubscription =
  req
    "CreateMonitoringSubscription"
    "fixture/CreateMonitoringSubscription.yaml"

requestGetOriginRequestPolicy :: GetOriginRequestPolicy -> TestTree
requestGetOriginRequestPolicy =
  req
    "GetOriginRequestPolicy"
    "fixture/GetOriginRequestPolicy.yaml"

requestUpdateDistribution :: UpdateDistribution -> TestTree
requestUpdateDistribution =
  req
    "UpdateDistribution"
    "fixture/UpdateDistribution.yaml"

requestDeleteDistribution :: DeleteDistribution -> TestTree
requestDeleteDistribution =
  req
    "DeleteDistribution"
    "fixture/DeleteDistribution.yaml"

requestDeleteRealtimeLogConfig :: DeleteRealtimeLogConfig -> TestTree
requestDeleteRealtimeLogConfig =
  req
    "DeleteRealtimeLogConfig"
    "fixture/DeleteRealtimeLogConfig.yaml"

requestGetStreamingDistribution :: GetStreamingDistribution -> TestTree
requestGetStreamingDistribution =
  req
    "GetStreamingDistribution"
    "fixture/GetStreamingDistribution.yaml"

requestCreateInvalidation :: CreateInvalidation -> TestTree
requestCreateInvalidation =
  req
    "CreateInvalidation"
    "fixture/CreateInvalidation.yaml"

requestGetCachePolicyConfig :: GetCachePolicyConfig -> TestTree
requestGetCachePolicyConfig =
  req
    "GetCachePolicyConfig"
    "fixture/GetCachePolicyConfig.yaml"

requestUpdateRealtimeLogConfig :: UpdateRealtimeLogConfig -> TestTree
requestUpdateRealtimeLogConfig =
  req
    "UpdateRealtimeLogConfig"
    "fixture/UpdateRealtimeLogConfig.yaml"

requestCreateRealtimeLogConfig :: CreateRealtimeLogConfig -> TestTree
requestCreateRealtimeLogConfig =
  req
    "CreateRealtimeLogConfig"
    "fixture/CreateRealtimeLogConfig.yaml"

requestCreateDistributionWithTags :: CreateDistributionWithTags -> TestTree
requestCreateDistributionWithTags =
  req
    "CreateDistributionWithTags"
    "fixture/CreateDistributionWithTags.yaml"

requestCreateFieldLevelEncryptionConfig :: CreateFieldLevelEncryptionConfig -> TestTree
requestCreateFieldLevelEncryptionConfig =
  req
    "CreateFieldLevelEncryptionConfig"
    "fixture/CreateFieldLevelEncryptionConfig.yaml"

requestListInvalidations :: ListInvalidations -> TestTree
requestListInvalidations =
  req
    "ListInvalidations"
    "fixture/ListInvalidations.yaml"

requestListCloudFrontOriginAccessIdentities :: ListCloudFrontOriginAccessIdentities -> TestTree
requestListCloudFrontOriginAccessIdentities =
  req
    "ListCloudFrontOriginAccessIdentities"
    "fixture/ListCloudFrontOriginAccessIdentities.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestGetStreamingDistributionConfig :: GetStreamingDistributionConfig -> TestTree
requestGetStreamingDistributionConfig =
  req
    "GetStreamingDistributionConfig"
    "fixture/GetStreamingDistributionConfig.yaml"

requestGetCachePolicy :: GetCachePolicy -> TestTree
requestGetCachePolicy =
  req
    "GetCachePolicy"
    "fixture/GetCachePolicy.yaml"

requestCreateCloudFrontOriginAccessIdentity :: CreateCloudFrontOriginAccessIdentity -> TestTree
requestCreateCloudFrontOriginAccessIdentity =
  req
    "CreateCloudFrontOriginAccessIdentity"
    "fixture/CreateCloudFrontOriginAccessIdentity.yaml"

requestCreatePublicKey :: CreatePublicKey -> TestTree
requestCreatePublicKey =
  req
    "CreatePublicKey"
    "fixture/CreatePublicKey.yaml"

-- Responses

responseUpdatePublicKey :: UpdatePublicKeyResponse -> TestTree
responseUpdatePublicKey =
  res
    "UpdatePublicKeyResponse"
    "fixture/UpdatePublicKeyResponse.proto"
    defaultService
    (Proxy :: Proxy UpdatePublicKey)

responseDeletePublicKey :: DeletePublicKeyResponse -> TestTree
responseDeletePublicKey =
  res
    "DeletePublicKeyResponse"
    "fixture/DeletePublicKeyResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePublicKey)

responseListPublicKeys :: ListPublicKeysResponse -> TestTree
responseListPublicKeys =
  res
    "ListPublicKeysResponse"
    "fixture/ListPublicKeysResponse.proto"
    defaultService
    (Proxy :: Proxy ListPublicKeys)

responseGetDistribution :: GetDistributionResponse -> TestTree
responseGetDistribution =
  res
    "GetDistributionResponse"
    "fixture/GetDistributionResponse.proto"
    defaultService
    (Proxy :: Proxy GetDistribution)

responseGetKeyGroupConfig :: GetKeyGroupConfigResponse -> TestTree
responseGetKeyGroupConfig =
  res
    "GetKeyGroupConfigResponse"
    "fixture/GetKeyGroupConfigResponse.proto"
    defaultService
    (Proxy :: Proxy GetKeyGroupConfig)

responseCreateFieldLevelEncryptionProfile :: CreateFieldLevelEncryptionProfileResponse -> TestTree
responseCreateFieldLevelEncryptionProfile =
  res
    "CreateFieldLevelEncryptionProfileResponse"
    "fixture/CreateFieldLevelEncryptionProfileResponse.proto"
    defaultService
    (Proxy :: Proxy CreateFieldLevelEncryptionProfile)

responseGetMonitoringSubscription :: GetMonitoringSubscriptionResponse -> TestTree
responseGetMonitoringSubscription =
  res
    "GetMonitoringSubscriptionResponse"
    "fixture/GetMonitoringSubscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy GetMonitoringSubscription)

responseCreateOriginRequestPolicy :: CreateOriginRequestPolicyResponse -> TestTree
responseCreateOriginRequestPolicy =
  res
    "CreateOriginRequestPolicyResponse"
    "fixture/CreateOriginRequestPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy CreateOriginRequestPolicy)

responseListDistributionsByCachePolicyId :: ListDistributionsByCachePolicyIdResponse -> TestTree
responseListDistributionsByCachePolicyId =
  res
    "ListDistributionsByCachePolicyIdResponse"
    "fixture/ListDistributionsByCachePolicyIdResponse.proto"
    defaultService
    (Proxy :: Proxy ListDistributionsByCachePolicyId)

responseListKeyGroups :: ListKeyGroupsResponse -> TestTree
responseListKeyGroups =
  res
    "ListKeyGroupsResponse"
    "fixture/ListKeyGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy ListKeyGroups)

responseListOriginRequestPolicies :: ListOriginRequestPoliciesResponse -> TestTree
responseListOriginRequestPolicies =
  res
    "ListOriginRequestPoliciesResponse"
    "fixture/ListOriginRequestPoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy ListOriginRequestPolicies)

responseGetKeyGroup :: GetKeyGroupResponse -> TestTree
responseGetKeyGroup =
  res
    "GetKeyGroupResponse"
    "fixture/GetKeyGroupResponse.proto"
    defaultService
    (Proxy :: Proxy GetKeyGroup)

responseGetDistributionConfig :: GetDistributionConfigResponse -> TestTree
responseGetDistributionConfig =
  res
    "GetDistributionConfigResponse"
    "fixture/GetDistributionConfigResponse.proto"
    defaultService
    (Proxy :: Proxy GetDistributionConfig)

responseListDistributions :: ListDistributionsResponse -> TestTree
responseListDistributions =
  res
    "ListDistributionsResponse"
    "fixture/ListDistributionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDistributions)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseListDistributionsByWebACLId :: ListDistributionsByWebACLIdResponse -> TestTree
responseListDistributionsByWebACLId =
  res
    "ListDistributionsByWebACLIdResponse"
    "fixture/ListDistributionsByWebACLIdResponse.proto"
    defaultService
    (Proxy :: Proxy ListDistributionsByWebACLId)

responseGetCloudFrontOriginAccessIdentity :: GetCloudFrontOriginAccessIdentityResponse -> TestTree
responseGetCloudFrontOriginAccessIdentity =
  res
    "GetCloudFrontOriginAccessIdentityResponse"
    "fixture/GetCloudFrontOriginAccessIdentityResponse.proto"
    defaultService
    (Proxy :: Proxy GetCloudFrontOriginAccessIdentity)

responseGetPublicKey :: GetPublicKeyResponse -> TestTree
responseGetPublicKey =
  res
    "GetPublicKeyResponse"
    "fixture/GetPublicKeyResponse.proto"
    defaultService
    (Proxy :: Proxy GetPublicKey)

responseListRealtimeLogConfigs :: ListRealtimeLogConfigsResponse -> TestTree
responseListRealtimeLogConfigs =
  res
    "ListRealtimeLogConfigsResponse"
    "fixture/ListRealtimeLogConfigsResponse.proto"
    defaultService
    (Proxy :: Proxy ListRealtimeLogConfigs)

responseUpdateFieldLevelEncryptionConfig :: UpdateFieldLevelEncryptionConfigResponse -> TestTree
responseUpdateFieldLevelEncryptionConfig =
  res
    "UpdateFieldLevelEncryptionConfigResponse"
    "fixture/UpdateFieldLevelEncryptionConfigResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateFieldLevelEncryptionConfig)

responseCreateCachePolicy :: CreateCachePolicyResponse -> TestTree
responseCreateCachePolicy =
  res
    "CreateCachePolicyResponse"
    "fixture/CreateCachePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCachePolicy)

responseListDistributionsByKeyGroup :: ListDistributionsByKeyGroupResponse -> TestTree
responseListDistributionsByKeyGroup =
  res
    "ListDistributionsByKeyGroupResponse"
    "fixture/ListDistributionsByKeyGroupResponse.proto"
    defaultService
    (Proxy :: Proxy ListDistributionsByKeyGroup)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseListCachePolicies :: ListCachePoliciesResponse -> TestTree
responseListCachePolicies =
  res
    "ListCachePoliciesResponse"
    "fixture/ListCachePoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy ListCachePolicies)

responseListDistributionsByOriginRequestPolicyId :: ListDistributionsByOriginRequestPolicyIdResponse -> TestTree
responseListDistributionsByOriginRequestPolicyId =
  res
    "ListDistributionsByOriginRequestPolicyIdResponse"
    "fixture/ListDistributionsByOriginRequestPolicyIdResponse.proto"
    defaultService
    (Proxy :: Proxy ListDistributionsByOriginRequestPolicyId)

responseListFieldLevelEncryptionConfigs :: ListFieldLevelEncryptionConfigsResponse -> TestTree
responseListFieldLevelEncryptionConfigs =
  res
    "ListFieldLevelEncryptionConfigsResponse"
    "fixture/ListFieldLevelEncryptionConfigsResponse.proto"
    defaultService
    (Proxy :: Proxy ListFieldLevelEncryptionConfigs)

responseDeleteFieldLevelEncryptionConfig :: DeleteFieldLevelEncryptionConfigResponse -> TestTree
responseDeleteFieldLevelEncryptionConfig =
  res
    "DeleteFieldLevelEncryptionConfigResponse"
    "fixture/DeleteFieldLevelEncryptionConfigResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteFieldLevelEncryptionConfig)

responseDeleteCachePolicy :: DeleteCachePolicyResponse -> TestTree
responseDeleteCachePolicy =
  res
    "DeleteCachePolicyResponse"
    "fixture/DeleteCachePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCachePolicy)

responseGetFieldLevelEncryption :: GetFieldLevelEncryptionResponse -> TestTree
responseGetFieldLevelEncryption =
  res
    "GetFieldLevelEncryptionResponse"
    "fixture/GetFieldLevelEncryptionResponse.proto"
    defaultService
    (Proxy :: Proxy GetFieldLevelEncryption)

responseUpdateCachePolicy :: UpdateCachePolicyResponse -> TestTree
responseUpdateCachePolicy =
  res
    "UpdateCachePolicyResponse"
    "fixture/UpdateCachePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateCachePolicy)

responseGetInvalidation :: GetInvalidationResponse -> TestTree
responseGetInvalidation =
  res
    "GetInvalidationResponse"
    "fixture/GetInvalidationResponse.proto"
    defaultService
    (Proxy :: Proxy GetInvalidation)

responseGetPublicKeyConfig :: GetPublicKeyConfigResponse -> TestTree
responseGetPublicKeyConfig =
  res
    "GetPublicKeyConfigResponse"
    "fixture/GetPublicKeyConfigResponse.proto"
    defaultService
    (Proxy :: Proxy GetPublicKeyConfig)

responseGetCloudFrontOriginAccessIdentityConfig :: GetCloudFrontOriginAccessIdentityConfigResponse -> TestTree
responseGetCloudFrontOriginAccessIdentityConfig =
  res
    "GetCloudFrontOriginAccessIdentityConfigResponse"
    "fixture/GetCloudFrontOriginAccessIdentityConfigResponse.proto"
    defaultService
    (Proxy :: Proxy GetCloudFrontOriginAccessIdentityConfig)

responseCreateStreamingDistribution :: CreateStreamingDistributionResponse -> TestTree
responseCreateStreamingDistribution =
  res
    "CreateStreamingDistributionResponse"
    "fixture/CreateStreamingDistributionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateStreamingDistribution)

responseDeleteCloudFrontOriginAccessIdentity :: DeleteCloudFrontOriginAccessIdentityResponse -> TestTree
responseDeleteCloudFrontOriginAccessIdentity =
  res
    "DeleteCloudFrontOriginAccessIdentityResponse"
    "fixture/DeleteCloudFrontOriginAccessIdentityResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCloudFrontOriginAccessIdentity)

responseDeleteStreamingDistribution :: DeleteStreamingDistributionResponse -> TestTree
responseDeleteStreamingDistribution =
  res
    "DeleteStreamingDistributionResponse"
    "fixture/DeleteStreamingDistributionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteStreamingDistribution)

responseGetFieldLevelEncryptionConfig :: GetFieldLevelEncryptionConfigResponse -> TestTree
responseGetFieldLevelEncryptionConfig =
  res
    "GetFieldLevelEncryptionConfigResponse"
    "fixture/GetFieldLevelEncryptionConfigResponse.proto"
    defaultService
    (Proxy :: Proxy GetFieldLevelEncryptionConfig)

responseGetRealtimeLogConfig :: GetRealtimeLogConfigResponse -> TestTree
responseGetRealtimeLogConfig =
  res
    "GetRealtimeLogConfigResponse"
    "fixture/GetRealtimeLogConfigResponse.proto"
    defaultService
    (Proxy :: Proxy GetRealtimeLogConfig)

responseUpdateCloudFrontOriginAccessIdentity :: UpdateCloudFrontOriginAccessIdentityResponse -> TestTree
responseUpdateCloudFrontOriginAccessIdentity =
  res
    "UpdateCloudFrontOriginAccessIdentityResponse"
    "fixture/UpdateCloudFrontOriginAccessIdentityResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateCloudFrontOriginAccessIdentity)

responseUpdateStreamingDistribution :: UpdateStreamingDistributionResponse -> TestTree
responseUpdateStreamingDistribution =
  res
    "UpdateStreamingDistributionResponse"
    "fixture/UpdateStreamingDistributionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateStreamingDistribution)

responseListStreamingDistributions :: ListStreamingDistributionsResponse -> TestTree
responseListStreamingDistributions =
  res
    "ListStreamingDistributionsResponse"
    "fixture/ListStreamingDistributionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListStreamingDistributions)

responseCreateKeyGroup :: CreateKeyGroupResponse -> TestTree
responseCreateKeyGroup =
  res
    "CreateKeyGroupResponse"
    "fixture/CreateKeyGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateKeyGroup)

responseUpdateOriginRequestPolicy :: UpdateOriginRequestPolicyResponse -> TestTree
responseUpdateOriginRequestPolicy =
  res
    "UpdateOriginRequestPolicyResponse"
    "fixture/UpdateOriginRequestPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateOriginRequestPolicy)

responseGetFieldLevelEncryptionProfileConfig :: GetFieldLevelEncryptionProfileConfigResponse -> TestTree
responseGetFieldLevelEncryptionProfileConfig =
  res
    "GetFieldLevelEncryptionProfileConfigResponse"
    "fixture/GetFieldLevelEncryptionProfileConfigResponse.proto"
    defaultService
    (Proxy :: Proxy GetFieldLevelEncryptionProfileConfig)

responseDeleteOriginRequestPolicy :: DeleteOriginRequestPolicyResponse -> TestTree
responseDeleteOriginRequestPolicy =
  res
    "DeleteOriginRequestPolicyResponse"
    "fixture/DeleteOriginRequestPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteOriginRequestPolicy)

responseListFieldLevelEncryptionProfiles :: ListFieldLevelEncryptionProfilesResponse -> TestTree
responseListFieldLevelEncryptionProfiles =
  res
    "ListFieldLevelEncryptionProfilesResponse"
    "fixture/ListFieldLevelEncryptionProfilesResponse.proto"
    defaultService
    (Proxy :: Proxy ListFieldLevelEncryptionProfiles)

responseDeleteFieldLevelEncryptionProfile :: DeleteFieldLevelEncryptionProfileResponse -> TestTree
responseDeleteFieldLevelEncryptionProfile =
  res
    "DeleteFieldLevelEncryptionProfileResponse"
    "fixture/DeleteFieldLevelEncryptionProfileResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteFieldLevelEncryptionProfile)

responseGetOriginRequestPolicyConfig :: GetOriginRequestPolicyConfigResponse -> TestTree
responseGetOriginRequestPolicyConfig =
  res
    "GetOriginRequestPolicyConfigResponse"
    "fixture/GetOriginRequestPolicyConfigResponse.proto"
    defaultService
    (Proxy :: Proxy GetOriginRequestPolicyConfig)

responseUpdateKeyGroup :: UpdateKeyGroupResponse -> TestTree
responseUpdateKeyGroup =
  res
    "UpdateKeyGroupResponse"
    "fixture/UpdateKeyGroupResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateKeyGroup)

responseDeleteKeyGroup :: DeleteKeyGroupResponse -> TestTree
responseDeleteKeyGroup =
  res
    "DeleteKeyGroupResponse"
    "fixture/DeleteKeyGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteKeyGroup)

responseCreateStreamingDistributionWithTags :: CreateStreamingDistributionWithTagsResponse -> TestTree
responseCreateStreamingDistributionWithTags =
  res
    "CreateStreamingDistributionWithTagsResponse"
    "fixture/CreateStreamingDistributionWithTagsResponse.proto"
    defaultService
    (Proxy :: Proxy CreateStreamingDistributionWithTags)

responseListDistributionsByRealtimeLogConfig :: ListDistributionsByRealtimeLogConfigResponse -> TestTree
responseListDistributionsByRealtimeLogConfig =
  res
    "ListDistributionsByRealtimeLogConfigResponse"
    "fixture/ListDistributionsByRealtimeLogConfigResponse.proto"
    defaultService
    (Proxy :: Proxy ListDistributionsByRealtimeLogConfig)

responseUpdateFieldLevelEncryptionProfile :: UpdateFieldLevelEncryptionProfileResponse -> TestTree
responseUpdateFieldLevelEncryptionProfile =
  res
    "UpdateFieldLevelEncryptionProfileResponse"
    "fixture/UpdateFieldLevelEncryptionProfileResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateFieldLevelEncryptionProfile)

responseCreateDistribution :: CreateDistributionResponse -> TestTree
responseCreateDistribution =
  res
    "CreateDistributionResponse"
    "fixture/CreateDistributionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDistribution)

responseDeleteMonitoringSubscription :: DeleteMonitoringSubscriptionResponse -> TestTree
responseDeleteMonitoringSubscription =
  res
    "DeleteMonitoringSubscriptionResponse"
    "fixture/DeleteMonitoringSubscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteMonitoringSubscription)

responseGetFieldLevelEncryptionProfile :: GetFieldLevelEncryptionProfileResponse -> TestTree
responseGetFieldLevelEncryptionProfile =
  res
    "GetFieldLevelEncryptionProfileResponse"
    "fixture/GetFieldLevelEncryptionProfileResponse.proto"
    defaultService
    (Proxy :: Proxy GetFieldLevelEncryptionProfile)

responseCreateMonitoringSubscription :: CreateMonitoringSubscriptionResponse -> TestTree
responseCreateMonitoringSubscription =
  res
    "CreateMonitoringSubscriptionResponse"
    "fixture/CreateMonitoringSubscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateMonitoringSubscription)

responseGetOriginRequestPolicy :: GetOriginRequestPolicyResponse -> TestTree
responseGetOriginRequestPolicy =
  res
    "GetOriginRequestPolicyResponse"
    "fixture/GetOriginRequestPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetOriginRequestPolicy)

responseUpdateDistribution :: UpdateDistributionResponse -> TestTree
responseUpdateDistribution =
  res
    "UpdateDistributionResponse"
    "fixture/UpdateDistributionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDistribution)

responseDeleteDistribution :: DeleteDistributionResponse -> TestTree
responseDeleteDistribution =
  res
    "DeleteDistributionResponse"
    "fixture/DeleteDistributionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDistribution)

responseDeleteRealtimeLogConfig :: DeleteRealtimeLogConfigResponse -> TestTree
responseDeleteRealtimeLogConfig =
  res
    "DeleteRealtimeLogConfigResponse"
    "fixture/DeleteRealtimeLogConfigResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRealtimeLogConfig)

responseGetStreamingDistribution :: GetStreamingDistributionResponse -> TestTree
responseGetStreamingDistribution =
  res
    "GetStreamingDistributionResponse"
    "fixture/GetStreamingDistributionResponse.proto"
    defaultService
    (Proxy :: Proxy GetStreamingDistribution)

responseCreateInvalidation :: CreateInvalidationResponse -> TestTree
responseCreateInvalidation =
  res
    "CreateInvalidationResponse"
    "fixture/CreateInvalidationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateInvalidation)

responseGetCachePolicyConfig :: GetCachePolicyConfigResponse -> TestTree
responseGetCachePolicyConfig =
  res
    "GetCachePolicyConfigResponse"
    "fixture/GetCachePolicyConfigResponse.proto"
    defaultService
    (Proxy :: Proxy GetCachePolicyConfig)

responseUpdateRealtimeLogConfig :: UpdateRealtimeLogConfigResponse -> TestTree
responseUpdateRealtimeLogConfig =
  res
    "UpdateRealtimeLogConfigResponse"
    "fixture/UpdateRealtimeLogConfigResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRealtimeLogConfig)

responseCreateRealtimeLogConfig :: CreateRealtimeLogConfigResponse -> TestTree
responseCreateRealtimeLogConfig =
  res
    "CreateRealtimeLogConfigResponse"
    "fixture/CreateRealtimeLogConfigResponse.proto"
    defaultService
    (Proxy :: Proxy CreateRealtimeLogConfig)

responseCreateDistributionWithTags :: CreateDistributionWithTagsResponse -> TestTree
responseCreateDistributionWithTags =
  res
    "CreateDistributionWithTagsResponse"
    "fixture/CreateDistributionWithTagsResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDistributionWithTags)

responseCreateFieldLevelEncryptionConfig :: CreateFieldLevelEncryptionConfigResponse -> TestTree
responseCreateFieldLevelEncryptionConfig =
  res
    "CreateFieldLevelEncryptionConfigResponse"
    "fixture/CreateFieldLevelEncryptionConfigResponse.proto"
    defaultService
    (Proxy :: Proxy CreateFieldLevelEncryptionConfig)

responseListInvalidations :: ListInvalidationsResponse -> TestTree
responseListInvalidations =
  res
    "ListInvalidationsResponse"
    "fixture/ListInvalidationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListInvalidations)

responseListCloudFrontOriginAccessIdentities :: ListCloudFrontOriginAccessIdentitiesResponse -> TestTree
responseListCloudFrontOriginAccessIdentities =
  res
    "ListCloudFrontOriginAccessIdentitiesResponse"
    "fixture/ListCloudFrontOriginAccessIdentitiesResponse.proto"
    defaultService
    (Proxy :: Proxy ListCloudFrontOriginAccessIdentities)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseGetStreamingDistributionConfig :: GetStreamingDistributionConfigResponse -> TestTree
responseGetStreamingDistributionConfig =
  res
    "GetStreamingDistributionConfigResponse"
    "fixture/GetStreamingDistributionConfigResponse.proto"
    defaultService
    (Proxy :: Proxy GetStreamingDistributionConfig)

responseGetCachePolicy :: GetCachePolicyResponse -> TestTree
responseGetCachePolicy =
  res
    "GetCachePolicyResponse"
    "fixture/GetCachePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetCachePolicy)

responseCreateCloudFrontOriginAccessIdentity :: CreateCloudFrontOriginAccessIdentityResponse -> TestTree
responseCreateCloudFrontOriginAccessIdentity =
  res
    "CreateCloudFrontOriginAccessIdentityResponse"
    "fixture/CreateCloudFrontOriginAccessIdentityResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCloudFrontOriginAccessIdentity)

responseCreatePublicKey :: CreatePublicKeyResponse -> TestTree
responseCreatePublicKey =
  res
    "CreatePublicKeyResponse"
    "fixture/CreatePublicKeyResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePublicKey)
