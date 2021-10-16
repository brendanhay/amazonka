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
--         [ requestDeletePublicKey $
--             newDeletePublicKey
--
--         , requestListPublicKeys $
--             newListPublicKeys
--
--         , requestUpdatePublicKey $
--             newUpdatePublicKey
--
--         , requestGetDistribution $
--             newGetDistribution
--
--         , requestGetFunction $
--             newGetFunction
--
--         , requestListConflictingAliases $
--             newListConflictingAliases
--
--         , requestGetKeyGroupConfig $
--             newGetKeyGroupConfig
--
--         , requestCreateFieldLevelEncryptionProfile $
--             newCreateFieldLevelEncryptionProfile
--
--         , requestListKeyGroups $
--             newListKeyGroups
--
--         , requestGetMonitoringSubscription $
--             newGetMonitoringSubscription
--
--         , requestListDistributionsByCachePolicyId $
--             newListDistributionsByCachePolicyId
--
--         , requestListOriginRequestPolicies $
--             newListOriginRequestPolicies
--
--         , requestCreateOriginRequestPolicy $
--             newCreateOriginRequestPolicy
--
--         , requestGetKeyGroup $
--             newGetKeyGroup
--
--         , requestGetDistributionConfig $
--             newGetDistributionConfig
--
--         , requestListFunctions $
--             newListFunctions
--
--         , requestListDistributionsByWebACLId $
--             newListDistributionsByWebACLId
--
--         , requestListDistributions $
--             newListDistributions
--
--         , requestPublishFunction $
--             newPublishFunction
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUpdateFieldLevelEncryptionConfig $
--             newUpdateFieldLevelEncryptionConfig
--
--         , requestGetCloudFrontOriginAccessIdentity $
--             newGetCloudFrontOriginAccessIdentity
--
--         , requestDeleteFieldLevelEncryptionConfig $
--             newDeleteFieldLevelEncryptionConfig
--
--         , requestListRealtimeLogConfigs $
--             newListRealtimeLogConfigs
--
--         , requestListCachePolicies $
--             newListCachePolicies
--
--         , requestGetPublicKey $
--             newGetPublicKey
--
--         , requestCreateCachePolicy $
--             newCreateCachePolicy
--
--         , requestListDistributionsByKeyGroup $
--             newListDistributionsByKeyGroup
--
--         , requestListDistributionsByOriginRequestPolicyId $
--             newListDistributionsByOriginRequestPolicyId
--
--         , requestListFieldLevelEncryptionConfigs $
--             newListFieldLevelEncryptionConfigs
--
--         , requestGetFieldLevelEncryption $
--             newGetFieldLevelEncryption
--
--         , requestUpdateCachePolicy $
--             newUpdateCachePolicy
--
--         , requestDeleteCachePolicy $
--             newDeleteCachePolicy
--
--         , requestGetInvalidation $
--             newGetInvalidation
--
--         , requestCreateStreamingDistribution $
--             newCreateStreamingDistribution
--
--         , requestGetPublicKeyConfig $
--             newGetPublicKeyConfig
--
--         , requestGetCloudFrontOriginAccessIdentityConfig $
--             newGetCloudFrontOriginAccessIdentityConfig
--
--         , requestUpdateCloudFrontOriginAccessIdentity $
--             newUpdateCloudFrontOriginAccessIdentity
--
--         , requestGetRealtimeLogConfig $
--             newGetRealtimeLogConfig
--
--         , requestDeleteStreamingDistribution $
--             newDeleteStreamingDistribution
--
--         , requestUpdateStreamingDistribution $
--             newUpdateStreamingDistribution
--
--         , requestDescribeFunction $
--             newDescribeFunction
--
--         , requestDeleteCloudFrontOriginAccessIdentity $
--             newDeleteCloudFrontOriginAccessIdentity
--
--         , requestGetFieldLevelEncryptionConfig $
--             newGetFieldLevelEncryptionConfig
--
--         , requestListStreamingDistributions $
--             newListStreamingDistributions
--
--         , requestGetFieldLevelEncryptionProfileConfig $
--             newGetFieldLevelEncryptionProfileConfig
--
--         , requestDeleteOriginRequestPolicy $
--             newDeleteOriginRequestPolicy
--
--         , requestTestFunction $
--             newTestFunction
--
--         , requestCreateKeyGroup $
--             newCreateKeyGroup
--
--         , requestUpdateOriginRequestPolicy $
--             newUpdateOriginRequestPolicy
--
--         , requestUpdateFieldLevelEncryptionProfile $
--             newUpdateFieldLevelEncryptionProfile
--
--         , requestDeleteKeyGroup $
--             newDeleteKeyGroup
--
--         , requestGetOriginRequestPolicyConfig $
--             newGetOriginRequestPolicyConfig
--
--         , requestListFieldLevelEncryptionProfiles $
--             newListFieldLevelEncryptionProfiles
--
--         , requestDeleteFieldLevelEncryptionProfile $
--             newDeleteFieldLevelEncryptionProfile
--
--         , requestUpdateKeyGroup $
--             newUpdateKeyGroup
--
--         , requestCreateStreamingDistributionWithTags $
--             newCreateStreamingDistributionWithTags
--
--         , requestListDistributionsByRealtimeLogConfig $
--             newListDistributionsByRealtimeLogConfig
--
--         , requestCreateFunction $
--             newCreateFunction
--
--         , requestDeleteMonitoringSubscription $
--             newDeleteMonitoringSubscription
--
--         , requestGetFieldLevelEncryptionProfile $
--             newGetFieldLevelEncryptionProfile
--
--         , requestCreateDistribution $
--             newCreateDistribution
--
--         , requestDeleteDistribution $
--             newDeleteDistribution
--
--         , requestDeleteFunction $
--             newDeleteFunction
--
--         , requestUpdateDistribution $
--             newUpdateDistribution
--
--         , requestCreateMonitoringSubscription $
--             newCreateMonitoringSubscription
--
--         , requestGetOriginRequestPolicy $
--             newGetOriginRequestPolicy
--
--         , requestUpdateFunction $
--             newUpdateFunction
--
--         , requestGetCachePolicyConfig $
--             newGetCachePolicyConfig
--
--         , requestGetStreamingDistribution $
--             newGetStreamingDistribution
--
--         , requestCreateInvalidation $
--             newCreateInvalidation
--
--         , requestUpdateRealtimeLogConfig $
--             newUpdateRealtimeLogConfig
--
--         , requestDeleteRealtimeLogConfig $
--             newDeleteRealtimeLogConfig
--
--         , requestListInvalidations $
--             newListInvalidations
--
--         , requestCreateDistributionWithTags $
--             newCreateDistributionWithTags
--
--         , requestCreateFieldLevelEncryptionConfig $
--             newCreateFieldLevelEncryptionConfig
--
--         , requestCreateRealtimeLogConfig $
--             newCreateRealtimeLogConfig
--
--         , requestGetStreamingDistributionConfig $
--             newGetStreamingDistributionConfig
--
--         , requestGetCachePolicy $
--             newGetCachePolicy
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestCreatePublicKey $
--             newCreatePublicKey
--
--         , requestCreateCloudFrontOriginAccessIdentity $
--             newCreateCloudFrontOriginAccessIdentity
--
--         , requestListCloudFrontOriginAccessIdentities $
--             newListCloudFrontOriginAccessIdentities
--
--         , requestAssociateAlias $
--             newAssociateAlias
--
--           ]

--     , testGroup "response"
--         [ responseDeletePublicKey $
--             newDeletePublicKeyResponse
--
--         , responseListPublicKeys $
--             newListPublicKeysResponse
--
--         , responseUpdatePublicKey $
--             newUpdatePublicKeyResponse
--
--         , responseGetDistribution $
--             newGetDistributionResponse
--
--         , responseGetFunction $
--             newGetFunctionResponse
--
--         , responseListConflictingAliases $
--             newListConflictingAliasesResponse
--
--         , responseGetKeyGroupConfig $
--             newGetKeyGroupConfigResponse
--
--         , responseCreateFieldLevelEncryptionProfile $
--             newCreateFieldLevelEncryptionProfileResponse
--
--         , responseListKeyGroups $
--             newListKeyGroupsResponse
--
--         , responseGetMonitoringSubscription $
--             newGetMonitoringSubscriptionResponse
--
--         , responseListDistributionsByCachePolicyId $
--             newListDistributionsByCachePolicyIdResponse
--
--         , responseListOriginRequestPolicies $
--             newListOriginRequestPoliciesResponse
--
--         , responseCreateOriginRequestPolicy $
--             newCreateOriginRequestPolicyResponse
--
--         , responseGetKeyGroup $
--             newGetKeyGroupResponse
--
--         , responseGetDistributionConfig $
--             newGetDistributionConfigResponse
--
--         , responseListFunctions $
--             newListFunctionsResponse
--
--         , responseListDistributionsByWebACLId $
--             newListDistributionsByWebACLIdResponse
--
--         , responseListDistributions $
--             newListDistributionsResponse
--
--         , responsePublishFunction $
--             newPublishFunctionResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUpdateFieldLevelEncryptionConfig $
--             newUpdateFieldLevelEncryptionConfigResponse
--
--         , responseGetCloudFrontOriginAccessIdentity $
--             newGetCloudFrontOriginAccessIdentityResponse
--
--         , responseDeleteFieldLevelEncryptionConfig $
--             newDeleteFieldLevelEncryptionConfigResponse
--
--         , responseListRealtimeLogConfigs $
--             newListRealtimeLogConfigsResponse
--
--         , responseListCachePolicies $
--             newListCachePoliciesResponse
--
--         , responseGetPublicKey $
--             newGetPublicKeyResponse
--
--         , responseCreateCachePolicy $
--             newCreateCachePolicyResponse
--
--         , responseListDistributionsByKeyGroup $
--             newListDistributionsByKeyGroupResponse
--
--         , responseListDistributionsByOriginRequestPolicyId $
--             newListDistributionsByOriginRequestPolicyIdResponse
--
--         , responseListFieldLevelEncryptionConfigs $
--             newListFieldLevelEncryptionConfigsResponse
--
--         , responseGetFieldLevelEncryption $
--             newGetFieldLevelEncryptionResponse
--
--         , responseUpdateCachePolicy $
--             newUpdateCachePolicyResponse
--
--         , responseDeleteCachePolicy $
--             newDeleteCachePolicyResponse
--
--         , responseGetInvalidation $
--             newGetInvalidationResponse
--
--         , responseCreateStreamingDistribution $
--             newCreateStreamingDistributionResponse
--
--         , responseGetPublicKeyConfig $
--             newGetPublicKeyConfigResponse
--
--         , responseGetCloudFrontOriginAccessIdentityConfig $
--             newGetCloudFrontOriginAccessIdentityConfigResponse
--
--         , responseUpdateCloudFrontOriginAccessIdentity $
--             newUpdateCloudFrontOriginAccessIdentityResponse
--
--         , responseGetRealtimeLogConfig $
--             newGetRealtimeLogConfigResponse
--
--         , responseDeleteStreamingDistribution $
--             newDeleteStreamingDistributionResponse
--
--         , responseUpdateStreamingDistribution $
--             newUpdateStreamingDistributionResponse
--
--         , responseDescribeFunction $
--             newDescribeFunctionResponse
--
--         , responseDeleteCloudFrontOriginAccessIdentity $
--             newDeleteCloudFrontOriginAccessIdentityResponse
--
--         , responseGetFieldLevelEncryptionConfig $
--             newGetFieldLevelEncryptionConfigResponse
--
--         , responseListStreamingDistributions $
--             newListStreamingDistributionsResponse
--
--         , responseGetFieldLevelEncryptionProfileConfig $
--             newGetFieldLevelEncryptionProfileConfigResponse
--
--         , responseDeleteOriginRequestPolicy $
--             newDeleteOriginRequestPolicyResponse
--
--         , responseTestFunction $
--             newTestFunctionResponse
--
--         , responseCreateKeyGroup $
--             newCreateKeyGroupResponse
--
--         , responseUpdateOriginRequestPolicy $
--             newUpdateOriginRequestPolicyResponse
--
--         , responseUpdateFieldLevelEncryptionProfile $
--             newUpdateFieldLevelEncryptionProfileResponse
--
--         , responseDeleteKeyGroup $
--             newDeleteKeyGroupResponse
--
--         , responseGetOriginRequestPolicyConfig $
--             newGetOriginRequestPolicyConfigResponse
--
--         , responseListFieldLevelEncryptionProfiles $
--             newListFieldLevelEncryptionProfilesResponse
--
--         , responseDeleteFieldLevelEncryptionProfile $
--             newDeleteFieldLevelEncryptionProfileResponse
--
--         , responseUpdateKeyGroup $
--             newUpdateKeyGroupResponse
--
--         , responseCreateStreamingDistributionWithTags $
--             newCreateStreamingDistributionWithTagsResponse
--
--         , responseListDistributionsByRealtimeLogConfig $
--             newListDistributionsByRealtimeLogConfigResponse
--
--         , responseCreateFunction $
--             newCreateFunctionResponse
--
--         , responseDeleteMonitoringSubscription $
--             newDeleteMonitoringSubscriptionResponse
--
--         , responseGetFieldLevelEncryptionProfile $
--             newGetFieldLevelEncryptionProfileResponse
--
--         , responseCreateDistribution $
--             newCreateDistributionResponse
--
--         , responseDeleteDistribution $
--             newDeleteDistributionResponse
--
--         , responseDeleteFunction $
--             newDeleteFunctionResponse
--
--         , responseUpdateDistribution $
--             newUpdateDistributionResponse
--
--         , responseCreateMonitoringSubscription $
--             newCreateMonitoringSubscriptionResponse
--
--         , responseGetOriginRequestPolicy $
--             newGetOriginRequestPolicyResponse
--
--         , responseUpdateFunction $
--             newUpdateFunctionResponse
--
--         , responseGetCachePolicyConfig $
--             newGetCachePolicyConfigResponse
--
--         , responseGetStreamingDistribution $
--             newGetStreamingDistributionResponse
--
--         , responseCreateInvalidation $
--             newCreateInvalidationResponse
--
--         , responseUpdateRealtimeLogConfig $
--             newUpdateRealtimeLogConfigResponse
--
--         , responseDeleteRealtimeLogConfig $
--             newDeleteRealtimeLogConfigResponse
--
--         , responseListInvalidations $
--             newListInvalidationsResponse
--
--         , responseCreateDistributionWithTags $
--             newCreateDistributionWithTagsResponse
--
--         , responseCreateFieldLevelEncryptionConfig $
--             newCreateFieldLevelEncryptionConfigResponse
--
--         , responseCreateRealtimeLogConfig $
--             newCreateRealtimeLogConfigResponse
--
--         , responseGetStreamingDistributionConfig $
--             newGetStreamingDistributionConfigResponse
--
--         , responseGetCachePolicy $
--             newGetCachePolicyResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseCreatePublicKey $
--             newCreatePublicKeyResponse
--
--         , responseCreateCloudFrontOriginAccessIdentity $
--             newCreateCloudFrontOriginAccessIdentityResponse
--
--         , responseListCloudFrontOriginAccessIdentities $
--             newListCloudFrontOriginAccessIdentitiesResponse
--
--         , responseAssociateAlias $
--             newAssociateAliasResponse
--
--           ]
--     ]

-- Requests

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

requestUpdatePublicKey :: UpdatePublicKey -> TestTree
requestUpdatePublicKey =
  req
    "UpdatePublicKey"
    "fixture/UpdatePublicKey.yaml"

requestGetDistribution :: GetDistribution -> TestTree
requestGetDistribution =
  req
    "GetDistribution"
    "fixture/GetDistribution.yaml"

requestGetFunction :: GetFunction -> TestTree
requestGetFunction =
  req
    "GetFunction"
    "fixture/GetFunction.yaml"

requestListConflictingAliases :: ListConflictingAliases -> TestTree
requestListConflictingAliases =
  req
    "ListConflictingAliases"
    "fixture/ListConflictingAliases.yaml"

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

requestListKeyGroups :: ListKeyGroups -> TestTree
requestListKeyGroups =
  req
    "ListKeyGroups"
    "fixture/ListKeyGroups.yaml"

requestGetMonitoringSubscription :: GetMonitoringSubscription -> TestTree
requestGetMonitoringSubscription =
  req
    "GetMonitoringSubscription"
    "fixture/GetMonitoringSubscription.yaml"

requestListDistributionsByCachePolicyId :: ListDistributionsByCachePolicyId -> TestTree
requestListDistributionsByCachePolicyId =
  req
    "ListDistributionsByCachePolicyId"
    "fixture/ListDistributionsByCachePolicyId.yaml"

requestListOriginRequestPolicies :: ListOriginRequestPolicies -> TestTree
requestListOriginRequestPolicies =
  req
    "ListOriginRequestPolicies"
    "fixture/ListOriginRequestPolicies.yaml"

requestCreateOriginRequestPolicy :: CreateOriginRequestPolicy -> TestTree
requestCreateOriginRequestPolicy =
  req
    "CreateOriginRequestPolicy"
    "fixture/CreateOriginRequestPolicy.yaml"

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

requestListFunctions :: ListFunctions -> TestTree
requestListFunctions =
  req
    "ListFunctions"
    "fixture/ListFunctions.yaml"

requestListDistributionsByWebACLId :: ListDistributionsByWebACLId -> TestTree
requestListDistributionsByWebACLId =
  req
    "ListDistributionsByWebACLId"
    "fixture/ListDistributionsByWebACLId.yaml"

requestListDistributions :: ListDistributions -> TestTree
requestListDistributions =
  req
    "ListDistributions"
    "fixture/ListDistributions.yaml"

requestPublishFunction :: PublishFunction -> TestTree
requestPublishFunction =
  req
    "PublishFunction"
    "fixture/PublishFunction.yaml"

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

requestUpdateFieldLevelEncryptionConfig :: UpdateFieldLevelEncryptionConfig -> TestTree
requestUpdateFieldLevelEncryptionConfig =
  req
    "UpdateFieldLevelEncryptionConfig"
    "fixture/UpdateFieldLevelEncryptionConfig.yaml"

requestGetCloudFrontOriginAccessIdentity :: GetCloudFrontOriginAccessIdentity -> TestTree
requestGetCloudFrontOriginAccessIdentity =
  req
    "GetCloudFrontOriginAccessIdentity"
    "fixture/GetCloudFrontOriginAccessIdentity.yaml"

requestDeleteFieldLevelEncryptionConfig :: DeleteFieldLevelEncryptionConfig -> TestTree
requestDeleteFieldLevelEncryptionConfig =
  req
    "DeleteFieldLevelEncryptionConfig"
    "fixture/DeleteFieldLevelEncryptionConfig.yaml"

requestListRealtimeLogConfigs :: ListRealtimeLogConfigs -> TestTree
requestListRealtimeLogConfigs =
  req
    "ListRealtimeLogConfigs"
    "fixture/ListRealtimeLogConfigs.yaml"

requestListCachePolicies :: ListCachePolicies -> TestTree
requestListCachePolicies =
  req
    "ListCachePolicies"
    "fixture/ListCachePolicies.yaml"

requestGetPublicKey :: GetPublicKey -> TestTree
requestGetPublicKey =
  req
    "GetPublicKey"
    "fixture/GetPublicKey.yaml"

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

requestDeleteCachePolicy :: DeleteCachePolicy -> TestTree
requestDeleteCachePolicy =
  req
    "DeleteCachePolicy"
    "fixture/DeleteCachePolicy.yaml"

requestGetInvalidation :: GetInvalidation -> TestTree
requestGetInvalidation =
  req
    "GetInvalidation"
    "fixture/GetInvalidation.yaml"

requestCreateStreamingDistribution :: CreateStreamingDistribution -> TestTree
requestCreateStreamingDistribution =
  req
    "CreateStreamingDistribution"
    "fixture/CreateStreamingDistribution.yaml"

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

requestUpdateCloudFrontOriginAccessIdentity :: UpdateCloudFrontOriginAccessIdentity -> TestTree
requestUpdateCloudFrontOriginAccessIdentity =
  req
    "UpdateCloudFrontOriginAccessIdentity"
    "fixture/UpdateCloudFrontOriginAccessIdentity.yaml"

requestGetRealtimeLogConfig :: GetRealtimeLogConfig -> TestTree
requestGetRealtimeLogConfig =
  req
    "GetRealtimeLogConfig"
    "fixture/GetRealtimeLogConfig.yaml"

requestDeleteStreamingDistribution :: DeleteStreamingDistribution -> TestTree
requestDeleteStreamingDistribution =
  req
    "DeleteStreamingDistribution"
    "fixture/DeleteStreamingDistribution.yaml"

requestUpdateStreamingDistribution :: UpdateStreamingDistribution -> TestTree
requestUpdateStreamingDistribution =
  req
    "UpdateStreamingDistribution"
    "fixture/UpdateStreamingDistribution.yaml"

requestDescribeFunction :: DescribeFunction -> TestTree
requestDescribeFunction =
  req
    "DescribeFunction"
    "fixture/DescribeFunction.yaml"

requestDeleteCloudFrontOriginAccessIdentity :: DeleteCloudFrontOriginAccessIdentity -> TestTree
requestDeleteCloudFrontOriginAccessIdentity =
  req
    "DeleteCloudFrontOriginAccessIdentity"
    "fixture/DeleteCloudFrontOriginAccessIdentity.yaml"

requestGetFieldLevelEncryptionConfig :: GetFieldLevelEncryptionConfig -> TestTree
requestGetFieldLevelEncryptionConfig =
  req
    "GetFieldLevelEncryptionConfig"
    "fixture/GetFieldLevelEncryptionConfig.yaml"

requestListStreamingDistributions :: ListStreamingDistributions -> TestTree
requestListStreamingDistributions =
  req
    "ListStreamingDistributions"
    "fixture/ListStreamingDistributions.yaml"

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

requestTestFunction :: TestFunction -> TestTree
requestTestFunction =
  req
    "TestFunction"
    "fixture/TestFunction.yaml"

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

requestUpdateFieldLevelEncryptionProfile :: UpdateFieldLevelEncryptionProfile -> TestTree
requestUpdateFieldLevelEncryptionProfile =
  req
    "UpdateFieldLevelEncryptionProfile"
    "fixture/UpdateFieldLevelEncryptionProfile.yaml"

requestDeleteKeyGroup :: DeleteKeyGroup -> TestTree
requestDeleteKeyGroup =
  req
    "DeleteKeyGroup"
    "fixture/DeleteKeyGroup.yaml"

requestGetOriginRequestPolicyConfig :: GetOriginRequestPolicyConfig -> TestTree
requestGetOriginRequestPolicyConfig =
  req
    "GetOriginRequestPolicyConfig"
    "fixture/GetOriginRequestPolicyConfig.yaml"

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

requestUpdateKeyGroup :: UpdateKeyGroup -> TestTree
requestUpdateKeyGroup =
  req
    "UpdateKeyGroup"
    "fixture/UpdateKeyGroup.yaml"

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

requestCreateFunction :: CreateFunction -> TestTree
requestCreateFunction =
  req
    "CreateFunction"
    "fixture/CreateFunction.yaml"

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

requestCreateDistribution :: CreateDistribution -> TestTree
requestCreateDistribution =
  req
    "CreateDistribution"
    "fixture/CreateDistribution.yaml"

requestDeleteDistribution :: DeleteDistribution -> TestTree
requestDeleteDistribution =
  req
    "DeleteDistribution"
    "fixture/DeleteDistribution.yaml"

requestDeleteFunction :: DeleteFunction -> TestTree
requestDeleteFunction =
  req
    "DeleteFunction"
    "fixture/DeleteFunction.yaml"

requestUpdateDistribution :: UpdateDistribution -> TestTree
requestUpdateDistribution =
  req
    "UpdateDistribution"
    "fixture/UpdateDistribution.yaml"

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

requestUpdateFunction :: UpdateFunction -> TestTree
requestUpdateFunction =
  req
    "UpdateFunction"
    "fixture/UpdateFunction.yaml"

requestGetCachePolicyConfig :: GetCachePolicyConfig -> TestTree
requestGetCachePolicyConfig =
  req
    "GetCachePolicyConfig"
    "fixture/GetCachePolicyConfig.yaml"

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

requestUpdateRealtimeLogConfig :: UpdateRealtimeLogConfig -> TestTree
requestUpdateRealtimeLogConfig =
  req
    "UpdateRealtimeLogConfig"
    "fixture/UpdateRealtimeLogConfig.yaml"

requestDeleteRealtimeLogConfig :: DeleteRealtimeLogConfig -> TestTree
requestDeleteRealtimeLogConfig =
  req
    "DeleteRealtimeLogConfig"
    "fixture/DeleteRealtimeLogConfig.yaml"

requestListInvalidations :: ListInvalidations -> TestTree
requestListInvalidations =
  req
    "ListInvalidations"
    "fixture/ListInvalidations.yaml"

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

requestCreateRealtimeLogConfig :: CreateRealtimeLogConfig -> TestTree
requestCreateRealtimeLogConfig =
  req
    "CreateRealtimeLogConfig"
    "fixture/CreateRealtimeLogConfig.yaml"

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

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestCreatePublicKey :: CreatePublicKey -> TestTree
requestCreatePublicKey =
  req
    "CreatePublicKey"
    "fixture/CreatePublicKey.yaml"

requestCreateCloudFrontOriginAccessIdentity :: CreateCloudFrontOriginAccessIdentity -> TestTree
requestCreateCloudFrontOriginAccessIdentity =
  req
    "CreateCloudFrontOriginAccessIdentity"
    "fixture/CreateCloudFrontOriginAccessIdentity.yaml"

requestListCloudFrontOriginAccessIdentities :: ListCloudFrontOriginAccessIdentities -> TestTree
requestListCloudFrontOriginAccessIdentities =
  req
    "ListCloudFrontOriginAccessIdentities"
    "fixture/ListCloudFrontOriginAccessIdentities.yaml"

requestAssociateAlias :: AssociateAlias -> TestTree
requestAssociateAlias =
  req
    "AssociateAlias"
    "fixture/AssociateAlias.yaml"

-- Responses

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

responseUpdatePublicKey :: UpdatePublicKeyResponse -> TestTree
responseUpdatePublicKey =
  res
    "UpdatePublicKeyResponse"
    "fixture/UpdatePublicKeyResponse.proto"
    defaultService
    (Proxy :: Proxy UpdatePublicKey)

responseGetDistribution :: GetDistributionResponse -> TestTree
responseGetDistribution =
  res
    "GetDistributionResponse"
    "fixture/GetDistributionResponse.proto"
    defaultService
    (Proxy :: Proxy GetDistribution)

responseGetFunction :: GetFunctionResponse -> TestTree
responseGetFunction =
  res
    "GetFunctionResponse"
    "fixture/GetFunctionResponse.proto"
    defaultService
    (Proxy :: Proxy GetFunction)

responseListConflictingAliases :: ListConflictingAliasesResponse -> TestTree
responseListConflictingAliases =
  res
    "ListConflictingAliasesResponse"
    "fixture/ListConflictingAliasesResponse.proto"
    defaultService
    (Proxy :: Proxy ListConflictingAliases)

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

responseListKeyGroups :: ListKeyGroupsResponse -> TestTree
responseListKeyGroups =
  res
    "ListKeyGroupsResponse"
    "fixture/ListKeyGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy ListKeyGroups)

responseGetMonitoringSubscription :: GetMonitoringSubscriptionResponse -> TestTree
responseGetMonitoringSubscription =
  res
    "GetMonitoringSubscriptionResponse"
    "fixture/GetMonitoringSubscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy GetMonitoringSubscription)

responseListDistributionsByCachePolicyId :: ListDistributionsByCachePolicyIdResponse -> TestTree
responseListDistributionsByCachePolicyId =
  res
    "ListDistributionsByCachePolicyIdResponse"
    "fixture/ListDistributionsByCachePolicyIdResponse.proto"
    defaultService
    (Proxy :: Proxy ListDistributionsByCachePolicyId)

responseListOriginRequestPolicies :: ListOriginRequestPoliciesResponse -> TestTree
responseListOriginRequestPolicies =
  res
    "ListOriginRequestPoliciesResponse"
    "fixture/ListOriginRequestPoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy ListOriginRequestPolicies)

responseCreateOriginRequestPolicy :: CreateOriginRequestPolicyResponse -> TestTree
responseCreateOriginRequestPolicy =
  res
    "CreateOriginRequestPolicyResponse"
    "fixture/CreateOriginRequestPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy CreateOriginRequestPolicy)

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

responseListFunctions :: ListFunctionsResponse -> TestTree
responseListFunctions =
  res
    "ListFunctionsResponse"
    "fixture/ListFunctionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListFunctions)

responseListDistributionsByWebACLId :: ListDistributionsByWebACLIdResponse -> TestTree
responseListDistributionsByWebACLId =
  res
    "ListDistributionsByWebACLIdResponse"
    "fixture/ListDistributionsByWebACLIdResponse.proto"
    defaultService
    (Proxy :: Proxy ListDistributionsByWebACLId)

responseListDistributions :: ListDistributionsResponse -> TestTree
responseListDistributions =
  res
    "ListDistributionsResponse"
    "fixture/ListDistributionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDistributions)

responsePublishFunction :: PublishFunctionResponse -> TestTree
responsePublishFunction =
  res
    "PublishFunctionResponse"
    "fixture/PublishFunctionResponse.proto"
    defaultService
    (Proxy :: Proxy PublishFunction)

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

responseUpdateFieldLevelEncryptionConfig :: UpdateFieldLevelEncryptionConfigResponse -> TestTree
responseUpdateFieldLevelEncryptionConfig =
  res
    "UpdateFieldLevelEncryptionConfigResponse"
    "fixture/UpdateFieldLevelEncryptionConfigResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateFieldLevelEncryptionConfig)

responseGetCloudFrontOriginAccessIdentity :: GetCloudFrontOriginAccessIdentityResponse -> TestTree
responseGetCloudFrontOriginAccessIdentity =
  res
    "GetCloudFrontOriginAccessIdentityResponse"
    "fixture/GetCloudFrontOriginAccessIdentityResponse.proto"
    defaultService
    (Proxy :: Proxy GetCloudFrontOriginAccessIdentity)

responseDeleteFieldLevelEncryptionConfig :: DeleteFieldLevelEncryptionConfigResponse -> TestTree
responseDeleteFieldLevelEncryptionConfig =
  res
    "DeleteFieldLevelEncryptionConfigResponse"
    "fixture/DeleteFieldLevelEncryptionConfigResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteFieldLevelEncryptionConfig)

responseListRealtimeLogConfigs :: ListRealtimeLogConfigsResponse -> TestTree
responseListRealtimeLogConfigs =
  res
    "ListRealtimeLogConfigsResponse"
    "fixture/ListRealtimeLogConfigsResponse.proto"
    defaultService
    (Proxy :: Proxy ListRealtimeLogConfigs)

responseListCachePolicies :: ListCachePoliciesResponse -> TestTree
responseListCachePolicies =
  res
    "ListCachePoliciesResponse"
    "fixture/ListCachePoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy ListCachePolicies)

responseGetPublicKey :: GetPublicKeyResponse -> TestTree
responseGetPublicKey =
  res
    "GetPublicKeyResponse"
    "fixture/GetPublicKeyResponse.proto"
    defaultService
    (Proxy :: Proxy GetPublicKey)

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

responseDeleteCachePolicy :: DeleteCachePolicyResponse -> TestTree
responseDeleteCachePolicy =
  res
    "DeleteCachePolicyResponse"
    "fixture/DeleteCachePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCachePolicy)

responseGetInvalidation :: GetInvalidationResponse -> TestTree
responseGetInvalidation =
  res
    "GetInvalidationResponse"
    "fixture/GetInvalidationResponse.proto"
    defaultService
    (Proxy :: Proxy GetInvalidation)

responseCreateStreamingDistribution :: CreateStreamingDistributionResponse -> TestTree
responseCreateStreamingDistribution =
  res
    "CreateStreamingDistributionResponse"
    "fixture/CreateStreamingDistributionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateStreamingDistribution)

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

responseUpdateCloudFrontOriginAccessIdentity :: UpdateCloudFrontOriginAccessIdentityResponse -> TestTree
responseUpdateCloudFrontOriginAccessIdentity =
  res
    "UpdateCloudFrontOriginAccessIdentityResponse"
    "fixture/UpdateCloudFrontOriginAccessIdentityResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateCloudFrontOriginAccessIdentity)

responseGetRealtimeLogConfig :: GetRealtimeLogConfigResponse -> TestTree
responseGetRealtimeLogConfig =
  res
    "GetRealtimeLogConfigResponse"
    "fixture/GetRealtimeLogConfigResponse.proto"
    defaultService
    (Proxy :: Proxy GetRealtimeLogConfig)

responseDeleteStreamingDistribution :: DeleteStreamingDistributionResponse -> TestTree
responseDeleteStreamingDistribution =
  res
    "DeleteStreamingDistributionResponse"
    "fixture/DeleteStreamingDistributionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteStreamingDistribution)

responseUpdateStreamingDistribution :: UpdateStreamingDistributionResponse -> TestTree
responseUpdateStreamingDistribution =
  res
    "UpdateStreamingDistributionResponse"
    "fixture/UpdateStreamingDistributionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateStreamingDistribution)

responseDescribeFunction :: DescribeFunctionResponse -> TestTree
responseDescribeFunction =
  res
    "DescribeFunctionResponse"
    "fixture/DescribeFunctionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeFunction)

responseDeleteCloudFrontOriginAccessIdentity :: DeleteCloudFrontOriginAccessIdentityResponse -> TestTree
responseDeleteCloudFrontOriginAccessIdentity =
  res
    "DeleteCloudFrontOriginAccessIdentityResponse"
    "fixture/DeleteCloudFrontOriginAccessIdentityResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCloudFrontOriginAccessIdentity)

responseGetFieldLevelEncryptionConfig :: GetFieldLevelEncryptionConfigResponse -> TestTree
responseGetFieldLevelEncryptionConfig =
  res
    "GetFieldLevelEncryptionConfigResponse"
    "fixture/GetFieldLevelEncryptionConfigResponse.proto"
    defaultService
    (Proxy :: Proxy GetFieldLevelEncryptionConfig)

responseListStreamingDistributions :: ListStreamingDistributionsResponse -> TestTree
responseListStreamingDistributions =
  res
    "ListStreamingDistributionsResponse"
    "fixture/ListStreamingDistributionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListStreamingDistributions)

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

responseTestFunction :: TestFunctionResponse -> TestTree
responseTestFunction =
  res
    "TestFunctionResponse"
    "fixture/TestFunctionResponse.proto"
    defaultService
    (Proxy :: Proxy TestFunction)

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

responseUpdateFieldLevelEncryptionProfile :: UpdateFieldLevelEncryptionProfileResponse -> TestTree
responseUpdateFieldLevelEncryptionProfile =
  res
    "UpdateFieldLevelEncryptionProfileResponse"
    "fixture/UpdateFieldLevelEncryptionProfileResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateFieldLevelEncryptionProfile)

responseDeleteKeyGroup :: DeleteKeyGroupResponse -> TestTree
responseDeleteKeyGroup =
  res
    "DeleteKeyGroupResponse"
    "fixture/DeleteKeyGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteKeyGroup)

responseGetOriginRequestPolicyConfig :: GetOriginRequestPolicyConfigResponse -> TestTree
responseGetOriginRequestPolicyConfig =
  res
    "GetOriginRequestPolicyConfigResponse"
    "fixture/GetOriginRequestPolicyConfigResponse.proto"
    defaultService
    (Proxy :: Proxy GetOriginRequestPolicyConfig)

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

responseUpdateKeyGroup :: UpdateKeyGroupResponse -> TestTree
responseUpdateKeyGroup =
  res
    "UpdateKeyGroupResponse"
    "fixture/UpdateKeyGroupResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateKeyGroup)

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

responseCreateFunction :: CreateFunctionResponse -> TestTree
responseCreateFunction =
  res
    "CreateFunctionResponse"
    "fixture/CreateFunctionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateFunction)

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

responseCreateDistribution :: CreateDistributionResponse -> TestTree
responseCreateDistribution =
  res
    "CreateDistributionResponse"
    "fixture/CreateDistributionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDistribution)

responseDeleteDistribution :: DeleteDistributionResponse -> TestTree
responseDeleteDistribution =
  res
    "DeleteDistributionResponse"
    "fixture/DeleteDistributionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDistribution)

responseDeleteFunction :: DeleteFunctionResponse -> TestTree
responseDeleteFunction =
  res
    "DeleteFunctionResponse"
    "fixture/DeleteFunctionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteFunction)

responseUpdateDistribution :: UpdateDistributionResponse -> TestTree
responseUpdateDistribution =
  res
    "UpdateDistributionResponse"
    "fixture/UpdateDistributionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDistribution)

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

responseUpdateFunction :: UpdateFunctionResponse -> TestTree
responseUpdateFunction =
  res
    "UpdateFunctionResponse"
    "fixture/UpdateFunctionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateFunction)

responseGetCachePolicyConfig :: GetCachePolicyConfigResponse -> TestTree
responseGetCachePolicyConfig =
  res
    "GetCachePolicyConfigResponse"
    "fixture/GetCachePolicyConfigResponse.proto"
    defaultService
    (Proxy :: Proxy GetCachePolicyConfig)

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

responseUpdateRealtimeLogConfig :: UpdateRealtimeLogConfigResponse -> TestTree
responseUpdateRealtimeLogConfig =
  res
    "UpdateRealtimeLogConfigResponse"
    "fixture/UpdateRealtimeLogConfigResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRealtimeLogConfig)

responseDeleteRealtimeLogConfig :: DeleteRealtimeLogConfigResponse -> TestTree
responseDeleteRealtimeLogConfig =
  res
    "DeleteRealtimeLogConfigResponse"
    "fixture/DeleteRealtimeLogConfigResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRealtimeLogConfig)

responseListInvalidations :: ListInvalidationsResponse -> TestTree
responseListInvalidations =
  res
    "ListInvalidationsResponse"
    "fixture/ListInvalidationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListInvalidations)

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

responseCreateRealtimeLogConfig :: CreateRealtimeLogConfigResponse -> TestTree
responseCreateRealtimeLogConfig =
  res
    "CreateRealtimeLogConfigResponse"
    "fixture/CreateRealtimeLogConfigResponse.proto"
    defaultService
    (Proxy :: Proxy CreateRealtimeLogConfig)

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

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseCreatePublicKey :: CreatePublicKeyResponse -> TestTree
responseCreatePublicKey =
  res
    "CreatePublicKeyResponse"
    "fixture/CreatePublicKeyResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePublicKey)

responseCreateCloudFrontOriginAccessIdentity :: CreateCloudFrontOriginAccessIdentityResponse -> TestTree
responseCreateCloudFrontOriginAccessIdentity =
  res
    "CreateCloudFrontOriginAccessIdentityResponse"
    "fixture/CreateCloudFrontOriginAccessIdentityResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCloudFrontOriginAccessIdentity)

responseListCloudFrontOriginAccessIdentities :: ListCloudFrontOriginAccessIdentitiesResponse -> TestTree
responseListCloudFrontOriginAccessIdentities =
  res
    "ListCloudFrontOriginAccessIdentitiesResponse"
    "fixture/ListCloudFrontOriginAccessIdentitiesResponse.proto"
    defaultService
    (Proxy :: Proxy ListCloudFrontOriginAccessIdentities)

responseAssociateAlias :: AssociateAliasResponse -> TestTree
responseAssociateAlias =
  res
    "AssociateAliasResponse"
    "fixture/AssociateAliasResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateAlias)
