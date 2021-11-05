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

import Amazonka.CloudFront
import qualified Data.Proxy as Proxy
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
--         [ requestDeleteOriginRequestPolicy $
--             newDeleteOriginRequestPolicy
--
--         , requestUpdateOriginRequestPolicy $
--             newUpdateOriginRequestPolicy
--
--         , requestListConflictingAliases $
--             newListConflictingAliases
--
--         , requestDeleteStreamingDistribution $
--             newDeleteStreamingDistribution
--
--         , requestUpdateStreamingDistribution $
--             newUpdateStreamingDistribution
--
--         , requestListPublicKeys $
--             newListPublicKeys
--
--         , requestGetFieldLevelEncryptionConfig $
--             newGetFieldLevelEncryptionConfig
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestCreatePublicKey $
--             newCreatePublicKey
--
--         , requestGetPublicKeyConfig $
--             newGetPublicKeyConfig
--
--         , requestCreateDistributionWithTags $
--             newCreateDistributionWithTags
--
--         , requestCreateFieldLevelEncryptionConfig $
--             newCreateFieldLevelEncryptionConfig
--
--         , requestDeleteCachePolicy $
--             newDeleteCachePolicy
--
--         , requestUpdateCachePolicy $
--             newUpdateCachePolicy
--
--         , requestGetFieldLevelEncryption $
--             newGetFieldLevelEncryption
--
--         , requestListRealtimeLogConfigs $
--             newListRealtimeLogConfigs
--
--         , requestGetPublicKey $
--             newGetPublicKey
--
--         , requestDeleteRealtimeLogConfig $
--             newDeleteRealtimeLogConfig
--
--         , requestUpdateRealtimeLogConfig $
--             newUpdateRealtimeLogConfig
--
--         , requestListDistributionsByOriginRequestPolicyId $
--             newListDistributionsByOriginRequestPolicyId
--
--         , requestDeleteFieldLevelEncryptionConfig $
--             newDeleteFieldLevelEncryptionConfig
--
--         , requestUpdateFieldLevelEncryptionConfig $
--             newUpdateFieldLevelEncryptionConfig
--
--         , requestGetKeyGroup $
--             newGetKeyGroup
--
--         , requestCreateDistribution $
--             newCreateDistribution
--
--         , requestGetFieldLevelEncryptionProfile $
--             newGetFieldLevelEncryptionProfile
--
--         , requestDeleteMonitoringSubscription $
--             newDeleteMonitoringSubscription
--
--         , requestCreateFunction $
--             newCreateFunction
--
--         , requestGetDistributionConfig $
--             newGetDistributionConfig
--
--         , requestCreateStreamingDistributionWithTags $
--             newCreateStreamingDistributionWithTags
--
--         , requestDeleteFieldLevelEncryptionProfile $
--             newDeleteFieldLevelEncryptionProfile
--
--         , requestUpdateFieldLevelEncryptionProfile $
--             newUpdateFieldLevelEncryptionProfile
--
--         , requestListDistributionsByCachePolicyId $
--             newListDistributionsByCachePolicyId
--
--         , requestTestFunction $
--             newTestFunction
--
--         , requestCreateFieldLevelEncryptionProfile $
--             newCreateFieldLevelEncryptionProfile
--
--         , requestGetKeyGroupConfig $
--             newGetKeyGroupConfig
--
--         , requestGetDistribution $
--             newGetDistribution
--
--         , requestGetFieldLevelEncryptionProfileConfig $
--             newGetFieldLevelEncryptionProfileConfig
--
--         , requestGetFunction $
--             newGetFunction
--
--         , requestCreateKeyGroup $
--             newCreateKeyGroup
--
--         , requestUpdateCloudFrontOriginAccessIdentity $
--             newUpdateCloudFrontOriginAccessIdentity
--
--         , requestDeleteCloudFrontOriginAccessIdentity $
--             newDeleteCloudFrontOriginAccessIdentity
--
--         , requestListStreamingDistributions $
--             newListStreamingDistributions
--
--         , requestDeletePublicKey $
--             newDeletePublicKey
--
--         , requestUpdatePublicKey $
--             newUpdatePublicKey
--
--         , requestGetRealtimeLogConfig $
--             newGetRealtimeLogConfig
--
--         , requestDescribeFunction $
--             newDescribeFunction
--
--         , requestGetStreamingDistributionConfig $
--             newGetStreamingDistributionConfig
--
--         , requestGetCloudFrontOriginAccessIdentityConfig $
--             newGetCloudFrontOriginAccessIdentityConfig
--
--         , requestCreateStreamingDistribution $
--             newCreateStreamingDistribution
--
--         , requestCreateCloudFrontOriginAccessIdentity $
--             newCreateCloudFrontOriginAccessIdentity
--
--         , requestListCloudFrontOriginAccessIdentities $
--             newListCloudFrontOriginAccessIdentities
--
--         , requestGetInvalidation $
--             newGetInvalidation
--
--         , requestGetCachePolicy $
--             newGetCachePolicy
--
--         , requestAssociateAlias $
--             newAssociateAlias
--
--         , requestCreateRealtimeLogConfig $
--             newCreateRealtimeLogConfig
--
--         , requestListInvalidations $
--             newListInvalidations
--
--         , requestCreateInvalidation $
--             newCreateInvalidation
--
--         , requestGetCloudFrontOriginAccessIdentity $
--             newGetCloudFrontOriginAccessIdentity
--
--         , requestListCachePolicies $
--             newListCachePolicies
--
--         , requestCreateCachePolicy $
--             newCreateCachePolicy
--
--         , requestGetCachePolicyConfig $
--             newGetCachePolicyConfig
--
--         , requestListFieldLevelEncryptionConfigs $
--             newListFieldLevelEncryptionConfigs
--
--         , requestListDistributionsByKeyGroup $
--             newListDistributionsByKeyGroup
--
--         , requestTagResource $
--             newTagResource
--
--         , requestGetStreamingDistribution $
--             newGetStreamingDistribution
--
--         , requestUpdateDistribution $
--             newUpdateDistribution
--
--         , requestUpdateFunction $
--             newUpdateFunction
--
--         , requestDeleteDistribution $
--             newDeleteDistribution
--
--         , requestDeleteFunction $
--             newDeleteFunction
--
--         , requestGetOriginRequestPolicy $
--             newGetOriginRequestPolicy
--
--         , requestPublishFunction $
--             newPublishFunction
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestCreateMonitoringSubscription $
--             newCreateMonitoringSubscription
--
--         , requestListDistributionsByWebACLId $
--             newListDistributionsByWebACLId
--
--         , requestListDistributions $
--             newListDistributions
--
--         , requestListFunctions $
--             newListFunctions
--
--         , requestListDistributionsByRealtimeLogConfig $
--             newListDistributionsByRealtimeLogConfig
--
--         , requestCreateOriginRequestPolicy $
--             newCreateOriginRequestPolicy
--
--         , requestListKeyGroups $
--             newListKeyGroups
--
--         , requestListFieldLevelEncryptionProfiles $
--             newListFieldLevelEncryptionProfiles
--
--         , requestGetMonitoringSubscription $
--             newGetMonitoringSubscription
--
--         , requestUpdateKeyGroup $
--             newUpdateKeyGroup
--
--         , requestDeleteKeyGroup $
--             newDeleteKeyGroup
--
--         , requestListOriginRequestPolicies $
--             newListOriginRequestPolicies
--
--         , requestGetOriginRequestPolicyConfig $
--             newGetOriginRequestPolicyConfig
--
--           ]

--     , testGroup "response"
--         [ responseDeleteOriginRequestPolicy $
--             newDeleteOriginRequestPolicyResponse
--
--         , responseUpdateOriginRequestPolicy $
--             newUpdateOriginRequestPolicyResponse
--
--         , responseListConflictingAliases $
--             newListConflictingAliasesResponse
--
--         , responseDeleteStreamingDistribution $
--             newDeleteStreamingDistributionResponse
--
--         , responseUpdateStreamingDistribution $
--             newUpdateStreamingDistributionResponse
--
--         , responseListPublicKeys $
--             newListPublicKeysResponse
--
--         , responseGetFieldLevelEncryptionConfig $
--             newGetFieldLevelEncryptionConfigResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseCreatePublicKey $
--             newCreatePublicKeyResponse
--
--         , responseGetPublicKeyConfig $
--             newGetPublicKeyConfigResponse
--
--         , responseCreateDistributionWithTags $
--             newCreateDistributionWithTagsResponse
--
--         , responseCreateFieldLevelEncryptionConfig $
--             newCreateFieldLevelEncryptionConfigResponse
--
--         , responseDeleteCachePolicy $
--             newDeleteCachePolicyResponse
--
--         , responseUpdateCachePolicy $
--             newUpdateCachePolicyResponse
--
--         , responseGetFieldLevelEncryption $
--             newGetFieldLevelEncryptionResponse
--
--         , responseListRealtimeLogConfigs $
--             newListRealtimeLogConfigsResponse
--
--         , responseGetPublicKey $
--             newGetPublicKeyResponse
--
--         , responseDeleteRealtimeLogConfig $
--             newDeleteRealtimeLogConfigResponse
--
--         , responseUpdateRealtimeLogConfig $
--             newUpdateRealtimeLogConfigResponse
--
--         , responseListDistributionsByOriginRequestPolicyId $
--             newListDistributionsByOriginRequestPolicyIdResponse
--
--         , responseDeleteFieldLevelEncryptionConfig $
--             newDeleteFieldLevelEncryptionConfigResponse
--
--         , responseUpdateFieldLevelEncryptionConfig $
--             newUpdateFieldLevelEncryptionConfigResponse
--
--         , responseGetKeyGroup $
--             newGetKeyGroupResponse
--
--         , responseCreateDistribution $
--             newCreateDistributionResponse
--
--         , responseGetFieldLevelEncryptionProfile $
--             newGetFieldLevelEncryptionProfileResponse
--
--         , responseDeleteMonitoringSubscription $
--             newDeleteMonitoringSubscriptionResponse
--
--         , responseCreateFunction $
--             newCreateFunctionResponse
--
--         , responseGetDistributionConfig $
--             newGetDistributionConfigResponse
--
--         , responseCreateStreamingDistributionWithTags $
--             newCreateStreamingDistributionWithTagsResponse
--
--         , responseDeleteFieldLevelEncryptionProfile $
--             newDeleteFieldLevelEncryptionProfileResponse
--
--         , responseUpdateFieldLevelEncryptionProfile $
--             newUpdateFieldLevelEncryptionProfileResponse
--
--         , responseListDistributionsByCachePolicyId $
--             newListDistributionsByCachePolicyIdResponse
--
--         , responseTestFunction $
--             newTestFunctionResponse
--
--         , responseCreateFieldLevelEncryptionProfile $
--             newCreateFieldLevelEncryptionProfileResponse
--
--         , responseGetKeyGroupConfig $
--             newGetKeyGroupConfigResponse
--
--         , responseGetDistribution $
--             newGetDistributionResponse
--
--         , responseGetFieldLevelEncryptionProfileConfig $
--             newGetFieldLevelEncryptionProfileConfigResponse
--
--         , responseGetFunction $
--             newGetFunctionResponse
--
--         , responseCreateKeyGroup $
--             newCreateKeyGroupResponse
--
--         , responseUpdateCloudFrontOriginAccessIdentity $
--             newUpdateCloudFrontOriginAccessIdentityResponse
--
--         , responseDeleteCloudFrontOriginAccessIdentity $
--             newDeleteCloudFrontOriginAccessIdentityResponse
--
--         , responseListStreamingDistributions $
--             newListStreamingDistributionsResponse
--
--         , responseDeletePublicKey $
--             newDeletePublicKeyResponse
--
--         , responseUpdatePublicKey $
--             newUpdatePublicKeyResponse
--
--         , responseGetRealtimeLogConfig $
--             newGetRealtimeLogConfigResponse
--
--         , responseDescribeFunction $
--             newDescribeFunctionResponse
--
--         , responseGetStreamingDistributionConfig $
--             newGetStreamingDistributionConfigResponse
--
--         , responseGetCloudFrontOriginAccessIdentityConfig $
--             newGetCloudFrontOriginAccessIdentityConfigResponse
--
--         , responseCreateStreamingDistribution $
--             newCreateStreamingDistributionResponse
--
--         , responseCreateCloudFrontOriginAccessIdentity $
--             newCreateCloudFrontOriginAccessIdentityResponse
--
--         , responseListCloudFrontOriginAccessIdentities $
--             newListCloudFrontOriginAccessIdentitiesResponse
--
--         , responseGetInvalidation $
--             newGetInvalidationResponse
--
--         , responseGetCachePolicy $
--             newGetCachePolicyResponse
--
--         , responseAssociateAlias $
--             newAssociateAliasResponse
--
--         , responseCreateRealtimeLogConfig $
--             newCreateRealtimeLogConfigResponse
--
--         , responseListInvalidations $
--             newListInvalidationsResponse
--
--         , responseCreateInvalidation $
--             newCreateInvalidationResponse
--
--         , responseGetCloudFrontOriginAccessIdentity $
--             newGetCloudFrontOriginAccessIdentityResponse
--
--         , responseListCachePolicies $
--             newListCachePoliciesResponse
--
--         , responseCreateCachePolicy $
--             newCreateCachePolicyResponse
--
--         , responseGetCachePolicyConfig $
--             newGetCachePolicyConfigResponse
--
--         , responseListFieldLevelEncryptionConfigs $
--             newListFieldLevelEncryptionConfigsResponse
--
--         , responseListDistributionsByKeyGroup $
--             newListDistributionsByKeyGroupResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseGetStreamingDistribution $
--             newGetStreamingDistributionResponse
--
--         , responseUpdateDistribution $
--             newUpdateDistributionResponse
--
--         , responseUpdateFunction $
--             newUpdateFunctionResponse
--
--         , responseDeleteDistribution $
--             newDeleteDistributionResponse
--
--         , responseDeleteFunction $
--             newDeleteFunctionResponse
--
--         , responseGetOriginRequestPolicy $
--             newGetOriginRequestPolicyResponse
--
--         , responsePublishFunction $
--             newPublishFunctionResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseCreateMonitoringSubscription $
--             newCreateMonitoringSubscriptionResponse
--
--         , responseListDistributionsByWebACLId $
--             newListDistributionsByWebACLIdResponse
--
--         , responseListDistributions $
--             newListDistributionsResponse
--
--         , responseListFunctions $
--             newListFunctionsResponse
--
--         , responseListDistributionsByRealtimeLogConfig $
--             newListDistributionsByRealtimeLogConfigResponse
--
--         , responseCreateOriginRequestPolicy $
--             newCreateOriginRequestPolicyResponse
--
--         , responseListKeyGroups $
--             newListKeyGroupsResponse
--
--         , responseListFieldLevelEncryptionProfiles $
--             newListFieldLevelEncryptionProfilesResponse
--
--         , responseGetMonitoringSubscription $
--             newGetMonitoringSubscriptionResponse
--
--         , responseUpdateKeyGroup $
--             newUpdateKeyGroupResponse
--
--         , responseDeleteKeyGroup $
--             newDeleteKeyGroupResponse
--
--         , responseListOriginRequestPolicies $
--             newListOriginRequestPoliciesResponse
--
--         , responseGetOriginRequestPolicyConfig $
--             newGetOriginRequestPolicyConfigResponse
--
--           ]
--     ]

-- Requests

requestDeleteOriginRequestPolicy :: DeleteOriginRequestPolicy -> TestTree
requestDeleteOriginRequestPolicy =
  req
    "DeleteOriginRequestPolicy"
    "fixture/DeleteOriginRequestPolicy.yaml"

requestUpdateOriginRequestPolicy :: UpdateOriginRequestPolicy -> TestTree
requestUpdateOriginRequestPolicy =
  req
    "UpdateOriginRequestPolicy"
    "fixture/UpdateOriginRequestPolicy.yaml"

requestListConflictingAliases :: ListConflictingAliases -> TestTree
requestListConflictingAliases =
  req
    "ListConflictingAliases"
    "fixture/ListConflictingAliases.yaml"

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

requestListPublicKeys :: ListPublicKeys -> TestTree
requestListPublicKeys =
  req
    "ListPublicKeys"
    "fixture/ListPublicKeys.yaml"

requestGetFieldLevelEncryptionConfig :: GetFieldLevelEncryptionConfig -> TestTree
requestGetFieldLevelEncryptionConfig =
  req
    "GetFieldLevelEncryptionConfig"
    "fixture/GetFieldLevelEncryptionConfig.yaml"

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

requestGetPublicKeyConfig :: GetPublicKeyConfig -> TestTree
requestGetPublicKeyConfig =
  req
    "GetPublicKeyConfig"
    "fixture/GetPublicKeyConfig.yaml"

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

requestDeleteCachePolicy :: DeleteCachePolicy -> TestTree
requestDeleteCachePolicy =
  req
    "DeleteCachePolicy"
    "fixture/DeleteCachePolicy.yaml"

requestUpdateCachePolicy :: UpdateCachePolicy -> TestTree
requestUpdateCachePolicy =
  req
    "UpdateCachePolicy"
    "fixture/UpdateCachePolicy.yaml"

requestGetFieldLevelEncryption :: GetFieldLevelEncryption -> TestTree
requestGetFieldLevelEncryption =
  req
    "GetFieldLevelEncryption"
    "fixture/GetFieldLevelEncryption.yaml"

requestListRealtimeLogConfigs :: ListRealtimeLogConfigs -> TestTree
requestListRealtimeLogConfigs =
  req
    "ListRealtimeLogConfigs"
    "fixture/ListRealtimeLogConfigs.yaml"

requestGetPublicKey :: GetPublicKey -> TestTree
requestGetPublicKey =
  req
    "GetPublicKey"
    "fixture/GetPublicKey.yaml"

requestDeleteRealtimeLogConfig :: DeleteRealtimeLogConfig -> TestTree
requestDeleteRealtimeLogConfig =
  req
    "DeleteRealtimeLogConfig"
    "fixture/DeleteRealtimeLogConfig.yaml"

requestUpdateRealtimeLogConfig :: UpdateRealtimeLogConfig -> TestTree
requestUpdateRealtimeLogConfig =
  req
    "UpdateRealtimeLogConfig"
    "fixture/UpdateRealtimeLogConfig.yaml"

requestListDistributionsByOriginRequestPolicyId :: ListDistributionsByOriginRequestPolicyId -> TestTree
requestListDistributionsByOriginRequestPolicyId =
  req
    "ListDistributionsByOriginRequestPolicyId"
    "fixture/ListDistributionsByOriginRequestPolicyId.yaml"

requestDeleteFieldLevelEncryptionConfig :: DeleteFieldLevelEncryptionConfig -> TestTree
requestDeleteFieldLevelEncryptionConfig =
  req
    "DeleteFieldLevelEncryptionConfig"
    "fixture/DeleteFieldLevelEncryptionConfig.yaml"

requestUpdateFieldLevelEncryptionConfig :: UpdateFieldLevelEncryptionConfig -> TestTree
requestUpdateFieldLevelEncryptionConfig =
  req
    "UpdateFieldLevelEncryptionConfig"
    "fixture/UpdateFieldLevelEncryptionConfig.yaml"

requestGetKeyGroup :: GetKeyGroup -> TestTree
requestGetKeyGroup =
  req
    "GetKeyGroup"
    "fixture/GetKeyGroup.yaml"

requestCreateDistribution :: CreateDistribution -> TestTree
requestCreateDistribution =
  req
    "CreateDistribution"
    "fixture/CreateDistribution.yaml"

requestGetFieldLevelEncryptionProfile :: GetFieldLevelEncryptionProfile -> TestTree
requestGetFieldLevelEncryptionProfile =
  req
    "GetFieldLevelEncryptionProfile"
    "fixture/GetFieldLevelEncryptionProfile.yaml"

requestDeleteMonitoringSubscription :: DeleteMonitoringSubscription -> TestTree
requestDeleteMonitoringSubscription =
  req
    "DeleteMonitoringSubscription"
    "fixture/DeleteMonitoringSubscription.yaml"

requestCreateFunction :: CreateFunction -> TestTree
requestCreateFunction =
  req
    "CreateFunction"
    "fixture/CreateFunction.yaml"

requestGetDistributionConfig :: GetDistributionConfig -> TestTree
requestGetDistributionConfig =
  req
    "GetDistributionConfig"
    "fixture/GetDistributionConfig.yaml"

requestCreateStreamingDistributionWithTags :: CreateStreamingDistributionWithTags -> TestTree
requestCreateStreamingDistributionWithTags =
  req
    "CreateStreamingDistributionWithTags"
    "fixture/CreateStreamingDistributionWithTags.yaml"

requestDeleteFieldLevelEncryptionProfile :: DeleteFieldLevelEncryptionProfile -> TestTree
requestDeleteFieldLevelEncryptionProfile =
  req
    "DeleteFieldLevelEncryptionProfile"
    "fixture/DeleteFieldLevelEncryptionProfile.yaml"

requestUpdateFieldLevelEncryptionProfile :: UpdateFieldLevelEncryptionProfile -> TestTree
requestUpdateFieldLevelEncryptionProfile =
  req
    "UpdateFieldLevelEncryptionProfile"
    "fixture/UpdateFieldLevelEncryptionProfile.yaml"

requestListDistributionsByCachePolicyId :: ListDistributionsByCachePolicyId -> TestTree
requestListDistributionsByCachePolicyId =
  req
    "ListDistributionsByCachePolicyId"
    "fixture/ListDistributionsByCachePolicyId.yaml"

requestTestFunction :: TestFunction -> TestTree
requestTestFunction =
  req
    "TestFunction"
    "fixture/TestFunction.yaml"

requestCreateFieldLevelEncryptionProfile :: CreateFieldLevelEncryptionProfile -> TestTree
requestCreateFieldLevelEncryptionProfile =
  req
    "CreateFieldLevelEncryptionProfile"
    "fixture/CreateFieldLevelEncryptionProfile.yaml"

requestGetKeyGroupConfig :: GetKeyGroupConfig -> TestTree
requestGetKeyGroupConfig =
  req
    "GetKeyGroupConfig"
    "fixture/GetKeyGroupConfig.yaml"

requestGetDistribution :: GetDistribution -> TestTree
requestGetDistribution =
  req
    "GetDistribution"
    "fixture/GetDistribution.yaml"

requestGetFieldLevelEncryptionProfileConfig :: GetFieldLevelEncryptionProfileConfig -> TestTree
requestGetFieldLevelEncryptionProfileConfig =
  req
    "GetFieldLevelEncryptionProfileConfig"
    "fixture/GetFieldLevelEncryptionProfileConfig.yaml"

requestGetFunction :: GetFunction -> TestTree
requestGetFunction =
  req
    "GetFunction"
    "fixture/GetFunction.yaml"

requestCreateKeyGroup :: CreateKeyGroup -> TestTree
requestCreateKeyGroup =
  req
    "CreateKeyGroup"
    "fixture/CreateKeyGroup.yaml"

requestUpdateCloudFrontOriginAccessIdentity :: UpdateCloudFrontOriginAccessIdentity -> TestTree
requestUpdateCloudFrontOriginAccessIdentity =
  req
    "UpdateCloudFrontOriginAccessIdentity"
    "fixture/UpdateCloudFrontOriginAccessIdentity.yaml"

requestDeleteCloudFrontOriginAccessIdentity :: DeleteCloudFrontOriginAccessIdentity -> TestTree
requestDeleteCloudFrontOriginAccessIdentity =
  req
    "DeleteCloudFrontOriginAccessIdentity"
    "fixture/DeleteCloudFrontOriginAccessIdentity.yaml"

requestListStreamingDistributions :: ListStreamingDistributions -> TestTree
requestListStreamingDistributions =
  req
    "ListStreamingDistributions"
    "fixture/ListStreamingDistributions.yaml"

requestDeletePublicKey :: DeletePublicKey -> TestTree
requestDeletePublicKey =
  req
    "DeletePublicKey"
    "fixture/DeletePublicKey.yaml"

requestUpdatePublicKey :: UpdatePublicKey -> TestTree
requestUpdatePublicKey =
  req
    "UpdatePublicKey"
    "fixture/UpdatePublicKey.yaml"

requestGetRealtimeLogConfig :: GetRealtimeLogConfig -> TestTree
requestGetRealtimeLogConfig =
  req
    "GetRealtimeLogConfig"
    "fixture/GetRealtimeLogConfig.yaml"

requestDescribeFunction :: DescribeFunction -> TestTree
requestDescribeFunction =
  req
    "DescribeFunction"
    "fixture/DescribeFunction.yaml"

requestGetStreamingDistributionConfig :: GetStreamingDistributionConfig -> TestTree
requestGetStreamingDistributionConfig =
  req
    "GetStreamingDistributionConfig"
    "fixture/GetStreamingDistributionConfig.yaml"

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

requestGetInvalidation :: GetInvalidation -> TestTree
requestGetInvalidation =
  req
    "GetInvalidation"
    "fixture/GetInvalidation.yaml"

requestGetCachePolicy :: GetCachePolicy -> TestTree
requestGetCachePolicy =
  req
    "GetCachePolicy"
    "fixture/GetCachePolicy.yaml"

requestAssociateAlias :: AssociateAlias -> TestTree
requestAssociateAlias =
  req
    "AssociateAlias"
    "fixture/AssociateAlias.yaml"

requestCreateRealtimeLogConfig :: CreateRealtimeLogConfig -> TestTree
requestCreateRealtimeLogConfig =
  req
    "CreateRealtimeLogConfig"
    "fixture/CreateRealtimeLogConfig.yaml"

requestListInvalidations :: ListInvalidations -> TestTree
requestListInvalidations =
  req
    "ListInvalidations"
    "fixture/ListInvalidations.yaml"

requestCreateInvalidation :: CreateInvalidation -> TestTree
requestCreateInvalidation =
  req
    "CreateInvalidation"
    "fixture/CreateInvalidation.yaml"

requestGetCloudFrontOriginAccessIdentity :: GetCloudFrontOriginAccessIdentity -> TestTree
requestGetCloudFrontOriginAccessIdentity =
  req
    "GetCloudFrontOriginAccessIdentity"
    "fixture/GetCloudFrontOriginAccessIdentity.yaml"

requestListCachePolicies :: ListCachePolicies -> TestTree
requestListCachePolicies =
  req
    "ListCachePolicies"
    "fixture/ListCachePolicies.yaml"

requestCreateCachePolicy :: CreateCachePolicy -> TestTree
requestCreateCachePolicy =
  req
    "CreateCachePolicy"
    "fixture/CreateCachePolicy.yaml"

requestGetCachePolicyConfig :: GetCachePolicyConfig -> TestTree
requestGetCachePolicyConfig =
  req
    "GetCachePolicyConfig"
    "fixture/GetCachePolicyConfig.yaml"

requestListFieldLevelEncryptionConfigs :: ListFieldLevelEncryptionConfigs -> TestTree
requestListFieldLevelEncryptionConfigs =
  req
    "ListFieldLevelEncryptionConfigs"
    "fixture/ListFieldLevelEncryptionConfigs.yaml"

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

requestGetStreamingDistribution :: GetStreamingDistribution -> TestTree
requestGetStreamingDistribution =
  req
    "GetStreamingDistribution"
    "fixture/GetStreamingDistribution.yaml"

requestUpdateDistribution :: UpdateDistribution -> TestTree
requestUpdateDistribution =
  req
    "UpdateDistribution"
    "fixture/UpdateDistribution.yaml"

requestUpdateFunction :: UpdateFunction -> TestTree
requestUpdateFunction =
  req
    "UpdateFunction"
    "fixture/UpdateFunction.yaml"

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

requestGetOriginRequestPolicy :: GetOriginRequestPolicy -> TestTree
requestGetOriginRequestPolicy =
  req
    "GetOriginRequestPolicy"
    "fixture/GetOriginRequestPolicy.yaml"

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

requestCreateMonitoringSubscription :: CreateMonitoringSubscription -> TestTree
requestCreateMonitoringSubscription =
  req
    "CreateMonitoringSubscription"
    "fixture/CreateMonitoringSubscription.yaml"

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

requestListFunctions :: ListFunctions -> TestTree
requestListFunctions =
  req
    "ListFunctions"
    "fixture/ListFunctions.yaml"

requestListDistributionsByRealtimeLogConfig :: ListDistributionsByRealtimeLogConfig -> TestTree
requestListDistributionsByRealtimeLogConfig =
  req
    "ListDistributionsByRealtimeLogConfig"
    "fixture/ListDistributionsByRealtimeLogConfig.yaml"

requestCreateOriginRequestPolicy :: CreateOriginRequestPolicy -> TestTree
requestCreateOriginRequestPolicy =
  req
    "CreateOriginRequestPolicy"
    "fixture/CreateOriginRequestPolicy.yaml"

requestListKeyGroups :: ListKeyGroups -> TestTree
requestListKeyGroups =
  req
    "ListKeyGroups"
    "fixture/ListKeyGroups.yaml"

requestListFieldLevelEncryptionProfiles :: ListFieldLevelEncryptionProfiles -> TestTree
requestListFieldLevelEncryptionProfiles =
  req
    "ListFieldLevelEncryptionProfiles"
    "fixture/ListFieldLevelEncryptionProfiles.yaml"

requestGetMonitoringSubscription :: GetMonitoringSubscription -> TestTree
requestGetMonitoringSubscription =
  req
    "GetMonitoringSubscription"
    "fixture/GetMonitoringSubscription.yaml"

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

requestListOriginRequestPolicies :: ListOriginRequestPolicies -> TestTree
requestListOriginRequestPolicies =
  req
    "ListOriginRequestPolicies"
    "fixture/ListOriginRequestPolicies.yaml"

requestGetOriginRequestPolicyConfig :: GetOriginRequestPolicyConfig -> TestTree
requestGetOriginRequestPolicyConfig =
  req
    "GetOriginRequestPolicyConfig"
    "fixture/GetOriginRequestPolicyConfig.yaml"

-- Responses

responseDeleteOriginRequestPolicy :: DeleteOriginRequestPolicyResponse -> TestTree
responseDeleteOriginRequestPolicy =
  res
    "DeleteOriginRequestPolicyResponse"
    "fixture/DeleteOriginRequestPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteOriginRequestPolicy)

responseUpdateOriginRequestPolicy :: UpdateOriginRequestPolicyResponse -> TestTree
responseUpdateOriginRequestPolicy =
  res
    "UpdateOriginRequestPolicyResponse"
    "fixture/UpdateOriginRequestPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateOriginRequestPolicy)

responseListConflictingAliases :: ListConflictingAliasesResponse -> TestTree
responseListConflictingAliases =
  res
    "ListConflictingAliasesResponse"
    "fixture/ListConflictingAliasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListConflictingAliases)

responseDeleteStreamingDistribution :: DeleteStreamingDistributionResponse -> TestTree
responseDeleteStreamingDistribution =
  res
    "DeleteStreamingDistributionResponse"
    "fixture/DeleteStreamingDistributionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteStreamingDistribution)

responseUpdateStreamingDistribution :: UpdateStreamingDistributionResponse -> TestTree
responseUpdateStreamingDistribution =
  res
    "UpdateStreamingDistributionResponse"
    "fixture/UpdateStreamingDistributionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateStreamingDistribution)

responseListPublicKeys :: ListPublicKeysResponse -> TestTree
responseListPublicKeys =
  res
    "ListPublicKeysResponse"
    "fixture/ListPublicKeysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPublicKeys)

responseGetFieldLevelEncryptionConfig :: GetFieldLevelEncryptionConfigResponse -> TestTree
responseGetFieldLevelEncryptionConfig =
  res
    "GetFieldLevelEncryptionConfigResponse"
    "fixture/GetFieldLevelEncryptionConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFieldLevelEncryptionConfig)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseCreatePublicKey :: CreatePublicKeyResponse -> TestTree
responseCreatePublicKey =
  res
    "CreatePublicKeyResponse"
    "fixture/CreatePublicKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePublicKey)

responseGetPublicKeyConfig :: GetPublicKeyConfigResponse -> TestTree
responseGetPublicKeyConfig =
  res
    "GetPublicKeyConfigResponse"
    "fixture/GetPublicKeyConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPublicKeyConfig)

responseCreateDistributionWithTags :: CreateDistributionWithTagsResponse -> TestTree
responseCreateDistributionWithTags =
  res
    "CreateDistributionWithTagsResponse"
    "fixture/CreateDistributionWithTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDistributionWithTags)

responseCreateFieldLevelEncryptionConfig :: CreateFieldLevelEncryptionConfigResponse -> TestTree
responseCreateFieldLevelEncryptionConfig =
  res
    "CreateFieldLevelEncryptionConfigResponse"
    "fixture/CreateFieldLevelEncryptionConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFieldLevelEncryptionConfig)

responseDeleteCachePolicy :: DeleteCachePolicyResponse -> TestTree
responseDeleteCachePolicy =
  res
    "DeleteCachePolicyResponse"
    "fixture/DeleteCachePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCachePolicy)

responseUpdateCachePolicy :: UpdateCachePolicyResponse -> TestTree
responseUpdateCachePolicy =
  res
    "UpdateCachePolicyResponse"
    "fixture/UpdateCachePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCachePolicy)

responseGetFieldLevelEncryption :: GetFieldLevelEncryptionResponse -> TestTree
responseGetFieldLevelEncryption =
  res
    "GetFieldLevelEncryptionResponse"
    "fixture/GetFieldLevelEncryptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFieldLevelEncryption)

responseListRealtimeLogConfigs :: ListRealtimeLogConfigsResponse -> TestTree
responseListRealtimeLogConfigs =
  res
    "ListRealtimeLogConfigsResponse"
    "fixture/ListRealtimeLogConfigsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRealtimeLogConfigs)

responseGetPublicKey :: GetPublicKeyResponse -> TestTree
responseGetPublicKey =
  res
    "GetPublicKeyResponse"
    "fixture/GetPublicKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPublicKey)

responseDeleteRealtimeLogConfig :: DeleteRealtimeLogConfigResponse -> TestTree
responseDeleteRealtimeLogConfig =
  res
    "DeleteRealtimeLogConfigResponse"
    "fixture/DeleteRealtimeLogConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRealtimeLogConfig)

responseUpdateRealtimeLogConfig :: UpdateRealtimeLogConfigResponse -> TestTree
responseUpdateRealtimeLogConfig =
  res
    "UpdateRealtimeLogConfigResponse"
    "fixture/UpdateRealtimeLogConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRealtimeLogConfig)

responseListDistributionsByOriginRequestPolicyId :: ListDistributionsByOriginRequestPolicyIdResponse -> TestTree
responseListDistributionsByOriginRequestPolicyId =
  res
    "ListDistributionsByOriginRequestPolicyIdResponse"
    "fixture/ListDistributionsByOriginRequestPolicyIdResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDistributionsByOriginRequestPolicyId)

responseDeleteFieldLevelEncryptionConfig :: DeleteFieldLevelEncryptionConfigResponse -> TestTree
responseDeleteFieldLevelEncryptionConfig =
  res
    "DeleteFieldLevelEncryptionConfigResponse"
    "fixture/DeleteFieldLevelEncryptionConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFieldLevelEncryptionConfig)

responseUpdateFieldLevelEncryptionConfig :: UpdateFieldLevelEncryptionConfigResponse -> TestTree
responseUpdateFieldLevelEncryptionConfig =
  res
    "UpdateFieldLevelEncryptionConfigResponse"
    "fixture/UpdateFieldLevelEncryptionConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFieldLevelEncryptionConfig)

responseGetKeyGroup :: GetKeyGroupResponse -> TestTree
responseGetKeyGroup =
  res
    "GetKeyGroupResponse"
    "fixture/GetKeyGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetKeyGroup)

responseCreateDistribution :: CreateDistributionResponse -> TestTree
responseCreateDistribution =
  res
    "CreateDistributionResponse"
    "fixture/CreateDistributionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDistribution)

responseGetFieldLevelEncryptionProfile :: GetFieldLevelEncryptionProfileResponse -> TestTree
responseGetFieldLevelEncryptionProfile =
  res
    "GetFieldLevelEncryptionProfileResponse"
    "fixture/GetFieldLevelEncryptionProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFieldLevelEncryptionProfile)

responseDeleteMonitoringSubscription :: DeleteMonitoringSubscriptionResponse -> TestTree
responseDeleteMonitoringSubscription =
  res
    "DeleteMonitoringSubscriptionResponse"
    "fixture/DeleteMonitoringSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMonitoringSubscription)

responseCreateFunction :: CreateFunctionResponse -> TestTree
responseCreateFunction =
  res
    "CreateFunctionResponse"
    "fixture/CreateFunctionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFunction)

responseGetDistributionConfig :: GetDistributionConfigResponse -> TestTree
responseGetDistributionConfig =
  res
    "GetDistributionConfigResponse"
    "fixture/GetDistributionConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDistributionConfig)

responseCreateStreamingDistributionWithTags :: CreateStreamingDistributionWithTagsResponse -> TestTree
responseCreateStreamingDistributionWithTags =
  res
    "CreateStreamingDistributionWithTagsResponse"
    "fixture/CreateStreamingDistributionWithTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStreamingDistributionWithTags)

responseDeleteFieldLevelEncryptionProfile :: DeleteFieldLevelEncryptionProfileResponse -> TestTree
responseDeleteFieldLevelEncryptionProfile =
  res
    "DeleteFieldLevelEncryptionProfileResponse"
    "fixture/DeleteFieldLevelEncryptionProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFieldLevelEncryptionProfile)

responseUpdateFieldLevelEncryptionProfile :: UpdateFieldLevelEncryptionProfileResponse -> TestTree
responseUpdateFieldLevelEncryptionProfile =
  res
    "UpdateFieldLevelEncryptionProfileResponse"
    "fixture/UpdateFieldLevelEncryptionProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFieldLevelEncryptionProfile)

responseListDistributionsByCachePolicyId :: ListDistributionsByCachePolicyIdResponse -> TestTree
responseListDistributionsByCachePolicyId =
  res
    "ListDistributionsByCachePolicyIdResponse"
    "fixture/ListDistributionsByCachePolicyIdResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDistributionsByCachePolicyId)

responseTestFunction :: TestFunctionResponse -> TestTree
responseTestFunction =
  res
    "TestFunctionResponse"
    "fixture/TestFunctionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TestFunction)

responseCreateFieldLevelEncryptionProfile :: CreateFieldLevelEncryptionProfileResponse -> TestTree
responseCreateFieldLevelEncryptionProfile =
  res
    "CreateFieldLevelEncryptionProfileResponse"
    "fixture/CreateFieldLevelEncryptionProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFieldLevelEncryptionProfile)

responseGetKeyGroupConfig :: GetKeyGroupConfigResponse -> TestTree
responseGetKeyGroupConfig =
  res
    "GetKeyGroupConfigResponse"
    "fixture/GetKeyGroupConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetKeyGroupConfig)

responseGetDistribution :: GetDistributionResponse -> TestTree
responseGetDistribution =
  res
    "GetDistributionResponse"
    "fixture/GetDistributionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDistribution)

responseGetFieldLevelEncryptionProfileConfig :: GetFieldLevelEncryptionProfileConfigResponse -> TestTree
responseGetFieldLevelEncryptionProfileConfig =
  res
    "GetFieldLevelEncryptionProfileConfigResponse"
    "fixture/GetFieldLevelEncryptionProfileConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFieldLevelEncryptionProfileConfig)

responseGetFunction :: GetFunctionResponse -> TestTree
responseGetFunction =
  res
    "GetFunctionResponse"
    "fixture/GetFunctionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFunction)

responseCreateKeyGroup :: CreateKeyGroupResponse -> TestTree
responseCreateKeyGroup =
  res
    "CreateKeyGroupResponse"
    "fixture/CreateKeyGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateKeyGroup)

responseUpdateCloudFrontOriginAccessIdentity :: UpdateCloudFrontOriginAccessIdentityResponse -> TestTree
responseUpdateCloudFrontOriginAccessIdentity =
  res
    "UpdateCloudFrontOriginAccessIdentityResponse"
    "fixture/UpdateCloudFrontOriginAccessIdentityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCloudFrontOriginAccessIdentity)

responseDeleteCloudFrontOriginAccessIdentity :: DeleteCloudFrontOriginAccessIdentityResponse -> TestTree
responseDeleteCloudFrontOriginAccessIdentity =
  res
    "DeleteCloudFrontOriginAccessIdentityResponse"
    "fixture/DeleteCloudFrontOriginAccessIdentityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCloudFrontOriginAccessIdentity)

responseListStreamingDistributions :: ListStreamingDistributionsResponse -> TestTree
responseListStreamingDistributions =
  res
    "ListStreamingDistributionsResponse"
    "fixture/ListStreamingDistributionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStreamingDistributions)

responseDeletePublicKey :: DeletePublicKeyResponse -> TestTree
responseDeletePublicKey =
  res
    "DeletePublicKeyResponse"
    "fixture/DeletePublicKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePublicKey)

responseUpdatePublicKey :: UpdatePublicKeyResponse -> TestTree
responseUpdatePublicKey =
  res
    "UpdatePublicKeyResponse"
    "fixture/UpdatePublicKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePublicKey)

responseGetRealtimeLogConfig :: GetRealtimeLogConfigResponse -> TestTree
responseGetRealtimeLogConfig =
  res
    "GetRealtimeLogConfigResponse"
    "fixture/GetRealtimeLogConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRealtimeLogConfig)

responseDescribeFunction :: DescribeFunctionResponse -> TestTree
responseDescribeFunction =
  res
    "DescribeFunctionResponse"
    "fixture/DescribeFunctionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFunction)

responseGetStreamingDistributionConfig :: GetStreamingDistributionConfigResponse -> TestTree
responseGetStreamingDistributionConfig =
  res
    "GetStreamingDistributionConfigResponse"
    "fixture/GetStreamingDistributionConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetStreamingDistributionConfig)

responseGetCloudFrontOriginAccessIdentityConfig :: GetCloudFrontOriginAccessIdentityConfigResponse -> TestTree
responseGetCloudFrontOriginAccessIdentityConfig =
  res
    "GetCloudFrontOriginAccessIdentityConfigResponse"
    "fixture/GetCloudFrontOriginAccessIdentityConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCloudFrontOriginAccessIdentityConfig)

responseCreateStreamingDistribution :: CreateStreamingDistributionResponse -> TestTree
responseCreateStreamingDistribution =
  res
    "CreateStreamingDistributionResponse"
    "fixture/CreateStreamingDistributionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStreamingDistribution)

responseCreateCloudFrontOriginAccessIdentity :: CreateCloudFrontOriginAccessIdentityResponse -> TestTree
responseCreateCloudFrontOriginAccessIdentity =
  res
    "CreateCloudFrontOriginAccessIdentityResponse"
    "fixture/CreateCloudFrontOriginAccessIdentityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCloudFrontOriginAccessIdentity)

responseListCloudFrontOriginAccessIdentities :: ListCloudFrontOriginAccessIdentitiesResponse -> TestTree
responseListCloudFrontOriginAccessIdentities =
  res
    "ListCloudFrontOriginAccessIdentitiesResponse"
    "fixture/ListCloudFrontOriginAccessIdentitiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCloudFrontOriginAccessIdentities)

responseGetInvalidation :: GetInvalidationResponse -> TestTree
responseGetInvalidation =
  res
    "GetInvalidationResponse"
    "fixture/GetInvalidationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInvalidation)

responseGetCachePolicy :: GetCachePolicyResponse -> TestTree
responseGetCachePolicy =
  res
    "GetCachePolicyResponse"
    "fixture/GetCachePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCachePolicy)

responseAssociateAlias :: AssociateAliasResponse -> TestTree
responseAssociateAlias =
  res
    "AssociateAliasResponse"
    "fixture/AssociateAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateAlias)

responseCreateRealtimeLogConfig :: CreateRealtimeLogConfigResponse -> TestTree
responseCreateRealtimeLogConfig =
  res
    "CreateRealtimeLogConfigResponse"
    "fixture/CreateRealtimeLogConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRealtimeLogConfig)

responseListInvalidations :: ListInvalidationsResponse -> TestTree
responseListInvalidations =
  res
    "ListInvalidationsResponse"
    "fixture/ListInvalidationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInvalidations)

responseCreateInvalidation :: CreateInvalidationResponse -> TestTree
responseCreateInvalidation =
  res
    "CreateInvalidationResponse"
    "fixture/CreateInvalidationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateInvalidation)

responseGetCloudFrontOriginAccessIdentity :: GetCloudFrontOriginAccessIdentityResponse -> TestTree
responseGetCloudFrontOriginAccessIdentity =
  res
    "GetCloudFrontOriginAccessIdentityResponse"
    "fixture/GetCloudFrontOriginAccessIdentityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCloudFrontOriginAccessIdentity)

responseListCachePolicies :: ListCachePoliciesResponse -> TestTree
responseListCachePolicies =
  res
    "ListCachePoliciesResponse"
    "fixture/ListCachePoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCachePolicies)

responseCreateCachePolicy :: CreateCachePolicyResponse -> TestTree
responseCreateCachePolicy =
  res
    "CreateCachePolicyResponse"
    "fixture/CreateCachePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCachePolicy)

responseGetCachePolicyConfig :: GetCachePolicyConfigResponse -> TestTree
responseGetCachePolicyConfig =
  res
    "GetCachePolicyConfigResponse"
    "fixture/GetCachePolicyConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCachePolicyConfig)

responseListFieldLevelEncryptionConfigs :: ListFieldLevelEncryptionConfigsResponse -> TestTree
responseListFieldLevelEncryptionConfigs =
  res
    "ListFieldLevelEncryptionConfigsResponse"
    "fixture/ListFieldLevelEncryptionConfigsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFieldLevelEncryptionConfigs)

responseListDistributionsByKeyGroup :: ListDistributionsByKeyGroupResponse -> TestTree
responseListDistributionsByKeyGroup =
  res
    "ListDistributionsByKeyGroupResponse"
    "fixture/ListDistributionsByKeyGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDistributionsByKeyGroup)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseGetStreamingDistribution :: GetStreamingDistributionResponse -> TestTree
responseGetStreamingDistribution =
  res
    "GetStreamingDistributionResponse"
    "fixture/GetStreamingDistributionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetStreamingDistribution)

responseUpdateDistribution :: UpdateDistributionResponse -> TestTree
responseUpdateDistribution =
  res
    "UpdateDistributionResponse"
    "fixture/UpdateDistributionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDistribution)

responseUpdateFunction :: UpdateFunctionResponse -> TestTree
responseUpdateFunction =
  res
    "UpdateFunctionResponse"
    "fixture/UpdateFunctionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFunction)

responseDeleteDistribution :: DeleteDistributionResponse -> TestTree
responseDeleteDistribution =
  res
    "DeleteDistributionResponse"
    "fixture/DeleteDistributionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDistribution)

responseDeleteFunction :: DeleteFunctionResponse -> TestTree
responseDeleteFunction =
  res
    "DeleteFunctionResponse"
    "fixture/DeleteFunctionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFunction)

responseGetOriginRequestPolicy :: GetOriginRequestPolicyResponse -> TestTree
responseGetOriginRequestPolicy =
  res
    "GetOriginRequestPolicyResponse"
    "fixture/GetOriginRequestPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetOriginRequestPolicy)

responsePublishFunction :: PublishFunctionResponse -> TestTree
responsePublishFunction =
  res
    "PublishFunctionResponse"
    "fixture/PublishFunctionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PublishFunction)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseCreateMonitoringSubscription :: CreateMonitoringSubscriptionResponse -> TestTree
responseCreateMonitoringSubscription =
  res
    "CreateMonitoringSubscriptionResponse"
    "fixture/CreateMonitoringSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMonitoringSubscription)

responseListDistributionsByWebACLId :: ListDistributionsByWebACLIdResponse -> TestTree
responseListDistributionsByWebACLId =
  res
    "ListDistributionsByWebACLIdResponse"
    "fixture/ListDistributionsByWebACLIdResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDistributionsByWebACLId)

responseListDistributions :: ListDistributionsResponse -> TestTree
responseListDistributions =
  res
    "ListDistributionsResponse"
    "fixture/ListDistributionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDistributions)

responseListFunctions :: ListFunctionsResponse -> TestTree
responseListFunctions =
  res
    "ListFunctionsResponse"
    "fixture/ListFunctionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFunctions)

responseListDistributionsByRealtimeLogConfig :: ListDistributionsByRealtimeLogConfigResponse -> TestTree
responseListDistributionsByRealtimeLogConfig =
  res
    "ListDistributionsByRealtimeLogConfigResponse"
    "fixture/ListDistributionsByRealtimeLogConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDistributionsByRealtimeLogConfig)

responseCreateOriginRequestPolicy :: CreateOriginRequestPolicyResponse -> TestTree
responseCreateOriginRequestPolicy =
  res
    "CreateOriginRequestPolicyResponse"
    "fixture/CreateOriginRequestPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateOriginRequestPolicy)

responseListKeyGroups :: ListKeyGroupsResponse -> TestTree
responseListKeyGroups =
  res
    "ListKeyGroupsResponse"
    "fixture/ListKeyGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListKeyGroups)

responseListFieldLevelEncryptionProfiles :: ListFieldLevelEncryptionProfilesResponse -> TestTree
responseListFieldLevelEncryptionProfiles =
  res
    "ListFieldLevelEncryptionProfilesResponse"
    "fixture/ListFieldLevelEncryptionProfilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFieldLevelEncryptionProfiles)

responseGetMonitoringSubscription :: GetMonitoringSubscriptionResponse -> TestTree
responseGetMonitoringSubscription =
  res
    "GetMonitoringSubscriptionResponse"
    "fixture/GetMonitoringSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMonitoringSubscription)

responseUpdateKeyGroup :: UpdateKeyGroupResponse -> TestTree
responseUpdateKeyGroup =
  res
    "UpdateKeyGroupResponse"
    "fixture/UpdateKeyGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateKeyGroup)

responseDeleteKeyGroup :: DeleteKeyGroupResponse -> TestTree
responseDeleteKeyGroup =
  res
    "DeleteKeyGroupResponse"
    "fixture/DeleteKeyGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteKeyGroup)

responseListOriginRequestPolicies :: ListOriginRequestPoliciesResponse -> TestTree
responseListOriginRequestPolicies =
  res
    "ListOriginRequestPoliciesResponse"
    "fixture/ListOriginRequestPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOriginRequestPolicies)

responseGetOriginRequestPolicyConfig :: GetOriginRequestPolicyConfigResponse -> TestTree
responseGetOriginRequestPolicyConfig =
  res
    "GetOriginRequestPolicyConfigResponse"
    "fixture/GetOriginRequestPolicyConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetOriginRequestPolicyConfig)
