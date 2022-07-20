{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.CloudFront
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.CloudFront where

import Amazonka.CloudFront
import qualified Data.Proxy as Proxy
import Test.Amazonka.CloudFront.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAssociateAlias $
--             newAssociateAlias
--
--         , requestCreateCachePolicy $
--             newCreateCachePolicy
--
--         , requestCreateCloudFrontOriginAccessIdentity $
--             newCreateCloudFrontOriginAccessIdentity
--
--         , requestCreateDistribution $
--             newCreateDistribution
--
--         , requestCreateDistributionWithTags $
--             newCreateDistributionWithTags
--
--         , requestCreateFieldLevelEncryptionConfig $
--             newCreateFieldLevelEncryptionConfig
--
--         , requestCreateFieldLevelEncryptionProfile $
--             newCreateFieldLevelEncryptionProfile
--
--         , requestCreateFunction $
--             newCreateFunction
--
--         , requestCreateInvalidation $
--             newCreateInvalidation
--
--         , requestCreateKeyGroup $
--             newCreateKeyGroup
--
--         , requestCreateMonitoringSubscription $
--             newCreateMonitoringSubscription
--
--         , requestCreateOriginRequestPolicy $
--             newCreateOriginRequestPolicy
--
--         , requestCreatePublicKey $
--             newCreatePublicKey
--
--         , requestCreateRealtimeLogConfig $
--             newCreateRealtimeLogConfig
--
--         , requestCreateStreamingDistribution $
--             newCreateStreamingDistribution
--
--         , requestCreateStreamingDistributionWithTags $
--             newCreateStreamingDistributionWithTags
--
--         , requestDeleteCachePolicy $
--             newDeleteCachePolicy
--
--         , requestDeleteCloudFrontOriginAccessIdentity $
--             newDeleteCloudFrontOriginAccessIdentity
--
--         , requestDeleteDistribution $
--             newDeleteDistribution
--
--         , requestDeleteFieldLevelEncryptionConfig $
--             newDeleteFieldLevelEncryptionConfig
--
--         , requestDeleteFieldLevelEncryptionProfile $
--             newDeleteFieldLevelEncryptionProfile
--
--         , requestDeleteFunction $
--             newDeleteFunction
--
--         , requestDeleteKeyGroup $
--             newDeleteKeyGroup
--
--         , requestDeleteMonitoringSubscription $
--             newDeleteMonitoringSubscription
--
--         , requestDeleteOriginRequestPolicy $
--             newDeleteOriginRequestPolicy
--
--         , requestDeletePublicKey $
--             newDeletePublicKey
--
--         , requestDeleteRealtimeLogConfig $
--             newDeleteRealtimeLogConfig
--
--         , requestDeleteStreamingDistribution $
--             newDeleteStreamingDistribution
--
--         , requestDescribeFunction $
--             newDescribeFunction
--
--         , requestGetCachePolicy $
--             newGetCachePolicy
--
--         , requestGetCachePolicyConfig $
--             newGetCachePolicyConfig
--
--         , requestGetCloudFrontOriginAccessIdentity $
--             newGetCloudFrontOriginAccessIdentity
--
--         , requestGetCloudFrontOriginAccessIdentityConfig $
--             newGetCloudFrontOriginAccessIdentityConfig
--
--         , requestGetDistribution $
--             newGetDistribution
--
--         , requestGetDistributionConfig $
--             newGetDistributionConfig
--
--         , requestGetFieldLevelEncryption $
--             newGetFieldLevelEncryption
--
--         , requestGetFieldLevelEncryptionConfig $
--             newGetFieldLevelEncryptionConfig
--
--         , requestGetFieldLevelEncryptionProfile $
--             newGetFieldLevelEncryptionProfile
--
--         , requestGetFieldLevelEncryptionProfileConfig $
--             newGetFieldLevelEncryptionProfileConfig
--
--         , requestGetFunction $
--             newGetFunction
--
--         , requestGetInvalidation $
--             newGetInvalidation
--
--         , requestGetKeyGroup $
--             newGetKeyGroup
--
--         , requestGetKeyGroupConfig $
--             newGetKeyGroupConfig
--
--         , requestGetMonitoringSubscription $
--             newGetMonitoringSubscription
--
--         , requestGetOriginRequestPolicy $
--             newGetOriginRequestPolicy
--
--         , requestGetOriginRequestPolicyConfig $
--             newGetOriginRequestPolicyConfig
--
--         , requestGetPublicKey $
--             newGetPublicKey
--
--         , requestGetPublicKeyConfig $
--             newGetPublicKeyConfig
--
--         , requestGetRealtimeLogConfig $
--             newGetRealtimeLogConfig
--
--         , requestGetStreamingDistribution $
--             newGetStreamingDistribution
--
--         , requestGetStreamingDistributionConfig $
--             newGetStreamingDistributionConfig
--
--         , requestListCachePolicies $
--             newListCachePolicies
--
--         , requestListCloudFrontOriginAccessIdentities $
--             newListCloudFrontOriginAccessIdentities
--
--         , requestListConflictingAliases $
--             newListConflictingAliases
--
--         , requestListDistributions $
--             newListDistributions
--
--         , requestListDistributionsByCachePolicyId $
--             newListDistributionsByCachePolicyId
--
--         , requestListDistributionsByKeyGroup $
--             newListDistributionsByKeyGroup
--
--         , requestListDistributionsByOriginRequestPolicyId $
--             newListDistributionsByOriginRequestPolicyId
--
--         , requestListDistributionsByRealtimeLogConfig $
--             newListDistributionsByRealtimeLogConfig
--
--         , requestListDistributionsByWebACLId $
--             newListDistributionsByWebACLId
--
--         , requestListFieldLevelEncryptionConfigs $
--             newListFieldLevelEncryptionConfigs
--
--         , requestListFieldLevelEncryptionProfiles $
--             newListFieldLevelEncryptionProfiles
--
--         , requestListFunctions $
--             newListFunctions
--
--         , requestListInvalidations $
--             newListInvalidations
--
--         , requestListKeyGroups $
--             newListKeyGroups
--
--         , requestListOriginRequestPolicies $
--             newListOriginRequestPolicies
--
--         , requestListPublicKeys $
--             newListPublicKeys
--
--         , requestListRealtimeLogConfigs $
--             newListRealtimeLogConfigs
--
--         , requestListStreamingDistributions $
--             newListStreamingDistributions
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPublishFunction $
--             newPublishFunction
--
--         , requestTagResource $
--             newTagResource
--
--         , requestTestFunction $
--             newTestFunction
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateCachePolicy $
--             newUpdateCachePolicy
--
--         , requestUpdateCloudFrontOriginAccessIdentity $
--             newUpdateCloudFrontOriginAccessIdentity
--
--         , requestUpdateDistribution $
--             newUpdateDistribution
--
--         , requestUpdateFieldLevelEncryptionConfig $
--             newUpdateFieldLevelEncryptionConfig
--
--         , requestUpdateFieldLevelEncryptionProfile $
--             newUpdateFieldLevelEncryptionProfile
--
--         , requestUpdateFunction $
--             newUpdateFunction
--
--         , requestUpdateKeyGroup $
--             newUpdateKeyGroup
--
--         , requestUpdateOriginRequestPolicy $
--             newUpdateOriginRequestPolicy
--
--         , requestUpdatePublicKey $
--             newUpdatePublicKey
--
--         , requestUpdateRealtimeLogConfig $
--             newUpdateRealtimeLogConfig
--
--         , requestUpdateStreamingDistribution $
--             newUpdateStreamingDistribution
--
--           ]

--     , testGroup "response"
--         [ responseAssociateAlias $
--             newAssociateAliasResponse
--
--         , responseCreateCachePolicy $
--             newCreateCachePolicyResponse
--
--         , responseCreateCloudFrontOriginAccessIdentity $
--             newCreateCloudFrontOriginAccessIdentityResponse
--
--         , responseCreateDistribution $
--             newCreateDistributionResponse
--
--         , responseCreateDistributionWithTags $
--             newCreateDistributionWithTagsResponse
--
--         , responseCreateFieldLevelEncryptionConfig $
--             newCreateFieldLevelEncryptionConfigResponse
--
--         , responseCreateFieldLevelEncryptionProfile $
--             newCreateFieldLevelEncryptionProfileResponse
--
--         , responseCreateFunction $
--             newCreateFunctionResponse
--
--         , responseCreateInvalidation $
--             newCreateInvalidationResponse
--
--         , responseCreateKeyGroup $
--             newCreateKeyGroupResponse
--
--         , responseCreateMonitoringSubscription $
--             newCreateMonitoringSubscriptionResponse
--
--         , responseCreateOriginRequestPolicy $
--             newCreateOriginRequestPolicyResponse
--
--         , responseCreatePublicKey $
--             newCreatePublicKeyResponse
--
--         , responseCreateRealtimeLogConfig $
--             newCreateRealtimeLogConfigResponse
--
--         , responseCreateStreamingDistribution $
--             newCreateStreamingDistributionResponse
--
--         , responseCreateStreamingDistributionWithTags $
--             newCreateStreamingDistributionWithTagsResponse
--
--         , responseDeleteCachePolicy $
--             newDeleteCachePolicyResponse
--
--         , responseDeleteCloudFrontOriginAccessIdentity $
--             newDeleteCloudFrontOriginAccessIdentityResponse
--
--         , responseDeleteDistribution $
--             newDeleteDistributionResponse
--
--         , responseDeleteFieldLevelEncryptionConfig $
--             newDeleteFieldLevelEncryptionConfigResponse
--
--         , responseDeleteFieldLevelEncryptionProfile $
--             newDeleteFieldLevelEncryptionProfileResponse
--
--         , responseDeleteFunction $
--             newDeleteFunctionResponse
--
--         , responseDeleteKeyGroup $
--             newDeleteKeyGroupResponse
--
--         , responseDeleteMonitoringSubscription $
--             newDeleteMonitoringSubscriptionResponse
--
--         , responseDeleteOriginRequestPolicy $
--             newDeleteOriginRequestPolicyResponse
--
--         , responseDeletePublicKey $
--             newDeletePublicKeyResponse
--
--         , responseDeleteRealtimeLogConfig $
--             newDeleteRealtimeLogConfigResponse
--
--         , responseDeleteStreamingDistribution $
--             newDeleteStreamingDistributionResponse
--
--         , responseDescribeFunction $
--             newDescribeFunctionResponse
--
--         , responseGetCachePolicy $
--             newGetCachePolicyResponse
--
--         , responseGetCachePolicyConfig $
--             newGetCachePolicyConfigResponse
--
--         , responseGetCloudFrontOriginAccessIdentity $
--             newGetCloudFrontOriginAccessIdentityResponse
--
--         , responseGetCloudFrontOriginAccessIdentityConfig $
--             newGetCloudFrontOriginAccessIdentityConfigResponse
--
--         , responseGetDistribution $
--             newGetDistributionResponse
--
--         , responseGetDistributionConfig $
--             newGetDistributionConfigResponse
--
--         , responseGetFieldLevelEncryption $
--             newGetFieldLevelEncryptionResponse
--
--         , responseGetFieldLevelEncryptionConfig $
--             newGetFieldLevelEncryptionConfigResponse
--
--         , responseGetFieldLevelEncryptionProfile $
--             newGetFieldLevelEncryptionProfileResponse
--
--         , responseGetFieldLevelEncryptionProfileConfig $
--             newGetFieldLevelEncryptionProfileConfigResponse
--
--         , responseGetFunction $
--             newGetFunctionResponse
--
--         , responseGetInvalidation $
--             newGetInvalidationResponse
--
--         , responseGetKeyGroup $
--             newGetKeyGroupResponse
--
--         , responseGetKeyGroupConfig $
--             newGetKeyGroupConfigResponse
--
--         , responseGetMonitoringSubscription $
--             newGetMonitoringSubscriptionResponse
--
--         , responseGetOriginRequestPolicy $
--             newGetOriginRequestPolicyResponse
--
--         , responseGetOriginRequestPolicyConfig $
--             newGetOriginRequestPolicyConfigResponse
--
--         , responseGetPublicKey $
--             newGetPublicKeyResponse
--
--         , responseGetPublicKeyConfig $
--             newGetPublicKeyConfigResponse
--
--         , responseGetRealtimeLogConfig $
--             newGetRealtimeLogConfigResponse
--
--         , responseGetStreamingDistribution $
--             newGetStreamingDistributionResponse
--
--         , responseGetStreamingDistributionConfig $
--             newGetStreamingDistributionConfigResponse
--
--         , responseListCachePolicies $
--             newListCachePoliciesResponse
--
--         , responseListCloudFrontOriginAccessIdentities $
--             newListCloudFrontOriginAccessIdentitiesResponse
--
--         , responseListConflictingAliases $
--             newListConflictingAliasesResponse
--
--         , responseListDistributions $
--             newListDistributionsResponse
--
--         , responseListDistributionsByCachePolicyId $
--             newListDistributionsByCachePolicyIdResponse
--
--         , responseListDistributionsByKeyGroup $
--             newListDistributionsByKeyGroupResponse
--
--         , responseListDistributionsByOriginRequestPolicyId $
--             newListDistributionsByOriginRequestPolicyIdResponse
--
--         , responseListDistributionsByRealtimeLogConfig $
--             newListDistributionsByRealtimeLogConfigResponse
--
--         , responseListDistributionsByWebACLId $
--             newListDistributionsByWebACLIdResponse
--
--         , responseListFieldLevelEncryptionConfigs $
--             newListFieldLevelEncryptionConfigsResponse
--
--         , responseListFieldLevelEncryptionProfiles $
--             newListFieldLevelEncryptionProfilesResponse
--
--         , responseListFunctions $
--             newListFunctionsResponse
--
--         , responseListInvalidations $
--             newListInvalidationsResponse
--
--         , responseListKeyGroups $
--             newListKeyGroupsResponse
--
--         , responseListOriginRequestPolicies $
--             newListOriginRequestPoliciesResponse
--
--         , responseListPublicKeys $
--             newListPublicKeysResponse
--
--         , responseListRealtimeLogConfigs $
--             newListRealtimeLogConfigsResponse
--
--         , responseListStreamingDistributions $
--             newListStreamingDistributionsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePublishFunction $
--             newPublishFunctionResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseTestFunction $
--             newTestFunctionResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateCachePolicy $
--             newUpdateCachePolicyResponse
--
--         , responseUpdateCloudFrontOriginAccessIdentity $
--             newUpdateCloudFrontOriginAccessIdentityResponse
--
--         , responseUpdateDistribution $
--             newUpdateDistributionResponse
--
--         , responseUpdateFieldLevelEncryptionConfig $
--             newUpdateFieldLevelEncryptionConfigResponse
--
--         , responseUpdateFieldLevelEncryptionProfile $
--             newUpdateFieldLevelEncryptionProfileResponse
--
--         , responseUpdateFunction $
--             newUpdateFunctionResponse
--
--         , responseUpdateKeyGroup $
--             newUpdateKeyGroupResponse
--
--         , responseUpdateOriginRequestPolicy $
--             newUpdateOriginRequestPolicyResponse
--
--         , responseUpdatePublicKey $
--             newUpdatePublicKeyResponse
--
--         , responseUpdateRealtimeLogConfig $
--             newUpdateRealtimeLogConfigResponse
--
--         , responseUpdateStreamingDistribution $
--             newUpdateStreamingDistributionResponse
--
--           ]
--     ]

-- Requests

requestAssociateAlias :: AssociateAlias -> TestTree
requestAssociateAlias =
  req
    "AssociateAlias"
    "fixture/AssociateAlias.yaml"

requestCreateCachePolicy :: CreateCachePolicy -> TestTree
requestCreateCachePolicy =
  req
    "CreateCachePolicy"
    "fixture/CreateCachePolicy.yaml"

requestCreateCloudFrontOriginAccessIdentity :: CreateCloudFrontOriginAccessIdentity -> TestTree
requestCreateCloudFrontOriginAccessIdentity =
  req
    "CreateCloudFrontOriginAccessIdentity"
    "fixture/CreateCloudFrontOriginAccessIdentity.yaml"

requestCreateDistribution :: CreateDistribution -> TestTree
requestCreateDistribution =
  req
    "CreateDistribution"
    "fixture/CreateDistribution.yaml"

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

requestCreateFieldLevelEncryptionProfile :: CreateFieldLevelEncryptionProfile -> TestTree
requestCreateFieldLevelEncryptionProfile =
  req
    "CreateFieldLevelEncryptionProfile"
    "fixture/CreateFieldLevelEncryptionProfile.yaml"

requestCreateFunction :: CreateFunction -> TestTree
requestCreateFunction =
  req
    "CreateFunction"
    "fixture/CreateFunction.yaml"

requestCreateInvalidation :: CreateInvalidation -> TestTree
requestCreateInvalidation =
  req
    "CreateInvalidation"
    "fixture/CreateInvalidation.yaml"

requestCreateKeyGroup :: CreateKeyGroup -> TestTree
requestCreateKeyGroup =
  req
    "CreateKeyGroup"
    "fixture/CreateKeyGroup.yaml"

requestCreateMonitoringSubscription :: CreateMonitoringSubscription -> TestTree
requestCreateMonitoringSubscription =
  req
    "CreateMonitoringSubscription"
    "fixture/CreateMonitoringSubscription.yaml"

requestCreateOriginRequestPolicy :: CreateOriginRequestPolicy -> TestTree
requestCreateOriginRequestPolicy =
  req
    "CreateOriginRequestPolicy"
    "fixture/CreateOriginRequestPolicy.yaml"

requestCreatePublicKey :: CreatePublicKey -> TestTree
requestCreatePublicKey =
  req
    "CreatePublicKey"
    "fixture/CreatePublicKey.yaml"

requestCreateRealtimeLogConfig :: CreateRealtimeLogConfig -> TestTree
requestCreateRealtimeLogConfig =
  req
    "CreateRealtimeLogConfig"
    "fixture/CreateRealtimeLogConfig.yaml"

requestCreateStreamingDistribution :: CreateStreamingDistribution -> TestTree
requestCreateStreamingDistribution =
  req
    "CreateStreamingDistribution"
    "fixture/CreateStreamingDistribution.yaml"

requestCreateStreamingDistributionWithTags :: CreateStreamingDistributionWithTags -> TestTree
requestCreateStreamingDistributionWithTags =
  req
    "CreateStreamingDistributionWithTags"
    "fixture/CreateStreamingDistributionWithTags.yaml"

requestDeleteCachePolicy :: DeleteCachePolicy -> TestTree
requestDeleteCachePolicy =
  req
    "DeleteCachePolicy"
    "fixture/DeleteCachePolicy.yaml"

requestDeleteCloudFrontOriginAccessIdentity :: DeleteCloudFrontOriginAccessIdentity -> TestTree
requestDeleteCloudFrontOriginAccessIdentity =
  req
    "DeleteCloudFrontOriginAccessIdentity"
    "fixture/DeleteCloudFrontOriginAccessIdentity.yaml"

requestDeleteDistribution :: DeleteDistribution -> TestTree
requestDeleteDistribution =
  req
    "DeleteDistribution"
    "fixture/DeleteDistribution.yaml"

requestDeleteFieldLevelEncryptionConfig :: DeleteFieldLevelEncryptionConfig -> TestTree
requestDeleteFieldLevelEncryptionConfig =
  req
    "DeleteFieldLevelEncryptionConfig"
    "fixture/DeleteFieldLevelEncryptionConfig.yaml"

requestDeleteFieldLevelEncryptionProfile :: DeleteFieldLevelEncryptionProfile -> TestTree
requestDeleteFieldLevelEncryptionProfile =
  req
    "DeleteFieldLevelEncryptionProfile"
    "fixture/DeleteFieldLevelEncryptionProfile.yaml"

requestDeleteFunction :: DeleteFunction -> TestTree
requestDeleteFunction =
  req
    "DeleteFunction"
    "fixture/DeleteFunction.yaml"

requestDeleteKeyGroup :: DeleteKeyGroup -> TestTree
requestDeleteKeyGroup =
  req
    "DeleteKeyGroup"
    "fixture/DeleteKeyGroup.yaml"

requestDeleteMonitoringSubscription :: DeleteMonitoringSubscription -> TestTree
requestDeleteMonitoringSubscription =
  req
    "DeleteMonitoringSubscription"
    "fixture/DeleteMonitoringSubscription.yaml"

requestDeleteOriginRequestPolicy :: DeleteOriginRequestPolicy -> TestTree
requestDeleteOriginRequestPolicy =
  req
    "DeleteOriginRequestPolicy"
    "fixture/DeleteOriginRequestPolicy.yaml"

requestDeletePublicKey :: DeletePublicKey -> TestTree
requestDeletePublicKey =
  req
    "DeletePublicKey"
    "fixture/DeletePublicKey.yaml"

requestDeleteRealtimeLogConfig :: DeleteRealtimeLogConfig -> TestTree
requestDeleteRealtimeLogConfig =
  req
    "DeleteRealtimeLogConfig"
    "fixture/DeleteRealtimeLogConfig.yaml"

requestDeleteStreamingDistribution :: DeleteStreamingDistribution -> TestTree
requestDeleteStreamingDistribution =
  req
    "DeleteStreamingDistribution"
    "fixture/DeleteStreamingDistribution.yaml"

requestDescribeFunction :: DescribeFunction -> TestTree
requestDescribeFunction =
  req
    "DescribeFunction"
    "fixture/DescribeFunction.yaml"

requestGetCachePolicy :: GetCachePolicy -> TestTree
requestGetCachePolicy =
  req
    "GetCachePolicy"
    "fixture/GetCachePolicy.yaml"

requestGetCachePolicyConfig :: GetCachePolicyConfig -> TestTree
requestGetCachePolicyConfig =
  req
    "GetCachePolicyConfig"
    "fixture/GetCachePolicyConfig.yaml"

requestGetCloudFrontOriginAccessIdentity :: GetCloudFrontOriginAccessIdentity -> TestTree
requestGetCloudFrontOriginAccessIdentity =
  req
    "GetCloudFrontOriginAccessIdentity"
    "fixture/GetCloudFrontOriginAccessIdentity.yaml"

requestGetCloudFrontOriginAccessIdentityConfig :: GetCloudFrontOriginAccessIdentityConfig -> TestTree
requestGetCloudFrontOriginAccessIdentityConfig =
  req
    "GetCloudFrontOriginAccessIdentityConfig"
    "fixture/GetCloudFrontOriginAccessIdentityConfig.yaml"

requestGetDistribution :: GetDistribution -> TestTree
requestGetDistribution =
  req
    "GetDistribution"
    "fixture/GetDistribution.yaml"

requestGetDistributionConfig :: GetDistributionConfig -> TestTree
requestGetDistributionConfig =
  req
    "GetDistributionConfig"
    "fixture/GetDistributionConfig.yaml"

requestGetFieldLevelEncryption :: GetFieldLevelEncryption -> TestTree
requestGetFieldLevelEncryption =
  req
    "GetFieldLevelEncryption"
    "fixture/GetFieldLevelEncryption.yaml"

requestGetFieldLevelEncryptionConfig :: GetFieldLevelEncryptionConfig -> TestTree
requestGetFieldLevelEncryptionConfig =
  req
    "GetFieldLevelEncryptionConfig"
    "fixture/GetFieldLevelEncryptionConfig.yaml"

requestGetFieldLevelEncryptionProfile :: GetFieldLevelEncryptionProfile -> TestTree
requestGetFieldLevelEncryptionProfile =
  req
    "GetFieldLevelEncryptionProfile"
    "fixture/GetFieldLevelEncryptionProfile.yaml"

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

requestGetInvalidation :: GetInvalidation -> TestTree
requestGetInvalidation =
  req
    "GetInvalidation"
    "fixture/GetInvalidation.yaml"

requestGetKeyGroup :: GetKeyGroup -> TestTree
requestGetKeyGroup =
  req
    "GetKeyGroup"
    "fixture/GetKeyGroup.yaml"

requestGetKeyGroupConfig :: GetKeyGroupConfig -> TestTree
requestGetKeyGroupConfig =
  req
    "GetKeyGroupConfig"
    "fixture/GetKeyGroupConfig.yaml"

requestGetMonitoringSubscription :: GetMonitoringSubscription -> TestTree
requestGetMonitoringSubscription =
  req
    "GetMonitoringSubscription"
    "fixture/GetMonitoringSubscription.yaml"

requestGetOriginRequestPolicy :: GetOriginRequestPolicy -> TestTree
requestGetOriginRequestPolicy =
  req
    "GetOriginRequestPolicy"
    "fixture/GetOriginRequestPolicy.yaml"

requestGetOriginRequestPolicyConfig :: GetOriginRequestPolicyConfig -> TestTree
requestGetOriginRequestPolicyConfig =
  req
    "GetOriginRequestPolicyConfig"
    "fixture/GetOriginRequestPolicyConfig.yaml"

requestGetPublicKey :: GetPublicKey -> TestTree
requestGetPublicKey =
  req
    "GetPublicKey"
    "fixture/GetPublicKey.yaml"

requestGetPublicKeyConfig :: GetPublicKeyConfig -> TestTree
requestGetPublicKeyConfig =
  req
    "GetPublicKeyConfig"
    "fixture/GetPublicKeyConfig.yaml"

requestGetRealtimeLogConfig :: GetRealtimeLogConfig -> TestTree
requestGetRealtimeLogConfig =
  req
    "GetRealtimeLogConfig"
    "fixture/GetRealtimeLogConfig.yaml"

requestGetStreamingDistribution :: GetStreamingDistribution -> TestTree
requestGetStreamingDistribution =
  req
    "GetStreamingDistribution"
    "fixture/GetStreamingDistribution.yaml"

requestGetStreamingDistributionConfig :: GetStreamingDistributionConfig -> TestTree
requestGetStreamingDistributionConfig =
  req
    "GetStreamingDistributionConfig"
    "fixture/GetStreamingDistributionConfig.yaml"

requestListCachePolicies :: ListCachePolicies -> TestTree
requestListCachePolicies =
  req
    "ListCachePolicies"
    "fixture/ListCachePolicies.yaml"

requestListCloudFrontOriginAccessIdentities :: ListCloudFrontOriginAccessIdentities -> TestTree
requestListCloudFrontOriginAccessIdentities =
  req
    "ListCloudFrontOriginAccessIdentities"
    "fixture/ListCloudFrontOriginAccessIdentities.yaml"

requestListConflictingAliases :: ListConflictingAliases -> TestTree
requestListConflictingAliases =
  req
    "ListConflictingAliases"
    "fixture/ListConflictingAliases.yaml"

requestListDistributions :: ListDistributions -> TestTree
requestListDistributions =
  req
    "ListDistributions"
    "fixture/ListDistributions.yaml"

requestListDistributionsByCachePolicyId :: ListDistributionsByCachePolicyId -> TestTree
requestListDistributionsByCachePolicyId =
  req
    "ListDistributionsByCachePolicyId"
    "fixture/ListDistributionsByCachePolicyId.yaml"

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

requestListDistributionsByRealtimeLogConfig :: ListDistributionsByRealtimeLogConfig -> TestTree
requestListDistributionsByRealtimeLogConfig =
  req
    "ListDistributionsByRealtimeLogConfig"
    "fixture/ListDistributionsByRealtimeLogConfig.yaml"

requestListDistributionsByWebACLId :: ListDistributionsByWebACLId -> TestTree
requestListDistributionsByWebACLId =
  req
    "ListDistributionsByWebACLId"
    "fixture/ListDistributionsByWebACLId.yaml"

requestListFieldLevelEncryptionConfigs :: ListFieldLevelEncryptionConfigs -> TestTree
requestListFieldLevelEncryptionConfigs =
  req
    "ListFieldLevelEncryptionConfigs"
    "fixture/ListFieldLevelEncryptionConfigs.yaml"

requestListFieldLevelEncryptionProfiles :: ListFieldLevelEncryptionProfiles -> TestTree
requestListFieldLevelEncryptionProfiles =
  req
    "ListFieldLevelEncryptionProfiles"
    "fixture/ListFieldLevelEncryptionProfiles.yaml"

requestListFunctions :: ListFunctions -> TestTree
requestListFunctions =
  req
    "ListFunctions"
    "fixture/ListFunctions.yaml"

requestListInvalidations :: ListInvalidations -> TestTree
requestListInvalidations =
  req
    "ListInvalidations"
    "fixture/ListInvalidations.yaml"

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

requestListPublicKeys :: ListPublicKeys -> TestTree
requestListPublicKeys =
  req
    "ListPublicKeys"
    "fixture/ListPublicKeys.yaml"

requestListRealtimeLogConfigs :: ListRealtimeLogConfigs -> TestTree
requestListRealtimeLogConfigs =
  req
    "ListRealtimeLogConfigs"
    "fixture/ListRealtimeLogConfigs.yaml"

requestListStreamingDistributions :: ListStreamingDistributions -> TestTree
requestListStreamingDistributions =
  req
    "ListStreamingDistributions"
    "fixture/ListStreamingDistributions.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPublishFunction :: PublishFunction -> TestTree
requestPublishFunction =
  req
    "PublishFunction"
    "fixture/PublishFunction.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestTestFunction :: TestFunction -> TestTree
requestTestFunction =
  req
    "TestFunction"
    "fixture/TestFunction.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateCachePolicy :: UpdateCachePolicy -> TestTree
requestUpdateCachePolicy =
  req
    "UpdateCachePolicy"
    "fixture/UpdateCachePolicy.yaml"

requestUpdateCloudFrontOriginAccessIdentity :: UpdateCloudFrontOriginAccessIdentity -> TestTree
requestUpdateCloudFrontOriginAccessIdentity =
  req
    "UpdateCloudFrontOriginAccessIdentity"
    "fixture/UpdateCloudFrontOriginAccessIdentity.yaml"

requestUpdateDistribution :: UpdateDistribution -> TestTree
requestUpdateDistribution =
  req
    "UpdateDistribution"
    "fixture/UpdateDistribution.yaml"

requestUpdateFieldLevelEncryptionConfig :: UpdateFieldLevelEncryptionConfig -> TestTree
requestUpdateFieldLevelEncryptionConfig =
  req
    "UpdateFieldLevelEncryptionConfig"
    "fixture/UpdateFieldLevelEncryptionConfig.yaml"

requestUpdateFieldLevelEncryptionProfile :: UpdateFieldLevelEncryptionProfile -> TestTree
requestUpdateFieldLevelEncryptionProfile =
  req
    "UpdateFieldLevelEncryptionProfile"
    "fixture/UpdateFieldLevelEncryptionProfile.yaml"

requestUpdateFunction :: UpdateFunction -> TestTree
requestUpdateFunction =
  req
    "UpdateFunction"
    "fixture/UpdateFunction.yaml"

requestUpdateKeyGroup :: UpdateKeyGroup -> TestTree
requestUpdateKeyGroup =
  req
    "UpdateKeyGroup"
    "fixture/UpdateKeyGroup.yaml"

requestUpdateOriginRequestPolicy :: UpdateOriginRequestPolicy -> TestTree
requestUpdateOriginRequestPolicy =
  req
    "UpdateOriginRequestPolicy"
    "fixture/UpdateOriginRequestPolicy.yaml"

requestUpdatePublicKey :: UpdatePublicKey -> TestTree
requestUpdatePublicKey =
  req
    "UpdatePublicKey"
    "fixture/UpdatePublicKey.yaml"

requestUpdateRealtimeLogConfig :: UpdateRealtimeLogConfig -> TestTree
requestUpdateRealtimeLogConfig =
  req
    "UpdateRealtimeLogConfig"
    "fixture/UpdateRealtimeLogConfig.yaml"

requestUpdateStreamingDistribution :: UpdateStreamingDistribution -> TestTree
requestUpdateStreamingDistribution =
  req
    "UpdateStreamingDistribution"
    "fixture/UpdateStreamingDistribution.yaml"

-- Responses

responseAssociateAlias :: AssociateAliasResponse -> TestTree
responseAssociateAlias =
  res
    "AssociateAliasResponse"
    "fixture/AssociateAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateAlias)

responseCreateCachePolicy :: CreateCachePolicyResponse -> TestTree
responseCreateCachePolicy =
  res
    "CreateCachePolicyResponse"
    "fixture/CreateCachePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCachePolicy)

responseCreateCloudFrontOriginAccessIdentity :: CreateCloudFrontOriginAccessIdentityResponse -> TestTree
responseCreateCloudFrontOriginAccessIdentity =
  res
    "CreateCloudFrontOriginAccessIdentityResponse"
    "fixture/CreateCloudFrontOriginAccessIdentityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCloudFrontOriginAccessIdentity)

responseCreateDistribution :: CreateDistributionResponse -> TestTree
responseCreateDistribution =
  res
    "CreateDistributionResponse"
    "fixture/CreateDistributionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDistribution)

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

responseCreateFieldLevelEncryptionProfile :: CreateFieldLevelEncryptionProfileResponse -> TestTree
responseCreateFieldLevelEncryptionProfile =
  res
    "CreateFieldLevelEncryptionProfileResponse"
    "fixture/CreateFieldLevelEncryptionProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFieldLevelEncryptionProfile)

responseCreateFunction :: CreateFunctionResponse -> TestTree
responseCreateFunction =
  res
    "CreateFunctionResponse"
    "fixture/CreateFunctionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFunction)

responseCreateInvalidation :: CreateInvalidationResponse -> TestTree
responseCreateInvalidation =
  res
    "CreateInvalidationResponse"
    "fixture/CreateInvalidationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateInvalidation)

responseCreateKeyGroup :: CreateKeyGroupResponse -> TestTree
responseCreateKeyGroup =
  res
    "CreateKeyGroupResponse"
    "fixture/CreateKeyGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateKeyGroup)

responseCreateMonitoringSubscription :: CreateMonitoringSubscriptionResponse -> TestTree
responseCreateMonitoringSubscription =
  res
    "CreateMonitoringSubscriptionResponse"
    "fixture/CreateMonitoringSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMonitoringSubscription)

responseCreateOriginRequestPolicy :: CreateOriginRequestPolicyResponse -> TestTree
responseCreateOriginRequestPolicy =
  res
    "CreateOriginRequestPolicyResponse"
    "fixture/CreateOriginRequestPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateOriginRequestPolicy)

responseCreatePublicKey :: CreatePublicKeyResponse -> TestTree
responseCreatePublicKey =
  res
    "CreatePublicKeyResponse"
    "fixture/CreatePublicKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePublicKey)

responseCreateRealtimeLogConfig :: CreateRealtimeLogConfigResponse -> TestTree
responseCreateRealtimeLogConfig =
  res
    "CreateRealtimeLogConfigResponse"
    "fixture/CreateRealtimeLogConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRealtimeLogConfig)

responseCreateStreamingDistribution :: CreateStreamingDistributionResponse -> TestTree
responseCreateStreamingDistribution =
  res
    "CreateStreamingDistributionResponse"
    "fixture/CreateStreamingDistributionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStreamingDistribution)

responseCreateStreamingDistributionWithTags :: CreateStreamingDistributionWithTagsResponse -> TestTree
responseCreateStreamingDistributionWithTags =
  res
    "CreateStreamingDistributionWithTagsResponse"
    "fixture/CreateStreamingDistributionWithTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStreamingDistributionWithTags)

responseDeleteCachePolicy :: DeleteCachePolicyResponse -> TestTree
responseDeleteCachePolicy =
  res
    "DeleteCachePolicyResponse"
    "fixture/DeleteCachePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCachePolicy)

responseDeleteCloudFrontOriginAccessIdentity :: DeleteCloudFrontOriginAccessIdentityResponse -> TestTree
responseDeleteCloudFrontOriginAccessIdentity =
  res
    "DeleteCloudFrontOriginAccessIdentityResponse"
    "fixture/DeleteCloudFrontOriginAccessIdentityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCloudFrontOriginAccessIdentity)

responseDeleteDistribution :: DeleteDistributionResponse -> TestTree
responseDeleteDistribution =
  res
    "DeleteDistributionResponse"
    "fixture/DeleteDistributionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDistribution)

responseDeleteFieldLevelEncryptionConfig :: DeleteFieldLevelEncryptionConfigResponse -> TestTree
responseDeleteFieldLevelEncryptionConfig =
  res
    "DeleteFieldLevelEncryptionConfigResponse"
    "fixture/DeleteFieldLevelEncryptionConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFieldLevelEncryptionConfig)

responseDeleteFieldLevelEncryptionProfile :: DeleteFieldLevelEncryptionProfileResponse -> TestTree
responseDeleteFieldLevelEncryptionProfile =
  res
    "DeleteFieldLevelEncryptionProfileResponse"
    "fixture/DeleteFieldLevelEncryptionProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFieldLevelEncryptionProfile)

responseDeleteFunction :: DeleteFunctionResponse -> TestTree
responseDeleteFunction =
  res
    "DeleteFunctionResponse"
    "fixture/DeleteFunctionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFunction)

responseDeleteKeyGroup :: DeleteKeyGroupResponse -> TestTree
responseDeleteKeyGroup =
  res
    "DeleteKeyGroupResponse"
    "fixture/DeleteKeyGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteKeyGroup)

responseDeleteMonitoringSubscription :: DeleteMonitoringSubscriptionResponse -> TestTree
responseDeleteMonitoringSubscription =
  res
    "DeleteMonitoringSubscriptionResponse"
    "fixture/DeleteMonitoringSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMonitoringSubscription)

responseDeleteOriginRequestPolicy :: DeleteOriginRequestPolicyResponse -> TestTree
responseDeleteOriginRequestPolicy =
  res
    "DeleteOriginRequestPolicyResponse"
    "fixture/DeleteOriginRequestPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteOriginRequestPolicy)

responseDeletePublicKey :: DeletePublicKeyResponse -> TestTree
responseDeletePublicKey =
  res
    "DeletePublicKeyResponse"
    "fixture/DeletePublicKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePublicKey)

responseDeleteRealtimeLogConfig :: DeleteRealtimeLogConfigResponse -> TestTree
responseDeleteRealtimeLogConfig =
  res
    "DeleteRealtimeLogConfigResponse"
    "fixture/DeleteRealtimeLogConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRealtimeLogConfig)

responseDeleteStreamingDistribution :: DeleteStreamingDistributionResponse -> TestTree
responseDeleteStreamingDistribution =
  res
    "DeleteStreamingDistributionResponse"
    "fixture/DeleteStreamingDistributionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteStreamingDistribution)

responseDescribeFunction :: DescribeFunctionResponse -> TestTree
responseDescribeFunction =
  res
    "DescribeFunctionResponse"
    "fixture/DescribeFunctionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFunction)

responseGetCachePolicy :: GetCachePolicyResponse -> TestTree
responseGetCachePolicy =
  res
    "GetCachePolicyResponse"
    "fixture/GetCachePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCachePolicy)

responseGetCachePolicyConfig :: GetCachePolicyConfigResponse -> TestTree
responseGetCachePolicyConfig =
  res
    "GetCachePolicyConfigResponse"
    "fixture/GetCachePolicyConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCachePolicyConfig)

responseGetCloudFrontOriginAccessIdentity :: GetCloudFrontOriginAccessIdentityResponse -> TestTree
responseGetCloudFrontOriginAccessIdentity =
  res
    "GetCloudFrontOriginAccessIdentityResponse"
    "fixture/GetCloudFrontOriginAccessIdentityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCloudFrontOriginAccessIdentity)

responseGetCloudFrontOriginAccessIdentityConfig :: GetCloudFrontOriginAccessIdentityConfigResponse -> TestTree
responseGetCloudFrontOriginAccessIdentityConfig =
  res
    "GetCloudFrontOriginAccessIdentityConfigResponse"
    "fixture/GetCloudFrontOriginAccessIdentityConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCloudFrontOriginAccessIdentityConfig)

responseGetDistribution :: GetDistributionResponse -> TestTree
responseGetDistribution =
  res
    "GetDistributionResponse"
    "fixture/GetDistributionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDistribution)

responseGetDistributionConfig :: GetDistributionConfigResponse -> TestTree
responseGetDistributionConfig =
  res
    "GetDistributionConfigResponse"
    "fixture/GetDistributionConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDistributionConfig)

responseGetFieldLevelEncryption :: GetFieldLevelEncryptionResponse -> TestTree
responseGetFieldLevelEncryption =
  res
    "GetFieldLevelEncryptionResponse"
    "fixture/GetFieldLevelEncryptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFieldLevelEncryption)

responseGetFieldLevelEncryptionConfig :: GetFieldLevelEncryptionConfigResponse -> TestTree
responseGetFieldLevelEncryptionConfig =
  res
    "GetFieldLevelEncryptionConfigResponse"
    "fixture/GetFieldLevelEncryptionConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFieldLevelEncryptionConfig)

responseGetFieldLevelEncryptionProfile :: GetFieldLevelEncryptionProfileResponse -> TestTree
responseGetFieldLevelEncryptionProfile =
  res
    "GetFieldLevelEncryptionProfileResponse"
    "fixture/GetFieldLevelEncryptionProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFieldLevelEncryptionProfile)

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

responseGetInvalidation :: GetInvalidationResponse -> TestTree
responseGetInvalidation =
  res
    "GetInvalidationResponse"
    "fixture/GetInvalidationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInvalidation)

responseGetKeyGroup :: GetKeyGroupResponse -> TestTree
responseGetKeyGroup =
  res
    "GetKeyGroupResponse"
    "fixture/GetKeyGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetKeyGroup)

responseGetKeyGroupConfig :: GetKeyGroupConfigResponse -> TestTree
responseGetKeyGroupConfig =
  res
    "GetKeyGroupConfigResponse"
    "fixture/GetKeyGroupConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetKeyGroupConfig)

responseGetMonitoringSubscription :: GetMonitoringSubscriptionResponse -> TestTree
responseGetMonitoringSubscription =
  res
    "GetMonitoringSubscriptionResponse"
    "fixture/GetMonitoringSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMonitoringSubscription)

responseGetOriginRequestPolicy :: GetOriginRequestPolicyResponse -> TestTree
responseGetOriginRequestPolicy =
  res
    "GetOriginRequestPolicyResponse"
    "fixture/GetOriginRequestPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetOriginRequestPolicy)

responseGetOriginRequestPolicyConfig :: GetOriginRequestPolicyConfigResponse -> TestTree
responseGetOriginRequestPolicyConfig =
  res
    "GetOriginRequestPolicyConfigResponse"
    "fixture/GetOriginRequestPolicyConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetOriginRequestPolicyConfig)

responseGetPublicKey :: GetPublicKeyResponse -> TestTree
responseGetPublicKey =
  res
    "GetPublicKeyResponse"
    "fixture/GetPublicKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPublicKey)

responseGetPublicKeyConfig :: GetPublicKeyConfigResponse -> TestTree
responseGetPublicKeyConfig =
  res
    "GetPublicKeyConfigResponse"
    "fixture/GetPublicKeyConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPublicKeyConfig)

responseGetRealtimeLogConfig :: GetRealtimeLogConfigResponse -> TestTree
responseGetRealtimeLogConfig =
  res
    "GetRealtimeLogConfigResponse"
    "fixture/GetRealtimeLogConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRealtimeLogConfig)

responseGetStreamingDistribution :: GetStreamingDistributionResponse -> TestTree
responseGetStreamingDistribution =
  res
    "GetStreamingDistributionResponse"
    "fixture/GetStreamingDistributionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetStreamingDistribution)

responseGetStreamingDistributionConfig :: GetStreamingDistributionConfigResponse -> TestTree
responseGetStreamingDistributionConfig =
  res
    "GetStreamingDistributionConfigResponse"
    "fixture/GetStreamingDistributionConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetStreamingDistributionConfig)

responseListCachePolicies :: ListCachePoliciesResponse -> TestTree
responseListCachePolicies =
  res
    "ListCachePoliciesResponse"
    "fixture/ListCachePoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCachePolicies)

responseListCloudFrontOriginAccessIdentities :: ListCloudFrontOriginAccessIdentitiesResponse -> TestTree
responseListCloudFrontOriginAccessIdentities =
  res
    "ListCloudFrontOriginAccessIdentitiesResponse"
    "fixture/ListCloudFrontOriginAccessIdentitiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCloudFrontOriginAccessIdentities)

responseListConflictingAliases :: ListConflictingAliasesResponse -> TestTree
responseListConflictingAliases =
  res
    "ListConflictingAliasesResponse"
    "fixture/ListConflictingAliasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListConflictingAliases)

responseListDistributions :: ListDistributionsResponse -> TestTree
responseListDistributions =
  res
    "ListDistributionsResponse"
    "fixture/ListDistributionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDistributions)

responseListDistributionsByCachePolicyId :: ListDistributionsByCachePolicyIdResponse -> TestTree
responseListDistributionsByCachePolicyId =
  res
    "ListDistributionsByCachePolicyIdResponse"
    "fixture/ListDistributionsByCachePolicyIdResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDistributionsByCachePolicyId)

responseListDistributionsByKeyGroup :: ListDistributionsByKeyGroupResponse -> TestTree
responseListDistributionsByKeyGroup =
  res
    "ListDistributionsByKeyGroupResponse"
    "fixture/ListDistributionsByKeyGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDistributionsByKeyGroup)

responseListDistributionsByOriginRequestPolicyId :: ListDistributionsByOriginRequestPolicyIdResponse -> TestTree
responseListDistributionsByOriginRequestPolicyId =
  res
    "ListDistributionsByOriginRequestPolicyIdResponse"
    "fixture/ListDistributionsByOriginRequestPolicyIdResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDistributionsByOriginRequestPolicyId)

responseListDistributionsByRealtimeLogConfig :: ListDistributionsByRealtimeLogConfigResponse -> TestTree
responseListDistributionsByRealtimeLogConfig =
  res
    "ListDistributionsByRealtimeLogConfigResponse"
    "fixture/ListDistributionsByRealtimeLogConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDistributionsByRealtimeLogConfig)

responseListDistributionsByWebACLId :: ListDistributionsByWebACLIdResponse -> TestTree
responseListDistributionsByWebACLId =
  res
    "ListDistributionsByWebACLIdResponse"
    "fixture/ListDistributionsByWebACLIdResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDistributionsByWebACLId)

responseListFieldLevelEncryptionConfigs :: ListFieldLevelEncryptionConfigsResponse -> TestTree
responseListFieldLevelEncryptionConfigs =
  res
    "ListFieldLevelEncryptionConfigsResponse"
    "fixture/ListFieldLevelEncryptionConfigsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFieldLevelEncryptionConfigs)

responseListFieldLevelEncryptionProfiles :: ListFieldLevelEncryptionProfilesResponse -> TestTree
responseListFieldLevelEncryptionProfiles =
  res
    "ListFieldLevelEncryptionProfilesResponse"
    "fixture/ListFieldLevelEncryptionProfilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFieldLevelEncryptionProfiles)

responseListFunctions :: ListFunctionsResponse -> TestTree
responseListFunctions =
  res
    "ListFunctionsResponse"
    "fixture/ListFunctionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFunctions)

responseListInvalidations :: ListInvalidationsResponse -> TestTree
responseListInvalidations =
  res
    "ListInvalidationsResponse"
    "fixture/ListInvalidationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInvalidations)

responseListKeyGroups :: ListKeyGroupsResponse -> TestTree
responseListKeyGroups =
  res
    "ListKeyGroupsResponse"
    "fixture/ListKeyGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListKeyGroups)

responseListOriginRequestPolicies :: ListOriginRequestPoliciesResponse -> TestTree
responseListOriginRequestPolicies =
  res
    "ListOriginRequestPoliciesResponse"
    "fixture/ListOriginRequestPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOriginRequestPolicies)

responseListPublicKeys :: ListPublicKeysResponse -> TestTree
responseListPublicKeys =
  res
    "ListPublicKeysResponse"
    "fixture/ListPublicKeysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPublicKeys)

responseListRealtimeLogConfigs :: ListRealtimeLogConfigsResponse -> TestTree
responseListRealtimeLogConfigs =
  res
    "ListRealtimeLogConfigsResponse"
    "fixture/ListRealtimeLogConfigsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRealtimeLogConfigs)

responseListStreamingDistributions :: ListStreamingDistributionsResponse -> TestTree
responseListStreamingDistributions =
  res
    "ListStreamingDistributionsResponse"
    "fixture/ListStreamingDistributionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStreamingDistributions)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePublishFunction :: PublishFunctionResponse -> TestTree
responsePublishFunction =
  res
    "PublishFunctionResponse"
    "fixture/PublishFunctionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PublishFunction)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseTestFunction :: TestFunctionResponse -> TestTree
responseTestFunction =
  res
    "TestFunctionResponse"
    "fixture/TestFunctionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TestFunction)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateCachePolicy :: UpdateCachePolicyResponse -> TestTree
responseUpdateCachePolicy =
  res
    "UpdateCachePolicyResponse"
    "fixture/UpdateCachePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCachePolicy)

responseUpdateCloudFrontOriginAccessIdentity :: UpdateCloudFrontOriginAccessIdentityResponse -> TestTree
responseUpdateCloudFrontOriginAccessIdentity =
  res
    "UpdateCloudFrontOriginAccessIdentityResponse"
    "fixture/UpdateCloudFrontOriginAccessIdentityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCloudFrontOriginAccessIdentity)

responseUpdateDistribution :: UpdateDistributionResponse -> TestTree
responseUpdateDistribution =
  res
    "UpdateDistributionResponse"
    "fixture/UpdateDistributionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDistribution)

responseUpdateFieldLevelEncryptionConfig :: UpdateFieldLevelEncryptionConfigResponse -> TestTree
responseUpdateFieldLevelEncryptionConfig =
  res
    "UpdateFieldLevelEncryptionConfigResponse"
    "fixture/UpdateFieldLevelEncryptionConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFieldLevelEncryptionConfig)

responseUpdateFieldLevelEncryptionProfile :: UpdateFieldLevelEncryptionProfileResponse -> TestTree
responseUpdateFieldLevelEncryptionProfile =
  res
    "UpdateFieldLevelEncryptionProfileResponse"
    "fixture/UpdateFieldLevelEncryptionProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFieldLevelEncryptionProfile)

responseUpdateFunction :: UpdateFunctionResponse -> TestTree
responseUpdateFunction =
  res
    "UpdateFunctionResponse"
    "fixture/UpdateFunctionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFunction)

responseUpdateKeyGroup :: UpdateKeyGroupResponse -> TestTree
responseUpdateKeyGroup =
  res
    "UpdateKeyGroupResponse"
    "fixture/UpdateKeyGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateKeyGroup)

responseUpdateOriginRequestPolicy :: UpdateOriginRequestPolicyResponse -> TestTree
responseUpdateOriginRequestPolicy =
  res
    "UpdateOriginRequestPolicyResponse"
    "fixture/UpdateOriginRequestPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateOriginRequestPolicy)

responseUpdatePublicKey :: UpdatePublicKeyResponse -> TestTree
responseUpdatePublicKey =
  res
    "UpdatePublicKeyResponse"
    "fixture/UpdatePublicKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePublicKey)

responseUpdateRealtimeLogConfig :: UpdateRealtimeLogConfigResponse -> TestTree
responseUpdateRealtimeLogConfig =
  res
    "UpdateRealtimeLogConfigResponse"
    "fixture/UpdateRealtimeLogConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRealtimeLogConfig)

responseUpdateStreamingDistribution :: UpdateStreamingDistributionResponse -> TestTree
responseUpdateStreamingDistribution =
  res
    "UpdateStreamingDistributionResponse"
    "fixture/UpdateStreamingDistributionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateStreamingDistribution)
