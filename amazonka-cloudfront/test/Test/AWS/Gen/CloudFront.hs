{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudFront
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
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
--         [ requestDeleteStreamingDistribution $
--             deleteStreamingDistribution
--
--         , requestUpdateStreamingDistribution $
--             updateStreamingDistribution
--
--         , requestListPublicKeys $
--             listPublicKeys
--
--         , requestGetFieldLevelEncryptionConfig $
--             getFieldLevelEncryptionConfig
--
--         , requestListTagsForResource $
--             listTagsForResource
--
--         , requestCreatePublicKey $
--             createPublicKey
--
--         , requestGetPublicKeyConfig $
--             getPublicKeyConfig
--
--         , requestCreateDistributionWithTags $
--             createDistributionWithTags
--
--         , requestCreateFieldLevelEncryptionConfig $
--             createFieldLevelEncryptionConfig
--
--         , requestGetFieldLevelEncryption $
--             getFieldLevelEncryption
--
--         , requestGetPublicKey $
--             getPublicKey
--
--         , requestDeleteFieldLevelEncryptionConfig $
--             deleteFieldLevelEncryptionConfig
--
--         , requestUpdateFieldLevelEncryptionConfig $
--             updateFieldLevelEncryptionConfig
--
--         , requestCreateDistribution $
--             createDistribution
--
--         , requestGetFieldLevelEncryptionProfile $
--             getFieldLevelEncryptionProfile
--
--         , requestGetDistributionConfig $
--             getDistributionConfig
--
--         , requestCreateStreamingDistributionWithTags $
--             createStreamingDistributionWithTags
--
--         , requestDeleteFieldLevelEncryptionProfile $
--             deleteFieldLevelEncryptionProfile
--
--         , requestUpdateFieldLevelEncryptionProfile $
--             updateFieldLevelEncryptionProfile
--
--         , requestDeleteServiceLinkedRole $
--             deleteServiceLinkedRole
--
--         , requestCreateFieldLevelEncryptionProfile $
--             createFieldLevelEncryptionProfile
--
--         , requestGetDistribution $
--             getDistribution
--
--         , requestGetFieldLevelEncryptionProfileConfig $
--             getFieldLevelEncryptionProfileConfig
--
--         , requestUpdateCloudFrontOriginAccessIdentity $
--             updateCloudFrontOriginAccessIdentity
--
--         , requestDeleteCloudFrontOriginAccessIdentity $
--             deleteCloudFrontOriginAccessIdentity
--
--         , requestListStreamingDistributions $
--             listStreamingDistributions
--
--         , requestDeletePublicKey $
--             deletePublicKey
--
--         , requestUpdatePublicKey $
--             updatePublicKey
--
--         , requestGetStreamingDistributionConfig $
--             getStreamingDistributionConfig
--
--         , requestGetCloudFrontOriginAccessIdentityConfig $
--             getCloudFrontOriginAccessIdentityConfig
--
--         , requestCreateStreamingDistribution $
--             createStreamingDistribution
--
--         , requestCreateCloudFrontOriginAccessIdentity $
--             createCloudFrontOriginAccessIdentity
--
--         , requestListCloudFrontOriginAccessIdentities $
--             listCloudFrontOriginAccessIdentities
--
--         , requestGetInvalidation $
--             getInvalidation
--
--         , requestListInvalidations $
--             listInvalidations
--
--         , requestCreateInvalidation $
--             createInvalidation
--
--         , requestGetCloudFrontOriginAccessIdentity $
--             getCloudFrontOriginAccessIdentity
--
--         , requestListFieldLevelEncryptionConfigs $
--             listFieldLevelEncryptionConfigs
--
--         , requestTagResource $
--             tagResource
--
--         , requestGetStreamingDistribution $
--             getStreamingDistribution
--
--         , requestUpdateDistribution $
--             updateDistribution
--
--         , requestDeleteDistribution $
--             deleteDistribution
--
--         , requestUntagResource $
--             untagResource
--
--         , requestListDistributionsByWebACLId $
--             listDistributionsByWebACLId
--
--         , requestListDistributions $
--             listDistributions
--
--         , requestListFieldLevelEncryptionProfiles $
--             listFieldLevelEncryptionProfiles
--
--           ]

--     , testGroup "response"
--         [ responseDeleteStreamingDistribution $
--             deleteStreamingDistributionResponse
--
--         , responseUpdateStreamingDistribution $
--             updateStreamingDistributionResponse
--
--         , responseListPublicKeys $
--             listPublicKeysResponse
--
--         , responseGetFieldLevelEncryptionConfig $
--             getFieldLevelEncryptionConfigResponse
--
--         , responseListTagsForResource $
--             listTagsForResourceResponse
--
--         , responseCreatePublicKey $
--             createPublicKeyResponse
--
--         , responseGetPublicKeyConfig $
--             getPublicKeyConfigResponse
--
--         , responseCreateDistributionWithTags $
--             createDistributionWithTagsResponse
--
--         , responseCreateFieldLevelEncryptionConfig $
--             createFieldLevelEncryptionConfigResponse
--
--         , responseGetFieldLevelEncryption $
--             getFieldLevelEncryptionResponse
--
--         , responseGetPublicKey $
--             getPublicKeyResponse
--
--         , responseDeleteFieldLevelEncryptionConfig $
--             deleteFieldLevelEncryptionConfigResponse
--
--         , responseUpdateFieldLevelEncryptionConfig $
--             updateFieldLevelEncryptionConfigResponse
--
--         , responseCreateDistribution $
--             createDistributionResponse
--
--         , responseGetFieldLevelEncryptionProfile $
--             getFieldLevelEncryptionProfileResponse
--
--         , responseGetDistributionConfig $
--             getDistributionConfigResponse
--
--         , responseCreateStreamingDistributionWithTags $
--             createStreamingDistributionWithTagsResponse
--
--         , responseDeleteFieldLevelEncryptionProfile $
--             deleteFieldLevelEncryptionProfileResponse
--
--         , responseUpdateFieldLevelEncryptionProfile $
--             updateFieldLevelEncryptionProfileResponse
--
--         , responseDeleteServiceLinkedRole $
--             deleteServiceLinkedRoleResponse
--
--         , responseCreateFieldLevelEncryptionProfile $
--             createFieldLevelEncryptionProfileResponse
--
--         , responseGetDistribution $
--             getDistributionResponse
--
--         , responseGetFieldLevelEncryptionProfileConfig $
--             getFieldLevelEncryptionProfileConfigResponse
--
--         , responseUpdateCloudFrontOriginAccessIdentity $
--             updateCloudFrontOriginAccessIdentityResponse
--
--         , responseDeleteCloudFrontOriginAccessIdentity $
--             deleteCloudFrontOriginAccessIdentityResponse
--
--         , responseListStreamingDistributions $
--             listStreamingDistributionsResponse
--
--         , responseDeletePublicKey $
--             deletePublicKeyResponse
--
--         , responseUpdatePublicKey $
--             updatePublicKeyResponse
--
--         , responseGetStreamingDistributionConfig $
--             getStreamingDistributionConfigResponse
--
--         , responseGetCloudFrontOriginAccessIdentityConfig $
--             getCloudFrontOriginAccessIdentityConfigResponse
--
--         , responseCreateStreamingDistribution $
--             createStreamingDistributionResponse
--
--         , responseCreateCloudFrontOriginAccessIdentity $
--             createCloudFrontOriginAccessIdentityResponse
--
--         , responseListCloudFrontOriginAccessIdentities $
--             listCloudFrontOriginAccessIdentitiesResponse
--
--         , responseGetInvalidation $
--             getInvalidationResponse
--
--         , responseListInvalidations $
--             listInvalidationsResponse
--
--         , responseCreateInvalidation $
--             createInvalidationResponse
--
--         , responseGetCloudFrontOriginAccessIdentity $
--             getCloudFrontOriginAccessIdentityResponse
--
--         , responseListFieldLevelEncryptionConfigs $
--             listFieldLevelEncryptionConfigsResponse
--
--         , responseTagResource $
--             tagResourceResponse
--
--         , responseGetStreamingDistribution $
--             getStreamingDistributionResponse
--
--         , responseUpdateDistribution $
--             updateDistributionResponse
--
--         , responseDeleteDistribution $
--             deleteDistributionResponse
--
--         , responseUntagResource $
--             untagResourceResponse
--
--         , responseListDistributionsByWebACLId $
--             listDistributionsByWebACLIdResponse
--
--         , responseListDistributions $
--             listDistributionsResponse
--
--         , responseListFieldLevelEncryptionProfiles $
--             listFieldLevelEncryptionProfilesResponse
--
--           ]
--     ]

-- Requests

requestDeleteStreamingDistribution :: DeleteStreamingDistribution -> TestTree
requestDeleteStreamingDistribution = req
    "DeleteStreamingDistribution"
    "fixture/DeleteStreamingDistribution.yaml"

requestUpdateStreamingDistribution :: UpdateStreamingDistribution -> TestTree
requestUpdateStreamingDistribution = req
    "UpdateStreamingDistribution"
    "fixture/UpdateStreamingDistribution.yaml"

requestListPublicKeys :: ListPublicKeys -> TestTree
requestListPublicKeys = req
    "ListPublicKeys"
    "fixture/ListPublicKeys.yaml"

requestGetFieldLevelEncryptionConfig :: GetFieldLevelEncryptionConfig -> TestTree
requestGetFieldLevelEncryptionConfig = req
    "GetFieldLevelEncryptionConfig"
    "fixture/GetFieldLevelEncryptionConfig.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource = req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestCreatePublicKey :: CreatePublicKey -> TestTree
requestCreatePublicKey = req
    "CreatePublicKey"
    "fixture/CreatePublicKey.yaml"

requestGetPublicKeyConfig :: GetPublicKeyConfig -> TestTree
requestGetPublicKeyConfig = req
    "GetPublicKeyConfig"
    "fixture/GetPublicKeyConfig.yaml"

requestCreateDistributionWithTags :: CreateDistributionWithTags -> TestTree
requestCreateDistributionWithTags = req
    "CreateDistributionWithTags"
    "fixture/CreateDistributionWithTags.yaml"

requestCreateFieldLevelEncryptionConfig :: CreateFieldLevelEncryptionConfig -> TestTree
requestCreateFieldLevelEncryptionConfig = req
    "CreateFieldLevelEncryptionConfig"
    "fixture/CreateFieldLevelEncryptionConfig.yaml"

requestGetFieldLevelEncryption :: GetFieldLevelEncryption -> TestTree
requestGetFieldLevelEncryption = req
    "GetFieldLevelEncryption"
    "fixture/GetFieldLevelEncryption.yaml"

requestGetPublicKey :: GetPublicKey -> TestTree
requestGetPublicKey = req
    "GetPublicKey"
    "fixture/GetPublicKey.yaml"

requestDeleteFieldLevelEncryptionConfig :: DeleteFieldLevelEncryptionConfig -> TestTree
requestDeleteFieldLevelEncryptionConfig = req
    "DeleteFieldLevelEncryptionConfig"
    "fixture/DeleteFieldLevelEncryptionConfig.yaml"

requestUpdateFieldLevelEncryptionConfig :: UpdateFieldLevelEncryptionConfig -> TestTree
requestUpdateFieldLevelEncryptionConfig = req
    "UpdateFieldLevelEncryptionConfig"
    "fixture/UpdateFieldLevelEncryptionConfig.yaml"

requestCreateDistribution :: CreateDistribution -> TestTree
requestCreateDistribution = req
    "CreateDistribution"
    "fixture/CreateDistribution.yaml"

requestGetFieldLevelEncryptionProfile :: GetFieldLevelEncryptionProfile -> TestTree
requestGetFieldLevelEncryptionProfile = req
    "GetFieldLevelEncryptionProfile"
    "fixture/GetFieldLevelEncryptionProfile.yaml"

requestGetDistributionConfig :: GetDistributionConfig -> TestTree
requestGetDistributionConfig = req
    "GetDistributionConfig"
    "fixture/GetDistributionConfig.yaml"

requestCreateStreamingDistributionWithTags :: CreateStreamingDistributionWithTags -> TestTree
requestCreateStreamingDistributionWithTags = req
    "CreateStreamingDistributionWithTags"
    "fixture/CreateStreamingDistributionWithTags.yaml"

requestDeleteFieldLevelEncryptionProfile :: DeleteFieldLevelEncryptionProfile -> TestTree
requestDeleteFieldLevelEncryptionProfile = req
    "DeleteFieldLevelEncryptionProfile"
    "fixture/DeleteFieldLevelEncryptionProfile.yaml"

requestUpdateFieldLevelEncryptionProfile :: UpdateFieldLevelEncryptionProfile -> TestTree
requestUpdateFieldLevelEncryptionProfile = req
    "UpdateFieldLevelEncryptionProfile"
    "fixture/UpdateFieldLevelEncryptionProfile.yaml"

requestDeleteServiceLinkedRole :: DeleteServiceLinkedRole -> TestTree
requestDeleteServiceLinkedRole = req
    "DeleteServiceLinkedRole"
    "fixture/DeleteServiceLinkedRole.yaml"

requestCreateFieldLevelEncryptionProfile :: CreateFieldLevelEncryptionProfile -> TestTree
requestCreateFieldLevelEncryptionProfile = req
    "CreateFieldLevelEncryptionProfile"
    "fixture/CreateFieldLevelEncryptionProfile.yaml"

requestGetDistribution :: GetDistribution -> TestTree
requestGetDistribution = req
    "GetDistribution"
    "fixture/GetDistribution.yaml"

requestGetFieldLevelEncryptionProfileConfig :: GetFieldLevelEncryptionProfileConfig -> TestTree
requestGetFieldLevelEncryptionProfileConfig = req
    "GetFieldLevelEncryptionProfileConfig"
    "fixture/GetFieldLevelEncryptionProfileConfig.yaml"

requestUpdateCloudFrontOriginAccessIdentity :: UpdateCloudFrontOriginAccessIdentity -> TestTree
requestUpdateCloudFrontOriginAccessIdentity = req
    "UpdateCloudFrontOriginAccessIdentity"
    "fixture/UpdateCloudFrontOriginAccessIdentity.yaml"

requestDeleteCloudFrontOriginAccessIdentity :: DeleteCloudFrontOriginAccessIdentity -> TestTree
requestDeleteCloudFrontOriginAccessIdentity = req
    "DeleteCloudFrontOriginAccessIdentity"
    "fixture/DeleteCloudFrontOriginAccessIdentity.yaml"

requestListStreamingDistributions :: ListStreamingDistributions -> TestTree
requestListStreamingDistributions = req
    "ListStreamingDistributions"
    "fixture/ListStreamingDistributions.yaml"

requestDeletePublicKey :: DeletePublicKey -> TestTree
requestDeletePublicKey = req
    "DeletePublicKey"
    "fixture/DeletePublicKey.yaml"

requestUpdatePublicKey :: UpdatePublicKey -> TestTree
requestUpdatePublicKey = req
    "UpdatePublicKey"
    "fixture/UpdatePublicKey.yaml"

requestGetStreamingDistributionConfig :: GetStreamingDistributionConfig -> TestTree
requestGetStreamingDistributionConfig = req
    "GetStreamingDistributionConfig"
    "fixture/GetStreamingDistributionConfig.yaml"

requestGetCloudFrontOriginAccessIdentityConfig :: GetCloudFrontOriginAccessIdentityConfig -> TestTree
requestGetCloudFrontOriginAccessIdentityConfig = req
    "GetCloudFrontOriginAccessIdentityConfig"
    "fixture/GetCloudFrontOriginAccessIdentityConfig.yaml"

requestCreateStreamingDistribution :: CreateStreamingDistribution -> TestTree
requestCreateStreamingDistribution = req
    "CreateStreamingDistribution"
    "fixture/CreateStreamingDistribution.yaml"

requestCreateCloudFrontOriginAccessIdentity :: CreateCloudFrontOriginAccessIdentity -> TestTree
requestCreateCloudFrontOriginAccessIdentity = req
    "CreateCloudFrontOriginAccessIdentity"
    "fixture/CreateCloudFrontOriginAccessIdentity.yaml"

requestListCloudFrontOriginAccessIdentities :: ListCloudFrontOriginAccessIdentities -> TestTree
requestListCloudFrontOriginAccessIdentities = req
    "ListCloudFrontOriginAccessIdentities"
    "fixture/ListCloudFrontOriginAccessIdentities.yaml"

requestGetInvalidation :: GetInvalidation -> TestTree
requestGetInvalidation = req
    "GetInvalidation"
    "fixture/GetInvalidation.yaml"

requestListInvalidations :: ListInvalidations -> TestTree
requestListInvalidations = req
    "ListInvalidations"
    "fixture/ListInvalidations.yaml"

requestCreateInvalidation :: CreateInvalidation -> TestTree
requestCreateInvalidation = req
    "CreateInvalidation"
    "fixture/CreateInvalidation.yaml"

requestGetCloudFrontOriginAccessIdentity :: GetCloudFrontOriginAccessIdentity -> TestTree
requestGetCloudFrontOriginAccessIdentity = req
    "GetCloudFrontOriginAccessIdentity"
    "fixture/GetCloudFrontOriginAccessIdentity.yaml"

requestListFieldLevelEncryptionConfigs :: ListFieldLevelEncryptionConfigs -> TestTree
requestListFieldLevelEncryptionConfigs = req
    "ListFieldLevelEncryptionConfigs"
    "fixture/ListFieldLevelEncryptionConfigs.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource = req
    "TagResource"
    "fixture/TagResource.yaml"

requestGetStreamingDistribution :: GetStreamingDistribution -> TestTree
requestGetStreamingDistribution = req
    "GetStreamingDistribution"
    "fixture/GetStreamingDistribution.yaml"

requestUpdateDistribution :: UpdateDistribution -> TestTree
requestUpdateDistribution = req
    "UpdateDistribution"
    "fixture/UpdateDistribution.yaml"

requestDeleteDistribution :: DeleteDistribution -> TestTree
requestDeleteDistribution = req
    "DeleteDistribution"
    "fixture/DeleteDistribution.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource = req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestListDistributionsByWebACLId :: ListDistributionsByWebACLId -> TestTree
requestListDistributionsByWebACLId = req
    "ListDistributionsByWebACLId"
    "fixture/ListDistributionsByWebACLId.yaml"

requestListDistributions :: ListDistributions -> TestTree
requestListDistributions = req
    "ListDistributions"
    "fixture/ListDistributions.yaml"

requestListFieldLevelEncryptionProfiles :: ListFieldLevelEncryptionProfiles -> TestTree
requestListFieldLevelEncryptionProfiles = req
    "ListFieldLevelEncryptionProfiles"
    "fixture/ListFieldLevelEncryptionProfiles.yaml"

-- Responses

responseDeleteStreamingDistribution :: DeleteStreamingDistributionResponse -> TestTree
responseDeleteStreamingDistribution = res
    "DeleteStreamingDistributionResponse"
    "fixture/DeleteStreamingDistributionResponse.proto"
    cloudFront
    (Proxy :: Proxy DeleteStreamingDistribution)

responseUpdateStreamingDistribution :: UpdateStreamingDistributionResponse -> TestTree
responseUpdateStreamingDistribution = res
    "UpdateStreamingDistributionResponse"
    "fixture/UpdateStreamingDistributionResponse.proto"
    cloudFront
    (Proxy :: Proxy UpdateStreamingDistribution)

responseListPublicKeys :: ListPublicKeysResponse -> TestTree
responseListPublicKeys = res
    "ListPublicKeysResponse"
    "fixture/ListPublicKeysResponse.proto"
    cloudFront
    (Proxy :: Proxy ListPublicKeys)

responseGetFieldLevelEncryptionConfig :: GetFieldLevelEncryptionConfigResponse -> TestTree
responseGetFieldLevelEncryptionConfig = res
    "GetFieldLevelEncryptionConfigResponse"
    "fixture/GetFieldLevelEncryptionConfigResponse.proto"
    cloudFront
    (Proxy :: Proxy GetFieldLevelEncryptionConfig)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource = res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    cloudFront
    (Proxy :: Proxy ListTagsForResource)

responseCreatePublicKey :: CreatePublicKeyResponse -> TestTree
responseCreatePublicKey = res
    "CreatePublicKeyResponse"
    "fixture/CreatePublicKeyResponse.proto"
    cloudFront
    (Proxy :: Proxy CreatePublicKey)

responseGetPublicKeyConfig :: GetPublicKeyConfigResponse -> TestTree
responseGetPublicKeyConfig = res
    "GetPublicKeyConfigResponse"
    "fixture/GetPublicKeyConfigResponse.proto"
    cloudFront
    (Proxy :: Proxy GetPublicKeyConfig)

responseCreateDistributionWithTags :: CreateDistributionWithTagsResponse -> TestTree
responseCreateDistributionWithTags = res
    "CreateDistributionWithTagsResponse"
    "fixture/CreateDistributionWithTagsResponse.proto"
    cloudFront
    (Proxy :: Proxy CreateDistributionWithTags)

responseCreateFieldLevelEncryptionConfig :: CreateFieldLevelEncryptionConfigResponse -> TestTree
responseCreateFieldLevelEncryptionConfig = res
    "CreateFieldLevelEncryptionConfigResponse"
    "fixture/CreateFieldLevelEncryptionConfigResponse.proto"
    cloudFront
    (Proxy :: Proxy CreateFieldLevelEncryptionConfig)

responseGetFieldLevelEncryption :: GetFieldLevelEncryptionResponse -> TestTree
responseGetFieldLevelEncryption = res
    "GetFieldLevelEncryptionResponse"
    "fixture/GetFieldLevelEncryptionResponse.proto"
    cloudFront
    (Proxy :: Proxy GetFieldLevelEncryption)

responseGetPublicKey :: GetPublicKeyResponse -> TestTree
responseGetPublicKey = res
    "GetPublicKeyResponse"
    "fixture/GetPublicKeyResponse.proto"
    cloudFront
    (Proxy :: Proxy GetPublicKey)

responseDeleteFieldLevelEncryptionConfig :: DeleteFieldLevelEncryptionConfigResponse -> TestTree
responseDeleteFieldLevelEncryptionConfig = res
    "DeleteFieldLevelEncryptionConfigResponse"
    "fixture/DeleteFieldLevelEncryptionConfigResponse.proto"
    cloudFront
    (Proxy :: Proxy DeleteFieldLevelEncryptionConfig)

responseUpdateFieldLevelEncryptionConfig :: UpdateFieldLevelEncryptionConfigResponse -> TestTree
responseUpdateFieldLevelEncryptionConfig = res
    "UpdateFieldLevelEncryptionConfigResponse"
    "fixture/UpdateFieldLevelEncryptionConfigResponse.proto"
    cloudFront
    (Proxy :: Proxy UpdateFieldLevelEncryptionConfig)

responseCreateDistribution :: CreateDistributionResponse -> TestTree
responseCreateDistribution = res
    "CreateDistributionResponse"
    "fixture/CreateDistributionResponse.proto"
    cloudFront
    (Proxy :: Proxy CreateDistribution)

responseGetFieldLevelEncryptionProfile :: GetFieldLevelEncryptionProfileResponse -> TestTree
responseGetFieldLevelEncryptionProfile = res
    "GetFieldLevelEncryptionProfileResponse"
    "fixture/GetFieldLevelEncryptionProfileResponse.proto"
    cloudFront
    (Proxy :: Proxy GetFieldLevelEncryptionProfile)

responseGetDistributionConfig :: GetDistributionConfigResponse -> TestTree
responseGetDistributionConfig = res
    "GetDistributionConfigResponse"
    "fixture/GetDistributionConfigResponse.proto"
    cloudFront
    (Proxy :: Proxy GetDistributionConfig)

responseCreateStreamingDistributionWithTags :: CreateStreamingDistributionWithTagsResponse -> TestTree
responseCreateStreamingDistributionWithTags = res
    "CreateStreamingDistributionWithTagsResponse"
    "fixture/CreateStreamingDistributionWithTagsResponse.proto"
    cloudFront
    (Proxy :: Proxy CreateStreamingDistributionWithTags)

responseDeleteFieldLevelEncryptionProfile :: DeleteFieldLevelEncryptionProfileResponse -> TestTree
responseDeleteFieldLevelEncryptionProfile = res
    "DeleteFieldLevelEncryptionProfileResponse"
    "fixture/DeleteFieldLevelEncryptionProfileResponse.proto"
    cloudFront
    (Proxy :: Proxy DeleteFieldLevelEncryptionProfile)

responseUpdateFieldLevelEncryptionProfile :: UpdateFieldLevelEncryptionProfileResponse -> TestTree
responseUpdateFieldLevelEncryptionProfile = res
    "UpdateFieldLevelEncryptionProfileResponse"
    "fixture/UpdateFieldLevelEncryptionProfileResponse.proto"
    cloudFront
    (Proxy :: Proxy UpdateFieldLevelEncryptionProfile)

responseDeleteServiceLinkedRole :: DeleteServiceLinkedRoleResponse -> TestTree
responseDeleteServiceLinkedRole = res
    "DeleteServiceLinkedRoleResponse"
    "fixture/DeleteServiceLinkedRoleResponse.proto"
    cloudFront
    (Proxy :: Proxy DeleteServiceLinkedRole)

responseCreateFieldLevelEncryptionProfile :: CreateFieldLevelEncryptionProfileResponse -> TestTree
responseCreateFieldLevelEncryptionProfile = res
    "CreateFieldLevelEncryptionProfileResponse"
    "fixture/CreateFieldLevelEncryptionProfileResponse.proto"
    cloudFront
    (Proxy :: Proxy CreateFieldLevelEncryptionProfile)

responseGetDistribution :: GetDistributionResponse -> TestTree
responseGetDistribution = res
    "GetDistributionResponse"
    "fixture/GetDistributionResponse.proto"
    cloudFront
    (Proxy :: Proxy GetDistribution)

responseGetFieldLevelEncryptionProfileConfig :: GetFieldLevelEncryptionProfileConfigResponse -> TestTree
responseGetFieldLevelEncryptionProfileConfig = res
    "GetFieldLevelEncryptionProfileConfigResponse"
    "fixture/GetFieldLevelEncryptionProfileConfigResponse.proto"
    cloudFront
    (Proxy :: Proxy GetFieldLevelEncryptionProfileConfig)

responseUpdateCloudFrontOriginAccessIdentity :: UpdateCloudFrontOriginAccessIdentityResponse -> TestTree
responseUpdateCloudFrontOriginAccessIdentity = res
    "UpdateCloudFrontOriginAccessIdentityResponse"
    "fixture/UpdateCloudFrontOriginAccessIdentityResponse.proto"
    cloudFront
    (Proxy :: Proxy UpdateCloudFrontOriginAccessIdentity)

responseDeleteCloudFrontOriginAccessIdentity :: DeleteCloudFrontOriginAccessIdentityResponse -> TestTree
responseDeleteCloudFrontOriginAccessIdentity = res
    "DeleteCloudFrontOriginAccessIdentityResponse"
    "fixture/DeleteCloudFrontOriginAccessIdentityResponse.proto"
    cloudFront
    (Proxy :: Proxy DeleteCloudFrontOriginAccessIdentity)

responseListStreamingDistributions :: ListStreamingDistributionsResponse -> TestTree
responseListStreamingDistributions = res
    "ListStreamingDistributionsResponse"
    "fixture/ListStreamingDistributionsResponse.proto"
    cloudFront
    (Proxy :: Proxy ListStreamingDistributions)

responseDeletePublicKey :: DeletePublicKeyResponse -> TestTree
responseDeletePublicKey = res
    "DeletePublicKeyResponse"
    "fixture/DeletePublicKeyResponse.proto"
    cloudFront
    (Proxy :: Proxy DeletePublicKey)

responseUpdatePublicKey :: UpdatePublicKeyResponse -> TestTree
responseUpdatePublicKey = res
    "UpdatePublicKeyResponse"
    "fixture/UpdatePublicKeyResponse.proto"
    cloudFront
    (Proxy :: Proxy UpdatePublicKey)

responseGetStreamingDistributionConfig :: GetStreamingDistributionConfigResponse -> TestTree
responseGetStreamingDistributionConfig = res
    "GetStreamingDistributionConfigResponse"
    "fixture/GetStreamingDistributionConfigResponse.proto"
    cloudFront
    (Proxy :: Proxy GetStreamingDistributionConfig)

responseGetCloudFrontOriginAccessIdentityConfig :: GetCloudFrontOriginAccessIdentityConfigResponse -> TestTree
responseGetCloudFrontOriginAccessIdentityConfig = res
    "GetCloudFrontOriginAccessIdentityConfigResponse"
    "fixture/GetCloudFrontOriginAccessIdentityConfigResponse.proto"
    cloudFront
    (Proxy :: Proxy GetCloudFrontOriginAccessIdentityConfig)

responseCreateStreamingDistribution :: CreateStreamingDistributionResponse -> TestTree
responseCreateStreamingDistribution = res
    "CreateStreamingDistributionResponse"
    "fixture/CreateStreamingDistributionResponse.proto"
    cloudFront
    (Proxy :: Proxy CreateStreamingDistribution)

responseCreateCloudFrontOriginAccessIdentity :: CreateCloudFrontOriginAccessIdentityResponse -> TestTree
responseCreateCloudFrontOriginAccessIdentity = res
    "CreateCloudFrontOriginAccessIdentityResponse"
    "fixture/CreateCloudFrontOriginAccessIdentityResponse.proto"
    cloudFront
    (Proxy :: Proxy CreateCloudFrontOriginAccessIdentity)

responseListCloudFrontOriginAccessIdentities :: ListCloudFrontOriginAccessIdentitiesResponse -> TestTree
responseListCloudFrontOriginAccessIdentities = res
    "ListCloudFrontOriginAccessIdentitiesResponse"
    "fixture/ListCloudFrontOriginAccessIdentitiesResponse.proto"
    cloudFront
    (Proxy :: Proxy ListCloudFrontOriginAccessIdentities)

responseGetInvalidation :: GetInvalidationResponse -> TestTree
responseGetInvalidation = res
    "GetInvalidationResponse"
    "fixture/GetInvalidationResponse.proto"
    cloudFront
    (Proxy :: Proxy GetInvalidation)

responseListInvalidations :: ListInvalidationsResponse -> TestTree
responseListInvalidations = res
    "ListInvalidationsResponse"
    "fixture/ListInvalidationsResponse.proto"
    cloudFront
    (Proxy :: Proxy ListInvalidations)

responseCreateInvalidation :: CreateInvalidationResponse -> TestTree
responseCreateInvalidation = res
    "CreateInvalidationResponse"
    "fixture/CreateInvalidationResponse.proto"
    cloudFront
    (Proxy :: Proxy CreateInvalidation)

responseGetCloudFrontOriginAccessIdentity :: GetCloudFrontOriginAccessIdentityResponse -> TestTree
responseGetCloudFrontOriginAccessIdentity = res
    "GetCloudFrontOriginAccessIdentityResponse"
    "fixture/GetCloudFrontOriginAccessIdentityResponse.proto"
    cloudFront
    (Proxy :: Proxy GetCloudFrontOriginAccessIdentity)

responseListFieldLevelEncryptionConfigs :: ListFieldLevelEncryptionConfigsResponse -> TestTree
responseListFieldLevelEncryptionConfigs = res
    "ListFieldLevelEncryptionConfigsResponse"
    "fixture/ListFieldLevelEncryptionConfigsResponse.proto"
    cloudFront
    (Proxy :: Proxy ListFieldLevelEncryptionConfigs)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource = res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    cloudFront
    (Proxy :: Proxy TagResource)

responseGetStreamingDistribution :: GetStreamingDistributionResponse -> TestTree
responseGetStreamingDistribution = res
    "GetStreamingDistributionResponse"
    "fixture/GetStreamingDistributionResponse.proto"
    cloudFront
    (Proxy :: Proxy GetStreamingDistribution)

responseUpdateDistribution :: UpdateDistributionResponse -> TestTree
responseUpdateDistribution = res
    "UpdateDistributionResponse"
    "fixture/UpdateDistributionResponse.proto"
    cloudFront
    (Proxy :: Proxy UpdateDistribution)

responseDeleteDistribution :: DeleteDistributionResponse -> TestTree
responseDeleteDistribution = res
    "DeleteDistributionResponse"
    "fixture/DeleteDistributionResponse.proto"
    cloudFront
    (Proxy :: Proxy DeleteDistribution)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource = res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    cloudFront
    (Proxy :: Proxy UntagResource)

responseListDistributionsByWebACLId :: ListDistributionsByWebACLIdResponse -> TestTree
responseListDistributionsByWebACLId = res
    "ListDistributionsByWebACLIdResponse"
    "fixture/ListDistributionsByWebACLIdResponse.proto"
    cloudFront
    (Proxy :: Proxy ListDistributionsByWebACLId)

responseListDistributions :: ListDistributionsResponse -> TestTree
responseListDistributions = res
    "ListDistributionsResponse"
    "fixture/ListDistributionsResponse.proto"
    cloudFront
    (Proxy :: Proxy ListDistributions)

responseListFieldLevelEncryptionProfiles :: ListFieldLevelEncryptionProfilesResponse -> TestTree
responseListFieldLevelEncryptionProfiles = res
    "ListFieldLevelEncryptionProfilesResponse"
    "fixture/ListFieldLevelEncryptionProfilesResponse.proto"
    cloudFront
    (Proxy :: Proxy ListFieldLevelEncryptionProfiles)
