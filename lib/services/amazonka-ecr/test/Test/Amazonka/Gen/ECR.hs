{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.ECR
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.ECR where

import Amazonka.ECR
import qualified Data.Proxy as Proxy
import Test.Amazonka.ECR.Internal
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
--         [ requestBatchCheckLayerAvailability $
--             newBatchCheckLayerAvailability
--
--         , requestBatchDeleteImage $
--             newBatchDeleteImage
--
--         , requestBatchGetImage $
--             newBatchGetImage
--
--         , requestBatchGetRepositoryScanningConfiguration $
--             newBatchGetRepositoryScanningConfiguration
--
--         , requestCompleteLayerUpload $
--             newCompleteLayerUpload
--
--         , requestCreatePullThroughCacheRule $
--             newCreatePullThroughCacheRule
--
--         , requestCreateRepository $
--             newCreateRepository
--
--         , requestDeleteLifecyclePolicy $
--             newDeleteLifecyclePolicy
--
--         , requestDeletePullThroughCacheRule $
--             newDeletePullThroughCacheRule
--
--         , requestDeleteRegistryPolicy $
--             newDeleteRegistryPolicy
--
--         , requestDeleteRepository $
--             newDeleteRepository
--
--         , requestDeleteRepositoryPolicy $
--             newDeleteRepositoryPolicy
--
--         , requestDescribeImageReplicationStatus $
--             newDescribeImageReplicationStatus
--
--         , requestDescribeImageScanFindings $
--             newDescribeImageScanFindings
--
--         , requestDescribeImages $
--             newDescribeImages
--
--         , requestDescribePullThroughCacheRules $
--             newDescribePullThroughCacheRules
--
--         , requestDescribeRegistry $
--             newDescribeRegistry
--
--         , requestDescribeRepositories $
--             newDescribeRepositories
--
--         , requestGetAuthorizationToken $
--             newGetAuthorizationToken
--
--         , requestGetDownloadUrlForLayer $
--             newGetDownloadUrlForLayer
--
--         , requestGetLifecyclePolicy $
--             newGetLifecyclePolicy
--
--         , requestGetLifecyclePolicyPreview $
--             newGetLifecyclePolicyPreview
--
--         , requestGetRegistryPolicy $
--             newGetRegistryPolicy
--
--         , requestGetRegistryScanningConfiguration $
--             newGetRegistryScanningConfiguration
--
--         , requestGetRepositoryPolicy $
--             newGetRepositoryPolicy
--
--         , requestInitiateLayerUpload $
--             newInitiateLayerUpload
--
--         , requestListImages $
--             newListImages
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPutImage $
--             newPutImage
--
--         , requestPutImageScanningConfiguration $
--             newPutImageScanningConfiguration
--
--         , requestPutImageTagMutability $
--             newPutImageTagMutability
--
--         , requestPutLifecyclePolicy $
--             newPutLifecyclePolicy
--
--         , requestPutRegistryPolicy $
--             newPutRegistryPolicy
--
--         , requestPutRegistryScanningConfiguration $
--             newPutRegistryScanningConfiguration
--
--         , requestPutReplicationConfiguration $
--             newPutReplicationConfiguration
--
--         , requestSetRepositoryPolicy $
--             newSetRepositoryPolicy
--
--         , requestStartImageScan $
--             newStartImageScan
--
--         , requestStartLifecyclePolicyPreview $
--             newStartLifecyclePolicyPreview
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUploadLayerPart $
--             newUploadLayerPart
--
--           ]

--     , testGroup "response"
--         [ responseBatchCheckLayerAvailability $
--             newBatchCheckLayerAvailabilityResponse
--
--         , responseBatchDeleteImage $
--             newBatchDeleteImageResponse
--
--         , responseBatchGetImage $
--             newBatchGetImageResponse
--
--         , responseBatchGetRepositoryScanningConfiguration $
--             newBatchGetRepositoryScanningConfigurationResponse
--
--         , responseCompleteLayerUpload $
--             newCompleteLayerUploadResponse
--
--         , responseCreatePullThroughCacheRule $
--             newCreatePullThroughCacheRuleResponse
--
--         , responseCreateRepository $
--             newCreateRepositoryResponse
--
--         , responseDeleteLifecyclePolicy $
--             newDeleteLifecyclePolicyResponse
--
--         , responseDeletePullThroughCacheRule $
--             newDeletePullThroughCacheRuleResponse
--
--         , responseDeleteRegistryPolicy $
--             newDeleteRegistryPolicyResponse
--
--         , responseDeleteRepository $
--             newDeleteRepositoryResponse
--
--         , responseDeleteRepositoryPolicy $
--             newDeleteRepositoryPolicyResponse
--
--         , responseDescribeImageReplicationStatus $
--             newDescribeImageReplicationStatusResponse
--
--         , responseDescribeImageScanFindings $
--             newDescribeImageScanFindingsResponse
--
--         , responseDescribeImages $
--             newDescribeImagesResponse
--
--         , responseDescribePullThroughCacheRules $
--             newDescribePullThroughCacheRulesResponse
--
--         , responseDescribeRegistry $
--             newDescribeRegistryResponse
--
--         , responseDescribeRepositories $
--             newDescribeRepositoriesResponse
--
--         , responseGetAuthorizationToken $
--             newGetAuthorizationTokenResponse
--
--         , responseGetDownloadUrlForLayer $
--             newGetDownloadUrlForLayerResponse
--
--         , responseGetLifecyclePolicy $
--             newGetLifecyclePolicyResponse
--
--         , responseGetLifecyclePolicyPreview $
--             newGetLifecyclePolicyPreviewResponse
--
--         , responseGetRegistryPolicy $
--             newGetRegistryPolicyResponse
--
--         , responseGetRegistryScanningConfiguration $
--             newGetRegistryScanningConfigurationResponse
--
--         , responseGetRepositoryPolicy $
--             newGetRepositoryPolicyResponse
--
--         , responseInitiateLayerUpload $
--             newInitiateLayerUploadResponse
--
--         , responseListImages $
--             newListImagesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePutImage $
--             newPutImageResponse
--
--         , responsePutImageScanningConfiguration $
--             newPutImageScanningConfigurationResponse
--
--         , responsePutImageTagMutability $
--             newPutImageTagMutabilityResponse
--
--         , responsePutLifecyclePolicy $
--             newPutLifecyclePolicyResponse
--
--         , responsePutRegistryPolicy $
--             newPutRegistryPolicyResponse
--
--         , responsePutRegistryScanningConfiguration $
--             newPutRegistryScanningConfigurationResponse
--
--         , responsePutReplicationConfiguration $
--             newPutReplicationConfigurationResponse
--
--         , responseSetRepositoryPolicy $
--             newSetRepositoryPolicyResponse
--
--         , responseStartImageScan $
--             newStartImageScanResponse
--
--         , responseStartLifecyclePolicyPreview $
--             newStartLifecyclePolicyPreviewResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUploadLayerPart $
--             newUploadLayerPartResponse
--
--           ]
--     ]

-- Requests

requestBatchCheckLayerAvailability :: BatchCheckLayerAvailability -> TestTree
requestBatchCheckLayerAvailability =
  req
    "BatchCheckLayerAvailability"
    "fixture/BatchCheckLayerAvailability.yaml"

requestBatchDeleteImage :: BatchDeleteImage -> TestTree
requestBatchDeleteImage =
  req
    "BatchDeleteImage"
    "fixture/BatchDeleteImage.yaml"

requestBatchGetImage :: BatchGetImage -> TestTree
requestBatchGetImage =
  req
    "BatchGetImage"
    "fixture/BatchGetImage.yaml"

requestBatchGetRepositoryScanningConfiguration :: BatchGetRepositoryScanningConfiguration -> TestTree
requestBatchGetRepositoryScanningConfiguration =
  req
    "BatchGetRepositoryScanningConfiguration"
    "fixture/BatchGetRepositoryScanningConfiguration.yaml"

requestCompleteLayerUpload :: CompleteLayerUpload -> TestTree
requestCompleteLayerUpload =
  req
    "CompleteLayerUpload"
    "fixture/CompleteLayerUpload.yaml"

requestCreatePullThroughCacheRule :: CreatePullThroughCacheRule -> TestTree
requestCreatePullThroughCacheRule =
  req
    "CreatePullThroughCacheRule"
    "fixture/CreatePullThroughCacheRule.yaml"

requestCreateRepository :: CreateRepository -> TestTree
requestCreateRepository =
  req
    "CreateRepository"
    "fixture/CreateRepository.yaml"

requestDeleteLifecyclePolicy :: DeleteLifecyclePolicy -> TestTree
requestDeleteLifecyclePolicy =
  req
    "DeleteLifecyclePolicy"
    "fixture/DeleteLifecyclePolicy.yaml"

requestDeletePullThroughCacheRule :: DeletePullThroughCacheRule -> TestTree
requestDeletePullThroughCacheRule =
  req
    "DeletePullThroughCacheRule"
    "fixture/DeletePullThroughCacheRule.yaml"

requestDeleteRegistryPolicy :: DeleteRegistryPolicy -> TestTree
requestDeleteRegistryPolicy =
  req
    "DeleteRegistryPolicy"
    "fixture/DeleteRegistryPolicy.yaml"

requestDeleteRepository :: DeleteRepository -> TestTree
requestDeleteRepository =
  req
    "DeleteRepository"
    "fixture/DeleteRepository.yaml"

requestDeleteRepositoryPolicy :: DeleteRepositoryPolicy -> TestTree
requestDeleteRepositoryPolicy =
  req
    "DeleteRepositoryPolicy"
    "fixture/DeleteRepositoryPolicy.yaml"

requestDescribeImageReplicationStatus :: DescribeImageReplicationStatus -> TestTree
requestDescribeImageReplicationStatus =
  req
    "DescribeImageReplicationStatus"
    "fixture/DescribeImageReplicationStatus.yaml"

requestDescribeImageScanFindings :: DescribeImageScanFindings -> TestTree
requestDescribeImageScanFindings =
  req
    "DescribeImageScanFindings"
    "fixture/DescribeImageScanFindings.yaml"

requestDescribeImages :: DescribeImages -> TestTree
requestDescribeImages =
  req
    "DescribeImages"
    "fixture/DescribeImages.yaml"

requestDescribePullThroughCacheRules :: DescribePullThroughCacheRules -> TestTree
requestDescribePullThroughCacheRules =
  req
    "DescribePullThroughCacheRules"
    "fixture/DescribePullThroughCacheRules.yaml"

requestDescribeRegistry :: DescribeRegistry -> TestTree
requestDescribeRegistry =
  req
    "DescribeRegistry"
    "fixture/DescribeRegistry.yaml"

requestDescribeRepositories :: DescribeRepositories -> TestTree
requestDescribeRepositories =
  req
    "DescribeRepositories"
    "fixture/DescribeRepositories.yaml"

requestGetAuthorizationToken :: GetAuthorizationToken -> TestTree
requestGetAuthorizationToken =
  req
    "GetAuthorizationToken"
    "fixture/GetAuthorizationToken.yaml"

requestGetDownloadUrlForLayer :: GetDownloadUrlForLayer -> TestTree
requestGetDownloadUrlForLayer =
  req
    "GetDownloadUrlForLayer"
    "fixture/GetDownloadUrlForLayer.yaml"

requestGetLifecyclePolicy :: GetLifecyclePolicy -> TestTree
requestGetLifecyclePolicy =
  req
    "GetLifecyclePolicy"
    "fixture/GetLifecyclePolicy.yaml"

requestGetLifecyclePolicyPreview :: GetLifecyclePolicyPreview -> TestTree
requestGetLifecyclePolicyPreview =
  req
    "GetLifecyclePolicyPreview"
    "fixture/GetLifecyclePolicyPreview.yaml"

requestGetRegistryPolicy :: GetRegistryPolicy -> TestTree
requestGetRegistryPolicy =
  req
    "GetRegistryPolicy"
    "fixture/GetRegistryPolicy.yaml"

requestGetRegistryScanningConfiguration :: GetRegistryScanningConfiguration -> TestTree
requestGetRegistryScanningConfiguration =
  req
    "GetRegistryScanningConfiguration"
    "fixture/GetRegistryScanningConfiguration.yaml"

requestGetRepositoryPolicy :: GetRepositoryPolicy -> TestTree
requestGetRepositoryPolicy =
  req
    "GetRepositoryPolicy"
    "fixture/GetRepositoryPolicy.yaml"

requestInitiateLayerUpload :: InitiateLayerUpload -> TestTree
requestInitiateLayerUpload =
  req
    "InitiateLayerUpload"
    "fixture/InitiateLayerUpload.yaml"

requestListImages :: ListImages -> TestTree
requestListImages =
  req
    "ListImages"
    "fixture/ListImages.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPutImage :: PutImage -> TestTree
requestPutImage =
  req
    "PutImage"
    "fixture/PutImage.yaml"

requestPutImageScanningConfiguration :: PutImageScanningConfiguration -> TestTree
requestPutImageScanningConfiguration =
  req
    "PutImageScanningConfiguration"
    "fixture/PutImageScanningConfiguration.yaml"

requestPutImageTagMutability :: PutImageTagMutability -> TestTree
requestPutImageTagMutability =
  req
    "PutImageTagMutability"
    "fixture/PutImageTagMutability.yaml"

requestPutLifecyclePolicy :: PutLifecyclePolicy -> TestTree
requestPutLifecyclePolicy =
  req
    "PutLifecyclePolicy"
    "fixture/PutLifecyclePolicy.yaml"

requestPutRegistryPolicy :: PutRegistryPolicy -> TestTree
requestPutRegistryPolicy =
  req
    "PutRegistryPolicy"
    "fixture/PutRegistryPolicy.yaml"

requestPutRegistryScanningConfiguration :: PutRegistryScanningConfiguration -> TestTree
requestPutRegistryScanningConfiguration =
  req
    "PutRegistryScanningConfiguration"
    "fixture/PutRegistryScanningConfiguration.yaml"

requestPutReplicationConfiguration :: PutReplicationConfiguration -> TestTree
requestPutReplicationConfiguration =
  req
    "PutReplicationConfiguration"
    "fixture/PutReplicationConfiguration.yaml"

requestSetRepositoryPolicy :: SetRepositoryPolicy -> TestTree
requestSetRepositoryPolicy =
  req
    "SetRepositoryPolicy"
    "fixture/SetRepositoryPolicy.yaml"

requestStartImageScan :: StartImageScan -> TestTree
requestStartImageScan =
  req
    "StartImageScan"
    "fixture/StartImageScan.yaml"

requestStartLifecyclePolicyPreview :: StartLifecyclePolicyPreview -> TestTree
requestStartLifecyclePolicyPreview =
  req
    "StartLifecyclePolicyPreview"
    "fixture/StartLifecyclePolicyPreview.yaml"

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

requestUploadLayerPart :: UploadLayerPart -> TestTree
requestUploadLayerPart =
  req
    "UploadLayerPart"
    "fixture/UploadLayerPart.yaml"

-- Responses

responseBatchCheckLayerAvailability :: BatchCheckLayerAvailabilityResponse -> TestTree
responseBatchCheckLayerAvailability =
  res
    "BatchCheckLayerAvailabilityResponse"
    "fixture/BatchCheckLayerAvailabilityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchCheckLayerAvailability)

responseBatchDeleteImage :: BatchDeleteImageResponse -> TestTree
responseBatchDeleteImage =
  res
    "BatchDeleteImageResponse"
    "fixture/BatchDeleteImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDeleteImage)

responseBatchGetImage :: BatchGetImageResponse -> TestTree
responseBatchGetImage =
  res
    "BatchGetImageResponse"
    "fixture/BatchGetImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetImage)

responseBatchGetRepositoryScanningConfiguration :: BatchGetRepositoryScanningConfigurationResponse -> TestTree
responseBatchGetRepositoryScanningConfiguration =
  res
    "BatchGetRepositoryScanningConfigurationResponse"
    "fixture/BatchGetRepositoryScanningConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetRepositoryScanningConfiguration)

responseCompleteLayerUpload :: CompleteLayerUploadResponse -> TestTree
responseCompleteLayerUpload =
  res
    "CompleteLayerUploadResponse"
    "fixture/CompleteLayerUploadResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CompleteLayerUpload)

responseCreatePullThroughCacheRule :: CreatePullThroughCacheRuleResponse -> TestTree
responseCreatePullThroughCacheRule =
  res
    "CreatePullThroughCacheRuleResponse"
    "fixture/CreatePullThroughCacheRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePullThroughCacheRule)

responseCreateRepository :: CreateRepositoryResponse -> TestTree
responseCreateRepository =
  res
    "CreateRepositoryResponse"
    "fixture/CreateRepositoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRepository)

responseDeleteLifecyclePolicy :: DeleteLifecyclePolicyResponse -> TestTree
responseDeleteLifecyclePolicy =
  res
    "DeleteLifecyclePolicyResponse"
    "fixture/DeleteLifecyclePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLifecyclePolicy)

responseDeletePullThroughCacheRule :: DeletePullThroughCacheRuleResponse -> TestTree
responseDeletePullThroughCacheRule =
  res
    "DeletePullThroughCacheRuleResponse"
    "fixture/DeletePullThroughCacheRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePullThroughCacheRule)

responseDeleteRegistryPolicy :: DeleteRegistryPolicyResponse -> TestTree
responseDeleteRegistryPolicy =
  res
    "DeleteRegistryPolicyResponse"
    "fixture/DeleteRegistryPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRegistryPolicy)

responseDeleteRepository :: DeleteRepositoryResponse -> TestTree
responseDeleteRepository =
  res
    "DeleteRepositoryResponse"
    "fixture/DeleteRepositoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRepository)

responseDeleteRepositoryPolicy :: DeleteRepositoryPolicyResponse -> TestTree
responseDeleteRepositoryPolicy =
  res
    "DeleteRepositoryPolicyResponse"
    "fixture/DeleteRepositoryPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRepositoryPolicy)

responseDescribeImageReplicationStatus :: DescribeImageReplicationStatusResponse -> TestTree
responseDescribeImageReplicationStatus =
  res
    "DescribeImageReplicationStatusResponse"
    "fixture/DescribeImageReplicationStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeImageReplicationStatus)

responseDescribeImageScanFindings :: DescribeImageScanFindingsResponse -> TestTree
responseDescribeImageScanFindings =
  res
    "DescribeImageScanFindingsResponse"
    "fixture/DescribeImageScanFindingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeImageScanFindings)

responseDescribeImages :: DescribeImagesResponse -> TestTree
responseDescribeImages =
  res
    "DescribeImagesResponse"
    "fixture/DescribeImagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeImages)

responseDescribePullThroughCacheRules :: DescribePullThroughCacheRulesResponse -> TestTree
responseDescribePullThroughCacheRules =
  res
    "DescribePullThroughCacheRulesResponse"
    "fixture/DescribePullThroughCacheRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePullThroughCacheRules)

responseDescribeRegistry :: DescribeRegistryResponse -> TestTree
responseDescribeRegistry =
  res
    "DescribeRegistryResponse"
    "fixture/DescribeRegistryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRegistry)

responseDescribeRepositories :: DescribeRepositoriesResponse -> TestTree
responseDescribeRepositories =
  res
    "DescribeRepositoriesResponse"
    "fixture/DescribeRepositoriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRepositories)

responseGetAuthorizationToken :: GetAuthorizationTokenResponse -> TestTree
responseGetAuthorizationToken =
  res
    "GetAuthorizationTokenResponse"
    "fixture/GetAuthorizationTokenResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAuthorizationToken)

responseGetDownloadUrlForLayer :: GetDownloadUrlForLayerResponse -> TestTree
responseGetDownloadUrlForLayer =
  res
    "GetDownloadUrlForLayerResponse"
    "fixture/GetDownloadUrlForLayerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDownloadUrlForLayer)

responseGetLifecyclePolicy :: GetLifecyclePolicyResponse -> TestTree
responseGetLifecyclePolicy =
  res
    "GetLifecyclePolicyResponse"
    "fixture/GetLifecyclePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLifecyclePolicy)

responseGetLifecyclePolicyPreview :: GetLifecyclePolicyPreviewResponse -> TestTree
responseGetLifecyclePolicyPreview =
  res
    "GetLifecyclePolicyPreviewResponse"
    "fixture/GetLifecyclePolicyPreviewResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLifecyclePolicyPreview)

responseGetRegistryPolicy :: GetRegistryPolicyResponse -> TestTree
responseGetRegistryPolicy =
  res
    "GetRegistryPolicyResponse"
    "fixture/GetRegistryPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRegistryPolicy)

responseGetRegistryScanningConfiguration :: GetRegistryScanningConfigurationResponse -> TestTree
responseGetRegistryScanningConfiguration =
  res
    "GetRegistryScanningConfigurationResponse"
    "fixture/GetRegistryScanningConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRegistryScanningConfiguration)

responseGetRepositoryPolicy :: GetRepositoryPolicyResponse -> TestTree
responseGetRepositoryPolicy =
  res
    "GetRepositoryPolicyResponse"
    "fixture/GetRepositoryPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRepositoryPolicy)

responseInitiateLayerUpload :: InitiateLayerUploadResponse -> TestTree
responseInitiateLayerUpload =
  res
    "InitiateLayerUploadResponse"
    "fixture/InitiateLayerUploadResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy InitiateLayerUpload)

responseListImages :: ListImagesResponse -> TestTree
responseListImages =
  res
    "ListImagesResponse"
    "fixture/ListImagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListImages)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePutImage :: PutImageResponse -> TestTree
responsePutImage =
  res
    "PutImageResponse"
    "fixture/PutImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutImage)

responsePutImageScanningConfiguration :: PutImageScanningConfigurationResponse -> TestTree
responsePutImageScanningConfiguration =
  res
    "PutImageScanningConfigurationResponse"
    "fixture/PutImageScanningConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutImageScanningConfiguration)

responsePutImageTagMutability :: PutImageTagMutabilityResponse -> TestTree
responsePutImageTagMutability =
  res
    "PutImageTagMutabilityResponse"
    "fixture/PutImageTagMutabilityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutImageTagMutability)

responsePutLifecyclePolicy :: PutLifecyclePolicyResponse -> TestTree
responsePutLifecyclePolicy =
  res
    "PutLifecyclePolicyResponse"
    "fixture/PutLifecyclePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutLifecyclePolicy)

responsePutRegistryPolicy :: PutRegistryPolicyResponse -> TestTree
responsePutRegistryPolicy =
  res
    "PutRegistryPolicyResponse"
    "fixture/PutRegistryPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutRegistryPolicy)

responsePutRegistryScanningConfiguration :: PutRegistryScanningConfigurationResponse -> TestTree
responsePutRegistryScanningConfiguration =
  res
    "PutRegistryScanningConfigurationResponse"
    "fixture/PutRegistryScanningConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutRegistryScanningConfiguration)

responsePutReplicationConfiguration :: PutReplicationConfigurationResponse -> TestTree
responsePutReplicationConfiguration =
  res
    "PutReplicationConfigurationResponse"
    "fixture/PutReplicationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutReplicationConfiguration)

responseSetRepositoryPolicy :: SetRepositoryPolicyResponse -> TestTree
responseSetRepositoryPolicy =
  res
    "SetRepositoryPolicyResponse"
    "fixture/SetRepositoryPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetRepositoryPolicy)

responseStartImageScan :: StartImageScanResponse -> TestTree
responseStartImageScan =
  res
    "StartImageScanResponse"
    "fixture/StartImageScanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartImageScan)

responseStartLifecyclePolicyPreview :: StartLifecyclePolicyPreviewResponse -> TestTree
responseStartLifecyclePolicyPreview =
  res
    "StartLifecyclePolicyPreviewResponse"
    "fixture/StartLifecyclePolicyPreviewResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartLifecyclePolicyPreview)

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

responseUploadLayerPart :: UploadLayerPartResponse -> TestTree
responseUploadLayerPart =
  res
    "UploadLayerPartResponse"
    "fixture/UploadLayerPartResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UploadLayerPart)
