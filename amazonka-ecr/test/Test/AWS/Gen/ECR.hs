{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ECR
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.ECR where

import Data.Proxy
import Network.AWS.ECR
import Test.AWS.ECR.Internal
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
--         [ requestUploadLayerPart $
--             newUploadLayerPart
--
--         , requestPutLifecyclePolicy $
--             newPutLifecyclePolicy
--
--         , requestPutRegistryPolicy $
--             newPutRegistryPolicy
--
--         , requestStartLifecyclePolicyPreview $
--             newStartLifecyclePolicyPreview
--
--         , requestDescribeRepositories $
--             newDescribeRepositories
--
--         , requestPutImage $
--             newPutImage
--
--         , requestListImages $
--             newListImages
--
--         , requestGetRegistryPolicy $
--             newGetRegistryPolicy
--
--         , requestInitiateLayerUpload $
--             newInitiateLayerUpload
--
--         , requestDescribeImageScanFindings $
--             newDescribeImageScanFindings
--
--         , requestDeleteRepositoryPolicy $
--             newDeleteRepositoryPolicy
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestTagResource $
--             newTagResource
--
--         , requestSetRepositoryPolicy $
--             newSetRepositoryPolicy
--
--         , requestDescribeRegistry $
--             newDescribeRegistry
--
--         , requestBatchDeleteImage $
--             newBatchDeleteImage
--
--         , requestPutImageScanningConfiguration $
--             newPutImageScanningConfiguration
--
--         , requestDeleteLifecyclePolicy $
--             newDeleteLifecyclePolicy
--
--         , requestDeleteRegistryPolicy $
--             newDeleteRegistryPolicy
--
--         , requestGetRepositoryPolicy $
--             newGetRepositoryPolicy
--
--         , requestCompleteLayerUpload $
--             newCompleteLayerUpload
--
--         , requestGetDownloadUrlForLayer $
--             newGetDownloadUrlForLayer
--
--         , requestDescribeImages $
--             newDescribeImages
--
--         , requestGetAuthorizationToken $
--             newGetAuthorizationToken
--
--         , requestCreateRepository $
--             newCreateRepository
--
--         , requestDeleteRepository $
--             newDeleteRepository
--
--         , requestGetLifecyclePolicy $
--             newGetLifecyclePolicy
--
--         , requestStartImageScan $
--             newStartImageScan
--
--         , requestBatchCheckLayerAvailability $
--             newBatchCheckLayerAvailability
--
--         , requestBatchGetImage $
--             newBatchGetImage
--
--         , requestPutReplicationConfiguration $
--             newPutReplicationConfiguration
--
--         , requestPutImageTagMutability $
--             newPutImageTagMutability
--
--         , requestDescribeImageReplicationStatus $
--             newDescribeImageReplicationStatus
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestGetLifecyclePolicyPreview $
--             newGetLifecyclePolicyPreview
--
--           ]

--     , testGroup "response"
--         [ responseUploadLayerPart $
--             newUploadLayerPartResponse
--
--         , responsePutLifecyclePolicy $
--             newPutLifecyclePolicyResponse
--
--         , responsePutRegistryPolicy $
--             newPutRegistryPolicyResponse
--
--         , responseStartLifecyclePolicyPreview $
--             newStartLifecyclePolicyPreviewResponse
--
--         , responseDescribeRepositories $
--             newDescribeRepositoriesResponse
--
--         , responsePutImage $
--             newPutImageResponse
--
--         , responseListImages $
--             newListImagesResponse
--
--         , responseGetRegistryPolicy $
--             newGetRegistryPolicyResponse
--
--         , responseInitiateLayerUpload $
--             newInitiateLayerUploadResponse
--
--         , responseDescribeImageScanFindings $
--             newDescribeImageScanFindingsResponse
--
--         , responseDeleteRepositoryPolicy $
--             newDeleteRepositoryPolicyResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseSetRepositoryPolicy $
--             newSetRepositoryPolicyResponse
--
--         , responseDescribeRegistry $
--             newDescribeRegistryResponse
--
--         , responseBatchDeleteImage $
--             newBatchDeleteImageResponse
--
--         , responsePutImageScanningConfiguration $
--             newPutImageScanningConfigurationResponse
--
--         , responseDeleteLifecyclePolicy $
--             newDeleteLifecyclePolicyResponse
--
--         , responseDeleteRegistryPolicy $
--             newDeleteRegistryPolicyResponse
--
--         , responseGetRepositoryPolicy $
--             newGetRepositoryPolicyResponse
--
--         , responseCompleteLayerUpload $
--             newCompleteLayerUploadResponse
--
--         , responseGetDownloadUrlForLayer $
--             newGetDownloadUrlForLayerResponse
--
--         , responseDescribeImages $
--             newDescribeImagesResponse
--
--         , responseGetAuthorizationToken $
--             newGetAuthorizationTokenResponse
--
--         , responseCreateRepository $
--             newCreateRepositoryResponse
--
--         , responseDeleteRepository $
--             newDeleteRepositoryResponse
--
--         , responseGetLifecyclePolicy $
--             newGetLifecyclePolicyResponse
--
--         , responseStartImageScan $
--             newStartImageScanResponse
--
--         , responseBatchCheckLayerAvailability $
--             newBatchCheckLayerAvailabilityResponse
--
--         , responseBatchGetImage $
--             newBatchGetImageResponse
--
--         , responsePutReplicationConfiguration $
--             newPutReplicationConfigurationResponse
--
--         , responsePutImageTagMutability $
--             newPutImageTagMutabilityResponse
--
--         , responseDescribeImageReplicationStatus $
--             newDescribeImageReplicationStatusResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseGetLifecyclePolicyPreview $
--             newGetLifecyclePolicyPreviewResponse
--
--           ]
--     ]

-- Requests

requestUploadLayerPart :: UploadLayerPart -> TestTree
requestUploadLayerPart =
  req
    "UploadLayerPart"
    "fixture/UploadLayerPart.yaml"

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

requestStartLifecyclePolicyPreview :: StartLifecyclePolicyPreview -> TestTree
requestStartLifecyclePolicyPreview =
  req
    "StartLifecyclePolicyPreview"
    "fixture/StartLifecyclePolicyPreview.yaml"

requestDescribeRepositories :: DescribeRepositories -> TestTree
requestDescribeRepositories =
  req
    "DescribeRepositories"
    "fixture/DescribeRepositories.yaml"

requestPutImage :: PutImage -> TestTree
requestPutImage =
  req
    "PutImage"
    "fixture/PutImage.yaml"

requestListImages :: ListImages -> TestTree
requestListImages =
  req
    "ListImages"
    "fixture/ListImages.yaml"

requestGetRegistryPolicy :: GetRegistryPolicy -> TestTree
requestGetRegistryPolicy =
  req
    "GetRegistryPolicy"
    "fixture/GetRegistryPolicy.yaml"

requestInitiateLayerUpload :: InitiateLayerUpload -> TestTree
requestInitiateLayerUpload =
  req
    "InitiateLayerUpload"
    "fixture/InitiateLayerUpload.yaml"

requestDescribeImageScanFindings :: DescribeImageScanFindings -> TestTree
requestDescribeImageScanFindings =
  req
    "DescribeImageScanFindings"
    "fixture/DescribeImageScanFindings.yaml"

requestDeleteRepositoryPolicy :: DeleteRepositoryPolicy -> TestTree
requestDeleteRepositoryPolicy =
  req
    "DeleteRepositoryPolicy"
    "fixture/DeleteRepositoryPolicy.yaml"

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

requestSetRepositoryPolicy :: SetRepositoryPolicy -> TestTree
requestSetRepositoryPolicy =
  req
    "SetRepositoryPolicy"
    "fixture/SetRepositoryPolicy.yaml"

requestDescribeRegistry :: DescribeRegistry -> TestTree
requestDescribeRegistry =
  req
    "DescribeRegistry"
    "fixture/DescribeRegistry.yaml"

requestBatchDeleteImage :: BatchDeleteImage -> TestTree
requestBatchDeleteImage =
  req
    "BatchDeleteImage"
    "fixture/BatchDeleteImage.yaml"

requestPutImageScanningConfiguration :: PutImageScanningConfiguration -> TestTree
requestPutImageScanningConfiguration =
  req
    "PutImageScanningConfiguration"
    "fixture/PutImageScanningConfiguration.yaml"

requestDeleteLifecyclePolicy :: DeleteLifecyclePolicy -> TestTree
requestDeleteLifecyclePolicy =
  req
    "DeleteLifecyclePolicy"
    "fixture/DeleteLifecyclePolicy.yaml"

requestDeleteRegistryPolicy :: DeleteRegistryPolicy -> TestTree
requestDeleteRegistryPolicy =
  req
    "DeleteRegistryPolicy"
    "fixture/DeleteRegistryPolicy.yaml"

requestGetRepositoryPolicy :: GetRepositoryPolicy -> TestTree
requestGetRepositoryPolicy =
  req
    "GetRepositoryPolicy"
    "fixture/GetRepositoryPolicy.yaml"

requestCompleteLayerUpload :: CompleteLayerUpload -> TestTree
requestCompleteLayerUpload =
  req
    "CompleteLayerUpload"
    "fixture/CompleteLayerUpload.yaml"

requestGetDownloadUrlForLayer :: GetDownloadUrlForLayer -> TestTree
requestGetDownloadUrlForLayer =
  req
    "GetDownloadUrlForLayer"
    "fixture/GetDownloadUrlForLayer.yaml"

requestDescribeImages :: DescribeImages -> TestTree
requestDescribeImages =
  req
    "DescribeImages"
    "fixture/DescribeImages.yaml"

requestGetAuthorizationToken :: GetAuthorizationToken -> TestTree
requestGetAuthorizationToken =
  req
    "GetAuthorizationToken"
    "fixture/GetAuthorizationToken.yaml"

requestCreateRepository :: CreateRepository -> TestTree
requestCreateRepository =
  req
    "CreateRepository"
    "fixture/CreateRepository.yaml"

requestDeleteRepository :: DeleteRepository -> TestTree
requestDeleteRepository =
  req
    "DeleteRepository"
    "fixture/DeleteRepository.yaml"

requestGetLifecyclePolicy :: GetLifecyclePolicy -> TestTree
requestGetLifecyclePolicy =
  req
    "GetLifecyclePolicy"
    "fixture/GetLifecyclePolicy.yaml"

requestStartImageScan :: StartImageScan -> TestTree
requestStartImageScan =
  req
    "StartImageScan"
    "fixture/StartImageScan.yaml"

requestBatchCheckLayerAvailability :: BatchCheckLayerAvailability -> TestTree
requestBatchCheckLayerAvailability =
  req
    "BatchCheckLayerAvailability"
    "fixture/BatchCheckLayerAvailability.yaml"

requestBatchGetImage :: BatchGetImage -> TestTree
requestBatchGetImage =
  req
    "BatchGetImage"
    "fixture/BatchGetImage.yaml"

requestPutReplicationConfiguration :: PutReplicationConfiguration -> TestTree
requestPutReplicationConfiguration =
  req
    "PutReplicationConfiguration"
    "fixture/PutReplicationConfiguration.yaml"

requestPutImageTagMutability :: PutImageTagMutability -> TestTree
requestPutImageTagMutability =
  req
    "PutImageTagMutability"
    "fixture/PutImageTagMutability.yaml"

requestDescribeImageReplicationStatus :: DescribeImageReplicationStatus -> TestTree
requestDescribeImageReplicationStatus =
  req
    "DescribeImageReplicationStatus"
    "fixture/DescribeImageReplicationStatus.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestGetLifecyclePolicyPreview :: GetLifecyclePolicyPreview -> TestTree
requestGetLifecyclePolicyPreview =
  req
    "GetLifecyclePolicyPreview"
    "fixture/GetLifecyclePolicyPreview.yaml"

-- Responses

responseUploadLayerPart :: UploadLayerPartResponse -> TestTree
responseUploadLayerPart =
  res
    "UploadLayerPartResponse"
    "fixture/UploadLayerPartResponse.proto"
    defaultService
    (Proxy :: Proxy UploadLayerPart)

responsePutLifecyclePolicy :: PutLifecyclePolicyResponse -> TestTree
responsePutLifecyclePolicy =
  res
    "PutLifecyclePolicyResponse"
    "fixture/PutLifecyclePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutLifecyclePolicy)

responsePutRegistryPolicy :: PutRegistryPolicyResponse -> TestTree
responsePutRegistryPolicy =
  res
    "PutRegistryPolicyResponse"
    "fixture/PutRegistryPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutRegistryPolicy)

responseStartLifecyclePolicyPreview :: StartLifecyclePolicyPreviewResponse -> TestTree
responseStartLifecyclePolicyPreview =
  res
    "StartLifecyclePolicyPreviewResponse"
    "fixture/StartLifecyclePolicyPreviewResponse.proto"
    defaultService
    (Proxy :: Proxy StartLifecyclePolicyPreview)

responseDescribeRepositories :: DescribeRepositoriesResponse -> TestTree
responseDescribeRepositories =
  res
    "DescribeRepositoriesResponse"
    "fixture/DescribeRepositoriesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeRepositories)

responsePutImage :: PutImageResponse -> TestTree
responsePutImage =
  res
    "PutImageResponse"
    "fixture/PutImageResponse.proto"
    defaultService
    (Proxy :: Proxy PutImage)

responseListImages :: ListImagesResponse -> TestTree
responseListImages =
  res
    "ListImagesResponse"
    "fixture/ListImagesResponse.proto"
    defaultService
    (Proxy :: Proxy ListImages)

responseGetRegistryPolicy :: GetRegistryPolicyResponse -> TestTree
responseGetRegistryPolicy =
  res
    "GetRegistryPolicyResponse"
    "fixture/GetRegistryPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetRegistryPolicy)

responseInitiateLayerUpload :: InitiateLayerUploadResponse -> TestTree
responseInitiateLayerUpload =
  res
    "InitiateLayerUploadResponse"
    "fixture/InitiateLayerUploadResponse.proto"
    defaultService
    (Proxy :: Proxy InitiateLayerUpload)

responseDescribeImageScanFindings :: DescribeImageScanFindingsResponse -> TestTree
responseDescribeImageScanFindings =
  res
    "DescribeImageScanFindingsResponse"
    "fixture/DescribeImageScanFindingsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeImageScanFindings)

responseDeleteRepositoryPolicy :: DeleteRepositoryPolicyResponse -> TestTree
responseDeleteRepositoryPolicy =
  res
    "DeleteRepositoryPolicyResponse"
    "fixture/DeleteRepositoryPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRepositoryPolicy)

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

responseSetRepositoryPolicy :: SetRepositoryPolicyResponse -> TestTree
responseSetRepositoryPolicy =
  res
    "SetRepositoryPolicyResponse"
    "fixture/SetRepositoryPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy SetRepositoryPolicy)

responseDescribeRegistry :: DescribeRegistryResponse -> TestTree
responseDescribeRegistry =
  res
    "DescribeRegistryResponse"
    "fixture/DescribeRegistryResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeRegistry)

responseBatchDeleteImage :: BatchDeleteImageResponse -> TestTree
responseBatchDeleteImage =
  res
    "BatchDeleteImageResponse"
    "fixture/BatchDeleteImageResponse.proto"
    defaultService
    (Proxy :: Proxy BatchDeleteImage)

responsePutImageScanningConfiguration :: PutImageScanningConfigurationResponse -> TestTree
responsePutImageScanningConfiguration =
  res
    "PutImageScanningConfigurationResponse"
    "fixture/PutImageScanningConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy PutImageScanningConfiguration)

responseDeleteLifecyclePolicy :: DeleteLifecyclePolicyResponse -> TestTree
responseDeleteLifecyclePolicy =
  res
    "DeleteLifecyclePolicyResponse"
    "fixture/DeleteLifecyclePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteLifecyclePolicy)

responseDeleteRegistryPolicy :: DeleteRegistryPolicyResponse -> TestTree
responseDeleteRegistryPolicy =
  res
    "DeleteRegistryPolicyResponse"
    "fixture/DeleteRegistryPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRegistryPolicy)

responseGetRepositoryPolicy :: GetRepositoryPolicyResponse -> TestTree
responseGetRepositoryPolicy =
  res
    "GetRepositoryPolicyResponse"
    "fixture/GetRepositoryPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetRepositoryPolicy)

responseCompleteLayerUpload :: CompleteLayerUploadResponse -> TestTree
responseCompleteLayerUpload =
  res
    "CompleteLayerUploadResponse"
    "fixture/CompleteLayerUploadResponse.proto"
    defaultService
    (Proxy :: Proxy CompleteLayerUpload)

responseGetDownloadUrlForLayer :: GetDownloadUrlForLayerResponse -> TestTree
responseGetDownloadUrlForLayer =
  res
    "GetDownloadUrlForLayerResponse"
    "fixture/GetDownloadUrlForLayerResponse.proto"
    defaultService
    (Proxy :: Proxy GetDownloadUrlForLayer)

responseDescribeImages :: DescribeImagesResponse -> TestTree
responseDescribeImages =
  res
    "DescribeImagesResponse"
    "fixture/DescribeImagesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeImages)

responseGetAuthorizationToken :: GetAuthorizationTokenResponse -> TestTree
responseGetAuthorizationToken =
  res
    "GetAuthorizationTokenResponse"
    "fixture/GetAuthorizationTokenResponse.proto"
    defaultService
    (Proxy :: Proxy GetAuthorizationToken)

responseCreateRepository :: CreateRepositoryResponse -> TestTree
responseCreateRepository =
  res
    "CreateRepositoryResponse"
    "fixture/CreateRepositoryResponse.proto"
    defaultService
    (Proxy :: Proxy CreateRepository)

responseDeleteRepository :: DeleteRepositoryResponse -> TestTree
responseDeleteRepository =
  res
    "DeleteRepositoryResponse"
    "fixture/DeleteRepositoryResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRepository)

responseGetLifecyclePolicy :: GetLifecyclePolicyResponse -> TestTree
responseGetLifecyclePolicy =
  res
    "GetLifecyclePolicyResponse"
    "fixture/GetLifecyclePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetLifecyclePolicy)

responseStartImageScan :: StartImageScanResponse -> TestTree
responseStartImageScan =
  res
    "StartImageScanResponse"
    "fixture/StartImageScanResponse.proto"
    defaultService
    (Proxy :: Proxy StartImageScan)

responseBatchCheckLayerAvailability :: BatchCheckLayerAvailabilityResponse -> TestTree
responseBatchCheckLayerAvailability =
  res
    "BatchCheckLayerAvailabilityResponse"
    "fixture/BatchCheckLayerAvailabilityResponse.proto"
    defaultService
    (Proxy :: Proxy BatchCheckLayerAvailability)

responseBatchGetImage :: BatchGetImageResponse -> TestTree
responseBatchGetImage =
  res
    "BatchGetImageResponse"
    "fixture/BatchGetImageResponse.proto"
    defaultService
    (Proxy :: Proxy BatchGetImage)

responsePutReplicationConfiguration :: PutReplicationConfigurationResponse -> TestTree
responsePutReplicationConfiguration =
  res
    "PutReplicationConfigurationResponse"
    "fixture/PutReplicationConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy PutReplicationConfiguration)

responsePutImageTagMutability :: PutImageTagMutabilityResponse -> TestTree
responsePutImageTagMutability =
  res
    "PutImageTagMutabilityResponse"
    "fixture/PutImageTagMutabilityResponse.proto"
    defaultService
    (Proxy :: Proxy PutImageTagMutability)

responseDescribeImageReplicationStatus :: DescribeImageReplicationStatusResponse -> TestTree
responseDescribeImageReplicationStatus =
  res
    "DescribeImageReplicationStatusResponse"
    "fixture/DescribeImageReplicationStatusResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeImageReplicationStatus)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseGetLifecyclePolicyPreview :: GetLifecyclePolicyPreviewResponse -> TestTree
responseGetLifecyclePolicyPreview =
  res
    "GetLifecyclePolicyPreviewResponse"
    "fixture/GetLifecyclePolicyPreviewResponse.proto"
    defaultService
    (Proxy :: Proxy GetLifecyclePolicyPreview)
