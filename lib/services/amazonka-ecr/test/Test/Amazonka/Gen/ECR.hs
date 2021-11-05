{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.ECR
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--         [ requestGetRepositoryPolicy $
--             newGetRepositoryPolicy
--
--         , requestPutImageScanningConfiguration $
--             newPutImageScanningConfiguration
--
--         , requestPutLifecyclePolicy $
--             newPutLifecyclePolicy
--
--         , requestDeleteLifecyclePolicy $
--             newDeleteLifecyclePolicy
--
--         , requestPutImageTagMutability $
--             newPutImageTagMutability
--
--         , requestBatchDeleteImage $
--             newBatchDeleteImage
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestGetLifecyclePolicyPreview $
--             newGetLifecyclePolicyPreview
--
--         , requestBatchCheckLayerAvailability $
--             newBatchCheckLayerAvailability
--
--         , requestDescribeRegistry $
--             newDescribeRegistry
--
--         , requestDeleteRepositoryPolicy $
--             newDeleteRepositoryPolicy
--
--         , requestCreateRepository $
--             newCreateRepository
--
--         , requestCompleteLayerUpload $
--             newCompleteLayerUpload
--
--         , requestDescribeRepositories $
--             newDescribeRepositories
--
--         , requestStartLifecyclePolicyPreview $
--             newStartLifecyclePolicyPreview
--
--         , requestDeleteRegistryPolicy $
--             newDeleteRegistryPolicy
--
--         , requestPutRegistryPolicy $
--             newPutRegistryPolicy
--
--         , requestUploadLayerPart $
--             newUploadLayerPart
--
--         , requestDescribeImageReplicationStatus $
--             newDescribeImageReplicationStatus
--
--         , requestBatchGetImage $
--             newBatchGetImage
--
--         , requestPutReplicationConfiguration $
--             newPutReplicationConfiguration
--
--         , requestStartImageScan $
--             newStartImageScan
--
--         , requestGetLifecyclePolicy $
--             newGetLifecyclePolicy
--
--         , requestTagResource $
--             newTagResource
--
--         , requestSetRepositoryPolicy $
--             newSetRepositoryPolicy
--
--         , requestDescribeImageScanFindings $
--             newDescribeImageScanFindings
--
--         , requestInitiateLayerUpload $
--             newInitiateLayerUpload
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDeleteRepository $
--             newDeleteRepository
--
--         , requestGetRegistryPolicy $
--             newGetRegistryPolicy
--
--         , requestPutImage $
--             newPutImage
--
--         , requestListImages $
--             newListImages
--
--         , requestGetAuthorizationToken $
--             newGetAuthorizationToken
--
--         , requestGetDownloadUrlForLayer $
--             newGetDownloadUrlForLayer
--
--         , requestDescribeImages $
--             newDescribeImages
--
--           ]

--     , testGroup "response"
--         [ responseGetRepositoryPolicy $
--             newGetRepositoryPolicyResponse
--
--         , responsePutImageScanningConfiguration $
--             newPutImageScanningConfigurationResponse
--
--         , responsePutLifecyclePolicy $
--             newPutLifecyclePolicyResponse
--
--         , responseDeleteLifecyclePolicy $
--             newDeleteLifecyclePolicyResponse
--
--         , responsePutImageTagMutability $
--             newPutImageTagMutabilityResponse
--
--         , responseBatchDeleteImage $
--             newBatchDeleteImageResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseGetLifecyclePolicyPreview $
--             newGetLifecyclePolicyPreviewResponse
--
--         , responseBatchCheckLayerAvailability $
--             newBatchCheckLayerAvailabilityResponse
--
--         , responseDescribeRegistry $
--             newDescribeRegistryResponse
--
--         , responseDeleteRepositoryPolicy $
--             newDeleteRepositoryPolicyResponse
--
--         , responseCreateRepository $
--             newCreateRepositoryResponse
--
--         , responseCompleteLayerUpload $
--             newCompleteLayerUploadResponse
--
--         , responseDescribeRepositories $
--             newDescribeRepositoriesResponse
--
--         , responseStartLifecyclePolicyPreview $
--             newStartLifecyclePolicyPreviewResponse
--
--         , responseDeleteRegistryPolicy $
--             newDeleteRegistryPolicyResponse
--
--         , responsePutRegistryPolicy $
--             newPutRegistryPolicyResponse
--
--         , responseUploadLayerPart $
--             newUploadLayerPartResponse
--
--         , responseDescribeImageReplicationStatus $
--             newDescribeImageReplicationStatusResponse
--
--         , responseBatchGetImage $
--             newBatchGetImageResponse
--
--         , responsePutReplicationConfiguration $
--             newPutReplicationConfigurationResponse
--
--         , responseStartImageScan $
--             newStartImageScanResponse
--
--         , responseGetLifecyclePolicy $
--             newGetLifecyclePolicyResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseSetRepositoryPolicy $
--             newSetRepositoryPolicyResponse
--
--         , responseDescribeImageScanFindings $
--             newDescribeImageScanFindingsResponse
--
--         , responseInitiateLayerUpload $
--             newInitiateLayerUploadResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDeleteRepository $
--             newDeleteRepositoryResponse
--
--         , responseGetRegistryPolicy $
--             newGetRegistryPolicyResponse
--
--         , responsePutImage $
--             newPutImageResponse
--
--         , responseListImages $
--             newListImagesResponse
--
--         , responseGetAuthorizationToken $
--             newGetAuthorizationTokenResponse
--
--         , responseGetDownloadUrlForLayer $
--             newGetDownloadUrlForLayerResponse
--
--         , responseDescribeImages $
--             newDescribeImagesResponse
--
--           ]
--     ]

-- Requests

requestGetRepositoryPolicy :: GetRepositoryPolicy -> TestTree
requestGetRepositoryPolicy =
  req
    "GetRepositoryPolicy"
    "fixture/GetRepositoryPolicy.yaml"

requestPutImageScanningConfiguration :: PutImageScanningConfiguration -> TestTree
requestPutImageScanningConfiguration =
  req
    "PutImageScanningConfiguration"
    "fixture/PutImageScanningConfiguration.yaml"

requestPutLifecyclePolicy :: PutLifecyclePolicy -> TestTree
requestPutLifecyclePolicy =
  req
    "PutLifecyclePolicy"
    "fixture/PutLifecyclePolicy.yaml"

requestDeleteLifecyclePolicy :: DeleteLifecyclePolicy -> TestTree
requestDeleteLifecyclePolicy =
  req
    "DeleteLifecyclePolicy"
    "fixture/DeleteLifecyclePolicy.yaml"

requestPutImageTagMutability :: PutImageTagMutability -> TestTree
requestPutImageTagMutability =
  req
    "PutImageTagMutability"
    "fixture/PutImageTagMutability.yaml"

requestBatchDeleteImage :: BatchDeleteImage -> TestTree
requestBatchDeleteImage =
  req
    "BatchDeleteImage"
    "fixture/BatchDeleteImage.yaml"

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

requestBatchCheckLayerAvailability :: BatchCheckLayerAvailability -> TestTree
requestBatchCheckLayerAvailability =
  req
    "BatchCheckLayerAvailability"
    "fixture/BatchCheckLayerAvailability.yaml"

requestDescribeRegistry :: DescribeRegistry -> TestTree
requestDescribeRegistry =
  req
    "DescribeRegistry"
    "fixture/DescribeRegistry.yaml"

requestDeleteRepositoryPolicy :: DeleteRepositoryPolicy -> TestTree
requestDeleteRepositoryPolicy =
  req
    "DeleteRepositoryPolicy"
    "fixture/DeleteRepositoryPolicy.yaml"

requestCreateRepository :: CreateRepository -> TestTree
requestCreateRepository =
  req
    "CreateRepository"
    "fixture/CreateRepository.yaml"

requestCompleteLayerUpload :: CompleteLayerUpload -> TestTree
requestCompleteLayerUpload =
  req
    "CompleteLayerUpload"
    "fixture/CompleteLayerUpload.yaml"

requestDescribeRepositories :: DescribeRepositories -> TestTree
requestDescribeRepositories =
  req
    "DescribeRepositories"
    "fixture/DescribeRepositories.yaml"

requestStartLifecyclePolicyPreview :: StartLifecyclePolicyPreview -> TestTree
requestStartLifecyclePolicyPreview =
  req
    "StartLifecyclePolicyPreview"
    "fixture/StartLifecyclePolicyPreview.yaml"

requestDeleteRegistryPolicy :: DeleteRegistryPolicy -> TestTree
requestDeleteRegistryPolicy =
  req
    "DeleteRegistryPolicy"
    "fixture/DeleteRegistryPolicy.yaml"

requestPutRegistryPolicy :: PutRegistryPolicy -> TestTree
requestPutRegistryPolicy =
  req
    "PutRegistryPolicy"
    "fixture/PutRegistryPolicy.yaml"

requestUploadLayerPart :: UploadLayerPart -> TestTree
requestUploadLayerPart =
  req
    "UploadLayerPart"
    "fixture/UploadLayerPart.yaml"

requestDescribeImageReplicationStatus :: DescribeImageReplicationStatus -> TestTree
requestDescribeImageReplicationStatus =
  req
    "DescribeImageReplicationStatus"
    "fixture/DescribeImageReplicationStatus.yaml"

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

requestStartImageScan :: StartImageScan -> TestTree
requestStartImageScan =
  req
    "StartImageScan"
    "fixture/StartImageScan.yaml"

requestGetLifecyclePolicy :: GetLifecyclePolicy -> TestTree
requestGetLifecyclePolicy =
  req
    "GetLifecyclePolicy"
    "fixture/GetLifecyclePolicy.yaml"

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

requestDescribeImageScanFindings :: DescribeImageScanFindings -> TestTree
requestDescribeImageScanFindings =
  req
    "DescribeImageScanFindings"
    "fixture/DescribeImageScanFindings.yaml"

requestInitiateLayerUpload :: InitiateLayerUpload -> TestTree
requestInitiateLayerUpload =
  req
    "InitiateLayerUpload"
    "fixture/InitiateLayerUpload.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDeleteRepository :: DeleteRepository -> TestTree
requestDeleteRepository =
  req
    "DeleteRepository"
    "fixture/DeleteRepository.yaml"

requestGetRegistryPolicy :: GetRegistryPolicy -> TestTree
requestGetRegistryPolicy =
  req
    "GetRegistryPolicy"
    "fixture/GetRegistryPolicy.yaml"

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

requestDescribeImages :: DescribeImages -> TestTree
requestDescribeImages =
  req
    "DescribeImages"
    "fixture/DescribeImages.yaml"

-- Responses

responseGetRepositoryPolicy :: GetRepositoryPolicyResponse -> TestTree
responseGetRepositoryPolicy =
  res
    "GetRepositoryPolicyResponse"
    "fixture/GetRepositoryPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRepositoryPolicy)

responsePutImageScanningConfiguration :: PutImageScanningConfigurationResponse -> TestTree
responsePutImageScanningConfiguration =
  res
    "PutImageScanningConfigurationResponse"
    "fixture/PutImageScanningConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutImageScanningConfiguration)

responsePutLifecyclePolicy :: PutLifecyclePolicyResponse -> TestTree
responsePutLifecyclePolicy =
  res
    "PutLifecyclePolicyResponse"
    "fixture/PutLifecyclePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutLifecyclePolicy)

responseDeleteLifecyclePolicy :: DeleteLifecyclePolicyResponse -> TestTree
responseDeleteLifecyclePolicy =
  res
    "DeleteLifecyclePolicyResponse"
    "fixture/DeleteLifecyclePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLifecyclePolicy)

responsePutImageTagMutability :: PutImageTagMutabilityResponse -> TestTree
responsePutImageTagMutability =
  res
    "PutImageTagMutabilityResponse"
    "fixture/PutImageTagMutabilityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutImageTagMutability)

responseBatchDeleteImage :: BatchDeleteImageResponse -> TestTree
responseBatchDeleteImage =
  res
    "BatchDeleteImageResponse"
    "fixture/BatchDeleteImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDeleteImage)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseGetLifecyclePolicyPreview :: GetLifecyclePolicyPreviewResponse -> TestTree
responseGetLifecyclePolicyPreview =
  res
    "GetLifecyclePolicyPreviewResponse"
    "fixture/GetLifecyclePolicyPreviewResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLifecyclePolicyPreview)

responseBatchCheckLayerAvailability :: BatchCheckLayerAvailabilityResponse -> TestTree
responseBatchCheckLayerAvailability =
  res
    "BatchCheckLayerAvailabilityResponse"
    "fixture/BatchCheckLayerAvailabilityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchCheckLayerAvailability)

responseDescribeRegistry :: DescribeRegistryResponse -> TestTree
responseDescribeRegistry =
  res
    "DescribeRegistryResponse"
    "fixture/DescribeRegistryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRegistry)

responseDeleteRepositoryPolicy :: DeleteRepositoryPolicyResponse -> TestTree
responseDeleteRepositoryPolicy =
  res
    "DeleteRepositoryPolicyResponse"
    "fixture/DeleteRepositoryPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRepositoryPolicy)

responseCreateRepository :: CreateRepositoryResponse -> TestTree
responseCreateRepository =
  res
    "CreateRepositoryResponse"
    "fixture/CreateRepositoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRepository)

responseCompleteLayerUpload :: CompleteLayerUploadResponse -> TestTree
responseCompleteLayerUpload =
  res
    "CompleteLayerUploadResponse"
    "fixture/CompleteLayerUploadResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CompleteLayerUpload)

responseDescribeRepositories :: DescribeRepositoriesResponse -> TestTree
responseDescribeRepositories =
  res
    "DescribeRepositoriesResponse"
    "fixture/DescribeRepositoriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRepositories)

responseStartLifecyclePolicyPreview :: StartLifecyclePolicyPreviewResponse -> TestTree
responseStartLifecyclePolicyPreview =
  res
    "StartLifecyclePolicyPreviewResponse"
    "fixture/StartLifecyclePolicyPreviewResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartLifecyclePolicyPreview)

responseDeleteRegistryPolicy :: DeleteRegistryPolicyResponse -> TestTree
responseDeleteRegistryPolicy =
  res
    "DeleteRegistryPolicyResponse"
    "fixture/DeleteRegistryPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRegistryPolicy)

responsePutRegistryPolicy :: PutRegistryPolicyResponse -> TestTree
responsePutRegistryPolicy =
  res
    "PutRegistryPolicyResponse"
    "fixture/PutRegistryPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutRegistryPolicy)

responseUploadLayerPart :: UploadLayerPartResponse -> TestTree
responseUploadLayerPart =
  res
    "UploadLayerPartResponse"
    "fixture/UploadLayerPartResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UploadLayerPart)

responseDescribeImageReplicationStatus :: DescribeImageReplicationStatusResponse -> TestTree
responseDescribeImageReplicationStatus =
  res
    "DescribeImageReplicationStatusResponse"
    "fixture/DescribeImageReplicationStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeImageReplicationStatus)

responseBatchGetImage :: BatchGetImageResponse -> TestTree
responseBatchGetImage =
  res
    "BatchGetImageResponse"
    "fixture/BatchGetImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetImage)

responsePutReplicationConfiguration :: PutReplicationConfigurationResponse -> TestTree
responsePutReplicationConfiguration =
  res
    "PutReplicationConfigurationResponse"
    "fixture/PutReplicationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutReplicationConfiguration)

responseStartImageScan :: StartImageScanResponse -> TestTree
responseStartImageScan =
  res
    "StartImageScanResponse"
    "fixture/StartImageScanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartImageScan)

responseGetLifecyclePolicy :: GetLifecyclePolicyResponse -> TestTree
responseGetLifecyclePolicy =
  res
    "GetLifecyclePolicyResponse"
    "fixture/GetLifecyclePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLifecyclePolicy)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseSetRepositoryPolicy :: SetRepositoryPolicyResponse -> TestTree
responseSetRepositoryPolicy =
  res
    "SetRepositoryPolicyResponse"
    "fixture/SetRepositoryPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetRepositoryPolicy)

responseDescribeImageScanFindings :: DescribeImageScanFindingsResponse -> TestTree
responseDescribeImageScanFindings =
  res
    "DescribeImageScanFindingsResponse"
    "fixture/DescribeImageScanFindingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeImageScanFindings)

responseInitiateLayerUpload :: InitiateLayerUploadResponse -> TestTree
responseInitiateLayerUpload =
  res
    "InitiateLayerUploadResponse"
    "fixture/InitiateLayerUploadResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy InitiateLayerUpload)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseDeleteRepository :: DeleteRepositoryResponse -> TestTree
responseDeleteRepository =
  res
    "DeleteRepositoryResponse"
    "fixture/DeleteRepositoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRepository)

responseGetRegistryPolicy :: GetRegistryPolicyResponse -> TestTree
responseGetRegistryPolicy =
  res
    "GetRegistryPolicyResponse"
    "fixture/GetRegistryPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRegistryPolicy)

responsePutImage :: PutImageResponse -> TestTree
responsePutImage =
  res
    "PutImageResponse"
    "fixture/PutImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutImage)

responseListImages :: ListImagesResponse -> TestTree
responseListImages =
  res
    "ListImagesResponse"
    "fixture/ListImagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListImages)

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

responseDescribeImages :: DescribeImagesResponse -> TestTree
responseDescribeImages =
  res
    "DescribeImagesResponse"
    "fixture/DescribeImagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeImages)
