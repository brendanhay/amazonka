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
--         , requestListImages $
--             newListImages
--
--         , requestPutImage $
--             newPutImage
--
--         , requestInitiateLayerUpload $
--             newInitiateLayerUpload
--
--         , requestGetRegistryPolicy $
--             newGetRegistryPolicy
--
--         , requestDeleteRepositoryPolicy $
--             newDeleteRepositoryPolicy
--
--         , requestDescribeImageScanFindings $
--             newDescribeImageScanFindings
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestSetRepositoryPolicy $
--             newSetRepositoryPolicy
--
--         , requestDescribeRegistry $
--             newDescribeRegistry
--
--         , requestTagResource $
--             newTagResource
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
--         , requestDescribeImages $
--             newDescribeImages
--
--         , requestGetDownloadUrlForLayer $
--             newGetDownloadUrlForLayer
--
--         , requestCompleteLayerUpload $
--             newCompleteLayerUpload
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
--         , requestBatchCheckLayerAvailability $
--             newBatchCheckLayerAvailability
--
--         , requestGetLifecyclePolicy $
--             newGetLifecyclePolicy
--
--         , requestStartImageScan $
--             newStartImageScan
--
--         , requestPutReplicationConfiguration $
--             newPutReplicationConfiguration
--
--         , requestBatchGetImage $
--             newBatchGetImage
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPutImageTagMutability $
--             newPutImageTagMutability
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
--         , responseListImages $
--             newListImagesResponse
--
--         , responsePutImage $
--             newPutImageResponse
--
--         , responseInitiateLayerUpload $
--             newInitiateLayerUploadResponse
--
--         , responseGetRegistryPolicy $
--             newGetRegistryPolicyResponse
--
--         , responseDeleteRepositoryPolicy $
--             newDeleteRepositoryPolicyResponse
--
--         , responseDescribeImageScanFindings $
--             newDescribeImageScanFindingsResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseSetRepositoryPolicy $
--             newSetRepositoryPolicyResponse
--
--         , responseDescribeRegistry $
--             newDescribeRegistryResponse
--
--         , responseTagResource $
--             newTagResourceResponse
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
--         , responseDescribeImages $
--             newDescribeImagesResponse
--
--         , responseGetDownloadUrlForLayer $
--             newGetDownloadUrlForLayerResponse
--
--         , responseCompleteLayerUpload $
--             newCompleteLayerUploadResponse
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
--         , responseBatchCheckLayerAvailability $
--             newBatchCheckLayerAvailabilityResponse
--
--         , responseGetLifecyclePolicy $
--             newGetLifecyclePolicyResponse
--
--         , responseStartImageScan $
--             newStartImageScanResponse
--
--         , responsePutReplicationConfiguration $
--             newPutReplicationConfigurationResponse
--
--         , responseBatchGetImage $
--             newBatchGetImageResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePutImageTagMutability $
--             newPutImageTagMutabilityResponse
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

requestListImages :: ListImages -> TestTree
requestListImages =
  req
    "ListImages"
    "fixture/ListImages.yaml"

requestPutImage :: PutImage -> TestTree
requestPutImage =
  req
    "PutImage"
    "fixture/PutImage.yaml"

requestInitiateLayerUpload :: InitiateLayerUpload -> TestTree
requestInitiateLayerUpload =
  req
    "InitiateLayerUpload"
    "fixture/InitiateLayerUpload.yaml"

requestGetRegistryPolicy :: GetRegistryPolicy -> TestTree
requestGetRegistryPolicy =
  req
    "GetRegistryPolicy"
    "fixture/GetRegistryPolicy.yaml"

requestDeleteRepositoryPolicy :: DeleteRepositoryPolicy -> TestTree
requestDeleteRepositoryPolicy =
  req
    "DeleteRepositoryPolicy"
    "fixture/DeleteRepositoryPolicy.yaml"

requestDescribeImageScanFindings :: DescribeImageScanFindings -> TestTree
requestDescribeImageScanFindings =
  req
    "DescribeImageScanFindings"
    "fixture/DescribeImageScanFindings.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

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

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

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

requestDescribeImages :: DescribeImages -> TestTree
requestDescribeImages =
  req
    "DescribeImages"
    "fixture/DescribeImages.yaml"

requestGetDownloadUrlForLayer :: GetDownloadUrlForLayer -> TestTree
requestGetDownloadUrlForLayer =
  req
    "GetDownloadUrlForLayer"
    "fixture/GetDownloadUrlForLayer.yaml"

requestCompleteLayerUpload :: CompleteLayerUpload -> TestTree
requestCompleteLayerUpload =
  req
    "CompleteLayerUpload"
    "fixture/CompleteLayerUpload.yaml"

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

requestBatchCheckLayerAvailability :: BatchCheckLayerAvailability -> TestTree
requestBatchCheckLayerAvailability =
  req
    "BatchCheckLayerAvailability"
    "fixture/BatchCheckLayerAvailability.yaml"

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

requestPutReplicationConfiguration :: PutReplicationConfiguration -> TestTree
requestPutReplicationConfiguration =
  req
    "PutReplicationConfiguration"
    "fixture/PutReplicationConfiguration.yaml"

requestBatchGetImage :: BatchGetImage -> TestTree
requestBatchGetImage =
  req
    "BatchGetImage"
    "fixture/BatchGetImage.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPutImageTagMutability :: PutImageTagMutability -> TestTree
requestPutImageTagMutability =
  req
    "PutImageTagMutability"
    "fixture/PutImageTagMutability.yaml"

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

responseListImages :: ListImagesResponse -> TestTree
responseListImages =
  res
    "ListImagesResponse"
    "fixture/ListImagesResponse.proto"
    defaultService
    (Proxy :: Proxy ListImages)

responsePutImage :: PutImageResponse -> TestTree
responsePutImage =
  res
    "PutImageResponse"
    "fixture/PutImageResponse.proto"
    defaultService
    (Proxy :: Proxy PutImage)

responseInitiateLayerUpload :: InitiateLayerUploadResponse -> TestTree
responseInitiateLayerUpload =
  res
    "InitiateLayerUploadResponse"
    "fixture/InitiateLayerUploadResponse.proto"
    defaultService
    (Proxy :: Proxy InitiateLayerUpload)

responseGetRegistryPolicy :: GetRegistryPolicyResponse -> TestTree
responseGetRegistryPolicy =
  res
    "GetRegistryPolicyResponse"
    "fixture/GetRegistryPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetRegistryPolicy)

responseDeleteRepositoryPolicy :: DeleteRepositoryPolicyResponse -> TestTree
responseDeleteRepositoryPolicy =
  res
    "DeleteRepositoryPolicyResponse"
    "fixture/DeleteRepositoryPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRepositoryPolicy)

responseDescribeImageScanFindings :: DescribeImageScanFindingsResponse -> TestTree
responseDescribeImageScanFindings =
  res
    "DescribeImageScanFindingsResponse"
    "fixture/DescribeImageScanFindingsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeImageScanFindings)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

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

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

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

responseDescribeImages :: DescribeImagesResponse -> TestTree
responseDescribeImages =
  res
    "DescribeImagesResponse"
    "fixture/DescribeImagesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeImages)

responseGetDownloadUrlForLayer :: GetDownloadUrlForLayerResponse -> TestTree
responseGetDownloadUrlForLayer =
  res
    "GetDownloadUrlForLayerResponse"
    "fixture/GetDownloadUrlForLayerResponse.proto"
    defaultService
    (Proxy :: Proxy GetDownloadUrlForLayer)

responseCompleteLayerUpload :: CompleteLayerUploadResponse -> TestTree
responseCompleteLayerUpload =
  res
    "CompleteLayerUploadResponse"
    "fixture/CompleteLayerUploadResponse.proto"
    defaultService
    (Proxy :: Proxy CompleteLayerUpload)

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

responseBatchCheckLayerAvailability :: BatchCheckLayerAvailabilityResponse -> TestTree
responseBatchCheckLayerAvailability =
  res
    "BatchCheckLayerAvailabilityResponse"
    "fixture/BatchCheckLayerAvailabilityResponse.proto"
    defaultService
    (Proxy :: Proxy BatchCheckLayerAvailability)

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

responsePutReplicationConfiguration :: PutReplicationConfigurationResponse -> TestTree
responsePutReplicationConfiguration =
  res
    "PutReplicationConfigurationResponse"
    "fixture/PutReplicationConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy PutReplicationConfiguration)

responseBatchGetImage :: BatchGetImageResponse -> TestTree
responseBatchGetImage =
  res
    "BatchGetImageResponse"
    "fixture/BatchGetImageResponse.proto"
    defaultService
    (Proxy :: Proxy BatchGetImage)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responsePutImageTagMutability :: PutImageTagMutabilityResponse -> TestTree
responsePutImageTagMutability =
  res
    "PutImageTagMutabilityResponse"
    "fixture/PutImageTagMutabilityResponse.proto"
    defaultService
    (Proxy :: Proxy PutImageTagMutability)

responseGetLifecyclePolicyPreview :: GetLifecyclePolicyPreviewResponse -> TestTree
responseGetLifecyclePolicyPreview =
  res
    "GetLifecyclePolicyPreviewResponse"
    "fixture/GetLifecyclePolicyPreviewResponse.proto"
    defaultService
    (Proxy :: Proxy GetLifecyclePolicyPreview)
