{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ECR
-- Copyright   : (c) 2013-2020 Brendan Hay
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
--         [ requestGetRepositoryPolicy $
--             getRepositoryPolicy
--
--         , requestPutImageScanningConfiguration $
--             putImageScanningConfiguration
--
--         , requestPutLifecyclePolicy $
--             putLifecyclePolicy
--
--         , requestDeleteLifecyclePolicy $
--             deleteLifecyclePolicy
--
--         , requestPutImageTagMutability $
--             putImageTagMutability
--
--         , requestBatchDeleteImage $
--             batchDeleteImage
--
--         , requestListTagsForResource $
--             listTagsForResource
--
--         , requestGetLifecyclePolicyPreview $
--             getLifecyclePolicyPreview
--
--         , requestBatchCheckLayerAvailability $
--             batchCheckLayerAvailability
--
--         , requestDeleteRepositoryPolicy $
--             deleteRepositoryPolicy
--
--         , requestCreateRepository $
--             createRepository
--
--         , requestCompleteLayerUpload $
--             completeLayerUpload
--
--         , requestDescribeRepositories $
--             describeRepositories
--
--         , requestStartLifecyclePolicyPreview $
--             startLifecyclePolicyPreview
--
--         , requestUploadLayerPart $
--             uploadLayerPart
--
--         , requestBatchGetImage $
--             batchGetImage
--
--         , requestStartImageScan $
--             startImageScan
--
--         , requestGetLifecyclePolicy $
--             getLifecyclePolicy
--
--         , requestTagResource $
--             tagResource
--
--         , requestSetRepositoryPolicy $
--             setRepositoryPolicy
--
--         , requestDescribeImageScanFindings $
--             describeImageScanFindings
--
--         , requestInitiateLayerUpload $
--             initiateLayerUpload
--
--         , requestUntagResource $
--             untagResource
--
--         , requestDeleteRepository $
--             deleteRepository
--
--         , requestPutImage $
--             putImage
--
--         , requestListImages $
--             listImages
--
--         , requestGetAuthorizationToken $
--             getAuthorizationToken
--
--         , requestGetDownloadURLForLayer $
--             getDownloadURLForLayer
--
--         , requestDescribeImages $
--             describeImages
--
--           ]

--     , testGroup "response"
--         [ responseGetRepositoryPolicy $
--             getRepositoryPolicyResponse
--
--         , responsePutImageScanningConfiguration $
--             putImageScanningConfigurationResponse
--
--         , responsePutLifecyclePolicy $
--             putLifecyclePolicyResponse
--
--         , responseDeleteLifecyclePolicy $
--             deleteLifecyclePolicyResponse
--
--         , responsePutImageTagMutability $
--             putImageTagMutabilityResponse
--
--         , responseBatchDeleteImage $
--             batchDeleteImageResponse
--
--         , responseListTagsForResource $
--             listTagsForResourceResponse
--
--         , responseGetLifecyclePolicyPreview $
--             getLifecyclePolicyPreviewResponse
--
--         , responseBatchCheckLayerAvailability $
--             batchCheckLayerAvailabilityResponse
--
--         , responseDeleteRepositoryPolicy $
--             deleteRepositoryPolicyResponse
--
--         , responseCreateRepository $
--             createRepositoryResponse
--
--         , responseCompleteLayerUpload $
--             completeLayerUploadResponse
--
--         , responseDescribeRepositories $
--             describeRepositoriesResponse
--
--         , responseStartLifecyclePolicyPreview $
--             startLifecyclePolicyPreviewResponse
--
--         , responseUploadLayerPart $
--             uploadLayerPartResponse
--
--         , responseBatchGetImage $
--             batchGetImageResponse
--
--         , responseStartImageScan $
--             startImageScanResponse
--
--         , responseGetLifecyclePolicy $
--             getLifecyclePolicyResponse
--
--         , responseTagResource $
--             tagResourceResponse
--
--         , responseSetRepositoryPolicy $
--             setRepositoryPolicyResponse
--
--         , responseDescribeImageScanFindings $
--             describeImageScanFindingsResponse
--
--         , responseInitiateLayerUpload $
--             initiateLayerUploadResponse
--
--         , responseUntagResource $
--             untagResourceResponse
--
--         , responseDeleteRepository $
--             deleteRepositoryResponse
--
--         , responsePutImage $
--             putImageResponse
--
--         , responseListImages $
--             listImagesResponse
--
--         , responseGetAuthorizationToken $
--             getAuthorizationTokenResponse
--
--         , responseGetDownloadURLForLayer $
--             getDownloadURLForLayerResponse
--
--         , responseDescribeImages $
--             describeImagesResponse
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

requestUploadLayerPart :: UploadLayerPart -> TestTree
requestUploadLayerPart =
  req
    "UploadLayerPart"
    "fixture/UploadLayerPart.yaml"

requestBatchGetImage :: BatchGetImage -> TestTree
requestBatchGetImage =
  req
    "BatchGetImage"
    "fixture/BatchGetImage.yaml"

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

requestGetDownloadURLForLayer :: GetDownloadURLForLayer -> TestTree
requestGetDownloadURLForLayer =
  req
    "GetDownloadURLForLayer"
    "fixture/GetDownloadURLForLayer.yaml"

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
    ecr
    (Proxy :: Proxy GetRepositoryPolicy)

responsePutImageScanningConfiguration :: PutImageScanningConfigurationResponse -> TestTree
responsePutImageScanningConfiguration =
  res
    "PutImageScanningConfigurationResponse"
    "fixture/PutImageScanningConfigurationResponse.proto"
    ecr
    (Proxy :: Proxy PutImageScanningConfiguration)

responsePutLifecyclePolicy :: PutLifecyclePolicyResponse -> TestTree
responsePutLifecyclePolicy =
  res
    "PutLifecyclePolicyResponse"
    "fixture/PutLifecyclePolicyResponse.proto"
    ecr
    (Proxy :: Proxy PutLifecyclePolicy)

responseDeleteLifecyclePolicy :: DeleteLifecyclePolicyResponse -> TestTree
responseDeleteLifecyclePolicy =
  res
    "DeleteLifecyclePolicyResponse"
    "fixture/DeleteLifecyclePolicyResponse.proto"
    ecr
    (Proxy :: Proxy DeleteLifecyclePolicy)

responsePutImageTagMutability :: PutImageTagMutabilityResponse -> TestTree
responsePutImageTagMutability =
  res
    "PutImageTagMutabilityResponse"
    "fixture/PutImageTagMutabilityResponse.proto"
    ecr
    (Proxy :: Proxy PutImageTagMutability)

responseBatchDeleteImage :: BatchDeleteImageResponse -> TestTree
responseBatchDeleteImage =
  res
    "BatchDeleteImageResponse"
    "fixture/BatchDeleteImageResponse.proto"
    ecr
    (Proxy :: Proxy BatchDeleteImage)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    ecr
    (Proxy :: Proxy ListTagsForResource)

responseGetLifecyclePolicyPreview :: GetLifecyclePolicyPreviewResponse -> TestTree
responseGetLifecyclePolicyPreview =
  res
    "GetLifecyclePolicyPreviewResponse"
    "fixture/GetLifecyclePolicyPreviewResponse.proto"
    ecr
    (Proxy :: Proxy GetLifecyclePolicyPreview)

responseBatchCheckLayerAvailability :: BatchCheckLayerAvailabilityResponse -> TestTree
responseBatchCheckLayerAvailability =
  res
    "BatchCheckLayerAvailabilityResponse"
    "fixture/BatchCheckLayerAvailabilityResponse.proto"
    ecr
    (Proxy :: Proxy BatchCheckLayerAvailability)

responseDeleteRepositoryPolicy :: DeleteRepositoryPolicyResponse -> TestTree
responseDeleteRepositoryPolicy =
  res
    "DeleteRepositoryPolicyResponse"
    "fixture/DeleteRepositoryPolicyResponse.proto"
    ecr
    (Proxy :: Proxy DeleteRepositoryPolicy)

responseCreateRepository :: CreateRepositoryResponse -> TestTree
responseCreateRepository =
  res
    "CreateRepositoryResponse"
    "fixture/CreateRepositoryResponse.proto"
    ecr
    (Proxy :: Proxy CreateRepository)

responseCompleteLayerUpload :: CompleteLayerUploadResponse -> TestTree
responseCompleteLayerUpload =
  res
    "CompleteLayerUploadResponse"
    "fixture/CompleteLayerUploadResponse.proto"
    ecr
    (Proxy :: Proxy CompleteLayerUpload)

responseDescribeRepositories :: DescribeRepositoriesResponse -> TestTree
responseDescribeRepositories =
  res
    "DescribeRepositoriesResponse"
    "fixture/DescribeRepositoriesResponse.proto"
    ecr
    (Proxy :: Proxy DescribeRepositories)

responseStartLifecyclePolicyPreview :: StartLifecyclePolicyPreviewResponse -> TestTree
responseStartLifecyclePolicyPreview =
  res
    "StartLifecyclePolicyPreviewResponse"
    "fixture/StartLifecyclePolicyPreviewResponse.proto"
    ecr
    (Proxy :: Proxy StartLifecyclePolicyPreview)

responseUploadLayerPart :: UploadLayerPartResponse -> TestTree
responseUploadLayerPart =
  res
    "UploadLayerPartResponse"
    "fixture/UploadLayerPartResponse.proto"
    ecr
    (Proxy :: Proxy UploadLayerPart)

responseBatchGetImage :: BatchGetImageResponse -> TestTree
responseBatchGetImage =
  res
    "BatchGetImageResponse"
    "fixture/BatchGetImageResponse.proto"
    ecr
    (Proxy :: Proxy BatchGetImage)

responseStartImageScan :: StartImageScanResponse -> TestTree
responseStartImageScan =
  res
    "StartImageScanResponse"
    "fixture/StartImageScanResponse.proto"
    ecr
    (Proxy :: Proxy StartImageScan)

responseGetLifecyclePolicy :: GetLifecyclePolicyResponse -> TestTree
responseGetLifecyclePolicy =
  res
    "GetLifecyclePolicyResponse"
    "fixture/GetLifecyclePolicyResponse.proto"
    ecr
    (Proxy :: Proxy GetLifecyclePolicy)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    ecr
    (Proxy :: Proxy TagResource)

responseSetRepositoryPolicy :: SetRepositoryPolicyResponse -> TestTree
responseSetRepositoryPolicy =
  res
    "SetRepositoryPolicyResponse"
    "fixture/SetRepositoryPolicyResponse.proto"
    ecr
    (Proxy :: Proxy SetRepositoryPolicy)

responseDescribeImageScanFindings :: DescribeImageScanFindingsResponse -> TestTree
responseDescribeImageScanFindings =
  res
    "DescribeImageScanFindingsResponse"
    "fixture/DescribeImageScanFindingsResponse.proto"
    ecr
    (Proxy :: Proxy DescribeImageScanFindings)

responseInitiateLayerUpload :: InitiateLayerUploadResponse -> TestTree
responseInitiateLayerUpload =
  res
    "InitiateLayerUploadResponse"
    "fixture/InitiateLayerUploadResponse.proto"
    ecr
    (Proxy :: Proxy InitiateLayerUpload)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    ecr
    (Proxy :: Proxy UntagResource)

responseDeleteRepository :: DeleteRepositoryResponse -> TestTree
responseDeleteRepository =
  res
    "DeleteRepositoryResponse"
    "fixture/DeleteRepositoryResponse.proto"
    ecr
    (Proxy :: Proxy DeleteRepository)

responsePutImage :: PutImageResponse -> TestTree
responsePutImage =
  res
    "PutImageResponse"
    "fixture/PutImageResponse.proto"
    ecr
    (Proxy :: Proxy PutImage)

responseListImages :: ListImagesResponse -> TestTree
responseListImages =
  res
    "ListImagesResponse"
    "fixture/ListImagesResponse.proto"
    ecr
    (Proxy :: Proxy ListImages)

responseGetAuthorizationToken :: GetAuthorizationTokenResponse -> TestTree
responseGetAuthorizationToken =
  res
    "GetAuthorizationTokenResponse"
    "fixture/GetAuthorizationTokenResponse.proto"
    ecr
    (Proxy :: Proxy GetAuthorizationToken)

responseGetDownloadURLForLayer :: GetDownloadURLForLayerResponse -> TestTree
responseGetDownloadURLForLayer =
  res
    "GetDownloadURLForLayerResponse"
    "fixture/GetDownloadURLForLayerResponse.proto"
    ecr
    (Proxy :: Proxy GetDownloadURLForLayer)

responseDescribeImages :: DescribeImagesResponse -> TestTree
responseDescribeImages =
  res
    "DescribeImagesResponse"
    "fixture/DescribeImagesResponse.proto"
    ecr
    (Proxy :: Proxy DescribeImages)
