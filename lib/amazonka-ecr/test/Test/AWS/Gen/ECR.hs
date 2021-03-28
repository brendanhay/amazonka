{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ECR
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.ECR where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.ECR
import Test.AWS.ECR.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGetRepositoryPolicy $
--             mkGetRepositoryPolicy
--
--         , requestPutImageScanningConfiguration $
--             mkPutImageScanningConfiguration
--
--         , requestPutLifecyclePolicy $
--             mkPutLifecyclePolicy
--
--         , requestDeleteLifecyclePolicy $
--             mkDeleteLifecyclePolicy
--
--         , requestPutImageTagMutability $
--             mkPutImageTagMutability
--
--         , requestBatchDeleteImage $
--             mkBatchDeleteImage
--
--         , requestListTagsForResource $
--             mkListTagsForResource
--
--         , requestGetLifecyclePolicyPreview $
--             mkGetLifecyclePolicyPreview
--
--         , requestBatchCheckLayerAvailability $
--             mkBatchCheckLayerAvailability
--
--         , requestDeleteRepositoryPolicy $
--             mkDeleteRepositoryPolicy
--
--         , requestCreateRepository $
--             mkCreateRepository
--
--         , requestCompleteLayerUpload $
--             mkCompleteLayerUpload
--
--         , requestDescribeRepositories $
--             mkDescribeRepositories
--
--         , requestStartLifecyclePolicyPreview $
--             mkStartLifecyclePolicyPreview
--
--         , requestUploadLayerPart $
--             mkUploadLayerPart
--
--         , requestBatchGetImage $
--             mkBatchGetImage
--
--         , requestStartImageScan $
--             mkStartImageScan
--
--         , requestGetLifecyclePolicy $
--             mkGetLifecyclePolicy
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestSetRepositoryPolicy $
--             mkSetRepositoryPolicy
--
--         , requestDescribeImageScanFindings $
--             mkDescribeImageScanFindings
--
--         , requestInitiateLayerUpload $
--             mkInitiateLayerUpload
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestDeleteRepository $
--             mkDeleteRepository
--
--         , requestPutImage $
--             mkPutImage
--
--         , requestListImages $
--             mkListImages
--
--         , requestGetAuthorizationToken $
--             mkGetAuthorizationToken
--
--         , requestGetDownloadUrlForLayer $
--             mkGetDownloadUrlForLayer
--
--         , requestDescribeImages $
--             mkDescribeImages
--
--           ]

--     , testGroup "response"
--         [ responseGetRepositoryPolicy $
--             mkGetRepositoryPolicyResponse
--
--         , responsePutImageScanningConfiguration $
--             mkPutImageScanningConfigurationResponse
--
--         , responsePutLifecyclePolicy $
--             mkPutLifecyclePolicyResponse
--
--         , responseDeleteLifecyclePolicy $
--             mkDeleteLifecyclePolicyResponse
--
--         , responsePutImageTagMutability $
--             mkPutImageTagMutabilityResponse
--
--         , responseBatchDeleteImage $
--             mkBatchDeleteImageResponse
--
--         , responseListTagsForResource $
--             mkListTagsForResourceResponse
--
--         , responseGetLifecyclePolicyPreview $
--             mkGetLifecyclePolicyPreviewResponse
--
--         , responseBatchCheckLayerAvailability $
--             mkBatchCheckLayerAvailabilityResponse
--
--         , responseDeleteRepositoryPolicy $
--             mkDeleteRepositoryPolicyResponse
--
--         , responseCreateRepository $
--             mkCreateRepositoryResponse
--
--         , responseCompleteLayerUpload $
--             mkCompleteLayerUploadResponse
--
--         , responseDescribeRepositories $
--             mkDescribeRepositoriesResponse
--
--         , responseStartLifecyclePolicyPreview $
--             mkStartLifecyclePolicyPreviewResponse
--
--         , responseUploadLayerPart $
--             mkUploadLayerPartResponse
--
--         , responseBatchGetImage $
--             mkBatchGetImageResponse
--
--         , responseStartImageScan $
--             mkStartImageScanResponse
--
--         , responseGetLifecyclePolicy $
--             mkGetLifecyclePolicyResponse
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responseSetRepositoryPolicy $
--             mkSetRepositoryPolicyResponse
--
--         , responseDescribeImageScanFindings $
--             mkDescribeImageScanFindingsResponse
--
--         , responseInitiateLayerUpload $
--             mkInitiateLayerUploadResponse
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responseDeleteRepository $
--             mkDeleteRepositoryResponse
--
--         , responsePutImage $
--             mkPutImageResponse
--
--         , responseListImages $
--             mkListImagesResponse
--
--         , responseGetAuthorizationToken $
--             mkGetAuthorizationTokenResponse
--
--         , responseGetDownloadUrlForLayer $
--             mkGetDownloadUrlForLayerResponse
--
--         , responseDescribeImages $
--             mkDescribeImagesResponse
--
--           ]
--     ]

-- Requests

requestGetRepositoryPolicy :: GetRepositoryPolicy -> TestTree
requestGetRepositoryPolicy = req
    "GetRepositoryPolicy"
    "fixture/GetRepositoryPolicy.yaml"

requestPutImageScanningConfiguration :: PutImageScanningConfiguration -> TestTree
requestPutImageScanningConfiguration = req
    "PutImageScanningConfiguration"
    "fixture/PutImageScanningConfiguration.yaml"

requestPutLifecyclePolicy :: PutLifecyclePolicy -> TestTree
requestPutLifecyclePolicy = req
    "PutLifecyclePolicy"
    "fixture/PutLifecyclePolicy.yaml"

requestDeleteLifecyclePolicy :: DeleteLifecyclePolicy -> TestTree
requestDeleteLifecyclePolicy = req
    "DeleteLifecyclePolicy"
    "fixture/DeleteLifecyclePolicy.yaml"

requestPutImageTagMutability :: PutImageTagMutability -> TestTree
requestPutImageTagMutability = req
    "PutImageTagMutability"
    "fixture/PutImageTagMutability.yaml"

requestBatchDeleteImage :: BatchDeleteImage -> TestTree
requestBatchDeleteImage = req
    "BatchDeleteImage"
    "fixture/BatchDeleteImage.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource = req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestGetLifecyclePolicyPreview :: GetLifecyclePolicyPreview -> TestTree
requestGetLifecyclePolicyPreview = req
    "GetLifecyclePolicyPreview"
    "fixture/GetLifecyclePolicyPreview.yaml"

requestBatchCheckLayerAvailability :: BatchCheckLayerAvailability -> TestTree
requestBatchCheckLayerAvailability = req
    "BatchCheckLayerAvailability"
    "fixture/BatchCheckLayerAvailability.yaml"

requestDeleteRepositoryPolicy :: DeleteRepositoryPolicy -> TestTree
requestDeleteRepositoryPolicy = req
    "DeleteRepositoryPolicy"
    "fixture/DeleteRepositoryPolicy.yaml"

requestCreateRepository :: CreateRepository -> TestTree
requestCreateRepository = req
    "CreateRepository"
    "fixture/CreateRepository.yaml"

requestCompleteLayerUpload :: CompleteLayerUpload -> TestTree
requestCompleteLayerUpload = req
    "CompleteLayerUpload"
    "fixture/CompleteLayerUpload.yaml"

requestDescribeRepositories :: DescribeRepositories -> TestTree
requestDescribeRepositories = req
    "DescribeRepositories"
    "fixture/DescribeRepositories.yaml"

requestStartLifecyclePolicyPreview :: StartLifecyclePolicyPreview -> TestTree
requestStartLifecyclePolicyPreview = req
    "StartLifecyclePolicyPreview"
    "fixture/StartLifecyclePolicyPreview.yaml"

requestUploadLayerPart :: UploadLayerPart -> TestTree
requestUploadLayerPart = req
    "UploadLayerPart"
    "fixture/UploadLayerPart.yaml"

requestBatchGetImage :: BatchGetImage -> TestTree
requestBatchGetImage = req
    "BatchGetImage"
    "fixture/BatchGetImage.yaml"

requestStartImageScan :: StartImageScan -> TestTree
requestStartImageScan = req
    "StartImageScan"
    "fixture/StartImageScan.yaml"

requestGetLifecyclePolicy :: GetLifecyclePolicy -> TestTree
requestGetLifecyclePolicy = req
    "GetLifecyclePolicy"
    "fixture/GetLifecyclePolicy.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource = req
    "TagResource"
    "fixture/TagResource.yaml"

requestSetRepositoryPolicy :: SetRepositoryPolicy -> TestTree
requestSetRepositoryPolicy = req
    "SetRepositoryPolicy"
    "fixture/SetRepositoryPolicy.yaml"

requestDescribeImageScanFindings :: DescribeImageScanFindings -> TestTree
requestDescribeImageScanFindings = req
    "DescribeImageScanFindings"
    "fixture/DescribeImageScanFindings.yaml"

requestInitiateLayerUpload :: InitiateLayerUpload -> TestTree
requestInitiateLayerUpload = req
    "InitiateLayerUpload"
    "fixture/InitiateLayerUpload.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource = req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDeleteRepository :: DeleteRepository -> TestTree
requestDeleteRepository = req
    "DeleteRepository"
    "fixture/DeleteRepository.yaml"

requestPutImage :: PutImage -> TestTree
requestPutImage = req
    "PutImage"
    "fixture/PutImage.yaml"

requestListImages :: ListImages -> TestTree
requestListImages = req
    "ListImages"
    "fixture/ListImages.yaml"

requestGetAuthorizationToken :: GetAuthorizationToken -> TestTree
requestGetAuthorizationToken = req
    "GetAuthorizationToken"
    "fixture/GetAuthorizationToken.yaml"

requestGetDownloadUrlForLayer :: GetDownloadUrlForLayer -> TestTree
requestGetDownloadUrlForLayer = req
    "GetDownloadUrlForLayer"
    "fixture/GetDownloadUrlForLayer.yaml"

requestDescribeImages :: DescribeImages -> TestTree
requestDescribeImages = req
    "DescribeImages"
    "fixture/DescribeImages.yaml"

-- Responses

responseGetRepositoryPolicy :: GetRepositoryPolicyResponse -> TestTree
responseGetRepositoryPolicy = res
    "GetRepositoryPolicyResponse"
    "fixture/GetRepositoryPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetRepositoryPolicy)

responsePutImageScanningConfiguration :: PutImageScanningConfigurationResponse -> TestTree
responsePutImageScanningConfiguration = res
    "PutImageScanningConfigurationResponse"
    "fixture/PutImageScanningConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutImageScanningConfiguration)

responsePutLifecyclePolicy :: PutLifecyclePolicyResponse -> TestTree
responsePutLifecyclePolicy = res
    "PutLifecyclePolicyResponse"
    "fixture/PutLifecyclePolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutLifecyclePolicy)

responseDeleteLifecyclePolicy :: DeleteLifecyclePolicyResponse -> TestTree
responseDeleteLifecyclePolicy = res
    "DeleteLifecyclePolicyResponse"
    "fixture/DeleteLifecyclePolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteLifecyclePolicy)

responsePutImageTagMutability :: PutImageTagMutabilityResponse -> TestTree
responsePutImageTagMutability = res
    "PutImageTagMutabilityResponse"
    "fixture/PutImageTagMutabilityResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutImageTagMutability)

responseBatchDeleteImage :: BatchDeleteImageResponse -> TestTree
responseBatchDeleteImage = res
    "BatchDeleteImageResponse"
    "fixture/BatchDeleteImageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy BatchDeleteImage)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource = res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTagsForResource)

responseGetLifecyclePolicyPreview :: GetLifecyclePolicyPreviewResponse -> TestTree
responseGetLifecyclePolicyPreview = res
    "GetLifecyclePolicyPreviewResponse"
    "fixture/GetLifecyclePolicyPreviewResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetLifecyclePolicyPreview)

responseBatchCheckLayerAvailability :: BatchCheckLayerAvailabilityResponse -> TestTree
responseBatchCheckLayerAvailability = res
    "BatchCheckLayerAvailabilityResponse"
    "fixture/BatchCheckLayerAvailabilityResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy BatchCheckLayerAvailability)

responseDeleteRepositoryPolicy :: DeleteRepositoryPolicyResponse -> TestTree
responseDeleteRepositoryPolicy = res
    "DeleteRepositoryPolicyResponse"
    "fixture/DeleteRepositoryPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteRepositoryPolicy)

responseCreateRepository :: CreateRepositoryResponse -> TestTree
responseCreateRepository = res
    "CreateRepositoryResponse"
    "fixture/CreateRepositoryResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateRepository)

responseCompleteLayerUpload :: CompleteLayerUploadResponse -> TestTree
responseCompleteLayerUpload = res
    "CompleteLayerUploadResponse"
    "fixture/CompleteLayerUploadResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CompleteLayerUpload)

responseDescribeRepositories :: DescribeRepositoriesResponse -> TestTree
responseDescribeRepositories = res
    "DescribeRepositoriesResponse"
    "fixture/DescribeRepositoriesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeRepositories)

responseStartLifecyclePolicyPreview :: StartLifecyclePolicyPreviewResponse -> TestTree
responseStartLifecyclePolicyPreview = res
    "StartLifecyclePolicyPreviewResponse"
    "fixture/StartLifecyclePolicyPreviewResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartLifecyclePolicyPreview)

responseUploadLayerPart :: UploadLayerPartResponse -> TestTree
responseUploadLayerPart = res
    "UploadLayerPartResponse"
    "fixture/UploadLayerPartResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UploadLayerPart)

responseBatchGetImage :: BatchGetImageResponse -> TestTree
responseBatchGetImage = res
    "BatchGetImageResponse"
    "fixture/BatchGetImageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy BatchGetImage)

responseStartImageScan :: StartImageScanResponse -> TestTree
responseStartImageScan = res
    "StartImageScanResponse"
    "fixture/StartImageScanResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartImageScan)

responseGetLifecyclePolicy :: GetLifecyclePolicyResponse -> TestTree
responseGetLifecyclePolicy = res
    "GetLifecyclePolicyResponse"
    "fixture/GetLifecyclePolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetLifecyclePolicy)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource = res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TagResource)

responseSetRepositoryPolicy :: SetRepositoryPolicyResponse -> TestTree
responseSetRepositoryPolicy = res
    "SetRepositoryPolicyResponse"
    "fixture/SetRepositoryPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SetRepositoryPolicy)

responseDescribeImageScanFindings :: DescribeImageScanFindingsResponse -> TestTree
responseDescribeImageScanFindings = res
    "DescribeImageScanFindingsResponse"
    "fixture/DescribeImageScanFindingsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeImageScanFindings)

responseInitiateLayerUpload :: InitiateLayerUploadResponse -> TestTree
responseInitiateLayerUpload = res
    "InitiateLayerUploadResponse"
    "fixture/InitiateLayerUploadResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy InitiateLayerUpload)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource = res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UntagResource)

responseDeleteRepository :: DeleteRepositoryResponse -> TestTree
responseDeleteRepository = res
    "DeleteRepositoryResponse"
    "fixture/DeleteRepositoryResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteRepository)

responsePutImage :: PutImageResponse -> TestTree
responsePutImage = res
    "PutImageResponse"
    "fixture/PutImageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutImage)

responseListImages :: ListImagesResponse -> TestTree
responseListImages = res
    "ListImagesResponse"
    "fixture/ListImagesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListImages)

responseGetAuthorizationToken :: GetAuthorizationTokenResponse -> TestTree
responseGetAuthorizationToken = res
    "GetAuthorizationTokenResponse"
    "fixture/GetAuthorizationTokenResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetAuthorizationToken)

responseGetDownloadUrlForLayer :: GetDownloadUrlForLayerResponse -> TestTree
responseGetDownloadUrlForLayer = res
    "GetDownloadUrlForLayerResponse"
    "fixture/GetDownloadUrlForLayerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetDownloadUrlForLayer)

responseDescribeImages :: DescribeImagesResponse -> TestTree
responseDescribeImages = res
    "DescribeImagesResponse"
    "fixture/DescribeImagesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeImages)
