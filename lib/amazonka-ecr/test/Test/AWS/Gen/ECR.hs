{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ECR
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
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
--         , requestPutLifecyclePolicy $
--             putLifecyclePolicy
--
--         , requestDeleteLifecyclePolicy $
--             deleteLifecyclePolicy
--
--         , requestBatchDeleteImage $
--             batchDeleteImage
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
--         , requestGetLifecyclePolicy $
--             getLifecyclePolicy
--
--         , requestSetRepositoryPolicy $
--             setRepositoryPolicy
--
--         , requestInitiateLayerUpload $
--             initiateLayerUpload
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
--         , responsePutLifecyclePolicy $
--             putLifecyclePolicyResponse
--
--         , responseDeleteLifecyclePolicy $
--             deleteLifecyclePolicyResponse
--
--         , responseBatchDeleteImage $
--             batchDeleteImageResponse
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
--         , responseGetLifecyclePolicy $
--             getLifecyclePolicyResponse
--
--         , responseSetRepositoryPolicy $
--             setRepositoryPolicyResponse
--
--         , responseInitiateLayerUpload $
--             initiateLayerUploadResponse
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
requestGetRepositoryPolicy = req
    "GetRepositoryPolicy"
    "fixture/GetRepositoryPolicy.yaml"

requestPutLifecyclePolicy :: PutLifecyclePolicy -> TestTree
requestPutLifecyclePolicy = req
    "PutLifecyclePolicy"
    "fixture/PutLifecyclePolicy.yaml"

requestDeleteLifecyclePolicy :: DeleteLifecyclePolicy -> TestTree
requestDeleteLifecyclePolicy = req
    "DeleteLifecyclePolicy"
    "fixture/DeleteLifecyclePolicy.yaml"

requestBatchDeleteImage :: BatchDeleteImage -> TestTree
requestBatchDeleteImage = req
    "BatchDeleteImage"
    "fixture/BatchDeleteImage.yaml"

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

requestGetLifecyclePolicy :: GetLifecyclePolicy -> TestTree
requestGetLifecyclePolicy = req
    "GetLifecyclePolicy"
    "fixture/GetLifecyclePolicy.yaml"

requestSetRepositoryPolicy :: SetRepositoryPolicy -> TestTree
requestSetRepositoryPolicy = req
    "SetRepositoryPolicy"
    "fixture/SetRepositoryPolicy.yaml"

requestInitiateLayerUpload :: InitiateLayerUpload -> TestTree
requestInitiateLayerUpload = req
    "InitiateLayerUpload"
    "fixture/InitiateLayerUpload.yaml"

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

requestGetDownloadURLForLayer :: GetDownloadURLForLayer -> TestTree
requestGetDownloadURLForLayer = req
    "GetDownloadURLForLayer"
    "fixture/GetDownloadURLForLayer.yaml"

requestDescribeImages :: DescribeImages -> TestTree
requestDescribeImages = req
    "DescribeImages"
    "fixture/DescribeImages.yaml"

-- Responses

responseGetRepositoryPolicy :: GetRepositoryPolicyResponse -> TestTree
responseGetRepositoryPolicy = res
    "GetRepositoryPolicyResponse"
    "fixture/GetRepositoryPolicyResponse.proto"
    ecr
    (Proxy :: Proxy GetRepositoryPolicy)

responsePutLifecyclePolicy :: PutLifecyclePolicyResponse -> TestTree
responsePutLifecyclePolicy = res
    "PutLifecyclePolicyResponse"
    "fixture/PutLifecyclePolicyResponse.proto"
    ecr
    (Proxy :: Proxy PutLifecyclePolicy)

responseDeleteLifecyclePolicy :: DeleteLifecyclePolicyResponse -> TestTree
responseDeleteLifecyclePolicy = res
    "DeleteLifecyclePolicyResponse"
    "fixture/DeleteLifecyclePolicyResponse.proto"
    ecr
    (Proxy :: Proxy DeleteLifecyclePolicy)

responseBatchDeleteImage :: BatchDeleteImageResponse -> TestTree
responseBatchDeleteImage = res
    "BatchDeleteImageResponse"
    "fixture/BatchDeleteImageResponse.proto"
    ecr
    (Proxy :: Proxy BatchDeleteImage)

responseGetLifecyclePolicyPreview :: GetLifecyclePolicyPreviewResponse -> TestTree
responseGetLifecyclePolicyPreview = res
    "GetLifecyclePolicyPreviewResponse"
    "fixture/GetLifecyclePolicyPreviewResponse.proto"
    ecr
    (Proxy :: Proxy GetLifecyclePolicyPreview)

responseBatchCheckLayerAvailability :: BatchCheckLayerAvailabilityResponse -> TestTree
responseBatchCheckLayerAvailability = res
    "BatchCheckLayerAvailabilityResponse"
    "fixture/BatchCheckLayerAvailabilityResponse.proto"
    ecr
    (Proxy :: Proxy BatchCheckLayerAvailability)

responseDeleteRepositoryPolicy :: DeleteRepositoryPolicyResponse -> TestTree
responseDeleteRepositoryPolicy = res
    "DeleteRepositoryPolicyResponse"
    "fixture/DeleteRepositoryPolicyResponse.proto"
    ecr
    (Proxy :: Proxy DeleteRepositoryPolicy)

responseCreateRepository :: CreateRepositoryResponse -> TestTree
responseCreateRepository = res
    "CreateRepositoryResponse"
    "fixture/CreateRepositoryResponse.proto"
    ecr
    (Proxy :: Proxy CreateRepository)

responseCompleteLayerUpload :: CompleteLayerUploadResponse -> TestTree
responseCompleteLayerUpload = res
    "CompleteLayerUploadResponse"
    "fixture/CompleteLayerUploadResponse.proto"
    ecr
    (Proxy :: Proxy CompleteLayerUpload)

responseDescribeRepositories :: DescribeRepositoriesResponse -> TestTree
responseDescribeRepositories = res
    "DescribeRepositoriesResponse"
    "fixture/DescribeRepositoriesResponse.proto"
    ecr
    (Proxy :: Proxy DescribeRepositories)

responseStartLifecyclePolicyPreview :: StartLifecyclePolicyPreviewResponse -> TestTree
responseStartLifecyclePolicyPreview = res
    "StartLifecyclePolicyPreviewResponse"
    "fixture/StartLifecyclePolicyPreviewResponse.proto"
    ecr
    (Proxy :: Proxy StartLifecyclePolicyPreview)

responseUploadLayerPart :: UploadLayerPartResponse -> TestTree
responseUploadLayerPart = res
    "UploadLayerPartResponse"
    "fixture/UploadLayerPartResponse.proto"
    ecr
    (Proxy :: Proxy UploadLayerPart)

responseBatchGetImage :: BatchGetImageResponse -> TestTree
responseBatchGetImage = res
    "BatchGetImageResponse"
    "fixture/BatchGetImageResponse.proto"
    ecr
    (Proxy :: Proxy BatchGetImage)

responseGetLifecyclePolicy :: GetLifecyclePolicyResponse -> TestTree
responseGetLifecyclePolicy = res
    "GetLifecyclePolicyResponse"
    "fixture/GetLifecyclePolicyResponse.proto"
    ecr
    (Proxy :: Proxy GetLifecyclePolicy)

responseSetRepositoryPolicy :: SetRepositoryPolicyResponse -> TestTree
responseSetRepositoryPolicy = res
    "SetRepositoryPolicyResponse"
    "fixture/SetRepositoryPolicyResponse.proto"
    ecr
    (Proxy :: Proxy SetRepositoryPolicy)

responseInitiateLayerUpload :: InitiateLayerUploadResponse -> TestTree
responseInitiateLayerUpload = res
    "InitiateLayerUploadResponse"
    "fixture/InitiateLayerUploadResponse.proto"
    ecr
    (Proxy :: Proxy InitiateLayerUpload)

responseDeleteRepository :: DeleteRepositoryResponse -> TestTree
responseDeleteRepository = res
    "DeleteRepositoryResponse"
    "fixture/DeleteRepositoryResponse.proto"
    ecr
    (Proxy :: Proxy DeleteRepository)

responsePutImage :: PutImageResponse -> TestTree
responsePutImage = res
    "PutImageResponse"
    "fixture/PutImageResponse.proto"
    ecr
    (Proxy :: Proxy PutImage)

responseListImages :: ListImagesResponse -> TestTree
responseListImages = res
    "ListImagesResponse"
    "fixture/ListImagesResponse.proto"
    ecr
    (Proxy :: Proxy ListImages)

responseGetAuthorizationToken :: GetAuthorizationTokenResponse -> TestTree
responseGetAuthorizationToken = res
    "GetAuthorizationTokenResponse"
    "fixture/GetAuthorizationTokenResponse.proto"
    ecr
    (Proxy :: Proxy GetAuthorizationToken)

responseGetDownloadURLForLayer :: GetDownloadURLForLayerResponse -> TestTree
responseGetDownloadURLForLayer = res
    "GetDownloadURLForLayerResponse"
    "fixture/GetDownloadURLForLayerResponse.proto"
    ecr
    (Proxy :: Proxy GetDownloadURLForLayer)

responseDescribeImages :: DescribeImagesResponse -> TestTree
responseDescribeImages = res
    "DescribeImagesResponse"
    "fixture/DescribeImagesResponse.proto"
    ecr
    (Proxy :: Proxy DescribeImages)
