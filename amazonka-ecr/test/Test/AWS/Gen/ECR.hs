{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ECR
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
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
--         [ testGetRepositoryPolicy $
--             getRepositoryPolicy
--
--         , testBatchDeleteImage $
--             batchDeleteImage
--
--         , testBatchCheckLayerAvailability $
--             batchCheckLayerAvailability
--
--         , testDeleteRepositoryPolicy $
--             deleteRepositoryPolicy
--
--         , testCreateRepository $
--             createRepository
--
--         , testCompleteLayerUpload $
--             completeLayerUpload
--
--         , testDescribeRepositories $
--             describeRepositories
--
--         , testUploadLayerPart $
--             uploadLayerPart
--
--         , testBatchGetImage $
--             batchGetImage
--
--         , testSetRepositoryPolicy $
--             setRepositoryPolicy
--
--         , testInitiateLayerUpload $
--             initiateLayerUpload
--
--         , testDeleteRepository $
--             deleteRepository
--
--         , testPutImage $
--             putImage
--
--         , testListImages $
--             listImages
--
--         , testGetAuthorizationToken $
--             getAuthorizationToken
--
--         , testGetDownloadURLForLayer $
--             getDownloadURLForLayer
--
--           ]

--     , testGroup "response"
--         [ testGetRepositoryPolicyResponse $
--             getRepositoryPolicyResponse
--
--         , testBatchDeleteImageResponse $
--             batchDeleteImageResponse
--
--         , testBatchCheckLayerAvailabilityResponse $
--             batchCheckLayerAvailabilityResponse
--
--         , testDeleteRepositoryPolicyResponse $
--             deleteRepositoryPolicyResponse
--
--         , testCreateRepositoryResponse $
--             createRepositoryResponse
--
--         , testCompleteLayerUploadResponse $
--             completeLayerUploadResponse
--
--         , testDescribeRepositoriesResponse $
--             describeRepositoriesResponse
--
--         , testUploadLayerPartResponse $
--             uploadLayerPartResponse
--
--         , testBatchGetImageResponse $
--             batchGetImageResponse
--
--         , testSetRepositoryPolicyResponse $
--             setRepositoryPolicyResponse
--
--         , testInitiateLayerUploadResponse $
--             initiateLayerUploadResponse
--
--         , testDeleteRepositoryResponse $
--             deleteRepositoryResponse
--
--         , testPutImageResponse $
--             putImageResponse
--
--         , testListImagesResponse $
--             listImagesResponse
--
--         , testGetAuthorizationTokenResponse $
--             getAuthorizationTokenResponse
--
--         , testGetDownloadURLForLayerResponse $
--             getDownloadURLForLayerResponse
--
--           ]
--     ]

-- Requests

testGetRepositoryPolicy :: GetRepositoryPolicy -> TestTree
testGetRepositoryPolicy = req
    "GetRepositoryPolicy"
    "fixture/GetRepositoryPolicy.yaml"

testBatchDeleteImage :: BatchDeleteImage -> TestTree
testBatchDeleteImage = req
    "BatchDeleteImage"
    "fixture/BatchDeleteImage.yaml"

testBatchCheckLayerAvailability :: BatchCheckLayerAvailability -> TestTree
testBatchCheckLayerAvailability = req
    "BatchCheckLayerAvailability"
    "fixture/BatchCheckLayerAvailability.yaml"

testDeleteRepositoryPolicy :: DeleteRepositoryPolicy -> TestTree
testDeleteRepositoryPolicy = req
    "DeleteRepositoryPolicy"
    "fixture/DeleteRepositoryPolicy.yaml"

testCreateRepository :: CreateRepository -> TestTree
testCreateRepository = req
    "CreateRepository"
    "fixture/CreateRepository.yaml"

testCompleteLayerUpload :: CompleteLayerUpload -> TestTree
testCompleteLayerUpload = req
    "CompleteLayerUpload"
    "fixture/CompleteLayerUpload.yaml"

testDescribeRepositories :: DescribeRepositories -> TestTree
testDescribeRepositories = req
    "DescribeRepositories"
    "fixture/DescribeRepositories.yaml"

testUploadLayerPart :: UploadLayerPart -> TestTree
testUploadLayerPart = req
    "UploadLayerPart"
    "fixture/UploadLayerPart.yaml"

testBatchGetImage :: BatchGetImage -> TestTree
testBatchGetImage = req
    "BatchGetImage"
    "fixture/BatchGetImage.yaml"

testSetRepositoryPolicy :: SetRepositoryPolicy -> TestTree
testSetRepositoryPolicy = req
    "SetRepositoryPolicy"
    "fixture/SetRepositoryPolicy.yaml"

testInitiateLayerUpload :: InitiateLayerUpload -> TestTree
testInitiateLayerUpload = req
    "InitiateLayerUpload"
    "fixture/InitiateLayerUpload.yaml"

testDeleteRepository :: DeleteRepository -> TestTree
testDeleteRepository = req
    "DeleteRepository"
    "fixture/DeleteRepository.yaml"

testPutImage :: PutImage -> TestTree
testPutImage = req
    "PutImage"
    "fixture/PutImage.yaml"

testListImages :: ListImages -> TestTree
testListImages = req
    "ListImages"
    "fixture/ListImages.yaml"

testGetAuthorizationToken :: GetAuthorizationToken -> TestTree
testGetAuthorizationToken = req
    "GetAuthorizationToken"
    "fixture/GetAuthorizationToken.yaml"

testGetDownloadURLForLayer :: GetDownloadURLForLayer -> TestTree
testGetDownloadURLForLayer = req
    "GetDownloadURLForLayer"
    "fixture/GetDownloadURLForLayer.yaml"

-- Responses

testGetRepositoryPolicyResponse :: GetRepositoryPolicyResponse -> TestTree
testGetRepositoryPolicyResponse = res
    "GetRepositoryPolicyResponse"
    "fixture/GetRepositoryPolicyResponse.proto"
    ecr
    (Proxy :: Proxy GetRepositoryPolicy)

testBatchDeleteImageResponse :: BatchDeleteImageResponse -> TestTree
testBatchDeleteImageResponse = res
    "BatchDeleteImageResponse"
    "fixture/BatchDeleteImageResponse.proto"
    ecr
    (Proxy :: Proxy BatchDeleteImage)

testBatchCheckLayerAvailabilityResponse :: BatchCheckLayerAvailabilityResponse -> TestTree
testBatchCheckLayerAvailabilityResponse = res
    "BatchCheckLayerAvailabilityResponse"
    "fixture/BatchCheckLayerAvailabilityResponse.proto"
    ecr
    (Proxy :: Proxy BatchCheckLayerAvailability)

testDeleteRepositoryPolicyResponse :: DeleteRepositoryPolicyResponse -> TestTree
testDeleteRepositoryPolicyResponse = res
    "DeleteRepositoryPolicyResponse"
    "fixture/DeleteRepositoryPolicyResponse.proto"
    ecr
    (Proxy :: Proxy DeleteRepositoryPolicy)

testCreateRepositoryResponse :: CreateRepositoryResponse -> TestTree
testCreateRepositoryResponse = res
    "CreateRepositoryResponse"
    "fixture/CreateRepositoryResponse.proto"
    ecr
    (Proxy :: Proxy CreateRepository)

testCompleteLayerUploadResponse :: CompleteLayerUploadResponse -> TestTree
testCompleteLayerUploadResponse = res
    "CompleteLayerUploadResponse"
    "fixture/CompleteLayerUploadResponse.proto"
    ecr
    (Proxy :: Proxy CompleteLayerUpload)

testDescribeRepositoriesResponse :: DescribeRepositoriesResponse -> TestTree
testDescribeRepositoriesResponse = res
    "DescribeRepositoriesResponse"
    "fixture/DescribeRepositoriesResponse.proto"
    ecr
    (Proxy :: Proxy DescribeRepositories)

testUploadLayerPartResponse :: UploadLayerPartResponse -> TestTree
testUploadLayerPartResponse = res
    "UploadLayerPartResponse"
    "fixture/UploadLayerPartResponse.proto"
    ecr
    (Proxy :: Proxy UploadLayerPart)

testBatchGetImageResponse :: BatchGetImageResponse -> TestTree
testBatchGetImageResponse = res
    "BatchGetImageResponse"
    "fixture/BatchGetImageResponse.proto"
    ecr
    (Proxy :: Proxy BatchGetImage)

testSetRepositoryPolicyResponse :: SetRepositoryPolicyResponse -> TestTree
testSetRepositoryPolicyResponse = res
    "SetRepositoryPolicyResponse"
    "fixture/SetRepositoryPolicyResponse.proto"
    ecr
    (Proxy :: Proxy SetRepositoryPolicy)

testInitiateLayerUploadResponse :: InitiateLayerUploadResponse -> TestTree
testInitiateLayerUploadResponse = res
    "InitiateLayerUploadResponse"
    "fixture/InitiateLayerUploadResponse.proto"
    ecr
    (Proxy :: Proxy InitiateLayerUpload)

testDeleteRepositoryResponse :: DeleteRepositoryResponse -> TestTree
testDeleteRepositoryResponse = res
    "DeleteRepositoryResponse"
    "fixture/DeleteRepositoryResponse.proto"
    ecr
    (Proxy :: Proxy DeleteRepository)

testPutImageResponse :: PutImageResponse -> TestTree
testPutImageResponse = res
    "PutImageResponse"
    "fixture/PutImageResponse.proto"
    ecr
    (Proxy :: Proxy PutImage)

testListImagesResponse :: ListImagesResponse -> TestTree
testListImagesResponse = res
    "ListImagesResponse"
    "fixture/ListImagesResponse.proto"
    ecr
    (Proxy :: Proxy ListImages)

testGetAuthorizationTokenResponse :: GetAuthorizationTokenResponse -> TestTree
testGetAuthorizationTokenResponse = res
    "GetAuthorizationTokenResponse"
    "fixture/GetAuthorizationTokenResponse.proto"
    ecr
    (Proxy :: Proxy GetAuthorizationToken)

testGetDownloadURLForLayerResponse :: GetDownloadURLForLayerResponse -> TestTree
testGetDownloadURLForLayerResponse = res
    "GetDownloadURLForLayerResponse"
    "fixture/GetDownloadURLForLayerResponse.proto"
    ecr
    (Proxy :: Proxy GetDownloadURLForLayer)
