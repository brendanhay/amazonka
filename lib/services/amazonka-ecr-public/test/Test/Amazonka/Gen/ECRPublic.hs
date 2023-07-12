{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.ECRPublic
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.ECRPublic where

import Amazonka.ECRPublic
import qualified Data.Proxy as Proxy
import Test.Amazonka.ECRPublic.Internal
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
--         , requestCompleteLayerUpload $
--             newCompleteLayerUpload
--
--         , requestCreateRepository $
--             newCreateRepository
--
--         , requestDeleteRepository $
--             newDeleteRepository
--
--         , requestDeleteRepositoryPolicy $
--             newDeleteRepositoryPolicy
--
--         , requestDescribeImageTags $
--             newDescribeImageTags
--
--         , requestDescribeImages $
--             newDescribeImages
--
--         , requestDescribeRegistries $
--             newDescribeRegistries
--
--         , requestDescribeRepositories $
--             newDescribeRepositories
--
--         , requestGetAuthorizationToken $
--             newGetAuthorizationToken
--
--         , requestGetRegistryCatalogData $
--             newGetRegistryCatalogData
--
--         , requestGetRepositoryCatalogData $
--             newGetRepositoryCatalogData
--
--         , requestGetRepositoryPolicy $
--             newGetRepositoryPolicy
--
--         , requestInitiateLayerUpload $
--             newInitiateLayerUpload
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPutImage $
--             newPutImage
--
--         , requestPutRegistryCatalogData $
--             newPutRegistryCatalogData
--
--         , requestPutRepositoryCatalogData $
--             newPutRepositoryCatalogData
--
--         , requestSetRepositoryPolicy $
--             newSetRepositoryPolicy
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
--         , responseCompleteLayerUpload $
--             newCompleteLayerUploadResponse
--
--         , responseCreateRepository $
--             newCreateRepositoryResponse
--
--         , responseDeleteRepository $
--             newDeleteRepositoryResponse
--
--         , responseDeleteRepositoryPolicy $
--             newDeleteRepositoryPolicyResponse
--
--         , responseDescribeImageTags $
--             newDescribeImageTagsResponse
--
--         , responseDescribeImages $
--             newDescribeImagesResponse
--
--         , responseDescribeRegistries $
--             newDescribeRegistriesResponse
--
--         , responseDescribeRepositories $
--             newDescribeRepositoriesResponse
--
--         , responseGetAuthorizationToken $
--             newGetAuthorizationTokenResponse
--
--         , responseGetRegistryCatalogData $
--             newGetRegistryCatalogDataResponse
--
--         , responseGetRepositoryCatalogData $
--             newGetRepositoryCatalogDataResponse
--
--         , responseGetRepositoryPolicy $
--             newGetRepositoryPolicyResponse
--
--         , responseInitiateLayerUpload $
--             newInitiateLayerUploadResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePutImage $
--             newPutImageResponse
--
--         , responsePutRegistryCatalogData $
--             newPutRegistryCatalogDataResponse
--
--         , responsePutRepositoryCatalogData $
--             newPutRepositoryCatalogDataResponse
--
--         , responseSetRepositoryPolicy $
--             newSetRepositoryPolicyResponse
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

requestCompleteLayerUpload :: CompleteLayerUpload -> TestTree
requestCompleteLayerUpload =
  req
    "CompleteLayerUpload"
    "fixture/CompleteLayerUpload.yaml"

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

requestDeleteRepositoryPolicy :: DeleteRepositoryPolicy -> TestTree
requestDeleteRepositoryPolicy =
  req
    "DeleteRepositoryPolicy"
    "fixture/DeleteRepositoryPolicy.yaml"

requestDescribeImageTags :: DescribeImageTags -> TestTree
requestDescribeImageTags =
  req
    "DescribeImageTags"
    "fixture/DescribeImageTags.yaml"

requestDescribeImages :: DescribeImages -> TestTree
requestDescribeImages =
  req
    "DescribeImages"
    "fixture/DescribeImages.yaml"

requestDescribeRegistries :: DescribeRegistries -> TestTree
requestDescribeRegistries =
  req
    "DescribeRegistries"
    "fixture/DescribeRegistries.yaml"

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

requestGetRegistryCatalogData :: GetRegistryCatalogData -> TestTree
requestGetRegistryCatalogData =
  req
    "GetRegistryCatalogData"
    "fixture/GetRegistryCatalogData.yaml"

requestGetRepositoryCatalogData :: GetRepositoryCatalogData -> TestTree
requestGetRepositoryCatalogData =
  req
    "GetRepositoryCatalogData"
    "fixture/GetRepositoryCatalogData.yaml"

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

requestPutRegistryCatalogData :: PutRegistryCatalogData -> TestTree
requestPutRegistryCatalogData =
  req
    "PutRegistryCatalogData"
    "fixture/PutRegistryCatalogData.yaml"

requestPutRepositoryCatalogData :: PutRepositoryCatalogData -> TestTree
requestPutRepositoryCatalogData =
  req
    "PutRepositoryCatalogData"
    "fixture/PutRepositoryCatalogData.yaml"

requestSetRepositoryPolicy :: SetRepositoryPolicy -> TestTree
requestSetRepositoryPolicy =
  req
    "SetRepositoryPolicy"
    "fixture/SetRepositoryPolicy.yaml"

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

responseCompleteLayerUpload :: CompleteLayerUploadResponse -> TestTree
responseCompleteLayerUpload =
  res
    "CompleteLayerUploadResponse"
    "fixture/CompleteLayerUploadResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CompleteLayerUpload)

responseCreateRepository :: CreateRepositoryResponse -> TestTree
responseCreateRepository =
  res
    "CreateRepositoryResponse"
    "fixture/CreateRepositoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRepository)

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

responseDescribeImageTags :: DescribeImageTagsResponse -> TestTree
responseDescribeImageTags =
  res
    "DescribeImageTagsResponse"
    "fixture/DescribeImageTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeImageTags)

responseDescribeImages :: DescribeImagesResponse -> TestTree
responseDescribeImages =
  res
    "DescribeImagesResponse"
    "fixture/DescribeImagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeImages)

responseDescribeRegistries :: DescribeRegistriesResponse -> TestTree
responseDescribeRegistries =
  res
    "DescribeRegistriesResponse"
    "fixture/DescribeRegistriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRegistries)

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

responseGetRegistryCatalogData :: GetRegistryCatalogDataResponse -> TestTree
responseGetRegistryCatalogData =
  res
    "GetRegistryCatalogDataResponse"
    "fixture/GetRegistryCatalogDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRegistryCatalogData)

responseGetRepositoryCatalogData :: GetRepositoryCatalogDataResponse -> TestTree
responseGetRepositoryCatalogData =
  res
    "GetRepositoryCatalogDataResponse"
    "fixture/GetRepositoryCatalogDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRepositoryCatalogData)

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

responsePutRegistryCatalogData :: PutRegistryCatalogDataResponse -> TestTree
responsePutRegistryCatalogData =
  res
    "PutRegistryCatalogDataResponse"
    "fixture/PutRegistryCatalogDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutRegistryCatalogData)

responsePutRepositoryCatalogData :: PutRepositoryCatalogDataResponse -> TestTree
responsePutRepositoryCatalogData =
  res
    "PutRepositoryCatalogDataResponse"
    "fixture/PutRepositoryCatalogDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutRepositoryCatalogData)

responseSetRepositoryPolicy :: SetRepositoryPolicyResponse -> TestTree
responseSetRepositoryPolicy =
  res
    "SetRepositoryPolicyResponse"
    "fixture/SetRepositoryPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetRepositoryPolicy)

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
