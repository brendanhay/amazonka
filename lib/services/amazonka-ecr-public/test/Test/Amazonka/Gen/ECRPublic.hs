{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.ECRPublic
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
--         [ requestGetRepositoryPolicy $
--             newGetRepositoryPolicy
--
--         , requestPutRegistryCatalogData $
--             newPutRegistryCatalogData
--
--         , requestBatchDeleteImage $
--             newBatchDeleteImage
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestBatchCheckLayerAvailability $
--             newBatchCheckLayerAvailability
--
--         , requestPutRepositoryCatalogData $
--             newPutRepositoryCatalogData
--
--         , requestDeleteRepositoryPolicy $
--             newDeleteRepositoryPolicy
--
--         , requestCreateRepository $
--             newCreateRepository
--
--         , requestDescribeRegistries $
--             newDescribeRegistries
--
--         , requestCompleteLayerUpload $
--             newCompleteLayerUpload
--
--         , requestDescribeRepositories $
--             newDescribeRepositories
--
--         , requestUploadLayerPart $
--             newUploadLayerPart
--
--         , requestGetRepositoryCatalogData $
--             newGetRepositoryCatalogData
--
--         , requestGetRegistryCatalogData $
--             newGetRegistryCatalogData
--
--         , requestTagResource $
--             newTagResource
--
--         , requestSetRepositoryPolicy $
--             newSetRepositoryPolicy
--
--         , requestInitiateLayerUpload $
--             newInitiateLayerUpload
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDescribeImageTags $
--             newDescribeImageTags
--
--         , requestDeleteRepository $
--             newDeleteRepository
--
--         , requestPutImage $
--             newPutImage
--
--         , requestGetAuthorizationToken $
--             newGetAuthorizationToken
--
--         , requestDescribeImages $
--             newDescribeImages
--
--           ]

--     , testGroup "response"
--         [ responseGetRepositoryPolicy $
--             newGetRepositoryPolicyResponse
--
--         , responsePutRegistryCatalogData $
--             newPutRegistryCatalogDataResponse
--
--         , responseBatchDeleteImage $
--             newBatchDeleteImageResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseBatchCheckLayerAvailability $
--             newBatchCheckLayerAvailabilityResponse
--
--         , responsePutRepositoryCatalogData $
--             newPutRepositoryCatalogDataResponse
--
--         , responseDeleteRepositoryPolicy $
--             newDeleteRepositoryPolicyResponse
--
--         , responseCreateRepository $
--             newCreateRepositoryResponse
--
--         , responseDescribeRegistries $
--             newDescribeRegistriesResponse
--
--         , responseCompleteLayerUpload $
--             newCompleteLayerUploadResponse
--
--         , responseDescribeRepositories $
--             newDescribeRepositoriesResponse
--
--         , responseUploadLayerPart $
--             newUploadLayerPartResponse
--
--         , responseGetRepositoryCatalogData $
--             newGetRepositoryCatalogDataResponse
--
--         , responseGetRegistryCatalogData $
--             newGetRegistryCatalogDataResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseSetRepositoryPolicy $
--             newSetRepositoryPolicyResponse
--
--         , responseInitiateLayerUpload $
--             newInitiateLayerUploadResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDescribeImageTags $
--             newDescribeImageTagsResponse
--
--         , responseDeleteRepository $
--             newDeleteRepositoryResponse
--
--         , responsePutImage $
--             newPutImageResponse
--
--         , responseGetAuthorizationToken $
--             newGetAuthorizationTokenResponse
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

requestPutRegistryCatalogData :: PutRegistryCatalogData -> TestTree
requestPutRegistryCatalogData =
  req
    "PutRegistryCatalogData"
    "fixture/PutRegistryCatalogData.yaml"

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

requestBatchCheckLayerAvailability :: BatchCheckLayerAvailability -> TestTree
requestBatchCheckLayerAvailability =
  req
    "BatchCheckLayerAvailability"
    "fixture/BatchCheckLayerAvailability.yaml"

requestPutRepositoryCatalogData :: PutRepositoryCatalogData -> TestTree
requestPutRepositoryCatalogData =
  req
    "PutRepositoryCatalogData"
    "fixture/PutRepositoryCatalogData.yaml"

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

requestDescribeRegistries :: DescribeRegistries -> TestTree
requestDescribeRegistries =
  req
    "DescribeRegistries"
    "fixture/DescribeRegistries.yaml"

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

requestUploadLayerPart :: UploadLayerPart -> TestTree
requestUploadLayerPart =
  req
    "UploadLayerPart"
    "fixture/UploadLayerPart.yaml"

requestGetRepositoryCatalogData :: GetRepositoryCatalogData -> TestTree
requestGetRepositoryCatalogData =
  req
    "GetRepositoryCatalogData"
    "fixture/GetRepositoryCatalogData.yaml"

requestGetRegistryCatalogData :: GetRegistryCatalogData -> TestTree
requestGetRegistryCatalogData =
  req
    "GetRegistryCatalogData"
    "fixture/GetRegistryCatalogData.yaml"

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

requestDescribeImageTags :: DescribeImageTags -> TestTree
requestDescribeImageTags =
  req
    "DescribeImageTags"
    "fixture/DescribeImageTags.yaml"

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

requestGetAuthorizationToken :: GetAuthorizationToken -> TestTree
requestGetAuthorizationToken =
  req
    "GetAuthorizationToken"
    "fixture/GetAuthorizationToken.yaml"

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

responsePutRegistryCatalogData :: PutRegistryCatalogDataResponse -> TestTree
responsePutRegistryCatalogData =
  res
    "PutRegistryCatalogDataResponse"
    "fixture/PutRegistryCatalogDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutRegistryCatalogData)

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

responseBatchCheckLayerAvailability :: BatchCheckLayerAvailabilityResponse -> TestTree
responseBatchCheckLayerAvailability =
  res
    "BatchCheckLayerAvailabilityResponse"
    "fixture/BatchCheckLayerAvailabilityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchCheckLayerAvailability)

responsePutRepositoryCatalogData :: PutRepositoryCatalogDataResponse -> TestTree
responsePutRepositoryCatalogData =
  res
    "PutRepositoryCatalogDataResponse"
    "fixture/PutRepositoryCatalogDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutRepositoryCatalogData)

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

responseDescribeRegistries :: DescribeRegistriesResponse -> TestTree
responseDescribeRegistries =
  res
    "DescribeRegistriesResponse"
    "fixture/DescribeRegistriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRegistries)

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

responseUploadLayerPart :: UploadLayerPartResponse -> TestTree
responseUploadLayerPart =
  res
    "UploadLayerPartResponse"
    "fixture/UploadLayerPartResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UploadLayerPart)

responseGetRepositoryCatalogData :: GetRepositoryCatalogDataResponse -> TestTree
responseGetRepositoryCatalogData =
  res
    "GetRepositoryCatalogDataResponse"
    "fixture/GetRepositoryCatalogDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRepositoryCatalogData)

responseGetRegistryCatalogData :: GetRegistryCatalogDataResponse -> TestTree
responseGetRegistryCatalogData =
  res
    "GetRegistryCatalogDataResponse"
    "fixture/GetRegistryCatalogDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRegistryCatalogData)

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

responseDescribeImageTags :: DescribeImageTagsResponse -> TestTree
responseDescribeImageTags =
  res
    "DescribeImageTagsResponse"
    "fixture/DescribeImageTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeImageTags)

responseDeleteRepository :: DeleteRepositoryResponse -> TestTree
responseDeleteRepository =
  res
    "DeleteRepositoryResponse"
    "fixture/DeleteRepositoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRepository)

responsePutImage :: PutImageResponse -> TestTree
responsePutImage =
  res
    "PutImageResponse"
    "fixture/PutImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutImage)

responseGetAuthorizationToken :: GetAuthorizationTokenResponse -> TestTree
responseGetAuthorizationToken =
  res
    "GetAuthorizationTokenResponse"
    "fixture/GetAuthorizationTokenResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAuthorizationToken)

responseDescribeImages :: DescribeImagesResponse -> TestTree
responseDescribeImages =
  res
    "DescribeImagesResponse"
    "fixture/DescribeImagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeImages)
