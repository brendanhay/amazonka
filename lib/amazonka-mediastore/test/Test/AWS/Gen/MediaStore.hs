{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.MediaStore
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.MediaStore where

import Data.Proxy
import Network.AWS.MediaStore
import Test.AWS.Fixture
import Test.AWS.MediaStore.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestStopAccessLogging $
--             mkStopAccessLogging
--
--         , requestPutLifecyclePolicy $
--             mkPutLifecyclePolicy
--
--         , requestDeleteLifecyclePolicy $
--             mkDeleteLifecyclePolicy
--
--         , requestListTagsForResource $
--             mkListTagsForResource
--
--         , requestCreateContainer $
--             mkCreateContainer
--
--         , requestListContainers $
--             mkListContainers
--
--         , requestDeleteContainer $
--             mkDeleteContainer
--
--         , requestPutCORSPolicy $
--             mkPutCORSPolicy
--
--         , requestDeleteCORSPolicy $
--             mkDeleteCORSPolicy
--
--         , requestStartAccessLogging $
--             mkStartAccessLogging
--
--         , requestDescribeContainer $
--             mkDescribeContainer
--
--         , requestGetMetricPolicy $
--             mkGetMetricPolicy
--
--         , requestDeleteMetricPolicy $
--             mkDeleteMetricPolicy
--
--         , requestPutMetricPolicy $
--             mkPutMetricPolicy
--
--         , requestGetLifecyclePolicy $
--             mkGetLifecyclePolicy
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestGetCORSPolicy $
--             mkGetCORSPolicy
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestDeleteContainerPolicy $
--             mkDeleteContainerPolicy
--
--         , requestPutContainerPolicy $
--             mkPutContainerPolicy
--
--         , requestGetContainerPolicy $
--             mkGetContainerPolicy
--
--           ]

--     , testGroup "response"
--         [ responseStopAccessLogging $
--             mkStopAccessLoggingResponse
--
--         , responsePutLifecyclePolicy $
--             mkPutLifecyclePolicyResponse
--
--         , responseDeleteLifecyclePolicy $
--             mkDeleteLifecyclePolicyResponse
--
--         , responseListTagsForResource $
--             mkListTagsForResourceResponse
--
--         , responseCreateContainer $
--             mkCreateContainerResponse
--
--         , responseListContainers $
--             mkListContainersResponse
--
--         , responseDeleteContainer $
--             mkDeleteContainerResponse
--
--         , responsePutCORSPolicy $
--             mkPutCORSPolicyResponse
--
--         , responseDeleteCORSPolicy $
--             mkDeleteCORSPolicyResponse
--
--         , responseStartAccessLogging $
--             mkStartAccessLoggingResponse
--
--         , responseDescribeContainer $
--             mkDescribeContainerResponse
--
--         , responseGetMetricPolicy $
--             mkGetMetricPolicyResponse
--
--         , responseDeleteMetricPolicy $
--             mkDeleteMetricPolicyResponse
--
--         , responsePutMetricPolicy $
--             mkPutMetricPolicyResponse
--
--         , responseGetLifecyclePolicy $
--             mkGetLifecyclePolicyResponse
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responseGetCORSPolicy $
--             mkGetCORSPolicyResponse
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responseDeleteContainerPolicy $
--             mkDeleteContainerPolicyResponse
--
--         , responsePutContainerPolicy $
--             mkPutContainerPolicyResponse
--
--         , responseGetContainerPolicy $
--             mkGetContainerPolicyResponse
--
--           ]
--     ]

-- Requests

requestStopAccessLogging :: StopAccessLogging -> TestTree
requestStopAccessLogging =
  req
    "StopAccessLogging"
    "fixture/StopAccessLogging.yaml"

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

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestCreateContainer :: CreateContainer -> TestTree
requestCreateContainer =
  req
    "CreateContainer"
    "fixture/CreateContainer.yaml"

requestListContainers :: ListContainers -> TestTree
requestListContainers =
  req
    "ListContainers"
    "fixture/ListContainers.yaml"

requestDeleteContainer :: DeleteContainer -> TestTree
requestDeleteContainer =
  req
    "DeleteContainer"
    "fixture/DeleteContainer.yaml"

requestPutCORSPolicy :: PutCORSPolicy -> TestTree
requestPutCORSPolicy =
  req
    "PutCORSPolicy"
    "fixture/PutCORSPolicy.yaml"

requestDeleteCORSPolicy :: DeleteCORSPolicy -> TestTree
requestDeleteCORSPolicy =
  req
    "DeleteCORSPolicy"
    "fixture/DeleteCORSPolicy.yaml"

requestStartAccessLogging :: StartAccessLogging -> TestTree
requestStartAccessLogging =
  req
    "StartAccessLogging"
    "fixture/StartAccessLogging.yaml"

requestDescribeContainer :: DescribeContainer -> TestTree
requestDescribeContainer =
  req
    "DescribeContainer"
    "fixture/DescribeContainer.yaml"

requestGetMetricPolicy :: GetMetricPolicy -> TestTree
requestGetMetricPolicy =
  req
    "GetMetricPolicy"
    "fixture/GetMetricPolicy.yaml"

requestDeleteMetricPolicy :: DeleteMetricPolicy -> TestTree
requestDeleteMetricPolicy =
  req
    "DeleteMetricPolicy"
    "fixture/DeleteMetricPolicy.yaml"

requestPutMetricPolicy :: PutMetricPolicy -> TestTree
requestPutMetricPolicy =
  req
    "PutMetricPolicy"
    "fixture/PutMetricPolicy.yaml"

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

requestGetCORSPolicy :: GetCORSPolicy -> TestTree
requestGetCORSPolicy =
  req
    "GetCORSPolicy"
    "fixture/GetCORSPolicy.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDeleteContainerPolicy :: DeleteContainerPolicy -> TestTree
requestDeleteContainerPolicy =
  req
    "DeleteContainerPolicy"
    "fixture/DeleteContainerPolicy.yaml"

requestPutContainerPolicy :: PutContainerPolicy -> TestTree
requestPutContainerPolicy =
  req
    "PutContainerPolicy"
    "fixture/PutContainerPolicy.yaml"

requestGetContainerPolicy :: GetContainerPolicy -> TestTree
requestGetContainerPolicy =
  req
    "GetContainerPolicy"
    "fixture/GetContainerPolicy.yaml"

-- Responses

responseStopAccessLogging :: StopAccessLoggingResponse -> TestTree
responseStopAccessLogging =
  res
    "StopAccessLoggingResponse"
    "fixture/StopAccessLoggingResponse.proto"
    mediaStoreService
    (Proxy :: Proxy StopAccessLogging)

responsePutLifecyclePolicy :: PutLifecyclePolicyResponse -> TestTree
responsePutLifecyclePolicy =
  res
    "PutLifecyclePolicyResponse"
    "fixture/PutLifecyclePolicyResponse.proto"
    mediaStoreService
    (Proxy :: Proxy PutLifecyclePolicy)

responseDeleteLifecyclePolicy :: DeleteLifecyclePolicyResponse -> TestTree
responseDeleteLifecyclePolicy =
  res
    "DeleteLifecyclePolicyResponse"
    "fixture/DeleteLifecyclePolicyResponse.proto"
    mediaStoreService
    (Proxy :: Proxy DeleteLifecyclePolicy)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    mediaStoreService
    (Proxy :: Proxy ListTagsForResource)

responseCreateContainer :: CreateContainerResponse -> TestTree
responseCreateContainer =
  res
    "CreateContainerResponse"
    "fixture/CreateContainerResponse.proto"
    mediaStoreService
    (Proxy :: Proxy CreateContainer)

responseListContainers :: ListContainersResponse -> TestTree
responseListContainers =
  res
    "ListContainersResponse"
    "fixture/ListContainersResponse.proto"
    mediaStoreService
    (Proxy :: Proxy ListContainers)

responseDeleteContainer :: DeleteContainerResponse -> TestTree
responseDeleteContainer =
  res
    "DeleteContainerResponse"
    "fixture/DeleteContainerResponse.proto"
    mediaStoreService
    (Proxy :: Proxy DeleteContainer)

responsePutCORSPolicy :: PutCORSPolicyResponse -> TestTree
responsePutCORSPolicy =
  res
    "PutCORSPolicyResponse"
    "fixture/PutCORSPolicyResponse.proto"
    mediaStoreService
    (Proxy :: Proxy PutCORSPolicy)

responseDeleteCORSPolicy :: DeleteCORSPolicyResponse -> TestTree
responseDeleteCORSPolicy =
  res
    "DeleteCORSPolicyResponse"
    "fixture/DeleteCORSPolicyResponse.proto"
    mediaStoreService
    (Proxy :: Proxy DeleteCORSPolicy)

responseStartAccessLogging :: StartAccessLoggingResponse -> TestTree
responseStartAccessLogging =
  res
    "StartAccessLoggingResponse"
    "fixture/StartAccessLoggingResponse.proto"
    mediaStoreService
    (Proxy :: Proxy StartAccessLogging)

responseDescribeContainer :: DescribeContainerResponse -> TestTree
responseDescribeContainer =
  res
    "DescribeContainerResponse"
    "fixture/DescribeContainerResponse.proto"
    mediaStoreService
    (Proxy :: Proxy DescribeContainer)

responseGetMetricPolicy :: GetMetricPolicyResponse -> TestTree
responseGetMetricPolicy =
  res
    "GetMetricPolicyResponse"
    "fixture/GetMetricPolicyResponse.proto"
    mediaStoreService
    (Proxy :: Proxy GetMetricPolicy)

responseDeleteMetricPolicy :: DeleteMetricPolicyResponse -> TestTree
responseDeleteMetricPolicy =
  res
    "DeleteMetricPolicyResponse"
    "fixture/DeleteMetricPolicyResponse.proto"
    mediaStoreService
    (Proxy :: Proxy DeleteMetricPolicy)

responsePutMetricPolicy :: PutMetricPolicyResponse -> TestTree
responsePutMetricPolicy =
  res
    "PutMetricPolicyResponse"
    "fixture/PutMetricPolicyResponse.proto"
    mediaStoreService
    (Proxy :: Proxy PutMetricPolicy)

responseGetLifecyclePolicy :: GetLifecyclePolicyResponse -> TestTree
responseGetLifecyclePolicy =
  res
    "GetLifecyclePolicyResponse"
    "fixture/GetLifecyclePolicyResponse.proto"
    mediaStoreService
    (Proxy :: Proxy GetLifecyclePolicy)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    mediaStoreService
    (Proxy :: Proxy TagResource)

responseGetCORSPolicy :: GetCORSPolicyResponse -> TestTree
responseGetCORSPolicy =
  res
    "GetCORSPolicyResponse"
    "fixture/GetCORSPolicyResponse.proto"
    mediaStoreService
    (Proxy :: Proxy GetCORSPolicy)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    mediaStoreService
    (Proxy :: Proxy UntagResource)

responseDeleteContainerPolicy :: DeleteContainerPolicyResponse -> TestTree
responseDeleteContainerPolicy =
  res
    "DeleteContainerPolicyResponse"
    "fixture/DeleteContainerPolicyResponse.proto"
    mediaStoreService
    (Proxy :: Proxy DeleteContainerPolicy)

responsePutContainerPolicy :: PutContainerPolicyResponse -> TestTree
responsePutContainerPolicy =
  res
    "PutContainerPolicyResponse"
    "fixture/PutContainerPolicyResponse.proto"
    mediaStoreService
    (Proxy :: Proxy PutContainerPolicy)

responseGetContainerPolicy :: GetContainerPolicyResponse -> TestTree
responseGetContainerPolicy =
  res
    "GetContainerPolicyResponse"
    "fixture/GetContainerPolicyResponse.proto"
    mediaStoreService
    (Proxy :: Proxy GetContainerPolicy)
