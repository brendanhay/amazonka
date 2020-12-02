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
--             stopAccessLogging
--
--         , requestPutLifecyclePolicy $
--             putLifecyclePolicy
--
--         , requestDeleteLifecyclePolicy $
--             deleteLifecyclePolicy
--
--         , requestListTagsForResource $
--             listTagsForResource
--
--         , requestCreateContainer $
--             createContainer
--
--         , requestListContainers $
--             listContainers
--
--         , requestDeleteContainer $
--             deleteContainer
--
--         , requestPutCORSPolicy $
--             putCORSPolicy
--
--         , requestDeleteCORSPolicy $
--             deleteCORSPolicy
--
--         , requestStartAccessLogging $
--             startAccessLogging
--
--         , requestDescribeContainer $
--             describeContainer
--
--         , requestGetMetricPolicy $
--             getMetricPolicy
--
--         , requestDeleteMetricPolicy $
--             deleteMetricPolicy
--
--         , requestPutMetricPolicy $
--             putMetricPolicy
--
--         , requestGetLifecyclePolicy $
--             getLifecyclePolicy
--
--         , requestTagResource $
--             tagResource
--
--         , requestGetCORSPolicy $
--             getCORSPolicy
--
--         , requestUntagResource $
--             untagResource
--
--         , requestDeleteContainerPolicy $
--             deleteContainerPolicy
--
--         , requestPutContainerPolicy $
--             putContainerPolicy
--
--         , requestGetContainerPolicy $
--             getContainerPolicy
--
--           ]

--     , testGroup "response"
--         [ responseStopAccessLogging $
--             stopAccessLoggingResponse
--
--         , responsePutLifecyclePolicy $
--             putLifecyclePolicyResponse
--
--         , responseDeleteLifecyclePolicy $
--             deleteLifecyclePolicyResponse
--
--         , responseListTagsForResource $
--             listTagsForResourceResponse
--
--         , responseCreateContainer $
--             createContainerResponse
--
--         , responseListContainers $
--             listContainersResponse
--
--         , responseDeleteContainer $
--             deleteContainerResponse
--
--         , responsePutCORSPolicy $
--             putCORSPolicyResponse
--
--         , responseDeleteCORSPolicy $
--             deleteCORSPolicyResponse
--
--         , responseStartAccessLogging $
--             startAccessLoggingResponse
--
--         , responseDescribeContainer $
--             describeContainerResponse
--
--         , responseGetMetricPolicy $
--             getMetricPolicyResponse
--
--         , responseDeleteMetricPolicy $
--             deleteMetricPolicyResponse
--
--         , responsePutMetricPolicy $
--             putMetricPolicyResponse
--
--         , responseGetLifecyclePolicy $
--             getLifecyclePolicyResponse
--
--         , responseTagResource $
--             tagResourceResponse
--
--         , responseGetCORSPolicy $
--             getCORSPolicyResponse
--
--         , responseUntagResource $
--             untagResourceResponse
--
--         , responseDeleteContainerPolicy $
--             deleteContainerPolicyResponse
--
--         , responsePutContainerPolicy $
--             putContainerPolicyResponse
--
--         , responseGetContainerPolicy $
--             getContainerPolicyResponse
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
    mediaStore
    (Proxy :: Proxy StopAccessLogging)

responsePutLifecyclePolicy :: PutLifecyclePolicyResponse -> TestTree
responsePutLifecyclePolicy =
  res
    "PutLifecyclePolicyResponse"
    "fixture/PutLifecyclePolicyResponse.proto"
    mediaStore
    (Proxy :: Proxy PutLifecyclePolicy)

responseDeleteLifecyclePolicy :: DeleteLifecyclePolicyResponse -> TestTree
responseDeleteLifecyclePolicy =
  res
    "DeleteLifecyclePolicyResponse"
    "fixture/DeleteLifecyclePolicyResponse.proto"
    mediaStore
    (Proxy :: Proxy DeleteLifecyclePolicy)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    mediaStore
    (Proxy :: Proxy ListTagsForResource)

responseCreateContainer :: CreateContainerResponse -> TestTree
responseCreateContainer =
  res
    "CreateContainerResponse"
    "fixture/CreateContainerResponse.proto"
    mediaStore
    (Proxy :: Proxy CreateContainer)

responseListContainers :: ListContainersResponse -> TestTree
responseListContainers =
  res
    "ListContainersResponse"
    "fixture/ListContainersResponse.proto"
    mediaStore
    (Proxy :: Proxy ListContainers)

responseDeleteContainer :: DeleteContainerResponse -> TestTree
responseDeleteContainer =
  res
    "DeleteContainerResponse"
    "fixture/DeleteContainerResponse.proto"
    mediaStore
    (Proxy :: Proxy DeleteContainer)

responsePutCORSPolicy :: PutCORSPolicyResponse -> TestTree
responsePutCORSPolicy =
  res
    "PutCORSPolicyResponse"
    "fixture/PutCORSPolicyResponse.proto"
    mediaStore
    (Proxy :: Proxy PutCORSPolicy)

responseDeleteCORSPolicy :: DeleteCORSPolicyResponse -> TestTree
responseDeleteCORSPolicy =
  res
    "DeleteCORSPolicyResponse"
    "fixture/DeleteCORSPolicyResponse.proto"
    mediaStore
    (Proxy :: Proxy DeleteCORSPolicy)

responseStartAccessLogging :: StartAccessLoggingResponse -> TestTree
responseStartAccessLogging =
  res
    "StartAccessLoggingResponse"
    "fixture/StartAccessLoggingResponse.proto"
    mediaStore
    (Proxy :: Proxy StartAccessLogging)

responseDescribeContainer :: DescribeContainerResponse -> TestTree
responseDescribeContainer =
  res
    "DescribeContainerResponse"
    "fixture/DescribeContainerResponse.proto"
    mediaStore
    (Proxy :: Proxy DescribeContainer)

responseGetMetricPolicy :: GetMetricPolicyResponse -> TestTree
responseGetMetricPolicy =
  res
    "GetMetricPolicyResponse"
    "fixture/GetMetricPolicyResponse.proto"
    mediaStore
    (Proxy :: Proxy GetMetricPolicy)

responseDeleteMetricPolicy :: DeleteMetricPolicyResponse -> TestTree
responseDeleteMetricPolicy =
  res
    "DeleteMetricPolicyResponse"
    "fixture/DeleteMetricPolicyResponse.proto"
    mediaStore
    (Proxy :: Proxy DeleteMetricPolicy)

responsePutMetricPolicy :: PutMetricPolicyResponse -> TestTree
responsePutMetricPolicy =
  res
    "PutMetricPolicyResponse"
    "fixture/PutMetricPolicyResponse.proto"
    mediaStore
    (Proxy :: Proxy PutMetricPolicy)

responseGetLifecyclePolicy :: GetLifecyclePolicyResponse -> TestTree
responseGetLifecyclePolicy =
  res
    "GetLifecyclePolicyResponse"
    "fixture/GetLifecyclePolicyResponse.proto"
    mediaStore
    (Proxy :: Proxy GetLifecyclePolicy)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    mediaStore
    (Proxy :: Proxy TagResource)

responseGetCORSPolicy :: GetCORSPolicyResponse -> TestTree
responseGetCORSPolicy =
  res
    "GetCORSPolicyResponse"
    "fixture/GetCORSPolicyResponse.proto"
    mediaStore
    (Proxy :: Proxy GetCORSPolicy)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    mediaStore
    (Proxy :: Proxy UntagResource)

responseDeleteContainerPolicy :: DeleteContainerPolicyResponse -> TestTree
responseDeleteContainerPolicy =
  res
    "DeleteContainerPolicyResponse"
    "fixture/DeleteContainerPolicyResponse.proto"
    mediaStore
    (Proxy :: Proxy DeleteContainerPolicy)

responsePutContainerPolicy :: PutContainerPolicyResponse -> TestTree
responsePutContainerPolicy =
  res
    "PutContainerPolicyResponse"
    "fixture/PutContainerPolicyResponse.proto"
    mediaStore
    (Proxy :: Proxy PutContainerPolicy)

responseGetContainerPolicy :: GetContainerPolicyResponse -> TestTree
responseGetContainerPolicy =
  res
    "GetContainerPolicyResponse"
    "fixture/GetContainerPolicyResponse.proto"
    mediaStore
    (Proxy :: Proxy GetContainerPolicy)
