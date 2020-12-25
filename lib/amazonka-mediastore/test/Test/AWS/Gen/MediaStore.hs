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
--         , requestPutCorsPolicy $
--             mkPutCorsPolicy
--
--         , requestDeleteCorsPolicy $
--             mkDeleteCorsPolicy
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
--         , requestGetCorsPolicy $
--             mkGetCorsPolicy
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
--         , responsePutCorsPolicy $
--             mkPutCorsPolicyResponse
--
--         , responseDeleteCorsPolicy $
--             mkDeleteCorsPolicyResponse
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
--         , responseGetCorsPolicy $
--             mkGetCorsPolicyResponse
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

requestPutCorsPolicy :: PutCorsPolicy -> TestTree
requestPutCorsPolicy =
  req
    "PutCorsPolicy"
    "fixture/PutCorsPolicy.yaml"

requestDeleteCorsPolicy :: DeleteCorsPolicy -> TestTree
requestDeleteCorsPolicy =
  req
    "DeleteCorsPolicy"
    "fixture/DeleteCorsPolicy.yaml"

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

requestGetCorsPolicy :: GetCorsPolicy -> TestTree
requestGetCorsPolicy =
  req
    "GetCorsPolicy"
    "fixture/GetCorsPolicy.yaml"

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
    mkServiceConfig
    (Proxy :: Proxy StopAccessLogging)

responsePutLifecyclePolicy :: PutLifecyclePolicyResponse -> TestTree
responsePutLifecyclePolicy =
  res
    "PutLifecyclePolicyResponse"
    "fixture/PutLifecyclePolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutLifecyclePolicy)

responseDeleteLifecyclePolicy :: DeleteLifecyclePolicyResponse -> TestTree
responseDeleteLifecyclePolicy =
  res
    "DeleteLifecyclePolicyResponse"
    "fixture/DeleteLifecyclePolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteLifecyclePolicy)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTagsForResource)

responseCreateContainer :: CreateContainerResponse -> TestTree
responseCreateContainer =
  res
    "CreateContainerResponse"
    "fixture/CreateContainerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateContainer)

responseListContainers :: ListContainersResponse -> TestTree
responseListContainers =
  res
    "ListContainersResponse"
    "fixture/ListContainersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListContainers)

responseDeleteContainer :: DeleteContainerResponse -> TestTree
responseDeleteContainer =
  res
    "DeleteContainerResponse"
    "fixture/DeleteContainerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteContainer)

responsePutCorsPolicy :: PutCorsPolicyResponse -> TestTree
responsePutCorsPolicy =
  res
    "PutCorsPolicyResponse"
    "fixture/PutCorsPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutCorsPolicy)

responseDeleteCorsPolicy :: DeleteCorsPolicyResponse -> TestTree
responseDeleteCorsPolicy =
  res
    "DeleteCorsPolicyResponse"
    "fixture/DeleteCorsPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteCorsPolicy)

responseStartAccessLogging :: StartAccessLoggingResponse -> TestTree
responseStartAccessLogging =
  res
    "StartAccessLoggingResponse"
    "fixture/StartAccessLoggingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartAccessLogging)

responseDescribeContainer :: DescribeContainerResponse -> TestTree
responseDescribeContainer =
  res
    "DescribeContainerResponse"
    "fixture/DescribeContainerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeContainer)

responseGetMetricPolicy :: GetMetricPolicyResponse -> TestTree
responseGetMetricPolicy =
  res
    "GetMetricPolicyResponse"
    "fixture/GetMetricPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetMetricPolicy)

responseDeleteMetricPolicy :: DeleteMetricPolicyResponse -> TestTree
responseDeleteMetricPolicy =
  res
    "DeleteMetricPolicyResponse"
    "fixture/DeleteMetricPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteMetricPolicy)

responsePutMetricPolicy :: PutMetricPolicyResponse -> TestTree
responsePutMetricPolicy =
  res
    "PutMetricPolicyResponse"
    "fixture/PutMetricPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutMetricPolicy)

responseGetLifecyclePolicy :: GetLifecyclePolicyResponse -> TestTree
responseGetLifecyclePolicy =
  res
    "GetLifecyclePolicyResponse"
    "fixture/GetLifecyclePolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetLifecyclePolicy)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TagResource)

responseGetCorsPolicy :: GetCorsPolicyResponse -> TestTree
responseGetCorsPolicy =
  res
    "GetCorsPolicyResponse"
    "fixture/GetCorsPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetCorsPolicy)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UntagResource)

responseDeleteContainerPolicy :: DeleteContainerPolicyResponse -> TestTree
responseDeleteContainerPolicy =
  res
    "DeleteContainerPolicyResponse"
    "fixture/DeleteContainerPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteContainerPolicy)

responsePutContainerPolicy :: PutContainerPolicyResponse -> TestTree
responsePutContainerPolicy =
  res
    "PutContainerPolicyResponse"
    "fixture/PutContainerPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutContainerPolicy)

responseGetContainerPolicy :: GetContainerPolicyResponse -> TestTree
responseGetContainerPolicy =
  res
    "GetContainerPolicyResponse"
    "fixture/GetContainerPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetContainerPolicy)
