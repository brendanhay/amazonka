{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.MediaStore
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--             newStopAccessLogging
--
--         , requestPutLifecyclePolicy $
--             newPutLifecyclePolicy
--
--         , requestDeleteLifecyclePolicy $
--             newDeleteLifecyclePolicy
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestCreateContainer $
--             newCreateContainer
--
--         , requestListContainers $
--             newListContainers
--
--         , requestDeleteContainer $
--             newDeleteContainer
--
--         , requestPutCorsPolicy $
--             newPutCorsPolicy
--
--         , requestDeleteCorsPolicy $
--             newDeleteCorsPolicy
--
--         , requestStartAccessLogging $
--             newStartAccessLogging
--
--         , requestDescribeContainer $
--             newDescribeContainer
--
--         , requestGetMetricPolicy $
--             newGetMetricPolicy
--
--         , requestDeleteMetricPolicy $
--             newDeleteMetricPolicy
--
--         , requestPutMetricPolicy $
--             newPutMetricPolicy
--
--         , requestGetLifecyclePolicy $
--             newGetLifecyclePolicy
--
--         , requestTagResource $
--             newTagResource
--
--         , requestGetCorsPolicy $
--             newGetCorsPolicy
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDeleteContainerPolicy $
--             newDeleteContainerPolicy
--
--         , requestPutContainerPolicy $
--             newPutContainerPolicy
--
--         , requestGetContainerPolicy $
--             newGetContainerPolicy
--
--           ]

--     , testGroup "response"
--         [ responseStopAccessLogging $
--             newStopAccessLoggingResponse
--
--         , responsePutLifecyclePolicy $
--             newPutLifecyclePolicyResponse
--
--         , responseDeleteLifecyclePolicy $
--             newDeleteLifecyclePolicyResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseCreateContainer $
--             newCreateContainerResponse
--
--         , responseListContainers $
--             newListContainersResponse
--
--         , responseDeleteContainer $
--             newDeleteContainerResponse
--
--         , responsePutCorsPolicy $
--             newPutCorsPolicyResponse
--
--         , responseDeleteCorsPolicy $
--             newDeleteCorsPolicyResponse
--
--         , responseStartAccessLogging $
--             newStartAccessLoggingResponse
--
--         , responseDescribeContainer $
--             newDescribeContainerResponse
--
--         , responseGetMetricPolicy $
--             newGetMetricPolicyResponse
--
--         , responseDeleteMetricPolicy $
--             newDeleteMetricPolicyResponse
--
--         , responsePutMetricPolicy $
--             newPutMetricPolicyResponse
--
--         , responseGetLifecyclePolicy $
--             newGetLifecyclePolicyResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseGetCorsPolicy $
--             newGetCorsPolicyResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDeleteContainerPolicy $
--             newDeleteContainerPolicyResponse
--
--         , responsePutContainerPolicy $
--             newPutContainerPolicyResponse
--
--         , responseGetContainerPolicy $
--             newGetContainerPolicyResponse
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
    defaultService
    (Proxy :: Proxy StopAccessLogging)

responsePutLifecyclePolicy :: PutLifecyclePolicyResponse -> TestTree
responsePutLifecyclePolicy =
  res
    "PutLifecyclePolicyResponse"
    "fixture/PutLifecyclePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutLifecyclePolicy)

responseDeleteLifecyclePolicy :: DeleteLifecyclePolicyResponse -> TestTree
responseDeleteLifecyclePolicy =
  res
    "DeleteLifecyclePolicyResponse"
    "fixture/DeleteLifecyclePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteLifecyclePolicy)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseCreateContainer :: CreateContainerResponse -> TestTree
responseCreateContainer =
  res
    "CreateContainerResponse"
    "fixture/CreateContainerResponse.proto"
    defaultService
    (Proxy :: Proxy CreateContainer)

responseListContainers :: ListContainersResponse -> TestTree
responseListContainers =
  res
    "ListContainersResponse"
    "fixture/ListContainersResponse.proto"
    defaultService
    (Proxy :: Proxy ListContainers)

responseDeleteContainer :: DeleteContainerResponse -> TestTree
responseDeleteContainer =
  res
    "DeleteContainerResponse"
    "fixture/DeleteContainerResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteContainer)

responsePutCorsPolicy :: PutCorsPolicyResponse -> TestTree
responsePutCorsPolicy =
  res
    "PutCorsPolicyResponse"
    "fixture/PutCorsPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutCorsPolicy)

responseDeleteCorsPolicy :: DeleteCorsPolicyResponse -> TestTree
responseDeleteCorsPolicy =
  res
    "DeleteCorsPolicyResponse"
    "fixture/DeleteCorsPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCorsPolicy)

responseStartAccessLogging :: StartAccessLoggingResponse -> TestTree
responseStartAccessLogging =
  res
    "StartAccessLoggingResponse"
    "fixture/StartAccessLoggingResponse.proto"
    defaultService
    (Proxy :: Proxy StartAccessLogging)

responseDescribeContainer :: DescribeContainerResponse -> TestTree
responseDescribeContainer =
  res
    "DescribeContainerResponse"
    "fixture/DescribeContainerResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeContainer)

responseGetMetricPolicy :: GetMetricPolicyResponse -> TestTree
responseGetMetricPolicy =
  res
    "GetMetricPolicyResponse"
    "fixture/GetMetricPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetMetricPolicy)

responseDeleteMetricPolicy :: DeleteMetricPolicyResponse -> TestTree
responseDeleteMetricPolicy =
  res
    "DeleteMetricPolicyResponse"
    "fixture/DeleteMetricPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteMetricPolicy)

responsePutMetricPolicy :: PutMetricPolicyResponse -> TestTree
responsePutMetricPolicy =
  res
    "PutMetricPolicyResponse"
    "fixture/PutMetricPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutMetricPolicy)

responseGetLifecyclePolicy :: GetLifecyclePolicyResponse -> TestTree
responseGetLifecyclePolicy =
  res
    "GetLifecyclePolicyResponse"
    "fixture/GetLifecyclePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetLifecyclePolicy)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseGetCorsPolicy :: GetCorsPolicyResponse -> TestTree
responseGetCorsPolicy =
  res
    "GetCorsPolicyResponse"
    "fixture/GetCorsPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetCorsPolicy)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseDeleteContainerPolicy :: DeleteContainerPolicyResponse -> TestTree
responseDeleteContainerPolicy =
  res
    "DeleteContainerPolicyResponse"
    "fixture/DeleteContainerPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteContainerPolicy)

responsePutContainerPolicy :: PutContainerPolicyResponse -> TestTree
responsePutContainerPolicy =
  res
    "PutContainerPolicyResponse"
    "fixture/PutContainerPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutContainerPolicy)

responseGetContainerPolicy :: GetContainerPolicyResponse -> TestTree
responseGetContainerPolicy =
  res
    "GetContainerPolicyResponse"
    "fixture/GetContainerPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetContainerPolicy)
