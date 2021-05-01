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
--         [ requestPutLifecyclePolicy $
--             newPutLifecyclePolicy
--
--         , requestPutCorsPolicy $
--             newPutCorsPolicy
--
--         , requestPutContainerPolicy $
--             newPutContainerPolicy
--
--         , requestDeleteContainer $
--             newDeleteContainer
--
--         , requestGetCorsPolicy $
--             newGetCorsPolicy
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestTagResource $
--             newTagResource
--
--         , requestDeleteMetricPolicy $
--             newDeleteMetricPolicy
--
--         , requestDescribeContainer $
--             newDescribeContainer
--
--         , requestGetMetricPolicy $
--             newGetMetricPolicy
--
--         , requestStartAccessLogging $
--             newStartAccessLogging
--
--         , requestDeleteLifecyclePolicy $
--             newDeleteLifecyclePolicy
--
--         , requestStopAccessLogging $
--             newStopAccessLogging
--
--         , requestDeleteCorsPolicy $
--             newDeleteCorsPolicy
--
--         , requestGetContainerPolicy $
--             newGetContainerPolicy
--
--         , requestDeleteContainerPolicy $
--             newDeleteContainerPolicy
--
--         , requestListContainers $
--             newListContainers
--
--         , requestCreateContainer $
--             newCreateContainer
--
--         , requestGetLifecyclePolicy $
--             newGetLifecyclePolicy
--
--         , requestPutMetricPolicy $
--             newPutMetricPolicy
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--           ]

--     , testGroup "response"
--         [ responsePutLifecyclePolicy $
--             newPutLifecyclePolicyResponse
--
--         , responsePutCorsPolicy $
--             newPutCorsPolicyResponse
--
--         , responsePutContainerPolicy $
--             newPutContainerPolicyResponse
--
--         , responseDeleteContainer $
--             newDeleteContainerResponse
--
--         , responseGetCorsPolicy $
--             newGetCorsPolicyResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseDeleteMetricPolicy $
--             newDeleteMetricPolicyResponse
--
--         , responseDescribeContainer $
--             newDescribeContainerResponse
--
--         , responseGetMetricPolicy $
--             newGetMetricPolicyResponse
--
--         , responseStartAccessLogging $
--             newStartAccessLoggingResponse
--
--         , responseDeleteLifecyclePolicy $
--             newDeleteLifecyclePolicyResponse
--
--         , responseStopAccessLogging $
--             newStopAccessLoggingResponse
--
--         , responseDeleteCorsPolicy $
--             newDeleteCorsPolicyResponse
--
--         , responseGetContainerPolicy $
--             newGetContainerPolicyResponse
--
--         , responseDeleteContainerPolicy $
--             newDeleteContainerPolicyResponse
--
--         , responseListContainers $
--             newListContainersResponse
--
--         , responseCreateContainer $
--             newCreateContainerResponse
--
--         , responseGetLifecyclePolicy $
--             newGetLifecyclePolicyResponse
--
--         , responsePutMetricPolicy $
--             newPutMetricPolicyResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--           ]
--     ]

-- Requests

requestPutLifecyclePolicy :: PutLifecyclePolicy -> TestTree
requestPutLifecyclePolicy =
  req
    "PutLifecyclePolicy"
    "fixture/PutLifecyclePolicy.yaml"

requestPutCorsPolicy :: PutCorsPolicy -> TestTree
requestPutCorsPolicy =
  req
    "PutCorsPolicy"
    "fixture/PutCorsPolicy.yaml"

requestPutContainerPolicy :: PutContainerPolicy -> TestTree
requestPutContainerPolicy =
  req
    "PutContainerPolicy"
    "fixture/PutContainerPolicy.yaml"

requestDeleteContainer :: DeleteContainer -> TestTree
requestDeleteContainer =
  req
    "DeleteContainer"
    "fixture/DeleteContainer.yaml"

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

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestDeleteMetricPolicy :: DeleteMetricPolicy -> TestTree
requestDeleteMetricPolicy =
  req
    "DeleteMetricPolicy"
    "fixture/DeleteMetricPolicy.yaml"

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

requestStartAccessLogging :: StartAccessLogging -> TestTree
requestStartAccessLogging =
  req
    "StartAccessLogging"
    "fixture/StartAccessLogging.yaml"

requestDeleteLifecyclePolicy :: DeleteLifecyclePolicy -> TestTree
requestDeleteLifecyclePolicy =
  req
    "DeleteLifecyclePolicy"
    "fixture/DeleteLifecyclePolicy.yaml"

requestStopAccessLogging :: StopAccessLogging -> TestTree
requestStopAccessLogging =
  req
    "StopAccessLogging"
    "fixture/StopAccessLogging.yaml"

requestDeleteCorsPolicy :: DeleteCorsPolicy -> TestTree
requestDeleteCorsPolicy =
  req
    "DeleteCorsPolicy"
    "fixture/DeleteCorsPolicy.yaml"

requestGetContainerPolicy :: GetContainerPolicy -> TestTree
requestGetContainerPolicy =
  req
    "GetContainerPolicy"
    "fixture/GetContainerPolicy.yaml"

requestDeleteContainerPolicy :: DeleteContainerPolicy -> TestTree
requestDeleteContainerPolicy =
  req
    "DeleteContainerPolicy"
    "fixture/DeleteContainerPolicy.yaml"

requestListContainers :: ListContainers -> TestTree
requestListContainers =
  req
    "ListContainers"
    "fixture/ListContainers.yaml"

requestCreateContainer :: CreateContainer -> TestTree
requestCreateContainer =
  req
    "CreateContainer"
    "fixture/CreateContainer.yaml"

requestGetLifecyclePolicy :: GetLifecyclePolicy -> TestTree
requestGetLifecyclePolicy =
  req
    "GetLifecyclePolicy"
    "fixture/GetLifecyclePolicy.yaml"

requestPutMetricPolicy :: PutMetricPolicy -> TestTree
requestPutMetricPolicy =
  req
    "PutMetricPolicy"
    "fixture/PutMetricPolicy.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

-- Responses

responsePutLifecyclePolicy :: PutLifecyclePolicyResponse -> TestTree
responsePutLifecyclePolicy =
  res
    "PutLifecyclePolicyResponse"
    "fixture/PutLifecyclePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutLifecyclePolicy)

responsePutCorsPolicy :: PutCorsPolicyResponse -> TestTree
responsePutCorsPolicy =
  res
    "PutCorsPolicyResponse"
    "fixture/PutCorsPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutCorsPolicy)

responsePutContainerPolicy :: PutContainerPolicyResponse -> TestTree
responsePutContainerPolicy =
  res
    "PutContainerPolicyResponse"
    "fixture/PutContainerPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutContainerPolicy)

responseDeleteContainer :: DeleteContainerResponse -> TestTree
responseDeleteContainer =
  res
    "DeleteContainerResponse"
    "fixture/DeleteContainerResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteContainer)

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

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseDeleteMetricPolicy :: DeleteMetricPolicyResponse -> TestTree
responseDeleteMetricPolicy =
  res
    "DeleteMetricPolicyResponse"
    "fixture/DeleteMetricPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteMetricPolicy)

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

responseStartAccessLogging :: StartAccessLoggingResponse -> TestTree
responseStartAccessLogging =
  res
    "StartAccessLoggingResponse"
    "fixture/StartAccessLoggingResponse.proto"
    defaultService
    (Proxy :: Proxy StartAccessLogging)

responseDeleteLifecyclePolicy :: DeleteLifecyclePolicyResponse -> TestTree
responseDeleteLifecyclePolicy =
  res
    "DeleteLifecyclePolicyResponse"
    "fixture/DeleteLifecyclePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteLifecyclePolicy)

responseStopAccessLogging :: StopAccessLoggingResponse -> TestTree
responseStopAccessLogging =
  res
    "StopAccessLoggingResponse"
    "fixture/StopAccessLoggingResponse.proto"
    defaultService
    (Proxy :: Proxy StopAccessLogging)

responseDeleteCorsPolicy :: DeleteCorsPolicyResponse -> TestTree
responseDeleteCorsPolicy =
  res
    "DeleteCorsPolicyResponse"
    "fixture/DeleteCorsPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCorsPolicy)

responseGetContainerPolicy :: GetContainerPolicyResponse -> TestTree
responseGetContainerPolicy =
  res
    "GetContainerPolicyResponse"
    "fixture/GetContainerPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetContainerPolicy)

responseDeleteContainerPolicy :: DeleteContainerPolicyResponse -> TestTree
responseDeleteContainerPolicy =
  res
    "DeleteContainerPolicyResponse"
    "fixture/DeleteContainerPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteContainerPolicy)

responseListContainers :: ListContainersResponse -> TestTree
responseListContainers =
  res
    "ListContainersResponse"
    "fixture/ListContainersResponse.proto"
    defaultService
    (Proxy :: Proxy ListContainers)

responseCreateContainer :: CreateContainerResponse -> TestTree
responseCreateContainer =
  res
    "CreateContainerResponse"
    "fixture/CreateContainerResponse.proto"
    defaultService
    (Proxy :: Proxy CreateContainer)

responseGetLifecyclePolicy :: GetLifecyclePolicyResponse -> TestTree
responseGetLifecyclePolicy =
  res
    "GetLifecyclePolicyResponse"
    "fixture/GetLifecyclePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetLifecyclePolicy)

responsePutMetricPolicy :: PutMetricPolicyResponse -> TestTree
responsePutMetricPolicy =
  res
    "PutMetricPolicyResponse"
    "fixture/PutMetricPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutMetricPolicy)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)
