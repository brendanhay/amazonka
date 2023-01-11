{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.MediaStore
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.MediaStore where

import Amazonka.MediaStore
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.MediaStore.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateContainer $
--             newCreateContainer
--
--         , requestDeleteContainer $
--             newDeleteContainer
--
--         , requestDeleteContainerPolicy $
--             newDeleteContainerPolicy
--
--         , requestDeleteCorsPolicy $
--             newDeleteCorsPolicy
--
--         , requestDeleteLifecyclePolicy $
--             newDeleteLifecyclePolicy
--
--         , requestDeleteMetricPolicy $
--             newDeleteMetricPolicy
--
--         , requestDescribeContainer $
--             newDescribeContainer
--
--         , requestGetContainerPolicy $
--             newGetContainerPolicy
--
--         , requestGetCorsPolicy $
--             newGetCorsPolicy
--
--         , requestGetLifecyclePolicy $
--             newGetLifecyclePolicy
--
--         , requestGetMetricPolicy $
--             newGetMetricPolicy
--
--         , requestListContainers $
--             newListContainers
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPutContainerPolicy $
--             newPutContainerPolicy
--
--         , requestPutCorsPolicy $
--             newPutCorsPolicy
--
--         , requestPutLifecyclePolicy $
--             newPutLifecyclePolicy
--
--         , requestPutMetricPolicy $
--             newPutMetricPolicy
--
--         , requestStartAccessLogging $
--             newStartAccessLogging
--
--         , requestStopAccessLogging $
--             newStopAccessLogging
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--           ]

--     , testGroup "response"
--         [ responseCreateContainer $
--             newCreateContainerResponse
--
--         , responseDeleteContainer $
--             newDeleteContainerResponse
--
--         , responseDeleteContainerPolicy $
--             newDeleteContainerPolicyResponse
--
--         , responseDeleteCorsPolicy $
--             newDeleteCorsPolicyResponse
--
--         , responseDeleteLifecyclePolicy $
--             newDeleteLifecyclePolicyResponse
--
--         , responseDeleteMetricPolicy $
--             newDeleteMetricPolicyResponse
--
--         , responseDescribeContainer $
--             newDescribeContainerResponse
--
--         , responseGetContainerPolicy $
--             newGetContainerPolicyResponse
--
--         , responseGetCorsPolicy $
--             newGetCorsPolicyResponse
--
--         , responseGetLifecyclePolicy $
--             newGetLifecyclePolicyResponse
--
--         , responseGetMetricPolicy $
--             newGetMetricPolicyResponse
--
--         , responseListContainers $
--             newListContainersResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePutContainerPolicy $
--             newPutContainerPolicyResponse
--
--         , responsePutCorsPolicy $
--             newPutCorsPolicyResponse
--
--         , responsePutLifecyclePolicy $
--             newPutLifecyclePolicyResponse
--
--         , responsePutMetricPolicy $
--             newPutMetricPolicyResponse
--
--         , responseStartAccessLogging $
--             newStartAccessLoggingResponse
--
--         , responseStopAccessLogging $
--             newStopAccessLoggingResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--           ]
--     ]

-- Requests

requestCreateContainer :: CreateContainer -> TestTree
requestCreateContainer =
  req
    "CreateContainer"
    "fixture/CreateContainer.yaml"

requestDeleteContainer :: DeleteContainer -> TestTree
requestDeleteContainer =
  req
    "DeleteContainer"
    "fixture/DeleteContainer.yaml"

requestDeleteContainerPolicy :: DeleteContainerPolicy -> TestTree
requestDeleteContainerPolicy =
  req
    "DeleteContainerPolicy"
    "fixture/DeleteContainerPolicy.yaml"

requestDeleteCorsPolicy :: DeleteCorsPolicy -> TestTree
requestDeleteCorsPolicy =
  req
    "DeleteCorsPolicy"
    "fixture/DeleteCorsPolicy.yaml"

requestDeleteLifecyclePolicy :: DeleteLifecyclePolicy -> TestTree
requestDeleteLifecyclePolicy =
  req
    "DeleteLifecyclePolicy"
    "fixture/DeleteLifecyclePolicy.yaml"

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

requestGetContainerPolicy :: GetContainerPolicy -> TestTree
requestGetContainerPolicy =
  req
    "GetContainerPolicy"
    "fixture/GetContainerPolicy.yaml"

requestGetCorsPolicy :: GetCorsPolicy -> TestTree
requestGetCorsPolicy =
  req
    "GetCorsPolicy"
    "fixture/GetCorsPolicy.yaml"

requestGetLifecyclePolicy :: GetLifecyclePolicy -> TestTree
requestGetLifecyclePolicy =
  req
    "GetLifecyclePolicy"
    "fixture/GetLifecyclePolicy.yaml"

requestGetMetricPolicy :: GetMetricPolicy -> TestTree
requestGetMetricPolicy =
  req
    "GetMetricPolicy"
    "fixture/GetMetricPolicy.yaml"

requestListContainers :: ListContainers -> TestTree
requestListContainers =
  req
    "ListContainers"
    "fixture/ListContainers.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPutContainerPolicy :: PutContainerPolicy -> TestTree
requestPutContainerPolicy =
  req
    "PutContainerPolicy"
    "fixture/PutContainerPolicy.yaml"

requestPutCorsPolicy :: PutCorsPolicy -> TestTree
requestPutCorsPolicy =
  req
    "PutCorsPolicy"
    "fixture/PutCorsPolicy.yaml"

requestPutLifecyclePolicy :: PutLifecyclePolicy -> TestTree
requestPutLifecyclePolicy =
  req
    "PutLifecyclePolicy"
    "fixture/PutLifecyclePolicy.yaml"

requestPutMetricPolicy :: PutMetricPolicy -> TestTree
requestPutMetricPolicy =
  req
    "PutMetricPolicy"
    "fixture/PutMetricPolicy.yaml"

requestStartAccessLogging :: StartAccessLogging -> TestTree
requestStartAccessLogging =
  req
    "StartAccessLogging"
    "fixture/StartAccessLogging.yaml"

requestStopAccessLogging :: StopAccessLogging -> TestTree
requestStopAccessLogging =
  req
    "StopAccessLogging"
    "fixture/StopAccessLogging.yaml"

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

-- Responses

responseCreateContainer :: CreateContainerResponse -> TestTree
responseCreateContainer =
  res
    "CreateContainerResponse"
    "fixture/CreateContainerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateContainer)

responseDeleteContainer :: DeleteContainerResponse -> TestTree
responseDeleteContainer =
  res
    "DeleteContainerResponse"
    "fixture/DeleteContainerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteContainer)

responseDeleteContainerPolicy :: DeleteContainerPolicyResponse -> TestTree
responseDeleteContainerPolicy =
  res
    "DeleteContainerPolicyResponse"
    "fixture/DeleteContainerPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteContainerPolicy)

responseDeleteCorsPolicy :: DeleteCorsPolicyResponse -> TestTree
responseDeleteCorsPolicy =
  res
    "DeleteCorsPolicyResponse"
    "fixture/DeleteCorsPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCorsPolicy)

responseDeleteLifecyclePolicy :: DeleteLifecyclePolicyResponse -> TestTree
responseDeleteLifecyclePolicy =
  res
    "DeleteLifecyclePolicyResponse"
    "fixture/DeleteLifecyclePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLifecyclePolicy)

responseDeleteMetricPolicy :: DeleteMetricPolicyResponse -> TestTree
responseDeleteMetricPolicy =
  res
    "DeleteMetricPolicyResponse"
    "fixture/DeleteMetricPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMetricPolicy)

responseDescribeContainer :: DescribeContainerResponse -> TestTree
responseDescribeContainer =
  res
    "DescribeContainerResponse"
    "fixture/DescribeContainerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeContainer)

responseGetContainerPolicy :: GetContainerPolicyResponse -> TestTree
responseGetContainerPolicy =
  res
    "GetContainerPolicyResponse"
    "fixture/GetContainerPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetContainerPolicy)

responseGetCorsPolicy :: GetCorsPolicyResponse -> TestTree
responseGetCorsPolicy =
  res
    "GetCorsPolicyResponse"
    "fixture/GetCorsPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCorsPolicy)

responseGetLifecyclePolicy :: GetLifecyclePolicyResponse -> TestTree
responseGetLifecyclePolicy =
  res
    "GetLifecyclePolicyResponse"
    "fixture/GetLifecyclePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLifecyclePolicy)

responseGetMetricPolicy :: GetMetricPolicyResponse -> TestTree
responseGetMetricPolicy =
  res
    "GetMetricPolicyResponse"
    "fixture/GetMetricPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMetricPolicy)

responseListContainers :: ListContainersResponse -> TestTree
responseListContainers =
  res
    "ListContainersResponse"
    "fixture/ListContainersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListContainers)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePutContainerPolicy :: PutContainerPolicyResponse -> TestTree
responsePutContainerPolicy =
  res
    "PutContainerPolicyResponse"
    "fixture/PutContainerPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutContainerPolicy)

responsePutCorsPolicy :: PutCorsPolicyResponse -> TestTree
responsePutCorsPolicy =
  res
    "PutCorsPolicyResponse"
    "fixture/PutCorsPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutCorsPolicy)

responsePutLifecyclePolicy :: PutLifecyclePolicyResponse -> TestTree
responsePutLifecyclePolicy =
  res
    "PutLifecyclePolicyResponse"
    "fixture/PutLifecyclePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutLifecyclePolicy)

responsePutMetricPolicy :: PutMetricPolicyResponse -> TestTree
responsePutMetricPolicy =
  res
    "PutMetricPolicyResponse"
    "fixture/PutMetricPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutMetricPolicy)

responseStartAccessLogging :: StartAccessLoggingResponse -> TestTree
responseStartAccessLogging =
  res
    "StartAccessLoggingResponse"
    "fixture/StartAccessLoggingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartAccessLogging)

responseStopAccessLogging :: StopAccessLoggingResponse -> TestTree
responseStopAccessLogging =
  res
    "StopAccessLoggingResponse"
    "fixture/StopAccessLoggingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopAccessLogging)

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
