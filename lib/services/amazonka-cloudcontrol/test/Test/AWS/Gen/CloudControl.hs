{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudControl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.CloudControl where

import Data.Proxy
import Network.AWS.CloudControl
import Test.AWS.CloudControl.Internal
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
--         [ requestGetResource $
--             newGetResource
--
--         , requestListResourceRequests $
--             newListResourceRequests
--
--         , requestCancelResourceRequest $
--             newCancelResourceRequest
--
--         , requestDeleteResource $
--             newDeleteResource
--
--         , requestUpdateResource $
--             newUpdateResource
--
--         , requestListResources $
--             newListResources
--
--         , requestCreateResource $
--             newCreateResource
--
--         , requestGetResourceRequestStatus $
--             newGetResourceRequestStatus
--
--           ]

--     , testGroup "response"
--         [ responseGetResource $
--             newGetResourceResponse
--
--         , responseListResourceRequests $
--             newListResourceRequestsResponse
--
--         , responseCancelResourceRequest $
--             newCancelResourceRequestResponse
--
--         , responseDeleteResource $
--             newDeleteResourceResponse
--
--         , responseUpdateResource $
--             newUpdateResourceResponse
--
--         , responseListResources $
--             newListResourcesResponse
--
--         , responseCreateResource $
--             newCreateResourceResponse
--
--         , responseGetResourceRequestStatus $
--             newGetResourceRequestStatusResponse
--
--           ]
--     ]

-- Requests

requestGetResource :: GetResource -> TestTree
requestGetResource =
  req
    "GetResource"
    "fixture/GetResource.yaml"

requestListResourceRequests :: ListResourceRequests -> TestTree
requestListResourceRequests =
  req
    "ListResourceRequests"
    "fixture/ListResourceRequests.yaml"

requestCancelResourceRequest :: CancelResourceRequest -> TestTree
requestCancelResourceRequest =
  req
    "CancelResourceRequest"
    "fixture/CancelResourceRequest.yaml"

requestDeleteResource :: DeleteResource -> TestTree
requestDeleteResource =
  req
    "DeleteResource"
    "fixture/DeleteResource.yaml"

requestUpdateResource :: UpdateResource -> TestTree
requestUpdateResource =
  req
    "UpdateResource"
    "fixture/UpdateResource.yaml"

requestListResources :: ListResources -> TestTree
requestListResources =
  req
    "ListResources"
    "fixture/ListResources.yaml"

requestCreateResource :: CreateResource -> TestTree
requestCreateResource =
  req
    "CreateResource"
    "fixture/CreateResource.yaml"

requestGetResourceRequestStatus :: GetResourceRequestStatus -> TestTree
requestGetResourceRequestStatus =
  req
    "GetResourceRequestStatus"
    "fixture/GetResourceRequestStatus.yaml"

-- Responses

responseGetResource :: GetResourceResponse -> TestTree
responseGetResource =
  res
    "GetResourceResponse"
    "fixture/GetResourceResponse.proto"
    defaultService
    (Proxy :: Proxy GetResource)

responseListResourceRequests :: ListResourceRequestsResponse -> TestTree
responseListResourceRequests =
  res
    "ListResourceRequestsResponse"
    "fixture/ListResourceRequestsResponse.proto"
    defaultService
    (Proxy :: Proxy ListResourceRequests)

responseCancelResourceRequest :: CancelResourceRequestResponse -> TestTree
responseCancelResourceRequest =
  res
    "CancelResourceRequestResponse"
    "fixture/CancelResourceRequestResponse.proto"
    defaultService
    (Proxy :: Proxy CancelResourceRequest)

responseDeleteResource :: DeleteResourceResponse -> TestTree
responseDeleteResource =
  res
    "DeleteResourceResponse"
    "fixture/DeleteResourceResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteResource)

responseUpdateResource :: UpdateResourceResponse -> TestTree
responseUpdateResource =
  res
    "UpdateResourceResponse"
    "fixture/UpdateResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateResource)

responseListResources :: ListResourcesResponse -> TestTree
responseListResources =
  res
    "ListResourcesResponse"
    "fixture/ListResourcesResponse.proto"
    defaultService
    (Proxy :: Proxy ListResources)

responseCreateResource :: CreateResourceResponse -> TestTree
responseCreateResource =
  res
    "CreateResourceResponse"
    "fixture/CreateResourceResponse.proto"
    defaultService
    (Proxy :: Proxy CreateResource)

responseGetResourceRequestStatus :: GetResourceRequestStatusResponse -> TestTree
responseGetResourceRequestStatus =
  res
    "GetResourceRequestStatusResponse"
    "fixture/GetResourceRequestStatusResponse.proto"
    defaultService
    (Proxy :: Proxy GetResourceRequestStatus)
