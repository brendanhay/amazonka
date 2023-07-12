{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.CloudControl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.CloudControl where

import Amazonka.CloudControl
import qualified Data.Proxy as Proxy
import Test.Amazonka.CloudControl.Internal
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
--         [ requestCancelResourceRequest $
--             newCancelResourceRequest
--
--         , requestCreateResource $
--             newCreateResource
--
--         , requestDeleteResource $
--             newDeleteResource
--
--         , requestGetResource $
--             newGetResource
--
--         , requestGetResourceRequestStatus $
--             newGetResourceRequestStatus
--
--         , requestListResourceRequests $
--             newListResourceRequests
--
--         , requestListResources $
--             newListResources
--
--         , requestUpdateResource $
--             newUpdateResource
--
--           ]

--     , testGroup "response"
--         [ responseCancelResourceRequest $
--             newCancelResourceRequestResponse
--
--         , responseCreateResource $
--             newCreateResourceResponse
--
--         , responseDeleteResource $
--             newDeleteResourceResponse
--
--         , responseGetResource $
--             newGetResourceResponse
--
--         , responseGetResourceRequestStatus $
--             newGetResourceRequestStatusResponse
--
--         , responseListResourceRequests $
--             newListResourceRequestsResponse
--
--         , responseListResources $
--             newListResourcesResponse
--
--         , responseUpdateResource $
--             newUpdateResourceResponse
--
--           ]
--     ]

-- Requests

requestCancelResourceRequest :: CancelResourceRequest -> TestTree
requestCancelResourceRequest =
  req
    "CancelResourceRequest"
    "fixture/CancelResourceRequest.yaml"

requestCreateResource :: CreateResource -> TestTree
requestCreateResource =
  req
    "CreateResource"
    "fixture/CreateResource.yaml"

requestDeleteResource :: DeleteResource -> TestTree
requestDeleteResource =
  req
    "DeleteResource"
    "fixture/DeleteResource.yaml"

requestGetResource :: GetResource -> TestTree
requestGetResource =
  req
    "GetResource"
    "fixture/GetResource.yaml"

requestGetResourceRequestStatus :: GetResourceRequestStatus -> TestTree
requestGetResourceRequestStatus =
  req
    "GetResourceRequestStatus"
    "fixture/GetResourceRequestStatus.yaml"

requestListResourceRequests :: ListResourceRequests -> TestTree
requestListResourceRequests =
  req
    "ListResourceRequests"
    "fixture/ListResourceRequests.yaml"

requestListResources :: ListResources -> TestTree
requestListResources =
  req
    "ListResources"
    "fixture/ListResources.yaml"

requestUpdateResource :: UpdateResource -> TestTree
requestUpdateResource =
  req
    "UpdateResource"
    "fixture/UpdateResource.yaml"

-- Responses

responseCancelResourceRequest :: CancelResourceRequestResponse -> TestTree
responseCancelResourceRequest =
  res
    "CancelResourceRequestResponse"
    "fixture/CancelResourceRequestResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelResourceRequest)

responseCreateResource :: CreateResourceResponse -> TestTree
responseCreateResource =
  res
    "CreateResourceResponse"
    "fixture/CreateResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateResource)

responseDeleteResource :: DeleteResourceResponse -> TestTree
responseDeleteResource =
  res
    "DeleteResourceResponse"
    "fixture/DeleteResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResource)

responseGetResource :: GetResourceResponse -> TestTree
responseGetResource =
  res
    "GetResourceResponse"
    "fixture/GetResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResource)

responseGetResourceRequestStatus :: GetResourceRequestStatusResponse -> TestTree
responseGetResourceRequestStatus =
  res
    "GetResourceRequestStatusResponse"
    "fixture/GetResourceRequestStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResourceRequestStatus)

responseListResourceRequests :: ListResourceRequestsResponse -> TestTree
responseListResourceRequests =
  res
    "ListResourceRequestsResponse"
    "fixture/ListResourceRequestsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResourceRequests)

responseListResources :: ListResourcesResponse -> TestTree
responseListResources =
  res
    "ListResourcesResponse"
    "fixture/ListResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResources)

responseUpdateResource :: UpdateResourceResponse -> TestTree
responseUpdateResource =
  res
    "UpdateResourceResponse"
    "fixture/UpdateResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateResource)
