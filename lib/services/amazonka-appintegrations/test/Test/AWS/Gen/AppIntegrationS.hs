{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.AppIntegrationS
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.AppIntegrationS where

import Amazonka.AppIntegrationS
import qualified Data.Proxy as Proxy
import Test.AWS.AppIntegrationS.Internal
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
--         [ requestGetEventIntegration $
--             newGetEventIntegration
--
--         , requestListDataIntegrations $
--             newListDataIntegrations
--
--         , requestCreateDataIntegration $
--             newCreateDataIntegration
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListEventIntegrationAssociations $
--             newListEventIntegrationAssociations
--
--         , requestGetDataIntegration $
--             newGetDataIntegration
--
--         , requestListEventIntegrations $
--             newListEventIntegrations
--
--         , requestDeleteEventIntegration $
--             newDeleteEventIntegration
--
--         , requestUpdateEventIntegration $
--             newUpdateEventIntegration
--
--         , requestDeleteDataIntegration $
--             newDeleteDataIntegration
--
--         , requestUpdateDataIntegration $
--             newUpdateDataIntegration
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestListDataIntegrationAssociations $
--             newListDataIntegrationAssociations
--
--         , requestCreateEventIntegration $
--             newCreateEventIntegration
--
--           ]

--     , testGroup "response"
--         [ responseGetEventIntegration $
--             newGetEventIntegrationResponse
--
--         , responseListDataIntegrations $
--             newListDataIntegrationsResponse
--
--         , responseCreateDataIntegration $
--             newCreateDataIntegrationResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListEventIntegrationAssociations $
--             newListEventIntegrationAssociationsResponse
--
--         , responseGetDataIntegration $
--             newGetDataIntegrationResponse
--
--         , responseListEventIntegrations $
--             newListEventIntegrationsResponse
--
--         , responseDeleteEventIntegration $
--             newDeleteEventIntegrationResponse
--
--         , responseUpdateEventIntegration $
--             newUpdateEventIntegrationResponse
--
--         , responseDeleteDataIntegration $
--             newDeleteDataIntegrationResponse
--
--         , responseUpdateDataIntegration $
--             newUpdateDataIntegrationResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseListDataIntegrationAssociations $
--             newListDataIntegrationAssociationsResponse
--
--         , responseCreateEventIntegration $
--             newCreateEventIntegrationResponse
--
--           ]
--     ]

-- Requests

requestGetEventIntegration :: GetEventIntegration -> TestTree
requestGetEventIntegration =
  req
    "GetEventIntegration"
    "fixture/GetEventIntegration.yaml"

requestListDataIntegrations :: ListDataIntegrations -> TestTree
requestListDataIntegrations =
  req
    "ListDataIntegrations"
    "fixture/ListDataIntegrations.yaml"

requestCreateDataIntegration :: CreateDataIntegration -> TestTree
requestCreateDataIntegration =
  req
    "CreateDataIntegration"
    "fixture/CreateDataIntegration.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListEventIntegrationAssociations :: ListEventIntegrationAssociations -> TestTree
requestListEventIntegrationAssociations =
  req
    "ListEventIntegrationAssociations"
    "fixture/ListEventIntegrationAssociations.yaml"

requestGetDataIntegration :: GetDataIntegration -> TestTree
requestGetDataIntegration =
  req
    "GetDataIntegration"
    "fixture/GetDataIntegration.yaml"

requestListEventIntegrations :: ListEventIntegrations -> TestTree
requestListEventIntegrations =
  req
    "ListEventIntegrations"
    "fixture/ListEventIntegrations.yaml"

requestDeleteEventIntegration :: DeleteEventIntegration -> TestTree
requestDeleteEventIntegration =
  req
    "DeleteEventIntegration"
    "fixture/DeleteEventIntegration.yaml"

requestUpdateEventIntegration :: UpdateEventIntegration -> TestTree
requestUpdateEventIntegration =
  req
    "UpdateEventIntegration"
    "fixture/UpdateEventIntegration.yaml"

requestDeleteDataIntegration :: DeleteDataIntegration -> TestTree
requestDeleteDataIntegration =
  req
    "DeleteDataIntegration"
    "fixture/DeleteDataIntegration.yaml"

requestUpdateDataIntegration :: UpdateDataIntegration -> TestTree
requestUpdateDataIntegration =
  req
    "UpdateDataIntegration"
    "fixture/UpdateDataIntegration.yaml"

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

requestListDataIntegrationAssociations :: ListDataIntegrationAssociations -> TestTree
requestListDataIntegrationAssociations =
  req
    "ListDataIntegrationAssociations"
    "fixture/ListDataIntegrationAssociations.yaml"

requestCreateEventIntegration :: CreateEventIntegration -> TestTree
requestCreateEventIntegration =
  req
    "CreateEventIntegration"
    "fixture/CreateEventIntegration.yaml"

-- Responses

responseGetEventIntegration :: GetEventIntegrationResponse -> TestTree
responseGetEventIntegration =
  res
    "GetEventIntegrationResponse"
    "fixture/GetEventIntegrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEventIntegration)

responseListDataIntegrations :: ListDataIntegrationsResponse -> TestTree
responseListDataIntegrations =
  res
    "ListDataIntegrationsResponse"
    "fixture/ListDataIntegrationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDataIntegrations)

responseCreateDataIntegration :: CreateDataIntegrationResponse -> TestTree
responseCreateDataIntegration =
  res
    "CreateDataIntegrationResponse"
    "fixture/CreateDataIntegrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDataIntegration)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListEventIntegrationAssociations :: ListEventIntegrationAssociationsResponse -> TestTree
responseListEventIntegrationAssociations =
  res
    "ListEventIntegrationAssociationsResponse"
    "fixture/ListEventIntegrationAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEventIntegrationAssociations)

responseGetDataIntegration :: GetDataIntegrationResponse -> TestTree
responseGetDataIntegration =
  res
    "GetDataIntegrationResponse"
    "fixture/GetDataIntegrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDataIntegration)

responseListEventIntegrations :: ListEventIntegrationsResponse -> TestTree
responseListEventIntegrations =
  res
    "ListEventIntegrationsResponse"
    "fixture/ListEventIntegrationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEventIntegrations)

responseDeleteEventIntegration :: DeleteEventIntegrationResponse -> TestTree
responseDeleteEventIntegration =
  res
    "DeleteEventIntegrationResponse"
    "fixture/DeleteEventIntegrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEventIntegration)

responseUpdateEventIntegration :: UpdateEventIntegrationResponse -> TestTree
responseUpdateEventIntegration =
  res
    "UpdateEventIntegrationResponse"
    "fixture/UpdateEventIntegrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEventIntegration)

responseDeleteDataIntegration :: DeleteDataIntegrationResponse -> TestTree
responseDeleteDataIntegration =
  res
    "DeleteDataIntegrationResponse"
    "fixture/DeleteDataIntegrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDataIntegration)

responseUpdateDataIntegration :: UpdateDataIntegrationResponse -> TestTree
responseUpdateDataIntegration =
  res
    "UpdateDataIntegrationResponse"
    "fixture/UpdateDataIntegrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDataIntegration)

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

responseListDataIntegrationAssociations :: ListDataIntegrationAssociationsResponse -> TestTree
responseListDataIntegrationAssociations =
  res
    "ListDataIntegrationAssociationsResponse"
    "fixture/ListDataIntegrationAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDataIntegrationAssociations)

responseCreateEventIntegration :: CreateEventIntegrationResponse -> TestTree
responseCreateEventIntegration =
  res
    "CreateEventIntegrationResponse"
    "fixture/CreateEventIntegrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEventIntegration)
