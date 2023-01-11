{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.AppIntegrationS
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.AppIntegrationS where

import Amazonka.AppIntegrationS
import qualified Data.Proxy as Proxy
import Test.Amazonka.AppIntegrationS.Internal
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
--         [ requestCreateDataIntegration $
--             newCreateDataIntegration
--
--         , requestCreateEventIntegration $
--             newCreateEventIntegration
--
--         , requestDeleteDataIntegration $
--             newDeleteDataIntegration
--
--         , requestDeleteEventIntegration $
--             newDeleteEventIntegration
--
--         , requestGetDataIntegration $
--             newGetDataIntegration
--
--         , requestGetEventIntegration $
--             newGetEventIntegration
--
--         , requestListDataIntegrationAssociations $
--             newListDataIntegrationAssociations
--
--         , requestListDataIntegrations $
--             newListDataIntegrations
--
--         , requestListEventIntegrationAssociations $
--             newListEventIntegrationAssociations
--
--         , requestListEventIntegrations $
--             newListEventIntegrations
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateDataIntegration $
--             newUpdateDataIntegration
--
--         , requestUpdateEventIntegration $
--             newUpdateEventIntegration
--
--           ]

--     , testGroup "response"
--         [ responseCreateDataIntegration $
--             newCreateDataIntegrationResponse
--
--         , responseCreateEventIntegration $
--             newCreateEventIntegrationResponse
--
--         , responseDeleteDataIntegration $
--             newDeleteDataIntegrationResponse
--
--         , responseDeleteEventIntegration $
--             newDeleteEventIntegrationResponse
--
--         , responseGetDataIntegration $
--             newGetDataIntegrationResponse
--
--         , responseGetEventIntegration $
--             newGetEventIntegrationResponse
--
--         , responseListDataIntegrationAssociations $
--             newListDataIntegrationAssociationsResponse
--
--         , responseListDataIntegrations $
--             newListDataIntegrationsResponse
--
--         , responseListEventIntegrationAssociations $
--             newListEventIntegrationAssociationsResponse
--
--         , responseListEventIntegrations $
--             newListEventIntegrationsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateDataIntegration $
--             newUpdateDataIntegrationResponse
--
--         , responseUpdateEventIntegration $
--             newUpdateEventIntegrationResponse
--
--           ]
--     ]

-- Requests

requestCreateDataIntegration :: CreateDataIntegration -> TestTree
requestCreateDataIntegration =
  req
    "CreateDataIntegration"
    "fixture/CreateDataIntegration.yaml"

requestCreateEventIntegration :: CreateEventIntegration -> TestTree
requestCreateEventIntegration =
  req
    "CreateEventIntegration"
    "fixture/CreateEventIntegration.yaml"

requestDeleteDataIntegration :: DeleteDataIntegration -> TestTree
requestDeleteDataIntegration =
  req
    "DeleteDataIntegration"
    "fixture/DeleteDataIntegration.yaml"

requestDeleteEventIntegration :: DeleteEventIntegration -> TestTree
requestDeleteEventIntegration =
  req
    "DeleteEventIntegration"
    "fixture/DeleteEventIntegration.yaml"

requestGetDataIntegration :: GetDataIntegration -> TestTree
requestGetDataIntegration =
  req
    "GetDataIntegration"
    "fixture/GetDataIntegration.yaml"

requestGetEventIntegration :: GetEventIntegration -> TestTree
requestGetEventIntegration =
  req
    "GetEventIntegration"
    "fixture/GetEventIntegration.yaml"

requestListDataIntegrationAssociations :: ListDataIntegrationAssociations -> TestTree
requestListDataIntegrationAssociations =
  req
    "ListDataIntegrationAssociations"
    "fixture/ListDataIntegrationAssociations.yaml"

requestListDataIntegrations :: ListDataIntegrations -> TestTree
requestListDataIntegrations =
  req
    "ListDataIntegrations"
    "fixture/ListDataIntegrations.yaml"

requestListEventIntegrationAssociations :: ListEventIntegrationAssociations -> TestTree
requestListEventIntegrationAssociations =
  req
    "ListEventIntegrationAssociations"
    "fixture/ListEventIntegrationAssociations.yaml"

requestListEventIntegrations :: ListEventIntegrations -> TestTree
requestListEventIntegrations =
  req
    "ListEventIntegrations"
    "fixture/ListEventIntegrations.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

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

requestUpdateDataIntegration :: UpdateDataIntegration -> TestTree
requestUpdateDataIntegration =
  req
    "UpdateDataIntegration"
    "fixture/UpdateDataIntegration.yaml"

requestUpdateEventIntegration :: UpdateEventIntegration -> TestTree
requestUpdateEventIntegration =
  req
    "UpdateEventIntegration"
    "fixture/UpdateEventIntegration.yaml"

-- Responses

responseCreateDataIntegration :: CreateDataIntegrationResponse -> TestTree
responseCreateDataIntegration =
  res
    "CreateDataIntegrationResponse"
    "fixture/CreateDataIntegrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDataIntegration)

responseCreateEventIntegration :: CreateEventIntegrationResponse -> TestTree
responseCreateEventIntegration =
  res
    "CreateEventIntegrationResponse"
    "fixture/CreateEventIntegrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEventIntegration)

responseDeleteDataIntegration :: DeleteDataIntegrationResponse -> TestTree
responseDeleteDataIntegration =
  res
    "DeleteDataIntegrationResponse"
    "fixture/DeleteDataIntegrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDataIntegration)

responseDeleteEventIntegration :: DeleteEventIntegrationResponse -> TestTree
responseDeleteEventIntegration =
  res
    "DeleteEventIntegrationResponse"
    "fixture/DeleteEventIntegrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEventIntegration)

responseGetDataIntegration :: GetDataIntegrationResponse -> TestTree
responseGetDataIntegration =
  res
    "GetDataIntegrationResponse"
    "fixture/GetDataIntegrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDataIntegration)

responseGetEventIntegration :: GetEventIntegrationResponse -> TestTree
responseGetEventIntegration =
  res
    "GetEventIntegrationResponse"
    "fixture/GetEventIntegrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEventIntegration)

responseListDataIntegrationAssociations :: ListDataIntegrationAssociationsResponse -> TestTree
responseListDataIntegrationAssociations =
  res
    "ListDataIntegrationAssociationsResponse"
    "fixture/ListDataIntegrationAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDataIntegrationAssociations)

responseListDataIntegrations :: ListDataIntegrationsResponse -> TestTree
responseListDataIntegrations =
  res
    "ListDataIntegrationsResponse"
    "fixture/ListDataIntegrationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDataIntegrations)

responseListEventIntegrationAssociations :: ListEventIntegrationAssociationsResponse -> TestTree
responseListEventIntegrationAssociations =
  res
    "ListEventIntegrationAssociationsResponse"
    "fixture/ListEventIntegrationAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEventIntegrationAssociations)

responseListEventIntegrations :: ListEventIntegrationsResponse -> TestTree
responseListEventIntegrations =
  res
    "ListEventIntegrationsResponse"
    "fixture/ListEventIntegrationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEventIntegrations)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

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

responseUpdateDataIntegration :: UpdateDataIntegrationResponse -> TestTree
responseUpdateDataIntegration =
  res
    "UpdateDataIntegrationResponse"
    "fixture/UpdateDataIntegrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDataIntegration)

responseUpdateEventIntegration :: UpdateEventIntegrationResponse -> TestTree
responseUpdateEventIntegration =
  res
    "UpdateEventIntegrationResponse"
    "fixture/UpdateEventIntegrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEventIntegration)
