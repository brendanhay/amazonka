{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.AppFlow
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.AppFlow where

import Amazonka.AppFlow
import qualified Data.Proxy as Proxy
import Test.Amazonka.AppFlow.Internal
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
--         [ requestCreateConnectorProfile $
--             newCreateConnectorProfile
--
--         , requestCreateFlow $
--             newCreateFlow
--
--         , requestDeleteConnectorProfile $
--             newDeleteConnectorProfile
--
--         , requestDeleteFlow $
--             newDeleteFlow
--
--         , requestDescribeConnector $
--             newDescribeConnector
--
--         , requestDescribeConnectorEntity $
--             newDescribeConnectorEntity
--
--         , requestDescribeConnectorProfiles $
--             newDescribeConnectorProfiles
--
--         , requestDescribeConnectors $
--             newDescribeConnectors
--
--         , requestDescribeFlow $
--             newDescribeFlow
--
--         , requestDescribeFlowExecutionRecords $
--             newDescribeFlowExecutionRecords
--
--         , requestListConnectorEntities $
--             newListConnectorEntities
--
--         , requestListConnectors $
--             newListConnectors
--
--         , requestListFlows $
--             newListFlows
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestRegisterConnector $
--             newRegisterConnector
--
--         , requestStartFlow $
--             newStartFlow
--
--         , requestStopFlow $
--             newStopFlow
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUnregisterConnector $
--             newUnregisterConnector
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateConnectorProfile $
--             newUpdateConnectorProfile
--
--         , requestUpdateConnectorRegistration $
--             newUpdateConnectorRegistration
--
--         , requestUpdateFlow $
--             newUpdateFlow
--
--           ]

--     , testGroup "response"
--         [ responseCreateConnectorProfile $
--             newCreateConnectorProfileResponse
--
--         , responseCreateFlow $
--             newCreateFlowResponse
--
--         , responseDeleteConnectorProfile $
--             newDeleteConnectorProfileResponse
--
--         , responseDeleteFlow $
--             newDeleteFlowResponse
--
--         , responseDescribeConnector $
--             newDescribeConnectorResponse
--
--         , responseDescribeConnectorEntity $
--             newDescribeConnectorEntityResponse
--
--         , responseDescribeConnectorProfiles $
--             newDescribeConnectorProfilesResponse
--
--         , responseDescribeConnectors $
--             newDescribeConnectorsResponse
--
--         , responseDescribeFlow $
--             newDescribeFlowResponse
--
--         , responseDescribeFlowExecutionRecords $
--             newDescribeFlowExecutionRecordsResponse
--
--         , responseListConnectorEntities $
--             newListConnectorEntitiesResponse
--
--         , responseListConnectors $
--             newListConnectorsResponse
--
--         , responseListFlows $
--             newListFlowsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseRegisterConnector $
--             newRegisterConnectorResponse
--
--         , responseStartFlow $
--             newStartFlowResponse
--
--         , responseStopFlow $
--             newStopFlowResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUnregisterConnector $
--             newUnregisterConnectorResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateConnectorProfile $
--             newUpdateConnectorProfileResponse
--
--         , responseUpdateConnectorRegistration $
--             newUpdateConnectorRegistrationResponse
--
--         , responseUpdateFlow $
--             newUpdateFlowResponse
--
--           ]
--     ]

-- Requests

requestCreateConnectorProfile :: CreateConnectorProfile -> TestTree
requestCreateConnectorProfile =
  req
    "CreateConnectorProfile"
    "fixture/CreateConnectorProfile.yaml"

requestCreateFlow :: CreateFlow -> TestTree
requestCreateFlow =
  req
    "CreateFlow"
    "fixture/CreateFlow.yaml"

requestDeleteConnectorProfile :: DeleteConnectorProfile -> TestTree
requestDeleteConnectorProfile =
  req
    "DeleteConnectorProfile"
    "fixture/DeleteConnectorProfile.yaml"

requestDeleteFlow :: DeleteFlow -> TestTree
requestDeleteFlow =
  req
    "DeleteFlow"
    "fixture/DeleteFlow.yaml"

requestDescribeConnector :: DescribeConnector -> TestTree
requestDescribeConnector =
  req
    "DescribeConnector"
    "fixture/DescribeConnector.yaml"

requestDescribeConnectorEntity :: DescribeConnectorEntity -> TestTree
requestDescribeConnectorEntity =
  req
    "DescribeConnectorEntity"
    "fixture/DescribeConnectorEntity.yaml"

requestDescribeConnectorProfiles :: DescribeConnectorProfiles -> TestTree
requestDescribeConnectorProfiles =
  req
    "DescribeConnectorProfiles"
    "fixture/DescribeConnectorProfiles.yaml"

requestDescribeConnectors :: DescribeConnectors -> TestTree
requestDescribeConnectors =
  req
    "DescribeConnectors"
    "fixture/DescribeConnectors.yaml"

requestDescribeFlow :: DescribeFlow -> TestTree
requestDescribeFlow =
  req
    "DescribeFlow"
    "fixture/DescribeFlow.yaml"

requestDescribeFlowExecutionRecords :: DescribeFlowExecutionRecords -> TestTree
requestDescribeFlowExecutionRecords =
  req
    "DescribeFlowExecutionRecords"
    "fixture/DescribeFlowExecutionRecords.yaml"

requestListConnectorEntities :: ListConnectorEntities -> TestTree
requestListConnectorEntities =
  req
    "ListConnectorEntities"
    "fixture/ListConnectorEntities.yaml"

requestListConnectors :: ListConnectors -> TestTree
requestListConnectors =
  req
    "ListConnectors"
    "fixture/ListConnectors.yaml"

requestListFlows :: ListFlows -> TestTree
requestListFlows =
  req
    "ListFlows"
    "fixture/ListFlows.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestRegisterConnector :: RegisterConnector -> TestTree
requestRegisterConnector =
  req
    "RegisterConnector"
    "fixture/RegisterConnector.yaml"

requestStartFlow :: StartFlow -> TestTree
requestStartFlow =
  req
    "StartFlow"
    "fixture/StartFlow.yaml"

requestStopFlow :: StopFlow -> TestTree
requestStopFlow =
  req
    "StopFlow"
    "fixture/StopFlow.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUnregisterConnector :: UnregisterConnector -> TestTree
requestUnregisterConnector =
  req
    "UnregisterConnector"
    "fixture/UnregisterConnector.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateConnectorProfile :: UpdateConnectorProfile -> TestTree
requestUpdateConnectorProfile =
  req
    "UpdateConnectorProfile"
    "fixture/UpdateConnectorProfile.yaml"

requestUpdateConnectorRegistration :: UpdateConnectorRegistration -> TestTree
requestUpdateConnectorRegistration =
  req
    "UpdateConnectorRegistration"
    "fixture/UpdateConnectorRegistration.yaml"

requestUpdateFlow :: UpdateFlow -> TestTree
requestUpdateFlow =
  req
    "UpdateFlow"
    "fixture/UpdateFlow.yaml"

-- Responses

responseCreateConnectorProfile :: CreateConnectorProfileResponse -> TestTree
responseCreateConnectorProfile =
  res
    "CreateConnectorProfileResponse"
    "fixture/CreateConnectorProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateConnectorProfile)

responseCreateFlow :: CreateFlowResponse -> TestTree
responseCreateFlow =
  res
    "CreateFlowResponse"
    "fixture/CreateFlowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFlow)

responseDeleteConnectorProfile :: DeleteConnectorProfileResponse -> TestTree
responseDeleteConnectorProfile =
  res
    "DeleteConnectorProfileResponse"
    "fixture/DeleteConnectorProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConnectorProfile)

responseDeleteFlow :: DeleteFlowResponse -> TestTree
responseDeleteFlow =
  res
    "DeleteFlowResponse"
    "fixture/DeleteFlowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFlow)

responseDescribeConnector :: DescribeConnectorResponse -> TestTree
responseDescribeConnector =
  res
    "DescribeConnectorResponse"
    "fixture/DescribeConnectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConnector)

responseDescribeConnectorEntity :: DescribeConnectorEntityResponse -> TestTree
responseDescribeConnectorEntity =
  res
    "DescribeConnectorEntityResponse"
    "fixture/DescribeConnectorEntityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConnectorEntity)

responseDescribeConnectorProfiles :: DescribeConnectorProfilesResponse -> TestTree
responseDescribeConnectorProfiles =
  res
    "DescribeConnectorProfilesResponse"
    "fixture/DescribeConnectorProfilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConnectorProfiles)

responseDescribeConnectors :: DescribeConnectorsResponse -> TestTree
responseDescribeConnectors =
  res
    "DescribeConnectorsResponse"
    "fixture/DescribeConnectorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConnectors)

responseDescribeFlow :: DescribeFlowResponse -> TestTree
responseDescribeFlow =
  res
    "DescribeFlowResponse"
    "fixture/DescribeFlowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFlow)

responseDescribeFlowExecutionRecords :: DescribeFlowExecutionRecordsResponse -> TestTree
responseDescribeFlowExecutionRecords =
  res
    "DescribeFlowExecutionRecordsResponse"
    "fixture/DescribeFlowExecutionRecordsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFlowExecutionRecords)

responseListConnectorEntities :: ListConnectorEntitiesResponse -> TestTree
responseListConnectorEntities =
  res
    "ListConnectorEntitiesResponse"
    "fixture/ListConnectorEntitiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListConnectorEntities)

responseListConnectors :: ListConnectorsResponse -> TestTree
responseListConnectors =
  res
    "ListConnectorsResponse"
    "fixture/ListConnectorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListConnectors)

responseListFlows :: ListFlowsResponse -> TestTree
responseListFlows =
  res
    "ListFlowsResponse"
    "fixture/ListFlowsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFlows)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseRegisterConnector :: RegisterConnectorResponse -> TestTree
responseRegisterConnector =
  res
    "RegisterConnectorResponse"
    "fixture/RegisterConnectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterConnector)

responseStartFlow :: StartFlowResponse -> TestTree
responseStartFlow =
  res
    "StartFlowResponse"
    "fixture/StartFlowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartFlow)

responseStopFlow :: StopFlowResponse -> TestTree
responseStopFlow =
  res
    "StopFlowResponse"
    "fixture/StopFlowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopFlow)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUnregisterConnector :: UnregisterConnectorResponse -> TestTree
responseUnregisterConnector =
  res
    "UnregisterConnectorResponse"
    "fixture/UnregisterConnectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UnregisterConnector)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateConnectorProfile :: UpdateConnectorProfileResponse -> TestTree
responseUpdateConnectorProfile =
  res
    "UpdateConnectorProfileResponse"
    "fixture/UpdateConnectorProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateConnectorProfile)

responseUpdateConnectorRegistration :: UpdateConnectorRegistrationResponse -> TestTree
responseUpdateConnectorRegistration =
  res
    "UpdateConnectorRegistrationResponse"
    "fixture/UpdateConnectorRegistrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateConnectorRegistration)

responseUpdateFlow :: UpdateFlowResponse -> TestTree
responseUpdateFlow =
  res
    "UpdateFlowResponse"
    "fixture/UpdateFlowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFlow)
