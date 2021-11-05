{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.AppFlow
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.AppFlow where

import qualified Data.Proxy as Proxy
import Network.AWS.AppFlow
import Test.AWS.AppFlow.Internal
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
--         [ requestListConnectorEntities $
--             newListConnectorEntities
--
--         , requestCreateConnectorProfile $
--             newCreateConnectorProfile
--
--         , requestStartFlow $
--             newStartFlow
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestCreateFlow $
--             newCreateFlow
--
--         , requestDeleteConnectorProfile $
--             newDeleteConnectorProfile
--
--         , requestUpdateConnectorProfile $
--             newUpdateConnectorProfile
--
--         , requestDescribeFlow $
--             newDescribeFlow
--
--         , requestStopFlow $
--             newStopFlow
--
--         , requestDescribeConnectors $
--             newDescribeConnectors
--
--         , requestDescribeConnectorEntity $
--             newDescribeConnectorEntity
--
--         , requestTagResource $
--             newTagResource
--
--         , requestListFlows $
--             newListFlows
--
--         , requestDescribeFlowExecutionRecords $
--             newDescribeFlowExecutionRecords
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateFlow $
--             newUpdateFlow
--
--         , requestDeleteFlow $
--             newDeleteFlow
--
--         , requestDescribeConnectorProfiles $
--             newDescribeConnectorProfiles
--
--           ]

--     , testGroup "response"
--         [ responseListConnectorEntities $
--             newListConnectorEntitiesResponse
--
--         , responseCreateConnectorProfile $
--             newCreateConnectorProfileResponse
--
--         , responseStartFlow $
--             newStartFlowResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseCreateFlow $
--             newCreateFlowResponse
--
--         , responseDeleteConnectorProfile $
--             newDeleteConnectorProfileResponse
--
--         , responseUpdateConnectorProfile $
--             newUpdateConnectorProfileResponse
--
--         , responseDescribeFlow $
--             newDescribeFlowResponse
--
--         , responseStopFlow $
--             newStopFlowResponse
--
--         , responseDescribeConnectors $
--             newDescribeConnectorsResponse
--
--         , responseDescribeConnectorEntity $
--             newDescribeConnectorEntityResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseListFlows $
--             newListFlowsResponse
--
--         , responseDescribeFlowExecutionRecords $
--             newDescribeFlowExecutionRecordsResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateFlow $
--             newUpdateFlowResponse
--
--         , responseDeleteFlow $
--             newDeleteFlowResponse
--
--         , responseDescribeConnectorProfiles $
--             newDescribeConnectorProfilesResponse
--
--           ]
--     ]

-- Requests

requestListConnectorEntities :: ListConnectorEntities -> TestTree
requestListConnectorEntities =
  req
    "ListConnectorEntities"
    "fixture/ListConnectorEntities.yaml"

requestCreateConnectorProfile :: CreateConnectorProfile -> TestTree
requestCreateConnectorProfile =
  req
    "CreateConnectorProfile"
    "fixture/CreateConnectorProfile.yaml"

requestStartFlow :: StartFlow -> TestTree
requestStartFlow =
  req
    "StartFlow"
    "fixture/StartFlow.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

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

requestUpdateConnectorProfile :: UpdateConnectorProfile -> TestTree
requestUpdateConnectorProfile =
  req
    "UpdateConnectorProfile"
    "fixture/UpdateConnectorProfile.yaml"

requestDescribeFlow :: DescribeFlow -> TestTree
requestDescribeFlow =
  req
    "DescribeFlow"
    "fixture/DescribeFlow.yaml"

requestStopFlow :: StopFlow -> TestTree
requestStopFlow =
  req
    "StopFlow"
    "fixture/StopFlow.yaml"

requestDescribeConnectors :: DescribeConnectors -> TestTree
requestDescribeConnectors =
  req
    "DescribeConnectors"
    "fixture/DescribeConnectors.yaml"

requestDescribeConnectorEntity :: DescribeConnectorEntity -> TestTree
requestDescribeConnectorEntity =
  req
    "DescribeConnectorEntity"
    "fixture/DescribeConnectorEntity.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestListFlows :: ListFlows -> TestTree
requestListFlows =
  req
    "ListFlows"
    "fixture/ListFlows.yaml"

requestDescribeFlowExecutionRecords :: DescribeFlowExecutionRecords -> TestTree
requestDescribeFlowExecutionRecords =
  req
    "DescribeFlowExecutionRecords"
    "fixture/DescribeFlowExecutionRecords.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateFlow :: UpdateFlow -> TestTree
requestUpdateFlow =
  req
    "UpdateFlow"
    "fixture/UpdateFlow.yaml"

requestDeleteFlow :: DeleteFlow -> TestTree
requestDeleteFlow =
  req
    "DeleteFlow"
    "fixture/DeleteFlow.yaml"

requestDescribeConnectorProfiles :: DescribeConnectorProfiles -> TestTree
requestDescribeConnectorProfiles =
  req
    "DescribeConnectorProfiles"
    "fixture/DescribeConnectorProfiles.yaml"

-- Responses

responseListConnectorEntities :: ListConnectorEntitiesResponse -> TestTree
responseListConnectorEntities =
  res
    "ListConnectorEntitiesResponse"
    "fixture/ListConnectorEntitiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListConnectorEntities)

responseCreateConnectorProfile :: CreateConnectorProfileResponse -> TestTree
responseCreateConnectorProfile =
  res
    "CreateConnectorProfileResponse"
    "fixture/CreateConnectorProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateConnectorProfile)

responseStartFlow :: StartFlowResponse -> TestTree
responseStartFlow =
  res
    "StartFlowResponse"
    "fixture/StartFlowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartFlow)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

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

responseUpdateConnectorProfile :: UpdateConnectorProfileResponse -> TestTree
responseUpdateConnectorProfile =
  res
    "UpdateConnectorProfileResponse"
    "fixture/UpdateConnectorProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateConnectorProfile)

responseDescribeFlow :: DescribeFlowResponse -> TestTree
responseDescribeFlow =
  res
    "DescribeFlowResponse"
    "fixture/DescribeFlowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFlow)

responseStopFlow :: StopFlowResponse -> TestTree
responseStopFlow =
  res
    "StopFlowResponse"
    "fixture/StopFlowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopFlow)

responseDescribeConnectors :: DescribeConnectorsResponse -> TestTree
responseDescribeConnectors =
  res
    "DescribeConnectorsResponse"
    "fixture/DescribeConnectorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConnectors)

responseDescribeConnectorEntity :: DescribeConnectorEntityResponse -> TestTree
responseDescribeConnectorEntity =
  res
    "DescribeConnectorEntityResponse"
    "fixture/DescribeConnectorEntityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConnectorEntity)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseListFlows :: ListFlowsResponse -> TestTree
responseListFlows =
  res
    "ListFlowsResponse"
    "fixture/ListFlowsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFlows)

responseDescribeFlowExecutionRecords :: DescribeFlowExecutionRecordsResponse -> TestTree
responseDescribeFlowExecutionRecords =
  res
    "DescribeFlowExecutionRecordsResponse"
    "fixture/DescribeFlowExecutionRecordsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFlowExecutionRecords)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateFlow :: UpdateFlowResponse -> TestTree
responseUpdateFlow =
  res
    "UpdateFlowResponse"
    "fixture/UpdateFlowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFlow)

responseDeleteFlow :: DeleteFlowResponse -> TestTree
responseDeleteFlow =
  res
    "DeleteFlowResponse"
    "fixture/DeleteFlowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFlow)

responseDescribeConnectorProfiles :: DescribeConnectorProfilesResponse -> TestTree
responseDescribeConnectorProfiles =
  res
    "DescribeConnectorProfilesResponse"
    "fixture/DescribeConnectorProfilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConnectorProfiles)
