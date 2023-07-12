{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Rum
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Rum where

import Amazonka.Rum
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.Rum.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestBatchCreateRumMetricDefinitions $
--             newBatchCreateRumMetricDefinitions
--
--         , requestBatchDeleteRumMetricDefinitions $
--             newBatchDeleteRumMetricDefinitions
--
--         , requestBatchGetRumMetricDefinitions $
--             newBatchGetRumMetricDefinitions
--
--         , requestCreateAppMonitor $
--             newCreateAppMonitor
--
--         , requestDeleteAppMonitor $
--             newDeleteAppMonitor
--
--         , requestDeleteRumMetricsDestination $
--             newDeleteRumMetricsDestination
--
--         , requestGetAppMonitor $
--             newGetAppMonitor
--
--         , requestGetAppMonitorData $
--             newGetAppMonitorData
--
--         , requestListAppMonitors $
--             newListAppMonitors
--
--         , requestListRumMetricsDestinations $
--             newListRumMetricsDestinations
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPutRumEvents $
--             newPutRumEvents
--
--         , requestPutRumMetricsDestination $
--             newPutRumMetricsDestination
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateAppMonitor $
--             newUpdateAppMonitor
--
--         , requestUpdateRumMetricDefinition $
--             newUpdateRumMetricDefinition
--
--           ]

--     , testGroup "response"
--         [ responseBatchCreateRumMetricDefinitions $
--             newBatchCreateRumMetricDefinitionsResponse
--
--         , responseBatchDeleteRumMetricDefinitions $
--             newBatchDeleteRumMetricDefinitionsResponse
--
--         , responseBatchGetRumMetricDefinitions $
--             newBatchGetRumMetricDefinitionsResponse
--
--         , responseCreateAppMonitor $
--             newCreateAppMonitorResponse
--
--         , responseDeleteAppMonitor $
--             newDeleteAppMonitorResponse
--
--         , responseDeleteRumMetricsDestination $
--             newDeleteRumMetricsDestinationResponse
--
--         , responseGetAppMonitor $
--             newGetAppMonitorResponse
--
--         , responseGetAppMonitorData $
--             newGetAppMonitorDataResponse
--
--         , responseListAppMonitors $
--             newListAppMonitorsResponse
--
--         , responseListRumMetricsDestinations $
--             newListRumMetricsDestinationsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePutRumEvents $
--             newPutRumEventsResponse
--
--         , responsePutRumMetricsDestination $
--             newPutRumMetricsDestinationResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateAppMonitor $
--             newUpdateAppMonitorResponse
--
--         , responseUpdateRumMetricDefinition $
--             newUpdateRumMetricDefinitionResponse
--
--           ]
--     ]

-- Requests

requestBatchCreateRumMetricDefinitions :: BatchCreateRumMetricDefinitions -> TestTree
requestBatchCreateRumMetricDefinitions =
  req
    "BatchCreateRumMetricDefinitions"
    "fixture/BatchCreateRumMetricDefinitions.yaml"

requestBatchDeleteRumMetricDefinitions :: BatchDeleteRumMetricDefinitions -> TestTree
requestBatchDeleteRumMetricDefinitions =
  req
    "BatchDeleteRumMetricDefinitions"
    "fixture/BatchDeleteRumMetricDefinitions.yaml"

requestBatchGetRumMetricDefinitions :: BatchGetRumMetricDefinitions -> TestTree
requestBatchGetRumMetricDefinitions =
  req
    "BatchGetRumMetricDefinitions"
    "fixture/BatchGetRumMetricDefinitions.yaml"

requestCreateAppMonitor :: CreateAppMonitor -> TestTree
requestCreateAppMonitor =
  req
    "CreateAppMonitor"
    "fixture/CreateAppMonitor.yaml"

requestDeleteAppMonitor :: DeleteAppMonitor -> TestTree
requestDeleteAppMonitor =
  req
    "DeleteAppMonitor"
    "fixture/DeleteAppMonitor.yaml"

requestDeleteRumMetricsDestination :: DeleteRumMetricsDestination -> TestTree
requestDeleteRumMetricsDestination =
  req
    "DeleteRumMetricsDestination"
    "fixture/DeleteRumMetricsDestination.yaml"

requestGetAppMonitor :: GetAppMonitor -> TestTree
requestGetAppMonitor =
  req
    "GetAppMonitor"
    "fixture/GetAppMonitor.yaml"

requestGetAppMonitorData :: GetAppMonitorData -> TestTree
requestGetAppMonitorData =
  req
    "GetAppMonitorData"
    "fixture/GetAppMonitorData.yaml"

requestListAppMonitors :: ListAppMonitors -> TestTree
requestListAppMonitors =
  req
    "ListAppMonitors"
    "fixture/ListAppMonitors.yaml"

requestListRumMetricsDestinations :: ListRumMetricsDestinations -> TestTree
requestListRumMetricsDestinations =
  req
    "ListRumMetricsDestinations"
    "fixture/ListRumMetricsDestinations.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPutRumEvents :: PutRumEvents -> TestTree
requestPutRumEvents =
  req
    "PutRumEvents"
    "fixture/PutRumEvents.yaml"

requestPutRumMetricsDestination :: PutRumMetricsDestination -> TestTree
requestPutRumMetricsDestination =
  req
    "PutRumMetricsDestination"
    "fixture/PutRumMetricsDestination.yaml"

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

requestUpdateAppMonitor :: UpdateAppMonitor -> TestTree
requestUpdateAppMonitor =
  req
    "UpdateAppMonitor"
    "fixture/UpdateAppMonitor.yaml"

requestUpdateRumMetricDefinition :: UpdateRumMetricDefinition -> TestTree
requestUpdateRumMetricDefinition =
  req
    "UpdateRumMetricDefinition"
    "fixture/UpdateRumMetricDefinition.yaml"

-- Responses

responseBatchCreateRumMetricDefinitions :: BatchCreateRumMetricDefinitionsResponse -> TestTree
responseBatchCreateRumMetricDefinitions =
  res
    "BatchCreateRumMetricDefinitionsResponse"
    "fixture/BatchCreateRumMetricDefinitionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchCreateRumMetricDefinitions)

responseBatchDeleteRumMetricDefinitions :: BatchDeleteRumMetricDefinitionsResponse -> TestTree
responseBatchDeleteRumMetricDefinitions =
  res
    "BatchDeleteRumMetricDefinitionsResponse"
    "fixture/BatchDeleteRumMetricDefinitionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDeleteRumMetricDefinitions)

responseBatchGetRumMetricDefinitions :: BatchGetRumMetricDefinitionsResponse -> TestTree
responseBatchGetRumMetricDefinitions =
  res
    "BatchGetRumMetricDefinitionsResponse"
    "fixture/BatchGetRumMetricDefinitionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetRumMetricDefinitions)

responseCreateAppMonitor :: CreateAppMonitorResponse -> TestTree
responseCreateAppMonitor =
  res
    "CreateAppMonitorResponse"
    "fixture/CreateAppMonitorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAppMonitor)

responseDeleteAppMonitor :: DeleteAppMonitorResponse -> TestTree
responseDeleteAppMonitor =
  res
    "DeleteAppMonitorResponse"
    "fixture/DeleteAppMonitorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAppMonitor)

responseDeleteRumMetricsDestination :: DeleteRumMetricsDestinationResponse -> TestTree
responseDeleteRumMetricsDestination =
  res
    "DeleteRumMetricsDestinationResponse"
    "fixture/DeleteRumMetricsDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRumMetricsDestination)

responseGetAppMonitor :: GetAppMonitorResponse -> TestTree
responseGetAppMonitor =
  res
    "GetAppMonitorResponse"
    "fixture/GetAppMonitorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAppMonitor)

responseGetAppMonitorData :: GetAppMonitorDataResponse -> TestTree
responseGetAppMonitorData =
  res
    "GetAppMonitorDataResponse"
    "fixture/GetAppMonitorDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAppMonitorData)

responseListAppMonitors :: ListAppMonitorsResponse -> TestTree
responseListAppMonitors =
  res
    "ListAppMonitorsResponse"
    "fixture/ListAppMonitorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAppMonitors)

responseListRumMetricsDestinations :: ListRumMetricsDestinationsResponse -> TestTree
responseListRumMetricsDestinations =
  res
    "ListRumMetricsDestinationsResponse"
    "fixture/ListRumMetricsDestinationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRumMetricsDestinations)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePutRumEvents :: PutRumEventsResponse -> TestTree
responsePutRumEvents =
  res
    "PutRumEventsResponse"
    "fixture/PutRumEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutRumEvents)

responsePutRumMetricsDestination :: PutRumMetricsDestinationResponse -> TestTree
responsePutRumMetricsDestination =
  res
    "PutRumMetricsDestinationResponse"
    "fixture/PutRumMetricsDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutRumMetricsDestination)

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

responseUpdateAppMonitor :: UpdateAppMonitorResponse -> TestTree
responseUpdateAppMonitor =
  res
    "UpdateAppMonitorResponse"
    "fixture/UpdateAppMonitorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAppMonitor)

responseUpdateRumMetricDefinition :: UpdateRumMetricDefinitionResponse -> TestTree
responseUpdateRumMetricDefinition =
  res
    "UpdateRumMetricDefinitionResponse"
    "fixture/UpdateRumMetricDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRumMetricDefinition)
