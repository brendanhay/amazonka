{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Discovery
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.Discovery where

import Data.Proxy
import Network.AWS.Discovery
import Test.AWS.Discovery.Internal
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
--         [ requestDescribeTags $
--             newDescribeTags
--
--         , requestDescribeContinuousExports $
--             newDescribeContinuousExports
--
--         , requestStopDataCollectionByAgentIds $
--             newStopDataCollectionByAgentIds
--
--         , requestCreateTags $
--             newCreateTags
--
--         , requestBatchDeleteImportData $
--             newBatchDeleteImportData
--
--         , requestDeleteTags $
--             newDeleteTags
--
--         , requestStartImportTask $
--             newStartImportTask
--
--         , requestDeleteApplications $
--             newDeleteApplications
--
--         , requestUpdateApplication $
--             newUpdateApplication
--
--         , requestDescribeConfigurations $
--             newDescribeConfigurations
--
--         , requestDescribeImportTasks $
--             newDescribeImportTasks
--
--         , requestCreateApplication $
--             newCreateApplication
--
--         , requestListConfigurations $
--             newListConfigurations
--
--         , requestStartContinuousExport $
--             newStartContinuousExport
--
--         , requestDescribeAgents $
--             newDescribeAgents
--
--         , requestDescribeExportTasks $
--             newDescribeExportTasks
--
--         , requestStartDataCollectionByAgentIds $
--             newStartDataCollectionByAgentIds
--
--         , requestGetDiscoverySummary $
--             newGetDiscoverySummary
--
--         , requestDisassociateConfigurationItemsFromApplication $
--             newDisassociateConfigurationItemsFromApplication
--
--         , requestAssociateConfigurationItemsToApplication $
--             newAssociateConfigurationItemsToApplication
--
--         , requestListServerNeighbors $
--             newListServerNeighbors
--
--         , requestStopContinuousExport $
--             newStopContinuousExport
--
--         , requestStartExportTask $
--             newStartExportTask
--
--           ]

--     , testGroup "response"
--         [ responseDescribeTags $
--             newDescribeTagsResponse
--
--         , responseDescribeContinuousExports $
--             newDescribeContinuousExportsResponse
--
--         , responseStopDataCollectionByAgentIds $
--             newStopDataCollectionByAgentIdsResponse
--
--         , responseCreateTags $
--             newCreateTagsResponse
--
--         , responseBatchDeleteImportData $
--             newBatchDeleteImportDataResponse
--
--         , responseDeleteTags $
--             newDeleteTagsResponse
--
--         , responseStartImportTask $
--             newStartImportTaskResponse
--
--         , responseDeleteApplications $
--             newDeleteApplicationsResponse
--
--         , responseUpdateApplication $
--             newUpdateApplicationResponse
--
--         , responseDescribeConfigurations $
--             newDescribeConfigurationsResponse
--
--         , responseDescribeImportTasks $
--             newDescribeImportTasksResponse
--
--         , responseCreateApplication $
--             newCreateApplicationResponse
--
--         , responseListConfigurations $
--             newListConfigurationsResponse
--
--         , responseStartContinuousExport $
--             newStartContinuousExportResponse
--
--         , responseDescribeAgents $
--             newDescribeAgentsResponse
--
--         , responseDescribeExportTasks $
--             newDescribeExportTasksResponse
--
--         , responseStartDataCollectionByAgentIds $
--             newStartDataCollectionByAgentIdsResponse
--
--         , responseGetDiscoverySummary $
--             newGetDiscoverySummaryResponse
--
--         , responseDisassociateConfigurationItemsFromApplication $
--             newDisassociateConfigurationItemsFromApplicationResponse
--
--         , responseAssociateConfigurationItemsToApplication $
--             newAssociateConfigurationItemsToApplicationResponse
--
--         , responseListServerNeighbors $
--             newListServerNeighborsResponse
--
--         , responseStopContinuousExport $
--             newStopContinuousExportResponse
--
--         , responseStartExportTask $
--             newStartExportTaskResponse
--
--           ]
--     ]

-- Requests

requestDescribeTags :: DescribeTags -> TestTree
requestDescribeTags =
  req
    "DescribeTags"
    "fixture/DescribeTags.yaml"

requestDescribeContinuousExports :: DescribeContinuousExports -> TestTree
requestDescribeContinuousExports =
  req
    "DescribeContinuousExports"
    "fixture/DescribeContinuousExports.yaml"

requestStopDataCollectionByAgentIds :: StopDataCollectionByAgentIds -> TestTree
requestStopDataCollectionByAgentIds =
  req
    "StopDataCollectionByAgentIds"
    "fixture/StopDataCollectionByAgentIds.yaml"

requestCreateTags :: CreateTags -> TestTree
requestCreateTags =
  req
    "CreateTags"
    "fixture/CreateTags.yaml"

requestBatchDeleteImportData :: BatchDeleteImportData -> TestTree
requestBatchDeleteImportData =
  req
    "BatchDeleteImportData"
    "fixture/BatchDeleteImportData.yaml"

requestDeleteTags :: DeleteTags -> TestTree
requestDeleteTags =
  req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

requestStartImportTask :: StartImportTask -> TestTree
requestStartImportTask =
  req
    "StartImportTask"
    "fixture/StartImportTask.yaml"

requestDeleteApplications :: DeleteApplications -> TestTree
requestDeleteApplications =
  req
    "DeleteApplications"
    "fixture/DeleteApplications.yaml"

requestUpdateApplication :: UpdateApplication -> TestTree
requestUpdateApplication =
  req
    "UpdateApplication"
    "fixture/UpdateApplication.yaml"

requestDescribeConfigurations :: DescribeConfigurations -> TestTree
requestDescribeConfigurations =
  req
    "DescribeConfigurations"
    "fixture/DescribeConfigurations.yaml"

requestDescribeImportTasks :: DescribeImportTasks -> TestTree
requestDescribeImportTasks =
  req
    "DescribeImportTasks"
    "fixture/DescribeImportTasks.yaml"

requestCreateApplication :: CreateApplication -> TestTree
requestCreateApplication =
  req
    "CreateApplication"
    "fixture/CreateApplication.yaml"

requestListConfigurations :: ListConfigurations -> TestTree
requestListConfigurations =
  req
    "ListConfigurations"
    "fixture/ListConfigurations.yaml"

requestStartContinuousExport :: StartContinuousExport -> TestTree
requestStartContinuousExport =
  req
    "StartContinuousExport"
    "fixture/StartContinuousExport.yaml"

requestDescribeAgents :: DescribeAgents -> TestTree
requestDescribeAgents =
  req
    "DescribeAgents"
    "fixture/DescribeAgents.yaml"

requestDescribeExportTasks :: DescribeExportTasks -> TestTree
requestDescribeExportTasks =
  req
    "DescribeExportTasks"
    "fixture/DescribeExportTasks.yaml"

requestStartDataCollectionByAgentIds :: StartDataCollectionByAgentIds -> TestTree
requestStartDataCollectionByAgentIds =
  req
    "StartDataCollectionByAgentIds"
    "fixture/StartDataCollectionByAgentIds.yaml"

requestGetDiscoverySummary :: GetDiscoverySummary -> TestTree
requestGetDiscoverySummary =
  req
    "GetDiscoverySummary"
    "fixture/GetDiscoverySummary.yaml"

requestDisassociateConfigurationItemsFromApplication :: DisassociateConfigurationItemsFromApplication -> TestTree
requestDisassociateConfigurationItemsFromApplication =
  req
    "DisassociateConfigurationItemsFromApplication"
    "fixture/DisassociateConfigurationItemsFromApplication.yaml"

requestAssociateConfigurationItemsToApplication :: AssociateConfigurationItemsToApplication -> TestTree
requestAssociateConfigurationItemsToApplication =
  req
    "AssociateConfigurationItemsToApplication"
    "fixture/AssociateConfigurationItemsToApplication.yaml"

requestListServerNeighbors :: ListServerNeighbors -> TestTree
requestListServerNeighbors =
  req
    "ListServerNeighbors"
    "fixture/ListServerNeighbors.yaml"

requestStopContinuousExport :: StopContinuousExport -> TestTree
requestStopContinuousExport =
  req
    "StopContinuousExport"
    "fixture/StopContinuousExport.yaml"

requestStartExportTask :: StartExportTask -> TestTree
requestStartExportTask =
  req
    "StartExportTask"
    "fixture/StartExportTask.yaml"

-- Responses

responseDescribeTags :: DescribeTagsResponse -> TestTree
responseDescribeTags =
  res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTags)

responseDescribeContinuousExports :: DescribeContinuousExportsResponse -> TestTree
responseDescribeContinuousExports =
  res
    "DescribeContinuousExportsResponse"
    "fixture/DescribeContinuousExportsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeContinuousExports)

responseStopDataCollectionByAgentIds :: StopDataCollectionByAgentIdsResponse -> TestTree
responseStopDataCollectionByAgentIds =
  res
    "StopDataCollectionByAgentIdsResponse"
    "fixture/StopDataCollectionByAgentIdsResponse.proto"
    defaultService
    (Proxy :: Proxy StopDataCollectionByAgentIds)

responseCreateTags :: CreateTagsResponse -> TestTree
responseCreateTags =
  res
    "CreateTagsResponse"
    "fixture/CreateTagsResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTags)

responseBatchDeleteImportData :: BatchDeleteImportDataResponse -> TestTree
responseBatchDeleteImportData =
  res
    "BatchDeleteImportDataResponse"
    "fixture/BatchDeleteImportDataResponse.proto"
    defaultService
    (Proxy :: Proxy BatchDeleteImportData)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags =
  res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTags)

responseStartImportTask :: StartImportTaskResponse -> TestTree
responseStartImportTask =
  res
    "StartImportTaskResponse"
    "fixture/StartImportTaskResponse.proto"
    defaultService
    (Proxy :: Proxy StartImportTask)

responseDeleteApplications :: DeleteApplicationsResponse -> TestTree
responseDeleteApplications =
  res
    "DeleteApplicationsResponse"
    "fixture/DeleteApplicationsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteApplications)

responseUpdateApplication :: UpdateApplicationResponse -> TestTree
responseUpdateApplication =
  res
    "UpdateApplicationResponse"
    "fixture/UpdateApplicationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateApplication)

responseDescribeConfigurations :: DescribeConfigurationsResponse -> TestTree
responseDescribeConfigurations =
  res
    "DescribeConfigurationsResponse"
    "fixture/DescribeConfigurationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeConfigurations)

responseDescribeImportTasks :: DescribeImportTasksResponse -> TestTree
responseDescribeImportTasks =
  res
    "DescribeImportTasksResponse"
    "fixture/DescribeImportTasksResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeImportTasks)

responseCreateApplication :: CreateApplicationResponse -> TestTree
responseCreateApplication =
  res
    "CreateApplicationResponse"
    "fixture/CreateApplicationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateApplication)

responseListConfigurations :: ListConfigurationsResponse -> TestTree
responseListConfigurations =
  res
    "ListConfigurationsResponse"
    "fixture/ListConfigurationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListConfigurations)

responseStartContinuousExport :: StartContinuousExportResponse -> TestTree
responseStartContinuousExport =
  res
    "StartContinuousExportResponse"
    "fixture/StartContinuousExportResponse.proto"
    defaultService
    (Proxy :: Proxy StartContinuousExport)

responseDescribeAgents :: DescribeAgentsResponse -> TestTree
responseDescribeAgents =
  res
    "DescribeAgentsResponse"
    "fixture/DescribeAgentsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAgents)

responseDescribeExportTasks :: DescribeExportTasksResponse -> TestTree
responseDescribeExportTasks =
  res
    "DescribeExportTasksResponse"
    "fixture/DescribeExportTasksResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeExportTasks)

responseStartDataCollectionByAgentIds :: StartDataCollectionByAgentIdsResponse -> TestTree
responseStartDataCollectionByAgentIds =
  res
    "StartDataCollectionByAgentIdsResponse"
    "fixture/StartDataCollectionByAgentIdsResponse.proto"
    defaultService
    (Proxy :: Proxy StartDataCollectionByAgentIds)

responseGetDiscoverySummary :: GetDiscoverySummaryResponse -> TestTree
responseGetDiscoverySummary =
  res
    "GetDiscoverySummaryResponse"
    "fixture/GetDiscoverySummaryResponse.proto"
    defaultService
    (Proxy :: Proxy GetDiscoverySummary)

responseDisassociateConfigurationItemsFromApplication :: DisassociateConfigurationItemsFromApplicationResponse -> TestTree
responseDisassociateConfigurationItemsFromApplication =
  res
    "DisassociateConfigurationItemsFromApplicationResponse"
    "fixture/DisassociateConfigurationItemsFromApplicationResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateConfigurationItemsFromApplication)

responseAssociateConfigurationItemsToApplication :: AssociateConfigurationItemsToApplicationResponse -> TestTree
responseAssociateConfigurationItemsToApplication =
  res
    "AssociateConfigurationItemsToApplicationResponse"
    "fixture/AssociateConfigurationItemsToApplicationResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateConfigurationItemsToApplication)

responseListServerNeighbors :: ListServerNeighborsResponse -> TestTree
responseListServerNeighbors =
  res
    "ListServerNeighborsResponse"
    "fixture/ListServerNeighborsResponse.proto"
    defaultService
    (Proxy :: Proxy ListServerNeighbors)

responseStopContinuousExport :: StopContinuousExportResponse -> TestTree
responseStopContinuousExport =
  res
    "StopContinuousExportResponse"
    "fixture/StopContinuousExportResponse.proto"
    defaultService
    (Proxy :: Proxy StopContinuousExport)

responseStartExportTask :: StartExportTaskResponse -> TestTree
responseStartExportTask =
  res
    "StartExportTaskResponse"
    "fixture/StartExportTaskResponse.proto"
    defaultService
    (Proxy :: Proxy StartExportTask)
