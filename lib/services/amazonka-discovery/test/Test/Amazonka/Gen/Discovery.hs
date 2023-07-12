{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Discovery
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Discovery where

import Amazonka.Discovery
import qualified Data.Proxy as Proxy
import Test.Amazonka.Discovery.Internal
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
--         [ requestAssociateConfigurationItemsToApplication $
--             newAssociateConfigurationItemsToApplication
--
--         , requestBatchDeleteImportData $
--             newBatchDeleteImportData
--
--         , requestCreateApplication $
--             newCreateApplication
--
--         , requestCreateTags $
--             newCreateTags
--
--         , requestDeleteApplications $
--             newDeleteApplications
--
--         , requestDeleteTags $
--             newDeleteTags
--
--         , requestDescribeAgents $
--             newDescribeAgents
--
--         , requestDescribeConfigurations $
--             newDescribeConfigurations
--
--         , requestDescribeContinuousExports $
--             newDescribeContinuousExports
--
--         , requestDescribeExportTasks $
--             newDescribeExportTasks
--
--         , requestDescribeImportTasks $
--             newDescribeImportTasks
--
--         , requestDescribeTags $
--             newDescribeTags
--
--         , requestDisassociateConfigurationItemsFromApplication $
--             newDisassociateConfigurationItemsFromApplication
--
--         , requestGetDiscoverySummary $
--             newGetDiscoverySummary
--
--         , requestListConfigurations $
--             newListConfigurations
--
--         , requestListServerNeighbors $
--             newListServerNeighbors
--
--         , requestStartContinuousExport $
--             newStartContinuousExport
--
--         , requestStartDataCollectionByAgentIds $
--             newStartDataCollectionByAgentIds
--
--         , requestStartExportTask $
--             newStartExportTask
--
--         , requestStartImportTask $
--             newStartImportTask
--
--         , requestStopContinuousExport $
--             newStopContinuousExport
--
--         , requestStopDataCollectionByAgentIds $
--             newStopDataCollectionByAgentIds
--
--         , requestUpdateApplication $
--             newUpdateApplication
--
--           ]

--     , testGroup "response"
--         [ responseAssociateConfigurationItemsToApplication $
--             newAssociateConfigurationItemsToApplicationResponse
--
--         , responseBatchDeleteImportData $
--             newBatchDeleteImportDataResponse
--
--         , responseCreateApplication $
--             newCreateApplicationResponse
--
--         , responseCreateTags $
--             newCreateTagsResponse
--
--         , responseDeleteApplications $
--             newDeleteApplicationsResponse
--
--         , responseDeleteTags $
--             newDeleteTagsResponse
--
--         , responseDescribeAgents $
--             newDescribeAgentsResponse
--
--         , responseDescribeConfigurations $
--             newDescribeConfigurationsResponse
--
--         , responseDescribeContinuousExports $
--             newDescribeContinuousExportsResponse
--
--         , responseDescribeExportTasks $
--             newDescribeExportTasksResponse
--
--         , responseDescribeImportTasks $
--             newDescribeImportTasksResponse
--
--         , responseDescribeTags $
--             newDescribeTagsResponse
--
--         , responseDisassociateConfigurationItemsFromApplication $
--             newDisassociateConfigurationItemsFromApplicationResponse
--
--         , responseGetDiscoverySummary $
--             newGetDiscoverySummaryResponse
--
--         , responseListConfigurations $
--             newListConfigurationsResponse
--
--         , responseListServerNeighbors $
--             newListServerNeighborsResponse
--
--         , responseStartContinuousExport $
--             newStartContinuousExportResponse
--
--         , responseStartDataCollectionByAgentIds $
--             newStartDataCollectionByAgentIdsResponse
--
--         , responseStartExportTask $
--             newStartExportTaskResponse
--
--         , responseStartImportTask $
--             newStartImportTaskResponse
--
--         , responseStopContinuousExport $
--             newStopContinuousExportResponse
--
--         , responseStopDataCollectionByAgentIds $
--             newStopDataCollectionByAgentIdsResponse
--
--         , responseUpdateApplication $
--             newUpdateApplicationResponse
--
--           ]
--     ]

-- Requests

requestAssociateConfigurationItemsToApplication :: AssociateConfigurationItemsToApplication -> TestTree
requestAssociateConfigurationItemsToApplication =
  req
    "AssociateConfigurationItemsToApplication"
    "fixture/AssociateConfigurationItemsToApplication.yaml"

requestBatchDeleteImportData :: BatchDeleteImportData -> TestTree
requestBatchDeleteImportData =
  req
    "BatchDeleteImportData"
    "fixture/BatchDeleteImportData.yaml"

requestCreateApplication :: CreateApplication -> TestTree
requestCreateApplication =
  req
    "CreateApplication"
    "fixture/CreateApplication.yaml"

requestCreateTags :: CreateTags -> TestTree
requestCreateTags =
  req
    "CreateTags"
    "fixture/CreateTags.yaml"

requestDeleteApplications :: DeleteApplications -> TestTree
requestDeleteApplications =
  req
    "DeleteApplications"
    "fixture/DeleteApplications.yaml"

requestDeleteTags :: DeleteTags -> TestTree
requestDeleteTags =
  req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

requestDescribeAgents :: DescribeAgents -> TestTree
requestDescribeAgents =
  req
    "DescribeAgents"
    "fixture/DescribeAgents.yaml"

requestDescribeConfigurations :: DescribeConfigurations -> TestTree
requestDescribeConfigurations =
  req
    "DescribeConfigurations"
    "fixture/DescribeConfigurations.yaml"

requestDescribeContinuousExports :: DescribeContinuousExports -> TestTree
requestDescribeContinuousExports =
  req
    "DescribeContinuousExports"
    "fixture/DescribeContinuousExports.yaml"

requestDescribeExportTasks :: DescribeExportTasks -> TestTree
requestDescribeExportTasks =
  req
    "DescribeExportTasks"
    "fixture/DescribeExportTasks.yaml"

requestDescribeImportTasks :: DescribeImportTasks -> TestTree
requestDescribeImportTasks =
  req
    "DescribeImportTasks"
    "fixture/DescribeImportTasks.yaml"

requestDescribeTags :: DescribeTags -> TestTree
requestDescribeTags =
  req
    "DescribeTags"
    "fixture/DescribeTags.yaml"

requestDisassociateConfigurationItemsFromApplication :: DisassociateConfigurationItemsFromApplication -> TestTree
requestDisassociateConfigurationItemsFromApplication =
  req
    "DisassociateConfigurationItemsFromApplication"
    "fixture/DisassociateConfigurationItemsFromApplication.yaml"

requestGetDiscoverySummary :: GetDiscoverySummary -> TestTree
requestGetDiscoverySummary =
  req
    "GetDiscoverySummary"
    "fixture/GetDiscoverySummary.yaml"

requestListConfigurations :: ListConfigurations -> TestTree
requestListConfigurations =
  req
    "ListConfigurations"
    "fixture/ListConfigurations.yaml"

requestListServerNeighbors :: ListServerNeighbors -> TestTree
requestListServerNeighbors =
  req
    "ListServerNeighbors"
    "fixture/ListServerNeighbors.yaml"

requestStartContinuousExport :: StartContinuousExport -> TestTree
requestStartContinuousExport =
  req
    "StartContinuousExport"
    "fixture/StartContinuousExport.yaml"

requestStartDataCollectionByAgentIds :: StartDataCollectionByAgentIds -> TestTree
requestStartDataCollectionByAgentIds =
  req
    "StartDataCollectionByAgentIds"
    "fixture/StartDataCollectionByAgentIds.yaml"

requestStartExportTask :: StartExportTask -> TestTree
requestStartExportTask =
  req
    "StartExportTask"
    "fixture/StartExportTask.yaml"

requestStartImportTask :: StartImportTask -> TestTree
requestStartImportTask =
  req
    "StartImportTask"
    "fixture/StartImportTask.yaml"

requestStopContinuousExport :: StopContinuousExport -> TestTree
requestStopContinuousExport =
  req
    "StopContinuousExport"
    "fixture/StopContinuousExport.yaml"

requestStopDataCollectionByAgentIds :: StopDataCollectionByAgentIds -> TestTree
requestStopDataCollectionByAgentIds =
  req
    "StopDataCollectionByAgentIds"
    "fixture/StopDataCollectionByAgentIds.yaml"

requestUpdateApplication :: UpdateApplication -> TestTree
requestUpdateApplication =
  req
    "UpdateApplication"
    "fixture/UpdateApplication.yaml"

-- Responses

responseAssociateConfigurationItemsToApplication :: AssociateConfigurationItemsToApplicationResponse -> TestTree
responseAssociateConfigurationItemsToApplication =
  res
    "AssociateConfigurationItemsToApplicationResponse"
    "fixture/AssociateConfigurationItemsToApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateConfigurationItemsToApplication)

responseBatchDeleteImportData :: BatchDeleteImportDataResponse -> TestTree
responseBatchDeleteImportData =
  res
    "BatchDeleteImportDataResponse"
    "fixture/BatchDeleteImportDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDeleteImportData)

responseCreateApplication :: CreateApplicationResponse -> TestTree
responseCreateApplication =
  res
    "CreateApplicationResponse"
    "fixture/CreateApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateApplication)

responseCreateTags :: CreateTagsResponse -> TestTree
responseCreateTags =
  res
    "CreateTagsResponse"
    "fixture/CreateTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTags)

responseDeleteApplications :: DeleteApplicationsResponse -> TestTree
responseDeleteApplications =
  res
    "DeleteApplicationsResponse"
    "fixture/DeleteApplicationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApplications)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags =
  res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTags)

responseDescribeAgents :: DescribeAgentsResponse -> TestTree
responseDescribeAgents =
  res
    "DescribeAgentsResponse"
    "fixture/DescribeAgentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAgents)

responseDescribeConfigurations :: DescribeConfigurationsResponse -> TestTree
responseDescribeConfigurations =
  res
    "DescribeConfigurationsResponse"
    "fixture/DescribeConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConfigurations)

responseDescribeContinuousExports :: DescribeContinuousExportsResponse -> TestTree
responseDescribeContinuousExports =
  res
    "DescribeContinuousExportsResponse"
    "fixture/DescribeContinuousExportsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeContinuousExports)

responseDescribeExportTasks :: DescribeExportTasksResponse -> TestTree
responseDescribeExportTasks =
  res
    "DescribeExportTasksResponse"
    "fixture/DescribeExportTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeExportTasks)

responseDescribeImportTasks :: DescribeImportTasksResponse -> TestTree
responseDescribeImportTasks =
  res
    "DescribeImportTasksResponse"
    "fixture/DescribeImportTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeImportTasks)

responseDescribeTags :: DescribeTagsResponse -> TestTree
responseDescribeTags =
  res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTags)

responseDisassociateConfigurationItemsFromApplication :: DisassociateConfigurationItemsFromApplicationResponse -> TestTree
responseDisassociateConfigurationItemsFromApplication =
  res
    "DisassociateConfigurationItemsFromApplicationResponse"
    "fixture/DisassociateConfigurationItemsFromApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateConfigurationItemsFromApplication)

responseGetDiscoverySummary :: GetDiscoverySummaryResponse -> TestTree
responseGetDiscoverySummary =
  res
    "GetDiscoverySummaryResponse"
    "fixture/GetDiscoverySummaryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDiscoverySummary)

responseListConfigurations :: ListConfigurationsResponse -> TestTree
responseListConfigurations =
  res
    "ListConfigurationsResponse"
    "fixture/ListConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListConfigurations)

responseListServerNeighbors :: ListServerNeighborsResponse -> TestTree
responseListServerNeighbors =
  res
    "ListServerNeighborsResponse"
    "fixture/ListServerNeighborsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListServerNeighbors)

responseStartContinuousExport :: StartContinuousExportResponse -> TestTree
responseStartContinuousExport =
  res
    "StartContinuousExportResponse"
    "fixture/StartContinuousExportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartContinuousExport)

responseStartDataCollectionByAgentIds :: StartDataCollectionByAgentIdsResponse -> TestTree
responseStartDataCollectionByAgentIds =
  res
    "StartDataCollectionByAgentIdsResponse"
    "fixture/StartDataCollectionByAgentIdsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartDataCollectionByAgentIds)

responseStartExportTask :: StartExportTaskResponse -> TestTree
responseStartExportTask =
  res
    "StartExportTaskResponse"
    "fixture/StartExportTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartExportTask)

responseStartImportTask :: StartImportTaskResponse -> TestTree
responseStartImportTask =
  res
    "StartImportTaskResponse"
    "fixture/StartImportTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartImportTask)

responseStopContinuousExport :: StopContinuousExportResponse -> TestTree
responseStopContinuousExport =
  res
    "StopContinuousExportResponse"
    "fixture/StopContinuousExportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopContinuousExport)

responseStopDataCollectionByAgentIds :: StopDataCollectionByAgentIdsResponse -> TestTree
responseStopDataCollectionByAgentIds =
  res
    "StopDataCollectionByAgentIdsResponse"
    "fixture/StopDataCollectionByAgentIdsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopDataCollectionByAgentIds)

responseUpdateApplication :: UpdateApplicationResponse -> TestTree
responseUpdateApplication =
  res
    "UpdateApplicationResponse"
    "fixture/UpdateApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApplication)
