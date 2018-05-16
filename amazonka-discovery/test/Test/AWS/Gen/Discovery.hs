{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Discovery
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
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
--             describeTags
--
--         , requestStopDataCollectionByAgentIds $
--             stopDataCollectionByAgentIds
--
--         , requestCreateTags $
--             createTags
--
--         , requestDeleteTags $
--             deleteTags
--
--         , requestDeleteApplications $
--             deleteApplications
--
--         , requestUpdateApplication $
--             updateApplication
--
--         , requestDescribeConfigurations $
--             describeConfigurations
--
--         , requestCreateApplication $
--             createApplication
--
--         , requestListConfigurations $
--             listConfigurations
--
--         , requestDescribeAgents $
--             describeAgents
--
--         , requestDescribeExportTasks $
--             describeExportTasks
--
--         , requestStartDataCollectionByAgentIds $
--             startDataCollectionByAgentIds
--
--         , requestGetDiscoverySummary $
--             getDiscoverySummary
--
--         , requestDisassociateConfigurationItemsFromApplication $
--             disassociateConfigurationItemsFromApplication
--
--         , requestAssociateConfigurationItemsToApplication $
--             associateConfigurationItemsToApplication
--
--         , requestListServerNeighbors $
--             listServerNeighbors
--
--         , requestStartExportTask $
--             startExportTask
--
--           ]

--     , testGroup "response"
--         [ responseDescribeTags $
--             describeTagsResponse
--
--         , responseStopDataCollectionByAgentIds $
--             stopDataCollectionByAgentIdsResponse
--
--         , responseCreateTags $
--             createTagsResponse
--
--         , responseDeleteTags $
--             deleteTagsResponse
--
--         , responseDeleteApplications $
--             deleteApplicationsResponse
--
--         , responseUpdateApplication $
--             updateApplicationResponse
--
--         , responseDescribeConfigurations $
--             describeConfigurationsResponse
--
--         , responseCreateApplication $
--             createApplicationResponse
--
--         , responseListConfigurations $
--             listConfigurationsResponse
--
--         , responseDescribeAgents $
--             describeAgentsResponse
--
--         , responseDescribeExportTasks $
--             describeExportTasksResponse
--
--         , responseStartDataCollectionByAgentIds $
--             startDataCollectionByAgentIdsResponse
--
--         , responseGetDiscoverySummary $
--             getDiscoverySummaryResponse
--
--         , responseDisassociateConfigurationItemsFromApplication $
--             disassociateConfigurationItemsFromApplicationResponse
--
--         , responseAssociateConfigurationItemsToApplication $
--             associateConfigurationItemsToApplicationResponse
--
--         , responseListServerNeighbors $
--             listServerNeighborsResponse
--
--         , responseStartExportTask $
--             startExportTaskResponse
--
--           ]
--     ]

-- Requests

requestDescribeTags :: DescribeTags -> TestTree
requestDescribeTags = req
    "DescribeTags"
    "fixture/DescribeTags.yaml"

requestStopDataCollectionByAgentIds :: StopDataCollectionByAgentIds -> TestTree
requestStopDataCollectionByAgentIds = req
    "StopDataCollectionByAgentIds"
    "fixture/StopDataCollectionByAgentIds.yaml"

requestCreateTags :: CreateTags -> TestTree
requestCreateTags = req
    "CreateTags"
    "fixture/CreateTags.yaml"

requestDeleteTags :: DeleteTags -> TestTree
requestDeleteTags = req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

requestDeleteApplications :: DeleteApplications -> TestTree
requestDeleteApplications = req
    "DeleteApplications"
    "fixture/DeleteApplications.yaml"

requestUpdateApplication :: UpdateApplication -> TestTree
requestUpdateApplication = req
    "UpdateApplication"
    "fixture/UpdateApplication.yaml"

requestDescribeConfigurations :: DescribeConfigurations -> TestTree
requestDescribeConfigurations = req
    "DescribeConfigurations"
    "fixture/DescribeConfigurations.yaml"

requestCreateApplication :: CreateApplication -> TestTree
requestCreateApplication = req
    "CreateApplication"
    "fixture/CreateApplication.yaml"

requestListConfigurations :: ListConfigurations -> TestTree
requestListConfigurations = req
    "ListConfigurations"
    "fixture/ListConfigurations.yaml"

requestDescribeAgents :: DescribeAgents -> TestTree
requestDescribeAgents = req
    "DescribeAgents"
    "fixture/DescribeAgents.yaml"

requestDescribeExportTasks :: DescribeExportTasks -> TestTree
requestDescribeExportTasks = req
    "DescribeExportTasks"
    "fixture/DescribeExportTasks.yaml"

requestStartDataCollectionByAgentIds :: StartDataCollectionByAgentIds -> TestTree
requestStartDataCollectionByAgentIds = req
    "StartDataCollectionByAgentIds"
    "fixture/StartDataCollectionByAgentIds.yaml"

requestGetDiscoverySummary :: GetDiscoverySummary -> TestTree
requestGetDiscoverySummary = req
    "GetDiscoverySummary"
    "fixture/GetDiscoverySummary.yaml"

requestDisassociateConfigurationItemsFromApplication :: DisassociateConfigurationItemsFromApplication -> TestTree
requestDisassociateConfigurationItemsFromApplication = req
    "DisassociateConfigurationItemsFromApplication"
    "fixture/DisassociateConfigurationItemsFromApplication.yaml"

requestAssociateConfigurationItemsToApplication :: AssociateConfigurationItemsToApplication -> TestTree
requestAssociateConfigurationItemsToApplication = req
    "AssociateConfigurationItemsToApplication"
    "fixture/AssociateConfigurationItemsToApplication.yaml"

requestListServerNeighbors :: ListServerNeighbors -> TestTree
requestListServerNeighbors = req
    "ListServerNeighbors"
    "fixture/ListServerNeighbors.yaml"

requestStartExportTask :: StartExportTask -> TestTree
requestStartExportTask = req
    "StartExportTask"
    "fixture/StartExportTask.yaml"

-- Responses

responseDescribeTags :: DescribeTagsResponse -> TestTree
responseDescribeTags = res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    discovery
    (Proxy :: Proxy DescribeTags)

responseStopDataCollectionByAgentIds :: StopDataCollectionByAgentIdsResponse -> TestTree
responseStopDataCollectionByAgentIds = res
    "StopDataCollectionByAgentIdsResponse"
    "fixture/StopDataCollectionByAgentIdsResponse.proto"
    discovery
    (Proxy :: Proxy StopDataCollectionByAgentIds)

responseCreateTags :: CreateTagsResponse -> TestTree
responseCreateTags = res
    "CreateTagsResponse"
    "fixture/CreateTagsResponse.proto"
    discovery
    (Proxy :: Proxy CreateTags)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags = res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    discovery
    (Proxy :: Proxy DeleteTags)

responseDeleteApplications :: DeleteApplicationsResponse -> TestTree
responseDeleteApplications = res
    "DeleteApplicationsResponse"
    "fixture/DeleteApplicationsResponse.proto"
    discovery
    (Proxy :: Proxy DeleteApplications)

responseUpdateApplication :: UpdateApplicationResponse -> TestTree
responseUpdateApplication = res
    "UpdateApplicationResponse"
    "fixture/UpdateApplicationResponse.proto"
    discovery
    (Proxy :: Proxy UpdateApplication)

responseDescribeConfigurations :: DescribeConfigurationsResponse -> TestTree
responseDescribeConfigurations = res
    "DescribeConfigurationsResponse"
    "fixture/DescribeConfigurationsResponse.proto"
    discovery
    (Proxy :: Proxy DescribeConfigurations)

responseCreateApplication :: CreateApplicationResponse -> TestTree
responseCreateApplication = res
    "CreateApplicationResponse"
    "fixture/CreateApplicationResponse.proto"
    discovery
    (Proxy :: Proxy CreateApplication)

responseListConfigurations :: ListConfigurationsResponse -> TestTree
responseListConfigurations = res
    "ListConfigurationsResponse"
    "fixture/ListConfigurationsResponse.proto"
    discovery
    (Proxy :: Proxy ListConfigurations)

responseDescribeAgents :: DescribeAgentsResponse -> TestTree
responseDescribeAgents = res
    "DescribeAgentsResponse"
    "fixture/DescribeAgentsResponse.proto"
    discovery
    (Proxy :: Proxy DescribeAgents)

responseDescribeExportTasks :: DescribeExportTasksResponse -> TestTree
responseDescribeExportTasks = res
    "DescribeExportTasksResponse"
    "fixture/DescribeExportTasksResponse.proto"
    discovery
    (Proxy :: Proxy DescribeExportTasks)

responseStartDataCollectionByAgentIds :: StartDataCollectionByAgentIdsResponse -> TestTree
responseStartDataCollectionByAgentIds = res
    "StartDataCollectionByAgentIdsResponse"
    "fixture/StartDataCollectionByAgentIdsResponse.proto"
    discovery
    (Proxy :: Proxy StartDataCollectionByAgentIds)

responseGetDiscoverySummary :: GetDiscoverySummaryResponse -> TestTree
responseGetDiscoverySummary = res
    "GetDiscoverySummaryResponse"
    "fixture/GetDiscoverySummaryResponse.proto"
    discovery
    (Proxy :: Proxy GetDiscoverySummary)

responseDisassociateConfigurationItemsFromApplication :: DisassociateConfigurationItemsFromApplicationResponse -> TestTree
responseDisassociateConfigurationItemsFromApplication = res
    "DisassociateConfigurationItemsFromApplicationResponse"
    "fixture/DisassociateConfigurationItemsFromApplicationResponse.proto"
    discovery
    (Proxy :: Proxy DisassociateConfigurationItemsFromApplication)

responseAssociateConfigurationItemsToApplication :: AssociateConfigurationItemsToApplicationResponse -> TestTree
responseAssociateConfigurationItemsToApplication = res
    "AssociateConfigurationItemsToApplicationResponse"
    "fixture/AssociateConfigurationItemsToApplicationResponse.proto"
    discovery
    (Proxy :: Proxy AssociateConfigurationItemsToApplication)

responseListServerNeighbors :: ListServerNeighborsResponse -> TestTree
responseListServerNeighbors = res
    "ListServerNeighborsResponse"
    "fixture/ListServerNeighborsResponse.proto"
    discovery
    (Proxy :: Proxy ListServerNeighbors)

responseStartExportTask :: StartExportTaskResponse -> TestTree
responseStartExportTask = res
    "StartExportTaskResponse"
    "fixture/StartExportTaskResponse.proto"
    discovery
    (Proxy :: Proxy StartExportTask)
