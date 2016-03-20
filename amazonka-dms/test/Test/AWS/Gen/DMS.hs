{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.DMS
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.DMS where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.DMS
import Test.AWS.DMS.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testDeleteReplicationInstance $
--             deleteReplicationInstance
--
--         , testCreateEndpoint $
--             createEndpoint
--
--         , testDescribeSchemas $
--             describeSchemas
--
--         , testDeleteEndpoint $
--             deleteEndpoint
--
--         , testListTagsForResource $
--             listTagsForResource
--
--         , testDescribeEndpointTypes $
--             describeEndpointTypes
--
--         , testDeleteReplicationTask $
--             deleteReplicationTask
--
--         , testTestConnection $
--             testConnection
--
--         , testDescribeConnections $
--             describeConnections
--
--         , testRemoveTagsFromResource $
--             removeTagsFromResource
--
--         , testModifyEndpoint $
--             modifyEndpoint
--
--         , testDescribeTableStatistics $
--             describeTableStatistics
--
--         , testDescribeReplicationSubnetGroups $
--             describeReplicationSubnetGroups
--
--         , testStartReplicationTask $
--             startReplicationTask
--
--         , testAddTagsToResource $
--             addTagsToResource
--
--         , testCreateReplicationSubnetGroup $
--             createReplicationSubnetGroup
--
--         , testRefreshSchemas $
--             refreshSchemas
--
--         , testDescribeReplicationTasks $
--             describeReplicationTasks
--
--         , testDescribeOrderableReplicationInstances $
--             describeOrderableReplicationInstances
--
--         , testCreateReplicationTask $
--             createReplicationTask
--
--         , testDescribeEndpoints $
--             describeEndpoints
--
--         , testModifyReplicationInstance $
--             modifyReplicationInstance
--
--         , testModifyReplicationSubnetGroup $
--             modifyReplicationSubnetGroup
--
--         , testDescribeAccountAttributes $
--             describeAccountAttributes
--
--         , testDescribeReplicationInstances $
--             describeReplicationInstances
--
--         , testDescribeRefreshSchemasStatus $
--             describeRefreshSchemasStatus
--
--         , testStopReplicationTask $
--             stopReplicationTask
--
--         , testCreateReplicationInstance $
--             createReplicationInstance
--
--         , testDeleteReplicationSubnetGroup $
--             deleteReplicationSubnetGroup
--
--           ]

--     , testGroup "response"
--         [ testDeleteReplicationInstanceResponse $
--             deleteReplicationInstanceResponse
--
--         , testCreateEndpointResponse $
--             createEndpointResponse
--
--         , testDescribeSchemasResponse $
--             describeSchemasResponse
--
--         , testDeleteEndpointResponse $
--             deleteEndpointResponse
--
--         , testListTagsForResourceResponse $
--             listTagsForResourceResponse
--
--         , testDescribeEndpointTypesResponse $
--             describeEndpointTypesResponse
--
--         , testDeleteReplicationTaskResponse $
--             deleteReplicationTaskResponse
--
--         , testTestConnectionResponse $
--             testConnectionResponse
--
--         , testDescribeConnectionsResponse $
--             describeConnectionsResponse
--
--         , testRemoveTagsFromResourceResponse $
--             removeTagsFromResourceResponse
--
--         , testModifyEndpointResponse $
--             modifyEndpointResponse
--
--         , testDescribeTableStatisticsResponse $
--             describeTableStatisticsResponse
--
--         , testDescribeReplicationSubnetGroupsResponse $
--             describeReplicationSubnetGroupsResponse
--
--         , testStartReplicationTaskResponse $
--             startReplicationTaskResponse
--
--         , testAddTagsToResourceResponse $
--             addTagsToResourceResponse
--
--         , testCreateReplicationSubnetGroupResponse $
--             createReplicationSubnetGroupResponse
--
--         , testRefreshSchemasResponse $
--             refreshSchemasResponse
--
--         , testDescribeReplicationTasksResponse $
--             describeReplicationTasksResponse
--
--         , testDescribeOrderableReplicationInstancesResponse $
--             describeOrderableReplicationInstancesResponse
--
--         , testCreateReplicationTaskResponse $
--             createReplicationTaskResponse
--
--         , testDescribeEndpointsResponse $
--             describeEndpointsResponse
--
--         , testModifyReplicationInstanceResponse $
--             modifyReplicationInstanceResponse
--
--         , testModifyReplicationSubnetGroupResponse $
--             modifyReplicationSubnetGroupResponse
--
--         , testDescribeAccountAttributesResponse $
--             describeAccountAttributesResponse
--
--         , testDescribeReplicationInstancesResponse $
--             describeReplicationInstancesResponse
--
--         , testDescribeRefreshSchemasStatusResponse $
--             describeRefreshSchemasStatusResponse
--
--         , testStopReplicationTaskResponse $
--             stopReplicationTaskResponse
--
--         , testCreateReplicationInstanceResponse $
--             createReplicationInstanceResponse
--
--         , testDeleteReplicationSubnetGroupResponse $
--             deleteReplicationSubnetGroupResponse
--
--           ]
--     ]

-- Requests

testDeleteReplicationInstance :: DeleteReplicationInstance -> TestTree
testDeleteReplicationInstance = req
    "DeleteReplicationInstance"
    "fixture/DeleteReplicationInstance.yaml"

testCreateEndpoint :: CreateEndpoint -> TestTree
testCreateEndpoint = req
    "CreateEndpoint"
    "fixture/CreateEndpoint.yaml"

testDescribeSchemas :: DescribeSchemas -> TestTree
testDescribeSchemas = req
    "DescribeSchemas"
    "fixture/DescribeSchemas.yaml"

testDeleteEndpoint :: DeleteEndpoint -> TestTree
testDeleteEndpoint = req
    "DeleteEndpoint"
    "fixture/DeleteEndpoint.yaml"

testListTagsForResource :: ListTagsForResource -> TestTree
testListTagsForResource = req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

testDescribeEndpointTypes :: DescribeEndpointTypes -> TestTree
testDescribeEndpointTypes = req
    "DescribeEndpointTypes"
    "fixture/DescribeEndpointTypes.yaml"

testDeleteReplicationTask :: DeleteReplicationTask -> TestTree
testDeleteReplicationTask = req
    "DeleteReplicationTask"
    "fixture/DeleteReplicationTask.yaml"

testTestConnection :: TestConnection -> TestTree
testTestConnection = req
    "TestConnection"
    "fixture/TestConnection.yaml"

testDescribeConnections :: DescribeConnections -> TestTree
testDescribeConnections = req
    "DescribeConnections"
    "fixture/DescribeConnections.yaml"

testRemoveTagsFromResource :: RemoveTagsFromResource -> TestTree
testRemoveTagsFromResource = req
    "RemoveTagsFromResource"
    "fixture/RemoveTagsFromResource.yaml"

testModifyEndpoint :: ModifyEndpoint -> TestTree
testModifyEndpoint = req
    "ModifyEndpoint"
    "fixture/ModifyEndpoint.yaml"

testDescribeTableStatistics :: DescribeTableStatistics -> TestTree
testDescribeTableStatistics = req
    "DescribeTableStatistics"
    "fixture/DescribeTableStatistics.yaml"

testDescribeReplicationSubnetGroups :: DescribeReplicationSubnetGroups -> TestTree
testDescribeReplicationSubnetGroups = req
    "DescribeReplicationSubnetGroups"
    "fixture/DescribeReplicationSubnetGroups.yaml"

testStartReplicationTask :: StartReplicationTask -> TestTree
testStartReplicationTask = req
    "StartReplicationTask"
    "fixture/StartReplicationTask.yaml"

testAddTagsToResource :: AddTagsToResource -> TestTree
testAddTagsToResource = req
    "AddTagsToResource"
    "fixture/AddTagsToResource.yaml"

testCreateReplicationSubnetGroup :: CreateReplicationSubnetGroup -> TestTree
testCreateReplicationSubnetGroup = req
    "CreateReplicationSubnetGroup"
    "fixture/CreateReplicationSubnetGroup.yaml"

testRefreshSchemas :: RefreshSchemas -> TestTree
testRefreshSchemas = req
    "RefreshSchemas"
    "fixture/RefreshSchemas.yaml"

testDescribeReplicationTasks :: DescribeReplicationTasks -> TestTree
testDescribeReplicationTasks = req
    "DescribeReplicationTasks"
    "fixture/DescribeReplicationTasks.yaml"

testDescribeOrderableReplicationInstances :: DescribeOrderableReplicationInstances -> TestTree
testDescribeOrderableReplicationInstances = req
    "DescribeOrderableReplicationInstances"
    "fixture/DescribeOrderableReplicationInstances.yaml"

testCreateReplicationTask :: CreateReplicationTask -> TestTree
testCreateReplicationTask = req
    "CreateReplicationTask"
    "fixture/CreateReplicationTask.yaml"

testDescribeEndpoints :: DescribeEndpoints -> TestTree
testDescribeEndpoints = req
    "DescribeEndpoints"
    "fixture/DescribeEndpoints.yaml"

testModifyReplicationInstance :: ModifyReplicationInstance -> TestTree
testModifyReplicationInstance = req
    "ModifyReplicationInstance"
    "fixture/ModifyReplicationInstance.yaml"

testModifyReplicationSubnetGroup :: ModifyReplicationSubnetGroup -> TestTree
testModifyReplicationSubnetGroup = req
    "ModifyReplicationSubnetGroup"
    "fixture/ModifyReplicationSubnetGroup.yaml"

testDescribeAccountAttributes :: DescribeAccountAttributes -> TestTree
testDescribeAccountAttributes = req
    "DescribeAccountAttributes"
    "fixture/DescribeAccountAttributes.yaml"

testDescribeReplicationInstances :: DescribeReplicationInstances -> TestTree
testDescribeReplicationInstances = req
    "DescribeReplicationInstances"
    "fixture/DescribeReplicationInstances.yaml"

testDescribeRefreshSchemasStatus :: DescribeRefreshSchemasStatus -> TestTree
testDescribeRefreshSchemasStatus = req
    "DescribeRefreshSchemasStatus"
    "fixture/DescribeRefreshSchemasStatus.yaml"

testStopReplicationTask :: StopReplicationTask -> TestTree
testStopReplicationTask = req
    "StopReplicationTask"
    "fixture/StopReplicationTask.yaml"

testCreateReplicationInstance :: CreateReplicationInstance -> TestTree
testCreateReplicationInstance = req
    "CreateReplicationInstance"
    "fixture/CreateReplicationInstance.yaml"

testDeleteReplicationSubnetGroup :: DeleteReplicationSubnetGroup -> TestTree
testDeleteReplicationSubnetGroup = req
    "DeleteReplicationSubnetGroup"
    "fixture/DeleteReplicationSubnetGroup.yaml"

-- Responses

testDeleteReplicationInstanceResponse :: DeleteReplicationInstanceResponse -> TestTree
testDeleteReplicationInstanceResponse = res
    "DeleteReplicationInstanceResponse"
    "fixture/DeleteReplicationInstanceResponse.proto"
    dms
    (Proxy :: Proxy DeleteReplicationInstance)

testCreateEndpointResponse :: CreateEndpointResponse -> TestTree
testCreateEndpointResponse = res
    "CreateEndpointResponse"
    "fixture/CreateEndpointResponse.proto"
    dms
    (Proxy :: Proxy CreateEndpoint)

testDescribeSchemasResponse :: DescribeSchemasResponse -> TestTree
testDescribeSchemasResponse = res
    "DescribeSchemasResponse"
    "fixture/DescribeSchemasResponse.proto"
    dms
    (Proxy :: Proxy DescribeSchemas)

testDeleteEndpointResponse :: DeleteEndpointResponse -> TestTree
testDeleteEndpointResponse = res
    "DeleteEndpointResponse"
    "fixture/DeleteEndpointResponse.proto"
    dms
    (Proxy :: Proxy DeleteEndpoint)

testListTagsForResourceResponse :: ListTagsForResourceResponse -> TestTree
testListTagsForResourceResponse = res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    dms
    (Proxy :: Proxy ListTagsForResource)

testDescribeEndpointTypesResponse :: DescribeEndpointTypesResponse -> TestTree
testDescribeEndpointTypesResponse = res
    "DescribeEndpointTypesResponse"
    "fixture/DescribeEndpointTypesResponse.proto"
    dms
    (Proxy :: Proxy DescribeEndpointTypes)

testDeleteReplicationTaskResponse :: DeleteReplicationTaskResponse -> TestTree
testDeleteReplicationTaskResponse = res
    "DeleteReplicationTaskResponse"
    "fixture/DeleteReplicationTaskResponse.proto"
    dms
    (Proxy :: Proxy DeleteReplicationTask)

testTestConnectionResponse :: TestConnectionResponse -> TestTree
testTestConnectionResponse = res
    "TestConnectionResponse"
    "fixture/TestConnectionResponse.proto"
    dms
    (Proxy :: Proxy TestConnection)

testDescribeConnectionsResponse :: DescribeConnectionsResponse -> TestTree
testDescribeConnectionsResponse = res
    "DescribeConnectionsResponse"
    "fixture/DescribeConnectionsResponse.proto"
    dms
    (Proxy :: Proxy DescribeConnections)

testRemoveTagsFromResourceResponse :: RemoveTagsFromResourceResponse -> TestTree
testRemoveTagsFromResourceResponse = res
    "RemoveTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse.proto"
    dms
    (Proxy :: Proxy RemoveTagsFromResource)

testModifyEndpointResponse :: ModifyEndpointResponse -> TestTree
testModifyEndpointResponse = res
    "ModifyEndpointResponse"
    "fixture/ModifyEndpointResponse.proto"
    dms
    (Proxy :: Proxy ModifyEndpoint)

testDescribeTableStatisticsResponse :: DescribeTableStatisticsResponse -> TestTree
testDescribeTableStatisticsResponse = res
    "DescribeTableStatisticsResponse"
    "fixture/DescribeTableStatisticsResponse.proto"
    dms
    (Proxy :: Proxy DescribeTableStatistics)

testDescribeReplicationSubnetGroupsResponse :: DescribeReplicationSubnetGroupsResponse -> TestTree
testDescribeReplicationSubnetGroupsResponse = res
    "DescribeReplicationSubnetGroupsResponse"
    "fixture/DescribeReplicationSubnetGroupsResponse.proto"
    dms
    (Proxy :: Proxy DescribeReplicationSubnetGroups)

testStartReplicationTaskResponse :: StartReplicationTaskResponse -> TestTree
testStartReplicationTaskResponse = res
    "StartReplicationTaskResponse"
    "fixture/StartReplicationTaskResponse.proto"
    dms
    (Proxy :: Proxy StartReplicationTask)

testAddTagsToResourceResponse :: AddTagsToResourceResponse -> TestTree
testAddTagsToResourceResponse = res
    "AddTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse.proto"
    dms
    (Proxy :: Proxy AddTagsToResource)

testCreateReplicationSubnetGroupResponse :: CreateReplicationSubnetGroupResponse -> TestTree
testCreateReplicationSubnetGroupResponse = res
    "CreateReplicationSubnetGroupResponse"
    "fixture/CreateReplicationSubnetGroupResponse.proto"
    dms
    (Proxy :: Proxy CreateReplicationSubnetGroup)

testRefreshSchemasResponse :: RefreshSchemasResponse -> TestTree
testRefreshSchemasResponse = res
    "RefreshSchemasResponse"
    "fixture/RefreshSchemasResponse.proto"
    dms
    (Proxy :: Proxy RefreshSchemas)

testDescribeReplicationTasksResponse :: DescribeReplicationTasksResponse -> TestTree
testDescribeReplicationTasksResponse = res
    "DescribeReplicationTasksResponse"
    "fixture/DescribeReplicationTasksResponse.proto"
    dms
    (Proxy :: Proxy DescribeReplicationTasks)

testDescribeOrderableReplicationInstancesResponse :: DescribeOrderableReplicationInstancesResponse -> TestTree
testDescribeOrderableReplicationInstancesResponse = res
    "DescribeOrderableReplicationInstancesResponse"
    "fixture/DescribeOrderableReplicationInstancesResponse.proto"
    dms
    (Proxy :: Proxy DescribeOrderableReplicationInstances)

testCreateReplicationTaskResponse :: CreateReplicationTaskResponse -> TestTree
testCreateReplicationTaskResponse = res
    "CreateReplicationTaskResponse"
    "fixture/CreateReplicationTaskResponse.proto"
    dms
    (Proxy :: Proxy CreateReplicationTask)

testDescribeEndpointsResponse :: DescribeEndpointsResponse -> TestTree
testDescribeEndpointsResponse = res
    "DescribeEndpointsResponse"
    "fixture/DescribeEndpointsResponse.proto"
    dms
    (Proxy :: Proxy DescribeEndpoints)

testModifyReplicationInstanceResponse :: ModifyReplicationInstanceResponse -> TestTree
testModifyReplicationInstanceResponse = res
    "ModifyReplicationInstanceResponse"
    "fixture/ModifyReplicationInstanceResponse.proto"
    dms
    (Proxy :: Proxy ModifyReplicationInstance)

testModifyReplicationSubnetGroupResponse :: ModifyReplicationSubnetGroupResponse -> TestTree
testModifyReplicationSubnetGroupResponse = res
    "ModifyReplicationSubnetGroupResponse"
    "fixture/ModifyReplicationSubnetGroupResponse.proto"
    dms
    (Proxy :: Proxy ModifyReplicationSubnetGroup)

testDescribeAccountAttributesResponse :: DescribeAccountAttributesResponse -> TestTree
testDescribeAccountAttributesResponse = res
    "DescribeAccountAttributesResponse"
    "fixture/DescribeAccountAttributesResponse.proto"
    dms
    (Proxy :: Proxy DescribeAccountAttributes)

testDescribeReplicationInstancesResponse :: DescribeReplicationInstancesResponse -> TestTree
testDescribeReplicationInstancesResponse = res
    "DescribeReplicationInstancesResponse"
    "fixture/DescribeReplicationInstancesResponse.proto"
    dms
    (Proxy :: Proxy DescribeReplicationInstances)

testDescribeRefreshSchemasStatusResponse :: DescribeRefreshSchemasStatusResponse -> TestTree
testDescribeRefreshSchemasStatusResponse = res
    "DescribeRefreshSchemasStatusResponse"
    "fixture/DescribeRefreshSchemasStatusResponse.proto"
    dms
    (Proxy :: Proxy DescribeRefreshSchemasStatus)

testStopReplicationTaskResponse :: StopReplicationTaskResponse -> TestTree
testStopReplicationTaskResponse = res
    "StopReplicationTaskResponse"
    "fixture/StopReplicationTaskResponse.proto"
    dms
    (Proxy :: Proxy StopReplicationTask)

testCreateReplicationInstanceResponse :: CreateReplicationInstanceResponse -> TestTree
testCreateReplicationInstanceResponse = res
    "CreateReplicationInstanceResponse"
    "fixture/CreateReplicationInstanceResponse.proto"
    dms
    (Proxy :: Proxy CreateReplicationInstance)

testDeleteReplicationSubnetGroupResponse :: DeleteReplicationSubnetGroupResponse -> TestTree
testDeleteReplicationSubnetGroupResponse = res
    "DeleteReplicationSubnetGroupResponse"
    "fixture/DeleteReplicationSubnetGroupResponse.proto"
    dms
    (Proxy :: Proxy DeleteReplicationSubnetGroup)
