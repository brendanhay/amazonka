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
--         [ requestDeleteReplicationInstance $
--             deleteReplicationInstance
--
--         , requestCreateEndpoint $
--             createEndpoint
--
--         , requestDescribeSchemas $
--             describeSchemas
--
--         , requestDeleteEndpoint $
--             deleteEndpoint
--
--         , requestListTagsForResource $
--             listTagsForResource
--
--         , requestDescribeEndpointTypes $
--             describeEndpointTypes
--
--         , requestDeleteReplicationTask $
--             deleteReplicationTask
--
--         , requestTestConnection $
--             testConnection
--
--         , requestDescribeConnections $
--             describeConnections
--
--         , requestRemoveTagsFromResource $
--             removeTagsFromResource
--
--         , requestModifyEndpoint $
--             modifyEndpoint
--
--         , requestDescribeCertificates $
--             describeCertificates
--
--         , requestDescribeTableStatistics $
--             describeTableStatistics
--
--         , requestDescribeReplicationSubnetGroups $
--             describeReplicationSubnetGroups
--
--         , requestStartReplicationTask $
--             startReplicationTask
--
--         , requestAddTagsToResource $
--             addTagsToResource
--
--         , requestCreateReplicationSubnetGroup $
--             createReplicationSubnetGroup
--
--         , requestDeleteCertificate $
--             deleteCertificate
--
--         , requestRefreshSchemas $
--             refreshSchemas
--
--         , requestDescribeReplicationTasks $
--             describeReplicationTasks
--
--         , requestDescribeOrderableReplicationInstances $
--             describeOrderableReplicationInstances
--
--         , requestCreateReplicationTask $
--             createReplicationTask
--
--         , requestDescribeEndpoints $
--             describeEndpoints
--
--         , requestModifyReplicationInstance $
--             modifyReplicationInstance
--
--         , requestImportCertificate $
--             importCertificate
--
--         , requestModifyReplicationSubnetGroup $
--             modifyReplicationSubnetGroup
--
--         , requestDescribeAccountAttributes $
--             describeAccountAttributes
--
--         , requestDescribeReplicationInstances $
--             describeReplicationInstances
--
--         , requestDescribeRefreshSchemasStatus $
--             describeRefreshSchemasStatus
--
--         , requestStopReplicationTask $
--             stopReplicationTask
--
--         , requestCreateReplicationInstance $
--             createReplicationInstance
--
--         , requestDeleteReplicationSubnetGroup $
--             deleteReplicationSubnetGroup
--
--           ]

--     , testGroup "response"
--         [ responseDeleteReplicationInstance $
--             deleteReplicationInstanceResponse
--
--         , responseCreateEndpoint $
--             createEndpointResponse
--
--         , responseDescribeSchemas $
--             describeSchemasResponse
--
--         , responseDeleteEndpoint $
--             deleteEndpointResponse
--
--         , responseListTagsForResource $
--             listTagsForResourceResponse
--
--         , responseDescribeEndpointTypes $
--             describeEndpointTypesResponse
--
--         , responseDeleteReplicationTask $
--             deleteReplicationTaskResponse
--
--         , responseTestConnection $
--             testConnectionResponse
--
--         , responseDescribeConnections $
--             describeConnectionsResponse
--
--         , responseRemoveTagsFromResource $
--             removeTagsFromResourceResponse
--
--         , responseModifyEndpoint $
--             modifyEndpointResponse
--
--         , responseDescribeCertificates $
--             describeCertificatesResponse
--
--         , responseDescribeTableStatistics $
--             describeTableStatisticsResponse
--
--         , responseDescribeReplicationSubnetGroups $
--             describeReplicationSubnetGroupsResponse
--
--         , responseStartReplicationTask $
--             startReplicationTaskResponse
--
--         , responseAddTagsToResource $
--             addTagsToResourceResponse
--
--         , responseCreateReplicationSubnetGroup $
--             createReplicationSubnetGroupResponse
--
--         , responseDeleteCertificate $
--             deleteCertificateResponse
--
--         , responseRefreshSchemas $
--             refreshSchemasResponse
--
--         , responseDescribeReplicationTasks $
--             describeReplicationTasksResponse
--
--         , responseDescribeOrderableReplicationInstances $
--             describeOrderableReplicationInstancesResponse
--
--         , responseCreateReplicationTask $
--             createReplicationTaskResponse
--
--         , responseDescribeEndpoints $
--             describeEndpointsResponse
--
--         , responseModifyReplicationInstance $
--             modifyReplicationInstanceResponse
--
--         , responseImportCertificate $
--             importCertificateResponse
--
--         , responseModifyReplicationSubnetGroup $
--             modifyReplicationSubnetGroupResponse
--
--         , responseDescribeAccountAttributes $
--             describeAccountAttributesResponse
--
--         , responseDescribeReplicationInstances $
--             describeReplicationInstancesResponse
--
--         , responseDescribeRefreshSchemasStatus $
--             describeRefreshSchemasStatusResponse
--
--         , responseStopReplicationTask $
--             stopReplicationTaskResponse
--
--         , responseCreateReplicationInstance $
--             createReplicationInstanceResponse
--
--         , responseDeleteReplicationSubnetGroup $
--             deleteReplicationSubnetGroupResponse
--
--           ]
--     ]

-- Requests

requestDeleteReplicationInstance :: DeleteReplicationInstance -> TestTree
requestDeleteReplicationInstance = req
    "DeleteReplicationInstance"
    "fixture/DeleteReplicationInstance.yaml"

requestCreateEndpoint :: CreateEndpoint -> TestTree
requestCreateEndpoint = req
    "CreateEndpoint"
    "fixture/CreateEndpoint.yaml"

requestDescribeSchemas :: DescribeSchemas -> TestTree
requestDescribeSchemas = req
    "DescribeSchemas"
    "fixture/DescribeSchemas.yaml"

requestDeleteEndpoint :: DeleteEndpoint -> TestTree
requestDeleteEndpoint = req
    "DeleteEndpoint"
    "fixture/DeleteEndpoint.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource = req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestDescribeEndpointTypes :: DescribeEndpointTypes -> TestTree
requestDescribeEndpointTypes = req
    "DescribeEndpointTypes"
    "fixture/DescribeEndpointTypes.yaml"

requestDeleteReplicationTask :: DeleteReplicationTask -> TestTree
requestDeleteReplicationTask = req
    "DeleteReplicationTask"
    "fixture/DeleteReplicationTask.yaml"

requestTestConnection :: TestConnection -> TestTree
requestTestConnection = req
    "TestConnection"
    "fixture/TestConnection.yaml"

requestDescribeConnections :: DescribeConnections -> TestTree
requestDescribeConnections = req
    "DescribeConnections"
    "fixture/DescribeConnections.yaml"

requestRemoveTagsFromResource :: RemoveTagsFromResource -> TestTree
requestRemoveTagsFromResource = req
    "RemoveTagsFromResource"
    "fixture/RemoveTagsFromResource.yaml"

requestModifyEndpoint :: ModifyEndpoint -> TestTree
requestModifyEndpoint = req
    "ModifyEndpoint"
    "fixture/ModifyEndpoint.yaml"

requestDescribeCertificates :: DescribeCertificates -> TestTree
requestDescribeCertificates = req
    "DescribeCertificates"
    "fixture/DescribeCertificates.yaml"

requestDescribeTableStatistics :: DescribeTableStatistics -> TestTree
requestDescribeTableStatistics = req
    "DescribeTableStatistics"
    "fixture/DescribeTableStatistics.yaml"

requestDescribeReplicationSubnetGroups :: DescribeReplicationSubnetGroups -> TestTree
requestDescribeReplicationSubnetGroups = req
    "DescribeReplicationSubnetGroups"
    "fixture/DescribeReplicationSubnetGroups.yaml"

requestStartReplicationTask :: StartReplicationTask -> TestTree
requestStartReplicationTask = req
    "StartReplicationTask"
    "fixture/StartReplicationTask.yaml"

requestAddTagsToResource :: AddTagsToResource -> TestTree
requestAddTagsToResource = req
    "AddTagsToResource"
    "fixture/AddTagsToResource.yaml"

requestCreateReplicationSubnetGroup :: CreateReplicationSubnetGroup -> TestTree
requestCreateReplicationSubnetGroup = req
    "CreateReplicationSubnetGroup"
    "fixture/CreateReplicationSubnetGroup.yaml"

requestDeleteCertificate :: DeleteCertificate -> TestTree
requestDeleteCertificate = req
    "DeleteCertificate"
    "fixture/DeleteCertificate.yaml"

requestRefreshSchemas :: RefreshSchemas -> TestTree
requestRefreshSchemas = req
    "RefreshSchemas"
    "fixture/RefreshSchemas.yaml"

requestDescribeReplicationTasks :: DescribeReplicationTasks -> TestTree
requestDescribeReplicationTasks = req
    "DescribeReplicationTasks"
    "fixture/DescribeReplicationTasks.yaml"

requestDescribeOrderableReplicationInstances :: DescribeOrderableReplicationInstances -> TestTree
requestDescribeOrderableReplicationInstances = req
    "DescribeOrderableReplicationInstances"
    "fixture/DescribeOrderableReplicationInstances.yaml"

requestCreateReplicationTask :: CreateReplicationTask -> TestTree
requestCreateReplicationTask = req
    "CreateReplicationTask"
    "fixture/CreateReplicationTask.yaml"

requestDescribeEndpoints :: DescribeEndpoints -> TestTree
requestDescribeEndpoints = req
    "DescribeEndpoints"
    "fixture/DescribeEndpoints.yaml"

requestModifyReplicationInstance :: ModifyReplicationInstance -> TestTree
requestModifyReplicationInstance = req
    "ModifyReplicationInstance"
    "fixture/ModifyReplicationInstance.yaml"

requestImportCertificate :: ImportCertificate -> TestTree
requestImportCertificate = req
    "ImportCertificate"
    "fixture/ImportCertificate.yaml"

requestModifyReplicationSubnetGroup :: ModifyReplicationSubnetGroup -> TestTree
requestModifyReplicationSubnetGroup = req
    "ModifyReplicationSubnetGroup"
    "fixture/ModifyReplicationSubnetGroup.yaml"

requestDescribeAccountAttributes :: DescribeAccountAttributes -> TestTree
requestDescribeAccountAttributes = req
    "DescribeAccountAttributes"
    "fixture/DescribeAccountAttributes.yaml"

requestDescribeReplicationInstances :: DescribeReplicationInstances -> TestTree
requestDescribeReplicationInstances = req
    "DescribeReplicationInstances"
    "fixture/DescribeReplicationInstances.yaml"

requestDescribeRefreshSchemasStatus :: DescribeRefreshSchemasStatus -> TestTree
requestDescribeRefreshSchemasStatus = req
    "DescribeRefreshSchemasStatus"
    "fixture/DescribeRefreshSchemasStatus.yaml"

requestStopReplicationTask :: StopReplicationTask -> TestTree
requestStopReplicationTask = req
    "StopReplicationTask"
    "fixture/StopReplicationTask.yaml"

requestCreateReplicationInstance :: CreateReplicationInstance -> TestTree
requestCreateReplicationInstance = req
    "CreateReplicationInstance"
    "fixture/CreateReplicationInstance.yaml"

requestDeleteReplicationSubnetGroup :: DeleteReplicationSubnetGroup -> TestTree
requestDeleteReplicationSubnetGroup = req
    "DeleteReplicationSubnetGroup"
    "fixture/DeleteReplicationSubnetGroup.yaml"

-- Responses

responseDeleteReplicationInstance :: DeleteReplicationInstanceResponse -> TestTree
responseDeleteReplicationInstance = res
    "DeleteReplicationInstanceResponse"
    "fixture/DeleteReplicationInstanceResponse.proto"
    dms
    (Proxy :: Proxy DeleteReplicationInstance)

responseCreateEndpoint :: CreateEndpointResponse -> TestTree
responseCreateEndpoint = res
    "CreateEndpointResponse"
    "fixture/CreateEndpointResponse.proto"
    dms
    (Proxy :: Proxy CreateEndpoint)

responseDescribeSchemas :: DescribeSchemasResponse -> TestTree
responseDescribeSchemas = res
    "DescribeSchemasResponse"
    "fixture/DescribeSchemasResponse.proto"
    dms
    (Proxy :: Proxy DescribeSchemas)

responseDeleteEndpoint :: DeleteEndpointResponse -> TestTree
responseDeleteEndpoint = res
    "DeleteEndpointResponse"
    "fixture/DeleteEndpointResponse.proto"
    dms
    (Proxy :: Proxy DeleteEndpoint)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource = res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    dms
    (Proxy :: Proxy ListTagsForResource)

responseDescribeEndpointTypes :: DescribeEndpointTypesResponse -> TestTree
responseDescribeEndpointTypes = res
    "DescribeEndpointTypesResponse"
    "fixture/DescribeEndpointTypesResponse.proto"
    dms
    (Proxy :: Proxy DescribeEndpointTypes)

responseDeleteReplicationTask :: DeleteReplicationTaskResponse -> TestTree
responseDeleteReplicationTask = res
    "DeleteReplicationTaskResponse"
    "fixture/DeleteReplicationTaskResponse.proto"
    dms
    (Proxy :: Proxy DeleteReplicationTask)

responseTestConnection :: TestConnectionResponse -> TestTree
responseTestConnection = res
    "TestConnectionResponse"
    "fixture/TestConnectionResponse.proto"
    dms
    (Proxy :: Proxy TestConnection)

responseDescribeConnections :: DescribeConnectionsResponse -> TestTree
responseDescribeConnections = res
    "DescribeConnectionsResponse"
    "fixture/DescribeConnectionsResponse.proto"
    dms
    (Proxy :: Proxy DescribeConnections)

responseRemoveTagsFromResource :: RemoveTagsFromResourceResponse -> TestTree
responseRemoveTagsFromResource = res
    "RemoveTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse.proto"
    dms
    (Proxy :: Proxy RemoveTagsFromResource)

responseModifyEndpoint :: ModifyEndpointResponse -> TestTree
responseModifyEndpoint = res
    "ModifyEndpointResponse"
    "fixture/ModifyEndpointResponse.proto"
    dms
    (Proxy :: Proxy ModifyEndpoint)

responseDescribeCertificates :: DescribeCertificatesResponse -> TestTree
responseDescribeCertificates = res
    "DescribeCertificatesResponse"
    "fixture/DescribeCertificatesResponse.proto"
    dms
    (Proxy :: Proxy DescribeCertificates)

responseDescribeTableStatistics :: DescribeTableStatisticsResponse -> TestTree
responseDescribeTableStatistics = res
    "DescribeTableStatisticsResponse"
    "fixture/DescribeTableStatisticsResponse.proto"
    dms
    (Proxy :: Proxy DescribeTableStatistics)

responseDescribeReplicationSubnetGroups :: DescribeReplicationSubnetGroupsResponse -> TestTree
responseDescribeReplicationSubnetGroups = res
    "DescribeReplicationSubnetGroupsResponse"
    "fixture/DescribeReplicationSubnetGroupsResponse.proto"
    dms
    (Proxy :: Proxy DescribeReplicationSubnetGroups)

responseStartReplicationTask :: StartReplicationTaskResponse -> TestTree
responseStartReplicationTask = res
    "StartReplicationTaskResponse"
    "fixture/StartReplicationTaskResponse.proto"
    dms
    (Proxy :: Proxy StartReplicationTask)

responseAddTagsToResource :: AddTagsToResourceResponse -> TestTree
responseAddTagsToResource = res
    "AddTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse.proto"
    dms
    (Proxy :: Proxy AddTagsToResource)

responseCreateReplicationSubnetGroup :: CreateReplicationSubnetGroupResponse -> TestTree
responseCreateReplicationSubnetGroup = res
    "CreateReplicationSubnetGroupResponse"
    "fixture/CreateReplicationSubnetGroupResponse.proto"
    dms
    (Proxy :: Proxy CreateReplicationSubnetGroup)

responseDeleteCertificate :: DeleteCertificateResponse -> TestTree
responseDeleteCertificate = res
    "DeleteCertificateResponse"
    "fixture/DeleteCertificateResponse.proto"
    dms
    (Proxy :: Proxy DeleteCertificate)

responseRefreshSchemas :: RefreshSchemasResponse -> TestTree
responseRefreshSchemas = res
    "RefreshSchemasResponse"
    "fixture/RefreshSchemasResponse.proto"
    dms
    (Proxy :: Proxy RefreshSchemas)

responseDescribeReplicationTasks :: DescribeReplicationTasksResponse -> TestTree
responseDescribeReplicationTasks = res
    "DescribeReplicationTasksResponse"
    "fixture/DescribeReplicationTasksResponse.proto"
    dms
    (Proxy :: Proxy DescribeReplicationTasks)

responseDescribeOrderableReplicationInstances :: DescribeOrderableReplicationInstancesResponse -> TestTree
responseDescribeOrderableReplicationInstances = res
    "DescribeOrderableReplicationInstancesResponse"
    "fixture/DescribeOrderableReplicationInstancesResponse.proto"
    dms
    (Proxy :: Proxy DescribeOrderableReplicationInstances)

responseCreateReplicationTask :: CreateReplicationTaskResponse -> TestTree
responseCreateReplicationTask = res
    "CreateReplicationTaskResponse"
    "fixture/CreateReplicationTaskResponse.proto"
    dms
    (Proxy :: Proxy CreateReplicationTask)

responseDescribeEndpoints :: DescribeEndpointsResponse -> TestTree
responseDescribeEndpoints = res
    "DescribeEndpointsResponse"
    "fixture/DescribeEndpointsResponse.proto"
    dms
    (Proxy :: Proxy DescribeEndpoints)

responseModifyReplicationInstance :: ModifyReplicationInstanceResponse -> TestTree
responseModifyReplicationInstance = res
    "ModifyReplicationInstanceResponse"
    "fixture/ModifyReplicationInstanceResponse.proto"
    dms
    (Proxy :: Proxy ModifyReplicationInstance)

responseImportCertificate :: ImportCertificateResponse -> TestTree
responseImportCertificate = res
    "ImportCertificateResponse"
    "fixture/ImportCertificateResponse.proto"
    dms
    (Proxy :: Proxy ImportCertificate)

responseModifyReplicationSubnetGroup :: ModifyReplicationSubnetGroupResponse -> TestTree
responseModifyReplicationSubnetGroup = res
    "ModifyReplicationSubnetGroupResponse"
    "fixture/ModifyReplicationSubnetGroupResponse.proto"
    dms
    (Proxy :: Proxy ModifyReplicationSubnetGroup)

responseDescribeAccountAttributes :: DescribeAccountAttributesResponse -> TestTree
responseDescribeAccountAttributes = res
    "DescribeAccountAttributesResponse"
    "fixture/DescribeAccountAttributesResponse.proto"
    dms
    (Proxy :: Proxy DescribeAccountAttributes)

responseDescribeReplicationInstances :: DescribeReplicationInstancesResponse -> TestTree
responseDescribeReplicationInstances = res
    "DescribeReplicationInstancesResponse"
    "fixture/DescribeReplicationInstancesResponse.proto"
    dms
    (Proxy :: Proxy DescribeReplicationInstances)

responseDescribeRefreshSchemasStatus :: DescribeRefreshSchemasStatusResponse -> TestTree
responseDescribeRefreshSchemasStatus = res
    "DescribeRefreshSchemasStatusResponse"
    "fixture/DescribeRefreshSchemasStatusResponse.proto"
    dms
    (Proxy :: Proxy DescribeRefreshSchemasStatus)

responseStopReplicationTask :: StopReplicationTaskResponse -> TestTree
responseStopReplicationTask = res
    "StopReplicationTaskResponse"
    "fixture/StopReplicationTaskResponse.proto"
    dms
    (Proxy :: Proxy StopReplicationTask)

responseCreateReplicationInstance :: CreateReplicationInstanceResponse -> TestTree
responseCreateReplicationInstance = res
    "CreateReplicationInstanceResponse"
    "fixture/CreateReplicationInstanceResponse.proto"
    dms
    (Proxy :: Proxy CreateReplicationInstance)

responseDeleteReplicationSubnetGroup :: DeleteReplicationSubnetGroupResponse -> TestTree
responseDeleteReplicationSubnetGroup = res
    "DeleteReplicationSubnetGroupResponse"
    "fixture/DeleteReplicationSubnetGroupResponse.proto"
    dms
    (Proxy :: Proxy DeleteReplicationSubnetGroup)
