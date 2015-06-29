-- Module      : Test.AWS.Gen.ElastiCache
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Test.AWS.Gen.ElastiCache where

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.ElastiCache

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ addTagsToResourceTest $
--             addTagsToResource
--
--         , authorizeCacheSecurityGroupIngressTest $
--             authorizeCacheSecurityGroupIngress
--
--         , copySnapshotTest $
--             copySnapshot
--
--         , createCacheClusterTest $
--             createCacheCluster
--
--         , createCacheParameterGroupTest $
--             createCacheParameterGroup
--
--         , createCacheSecurityGroupTest $
--             createCacheSecurityGroup
--
--         , createCacheSubnetGroupTest $
--             createCacheSubnetGroup
--
--         , createReplicationGroupTest $
--             createReplicationGroup
--
--         , createSnapshotTest $
--             createSnapshot
--
--         , deleteCacheClusterTest $
--             deleteCacheCluster
--
--         , deleteCacheParameterGroupTest $
--             deleteCacheParameterGroup
--
--         , deleteCacheSecurityGroupTest $
--             deleteCacheSecurityGroup
--
--         , deleteCacheSubnetGroupTest $
--             deleteCacheSubnetGroup
--
--         , deleteReplicationGroupTest $
--             deleteReplicationGroup
--
--         , deleteSnapshotTest $
--             deleteSnapshot
--
--         , describeCacheClustersTest $
--             describeCacheClusters
--
--         , describeCacheEngineVersionsTest $
--             describeCacheEngineVersions
--
--         , describeCacheParameterGroupsTest $
--             describeCacheParameterGroups
--
--         , describeCacheParametersTest $
--             describeCacheParameters
--
--         , describeCacheSecurityGroupsTest $
--             describeCacheSecurityGroups
--
--         , describeCacheSubnetGroupsTest $
--             describeCacheSubnetGroups
--
--         , describeEngineDefaultParametersTest $
--             describeEngineDefaultParameters
--
--         , describeEventsTest $
--             describeEvents
--
--         , describeReplicationGroupsTest $
--             describeReplicationGroups
--
--         , describeReservedCacheNodesTest $
--             describeReservedCacheNodes
--
--         , describeReservedCacheNodesOfferingsTest $
--             describeReservedCacheNodesOfferings
--
--         , describeSnapshotsTest $
--             describeSnapshots
--
--         , listTagsForResourceTest $
--             listTagsForResource
--
--         , modifyCacheClusterTest $
--             modifyCacheCluster
--
--         , modifyCacheParameterGroupTest $
--             modifyCacheParameterGroup
--
--         , modifyCacheSubnetGroupTest $
--             modifyCacheSubnetGroup
--
--         , modifyReplicationGroupTest $
--             modifyReplicationGroup
--
--         , purchaseReservedCacheNodesOfferingTest $
--             purchaseReservedCacheNodesOffering
--
--         , rebootCacheClusterTest $
--             rebootCacheCluster
--
--         , removeTagsFromResourceTest $
--             removeTagsFromResource
--
--         , resetCacheParameterGroupTest $
--             resetCacheParameterGroup
--
--         , revokeCacheSecurityGroupIngressTest $
--             revokeCacheSecurityGroupIngress
--
--           ]

--     , testGroup "response"
--         [ addTagsToResourceResponseTest $
--             tagListMessage
--
--         , authorizeCacheSecurityGroupIngressResponseTest $
--             authorizeCacheSecurityGroupIngressResponse
--
--         , copySnapshotResponseTest $
--             copySnapshotResponse
--
--         , createCacheClusterResponseTest $
--             createCacheClusterResponse
--
--         , createCacheParameterGroupResponseTest $
--             createCacheParameterGroupResponse
--
--         , createCacheSecurityGroupResponseTest $
--             createCacheSecurityGroupResponse
--
--         , createCacheSubnetGroupResponseTest $
--             createCacheSubnetGroupResponse
--
--         , createReplicationGroupResponseTest $
--             createReplicationGroupResponse
--
--         , createSnapshotResponseTest $
--             createSnapshotResponse
--
--         , deleteCacheClusterResponseTest $
--             deleteCacheClusterResponse
--
--         , deleteCacheParameterGroupResponseTest $
--             deleteCacheParameterGroupResponse
--
--         , deleteCacheSecurityGroupResponseTest $
--             deleteCacheSecurityGroupResponse
--
--         , deleteCacheSubnetGroupResponseTest $
--             deleteCacheSubnetGroupResponse
--
--         , deleteReplicationGroupResponseTest $
--             deleteReplicationGroupResponse
--
--         , deleteSnapshotResponseTest $
--             deleteSnapshotResponse
--
--         , describeCacheClustersResponseTest $
--             describeCacheClustersResponse
--
--         , describeCacheEngineVersionsResponseTest $
--             describeCacheEngineVersionsResponse
--
--         , describeCacheParameterGroupsResponseTest $
--             describeCacheParameterGroupsResponse
--
--         , describeCacheParametersResponseTest $
--             describeCacheParametersResponse
--
--         , describeCacheSecurityGroupsResponseTest $
--             describeCacheSecurityGroupsResponse
--
--         , describeCacheSubnetGroupsResponseTest $
--             describeCacheSubnetGroupsResponse
--
--         , describeEngineDefaultParametersResponseTest $
--             describeEngineDefaultParametersResponse
--
--         , describeEventsResponseTest $
--             describeEventsResponse
--
--         , describeReplicationGroupsResponseTest $
--             describeReplicationGroupsResponse
--
--         , describeReservedCacheNodesResponseTest $
--             describeReservedCacheNodesResponse
--
--         , describeReservedCacheNodesOfferingsResponseTest $
--             describeReservedCacheNodesOfferingsResponse
--
--         , describeSnapshotsResponseTest $
--             describeSnapshotsResponse
--
--         , listTagsForResourceResponseTest $
--             tagListMessage
--
--         , modifyCacheClusterResponseTest $
--             modifyCacheClusterResponse
--
--         , modifyCacheParameterGroupResponseTest $
--             cacheParameterGroupNameMessage
--
--         , modifyCacheSubnetGroupResponseTest $
--             modifyCacheSubnetGroupResponse
--
--         , modifyReplicationGroupResponseTest $
--             modifyReplicationGroupResponse
--
--         , purchaseReservedCacheNodesOfferingResponseTest $
--             purchaseReservedCacheNodesOfferingResponse
--
--         , rebootCacheClusterResponseTest $
--             rebootCacheClusterResponse
--
--         , removeTagsFromResourceResponseTest $
--             tagListMessage
--
--         , resetCacheParameterGroupResponseTest $
--             cacheParameterGroupNameMessage
--
--         , revokeCacheSecurityGroupIngressResponseTest $
--             revokeCacheSecurityGroupIngressResponse
--
--           ]
--     ]

-- Requests

addTagsToResourceTest :: AddTagsToResource -> TestTree
addTagsToResourceTest = undefined

authorizeCacheSecurityGroupIngressTest :: AuthorizeCacheSecurityGroupIngress -> TestTree
authorizeCacheSecurityGroupIngressTest = undefined

copySnapshotTest :: CopySnapshot -> TestTree
copySnapshotTest = undefined

createCacheClusterTest :: CreateCacheCluster -> TestTree
createCacheClusterTest = undefined

createCacheParameterGroupTest :: CreateCacheParameterGroup -> TestTree
createCacheParameterGroupTest = undefined

createCacheSecurityGroupTest :: CreateCacheSecurityGroup -> TestTree
createCacheSecurityGroupTest = undefined

createCacheSubnetGroupTest :: CreateCacheSubnetGroup -> TestTree
createCacheSubnetGroupTest = undefined

createReplicationGroupTest :: CreateReplicationGroup -> TestTree
createReplicationGroupTest = undefined

createSnapshotTest :: CreateSnapshot -> TestTree
createSnapshotTest = undefined

deleteCacheClusterTest :: DeleteCacheCluster -> TestTree
deleteCacheClusterTest = undefined

deleteCacheParameterGroupTest :: DeleteCacheParameterGroup -> TestTree
deleteCacheParameterGroupTest = undefined

deleteCacheSecurityGroupTest :: DeleteCacheSecurityGroup -> TestTree
deleteCacheSecurityGroupTest = undefined

deleteCacheSubnetGroupTest :: DeleteCacheSubnetGroup -> TestTree
deleteCacheSubnetGroupTest = undefined

deleteReplicationGroupTest :: DeleteReplicationGroup -> TestTree
deleteReplicationGroupTest = undefined

deleteSnapshotTest :: DeleteSnapshot -> TestTree
deleteSnapshotTest = undefined

describeCacheClustersTest :: DescribeCacheClusters -> TestTree
describeCacheClustersTest = undefined

describeCacheEngineVersionsTest :: DescribeCacheEngineVersions -> TestTree
describeCacheEngineVersionsTest = undefined

describeCacheParameterGroupsTest :: DescribeCacheParameterGroups -> TestTree
describeCacheParameterGroupsTest = undefined

describeCacheParametersTest :: DescribeCacheParameters -> TestTree
describeCacheParametersTest = undefined

describeCacheSecurityGroupsTest :: DescribeCacheSecurityGroups -> TestTree
describeCacheSecurityGroupsTest = undefined

describeCacheSubnetGroupsTest :: DescribeCacheSubnetGroups -> TestTree
describeCacheSubnetGroupsTest = undefined

describeEngineDefaultParametersTest :: DescribeEngineDefaultParameters -> TestTree
describeEngineDefaultParametersTest = undefined

describeEventsTest :: DescribeEvents -> TestTree
describeEventsTest = undefined

describeReplicationGroupsTest :: DescribeReplicationGroups -> TestTree
describeReplicationGroupsTest = undefined

describeReservedCacheNodesTest :: DescribeReservedCacheNodes -> TestTree
describeReservedCacheNodesTest = undefined

describeReservedCacheNodesOfferingsTest :: DescribeReservedCacheNodesOfferings -> TestTree
describeReservedCacheNodesOfferingsTest = undefined

describeSnapshotsTest :: DescribeSnapshots -> TestTree
describeSnapshotsTest = undefined

listTagsForResourceTest :: ListTagsForResource -> TestTree
listTagsForResourceTest = undefined

modifyCacheClusterTest :: ModifyCacheCluster -> TestTree
modifyCacheClusterTest = undefined

modifyCacheParameterGroupTest :: ModifyCacheParameterGroup -> TestTree
modifyCacheParameterGroupTest = undefined

modifyCacheSubnetGroupTest :: ModifyCacheSubnetGroup -> TestTree
modifyCacheSubnetGroupTest = undefined

modifyReplicationGroupTest :: ModifyReplicationGroup -> TestTree
modifyReplicationGroupTest = undefined

purchaseReservedCacheNodesOfferingTest :: PurchaseReservedCacheNodesOffering -> TestTree
purchaseReservedCacheNodesOfferingTest = undefined

rebootCacheClusterTest :: RebootCacheCluster -> TestTree
rebootCacheClusterTest = undefined

removeTagsFromResourceTest :: RemoveTagsFromResource -> TestTree
removeTagsFromResourceTest = undefined

resetCacheParameterGroupTest :: ResetCacheParameterGroup -> TestTree
resetCacheParameterGroupTest = undefined

revokeCacheSecurityGroupIngressTest :: RevokeCacheSecurityGroupIngress -> TestTree
revokeCacheSecurityGroupIngressTest = undefined

-- Responses

addTagsToResourceResponseTest :: TagListMessage -> TestTree
addTagsToResourceResponseTest = resp
    "addTagsToResourceResponse"
    "fixture/TagListMessage"
    (Proxy :: Proxy AddTagsToResource)

authorizeCacheSecurityGroupIngressResponseTest :: AuthorizeCacheSecurityGroupIngressResponse -> TestTree
authorizeCacheSecurityGroupIngressResponseTest = resp
    "authorizeCacheSecurityGroupIngressResponse"
    "fixture/AuthorizeCacheSecurityGroupIngressResponse"
    (Proxy :: Proxy AuthorizeCacheSecurityGroupIngress)

copySnapshotResponseTest :: CopySnapshotResponse -> TestTree
copySnapshotResponseTest = resp
    "copySnapshotResponse"
    "fixture/CopySnapshotResponse"
    (Proxy :: Proxy CopySnapshot)

createCacheClusterResponseTest :: CreateCacheClusterResponse -> TestTree
createCacheClusterResponseTest = resp
    "createCacheClusterResponse"
    "fixture/CreateCacheClusterResponse"
    (Proxy :: Proxy CreateCacheCluster)

createCacheParameterGroupResponseTest :: CreateCacheParameterGroupResponse -> TestTree
createCacheParameterGroupResponseTest = resp
    "createCacheParameterGroupResponse"
    "fixture/CreateCacheParameterGroupResponse"
    (Proxy :: Proxy CreateCacheParameterGroup)

createCacheSecurityGroupResponseTest :: CreateCacheSecurityGroupResponse -> TestTree
createCacheSecurityGroupResponseTest = resp
    "createCacheSecurityGroupResponse"
    "fixture/CreateCacheSecurityGroupResponse"
    (Proxy :: Proxy CreateCacheSecurityGroup)

createCacheSubnetGroupResponseTest :: CreateCacheSubnetGroupResponse -> TestTree
createCacheSubnetGroupResponseTest = resp
    "createCacheSubnetGroupResponse"
    "fixture/CreateCacheSubnetGroupResponse"
    (Proxy :: Proxy CreateCacheSubnetGroup)

createReplicationGroupResponseTest :: CreateReplicationGroupResponse -> TestTree
createReplicationGroupResponseTest = resp
    "createReplicationGroupResponse"
    "fixture/CreateReplicationGroupResponse"
    (Proxy :: Proxy CreateReplicationGroup)

createSnapshotResponseTest :: CreateSnapshotResponse -> TestTree
createSnapshotResponseTest = resp
    "createSnapshotResponse"
    "fixture/CreateSnapshotResponse"
    (Proxy :: Proxy CreateSnapshot)

deleteCacheClusterResponseTest :: DeleteCacheClusterResponse -> TestTree
deleteCacheClusterResponseTest = resp
    "deleteCacheClusterResponse"
    "fixture/DeleteCacheClusterResponse"
    (Proxy :: Proxy DeleteCacheCluster)

deleteCacheParameterGroupResponseTest :: DeleteCacheParameterGroupResponse -> TestTree
deleteCacheParameterGroupResponseTest = resp
    "deleteCacheParameterGroupResponse"
    "fixture/DeleteCacheParameterGroupResponse"
    (Proxy :: Proxy DeleteCacheParameterGroup)

deleteCacheSecurityGroupResponseTest :: DeleteCacheSecurityGroupResponse -> TestTree
deleteCacheSecurityGroupResponseTest = resp
    "deleteCacheSecurityGroupResponse"
    "fixture/DeleteCacheSecurityGroupResponse"
    (Proxy :: Proxy DeleteCacheSecurityGroup)

deleteCacheSubnetGroupResponseTest :: DeleteCacheSubnetGroupResponse -> TestTree
deleteCacheSubnetGroupResponseTest = resp
    "deleteCacheSubnetGroupResponse"
    "fixture/DeleteCacheSubnetGroupResponse"
    (Proxy :: Proxy DeleteCacheSubnetGroup)

deleteReplicationGroupResponseTest :: DeleteReplicationGroupResponse -> TestTree
deleteReplicationGroupResponseTest = resp
    "deleteReplicationGroupResponse"
    "fixture/DeleteReplicationGroupResponse"
    (Proxy :: Proxy DeleteReplicationGroup)

deleteSnapshotResponseTest :: DeleteSnapshotResponse -> TestTree
deleteSnapshotResponseTest = resp
    "deleteSnapshotResponse"
    "fixture/DeleteSnapshotResponse"
    (Proxy :: Proxy DeleteSnapshot)

describeCacheClustersResponseTest :: DescribeCacheClustersResponse -> TestTree
describeCacheClustersResponseTest = resp
    "describeCacheClustersResponse"
    "fixture/DescribeCacheClustersResponse"
    (Proxy :: Proxy DescribeCacheClusters)

describeCacheEngineVersionsResponseTest :: DescribeCacheEngineVersionsResponse -> TestTree
describeCacheEngineVersionsResponseTest = resp
    "describeCacheEngineVersionsResponse"
    "fixture/DescribeCacheEngineVersionsResponse"
    (Proxy :: Proxy DescribeCacheEngineVersions)

describeCacheParameterGroupsResponseTest :: DescribeCacheParameterGroupsResponse -> TestTree
describeCacheParameterGroupsResponseTest = resp
    "describeCacheParameterGroupsResponse"
    "fixture/DescribeCacheParameterGroupsResponse"
    (Proxy :: Proxy DescribeCacheParameterGroups)

describeCacheParametersResponseTest :: DescribeCacheParametersResponse -> TestTree
describeCacheParametersResponseTest = resp
    "describeCacheParametersResponse"
    "fixture/DescribeCacheParametersResponse"
    (Proxy :: Proxy DescribeCacheParameters)

describeCacheSecurityGroupsResponseTest :: DescribeCacheSecurityGroupsResponse -> TestTree
describeCacheSecurityGroupsResponseTest = resp
    "describeCacheSecurityGroupsResponse"
    "fixture/DescribeCacheSecurityGroupsResponse"
    (Proxy :: Proxy DescribeCacheSecurityGroups)

describeCacheSubnetGroupsResponseTest :: DescribeCacheSubnetGroupsResponse -> TestTree
describeCacheSubnetGroupsResponseTest = resp
    "describeCacheSubnetGroupsResponse"
    "fixture/DescribeCacheSubnetGroupsResponse"
    (Proxy :: Proxy DescribeCacheSubnetGroups)

describeEngineDefaultParametersResponseTest :: DescribeEngineDefaultParametersResponse -> TestTree
describeEngineDefaultParametersResponseTest = resp
    "describeEngineDefaultParametersResponse"
    "fixture/DescribeEngineDefaultParametersResponse"
    (Proxy :: Proxy DescribeEngineDefaultParameters)

describeEventsResponseTest :: DescribeEventsResponse -> TestTree
describeEventsResponseTest = resp
    "describeEventsResponse"
    "fixture/DescribeEventsResponse"
    (Proxy :: Proxy DescribeEvents)

describeReplicationGroupsResponseTest :: DescribeReplicationGroupsResponse -> TestTree
describeReplicationGroupsResponseTest = resp
    "describeReplicationGroupsResponse"
    "fixture/DescribeReplicationGroupsResponse"
    (Proxy :: Proxy DescribeReplicationGroups)

describeReservedCacheNodesResponseTest :: DescribeReservedCacheNodesResponse -> TestTree
describeReservedCacheNodesResponseTest = resp
    "describeReservedCacheNodesResponse"
    "fixture/DescribeReservedCacheNodesResponse"
    (Proxy :: Proxy DescribeReservedCacheNodes)

describeReservedCacheNodesOfferingsResponseTest :: DescribeReservedCacheNodesOfferingsResponse -> TestTree
describeReservedCacheNodesOfferingsResponseTest = resp
    "describeReservedCacheNodesOfferingsResponse"
    "fixture/DescribeReservedCacheNodesOfferingsResponse"
    (Proxy :: Proxy DescribeReservedCacheNodesOfferings)

describeSnapshotsResponseTest :: DescribeSnapshotsResponse -> TestTree
describeSnapshotsResponseTest = resp
    "describeSnapshotsResponse"
    "fixture/DescribeSnapshotsResponse"
    (Proxy :: Proxy DescribeSnapshots)

listTagsForResourceResponseTest :: TagListMessage -> TestTree
listTagsForResourceResponseTest = resp
    "listTagsForResourceResponse"
    "fixture/TagListMessage"
    (Proxy :: Proxy ListTagsForResource)

modifyCacheClusterResponseTest :: ModifyCacheClusterResponse -> TestTree
modifyCacheClusterResponseTest = resp
    "modifyCacheClusterResponse"
    "fixture/ModifyCacheClusterResponse"
    (Proxy :: Proxy ModifyCacheCluster)

modifyCacheParameterGroupResponseTest :: CacheParameterGroupNameMessage -> TestTree
modifyCacheParameterGroupResponseTest = resp
    "modifyCacheParameterGroupResponse"
    "fixture/CacheParameterGroupNameMessage"
    (Proxy :: Proxy ModifyCacheParameterGroup)

modifyCacheSubnetGroupResponseTest :: ModifyCacheSubnetGroupResponse -> TestTree
modifyCacheSubnetGroupResponseTest = resp
    "modifyCacheSubnetGroupResponse"
    "fixture/ModifyCacheSubnetGroupResponse"
    (Proxy :: Proxy ModifyCacheSubnetGroup)

modifyReplicationGroupResponseTest :: ModifyReplicationGroupResponse -> TestTree
modifyReplicationGroupResponseTest = resp
    "modifyReplicationGroupResponse"
    "fixture/ModifyReplicationGroupResponse"
    (Proxy :: Proxy ModifyReplicationGroup)

purchaseReservedCacheNodesOfferingResponseTest :: PurchaseReservedCacheNodesOfferingResponse -> TestTree
purchaseReservedCacheNodesOfferingResponseTest = resp
    "purchaseReservedCacheNodesOfferingResponse"
    "fixture/PurchaseReservedCacheNodesOfferingResponse"
    (Proxy :: Proxy PurchaseReservedCacheNodesOffering)

rebootCacheClusterResponseTest :: RebootCacheClusterResponse -> TestTree
rebootCacheClusterResponseTest = resp
    "rebootCacheClusterResponse"
    "fixture/RebootCacheClusterResponse"
    (Proxy :: Proxy RebootCacheCluster)

removeTagsFromResourceResponseTest :: TagListMessage -> TestTree
removeTagsFromResourceResponseTest = resp
    "removeTagsFromResourceResponse"
    "fixture/TagListMessage"
    (Proxy :: Proxy RemoveTagsFromResource)

resetCacheParameterGroupResponseTest :: CacheParameterGroupNameMessage -> TestTree
resetCacheParameterGroupResponseTest = resp
    "resetCacheParameterGroupResponse"
    "fixture/CacheParameterGroupNameMessage"
    (Proxy :: Proxy ResetCacheParameterGroup)

revokeCacheSecurityGroupIngressResponseTest :: RevokeCacheSecurityGroupIngressResponse -> TestTree
revokeCacheSecurityGroupIngressResponseTest = resp
    "revokeCacheSecurityGroupIngressResponse"
    "fixture/RevokeCacheSecurityGroupIngressResponse"
    (Proxy :: Proxy RevokeCacheSecurityGroupIngress)
