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

import           Data.Proxy
import           Network.AWS.ElastiCache
import           Test.AWS.Fixture
import           Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ deleteCacheSecurityGroupTest $
--             deleteCacheSecurityGroup
--
--         , createReplicationGroupTest $
--             createReplicationGroup
--
--         , deleteCacheClusterTest $
--             deleteCacheCluster
--
--         , rebootCacheClusterTest $
--             rebootCacheCluster
--
--         , revokeCacheSecurityGroupIngressTest $
--             revokeCacheSecurityGroupIngress
--
--         , describeEventsTest $
--             describeEvents
--
--         , describeEngineDefaultParametersTest $
--             describeEngineDefaultParameters
--
--         , modifyCacheParameterGroupTest $
--             modifyCacheParameterGroup
--
--         , createCacheClusterTest $
--             createCacheCluster
--
--         , listTagsForResourceTest $
--             listTagsForResource
--
--         , deleteReplicationGroupTest $
--             deleteReplicationGroup
--
--         , purchaseReservedCacheNodesOfferingTest $
--             purchaseReservedCacheNodesOffering
--
--         , describeCacheClustersTest $
--             describeCacheClusters
--
--         , modifyReplicationGroupTest $
--             modifyReplicationGroup
--
--         , removeTagsFromResourceTest $
--             removeTagsFromResource
--
--         , describeCacheParametersTest $
--             describeCacheParameters
--
--         , describeCacheSubnetGroupsTest $
--             describeCacheSubnetGroups
--
--         , createCacheSecurityGroupTest $
--             createCacheSecurityGroup
--
--         , addTagsToResourceTest $
--             addTagsToResource
--
--         , authorizeCacheSecurityGroupIngressTest $
--             authorizeCacheSecurityGroupIngress
--
--         , copySnapshotTest $
--             copySnapshot
--
--         , createCacheSubnetGroupTest $
--             createCacheSubnetGroup
--
--         , describeCacheParameterGroupsTest $
--             describeCacheParameterGroups
--
--         , resetCacheParameterGroupTest $
--             resetCacheParameterGroup
--
--         , describeSnapshotsTest $
--             describeSnapshots
--
--         , describeReservedCacheNodesOfferingsTest $
--             describeReservedCacheNodesOfferings
--
--         , deleteSnapshotTest $
--             deleteSnapshot
--
--         , describeReplicationGroupsTest $
--             describeReplicationGroups
--
--         , modifyCacheSubnetGroupTest $
--             modifyCacheSubnetGroup
--
--         , createSnapshotTest $
--             createSnapshot
--
--         , describeCacheSecurityGroupsTest $
--             describeCacheSecurityGroups
--
--         , deleteCacheParameterGroupTest $
--             deleteCacheParameterGroup
--
--         , describeReservedCacheNodesTest $
--             describeReservedCacheNodes
--
--         , describeCacheEngineVersionsTest $
--             describeCacheEngineVersions
--
--         , modifyCacheClusterTest $
--             modifyCacheCluster
--
--         , createCacheParameterGroupTest $
--             createCacheParameterGroup
--
--         , deleteCacheSubnetGroupTest $
--             deleteCacheSubnetGroup
--
--           ]

--     , testGroup "response"
--         [ deleteCacheSecurityGroupResponseTest $
--             deleteCacheSecurityGroupResponse
--
--         , createReplicationGroupResponseTest $
--             createReplicationGroupResponse
--
--         , deleteCacheClusterResponseTest $
--             deleteCacheClusterResponse
--
--         , rebootCacheClusterResponseTest $
--             rebootCacheClusterResponse
--
--         , revokeCacheSecurityGroupIngressResponseTest $
--             revokeCacheSecurityGroupIngressResponse
--
--         , describeEventsResponseTest $
--             describeEventsResponse
--
--         , describeEngineDefaultParametersResponseTest $
--             describeEngineDefaultParametersResponse
--
--         , cacheParameterGroupNameMessageTest $
--             cacheParameterGroupNameMessage
--
--         , createCacheClusterResponseTest $
--             createCacheClusterResponse
--
--         , tagListMessageTest $
--             tagListMessage
--
--         , deleteReplicationGroupResponseTest $
--             deleteReplicationGroupResponse
--
--         , purchaseReservedCacheNodesOfferingResponseTest $
--             purchaseReservedCacheNodesOfferingResponse
--
--         , describeCacheClustersResponseTest $
--             describeCacheClustersResponse
--
--         , modifyReplicationGroupResponseTest $
--             modifyReplicationGroupResponse
--
--         , tagListMessageTest $
--             tagListMessage
--
--         , describeCacheParametersResponseTest $
--             describeCacheParametersResponse
--
--         , describeCacheSubnetGroupsResponseTest $
--             describeCacheSubnetGroupsResponse
--
--         , createCacheSecurityGroupResponseTest $
--             createCacheSecurityGroupResponse
--
--         , tagListMessageTest $
--             tagListMessage
--
--         , authorizeCacheSecurityGroupIngressResponseTest $
--             authorizeCacheSecurityGroupIngressResponse
--
--         , copySnapshotResponseTest $
--             copySnapshotResponse
--
--         , createCacheSubnetGroupResponseTest $
--             createCacheSubnetGroupResponse
--
--         , describeCacheParameterGroupsResponseTest $
--             describeCacheParameterGroupsResponse
--
--         , cacheParameterGroupNameMessageTest $
--             cacheParameterGroupNameMessage
--
--         , describeSnapshotsResponseTest $
--             describeSnapshotsResponse
--
--         , describeReservedCacheNodesOfferingsResponseTest $
--             describeReservedCacheNodesOfferingsResponse
--
--         , deleteSnapshotResponseTest $
--             deleteSnapshotResponse
--
--         , describeReplicationGroupsResponseTest $
--             describeReplicationGroupsResponse
--
--         , modifyCacheSubnetGroupResponseTest $
--             modifyCacheSubnetGroupResponse
--
--         , createSnapshotResponseTest $
--             createSnapshotResponse
--
--         , describeCacheSecurityGroupsResponseTest $
--             describeCacheSecurityGroupsResponse
--
--         , deleteCacheParameterGroupResponseTest $
--             deleteCacheParameterGroupResponse
--
--         , describeReservedCacheNodesResponseTest $
--             describeReservedCacheNodesResponse
--
--         , describeCacheEngineVersionsResponseTest $
--             describeCacheEngineVersionsResponse
--
--         , modifyCacheClusterResponseTest $
--             modifyCacheClusterResponse
--
--         , createCacheParameterGroupResponseTest $
--             createCacheParameterGroupResponse
--
--         , deleteCacheSubnetGroupResponseTest $
--             deleteCacheSubnetGroupResponse
--
--           ]
--     ]

-- Requests

deleteCacheSecurityGroupTest :: DeleteCacheSecurityGroup -> TestTree
deleteCacheSecurityGroupTest = undefined

createReplicationGroupTest :: CreateReplicationGroup -> TestTree
createReplicationGroupTest = undefined

deleteCacheClusterTest :: DeleteCacheCluster -> TestTree
deleteCacheClusterTest = undefined

rebootCacheClusterTest :: RebootCacheCluster -> TestTree
rebootCacheClusterTest = undefined

revokeCacheSecurityGroupIngressTest :: RevokeCacheSecurityGroupIngress -> TestTree
revokeCacheSecurityGroupIngressTest = undefined

describeEventsTest :: DescribeEvents -> TestTree
describeEventsTest = undefined

describeEngineDefaultParametersTest :: DescribeEngineDefaultParameters -> TestTree
describeEngineDefaultParametersTest = undefined

modifyCacheParameterGroupTest :: ModifyCacheParameterGroup -> TestTree
modifyCacheParameterGroupTest = undefined

createCacheClusterTest :: CreateCacheCluster -> TestTree
createCacheClusterTest = undefined

listTagsForResourceTest :: ListTagsForResource -> TestTree
listTagsForResourceTest = undefined

deleteReplicationGroupTest :: DeleteReplicationGroup -> TestTree
deleteReplicationGroupTest = undefined

purchaseReservedCacheNodesOfferingTest :: PurchaseReservedCacheNodesOffering -> TestTree
purchaseReservedCacheNodesOfferingTest = undefined

describeCacheClustersTest :: DescribeCacheClusters -> TestTree
describeCacheClustersTest = undefined

modifyReplicationGroupTest :: ModifyReplicationGroup -> TestTree
modifyReplicationGroupTest = undefined

removeTagsFromResourceTest :: RemoveTagsFromResource -> TestTree
removeTagsFromResourceTest = undefined

describeCacheParametersTest :: DescribeCacheParameters -> TestTree
describeCacheParametersTest = undefined

describeCacheSubnetGroupsTest :: DescribeCacheSubnetGroups -> TestTree
describeCacheSubnetGroupsTest = undefined

createCacheSecurityGroupTest :: CreateCacheSecurityGroup -> TestTree
createCacheSecurityGroupTest = undefined

addTagsToResourceTest :: AddTagsToResource -> TestTree
addTagsToResourceTest = undefined

authorizeCacheSecurityGroupIngressTest :: AuthorizeCacheSecurityGroupIngress -> TestTree
authorizeCacheSecurityGroupIngressTest = undefined

copySnapshotTest :: CopySnapshot -> TestTree
copySnapshotTest = undefined

createCacheSubnetGroupTest :: CreateCacheSubnetGroup -> TestTree
createCacheSubnetGroupTest = undefined

describeCacheParameterGroupsTest :: DescribeCacheParameterGroups -> TestTree
describeCacheParameterGroupsTest = undefined

resetCacheParameterGroupTest :: ResetCacheParameterGroup -> TestTree
resetCacheParameterGroupTest = undefined

describeSnapshotsTest :: DescribeSnapshots -> TestTree
describeSnapshotsTest = undefined

describeReservedCacheNodesOfferingsTest :: DescribeReservedCacheNodesOfferings -> TestTree
describeReservedCacheNodesOfferingsTest = undefined

deleteSnapshotTest :: DeleteSnapshot -> TestTree
deleteSnapshotTest = undefined

describeReplicationGroupsTest :: DescribeReplicationGroups -> TestTree
describeReplicationGroupsTest = undefined

modifyCacheSubnetGroupTest :: ModifyCacheSubnetGroup -> TestTree
modifyCacheSubnetGroupTest = undefined

createSnapshotTest :: CreateSnapshot -> TestTree
createSnapshotTest = undefined

describeCacheSecurityGroupsTest :: DescribeCacheSecurityGroups -> TestTree
describeCacheSecurityGroupsTest = undefined

deleteCacheParameterGroupTest :: DeleteCacheParameterGroup -> TestTree
deleteCacheParameterGroupTest = undefined

describeReservedCacheNodesTest :: DescribeReservedCacheNodes -> TestTree
describeReservedCacheNodesTest = undefined

describeCacheEngineVersionsTest :: DescribeCacheEngineVersions -> TestTree
describeCacheEngineVersionsTest = undefined

modifyCacheClusterTest :: ModifyCacheCluster -> TestTree
modifyCacheClusterTest = undefined

createCacheParameterGroupTest :: CreateCacheParameterGroup -> TestTree
createCacheParameterGroupTest = undefined

deleteCacheSubnetGroupTest :: DeleteCacheSubnetGroup -> TestTree
deleteCacheSubnetGroupTest = undefined

-- Responses

deleteCacheSecurityGroupResponseTest :: DeleteCacheSecurityGroupResponse -> TestTree
deleteCacheSecurityGroupResponseTest = resp
    "DeleteCacheSecurityGroup"
    "fixture/ElastiCache/DeleteCacheSecurityGroupResponse"
    (Proxy :: Proxy DeleteCacheSecurityGroup)

createReplicationGroupResponseTest :: CreateReplicationGroupResponse -> TestTree
createReplicationGroupResponseTest = resp
    "CreateReplicationGroup"
    "fixture/ElastiCache/CreateReplicationGroupResponse"
    (Proxy :: Proxy CreateReplicationGroup)

deleteCacheClusterResponseTest :: DeleteCacheClusterResponse -> TestTree
deleteCacheClusterResponseTest = resp
    "DeleteCacheCluster"
    "fixture/ElastiCache/DeleteCacheClusterResponse"
    (Proxy :: Proxy DeleteCacheCluster)

rebootCacheClusterResponseTest :: RebootCacheClusterResponse -> TestTree
rebootCacheClusterResponseTest = resp
    "RebootCacheCluster"
    "fixture/ElastiCache/RebootCacheClusterResponse"
    (Proxy :: Proxy RebootCacheCluster)

revokeCacheSecurityGroupIngressResponseTest :: RevokeCacheSecurityGroupIngressResponse -> TestTree
revokeCacheSecurityGroupIngressResponseTest = resp
    "RevokeCacheSecurityGroupIngress"
    "fixture/ElastiCache/RevokeCacheSecurityGroupIngressResponse"
    (Proxy :: Proxy RevokeCacheSecurityGroupIngress)

describeEventsResponseTest :: DescribeEventsResponse -> TestTree
describeEventsResponseTest = resp
    "DescribeEvents"
    "fixture/ElastiCache/DescribeEventsResponse"
    (Proxy :: Proxy DescribeEvents)

describeEngineDefaultParametersResponseTest :: DescribeEngineDefaultParametersResponse -> TestTree
describeEngineDefaultParametersResponseTest = resp
    "DescribeEngineDefaultParameters"
    "fixture/ElastiCache/DescribeEngineDefaultParametersResponse"
    (Proxy :: Proxy DescribeEngineDefaultParameters)

cacheParameterGroupNameMessageTest :: CacheParameterGroupNameMessage -> TestTree
cacheParameterGroupNameMessageTest = resp
    "ModifyCacheParameterGroup"
    "fixture/ElastiCache/CacheParameterGroupNameMessage"
    (Proxy :: Proxy ModifyCacheParameterGroup)

createCacheClusterResponseTest :: CreateCacheClusterResponse -> TestTree
createCacheClusterResponseTest = resp
    "CreateCacheCluster"
    "fixture/ElastiCache/CreateCacheClusterResponse"
    (Proxy :: Proxy CreateCacheCluster)

tagListMessageTest :: TagListMessage -> TestTree
tagListMessageTest = resp
    "ListTagsForResource"
    "fixture/ElastiCache/TagListMessage"
    (Proxy :: Proxy ListTagsForResource)

deleteReplicationGroupResponseTest :: DeleteReplicationGroupResponse -> TestTree
deleteReplicationGroupResponseTest = resp
    "DeleteReplicationGroup"
    "fixture/ElastiCache/DeleteReplicationGroupResponse"
    (Proxy :: Proxy DeleteReplicationGroup)

purchaseReservedCacheNodesOfferingResponseTest :: PurchaseReservedCacheNodesOfferingResponse -> TestTree
purchaseReservedCacheNodesOfferingResponseTest = resp
    "PurchaseReservedCacheNodesOffering"
    "fixture/ElastiCache/PurchaseReservedCacheNodesOfferingResponse"
    (Proxy :: Proxy PurchaseReservedCacheNodesOffering)

describeCacheClustersResponseTest :: DescribeCacheClustersResponse -> TestTree
describeCacheClustersResponseTest = resp
    "DescribeCacheClusters"
    "fixture/ElastiCache/DescribeCacheClustersResponse"
    (Proxy :: Proxy DescribeCacheClusters)

modifyReplicationGroupResponseTest :: ModifyReplicationGroupResponse -> TestTree
modifyReplicationGroupResponseTest = resp
    "ModifyReplicationGroup"
    "fixture/ElastiCache/ModifyReplicationGroupResponse"
    (Proxy :: Proxy ModifyReplicationGroup)

tagListMessageTest :: TagListMessage -> TestTree
tagListMessageTest = resp
    "RemoveTagsFromResource"
    "fixture/ElastiCache/TagListMessage"
    (Proxy :: Proxy RemoveTagsFromResource)

describeCacheParametersResponseTest :: DescribeCacheParametersResponse -> TestTree
describeCacheParametersResponseTest = resp
    "DescribeCacheParameters"
    "fixture/ElastiCache/DescribeCacheParametersResponse"
    (Proxy :: Proxy DescribeCacheParameters)

describeCacheSubnetGroupsResponseTest :: DescribeCacheSubnetGroupsResponse -> TestTree
describeCacheSubnetGroupsResponseTest = resp
    "DescribeCacheSubnetGroups"
    "fixture/ElastiCache/DescribeCacheSubnetGroupsResponse"
    (Proxy :: Proxy DescribeCacheSubnetGroups)

createCacheSecurityGroupResponseTest :: CreateCacheSecurityGroupResponse -> TestTree
createCacheSecurityGroupResponseTest = resp
    "CreateCacheSecurityGroup"
    "fixture/ElastiCache/CreateCacheSecurityGroupResponse"
    (Proxy :: Proxy CreateCacheSecurityGroup)

tagListMessageTest :: TagListMessage -> TestTree
tagListMessageTest = resp
    "AddTagsToResource"
    "fixture/ElastiCache/TagListMessage"
    (Proxy :: Proxy AddTagsToResource)

authorizeCacheSecurityGroupIngressResponseTest :: AuthorizeCacheSecurityGroupIngressResponse -> TestTree
authorizeCacheSecurityGroupIngressResponseTest = resp
    "AuthorizeCacheSecurityGroupIngress"
    "fixture/ElastiCache/AuthorizeCacheSecurityGroupIngressResponse"
    (Proxy :: Proxy AuthorizeCacheSecurityGroupIngress)

copySnapshotResponseTest :: CopySnapshotResponse -> TestTree
copySnapshotResponseTest = resp
    "CopySnapshot"
    "fixture/ElastiCache/CopySnapshotResponse"
    (Proxy :: Proxy CopySnapshot)

createCacheSubnetGroupResponseTest :: CreateCacheSubnetGroupResponse -> TestTree
createCacheSubnetGroupResponseTest = resp
    "CreateCacheSubnetGroup"
    "fixture/ElastiCache/CreateCacheSubnetGroupResponse"
    (Proxy :: Proxy CreateCacheSubnetGroup)

describeCacheParameterGroupsResponseTest :: DescribeCacheParameterGroupsResponse -> TestTree
describeCacheParameterGroupsResponseTest = resp
    "DescribeCacheParameterGroups"
    "fixture/ElastiCache/DescribeCacheParameterGroupsResponse"
    (Proxy :: Proxy DescribeCacheParameterGroups)

cacheParameterGroupNameMessageTest :: CacheParameterGroupNameMessage -> TestTree
cacheParameterGroupNameMessageTest = resp
    "ResetCacheParameterGroup"
    "fixture/ElastiCache/CacheParameterGroupNameMessage"
    (Proxy :: Proxy ResetCacheParameterGroup)

describeSnapshotsResponseTest :: DescribeSnapshotsResponse -> TestTree
describeSnapshotsResponseTest = resp
    "DescribeSnapshots"
    "fixture/ElastiCache/DescribeSnapshotsResponse"
    (Proxy :: Proxy DescribeSnapshots)

describeReservedCacheNodesOfferingsResponseTest :: DescribeReservedCacheNodesOfferingsResponse -> TestTree
describeReservedCacheNodesOfferingsResponseTest = resp
    "DescribeReservedCacheNodesOfferings"
    "fixture/ElastiCache/DescribeReservedCacheNodesOfferingsResponse"
    (Proxy :: Proxy DescribeReservedCacheNodesOfferings)

deleteSnapshotResponseTest :: DeleteSnapshotResponse -> TestTree
deleteSnapshotResponseTest = resp
    "DeleteSnapshot"
    "fixture/ElastiCache/DeleteSnapshotResponse"
    (Proxy :: Proxy DeleteSnapshot)

describeReplicationGroupsResponseTest :: DescribeReplicationGroupsResponse -> TestTree
describeReplicationGroupsResponseTest = resp
    "DescribeReplicationGroups"
    "fixture/ElastiCache/DescribeReplicationGroupsResponse"
    (Proxy :: Proxy DescribeReplicationGroups)

modifyCacheSubnetGroupResponseTest :: ModifyCacheSubnetGroupResponse -> TestTree
modifyCacheSubnetGroupResponseTest = resp
    "ModifyCacheSubnetGroup"
    "fixture/ElastiCache/ModifyCacheSubnetGroupResponse"
    (Proxy :: Proxy ModifyCacheSubnetGroup)

createSnapshotResponseTest :: CreateSnapshotResponse -> TestTree
createSnapshotResponseTest = resp
    "CreateSnapshot"
    "fixture/ElastiCache/CreateSnapshotResponse"
    (Proxy :: Proxy CreateSnapshot)

describeCacheSecurityGroupsResponseTest :: DescribeCacheSecurityGroupsResponse -> TestTree
describeCacheSecurityGroupsResponseTest = resp
    "DescribeCacheSecurityGroups"
    "fixture/ElastiCache/DescribeCacheSecurityGroupsResponse"
    (Proxy :: Proxy DescribeCacheSecurityGroups)

deleteCacheParameterGroupResponseTest :: DeleteCacheParameterGroupResponse -> TestTree
deleteCacheParameterGroupResponseTest = resp
    "DeleteCacheParameterGroup"
    "fixture/ElastiCache/DeleteCacheParameterGroupResponse"
    (Proxy :: Proxy DeleteCacheParameterGroup)

describeReservedCacheNodesResponseTest :: DescribeReservedCacheNodesResponse -> TestTree
describeReservedCacheNodesResponseTest = resp
    "DescribeReservedCacheNodes"
    "fixture/ElastiCache/DescribeReservedCacheNodesResponse"
    (Proxy :: Proxy DescribeReservedCacheNodes)

describeCacheEngineVersionsResponseTest :: DescribeCacheEngineVersionsResponse -> TestTree
describeCacheEngineVersionsResponseTest = resp
    "DescribeCacheEngineVersions"
    "fixture/ElastiCache/DescribeCacheEngineVersionsResponse"
    (Proxy :: Proxy DescribeCacheEngineVersions)

modifyCacheClusterResponseTest :: ModifyCacheClusterResponse -> TestTree
modifyCacheClusterResponseTest = resp
    "ModifyCacheCluster"
    "fixture/ElastiCache/ModifyCacheClusterResponse"
    (Proxy :: Proxy ModifyCacheCluster)

createCacheParameterGroupResponseTest :: CreateCacheParameterGroupResponse -> TestTree
createCacheParameterGroupResponseTest = resp
    "CreateCacheParameterGroup"
    "fixture/ElastiCache/CreateCacheParameterGroupResponse"
    (Proxy :: Proxy CreateCacheParameterGroup)

deleteCacheSubnetGroupResponseTest :: DeleteCacheSubnetGroupResponse -> TestTree
deleteCacheSubnetGroupResponseTest = resp
    "DeleteCacheSubnetGroup"
    "fixture/ElastiCache/DeleteCacheSubnetGroupResponse"
    (Proxy :: Proxy DeleteCacheSubnetGroup)
