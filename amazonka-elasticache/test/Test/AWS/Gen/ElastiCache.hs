{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ElastiCache
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.ElastiCache where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.ElastiCache
import Test.AWS.ElastiCache.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testDeleteCacheSecurityGroup $
--             deleteCacheSecurityGroup
--
--         , testCreateReplicationGroup $
--             createReplicationGroup
--
--         , testDeleteCacheCluster $
--             deleteCacheCluster
--
--         , testRebootCacheCluster $
--             rebootCacheCluster
--
--         , testRevokeCacheSecurityGroupIngress $
--             revokeCacheSecurityGroupIngress
--
--         , testDescribeEvents $
--             describeEvents
--
--         , testDescribeEngineDefaultParameters $
--             describeEngineDefaultParameters
--
--         , testModifyCacheParameterGroup $
--             modifyCacheParameterGroup
--
--         , testCreateCacheCluster $
--             createCacheCluster
--
--         , testListTagsForResource $
--             listTagsForResource
--
--         , testDeleteReplicationGroup $
--             deleteReplicationGroup
--
--         , testPurchaseReservedCacheNodesOffering $
--             purchaseReservedCacheNodesOffering
--
--         , testDescribeCacheClusters $
--             describeCacheClusters
--
--         , testModifyReplicationGroup $
--             modifyReplicationGroup
--
--         , testRemoveTagsFromResource $
--             removeTagsFromResource
--
--         , testDescribeCacheParameters $
--             describeCacheParameters
--
--         , testDescribeCacheSubnetGroups $
--             describeCacheSubnetGroups
--
--         , testCreateCacheSecurityGroup $
--             createCacheSecurityGroup
--
--         , testAddTagsToResource $
--             addTagsToResource
--
--         , testAuthorizeCacheSecurityGroupIngress $
--             authorizeCacheSecurityGroupIngress
--
--         , testCopySnapshot $
--             copySnapshot
--
--         , testCreateCacheSubnetGroup $
--             createCacheSubnetGroup
--
--         , testDescribeCacheParameterGroups $
--             describeCacheParameterGroups
--
--         , testResetCacheParameterGroup $
--             resetCacheParameterGroup
--
--         , testDescribeSnapshots $
--             describeSnapshots
--
--         , testDescribeReservedCacheNodesOfferings $
--             describeReservedCacheNodesOfferings
--
--         , testDeleteSnapshot $
--             deleteSnapshot
--
--         , testDescribeReplicationGroups $
--             describeReplicationGroups
--
--         , testModifyCacheSubnetGroup $
--             modifyCacheSubnetGroup
--
--         , testCreateSnapshot $
--             createSnapshot
--
--         , testDescribeCacheSecurityGroups $
--             describeCacheSecurityGroups
--
--         , testDeleteCacheParameterGroup $
--             deleteCacheParameterGroup
--
--         , testDescribeReservedCacheNodes $
--             describeReservedCacheNodes
--
--         , testDescribeCacheEngineVersions $
--             describeCacheEngineVersions
--
--         , testModifyCacheCluster $
--             modifyCacheCluster
--
--         , testCreateCacheParameterGroup $
--             createCacheParameterGroup
--
--         , testDeleteCacheSubnetGroup $
--             deleteCacheSubnetGroup
--
--           ]

--     , testGroup "response"
--         [ testDeleteCacheSecurityGroupResponse $
--             deleteCacheSecurityGroupResponse
--
--         , testCreateReplicationGroupResponse $
--             createReplicationGroupResponse
--
--         , testDeleteCacheClusterResponse $
--             deleteCacheClusterResponse
--
--         , testRebootCacheClusterResponse $
--             rebootCacheClusterResponse
--
--         , testRevokeCacheSecurityGroupIngressResponse $
--             revokeCacheSecurityGroupIngressResponse
--
--         , testDescribeEventsResponse $
--             describeEventsResponse
--
--         , testDescribeEngineDefaultParametersResponse $
--             describeEngineDefaultParametersResponse
--
--         , testModifyCacheParameterGroupResponse $
--             cacheParameterGroupNameMessage
--
--         , testCreateCacheClusterResponse $
--             createCacheClusterResponse
--
--         , testListTagsForResourceResponse $
--             tagListMessage
--
--         , testDeleteReplicationGroupResponse $
--             deleteReplicationGroupResponse
--
--         , testPurchaseReservedCacheNodesOfferingResponse $
--             purchaseReservedCacheNodesOfferingResponse
--
--         , testDescribeCacheClustersResponse $
--             describeCacheClustersResponse
--
--         , testModifyReplicationGroupResponse $
--             modifyReplicationGroupResponse
--
--         , testRemoveTagsFromResourceResponse $
--             tagListMessage
--
--         , testDescribeCacheParametersResponse $
--             describeCacheParametersResponse
--
--         , testDescribeCacheSubnetGroupsResponse $
--             describeCacheSubnetGroupsResponse
--
--         , testCreateCacheSecurityGroupResponse $
--             createCacheSecurityGroupResponse
--
--         , testAddTagsToResourceResponse $
--             tagListMessage
--
--         , testAuthorizeCacheSecurityGroupIngressResponse $
--             authorizeCacheSecurityGroupIngressResponse
--
--         , testCopySnapshotResponse $
--             copySnapshotResponse
--
--         , testCreateCacheSubnetGroupResponse $
--             createCacheSubnetGroupResponse
--
--         , testDescribeCacheParameterGroupsResponse $
--             describeCacheParameterGroupsResponse
--
--         , testResetCacheParameterGroupResponse $
--             cacheParameterGroupNameMessage
--
--         , testDescribeSnapshotsResponse $
--             describeSnapshotsResponse
--
--         , testDescribeReservedCacheNodesOfferingsResponse $
--             describeReservedCacheNodesOfferingsResponse
--
--         , testDeleteSnapshotResponse $
--             deleteSnapshotResponse
--
--         , testDescribeReplicationGroupsResponse $
--             describeReplicationGroupsResponse
--
--         , testModifyCacheSubnetGroupResponse $
--             modifyCacheSubnetGroupResponse
--
--         , testCreateSnapshotResponse $
--             createSnapshotResponse
--
--         , testDescribeCacheSecurityGroupsResponse $
--             describeCacheSecurityGroupsResponse
--
--         , testDeleteCacheParameterGroupResponse $
--             deleteCacheParameterGroupResponse
--
--         , testDescribeReservedCacheNodesResponse $
--             describeReservedCacheNodesResponse
--
--         , testDescribeCacheEngineVersionsResponse $
--             describeCacheEngineVersionsResponse
--
--         , testModifyCacheClusterResponse $
--             modifyCacheClusterResponse
--
--         , testCreateCacheParameterGroupResponse $
--             createCacheParameterGroupResponse
--
--         , testDeleteCacheSubnetGroupResponse $
--             deleteCacheSubnetGroupResponse
--
--           ]
--     ]

-- Requests

testDeleteCacheSecurityGroup :: DeleteCacheSecurityGroup -> TestTree
testDeleteCacheSecurityGroup = req
    "DeleteCacheSecurityGroup"
    "fixture/DeleteCacheSecurityGroup"

testCreateReplicationGroup :: CreateReplicationGroup -> TestTree
testCreateReplicationGroup = req
    "CreateReplicationGroup"
    "fixture/CreateReplicationGroup"

testDeleteCacheCluster :: DeleteCacheCluster -> TestTree
testDeleteCacheCluster = req
    "DeleteCacheCluster"
    "fixture/DeleteCacheCluster"

testRebootCacheCluster :: RebootCacheCluster -> TestTree
testRebootCacheCluster = req
    "RebootCacheCluster"
    "fixture/RebootCacheCluster"

testRevokeCacheSecurityGroupIngress :: RevokeCacheSecurityGroupIngress -> TestTree
testRevokeCacheSecurityGroupIngress = req
    "RevokeCacheSecurityGroupIngress"
    "fixture/RevokeCacheSecurityGroupIngress"

testDescribeEvents :: DescribeEvents -> TestTree
testDescribeEvents = req
    "DescribeEvents"
    "fixture/DescribeEvents"

testDescribeEngineDefaultParameters :: DescribeEngineDefaultParameters -> TestTree
testDescribeEngineDefaultParameters = req
    "DescribeEngineDefaultParameters"
    "fixture/DescribeEngineDefaultParameters"

testModifyCacheParameterGroup :: ModifyCacheParameterGroup -> TestTree
testModifyCacheParameterGroup = req
    "ModifyCacheParameterGroup"
    "fixture/ModifyCacheParameterGroup"

testCreateCacheCluster :: CreateCacheCluster -> TestTree
testCreateCacheCluster = req
    "CreateCacheCluster"
    "fixture/CreateCacheCluster"

testListTagsForResource :: ListTagsForResource -> TestTree
testListTagsForResource = req
    "ListTagsForResource"
    "fixture/ListTagsForResource"

testDeleteReplicationGroup :: DeleteReplicationGroup -> TestTree
testDeleteReplicationGroup = req
    "DeleteReplicationGroup"
    "fixture/DeleteReplicationGroup"

testPurchaseReservedCacheNodesOffering :: PurchaseReservedCacheNodesOffering -> TestTree
testPurchaseReservedCacheNodesOffering = req
    "PurchaseReservedCacheNodesOffering"
    "fixture/PurchaseReservedCacheNodesOffering"

testDescribeCacheClusters :: DescribeCacheClusters -> TestTree
testDescribeCacheClusters = req
    "DescribeCacheClusters"
    "fixture/DescribeCacheClusters"

testModifyReplicationGroup :: ModifyReplicationGroup -> TestTree
testModifyReplicationGroup = req
    "ModifyReplicationGroup"
    "fixture/ModifyReplicationGroup"

testRemoveTagsFromResource :: RemoveTagsFromResource -> TestTree
testRemoveTagsFromResource = req
    "RemoveTagsFromResource"
    "fixture/RemoveTagsFromResource"

testDescribeCacheParameters :: DescribeCacheParameters -> TestTree
testDescribeCacheParameters = req
    "DescribeCacheParameters"
    "fixture/DescribeCacheParameters"

testDescribeCacheSubnetGroups :: DescribeCacheSubnetGroups -> TestTree
testDescribeCacheSubnetGroups = req
    "DescribeCacheSubnetGroups"
    "fixture/DescribeCacheSubnetGroups"

testCreateCacheSecurityGroup :: CreateCacheSecurityGroup -> TestTree
testCreateCacheSecurityGroup = req
    "CreateCacheSecurityGroup"
    "fixture/CreateCacheSecurityGroup"

testAddTagsToResource :: AddTagsToResource -> TestTree
testAddTagsToResource = req
    "AddTagsToResource"
    "fixture/AddTagsToResource"

testAuthorizeCacheSecurityGroupIngress :: AuthorizeCacheSecurityGroupIngress -> TestTree
testAuthorizeCacheSecurityGroupIngress = req
    "AuthorizeCacheSecurityGroupIngress"
    "fixture/AuthorizeCacheSecurityGroupIngress"

testCopySnapshot :: CopySnapshot -> TestTree
testCopySnapshot = req
    "CopySnapshot"
    "fixture/CopySnapshot"

testCreateCacheSubnetGroup :: CreateCacheSubnetGroup -> TestTree
testCreateCacheSubnetGroup = req
    "CreateCacheSubnetGroup"
    "fixture/CreateCacheSubnetGroup"

testDescribeCacheParameterGroups :: DescribeCacheParameterGroups -> TestTree
testDescribeCacheParameterGroups = req
    "DescribeCacheParameterGroups"
    "fixture/DescribeCacheParameterGroups"

testResetCacheParameterGroup :: ResetCacheParameterGroup -> TestTree
testResetCacheParameterGroup = req
    "ResetCacheParameterGroup"
    "fixture/ResetCacheParameterGroup"

testDescribeSnapshots :: DescribeSnapshots -> TestTree
testDescribeSnapshots = req
    "DescribeSnapshots"
    "fixture/DescribeSnapshots"

testDescribeReservedCacheNodesOfferings :: DescribeReservedCacheNodesOfferings -> TestTree
testDescribeReservedCacheNodesOfferings = req
    "DescribeReservedCacheNodesOfferings"
    "fixture/DescribeReservedCacheNodesOfferings"

testDeleteSnapshot :: DeleteSnapshot -> TestTree
testDeleteSnapshot = req
    "DeleteSnapshot"
    "fixture/DeleteSnapshot"

testDescribeReplicationGroups :: DescribeReplicationGroups -> TestTree
testDescribeReplicationGroups = req
    "DescribeReplicationGroups"
    "fixture/DescribeReplicationGroups"

testModifyCacheSubnetGroup :: ModifyCacheSubnetGroup -> TestTree
testModifyCacheSubnetGroup = req
    "ModifyCacheSubnetGroup"
    "fixture/ModifyCacheSubnetGroup"

testCreateSnapshot :: CreateSnapshot -> TestTree
testCreateSnapshot = req
    "CreateSnapshot"
    "fixture/CreateSnapshot"

testDescribeCacheSecurityGroups :: DescribeCacheSecurityGroups -> TestTree
testDescribeCacheSecurityGroups = req
    "DescribeCacheSecurityGroups"
    "fixture/DescribeCacheSecurityGroups"

testDeleteCacheParameterGroup :: DeleteCacheParameterGroup -> TestTree
testDeleteCacheParameterGroup = req
    "DeleteCacheParameterGroup"
    "fixture/DeleteCacheParameterGroup"

testDescribeReservedCacheNodes :: DescribeReservedCacheNodes -> TestTree
testDescribeReservedCacheNodes = req
    "DescribeReservedCacheNodes"
    "fixture/DescribeReservedCacheNodes"

testDescribeCacheEngineVersions :: DescribeCacheEngineVersions -> TestTree
testDescribeCacheEngineVersions = req
    "DescribeCacheEngineVersions"
    "fixture/DescribeCacheEngineVersions"

testModifyCacheCluster :: ModifyCacheCluster -> TestTree
testModifyCacheCluster = req
    "ModifyCacheCluster"
    "fixture/ModifyCacheCluster"

testCreateCacheParameterGroup :: CreateCacheParameterGroup -> TestTree
testCreateCacheParameterGroup = req
    "CreateCacheParameterGroup"
    "fixture/CreateCacheParameterGroup"

testDeleteCacheSubnetGroup :: DeleteCacheSubnetGroup -> TestTree
testDeleteCacheSubnetGroup = req
    "DeleteCacheSubnetGroup"
    "fixture/DeleteCacheSubnetGroup"

-- Responses

testDeleteCacheSecurityGroupResponse :: DeleteCacheSecurityGroupResponse -> TestTree
testDeleteCacheSecurityGroupResponse = res
    "DeleteCacheSecurityGroupResponse"
    "fixture/DeleteCacheSecurityGroupResponse"
    elastiCache
    (Proxy :: Proxy DeleteCacheSecurityGroup)

testCreateReplicationGroupResponse :: CreateReplicationGroupResponse -> TestTree
testCreateReplicationGroupResponse = res
    "CreateReplicationGroupResponse"
    "fixture/CreateReplicationGroupResponse"
    elastiCache
    (Proxy :: Proxy CreateReplicationGroup)

testDeleteCacheClusterResponse :: DeleteCacheClusterResponse -> TestTree
testDeleteCacheClusterResponse = res
    "DeleteCacheClusterResponse"
    "fixture/DeleteCacheClusterResponse"
    elastiCache
    (Proxy :: Proxy DeleteCacheCluster)

testRebootCacheClusterResponse :: RebootCacheClusterResponse -> TestTree
testRebootCacheClusterResponse = res
    "RebootCacheClusterResponse"
    "fixture/RebootCacheClusterResponse"
    elastiCache
    (Proxy :: Proxy RebootCacheCluster)

testRevokeCacheSecurityGroupIngressResponse :: RevokeCacheSecurityGroupIngressResponse -> TestTree
testRevokeCacheSecurityGroupIngressResponse = res
    "RevokeCacheSecurityGroupIngressResponse"
    "fixture/RevokeCacheSecurityGroupIngressResponse"
    elastiCache
    (Proxy :: Proxy RevokeCacheSecurityGroupIngress)

testDescribeEventsResponse :: DescribeEventsResponse -> TestTree
testDescribeEventsResponse = res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse"
    elastiCache
    (Proxy :: Proxy DescribeEvents)

testDescribeEngineDefaultParametersResponse :: DescribeEngineDefaultParametersResponse -> TestTree
testDescribeEngineDefaultParametersResponse = res
    "DescribeEngineDefaultParametersResponse"
    "fixture/DescribeEngineDefaultParametersResponse"
    elastiCache
    (Proxy :: Proxy DescribeEngineDefaultParameters)

testModifyCacheParameterGroupResponse :: CacheParameterGroupNameMessage -> TestTree
testModifyCacheParameterGroupResponse = res
    "ModifyCacheParameterGroupResponse"
    "fixture/ModifyCacheParameterGroupResponse"
    elastiCache
    (Proxy :: Proxy ModifyCacheParameterGroup)

testCreateCacheClusterResponse :: CreateCacheClusterResponse -> TestTree
testCreateCacheClusterResponse = res
    "CreateCacheClusterResponse"
    "fixture/CreateCacheClusterResponse"
    elastiCache
    (Proxy :: Proxy CreateCacheCluster)

testListTagsForResourceResponse :: TagListMessage -> TestTree
testListTagsForResourceResponse = res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse"
    elastiCache
    (Proxy :: Proxy ListTagsForResource)

testDeleteReplicationGroupResponse :: DeleteReplicationGroupResponse -> TestTree
testDeleteReplicationGroupResponse = res
    "DeleteReplicationGroupResponse"
    "fixture/DeleteReplicationGroupResponse"
    elastiCache
    (Proxy :: Proxy DeleteReplicationGroup)

testPurchaseReservedCacheNodesOfferingResponse :: PurchaseReservedCacheNodesOfferingResponse -> TestTree
testPurchaseReservedCacheNodesOfferingResponse = res
    "PurchaseReservedCacheNodesOfferingResponse"
    "fixture/PurchaseReservedCacheNodesOfferingResponse"
    elastiCache
    (Proxy :: Proxy PurchaseReservedCacheNodesOffering)

testDescribeCacheClustersResponse :: DescribeCacheClustersResponse -> TestTree
testDescribeCacheClustersResponse = res
    "DescribeCacheClustersResponse"
    "fixture/DescribeCacheClustersResponse"
    elastiCache
    (Proxy :: Proxy DescribeCacheClusters)

testModifyReplicationGroupResponse :: ModifyReplicationGroupResponse -> TestTree
testModifyReplicationGroupResponse = res
    "ModifyReplicationGroupResponse"
    "fixture/ModifyReplicationGroupResponse"
    elastiCache
    (Proxy :: Proxy ModifyReplicationGroup)

testRemoveTagsFromResourceResponse :: TagListMessage -> TestTree
testRemoveTagsFromResourceResponse = res
    "RemoveTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse"
    elastiCache
    (Proxy :: Proxy RemoveTagsFromResource)

testDescribeCacheParametersResponse :: DescribeCacheParametersResponse -> TestTree
testDescribeCacheParametersResponse = res
    "DescribeCacheParametersResponse"
    "fixture/DescribeCacheParametersResponse"
    elastiCache
    (Proxy :: Proxy DescribeCacheParameters)

testDescribeCacheSubnetGroupsResponse :: DescribeCacheSubnetGroupsResponse -> TestTree
testDescribeCacheSubnetGroupsResponse = res
    "DescribeCacheSubnetGroupsResponse"
    "fixture/DescribeCacheSubnetGroupsResponse"
    elastiCache
    (Proxy :: Proxy DescribeCacheSubnetGroups)

testCreateCacheSecurityGroupResponse :: CreateCacheSecurityGroupResponse -> TestTree
testCreateCacheSecurityGroupResponse = res
    "CreateCacheSecurityGroupResponse"
    "fixture/CreateCacheSecurityGroupResponse"
    elastiCache
    (Proxy :: Proxy CreateCacheSecurityGroup)

testAddTagsToResourceResponse :: TagListMessage -> TestTree
testAddTagsToResourceResponse = res
    "AddTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse"
    elastiCache
    (Proxy :: Proxy AddTagsToResource)

testAuthorizeCacheSecurityGroupIngressResponse :: AuthorizeCacheSecurityGroupIngressResponse -> TestTree
testAuthorizeCacheSecurityGroupIngressResponse = res
    "AuthorizeCacheSecurityGroupIngressResponse"
    "fixture/AuthorizeCacheSecurityGroupIngressResponse"
    elastiCache
    (Proxy :: Proxy AuthorizeCacheSecurityGroupIngress)

testCopySnapshotResponse :: CopySnapshotResponse -> TestTree
testCopySnapshotResponse = res
    "CopySnapshotResponse"
    "fixture/CopySnapshotResponse"
    elastiCache
    (Proxy :: Proxy CopySnapshot)

testCreateCacheSubnetGroupResponse :: CreateCacheSubnetGroupResponse -> TestTree
testCreateCacheSubnetGroupResponse = res
    "CreateCacheSubnetGroupResponse"
    "fixture/CreateCacheSubnetGroupResponse"
    elastiCache
    (Proxy :: Proxy CreateCacheSubnetGroup)

testDescribeCacheParameterGroupsResponse :: DescribeCacheParameterGroupsResponse -> TestTree
testDescribeCacheParameterGroupsResponse = res
    "DescribeCacheParameterGroupsResponse"
    "fixture/DescribeCacheParameterGroupsResponse"
    elastiCache
    (Proxy :: Proxy DescribeCacheParameterGroups)

testResetCacheParameterGroupResponse :: CacheParameterGroupNameMessage -> TestTree
testResetCacheParameterGroupResponse = res
    "ResetCacheParameterGroupResponse"
    "fixture/ResetCacheParameterGroupResponse"
    elastiCache
    (Proxy :: Proxy ResetCacheParameterGroup)

testDescribeSnapshotsResponse :: DescribeSnapshotsResponse -> TestTree
testDescribeSnapshotsResponse = res
    "DescribeSnapshotsResponse"
    "fixture/DescribeSnapshotsResponse"
    elastiCache
    (Proxy :: Proxy DescribeSnapshots)

testDescribeReservedCacheNodesOfferingsResponse :: DescribeReservedCacheNodesOfferingsResponse -> TestTree
testDescribeReservedCacheNodesOfferingsResponse = res
    "DescribeReservedCacheNodesOfferingsResponse"
    "fixture/DescribeReservedCacheNodesOfferingsResponse"
    elastiCache
    (Proxy :: Proxy DescribeReservedCacheNodesOfferings)

testDeleteSnapshotResponse :: DeleteSnapshotResponse -> TestTree
testDeleteSnapshotResponse = res
    "DeleteSnapshotResponse"
    "fixture/DeleteSnapshotResponse"
    elastiCache
    (Proxy :: Proxy DeleteSnapshot)

testDescribeReplicationGroupsResponse :: DescribeReplicationGroupsResponse -> TestTree
testDescribeReplicationGroupsResponse = res
    "DescribeReplicationGroupsResponse"
    "fixture/DescribeReplicationGroupsResponse"
    elastiCache
    (Proxy :: Proxy DescribeReplicationGroups)

testModifyCacheSubnetGroupResponse :: ModifyCacheSubnetGroupResponse -> TestTree
testModifyCacheSubnetGroupResponse = res
    "ModifyCacheSubnetGroupResponse"
    "fixture/ModifyCacheSubnetGroupResponse"
    elastiCache
    (Proxy :: Proxy ModifyCacheSubnetGroup)

testCreateSnapshotResponse :: CreateSnapshotResponse -> TestTree
testCreateSnapshotResponse = res
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse"
    elastiCache
    (Proxy :: Proxy CreateSnapshot)

testDescribeCacheSecurityGroupsResponse :: DescribeCacheSecurityGroupsResponse -> TestTree
testDescribeCacheSecurityGroupsResponse = res
    "DescribeCacheSecurityGroupsResponse"
    "fixture/DescribeCacheSecurityGroupsResponse"
    elastiCache
    (Proxy :: Proxy DescribeCacheSecurityGroups)

testDeleteCacheParameterGroupResponse :: DeleteCacheParameterGroupResponse -> TestTree
testDeleteCacheParameterGroupResponse = res
    "DeleteCacheParameterGroupResponse"
    "fixture/DeleteCacheParameterGroupResponse"
    elastiCache
    (Proxy :: Proxy DeleteCacheParameterGroup)

testDescribeReservedCacheNodesResponse :: DescribeReservedCacheNodesResponse -> TestTree
testDescribeReservedCacheNodesResponse = res
    "DescribeReservedCacheNodesResponse"
    "fixture/DescribeReservedCacheNodesResponse"
    elastiCache
    (Proxy :: Proxy DescribeReservedCacheNodes)

testDescribeCacheEngineVersionsResponse :: DescribeCacheEngineVersionsResponse -> TestTree
testDescribeCacheEngineVersionsResponse = res
    "DescribeCacheEngineVersionsResponse"
    "fixture/DescribeCacheEngineVersionsResponse"
    elastiCache
    (Proxy :: Proxy DescribeCacheEngineVersions)

testModifyCacheClusterResponse :: ModifyCacheClusterResponse -> TestTree
testModifyCacheClusterResponse = res
    "ModifyCacheClusterResponse"
    "fixture/ModifyCacheClusterResponse"
    elastiCache
    (Proxy :: Proxy ModifyCacheCluster)

testCreateCacheParameterGroupResponse :: CreateCacheParameterGroupResponse -> TestTree
testCreateCacheParameterGroupResponse = res
    "CreateCacheParameterGroupResponse"
    "fixture/CreateCacheParameterGroupResponse"
    elastiCache
    (Proxy :: Proxy CreateCacheParameterGroup)

testDeleteCacheSubnetGroupResponse :: DeleteCacheSubnetGroupResponse -> TestTree
testDeleteCacheSubnetGroupResponse = res
    "DeleteCacheSubnetGroupResponse"
    "fixture/DeleteCacheSubnetGroupResponse"
    elastiCache
    (Proxy :: Proxy DeleteCacheSubnetGroup)
