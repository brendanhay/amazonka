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
    "fixture/DeleteCacheSecurityGroup.yaml"

testCreateReplicationGroup :: CreateReplicationGroup -> TestTree
testCreateReplicationGroup = req
    "CreateReplicationGroup"
    "fixture/CreateReplicationGroup.yaml"

testDeleteCacheCluster :: DeleteCacheCluster -> TestTree
testDeleteCacheCluster = req
    "DeleteCacheCluster"
    "fixture/DeleteCacheCluster.yaml"

testRebootCacheCluster :: RebootCacheCluster -> TestTree
testRebootCacheCluster = req
    "RebootCacheCluster"
    "fixture/RebootCacheCluster.yaml"

testRevokeCacheSecurityGroupIngress :: RevokeCacheSecurityGroupIngress -> TestTree
testRevokeCacheSecurityGroupIngress = req
    "RevokeCacheSecurityGroupIngress"
    "fixture/RevokeCacheSecurityGroupIngress.yaml"

testDescribeEvents :: DescribeEvents -> TestTree
testDescribeEvents = req
    "DescribeEvents"
    "fixture/DescribeEvents.yaml"

testDescribeEngineDefaultParameters :: DescribeEngineDefaultParameters -> TestTree
testDescribeEngineDefaultParameters = req
    "DescribeEngineDefaultParameters"
    "fixture/DescribeEngineDefaultParameters.yaml"

testModifyCacheParameterGroup :: ModifyCacheParameterGroup -> TestTree
testModifyCacheParameterGroup = req
    "ModifyCacheParameterGroup"
    "fixture/ModifyCacheParameterGroup.yaml"

testCreateCacheCluster :: CreateCacheCluster -> TestTree
testCreateCacheCluster = req
    "CreateCacheCluster"
    "fixture/CreateCacheCluster.yaml"

testListTagsForResource :: ListTagsForResource -> TestTree
testListTagsForResource = req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

testDeleteReplicationGroup :: DeleteReplicationGroup -> TestTree
testDeleteReplicationGroup = req
    "DeleteReplicationGroup"
    "fixture/DeleteReplicationGroup.yaml"

testPurchaseReservedCacheNodesOffering :: PurchaseReservedCacheNodesOffering -> TestTree
testPurchaseReservedCacheNodesOffering = req
    "PurchaseReservedCacheNodesOffering"
    "fixture/PurchaseReservedCacheNodesOffering.yaml"

testDescribeCacheClusters :: DescribeCacheClusters -> TestTree
testDescribeCacheClusters = req
    "DescribeCacheClusters"
    "fixture/DescribeCacheClusters.yaml"

testModifyReplicationGroup :: ModifyReplicationGroup -> TestTree
testModifyReplicationGroup = req
    "ModifyReplicationGroup"
    "fixture/ModifyReplicationGroup.yaml"

testRemoveTagsFromResource :: RemoveTagsFromResource -> TestTree
testRemoveTagsFromResource = req
    "RemoveTagsFromResource"
    "fixture/RemoveTagsFromResource.yaml"

testDescribeCacheParameters :: DescribeCacheParameters -> TestTree
testDescribeCacheParameters = req
    "DescribeCacheParameters"
    "fixture/DescribeCacheParameters.yaml"

testDescribeCacheSubnetGroups :: DescribeCacheSubnetGroups -> TestTree
testDescribeCacheSubnetGroups = req
    "DescribeCacheSubnetGroups"
    "fixture/DescribeCacheSubnetGroups.yaml"

testCreateCacheSecurityGroup :: CreateCacheSecurityGroup -> TestTree
testCreateCacheSecurityGroup = req
    "CreateCacheSecurityGroup"
    "fixture/CreateCacheSecurityGroup.yaml"

testAddTagsToResource :: AddTagsToResource -> TestTree
testAddTagsToResource = req
    "AddTagsToResource"
    "fixture/AddTagsToResource.yaml"

testAuthorizeCacheSecurityGroupIngress :: AuthorizeCacheSecurityGroupIngress -> TestTree
testAuthorizeCacheSecurityGroupIngress = req
    "AuthorizeCacheSecurityGroupIngress"
    "fixture/AuthorizeCacheSecurityGroupIngress.yaml"

testCopySnapshot :: CopySnapshot -> TestTree
testCopySnapshot = req
    "CopySnapshot"
    "fixture/CopySnapshot.yaml"

testCreateCacheSubnetGroup :: CreateCacheSubnetGroup -> TestTree
testCreateCacheSubnetGroup = req
    "CreateCacheSubnetGroup"
    "fixture/CreateCacheSubnetGroup.yaml"

testDescribeCacheParameterGroups :: DescribeCacheParameterGroups -> TestTree
testDescribeCacheParameterGroups = req
    "DescribeCacheParameterGroups"
    "fixture/DescribeCacheParameterGroups.yaml"

testResetCacheParameterGroup :: ResetCacheParameterGroup -> TestTree
testResetCacheParameterGroup = req
    "ResetCacheParameterGroup"
    "fixture/ResetCacheParameterGroup.yaml"

testDescribeSnapshots :: DescribeSnapshots -> TestTree
testDescribeSnapshots = req
    "DescribeSnapshots"
    "fixture/DescribeSnapshots.yaml"

testDescribeReservedCacheNodesOfferings :: DescribeReservedCacheNodesOfferings -> TestTree
testDescribeReservedCacheNodesOfferings = req
    "DescribeReservedCacheNodesOfferings"
    "fixture/DescribeReservedCacheNodesOfferings.yaml"

testDeleteSnapshot :: DeleteSnapshot -> TestTree
testDeleteSnapshot = req
    "DeleteSnapshot"
    "fixture/DeleteSnapshot.yaml"

testDescribeReplicationGroups :: DescribeReplicationGroups -> TestTree
testDescribeReplicationGroups = req
    "DescribeReplicationGroups"
    "fixture/DescribeReplicationGroups.yaml"

testModifyCacheSubnetGroup :: ModifyCacheSubnetGroup -> TestTree
testModifyCacheSubnetGroup = req
    "ModifyCacheSubnetGroup"
    "fixture/ModifyCacheSubnetGroup.yaml"

testCreateSnapshot :: CreateSnapshot -> TestTree
testCreateSnapshot = req
    "CreateSnapshot"
    "fixture/CreateSnapshot.yaml"

testDescribeCacheSecurityGroups :: DescribeCacheSecurityGroups -> TestTree
testDescribeCacheSecurityGroups = req
    "DescribeCacheSecurityGroups"
    "fixture/DescribeCacheSecurityGroups.yaml"

testDeleteCacheParameterGroup :: DeleteCacheParameterGroup -> TestTree
testDeleteCacheParameterGroup = req
    "DeleteCacheParameterGroup"
    "fixture/DeleteCacheParameterGroup.yaml"

testDescribeReservedCacheNodes :: DescribeReservedCacheNodes -> TestTree
testDescribeReservedCacheNodes = req
    "DescribeReservedCacheNodes"
    "fixture/DescribeReservedCacheNodes.yaml"

testDescribeCacheEngineVersions :: DescribeCacheEngineVersions -> TestTree
testDescribeCacheEngineVersions = req
    "DescribeCacheEngineVersions"
    "fixture/DescribeCacheEngineVersions.yaml"

testModifyCacheCluster :: ModifyCacheCluster -> TestTree
testModifyCacheCluster = req
    "ModifyCacheCluster"
    "fixture/ModifyCacheCluster.yaml"

testCreateCacheParameterGroup :: CreateCacheParameterGroup -> TestTree
testCreateCacheParameterGroup = req
    "CreateCacheParameterGroup"
    "fixture/CreateCacheParameterGroup.yaml"

testDeleteCacheSubnetGroup :: DeleteCacheSubnetGroup -> TestTree
testDeleteCacheSubnetGroup = req
    "DeleteCacheSubnetGroup"
    "fixture/DeleteCacheSubnetGroup.yaml"

-- Responses

testDeleteCacheSecurityGroupResponse :: DeleteCacheSecurityGroupResponse -> TestTree
testDeleteCacheSecurityGroupResponse = res
    "DeleteCacheSecurityGroupResponse"
    "fixture/DeleteCacheSecurityGroupResponse.proto"
    elastiCache
    (Proxy :: Proxy DeleteCacheSecurityGroup)

testCreateReplicationGroupResponse :: CreateReplicationGroupResponse -> TestTree
testCreateReplicationGroupResponse = res
    "CreateReplicationGroupResponse"
    "fixture/CreateReplicationGroupResponse.proto"
    elastiCache
    (Proxy :: Proxy CreateReplicationGroup)

testDeleteCacheClusterResponse :: DeleteCacheClusterResponse -> TestTree
testDeleteCacheClusterResponse = res
    "DeleteCacheClusterResponse"
    "fixture/DeleteCacheClusterResponse.proto"
    elastiCache
    (Proxy :: Proxy DeleteCacheCluster)

testRebootCacheClusterResponse :: RebootCacheClusterResponse -> TestTree
testRebootCacheClusterResponse = res
    "RebootCacheClusterResponse"
    "fixture/RebootCacheClusterResponse.proto"
    elastiCache
    (Proxy :: Proxy RebootCacheCluster)

testRevokeCacheSecurityGroupIngressResponse :: RevokeCacheSecurityGroupIngressResponse -> TestTree
testRevokeCacheSecurityGroupIngressResponse = res
    "RevokeCacheSecurityGroupIngressResponse"
    "fixture/RevokeCacheSecurityGroupIngressResponse.proto"
    elastiCache
    (Proxy :: Proxy RevokeCacheSecurityGroupIngress)

testDescribeEventsResponse :: DescribeEventsResponse -> TestTree
testDescribeEventsResponse = res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse.proto"
    elastiCache
    (Proxy :: Proxy DescribeEvents)

testDescribeEngineDefaultParametersResponse :: DescribeEngineDefaultParametersResponse -> TestTree
testDescribeEngineDefaultParametersResponse = res
    "DescribeEngineDefaultParametersResponse"
    "fixture/DescribeEngineDefaultParametersResponse.proto"
    elastiCache
    (Proxy :: Proxy DescribeEngineDefaultParameters)

testModifyCacheParameterGroupResponse :: CacheParameterGroupNameMessage -> TestTree
testModifyCacheParameterGroupResponse = res
    "ModifyCacheParameterGroupResponse"
    "fixture/ModifyCacheParameterGroupResponse.proto"
    elastiCache
    (Proxy :: Proxy ModifyCacheParameterGroup)

testCreateCacheClusterResponse :: CreateCacheClusterResponse -> TestTree
testCreateCacheClusterResponse = res
    "CreateCacheClusterResponse"
    "fixture/CreateCacheClusterResponse.proto"
    elastiCache
    (Proxy :: Proxy CreateCacheCluster)

testListTagsForResourceResponse :: TagListMessage -> TestTree
testListTagsForResourceResponse = res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    elastiCache
    (Proxy :: Proxy ListTagsForResource)

testDeleteReplicationGroupResponse :: DeleteReplicationGroupResponse -> TestTree
testDeleteReplicationGroupResponse = res
    "DeleteReplicationGroupResponse"
    "fixture/DeleteReplicationGroupResponse.proto"
    elastiCache
    (Proxy :: Proxy DeleteReplicationGroup)

testPurchaseReservedCacheNodesOfferingResponse :: PurchaseReservedCacheNodesOfferingResponse -> TestTree
testPurchaseReservedCacheNodesOfferingResponse = res
    "PurchaseReservedCacheNodesOfferingResponse"
    "fixture/PurchaseReservedCacheNodesOfferingResponse.proto"
    elastiCache
    (Proxy :: Proxy PurchaseReservedCacheNodesOffering)

testDescribeCacheClustersResponse :: DescribeCacheClustersResponse -> TestTree
testDescribeCacheClustersResponse = res
    "DescribeCacheClustersResponse"
    "fixture/DescribeCacheClustersResponse.proto"
    elastiCache
    (Proxy :: Proxy DescribeCacheClusters)

testModifyReplicationGroupResponse :: ModifyReplicationGroupResponse -> TestTree
testModifyReplicationGroupResponse = res
    "ModifyReplicationGroupResponse"
    "fixture/ModifyReplicationGroupResponse.proto"
    elastiCache
    (Proxy :: Proxy ModifyReplicationGroup)

testRemoveTagsFromResourceResponse :: TagListMessage -> TestTree
testRemoveTagsFromResourceResponse = res
    "RemoveTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse.proto"
    elastiCache
    (Proxy :: Proxy RemoveTagsFromResource)

testDescribeCacheParametersResponse :: DescribeCacheParametersResponse -> TestTree
testDescribeCacheParametersResponse = res
    "DescribeCacheParametersResponse"
    "fixture/DescribeCacheParametersResponse.proto"
    elastiCache
    (Proxy :: Proxy DescribeCacheParameters)

testDescribeCacheSubnetGroupsResponse :: DescribeCacheSubnetGroupsResponse -> TestTree
testDescribeCacheSubnetGroupsResponse = res
    "DescribeCacheSubnetGroupsResponse"
    "fixture/DescribeCacheSubnetGroupsResponse.proto"
    elastiCache
    (Proxy :: Proxy DescribeCacheSubnetGroups)

testCreateCacheSecurityGroupResponse :: CreateCacheSecurityGroupResponse -> TestTree
testCreateCacheSecurityGroupResponse = res
    "CreateCacheSecurityGroupResponse"
    "fixture/CreateCacheSecurityGroupResponse.proto"
    elastiCache
    (Proxy :: Proxy CreateCacheSecurityGroup)

testAddTagsToResourceResponse :: TagListMessage -> TestTree
testAddTagsToResourceResponse = res
    "AddTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse.proto"
    elastiCache
    (Proxy :: Proxy AddTagsToResource)

testAuthorizeCacheSecurityGroupIngressResponse :: AuthorizeCacheSecurityGroupIngressResponse -> TestTree
testAuthorizeCacheSecurityGroupIngressResponse = res
    "AuthorizeCacheSecurityGroupIngressResponse"
    "fixture/AuthorizeCacheSecurityGroupIngressResponse.proto"
    elastiCache
    (Proxy :: Proxy AuthorizeCacheSecurityGroupIngress)

testCopySnapshotResponse :: CopySnapshotResponse -> TestTree
testCopySnapshotResponse = res
    "CopySnapshotResponse"
    "fixture/CopySnapshotResponse.proto"
    elastiCache
    (Proxy :: Proxy CopySnapshot)

testCreateCacheSubnetGroupResponse :: CreateCacheSubnetGroupResponse -> TestTree
testCreateCacheSubnetGroupResponse = res
    "CreateCacheSubnetGroupResponse"
    "fixture/CreateCacheSubnetGroupResponse.proto"
    elastiCache
    (Proxy :: Proxy CreateCacheSubnetGroup)

testDescribeCacheParameterGroupsResponse :: DescribeCacheParameterGroupsResponse -> TestTree
testDescribeCacheParameterGroupsResponse = res
    "DescribeCacheParameterGroupsResponse"
    "fixture/DescribeCacheParameterGroupsResponse.proto"
    elastiCache
    (Proxy :: Proxy DescribeCacheParameterGroups)

testResetCacheParameterGroupResponse :: CacheParameterGroupNameMessage -> TestTree
testResetCacheParameterGroupResponse = res
    "ResetCacheParameterGroupResponse"
    "fixture/ResetCacheParameterGroupResponse.proto"
    elastiCache
    (Proxy :: Proxy ResetCacheParameterGroup)

testDescribeSnapshotsResponse :: DescribeSnapshotsResponse -> TestTree
testDescribeSnapshotsResponse = res
    "DescribeSnapshotsResponse"
    "fixture/DescribeSnapshotsResponse.proto"
    elastiCache
    (Proxy :: Proxy DescribeSnapshots)

testDescribeReservedCacheNodesOfferingsResponse :: DescribeReservedCacheNodesOfferingsResponse -> TestTree
testDescribeReservedCacheNodesOfferingsResponse = res
    "DescribeReservedCacheNodesOfferingsResponse"
    "fixture/DescribeReservedCacheNodesOfferingsResponse.proto"
    elastiCache
    (Proxy :: Proxy DescribeReservedCacheNodesOfferings)

testDeleteSnapshotResponse :: DeleteSnapshotResponse -> TestTree
testDeleteSnapshotResponse = res
    "DeleteSnapshotResponse"
    "fixture/DeleteSnapshotResponse.proto"
    elastiCache
    (Proxy :: Proxy DeleteSnapshot)

testDescribeReplicationGroupsResponse :: DescribeReplicationGroupsResponse -> TestTree
testDescribeReplicationGroupsResponse = res
    "DescribeReplicationGroupsResponse"
    "fixture/DescribeReplicationGroupsResponse.proto"
    elastiCache
    (Proxy :: Proxy DescribeReplicationGroups)

testModifyCacheSubnetGroupResponse :: ModifyCacheSubnetGroupResponse -> TestTree
testModifyCacheSubnetGroupResponse = res
    "ModifyCacheSubnetGroupResponse"
    "fixture/ModifyCacheSubnetGroupResponse.proto"
    elastiCache
    (Proxy :: Proxy ModifyCacheSubnetGroup)

testCreateSnapshotResponse :: CreateSnapshotResponse -> TestTree
testCreateSnapshotResponse = res
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse.proto"
    elastiCache
    (Proxy :: Proxy CreateSnapshot)

testDescribeCacheSecurityGroupsResponse :: DescribeCacheSecurityGroupsResponse -> TestTree
testDescribeCacheSecurityGroupsResponse = res
    "DescribeCacheSecurityGroupsResponse"
    "fixture/DescribeCacheSecurityGroupsResponse.proto"
    elastiCache
    (Proxy :: Proxy DescribeCacheSecurityGroups)

testDeleteCacheParameterGroupResponse :: DeleteCacheParameterGroupResponse -> TestTree
testDeleteCacheParameterGroupResponse = res
    "DeleteCacheParameterGroupResponse"
    "fixture/DeleteCacheParameterGroupResponse.proto"
    elastiCache
    (Proxy :: Proxy DeleteCacheParameterGroup)

testDescribeReservedCacheNodesResponse :: DescribeReservedCacheNodesResponse -> TestTree
testDescribeReservedCacheNodesResponse = res
    "DescribeReservedCacheNodesResponse"
    "fixture/DescribeReservedCacheNodesResponse.proto"
    elastiCache
    (Proxy :: Proxy DescribeReservedCacheNodes)

testDescribeCacheEngineVersionsResponse :: DescribeCacheEngineVersionsResponse -> TestTree
testDescribeCacheEngineVersionsResponse = res
    "DescribeCacheEngineVersionsResponse"
    "fixture/DescribeCacheEngineVersionsResponse.proto"
    elastiCache
    (Proxy :: Proxy DescribeCacheEngineVersions)

testModifyCacheClusterResponse :: ModifyCacheClusterResponse -> TestTree
testModifyCacheClusterResponse = res
    "ModifyCacheClusterResponse"
    "fixture/ModifyCacheClusterResponse.proto"
    elastiCache
    (Proxy :: Proxy ModifyCacheCluster)

testCreateCacheParameterGroupResponse :: CreateCacheParameterGroupResponse -> TestTree
testCreateCacheParameterGroupResponse = res
    "CreateCacheParameterGroupResponse"
    "fixture/CreateCacheParameterGroupResponse.proto"
    elastiCache
    (Proxy :: Proxy CreateCacheParameterGroup)

testDeleteCacheSubnetGroupResponse :: DeleteCacheSubnetGroupResponse -> TestTree
testDeleteCacheSubnetGroupResponse = res
    "DeleteCacheSubnetGroupResponse"
    "fixture/DeleteCacheSubnetGroupResponse.proto"
    elastiCache
    (Proxy :: Proxy DeleteCacheSubnetGroup)
