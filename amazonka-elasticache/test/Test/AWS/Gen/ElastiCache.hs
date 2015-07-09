{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ElastiCache
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.ElastiCache where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.ElastiCache

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
testDeleteCacheSecurityGroup = undefined

testCreateReplicationGroup :: CreateReplicationGroup -> TestTree
testCreateReplicationGroup = undefined

testDeleteCacheCluster :: DeleteCacheCluster -> TestTree
testDeleteCacheCluster = undefined

testRebootCacheCluster :: RebootCacheCluster -> TestTree
testRebootCacheCluster = undefined

testRevokeCacheSecurityGroupIngress :: RevokeCacheSecurityGroupIngress -> TestTree
testRevokeCacheSecurityGroupIngress = undefined

testDescribeEvents :: DescribeEvents -> TestTree
testDescribeEvents = undefined

testDescribeEngineDefaultParameters :: DescribeEngineDefaultParameters -> TestTree
testDescribeEngineDefaultParameters = undefined

testModifyCacheParameterGroup :: ModifyCacheParameterGroup -> TestTree
testModifyCacheParameterGroup = undefined

testCreateCacheCluster :: CreateCacheCluster -> TestTree
testCreateCacheCluster = undefined

testListTagsForResource :: ListTagsForResource -> TestTree
testListTagsForResource = undefined

testDeleteReplicationGroup :: DeleteReplicationGroup -> TestTree
testDeleteReplicationGroup = undefined

testPurchaseReservedCacheNodesOffering :: PurchaseReservedCacheNodesOffering -> TestTree
testPurchaseReservedCacheNodesOffering = undefined

testDescribeCacheClusters :: DescribeCacheClusters -> TestTree
testDescribeCacheClusters = undefined

testModifyReplicationGroup :: ModifyReplicationGroup -> TestTree
testModifyReplicationGroup = undefined

testRemoveTagsFromResource :: RemoveTagsFromResource -> TestTree
testRemoveTagsFromResource = undefined

testDescribeCacheParameters :: DescribeCacheParameters -> TestTree
testDescribeCacheParameters = undefined

testDescribeCacheSubnetGroups :: DescribeCacheSubnetGroups -> TestTree
testDescribeCacheSubnetGroups = undefined

testCreateCacheSecurityGroup :: CreateCacheSecurityGroup -> TestTree
testCreateCacheSecurityGroup = undefined

testAddTagsToResource :: AddTagsToResource -> TestTree
testAddTagsToResource = undefined

testAuthorizeCacheSecurityGroupIngress :: AuthorizeCacheSecurityGroupIngress -> TestTree
testAuthorizeCacheSecurityGroupIngress = undefined

testCopySnapshot :: CopySnapshot -> TestTree
testCopySnapshot = undefined

testCreateCacheSubnetGroup :: CreateCacheSubnetGroup -> TestTree
testCreateCacheSubnetGroup = undefined

testDescribeCacheParameterGroups :: DescribeCacheParameterGroups -> TestTree
testDescribeCacheParameterGroups = undefined

testResetCacheParameterGroup :: ResetCacheParameterGroup -> TestTree
testResetCacheParameterGroup = undefined

testDescribeSnapshots :: DescribeSnapshots -> TestTree
testDescribeSnapshots = undefined

testDescribeReservedCacheNodesOfferings :: DescribeReservedCacheNodesOfferings -> TestTree
testDescribeReservedCacheNodesOfferings = undefined

testDeleteSnapshot :: DeleteSnapshot -> TestTree
testDeleteSnapshot = undefined

testDescribeReplicationGroups :: DescribeReplicationGroups -> TestTree
testDescribeReplicationGroups = undefined

testModifyCacheSubnetGroup :: ModifyCacheSubnetGroup -> TestTree
testModifyCacheSubnetGroup = undefined

testCreateSnapshot :: CreateSnapshot -> TestTree
testCreateSnapshot = undefined

testDescribeCacheSecurityGroups :: DescribeCacheSecurityGroups -> TestTree
testDescribeCacheSecurityGroups = undefined

testDeleteCacheParameterGroup :: DeleteCacheParameterGroup -> TestTree
testDeleteCacheParameterGroup = undefined

testDescribeReservedCacheNodes :: DescribeReservedCacheNodes -> TestTree
testDescribeReservedCacheNodes = undefined

testDescribeCacheEngineVersions :: DescribeCacheEngineVersions -> TestTree
testDescribeCacheEngineVersions = undefined

testModifyCacheCluster :: ModifyCacheCluster -> TestTree
testModifyCacheCluster = undefined

testCreateCacheParameterGroup :: CreateCacheParameterGroup -> TestTree
testCreateCacheParameterGroup = undefined

testDeleteCacheSubnetGroup :: DeleteCacheSubnetGroup -> TestTree
testDeleteCacheSubnetGroup = undefined

-- Responses

testDeleteCacheSecurityGroupResponse :: DeleteCacheSecurityGroupResponse -> TestTree
testDeleteCacheSecurityGroupResponse = resp
    "DeleteCacheSecurityGroupResponse"
    "fixture/DeleteCacheSecurityGroupResponse"
    (Proxy :: Proxy DeleteCacheSecurityGroup)

testCreateReplicationGroupResponse :: CreateReplicationGroupResponse -> TestTree
testCreateReplicationGroupResponse = resp
    "CreateReplicationGroupResponse"
    "fixture/CreateReplicationGroupResponse"
    (Proxy :: Proxy CreateReplicationGroup)

testDeleteCacheClusterResponse :: DeleteCacheClusterResponse -> TestTree
testDeleteCacheClusterResponse = resp
    "DeleteCacheClusterResponse"
    "fixture/DeleteCacheClusterResponse"
    (Proxy :: Proxy DeleteCacheCluster)

testRebootCacheClusterResponse :: RebootCacheClusterResponse -> TestTree
testRebootCacheClusterResponse = resp
    "RebootCacheClusterResponse"
    "fixture/RebootCacheClusterResponse"
    (Proxy :: Proxy RebootCacheCluster)

testRevokeCacheSecurityGroupIngressResponse :: RevokeCacheSecurityGroupIngressResponse -> TestTree
testRevokeCacheSecurityGroupIngressResponse = resp
    "RevokeCacheSecurityGroupIngressResponse"
    "fixture/RevokeCacheSecurityGroupIngressResponse"
    (Proxy :: Proxy RevokeCacheSecurityGroupIngress)

testDescribeEventsResponse :: DescribeEventsResponse -> TestTree
testDescribeEventsResponse = resp
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse"
    (Proxy :: Proxy DescribeEvents)

testDescribeEngineDefaultParametersResponse :: DescribeEngineDefaultParametersResponse -> TestTree
testDescribeEngineDefaultParametersResponse = resp
    "DescribeEngineDefaultParametersResponse"
    "fixture/DescribeEngineDefaultParametersResponse"
    (Proxy :: Proxy DescribeEngineDefaultParameters)

testModifyCacheParameterGroupResponse :: CacheParameterGroupNameMessage -> TestTree
testModifyCacheParameterGroupResponse = resp
    "ModifyCacheParameterGroupResponse"
    "fixture/ModifyCacheParameterGroupResponse"
    (Proxy :: Proxy ModifyCacheParameterGroup)

testCreateCacheClusterResponse :: CreateCacheClusterResponse -> TestTree
testCreateCacheClusterResponse = resp
    "CreateCacheClusterResponse"
    "fixture/CreateCacheClusterResponse"
    (Proxy :: Proxy CreateCacheCluster)

testListTagsForResourceResponse :: TagListMessage -> TestTree
testListTagsForResourceResponse = resp
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse"
    (Proxy :: Proxy ListTagsForResource)

testDeleteReplicationGroupResponse :: DeleteReplicationGroupResponse -> TestTree
testDeleteReplicationGroupResponse = resp
    "DeleteReplicationGroupResponse"
    "fixture/DeleteReplicationGroupResponse"
    (Proxy :: Proxy DeleteReplicationGroup)

testPurchaseReservedCacheNodesOfferingResponse :: PurchaseReservedCacheNodesOfferingResponse -> TestTree
testPurchaseReservedCacheNodesOfferingResponse = resp
    "PurchaseReservedCacheNodesOfferingResponse"
    "fixture/PurchaseReservedCacheNodesOfferingResponse"
    (Proxy :: Proxy PurchaseReservedCacheNodesOffering)

testDescribeCacheClustersResponse :: DescribeCacheClustersResponse -> TestTree
testDescribeCacheClustersResponse = resp
    "DescribeCacheClustersResponse"
    "fixture/DescribeCacheClustersResponse"
    (Proxy :: Proxy DescribeCacheClusters)

testModifyReplicationGroupResponse :: ModifyReplicationGroupResponse -> TestTree
testModifyReplicationGroupResponse = resp
    "ModifyReplicationGroupResponse"
    "fixture/ModifyReplicationGroupResponse"
    (Proxy :: Proxy ModifyReplicationGroup)

testRemoveTagsFromResourceResponse :: TagListMessage -> TestTree
testRemoveTagsFromResourceResponse = resp
    "RemoveTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse"
    (Proxy :: Proxy RemoveTagsFromResource)

testDescribeCacheParametersResponse :: DescribeCacheParametersResponse -> TestTree
testDescribeCacheParametersResponse = resp
    "DescribeCacheParametersResponse"
    "fixture/DescribeCacheParametersResponse"
    (Proxy :: Proxy DescribeCacheParameters)

testDescribeCacheSubnetGroupsResponse :: DescribeCacheSubnetGroupsResponse -> TestTree
testDescribeCacheSubnetGroupsResponse = resp
    "DescribeCacheSubnetGroupsResponse"
    "fixture/DescribeCacheSubnetGroupsResponse"
    (Proxy :: Proxy DescribeCacheSubnetGroups)

testCreateCacheSecurityGroupResponse :: CreateCacheSecurityGroupResponse -> TestTree
testCreateCacheSecurityGroupResponse = resp
    "CreateCacheSecurityGroupResponse"
    "fixture/CreateCacheSecurityGroupResponse"
    (Proxy :: Proxy CreateCacheSecurityGroup)

testAddTagsToResourceResponse :: TagListMessage -> TestTree
testAddTagsToResourceResponse = resp
    "AddTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse"
    (Proxy :: Proxy AddTagsToResource)

testAuthorizeCacheSecurityGroupIngressResponse :: AuthorizeCacheSecurityGroupIngressResponse -> TestTree
testAuthorizeCacheSecurityGroupIngressResponse = resp
    "AuthorizeCacheSecurityGroupIngressResponse"
    "fixture/AuthorizeCacheSecurityGroupIngressResponse"
    (Proxy :: Proxy AuthorizeCacheSecurityGroupIngress)

testCopySnapshotResponse :: CopySnapshotResponse -> TestTree
testCopySnapshotResponse = resp
    "CopySnapshotResponse"
    "fixture/CopySnapshotResponse"
    (Proxy :: Proxy CopySnapshot)

testCreateCacheSubnetGroupResponse :: CreateCacheSubnetGroupResponse -> TestTree
testCreateCacheSubnetGroupResponse = resp
    "CreateCacheSubnetGroupResponse"
    "fixture/CreateCacheSubnetGroupResponse"
    (Proxy :: Proxy CreateCacheSubnetGroup)

testDescribeCacheParameterGroupsResponse :: DescribeCacheParameterGroupsResponse -> TestTree
testDescribeCacheParameterGroupsResponse = resp
    "DescribeCacheParameterGroupsResponse"
    "fixture/DescribeCacheParameterGroupsResponse"
    (Proxy :: Proxy DescribeCacheParameterGroups)

testResetCacheParameterGroupResponse :: CacheParameterGroupNameMessage -> TestTree
testResetCacheParameterGroupResponse = resp
    "ResetCacheParameterGroupResponse"
    "fixture/ResetCacheParameterGroupResponse"
    (Proxy :: Proxy ResetCacheParameterGroup)

testDescribeSnapshotsResponse :: DescribeSnapshotsResponse -> TestTree
testDescribeSnapshotsResponse = resp
    "DescribeSnapshotsResponse"
    "fixture/DescribeSnapshotsResponse"
    (Proxy :: Proxy DescribeSnapshots)

testDescribeReservedCacheNodesOfferingsResponse :: DescribeReservedCacheNodesOfferingsResponse -> TestTree
testDescribeReservedCacheNodesOfferingsResponse = resp
    "DescribeReservedCacheNodesOfferingsResponse"
    "fixture/DescribeReservedCacheNodesOfferingsResponse"
    (Proxy :: Proxy DescribeReservedCacheNodesOfferings)

testDeleteSnapshotResponse :: DeleteSnapshotResponse -> TestTree
testDeleteSnapshotResponse = resp
    "DeleteSnapshotResponse"
    "fixture/DeleteSnapshotResponse"
    (Proxy :: Proxy DeleteSnapshot)

testDescribeReplicationGroupsResponse :: DescribeReplicationGroupsResponse -> TestTree
testDescribeReplicationGroupsResponse = resp
    "DescribeReplicationGroupsResponse"
    "fixture/DescribeReplicationGroupsResponse"
    (Proxy :: Proxy DescribeReplicationGroups)

testModifyCacheSubnetGroupResponse :: ModifyCacheSubnetGroupResponse -> TestTree
testModifyCacheSubnetGroupResponse = resp
    "ModifyCacheSubnetGroupResponse"
    "fixture/ModifyCacheSubnetGroupResponse"
    (Proxy :: Proxy ModifyCacheSubnetGroup)

testCreateSnapshotResponse :: CreateSnapshotResponse -> TestTree
testCreateSnapshotResponse = resp
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse"
    (Proxy :: Proxy CreateSnapshot)

testDescribeCacheSecurityGroupsResponse :: DescribeCacheSecurityGroupsResponse -> TestTree
testDescribeCacheSecurityGroupsResponse = resp
    "DescribeCacheSecurityGroupsResponse"
    "fixture/DescribeCacheSecurityGroupsResponse"
    (Proxy :: Proxy DescribeCacheSecurityGroups)

testDeleteCacheParameterGroupResponse :: DeleteCacheParameterGroupResponse -> TestTree
testDeleteCacheParameterGroupResponse = resp
    "DeleteCacheParameterGroupResponse"
    "fixture/DeleteCacheParameterGroupResponse"
    (Proxy :: Proxy DeleteCacheParameterGroup)

testDescribeReservedCacheNodesResponse :: DescribeReservedCacheNodesResponse -> TestTree
testDescribeReservedCacheNodesResponse = resp
    "DescribeReservedCacheNodesResponse"
    "fixture/DescribeReservedCacheNodesResponse"
    (Proxy :: Proxy DescribeReservedCacheNodes)

testDescribeCacheEngineVersionsResponse :: DescribeCacheEngineVersionsResponse -> TestTree
testDescribeCacheEngineVersionsResponse = resp
    "DescribeCacheEngineVersionsResponse"
    "fixture/DescribeCacheEngineVersionsResponse"
    (Proxy :: Proxy DescribeCacheEngineVersions)

testModifyCacheClusterResponse :: ModifyCacheClusterResponse -> TestTree
testModifyCacheClusterResponse = resp
    "ModifyCacheClusterResponse"
    "fixture/ModifyCacheClusterResponse"
    (Proxy :: Proxy ModifyCacheCluster)

testCreateCacheParameterGroupResponse :: CreateCacheParameterGroupResponse -> TestTree
testCreateCacheParameterGroupResponse = resp
    "CreateCacheParameterGroupResponse"
    "fixture/CreateCacheParameterGroupResponse"
    (Proxy :: Proxy CreateCacheParameterGroup)

testDeleteCacheSubnetGroupResponse :: DeleteCacheSubnetGroupResponse -> TestTree
testDeleteCacheSubnetGroupResponse = resp
    "DeleteCacheSubnetGroupResponse"
    "fixture/DeleteCacheSubnetGroupResponse"
    (Proxy :: Proxy DeleteCacheSubnetGroup)

instance Out AZMode
instance Out AddTagsToResource
instance Out AuthorizeCacheSecurityGroupIngress
instance Out AuthorizeCacheSecurityGroupIngressResponse
instance Out AutomaticFailoverStatus
instance Out AvailabilityZone
instance Out CacheCluster
instance Out CacheEngineVersion
instance Out CacheNode
instance Out CacheNodeTypeSpecificParameter
instance Out CacheNodeTypeSpecificValue
instance Out CacheParameterGroup
instance Out CacheParameterGroupNameMessage
instance Out CacheParameterGroupStatus
instance Out CacheSecurityGroup
instance Out CacheSecurityGroupMembership
instance Out CacheSubnetGroup
instance Out CopySnapshot
instance Out CopySnapshotResponse
instance Out CreateCacheCluster
instance Out CreateCacheClusterResponse
instance Out CreateCacheParameterGroup
instance Out CreateCacheParameterGroupResponse
instance Out CreateCacheSecurityGroup
instance Out CreateCacheSecurityGroupResponse
instance Out CreateCacheSubnetGroup
instance Out CreateCacheSubnetGroupResponse
instance Out CreateReplicationGroup
instance Out CreateReplicationGroupResponse
instance Out CreateSnapshot
instance Out CreateSnapshotResponse
instance Out DeleteCacheCluster
instance Out DeleteCacheClusterResponse
instance Out DeleteCacheParameterGroup
instance Out DeleteCacheParameterGroupResponse
instance Out DeleteCacheSecurityGroup
instance Out DeleteCacheSecurityGroupResponse
instance Out DeleteCacheSubnetGroup
instance Out DeleteCacheSubnetGroupResponse
instance Out DeleteReplicationGroup
instance Out DeleteReplicationGroupResponse
instance Out DeleteSnapshot
instance Out DeleteSnapshotResponse
instance Out DescribeCacheClusters
instance Out DescribeCacheClustersResponse
instance Out DescribeCacheEngineVersions
instance Out DescribeCacheEngineVersionsResponse
instance Out DescribeCacheParameterGroups
instance Out DescribeCacheParameterGroupsResponse
instance Out DescribeCacheParameters
instance Out DescribeCacheParametersResponse
instance Out DescribeCacheSecurityGroups
instance Out DescribeCacheSecurityGroupsResponse
instance Out DescribeCacheSubnetGroups
instance Out DescribeCacheSubnetGroupsResponse
instance Out DescribeEngineDefaultParameters
instance Out DescribeEngineDefaultParametersResponse
instance Out DescribeEvents
instance Out DescribeEventsResponse
instance Out DescribeReplicationGroups
instance Out DescribeReplicationGroupsResponse
instance Out DescribeReservedCacheNodes
instance Out DescribeReservedCacheNodesOfferings
instance Out DescribeReservedCacheNodesOfferingsResponse
instance Out DescribeReservedCacheNodesResponse
instance Out DescribeSnapshots
instance Out DescribeSnapshotsResponse
instance Out EC2SecurityGroup
instance Out Endpoint
instance Out EngineDefaults
instance Out Event
instance Out ListTagsForResource
instance Out ModifyCacheCluster
instance Out ModifyCacheClusterResponse
instance Out ModifyCacheParameterGroup
instance Out ModifyCacheSubnetGroup
instance Out ModifyCacheSubnetGroupResponse
instance Out ModifyReplicationGroup
instance Out ModifyReplicationGroupResponse
instance Out NodeGroup
instance Out NodeGroupMember
instance Out NodeSnapshot
instance Out NotificationConfiguration
instance Out Parameter
instance Out ParameterNameValue
instance Out PendingAutomaticFailoverStatus
instance Out PendingModifiedValues
instance Out PurchaseReservedCacheNodesOffering
instance Out PurchaseReservedCacheNodesOfferingResponse
instance Out RebootCacheCluster
instance Out RebootCacheClusterResponse
instance Out RecurringCharge
instance Out RemoveTagsFromResource
instance Out ReplicationGroup
instance Out ReplicationGroupPendingModifiedValues
instance Out ReservedCacheNode
instance Out ReservedCacheNodesOffering
instance Out ResetCacheParameterGroup
instance Out RevokeCacheSecurityGroupIngress
instance Out RevokeCacheSecurityGroupIngressResponse
instance Out SecurityGroupMembership
instance Out Snapshot
instance Out SourceType
instance Out Subnet
instance Out Tag
instance Out TagListMessage
