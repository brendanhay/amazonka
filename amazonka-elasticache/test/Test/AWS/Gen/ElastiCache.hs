{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ElastiCache
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.ElastiCache where

import Data.Proxy
import Network.AWS.ElastiCache
import Test.AWS.ElastiCache.Internal
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
--         [ requestDeleteCacheSecurityGroup $
--             deleteCacheSecurityGroup
--
--         , requestCreateReplicationGroup $
--             createReplicationGroup
--
--         , requestDeleteCacheCluster $
--             deleteCacheCluster
--
--         , requestRebootCacheCluster $
--             rebootCacheCluster
--
--         , requestRevokeCacheSecurityGroupIngress $
--             revokeCacheSecurityGroupIngress
--
--         , requestCreateCacheCluster $
--             createCacheCluster
--
--         , requestDescribeEvents $
--             describeEvents
--
--         , requestDescribeEngineDefaultParameters $
--             describeEngineDefaultParameters
--
--         , requestModifyCacheParameterGroup $
--             modifyCacheParameterGroup
--
--         , requestTestFailover $
--             testFailover
--
--         , requestDeleteReplicationGroup $
--             deleteReplicationGroup
--
--         , requestListTagsForResource $
--             listTagsForResource
--
--         , requestDescribeCacheClusters $
--             describeCacheClusters
--
--         , requestPurchaseReservedCacheNodesOffering $
--             purchaseReservedCacheNodesOffering
--
--         , requestRemoveTagsFromResource $
--             removeTagsFromResource
--
--         , requestModifyReplicationGroup $
--             modifyReplicationGroup
--
--         , requestDescribeCacheParameters $
--             describeCacheParameters
--
--         , requestDescribeCacheSubnetGroups $
--             describeCacheSubnetGroups
--
--         , requestCreateCacheSecurityGroup $
--             createCacheSecurityGroup
--
--         , requestAddTagsToResource $
--             addTagsToResource
--
--         , requestAuthorizeCacheSecurityGroupIngress $
--             authorizeCacheSecurityGroupIngress
--
--         , requestCopySnapshot $
--             copySnapshot
--
--         , requestCreateCacheSubnetGroup $
--             createCacheSubnetGroup
--
--         , requestDescribeCacheParameterGroups $
--             describeCacheParameterGroups
--
--         , requestResetCacheParameterGroup $
--             resetCacheParameterGroup
--
--         , requestListAllowedNodeTypeModifications $
--             listAllowedNodeTypeModifications
--
--         , requestModifyReplicationGroupShardConfiguration $
--             modifyReplicationGroupShardConfiguration
--
--         , requestDescribeSnapshots $
--             describeSnapshots
--
--         , requestDescribeReplicationGroups $
--             describeReplicationGroups
--
--         , requestDeleteSnapshot $
--             deleteSnapshot
--
--         , requestDescribeReservedCacheNodesOfferings $
--             describeReservedCacheNodesOfferings
--
--         , requestModifyCacheSubnetGroup $
--             modifyCacheSubnetGroup
--
--         , requestCreateSnapshot $
--             createSnapshot
--
--         , requestDeleteCacheParameterGroup $
--             deleteCacheParameterGroup
--
--         , requestDescribeCacheSecurityGroups $
--             describeCacheSecurityGroups
--
--         , requestModifyCacheCluster $
--             modifyCacheCluster
--
--         , requestDescribeCacheEngineVersions $
--             describeCacheEngineVersions
--
--         , requestCreateCacheParameterGroup $
--             createCacheParameterGroup
--
--         , requestDescribeReservedCacheNodes $
--             describeReservedCacheNodes
--
--         , requestDeleteCacheSubnetGroup $
--             deleteCacheSubnetGroup
--
--           ]

--     , testGroup "response"
--         [ responseDeleteCacheSecurityGroup $
--             deleteCacheSecurityGroupResponse
--
--         , responseCreateReplicationGroup $
--             createReplicationGroupResponse
--
--         , responseDeleteCacheCluster $
--             deleteCacheClusterResponse
--
--         , responseRebootCacheCluster $
--             rebootCacheClusterResponse
--
--         , responseRevokeCacheSecurityGroupIngress $
--             revokeCacheSecurityGroupIngressResponse
--
--         , responseCreateCacheCluster $
--             createCacheClusterResponse
--
--         , responseDescribeEvents $
--             describeEventsResponse
--
--         , responseDescribeEngineDefaultParameters $
--             describeEngineDefaultParametersResponse
--
--         , responseModifyCacheParameterGroup $
--             cacheParameterGroupNameMessage
--
--         , responseTestFailover $
--             testFailoverResponse
--
--         , responseDeleteReplicationGroup $
--             deleteReplicationGroupResponse
--
--         , responseListTagsForResource $
--             tagListMessage
--
--         , responseDescribeCacheClusters $
--             describeCacheClustersResponse
--
--         , responsePurchaseReservedCacheNodesOffering $
--             purchaseReservedCacheNodesOfferingResponse
--
--         , responseRemoveTagsFromResource $
--             tagListMessage
--
--         , responseModifyReplicationGroup $
--             modifyReplicationGroupResponse
--
--         , responseDescribeCacheParameters $
--             describeCacheParametersResponse
--
--         , responseDescribeCacheSubnetGroups $
--             describeCacheSubnetGroupsResponse
--
--         , responseCreateCacheSecurityGroup $
--             createCacheSecurityGroupResponse
--
--         , responseAddTagsToResource $
--             tagListMessage
--
--         , responseAuthorizeCacheSecurityGroupIngress $
--             authorizeCacheSecurityGroupIngressResponse
--
--         , responseCopySnapshot $
--             copySnapshotResponse
--
--         , responseCreateCacheSubnetGroup $
--             createCacheSubnetGroupResponse
--
--         , responseDescribeCacheParameterGroups $
--             describeCacheParameterGroupsResponse
--
--         , responseResetCacheParameterGroup $
--             cacheParameterGroupNameMessage
--
--         , responseListAllowedNodeTypeModifications $
--             listAllowedNodeTypeModificationsResponse
--
--         , responseModifyReplicationGroupShardConfiguration $
--             modifyReplicationGroupShardConfigurationResponse
--
--         , responseDescribeSnapshots $
--             describeSnapshotsResponse
--
--         , responseDescribeReplicationGroups $
--             describeReplicationGroupsResponse
--
--         , responseDeleteSnapshot $
--             deleteSnapshotResponse
--
--         , responseDescribeReservedCacheNodesOfferings $
--             describeReservedCacheNodesOfferingsResponse
--
--         , responseModifyCacheSubnetGroup $
--             modifyCacheSubnetGroupResponse
--
--         , responseCreateSnapshot $
--             createSnapshotResponse
--
--         , responseDeleteCacheParameterGroup $
--             deleteCacheParameterGroupResponse
--
--         , responseDescribeCacheSecurityGroups $
--             describeCacheSecurityGroupsResponse
--
--         , responseModifyCacheCluster $
--             modifyCacheClusterResponse
--
--         , responseDescribeCacheEngineVersions $
--             describeCacheEngineVersionsResponse
--
--         , responseCreateCacheParameterGroup $
--             createCacheParameterGroupResponse
--
--         , responseDescribeReservedCacheNodes $
--             describeReservedCacheNodesResponse
--
--         , responseDeleteCacheSubnetGroup $
--             deleteCacheSubnetGroupResponse
--
--           ]
--     ]

-- Requests

requestDeleteCacheSecurityGroup :: DeleteCacheSecurityGroup -> TestTree
requestDeleteCacheSecurityGroup = req
    "DeleteCacheSecurityGroup"
    "fixture/DeleteCacheSecurityGroup.yaml"

requestCreateReplicationGroup :: CreateReplicationGroup -> TestTree
requestCreateReplicationGroup = req
    "CreateReplicationGroup"
    "fixture/CreateReplicationGroup.yaml"

requestDeleteCacheCluster :: DeleteCacheCluster -> TestTree
requestDeleteCacheCluster = req
    "DeleteCacheCluster"
    "fixture/DeleteCacheCluster.yaml"

requestRebootCacheCluster :: RebootCacheCluster -> TestTree
requestRebootCacheCluster = req
    "RebootCacheCluster"
    "fixture/RebootCacheCluster.yaml"

requestRevokeCacheSecurityGroupIngress :: RevokeCacheSecurityGroupIngress -> TestTree
requestRevokeCacheSecurityGroupIngress = req
    "RevokeCacheSecurityGroupIngress"
    "fixture/RevokeCacheSecurityGroupIngress.yaml"

requestCreateCacheCluster :: CreateCacheCluster -> TestTree
requestCreateCacheCluster = req
    "CreateCacheCluster"
    "fixture/CreateCacheCluster.yaml"

requestDescribeEvents :: DescribeEvents -> TestTree
requestDescribeEvents = req
    "DescribeEvents"
    "fixture/DescribeEvents.yaml"

requestDescribeEngineDefaultParameters :: DescribeEngineDefaultParameters -> TestTree
requestDescribeEngineDefaultParameters = req
    "DescribeEngineDefaultParameters"
    "fixture/DescribeEngineDefaultParameters.yaml"

requestModifyCacheParameterGroup :: ModifyCacheParameterGroup -> TestTree
requestModifyCacheParameterGroup = req
    "ModifyCacheParameterGroup"
    "fixture/ModifyCacheParameterGroup.yaml"

requestTestFailover :: TestFailover -> TestTree
requestTestFailover = req
    "TestFailover"
    "fixture/TestFailover.yaml"

requestDeleteReplicationGroup :: DeleteReplicationGroup -> TestTree
requestDeleteReplicationGroup = req
    "DeleteReplicationGroup"
    "fixture/DeleteReplicationGroup.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource = req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestDescribeCacheClusters :: DescribeCacheClusters -> TestTree
requestDescribeCacheClusters = req
    "DescribeCacheClusters"
    "fixture/DescribeCacheClusters.yaml"

requestPurchaseReservedCacheNodesOffering :: PurchaseReservedCacheNodesOffering -> TestTree
requestPurchaseReservedCacheNodesOffering = req
    "PurchaseReservedCacheNodesOffering"
    "fixture/PurchaseReservedCacheNodesOffering.yaml"

requestRemoveTagsFromResource :: RemoveTagsFromResource -> TestTree
requestRemoveTagsFromResource = req
    "RemoveTagsFromResource"
    "fixture/RemoveTagsFromResource.yaml"

requestModifyReplicationGroup :: ModifyReplicationGroup -> TestTree
requestModifyReplicationGroup = req
    "ModifyReplicationGroup"
    "fixture/ModifyReplicationGroup.yaml"

requestDescribeCacheParameters :: DescribeCacheParameters -> TestTree
requestDescribeCacheParameters = req
    "DescribeCacheParameters"
    "fixture/DescribeCacheParameters.yaml"

requestDescribeCacheSubnetGroups :: DescribeCacheSubnetGroups -> TestTree
requestDescribeCacheSubnetGroups = req
    "DescribeCacheSubnetGroups"
    "fixture/DescribeCacheSubnetGroups.yaml"

requestCreateCacheSecurityGroup :: CreateCacheSecurityGroup -> TestTree
requestCreateCacheSecurityGroup = req
    "CreateCacheSecurityGroup"
    "fixture/CreateCacheSecurityGroup.yaml"

requestAddTagsToResource :: AddTagsToResource -> TestTree
requestAddTagsToResource = req
    "AddTagsToResource"
    "fixture/AddTagsToResource.yaml"

requestAuthorizeCacheSecurityGroupIngress :: AuthorizeCacheSecurityGroupIngress -> TestTree
requestAuthorizeCacheSecurityGroupIngress = req
    "AuthorizeCacheSecurityGroupIngress"
    "fixture/AuthorizeCacheSecurityGroupIngress.yaml"

requestCopySnapshot :: CopySnapshot -> TestTree
requestCopySnapshot = req
    "CopySnapshot"
    "fixture/CopySnapshot.yaml"

requestCreateCacheSubnetGroup :: CreateCacheSubnetGroup -> TestTree
requestCreateCacheSubnetGroup = req
    "CreateCacheSubnetGroup"
    "fixture/CreateCacheSubnetGroup.yaml"

requestDescribeCacheParameterGroups :: DescribeCacheParameterGroups -> TestTree
requestDescribeCacheParameterGroups = req
    "DescribeCacheParameterGroups"
    "fixture/DescribeCacheParameterGroups.yaml"

requestResetCacheParameterGroup :: ResetCacheParameterGroup -> TestTree
requestResetCacheParameterGroup = req
    "ResetCacheParameterGroup"
    "fixture/ResetCacheParameterGroup.yaml"

requestListAllowedNodeTypeModifications :: ListAllowedNodeTypeModifications -> TestTree
requestListAllowedNodeTypeModifications = req
    "ListAllowedNodeTypeModifications"
    "fixture/ListAllowedNodeTypeModifications.yaml"

requestModifyReplicationGroupShardConfiguration :: ModifyReplicationGroupShardConfiguration -> TestTree
requestModifyReplicationGroupShardConfiguration = req
    "ModifyReplicationGroupShardConfiguration"
    "fixture/ModifyReplicationGroupShardConfiguration.yaml"

requestDescribeSnapshots :: DescribeSnapshots -> TestTree
requestDescribeSnapshots = req
    "DescribeSnapshots"
    "fixture/DescribeSnapshots.yaml"

requestDescribeReplicationGroups :: DescribeReplicationGroups -> TestTree
requestDescribeReplicationGroups = req
    "DescribeReplicationGroups"
    "fixture/DescribeReplicationGroups.yaml"

requestDeleteSnapshot :: DeleteSnapshot -> TestTree
requestDeleteSnapshot = req
    "DeleteSnapshot"
    "fixture/DeleteSnapshot.yaml"

requestDescribeReservedCacheNodesOfferings :: DescribeReservedCacheNodesOfferings -> TestTree
requestDescribeReservedCacheNodesOfferings = req
    "DescribeReservedCacheNodesOfferings"
    "fixture/DescribeReservedCacheNodesOfferings.yaml"

requestModifyCacheSubnetGroup :: ModifyCacheSubnetGroup -> TestTree
requestModifyCacheSubnetGroup = req
    "ModifyCacheSubnetGroup"
    "fixture/ModifyCacheSubnetGroup.yaml"

requestCreateSnapshot :: CreateSnapshot -> TestTree
requestCreateSnapshot = req
    "CreateSnapshot"
    "fixture/CreateSnapshot.yaml"

requestDeleteCacheParameterGroup :: DeleteCacheParameterGroup -> TestTree
requestDeleteCacheParameterGroup = req
    "DeleteCacheParameterGroup"
    "fixture/DeleteCacheParameterGroup.yaml"

requestDescribeCacheSecurityGroups :: DescribeCacheSecurityGroups -> TestTree
requestDescribeCacheSecurityGroups = req
    "DescribeCacheSecurityGroups"
    "fixture/DescribeCacheSecurityGroups.yaml"

requestModifyCacheCluster :: ModifyCacheCluster -> TestTree
requestModifyCacheCluster = req
    "ModifyCacheCluster"
    "fixture/ModifyCacheCluster.yaml"

requestDescribeCacheEngineVersions :: DescribeCacheEngineVersions -> TestTree
requestDescribeCacheEngineVersions = req
    "DescribeCacheEngineVersions"
    "fixture/DescribeCacheEngineVersions.yaml"

requestCreateCacheParameterGroup :: CreateCacheParameterGroup -> TestTree
requestCreateCacheParameterGroup = req
    "CreateCacheParameterGroup"
    "fixture/CreateCacheParameterGroup.yaml"

requestDescribeReservedCacheNodes :: DescribeReservedCacheNodes -> TestTree
requestDescribeReservedCacheNodes = req
    "DescribeReservedCacheNodes"
    "fixture/DescribeReservedCacheNodes.yaml"

requestDeleteCacheSubnetGroup :: DeleteCacheSubnetGroup -> TestTree
requestDeleteCacheSubnetGroup = req
    "DeleteCacheSubnetGroup"
    "fixture/DeleteCacheSubnetGroup.yaml"

-- Responses

responseDeleteCacheSecurityGroup :: DeleteCacheSecurityGroupResponse -> TestTree
responseDeleteCacheSecurityGroup = res
    "DeleteCacheSecurityGroupResponse"
    "fixture/DeleteCacheSecurityGroupResponse.proto"
    elastiCache
    (Proxy :: Proxy DeleteCacheSecurityGroup)

responseCreateReplicationGroup :: CreateReplicationGroupResponse -> TestTree
responseCreateReplicationGroup = res
    "CreateReplicationGroupResponse"
    "fixture/CreateReplicationGroupResponse.proto"
    elastiCache
    (Proxy :: Proxy CreateReplicationGroup)

responseDeleteCacheCluster :: DeleteCacheClusterResponse -> TestTree
responseDeleteCacheCluster = res
    "DeleteCacheClusterResponse"
    "fixture/DeleteCacheClusterResponse.proto"
    elastiCache
    (Proxy :: Proxy DeleteCacheCluster)

responseRebootCacheCluster :: RebootCacheClusterResponse -> TestTree
responseRebootCacheCluster = res
    "RebootCacheClusterResponse"
    "fixture/RebootCacheClusterResponse.proto"
    elastiCache
    (Proxy :: Proxy RebootCacheCluster)

responseRevokeCacheSecurityGroupIngress :: RevokeCacheSecurityGroupIngressResponse -> TestTree
responseRevokeCacheSecurityGroupIngress = res
    "RevokeCacheSecurityGroupIngressResponse"
    "fixture/RevokeCacheSecurityGroupIngressResponse.proto"
    elastiCache
    (Proxy :: Proxy RevokeCacheSecurityGroupIngress)

responseCreateCacheCluster :: CreateCacheClusterResponse -> TestTree
responseCreateCacheCluster = res
    "CreateCacheClusterResponse"
    "fixture/CreateCacheClusterResponse.proto"
    elastiCache
    (Proxy :: Proxy CreateCacheCluster)

responseDescribeEvents :: DescribeEventsResponse -> TestTree
responseDescribeEvents = res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse.proto"
    elastiCache
    (Proxy :: Proxy DescribeEvents)

responseDescribeEngineDefaultParameters :: DescribeEngineDefaultParametersResponse -> TestTree
responseDescribeEngineDefaultParameters = res
    "DescribeEngineDefaultParametersResponse"
    "fixture/DescribeEngineDefaultParametersResponse.proto"
    elastiCache
    (Proxy :: Proxy DescribeEngineDefaultParameters)

responseModifyCacheParameterGroup :: CacheParameterGroupNameMessage -> TestTree
responseModifyCacheParameterGroup = res
    "ModifyCacheParameterGroupResponse"
    "fixture/ModifyCacheParameterGroupResponse.proto"
    elastiCache
    (Proxy :: Proxy ModifyCacheParameterGroup)

responseTestFailover :: TestFailoverResponse -> TestTree
responseTestFailover = res
    "TestFailoverResponse"
    "fixture/TestFailoverResponse.proto"
    elastiCache
    (Proxy :: Proxy TestFailover)

responseDeleteReplicationGroup :: DeleteReplicationGroupResponse -> TestTree
responseDeleteReplicationGroup = res
    "DeleteReplicationGroupResponse"
    "fixture/DeleteReplicationGroupResponse.proto"
    elastiCache
    (Proxy :: Proxy DeleteReplicationGroup)

responseListTagsForResource :: TagListMessage -> TestTree
responseListTagsForResource = res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    elastiCache
    (Proxy :: Proxy ListTagsForResource)

responseDescribeCacheClusters :: DescribeCacheClustersResponse -> TestTree
responseDescribeCacheClusters = res
    "DescribeCacheClustersResponse"
    "fixture/DescribeCacheClustersResponse.proto"
    elastiCache
    (Proxy :: Proxy DescribeCacheClusters)

responsePurchaseReservedCacheNodesOffering :: PurchaseReservedCacheNodesOfferingResponse -> TestTree
responsePurchaseReservedCacheNodesOffering = res
    "PurchaseReservedCacheNodesOfferingResponse"
    "fixture/PurchaseReservedCacheNodesOfferingResponse.proto"
    elastiCache
    (Proxy :: Proxy PurchaseReservedCacheNodesOffering)

responseRemoveTagsFromResource :: TagListMessage -> TestTree
responseRemoveTagsFromResource = res
    "RemoveTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse.proto"
    elastiCache
    (Proxy :: Proxy RemoveTagsFromResource)

responseModifyReplicationGroup :: ModifyReplicationGroupResponse -> TestTree
responseModifyReplicationGroup = res
    "ModifyReplicationGroupResponse"
    "fixture/ModifyReplicationGroupResponse.proto"
    elastiCache
    (Proxy :: Proxy ModifyReplicationGroup)

responseDescribeCacheParameters :: DescribeCacheParametersResponse -> TestTree
responseDescribeCacheParameters = res
    "DescribeCacheParametersResponse"
    "fixture/DescribeCacheParametersResponse.proto"
    elastiCache
    (Proxy :: Proxy DescribeCacheParameters)

responseDescribeCacheSubnetGroups :: DescribeCacheSubnetGroupsResponse -> TestTree
responseDescribeCacheSubnetGroups = res
    "DescribeCacheSubnetGroupsResponse"
    "fixture/DescribeCacheSubnetGroupsResponse.proto"
    elastiCache
    (Proxy :: Proxy DescribeCacheSubnetGroups)

responseCreateCacheSecurityGroup :: CreateCacheSecurityGroupResponse -> TestTree
responseCreateCacheSecurityGroup = res
    "CreateCacheSecurityGroupResponse"
    "fixture/CreateCacheSecurityGroupResponse.proto"
    elastiCache
    (Proxy :: Proxy CreateCacheSecurityGroup)

responseAddTagsToResource :: TagListMessage -> TestTree
responseAddTagsToResource = res
    "AddTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse.proto"
    elastiCache
    (Proxy :: Proxy AddTagsToResource)

responseAuthorizeCacheSecurityGroupIngress :: AuthorizeCacheSecurityGroupIngressResponse -> TestTree
responseAuthorizeCacheSecurityGroupIngress = res
    "AuthorizeCacheSecurityGroupIngressResponse"
    "fixture/AuthorizeCacheSecurityGroupIngressResponse.proto"
    elastiCache
    (Proxy :: Proxy AuthorizeCacheSecurityGroupIngress)

responseCopySnapshot :: CopySnapshotResponse -> TestTree
responseCopySnapshot = res
    "CopySnapshotResponse"
    "fixture/CopySnapshotResponse.proto"
    elastiCache
    (Proxy :: Proxy CopySnapshot)

responseCreateCacheSubnetGroup :: CreateCacheSubnetGroupResponse -> TestTree
responseCreateCacheSubnetGroup = res
    "CreateCacheSubnetGroupResponse"
    "fixture/CreateCacheSubnetGroupResponse.proto"
    elastiCache
    (Proxy :: Proxy CreateCacheSubnetGroup)

responseDescribeCacheParameterGroups :: DescribeCacheParameterGroupsResponse -> TestTree
responseDescribeCacheParameterGroups = res
    "DescribeCacheParameterGroupsResponse"
    "fixture/DescribeCacheParameterGroupsResponse.proto"
    elastiCache
    (Proxy :: Proxy DescribeCacheParameterGroups)

responseResetCacheParameterGroup :: CacheParameterGroupNameMessage -> TestTree
responseResetCacheParameterGroup = res
    "ResetCacheParameterGroupResponse"
    "fixture/ResetCacheParameterGroupResponse.proto"
    elastiCache
    (Proxy :: Proxy ResetCacheParameterGroup)

responseListAllowedNodeTypeModifications :: ListAllowedNodeTypeModificationsResponse -> TestTree
responseListAllowedNodeTypeModifications = res
    "ListAllowedNodeTypeModificationsResponse"
    "fixture/ListAllowedNodeTypeModificationsResponse.proto"
    elastiCache
    (Proxy :: Proxy ListAllowedNodeTypeModifications)

responseModifyReplicationGroupShardConfiguration :: ModifyReplicationGroupShardConfigurationResponse -> TestTree
responseModifyReplicationGroupShardConfiguration = res
    "ModifyReplicationGroupShardConfigurationResponse"
    "fixture/ModifyReplicationGroupShardConfigurationResponse.proto"
    elastiCache
    (Proxy :: Proxy ModifyReplicationGroupShardConfiguration)

responseDescribeSnapshots :: DescribeSnapshotsResponse -> TestTree
responseDescribeSnapshots = res
    "DescribeSnapshotsResponse"
    "fixture/DescribeSnapshotsResponse.proto"
    elastiCache
    (Proxy :: Proxy DescribeSnapshots)

responseDescribeReplicationGroups :: DescribeReplicationGroupsResponse -> TestTree
responseDescribeReplicationGroups = res
    "DescribeReplicationGroupsResponse"
    "fixture/DescribeReplicationGroupsResponse.proto"
    elastiCache
    (Proxy :: Proxy DescribeReplicationGroups)

responseDeleteSnapshot :: DeleteSnapshotResponse -> TestTree
responseDeleteSnapshot = res
    "DeleteSnapshotResponse"
    "fixture/DeleteSnapshotResponse.proto"
    elastiCache
    (Proxy :: Proxy DeleteSnapshot)

responseDescribeReservedCacheNodesOfferings :: DescribeReservedCacheNodesOfferingsResponse -> TestTree
responseDescribeReservedCacheNodesOfferings = res
    "DescribeReservedCacheNodesOfferingsResponse"
    "fixture/DescribeReservedCacheNodesOfferingsResponse.proto"
    elastiCache
    (Proxy :: Proxy DescribeReservedCacheNodesOfferings)

responseModifyCacheSubnetGroup :: ModifyCacheSubnetGroupResponse -> TestTree
responseModifyCacheSubnetGroup = res
    "ModifyCacheSubnetGroupResponse"
    "fixture/ModifyCacheSubnetGroupResponse.proto"
    elastiCache
    (Proxy :: Proxy ModifyCacheSubnetGroup)

responseCreateSnapshot :: CreateSnapshotResponse -> TestTree
responseCreateSnapshot = res
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse.proto"
    elastiCache
    (Proxy :: Proxy CreateSnapshot)

responseDeleteCacheParameterGroup :: DeleteCacheParameterGroupResponse -> TestTree
responseDeleteCacheParameterGroup = res
    "DeleteCacheParameterGroupResponse"
    "fixture/DeleteCacheParameterGroupResponse.proto"
    elastiCache
    (Proxy :: Proxy DeleteCacheParameterGroup)

responseDescribeCacheSecurityGroups :: DescribeCacheSecurityGroupsResponse -> TestTree
responseDescribeCacheSecurityGroups = res
    "DescribeCacheSecurityGroupsResponse"
    "fixture/DescribeCacheSecurityGroupsResponse.proto"
    elastiCache
    (Proxy :: Proxy DescribeCacheSecurityGroups)

responseModifyCacheCluster :: ModifyCacheClusterResponse -> TestTree
responseModifyCacheCluster = res
    "ModifyCacheClusterResponse"
    "fixture/ModifyCacheClusterResponse.proto"
    elastiCache
    (Proxy :: Proxy ModifyCacheCluster)

responseDescribeCacheEngineVersions :: DescribeCacheEngineVersionsResponse -> TestTree
responseDescribeCacheEngineVersions = res
    "DescribeCacheEngineVersionsResponse"
    "fixture/DescribeCacheEngineVersionsResponse.proto"
    elastiCache
    (Proxy :: Proxy DescribeCacheEngineVersions)

responseCreateCacheParameterGroup :: CreateCacheParameterGroupResponse -> TestTree
responseCreateCacheParameterGroup = res
    "CreateCacheParameterGroupResponse"
    "fixture/CreateCacheParameterGroupResponse.proto"
    elastiCache
    (Proxy :: Proxy CreateCacheParameterGroup)

responseDescribeReservedCacheNodes :: DescribeReservedCacheNodesResponse -> TestTree
responseDescribeReservedCacheNodes = res
    "DescribeReservedCacheNodesResponse"
    "fixture/DescribeReservedCacheNodesResponse.proto"
    elastiCache
    (Proxy :: Proxy DescribeReservedCacheNodes)

responseDeleteCacheSubnetGroup :: DeleteCacheSubnetGroupResponse -> TestTree
responseDeleteCacheSubnetGroup = res
    "DeleteCacheSubnetGroupResponse"
    "fixture/DeleteCacheSubnetGroupResponse.proto"
    elastiCache
    (Proxy :: Proxy DeleteCacheSubnetGroup)
