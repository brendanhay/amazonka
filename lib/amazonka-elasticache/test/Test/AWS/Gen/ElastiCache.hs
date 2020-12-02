{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ElastiCache
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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
--         [ requestStartMigration $
--             startMigration
--
--         , requestDeleteCacheSecurityGroup $
--             deleteCacheSecurityGroup
--
--         , requestCreateReplicationGroup $
--             createReplicationGroup
--
--         , requestDeleteCacheCluster $
--             deleteCacheCluster
--
--         , requestIncreaseNodeGroupsInGlobalReplicationGroup $
--             increaseNodeGroupsInGlobalReplicationGroup
--
--         , requestDescribeUsers $
--             describeUsers
--
--         , requestRebootCacheCluster $
--             rebootCacheCluster
--
--         , requestCreateUserGroup $
--             createUserGroup
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
--         , requestDisassociateGlobalReplicationGroup $
--             disassociateGlobalReplicationGroup
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
--         , requestCompleteMigration $
--             completeMigration
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
--         , requestDescribeGlobalReplicationGroups $
--             describeGlobalReplicationGroups
--
--         , requestDescribeCacheSubnetGroups $
--             describeCacheSubnetGroups
--
--         , requestDescribeUpdateActions $
--             describeUpdateActions
--
--         , requestRebalanceSlotsInGlobalReplicationGroup $
--             rebalanceSlotsInGlobalReplicationGroup
--
--         , requestCreateCacheSecurityGroup $
--             createCacheSecurityGroup
--
--         , requestDecreaseReplicaCount $
--             decreaseReplicaCount
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
--         , requestFailoverGlobalReplicationGroup $
--             failoverGlobalReplicationGroup
--
--         , requestCreateCacheSubnetGroup $
--             createCacheSubnetGroup
--
--         , requestCreateGlobalReplicationGroup $
--             createGlobalReplicationGroup
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
--         , requestIncreaseReplicaCount $
--             increaseReplicaCount
--
--         , requestModifyReplicationGroupShardConfiguration $
--             modifyReplicationGroupShardConfiguration
--
--         , requestBatchApplyUpdateAction $
--             batchApplyUpdateAction
--
--         , requestDeleteUserGroup $
--             deleteUserGroup
--
--         , requestDescribeServiceUpdates $
--             describeServiceUpdates
--
--         , requestDescribeSnapshots $
--             describeSnapshots
--
--         , requestDescribeReplicationGroups $
--             describeReplicationGroups
--
--         , requestCreateUser $
--             createUser
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
--         , requestDeleteUser $
--             deleteUser
--
--         , requestCreateSnapshot $
--             createSnapshot
--
--         , requestModifyGlobalReplicationGroup $
--             modifyGlobalReplicationGroup
--
--         , requestDescribeUserGroups $
--             describeUserGroups
--
--         , requestDeleteCacheParameterGroup $
--             deleteCacheParameterGroup
--
--         , requestDescribeCacheSecurityGroups $
--             describeCacheSecurityGroups
--
--         , requestBatchStopUpdateAction $
--             batchStopUpdateAction
--
--         , requestModifyCacheCluster $
--             modifyCacheCluster
--
--         , requestDescribeCacheEngineVersions $
--             describeCacheEngineVersions
--
--         , requestModifyUserGroup $
--             modifyUserGroup
--
--         , requestCreateCacheParameterGroup $
--             createCacheParameterGroup
--
--         , requestDescribeReservedCacheNodes $
--             describeReservedCacheNodes
--
--         , requestDeleteGlobalReplicationGroup $
--             deleteGlobalReplicationGroup
--
--         , requestDecreaseNodeGroupsInGlobalReplicationGroup $
--             decreaseNodeGroupsInGlobalReplicationGroup
--
--         , requestModifyUser $
--             modifyUser
--
--         , requestDeleteCacheSubnetGroup $
--             deleteCacheSubnetGroup
--
--           ]

--     , testGroup "response"
--         [ responseStartMigration $
--             startMigrationResponse
--
--         , responseDeleteCacheSecurityGroup $
--             deleteCacheSecurityGroupResponse
--
--         , responseCreateReplicationGroup $
--             createReplicationGroupResponse
--
--         , responseDeleteCacheCluster $
--             deleteCacheClusterResponse
--
--         , responseIncreaseNodeGroupsInGlobalReplicationGroup $
--             increaseNodeGroupsInGlobalReplicationGroupResponse
--
--         , responseDescribeUsers $
--             describeUsersResponse
--
--         , responseRebootCacheCluster $
--             rebootCacheClusterResponse
--
--         , responseCreateUserGroup $
--             userGroup
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
--         , responseDisassociateGlobalReplicationGroup $
--             disassociateGlobalReplicationGroupResponse
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
--         , responseCompleteMigration $
--             completeMigrationResponse
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
--         , responseDescribeGlobalReplicationGroups $
--             describeGlobalReplicationGroupsResponse
--
--         , responseDescribeCacheSubnetGroups $
--             describeCacheSubnetGroupsResponse
--
--         , responseDescribeUpdateActions $
--             describeUpdateActionsResponse
--
--         , responseRebalanceSlotsInGlobalReplicationGroup $
--             rebalanceSlotsInGlobalReplicationGroupResponse
--
--         , responseCreateCacheSecurityGroup $
--             createCacheSecurityGroupResponse
--
--         , responseDecreaseReplicaCount $
--             decreaseReplicaCountResponse
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
--         , responseFailoverGlobalReplicationGroup $
--             failoverGlobalReplicationGroupResponse
--
--         , responseCreateCacheSubnetGroup $
--             createCacheSubnetGroupResponse
--
--         , responseCreateGlobalReplicationGroup $
--             createGlobalReplicationGroupResponse
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
--         , responseIncreaseReplicaCount $
--             increaseReplicaCountResponse
--
--         , responseModifyReplicationGroupShardConfiguration $
--             modifyReplicationGroupShardConfigurationResponse
--
--         , responseBatchApplyUpdateAction $
--             updateActionResultsMessage
--
--         , responseDeleteUserGroup $
--             userGroup
--
--         , responseDescribeServiceUpdates $
--             describeServiceUpdatesResponse
--
--         , responseDescribeSnapshots $
--             describeSnapshotsResponse
--
--         , responseDescribeReplicationGroups $
--             describeReplicationGroupsResponse
--
--         , responseCreateUser $
--             user
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
--         , responseDeleteUser $
--             user
--
--         , responseCreateSnapshot $
--             createSnapshotResponse
--
--         , responseModifyGlobalReplicationGroup $
--             modifyGlobalReplicationGroupResponse
--
--         , responseDescribeUserGroups $
--             describeUserGroupsResponse
--
--         , responseDeleteCacheParameterGroup $
--             deleteCacheParameterGroupResponse
--
--         , responseDescribeCacheSecurityGroups $
--             describeCacheSecurityGroupsResponse
--
--         , responseBatchStopUpdateAction $
--             updateActionResultsMessage
--
--         , responseModifyCacheCluster $
--             modifyCacheClusterResponse
--
--         , responseDescribeCacheEngineVersions $
--             describeCacheEngineVersionsResponse
--
--         , responseModifyUserGroup $
--             userGroup
--
--         , responseCreateCacheParameterGroup $
--             createCacheParameterGroupResponse
--
--         , responseDescribeReservedCacheNodes $
--             describeReservedCacheNodesResponse
--
--         , responseDeleteGlobalReplicationGroup $
--             deleteGlobalReplicationGroupResponse
--
--         , responseDecreaseNodeGroupsInGlobalReplicationGroup $
--             decreaseNodeGroupsInGlobalReplicationGroupResponse
--
--         , responseModifyUser $
--             user
--
--         , responseDeleteCacheSubnetGroup $
--             deleteCacheSubnetGroupResponse
--
--           ]
--     ]

-- Requests

requestStartMigration :: StartMigration -> TestTree
requestStartMigration =
  req
    "StartMigration"
    "fixture/StartMigration.yaml"

requestDeleteCacheSecurityGroup :: DeleteCacheSecurityGroup -> TestTree
requestDeleteCacheSecurityGroup =
  req
    "DeleteCacheSecurityGroup"
    "fixture/DeleteCacheSecurityGroup.yaml"

requestCreateReplicationGroup :: CreateReplicationGroup -> TestTree
requestCreateReplicationGroup =
  req
    "CreateReplicationGroup"
    "fixture/CreateReplicationGroup.yaml"

requestDeleteCacheCluster :: DeleteCacheCluster -> TestTree
requestDeleteCacheCluster =
  req
    "DeleteCacheCluster"
    "fixture/DeleteCacheCluster.yaml"

requestIncreaseNodeGroupsInGlobalReplicationGroup :: IncreaseNodeGroupsInGlobalReplicationGroup -> TestTree
requestIncreaseNodeGroupsInGlobalReplicationGroup =
  req
    "IncreaseNodeGroupsInGlobalReplicationGroup"
    "fixture/IncreaseNodeGroupsInGlobalReplicationGroup.yaml"

requestDescribeUsers :: DescribeUsers -> TestTree
requestDescribeUsers =
  req
    "DescribeUsers"
    "fixture/DescribeUsers.yaml"

requestRebootCacheCluster :: RebootCacheCluster -> TestTree
requestRebootCacheCluster =
  req
    "RebootCacheCluster"
    "fixture/RebootCacheCluster.yaml"

requestCreateUserGroup :: CreateUserGroup -> TestTree
requestCreateUserGroup =
  req
    "CreateUserGroup"
    "fixture/CreateUserGroup.yaml"

requestRevokeCacheSecurityGroupIngress :: RevokeCacheSecurityGroupIngress -> TestTree
requestRevokeCacheSecurityGroupIngress =
  req
    "RevokeCacheSecurityGroupIngress"
    "fixture/RevokeCacheSecurityGroupIngress.yaml"

requestCreateCacheCluster :: CreateCacheCluster -> TestTree
requestCreateCacheCluster =
  req
    "CreateCacheCluster"
    "fixture/CreateCacheCluster.yaml"

requestDescribeEvents :: DescribeEvents -> TestTree
requestDescribeEvents =
  req
    "DescribeEvents"
    "fixture/DescribeEvents.yaml"

requestDescribeEngineDefaultParameters :: DescribeEngineDefaultParameters -> TestTree
requestDescribeEngineDefaultParameters =
  req
    "DescribeEngineDefaultParameters"
    "fixture/DescribeEngineDefaultParameters.yaml"

requestDisassociateGlobalReplicationGroup :: DisassociateGlobalReplicationGroup -> TestTree
requestDisassociateGlobalReplicationGroup =
  req
    "DisassociateGlobalReplicationGroup"
    "fixture/DisassociateGlobalReplicationGroup.yaml"

requestModifyCacheParameterGroup :: ModifyCacheParameterGroup -> TestTree
requestModifyCacheParameterGroup =
  req
    "ModifyCacheParameterGroup"
    "fixture/ModifyCacheParameterGroup.yaml"

requestTestFailover :: TestFailover -> TestTree
requestTestFailover =
  req
    "TestFailover"
    "fixture/TestFailover.yaml"

requestDeleteReplicationGroup :: DeleteReplicationGroup -> TestTree
requestDeleteReplicationGroup =
  req
    "DeleteReplicationGroup"
    "fixture/DeleteReplicationGroup.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestCompleteMigration :: CompleteMigration -> TestTree
requestCompleteMigration =
  req
    "CompleteMigration"
    "fixture/CompleteMigration.yaml"

requestDescribeCacheClusters :: DescribeCacheClusters -> TestTree
requestDescribeCacheClusters =
  req
    "DescribeCacheClusters"
    "fixture/DescribeCacheClusters.yaml"

requestPurchaseReservedCacheNodesOffering :: PurchaseReservedCacheNodesOffering -> TestTree
requestPurchaseReservedCacheNodesOffering =
  req
    "PurchaseReservedCacheNodesOffering"
    "fixture/PurchaseReservedCacheNodesOffering.yaml"

requestRemoveTagsFromResource :: RemoveTagsFromResource -> TestTree
requestRemoveTagsFromResource =
  req
    "RemoveTagsFromResource"
    "fixture/RemoveTagsFromResource.yaml"

requestModifyReplicationGroup :: ModifyReplicationGroup -> TestTree
requestModifyReplicationGroup =
  req
    "ModifyReplicationGroup"
    "fixture/ModifyReplicationGroup.yaml"

requestDescribeCacheParameters :: DescribeCacheParameters -> TestTree
requestDescribeCacheParameters =
  req
    "DescribeCacheParameters"
    "fixture/DescribeCacheParameters.yaml"

requestDescribeGlobalReplicationGroups :: DescribeGlobalReplicationGroups -> TestTree
requestDescribeGlobalReplicationGroups =
  req
    "DescribeGlobalReplicationGroups"
    "fixture/DescribeGlobalReplicationGroups.yaml"

requestDescribeCacheSubnetGroups :: DescribeCacheSubnetGroups -> TestTree
requestDescribeCacheSubnetGroups =
  req
    "DescribeCacheSubnetGroups"
    "fixture/DescribeCacheSubnetGroups.yaml"

requestDescribeUpdateActions :: DescribeUpdateActions -> TestTree
requestDescribeUpdateActions =
  req
    "DescribeUpdateActions"
    "fixture/DescribeUpdateActions.yaml"

requestRebalanceSlotsInGlobalReplicationGroup :: RebalanceSlotsInGlobalReplicationGroup -> TestTree
requestRebalanceSlotsInGlobalReplicationGroup =
  req
    "RebalanceSlotsInGlobalReplicationGroup"
    "fixture/RebalanceSlotsInGlobalReplicationGroup.yaml"

requestCreateCacheSecurityGroup :: CreateCacheSecurityGroup -> TestTree
requestCreateCacheSecurityGroup =
  req
    "CreateCacheSecurityGroup"
    "fixture/CreateCacheSecurityGroup.yaml"

requestDecreaseReplicaCount :: DecreaseReplicaCount -> TestTree
requestDecreaseReplicaCount =
  req
    "DecreaseReplicaCount"
    "fixture/DecreaseReplicaCount.yaml"

requestAddTagsToResource :: AddTagsToResource -> TestTree
requestAddTagsToResource =
  req
    "AddTagsToResource"
    "fixture/AddTagsToResource.yaml"

requestAuthorizeCacheSecurityGroupIngress :: AuthorizeCacheSecurityGroupIngress -> TestTree
requestAuthorizeCacheSecurityGroupIngress =
  req
    "AuthorizeCacheSecurityGroupIngress"
    "fixture/AuthorizeCacheSecurityGroupIngress.yaml"

requestCopySnapshot :: CopySnapshot -> TestTree
requestCopySnapshot =
  req
    "CopySnapshot"
    "fixture/CopySnapshot.yaml"

requestFailoverGlobalReplicationGroup :: FailoverGlobalReplicationGroup -> TestTree
requestFailoverGlobalReplicationGroup =
  req
    "FailoverGlobalReplicationGroup"
    "fixture/FailoverGlobalReplicationGroup.yaml"

requestCreateCacheSubnetGroup :: CreateCacheSubnetGroup -> TestTree
requestCreateCacheSubnetGroup =
  req
    "CreateCacheSubnetGroup"
    "fixture/CreateCacheSubnetGroup.yaml"

requestCreateGlobalReplicationGroup :: CreateGlobalReplicationGroup -> TestTree
requestCreateGlobalReplicationGroup =
  req
    "CreateGlobalReplicationGroup"
    "fixture/CreateGlobalReplicationGroup.yaml"

requestDescribeCacheParameterGroups :: DescribeCacheParameterGroups -> TestTree
requestDescribeCacheParameterGroups =
  req
    "DescribeCacheParameterGroups"
    "fixture/DescribeCacheParameterGroups.yaml"

requestResetCacheParameterGroup :: ResetCacheParameterGroup -> TestTree
requestResetCacheParameterGroup =
  req
    "ResetCacheParameterGroup"
    "fixture/ResetCacheParameterGroup.yaml"

requestListAllowedNodeTypeModifications :: ListAllowedNodeTypeModifications -> TestTree
requestListAllowedNodeTypeModifications =
  req
    "ListAllowedNodeTypeModifications"
    "fixture/ListAllowedNodeTypeModifications.yaml"

requestIncreaseReplicaCount :: IncreaseReplicaCount -> TestTree
requestIncreaseReplicaCount =
  req
    "IncreaseReplicaCount"
    "fixture/IncreaseReplicaCount.yaml"

requestModifyReplicationGroupShardConfiguration :: ModifyReplicationGroupShardConfiguration -> TestTree
requestModifyReplicationGroupShardConfiguration =
  req
    "ModifyReplicationGroupShardConfiguration"
    "fixture/ModifyReplicationGroupShardConfiguration.yaml"

requestBatchApplyUpdateAction :: BatchApplyUpdateAction -> TestTree
requestBatchApplyUpdateAction =
  req
    "BatchApplyUpdateAction"
    "fixture/BatchApplyUpdateAction.yaml"

requestDeleteUserGroup :: DeleteUserGroup -> TestTree
requestDeleteUserGroup =
  req
    "DeleteUserGroup"
    "fixture/DeleteUserGroup.yaml"

requestDescribeServiceUpdates :: DescribeServiceUpdates -> TestTree
requestDescribeServiceUpdates =
  req
    "DescribeServiceUpdates"
    "fixture/DescribeServiceUpdates.yaml"

requestDescribeSnapshots :: DescribeSnapshots -> TestTree
requestDescribeSnapshots =
  req
    "DescribeSnapshots"
    "fixture/DescribeSnapshots.yaml"

requestDescribeReplicationGroups :: DescribeReplicationGroups -> TestTree
requestDescribeReplicationGroups =
  req
    "DescribeReplicationGroups"
    "fixture/DescribeReplicationGroups.yaml"

requestCreateUser :: CreateUser -> TestTree
requestCreateUser =
  req
    "CreateUser"
    "fixture/CreateUser.yaml"

requestDeleteSnapshot :: DeleteSnapshot -> TestTree
requestDeleteSnapshot =
  req
    "DeleteSnapshot"
    "fixture/DeleteSnapshot.yaml"

requestDescribeReservedCacheNodesOfferings :: DescribeReservedCacheNodesOfferings -> TestTree
requestDescribeReservedCacheNodesOfferings =
  req
    "DescribeReservedCacheNodesOfferings"
    "fixture/DescribeReservedCacheNodesOfferings.yaml"

requestModifyCacheSubnetGroup :: ModifyCacheSubnetGroup -> TestTree
requestModifyCacheSubnetGroup =
  req
    "ModifyCacheSubnetGroup"
    "fixture/ModifyCacheSubnetGroup.yaml"

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser =
  req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

requestCreateSnapshot :: CreateSnapshot -> TestTree
requestCreateSnapshot =
  req
    "CreateSnapshot"
    "fixture/CreateSnapshot.yaml"

requestModifyGlobalReplicationGroup :: ModifyGlobalReplicationGroup -> TestTree
requestModifyGlobalReplicationGroup =
  req
    "ModifyGlobalReplicationGroup"
    "fixture/ModifyGlobalReplicationGroup.yaml"

requestDescribeUserGroups :: DescribeUserGroups -> TestTree
requestDescribeUserGroups =
  req
    "DescribeUserGroups"
    "fixture/DescribeUserGroups.yaml"

requestDeleteCacheParameterGroup :: DeleteCacheParameterGroup -> TestTree
requestDeleteCacheParameterGroup =
  req
    "DeleteCacheParameterGroup"
    "fixture/DeleteCacheParameterGroup.yaml"

requestDescribeCacheSecurityGroups :: DescribeCacheSecurityGroups -> TestTree
requestDescribeCacheSecurityGroups =
  req
    "DescribeCacheSecurityGroups"
    "fixture/DescribeCacheSecurityGroups.yaml"

requestBatchStopUpdateAction :: BatchStopUpdateAction -> TestTree
requestBatchStopUpdateAction =
  req
    "BatchStopUpdateAction"
    "fixture/BatchStopUpdateAction.yaml"

requestModifyCacheCluster :: ModifyCacheCluster -> TestTree
requestModifyCacheCluster =
  req
    "ModifyCacheCluster"
    "fixture/ModifyCacheCluster.yaml"

requestDescribeCacheEngineVersions :: DescribeCacheEngineVersions -> TestTree
requestDescribeCacheEngineVersions =
  req
    "DescribeCacheEngineVersions"
    "fixture/DescribeCacheEngineVersions.yaml"

requestModifyUserGroup :: ModifyUserGroup -> TestTree
requestModifyUserGroup =
  req
    "ModifyUserGroup"
    "fixture/ModifyUserGroup.yaml"

requestCreateCacheParameterGroup :: CreateCacheParameterGroup -> TestTree
requestCreateCacheParameterGroup =
  req
    "CreateCacheParameterGroup"
    "fixture/CreateCacheParameterGroup.yaml"

requestDescribeReservedCacheNodes :: DescribeReservedCacheNodes -> TestTree
requestDescribeReservedCacheNodes =
  req
    "DescribeReservedCacheNodes"
    "fixture/DescribeReservedCacheNodes.yaml"

requestDeleteGlobalReplicationGroup :: DeleteGlobalReplicationGroup -> TestTree
requestDeleteGlobalReplicationGroup =
  req
    "DeleteGlobalReplicationGroup"
    "fixture/DeleteGlobalReplicationGroup.yaml"

requestDecreaseNodeGroupsInGlobalReplicationGroup :: DecreaseNodeGroupsInGlobalReplicationGroup -> TestTree
requestDecreaseNodeGroupsInGlobalReplicationGroup =
  req
    "DecreaseNodeGroupsInGlobalReplicationGroup"
    "fixture/DecreaseNodeGroupsInGlobalReplicationGroup.yaml"

requestModifyUser :: ModifyUser -> TestTree
requestModifyUser =
  req
    "ModifyUser"
    "fixture/ModifyUser.yaml"

requestDeleteCacheSubnetGroup :: DeleteCacheSubnetGroup -> TestTree
requestDeleteCacheSubnetGroup =
  req
    "DeleteCacheSubnetGroup"
    "fixture/DeleteCacheSubnetGroup.yaml"

-- Responses

responseStartMigration :: StartMigrationResponse -> TestTree
responseStartMigration =
  res
    "StartMigrationResponse"
    "fixture/StartMigrationResponse.proto"
    elastiCache
    (Proxy :: Proxy StartMigration)

responseDeleteCacheSecurityGroup :: DeleteCacheSecurityGroupResponse -> TestTree
responseDeleteCacheSecurityGroup =
  res
    "DeleteCacheSecurityGroupResponse"
    "fixture/DeleteCacheSecurityGroupResponse.proto"
    elastiCache
    (Proxy :: Proxy DeleteCacheSecurityGroup)

responseCreateReplicationGroup :: CreateReplicationGroupResponse -> TestTree
responseCreateReplicationGroup =
  res
    "CreateReplicationGroupResponse"
    "fixture/CreateReplicationGroupResponse.proto"
    elastiCache
    (Proxy :: Proxy CreateReplicationGroup)

responseDeleteCacheCluster :: DeleteCacheClusterResponse -> TestTree
responseDeleteCacheCluster =
  res
    "DeleteCacheClusterResponse"
    "fixture/DeleteCacheClusterResponse.proto"
    elastiCache
    (Proxy :: Proxy DeleteCacheCluster)

responseIncreaseNodeGroupsInGlobalReplicationGroup :: IncreaseNodeGroupsInGlobalReplicationGroupResponse -> TestTree
responseIncreaseNodeGroupsInGlobalReplicationGroup =
  res
    "IncreaseNodeGroupsInGlobalReplicationGroupResponse"
    "fixture/IncreaseNodeGroupsInGlobalReplicationGroupResponse.proto"
    elastiCache
    (Proxy :: Proxy IncreaseNodeGroupsInGlobalReplicationGroup)

responseDescribeUsers :: DescribeUsersResponse -> TestTree
responseDescribeUsers =
  res
    "DescribeUsersResponse"
    "fixture/DescribeUsersResponse.proto"
    elastiCache
    (Proxy :: Proxy DescribeUsers)

responseRebootCacheCluster :: RebootCacheClusterResponse -> TestTree
responseRebootCacheCluster =
  res
    "RebootCacheClusterResponse"
    "fixture/RebootCacheClusterResponse.proto"
    elastiCache
    (Proxy :: Proxy RebootCacheCluster)

responseCreateUserGroup :: UserGroup -> TestTree
responseCreateUserGroup =
  res
    "CreateUserGroupResponse"
    "fixture/CreateUserGroupResponse.proto"
    elastiCache
    (Proxy :: Proxy CreateUserGroup)

responseRevokeCacheSecurityGroupIngress :: RevokeCacheSecurityGroupIngressResponse -> TestTree
responseRevokeCacheSecurityGroupIngress =
  res
    "RevokeCacheSecurityGroupIngressResponse"
    "fixture/RevokeCacheSecurityGroupIngressResponse.proto"
    elastiCache
    (Proxy :: Proxy RevokeCacheSecurityGroupIngress)

responseCreateCacheCluster :: CreateCacheClusterResponse -> TestTree
responseCreateCacheCluster =
  res
    "CreateCacheClusterResponse"
    "fixture/CreateCacheClusterResponse.proto"
    elastiCache
    (Proxy :: Proxy CreateCacheCluster)

responseDescribeEvents :: DescribeEventsResponse -> TestTree
responseDescribeEvents =
  res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse.proto"
    elastiCache
    (Proxy :: Proxy DescribeEvents)

responseDescribeEngineDefaultParameters :: DescribeEngineDefaultParametersResponse -> TestTree
responseDescribeEngineDefaultParameters =
  res
    "DescribeEngineDefaultParametersResponse"
    "fixture/DescribeEngineDefaultParametersResponse.proto"
    elastiCache
    (Proxy :: Proxy DescribeEngineDefaultParameters)

responseDisassociateGlobalReplicationGroup :: DisassociateGlobalReplicationGroupResponse -> TestTree
responseDisassociateGlobalReplicationGroup =
  res
    "DisassociateGlobalReplicationGroupResponse"
    "fixture/DisassociateGlobalReplicationGroupResponse.proto"
    elastiCache
    (Proxy :: Proxy DisassociateGlobalReplicationGroup)

responseModifyCacheParameterGroup :: CacheParameterGroupNameMessage -> TestTree
responseModifyCacheParameterGroup =
  res
    "ModifyCacheParameterGroupResponse"
    "fixture/ModifyCacheParameterGroupResponse.proto"
    elastiCache
    (Proxy :: Proxy ModifyCacheParameterGroup)

responseTestFailover :: TestFailoverResponse -> TestTree
responseTestFailover =
  res
    "TestFailoverResponse"
    "fixture/TestFailoverResponse.proto"
    elastiCache
    (Proxy :: Proxy TestFailover)

responseDeleteReplicationGroup :: DeleteReplicationGroupResponse -> TestTree
responseDeleteReplicationGroup =
  res
    "DeleteReplicationGroupResponse"
    "fixture/DeleteReplicationGroupResponse.proto"
    elastiCache
    (Proxy :: Proxy DeleteReplicationGroup)

responseListTagsForResource :: TagListMessage -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    elastiCache
    (Proxy :: Proxy ListTagsForResource)

responseCompleteMigration :: CompleteMigrationResponse -> TestTree
responseCompleteMigration =
  res
    "CompleteMigrationResponse"
    "fixture/CompleteMigrationResponse.proto"
    elastiCache
    (Proxy :: Proxy CompleteMigration)

responseDescribeCacheClusters :: DescribeCacheClustersResponse -> TestTree
responseDescribeCacheClusters =
  res
    "DescribeCacheClustersResponse"
    "fixture/DescribeCacheClustersResponse.proto"
    elastiCache
    (Proxy :: Proxy DescribeCacheClusters)

responsePurchaseReservedCacheNodesOffering :: PurchaseReservedCacheNodesOfferingResponse -> TestTree
responsePurchaseReservedCacheNodesOffering =
  res
    "PurchaseReservedCacheNodesOfferingResponse"
    "fixture/PurchaseReservedCacheNodesOfferingResponse.proto"
    elastiCache
    (Proxy :: Proxy PurchaseReservedCacheNodesOffering)

responseRemoveTagsFromResource :: TagListMessage -> TestTree
responseRemoveTagsFromResource =
  res
    "RemoveTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse.proto"
    elastiCache
    (Proxy :: Proxy RemoveTagsFromResource)

responseModifyReplicationGroup :: ModifyReplicationGroupResponse -> TestTree
responseModifyReplicationGroup =
  res
    "ModifyReplicationGroupResponse"
    "fixture/ModifyReplicationGroupResponse.proto"
    elastiCache
    (Proxy :: Proxy ModifyReplicationGroup)

responseDescribeCacheParameters :: DescribeCacheParametersResponse -> TestTree
responseDescribeCacheParameters =
  res
    "DescribeCacheParametersResponse"
    "fixture/DescribeCacheParametersResponse.proto"
    elastiCache
    (Proxy :: Proxy DescribeCacheParameters)

responseDescribeGlobalReplicationGroups :: DescribeGlobalReplicationGroupsResponse -> TestTree
responseDescribeGlobalReplicationGroups =
  res
    "DescribeGlobalReplicationGroupsResponse"
    "fixture/DescribeGlobalReplicationGroupsResponse.proto"
    elastiCache
    (Proxy :: Proxy DescribeGlobalReplicationGroups)

responseDescribeCacheSubnetGroups :: DescribeCacheSubnetGroupsResponse -> TestTree
responseDescribeCacheSubnetGroups =
  res
    "DescribeCacheSubnetGroupsResponse"
    "fixture/DescribeCacheSubnetGroupsResponse.proto"
    elastiCache
    (Proxy :: Proxy DescribeCacheSubnetGroups)

responseDescribeUpdateActions :: DescribeUpdateActionsResponse -> TestTree
responseDescribeUpdateActions =
  res
    "DescribeUpdateActionsResponse"
    "fixture/DescribeUpdateActionsResponse.proto"
    elastiCache
    (Proxy :: Proxy DescribeUpdateActions)

responseRebalanceSlotsInGlobalReplicationGroup :: RebalanceSlotsInGlobalReplicationGroupResponse -> TestTree
responseRebalanceSlotsInGlobalReplicationGroup =
  res
    "RebalanceSlotsInGlobalReplicationGroupResponse"
    "fixture/RebalanceSlotsInGlobalReplicationGroupResponse.proto"
    elastiCache
    (Proxy :: Proxy RebalanceSlotsInGlobalReplicationGroup)

responseCreateCacheSecurityGroup :: CreateCacheSecurityGroupResponse -> TestTree
responseCreateCacheSecurityGroup =
  res
    "CreateCacheSecurityGroupResponse"
    "fixture/CreateCacheSecurityGroupResponse.proto"
    elastiCache
    (Proxy :: Proxy CreateCacheSecurityGroup)

responseDecreaseReplicaCount :: DecreaseReplicaCountResponse -> TestTree
responseDecreaseReplicaCount =
  res
    "DecreaseReplicaCountResponse"
    "fixture/DecreaseReplicaCountResponse.proto"
    elastiCache
    (Proxy :: Proxy DecreaseReplicaCount)

responseAddTagsToResource :: TagListMessage -> TestTree
responseAddTagsToResource =
  res
    "AddTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse.proto"
    elastiCache
    (Proxy :: Proxy AddTagsToResource)

responseAuthorizeCacheSecurityGroupIngress :: AuthorizeCacheSecurityGroupIngressResponse -> TestTree
responseAuthorizeCacheSecurityGroupIngress =
  res
    "AuthorizeCacheSecurityGroupIngressResponse"
    "fixture/AuthorizeCacheSecurityGroupIngressResponse.proto"
    elastiCache
    (Proxy :: Proxy AuthorizeCacheSecurityGroupIngress)

responseCopySnapshot :: CopySnapshotResponse -> TestTree
responseCopySnapshot =
  res
    "CopySnapshotResponse"
    "fixture/CopySnapshotResponse.proto"
    elastiCache
    (Proxy :: Proxy CopySnapshot)

responseFailoverGlobalReplicationGroup :: FailoverGlobalReplicationGroupResponse -> TestTree
responseFailoverGlobalReplicationGroup =
  res
    "FailoverGlobalReplicationGroupResponse"
    "fixture/FailoverGlobalReplicationGroupResponse.proto"
    elastiCache
    (Proxy :: Proxy FailoverGlobalReplicationGroup)

responseCreateCacheSubnetGroup :: CreateCacheSubnetGroupResponse -> TestTree
responseCreateCacheSubnetGroup =
  res
    "CreateCacheSubnetGroupResponse"
    "fixture/CreateCacheSubnetGroupResponse.proto"
    elastiCache
    (Proxy :: Proxy CreateCacheSubnetGroup)

responseCreateGlobalReplicationGroup :: CreateGlobalReplicationGroupResponse -> TestTree
responseCreateGlobalReplicationGroup =
  res
    "CreateGlobalReplicationGroupResponse"
    "fixture/CreateGlobalReplicationGroupResponse.proto"
    elastiCache
    (Proxy :: Proxy CreateGlobalReplicationGroup)

responseDescribeCacheParameterGroups :: DescribeCacheParameterGroupsResponse -> TestTree
responseDescribeCacheParameterGroups =
  res
    "DescribeCacheParameterGroupsResponse"
    "fixture/DescribeCacheParameterGroupsResponse.proto"
    elastiCache
    (Proxy :: Proxy DescribeCacheParameterGroups)

responseResetCacheParameterGroup :: CacheParameterGroupNameMessage -> TestTree
responseResetCacheParameterGroup =
  res
    "ResetCacheParameterGroupResponse"
    "fixture/ResetCacheParameterGroupResponse.proto"
    elastiCache
    (Proxy :: Proxy ResetCacheParameterGroup)

responseListAllowedNodeTypeModifications :: ListAllowedNodeTypeModificationsResponse -> TestTree
responseListAllowedNodeTypeModifications =
  res
    "ListAllowedNodeTypeModificationsResponse"
    "fixture/ListAllowedNodeTypeModificationsResponse.proto"
    elastiCache
    (Proxy :: Proxy ListAllowedNodeTypeModifications)

responseIncreaseReplicaCount :: IncreaseReplicaCountResponse -> TestTree
responseIncreaseReplicaCount =
  res
    "IncreaseReplicaCountResponse"
    "fixture/IncreaseReplicaCountResponse.proto"
    elastiCache
    (Proxy :: Proxy IncreaseReplicaCount)

responseModifyReplicationGroupShardConfiguration :: ModifyReplicationGroupShardConfigurationResponse -> TestTree
responseModifyReplicationGroupShardConfiguration =
  res
    "ModifyReplicationGroupShardConfigurationResponse"
    "fixture/ModifyReplicationGroupShardConfigurationResponse.proto"
    elastiCache
    (Proxy :: Proxy ModifyReplicationGroupShardConfiguration)

responseBatchApplyUpdateAction :: UpdateActionResultsMessage -> TestTree
responseBatchApplyUpdateAction =
  res
    "BatchApplyUpdateActionResponse"
    "fixture/BatchApplyUpdateActionResponse.proto"
    elastiCache
    (Proxy :: Proxy BatchApplyUpdateAction)

responseDeleteUserGroup :: UserGroup -> TestTree
responseDeleteUserGroup =
  res
    "DeleteUserGroupResponse"
    "fixture/DeleteUserGroupResponse.proto"
    elastiCache
    (Proxy :: Proxy DeleteUserGroup)

responseDescribeServiceUpdates :: DescribeServiceUpdatesResponse -> TestTree
responseDescribeServiceUpdates =
  res
    "DescribeServiceUpdatesResponse"
    "fixture/DescribeServiceUpdatesResponse.proto"
    elastiCache
    (Proxy :: Proxy DescribeServiceUpdates)

responseDescribeSnapshots :: DescribeSnapshotsResponse -> TestTree
responseDescribeSnapshots =
  res
    "DescribeSnapshotsResponse"
    "fixture/DescribeSnapshotsResponse.proto"
    elastiCache
    (Proxy :: Proxy DescribeSnapshots)

responseDescribeReplicationGroups :: DescribeReplicationGroupsResponse -> TestTree
responseDescribeReplicationGroups =
  res
    "DescribeReplicationGroupsResponse"
    "fixture/DescribeReplicationGroupsResponse.proto"
    elastiCache
    (Proxy :: Proxy DescribeReplicationGroups)

responseCreateUser :: User -> TestTree
responseCreateUser =
  res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    elastiCache
    (Proxy :: Proxy CreateUser)

responseDeleteSnapshot :: DeleteSnapshotResponse -> TestTree
responseDeleteSnapshot =
  res
    "DeleteSnapshotResponse"
    "fixture/DeleteSnapshotResponse.proto"
    elastiCache
    (Proxy :: Proxy DeleteSnapshot)

responseDescribeReservedCacheNodesOfferings :: DescribeReservedCacheNodesOfferingsResponse -> TestTree
responseDescribeReservedCacheNodesOfferings =
  res
    "DescribeReservedCacheNodesOfferingsResponse"
    "fixture/DescribeReservedCacheNodesOfferingsResponse.proto"
    elastiCache
    (Proxy :: Proxy DescribeReservedCacheNodesOfferings)

responseModifyCacheSubnetGroup :: ModifyCacheSubnetGroupResponse -> TestTree
responseModifyCacheSubnetGroup =
  res
    "ModifyCacheSubnetGroupResponse"
    "fixture/ModifyCacheSubnetGroupResponse.proto"
    elastiCache
    (Proxy :: Proxy ModifyCacheSubnetGroup)

responseDeleteUser :: User -> TestTree
responseDeleteUser =
  res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    elastiCache
    (Proxy :: Proxy DeleteUser)

responseCreateSnapshot :: CreateSnapshotResponse -> TestTree
responseCreateSnapshot =
  res
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse.proto"
    elastiCache
    (Proxy :: Proxy CreateSnapshot)

responseModifyGlobalReplicationGroup :: ModifyGlobalReplicationGroupResponse -> TestTree
responseModifyGlobalReplicationGroup =
  res
    "ModifyGlobalReplicationGroupResponse"
    "fixture/ModifyGlobalReplicationGroupResponse.proto"
    elastiCache
    (Proxy :: Proxy ModifyGlobalReplicationGroup)

responseDescribeUserGroups :: DescribeUserGroupsResponse -> TestTree
responseDescribeUserGroups =
  res
    "DescribeUserGroupsResponse"
    "fixture/DescribeUserGroupsResponse.proto"
    elastiCache
    (Proxy :: Proxy DescribeUserGroups)

responseDeleteCacheParameterGroup :: DeleteCacheParameterGroupResponse -> TestTree
responseDeleteCacheParameterGroup =
  res
    "DeleteCacheParameterGroupResponse"
    "fixture/DeleteCacheParameterGroupResponse.proto"
    elastiCache
    (Proxy :: Proxy DeleteCacheParameterGroup)

responseDescribeCacheSecurityGroups :: DescribeCacheSecurityGroupsResponse -> TestTree
responseDescribeCacheSecurityGroups =
  res
    "DescribeCacheSecurityGroupsResponse"
    "fixture/DescribeCacheSecurityGroupsResponse.proto"
    elastiCache
    (Proxy :: Proxy DescribeCacheSecurityGroups)

responseBatchStopUpdateAction :: UpdateActionResultsMessage -> TestTree
responseBatchStopUpdateAction =
  res
    "BatchStopUpdateActionResponse"
    "fixture/BatchStopUpdateActionResponse.proto"
    elastiCache
    (Proxy :: Proxy BatchStopUpdateAction)

responseModifyCacheCluster :: ModifyCacheClusterResponse -> TestTree
responseModifyCacheCluster =
  res
    "ModifyCacheClusterResponse"
    "fixture/ModifyCacheClusterResponse.proto"
    elastiCache
    (Proxy :: Proxy ModifyCacheCluster)

responseDescribeCacheEngineVersions :: DescribeCacheEngineVersionsResponse -> TestTree
responseDescribeCacheEngineVersions =
  res
    "DescribeCacheEngineVersionsResponse"
    "fixture/DescribeCacheEngineVersionsResponse.proto"
    elastiCache
    (Proxy :: Proxy DescribeCacheEngineVersions)

responseModifyUserGroup :: UserGroup -> TestTree
responseModifyUserGroup =
  res
    "ModifyUserGroupResponse"
    "fixture/ModifyUserGroupResponse.proto"
    elastiCache
    (Proxy :: Proxy ModifyUserGroup)

responseCreateCacheParameterGroup :: CreateCacheParameterGroupResponse -> TestTree
responseCreateCacheParameterGroup =
  res
    "CreateCacheParameterGroupResponse"
    "fixture/CreateCacheParameterGroupResponse.proto"
    elastiCache
    (Proxy :: Proxy CreateCacheParameterGroup)

responseDescribeReservedCacheNodes :: DescribeReservedCacheNodesResponse -> TestTree
responseDescribeReservedCacheNodes =
  res
    "DescribeReservedCacheNodesResponse"
    "fixture/DescribeReservedCacheNodesResponse.proto"
    elastiCache
    (Proxy :: Proxy DescribeReservedCacheNodes)

responseDeleteGlobalReplicationGroup :: DeleteGlobalReplicationGroupResponse -> TestTree
responseDeleteGlobalReplicationGroup =
  res
    "DeleteGlobalReplicationGroupResponse"
    "fixture/DeleteGlobalReplicationGroupResponse.proto"
    elastiCache
    (Proxy :: Proxy DeleteGlobalReplicationGroup)

responseDecreaseNodeGroupsInGlobalReplicationGroup :: DecreaseNodeGroupsInGlobalReplicationGroupResponse -> TestTree
responseDecreaseNodeGroupsInGlobalReplicationGroup =
  res
    "DecreaseNodeGroupsInGlobalReplicationGroupResponse"
    "fixture/DecreaseNodeGroupsInGlobalReplicationGroupResponse.proto"
    elastiCache
    (Proxy :: Proxy DecreaseNodeGroupsInGlobalReplicationGroup)

responseModifyUser :: User -> TestTree
responseModifyUser =
  res
    "ModifyUserResponse"
    "fixture/ModifyUserResponse.proto"
    elastiCache
    (Proxy :: Proxy ModifyUser)

responseDeleteCacheSubnetGroup :: DeleteCacheSubnetGroupResponse -> TestTree
responseDeleteCacheSubnetGroup =
  res
    "DeleteCacheSubnetGroupResponse"
    "fixture/DeleteCacheSubnetGroupResponse.proto"
    elastiCache
    (Proxy :: Proxy DeleteCacheSubnetGroup)
