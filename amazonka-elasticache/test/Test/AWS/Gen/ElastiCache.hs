{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ElastiCache
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--         [ requestCreateReplicationGroup $
--             newCreateReplicationGroup
--
--         , requestDeleteCacheCluster $
--             newDeleteCacheCluster
--
--         , requestDeleteUserGroup $
--             newDeleteUserGroup
--
--         , requestRebootCacheCluster $
--             newRebootCacheCluster
--
--         , requestIncreaseNodeGroupsInGlobalReplicationGroup $
--             newIncreaseNodeGroupsInGlobalReplicationGroup
--
--         , requestStartMigration $
--             newStartMigration
--
--         , requestDeleteCacheSecurityGroup $
--             newDeleteCacheSecurityGroup
--
--         , requestAuthorizeCacheSecurityGroupIngress $
--             newAuthorizeCacheSecurityGroupIngress
--
--         , requestDecreaseReplicaCount $
--             newDecreaseReplicaCount
--
--         , requestCreateCacheSecurityGroup $
--             newCreateCacheSecurityGroup
--
--         , requestRebalanceSlotsInGlobalReplicationGroup $
--             newRebalanceSlotsInGlobalReplicationGroup
--
--         , requestCopySnapshot $
--             newCopySnapshot
--
--         , requestModifyCacheCluster $
--             newModifyCacheCluster
--
--         , requestDescribeCacheSubnetGroups $
--             newDescribeCacheSubnetGroups
--
--         , requestDescribeGlobalReplicationGroups $
--             newDescribeGlobalReplicationGroups
--
--         , requestDescribeReservedCacheNodes $
--             newDescribeReservedCacheNodes
--
--         , requestDeleteCacheParameterGroup $
--             newDeleteCacheParameterGroup
--
--         , requestRemoveTagsFromResource $
--             newRemoveTagsFromResource
--
--         , requestDescribeCacheSecurityGroups $
--             newDescribeCacheSecurityGroups
--
--         , requestModifyReplicationGroup $
--             newModifyReplicationGroup
--
--         , requestBatchStopUpdateAction $
--             newBatchStopUpdateAction
--
--         , requestPurchaseReservedCacheNodesOffering $
--             newPurchaseReservedCacheNodesOffering
--
--         , requestDeleteSnapshot $
--             newDeleteSnapshot
--
--         , requestCreateUser $
--             newCreateUser
--
--         , requestCompleteMigration $
--             newCompleteMigration
--
--         , requestDescribeEvents $
--             newDescribeEvents
--
--         , requestTestFailover $
--             newTestFailover
--
--         , requestDescribeSnapshots $
--             newDescribeSnapshots
--
--         , requestDisassociateGlobalReplicationGroup $
--             newDisassociateGlobalReplicationGroup
--
--         , requestDeleteReplicationGroup $
--             newDeleteReplicationGroup
--
--         , requestCreateCacheCluster $
--             newCreateCacheCluster
--
--         , requestBatchApplyUpdateAction $
--             newBatchApplyUpdateAction
--
--         , requestListAllowedNodeTypeModifications $
--             newListAllowedNodeTypeModifications
--
--         , requestIncreaseReplicaCount $
--             newIncreaseReplicaCount
--
--         , requestResetCacheParameterGroup $
--             newResetCacheParameterGroup
--
--         , requestModifyReplicationGroupShardConfiguration $
--             newModifyReplicationGroupShardConfiguration
--
--         , requestDescribeUsers $
--             newDescribeUsers
--
--         , requestCreateCacheSubnetGroup $
--             newCreateCacheSubnetGroup
--
--         , requestCreateGlobalReplicationGroup $
--             newCreateGlobalReplicationGroup
--
--         , requestDescribeCacheParameterGroups $
--             newDescribeCacheParameterGroups
--
--         , requestFailoverGlobalReplicationGroup $
--             newFailoverGlobalReplicationGroup
--
--         , requestAddTagsToResource $
--             newAddTagsToResource
--
--         , requestDecreaseNodeGroupsInGlobalReplicationGroup $
--             newDecreaseNodeGroupsInGlobalReplicationGroup
--
--         , requestDeleteGlobalReplicationGroup $
--             newDeleteGlobalReplicationGroup
--
--         , requestModifyUser $
--             newModifyUser
--
--         , requestDeleteCacheSubnetGroup $
--             newDeleteCacheSubnetGroup
--
--         , requestDescribeUpdateActions $
--             newDescribeUpdateActions
--
--         , requestDescribeCacheEngineVersions $
--             newDescribeCacheEngineVersions
--
--         , requestModifyUserGroup $
--             newModifyUserGroup
--
--         , requestCreateCacheParameterGroup $
--             newCreateCacheParameterGroup
--
--         , requestDescribeCacheParameters $
--             newDescribeCacheParameters
--
--         , requestDescribeUserGroups $
--             newDescribeUserGroups
--
--         , requestDescribeCacheClusters $
--             newDescribeCacheClusters
--
--         , requestCreateSnapshot $
--             newCreateSnapshot
--
--         , requestModifyGlobalReplicationGroup $
--             newModifyGlobalReplicationGroup
--
--         , requestModifyCacheSubnetGroup $
--             newModifyCacheSubnetGroup
--
--         , requestDeleteUser $
--             newDeleteUser
--
--         , requestDescribeReplicationGroups $
--             newDescribeReplicationGroups
--
--         , requestDescribeReservedCacheNodesOfferings $
--             newDescribeReservedCacheNodesOfferings
--
--         , requestDescribeEngineDefaultParameters $
--             newDescribeEngineDefaultParameters
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestRevokeCacheSecurityGroupIngress $
--             newRevokeCacheSecurityGroupIngress
--
--         , requestCreateUserGroup $
--             newCreateUserGroup
--
--         , requestModifyCacheParameterGroup $
--             newModifyCacheParameterGroup
--
--         , requestDescribeServiceUpdates $
--             newDescribeServiceUpdates
--
--           ]

--     , testGroup "response"
--         [ responseCreateReplicationGroup $
--             newCreateReplicationGroupResponse
--
--         , responseDeleteCacheCluster $
--             newDeleteCacheClusterResponse
--
--         , responseDeleteUserGroup $
--             newUserGroup
--
--         , responseRebootCacheCluster $
--             newRebootCacheClusterResponse
--
--         , responseIncreaseNodeGroupsInGlobalReplicationGroup $
--             newIncreaseNodeGroupsInGlobalReplicationGroupResponse
--
--         , responseStartMigration $
--             newStartMigrationResponse
--
--         , responseDeleteCacheSecurityGroup $
--             newDeleteCacheSecurityGroupResponse
--
--         , responseAuthorizeCacheSecurityGroupIngress $
--             newAuthorizeCacheSecurityGroupIngressResponse
--
--         , responseDecreaseReplicaCount $
--             newDecreaseReplicaCountResponse
--
--         , responseCreateCacheSecurityGroup $
--             newCreateCacheSecurityGroupResponse
--
--         , responseRebalanceSlotsInGlobalReplicationGroup $
--             newRebalanceSlotsInGlobalReplicationGroupResponse
--
--         , responseCopySnapshot $
--             newCopySnapshotResponse
--
--         , responseModifyCacheCluster $
--             newModifyCacheClusterResponse
--
--         , responseDescribeCacheSubnetGroups $
--             newDescribeCacheSubnetGroupsResponse
--
--         , responseDescribeGlobalReplicationGroups $
--             newDescribeGlobalReplicationGroupsResponse
--
--         , responseDescribeReservedCacheNodes $
--             newDescribeReservedCacheNodesResponse
--
--         , responseDeleteCacheParameterGroup $
--             newDeleteCacheParameterGroupResponse
--
--         , responseRemoveTagsFromResource $
--             newTagListMessage
--
--         , responseDescribeCacheSecurityGroups $
--             newDescribeCacheSecurityGroupsResponse
--
--         , responseModifyReplicationGroup $
--             newModifyReplicationGroupResponse
--
--         , responseBatchStopUpdateAction $
--             newUpdateActionResultsMessage
--
--         , responsePurchaseReservedCacheNodesOffering $
--             newPurchaseReservedCacheNodesOfferingResponse
--
--         , responseDeleteSnapshot $
--             newDeleteSnapshotResponse
--
--         , responseCreateUser $
--             newUser
--
--         , responseCompleteMigration $
--             newCompleteMigrationResponse
--
--         , responseDescribeEvents $
--             newDescribeEventsResponse
--
--         , responseTestFailover $
--             newTestFailoverResponse
--
--         , responseDescribeSnapshots $
--             newDescribeSnapshotsResponse
--
--         , responseDisassociateGlobalReplicationGroup $
--             newDisassociateGlobalReplicationGroupResponse
--
--         , responseDeleteReplicationGroup $
--             newDeleteReplicationGroupResponse
--
--         , responseCreateCacheCluster $
--             newCreateCacheClusterResponse
--
--         , responseBatchApplyUpdateAction $
--             newUpdateActionResultsMessage
--
--         , responseListAllowedNodeTypeModifications $
--             newListAllowedNodeTypeModificationsResponse
--
--         , responseIncreaseReplicaCount $
--             newIncreaseReplicaCountResponse
--
--         , responseResetCacheParameterGroup $
--             newCacheParameterGroupNameMessage
--
--         , responseModifyReplicationGroupShardConfiguration $
--             newModifyReplicationGroupShardConfigurationResponse
--
--         , responseDescribeUsers $
--             newDescribeUsersResponse
--
--         , responseCreateCacheSubnetGroup $
--             newCreateCacheSubnetGroupResponse
--
--         , responseCreateGlobalReplicationGroup $
--             newCreateGlobalReplicationGroupResponse
--
--         , responseDescribeCacheParameterGroups $
--             newDescribeCacheParameterGroupsResponse
--
--         , responseFailoverGlobalReplicationGroup $
--             newFailoverGlobalReplicationGroupResponse
--
--         , responseAddTagsToResource $
--             newTagListMessage
--
--         , responseDecreaseNodeGroupsInGlobalReplicationGroup $
--             newDecreaseNodeGroupsInGlobalReplicationGroupResponse
--
--         , responseDeleteGlobalReplicationGroup $
--             newDeleteGlobalReplicationGroupResponse
--
--         , responseModifyUser $
--             newUser
--
--         , responseDeleteCacheSubnetGroup $
--             newDeleteCacheSubnetGroupResponse
--
--         , responseDescribeUpdateActions $
--             newDescribeUpdateActionsResponse
--
--         , responseDescribeCacheEngineVersions $
--             newDescribeCacheEngineVersionsResponse
--
--         , responseModifyUserGroup $
--             newUserGroup
--
--         , responseCreateCacheParameterGroup $
--             newCreateCacheParameterGroupResponse
--
--         , responseDescribeCacheParameters $
--             newDescribeCacheParametersResponse
--
--         , responseDescribeUserGroups $
--             newDescribeUserGroupsResponse
--
--         , responseDescribeCacheClusters $
--             newDescribeCacheClustersResponse
--
--         , responseCreateSnapshot $
--             newCreateSnapshotResponse
--
--         , responseModifyGlobalReplicationGroup $
--             newModifyGlobalReplicationGroupResponse
--
--         , responseModifyCacheSubnetGroup $
--             newModifyCacheSubnetGroupResponse
--
--         , responseDeleteUser $
--             newUser
--
--         , responseDescribeReplicationGroups $
--             newDescribeReplicationGroupsResponse
--
--         , responseDescribeReservedCacheNodesOfferings $
--             newDescribeReservedCacheNodesOfferingsResponse
--
--         , responseDescribeEngineDefaultParameters $
--             newDescribeEngineDefaultParametersResponse
--
--         , responseListTagsForResource $
--             newTagListMessage
--
--         , responseRevokeCacheSecurityGroupIngress $
--             newRevokeCacheSecurityGroupIngressResponse
--
--         , responseCreateUserGroup $
--             newUserGroup
--
--         , responseModifyCacheParameterGroup $
--             newCacheParameterGroupNameMessage
--
--         , responseDescribeServiceUpdates $
--             newDescribeServiceUpdatesResponse
--
--           ]
--     ]

-- Requests

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

requestDeleteUserGroup :: DeleteUserGroup -> TestTree
requestDeleteUserGroup =
  req
    "DeleteUserGroup"
    "fixture/DeleteUserGroup.yaml"

requestRebootCacheCluster :: RebootCacheCluster -> TestTree
requestRebootCacheCluster =
  req
    "RebootCacheCluster"
    "fixture/RebootCacheCluster.yaml"

requestIncreaseNodeGroupsInGlobalReplicationGroup :: IncreaseNodeGroupsInGlobalReplicationGroup -> TestTree
requestIncreaseNodeGroupsInGlobalReplicationGroup =
  req
    "IncreaseNodeGroupsInGlobalReplicationGroup"
    "fixture/IncreaseNodeGroupsInGlobalReplicationGroup.yaml"

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

requestAuthorizeCacheSecurityGroupIngress :: AuthorizeCacheSecurityGroupIngress -> TestTree
requestAuthorizeCacheSecurityGroupIngress =
  req
    "AuthorizeCacheSecurityGroupIngress"
    "fixture/AuthorizeCacheSecurityGroupIngress.yaml"

requestDecreaseReplicaCount :: DecreaseReplicaCount -> TestTree
requestDecreaseReplicaCount =
  req
    "DecreaseReplicaCount"
    "fixture/DecreaseReplicaCount.yaml"

requestCreateCacheSecurityGroup :: CreateCacheSecurityGroup -> TestTree
requestCreateCacheSecurityGroup =
  req
    "CreateCacheSecurityGroup"
    "fixture/CreateCacheSecurityGroup.yaml"

requestRebalanceSlotsInGlobalReplicationGroup :: RebalanceSlotsInGlobalReplicationGroup -> TestTree
requestRebalanceSlotsInGlobalReplicationGroup =
  req
    "RebalanceSlotsInGlobalReplicationGroup"
    "fixture/RebalanceSlotsInGlobalReplicationGroup.yaml"

requestCopySnapshot :: CopySnapshot -> TestTree
requestCopySnapshot =
  req
    "CopySnapshot"
    "fixture/CopySnapshot.yaml"

requestModifyCacheCluster :: ModifyCacheCluster -> TestTree
requestModifyCacheCluster =
  req
    "ModifyCacheCluster"
    "fixture/ModifyCacheCluster.yaml"

requestDescribeCacheSubnetGroups :: DescribeCacheSubnetGroups -> TestTree
requestDescribeCacheSubnetGroups =
  req
    "DescribeCacheSubnetGroups"
    "fixture/DescribeCacheSubnetGroups.yaml"

requestDescribeGlobalReplicationGroups :: DescribeGlobalReplicationGroups -> TestTree
requestDescribeGlobalReplicationGroups =
  req
    "DescribeGlobalReplicationGroups"
    "fixture/DescribeGlobalReplicationGroups.yaml"

requestDescribeReservedCacheNodes :: DescribeReservedCacheNodes -> TestTree
requestDescribeReservedCacheNodes =
  req
    "DescribeReservedCacheNodes"
    "fixture/DescribeReservedCacheNodes.yaml"

requestDeleteCacheParameterGroup :: DeleteCacheParameterGroup -> TestTree
requestDeleteCacheParameterGroup =
  req
    "DeleteCacheParameterGroup"
    "fixture/DeleteCacheParameterGroup.yaml"

requestRemoveTagsFromResource :: RemoveTagsFromResource -> TestTree
requestRemoveTagsFromResource =
  req
    "RemoveTagsFromResource"
    "fixture/RemoveTagsFromResource.yaml"

requestDescribeCacheSecurityGroups :: DescribeCacheSecurityGroups -> TestTree
requestDescribeCacheSecurityGroups =
  req
    "DescribeCacheSecurityGroups"
    "fixture/DescribeCacheSecurityGroups.yaml"

requestModifyReplicationGroup :: ModifyReplicationGroup -> TestTree
requestModifyReplicationGroup =
  req
    "ModifyReplicationGroup"
    "fixture/ModifyReplicationGroup.yaml"

requestBatchStopUpdateAction :: BatchStopUpdateAction -> TestTree
requestBatchStopUpdateAction =
  req
    "BatchStopUpdateAction"
    "fixture/BatchStopUpdateAction.yaml"

requestPurchaseReservedCacheNodesOffering :: PurchaseReservedCacheNodesOffering -> TestTree
requestPurchaseReservedCacheNodesOffering =
  req
    "PurchaseReservedCacheNodesOffering"
    "fixture/PurchaseReservedCacheNodesOffering.yaml"

requestDeleteSnapshot :: DeleteSnapshot -> TestTree
requestDeleteSnapshot =
  req
    "DeleteSnapshot"
    "fixture/DeleteSnapshot.yaml"

requestCreateUser :: CreateUser -> TestTree
requestCreateUser =
  req
    "CreateUser"
    "fixture/CreateUser.yaml"

requestCompleteMigration :: CompleteMigration -> TestTree
requestCompleteMigration =
  req
    "CompleteMigration"
    "fixture/CompleteMigration.yaml"

requestDescribeEvents :: DescribeEvents -> TestTree
requestDescribeEvents =
  req
    "DescribeEvents"
    "fixture/DescribeEvents.yaml"

requestTestFailover :: TestFailover -> TestTree
requestTestFailover =
  req
    "TestFailover"
    "fixture/TestFailover.yaml"

requestDescribeSnapshots :: DescribeSnapshots -> TestTree
requestDescribeSnapshots =
  req
    "DescribeSnapshots"
    "fixture/DescribeSnapshots.yaml"

requestDisassociateGlobalReplicationGroup :: DisassociateGlobalReplicationGroup -> TestTree
requestDisassociateGlobalReplicationGroup =
  req
    "DisassociateGlobalReplicationGroup"
    "fixture/DisassociateGlobalReplicationGroup.yaml"

requestDeleteReplicationGroup :: DeleteReplicationGroup -> TestTree
requestDeleteReplicationGroup =
  req
    "DeleteReplicationGroup"
    "fixture/DeleteReplicationGroup.yaml"

requestCreateCacheCluster :: CreateCacheCluster -> TestTree
requestCreateCacheCluster =
  req
    "CreateCacheCluster"
    "fixture/CreateCacheCluster.yaml"

requestBatchApplyUpdateAction :: BatchApplyUpdateAction -> TestTree
requestBatchApplyUpdateAction =
  req
    "BatchApplyUpdateAction"
    "fixture/BatchApplyUpdateAction.yaml"

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

requestResetCacheParameterGroup :: ResetCacheParameterGroup -> TestTree
requestResetCacheParameterGroup =
  req
    "ResetCacheParameterGroup"
    "fixture/ResetCacheParameterGroup.yaml"

requestModifyReplicationGroupShardConfiguration :: ModifyReplicationGroupShardConfiguration -> TestTree
requestModifyReplicationGroupShardConfiguration =
  req
    "ModifyReplicationGroupShardConfiguration"
    "fixture/ModifyReplicationGroupShardConfiguration.yaml"

requestDescribeUsers :: DescribeUsers -> TestTree
requestDescribeUsers =
  req
    "DescribeUsers"
    "fixture/DescribeUsers.yaml"

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

requestFailoverGlobalReplicationGroup :: FailoverGlobalReplicationGroup -> TestTree
requestFailoverGlobalReplicationGroup =
  req
    "FailoverGlobalReplicationGroup"
    "fixture/FailoverGlobalReplicationGroup.yaml"

requestAddTagsToResource :: AddTagsToResource -> TestTree
requestAddTagsToResource =
  req
    "AddTagsToResource"
    "fixture/AddTagsToResource.yaml"

requestDecreaseNodeGroupsInGlobalReplicationGroup :: DecreaseNodeGroupsInGlobalReplicationGroup -> TestTree
requestDecreaseNodeGroupsInGlobalReplicationGroup =
  req
    "DecreaseNodeGroupsInGlobalReplicationGroup"
    "fixture/DecreaseNodeGroupsInGlobalReplicationGroup.yaml"

requestDeleteGlobalReplicationGroup :: DeleteGlobalReplicationGroup -> TestTree
requestDeleteGlobalReplicationGroup =
  req
    "DeleteGlobalReplicationGroup"
    "fixture/DeleteGlobalReplicationGroup.yaml"

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

requestDescribeUpdateActions :: DescribeUpdateActions -> TestTree
requestDescribeUpdateActions =
  req
    "DescribeUpdateActions"
    "fixture/DescribeUpdateActions.yaml"

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

requestDescribeCacheParameters :: DescribeCacheParameters -> TestTree
requestDescribeCacheParameters =
  req
    "DescribeCacheParameters"
    "fixture/DescribeCacheParameters.yaml"

requestDescribeUserGroups :: DescribeUserGroups -> TestTree
requestDescribeUserGroups =
  req
    "DescribeUserGroups"
    "fixture/DescribeUserGroups.yaml"

requestDescribeCacheClusters :: DescribeCacheClusters -> TestTree
requestDescribeCacheClusters =
  req
    "DescribeCacheClusters"
    "fixture/DescribeCacheClusters.yaml"

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

requestDescribeReplicationGroups :: DescribeReplicationGroups -> TestTree
requestDescribeReplicationGroups =
  req
    "DescribeReplicationGroups"
    "fixture/DescribeReplicationGroups.yaml"

requestDescribeReservedCacheNodesOfferings :: DescribeReservedCacheNodesOfferings -> TestTree
requestDescribeReservedCacheNodesOfferings =
  req
    "DescribeReservedCacheNodesOfferings"
    "fixture/DescribeReservedCacheNodesOfferings.yaml"

requestDescribeEngineDefaultParameters :: DescribeEngineDefaultParameters -> TestTree
requestDescribeEngineDefaultParameters =
  req
    "DescribeEngineDefaultParameters"
    "fixture/DescribeEngineDefaultParameters.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestRevokeCacheSecurityGroupIngress :: RevokeCacheSecurityGroupIngress -> TestTree
requestRevokeCacheSecurityGroupIngress =
  req
    "RevokeCacheSecurityGroupIngress"
    "fixture/RevokeCacheSecurityGroupIngress.yaml"

requestCreateUserGroup :: CreateUserGroup -> TestTree
requestCreateUserGroup =
  req
    "CreateUserGroup"
    "fixture/CreateUserGroup.yaml"

requestModifyCacheParameterGroup :: ModifyCacheParameterGroup -> TestTree
requestModifyCacheParameterGroup =
  req
    "ModifyCacheParameterGroup"
    "fixture/ModifyCacheParameterGroup.yaml"

requestDescribeServiceUpdates :: DescribeServiceUpdates -> TestTree
requestDescribeServiceUpdates =
  req
    "DescribeServiceUpdates"
    "fixture/DescribeServiceUpdates.yaml"

-- Responses

responseCreateReplicationGroup :: CreateReplicationGroupResponse -> TestTree
responseCreateReplicationGroup =
  res
    "CreateReplicationGroupResponse"
    "fixture/CreateReplicationGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateReplicationGroup)

responseDeleteCacheCluster :: DeleteCacheClusterResponse -> TestTree
responseDeleteCacheCluster =
  res
    "DeleteCacheClusterResponse"
    "fixture/DeleteCacheClusterResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCacheCluster)

responseDeleteUserGroup :: UserGroup -> TestTree
responseDeleteUserGroup =
  res
    "DeleteUserGroupResponse"
    "fixture/DeleteUserGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUserGroup)

responseRebootCacheCluster :: RebootCacheClusterResponse -> TestTree
responseRebootCacheCluster =
  res
    "RebootCacheClusterResponse"
    "fixture/RebootCacheClusterResponse.proto"
    defaultService
    (Proxy :: Proxy RebootCacheCluster)

responseIncreaseNodeGroupsInGlobalReplicationGroup :: IncreaseNodeGroupsInGlobalReplicationGroupResponse -> TestTree
responseIncreaseNodeGroupsInGlobalReplicationGroup =
  res
    "IncreaseNodeGroupsInGlobalReplicationGroupResponse"
    "fixture/IncreaseNodeGroupsInGlobalReplicationGroupResponse.proto"
    defaultService
    (Proxy :: Proxy IncreaseNodeGroupsInGlobalReplicationGroup)

responseStartMigration :: StartMigrationResponse -> TestTree
responseStartMigration =
  res
    "StartMigrationResponse"
    "fixture/StartMigrationResponse.proto"
    defaultService
    (Proxy :: Proxy StartMigration)

responseDeleteCacheSecurityGroup :: DeleteCacheSecurityGroupResponse -> TestTree
responseDeleteCacheSecurityGroup =
  res
    "DeleteCacheSecurityGroupResponse"
    "fixture/DeleteCacheSecurityGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCacheSecurityGroup)

responseAuthorizeCacheSecurityGroupIngress :: AuthorizeCacheSecurityGroupIngressResponse -> TestTree
responseAuthorizeCacheSecurityGroupIngress =
  res
    "AuthorizeCacheSecurityGroupIngressResponse"
    "fixture/AuthorizeCacheSecurityGroupIngressResponse.proto"
    defaultService
    (Proxy :: Proxy AuthorizeCacheSecurityGroupIngress)

responseDecreaseReplicaCount :: DecreaseReplicaCountResponse -> TestTree
responseDecreaseReplicaCount =
  res
    "DecreaseReplicaCountResponse"
    "fixture/DecreaseReplicaCountResponse.proto"
    defaultService
    (Proxy :: Proxy DecreaseReplicaCount)

responseCreateCacheSecurityGroup :: CreateCacheSecurityGroupResponse -> TestTree
responseCreateCacheSecurityGroup =
  res
    "CreateCacheSecurityGroupResponse"
    "fixture/CreateCacheSecurityGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCacheSecurityGroup)

responseRebalanceSlotsInGlobalReplicationGroup :: RebalanceSlotsInGlobalReplicationGroupResponse -> TestTree
responseRebalanceSlotsInGlobalReplicationGroup =
  res
    "RebalanceSlotsInGlobalReplicationGroupResponse"
    "fixture/RebalanceSlotsInGlobalReplicationGroupResponse.proto"
    defaultService
    (Proxy :: Proxy RebalanceSlotsInGlobalReplicationGroup)

responseCopySnapshot :: CopySnapshotResponse -> TestTree
responseCopySnapshot =
  res
    "CopySnapshotResponse"
    "fixture/CopySnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy CopySnapshot)

responseModifyCacheCluster :: ModifyCacheClusterResponse -> TestTree
responseModifyCacheCluster =
  res
    "ModifyCacheClusterResponse"
    "fixture/ModifyCacheClusterResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyCacheCluster)

responseDescribeCacheSubnetGroups :: DescribeCacheSubnetGroupsResponse -> TestTree
responseDescribeCacheSubnetGroups =
  res
    "DescribeCacheSubnetGroupsResponse"
    "fixture/DescribeCacheSubnetGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCacheSubnetGroups)

responseDescribeGlobalReplicationGroups :: DescribeGlobalReplicationGroupsResponse -> TestTree
responseDescribeGlobalReplicationGroups =
  res
    "DescribeGlobalReplicationGroupsResponse"
    "fixture/DescribeGlobalReplicationGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeGlobalReplicationGroups)

responseDescribeReservedCacheNodes :: DescribeReservedCacheNodesResponse -> TestTree
responseDescribeReservedCacheNodes =
  res
    "DescribeReservedCacheNodesResponse"
    "fixture/DescribeReservedCacheNodesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeReservedCacheNodes)

responseDeleteCacheParameterGroup :: DeleteCacheParameterGroupResponse -> TestTree
responseDeleteCacheParameterGroup =
  res
    "DeleteCacheParameterGroupResponse"
    "fixture/DeleteCacheParameterGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCacheParameterGroup)

responseRemoveTagsFromResource :: TagListMessage -> TestTree
responseRemoveTagsFromResource =
  res
    "RemoveTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveTagsFromResource)

responseDescribeCacheSecurityGroups :: DescribeCacheSecurityGroupsResponse -> TestTree
responseDescribeCacheSecurityGroups =
  res
    "DescribeCacheSecurityGroupsResponse"
    "fixture/DescribeCacheSecurityGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCacheSecurityGroups)

responseModifyReplicationGroup :: ModifyReplicationGroupResponse -> TestTree
responseModifyReplicationGroup =
  res
    "ModifyReplicationGroupResponse"
    "fixture/ModifyReplicationGroupResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyReplicationGroup)

responseBatchStopUpdateAction :: UpdateActionResultsMessage -> TestTree
responseBatchStopUpdateAction =
  res
    "BatchStopUpdateActionResponse"
    "fixture/BatchStopUpdateActionResponse.proto"
    defaultService
    (Proxy :: Proxy BatchStopUpdateAction)

responsePurchaseReservedCacheNodesOffering :: PurchaseReservedCacheNodesOfferingResponse -> TestTree
responsePurchaseReservedCacheNodesOffering =
  res
    "PurchaseReservedCacheNodesOfferingResponse"
    "fixture/PurchaseReservedCacheNodesOfferingResponse.proto"
    defaultService
    (Proxy :: Proxy PurchaseReservedCacheNodesOffering)

responseDeleteSnapshot :: DeleteSnapshotResponse -> TestTree
responseDeleteSnapshot =
  res
    "DeleteSnapshotResponse"
    "fixture/DeleteSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSnapshot)

responseCreateUser :: User -> TestTree
responseCreateUser =
  res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUser)

responseCompleteMigration :: CompleteMigrationResponse -> TestTree
responseCompleteMigration =
  res
    "CompleteMigrationResponse"
    "fixture/CompleteMigrationResponse.proto"
    defaultService
    (Proxy :: Proxy CompleteMigration)

responseDescribeEvents :: DescribeEventsResponse -> TestTree
responseDescribeEvents =
  res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEvents)

responseTestFailover :: TestFailoverResponse -> TestTree
responseTestFailover =
  res
    "TestFailoverResponse"
    "fixture/TestFailoverResponse.proto"
    defaultService
    (Proxy :: Proxy TestFailover)

responseDescribeSnapshots :: DescribeSnapshotsResponse -> TestTree
responseDescribeSnapshots =
  res
    "DescribeSnapshotsResponse"
    "fixture/DescribeSnapshotsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSnapshots)

responseDisassociateGlobalReplicationGroup :: DisassociateGlobalReplicationGroupResponse -> TestTree
responseDisassociateGlobalReplicationGroup =
  res
    "DisassociateGlobalReplicationGroupResponse"
    "fixture/DisassociateGlobalReplicationGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateGlobalReplicationGroup)

responseDeleteReplicationGroup :: DeleteReplicationGroupResponse -> TestTree
responseDeleteReplicationGroup =
  res
    "DeleteReplicationGroupResponse"
    "fixture/DeleteReplicationGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteReplicationGroup)

responseCreateCacheCluster :: CreateCacheClusterResponse -> TestTree
responseCreateCacheCluster =
  res
    "CreateCacheClusterResponse"
    "fixture/CreateCacheClusterResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCacheCluster)

responseBatchApplyUpdateAction :: UpdateActionResultsMessage -> TestTree
responseBatchApplyUpdateAction =
  res
    "BatchApplyUpdateActionResponse"
    "fixture/BatchApplyUpdateActionResponse.proto"
    defaultService
    (Proxy :: Proxy BatchApplyUpdateAction)

responseListAllowedNodeTypeModifications :: ListAllowedNodeTypeModificationsResponse -> TestTree
responseListAllowedNodeTypeModifications =
  res
    "ListAllowedNodeTypeModificationsResponse"
    "fixture/ListAllowedNodeTypeModificationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListAllowedNodeTypeModifications)

responseIncreaseReplicaCount :: IncreaseReplicaCountResponse -> TestTree
responseIncreaseReplicaCount =
  res
    "IncreaseReplicaCountResponse"
    "fixture/IncreaseReplicaCountResponse.proto"
    defaultService
    (Proxy :: Proxy IncreaseReplicaCount)

responseResetCacheParameterGroup :: CacheParameterGroupNameMessage -> TestTree
responseResetCacheParameterGroup =
  res
    "ResetCacheParameterGroupResponse"
    "fixture/ResetCacheParameterGroupResponse.proto"
    defaultService
    (Proxy :: Proxy ResetCacheParameterGroup)

responseModifyReplicationGroupShardConfiguration :: ModifyReplicationGroupShardConfigurationResponse -> TestTree
responseModifyReplicationGroupShardConfiguration =
  res
    "ModifyReplicationGroupShardConfigurationResponse"
    "fixture/ModifyReplicationGroupShardConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyReplicationGroupShardConfiguration)

responseDescribeUsers :: DescribeUsersResponse -> TestTree
responseDescribeUsers =
  res
    "DescribeUsersResponse"
    "fixture/DescribeUsersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeUsers)

responseCreateCacheSubnetGroup :: CreateCacheSubnetGroupResponse -> TestTree
responseCreateCacheSubnetGroup =
  res
    "CreateCacheSubnetGroupResponse"
    "fixture/CreateCacheSubnetGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCacheSubnetGroup)

responseCreateGlobalReplicationGroup :: CreateGlobalReplicationGroupResponse -> TestTree
responseCreateGlobalReplicationGroup =
  res
    "CreateGlobalReplicationGroupResponse"
    "fixture/CreateGlobalReplicationGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateGlobalReplicationGroup)

responseDescribeCacheParameterGroups :: DescribeCacheParameterGroupsResponse -> TestTree
responseDescribeCacheParameterGroups =
  res
    "DescribeCacheParameterGroupsResponse"
    "fixture/DescribeCacheParameterGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCacheParameterGroups)

responseFailoverGlobalReplicationGroup :: FailoverGlobalReplicationGroupResponse -> TestTree
responseFailoverGlobalReplicationGroup =
  res
    "FailoverGlobalReplicationGroupResponse"
    "fixture/FailoverGlobalReplicationGroupResponse.proto"
    defaultService
    (Proxy :: Proxy FailoverGlobalReplicationGroup)

responseAddTagsToResource :: TagListMessage -> TestTree
responseAddTagsToResource =
  res
    "AddTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse.proto"
    defaultService
    (Proxy :: Proxy AddTagsToResource)

responseDecreaseNodeGroupsInGlobalReplicationGroup :: DecreaseNodeGroupsInGlobalReplicationGroupResponse -> TestTree
responseDecreaseNodeGroupsInGlobalReplicationGroup =
  res
    "DecreaseNodeGroupsInGlobalReplicationGroupResponse"
    "fixture/DecreaseNodeGroupsInGlobalReplicationGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DecreaseNodeGroupsInGlobalReplicationGroup)

responseDeleteGlobalReplicationGroup :: DeleteGlobalReplicationGroupResponse -> TestTree
responseDeleteGlobalReplicationGroup =
  res
    "DeleteGlobalReplicationGroupResponse"
    "fixture/DeleteGlobalReplicationGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteGlobalReplicationGroup)

responseModifyUser :: User -> TestTree
responseModifyUser =
  res
    "ModifyUserResponse"
    "fixture/ModifyUserResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyUser)

responseDeleteCacheSubnetGroup :: DeleteCacheSubnetGroupResponse -> TestTree
responseDeleteCacheSubnetGroup =
  res
    "DeleteCacheSubnetGroupResponse"
    "fixture/DeleteCacheSubnetGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCacheSubnetGroup)

responseDescribeUpdateActions :: DescribeUpdateActionsResponse -> TestTree
responseDescribeUpdateActions =
  res
    "DescribeUpdateActionsResponse"
    "fixture/DescribeUpdateActionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeUpdateActions)

responseDescribeCacheEngineVersions :: DescribeCacheEngineVersionsResponse -> TestTree
responseDescribeCacheEngineVersions =
  res
    "DescribeCacheEngineVersionsResponse"
    "fixture/DescribeCacheEngineVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCacheEngineVersions)

responseModifyUserGroup :: UserGroup -> TestTree
responseModifyUserGroup =
  res
    "ModifyUserGroupResponse"
    "fixture/ModifyUserGroupResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyUserGroup)

responseCreateCacheParameterGroup :: CreateCacheParameterGroupResponse -> TestTree
responseCreateCacheParameterGroup =
  res
    "CreateCacheParameterGroupResponse"
    "fixture/CreateCacheParameterGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCacheParameterGroup)

responseDescribeCacheParameters :: DescribeCacheParametersResponse -> TestTree
responseDescribeCacheParameters =
  res
    "DescribeCacheParametersResponse"
    "fixture/DescribeCacheParametersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCacheParameters)

responseDescribeUserGroups :: DescribeUserGroupsResponse -> TestTree
responseDescribeUserGroups =
  res
    "DescribeUserGroupsResponse"
    "fixture/DescribeUserGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeUserGroups)

responseDescribeCacheClusters :: DescribeCacheClustersResponse -> TestTree
responseDescribeCacheClusters =
  res
    "DescribeCacheClustersResponse"
    "fixture/DescribeCacheClustersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCacheClusters)

responseCreateSnapshot :: CreateSnapshotResponse -> TestTree
responseCreateSnapshot =
  res
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSnapshot)

responseModifyGlobalReplicationGroup :: ModifyGlobalReplicationGroupResponse -> TestTree
responseModifyGlobalReplicationGroup =
  res
    "ModifyGlobalReplicationGroupResponse"
    "fixture/ModifyGlobalReplicationGroupResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyGlobalReplicationGroup)

responseModifyCacheSubnetGroup :: ModifyCacheSubnetGroupResponse -> TestTree
responseModifyCacheSubnetGroup =
  res
    "ModifyCacheSubnetGroupResponse"
    "fixture/ModifyCacheSubnetGroupResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyCacheSubnetGroup)

responseDeleteUser :: User -> TestTree
responseDeleteUser =
  res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUser)

responseDescribeReplicationGroups :: DescribeReplicationGroupsResponse -> TestTree
responseDescribeReplicationGroups =
  res
    "DescribeReplicationGroupsResponse"
    "fixture/DescribeReplicationGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeReplicationGroups)

responseDescribeReservedCacheNodesOfferings :: DescribeReservedCacheNodesOfferingsResponse -> TestTree
responseDescribeReservedCacheNodesOfferings =
  res
    "DescribeReservedCacheNodesOfferingsResponse"
    "fixture/DescribeReservedCacheNodesOfferingsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeReservedCacheNodesOfferings)

responseDescribeEngineDefaultParameters :: DescribeEngineDefaultParametersResponse -> TestTree
responseDescribeEngineDefaultParameters =
  res
    "DescribeEngineDefaultParametersResponse"
    "fixture/DescribeEngineDefaultParametersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEngineDefaultParameters)

responseListTagsForResource :: TagListMessage -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseRevokeCacheSecurityGroupIngress :: RevokeCacheSecurityGroupIngressResponse -> TestTree
responseRevokeCacheSecurityGroupIngress =
  res
    "RevokeCacheSecurityGroupIngressResponse"
    "fixture/RevokeCacheSecurityGroupIngressResponse.proto"
    defaultService
    (Proxy :: Proxy RevokeCacheSecurityGroupIngress)

responseCreateUserGroup :: UserGroup -> TestTree
responseCreateUserGroup =
  res
    "CreateUserGroupResponse"
    "fixture/CreateUserGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUserGroup)

responseModifyCacheParameterGroup :: CacheParameterGroupNameMessage -> TestTree
responseModifyCacheParameterGroup =
  res
    "ModifyCacheParameterGroupResponse"
    "fixture/ModifyCacheParameterGroupResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyCacheParameterGroup)

responseDescribeServiceUpdates :: DescribeServiceUpdatesResponse -> TestTree
responseDescribeServiceUpdates =
  res
    "DescribeServiceUpdatesResponse"
    "fixture/DescribeServiceUpdatesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeServiceUpdates)
