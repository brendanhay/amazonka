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
--         , requestRebootCacheCluster $
--             newRebootCacheCluster
--
--         , requestIncreaseNodeGroupsInGlobalReplicationGroup $
--             newIncreaseNodeGroupsInGlobalReplicationGroup
--
--         , requestDeleteUserGroup $
--             newDeleteUserGroup
--
--         , requestDeleteCacheSecurityGroup $
--             newDeleteCacheSecurityGroup
--
--         , requestStartMigration $
--             newStartMigration
--
--         , requestAuthorizeCacheSecurityGroupIngress $
--             newAuthorizeCacheSecurityGroupIngress
--
--         , requestCopySnapshot $
--             newCopySnapshot
--
--         , requestDecreaseReplicaCount $
--             newDecreaseReplicaCount
--
--         , requestRebalanceSlotsInGlobalReplicationGroup $
--             newRebalanceSlotsInGlobalReplicationGroup
--
--         , requestCreateCacheSecurityGroup $
--             newCreateCacheSecurityGroup
--
--         , requestDescribeCacheSubnetGroups $
--             newDescribeCacheSubnetGroups
--
--         , requestDescribeGlobalReplicationGroups $
--             newDescribeGlobalReplicationGroups
--
--         , requestModifyCacheCluster $
--             newModifyCacheCluster
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
--         , requestBatchStopUpdateAction $
--             newBatchStopUpdateAction
--
--         , requestModifyReplicationGroup $
--             newModifyReplicationGroup
--
--         , requestPurchaseReservedCacheNodesOffering $
--             newPurchaseReservedCacheNodesOffering
--
--         , requestCreateUser $
--             newCreateUser
--
--         , requestDeleteSnapshot $
--             newDeleteSnapshot
--
--         , requestCompleteMigration $
--             newCompleteMigration
--
--         , requestCreateCacheCluster $
--             newCreateCacheCluster
--
--         , requestDisassociateGlobalReplicationGroup $
--             newDisassociateGlobalReplicationGroup
--
--         , requestDescribeEvents $
--             newDescribeEvents
--
--         , requestDeleteReplicationGroup $
--             newDeleteReplicationGroup
--
--         , requestDescribeSnapshots $
--             newDescribeSnapshots
--
--         , requestTestFailover $
--             newTestFailover
--
--         , requestBatchApplyUpdateAction $
--             newBatchApplyUpdateAction
--
--         , requestIncreaseReplicaCount $
--             newIncreaseReplicaCount
--
--         , requestModifyReplicationGroupShardConfiguration $
--             newModifyReplicationGroupShardConfiguration
--
--         , requestDescribeUsers $
--             newDescribeUsers
--
--         , requestListAllowedNodeTypeModifications $
--             newListAllowedNodeTypeModifications
--
--         , requestResetCacheParameterGroup $
--             newResetCacheParameterGroup
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
--         , requestDescribeUpdateActions $
--             newDescribeUpdateActions
--
--         , requestModifyUser $
--             newModifyUser
--
--         , requestDeleteCacheSubnetGroup $
--             newDeleteCacheSubnetGroup
--
--         , requestDeleteGlobalReplicationGroup $
--             newDeleteGlobalReplicationGroup
--
--         , requestCreateCacheParameterGroup $
--             newCreateCacheParameterGroup
--
--         , requestDescribeCacheEngineVersions $
--             newDescribeCacheEngineVersions
--
--         , requestModifyUserGroup $
--             newModifyUserGroup
--
--         , requestDescribeCacheParameters $
--             newDescribeCacheParameters
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
--         , requestDescribeUserGroups $
--             newDescribeUserGroups
--
--         , requestCreateSnapshot $
--             newCreateSnapshot
--
--         , requestDescribeCacheClusters $
--             newDescribeCacheClusters
--
--         , requestDescribeReservedCacheNodesOfferings $
--             newDescribeReservedCacheNodesOfferings
--
--         , requestDescribeReplicationGroups $
--             newDescribeReplicationGroups
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestModifyCacheParameterGroup $
--             newModifyCacheParameterGroup
--
--         , requestDescribeServiceUpdates $
--             newDescribeServiceUpdates
--
--         , requestDescribeEngineDefaultParameters $
--             newDescribeEngineDefaultParameters
--
--         , requestCreateUserGroup $
--             newCreateUserGroup
--
--         , requestRevokeCacheSecurityGroupIngress $
--             newRevokeCacheSecurityGroupIngress
--
--           ]

--     , testGroup "response"
--         [ responseCreateReplicationGroup $
--             newCreateReplicationGroupResponse
--
--         , responseDeleteCacheCluster $
--             newDeleteCacheClusterResponse
--
--         , responseRebootCacheCluster $
--             newRebootCacheClusterResponse
--
--         , responseIncreaseNodeGroupsInGlobalReplicationGroup $
--             newIncreaseNodeGroupsInGlobalReplicationGroupResponse
--
--         , responseDeleteUserGroup $
--             newUserGroup
--
--         , responseDeleteCacheSecurityGroup $
--             newDeleteCacheSecurityGroupResponse
--
--         , responseStartMigration $
--             newStartMigrationResponse
--
--         , responseAuthorizeCacheSecurityGroupIngress $
--             newAuthorizeCacheSecurityGroupIngressResponse
--
--         , responseCopySnapshot $
--             newCopySnapshotResponse
--
--         , responseDecreaseReplicaCount $
--             newDecreaseReplicaCountResponse
--
--         , responseRebalanceSlotsInGlobalReplicationGroup $
--             newRebalanceSlotsInGlobalReplicationGroupResponse
--
--         , responseCreateCacheSecurityGroup $
--             newCreateCacheSecurityGroupResponse
--
--         , responseDescribeCacheSubnetGroups $
--             newDescribeCacheSubnetGroupsResponse
--
--         , responseDescribeGlobalReplicationGroups $
--             newDescribeGlobalReplicationGroupsResponse
--
--         , responseModifyCacheCluster $
--             newModifyCacheClusterResponse
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
--         , responseBatchStopUpdateAction $
--             newUpdateActionResultsMessage
--
--         , responseModifyReplicationGroup $
--             newModifyReplicationGroupResponse
--
--         , responsePurchaseReservedCacheNodesOffering $
--             newPurchaseReservedCacheNodesOfferingResponse
--
--         , responseCreateUser $
--             newUser
--
--         , responseDeleteSnapshot $
--             newDeleteSnapshotResponse
--
--         , responseCompleteMigration $
--             newCompleteMigrationResponse
--
--         , responseCreateCacheCluster $
--             newCreateCacheClusterResponse
--
--         , responseDisassociateGlobalReplicationGroup $
--             newDisassociateGlobalReplicationGroupResponse
--
--         , responseDescribeEvents $
--             newDescribeEventsResponse
--
--         , responseDeleteReplicationGroup $
--             newDeleteReplicationGroupResponse
--
--         , responseDescribeSnapshots $
--             newDescribeSnapshotsResponse
--
--         , responseTestFailover $
--             newTestFailoverResponse
--
--         , responseBatchApplyUpdateAction $
--             newUpdateActionResultsMessage
--
--         , responseIncreaseReplicaCount $
--             newIncreaseReplicaCountResponse
--
--         , responseModifyReplicationGroupShardConfiguration $
--             newModifyReplicationGroupShardConfigurationResponse
--
--         , responseDescribeUsers $
--             newDescribeUsersResponse
--
--         , responseListAllowedNodeTypeModifications $
--             newListAllowedNodeTypeModificationsResponse
--
--         , responseResetCacheParameterGroup $
--             newCacheParameterGroupNameMessage
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
--         , responseDescribeUpdateActions $
--             newDescribeUpdateActionsResponse
--
--         , responseModifyUser $
--             newUser
--
--         , responseDeleteCacheSubnetGroup $
--             newDeleteCacheSubnetGroupResponse
--
--         , responseDeleteGlobalReplicationGroup $
--             newDeleteGlobalReplicationGroupResponse
--
--         , responseCreateCacheParameterGroup $
--             newCreateCacheParameterGroupResponse
--
--         , responseDescribeCacheEngineVersions $
--             newDescribeCacheEngineVersionsResponse
--
--         , responseModifyUserGroup $
--             newUserGroup
--
--         , responseDescribeCacheParameters $
--             newDescribeCacheParametersResponse
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
--         , responseDescribeUserGroups $
--             newDescribeUserGroupsResponse
--
--         , responseCreateSnapshot $
--             newCreateSnapshotResponse
--
--         , responseDescribeCacheClusters $
--             newDescribeCacheClustersResponse
--
--         , responseDescribeReservedCacheNodesOfferings $
--             newDescribeReservedCacheNodesOfferingsResponse
--
--         , responseDescribeReplicationGroups $
--             newDescribeReplicationGroupsResponse
--
--         , responseListTagsForResource $
--             newTagListMessage
--
--         , responseModifyCacheParameterGroup $
--             newCacheParameterGroupNameMessage
--
--         , responseDescribeServiceUpdates $
--             newDescribeServiceUpdatesResponse
--
--         , responseDescribeEngineDefaultParameters $
--             newDescribeEngineDefaultParametersResponse
--
--         , responseCreateUserGroup $
--             newUserGroup
--
--         , responseRevokeCacheSecurityGroupIngress $
--             newRevokeCacheSecurityGroupIngressResponse
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

requestDeleteUserGroup :: DeleteUserGroup -> TestTree
requestDeleteUserGroup =
  req
    "DeleteUserGroup"
    "fixture/DeleteUserGroup.yaml"

requestDeleteCacheSecurityGroup :: DeleteCacheSecurityGroup -> TestTree
requestDeleteCacheSecurityGroup =
  req
    "DeleteCacheSecurityGroup"
    "fixture/DeleteCacheSecurityGroup.yaml"

requestStartMigration :: StartMigration -> TestTree
requestStartMigration =
  req
    "StartMigration"
    "fixture/StartMigration.yaml"

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

requestDecreaseReplicaCount :: DecreaseReplicaCount -> TestTree
requestDecreaseReplicaCount =
  req
    "DecreaseReplicaCount"
    "fixture/DecreaseReplicaCount.yaml"

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

requestModifyCacheCluster :: ModifyCacheCluster -> TestTree
requestModifyCacheCluster =
  req
    "ModifyCacheCluster"
    "fixture/ModifyCacheCluster.yaml"

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

requestBatchStopUpdateAction :: BatchStopUpdateAction -> TestTree
requestBatchStopUpdateAction =
  req
    "BatchStopUpdateAction"
    "fixture/BatchStopUpdateAction.yaml"

requestModifyReplicationGroup :: ModifyReplicationGroup -> TestTree
requestModifyReplicationGroup =
  req
    "ModifyReplicationGroup"
    "fixture/ModifyReplicationGroup.yaml"

requestPurchaseReservedCacheNodesOffering :: PurchaseReservedCacheNodesOffering -> TestTree
requestPurchaseReservedCacheNodesOffering =
  req
    "PurchaseReservedCacheNodesOffering"
    "fixture/PurchaseReservedCacheNodesOffering.yaml"

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

requestCompleteMigration :: CompleteMigration -> TestTree
requestCompleteMigration =
  req
    "CompleteMigration"
    "fixture/CompleteMigration.yaml"

requestCreateCacheCluster :: CreateCacheCluster -> TestTree
requestCreateCacheCluster =
  req
    "CreateCacheCluster"
    "fixture/CreateCacheCluster.yaml"

requestDisassociateGlobalReplicationGroup :: DisassociateGlobalReplicationGroup -> TestTree
requestDisassociateGlobalReplicationGroup =
  req
    "DisassociateGlobalReplicationGroup"
    "fixture/DisassociateGlobalReplicationGroup.yaml"

requestDescribeEvents :: DescribeEvents -> TestTree
requestDescribeEvents =
  req
    "DescribeEvents"
    "fixture/DescribeEvents.yaml"

requestDeleteReplicationGroup :: DeleteReplicationGroup -> TestTree
requestDeleteReplicationGroup =
  req
    "DeleteReplicationGroup"
    "fixture/DeleteReplicationGroup.yaml"

requestDescribeSnapshots :: DescribeSnapshots -> TestTree
requestDescribeSnapshots =
  req
    "DescribeSnapshots"
    "fixture/DescribeSnapshots.yaml"

requestTestFailover :: TestFailover -> TestTree
requestTestFailover =
  req
    "TestFailover"
    "fixture/TestFailover.yaml"

requestBatchApplyUpdateAction :: BatchApplyUpdateAction -> TestTree
requestBatchApplyUpdateAction =
  req
    "BatchApplyUpdateAction"
    "fixture/BatchApplyUpdateAction.yaml"

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

requestDescribeUsers :: DescribeUsers -> TestTree
requestDescribeUsers =
  req
    "DescribeUsers"
    "fixture/DescribeUsers.yaml"

requestListAllowedNodeTypeModifications :: ListAllowedNodeTypeModifications -> TestTree
requestListAllowedNodeTypeModifications =
  req
    "ListAllowedNodeTypeModifications"
    "fixture/ListAllowedNodeTypeModifications.yaml"

requestResetCacheParameterGroup :: ResetCacheParameterGroup -> TestTree
requestResetCacheParameterGroup =
  req
    "ResetCacheParameterGroup"
    "fixture/ResetCacheParameterGroup.yaml"

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

requestDescribeUpdateActions :: DescribeUpdateActions -> TestTree
requestDescribeUpdateActions =
  req
    "DescribeUpdateActions"
    "fixture/DescribeUpdateActions.yaml"

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

requestDeleteGlobalReplicationGroup :: DeleteGlobalReplicationGroup -> TestTree
requestDeleteGlobalReplicationGroup =
  req
    "DeleteGlobalReplicationGroup"
    "fixture/DeleteGlobalReplicationGroup.yaml"

requestCreateCacheParameterGroup :: CreateCacheParameterGroup -> TestTree
requestCreateCacheParameterGroup =
  req
    "CreateCacheParameterGroup"
    "fixture/CreateCacheParameterGroup.yaml"

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

requestDescribeCacheParameters :: DescribeCacheParameters -> TestTree
requestDescribeCacheParameters =
  req
    "DescribeCacheParameters"
    "fixture/DescribeCacheParameters.yaml"

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

requestDescribeUserGroups :: DescribeUserGroups -> TestTree
requestDescribeUserGroups =
  req
    "DescribeUserGroups"
    "fixture/DescribeUserGroups.yaml"

requestCreateSnapshot :: CreateSnapshot -> TestTree
requestCreateSnapshot =
  req
    "CreateSnapshot"
    "fixture/CreateSnapshot.yaml"

requestDescribeCacheClusters :: DescribeCacheClusters -> TestTree
requestDescribeCacheClusters =
  req
    "DescribeCacheClusters"
    "fixture/DescribeCacheClusters.yaml"

requestDescribeReservedCacheNodesOfferings :: DescribeReservedCacheNodesOfferings -> TestTree
requestDescribeReservedCacheNodesOfferings =
  req
    "DescribeReservedCacheNodesOfferings"
    "fixture/DescribeReservedCacheNodesOfferings.yaml"

requestDescribeReplicationGroups :: DescribeReplicationGroups -> TestTree
requestDescribeReplicationGroups =
  req
    "DescribeReplicationGroups"
    "fixture/DescribeReplicationGroups.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

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

requestDescribeEngineDefaultParameters :: DescribeEngineDefaultParameters -> TestTree
requestDescribeEngineDefaultParameters =
  req
    "DescribeEngineDefaultParameters"
    "fixture/DescribeEngineDefaultParameters.yaml"

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

responseDeleteUserGroup :: UserGroup -> TestTree
responseDeleteUserGroup =
  res
    "DeleteUserGroupResponse"
    "fixture/DeleteUserGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUserGroup)

responseDeleteCacheSecurityGroup :: DeleteCacheSecurityGroupResponse -> TestTree
responseDeleteCacheSecurityGroup =
  res
    "DeleteCacheSecurityGroupResponse"
    "fixture/DeleteCacheSecurityGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCacheSecurityGroup)

responseStartMigration :: StartMigrationResponse -> TestTree
responseStartMigration =
  res
    "StartMigrationResponse"
    "fixture/StartMigrationResponse.proto"
    defaultService
    (Proxy :: Proxy StartMigration)

responseAuthorizeCacheSecurityGroupIngress :: AuthorizeCacheSecurityGroupIngressResponse -> TestTree
responseAuthorizeCacheSecurityGroupIngress =
  res
    "AuthorizeCacheSecurityGroupIngressResponse"
    "fixture/AuthorizeCacheSecurityGroupIngressResponse.proto"
    defaultService
    (Proxy :: Proxy AuthorizeCacheSecurityGroupIngress)

responseCopySnapshot :: CopySnapshotResponse -> TestTree
responseCopySnapshot =
  res
    "CopySnapshotResponse"
    "fixture/CopySnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy CopySnapshot)

responseDecreaseReplicaCount :: DecreaseReplicaCountResponse -> TestTree
responseDecreaseReplicaCount =
  res
    "DecreaseReplicaCountResponse"
    "fixture/DecreaseReplicaCountResponse.proto"
    defaultService
    (Proxy :: Proxy DecreaseReplicaCount)

responseRebalanceSlotsInGlobalReplicationGroup :: RebalanceSlotsInGlobalReplicationGroupResponse -> TestTree
responseRebalanceSlotsInGlobalReplicationGroup =
  res
    "RebalanceSlotsInGlobalReplicationGroupResponse"
    "fixture/RebalanceSlotsInGlobalReplicationGroupResponse.proto"
    defaultService
    (Proxy :: Proxy RebalanceSlotsInGlobalReplicationGroup)

responseCreateCacheSecurityGroup :: CreateCacheSecurityGroupResponse -> TestTree
responseCreateCacheSecurityGroup =
  res
    "CreateCacheSecurityGroupResponse"
    "fixture/CreateCacheSecurityGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCacheSecurityGroup)

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

responseModifyCacheCluster :: ModifyCacheClusterResponse -> TestTree
responseModifyCacheCluster =
  res
    "ModifyCacheClusterResponse"
    "fixture/ModifyCacheClusterResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyCacheCluster)

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

responseBatchStopUpdateAction :: UpdateActionResultsMessage -> TestTree
responseBatchStopUpdateAction =
  res
    "BatchStopUpdateActionResponse"
    "fixture/BatchStopUpdateActionResponse.proto"
    defaultService
    (Proxy :: Proxy BatchStopUpdateAction)

responseModifyReplicationGroup :: ModifyReplicationGroupResponse -> TestTree
responseModifyReplicationGroup =
  res
    "ModifyReplicationGroupResponse"
    "fixture/ModifyReplicationGroupResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyReplicationGroup)

responsePurchaseReservedCacheNodesOffering :: PurchaseReservedCacheNodesOfferingResponse -> TestTree
responsePurchaseReservedCacheNodesOffering =
  res
    "PurchaseReservedCacheNodesOfferingResponse"
    "fixture/PurchaseReservedCacheNodesOfferingResponse.proto"
    defaultService
    (Proxy :: Proxy PurchaseReservedCacheNodesOffering)

responseCreateUser :: User -> TestTree
responseCreateUser =
  res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUser)

responseDeleteSnapshot :: DeleteSnapshotResponse -> TestTree
responseDeleteSnapshot =
  res
    "DeleteSnapshotResponse"
    "fixture/DeleteSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSnapshot)

responseCompleteMigration :: CompleteMigrationResponse -> TestTree
responseCompleteMigration =
  res
    "CompleteMigrationResponse"
    "fixture/CompleteMigrationResponse.proto"
    defaultService
    (Proxy :: Proxy CompleteMigration)

responseCreateCacheCluster :: CreateCacheClusterResponse -> TestTree
responseCreateCacheCluster =
  res
    "CreateCacheClusterResponse"
    "fixture/CreateCacheClusterResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCacheCluster)

responseDisassociateGlobalReplicationGroup :: DisassociateGlobalReplicationGroupResponse -> TestTree
responseDisassociateGlobalReplicationGroup =
  res
    "DisassociateGlobalReplicationGroupResponse"
    "fixture/DisassociateGlobalReplicationGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateGlobalReplicationGroup)

responseDescribeEvents :: DescribeEventsResponse -> TestTree
responseDescribeEvents =
  res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEvents)

responseDeleteReplicationGroup :: DeleteReplicationGroupResponse -> TestTree
responseDeleteReplicationGroup =
  res
    "DeleteReplicationGroupResponse"
    "fixture/DeleteReplicationGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteReplicationGroup)

responseDescribeSnapshots :: DescribeSnapshotsResponse -> TestTree
responseDescribeSnapshots =
  res
    "DescribeSnapshotsResponse"
    "fixture/DescribeSnapshotsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSnapshots)

responseTestFailover :: TestFailoverResponse -> TestTree
responseTestFailover =
  res
    "TestFailoverResponse"
    "fixture/TestFailoverResponse.proto"
    defaultService
    (Proxy :: Proxy TestFailover)

responseBatchApplyUpdateAction :: UpdateActionResultsMessage -> TestTree
responseBatchApplyUpdateAction =
  res
    "BatchApplyUpdateActionResponse"
    "fixture/BatchApplyUpdateActionResponse.proto"
    defaultService
    (Proxy :: Proxy BatchApplyUpdateAction)

responseIncreaseReplicaCount :: IncreaseReplicaCountResponse -> TestTree
responseIncreaseReplicaCount =
  res
    "IncreaseReplicaCountResponse"
    "fixture/IncreaseReplicaCountResponse.proto"
    defaultService
    (Proxy :: Proxy IncreaseReplicaCount)

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

responseListAllowedNodeTypeModifications :: ListAllowedNodeTypeModificationsResponse -> TestTree
responseListAllowedNodeTypeModifications =
  res
    "ListAllowedNodeTypeModificationsResponse"
    "fixture/ListAllowedNodeTypeModificationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListAllowedNodeTypeModifications)

responseResetCacheParameterGroup :: CacheParameterGroupNameMessage -> TestTree
responseResetCacheParameterGroup =
  res
    "ResetCacheParameterGroupResponse"
    "fixture/ResetCacheParameterGroupResponse.proto"
    defaultService
    (Proxy :: Proxy ResetCacheParameterGroup)

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

responseDescribeUpdateActions :: DescribeUpdateActionsResponse -> TestTree
responseDescribeUpdateActions =
  res
    "DescribeUpdateActionsResponse"
    "fixture/DescribeUpdateActionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeUpdateActions)

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

responseDeleteGlobalReplicationGroup :: DeleteGlobalReplicationGroupResponse -> TestTree
responseDeleteGlobalReplicationGroup =
  res
    "DeleteGlobalReplicationGroupResponse"
    "fixture/DeleteGlobalReplicationGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteGlobalReplicationGroup)

responseCreateCacheParameterGroup :: CreateCacheParameterGroupResponse -> TestTree
responseCreateCacheParameterGroup =
  res
    "CreateCacheParameterGroupResponse"
    "fixture/CreateCacheParameterGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCacheParameterGroup)

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

responseDescribeCacheParameters :: DescribeCacheParametersResponse -> TestTree
responseDescribeCacheParameters =
  res
    "DescribeCacheParametersResponse"
    "fixture/DescribeCacheParametersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCacheParameters)

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

responseDescribeUserGroups :: DescribeUserGroupsResponse -> TestTree
responseDescribeUserGroups =
  res
    "DescribeUserGroupsResponse"
    "fixture/DescribeUserGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeUserGroups)

responseCreateSnapshot :: CreateSnapshotResponse -> TestTree
responseCreateSnapshot =
  res
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSnapshot)

responseDescribeCacheClusters :: DescribeCacheClustersResponse -> TestTree
responseDescribeCacheClusters =
  res
    "DescribeCacheClustersResponse"
    "fixture/DescribeCacheClustersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCacheClusters)

responseDescribeReservedCacheNodesOfferings :: DescribeReservedCacheNodesOfferingsResponse -> TestTree
responseDescribeReservedCacheNodesOfferings =
  res
    "DescribeReservedCacheNodesOfferingsResponse"
    "fixture/DescribeReservedCacheNodesOfferingsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeReservedCacheNodesOfferings)

responseDescribeReplicationGroups :: DescribeReplicationGroupsResponse -> TestTree
responseDescribeReplicationGroups =
  res
    "DescribeReplicationGroupsResponse"
    "fixture/DescribeReplicationGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeReplicationGroups)

responseListTagsForResource :: TagListMessage -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

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

responseDescribeEngineDefaultParameters :: DescribeEngineDefaultParametersResponse -> TestTree
responseDescribeEngineDefaultParameters =
  res
    "DescribeEngineDefaultParametersResponse"
    "fixture/DescribeEngineDefaultParametersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEngineDefaultParameters)

responseCreateUserGroup :: UserGroup -> TestTree
responseCreateUserGroup =
  res
    "CreateUserGroupResponse"
    "fixture/CreateUserGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUserGroup)

responseRevokeCacheSecurityGroupIngress :: RevokeCacheSecurityGroupIngressResponse -> TestTree
responseRevokeCacheSecurityGroupIngress =
  res
    "RevokeCacheSecurityGroupIngressResponse"
    "fixture/RevokeCacheSecurityGroupIngressResponse.proto"
    defaultService
    (Proxy :: Proxy RevokeCacheSecurityGroupIngress)
