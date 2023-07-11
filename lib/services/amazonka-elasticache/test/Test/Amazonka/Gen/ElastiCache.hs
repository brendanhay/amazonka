{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.ElastiCache
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.ElastiCache where

import Amazonka.ElastiCache
import qualified Data.Proxy as Proxy
import Test.Amazonka.ElastiCache.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAddTagsToResource $
--             newAddTagsToResource
--
--         , requestAuthorizeCacheSecurityGroupIngress $
--             newAuthorizeCacheSecurityGroupIngress
--
--         , requestBatchApplyUpdateAction $
--             newBatchApplyUpdateAction
--
--         , requestBatchStopUpdateAction $
--             newBatchStopUpdateAction
--
--         , requestCompleteMigration $
--             newCompleteMigration
--
--         , requestCopySnapshot $
--             newCopySnapshot
--
--         , requestCreateCacheCluster $
--             newCreateCacheCluster
--
--         , requestCreateCacheParameterGroup $
--             newCreateCacheParameterGroup
--
--         , requestCreateCacheSecurityGroup $
--             newCreateCacheSecurityGroup
--
--         , requestCreateCacheSubnetGroup $
--             newCreateCacheSubnetGroup
--
--         , requestCreateGlobalReplicationGroup $
--             newCreateGlobalReplicationGroup
--
--         , requestCreateReplicationGroup $
--             newCreateReplicationGroup
--
--         , requestCreateSnapshot $
--             newCreateSnapshot
--
--         , requestCreateUser $
--             newCreateUser
--
--         , requestCreateUserGroup $
--             newCreateUserGroup
--
--         , requestDecreaseNodeGroupsInGlobalReplicationGroup $
--             newDecreaseNodeGroupsInGlobalReplicationGroup
--
--         , requestDecreaseReplicaCount $
--             newDecreaseReplicaCount
--
--         , requestDeleteCacheCluster $
--             newDeleteCacheCluster
--
--         , requestDeleteCacheParameterGroup $
--             newDeleteCacheParameterGroup
--
--         , requestDeleteCacheSecurityGroup $
--             newDeleteCacheSecurityGroup
--
--         , requestDeleteCacheSubnetGroup $
--             newDeleteCacheSubnetGroup
--
--         , requestDeleteGlobalReplicationGroup $
--             newDeleteGlobalReplicationGroup
--
--         , requestDeleteReplicationGroup $
--             newDeleteReplicationGroup
--
--         , requestDeleteSnapshot $
--             newDeleteSnapshot
--
--         , requestDeleteUser $
--             newDeleteUser
--
--         , requestDeleteUserGroup $
--             newDeleteUserGroup
--
--         , requestDescribeCacheClusters $
--             newDescribeCacheClusters
--
--         , requestDescribeCacheEngineVersions $
--             newDescribeCacheEngineVersions
--
--         , requestDescribeCacheParameterGroups $
--             newDescribeCacheParameterGroups
--
--         , requestDescribeCacheParameters $
--             newDescribeCacheParameters
--
--         , requestDescribeCacheSecurityGroups $
--             newDescribeCacheSecurityGroups
--
--         , requestDescribeCacheSubnetGroups $
--             newDescribeCacheSubnetGroups
--
--         , requestDescribeEngineDefaultParameters $
--             newDescribeEngineDefaultParameters
--
--         , requestDescribeEvents $
--             newDescribeEvents
--
--         , requestDescribeGlobalReplicationGroups $
--             newDescribeGlobalReplicationGroups
--
--         , requestDescribeReplicationGroups $
--             newDescribeReplicationGroups
--
--         , requestDescribeReservedCacheNodes $
--             newDescribeReservedCacheNodes
--
--         , requestDescribeReservedCacheNodesOfferings $
--             newDescribeReservedCacheNodesOfferings
--
--         , requestDescribeServiceUpdates $
--             newDescribeServiceUpdates
--
--         , requestDescribeSnapshots $
--             newDescribeSnapshots
--
--         , requestDescribeUpdateActions $
--             newDescribeUpdateActions
--
--         , requestDescribeUserGroups $
--             newDescribeUserGroups
--
--         , requestDescribeUsers $
--             newDescribeUsers
--
--         , requestDisassociateGlobalReplicationGroup $
--             newDisassociateGlobalReplicationGroup
--
--         , requestFailoverGlobalReplicationGroup $
--             newFailoverGlobalReplicationGroup
--
--         , requestIncreaseNodeGroupsInGlobalReplicationGroup $
--             newIncreaseNodeGroupsInGlobalReplicationGroup
--
--         , requestIncreaseReplicaCount $
--             newIncreaseReplicaCount
--
--         , requestListAllowedNodeTypeModifications $
--             newListAllowedNodeTypeModifications
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestModifyCacheCluster $
--             newModifyCacheCluster
--
--         , requestModifyCacheParameterGroup $
--             newModifyCacheParameterGroup
--
--         , requestModifyCacheSubnetGroup $
--             newModifyCacheSubnetGroup
--
--         , requestModifyGlobalReplicationGroup $
--             newModifyGlobalReplicationGroup
--
--         , requestModifyReplicationGroup $
--             newModifyReplicationGroup
--
--         , requestModifyReplicationGroupShardConfiguration $
--             newModifyReplicationGroupShardConfiguration
--
--         , requestModifyUser $
--             newModifyUser
--
--         , requestModifyUserGroup $
--             newModifyUserGroup
--
--         , requestPurchaseReservedCacheNodesOffering $
--             newPurchaseReservedCacheNodesOffering
--
--         , requestRebalanceSlotsInGlobalReplicationGroup $
--             newRebalanceSlotsInGlobalReplicationGroup
--
--         , requestRebootCacheCluster $
--             newRebootCacheCluster
--
--         , requestRemoveTagsFromResource $
--             newRemoveTagsFromResource
--
--         , requestResetCacheParameterGroup $
--             newResetCacheParameterGroup
--
--         , requestRevokeCacheSecurityGroupIngress $
--             newRevokeCacheSecurityGroupIngress
--
--         , requestStartMigration $
--             newStartMigration
--
--         , requestTestFailover $
--             newTestFailover
--
--           ]

--     , testGroup "response"
--         [ responseAddTagsToResource $
--             newTagListMessage
--
--         , responseAuthorizeCacheSecurityGroupIngress $
--             newAuthorizeCacheSecurityGroupIngressResponse
--
--         , responseBatchApplyUpdateAction $
--             newUpdateActionResultsMessage
--
--         , responseBatchStopUpdateAction $
--             newUpdateActionResultsMessage
--
--         , responseCompleteMigration $
--             newCompleteMigrationResponse
--
--         , responseCopySnapshot $
--             newCopySnapshotResponse
--
--         , responseCreateCacheCluster $
--             newCreateCacheClusterResponse
--
--         , responseCreateCacheParameterGroup $
--             newCreateCacheParameterGroupResponse
--
--         , responseCreateCacheSecurityGroup $
--             newCreateCacheSecurityGroupResponse
--
--         , responseCreateCacheSubnetGroup $
--             newCreateCacheSubnetGroupResponse
--
--         , responseCreateGlobalReplicationGroup $
--             newCreateGlobalReplicationGroupResponse
--
--         , responseCreateReplicationGroup $
--             newCreateReplicationGroupResponse
--
--         , responseCreateSnapshot $
--             newCreateSnapshotResponse
--
--         , responseCreateUser $
--             newUser
--
--         , responseCreateUserGroup $
--             newUserGroup
--
--         , responseDecreaseNodeGroupsInGlobalReplicationGroup $
--             newDecreaseNodeGroupsInGlobalReplicationGroupResponse
--
--         , responseDecreaseReplicaCount $
--             newDecreaseReplicaCountResponse
--
--         , responseDeleteCacheCluster $
--             newDeleteCacheClusterResponse
--
--         , responseDeleteCacheParameterGroup $
--             newDeleteCacheParameterGroupResponse
--
--         , responseDeleteCacheSecurityGroup $
--             newDeleteCacheSecurityGroupResponse
--
--         , responseDeleteCacheSubnetGroup $
--             newDeleteCacheSubnetGroupResponse
--
--         , responseDeleteGlobalReplicationGroup $
--             newDeleteGlobalReplicationGroupResponse
--
--         , responseDeleteReplicationGroup $
--             newDeleteReplicationGroupResponse
--
--         , responseDeleteSnapshot $
--             newDeleteSnapshotResponse
--
--         , responseDeleteUser $
--             newUser
--
--         , responseDeleteUserGroup $
--             newUserGroup
--
--         , responseDescribeCacheClusters $
--             newDescribeCacheClustersResponse
--
--         , responseDescribeCacheEngineVersions $
--             newDescribeCacheEngineVersionsResponse
--
--         , responseDescribeCacheParameterGroups $
--             newDescribeCacheParameterGroupsResponse
--
--         , responseDescribeCacheParameters $
--             newDescribeCacheParametersResponse
--
--         , responseDescribeCacheSecurityGroups $
--             newDescribeCacheSecurityGroupsResponse
--
--         , responseDescribeCacheSubnetGroups $
--             newDescribeCacheSubnetGroupsResponse
--
--         , responseDescribeEngineDefaultParameters $
--             newDescribeEngineDefaultParametersResponse
--
--         , responseDescribeEvents $
--             newDescribeEventsResponse
--
--         , responseDescribeGlobalReplicationGroups $
--             newDescribeGlobalReplicationGroupsResponse
--
--         , responseDescribeReplicationGroups $
--             newDescribeReplicationGroupsResponse
--
--         , responseDescribeReservedCacheNodes $
--             newDescribeReservedCacheNodesResponse
--
--         , responseDescribeReservedCacheNodesOfferings $
--             newDescribeReservedCacheNodesOfferingsResponse
--
--         , responseDescribeServiceUpdates $
--             newDescribeServiceUpdatesResponse
--
--         , responseDescribeSnapshots $
--             newDescribeSnapshotsResponse
--
--         , responseDescribeUpdateActions $
--             newDescribeUpdateActionsResponse
--
--         , responseDescribeUserGroups $
--             newDescribeUserGroupsResponse
--
--         , responseDescribeUsers $
--             newDescribeUsersResponse
--
--         , responseDisassociateGlobalReplicationGroup $
--             newDisassociateGlobalReplicationGroupResponse
--
--         , responseFailoverGlobalReplicationGroup $
--             newFailoverGlobalReplicationGroupResponse
--
--         , responseIncreaseNodeGroupsInGlobalReplicationGroup $
--             newIncreaseNodeGroupsInGlobalReplicationGroupResponse
--
--         , responseIncreaseReplicaCount $
--             newIncreaseReplicaCountResponse
--
--         , responseListAllowedNodeTypeModifications $
--             newListAllowedNodeTypeModificationsResponse
--
--         , responseListTagsForResource $
--             newTagListMessage
--
--         , responseModifyCacheCluster $
--             newModifyCacheClusterResponse
--
--         , responseModifyCacheParameterGroup $
--             newCacheParameterGroupNameMessage
--
--         , responseModifyCacheSubnetGroup $
--             newModifyCacheSubnetGroupResponse
--
--         , responseModifyGlobalReplicationGroup $
--             newModifyGlobalReplicationGroupResponse
--
--         , responseModifyReplicationGroup $
--             newModifyReplicationGroupResponse
--
--         , responseModifyReplicationGroupShardConfiguration $
--             newModifyReplicationGroupShardConfigurationResponse
--
--         , responseModifyUser $
--             newUser
--
--         , responseModifyUserGroup $
--             newUserGroup
--
--         , responsePurchaseReservedCacheNodesOffering $
--             newPurchaseReservedCacheNodesOfferingResponse
--
--         , responseRebalanceSlotsInGlobalReplicationGroup $
--             newRebalanceSlotsInGlobalReplicationGroupResponse
--
--         , responseRebootCacheCluster $
--             newRebootCacheClusterResponse
--
--         , responseRemoveTagsFromResource $
--             newTagListMessage
--
--         , responseResetCacheParameterGroup $
--             newCacheParameterGroupNameMessage
--
--         , responseRevokeCacheSecurityGroupIngress $
--             newRevokeCacheSecurityGroupIngressResponse
--
--         , responseStartMigration $
--             newStartMigrationResponse
--
--         , responseTestFailover $
--             newTestFailoverResponse
--
--           ]
--     ]

-- Requests

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

requestBatchApplyUpdateAction :: BatchApplyUpdateAction -> TestTree
requestBatchApplyUpdateAction =
  req
    "BatchApplyUpdateAction"
    "fixture/BatchApplyUpdateAction.yaml"

requestBatchStopUpdateAction :: BatchStopUpdateAction -> TestTree
requestBatchStopUpdateAction =
  req
    "BatchStopUpdateAction"
    "fixture/BatchStopUpdateAction.yaml"

requestCompleteMigration :: CompleteMigration -> TestTree
requestCompleteMigration =
  req
    "CompleteMigration"
    "fixture/CompleteMigration.yaml"

requestCopySnapshot :: CopySnapshot -> TestTree
requestCopySnapshot =
  req
    "CopySnapshot"
    "fixture/CopySnapshot.yaml"

requestCreateCacheCluster :: CreateCacheCluster -> TestTree
requestCreateCacheCluster =
  req
    "CreateCacheCluster"
    "fixture/CreateCacheCluster.yaml"

requestCreateCacheParameterGroup :: CreateCacheParameterGroup -> TestTree
requestCreateCacheParameterGroup =
  req
    "CreateCacheParameterGroup"
    "fixture/CreateCacheParameterGroup.yaml"

requestCreateCacheSecurityGroup :: CreateCacheSecurityGroup -> TestTree
requestCreateCacheSecurityGroup =
  req
    "CreateCacheSecurityGroup"
    "fixture/CreateCacheSecurityGroup.yaml"

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

requestCreateReplicationGroup :: CreateReplicationGroup -> TestTree
requestCreateReplicationGroup =
  req
    "CreateReplicationGroup"
    "fixture/CreateReplicationGroup.yaml"

requestCreateSnapshot :: CreateSnapshot -> TestTree
requestCreateSnapshot =
  req
    "CreateSnapshot"
    "fixture/CreateSnapshot.yaml"

requestCreateUser :: CreateUser -> TestTree
requestCreateUser =
  req
    "CreateUser"
    "fixture/CreateUser.yaml"

requestCreateUserGroup :: CreateUserGroup -> TestTree
requestCreateUserGroup =
  req
    "CreateUserGroup"
    "fixture/CreateUserGroup.yaml"

requestDecreaseNodeGroupsInGlobalReplicationGroup :: DecreaseNodeGroupsInGlobalReplicationGroup -> TestTree
requestDecreaseNodeGroupsInGlobalReplicationGroup =
  req
    "DecreaseNodeGroupsInGlobalReplicationGroup"
    "fixture/DecreaseNodeGroupsInGlobalReplicationGroup.yaml"

requestDecreaseReplicaCount :: DecreaseReplicaCount -> TestTree
requestDecreaseReplicaCount =
  req
    "DecreaseReplicaCount"
    "fixture/DecreaseReplicaCount.yaml"

requestDeleteCacheCluster :: DeleteCacheCluster -> TestTree
requestDeleteCacheCluster =
  req
    "DeleteCacheCluster"
    "fixture/DeleteCacheCluster.yaml"

requestDeleteCacheParameterGroup :: DeleteCacheParameterGroup -> TestTree
requestDeleteCacheParameterGroup =
  req
    "DeleteCacheParameterGroup"
    "fixture/DeleteCacheParameterGroup.yaml"

requestDeleteCacheSecurityGroup :: DeleteCacheSecurityGroup -> TestTree
requestDeleteCacheSecurityGroup =
  req
    "DeleteCacheSecurityGroup"
    "fixture/DeleteCacheSecurityGroup.yaml"

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

requestDeleteReplicationGroup :: DeleteReplicationGroup -> TestTree
requestDeleteReplicationGroup =
  req
    "DeleteReplicationGroup"
    "fixture/DeleteReplicationGroup.yaml"

requestDeleteSnapshot :: DeleteSnapshot -> TestTree
requestDeleteSnapshot =
  req
    "DeleteSnapshot"
    "fixture/DeleteSnapshot.yaml"

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser =
  req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

requestDeleteUserGroup :: DeleteUserGroup -> TestTree
requestDeleteUserGroup =
  req
    "DeleteUserGroup"
    "fixture/DeleteUserGroup.yaml"

requestDescribeCacheClusters :: DescribeCacheClusters -> TestTree
requestDescribeCacheClusters =
  req
    "DescribeCacheClusters"
    "fixture/DescribeCacheClusters.yaml"

requestDescribeCacheEngineVersions :: DescribeCacheEngineVersions -> TestTree
requestDescribeCacheEngineVersions =
  req
    "DescribeCacheEngineVersions"
    "fixture/DescribeCacheEngineVersions.yaml"

requestDescribeCacheParameterGroups :: DescribeCacheParameterGroups -> TestTree
requestDescribeCacheParameterGroups =
  req
    "DescribeCacheParameterGroups"
    "fixture/DescribeCacheParameterGroups.yaml"

requestDescribeCacheParameters :: DescribeCacheParameters -> TestTree
requestDescribeCacheParameters =
  req
    "DescribeCacheParameters"
    "fixture/DescribeCacheParameters.yaml"

requestDescribeCacheSecurityGroups :: DescribeCacheSecurityGroups -> TestTree
requestDescribeCacheSecurityGroups =
  req
    "DescribeCacheSecurityGroups"
    "fixture/DescribeCacheSecurityGroups.yaml"

requestDescribeCacheSubnetGroups :: DescribeCacheSubnetGroups -> TestTree
requestDescribeCacheSubnetGroups =
  req
    "DescribeCacheSubnetGroups"
    "fixture/DescribeCacheSubnetGroups.yaml"

requestDescribeEngineDefaultParameters :: DescribeEngineDefaultParameters -> TestTree
requestDescribeEngineDefaultParameters =
  req
    "DescribeEngineDefaultParameters"
    "fixture/DescribeEngineDefaultParameters.yaml"

requestDescribeEvents :: DescribeEvents -> TestTree
requestDescribeEvents =
  req
    "DescribeEvents"
    "fixture/DescribeEvents.yaml"

requestDescribeGlobalReplicationGroups :: DescribeGlobalReplicationGroups -> TestTree
requestDescribeGlobalReplicationGroups =
  req
    "DescribeGlobalReplicationGroups"
    "fixture/DescribeGlobalReplicationGroups.yaml"

requestDescribeReplicationGroups :: DescribeReplicationGroups -> TestTree
requestDescribeReplicationGroups =
  req
    "DescribeReplicationGroups"
    "fixture/DescribeReplicationGroups.yaml"

requestDescribeReservedCacheNodes :: DescribeReservedCacheNodes -> TestTree
requestDescribeReservedCacheNodes =
  req
    "DescribeReservedCacheNodes"
    "fixture/DescribeReservedCacheNodes.yaml"

requestDescribeReservedCacheNodesOfferings :: DescribeReservedCacheNodesOfferings -> TestTree
requestDescribeReservedCacheNodesOfferings =
  req
    "DescribeReservedCacheNodesOfferings"
    "fixture/DescribeReservedCacheNodesOfferings.yaml"

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

requestDescribeUpdateActions :: DescribeUpdateActions -> TestTree
requestDescribeUpdateActions =
  req
    "DescribeUpdateActions"
    "fixture/DescribeUpdateActions.yaml"

requestDescribeUserGroups :: DescribeUserGroups -> TestTree
requestDescribeUserGroups =
  req
    "DescribeUserGroups"
    "fixture/DescribeUserGroups.yaml"

requestDescribeUsers :: DescribeUsers -> TestTree
requestDescribeUsers =
  req
    "DescribeUsers"
    "fixture/DescribeUsers.yaml"

requestDisassociateGlobalReplicationGroup :: DisassociateGlobalReplicationGroup -> TestTree
requestDisassociateGlobalReplicationGroup =
  req
    "DisassociateGlobalReplicationGroup"
    "fixture/DisassociateGlobalReplicationGroup.yaml"

requestFailoverGlobalReplicationGroup :: FailoverGlobalReplicationGroup -> TestTree
requestFailoverGlobalReplicationGroup =
  req
    "FailoverGlobalReplicationGroup"
    "fixture/FailoverGlobalReplicationGroup.yaml"

requestIncreaseNodeGroupsInGlobalReplicationGroup :: IncreaseNodeGroupsInGlobalReplicationGroup -> TestTree
requestIncreaseNodeGroupsInGlobalReplicationGroup =
  req
    "IncreaseNodeGroupsInGlobalReplicationGroup"
    "fixture/IncreaseNodeGroupsInGlobalReplicationGroup.yaml"

requestIncreaseReplicaCount :: IncreaseReplicaCount -> TestTree
requestIncreaseReplicaCount =
  req
    "IncreaseReplicaCount"
    "fixture/IncreaseReplicaCount.yaml"

requestListAllowedNodeTypeModifications :: ListAllowedNodeTypeModifications -> TestTree
requestListAllowedNodeTypeModifications =
  req
    "ListAllowedNodeTypeModifications"
    "fixture/ListAllowedNodeTypeModifications.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestModifyCacheCluster :: ModifyCacheCluster -> TestTree
requestModifyCacheCluster =
  req
    "ModifyCacheCluster"
    "fixture/ModifyCacheCluster.yaml"

requestModifyCacheParameterGroup :: ModifyCacheParameterGroup -> TestTree
requestModifyCacheParameterGroup =
  req
    "ModifyCacheParameterGroup"
    "fixture/ModifyCacheParameterGroup.yaml"

requestModifyCacheSubnetGroup :: ModifyCacheSubnetGroup -> TestTree
requestModifyCacheSubnetGroup =
  req
    "ModifyCacheSubnetGroup"
    "fixture/ModifyCacheSubnetGroup.yaml"

requestModifyGlobalReplicationGroup :: ModifyGlobalReplicationGroup -> TestTree
requestModifyGlobalReplicationGroup =
  req
    "ModifyGlobalReplicationGroup"
    "fixture/ModifyGlobalReplicationGroup.yaml"

requestModifyReplicationGroup :: ModifyReplicationGroup -> TestTree
requestModifyReplicationGroup =
  req
    "ModifyReplicationGroup"
    "fixture/ModifyReplicationGroup.yaml"

requestModifyReplicationGroupShardConfiguration :: ModifyReplicationGroupShardConfiguration -> TestTree
requestModifyReplicationGroupShardConfiguration =
  req
    "ModifyReplicationGroupShardConfiguration"
    "fixture/ModifyReplicationGroupShardConfiguration.yaml"

requestModifyUser :: ModifyUser -> TestTree
requestModifyUser =
  req
    "ModifyUser"
    "fixture/ModifyUser.yaml"

requestModifyUserGroup :: ModifyUserGroup -> TestTree
requestModifyUserGroup =
  req
    "ModifyUserGroup"
    "fixture/ModifyUserGroup.yaml"

requestPurchaseReservedCacheNodesOffering :: PurchaseReservedCacheNodesOffering -> TestTree
requestPurchaseReservedCacheNodesOffering =
  req
    "PurchaseReservedCacheNodesOffering"
    "fixture/PurchaseReservedCacheNodesOffering.yaml"

requestRebalanceSlotsInGlobalReplicationGroup :: RebalanceSlotsInGlobalReplicationGroup -> TestTree
requestRebalanceSlotsInGlobalReplicationGroup =
  req
    "RebalanceSlotsInGlobalReplicationGroup"
    "fixture/RebalanceSlotsInGlobalReplicationGroup.yaml"

requestRebootCacheCluster :: RebootCacheCluster -> TestTree
requestRebootCacheCluster =
  req
    "RebootCacheCluster"
    "fixture/RebootCacheCluster.yaml"

requestRemoveTagsFromResource :: RemoveTagsFromResource -> TestTree
requestRemoveTagsFromResource =
  req
    "RemoveTagsFromResource"
    "fixture/RemoveTagsFromResource.yaml"

requestResetCacheParameterGroup :: ResetCacheParameterGroup -> TestTree
requestResetCacheParameterGroup =
  req
    "ResetCacheParameterGroup"
    "fixture/ResetCacheParameterGroup.yaml"

requestRevokeCacheSecurityGroupIngress :: RevokeCacheSecurityGroupIngress -> TestTree
requestRevokeCacheSecurityGroupIngress =
  req
    "RevokeCacheSecurityGroupIngress"
    "fixture/RevokeCacheSecurityGroupIngress.yaml"

requestStartMigration :: StartMigration -> TestTree
requestStartMigration =
  req
    "StartMigration"
    "fixture/StartMigration.yaml"

requestTestFailover :: TestFailover -> TestTree
requestTestFailover =
  req
    "TestFailover"
    "fixture/TestFailover.yaml"

-- Responses

responseAddTagsToResource :: TagListMessage -> TestTree
responseAddTagsToResource =
  res
    "AddTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddTagsToResource)

responseAuthorizeCacheSecurityGroupIngress :: AuthorizeCacheSecurityGroupIngressResponse -> TestTree
responseAuthorizeCacheSecurityGroupIngress =
  res
    "AuthorizeCacheSecurityGroupIngressResponse"
    "fixture/AuthorizeCacheSecurityGroupIngressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AuthorizeCacheSecurityGroupIngress)

responseBatchApplyUpdateAction :: UpdateActionResultsMessage -> TestTree
responseBatchApplyUpdateAction =
  res
    "BatchApplyUpdateActionResponse"
    "fixture/BatchApplyUpdateActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchApplyUpdateAction)

responseBatchStopUpdateAction :: UpdateActionResultsMessage -> TestTree
responseBatchStopUpdateAction =
  res
    "BatchStopUpdateActionResponse"
    "fixture/BatchStopUpdateActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchStopUpdateAction)

responseCompleteMigration :: CompleteMigrationResponse -> TestTree
responseCompleteMigration =
  res
    "CompleteMigrationResponse"
    "fixture/CompleteMigrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CompleteMigration)

responseCopySnapshot :: CopySnapshotResponse -> TestTree
responseCopySnapshot =
  res
    "CopySnapshotResponse"
    "fixture/CopySnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CopySnapshot)

responseCreateCacheCluster :: CreateCacheClusterResponse -> TestTree
responseCreateCacheCluster =
  res
    "CreateCacheClusterResponse"
    "fixture/CreateCacheClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCacheCluster)

responseCreateCacheParameterGroup :: CreateCacheParameterGroupResponse -> TestTree
responseCreateCacheParameterGroup =
  res
    "CreateCacheParameterGroupResponse"
    "fixture/CreateCacheParameterGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCacheParameterGroup)

responseCreateCacheSecurityGroup :: CreateCacheSecurityGroupResponse -> TestTree
responseCreateCacheSecurityGroup =
  res
    "CreateCacheSecurityGroupResponse"
    "fixture/CreateCacheSecurityGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCacheSecurityGroup)

responseCreateCacheSubnetGroup :: CreateCacheSubnetGroupResponse -> TestTree
responseCreateCacheSubnetGroup =
  res
    "CreateCacheSubnetGroupResponse"
    "fixture/CreateCacheSubnetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCacheSubnetGroup)

responseCreateGlobalReplicationGroup :: CreateGlobalReplicationGroupResponse -> TestTree
responseCreateGlobalReplicationGroup =
  res
    "CreateGlobalReplicationGroupResponse"
    "fixture/CreateGlobalReplicationGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGlobalReplicationGroup)

responseCreateReplicationGroup :: CreateReplicationGroupResponse -> TestTree
responseCreateReplicationGroup =
  res
    "CreateReplicationGroupResponse"
    "fixture/CreateReplicationGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateReplicationGroup)

responseCreateSnapshot :: CreateSnapshotResponse -> TestTree
responseCreateSnapshot =
  res
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSnapshot)

responseCreateUser :: User -> TestTree
responseCreateUser =
  res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUser)

responseCreateUserGroup :: UserGroup -> TestTree
responseCreateUserGroup =
  res
    "CreateUserGroupResponse"
    "fixture/CreateUserGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUserGroup)

responseDecreaseNodeGroupsInGlobalReplicationGroup :: DecreaseNodeGroupsInGlobalReplicationGroupResponse -> TestTree
responseDecreaseNodeGroupsInGlobalReplicationGroup =
  res
    "DecreaseNodeGroupsInGlobalReplicationGroupResponse"
    "fixture/DecreaseNodeGroupsInGlobalReplicationGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DecreaseNodeGroupsInGlobalReplicationGroup)

responseDecreaseReplicaCount :: DecreaseReplicaCountResponse -> TestTree
responseDecreaseReplicaCount =
  res
    "DecreaseReplicaCountResponse"
    "fixture/DecreaseReplicaCountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DecreaseReplicaCount)

responseDeleteCacheCluster :: DeleteCacheClusterResponse -> TestTree
responseDeleteCacheCluster =
  res
    "DeleteCacheClusterResponse"
    "fixture/DeleteCacheClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCacheCluster)

responseDeleteCacheParameterGroup :: DeleteCacheParameterGroupResponse -> TestTree
responseDeleteCacheParameterGroup =
  res
    "DeleteCacheParameterGroupResponse"
    "fixture/DeleteCacheParameterGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCacheParameterGroup)

responseDeleteCacheSecurityGroup :: DeleteCacheSecurityGroupResponse -> TestTree
responseDeleteCacheSecurityGroup =
  res
    "DeleteCacheSecurityGroupResponse"
    "fixture/DeleteCacheSecurityGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCacheSecurityGroup)

responseDeleteCacheSubnetGroup :: DeleteCacheSubnetGroupResponse -> TestTree
responseDeleteCacheSubnetGroup =
  res
    "DeleteCacheSubnetGroupResponse"
    "fixture/DeleteCacheSubnetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCacheSubnetGroup)

responseDeleteGlobalReplicationGroup :: DeleteGlobalReplicationGroupResponse -> TestTree
responseDeleteGlobalReplicationGroup =
  res
    "DeleteGlobalReplicationGroupResponse"
    "fixture/DeleteGlobalReplicationGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGlobalReplicationGroup)

responseDeleteReplicationGroup :: DeleteReplicationGroupResponse -> TestTree
responseDeleteReplicationGroup =
  res
    "DeleteReplicationGroupResponse"
    "fixture/DeleteReplicationGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteReplicationGroup)

responseDeleteSnapshot :: DeleteSnapshotResponse -> TestTree
responseDeleteSnapshot =
  res
    "DeleteSnapshotResponse"
    "fixture/DeleteSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSnapshot)

responseDeleteUser :: User -> TestTree
responseDeleteUser =
  res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUser)

responseDeleteUserGroup :: UserGroup -> TestTree
responseDeleteUserGroup =
  res
    "DeleteUserGroupResponse"
    "fixture/DeleteUserGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUserGroup)

responseDescribeCacheClusters :: DescribeCacheClustersResponse -> TestTree
responseDescribeCacheClusters =
  res
    "DescribeCacheClustersResponse"
    "fixture/DescribeCacheClustersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCacheClusters)

responseDescribeCacheEngineVersions :: DescribeCacheEngineVersionsResponse -> TestTree
responseDescribeCacheEngineVersions =
  res
    "DescribeCacheEngineVersionsResponse"
    "fixture/DescribeCacheEngineVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCacheEngineVersions)

responseDescribeCacheParameterGroups :: DescribeCacheParameterGroupsResponse -> TestTree
responseDescribeCacheParameterGroups =
  res
    "DescribeCacheParameterGroupsResponse"
    "fixture/DescribeCacheParameterGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCacheParameterGroups)

responseDescribeCacheParameters :: DescribeCacheParametersResponse -> TestTree
responseDescribeCacheParameters =
  res
    "DescribeCacheParametersResponse"
    "fixture/DescribeCacheParametersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCacheParameters)

responseDescribeCacheSecurityGroups :: DescribeCacheSecurityGroupsResponse -> TestTree
responseDescribeCacheSecurityGroups =
  res
    "DescribeCacheSecurityGroupsResponse"
    "fixture/DescribeCacheSecurityGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCacheSecurityGroups)

responseDescribeCacheSubnetGroups :: DescribeCacheSubnetGroupsResponse -> TestTree
responseDescribeCacheSubnetGroups =
  res
    "DescribeCacheSubnetGroupsResponse"
    "fixture/DescribeCacheSubnetGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCacheSubnetGroups)

responseDescribeEngineDefaultParameters :: DescribeEngineDefaultParametersResponse -> TestTree
responseDescribeEngineDefaultParameters =
  res
    "DescribeEngineDefaultParametersResponse"
    "fixture/DescribeEngineDefaultParametersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEngineDefaultParameters)

responseDescribeEvents :: DescribeEventsResponse -> TestTree
responseDescribeEvents =
  res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEvents)

responseDescribeGlobalReplicationGroups :: DescribeGlobalReplicationGroupsResponse -> TestTree
responseDescribeGlobalReplicationGroups =
  res
    "DescribeGlobalReplicationGroupsResponse"
    "fixture/DescribeGlobalReplicationGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeGlobalReplicationGroups)

responseDescribeReplicationGroups :: DescribeReplicationGroupsResponse -> TestTree
responseDescribeReplicationGroups =
  res
    "DescribeReplicationGroupsResponse"
    "fixture/DescribeReplicationGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReplicationGroups)

responseDescribeReservedCacheNodes :: DescribeReservedCacheNodesResponse -> TestTree
responseDescribeReservedCacheNodes =
  res
    "DescribeReservedCacheNodesResponse"
    "fixture/DescribeReservedCacheNodesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReservedCacheNodes)

responseDescribeReservedCacheNodesOfferings :: DescribeReservedCacheNodesOfferingsResponse -> TestTree
responseDescribeReservedCacheNodesOfferings =
  res
    "DescribeReservedCacheNodesOfferingsResponse"
    "fixture/DescribeReservedCacheNodesOfferingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReservedCacheNodesOfferings)

responseDescribeServiceUpdates :: DescribeServiceUpdatesResponse -> TestTree
responseDescribeServiceUpdates =
  res
    "DescribeServiceUpdatesResponse"
    "fixture/DescribeServiceUpdatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeServiceUpdates)

responseDescribeSnapshots :: DescribeSnapshotsResponse -> TestTree
responseDescribeSnapshots =
  res
    "DescribeSnapshotsResponse"
    "fixture/DescribeSnapshotsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSnapshots)

responseDescribeUpdateActions :: DescribeUpdateActionsResponse -> TestTree
responseDescribeUpdateActions =
  res
    "DescribeUpdateActionsResponse"
    "fixture/DescribeUpdateActionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeUpdateActions)

responseDescribeUserGroups :: DescribeUserGroupsResponse -> TestTree
responseDescribeUserGroups =
  res
    "DescribeUserGroupsResponse"
    "fixture/DescribeUserGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeUserGroups)

responseDescribeUsers :: DescribeUsersResponse -> TestTree
responseDescribeUsers =
  res
    "DescribeUsersResponse"
    "fixture/DescribeUsersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeUsers)

responseDisassociateGlobalReplicationGroup :: DisassociateGlobalReplicationGroupResponse -> TestTree
responseDisassociateGlobalReplicationGroup =
  res
    "DisassociateGlobalReplicationGroupResponse"
    "fixture/DisassociateGlobalReplicationGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateGlobalReplicationGroup)

responseFailoverGlobalReplicationGroup :: FailoverGlobalReplicationGroupResponse -> TestTree
responseFailoverGlobalReplicationGroup =
  res
    "FailoverGlobalReplicationGroupResponse"
    "fixture/FailoverGlobalReplicationGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy FailoverGlobalReplicationGroup)

responseIncreaseNodeGroupsInGlobalReplicationGroup :: IncreaseNodeGroupsInGlobalReplicationGroupResponse -> TestTree
responseIncreaseNodeGroupsInGlobalReplicationGroup =
  res
    "IncreaseNodeGroupsInGlobalReplicationGroupResponse"
    "fixture/IncreaseNodeGroupsInGlobalReplicationGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy IncreaseNodeGroupsInGlobalReplicationGroup)

responseIncreaseReplicaCount :: IncreaseReplicaCountResponse -> TestTree
responseIncreaseReplicaCount =
  res
    "IncreaseReplicaCountResponse"
    "fixture/IncreaseReplicaCountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy IncreaseReplicaCount)

responseListAllowedNodeTypeModifications :: ListAllowedNodeTypeModificationsResponse -> TestTree
responseListAllowedNodeTypeModifications =
  res
    "ListAllowedNodeTypeModificationsResponse"
    "fixture/ListAllowedNodeTypeModificationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAllowedNodeTypeModifications)

responseListTagsForResource :: TagListMessage -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseModifyCacheCluster :: ModifyCacheClusterResponse -> TestTree
responseModifyCacheCluster =
  res
    "ModifyCacheClusterResponse"
    "fixture/ModifyCacheClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyCacheCluster)

responseModifyCacheParameterGroup :: CacheParameterGroupNameMessage -> TestTree
responseModifyCacheParameterGroup =
  res
    "ModifyCacheParameterGroupResponse"
    "fixture/ModifyCacheParameterGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyCacheParameterGroup)

responseModifyCacheSubnetGroup :: ModifyCacheSubnetGroupResponse -> TestTree
responseModifyCacheSubnetGroup =
  res
    "ModifyCacheSubnetGroupResponse"
    "fixture/ModifyCacheSubnetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyCacheSubnetGroup)

responseModifyGlobalReplicationGroup :: ModifyGlobalReplicationGroupResponse -> TestTree
responseModifyGlobalReplicationGroup =
  res
    "ModifyGlobalReplicationGroupResponse"
    "fixture/ModifyGlobalReplicationGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyGlobalReplicationGroup)

responseModifyReplicationGroup :: ModifyReplicationGroupResponse -> TestTree
responseModifyReplicationGroup =
  res
    "ModifyReplicationGroupResponse"
    "fixture/ModifyReplicationGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyReplicationGroup)

responseModifyReplicationGroupShardConfiguration :: ModifyReplicationGroupShardConfigurationResponse -> TestTree
responseModifyReplicationGroupShardConfiguration =
  res
    "ModifyReplicationGroupShardConfigurationResponse"
    "fixture/ModifyReplicationGroupShardConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyReplicationGroupShardConfiguration)

responseModifyUser :: User -> TestTree
responseModifyUser =
  res
    "ModifyUserResponse"
    "fixture/ModifyUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyUser)

responseModifyUserGroup :: UserGroup -> TestTree
responseModifyUserGroup =
  res
    "ModifyUserGroupResponse"
    "fixture/ModifyUserGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyUserGroup)

responsePurchaseReservedCacheNodesOffering :: PurchaseReservedCacheNodesOfferingResponse -> TestTree
responsePurchaseReservedCacheNodesOffering =
  res
    "PurchaseReservedCacheNodesOfferingResponse"
    "fixture/PurchaseReservedCacheNodesOfferingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PurchaseReservedCacheNodesOffering)

responseRebalanceSlotsInGlobalReplicationGroup :: RebalanceSlotsInGlobalReplicationGroupResponse -> TestTree
responseRebalanceSlotsInGlobalReplicationGroup =
  res
    "RebalanceSlotsInGlobalReplicationGroupResponse"
    "fixture/RebalanceSlotsInGlobalReplicationGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RebalanceSlotsInGlobalReplicationGroup)

responseRebootCacheCluster :: RebootCacheClusterResponse -> TestTree
responseRebootCacheCluster =
  res
    "RebootCacheClusterResponse"
    "fixture/RebootCacheClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RebootCacheCluster)

responseRemoveTagsFromResource :: TagListMessage -> TestTree
responseRemoveTagsFromResource =
  res
    "RemoveTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveTagsFromResource)

responseResetCacheParameterGroup :: CacheParameterGroupNameMessage -> TestTree
responseResetCacheParameterGroup =
  res
    "ResetCacheParameterGroupResponse"
    "fixture/ResetCacheParameterGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResetCacheParameterGroup)

responseRevokeCacheSecurityGroupIngress :: RevokeCacheSecurityGroupIngressResponse -> TestTree
responseRevokeCacheSecurityGroupIngress =
  res
    "RevokeCacheSecurityGroupIngressResponse"
    "fixture/RevokeCacheSecurityGroupIngressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RevokeCacheSecurityGroupIngress)

responseStartMigration :: StartMigrationResponse -> TestTree
responseStartMigration =
  res
    "StartMigrationResponse"
    "fixture/StartMigrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartMigration)

responseTestFailover :: TestFailoverResponse -> TestTree
responseTestFailover =
  res
    "TestFailoverResponse"
    "fixture/TestFailoverResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TestFailover)
