{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.ElastiCache
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
--         [ requestStartMigration $
--             newStartMigration
--
--         , requestDeleteCacheSecurityGroup $
--             newDeleteCacheSecurityGroup
--
--         , requestCreateReplicationGroup $
--             newCreateReplicationGroup
--
--         , requestDeleteCacheCluster $
--             newDeleteCacheCluster
--
--         , requestIncreaseNodeGroupsInGlobalReplicationGroup $
--             newIncreaseNodeGroupsInGlobalReplicationGroup
--
--         , requestDescribeUsers $
--             newDescribeUsers
--
--         , requestRebootCacheCluster $
--             newRebootCacheCluster
--
--         , requestCreateUserGroup $
--             newCreateUserGroup
--
--         , requestRevokeCacheSecurityGroupIngress $
--             newRevokeCacheSecurityGroupIngress
--
--         , requestCreateCacheCluster $
--             newCreateCacheCluster
--
--         , requestDescribeEvents $
--             newDescribeEvents
--
--         , requestDescribeEngineDefaultParameters $
--             newDescribeEngineDefaultParameters
--
--         , requestDisassociateGlobalReplicationGroup $
--             newDisassociateGlobalReplicationGroup
--
--         , requestModifyCacheParameterGroup $
--             newModifyCacheParameterGroup
--
--         , requestTestFailover $
--             newTestFailover
--
--         , requestDeleteReplicationGroup $
--             newDeleteReplicationGroup
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestCompleteMigration $
--             newCompleteMigration
--
--         , requestDescribeCacheClusters $
--             newDescribeCacheClusters
--
--         , requestPurchaseReservedCacheNodesOffering $
--             newPurchaseReservedCacheNodesOffering
--
--         , requestRemoveTagsFromResource $
--             newRemoveTagsFromResource
--
--         , requestModifyReplicationGroup $
--             newModifyReplicationGroup
--
--         , requestDescribeCacheParameters $
--             newDescribeCacheParameters
--
--         , requestDescribeGlobalReplicationGroups $
--             newDescribeGlobalReplicationGroups
--
--         , requestDescribeCacheSubnetGroups $
--             newDescribeCacheSubnetGroups
--
--         , requestDescribeUpdateActions $
--             newDescribeUpdateActions
--
--         , requestRebalanceSlotsInGlobalReplicationGroup $
--             newRebalanceSlotsInGlobalReplicationGroup
--
--         , requestCreateCacheSecurityGroup $
--             newCreateCacheSecurityGroup
--
--         , requestDecreaseReplicaCount $
--             newDecreaseReplicaCount
--
--         , requestAddTagsToResource $
--             newAddTagsToResource
--
--         , requestAuthorizeCacheSecurityGroupIngress $
--             newAuthorizeCacheSecurityGroupIngress
--
--         , requestCopySnapshot $
--             newCopySnapshot
--
--         , requestFailoverGlobalReplicationGroup $
--             newFailoverGlobalReplicationGroup
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
--         , requestResetCacheParameterGroup $
--             newResetCacheParameterGroup
--
--         , requestListAllowedNodeTypeModifications $
--             newListAllowedNodeTypeModifications
--
--         , requestIncreaseReplicaCount $
--             newIncreaseReplicaCount
--
--         , requestModifyReplicationGroupShardConfiguration $
--             newModifyReplicationGroupShardConfiguration
--
--         , requestBatchApplyUpdateAction $
--             newBatchApplyUpdateAction
--
--         , requestDeleteUserGroup $
--             newDeleteUserGroup
--
--         , requestDescribeServiceUpdates $
--             newDescribeServiceUpdates
--
--         , requestDescribeSnapshots $
--             newDescribeSnapshots
--
--         , requestDescribeReplicationGroups $
--             newDescribeReplicationGroups
--
--         , requestCreateUser $
--             newCreateUser
--
--         , requestDeleteSnapshot $
--             newDeleteSnapshot
--
--         , requestDescribeReservedCacheNodesOfferings $
--             newDescribeReservedCacheNodesOfferings
--
--         , requestModifyCacheSubnetGroup $
--             newModifyCacheSubnetGroup
--
--         , requestDeleteUser $
--             newDeleteUser
--
--         , requestCreateSnapshot $
--             newCreateSnapshot
--
--         , requestModifyGlobalReplicationGroup $
--             newModifyGlobalReplicationGroup
--
--         , requestDescribeUserGroups $
--             newDescribeUserGroups
--
--         , requestDeleteCacheParameterGroup $
--             newDeleteCacheParameterGroup
--
--         , requestDescribeCacheSecurityGroups $
--             newDescribeCacheSecurityGroups
--
--         , requestBatchStopUpdateAction $
--             newBatchStopUpdateAction
--
--         , requestModifyCacheCluster $
--             newModifyCacheCluster
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
--         , requestDescribeReservedCacheNodes $
--             newDescribeReservedCacheNodes
--
--         , requestDeleteGlobalReplicationGroup $
--             newDeleteGlobalReplicationGroup
--
--         , requestDecreaseNodeGroupsInGlobalReplicationGroup $
--             newDecreaseNodeGroupsInGlobalReplicationGroup
--
--         , requestModifyUser $
--             newModifyUser
--
--         , requestDeleteCacheSubnetGroup $
--             newDeleteCacheSubnetGroup
--
--           ]

--     , testGroup "response"
--         [ responseStartMigration $
--             newStartMigrationResponse
--
--         , responseDeleteCacheSecurityGroup $
--             newDeleteCacheSecurityGroupResponse
--
--         , responseCreateReplicationGroup $
--             newCreateReplicationGroupResponse
--
--         , responseDeleteCacheCluster $
--             newDeleteCacheClusterResponse
--
--         , responseIncreaseNodeGroupsInGlobalReplicationGroup $
--             newIncreaseNodeGroupsInGlobalReplicationGroupResponse
--
--         , responseDescribeUsers $
--             newDescribeUsersResponse
--
--         , responseRebootCacheCluster $
--             newRebootCacheClusterResponse
--
--         , responseCreateUserGroup $
--             newUserGroup
--
--         , responseRevokeCacheSecurityGroupIngress $
--             newRevokeCacheSecurityGroupIngressResponse
--
--         , responseCreateCacheCluster $
--             newCreateCacheClusterResponse
--
--         , responseDescribeEvents $
--             newDescribeEventsResponse
--
--         , responseDescribeEngineDefaultParameters $
--             newDescribeEngineDefaultParametersResponse
--
--         , responseDisassociateGlobalReplicationGroup $
--             newDisassociateGlobalReplicationGroupResponse
--
--         , responseModifyCacheParameterGroup $
--             newCacheParameterGroupNameMessage
--
--         , responseTestFailover $
--             newTestFailoverResponse
--
--         , responseDeleteReplicationGroup $
--             newDeleteReplicationGroupResponse
--
--         , responseListTagsForResource $
--             newTagListMessage
--
--         , responseCompleteMigration $
--             newCompleteMigrationResponse
--
--         , responseDescribeCacheClusters $
--             newDescribeCacheClustersResponse
--
--         , responsePurchaseReservedCacheNodesOffering $
--             newPurchaseReservedCacheNodesOfferingResponse
--
--         , responseRemoveTagsFromResource $
--             newTagListMessage
--
--         , responseModifyReplicationGroup $
--             newModifyReplicationGroupResponse
--
--         , responseDescribeCacheParameters $
--             newDescribeCacheParametersResponse
--
--         , responseDescribeGlobalReplicationGroups $
--             newDescribeGlobalReplicationGroupsResponse
--
--         , responseDescribeCacheSubnetGroups $
--             newDescribeCacheSubnetGroupsResponse
--
--         , responseDescribeUpdateActions $
--             newDescribeUpdateActionsResponse
--
--         , responseRebalanceSlotsInGlobalReplicationGroup $
--             newRebalanceSlotsInGlobalReplicationGroupResponse
--
--         , responseCreateCacheSecurityGroup $
--             newCreateCacheSecurityGroupResponse
--
--         , responseDecreaseReplicaCount $
--             newDecreaseReplicaCountResponse
--
--         , responseAddTagsToResource $
--             newTagListMessage
--
--         , responseAuthorizeCacheSecurityGroupIngress $
--             newAuthorizeCacheSecurityGroupIngressResponse
--
--         , responseCopySnapshot $
--             newCopySnapshotResponse
--
--         , responseFailoverGlobalReplicationGroup $
--             newFailoverGlobalReplicationGroupResponse
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
--         , responseResetCacheParameterGroup $
--             newCacheParameterGroupNameMessage
--
--         , responseListAllowedNodeTypeModifications $
--             newListAllowedNodeTypeModificationsResponse
--
--         , responseIncreaseReplicaCount $
--             newIncreaseReplicaCountResponse
--
--         , responseModifyReplicationGroupShardConfiguration $
--             newModifyReplicationGroupShardConfigurationResponse
--
--         , responseBatchApplyUpdateAction $
--             newUpdateActionResultsMessage
--
--         , responseDeleteUserGroup $
--             newUserGroup
--
--         , responseDescribeServiceUpdates $
--             newDescribeServiceUpdatesResponse
--
--         , responseDescribeSnapshots $
--             newDescribeSnapshotsResponse
--
--         , responseDescribeReplicationGroups $
--             newDescribeReplicationGroupsResponse
--
--         , responseCreateUser $
--             newUser
--
--         , responseDeleteSnapshot $
--             newDeleteSnapshotResponse
--
--         , responseDescribeReservedCacheNodesOfferings $
--             newDescribeReservedCacheNodesOfferingsResponse
--
--         , responseModifyCacheSubnetGroup $
--             newModifyCacheSubnetGroupResponse
--
--         , responseDeleteUser $
--             newUser
--
--         , responseCreateSnapshot $
--             newCreateSnapshotResponse
--
--         , responseModifyGlobalReplicationGroup $
--             newModifyGlobalReplicationGroupResponse
--
--         , responseDescribeUserGroups $
--             newDescribeUserGroupsResponse
--
--         , responseDeleteCacheParameterGroup $
--             newDeleteCacheParameterGroupResponse
--
--         , responseDescribeCacheSecurityGroups $
--             newDescribeCacheSecurityGroupsResponse
--
--         , responseBatchStopUpdateAction $
--             newUpdateActionResultsMessage
--
--         , responseModifyCacheCluster $
--             newModifyCacheClusterResponse
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
--         , responseDescribeReservedCacheNodes $
--             newDescribeReservedCacheNodesResponse
--
--         , responseDeleteGlobalReplicationGroup $
--             newDeleteGlobalReplicationGroupResponse
--
--         , responseDecreaseNodeGroupsInGlobalReplicationGroup $
--             newDecreaseNodeGroupsInGlobalReplicationGroupResponse
--
--         , responseModifyUser $
--             newUser
--
--         , responseDeleteCacheSubnetGroup $
--             newDeleteCacheSubnetGroupResponse
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
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartMigration)

responseDeleteCacheSecurityGroup :: DeleteCacheSecurityGroupResponse -> TestTree
responseDeleteCacheSecurityGroup =
  res
    "DeleteCacheSecurityGroupResponse"
    "fixture/DeleteCacheSecurityGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCacheSecurityGroup)

responseCreateReplicationGroup :: CreateReplicationGroupResponse -> TestTree
responseCreateReplicationGroup =
  res
    "CreateReplicationGroupResponse"
    "fixture/CreateReplicationGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateReplicationGroup)

responseDeleteCacheCluster :: DeleteCacheClusterResponse -> TestTree
responseDeleteCacheCluster =
  res
    "DeleteCacheClusterResponse"
    "fixture/DeleteCacheClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCacheCluster)

responseIncreaseNodeGroupsInGlobalReplicationGroup :: IncreaseNodeGroupsInGlobalReplicationGroupResponse -> TestTree
responseIncreaseNodeGroupsInGlobalReplicationGroup =
  res
    "IncreaseNodeGroupsInGlobalReplicationGroupResponse"
    "fixture/IncreaseNodeGroupsInGlobalReplicationGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy IncreaseNodeGroupsInGlobalReplicationGroup)

responseDescribeUsers :: DescribeUsersResponse -> TestTree
responseDescribeUsers =
  res
    "DescribeUsersResponse"
    "fixture/DescribeUsersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeUsers)

responseRebootCacheCluster :: RebootCacheClusterResponse -> TestTree
responseRebootCacheCluster =
  res
    "RebootCacheClusterResponse"
    "fixture/RebootCacheClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RebootCacheCluster)

responseCreateUserGroup :: UserGroup -> TestTree
responseCreateUserGroup =
  res
    "CreateUserGroupResponse"
    "fixture/CreateUserGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUserGroup)

responseRevokeCacheSecurityGroupIngress :: RevokeCacheSecurityGroupIngressResponse -> TestTree
responseRevokeCacheSecurityGroupIngress =
  res
    "RevokeCacheSecurityGroupIngressResponse"
    "fixture/RevokeCacheSecurityGroupIngressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RevokeCacheSecurityGroupIngress)

responseCreateCacheCluster :: CreateCacheClusterResponse -> TestTree
responseCreateCacheCluster =
  res
    "CreateCacheClusterResponse"
    "fixture/CreateCacheClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCacheCluster)

responseDescribeEvents :: DescribeEventsResponse -> TestTree
responseDescribeEvents =
  res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEvents)

responseDescribeEngineDefaultParameters :: DescribeEngineDefaultParametersResponse -> TestTree
responseDescribeEngineDefaultParameters =
  res
    "DescribeEngineDefaultParametersResponse"
    "fixture/DescribeEngineDefaultParametersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEngineDefaultParameters)

responseDisassociateGlobalReplicationGroup :: DisassociateGlobalReplicationGroupResponse -> TestTree
responseDisassociateGlobalReplicationGroup =
  res
    "DisassociateGlobalReplicationGroupResponse"
    "fixture/DisassociateGlobalReplicationGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateGlobalReplicationGroup)

responseModifyCacheParameterGroup :: CacheParameterGroupNameMessage -> TestTree
responseModifyCacheParameterGroup =
  res
    "ModifyCacheParameterGroupResponse"
    "fixture/ModifyCacheParameterGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyCacheParameterGroup)

responseTestFailover :: TestFailoverResponse -> TestTree
responseTestFailover =
  res
    "TestFailoverResponse"
    "fixture/TestFailoverResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TestFailover)

responseDeleteReplicationGroup :: DeleteReplicationGroupResponse -> TestTree
responseDeleteReplicationGroup =
  res
    "DeleteReplicationGroupResponse"
    "fixture/DeleteReplicationGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteReplicationGroup)

responseListTagsForResource :: TagListMessage -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseCompleteMigration :: CompleteMigrationResponse -> TestTree
responseCompleteMigration =
  res
    "CompleteMigrationResponse"
    "fixture/CompleteMigrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CompleteMigration)

responseDescribeCacheClusters :: DescribeCacheClustersResponse -> TestTree
responseDescribeCacheClusters =
  res
    "DescribeCacheClustersResponse"
    "fixture/DescribeCacheClustersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCacheClusters)

responsePurchaseReservedCacheNodesOffering :: PurchaseReservedCacheNodesOfferingResponse -> TestTree
responsePurchaseReservedCacheNodesOffering =
  res
    "PurchaseReservedCacheNodesOfferingResponse"
    "fixture/PurchaseReservedCacheNodesOfferingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PurchaseReservedCacheNodesOffering)

responseRemoveTagsFromResource :: TagListMessage -> TestTree
responseRemoveTagsFromResource =
  res
    "RemoveTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveTagsFromResource)

responseModifyReplicationGroup :: ModifyReplicationGroupResponse -> TestTree
responseModifyReplicationGroup =
  res
    "ModifyReplicationGroupResponse"
    "fixture/ModifyReplicationGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyReplicationGroup)

responseDescribeCacheParameters :: DescribeCacheParametersResponse -> TestTree
responseDescribeCacheParameters =
  res
    "DescribeCacheParametersResponse"
    "fixture/DescribeCacheParametersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCacheParameters)

responseDescribeGlobalReplicationGroups :: DescribeGlobalReplicationGroupsResponse -> TestTree
responseDescribeGlobalReplicationGroups =
  res
    "DescribeGlobalReplicationGroupsResponse"
    "fixture/DescribeGlobalReplicationGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeGlobalReplicationGroups)

responseDescribeCacheSubnetGroups :: DescribeCacheSubnetGroupsResponse -> TestTree
responseDescribeCacheSubnetGroups =
  res
    "DescribeCacheSubnetGroupsResponse"
    "fixture/DescribeCacheSubnetGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCacheSubnetGroups)

responseDescribeUpdateActions :: DescribeUpdateActionsResponse -> TestTree
responseDescribeUpdateActions =
  res
    "DescribeUpdateActionsResponse"
    "fixture/DescribeUpdateActionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeUpdateActions)

responseRebalanceSlotsInGlobalReplicationGroup :: RebalanceSlotsInGlobalReplicationGroupResponse -> TestTree
responseRebalanceSlotsInGlobalReplicationGroup =
  res
    "RebalanceSlotsInGlobalReplicationGroupResponse"
    "fixture/RebalanceSlotsInGlobalReplicationGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RebalanceSlotsInGlobalReplicationGroup)

responseCreateCacheSecurityGroup :: CreateCacheSecurityGroupResponse -> TestTree
responseCreateCacheSecurityGroup =
  res
    "CreateCacheSecurityGroupResponse"
    "fixture/CreateCacheSecurityGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCacheSecurityGroup)

responseDecreaseReplicaCount :: DecreaseReplicaCountResponse -> TestTree
responseDecreaseReplicaCount =
  res
    "DecreaseReplicaCountResponse"
    "fixture/DecreaseReplicaCountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DecreaseReplicaCount)

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

responseCopySnapshot :: CopySnapshotResponse -> TestTree
responseCopySnapshot =
  res
    "CopySnapshotResponse"
    "fixture/CopySnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CopySnapshot)

responseFailoverGlobalReplicationGroup :: FailoverGlobalReplicationGroupResponse -> TestTree
responseFailoverGlobalReplicationGroup =
  res
    "FailoverGlobalReplicationGroupResponse"
    "fixture/FailoverGlobalReplicationGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy FailoverGlobalReplicationGroup)

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

responseDescribeCacheParameterGroups :: DescribeCacheParameterGroupsResponse -> TestTree
responseDescribeCacheParameterGroups =
  res
    "DescribeCacheParameterGroupsResponse"
    "fixture/DescribeCacheParameterGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCacheParameterGroups)

responseResetCacheParameterGroup :: CacheParameterGroupNameMessage -> TestTree
responseResetCacheParameterGroup =
  res
    "ResetCacheParameterGroupResponse"
    "fixture/ResetCacheParameterGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResetCacheParameterGroup)

responseListAllowedNodeTypeModifications :: ListAllowedNodeTypeModificationsResponse -> TestTree
responseListAllowedNodeTypeModifications =
  res
    "ListAllowedNodeTypeModificationsResponse"
    "fixture/ListAllowedNodeTypeModificationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAllowedNodeTypeModifications)

responseIncreaseReplicaCount :: IncreaseReplicaCountResponse -> TestTree
responseIncreaseReplicaCount =
  res
    "IncreaseReplicaCountResponse"
    "fixture/IncreaseReplicaCountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy IncreaseReplicaCount)

responseModifyReplicationGroupShardConfiguration :: ModifyReplicationGroupShardConfigurationResponse -> TestTree
responseModifyReplicationGroupShardConfiguration =
  res
    "ModifyReplicationGroupShardConfigurationResponse"
    "fixture/ModifyReplicationGroupShardConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyReplicationGroupShardConfiguration)

responseBatchApplyUpdateAction :: UpdateActionResultsMessage -> TestTree
responseBatchApplyUpdateAction =
  res
    "BatchApplyUpdateActionResponse"
    "fixture/BatchApplyUpdateActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchApplyUpdateAction)

responseDeleteUserGroup :: UserGroup -> TestTree
responseDeleteUserGroup =
  res
    "DeleteUserGroupResponse"
    "fixture/DeleteUserGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUserGroup)

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

responseDescribeReplicationGroups :: DescribeReplicationGroupsResponse -> TestTree
responseDescribeReplicationGroups =
  res
    "DescribeReplicationGroupsResponse"
    "fixture/DescribeReplicationGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReplicationGroups)

responseCreateUser :: User -> TestTree
responseCreateUser =
  res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUser)

responseDeleteSnapshot :: DeleteSnapshotResponse -> TestTree
responseDeleteSnapshot =
  res
    "DeleteSnapshotResponse"
    "fixture/DeleteSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSnapshot)

responseDescribeReservedCacheNodesOfferings :: DescribeReservedCacheNodesOfferingsResponse -> TestTree
responseDescribeReservedCacheNodesOfferings =
  res
    "DescribeReservedCacheNodesOfferingsResponse"
    "fixture/DescribeReservedCacheNodesOfferingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReservedCacheNodesOfferings)

responseModifyCacheSubnetGroup :: ModifyCacheSubnetGroupResponse -> TestTree
responseModifyCacheSubnetGroup =
  res
    "ModifyCacheSubnetGroupResponse"
    "fixture/ModifyCacheSubnetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyCacheSubnetGroup)

responseDeleteUser :: User -> TestTree
responseDeleteUser =
  res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUser)

responseCreateSnapshot :: CreateSnapshotResponse -> TestTree
responseCreateSnapshot =
  res
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSnapshot)

responseModifyGlobalReplicationGroup :: ModifyGlobalReplicationGroupResponse -> TestTree
responseModifyGlobalReplicationGroup =
  res
    "ModifyGlobalReplicationGroupResponse"
    "fixture/ModifyGlobalReplicationGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyGlobalReplicationGroup)

responseDescribeUserGroups :: DescribeUserGroupsResponse -> TestTree
responseDescribeUserGroups =
  res
    "DescribeUserGroupsResponse"
    "fixture/DescribeUserGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeUserGroups)

responseDeleteCacheParameterGroup :: DeleteCacheParameterGroupResponse -> TestTree
responseDeleteCacheParameterGroup =
  res
    "DeleteCacheParameterGroupResponse"
    "fixture/DeleteCacheParameterGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCacheParameterGroup)

responseDescribeCacheSecurityGroups :: DescribeCacheSecurityGroupsResponse -> TestTree
responseDescribeCacheSecurityGroups =
  res
    "DescribeCacheSecurityGroupsResponse"
    "fixture/DescribeCacheSecurityGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCacheSecurityGroups)

responseBatchStopUpdateAction :: UpdateActionResultsMessage -> TestTree
responseBatchStopUpdateAction =
  res
    "BatchStopUpdateActionResponse"
    "fixture/BatchStopUpdateActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchStopUpdateAction)

responseModifyCacheCluster :: ModifyCacheClusterResponse -> TestTree
responseModifyCacheCluster =
  res
    "ModifyCacheClusterResponse"
    "fixture/ModifyCacheClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyCacheCluster)

responseDescribeCacheEngineVersions :: DescribeCacheEngineVersionsResponse -> TestTree
responseDescribeCacheEngineVersions =
  res
    "DescribeCacheEngineVersionsResponse"
    "fixture/DescribeCacheEngineVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCacheEngineVersions)

responseModifyUserGroup :: UserGroup -> TestTree
responseModifyUserGroup =
  res
    "ModifyUserGroupResponse"
    "fixture/ModifyUserGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyUserGroup)

responseCreateCacheParameterGroup :: CreateCacheParameterGroupResponse -> TestTree
responseCreateCacheParameterGroup =
  res
    "CreateCacheParameterGroupResponse"
    "fixture/CreateCacheParameterGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCacheParameterGroup)

responseDescribeReservedCacheNodes :: DescribeReservedCacheNodesResponse -> TestTree
responseDescribeReservedCacheNodes =
  res
    "DescribeReservedCacheNodesResponse"
    "fixture/DescribeReservedCacheNodesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReservedCacheNodes)

responseDeleteGlobalReplicationGroup :: DeleteGlobalReplicationGroupResponse -> TestTree
responseDeleteGlobalReplicationGroup =
  res
    "DeleteGlobalReplicationGroupResponse"
    "fixture/DeleteGlobalReplicationGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGlobalReplicationGroup)

responseDecreaseNodeGroupsInGlobalReplicationGroup :: DecreaseNodeGroupsInGlobalReplicationGroupResponse -> TestTree
responseDecreaseNodeGroupsInGlobalReplicationGroup =
  res
    "DecreaseNodeGroupsInGlobalReplicationGroupResponse"
    "fixture/DecreaseNodeGroupsInGlobalReplicationGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DecreaseNodeGroupsInGlobalReplicationGroup)

responseModifyUser :: User -> TestTree
responseModifyUser =
  res
    "ModifyUserResponse"
    "fixture/ModifyUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyUser)

responseDeleteCacheSubnetGroup :: DeleteCacheSubnetGroupResponse -> TestTree
responseDeleteCacheSubnetGroup =
  res
    "DeleteCacheSubnetGroupResponse"
    "fixture/DeleteCacheSubnetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCacheSubnetGroup)
