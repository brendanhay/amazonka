{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ElastiCache
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
--         [ requestStartMigration $
--             mkStartMigration
--
--         , requestDeleteCacheSecurityGroup $
--             mkDeleteCacheSecurityGroup
--
--         , requestCreateReplicationGroup $
--             mkCreateReplicationGroup
--
--         , requestDeleteCacheCluster $
--             mkDeleteCacheCluster
--
--         , requestIncreaseNodeGroupsInGlobalReplicationGroup $
--             mkIncreaseNodeGroupsInGlobalReplicationGroup
--
--         , requestDescribeUsers $
--             mkDescribeUsers
--
--         , requestRebootCacheCluster $
--             mkRebootCacheCluster
--
--         , requestCreateUserGroup $
--             mkCreateUserGroup
--
--         , requestRevokeCacheSecurityGroupIngress $
--             mkRevokeCacheSecurityGroupIngress
--
--         , requestCreateCacheCluster $
--             mkCreateCacheCluster
--
--         , requestDescribeEvents $
--             mkDescribeEvents
--
--         , requestDescribeEngineDefaultParameters $
--             mkDescribeEngineDefaultParameters
--
--         , requestDisassociateGlobalReplicationGroup $
--             mkDisassociateGlobalReplicationGroup
--
--         , requestModifyCacheParameterGroup $
--             mkModifyCacheParameterGroup
--
--         , requestTestFailover $
--             mkTestFailover
--
--         , requestDeleteReplicationGroup $
--             mkDeleteReplicationGroup
--
--         , requestListTagsForResource $
--             mkListTagsForResource
--
--         , requestCompleteMigration $
--             mkCompleteMigration
--
--         , requestDescribeCacheClusters $
--             mkDescribeCacheClusters
--
--         , requestPurchaseReservedCacheNodesOffering $
--             mkPurchaseReservedCacheNodesOffering
--
--         , requestRemoveTagsFromResource $
--             mkRemoveTagsFromResource
--
--         , requestModifyReplicationGroup $
--             mkModifyReplicationGroup
--
--         , requestDescribeCacheParameters $
--             mkDescribeCacheParameters
--
--         , requestDescribeGlobalReplicationGroups $
--             mkDescribeGlobalReplicationGroups
--
--         , requestDescribeCacheSubnetGroups $
--             mkDescribeCacheSubnetGroups
--
--         , requestDescribeUpdateActions $
--             mkDescribeUpdateActions
--
--         , requestRebalanceSlotsInGlobalReplicationGroup $
--             mkRebalanceSlotsInGlobalReplicationGroup
--
--         , requestCreateCacheSecurityGroup $
--             mkCreateCacheSecurityGroup
--
--         , requestDecreaseReplicaCount $
--             mkDecreaseReplicaCount
--
--         , requestAddTagsToResource $
--             mkAddTagsToResource
--
--         , requestAuthorizeCacheSecurityGroupIngress $
--             mkAuthorizeCacheSecurityGroupIngress
--
--         , requestCopySnapshot $
--             mkCopySnapshot
--
--         , requestFailoverGlobalReplicationGroup $
--             mkFailoverGlobalReplicationGroup
--
--         , requestCreateCacheSubnetGroup $
--             mkCreateCacheSubnetGroup
--
--         , requestCreateGlobalReplicationGroup $
--             mkCreateGlobalReplicationGroup
--
--         , requestDescribeCacheParameterGroups $
--             mkDescribeCacheParameterGroups
--
--         , requestResetCacheParameterGroup $
--             mkResetCacheParameterGroup
--
--         , requestListAllowedNodeTypeModifications $
--             mkListAllowedNodeTypeModifications
--
--         , requestIncreaseReplicaCount $
--             mkIncreaseReplicaCount
--
--         , requestModifyReplicationGroupShardConfiguration $
--             mkModifyReplicationGroupShardConfiguration
--
--         , requestBatchApplyUpdateAction $
--             mkBatchApplyUpdateAction
--
--         , requestDeleteUserGroup $
--             mkDeleteUserGroup
--
--         , requestDescribeServiceUpdates $
--             mkDescribeServiceUpdates
--
--         , requestDescribeSnapshots $
--             mkDescribeSnapshots
--
--         , requestDescribeReplicationGroups $
--             mkDescribeReplicationGroups
--
--         , requestCreateUser $
--             mkCreateUser
--
--         , requestDeleteSnapshot $
--             mkDeleteSnapshot
--
--         , requestDescribeReservedCacheNodesOfferings $
--             mkDescribeReservedCacheNodesOfferings
--
--         , requestModifyCacheSubnetGroup $
--             mkModifyCacheSubnetGroup
--
--         , requestDeleteUser $
--             mkDeleteUser
--
--         , requestCreateSnapshot $
--             mkCreateSnapshot
--
--         , requestModifyGlobalReplicationGroup $
--             mkModifyGlobalReplicationGroup
--
--         , requestDescribeUserGroups $
--             mkDescribeUserGroups
--
--         , requestDeleteCacheParameterGroup $
--             mkDeleteCacheParameterGroup
--
--         , requestDescribeCacheSecurityGroups $
--             mkDescribeCacheSecurityGroups
--
--         , requestBatchStopUpdateAction $
--             mkBatchStopUpdateAction
--
--         , requestModifyCacheCluster $
--             mkModifyCacheCluster
--
--         , requestDescribeCacheEngineVersions $
--             mkDescribeCacheEngineVersions
--
--         , requestModifyUserGroup $
--             mkModifyUserGroup
--
--         , requestCreateCacheParameterGroup $
--             mkCreateCacheParameterGroup
--
--         , requestDescribeReservedCacheNodes $
--             mkDescribeReservedCacheNodes
--
--         , requestDeleteGlobalReplicationGroup $
--             mkDeleteGlobalReplicationGroup
--
--         , requestDecreaseNodeGroupsInGlobalReplicationGroup $
--             mkDecreaseNodeGroupsInGlobalReplicationGroup
--
--         , requestModifyUser $
--             mkModifyUser
--
--         , requestDeleteCacheSubnetGroup $
--             mkDeleteCacheSubnetGroup
--
--           ]

--     , testGroup "response"
--         [ responseStartMigration $
--             mkStartMigrationResponse
--
--         , responseDeleteCacheSecurityGroup $
--             mkDeleteCacheSecurityGroupResponse
--
--         , responseCreateReplicationGroup $
--             mkCreateReplicationGroupResponse
--
--         , responseDeleteCacheCluster $
--             mkDeleteCacheClusterResponse
--
--         , responseIncreaseNodeGroupsInGlobalReplicationGroup $
--             mkIncreaseNodeGroupsInGlobalReplicationGroupResponse
--
--         , responseDescribeUsers $
--             mkDescribeUsersResponse
--
--         , responseRebootCacheCluster $
--             mkRebootCacheClusterResponse
--
--         , responseCreateUserGroup $
--             mkUserGroup
--
--         , responseRevokeCacheSecurityGroupIngress $
--             mkRevokeCacheSecurityGroupIngressResponse
--
--         , responseCreateCacheCluster $
--             mkCreateCacheClusterResponse
--
--         , responseDescribeEvents $
--             mkDescribeEventsResponse
--
--         , responseDescribeEngineDefaultParameters $
--             mkDescribeEngineDefaultParametersResponse
--
--         , responseDisassociateGlobalReplicationGroup $
--             mkDisassociateGlobalReplicationGroupResponse
--
--         , responseModifyCacheParameterGroup $
--             mkCacheParameterGroupNameMessage
--
--         , responseTestFailover $
--             mkTestFailoverResponse
--
--         , responseDeleteReplicationGroup $
--             mkDeleteReplicationGroupResponse
--
--         , responseListTagsForResource $
--             mkTagListMessage
--
--         , responseCompleteMigration $
--             mkCompleteMigrationResponse
--
--         , responseDescribeCacheClusters $
--             mkDescribeCacheClustersResponse
--
--         , responsePurchaseReservedCacheNodesOffering $
--             mkPurchaseReservedCacheNodesOfferingResponse
--
--         , responseRemoveTagsFromResource $
--             mkTagListMessage
--
--         , responseModifyReplicationGroup $
--             mkModifyReplicationGroupResponse
--
--         , responseDescribeCacheParameters $
--             mkDescribeCacheParametersResponse
--
--         , responseDescribeGlobalReplicationGroups $
--             mkDescribeGlobalReplicationGroupsResponse
--
--         , responseDescribeCacheSubnetGroups $
--             mkDescribeCacheSubnetGroupsResponse
--
--         , responseDescribeUpdateActions $
--             mkDescribeUpdateActionsResponse
--
--         , responseRebalanceSlotsInGlobalReplicationGroup $
--             mkRebalanceSlotsInGlobalReplicationGroupResponse
--
--         , responseCreateCacheSecurityGroup $
--             mkCreateCacheSecurityGroupResponse
--
--         , responseDecreaseReplicaCount $
--             mkDecreaseReplicaCountResponse
--
--         , responseAddTagsToResource $
--             mkTagListMessage
--
--         , responseAuthorizeCacheSecurityGroupIngress $
--             mkAuthorizeCacheSecurityGroupIngressResponse
--
--         , responseCopySnapshot $
--             mkCopySnapshotResponse
--
--         , responseFailoverGlobalReplicationGroup $
--             mkFailoverGlobalReplicationGroupResponse
--
--         , responseCreateCacheSubnetGroup $
--             mkCreateCacheSubnetGroupResponse
--
--         , responseCreateGlobalReplicationGroup $
--             mkCreateGlobalReplicationGroupResponse
--
--         , responseDescribeCacheParameterGroups $
--             mkDescribeCacheParameterGroupsResponse
--
--         , responseResetCacheParameterGroup $
--             mkCacheParameterGroupNameMessage
--
--         , responseListAllowedNodeTypeModifications $
--             mkListAllowedNodeTypeModificationsResponse
--
--         , responseIncreaseReplicaCount $
--             mkIncreaseReplicaCountResponse
--
--         , responseModifyReplicationGroupShardConfiguration $
--             mkModifyReplicationGroupShardConfigurationResponse
--
--         , responseBatchApplyUpdateAction $
--             mkUpdateActionResultsMessage
--
--         , responseDeleteUserGroup $
--             mkUserGroup
--
--         , responseDescribeServiceUpdates $
--             mkDescribeServiceUpdatesResponse
--
--         , responseDescribeSnapshots $
--             mkDescribeSnapshotsResponse
--
--         , responseDescribeReplicationGroups $
--             mkDescribeReplicationGroupsResponse
--
--         , responseCreateUser $
--             mkUser
--
--         , responseDeleteSnapshot $
--             mkDeleteSnapshotResponse
--
--         , responseDescribeReservedCacheNodesOfferings $
--             mkDescribeReservedCacheNodesOfferingsResponse
--
--         , responseModifyCacheSubnetGroup $
--             mkModifyCacheSubnetGroupResponse
--
--         , responseDeleteUser $
--             mkUser
--
--         , responseCreateSnapshot $
--             mkCreateSnapshotResponse
--
--         , responseModifyGlobalReplicationGroup $
--             mkModifyGlobalReplicationGroupResponse
--
--         , responseDescribeUserGroups $
--             mkDescribeUserGroupsResponse
--
--         , responseDeleteCacheParameterGroup $
--             mkDeleteCacheParameterGroupResponse
--
--         , responseDescribeCacheSecurityGroups $
--             mkDescribeCacheSecurityGroupsResponse
--
--         , responseBatchStopUpdateAction $
--             mkUpdateActionResultsMessage
--
--         , responseModifyCacheCluster $
--             mkModifyCacheClusterResponse
--
--         , responseDescribeCacheEngineVersions $
--             mkDescribeCacheEngineVersionsResponse
--
--         , responseModifyUserGroup $
--             mkUserGroup
--
--         , responseCreateCacheParameterGroup $
--             mkCreateCacheParameterGroupResponse
--
--         , responseDescribeReservedCacheNodes $
--             mkDescribeReservedCacheNodesResponse
--
--         , responseDeleteGlobalReplicationGroup $
--             mkDeleteGlobalReplicationGroupResponse
--
--         , responseDecreaseNodeGroupsInGlobalReplicationGroup $
--             mkDecreaseNodeGroupsInGlobalReplicationGroupResponse
--
--         , responseModifyUser $
--             mkUser
--
--         , responseDeleteCacheSubnetGroup $
--             mkDeleteCacheSubnetGroupResponse
--
--           ]
--     ]

-- Requests

requestStartMigration :: StartMigration -> TestTree
requestStartMigration = req
    "StartMigration"
    "fixture/StartMigration.yaml"

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

requestIncreaseNodeGroupsInGlobalReplicationGroup :: IncreaseNodeGroupsInGlobalReplicationGroup -> TestTree
requestIncreaseNodeGroupsInGlobalReplicationGroup = req
    "IncreaseNodeGroupsInGlobalReplicationGroup"
    "fixture/IncreaseNodeGroupsInGlobalReplicationGroup.yaml"

requestDescribeUsers :: DescribeUsers -> TestTree
requestDescribeUsers = req
    "DescribeUsers"
    "fixture/DescribeUsers.yaml"

requestRebootCacheCluster :: RebootCacheCluster -> TestTree
requestRebootCacheCluster = req
    "RebootCacheCluster"
    "fixture/RebootCacheCluster.yaml"

requestCreateUserGroup :: CreateUserGroup -> TestTree
requestCreateUserGroup = req
    "CreateUserGroup"
    "fixture/CreateUserGroup.yaml"

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

requestDisassociateGlobalReplicationGroup :: DisassociateGlobalReplicationGroup -> TestTree
requestDisassociateGlobalReplicationGroup = req
    "DisassociateGlobalReplicationGroup"
    "fixture/DisassociateGlobalReplicationGroup.yaml"

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

requestCompleteMigration :: CompleteMigration -> TestTree
requestCompleteMigration = req
    "CompleteMigration"
    "fixture/CompleteMigration.yaml"

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

requestDescribeGlobalReplicationGroups :: DescribeGlobalReplicationGroups -> TestTree
requestDescribeGlobalReplicationGroups = req
    "DescribeGlobalReplicationGroups"
    "fixture/DescribeGlobalReplicationGroups.yaml"

requestDescribeCacheSubnetGroups :: DescribeCacheSubnetGroups -> TestTree
requestDescribeCacheSubnetGroups = req
    "DescribeCacheSubnetGroups"
    "fixture/DescribeCacheSubnetGroups.yaml"

requestDescribeUpdateActions :: DescribeUpdateActions -> TestTree
requestDescribeUpdateActions = req
    "DescribeUpdateActions"
    "fixture/DescribeUpdateActions.yaml"

requestRebalanceSlotsInGlobalReplicationGroup :: RebalanceSlotsInGlobalReplicationGroup -> TestTree
requestRebalanceSlotsInGlobalReplicationGroup = req
    "RebalanceSlotsInGlobalReplicationGroup"
    "fixture/RebalanceSlotsInGlobalReplicationGroup.yaml"

requestCreateCacheSecurityGroup :: CreateCacheSecurityGroup -> TestTree
requestCreateCacheSecurityGroup = req
    "CreateCacheSecurityGroup"
    "fixture/CreateCacheSecurityGroup.yaml"

requestDecreaseReplicaCount :: DecreaseReplicaCount -> TestTree
requestDecreaseReplicaCount = req
    "DecreaseReplicaCount"
    "fixture/DecreaseReplicaCount.yaml"

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

requestFailoverGlobalReplicationGroup :: FailoverGlobalReplicationGroup -> TestTree
requestFailoverGlobalReplicationGroup = req
    "FailoverGlobalReplicationGroup"
    "fixture/FailoverGlobalReplicationGroup.yaml"

requestCreateCacheSubnetGroup :: CreateCacheSubnetGroup -> TestTree
requestCreateCacheSubnetGroup = req
    "CreateCacheSubnetGroup"
    "fixture/CreateCacheSubnetGroup.yaml"

requestCreateGlobalReplicationGroup :: CreateGlobalReplicationGroup -> TestTree
requestCreateGlobalReplicationGroup = req
    "CreateGlobalReplicationGroup"
    "fixture/CreateGlobalReplicationGroup.yaml"

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

requestIncreaseReplicaCount :: IncreaseReplicaCount -> TestTree
requestIncreaseReplicaCount = req
    "IncreaseReplicaCount"
    "fixture/IncreaseReplicaCount.yaml"

requestModifyReplicationGroupShardConfiguration :: ModifyReplicationGroupShardConfiguration -> TestTree
requestModifyReplicationGroupShardConfiguration = req
    "ModifyReplicationGroupShardConfiguration"
    "fixture/ModifyReplicationGroupShardConfiguration.yaml"

requestBatchApplyUpdateAction :: BatchApplyUpdateAction -> TestTree
requestBatchApplyUpdateAction = req
    "BatchApplyUpdateAction"
    "fixture/BatchApplyUpdateAction.yaml"

requestDeleteUserGroup :: DeleteUserGroup -> TestTree
requestDeleteUserGroup = req
    "DeleteUserGroup"
    "fixture/DeleteUserGroup.yaml"

requestDescribeServiceUpdates :: DescribeServiceUpdates -> TestTree
requestDescribeServiceUpdates = req
    "DescribeServiceUpdates"
    "fixture/DescribeServiceUpdates.yaml"

requestDescribeSnapshots :: DescribeSnapshots -> TestTree
requestDescribeSnapshots = req
    "DescribeSnapshots"
    "fixture/DescribeSnapshots.yaml"

requestDescribeReplicationGroups :: DescribeReplicationGroups -> TestTree
requestDescribeReplicationGroups = req
    "DescribeReplicationGroups"
    "fixture/DescribeReplicationGroups.yaml"

requestCreateUser :: CreateUser -> TestTree
requestCreateUser = req
    "CreateUser"
    "fixture/CreateUser.yaml"

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

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser = req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

requestCreateSnapshot :: CreateSnapshot -> TestTree
requestCreateSnapshot = req
    "CreateSnapshot"
    "fixture/CreateSnapshot.yaml"

requestModifyGlobalReplicationGroup :: ModifyGlobalReplicationGroup -> TestTree
requestModifyGlobalReplicationGroup = req
    "ModifyGlobalReplicationGroup"
    "fixture/ModifyGlobalReplicationGroup.yaml"

requestDescribeUserGroups :: DescribeUserGroups -> TestTree
requestDescribeUserGroups = req
    "DescribeUserGroups"
    "fixture/DescribeUserGroups.yaml"

requestDeleteCacheParameterGroup :: DeleteCacheParameterGroup -> TestTree
requestDeleteCacheParameterGroup = req
    "DeleteCacheParameterGroup"
    "fixture/DeleteCacheParameterGroup.yaml"

requestDescribeCacheSecurityGroups :: DescribeCacheSecurityGroups -> TestTree
requestDescribeCacheSecurityGroups = req
    "DescribeCacheSecurityGroups"
    "fixture/DescribeCacheSecurityGroups.yaml"

requestBatchStopUpdateAction :: BatchStopUpdateAction -> TestTree
requestBatchStopUpdateAction = req
    "BatchStopUpdateAction"
    "fixture/BatchStopUpdateAction.yaml"

requestModifyCacheCluster :: ModifyCacheCluster -> TestTree
requestModifyCacheCluster = req
    "ModifyCacheCluster"
    "fixture/ModifyCacheCluster.yaml"

requestDescribeCacheEngineVersions :: DescribeCacheEngineVersions -> TestTree
requestDescribeCacheEngineVersions = req
    "DescribeCacheEngineVersions"
    "fixture/DescribeCacheEngineVersions.yaml"

requestModifyUserGroup :: ModifyUserGroup -> TestTree
requestModifyUserGroup = req
    "ModifyUserGroup"
    "fixture/ModifyUserGroup.yaml"

requestCreateCacheParameterGroup :: CreateCacheParameterGroup -> TestTree
requestCreateCacheParameterGroup = req
    "CreateCacheParameterGroup"
    "fixture/CreateCacheParameterGroup.yaml"

requestDescribeReservedCacheNodes :: DescribeReservedCacheNodes -> TestTree
requestDescribeReservedCacheNodes = req
    "DescribeReservedCacheNodes"
    "fixture/DescribeReservedCacheNodes.yaml"

requestDeleteGlobalReplicationGroup :: DeleteGlobalReplicationGroup -> TestTree
requestDeleteGlobalReplicationGroup = req
    "DeleteGlobalReplicationGroup"
    "fixture/DeleteGlobalReplicationGroup.yaml"

requestDecreaseNodeGroupsInGlobalReplicationGroup :: DecreaseNodeGroupsInGlobalReplicationGroup -> TestTree
requestDecreaseNodeGroupsInGlobalReplicationGroup = req
    "DecreaseNodeGroupsInGlobalReplicationGroup"
    "fixture/DecreaseNodeGroupsInGlobalReplicationGroup.yaml"

requestModifyUser :: ModifyUser -> TestTree
requestModifyUser = req
    "ModifyUser"
    "fixture/ModifyUser.yaml"

requestDeleteCacheSubnetGroup :: DeleteCacheSubnetGroup -> TestTree
requestDeleteCacheSubnetGroup = req
    "DeleteCacheSubnetGroup"
    "fixture/DeleteCacheSubnetGroup.yaml"

-- Responses

responseStartMigration :: StartMigrationResponse -> TestTree
responseStartMigration = res
    "StartMigrationResponse"
    "fixture/StartMigrationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartMigration)

responseDeleteCacheSecurityGroup :: DeleteCacheSecurityGroupResponse -> TestTree
responseDeleteCacheSecurityGroup = res
    "DeleteCacheSecurityGroupResponse"
    "fixture/DeleteCacheSecurityGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteCacheSecurityGroup)

responseCreateReplicationGroup :: CreateReplicationGroupResponse -> TestTree
responseCreateReplicationGroup = res
    "CreateReplicationGroupResponse"
    "fixture/CreateReplicationGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateReplicationGroup)

responseDeleteCacheCluster :: DeleteCacheClusterResponse -> TestTree
responseDeleteCacheCluster = res
    "DeleteCacheClusterResponse"
    "fixture/DeleteCacheClusterResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteCacheCluster)

responseIncreaseNodeGroupsInGlobalReplicationGroup :: IncreaseNodeGroupsInGlobalReplicationGroupResponse -> TestTree
responseIncreaseNodeGroupsInGlobalReplicationGroup = res
    "IncreaseNodeGroupsInGlobalReplicationGroupResponse"
    "fixture/IncreaseNodeGroupsInGlobalReplicationGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy IncreaseNodeGroupsInGlobalReplicationGroup)

responseDescribeUsers :: DescribeUsersResponse -> TestTree
responseDescribeUsers = res
    "DescribeUsersResponse"
    "fixture/DescribeUsersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeUsers)

responseRebootCacheCluster :: RebootCacheClusterResponse -> TestTree
responseRebootCacheCluster = res
    "RebootCacheClusterResponse"
    "fixture/RebootCacheClusterResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RebootCacheCluster)

responseCreateUserGroup :: UserGroup -> TestTree
responseCreateUserGroup = res
    "CreateUserGroupResponse"
    "fixture/CreateUserGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateUserGroup)

responseRevokeCacheSecurityGroupIngress :: RevokeCacheSecurityGroupIngressResponse -> TestTree
responseRevokeCacheSecurityGroupIngress = res
    "RevokeCacheSecurityGroupIngressResponse"
    "fixture/RevokeCacheSecurityGroupIngressResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RevokeCacheSecurityGroupIngress)

responseCreateCacheCluster :: CreateCacheClusterResponse -> TestTree
responseCreateCacheCluster = res
    "CreateCacheClusterResponse"
    "fixture/CreateCacheClusterResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateCacheCluster)

responseDescribeEvents :: DescribeEventsResponse -> TestTree
responseDescribeEvents = res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeEvents)

responseDescribeEngineDefaultParameters :: DescribeEngineDefaultParametersResponse -> TestTree
responseDescribeEngineDefaultParameters = res
    "DescribeEngineDefaultParametersResponse"
    "fixture/DescribeEngineDefaultParametersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeEngineDefaultParameters)

responseDisassociateGlobalReplicationGroup :: DisassociateGlobalReplicationGroupResponse -> TestTree
responseDisassociateGlobalReplicationGroup = res
    "DisassociateGlobalReplicationGroupResponse"
    "fixture/DisassociateGlobalReplicationGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisassociateGlobalReplicationGroup)

responseModifyCacheParameterGroup :: CacheParameterGroupNameMessage -> TestTree
responseModifyCacheParameterGroup = res
    "ModifyCacheParameterGroupResponse"
    "fixture/ModifyCacheParameterGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyCacheParameterGroup)

responseTestFailover :: TestFailoverResponse -> TestTree
responseTestFailover = res
    "TestFailoverResponse"
    "fixture/TestFailoverResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TestFailover)

responseDeleteReplicationGroup :: DeleteReplicationGroupResponse -> TestTree
responseDeleteReplicationGroup = res
    "DeleteReplicationGroupResponse"
    "fixture/DeleteReplicationGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteReplicationGroup)

responseListTagsForResource :: TagListMessage -> TestTree
responseListTagsForResource = res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTagsForResource)

responseCompleteMigration :: CompleteMigrationResponse -> TestTree
responseCompleteMigration = res
    "CompleteMigrationResponse"
    "fixture/CompleteMigrationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CompleteMigration)

responseDescribeCacheClusters :: DescribeCacheClustersResponse -> TestTree
responseDescribeCacheClusters = res
    "DescribeCacheClustersResponse"
    "fixture/DescribeCacheClustersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeCacheClusters)

responsePurchaseReservedCacheNodesOffering :: PurchaseReservedCacheNodesOfferingResponse -> TestTree
responsePurchaseReservedCacheNodesOffering = res
    "PurchaseReservedCacheNodesOfferingResponse"
    "fixture/PurchaseReservedCacheNodesOfferingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PurchaseReservedCacheNodesOffering)

responseRemoveTagsFromResource :: TagListMessage -> TestTree
responseRemoveTagsFromResource = res
    "RemoveTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RemoveTagsFromResource)

responseModifyReplicationGroup :: ModifyReplicationGroupResponse -> TestTree
responseModifyReplicationGroup = res
    "ModifyReplicationGroupResponse"
    "fixture/ModifyReplicationGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyReplicationGroup)

responseDescribeCacheParameters :: DescribeCacheParametersResponse -> TestTree
responseDescribeCacheParameters = res
    "DescribeCacheParametersResponse"
    "fixture/DescribeCacheParametersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeCacheParameters)

responseDescribeGlobalReplicationGroups :: DescribeGlobalReplicationGroupsResponse -> TestTree
responseDescribeGlobalReplicationGroups = res
    "DescribeGlobalReplicationGroupsResponse"
    "fixture/DescribeGlobalReplicationGroupsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeGlobalReplicationGroups)

responseDescribeCacheSubnetGroups :: DescribeCacheSubnetGroupsResponse -> TestTree
responseDescribeCacheSubnetGroups = res
    "DescribeCacheSubnetGroupsResponse"
    "fixture/DescribeCacheSubnetGroupsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeCacheSubnetGroups)

responseDescribeUpdateActions :: DescribeUpdateActionsResponse -> TestTree
responseDescribeUpdateActions = res
    "DescribeUpdateActionsResponse"
    "fixture/DescribeUpdateActionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeUpdateActions)

responseRebalanceSlotsInGlobalReplicationGroup :: RebalanceSlotsInGlobalReplicationGroupResponse -> TestTree
responseRebalanceSlotsInGlobalReplicationGroup = res
    "RebalanceSlotsInGlobalReplicationGroupResponse"
    "fixture/RebalanceSlotsInGlobalReplicationGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RebalanceSlotsInGlobalReplicationGroup)

responseCreateCacheSecurityGroup :: CreateCacheSecurityGroupResponse -> TestTree
responseCreateCacheSecurityGroup = res
    "CreateCacheSecurityGroupResponse"
    "fixture/CreateCacheSecurityGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateCacheSecurityGroup)

responseDecreaseReplicaCount :: DecreaseReplicaCountResponse -> TestTree
responseDecreaseReplicaCount = res
    "DecreaseReplicaCountResponse"
    "fixture/DecreaseReplicaCountResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DecreaseReplicaCount)

responseAddTagsToResource :: TagListMessage -> TestTree
responseAddTagsToResource = res
    "AddTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AddTagsToResource)

responseAuthorizeCacheSecurityGroupIngress :: AuthorizeCacheSecurityGroupIngressResponse -> TestTree
responseAuthorizeCacheSecurityGroupIngress = res
    "AuthorizeCacheSecurityGroupIngressResponse"
    "fixture/AuthorizeCacheSecurityGroupIngressResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AuthorizeCacheSecurityGroupIngress)

responseCopySnapshot :: CopySnapshotResponse -> TestTree
responseCopySnapshot = res
    "CopySnapshotResponse"
    "fixture/CopySnapshotResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CopySnapshot)

responseFailoverGlobalReplicationGroup :: FailoverGlobalReplicationGroupResponse -> TestTree
responseFailoverGlobalReplicationGroup = res
    "FailoverGlobalReplicationGroupResponse"
    "fixture/FailoverGlobalReplicationGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy FailoverGlobalReplicationGroup)

responseCreateCacheSubnetGroup :: CreateCacheSubnetGroupResponse -> TestTree
responseCreateCacheSubnetGroup = res
    "CreateCacheSubnetGroupResponse"
    "fixture/CreateCacheSubnetGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateCacheSubnetGroup)

responseCreateGlobalReplicationGroup :: CreateGlobalReplicationGroupResponse -> TestTree
responseCreateGlobalReplicationGroup = res
    "CreateGlobalReplicationGroupResponse"
    "fixture/CreateGlobalReplicationGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateGlobalReplicationGroup)

responseDescribeCacheParameterGroups :: DescribeCacheParameterGroupsResponse -> TestTree
responseDescribeCacheParameterGroups = res
    "DescribeCacheParameterGroupsResponse"
    "fixture/DescribeCacheParameterGroupsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeCacheParameterGroups)

responseResetCacheParameterGroup :: CacheParameterGroupNameMessage -> TestTree
responseResetCacheParameterGroup = res
    "ResetCacheParameterGroupResponse"
    "fixture/ResetCacheParameterGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ResetCacheParameterGroup)

responseListAllowedNodeTypeModifications :: ListAllowedNodeTypeModificationsResponse -> TestTree
responseListAllowedNodeTypeModifications = res
    "ListAllowedNodeTypeModificationsResponse"
    "fixture/ListAllowedNodeTypeModificationsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListAllowedNodeTypeModifications)

responseIncreaseReplicaCount :: IncreaseReplicaCountResponse -> TestTree
responseIncreaseReplicaCount = res
    "IncreaseReplicaCountResponse"
    "fixture/IncreaseReplicaCountResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy IncreaseReplicaCount)

responseModifyReplicationGroupShardConfiguration :: ModifyReplicationGroupShardConfigurationResponse -> TestTree
responseModifyReplicationGroupShardConfiguration = res
    "ModifyReplicationGroupShardConfigurationResponse"
    "fixture/ModifyReplicationGroupShardConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyReplicationGroupShardConfiguration)

responseBatchApplyUpdateAction :: UpdateActionResultsMessage -> TestTree
responseBatchApplyUpdateAction = res
    "BatchApplyUpdateActionResponse"
    "fixture/BatchApplyUpdateActionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy BatchApplyUpdateAction)

responseDeleteUserGroup :: UserGroup -> TestTree
responseDeleteUserGroup = res
    "DeleteUserGroupResponse"
    "fixture/DeleteUserGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteUserGroup)

responseDescribeServiceUpdates :: DescribeServiceUpdatesResponse -> TestTree
responseDescribeServiceUpdates = res
    "DescribeServiceUpdatesResponse"
    "fixture/DescribeServiceUpdatesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeServiceUpdates)

responseDescribeSnapshots :: DescribeSnapshotsResponse -> TestTree
responseDescribeSnapshots = res
    "DescribeSnapshotsResponse"
    "fixture/DescribeSnapshotsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeSnapshots)

responseDescribeReplicationGroups :: DescribeReplicationGroupsResponse -> TestTree
responseDescribeReplicationGroups = res
    "DescribeReplicationGroupsResponse"
    "fixture/DescribeReplicationGroupsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeReplicationGroups)

responseCreateUser :: User -> TestTree
responseCreateUser = res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateUser)

responseDeleteSnapshot :: DeleteSnapshotResponse -> TestTree
responseDeleteSnapshot = res
    "DeleteSnapshotResponse"
    "fixture/DeleteSnapshotResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteSnapshot)

responseDescribeReservedCacheNodesOfferings :: DescribeReservedCacheNodesOfferingsResponse -> TestTree
responseDescribeReservedCacheNodesOfferings = res
    "DescribeReservedCacheNodesOfferingsResponse"
    "fixture/DescribeReservedCacheNodesOfferingsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeReservedCacheNodesOfferings)

responseModifyCacheSubnetGroup :: ModifyCacheSubnetGroupResponse -> TestTree
responseModifyCacheSubnetGroup = res
    "ModifyCacheSubnetGroupResponse"
    "fixture/ModifyCacheSubnetGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyCacheSubnetGroup)

responseDeleteUser :: User -> TestTree
responseDeleteUser = res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteUser)

responseCreateSnapshot :: CreateSnapshotResponse -> TestTree
responseCreateSnapshot = res
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateSnapshot)

responseModifyGlobalReplicationGroup :: ModifyGlobalReplicationGroupResponse -> TestTree
responseModifyGlobalReplicationGroup = res
    "ModifyGlobalReplicationGroupResponse"
    "fixture/ModifyGlobalReplicationGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyGlobalReplicationGroup)

responseDescribeUserGroups :: DescribeUserGroupsResponse -> TestTree
responseDescribeUserGroups = res
    "DescribeUserGroupsResponse"
    "fixture/DescribeUserGroupsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeUserGroups)

responseDeleteCacheParameterGroup :: DeleteCacheParameterGroupResponse -> TestTree
responseDeleteCacheParameterGroup = res
    "DeleteCacheParameterGroupResponse"
    "fixture/DeleteCacheParameterGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteCacheParameterGroup)

responseDescribeCacheSecurityGroups :: DescribeCacheSecurityGroupsResponse -> TestTree
responseDescribeCacheSecurityGroups = res
    "DescribeCacheSecurityGroupsResponse"
    "fixture/DescribeCacheSecurityGroupsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeCacheSecurityGroups)

responseBatchStopUpdateAction :: UpdateActionResultsMessage -> TestTree
responseBatchStopUpdateAction = res
    "BatchStopUpdateActionResponse"
    "fixture/BatchStopUpdateActionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy BatchStopUpdateAction)

responseModifyCacheCluster :: ModifyCacheClusterResponse -> TestTree
responseModifyCacheCluster = res
    "ModifyCacheClusterResponse"
    "fixture/ModifyCacheClusterResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyCacheCluster)

responseDescribeCacheEngineVersions :: DescribeCacheEngineVersionsResponse -> TestTree
responseDescribeCacheEngineVersions = res
    "DescribeCacheEngineVersionsResponse"
    "fixture/DescribeCacheEngineVersionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeCacheEngineVersions)

responseModifyUserGroup :: UserGroup -> TestTree
responseModifyUserGroup = res
    "ModifyUserGroupResponse"
    "fixture/ModifyUserGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyUserGroup)

responseCreateCacheParameterGroup :: CreateCacheParameterGroupResponse -> TestTree
responseCreateCacheParameterGroup = res
    "CreateCacheParameterGroupResponse"
    "fixture/CreateCacheParameterGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateCacheParameterGroup)

responseDescribeReservedCacheNodes :: DescribeReservedCacheNodesResponse -> TestTree
responseDescribeReservedCacheNodes = res
    "DescribeReservedCacheNodesResponse"
    "fixture/DescribeReservedCacheNodesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeReservedCacheNodes)

responseDeleteGlobalReplicationGroup :: DeleteGlobalReplicationGroupResponse -> TestTree
responseDeleteGlobalReplicationGroup = res
    "DeleteGlobalReplicationGroupResponse"
    "fixture/DeleteGlobalReplicationGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteGlobalReplicationGroup)

responseDecreaseNodeGroupsInGlobalReplicationGroup :: DecreaseNodeGroupsInGlobalReplicationGroupResponse -> TestTree
responseDecreaseNodeGroupsInGlobalReplicationGroup = res
    "DecreaseNodeGroupsInGlobalReplicationGroupResponse"
    "fixture/DecreaseNodeGroupsInGlobalReplicationGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DecreaseNodeGroupsInGlobalReplicationGroup)

responseModifyUser :: User -> TestTree
responseModifyUser = res
    "ModifyUserResponse"
    "fixture/ModifyUserResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyUser)

responseDeleteCacheSubnetGroup :: DeleteCacheSubnetGroupResponse -> TestTree
responseDeleteCacheSubnetGroup = res
    "DeleteCacheSubnetGroupResponse"
    "fixture/DeleteCacheSubnetGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteCacheSubnetGroup)
