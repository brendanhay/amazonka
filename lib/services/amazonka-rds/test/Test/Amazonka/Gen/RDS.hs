{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.RDS
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.RDS where

import Amazonka.RDS
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.RDS.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAddRoleToDBCluster $
--             newAddRoleToDBCluster
--
--         , requestAddRoleToDBInstance $
--             newAddRoleToDBInstance
--
--         , requestAddSourceIdentifierToSubscription $
--             newAddSourceIdentifierToSubscription
--
--         , requestAddTagsToResource $
--             newAddTagsToResource
--
--         , requestApplyPendingMaintenanceAction $
--             newApplyPendingMaintenanceAction
--
--         , requestAuthorizeDBSecurityGroupIngress $
--             newAuthorizeDBSecurityGroupIngress
--
--         , requestBacktrackDBCluster $
--             newBacktrackDBCluster
--
--         , requestCancelExportTask $
--             newCancelExportTask
--
--         , requestCopyDBClusterParameterGroup $
--             newCopyDBClusterParameterGroup
--
--         , requestCopyDBClusterSnapshot $
--             newCopyDBClusterSnapshot
--
--         , requestCopyDBParameterGroup $
--             newCopyDBParameterGroup
--
--         , requestCopyDBSnapshot $
--             newCopyDBSnapshot
--
--         , requestCopyOptionGroup $
--             newCopyOptionGroup
--
--         , requestCreateBlueGreenDeployment $
--             newCreateBlueGreenDeployment
--
--         , requestCreateCustomDBEngineVersion $
--             newCreateCustomDBEngineVersion
--
--         , requestCreateDBCluster $
--             newCreateDBCluster
--
--         , requestCreateDBClusterEndpoint $
--             newCreateDBClusterEndpoint
--
--         , requestCreateDBClusterParameterGroup $
--             newCreateDBClusterParameterGroup
--
--         , requestCreateDBClusterSnapshot $
--             newCreateDBClusterSnapshot
--
--         , requestCreateDBInstance $
--             newCreateDBInstance
--
--         , requestCreateDBInstanceReadReplica $
--             newCreateDBInstanceReadReplica
--
--         , requestCreateDBParameterGroup $
--             newCreateDBParameterGroup
--
--         , requestCreateDBProxy $
--             newCreateDBProxy
--
--         , requestCreateDBProxyEndpoint $
--             newCreateDBProxyEndpoint
--
--         , requestCreateDBSecurityGroup $
--             newCreateDBSecurityGroup
--
--         , requestCreateDBSnapshot $
--             newCreateDBSnapshot
--
--         , requestCreateDBSubnetGroup $
--             newCreateDBSubnetGroup
--
--         , requestCreateEventSubscription $
--             newCreateEventSubscription
--
--         , requestCreateGlobalCluster $
--             newCreateGlobalCluster
--
--         , requestCreateOptionGroup $
--             newCreateOptionGroup
--
--         , requestDeleteBlueGreenDeployment $
--             newDeleteBlueGreenDeployment
--
--         , requestDeleteCustomDBEngineVersion $
--             newDeleteCustomDBEngineVersion
--
--         , requestDeleteDBCluster $
--             newDeleteDBCluster
--
--         , requestDeleteDBClusterEndpoint $
--             newDeleteDBClusterEndpoint
--
--         , requestDeleteDBClusterParameterGroup $
--             newDeleteDBClusterParameterGroup
--
--         , requestDeleteDBClusterSnapshot $
--             newDeleteDBClusterSnapshot
--
--         , requestDeleteDBInstance $
--             newDeleteDBInstance
--
--         , requestDeleteDBInstanceAutomatedBackup $
--             newDeleteDBInstanceAutomatedBackup
--
--         , requestDeleteDBParameterGroup $
--             newDeleteDBParameterGroup
--
--         , requestDeleteDBProxy $
--             newDeleteDBProxy
--
--         , requestDeleteDBProxyEndpoint $
--             newDeleteDBProxyEndpoint
--
--         , requestDeleteDBSecurityGroup $
--             newDeleteDBSecurityGroup
--
--         , requestDeleteDBSnapshot $
--             newDeleteDBSnapshot
--
--         , requestDeleteDBSubnetGroup $
--             newDeleteDBSubnetGroup
--
--         , requestDeleteEventSubscription $
--             newDeleteEventSubscription
--
--         , requestDeleteGlobalCluster $
--             newDeleteGlobalCluster
--
--         , requestDeleteOptionGroup $
--             newDeleteOptionGroup
--
--         , requestDeregisterDBProxyTargets $
--             newDeregisterDBProxyTargets
--
--         , requestDescribeAccountAttributes $
--             newDescribeAccountAttributes
--
--         , requestDescribeBlueGreenDeployments $
--             newDescribeBlueGreenDeployments
--
--         , requestDescribeCertificates $
--             newDescribeCertificates
--
--         , requestDescribeDBClusterBacktracks $
--             newDescribeDBClusterBacktracks
--
--         , requestDescribeDBClusterEndpoints $
--             newDescribeDBClusterEndpoints
--
--         , requestDescribeDBClusterParameterGroups $
--             newDescribeDBClusterParameterGroups
--
--         , requestDescribeDBClusterParameters $
--             newDescribeDBClusterParameters
--
--         , requestDescribeDBClusterSnapshotAttributes $
--             newDescribeDBClusterSnapshotAttributes
--
--         , requestDescribeDBClusterSnapshots $
--             newDescribeDBClusterSnapshots
--
--         , requestDescribeDBClusters $
--             newDescribeDBClusters
--
--         , requestDescribeDBEngineVersions $
--             newDescribeDBEngineVersions
--
--         , requestDescribeDBInstanceAutomatedBackups $
--             newDescribeDBInstanceAutomatedBackups
--
--         , requestDescribeDBInstances $
--             newDescribeDBInstances
--
--         , requestDescribeDBLogFiles $
--             newDescribeDBLogFiles
--
--         , requestDescribeDBParameterGroups $
--             newDescribeDBParameterGroups
--
--         , requestDescribeDBParameters $
--             newDescribeDBParameters
--
--         , requestDescribeDBProxies $
--             newDescribeDBProxies
--
--         , requestDescribeDBProxyEndpoints $
--             newDescribeDBProxyEndpoints
--
--         , requestDescribeDBProxyTargetGroups $
--             newDescribeDBProxyTargetGroups
--
--         , requestDescribeDBProxyTargets $
--             newDescribeDBProxyTargets
--
--         , requestDescribeDBSecurityGroups $
--             newDescribeDBSecurityGroups
--
--         , requestDescribeDBSnapshotAttributes $
--             newDescribeDBSnapshotAttributes
--
--         , requestDescribeDBSnapshots $
--             newDescribeDBSnapshots
--
--         , requestDescribeDBSubnetGroups $
--             newDescribeDBSubnetGroups
--
--         , requestDescribeEngineDefaultClusterParameters $
--             newDescribeEngineDefaultClusterParameters
--
--         , requestDescribeEngineDefaultParameters $
--             newDescribeEngineDefaultParameters
--
--         , requestDescribeEventCategories $
--             newDescribeEventCategories
--
--         , requestDescribeEventSubscriptions $
--             newDescribeEventSubscriptions
--
--         , requestDescribeEvents $
--             newDescribeEvents
--
--         , requestDescribeExportTasks $
--             newDescribeExportTasks
--
--         , requestDescribeGlobalClusters $
--             newDescribeGlobalClusters
--
--         , requestDescribeOptionGroupOptions $
--             newDescribeOptionGroupOptions
--
--         , requestDescribeOptionGroups $
--             newDescribeOptionGroups
--
--         , requestDescribeOrderableDBInstanceOptions $
--             newDescribeOrderableDBInstanceOptions
--
--         , requestDescribePendingMaintenanceActions $
--             newDescribePendingMaintenanceActions
--
--         , requestDescribeReservedDBInstances $
--             newDescribeReservedDBInstances
--
--         , requestDescribeReservedDBInstancesOfferings $
--             newDescribeReservedDBInstancesOfferings
--
--         , requestDescribeSourceRegions $
--             newDescribeSourceRegions
--
--         , requestDescribeValidDBInstanceModifications $
--             newDescribeValidDBInstanceModifications
--
--         , requestDownloadDBLogFilePortion $
--             newDownloadDBLogFilePortion
--
--         , requestFailoverDBCluster $
--             newFailoverDBCluster
--
--         , requestFailoverGlobalCluster $
--             newFailoverGlobalCluster
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestModifyActivityStream $
--             newModifyActivityStream
--
--         , requestModifyCertificates $
--             newModifyCertificates
--
--         , requestModifyCurrentDBClusterCapacity $
--             newModifyCurrentDBClusterCapacity
--
--         , requestModifyCustomDBEngineVersion $
--             newModifyCustomDBEngineVersion
--
--         , requestModifyDBCluster $
--             newModifyDBCluster
--
--         , requestModifyDBClusterEndpoint $
--             newModifyDBClusterEndpoint
--
--         , requestModifyDBClusterParameterGroup $
--             newModifyDBClusterParameterGroup
--
--         , requestModifyDBClusterSnapshotAttribute $
--             newModifyDBClusterSnapshotAttribute
--
--         , requestModifyDBInstance $
--             newModifyDBInstance
--
--         , requestModifyDBParameterGroup $
--             newModifyDBParameterGroup
--
--         , requestModifyDBProxy $
--             newModifyDBProxy
--
--         , requestModifyDBProxyEndpoint $
--             newModifyDBProxyEndpoint
--
--         , requestModifyDBProxyTargetGroup $
--             newModifyDBProxyTargetGroup
--
--         , requestModifyDBSnapshot $
--             newModifyDBSnapshot
--
--         , requestModifyDBSnapshotAttribute $
--             newModifyDBSnapshotAttribute
--
--         , requestModifyDBSubnetGroup $
--             newModifyDBSubnetGroup
--
--         , requestModifyEventSubscription $
--             newModifyEventSubscription
--
--         , requestModifyGlobalCluster $
--             newModifyGlobalCluster
--
--         , requestModifyOptionGroup $
--             newModifyOptionGroup
--
--         , requestPromoteReadReplica $
--             newPromoteReadReplica
--
--         , requestPromoteReadReplicaDBCluster $
--             newPromoteReadReplicaDBCluster
--
--         , requestPurchaseReservedDBInstancesOffering $
--             newPurchaseReservedDBInstancesOffering
--
--         , requestRebootDBCluster $
--             newRebootDBCluster
--
--         , requestRebootDBInstance $
--             newRebootDBInstance
--
--         , requestRegisterDBProxyTargets $
--             newRegisterDBProxyTargets
--
--         , requestRemoveFromGlobalCluster $
--             newRemoveFromGlobalCluster
--
--         , requestRemoveRoleFromDBCluster $
--             newRemoveRoleFromDBCluster
--
--         , requestRemoveRoleFromDBInstance $
--             newRemoveRoleFromDBInstance
--
--         , requestRemoveSourceIdentifierFromSubscription $
--             newRemoveSourceIdentifierFromSubscription
--
--         , requestRemoveTagsFromResource $
--             newRemoveTagsFromResource
--
--         , requestResetDBClusterParameterGroup $
--             newResetDBClusterParameterGroup
--
--         , requestResetDBParameterGroup $
--             newResetDBParameterGroup
--
--         , requestRestoreDBClusterFromS3 $
--             newRestoreDBClusterFromS3
--
--         , requestRestoreDBClusterFromSnapshot $
--             newRestoreDBClusterFromSnapshot
--
--         , requestRestoreDBClusterToPointInTime $
--             newRestoreDBClusterToPointInTime
--
--         , requestRestoreDBInstanceFromDBSnapshot $
--             newRestoreDBInstanceFromDBSnapshot
--
--         , requestRestoreDBInstanceFromS3 $
--             newRestoreDBInstanceFromS3
--
--         , requestRestoreDBInstanceToPointInTime $
--             newRestoreDBInstanceToPointInTime
--
--         , requestRevokeDBSecurityGroupIngress $
--             newRevokeDBSecurityGroupIngress
--
--         , requestStartActivityStream $
--             newStartActivityStream
--
--         , requestStartDBCluster $
--             newStartDBCluster
--
--         , requestStartDBInstance $
--             newStartDBInstance
--
--         , requestStartDBInstanceAutomatedBackupsReplication $
--             newStartDBInstanceAutomatedBackupsReplication
--
--         , requestStartExportTask $
--             newStartExportTask
--
--         , requestStopActivityStream $
--             newStopActivityStream
--
--         , requestStopDBCluster $
--             newStopDBCluster
--
--         , requestStopDBInstance $
--             newStopDBInstance
--
--         , requestStopDBInstanceAutomatedBackupsReplication $
--             newStopDBInstanceAutomatedBackupsReplication
--
--         , requestSwitchoverBlueGreenDeployment $
--             newSwitchoverBlueGreenDeployment
--
--         , requestSwitchoverReadReplica $
--             newSwitchoverReadReplica
--
--           ]

--     , testGroup "response"
--         [ responseAddRoleToDBCluster $
--             newAddRoleToDBClusterResponse
--
--         , responseAddRoleToDBInstance $
--             newAddRoleToDBInstanceResponse
--
--         , responseAddSourceIdentifierToSubscription $
--             newAddSourceIdentifierToSubscriptionResponse
--
--         , responseAddTagsToResource $
--             newAddTagsToResourceResponse
--
--         , responseApplyPendingMaintenanceAction $
--             newApplyPendingMaintenanceActionResponse
--
--         , responseAuthorizeDBSecurityGroupIngress $
--             newAuthorizeDBSecurityGroupIngressResponse
--
--         , responseBacktrackDBCluster $
--             newDBClusterBacktrack
--
--         , responseCancelExportTask $
--             newExportTask
--
--         , responseCopyDBClusterParameterGroup $
--             newCopyDBClusterParameterGroupResponse
--
--         , responseCopyDBClusterSnapshot $
--             newCopyDBClusterSnapshotResponse
--
--         , responseCopyDBParameterGroup $
--             newCopyDBParameterGroupResponse
--
--         , responseCopyDBSnapshot $
--             newCopyDBSnapshotResponse
--
--         , responseCopyOptionGroup $
--             newCopyOptionGroupResponse
--
--         , responseCreateBlueGreenDeployment $
--             newCreateBlueGreenDeploymentResponse
--
--         , responseCreateCustomDBEngineVersion $
--             newDBEngineVersion
--
--         , responseCreateDBCluster $
--             newCreateDBClusterResponse
--
--         , responseCreateDBClusterEndpoint $
--             newDBClusterEndpoint
--
--         , responseCreateDBClusterParameterGroup $
--             newCreateDBClusterParameterGroupResponse
--
--         , responseCreateDBClusterSnapshot $
--             newCreateDBClusterSnapshotResponse
--
--         , responseCreateDBInstance $
--             newCreateDBInstanceResponse
--
--         , responseCreateDBInstanceReadReplica $
--             newCreateDBInstanceReadReplicaResponse
--
--         , responseCreateDBParameterGroup $
--             newCreateDBParameterGroupResponse
--
--         , responseCreateDBProxy $
--             newCreateDBProxyResponse
--
--         , responseCreateDBProxyEndpoint $
--             newCreateDBProxyEndpointResponse
--
--         , responseCreateDBSecurityGroup $
--             newCreateDBSecurityGroupResponse
--
--         , responseCreateDBSnapshot $
--             newCreateDBSnapshotResponse
--
--         , responseCreateDBSubnetGroup $
--             newCreateDBSubnetGroupResponse
--
--         , responseCreateEventSubscription $
--             newCreateEventSubscriptionResponse
--
--         , responseCreateGlobalCluster $
--             newCreateGlobalClusterResponse
--
--         , responseCreateOptionGroup $
--             newCreateOptionGroupResponse
--
--         , responseDeleteBlueGreenDeployment $
--             newDeleteBlueGreenDeploymentResponse
--
--         , responseDeleteCustomDBEngineVersion $
--             newDBEngineVersion
--
--         , responseDeleteDBCluster $
--             newDeleteDBClusterResponse
--
--         , responseDeleteDBClusterEndpoint $
--             newDBClusterEndpoint
--
--         , responseDeleteDBClusterParameterGroup $
--             newDeleteDBClusterParameterGroupResponse
--
--         , responseDeleteDBClusterSnapshot $
--             newDeleteDBClusterSnapshotResponse
--
--         , responseDeleteDBInstance $
--             newDeleteDBInstanceResponse
--
--         , responseDeleteDBInstanceAutomatedBackup $
--             newDeleteDBInstanceAutomatedBackupResponse
--
--         , responseDeleteDBParameterGroup $
--             newDeleteDBParameterGroupResponse
--
--         , responseDeleteDBProxy $
--             newDeleteDBProxyResponse
--
--         , responseDeleteDBProxyEndpoint $
--             newDeleteDBProxyEndpointResponse
--
--         , responseDeleteDBSecurityGroup $
--             newDeleteDBSecurityGroupResponse
--
--         , responseDeleteDBSnapshot $
--             newDeleteDBSnapshotResponse
--
--         , responseDeleteDBSubnetGroup $
--             newDeleteDBSubnetGroupResponse
--
--         , responseDeleteEventSubscription $
--             newDeleteEventSubscriptionResponse
--
--         , responseDeleteGlobalCluster $
--             newDeleteGlobalClusterResponse
--
--         , responseDeleteOptionGroup $
--             newDeleteOptionGroupResponse
--
--         , responseDeregisterDBProxyTargets $
--             newDeregisterDBProxyTargetsResponse
--
--         , responseDescribeAccountAttributes $
--             newDescribeAccountAttributesResponse
--
--         , responseDescribeBlueGreenDeployments $
--             newDescribeBlueGreenDeploymentsResponse
--
--         , responseDescribeCertificates $
--             newDescribeCertificatesResponse
--
--         , responseDescribeDBClusterBacktracks $
--             newDescribeDBClusterBacktracksResponse
--
--         , responseDescribeDBClusterEndpoints $
--             newDescribeDBClusterEndpointsResponse
--
--         , responseDescribeDBClusterParameterGroups $
--             newDescribeDBClusterParameterGroupsResponse
--
--         , responseDescribeDBClusterParameters $
--             newDescribeDBClusterParametersResponse
--
--         , responseDescribeDBClusterSnapshotAttributes $
--             newDescribeDBClusterSnapshotAttributesResponse
--
--         , responseDescribeDBClusterSnapshots $
--             newDescribeDBClusterSnapshotsResponse
--
--         , responseDescribeDBClusters $
--             newDescribeDBClustersResponse
--
--         , responseDescribeDBEngineVersions $
--             newDescribeDBEngineVersionsResponse
--
--         , responseDescribeDBInstanceAutomatedBackups $
--             newDescribeDBInstanceAutomatedBackupsResponse
--
--         , responseDescribeDBInstances $
--             newDescribeDBInstancesResponse
--
--         , responseDescribeDBLogFiles $
--             newDescribeDBLogFilesResponse
--
--         , responseDescribeDBParameterGroups $
--             newDescribeDBParameterGroupsResponse
--
--         , responseDescribeDBParameters $
--             newDescribeDBParametersResponse
--
--         , responseDescribeDBProxies $
--             newDescribeDBProxiesResponse
--
--         , responseDescribeDBProxyEndpoints $
--             newDescribeDBProxyEndpointsResponse
--
--         , responseDescribeDBProxyTargetGroups $
--             newDescribeDBProxyTargetGroupsResponse
--
--         , responseDescribeDBProxyTargets $
--             newDescribeDBProxyTargetsResponse
--
--         , responseDescribeDBSecurityGroups $
--             newDescribeDBSecurityGroupsResponse
--
--         , responseDescribeDBSnapshotAttributes $
--             newDescribeDBSnapshotAttributesResponse
--
--         , responseDescribeDBSnapshots $
--             newDescribeDBSnapshotsResponse
--
--         , responseDescribeDBSubnetGroups $
--             newDescribeDBSubnetGroupsResponse
--
--         , responseDescribeEngineDefaultClusterParameters $
--             newDescribeEngineDefaultClusterParametersResponse
--
--         , responseDescribeEngineDefaultParameters $
--             newDescribeEngineDefaultParametersResponse
--
--         , responseDescribeEventCategories $
--             newDescribeEventCategoriesResponse
--
--         , responseDescribeEventSubscriptions $
--             newDescribeEventSubscriptionsResponse
--
--         , responseDescribeEvents $
--             newDescribeEventsResponse
--
--         , responseDescribeExportTasks $
--             newDescribeExportTasksResponse
--
--         , responseDescribeGlobalClusters $
--             newDescribeGlobalClustersResponse
--
--         , responseDescribeOptionGroupOptions $
--             newDescribeOptionGroupOptionsResponse
--
--         , responseDescribeOptionGroups $
--             newDescribeOptionGroupsResponse
--
--         , responseDescribeOrderableDBInstanceOptions $
--             newDescribeOrderableDBInstanceOptionsResponse
--
--         , responseDescribePendingMaintenanceActions $
--             newDescribePendingMaintenanceActionsResponse
--
--         , responseDescribeReservedDBInstances $
--             newDescribeReservedDBInstancesResponse
--
--         , responseDescribeReservedDBInstancesOfferings $
--             newDescribeReservedDBInstancesOfferingsResponse
--
--         , responseDescribeSourceRegions $
--             newDescribeSourceRegionsResponse
--
--         , responseDescribeValidDBInstanceModifications $
--             newDescribeValidDBInstanceModificationsResponse
--
--         , responseDownloadDBLogFilePortion $
--             newDownloadDBLogFilePortionResponse
--
--         , responseFailoverDBCluster $
--             newFailoverDBClusterResponse
--
--         , responseFailoverGlobalCluster $
--             newFailoverGlobalClusterResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseModifyActivityStream $
--             newModifyActivityStreamResponse
--
--         , responseModifyCertificates $
--             newModifyCertificatesResponse
--
--         , responseModifyCurrentDBClusterCapacity $
--             newModifyCurrentDBClusterCapacityResponse
--
--         , responseModifyCustomDBEngineVersion $
--             newDBEngineVersion
--
--         , responseModifyDBCluster $
--             newModifyDBClusterResponse
--
--         , responseModifyDBClusterEndpoint $
--             newDBClusterEndpoint
--
--         , responseModifyDBClusterParameterGroup $
--             newDBClusterParameterGroupNameMessage
--
--         , responseModifyDBClusterSnapshotAttribute $
--             newModifyDBClusterSnapshotAttributeResponse
--
--         , responseModifyDBInstance $
--             newModifyDBInstanceResponse
--
--         , responseModifyDBParameterGroup $
--             newDBParameterGroupNameMessage
--
--         , responseModifyDBProxy $
--             newModifyDBProxyResponse
--
--         , responseModifyDBProxyEndpoint $
--             newModifyDBProxyEndpointResponse
--
--         , responseModifyDBProxyTargetGroup $
--             newModifyDBProxyTargetGroupResponse
--
--         , responseModifyDBSnapshot $
--             newModifyDBSnapshotResponse
--
--         , responseModifyDBSnapshotAttribute $
--             newModifyDBSnapshotAttributeResponse
--
--         , responseModifyDBSubnetGroup $
--             newModifyDBSubnetGroupResponse
--
--         , responseModifyEventSubscription $
--             newModifyEventSubscriptionResponse
--
--         , responseModifyGlobalCluster $
--             newModifyGlobalClusterResponse
--
--         , responseModifyOptionGroup $
--             newModifyOptionGroupResponse
--
--         , responsePromoteReadReplica $
--             newPromoteReadReplicaResponse
--
--         , responsePromoteReadReplicaDBCluster $
--             newPromoteReadReplicaDBClusterResponse
--
--         , responsePurchaseReservedDBInstancesOffering $
--             newPurchaseReservedDBInstancesOfferingResponse
--
--         , responseRebootDBCluster $
--             newRebootDBClusterResponse
--
--         , responseRebootDBInstance $
--             newRebootDBInstanceResponse
--
--         , responseRegisterDBProxyTargets $
--             newRegisterDBProxyTargetsResponse
--
--         , responseRemoveFromGlobalCluster $
--             newRemoveFromGlobalClusterResponse
--
--         , responseRemoveRoleFromDBCluster $
--             newRemoveRoleFromDBClusterResponse
--
--         , responseRemoveRoleFromDBInstance $
--             newRemoveRoleFromDBInstanceResponse
--
--         , responseRemoveSourceIdentifierFromSubscription $
--             newRemoveSourceIdentifierFromSubscriptionResponse
--
--         , responseRemoveTagsFromResource $
--             newRemoveTagsFromResourceResponse
--
--         , responseResetDBClusterParameterGroup $
--             newDBClusterParameterGroupNameMessage
--
--         , responseResetDBParameterGroup $
--             newDBParameterGroupNameMessage
--
--         , responseRestoreDBClusterFromS3 $
--             newRestoreDBClusterFromS3Response
--
--         , responseRestoreDBClusterFromSnapshot $
--             newRestoreDBClusterFromSnapshotResponse
--
--         , responseRestoreDBClusterToPointInTime $
--             newRestoreDBClusterToPointInTimeResponse
--
--         , responseRestoreDBInstanceFromDBSnapshot $
--             newRestoreDBInstanceFromDBSnapshotResponse
--
--         , responseRestoreDBInstanceFromS3 $
--             newRestoreDBInstanceFromS3Response
--
--         , responseRestoreDBInstanceToPointInTime $
--             newRestoreDBInstanceToPointInTimeResponse
--
--         , responseRevokeDBSecurityGroupIngress $
--             newRevokeDBSecurityGroupIngressResponse
--
--         , responseStartActivityStream $
--             newStartActivityStreamResponse
--
--         , responseStartDBCluster $
--             newStartDBClusterResponse
--
--         , responseStartDBInstance $
--             newStartDBInstanceResponse
--
--         , responseStartDBInstanceAutomatedBackupsReplication $
--             newStartDBInstanceAutomatedBackupsReplicationResponse
--
--         , responseStartExportTask $
--             newExportTask
--
--         , responseStopActivityStream $
--             newStopActivityStreamResponse
--
--         , responseStopDBCluster $
--             newStopDBClusterResponse
--
--         , responseStopDBInstance $
--             newStopDBInstanceResponse
--
--         , responseStopDBInstanceAutomatedBackupsReplication $
--             newStopDBInstanceAutomatedBackupsReplicationResponse
--
--         , responseSwitchoverBlueGreenDeployment $
--             newSwitchoverBlueGreenDeploymentResponse
--
--         , responseSwitchoverReadReplica $
--             newSwitchoverReadReplicaResponse
--
--           ]
--     ]

-- Requests

requestAddRoleToDBCluster :: AddRoleToDBCluster -> TestTree
requestAddRoleToDBCluster =
  req
    "AddRoleToDBCluster"
    "fixture/AddRoleToDBCluster.yaml"

requestAddRoleToDBInstance :: AddRoleToDBInstance -> TestTree
requestAddRoleToDBInstance =
  req
    "AddRoleToDBInstance"
    "fixture/AddRoleToDBInstance.yaml"

requestAddSourceIdentifierToSubscription :: AddSourceIdentifierToSubscription -> TestTree
requestAddSourceIdentifierToSubscription =
  req
    "AddSourceIdentifierToSubscription"
    "fixture/AddSourceIdentifierToSubscription.yaml"

requestAddTagsToResource :: AddTagsToResource -> TestTree
requestAddTagsToResource =
  req
    "AddTagsToResource"
    "fixture/AddTagsToResource.yaml"

requestApplyPendingMaintenanceAction :: ApplyPendingMaintenanceAction -> TestTree
requestApplyPendingMaintenanceAction =
  req
    "ApplyPendingMaintenanceAction"
    "fixture/ApplyPendingMaintenanceAction.yaml"

requestAuthorizeDBSecurityGroupIngress :: AuthorizeDBSecurityGroupIngress -> TestTree
requestAuthorizeDBSecurityGroupIngress =
  req
    "AuthorizeDBSecurityGroupIngress"
    "fixture/AuthorizeDBSecurityGroupIngress.yaml"

requestBacktrackDBCluster :: BacktrackDBCluster -> TestTree
requestBacktrackDBCluster =
  req
    "BacktrackDBCluster"
    "fixture/BacktrackDBCluster.yaml"

requestCancelExportTask :: CancelExportTask -> TestTree
requestCancelExportTask =
  req
    "CancelExportTask"
    "fixture/CancelExportTask.yaml"

requestCopyDBClusterParameterGroup :: CopyDBClusterParameterGroup -> TestTree
requestCopyDBClusterParameterGroup =
  req
    "CopyDBClusterParameterGroup"
    "fixture/CopyDBClusterParameterGroup.yaml"

requestCopyDBClusterSnapshot :: CopyDBClusterSnapshot -> TestTree
requestCopyDBClusterSnapshot =
  req
    "CopyDBClusterSnapshot"
    "fixture/CopyDBClusterSnapshot.yaml"

requestCopyDBParameterGroup :: CopyDBParameterGroup -> TestTree
requestCopyDBParameterGroup =
  req
    "CopyDBParameterGroup"
    "fixture/CopyDBParameterGroup.yaml"

requestCopyDBSnapshot :: CopyDBSnapshot -> TestTree
requestCopyDBSnapshot =
  req
    "CopyDBSnapshot"
    "fixture/CopyDBSnapshot.yaml"

requestCopyOptionGroup :: CopyOptionGroup -> TestTree
requestCopyOptionGroup =
  req
    "CopyOptionGroup"
    "fixture/CopyOptionGroup.yaml"

requestCreateBlueGreenDeployment :: CreateBlueGreenDeployment -> TestTree
requestCreateBlueGreenDeployment =
  req
    "CreateBlueGreenDeployment"
    "fixture/CreateBlueGreenDeployment.yaml"

requestCreateCustomDBEngineVersion :: CreateCustomDBEngineVersion -> TestTree
requestCreateCustomDBEngineVersion =
  req
    "CreateCustomDBEngineVersion"
    "fixture/CreateCustomDBEngineVersion.yaml"

requestCreateDBCluster :: CreateDBCluster -> TestTree
requestCreateDBCluster =
  req
    "CreateDBCluster"
    "fixture/CreateDBCluster.yaml"

requestCreateDBClusterEndpoint :: CreateDBClusterEndpoint -> TestTree
requestCreateDBClusterEndpoint =
  req
    "CreateDBClusterEndpoint"
    "fixture/CreateDBClusterEndpoint.yaml"

requestCreateDBClusterParameterGroup :: CreateDBClusterParameterGroup -> TestTree
requestCreateDBClusterParameterGroup =
  req
    "CreateDBClusterParameterGroup"
    "fixture/CreateDBClusterParameterGroup.yaml"

requestCreateDBClusterSnapshot :: CreateDBClusterSnapshot -> TestTree
requestCreateDBClusterSnapshot =
  req
    "CreateDBClusterSnapshot"
    "fixture/CreateDBClusterSnapshot.yaml"

requestCreateDBInstance :: CreateDBInstance -> TestTree
requestCreateDBInstance =
  req
    "CreateDBInstance"
    "fixture/CreateDBInstance.yaml"

requestCreateDBInstanceReadReplica :: CreateDBInstanceReadReplica -> TestTree
requestCreateDBInstanceReadReplica =
  req
    "CreateDBInstanceReadReplica"
    "fixture/CreateDBInstanceReadReplica.yaml"

requestCreateDBParameterGroup :: CreateDBParameterGroup -> TestTree
requestCreateDBParameterGroup =
  req
    "CreateDBParameterGroup"
    "fixture/CreateDBParameterGroup.yaml"

requestCreateDBProxy :: CreateDBProxy -> TestTree
requestCreateDBProxy =
  req
    "CreateDBProxy"
    "fixture/CreateDBProxy.yaml"

requestCreateDBProxyEndpoint :: CreateDBProxyEndpoint -> TestTree
requestCreateDBProxyEndpoint =
  req
    "CreateDBProxyEndpoint"
    "fixture/CreateDBProxyEndpoint.yaml"

requestCreateDBSecurityGroup :: CreateDBSecurityGroup -> TestTree
requestCreateDBSecurityGroup =
  req
    "CreateDBSecurityGroup"
    "fixture/CreateDBSecurityGroup.yaml"

requestCreateDBSnapshot :: CreateDBSnapshot -> TestTree
requestCreateDBSnapshot =
  req
    "CreateDBSnapshot"
    "fixture/CreateDBSnapshot.yaml"

requestCreateDBSubnetGroup :: CreateDBSubnetGroup -> TestTree
requestCreateDBSubnetGroup =
  req
    "CreateDBSubnetGroup"
    "fixture/CreateDBSubnetGroup.yaml"

requestCreateEventSubscription :: CreateEventSubscription -> TestTree
requestCreateEventSubscription =
  req
    "CreateEventSubscription"
    "fixture/CreateEventSubscription.yaml"

requestCreateGlobalCluster :: CreateGlobalCluster -> TestTree
requestCreateGlobalCluster =
  req
    "CreateGlobalCluster"
    "fixture/CreateGlobalCluster.yaml"

requestCreateOptionGroup :: CreateOptionGroup -> TestTree
requestCreateOptionGroup =
  req
    "CreateOptionGroup"
    "fixture/CreateOptionGroup.yaml"

requestDeleteBlueGreenDeployment :: DeleteBlueGreenDeployment -> TestTree
requestDeleteBlueGreenDeployment =
  req
    "DeleteBlueGreenDeployment"
    "fixture/DeleteBlueGreenDeployment.yaml"

requestDeleteCustomDBEngineVersion :: DeleteCustomDBEngineVersion -> TestTree
requestDeleteCustomDBEngineVersion =
  req
    "DeleteCustomDBEngineVersion"
    "fixture/DeleteCustomDBEngineVersion.yaml"

requestDeleteDBCluster :: DeleteDBCluster -> TestTree
requestDeleteDBCluster =
  req
    "DeleteDBCluster"
    "fixture/DeleteDBCluster.yaml"

requestDeleteDBClusterEndpoint :: DeleteDBClusterEndpoint -> TestTree
requestDeleteDBClusterEndpoint =
  req
    "DeleteDBClusterEndpoint"
    "fixture/DeleteDBClusterEndpoint.yaml"

requestDeleteDBClusterParameterGroup :: DeleteDBClusterParameterGroup -> TestTree
requestDeleteDBClusterParameterGroup =
  req
    "DeleteDBClusterParameterGroup"
    "fixture/DeleteDBClusterParameterGroup.yaml"

requestDeleteDBClusterSnapshot :: DeleteDBClusterSnapshot -> TestTree
requestDeleteDBClusterSnapshot =
  req
    "DeleteDBClusterSnapshot"
    "fixture/DeleteDBClusterSnapshot.yaml"

requestDeleteDBInstance :: DeleteDBInstance -> TestTree
requestDeleteDBInstance =
  req
    "DeleteDBInstance"
    "fixture/DeleteDBInstance.yaml"

requestDeleteDBInstanceAutomatedBackup :: DeleteDBInstanceAutomatedBackup -> TestTree
requestDeleteDBInstanceAutomatedBackup =
  req
    "DeleteDBInstanceAutomatedBackup"
    "fixture/DeleteDBInstanceAutomatedBackup.yaml"

requestDeleteDBParameterGroup :: DeleteDBParameterGroup -> TestTree
requestDeleteDBParameterGroup =
  req
    "DeleteDBParameterGroup"
    "fixture/DeleteDBParameterGroup.yaml"

requestDeleteDBProxy :: DeleteDBProxy -> TestTree
requestDeleteDBProxy =
  req
    "DeleteDBProxy"
    "fixture/DeleteDBProxy.yaml"

requestDeleteDBProxyEndpoint :: DeleteDBProxyEndpoint -> TestTree
requestDeleteDBProxyEndpoint =
  req
    "DeleteDBProxyEndpoint"
    "fixture/DeleteDBProxyEndpoint.yaml"

requestDeleteDBSecurityGroup :: DeleteDBSecurityGroup -> TestTree
requestDeleteDBSecurityGroup =
  req
    "DeleteDBSecurityGroup"
    "fixture/DeleteDBSecurityGroup.yaml"

requestDeleteDBSnapshot :: DeleteDBSnapshot -> TestTree
requestDeleteDBSnapshot =
  req
    "DeleteDBSnapshot"
    "fixture/DeleteDBSnapshot.yaml"

requestDeleteDBSubnetGroup :: DeleteDBSubnetGroup -> TestTree
requestDeleteDBSubnetGroup =
  req
    "DeleteDBSubnetGroup"
    "fixture/DeleteDBSubnetGroup.yaml"

requestDeleteEventSubscription :: DeleteEventSubscription -> TestTree
requestDeleteEventSubscription =
  req
    "DeleteEventSubscription"
    "fixture/DeleteEventSubscription.yaml"

requestDeleteGlobalCluster :: DeleteGlobalCluster -> TestTree
requestDeleteGlobalCluster =
  req
    "DeleteGlobalCluster"
    "fixture/DeleteGlobalCluster.yaml"

requestDeleteOptionGroup :: DeleteOptionGroup -> TestTree
requestDeleteOptionGroup =
  req
    "DeleteOptionGroup"
    "fixture/DeleteOptionGroup.yaml"

requestDeregisterDBProxyTargets :: DeregisterDBProxyTargets -> TestTree
requestDeregisterDBProxyTargets =
  req
    "DeregisterDBProxyTargets"
    "fixture/DeregisterDBProxyTargets.yaml"

requestDescribeAccountAttributes :: DescribeAccountAttributes -> TestTree
requestDescribeAccountAttributes =
  req
    "DescribeAccountAttributes"
    "fixture/DescribeAccountAttributes.yaml"

requestDescribeBlueGreenDeployments :: DescribeBlueGreenDeployments -> TestTree
requestDescribeBlueGreenDeployments =
  req
    "DescribeBlueGreenDeployments"
    "fixture/DescribeBlueGreenDeployments.yaml"

requestDescribeCertificates :: DescribeCertificates -> TestTree
requestDescribeCertificates =
  req
    "DescribeCertificates"
    "fixture/DescribeCertificates.yaml"

requestDescribeDBClusterBacktracks :: DescribeDBClusterBacktracks -> TestTree
requestDescribeDBClusterBacktracks =
  req
    "DescribeDBClusterBacktracks"
    "fixture/DescribeDBClusterBacktracks.yaml"

requestDescribeDBClusterEndpoints :: DescribeDBClusterEndpoints -> TestTree
requestDescribeDBClusterEndpoints =
  req
    "DescribeDBClusterEndpoints"
    "fixture/DescribeDBClusterEndpoints.yaml"

requestDescribeDBClusterParameterGroups :: DescribeDBClusterParameterGroups -> TestTree
requestDescribeDBClusterParameterGroups =
  req
    "DescribeDBClusterParameterGroups"
    "fixture/DescribeDBClusterParameterGroups.yaml"

requestDescribeDBClusterParameters :: DescribeDBClusterParameters -> TestTree
requestDescribeDBClusterParameters =
  req
    "DescribeDBClusterParameters"
    "fixture/DescribeDBClusterParameters.yaml"

requestDescribeDBClusterSnapshotAttributes :: DescribeDBClusterSnapshotAttributes -> TestTree
requestDescribeDBClusterSnapshotAttributes =
  req
    "DescribeDBClusterSnapshotAttributes"
    "fixture/DescribeDBClusterSnapshotAttributes.yaml"

requestDescribeDBClusterSnapshots :: DescribeDBClusterSnapshots -> TestTree
requestDescribeDBClusterSnapshots =
  req
    "DescribeDBClusterSnapshots"
    "fixture/DescribeDBClusterSnapshots.yaml"

requestDescribeDBClusters :: DescribeDBClusters -> TestTree
requestDescribeDBClusters =
  req
    "DescribeDBClusters"
    "fixture/DescribeDBClusters.yaml"

requestDescribeDBEngineVersions :: DescribeDBEngineVersions -> TestTree
requestDescribeDBEngineVersions =
  req
    "DescribeDBEngineVersions"
    "fixture/DescribeDBEngineVersions.yaml"

requestDescribeDBInstanceAutomatedBackups :: DescribeDBInstanceAutomatedBackups -> TestTree
requestDescribeDBInstanceAutomatedBackups =
  req
    "DescribeDBInstanceAutomatedBackups"
    "fixture/DescribeDBInstanceAutomatedBackups.yaml"

requestDescribeDBInstances :: DescribeDBInstances -> TestTree
requestDescribeDBInstances =
  req
    "DescribeDBInstances"
    "fixture/DescribeDBInstances.yaml"

requestDescribeDBLogFiles :: DescribeDBLogFiles -> TestTree
requestDescribeDBLogFiles =
  req
    "DescribeDBLogFiles"
    "fixture/DescribeDBLogFiles.yaml"

requestDescribeDBParameterGroups :: DescribeDBParameterGroups -> TestTree
requestDescribeDBParameterGroups =
  req
    "DescribeDBParameterGroups"
    "fixture/DescribeDBParameterGroups.yaml"

requestDescribeDBParameters :: DescribeDBParameters -> TestTree
requestDescribeDBParameters =
  req
    "DescribeDBParameters"
    "fixture/DescribeDBParameters.yaml"

requestDescribeDBProxies :: DescribeDBProxies -> TestTree
requestDescribeDBProxies =
  req
    "DescribeDBProxies"
    "fixture/DescribeDBProxies.yaml"

requestDescribeDBProxyEndpoints :: DescribeDBProxyEndpoints -> TestTree
requestDescribeDBProxyEndpoints =
  req
    "DescribeDBProxyEndpoints"
    "fixture/DescribeDBProxyEndpoints.yaml"

requestDescribeDBProxyTargetGroups :: DescribeDBProxyTargetGroups -> TestTree
requestDescribeDBProxyTargetGroups =
  req
    "DescribeDBProxyTargetGroups"
    "fixture/DescribeDBProxyTargetGroups.yaml"

requestDescribeDBProxyTargets :: DescribeDBProxyTargets -> TestTree
requestDescribeDBProxyTargets =
  req
    "DescribeDBProxyTargets"
    "fixture/DescribeDBProxyTargets.yaml"

requestDescribeDBSecurityGroups :: DescribeDBSecurityGroups -> TestTree
requestDescribeDBSecurityGroups =
  req
    "DescribeDBSecurityGroups"
    "fixture/DescribeDBSecurityGroups.yaml"

requestDescribeDBSnapshotAttributes :: DescribeDBSnapshotAttributes -> TestTree
requestDescribeDBSnapshotAttributes =
  req
    "DescribeDBSnapshotAttributes"
    "fixture/DescribeDBSnapshotAttributes.yaml"

requestDescribeDBSnapshots :: DescribeDBSnapshots -> TestTree
requestDescribeDBSnapshots =
  req
    "DescribeDBSnapshots"
    "fixture/DescribeDBSnapshots.yaml"

requestDescribeDBSubnetGroups :: DescribeDBSubnetGroups -> TestTree
requestDescribeDBSubnetGroups =
  req
    "DescribeDBSubnetGroups"
    "fixture/DescribeDBSubnetGroups.yaml"

requestDescribeEngineDefaultClusterParameters :: DescribeEngineDefaultClusterParameters -> TestTree
requestDescribeEngineDefaultClusterParameters =
  req
    "DescribeEngineDefaultClusterParameters"
    "fixture/DescribeEngineDefaultClusterParameters.yaml"

requestDescribeEngineDefaultParameters :: DescribeEngineDefaultParameters -> TestTree
requestDescribeEngineDefaultParameters =
  req
    "DescribeEngineDefaultParameters"
    "fixture/DescribeEngineDefaultParameters.yaml"

requestDescribeEventCategories :: DescribeEventCategories -> TestTree
requestDescribeEventCategories =
  req
    "DescribeEventCategories"
    "fixture/DescribeEventCategories.yaml"

requestDescribeEventSubscriptions :: DescribeEventSubscriptions -> TestTree
requestDescribeEventSubscriptions =
  req
    "DescribeEventSubscriptions"
    "fixture/DescribeEventSubscriptions.yaml"

requestDescribeEvents :: DescribeEvents -> TestTree
requestDescribeEvents =
  req
    "DescribeEvents"
    "fixture/DescribeEvents.yaml"

requestDescribeExportTasks :: DescribeExportTasks -> TestTree
requestDescribeExportTasks =
  req
    "DescribeExportTasks"
    "fixture/DescribeExportTasks.yaml"

requestDescribeGlobalClusters :: DescribeGlobalClusters -> TestTree
requestDescribeGlobalClusters =
  req
    "DescribeGlobalClusters"
    "fixture/DescribeGlobalClusters.yaml"

requestDescribeOptionGroupOptions :: DescribeOptionGroupOptions -> TestTree
requestDescribeOptionGroupOptions =
  req
    "DescribeOptionGroupOptions"
    "fixture/DescribeOptionGroupOptions.yaml"

requestDescribeOptionGroups :: DescribeOptionGroups -> TestTree
requestDescribeOptionGroups =
  req
    "DescribeOptionGroups"
    "fixture/DescribeOptionGroups.yaml"

requestDescribeOrderableDBInstanceOptions :: DescribeOrderableDBInstanceOptions -> TestTree
requestDescribeOrderableDBInstanceOptions =
  req
    "DescribeOrderableDBInstanceOptions"
    "fixture/DescribeOrderableDBInstanceOptions.yaml"

requestDescribePendingMaintenanceActions :: DescribePendingMaintenanceActions -> TestTree
requestDescribePendingMaintenanceActions =
  req
    "DescribePendingMaintenanceActions"
    "fixture/DescribePendingMaintenanceActions.yaml"

requestDescribeReservedDBInstances :: DescribeReservedDBInstances -> TestTree
requestDescribeReservedDBInstances =
  req
    "DescribeReservedDBInstances"
    "fixture/DescribeReservedDBInstances.yaml"

requestDescribeReservedDBInstancesOfferings :: DescribeReservedDBInstancesOfferings -> TestTree
requestDescribeReservedDBInstancesOfferings =
  req
    "DescribeReservedDBInstancesOfferings"
    "fixture/DescribeReservedDBInstancesOfferings.yaml"

requestDescribeSourceRegions :: DescribeSourceRegions -> TestTree
requestDescribeSourceRegions =
  req
    "DescribeSourceRegions"
    "fixture/DescribeSourceRegions.yaml"

requestDescribeValidDBInstanceModifications :: DescribeValidDBInstanceModifications -> TestTree
requestDescribeValidDBInstanceModifications =
  req
    "DescribeValidDBInstanceModifications"
    "fixture/DescribeValidDBInstanceModifications.yaml"

requestDownloadDBLogFilePortion :: DownloadDBLogFilePortion -> TestTree
requestDownloadDBLogFilePortion =
  req
    "DownloadDBLogFilePortion"
    "fixture/DownloadDBLogFilePortion.yaml"

requestFailoverDBCluster :: FailoverDBCluster -> TestTree
requestFailoverDBCluster =
  req
    "FailoverDBCluster"
    "fixture/FailoverDBCluster.yaml"

requestFailoverGlobalCluster :: FailoverGlobalCluster -> TestTree
requestFailoverGlobalCluster =
  req
    "FailoverGlobalCluster"
    "fixture/FailoverGlobalCluster.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestModifyActivityStream :: ModifyActivityStream -> TestTree
requestModifyActivityStream =
  req
    "ModifyActivityStream"
    "fixture/ModifyActivityStream.yaml"

requestModifyCertificates :: ModifyCertificates -> TestTree
requestModifyCertificates =
  req
    "ModifyCertificates"
    "fixture/ModifyCertificates.yaml"

requestModifyCurrentDBClusterCapacity :: ModifyCurrentDBClusterCapacity -> TestTree
requestModifyCurrentDBClusterCapacity =
  req
    "ModifyCurrentDBClusterCapacity"
    "fixture/ModifyCurrentDBClusterCapacity.yaml"

requestModifyCustomDBEngineVersion :: ModifyCustomDBEngineVersion -> TestTree
requestModifyCustomDBEngineVersion =
  req
    "ModifyCustomDBEngineVersion"
    "fixture/ModifyCustomDBEngineVersion.yaml"

requestModifyDBCluster :: ModifyDBCluster -> TestTree
requestModifyDBCluster =
  req
    "ModifyDBCluster"
    "fixture/ModifyDBCluster.yaml"

requestModifyDBClusterEndpoint :: ModifyDBClusterEndpoint -> TestTree
requestModifyDBClusterEndpoint =
  req
    "ModifyDBClusterEndpoint"
    "fixture/ModifyDBClusterEndpoint.yaml"

requestModifyDBClusterParameterGroup :: ModifyDBClusterParameterGroup -> TestTree
requestModifyDBClusterParameterGroup =
  req
    "ModifyDBClusterParameterGroup"
    "fixture/ModifyDBClusterParameterGroup.yaml"

requestModifyDBClusterSnapshotAttribute :: ModifyDBClusterSnapshotAttribute -> TestTree
requestModifyDBClusterSnapshotAttribute =
  req
    "ModifyDBClusterSnapshotAttribute"
    "fixture/ModifyDBClusterSnapshotAttribute.yaml"

requestModifyDBInstance :: ModifyDBInstance -> TestTree
requestModifyDBInstance =
  req
    "ModifyDBInstance"
    "fixture/ModifyDBInstance.yaml"

requestModifyDBParameterGroup :: ModifyDBParameterGroup -> TestTree
requestModifyDBParameterGroup =
  req
    "ModifyDBParameterGroup"
    "fixture/ModifyDBParameterGroup.yaml"

requestModifyDBProxy :: ModifyDBProxy -> TestTree
requestModifyDBProxy =
  req
    "ModifyDBProxy"
    "fixture/ModifyDBProxy.yaml"

requestModifyDBProxyEndpoint :: ModifyDBProxyEndpoint -> TestTree
requestModifyDBProxyEndpoint =
  req
    "ModifyDBProxyEndpoint"
    "fixture/ModifyDBProxyEndpoint.yaml"

requestModifyDBProxyTargetGroup :: ModifyDBProxyTargetGroup -> TestTree
requestModifyDBProxyTargetGroup =
  req
    "ModifyDBProxyTargetGroup"
    "fixture/ModifyDBProxyTargetGroup.yaml"

requestModifyDBSnapshot :: ModifyDBSnapshot -> TestTree
requestModifyDBSnapshot =
  req
    "ModifyDBSnapshot"
    "fixture/ModifyDBSnapshot.yaml"

requestModifyDBSnapshotAttribute :: ModifyDBSnapshotAttribute -> TestTree
requestModifyDBSnapshotAttribute =
  req
    "ModifyDBSnapshotAttribute"
    "fixture/ModifyDBSnapshotAttribute.yaml"

requestModifyDBSubnetGroup :: ModifyDBSubnetGroup -> TestTree
requestModifyDBSubnetGroup =
  req
    "ModifyDBSubnetGroup"
    "fixture/ModifyDBSubnetGroup.yaml"

requestModifyEventSubscription :: ModifyEventSubscription -> TestTree
requestModifyEventSubscription =
  req
    "ModifyEventSubscription"
    "fixture/ModifyEventSubscription.yaml"

requestModifyGlobalCluster :: ModifyGlobalCluster -> TestTree
requestModifyGlobalCluster =
  req
    "ModifyGlobalCluster"
    "fixture/ModifyGlobalCluster.yaml"

requestModifyOptionGroup :: ModifyOptionGroup -> TestTree
requestModifyOptionGroup =
  req
    "ModifyOptionGroup"
    "fixture/ModifyOptionGroup.yaml"

requestPromoteReadReplica :: PromoteReadReplica -> TestTree
requestPromoteReadReplica =
  req
    "PromoteReadReplica"
    "fixture/PromoteReadReplica.yaml"

requestPromoteReadReplicaDBCluster :: PromoteReadReplicaDBCluster -> TestTree
requestPromoteReadReplicaDBCluster =
  req
    "PromoteReadReplicaDBCluster"
    "fixture/PromoteReadReplicaDBCluster.yaml"

requestPurchaseReservedDBInstancesOffering :: PurchaseReservedDBInstancesOffering -> TestTree
requestPurchaseReservedDBInstancesOffering =
  req
    "PurchaseReservedDBInstancesOffering"
    "fixture/PurchaseReservedDBInstancesOffering.yaml"

requestRebootDBCluster :: RebootDBCluster -> TestTree
requestRebootDBCluster =
  req
    "RebootDBCluster"
    "fixture/RebootDBCluster.yaml"

requestRebootDBInstance :: RebootDBInstance -> TestTree
requestRebootDBInstance =
  req
    "RebootDBInstance"
    "fixture/RebootDBInstance.yaml"

requestRegisterDBProxyTargets :: RegisterDBProxyTargets -> TestTree
requestRegisterDBProxyTargets =
  req
    "RegisterDBProxyTargets"
    "fixture/RegisterDBProxyTargets.yaml"

requestRemoveFromGlobalCluster :: RemoveFromGlobalCluster -> TestTree
requestRemoveFromGlobalCluster =
  req
    "RemoveFromGlobalCluster"
    "fixture/RemoveFromGlobalCluster.yaml"

requestRemoveRoleFromDBCluster :: RemoveRoleFromDBCluster -> TestTree
requestRemoveRoleFromDBCluster =
  req
    "RemoveRoleFromDBCluster"
    "fixture/RemoveRoleFromDBCluster.yaml"

requestRemoveRoleFromDBInstance :: RemoveRoleFromDBInstance -> TestTree
requestRemoveRoleFromDBInstance =
  req
    "RemoveRoleFromDBInstance"
    "fixture/RemoveRoleFromDBInstance.yaml"

requestRemoveSourceIdentifierFromSubscription :: RemoveSourceIdentifierFromSubscription -> TestTree
requestRemoveSourceIdentifierFromSubscription =
  req
    "RemoveSourceIdentifierFromSubscription"
    "fixture/RemoveSourceIdentifierFromSubscription.yaml"

requestRemoveTagsFromResource :: RemoveTagsFromResource -> TestTree
requestRemoveTagsFromResource =
  req
    "RemoveTagsFromResource"
    "fixture/RemoveTagsFromResource.yaml"

requestResetDBClusterParameterGroup :: ResetDBClusterParameterGroup -> TestTree
requestResetDBClusterParameterGroup =
  req
    "ResetDBClusterParameterGroup"
    "fixture/ResetDBClusterParameterGroup.yaml"

requestResetDBParameterGroup :: ResetDBParameterGroup -> TestTree
requestResetDBParameterGroup =
  req
    "ResetDBParameterGroup"
    "fixture/ResetDBParameterGroup.yaml"

requestRestoreDBClusterFromS3 :: RestoreDBClusterFromS3 -> TestTree
requestRestoreDBClusterFromS3 =
  req
    "RestoreDBClusterFromS3"
    "fixture/RestoreDBClusterFromS3.yaml"

requestRestoreDBClusterFromSnapshot :: RestoreDBClusterFromSnapshot -> TestTree
requestRestoreDBClusterFromSnapshot =
  req
    "RestoreDBClusterFromSnapshot"
    "fixture/RestoreDBClusterFromSnapshot.yaml"

requestRestoreDBClusterToPointInTime :: RestoreDBClusterToPointInTime -> TestTree
requestRestoreDBClusterToPointInTime =
  req
    "RestoreDBClusterToPointInTime"
    "fixture/RestoreDBClusterToPointInTime.yaml"

requestRestoreDBInstanceFromDBSnapshot :: RestoreDBInstanceFromDBSnapshot -> TestTree
requestRestoreDBInstanceFromDBSnapshot =
  req
    "RestoreDBInstanceFromDBSnapshot"
    "fixture/RestoreDBInstanceFromDBSnapshot.yaml"

requestRestoreDBInstanceFromS3 :: RestoreDBInstanceFromS3 -> TestTree
requestRestoreDBInstanceFromS3 =
  req
    "RestoreDBInstanceFromS3"
    "fixture/RestoreDBInstanceFromS3.yaml"

requestRestoreDBInstanceToPointInTime :: RestoreDBInstanceToPointInTime -> TestTree
requestRestoreDBInstanceToPointInTime =
  req
    "RestoreDBInstanceToPointInTime"
    "fixture/RestoreDBInstanceToPointInTime.yaml"

requestRevokeDBSecurityGroupIngress :: RevokeDBSecurityGroupIngress -> TestTree
requestRevokeDBSecurityGroupIngress =
  req
    "RevokeDBSecurityGroupIngress"
    "fixture/RevokeDBSecurityGroupIngress.yaml"

requestStartActivityStream :: StartActivityStream -> TestTree
requestStartActivityStream =
  req
    "StartActivityStream"
    "fixture/StartActivityStream.yaml"

requestStartDBCluster :: StartDBCluster -> TestTree
requestStartDBCluster =
  req
    "StartDBCluster"
    "fixture/StartDBCluster.yaml"

requestStartDBInstance :: StartDBInstance -> TestTree
requestStartDBInstance =
  req
    "StartDBInstance"
    "fixture/StartDBInstance.yaml"

requestStartDBInstanceAutomatedBackupsReplication :: StartDBInstanceAutomatedBackupsReplication -> TestTree
requestStartDBInstanceAutomatedBackupsReplication =
  req
    "StartDBInstanceAutomatedBackupsReplication"
    "fixture/StartDBInstanceAutomatedBackupsReplication.yaml"

requestStartExportTask :: StartExportTask -> TestTree
requestStartExportTask =
  req
    "StartExportTask"
    "fixture/StartExportTask.yaml"

requestStopActivityStream :: StopActivityStream -> TestTree
requestStopActivityStream =
  req
    "StopActivityStream"
    "fixture/StopActivityStream.yaml"

requestStopDBCluster :: StopDBCluster -> TestTree
requestStopDBCluster =
  req
    "StopDBCluster"
    "fixture/StopDBCluster.yaml"

requestStopDBInstance :: StopDBInstance -> TestTree
requestStopDBInstance =
  req
    "StopDBInstance"
    "fixture/StopDBInstance.yaml"

requestStopDBInstanceAutomatedBackupsReplication :: StopDBInstanceAutomatedBackupsReplication -> TestTree
requestStopDBInstanceAutomatedBackupsReplication =
  req
    "StopDBInstanceAutomatedBackupsReplication"
    "fixture/StopDBInstanceAutomatedBackupsReplication.yaml"

requestSwitchoverBlueGreenDeployment :: SwitchoverBlueGreenDeployment -> TestTree
requestSwitchoverBlueGreenDeployment =
  req
    "SwitchoverBlueGreenDeployment"
    "fixture/SwitchoverBlueGreenDeployment.yaml"

requestSwitchoverReadReplica :: SwitchoverReadReplica -> TestTree
requestSwitchoverReadReplica =
  req
    "SwitchoverReadReplica"
    "fixture/SwitchoverReadReplica.yaml"

-- Responses

responseAddRoleToDBCluster :: AddRoleToDBClusterResponse -> TestTree
responseAddRoleToDBCluster =
  res
    "AddRoleToDBClusterResponse"
    "fixture/AddRoleToDBClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddRoleToDBCluster)

responseAddRoleToDBInstance :: AddRoleToDBInstanceResponse -> TestTree
responseAddRoleToDBInstance =
  res
    "AddRoleToDBInstanceResponse"
    "fixture/AddRoleToDBInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddRoleToDBInstance)

responseAddSourceIdentifierToSubscription :: AddSourceIdentifierToSubscriptionResponse -> TestTree
responseAddSourceIdentifierToSubscription =
  res
    "AddSourceIdentifierToSubscriptionResponse"
    "fixture/AddSourceIdentifierToSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddSourceIdentifierToSubscription)

responseAddTagsToResource :: AddTagsToResourceResponse -> TestTree
responseAddTagsToResource =
  res
    "AddTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddTagsToResource)

responseApplyPendingMaintenanceAction :: ApplyPendingMaintenanceActionResponse -> TestTree
responseApplyPendingMaintenanceAction =
  res
    "ApplyPendingMaintenanceActionResponse"
    "fixture/ApplyPendingMaintenanceActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ApplyPendingMaintenanceAction)

responseAuthorizeDBSecurityGroupIngress :: AuthorizeDBSecurityGroupIngressResponse -> TestTree
responseAuthorizeDBSecurityGroupIngress =
  res
    "AuthorizeDBSecurityGroupIngressResponse"
    "fixture/AuthorizeDBSecurityGroupIngressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AuthorizeDBSecurityGroupIngress)

responseBacktrackDBCluster :: DBClusterBacktrack -> TestTree
responseBacktrackDBCluster =
  res
    "BacktrackDBClusterResponse"
    "fixture/BacktrackDBClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BacktrackDBCluster)

responseCancelExportTask :: ExportTask -> TestTree
responseCancelExportTask =
  res
    "CancelExportTaskResponse"
    "fixture/CancelExportTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelExportTask)

responseCopyDBClusterParameterGroup :: CopyDBClusterParameterGroupResponse -> TestTree
responseCopyDBClusterParameterGroup =
  res
    "CopyDBClusterParameterGroupResponse"
    "fixture/CopyDBClusterParameterGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CopyDBClusterParameterGroup)

responseCopyDBClusterSnapshot :: CopyDBClusterSnapshotResponse -> TestTree
responseCopyDBClusterSnapshot =
  res
    "CopyDBClusterSnapshotResponse"
    "fixture/CopyDBClusterSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CopyDBClusterSnapshot)

responseCopyDBParameterGroup :: CopyDBParameterGroupResponse -> TestTree
responseCopyDBParameterGroup =
  res
    "CopyDBParameterGroupResponse"
    "fixture/CopyDBParameterGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CopyDBParameterGroup)

responseCopyDBSnapshot :: CopyDBSnapshotResponse -> TestTree
responseCopyDBSnapshot =
  res
    "CopyDBSnapshotResponse"
    "fixture/CopyDBSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CopyDBSnapshot)

responseCopyOptionGroup :: CopyOptionGroupResponse -> TestTree
responseCopyOptionGroup =
  res
    "CopyOptionGroupResponse"
    "fixture/CopyOptionGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CopyOptionGroup)

responseCreateBlueGreenDeployment :: CreateBlueGreenDeploymentResponse -> TestTree
responseCreateBlueGreenDeployment =
  res
    "CreateBlueGreenDeploymentResponse"
    "fixture/CreateBlueGreenDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBlueGreenDeployment)

responseCreateCustomDBEngineVersion :: DBEngineVersion -> TestTree
responseCreateCustomDBEngineVersion =
  res
    "CreateCustomDBEngineVersionResponse"
    "fixture/CreateCustomDBEngineVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCustomDBEngineVersion)

responseCreateDBCluster :: CreateDBClusterResponse -> TestTree
responseCreateDBCluster =
  res
    "CreateDBClusterResponse"
    "fixture/CreateDBClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDBCluster)

responseCreateDBClusterEndpoint :: DBClusterEndpoint -> TestTree
responseCreateDBClusterEndpoint =
  res
    "CreateDBClusterEndpointResponse"
    "fixture/CreateDBClusterEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDBClusterEndpoint)

responseCreateDBClusterParameterGroup :: CreateDBClusterParameterGroupResponse -> TestTree
responseCreateDBClusterParameterGroup =
  res
    "CreateDBClusterParameterGroupResponse"
    "fixture/CreateDBClusterParameterGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDBClusterParameterGroup)

responseCreateDBClusterSnapshot :: CreateDBClusterSnapshotResponse -> TestTree
responseCreateDBClusterSnapshot =
  res
    "CreateDBClusterSnapshotResponse"
    "fixture/CreateDBClusterSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDBClusterSnapshot)

responseCreateDBInstance :: CreateDBInstanceResponse -> TestTree
responseCreateDBInstance =
  res
    "CreateDBInstanceResponse"
    "fixture/CreateDBInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDBInstance)

responseCreateDBInstanceReadReplica :: CreateDBInstanceReadReplicaResponse -> TestTree
responseCreateDBInstanceReadReplica =
  res
    "CreateDBInstanceReadReplicaResponse"
    "fixture/CreateDBInstanceReadReplicaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDBInstanceReadReplica)

responseCreateDBParameterGroup :: CreateDBParameterGroupResponse -> TestTree
responseCreateDBParameterGroup =
  res
    "CreateDBParameterGroupResponse"
    "fixture/CreateDBParameterGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDBParameterGroup)

responseCreateDBProxy :: CreateDBProxyResponse -> TestTree
responseCreateDBProxy =
  res
    "CreateDBProxyResponse"
    "fixture/CreateDBProxyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDBProxy)

responseCreateDBProxyEndpoint :: CreateDBProxyEndpointResponse -> TestTree
responseCreateDBProxyEndpoint =
  res
    "CreateDBProxyEndpointResponse"
    "fixture/CreateDBProxyEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDBProxyEndpoint)

responseCreateDBSecurityGroup :: CreateDBSecurityGroupResponse -> TestTree
responseCreateDBSecurityGroup =
  res
    "CreateDBSecurityGroupResponse"
    "fixture/CreateDBSecurityGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDBSecurityGroup)

responseCreateDBSnapshot :: CreateDBSnapshotResponse -> TestTree
responseCreateDBSnapshot =
  res
    "CreateDBSnapshotResponse"
    "fixture/CreateDBSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDBSnapshot)

responseCreateDBSubnetGroup :: CreateDBSubnetGroupResponse -> TestTree
responseCreateDBSubnetGroup =
  res
    "CreateDBSubnetGroupResponse"
    "fixture/CreateDBSubnetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDBSubnetGroup)

responseCreateEventSubscription :: CreateEventSubscriptionResponse -> TestTree
responseCreateEventSubscription =
  res
    "CreateEventSubscriptionResponse"
    "fixture/CreateEventSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEventSubscription)

responseCreateGlobalCluster :: CreateGlobalClusterResponse -> TestTree
responseCreateGlobalCluster =
  res
    "CreateGlobalClusterResponse"
    "fixture/CreateGlobalClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGlobalCluster)

responseCreateOptionGroup :: CreateOptionGroupResponse -> TestTree
responseCreateOptionGroup =
  res
    "CreateOptionGroupResponse"
    "fixture/CreateOptionGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateOptionGroup)

responseDeleteBlueGreenDeployment :: DeleteBlueGreenDeploymentResponse -> TestTree
responseDeleteBlueGreenDeployment =
  res
    "DeleteBlueGreenDeploymentResponse"
    "fixture/DeleteBlueGreenDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBlueGreenDeployment)

responseDeleteCustomDBEngineVersion :: DBEngineVersion -> TestTree
responseDeleteCustomDBEngineVersion =
  res
    "DeleteCustomDBEngineVersionResponse"
    "fixture/DeleteCustomDBEngineVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCustomDBEngineVersion)

responseDeleteDBCluster :: DeleteDBClusterResponse -> TestTree
responseDeleteDBCluster =
  res
    "DeleteDBClusterResponse"
    "fixture/DeleteDBClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDBCluster)

responseDeleteDBClusterEndpoint :: DBClusterEndpoint -> TestTree
responseDeleteDBClusterEndpoint =
  res
    "DeleteDBClusterEndpointResponse"
    "fixture/DeleteDBClusterEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDBClusterEndpoint)

responseDeleteDBClusterParameterGroup :: DeleteDBClusterParameterGroupResponse -> TestTree
responseDeleteDBClusterParameterGroup =
  res
    "DeleteDBClusterParameterGroupResponse"
    "fixture/DeleteDBClusterParameterGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDBClusterParameterGroup)

responseDeleteDBClusterSnapshot :: DeleteDBClusterSnapshotResponse -> TestTree
responseDeleteDBClusterSnapshot =
  res
    "DeleteDBClusterSnapshotResponse"
    "fixture/DeleteDBClusterSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDBClusterSnapshot)

responseDeleteDBInstance :: DeleteDBInstanceResponse -> TestTree
responseDeleteDBInstance =
  res
    "DeleteDBInstanceResponse"
    "fixture/DeleteDBInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDBInstance)

responseDeleteDBInstanceAutomatedBackup :: DeleteDBInstanceAutomatedBackupResponse -> TestTree
responseDeleteDBInstanceAutomatedBackup =
  res
    "DeleteDBInstanceAutomatedBackupResponse"
    "fixture/DeleteDBInstanceAutomatedBackupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDBInstanceAutomatedBackup)

responseDeleteDBParameterGroup :: DeleteDBParameterGroupResponse -> TestTree
responseDeleteDBParameterGroup =
  res
    "DeleteDBParameterGroupResponse"
    "fixture/DeleteDBParameterGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDBParameterGroup)

responseDeleteDBProxy :: DeleteDBProxyResponse -> TestTree
responseDeleteDBProxy =
  res
    "DeleteDBProxyResponse"
    "fixture/DeleteDBProxyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDBProxy)

responseDeleteDBProxyEndpoint :: DeleteDBProxyEndpointResponse -> TestTree
responseDeleteDBProxyEndpoint =
  res
    "DeleteDBProxyEndpointResponse"
    "fixture/DeleteDBProxyEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDBProxyEndpoint)

responseDeleteDBSecurityGroup :: DeleteDBSecurityGroupResponse -> TestTree
responseDeleteDBSecurityGroup =
  res
    "DeleteDBSecurityGroupResponse"
    "fixture/DeleteDBSecurityGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDBSecurityGroup)

responseDeleteDBSnapshot :: DeleteDBSnapshotResponse -> TestTree
responseDeleteDBSnapshot =
  res
    "DeleteDBSnapshotResponse"
    "fixture/DeleteDBSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDBSnapshot)

responseDeleteDBSubnetGroup :: DeleteDBSubnetGroupResponse -> TestTree
responseDeleteDBSubnetGroup =
  res
    "DeleteDBSubnetGroupResponse"
    "fixture/DeleteDBSubnetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDBSubnetGroup)

responseDeleteEventSubscription :: DeleteEventSubscriptionResponse -> TestTree
responseDeleteEventSubscription =
  res
    "DeleteEventSubscriptionResponse"
    "fixture/DeleteEventSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEventSubscription)

responseDeleteGlobalCluster :: DeleteGlobalClusterResponse -> TestTree
responseDeleteGlobalCluster =
  res
    "DeleteGlobalClusterResponse"
    "fixture/DeleteGlobalClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGlobalCluster)

responseDeleteOptionGroup :: DeleteOptionGroupResponse -> TestTree
responseDeleteOptionGroup =
  res
    "DeleteOptionGroupResponse"
    "fixture/DeleteOptionGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteOptionGroup)

responseDeregisterDBProxyTargets :: DeregisterDBProxyTargetsResponse -> TestTree
responseDeregisterDBProxyTargets =
  res
    "DeregisterDBProxyTargetsResponse"
    "fixture/DeregisterDBProxyTargetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterDBProxyTargets)

responseDescribeAccountAttributes :: DescribeAccountAttributesResponse -> TestTree
responseDescribeAccountAttributes =
  res
    "DescribeAccountAttributesResponse"
    "fixture/DescribeAccountAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAccountAttributes)

responseDescribeBlueGreenDeployments :: DescribeBlueGreenDeploymentsResponse -> TestTree
responseDescribeBlueGreenDeployments =
  res
    "DescribeBlueGreenDeploymentsResponse"
    "fixture/DescribeBlueGreenDeploymentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBlueGreenDeployments)

responseDescribeCertificates :: DescribeCertificatesResponse -> TestTree
responseDescribeCertificates =
  res
    "DescribeCertificatesResponse"
    "fixture/DescribeCertificatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCertificates)

responseDescribeDBClusterBacktracks :: DescribeDBClusterBacktracksResponse -> TestTree
responseDescribeDBClusterBacktracks =
  res
    "DescribeDBClusterBacktracksResponse"
    "fixture/DescribeDBClusterBacktracksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDBClusterBacktracks)

responseDescribeDBClusterEndpoints :: DescribeDBClusterEndpointsResponse -> TestTree
responseDescribeDBClusterEndpoints =
  res
    "DescribeDBClusterEndpointsResponse"
    "fixture/DescribeDBClusterEndpointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDBClusterEndpoints)

responseDescribeDBClusterParameterGroups :: DescribeDBClusterParameterGroupsResponse -> TestTree
responseDescribeDBClusterParameterGroups =
  res
    "DescribeDBClusterParameterGroupsResponse"
    "fixture/DescribeDBClusterParameterGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDBClusterParameterGroups)

responseDescribeDBClusterParameters :: DescribeDBClusterParametersResponse -> TestTree
responseDescribeDBClusterParameters =
  res
    "DescribeDBClusterParametersResponse"
    "fixture/DescribeDBClusterParametersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDBClusterParameters)

responseDescribeDBClusterSnapshotAttributes :: DescribeDBClusterSnapshotAttributesResponse -> TestTree
responseDescribeDBClusterSnapshotAttributes =
  res
    "DescribeDBClusterSnapshotAttributesResponse"
    "fixture/DescribeDBClusterSnapshotAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDBClusterSnapshotAttributes)

responseDescribeDBClusterSnapshots :: DescribeDBClusterSnapshotsResponse -> TestTree
responseDescribeDBClusterSnapshots =
  res
    "DescribeDBClusterSnapshotsResponse"
    "fixture/DescribeDBClusterSnapshotsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDBClusterSnapshots)

responseDescribeDBClusters :: DescribeDBClustersResponse -> TestTree
responseDescribeDBClusters =
  res
    "DescribeDBClustersResponse"
    "fixture/DescribeDBClustersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDBClusters)

responseDescribeDBEngineVersions :: DescribeDBEngineVersionsResponse -> TestTree
responseDescribeDBEngineVersions =
  res
    "DescribeDBEngineVersionsResponse"
    "fixture/DescribeDBEngineVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDBEngineVersions)

responseDescribeDBInstanceAutomatedBackups :: DescribeDBInstanceAutomatedBackupsResponse -> TestTree
responseDescribeDBInstanceAutomatedBackups =
  res
    "DescribeDBInstanceAutomatedBackupsResponse"
    "fixture/DescribeDBInstanceAutomatedBackupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDBInstanceAutomatedBackups)

responseDescribeDBInstances :: DescribeDBInstancesResponse -> TestTree
responseDescribeDBInstances =
  res
    "DescribeDBInstancesResponse"
    "fixture/DescribeDBInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDBInstances)

responseDescribeDBLogFiles :: DescribeDBLogFilesResponse -> TestTree
responseDescribeDBLogFiles =
  res
    "DescribeDBLogFilesResponse"
    "fixture/DescribeDBLogFilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDBLogFiles)

responseDescribeDBParameterGroups :: DescribeDBParameterGroupsResponse -> TestTree
responseDescribeDBParameterGroups =
  res
    "DescribeDBParameterGroupsResponse"
    "fixture/DescribeDBParameterGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDBParameterGroups)

responseDescribeDBParameters :: DescribeDBParametersResponse -> TestTree
responseDescribeDBParameters =
  res
    "DescribeDBParametersResponse"
    "fixture/DescribeDBParametersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDBParameters)

responseDescribeDBProxies :: DescribeDBProxiesResponse -> TestTree
responseDescribeDBProxies =
  res
    "DescribeDBProxiesResponse"
    "fixture/DescribeDBProxiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDBProxies)

responseDescribeDBProxyEndpoints :: DescribeDBProxyEndpointsResponse -> TestTree
responseDescribeDBProxyEndpoints =
  res
    "DescribeDBProxyEndpointsResponse"
    "fixture/DescribeDBProxyEndpointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDBProxyEndpoints)

responseDescribeDBProxyTargetGroups :: DescribeDBProxyTargetGroupsResponse -> TestTree
responseDescribeDBProxyTargetGroups =
  res
    "DescribeDBProxyTargetGroupsResponse"
    "fixture/DescribeDBProxyTargetGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDBProxyTargetGroups)

responseDescribeDBProxyTargets :: DescribeDBProxyTargetsResponse -> TestTree
responseDescribeDBProxyTargets =
  res
    "DescribeDBProxyTargetsResponse"
    "fixture/DescribeDBProxyTargetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDBProxyTargets)

responseDescribeDBSecurityGroups :: DescribeDBSecurityGroupsResponse -> TestTree
responseDescribeDBSecurityGroups =
  res
    "DescribeDBSecurityGroupsResponse"
    "fixture/DescribeDBSecurityGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDBSecurityGroups)

responseDescribeDBSnapshotAttributes :: DescribeDBSnapshotAttributesResponse -> TestTree
responseDescribeDBSnapshotAttributes =
  res
    "DescribeDBSnapshotAttributesResponse"
    "fixture/DescribeDBSnapshotAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDBSnapshotAttributes)

responseDescribeDBSnapshots :: DescribeDBSnapshotsResponse -> TestTree
responseDescribeDBSnapshots =
  res
    "DescribeDBSnapshotsResponse"
    "fixture/DescribeDBSnapshotsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDBSnapshots)

responseDescribeDBSubnetGroups :: DescribeDBSubnetGroupsResponse -> TestTree
responseDescribeDBSubnetGroups =
  res
    "DescribeDBSubnetGroupsResponse"
    "fixture/DescribeDBSubnetGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDBSubnetGroups)

responseDescribeEngineDefaultClusterParameters :: DescribeEngineDefaultClusterParametersResponse -> TestTree
responseDescribeEngineDefaultClusterParameters =
  res
    "DescribeEngineDefaultClusterParametersResponse"
    "fixture/DescribeEngineDefaultClusterParametersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEngineDefaultClusterParameters)

responseDescribeEngineDefaultParameters :: DescribeEngineDefaultParametersResponse -> TestTree
responseDescribeEngineDefaultParameters =
  res
    "DescribeEngineDefaultParametersResponse"
    "fixture/DescribeEngineDefaultParametersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEngineDefaultParameters)

responseDescribeEventCategories :: DescribeEventCategoriesResponse -> TestTree
responseDescribeEventCategories =
  res
    "DescribeEventCategoriesResponse"
    "fixture/DescribeEventCategoriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEventCategories)

responseDescribeEventSubscriptions :: DescribeEventSubscriptionsResponse -> TestTree
responseDescribeEventSubscriptions =
  res
    "DescribeEventSubscriptionsResponse"
    "fixture/DescribeEventSubscriptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEventSubscriptions)

responseDescribeEvents :: DescribeEventsResponse -> TestTree
responseDescribeEvents =
  res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEvents)

responseDescribeExportTasks :: DescribeExportTasksResponse -> TestTree
responseDescribeExportTasks =
  res
    "DescribeExportTasksResponse"
    "fixture/DescribeExportTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeExportTasks)

responseDescribeGlobalClusters :: DescribeGlobalClustersResponse -> TestTree
responseDescribeGlobalClusters =
  res
    "DescribeGlobalClustersResponse"
    "fixture/DescribeGlobalClustersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeGlobalClusters)

responseDescribeOptionGroupOptions :: DescribeOptionGroupOptionsResponse -> TestTree
responseDescribeOptionGroupOptions =
  res
    "DescribeOptionGroupOptionsResponse"
    "fixture/DescribeOptionGroupOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeOptionGroupOptions)

responseDescribeOptionGroups :: DescribeOptionGroupsResponse -> TestTree
responseDescribeOptionGroups =
  res
    "DescribeOptionGroupsResponse"
    "fixture/DescribeOptionGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeOptionGroups)

responseDescribeOrderableDBInstanceOptions :: DescribeOrderableDBInstanceOptionsResponse -> TestTree
responseDescribeOrderableDBInstanceOptions =
  res
    "DescribeOrderableDBInstanceOptionsResponse"
    "fixture/DescribeOrderableDBInstanceOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeOrderableDBInstanceOptions)

responseDescribePendingMaintenanceActions :: DescribePendingMaintenanceActionsResponse -> TestTree
responseDescribePendingMaintenanceActions =
  res
    "DescribePendingMaintenanceActionsResponse"
    "fixture/DescribePendingMaintenanceActionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePendingMaintenanceActions)

responseDescribeReservedDBInstances :: DescribeReservedDBInstancesResponse -> TestTree
responseDescribeReservedDBInstances =
  res
    "DescribeReservedDBInstancesResponse"
    "fixture/DescribeReservedDBInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReservedDBInstances)

responseDescribeReservedDBInstancesOfferings :: DescribeReservedDBInstancesOfferingsResponse -> TestTree
responseDescribeReservedDBInstancesOfferings =
  res
    "DescribeReservedDBInstancesOfferingsResponse"
    "fixture/DescribeReservedDBInstancesOfferingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReservedDBInstancesOfferings)

responseDescribeSourceRegions :: DescribeSourceRegionsResponse -> TestTree
responseDescribeSourceRegions =
  res
    "DescribeSourceRegionsResponse"
    "fixture/DescribeSourceRegionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSourceRegions)

responseDescribeValidDBInstanceModifications :: DescribeValidDBInstanceModificationsResponse -> TestTree
responseDescribeValidDBInstanceModifications =
  res
    "DescribeValidDBInstanceModificationsResponse"
    "fixture/DescribeValidDBInstanceModificationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeValidDBInstanceModifications)

responseDownloadDBLogFilePortion :: DownloadDBLogFilePortionResponse -> TestTree
responseDownloadDBLogFilePortion =
  res
    "DownloadDBLogFilePortionResponse"
    "fixture/DownloadDBLogFilePortionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DownloadDBLogFilePortion)

responseFailoverDBCluster :: FailoverDBClusterResponse -> TestTree
responseFailoverDBCluster =
  res
    "FailoverDBClusterResponse"
    "fixture/FailoverDBClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy FailoverDBCluster)

responseFailoverGlobalCluster :: FailoverGlobalClusterResponse -> TestTree
responseFailoverGlobalCluster =
  res
    "FailoverGlobalClusterResponse"
    "fixture/FailoverGlobalClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy FailoverGlobalCluster)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseModifyActivityStream :: ModifyActivityStreamResponse -> TestTree
responseModifyActivityStream =
  res
    "ModifyActivityStreamResponse"
    "fixture/ModifyActivityStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyActivityStream)

responseModifyCertificates :: ModifyCertificatesResponse -> TestTree
responseModifyCertificates =
  res
    "ModifyCertificatesResponse"
    "fixture/ModifyCertificatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyCertificates)

responseModifyCurrentDBClusterCapacity :: ModifyCurrentDBClusterCapacityResponse -> TestTree
responseModifyCurrentDBClusterCapacity =
  res
    "ModifyCurrentDBClusterCapacityResponse"
    "fixture/ModifyCurrentDBClusterCapacityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyCurrentDBClusterCapacity)

responseModifyCustomDBEngineVersion :: DBEngineVersion -> TestTree
responseModifyCustomDBEngineVersion =
  res
    "ModifyCustomDBEngineVersionResponse"
    "fixture/ModifyCustomDBEngineVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyCustomDBEngineVersion)

responseModifyDBCluster :: ModifyDBClusterResponse -> TestTree
responseModifyDBCluster =
  res
    "ModifyDBClusterResponse"
    "fixture/ModifyDBClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyDBCluster)

responseModifyDBClusterEndpoint :: DBClusterEndpoint -> TestTree
responseModifyDBClusterEndpoint =
  res
    "ModifyDBClusterEndpointResponse"
    "fixture/ModifyDBClusterEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyDBClusterEndpoint)

responseModifyDBClusterParameterGroup :: DBClusterParameterGroupNameMessage -> TestTree
responseModifyDBClusterParameterGroup =
  res
    "ModifyDBClusterParameterGroupResponse"
    "fixture/ModifyDBClusterParameterGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyDBClusterParameterGroup)

responseModifyDBClusterSnapshotAttribute :: ModifyDBClusterSnapshotAttributeResponse -> TestTree
responseModifyDBClusterSnapshotAttribute =
  res
    "ModifyDBClusterSnapshotAttributeResponse"
    "fixture/ModifyDBClusterSnapshotAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyDBClusterSnapshotAttribute)

responseModifyDBInstance :: ModifyDBInstanceResponse -> TestTree
responseModifyDBInstance =
  res
    "ModifyDBInstanceResponse"
    "fixture/ModifyDBInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyDBInstance)

responseModifyDBParameterGroup :: DBParameterGroupNameMessage -> TestTree
responseModifyDBParameterGroup =
  res
    "ModifyDBParameterGroupResponse"
    "fixture/ModifyDBParameterGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyDBParameterGroup)

responseModifyDBProxy :: ModifyDBProxyResponse -> TestTree
responseModifyDBProxy =
  res
    "ModifyDBProxyResponse"
    "fixture/ModifyDBProxyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyDBProxy)

responseModifyDBProxyEndpoint :: ModifyDBProxyEndpointResponse -> TestTree
responseModifyDBProxyEndpoint =
  res
    "ModifyDBProxyEndpointResponse"
    "fixture/ModifyDBProxyEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyDBProxyEndpoint)

responseModifyDBProxyTargetGroup :: ModifyDBProxyTargetGroupResponse -> TestTree
responseModifyDBProxyTargetGroup =
  res
    "ModifyDBProxyTargetGroupResponse"
    "fixture/ModifyDBProxyTargetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyDBProxyTargetGroup)

responseModifyDBSnapshot :: ModifyDBSnapshotResponse -> TestTree
responseModifyDBSnapshot =
  res
    "ModifyDBSnapshotResponse"
    "fixture/ModifyDBSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyDBSnapshot)

responseModifyDBSnapshotAttribute :: ModifyDBSnapshotAttributeResponse -> TestTree
responseModifyDBSnapshotAttribute =
  res
    "ModifyDBSnapshotAttributeResponse"
    "fixture/ModifyDBSnapshotAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyDBSnapshotAttribute)

responseModifyDBSubnetGroup :: ModifyDBSubnetGroupResponse -> TestTree
responseModifyDBSubnetGroup =
  res
    "ModifyDBSubnetGroupResponse"
    "fixture/ModifyDBSubnetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyDBSubnetGroup)

responseModifyEventSubscription :: ModifyEventSubscriptionResponse -> TestTree
responseModifyEventSubscription =
  res
    "ModifyEventSubscriptionResponse"
    "fixture/ModifyEventSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyEventSubscription)

responseModifyGlobalCluster :: ModifyGlobalClusterResponse -> TestTree
responseModifyGlobalCluster =
  res
    "ModifyGlobalClusterResponse"
    "fixture/ModifyGlobalClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyGlobalCluster)

responseModifyOptionGroup :: ModifyOptionGroupResponse -> TestTree
responseModifyOptionGroup =
  res
    "ModifyOptionGroupResponse"
    "fixture/ModifyOptionGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyOptionGroup)

responsePromoteReadReplica :: PromoteReadReplicaResponse -> TestTree
responsePromoteReadReplica =
  res
    "PromoteReadReplicaResponse"
    "fixture/PromoteReadReplicaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PromoteReadReplica)

responsePromoteReadReplicaDBCluster :: PromoteReadReplicaDBClusterResponse -> TestTree
responsePromoteReadReplicaDBCluster =
  res
    "PromoteReadReplicaDBClusterResponse"
    "fixture/PromoteReadReplicaDBClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PromoteReadReplicaDBCluster)

responsePurchaseReservedDBInstancesOffering :: PurchaseReservedDBInstancesOfferingResponse -> TestTree
responsePurchaseReservedDBInstancesOffering =
  res
    "PurchaseReservedDBInstancesOfferingResponse"
    "fixture/PurchaseReservedDBInstancesOfferingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PurchaseReservedDBInstancesOffering)

responseRebootDBCluster :: RebootDBClusterResponse -> TestTree
responseRebootDBCluster =
  res
    "RebootDBClusterResponse"
    "fixture/RebootDBClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RebootDBCluster)

responseRebootDBInstance :: RebootDBInstanceResponse -> TestTree
responseRebootDBInstance =
  res
    "RebootDBInstanceResponse"
    "fixture/RebootDBInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RebootDBInstance)

responseRegisterDBProxyTargets :: RegisterDBProxyTargetsResponse -> TestTree
responseRegisterDBProxyTargets =
  res
    "RegisterDBProxyTargetsResponse"
    "fixture/RegisterDBProxyTargetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterDBProxyTargets)

responseRemoveFromGlobalCluster :: RemoveFromGlobalClusterResponse -> TestTree
responseRemoveFromGlobalCluster =
  res
    "RemoveFromGlobalClusterResponse"
    "fixture/RemoveFromGlobalClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveFromGlobalCluster)

responseRemoveRoleFromDBCluster :: RemoveRoleFromDBClusterResponse -> TestTree
responseRemoveRoleFromDBCluster =
  res
    "RemoveRoleFromDBClusterResponse"
    "fixture/RemoveRoleFromDBClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveRoleFromDBCluster)

responseRemoveRoleFromDBInstance :: RemoveRoleFromDBInstanceResponse -> TestTree
responseRemoveRoleFromDBInstance =
  res
    "RemoveRoleFromDBInstanceResponse"
    "fixture/RemoveRoleFromDBInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveRoleFromDBInstance)

responseRemoveSourceIdentifierFromSubscription :: RemoveSourceIdentifierFromSubscriptionResponse -> TestTree
responseRemoveSourceIdentifierFromSubscription =
  res
    "RemoveSourceIdentifierFromSubscriptionResponse"
    "fixture/RemoveSourceIdentifierFromSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveSourceIdentifierFromSubscription)

responseRemoveTagsFromResource :: RemoveTagsFromResourceResponse -> TestTree
responseRemoveTagsFromResource =
  res
    "RemoveTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveTagsFromResource)

responseResetDBClusterParameterGroup :: DBClusterParameterGroupNameMessage -> TestTree
responseResetDBClusterParameterGroup =
  res
    "ResetDBClusterParameterGroupResponse"
    "fixture/ResetDBClusterParameterGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResetDBClusterParameterGroup)

responseResetDBParameterGroup :: DBParameterGroupNameMessage -> TestTree
responseResetDBParameterGroup =
  res
    "ResetDBParameterGroupResponse"
    "fixture/ResetDBParameterGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResetDBParameterGroup)

responseRestoreDBClusterFromS3 :: RestoreDBClusterFromS3Response -> TestTree
responseRestoreDBClusterFromS3 =
  res
    "RestoreDBClusterFromS3Response"
    "fixture/RestoreDBClusterFromS3Response.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestoreDBClusterFromS3)

responseRestoreDBClusterFromSnapshot :: RestoreDBClusterFromSnapshotResponse -> TestTree
responseRestoreDBClusterFromSnapshot =
  res
    "RestoreDBClusterFromSnapshotResponse"
    "fixture/RestoreDBClusterFromSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestoreDBClusterFromSnapshot)

responseRestoreDBClusterToPointInTime :: RestoreDBClusterToPointInTimeResponse -> TestTree
responseRestoreDBClusterToPointInTime =
  res
    "RestoreDBClusterToPointInTimeResponse"
    "fixture/RestoreDBClusterToPointInTimeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestoreDBClusterToPointInTime)

responseRestoreDBInstanceFromDBSnapshot :: RestoreDBInstanceFromDBSnapshotResponse -> TestTree
responseRestoreDBInstanceFromDBSnapshot =
  res
    "RestoreDBInstanceFromDBSnapshotResponse"
    "fixture/RestoreDBInstanceFromDBSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestoreDBInstanceFromDBSnapshot)

responseRestoreDBInstanceFromS3 :: RestoreDBInstanceFromS3Response -> TestTree
responseRestoreDBInstanceFromS3 =
  res
    "RestoreDBInstanceFromS3Response"
    "fixture/RestoreDBInstanceFromS3Response.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestoreDBInstanceFromS3)

responseRestoreDBInstanceToPointInTime :: RestoreDBInstanceToPointInTimeResponse -> TestTree
responseRestoreDBInstanceToPointInTime =
  res
    "RestoreDBInstanceToPointInTimeResponse"
    "fixture/RestoreDBInstanceToPointInTimeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestoreDBInstanceToPointInTime)

responseRevokeDBSecurityGroupIngress :: RevokeDBSecurityGroupIngressResponse -> TestTree
responseRevokeDBSecurityGroupIngress =
  res
    "RevokeDBSecurityGroupIngressResponse"
    "fixture/RevokeDBSecurityGroupIngressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RevokeDBSecurityGroupIngress)

responseStartActivityStream :: StartActivityStreamResponse -> TestTree
responseStartActivityStream =
  res
    "StartActivityStreamResponse"
    "fixture/StartActivityStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartActivityStream)

responseStartDBCluster :: StartDBClusterResponse -> TestTree
responseStartDBCluster =
  res
    "StartDBClusterResponse"
    "fixture/StartDBClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartDBCluster)

responseStartDBInstance :: StartDBInstanceResponse -> TestTree
responseStartDBInstance =
  res
    "StartDBInstanceResponse"
    "fixture/StartDBInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartDBInstance)

responseStartDBInstanceAutomatedBackupsReplication :: StartDBInstanceAutomatedBackupsReplicationResponse -> TestTree
responseStartDBInstanceAutomatedBackupsReplication =
  res
    "StartDBInstanceAutomatedBackupsReplicationResponse"
    "fixture/StartDBInstanceAutomatedBackupsReplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartDBInstanceAutomatedBackupsReplication)

responseStartExportTask :: ExportTask -> TestTree
responseStartExportTask =
  res
    "StartExportTaskResponse"
    "fixture/StartExportTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartExportTask)

responseStopActivityStream :: StopActivityStreamResponse -> TestTree
responseStopActivityStream =
  res
    "StopActivityStreamResponse"
    "fixture/StopActivityStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopActivityStream)

responseStopDBCluster :: StopDBClusterResponse -> TestTree
responseStopDBCluster =
  res
    "StopDBClusterResponse"
    "fixture/StopDBClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopDBCluster)

responseStopDBInstance :: StopDBInstanceResponse -> TestTree
responseStopDBInstance =
  res
    "StopDBInstanceResponse"
    "fixture/StopDBInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopDBInstance)

responseStopDBInstanceAutomatedBackupsReplication :: StopDBInstanceAutomatedBackupsReplicationResponse -> TestTree
responseStopDBInstanceAutomatedBackupsReplication =
  res
    "StopDBInstanceAutomatedBackupsReplicationResponse"
    "fixture/StopDBInstanceAutomatedBackupsReplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopDBInstanceAutomatedBackupsReplication)

responseSwitchoverBlueGreenDeployment :: SwitchoverBlueGreenDeploymentResponse -> TestTree
responseSwitchoverBlueGreenDeployment =
  res
    "SwitchoverBlueGreenDeploymentResponse"
    "fixture/SwitchoverBlueGreenDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SwitchoverBlueGreenDeployment)

responseSwitchoverReadReplica :: SwitchoverReadReplicaResponse -> TestTree
responseSwitchoverReadReplica =
  res
    "SwitchoverReadReplicaResponse"
    "fixture/SwitchoverReadReplicaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SwitchoverReadReplica)
