{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.RDS
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.RDS where

import Data.Proxy
import Network.AWS.RDS
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.RDS.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestResetDBClusterParameterGroup $
--             newResetDBClusterParameterGroup
--
--         , requestStopDBInstance $
--             newStopDBInstance
--
--         , requestModifyDBInstance $
--             newModifyDBInstance
--
--         , requestDescribeEventCategories $
--             newDescribeEventCategories
--
--         , requestStartDBInstance $
--             newStartDBInstance
--
--         , requestCopyDBClusterParameterGroup $
--             newCopyDBClusterParameterGroup
--
--         , requestDescribeExportTasks $
--             newDescribeExportTasks
--
--         , requestCopyDBSnapshot $
--             newCopyDBSnapshot
--
--         , requestPromoteReadReplica $
--             newPromoteReadReplica
--
--         , requestDescribeDBSnapshots $
--             newDescribeDBSnapshots
--
--         , requestModifyDBCluster $
--             newModifyDBCluster
--
--         , requestModifyOptionGroup $
--             newModifyOptionGroup
--
--         , requestStopDBCluster $
--             newStopDBCluster
--
--         , requestCreateDBClusterSnapshot $
--             newCreateDBClusterSnapshot
--
--         , requestDescribeDBClusterParameterGroups $
--             newDescribeDBClusterParameterGroups
--
--         , requestDescribeDBEngineVersions $
--             newDescribeDBEngineVersions
--
--         , requestStartDBCluster $
--             newStartDBCluster
--
--         , requestDescribeOptionGroupOptions $
--             newDescribeOptionGroupOptions
--
--         , requestDeleteInstallationMedia $
--             newDeleteInstallationMedia
--
--         , requestCopyOptionGroup $
--             newCopyOptionGroup
--
--         , requestDescribeDBInstanceAutomatedBackups $
--             newDescribeDBInstanceAutomatedBackups
--
--         , requestModifyGlobalCluster $
--             newModifyGlobalCluster
--
--         , requestDescribeDBProxies $
--             newDescribeDBProxies
--
--         , requestRestoreDBClusterToPointInTime $
--             newRestoreDBClusterToPointInTime
--
--         , requestStopActivityStream $
--             newStopActivityStream
--
--         , requestCreateDBInstanceReadReplica $
--             newCreateDBInstanceReadReplica
--
--         , requestDescribeValidDBInstanceModifications $
--             newDescribeValidDBInstanceModifications
--
--         , requestStartExportTask $
--             newStartExportTask
--
--         , requestDescribeDBParameters $
--             newDescribeDBParameters
--
--         , requestDescribeDBInstances $
--             newDescribeDBInstances
--
--         , requestDescribeEventSubscriptions $
--             newDescribeEventSubscriptions
--
--         , requestRestoreDBInstanceFromS $
--             newRestoreDBInstanceFromS
--
--         , requestStartActivityStream $
--             newStartActivityStream
--
--         , requestDescribeDBClusterEndpoints $
--             newDescribeDBClusterEndpoints
--
--         , requestModifyCertificates $
--             newModifyCertificates
--
--         , requestDescribeDBClusterSnapshots $
--             newDescribeDBClusterSnapshots
--
--         , requestDeleteDBClusterEndpoint $
--             newDeleteDBClusterEndpoint
--
--         , requestModifyDBSnapshotAttribute $
--             newModifyDBSnapshotAttribute
--
--         , requestDescribeReservedDBInstancesOfferings $
--             newDescribeReservedDBInstancesOfferings
--
--         , requestDeleteDBInstance $
--             newDeleteDBInstance
--
--         , requestRebootDBInstance $
--             newRebootDBInstance
--
--         , requestDeleteEventSubscription $
--             newDeleteEventSubscription
--
--         , requestDescribeDBParameterGroups $
--             newDescribeDBParameterGroups
--
--         , requestDeleteDBInstanceAutomatedBackup $
--             newDeleteDBInstanceAutomatedBackup
--
--         , requestDescribeDBClusterBacktracks $
--             newDescribeDBClusterBacktracks
--
--         , requestCreateDBClusterParameterGroup $
--             newCreateDBClusterParameterGroup
--
--         , requestCreateDBProxy $
--             newCreateDBProxy
--
--         , requestDescribeDBClusterParameters $
--             newDescribeDBClusterParameters
--
--         , requestDeleteDBSubnetGroup $
--             newDeleteDBSubnetGroup
--
--         , requestDescribeAccountAttributes $
--             newDescribeAccountAttributes
--
--         , requestRemoveTagsFromResource $
--             newRemoveTagsFromResource
--
--         , requestCreateDBSecurityGroup $
--             newCreateDBSecurityGroup
--
--         , requestDescribeCertificates $
--             newDescribeCertificates
--
--         , requestCreateDBInstance $
--             newCreateDBInstance
--
--         , requestRemoveRoleFromDBInstance $
--             newRemoveRoleFromDBInstance
--
--         , requestDescribeEngineDefaultClusterParameters $
--             newDescribeEngineDefaultClusterParameters
--
--         , requestStartDBInstanceAutomatedBackupsReplication $
--             newStartDBInstanceAutomatedBackupsReplication
--
--         , requestCreateOptionGroup $
--             newCreateOptionGroup
--
--         , requestCreateCustomAvailabilityZone $
--             newCreateCustomAvailabilityZone
--
--         , requestStopDBInstanceAutomatedBackupsReplication $
--             newStopDBInstanceAutomatedBackupsReplication
--
--         , requestFailoverDBCluster $
--             newFailoverDBCluster
--
--         , requestRemoveRoleFromDBCluster $
--             newRemoveRoleFromDBCluster
--
--         , requestDescribeDBSnapshotAttributes $
--             newDescribeDBSnapshotAttributes
--
--         , requestCreateDBCluster $
--             newCreateDBCluster
--
--         , requestApplyPendingMaintenanceAction $
--             newApplyPendingMaintenanceAction
--
--         , requestBacktrackDBCluster $
--             newBacktrackDBCluster
--
--         , requestDescribeReservedDBInstances $
--             newDescribeReservedDBInstances
--
--         , requestDeleteDBCluster $
--             newDeleteDBCluster
--
--         , requestDeleteOptionGroup $
--             newDeleteOptionGroup
--
--         , requestDescribePendingMaintenanceActions $
--             newDescribePendingMaintenanceActions
--
--         , requestDescribeDBProxyTargetGroups $
--             newDescribeDBProxyTargetGroups
--
--         , requestModifyDBClusterParameterGroup $
--             newModifyDBClusterParameterGroup
--
--         , requestDescribeDBClusterSnapshotAttributes $
--             newDescribeDBClusterSnapshotAttributes
--
--         , requestDescribeOptionGroups $
--             newDescribeOptionGroups
--
--         , requestDescribeDBClusters $
--             newDescribeDBClusters
--
--         , requestDescribeEvents $
--             newDescribeEvents
--
--         , requestModifyDBProxy $
--             newModifyDBProxy
--
--         , requestDescribeDBLogFiles $
--             newDescribeDBLogFiles
--
--         , requestRestoreDBClusterFromS $
--             newRestoreDBClusterFromS
--
--         , requestCancelExportTask $
--             newCancelExportTask
--
--         , requestDescribeGlobalClusters $
--             newDescribeGlobalClusters
--
--         , requestAddSourceIdentifierToSubscription $
--             newAddSourceIdentifierToSubscription
--
--         , requestModifyEventSubscription $
--             newModifyEventSubscription
--
--         , requestModifyDBClusterEndpoint $
--             newModifyDBClusterEndpoint
--
--         , requestModifyDBClusterSnapshotAttribute $
--             newModifyDBClusterSnapshotAttribute
--
--         , requestDescribeDBSubnetGroups $
--             newDescribeDBSubnetGroups
--
--         , requestModifyDBProxyTargetGroup $
--             newModifyDBProxyTargetGroup
--
--         , requestCreateDBParameterGroup $
--             newCreateDBParameterGroup
--
--         , requestDeleteDBClusterSnapshot $
--             newDeleteDBClusterSnapshot
--
--         , requestRemoveFromGlobalCluster $
--             newRemoveFromGlobalCluster
--
--         , requestDescribeDBSecurityGroups $
--             newDescribeDBSecurityGroups
--
--         , requestAddTagsToResource $
--             newAddTagsToResource
--
--         , requestRegisterDBProxyTargets $
--             newRegisterDBProxyTargets
--
--         , requestDownloadDBLogFilePortion $
--             newDownloadDBLogFilePortion
--
--         , requestModifyCurrentDBClusterCapacity $
--             newModifyCurrentDBClusterCapacity
--
--         , requestAddRoleToDBInstance $
--             newAddRoleToDBInstance
--
--         , requestDeleteDBParameterGroup $
--             newDeleteDBParameterGroup
--
--         , requestCreateDBSnapshot $
--             newCreateDBSnapshot
--
--         , requestCreateDBSubnetGroup $
--             newCreateDBSubnetGroup
--
--         , requestDescribeOrderableDBInstanceOptions $
--             newDescribeOrderableDBInstanceOptions
--
--         , requestDeleteDBSecurityGroup $
--             newDeleteDBSecurityGroup
--
--         , requestRestoreDBClusterFromSnapshot $
--             newRestoreDBClusterFromSnapshot
--
--         , requestRestoreDBInstanceFromDBSnapshot $
--             newRestoreDBInstanceFromDBSnapshot
--
--         , requestCreateDBClusterEndpoint $
--             newCreateDBClusterEndpoint
--
--         , requestCreateEventSubscription $
--             newCreateEventSubscription
--
--         , requestDeleteDBSnapshot $
--             newDeleteDBSnapshot
--
--         , requestDeleteDBClusterParameterGroup $
--             newDeleteDBClusterParameterGroup
--
--         , requestDescribeSourceRegions $
--             newDescribeSourceRegions
--
--         , requestPurchaseReservedDBInstancesOffering $
--             newPurchaseReservedDBInstancesOffering
--
--         , requestAuthorizeDBSecurityGroupIngress $
--             newAuthorizeDBSecurityGroupIngress
--
--         , requestDeleteDBProxy $
--             newDeleteDBProxy
--
--         , requestRevokeDBSecurityGroupIngress $
--             newRevokeDBSecurityGroupIngress
--
--         , requestModifyDBParameterGroup $
--             newModifyDBParameterGroup
--
--         , requestDeregisterDBProxyTargets $
--             newDeregisterDBProxyTargets
--
--         , requestDeleteGlobalCluster $
--             newDeleteGlobalCluster
--
--         , requestPromoteReadReplicaDBCluster $
--             newPromoteReadReplicaDBCluster
--
--         , requestCreateGlobalCluster $
--             newCreateGlobalCluster
--
--         , requestResetDBParameterGroup $
--             newResetDBParameterGroup
--
--         , requestDescribeDBProxyTargets $
--             newDescribeDBProxyTargets
--
--         , requestImportInstallationMedia $
--             newImportInstallationMedia
--
--         , requestDeleteCustomAvailabilityZone $
--             newDeleteCustomAvailabilityZone
--
--         , requestFailoverGlobalCluster $
--             newFailoverGlobalCluster
--
--         , requestRemoveSourceIdentifierFromSubscription $
--             newRemoveSourceIdentifierFromSubscription
--
--         , requestCopyDBClusterSnapshot $
--             newCopyDBClusterSnapshot
--
--         , requestDescribeInstallationMedia $
--             newDescribeInstallationMedia
--
--         , requestCopyDBParameterGroup $
--             newCopyDBParameterGroup
--
--         , requestRestoreDBInstanceToPointInTime $
--             newRestoreDBInstanceToPointInTime
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestModifyDBSnapshot $
--             newModifyDBSnapshot
--
--         , requestModifyDBSubnetGroup $
--             newModifyDBSubnetGroup
--
--         , requestDescribeEngineDefaultParameters $
--             newDescribeEngineDefaultParameters
--
--         , requestDescribeCustomAvailabilityZones $
--             newDescribeCustomAvailabilityZones
--
--         , requestAddRoleToDBCluster $
--             newAddRoleToDBCluster
--
--           ]

--     , testGroup "response"
--         [ responseResetDBClusterParameterGroup $
--             newDBClusterParameterGroupNameMessage
--
--         , responseStopDBInstance $
--             newStopDBInstanceResponse
--
--         , responseModifyDBInstance $
--             newModifyDBInstanceResponse
--
--         , responseDescribeEventCategories $
--             newDescribeEventCategoriesResponse
--
--         , responseStartDBInstance $
--             newStartDBInstanceResponse
--
--         , responseCopyDBClusterParameterGroup $
--             newCopyDBClusterParameterGroupResponse
--
--         , responseDescribeExportTasks $
--             newDescribeExportTasksResponse
--
--         , responseCopyDBSnapshot $
--             newCopyDBSnapshotResponse
--
--         , responsePromoteReadReplica $
--             newPromoteReadReplicaResponse
--
--         , responseDescribeDBSnapshots $
--             newDescribeDBSnapshotsResponse
--
--         , responseModifyDBCluster $
--             newModifyDBClusterResponse
--
--         , responseModifyOptionGroup $
--             newModifyOptionGroupResponse
--
--         , responseStopDBCluster $
--             newStopDBClusterResponse
--
--         , responseCreateDBClusterSnapshot $
--             newCreateDBClusterSnapshotResponse
--
--         , responseDescribeDBClusterParameterGroups $
--             newDescribeDBClusterParameterGroupsResponse
--
--         , responseDescribeDBEngineVersions $
--             newDescribeDBEngineVersionsResponse
--
--         , responseStartDBCluster $
--             newStartDBClusterResponse
--
--         , responseDescribeOptionGroupOptions $
--             newDescribeOptionGroupOptionsResponse
--
--         , responseDeleteInstallationMedia $
--             newInstallationMedia
--
--         , responseCopyOptionGroup $
--             newCopyOptionGroupResponse
--
--         , responseDescribeDBInstanceAutomatedBackups $
--             newDescribeDBInstanceAutomatedBackupsResponse
--
--         , responseModifyGlobalCluster $
--             newModifyGlobalClusterResponse
--
--         , responseDescribeDBProxies $
--             newDescribeDBProxiesResponse
--
--         , responseRestoreDBClusterToPointInTime $
--             newRestoreDBClusterToPointInTimeResponse
--
--         , responseStopActivityStream $
--             newStopActivityStreamResponse
--
--         , responseCreateDBInstanceReadReplica $
--             newCreateDBInstanceReadReplicaResponse
--
--         , responseDescribeValidDBInstanceModifications $
--             newDescribeValidDBInstanceModificationsResponse
--
--         , responseStartExportTask $
--             newExportTask
--
--         , responseDescribeDBParameters $
--             newDescribeDBParametersResponse
--
--         , responseDescribeDBInstances $
--             newDescribeDBInstancesResponse
--
--         , responseDescribeEventSubscriptions $
--             newDescribeEventSubscriptionsResponse
--
--         , responseRestoreDBInstanceFromS $
--             newRestoreDBInstanceFromSResponse
--
--         , responseStartActivityStream $
--             newStartActivityStreamResponse
--
--         , responseDescribeDBClusterEndpoints $
--             newDescribeDBClusterEndpointsResponse
--
--         , responseModifyCertificates $
--             newModifyCertificatesResponse
--
--         , responseDescribeDBClusterSnapshots $
--             newDescribeDBClusterSnapshotsResponse
--
--         , responseDeleteDBClusterEndpoint $
--             newDBClusterEndpoint
--
--         , responseModifyDBSnapshotAttribute $
--             newModifyDBSnapshotAttributeResponse
--
--         , responseDescribeReservedDBInstancesOfferings $
--             newDescribeReservedDBInstancesOfferingsResponse
--
--         , responseDeleteDBInstance $
--             newDeleteDBInstanceResponse
--
--         , responseRebootDBInstance $
--             newRebootDBInstanceResponse
--
--         , responseDeleteEventSubscription $
--             newDeleteEventSubscriptionResponse
--
--         , responseDescribeDBParameterGroups $
--             newDescribeDBParameterGroupsResponse
--
--         , responseDeleteDBInstanceAutomatedBackup $
--             newDeleteDBInstanceAutomatedBackupResponse
--
--         , responseDescribeDBClusterBacktracks $
--             newDescribeDBClusterBacktracksResponse
--
--         , responseCreateDBClusterParameterGroup $
--             newCreateDBClusterParameterGroupResponse
--
--         , responseCreateDBProxy $
--             newCreateDBProxyResponse
--
--         , responseDescribeDBClusterParameters $
--             newDescribeDBClusterParametersResponse
--
--         , responseDeleteDBSubnetGroup $
--             newDeleteDBSubnetGroupResponse
--
--         , responseDescribeAccountAttributes $
--             newDescribeAccountAttributesResponse
--
--         , responseRemoveTagsFromResource $
--             newRemoveTagsFromResourceResponse
--
--         , responseCreateDBSecurityGroup $
--             newCreateDBSecurityGroupResponse
--
--         , responseDescribeCertificates $
--             newDescribeCertificatesResponse
--
--         , responseCreateDBInstance $
--             newCreateDBInstanceResponse
--
--         , responseRemoveRoleFromDBInstance $
--             newRemoveRoleFromDBInstanceResponse
--
--         , responseDescribeEngineDefaultClusterParameters $
--             newDescribeEngineDefaultClusterParametersResponse
--
--         , responseStartDBInstanceAutomatedBackupsReplication $
--             newStartDBInstanceAutomatedBackupsReplicationResponse
--
--         , responseCreateOptionGroup $
--             newCreateOptionGroupResponse
--
--         , responseCreateCustomAvailabilityZone $
--             newCreateCustomAvailabilityZoneResponse
--
--         , responseStopDBInstanceAutomatedBackupsReplication $
--             newStopDBInstanceAutomatedBackupsReplicationResponse
--
--         , responseFailoverDBCluster $
--             newFailoverDBClusterResponse
--
--         , responseRemoveRoleFromDBCluster $
--             newRemoveRoleFromDBClusterResponse
--
--         , responseDescribeDBSnapshotAttributes $
--             newDescribeDBSnapshotAttributesResponse
--
--         , responseCreateDBCluster $
--             newCreateDBClusterResponse
--
--         , responseApplyPendingMaintenanceAction $
--             newApplyPendingMaintenanceActionResponse
--
--         , responseBacktrackDBCluster $
--             newDBClusterBacktrack
--
--         , responseDescribeReservedDBInstances $
--             newDescribeReservedDBInstancesResponse
--
--         , responseDeleteDBCluster $
--             newDeleteDBClusterResponse
--
--         , responseDeleteOptionGroup $
--             newDeleteOptionGroupResponse
--
--         , responseDescribePendingMaintenanceActions $
--             newDescribePendingMaintenanceActionsResponse
--
--         , responseDescribeDBProxyTargetGroups $
--             newDescribeDBProxyTargetGroupsResponse
--
--         , responseModifyDBClusterParameterGroup $
--             newDBClusterParameterGroupNameMessage
--
--         , responseDescribeDBClusterSnapshotAttributes $
--             newDescribeDBClusterSnapshotAttributesResponse
--
--         , responseDescribeOptionGroups $
--             newDescribeOptionGroupsResponse
--
--         , responseDescribeDBClusters $
--             newDescribeDBClustersResponse
--
--         , responseDescribeEvents $
--             newDescribeEventsResponse
--
--         , responseModifyDBProxy $
--             newModifyDBProxyResponse
--
--         , responseDescribeDBLogFiles $
--             newDescribeDBLogFilesResponse
--
--         , responseRestoreDBClusterFromS $
--             newRestoreDBClusterFromSResponse
--
--         , responseCancelExportTask $
--             newExportTask
--
--         , responseDescribeGlobalClusters $
--             newDescribeGlobalClustersResponse
--
--         , responseAddSourceIdentifierToSubscription $
--             newAddSourceIdentifierToSubscriptionResponse
--
--         , responseModifyEventSubscription $
--             newModifyEventSubscriptionResponse
--
--         , responseModifyDBClusterEndpoint $
--             newDBClusterEndpoint
--
--         , responseModifyDBClusterSnapshotAttribute $
--             newModifyDBClusterSnapshotAttributeResponse
--
--         , responseDescribeDBSubnetGroups $
--             newDescribeDBSubnetGroupsResponse
--
--         , responseModifyDBProxyTargetGroup $
--             newModifyDBProxyTargetGroupResponse
--
--         , responseCreateDBParameterGroup $
--             newCreateDBParameterGroupResponse
--
--         , responseDeleteDBClusterSnapshot $
--             newDeleteDBClusterSnapshotResponse
--
--         , responseRemoveFromGlobalCluster $
--             newRemoveFromGlobalClusterResponse
--
--         , responseDescribeDBSecurityGroups $
--             newDescribeDBSecurityGroupsResponse
--
--         , responseAddTagsToResource $
--             newAddTagsToResourceResponse
--
--         , responseRegisterDBProxyTargets $
--             newRegisterDBProxyTargetsResponse
--
--         , responseDownloadDBLogFilePortion $
--             newDownloadDBLogFilePortionResponse
--
--         , responseModifyCurrentDBClusterCapacity $
--             newModifyCurrentDBClusterCapacityResponse
--
--         , responseAddRoleToDBInstance $
--             newAddRoleToDBInstanceResponse
--
--         , responseDeleteDBParameterGroup $
--             newDeleteDBParameterGroupResponse
--
--         , responseCreateDBSnapshot $
--             newCreateDBSnapshotResponse
--
--         , responseCreateDBSubnetGroup $
--             newCreateDBSubnetGroupResponse
--
--         , responseDescribeOrderableDBInstanceOptions $
--             newDescribeOrderableDBInstanceOptionsResponse
--
--         , responseDeleteDBSecurityGroup $
--             newDeleteDBSecurityGroupResponse
--
--         , responseRestoreDBClusterFromSnapshot $
--             newRestoreDBClusterFromSnapshotResponse
--
--         , responseRestoreDBInstanceFromDBSnapshot $
--             newRestoreDBInstanceFromDBSnapshotResponse
--
--         , responseCreateDBClusterEndpoint $
--             newDBClusterEndpoint
--
--         , responseCreateEventSubscription $
--             newCreateEventSubscriptionResponse
--
--         , responseDeleteDBSnapshot $
--             newDeleteDBSnapshotResponse
--
--         , responseDeleteDBClusterParameterGroup $
--             newDeleteDBClusterParameterGroupResponse
--
--         , responseDescribeSourceRegions $
--             newDescribeSourceRegionsResponse
--
--         , responsePurchaseReservedDBInstancesOffering $
--             newPurchaseReservedDBInstancesOfferingResponse
--
--         , responseAuthorizeDBSecurityGroupIngress $
--             newAuthorizeDBSecurityGroupIngressResponse
--
--         , responseDeleteDBProxy $
--             newDeleteDBProxyResponse
--
--         , responseRevokeDBSecurityGroupIngress $
--             newRevokeDBSecurityGroupIngressResponse
--
--         , responseModifyDBParameterGroup $
--             newDBParameterGroupNameMessage
--
--         , responseDeregisterDBProxyTargets $
--             newDeregisterDBProxyTargetsResponse
--
--         , responseDeleteGlobalCluster $
--             newDeleteGlobalClusterResponse
--
--         , responsePromoteReadReplicaDBCluster $
--             newPromoteReadReplicaDBClusterResponse
--
--         , responseCreateGlobalCluster $
--             newCreateGlobalClusterResponse
--
--         , responseResetDBParameterGroup $
--             newDBParameterGroupNameMessage
--
--         , responseDescribeDBProxyTargets $
--             newDescribeDBProxyTargetsResponse
--
--         , responseImportInstallationMedia $
--             newInstallationMedia
--
--         , responseDeleteCustomAvailabilityZone $
--             newDeleteCustomAvailabilityZoneResponse
--
--         , responseFailoverGlobalCluster $
--             newFailoverGlobalClusterResponse
--
--         , responseRemoveSourceIdentifierFromSubscription $
--             newRemoveSourceIdentifierFromSubscriptionResponse
--
--         , responseCopyDBClusterSnapshot $
--             newCopyDBClusterSnapshotResponse
--
--         , responseDescribeInstallationMedia $
--             newDescribeInstallationMediaResponse
--
--         , responseCopyDBParameterGroup $
--             newCopyDBParameterGroupResponse
--
--         , responseRestoreDBInstanceToPointInTime $
--             newRestoreDBInstanceToPointInTimeResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseModifyDBSnapshot $
--             newModifyDBSnapshotResponse
--
--         , responseModifyDBSubnetGroup $
--             newModifyDBSubnetGroupResponse
--
--         , responseDescribeEngineDefaultParameters $
--             newDescribeEngineDefaultParametersResponse
--
--         , responseDescribeCustomAvailabilityZones $
--             newDescribeCustomAvailabilityZonesResponse
--
--         , responseAddRoleToDBCluster $
--             newAddRoleToDBClusterResponse
--
--           ]
--     ]

-- Requests

requestResetDBClusterParameterGroup :: ResetDBClusterParameterGroup -> TestTree
requestResetDBClusterParameterGroup =
  req
    "ResetDBClusterParameterGroup"
    "fixture/ResetDBClusterParameterGroup.yaml"

requestStopDBInstance :: StopDBInstance -> TestTree
requestStopDBInstance =
  req
    "StopDBInstance"
    "fixture/StopDBInstance.yaml"

requestModifyDBInstance :: ModifyDBInstance -> TestTree
requestModifyDBInstance =
  req
    "ModifyDBInstance"
    "fixture/ModifyDBInstance.yaml"

requestDescribeEventCategories :: DescribeEventCategories -> TestTree
requestDescribeEventCategories =
  req
    "DescribeEventCategories"
    "fixture/DescribeEventCategories.yaml"

requestStartDBInstance :: StartDBInstance -> TestTree
requestStartDBInstance =
  req
    "StartDBInstance"
    "fixture/StartDBInstance.yaml"

requestCopyDBClusterParameterGroup :: CopyDBClusterParameterGroup -> TestTree
requestCopyDBClusterParameterGroup =
  req
    "CopyDBClusterParameterGroup"
    "fixture/CopyDBClusterParameterGroup.yaml"

requestDescribeExportTasks :: DescribeExportTasks -> TestTree
requestDescribeExportTasks =
  req
    "DescribeExportTasks"
    "fixture/DescribeExportTasks.yaml"

requestCopyDBSnapshot :: CopyDBSnapshot -> TestTree
requestCopyDBSnapshot =
  req
    "CopyDBSnapshot"
    "fixture/CopyDBSnapshot.yaml"

requestPromoteReadReplica :: PromoteReadReplica -> TestTree
requestPromoteReadReplica =
  req
    "PromoteReadReplica"
    "fixture/PromoteReadReplica.yaml"

requestDescribeDBSnapshots :: DescribeDBSnapshots -> TestTree
requestDescribeDBSnapshots =
  req
    "DescribeDBSnapshots"
    "fixture/DescribeDBSnapshots.yaml"

requestModifyDBCluster :: ModifyDBCluster -> TestTree
requestModifyDBCluster =
  req
    "ModifyDBCluster"
    "fixture/ModifyDBCluster.yaml"

requestModifyOptionGroup :: ModifyOptionGroup -> TestTree
requestModifyOptionGroup =
  req
    "ModifyOptionGroup"
    "fixture/ModifyOptionGroup.yaml"

requestStopDBCluster :: StopDBCluster -> TestTree
requestStopDBCluster =
  req
    "StopDBCluster"
    "fixture/StopDBCluster.yaml"

requestCreateDBClusterSnapshot :: CreateDBClusterSnapshot -> TestTree
requestCreateDBClusterSnapshot =
  req
    "CreateDBClusterSnapshot"
    "fixture/CreateDBClusterSnapshot.yaml"

requestDescribeDBClusterParameterGroups :: DescribeDBClusterParameterGroups -> TestTree
requestDescribeDBClusterParameterGroups =
  req
    "DescribeDBClusterParameterGroups"
    "fixture/DescribeDBClusterParameterGroups.yaml"

requestDescribeDBEngineVersions :: DescribeDBEngineVersions -> TestTree
requestDescribeDBEngineVersions =
  req
    "DescribeDBEngineVersions"
    "fixture/DescribeDBEngineVersions.yaml"

requestStartDBCluster :: StartDBCluster -> TestTree
requestStartDBCluster =
  req
    "StartDBCluster"
    "fixture/StartDBCluster.yaml"

requestDescribeOptionGroupOptions :: DescribeOptionGroupOptions -> TestTree
requestDescribeOptionGroupOptions =
  req
    "DescribeOptionGroupOptions"
    "fixture/DescribeOptionGroupOptions.yaml"

requestDeleteInstallationMedia :: DeleteInstallationMedia -> TestTree
requestDeleteInstallationMedia =
  req
    "DeleteInstallationMedia"
    "fixture/DeleteInstallationMedia.yaml"

requestCopyOptionGroup :: CopyOptionGroup -> TestTree
requestCopyOptionGroup =
  req
    "CopyOptionGroup"
    "fixture/CopyOptionGroup.yaml"

requestDescribeDBInstanceAutomatedBackups :: DescribeDBInstanceAutomatedBackups -> TestTree
requestDescribeDBInstanceAutomatedBackups =
  req
    "DescribeDBInstanceAutomatedBackups"
    "fixture/DescribeDBInstanceAutomatedBackups.yaml"

requestModifyGlobalCluster :: ModifyGlobalCluster -> TestTree
requestModifyGlobalCluster =
  req
    "ModifyGlobalCluster"
    "fixture/ModifyGlobalCluster.yaml"

requestDescribeDBProxies :: DescribeDBProxies -> TestTree
requestDescribeDBProxies =
  req
    "DescribeDBProxies"
    "fixture/DescribeDBProxies.yaml"

requestRestoreDBClusterToPointInTime :: RestoreDBClusterToPointInTime -> TestTree
requestRestoreDBClusterToPointInTime =
  req
    "RestoreDBClusterToPointInTime"
    "fixture/RestoreDBClusterToPointInTime.yaml"

requestStopActivityStream :: StopActivityStream -> TestTree
requestStopActivityStream =
  req
    "StopActivityStream"
    "fixture/StopActivityStream.yaml"

requestCreateDBInstanceReadReplica :: CreateDBInstanceReadReplica -> TestTree
requestCreateDBInstanceReadReplica =
  req
    "CreateDBInstanceReadReplica"
    "fixture/CreateDBInstanceReadReplica.yaml"

requestDescribeValidDBInstanceModifications :: DescribeValidDBInstanceModifications -> TestTree
requestDescribeValidDBInstanceModifications =
  req
    "DescribeValidDBInstanceModifications"
    "fixture/DescribeValidDBInstanceModifications.yaml"

requestStartExportTask :: StartExportTask -> TestTree
requestStartExportTask =
  req
    "StartExportTask"
    "fixture/StartExportTask.yaml"

requestDescribeDBParameters :: DescribeDBParameters -> TestTree
requestDescribeDBParameters =
  req
    "DescribeDBParameters"
    "fixture/DescribeDBParameters.yaml"

requestDescribeDBInstances :: DescribeDBInstances -> TestTree
requestDescribeDBInstances =
  req
    "DescribeDBInstances"
    "fixture/DescribeDBInstances.yaml"

requestDescribeEventSubscriptions :: DescribeEventSubscriptions -> TestTree
requestDescribeEventSubscriptions =
  req
    "DescribeEventSubscriptions"
    "fixture/DescribeEventSubscriptions.yaml"

requestRestoreDBInstanceFromS :: RestoreDBInstanceFromS -> TestTree
requestRestoreDBInstanceFromS =
  req
    "RestoreDBInstanceFromS"
    "fixture/RestoreDBInstanceFromS.yaml"

requestStartActivityStream :: StartActivityStream -> TestTree
requestStartActivityStream =
  req
    "StartActivityStream"
    "fixture/StartActivityStream.yaml"

requestDescribeDBClusterEndpoints :: DescribeDBClusterEndpoints -> TestTree
requestDescribeDBClusterEndpoints =
  req
    "DescribeDBClusterEndpoints"
    "fixture/DescribeDBClusterEndpoints.yaml"

requestModifyCertificates :: ModifyCertificates -> TestTree
requestModifyCertificates =
  req
    "ModifyCertificates"
    "fixture/ModifyCertificates.yaml"

requestDescribeDBClusterSnapshots :: DescribeDBClusterSnapshots -> TestTree
requestDescribeDBClusterSnapshots =
  req
    "DescribeDBClusterSnapshots"
    "fixture/DescribeDBClusterSnapshots.yaml"

requestDeleteDBClusterEndpoint :: DeleteDBClusterEndpoint -> TestTree
requestDeleteDBClusterEndpoint =
  req
    "DeleteDBClusterEndpoint"
    "fixture/DeleteDBClusterEndpoint.yaml"

requestModifyDBSnapshotAttribute :: ModifyDBSnapshotAttribute -> TestTree
requestModifyDBSnapshotAttribute =
  req
    "ModifyDBSnapshotAttribute"
    "fixture/ModifyDBSnapshotAttribute.yaml"

requestDescribeReservedDBInstancesOfferings :: DescribeReservedDBInstancesOfferings -> TestTree
requestDescribeReservedDBInstancesOfferings =
  req
    "DescribeReservedDBInstancesOfferings"
    "fixture/DescribeReservedDBInstancesOfferings.yaml"

requestDeleteDBInstance :: DeleteDBInstance -> TestTree
requestDeleteDBInstance =
  req
    "DeleteDBInstance"
    "fixture/DeleteDBInstance.yaml"

requestRebootDBInstance :: RebootDBInstance -> TestTree
requestRebootDBInstance =
  req
    "RebootDBInstance"
    "fixture/RebootDBInstance.yaml"

requestDeleteEventSubscription :: DeleteEventSubscription -> TestTree
requestDeleteEventSubscription =
  req
    "DeleteEventSubscription"
    "fixture/DeleteEventSubscription.yaml"

requestDescribeDBParameterGroups :: DescribeDBParameterGroups -> TestTree
requestDescribeDBParameterGroups =
  req
    "DescribeDBParameterGroups"
    "fixture/DescribeDBParameterGroups.yaml"

requestDeleteDBInstanceAutomatedBackup :: DeleteDBInstanceAutomatedBackup -> TestTree
requestDeleteDBInstanceAutomatedBackup =
  req
    "DeleteDBInstanceAutomatedBackup"
    "fixture/DeleteDBInstanceAutomatedBackup.yaml"

requestDescribeDBClusterBacktracks :: DescribeDBClusterBacktracks -> TestTree
requestDescribeDBClusterBacktracks =
  req
    "DescribeDBClusterBacktracks"
    "fixture/DescribeDBClusterBacktracks.yaml"

requestCreateDBClusterParameterGroup :: CreateDBClusterParameterGroup -> TestTree
requestCreateDBClusterParameterGroup =
  req
    "CreateDBClusterParameterGroup"
    "fixture/CreateDBClusterParameterGroup.yaml"

requestCreateDBProxy :: CreateDBProxy -> TestTree
requestCreateDBProxy =
  req
    "CreateDBProxy"
    "fixture/CreateDBProxy.yaml"

requestDescribeDBClusterParameters :: DescribeDBClusterParameters -> TestTree
requestDescribeDBClusterParameters =
  req
    "DescribeDBClusterParameters"
    "fixture/DescribeDBClusterParameters.yaml"

requestDeleteDBSubnetGroup :: DeleteDBSubnetGroup -> TestTree
requestDeleteDBSubnetGroup =
  req
    "DeleteDBSubnetGroup"
    "fixture/DeleteDBSubnetGroup.yaml"

requestDescribeAccountAttributes :: DescribeAccountAttributes -> TestTree
requestDescribeAccountAttributes =
  req
    "DescribeAccountAttributes"
    "fixture/DescribeAccountAttributes.yaml"

requestRemoveTagsFromResource :: RemoveTagsFromResource -> TestTree
requestRemoveTagsFromResource =
  req
    "RemoveTagsFromResource"
    "fixture/RemoveTagsFromResource.yaml"

requestCreateDBSecurityGroup :: CreateDBSecurityGroup -> TestTree
requestCreateDBSecurityGroup =
  req
    "CreateDBSecurityGroup"
    "fixture/CreateDBSecurityGroup.yaml"

requestDescribeCertificates :: DescribeCertificates -> TestTree
requestDescribeCertificates =
  req
    "DescribeCertificates"
    "fixture/DescribeCertificates.yaml"

requestCreateDBInstance :: CreateDBInstance -> TestTree
requestCreateDBInstance =
  req
    "CreateDBInstance"
    "fixture/CreateDBInstance.yaml"

requestRemoveRoleFromDBInstance :: RemoveRoleFromDBInstance -> TestTree
requestRemoveRoleFromDBInstance =
  req
    "RemoveRoleFromDBInstance"
    "fixture/RemoveRoleFromDBInstance.yaml"

requestDescribeEngineDefaultClusterParameters :: DescribeEngineDefaultClusterParameters -> TestTree
requestDescribeEngineDefaultClusterParameters =
  req
    "DescribeEngineDefaultClusterParameters"
    "fixture/DescribeEngineDefaultClusterParameters.yaml"

requestStartDBInstanceAutomatedBackupsReplication :: StartDBInstanceAutomatedBackupsReplication -> TestTree
requestStartDBInstanceAutomatedBackupsReplication =
  req
    "StartDBInstanceAutomatedBackupsReplication"
    "fixture/StartDBInstanceAutomatedBackupsReplication.yaml"

requestCreateOptionGroup :: CreateOptionGroup -> TestTree
requestCreateOptionGroup =
  req
    "CreateOptionGroup"
    "fixture/CreateOptionGroup.yaml"

requestCreateCustomAvailabilityZone :: CreateCustomAvailabilityZone -> TestTree
requestCreateCustomAvailabilityZone =
  req
    "CreateCustomAvailabilityZone"
    "fixture/CreateCustomAvailabilityZone.yaml"

requestStopDBInstanceAutomatedBackupsReplication :: StopDBInstanceAutomatedBackupsReplication -> TestTree
requestStopDBInstanceAutomatedBackupsReplication =
  req
    "StopDBInstanceAutomatedBackupsReplication"
    "fixture/StopDBInstanceAutomatedBackupsReplication.yaml"

requestFailoverDBCluster :: FailoverDBCluster -> TestTree
requestFailoverDBCluster =
  req
    "FailoverDBCluster"
    "fixture/FailoverDBCluster.yaml"

requestRemoveRoleFromDBCluster :: RemoveRoleFromDBCluster -> TestTree
requestRemoveRoleFromDBCluster =
  req
    "RemoveRoleFromDBCluster"
    "fixture/RemoveRoleFromDBCluster.yaml"

requestDescribeDBSnapshotAttributes :: DescribeDBSnapshotAttributes -> TestTree
requestDescribeDBSnapshotAttributes =
  req
    "DescribeDBSnapshotAttributes"
    "fixture/DescribeDBSnapshotAttributes.yaml"

requestCreateDBCluster :: CreateDBCluster -> TestTree
requestCreateDBCluster =
  req
    "CreateDBCluster"
    "fixture/CreateDBCluster.yaml"

requestApplyPendingMaintenanceAction :: ApplyPendingMaintenanceAction -> TestTree
requestApplyPendingMaintenanceAction =
  req
    "ApplyPendingMaintenanceAction"
    "fixture/ApplyPendingMaintenanceAction.yaml"

requestBacktrackDBCluster :: BacktrackDBCluster -> TestTree
requestBacktrackDBCluster =
  req
    "BacktrackDBCluster"
    "fixture/BacktrackDBCluster.yaml"

requestDescribeReservedDBInstances :: DescribeReservedDBInstances -> TestTree
requestDescribeReservedDBInstances =
  req
    "DescribeReservedDBInstances"
    "fixture/DescribeReservedDBInstances.yaml"

requestDeleteDBCluster :: DeleteDBCluster -> TestTree
requestDeleteDBCluster =
  req
    "DeleteDBCluster"
    "fixture/DeleteDBCluster.yaml"

requestDeleteOptionGroup :: DeleteOptionGroup -> TestTree
requestDeleteOptionGroup =
  req
    "DeleteOptionGroup"
    "fixture/DeleteOptionGroup.yaml"

requestDescribePendingMaintenanceActions :: DescribePendingMaintenanceActions -> TestTree
requestDescribePendingMaintenanceActions =
  req
    "DescribePendingMaintenanceActions"
    "fixture/DescribePendingMaintenanceActions.yaml"

requestDescribeDBProxyTargetGroups :: DescribeDBProxyTargetGroups -> TestTree
requestDescribeDBProxyTargetGroups =
  req
    "DescribeDBProxyTargetGroups"
    "fixture/DescribeDBProxyTargetGroups.yaml"

requestModifyDBClusterParameterGroup :: ModifyDBClusterParameterGroup -> TestTree
requestModifyDBClusterParameterGroup =
  req
    "ModifyDBClusterParameterGroup"
    "fixture/ModifyDBClusterParameterGroup.yaml"

requestDescribeDBClusterSnapshotAttributes :: DescribeDBClusterSnapshotAttributes -> TestTree
requestDescribeDBClusterSnapshotAttributes =
  req
    "DescribeDBClusterSnapshotAttributes"
    "fixture/DescribeDBClusterSnapshotAttributes.yaml"

requestDescribeOptionGroups :: DescribeOptionGroups -> TestTree
requestDescribeOptionGroups =
  req
    "DescribeOptionGroups"
    "fixture/DescribeOptionGroups.yaml"

requestDescribeDBClusters :: DescribeDBClusters -> TestTree
requestDescribeDBClusters =
  req
    "DescribeDBClusters"
    "fixture/DescribeDBClusters.yaml"

requestDescribeEvents :: DescribeEvents -> TestTree
requestDescribeEvents =
  req
    "DescribeEvents"
    "fixture/DescribeEvents.yaml"

requestModifyDBProxy :: ModifyDBProxy -> TestTree
requestModifyDBProxy =
  req
    "ModifyDBProxy"
    "fixture/ModifyDBProxy.yaml"

requestDescribeDBLogFiles :: DescribeDBLogFiles -> TestTree
requestDescribeDBLogFiles =
  req
    "DescribeDBLogFiles"
    "fixture/DescribeDBLogFiles.yaml"

requestRestoreDBClusterFromS :: RestoreDBClusterFromS -> TestTree
requestRestoreDBClusterFromS =
  req
    "RestoreDBClusterFromS"
    "fixture/RestoreDBClusterFromS.yaml"

requestCancelExportTask :: CancelExportTask -> TestTree
requestCancelExportTask =
  req
    "CancelExportTask"
    "fixture/CancelExportTask.yaml"

requestDescribeGlobalClusters :: DescribeGlobalClusters -> TestTree
requestDescribeGlobalClusters =
  req
    "DescribeGlobalClusters"
    "fixture/DescribeGlobalClusters.yaml"

requestAddSourceIdentifierToSubscription :: AddSourceIdentifierToSubscription -> TestTree
requestAddSourceIdentifierToSubscription =
  req
    "AddSourceIdentifierToSubscription"
    "fixture/AddSourceIdentifierToSubscription.yaml"

requestModifyEventSubscription :: ModifyEventSubscription -> TestTree
requestModifyEventSubscription =
  req
    "ModifyEventSubscription"
    "fixture/ModifyEventSubscription.yaml"

requestModifyDBClusterEndpoint :: ModifyDBClusterEndpoint -> TestTree
requestModifyDBClusterEndpoint =
  req
    "ModifyDBClusterEndpoint"
    "fixture/ModifyDBClusterEndpoint.yaml"

requestModifyDBClusterSnapshotAttribute :: ModifyDBClusterSnapshotAttribute -> TestTree
requestModifyDBClusterSnapshotAttribute =
  req
    "ModifyDBClusterSnapshotAttribute"
    "fixture/ModifyDBClusterSnapshotAttribute.yaml"

requestDescribeDBSubnetGroups :: DescribeDBSubnetGroups -> TestTree
requestDescribeDBSubnetGroups =
  req
    "DescribeDBSubnetGroups"
    "fixture/DescribeDBSubnetGroups.yaml"

requestModifyDBProxyTargetGroup :: ModifyDBProxyTargetGroup -> TestTree
requestModifyDBProxyTargetGroup =
  req
    "ModifyDBProxyTargetGroup"
    "fixture/ModifyDBProxyTargetGroup.yaml"

requestCreateDBParameterGroup :: CreateDBParameterGroup -> TestTree
requestCreateDBParameterGroup =
  req
    "CreateDBParameterGroup"
    "fixture/CreateDBParameterGroup.yaml"

requestDeleteDBClusterSnapshot :: DeleteDBClusterSnapshot -> TestTree
requestDeleteDBClusterSnapshot =
  req
    "DeleteDBClusterSnapshot"
    "fixture/DeleteDBClusterSnapshot.yaml"

requestRemoveFromGlobalCluster :: RemoveFromGlobalCluster -> TestTree
requestRemoveFromGlobalCluster =
  req
    "RemoveFromGlobalCluster"
    "fixture/RemoveFromGlobalCluster.yaml"

requestDescribeDBSecurityGroups :: DescribeDBSecurityGroups -> TestTree
requestDescribeDBSecurityGroups =
  req
    "DescribeDBSecurityGroups"
    "fixture/DescribeDBSecurityGroups.yaml"

requestAddTagsToResource :: AddTagsToResource -> TestTree
requestAddTagsToResource =
  req
    "AddTagsToResource"
    "fixture/AddTagsToResource.yaml"

requestRegisterDBProxyTargets :: RegisterDBProxyTargets -> TestTree
requestRegisterDBProxyTargets =
  req
    "RegisterDBProxyTargets"
    "fixture/RegisterDBProxyTargets.yaml"

requestDownloadDBLogFilePortion :: DownloadDBLogFilePortion -> TestTree
requestDownloadDBLogFilePortion =
  req
    "DownloadDBLogFilePortion"
    "fixture/DownloadDBLogFilePortion.yaml"

requestModifyCurrentDBClusterCapacity :: ModifyCurrentDBClusterCapacity -> TestTree
requestModifyCurrentDBClusterCapacity =
  req
    "ModifyCurrentDBClusterCapacity"
    "fixture/ModifyCurrentDBClusterCapacity.yaml"

requestAddRoleToDBInstance :: AddRoleToDBInstance -> TestTree
requestAddRoleToDBInstance =
  req
    "AddRoleToDBInstance"
    "fixture/AddRoleToDBInstance.yaml"

requestDeleteDBParameterGroup :: DeleteDBParameterGroup -> TestTree
requestDeleteDBParameterGroup =
  req
    "DeleteDBParameterGroup"
    "fixture/DeleteDBParameterGroup.yaml"

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

requestDescribeOrderableDBInstanceOptions :: DescribeOrderableDBInstanceOptions -> TestTree
requestDescribeOrderableDBInstanceOptions =
  req
    "DescribeOrderableDBInstanceOptions"
    "fixture/DescribeOrderableDBInstanceOptions.yaml"

requestDeleteDBSecurityGroup :: DeleteDBSecurityGroup -> TestTree
requestDeleteDBSecurityGroup =
  req
    "DeleteDBSecurityGroup"
    "fixture/DeleteDBSecurityGroup.yaml"

requestRestoreDBClusterFromSnapshot :: RestoreDBClusterFromSnapshot -> TestTree
requestRestoreDBClusterFromSnapshot =
  req
    "RestoreDBClusterFromSnapshot"
    "fixture/RestoreDBClusterFromSnapshot.yaml"

requestRestoreDBInstanceFromDBSnapshot :: RestoreDBInstanceFromDBSnapshot -> TestTree
requestRestoreDBInstanceFromDBSnapshot =
  req
    "RestoreDBInstanceFromDBSnapshot"
    "fixture/RestoreDBInstanceFromDBSnapshot.yaml"

requestCreateDBClusterEndpoint :: CreateDBClusterEndpoint -> TestTree
requestCreateDBClusterEndpoint =
  req
    "CreateDBClusterEndpoint"
    "fixture/CreateDBClusterEndpoint.yaml"

requestCreateEventSubscription :: CreateEventSubscription -> TestTree
requestCreateEventSubscription =
  req
    "CreateEventSubscription"
    "fixture/CreateEventSubscription.yaml"

requestDeleteDBSnapshot :: DeleteDBSnapshot -> TestTree
requestDeleteDBSnapshot =
  req
    "DeleteDBSnapshot"
    "fixture/DeleteDBSnapshot.yaml"

requestDeleteDBClusterParameterGroup :: DeleteDBClusterParameterGroup -> TestTree
requestDeleteDBClusterParameterGroup =
  req
    "DeleteDBClusterParameterGroup"
    "fixture/DeleteDBClusterParameterGroup.yaml"

requestDescribeSourceRegions :: DescribeSourceRegions -> TestTree
requestDescribeSourceRegions =
  req
    "DescribeSourceRegions"
    "fixture/DescribeSourceRegions.yaml"

requestPurchaseReservedDBInstancesOffering :: PurchaseReservedDBInstancesOffering -> TestTree
requestPurchaseReservedDBInstancesOffering =
  req
    "PurchaseReservedDBInstancesOffering"
    "fixture/PurchaseReservedDBInstancesOffering.yaml"

requestAuthorizeDBSecurityGroupIngress :: AuthorizeDBSecurityGroupIngress -> TestTree
requestAuthorizeDBSecurityGroupIngress =
  req
    "AuthorizeDBSecurityGroupIngress"
    "fixture/AuthorizeDBSecurityGroupIngress.yaml"

requestDeleteDBProxy :: DeleteDBProxy -> TestTree
requestDeleteDBProxy =
  req
    "DeleteDBProxy"
    "fixture/DeleteDBProxy.yaml"

requestRevokeDBSecurityGroupIngress :: RevokeDBSecurityGroupIngress -> TestTree
requestRevokeDBSecurityGroupIngress =
  req
    "RevokeDBSecurityGroupIngress"
    "fixture/RevokeDBSecurityGroupIngress.yaml"

requestModifyDBParameterGroup :: ModifyDBParameterGroup -> TestTree
requestModifyDBParameterGroup =
  req
    "ModifyDBParameterGroup"
    "fixture/ModifyDBParameterGroup.yaml"

requestDeregisterDBProxyTargets :: DeregisterDBProxyTargets -> TestTree
requestDeregisterDBProxyTargets =
  req
    "DeregisterDBProxyTargets"
    "fixture/DeregisterDBProxyTargets.yaml"

requestDeleteGlobalCluster :: DeleteGlobalCluster -> TestTree
requestDeleteGlobalCluster =
  req
    "DeleteGlobalCluster"
    "fixture/DeleteGlobalCluster.yaml"

requestPromoteReadReplicaDBCluster :: PromoteReadReplicaDBCluster -> TestTree
requestPromoteReadReplicaDBCluster =
  req
    "PromoteReadReplicaDBCluster"
    "fixture/PromoteReadReplicaDBCluster.yaml"

requestCreateGlobalCluster :: CreateGlobalCluster -> TestTree
requestCreateGlobalCluster =
  req
    "CreateGlobalCluster"
    "fixture/CreateGlobalCluster.yaml"

requestResetDBParameterGroup :: ResetDBParameterGroup -> TestTree
requestResetDBParameterGroup =
  req
    "ResetDBParameterGroup"
    "fixture/ResetDBParameterGroup.yaml"

requestDescribeDBProxyTargets :: DescribeDBProxyTargets -> TestTree
requestDescribeDBProxyTargets =
  req
    "DescribeDBProxyTargets"
    "fixture/DescribeDBProxyTargets.yaml"

requestImportInstallationMedia :: ImportInstallationMedia -> TestTree
requestImportInstallationMedia =
  req
    "ImportInstallationMedia"
    "fixture/ImportInstallationMedia.yaml"

requestDeleteCustomAvailabilityZone :: DeleteCustomAvailabilityZone -> TestTree
requestDeleteCustomAvailabilityZone =
  req
    "DeleteCustomAvailabilityZone"
    "fixture/DeleteCustomAvailabilityZone.yaml"

requestFailoverGlobalCluster :: FailoverGlobalCluster -> TestTree
requestFailoverGlobalCluster =
  req
    "FailoverGlobalCluster"
    "fixture/FailoverGlobalCluster.yaml"

requestRemoveSourceIdentifierFromSubscription :: RemoveSourceIdentifierFromSubscription -> TestTree
requestRemoveSourceIdentifierFromSubscription =
  req
    "RemoveSourceIdentifierFromSubscription"
    "fixture/RemoveSourceIdentifierFromSubscription.yaml"

requestCopyDBClusterSnapshot :: CopyDBClusterSnapshot -> TestTree
requestCopyDBClusterSnapshot =
  req
    "CopyDBClusterSnapshot"
    "fixture/CopyDBClusterSnapshot.yaml"

requestDescribeInstallationMedia :: DescribeInstallationMedia -> TestTree
requestDescribeInstallationMedia =
  req
    "DescribeInstallationMedia"
    "fixture/DescribeInstallationMedia.yaml"

requestCopyDBParameterGroup :: CopyDBParameterGroup -> TestTree
requestCopyDBParameterGroup =
  req
    "CopyDBParameterGroup"
    "fixture/CopyDBParameterGroup.yaml"

requestRestoreDBInstanceToPointInTime :: RestoreDBInstanceToPointInTime -> TestTree
requestRestoreDBInstanceToPointInTime =
  req
    "RestoreDBInstanceToPointInTime"
    "fixture/RestoreDBInstanceToPointInTime.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestModifyDBSnapshot :: ModifyDBSnapshot -> TestTree
requestModifyDBSnapshot =
  req
    "ModifyDBSnapshot"
    "fixture/ModifyDBSnapshot.yaml"

requestModifyDBSubnetGroup :: ModifyDBSubnetGroup -> TestTree
requestModifyDBSubnetGroup =
  req
    "ModifyDBSubnetGroup"
    "fixture/ModifyDBSubnetGroup.yaml"

requestDescribeEngineDefaultParameters :: DescribeEngineDefaultParameters -> TestTree
requestDescribeEngineDefaultParameters =
  req
    "DescribeEngineDefaultParameters"
    "fixture/DescribeEngineDefaultParameters.yaml"

requestDescribeCustomAvailabilityZones :: DescribeCustomAvailabilityZones -> TestTree
requestDescribeCustomAvailabilityZones =
  req
    "DescribeCustomAvailabilityZones"
    "fixture/DescribeCustomAvailabilityZones.yaml"

requestAddRoleToDBCluster :: AddRoleToDBCluster -> TestTree
requestAddRoleToDBCluster =
  req
    "AddRoleToDBCluster"
    "fixture/AddRoleToDBCluster.yaml"

-- Responses

responseResetDBClusterParameterGroup :: DBClusterParameterGroupNameMessage -> TestTree
responseResetDBClusterParameterGroup =
  res
    "ResetDBClusterParameterGroupResponse"
    "fixture/ResetDBClusterParameterGroupResponse.proto"
    defaultService
    (Proxy :: Proxy ResetDBClusterParameterGroup)

responseStopDBInstance :: StopDBInstanceResponse -> TestTree
responseStopDBInstance =
  res
    "StopDBInstanceResponse"
    "fixture/StopDBInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy StopDBInstance)

responseModifyDBInstance :: ModifyDBInstanceResponse -> TestTree
responseModifyDBInstance =
  res
    "ModifyDBInstanceResponse"
    "fixture/ModifyDBInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyDBInstance)

responseDescribeEventCategories :: DescribeEventCategoriesResponse -> TestTree
responseDescribeEventCategories =
  res
    "DescribeEventCategoriesResponse"
    "fixture/DescribeEventCategoriesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEventCategories)

responseStartDBInstance :: StartDBInstanceResponse -> TestTree
responseStartDBInstance =
  res
    "StartDBInstanceResponse"
    "fixture/StartDBInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy StartDBInstance)

responseCopyDBClusterParameterGroup :: CopyDBClusterParameterGroupResponse -> TestTree
responseCopyDBClusterParameterGroup =
  res
    "CopyDBClusterParameterGroupResponse"
    "fixture/CopyDBClusterParameterGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CopyDBClusterParameterGroup)

responseDescribeExportTasks :: DescribeExportTasksResponse -> TestTree
responseDescribeExportTasks =
  res
    "DescribeExportTasksResponse"
    "fixture/DescribeExportTasksResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeExportTasks)

responseCopyDBSnapshot :: CopyDBSnapshotResponse -> TestTree
responseCopyDBSnapshot =
  res
    "CopyDBSnapshotResponse"
    "fixture/CopyDBSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy CopyDBSnapshot)

responsePromoteReadReplica :: PromoteReadReplicaResponse -> TestTree
responsePromoteReadReplica =
  res
    "PromoteReadReplicaResponse"
    "fixture/PromoteReadReplicaResponse.proto"
    defaultService
    (Proxy :: Proxy PromoteReadReplica)

responseDescribeDBSnapshots :: DescribeDBSnapshotsResponse -> TestTree
responseDescribeDBSnapshots =
  res
    "DescribeDBSnapshotsResponse"
    "fixture/DescribeDBSnapshotsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDBSnapshots)

responseModifyDBCluster :: ModifyDBClusterResponse -> TestTree
responseModifyDBCluster =
  res
    "ModifyDBClusterResponse"
    "fixture/ModifyDBClusterResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyDBCluster)

responseModifyOptionGroup :: ModifyOptionGroupResponse -> TestTree
responseModifyOptionGroup =
  res
    "ModifyOptionGroupResponse"
    "fixture/ModifyOptionGroupResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyOptionGroup)

responseStopDBCluster :: StopDBClusterResponse -> TestTree
responseStopDBCluster =
  res
    "StopDBClusterResponse"
    "fixture/StopDBClusterResponse.proto"
    defaultService
    (Proxy :: Proxy StopDBCluster)

responseCreateDBClusterSnapshot :: CreateDBClusterSnapshotResponse -> TestTree
responseCreateDBClusterSnapshot =
  res
    "CreateDBClusterSnapshotResponse"
    "fixture/CreateDBClusterSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDBClusterSnapshot)

responseDescribeDBClusterParameterGroups :: DescribeDBClusterParameterGroupsResponse -> TestTree
responseDescribeDBClusterParameterGroups =
  res
    "DescribeDBClusterParameterGroupsResponse"
    "fixture/DescribeDBClusterParameterGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDBClusterParameterGroups)

responseDescribeDBEngineVersions :: DescribeDBEngineVersionsResponse -> TestTree
responseDescribeDBEngineVersions =
  res
    "DescribeDBEngineVersionsResponse"
    "fixture/DescribeDBEngineVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDBEngineVersions)

responseStartDBCluster :: StartDBClusterResponse -> TestTree
responseStartDBCluster =
  res
    "StartDBClusterResponse"
    "fixture/StartDBClusterResponse.proto"
    defaultService
    (Proxy :: Proxy StartDBCluster)

responseDescribeOptionGroupOptions :: DescribeOptionGroupOptionsResponse -> TestTree
responseDescribeOptionGroupOptions =
  res
    "DescribeOptionGroupOptionsResponse"
    "fixture/DescribeOptionGroupOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeOptionGroupOptions)

responseDeleteInstallationMedia :: InstallationMedia -> TestTree
responseDeleteInstallationMedia =
  res
    "DeleteInstallationMediaResponse"
    "fixture/DeleteInstallationMediaResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteInstallationMedia)

responseCopyOptionGroup :: CopyOptionGroupResponse -> TestTree
responseCopyOptionGroup =
  res
    "CopyOptionGroupResponse"
    "fixture/CopyOptionGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CopyOptionGroup)

responseDescribeDBInstanceAutomatedBackups :: DescribeDBInstanceAutomatedBackupsResponse -> TestTree
responseDescribeDBInstanceAutomatedBackups =
  res
    "DescribeDBInstanceAutomatedBackupsResponse"
    "fixture/DescribeDBInstanceAutomatedBackupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDBInstanceAutomatedBackups)

responseModifyGlobalCluster :: ModifyGlobalClusterResponse -> TestTree
responseModifyGlobalCluster =
  res
    "ModifyGlobalClusterResponse"
    "fixture/ModifyGlobalClusterResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyGlobalCluster)

responseDescribeDBProxies :: DescribeDBProxiesResponse -> TestTree
responseDescribeDBProxies =
  res
    "DescribeDBProxiesResponse"
    "fixture/DescribeDBProxiesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDBProxies)

responseRestoreDBClusterToPointInTime :: RestoreDBClusterToPointInTimeResponse -> TestTree
responseRestoreDBClusterToPointInTime =
  res
    "RestoreDBClusterToPointInTimeResponse"
    "fixture/RestoreDBClusterToPointInTimeResponse.proto"
    defaultService
    (Proxy :: Proxy RestoreDBClusterToPointInTime)

responseStopActivityStream :: StopActivityStreamResponse -> TestTree
responseStopActivityStream =
  res
    "StopActivityStreamResponse"
    "fixture/StopActivityStreamResponse.proto"
    defaultService
    (Proxy :: Proxy StopActivityStream)

responseCreateDBInstanceReadReplica :: CreateDBInstanceReadReplicaResponse -> TestTree
responseCreateDBInstanceReadReplica =
  res
    "CreateDBInstanceReadReplicaResponse"
    "fixture/CreateDBInstanceReadReplicaResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDBInstanceReadReplica)

responseDescribeValidDBInstanceModifications :: DescribeValidDBInstanceModificationsResponse -> TestTree
responseDescribeValidDBInstanceModifications =
  res
    "DescribeValidDBInstanceModificationsResponse"
    "fixture/DescribeValidDBInstanceModificationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeValidDBInstanceModifications)

responseStartExportTask :: ExportTask -> TestTree
responseStartExportTask =
  res
    "StartExportTaskResponse"
    "fixture/StartExportTaskResponse.proto"
    defaultService
    (Proxy :: Proxy StartExportTask)

responseDescribeDBParameters :: DescribeDBParametersResponse -> TestTree
responseDescribeDBParameters =
  res
    "DescribeDBParametersResponse"
    "fixture/DescribeDBParametersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDBParameters)

responseDescribeDBInstances :: DescribeDBInstancesResponse -> TestTree
responseDescribeDBInstances =
  res
    "DescribeDBInstancesResponse"
    "fixture/DescribeDBInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDBInstances)

responseDescribeEventSubscriptions :: DescribeEventSubscriptionsResponse -> TestTree
responseDescribeEventSubscriptions =
  res
    "DescribeEventSubscriptionsResponse"
    "fixture/DescribeEventSubscriptionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEventSubscriptions)

responseRestoreDBInstanceFromS :: RestoreDBInstanceFromSResponse -> TestTree
responseRestoreDBInstanceFromS =
  res
    "RestoreDBInstanceFromSResponse"
    "fixture/RestoreDBInstanceFromSResponse.proto"
    defaultService
    (Proxy :: Proxy RestoreDBInstanceFromS)

responseStartActivityStream :: StartActivityStreamResponse -> TestTree
responseStartActivityStream =
  res
    "StartActivityStreamResponse"
    "fixture/StartActivityStreamResponse.proto"
    defaultService
    (Proxy :: Proxy StartActivityStream)

responseDescribeDBClusterEndpoints :: DescribeDBClusterEndpointsResponse -> TestTree
responseDescribeDBClusterEndpoints =
  res
    "DescribeDBClusterEndpointsResponse"
    "fixture/DescribeDBClusterEndpointsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDBClusterEndpoints)

responseModifyCertificates :: ModifyCertificatesResponse -> TestTree
responseModifyCertificates =
  res
    "ModifyCertificatesResponse"
    "fixture/ModifyCertificatesResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyCertificates)

responseDescribeDBClusterSnapshots :: DescribeDBClusterSnapshotsResponse -> TestTree
responseDescribeDBClusterSnapshots =
  res
    "DescribeDBClusterSnapshotsResponse"
    "fixture/DescribeDBClusterSnapshotsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDBClusterSnapshots)

responseDeleteDBClusterEndpoint :: DBClusterEndpoint -> TestTree
responseDeleteDBClusterEndpoint =
  res
    "DeleteDBClusterEndpointResponse"
    "fixture/DeleteDBClusterEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDBClusterEndpoint)

responseModifyDBSnapshotAttribute :: ModifyDBSnapshotAttributeResponse -> TestTree
responseModifyDBSnapshotAttribute =
  res
    "ModifyDBSnapshotAttributeResponse"
    "fixture/ModifyDBSnapshotAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyDBSnapshotAttribute)

responseDescribeReservedDBInstancesOfferings :: DescribeReservedDBInstancesOfferingsResponse -> TestTree
responseDescribeReservedDBInstancesOfferings =
  res
    "DescribeReservedDBInstancesOfferingsResponse"
    "fixture/DescribeReservedDBInstancesOfferingsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeReservedDBInstancesOfferings)

responseDeleteDBInstance :: DeleteDBInstanceResponse -> TestTree
responseDeleteDBInstance =
  res
    "DeleteDBInstanceResponse"
    "fixture/DeleteDBInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDBInstance)

responseRebootDBInstance :: RebootDBInstanceResponse -> TestTree
responseRebootDBInstance =
  res
    "RebootDBInstanceResponse"
    "fixture/RebootDBInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy RebootDBInstance)

responseDeleteEventSubscription :: DeleteEventSubscriptionResponse -> TestTree
responseDeleteEventSubscription =
  res
    "DeleteEventSubscriptionResponse"
    "fixture/DeleteEventSubscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteEventSubscription)

responseDescribeDBParameterGroups :: DescribeDBParameterGroupsResponse -> TestTree
responseDescribeDBParameterGroups =
  res
    "DescribeDBParameterGroupsResponse"
    "fixture/DescribeDBParameterGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDBParameterGroups)

responseDeleteDBInstanceAutomatedBackup :: DeleteDBInstanceAutomatedBackupResponse -> TestTree
responseDeleteDBInstanceAutomatedBackup =
  res
    "DeleteDBInstanceAutomatedBackupResponse"
    "fixture/DeleteDBInstanceAutomatedBackupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDBInstanceAutomatedBackup)

responseDescribeDBClusterBacktracks :: DescribeDBClusterBacktracksResponse -> TestTree
responseDescribeDBClusterBacktracks =
  res
    "DescribeDBClusterBacktracksResponse"
    "fixture/DescribeDBClusterBacktracksResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDBClusterBacktracks)

responseCreateDBClusterParameterGroup :: CreateDBClusterParameterGroupResponse -> TestTree
responseCreateDBClusterParameterGroup =
  res
    "CreateDBClusterParameterGroupResponse"
    "fixture/CreateDBClusterParameterGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDBClusterParameterGroup)

responseCreateDBProxy :: CreateDBProxyResponse -> TestTree
responseCreateDBProxy =
  res
    "CreateDBProxyResponse"
    "fixture/CreateDBProxyResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDBProxy)

responseDescribeDBClusterParameters :: DescribeDBClusterParametersResponse -> TestTree
responseDescribeDBClusterParameters =
  res
    "DescribeDBClusterParametersResponse"
    "fixture/DescribeDBClusterParametersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDBClusterParameters)

responseDeleteDBSubnetGroup :: DeleteDBSubnetGroupResponse -> TestTree
responseDeleteDBSubnetGroup =
  res
    "DeleteDBSubnetGroupResponse"
    "fixture/DeleteDBSubnetGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDBSubnetGroup)

responseDescribeAccountAttributes :: DescribeAccountAttributesResponse -> TestTree
responseDescribeAccountAttributes =
  res
    "DescribeAccountAttributesResponse"
    "fixture/DescribeAccountAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAccountAttributes)

responseRemoveTagsFromResource :: RemoveTagsFromResourceResponse -> TestTree
responseRemoveTagsFromResource =
  res
    "RemoveTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveTagsFromResource)

responseCreateDBSecurityGroup :: CreateDBSecurityGroupResponse -> TestTree
responseCreateDBSecurityGroup =
  res
    "CreateDBSecurityGroupResponse"
    "fixture/CreateDBSecurityGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDBSecurityGroup)

responseDescribeCertificates :: DescribeCertificatesResponse -> TestTree
responseDescribeCertificates =
  res
    "DescribeCertificatesResponse"
    "fixture/DescribeCertificatesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCertificates)

responseCreateDBInstance :: CreateDBInstanceResponse -> TestTree
responseCreateDBInstance =
  res
    "CreateDBInstanceResponse"
    "fixture/CreateDBInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDBInstance)

responseRemoveRoleFromDBInstance :: RemoveRoleFromDBInstanceResponse -> TestTree
responseRemoveRoleFromDBInstance =
  res
    "RemoveRoleFromDBInstanceResponse"
    "fixture/RemoveRoleFromDBInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveRoleFromDBInstance)

responseDescribeEngineDefaultClusterParameters :: DescribeEngineDefaultClusterParametersResponse -> TestTree
responseDescribeEngineDefaultClusterParameters =
  res
    "DescribeEngineDefaultClusterParametersResponse"
    "fixture/DescribeEngineDefaultClusterParametersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEngineDefaultClusterParameters)

responseStartDBInstanceAutomatedBackupsReplication :: StartDBInstanceAutomatedBackupsReplicationResponse -> TestTree
responseStartDBInstanceAutomatedBackupsReplication =
  res
    "StartDBInstanceAutomatedBackupsReplicationResponse"
    "fixture/StartDBInstanceAutomatedBackupsReplicationResponse.proto"
    defaultService
    (Proxy :: Proxy StartDBInstanceAutomatedBackupsReplication)

responseCreateOptionGroup :: CreateOptionGroupResponse -> TestTree
responseCreateOptionGroup =
  res
    "CreateOptionGroupResponse"
    "fixture/CreateOptionGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateOptionGroup)

responseCreateCustomAvailabilityZone :: CreateCustomAvailabilityZoneResponse -> TestTree
responseCreateCustomAvailabilityZone =
  res
    "CreateCustomAvailabilityZoneResponse"
    "fixture/CreateCustomAvailabilityZoneResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCustomAvailabilityZone)

responseStopDBInstanceAutomatedBackupsReplication :: StopDBInstanceAutomatedBackupsReplicationResponse -> TestTree
responseStopDBInstanceAutomatedBackupsReplication =
  res
    "StopDBInstanceAutomatedBackupsReplicationResponse"
    "fixture/StopDBInstanceAutomatedBackupsReplicationResponse.proto"
    defaultService
    (Proxy :: Proxy StopDBInstanceAutomatedBackupsReplication)

responseFailoverDBCluster :: FailoverDBClusterResponse -> TestTree
responseFailoverDBCluster =
  res
    "FailoverDBClusterResponse"
    "fixture/FailoverDBClusterResponse.proto"
    defaultService
    (Proxy :: Proxy FailoverDBCluster)

responseRemoveRoleFromDBCluster :: RemoveRoleFromDBClusterResponse -> TestTree
responseRemoveRoleFromDBCluster =
  res
    "RemoveRoleFromDBClusterResponse"
    "fixture/RemoveRoleFromDBClusterResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveRoleFromDBCluster)

responseDescribeDBSnapshotAttributes :: DescribeDBSnapshotAttributesResponse -> TestTree
responseDescribeDBSnapshotAttributes =
  res
    "DescribeDBSnapshotAttributesResponse"
    "fixture/DescribeDBSnapshotAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDBSnapshotAttributes)

responseCreateDBCluster :: CreateDBClusterResponse -> TestTree
responseCreateDBCluster =
  res
    "CreateDBClusterResponse"
    "fixture/CreateDBClusterResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDBCluster)

responseApplyPendingMaintenanceAction :: ApplyPendingMaintenanceActionResponse -> TestTree
responseApplyPendingMaintenanceAction =
  res
    "ApplyPendingMaintenanceActionResponse"
    "fixture/ApplyPendingMaintenanceActionResponse.proto"
    defaultService
    (Proxy :: Proxy ApplyPendingMaintenanceAction)

responseBacktrackDBCluster :: DBClusterBacktrack -> TestTree
responseBacktrackDBCluster =
  res
    "BacktrackDBClusterResponse"
    "fixture/BacktrackDBClusterResponse.proto"
    defaultService
    (Proxy :: Proxy BacktrackDBCluster)

responseDescribeReservedDBInstances :: DescribeReservedDBInstancesResponse -> TestTree
responseDescribeReservedDBInstances =
  res
    "DescribeReservedDBInstancesResponse"
    "fixture/DescribeReservedDBInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeReservedDBInstances)

responseDeleteDBCluster :: DeleteDBClusterResponse -> TestTree
responseDeleteDBCluster =
  res
    "DeleteDBClusterResponse"
    "fixture/DeleteDBClusterResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDBCluster)

responseDeleteOptionGroup :: DeleteOptionGroupResponse -> TestTree
responseDeleteOptionGroup =
  res
    "DeleteOptionGroupResponse"
    "fixture/DeleteOptionGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteOptionGroup)

responseDescribePendingMaintenanceActions :: DescribePendingMaintenanceActionsResponse -> TestTree
responseDescribePendingMaintenanceActions =
  res
    "DescribePendingMaintenanceActionsResponse"
    "fixture/DescribePendingMaintenanceActionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePendingMaintenanceActions)

responseDescribeDBProxyTargetGroups :: DescribeDBProxyTargetGroupsResponse -> TestTree
responseDescribeDBProxyTargetGroups =
  res
    "DescribeDBProxyTargetGroupsResponse"
    "fixture/DescribeDBProxyTargetGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDBProxyTargetGroups)

responseModifyDBClusterParameterGroup :: DBClusterParameterGroupNameMessage -> TestTree
responseModifyDBClusterParameterGroup =
  res
    "ModifyDBClusterParameterGroupResponse"
    "fixture/ModifyDBClusterParameterGroupResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyDBClusterParameterGroup)

responseDescribeDBClusterSnapshotAttributes :: DescribeDBClusterSnapshotAttributesResponse -> TestTree
responseDescribeDBClusterSnapshotAttributes =
  res
    "DescribeDBClusterSnapshotAttributesResponse"
    "fixture/DescribeDBClusterSnapshotAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDBClusterSnapshotAttributes)

responseDescribeOptionGroups :: DescribeOptionGroupsResponse -> TestTree
responseDescribeOptionGroups =
  res
    "DescribeOptionGroupsResponse"
    "fixture/DescribeOptionGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeOptionGroups)

responseDescribeDBClusters :: DescribeDBClustersResponse -> TestTree
responseDescribeDBClusters =
  res
    "DescribeDBClustersResponse"
    "fixture/DescribeDBClustersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDBClusters)

responseDescribeEvents :: DescribeEventsResponse -> TestTree
responseDescribeEvents =
  res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEvents)

responseModifyDBProxy :: ModifyDBProxyResponse -> TestTree
responseModifyDBProxy =
  res
    "ModifyDBProxyResponse"
    "fixture/ModifyDBProxyResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyDBProxy)

responseDescribeDBLogFiles :: DescribeDBLogFilesResponse -> TestTree
responseDescribeDBLogFiles =
  res
    "DescribeDBLogFilesResponse"
    "fixture/DescribeDBLogFilesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDBLogFiles)

responseRestoreDBClusterFromS :: RestoreDBClusterFromSResponse -> TestTree
responseRestoreDBClusterFromS =
  res
    "RestoreDBClusterFromSResponse"
    "fixture/RestoreDBClusterFromSResponse.proto"
    defaultService
    (Proxy :: Proxy RestoreDBClusterFromS)

responseCancelExportTask :: ExportTask -> TestTree
responseCancelExportTask =
  res
    "CancelExportTaskResponse"
    "fixture/CancelExportTaskResponse.proto"
    defaultService
    (Proxy :: Proxy CancelExportTask)

responseDescribeGlobalClusters :: DescribeGlobalClustersResponse -> TestTree
responseDescribeGlobalClusters =
  res
    "DescribeGlobalClustersResponse"
    "fixture/DescribeGlobalClustersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeGlobalClusters)

responseAddSourceIdentifierToSubscription :: AddSourceIdentifierToSubscriptionResponse -> TestTree
responseAddSourceIdentifierToSubscription =
  res
    "AddSourceIdentifierToSubscriptionResponse"
    "fixture/AddSourceIdentifierToSubscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy AddSourceIdentifierToSubscription)

responseModifyEventSubscription :: ModifyEventSubscriptionResponse -> TestTree
responseModifyEventSubscription =
  res
    "ModifyEventSubscriptionResponse"
    "fixture/ModifyEventSubscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyEventSubscription)

responseModifyDBClusterEndpoint :: DBClusterEndpoint -> TestTree
responseModifyDBClusterEndpoint =
  res
    "ModifyDBClusterEndpointResponse"
    "fixture/ModifyDBClusterEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyDBClusterEndpoint)

responseModifyDBClusterSnapshotAttribute :: ModifyDBClusterSnapshotAttributeResponse -> TestTree
responseModifyDBClusterSnapshotAttribute =
  res
    "ModifyDBClusterSnapshotAttributeResponse"
    "fixture/ModifyDBClusterSnapshotAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyDBClusterSnapshotAttribute)

responseDescribeDBSubnetGroups :: DescribeDBSubnetGroupsResponse -> TestTree
responseDescribeDBSubnetGroups =
  res
    "DescribeDBSubnetGroupsResponse"
    "fixture/DescribeDBSubnetGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDBSubnetGroups)

responseModifyDBProxyTargetGroup :: ModifyDBProxyTargetGroupResponse -> TestTree
responseModifyDBProxyTargetGroup =
  res
    "ModifyDBProxyTargetGroupResponse"
    "fixture/ModifyDBProxyTargetGroupResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyDBProxyTargetGroup)

responseCreateDBParameterGroup :: CreateDBParameterGroupResponse -> TestTree
responseCreateDBParameterGroup =
  res
    "CreateDBParameterGroupResponse"
    "fixture/CreateDBParameterGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDBParameterGroup)

responseDeleteDBClusterSnapshot :: DeleteDBClusterSnapshotResponse -> TestTree
responseDeleteDBClusterSnapshot =
  res
    "DeleteDBClusterSnapshotResponse"
    "fixture/DeleteDBClusterSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDBClusterSnapshot)

responseRemoveFromGlobalCluster :: RemoveFromGlobalClusterResponse -> TestTree
responseRemoveFromGlobalCluster =
  res
    "RemoveFromGlobalClusterResponse"
    "fixture/RemoveFromGlobalClusterResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveFromGlobalCluster)

responseDescribeDBSecurityGroups :: DescribeDBSecurityGroupsResponse -> TestTree
responseDescribeDBSecurityGroups =
  res
    "DescribeDBSecurityGroupsResponse"
    "fixture/DescribeDBSecurityGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDBSecurityGroups)

responseAddTagsToResource :: AddTagsToResourceResponse -> TestTree
responseAddTagsToResource =
  res
    "AddTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse.proto"
    defaultService
    (Proxy :: Proxy AddTagsToResource)

responseRegisterDBProxyTargets :: RegisterDBProxyTargetsResponse -> TestTree
responseRegisterDBProxyTargets =
  res
    "RegisterDBProxyTargetsResponse"
    "fixture/RegisterDBProxyTargetsResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterDBProxyTargets)

responseDownloadDBLogFilePortion :: DownloadDBLogFilePortionResponse -> TestTree
responseDownloadDBLogFilePortion =
  res
    "DownloadDBLogFilePortionResponse"
    "fixture/DownloadDBLogFilePortionResponse.proto"
    defaultService
    (Proxy :: Proxy DownloadDBLogFilePortion)

responseModifyCurrentDBClusterCapacity :: ModifyCurrentDBClusterCapacityResponse -> TestTree
responseModifyCurrentDBClusterCapacity =
  res
    "ModifyCurrentDBClusterCapacityResponse"
    "fixture/ModifyCurrentDBClusterCapacityResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyCurrentDBClusterCapacity)

responseAddRoleToDBInstance :: AddRoleToDBInstanceResponse -> TestTree
responseAddRoleToDBInstance =
  res
    "AddRoleToDBInstanceResponse"
    "fixture/AddRoleToDBInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy AddRoleToDBInstance)

responseDeleteDBParameterGroup :: DeleteDBParameterGroupResponse -> TestTree
responseDeleteDBParameterGroup =
  res
    "DeleteDBParameterGroupResponse"
    "fixture/DeleteDBParameterGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDBParameterGroup)

responseCreateDBSnapshot :: CreateDBSnapshotResponse -> TestTree
responseCreateDBSnapshot =
  res
    "CreateDBSnapshotResponse"
    "fixture/CreateDBSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDBSnapshot)

responseCreateDBSubnetGroup :: CreateDBSubnetGroupResponse -> TestTree
responseCreateDBSubnetGroup =
  res
    "CreateDBSubnetGroupResponse"
    "fixture/CreateDBSubnetGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDBSubnetGroup)

responseDescribeOrderableDBInstanceOptions :: DescribeOrderableDBInstanceOptionsResponse -> TestTree
responseDescribeOrderableDBInstanceOptions =
  res
    "DescribeOrderableDBInstanceOptionsResponse"
    "fixture/DescribeOrderableDBInstanceOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeOrderableDBInstanceOptions)

responseDeleteDBSecurityGroup :: DeleteDBSecurityGroupResponse -> TestTree
responseDeleteDBSecurityGroup =
  res
    "DeleteDBSecurityGroupResponse"
    "fixture/DeleteDBSecurityGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDBSecurityGroup)

responseRestoreDBClusterFromSnapshot :: RestoreDBClusterFromSnapshotResponse -> TestTree
responseRestoreDBClusterFromSnapshot =
  res
    "RestoreDBClusterFromSnapshotResponse"
    "fixture/RestoreDBClusterFromSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy RestoreDBClusterFromSnapshot)

responseRestoreDBInstanceFromDBSnapshot :: RestoreDBInstanceFromDBSnapshotResponse -> TestTree
responseRestoreDBInstanceFromDBSnapshot =
  res
    "RestoreDBInstanceFromDBSnapshotResponse"
    "fixture/RestoreDBInstanceFromDBSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy RestoreDBInstanceFromDBSnapshot)

responseCreateDBClusterEndpoint :: DBClusterEndpoint -> TestTree
responseCreateDBClusterEndpoint =
  res
    "CreateDBClusterEndpointResponse"
    "fixture/CreateDBClusterEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDBClusterEndpoint)

responseCreateEventSubscription :: CreateEventSubscriptionResponse -> TestTree
responseCreateEventSubscription =
  res
    "CreateEventSubscriptionResponse"
    "fixture/CreateEventSubscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateEventSubscription)

responseDeleteDBSnapshot :: DeleteDBSnapshotResponse -> TestTree
responseDeleteDBSnapshot =
  res
    "DeleteDBSnapshotResponse"
    "fixture/DeleteDBSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDBSnapshot)

responseDeleteDBClusterParameterGroup :: DeleteDBClusterParameterGroupResponse -> TestTree
responseDeleteDBClusterParameterGroup =
  res
    "DeleteDBClusterParameterGroupResponse"
    "fixture/DeleteDBClusterParameterGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDBClusterParameterGroup)

responseDescribeSourceRegions :: DescribeSourceRegionsResponse -> TestTree
responseDescribeSourceRegions =
  res
    "DescribeSourceRegionsResponse"
    "fixture/DescribeSourceRegionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSourceRegions)

responsePurchaseReservedDBInstancesOffering :: PurchaseReservedDBInstancesOfferingResponse -> TestTree
responsePurchaseReservedDBInstancesOffering =
  res
    "PurchaseReservedDBInstancesOfferingResponse"
    "fixture/PurchaseReservedDBInstancesOfferingResponse.proto"
    defaultService
    (Proxy :: Proxy PurchaseReservedDBInstancesOffering)

responseAuthorizeDBSecurityGroupIngress :: AuthorizeDBSecurityGroupIngressResponse -> TestTree
responseAuthorizeDBSecurityGroupIngress =
  res
    "AuthorizeDBSecurityGroupIngressResponse"
    "fixture/AuthorizeDBSecurityGroupIngressResponse.proto"
    defaultService
    (Proxy :: Proxy AuthorizeDBSecurityGroupIngress)

responseDeleteDBProxy :: DeleteDBProxyResponse -> TestTree
responseDeleteDBProxy =
  res
    "DeleteDBProxyResponse"
    "fixture/DeleteDBProxyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDBProxy)

responseRevokeDBSecurityGroupIngress :: RevokeDBSecurityGroupIngressResponse -> TestTree
responseRevokeDBSecurityGroupIngress =
  res
    "RevokeDBSecurityGroupIngressResponse"
    "fixture/RevokeDBSecurityGroupIngressResponse.proto"
    defaultService
    (Proxy :: Proxy RevokeDBSecurityGroupIngress)

responseModifyDBParameterGroup :: DBParameterGroupNameMessage -> TestTree
responseModifyDBParameterGroup =
  res
    "ModifyDBParameterGroupResponse"
    "fixture/ModifyDBParameterGroupResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyDBParameterGroup)

responseDeregisterDBProxyTargets :: DeregisterDBProxyTargetsResponse -> TestTree
responseDeregisterDBProxyTargets =
  res
    "DeregisterDBProxyTargetsResponse"
    "fixture/DeregisterDBProxyTargetsResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterDBProxyTargets)

responseDeleteGlobalCluster :: DeleteGlobalClusterResponse -> TestTree
responseDeleteGlobalCluster =
  res
    "DeleteGlobalClusterResponse"
    "fixture/DeleteGlobalClusterResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteGlobalCluster)

responsePromoteReadReplicaDBCluster :: PromoteReadReplicaDBClusterResponse -> TestTree
responsePromoteReadReplicaDBCluster =
  res
    "PromoteReadReplicaDBClusterResponse"
    "fixture/PromoteReadReplicaDBClusterResponse.proto"
    defaultService
    (Proxy :: Proxy PromoteReadReplicaDBCluster)

responseCreateGlobalCluster :: CreateGlobalClusterResponse -> TestTree
responseCreateGlobalCluster =
  res
    "CreateGlobalClusterResponse"
    "fixture/CreateGlobalClusterResponse.proto"
    defaultService
    (Proxy :: Proxy CreateGlobalCluster)

responseResetDBParameterGroup :: DBParameterGroupNameMessage -> TestTree
responseResetDBParameterGroup =
  res
    "ResetDBParameterGroupResponse"
    "fixture/ResetDBParameterGroupResponse.proto"
    defaultService
    (Proxy :: Proxy ResetDBParameterGroup)

responseDescribeDBProxyTargets :: DescribeDBProxyTargetsResponse -> TestTree
responseDescribeDBProxyTargets =
  res
    "DescribeDBProxyTargetsResponse"
    "fixture/DescribeDBProxyTargetsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDBProxyTargets)

responseImportInstallationMedia :: InstallationMedia -> TestTree
responseImportInstallationMedia =
  res
    "ImportInstallationMediaResponse"
    "fixture/ImportInstallationMediaResponse.proto"
    defaultService
    (Proxy :: Proxy ImportInstallationMedia)

responseDeleteCustomAvailabilityZone :: DeleteCustomAvailabilityZoneResponse -> TestTree
responseDeleteCustomAvailabilityZone =
  res
    "DeleteCustomAvailabilityZoneResponse"
    "fixture/DeleteCustomAvailabilityZoneResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCustomAvailabilityZone)

responseFailoverGlobalCluster :: FailoverGlobalClusterResponse -> TestTree
responseFailoverGlobalCluster =
  res
    "FailoverGlobalClusterResponse"
    "fixture/FailoverGlobalClusterResponse.proto"
    defaultService
    (Proxy :: Proxy FailoverGlobalCluster)

responseRemoveSourceIdentifierFromSubscription :: RemoveSourceIdentifierFromSubscriptionResponse -> TestTree
responseRemoveSourceIdentifierFromSubscription =
  res
    "RemoveSourceIdentifierFromSubscriptionResponse"
    "fixture/RemoveSourceIdentifierFromSubscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveSourceIdentifierFromSubscription)

responseCopyDBClusterSnapshot :: CopyDBClusterSnapshotResponse -> TestTree
responseCopyDBClusterSnapshot =
  res
    "CopyDBClusterSnapshotResponse"
    "fixture/CopyDBClusterSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy CopyDBClusterSnapshot)

responseDescribeInstallationMedia :: DescribeInstallationMediaResponse -> TestTree
responseDescribeInstallationMedia =
  res
    "DescribeInstallationMediaResponse"
    "fixture/DescribeInstallationMediaResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInstallationMedia)

responseCopyDBParameterGroup :: CopyDBParameterGroupResponse -> TestTree
responseCopyDBParameterGroup =
  res
    "CopyDBParameterGroupResponse"
    "fixture/CopyDBParameterGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CopyDBParameterGroup)

responseRestoreDBInstanceToPointInTime :: RestoreDBInstanceToPointInTimeResponse -> TestTree
responseRestoreDBInstanceToPointInTime =
  res
    "RestoreDBInstanceToPointInTimeResponse"
    "fixture/RestoreDBInstanceToPointInTimeResponse.proto"
    defaultService
    (Proxy :: Proxy RestoreDBInstanceToPointInTime)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseModifyDBSnapshot :: ModifyDBSnapshotResponse -> TestTree
responseModifyDBSnapshot =
  res
    "ModifyDBSnapshotResponse"
    "fixture/ModifyDBSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyDBSnapshot)

responseModifyDBSubnetGroup :: ModifyDBSubnetGroupResponse -> TestTree
responseModifyDBSubnetGroup =
  res
    "ModifyDBSubnetGroupResponse"
    "fixture/ModifyDBSubnetGroupResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyDBSubnetGroup)

responseDescribeEngineDefaultParameters :: DescribeEngineDefaultParametersResponse -> TestTree
responseDescribeEngineDefaultParameters =
  res
    "DescribeEngineDefaultParametersResponse"
    "fixture/DescribeEngineDefaultParametersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEngineDefaultParameters)

responseDescribeCustomAvailabilityZones :: DescribeCustomAvailabilityZonesResponse -> TestTree
responseDescribeCustomAvailabilityZones =
  res
    "DescribeCustomAvailabilityZonesResponse"
    "fixture/DescribeCustomAvailabilityZonesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCustomAvailabilityZones)

responseAddRoleToDBCluster :: AddRoleToDBClusterResponse -> TestTree
responseAddRoleToDBCluster =
  res
    "AddRoleToDBClusterResponse"
    "fixture/AddRoleToDBClusterResponse.proto"
    defaultService
    (Proxy :: Proxy AddRoleToDBCluster)
