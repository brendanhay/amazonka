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
--         [ requestDescribeExportTasks $
--             newDescribeExportTasks
--
--         , requestModifyDBInstance $
--             newModifyDBInstance
--
--         , requestCopyDBSnapshot $
--             newCopyDBSnapshot
--
--         , requestStopDBInstance $
--             newStopDBInstance
--
--         , requestDescribeEventCategories $
--             newDescribeEventCategories
--
--         , requestCopyDBClusterParameterGroup $
--             newCopyDBClusterParameterGroup
--
--         , requestResetDBClusterParameterGroup $
--             newResetDBClusterParameterGroup
--
--         , requestDescribeDBProxyEndpoints $
--             newDescribeDBProxyEndpoints
--
--         , requestStartDBInstance $
--             newStartDBInstance
--
--         , requestModifyDBCluster $
--             newModifyDBCluster
--
--         , requestCreateDBClusterSnapshot $
--             newCreateDBClusterSnapshot
--
--         , requestDescribeDBClusterParameterGroups $
--             newDescribeDBClusterParameterGroups
--
--         , requestModifyOptionGroup $
--             newModifyOptionGroup
--
--         , requestStartDBCluster $
--             newStartDBCluster
--
--         , requestDescribeDBSnapshots $
--             newDescribeDBSnapshots
--
--         , requestStopDBCluster $
--             newStopDBCluster
--
--         , requestPromoteReadReplica $
--             newPromoteReadReplica
--
--         , requestDescribeDBEngineVersions $
--             newDescribeDBEngineVersions
--
--         , requestModifyGlobalCluster $
--             newModifyGlobalCluster
--
--         , requestCreateDBInstanceReadReplica $
--             newCreateDBInstanceReadReplica
--
--         , requestStartActivityStream $
--             newStartActivityStream
--
--         , requestDescribeDBInstanceAutomatedBackups $
--             newDescribeDBInstanceAutomatedBackups
--
--         , requestDeleteInstallationMedia $
--             newDeleteInstallationMedia
--
--         , requestDescribeOptionGroupOptions $
--             newDescribeOptionGroupOptions
--
--         , requestRestoreDBInstanceFromS $
--             newRestoreDBInstanceFromS
--
--         , requestDescribeDBParameters $
--             newDescribeDBParameters
--
--         , requestCopyOptionGroup $
--             newCopyOptionGroup
--
--         , requestStopActivityStream $
--             newStopActivityStream
--
--         , requestDescribeValidDBInstanceModifications $
--             newDescribeValidDBInstanceModifications
--
--         , requestStartExportTask $
--             newStartExportTask
--
--         , requestDescribeDBProxies $
--             newDescribeDBProxies
--
--         , requestRestoreDBClusterToPointInTime $
--             newRestoreDBClusterToPointInTime
--
--         , requestDescribeDBInstances $
--             newDescribeDBInstances
--
--         , requestDescribeDBClusterEndpoints $
--             newDescribeDBClusterEndpoints
--
--         , requestDescribeEventSubscriptions $
--             newDescribeEventSubscriptions
--
--         , requestDeleteEventSubscription $
--             newDeleteEventSubscription
--
--         , requestDescribeReservedDBInstancesOfferings $
--             newDescribeReservedDBInstancesOfferings
--
--         , requestRebootDBInstance $
--             newRebootDBInstance
--
--         , requestDescribeDBClusterBacktracks $
--             newDescribeDBClusterBacktracks
--
--         , requestDescribeDBParameterGroups $
--             newDescribeDBParameterGroups
--
--         , requestCreateDBClusterParameterGroup $
--             newCreateDBClusterParameterGroup
--
--         , requestDeleteDBInstance $
--             newDeleteDBInstance
--
--         , requestCreateDBProxy $
--             newCreateDBProxy
--
--         , requestModifyCertificates $
--             newModifyCertificates
--
--         , requestDeleteDBInstanceAutomatedBackup $
--             newDeleteDBInstanceAutomatedBackup
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
--         , requestDescribeAccountAttributes $
--             newDescribeAccountAttributes
--
--         , requestRemoveRoleFromDBInstance $
--             newRemoveRoleFromDBInstance
--
--         , requestDescribeDBClusterParameters $
--             newDescribeDBClusterParameters
--
--         , requestDeleteDBSubnetGroup $
--             newDeleteDBSubnetGroup
--
--         , requestDescribeEngineDefaultClusterParameters $
--             newDescribeEngineDefaultClusterParameters
--
--         , requestStopDBInstanceAutomatedBackupsReplication $
--             newStopDBInstanceAutomatedBackupsReplication
--
--         , requestBacktrackDBCluster $
--             newBacktrackDBCluster
--
--         , requestRemoveRoleFromDBCluster $
--             newRemoveRoleFromDBCluster
--
--         , requestCreateDBCluster $
--             newCreateDBCluster
--
--         , requestApplyPendingMaintenanceAction $
--             newApplyPendingMaintenanceAction
--
--         , requestFailoverDBCluster $
--             newFailoverDBCluster
--
--         , requestStartDBInstanceAutomatedBackupsReplication $
--             newStartDBInstanceAutomatedBackupsReplication
--
--         , requestCreateCustomAvailabilityZone $
--             newCreateCustomAvailabilityZone
--
--         , requestDeleteDBProxyEndpoint $
--             newDeleteDBProxyEndpoint
--
--         , requestDescribeDBSnapshotAttributes $
--             newDescribeDBSnapshotAttributes
--
--         , requestCreateOptionGroup $
--             newCreateOptionGroup
--
--         , requestDeleteDBCluster $
--             newDeleteDBCluster
--
--         , requestCreateDBProxyEndpoint $
--             newCreateDBProxyEndpoint
--
--         , requestDescribeReservedDBInstances $
--             newDescribeReservedDBInstances
--
--         , requestDeleteOptionGroup $
--             newDeleteOptionGroup
--
--         , requestDescribeEvents $
--             newDescribeEvents
--
--         , requestDescribeDBProxyTargetGroups $
--             newDescribeDBProxyTargetGroups
--
--         , requestModifyDBClusterParameterGroup $
--             newModifyDBClusterParameterGroup
--
--         , requestDescribeDBLogFiles $
--             newDescribeDBLogFiles
--
--         , requestDescribeOptionGroups $
--             newDescribeOptionGroups
--
--         , requestDescribePendingMaintenanceActions $
--             newDescribePendingMaintenanceActions
--
--         , requestRestoreDBClusterFromS $
--             newRestoreDBClusterFromS
--
--         , requestDescribeDBClusters $
--             newDescribeDBClusters
--
--         , requestDescribeDBClusterSnapshotAttributes $
--             newDescribeDBClusterSnapshotAttributes
--
--         , requestModifyDBProxy $
--             newModifyDBProxy
--
--         , requestDescribeGlobalClusters $
--             newDescribeGlobalClusters
--
--         , requestAddSourceIdentifierToSubscription $
--             newAddSourceIdentifierToSubscription
--
--         , requestModifyDBClusterEndpoint $
--             newModifyDBClusterEndpoint
--
--         , requestModifyEventSubscription $
--             newModifyEventSubscription
--
--         , requestCancelExportTask $
--             newCancelExportTask
--
--         , requestCreateDBParameterGroup $
--             newCreateDBParameterGroup
--
--         , requestModifyDBClusterSnapshotAttribute $
--             newModifyDBClusterSnapshotAttribute
--
--         , requestModifyDBProxyTargetGroup $
--             newModifyDBProxyTargetGroup
--
--         , requestDescribeDBSubnetGroups $
--             newDescribeDBSubnetGroups
--
--         , requestAddTagsToResource $
--             newAddTagsToResource
--
--         , requestModifyDBProxyEndpoint $
--             newModifyDBProxyEndpoint
--
--         , requestAddRoleToDBInstance $
--             newAddRoleToDBInstance
--
--         , requestRegisterDBProxyTargets $
--             newRegisterDBProxyTargets
--
--         , requestModifyCurrentDBClusterCapacity $
--             newModifyCurrentDBClusterCapacity
--
--         , requestDeleteDBParameterGroup $
--             newDeleteDBParameterGroup
--
--         , requestDownloadDBLogFilePortion $
--             newDownloadDBLogFilePortion
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
--         , requestCreateDBSnapshot $
--             newCreateDBSnapshot
--
--         , requestRestoreDBClusterFromSnapshot $
--             newRestoreDBClusterFromSnapshot
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
--         , requestCreateEventSubscription $
--             newCreateEventSubscription
--
--         , requestAuthorizeDBSecurityGroupIngress $
--             newAuthorizeDBSecurityGroupIngress
--
--         , requestRestoreDBInstanceFromDBSnapshot $
--             newRestoreDBInstanceFromDBSnapshot
--
--         , requestDescribeSourceRegions $
--             newDescribeSourceRegions
--
--         , requestDeleteDBSnapshot $
--             newDeleteDBSnapshot
--
--         , requestCreateDBClusterEndpoint $
--             newCreateDBClusterEndpoint
--
--         , requestDeleteDBProxy $
--             newDeleteDBProxy
--
--         , requestPurchaseReservedDBInstancesOffering $
--             newPurchaseReservedDBInstancesOffering
--
--         , requestDeleteDBClusterParameterGroup $
--             newDeleteDBClusterParameterGroup
--
--         , requestDeleteGlobalCluster $
--             newDeleteGlobalCluster
--
--         , requestModifyDBParameterGroup $
--             newModifyDBParameterGroup
--
--         , requestRevokeDBSecurityGroupIngress $
--             newRevokeDBSecurityGroupIngress
--
--         , requestPromoteReadReplicaDBCluster $
--             newPromoteReadReplicaDBCluster
--
--         , requestDeregisterDBProxyTargets $
--             newDeregisterDBProxyTargets
--
--         , requestFailoverGlobalCluster $
--             newFailoverGlobalCluster
--
--         , requestResetDBParameterGroup $
--             newResetDBParameterGroup
--
--         , requestCopyDBParameterGroup $
--             newCopyDBParameterGroup
--
--         , requestDescribeInstallationMedia $
--             newDescribeInstallationMedia
--
--         , requestCopyDBClusterSnapshot $
--             newCopyDBClusterSnapshot
--
--         , requestImportInstallationMedia $
--             newImportInstallationMedia
--
--         , requestDeleteCustomAvailabilityZone $
--             newDeleteCustomAvailabilityZone
--
--         , requestCreateGlobalCluster $
--             newCreateGlobalCluster
--
--         , requestDescribeDBProxyTargets $
--             newDescribeDBProxyTargets
--
--         , requestRemoveSourceIdentifierFromSubscription $
--             newRemoveSourceIdentifierFromSubscription
--
--         , requestDescribeCustomAvailabilityZones $
--             newDescribeCustomAvailabilityZones
--
--         , requestModifyDBSnapshot $
--             newModifyDBSnapshot
--
--         , requestAddRoleToDBCluster $
--             newAddRoleToDBCluster
--
--         , requestDescribeEngineDefaultParameters $
--             newDescribeEngineDefaultParameters
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestModifyDBSubnetGroup $
--             newModifyDBSubnetGroup
--
--         , requestRestoreDBInstanceToPointInTime $
--             newRestoreDBInstanceToPointInTime
--
--           ]

--     , testGroup "response"
--         [ responseDescribeExportTasks $
--             newDescribeExportTasksResponse
--
--         , responseModifyDBInstance $
--             newModifyDBInstanceResponse
--
--         , responseCopyDBSnapshot $
--             newCopyDBSnapshotResponse
--
--         , responseStopDBInstance $
--             newStopDBInstanceResponse
--
--         , responseDescribeEventCategories $
--             newDescribeEventCategoriesResponse
--
--         , responseCopyDBClusterParameterGroup $
--             newCopyDBClusterParameterGroupResponse
--
--         , responseResetDBClusterParameterGroup $
--             newDBClusterParameterGroupNameMessage
--
--         , responseDescribeDBProxyEndpoints $
--             newDescribeDBProxyEndpointsResponse
--
--         , responseStartDBInstance $
--             newStartDBInstanceResponse
--
--         , responseModifyDBCluster $
--             newModifyDBClusterResponse
--
--         , responseCreateDBClusterSnapshot $
--             newCreateDBClusterSnapshotResponse
--
--         , responseDescribeDBClusterParameterGroups $
--             newDescribeDBClusterParameterGroupsResponse
--
--         , responseModifyOptionGroup $
--             newModifyOptionGroupResponse
--
--         , responseStartDBCluster $
--             newStartDBClusterResponse
--
--         , responseDescribeDBSnapshots $
--             newDescribeDBSnapshotsResponse
--
--         , responseStopDBCluster $
--             newStopDBClusterResponse
--
--         , responsePromoteReadReplica $
--             newPromoteReadReplicaResponse
--
--         , responseDescribeDBEngineVersions $
--             newDescribeDBEngineVersionsResponse
--
--         , responseModifyGlobalCluster $
--             newModifyGlobalClusterResponse
--
--         , responseCreateDBInstanceReadReplica $
--             newCreateDBInstanceReadReplicaResponse
--
--         , responseStartActivityStream $
--             newStartActivityStreamResponse
--
--         , responseDescribeDBInstanceAutomatedBackups $
--             newDescribeDBInstanceAutomatedBackupsResponse
--
--         , responseDeleteInstallationMedia $
--             newInstallationMedia
--
--         , responseDescribeOptionGroupOptions $
--             newDescribeOptionGroupOptionsResponse
--
--         , responseRestoreDBInstanceFromS $
--             newRestoreDBInstanceFromSResponse
--
--         , responseDescribeDBParameters $
--             newDescribeDBParametersResponse
--
--         , responseCopyOptionGroup $
--             newCopyOptionGroupResponse
--
--         , responseStopActivityStream $
--             newStopActivityStreamResponse
--
--         , responseDescribeValidDBInstanceModifications $
--             newDescribeValidDBInstanceModificationsResponse
--
--         , responseStartExportTask $
--             newExportTask
--
--         , responseDescribeDBProxies $
--             newDescribeDBProxiesResponse
--
--         , responseRestoreDBClusterToPointInTime $
--             newRestoreDBClusterToPointInTimeResponse
--
--         , responseDescribeDBInstances $
--             newDescribeDBInstancesResponse
--
--         , responseDescribeDBClusterEndpoints $
--             newDescribeDBClusterEndpointsResponse
--
--         , responseDescribeEventSubscriptions $
--             newDescribeEventSubscriptionsResponse
--
--         , responseDeleteEventSubscription $
--             newDeleteEventSubscriptionResponse
--
--         , responseDescribeReservedDBInstancesOfferings $
--             newDescribeReservedDBInstancesOfferingsResponse
--
--         , responseRebootDBInstance $
--             newRebootDBInstanceResponse
--
--         , responseDescribeDBClusterBacktracks $
--             newDescribeDBClusterBacktracksResponse
--
--         , responseDescribeDBParameterGroups $
--             newDescribeDBParameterGroupsResponse
--
--         , responseCreateDBClusterParameterGroup $
--             newCreateDBClusterParameterGroupResponse
--
--         , responseDeleteDBInstance $
--             newDeleteDBInstanceResponse
--
--         , responseCreateDBProxy $
--             newCreateDBProxyResponse
--
--         , responseModifyCertificates $
--             newModifyCertificatesResponse
--
--         , responseDeleteDBInstanceAutomatedBackup $
--             newDeleteDBInstanceAutomatedBackupResponse
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
--         , responseDescribeAccountAttributes $
--             newDescribeAccountAttributesResponse
--
--         , responseRemoveRoleFromDBInstance $
--             newRemoveRoleFromDBInstanceResponse
--
--         , responseDescribeDBClusterParameters $
--             newDescribeDBClusterParametersResponse
--
--         , responseDeleteDBSubnetGroup $
--             newDeleteDBSubnetGroupResponse
--
--         , responseDescribeEngineDefaultClusterParameters $
--             newDescribeEngineDefaultClusterParametersResponse
--
--         , responseStopDBInstanceAutomatedBackupsReplication $
--             newStopDBInstanceAutomatedBackupsReplicationResponse
--
--         , responseBacktrackDBCluster $
--             newDBClusterBacktrack
--
--         , responseRemoveRoleFromDBCluster $
--             newRemoveRoleFromDBClusterResponse
--
--         , responseCreateDBCluster $
--             newCreateDBClusterResponse
--
--         , responseApplyPendingMaintenanceAction $
--             newApplyPendingMaintenanceActionResponse
--
--         , responseFailoverDBCluster $
--             newFailoverDBClusterResponse
--
--         , responseStartDBInstanceAutomatedBackupsReplication $
--             newStartDBInstanceAutomatedBackupsReplicationResponse
--
--         , responseCreateCustomAvailabilityZone $
--             newCreateCustomAvailabilityZoneResponse
--
--         , responseDeleteDBProxyEndpoint $
--             newDeleteDBProxyEndpointResponse
--
--         , responseDescribeDBSnapshotAttributes $
--             newDescribeDBSnapshotAttributesResponse
--
--         , responseCreateOptionGroup $
--             newCreateOptionGroupResponse
--
--         , responseDeleteDBCluster $
--             newDeleteDBClusterResponse
--
--         , responseCreateDBProxyEndpoint $
--             newCreateDBProxyEndpointResponse
--
--         , responseDescribeReservedDBInstances $
--             newDescribeReservedDBInstancesResponse
--
--         , responseDeleteOptionGroup $
--             newDeleteOptionGroupResponse
--
--         , responseDescribeEvents $
--             newDescribeEventsResponse
--
--         , responseDescribeDBProxyTargetGroups $
--             newDescribeDBProxyTargetGroupsResponse
--
--         , responseModifyDBClusterParameterGroup $
--             newDBClusterParameterGroupNameMessage
--
--         , responseDescribeDBLogFiles $
--             newDescribeDBLogFilesResponse
--
--         , responseDescribeOptionGroups $
--             newDescribeOptionGroupsResponse
--
--         , responseDescribePendingMaintenanceActions $
--             newDescribePendingMaintenanceActionsResponse
--
--         , responseRestoreDBClusterFromS $
--             newRestoreDBClusterFromSResponse
--
--         , responseDescribeDBClusters $
--             newDescribeDBClustersResponse
--
--         , responseDescribeDBClusterSnapshotAttributes $
--             newDescribeDBClusterSnapshotAttributesResponse
--
--         , responseModifyDBProxy $
--             newModifyDBProxyResponse
--
--         , responseDescribeGlobalClusters $
--             newDescribeGlobalClustersResponse
--
--         , responseAddSourceIdentifierToSubscription $
--             newAddSourceIdentifierToSubscriptionResponse
--
--         , responseModifyDBClusterEndpoint $
--             newDBClusterEndpoint
--
--         , responseModifyEventSubscription $
--             newModifyEventSubscriptionResponse
--
--         , responseCancelExportTask $
--             newExportTask
--
--         , responseCreateDBParameterGroup $
--             newCreateDBParameterGroupResponse
--
--         , responseModifyDBClusterSnapshotAttribute $
--             newModifyDBClusterSnapshotAttributeResponse
--
--         , responseModifyDBProxyTargetGroup $
--             newModifyDBProxyTargetGroupResponse
--
--         , responseDescribeDBSubnetGroups $
--             newDescribeDBSubnetGroupsResponse
--
--         , responseAddTagsToResource $
--             newAddTagsToResourceResponse
--
--         , responseModifyDBProxyEndpoint $
--             newModifyDBProxyEndpointResponse
--
--         , responseAddRoleToDBInstance $
--             newAddRoleToDBInstanceResponse
--
--         , responseRegisterDBProxyTargets $
--             newRegisterDBProxyTargetsResponse
--
--         , responseModifyCurrentDBClusterCapacity $
--             newModifyCurrentDBClusterCapacityResponse
--
--         , responseDeleteDBParameterGroup $
--             newDeleteDBParameterGroupResponse
--
--         , responseDownloadDBLogFilePortion $
--             newDownloadDBLogFilePortionResponse
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
--         , responseCreateDBSnapshot $
--             newCreateDBSnapshotResponse
--
--         , responseRestoreDBClusterFromSnapshot $
--             newRestoreDBClusterFromSnapshotResponse
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
--         , responseCreateEventSubscription $
--             newCreateEventSubscriptionResponse
--
--         , responseAuthorizeDBSecurityGroupIngress $
--             newAuthorizeDBSecurityGroupIngressResponse
--
--         , responseRestoreDBInstanceFromDBSnapshot $
--             newRestoreDBInstanceFromDBSnapshotResponse
--
--         , responseDescribeSourceRegions $
--             newDescribeSourceRegionsResponse
--
--         , responseDeleteDBSnapshot $
--             newDeleteDBSnapshotResponse
--
--         , responseCreateDBClusterEndpoint $
--             newDBClusterEndpoint
--
--         , responseDeleteDBProxy $
--             newDeleteDBProxyResponse
--
--         , responsePurchaseReservedDBInstancesOffering $
--             newPurchaseReservedDBInstancesOfferingResponse
--
--         , responseDeleteDBClusterParameterGroup $
--             newDeleteDBClusterParameterGroupResponse
--
--         , responseDeleteGlobalCluster $
--             newDeleteGlobalClusterResponse
--
--         , responseModifyDBParameterGroup $
--             newDBParameterGroupNameMessage
--
--         , responseRevokeDBSecurityGroupIngress $
--             newRevokeDBSecurityGroupIngressResponse
--
--         , responsePromoteReadReplicaDBCluster $
--             newPromoteReadReplicaDBClusterResponse
--
--         , responseDeregisterDBProxyTargets $
--             newDeregisterDBProxyTargetsResponse
--
--         , responseFailoverGlobalCluster $
--             newFailoverGlobalClusterResponse
--
--         , responseResetDBParameterGroup $
--             newDBParameterGroupNameMessage
--
--         , responseCopyDBParameterGroup $
--             newCopyDBParameterGroupResponse
--
--         , responseDescribeInstallationMedia $
--             newDescribeInstallationMediaResponse
--
--         , responseCopyDBClusterSnapshot $
--             newCopyDBClusterSnapshotResponse
--
--         , responseImportInstallationMedia $
--             newInstallationMedia
--
--         , responseDeleteCustomAvailabilityZone $
--             newDeleteCustomAvailabilityZoneResponse
--
--         , responseCreateGlobalCluster $
--             newCreateGlobalClusterResponse
--
--         , responseDescribeDBProxyTargets $
--             newDescribeDBProxyTargetsResponse
--
--         , responseRemoveSourceIdentifierFromSubscription $
--             newRemoveSourceIdentifierFromSubscriptionResponse
--
--         , responseDescribeCustomAvailabilityZones $
--             newDescribeCustomAvailabilityZonesResponse
--
--         , responseModifyDBSnapshot $
--             newModifyDBSnapshotResponse
--
--         , responseAddRoleToDBCluster $
--             newAddRoleToDBClusterResponse
--
--         , responseDescribeEngineDefaultParameters $
--             newDescribeEngineDefaultParametersResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseModifyDBSubnetGroup $
--             newModifyDBSubnetGroupResponse
--
--         , responseRestoreDBInstanceToPointInTime $
--             newRestoreDBInstanceToPointInTimeResponse
--
--           ]
--     ]

-- Requests

requestDescribeExportTasks :: DescribeExportTasks -> TestTree
requestDescribeExportTasks =
  req
    "DescribeExportTasks"
    "fixture/DescribeExportTasks.yaml"

requestModifyDBInstance :: ModifyDBInstance -> TestTree
requestModifyDBInstance =
  req
    "ModifyDBInstance"
    "fixture/ModifyDBInstance.yaml"

requestCopyDBSnapshot :: CopyDBSnapshot -> TestTree
requestCopyDBSnapshot =
  req
    "CopyDBSnapshot"
    "fixture/CopyDBSnapshot.yaml"

requestStopDBInstance :: StopDBInstance -> TestTree
requestStopDBInstance =
  req
    "StopDBInstance"
    "fixture/StopDBInstance.yaml"

requestDescribeEventCategories :: DescribeEventCategories -> TestTree
requestDescribeEventCategories =
  req
    "DescribeEventCategories"
    "fixture/DescribeEventCategories.yaml"

requestCopyDBClusterParameterGroup :: CopyDBClusterParameterGroup -> TestTree
requestCopyDBClusterParameterGroup =
  req
    "CopyDBClusterParameterGroup"
    "fixture/CopyDBClusterParameterGroup.yaml"

requestResetDBClusterParameterGroup :: ResetDBClusterParameterGroup -> TestTree
requestResetDBClusterParameterGroup =
  req
    "ResetDBClusterParameterGroup"
    "fixture/ResetDBClusterParameterGroup.yaml"

requestDescribeDBProxyEndpoints :: DescribeDBProxyEndpoints -> TestTree
requestDescribeDBProxyEndpoints =
  req
    "DescribeDBProxyEndpoints"
    "fixture/DescribeDBProxyEndpoints.yaml"

requestStartDBInstance :: StartDBInstance -> TestTree
requestStartDBInstance =
  req
    "StartDBInstance"
    "fixture/StartDBInstance.yaml"

requestModifyDBCluster :: ModifyDBCluster -> TestTree
requestModifyDBCluster =
  req
    "ModifyDBCluster"
    "fixture/ModifyDBCluster.yaml"

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

requestModifyOptionGroup :: ModifyOptionGroup -> TestTree
requestModifyOptionGroup =
  req
    "ModifyOptionGroup"
    "fixture/ModifyOptionGroup.yaml"

requestStartDBCluster :: StartDBCluster -> TestTree
requestStartDBCluster =
  req
    "StartDBCluster"
    "fixture/StartDBCluster.yaml"

requestDescribeDBSnapshots :: DescribeDBSnapshots -> TestTree
requestDescribeDBSnapshots =
  req
    "DescribeDBSnapshots"
    "fixture/DescribeDBSnapshots.yaml"

requestStopDBCluster :: StopDBCluster -> TestTree
requestStopDBCluster =
  req
    "StopDBCluster"
    "fixture/StopDBCluster.yaml"

requestPromoteReadReplica :: PromoteReadReplica -> TestTree
requestPromoteReadReplica =
  req
    "PromoteReadReplica"
    "fixture/PromoteReadReplica.yaml"

requestDescribeDBEngineVersions :: DescribeDBEngineVersions -> TestTree
requestDescribeDBEngineVersions =
  req
    "DescribeDBEngineVersions"
    "fixture/DescribeDBEngineVersions.yaml"

requestModifyGlobalCluster :: ModifyGlobalCluster -> TestTree
requestModifyGlobalCluster =
  req
    "ModifyGlobalCluster"
    "fixture/ModifyGlobalCluster.yaml"

requestCreateDBInstanceReadReplica :: CreateDBInstanceReadReplica -> TestTree
requestCreateDBInstanceReadReplica =
  req
    "CreateDBInstanceReadReplica"
    "fixture/CreateDBInstanceReadReplica.yaml"

requestStartActivityStream :: StartActivityStream -> TestTree
requestStartActivityStream =
  req
    "StartActivityStream"
    "fixture/StartActivityStream.yaml"

requestDescribeDBInstanceAutomatedBackups :: DescribeDBInstanceAutomatedBackups -> TestTree
requestDescribeDBInstanceAutomatedBackups =
  req
    "DescribeDBInstanceAutomatedBackups"
    "fixture/DescribeDBInstanceAutomatedBackups.yaml"

requestDeleteInstallationMedia :: DeleteInstallationMedia -> TestTree
requestDeleteInstallationMedia =
  req
    "DeleteInstallationMedia"
    "fixture/DeleteInstallationMedia.yaml"

requestDescribeOptionGroupOptions :: DescribeOptionGroupOptions -> TestTree
requestDescribeOptionGroupOptions =
  req
    "DescribeOptionGroupOptions"
    "fixture/DescribeOptionGroupOptions.yaml"

requestRestoreDBInstanceFromS :: RestoreDBInstanceFromS -> TestTree
requestRestoreDBInstanceFromS =
  req
    "RestoreDBInstanceFromS"
    "fixture/RestoreDBInstanceFromS.yaml"

requestDescribeDBParameters :: DescribeDBParameters -> TestTree
requestDescribeDBParameters =
  req
    "DescribeDBParameters"
    "fixture/DescribeDBParameters.yaml"

requestCopyOptionGroup :: CopyOptionGroup -> TestTree
requestCopyOptionGroup =
  req
    "CopyOptionGroup"
    "fixture/CopyOptionGroup.yaml"

requestStopActivityStream :: StopActivityStream -> TestTree
requestStopActivityStream =
  req
    "StopActivityStream"
    "fixture/StopActivityStream.yaml"

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

requestDescribeDBInstances :: DescribeDBInstances -> TestTree
requestDescribeDBInstances =
  req
    "DescribeDBInstances"
    "fixture/DescribeDBInstances.yaml"

requestDescribeDBClusterEndpoints :: DescribeDBClusterEndpoints -> TestTree
requestDescribeDBClusterEndpoints =
  req
    "DescribeDBClusterEndpoints"
    "fixture/DescribeDBClusterEndpoints.yaml"

requestDescribeEventSubscriptions :: DescribeEventSubscriptions -> TestTree
requestDescribeEventSubscriptions =
  req
    "DescribeEventSubscriptions"
    "fixture/DescribeEventSubscriptions.yaml"

requestDeleteEventSubscription :: DeleteEventSubscription -> TestTree
requestDeleteEventSubscription =
  req
    "DeleteEventSubscription"
    "fixture/DeleteEventSubscription.yaml"

requestDescribeReservedDBInstancesOfferings :: DescribeReservedDBInstancesOfferings -> TestTree
requestDescribeReservedDBInstancesOfferings =
  req
    "DescribeReservedDBInstancesOfferings"
    "fixture/DescribeReservedDBInstancesOfferings.yaml"

requestRebootDBInstance :: RebootDBInstance -> TestTree
requestRebootDBInstance =
  req
    "RebootDBInstance"
    "fixture/RebootDBInstance.yaml"

requestDescribeDBClusterBacktracks :: DescribeDBClusterBacktracks -> TestTree
requestDescribeDBClusterBacktracks =
  req
    "DescribeDBClusterBacktracks"
    "fixture/DescribeDBClusterBacktracks.yaml"

requestDescribeDBParameterGroups :: DescribeDBParameterGroups -> TestTree
requestDescribeDBParameterGroups =
  req
    "DescribeDBParameterGroups"
    "fixture/DescribeDBParameterGroups.yaml"

requestCreateDBClusterParameterGroup :: CreateDBClusterParameterGroup -> TestTree
requestCreateDBClusterParameterGroup =
  req
    "CreateDBClusterParameterGroup"
    "fixture/CreateDBClusterParameterGroup.yaml"

requestDeleteDBInstance :: DeleteDBInstance -> TestTree
requestDeleteDBInstance =
  req
    "DeleteDBInstance"
    "fixture/DeleteDBInstance.yaml"

requestCreateDBProxy :: CreateDBProxy -> TestTree
requestCreateDBProxy =
  req
    "CreateDBProxy"
    "fixture/CreateDBProxy.yaml"

requestModifyCertificates :: ModifyCertificates -> TestTree
requestModifyCertificates =
  req
    "ModifyCertificates"
    "fixture/ModifyCertificates.yaml"

requestDeleteDBInstanceAutomatedBackup :: DeleteDBInstanceAutomatedBackup -> TestTree
requestDeleteDBInstanceAutomatedBackup =
  req
    "DeleteDBInstanceAutomatedBackup"
    "fixture/DeleteDBInstanceAutomatedBackup.yaml"

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

requestDescribeAccountAttributes :: DescribeAccountAttributes -> TestTree
requestDescribeAccountAttributes =
  req
    "DescribeAccountAttributes"
    "fixture/DescribeAccountAttributes.yaml"

requestRemoveRoleFromDBInstance :: RemoveRoleFromDBInstance -> TestTree
requestRemoveRoleFromDBInstance =
  req
    "RemoveRoleFromDBInstance"
    "fixture/RemoveRoleFromDBInstance.yaml"

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

requestDescribeEngineDefaultClusterParameters :: DescribeEngineDefaultClusterParameters -> TestTree
requestDescribeEngineDefaultClusterParameters =
  req
    "DescribeEngineDefaultClusterParameters"
    "fixture/DescribeEngineDefaultClusterParameters.yaml"

requestStopDBInstanceAutomatedBackupsReplication :: StopDBInstanceAutomatedBackupsReplication -> TestTree
requestStopDBInstanceAutomatedBackupsReplication =
  req
    "StopDBInstanceAutomatedBackupsReplication"
    "fixture/StopDBInstanceAutomatedBackupsReplication.yaml"

requestBacktrackDBCluster :: BacktrackDBCluster -> TestTree
requestBacktrackDBCluster =
  req
    "BacktrackDBCluster"
    "fixture/BacktrackDBCluster.yaml"

requestRemoveRoleFromDBCluster :: RemoveRoleFromDBCluster -> TestTree
requestRemoveRoleFromDBCluster =
  req
    "RemoveRoleFromDBCluster"
    "fixture/RemoveRoleFromDBCluster.yaml"

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

requestFailoverDBCluster :: FailoverDBCluster -> TestTree
requestFailoverDBCluster =
  req
    "FailoverDBCluster"
    "fixture/FailoverDBCluster.yaml"

requestStartDBInstanceAutomatedBackupsReplication :: StartDBInstanceAutomatedBackupsReplication -> TestTree
requestStartDBInstanceAutomatedBackupsReplication =
  req
    "StartDBInstanceAutomatedBackupsReplication"
    "fixture/StartDBInstanceAutomatedBackupsReplication.yaml"

requestCreateCustomAvailabilityZone :: CreateCustomAvailabilityZone -> TestTree
requestCreateCustomAvailabilityZone =
  req
    "CreateCustomAvailabilityZone"
    "fixture/CreateCustomAvailabilityZone.yaml"

requestDeleteDBProxyEndpoint :: DeleteDBProxyEndpoint -> TestTree
requestDeleteDBProxyEndpoint =
  req
    "DeleteDBProxyEndpoint"
    "fixture/DeleteDBProxyEndpoint.yaml"

requestDescribeDBSnapshotAttributes :: DescribeDBSnapshotAttributes -> TestTree
requestDescribeDBSnapshotAttributes =
  req
    "DescribeDBSnapshotAttributes"
    "fixture/DescribeDBSnapshotAttributes.yaml"

requestCreateOptionGroup :: CreateOptionGroup -> TestTree
requestCreateOptionGroup =
  req
    "CreateOptionGroup"
    "fixture/CreateOptionGroup.yaml"

requestDeleteDBCluster :: DeleteDBCluster -> TestTree
requestDeleteDBCluster =
  req
    "DeleteDBCluster"
    "fixture/DeleteDBCluster.yaml"

requestCreateDBProxyEndpoint :: CreateDBProxyEndpoint -> TestTree
requestCreateDBProxyEndpoint =
  req
    "CreateDBProxyEndpoint"
    "fixture/CreateDBProxyEndpoint.yaml"

requestDescribeReservedDBInstances :: DescribeReservedDBInstances -> TestTree
requestDescribeReservedDBInstances =
  req
    "DescribeReservedDBInstances"
    "fixture/DescribeReservedDBInstances.yaml"

requestDeleteOptionGroup :: DeleteOptionGroup -> TestTree
requestDeleteOptionGroup =
  req
    "DeleteOptionGroup"
    "fixture/DeleteOptionGroup.yaml"

requestDescribeEvents :: DescribeEvents -> TestTree
requestDescribeEvents =
  req
    "DescribeEvents"
    "fixture/DescribeEvents.yaml"

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

requestDescribeDBLogFiles :: DescribeDBLogFiles -> TestTree
requestDescribeDBLogFiles =
  req
    "DescribeDBLogFiles"
    "fixture/DescribeDBLogFiles.yaml"

requestDescribeOptionGroups :: DescribeOptionGroups -> TestTree
requestDescribeOptionGroups =
  req
    "DescribeOptionGroups"
    "fixture/DescribeOptionGroups.yaml"

requestDescribePendingMaintenanceActions :: DescribePendingMaintenanceActions -> TestTree
requestDescribePendingMaintenanceActions =
  req
    "DescribePendingMaintenanceActions"
    "fixture/DescribePendingMaintenanceActions.yaml"

requestRestoreDBClusterFromS :: RestoreDBClusterFromS -> TestTree
requestRestoreDBClusterFromS =
  req
    "RestoreDBClusterFromS"
    "fixture/RestoreDBClusterFromS.yaml"

requestDescribeDBClusters :: DescribeDBClusters -> TestTree
requestDescribeDBClusters =
  req
    "DescribeDBClusters"
    "fixture/DescribeDBClusters.yaml"

requestDescribeDBClusterSnapshotAttributes :: DescribeDBClusterSnapshotAttributes -> TestTree
requestDescribeDBClusterSnapshotAttributes =
  req
    "DescribeDBClusterSnapshotAttributes"
    "fixture/DescribeDBClusterSnapshotAttributes.yaml"

requestModifyDBProxy :: ModifyDBProxy -> TestTree
requestModifyDBProxy =
  req
    "ModifyDBProxy"
    "fixture/ModifyDBProxy.yaml"

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

requestModifyDBClusterEndpoint :: ModifyDBClusterEndpoint -> TestTree
requestModifyDBClusterEndpoint =
  req
    "ModifyDBClusterEndpoint"
    "fixture/ModifyDBClusterEndpoint.yaml"

requestModifyEventSubscription :: ModifyEventSubscription -> TestTree
requestModifyEventSubscription =
  req
    "ModifyEventSubscription"
    "fixture/ModifyEventSubscription.yaml"

requestCancelExportTask :: CancelExportTask -> TestTree
requestCancelExportTask =
  req
    "CancelExportTask"
    "fixture/CancelExportTask.yaml"

requestCreateDBParameterGroup :: CreateDBParameterGroup -> TestTree
requestCreateDBParameterGroup =
  req
    "CreateDBParameterGroup"
    "fixture/CreateDBParameterGroup.yaml"

requestModifyDBClusterSnapshotAttribute :: ModifyDBClusterSnapshotAttribute -> TestTree
requestModifyDBClusterSnapshotAttribute =
  req
    "ModifyDBClusterSnapshotAttribute"
    "fixture/ModifyDBClusterSnapshotAttribute.yaml"

requestModifyDBProxyTargetGroup :: ModifyDBProxyTargetGroup -> TestTree
requestModifyDBProxyTargetGroup =
  req
    "ModifyDBProxyTargetGroup"
    "fixture/ModifyDBProxyTargetGroup.yaml"

requestDescribeDBSubnetGroups :: DescribeDBSubnetGroups -> TestTree
requestDescribeDBSubnetGroups =
  req
    "DescribeDBSubnetGroups"
    "fixture/DescribeDBSubnetGroups.yaml"

requestAddTagsToResource :: AddTagsToResource -> TestTree
requestAddTagsToResource =
  req
    "AddTagsToResource"
    "fixture/AddTagsToResource.yaml"

requestModifyDBProxyEndpoint :: ModifyDBProxyEndpoint -> TestTree
requestModifyDBProxyEndpoint =
  req
    "ModifyDBProxyEndpoint"
    "fixture/ModifyDBProxyEndpoint.yaml"

requestAddRoleToDBInstance :: AddRoleToDBInstance -> TestTree
requestAddRoleToDBInstance =
  req
    "AddRoleToDBInstance"
    "fixture/AddRoleToDBInstance.yaml"

requestRegisterDBProxyTargets :: RegisterDBProxyTargets -> TestTree
requestRegisterDBProxyTargets =
  req
    "RegisterDBProxyTargets"
    "fixture/RegisterDBProxyTargets.yaml"

requestModifyCurrentDBClusterCapacity :: ModifyCurrentDBClusterCapacity -> TestTree
requestModifyCurrentDBClusterCapacity =
  req
    "ModifyCurrentDBClusterCapacity"
    "fixture/ModifyCurrentDBClusterCapacity.yaml"

requestDeleteDBParameterGroup :: DeleteDBParameterGroup -> TestTree
requestDeleteDBParameterGroup =
  req
    "DeleteDBParameterGroup"
    "fixture/DeleteDBParameterGroup.yaml"

requestDownloadDBLogFilePortion :: DownloadDBLogFilePortion -> TestTree
requestDownloadDBLogFilePortion =
  req
    "DownloadDBLogFilePortion"
    "fixture/DownloadDBLogFilePortion.yaml"

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

requestCreateDBSnapshot :: CreateDBSnapshot -> TestTree
requestCreateDBSnapshot =
  req
    "CreateDBSnapshot"
    "fixture/CreateDBSnapshot.yaml"

requestRestoreDBClusterFromSnapshot :: RestoreDBClusterFromSnapshot -> TestTree
requestRestoreDBClusterFromSnapshot =
  req
    "RestoreDBClusterFromSnapshot"
    "fixture/RestoreDBClusterFromSnapshot.yaml"

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

requestCreateEventSubscription :: CreateEventSubscription -> TestTree
requestCreateEventSubscription =
  req
    "CreateEventSubscription"
    "fixture/CreateEventSubscription.yaml"

requestAuthorizeDBSecurityGroupIngress :: AuthorizeDBSecurityGroupIngress -> TestTree
requestAuthorizeDBSecurityGroupIngress =
  req
    "AuthorizeDBSecurityGroupIngress"
    "fixture/AuthorizeDBSecurityGroupIngress.yaml"

requestRestoreDBInstanceFromDBSnapshot :: RestoreDBInstanceFromDBSnapshot -> TestTree
requestRestoreDBInstanceFromDBSnapshot =
  req
    "RestoreDBInstanceFromDBSnapshot"
    "fixture/RestoreDBInstanceFromDBSnapshot.yaml"

requestDescribeSourceRegions :: DescribeSourceRegions -> TestTree
requestDescribeSourceRegions =
  req
    "DescribeSourceRegions"
    "fixture/DescribeSourceRegions.yaml"

requestDeleteDBSnapshot :: DeleteDBSnapshot -> TestTree
requestDeleteDBSnapshot =
  req
    "DeleteDBSnapshot"
    "fixture/DeleteDBSnapshot.yaml"

requestCreateDBClusterEndpoint :: CreateDBClusterEndpoint -> TestTree
requestCreateDBClusterEndpoint =
  req
    "CreateDBClusterEndpoint"
    "fixture/CreateDBClusterEndpoint.yaml"

requestDeleteDBProxy :: DeleteDBProxy -> TestTree
requestDeleteDBProxy =
  req
    "DeleteDBProxy"
    "fixture/DeleteDBProxy.yaml"

requestPurchaseReservedDBInstancesOffering :: PurchaseReservedDBInstancesOffering -> TestTree
requestPurchaseReservedDBInstancesOffering =
  req
    "PurchaseReservedDBInstancesOffering"
    "fixture/PurchaseReservedDBInstancesOffering.yaml"

requestDeleteDBClusterParameterGroup :: DeleteDBClusterParameterGroup -> TestTree
requestDeleteDBClusterParameterGroup =
  req
    "DeleteDBClusterParameterGroup"
    "fixture/DeleteDBClusterParameterGroup.yaml"

requestDeleteGlobalCluster :: DeleteGlobalCluster -> TestTree
requestDeleteGlobalCluster =
  req
    "DeleteGlobalCluster"
    "fixture/DeleteGlobalCluster.yaml"

requestModifyDBParameterGroup :: ModifyDBParameterGroup -> TestTree
requestModifyDBParameterGroup =
  req
    "ModifyDBParameterGroup"
    "fixture/ModifyDBParameterGroup.yaml"

requestRevokeDBSecurityGroupIngress :: RevokeDBSecurityGroupIngress -> TestTree
requestRevokeDBSecurityGroupIngress =
  req
    "RevokeDBSecurityGroupIngress"
    "fixture/RevokeDBSecurityGroupIngress.yaml"

requestPromoteReadReplicaDBCluster :: PromoteReadReplicaDBCluster -> TestTree
requestPromoteReadReplicaDBCluster =
  req
    "PromoteReadReplicaDBCluster"
    "fixture/PromoteReadReplicaDBCluster.yaml"

requestDeregisterDBProxyTargets :: DeregisterDBProxyTargets -> TestTree
requestDeregisterDBProxyTargets =
  req
    "DeregisterDBProxyTargets"
    "fixture/DeregisterDBProxyTargets.yaml"

requestFailoverGlobalCluster :: FailoverGlobalCluster -> TestTree
requestFailoverGlobalCluster =
  req
    "FailoverGlobalCluster"
    "fixture/FailoverGlobalCluster.yaml"

requestResetDBParameterGroup :: ResetDBParameterGroup -> TestTree
requestResetDBParameterGroup =
  req
    "ResetDBParameterGroup"
    "fixture/ResetDBParameterGroup.yaml"

requestCopyDBParameterGroup :: CopyDBParameterGroup -> TestTree
requestCopyDBParameterGroup =
  req
    "CopyDBParameterGroup"
    "fixture/CopyDBParameterGroup.yaml"

requestDescribeInstallationMedia :: DescribeInstallationMedia -> TestTree
requestDescribeInstallationMedia =
  req
    "DescribeInstallationMedia"
    "fixture/DescribeInstallationMedia.yaml"

requestCopyDBClusterSnapshot :: CopyDBClusterSnapshot -> TestTree
requestCopyDBClusterSnapshot =
  req
    "CopyDBClusterSnapshot"
    "fixture/CopyDBClusterSnapshot.yaml"

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

requestCreateGlobalCluster :: CreateGlobalCluster -> TestTree
requestCreateGlobalCluster =
  req
    "CreateGlobalCluster"
    "fixture/CreateGlobalCluster.yaml"

requestDescribeDBProxyTargets :: DescribeDBProxyTargets -> TestTree
requestDescribeDBProxyTargets =
  req
    "DescribeDBProxyTargets"
    "fixture/DescribeDBProxyTargets.yaml"

requestRemoveSourceIdentifierFromSubscription :: RemoveSourceIdentifierFromSubscription -> TestTree
requestRemoveSourceIdentifierFromSubscription =
  req
    "RemoveSourceIdentifierFromSubscription"
    "fixture/RemoveSourceIdentifierFromSubscription.yaml"

requestDescribeCustomAvailabilityZones :: DescribeCustomAvailabilityZones -> TestTree
requestDescribeCustomAvailabilityZones =
  req
    "DescribeCustomAvailabilityZones"
    "fixture/DescribeCustomAvailabilityZones.yaml"

requestModifyDBSnapshot :: ModifyDBSnapshot -> TestTree
requestModifyDBSnapshot =
  req
    "ModifyDBSnapshot"
    "fixture/ModifyDBSnapshot.yaml"

requestAddRoleToDBCluster :: AddRoleToDBCluster -> TestTree
requestAddRoleToDBCluster =
  req
    "AddRoleToDBCluster"
    "fixture/AddRoleToDBCluster.yaml"

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

requestModifyDBSubnetGroup :: ModifyDBSubnetGroup -> TestTree
requestModifyDBSubnetGroup =
  req
    "ModifyDBSubnetGroup"
    "fixture/ModifyDBSubnetGroup.yaml"

requestRestoreDBInstanceToPointInTime :: RestoreDBInstanceToPointInTime -> TestTree
requestRestoreDBInstanceToPointInTime =
  req
    "RestoreDBInstanceToPointInTime"
    "fixture/RestoreDBInstanceToPointInTime.yaml"

-- Responses

responseDescribeExportTasks :: DescribeExportTasksResponse -> TestTree
responseDescribeExportTasks =
  res
    "DescribeExportTasksResponse"
    "fixture/DescribeExportTasksResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeExportTasks)

responseModifyDBInstance :: ModifyDBInstanceResponse -> TestTree
responseModifyDBInstance =
  res
    "ModifyDBInstanceResponse"
    "fixture/ModifyDBInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyDBInstance)

responseCopyDBSnapshot :: CopyDBSnapshotResponse -> TestTree
responseCopyDBSnapshot =
  res
    "CopyDBSnapshotResponse"
    "fixture/CopyDBSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy CopyDBSnapshot)

responseStopDBInstance :: StopDBInstanceResponse -> TestTree
responseStopDBInstance =
  res
    "StopDBInstanceResponse"
    "fixture/StopDBInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy StopDBInstance)

responseDescribeEventCategories :: DescribeEventCategoriesResponse -> TestTree
responseDescribeEventCategories =
  res
    "DescribeEventCategoriesResponse"
    "fixture/DescribeEventCategoriesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEventCategories)

responseCopyDBClusterParameterGroup :: CopyDBClusterParameterGroupResponse -> TestTree
responseCopyDBClusterParameterGroup =
  res
    "CopyDBClusterParameterGroupResponse"
    "fixture/CopyDBClusterParameterGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CopyDBClusterParameterGroup)

responseResetDBClusterParameterGroup :: DBClusterParameterGroupNameMessage -> TestTree
responseResetDBClusterParameterGroup =
  res
    "ResetDBClusterParameterGroupResponse"
    "fixture/ResetDBClusterParameterGroupResponse.proto"
    defaultService
    (Proxy :: Proxy ResetDBClusterParameterGroup)

responseDescribeDBProxyEndpoints :: DescribeDBProxyEndpointsResponse -> TestTree
responseDescribeDBProxyEndpoints =
  res
    "DescribeDBProxyEndpointsResponse"
    "fixture/DescribeDBProxyEndpointsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDBProxyEndpoints)

responseStartDBInstance :: StartDBInstanceResponse -> TestTree
responseStartDBInstance =
  res
    "StartDBInstanceResponse"
    "fixture/StartDBInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy StartDBInstance)

responseModifyDBCluster :: ModifyDBClusterResponse -> TestTree
responseModifyDBCluster =
  res
    "ModifyDBClusterResponse"
    "fixture/ModifyDBClusterResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyDBCluster)

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

responseModifyOptionGroup :: ModifyOptionGroupResponse -> TestTree
responseModifyOptionGroup =
  res
    "ModifyOptionGroupResponse"
    "fixture/ModifyOptionGroupResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyOptionGroup)

responseStartDBCluster :: StartDBClusterResponse -> TestTree
responseStartDBCluster =
  res
    "StartDBClusterResponse"
    "fixture/StartDBClusterResponse.proto"
    defaultService
    (Proxy :: Proxy StartDBCluster)

responseDescribeDBSnapshots :: DescribeDBSnapshotsResponse -> TestTree
responseDescribeDBSnapshots =
  res
    "DescribeDBSnapshotsResponse"
    "fixture/DescribeDBSnapshotsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDBSnapshots)

responseStopDBCluster :: StopDBClusterResponse -> TestTree
responseStopDBCluster =
  res
    "StopDBClusterResponse"
    "fixture/StopDBClusterResponse.proto"
    defaultService
    (Proxy :: Proxy StopDBCluster)

responsePromoteReadReplica :: PromoteReadReplicaResponse -> TestTree
responsePromoteReadReplica =
  res
    "PromoteReadReplicaResponse"
    "fixture/PromoteReadReplicaResponse.proto"
    defaultService
    (Proxy :: Proxy PromoteReadReplica)

responseDescribeDBEngineVersions :: DescribeDBEngineVersionsResponse -> TestTree
responseDescribeDBEngineVersions =
  res
    "DescribeDBEngineVersionsResponse"
    "fixture/DescribeDBEngineVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDBEngineVersions)

responseModifyGlobalCluster :: ModifyGlobalClusterResponse -> TestTree
responseModifyGlobalCluster =
  res
    "ModifyGlobalClusterResponse"
    "fixture/ModifyGlobalClusterResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyGlobalCluster)

responseCreateDBInstanceReadReplica :: CreateDBInstanceReadReplicaResponse -> TestTree
responseCreateDBInstanceReadReplica =
  res
    "CreateDBInstanceReadReplicaResponse"
    "fixture/CreateDBInstanceReadReplicaResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDBInstanceReadReplica)

responseStartActivityStream :: StartActivityStreamResponse -> TestTree
responseStartActivityStream =
  res
    "StartActivityStreamResponse"
    "fixture/StartActivityStreamResponse.proto"
    defaultService
    (Proxy :: Proxy StartActivityStream)

responseDescribeDBInstanceAutomatedBackups :: DescribeDBInstanceAutomatedBackupsResponse -> TestTree
responseDescribeDBInstanceAutomatedBackups =
  res
    "DescribeDBInstanceAutomatedBackupsResponse"
    "fixture/DescribeDBInstanceAutomatedBackupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDBInstanceAutomatedBackups)

responseDeleteInstallationMedia :: InstallationMedia -> TestTree
responseDeleteInstallationMedia =
  res
    "DeleteInstallationMediaResponse"
    "fixture/DeleteInstallationMediaResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteInstallationMedia)

responseDescribeOptionGroupOptions :: DescribeOptionGroupOptionsResponse -> TestTree
responseDescribeOptionGroupOptions =
  res
    "DescribeOptionGroupOptionsResponse"
    "fixture/DescribeOptionGroupOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeOptionGroupOptions)

responseRestoreDBInstanceFromS :: RestoreDBInstanceFromSResponse -> TestTree
responseRestoreDBInstanceFromS =
  res
    "RestoreDBInstanceFromSResponse"
    "fixture/RestoreDBInstanceFromSResponse.proto"
    defaultService
    (Proxy :: Proxy RestoreDBInstanceFromS)

responseDescribeDBParameters :: DescribeDBParametersResponse -> TestTree
responseDescribeDBParameters =
  res
    "DescribeDBParametersResponse"
    "fixture/DescribeDBParametersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDBParameters)

responseCopyOptionGroup :: CopyOptionGroupResponse -> TestTree
responseCopyOptionGroup =
  res
    "CopyOptionGroupResponse"
    "fixture/CopyOptionGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CopyOptionGroup)

responseStopActivityStream :: StopActivityStreamResponse -> TestTree
responseStopActivityStream =
  res
    "StopActivityStreamResponse"
    "fixture/StopActivityStreamResponse.proto"
    defaultService
    (Proxy :: Proxy StopActivityStream)

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

responseDescribeDBInstances :: DescribeDBInstancesResponse -> TestTree
responseDescribeDBInstances =
  res
    "DescribeDBInstancesResponse"
    "fixture/DescribeDBInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDBInstances)

responseDescribeDBClusterEndpoints :: DescribeDBClusterEndpointsResponse -> TestTree
responseDescribeDBClusterEndpoints =
  res
    "DescribeDBClusterEndpointsResponse"
    "fixture/DescribeDBClusterEndpointsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDBClusterEndpoints)

responseDescribeEventSubscriptions :: DescribeEventSubscriptionsResponse -> TestTree
responseDescribeEventSubscriptions =
  res
    "DescribeEventSubscriptionsResponse"
    "fixture/DescribeEventSubscriptionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEventSubscriptions)

responseDeleteEventSubscription :: DeleteEventSubscriptionResponse -> TestTree
responseDeleteEventSubscription =
  res
    "DeleteEventSubscriptionResponse"
    "fixture/DeleteEventSubscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteEventSubscription)

responseDescribeReservedDBInstancesOfferings :: DescribeReservedDBInstancesOfferingsResponse -> TestTree
responseDescribeReservedDBInstancesOfferings =
  res
    "DescribeReservedDBInstancesOfferingsResponse"
    "fixture/DescribeReservedDBInstancesOfferingsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeReservedDBInstancesOfferings)

responseRebootDBInstance :: RebootDBInstanceResponse -> TestTree
responseRebootDBInstance =
  res
    "RebootDBInstanceResponse"
    "fixture/RebootDBInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy RebootDBInstance)

responseDescribeDBClusterBacktracks :: DescribeDBClusterBacktracksResponse -> TestTree
responseDescribeDBClusterBacktracks =
  res
    "DescribeDBClusterBacktracksResponse"
    "fixture/DescribeDBClusterBacktracksResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDBClusterBacktracks)

responseDescribeDBParameterGroups :: DescribeDBParameterGroupsResponse -> TestTree
responseDescribeDBParameterGroups =
  res
    "DescribeDBParameterGroupsResponse"
    "fixture/DescribeDBParameterGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDBParameterGroups)

responseCreateDBClusterParameterGroup :: CreateDBClusterParameterGroupResponse -> TestTree
responseCreateDBClusterParameterGroup =
  res
    "CreateDBClusterParameterGroupResponse"
    "fixture/CreateDBClusterParameterGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDBClusterParameterGroup)

responseDeleteDBInstance :: DeleteDBInstanceResponse -> TestTree
responseDeleteDBInstance =
  res
    "DeleteDBInstanceResponse"
    "fixture/DeleteDBInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDBInstance)

responseCreateDBProxy :: CreateDBProxyResponse -> TestTree
responseCreateDBProxy =
  res
    "CreateDBProxyResponse"
    "fixture/CreateDBProxyResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDBProxy)

responseModifyCertificates :: ModifyCertificatesResponse -> TestTree
responseModifyCertificates =
  res
    "ModifyCertificatesResponse"
    "fixture/ModifyCertificatesResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyCertificates)

responseDeleteDBInstanceAutomatedBackup :: DeleteDBInstanceAutomatedBackupResponse -> TestTree
responseDeleteDBInstanceAutomatedBackup =
  res
    "DeleteDBInstanceAutomatedBackupResponse"
    "fixture/DeleteDBInstanceAutomatedBackupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDBInstanceAutomatedBackup)

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

responseDescribeAccountAttributes :: DescribeAccountAttributesResponse -> TestTree
responseDescribeAccountAttributes =
  res
    "DescribeAccountAttributesResponse"
    "fixture/DescribeAccountAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAccountAttributes)

responseRemoveRoleFromDBInstance :: RemoveRoleFromDBInstanceResponse -> TestTree
responseRemoveRoleFromDBInstance =
  res
    "RemoveRoleFromDBInstanceResponse"
    "fixture/RemoveRoleFromDBInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveRoleFromDBInstance)

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

responseDescribeEngineDefaultClusterParameters :: DescribeEngineDefaultClusterParametersResponse -> TestTree
responseDescribeEngineDefaultClusterParameters =
  res
    "DescribeEngineDefaultClusterParametersResponse"
    "fixture/DescribeEngineDefaultClusterParametersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEngineDefaultClusterParameters)

responseStopDBInstanceAutomatedBackupsReplication :: StopDBInstanceAutomatedBackupsReplicationResponse -> TestTree
responseStopDBInstanceAutomatedBackupsReplication =
  res
    "StopDBInstanceAutomatedBackupsReplicationResponse"
    "fixture/StopDBInstanceAutomatedBackupsReplicationResponse.proto"
    defaultService
    (Proxy :: Proxy StopDBInstanceAutomatedBackupsReplication)

responseBacktrackDBCluster :: DBClusterBacktrack -> TestTree
responseBacktrackDBCluster =
  res
    "BacktrackDBClusterResponse"
    "fixture/BacktrackDBClusterResponse.proto"
    defaultService
    (Proxy :: Proxy BacktrackDBCluster)

responseRemoveRoleFromDBCluster :: RemoveRoleFromDBClusterResponse -> TestTree
responseRemoveRoleFromDBCluster =
  res
    "RemoveRoleFromDBClusterResponse"
    "fixture/RemoveRoleFromDBClusterResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveRoleFromDBCluster)

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

responseFailoverDBCluster :: FailoverDBClusterResponse -> TestTree
responseFailoverDBCluster =
  res
    "FailoverDBClusterResponse"
    "fixture/FailoverDBClusterResponse.proto"
    defaultService
    (Proxy :: Proxy FailoverDBCluster)

responseStartDBInstanceAutomatedBackupsReplication :: StartDBInstanceAutomatedBackupsReplicationResponse -> TestTree
responseStartDBInstanceAutomatedBackupsReplication =
  res
    "StartDBInstanceAutomatedBackupsReplicationResponse"
    "fixture/StartDBInstanceAutomatedBackupsReplicationResponse.proto"
    defaultService
    (Proxy :: Proxy StartDBInstanceAutomatedBackupsReplication)

responseCreateCustomAvailabilityZone :: CreateCustomAvailabilityZoneResponse -> TestTree
responseCreateCustomAvailabilityZone =
  res
    "CreateCustomAvailabilityZoneResponse"
    "fixture/CreateCustomAvailabilityZoneResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCustomAvailabilityZone)

responseDeleteDBProxyEndpoint :: DeleteDBProxyEndpointResponse -> TestTree
responseDeleteDBProxyEndpoint =
  res
    "DeleteDBProxyEndpointResponse"
    "fixture/DeleteDBProxyEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDBProxyEndpoint)

responseDescribeDBSnapshotAttributes :: DescribeDBSnapshotAttributesResponse -> TestTree
responseDescribeDBSnapshotAttributes =
  res
    "DescribeDBSnapshotAttributesResponse"
    "fixture/DescribeDBSnapshotAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDBSnapshotAttributes)

responseCreateOptionGroup :: CreateOptionGroupResponse -> TestTree
responseCreateOptionGroup =
  res
    "CreateOptionGroupResponse"
    "fixture/CreateOptionGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateOptionGroup)

responseDeleteDBCluster :: DeleteDBClusterResponse -> TestTree
responseDeleteDBCluster =
  res
    "DeleteDBClusterResponse"
    "fixture/DeleteDBClusterResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDBCluster)

responseCreateDBProxyEndpoint :: CreateDBProxyEndpointResponse -> TestTree
responseCreateDBProxyEndpoint =
  res
    "CreateDBProxyEndpointResponse"
    "fixture/CreateDBProxyEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDBProxyEndpoint)

responseDescribeReservedDBInstances :: DescribeReservedDBInstancesResponse -> TestTree
responseDescribeReservedDBInstances =
  res
    "DescribeReservedDBInstancesResponse"
    "fixture/DescribeReservedDBInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeReservedDBInstances)

responseDeleteOptionGroup :: DeleteOptionGroupResponse -> TestTree
responseDeleteOptionGroup =
  res
    "DeleteOptionGroupResponse"
    "fixture/DeleteOptionGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteOptionGroup)

responseDescribeEvents :: DescribeEventsResponse -> TestTree
responseDescribeEvents =
  res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEvents)

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

responseDescribeDBLogFiles :: DescribeDBLogFilesResponse -> TestTree
responseDescribeDBLogFiles =
  res
    "DescribeDBLogFilesResponse"
    "fixture/DescribeDBLogFilesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDBLogFiles)

responseDescribeOptionGroups :: DescribeOptionGroupsResponse -> TestTree
responseDescribeOptionGroups =
  res
    "DescribeOptionGroupsResponse"
    "fixture/DescribeOptionGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeOptionGroups)

responseDescribePendingMaintenanceActions :: DescribePendingMaintenanceActionsResponse -> TestTree
responseDescribePendingMaintenanceActions =
  res
    "DescribePendingMaintenanceActionsResponse"
    "fixture/DescribePendingMaintenanceActionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePendingMaintenanceActions)

responseRestoreDBClusterFromS :: RestoreDBClusterFromSResponse -> TestTree
responseRestoreDBClusterFromS =
  res
    "RestoreDBClusterFromSResponse"
    "fixture/RestoreDBClusterFromSResponse.proto"
    defaultService
    (Proxy :: Proxy RestoreDBClusterFromS)

responseDescribeDBClusters :: DescribeDBClustersResponse -> TestTree
responseDescribeDBClusters =
  res
    "DescribeDBClustersResponse"
    "fixture/DescribeDBClustersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDBClusters)

responseDescribeDBClusterSnapshotAttributes :: DescribeDBClusterSnapshotAttributesResponse -> TestTree
responseDescribeDBClusterSnapshotAttributes =
  res
    "DescribeDBClusterSnapshotAttributesResponse"
    "fixture/DescribeDBClusterSnapshotAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDBClusterSnapshotAttributes)

responseModifyDBProxy :: ModifyDBProxyResponse -> TestTree
responseModifyDBProxy =
  res
    "ModifyDBProxyResponse"
    "fixture/ModifyDBProxyResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyDBProxy)

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

responseModifyDBClusterEndpoint :: DBClusterEndpoint -> TestTree
responseModifyDBClusterEndpoint =
  res
    "ModifyDBClusterEndpointResponse"
    "fixture/ModifyDBClusterEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyDBClusterEndpoint)

responseModifyEventSubscription :: ModifyEventSubscriptionResponse -> TestTree
responseModifyEventSubscription =
  res
    "ModifyEventSubscriptionResponse"
    "fixture/ModifyEventSubscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyEventSubscription)

responseCancelExportTask :: ExportTask -> TestTree
responseCancelExportTask =
  res
    "CancelExportTaskResponse"
    "fixture/CancelExportTaskResponse.proto"
    defaultService
    (Proxy :: Proxy CancelExportTask)

responseCreateDBParameterGroup :: CreateDBParameterGroupResponse -> TestTree
responseCreateDBParameterGroup =
  res
    "CreateDBParameterGroupResponse"
    "fixture/CreateDBParameterGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDBParameterGroup)

responseModifyDBClusterSnapshotAttribute :: ModifyDBClusterSnapshotAttributeResponse -> TestTree
responseModifyDBClusterSnapshotAttribute =
  res
    "ModifyDBClusterSnapshotAttributeResponse"
    "fixture/ModifyDBClusterSnapshotAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyDBClusterSnapshotAttribute)

responseModifyDBProxyTargetGroup :: ModifyDBProxyTargetGroupResponse -> TestTree
responseModifyDBProxyTargetGroup =
  res
    "ModifyDBProxyTargetGroupResponse"
    "fixture/ModifyDBProxyTargetGroupResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyDBProxyTargetGroup)

responseDescribeDBSubnetGroups :: DescribeDBSubnetGroupsResponse -> TestTree
responseDescribeDBSubnetGroups =
  res
    "DescribeDBSubnetGroupsResponse"
    "fixture/DescribeDBSubnetGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDBSubnetGroups)

responseAddTagsToResource :: AddTagsToResourceResponse -> TestTree
responseAddTagsToResource =
  res
    "AddTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse.proto"
    defaultService
    (Proxy :: Proxy AddTagsToResource)

responseModifyDBProxyEndpoint :: ModifyDBProxyEndpointResponse -> TestTree
responseModifyDBProxyEndpoint =
  res
    "ModifyDBProxyEndpointResponse"
    "fixture/ModifyDBProxyEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyDBProxyEndpoint)

responseAddRoleToDBInstance :: AddRoleToDBInstanceResponse -> TestTree
responseAddRoleToDBInstance =
  res
    "AddRoleToDBInstanceResponse"
    "fixture/AddRoleToDBInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy AddRoleToDBInstance)

responseRegisterDBProxyTargets :: RegisterDBProxyTargetsResponse -> TestTree
responseRegisterDBProxyTargets =
  res
    "RegisterDBProxyTargetsResponse"
    "fixture/RegisterDBProxyTargetsResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterDBProxyTargets)

responseModifyCurrentDBClusterCapacity :: ModifyCurrentDBClusterCapacityResponse -> TestTree
responseModifyCurrentDBClusterCapacity =
  res
    "ModifyCurrentDBClusterCapacityResponse"
    "fixture/ModifyCurrentDBClusterCapacityResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyCurrentDBClusterCapacity)

responseDeleteDBParameterGroup :: DeleteDBParameterGroupResponse -> TestTree
responseDeleteDBParameterGroup =
  res
    "DeleteDBParameterGroupResponse"
    "fixture/DeleteDBParameterGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDBParameterGroup)

responseDownloadDBLogFilePortion :: DownloadDBLogFilePortionResponse -> TestTree
responseDownloadDBLogFilePortion =
  res
    "DownloadDBLogFilePortionResponse"
    "fixture/DownloadDBLogFilePortionResponse.proto"
    defaultService
    (Proxy :: Proxy DownloadDBLogFilePortion)

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

responseCreateDBSnapshot :: CreateDBSnapshotResponse -> TestTree
responseCreateDBSnapshot =
  res
    "CreateDBSnapshotResponse"
    "fixture/CreateDBSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDBSnapshot)

responseRestoreDBClusterFromSnapshot :: RestoreDBClusterFromSnapshotResponse -> TestTree
responseRestoreDBClusterFromSnapshot =
  res
    "RestoreDBClusterFromSnapshotResponse"
    "fixture/RestoreDBClusterFromSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy RestoreDBClusterFromSnapshot)

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

responseCreateEventSubscription :: CreateEventSubscriptionResponse -> TestTree
responseCreateEventSubscription =
  res
    "CreateEventSubscriptionResponse"
    "fixture/CreateEventSubscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateEventSubscription)

responseAuthorizeDBSecurityGroupIngress :: AuthorizeDBSecurityGroupIngressResponse -> TestTree
responseAuthorizeDBSecurityGroupIngress =
  res
    "AuthorizeDBSecurityGroupIngressResponse"
    "fixture/AuthorizeDBSecurityGroupIngressResponse.proto"
    defaultService
    (Proxy :: Proxy AuthorizeDBSecurityGroupIngress)

responseRestoreDBInstanceFromDBSnapshot :: RestoreDBInstanceFromDBSnapshotResponse -> TestTree
responseRestoreDBInstanceFromDBSnapshot =
  res
    "RestoreDBInstanceFromDBSnapshotResponse"
    "fixture/RestoreDBInstanceFromDBSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy RestoreDBInstanceFromDBSnapshot)

responseDescribeSourceRegions :: DescribeSourceRegionsResponse -> TestTree
responseDescribeSourceRegions =
  res
    "DescribeSourceRegionsResponse"
    "fixture/DescribeSourceRegionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSourceRegions)

responseDeleteDBSnapshot :: DeleteDBSnapshotResponse -> TestTree
responseDeleteDBSnapshot =
  res
    "DeleteDBSnapshotResponse"
    "fixture/DeleteDBSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDBSnapshot)

responseCreateDBClusterEndpoint :: DBClusterEndpoint -> TestTree
responseCreateDBClusterEndpoint =
  res
    "CreateDBClusterEndpointResponse"
    "fixture/CreateDBClusterEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDBClusterEndpoint)

responseDeleteDBProxy :: DeleteDBProxyResponse -> TestTree
responseDeleteDBProxy =
  res
    "DeleteDBProxyResponse"
    "fixture/DeleteDBProxyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDBProxy)

responsePurchaseReservedDBInstancesOffering :: PurchaseReservedDBInstancesOfferingResponse -> TestTree
responsePurchaseReservedDBInstancesOffering =
  res
    "PurchaseReservedDBInstancesOfferingResponse"
    "fixture/PurchaseReservedDBInstancesOfferingResponse.proto"
    defaultService
    (Proxy :: Proxy PurchaseReservedDBInstancesOffering)

responseDeleteDBClusterParameterGroup :: DeleteDBClusterParameterGroupResponse -> TestTree
responseDeleteDBClusterParameterGroup =
  res
    "DeleteDBClusterParameterGroupResponse"
    "fixture/DeleteDBClusterParameterGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDBClusterParameterGroup)

responseDeleteGlobalCluster :: DeleteGlobalClusterResponse -> TestTree
responseDeleteGlobalCluster =
  res
    "DeleteGlobalClusterResponse"
    "fixture/DeleteGlobalClusterResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteGlobalCluster)

responseModifyDBParameterGroup :: DBParameterGroupNameMessage -> TestTree
responseModifyDBParameterGroup =
  res
    "ModifyDBParameterGroupResponse"
    "fixture/ModifyDBParameterGroupResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyDBParameterGroup)

responseRevokeDBSecurityGroupIngress :: RevokeDBSecurityGroupIngressResponse -> TestTree
responseRevokeDBSecurityGroupIngress =
  res
    "RevokeDBSecurityGroupIngressResponse"
    "fixture/RevokeDBSecurityGroupIngressResponse.proto"
    defaultService
    (Proxy :: Proxy RevokeDBSecurityGroupIngress)

responsePromoteReadReplicaDBCluster :: PromoteReadReplicaDBClusterResponse -> TestTree
responsePromoteReadReplicaDBCluster =
  res
    "PromoteReadReplicaDBClusterResponse"
    "fixture/PromoteReadReplicaDBClusterResponse.proto"
    defaultService
    (Proxy :: Proxy PromoteReadReplicaDBCluster)

responseDeregisterDBProxyTargets :: DeregisterDBProxyTargetsResponse -> TestTree
responseDeregisterDBProxyTargets =
  res
    "DeregisterDBProxyTargetsResponse"
    "fixture/DeregisterDBProxyTargetsResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterDBProxyTargets)

responseFailoverGlobalCluster :: FailoverGlobalClusterResponse -> TestTree
responseFailoverGlobalCluster =
  res
    "FailoverGlobalClusterResponse"
    "fixture/FailoverGlobalClusterResponse.proto"
    defaultService
    (Proxy :: Proxy FailoverGlobalCluster)

responseResetDBParameterGroup :: DBParameterGroupNameMessage -> TestTree
responseResetDBParameterGroup =
  res
    "ResetDBParameterGroupResponse"
    "fixture/ResetDBParameterGroupResponse.proto"
    defaultService
    (Proxy :: Proxy ResetDBParameterGroup)

responseCopyDBParameterGroup :: CopyDBParameterGroupResponse -> TestTree
responseCopyDBParameterGroup =
  res
    "CopyDBParameterGroupResponse"
    "fixture/CopyDBParameterGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CopyDBParameterGroup)

responseDescribeInstallationMedia :: DescribeInstallationMediaResponse -> TestTree
responseDescribeInstallationMedia =
  res
    "DescribeInstallationMediaResponse"
    "fixture/DescribeInstallationMediaResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInstallationMedia)

responseCopyDBClusterSnapshot :: CopyDBClusterSnapshotResponse -> TestTree
responseCopyDBClusterSnapshot =
  res
    "CopyDBClusterSnapshotResponse"
    "fixture/CopyDBClusterSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy CopyDBClusterSnapshot)

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

responseCreateGlobalCluster :: CreateGlobalClusterResponse -> TestTree
responseCreateGlobalCluster =
  res
    "CreateGlobalClusterResponse"
    "fixture/CreateGlobalClusterResponse.proto"
    defaultService
    (Proxy :: Proxy CreateGlobalCluster)

responseDescribeDBProxyTargets :: DescribeDBProxyTargetsResponse -> TestTree
responseDescribeDBProxyTargets =
  res
    "DescribeDBProxyTargetsResponse"
    "fixture/DescribeDBProxyTargetsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDBProxyTargets)

responseRemoveSourceIdentifierFromSubscription :: RemoveSourceIdentifierFromSubscriptionResponse -> TestTree
responseRemoveSourceIdentifierFromSubscription =
  res
    "RemoveSourceIdentifierFromSubscriptionResponse"
    "fixture/RemoveSourceIdentifierFromSubscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveSourceIdentifierFromSubscription)

responseDescribeCustomAvailabilityZones :: DescribeCustomAvailabilityZonesResponse -> TestTree
responseDescribeCustomAvailabilityZones =
  res
    "DescribeCustomAvailabilityZonesResponse"
    "fixture/DescribeCustomAvailabilityZonesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCustomAvailabilityZones)

responseModifyDBSnapshot :: ModifyDBSnapshotResponse -> TestTree
responseModifyDBSnapshot =
  res
    "ModifyDBSnapshotResponse"
    "fixture/ModifyDBSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyDBSnapshot)

responseAddRoleToDBCluster :: AddRoleToDBClusterResponse -> TestTree
responseAddRoleToDBCluster =
  res
    "AddRoleToDBClusterResponse"
    "fixture/AddRoleToDBClusterResponse.proto"
    defaultService
    (Proxy :: Proxy AddRoleToDBCluster)

responseDescribeEngineDefaultParameters :: DescribeEngineDefaultParametersResponse -> TestTree
responseDescribeEngineDefaultParameters =
  res
    "DescribeEngineDefaultParametersResponse"
    "fixture/DescribeEngineDefaultParametersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEngineDefaultParameters)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseModifyDBSubnetGroup :: ModifyDBSubnetGroupResponse -> TestTree
responseModifyDBSubnetGroup =
  res
    "ModifyDBSubnetGroupResponse"
    "fixture/ModifyDBSubnetGroupResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyDBSubnetGroup)

responseRestoreDBInstanceToPointInTime :: RestoreDBInstanceToPointInTimeResponse -> TestTree
responseRestoreDBInstanceToPointInTime =
  res
    "RestoreDBInstanceToPointInTimeResponse"
    "fixture/RestoreDBInstanceToPointInTimeResponse.proto"
    defaultService
    (Proxy :: Proxy RestoreDBInstanceToPointInTime)
