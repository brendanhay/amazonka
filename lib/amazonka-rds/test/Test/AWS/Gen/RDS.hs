{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.RDS
-- Copyright   : (c) 2013-2020 Brendan Hay
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
--         [ requestStartDBCluster $
--             startDBCluster
--
--         , requestDescribeDBClusterParameterGroups $
--             describeDBClusterParameterGroups
--
--         , requestPromoteReadReplica $
--             promoteReadReplica
--
--         , requestDescribeDBEngineVersions $
--             describeDBEngineVersions
--
--         , requestStopDBInstance $
--             stopDBInstance
--
--         , requestModifyDBClusterEndpoint $
--             modifyDBClusterEndpoint
--
--         , requestCopyDBSnapshot $
--             copyDBSnapshot
--
--         , requestAddSourceIdentifierToSubscription $
--             addSourceIdentifierToSubscription
--
--         , requestModifyDBInstance $
--             modifyDBInstance
--
--         , requestModifyEventSubscription $
--             modifyEventSubscription
--
--         , requestResetDBClusterParameterGroup $
--             resetDBClusterParameterGroup
--
--         , requestDescribeCustomAvailabilityZones $
--             describeCustomAvailabilityZones
--
--         , requestRestoreDBClusterFromS3 $
--             restoreDBClusterFromS3
--
--         , requestDescribeEvents $
--             describeEvents
--
--         , requestDescribeEngineDefaultParameters $
--             describeEngineDefaultParameters
--
--         , requestDescribeOptionGroups $
--             describeOptionGroups
--
--         , requestDescribeDBLogFiles $
--             describeDBLogFiles
--
--         , requestDescribeDBClusters $
--             describeDBClusters
--
--         , requestModifyDBSubnetGroup $
--             modifyDBSubnetGroup
--
--         , requestListTagsForResource $
--             listTagsForResource
--
--         , requestDeleteOptionGroup $
--             deleteOptionGroup
--
--         , requestDeleteDBCluster $
--             deleteDBCluster
--
--         , requestDescribeReservedDBInstances $
--             describeReservedDBInstances
--
--         , requestCopyDBParameterGroup $
--             copyDBParameterGroup
--
--         , requestRemoveSourceIdentifierFromSubscription $
--             removeSourceIdentifierFromSubscription
--
--         , requestDeleteCustomAvailabilityZone $
--             deleteCustomAvailabilityZone
--
--         , requestDescribeDBProxyTargets $
--             describeDBProxyTargets
--
--         , requestDescribeEngineDefaultClusterParameters $
--             describeEngineDefaultClusterParameters
--
--         , requestDescribeDBSnapshotAttributes $
--             describeDBSnapshotAttributes
--
--         , requestCreateCustomAvailabilityZone $
--             createCustomAvailabilityZone
--
--         , requestBacktrackDBCluster $
--             backtrackDBCluster
--
--         , requestDeleteGlobalCluster $
--             deleteGlobalCluster
--
--         , requestPromoteReadReplicaDBCluster $
--             promoteReadReplicaDBCluster
--
--         , requestRemoveTagsFromResource $
--             removeTagsFromResource
--
--         , requestRestoreDBInstanceFromDBSnapshot $
--             restoreDBInstanceFromDBSnapshot
--
--         , requestDeleteDBProxy $
--             deleteDBProxy
--
--         , requestCreateEventSubscription $
--             createEventSubscription
--
--         , requestPurchaseReservedDBInstancesOffering $
--             purchaseReservedDBInstancesOffering
--
--         , requestCreateDBInstance $
--             createDBInstance
--
--         , requestDeleteDBClusterParameterGroup $
--             deleteDBClusterParameterGroup
--
--         , requestDescribeCertificates $
--             describeCertificates
--
--         , requestAuthorizeDBSecurityGroupIngress $
--             authorizeDBSecurityGroupIngress
--
--         , requestRemoveRoleFromDBInstance $
--             removeRoleFromDBInstance
--
--         , requestDescribeSourceRegions $
--             describeSourceRegions
--
--         , requestCreateDBClusterEndpoint $
--             createDBClusterEndpoint
--
--         , requestRestoreDBClusterFromSnapshot $
--             restoreDBClusterFromSnapshot
--
--         , requestDescribeOrderableDBInstanceOptions $
--             describeOrderableDBInstanceOptions
--
--         , requestDeleteDBClusterEndpoint $
--             deleteDBClusterEndpoint
--
--         , requestCreateDBProxy $
--             createDBProxy
--
--         , requestDeleteDBInstanceAutomatedBackup $
--             deleteDBInstanceAutomatedBackup
--
--         , requestCreateDBClusterParameterGroup $
--             createDBClusterParameterGroup
--
--         , requestCreateDBSnapshot $
--             createDBSnapshot
--
--         , requestDeleteEventSubscription $
--             deleteEventSubscription
--
--         , requestDescribeDBClusterBacktracks $
--             describeDBClusterBacktracks
--
--         , requestDescribeDBParameterGroups $
--             describeDBParameterGroups
--
--         , requestModifyDBSnapshotAttribute $
--             modifyDBSnapshotAttribute
--
--         , requestDescribeDBInstanceAutomatedBackups $
--             describeDBInstanceAutomatedBackups
--
--         , requestRemoveFromGlobalCluster $
--             removeFromGlobalCluster
--
--         , requestAddRoleToDBInstance $
--             addRoleToDBInstance
--
--         , requestDeleteDBClusterSnapshot $
--             deleteDBClusterSnapshot
--
--         , requestDescribeValidDBInstanceModifications $
--             describeValidDBInstanceModifications
--
--         , requestDescribeDBClusterEndpoints $
--             describeDBClusterEndpoints
--
--         , requestDescribeOptionGroupOptions $
--             describeOptionGroupOptions
--
--         , requestDescribeEventSubscriptions $
--             describeEventSubscriptions
--
--         , requestAddTagsToResource $
--             addTagsToResource
--
--         , requestDescribeDBParameters $
--             describeDBParameters
--
--         , requestStopActivityStream $
--             stopActivityStream
--
--         , requestCreateDBClusterSnapshot $
--             createDBClusterSnapshot
--
--         , requestDescribeDBSnapshots $
--             describeDBSnapshots
--
--         , requestModifyDBProxyTargetGroup $
--             modifyDBProxyTargetGroup
--
--         , requestDescribeDBSubnetGroups $
--             describeDBSubnetGroups
--
--         , requestModifyOptionGroup $
--             modifyOptionGroup
--
--         , requestStopDBCluster $
--             stopDBCluster
--
--         , requestCreateDBParameterGroup $
--             createDBParameterGroup
--
--         , requestModifyDBClusterSnapshotAttribute $
--             modifyDBClusterSnapshotAttribute
--
--         , requestModifyDBCluster $
--             modifyDBCluster
--
--         , requestCopyDBClusterParameterGroup $
--             copyDBClusterParameterGroup
--
--         , requestDescribeEventCategories $
--             describeEventCategories
--
--         , requestDescribeGlobalClusters $
--             describeGlobalClusters
--
--         , requestStartDBInstance $
--             startDBInstance
--
--         , requestDescribeExportTasks $
--             describeExportTasks
--
--         , requestCancelExportTask $
--             cancelExportTask
--
--         , requestModifyDBClusterParameterGroup $
--             modifyDBClusterParameterGroup
--
--         , requestRestoreDBInstanceToPointInTime $
--             restoreDBInstanceToPointInTime
--
--         , requestDescribeDBClusterSnapshotAttributes $
--             describeDBClusterSnapshotAttributes
--
--         , requestModifyDBSnapshot $
--             modifyDBSnapshot
--
--         , requestDescribeDBProxyTargetGroups $
--             describeDBProxyTargetGroups
--
--         , requestModifyDBProxy $
--             modifyDBProxy
--
--         , requestDescribePendingMaintenanceActions $
--             describePendingMaintenanceActions
--
--         , requestAddRoleToDBCluster $
--             addRoleToDBCluster
--
--         , requestCopyDBClusterSnapshot $
--             copyDBClusterSnapshot
--
--         , requestImportInstallationMedia $
--             importInstallationMedia
--
--         , requestCreateGlobalCluster $
--             createGlobalCluster
--
--         , requestResetDBParameterGroup $
--             resetDBParameterGroup
--
--         , requestDescribeInstallationMedia $
--             describeInstallationMedia
--
--         , requestDeregisterDBProxyTargets $
--             deregisterDBProxyTargets
--
--         , requestCreateDBCluster $
--             createDBCluster
--
--         , requestRemoveRoleFromDBCluster $
--             removeRoleFromDBCluster
--
--         , requestFailoverDBCluster $
--             failoverDBCluster
--
--         , requestRevokeDBSecurityGroupIngress $
--             revokeDBSecurityGroupIngress
--
--         , requestModifyDBParameterGroup $
--             modifyDBParameterGroup
--
--         , requestApplyPendingMaintenanceAction $
--             applyPendingMaintenanceAction
--
--         , requestCreateOptionGroup $
--             createOptionGroup
--
--         , requestDescribeAccountAttributes $
--             describeAccountAttributes
--
--         , requestDeleteDBSnapshot $
--             deleteDBSnapshot
--
--         , requestDescribeDBClusterParameters $
--             describeDBClusterParameters
--
--         , requestDeleteDBSubnetGroup $
--             deleteDBSubnetGroup
--
--         , requestCreateDBSecurityGroup $
--             createDBSecurityGroup
--
--         , requestModifyCertificates $
--             modifyCertificates
--
--         , requestDescribeDBClusterSnapshots $
--             describeDBClusterSnapshots
--
--         , requestRebootDBInstance $
--             rebootDBInstance
--
--         , requestCreateDBSubnetGroup $
--             createDBSubnetGroup
--
--         , requestDescribeReservedDBInstancesOfferings $
--             describeReservedDBInstancesOfferings
--
--         , requestDeleteDBSecurityGroup $
--             deleteDBSecurityGroup
--
--         , requestDeleteDBInstance $
--             deleteDBInstance
--
--         , requestStartActivityStream $
--             startActivityStream
--
--         , requestCreateDBInstanceReadReplica $
--             createDBInstanceReadReplica
--
--         , requestDeleteDBParameterGroup $
--             deleteDBParameterGroup
--
--         , requestModifyCurrentDBClusterCapacity $
--             modifyCurrentDBClusterCapacity
--
--         , requestModifyGlobalCluster $
--             modifyGlobalCluster
--
--         , requestRegisterDBProxyTargets $
--             registerDBProxyTargets
--
--         , requestDescribeDBSecurityGroups $
--             describeDBSecurityGroups
--
--         , requestCopyOptionGroup $
--             copyOptionGroup
--
--         , requestRestoreDBClusterToPointInTime $
--             restoreDBClusterToPointInTime
--
--         , requestDeleteInstallationMedia $
--             deleteInstallationMedia
--
--         , requestDescribeDBInstances $
--             describeDBInstances
--
--         , requestRestoreDBInstanceFromS3 $
--             restoreDBInstanceFromS3
--
--         , requestDownloadDBLogFilePortion $
--             downloadDBLogFilePortion
--
--         , requestDescribeDBProxies $
--             describeDBProxies
--
--         , requestStartExportTask $
--             startExportTask
--
--           ]

--     , testGroup "response"
--         [ responseStartDBCluster $
--             startDBClusterResponse
--
--         , responseDescribeDBClusterParameterGroups $
--             describeDBClusterParameterGroupsResponse
--
--         , responsePromoteReadReplica $
--             promoteReadReplicaResponse
--
--         , responseDescribeDBEngineVersions $
--             describeDBEngineVersionsResponse
--
--         , responseStopDBInstance $
--             stopDBInstanceResponse
--
--         , responseModifyDBClusterEndpoint $
--             dbClusterEndpoint
--
--         , responseCopyDBSnapshot $
--             copyDBSnapshotResponse
--
--         , responseAddSourceIdentifierToSubscription $
--             addSourceIdentifierToSubscriptionResponse
--
--         , responseModifyDBInstance $
--             modifyDBInstanceResponse
--
--         , responseModifyEventSubscription $
--             modifyEventSubscriptionResponse
--
--         , responseResetDBClusterParameterGroup $
--             dbClusterParameterGroupNameMessage
--
--         , responseDescribeCustomAvailabilityZones $
--             describeCustomAvailabilityZonesResponse
--
--         , responseRestoreDBClusterFromS3 $
--             restoreDBClusterFromS3Response
--
--         , responseDescribeEvents $
--             describeEventsResponse
--
--         , responseDescribeEngineDefaultParameters $
--             describeEngineDefaultParametersResponse
--
--         , responseDescribeOptionGroups $
--             describeOptionGroupsResponse
--
--         , responseDescribeDBLogFiles $
--             describeDBLogFilesResponse
--
--         , responseDescribeDBClusters $
--             describeDBClustersResponse
--
--         , responseModifyDBSubnetGroup $
--             modifyDBSubnetGroupResponse
--
--         , responseListTagsForResource $
--             listTagsForResourceResponse
--
--         , responseDeleteOptionGroup $
--             deleteOptionGroupResponse
--
--         , responseDeleteDBCluster $
--             deleteDBClusterResponse
--
--         , responseDescribeReservedDBInstances $
--             describeReservedDBInstancesResponse
--
--         , responseCopyDBParameterGroup $
--             copyDBParameterGroupResponse
--
--         , responseRemoveSourceIdentifierFromSubscription $
--             removeSourceIdentifierFromSubscriptionResponse
--
--         , responseDeleteCustomAvailabilityZone $
--             deleteCustomAvailabilityZoneResponse
--
--         , responseDescribeDBProxyTargets $
--             describeDBProxyTargetsResponse
--
--         , responseDescribeEngineDefaultClusterParameters $
--             describeEngineDefaultClusterParametersResponse
--
--         , responseDescribeDBSnapshotAttributes $
--             describeDBSnapshotAttributesResponse
--
--         , responseCreateCustomAvailabilityZone $
--             createCustomAvailabilityZoneResponse
--
--         , responseBacktrackDBCluster $
--             dbClusterBacktrack
--
--         , responseDeleteGlobalCluster $
--             deleteGlobalClusterResponse
--
--         , responsePromoteReadReplicaDBCluster $
--             promoteReadReplicaDBClusterResponse
--
--         , responseRemoveTagsFromResource $
--             removeTagsFromResourceResponse
--
--         , responseRestoreDBInstanceFromDBSnapshot $
--             restoreDBInstanceFromDBSnapshotResponse
--
--         , responseDeleteDBProxy $
--             deleteDBProxyResponse
--
--         , responseCreateEventSubscription $
--             createEventSubscriptionResponse
--
--         , responsePurchaseReservedDBInstancesOffering $
--             purchaseReservedDBInstancesOfferingResponse
--
--         , responseCreateDBInstance $
--             createDBInstanceResponse
--
--         , responseDeleteDBClusterParameterGroup $
--             deleteDBClusterParameterGroupResponse
--
--         , responseDescribeCertificates $
--             describeCertificatesResponse
--
--         , responseAuthorizeDBSecurityGroupIngress $
--             authorizeDBSecurityGroupIngressResponse
--
--         , responseRemoveRoleFromDBInstance $
--             removeRoleFromDBInstanceResponse
--
--         , responseDescribeSourceRegions $
--             describeSourceRegionsResponse
--
--         , responseCreateDBClusterEndpoint $
--             dbClusterEndpoint
--
--         , responseRestoreDBClusterFromSnapshot $
--             restoreDBClusterFromSnapshotResponse
--
--         , responseDescribeOrderableDBInstanceOptions $
--             describeOrderableDBInstanceOptionsResponse
--
--         , responseDeleteDBClusterEndpoint $
--             dbClusterEndpoint
--
--         , responseCreateDBProxy $
--             createDBProxyResponse
--
--         , responseDeleteDBInstanceAutomatedBackup $
--             deleteDBInstanceAutomatedBackupResponse
--
--         , responseCreateDBClusterParameterGroup $
--             createDBClusterParameterGroupResponse
--
--         , responseCreateDBSnapshot $
--             createDBSnapshotResponse
--
--         , responseDeleteEventSubscription $
--             deleteEventSubscriptionResponse
--
--         , responseDescribeDBClusterBacktracks $
--             describeDBClusterBacktracksResponse
--
--         , responseDescribeDBParameterGroups $
--             describeDBParameterGroupsResponse
--
--         , responseModifyDBSnapshotAttribute $
--             modifyDBSnapshotAttributeResponse
--
--         , responseDescribeDBInstanceAutomatedBackups $
--             describeDBInstanceAutomatedBackupsResponse
--
--         , responseRemoveFromGlobalCluster $
--             removeFromGlobalClusterResponse
--
--         , responseAddRoleToDBInstance $
--             addRoleToDBInstanceResponse
--
--         , responseDeleteDBClusterSnapshot $
--             deleteDBClusterSnapshotResponse
--
--         , responseDescribeValidDBInstanceModifications $
--             describeValidDBInstanceModificationsResponse
--
--         , responseDescribeDBClusterEndpoints $
--             describeDBClusterEndpointsResponse
--
--         , responseDescribeOptionGroupOptions $
--             describeOptionGroupOptionsResponse
--
--         , responseDescribeEventSubscriptions $
--             describeEventSubscriptionsResponse
--
--         , responseAddTagsToResource $
--             addTagsToResourceResponse
--
--         , responseDescribeDBParameters $
--             describeDBParametersResponse
--
--         , responseStopActivityStream $
--             stopActivityStreamResponse
--
--         , responseCreateDBClusterSnapshot $
--             createDBClusterSnapshotResponse
--
--         , responseDescribeDBSnapshots $
--             describeDBSnapshotsResponse
--
--         , responseModifyDBProxyTargetGroup $
--             modifyDBProxyTargetGroupResponse
--
--         , responseDescribeDBSubnetGroups $
--             describeDBSubnetGroupsResponse
--
--         , responseModifyOptionGroup $
--             modifyOptionGroupResponse
--
--         , responseStopDBCluster $
--             stopDBClusterResponse
--
--         , responseCreateDBParameterGroup $
--             createDBParameterGroupResponse
--
--         , responseModifyDBClusterSnapshotAttribute $
--             modifyDBClusterSnapshotAttributeResponse
--
--         , responseModifyDBCluster $
--             modifyDBClusterResponse
--
--         , responseCopyDBClusterParameterGroup $
--             copyDBClusterParameterGroupResponse
--
--         , responseDescribeEventCategories $
--             describeEventCategoriesResponse
--
--         , responseDescribeGlobalClusters $
--             describeGlobalClustersResponse
--
--         , responseStartDBInstance $
--             startDBInstanceResponse
--
--         , responseDescribeExportTasks $
--             describeExportTasksResponse
--
--         , responseCancelExportTask $
--             exportTask
--
--         , responseModifyDBClusterParameterGroup $
--             dbClusterParameterGroupNameMessage
--
--         , responseRestoreDBInstanceToPointInTime $
--             restoreDBInstanceToPointInTimeResponse
--
--         , responseDescribeDBClusterSnapshotAttributes $
--             describeDBClusterSnapshotAttributesResponse
--
--         , responseModifyDBSnapshot $
--             modifyDBSnapshotResponse
--
--         , responseDescribeDBProxyTargetGroups $
--             describeDBProxyTargetGroupsResponse
--
--         , responseModifyDBProxy $
--             modifyDBProxyResponse
--
--         , responseDescribePendingMaintenanceActions $
--             describePendingMaintenanceActionsResponse
--
--         , responseAddRoleToDBCluster $
--             addRoleToDBClusterResponse
--
--         , responseCopyDBClusterSnapshot $
--             copyDBClusterSnapshotResponse
--
--         , responseImportInstallationMedia $
--             installationMedia
--
--         , responseCreateGlobalCluster $
--             createGlobalClusterResponse
--
--         , responseResetDBParameterGroup $
--             dbParameterGroupNameMessage
--
--         , responseDescribeInstallationMedia $
--             describeInstallationMediaResponse
--
--         , responseDeregisterDBProxyTargets $
--             deregisterDBProxyTargetsResponse
--
--         , responseCreateDBCluster $
--             createDBClusterResponse
--
--         , responseRemoveRoleFromDBCluster $
--             removeRoleFromDBClusterResponse
--
--         , responseFailoverDBCluster $
--             failoverDBClusterResponse
--
--         , responseRevokeDBSecurityGroupIngress $
--             revokeDBSecurityGroupIngressResponse
--
--         , responseModifyDBParameterGroup $
--             dbParameterGroupNameMessage
--
--         , responseApplyPendingMaintenanceAction $
--             applyPendingMaintenanceActionResponse
--
--         , responseCreateOptionGroup $
--             createOptionGroupResponse
--
--         , responseDescribeAccountAttributes $
--             describeAccountAttributesResponse
--
--         , responseDeleteDBSnapshot $
--             deleteDBSnapshotResponse
--
--         , responseDescribeDBClusterParameters $
--             describeDBClusterParametersResponse
--
--         , responseDeleteDBSubnetGroup $
--             deleteDBSubnetGroupResponse
--
--         , responseCreateDBSecurityGroup $
--             createDBSecurityGroupResponse
--
--         , responseModifyCertificates $
--             modifyCertificatesResponse
--
--         , responseDescribeDBClusterSnapshots $
--             describeDBClusterSnapshotsResponse
--
--         , responseRebootDBInstance $
--             rebootDBInstanceResponse
--
--         , responseCreateDBSubnetGroup $
--             createDBSubnetGroupResponse
--
--         , responseDescribeReservedDBInstancesOfferings $
--             describeReservedDBInstancesOfferingsResponse
--
--         , responseDeleteDBSecurityGroup $
--             deleteDBSecurityGroupResponse
--
--         , responseDeleteDBInstance $
--             deleteDBInstanceResponse
--
--         , responseStartActivityStream $
--             startActivityStreamResponse
--
--         , responseCreateDBInstanceReadReplica $
--             createDBInstanceReadReplicaResponse
--
--         , responseDeleteDBParameterGroup $
--             deleteDBParameterGroupResponse
--
--         , responseModifyCurrentDBClusterCapacity $
--             modifyCurrentDBClusterCapacityResponse
--
--         , responseModifyGlobalCluster $
--             modifyGlobalClusterResponse
--
--         , responseRegisterDBProxyTargets $
--             registerDBProxyTargetsResponse
--
--         , responseDescribeDBSecurityGroups $
--             describeDBSecurityGroupsResponse
--
--         , responseCopyOptionGroup $
--             copyOptionGroupResponse
--
--         , responseRestoreDBClusterToPointInTime $
--             restoreDBClusterToPointInTimeResponse
--
--         , responseDeleteInstallationMedia $
--             installationMedia
--
--         , responseDescribeDBInstances $
--             describeDBInstancesResponse
--
--         , responseRestoreDBInstanceFromS3 $
--             restoreDBInstanceFromS3Response
--
--         , responseDownloadDBLogFilePortion $
--             downloadDBLogFilePortionResponse
--
--         , responseDescribeDBProxies $
--             describeDBProxiesResponse
--
--         , responseStartExportTask $
--             exportTask
--
--           ]
--     ]

-- Requests

requestStartDBCluster :: StartDBCluster -> TestTree
requestStartDBCluster =
  req
    "StartDBCluster"
    "fixture/StartDBCluster.yaml"

requestDescribeDBClusterParameterGroups :: DescribeDBClusterParameterGroups -> TestTree
requestDescribeDBClusterParameterGroups =
  req
    "DescribeDBClusterParameterGroups"
    "fixture/DescribeDBClusterParameterGroups.yaml"

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

requestStopDBInstance :: StopDBInstance -> TestTree
requestStopDBInstance =
  req
    "StopDBInstance"
    "fixture/StopDBInstance.yaml"

requestModifyDBClusterEndpoint :: ModifyDBClusterEndpoint -> TestTree
requestModifyDBClusterEndpoint =
  req
    "ModifyDBClusterEndpoint"
    "fixture/ModifyDBClusterEndpoint.yaml"

requestCopyDBSnapshot :: CopyDBSnapshot -> TestTree
requestCopyDBSnapshot =
  req
    "CopyDBSnapshot"
    "fixture/CopyDBSnapshot.yaml"

requestAddSourceIdentifierToSubscription :: AddSourceIdentifierToSubscription -> TestTree
requestAddSourceIdentifierToSubscription =
  req
    "AddSourceIdentifierToSubscription"
    "fixture/AddSourceIdentifierToSubscription.yaml"

requestModifyDBInstance :: ModifyDBInstance -> TestTree
requestModifyDBInstance =
  req
    "ModifyDBInstance"
    "fixture/ModifyDBInstance.yaml"

requestModifyEventSubscription :: ModifyEventSubscription -> TestTree
requestModifyEventSubscription =
  req
    "ModifyEventSubscription"
    "fixture/ModifyEventSubscription.yaml"

requestResetDBClusterParameterGroup :: ResetDBClusterParameterGroup -> TestTree
requestResetDBClusterParameterGroup =
  req
    "ResetDBClusterParameterGroup"
    "fixture/ResetDBClusterParameterGroup.yaml"

requestDescribeCustomAvailabilityZones :: DescribeCustomAvailabilityZones -> TestTree
requestDescribeCustomAvailabilityZones =
  req
    "DescribeCustomAvailabilityZones"
    "fixture/DescribeCustomAvailabilityZones.yaml"

requestRestoreDBClusterFromS3 :: RestoreDBClusterFromS3 -> TestTree
requestRestoreDBClusterFromS3 =
  req
    "RestoreDBClusterFromS3"
    "fixture/RestoreDBClusterFromS3.yaml"

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

requestDescribeOptionGroups :: DescribeOptionGroups -> TestTree
requestDescribeOptionGroups =
  req
    "DescribeOptionGroups"
    "fixture/DescribeOptionGroups.yaml"

requestDescribeDBLogFiles :: DescribeDBLogFiles -> TestTree
requestDescribeDBLogFiles =
  req
    "DescribeDBLogFiles"
    "fixture/DescribeDBLogFiles.yaml"

requestDescribeDBClusters :: DescribeDBClusters -> TestTree
requestDescribeDBClusters =
  req
    "DescribeDBClusters"
    "fixture/DescribeDBClusters.yaml"

requestModifyDBSubnetGroup :: ModifyDBSubnetGroup -> TestTree
requestModifyDBSubnetGroup =
  req
    "ModifyDBSubnetGroup"
    "fixture/ModifyDBSubnetGroup.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestDeleteOptionGroup :: DeleteOptionGroup -> TestTree
requestDeleteOptionGroup =
  req
    "DeleteOptionGroup"
    "fixture/DeleteOptionGroup.yaml"

requestDeleteDBCluster :: DeleteDBCluster -> TestTree
requestDeleteDBCluster =
  req
    "DeleteDBCluster"
    "fixture/DeleteDBCluster.yaml"

requestDescribeReservedDBInstances :: DescribeReservedDBInstances -> TestTree
requestDescribeReservedDBInstances =
  req
    "DescribeReservedDBInstances"
    "fixture/DescribeReservedDBInstances.yaml"

requestCopyDBParameterGroup :: CopyDBParameterGroup -> TestTree
requestCopyDBParameterGroup =
  req
    "CopyDBParameterGroup"
    "fixture/CopyDBParameterGroup.yaml"

requestRemoveSourceIdentifierFromSubscription :: RemoveSourceIdentifierFromSubscription -> TestTree
requestRemoveSourceIdentifierFromSubscription =
  req
    "RemoveSourceIdentifierFromSubscription"
    "fixture/RemoveSourceIdentifierFromSubscription.yaml"

requestDeleteCustomAvailabilityZone :: DeleteCustomAvailabilityZone -> TestTree
requestDeleteCustomAvailabilityZone =
  req
    "DeleteCustomAvailabilityZone"
    "fixture/DeleteCustomAvailabilityZone.yaml"

requestDescribeDBProxyTargets :: DescribeDBProxyTargets -> TestTree
requestDescribeDBProxyTargets =
  req
    "DescribeDBProxyTargets"
    "fixture/DescribeDBProxyTargets.yaml"

requestDescribeEngineDefaultClusterParameters :: DescribeEngineDefaultClusterParameters -> TestTree
requestDescribeEngineDefaultClusterParameters =
  req
    "DescribeEngineDefaultClusterParameters"
    "fixture/DescribeEngineDefaultClusterParameters.yaml"

requestDescribeDBSnapshotAttributes :: DescribeDBSnapshotAttributes -> TestTree
requestDescribeDBSnapshotAttributes =
  req
    "DescribeDBSnapshotAttributes"
    "fixture/DescribeDBSnapshotAttributes.yaml"

requestCreateCustomAvailabilityZone :: CreateCustomAvailabilityZone -> TestTree
requestCreateCustomAvailabilityZone =
  req
    "CreateCustomAvailabilityZone"
    "fixture/CreateCustomAvailabilityZone.yaml"

requestBacktrackDBCluster :: BacktrackDBCluster -> TestTree
requestBacktrackDBCluster =
  req
    "BacktrackDBCluster"
    "fixture/BacktrackDBCluster.yaml"

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

requestRemoveTagsFromResource :: RemoveTagsFromResource -> TestTree
requestRemoveTagsFromResource =
  req
    "RemoveTagsFromResource"
    "fixture/RemoveTagsFromResource.yaml"

requestRestoreDBInstanceFromDBSnapshot :: RestoreDBInstanceFromDBSnapshot -> TestTree
requestRestoreDBInstanceFromDBSnapshot =
  req
    "RestoreDBInstanceFromDBSnapshot"
    "fixture/RestoreDBInstanceFromDBSnapshot.yaml"

requestDeleteDBProxy :: DeleteDBProxy -> TestTree
requestDeleteDBProxy =
  req
    "DeleteDBProxy"
    "fixture/DeleteDBProxy.yaml"

requestCreateEventSubscription :: CreateEventSubscription -> TestTree
requestCreateEventSubscription =
  req
    "CreateEventSubscription"
    "fixture/CreateEventSubscription.yaml"

requestPurchaseReservedDBInstancesOffering :: PurchaseReservedDBInstancesOffering -> TestTree
requestPurchaseReservedDBInstancesOffering =
  req
    "PurchaseReservedDBInstancesOffering"
    "fixture/PurchaseReservedDBInstancesOffering.yaml"

requestCreateDBInstance :: CreateDBInstance -> TestTree
requestCreateDBInstance =
  req
    "CreateDBInstance"
    "fixture/CreateDBInstance.yaml"

requestDeleteDBClusterParameterGroup :: DeleteDBClusterParameterGroup -> TestTree
requestDeleteDBClusterParameterGroup =
  req
    "DeleteDBClusterParameterGroup"
    "fixture/DeleteDBClusterParameterGroup.yaml"

requestDescribeCertificates :: DescribeCertificates -> TestTree
requestDescribeCertificates =
  req
    "DescribeCertificates"
    "fixture/DescribeCertificates.yaml"

requestAuthorizeDBSecurityGroupIngress :: AuthorizeDBSecurityGroupIngress -> TestTree
requestAuthorizeDBSecurityGroupIngress =
  req
    "AuthorizeDBSecurityGroupIngress"
    "fixture/AuthorizeDBSecurityGroupIngress.yaml"

requestRemoveRoleFromDBInstance :: RemoveRoleFromDBInstance -> TestTree
requestRemoveRoleFromDBInstance =
  req
    "RemoveRoleFromDBInstance"
    "fixture/RemoveRoleFromDBInstance.yaml"

requestDescribeSourceRegions :: DescribeSourceRegions -> TestTree
requestDescribeSourceRegions =
  req
    "DescribeSourceRegions"
    "fixture/DescribeSourceRegions.yaml"

requestCreateDBClusterEndpoint :: CreateDBClusterEndpoint -> TestTree
requestCreateDBClusterEndpoint =
  req
    "CreateDBClusterEndpoint"
    "fixture/CreateDBClusterEndpoint.yaml"

requestRestoreDBClusterFromSnapshot :: RestoreDBClusterFromSnapshot -> TestTree
requestRestoreDBClusterFromSnapshot =
  req
    "RestoreDBClusterFromSnapshot"
    "fixture/RestoreDBClusterFromSnapshot.yaml"

requestDescribeOrderableDBInstanceOptions :: DescribeOrderableDBInstanceOptions -> TestTree
requestDescribeOrderableDBInstanceOptions =
  req
    "DescribeOrderableDBInstanceOptions"
    "fixture/DescribeOrderableDBInstanceOptions.yaml"

requestDeleteDBClusterEndpoint :: DeleteDBClusterEndpoint -> TestTree
requestDeleteDBClusterEndpoint =
  req
    "DeleteDBClusterEndpoint"
    "fixture/DeleteDBClusterEndpoint.yaml"

requestCreateDBProxy :: CreateDBProxy -> TestTree
requestCreateDBProxy =
  req
    "CreateDBProxy"
    "fixture/CreateDBProxy.yaml"

requestDeleteDBInstanceAutomatedBackup :: DeleteDBInstanceAutomatedBackup -> TestTree
requestDeleteDBInstanceAutomatedBackup =
  req
    "DeleteDBInstanceAutomatedBackup"
    "fixture/DeleteDBInstanceAutomatedBackup.yaml"

requestCreateDBClusterParameterGroup :: CreateDBClusterParameterGroup -> TestTree
requestCreateDBClusterParameterGroup =
  req
    "CreateDBClusterParameterGroup"
    "fixture/CreateDBClusterParameterGroup.yaml"

requestCreateDBSnapshot :: CreateDBSnapshot -> TestTree
requestCreateDBSnapshot =
  req
    "CreateDBSnapshot"
    "fixture/CreateDBSnapshot.yaml"

requestDeleteEventSubscription :: DeleteEventSubscription -> TestTree
requestDeleteEventSubscription =
  req
    "DeleteEventSubscription"
    "fixture/DeleteEventSubscription.yaml"

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

requestModifyDBSnapshotAttribute :: ModifyDBSnapshotAttribute -> TestTree
requestModifyDBSnapshotAttribute =
  req
    "ModifyDBSnapshotAttribute"
    "fixture/ModifyDBSnapshotAttribute.yaml"

requestDescribeDBInstanceAutomatedBackups :: DescribeDBInstanceAutomatedBackups -> TestTree
requestDescribeDBInstanceAutomatedBackups =
  req
    "DescribeDBInstanceAutomatedBackups"
    "fixture/DescribeDBInstanceAutomatedBackups.yaml"

requestRemoveFromGlobalCluster :: RemoveFromGlobalCluster -> TestTree
requestRemoveFromGlobalCluster =
  req
    "RemoveFromGlobalCluster"
    "fixture/RemoveFromGlobalCluster.yaml"

requestAddRoleToDBInstance :: AddRoleToDBInstance -> TestTree
requestAddRoleToDBInstance =
  req
    "AddRoleToDBInstance"
    "fixture/AddRoleToDBInstance.yaml"

requestDeleteDBClusterSnapshot :: DeleteDBClusterSnapshot -> TestTree
requestDeleteDBClusterSnapshot =
  req
    "DeleteDBClusterSnapshot"
    "fixture/DeleteDBClusterSnapshot.yaml"

requestDescribeValidDBInstanceModifications :: DescribeValidDBInstanceModifications -> TestTree
requestDescribeValidDBInstanceModifications =
  req
    "DescribeValidDBInstanceModifications"
    "fixture/DescribeValidDBInstanceModifications.yaml"

requestDescribeDBClusterEndpoints :: DescribeDBClusterEndpoints -> TestTree
requestDescribeDBClusterEndpoints =
  req
    "DescribeDBClusterEndpoints"
    "fixture/DescribeDBClusterEndpoints.yaml"

requestDescribeOptionGroupOptions :: DescribeOptionGroupOptions -> TestTree
requestDescribeOptionGroupOptions =
  req
    "DescribeOptionGroupOptions"
    "fixture/DescribeOptionGroupOptions.yaml"

requestDescribeEventSubscriptions :: DescribeEventSubscriptions -> TestTree
requestDescribeEventSubscriptions =
  req
    "DescribeEventSubscriptions"
    "fixture/DescribeEventSubscriptions.yaml"

requestAddTagsToResource :: AddTagsToResource -> TestTree
requestAddTagsToResource =
  req
    "AddTagsToResource"
    "fixture/AddTagsToResource.yaml"

requestDescribeDBParameters :: DescribeDBParameters -> TestTree
requestDescribeDBParameters =
  req
    "DescribeDBParameters"
    "fixture/DescribeDBParameters.yaml"

requestStopActivityStream :: StopActivityStream -> TestTree
requestStopActivityStream =
  req
    "StopActivityStream"
    "fixture/StopActivityStream.yaml"

requestCreateDBClusterSnapshot :: CreateDBClusterSnapshot -> TestTree
requestCreateDBClusterSnapshot =
  req
    "CreateDBClusterSnapshot"
    "fixture/CreateDBClusterSnapshot.yaml"

requestDescribeDBSnapshots :: DescribeDBSnapshots -> TestTree
requestDescribeDBSnapshots =
  req
    "DescribeDBSnapshots"
    "fixture/DescribeDBSnapshots.yaml"

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

requestModifyDBCluster :: ModifyDBCluster -> TestTree
requestModifyDBCluster =
  req
    "ModifyDBCluster"
    "fixture/ModifyDBCluster.yaml"

requestCopyDBClusterParameterGroup :: CopyDBClusterParameterGroup -> TestTree
requestCopyDBClusterParameterGroup =
  req
    "CopyDBClusterParameterGroup"
    "fixture/CopyDBClusterParameterGroup.yaml"

requestDescribeEventCategories :: DescribeEventCategories -> TestTree
requestDescribeEventCategories =
  req
    "DescribeEventCategories"
    "fixture/DescribeEventCategories.yaml"

requestDescribeGlobalClusters :: DescribeGlobalClusters -> TestTree
requestDescribeGlobalClusters =
  req
    "DescribeGlobalClusters"
    "fixture/DescribeGlobalClusters.yaml"

requestStartDBInstance :: StartDBInstance -> TestTree
requestStartDBInstance =
  req
    "StartDBInstance"
    "fixture/StartDBInstance.yaml"

requestDescribeExportTasks :: DescribeExportTasks -> TestTree
requestDescribeExportTasks =
  req
    "DescribeExportTasks"
    "fixture/DescribeExportTasks.yaml"

requestCancelExportTask :: CancelExportTask -> TestTree
requestCancelExportTask =
  req
    "CancelExportTask"
    "fixture/CancelExportTask.yaml"

requestModifyDBClusterParameterGroup :: ModifyDBClusterParameterGroup -> TestTree
requestModifyDBClusterParameterGroup =
  req
    "ModifyDBClusterParameterGroup"
    "fixture/ModifyDBClusterParameterGroup.yaml"

requestRestoreDBInstanceToPointInTime :: RestoreDBInstanceToPointInTime -> TestTree
requestRestoreDBInstanceToPointInTime =
  req
    "RestoreDBInstanceToPointInTime"
    "fixture/RestoreDBInstanceToPointInTime.yaml"

requestDescribeDBClusterSnapshotAttributes :: DescribeDBClusterSnapshotAttributes -> TestTree
requestDescribeDBClusterSnapshotAttributes =
  req
    "DescribeDBClusterSnapshotAttributes"
    "fixture/DescribeDBClusterSnapshotAttributes.yaml"

requestModifyDBSnapshot :: ModifyDBSnapshot -> TestTree
requestModifyDBSnapshot =
  req
    "ModifyDBSnapshot"
    "fixture/ModifyDBSnapshot.yaml"

requestDescribeDBProxyTargetGroups :: DescribeDBProxyTargetGroups -> TestTree
requestDescribeDBProxyTargetGroups =
  req
    "DescribeDBProxyTargetGroups"
    "fixture/DescribeDBProxyTargetGroups.yaml"

requestModifyDBProxy :: ModifyDBProxy -> TestTree
requestModifyDBProxy =
  req
    "ModifyDBProxy"
    "fixture/ModifyDBProxy.yaml"

requestDescribePendingMaintenanceActions :: DescribePendingMaintenanceActions -> TestTree
requestDescribePendingMaintenanceActions =
  req
    "DescribePendingMaintenanceActions"
    "fixture/DescribePendingMaintenanceActions.yaml"

requestAddRoleToDBCluster :: AddRoleToDBCluster -> TestTree
requestAddRoleToDBCluster =
  req
    "AddRoleToDBCluster"
    "fixture/AddRoleToDBCluster.yaml"

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

requestDescribeInstallationMedia :: DescribeInstallationMedia -> TestTree
requestDescribeInstallationMedia =
  req
    "DescribeInstallationMedia"
    "fixture/DescribeInstallationMedia.yaml"

requestDeregisterDBProxyTargets :: DeregisterDBProxyTargets -> TestTree
requestDeregisterDBProxyTargets =
  req
    "DeregisterDBProxyTargets"
    "fixture/DeregisterDBProxyTargets.yaml"

requestCreateDBCluster :: CreateDBCluster -> TestTree
requestCreateDBCluster =
  req
    "CreateDBCluster"
    "fixture/CreateDBCluster.yaml"

requestRemoveRoleFromDBCluster :: RemoveRoleFromDBCluster -> TestTree
requestRemoveRoleFromDBCluster =
  req
    "RemoveRoleFromDBCluster"
    "fixture/RemoveRoleFromDBCluster.yaml"

requestFailoverDBCluster :: FailoverDBCluster -> TestTree
requestFailoverDBCluster =
  req
    "FailoverDBCluster"
    "fixture/FailoverDBCluster.yaml"

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

requestApplyPendingMaintenanceAction :: ApplyPendingMaintenanceAction -> TestTree
requestApplyPendingMaintenanceAction =
  req
    "ApplyPendingMaintenanceAction"
    "fixture/ApplyPendingMaintenanceAction.yaml"

requestCreateOptionGroup :: CreateOptionGroup -> TestTree
requestCreateOptionGroup =
  req
    "CreateOptionGroup"
    "fixture/CreateOptionGroup.yaml"

requestDescribeAccountAttributes :: DescribeAccountAttributes -> TestTree
requestDescribeAccountAttributes =
  req
    "DescribeAccountAttributes"
    "fixture/DescribeAccountAttributes.yaml"

requestDeleteDBSnapshot :: DeleteDBSnapshot -> TestTree
requestDeleteDBSnapshot =
  req
    "DeleteDBSnapshot"
    "fixture/DeleteDBSnapshot.yaml"

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

requestCreateDBSecurityGroup :: CreateDBSecurityGroup -> TestTree
requestCreateDBSecurityGroup =
  req
    "CreateDBSecurityGroup"
    "fixture/CreateDBSecurityGroup.yaml"

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

requestRebootDBInstance :: RebootDBInstance -> TestTree
requestRebootDBInstance =
  req
    "RebootDBInstance"
    "fixture/RebootDBInstance.yaml"

requestCreateDBSubnetGroup :: CreateDBSubnetGroup -> TestTree
requestCreateDBSubnetGroup =
  req
    "CreateDBSubnetGroup"
    "fixture/CreateDBSubnetGroup.yaml"

requestDescribeReservedDBInstancesOfferings :: DescribeReservedDBInstancesOfferings -> TestTree
requestDescribeReservedDBInstancesOfferings =
  req
    "DescribeReservedDBInstancesOfferings"
    "fixture/DescribeReservedDBInstancesOfferings.yaml"

requestDeleteDBSecurityGroup :: DeleteDBSecurityGroup -> TestTree
requestDeleteDBSecurityGroup =
  req
    "DeleteDBSecurityGroup"
    "fixture/DeleteDBSecurityGroup.yaml"

requestDeleteDBInstance :: DeleteDBInstance -> TestTree
requestDeleteDBInstance =
  req
    "DeleteDBInstance"
    "fixture/DeleteDBInstance.yaml"

requestStartActivityStream :: StartActivityStream -> TestTree
requestStartActivityStream =
  req
    "StartActivityStream"
    "fixture/StartActivityStream.yaml"

requestCreateDBInstanceReadReplica :: CreateDBInstanceReadReplica -> TestTree
requestCreateDBInstanceReadReplica =
  req
    "CreateDBInstanceReadReplica"
    "fixture/CreateDBInstanceReadReplica.yaml"

requestDeleteDBParameterGroup :: DeleteDBParameterGroup -> TestTree
requestDeleteDBParameterGroup =
  req
    "DeleteDBParameterGroup"
    "fixture/DeleteDBParameterGroup.yaml"

requestModifyCurrentDBClusterCapacity :: ModifyCurrentDBClusterCapacity -> TestTree
requestModifyCurrentDBClusterCapacity =
  req
    "ModifyCurrentDBClusterCapacity"
    "fixture/ModifyCurrentDBClusterCapacity.yaml"

requestModifyGlobalCluster :: ModifyGlobalCluster -> TestTree
requestModifyGlobalCluster =
  req
    "ModifyGlobalCluster"
    "fixture/ModifyGlobalCluster.yaml"

requestRegisterDBProxyTargets :: RegisterDBProxyTargets -> TestTree
requestRegisterDBProxyTargets =
  req
    "RegisterDBProxyTargets"
    "fixture/RegisterDBProxyTargets.yaml"

requestDescribeDBSecurityGroups :: DescribeDBSecurityGroups -> TestTree
requestDescribeDBSecurityGroups =
  req
    "DescribeDBSecurityGroups"
    "fixture/DescribeDBSecurityGroups.yaml"

requestCopyOptionGroup :: CopyOptionGroup -> TestTree
requestCopyOptionGroup =
  req
    "CopyOptionGroup"
    "fixture/CopyOptionGroup.yaml"

requestRestoreDBClusterToPointInTime :: RestoreDBClusterToPointInTime -> TestTree
requestRestoreDBClusterToPointInTime =
  req
    "RestoreDBClusterToPointInTime"
    "fixture/RestoreDBClusterToPointInTime.yaml"

requestDeleteInstallationMedia :: DeleteInstallationMedia -> TestTree
requestDeleteInstallationMedia =
  req
    "DeleteInstallationMedia"
    "fixture/DeleteInstallationMedia.yaml"

requestDescribeDBInstances :: DescribeDBInstances -> TestTree
requestDescribeDBInstances =
  req
    "DescribeDBInstances"
    "fixture/DescribeDBInstances.yaml"

requestRestoreDBInstanceFromS3 :: RestoreDBInstanceFromS3 -> TestTree
requestRestoreDBInstanceFromS3 =
  req
    "RestoreDBInstanceFromS3"
    "fixture/RestoreDBInstanceFromS3.yaml"

requestDownloadDBLogFilePortion :: DownloadDBLogFilePortion -> TestTree
requestDownloadDBLogFilePortion =
  req
    "DownloadDBLogFilePortion"
    "fixture/DownloadDBLogFilePortion.yaml"

requestDescribeDBProxies :: DescribeDBProxies -> TestTree
requestDescribeDBProxies =
  req
    "DescribeDBProxies"
    "fixture/DescribeDBProxies.yaml"

requestStartExportTask :: StartExportTask -> TestTree
requestStartExportTask =
  req
    "StartExportTask"
    "fixture/StartExportTask.yaml"

-- Responses

responseStartDBCluster :: StartDBClusterResponse -> TestTree
responseStartDBCluster =
  res
    "StartDBClusterResponse"
    "fixture/StartDBClusterResponse.proto"
    rds
    (Proxy :: Proxy StartDBCluster)

responseDescribeDBClusterParameterGroups :: DescribeDBClusterParameterGroupsResponse -> TestTree
responseDescribeDBClusterParameterGroups =
  res
    "DescribeDBClusterParameterGroupsResponse"
    "fixture/DescribeDBClusterParameterGroupsResponse.proto"
    rds
    (Proxy :: Proxy DescribeDBClusterParameterGroups)

responsePromoteReadReplica :: PromoteReadReplicaResponse -> TestTree
responsePromoteReadReplica =
  res
    "PromoteReadReplicaResponse"
    "fixture/PromoteReadReplicaResponse.proto"
    rds
    (Proxy :: Proxy PromoteReadReplica)

responseDescribeDBEngineVersions :: DescribeDBEngineVersionsResponse -> TestTree
responseDescribeDBEngineVersions =
  res
    "DescribeDBEngineVersionsResponse"
    "fixture/DescribeDBEngineVersionsResponse.proto"
    rds
    (Proxy :: Proxy DescribeDBEngineVersions)

responseStopDBInstance :: StopDBInstanceResponse -> TestTree
responseStopDBInstance =
  res
    "StopDBInstanceResponse"
    "fixture/StopDBInstanceResponse.proto"
    rds
    (Proxy :: Proxy StopDBInstance)

responseModifyDBClusterEndpoint :: DBClusterEndpoint -> TestTree
responseModifyDBClusterEndpoint =
  res
    "ModifyDBClusterEndpointResponse"
    "fixture/ModifyDBClusterEndpointResponse.proto"
    rds
    (Proxy :: Proxy ModifyDBClusterEndpoint)

responseCopyDBSnapshot :: CopyDBSnapshotResponse -> TestTree
responseCopyDBSnapshot =
  res
    "CopyDBSnapshotResponse"
    "fixture/CopyDBSnapshotResponse.proto"
    rds
    (Proxy :: Proxy CopyDBSnapshot)

responseAddSourceIdentifierToSubscription :: AddSourceIdentifierToSubscriptionResponse -> TestTree
responseAddSourceIdentifierToSubscription =
  res
    "AddSourceIdentifierToSubscriptionResponse"
    "fixture/AddSourceIdentifierToSubscriptionResponse.proto"
    rds
    (Proxy :: Proxy AddSourceIdentifierToSubscription)

responseModifyDBInstance :: ModifyDBInstanceResponse -> TestTree
responseModifyDBInstance =
  res
    "ModifyDBInstanceResponse"
    "fixture/ModifyDBInstanceResponse.proto"
    rds
    (Proxy :: Proxy ModifyDBInstance)

responseModifyEventSubscription :: ModifyEventSubscriptionResponse -> TestTree
responseModifyEventSubscription =
  res
    "ModifyEventSubscriptionResponse"
    "fixture/ModifyEventSubscriptionResponse.proto"
    rds
    (Proxy :: Proxy ModifyEventSubscription)

responseResetDBClusterParameterGroup :: DBClusterParameterGroupNameMessage -> TestTree
responseResetDBClusterParameterGroup =
  res
    "ResetDBClusterParameterGroupResponse"
    "fixture/ResetDBClusterParameterGroupResponse.proto"
    rds
    (Proxy :: Proxy ResetDBClusterParameterGroup)

responseDescribeCustomAvailabilityZones :: DescribeCustomAvailabilityZonesResponse -> TestTree
responseDescribeCustomAvailabilityZones =
  res
    "DescribeCustomAvailabilityZonesResponse"
    "fixture/DescribeCustomAvailabilityZonesResponse.proto"
    rds
    (Proxy :: Proxy DescribeCustomAvailabilityZones)

responseRestoreDBClusterFromS3 :: RestoreDBClusterFromS3Response -> TestTree
responseRestoreDBClusterFromS3 =
  res
    "RestoreDBClusterFromS3Response"
    "fixture/RestoreDBClusterFromS3Response.proto"
    rds
    (Proxy :: Proxy RestoreDBClusterFromS3)

responseDescribeEvents :: DescribeEventsResponse -> TestTree
responseDescribeEvents =
  res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse.proto"
    rds
    (Proxy :: Proxy DescribeEvents)

responseDescribeEngineDefaultParameters :: DescribeEngineDefaultParametersResponse -> TestTree
responseDescribeEngineDefaultParameters =
  res
    "DescribeEngineDefaultParametersResponse"
    "fixture/DescribeEngineDefaultParametersResponse.proto"
    rds
    (Proxy :: Proxy DescribeEngineDefaultParameters)

responseDescribeOptionGroups :: DescribeOptionGroupsResponse -> TestTree
responseDescribeOptionGroups =
  res
    "DescribeOptionGroupsResponse"
    "fixture/DescribeOptionGroupsResponse.proto"
    rds
    (Proxy :: Proxy DescribeOptionGroups)

responseDescribeDBLogFiles :: DescribeDBLogFilesResponse -> TestTree
responseDescribeDBLogFiles =
  res
    "DescribeDBLogFilesResponse"
    "fixture/DescribeDBLogFilesResponse.proto"
    rds
    (Proxy :: Proxy DescribeDBLogFiles)

responseDescribeDBClusters :: DescribeDBClustersResponse -> TestTree
responseDescribeDBClusters =
  res
    "DescribeDBClustersResponse"
    "fixture/DescribeDBClustersResponse.proto"
    rds
    (Proxy :: Proxy DescribeDBClusters)

responseModifyDBSubnetGroup :: ModifyDBSubnetGroupResponse -> TestTree
responseModifyDBSubnetGroup =
  res
    "ModifyDBSubnetGroupResponse"
    "fixture/ModifyDBSubnetGroupResponse.proto"
    rds
    (Proxy :: Proxy ModifyDBSubnetGroup)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    rds
    (Proxy :: Proxy ListTagsForResource)

responseDeleteOptionGroup :: DeleteOptionGroupResponse -> TestTree
responseDeleteOptionGroup =
  res
    "DeleteOptionGroupResponse"
    "fixture/DeleteOptionGroupResponse.proto"
    rds
    (Proxy :: Proxy DeleteOptionGroup)

responseDeleteDBCluster :: DeleteDBClusterResponse -> TestTree
responseDeleteDBCluster =
  res
    "DeleteDBClusterResponse"
    "fixture/DeleteDBClusterResponse.proto"
    rds
    (Proxy :: Proxy DeleteDBCluster)

responseDescribeReservedDBInstances :: DescribeReservedDBInstancesResponse -> TestTree
responseDescribeReservedDBInstances =
  res
    "DescribeReservedDBInstancesResponse"
    "fixture/DescribeReservedDBInstancesResponse.proto"
    rds
    (Proxy :: Proxy DescribeReservedDBInstances)

responseCopyDBParameterGroup :: CopyDBParameterGroupResponse -> TestTree
responseCopyDBParameterGroup =
  res
    "CopyDBParameterGroupResponse"
    "fixture/CopyDBParameterGroupResponse.proto"
    rds
    (Proxy :: Proxy CopyDBParameterGroup)

responseRemoveSourceIdentifierFromSubscription :: RemoveSourceIdentifierFromSubscriptionResponse -> TestTree
responseRemoveSourceIdentifierFromSubscription =
  res
    "RemoveSourceIdentifierFromSubscriptionResponse"
    "fixture/RemoveSourceIdentifierFromSubscriptionResponse.proto"
    rds
    (Proxy :: Proxy RemoveSourceIdentifierFromSubscription)

responseDeleteCustomAvailabilityZone :: DeleteCustomAvailabilityZoneResponse -> TestTree
responseDeleteCustomAvailabilityZone =
  res
    "DeleteCustomAvailabilityZoneResponse"
    "fixture/DeleteCustomAvailabilityZoneResponse.proto"
    rds
    (Proxy :: Proxy DeleteCustomAvailabilityZone)

responseDescribeDBProxyTargets :: DescribeDBProxyTargetsResponse -> TestTree
responseDescribeDBProxyTargets =
  res
    "DescribeDBProxyTargetsResponse"
    "fixture/DescribeDBProxyTargetsResponse.proto"
    rds
    (Proxy :: Proxy DescribeDBProxyTargets)

responseDescribeEngineDefaultClusterParameters :: DescribeEngineDefaultClusterParametersResponse -> TestTree
responseDescribeEngineDefaultClusterParameters =
  res
    "DescribeEngineDefaultClusterParametersResponse"
    "fixture/DescribeEngineDefaultClusterParametersResponse.proto"
    rds
    (Proxy :: Proxy DescribeEngineDefaultClusterParameters)

responseDescribeDBSnapshotAttributes :: DescribeDBSnapshotAttributesResponse -> TestTree
responseDescribeDBSnapshotAttributes =
  res
    "DescribeDBSnapshotAttributesResponse"
    "fixture/DescribeDBSnapshotAttributesResponse.proto"
    rds
    (Proxy :: Proxy DescribeDBSnapshotAttributes)

responseCreateCustomAvailabilityZone :: CreateCustomAvailabilityZoneResponse -> TestTree
responseCreateCustomAvailabilityZone =
  res
    "CreateCustomAvailabilityZoneResponse"
    "fixture/CreateCustomAvailabilityZoneResponse.proto"
    rds
    (Proxy :: Proxy CreateCustomAvailabilityZone)

responseBacktrackDBCluster :: DBClusterBacktrack -> TestTree
responseBacktrackDBCluster =
  res
    "BacktrackDBClusterResponse"
    "fixture/BacktrackDBClusterResponse.proto"
    rds
    (Proxy :: Proxy BacktrackDBCluster)

responseDeleteGlobalCluster :: DeleteGlobalClusterResponse -> TestTree
responseDeleteGlobalCluster =
  res
    "DeleteGlobalClusterResponse"
    "fixture/DeleteGlobalClusterResponse.proto"
    rds
    (Proxy :: Proxy DeleteGlobalCluster)

responsePromoteReadReplicaDBCluster :: PromoteReadReplicaDBClusterResponse -> TestTree
responsePromoteReadReplicaDBCluster =
  res
    "PromoteReadReplicaDBClusterResponse"
    "fixture/PromoteReadReplicaDBClusterResponse.proto"
    rds
    (Proxy :: Proxy PromoteReadReplicaDBCluster)

responseRemoveTagsFromResource :: RemoveTagsFromResourceResponse -> TestTree
responseRemoveTagsFromResource =
  res
    "RemoveTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse.proto"
    rds
    (Proxy :: Proxy RemoveTagsFromResource)

responseRestoreDBInstanceFromDBSnapshot :: RestoreDBInstanceFromDBSnapshotResponse -> TestTree
responseRestoreDBInstanceFromDBSnapshot =
  res
    "RestoreDBInstanceFromDBSnapshotResponse"
    "fixture/RestoreDBInstanceFromDBSnapshotResponse.proto"
    rds
    (Proxy :: Proxy RestoreDBInstanceFromDBSnapshot)

responseDeleteDBProxy :: DeleteDBProxyResponse -> TestTree
responseDeleteDBProxy =
  res
    "DeleteDBProxyResponse"
    "fixture/DeleteDBProxyResponse.proto"
    rds
    (Proxy :: Proxy DeleteDBProxy)

responseCreateEventSubscription :: CreateEventSubscriptionResponse -> TestTree
responseCreateEventSubscription =
  res
    "CreateEventSubscriptionResponse"
    "fixture/CreateEventSubscriptionResponse.proto"
    rds
    (Proxy :: Proxy CreateEventSubscription)

responsePurchaseReservedDBInstancesOffering :: PurchaseReservedDBInstancesOfferingResponse -> TestTree
responsePurchaseReservedDBInstancesOffering =
  res
    "PurchaseReservedDBInstancesOfferingResponse"
    "fixture/PurchaseReservedDBInstancesOfferingResponse.proto"
    rds
    (Proxy :: Proxy PurchaseReservedDBInstancesOffering)

responseCreateDBInstance :: CreateDBInstanceResponse -> TestTree
responseCreateDBInstance =
  res
    "CreateDBInstanceResponse"
    "fixture/CreateDBInstanceResponse.proto"
    rds
    (Proxy :: Proxy CreateDBInstance)

responseDeleteDBClusterParameterGroup :: DeleteDBClusterParameterGroupResponse -> TestTree
responseDeleteDBClusterParameterGroup =
  res
    "DeleteDBClusterParameterGroupResponse"
    "fixture/DeleteDBClusterParameterGroupResponse.proto"
    rds
    (Proxy :: Proxy DeleteDBClusterParameterGroup)

responseDescribeCertificates :: DescribeCertificatesResponse -> TestTree
responseDescribeCertificates =
  res
    "DescribeCertificatesResponse"
    "fixture/DescribeCertificatesResponse.proto"
    rds
    (Proxy :: Proxy DescribeCertificates)

responseAuthorizeDBSecurityGroupIngress :: AuthorizeDBSecurityGroupIngressResponse -> TestTree
responseAuthorizeDBSecurityGroupIngress =
  res
    "AuthorizeDBSecurityGroupIngressResponse"
    "fixture/AuthorizeDBSecurityGroupIngressResponse.proto"
    rds
    (Proxy :: Proxy AuthorizeDBSecurityGroupIngress)

responseRemoveRoleFromDBInstance :: RemoveRoleFromDBInstanceResponse -> TestTree
responseRemoveRoleFromDBInstance =
  res
    "RemoveRoleFromDBInstanceResponse"
    "fixture/RemoveRoleFromDBInstanceResponse.proto"
    rds
    (Proxy :: Proxy RemoveRoleFromDBInstance)

responseDescribeSourceRegions :: DescribeSourceRegionsResponse -> TestTree
responseDescribeSourceRegions =
  res
    "DescribeSourceRegionsResponse"
    "fixture/DescribeSourceRegionsResponse.proto"
    rds
    (Proxy :: Proxy DescribeSourceRegions)

responseCreateDBClusterEndpoint :: DBClusterEndpoint -> TestTree
responseCreateDBClusterEndpoint =
  res
    "CreateDBClusterEndpointResponse"
    "fixture/CreateDBClusterEndpointResponse.proto"
    rds
    (Proxy :: Proxy CreateDBClusterEndpoint)

responseRestoreDBClusterFromSnapshot :: RestoreDBClusterFromSnapshotResponse -> TestTree
responseRestoreDBClusterFromSnapshot =
  res
    "RestoreDBClusterFromSnapshotResponse"
    "fixture/RestoreDBClusterFromSnapshotResponse.proto"
    rds
    (Proxy :: Proxy RestoreDBClusterFromSnapshot)

responseDescribeOrderableDBInstanceOptions :: DescribeOrderableDBInstanceOptionsResponse -> TestTree
responseDescribeOrderableDBInstanceOptions =
  res
    "DescribeOrderableDBInstanceOptionsResponse"
    "fixture/DescribeOrderableDBInstanceOptionsResponse.proto"
    rds
    (Proxy :: Proxy DescribeOrderableDBInstanceOptions)

responseDeleteDBClusterEndpoint :: DBClusterEndpoint -> TestTree
responseDeleteDBClusterEndpoint =
  res
    "DeleteDBClusterEndpointResponse"
    "fixture/DeleteDBClusterEndpointResponse.proto"
    rds
    (Proxy :: Proxy DeleteDBClusterEndpoint)

responseCreateDBProxy :: CreateDBProxyResponse -> TestTree
responseCreateDBProxy =
  res
    "CreateDBProxyResponse"
    "fixture/CreateDBProxyResponse.proto"
    rds
    (Proxy :: Proxy CreateDBProxy)

responseDeleteDBInstanceAutomatedBackup :: DeleteDBInstanceAutomatedBackupResponse -> TestTree
responseDeleteDBInstanceAutomatedBackup =
  res
    "DeleteDBInstanceAutomatedBackupResponse"
    "fixture/DeleteDBInstanceAutomatedBackupResponse.proto"
    rds
    (Proxy :: Proxy DeleteDBInstanceAutomatedBackup)

responseCreateDBClusterParameterGroup :: CreateDBClusterParameterGroupResponse -> TestTree
responseCreateDBClusterParameterGroup =
  res
    "CreateDBClusterParameterGroupResponse"
    "fixture/CreateDBClusterParameterGroupResponse.proto"
    rds
    (Proxy :: Proxy CreateDBClusterParameterGroup)

responseCreateDBSnapshot :: CreateDBSnapshotResponse -> TestTree
responseCreateDBSnapshot =
  res
    "CreateDBSnapshotResponse"
    "fixture/CreateDBSnapshotResponse.proto"
    rds
    (Proxy :: Proxy CreateDBSnapshot)

responseDeleteEventSubscription :: DeleteEventSubscriptionResponse -> TestTree
responseDeleteEventSubscription =
  res
    "DeleteEventSubscriptionResponse"
    "fixture/DeleteEventSubscriptionResponse.proto"
    rds
    (Proxy :: Proxy DeleteEventSubscription)

responseDescribeDBClusterBacktracks :: DescribeDBClusterBacktracksResponse -> TestTree
responseDescribeDBClusterBacktracks =
  res
    "DescribeDBClusterBacktracksResponse"
    "fixture/DescribeDBClusterBacktracksResponse.proto"
    rds
    (Proxy :: Proxy DescribeDBClusterBacktracks)

responseDescribeDBParameterGroups :: DescribeDBParameterGroupsResponse -> TestTree
responseDescribeDBParameterGroups =
  res
    "DescribeDBParameterGroupsResponse"
    "fixture/DescribeDBParameterGroupsResponse.proto"
    rds
    (Proxy :: Proxy DescribeDBParameterGroups)

responseModifyDBSnapshotAttribute :: ModifyDBSnapshotAttributeResponse -> TestTree
responseModifyDBSnapshotAttribute =
  res
    "ModifyDBSnapshotAttributeResponse"
    "fixture/ModifyDBSnapshotAttributeResponse.proto"
    rds
    (Proxy :: Proxy ModifyDBSnapshotAttribute)

responseDescribeDBInstanceAutomatedBackups :: DescribeDBInstanceAutomatedBackupsResponse -> TestTree
responseDescribeDBInstanceAutomatedBackups =
  res
    "DescribeDBInstanceAutomatedBackupsResponse"
    "fixture/DescribeDBInstanceAutomatedBackupsResponse.proto"
    rds
    (Proxy :: Proxy DescribeDBInstanceAutomatedBackups)

responseRemoveFromGlobalCluster :: RemoveFromGlobalClusterResponse -> TestTree
responseRemoveFromGlobalCluster =
  res
    "RemoveFromGlobalClusterResponse"
    "fixture/RemoveFromGlobalClusterResponse.proto"
    rds
    (Proxy :: Proxy RemoveFromGlobalCluster)

responseAddRoleToDBInstance :: AddRoleToDBInstanceResponse -> TestTree
responseAddRoleToDBInstance =
  res
    "AddRoleToDBInstanceResponse"
    "fixture/AddRoleToDBInstanceResponse.proto"
    rds
    (Proxy :: Proxy AddRoleToDBInstance)

responseDeleteDBClusterSnapshot :: DeleteDBClusterSnapshotResponse -> TestTree
responseDeleteDBClusterSnapshot =
  res
    "DeleteDBClusterSnapshotResponse"
    "fixture/DeleteDBClusterSnapshotResponse.proto"
    rds
    (Proxy :: Proxy DeleteDBClusterSnapshot)

responseDescribeValidDBInstanceModifications :: DescribeValidDBInstanceModificationsResponse -> TestTree
responseDescribeValidDBInstanceModifications =
  res
    "DescribeValidDBInstanceModificationsResponse"
    "fixture/DescribeValidDBInstanceModificationsResponse.proto"
    rds
    (Proxy :: Proxy DescribeValidDBInstanceModifications)

responseDescribeDBClusterEndpoints :: DescribeDBClusterEndpointsResponse -> TestTree
responseDescribeDBClusterEndpoints =
  res
    "DescribeDBClusterEndpointsResponse"
    "fixture/DescribeDBClusterEndpointsResponse.proto"
    rds
    (Proxy :: Proxy DescribeDBClusterEndpoints)

responseDescribeOptionGroupOptions :: DescribeOptionGroupOptionsResponse -> TestTree
responseDescribeOptionGroupOptions =
  res
    "DescribeOptionGroupOptionsResponse"
    "fixture/DescribeOptionGroupOptionsResponse.proto"
    rds
    (Proxy :: Proxy DescribeOptionGroupOptions)

responseDescribeEventSubscriptions :: DescribeEventSubscriptionsResponse -> TestTree
responseDescribeEventSubscriptions =
  res
    "DescribeEventSubscriptionsResponse"
    "fixture/DescribeEventSubscriptionsResponse.proto"
    rds
    (Proxy :: Proxy DescribeEventSubscriptions)

responseAddTagsToResource :: AddTagsToResourceResponse -> TestTree
responseAddTagsToResource =
  res
    "AddTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse.proto"
    rds
    (Proxy :: Proxy AddTagsToResource)

responseDescribeDBParameters :: DescribeDBParametersResponse -> TestTree
responseDescribeDBParameters =
  res
    "DescribeDBParametersResponse"
    "fixture/DescribeDBParametersResponse.proto"
    rds
    (Proxy :: Proxy DescribeDBParameters)

responseStopActivityStream :: StopActivityStreamResponse -> TestTree
responseStopActivityStream =
  res
    "StopActivityStreamResponse"
    "fixture/StopActivityStreamResponse.proto"
    rds
    (Proxy :: Proxy StopActivityStream)

responseCreateDBClusterSnapshot :: CreateDBClusterSnapshotResponse -> TestTree
responseCreateDBClusterSnapshot =
  res
    "CreateDBClusterSnapshotResponse"
    "fixture/CreateDBClusterSnapshotResponse.proto"
    rds
    (Proxy :: Proxy CreateDBClusterSnapshot)

responseDescribeDBSnapshots :: DescribeDBSnapshotsResponse -> TestTree
responseDescribeDBSnapshots =
  res
    "DescribeDBSnapshotsResponse"
    "fixture/DescribeDBSnapshotsResponse.proto"
    rds
    (Proxy :: Proxy DescribeDBSnapshots)

responseModifyDBProxyTargetGroup :: ModifyDBProxyTargetGroupResponse -> TestTree
responseModifyDBProxyTargetGroup =
  res
    "ModifyDBProxyTargetGroupResponse"
    "fixture/ModifyDBProxyTargetGroupResponse.proto"
    rds
    (Proxy :: Proxy ModifyDBProxyTargetGroup)

responseDescribeDBSubnetGroups :: DescribeDBSubnetGroupsResponse -> TestTree
responseDescribeDBSubnetGroups =
  res
    "DescribeDBSubnetGroupsResponse"
    "fixture/DescribeDBSubnetGroupsResponse.proto"
    rds
    (Proxy :: Proxy DescribeDBSubnetGroups)

responseModifyOptionGroup :: ModifyOptionGroupResponse -> TestTree
responseModifyOptionGroup =
  res
    "ModifyOptionGroupResponse"
    "fixture/ModifyOptionGroupResponse.proto"
    rds
    (Proxy :: Proxy ModifyOptionGroup)

responseStopDBCluster :: StopDBClusterResponse -> TestTree
responseStopDBCluster =
  res
    "StopDBClusterResponse"
    "fixture/StopDBClusterResponse.proto"
    rds
    (Proxy :: Proxy StopDBCluster)

responseCreateDBParameterGroup :: CreateDBParameterGroupResponse -> TestTree
responseCreateDBParameterGroup =
  res
    "CreateDBParameterGroupResponse"
    "fixture/CreateDBParameterGroupResponse.proto"
    rds
    (Proxy :: Proxy CreateDBParameterGroup)

responseModifyDBClusterSnapshotAttribute :: ModifyDBClusterSnapshotAttributeResponse -> TestTree
responseModifyDBClusterSnapshotAttribute =
  res
    "ModifyDBClusterSnapshotAttributeResponse"
    "fixture/ModifyDBClusterSnapshotAttributeResponse.proto"
    rds
    (Proxy :: Proxy ModifyDBClusterSnapshotAttribute)

responseModifyDBCluster :: ModifyDBClusterResponse -> TestTree
responseModifyDBCluster =
  res
    "ModifyDBClusterResponse"
    "fixture/ModifyDBClusterResponse.proto"
    rds
    (Proxy :: Proxy ModifyDBCluster)

responseCopyDBClusterParameterGroup :: CopyDBClusterParameterGroupResponse -> TestTree
responseCopyDBClusterParameterGroup =
  res
    "CopyDBClusterParameterGroupResponse"
    "fixture/CopyDBClusterParameterGroupResponse.proto"
    rds
    (Proxy :: Proxy CopyDBClusterParameterGroup)

responseDescribeEventCategories :: DescribeEventCategoriesResponse -> TestTree
responseDescribeEventCategories =
  res
    "DescribeEventCategoriesResponse"
    "fixture/DescribeEventCategoriesResponse.proto"
    rds
    (Proxy :: Proxy DescribeEventCategories)

responseDescribeGlobalClusters :: DescribeGlobalClustersResponse -> TestTree
responseDescribeGlobalClusters =
  res
    "DescribeGlobalClustersResponse"
    "fixture/DescribeGlobalClustersResponse.proto"
    rds
    (Proxy :: Proxy DescribeGlobalClusters)

responseStartDBInstance :: StartDBInstanceResponse -> TestTree
responseStartDBInstance =
  res
    "StartDBInstanceResponse"
    "fixture/StartDBInstanceResponse.proto"
    rds
    (Proxy :: Proxy StartDBInstance)

responseDescribeExportTasks :: DescribeExportTasksResponse -> TestTree
responseDescribeExportTasks =
  res
    "DescribeExportTasksResponse"
    "fixture/DescribeExportTasksResponse.proto"
    rds
    (Proxy :: Proxy DescribeExportTasks)

responseCancelExportTask :: ExportTask -> TestTree
responseCancelExportTask =
  res
    "CancelExportTaskResponse"
    "fixture/CancelExportTaskResponse.proto"
    rds
    (Proxy :: Proxy CancelExportTask)

responseModifyDBClusterParameterGroup :: DBClusterParameterGroupNameMessage -> TestTree
responseModifyDBClusterParameterGroup =
  res
    "ModifyDBClusterParameterGroupResponse"
    "fixture/ModifyDBClusterParameterGroupResponse.proto"
    rds
    (Proxy :: Proxy ModifyDBClusterParameterGroup)

responseRestoreDBInstanceToPointInTime :: RestoreDBInstanceToPointInTimeResponse -> TestTree
responseRestoreDBInstanceToPointInTime =
  res
    "RestoreDBInstanceToPointInTimeResponse"
    "fixture/RestoreDBInstanceToPointInTimeResponse.proto"
    rds
    (Proxy :: Proxy RestoreDBInstanceToPointInTime)

responseDescribeDBClusterSnapshotAttributes :: DescribeDBClusterSnapshotAttributesResponse -> TestTree
responseDescribeDBClusterSnapshotAttributes =
  res
    "DescribeDBClusterSnapshotAttributesResponse"
    "fixture/DescribeDBClusterSnapshotAttributesResponse.proto"
    rds
    (Proxy :: Proxy DescribeDBClusterSnapshotAttributes)

responseModifyDBSnapshot :: ModifyDBSnapshotResponse -> TestTree
responseModifyDBSnapshot =
  res
    "ModifyDBSnapshotResponse"
    "fixture/ModifyDBSnapshotResponse.proto"
    rds
    (Proxy :: Proxy ModifyDBSnapshot)

responseDescribeDBProxyTargetGroups :: DescribeDBProxyTargetGroupsResponse -> TestTree
responseDescribeDBProxyTargetGroups =
  res
    "DescribeDBProxyTargetGroupsResponse"
    "fixture/DescribeDBProxyTargetGroupsResponse.proto"
    rds
    (Proxy :: Proxy DescribeDBProxyTargetGroups)

responseModifyDBProxy :: ModifyDBProxyResponse -> TestTree
responseModifyDBProxy =
  res
    "ModifyDBProxyResponse"
    "fixture/ModifyDBProxyResponse.proto"
    rds
    (Proxy :: Proxy ModifyDBProxy)

responseDescribePendingMaintenanceActions :: DescribePendingMaintenanceActionsResponse -> TestTree
responseDescribePendingMaintenanceActions =
  res
    "DescribePendingMaintenanceActionsResponse"
    "fixture/DescribePendingMaintenanceActionsResponse.proto"
    rds
    (Proxy :: Proxy DescribePendingMaintenanceActions)

responseAddRoleToDBCluster :: AddRoleToDBClusterResponse -> TestTree
responseAddRoleToDBCluster =
  res
    "AddRoleToDBClusterResponse"
    "fixture/AddRoleToDBClusterResponse.proto"
    rds
    (Proxy :: Proxy AddRoleToDBCluster)

responseCopyDBClusterSnapshot :: CopyDBClusterSnapshotResponse -> TestTree
responseCopyDBClusterSnapshot =
  res
    "CopyDBClusterSnapshotResponse"
    "fixture/CopyDBClusterSnapshotResponse.proto"
    rds
    (Proxy :: Proxy CopyDBClusterSnapshot)

responseImportInstallationMedia :: InstallationMedia -> TestTree
responseImportInstallationMedia =
  res
    "ImportInstallationMediaResponse"
    "fixture/ImportInstallationMediaResponse.proto"
    rds
    (Proxy :: Proxy ImportInstallationMedia)

responseCreateGlobalCluster :: CreateGlobalClusterResponse -> TestTree
responseCreateGlobalCluster =
  res
    "CreateGlobalClusterResponse"
    "fixture/CreateGlobalClusterResponse.proto"
    rds
    (Proxy :: Proxy CreateGlobalCluster)

responseResetDBParameterGroup :: DBParameterGroupNameMessage -> TestTree
responseResetDBParameterGroup =
  res
    "ResetDBParameterGroupResponse"
    "fixture/ResetDBParameterGroupResponse.proto"
    rds
    (Proxy :: Proxy ResetDBParameterGroup)

responseDescribeInstallationMedia :: DescribeInstallationMediaResponse -> TestTree
responseDescribeInstallationMedia =
  res
    "DescribeInstallationMediaResponse"
    "fixture/DescribeInstallationMediaResponse.proto"
    rds
    (Proxy :: Proxy DescribeInstallationMedia)

responseDeregisterDBProxyTargets :: DeregisterDBProxyTargetsResponse -> TestTree
responseDeregisterDBProxyTargets =
  res
    "DeregisterDBProxyTargetsResponse"
    "fixture/DeregisterDBProxyTargetsResponse.proto"
    rds
    (Proxy :: Proxy DeregisterDBProxyTargets)

responseCreateDBCluster :: CreateDBClusterResponse -> TestTree
responseCreateDBCluster =
  res
    "CreateDBClusterResponse"
    "fixture/CreateDBClusterResponse.proto"
    rds
    (Proxy :: Proxy CreateDBCluster)

responseRemoveRoleFromDBCluster :: RemoveRoleFromDBClusterResponse -> TestTree
responseRemoveRoleFromDBCluster =
  res
    "RemoveRoleFromDBClusterResponse"
    "fixture/RemoveRoleFromDBClusterResponse.proto"
    rds
    (Proxy :: Proxy RemoveRoleFromDBCluster)

responseFailoverDBCluster :: FailoverDBClusterResponse -> TestTree
responseFailoverDBCluster =
  res
    "FailoverDBClusterResponse"
    "fixture/FailoverDBClusterResponse.proto"
    rds
    (Proxy :: Proxy FailoverDBCluster)

responseRevokeDBSecurityGroupIngress :: RevokeDBSecurityGroupIngressResponse -> TestTree
responseRevokeDBSecurityGroupIngress =
  res
    "RevokeDBSecurityGroupIngressResponse"
    "fixture/RevokeDBSecurityGroupIngressResponse.proto"
    rds
    (Proxy :: Proxy RevokeDBSecurityGroupIngress)

responseModifyDBParameterGroup :: DBParameterGroupNameMessage -> TestTree
responseModifyDBParameterGroup =
  res
    "ModifyDBParameterGroupResponse"
    "fixture/ModifyDBParameterGroupResponse.proto"
    rds
    (Proxy :: Proxy ModifyDBParameterGroup)

responseApplyPendingMaintenanceAction :: ApplyPendingMaintenanceActionResponse -> TestTree
responseApplyPendingMaintenanceAction =
  res
    "ApplyPendingMaintenanceActionResponse"
    "fixture/ApplyPendingMaintenanceActionResponse.proto"
    rds
    (Proxy :: Proxy ApplyPendingMaintenanceAction)

responseCreateOptionGroup :: CreateOptionGroupResponse -> TestTree
responseCreateOptionGroup =
  res
    "CreateOptionGroupResponse"
    "fixture/CreateOptionGroupResponse.proto"
    rds
    (Proxy :: Proxy CreateOptionGroup)

responseDescribeAccountAttributes :: DescribeAccountAttributesResponse -> TestTree
responseDescribeAccountAttributes =
  res
    "DescribeAccountAttributesResponse"
    "fixture/DescribeAccountAttributesResponse.proto"
    rds
    (Proxy :: Proxy DescribeAccountAttributes)

responseDeleteDBSnapshot :: DeleteDBSnapshotResponse -> TestTree
responseDeleteDBSnapshot =
  res
    "DeleteDBSnapshotResponse"
    "fixture/DeleteDBSnapshotResponse.proto"
    rds
    (Proxy :: Proxy DeleteDBSnapshot)

responseDescribeDBClusterParameters :: DescribeDBClusterParametersResponse -> TestTree
responseDescribeDBClusterParameters =
  res
    "DescribeDBClusterParametersResponse"
    "fixture/DescribeDBClusterParametersResponse.proto"
    rds
    (Proxy :: Proxy DescribeDBClusterParameters)

responseDeleteDBSubnetGroup :: DeleteDBSubnetGroupResponse -> TestTree
responseDeleteDBSubnetGroup =
  res
    "DeleteDBSubnetGroupResponse"
    "fixture/DeleteDBSubnetGroupResponse.proto"
    rds
    (Proxy :: Proxy DeleteDBSubnetGroup)

responseCreateDBSecurityGroup :: CreateDBSecurityGroupResponse -> TestTree
responseCreateDBSecurityGroup =
  res
    "CreateDBSecurityGroupResponse"
    "fixture/CreateDBSecurityGroupResponse.proto"
    rds
    (Proxy :: Proxy CreateDBSecurityGroup)

responseModifyCertificates :: ModifyCertificatesResponse -> TestTree
responseModifyCertificates =
  res
    "ModifyCertificatesResponse"
    "fixture/ModifyCertificatesResponse.proto"
    rds
    (Proxy :: Proxy ModifyCertificates)

responseDescribeDBClusterSnapshots :: DescribeDBClusterSnapshotsResponse -> TestTree
responseDescribeDBClusterSnapshots =
  res
    "DescribeDBClusterSnapshotsResponse"
    "fixture/DescribeDBClusterSnapshotsResponse.proto"
    rds
    (Proxy :: Proxy DescribeDBClusterSnapshots)

responseRebootDBInstance :: RebootDBInstanceResponse -> TestTree
responseRebootDBInstance =
  res
    "RebootDBInstanceResponse"
    "fixture/RebootDBInstanceResponse.proto"
    rds
    (Proxy :: Proxy RebootDBInstance)

responseCreateDBSubnetGroup :: CreateDBSubnetGroupResponse -> TestTree
responseCreateDBSubnetGroup =
  res
    "CreateDBSubnetGroupResponse"
    "fixture/CreateDBSubnetGroupResponse.proto"
    rds
    (Proxy :: Proxy CreateDBSubnetGroup)

responseDescribeReservedDBInstancesOfferings :: DescribeReservedDBInstancesOfferingsResponse -> TestTree
responseDescribeReservedDBInstancesOfferings =
  res
    "DescribeReservedDBInstancesOfferingsResponse"
    "fixture/DescribeReservedDBInstancesOfferingsResponse.proto"
    rds
    (Proxy :: Proxy DescribeReservedDBInstancesOfferings)

responseDeleteDBSecurityGroup :: DeleteDBSecurityGroupResponse -> TestTree
responseDeleteDBSecurityGroup =
  res
    "DeleteDBSecurityGroupResponse"
    "fixture/DeleteDBSecurityGroupResponse.proto"
    rds
    (Proxy :: Proxy DeleteDBSecurityGroup)

responseDeleteDBInstance :: DeleteDBInstanceResponse -> TestTree
responseDeleteDBInstance =
  res
    "DeleteDBInstanceResponse"
    "fixture/DeleteDBInstanceResponse.proto"
    rds
    (Proxy :: Proxy DeleteDBInstance)

responseStartActivityStream :: StartActivityStreamResponse -> TestTree
responseStartActivityStream =
  res
    "StartActivityStreamResponse"
    "fixture/StartActivityStreamResponse.proto"
    rds
    (Proxy :: Proxy StartActivityStream)

responseCreateDBInstanceReadReplica :: CreateDBInstanceReadReplicaResponse -> TestTree
responseCreateDBInstanceReadReplica =
  res
    "CreateDBInstanceReadReplicaResponse"
    "fixture/CreateDBInstanceReadReplicaResponse.proto"
    rds
    (Proxy :: Proxy CreateDBInstanceReadReplica)

responseDeleteDBParameterGroup :: DeleteDBParameterGroupResponse -> TestTree
responseDeleteDBParameterGroup =
  res
    "DeleteDBParameterGroupResponse"
    "fixture/DeleteDBParameterGroupResponse.proto"
    rds
    (Proxy :: Proxy DeleteDBParameterGroup)

responseModifyCurrentDBClusterCapacity :: ModifyCurrentDBClusterCapacityResponse -> TestTree
responseModifyCurrentDBClusterCapacity =
  res
    "ModifyCurrentDBClusterCapacityResponse"
    "fixture/ModifyCurrentDBClusterCapacityResponse.proto"
    rds
    (Proxy :: Proxy ModifyCurrentDBClusterCapacity)

responseModifyGlobalCluster :: ModifyGlobalClusterResponse -> TestTree
responseModifyGlobalCluster =
  res
    "ModifyGlobalClusterResponse"
    "fixture/ModifyGlobalClusterResponse.proto"
    rds
    (Proxy :: Proxy ModifyGlobalCluster)

responseRegisterDBProxyTargets :: RegisterDBProxyTargetsResponse -> TestTree
responseRegisterDBProxyTargets =
  res
    "RegisterDBProxyTargetsResponse"
    "fixture/RegisterDBProxyTargetsResponse.proto"
    rds
    (Proxy :: Proxy RegisterDBProxyTargets)

responseDescribeDBSecurityGroups :: DescribeDBSecurityGroupsResponse -> TestTree
responseDescribeDBSecurityGroups =
  res
    "DescribeDBSecurityGroupsResponse"
    "fixture/DescribeDBSecurityGroupsResponse.proto"
    rds
    (Proxy :: Proxy DescribeDBSecurityGroups)

responseCopyOptionGroup :: CopyOptionGroupResponse -> TestTree
responseCopyOptionGroup =
  res
    "CopyOptionGroupResponse"
    "fixture/CopyOptionGroupResponse.proto"
    rds
    (Proxy :: Proxy CopyOptionGroup)

responseRestoreDBClusterToPointInTime :: RestoreDBClusterToPointInTimeResponse -> TestTree
responseRestoreDBClusterToPointInTime =
  res
    "RestoreDBClusterToPointInTimeResponse"
    "fixture/RestoreDBClusterToPointInTimeResponse.proto"
    rds
    (Proxy :: Proxy RestoreDBClusterToPointInTime)

responseDeleteInstallationMedia :: InstallationMedia -> TestTree
responseDeleteInstallationMedia =
  res
    "DeleteInstallationMediaResponse"
    "fixture/DeleteInstallationMediaResponse.proto"
    rds
    (Proxy :: Proxy DeleteInstallationMedia)

responseDescribeDBInstances :: DescribeDBInstancesResponse -> TestTree
responseDescribeDBInstances =
  res
    "DescribeDBInstancesResponse"
    "fixture/DescribeDBInstancesResponse.proto"
    rds
    (Proxy :: Proxy DescribeDBInstances)

responseRestoreDBInstanceFromS3 :: RestoreDBInstanceFromS3Response -> TestTree
responseRestoreDBInstanceFromS3 =
  res
    "RestoreDBInstanceFromS3Response"
    "fixture/RestoreDBInstanceFromS3Response.proto"
    rds
    (Proxy :: Proxy RestoreDBInstanceFromS3)

responseDownloadDBLogFilePortion :: DownloadDBLogFilePortionResponse -> TestTree
responseDownloadDBLogFilePortion =
  res
    "DownloadDBLogFilePortionResponse"
    "fixture/DownloadDBLogFilePortionResponse.proto"
    rds
    (Proxy :: Proxy DownloadDBLogFilePortion)

responseDescribeDBProxies :: DescribeDBProxiesResponse -> TestTree
responseDescribeDBProxies =
  res
    "DescribeDBProxiesResponse"
    "fixture/DescribeDBProxiesResponse.proto"
    rds
    (Proxy :: Proxy DescribeDBProxies)

responseStartExportTask :: ExportTask -> TestTree
responseStartExportTask =
  res
    "StartExportTaskResponse"
    "fixture/StartExportTaskResponse.proto"
    rds
    (Proxy :: Proxy StartExportTask)
