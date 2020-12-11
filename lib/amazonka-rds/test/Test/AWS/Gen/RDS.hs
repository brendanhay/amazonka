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
--             mkStartDBCluster
--
--         , requestDescribeDBClusterParameterGroups $
--             mkDescribeDBClusterParameterGroups
--
--         , requestPromoteReadReplica $
--             mkPromoteReadReplica
--
--         , requestDescribeDBEngineVersions $
--             mkDescribeDBEngineVersions
--
--         , requestStopDBInstance $
--             mkStopDBInstance
--
--         , requestModifyDBClusterEndpoint $
--             mkModifyDBClusterEndpoint
--
--         , requestCopyDBSnapshot $
--             mkCopyDBSnapshot
--
--         , requestAddSourceIdentifierToSubscription $
--             mkAddSourceIdentifierToSubscription
--
--         , requestModifyDBInstance $
--             mkModifyDBInstance
--
--         , requestModifyEventSubscription $
--             mkModifyEventSubscription
--
--         , requestResetDBClusterParameterGroup $
--             mkResetDBClusterParameterGroup
--
--         , requestDescribeCustomAvailabilityZones $
--             mkDescribeCustomAvailabilityZones
--
--         , requestRestoreDBClusterFromS3 $
--             mkRestoreDBClusterFromS3
--
--         , requestDescribeEvents $
--             mkDescribeEvents
--
--         , requestDescribeEngineDefaultParameters $
--             mkDescribeEngineDefaultParameters
--
--         , requestDescribeOptionGroups $
--             mkDescribeOptionGroups
--
--         , requestDescribeDBLogFiles $
--             mkDescribeDBLogFiles
--
--         , requestDescribeDBClusters $
--             mkDescribeDBClusters
--
--         , requestModifyDBSubnetGroup $
--             mkModifyDBSubnetGroup
--
--         , requestListTagsForResource $
--             mkListTagsForResource
--
--         , requestDeleteOptionGroup $
--             mkDeleteOptionGroup
--
--         , requestDeleteDBCluster $
--             mkDeleteDBCluster
--
--         , requestDescribeReservedDBInstances $
--             mkDescribeReservedDBInstances
--
--         , requestCopyDBParameterGroup $
--             mkCopyDBParameterGroup
--
--         , requestRemoveSourceIdentifierFromSubscription $
--             mkRemoveSourceIdentifierFromSubscription
--
--         , requestDeleteCustomAvailabilityZone $
--             mkDeleteCustomAvailabilityZone
--
--         , requestDescribeDBProxyTargets $
--             mkDescribeDBProxyTargets
--
--         , requestDescribeEngineDefaultClusterParameters $
--             mkDescribeEngineDefaultClusterParameters
--
--         , requestDescribeDBSnapshotAttributes $
--             mkDescribeDBSnapshotAttributes
--
--         , requestCreateCustomAvailabilityZone $
--             mkCreateCustomAvailabilityZone
--
--         , requestBacktrackDBCluster $
--             mkBacktrackDBCluster
--
--         , requestDeleteGlobalCluster $
--             mkDeleteGlobalCluster
--
--         , requestPromoteReadReplicaDBCluster $
--             mkPromoteReadReplicaDBCluster
--
--         , requestRemoveTagsFromResource $
--             mkRemoveTagsFromResource
--
--         , requestRestoreDBInstanceFromDBSnapshot $
--             mkRestoreDBInstanceFromDBSnapshot
--
--         , requestDeleteDBProxy $
--             mkDeleteDBProxy
--
--         , requestCreateEventSubscription $
--             mkCreateEventSubscription
--
--         , requestPurchaseReservedDBInstancesOffering $
--             mkPurchaseReservedDBInstancesOffering
--
--         , requestCreateDBInstance $
--             mkCreateDBInstance
--
--         , requestDeleteDBClusterParameterGroup $
--             mkDeleteDBClusterParameterGroup
--
--         , requestDescribeCertificates $
--             mkDescribeCertificates
--
--         , requestAuthorizeDBSecurityGroupIngress $
--             mkAuthorizeDBSecurityGroupIngress
--
--         , requestRemoveRoleFromDBInstance $
--             mkRemoveRoleFromDBInstance
--
--         , requestDescribeSourceRegions $
--             mkDescribeSourceRegions
--
--         , requestCreateDBClusterEndpoint $
--             mkCreateDBClusterEndpoint
--
--         , requestRestoreDBClusterFromSnapshot $
--             mkRestoreDBClusterFromSnapshot
--
--         , requestDescribeOrderableDBInstanceOptions $
--             mkDescribeOrderableDBInstanceOptions
--
--         , requestDeleteDBClusterEndpoint $
--             mkDeleteDBClusterEndpoint
--
--         , requestCreateDBProxy $
--             mkCreateDBProxy
--
--         , requestDeleteDBInstanceAutomatedBackup $
--             mkDeleteDBInstanceAutomatedBackup
--
--         , requestCreateDBClusterParameterGroup $
--             mkCreateDBClusterParameterGroup
--
--         , requestCreateDBSnapshot $
--             mkCreateDBSnapshot
--
--         , requestDeleteEventSubscription $
--             mkDeleteEventSubscription
--
--         , requestDescribeDBClusterBacktracks $
--             mkDescribeDBClusterBacktracks
--
--         , requestDescribeDBParameterGroups $
--             mkDescribeDBParameterGroups
--
--         , requestModifyDBSnapshotAttribute $
--             mkModifyDBSnapshotAttribute
--
--         , requestDescribeDBInstanceAutomatedBackups $
--             mkDescribeDBInstanceAutomatedBackups
--
--         , requestRemoveFromGlobalCluster $
--             mkRemoveFromGlobalCluster
--
--         , requestAddRoleToDBInstance $
--             mkAddRoleToDBInstance
--
--         , requestDeleteDBClusterSnapshot $
--             mkDeleteDBClusterSnapshot
--
--         , requestDescribeValidDBInstanceModifications $
--             mkDescribeValidDBInstanceModifications
--
--         , requestDescribeDBClusterEndpoints $
--             mkDescribeDBClusterEndpoints
--
--         , requestDescribeOptionGroupOptions $
--             mkDescribeOptionGroupOptions
--
--         , requestDescribeEventSubscriptions $
--             mkDescribeEventSubscriptions
--
--         , requestAddTagsToResource $
--             mkAddTagsToResource
--
--         , requestDescribeDBParameters $
--             mkDescribeDBParameters
--
--         , requestStopActivityStream $
--             mkStopActivityStream
--
--         , requestCreateDBClusterSnapshot $
--             mkCreateDBClusterSnapshot
--
--         , requestDescribeDBSnapshots $
--             mkDescribeDBSnapshots
--
--         , requestModifyDBProxyTargetGroup $
--             mkModifyDBProxyTargetGroup
--
--         , requestDescribeDBSubnetGroups $
--             mkDescribeDBSubnetGroups
--
--         , requestModifyOptionGroup $
--             mkModifyOptionGroup
--
--         , requestStopDBCluster $
--             mkStopDBCluster
--
--         , requestCreateDBParameterGroup $
--             mkCreateDBParameterGroup
--
--         , requestModifyDBClusterSnapshotAttribute $
--             mkModifyDBClusterSnapshotAttribute
--
--         , requestModifyDBCluster $
--             mkModifyDBCluster
--
--         , requestCopyDBClusterParameterGroup $
--             mkCopyDBClusterParameterGroup
--
--         , requestDescribeEventCategories $
--             mkDescribeEventCategories
--
--         , requestDescribeGlobalClusters $
--             mkDescribeGlobalClusters
--
--         , requestStartDBInstance $
--             mkStartDBInstance
--
--         , requestDescribeExportTasks $
--             mkDescribeExportTasks
--
--         , requestCancelExportTask $
--             mkCancelExportTask
--
--         , requestModifyDBClusterParameterGroup $
--             mkModifyDBClusterParameterGroup
--
--         , requestRestoreDBInstanceToPointInTime $
--             mkRestoreDBInstanceToPointInTime
--
--         , requestDescribeDBClusterSnapshotAttributes $
--             mkDescribeDBClusterSnapshotAttributes
--
--         , requestModifyDBSnapshot $
--             mkModifyDBSnapshot
--
--         , requestDescribeDBProxyTargetGroups $
--             mkDescribeDBProxyTargetGroups
--
--         , requestModifyDBProxy $
--             mkModifyDBProxy
--
--         , requestDescribePendingMaintenanceActions $
--             mkDescribePendingMaintenanceActions
--
--         , requestAddRoleToDBCluster $
--             mkAddRoleToDBCluster
--
--         , requestCopyDBClusterSnapshot $
--             mkCopyDBClusterSnapshot
--
--         , requestImportInstallationMedia $
--             mkImportInstallationMedia
--
--         , requestCreateGlobalCluster $
--             mkCreateGlobalCluster
--
--         , requestResetDBParameterGroup $
--             mkResetDBParameterGroup
--
--         , requestDescribeInstallationMedia $
--             mkDescribeInstallationMedia
--
--         , requestDeregisterDBProxyTargets $
--             mkDeregisterDBProxyTargets
--
--         , requestCreateDBCluster $
--             mkCreateDBCluster
--
--         , requestRemoveRoleFromDBCluster $
--             mkRemoveRoleFromDBCluster
--
--         , requestFailoverDBCluster $
--             mkFailoverDBCluster
--
--         , requestRevokeDBSecurityGroupIngress $
--             mkRevokeDBSecurityGroupIngress
--
--         , requestModifyDBParameterGroup $
--             mkModifyDBParameterGroup
--
--         , requestApplyPendingMaintenanceAction $
--             mkApplyPendingMaintenanceAction
--
--         , requestCreateOptionGroup $
--             mkCreateOptionGroup
--
--         , requestDescribeAccountAttributes $
--             mkDescribeAccountAttributes
--
--         , requestDeleteDBSnapshot $
--             mkDeleteDBSnapshot
--
--         , requestDescribeDBClusterParameters $
--             mkDescribeDBClusterParameters
--
--         , requestDeleteDBSubnetGroup $
--             mkDeleteDBSubnetGroup
--
--         , requestCreateDBSecurityGroup $
--             mkCreateDBSecurityGroup
--
--         , requestModifyCertificates $
--             mkModifyCertificates
--
--         , requestDescribeDBClusterSnapshots $
--             mkDescribeDBClusterSnapshots
--
--         , requestRebootDBInstance $
--             mkRebootDBInstance
--
--         , requestCreateDBSubnetGroup $
--             mkCreateDBSubnetGroup
--
--         , requestDescribeReservedDBInstancesOfferings $
--             mkDescribeReservedDBInstancesOfferings
--
--         , requestDeleteDBSecurityGroup $
--             mkDeleteDBSecurityGroup
--
--         , requestDeleteDBInstance $
--             mkDeleteDBInstance
--
--         , requestStartActivityStream $
--             mkStartActivityStream
--
--         , requestCreateDBInstanceReadReplica $
--             mkCreateDBInstanceReadReplica
--
--         , requestDeleteDBParameterGroup $
--             mkDeleteDBParameterGroup
--
--         , requestModifyCurrentDBClusterCapacity $
--             mkModifyCurrentDBClusterCapacity
--
--         , requestModifyGlobalCluster $
--             mkModifyGlobalCluster
--
--         , requestRegisterDBProxyTargets $
--             mkRegisterDBProxyTargets
--
--         , requestDescribeDBSecurityGroups $
--             mkDescribeDBSecurityGroups
--
--         , requestCopyOptionGroup $
--             mkCopyOptionGroup
--
--         , requestRestoreDBClusterToPointInTime $
--             mkRestoreDBClusterToPointInTime
--
--         , requestDeleteInstallationMedia $
--             mkDeleteInstallationMedia
--
--         , requestDescribeDBInstances $
--             mkDescribeDBInstances
--
--         , requestRestoreDBInstanceFromS3 $
--             mkRestoreDBInstanceFromS3
--
--         , requestDownloadDBLogFilePortion $
--             mkDownloadDBLogFilePortion
--
--         , requestDescribeDBProxies $
--             mkDescribeDBProxies
--
--         , requestStartExportTask $
--             mkStartExportTask
--
--           ]

--     , testGroup "response"
--         [ responseStartDBCluster $
--             mkStartDBClusterResponse
--
--         , responseDescribeDBClusterParameterGroups $
--             mkDescribeDBClusterParameterGroupsResponse
--
--         , responsePromoteReadReplica $
--             mkPromoteReadReplicaResponse
--
--         , responseDescribeDBEngineVersions $
--             mkDescribeDBEngineVersionsResponse
--
--         , responseStopDBInstance $
--             mkStopDBInstanceResponse
--
--         , responseModifyDBClusterEndpoint $
--             mkDBClusterEndpoint
--
--         , responseCopyDBSnapshot $
--             mkCopyDBSnapshotResponse
--
--         , responseAddSourceIdentifierToSubscription $
--             mkAddSourceIdentifierToSubscriptionResponse
--
--         , responseModifyDBInstance $
--             mkModifyDBInstanceResponse
--
--         , responseModifyEventSubscription $
--             mkModifyEventSubscriptionResponse
--
--         , responseResetDBClusterParameterGroup $
--             mkDBClusterParameterGroupNameMessage
--
--         , responseDescribeCustomAvailabilityZones $
--             mkDescribeCustomAvailabilityZonesResponse
--
--         , responseRestoreDBClusterFromS3 $
--             mkRestoreDBClusterFromS3Response
--
--         , responseDescribeEvents $
--             mkDescribeEventsResponse
--
--         , responseDescribeEngineDefaultParameters $
--             mkDescribeEngineDefaultParametersResponse
--
--         , responseDescribeOptionGroups $
--             mkDescribeOptionGroupsResponse
--
--         , responseDescribeDBLogFiles $
--             mkDescribeDBLogFilesResponse
--
--         , responseDescribeDBClusters $
--             mkDescribeDBClustersResponse
--
--         , responseModifyDBSubnetGroup $
--             mkModifyDBSubnetGroupResponse
--
--         , responseListTagsForResource $
--             mkListTagsForResourceResponse
--
--         , responseDeleteOptionGroup $
--             mkDeleteOptionGroupResponse
--
--         , responseDeleteDBCluster $
--             mkDeleteDBClusterResponse
--
--         , responseDescribeReservedDBInstances $
--             mkDescribeReservedDBInstancesResponse
--
--         , responseCopyDBParameterGroup $
--             mkCopyDBParameterGroupResponse
--
--         , responseRemoveSourceIdentifierFromSubscription $
--             mkRemoveSourceIdentifierFromSubscriptionResponse
--
--         , responseDeleteCustomAvailabilityZone $
--             mkDeleteCustomAvailabilityZoneResponse
--
--         , responseDescribeDBProxyTargets $
--             mkDescribeDBProxyTargetsResponse
--
--         , responseDescribeEngineDefaultClusterParameters $
--             mkDescribeEngineDefaultClusterParametersResponse
--
--         , responseDescribeDBSnapshotAttributes $
--             mkDescribeDBSnapshotAttributesResponse
--
--         , responseCreateCustomAvailabilityZone $
--             mkCreateCustomAvailabilityZoneResponse
--
--         , responseBacktrackDBCluster $
--             mkDBClusterBacktrack
--
--         , responseDeleteGlobalCluster $
--             mkDeleteGlobalClusterResponse
--
--         , responsePromoteReadReplicaDBCluster $
--             mkPromoteReadReplicaDBClusterResponse
--
--         , responseRemoveTagsFromResource $
--             mkRemoveTagsFromResourceResponse
--
--         , responseRestoreDBInstanceFromDBSnapshot $
--             mkRestoreDBInstanceFromDBSnapshotResponse
--
--         , responseDeleteDBProxy $
--             mkDeleteDBProxyResponse
--
--         , responseCreateEventSubscription $
--             mkCreateEventSubscriptionResponse
--
--         , responsePurchaseReservedDBInstancesOffering $
--             mkPurchaseReservedDBInstancesOfferingResponse
--
--         , responseCreateDBInstance $
--             mkCreateDBInstanceResponse
--
--         , responseDeleteDBClusterParameterGroup $
--             mkDeleteDBClusterParameterGroupResponse
--
--         , responseDescribeCertificates $
--             mkDescribeCertificatesResponse
--
--         , responseAuthorizeDBSecurityGroupIngress $
--             mkAuthorizeDBSecurityGroupIngressResponse
--
--         , responseRemoveRoleFromDBInstance $
--             mkRemoveRoleFromDBInstanceResponse
--
--         , responseDescribeSourceRegions $
--             mkDescribeSourceRegionsResponse
--
--         , responseCreateDBClusterEndpoint $
--             mkDBClusterEndpoint
--
--         , responseRestoreDBClusterFromSnapshot $
--             mkRestoreDBClusterFromSnapshotResponse
--
--         , responseDescribeOrderableDBInstanceOptions $
--             mkDescribeOrderableDBInstanceOptionsResponse
--
--         , responseDeleteDBClusterEndpoint $
--             mkDBClusterEndpoint
--
--         , responseCreateDBProxy $
--             mkCreateDBProxyResponse
--
--         , responseDeleteDBInstanceAutomatedBackup $
--             mkDeleteDBInstanceAutomatedBackupResponse
--
--         , responseCreateDBClusterParameterGroup $
--             mkCreateDBClusterParameterGroupResponse
--
--         , responseCreateDBSnapshot $
--             mkCreateDBSnapshotResponse
--
--         , responseDeleteEventSubscription $
--             mkDeleteEventSubscriptionResponse
--
--         , responseDescribeDBClusterBacktracks $
--             mkDescribeDBClusterBacktracksResponse
--
--         , responseDescribeDBParameterGroups $
--             mkDescribeDBParameterGroupsResponse
--
--         , responseModifyDBSnapshotAttribute $
--             mkModifyDBSnapshotAttributeResponse
--
--         , responseDescribeDBInstanceAutomatedBackups $
--             mkDescribeDBInstanceAutomatedBackupsResponse
--
--         , responseRemoveFromGlobalCluster $
--             mkRemoveFromGlobalClusterResponse
--
--         , responseAddRoleToDBInstance $
--             mkAddRoleToDBInstanceResponse
--
--         , responseDeleteDBClusterSnapshot $
--             mkDeleteDBClusterSnapshotResponse
--
--         , responseDescribeValidDBInstanceModifications $
--             mkDescribeValidDBInstanceModificationsResponse
--
--         , responseDescribeDBClusterEndpoints $
--             mkDescribeDBClusterEndpointsResponse
--
--         , responseDescribeOptionGroupOptions $
--             mkDescribeOptionGroupOptionsResponse
--
--         , responseDescribeEventSubscriptions $
--             mkDescribeEventSubscriptionsResponse
--
--         , responseAddTagsToResource $
--             mkAddTagsToResourceResponse
--
--         , responseDescribeDBParameters $
--             mkDescribeDBParametersResponse
--
--         , responseStopActivityStream $
--             mkStopActivityStreamResponse
--
--         , responseCreateDBClusterSnapshot $
--             mkCreateDBClusterSnapshotResponse
--
--         , responseDescribeDBSnapshots $
--             mkDescribeDBSnapshotsResponse
--
--         , responseModifyDBProxyTargetGroup $
--             mkModifyDBProxyTargetGroupResponse
--
--         , responseDescribeDBSubnetGroups $
--             mkDescribeDBSubnetGroupsResponse
--
--         , responseModifyOptionGroup $
--             mkModifyOptionGroupResponse
--
--         , responseStopDBCluster $
--             mkStopDBClusterResponse
--
--         , responseCreateDBParameterGroup $
--             mkCreateDBParameterGroupResponse
--
--         , responseModifyDBClusterSnapshotAttribute $
--             mkModifyDBClusterSnapshotAttributeResponse
--
--         , responseModifyDBCluster $
--             mkModifyDBClusterResponse
--
--         , responseCopyDBClusterParameterGroup $
--             mkCopyDBClusterParameterGroupResponse
--
--         , responseDescribeEventCategories $
--             mkDescribeEventCategoriesResponse
--
--         , responseDescribeGlobalClusters $
--             mkDescribeGlobalClustersResponse
--
--         , responseStartDBInstance $
--             mkStartDBInstanceResponse
--
--         , responseDescribeExportTasks $
--             mkDescribeExportTasksResponse
--
--         , responseCancelExportTask $
--             mkExportTask
--
--         , responseModifyDBClusterParameterGroup $
--             mkDBClusterParameterGroupNameMessage
--
--         , responseRestoreDBInstanceToPointInTime $
--             mkRestoreDBInstanceToPointInTimeResponse
--
--         , responseDescribeDBClusterSnapshotAttributes $
--             mkDescribeDBClusterSnapshotAttributesResponse
--
--         , responseModifyDBSnapshot $
--             mkModifyDBSnapshotResponse
--
--         , responseDescribeDBProxyTargetGroups $
--             mkDescribeDBProxyTargetGroupsResponse
--
--         , responseModifyDBProxy $
--             mkModifyDBProxyResponse
--
--         , responseDescribePendingMaintenanceActions $
--             mkDescribePendingMaintenanceActionsResponse
--
--         , responseAddRoleToDBCluster $
--             mkAddRoleToDBClusterResponse
--
--         , responseCopyDBClusterSnapshot $
--             mkCopyDBClusterSnapshotResponse
--
--         , responseImportInstallationMedia $
--             mkInstallationMedia
--
--         , responseCreateGlobalCluster $
--             mkCreateGlobalClusterResponse
--
--         , responseResetDBParameterGroup $
--             mkDBParameterGroupNameMessage
--
--         , responseDescribeInstallationMedia $
--             mkDescribeInstallationMediaResponse
--
--         , responseDeregisterDBProxyTargets $
--             mkDeregisterDBProxyTargetsResponse
--
--         , responseCreateDBCluster $
--             mkCreateDBClusterResponse
--
--         , responseRemoveRoleFromDBCluster $
--             mkRemoveRoleFromDBClusterResponse
--
--         , responseFailoverDBCluster $
--             mkFailoverDBClusterResponse
--
--         , responseRevokeDBSecurityGroupIngress $
--             mkRevokeDBSecurityGroupIngressResponse
--
--         , responseModifyDBParameterGroup $
--             mkDBParameterGroupNameMessage
--
--         , responseApplyPendingMaintenanceAction $
--             mkApplyPendingMaintenanceActionResponse
--
--         , responseCreateOptionGroup $
--             mkCreateOptionGroupResponse
--
--         , responseDescribeAccountAttributes $
--             mkDescribeAccountAttributesResponse
--
--         , responseDeleteDBSnapshot $
--             mkDeleteDBSnapshotResponse
--
--         , responseDescribeDBClusterParameters $
--             mkDescribeDBClusterParametersResponse
--
--         , responseDeleteDBSubnetGroup $
--             mkDeleteDBSubnetGroupResponse
--
--         , responseCreateDBSecurityGroup $
--             mkCreateDBSecurityGroupResponse
--
--         , responseModifyCertificates $
--             mkModifyCertificatesResponse
--
--         , responseDescribeDBClusterSnapshots $
--             mkDescribeDBClusterSnapshotsResponse
--
--         , responseRebootDBInstance $
--             mkRebootDBInstanceResponse
--
--         , responseCreateDBSubnetGroup $
--             mkCreateDBSubnetGroupResponse
--
--         , responseDescribeReservedDBInstancesOfferings $
--             mkDescribeReservedDBInstancesOfferingsResponse
--
--         , responseDeleteDBSecurityGroup $
--             mkDeleteDBSecurityGroupResponse
--
--         , responseDeleteDBInstance $
--             mkDeleteDBInstanceResponse
--
--         , responseStartActivityStream $
--             mkStartActivityStreamResponse
--
--         , responseCreateDBInstanceReadReplica $
--             mkCreateDBInstanceReadReplicaResponse
--
--         , responseDeleteDBParameterGroup $
--             mkDeleteDBParameterGroupResponse
--
--         , responseModifyCurrentDBClusterCapacity $
--             mkModifyCurrentDBClusterCapacityResponse
--
--         , responseModifyGlobalCluster $
--             mkModifyGlobalClusterResponse
--
--         , responseRegisterDBProxyTargets $
--             mkRegisterDBProxyTargetsResponse
--
--         , responseDescribeDBSecurityGroups $
--             mkDescribeDBSecurityGroupsResponse
--
--         , responseCopyOptionGroup $
--             mkCopyOptionGroupResponse
--
--         , responseRestoreDBClusterToPointInTime $
--             mkRestoreDBClusterToPointInTimeResponse
--
--         , responseDeleteInstallationMedia $
--             mkInstallationMedia
--
--         , responseDescribeDBInstances $
--             mkDescribeDBInstancesResponse
--
--         , responseRestoreDBInstanceFromS3 $
--             mkRestoreDBInstanceFromS3Response
--
--         , responseDownloadDBLogFilePortion $
--             mkDownloadDBLogFilePortionResponse
--
--         , responseDescribeDBProxies $
--             mkDescribeDBProxiesResponse
--
--         , responseStartExportTask $
--             mkExportTask
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
    rdsService
    (Proxy :: Proxy StartDBCluster)

responseDescribeDBClusterParameterGroups :: DescribeDBClusterParameterGroupsResponse -> TestTree
responseDescribeDBClusterParameterGroups =
  res
    "DescribeDBClusterParameterGroupsResponse"
    "fixture/DescribeDBClusterParameterGroupsResponse.proto"
    rdsService
    (Proxy :: Proxy DescribeDBClusterParameterGroups)

responsePromoteReadReplica :: PromoteReadReplicaResponse -> TestTree
responsePromoteReadReplica =
  res
    "PromoteReadReplicaResponse"
    "fixture/PromoteReadReplicaResponse.proto"
    rdsService
    (Proxy :: Proxy PromoteReadReplica)

responseDescribeDBEngineVersions :: DescribeDBEngineVersionsResponse -> TestTree
responseDescribeDBEngineVersions =
  res
    "DescribeDBEngineVersionsResponse"
    "fixture/DescribeDBEngineVersionsResponse.proto"
    rdsService
    (Proxy :: Proxy DescribeDBEngineVersions)

responseStopDBInstance :: StopDBInstanceResponse -> TestTree
responseStopDBInstance =
  res
    "StopDBInstanceResponse"
    "fixture/StopDBInstanceResponse.proto"
    rdsService
    (Proxy :: Proxy StopDBInstance)

responseModifyDBClusterEndpoint :: DBClusterEndpoint -> TestTree
responseModifyDBClusterEndpoint =
  res
    "ModifyDBClusterEndpointResponse"
    "fixture/ModifyDBClusterEndpointResponse.proto"
    rdsService
    (Proxy :: Proxy ModifyDBClusterEndpoint)

responseCopyDBSnapshot :: CopyDBSnapshotResponse -> TestTree
responseCopyDBSnapshot =
  res
    "CopyDBSnapshotResponse"
    "fixture/CopyDBSnapshotResponse.proto"
    rdsService
    (Proxy :: Proxy CopyDBSnapshot)

responseAddSourceIdentifierToSubscription :: AddSourceIdentifierToSubscriptionResponse -> TestTree
responseAddSourceIdentifierToSubscription =
  res
    "AddSourceIdentifierToSubscriptionResponse"
    "fixture/AddSourceIdentifierToSubscriptionResponse.proto"
    rdsService
    (Proxy :: Proxy AddSourceIdentifierToSubscription)

responseModifyDBInstance :: ModifyDBInstanceResponse -> TestTree
responseModifyDBInstance =
  res
    "ModifyDBInstanceResponse"
    "fixture/ModifyDBInstanceResponse.proto"
    rdsService
    (Proxy :: Proxy ModifyDBInstance)

responseModifyEventSubscription :: ModifyEventSubscriptionResponse -> TestTree
responseModifyEventSubscription =
  res
    "ModifyEventSubscriptionResponse"
    "fixture/ModifyEventSubscriptionResponse.proto"
    rdsService
    (Proxy :: Proxy ModifyEventSubscription)

responseResetDBClusterParameterGroup :: DBClusterParameterGroupNameMessage -> TestTree
responseResetDBClusterParameterGroup =
  res
    "ResetDBClusterParameterGroupResponse"
    "fixture/ResetDBClusterParameterGroupResponse.proto"
    rdsService
    (Proxy :: Proxy ResetDBClusterParameterGroup)

responseDescribeCustomAvailabilityZones :: DescribeCustomAvailabilityZonesResponse -> TestTree
responseDescribeCustomAvailabilityZones =
  res
    "DescribeCustomAvailabilityZonesResponse"
    "fixture/DescribeCustomAvailabilityZonesResponse.proto"
    rdsService
    (Proxy :: Proxy DescribeCustomAvailabilityZones)

responseRestoreDBClusterFromS3 :: RestoreDBClusterFromS3Response -> TestTree
responseRestoreDBClusterFromS3 =
  res
    "RestoreDBClusterFromS3Response"
    "fixture/RestoreDBClusterFromS3Response.proto"
    rdsService
    (Proxy :: Proxy RestoreDBClusterFromS3)

responseDescribeEvents :: DescribeEventsResponse -> TestTree
responseDescribeEvents =
  res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse.proto"
    rdsService
    (Proxy :: Proxy DescribeEvents)

responseDescribeEngineDefaultParameters :: DescribeEngineDefaultParametersResponse -> TestTree
responseDescribeEngineDefaultParameters =
  res
    "DescribeEngineDefaultParametersResponse"
    "fixture/DescribeEngineDefaultParametersResponse.proto"
    rdsService
    (Proxy :: Proxy DescribeEngineDefaultParameters)

responseDescribeOptionGroups :: DescribeOptionGroupsResponse -> TestTree
responseDescribeOptionGroups =
  res
    "DescribeOptionGroupsResponse"
    "fixture/DescribeOptionGroupsResponse.proto"
    rdsService
    (Proxy :: Proxy DescribeOptionGroups)

responseDescribeDBLogFiles :: DescribeDBLogFilesResponse -> TestTree
responseDescribeDBLogFiles =
  res
    "DescribeDBLogFilesResponse"
    "fixture/DescribeDBLogFilesResponse.proto"
    rdsService
    (Proxy :: Proxy DescribeDBLogFiles)

responseDescribeDBClusters :: DescribeDBClustersResponse -> TestTree
responseDescribeDBClusters =
  res
    "DescribeDBClustersResponse"
    "fixture/DescribeDBClustersResponse.proto"
    rdsService
    (Proxy :: Proxy DescribeDBClusters)

responseModifyDBSubnetGroup :: ModifyDBSubnetGroupResponse -> TestTree
responseModifyDBSubnetGroup =
  res
    "ModifyDBSubnetGroupResponse"
    "fixture/ModifyDBSubnetGroupResponse.proto"
    rdsService
    (Proxy :: Proxy ModifyDBSubnetGroup)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    rdsService
    (Proxy :: Proxy ListTagsForResource)

responseDeleteOptionGroup :: DeleteOptionGroupResponse -> TestTree
responseDeleteOptionGroup =
  res
    "DeleteOptionGroupResponse"
    "fixture/DeleteOptionGroupResponse.proto"
    rdsService
    (Proxy :: Proxy DeleteOptionGroup)

responseDeleteDBCluster :: DeleteDBClusterResponse -> TestTree
responseDeleteDBCluster =
  res
    "DeleteDBClusterResponse"
    "fixture/DeleteDBClusterResponse.proto"
    rdsService
    (Proxy :: Proxy DeleteDBCluster)

responseDescribeReservedDBInstances :: DescribeReservedDBInstancesResponse -> TestTree
responseDescribeReservedDBInstances =
  res
    "DescribeReservedDBInstancesResponse"
    "fixture/DescribeReservedDBInstancesResponse.proto"
    rdsService
    (Proxy :: Proxy DescribeReservedDBInstances)

responseCopyDBParameterGroup :: CopyDBParameterGroupResponse -> TestTree
responseCopyDBParameterGroup =
  res
    "CopyDBParameterGroupResponse"
    "fixture/CopyDBParameterGroupResponse.proto"
    rdsService
    (Proxy :: Proxy CopyDBParameterGroup)

responseRemoveSourceIdentifierFromSubscription :: RemoveSourceIdentifierFromSubscriptionResponse -> TestTree
responseRemoveSourceIdentifierFromSubscription =
  res
    "RemoveSourceIdentifierFromSubscriptionResponse"
    "fixture/RemoveSourceIdentifierFromSubscriptionResponse.proto"
    rdsService
    (Proxy :: Proxy RemoveSourceIdentifierFromSubscription)

responseDeleteCustomAvailabilityZone :: DeleteCustomAvailabilityZoneResponse -> TestTree
responseDeleteCustomAvailabilityZone =
  res
    "DeleteCustomAvailabilityZoneResponse"
    "fixture/DeleteCustomAvailabilityZoneResponse.proto"
    rdsService
    (Proxy :: Proxy DeleteCustomAvailabilityZone)

responseDescribeDBProxyTargets :: DescribeDBProxyTargetsResponse -> TestTree
responseDescribeDBProxyTargets =
  res
    "DescribeDBProxyTargetsResponse"
    "fixture/DescribeDBProxyTargetsResponse.proto"
    rdsService
    (Proxy :: Proxy DescribeDBProxyTargets)

responseDescribeEngineDefaultClusterParameters :: DescribeEngineDefaultClusterParametersResponse -> TestTree
responseDescribeEngineDefaultClusterParameters =
  res
    "DescribeEngineDefaultClusterParametersResponse"
    "fixture/DescribeEngineDefaultClusterParametersResponse.proto"
    rdsService
    (Proxy :: Proxy DescribeEngineDefaultClusterParameters)

responseDescribeDBSnapshotAttributes :: DescribeDBSnapshotAttributesResponse -> TestTree
responseDescribeDBSnapshotAttributes =
  res
    "DescribeDBSnapshotAttributesResponse"
    "fixture/DescribeDBSnapshotAttributesResponse.proto"
    rdsService
    (Proxy :: Proxy DescribeDBSnapshotAttributes)

responseCreateCustomAvailabilityZone :: CreateCustomAvailabilityZoneResponse -> TestTree
responseCreateCustomAvailabilityZone =
  res
    "CreateCustomAvailabilityZoneResponse"
    "fixture/CreateCustomAvailabilityZoneResponse.proto"
    rdsService
    (Proxy :: Proxy CreateCustomAvailabilityZone)

responseBacktrackDBCluster :: DBClusterBacktrack -> TestTree
responseBacktrackDBCluster =
  res
    "BacktrackDBClusterResponse"
    "fixture/BacktrackDBClusterResponse.proto"
    rdsService
    (Proxy :: Proxy BacktrackDBCluster)

responseDeleteGlobalCluster :: DeleteGlobalClusterResponse -> TestTree
responseDeleteGlobalCluster =
  res
    "DeleteGlobalClusterResponse"
    "fixture/DeleteGlobalClusterResponse.proto"
    rdsService
    (Proxy :: Proxy DeleteGlobalCluster)

responsePromoteReadReplicaDBCluster :: PromoteReadReplicaDBClusterResponse -> TestTree
responsePromoteReadReplicaDBCluster =
  res
    "PromoteReadReplicaDBClusterResponse"
    "fixture/PromoteReadReplicaDBClusterResponse.proto"
    rdsService
    (Proxy :: Proxy PromoteReadReplicaDBCluster)

responseRemoveTagsFromResource :: RemoveTagsFromResourceResponse -> TestTree
responseRemoveTagsFromResource =
  res
    "RemoveTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse.proto"
    rdsService
    (Proxy :: Proxy RemoveTagsFromResource)

responseRestoreDBInstanceFromDBSnapshot :: RestoreDBInstanceFromDBSnapshotResponse -> TestTree
responseRestoreDBInstanceFromDBSnapshot =
  res
    "RestoreDBInstanceFromDBSnapshotResponse"
    "fixture/RestoreDBInstanceFromDBSnapshotResponse.proto"
    rdsService
    (Proxy :: Proxy RestoreDBInstanceFromDBSnapshot)

responseDeleteDBProxy :: DeleteDBProxyResponse -> TestTree
responseDeleteDBProxy =
  res
    "DeleteDBProxyResponse"
    "fixture/DeleteDBProxyResponse.proto"
    rdsService
    (Proxy :: Proxy DeleteDBProxy)

responseCreateEventSubscription :: CreateEventSubscriptionResponse -> TestTree
responseCreateEventSubscription =
  res
    "CreateEventSubscriptionResponse"
    "fixture/CreateEventSubscriptionResponse.proto"
    rdsService
    (Proxy :: Proxy CreateEventSubscription)

responsePurchaseReservedDBInstancesOffering :: PurchaseReservedDBInstancesOfferingResponse -> TestTree
responsePurchaseReservedDBInstancesOffering =
  res
    "PurchaseReservedDBInstancesOfferingResponse"
    "fixture/PurchaseReservedDBInstancesOfferingResponse.proto"
    rdsService
    (Proxy :: Proxy PurchaseReservedDBInstancesOffering)

responseCreateDBInstance :: CreateDBInstanceResponse -> TestTree
responseCreateDBInstance =
  res
    "CreateDBInstanceResponse"
    "fixture/CreateDBInstanceResponse.proto"
    rdsService
    (Proxy :: Proxy CreateDBInstance)

responseDeleteDBClusterParameterGroup :: DeleteDBClusterParameterGroupResponse -> TestTree
responseDeleteDBClusterParameterGroup =
  res
    "DeleteDBClusterParameterGroupResponse"
    "fixture/DeleteDBClusterParameterGroupResponse.proto"
    rdsService
    (Proxy :: Proxy DeleteDBClusterParameterGroup)

responseDescribeCertificates :: DescribeCertificatesResponse -> TestTree
responseDescribeCertificates =
  res
    "DescribeCertificatesResponse"
    "fixture/DescribeCertificatesResponse.proto"
    rdsService
    (Proxy :: Proxy DescribeCertificates)

responseAuthorizeDBSecurityGroupIngress :: AuthorizeDBSecurityGroupIngressResponse -> TestTree
responseAuthorizeDBSecurityGroupIngress =
  res
    "AuthorizeDBSecurityGroupIngressResponse"
    "fixture/AuthorizeDBSecurityGroupIngressResponse.proto"
    rdsService
    (Proxy :: Proxy AuthorizeDBSecurityGroupIngress)

responseRemoveRoleFromDBInstance :: RemoveRoleFromDBInstanceResponse -> TestTree
responseRemoveRoleFromDBInstance =
  res
    "RemoveRoleFromDBInstanceResponse"
    "fixture/RemoveRoleFromDBInstanceResponse.proto"
    rdsService
    (Proxy :: Proxy RemoveRoleFromDBInstance)

responseDescribeSourceRegions :: DescribeSourceRegionsResponse -> TestTree
responseDescribeSourceRegions =
  res
    "DescribeSourceRegionsResponse"
    "fixture/DescribeSourceRegionsResponse.proto"
    rdsService
    (Proxy :: Proxy DescribeSourceRegions)

responseCreateDBClusterEndpoint :: DBClusterEndpoint -> TestTree
responseCreateDBClusterEndpoint =
  res
    "CreateDBClusterEndpointResponse"
    "fixture/CreateDBClusterEndpointResponse.proto"
    rdsService
    (Proxy :: Proxy CreateDBClusterEndpoint)

responseRestoreDBClusterFromSnapshot :: RestoreDBClusterFromSnapshotResponse -> TestTree
responseRestoreDBClusterFromSnapshot =
  res
    "RestoreDBClusterFromSnapshotResponse"
    "fixture/RestoreDBClusterFromSnapshotResponse.proto"
    rdsService
    (Proxy :: Proxy RestoreDBClusterFromSnapshot)

responseDescribeOrderableDBInstanceOptions :: DescribeOrderableDBInstanceOptionsResponse -> TestTree
responseDescribeOrderableDBInstanceOptions =
  res
    "DescribeOrderableDBInstanceOptionsResponse"
    "fixture/DescribeOrderableDBInstanceOptionsResponse.proto"
    rdsService
    (Proxy :: Proxy DescribeOrderableDBInstanceOptions)

responseDeleteDBClusterEndpoint :: DBClusterEndpoint -> TestTree
responseDeleteDBClusterEndpoint =
  res
    "DeleteDBClusterEndpointResponse"
    "fixture/DeleteDBClusterEndpointResponse.proto"
    rdsService
    (Proxy :: Proxy DeleteDBClusterEndpoint)

responseCreateDBProxy :: CreateDBProxyResponse -> TestTree
responseCreateDBProxy =
  res
    "CreateDBProxyResponse"
    "fixture/CreateDBProxyResponse.proto"
    rdsService
    (Proxy :: Proxy CreateDBProxy)

responseDeleteDBInstanceAutomatedBackup :: DeleteDBInstanceAutomatedBackupResponse -> TestTree
responseDeleteDBInstanceAutomatedBackup =
  res
    "DeleteDBInstanceAutomatedBackupResponse"
    "fixture/DeleteDBInstanceAutomatedBackupResponse.proto"
    rdsService
    (Proxy :: Proxy DeleteDBInstanceAutomatedBackup)

responseCreateDBClusterParameterGroup :: CreateDBClusterParameterGroupResponse -> TestTree
responseCreateDBClusterParameterGroup =
  res
    "CreateDBClusterParameterGroupResponse"
    "fixture/CreateDBClusterParameterGroupResponse.proto"
    rdsService
    (Proxy :: Proxy CreateDBClusterParameterGroup)

responseCreateDBSnapshot :: CreateDBSnapshotResponse -> TestTree
responseCreateDBSnapshot =
  res
    "CreateDBSnapshotResponse"
    "fixture/CreateDBSnapshotResponse.proto"
    rdsService
    (Proxy :: Proxy CreateDBSnapshot)

responseDeleteEventSubscription :: DeleteEventSubscriptionResponse -> TestTree
responseDeleteEventSubscription =
  res
    "DeleteEventSubscriptionResponse"
    "fixture/DeleteEventSubscriptionResponse.proto"
    rdsService
    (Proxy :: Proxy DeleteEventSubscription)

responseDescribeDBClusterBacktracks :: DescribeDBClusterBacktracksResponse -> TestTree
responseDescribeDBClusterBacktracks =
  res
    "DescribeDBClusterBacktracksResponse"
    "fixture/DescribeDBClusterBacktracksResponse.proto"
    rdsService
    (Proxy :: Proxy DescribeDBClusterBacktracks)

responseDescribeDBParameterGroups :: DescribeDBParameterGroupsResponse -> TestTree
responseDescribeDBParameterGroups =
  res
    "DescribeDBParameterGroupsResponse"
    "fixture/DescribeDBParameterGroupsResponse.proto"
    rdsService
    (Proxy :: Proxy DescribeDBParameterGroups)

responseModifyDBSnapshotAttribute :: ModifyDBSnapshotAttributeResponse -> TestTree
responseModifyDBSnapshotAttribute =
  res
    "ModifyDBSnapshotAttributeResponse"
    "fixture/ModifyDBSnapshotAttributeResponse.proto"
    rdsService
    (Proxy :: Proxy ModifyDBSnapshotAttribute)

responseDescribeDBInstanceAutomatedBackups :: DescribeDBInstanceAutomatedBackupsResponse -> TestTree
responseDescribeDBInstanceAutomatedBackups =
  res
    "DescribeDBInstanceAutomatedBackupsResponse"
    "fixture/DescribeDBInstanceAutomatedBackupsResponse.proto"
    rdsService
    (Proxy :: Proxy DescribeDBInstanceAutomatedBackups)

responseRemoveFromGlobalCluster :: RemoveFromGlobalClusterResponse -> TestTree
responseRemoveFromGlobalCluster =
  res
    "RemoveFromGlobalClusterResponse"
    "fixture/RemoveFromGlobalClusterResponse.proto"
    rdsService
    (Proxy :: Proxy RemoveFromGlobalCluster)

responseAddRoleToDBInstance :: AddRoleToDBInstanceResponse -> TestTree
responseAddRoleToDBInstance =
  res
    "AddRoleToDBInstanceResponse"
    "fixture/AddRoleToDBInstanceResponse.proto"
    rdsService
    (Proxy :: Proxy AddRoleToDBInstance)

responseDeleteDBClusterSnapshot :: DeleteDBClusterSnapshotResponse -> TestTree
responseDeleteDBClusterSnapshot =
  res
    "DeleteDBClusterSnapshotResponse"
    "fixture/DeleteDBClusterSnapshotResponse.proto"
    rdsService
    (Proxy :: Proxy DeleteDBClusterSnapshot)

responseDescribeValidDBInstanceModifications :: DescribeValidDBInstanceModificationsResponse -> TestTree
responseDescribeValidDBInstanceModifications =
  res
    "DescribeValidDBInstanceModificationsResponse"
    "fixture/DescribeValidDBInstanceModificationsResponse.proto"
    rdsService
    (Proxy :: Proxy DescribeValidDBInstanceModifications)

responseDescribeDBClusterEndpoints :: DescribeDBClusterEndpointsResponse -> TestTree
responseDescribeDBClusterEndpoints =
  res
    "DescribeDBClusterEndpointsResponse"
    "fixture/DescribeDBClusterEndpointsResponse.proto"
    rdsService
    (Proxy :: Proxy DescribeDBClusterEndpoints)

responseDescribeOptionGroupOptions :: DescribeOptionGroupOptionsResponse -> TestTree
responseDescribeOptionGroupOptions =
  res
    "DescribeOptionGroupOptionsResponse"
    "fixture/DescribeOptionGroupOptionsResponse.proto"
    rdsService
    (Proxy :: Proxy DescribeOptionGroupOptions)

responseDescribeEventSubscriptions :: DescribeEventSubscriptionsResponse -> TestTree
responseDescribeEventSubscriptions =
  res
    "DescribeEventSubscriptionsResponse"
    "fixture/DescribeEventSubscriptionsResponse.proto"
    rdsService
    (Proxy :: Proxy DescribeEventSubscriptions)

responseAddTagsToResource :: AddTagsToResourceResponse -> TestTree
responseAddTagsToResource =
  res
    "AddTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse.proto"
    rdsService
    (Proxy :: Proxy AddTagsToResource)

responseDescribeDBParameters :: DescribeDBParametersResponse -> TestTree
responseDescribeDBParameters =
  res
    "DescribeDBParametersResponse"
    "fixture/DescribeDBParametersResponse.proto"
    rdsService
    (Proxy :: Proxy DescribeDBParameters)

responseStopActivityStream :: StopActivityStreamResponse -> TestTree
responseStopActivityStream =
  res
    "StopActivityStreamResponse"
    "fixture/StopActivityStreamResponse.proto"
    rdsService
    (Proxy :: Proxy StopActivityStream)

responseCreateDBClusterSnapshot :: CreateDBClusterSnapshotResponse -> TestTree
responseCreateDBClusterSnapshot =
  res
    "CreateDBClusterSnapshotResponse"
    "fixture/CreateDBClusterSnapshotResponse.proto"
    rdsService
    (Proxy :: Proxy CreateDBClusterSnapshot)

responseDescribeDBSnapshots :: DescribeDBSnapshotsResponse -> TestTree
responseDescribeDBSnapshots =
  res
    "DescribeDBSnapshotsResponse"
    "fixture/DescribeDBSnapshotsResponse.proto"
    rdsService
    (Proxy :: Proxy DescribeDBSnapshots)

responseModifyDBProxyTargetGroup :: ModifyDBProxyTargetGroupResponse -> TestTree
responseModifyDBProxyTargetGroup =
  res
    "ModifyDBProxyTargetGroupResponse"
    "fixture/ModifyDBProxyTargetGroupResponse.proto"
    rdsService
    (Proxy :: Proxy ModifyDBProxyTargetGroup)

responseDescribeDBSubnetGroups :: DescribeDBSubnetGroupsResponse -> TestTree
responseDescribeDBSubnetGroups =
  res
    "DescribeDBSubnetGroupsResponse"
    "fixture/DescribeDBSubnetGroupsResponse.proto"
    rdsService
    (Proxy :: Proxy DescribeDBSubnetGroups)

responseModifyOptionGroup :: ModifyOptionGroupResponse -> TestTree
responseModifyOptionGroup =
  res
    "ModifyOptionGroupResponse"
    "fixture/ModifyOptionGroupResponse.proto"
    rdsService
    (Proxy :: Proxy ModifyOptionGroup)

responseStopDBCluster :: StopDBClusterResponse -> TestTree
responseStopDBCluster =
  res
    "StopDBClusterResponse"
    "fixture/StopDBClusterResponse.proto"
    rdsService
    (Proxy :: Proxy StopDBCluster)

responseCreateDBParameterGroup :: CreateDBParameterGroupResponse -> TestTree
responseCreateDBParameterGroup =
  res
    "CreateDBParameterGroupResponse"
    "fixture/CreateDBParameterGroupResponse.proto"
    rdsService
    (Proxy :: Proxy CreateDBParameterGroup)

responseModifyDBClusterSnapshotAttribute :: ModifyDBClusterSnapshotAttributeResponse -> TestTree
responseModifyDBClusterSnapshotAttribute =
  res
    "ModifyDBClusterSnapshotAttributeResponse"
    "fixture/ModifyDBClusterSnapshotAttributeResponse.proto"
    rdsService
    (Proxy :: Proxy ModifyDBClusterSnapshotAttribute)

responseModifyDBCluster :: ModifyDBClusterResponse -> TestTree
responseModifyDBCluster =
  res
    "ModifyDBClusterResponse"
    "fixture/ModifyDBClusterResponse.proto"
    rdsService
    (Proxy :: Proxy ModifyDBCluster)

responseCopyDBClusterParameterGroup :: CopyDBClusterParameterGroupResponse -> TestTree
responseCopyDBClusterParameterGroup =
  res
    "CopyDBClusterParameterGroupResponse"
    "fixture/CopyDBClusterParameterGroupResponse.proto"
    rdsService
    (Proxy :: Proxy CopyDBClusterParameterGroup)

responseDescribeEventCategories :: DescribeEventCategoriesResponse -> TestTree
responseDescribeEventCategories =
  res
    "DescribeEventCategoriesResponse"
    "fixture/DescribeEventCategoriesResponse.proto"
    rdsService
    (Proxy :: Proxy DescribeEventCategories)

responseDescribeGlobalClusters :: DescribeGlobalClustersResponse -> TestTree
responseDescribeGlobalClusters =
  res
    "DescribeGlobalClustersResponse"
    "fixture/DescribeGlobalClustersResponse.proto"
    rdsService
    (Proxy :: Proxy DescribeGlobalClusters)

responseStartDBInstance :: StartDBInstanceResponse -> TestTree
responseStartDBInstance =
  res
    "StartDBInstanceResponse"
    "fixture/StartDBInstanceResponse.proto"
    rdsService
    (Proxy :: Proxy StartDBInstance)

responseDescribeExportTasks :: DescribeExportTasksResponse -> TestTree
responseDescribeExportTasks =
  res
    "DescribeExportTasksResponse"
    "fixture/DescribeExportTasksResponse.proto"
    rdsService
    (Proxy :: Proxy DescribeExportTasks)

responseCancelExportTask :: ExportTask -> TestTree
responseCancelExportTask =
  res
    "CancelExportTaskResponse"
    "fixture/CancelExportTaskResponse.proto"
    rdsService
    (Proxy :: Proxy CancelExportTask)

responseModifyDBClusterParameterGroup :: DBClusterParameterGroupNameMessage -> TestTree
responseModifyDBClusterParameterGroup =
  res
    "ModifyDBClusterParameterGroupResponse"
    "fixture/ModifyDBClusterParameterGroupResponse.proto"
    rdsService
    (Proxy :: Proxy ModifyDBClusterParameterGroup)

responseRestoreDBInstanceToPointInTime :: RestoreDBInstanceToPointInTimeResponse -> TestTree
responseRestoreDBInstanceToPointInTime =
  res
    "RestoreDBInstanceToPointInTimeResponse"
    "fixture/RestoreDBInstanceToPointInTimeResponse.proto"
    rdsService
    (Proxy :: Proxy RestoreDBInstanceToPointInTime)

responseDescribeDBClusterSnapshotAttributes :: DescribeDBClusterSnapshotAttributesResponse -> TestTree
responseDescribeDBClusterSnapshotAttributes =
  res
    "DescribeDBClusterSnapshotAttributesResponse"
    "fixture/DescribeDBClusterSnapshotAttributesResponse.proto"
    rdsService
    (Proxy :: Proxy DescribeDBClusterSnapshotAttributes)

responseModifyDBSnapshot :: ModifyDBSnapshotResponse -> TestTree
responseModifyDBSnapshot =
  res
    "ModifyDBSnapshotResponse"
    "fixture/ModifyDBSnapshotResponse.proto"
    rdsService
    (Proxy :: Proxy ModifyDBSnapshot)

responseDescribeDBProxyTargetGroups :: DescribeDBProxyTargetGroupsResponse -> TestTree
responseDescribeDBProxyTargetGroups =
  res
    "DescribeDBProxyTargetGroupsResponse"
    "fixture/DescribeDBProxyTargetGroupsResponse.proto"
    rdsService
    (Proxy :: Proxy DescribeDBProxyTargetGroups)

responseModifyDBProxy :: ModifyDBProxyResponse -> TestTree
responseModifyDBProxy =
  res
    "ModifyDBProxyResponse"
    "fixture/ModifyDBProxyResponse.proto"
    rdsService
    (Proxy :: Proxy ModifyDBProxy)

responseDescribePendingMaintenanceActions :: DescribePendingMaintenanceActionsResponse -> TestTree
responseDescribePendingMaintenanceActions =
  res
    "DescribePendingMaintenanceActionsResponse"
    "fixture/DescribePendingMaintenanceActionsResponse.proto"
    rdsService
    (Proxy :: Proxy DescribePendingMaintenanceActions)

responseAddRoleToDBCluster :: AddRoleToDBClusterResponse -> TestTree
responseAddRoleToDBCluster =
  res
    "AddRoleToDBClusterResponse"
    "fixture/AddRoleToDBClusterResponse.proto"
    rdsService
    (Proxy :: Proxy AddRoleToDBCluster)

responseCopyDBClusterSnapshot :: CopyDBClusterSnapshotResponse -> TestTree
responseCopyDBClusterSnapshot =
  res
    "CopyDBClusterSnapshotResponse"
    "fixture/CopyDBClusterSnapshotResponse.proto"
    rdsService
    (Proxy :: Proxy CopyDBClusterSnapshot)

responseImportInstallationMedia :: InstallationMedia -> TestTree
responseImportInstallationMedia =
  res
    "ImportInstallationMediaResponse"
    "fixture/ImportInstallationMediaResponse.proto"
    rdsService
    (Proxy :: Proxy ImportInstallationMedia)

responseCreateGlobalCluster :: CreateGlobalClusterResponse -> TestTree
responseCreateGlobalCluster =
  res
    "CreateGlobalClusterResponse"
    "fixture/CreateGlobalClusterResponse.proto"
    rdsService
    (Proxy :: Proxy CreateGlobalCluster)

responseResetDBParameterGroup :: DBParameterGroupNameMessage -> TestTree
responseResetDBParameterGroup =
  res
    "ResetDBParameterGroupResponse"
    "fixture/ResetDBParameterGroupResponse.proto"
    rdsService
    (Proxy :: Proxy ResetDBParameterGroup)

responseDescribeInstallationMedia :: DescribeInstallationMediaResponse -> TestTree
responseDescribeInstallationMedia =
  res
    "DescribeInstallationMediaResponse"
    "fixture/DescribeInstallationMediaResponse.proto"
    rdsService
    (Proxy :: Proxy DescribeInstallationMedia)

responseDeregisterDBProxyTargets :: DeregisterDBProxyTargetsResponse -> TestTree
responseDeregisterDBProxyTargets =
  res
    "DeregisterDBProxyTargetsResponse"
    "fixture/DeregisterDBProxyTargetsResponse.proto"
    rdsService
    (Proxy :: Proxy DeregisterDBProxyTargets)

responseCreateDBCluster :: CreateDBClusterResponse -> TestTree
responseCreateDBCluster =
  res
    "CreateDBClusterResponse"
    "fixture/CreateDBClusterResponse.proto"
    rdsService
    (Proxy :: Proxy CreateDBCluster)

responseRemoveRoleFromDBCluster :: RemoveRoleFromDBClusterResponse -> TestTree
responseRemoveRoleFromDBCluster =
  res
    "RemoveRoleFromDBClusterResponse"
    "fixture/RemoveRoleFromDBClusterResponse.proto"
    rdsService
    (Proxy :: Proxy RemoveRoleFromDBCluster)

responseFailoverDBCluster :: FailoverDBClusterResponse -> TestTree
responseFailoverDBCluster =
  res
    "FailoverDBClusterResponse"
    "fixture/FailoverDBClusterResponse.proto"
    rdsService
    (Proxy :: Proxy FailoverDBCluster)

responseRevokeDBSecurityGroupIngress :: RevokeDBSecurityGroupIngressResponse -> TestTree
responseRevokeDBSecurityGroupIngress =
  res
    "RevokeDBSecurityGroupIngressResponse"
    "fixture/RevokeDBSecurityGroupIngressResponse.proto"
    rdsService
    (Proxy :: Proxy RevokeDBSecurityGroupIngress)

responseModifyDBParameterGroup :: DBParameterGroupNameMessage -> TestTree
responseModifyDBParameterGroup =
  res
    "ModifyDBParameterGroupResponse"
    "fixture/ModifyDBParameterGroupResponse.proto"
    rdsService
    (Proxy :: Proxy ModifyDBParameterGroup)

responseApplyPendingMaintenanceAction :: ApplyPendingMaintenanceActionResponse -> TestTree
responseApplyPendingMaintenanceAction =
  res
    "ApplyPendingMaintenanceActionResponse"
    "fixture/ApplyPendingMaintenanceActionResponse.proto"
    rdsService
    (Proxy :: Proxy ApplyPendingMaintenanceAction)

responseCreateOptionGroup :: CreateOptionGroupResponse -> TestTree
responseCreateOptionGroup =
  res
    "CreateOptionGroupResponse"
    "fixture/CreateOptionGroupResponse.proto"
    rdsService
    (Proxy :: Proxy CreateOptionGroup)

responseDescribeAccountAttributes :: DescribeAccountAttributesResponse -> TestTree
responseDescribeAccountAttributes =
  res
    "DescribeAccountAttributesResponse"
    "fixture/DescribeAccountAttributesResponse.proto"
    rdsService
    (Proxy :: Proxy DescribeAccountAttributes)

responseDeleteDBSnapshot :: DeleteDBSnapshotResponse -> TestTree
responseDeleteDBSnapshot =
  res
    "DeleteDBSnapshotResponse"
    "fixture/DeleteDBSnapshotResponse.proto"
    rdsService
    (Proxy :: Proxy DeleteDBSnapshot)

responseDescribeDBClusterParameters :: DescribeDBClusterParametersResponse -> TestTree
responseDescribeDBClusterParameters =
  res
    "DescribeDBClusterParametersResponse"
    "fixture/DescribeDBClusterParametersResponse.proto"
    rdsService
    (Proxy :: Proxy DescribeDBClusterParameters)

responseDeleteDBSubnetGroup :: DeleteDBSubnetGroupResponse -> TestTree
responseDeleteDBSubnetGroup =
  res
    "DeleteDBSubnetGroupResponse"
    "fixture/DeleteDBSubnetGroupResponse.proto"
    rdsService
    (Proxy :: Proxy DeleteDBSubnetGroup)

responseCreateDBSecurityGroup :: CreateDBSecurityGroupResponse -> TestTree
responseCreateDBSecurityGroup =
  res
    "CreateDBSecurityGroupResponse"
    "fixture/CreateDBSecurityGroupResponse.proto"
    rdsService
    (Proxy :: Proxy CreateDBSecurityGroup)

responseModifyCertificates :: ModifyCertificatesResponse -> TestTree
responseModifyCertificates =
  res
    "ModifyCertificatesResponse"
    "fixture/ModifyCertificatesResponse.proto"
    rdsService
    (Proxy :: Proxy ModifyCertificates)

responseDescribeDBClusterSnapshots :: DescribeDBClusterSnapshotsResponse -> TestTree
responseDescribeDBClusterSnapshots =
  res
    "DescribeDBClusterSnapshotsResponse"
    "fixture/DescribeDBClusterSnapshotsResponse.proto"
    rdsService
    (Proxy :: Proxy DescribeDBClusterSnapshots)

responseRebootDBInstance :: RebootDBInstanceResponse -> TestTree
responseRebootDBInstance =
  res
    "RebootDBInstanceResponse"
    "fixture/RebootDBInstanceResponse.proto"
    rdsService
    (Proxy :: Proxy RebootDBInstance)

responseCreateDBSubnetGroup :: CreateDBSubnetGroupResponse -> TestTree
responseCreateDBSubnetGroup =
  res
    "CreateDBSubnetGroupResponse"
    "fixture/CreateDBSubnetGroupResponse.proto"
    rdsService
    (Proxy :: Proxy CreateDBSubnetGroup)

responseDescribeReservedDBInstancesOfferings :: DescribeReservedDBInstancesOfferingsResponse -> TestTree
responseDescribeReservedDBInstancesOfferings =
  res
    "DescribeReservedDBInstancesOfferingsResponse"
    "fixture/DescribeReservedDBInstancesOfferingsResponse.proto"
    rdsService
    (Proxy :: Proxy DescribeReservedDBInstancesOfferings)

responseDeleteDBSecurityGroup :: DeleteDBSecurityGroupResponse -> TestTree
responseDeleteDBSecurityGroup =
  res
    "DeleteDBSecurityGroupResponse"
    "fixture/DeleteDBSecurityGroupResponse.proto"
    rdsService
    (Proxy :: Proxy DeleteDBSecurityGroup)

responseDeleteDBInstance :: DeleteDBInstanceResponse -> TestTree
responseDeleteDBInstance =
  res
    "DeleteDBInstanceResponse"
    "fixture/DeleteDBInstanceResponse.proto"
    rdsService
    (Proxy :: Proxy DeleteDBInstance)

responseStartActivityStream :: StartActivityStreamResponse -> TestTree
responseStartActivityStream =
  res
    "StartActivityStreamResponse"
    "fixture/StartActivityStreamResponse.proto"
    rdsService
    (Proxy :: Proxy StartActivityStream)

responseCreateDBInstanceReadReplica :: CreateDBInstanceReadReplicaResponse -> TestTree
responseCreateDBInstanceReadReplica =
  res
    "CreateDBInstanceReadReplicaResponse"
    "fixture/CreateDBInstanceReadReplicaResponse.proto"
    rdsService
    (Proxy :: Proxy CreateDBInstanceReadReplica)

responseDeleteDBParameterGroup :: DeleteDBParameterGroupResponse -> TestTree
responseDeleteDBParameterGroup =
  res
    "DeleteDBParameterGroupResponse"
    "fixture/DeleteDBParameterGroupResponse.proto"
    rdsService
    (Proxy :: Proxy DeleteDBParameterGroup)

responseModifyCurrentDBClusterCapacity :: ModifyCurrentDBClusterCapacityResponse -> TestTree
responseModifyCurrentDBClusterCapacity =
  res
    "ModifyCurrentDBClusterCapacityResponse"
    "fixture/ModifyCurrentDBClusterCapacityResponse.proto"
    rdsService
    (Proxy :: Proxy ModifyCurrentDBClusterCapacity)

responseModifyGlobalCluster :: ModifyGlobalClusterResponse -> TestTree
responseModifyGlobalCluster =
  res
    "ModifyGlobalClusterResponse"
    "fixture/ModifyGlobalClusterResponse.proto"
    rdsService
    (Proxy :: Proxy ModifyGlobalCluster)

responseRegisterDBProxyTargets :: RegisterDBProxyTargetsResponse -> TestTree
responseRegisterDBProxyTargets =
  res
    "RegisterDBProxyTargetsResponse"
    "fixture/RegisterDBProxyTargetsResponse.proto"
    rdsService
    (Proxy :: Proxy RegisterDBProxyTargets)

responseDescribeDBSecurityGroups :: DescribeDBSecurityGroupsResponse -> TestTree
responseDescribeDBSecurityGroups =
  res
    "DescribeDBSecurityGroupsResponse"
    "fixture/DescribeDBSecurityGroupsResponse.proto"
    rdsService
    (Proxy :: Proxy DescribeDBSecurityGroups)

responseCopyOptionGroup :: CopyOptionGroupResponse -> TestTree
responseCopyOptionGroup =
  res
    "CopyOptionGroupResponse"
    "fixture/CopyOptionGroupResponse.proto"
    rdsService
    (Proxy :: Proxy CopyOptionGroup)

responseRestoreDBClusterToPointInTime :: RestoreDBClusterToPointInTimeResponse -> TestTree
responseRestoreDBClusterToPointInTime =
  res
    "RestoreDBClusterToPointInTimeResponse"
    "fixture/RestoreDBClusterToPointInTimeResponse.proto"
    rdsService
    (Proxy :: Proxy RestoreDBClusterToPointInTime)

responseDeleteInstallationMedia :: InstallationMedia -> TestTree
responseDeleteInstallationMedia =
  res
    "DeleteInstallationMediaResponse"
    "fixture/DeleteInstallationMediaResponse.proto"
    rdsService
    (Proxy :: Proxy DeleteInstallationMedia)

responseDescribeDBInstances :: DescribeDBInstancesResponse -> TestTree
responseDescribeDBInstances =
  res
    "DescribeDBInstancesResponse"
    "fixture/DescribeDBInstancesResponse.proto"
    rdsService
    (Proxy :: Proxy DescribeDBInstances)

responseRestoreDBInstanceFromS3 :: RestoreDBInstanceFromS3Response -> TestTree
responseRestoreDBInstanceFromS3 =
  res
    "RestoreDBInstanceFromS3Response"
    "fixture/RestoreDBInstanceFromS3Response.proto"
    rdsService
    (Proxy :: Proxy RestoreDBInstanceFromS3)

responseDownloadDBLogFilePortion :: DownloadDBLogFilePortionResponse -> TestTree
responseDownloadDBLogFilePortion =
  res
    "DownloadDBLogFilePortionResponse"
    "fixture/DownloadDBLogFilePortionResponse.proto"
    rdsService
    (Proxy :: Proxy DownloadDBLogFilePortion)

responseDescribeDBProxies :: DescribeDBProxiesResponse -> TestTree
responseDescribeDBProxies =
  res
    "DescribeDBProxiesResponse"
    "fixture/DescribeDBProxiesResponse.proto"
    rdsService
    (Proxy :: Proxy DescribeDBProxies)

responseStartExportTask :: ExportTask -> TestTree
responseStartExportTask =
  res
    "StartExportTaskResponse"
    "fixture/StartExportTaskResponse.proto"
    rdsService
    (Proxy :: Proxy StartExportTask)
