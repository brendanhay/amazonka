{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.RDS
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
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
--         [ requestDescribeDBClusterParameterGroups $
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
--         , requestDescribeEngineDefaultClusterParameters $
--             describeEngineDefaultClusterParameters
--
--         , requestDescribeDBSnapshotAttributes $
--             describeDBSnapshotAttributes
--
--         , requestBacktrackDBCluster $
--             backtrackDBCluster
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
--         , requestDescribeSourceRegions $
--             describeSourceRegions
--
--         , requestRestoreDBClusterFromSnapshot $
--             restoreDBClusterFromSnapshot
--
--         , requestDescribeOrderableDBInstanceOptions $
--             describeOrderableDBInstanceOptions
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
--         , requestDeleteDBClusterSnapshot $
--             deleteDBClusterSnapshot
--
--         , requestDescribeValidDBInstanceModifications $
--             describeValidDBInstanceModifications
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
--         , requestCreateDBClusterSnapshot $
--             createDBClusterSnapshot
--
--         , requestDescribeDBSnapshots $
--             describeDBSnapshots
--
--         , requestDescribeDBSubnetGroups $
--             describeDBSubnetGroups
--
--         , requestModifyOptionGroup $
--             modifyOptionGroup
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
--         , requestStartDBInstance $
--             startDBInstance
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
--         , requestDescribePendingMaintenanceActions $
--             describePendingMaintenanceActions
--
--         , requestAddRoleToDBCluster $
--             addRoleToDBCluster
--
--         , requestCopyDBClusterSnapshot $
--             copyDBClusterSnapshot
--
--         , requestResetDBParameterGroup $
--             resetDBParameterGroup
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
--         , requestCreateDBInstanceReadReplica $
--             createDBInstanceReadReplica
--
--         , requestDeleteDBParameterGroup $
--             deleteDBParameterGroup
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
--         , requestDescribeDBInstances $
--             describeDBInstances
--
--         , requestRestoreDBInstanceFromS3 $
--             restoreDBInstanceFromS3
--
--         , requestDownloadDBLogFilePortion $
--             downloadDBLogFilePortion
--
--           ]

--     , testGroup "response"
--         [ responseDescribeDBClusterParameterGroups $
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
--         , responseDescribeEngineDefaultClusterParameters $
--             describeEngineDefaultClusterParametersResponse
--
--         , responseDescribeDBSnapshotAttributes $
--             describeDBSnapshotAttributesResponse
--
--         , responseBacktrackDBCluster $
--             dbClusterBacktrack
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
--         , responseDescribeSourceRegions $
--             describeSourceRegionsResponse
--
--         , responseRestoreDBClusterFromSnapshot $
--             restoreDBClusterFromSnapshotResponse
--
--         , responseDescribeOrderableDBInstanceOptions $
--             describeOrderableDBInstanceOptionsResponse
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
--         , responseDeleteDBClusterSnapshot $
--             deleteDBClusterSnapshotResponse
--
--         , responseDescribeValidDBInstanceModifications $
--             describeValidDBInstanceModificationsResponse
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
--         , responseCreateDBClusterSnapshot $
--             createDBClusterSnapshotResponse
--
--         , responseDescribeDBSnapshots $
--             describeDBSnapshotsResponse
--
--         , responseDescribeDBSubnetGroups $
--             describeDBSubnetGroupsResponse
--
--         , responseModifyOptionGroup $
--             modifyOptionGroupResponse
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
--         , responseStartDBInstance $
--             startDBInstanceResponse
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
--         , responseDescribePendingMaintenanceActions $
--             describePendingMaintenanceActionsResponse
--
--         , responseAddRoleToDBCluster $
--             addRoleToDBClusterResponse
--
--         , responseCopyDBClusterSnapshot $
--             copyDBClusterSnapshotResponse
--
--         , responseResetDBParameterGroup $
--             dbParameterGroupNameMessage
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
--         , responseCreateDBInstanceReadReplica $
--             createDBInstanceReadReplicaResponse
--
--         , responseDeleteDBParameterGroup $
--             deleteDBParameterGroupResponse
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
--         , responseDescribeDBInstances $
--             describeDBInstancesResponse
--
--         , responseRestoreDBInstanceFromS3 $
--             restoreDBInstanceFromS3Response
--
--         , responseDownloadDBLogFilePortion $
--             downloadDBLogFilePortionResponse
--
--           ]
--     ]

-- Requests

requestDescribeDBClusterParameterGroups :: DescribeDBClusterParameterGroups -> TestTree
requestDescribeDBClusterParameterGroups = req
    "DescribeDBClusterParameterGroups"
    "fixture/DescribeDBClusterParameterGroups.yaml"

requestPromoteReadReplica :: PromoteReadReplica -> TestTree
requestPromoteReadReplica = req
    "PromoteReadReplica"
    "fixture/PromoteReadReplica.yaml"

requestDescribeDBEngineVersions :: DescribeDBEngineVersions -> TestTree
requestDescribeDBEngineVersions = req
    "DescribeDBEngineVersions"
    "fixture/DescribeDBEngineVersions.yaml"

requestStopDBInstance :: StopDBInstance -> TestTree
requestStopDBInstance = req
    "StopDBInstance"
    "fixture/StopDBInstance.yaml"

requestCopyDBSnapshot :: CopyDBSnapshot -> TestTree
requestCopyDBSnapshot = req
    "CopyDBSnapshot"
    "fixture/CopyDBSnapshot.yaml"

requestAddSourceIdentifierToSubscription :: AddSourceIdentifierToSubscription -> TestTree
requestAddSourceIdentifierToSubscription = req
    "AddSourceIdentifierToSubscription"
    "fixture/AddSourceIdentifierToSubscription.yaml"

requestModifyDBInstance :: ModifyDBInstance -> TestTree
requestModifyDBInstance = req
    "ModifyDBInstance"
    "fixture/ModifyDBInstance.yaml"

requestModifyEventSubscription :: ModifyEventSubscription -> TestTree
requestModifyEventSubscription = req
    "ModifyEventSubscription"
    "fixture/ModifyEventSubscription.yaml"

requestResetDBClusterParameterGroup :: ResetDBClusterParameterGroup -> TestTree
requestResetDBClusterParameterGroup = req
    "ResetDBClusterParameterGroup"
    "fixture/ResetDBClusterParameterGroup.yaml"

requestRestoreDBClusterFromS3 :: RestoreDBClusterFromS3 -> TestTree
requestRestoreDBClusterFromS3 = req
    "RestoreDBClusterFromS3"
    "fixture/RestoreDBClusterFromS3.yaml"

requestDescribeEvents :: DescribeEvents -> TestTree
requestDescribeEvents = req
    "DescribeEvents"
    "fixture/DescribeEvents.yaml"

requestDescribeEngineDefaultParameters :: DescribeEngineDefaultParameters -> TestTree
requestDescribeEngineDefaultParameters = req
    "DescribeEngineDefaultParameters"
    "fixture/DescribeEngineDefaultParameters.yaml"

requestDescribeOptionGroups :: DescribeOptionGroups -> TestTree
requestDescribeOptionGroups = req
    "DescribeOptionGroups"
    "fixture/DescribeOptionGroups.yaml"

requestDescribeDBLogFiles :: DescribeDBLogFiles -> TestTree
requestDescribeDBLogFiles = req
    "DescribeDBLogFiles"
    "fixture/DescribeDBLogFiles.yaml"

requestDescribeDBClusters :: DescribeDBClusters -> TestTree
requestDescribeDBClusters = req
    "DescribeDBClusters"
    "fixture/DescribeDBClusters.yaml"

requestModifyDBSubnetGroup :: ModifyDBSubnetGroup -> TestTree
requestModifyDBSubnetGroup = req
    "ModifyDBSubnetGroup"
    "fixture/ModifyDBSubnetGroup.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource = req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestDeleteOptionGroup :: DeleteOptionGroup -> TestTree
requestDeleteOptionGroup = req
    "DeleteOptionGroup"
    "fixture/DeleteOptionGroup.yaml"

requestDeleteDBCluster :: DeleteDBCluster -> TestTree
requestDeleteDBCluster = req
    "DeleteDBCluster"
    "fixture/DeleteDBCluster.yaml"

requestDescribeReservedDBInstances :: DescribeReservedDBInstances -> TestTree
requestDescribeReservedDBInstances = req
    "DescribeReservedDBInstances"
    "fixture/DescribeReservedDBInstances.yaml"

requestCopyDBParameterGroup :: CopyDBParameterGroup -> TestTree
requestCopyDBParameterGroup = req
    "CopyDBParameterGroup"
    "fixture/CopyDBParameterGroup.yaml"

requestRemoveSourceIdentifierFromSubscription :: RemoveSourceIdentifierFromSubscription -> TestTree
requestRemoveSourceIdentifierFromSubscription = req
    "RemoveSourceIdentifierFromSubscription"
    "fixture/RemoveSourceIdentifierFromSubscription.yaml"

requestDescribeEngineDefaultClusterParameters :: DescribeEngineDefaultClusterParameters -> TestTree
requestDescribeEngineDefaultClusterParameters = req
    "DescribeEngineDefaultClusterParameters"
    "fixture/DescribeEngineDefaultClusterParameters.yaml"

requestDescribeDBSnapshotAttributes :: DescribeDBSnapshotAttributes -> TestTree
requestDescribeDBSnapshotAttributes = req
    "DescribeDBSnapshotAttributes"
    "fixture/DescribeDBSnapshotAttributes.yaml"

requestBacktrackDBCluster :: BacktrackDBCluster -> TestTree
requestBacktrackDBCluster = req
    "BacktrackDBCluster"
    "fixture/BacktrackDBCluster.yaml"

requestPromoteReadReplicaDBCluster :: PromoteReadReplicaDBCluster -> TestTree
requestPromoteReadReplicaDBCluster = req
    "PromoteReadReplicaDBCluster"
    "fixture/PromoteReadReplicaDBCluster.yaml"

requestRemoveTagsFromResource :: RemoveTagsFromResource -> TestTree
requestRemoveTagsFromResource = req
    "RemoveTagsFromResource"
    "fixture/RemoveTagsFromResource.yaml"

requestRestoreDBInstanceFromDBSnapshot :: RestoreDBInstanceFromDBSnapshot -> TestTree
requestRestoreDBInstanceFromDBSnapshot = req
    "RestoreDBInstanceFromDBSnapshot"
    "fixture/RestoreDBInstanceFromDBSnapshot.yaml"

requestCreateEventSubscription :: CreateEventSubscription -> TestTree
requestCreateEventSubscription = req
    "CreateEventSubscription"
    "fixture/CreateEventSubscription.yaml"

requestPurchaseReservedDBInstancesOffering :: PurchaseReservedDBInstancesOffering -> TestTree
requestPurchaseReservedDBInstancesOffering = req
    "PurchaseReservedDBInstancesOffering"
    "fixture/PurchaseReservedDBInstancesOffering.yaml"

requestCreateDBInstance :: CreateDBInstance -> TestTree
requestCreateDBInstance = req
    "CreateDBInstance"
    "fixture/CreateDBInstance.yaml"

requestDeleteDBClusterParameterGroup :: DeleteDBClusterParameterGroup -> TestTree
requestDeleteDBClusterParameterGroup = req
    "DeleteDBClusterParameterGroup"
    "fixture/DeleteDBClusterParameterGroup.yaml"

requestDescribeCertificates :: DescribeCertificates -> TestTree
requestDescribeCertificates = req
    "DescribeCertificates"
    "fixture/DescribeCertificates.yaml"

requestAuthorizeDBSecurityGroupIngress :: AuthorizeDBSecurityGroupIngress -> TestTree
requestAuthorizeDBSecurityGroupIngress = req
    "AuthorizeDBSecurityGroupIngress"
    "fixture/AuthorizeDBSecurityGroupIngress.yaml"

requestDescribeSourceRegions :: DescribeSourceRegions -> TestTree
requestDescribeSourceRegions = req
    "DescribeSourceRegions"
    "fixture/DescribeSourceRegions.yaml"

requestRestoreDBClusterFromSnapshot :: RestoreDBClusterFromSnapshot -> TestTree
requestRestoreDBClusterFromSnapshot = req
    "RestoreDBClusterFromSnapshot"
    "fixture/RestoreDBClusterFromSnapshot.yaml"

requestDescribeOrderableDBInstanceOptions :: DescribeOrderableDBInstanceOptions -> TestTree
requestDescribeOrderableDBInstanceOptions = req
    "DescribeOrderableDBInstanceOptions"
    "fixture/DescribeOrderableDBInstanceOptions.yaml"

requestCreateDBClusterParameterGroup :: CreateDBClusterParameterGroup -> TestTree
requestCreateDBClusterParameterGroup = req
    "CreateDBClusterParameterGroup"
    "fixture/CreateDBClusterParameterGroup.yaml"

requestCreateDBSnapshot :: CreateDBSnapshot -> TestTree
requestCreateDBSnapshot = req
    "CreateDBSnapshot"
    "fixture/CreateDBSnapshot.yaml"

requestDeleteEventSubscription :: DeleteEventSubscription -> TestTree
requestDeleteEventSubscription = req
    "DeleteEventSubscription"
    "fixture/DeleteEventSubscription.yaml"

requestDescribeDBClusterBacktracks :: DescribeDBClusterBacktracks -> TestTree
requestDescribeDBClusterBacktracks = req
    "DescribeDBClusterBacktracks"
    "fixture/DescribeDBClusterBacktracks.yaml"

requestDescribeDBParameterGroups :: DescribeDBParameterGroups -> TestTree
requestDescribeDBParameterGroups = req
    "DescribeDBParameterGroups"
    "fixture/DescribeDBParameterGroups.yaml"

requestModifyDBSnapshotAttribute :: ModifyDBSnapshotAttribute -> TestTree
requestModifyDBSnapshotAttribute = req
    "ModifyDBSnapshotAttribute"
    "fixture/ModifyDBSnapshotAttribute.yaml"

requestDeleteDBClusterSnapshot :: DeleteDBClusterSnapshot -> TestTree
requestDeleteDBClusterSnapshot = req
    "DeleteDBClusterSnapshot"
    "fixture/DeleteDBClusterSnapshot.yaml"

requestDescribeValidDBInstanceModifications :: DescribeValidDBInstanceModifications -> TestTree
requestDescribeValidDBInstanceModifications = req
    "DescribeValidDBInstanceModifications"
    "fixture/DescribeValidDBInstanceModifications.yaml"

requestDescribeOptionGroupOptions :: DescribeOptionGroupOptions -> TestTree
requestDescribeOptionGroupOptions = req
    "DescribeOptionGroupOptions"
    "fixture/DescribeOptionGroupOptions.yaml"

requestDescribeEventSubscriptions :: DescribeEventSubscriptions -> TestTree
requestDescribeEventSubscriptions = req
    "DescribeEventSubscriptions"
    "fixture/DescribeEventSubscriptions.yaml"

requestAddTagsToResource :: AddTagsToResource -> TestTree
requestAddTagsToResource = req
    "AddTagsToResource"
    "fixture/AddTagsToResource.yaml"

requestDescribeDBParameters :: DescribeDBParameters -> TestTree
requestDescribeDBParameters = req
    "DescribeDBParameters"
    "fixture/DescribeDBParameters.yaml"

requestCreateDBClusterSnapshot :: CreateDBClusterSnapshot -> TestTree
requestCreateDBClusterSnapshot = req
    "CreateDBClusterSnapshot"
    "fixture/CreateDBClusterSnapshot.yaml"

requestDescribeDBSnapshots :: DescribeDBSnapshots -> TestTree
requestDescribeDBSnapshots = req
    "DescribeDBSnapshots"
    "fixture/DescribeDBSnapshots.yaml"

requestDescribeDBSubnetGroups :: DescribeDBSubnetGroups -> TestTree
requestDescribeDBSubnetGroups = req
    "DescribeDBSubnetGroups"
    "fixture/DescribeDBSubnetGroups.yaml"

requestModifyOptionGroup :: ModifyOptionGroup -> TestTree
requestModifyOptionGroup = req
    "ModifyOptionGroup"
    "fixture/ModifyOptionGroup.yaml"

requestCreateDBParameterGroup :: CreateDBParameterGroup -> TestTree
requestCreateDBParameterGroup = req
    "CreateDBParameterGroup"
    "fixture/CreateDBParameterGroup.yaml"

requestModifyDBClusterSnapshotAttribute :: ModifyDBClusterSnapshotAttribute -> TestTree
requestModifyDBClusterSnapshotAttribute = req
    "ModifyDBClusterSnapshotAttribute"
    "fixture/ModifyDBClusterSnapshotAttribute.yaml"

requestModifyDBCluster :: ModifyDBCluster -> TestTree
requestModifyDBCluster = req
    "ModifyDBCluster"
    "fixture/ModifyDBCluster.yaml"

requestCopyDBClusterParameterGroup :: CopyDBClusterParameterGroup -> TestTree
requestCopyDBClusterParameterGroup = req
    "CopyDBClusterParameterGroup"
    "fixture/CopyDBClusterParameterGroup.yaml"

requestDescribeEventCategories :: DescribeEventCategories -> TestTree
requestDescribeEventCategories = req
    "DescribeEventCategories"
    "fixture/DescribeEventCategories.yaml"

requestStartDBInstance :: StartDBInstance -> TestTree
requestStartDBInstance = req
    "StartDBInstance"
    "fixture/StartDBInstance.yaml"

requestModifyDBClusterParameterGroup :: ModifyDBClusterParameterGroup -> TestTree
requestModifyDBClusterParameterGroup = req
    "ModifyDBClusterParameterGroup"
    "fixture/ModifyDBClusterParameterGroup.yaml"

requestRestoreDBInstanceToPointInTime :: RestoreDBInstanceToPointInTime -> TestTree
requestRestoreDBInstanceToPointInTime = req
    "RestoreDBInstanceToPointInTime"
    "fixture/RestoreDBInstanceToPointInTime.yaml"

requestDescribeDBClusterSnapshotAttributes :: DescribeDBClusterSnapshotAttributes -> TestTree
requestDescribeDBClusterSnapshotAttributes = req
    "DescribeDBClusterSnapshotAttributes"
    "fixture/DescribeDBClusterSnapshotAttributes.yaml"

requestModifyDBSnapshot :: ModifyDBSnapshot -> TestTree
requestModifyDBSnapshot = req
    "ModifyDBSnapshot"
    "fixture/ModifyDBSnapshot.yaml"

requestDescribePendingMaintenanceActions :: DescribePendingMaintenanceActions -> TestTree
requestDescribePendingMaintenanceActions = req
    "DescribePendingMaintenanceActions"
    "fixture/DescribePendingMaintenanceActions.yaml"

requestAddRoleToDBCluster :: AddRoleToDBCluster -> TestTree
requestAddRoleToDBCluster = req
    "AddRoleToDBCluster"
    "fixture/AddRoleToDBCluster.yaml"

requestCopyDBClusterSnapshot :: CopyDBClusterSnapshot -> TestTree
requestCopyDBClusterSnapshot = req
    "CopyDBClusterSnapshot"
    "fixture/CopyDBClusterSnapshot.yaml"

requestResetDBParameterGroup :: ResetDBParameterGroup -> TestTree
requestResetDBParameterGroup = req
    "ResetDBParameterGroup"
    "fixture/ResetDBParameterGroup.yaml"

requestCreateDBCluster :: CreateDBCluster -> TestTree
requestCreateDBCluster = req
    "CreateDBCluster"
    "fixture/CreateDBCluster.yaml"

requestRemoveRoleFromDBCluster :: RemoveRoleFromDBCluster -> TestTree
requestRemoveRoleFromDBCluster = req
    "RemoveRoleFromDBCluster"
    "fixture/RemoveRoleFromDBCluster.yaml"

requestFailoverDBCluster :: FailoverDBCluster -> TestTree
requestFailoverDBCluster = req
    "FailoverDBCluster"
    "fixture/FailoverDBCluster.yaml"

requestRevokeDBSecurityGroupIngress :: RevokeDBSecurityGroupIngress -> TestTree
requestRevokeDBSecurityGroupIngress = req
    "RevokeDBSecurityGroupIngress"
    "fixture/RevokeDBSecurityGroupIngress.yaml"

requestModifyDBParameterGroup :: ModifyDBParameterGroup -> TestTree
requestModifyDBParameterGroup = req
    "ModifyDBParameterGroup"
    "fixture/ModifyDBParameterGroup.yaml"

requestApplyPendingMaintenanceAction :: ApplyPendingMaintenanceAction -> TestTree
requestApplyPendingMaintenanceAction = req
    "ApplyPendingMaintenanceAction"
    "fixture/ApplyPendingMaintenanceAction.yaml"

requestCreateOptionGroup :: CreateOptionGroup -> TestTree
requestCreateOptionGroup = req
    "CreateOptionGroup"
    "fixture/CreateOptionGroup.yaml"

requestDescribeAccountAttributes :: DescribeAccountAttributes -> TestTree
requestDescribeAccountAttributes = req
    "DescribeAccountAttributes"
    "fixture/DescribeAccountAttributes.yaml"

requestDeleteDBSnapshot :: DeleteDBSnapshot -> TestTree
requestDeleteDBSnapshot = req
    "DeleteDBSnapshot"
    "fixture/DeleteDBSnapshot.yaml"

requestDescribeDBClusterParameters :: DescribeDBClusterParameters -> TestTree
requestDescribeDBClusterParameters = req
    "DescribeDBClusterParameters"
    "fixture/DescribeDBClusterParameters.yaml"

requestDeleteDBSubnetGroup :: DeleteDBSubnetGroup -> TestTree
requestDeleteDBSubnetGroup = req
    "DeleteDBSubnetGroup"
    "fixture/DeleteDBSubnetGroup.yaml"

requestCreateDBSecurityGroup :: CreateDBSecurityGroup -> TestTree
requestCreateDBSecurityGroup = req
    "CreateDBSecurityGroup"
    "fixture/CreateDBSecurityGroup.yaml"

requestDescribeDBClusterSnapshots :: DescribeDBClusterSnapshots -> TestTree
requestDescribeDBClusterSnapshots = req
    "DescribeDBClusterSnapshots"
    "fixture/DescribeDBClusterSnapshots.yaml"

requestRebootDBInstance :: RebootDBInstance -> TestTree
requestRebootDBInstance = req
    "RebootDBInstance"
    "fixture/RebootDBInstance.yaml"

requestCreateDBSubnetGroup :: CreateDBSubnetGroup -> TestTree
requestCreateDBSubnetGroup = req
    "CreateDBSubnetGroup"
    "fixture/CreateDBSubnetGroup.yaml"

requestDescribeReservedDBInstancesOfferings :: DescribeReservedDBInstancesOfferings -> TestTree
requestDescribeReservedDBInstancesOfferings = req
    "DescribeReservedDBInstancesOfferings"
    "fixture/DescribeReservedDBInstancesOfferings.yaml"

requestDeleteDBSecurityGroup :: DeleteDBSecurityGroup -> TestTree
requestDeleteDBSecurityGroup = req
    "DeleteDBSecurityGroup"
    "fixture/DeleteDBSecurityGroup.yaml"

requestDeleteDBInstance :: DeleteDBInstance -> TestTree
requestDeleteDBInstance = req
    "DeleteDBInstance"
    "fixture/DeleteDBInstance.yaml"

requestCreateDBInstanceReadReplica :: CreateDBInstanceReadReplica -> TestTree
requestCreateDBInstanceReadReplica = req
    "CreateDBInstanceReadReplica"
    "fixture/CreateDBInstanceReadReplica.yaml"

requestDeleteDBParameterGroup :: DeleteDBParameterGroup -> TestTree
requestDeleteDBParameterGroup = req
    "DeleteDBParameterGroup"
    "fixture/DeleteDBParameterGroup.yaml"

requestDescribeDBSecurityGroups :: DescribeDBSecurityGroups -> TestTree
requestDescribeDBSecurityGroups = req
    "DescribeDBSecurityGroups"
    "fixture/DescribeDBSecurityGroups.yaml"

requestCopyOptionGroup :: CopyOptionGroup -> TestTree
requestCopyOptionGroup = req
    "CopyOptionGroup"
    "fixture/CopyOptionGroup.yaml"

requestRestoreDBClusterToPointInTime :: RestoreDBClusterToPointInTime -> TestTree
requestRestoreDBClusterToPointInTime = req
    "RestoreDBClusterToPointInTime"
    "fixture/RestoreDBClusterToPointInTime.yaml"

requestDescribeDBInstances :: DescribeDBInstances -> TestTree
requestDescribeDBInstances = req
    "DescribeDBInstances"
    "fixture/DescribeDBInstances.yaml"

requestRestoreDBInstanceFromS3 :: RestoreDBInstanceFromS3 -> TestTree
requestRestoreDBInstanceFromS3 = req
    "RestoreDBInstanceFromS3"
    "fixture/RestoreDBInstanceFromS3.yaml"

requestDownloadDBLogFilePortion :: DownloadDBLogFilePortion -> TestTree
requestDownloadDBLogFilePortion = req
    "DownloadDBLogFilePortion"
    "fixture/DownloadDBLogFilePortion.yaml"

-- Responses

responseDescribeDBClusterParameterGroups :: DescribeDBClusterParameterGroupsResponse -> TestTree
responseDescribeDBClusterParameterGroups = res
    "DescribeDBClusterParameterGroupsResponse"
    "fixture/DescribeDBClusterParameterGroupsResponse.proto"
    rds
    (Proxy :: Proxy DescribeDBClusterParameterGroups)

responsePromoteReadReplica :: PromoteReadReplicaResponse -> TestTree
responsePromoteReadReplica = res
    "PromoteReadReplicaResponse"
    "fixture/PromoteReadReplicaResponse.proto"
    rds
    (Proxy :: Proxy PromoteReadReplica)

responseDescribeDBEngineVersions :: DescribeDBEngineVersionsResponse -> TestTree
responseDescribeDBEngineVersions = res
    "DescribeDBEngineVersionsResponse"
    "fixture/DescribeDBEngineVersionsResponse.proto"
    rds
    (Proxy :: Proxy DescribeDBEngineVersions)

responseStopDBInstance :: StopDBInstanceResponse -> TestTree
responseStopDBInstance = res
    "StopDBInstanceResponse"
    "fixture/StopDBInstanceResponse.proto"
    rds
    (Proxy :: Proxy StopDBInstance)

responseCopyDBSnapshot :: CopyDBSnapshotResponse -> TestTree
responseCopyDBSnapshot = res
    "CopyDBSnapshotResponse"
    "fixture/CopyDBSnapshotResponse.proto"
    rds
    (Proxy :: Proxy CopyDBSnapshot)

responseAddSourceIdentifierToSubscription :: AddSourceIdentifierToSubscriptionResponse -> TestTree
responseAddSourceIdentifierToSubscription = res
    "AddSourceIdentifierToSubscriptionResponse"
    "fixture/AddSourceIdentifierToSubscriptionResponse.proto"
    rds
    (Proxy :: Proxy AddSourceIdentifierToSubscription)

responseModifyDBInstance :: ModifyDBInstanceResponse -> TestTree
responseModifyDBInstance = res
    "ModifyDBInstanceResponse"
    "fixture/ModifyDBInstanceResponse.proto"
    rds
    (Proxy :: Proxy ModifyDBInstance)

responseModifyEventSubscription :: ModifyEventSubscriptionResponse -> TestTree
responseModifyEventSubscription = res
    "ModifyEventSubscriptionResponse"
    "fixture/ModifyEventSubscriptionResponse.proto"
    rds
    (Proxy :: Proxy ModifyEventSubscription)

responseResetDBClusterParameterGroup :: DBClusterParameterGroupNameMessage -> TestTree
responseResetDBClusterParameterGroup = res
    "ResetDBClusterParameterGroupResponse"
    "fixture/ResetDBClusterParameterGroupResponse.proto"
    rds
    (Proxy :: Proxy ResetDBClusterParameterGroup)

responseRestoreDBClusterFromS3 :: RestoreDBClusterFromS3Response -> TestTree
responseRestoreDBClusterFromS3 = res
    "RestoreDBClusterFromS3Response"
    "fixture/RestoreDBClusterFromS3Response.proto"
    rds
    (Proxy :: Proxy RestoreDBClusterFromS3)

responseDescribeEvents :: DescribeEventsResponse -> TestTree
responseDescribeEvents = res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse.proto"
    rds
    (Proxy :: Proxy DescribeEvents)

responseDescribeEngineDefaultParameters :: DescribeEngineDefaultParametersResponse -> TestTree
responseDescribeEngineDefaultParameters = res
    "DescribeEngineDefaultParametersResponse"
    "fixture/DescribeEngineDefaultParametersResponse.proto"
    rds
    (Proxy :: Proxy DescribeEngineDefaultParameters)

responseDescribeOptionGroups :: DescribeOptionGroupsResponse -> TestTree
responseDescribeOptionGroups = res
    "DescribeOptionGroupsResponse"
    "fixture/DescribeOptionGroupsResponse.proto"
    rds
    (Proxy :: Proxy DescribeOptionGroups)

responseDescribeDBLogFiles :: DescribeDBLogFilesResponse -> TestTree
responseDescribeDBLogFiles = res
    "DescribeDBLogFilesResponse"
    "fixture/DescribeDBLogFilesResponse.proto"
    rds
    (Proxy :: Proxy DescribeDBLogFiles)

responseDescribeDBClusters :: DescribeDBClustersResponse -> TestTree
responseDescribeDBClusters = res
    "DescribeDBClustersResponse"
    "fixture/DescribeDBClustersResponse.proto"
    rds
    (Proxy :: Proxy DescribeDBClusters)

responseModifyDBSubnetGroup :: ModifyDBSubnetGroupResponse -> TestTree
responseModifyDBSubnetGroup = res
    "ModifyDBSubnetGroupResponse"
    "fixture/ModifyDBSubnetGroupResponse.proto"
    rds
    (Proxy :: Proxy ModifyDBSubnetGroup)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource = res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    rds
    (Proxy :: Proxy ListTagsForResource)

responseDeleteOptionGroup :: DeleteOptionGroupResponse -> TestTree
responseDeleteOptionGroup = res
    "DeleteOptionGroupResponse"
    "fixture/DeleteOptionGroupResponse.proto"
    rds
    (Proxy :: Proxy DeleteOptionGroup)

responseDeleteDBCluster :: DeleteDBClusterResponse -> TestTree
responseDeleteDBCluster = res
    "DeleteDBClusterResponse"
    "fixture/DeleteDBClusterResponse.proto"
    rds
    (Proxy :: Proxy DeleteDBCluster)

responseDescribeReservedDBInstances :: DescribeReservedDBInstancesResponse -> TestTree
responseDescribeReservedDBInstances = res
    "DescribeReservedDBInstancesResponse"
    "fixture/DescribeReservedDBInstancesResponse.proto"
    rds
    (Proxy :: Proxy DescribeReservedDBInstances)

responseCopyDBParameterGroup :: CopyDBParameterGroupResponse -> TestTree
responseCopyDBParameterGroup = res
    "CopyDBParameterGroupResponse"
    "fixture/CopyDBParameterGroupResponse.proto"
    rds
    (Proxy :: Proxy CopyDBParameterGroup)

responseRemoveSourceIdentifierFromSubscription :: RemoveSourceIdentifierFromSubscriptionResponse -> TestTree
responseRemoveSourceIdentifierFromSubscription = res
    "RemoveSourceIdentifierFromSubscriptionResponse"
    "fixture/RemoveSourceIdentifierFromSubscriptionResponse.proto"
    rds
    (Proxy :: Proxy RemoveSourceIdentifierFromSubscription)

responseDescribeEngineDefaultClusterParameters :: DescribeEngineDefaultClusterParametersResponse -> TestTree
responseDescribeEngineDefaultClusterParameters = res
    "DescribeEngineDefaultClusterParametersResponse"
    "fixture/DescribeEngineDefaultClusterParametersResponse.proto"
    rds
    (Proxy :: Proxy DescribeEngineDefaultClusterParameters)

responseDescribeDBSnapshotAttributes :: DescribeDBSnapshotAttributesResponse -> TestTree
responseDescribeDBSnapshotAttributes = res
    "DescribeDBSnapshotAttributesResponse"
    "fixture/DescribeDBSnapshotAttributesResponse.proto"
    rds
    (Proxy :: Proxy DescribeDBSnapshotAttributes)

responseBacktrackDBCluster :: DBClusterBacktrack -> TestTree
responseBacktrackDBCluster = res
    "BacktrackDBClusterResponse"
    "fixture/BacktrackDBClusterResponse.proto"
    rds
    (Proxy :: Proxy BacktrackDBCluster)

responsePromoteReadReplicaDBCluster :: PromoteReadReplicaDBClusterResponse -> TestTree
responsePromoteReadReplicaDBCluster = res
    "PromoteReadReplicaDBClusterResponse"
    "fixture/PromoteReadReplicaDBClusterResponse.proto"
    rds
    (Proxy :: Proxy PromoteReadReplicaDBCluster)

responseRemoveTagsFromResource :: RemoveTagsFromResourceResponse -> TestTree
responseRemoveTagsFromResource = res
    "RemoveTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse.proto"
    rds
    (Proxy :: Proxy RemoveTagsFromResource)

responseRestoreDBInstanceFromDBSnapshot :: RestoreDBInstanceFromDBSnapshotResponse -> TestTree
responseRestoreDBInstanceFromDBSnapshot = res
    "RestoreDBInstanceFromDBSnapshotResponse"
    "fixture/RestoreDBInstanceFromDBSnapshotResponse.proto"
    rds
    (Proxy :: Proxy RestoreDBInstanceFromDBSnapshot)

responseCreateEventSubscription :: CreateEventSubscriptionResponse -> TestTree
responseCreateEventSubscription = res
    "CreateEventSubscriptionResponse"
    "fixture/CreateEventSubscriptionResponse.proto"
    rds
    (Proxy :: Proxy CreateEventSubscription)

responsePurchaseReservedDBInstancesOffering :: PurchaseReservedDBInstancesOfferingResponse -> TestTree
responsePurchaseReservedDBInstancesOffering = res
    "PurchaseReservedDBInstancesOfferingResponse"
    "fixture/PurchaseReservedDBInstancesOfferingResponse.proto"
    rds
    (Proxy :: Proxy PurchaseReservedDBInstancesOffering)

responseCreateDBInstance :: CreateDBInstanceResponse -> TestTree
responseCreateDBInstance = res
    "CreateDBInstanceResponse"
    "fixture/CreateDBInstanceResponse.proto"
    rds
    (Proxy :: Proxy CreateDBInstance)

responseDeleteDBClusterParameterGroup :: DeleteDBClusterParameterGroupResponse -> TestTree
responseDeleteDBClusterParameterGroup = res
    "DeleteDBClusterParameterGroupResponse"
    "fixture/DeleteDBClusterParameterGroupResponse.proto"
    rds
    (Proxy :: Proxy DeleteDBClusterParameterGroup)

responseDescribeCertificates :: DescribeCertificatesResponse -> TestTree
responseDescribeCertificates = res
    "DescribeCertificatesResponse"
    "fixture/DescribeCertificatesResponse.proto"
    rds
    (Proxy :: Proxy DescribeCertificates)

responseAuthorizeDBSecurityGroupIngress :: AuthorizeDBSecurityGroupIngressResponse -> TestTree
responseAuthorizeDBSecurityGroupIngress = res
    "AuthorizeDBSecurityGroupIngressResponse"
    "fixture/AuthorizeDBSecurityGroupIngressResponse.proto"
    rds
    (Proxy :: Proxy AuthorizeDBSecurityGroupIngress)

responseDescribeSourceRegions :: DescribeSourceRegionsResponse -> TestTree
responseDescribeSourceRegions = res
    "DescribeSourceRegionsResponse"
    "fixture/DescribeSourceRegionsResponse.proto"
    rds
    (Proxy :: Proxy DescribeSourceRegions)

responseRestoreDBClusterFromSnapshot :: RestoreDBClusterFromSnapshotResponse -> TestTree
responseRestoreDBClusterFromSnapshot = res
    "RestoreDBClusterFromSnapshotResponse"
    "fixture/RestoreDBClusterFromSnapshotResponse.proto"
    rds
    (Proxy :: Proxy RestoreDBClusterFromSnapshot)

responseDescribeOrderableDBInstanceOptions :: DescribeOrderableDBInstanceOptionsResponse -> TestTree
responseDescribeOrderableDBInstanceOptions = res
    "DescribeOrderableDBInstanceOptionsResponse"
    "fixture/DescribeOrderableDBInstanceOptionsResponse.proto"
    rds
    (Proxy :: Proxy DescribeOrderableDBInstanceOptions)

responseCreateDBClusterParameterGroup :: CreateDBClusterParameterGroupResponse -> TestTree
responseCreateDBClusterParameterGroup = res
    "CreateDBClusterParameterGroupResponse"
    "fixture/CreateDBClusterParameterGroupResponse.proto"
    rds
    (Proxy :: Proxy CreateDBClusterParameterGroup)

responseCreateDBSnapshot :: CreateDBSnapshotResponse -> TestTree
responseCreateDBSnapshot = res
    "CreateDBSnapshotResponse"
    "fixture/CreateDBSnapshotResponse.proto"
    rds
    (Proxy :: Proxy CreateDBSnapshot)

responseDeleteEventSubscription :: DeleteEventSubscriptionResponse -> TestTree
responseDeleteEventSubscription = res
    "DeleteEventSubscriptionResponse"
    "fixture/DeleteEventSubscriptionResponse.proto"
    rds
    (Proxy :: Proxy DeleteEventSubscription)

responseDescribeDBClusterBacktracks :: DescribeDBClusterBacktracksResponse -> TestTree
responseDescribeDBClusterBacktracks = res
    "DescribeDBClusterBacktracksResponse"
    "fixture/DescribeDBClusterBacktracksResponse.proto"
    rds
    (Proxy :: Proxy DescribeDBClusterBacktracks)

responseDescribeDBParameterGroups :: DescribeDBParameterGroupsResponse -> TestTree
responseDescribeDBParameterGroups = res
    "DescribeDBParameterGroupsResponse"
    "fixture/DescribeDBParameterGroupsResponse.proto"
    rds
    (Proxy :: Proxy DescribeDBParameterGroups)

responseModifyDBSnapshotAttribute :: ModifyDBSnapshotAttributeResponse -> TestTree
responseModifyDBSnapshotAttribute = res
    "ModifyDBSnapshotAttributeResponse"
    "fixture/ModifyDBSnapshotAttributeResponse.proto"
    rds
    (Proxy :: Proxy ModifyDBSnapshotAttribute)

responseDeleteDBClusterSnapshot :: DeleteDBClusterSnapshotResponse -> TestTree
responseDeleteDBClusterSnapshot = res
    "DeleteDBClusterSnapshotResponse"
    "fixture/DeleteDBClusterSnapshotResponse.proto"
    rds
    (Proxy :: Proxy DeleteDBClusterSnapshot)

responseDescribeValidDBInstanceModifications :: DescribeValidDBInstanceModificationsResponse -> TestTree
responseDescribeValidDBInstanceModifications = res
    "DescribeValidDBInstanceModificationsResponse"
    "fixture/DescribeValidDBInstanceModificationsResponse.proto"
    rds
    (Proxy :: Proxy DescribeValidDBInstanceModifications)

responseDescribeOptionGroupOptions :: DescribeOptionGroupOptionsResponse -> TestTree
responseDescribeOptionGroupOptions = res
    "DescribeOptionGroupOptionsResponse"
    "fixture/DescribeOptionGroupOptionsResponse.proto"
    rds
    (Proxy :: Proxy DescribeOptionGroupOptions)

responseDescribeEventSubscriptions :: DescribeEventSubscriptionsResponse -> TestTree
responseDescribeEventSubscriptions = res
    "DescribeEventSubscriptionsResponse"
    "fixture/DescribeEventSubscriptionsResponse.proto"
    rds
    (Proxy :: Proxy DescribeEventSubscriptions)

responseAddTagsToResource :: AddTagsToResourceResponse -> TestTree
responseAddTagsToResource = res
    "AddTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse.proto"
    rds
    (Proxy :: Proxy AddTagsToResource)

responseDescribeDBParameters :: DescribeDBParametersResponse -> TestTree
responseDescribeDBParameters = res
    "DescribeDBParametersResponse"
    "fixture/DescribeDBParametersResponse.proto"
    rds
    (Proxy :: Proxy DescribeDBParameters)

responseCreateDBClusterSnapshot :: CreateDBClusterSnapshotResponse -> TestTree
responseCreateDBClusterSnapshot = res
    "CreateDBClusterSnapshotResponse"
    "fixture/CreateDBClusterSnapshotResponse.proto"
    rds
    (Proxy :: Proxy CreateDBClusterSnapshot)

responseDescribeDBSnapshots :: DescribeDBSnapshotsResponse -> TestTree
responseDescribeDBSnapshots = res
    "DescribeDBSnapshotsResponse"
    "fixture/DescribeDBSnapshotsResponse.proto"
    rds
    (Proxy :: Proxy DescribeDBSnapshots)

responseDescribeDBSubnetGroups :: DescribeDBSubnetGroupsResponse -> TestTree
responseDescribeDBSubnetGroups = res
    "DescribeDBSubnetGroupsResponse"
    "fixture/DescribeDBSubnetGroupsResponse.proto"
    rds
    (Proxy :: Proxy DescribeDBSubnetGroups)

responseModifyOptionGroup :: ModifyOptionGroupResponse -> TestTree
responseModifyOptionGroup = res
    "ModifyOptionGroupResponse"
    "fixture/ModifyOptionGroupResponse.proto"
    rds
    (Proxy :: Proxy ModifyOptionGroup)

responseCreateDBParameterGroup :: CreateDBParameterGroupResponse -> TestTree
responseCreateDBParameterGroup = res
    "CreateDBParameterGroupResponse"
    "fixture/CreateDBParameterGroupResponse.proto"
    rds
    (Proxy :: Proxy CreateDBParameterGroup)

responseModifyDBClusterSnapshotAttribute :: ModifyDBClusterSnapshotAttributeResponse -> TestTree
responseModifyDBClusterSnapshotAttribute = res
    "ModifyDBClusterSnapshotAttributeResponse"
    "fixture/ModifyDBClusterSnapshotAttributeResponse.proto"
    rds
    (Proxy :: Proxy ModifyDBClusterSnapshotAttribute)

responseModifyDBCluster :: ModifyDBClusterResponse -> TestTree
responseModifyDBCluster = res
    "ModifyDBClusterResponse"
    "fixture/ModifyDBClusterResponse.proto"
    rds
    (Proxy :: Proxy ModifyDBCluster)

responseCopyDBClusterParameterGroup :: CopyDBClusterParameterGroupResponse -> TestTree
responseCopyDBClusterParameterGroup = res
    "CopyDBClusterParameterGroupResponse"
    "fixture/CopyDBClusterParameterGroupResponse.proto"
    rds
    (Proxy :: Proxy CopyDBClusterParameterGroup)

responseDescribeEventCategories :: DescribeEventCategoriesResponse -> TestTree
responseDescribeEventCategories = res
    "DescribeEventCategoriesResponse"
    "fixture/DescribeEventCategoriesResponse.proto"
    rds
    (Proxy :: Proxy DescribeEventCategories)

responseStartDBInstance :: StartDBInstanceResponse -> TestTree
responseStartDBInstance = res
    "StartDBInstanceResponse"
    "fixture/StartDBInstanceResponse.proto"
    rds
    (Proxy :: Proxy StartDBInstance)

responseModifyDBClusterParameterGroup :: DBClusterParameterGroupNameMessage -> TestTree
responseModifyDBClusterParameterGroup = res
    "ModifyDBClusterParameterGroupResponse"
    "fixture/ModifyDBClusterParameterGroupResponse.proto"
    rds
    (Proxy :: Proxy ModifyDBClusterParameterGroup)

responseRestoreDBInstanceToPointInTime :: RestoreDBInstanceToPointInTimeResponse -> TestTree
responseRestoreDBInstanceToPointInTime = res
    "RestoreDBInstanceToPointInTimeResponse"
    "fixture/RestoreDBInstanceToPointInTimeResponse.proto"
    rds
    (Proxy :: Proxy RestoreDBInstanceToPointInTime)

responseDescribeDBClusterSnapshotAttributes :: DescribeDBClusterSnapshotAttributesResponse -> TestTree
responseDescribeDBClusterSnapshotAttributes = res
    "DescribeDBClusterSnapshotAttributesResponse"
    "fixture/DescribeDBClusterSnapshotAttributesResponse.proto"
    rds
    (Proxy :: Proxy DescribeDBClusterSnapshotAttributes)

responseModifyDBSnapshot :: ModifyDBSnapshotResponse -> TestTree
responseModifyDBSnapshot = res
    "ModifyDBSnapshotResponse"
    "fixture/ModifyDBSnapshotResponse.proto"
    rds
    (Proxy :: Proxy ModifyDBSnapshot)

responseDescribePendingMaintenanceActions :: DescribePendingMaintenanceActionsResponse -> TestTree
responseDescribePendingMaintenanceActions = res
    "DescribePendingMaintenanceActionsResponse"
    "fixture/DescribePendingMaintenanceActionsResponse.proto"
    rds
    (Proxy :: Proxy DescribePendingMaintenanceActions)

responseAddRoleToDBCluster :: AddRoleToDBClusterResponse -> TestTree
responseAddRoleToDBCluster = res
    "AddRoleToDBClusterResponse"
    "fixture/AddRoleToDBClusterResponse.proto"
    rds
    (Proxy :: Proxy AddRoleToDBCluster)

responseCopyDBClusterSnapshot :: CopyDBClusterSnapshotResponse -> TestTree
responseCopyDBClusterSnapshot = res
    "CopyDBClusterSnapshotResponse"
    "fixture/CopyDBClusterSnapshotResponse.proto"
    rds
    (Proxy :: Proxy CopyDBClusterSnapshot)

responseResetDBParameterGroup :: DBParameterGroupNameMessage -> TestTree
responseResetDBParameterGroup = res
    "ResetDBParameterGroupResponse"
    "fixture/ResetDBParameterGroupResponse.proto"
    rds
    (Proxy :: Proxy ResetDBParameterGroup)

responseCreateDBCluster :: CreateDBClusterResponse -> TestTree
responseCreateDBCluster = res
    "CreateDBClusterResponse"
    "fixture/CreateDBClusterResponse.proto"
    rds
    (Proxy :: Proxy CreateDBCluster)

responseRemoveRoleFromDBCluster :: RemoveRoleFromDBClusterResponse -> TestTree
responseRemoveRoleFromDBCluster = res
    "RemoveRoleFromDBClusterResponse"
    "fixture/RemoveRoleFromDBClusterResponse.proto"
    rds
    (Proxy :: Proxy RemoveRoleFromDBCluster)

responseFailoverDBCluster :: FailoverDBClusterResponse -> TestTree
responseFailoverDBCluster = res
    "FailoverDBClusterResponse"
    "fixture/FailoverDBClusterResponse.proto"
    rds
    (Proxy :: Proxy FailoverDBCluster)

responseRevokeDBSecurityGroupIngress :: RevokeDBSecurityGroupIngressResponse -> TestTree
responseRevokeDBSecurityGroupIngress = res
    "RevokeDBSecurityGroupIngressResponse"
    "fixture/RevokeDBSecurityGroupIngressResponse.proto"
    rds
    (Proxy :: Proxy RevokeDBSecurityGroupIngress)

responseModifyDBParameterGroup :: DBParameterGroupNameMessage -> TestTree
responseModifyDBParameterGroup = res
    "ModifyDBParameterGroupResponse"
    "fixture/ModifyDBParameterGroupResponse.proto"
    rds
    (Proxy :: Proxy ModifyDBParameterGroup)

responseApplyPendingMaintenanceAction :: ApplyPendingMaintenanceActionResponse -> TestTree
responseApplyPendingMaintenanceAction = res
    "ApplyPendingMaintenanceActionResponse"
    "fixture/ApplyPendingMaintenanceActionResponse.proto"
    rds
    (Proxy :: Proxy ApplyPendingMaintenanceAction)

responseCreateOptionGroup :: CreateOptionGroupResponse -> TestTree
responseCreateOptionGroup = res
    "CreateOptionGroupResponse"
    "fixture/CreateOptionGroupResponse.proto"
    rds
    (Proxy :: Proxy CreateOptionGroup)

responseDescribeAccountAttributes :: DescribeAccountAttributesResponse -> TestTree
responseDescribeAccountAttributes = res
    "DescribeAccountAttributesResponse"
    "fixture/DescribeAccountAttributesResponse.proto"
    rds
    (Proxy :: Proxy DescribeAccountAttributes)

responseDeleteDBSnapshot :: DeleteDBSnapshotResponse -> TestTree
responseDeleteDBSnapshot = res
    "DeleteDBSnapshotResponse"
    "fixture/DeleteDBSnapshotResponse.proto"
    rds
    (Proxy :: Proxy DeleteDBSnapshot)

responseDescribeDBClusterParameters :: DescribeDBClusterParametersResponse -> TestTree
responseDescribeDBClusterParameters = res
    "DescribeDBClusterParametersResponse"
    "fixture/DescribeDBClusterParametersResponse.proto"
    rds
    (Proxy :: Proxy DescribeDBClusterParameters)

responseDeleteDBSubnetGroup :: DeleteDBSubnetGroupResponse -> TestTree
responseDeleteDBSubnetGroup = res
    "DeleteDBSubnetGroupResponse"
    "fixture/DeleteDBSubnetGroupResponse.proto"
    rds
    (Proxy :: Proxy DeleteDBSubnetGroup)

responseCreateDBSecurityGroup :: CreateDBSecurityGroupResponse -> TestTree
responseCreateDBSecurityGroup = res
    "CreateDBSecurityGroupResponse"
    "fixture/CreateDBSecurityGroupResponse.proto"
    rds
    (Proxy :: Proxy CreateDBSecurityGroup)

responseDescribeDBClusterSnapshots :: DescribeDBClusterSnapshotsResponse -> TestTree
responseDescribeDBClusterSnapshots = res
    "DescribeDBClusterSnapshotsResponse"
    "fixture/DescribeDBClusterSnapshotsResponse.proto"
    rds
    (Proxy :: Proxy DescribeDBClusterSnapshots)

responseRebootDBInstance :: RebootDBInstanceResponse -> TestTree
responseRebootDBInstance = res
    "RebootDBInstanceResponse"
    "fixture/RebootDBInstanceResponse.proto"
    rds
    (Proxy :: Proxy RebootDBInstance)

responseCreateDBSubnetGroup :: CreateDBSubnetGroupResponse -> TestTree
responseCreateDBSubnetGroup = res
    "CreateDBSubnetGroupResponse"
    "fixture/CreateDBSubnetGroupResponse.proto"
    rds
    (Proxy :: Proxy CreateDBSubnetGroup)

responseDescribeReservedDBInstancesOfferings :: DescribeReservedDBInstancesOfferingsResponse -> TestTree
responseDescribeReservedDBInstancesOfferings = res
    "DescribeReservedDBInstancesOfferingsResponse"
    "fixture/DescribeReservedDBInstancesOfferingsResponse.proto"
    rds
    (Proxy :: Proxy DescribeReservedDBInstancesOfferings)

responseDeleteDBSecurityGroup :: DeleteDBSecurityGroupResponse -> TestTree
responseDeleteDBSecurityGroup = res
    "DeleteDBSecurityGroupResponse"
    "fixture/DeleteDBSecurityGroupResponse.proto"
    rds
    (Proxy :: Proxy DeleteDBSecurityGroup)

responseDeleteDBInstance :: DeleteDBInstanceResponse -> TestTree
responseDeleteDBInstance = res
    "DeleteDBInstanceResponse"
    "fixture/DeleteDBInstanceResponse.proto"
    rds
    (Proxy :: Proxy DeleteDBInstance)

responseCreateDBInstanceReadReplica :: CreateDBInstanceReadReplicaResponse -> TestTree
responseCreateDBInstanceReadReplica = res
    "CreateDBInstanceReadReplicaResponse"
    "fixture/CreateDBInstanceReadReplicaResponse.proto"
    rds
    (Proxy :: Proxy CreateDBInstanceReadReplica)

responseDeleteDBParameterGroup :: DeleteDBParameterGroupResponse -> TestTree
responseDeleteDBParameterGroup = res
    "DeleteDBParameterGroupResponse"
    "fixture/DeleteDBParameterGroupResponse.proto"
    rds
    (Proxy :: Proxy DeleteDBParameterGroup)

responseDescribeDBSecurityGroups :: DescribeDBSecurityGroupsResponse -> TestTree
responseDescribeDBSecurityGroups = res
    "DescribeDBSecurityGroupsResponse"
    "fixture/DescribeDBSecurityGroupsResponse.proto"
    rds
    (Proxy :: Proxy DescribeDBSecurityGroups)

responseCopyOptionGroup :: CopyOptionGroupResponse -> TestTree
responseCopyOptionGroup = res
    "CopyOptionGroupResponse"
    "fixture/CopyOptionGroupResponse.proto"
    rds
    (Proxy :: Proxy CopyOptionGroup)

responseRestoreDBClusterToPointInTime :: RestoreDBClusterToPointInTimeResponse -> TestTree
responseRestoreDBClusterToPointInTime = res
    "RestoreDBClusterToPointInTimeResponse"
    "fixture/RestoreDBClusterToPointInTimeResponse.proto"
    rds
    (Proxy :: Proxy RestoreDBClusterToPointInTime)

responseDescribeDBInstances :: DescribeDBInstancesResponse -> TestTree
responseDescribeDBInstances = res
    "DescribeDBInstancesResponse"
    "fixture/DescribeDBInstancesResponse.proto"
    rds
    (Proxy :: Proxy DescribeDBInstances)

responseRestoreDBInstanceFromS3 :: RestoreDBInstanceFromS3Response -> TestTree
responseRestoreDBInstanceFromS3 = res
    "RestoreDBInstanceFromS3Response"
    "fixture/RestoreDBInstanceFromS3Response.proto"
    rds
    (Proxy :: Proxy RestoreDBInstanceFromS3)

responseDownloadDBLogFilePortion :: DownloadDBLogFilePortionResponse -> TestTree
responseDownloadDBLogFilePortion = res
    "DownloadDBLogFilePortionResponse"
    "fixture/DownloadDBLogFilePortionResponse.proto"
    rds
    (Proxy :: Proxy DownloadDBLogFilePortion)
