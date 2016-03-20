{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.RDS
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.RDS where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.RDS
import Test.AWS.RDS.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testDescribeDBClusterParameterGroups $
--             describeDBClusterParameterGroups
--
--         , testPromoteReadReplica $
--             promoteReadReplica
--
--         , testDescribeDBEngineVersions $
--             describeDBEngineVersions
--
--         , testCopyDBSnapshot $
--             copyDBSnapshot
--
--         , testAddSourceIdentifierToSubscription $
--             addSourceIdentifierToSubscription
--
--         , testModifyDBInstance $
--             modifyDBInstance
--
--         , testModifyEventSubscription $
--             modifyEventSubscription
--
--         , testResetDBClusterParameterGroup $
--             resetDBClusterParameterGroup
--
--         , testDescribeEvents $
--             describeEvents
--
--         , testDescribeEngineDefaultParameters $
--             describeEngineDefaultParameters
--
--         , testDescribeOptionGroups $
--             describeOptionGroups
--
--         , testDescribeDBLogFiles $
--             describeDBLogFiles
--
--         , testDescribeDBClusters $
--             describeDBClusters
--
--         , testModifyDBSubnetGroup $
--             modifyDBSubnetGroup
--
--         , testListTagsForResource $
--             listTagsForResource
--
--         , testDeleteOptionGroup $
--             deleteOptionGroup
--
--         , testDeleteDBCluster $
--             deleteDBCluster
--
--         , testDescribeReservedDBInstances $
--             describeReservedDBInstances
--
--         , testCopyDBParameterGroup $
--             copyDBParameterGroup
--
--         , testRemoveSourceIdentifierFromSubscription $
--             removeSourceIdentifierFromSubscription
--
--         , testDescribeEngineDefaultClusterParameters $
--             describeEngineDefaultClusterParameters
--
--         , testDescribeDBSnapshotAttributes $
--             describeDBSnapshotAttributes
--
--         , testRemoveTagsFromResource $
--             removeTagsFromResource
--
--         , testRestoreDBInstanceFromDBSnapshot $
--             restoreDBInstanceFromDBSnapshot
--
--         , testCreateEventSubscription $
--             createEventSubscription
--
--         , testPurchaseReservedDBInstancesOffering $
--             purchaseReservedDBInstancesOffering
--
--         , testCreateDBInstance $
--             createDBInstance
--
--         , testDeleteDBClusterParameterGroup $
--             deleteDBClusterParameterGroup
--
--         , testDescribeCertificates $
--             describeCertificates
--
--         , testAuthorizeDBSecurityGroupIngress $
--             authorizeDBSecurityGroupIngress
--
--         , testRestoreDBClusterFromSnapshot $
--             restoreDBClusterFromSnapshot
--
--         , testDescribeOrderableDBInstanceOptions $
--             describeOrderableDBInstanceOptions
--
--         , testCreateDBClusterParameterGroup $
--             createDBClusterParameterGroup
--
--         , testCreateDBSnapshot $
--             createDBSnapshot
--
--         , testDeleteEventSubscription $
--             deleteEventSubscription
--
--         , testDescribeDBParameterGroups $
--             describeDBParameterGroups
--
--         , testModifyDBSnapshotAttribute $
--             modifyDBSnapshotAttribute
--
--         , testDeleteDBClusterSnapshot $
--             deleteDBClusterSnapshot
--
--         , testDescribeOptionGroupOptions $
--             describeOptionGroupOptions
--
--         , testDescribeEventSubscriptions $
--             describeEventSubscriptions
--
--         , testAddTagsToResource $
--             addTagsToResource
--
--         , testDescribeDBParameters $
--             describeDBParameters
--
--         , testCreateDBClusterSnapshot $
--             createDBClusterSnapshot
--
--         , testDescribeDBSnapshots $
--             describeDBSnapshots
--
--         , testDescribeDBSubnetGroups $
--             describeDBSubnetGroups
--
--         , testModifyOptionGroup $
--             modifyOptionGroup
--
--         , testCreateDBParameterGroup $
--             createDBParameterGroup
--
--         , testModifyDBCluster $
--             modifyDBCluster
--
--         , testDescribeEventCategories $
--             describeEventCategories
--
--         , testModifyDBClusterParameterGroup $
--             modifyDBClusterParameterGroup
--
--         , testRestoreDBInstanceToPointInTime $
--             restoreDBInstanceToPointInTime
--
--         , testDescribePendingMaintenanceActions $
--             describePendingMaintenanceActions
--
--         , testCopyDBClusterSnapshot $
--             copyDBClusterSnapshot
--
--         , testResetDBParameterGroup $
--             resetDBParameterGroup
--
--         , testCreateDBCluster $
--             createDBCluster
--
--         , testFailoverDBCluster $
--             failoverDBCluster
--
--         , testRevokeDBSecurityGroupIngress $
--             revokeDBSecurityGroupIngress
--
--         , testModifyDBParameterGroup $
--             modifyDBParameterGroup
--
--         , testApplyPendingMaintenanceAction $
--             applyPendingMaintenanceAction
--
--         , testCreateOptionGroup $
--             createOptionGroup
--
--         , testDescribeAccountAttributes $
--             describeAccountAttributes
--
--         , testDeleteDBSnapshot $
--             deleteDBSnapshot
--
--         , testDescribeDBClusterParameters $
--             describeDBClusterParameters
--
--         , testDeleteDBSubnetGroup $
--             deleteDBSubnetGroup
--
--         , testCreateDBSecurityGroup $
--             createDBSecurityGroup
--
--         , testDescribeDBClusterSnapshots $
--             describeDBClusterSnapshots
--
--         , testRebootDBInstance $
--             rebootDBInstance
--
--         , testCreateDBSubnetGroup $
--             createDBSubnetGroup
--
--         , testDescribeReservedDBInstancesOfferings $
--             describeReservedDBInstancesOfferings
--
--         , testDeleteDBSecurityGroup $
--             deleteDBSecurityGroup
--
--         , testDeleteDBInstance $
--             deleteDBInstance
--
--         , testCreateDBInstanceReadReplica $
--             createDBInstanceReadReplica
--
--         , testDeleteDBParameterGroup $
--             deleteDBParameterGroup
--
--         , testDescribeDBSecurityGroups $
--             describeDBSecurityGroups
--
--         , testCopyOptionGroup $
--             copyOptionGroup
--
--         , testRestoreDBClusterToPointInTime $
--             restoreDBClusterToPointInTime
--
--         , testDescribeDBInstances $
--             describeDBInstances
--
--         , testDownloadDBLogFilePortion $
--             downloadDBLogFilePortion
--
--           ]

--     , testGroup "response"
--         [ testDescribeDBClusterParameterGroupsResponse $
--             describeDBClusterParameterGroupsResponse
--
--         , testPromoteReadReplicaResponse $
--             promoteReadReplicaResponse
--
--         , testDescribeDBEngineVersionsResponse $
--             describeDBEngineVersionsResponse
--
--         , testCopyDBSnapshotResponse $
--             copyDBSnapshotResponse
--
--         , testAddSourceIdentifierToSubscriptionResponse $
--             addSourceIdentifierToSubscriptionResponse
--
--         , testModifyDBInstanceResponse $
--             modifyDBInstanceResponse
--
--         , testModifyEventSubscriptionResponse $
--             modifyEventSubscriptionResponse
--
--         , testResetDBClusterParameterGroupResponse $
--             dbClusterParameterGroupNameMessage
--
--         , testDescribeEventsResponse $
--             describeEventsResponse
--
--         , testDescribeEngineDefaultParametersResponse $
--             describeEngineDefaultParametersResponse
--
--         , testDescribeOptionGroupsResponse $
--             describeOptionGroupsResponse
--
--         , testDescribeDBLogFilesResponse $
--             describeDBLogFilesResponse
--
--         , testDescribeDBClustersResponse $
--             describeDBClustersResponse
--
--         , testModifyDBSubnetGroupResponse $
--             modifyDBSubnetGroupResponse
--
--         , testListTagsForResourceResponse $
--             listTagsForResourceResponse
--
--         , testDeleteOptionGroupResponse $
--             deleteOptionGroupResponse
--
--         , testDeleteDBClusterResponse $
--             deleteDBClusterResponse
--
--         , testDescribeReservedDBInstancesResponse $
--             describeReservedDBInstancesResponse
--
--         , testCopyDBParameterGroupResponse $
--             copyDBParameterGroupResponse
--
--         , testRemoveSourceIdentifierFromSubscriptionResponse $
--             removeSourceIdentifierFromSubscriptionResponse
--
--         , testDescribeEngineDefaultClusterParametersResponse $
--             describeEngineDefaultClusterParametersResponse
--
--         , testDescribeDBSnapshotAttributesResponse $
--             describeDBSnapshotAttributesResponse
--
--         , testRemoveTagsFromResourceResponse $
--             removeTagsFromResourceResponse
--
--         , testRestoreDBInstanceFromDBSnapshotResponse $
--             restoreDBInstanceFromDBSnapshotResponse
--
--         , testCreateEventSubscriptionResponse $
--             createEventSubscriptionResponse
--
--         , testPurchaseReservedDBInstancesOfferingResponse $
--             purchaseReservedDBInstancesOfferingResponse
--
--         , testCreateDBInstanceResponse $
--             createDBInstanceResponse
--
--         , testDeleteDBClusterParameterGroupResponse $
--             deleteDBClusterParameterGroupResponse
--
--         , testDescribeCertificatesResponse $
--             describeCertificatesResponse
--
--         , testAuthorizeDBSecurityGroupIngressResponse $
--             authorizeDBSecurityGroupIngressResponse
--
--         , testRestoreDBClusterFromSnapshotResponse $
--             restoreDBClusterFromSnapshotResponse
--
--         , testDescribeOrderableDBInstanceOptionsResponse $
--             describeOrderableDBInstanceOptionsResponse
--
--         , testCreateDBClusterParameterGroupResponse $
--             createDBClusterParameterGroupResponse
--
--         , testCreateDBSnapshotResponse $
--             createDBSnapshotResponse
--
--         , testDeleteEventSubscriptionResponse $
--             deleteEventSubscriptionResponse
--
--         , testDescribeDBParameterGroupsResponse $
--             describeDBParameterGroupsResponse
--
--         , testModifyDBSnapshotAttributeResponse $
--             modifyDBSnapshotAttributeResponse
--
--         , testDeleteDBClusterSnapshotResponse $
--             deleteDBClusterSnapshotResponse
--
--         , testDescribeOptionGroupOptionsResponse $
--             describeOptionGroupOptionsResponse
--
--         , testDescribeEventSubscriptionsResponse $
--             describeEventSubscriptionsResponse
--
--         , testAddTagsToResourceResponse $
--             addTagsToResourceResponse
--
--         , testDescribeDBParametersResponse $
--             describeDBParametersResponse
--
--         , testCreateDBClusterSnapshotResponse $
--             createDBClusterSnapshotResponse
--
--         , testDescribeDBSnapshotsResponse $
--             describeDBSnapshotsResponse
--
--         , testDescribeDBSubnetGroupsResponse $
--             describeDBSubnetGroupsResponse
--
--         , testModifyOptionGroupResponse $
--             modifyOptionGroupResponse
--
--         , testCreateDBParameterGroupResponse $
--             createDBParameterGroupResponse
--
--         , testModifyDBClusterResponse $
--             modifyDBClusterResponse
--
--         , testDescribeEventCategoriesResponse $
--             describeEventCategoriesResponse
--
--         , testModifyDBClusterParameterGroupResponse $
--             dbClusterParameterGroupNameMessage
--
--         , testRestoreDBInstanceToPointInTimeResponse $
--             restoreDBInstanceToPointInTimeResponse
--
--         , testDescribePendingMaintenanceActionsResponse $
--             describePendingMaintenanceActionsResponse
--
--         , testCopyDBClusterSnapshotResponse $
--             copyDBClusterSnapshotResponse
--
--         , testResetDBParameterGroupResponse $
--             dbParameterGroupNameMessage
--
--         , testCreateDBClusterResponse $
--             createDBClusterResponse
--
--         , testFailoverDBClusterResponse $
--             failoverDBClusterResponse
--
--         , testRevokeDBSecurityGroupIngressResponse $
--             revokeDBSecurityGroupIngressResponse
--
--         , testModifyDBParameterGroupResponse $
--             dbParameterGroupNameMessage
--
--         , testApplyPendingMaintenanceActionResponse $
--             applyPendingMaintenanceActionResponse
--
--         , testCreateOptionGroupResponse $
--             createOptionGroupResponse
--
--         , testDescribeAccountAttributesResponse $
--             describeAccountAttributesResponse
--
--         , testDeleteDBSnapshotResponse $
--             deleteDBSnapshotResponse
--
--         , testDescribeDBClusterParametersResponse $
--             describeDBClusterParametersResponse
--
--         , testDeleteDBSubnetGroupResponse $
--             deleteDBSubnetGroupResponse
--
--         , testCreateDBSecurityGroupResponse $
--             createDBSecurityGroupResponse
--
--         , testDescribeDBClusterSnapshotsResponse $
--             describeDBClusterSnapshotsResponse
--
--         , testRebootDBInstanceResponse $
--             rebootDBInstanceResponse
--
--         , testCreateDBSubnetGroupResponse $
--             createDBSubnetGroupResponse
--
--         , testDescribeReservedDBInstancesOfferingsResponse $
--             describeReservedDBInstancesOfferingsResponse
--
--         , testDeleteDBSecurityGroupResponse $
--             deleteDBSecurityGroupResponse
--
--         , testDeleteDBInstanceResponse $
--             deleteDBInstanceResponse
--
--         , testCreateDBInstanceReadReplicaResponse $
--             createDBInstanceReadReplicaResponse
--
--         , testDeleteDBParameterGroupResponse $
--             deleteDBParameterGroupResponse
--
--         , testDescribeDBSecurityGroupsResponse $
--             describeDBSecurityGroupsResponse
--
--         , testCopyOptionGroupResponse $
--             copyOptionGroupResponse
--
--         , testRestoreDBClusterToPointInTimeResponse $
--             restoreDBClusterToPointInTimeResponse
--
--         , testDescribeDBInstancesResponse $
--             describeDBInstancesResponse
--
--         , testDownloadDBLogFilePortionResponse $
--             downloadDBLogFilePortionResponse
--
--           ]
--     ]

-- Requests

testDescribeDBClusterParameterGroups :: DescribeDBClusterParameterGroups -> TestTree
testDescribeDBClusterParameterGroups = req
    "DescribeDBClusterParameterGroups"
    "fixture/DescribeDBClusterParameterGroups.yaml"

testPromoteReadReplica :: PromoteReadReplica -> TestTree
testPromoteReadReplica = req
    "PromoteReadReplica"
    "fixture/PromoteReadReplica.yaml"

testDescribeDBEngineVersions :: DescribeDBEngineVersions -> TestTree
testDescribeDBEngineVersions = req
    "DescribeDBEngineVersions"
    "fixture/DescribeDBEngineVersions.yaml"

testCopyDBSnapshot :: CopyDBSnapshot -> TestTree
testCopyDBSnapshot = req
    "CopyDBSnapshot"
    "fixture/CopyDBSnapshot.yaml"

testAddSourceIdentifierToSubscription :: AddSourceIdentifierToSubscription -> TestTree
testAddSourceIdentifierToSubscription = req
    "AddSourceIdentifierToSubscription"
    "fixture/AddSourceIdentifierToSubscription.yaml"

testModifyDBInstance :: ModifyDBInstance -> TestTree
testModifyDBInstance = req
    "ModifyDBInstance"
    "fixture/ModifyDBInstance.yaml"

testModifyEventSubscription :: ModifyEventSubscription -> TestTree
testModifyEventSubscription = req
    "ModifyEventSubscription"
    "fixture/ModifyEventSubscription.yaml"

testResetDBClusterParameterGroup :: ResetDBClusterParameterGroup -> TestTree
testResetDBClusterParameterGroup = req
    "ResetDBClusterParameterGroup"
    "fixture/ResetDBClusterParameterGroup.yaml"

testDescribeEvents :: DescribeEvents -> TestTree
testDescribeEvents = req
    "DescribeEvents"
    "fixture/DescribeEvents.yaml"

testDescribeEngineDefaultParameters :: DescribeEngineDefaultParameters -> TestTree
testDescribeEngineDefaultParameters = req
    "DescribeEngineDefaultParameters"
    "fixture/DescribeEngineDefaultParameters.yaml"

testDescribeOptionGroups :: DescribeOptionGroups -> TestTree
testDescribeOptionGroups = req
    "DescribeOptionGroups"
    "fixture/DescribeOptionGroups.yaml"

testDescribeDBLogFiles :: DescribeDBLogFiles -> TestTree
testDescribeDBLogFiles = req
    "DescribeDBLogFiles"
    "fixture/DescribeDBLogFiles.yaml"

testDescribeDBClusters :: DescribeDBClusters -> TestTree
testDescribeDBClusters = req
    "DescribeDBClusters"
    "fixture/DescribeDBClusters.yaml"

testModifyDBSubnetGroup :: ModifyDBSubnetGroup -> TestTree
testModifyDBSubnetGroup = req
    "ModifyDBSubnetGroup"
    "fixture/ModifyDBSubnetGroup.yaml"

testListTagsForResource :: ListTagsForResource -> TestTree
testListTagsForResource = req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

testDeleteOptionGroup :: DeleteOptionGroup -> TestTree
testDeleteOptionGroup = req
    "DeleteOptionGroup"
    "fixture/DeleteOptionGroup.yaml"

testDeleteDBCluster :: DeleteDBCluster -> TestTree
testDeleteDBCluster = req
    "DeleteDBCluster"
    "fixture/DeleteDBCluster.yaml"

testDescribeReservedDBInstances :: DescribeReservedDBInstances -> TestTree
testDescribeReservedDBInstances = req
    "DescribeReservedDBInstances"
    "fixture/DescribeReservedDBInstances.yaml"

testCopyDBParameterGroup :: CopyDBParameterGroup -> TestTree
testCopyDBParameterGroup = req
    "CopyDBParameterGroup"
    "fixture/CopyDBParameterGroup.yaml"

testRemoveSourceIdentifierFromSubscription :: RemoveSourceIdentifierFromSubscription -> TestTree
testRemoveSourceIdentifierFromSubscription = req
    "RemoveSourceIdentifierFromSubscription"
    "fixture/RemoveSourceIdentifierFromSubscription.yaml"

testDescribeEngineDefaultClusterParameters :: DescribeEngineDefaultClusterParameters -> TestTree
testDescribeEngineDefaultClusterParameters = req
    "DescribeEngineDefaultClusterParameters"
    "fixture/DescribeEngineDefaultClusterParameters.yaml"

testDescribeDBSnapshotAttributes :: DescribeDBSnapshotAttributes -> TestTree
testDescribeDBSnapshotAttributes = req
    "DescribeDBSnapshotAttributes"
    "fixture/DescribeDBSnapshotAttributes.yaml"

testRemoveTagsFromResource :: RemoveTagsFromResource -> TestTree
testRemoveTagsFromResource = req
    "RemoveTagsFromResource"
    "fixture/RemoveTagsFromResource.yaml"

testRestoreDBInstanceFromDBSnapshot :: RestoreDBInstanceFromDBSnapshot -> TestTree
testRestoreDBInstanceFromDBSnapshot = req
    "RestoreDBInstanceFromDBSnapshot"
    "fixture/RestoreDBInstanceFromDBSnapshot.yaml"

testCreateEventSubscription :: CreateEventSubscription -> TestTree
testCreateEventSubscription = req
    "CreateEventSubscription"
    "fixture/CreateEventSubscription.yaml"

testPurchaseReservedDBInstancesOffering :: PurchaseReservedDBInstancesOffering -> TestTree
testPurchaseReservedDBInstancesOffering = req
    "PurchaseReservedDBInstancesOffering"
    "fixture/PurchaseReservedDBInstancesOffering.yaml"

testCreateDBInstance :: CreateDBInstance -> TestTree
testCreateDBInstance = req
    "CreateDBInstance"
    "fixture/CreateDBInstance.yaml"

testDeleteDBClusterParameterGroup :: DeleteDBClusterParameterGroup -> TestTree
testDeleteDBClusterParameterGroup = req
    "DeleteDBClusterParameterGroup"
    "fixture/DeleteDBClusterParameterGroup.yaml"

testDescribeCertificates :: DescribeCertificates -> TestTree
testDescribeCertificates = req
    "DescribeCertificates"
    "fixture/DescribeCertificates.yaml"

testAuthorizeDBSecurityGroupIngress :: AuthorizeDBSecurityGroupIngress -> TestTree
testAuthorizeDBSecurityGroupIngress = req
    "AuthorizeDBSecurityGroupIngress"
    "fixture/AuthorizeDBSecurityGroupIngress.yaml"

testRestoreDBClusterFromSnapshot :: RestoreDBClusterFromSnapshot -> TestTree
testRestoreDBClusterFromSnapshot = req
    "RestoreDBClusterFromSnapshot"
    "fixture/RestoreDBClusterFromSnapshot.yaml"

testDescribeOrderableDBInstanceOptions :: DescribeOrderableDBInstanceOptions -> TestTree
testDescribeOrderableDBInstanceOptions = req
    "DescribeOrderableDBInstanceOptions"
    "fixture/DescribeOrderableDBInstanceOptions.yaml"

testCreateDBClusterParameterGroup :: CreateDBClusterParameterGroup -> TestTree
testCreateDBClusterParameterGroup = req
    "CreateDBClusterParameterGroup"
    "fixture/CreateDBClusterParameterGroup.yaml"

testCreateDBSnapshot :: CreateDBSnapshot -> TestTree
testCreateDBSnapshot = req
    "CreateDBSnapshot"
    "fixture/CreateDBSnapshot.yaml"

testDeleteEventSubscription :: DeleteEventSubscription -> TestTree
testDeleteEventSubscription = req
    "DeleteEventSubscription"
    "fixture/DeleteEventSubscription.yaml"

testDescribeDBParameterGroups :: DescribeDBParameterGroups -> TestTree
testDescribeDBParameterGroups = req
    "DescribeDBParameterGroups"
    "fixture/DescribeDBParameterGroups.yaml"

testModifyDBSnapshotAttribute :: ModifyDBSnapshotAttribute -> TestTree
testModifyDBSnapshotAttribute = req
    "ModifyDBSnapshotAttribute"
    "fixture/ModifyDBSnapshotAttribute.yaml"

testDeleteDBClusterSnapshot :: DeleteDBClusterSnapshot -> TestTree
testDeleteDBClusterSnapshot = req
    "DeleteDBClusterSnapshot"
    "fixture/DeleteDBClusterSnapshot.yaml"

testDescribeOptionGroupOptions :: DescribeOptionGroupOptions -> TestTree
testDescribeOptionGroupOptions = req
    "DescribeOptionGroupOptions"
    "fixture/DescribeOptionGroupOptions.yaml"

testDescribeEventSubscriptions :: DescribeEventSubscriptions -> TestTree
testDescribeEventSubscriptions = req
    "DescribeEventSubscriptions"
    "fixture/DescribeEventSubscriptions.yaml"

testAddTagsToResource :: AddTagsToResource -> TestTree
testAddTagsToResource = req
    "AddTagsToResource"
    "fixture/AddTagsToResource.yaml"

testDescribeDBParameters :: DescribeDBParameters -> TestTree
testDescribeDBParameters = req
    "DescribeDBParameters"
    "fixture/DescribeDBParameters.yaml"

testCreateDBClusterSnapshot :: CreateDBClusterSnapshot -> TestTree
testCreateDBClusterSnapshot = req
    "CreateDBClusterSnapshot"
    "fixture/CreateDBClusterSnapshot.yaml"

testDescribeDBSnapshots :: DescribeDBSnapshots -> TestTree
testDescribeDBSnapshots = req
    "DescribeDBSnapshots"
    "fixture/DescribeDBSnapshots.yaml"

testDescribeDBSubnetGroups :: DescribeDBSubnetGroups -> TestTree
testDescribeDBSubnetGroups = req
    "DescribeDBSubnetGroups"
    "fixture/DescribeDBSubnetGroups.yaml"

testModifyOptionGroup :: ModifyOptionGroup -> TestTree
testModifyOptionGroup = req
    "ModifyOptionGroup"
    "fixture/ModifyOptionGroup.yaml"

testCreateDBParameterGroup :: CreateDBParameterGroup -> TestTree
testCreateDBParameterGroup = req
    "CreateDBParameterGroup"
    "fixture/CreateDBParameterGroup.yaml"

testModifyDBCluster :: ModifyDBCluster -> TestTree
testModifyDBCluster = req
    "ModifyDBCluster"
    "fixture/ModifyDBCluster.yaml"

testDescribeEventCategories :: DescribeEventCategories -> TestTree
testDescribeEventCategories = req
    "DescribeEventCategories"
    "fixture/DescribeEventCategories.yaml"

testModifyDBClusterParameterGroup :: ModifyDBClusterParameterGroup -> TestTree
testModifyDBClusterParameterGroup = req
    "ModifyDBClusterParameterGroup"
    "fixture/ModifyDBClusterParameterGroup.yaml"

testRestoreDBInstanceToPointInTime :: RestoreDBInstanceToPointInTime -> TestTree
testRestoreDBInstanceToPointInTime = req
    "RestoreDBInstanceToPointInTime"
    "fixture/RestoreDBInstanceToPointInTime.yaml"

testDescribePendingMaintenanceActions :: DescribePendingMaintenanceActions -> TestTree
testDescribePendingMaintenanceActions = req
    "DescribePendingMaintenanceActions"
    "fixture/DescribePendingMaintenanceActions.yaml"

testCopyDBClusterSnapshot :: CopyDBClusterSnapshot -> TestTree
testCopyDBClusterSnapshot = req
    "CopyDBClusterSnapshot"
    "fixture/CopyDBClusterSnapshot.yaml"

testResetDBParameterGroup :: ResetDBParameterGroup -> TestTree
testResetDBParameterGroup = req
    "ResetDBParameterGroup"
    "fixture/ResetDBParameterGroup.yaml"

testCreateDBCluster :: CreateDBCluster -> TestTree
testCreateDBCluster = req
    "CreateDBCluster"
    "fixture/CreateDBCluster.yaml"

testFailoverDBCluster :: FailoverDBCluster -> TestTree
testFailoverDBCluster = req
    "FailoverDBCluster"
    "fixture/FailoverDBCluster.yaml"

testRevokeDBSecurityGroupIngress :: RevokeDBSecurityGroupIngress -> TestTree
testRevokeDBSecurityGroupIngress = req
    "RevokeDBSecurityGroupIngress"
    "fixture/RevokeDBSecurityGroupIngress.yaml"

testModifyDBParameterGroup :: ModifyDBParameterGroup -> TestTree
testModifyDBParameterGroup = req
    "ModifyDBParameterGroup"
    "fixture/ModifyDBParameterGroup.yaml"

testApplyPendingMaintenanceAction :: ApplyPendingMaintenanceAction -> TestTree
testApplyPendingMaintenanceAction = req
    "ApplyPendingMaintenanceAction"
    "fixture/ApplyPendingMaintenanceAction.yaml"

testCreateOptionGroup :: CreateOptionGroup -> TestTree
testCreateOptionGroup = req
    "CreateOptionGroup"
    "fixture/CreateOptionGroup.yaml"

testDescribeAccountAttributes :: DescribeAccountAttributes -> TestTree
testDescribeAccountAttributes = req
    "DescribeAccountAttributes"
    "fixture/DescribeAccountAttributes.yaml"

testDeleteDBSnapshot :: DeleteDBSnapshot -> TestTree
testDeleteDBSnapshot = req
    "DeleteDBSnapshot"
    "fixture/DeleteDBSnapshot.yaml"

testDescribeDBClusterParameters :: DescribeDBClusterParameters -> TestTree
testDescribeDBClusterParameters = req
    "DescribeDBClusterParameters"
    "fixture/DescribeDBClusterParameters.yaml"

testDeleteDBSubnetGroup :: DeleteDBSubnetGroup -> TestTree
testDeleteDBSubnetGroup = req
    "DeleteDBSubnetGroup"
    "fixture/DeleteDBSubnetGroup.yaml"

testCreateDBSecurityGroup :: CreateDBSecurityGroup -> TestTree
testCreateDBSecurityGroup = req
    "CreateDBSecurityGroup"
    "fixture/CreateDBSecurityGroup.yaml"

testDescribeDBClusterSnapshots :: DescribeDBClusterSnapshots -> TestTree
testDescribeDBClusterSnapshots = req
    "DescribeDBClusterSnapshots"
    "fixture/DescribeDBClusterSnapshots.yaml"

testRebootDBInstance :: RebootDBInstance -> TestTree
testRebootDBInstance = req
    "RebootDBInstance"
    "fixture/RebootDBInstance.yaml"

testCreateDBSubnetGroup :: CreateDBSubnetGroup -> TestTree
testCreateDBSubnetGroup = req
    "CreateDBSubnetGroup"
    "fixture/CreateDBSubnetGroup.yaml"

testDescribeReservedDBInstancesOfferings :: DescribeReservedDBInstancesOfferings -> TestTree
testDescribeReservedDBInstancesOfferings = req
    "DescribeReservedDBInstancesOfferings"
    "fixture/DescribeReservedDBInstancesOfferings.yaml"

testDeleteDBSecurityGroup :: DeleteDBSecurityGroup -> TestTree
testDeleteDBSecurityGroup = req
    "DeleteDBSecurityGroup"
    "fixture/DeleteDBSecurityGroup.yaml"

testDeleteDBInstance :: DeleteDBInstance -> TestTree
testDeleteDBInstance = req
    "DeleteDBInstance"
    "fixture/DeleteDBInstance.yaml"

testCreateDBInstanceReadReplica :: CreateDBInstanceReadReplica -> TestTree
testCreateDBInstanceReadReplica = req
    "CreateDBInstanceReadReplica"
    "fixture/CreateDBInstanceReadReplica.yaml"

testDeleteDBParameterGroup :: DeleteDBParameterGroup -> TestTree
testDeleteDBParameterGroup = req
    "DeleteDBParameterGroup"
    "fixture/DeleteDBParameterGroup.yaml"

testDescribeDBSecurityGroups :: DescribeDBSecurityGroups -> TestTree
testDescribeDBSecurityGroups = req
    "DescribeDBSecurityGroups"
    "fixture/DescribeDBSecurityGroups.yaml"

testCopyOptionGroup :: CopyOptionGroup -> TestTree
testCopyOptionGroup = req
    "CopyOptionGroup"
    "fixture/CopyOptionGroup.yaml"

testRestoreDBClusterToPointInTime :: RestoreDBClusterToPointInTime -> TestTree
testRestoreDBClusterToPointInTime = req
    "RestoreDBClusterToPointInTime"
    "fixture/RestoreDBClusterToPointInTime.yaml"

testDescribeDBInstances :: DescribeDBInstances -> TestTree
testDescribeDBInstances = req
    "DescribeDBInstances"
    "fixture/DescribeDBInstances.yaml"

testDownloadDBLogFilePortion :: DownloadDBLogFilePortion -> TestTree
testDownloadDBLogFilePortion = req
    "DownloadDBLogFilePortion"
    "fixture/DownloadDBLogFilePortion.yaml"

-- Responses

testDescribeDBClusterParameterGroupsResponse :: DescribeDBClusterParameterGroupsResponse -> TestTree
testDescribeDBClusterParameterGroupsResponse = res
    "DescribeDBClusterParameterGroupsResponse"
    "fixture/DescribeDBClusterParameterGroupsResponse.proto"
    rDS
    (Proxy :: Proxy DescribeDBClusterParameterGroups)

testPromoteReadReplicaResponse :: PromoteReadReplicaResponse -> TestTree
testPromoteReadReplicaResponse = res
    "PromoteReadReplicaResponse"
    "fixture/PromoteReadReplicaResponse.proto"
    rDS
    (Proxy :: Proxy PromoteReadReplica)

testDescribeDBEngineVersionsResponse :: DescribeDBEngineVersionsResponse -> TestTree
testDescribeDBEngineVersionsResponse = res
    "DescribeDBEngineVersionsResponse"
    "fixture/DescribeDBEngineVersionsResponse.proto"
    rDS
    (Proxy :: Proxy DescribeDBEngineVersions)

testCopyDBSnapshotResponse :: CopyDBSnapshotResponse -> TestTree
testCopyDBSnapshotResponse = res
    "CopyDBSnapshotResponse"
    "fixture/CopyDBSnapshotResponse.proto"
    rDS
    (Proxy :: Proxy CopyDBSnapshot)

testAddSourceIdentifierToSubscriptionResponse :: AddSourceIdentifierToSubscriptionResponse -> TestTree
testAddSourceIdentifierToSubscriptionResponse = res
    "AddSourceIdentifierToSubscriptionResponse"
    "fixture/AddSourceIdentifierToSubscriptionResponse.proto"
    rDS
    (Proxy :: Proxy AddSourceIdentifierToSubscription)

testModifyDBInstanceResponse :: ModifyDBInstanceResponse -> TestTree
testModifyDBInstanceResponse = res
    "ModifyDBInstanceResponse"
    "fixture/ModifyDBInstanceResponse.proto"
    rDS
    (Proxy :: Proxy ModifyDBInstance)

testModifyEventSubscriptionResponse :: ModifyEventSubscriptionResponse -> TestTree
testModifyEventSubscriptionResponse = res
    "ModifyEventSubscriptionResponse"
    "fixture/ModifyEventSubscriptionResponse.proto"
    rDS
    (Proxy :: Proxy ModifyEventSubscription)

testResetDBClusterParameterGroupResponse :: DBClusterParameterGroupNameMessage -> TestTree
testResetDBClusterParameterGroupResponse = res
    "ResetDBClusterParameterGroupResponse"
    "fixture/ResetDBClusterParameterGroupResponse.proto"
    rDS
    (Proxy :: Proxy ResetDBClusterParameterGroup)

testDescribeEventsResponse :: DescribeEventsResponse -> TestTree
testDescribeEventsResponse = res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse.proto"
    rDS
    (Proxy :: Proxy DescribeEvents)

testDescribeEngineDefaultParametersResponse :: DescribeEngineDefaultParametersResponse -> TestTree
testDescribeEngineDefaultParametersResponse = res
    "DescribeEngineDefaultParametersResponse"
    "fixture/DescribeEngineDefaultParametersResponse.proto"
    rDS
    (Proxy :: Proxy DescribeEngineDefaultParameters)

testDescribeOptionGroupsResponse :: DescribeOptionGroupsResponse -> TestTree
testDescribeOptionGroupsResponse = res
    "DescribeOptionGroupsResponse"
    "fixture/DescribeOptionGroupsResponse.proto"
    rDS
    (Proxy :: Proxy DescribeOptionGroups)

testDescribeDBLogFilesResponse :: DescribeDBLogFilesResponse -> TestTree
testDescribeDBLogFilesResponse = res
    "DescribeDBLogFilesResponse"
    "fixture/DescribeDBLogFilesResponse.proto"
    rDS
    (Proxy :: Proxy DescribeDBLogFiles)

testDescribeDBClustersResponse :: DescribeDBClustersResponse -> TestTree
testDescribeDBClustersResponse = res
    "DescribeDBClustersResponse"
    "fixture/DescribeDBClustersResponse.proto"
    rDS
    (Proxy :: Proxy DescribeDBClusters)

testModifyDBSubnetGroupResponse :: ModifyDBSubnetGroupResponse -> TestTree
testModifyDBSubnetGroupResponse = res
    "ModifyDBSubnetGroupResponse"
    "fixture/ModifyDBSubnetGroupResponse.proto"
    rDS
    (Proxy :: Proxy ModifyDBSubnetGroup)

testListTagsForResourceResponse :: ListTagsForResourceResponse -> TestTree
testListTagsForResourceResponse = res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    rDS
    (Proxy :: Proxy ListTagsForResource)

testDeleteOptionGroupResponse :: DeleteOptionGroupResponse -> TestTree
testDeleteOptionGroupResponse = res
    "DeleteOptionGroupResponse"
    "fixture/DeleteOptionGroupResponse.proto"
    rDS
    (Proxy :: Proxy DeleteOptionGroup)

testDeleteDBClusterResponse :: DeleteDBClusterResponse -> TestTree
testDeleteDBClusterResponse = res
    "DeleteDBClusterResponse"
    "fixture/DeleteDBClusterResponse.proto"
    rDS
    (Proxy :: Proxy DeleteDBCluster)

testDescribeReservedDBInstancesResponse :: DescribeReservedDBInstancesResponse -> TestTree
testDescribeReservedDBInstancesResponse = res
    "DescribeReservedDBInstancesResponse"
    "fixture/DescribeReservedDBInstancesResponse.proto"
    rDS
    (Proxy :: Proxy DescribeReservedDBInstances)

testCopyDBParameterGroupResponse :: CopyDBParameterGroupResponse -> TestTree
testCopyDBParameterGroupResponse = res
    "CopyDBParameterGroupResponse"
    "fixture/CopyDBParameterGroupResponse.proto"
    rDS
    (Proxy :: Proxy CopyDBParameterGroup)

testRemoveSourceIdentifierFromSubscriptionResponse :: RemoveSourceIdentifierFromSubscriptionResponse -> TestTree
testRemoveSourceIdentifierFromSubscriptionResponse = res
    "RemoveSourceIdentifierFromSubscriptionResponse"
    "fixture/RemoveSourceIdentifierFromSubscriptionResponse.proto"
    rDS
    (Proxy :: Proxy RemoveSourceIdentifierFromSubscription)

testDescribeEngineDefaultClusterParametersResponse :: DescribeEngineDefaultClusterParametersResponse -> TestTree
testDescribeEngineDefaultClusterParametersResponse = res
    "DescribeEngineDefaultClusterParametersResponse"
    "fixture/DescribeEngineDefaultClusterParametersResponse.proto"
    rDS
    (Proxy :: Proxy DescribeEngineDefaultClusterParameters)

testDescribeDBSnapshotAttributesResponse :: DescribeDBSnapshotAttributesResponse -> TestTree
testDescribeDBSnapshotAttributesResponse = res
    "DescribeDBSnapshotAttributesResponse"
    "fixture/DescribeDBSnapshotAttributesResponse.proto"
    rDS
    (Proxy :: Proxy DescribeDBSnapshotAttributes)

testRemoveTagsFromResourceResponse :: RemoveTagsFromResourceResponse -> TestTree
testRemoveTagsFromResourceResponse = res
    "RemoveTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse.proto"
    rDS
    (Proxy :: Proxy RemoveTagsFromResource)

testRestoreDBInstanceFromDBSnapshotResponse :: RestoreDBInstanceFromDBSnapshotResponse -> TestTree
testRestoreDBInstanceFromDBSnapshotResponse = res
    "RestoreDBInstanceFromDBSnapshotResponse"
    "fixture/RestoreDBInstanceFromDBSnapshotResponse.proto"
    rDS
    (Proxy :: Proxy RestoreDBInstanceFromDBSnapshot)

testCreateEventSubscriptionResponse :: CreateEventSubscriptionResponse -> TestTree
testCreateEventSubscriptionResponse = res
    "CreateEventSubscriptionResponse"
    "fixture/CreateEventSubscriptionResponse.proto"
    rDS
    (Proxy :: Proxy CreateEventSubscription)

testPurchaseReservedDBInstancesOfferingResponse :: PurchaseReservedDBInstancesOfferingResponse -> TestTree
testPurchaseReservedDBInstancesOfferingResponse = res
    "PurchaseReservedDBInstancesOfferingResponse"
    "fixture/PurchaseReservedDBInstancesOfferingResponse.proto"
    rDS
    (Proxy :: Proxy PurchaseReservedDBInstancesOffering)

testCreateDBInstanceResponse :: CreateDBInstanceResponse -> TestTree
testCreateDBInstanceResponse = res
    "CreateDBInstanceResponse"
    "fixture/CreateDBInstanceResponse.proto"
    rDS
    (Proxy :: Proxy CreateDBInstance)

testDeleteDBClusterParameterGroupResponse :: DeleteDBClusterParameterGroupResponse -> TestTree
testDeleteDBClusterParameterGroupResponse = res
    "DeleteDBClusterParameterGroupResponse"
    "fixture/DeleteDBClusterParameterGroupResponse.proto"
    rDS
    (Proxy :: Proxy DeleteDBClusterParameterGroup)

testDescribeCertificatesResponse :: DescribeCertificatesResponse -> TestTree
testDescribeCertificatesResponse = res
    "DescribeCertificatesResponse"
    "fixture/DescribeCertificatesResponse.proto"
    rDS
    (Proxy :: Proxy DescribeCertificates)

testAuthorizeDBSecurityGroupIngressResponse :: AuthorizeDBSecurityGroupIngressResponse -> TestTree
testAuthorizeDBSecurityGroupIngressResponse = res
    "AuthorizeDBSecurityGroupIngressResponse"
    "fixture/AuthorizeDBSecurityGroupIngressResponse.proto"
    rDS
    (Proxy :: Proxy AuthorizeDBSecurityGroupIngress)

testRestoreDBClusterFromSnapshotResponse :: RestoreDBClusterFromSnapshotResponse -> TestTree
testRestoreDBClusterFromSnapshotResponse = res
    "RestoreDBClusterFromSnapshotResponse"
    "fixture/RestoreDBClusterFromSnapshotResponse.proto"
    rDS
    (Proxy :: Proxy RestoreDBClusterFromSnapshot)

testDescribeOrderableDBInstanceOptionsResponse :: DescribeOrderableDBInstanceOptionsResponse -> TestTree
testDescribeOrderableDBInstanceOptionsResponse = res
    "DescribeOrderableDBInstanceOptionsResponse"
    "fixture/DescribeOrderableDBInstanceOptionsResponse.proto"
    rDS
    (Proxy :: Proxy DescribeOrderableDBInstanceOptions)

testCreateDBClusterParameterGroupResponse :: CreateDBClusterParameterGroupResponse -> TestTree
testCreateDBClusterParameterGroupResponse = res
    "CreateDBClusterParameterGroupResponse"
    "fixture/CreateDBClusterParameterGroupResponse.proto"
    rDS
    (Proxy :: Proxy CreateDBClusterParameterGroup)

testCreateDBSnapshotResponse :: CreateDBSnapshotResponse -> TestTree
testCreateDBSnapshotResponse = res
    "CreateDBSnapshotResponse"
    "fixture/CreateDBSnapshotResponse.proto"
    rDS
    (Proxy :: Proxy CreateDBSnapshot)

testDeleteEventSubscriptionResponse :: DeleteEventSubscriptionResponse -> TestTree
testDeleteEventSubscriptionResponse = res
    "DeleteEventSubscriptionResponse"
    "fixture/DeleteEventSubscriptionResponse.proto"
    rDS
    (Proxy :: Proxy DeleteEventSubscription)

testDescribeDBParameterGroupsResponse :: DescribeDBParameterGroupsResponse -> TestTree
testDescribeDBParameterGroupsResponse = res
    "DescribeDBParameterGroupsResponse"
    "fixture/DescribeDBParameterGroupsResponse.proto"
    rDS
    (Proxy :: Proxy DescribeDBParameterGroups)

testModifyDBSnapshotAttributeResponse :: ModifyDBSnapshotAttributeResponse -> TestTree
testModifyDBSnapshotAttributeResponse = res
    "ModifyDBSnapshotAttributeResponse"
    "fixture/ModifyDBSnapshotAttributeResponse.proto"
    rDS
    (Proxy :: Proxy ModifyDBSnapshotAttribute)

testDeleteDBClusterSnapshotResponse :: DeleteDBClusterSnapshotResponse -> TestTree
testDeleteDBClusterSnapshotResponse = res
    "DeleteDBClusterSnapshotResponse"
    "fixture/DeleteDBClusterSnapshotResponse.proto"
    rDS
    (Proxy :: Proxy DeleteDBClusterSnapshot)

testDescribeOptionGroupOptionsResponse :: DescribeOptionGroupOptionsResponse -> TestTree
testDescribeOptionGroupOptionsResponse = res
    "DescribeOptionGroupOptionsResponse"
    "fixture/DescribeOptionGroupOptionsResponse.proto"
    rDS
    (Proxy :: Proxy DescribeOptionGroupOptions)

testDescribeEventSubscriptionsResponse :: DescribeEventSubscriptionsResponse -> TestTree
testDescribeEventSubscriptionsResponse = res
    "DescribeEventSubscriptionsResponse"
    "fixture/DescribeEventSubscriptionsResponse.proto"
    rDS
    (Proxy :: Proxy DescribeEventSubscriptions)

testAddTagsToResourceResponse :: AddTagsToResourceResponse -> TestTree
testAddTagsToResourceResponse = res
    "AddTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse.proto"
    rDS
    (Proxy :: Proxy AddTagsToResource)

testDescribeDBParametersResponse :: DescribeDBParametersResponse -> TestTree
testDescribeDBParametersResponse = res
    "DescribeDBParametersResponse"
    "fixture/DescribeDBParametersResponse.proto"
    rDS
    (Proxy :: Proxy DescribeDBParameters)

testCreateDBClusterSnapshotResponse :: CreateDBClusterSnapshotResponse -> TestTree
testCreateDBClusterSnapshotResponse = res
    "CreateDBClusterSnapshotResponse"
    "fixture/CreateDBClusterSnapshotResponse.proto"
    rDS
    (Proxy :: Proxy CreateDBClusterSnapshot)

testDescribeDBSnapshotsResponse :: DescribeDBSnapshotsResponse -> TestTree
testDescribeDBSnapshotsResponse = res
    "DescribeDBSnapshotsResponse"
    "fixture/DescribeDBSnapshotsResponse.proto"
    rDS
    (Proxy :: Proxy DescribeDBSnapshots)

testDescribeDBSubnetGroupsResponse :: DescribeDBSubnetGroupsResponse -> TestTree
testDescribeDBSubnetGroupsResponse = res
    "DescribeDBSubnetGroupsResponse"
    "fixture/DescribeDBSubnetGroupsResponse.proto"
    rDS
    (Proxy :: Proxy DescribeDBSubnetGroups)

testModifyOptionGroupResponse :: ModifyOptionGroupResponse -> TestTree
testModifyOptionGroupResponse = res
    "ModifyOptionGroupResponse"
    "fixture/ModifyOptionGroupResponse.proto"
    rDS
    (Proxy :: Proxy ModifyOptionGroup)

testCreateDBParameterGroupResponse :: CreateDBParameterGroupResponse -> TestTree
testCreateDBParameterGroupResponse = res
    "CreateDBParameterGroupResponse"
    "fixture/CreateDBParameterGroupResponse.proto"
    rDS
    (Proxy :: Proxy CreateDBParameterGroup)

testModifyDBClusterResponse :: ModifyDBClusterResponse -> TestTree
testModifyDBClusterResponse = res
    "ModifyDBClusterResponse"
    "fixture/ModifyDBClusterResponse.proto"
    rDS
    (Proxy :: Proxy ModifyDBCluster)

testDescribeEventCategoriesResponse :: DescribeEventCategoriesResponse -> TestTree
testDescribeEventCategoriesResponse = res
    "DescribeEventCategoriesResponse"
    "fixture/DescribeEventCategoriesResponse.proto"
    rDS
    (Proxy :: Proxy DescribeEventCategories)

testModifyDBClusterParameterGroupResponse :: DBClusterParameterGroupNameMessage -> TestTree
testModifyDBClusterParameterGroupResponse = res
    "ModifyDBClusterParameterGroupResponse"
    "fixture/ModifyDBClusterParameterGroupResponse.proto"
    rDS
    (Proxy :: Proxy ModifyDBClusterParameterGroup)

testRestoreDBInstanceToPointInTimeResponse :: RestoreDBInstanceToPointInTimeResponse -> TestTree
testRestoreDBInstanceToPointInTimeResponse = res
    "RestoreDBInstanceToPointInTimeResponse"
    "fixture/RestoreDBInstanceToPointInTimeResponse.proto"
    rDS
    (Proxy :: Proxy RestoreDBInstanceToPointInTime)

testDescribePendingMaintenanceActionsResponse :: DescribePendingMaintenanceActionsResponse -> TestTree
testDescribePendingMaintenanceActionsResponse = res
    "DescribePendingMaintenanceActionsResponse"
    "fixture/DescribePendingMaintenanceActionsResponse.proto"
    rDS
    (Proxy :: Proxy DescribePendingMaintenanceActions)

testCopyDBClusterSnapshotResponse :: CopyDBClusterSnapshotResponse -> TestTree
testCopyDBClusterSnapshotResponse = res
    "CopyDBClusterSnapshotResponse"
    "fixture/CopyDBClusterSnapshotResponse.proto"
    rDS
    (Proxy :: Proxy CopyDBClusterSnapshot)

testResetDBParameterGroupResponse :: DBParameterGroupNameMessage -> TestTree
testResetDBParameterGroupResponse = res
    "ResetDBParameterGroupResponse"
    "fixture/ResetDBParameterGroupResponse.proto"
    rDS
    (Proxy :: Proxy ResetDBParameterGroup)

testCreateDBClusterResponse :: CreateDBClusterResponse -> TestTree
testCreateDBClusterResponse = res
    "CreateDBClusterResponse"
    "fixture/CreateDBClusterResponse.proto"
    rDS
    (Proxy :: Proxy CreateDBCluster)

testFailoverDBClusterResponse :: FailoverDBClusterResponse -> TestTree
testFailoverDBClusterResponse = res
    "FailoverDBClusterResponse"
    "fixture/FailoverDBClusterResponse.proto"
    rDS
    (Proxy :: Proxy FailoverDBCluster)

testRevokeDBSecurityGroupIngressResponse :: RevokeDBSecurityGroupIngressResponse -> TestTree
testRevokeDBSecurityGroupIngressResponse = res
    "RevokeDBSecurityGroupIngressResponse"
    "fixture/RevokeDBSecurityGroupIngressResponse.proto"
    rDS
    (Proxy :: Proxy RevokeDBSecurityGroupIngress)

testModifyDBParameterGroupResponse :: DBParameterGroupNameMessage -> TestTree
testModifyDBParameterGroupResponse = res
    "ModifyDBParameterGroupResponse"
    "fixture/ModifyDBParameterGroupResponse.proto"
    rDS
    (Proxy :: Proxy ModifyDBParameterGroup)

testApplyPendingMaintenanceActionResponse :: ApplyPendingMaintenanceActionResponse -> TestTree
testApplyPendingMaintenanceActionResponse = res
    "ApplyPendingMaintenanceActionResponse"
    "fixture/ApplyPendingMaintenanceActionResponse.proto"
    rDS
    (Proxy :: Proxy ApplyPendingMaintenanceAction)

testCreateOptionGroupResponse :: CreateOptionGroupResponse -> TestTree
testCreateOptionGroupResponse = res
    "CreateOptionGroupResponse"
    "fixture/CreateOptionGroupResponse.proto"
    rDS
    (Proxy :: Proxy CreateOptionGroup)

testDescribeAccountAttributesResponse :: DescribeAccountAttributesResponse -> TestTree
testDescribeAccountAttributesResponse = res
    "DescribeAccountAttributesResponse"
    "fixture/DescribeAccountAttributesResponse.proto"
    rDS
    (Proxy :: Proxy DescribeAccountAttributes)

testDeleteDBSnapshotResponse :: DeleteDBSnapshotResponse -> TestTree
testDeleteDBSnapshotResponse = res
    "DeleteDBSnapshotResponse"
    "fixture/DeleteDBSnapshotResponse.proto"
    rDS
    (Proxy :: Proxy DeleteDBSnapshot)

testDescribeDBClusterParametersResponse :: DescribeDBClusterParametersResponse -> TestTree
testDescribeDBClusterParametersResponse = res
    "DescribeDBClusterParametersResponse"
    "fixture/DescribeDBClusterParametersResponse.proto"
    rDS
    (Proxy :: Proxy DescribeDBClusterParameters)

testDeleteDBSubnetGroupResponse :: DeleteDBSubnetGroupResponse -> TestTree
testDeleteDBSubnetGroupResponse = res
    "DeleteDBSubnetGroupResponse"
    "fixture/DeleteDBSubnetGroupResponse.proto"
    rDS
    (Proxy :: Proxy DeleteDBSubnetGroup)

testCreateDBSecurityGroupResponse :: CreateDBSecurityGroupResponse -> TestTree
testCreateDBSecurityGroupResponse = res
    "CreateDBSecurityGroupResponse"
    "fixture/CreateDBSecurityGroupResponse.proto"
    rDS
    (Proxy :: Proxy CreateDBSecurityGroup)

testDescribeDBClusterSnapshotsResponse :: DescribeDBClusterSnapshotsResponse -> TestTree
testDescribeDBClusterSnapshotsResponse = res
    "DescribeDBClusterSnapshotsResponse"
    "fixture/DescribeDBClusterSnapshotsResponse.proto"
    rDS
    (Proxy :: Proxy DescribeDBClusterSnapshots)

testRebootDBInstanceResponse :: RebootDBInstanceResponse -> TestTree
testRebootDBInstanceResponse = res
    "RebootDBInstanceResponse"
    "fixture/RebootDBInstanceResponse.proto"
    rDS
    (Proxy :: Proxy RebootDBInstance)

testCreateDBSubnetGroupResponse :: CreateDBSubnetGroupResponse -> TestTree
testCreateDBSubnetGroupResponse = res
    "CreateDBSubnetGroupResponse"
    "fixture/CreateDBSubnetGroupResponse.proto"
    rDS
    (Proxy :: Proxy CreateDBSubnetGroup)

testDescribeReservedDBInstancesOfferingsResponse :: DescribeReservedDBInstancesOfferingsResponse -> TestTree
testDescribeReservedDBInstancesOfferingsResponse = res
    "DescribeReservedDBInstancesOfferingsResponse"
    "fixture/DescribeReservedDBInstancesOfferingsResponse.proto"
    rDS
    (Proxy :: Proxy DescribeReservedDBInstancesOfferings)

testDeleteDBSecurityGroupResponse :: DeleteDBSecurityGroupResponse -> TestTree
testDeleteDBSecurityGroupResponse = res
    "DeleteDBSecurityGroupResponse"
    "fixture/DeleteDBSecurityGroupResponse.proto"
    rDS
    (Proxy :: Proxy DeleteDBSecurityGroup)

testDeleteDBInstanceResponse :: DeleteDBInstanceResponse -> TestTree
testDeleteDBInstanceResponse = res
    "DeleteDBInstanceResponse"
    "fixture/DeleteDBInstanceResponse.proto"
    rDS
    (Proxy :: Proxy DeleteDBInstance)

testCreateDBInstanceReadReplicaResponse :: CreateDBInstanceReadReplicaResponse -> TestTree
testCreateDBInstanceReadReplicaResponse = res
    "CreateDBInstanceReadReplicaResponse"
    "fixture/CreateDBInstanceReadReplicaResponse.proto"
    rDS
    (Proxy :: Proxy CreateDBInstanceReadReplica)

testDeleteDBParameterGroupResponse :: DeleteDBParameterGroupResponse -> TestTree
testDeleteDBParameterGroupResponse = res
    "DeleteDBParameterGroupResponse"
    "fixture/DeleteDBParameterGroupResponse.proto"
    rDS
    (Proxy :: Proxy DeleteDBParameterGroup)

testDescribeDBSecurityGroupsResponse :: DescribeDBSecurityGroupsResponse -> TestTree
testDescribeDBSecurityGroupsResponse = res
    "DescribeDBSecurityGroupsResponse"
    "fixture/DescribeDBSecurityGroupsResponse.proto"
    rDS
    (Proxy :: Proxy DescribeDBSecurityGroups)

testCopyOptionGroupResponse :: CopyOptionGroupResponse -> TestTree
testCopyOptionGroupResponse = res
    "CopyOptionGroupResponse"
    "fixture/CopyOptionGroupResponse.proto"
    rDS
    (Proxy :: Proxy CopyOptionGroup)

testRestoreDBClusterToPointInTimeResponse :: RestoreDBClusterToPointInTimeResponse -> TestTree
testRestoreDBClusterToPointInTimeResponse = res
    "RestoreDBClusterToPointInTimeResponse"
    "fixture/RestoreDBClusterToPointInTimeResponse.proto"
    rDS
    (Proxy :: Proxy RestoreDBClusterToPointInTime)

testDescribeDBInstancesResponse :: DescribeDBInstancesResponse -> TestTree
testDescribeDBInstancesResponse = res
    "DescribeDBInstancesResponse"
    "fixture/DescribeDBInstancesResponse.proto"
    rDS
    (Proxy :: Proxy DescribeDBInstances)

testDownloadDBLogFilePortionResponse :: DownloadDBLogFilePortionResponse -> TestTree
testDownloadDBLogFilePortionResponse = res
    "DownloadDBLogFilePortionResponse"
    "fixture/DownloadDBLogFilePortionResponse.proto"
    rDS
    (Proxy :: Proxy DownloadDBLogFilePortion)
