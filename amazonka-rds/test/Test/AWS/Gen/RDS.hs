{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.RDS
-- Copyright   : (c) 2013-2015 Brendan Hay
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
--         [ testDescribeDBEngineVersions $
--             describeDBEngineVersions
--
--         , testDescribeDBClusterParameterGroups $
--             describeDBClusterParameterGroups
--
--         , testPromoteReadReplica $
--             promoteReadReplica
--
--         , testModifyEventSubscription $
--             modifyEventSubscription
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
--         , testResetDBClusterParameterGroup $
--             resetDBClusterParameterGroup
--
--         , testDescribeEvents $
--             describeEvents
--
--         , testDescribeEngineDefaultParameters $
--             describeEngineDefaultParameters
--
--         , testDescribeDBClusters $
--             describeDBClusters
--
--         , testModifyDBSubnetGroup $
--             modifyDBSubnetGroup
--
--         , testDescribeDBLogFiles $
--             describeDBLogFiles
--
--         , testListTagsForResource $
--             listTagsForResource
--
--         , testDescribeOptionGroups $
--             describeOptionGroups
--
--         , testDeleteDBCluster $
--             deleteDBCluster
--
--         , testRemoveSourceIdentifierFromSubscription $
--             removeSourceIdentifierFromSubscription
--
--         , testCopyDBParameterGroup $
--             copyDBParameterGroup
--
--         , testDescribeReservedDBInstances $
--             describeReservedDBInstances
--
--         , testDeleteOptionGroup $
--             deleteOptionGroup
--
--         , testDescribeEngineDefaultClusterParameters $
--             describeEngineDefaultClusterParameters
--
--         , testCreateEventSubscription $
--             createEventSubscription
--
--         , testRemoveTagsFromResource $
--             removeTagsFromResource
--
--         , testCreateDBInstance $
--             createDBInstance
--
--         , testRestoreDBInstanceFromDBSnapshot $
--             restoreDBInstanceFromDBSnapshot
--
--         , testAuthorizeDBSecurityGroupIngress $
--             authorizeDBSecurityGroupIngress
--
--         , testDeleteDBClusterParameterGroup $
--             deleteDBClusterParameterGroup
--
--         , testPurchaseReservedDBInstancesOffering $
--             purchaseReservedDBInstancesOffering
--
--         , testDescribeCertificates $
--             describeCertificates
--
--         , testRestoreDBClusterFromSnapshot $
--             restoreDBClusterFromSnapshot
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
--         , testDescribeOrderableDBInstanceOptions $
--             describeOrderableDBInstanceOptions
--
--         , testCreateDBClusterParameterGroup $
--             createDBClusterParameterGroup
--
--         , testDescribeEventSubscriptions $
--             describeEventSubscriptions
--
--         , testAddTagsToResource $
--             addTagsToResource
--
--         , testDescribeOptionGroupOptions $
--             describeOptionGroupOptions
--
--         , testDescribeDBParameters $
--             describeDBParameters
--
--         , testDeleteDBClusterSnapshot $
--             deleteDBClusterSnapshot
--
--         , testDescribeDBSnapshots $
--             describeDBSnapshots
--
--         , testDescribeDBSubnetGroups $
--             describeDBSubnetGroups
--
--         , testCreateDBParameterGroup $
--             createDBParameterGroup
--
--         , testCreateDBClusterSnapshot $
--             createDBClusterSnapshot
--
--         , testModifyOptionGroup $
--             modifyOptionGroup
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
--         , testDescribePendingMaintenanceActions $
--             describePendingMaintenanceActions
--
--         , testRestoreDBInstanceToPointInTime $
--             restoreDBInstanceToPointInTime
--
--         , testResetDBParameterGroup $
--             resetDBParameterGroup
--
--         , testCopyDBClusterSnapshot $
--             copyDBClusterSnapshot
--
--         , testModifyDBParameterGroup $
--             modifyDBParameterGroup
--
--         , testFailoverDBCluster $
--             failoverDBCluster
--
--         , testCreateDBCluster $
--             createDBCluster
--
--         , testCreateOptionGroup $
--             createOptionGroup
--
--         , testApplyPendingMaintenanceAction $
--             applyPendingMaintenanceAction
--
--         , testRevokeDBSecurityGroupIngress $
--             revokeDBSecurityGroupIngress
--
--         , testDeleteDBSnapshot $
--             deleteDBSnapshot
--
--         , testDescribeDBClusterParameters $
--             describeDBClusterParameters
--
--         , testCreateDBSecurityGroup $
--             createDBSecurityGroup
--
--         , testDeleteDBSubnetGroup $
--             deleteDBSubnetGroup
--
--         , testDescribeAccountAttributes $
--             describeAccountAttributes
--
--         , testDeleteDBSecurityGroup $
--             deleteDBSecurityGroup
--
--         , testRebootDBInstance $
--             rebootDBInstance
--
--         , testDescribeDBClusterSnapshots $
--             describeDBClusterSnapshots
--
--         , testCreateDBSubnetGroup $
--             createDBSubnetGroup
--
--         , testDescribeReservedDBInstancesOfferings $
--             describeReservedDBInstancesOfferings
--
--         , testDeleteDBInstance $
--             deleteDBInstance
--
--         , testDescribeDBInstances $
--             describeDBInstances
--
--         , testCopyOptionGroup $
--             copyOptionGroup
--
--         , testDownloadDBLogFilePortion $
--             downloadDBLogFilePortion
--
--         , testCreateDBInstanceReadReplica $
--             createDBInstanceReadReplica
--
--         , testRestoreDBClusterToPointInTime $
--             restoreDBClusterToPointInTime
--
--         , testDeleteDBParameterGroup $
--             deleteDBParameterGroup
--
--         , testDescribeDBSecurityGroups $
--             describeDBSecurityGroups
--
--           ]

--     , testGroup "response"
--         [ testDescribeDBEngineVersionsResponse $
--             describeDBEngineVersionsResponse
--
--         , testDescribeDBClusterParameterGroupsResponse $
--             describeDBClusterParameterGroupsResponse
--
--         , testPromoteReadReplicaResponse $
--             promoteReadReplicaResponse
--
--         , testModifyEventSubscriptionResponse $
--             modifyEventSubscriptionResponse
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
--         , testResetDBClusterParameterGroupResponse $
--             dbClusterParameterGroupNameMessage
--
--         , testDescribeEventsResponse $
--             describeEventsResponse
--
--         , testDescribeEngineDefaultParametersResponse $
--             describeEngineDefaultParametersResponse
--
--         , testDescribeDBClustersResponse $
--             describeDBClustersResponse
--
--         , testModifyDBSubnetGroupResponse $
--             modifyDBSubnetGroupResponse
--
--         , testDescribeDBLogFilesResponse $
--             describeDBLogFilesResponse
--
--         , testListTagsForResourceResponse $
--             listTagsForResourceResponse
--
--         , testDescribeOptionGroupsResponse $
--             describeOptionGroupsResponse
--
--         , testDeleteDBClusterResponse $
--             deleteDBClusterResponse
--
--         , testRemoveSourceIdentifierFromSubscriptionResponse $
--             removeSourceIdentifierFromSubscriptionResponse
--
--         , testCopyDBParameterGroupResponse $
--             copyDBParameterGroupResponse
--
--         , testDescribeReservedDBInstancesResponse $
--             describeReservedDBInstancesResponse
--
--         , testDeleteOptionGroupResponse $
--             deleteOptionGroupResponse
--
--         , testDescribeEngineDefaultClusterParametersResponse $
--             describeEngineDefaultClusterParametersResponse
--
--         , testCreateEventSubscriptionResponse $
--             createEventSubscriptionResponse
--
--         , testRemoveTagsFromResourceResponse $
--             removeTagsFromResourceResponse
--
--         , testCreateDBInstanceResponse $
--             createDBInstanceResponse
--
--         , testRestoreDBInstanceFromDBSnapshotResponse $
--             restoreDBInstanceFromDBSnapshotResponse
--
--         , testAuthorizeDBSecurityGroupIngressResponse $
--             authorizeDBSecurityGroupIngressResponse
--
--         , testDeleteDBClusterParameterGroupResponse $
--             deleteDBClusterParameterGroupResponse
--
--         , testPurchaseReservedDBInstancesOfferingResponse $
--             purchaseReservedDBInstancesOfferingResponse
--
--         , testDescribeCertificatesResponse $
--             describeCertificatesResponse
--
--         , testRestoreDBClusterFromSnapshotResponse $
--             restoreDBClusterFromSnapshotResponse
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
--         , testDescribeOrderableDBInstanceOptionsResponse $
--             describeOrderableDBInstanceOptionsResponse
--
--         , testCreateDBClusterParameterGroupResponse $
--             createDBClusterParameterGroupResponse
--
--         , testDescribeEventSubscriptionsResponse $
--             describeEventSubscriptionsResponse
--
--         , testAddTagsToResourceResponse $
--             addTagsToResourceResponse
--
--         , testDescribeOptionGroupOptionsResponse $
--             describeOptionGroupOptionsResponse
--
--         , testDescribeDBParametersResponse $
--             describeDBParametersResponse
--
--         , testDeleteDBClusterSnapshotResponse $
--             deleteDBClusterSnapshotResponse
--
--         , testDescribeDBSnapshotsResponse $
--             describeDBSnapshotsResponse
--
--         , testDescribeDBSubnetGroupsResponse $
--             describeDBSubnetGroupsResponse
--
--         , testCreateDBParameterGroupResponse $
--             createDBParameterGroupResponse
--
--         , testCreateDBClusterSnapshotResponse $
--             createDBClusterSnapshotResponse
--
--         , testModifyOptionGroupResponse $
--             modifyOptionGroupResponse
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
--         , testDescribePendingMaintenanceActionsResponse $
--             describePendingMaintenanceActionsResponse
--
--         , testRestoreDBInstanceToPointInTimeResponse $
--             restoreDBInstanceToPointInTimeResponse
--
--         , testResetDBParameterGroupResponse $
--             dbParameterGroupNameMessage
--
--         , testCopyDBClusterSnapshotResponse $
--             copyDBClusterSnapshotResponse
--
--         , testModifyDBParameterGroupResponse $
--             dbParameterGroupNameMessage
--
--         , testFailoverDBClusterResponse $
--             failoverDBClusterResponse
--
--         , testCreateDBClusterResponse $
--             createDBClusterResponse
--
--         , testCreateOptionGroupResponse $
--             createOptionGroupResponse
--
--         , testApplyPendingMaintenanceActionResponse $
--             applyPendingMaintenanceActionResponse
--
--         , testRevokeDBSecurityGroupIngressResponse $
--             revokeDBSecurityGroupIngressResponse
--
--         , testDeleteDBSnapshotResponse $
--             deleteDBSnapshotResponse
--
--         , testDescribeDBClusterParametersResponse $
--             describeDBClusterParametersResponse
--
--         , testCreateDBSecurityGroupResponse $
--             createDBSecurityGroupResponse
--
--         , testDeleteDBSubnetGroupResponse $
--             deleteDBSubnetGroupResponse
--
--         , testDescribeAccountAttributesResponse $
--             describeAccountAttributesResponse
--
--         , testDeleteDBSecurityGroupResponse $
--             deleteDBSecurityGroupResponse
--
--         , testRebootDBInstanceResponse $
--             rebootDBInstanceResponse
--
--         , testDescribeDBClusterSnapshotsResponse $
--             describeDBClusterSnapshotsResponse
--
--         , testCreateDBSubnetGroupResponse $
--             createDBSubnetGroupResponse
--
--         , testDescribeReservedDBInstancesOfferingsResponse $
--             describeReservedDBInstancesOfferingsResponse
--
--         , testDeleteDBInstanceResponse $
--             deleteDBInstanceResponse
--
--         , testDescribeDBInstancesResponse $
--             describeDBInstancesResponse
--
--         , testCopyOptionGroupResponse $
--             copyOptionGroupResponse
--
--         , testDownloadDBLogFilePortionResponse $
--             downloadDBLogFilePortionResponse
--
--         , testCreateDBInstanceReadReplicaResponse $
--             createDBInstanceReadReplicaResponse
--
--         , testRestoreDBClusterToPointInTimeResponse $
--             restoreDBClusterToPointInTimeResponse
--
--         , testDeleteDBParameterGroupResponse $
--             deleteDBParameterGroupResponse
--
--         , testDescribeDBSecurityGroupsResponse $
--             describeDBSecurityGroupsResponse
--
--           ]
--     ]

-- Requests

testDescribeDBEngineVersions :: DescribeDBEngineVersions -> TestTree
testDescribeDBEngineVersions = req
    "DescribeDBEngineVersions"
    "fixture/DescribeDBEngineVersions"

testDescribeDBClusterParameterGroups :: DescribeDBClusterParameterGroups -> TestTree
testDescribeDBClusterParameterGroups = req
    "DescribeDBClusterParameterGroups"
    "fixture/DescribeDBClusterParameterGroups"

testPromoteReadReplica :: PromoteReadReplica -> TestTree
testPromoteReadReplica = req
    "PromoteReadReplica"
    "fixture/PromoteReadReplica"

testModifyEventSubscription :: ModifyEventSubscription -> TestTree
testModifyEventSubscription = req
    "ModifyEventSubscription"
    "fixture/ModifyEventSubscription"

testCopyDBSnapshot :: CopyDBSnapshot -> TestTree
testCopyDBSnapshot = req
    "CopyDBSnapshot"
    "fixture/CopyDBSnapshot"

testAddSourceIdentifierToSubscription :: AddSourceIdentifierToSubscription -> TestTree
testAddSourceIdentifierToSubscription = req
    "AddSourceIdentifierToSubscription"
    "fixture/AddSourceIdentifierToSubscription"

testModifyDBInstance :: ModifyDBInstance -> TestTree
testModifyDBInstance = req
    "ModifyDBInstance"
    "fixture/ModifyDBInstance"

testResetDBClusterParameterGroup :: ResetDBClusterParameterGroup -> TestTree
testResetDBClusterParameterGroup = req
    "ResetDBClusterParameterGroup"
    "fixture/ResetDBClusterParameterGroup"

testDescribeEvents :: DescribeEvents -> TestTree
testDescribeEvents = req
    "DescribeEvents"
    "fixture/DescribeEvents"

testDescribeEngineDefaultParameters :: DescribeEngineDefaultParameters -> TestTree
testDescribeEngineDefaultParameters = req
    "DescribeEngineDefaultParameters"
    "fixture/DescribeEngineDefaultParameters"

testDescribeDBClusters :: DescribeDBClusters -> TestTree
testDescribeDBClusters = req
    "DescribeDBClusters"
    "fixture/DescribeDBClusters"

testModifyDBSubnetGroup :: ModifyDBSubnetGroup -> TestTree
testModifyDBSubnetGroup = req
    "ModifyDBSubnetGroup"
    "fixture/ModifyDBSubnetGroup"

testDescribeDBLogFiles :: DescribeDBLogFiles -> TestTree
testDescribeDBLogFiles = req
    "DescribeDBLogFiles"
    "fixture/DescribeDBLogFiles"

testListTagsForResource :: ListTagsForResource -> TestTree
testListTagsForResource = req
    "ListTagsForResource"
    "fixture/ListTagsForResource"

testDescribeOptionGroups :: DescribeOptionGroups -> TestTree
testDescribeOptionGroups = req
    "DescribeOptionGroups"
    "fixture/DescribeOptionGroups"

testDeleteDBCluster :: DeleteDBCluster -> TestTree
testDeleteDBCluster = req
    "DeleteDBCluster"
    "fixture/DeleteDBCluster"

testRemoveSourceIdentifierFromSubscription :: RemoveSourceIdentifierFromSubscription -> TestTree
testRemoveSourceIdentifierFromSubscription = req
    "RemoveSourceIdentifierFromSubscription"
    "fixture/RemoveSourceIdentifierFromSubscription"

testCopyDBParameterGroup :: CopyDBParameterGroup -> TestTree
testCopyDBParameterGroup = req
    "CopyDBParameterGroup"
    "fixture/CopyDBParameterGroup"

testDescribeReservedDBInstances :: DescribeReservedDBInstances -> TestTree
testDescribeReservedDBInstances = req
    "DescribeReservedDBInstances"
    "fixture/DescribeReservedDBInstances"

testDeleteOptionGroup :: DeleteOptionGroup -> TestTree
testDeleteOptionGroup = req
    "DeleteOptionGroup"
    "fixture/DeleteOptionGroup"

testDescribeEngineDefaultClusterParameters :: DescribeEngineDefaultClusterParameters -> TestTree
testDescribeEngineDefaultClusterParameters = req
    "DescribeEngineDefaultClusterParameters"
    "fixture/DescribeEngineDefaultClusterParameters"

testCreateEventSubscription :: CreateEventSubscription -> TestTree
testCreateEventSubscription = req
    "CreateEventSubscription"
    "fixture/CreateEventSubscription"

testRemoveTagsFromResource :: RemoveTagsFromResource -> TestTree
testRemoveTagsFromResource = req
    "RemoveTagsFromResource"
    "fixture/RemoveTagsFromResource"

testCreateDBInstance :: CreateDBInstance -> TestTree
testCreateDBInstance = req
    "CreateDBInstance"
    "fixture/CreateDBInstance"

testRestoreDBInstanceFromDBSnapshot :: RestoreDBInstanceFromDBSnapshot -> TestTree
testRestoreDBInstanceFromDBSnapshot = req
    "RestoreDBInstanceFromDBSnapshot"
    "fixture/RestoreDBInstanceFromDBSnapshot"

testAuthorizeDBSecurityGroupIngress :: AuthorizeDBSecurityGroupIngress -> TestTree
testAuthorizeDBSecurityGroupIngress = req
    "AuthorizeDBSecurityGroupIngress"
    "fixture/AuthorizeDBSecurityGroupIngress"

testDeleteDBClusterParameterGroup :: DeleteDBClusterParameterGroup -> TestTree
testDeleteDBClusterParameterGroup = req
    "DeleteDBClusterParameterGroup"
    "fixture/DeleteDBClusterParameterGroup"

testPurchaseReservedDBInstancesOffering :: PurchaseReservedDBInstancesOffering -> TestTree
testPurchaseReservedDBInstancesOffering = req
    "PurchaseReservedDBInstancesOffering"
    "fixture/PurchaseReservedDBInstancesOffering"

testDescribeCertificates :: DescribeCertificates -> TestTree
testDescribeCertificates = req
    "DescribeCertificates"
    "fixture/DescribeCertificates"

testRestoreDBClusterFromSnapshot :: RestoreDBClusterFromSnapshot -> TestTree
testRestoreDBClusterFromSnapshot = req
    "RestoreDBClusterFromSnapshot"
    "fixture/RestoreDBClusterFromSnapshot"

testCreateDBSnapshot :: CreateDBSnapshot -> TestTree
testCreateDBSnapshot = req
    "CreateDBSnapshot"
    "fixture/CreateDBSnapshot"

testDeleteEventSubscription :: DeleteEventSubscription -> TestTree
testDeleteEventSubscription = req
    "DeleteEventSubscription"
    "fixture/DeleteEventSubscription"

testDescribeDBParameterGroups :: DescribeDBParameterGroups -> TestTree
testDescribeDBParameterGroups = req
    "DescribeDBParameterGroups"
    "fixture/DescribeDBParameterGroups"

testDescribeOrderableDBInstanceOptions :: DescribeOrderableDBInstanceOptions -> TestTree
testDescribeOrderableDBInstanceOptions = req
    "DescribeOrderableDBInstanceOptions"
    "fixture/DescribeOrderableDBInstanceOptions"

testCreateDBClusterParameterGroup :: CreateDBClusterParameterGroup -> TestTree
testCreateDBClusterParameterGroup = req
    "CreateDBClusterParameterGroup"
    "fixture/CreateDBClusterParameterGroup"

testDescribeEventSubscriptions :: DescribeEventSubscriptions -> TestTree
testDescribeEventSubscriptions = req
    "DescribeEventSubscriptions"
    "fixture/DescribeEventSubscriptions"

testAddTagsToResource :: AddTagsToResource -> TestTree
testAddTagsToResource = req
    "AddTagsToResource"
    "fixture/AddTagsToResource"

testDescribeOptionGroupOptions :: DescribeOptionGroupOptions -> TestTree
testDescribeOptionGroupOptions = req
    "DescribeOptionGroupOptions"
    "fixture/DescribeOptionGroupOptions"

testDescribeDBParameters :: DescribeDBParameters -> TestTree
testDescribeDBParameters = req
    "DescribeDBParameters"
    "fixture/DescribeDBParameters"

testDeleteDBClusterSnapshot :: DeleteDBClusterSnapshot -> TestTree
testDeleteDBClusterSnapshot = req
    "DeleteDBClusterSnapshot"
    "fixture/DeleteDBClusterSnapshot"

testDescribeDBSnapshots :: DescribeDBSnapshots -> TestTree
testDescribeDBSnapshots = req
    "DescribeDBSnapshots"
    "fixture/DescribeDBSnapshots"

testDescribeDBSubnetGroups :: DescribeDBSubnetGroups -> TestTree
testDescribeDBSubnetGroups = req
    "DescribeDBSubnetGroups"
    "fixture/DescribeDBSubnetGroups"

testCreateDBParameterGroup :: CreateDBParameterGroup -> TestTree
testCreateDBParameterGroup = req
    "CreateDBParameterGroup"
    "fixture/CreateDBParameterGroup"

testCreateDBClusterSnapshot :: CreateDBClusterSnapshot -> TestTree
testCreateDBClusterSnapshot = req
    "CreateDBClusterSnapshot"
    "fixture/CreateDBClusterSnapshot"

testModifyOptionGroup :: ModifyOptionGroup -> TestTree
testModifyOptionGroup = req
    "ModifyOptionGroup"
    "fixture/ModifyOptionGroup"

testModifyDBCluster :: ModifyDBCluster -> TestTree
testModifyDBCluster = req
    "ModifyDBCluster"
    "fixture/ModifyDBCluster"

testDescribeEventCategories :: DescribeEventCategories -> TestTree
testDescribeEventCategories = req
    "DescribeEventCategories"
    "fixture/DescribeEventCategories"

testModifyDBClusterParameterGroup :: ModifyDBClusterParameterGroup -> TestTree
testModifyDBClusterParameterGroup = req
    "ModifyDBClusterParameterGroup"
    "fixture/ModifyDBClusterParameterGroup"

testDescribePendingMaintenanceActions :: DescribePendingMaintenanceActions -> TestTree
testDescribePendingMaintenanceActions = req
    "DescribePendingMaintenanceActions"
    "fixture/DescribePendingMaintenanceActions"

testRestoreDBInstanceToPointInTime :: RestoreDBInstanceToPointInTime -> TestTree
testRestoreDBInstanceToPointInTime = req
    "RestoreDBInstanceToPointInTime"
    "fixture/RestoreDBInstanceToPointInTime"

testResetDBParameterGroup :: ResetDBParameterGroup -> TestTree
testResetDBParameterGroup = req
    "ResetDBParameterGroup"
    "fixture/ResetDBParameterGroup"

testCopyDBClusterSnapshot :: CopyDBClusterSnapshot -> TestTree
testCopyDBClusterSnapshot = req
    "CopyDBClusterSnapshot"
    "fixture/CopyDBClusterSnapshot"

testModifyDBParameterGroup :: ModifyDBParameterGroup -> TestTree
testModifyDBParameterGroup = req
    "ModifyDBParameterGroup"
    "fixture/ModifyDBParameterGroup"

testFailoverDBCluster :: FailoverDBCluster -> TestTree
testFailoverDBCluster = req
    "FailoverDBCluster"
    "fixture/FailoverDBCluster"

testCreateDBCluster :: CreateDBCluster -> TestTree
testCreateDBCluster = req
    "CreateDBCluster"
    "fixture/CreateDBCluster"

testCreateOptionGroup :: CreateOptionGroup -> TestTree
testCreateOptionGroup = req
    "CreateOptionGroup"
    "fixture/CreateOptionGroup"

testApplyPendingMaintenanceAction :: ApplyPendingMaintenanceAction -> TestTree
testApplyPendingMaintenanceAction = req
    "ApplyPendingMaintenanceAction"
    "fixture/ApplyPendingMaintenanceAction"

testRevokeDBSecurityGroupIngress :: RevokeDBSecurityGroupIngress -> TestTree
testRevokeDBSecurityGroupIngress = req
    "RevokeDBSecurityGroupIngress"
    "fixture/RevokeDBSecurityGroupIngress"

testDeleteDBSnapshot :: DeleteDBSnapshot -> TestTree
testDeleteDBSnapshot = req
    "DeleteDBSnapshot"
    "fixture/DeleteDBSnapshot"

testDescribeDBClusterParameters :: DescribeDBClusterParameters -> TestTree
testDescribeDBClusterParameters = req
    "DescribeDBClusterParameters"
    "fixture/DescribeDBClusterParameters"

testCreateDBSecurityGroup :: CreateDBSecurityGroup -> TestTree
testCreateDBSecurityGroup = req
    "CreateDBSecurityGroup"
    "fixture/CreateDBSecurityGroup"

testDeleteDBSubnetGroup :: DeleteDBSubnetGroup -> TestTree
testDeleteDBSubnetGroup = req
    "DeleteDBSubnetGroup"
    "fixture/DeleteDBSubnetGroup"

testDescribeAccountAttributes :: DescribeAccountAttributes -> TestTree
testDescribeAccountAttributes = req
    "DescribeAccountAttributes"
    "fixture/DescribeAccountAttributes"

testDeleteDBSecurityGroup :: DeleteDBSecurityGroup -> TestTree
testDeleteDBSecurityGroup = req
    "DeleteDBSecurityGroup"
    "fixture/DeleteDBSecurityGroup"

testRebootDBInstance :: RebootDBInstance -> TestTree
testRebootDBInstance = req
    "RebootDBInstance"
    "fixture/RebootDBInstance"

testDescribeDBClusterSnapshots :: DescribeDBClusterSnapshots -> TestTree
testDescribeDBClusterSnapshots = req
    "DescribeDBClusterSnapshots"
    "fixture/DescribeDBClusterSnapshots"

testCreateDBSubnetGroup :: CreateDBSubnetGroup -> TestTree
testCreateDBSubnetGroup = req
    "CreateDBSubnetGroup"
    "fixture/CreateDBSubnetGroup"

testDescribeReservedDBInstancesOfferings :: DescribeReservedDBInstancesOfferings -> TestTree
testDescribeReservedDBInstancesOfferings = req
    "DescribeReservedDBInstancesOfferings"
    "fixture/DescribeReservedDBInstancesOfferings"

testDeleteDBInstance :: DeleteDBInstance -> TestTree
testDeleteDBInstance = req
    "DeleteDBInstance"
    "fixture/DeleteDBInstance"

testDescribeDBInstances :: DescribeDBInstances -> TestTree
testDescribeDBInstances = req
    "DescribeDBInstances"
    "fixture/DescribeDBInstances"

testCopyOptionGroup :: CopyOptionGroup -> TestTree
testCopyOptionGroup = req
    "CopyOptionGroup"
    "fixture/CopyOptionGroup"

testDownloadDBLogFilePortion :: DownloadDBLogFilePortion -> TestTree
testDownloadDBLogFilePortion = req
    "DownloadDBLogFilePortion"
    "fixture/DownloadDBLogFilePortion"

testCreateDBInstanceReadReplica :: CreateDBInstanceReadReplica -> TestTree
testCreateDBInstanceReadReplica = req
    "CreateDBInstanceReadReplica"
    "fixture/CreateDBInstanceReadReplica"

testRestoreDBClusterToPointInTime :: RestoreDBClusterToPointInTime -> TestTree
testRestoreDBClusterToPointInTime = req
    "RestoreDBClusterToPointInTime"
    "fixture/RestoreDBClusterToPointInTime"

testDeleteDBParameterGroup :: DeleteDBParameterGroup -> TestTree
testDeleteDBParameterGroup = req
    "DeleteDBParameterGroup"
    "fixture/DeleteDBParameterGroup"

testDescribeDBSecurityGroups :: DescribeDBSecurityGroups -> TestTree
testDescribeDBSecurityGroups = req
    "DescribeDBSecurityGroups"
    "fixture/DescribeDBSecurityGroups"

-- Responses

testDescribeDBEngineVersionsResponse :: DescribeDBEngineVersionsResponse -> TestTree
testDescribeDBEngineVersionsResponse = res
    "DescribeDBEngineVersionsResponse"
    "fixture/DescribeDBEngineVersionsResponse"
    (Proxy :: Proxy DescribeDBEngineVersions)

testDescribeDBClusterParameterGroupsResponse :: DescribeDBClusterParameterGroupsResponse -> TestTree
testDescribeDBClusterParameterGroupsResponse = res
    "DescribeDBClusterParameterGroupsResponse"
    "fixture/DescribeDBClusterParameterGroupsResponse"
    (Proxy :: Proxy DescribeDBClusterParameterGroups)

testPromoteReadReplicaResponse :: PromoteReadReplicaResponse -> TestTree
testPromoteReadReplicaResponse = res
    "PromoteReadReplicaResponse"
    "fixture/PromoteReadReplicaResponse"
    (Proxy :: Proxy PromoteReadReplica)

testModifyEventSubscriptionResponse :: ModifyEventSubscriptionResponse -> TestTree
testModifyEventSubscriptionResponse = res
    "ModifyEventSubscriptionResponse"
    "fixture/ModifyEventSubscriptionResponse"
    (Proxy :: Proxy ModifyEventSubscription)

testCopyDBSnapshotResponse :: CopyDBSnapshotResponse -> TestTree
testCopyDBSnapshotResponse = res
    "CopyDBSnapshotResponse"
    "fixture/CopyDBSnapshotResponse"
    (Proxy :: Proxy CopyDBSnapshot)

testAddSourceIdentifierToSubscriptionResponse :: AddSourceIdentifierToSubscriptionResponse -> TestTree
testAddSourceIdentifierToSubscriptionResponse = res
    "AddSourceIdentifierToSubscriptionResponse"
    "fixture/AddSourceIdentifierToSubscriptionResponse"
    (Proxy :: Proxy AddSourceIdentifierToSubscription)

testModifyDBInstanceResponse :: ModifyDBInstanceResponse -> TestTree
testModifyDBInstanceResponse = res
    "ModifyDBInstanceResponse"
    "fixture/ModifyDBInstanceResponse"
    (Proxy :: Proxy ModifyDBInstance)

testResetDBClusterParameterGroupResponse :: DBClusterParameterGroupNameMessage -> TestTree
testResetDBClusterParameterGroupResponse = res
    "ResetDBClusterParameterGroupResponse"
    "fixture/ResetDBClusterParameterGroupResponse"
    (Proxy :: Proxy ResetDBClusterParameterGroup)

testDescribeEventsResponse :: DescribeEventsResponse -> TestTree
testDescribeEventsResponse = res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse"
    (Proxy :: Proxy DescribeEvents)

testDescribeEngineDefaultParametersResponse :: DescribeEngineDefaultParametersResponse -> TestTree
testDescribeEngineDefaultParametersResponse = res
    "DescribeEngineDefaultParametersResponse"
    "fixture/DescribeEngineDefaultParametersResponse"
    (Proxy :: Proxy DescribeEngineDefaultParameters)

testDescribeDBClustersResponse :: DescribeDBClustersResponse -> TestTree
testDescribeDBClustersResponse = res
    "DescribeDBClustersResponse"
    "fixture/DescribeDBClustersResponse"
    (Proxy :: Proxy DescribeDBClusters)

testModifyDBSubnetGroupResponse :: ModifyDBSubnetGroupResponse -> TestTree
testModifyDBSubnetGroupResponse = res
    "ModifyDBSubnetGroupResponse"
    "fixture/ModifyDBSubnetGroupResponse"
    (Proxy :: Proxy ModifyDBSubnetGroup)

testDescribeDBLogFilesResponse :: DescribeDBLogFilesResponse -> TestTree
testDescribeDBLogFilesResponse = res
    "DescribeDBLogFilesResponse"
    "fixture/DescribeDBLogFilesResponse"
    (Proxy :: Proxy DescribeDBLogFiles)

testListTagsForResourceResponse :: ListTagsForResourceResponse -> TestTree
testListTagsForResourceResponse = res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse"
    (Proxy :: Proxy ListTagsForResource)

testDescribeOptionGroupsResponse :: DescribeOptionGroupsResponse -> TestTree
testDescribeOptionGroupsResponse = res
    "DescribeOptionGroupsResponse"
    "fixture/DescribeOptionGroupsResponse"
    (Proxy :: Proxy DescribeOptionGroups)

testDeleteDBClusterResponse :: DeleteDBClusterResponse -> TestTree
testDeleteDBClusterResponse = res
    "DeleteDBClusterResponse"
    "fixture/DeleteDBClusterResponse"
    (Proxy :: Proxy DeleteDBCluster)

testRemoveSourceIdentifierFromSubscriptionResponse :: RemoveSourceIdentifierFromSubscriptionResponse -> TestTree
testRemoveSourceIdentifierFromSubscriptionResponse = res
    "RemoveSourceIdentifierFromSubscriptionResponse"
    "fixture/RemoveSourceIdentifierFromSubscriptionResponse"
    (Proxy :: Proxy RemoveSourceIdentifierFromSubscription)

testCopyDBParameterGroupResponse :: CopyDBParameterGroupResponse -> TestTree
testCopyDBParameterGroupResponse = res
    "CopyDBParameterGroupResponse"
    "fixture/CopyDBParameterGroupResponse"
    (Proxy :: Proxy CopyDBParameterGroup)

testDescribeReservedDBInstancesResponse :: DescribeReservedDBInstancesResponse -> TestTree
testDescribeReservedDBInstancesResponse = res
    "DescribeReservedDBInstancesResponse"
    "fixture/DescribeReservedDBInstancesResponse"
    (Proxy :: Proxy DescribeReservedDBInstances)

testDeleteOptionGroupResponse :: DeleteOptionGroupResponse -> TestTree
testDeleteOptionGroupResponse = res
    "DeleteOptionGroupResponse"
    "fixture/DeleteOptionGroupResponse"
    (Proxy :: Proxy DeleteOptionGroup)

testDescribeEngineDefaultClusterParametersResponse :: DescribeEngineDefaultClusterParametersResponse -> TestTree
testDescribeEngineDefaultClusterParametersResponse = res
    "DescribeEngineDefaultClusterParametersResponse"
    "fixture/DescribeEngineDefaultClusterParametersResponse"
    (Proxy :: Proxy DescribeEngineDefaultClusterParameters)

testCreateEventSubscriptionResponse :: CreateEventSubscriptionResponse -> TestTree
testCreateEventSubscriptionResponse = res
    "CreateEventSubscriptionResponse"
    "fixture/CreateEventSubscriptionResponse"
    (Proxy :: Proxy CreateEventSubscription)

testRemoveTagsFromResourceResponse :: RemoveTagsFromResourceResponse -> TestTree
testRemoveTagsFromResourceResponse = res
    "RemoveTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse"
    (Proxy :: Proxy RemoveTagsFromResource)

testCreateDBInstanceResponse :: CreateDBInstanceResponse -> TestTree
testCreateDBInstanceResponse = res
    "CreateDBInstanceResponse"
    "fixture/CreateDBInstanceResponse"
    (Proxy :: Proxy CreateDBInstance)

testRestoreDBInstanceFromDBSnapshotResponse :: RestoreDBInstanceFromDBSnapshotResponse -> TestTree
testRestoreDBInstanceFromDBSnapshotResponse = res
    "RestoreDBInstanceFromDBSnapshotResponse"
    "fixture/RestoreDBInstanceFromDBSnapshotResponse"
    (Proxy :: Proxy RestoreDBInstanceFromDBSnapshot)

testAuthorizeDBSecurityGroupIngressResponse :: AuthorizeDBSecurityGroupIngressResponse -> TestTree
testAuthorizeDBSecurityGroupIngressResponse = res
    "AuthorizeDBSecurityGroupIngressResponse"
    "fixture/AuthorizeDBSecurityGroupIngressResponse"
    (Proxy :: Proxy AuthorizeDBSecurityGroupIngress)

testDeleteDBClusterParameterGroupResponse :: DeleteDBClusterParameterGroupResponse -> TestTree
testDeleteDBClusterParameterGroupResponse = res
    "DeleteDBClusterParameterGroupResponse"
    "fixture/DeleteDBClusterParameterGroupResponse"
    (Proxy :: Proxy DeleteDBClusterParameterGroup)

testPurchaseReservedDBInstancesOfferingResponse :: PurchaseReservedDBInstancesOfferingResponse -> TestTree
testPurchaseReservedDBInstancesOfferingResponse = res
    "PurchaseReservedDBInstancesOfferingResponse"
    "fixture/PurchaseReservedDBInstancesOfferingResponse"
    (Proxy :: Proxy PurchaseReservedDBInstancesOffering)

testDescribeCertificatesResponse :: DescribeCertificatesResponse -> TestTree
testDescribeCertificatesResponse = res
    "DescribeCertificatesResponse"
    "fixture/DescribeCertificatesResponse"
    (Proxy :: Proxy DescribeCertificates)

testRestoreDBClusterFromSnapshotResponse :: RestoreDBClusterFromSnapshotResponse -> TestTree
testRestoreDBClusterFromSnapshotResponse = res
    "RestoreDBClusterFromSnapshotResponse"
    "fixture/RestoreDBClusterFromSnapshotResponse"
    (Proxy :: Proxy RestoreDBClusterFromSnapshot)

testCreateDBSnapshotResponse :: CreateDBSnapshotResponse -> TestTree
testCreateDBSnapshotResponse = res
    "CreateDBSnapshotResponse"
    "fixture/CreateDBSnapshotResponse"
    (Proxy :: Proxy CreateDBSnapshot)

testDeleteEventSubscriptionResponse :: DeleteEventSubscriptionResponse -> TestTree
testDeleteEventSubscriptionResponse = res
    "DeleteEventSubscriptionResponse"
    "fixture/DeleteEventSubscriptionResponse"
    (Proxy :: Proxy DeleteEventSubscription)

testDescribeDBParameterGroupsResponse :: DescribeDBParameterGroupsResponse -> TestTree
testDescribeDBParameterGroupsResponse = res
    "DescribeDBParameterGroupsResponse"
    "fixture/DescribeDBParameterGroupsResponse"
    (Proxy :: Proxy DescribeDBParameterGroups)

testDescribeOrderableDBInstanceOptionsResponse :: DescribeOrderableDBInstanceOptionsResponse -> TestTree
testDescribeOrderableDBInstanceOptionsResponse = res
    "DescribeOrderableDBInstanceOptionsResponse"
    "fixture/DescribeOrderableDBInstanceOptionsResponse"
    (Proxy :: Proxy DescribeOrderableDBInstanceOptions)

testCreateDBClusterParameterGroupResponse :: CreateDBClusterParameterGroupResponse -> TestTree
testCreateDBClusterParameterGroupResponse = res
    "CreateDBClusterParameterGroupResponse"
    "fixture/CreateDBClusterParameterGroupResponse"
    (Proxy :: Proxy CreateDBClusterParameterGroup)

testDescribeEventSubscriptionsResponse :: DescribeEventSubscriptionsResponse -> TestTree
testDescribeEventSubscriptionsResponse = res
    "DescribeEventSubscriptionsResponse"
    "fixture/DescribeEventSubscriptionsResponse"
    (Proxy :: Proxy DescribeEventSubscriptions)

testAddTagsToResourceResponse :: AddTagsToResourceResponse -> TestTree
testAddTagsToResourceResponse = res
    "AddTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse"
    (Proxy :: Proxy AddTagsToResource)

testDescribeOptionGroupOptionsResponse :: DescribeOptionGroupOptionsResponse -> TestTree
testDescribeOptionGroupOptionsResponse = res
    "DescribeOptionGroupOptionsResponse"
    "fixture/DescribeOptionGroupOptionsResponse"
    (Proxy :: Proxy DescribeOptionGroupOptions)

testDescribeDBParametersResponse :: DescribeDBParametersResponse -> TestTree
testDescribeDBParametersResponse = res
    "DescribeDBParametersResponse"
    "fixture/DescribeDBParametersResponse"
    (Proxy :: Proxy DescribeDBParameters)

testDeleteDBClusterSnapshotResponse :: DeleteDBClusterSnapshotResponse -> TestTree
testDeleteDBClusterSnapshotResponse = res
    "DeleteDBClusterSnapshotResponse"
    "fixture/DeleteDBClusterSnapshotResponse"
    (Proxy :: Proxy DeleteDBClusterSnapshot)

testDescribeDBSnapshotsResponse :: DescribeDBSnapshotsResponse -> TestTree
testDescribeDBSnapshotsResponse = res
    "DescribeDBSnapshotsResponse"
    "fixture/DescribeDBSnapshotsResponse"
    (Proxy :: Proxy DescribeDBSnapshots)

testDescribeDBSubnetGroupsResponse :: DescribeDBSubnetGroupsResponse -> TestTree
testDescribeDBSubnetGroupsResponse = res
    "DescribeDBSubnetGroupsResponse"
    "fixture/DescribeDBSubnetGroupsResponse"
    (Proxy :: Proxy DescribeDBSubnetGroups)

testCreateDBParameterGroupResponse :: CreateDBParameterGroupResponse -> TestTree
testCreateDBParameterGroupResponse = res
    "CreateDBParameterGroupResponse"
    "fixture/CreateDBParameterGroupResponse"
    (Proxy :: Proxy CreateDBParameterGroup)

testCreateDBClusterSnapshotResponse :: CreateDBClusterSnapshotResponse -> TestTree
testCreateDBClusterSnapshotResponse = res
    "CreateDBClusterSnapshotResponse"
    "fixture/CreateDBClusterSnapshotResponse"
    (Proxy :: Proxy CreateDBClusterSnapshot)

testModifyOptionGroupResponse :: ModifyOptionGroupResponse -> TestTree
testModifyOptionGroupResponse = res
    "ModifyOptionGroupResponse"
    "fixture/ModifyOptionGroupResponse"
    (Proxy :: Proxy ModifyOptionGroup)

testModifyDBClusterResponse :: ModifyDBClusterResponse -> TestTree
testModifyDBClusterResponse = res
    "ModifyDBClusterResponse"
    "fixture/ModifyDBClusterResponse"
    (Proxy :: Proxy ModifyDBCluster)

testDescribeEventCategoriesResponse :: DescribeEventCategoriesResponse -> TestTree
testDescribeEventCategoriesResponse = res
    "DescribeEventCategoriesResponse"
    "fixture/DescribeEventCategoriesResponse"
    (Proxy :: Proxy DescribeEventCategories)

testModifyDBClusterParameterGroupResponse :: DBClusterParameterGroupNameMessage -> TestTree
testModifyDBClusterParameterGroupResponse = res
    "ModifyDBClusterParameterGroupResponse"
    "fixture/ModifyDBClusterParameterGroupResponse"
    (Proxy :: Proxy ModifyDBClusterParameterGroup)

testDescribePendingMaintenanceActionsResponse :: DescribePendingMaintenanceActionsResponse -> TestTree
testDescribePendingMaintenanceActionsResponse = res
    "DescribePendingMaintenanceActionsResponse"
    "fixture/DescribePendingMaintenanceActionsResponse"
    (Proxy :: Proxy DescribePendingMaintenanceActions)

testRestoreDBInstanceToPointInTimeResponse :: RestoreDBInstanceToPointInTimeResponse -> TestTree
testRestoreDBInstanceToPointInTimeResponse = res
    "RestoreDBInstanceToPointInTimeResponse"
    "fixture/RestoreDBInstanceToPointInTimeResponse"
    (Proxy :: Proxy RestoreDBInstanceToPointInTime)

testResetDBParameterGroupResponse :: DBParameterGroupNameMessage -> TestTree
testResetDBParameterGroupResponse = res
    "ResetDBParameterGroupResponse"
    "fixture/ResetDBParameterGroupResponse"
    (Proxy :: Proxy ResetDBParameterGroup)

testCopyDBClusterSnapshotResponse :: CopyDBClusterSnapshotResponse -> TestTree
testCopyDBClusterSnapshotResponse = res
    "CopyDBClusterSnapshotResponse"
    "fixture/CopyDBClusterSnapshotResponse"
    (Proxy :: Proxy CopyDBClusterSnapshot)

testModifyDBParameterGroupResponse :: DBParameterGroupNameMessage -> TestTree
testModifyDBParameterGroupResponse = res
    "ModifyDBParameterGroupResponse"
    "fixture/ModifyDBParameterGroupResponse"
    (Proxy :: Proxy ModifyDBParameterGroup)

testFailoverDBClusterResponse :: FailoverDBClusterResponse -> TestTree
testFailoverDBClusterResponse = res
    "FailoverDBClusterResponse"
    "fixture/FailoverDBClusterResponse"
    (Proxy :: Proxy FailoverDBCluster)

testCreateDBClusterResponse :: CreateDBClusterResponse -> TestTree
testCreateDBClusterResponse = res
    "CreateDBClusterResponse"
    "fixture/CreateDBClusterResponse"
    (Proxy :: Proxy CreateDBCluster)

testCreateOptionGroupResponse :: CreateOptionGroupResponse -> TestTree
testCreateOptionGroupResponse = res
    "CreateOptionGroupResponse"
    "fixture/CreateOptionGroupResponse"
    (Proxy :: Proxy CreateOptionGroup)

testApplyPendingMaintenanceActionResponse :: ApplyPendingMaintenanceActionResponse -> TestTree
testApplyPendingMaintenanceActionResponse = res
    "ApplyPendingMaintenanceActionResponse"
    "fixture/ApplyPendingMaintenanceActionResponse"
    (Proxy :: Proxy ApplyPendingMaintenanceAction)

testRevokeDBSecurityGroupIngressResponse :: RevokeDBSecurityGroupIngressResponse -> TestTree
testRevokeDBSecurityGroupIngressResponse = res
    "RevokeDBSecurityGroupIngressResponse"
    "fixture/RevokeDBSecurityGroupIngressResponse"
    (Proxy :: Proxy RevokeDBSecurityGroupIngress)

testDeleteDBSnapshotResponse :: DeleteDBSnapshotResponse -> TestTree
testDeleteDBSnapshotResponse = res
    "DeleteDBSnapshotResponse"
    "fixture/DeleteDBSnapshotResponse"
    (Proxy :: Proxy DeleteDBSnapshot)

testDescribeDBClusterParametersResponse :: DescribeDBClusterParametersResponse -> TestTree
testDescribeDBClusterParametersResponse = res
    "DescribeDBClusterParametersResponse"
    "fixture/DescribeDBClusterParametersResponse"
    (Proxy :: Proxy DescribeDBClusterParameters)

testCreateDBSecurityGroupResponse :: CreateDBSecurityGroupResponse -> TestTree
testCreateDBSecurityGroupResponse = res
    "CreateDBSecurityGroupResponse"
    "fixture/CreateDBSecurityGroupResponse"
    (Proxy :: Proxy CreateDBSecurityGroup)

testDeleteDBSubnetGroupResponse :: DeleteDBSubnetGroupResponse -> TestTree
testDeleteDBSubnetGroupResponse = res
    "DeleteDBSubnetGroupResponse"
    "fixture/DeleteDBSubnetGroupResponse"
    (Proxy :: Proxy DeleteDBSubnetGroup)

testDescribeAccountAttributesResponse :: DescribeAccountAttributesResponse -> TestTree
testDescribeAccountAttributesResponse = res
    "DescribeAccountAttributesResponse"
    "fixture/DescribeAccountAttributesResponse"
    (Proxy :: Proxy DescribeAccountAttributes)

testDeleteDBSecurityGroupResponse :: DeleteDBSecurityGroupResponse -> TestTree
testDeleteDBSecurityGroupResponse = res
    "DeleteDBSecurityGroupResponse"
    "fixture/DeleteDBSecurityGroupResponse"
    (Proxy :: Proxy DeleteDBSecurityGroup)

testRebootDBInstanceResponse :: RebootDBInstanceResponse -> TestTree
testRebootDBInstanceResponse = res
    "RebootDBInstanceResponse"
    "fixture/RebootDBInstanceResponse"
    (Proxy :: Proxy RebootDBInstance)

testDescribeDBClusterSnapshotsResponse :: DescribeDBClusterSnapshotsResponse -> TestTree
testDescribeDBClusterSnapshotsResponse = res
    "DescribeDBClusterSnapshotsResponse"
    "fixture/DescribeDBClusterSnapshotsResponse"
    (Proxy :: Proxy DescribeDBClusterSnapshots)

testCreateDBSubnetGroupResponse :: CreateDBSubnetGroupResponse -> TestTree
testCreateDBSubnetGroupResponse = res
    "CreateDBSubnetGroupResponse"
    "fixture/CreateDBSubnetGroupResponse"
    (Proxy :: Proxy CreateDBSubnetGroup)

testDescribeReservedDBInstancesOfferingsResponse :: DescribeReservedDBInstancesOfferingsResponse -> TestTree
testDescribeReservedDBInstancesOfferingsResponse = res
    "DescribeReservedDBInstancesOfferingsResponse"
    "fixture/DescribeReservedDBInstancesOfferingsResponse"
    (Proxy :: Proxy DescribeReservedDBInstancesOfferings)

testDeleteDBInstanceResponse :: DeleteDBInstanceResponse -> TestTree
testDeleteDBInstanceResponse = res
    "DeleteDBInstanceResponse"
    "fixture/DeleteDBInstanceResponse"
    (Proxy :: Proxy DeleteDBInstance)

testDescribeDBInstancesResponse :: DescribeDBInstancesResponse -> TestTree
testDescribeDBInstancesResponse = res
    "DescribeDBInstancesResponse"
    "fixture/DescribeDBInstancesResponse"
    (Proxy :: Proxy DescribeDBInstances)

testCopyOptionGroupResponse :: CopyOptionGroupResponse -> TestTree
testCopyOptionGroupResponse = res
    "CopyOptionGroupResponse"
    "fixture/CopyOptionGroupResponse"
    (Proxy :: Proxy CopyOptionGroup)

testDownloadDBLogFilePortionResponse :: DownloadDBLogFilePortionResponse -> TestTree
testDownloadDBLogFilePortionResponse = res
    "DownloadDBLogFilePortionResponse"
    "fixture/DownloadDBLogFilePortionResponse"
    (Proxy :: Proxy DownloadDBLogFilePortion)

testCreateDBInstanceReadReplicaResponse :: CreateDBInstanceReadReplicaResponse -> TestTree
testCreateDBInstanceReadReplicaResponse = res
    "CreateDBInstanceReadReplicaResponse"
    "fixture/CreateDBInstanceReadReplicaResponse"
    (Proxy :: Proxy CreateDBInstanceReadReplica)

testRestoreDBClusterToPointInTimeResponse :: RestoreDBClusterToPointInTimeResponse -> TestTree
testRestoreDBClusterToPointInTimeResponse = res
    "RestoreDBClusterToPointInTimeResponse"
    "fixture/RestoreDBClusterToPointInTimeResponse"
    (Proxy :: Proxy RestoreDBClusterToPointInTime)

testDeleteDBParameterGroupResponse :: DeleteDBParameterGroupResponse -> TestTree
testDeleteDBParameterGroupResponse = res
    "DeleteDBParameterGroupResponse"
    "fixture/DeleteDBParameterGroupResponse"
    (Proxy :: Proxy DeleteDBParameterGroup)

testDescribeDBSecurityGroupsResponse :: DescribeDBSecurityGroupsResponse -> TestTree
testDescribeDBSecurityGroupsResponse = res
    "DescribeDBSecurityGroupsResponse"
    "fixture/DescribeDBSecurityGroupsResponse"
    (Proxy :: Proxy DescribeDBSecurityGroups)
