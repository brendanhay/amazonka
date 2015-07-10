{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.RDS
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
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
--         , testDescribeEvents $
--             describeEvents
--
--         , testDescribeEngineDefaultParameters $
--             describeEngineDefaultParameters
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
--         , testPurchaseReservedDBInstancesOffering $
--             purchaseReservedDBInstancesOffering
--
--         , testDescribeCertificates $
--             describeCertificates
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
--         , testDescribeDBSnapshots $
--             describeDBSnapshots
--
--         , testDescribeDBSubnetGroups $
--             describeDBSubnetGroups
--
--         , testCreateDBParameterGroup $
--             createDBParameterGroup
--
--         , testModifyOptionGroup $
--             modifyOptionGroup
--
--         , testDescribeEventCategories $
--             describeEventCategories
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
--         , testModifyDBParameterGroup $
--             modifyDBParameterGroup
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
--         , testDescribeEventsResponse $
--             describeEventsResponse
--
--         , testDescribeEngineDefaultParametersResponse $
--             describeEngineDefaultParametersResponse
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
--         , testPurchaseReservedDBInstancesOfferingResponse $
--             purchaseReservedDBInstancesOfferingResponse
--
--         , testDescribeCertificatesResponse $
--             describeCertificatesResponse
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
--         , testDescribeDBSnapshotsResponse $
--             describeDBSnapshotsResponse
--
--         , testDescribeDBSubnetGroupsResponse $
--             describeDBSubnetGroupsResponse
--
--         , testCreateDBParameterGroupResponse $
--             createDBParameterGroupResponse
--
--         , testModifyOptionGroupResponse $
--             modifyOptionGroupResponse
--
--         , testDescribeEventCategoriesResponse $
--             describeEventCategoriesResponse
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
--         , testModifyDBParameterGroupResponse $
--             dbParameterGroupNameMessage
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

testDescribeEvents :: DescribeEvents -> TestTree
testDescribeEvents = req
    "DescribeEvents"
    "fixture/DescribeEvents"

testDescribeEngineDefaultParameters :: DescribeEngineDefaultParameters -> TestTree
testDescribeEngineDefaultParameters = req
    "DescribeEngineDefaultParameters"
    "fixture/DescribeEngineDefaultParameters"

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

testPurchaseReservedDBInstancesOffering :: PurchaseReservedDBInstancesOffering -> TestTree
testPurchaseReservedDBInstancesOffering = req
    "PurchaseReservedDBInstancesOffering"
    "fixture/PurchaseReservedDBInstancesOffering"

testDescribeCertificates :: DescribeCertificates -> TestTree
testDescribeCertificates = req
    "DescribeCertificates"
    "fixture/DescribeCertificates"

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

testModifyOptionGroup :: ModifyOptionGroup -> TestTree
testModifyOptionGroup = req
    "ModifyOptionGroup"
    "fixture/ModifyOptionGroup"

testDescribeEventCategories :: DescribeEventCategories -> TestTree
testDescribeEventCategories = req
    "DescribeEventCategories"
    "fixture/DescribeEventCategories"

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

testModifyDBParameterGroup :: ModifyDBParameterGroup -> TestTree
testModifyDBParameterGroup = req
    "ModifyDBParameterGroup"
    "fixture/ModifyDBParameterGroup"

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

testModifyOptionGroupResponse :: ModifyOptionGroupResponse -> TestTree
testModifyOptionGroupResponse = res
    "ModifyOptionGroupResponse"
    "fixture/ModifyOptionGroupResponse"
    (Proxy :: Proxy ModifyOptionGroup)

testDescribeEventCategoriesResponse :: DescribeEventCategoriesResponse -> TestTree
testDescribeEventCategoriesResponse = res
    "DescribeEventCategoriesResponse"
    "fixture/DescribeEventCategoriesResponse"
    (Proxy :: Proxy DescribeEventCategories)

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

testModifyDBParameterGroupResponse :: DBParameterGroupNameMessage -> TestTree
testModifyDBParameterGroupResponse = res
    "ModifyDBParameterGroupResponse"
    "fixture/ModifyDBParameterGroupResponse"
    (Proxy :: Proxy ModifyDBParameterGroup)

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

instance Out AccountQuota
instance Out AddSourceIdentifierToSubscription
instance Out AddSourceIdentifierToSubscriptionResponse
instance Out AddTagsToResource
instance Out AddTagsToResourceResponse
instance Out ApplyMethod
instance Out ApplyPendingMaintenanceAction
instance Out ApplyPendingMaintenanceActionResponse
instance Out AuthorizeDBSecurityGroupIngress
instance Out AuthorizeDBSecurityGroupIngressResponse
instance Out AvailabilityZone
instance Out Certificate
instance Out CharacterSet
instance Out CopyDBParameterGroup
instance Out CopyDBParameterGroupResponse
instance Out CopyDBSnapshot
instance Out CopyDBSnapshotResponse
instance Out CopyOptionGroup
instance Out CopyOptionGroupResponse
instance Out CreateDBInstance
instance Out CreateDBInstanceReadReplica
instance Out CreateDBInstanceReadReplicaResponse
instance Out CreateDBInstanceResponse
instance Out CreateDBParameterGroup
instance Out CreateDBParameterGroupResponse
instance Out CreateDBSecurityGroup
instance Out CreateDBSecurityGroupResponse
instance Out CreateDBSnapshot
instance Out CreateDBSnapshotResponse
instance Out CreateDBSubnetGroup
instance Out CreateDBSubnetGroupResponse
instance Out CreateEventSubscription
instance Out CreateEventSubscriptionResponse
instance Out CreateOptionGroup
instance Out CreateOptionGroupResponse
instance Out DBEngineVersion
instance Out DBInstance
instance Out DBInstanceStatusInfo
instance Out DBParameterGroup
instance Out DBParameterGroupNameMessage
instance Out DBParameterGroupStatus
instance Out DBSecurityGroup
instance Out DBSecurityGroupMembership
instance Out DBSnapshot
instance Out DBSubnetGroup
instance Out DeleteDBInstance
instance Out DeleteDBInstanceResponse
instance Out DeleteDBParameterGroup
instance Out DeleteDBParameterGroupResponse
instance Out DeleteDBSecurityGroup
instance Out DeleteDBSecurityGroupResponse
instance Out DeleteDBSnapshot
instance Out DeleteDBSnapshotResponse
instance Out DeleteDBSubnetGroup
instance Out DeleteDBSubnetGroupResponse
instance Out DeleteEventSubscription
instance Out DeleteEventSubscriptionResponse
instance Out DeleteOptionGroup
instance Out DeleteOptionGroupResponse
instance Out DescribeAccountAttributes
instance Out DescribeAccountAttributesResponse
instance Out DescribeCertificates
instance Out DescribeCertificatesResponse
instance Out DescribeDBEngineVersions
instance Out DescribeDBEngineVersionsResponse
instance Out DescribeDBInstances
instance Out DescribeDBInstancesResponse
instance Out DescribeDBLogFiles
instance Out DescribeDBLogFilesDetails
instance Out DescribeDBLogFilesResponse
instance Out DescribeDBParameterGroups
instance Out DescribeDBParameterGroupsResponse
instance Out DescribeDBParameters
instance Out DescribeDBParametersResponse
instance Out DescribeDBSecurityGroups
instance Out DescribeDBSecurityGroupsResponse
instance Out DescribeDBSnapshots
instance Out DescribeDBSnapshotsResponse
instance Out DescribeDBSubnetGroups
instance Out DescribeDBSubnetGroupsResponse
instance Out DescribeEngineDefaultParameters
instance Out DescribeEngineDefaultParametersResponse
instance Out DescribeEventCategories
instance Out DescribeEventCategoriesResponse
instance Out DescribeEventSubscriptions
instance Out DescribeEventSubscriptionsResponse
instance Out DescribeEvents
instance Out DescribeEventsResponse
instance Out DescribeOptionGroupOptions
instance Out DescribeOptionGroupOptionsResponse
instance Out DescribeOptionGroups
instance Out DescribeOptionGroupsResponse
instance Out DescribeOrderableDBInstanceOptions
instance Out DescribeOrderableDBInstanceOptionsResponse
instance Out DescribePendingMaintenanceActions
instance Out DescribePendingMaintenanceActionsResponse
instance Out DescribeReservedDBInstances
instance Out DescribeReservedDBInstancesOfferings
instance Out DescribeReservedDBInstancesOfferingsResponse
instance Out DescribeReservedDBInstancesResponse
instance Out DownloadDBLogFilePortion
instance Out DownloadDBLogFilePortionResponse
instance Out EC2SecurityGroup
instance Out Endpoint
instance Out EngineDefaults
instance Out Event
instance Out EventCategoriesMap
instance Out EventSubscription
instance Out Filter
instance Out IPRange
instance Out ListTagsForResource
instance Out ListTagsForResourceResponse
instance Out ModifyDBInstance
instance Out ModifyDBInstanceResponse
instance Out ModifyDBParameterGroup
instance Out ModifyDBSubnetGroup
instance Out ModifyDBSubnetGroupResponse
instance Out ModifyEventSubscription
instance Out ModifyEventSubscriptionResponse
instance Out ModifyOptionGroup
instance Out ModifyOptionGroupResponse
instance Out Option
instance Out OptionConfiguration
instance Out OptionGroup
instance Out OptionGroupMembership
instance Out OptionGroupOption
instance Out OptionGroupOptionSetting
instance Out OptionSetting
instance Out OrderableDBInstanceOption
instance Out Parameter
instance Out PendingMaintenanceAction
instance Out PendingModifiedValues
instance Out PromoteReadReplica
instance Out PromoteReadReplicaResponse
instance Out PurchaseReservedDBInstancesOffering
instance Out PurchaseReservedDBInstancesOfferingResponse
instance Out RebootDBInstance
instance Out RebootDBInstanceResponse
instance Out RecurringCharge
instance Out RemoveSourceIdentifierFromSubscription
instance Out RemoveSourceIdentifierFromSubscriptionResponse
instance Out RemoveTagsFromResource
instance Out RemoveTagsFromResourceResponse
instance Out ReservedDBInstance
instance Out ReservedDBInstancesOffering
instance Out ResetDBParameterGroup
instance Out ResourcePendingMaintenanceActions
instance Out RestoreDBInstanceFromDBSnapshot
instance Out RestoreDBInstanceFromDBSnapshotResponse
instance Out RestoreDBInstanceToPointInTime
instance Out RestoreDBInstanceToPointInTimeResponse
instance Out RevokeDBSecurityGroupIngress
instance Out RevokeDBSecurityGroupIngressResponse
instance Out SourceType
instance Out Subnet
instance Out Tag
instance Out VPCSecurityGroupMembership
