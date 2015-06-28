-- Module      : Test.AWS.Gen.RDS
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Test.AWS.Gen.RDS where

import           Data.Proxy
import           Network.AWS.RDS
import           Test.AWS.Fixture
import           Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ describeDBEngineVersionsTest $
--             describeDBEngineVersions
--
--         , promoteReadReplicaTest $
--             promoteReadReplica
--
--         , modifyEventSubscriptionTest $
--             modifyEventSubscription
--
--         , copyDBSnapshotTest $
--             copyDBSnapshot
--
--         , addSourceIdentifierToSubscriptionTest $
--             addSourceIdentifierToSubscription
--
--         , modifyDBInstanceTest $
--             modifyDBInstance
--
--         , describeEventsTest $
--             describeEvents
--
--         , describeEngineDefaultParametersTest $
--             describeEngineDefaultParameters
--
--         , modifyDBSubnetGroupTest $
--             modifyDBSubnetGroup
--
--         , describeDBLogFilesTest $
--             describeDBLogFiles
--
--         , listTagsForResourceTest $
--             listTagsForResource
--
--         , describeOptionGroupsTest $
--             describeOptionGroups
--
--         , removeSourceIdentifierFromSubscriptionTest $
--             removeSourceIdentifierFromSubscription
--
--         , copyDBParameterGroupTest $
--             copyDBParameterGroup
--
--         , describeReservedDBInstancesTest $
--             describeReservedDBInstances
--
--         , deleteOptionGroupTest $
--             deleteOptionGroup
--
--         , createEventSubscriptionTest $
--             createEventSubscription
--
--         , removeTagsFromResourceTest $
--             removeTagsFromResource
--
--         , createDBInstanceTest $
--             createDBInstance
--
--         , restoreDBInstanceFromDBSnapshotTest $
--             restoreDBInstanceFromDBSnapshot
--
--         , authorizeDBSecurityGroupIngressTest $
--             authorizeDBSecurityGroupIngress
--
--         , purchaseReservedDBInstancesOfferingTest $
--             purchaseReservedDBInstancesOffering
--
--         , describeCertificatesTest $
--             describeCertificates
--
--         , createDBSnapshotTest $
--             createDBSnapshot
--
--         , deleteEventSubscriptionTest $
--             deleteEventSubscription
--
--         , describeDBParameterGroupsTest $
--             describeDBParameterGroups
--
--         , describeOrderableDBInstanceOptionsTest $
--             describeOrderableDBInstanceOptions
--
--         , describeEventSubscriptionsTest $
--             describeEventSubscriptions
--
--         , addTagsToResourceTest $
--             addTagsToResource
--
--         , describeOptionGroupOptionsTest $
--             describeOptionGroupOptions
--
--         , describeDBParametersTest $
--             describeDBParameters
--
--         , describeDBSnapshotsTest $
--             describeDBSnapshots
--
--         , describeDBSubnetGroupsTest $
--             describeDBSubnetGroups
--
--         , createDBParameterGroupTest $
--             createDBParameterGroup
--
--         , modifyOptionGroupTest $
--             modifyOptionGroup
--
--         , describeEventCategoriesTest $
--             describeEventCategories
--
--         , describePendingMaintenanceActionsTest $
--             describePendingMaintenanceActions
--
--         , restoreDBInstanceToPointInTimeTest $
--             restoreDBInstanceToPointInTime
--
--         , resetDBParameterGroupTest $
--             resetDBParameterGroup
--
--         , modifyDBParameterGroupTest $
--             modifyDBParameterGroup
--
--         , createOptionGroupTest $
--             createOptionGroup
--
--         , applyPendingMaintenanceActionTest $
--             applyPendingMaintenanceAction
--
--         , revokeDBSecurityGroupIngressTest $
--             revokeDBSecurityGroupIngress
--
--         , deleteDBSnapshotTest $
--             deleteDBSnapshot
--
--         , createDBSecurityGroupTest $
--             createDBSecurityGroup
--
--         , deleteDBSubnetGroupTest $
--             deleteDBSubnetGroup
--
--         , describeAccountAttributesTest $
--             describeAccountAttributes
--
--         , deleteDBSecurityGroupTest $
--             deleteDBSecurityGroup
--
--         , rebootDBInstanceTest $
--             rebootDBInstance
--
--         , createDBSubnetGroupTest $
--             createDBSubnetGroup
--
--         , describeReservedDBInstancesOfferingsTest $
--             describeReservedDBInstancesOfferings
--
--         , deleteDBInstanceTest $
--             deleteDBInstance
--
--         , describeDBInstancesTest $
--             describeDBInstances
--
--         , copyOptionGroupTest $
--             copyOptionGroup
--
--         , downloadDBLogFilePortionTest $
--             downloadDBLogFilePortion
--
--         , createDBInstanceReadReplicaTest $
--             createDBInstanceReadReplica
--
--         , deleteDBParameterGroupTest $
--             deleteDBParameterGroup
--
--         , describeDBSecurityGroupsTest $
--             describeDBSecurityGroups
--
--           ]

--     , testGroup "response"
--         [ describeDBEngineVersionsResponseTest $
--             describeDBEngineVersionsResponse
--
--         , promoteReadReplicaResponseTest $
--             promoteReadReplicaResponse
--
--         , modifyEventSubscriptionResponseTest $
--             modifyEventSubscriptionResponse
--
--         , copyDBSnapshotResponseTest $
--             copyDBSnapshotResponse
--
--         , addSourceIdentifierToSubscriptionResponseTest $
--             addSourceIdentifierToSubscriptionResponse
--
--         , modifyDBInstanceResponseTest $
--             modifyDBInstanceResponse
--
--         , describeEventsResponseTest $
--             describeEventsResponse
--
--         , describeEngineDefaultParametersResponseTest $
--             describeEngineDefaultParametersResponse
--
--         , modifyDBSubnetGroupResponseTest $
--             modifyDBSubnetGroupResponse
--
--         , describeDBLogFilesResponseTest $
--             describeDBLogFilesResponse
--
--         , listTagsForResourceResponseTest $
--             listTagsForResourceResponse
--
--         , describeOptionGroupsResponseTest $
--             describeOptionGroupsResponse
--
--         , removeSourceIdentifierFromSubscriptionResponseTest $
--             removeSourceIdentifierFromSubscriptionResponse
--
--         , copyDBParameterGroupResponseTest $
--             copyDBParameterGroupResponse
--
--         , describeReservedDBInstancesResponseTest $
--             describeReservedDBInstancesResponse
--
--         , deleteOptionGroupResponseTest $
--             deleteOptionGroupResponse
--
--         , createEventSubscriptionResponseTest $
--             createEventSubscriptionResponse
--
--         , removeTagsFromResourceResponseTest $
--             removeTagsFromResourceResponse
--
--         , createDBInstanceResponseTest $
--             createDBInstanceResponse
--
--         , restoreDBInstanceFromDBSnapshotResponseTest $
--             restoreDBInstanceFromDBSnapshotResponse
--
--         , authorizeDBSecurityGroupIngressResponseTest $
--             authorizeDBSecurityGroupIngressResponse
--
--         , purchaseReservedDBInstancesOfferingResponseTest $
--             purchaseReservedDBInstancesOfferingResponse
--
--         , describeCertificatesResponseTest $
--             describeCertificatesResponse
--
--         , createDBSnapshotResponseTest $
--             createDBSnapshotResponse
--
--         , deleteEventSubscriptionResponseTest $
--             deleteEventSubscriptionResponse
--
--         , describeDBParameterGroupsResponseTest $
--             describeDBParameterGroupsResponse
--
--         , describeOrderableDBInstanceOptionsResponseTest $
--             describeOrderableDBInstanceOptionsResponse
--
--         , describeEventSubscriptionsResponseTest $
--             describeEventSubscriptionsResponse
--
--         , addTagsToResourceResponseTest $
--             addTagsToResourceResponse
--
--         , describeOptionGroupOptionsResponseTest $
--             describeOptionGroupOptionsResponse
--
--         , describeDBParametersResponseTest $
--             describeDBParametersResponse
--
--         , describeDBSnapshotsResponseTest $
--             describeDBSnapshotsResponse
--
--         , describeDBSubnetGroupsResponseTest $
--             describeDBSubnetGroupsResponse
--
--         , createDBParameterGroupResponseTest $
--             createDBParameterGroupResponse
--
--         , modifyOptionGroupResponseTest $
--             modifyOptionGroupResponse
--
--         , describeEventCategoriesResponseTest $
--             describeEventCategoriesResponse
--
--         , describePendingMaintenanceActionsResponseTest $
--             describePendingMaintenanceActionsResponse
--
--         , restoreDBInstanceToPointInTimeResponseTest $
--             restoreDBInstanceToPointInTimeResponse
--
--         , dbParameterGroupNameMessageTest $
--             dbParameterGroupNameMessage
--
--         , dbParameterGroupNameMessageTest $
--             dbParameterGroupNameMessage
--
--         , createOptionGroupResponseTest $
--             createOptionGroupResponse
--
--         , applyPendingMaintenanceActionResponseTest $
--             applyPendingMaintenanceActionResponse
--
--         , revokeDBSecurityGroupIngressResponseTest $
--             revokeDBSecurityGroupIngressResponse
--
--         , deleteDBSnapshotResponseTest $
--             deleteDBSnapshotResponse
--
--         , createDBSecurityGroupResponseTest $
--             createDBSecurityGroupResponse
--
--         , deleteDBSubnetGroupResponseTest $
--             deleteDBSubnetGroupResponse
--
--         , describeAccountAttributesResponseTest $
--             describeAccountAttributesResponse
--
--         , deleteDBSecurityGroupResponseTest $
--             deleteDBSecurityGroupResponse
--
--         , rebootDBInstanceResponseTest $
--             rebootDBInstanceResponse
--
--         , createDBSubnetGroupResponseTest $
--             createDBSubnetGroupResponse
--
--         , describeReservedDBInstancesOfferingsResponseTest $
--             describeReservedDBInstancesOfferingsResponse
--
--         , deleteDBInstanceResponseTest $
--             deleteDBInstanceResponse
--
--         , describeDBInstancesResponseTest $
--             describeDBInstancesResponse
--
--         , copyOptionGroupResponseTest $
--             copyOptionGroupResponse
--
--         , downloadDBLogFilePortionResponseTest $
--             downloadDBLogFilePortionResponse
--
--         , createDBInstanceReadReplicaResponseTest $
--             createDBInstanceReadReplicaResponse
--
--         , deleteDBParameterGroupResponseTest $
--             deleteDBParameterGroupResponse
--
--         , describeDBSecurityGroupsResponseTest $
--             describeDBSecurityGroupsResponse
--
--           ]
--     ]

-- Requests

describeDBEngineVersionsTest :: DescribeDBEngineVersions -> TestTree
describeDBEngineVersionsTest = undefined

promoteReadReplicaTest :: PromoteReadReplica -> TestTree
promoteReadReplicaTest = undefined

modifyEventSubscriptionTest :: ModifyEventSubscription -> TestTree
modifyEventSubscriptionTest = undefined

copyDBSnapshotTest :: CopyDBSnapshot -> TestTree
copyDBSnapshotTest = undefined

addSourceIdentifierToSubscriptionTest :: AddSourceIdentifierToSubscription -> TestTree
addSourceIdentifierToSubscriptionTest = undefined

modifyDBInstanceTest :: ModifyDBInstance -> TestTree
modifyDBInstanceTest = undefined

describeEventsTest :: DescribeEvents -> TestTree
describeEventsTest = undefined

describeEngineDefaultParametersTest :: DescribeEngineDefaultParameters -> TestTree
describeEngineDefaultParametersTest = undefined

modifyDBSubnetGroupTest :: ModifyDBSubnetGroup -> TestTree
modifyDBSubnetGroupTest = undefined

describeDBLogFilesTest :: DescribeDBLogFiles -> TestTree
describeDBLogFilesTest = undefined

listTagsForResourceTest :: ListTagsForResource -> TestTree
listTagsForResourceTest = undefined

describeOptionGroupsTest :: DescribeOptionGroups -> TestTree
describeOptionGroupsTest = undefined

removeSourceIdentifierFromSubscriptionTest :: RemoveSourceIdentifierFromSubscription -> TestTree
removeSourceIdentifierFromSubscriptionTest = undefined

copyDBParameterGroupTest :: CopyDBParameterGroup -> TestTree
copyDBParameterGroupTest = undefined

describeReservedDBInstancesTest :: DescribeReservedDBInstances -> TestTree
describeReservedDBInstancesTest = undefined

deleteOptionGroupTest :: DeleteOptionGroup -> TestTree
deleteOptionGroupTest = undefined

createEventSubscriptionTest :: CreateEventSubscription -> TestTree
createEventSubscriptionTest = undefined

removeTagsFromResourceTest :: RemoveTagsFromResource -> TestTree
removeTagsFromResourceTest = undefined

createDBInstanceTest :: CreateDBInstance -> TestTree
createDBInstanceTest = undefined

restoreDBInstanceFromDBSnapshotTest :: RestoreDBInstanceFromDBSnapshot -> TestTree
restoreDBInstanceFromDBSnapshotTest = undefined

authorizeDBSecurityGroupIngressTest :: AuthorizeDBSecurityGroupIngress -> TestTree
authorizeDBSecurityGroupIngressTest = undefined

purchaseReservedDBInstancesOfferingTest :: PurchaseReservedDBInstancesOffering -> TestTree
purchaseReservedDBInstancesOfferingTest = undefined

describeCertificatesTest :: DescribeCertificates -> TestTree
describeCertificatesTest = undefined

createDBSnapshotTest :: CreateDBSnapshot -> TestTree
createDBSnapshotTest = undefined

deleteEventSubscriptionTest :: DeleteEventSubscription -> TestTree
deleteEventSubscriptionTest = undefined

describeDBParameterGroupsTest :: DescribeDBParameterGroups -> TestTree
describeDBParameterGroupsTest = undefined

describeOrderableDBInstanceOptionsTest :: DescribeOrderableDBInstanceOptions -> TestTree
describeOrderableDBInstanceOptionsTest = undefined

describeEventSubscriptionsTest :: DescribeEventSubscriptions -> TestTree
describeEventSubscriptionsTest = undefined

addTagsToResourceTest :: AddTagsToResource -> TestTree
addTagsToResourceTest = undefined

describeOptionGroupOptionsTest :: DescribeOptionGroupOptions -> TestTree
describeOptionGroupOptionsTest = undefined

describeDBParametersTest :: DescribeDBParameters -> TestTree
describeDBParametersTest = undefined

describeDBSnapshotsTest :: DescribeDBSnapshots -> TestTree
describeDBSnapshotsTest = undefined

describeDBSubnetGroupsTest :: DescribeDBSubnetGroups -> TestTree
describeDBSubnetGroupsTest = undefined

createDBParameterGroupTest :: CreateDBParameterGroup -> TestTree
createDBParameterGroupTest = undefined

modifyOptionGroupTest :: ModifyOptionGroup -> TestTree
modifyOptionGroupTest = undefined

describeEventCategoriesTest :: DescribeEventCategories -> TestTree
describeEventCategoriesTest = undefined

describePendingMaintenanceActionsTest :: DescribePendingMaintenanceActions -> TestTree
describePendingMaintenanceActionsTest = undefined

restoreDBInstanceToPointInTimeTest :: RestoreDBInstanceToPointInTime -> TestTree
restoreDBInstanceToPointInTimeTest = undefined

resetDBParameterGroupTest :: ResetDBParameterGroup -> TestTree
resetDBParameterGroupTest = undefined

modifyDBParameterGroupTest :: ModifyDBParameterGroup -> TestTree
modifyDBParameterGroupTest = undefined

createOptionGroupTest :: CreateOptionGroup -> TestTree
createOptionGroupTest = undefined

applyPendingMaintenanceActionTest :: ApplyPendingMaintenanceAction -> TestTree
applyPendingMaintenanceActionTest = undefined

revokeDBSecurityGroupIngressTest :: RevokeDBSecurityGroupIngress -> TestTree
revokeDBSecurityGroupIngressTest = undefined

deleteDBSnapshotTest :: DeleteDBSnapshot -> TestTree
deleteDBSnapshotTest = undefined

createDBSecurityGroupTest :: CreateDBSecurityGroup -> TestTree
createDBSecurityGroupTest = undefined

deleteDBSubnetGroupTest :: DeleteDBSubnetGroup -> TestTree
deleteDBSubnetGroupTest = undefined

describeAccountAttributesTest :: DescribeAccountAttributes -> TestTree
describeAccountAttributesTest = undefined

deleteDBSecurityGroupTest :: DeleteDBSecurityGroup -> TestTree
deleteDBSecurityGroupTest = undefined

rebootDBInstanceTest :: RebootDBInstance -> TestTree
rebootDBInstanceTest = undefined

createDBSubnetGroupTest :: CreateDBSubnetGroup -> TestTree
createDBSubnetGroupTest = undefined

describeReservedDBInstancesOfferingsTest :: DescribeReservedDBInstancesOfferings -> TestTree
describeReservedDBInstancesOfferingsTest = undefined

deleteDBInstanceTest :: DeleteDBInstance -> TestTree
deleteDBInstanceTest = undefined

describeDBInstancesTest :: DescribeDBInstances -> TestTree
describeDBInstancesTest = undefined

copyOptionGroupTest :: CopyOptionGroup -> TestTree
copyOptionGroupTest = undefined

downloadDBLogFilePortionTest :: DownloadDBLogFilePortion -> TestTree
downloadDBLogFilePortionTest = undefined

createDBInstanceReadReplicaTest :: CreateDBInstanceReadReplica -> TestTree
createDBInstanceReadReplicaTest = undefined

deleteDBParameterGroupTest :: DeleteDBParameterGroup -> TestTree
deleteDBParameterGroupTest = undefined

describeDBSecurityGroupsTest :: DescribeDBSecurityGroups -> TestTree
describeDBSecurityGroupsTest = undefined

-- Responses

describeDBEngineVersionsResponseTest :: DescribeDBEngineVersionsResponse -> TestTree
describeDBEngineVersionsResponseTest = resp
    "DescribeDBEngineVersions"
    "fixture/RDS/DescribeDBEngineVersionsResponse"
    (Proxy :: Proxy DescribeDBEngineVersions)

promoteReadReplicaResponseTest :: PromoteReadReplicaResponse -> TestTree
promoteReadReplicaResponseTest = resp
    "PromoteReadReplica"
    "fixture/RDS/PromoteReadReplicaResponse"
    (Proxy :: Proxy PromoteReadReplica)

modifyEventSubscriptionResponseTest :: ModifyEventSubscriptionResponse -> TestTree
modifyEventSubscriptionResponseTest = resp
    "ModifyEventSubscription"
    "fixture/RDS/ModifyEventSubscriptionResponse"
    (Proxy :: Proxy ModifyEventSubscription)

copyDBSnapshotResponseTest :: CopyDBSnapshotResponse -> TestTree
copyDBSnapshotResponseTest = resp
    "CopyDBSnapshot"
    "fixture/RDS/CopyDBSnapshotResponse"
    (Proxy :: Proxy CopyDBSnapshot)

addSourceIdentifierToSubscriptionResponseTest :: AddSourceIdentifierToSubscriptionResponse -> TestTree
addSourceIdentifierToSubscriptionResponseTest = resp
    "AddSourceIdentifierToSubscription"
    "fixture/RDS/AddSourceIdentifierToSubscriptionResponse"
    (Proxy :: Proxy AddSourceIdentifierToSubscription)

modifyDBInstanceResponseTest :: ModifyDBInstanceResponse -> TestTree
modifyDBInstanceResponseTest = resp
    "ModifyDBInstance"
    "fixture/RDS/ModifyDBInstanceResponse"
    (Proxy :: Proxy ModifyDBInstance)

describeEventsResponseTest :: DescribeEventsResponse -> TestTree
describeEventsResponseTest = resp
    "DescribeEvents"
    "fixture/RDS/DescribeEventsResponse"
    (Proxy :: Proxy DescribeEvents)

describeEngineDefaultParametersResponseTest :: DescribeEngineDefaultParametersResponse -> TestTree
describeEngineDefaultParametersResponseTest = resp
    "DescribeEngineDefaultParameters"
    "fixture/RDS/DescribeEngineDefaultParametersResponse"
    (Proxy :: Proxy DescribeEngineDefaultParameters)

modifyDBSubnetGroupResponseTest :: ModifyDBSubnetGroupResponse -> TestTree
modifyDBSubnetGroupResponseTest = resp
    "ModifyDBSubnetGroup"
    "fixture/RDS/ModifyDBSubnetGroupResponse"
    (Proxy :: Proxy ModifyDBSubnetGroup)

describeDBLogFilesResponseTest :: DescribeDBLogFilesResponse -> TestTree
describeDBLogFilesResponseTest = resp
    "DescribeDBLogFiles"
    "fixture/RDS/DescribeDBLogFilesResponse"
    (Proxy :: Proxy DescribeDBLogFiles)

listTagsForResourceResponseTest :: ListTagsForResourceResponse -> TestTree
listTagsForResourceResponseTest = resp
    "ListTagsForResource"
    "fixture/RDS/ListTagsForResourceResponse"
    (Proxy :: Proxy ListTagsForResource)

describeOptionGroupsResponseTest :: DescribeOptionGroupsResponse -> TestTree
describeOptionGroupsResponseTest = resp
    "DescribeOptionGroups"
    "fixture/RDS/DescribeOptionGroupsResponse"
    (Proxy :: Proxy DescribeOptionGroups)

removeSourceIdentifierFromSubscriptionResponseTest :: RemoveSourceIdentifierFromSubscriptionResponse -> TestTree
removeSourceIdentifierFromSubscriptionResponseTest = resp
    "RemoveSourceIdentifierFromSubscription"
    "fixture/RDS/RemoveSourceIdentifierFromSubscriptionResponse"
    (Proxy :: Proxy RemoveSourceIdentifierFromSubscription)

copyDBParameterGroupResponseTest :: CopyDBParameterGroupResponse -> TestTree
copyDBParameterGroupResponseTest = resp
    "CopyDBParameterGroup"
    "fixture/RDS/CopyDBParameterGroupResponse"
    (Proxy :: Proxy CopyDBParameterGroup)

describeReservedDBInstancesResponseTest :: DescribeReservedDBInstancesResponse -> TestTree
describeReservedDBInstancesResponseTest = resp
    "DescribeReservedDBInstances"
    "fixture/RDS/DescribeReservedDBInstancesResponse"
    (Proxy :: Proxy DescribeReservedDBInstances)

deleteOptionGroupResponseTest :: DeleteOptionGroupResponse -> TestTree
deleteOptionGroupResponseTest = resp
    "DeleteOptionGroup"
    "fixture/RDS/DeleteOptionGroupResponse"
    (Proxy :: Proxy DeleteOptionGroup)

createEventSubscriptionResponseTest :: CreateEventSubscriptionResponse -> TestTree
createEventSubscriptionResponseTest = resp
    "CreateEventSubscription"
    "fixture/RDS/CreateEventSubscriptionResponse"
    (Proxy :: Proxy CreateEventSubscription)

removeTagsFromResourceResponseTest :: RemoveTagsFromResourceResponse -> TestTree
removeTagsFromResourceResponseTest = resp
    "RemoveTagsFromResource"
    "fixture/RDS/RemoveTagsFromResourceResponse"
    (Proxy :: Proxy RemoveTagsFromResource)

createDBInstanceResponseTest :: CreateDBInstanceResponse -> TestTree
createDBInstanceResponseTest = resp
    "CreateDBInstance"
    "fixture/RDS/CreateDBInstanceResponse"
    (Proxy :: Proxy CreateDBInstance)

restoreDBInstanceFromDBSnapshotResponseTest :: RestoreDBInstanceFromDBSnapshotResponse -> TestTree
restoreDBInstanceFromDBSnapshotResponseTest = resp
    "RestoreDBInstanceFromDBSnapshot"
    "fixture/RDS/RestoreDBInstanceFromDBSnapshotResponse"
    (Proxy :: Proxy RestoreDBInstanceFromDBSnapshot)

authorizeDBSecurityGroupIngressResponseTest :: AuthorizeDBSecurityGroupIngressResponse -> TestTree
authorizeDBSecurityGroupIngressResponseTest = resp
    "AuthorizeDBSecurityGroupIngress"
    "fixture/RDS/AuthorizeDBSecurityGroupIngressResponse"
    (Proxy :: Proxy AuthorizeDBSecurityGroupIngress)

purchaseReservedDBInstancesOfferingResponseTest :: PurchaseReservedDBInstancesOfferingResponse -> TestTree
purchaseReservedDBInstancesOfferingResponseTest = resp
    "PurchaseReservedDBInstancesOffering"
    "fixture/RDS/PurchaseReservedDBInstancesOfferingResponse"
    (Proxy :: Proxy PurchaseReservedDBInstancesOffering)

describeCertificatesResponseTest :: DescribeCertificatesResponse -> TestTree
describeCertificatesResponseTest = resp
    "DescribeCertificates"
    "fixture/RDS/DescribeCertificatesResponse"
    (Proxy :: Proxy DescribeCertificates)

createDBSnapshotResponseTest :: CreateDBSnapshotResponse -> TestTree
createDBSnapshotResponseTest = resp
    "CreateDBSnapshot"
    "fixture/RDS/CreateDBSnapshotResponse"
    (Proxy :: Proxy CreateDBSnapshot)

deleteEventSubscriptionResponseTest :: DeleteEventSubscriptionResponse -> TestTree
deleteEventSubscriptionResponseTest = resp
    "DeleteEventSubscription"
    "fixture/RDS/DeleteEventSubscriptionResponse"
    (Proxy :: Proxy DeleteEventSubscription)

describeDBParameterGroupsResponseTest :: DescribeDBParameterGroupsResponse -> TestTree
describeDBParameterGroupsResponseTest = resp
    "DescribeDBParameterGroups"
    "fixture/RDS/DescribeDBParameterGroupsResponse"
    (Proxy :: Proxy DescribeDBParameterGroups)

describeOrderableDBInstanceOptionsResponseTest :: DescribeOrderableDBInstanceOptionsResponse -> TestTree
describeOrderableDBInstanceOptionsResponseTest = resp
    "DescribeOrderableDBInstanceOptions"
    "fixture/RDS/DescribeOrderableDBInstanceOptionsResponse"
    (Proxy :: Proxy DescribeOrderableDBInstanceOptions)

describeEventSubscriptionsResponseTest :: DescribeEventSubscriptionsResponse -> TestTree
describeEventSubscriptionsResponseTest = resp
    "DescribeEventSubscriptions"
    "fixture/RDS/DescribeEventSubscriptionsResponse"
    (Proxy :: Proxy DescribeEventSubscriptions)

addTagsToResourceResponseTest :: AddTagsToResourceResponse -> TestTree
addTagsToResourceResponseTest = resp
    "AddTagsToResource"
    "fixture/RDS/AddTagsToResourceResponse"
    (Proxy :: Proxy AddTagsToResource)

describeOptionGroupOptionsResponseTest :: DescribeOptionGroupOptionsResponse -> TestTree
describeOptionGroupOptionsResponseTest = resp
    "DescribeOptionGroupOptions"
    "fixture/RDS/DescribeOptionGroupOptionsResponse"
    (Proxy :: Proxy DescribeOptionGroupOptions)

describeDBParametersResponseTest :: DescribeDBParametersResponse -> TestTree
describeDBParametersResponseTest = resp
    "DescribeDBParameters"
    "fixture/RDS/DescribeDBParametersResponse"
    (Proxy :: Proxy DescribeDBParameters)

describeDBSnapshotsResponseTest :: DescribeDBSnapshotsResponse -> TestTree
describeDBSnapshotsResponseTest = resp
    "DescribeDBSnapshots"
    "fixture/RDS/DescribeDBSnapshotsResponse"
    (Proxy :: Proxy DescribeDBSnapshots)

describeDBSubnetGroupsResponseTest :: DescribeDBSubnetGroupsResponse -> TestTree
describeDBSubnetGroupsResponseTest = resp
    "DescribeDBSubnetGroups"
    "fixture/RDS/DescribeDBSubnetGroupsResponse"
    (Proxy :: Proxy DescribeDBSubnetGroups)

createDBParameterGroupResponseTest :: CreateDBParameterGroupResponse -> TestTree
createDBParameterGroupResponseTest = resp
    "CreateDBParameterGroup"
    "fixture/RDS/CreateDBParameterGroupResponse"
    (Proxy :: Proxy CreateDBParameterGroup)

modifyOptionGroupResponseTest :: ModifyOptionGroupResponse -> TestTree
modifyOptionGroupResponseTest = resp
    "ModifyOptionGroup"
    "fixture/RDS/ModifyOptionGroupResponse"
    (Proxy :: Proxy ModifyOptionGroup)

describeEventCategoriesResponseTest :: DescribeEventCategoriesResponse -> TestTree
describeEventCategoriesResponseTest = resp
    "DescribeEventCategories"
    "fixture/RDS/DescribeEventCategoriesResponse"
    (Proxy :: Proxy DescribeEventCategories)

describePendingMaintenanceActionsResponseTest :: DescribePendingMaintenanceActionsResponse -> TestTree
describePendingMaintenanceActionsResponseTest = resp
    "DescribePendingMaintenanceActions"
    "fixture/RDS/DescribePendingMaintenanceActionsResponse"
    (Proxy :: Proxy DescribePendingMaintenanceActions)

restoreDBInstanceToPointInTimeResponseTest :: RestoreDBInstanceToPointInTimeResponse -> TestTree
restoreDBInstanceToPointInTimeResponseTest = resp
    "RestoreDBInstanceToPointInTime"
    "fixture/RDS/RestoreDBInstanceToPointInTimeResponse"
    (Proxy :: Proxy RestoreDBInstanceToPointInTime)

dbParameterGroupNameMessageTest :: DBParameterGroupNameMessage -> TestTree
dbParameterGroupNameMessageTest = resp
    "ResetDBParameterGroup"
    "fixture/RDS/DBParameterGroupNameMessage"
    (Proxy :: Proxy ResetDBParameterGroup)

dbParameterGroupNameMessageTest :: DBParameterGroupNameMessage -> TestTree
dbParameterGroupNameMessageTest = resp
    "ModifyDBParameterGroup"
    "fixture/RDS/DBParameterGroupNameMessage"
    (Proxy :: Proxy ModifyDBParameterGroup)

createOptionGroupResponseTest :: CreateOptionGroupResponse -> TestTree
createOptionGroupResponseTest = resp
    "CreateOptionGroup"
    "fixture/RDS/CreateOptionGroupResponse"
    (Proxy :: Proxy CreateOptionGroup)

applyPendingMaintenanceActionResponseTest :: ApplyPendingMaintenanceActionResponse -> TestTree
applyPendingMaintenanceActionResponseTest = resp
    "ApplyPendingMaintenanceAction"
    "fixture/RDS/ApplyPendingMaintenanceActionResponse"
    (Proxy :: Proxy ApplyPendingMaintenanceAction)

revokeDBSecurityGroupIngressResponseTest :: RevokeDBSecurityGroupIngressResponse -> TestTree
revokeDBSecurityGroupIngressResponseTest = resp
    "RevokeDBSecurityGroupIngress"
    "fixture/RDS/RevokeDBSecurityGroupIngressResponse"
    (Proxy :: Proxy RevokeDBSecurityGroupIngress)

deleteDBSnapshotResponseTest :: DeleteDBSnapshotResponse -> TestTree
deleteDBSnapshotResponseTest = resp
    "DeleteDBSnapshot"
    "fixture/RDS/DeleteDBSnapshotResponse"
    (Proxy :: Proxy DeleteDBSnapshot)

createDBSecurityGroupResponseTest :: CreateDBSecurityGroupResponse -> TestTree
createDBSecurityGroupResponseTest = resp
    "CreateDBSecurityGroup"
    "fixture/RDS/CreateDBSecurityGroupResponse"
    (Proxy :: Proxy CreateDBSecurityGroup)

deleteDBSubnetGroupResponseTest :: DeleteDBSubnetGroupResponse -> TestTree
deleteDBSubnetGroupResponseTest = resp
    "DeleteDBSubnetGroup"
    "fixture/RDS/DeleteDBSubnetGroupResponse"
    (Proxy :: Proxy DeleteDBSubnetGroup)

describeAccountAttributesResponseTest :: DescribeAccountAttributesResponse -> TestTree
describeAccountAttributesResponseTest = resp
    "DescribeAccountAttributes"
    "fixture/RDS/DescribeAccountAttributesResponse"
    (Proxy :: Proxy DescribeAccountAttributes)

deleteDBSecurityGroupResponseTest :: DeleteDBSecurityGroupResponse -> TestTree
deleteDBSecurityGroupResponseTest = resp
    "DeleteDBSecurityGroup"
    "fixture/RDS/DeleteDBSecurityGroupResponse"
    (Proxy :: Proxy DeleteDBSecurityGroup)

rebootDBInstanceResponseTest :: RebootDBInstanceResponse -> TestTree
rebootDBInstanceResponseTest = resp
    "RebootDBInstance"
    "fixture/RDS/RebootDBInstanceResponse"
    (Proxy :: Proxy RebootDBInstance)

createDBSubnetGroupResponseTest :: CreateDBSubnetGroupResponse -> TestTree
createDBSubnetGroupResponseTest = resp
    "CreateDBSubnetGroup"
    "fixture/RDS/CreateDBSubnetGroupResponse"
    (Proxy :: Proxy CreateDBSubnetGroup)

describeReservedDBInstancesOfferingsResponseTest :: DescribeReservedDBInstancesOfferingsResponse -> TestTree
describeReservedDBInstancesOfferingsResponseTest = resp
    "DescribeReservedDBInstancesOfferings"
    "fixture/RDS/DescribeReservedDBInstancesOfferingsResponse"
    (Proxy :: Proxy DescribeReservedDBInstancesOfferings)

deleteDBInstanceResponseTest :: DeleteDBInstanceResponse -> TestTree
deleteDBInstanceResponseTest = resp
    "DeleteDBInstance"
    "fixture/RDS/DeleteDBInstanceResponse"
    (Proxy :: Proxy DeleteDBInstance)

describeDBInstancesResponseTest :: DescribeDBInstancesResponse -> TestTree
describeDBInstancesResponseTest = resp
    "DescribeDBInstances"
    "fixture/RDS/DescribeDBInstancesResponse"
    (Proxy :: Proxy DescribeDBInstances)

copyOptionGroupResponseTest :: CopyOptionGroupResponse -> TestTree
copyOptionGroupResponseTest = resp
    "CopyOptionGroup"
    "fixture/RDS/CopyOptionGroupResponse"
    (Proxy :: Proxy CopyOptionGroup)

downloadDBLogFilePortionResponseTest :: DownloadDBLogFilePortionResponse -> TestTree
downloadDBLogFilePortionResponseTest = resp
    "DownloadDBLogFilePortion"
    "fixture/RDS/DownloadDBLogFilePortionResponse"
    (Proxy :: Proxy DownloadDBLogFilePortion)

createDBInstanceReadReplicaResponseTest :: CreateDBInstanceReadReplicaResponse -> TestTree
createDBInstanceReadReplicaResponseTest = resp
    "CreateDBInstanceReadReplica"
    "fixture/RDS/CreateDBInstanceReadReplicaResponse"
    (Proxy :: Proxy CreateDBInstanceReadReplica)

deleteDBParameterGroupResponseTest :: DeleteDBParameterGroupResponse -> TestTree
deleteDBParameterGroupResponseTest = resp
    "DeleteDBParameterGroup"
    "fixture/RDS/DeleteDBParameterGroupResponse"
    (Proxy :: Proxy DeleteDBParameterGroup)

describeDBSecurityGroupsResponseTest :: DescribeDBSecurityGroupsResponse -> TestTree
describeDBSecurityGroupsResponseTest = resp
    "DescribeDBSecurityGroups"
    "fixture/RDS/DescribeDBSecurityGroupsResponse"
    (Proxy :: Proxy DescribeDBSecurityGroups)
