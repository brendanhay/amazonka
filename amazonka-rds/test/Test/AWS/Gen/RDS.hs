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

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.RDS

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ addSourceIdentifierToSubscriptionTest $
--             addSourceIdentifierToSubscription
--
--         , addTagsToResourceTest $
--             addTagsToResource
--
--         , applyPendingMaintenanceActionTest $
--             applyPendingMaintenanceAction
--
--         , authorizeDBSecurityGroupIngressTest $
--             authorizeDBSecurityGroupIngress
--
--         , copyDBParameterGroupTest $
--             copyDBParameterGroup
--
--         , copyDBSnapshotTest $
--             copyDBSnapshot
--
--         , copyOptionGroupTest $
--             copyOptionGroup
--
--         , createDBInstanceTest $
--             createDBInstance
--
--         , createDBInstanceReadReplicaTest $
--             createDBInstanceReadReplica
--
--         , createDBParameterGroupTest $
--             createDBParameterGroup
--
--         , createDBSecurityGroupTest $
--             createDBSecurityGroup
--
--         , createDBSnapshotTest $
--             createDBSnapshot
--
--         , createDBSubnetGroupTest $
--             createDBSubnetGroup
--
--         , createEventSubscriptionTest $
--             createEventSubscription
--
--         , createOptionGroupTest $
--             createOptionGroup
--
--         , deleteDBInstanceTest $
--             deleteDBInstance
--
--         , deleteDBParameterGroupTest $
--             deleteDBParameterGroup
--
--         , deleteDBSecurityGroupTest $
--             deleteDBSecurityGroup
--
--         , deleteDBSnapshotTest $
--             deleteDBSnapshot
--
--         , deleteDBSubnetGroupTest $
--             deleteDBSubnetGroup
--
--         , deleteEventSubscriptionTest $
--             deleteEventSubscription
--
--         , deleteOptionGroupTest $
--             deleteOptionGroup
--
--         , describeAccountAttributesTest $
--             describeAccountAttributes
--
--         , describeCertificatesTest $
--             describeCertificates
--
--         , describeDBEngineVersionsTest $
--             describeDBEngineVersions
--
--         , describeDBInstancesTest $
--             describeDBInstances
--
--         , describeDBLogFilesTest $
--             describeDBLogFiles
--
--         , describeDBParameterGroupsTest $
--             describeDBParameterGroups
--
--         , describeDBParametersTest $
--             describeDBParameters
--
--         , describeDBSecurityGroupsTest $
--             describeDBSecurityGroups
--
--         , describeDBSnapshotsTest $
--             describeDBSnapshots
--
--         , describeDBSubnetGroupsTest $
--             describeDBSubnetGroups
--
--         , describeEngineDefaultParametersTest $
--             describeEngineDefaultParameters
--
--         , describeEventCategoriesTest $
--             describeEventCategories
--
--         , describeEventSubscriptionsTest $
--             describeEventSubscriptions
--
--         , describeEventsTest $
--             describeEvents
--
--         , describeOptionGroupOptionsTest $
--             describeOptionGroupOptions
--
--         , describeOptionGroupsTest $
--             describeOptionGroups
--
--         , describeOrderableDBInstanceOptionsTest $
--             describeOrderableDBInstanceOptions
--
--         , describePendingMaintenanceActionsTest $
--             describePendingMaintenanceActions
--
--         , describeReservedDBInstancesTest $
--             describeReservedDBInstances
--
--         , describeReservedDBInstancesOfferingsTest $
--             describeReservedDBInstancesOfferings
--
--         , downloadDBLogFilePortionTest $
--             downloadDBLogFilePortion
--
--         , listTagsForResourceTest $
--             listTagsForResource
--
--         , modifyDBInstanceTest $
--             modifyDBInstance
--
--         , modifyDBParameterGroupTest $
--             modifyDBParameterGroup
--
--         , modifyDBSubnetGroupTest $
--             modifyDBSubnetGroup
--
--         , modifyEventSubscriptionTest $
--             modifyEventSubscription
--
--         , modifyOptionGroupTest $
--             modifyOptionGroup
--
--         , promoteReadReplicaTest $
--             promoteReadReplica
--
--         , purchaseReservedDBInstancesOfferingTest $
--             purchaseReservedDBInstancesOffering
--
--         , rebootDBInstanceTest $
--             rebootDBInstance
--
--         , removeSourceIdentifierFromSubscriptionTest $
--             removeSourceIdentifierFromSubscription
--
--         , removeTagsFromResourceTest $
--             removeTagsFromResource
--
--         , resetDBParameterGroupTest $
--             resetDBParameterGroup
--
--         , restoreDBInstanceFromDBSnapshotTest $
--             restoreDBInstanceFromDBSnapshot
--
--         , restoreDBInstanceToPointInTimeTest $
--             restoreDBInstanceToPointInTime
--
--         , revokeDBSecurityGroupIngressTest $
--             revokeDBSecurityGroupIngress
--
--           ]

--     , testGroup "response"
--         [ addSourceIdentifierToSubscriptionResponseTest $
--             addSourceIdentifierToSubscriptionResponse
--
--         , addTagsToResourceResponseTest $
--             addTagsToResourceResponse
--
--         , applyPendingMaintenanceActionResponseTest $
--             applyPendingMaintenanceActionResponse
--
--         , authorizeDBSecurityGroupIngressResponseTest $
--             authorizeDBSecurityGroupIngressResponse
--
--         , copyDBParameterGroupResponseTest $
--             copyDBParameterGroupResponse
--
--         , copyDBSnapshotResponseTest $
--             copyDBSnapshotResponse
--
--         , copyOptionGroupResponseTest $
--             copyOptionGroupResponse
--
--         , createDBInstanceResponseTest $
--             createDBInstanceResponse
--
--         , createDBInstanceReadReplicaResponseTest $
--             createDBInstanceReadReplicaResponse
--
--         , createDBParameterGroupResponseTest $
--             createDBParameterGroupResponse
--
--         , createDBSecurityGroupResponseTest $
--             createDBSecurityGroupResponse
--
--         , createDBSnapshotResponseTest $
--             createDBSnapshotResponse
--
--         , createDBSubnetGroupResponseTest $
--             createDBSubnetGroupResponse
--
--         , createEventSubscriptionResponseTest $
--             createEventSubscriptionResponse
--
--         , createOptionGroupResponseTest $
--             createOptionGroupResponse
--
--         , deleteDBInstanceResponseTest $
--             deleteDBInstanceResponse
--
--         , deleteDBParameterGroupResponseTest $
--             deleteDBParameterGroupResponse
--
--         , deleteDBSecurityGroupResponseTest $
--             deleteDBSecurityGroupResponse
--
--         , deleteDBSnapshotResponseTest $
--             deleteDBSnapshotResponse
--
--         , deleteDBSubnetGroupResponseTest $
--             deleteDBSubnetGroupResponse
--
--         , deleteEventSubscriptionResponseTest $
--             deleteEventSubscriptionResponse
--
--         , deleteOptionGroupResponseTest $
--             deleteOptionGroupResponse
--
--         , describeAccountAttributesResponseTest $
--             describeAccountAttributesResponse
--
--         , describeCertificatesResponseTest $
--             describeCertificatesResponse
--
--         , describeDBEngineVersionsResponseTest $
--             describeDBEngineVersionsResponse
--
--         , describeDBInstancesResponseTest $
--             describeDBInstancesResponse
--
--         , describeDBLogFilesResponseTest $
--             describeDBLogFilesResponse
--
--         , describeDBParameterGroupsResponseTest $
--             describeDBParameterGroupsResponse
--
--         , describeDBParametersResponseTest $
--             describeDBParametersResponse
--
--         , describeDBSecurityGroupsResponseTest $
--             describeDBSecurityGroupsResponse
--
--         , describeDBSnapshotsResponseTest $
--             describeDBSnapshotsResponse
--
--         , describeDBSubnetGroupsResponseTest $
--             describeDBSubnetGroupsResponse
--
--         , describeEngineDefaultParametersResponseTest $
--             describeEngineDefaultParametersResponse
--
--         , describeEventCategoriesResponseTest $
--             describeEventCategoriesResponse
--
--         , describeEventSubscriptionsResponseTest $
--             describeEventSubscriptionsResponse
--
--         , describeEventsResponseTest $
--             describeEventsResponse
--
--         , describeOptionGroupOptionsResponseTest $
--             describeOptionGroupOptionsResponse
--
--         , describeOptionGroupsResponseTest $
--             describeOptionGroupsResponse
--
--         , describeOrderableDBInstanceOptionsResponseTest $
--             describeOrderableDBInstanceOptionsResponse
--
--         , describePendingMaintenanceActionsResponseTest $
--             describePendingMaintenanceActionsResponse
--
--         , describeReservedDBInstancesResponseTest $
--             describeReservedDBInstancesResponse
--
--         , describeReservedDBInstancesOfferingsResponseTest $
--             describeReservedDBInstancesOfferingsResponse
--
--         , downloadDBLogFilePortionResponseTest $
--             downloadDBLogFilePortionResponse
--
--         , listTagsForResourceResponseTest $
--             listTagsForResourceResponse
--
--         , modifyDBInstanceResponseTest $
--             modifyDBInstanceResponse
--
--         , modifyDBParameterGroupResponseTest $
--             dbParameterGroupNameMessage
--
--         , modifyDBSubnetGroupResponseTest $
--             modifyDBSubnetGroupResponse
--
--         , modifyEventSubscriptionResponseTest $
--             modifyEventSubscriptionResponse
--
--         , modifyOptionGroupResponseTest $
--             modifyOptionGroupResponse
--
--         , promoteReadReplicaResponseTest $
--             promoteReadReplicaResponse
--
--         , purchaseReservedDBInstancesOfferingResponseTest $
--             purchaseReservedDBInstancesOfferingResponse
--
--         , rebootDBInstanceResponseTest $
--             rebootDBInstanceResponse
--
--         , removeSourceIdentifierFromSubscriptionResponseTest $
--             removeSourceIdentifierFromSubscriptionResponse
--
--         , removeTagsFromResourceResponseTest $
--             removeTagsFromResourceResponse
--
--         , resetDBParameterGroupResponseTest $
--             dbParameterGroupNameMessage
--
--         , restoreDBInstanceFromDBSnapshotResponseTest $
--             restoreDBInstanceFromDBSnapshotResponse
--
--         , restoreDBInstanceToPointInTimeResponseTest $
--             restoreDBInstanceToPointInTimeResponse
--
--         , revokeDBSecurityGroupIngressResponseTest $
--             revokeDBSecurityGroupIngressResponse
--
--           ]
--     ]

-- Requests

addSourceIdentifierToSubscriptionTest :: AddSourceIdentifierToSubscription -> TestTree
addSourceIdentifierToSubscriptionTest = undefined

addTagsToResourceTest :: AddTagsToResource -> TestTree
addTagsToResourceTest = undefined

applyPendingMaintenanceActionTest :: ApplyPendingMaintenanceAction -> TestTree
applyPendingMaintenanceActionTest = undefined

authorizeDBSecurityGroupIngressTest :: AuthorizeDBSecurityGroupIngress -> TestTree
authorizeDBSecurityGroupIngressTest = undefined

copyDBParameterGroupTest :: CopyDBParameterGroup -> TestTree
copyDBParameterGroupTest = undefined

copyDBSnapshotTest :: CopyDBSnapshot -> TestTree
copyDBSnapshotTest = undefined

copyOptionGroupTest :: CopyOptionGroup -> TestTree
copyOptionGroupTest = undefined

createDBInstanceTest :: CreateDBInstance -> TestTree
createDBInstanceTest = undefined

createDBInstanceReadReplicaTest :: CreateDBInstanceReadReplica -> TestTree
createDBInstanceReadReplicaTest = undefined

createDBParameterGroupTest :: CreateDBParameterGroup -> TestTree
createDBParameterGroupTest = undefined

createDBSecurityGroupTest :: CreateDBSecurityGroup -> TestTree
createDBSecurityGroupTest = undefined

createDBSnapshotTest :: CreateDBSnapshot -> TestTree
createDBSnapshotTest = undefined

createDBSubnetGroupTest :: CreateDBSubnetGroup -> TestTree
createDBSubnetGroupTest = undefined

createEventSubscriptionTest :: CreateEventSubscription -> TestTree
createEventSubscriptionTest = undefined

createOptionGroupTest :: CreateOptionGroup -> TestTree
createOptionGroupTest = undefined

deleteDBInstanceTest :: DeleteDBInstance -> TestTree
deleteDBInstanceTest = undefined

deleteDBParameterGroupTest :: DeleteDBParameterGroup -> TestTree
deleteDBParameterGroupTest = undefined

deleteDBSecurityGroupTest :: DeleteDBSecurityGroup -> TestTree
deleteDBSecurityGroupTest = undefined

deleteDBSnapshotTest :: DeleteDBSnapshot -> TestTree
deleteDBSnapshotTest = undefined

deleteDBSubnetGroupTest :: DeleteDBSubnetGroup -> TestTree
deleteDBSubnetGroupTest = undefined

deleteEventSubscriptionTest :: DeleteEventSubscription -> TestTree
deleteEventSubscriptionTest = undefined

deleteOptionGroupTest :: DeleteOptionGroup -> TestTree
deleteOptionGroupTest = undefined

describeAccountAttributesTest :: DescribeAccountAttributes -> TestTree
describeAccountAttributesTest = undefined

describeCertificatesTest :: DescribeCertificates -> TestTree
describeCertificatesTest = undefined

describeDBEngineVersionsTest :: DescribeDBEngineVersions -> TestTree
describeDBEngineVersionsTest = undefined

describeDBInstancesTest :: DescribeDBInstances -> TestTree
describeDBInstancesTest = undefined

describeDBLogFilesTest :: DescribeDBLogFiles -> TestTree
describeDBLogFilesTest = undefined

describeDBParameterGroupsTest :: DescribeDBParameterGroups -> TestTree
describeDBParameterGroupsTest = undefined

describeDBParametersTest :: DescribeDBParameters -> TestTree
describeDBParametersTest = undefined

describeDBSecurityGroupsTest :: DescribeDBSecurityGroups -> TestTree
describeDBSecurityGroupsTest = undefined

describeDBSnapshotsTest :: DescribeDBSnapshots -> TestTree
describeDBSnapshotsTest = undefined

describeDBSubnetGroupsTest :: DescribeDBSubnetGroups -> TestTree
describeDBSubnetGroupsTest = undefined

describeEngineDefaultParametersTest :: DescribeEngineDefaultParameters -> TestTree
describeEngineDefaultParametersTest = undefined

describeEventCategoriesTest :: DescribeEventCategories -> TestTree
describeEventCategoriesTest = undefined

describeEventSubscriptionsTest :: DescribeEventSubscriptions -> TestTree
describeEventSubscriptionsTest = undefined

describeEventsTest :: DescribeEvents -> TestTree
describeEventsTest = undefined

describeOptionGroupOptionsTest :: DescribeOptionGroupOptions -> TestTree
describeOptionGroupOptionsTest = undefined

describeOptionGroupsTest :: DescribeOptionGroups -> TestTree
describeOptionGroupsTest = undefined

describeOrderableDBInstanceOptionsTest :: DescribeOrderableDBInstanceOptions -> TestTree
describeOrderableDBInstanceOptionsTest = undefined

describePendingMaintenanceActionsTest :: DescribePendingMaintenanceActions -> TestTree
describePendingMaintenanceActionsTest = undefined

describeReservedDBInstancesTest :: DescribeReservedDBInstances -> TestTree
describeReservedDBInstancesTest = undefined

describeReservedDBInstancesOfferingsTest :: DescribeReservedDBInstancesOfferings -> TestTree
describeReservedDBInstancesOfferingsTest = undefined

downloadDBLogFilePortionTest :: DownloadDBLogFilePortion -> TestTree
downloadDBLogFilePortionTest = undefined

listTagsForResourceTest :: ListTagsForResource -> TestTree
listTagsForResourceTest = undefined

modifyDBInstanceTest :: ModifyDBInstance -> TestTree
modifyDBInstanceTest = undefined

modifyDBParameterGroupTest :: ModifyDBParameterGroup -> TestTree
modifyDBParameterGroupTest = undefined

modifyDBSubnetGroupTest :: ModifyDBSubnetGroup -> TestTree
modifyDBSubnetGroupTest = undefined

modifyEventSubscriptionTest :: ModifyEventSubscription -> TestTree
modifyEventSubscriptionTest = undefined

modifyOptionGroupTest :: ModifyOptionGroup -> TestTree
modifyOptionGroupTest = undefined

promoteReadReplicaTest :: PromoteReadReplica -> TestTree
promoteReadReplicaTest = undefined

purchaseReservedDBInstancesOfferingTest :: PurchaseReservedDBInstancesOffering -> TestTree
purchaseReservedDBInstancesOfferingTest = undefined

rebootDBInstanceTest :: RebootDBInstance -> TestTree
rebootDBInstanceTest = undefined

removeSourceIdentifierFromSubscriptionTest :: RemoveSourceIdentifierFromSubscription -> TestTree
removeSourceIdentifierFromSubscriptionTest = undefined

removeTagsFromResourceTest :: RemoveTagsFromResource -> TestTree
removeTagsFromResourceTest = undefined

resetDBParameterGroupTest :: ResetDBParameterGroup -> TestTree
resetDBParameterGroupTest = undefined

restoreDBInstanceFromDBSnapshotTest :: RestoreDBInstanceFromDBSnapshot -> TestTree
restoreDBInstanceFromDBSnapshotTest = undefined

restoreDBInstanceToPointInTimeTest :: RestoreDBInstanceToPointInTime -> TestTree
restoreDBInstanceToPointInTimeTest = undefined

revokeDBSecurityGroupIngressTest :: RevokeDBSecurityGroupIngress -> TestTree
revokeDBSecurityGroupIngressTest = undefined

-- Responses

addSourceIdentifierToSubscriptionResponseTest :: AddSourceIdentifierToSubscriptionResponse -> TestTree
addSourceIdentifierToSubscriptionResponseTest = resp
    "addSourceIdentifierToSubscriptionResponse"
    "fixture/AddSourceIdentifierToSubscriptionResponse"
    (Proxy :: Proxy AddSourceIdentifierToSubscription)

addTagsToResourceResponseTest :: AddTagsToResourceResponse -> TestTree
addTagsToResourceResponseTest = resp
    "addTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse"
    (Proxy :: Proxy AddTagsToResource)

applyPendingMaintenanceActionResponseTest :: ApplyPendingMaintenanceActionResponse -> TestTree
applyPendingMaintenanceActionResponseTest = resp
    "applyPendingMaintenanceActionResponse"
    "fixture/ApplyPendingMaintenanceActionResponse"
    (Proxy :: Proxy ApplyPendingMaintenanceAction)

authorizeDBSecurityGroupIngressResponseTest :: AuthorizeDBSecurityGroupIngressResponse -> TestTree
authorizeDBSecurityGroupIngressResponseTest = resp
    "authorizeDBSecurityGroupIngressResponse"
    "fixture/AuthorizeDBSecurityGroupIngressResponse"
    (Proxy :: Proxy AuthorizeDBSecurityGroupIngress)

copyDBParameterGroupResponseTest :: CopyDBParameterGroupResponse -> TestTree
copyDBParameterGroupResponseTest = resp
    "copyDBParameterGroupResponse"
    "fixture/CopyDBParameterGroupResponse"
    (Proxy :: Proxy CopyDBParameterGroup)

copyDBSnapshotResponseTest :: CopyDBSnapshotResponse -> TestTree
copyDBSnapshotResponseTest = resp
    "copyDBSnapshotResponse"
    "fixture/CopyDBSnapshotResponse"
    (Proxy :: Proxy CopyDBSnapshot)

copyOptionGroupResponseTest :: CopyOptionGroupResponse -> TestTree
copyOptionGroupResponseTest = resp
    "copyOptionGroupResponse"
    "fixture/CopyOptionGroupResponse"
    (Proxy :: Proxy CopyOptionGroup)

createDBInstanceResponseTest :: CreateDBInstanceResponse -> TestTree
createDBInstanceResponseTest = resp
    "createDBInstanceResponse"
    "fixture/CreateDBInstanceResponse"
    (Proxy :: Proxy CreateDBInstance)

createDBInstanceReadReplicaResponseTest :: CreateDBInstanceReadReplicaResponse -> TestTree
createDBInstanceReadReplicaResponseTest = resp
    "createDBInstanceReadReplicaResponse"
    "fixture/CreateDBInstanceReadReplicaResponse"
    (Proxy :: Proxy CreateDBInstanceReadReplica)

createDBParameterGroupResponseTest :: CreateDBParameterGroupResponse -> TestTree
createDBParameterGroupResponseTest = resp
    "createDBParameterGroupResponse"
    "fixture/CreateDBParameterGroupResponse"
    (Proxy :: Proxy CreateDBParameterGroup)

createDBSecurityGroupResponseTest :: CreateDBSecurityGroupResponse -> TestTree
createDBSecurityGroupResponseTest = resp
    "createDBSecurityGroupResponse"
    "fixture/CreateDBSecurityGroupResponse"
    (Proxy :: Proxy CreateDBSecurityGroup)

createDBSnapshotResponseTest :: CreateDBSnapshotResponse -> TestTree
createDBSnapshotResponseTest = resp
    "createDBSnapshotResponse"
    "fixture/CreateDBSnapshotResponse"
    (Proxy :: Proxy CreateDBSnapshot)

createDBSubnetGroupResponseTest :: CreateDBSubnetGroupResponse -> TestTree
createDBSubnetGroupResponseTest = resp
    "createDBSubnetGroupResponse"
    "fixture/CreateDBSubnetGroupResponse"
    (Proxy :: Proxy CreateDBSubnetGroup)

createEventSubscriptionResponseTest :: CreateEventSubscriptionResponse -> TestTree
createEventSubscriptionResponseTest = resp
    "createEventSubscriptionResponse"
    "fixture/CreateEventSubscriptionResponse"
    (Proxy :: Proxy CreateEventSubscription)

createOptionGroupResponseTest :: CreateOptionGroupResponse -> TestTree
createOptionGroupResponseTest = resp
    "createOptionGroupResponse"
    "fixture/CreateOptionGroupResponse"
    (Proxy :: Proxy CreateOptionGroup)

deleteDBInstanceResponseTest :: DeleteDBInstanceResponse -> TestTree
deleteDBInstanceResponseTest = resp
    "deleteDBInstanceResponse"
    "fixture/DeleteDBInstanceResponse"
    (Proxy :: Proxy DeleteDBInstance)

deleteDBParameterGroupResponseTest :: DeleteDBParameterGroupResponse -> TestTree
deleteDBParameterGroupResponseTest = resp
    "deleteDBParameterGroupResponse"
    "fixture/DeleteDBParameterGroupResponse"
    (Proxy :: Proxy DeleteDBParameterGroup)

deleteDBSecurityGroupResponseTest :: DeleteDBSecurityGroupResponse -> TestTree
deleteDBSecurityGroupResponseTest = resp
    "deleteDBSecurityGroupResponse"
    "fixture/DeleteDBSecurityGroupResponse"
    (Proxy :: Proxy DeleteDBSecurityGroup)

deleteDBSnapshotResponseTest :: DeleteDBSnapshotResponse -> TestTree
deleteDBSnapshotResponseTest = resp
    "deleteDBSnapshotResponse"
    "fixture/DeleteDBSnapshotResponse"
    (Proxy :: Proxy DeleteDBSnapshot)

deleteDBSubnetGroupResponseTest :: DeleteDBSubnetGroupResponse -> TestTree
deleteDBSubnetGroupResponseTest = resp
    "deleteDBSubnetGroupResponse"
    "fixture/DeleteDBSubnetGroupResponse"
    (Proxy :: Proxy DeleteDBSubnetGroup)

deleteEventSubscriptionResponseTest :: DeleteEventSubscriptionResponse -> TestTree
deleteEventSubscriptionResponseTest = resp
    "deleteEventSubscriptionResponse"
    "fixture/DeleteEventSubscriptionResponse"
    (Proxy :: Proxy DeleteEventSubscription)

deleteOptionGroupResponseTest :: DeleteOptionGroupResponse -> TestTree
deleteOptionGroupResponseTest = resp
    "deleteOptionGroupResponse"
    "fixture/DeleteOptionGroupResponse"
    (Proxy :: Proxy DeleteOptionGroup)

describeAccountAttributesResponseTest :: DescribeAccountAttributesResponse -> TestTree
describeAccountAttributesResponseTest = resp
    "describeAccountAttributesResponse"
    "fixture/DescribeAccountAttributesResponse"
    (Proxy :: Proxy DescribeAccountAttributes)

describeCertificatesResponseTest :: DescribeCertificatesResponse -> TestTree
describeCertificatesResponseTest = resp
    "describeCertificatesResponse"
    "fixture/DescribeCertificatesResponse"
    (Proxy :: Proxy DescribeCertificates)

describeDBEngineVersionsResponseTest :: DescribeDBEngineVersionsResponse -> TestTree
describeDBEngineVersionsResponseTest = resp
    "describeDBEngineVersionsResponse"
    "fixture/DescribeDBEngineVersionsResponse"
    (Proxy :: Proxy DescribeDBEngineVersions)

describeDBInstancesResponseTest :: DescribeDBInstancesResponse -> TestTree
describeDBInstancesResponseTest = resp
    "describeDBInstancesResponse"
    "fixture/DescribeDBInstancesResponse"
    (Proxy :: Proxy DescribeDBInstances)

describeDBLogFilesResponseTest :: DescribeDBLogFilesResponse -> TestTree
describeDBLogFilesResponseTest = resp
    "describeDBLogFilesResponse"
    "fixture/DescribeDBLogFilesResponse"
    (Proxy :: Proxy DescribeDBLogFiles)

describeDBParameterGroupsResponseTest :: DescribeDBParameterGroupsResponse -> TestTree
describeDBParameterGroupsResponseTest = resp
    "describeDBParameterGroupsResponse"
    "fixture/DescribeDBParameterGroupsResponse"
    (Proxy :: Proxy DescribeDBParameterGroups)

describeDBParametersResponseTest :: DescribeDBParametersResponse -> TestTree
describeDBParametersResponseTest = resp
    "describeDBParametersResponse"
    "fixture/DescribeDBParametersResponse"
    (Proxy :: Proxy DescribeDBParameters)

describeDBSecurityGroupsResponseTest :: DescribeDBSecurityGroupsResponse -> TestTree
describeDBSecurityGroupsResponseTest = resp
    "describeDBSecurityGroupsResponse"
    "fixture/DescribeDBSecurityGroupsResponse"
    (Proxy :: Proxy DescribeDBSecurityGroups)

describeDBSnapshotsResponseTest :: DescribeDBSnapshotsResponse -> TestTree
describeDBSnapshotsResponseTest = resp
    "describeDBSnapshotsResponse"
    "fixture/DescribeDBSnapshotsResponse"
    (Proxy :: Proxy DescribeDBSnapshots)

describeDBSubnetGroupsResponseTest :: DescribeDBSubnetGroupsResponse -> TestTree
describeDBSubnetGroupsResponseTest = resp
    "describeDBSubnetGroupsResponse"
    "fixture/DescribeDBSubnetGroupsResponse"
    (Proxy :: Proxy DescribeDBSubnetGroups)

describeEngineDefaultParametersResponseTest :: DescribeEngineDefaultParametersResponse -> TestTree
describeEngineDefaultParametersResponseTest = resp
    "describeEngineDefaultParametersResponse"
    "fixture/DescribeEngineDefaultParametersResponse"
    (Proxy :: Proxy DescribeEngineDefaultParameters)

describeEventCategoriesResponseTest :: DescribeEventCategoriesResponse -> TestTree
describeEventCategoriesResponseTest = resp
    "describeEventCategoriesResponse"
    "fixture/DescribeEventCategoriesResponse"
    (Proxy :: Proxy DescribeEventCategories)

describeEventSubscriptionsResponseTest :: DescribeEventSubscriptionsResponse -> TestTree
describeEventSubscriptionsResponseTest = resp
    "describeEventSubscriptionsResponse"
    "fixture/DescribeEventSubscriptionsResponse"
    (Proxy :: Proxy DescribeEventSubscriptions)

describeEventsResponseTest :: DescribeEventsResponse -> TestTree
describeEventsResponseTest = resp
    "describeEventsResponse"
    "fixture/DescribeEventsResponse"
    (Proxy :: Proxy DescribeEvents)

describeOptionGroupOptionsResponseTest :: DescribeOptionGroupOptionsResponse -> TestTree
describeOptionGroupOptionsResponseTest = resp
    "describeOptionGroupOptionsResponse"
    "fixture/DescribeOptionGroupOptionsResponse"
    (Proxy :: Proxy DescribeOptionGroupOptions)

describeOptionGroupsResponseTest :: DescribeOptionGroupsResponse -> TestTree
describeOptionGroupsResponseTest = resp
    "describeOptionGroupsResponse"
    "fixture/DescribeOptionGroupsResponse"
    (Proxy :: Proxy DescribeOptionGroups)

describeOrderableDBInstanceOptionsResponseTest :: DescribeOrderableDBInstanceOptionsResponse -> TestTree
describeOrderableDBInstanceOptionsResponseTest = resp
    "describeOrderableDBInstanceOptionsResponse"
    "fixture/DescribeOrderableDBInstanceOptionsResponse"
    (Proxy :: Proxy DescribeOrderableDBInstanceOptions)

describePendingMaintenanceActionsResponseTest :: DescribePendingMaintenanceActionsResponse -> TestTree
describePendingMaintenanceActionsResponseTest = resp
    "describePendingMaintenanceActionsResponse"
    "fixture/DescribePendingMaintenanceActionsResponse"
    (Proxy :: Proxy DescribePendingMaintenanceActions)

describeReservedDBInstancesResponseTest :: DescribeReservedDBInstancesResponse -> TestTree
describeReservedDBInstancesResponseTest = resp
    "describeReservedDBInstancesResponse"
    "fixture/DescribeReservedDBInstancesResponse"
    (Proxy :: Proxy DescribeReservedDBInstances)

describeReservedDBInstancesOfferingsResponseTest :: DescribeReservedDBInstancesOfferingsResponse -> TestTree
describeReservedDBInstancesOfferingsResponseTest = resp
    "describeReservedDBInstancesOfferingsResponse"
    "fixture/DescribeReservedDBInstancesOfferingsResponse"
    (Proxy :: Proxy DescribeReservedDBInstancesOfferings)

downloadDBLogFilePortionResponseTest :: DownloadDBLogFilePortionResponse -> TestTree
downloadDBLogFilePortionResponseTest = resp
    "downloadDBLogFilePortionResponse"
    "fixture/DownloadDBLogFilePortionResponse"
    (Proxy :: Proxy DownloadDBLogFilePortion)

listTagsForResourceResponseTest :: ListTagsForResourceResponse -> TestTree
listTagsForResourceResponseTest = resp
    "listTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse"
    (Proxy :: Proxy ListTagsForResource)

modifyDBInstanceResponseTest :: ModifyDBInstanceResponse -> TestTree
modifyDBInstanceResponseTest = resp
    "modifyDBInstanceResponse"
    "fixture/ModifyDBInstanceResponse"
    (Proxy :: Proxy ModifyDBInstance)

modifyDBParameterGroupResponseTest :: DBParameterGroupNameMessage -> TestTree
modifyDBParameterGroupResponseTest = resp
    "modifyDBParameterGroupResponse"
    "fixture/DBParameterGroupNameMessage"
    (Proxy :: Proxy ModifyDBParameterGroup)

modifyDBSubnetGroupResponseTest :: ModifyDBSubnetGroupResponse -> TestTree
modifyDBSubnetGroupResponseTest = resp
    "modifyDBSubnetGroupResponse"
    "fixture/ModifyDBSubnetGroupResponse"
    (Proxy :: Proxy ModifyDBSubnetGroup)

modifyEventSubscriptionResponseTest :: ModifyEventSubscriptionResponse -> TestTree
modifyEventSubscriptionResponseTest = resp
    "modifyEventSubscriptionResponse"
    "fixture/ModifyEventSubscriptionResponse"
    (Proxy :: Proxy ModifyEventSubscription)

modifyOptionGroupResponseTest :: ModifyOptionGroupResponse -> TestTree
modifyOptionGroupResponseTest = resp
    "modifyOptionGroupResponse"
    "fixture/ModifyOptionGroupResponse"
    (Proxy :: Proxy ModifyOptionGroup)

promoteReadReplicaResponseTest :: PromoteReadReplicaResponse -> TestTree
promoteReadReplicaResponseTest = resp
    "promoteReadReplicaResponse"
    "fixture/PromoteReadReplicaResponse"
    (Proxy :: Proxy PromoteReadReplica)

purchaseReservedDBInstancesOfferingResponseTest :: PurchaseReservedDBInstancesOfferingResponse -> TestTree
purchaseReservedDBInstancesOfferingResponseTest = resp
    "purchaseReservedDBInstancesOfferingResponse"
    "fixture/PurchaseReservedDBInstancesOfferingResponse"
    (Proxy :: Proxy PurchaseReservedDBInstancesOffering)

rebootDBInstanceResponseTest :: RebootDBInstanceResponse -> TestTree
rebootDBInstanceResponseTest = resp
    "rebootDBInstanceResponse"
    "fixture/RebootDBInstanceResponse"
    (Proxy :: Proxy RebootDBInstance)

removeSourceIdentifierFromSubscriptionResponseTest :: RemoveSourceIdentifierFromSubscriptionResponse -> TestTree
removeSourceIdentifierFromSubscriptionResponseTest = resp
    "removeSourceIdentifierFromSubscriptionResponse"
    "fixture/RemoveSourceIdentifierFromSubscriptionResponse"
    (Proxy :: Proxy RemoveSourceIdentifierFromSubscription)

removeTagsFromResourceResponseTest :: RemoveTagsFromResourceResponse -> TestTree
removeTagsFromResourceResponseTest = resp
    "removeTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse"
    (Proxy :: Proxy RemoveTagsFromResource)

resetDBParameterGroupResponseTest :: DBParameterGroupNameMessage -> TestTree
resetDBParameterGroupResponseTest = resp
    "resetDBParameterGroupResponse"
    "fixture/DBParameterGroupNameMessage"
    (Proxy :: Proxy ResetDBParameterGroup)

restoreDBInstanceFromDBSnapshotResponseTest :: RestoreDBInstanceFromDBSnapshotResponse -> TestTree
restoreDBInstanceFromDBSnapshotResponseTest = resp
    "restoreDBInstanceFromDBSnapshotResponse"
    "fixture/RestoreDBInstanceFromDBSnapshotResponse"
    (Proxy :: Proxy RestoreDBInstanceFromDBSnapshot)

restoreDBInstanceToPointInTimeResponseTest :: RestoreDBInstanceToPointInTimeResponse -> TestTree
restoreDBInstanceToPointInTimeResponseTest = resp
    "restoreDBInstanceToPointInTimeResponse"
    "fixture/RestoreDBInstanceToPointInTimeResponse"
    (Proxy :: Proxy RestoreDBInstanceToPointInTime)

revokeDBSecurityGroupIngressResponseTest :: RevokeDBSecurityGroupIngressResponse -> TestTree
revokeDBSecurityGroupIngressResponseTest = resp
    "revokeDBSecurityGroupIngressResponse"
    "fixture/RevokeDBSecurityGroupIngressResponse"
    (Proxy :: Proxy RevokeDBSecurityGroupIngress)
