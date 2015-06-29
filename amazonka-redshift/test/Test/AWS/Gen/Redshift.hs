-- Module      : Test.AWS.Gen.Redshift
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

module Test.AWS.Gen.Redshift where

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.Redshift

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ describeClustersTest $
--             describeClusters
--
--         , describeTagsTest $
--             describeTags
--
--         , modifyEventSubscriptionTest $
--             modifyEventSubscription
--
--         , disableLoggingTest $
--             disableLogging
--
--         , purchaseReservedNodeOfferingTest $
--             purchaseReservedNodeOffering
--
--         , deleteClusterSubnetGroupTest $
--             deleteClusterSubnetGroup
--
--         , deleteClusterSnapshotTest $
--             deleteClusterSnapshot
--
--         , describeEventsTest $
--             describeEvents
--
--         , describeReservedNodeOfferingsTest $
--             describeReservedNodeOfferings
--
--         , describeClusterParameterGroupsTest $
--             describeClusterParameterGroups
--
--         , createClusterSubnetGroupTest $
--             createClusterSubnetGroup
--
--         , describeReservedNodesTest $
--             describeReservedNodes
--
--         , enableLoggingTest $
--             enableLogging
--
--         , createTagsTest $
--             createTags
--
--         , describeClusterSecurityGroupsTest $
--             describeClusterSecurityGroups
--
--         , deleteClusterParameterGroupTest $
--             deleteClusterParameterGroup
--
--         , deleteTagsTest $
--             deleteTags
--
--         , enableSnapshotCopyTest $
--             enableSnapshotCopy
--
--         , modifySnapshotCopyRetentionPeriodTest $
--             modifySnapshotCopyRetentionPeriod
--
--         , describeClusterSnapshotsTest $
--             describeClusterSnapshots
--
--         , describeClusterSubnetGroupsTest $
--             describeClusterSubnetGroups
--
--         , authorizeSnapshotAccessTest $
--             authorizeSnapshotAccess
--
--         , createEventSubscriptionTest $
--             createEventSubscription
--
--         , rebootClusterTest $
--             rebootCluster
--
--         , describeOrderableClusterOptionsTest $
--             describeOrderableClusterOptions
--
--         , deleteClusterTest $
--             deleteCluster
--
--         , deleteEventSubscriptionTest $
--             deleteEventSubscription
--
--         , describeDefaultClusterParametersTest $
--             describeDefaultClusterParameters
--
--         , createClusterTest $
--             createCluster
--
--         , createHSMClientCertificateTest $
--             createHSMClientCertificate
--
--         , resetClusterParameterGroupTest $
--             resetClusterParameterGroup
--
--         , describeEventSubscriptionsTest $
--             describeEventSubscriptions
--
--         , describeHSMClientCertificatesTest $
--             describeHSMClientCertificates
--
--         , modifyClusterParameterGroupTest $
--             modifyClusterParameterGroup
--
--         , revokeClusterSecurityGroupIngressTest $
--             revokeClusterSecurityGroupIngress
--
--         , authorizeClusterSecurityGroupIngressTest $
--             authorizeClusterSecurityGroupIngress
--
--         , createClusterSecurityGroupTest $
--             createClusterSecurityGroup
--
--         , describeResizeTest $
--             describeResize
--
--         , describeEventCategoriesTest $
--             describeEventCategories
--
--         , deleteHSMConfigurationTest $
--             deleteHSMConfiguration
--
--         , deleteClusterSecurityGroupTest $
--             deleteClusterSecurityGroup
--
--         , createHSMConfigurationTest $
--             createHSMConfiguration
--
--         , modifyClusterTest $
--             modifyCluster
--
--         , createClusterSnapshotTest $
--             createClusterSnapshot
--
--         , describeLoggingStatusTest $
--             describeLoggingStatus
--
--         , describeClusterParametersTest $
--             describeClusterParameters
--
--         , disableSnapshotCopyTest $
--             disableSnapshotCopy
--
--         , restoreFromClusterSnapshotTest $
--             restoreFromClusterSnapshot
--
--         , describeHSMConfigurationsTest $
--             describeHSMConfigurations
--
--         , createClusterParameterGroupTest $
--             createClusterParameterGroup
--
--         , revokeSnapshotAccessTest $
--             revokeSnapshotAccess
--
--         , deleteHSMClientCertificateTest $
--             deleteHSMClientCertificate
--
--         , createSnapshotCopyGrantTest $
--             createSnapshotCopyGrant
--
--         , copyClusterSnapshotTest $
--             copyClusterSnapshot
--
--         , describeClusterVersionsTest $
--             describeClusterVersions
--
--         , modifyClusterSubnetGroupTest $
--             modifyClusterSubnetGroup
--
--         , deleteSnapshotCopyGrantTest $
--             deleteSnapshotCopyGrant
--
--         , describeSnapshotCopyGrantsTest $
--             describeSnapshotCopyGrants
--
--         , rotateEncryptionKeyTest $
--             rotateEncryptionKey
--
--           ]

--     , testGroup "response"
--         [ describeClustersResponseTest $
--             describeClustersResponse
--
--         , describeTagsResponseTest $
--             describeTagsResponse
--
--         , modifyEventSubscriptionResponseTest $
--             modifyEventSubscriptionResponse
--
--         , loggingStatusTest $
--             loggingStatus
--
--         , purchaseReservedNodeOfferingResponseTest $
--             purchaseReservedNodeOfferingResponse
--
--         , deleteClusterSubnetGroupResponseTest $
--             deleteClusterSubnetGroupResponse
--
--         , deleteClusterSnapshotResponseTest $
--             deleteClusterSnapshotResponse
--
--         , describeEventsResponseTest $
--             describeEventsResponse
--
--         , describeReservedNodeOfferingsResponseTest $
--             describeReservedNodeOfferingsResponse
--
--         , describeClusterParameterGroupsResponseTest $
--             describeClusterParameterGroupsResponse
--
--         , createClusterSubnetGroupResponseTest $
--             createClusterSubnetGroupResponse
--
--         , describeReservedNodesResponseTest $
--             describeReservedNodesResponse
--
--         , loggingStatusTest $
--             loggingStatus
--
--         , createTagsResponseTest $
--             createTagsResponse
--
--         , describeClusterSecurityGroupsResponseTest $
--             describeClusterSecurityGroupsResponse
--
--         , deleteClusterParameterGroupResponseTest $
--             deleteClusterParameterGroupResponse
--
--         , deleteTagsResponseTest $
--             deleteTagsResponse
--
--         , enableSnapshotCopyResponseTest $
--             enableSnapshotCopyResponse
--
--         , modifySnapshotCopyRetentionPeriodResponseTest $
--             modifySnapshotCopyRetentionPeriodResponse
--
--         , describeClusterSnapshotsResponseTest $
--             describeClusterSnapshotsResponse
--
--         , describeClusterSubnetGroupsResponseTest $
--             describeClusterSubnetGroupsResponse
--
--         , authorizeSnapshotAccessResponseTest $
--             authorizeSnapshotAccessResponse
--
--         , createEventSubscriptionResponseTest $
--             createEventSubscriptionResponse
--
--         , rebootClusterResponseTest $
--             rebootClusterResponse
--
--         , describeOrderableClusterOptionsResponseTest $
--             describeOrderableClusterOptionsResponse
--
--         , deleteClusterResponseTest $
--             deleteClusterResponse
--
--         , deleteEventSubscriptionResponseTest $
--             deleteEventSubscriptionResponse
--
--         , describeDefaultClusterParametersResponseTest $
--             describeDefaultClusterParametersResponse
--
--         , createClusterResponseTest $
--             createClusterResponse
--
--         , createHSMClientCertificateResponseTest $
--             createHSMClientCertificateResponse
--
--         , clusterParameterGroupNameMessageTest $
--             clusterParameterGroupNameMessage
--
--         , describeEventSubscriptionsResponseTest $
--             describeEventSubscriptionsResponse
--
--         , describeHSMClientCertificatesResponseTest $
--             describeHSMClientCertificatesResponse
--
--         , clusterParameterGroupNameMessageTest $
--             clusterParameterGroupNameMessage
--
--         , revokeClusterSecurityGroupIngressResponseTest $
--             revokeClusterSecurityGroupIngressResponse
--
--         , authorizeClusterSecurityGroupIngressResponseTest $
--             authorizeClusterSecurityGroupIngressResponse
--
--         , createClusterSecurityGroupResponseTest $
--             createClusterSecurityGroupResponse
--
--         , describeResizeResponseTest $
--             describeResizeResponse
--
--         , describeEventCategoriesResponseTest $
--             describeEventCategoriesResponse
--
--         , deleteHSMConfigurationResponseTest $
--             deleteHSMConfigurationResponse
--
--         , deleteClusterSecurityGroupResponseTest $
--             deleteClusterSecurityGroupResponse
--
--         , createHSMConfigurationResponseTest $
--             createHSMConfigurationResponse
--
--         , modifyClusterResponseTest $
--             modifyClusterResponse
--
--         , createClusterSnapshotResponseTest $
--             createClusterSnapshotResponse
--
--         , loggingStatusTest $
--             loggingStatus
--
--         , describeClusterParametersResponseTest $
--             describeClusterParametersResponse
--
--         , disableSnapshotCopyResponseTest $
--             disableSnapshotCopyResponse
--
--         , restoreFromClusterSnapshotResponseTest $
--             restoreFromClusterSnapshotResponse
--
--         , describeHSMConfigurationsResponseTest $
--             describeHSMConfigurationsResponse
--
--         , createClusterParameterGroupResponseTest $
--             createClusterParameterGroupResponse
--
--         , revokeSnapshotAccessResponseTest $
--             revokeSnapshotAccessResponse
--
--         , deleteHSMClientCertificateResponseTest $
--             deleteHSMClientCertificateResponse
--
--         , createSnapshotCopyGrantResponseTest $
--             createSnapshotCopyGrantResponse
--
--         , copyClusterSnapshotResponseTest $
--             copyClusterSnapshotResponse
--
--         , describeClusterVersionsResponseTest $
--             describeClusterVersionsResponse
--
--         , modifyClusterSubnetGroupResponseTest $
--             modifyClusterSubnetGroupResponse
--
--         , deleteSnapshotCopyGrantResponseTest $
--             deleteSnapshotCopyGrantResponse
--
--         , describeSnapshotCopyGrantsResponseTest $
--             describeSnapshotCopyGrantsResponse
--
--         , rotateEncryptionKeyResponseTest $
--             rotateEncryptionKeyResponse
--
--           ]
--     ]

-- Requests

describeClustersTest :: DescribeClusters -> TestTree
describeClustersTest = undefined

describeTagsTest :: DescribeTags -> TestTree
describeTagsTest = undefined

modifyEventSubscriptionTest :: ModifyEventSubscription -> TestTree
modifyEventSubscriptionTest = undefined

disableLoggingTest :: DisableLogging -> TestTree
disableLoggingTest = undefined

purchaseReservedNodeOfferingTest :: PurchaseReservedNodeOffering -> TestTree
purchaseReservedNodeOfferingTest = undefined

deleteClusterSubnetGroupTest :: DeleteClusterSubnetGroup -> TestTree
deleteClusterSubnetGroupTest = undefined

deleteClusterSnapshotTest :: DeleteClusterSnapshot -> TestTree
deleteClusterSnapshotTest = undefined

describeEventsTest :: DescribeEvents -> TestTree
describeEventsTest = undefined

describeReservedNodeOfferingsTest :: DescribeReservedNodeOfferings -> TestTree
describeReservedNodeOfferingsTest = undefined

describeClusterParameterGroupsTest :: DescribeClusterParameterGroups -> TestTree
describeClusterParameterGroupsTest = undefined

createClusterSubnetGroupTest :: CreateClusterSubnetGroup -> TestTree
createClusterSubnetGroupTest = undefined

describeReservedNodesTest :: DescribeReservedNodes -> TestTree
describeReservedNodesTest = undefined

enableLoggingTest :: EnableLogging -> TestTree
enableLoggingTest = undefined

createTagsTest :: CreateTags -> TestTree
createTagsTest = undefined

describeClusterSecurityGroupsTest :: DescribeClusterSecurityGroups -> TestTree
describeClusterSecurityGroupsTest = undefined

deleteClusterParameterGroupTest :: DeleteClusterParameterGroup -> TestTree
deleteClusterParameterGroupTest = undefined

deleteTagsTest :: DeleteTags -> TestTree
deleteTagsTest = undefined

enableSnapshotCopyTest :: EnableSnapshotCopy -> TestTree
enableSnapshotCopyTest = undefined

modifySnapshotCopyRetentionPeriodTest :: ModifySnapshotCopyRetentionPeriod -> TestTree
modifySnapshotCopyRetentionPeriodTest = undefined

describeClusterSnapshotsTest :: DescribeClusterSnapshots -> TestTree
describeClusterSnapshotsTest = undefined

describeClusterSubnetGroupsTest :: DescribeClusterSubnetGroups -> TestTree
describeClusterSubnetGroupsTest = undefined

authorizeSnapshotAccessTest :: AuthorizeSnapshotAccess -> TestTree
authorizeSnapshotAccessTest = undefined

createEventSubscriptionTest :: CreateEventSubscription -> TestTree
createEventSubscriptionTest = undefined

rebootClusterTest :: RebootCluster -> TestTree
rebootClusterTest = undefined

describeOrderableClusterOptionsTest :: DescribeOrderableClusterOptions -> TestTree
describeOrderableClusterOptionsTest = undefined

deleteClusterTest :: DeleteCluster -> TestTree
deleteClusterTest = undefined

deleteEventSubscriptionTest :: DeleteEventSubscription -> TestTree
deleteEventSubscriptionTest = undefined

describeDefaultClusterParametersTest :: DescribeDefaultClusterParameters -> TestTree
describeDefaultClusterParametersTest = undefined

createClusterTest :: CreateCluster -> TestTree
createClusterTest = undefined

createHSMClientCertificateTest :: CreateHSMClientCertificate -> TestTree
createHSMClientCertificateTest = undefined

resetClusterParameterGroupTest :: ResetClusterParameterGroup -> TestTree
resetClusterParameterGroupTest = undefined

describeEventSubscriptionsTest :: DescribeEventSubscriptions -> TestTree
describeEventSubscriptionsTest = undefined

describeHSMClientCertificatesTest :: DescribeHSMClientCertificates -> TestTree
describeHSMClientCertificatesTest = undefined

modifyClusterParameterGroupTest :: ModifyClusterParameterGroup -> TestTree
modifyClusterParameterGroupTest = undefined

revokeClusterSecurityGroupIngressTest :: RevokeClusterSecurityGroupIngress -> TestTree
revokeClusterSecurityGroupIngressTest = undefined

authorizeClusterSecurityGroupIngressTest :: AuthorizeClusterSecurityGroupIngress -> TestTree
authorizeClusterSecurityGroupIngressTest = undefined

createClusterSecurityGroupTest :: CreateClusterSecurityGroup -> TestTree
createClusterSecurityGroupTest = undefined

describeResizeTest :: DescribeResize -> TestTree
describeResizeTest = undefined

describeEventCategoriesTest :: DescribeEventCategories -> TestTree
describeEventCategoriesTest = undefined

deleteHSMConfigurationTest :: DeleteHSMConfiguration -> TestTree
deleteHSMConfigurationTest = undefined

deleteClusterSecurityGroupTest :: DeleteClusterSecurityGroup -> TestTree
deleteClusterSecurityGroupTest = undefined

createHSMConfigurationTest :: CreateHSMConfiguration -> TestTree
createHSMConfigurationTest = undefined

modifyClusterTest :: ModifyCluster -> TestTree
modifyClusterTest = undefined

createClusterSnapshotTest :: CreateClusterSnapshot -> TestTree
createClusterSnapshotTest = undefined

describeLoggingStatusTest :: DescribeLoggingStatus -> TestTree
describeLoggingStatusTest = undefined

describeClusterParametersTest :: DescribeClusterParameters -> TestTree
describeClusterParametersTest = undefined

disableSnapshotCopyTest :: DisableSnapshotCopy -> TestTree
disableSnapshotCopyTest = undefined

restoreFromClusterSnapshotTest :: RestoreFromClusterSnapshot -> TestTree
restoreFromClusterSnapshotTest = undefined

describeHSMConfigurationsTest :: DescribeHSMConfigurations -> TestTree
describeHSMConfigurationsTest = undefined

createClusterParameterGroupTest :: CreateClusterParameterGroup -> TestTree
createClusterParameterGroupTest = undefined

revokeSnapshotAccessTest :: RevokeSnapshotAccess -> TestTree
revokeSnapshotAccessTest = undefined

deleteHSMClientCertificateTest :: DeleteHSMClientCertificate -> TestTree
deleteHSMClientCertificateTest = undefined

createSnapshotCopyGrantTest :: CreateSnapshotCopyGrant -> TestTree
createSnapshotCopyGrantTest = undefined

copyClusterSnapshotTest :: CopyClusterSnapshot -> TestTree
copyClusterSnapshotTest = undefined

describeClusterVersionsTest :: DescribeClusterVersions -> TestTree
describeClusterVersionsTest = undefined

modifyClusterSubnetGroupTest :: ModifyClusterSubnetGroup -> TestTree
modifyClusterSubnetGroupTest = undefined

deleteSnapshotCopyGrantTest :: DeleteSnapshotCopyGrant -> TestTree
deleteSnapshotCopyGrantTest = undefined

describeSnapshotCopyGrantsTest :: DescribeSnapshotCopyGrants -> TestTree
describeSnapshotCopyGrantsTest = undefined

rotateEncryptionKeyTest :: RotateEncryptionKey -> TestTree
rotateEncryptionKeyTest = undefined

-- Responses

describeClustersResponseTest :: DescribeClustersResponse -> TestTree
describeClustersResponseTest = resp
    "DescribeClustersResponse"
    "fixture/DescribeClustersResponse"
    (Proxy :: Proxy DescribeClusters)

describeTagsResponseTest :: DescribeTagsResponse -> TestTree
describeTagsResponseTest = resp
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse"
    (Proxy :: Proxy DescribeTags)

modifyEventSubscriptionResponseTest :: ModifyEventSubscriptionResponse -> TestTree
modifyEventSubscriptionResponseTest = resp
    "ModifyEventSubscriptionResponse"
    "fixture/ModifyEventSubscriptionResponse"
    (Proxy :: Proxy ModifyEventSubscription)

loggingStatusTest :: LoggingStatus -> TestTree
loggingStatusTest = resp
    "LoggingStatus"
    "fixture/LoggingStatus"
    (Proxy :: Proxy DisableLogging)

purchaseReservedNodeOfferingResponseTest :: PurchaseReservedNodeOfferingResponse -> TestTree
purchaseReservedNodeOfferingResponseTest = resp
    "PurchaseReservedNodeOfferingResponse"
    "fixture/PurchaseReservedNodeOfferingResponse"
    (Proxy :: Proxy PurchaseReservedNodeOffering)

deleteClusterSubnetGroupResponseTest :: DeleteClusterSubnetGroupResponse -> TestTree
deleteClusterSubnetGroupResponseTest = resp
    "DeleteClusterSubnetGroupResponse"
    "fixture/DeleteClusterSubnetGroupResponse"
    (Proxy :: Proxy DeleteClusterSubnetGroup)

deleteClusterSnapshotResponseTest :: DeleteClusterSnapshotResponse -> TestTree
deleteClusterSnapshotResponseTest = resp
    "DeleteClusterSnapshotResponse"
    "fixture/DeleteClusterSnapshotResponse"
    (Proxy :: Proxy DeleteClusterSnapshot)

describeEventsResponseTest :: DescribeEventsResponse -> TestTree
describeEventsResponseTest = resp
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse"
    (Proxy :: Proxy DescribeEvents)

describeReservedNodeOfferingsResponseTest :: DescribeReservedNodeOfferingsResponse -> TestTree
describeReservedNodeOfferingsResponseTest = resp
    "DescribeReservedNodeOfferingsResponse"
    "fixture/DescribeReservedNodeOfferingsResponse"
    (Proxy :: Proxy DescribeReservedNodeOfferings)

describeClusterParameterGroupsResponseTest :: DescribeClusterParameterGroupsResponse -> TestTree
describeClusterParameterGroupsResponseTest = resp
    "DescribeClusterParameterGroupsResponse"
    "fixture/DescribeClusterParameterGroupsResponse"
    (Proxy :: Proxy DescribeClusterParameterGroups)

createClusterSubnetGroupResponseTest :: CreateClusterSubnetGroupResponse -> TestTree
createClusterSubnetGroupResponseTest = resp
    "CreateClusterSubnetGroupResponse"
    "fixture/CreateClusterSubnetGroupResponse"
    (Proxy :: Proxy CreateClusterSubnetGroup)

describeReservedNodesResponseTest :: DescribeReservedNodesResponse -> TestTree
describeReservedNodesResponseTest = resp
    "DescribeReservedNodesResponse"
    "fixture/DescribeReservedNodesResponse"
    (Proxy :: Proxy DescribeReservedNodes)

loggingStatusTest :: LoggingStatus -> TestTree
loggingStatusTest = resp
    "LoggingStatus"
    "fixture/LoggingStatus"
    (Proxy :: Proxy EnableLogging)

createTagsResponseTest :: CreateTagsResponse -> TestTree
createTagsResponseTest = resp
    "CreateTagsResponse"
    "fixture/CreateTagsResponse"
    (Proxy :: Proxy CreateTags)

describeClusterSecurityGroupsResponseTest :: DescribeClusterSecurityGroupsResponse -> TestTree
describeClusterSecurityGroupsResponseTest = resp
    "DescribeClusterSecurityGroupsResponse"
    "fixture/DescribeClusterSecurityGroupsResponse"
    (Proxy :: Proxy DescribeClusterSecurityGroups)

deleteClusterParameterGroupResponseTest :: DeleteClusterParameterGroupResponse -> TestTree
deleteClusterParameterGroupResponseTest = resp
    "DeleteClusterParameterGroupResponse"
    "fixture/DeleteClusterParameterGroupResponse"
    (Proxy :: Proxy DeleteClusterParameterGroup)

deleteTagsResponseTest :: DeleteTagsResponse -> TestTree
deleteTagsResponseTest = resp
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse"
    (Proxy :: Proxy DeleteTags)

enableSnapshotCopyResponseTest :: EnableSnapshotCopyResponse -> TestTree
enableSnapshotCopyResponseTest = resp
    "EnableSnapshotCopyResponse"
    "fixture/EnableSnapshotCopyResponse"
    (Proxy :: Proxy EnableSnapshotCopy)

modifySnapshotCopyRetentionPeriodResponseTest :: ModifySnapshotCopyRetentionPeriodResponse -> TestTree
modifySnapshotCopyRetentionPeriodResponseTest = resp
    "ModifySnapshotCopyRetentionPeriodResponse"
    "fixture/ModifySnapshotCopyRetentionPeriodResponse"
    (Proxy :: Proxy ModifySnapshotCopyRetentionPeriod)

describeClusterSnapshotsResponseTest :: DescribeClusterSnapshotsResponse -> TestTree
describeClusterSnapshotsResponseTest = resp
    "DescribeClusterSnapshotsResponse"
    "fixture/DescribeClusterSnapshotsResponse"
    (Proxy :: Proxy DescribeClusterSnapshots)

describeClusterSubnetGroupsResponseTest :: DescribeClusterSubnetGroupsResponse -> TestTree
describeClusterSubnetGroupsResponseTest = resp
    "DescribeClusterSubnetGroupsResponse"
    "fixture/DescribeClusterSubnetGroupsResponse"
    (Proxy :: Proxy DescribeClusterSubnetGroups)

authorizeSnapshotAccessResponseTest :: AuthorizeSnapshotAccessResponse -> TestTree
authorizeSnapshotAccessResponseTest = resp
    "AuthorizeSnapshotAccessResponse"
    "fixture/AuthorizeSnapshotAccessResponse"
    (Proxy :: Proxy AuthorizeSnapshotAccess)

createEventSubscriptionResponseTest :: CreateEventSubscriptionResponse -> TestTree
createEventSubscriptionResponseTest = resp
    "CreateEventSubscriptionResponse"
    "fixture/CreateEventSubscriptionResponse"
    (Proxy :: Proxy CreateEventSubscription)

rebootClusterResponseTest :: RebootClusterResponse -> TestTree
rebootClusterResponseTest = resp
    "RebootClusterResponse"
    "fixture/RebootClusterResponse"
    (Proxy :: Proxy RebootCluster)

describeOrderableClusterOptionsResponseTest :: DescribeOrderableClusterOptionsResponse -> TestTree
describeOrderableClusterOptionsResponseTest = resp
    "DescribeOrderableClusterOptionsResponse"
    "fixture/DescribeOrderableClusterOptionsResponse"
    (Proxy :: Proxy DescribeOrderableClusterOptions)

deleteClusterResponseTest :: DeleteClusterResponse -> TestTree
deleteClusterResponseTest = resp
    "DeleteClusterResponse"
    "fixture/DeleteClusterResponse"
    (Proxy :: Proxy DeleteCluster)

deleteEventSubscriptionResponseTest :: DeleteEventSubscriptionResponse -> TestTree
deleteEventSubscriptionResponseTest = resp
    "DeleteEventSubscriptionResponse"
    "fixture/DeleteEventSubscriptionResponse"
    (Proxy :: Proxy DeleteEventSubscription)

describeDefaultClusterParametersResponseTest :: DescribeDefaultClusterParametersResponse -> TestTree
describeDefaultClusterParametersResponseTest = resp
    "DescribeDefaultClusterParametersResponse"
    "fixture/DescribeDefaultClusterParametersResponse"
    (Proxy :: Proxy DescribeDefaultClusterParameters)

createClusterResponseTest :: CreateClusterResponse -> TestTree
createClusterResponseTest = resp
    "CreateClusterResponse"
    "fixture/CreateClusterResponse"
    (Proxy :: Proxy CreateCluster)

createHSMClientCertificateResponseTest :: CreateHSMClientCertificateResponse -> TestTree
createHSMClientCertificateResponseTest = resp
    "CreateHSMClientCertificateResponse"
    "fixture/CreateHSMClientCertificateResponse"
    (Proxy :: Proxy CreateHSMClientCertificate)

clusterParameterGroupNameMessageTest :: ClusterParameterGroupNameMessage -> TestTree
clusterParameterGroupNameMessageTest = resp
    "ClusterParameterGroupNameMessage"
    "fixture/ClusterParameterGroupNameMessage"
    (Proxy :: Proxy ResetClusterParameterGroup)

describeEventSubscriptionsResponseTest :: DescribeEventSubscriptionsResponse -> TestTree
describeEventSubscriptionsResponseTest = resp
    "DescribeEventSubscriptionsResponse"
    "fixture/DescribeEventSubscriptionsResponse"
    (Proxy :: Proxy DescribeEventSubscriptions)

describeHSMClientCertificatesResponseTest :: DescribeHSMClientCertificatesResponse -> TestTree
describeHSMClientCertificatesResponseTest = resp
    "DescribeHSMClientCertificatesResponse"
    "fixture/DescribeHSMClientCertificatesResponse"
    (Proxy :: Proxy DescribeHSMClientCertificates)

clusterParameterGroupNameMessageTest :: ClusterParameterGroupNameMessage -> TestTree
clusterParameterGroupNameMessageTest = resp
    "ClusterParameterGroupNameMessage"
    "fixture/ClusterParameterGroupNameMessage"
    (Proxy :: Proxy ModifyClusterParameterGroup)

revokeClusterSecurityGroupIngressResponseTest :: RevokeClusterSecurityGroupIngressResponse -> TestTree
revokeClusterSecurityGroupIngressResponseTest = resp
    "RevokeClusterSecurityGroupIngressResponse"
    "fixture/RevokeClusterSecurityGroupIngressResponse"
    (Proxy :: Proxy RevokeClusterSecurityGroupIngress)

authorizeClusterSecurityGroupIngressResponseTest :: AuthorizeClusterSecurityGroupIngressResponse -> TestTree
authorizeClusterSecurityGroupIngressResponseTest = resp
    "AuthorizeClusterSecurityGroupIngressResponse"
    "fixture/AuthorizeClusterSecurityGroupIngressResponse"
    (Proxy :: Proxy AuthorizeClusterSecurityGroupIngress)

createClusterSecurityGroupResponseTest :: CreateClusterSecurityGroupResponse -> TestTree
createClusterSecurityGroupResponseTest = resp
    "CreateClusterSecurityGroupResponse"
    "fixture/CreateClusterSecurityGroupResponse"
    (Proxy :: Proxy CreateClusterSecurityGroup)

describeResizeResponseTest :: DescribeResizeResponse -> TestTree
describeResizeResponseTest = resp
    "DescribeResizeResponse"
    "fixture/DescribeResizeResponse"
    (Proxy :: Proxy DescribeResize)

describeEventCategoriesResponseTest :: DescribeEventCategoriesResponse -> TestTree
describeEventCategoriesResponseTest = resp
    "DescribeEventCategoriesResponse"
    "fixture/DescribeEventCategoriesResponse"
    (Proxy :: Proxy DescribeEventCategories)

deleteHSMConfigurationResponseTest :: DeleteHSMConfigurationResponse -> TestTree
deleteHSMConfigurationResponseTest = resp
    "DeleteHSMConfigurationResponse"
    "fixture/DeleteHSMConfigurationResponse"
    (Proxy :: Proxy DeleteHSMConfiguration)

deleteClusterSecurityGroupResponseTest :: DeleteClusterSecurityGroupResponse -> TestTree
deleteClusterSecurityGroupResponseTest = resp
    "DeleteClusterSecurityGroupResponse"
    "fixture/DeleteClusterSecurityGroupResponse"
    (Proxy :: Proxy DeleteClusterSecurityGroup)

createHSMConfigurationResponseTest :: CreateHSMConfigurationResponse -> TestTree
createHSMConfigurationResponseTest = resp
    "CreateHSMConfigurationResponse"
    "fixture/CreateHSMConfigurationResponse"
    (Proxy :: Proxy CreateHSMConfiguration)

modifyClusterResponseTest :: ModifyClusterResponse -> TestTree
modifyClusterResponseTest = resp
    "ModifyClusterResponse"
    "fixture/ModifyClusterResponse"
    (Proxy :: Proxy ModifyCluster)

createClusterSnapshotResponseTest :: CreateClusterSnapshotResponse -> TestTree
createClusterSnapshotResponseTest = resp
    "CreateClusterSnapshotResponse"
    "fixture/CreateClusterSnapshotResponse"
    (Proxy :: Proxy CreateClusterSnapshot)

loggingStatusTest :: LoggingStatus -> TestTree
loggingStatusTest = resp
    "LoggingStatus"
    "fixture/LoggingStatus"
    (Proxy :: Proxy DescribeLoggingStatus)

describeClusterParametersResponseTest :: DescribeClusterParametersResponse -> TestTree
describeClusterParametersResponseTest = resp
    "DescribeClusterParametersResponse"
    "fixture/DescribeClusterParametersResponse"
    (Proxy :: Proxy DescribeClusterParameters)

disableSnapshotCopyResponseTest :: DisableSnapshotCopyResponse -> TestTree
disableSnapshotCopyResponseTest = resp
    "DisableSnapshotCopyResponse"
    "fixture/DisableSnapshotCopyResponse"
    (Proxy :: Proxy DisableSnapshotCopy)

restoreFromClusterSnapshotResponseTest :: RestoreFromClusterSnapshotResponse -> TestTree
restoreFromClusterSnapshotResponseTest = resp
    "RestoreFromClusterSnapshotResponse"
    "fixture/RestoreFromClusterSnapshotResponse"
    (Proxy :: Proxy RestoreFromClusterSnapshot)

describeHSMConfigurationsResponseTest :: DescribeHSMConfigurationsResponse -> TestTree
describeHSMConfigurationsResponseTest = resp
    "DescribeHSMConfigurationsResponse"
    "fixture/DescribeHSMConfigurationsResponse"
    (Proxy :: Proxy DescribeHSMConfigurations)

createClusterParameterGroupResponseTest :: CreateClusterParameterGroupResponse -> TestTree
createClusterParameterGroupResponseTest = resp
    "CreateClusterParameterGroupResponse"
    "fixture/CreateClusterParameterGroupResponse"
    (Proxy :: Proxy CreateClusterParameterGroup)

revokeSnapshotAccessResponseTest :: RevokeSnapshotAccessResponse -> TestTree
revokeSnapshotAccessResponseTest = resp
    "RevokeSnapshotAccessResponse"
    "fixture/RevokeSnapshotAccessResponse"
    (Proxy :: Proxy RevokeSnapshotAccess)

deleteHSMClientCertificateResponseTest :: DeleteHSMClientCertificateResponse -> TestTree
deleteHSMClientCertificateResponseTest = resp
    "DeleteHSMClientCertificateResponse"
    "fixture/DeleteHSMClientCertificateResponse"
    (Proxy :: Proxy DeleteHSMClientCertificate)

createSnapshotCopyGrantResponseTest :: CreateSnapshotCopyGrantResponse -> TestTree
createSnapshotCopyGrantResponseTest = resp
    "CreateSnapshotCopyGrantResponse"
    "fixture/CreateSnapshotCopyGrantResponse"
    (Proxy :: Proxy CreateSnapshotCopyGrant)

copyClusterSnapshotResponseTest :: CopyClusterSnapshotResponse -> TestTree
copyClusterSnapshotResponseTest = resp
    "CopyClusterSnapshotResponse"
    "fixture/CopyClusterSnapshotResponse"
    (Proxy :: Proxy CopyClusterSnapshot)

describeClusterVersionsResponseTest :: DescribeClusterVersionsResponse -> TestTree
describeClusterVersionsResponseTest = resp
    "DescribeClusterVersionsResponse"
    "fixture/DescribeClusterVersionsResponse"
    (Proxy :: Proxy DescribeClusterVersions)

modifyClusterSubnetGroupResponseTest :: ModifyClusterSubnetGroupResponse -> TestTree
modifyClusterSubnetGroupResponseTest = resp
    "ModifyClusterSubnetGroupResponse"
    "fixture/ModifyClusterSubnetGroupResponse"
    (Proxy :: Proxy ModifyClusterSubnetGroup)

deleteSnapshotCopyGrantResponseTest :: DeleteSnapshotCopyGrantResponse -> TestTree
deleteSnapshotCopyGrantResponseTest = resp
    "DeleteSnapshotCopyGrantResponse"
    "fixture/DeleteSnapshotCopyGrantResponse"
    (Proxy :: Proxy DeleteSnapshotCopyGrant)

describeSnapshotCopyGrantsResponseTest :: DescribeSnapshotCopyGrantsResponse -> TestTree
describeSnapshotCopyGrantsResponseTest = resp
    "DescribeSnapshotCopyGrantsResponse"
    "fixture/DescribeSnapshotCopyGrantsResponse"
    (Proxy :: Proxy DescribeSnapshotCopyGrants)

rotateEncryptionKeyResponseTest :: RotateEncryptionKeyResponse -> TestTree
rotateEncryptionKeyResponseTest = resp
    "RotateEncryptionKeyResponse"
    "fixture/RotateEncryptionKeyResponse"
    (Proxy :: Proxy RotateEncryptionKey)
