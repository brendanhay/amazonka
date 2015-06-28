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

import           Data.Proxy
import           Network.AWS.Redshift
import           Test.AWS.Fixture
import           Test.Tasty

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
    "DescribeClusters"
    "fixture/Redshift/DescribeClustersResponse"
    (Proxy :: Proxy DescribeClusters)

describeTagsResponseTest :: DescribeTagsResponse -> TestTree
describeTagsResponseTest = resp
    "DescribeTags"
    "fixture/Redshift/DescribeTagsResponse"
    (Proxy :: Proxy DescribeTags)

modifyEventSubscriptionResponseTest :: ModifyEventSubscriptionResponse -> TestTree
modifyEventSubscriptionResponseTest = resp
    "ModifyEventSubscription"
    "fixture/Redshift/ModifyEventSubscriptionResponse"
    (Proxy :: Proxy ModifyEventSubscription)

loggingStatusTest :: LoggingStatus -> TestTree
loggingStatusTest = resp
    "DisableLogging"
    "fixture/Redshift/LoggingStatus"
    (Proxy :: Proxy DisableLogging)

purchaseReservedNodeOfferingResponseTest :: PurchaseReservedNodeOfferingResponse -> TestTree
purchaseReservedNodeOfferingResponseTest = resp
    "PurchaseReservedNodeOffering"
    "fixture/Redshift/PurchaseReservedNodeOfferingResponse"
    (Proxy :: Proxy PurchaseReservedNodeOffering)

deleteClusterSubnetGroupResponseTest :: DeleteClusterSubnetGroupResponse -> TestTree
deleteClusterSubnetGroupResponseTest = resp
    "DeleteClusterSubnetGroup"
    "fixture/Redshift/DeleteClusterSubnetGroupResponse"
    (Proxy :: Proxy DeleteClusterSubnetGroup)

deleteClusterSnapshotResponseTest :: DeleteClusterSnapshotResponse -> TestTree
deleteClusterSnapshotResponseTest = resp
    "DeleteClusterSnapshot"
    "fixture/Redshift/DeleteClusterSnapshotResponse"
    (Proxy :: Proxy DeleteClusterSnapshot)

describeEventsResponseTest :: DescribeEventsResponse -> TestTree
describeEventsResponseTest = resp
    "DescribeEvents"
    "fixture/Redshift/DescribeEventsResponse"
    (Proxy :: Proxy DescribeEvents)

describeReservedNodeOfferingsResponseTest :: DescribeReservedNodeOfferingsResponse -> TestTree
describeReservedNodeOfferingsResponseTest = resp
    "DescribeReservedNodeOfferings"
    "fixture/Redshift/DescribeReservedNodeOfferingsResponse"
    (Proxy :: Proxy DescribeReservedNodeOfferings)

describeClusterParameterGroupsResponseTest :: DescribeClusterParameterGroupsResponse -> TestTree
describeClusterParameterGroupsResponseTest = resp
    "DescribeClusterParameterGroups"
    "fixture/Redshift/DescribeClusterParameterGroupsResponse"
    (Proxy :: Proxy DescribeClusterParameterGroups)

createClusterSubnetGroupResponseTest :: CreateClusterSubnetGroupResponse -> TestTree
createClusterSubnetGroupResponseTest = resp
    "CreateClusterSubnetGroup"
    "fixture/Redshift/CreateClusterSubnetGroupResponse"
    (Proxy :: Proxy CreateClusterSubnetGroup)

describeReservedNodesResponseTest :: DescribeReservedNodesResponse -> TestTree
describeReservedNodesResponseTest = resp
    "DescribeReservedNodes"
    "fixture/Redshift/DescribeReservedNodesResponse"
    (Proxy :: Proxy DescribeReservedNodes)

loggingStatusTest :: LoggingStatus -> TestTree
loggingStatusTest = resp
    "EnableLogging"
    "fixture/Redshift/LoggingStatus"
    (Proxy :: Proxy EnableLogging)

createTagsResponseTest :: CreateTagsResponse -> TestTree
createTagsResponseTest = resp
    "CreateTags"
    "fixture/Redshift/CreateTagsResponse"
    (Proxy :: Proxy CreateTags)

describeClusterSecurityGroupsResponseTest :: DescribeClusterSecurityGroupsResponse -> TestTree
describeClusterSecurityGroupsResponseTest = resp
    "DescribeClusterSecurityGroups"
    "fixture/Redshift/DescribeClusterSecurityGroupsResponse"
    (Proxy :: Proxy DescribeClusterSecurityGroups)

deleteClusterParameterGroupResponseTest :: DeleteClusterParameterGroupResponse -> TestTree
deleteClusterParameterGroupResponseTest = resp
    "DeleteClusterParameterGroup"
    "fixture/Redshift/DeleteClusterParameterGroupResponse"
    (Proxy :: Proxy DeleteClusterParameterGroup)

deleteTagsResponseTest :: DeleteTagsResponse -> TestTree
deleteTagsResponseTest = resp
    "DeleteTags"
    "fixture/Redshift/DeleteTagsResponse"
    (Proxy :: Proxy DeleteTags)

enableSnapshotCopyResponseTest :: EnableSnapshotCopyResponse -> TestTree
enableSnapshotCopyResponseTest = resp
    "EnableSnapshotCopy"
    "fixture/Redshift/EnableSnapshotCopyResponse"
    (Proxy :: Proxy EnableSnapshotCopy)

modifySnapshotCopyRetentionPeriodResponseTest :: ModifySnapshotCopyRetentionPeriodResponse -> TestTree
modifySnapshotCopyRetentionPeriodResponseTest = resp
    "ModifySnapshotCopyRetentionPeriod"
    "fixture/Redshift/ModifySnapshotCopyRetentionPeriodResponse"
    (Proxy :: Proxy ModifySnapshotCopyRetentionPeriod)

describeClusterSnapshotsResponseTest :: DescribeClusterSnapshotsResponse -> TestTree
describeClusterSnapshotsResponseTest = resp
    "DescribeClusterSnapshots"
    "fixture/Redshift/DescribeClusterSnapshotsResponse"
    (Proxy :: Proxy DescribeClusterSnapshots)

describeClusterSubnetGroupsResponseTest :: DescribeClusterSubnetGroupsResponse -> TestTree
describeClusterSubnetGroupsResponseTest = resp
    "DescribeClusterSubnetGroups"
    "fixture/Redshift/DescribeClusterSubnetGroupsResponse"
    (Proxy :: Proxy DescribeClusterSubnetGroups)

authorizeSnapshotAccessResponseTest :: AuthorizeSnapshotAccessResponse -> TestTree
authorizeSnapshotAccessResponseTest = resp
    "AuthorizeSnapshotAccess"
    "fixture/Redshift/AuthorizeSnapshotAccessResponse"
    (Proxy :: Proxy AuthorizeSnapshotAccess)

createEventSubscriptionResponseTest :: CreateEventSubscriptionResponse -> TestTree
createEventSubscriptionResponseTest = resp
    "CreateEventSubscription"
    "fixture/Redshift/CreateEventSubscriptionResponse"
    (Proxy :: Proxy CreateEventSubscription)

rebootClusterResponseTest :: RebootClusterResponse -> TestTree
rebootClusterResponseTest = resp
    "RebootCluster"
    "fixture/Redshift/RebootClusterResponse"
    (Proxy :: Proxy RebootCluster)

describeOrderableClusterOptionsResponseTest :: DescribeOrderableClusterOptionsResponse -> TestTree
describeOrderableClusterOptionsResponseTest = resp
    "DescribeOrderableClusterOptions"
    "fixture/Redshift/DescribeOrderableClusterOptionsResponse"
    (Proxy :: Proxy DescribeOrderableClusterOptions)

deleteClusterResponseTest :: DeleteClusterResponse -> TestTree
deleteClusterResponseTest = resp
    "DeleteCluster"
    "fixture/Redshift/DeleteClusterResponse"
    (Proxy :: Proxy DeleteCluster)

deleteEventSubscriptionResponseTest :: DeleteEventSubscriptionResponse -> TestTree
deleteEventSubscriptionResponseTest = resp
    "DeleteEventSubscription"
    "fixture/Redshift/DeleteEventSubscriptionResponse"
    (Proxy :: Proxy DeleteEventSubscription)

describeDefaultClusterParametersResponseTest :: DescribeDefaultClusterParametersResponse -> TestTree
describeDefaultClusterParametersResponseTest = resp
    "DescribeDefaultClusterParameters"
    "fixture/Redshift/DescribeDefaultClusterParametersResponse"
    (Proxy :: Proxy DescribeDefaultClusterParameters)

createClusterResponseTest :: CreateClusterResponse -> TestTree
createClusterResponseTest = resp
    "CreateCluster"
    "fixture/Redshift/CreateClusterResponse"
    (Proxy :: Proxy CreateCluster)

createHSMClientCertificateResponseTest :: CreateHSMClientCertificateResponse -> TestTree
createHSMClientCertificateResponseTest = resp
    "CreateHSMClientCertificate"
    "fixture/Redshift/CreateHSMClientCertificateResponse"
    (Proxy :: Proxy CreateHSMClientCertificate)

clusterParameterGroupNameMessageTest :: ClusterParameterGroupNameMessage -> TestTree
clusterParameterGroupNameMessageTest = resp
    "ResetClusterParameterGroup"
    "fixture/Redshift/ClusterParameterGroupNameMessage"
    (Proxy :: Proxy ResetClusterParameterGroup)

describeEventSubscriptionsResponseTest :: DescribeEventSubscriptionsResponse -> TestTree
describeEventSubscriptionsResponseTest = resp
    "DescribeEventSubscriptions"
    "fixture/Redshift/DescribeEventSubscriptionsResponse"
    (Proxy :: Proxy DescribeEventSubscriptions)

describeHSMClientCertificatesResponseTest :: DescribeHSMClientCertificatesResponse -> TestTree
describeHSMClientCertificatesResponseTest = resp
    "DescribeHSMClientCertificates"
    "fixture/Redshift/DescribeHSMClientCertificatesResponse"
    (Proxy :: Proxy DescribeHSMClientCertificates)

clusterParameterGroupNameMessageTest :: ClusterParameterGroupNameMessage -> TestTree
clusterParameterGroupNameMessageTest = resp
    "ModifyClusterParameterGroup"
    "fixture/Redshift/ClusterParameterGroupNameMessage"
    (Proxy :: Proxy ModifyClusterParameterGroup)

revokeClusterSecurityGroupIngressResponseTest :: RevokeClusterSecurityGroupIngressResponse -> TestTree
revokeClusterSecurityGroupIngressResponseTest = resp
    "RevokeClusterSecurityGroupIngress"
    "fixture/Redshift/RevokeClusterSecurityGroupIngressResponse"
    (Proxy :: Proxy RevokeClusterSecurityGroupIngress)

authorizeClusterSecurityGroupIngressResponseTest :: AuthorizeClusterSecurityGroupIngressResponse -> TestTree
authorizeClusterSecurityGroupIngressResponseTest = resp
    "AuthorizeClusterSecurityGroupIngress"
    "fixture/Redshift/AuthorizeClusterSecurityGroupIngressResponse"
    (Proxy :: Proxy AuthorizeClusterSecurityGroupIngress)

createClusterSecurityGroupResponseTest :: CreateClusterSecurityGroupResponse -> TestTree
createClusterSecurityGroupResponseTest = resp
    "CreateClusterSecurityGroup"
    "fixture/Redshift/CreateClusterSecurityGroupResponse"
    (Proxy :: Proxy CreateClusterSecurityGroup)

describeResizeResponseTest :: DescribeResizeResponse -> TestTree
describeResizeResponseTest = resp
    "DescribeResize"
    "fixture/Redshift/DescribeResizeResponse"
    (Proxy :: Proxy DescribeResize)

describeEventCategoriesResponseTest :: DescribeEventCategoriesResponse -> TestTree
describeEventCategoriesResponseTest = resp
    "DescribeEventCategories"
    "fixture/Redshift/DescribeEventCategoriesResponse"
    (Proxy :: Proxy DescribeEventCategories)

deleteHSMConfigurationResponseTest :: DeleteHSMConfigurationResponse -> TestTree
deleteHSMConfigurationResponseTest = resp
    "DeleteHSMConfiguration"
    "fixture/Redshift/DeleteHSMConfigurationResponse"
    (Proxy :: Proxy DeleteHSMConfiguration)

deleteClusterSecurityGroupResponseTest :: DeleteClusterSecurityGroupResponse -> TestTree
deleteClusterSecurityGroupResponseTest = resp
    "DeleteClusterSecurityGroup"
    "fixture/Redshift/DeleteClusterSecurityGroupResponse"
    (Proxy :: Proxy DeleteClusterSecurityGroup)

createHSMConfigurationResponseTest :: CreateHSMConfigurationResponse -> TestTree
createHSMConfigurationResponseTest = resp
    "CreateHSMConfiguration"
    "fixture/Redshift/CreateHSMConfigurationResponse"
    (Proxy :: Proxy CreateHSMConfiguration)

modifyClusterResponseTest :: ModifyClusterResponse -> TestTree
modifyClusterResponseTest = resp
    "ModifyCluster"
    "fixture/Redshift/ModifyClusterResponse"
    (Proxy :: Proxy ModifyCluster)

createClusterSnapshotResponseTest :: CreateClusterSnapshotResponse -> TestTree
createClusterSnapshotResponseTest = resp
    "CreateClusterSnapshot"
    "fixture/Redshift/CreateClusterSnapshotResponse"
    (Proxy :: Proxy CreateClusterSnapshot)

loggingStatusTest :: LoggingStatus -> TestTree
loggingStatusTest = resp
    "DescribeLoggingStatus"
    "fixture/Redshift/LoggingStatus"
    (Proxy :: Proxy DescribeLoggingStatus)

describeClusterParametersResponseTest :: DescribeClusterParametersResponse -> TestTree
describeClusterParametersResponseTest = resp
    "DescribeClusterParameters"
    "fixture/Redshift/DescribeClusterParametersResponse"
    (Proxy :: Proxy DescribeClusterParameters)

disableSnapshotCopyResponseTest :: DisableSnapshotCopyResponse -> TestTree
disableSnapshotCopyResponseTest = resp
    "DisableSnapshotCopy"
    "fixture/Redshift/DisableSnapshotCopyResponse"
    (Proxy :: Proxy DisableSnapshotCopy)

restoreFromClusterSnapshotResponseTest :: RestoreFromClusterSnapshotResponse -> TestTree
restoreFromClusterSnapshotResponseTest = resp
    "RestoreFromClusterSnapshot"
    "fixture/Redshift/RestoreFromClusterSnapshotResponse"
    (Proxy :: Proxy RestoreFromClusterSnapshot)

describeHSMConfigurationsResponseTest :: DescribeHSMConfigurationsResponse -> TestTree
describeHSMConfigurationsResponseTest = resp
    "DescribeHSMConfigurations"
    "fixture/Redshift/DescribeHSMConfigurationsResponse"
    (Proxy :: Proxy DescribeHSMConfigurations)

createClusterParameterGroupResponseTest :: CreateClusterParameterGroupResponse -> TestTree
createClusterParameterGroupResponseTest = resp
    "CreateClusterParameterGroup"
    "fixture/Redshift/CreateClusterParameterGroupResponse"
    (Proxy :: Proxy CreateClusterParameterGroup)

revokeSnapshotAccessResponseTest :: RevokeSnapshotAccessResponse -> TestTree
revokeSnapshotAccessResponseTest = resp
    "RevokeSnapshotAccess"
    "fixture/Redshift/RevokeSnapshotAccessResponse"
    (Proxy :: Proxy RevokeSnapshotAccess)

deleteHSMClientCertificateResponseTest :: DeleteHSMClientCertificateResponse -> TestTree
deleteHSMClientCertificateResponseTest = resp
    "DeleteHSMClientCertificate"
    "fixture/Redshift/DeleteHSMClientCertificateResponse"
    (Proxy :: Proxy DeleteHSMClientCertificate)

createSnapshotCopyGrantResponseTest :: CreateSnapshotCopyGrantResponse -> TestTree
createSnapshotCopyGrantResponseTest = resp
    "CreateSnapshotCopyGrant"
    "fixture/Redshift/CreateSnapshotCopyGrantResponse"
    (Proxy :: Proxy CreateSnapshotCopyGrant)

copyClusterSnapshotResponseTest :: CopyClusterSnapshotResponse -> TestTree
copyClusterSnapshotResponseTest = resp
    "CopyClusterSnapshot"
    "fixture/Redshift/CopyClusterSnapshotResponse"
    (Proxy :: Proxy CopyClusterSnapshot)

describeClusterVersionsResponseTest :: DescribeClusterVersionsResponse -> TestTree
describeClusterVersionsResponseTest = resp
    "DescribeClusterVersions"
    "fixture/Redshift/DescribeClusterVersionsResponse"
    (Proxy :: Proxy DescribeClusterVersions)

modifyClusterSubnetGroupResponseTest :: ModifyClusterSubnetGroupResponse -> TestTree
modifyClusterSubnetGroupResponseTest = resp
    "ModifyClusterSubnetGroup"
    "fixture/Redshift/ModifyClusterSubnetGroupResponse"
    (Proxy :: Proxy ModifyClusterSubnetGroup)

deleteSnapshotCopyGrantResponseTest :: DeleteSnapshotCopyGrantResponse -> TestTree
deleteSnapshotCopyGrantResponseTest = resp
    "DeleteSnapshotCopyGrant"
    "fixture/Redshift/DeleteSnapshotCopyGrantResponse"
    (Proxy :: Proxy DeleteSnapshotCopyGrant)

describeSnapshotCopyGrantsResponseTest :: DescribeSnapshotCopyGrantsResponse -> TestTree
describeSnapshotCopyGrantsResponseTest = resp
    "DescribeSnapshotCopyGrants"
    "fixture/Redshift/DescribeSnapshotCopyGrantsResponse"
    (Proxy :: Proxy DescribeSnapshotCopyGrants)

rotateEncryptionKeyResponseTest :: RotateEncryptionKeyResponse -> TestTree
rotateEncryptionKeyResponseTest = resp
    "RotateEncryptionKey"
    "fixture/Redshift/RotateEncryptionKeyResponse"
    (Proxy :: Proxy RotateEncryptionKey)
