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
--         [ authorizeClusterSecurityGroupIngressTest $
--             authorizeClusterSecurityGroupIngress
--
--         , authorizeSnapshotAccessTest $
--             authorizeSnapshotAccess
--
--         , copyClusterSnapshotTest $
--             copyClusterSnapshot
--
--         , createClusterTest $
--             createCluster
--
--         , createClusterParameterGroupTest $
--             createClusterParameterGroup
--
--         , createClusterSecurityGroupTest $
--             createClusterSecurityGroup
--
--         , createClusterSnapshotTest $
--             createClusterSnapshot
--
--         , createClusterSubnetGroupTest $
--             createClusterSubnetGroup
--
--         , createEventSubscriptionTest $
--             createEventSubscription
--
--         , createHSMClientCertificateTest $
--             createHSMClientCertificate
--
--         , createHSMConfigurationTest $
--             createHSMConfiguration
--
--         , createSnapshotCopyGrantTest $
--             createSnapshotCopyGrant
--
--         , createTagsTest $
--             createTags
--
--         , deleteClusterTest $
--             deleteCluster
--
--         , deleteClusterParameterGroupTest $
--             deleteClusterParameterGroup
--
--         , deleteClusterSecurityGroupTest $
--             deleteClusterSecurityGroup
--
--         , deleteClusterSnapshotTest $
--             deleteClusterSnapshot
--
--         , deleteClusterSubnetGroupTest $
--             deleteClusterSubnetGroup
--
--         , deleteEventSubscriptionTest $
--             deleteEventSubscription
--
--         , deleteHSMClientCertificateTest $
--             deleteHSMClientCertificate
--
--         , deleteHSMConfigurationTest $
--             deleteHSMConfiguration
--
--         , deleteSnapshotCopyGrantTest $
--             deleteSnapshotCopyGrant
--
--         , deleteTagsTest $
--             deleteTags
--
--         , describeClusterParameterGroupsTest $
--             describeClusterParameterGroups
--
--         , describeClusterParametersTest $
--             describeClusterParameters
--
--         , describeClusterSecurityGroupsTest $
--             describeClusterSecurityGroups
--
--         , describeClusterSnapshotsTest $
--             describeClusterSnapshots
--
--         , describeClusterSubnetGroupsTest $
--             describeClusterSubnetGroups
--
--         , describeClusterVersionsTest $
--             describeClusterVersions
--
--         , describeClustersTest $
--             describeClusters
--
--         , describeDefaultClusterParametersTest $
--             describeDefaultClusterParameters
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
--         , describeHSMClientCertificatesTest $
--             describeHSMClientCertificates
--
--         , describeHSMConfigurationsTest $
--             describeHSMConfigurations
--
--         , describeLoggingStatusTest $
--             describeLoggingStatus
--
--         , describeOrderableClusterOptionsTest $
--             describeOrderableClusterOptions
--
--         , describeReservedNodeOfferingsTest $
--             describeReservedNodeOfferings
--
--         , describeReservedNodesTest $
--             describeReservedNodes
--
--         , describeResizeTest $
--             describeResize
--
--         , describeSnapshotCopyGrantsTest $
--             describeSnapshotCopyGrants
--
--         , describeTagsTest $
--             describeTags
--
--         , disableLoggingTest $
--             disableLogging
--
--         , disableSnapshotCopyTest $
--             disableSnapshotCopy
--
--         , enableLoggingTest $
--             enableLogging
--
--         , enableSnapshotCopyTest $
--             enableSnapshotCopy
--
--         , modifyClusterTest $
--             modifyCluster
--
--         , modifyClusterParameterGroupTest $
--             modifyClusterParameterGroup
--
--         , modifyClusterSubnetGroupTest $
--             modifyClusterSubnetGroup
--
--         , modifyEventSubscriptionTest $
--             modifyEventSubscription
--
--         , modifySnapshotCopyRetentionPeriodTest $
--             modifySnapshotCopyRetentionPeriod
--
--         , purchaseReservedNodeOfferingTest $
--             purchaseReservedNodeOffering
--
--         , rebootClusterTest $
--             rebootCluster
--
--         , resetClusterParameterGroupTest $
--             resetClusterParameterGroup
--
--         , restoreFromClusterSnapshotTest $
--             restoreFromClusterSnapshot
--
--         , revokeClusterSecurityGroupIngressTest $
--             revokeClusterSecurityGroupIngress
--
--         , revokeSnapshotAccessTest $
--             revokeSnapshotAccess
--
--         , rotateEncryptionKeyTest $
--             rotateEncryptionKey
--
--           ]

--     , testGroup "response"
--         [ authorizeClusterSecurityGroupIngressResponseTest $
--             authorizeClusterSecurityGroupIngressResponse
--
--         , authorizeSnapshotAccessResponseTest $
--             authorizeSnapshotAccessResponse
--
--         , copyClusterSnapshotResponseTest $
--             copyClusterSnapshotResponse
--
--         , createClusterResponseTest $
--             createClusterResponse
--
--         , createClusterParameterGroupResponseTest $
--             createClusterParameterGroupResponse
--
--         , createClusterSecurityGroupResponseTest $
--             createClusterSecurityGroupResponse
--
--         , createClusterSnapshotResponseTest $
--             createClusterSnapshotResponse
--
--         , createClusterSubnetGroupResponseTest $
--             createClusterSubnetGroupResponse
--
--         , createEventSubscriptionResponseTest $
--             createEventSubscriptionResponse
--
--         , createHSMClientCertificateResponseTest $
--             createHSMClientCertificateResponse
--
--         , createHSMConfigurationResponseTest $
--             createHSMConfigurationResponse
--
--         , createSnapshotCopyGrantResponseTest $
--             createSnapshotCopyGrantResponse
--
--         , createTagsResponseTest $
--             createTagsResponse
--
--         , deleteClusterResponseTest $
--             deleteClusterResponse
--
--         , deleteClusterParameterGroupResponseTest $
--             deleteClusterParameterGroupResponse
--
--         , deleteClusterSecurityGroupResponseTest $
--             deleteClusterSecurityGroupResponse
--
--         , deleteClusterSnapshotResponseTest $
--             deleteClusterSnapshotResponse
--
--         , deleteClusterSubnetGroupResponseTest $
--             deleteClusterSubnetGroupResponse
--
--         , deleteEventSubscriptionResponseTest $
--             deleteEventSubscriptionResponse
--
--         , deleteHSMClientCertificateResponseTest $
--             deleteHSMClientCertificateResponse
--
--         , deleteHSMConfigurationResponseTest $
--             deleteHSMConfigurationResponse
--
--         , deleteSnapshotCopyGrantResponseTest $
--             deleteSnapshotCopyGrantResponse
--
--         , deleteTagsResponseTest $
--             deleteTagsResponse
--
--         , describeClusterParameterGroupsResponseTest $
--             describeClusterParameterGroupsResponse
--
--         , describeClusterParametersResponseTest $
--             describeClusterParametersResponse
--
--         , describeClusterSecurityGroupsResponseTest $
--             describeClusterSecurityGroupsResponse
--
--         , describeClusterSnapshotsResponseTest $
--             describeClusterSnapshotsResponse
--
--         , describeClusterSubnetGroupsResponseTest $
--             describeClusterSubnetGroupsResponse
--
--         , describeClusterVersionsResponseTest $
--             describeClusterVersionsResponse
--
--         , describeClustersResponseTest $
--             describeClustersResponse
--
--         , describeDefaultClusterParametersResponseTest $
--             describeDefaultClusterParametersResponse
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
--         , describeHSMClientCertificatesResponseTest $
--             describeHSMClientCertificatesResponse
--
--         , describeHSMConfigurationsResponseTest $
--             describeHSMConfigurationsResponse
--
--         , describeLoggingStatusResponseTest $
--             loggingStatus
--
--         , describeOrderableClusterOptionsResponseTest $
--             describeOrderableClusterOptionsResponse
--
--         , describeReservedNodeOfferingsResponseTest $
--             describeReservedNodeOfferingsResponse
--
--         , describeReservedNodesResponseTest $
--             describeReservedNodesResponse
--
--         , describeResizeResponseTest $
--             describeResizeResponse
--
--         , describeSnapshotCopyGrantsResponseTest $
--             describeSnapshotCopyGrantsResponse
--
--         , describeTagsResponseTest $
--             describeTagsResponse
--
--         , disableLoggingResponseTest $
--             loggingStatus
--
--         , disableSnapshotCopyResponseTest $
--             disableSnapshotCopyResponse
--
--         , enableLoggingResponseTest $
--             loggingStatus
--
--         , enableSnapshotCopyResponseTest $
--             enableSnapshotCopyResponse
--
--         , modifyClusterResponseTest $
--             modifyClusterResponse
--
--         , modifyClusterParameterGroupResponseTest $
--             clusterParameterGroupNameMessage
--
--         , modifyClusterSubnetGroupResponseTest $
--             modifyClusterSubnetGroupResponse
--
--         , modifyEventSubscriptionResponseTest $
--             modifyEventSubscriptionResponse
--
--         , modifySnapshotCopyRetentionPeriodResponseTest $
--             modifySnapshotCopyRetentionPeriodResponse
--
--         , purchaseReservedNodeOfferingResponseTest $
--             purchaseReservedNodeOfferingResponse
--
--         , rebootClusterResponseTest $
--             rebootClusterResponse
--
--         , resetClusterParameterGroupResponseTest $
--             clusterParameterGroupNameMessage
--
--         , restoreFromClusterSnapshotResponseTest $
--             restoreFromClusterSnapshotResponse
--
--         , revokeClusterSecurityGroupIngressResponseTest $
--             revokeClusterSecurityGroupIngressResponse
--
--         , revokeSnapshotAccessResponseTest $
--             revokeSnapshotAccessResponse
--
--         , rotateEncryptionKeyResponseTest $
--             rotateEncryptionKeyResponse
--
--           ]
--     ]

-- Requests

authorizeClusterSecurityGroupIngressTest :: AuthorizeClusterSecurityGroupIngress -> TestTree
authorizeClusterSecurityGroupIngressTest = undefined

authorizeSnapshotAccessTest :: AuthorizeSnapshotAccess -> TestTree
authorizeSnapshotAccessTest = undefined

copyClusterSnapshotTest :: CopyClusterSnapshot -> TestTree
copyClusterSnapshotTest = undefined

createClusterTest :: CreateCluster -> TestTree
createClusterTest = undefined

createClusterParameterGroupTest :: CreateClusterParameterGroup -> TestTree
createClusterParameterGroupTest = undefined

createClusterSecurityGroupTest :: CreateClusterSecurityGroup -> TestTree
createClusterSecurityGroupTest = undefined

createClusterSnapshotTest :: CreateClusterSnapshot -> TestTree
createClusterSnapshotTest = undefined

createClusterSubnetGroupTest :: CreateClusterSubnetGroup -> TestTree
createClusterSubnetGroupTest = undefined

createEventSubscriptionTest :: CreateEventSubscription -> TestTree
createEventSubscriptionTest = undefined

createHSMClientCertificateTest :: CreateHSMClientCertificate -> TestTree
createHSMClientCertificateTest = undefined

createHSMConfigurationTest :: CreateHSMConfiguration -> TestTree
createHSMConfigurationTest = undefined

createSnapshotCopyGrantTest :: CreateSnapshotCopyGrant -> TestTree
createSnapshotCopyGrantTest = undefined

createTagsTest :: CreateTags -> TestTree
createTagsTest = undefined

deleteClusterTest :: DeleteCluster -> TestTree
deleteClusterTest = undefined

deleteClusterParameterGroupTest :: DeleteClusterParameterGroup -> TestTree
deleteClusterParameterGroupTest = undefined

deleteClusterSecurityGroupTest :: DeleteClusterSecurityGroup -> TestTree
deleteClusterSecurityGroupTest = undefined

deleteClusterSnapshotTest :: DeleteClusterSnapshot -> TestTree
deleteClusterSnapshotTest = undefined

deleteClusterSubnetGroupTest :: DeleteClusterSubnetGroup -> TestTree
deleteClusterSubnetGroupTest = undefined

deleteEventSubscriptionTest :: DeleteEventSubscription -> TestTree
deleteEventSubscriptionTest = undefined

deleteHSMClientCertificateTest :: DeleteHSMClientCertificate -> TestTree
deleteHSMClientCertificateTest = undefined

deleteHSMConfigurationTest :: DeleteHSMConfiguration -> TestTree
deleteHSMConfigurationTest = undefined

deleteSnapshotCopyGrantTest :: DeleteSnapshotCopyGrant -> TestTree
deleteSnapshotCopyGrantTest = undefined

deleteTagsTest :: DeleteTags -> TestTree
deleteTagsTest = undefined

describeClusterParameterGroupsTest :: DescribeClusterParameterGroups -> TestTree
describeClusterParameterGroupsTest = undefined

describeClusterParametersTest :: DescribeClusterParameters -> TestTree
describeClusterParametersTest = undefined

describeClusterSecurityGroupsTest :: DescribeClusterSecurityGroups -> TestTree
describeClusterSecurityGroupsTest = undefined

describeClusterSnapshotsTest :: DescribeClusterSnapshots -> TestTree
describeClusterSnapshotsTest = undefined

describeClusterSubnetGroupsTest :: DescribeClusterSubnetGroups -> TestTree
describeClusterSubnetGroupsTest = undefined

describeClusterVersionsTest :: DescribeClusterVersions -> TestTree
describeClusterVersionsTest = undefined

describeClustersTest :: DescribeClusters -> TestTree
describeClustersTest = undefined

describeDefaultClusterParametersTest :: DescribeDefaultClusterParameters -> TestTree
describeDefaultClusterParametersTest = undefined

describeEventCategoriesTest :: DescribeEventCategories -> TestTree
describeEventCategoriesTest = undefined

describeEventSubscriptionsTest :: DescribeEventSubscriptions -> TestTree
describeEventSubscriptionsTest = undefined

describeEventsTest :: DescribeEvents -> TestTree
describeEventsTest = undefined

describeHSMClientCertificatesTest :: DescribeHSMClientCertificates -> TestTree
describeHSMClientCertificatesTest = undefined

describeHSMConfigurationsTest :: DescribeHSMConfigurations -> TestTree
describeHSMConfigurationsTest = undefined

describeLoggingStatusTest :: DescribeLoggingStatus -> TestTree
describeLoggingStatusTest = undefined

describeOrderableClusterOptionsTest :: DescribeOrderableClusterOptions -> TestTree
describeOrderableClusterOptionsTest = undefined

describeReservedNodeOfferingsTest :: DescribeReservedNodeOfferings -> TestTree
describeReservedNodeOfferingsTest = undefined

describeReservedNodesTest :: DescribeReservedNodes -> TestTree
describeReservedNodesTest = undefined

describeResizeTest :: DescribeResize -> TestTree
describeResizeTest = undefined

describeSnapshotCopyGrantsTest :: DescribeSnapshotCopyGrants -> TestTree
describeSnapshotCopyGrantsTest = undefined

describeTagsTest :: DescribeTags -> TestTree
describeTagsTest = undefined

disableLoggingTest :: DisableLogging -> TestTree
disableLoggingTest = undefined

disableSnapshotCopyTest :: DisableSnapshotCopy -> TestTree
disableSnapshotCopyTest = undefined

enableLoggingTest :: EnableLogging -> TestTree
enableLoggingTest = undefined

enableSnapshotCopyTest :: EnableSnapshotCopy -> TestTree
enableSnapshotCopyTest = undefined

modifyClusterTest :: ModifyCluster -> TestTree
modifyClusterTest = undefined

modifyClusterParameterGroupTest :: ModifyClusterParameterGroup -> TestTree
modifyClusterParameterGroupTest = undefined

modifyClusterSubnetGroupTest :: ModifyClusterSubnetGroup -> TestTree
modifyClusterSubnetGroupTest = undefined

modifyEventSubscriptionTest :: ModifyEventSubscription -> TestTree
modifyEventSubscriptionTest = undefined

modifySnapshotCopyRetentionPeriodTest :: ModifySnapshotCopyRetentionPeriod -> TestTree
modifySnapshotCopyRetentionPeriodTest = undefined

purchaseReservedNodeOfferingTest :: PurchaseReservedNodeOffering -> TestTree
purchaseReservedNodeOfferingTest = undefined

rebootClusterTest :: RebootCluster -> TestTree
rebootClusterTest = undefined

resetClusterParameterGroupTest :: ResetClusterParameterGroup -> TestTree
resetClusterParameterGroupTest = undefined

restoreFromClusterSnapshotTest :: RestoreFromClusterSnapshot -> TestTree
restoreFromClusterSnapshotTest = undefined

revokeClusterSecurityGroupIngressTest :: RevokeClusterSecurityGroupIngress -> TestTree
revokeClusterSecurityGroupIngressTest = undefined

revokeSnapshotAccessTest :: RevokeSnapshotAccess -> TestTree
revokeSnapshotAccessTest = undefined

rotateEncryptionKeyTest :: RotateEncryptionKey -> TestTree
rotateEncryptionKeyTest = undefined

-- Responses

authorizeClusterSecurityGroupIngressResponseTest :: AuthorizeClusterSecurityGroupIngressResponse -> TestTree
authorizeClusterSecurityGroupIngressResponseTest = resp
    "authorizeClusterSecurityGroupIngressResponse"
    "fixture/AuthorizeClusterSecurityGroupIngressResponse"
    (Proxy :: Proxy AuthorizeClusterSecurityGroupIngress)

authorizeSnapshotAccessResponseTest :: AuthorizeSnapshotAccessResponse -> TestTree
authorizeSnapshotAccessResponseTest = resp
    "authorizeSnapshotAccessResponse"
    "fixture/AuthorizeSnapshotAccessResponse"
    (Proxy :: Proxy AuthorizeSnapshotAccess)

copyClusterSnapshotResponseTest :: CopyClusterSnapshotResponse -> TestTree
copyClusterSnapshotResponseTest = resp
    "copyClusterSnapshotResponse"
    "fixture/CopyClusterSnapshotResponse"
    (Proxy :: Proxy CopyClusterSnapshot)

createClusterResponseTest :: CreateClusterResponse -> TestTree
createClusterResponseTest = resp
    "createClusterResponse"
    "fixture/CreateClusterResponse"
    (Proxy :: Proxy CreateCluster)

createClusterParameterGroupResponseTest :: CreateClusterParameterGroupResponse -> TestTree
createClusterParameterGroupResponseTest = resp
    "createClusterParameterGroupResponse"
    "fixture/CreateClusterParameterGroupResponse"
    (Proxy :: Proxy CreateClusterParameterGroup)

createClusterSecurityGroupResponseTest :: CreateClusterSecurityGroupResponse -> TestTree
createClusterSecurityGroupResponseTest = resp
    "createClusterSecurityGroupResponse"
    "fixture/CreateClusterSecurityGroupResponse"
    (Proxy :: Proxy CreateClusterSecurityGroup)

createClusterSnapshotResponseTest :: CreateClusterSnapshotResponse -> TestTree
createClusterSnapshotResponseTest = resp
    "createClusterSnapshotResponse"
    "fixture/CreateClusterSnapshotResponse"
    (Proxy :: Proxy CreateClusterSnapshot)

createClusterSubnetGroupResponseTest :: CreateClusterSubnetGroupResponse -> TestTree
createClusterSubnetGroupResponseTest = resp
    "createClusterSubnetGroupResponse"
    "fixture/CreateClusterSubnetGroupResponse"
    (Proxy :: Proxy CreateClusterSubnetGroup)

createEventSubscriptionResponseTest :: CreateEventSubscriptionResponse -> TestTree
createEventSubscriptionResponseTest = resp
    "createEventSubscriptionResponse"
    "fixture/CreateEventSubscriptionResponse"
    (Proxy :: Proxy CreateEventSubscription)

createHSMClientCertificateResponseTest :: CreateHSMClientCertificateResponse -> TestTree
createHSMClientCertificateResponseTest = resp
    "createHSMClientCertificateResponse"
    "fixture/CreateHSMClientCertificateResponse"
    (Proxy :: Proxy CreateHSMClientCertificate)

createHSMConfigurationResponseTest :: CreateHSMConfigurationResponse -> TestTree
createHSMConfigurationResponseTest = resp
    "createHSMConfigurationResponse"
    "fixture/CreateHSMConfigurationResponse"
    (Proxy :: Proxy CreateHSMConfiguration)

createSnapshotCopyGrantResponseTest :: CreateSnapshotCopyGrantResponse -> TestTree
createSnapshotCopyGrantResponseTest = resp
    "createSnapshotCopyGrantResponse"
    "fixture/CreateSnapshotCopyGrantResponse"
    (Proxy :: Proxy CreateSnapshotCopyGrant)

createTagsResponseTest :: CreateTagsResponse -> TestTree
createTagsResponseTest = resp
    "createTagsResponse"
    "fixture/CreateTagsResponse"
    (Proxy :: Proxy CreateTags)

deleteClusterResponseTest :: DeleteClusterResponse -> TestTree
deleteClusterResponseTest = resp
    "deleteClusterResponse"
    "fixture/DeleteClusterResponse"
    (Proxy :: Proxy DeleteCluster)

deleteClusterParameterGroupResponseTest :: DeleteClusterParameterGroupResponse -> TestTree
deleteClusterParameterGroupResponseTest = resp
    "deleteClusterParameterGroupResponse"
    "fixture/DeleteClusterParameterGroupResponse"
    (Proxy :: Proxy DeleteClusterParameterGroup)

deleteClusterSecurityGroupResponseTest :: DeleteClusterSecurityGroupResponse -> TestTree
deleteClusterSecurityGroupResponseTest = resp
    "deleteClusterSecurityGroupResponse"
    "fixture/DeleteClusterSecurityGroupResponse"
    (Proxy :: Proxy DeleteClusterSecurityGroup)

deleteClusterSnapshotResponseTest :: DeleteClusterSnapshotResponse -> TestTree
deleteClusterSnapshotResponseTest = resp
    "deleteClusterSnapshotResponse"
    "fixture/DeleteClusterSnapshotResponse"
    (Proxy :: Proxy DeleteClusterSnapshot)

deleteClusterSubnetGroupResponseTest :: DeleteClusterSubnetGroupResponse -> TestTree
deleteClusterSubnetGroupResponseTest = resp
    "deleteClusterSubnetGroupResponse"
    "fixture/DeleteClusterSubnetGroupResponse"
    (Proxy :: Proxy DeleteClusterSubnetGroup)

deleteEventSubscriptionResponseTest :: DeleteEventSubscriptionResponse -> TestTree
deleteEventSubscriptionResponseTest = resp
    "deleteEventSubscriptionResponse"
    "fixture/DeleteEventSubscriptionResponse"
    (Proxy :: Proxy DeleteEventSubscription)

deleteHSMClientCertificateResponseTest :: DeleteHSMClientCertificateResponse -> TestTree
deleteHSMClientCertificateResponseTest = resp
    "deleteHSMClientCertificateResponse"
    "fixture/DeleteHSMClientCertificateResponse"
    (Proxy :: Proxy DeleteHSMClientCertificate)

deleteHSMConfigurationResponseTest :: DeleteHSMConfigurationResponse -> TestTree
deleteHSMConfigurationResponseTest = resp
    "deleteHSMConfigurationResponse"
    "fixture/DeleteHSMConfigurationResponse"
    (Proxy :: Proxy DeleteHSMConfiguration)

deleteSnapshotCopyGrantResponseTest :: DeleteSnapshotCopyGrantResponse -> TestTree
deleteSnapshotCopyGrantResponseTest = resp
    "deleteSnapshotCopyGrantResponse"
    "fixture/DeleteSnapshotCopyGrantResponse"
    (Proxy :: Proxy DeleteSnapshotCopyGrant)

deleteTagsResponseTest :: DeleteTagsResponse -> TestTree
deleteTagsResponseTest = resp
    "deleteTagsResponse"
    "fixture/DeleteTagsResponse"
    (Proxy :: Proxy DeleteTags)

describeClusterParameterGroupsResponseTest :: DescribeClusterParameterGroupsResponse -> TestTree
describeClusterParameterGroupsResponseTest = resp
    "describeClusterParameterGroupsResponse"
    "fixture/DescribeClusterParameterGroupsResponse"
    (Proxy :: Proxy DescribeClusterParameterGroups)

describeClusterParametersResponseTest :: DescribeClusterParametersResponse -> TestTree
describeClusterParametersResponseTest = resp
    "describeClusterParametersResponse"
    "fixture/DescribeClusterParametersResponse"
    (Proxy :: Proxy DescribeClusterParameters)

describeClusterSecurityGroupsResponseTest :: DescribeClusterSecurityGroupsResponse -> TestTree
describeClusterSecurityGroupsResponseTest = resp
    "describeClusterSecurityGroupsResponse"
    "fixture/DescribeClusterSecurityGroupsResponse"
    (Proxy :: Proxy DescribeClusterSecurityGroups)

describeClusterSnapshotsResponseTest :: DescribeClusterSnapshotsResponse -> TestTree
describeClusterSnapshotsResponseTest = resp
    "describeClusterSnapshotsResponse"
    "fixture/DescribeClusterSnapshotsResponse"
    (Proxy :: Proxy DescribeClusterSnapshots)

describeClusterSubnetGroupsResponseTest :: DescribeClusterSubnetGroupsResponse -> TestTree
describeClusterSubnetGroupsResponseTest = resp
    "describeClusterSubnetGroupsResponse"
    "fixture/DescribeClusterSubnetGroupsResponse"
    (Proxy :: Proxy DescribeClusterSubnetGroups)

describeClusterVersionsResponseTest :: DescribeClusterVersionsResponse -> TestTree
describeClusterVersionsResponseTest = resp
    "describeClusterVersionsResponse"
    "fixture/DescribeClusterVersionsResponse"
    (Proxy :: Proxy DescribeClusterVersions)

describeClustersResponseTest :: DescribeClustersResponse -> TestTree
describeClustersResponseTest = resp
    "describeClustersResponse"
    "fixture/DescribeClustersResponse"
    (Proxy :: Proxy DescribeClusters)

describeDefaultClusterParametersResponseTest :: DescribeDefaultClusterParametersResponse -> TestTree
describeDefaultClusterParametersResponseTest = resp
    "describeDefaultClusterParametersResponse"
    "fixture/DescribeDefaultClusterParametersResponse"
    (Proxy :: Proxy DescribeDefaultClusterParameters)

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

describeHSMClientCertificatesResponseTest :: DescribeHSMClientCertificatesResponse -> TestTree
describeHSMClientCertificatesResponseTest = resp
    "describeHSMClientCertificatesResponse"
    "fixture/DescribeHSMClientCertificatesResponse"
    (Proxy :: Proxy DescribeHSMClientCertificates)

describeHSMConfigurationsResponseTest :: DescribeHSMConfigurationsResponse -> TestTree
describeHSMConfigurationsResponseTest = resp
    "describeHSMConfigurationsResponse"
    "fixture/DescribeHSMConfigurationsResponse"
    (Proxy :: Proxy DescribeHSMConfigurations)

describeLoggingStatusResponseTest :: LoggingStatus -> TestTree
describeLoggingStatusResponseTest = resp
    "describeLoggingStatusResponse"
    "fixture/LoggingStatus"
    (Proxy :: Proxy DescribeLoggingStatus)

describeOrderableClusterOptionsResponseTest :: DescribeOrderableClusterOptionsResponse -> TestTree
describeOrderableClusterOptionsResponseTest = resp
    "describeOrderableClusterOptionsResponse"
    "fixture/DescribeOrderableClusterOptionsResponse"
    (Proxy :: Proxy DescribeOrderableClusterOptions)

describeReservedNodeOfferingsResponseTest :: DescribeReservedNodeOfferingsResponse -> TestTree
describeReservedNodeOfferingsResponseTest = resp
    "describeReservedNodeOfferingsResponse"
    "fixture/DescribeReservedNodeOfferingsResponse"
    (Proxy :: Proxy DescribeReservedNodeOfferings)

describeReservedNodesResponseTest :: DescribeReservedNodesResponse -> TestTree
describeReservedNodesResponseTest = resp
    "describeReservedNodesResponse"
    "fixture/DescribeReservedNodesResponse"
    (Proxy :: Proxy DescribeReservedNodes)

describeResizeResponseTest :: DescribeResizeResponse -> TestTree
describeResizeResponseTest = resp
    "describeResizeResponse"
    "fixture/DescribeResizeResponse"
    (Proxy :: Proxy DescribeResize)

describeSnapshotCopyGrantsResponseTest :: DescribeSnapshotCopyGrantsResponse -> TestTree
describeSnapshotCopyGrantsResponseTest = resp
    "describeSnapshotCopyGrantsResponse"
    "fixture/DescribeSnapshotCopyGrantsResponse"
    (Proxy :: Proxy DescribeSnapshotCopyGrants)

describeTagsResponseTest :: DescribeTagsResponse -> TestTree
describeTagsResponseTest = resp
    "describeTagsResponse"
    "fixture/DescribeTagsResponse"
    (Proxy :: Proxy DescribeTags)

disableLoggingResponseTest :: LoggingStatus -> TestTree
disableLoggingResponseTest = resp
    "disableLoggingResponse"
    "fixture/LoggingStatus"
    (Proxy :: Proxy DisableLogging)

disableSnapshotCopyResponseTest :: DisableSnapshotCopyResponse -> TestTree
disableSnapshotCopyResponseTest = resp
    "disableSnapshotCopyResponse"
    "fixture/DisableSnapshotCopyResponse"
    (Proxy :: Proxy DisableSnapshotCopy)

enableLoggingResponseTest :: LoggingStatus -> TestTree
enableLoggingResponseTest = resp
    "enableLoggingResponse"
    "fixture/LoggingStatus"
    (Proxy :: Proxy EnableLogging)

enableSnapshotCopyResponseTest :: EnableSnapshotCopyResponse -> TestTree
enableSnapshotCopyResponseTest = resp
    "enableSnapshotCopyResponse"
    "fixture/EnableSnapshotCopyResponse"
    (Proxy :: Proxy EnableSnapshotCopy)

modifyClusterResponseTest :: ModifyClusterResponse -> TestTree
modifyClusterResponseTest = resp
    "modifyClusterResponse"
    "fixture/ModifyClusterResponse"
    (Proxy :: Proxy ModifyCluster)

modifyClusterParameterGroupResponseTest :: ClusterParameterGroupNameMessage -> TestTree
modifyClusterParameterGroupResponseTest = resp
    "modifyClusterParameterGroupResponse"
    "fixture/ClusterParameterGroupNameMessage"
    (Proxy :: Proxy ModifyClusterParameterGroup)

modifyClusterSubnetGroupResponseTest :: ModifyClusterSubnetGroupResponse -> TestTree
modifyClusterSubnetGroupResponseTest = resp
    "modifyClusterSubnetGroupResponse"
    "fixture/ModifyClusterSubnetGroupResponse"
    (Proxy :: Proxy ModifyClusterSubnetGroup)

modifyEventSubscriptionResponseTest :: ModifyEventSubscriptionResponse -> TestTree
modifyEventSubscriptionResponseTest = resp
    "modifyEventSubscriptionResponse"
    "fixture/ModifyEventSubscriptionResponse"
    (Proxy :: Proxy ModifyEventSubscription)

modifySnapshotCopyRetentionPeriodResponseTest :: ModifySnapshotCopyRetentionPeriodResponse -> TestTree
modifySnapshotCopyRetentionPeriodResponseTest = resp
    "modifySnapshotCopyRetentionPeriodResponse"
    "fixture/ModifySnapshotCopyRetentionPeriodResponse"
    (Proxy :: Proxy ModifySnapshotCopyRetentionPeriod)

purchaseReservedNodeOfferingResponseTest :: PurchaseReservedNodeOfferingResponse -> TestTree
purchaseReservedNodeOfferingResponseTest = resp
    "purchaseReservedNodeOfferingResponse"
    "fixture/PurchaseReservedNodeOfferingResponse"
    (Proxy :: Proxy PurchaseReservedNodeOffering)

rebootClusterResponseTest :: RebootClusterResponse -> TestTree
rebootClusterResponseTest = resp
    "rebootClusterResponse"
    "fixture/RebootClusterResponse"
    (Proxy :: Proxy RebootCluster)

resetClusterParameterGroupResponseTest :: ClusterParameterGroupNameMessage -> TestTree
resetClusterParameterGroupResponseTest = resp
    "resetClusterParameterGroupResponse"
    "fixture/ClusterParameterGroupNameMessage"
    (Proxy :: Proxy ResetClusterParameterGroup)

restoreFromClusterSnapshotResponseTest :: RestoreFromClusterSnapshotResponse -> TestTree
restoreFromClusterSnapshotResponseTest = resp
    "restoreFromClusterSnapshotResponse"
    "fixture/RestoreFromClusterSnapshotResponse"
    (Proxy :: Proxy RestoreFromClusterSnapshot)

revokeClusterSecurityGroupIngressResponseTest :: RevokeClusterSecurityGroupIngressResponse -> TestTree
revokeClusterSecurityGroupIngressResponseTest = resp
    "revokeClusterSecurityGroupIngressResponse"
    "fixture/RevokeClusterSecurityGroupIngressResponse"
    (Proxy :: Proxy RevokeClusterSecurityGroupIngress)

revokeSnapshotAccessResponseTest :: RevokeSnapshotAccessResponse -> TestTree
revokeSnapshotAccessResponseTest = resp
    "revokeSnapshotAccessResponse"
    "fixture/RevokeSnapshotAccessResponse"
    (Proxy :: Proxy RevokeSnapshotAccess)

rotateEncryptionKeyResponseTest :: RotateEncryptionKeyResponse -> TestTree
rotateEncryptionKeyResponseTest = resp
    "rotateEncryptionKeyResponse"
    "fixture/RotateEncryptionKeyResponse"
    (Proxy :: Proxy RotateEncryptionKey)
