{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Redshift
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.Redshift where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.Redshift
import Test.AWS.Redshift.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testDescribeClusters $
--             describeClusters
--
--         , testDescribeTags $
--             describeTags
--
--         , testModifyEventSubscription $
--             modifyEventSubscription
--
--         , testDisableLogging $
--             disableLogging
--
--         , testPurchaseReservedNodeOffering $
--             purchaseReservedNodeOffering
--
--         , testDeleteClusterSubnetGroup $
--             deleteClusterSubnetGroup
--
--         , testDeleteClusterSnapshot $
--             deleteClusterSnapshot
--
--         , testDescribeEvents $
--             describeEvents
--
--         , testDescribeReservedNodeOfferings $
--             describeReservedNodeOfferings
--
--         , testDescribeClusterParameterGroups $
--             describeClusterParameterGroups
--
--         , testCreateClusterSubnetGroup $
--             createClusterSubnetGroup
--
--         , testDescribeReservedNodes $
--             describeReservedNodes
--
--         , testEnableLogging $
--             enableLogging
--
--         , testCreateTags $
--             createTags
--
--         , testDescribeClusterSecurityGroups $
--             describeClusterSecurityGroups
--
--         , testDeleteClusterParameterGroup $
--             deleteClusterParameterGroup
--
--         , testDeleteTags $
--             deleteTags
--
--         , testEnableSnapshotCopy $
--             enableSnapshotCopy
--
--         , testModifySnapshotCopyRetentionPeriod $
--             modifySnapshotCopyRetentionPeriod
--
--         , testDescribeClusterSnapshots $
--             describeClusterSnapshots
--
--         , testDescribeClusterSubnetGroups $
--             describeClusterSubnetGroups
--
--         , testAuthorizeSnapshotAccess $
--             authorizeSnapshotAccess
--
--         , testCreateEventSubscription $
--             createEventSubscription
--
--         , testRebootCluster $
--             rebootCluster
--
--         , testDescribeOrderableClusterOptions $
--             describeOrderableClusterOptions
--
--         , testDeleteCluster $
--             deleteCluster
--
--         , testDeleteEventSubscription $
--             deleteEventSubscription
--
--         , testDescribeDefaultClusterParameters $
--             describeDefaultClusterParameters
--
--         , testCreateCluster $
--             createCluster
--
--         , testCreateHSMClientCertificate $
--             createHSMClientCertificate
--
--         , testResetClusterParameterGroup $
--             resetClusterParameterGroup
--
--         , testDescribeEventSubscriptions $
--             describeEventSubscriptions
--
--         , testDescribeHSMClientCertificates $
--             describeHSMClientCertificates
--
--         , testModifyClusterParameterGroup $
--             modifyClusterParameterGroup
--
--         , testRevokeClusterSecurityGroupIngress $
--             revokeClusterSecurityGroupIngress
--
--         , testAuthorizeClusterSecurityGroupIngress $
--             authorizeClusterSecurityGroupIngress
--
--         , testCreateClusterSecurityGroup $
--             createClusterSecurityGroup
--
--         , testDescribeResize $
--             describeResize
--
--         , testDescribeEventCategories $
--             describeEventCategories
--
--         , testDeleteHSMConfiguration $
--             deleteHSMConfiguration
--
--         , testDeleteClusterSecurityGroup $
--             deleteClusterSecurityGroup
--
--         , testCreateHSMConfiguration $
--             createHSMConfiguration
--
--         , testModifyCluster $
--             modifyCluster
--
--         , testCreateClusterSnapshot $
--             createClusterSnapshot
--
--         , testDescribeLoggingStatus $
--             describeLoggingStatus
--
--         , testDescribeClusterParameters $
--             describeClusterParameters
--
--         , testDisableSnapshotCopy $
--             disableSnapshotCopy
--
--         , testRestoreFromClusterSnapshot $
--             restoreFromClusterSnapshot
--
--         , testDescribeHSMConfigurations $
--             describeHSMConfigurations
--
--         , testCreateClusterParameterGroup $
--             createClusterParameterGroup
--
--         , testRevokeSnapshotAccess $
--             revokeSnapshotAccess
--
--         , testDeleteHSMClientCertificate $
--             deleteHSMClientCertificate
--
--         , testCreateSnapshotCopyGrant $
--             createSnapshotCopyGrant
--
--         , testCopyClusterSnapshot $
--             copyClusterSnapshot
--
--         , testDescribeClusterVersions $
--             describeClusterVersions
--
--         , testModifyClusterSubnetGroup $
--             modifyClusterSubnetGroup
--
--         , testDeleteSnapshotCopyGrant $
--             deleteSnapshotCopyGrant
--
--         , testDescribeSnapshotCopyGrants $
--             describeSnapshotCopyGrants
--
--         , testRotateEncryptionKey $
--             rotateEncryptionKey
--
--           ]

--     , testGroup "response"
--         [ testDescribeClustersResponse $
--             describeClustersResponse
--
--         , testDescribeTagsResponse $
--             describeTagsResponse
--
--         , testModifyEventSubscriptionResponse $
--             modifyEventSubscriptionResponse
--
--         , testDisableLoggingResponse $
--             loggingStatus
--
--         , testPurchaseReservedNodeOfferingResponse $
--             purchaseReservedNodeOfferingResponse
--
--         , testDeleteClusterSubnetGroupResponse $
--             deleteClusterSubnetGroupResponse
--
--         , testDeleteClusterSnapshotResponse $
--             deleteClusterSnapshotResponse
--
--         , testDescribeEventsResponse $
--             describeEventsResponse
--
--         , testDescribeReservedNodeOfferingsResponse $
--             describeReservedNodeOfferingsResponse
--
--         , testDescribeClusterParameterGroupsResponse $
--             describeClusterParameterGroupsResponse
--
--         , testCreateClusterSubnetGroupResponse $
--             createClusterSubnetGroupResponse
--
--         , testDescribeReservedNodesResponse $
--             describeReservedNodesResponse
--
--         , testEnableLoggingResponse $
--             loggingStatus
--
--         , testCreateTagsResponse $
--             createTagsResponse
--
--         , testDescribeClusterSecurityGroupsResponse $
--             describeClusterSecurityGroupsResponse
--
--         , testDeleteClusterParameterGroupResponse $
--             deleteClusterParameterGroupResponse
--
--         , testDeleteTagsResponse $
--             deleteTagsResponse
--
--         , testEnableSnapshotCopyResponse $
--             enableSnapshotCopyResponse
--
--         , testModifySnapshotCopyRetentionPeriodResponse $
--             modifySnapshotCopyRetentionPeriodResponse
--
--         , testDescribeClusterSnapshotsResponse $
--             describeClusterSnapshotsResponse
--
--         , testDescribeClusterSubnetGroupsResponse $
--             describeClusterSubnetGroupsResponse
--
--         , testAuthorizeSnapshotAccessResponse $
--             authorizeSnapshotAccessResponse
--
--         , testCreateEventSubscriptionResponse $
--             createEventSubscriptionResponse
--
--         , testRebootClusterResponse $
--             rebootClusterResponse
--
--         , testDescribeOrderableClusterOptionsResponse $
--             describeOrderableClusterOptionsResponse
--
--         , testDeleteClusterResponse $
--             deleteClusterResponse
--
--         , testDeleteEventSubscriptionResponse $
--             deleteEventSubscriptionResponse
--
--         , testDescribeDefaultClusterParametersResponse $
--             describeDefaultClusterParametersResponse
--
--         , testCreateClusterResponse $
--             createClusterResponse
--
--         , testCreateHSMClientCertificateResponse $
--             createHSMClientCertificateResponse
--
--         , testResetClusterParameterGroupResponse $
--             clusterParameterGroupNameMessage
--
--         , testDescribeEventSubscriptionsResponse $
--             describeEventSubscriptionsResponse
--
--         , testDescribeHSMClientCertificatesResponse $
--             describeHSMClientCertificatesResponse
--
--         , testModifyClusterParameterGroupResponse $
--             clusterParameterGroupNameMessage
--
--         , testRevokeClusterSecurityGroupIngressResponse $
--             revokeClusterSecurityGroupIngressResponse
--
--         , testAuthorizeClusterSecurityGroupIngressResponse $
--             authorizeClusterSecurityGroupIngressResponse
--
--         , testCreateClusterSecurityGroupResponse $
--             createClusterSecurityGroupResponse
--
--         , testDescribeResizeResponse $
--             describeResizeResponse
--
--         , testDescribeEventCategoriesResponse $
--             describeEventCategoriesResponse
--
--         , testDeleteHSMConfigurationResponse $
--             deleteHSMConfigurationResponse
--
--         , testDeleteClusterSecurityGroupResponse $
--             deleteClusterSecurityGroupResponse
--
--         , testCreateHSMConfigurationResponse $
--             createHSMConfigurationResponse
--
--         , testModifyClusterResponse $
--             modifyClusterResponse
--
--         , testCreateClusterSnapshotResponse $
--             createClusterSnapshotResponse
--
--         , testDescribeLoggingStatusResponse $
--             loggingStatus
--
--         , testDescribeClusterParametersResponse $
--             describeClusterParametersResponse
--
--         , testDisableSnapshotCopyResponse $
--             disableSnapshotCopyResponse
--
--         , testRestoreFromClusterSnapshotResponse $
--             restoreFromClusterSnapshotResponse
--
--         , testDescribeHSMConfigurationsResponse $
--             describeHSMConfigurationsResponse
--
--         , testCreateClusterParameterGroupResponse $
--             createClusterParameterGroupResponse
--
--         , testRevokeSnapshotAccessResponse $
--             revokeSnapshotAccessResponse
--
--         , testDeleteHSMClientCertificateResponse $
--             deleteHSMClientCertificateResponse
--
--         , testCreateSnapshotCopyGrantResponse $
--             createSnapshotCopyGrantResponse
--
--         , testCopyClusterSnapshotResponse $
--             copyClusterSnapshotResponse
--
--         , testDescribeClusterVersionsResponse $
--             describeClusterVersionsResponse
--
--         , testModifyClusterSubnetGroupResponse $
--             modifyClusterSubnetGroupResponse
--
--         , testDeleteSnapshotCopyGrantResponse $
--             deleteSnapshotCopyGrantResponse
--
--         , testDescribeSnapshotCopyGrantsResponse $
--             describeSnapshotCopyGrantsResponse
--
--         , testRotateEncryptionKeyResponse $
--             rotateEncryptionKeyResponse
--
--           ]
--     ]

-- Requests

testDescribeClusters :: DescribeClusters -> TestTree
testDescribeClusters = req
    "DescribeClusters"
    "fixture/DescribeClusters"

testDescribeTags :: DescribeTags -> TestTree
testDescribeTags = req
    "DescribeTags"
    "fixture/DescribeTags"

testModifyEventSubscription :: ModifyEventSubscription -> TestTree
testModifyEventSubscription = req
    "ModifyEventSubscription"
    "fixture/ModifyEventSubscription"

testDisableLogging :: DisableLogging -> TestTree
testDisableLogging = req
    "DisableLogging"
    "fixture/DisableLogging"

testPurchaseReservedNodeOffering :: PurchaseReservedNodeOffering -> TestTree
testPurchaseReservedNodeOffering = req
    "PurchaseReservedNodeOffering"
    "fixture/PurchaseReservedNodeOffering"

testDeleteClusterSubnetGroup :: DeleteClusterSubnetGroup -> TestTree
testDeleteClusterSubnetGroup = req
    "DeleteClusterSubnetGroup"
    "fixture/DeleteClusterSubnetGroup"

testDeleteClusterSnapshot :: DeleteClusterSnapshot -> TestTree
testDeleteClusterSnapshot = req
    "DeleteClusterSnapshot"
    "fixture/DeleteClusterSnapshot"

testDescribeEvents :: DescribeEvents -> TestTree
testDescribeEvents = req
    "DescribeEvents"
    "fixture/DescribeEvents"

testDescribeReservedNodeOfferings :: DescribeReservedNodeOfferings -> TestTree
testDescribeReservedNodeOfferings = req
    "DescribeReservedNodeOfferings"
    "fixture/DescribeReservedNodeOfferings"

testDescribeClusterParameterGroups :: DescribeClusterParameterGroups -> TestTree
testDescribeClusterParameterGroups = req
    "DescribeClusterParameterGroups"
    "fixture/DescribeClusterParameterGroups"

testCreateClusterSubnetGroup :: CreateClusterSubnetGroup -> TestTree
testCreateClusterSubnetGroup = req
    "CreateClusterSubnetGroup"
    "fixture/CreateClusterSubnetGroup"

testDescribeReservedNodes :: DescribeReservedNodes -> TestTree
testDescribeReservedNodes = req
    "DescribeReservedNodes"
    "fixture/DescribeReservedNodes"

testEnableLogging :: EnableLogging -> TestTree
testEnableLogging = req
    "EnableLogging"
    "fixture/EnableLogging"

testCreateTags :: CreateTags -> TestTree
testCreateTags = req
    "CreateTags"
    "fixture/CreateTags"

testDescribeClusterSecurityGroups :: DescribeClusterSecurityGroups -> TestTree
testDescribeClusterSecurityGroups = req
    "DescribeClusterSecurityGroups"
    "fixture/DescribeClusterSecurityGroups"

testDeleteClusterParameterGroup :: DeleteClusterParameterGroup -> TestTree
testDeleteClusterParameterGroup = req
    "DeleteClusterParameterGroup"
    "fixture/DeleteClusterParameterGroup"

testDeleteTags :: DeleteTags -> TestTree
testDeleteTags = req
    "DeleteTags"
    "fixture/DeleteTags"

testEnableSnapshotCopy :: EnableSnapshotCopy -> TestTree
testEnableSnapshotCopy = req
    "EnableSnapshotCopy"
    "fixture/EnableSnapshotCopy"

testModifySnapshotCopyRetentionPeriod :: ModifySnapshotCopyRetentionPeriod -> TestTree
testModifySnapshotCopyRetentionPeriod = req
    "ModifySnapshotCopyRetentionPeriod"
    "fixture/ModifySnapshotCopyRetentionPeriod"

testDescribeClusterSnapshots :: DescribeClusterSnapshots -> TestTree
testDescribeClusterSnapshots = req
    "DescribeClusterSnapshots"
    "fixture/DescribeClusterSnapshots"

testDescribeClusterSubnetGroups :: DescribeClusterSubnetGroups -> TestTree
testDescribeClusterSubnetGroups = req
    "DescribeClusterSubnetGroups"
    "fixture/DescribeClusterSubnetGroups"

testAuthorizeSnapshotAccess :: AuthorizeSnapshotAccess -> TestTree
testAuthorizeSnapshotAccess = req
    "AuthorizeSnapshotAccess"
    "fixture/AuthorizeSnapshotAccess"

testCreateEventSubscription :: CreateEventSubscription -> TestTree
testCreateEventSubscription = req
    "CreateEventSubscription"
    "fixture/CreateEventSubscription"

testRebootCluster :: RebootCluster -> TestTree
testRebootCluster = req
    "RebootCluster"
    "fixture/RebootCluster"

testDescribeOrderableClusterOptions :: DescribeOrderableClusterOptions -> TestTree
testDescribeOrderableClusterOptions = req
    "DescribeOrderableClusterOptions"
    "fixture/DescribeOrderableClusterOptions"

testDeleteCluster :: DeleteCluster -> TestTree
testDeleteCluster = req
    "DeleteCluster"
    "fixture/DeleteCluster"

testDeleteEventSubscription :: DeleteEventSubscription -> TestTree
testDeleteEventSubscription = req
    "DeleteEventSubscription"
    "fixture/DeleteEventSubscription"

testDescribeDefaultClusterParameters :: DescribeDefaultClusterParameters -> TestTree
testDescribeDefaultClusterParameters = req
    "DescribeDefaultClusterParameters"
    "fixture/DescribeDefaultClusterParameters"

testCreateCluster :: CreateCluster -> TestTree
testCreateCluster = req
    "CreateCluster"
    "fixture/CreateCluster"

testCreateHSMClientCertificate :: CreateHSMClientCertificate -> TestTree
testCreateHSMClientCertificate = req
    "CreateHSMClientCertificate"
    "fixture/CreateHSMClientCertificate"

testResetClusterParameterGroup :: ResetClusterParameterGroup -> TestTree
testResetClusterParameterGroup = req
    "ResetClusterParameterGroup"
    "fixture/ResetClusterParameterGroup"

testDescribeEventSubscriptions :: DescribeEventSubscriptions -> TestTree
testDescribeEventSubscriptions = req
    "DescribeEventSubscriptions"
    "fixture/DescribeEventSubscriptions"

testDescribeHSMClientCertificates :: DescribeHSMClientCertificates -> TestTree
testDescribeHSMClientCertificates = req
    "DescribeHSMClientCertificates"
    "fixture/DescribeHSMClientCertificates"

testModifyClusterParameterGroup :: ModifyClusterParameterGroup -> TestTree
testModifyClusterParameterGroup = req
    "ModifyClusterParameterGroup"
    "fixture/ModifyClusterParameterGroup"

testRevokeClusterSecurityGroupIngress :: RevokeClusterSecurityGroupIngress -> TestTree
testRevokeClusterSecurityGroupIngress = req
    "RevokeClusterSecurityGroupIngress"
    "fixture/RevokeClusterSecurityGroupIngress"

testAuthorizeClusterSecurityGroupIngress :: AuthorizeClusterSecurityGroupIngress -> TestTree
testAuthorizeClusterSecurityGroupIngress = req
    "AuthorizeClusterSecurityGroupIngress"
    "fixture/AuthorizeClusterSecurityGroupIngress"

testCreateClusterSecurityGroup :: CreateClusterSecurityGroup -> TestTree
testCreateClusterSecurityGroup = req
    "CreateClusterSecurityGroup"
    "fixture/CreateClusterSecurityGroup"

testDescribeResize :: DescribeResize -> TestTree
testDescribeResize = req
    "DescribeResize"
    "fixture/DescribeResize"

testDescribeEventCategories :: DescribeEventCategories -> TestTree
testDescribeEventCategories = req
    "DescribeEventCategories"
    "fixture/DescribeEventCategories"

testDeleteHSMConfiguration :: DeleteHSMConfiguration -> TestTree
testDeleteHSMConfiguration = req
    "DeleteHSMConfiguration"
    "fixture/DeleteHSMConfiguration"

testDeleteClusterSecurityGroup :: DeleteClusterSecurityGroup -> TestTree
testDeleteClusterSecurityGroup = req
    "DeleteClusterSecurityGroup"
    "fixture/DeleteClusterSecurityGroup"

testCreateHSMConfiguration :: CreateHSMConfiguration -> TestTree
testCreateHSMConfiguration = req
    "CreateHSMConfiguration"
    "fixture/CreateHSMConfiguration"

testModifyCluster :: ModifyCluster -> TestTree
testModifyCluster = req
    "ModifyCluster"
    "fixture/ModifyCluster"

testCreateClusterSnapshot :: CreateClusterSnapshot -> TestTree
testCreateClusterSnapshot = req
    "CreateClusterSnapshot"
    "fixture/CreateClusterSnapshot"

testDescribeLoggingStatus :: DescribeLoggingStatus -> TestTree
testDescribeLoggingStatus = req
    "DescribeLoggingStatus"
    "fixture/DescribeLoggingStatus"

testDescribeClusterParameters :: DescribeClusterParameters -> TestTree
testDescribeClusterParameters = req
    "DescribeClusterParameters"
    "fixture/DescribeClusterParameters"

testDisableSnapshotCopy :: DisableSnapshotCopy -> TestTree
testDisableSnapshotCopy = req
    "DisableSnapshotCopy"
    "fixture/DisableSnapshotCopy"

testRestoreFromClusterSnapshot :: RestoreFromClusterSnapshot -> TestTree
testRestoreFromClusterSnapshot = req
    "RestoreFromClusterSnapshot"
    "fixture/RestoreFromClusterSnapshot"

testDescribeHSMConfigurations :: DescribeHSMConfigurations -> TestTree
testDescribeHSMConfigurations = req
    "DescribeHSMConfigurations"
    "fixture/DescribeHSMConfigurations"

testCreateClusterParameterGroup :: CreateClusterParameterGroup -> TestTree
testCreateClusterParameterGroup = req
    "CreateClusterParameterGroup"
    "fixture/CreateClusterParameterGroup"

testRevokeSnapshotAccess :: RevokeSnapshotAccess -> TestTree
testRevokeSnapshotAccess = req
    "RevokeSnapshotAccess"
    "fixture/RevokeSnapshotAccess"

testDeleteHSMClientCertificate :: DeleteHSMClientCertificate -> TestTree
testDeleteHSMClientCertificate = req
    "DeleteHSMClientCertificate"
    "fixture/DeleteHSMClientCertificate"

testCreateSnapshotCopyGrant :: CreateSnapshotCopyGrant -> TestTree
testCreateSnapshotCopyGrant = req
    "CreateSnapshotCopyGrant"
    "fixture/CreateSnapshotCopyGrant"

testCopyClusterSnapshot :: CopyClusterSnapshot -> TestTree
testCopyClusterSnapshot = req
    "CopyClusterSnapshot"
    "fixture/CopyClusterSnapshot"

testDescribeClusterVersions :: DescribeClusterVersions -> TestTree
testDescribeClusterVersions = req
    "DescribeClusterVersions"
    "fixture/DescribeClusterVersions"

testModifyClusterSubnetGroup :: ModifyClusterSubnetGroup -> TestTree
testModifyClusterSubnetGroup = req
    "ModifyClusterSubnetGroup"
    "fixture/ModifyClusterSubnetGroup"

testDeleteSnapshotCopyGrant :: DeleteSnapshotCopyGrant -> TestTree
testDeleteSnapshotCopyGrant = req
    "DeleteSnapshotCopyGrant"
    "fixture/DeleteSnapshotCopyGrant"

testDescribeSnapshotCopyGrants :: DescribeSnapshotCopyGrants -> TestTree
testDescribeSnapshotCopyGrants = req
    "DescribeSnapshotCopyGrants"
    "fixture/DescribeSnapshotCopyGrants"

testRotateEncryptionKey :: RotateEncryptionKey -> TestTree
testRotateEncryptionKey = req
    "RotateEncryptionKey"
    "fixture/RotateEncryptionKey"

-- Responses

testDescribeClustersResponse :: DescribeClustersResponse -> TestTree
testDescribeClustersResponse = res
    "DescribeClustersResponse"
    "fixture/DescribeClustersResponse"
    (Proxy :: Proxy DescribeClusters)

testDescribeTagsResponse :: DescribeTagsResponse -> TestTree
testDescribeTagsResponse = res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse"
    (Proxy :: Proxy DescribeTags)

testModifyEventSubscriptionResponse :: ModifyEventSubscriptionResponse -> TestTree
testModifyEventSubscriptionResponse = res
    "ModifyEventSubscriptionResponse"
    "fixture/ModifyEventSubscriptionResponse"
    (Proxy :: Proxy ModifyEventSubscription)

testDisableLoggingResponse :: LoggingStatus -> TestTree
testDisableLoggingResponse = res
    "DisableLoggingResponse"
    "fixture/DisableLoggingResponse"
    (Proxy :: Proxy DisableLogging)

testPurchaseReservedNodeOfferingResponse :: PurchaseReservedNodeOfferingResponse -> TestTree
testPurchaseReservedNodeOfferingResponse = res
    "PurchaseReservedNodeOfferingResponse"
    "fixture/PurchaseReservedNodeOfferingResponse"
    (Proxy :: Proxy PurchaseReservedNodeOffering)

testDeleteClusterSubnetGroupResponse :: DeleteClusterSubnetGroupResponse -> TestTree
testDeleteClusterSubnetGroupResponse = res
    "DeleteClusterSubnetGroupResponse"
    "fixture/DeleteClusterSubnetGroupResponse"
    (Proxy :: Proxy DeleteClusterSubnetGroup)

testDeleteClusterSnapshotResponse :: DeleteClusterSnapshotResponse -> TestTree
testDeleteClusterSnapshotResponse = res
    "DeleteClusterSnapshotResponse"
    "fixture/DeleteClusterSnapshotResponse"
    (Proxy :: Proxy DeleteClusterSnapshot)

testDescribeEventsResponse :: DescribeEventsResponse -> TestTree
testDescribeEventsResponse = res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse"
    (Proxy :: Proxy DescribeEvents)

testDescribeReservedNodeOfferingsResponse :: DescribeReservedNodeOfferingsResponse -> TestTree
testDescribeReservedNodeOfferingsResponse = res
    "DescribeReservedNodeOfferingsResponse"
    "fixture/DescribeReservedNodeOfferingsResponse"
    (Proxy :: Proxy DescribeReservedNodeOfferings)

testDescribeClusterParameterGroupsResponse :: DescribeClusterParameterGroupsResponse -> TestTree
testDescribeClusterParameterGroupsResponse = res
    "DescribeClusterParameterGroupsResponse"
    "fixture/DescribeClusterParameterGroupsResponse"
    (Proxy :: Proxy DescribeClusterParameterGroups)

testCreateClusterSubnetGroupResponse :: CreateClusterSubnetGroupResponse -> TestTree
testCreateClusterSubnetGroupResponse = res
    "CreateClusterSubnetGroupResponse"
    "fixture/CreateClusterSubnetGroupResponse"
    (Proxy :: Proxy CreateClusterSubnetGroup)

testDescribeReservedNodesResponse :: DescribeReservedNodesResponse -> TestTree
testDescribeReservedNodesResponse = res
    "DescribeReservedNodesResponse"
    "fixture/DescribeReservedNodesResponse"
    (Proxy :: Proxy DescribeReservedNodes)

testEnableLoggingResponse :: LoggingStatus -> TestTree
testEnableLoggingResponse = res
    "EnableLoggingResponse"
    "fixture/EnableLoggingResponse"
    (Proxy :: Proxy EnableLogging)

testCreateTagsResponse :: CreateTagsResponse -> TestTree
testCreateTagsResponse = res
    "CreateTagsResponse"
    "fixture/CreateTagsResponse"
    (Proxy :: Proxy CreateTags)

testDescribeClusterSecurityGroupsResponse :: DescribeClusterSecurityGroupsResponse -> TestTree
testDescribeClusterSecurityGroupsResponse = res
    "DescribeClusterSecurityGroupsResponse"
    "fixture/DescribeClusterSecurityGroupsResponse"
    (Proxy :: Proxy DescribeClusterSecurityGroups)

testDeleteClusterParameterGroupResponse :: DeleteClusterParameterGroupResponse -> TestTree
testDeleteClusterParameterGroupResponse = res
    "DeleteClusterParameterGroupResponse"
    "fixture/DeleteClusterParameterGroupResponse"
    (Proxy :: Proxy DeleteClusterParameterGroup)

testDeleteTagsResponse :: DeleteTagsResponse -> TestTree
testDeleteTagsResponse = res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse"
    (Proxy :: Proxy DeleteTags)

testEnableSnapshotCopyResponse :: EnableSnapshotCopyResponse -> TestTree
testEnableSnapshotCopyResponse = res
    "EnableSnapshotCopyResponse"
    "fixture/EnableSnapshotCopyResponse"
    (Proxy :: Proxy EnableSnapshotCopy)

testModifySnapshotCopyRetentionPeriodResponse :: ModifySnapshotCopyRetentionPeriodResponse -> TestTree
testModifySnapshotCopyRetentionPeriodResponse = res
    "ModifySnapshotCopyRetentionPeriodResponse"
    "fixture/ModifySnapshotCopyRetentionPeriodResponse"
    (Proxy :: Proxy ModifySnapshotCopyRetentionPeriod)

testDescribeClusterSnapshotsResponse :: DescribeClusterSnapshotsResponse -> TestTree
testDescribeClusterSnapshotsResponse = res
    "DescribeClusterSnapshotsResponse"
    "fixture/DescribeClusterSnapshotsResponse"
    (Proxy :: Proxy DescribeClusterSnapshots)

testDescribeClusterSubnetGroupsResponse :: DescribeClusterSubnetGroupsResponse -> TestTree
testDescribeClusterSubnetGroupsResponse = res
    "DescribeClusterSubnetGroupsResponse"
    "fixture/DescribeClusterSubnetGroupsResponse"
    (Proxy :: Proxy DescribeClusterSubnetGroups)

testAuthorizeSnapshotAccessResponse :: AuthorizeSnapshotAccessResponse -> TestTree
testAuthorizeSnapshotAccessResponse = res
    "AuthorizeSnapshotAccessResponse"
    "fixture/AuthorizeSnapshotAccessResponse"
    (Proxy :: Proxy AuthorizeSnapshotAccess)

testCreateEventSubscriptionResponse :: CreateEventSubscriptionResponse -> TestTree
testCreateEventSubscriptionResponse = res
    "CreateEventSubscriptionResponse"
    "fixture/CreateEventSubscriptionResponse"
    (Proxy :: Proxy CreateEventSubscription)

testRebootClusterResponse :: RebootClusterResponse -> TestTree
testRebootClusterResponse = res
    "RebootClusterResponse"
    "fixture/RebootClusterResponse"
    (Proxy :: Proxy RebootCluster)

testDescribeOrderableClusterOptionsResponse :: DescribeOrderableClusterOptionsResponse -> TestTree
testDescribeOrderableClusterOptionsResponse = res
    "DescribeOrderableClusterOptionsResponse"
    "fixture/DescribeOrderableClusterOptionsResponse"
    (Proxy :: Proxy DescribeOrderableClusterOptions)

testDeleteClusterResponse :: DeleteClusterResponse -> TestTree
testDeleteClusterResponse = res
    "DeleteClusterResponse"
    "fixture/DeleteClusterResponse"
    (Proxy :: Proxy DeleteCluster)

testDeleteEventSubscriptionResponse :: DeleteEventSubscriptionResponse -> TestTree
testDeleteEventSubscriptionResponse = res
    "DeleteEventSubscriptionResponse"
    "fixture/DeleteEventSubscriptionResponse"
    (Proxy :: Proxy DeleteEventSubscription)

testDescribeDefaultClusterParametersResponse :: DescribeDefaultClusterParametersResponse -> TestTree
testDescribeDefaultClusterParametersResponse = res
    "DescribeDefaultClusterParametersResponse"
    "fixture/DescribeDefaultClusterParametersResponse"
    (Proxy :: Proxy DescribeDefaultClusterParameters)

testCreateClusterResponse :: CreateClusterResponse -> TestTree
testCreateClusterResponse = res
    "CreateClusterResponse"
    "fixture/CreateClusterResponse"
    (Proxy :: Proxy CreateCluster)

testCreateHSMClientCertificateResponse :: CreateHSMClientCertificateResponse -> TestTree
testCreateHSMClientCertificateResponse = res
    "CreateHSMClientCertificateResponse"
    "fixture/CreateHSMClientCertificateResponse"
    (Proxy :: Proxy CreateHSMClientCertificate)

testResetClusterParameterGroupResponse :: ClusterParameterGroupNameMessage -> TestTree
testResetClusterParameterGroupResponse = res
    "ResetClusterParameterGroupResponse"
    "fixture/ResetClusterParameterGroupResponse"
    (Proxy :: Proxy ResetClusterParameterGroup)

testDescribeEventSubscriptionsResponse :: DescribeEventSubscriptionsResponse -> TestTree
testDescribeEventSubscriptionsResponse = res
    "DescribeEventSubscriptionsResponse"
    "fixture/DescribeEventSubscriptionsResponse"
    (Proxy :: Proxy DescribeEventSubscriptions)

testDescribeHSMClientCertificatesResponse :: DescribeHSMClientCertificatesResponse -> TestTree
testDescribeHSMClientCertificatesResponse = res
    "DescribeHSMClientCertificatesResponse"
    "fixture/DescribeHSMClientCertificatesResponse"
    (Proxy :: Proxy DescribeHSMClientCertificates)

testModifyClusterParameterGroupResponse :: ClusterParameterGroupNameMessage -> TestTree
testModifyClusterParameterGroupResponse = res
    "ModifyClusterParameterGroupResponse"
    "fixture/ModifyClusterParameterGroupResponse"
    (Proxy :: Proxy ModifyClusterParameterGroup)

testRevokeClusterSecurityGroupIngressResponse :: RevokeClusterSecurityGroupIngressResponse -> TestTree
testRevokeClusterSecurityGroupIngressResponse = res
    "RevokeClusterSecurityGroupIngressResponse"
    "fixture/RevokeClusterSecurityGroupIngressResponse"
    (Proxy :: Proxy RevokeClusterSecurityGroupIngress)

testAuthorizeClusterSecurityGroupIngressResponse :: AuthorizeClusterSecurityGroupIngressResponse -> TestTree
testAuthorizeClusterSecurityGroupIngressResponse = res
    "AuthorizeClusterSecurityGroupIngressResponse"
    "fixture/AuthorizeClusterSecurityGroupIngressResponse"
    (Proxy :: Proxy AuthorizeClusterSecurityGroupIngress)

testCreateClusterSecurityGroupResponse :: CreateClusterSecurityGroupResponse -> TestTree
testCreateClusterSecurityGroupResponse = res
    "CreateClusterSecurityGroupResponse"
    "fixture/CreateClusterSecurityGroupResponse"
    (Proxy :: Proxy CreateClusterSecurityGroup)

testDescribeResizeResponse :: DescribeResizeResponse -> TestTree
testDescribeResizeResponse = res
    "DescribeResizeResponse"
    "fixture/DescribeResizeResponse"
    (Proxy :: Proxy DescribeResize)

testDescribeEventCategoriesResponse :: DescribeEventCategoriesResponse -> TestTree
testDescribeEventCategoriesResponse = res
    "DescribeEventCategoriesResponse"
    "fixture/DescribeEventCategoriesResponse"
    (Proxy :: Proxy DescribeEventCategories)

testDeleteHSMConfigurationResponse :: DeleteHSMConfigurationResponse -> TestTree
testDeleteHSMConfigurationResponse = res
    "DeleteHSMConfigurationResponse"
    "fixture/DeleteHSMConfigurationResponse"
    (Proxy :: Proxy DeleteHSMConfiguration)

testDeleteClusterSecurityGroupResponse :: DeleteClusterSecurityGroupResponse -> TestTree
testDeleteClusterSecurityGroupResponse = res
    "DeleteClusterSecurityGroupResponse"
    "fixture/DeleteClusterSecurityGroupResponse"
    (Proxy :: Proxy DeleteClusterSecurityGroup)

testCreateHSMConfigurationResponse :: CreateHSMConfigurationResponse -> TestTree
testCreateHSMConfigurationResponse = res
    "CreateHSMConfigurationResponse"
    "fixture/CreateHSMConfigurationResponse"
    (Proxy :: Proxy CreateHSMConfiguration)

testModifyClusterResponse :: ModifyClusterResponse -> TestTree
testModifyClusterResponse = res
    "ModifyClusterResponse"
    "fixture/ModifyClusterResponse"
    (Proxy :: Proxy ModifyCluster)

testCreateClusterSnapshotResponse :: CreateClusterSnapshotResponse -> TestTree
testCreateClusterSnapshotResponse = res
    "CreateClusterSnapshotResponse"
    "fixture/CreateClusterSnapshotResponse"
    (Proxy :: Proxy CreateClusterSnapshot)

testDescribeLoggingStatusResponse :: LoggingStatus -> TestTree
testDescribeLoggingStatusResponse = res
    "DescribeLoggingStatusResponse"
    "fixture/DescribeLoggingStatusResponse"
    (Proxy :: Proxy DescribeLoggingStatus)

testDescribeClusterParametersResponse :: DescribeClusterParametersResponse -> TestTree
testDescribeClusterParametersResponse = res
    "DescribeClusterParametersResponse"
    "fixture/DescribeClusterParametersResponse"
    (Proxy :: Proxy DescribeClusterParameters)

testDisableSnapshotCopyResponse :: DisableSnapshotCopyResponse -> TestTree
testDisableSnapshotCopyResponse = res
    "DisableSnapshotCopyResponse"
    "fixture/DisableSnapshotCopyResponse"
    (Proxy :: Proxy DisableSnapshotCopy)

testRestoreFromClusterSnapshotResponse :: RestoreFromClusterSnapshotResponse -> TestTree
testRestoreFromClusterSnapshotResponse = res
    "RestoreFromClusterSnapshotResponse"
    "fixture/RestoreFromClusterSnapshotResponse"
    (Proxy :: Proxy RestoreFromClusterSnapshot)

testDescribeHSMConfigurationsResponse :: DescribeHSMConfigurationsResponse -> TestTree
testDescribeHSMConfigurationsResponse = res
    "DescribeHSMConfigurationsResponse"
    "fixture/DescribeHSMConfigurationsResponse"
    (Proxy :: Proxy DescribeHSMConfigurations)

testCreateClusterParameterGroupResponse :: CreateClusterParameterGroupResponse -> TestTree
testCreateClusterParameterGroupResponse = res
    "CreateClusterParameterGroupResponse"
    "fixture/CreateClusterParameterGroupResponse"
    (Proxy :: Proxy CreateClusterParameterGroup)

testRevokeSnapshotAccessResponse :: RevokeSnapshotAccessResponse -> TestTree
testRevokeSnapshotAccessResponse = res
    "RevokeSnapshotAccessResponse"
    "fixture/RevokeSnapshotAccessResponse"
    (Proxy :: Proxy RevokeSnapshotAccess)

testDeleteHSMClientCertificateResponse :: DeleteHSMClientCertificateResponse -> TestTree
testDeleteHSMClientCertificateResponse = res
    "DeleteHSMClientCertificateResponse"
    "fixture/DeleteHSMClientCertificateResponse"
    (Proxy :: Proxy DeleteHSMClientCertificate)

testCreateSnapshotCopyGrantResponse :: CreateSnapshotCopyGrantResponse -> TestTree
testCreateSnapshotCopyGrantResponse = res
    "CreateSnapshotCopyGrantResponse"
    "fixture/CreateSnapshotCopyGrantResponse"
    (Proxy :: Proxy CreateSnapshotCopyGrant)

testCopyClusterSnapshotResponse :: CopyClusterSnapshotResponse -> TestTree
testCopyClusterSnapshotResponse = res
    "CopyClusterSnapshotResponse"
    "fixture/CopyClusterSnapshotResponse"
    (Proxy :: Proxy CopyClusterSnapshot)

testDescribeClusterVersionsResponse :: DescribeClusterVersionsResponse -> TestTree
testDescribeClusterVersionsResponse = res
    "DescribeClusterVersionsResponse"
    "fixture/DescribeClusterVersionsResponse"
    (Proxy :: Proxy DescribeClusterVersions)

testModifyClusterSubnetGroupResponse :: ModifyClusterSubnetGroupResponse -> TestTree
testModifyClusterSubnetGroupResponse = res
    "ModifyClusterSubnetGroupResponse"
    "fixture/ModifyClusterSubnetGroupResponse"
    (Proxy :: Proxy ModifyClusterSubnetGroup)

testDeleteSnapshotCopyGrantResponse :: DeleteSnapshotCopyGrantResponse -> TestTree
testDeleteSnapshotCopyGrantResponse = res
    "DeleteSnapshotCopyGrantResponse"
    "fixture/DeleteSnapshotCopyGrantResponse"
    (Proxy :: Proxy DeleteSnapshotCopyGrant)

testDescribeSnapshotCopyGrantsResponse :: DescribeSnapshotCopyGrantsResponse -> TestTree
testDescribeSnapshotCopyGrantsResponse = res
    "DescribeSnapshotCopyGrantsResponse"
    "fixture/DescribeSnapshotCopyGrantsResponse"
    (Proxy :: Proxy DescribeSnapshotCopyGrants)

testRotateEncryptionKeyResponse :: RotateEncryptionKeyResponse -> TestTree
testRotateEncryptionKeyResponse = res
    "RotateEncryptionKeyResponse"
    "fixture/RotateEncryptionKeyResponse"
    (Proxy :: Proxy RotateEncryptionKey)
