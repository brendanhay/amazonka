{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Redshift
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
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
    "fixture/DescribeClusters.yaml"

testDescribeTags :: DescribeTags -> TestTree
testDescribeTags = req
    "DescribeTags"
    "fixture/DescribeTags.yaml"

testModifyEventSubscription :: ModifyEventSubscription -> TestTree
testModifyEventSubscription = req
    "ModifyEventSubscription"
    "fixture/ModifyEventSubscription.yaml"

testDisableLogging :: DisableLogging -> TestTree
testDisableLogging = req
    "DisableLogging"
    "fixture/DisableLogging.yaml"

testPurchaseReservedNodeOffering :: PurchaseReservedNodeOffering -> TestTree
testPurchaseReservedNodeOffering = req
    "PurchaseReservedNodeOffering"
    "fixture/PurchaseReservedNodeOffering.yaml"

testDeleteClusterSubnetGroup :: DeleteClusterSubnetGroup -> TestTree
testDeleteClusterSubnetGroup = req
    "DeleteClusterSubnetGroup"
    "fixture/DeleteClusterSubnetGroup.yaml"

testDeleteClusterSnapshot :: DeleteClusterSnapshot -> TestTree
testDeleteClusterSnapshot = req
    "DeleteClusterSnapshot"
    "fixture/DeleteClusterSnapshot.yaml"

testDescribeEvents :: DescribeEvents -> TestTree
testDescribeEvents = req
    "DescribeEvents"
    "fixture/DescribeEvents.yaml"

testDescribeReservedNodeOfferings :: DescribeReservedNodeOfferings -> TestTree
testDescribeReservedNodeOfferings = req
    "DescribeReservedNodeOfferings"
    "fixture/DescribeReservedNodeOfferings.yaml"

testDescribeClusterParameterGroups :: DescribeClusterParameterGroups -> TestTree
testDescribeClusterParameterGroups = req
    "DescribeClusterParameterGroups"
    "fixture/DescribeClusterParameterGroups.yaml"

testCreateClusterSubnetGroup :: CreateClusterSubnetGroup -> TestTree
testCreateClusterSubnetGroup = req
    "CreateClusterSubnetGroup"
    "fixture/CreateClusterSubnetGroup.yaml"

testDescribeReservedNodes :: DescribeReservedNodes -> TestTree
testDescribeReservedNodes = req
    "DescribeReservedNodes"
    "fixture/DescribeReservedNodes.yaml"

testEnableLogging :: EnableLogging -> TestTree
testEnableLogging = req
    "EnableLogging"
    "fixture/EnableLogging.yaml"

testCreateTags :: CreateTags -> TestTree
testCreateTags = req
    "CreateTags"
    "fixture/CreateTags.yaml"

testDescribeClusterSecurityGroups :: DescribeClusterSecurityGroups -> TestTree
testDescribeClusterSecurityGroups = req
    "DescribeClusterSecurityGroups"
    "fixture/DescribeClusterSecurityGroups.yaml"

testDeleteClusterParameterGroup :: DeleteClusterParameterGroup -> TestTree
testDeleteClusterParameterGroup = req
    "DeleteClusterParameterGroup"
    "fixture/DeleteClusterParameterGroup.yaml"

testDeleteTags :: DeleteTags -> TestTree
testDeleteTags = req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

testEnableSnapshotCopy :: EnableSnapshotCopy -> TestTree
testEnableSnapshotCopy = req
    "EnableSnapshotCopy"
    "fixture/EnableSnapshotCopy.yaml"

testModifySnapshotCopyRetentionPeriod :: ModifySnapshotCopyRetentionPeriod -> TestTree
testModifySnapshotCopyRetentionPeriod = req
    "ModifySnapshotCopyRetentionPeriod"
    "fixture/ModifySnapshotCopyRetentionPeriod.yaml"

testDescribeClusterSnapshots :: DescribeClusterSnapshots -> TestTree
testDescribeClusterSnapshots = req
    "DescribeClusterSnapshots"
    "fixture/DescribeClusterSnapshots.yaml"

testDescribeClusterSubnetGroups :: DescribeClusterSubnetGroups -> TestTree
testDescribeClusterSubnetGroups = req
    "DescribeClusterSubnetGroups"
    "fixture/DescribeClusterSubnetGroups.yaml"

testAuthorizeSnapshotAccess :: AuthorizeSnapshotAccess -> TestTree
testAuthorizeSnapshotAccess = req
    "AuthorizeSnapshotAccess"
    "fixture/AuthorizeSnapshotAccess.yaml"

testCreateEventSubscription :: CreateEventSubscription -> TestTree
testCreateEventSubscription = req
    "CreateEventSubscription"
    "fixture/CreateEventSubscription.yaml"

testRebootCluster :: RebootCluster -> TestTree
testRebootCluster = req
    "RebootCluster"
    "fixture/RebootCluster.yaml"

testDescribeOrderableClusterOptions :: DescribeOrderableClusterOptions -> TestTree
testDescribeOrderableClusterOptions = req
    "DescribeOrderableClusterOptions"
    "fixture/DescribeOrderableClusterOptions.yaml"

testDeleteCluster :: DeleteCluster -> TestTree
testDeleteCluster = req
    "DeleteCluster"
    "fixture/DeleteCluster.yaml"

testDeleteEventSubscription :: DeleteEventSubscription -> TestTree
testDeleteEventSubscription = req
    "DeleteEventSubscription"
    "fixture/DeleteEventSubscription.yaml"

testDescribeDefaultClusterParameters :: DescribeDefaultClusterParameters -> TestTree
testDescribeDefaultClusterParameters = req
    "DescribeDefaultClusterParameters"
    "fixture/DescribeDefaultClusterParameters.yaml"

testCreateCluster :: CreateCluster -> TestTree
testCreateCluster = req
    "CreateCluster"
    "fixture/CreateCluster.yaml"

testCreateHSMClientCertificate :: CreateHSMClientCertificate -> TestTree
testCreateHSMClientCertificate = req
    "CreateHSMClientCertificate"
    "fixture/CreateHSMClientCertificate.yaml"

testResetClusterParameterGroup :: ResetClusterParameterGroup -> TestTree
testResetClusterParameterGroup = req
    "ResetClusterParameterGroup"
    "fixture/ResetClusterParameterGroup.yaml"

testDescribeEventSubscriptions :: DescribeEventSubscriptions -> TestTree
testDescribeEventSubscriptions = req
    "DescribeEventSubscriptions"
    "fixture/DescribeEventSubscriptions.yaml"

testDescribeHSMClientCertificates :: DescribeHSMClientCertificates -> TestTree
testDescribeHSMClientCertificates = req
    "DescribeHSMClientCertificates"
    "fixture/DescribeHSMClientCertificates.yaml"

testModifyClusterParameterGroup :: ModifyClusterParameterGroup -> TestTree
testModifyClusterParameterGroup = req
    "ModifyClusterParameterGroup"
    "fixture/ModifyClusterParameterGroup.yaml"

testRevokeClusterSecurityGroupIngress :: RevokeClusterSecurityGroupIngress -> TestTree
testRevokeClusterSecurityGroupIngress = req
    "RevokeClusterSecurityGroupIngress"
    "fixture/RevokeClusterSecurityGroupIngress.yaml"

testAuthorizeClusterSecurityGroupIngress :: AuthorizeClusterSecurityGroupIngress -> TestTree
testAuthorizeClusterSecurityGroupIngress = req
    "AuthorizeClusterSecurityGroupIngress"
    "fixture/AuthorizeClusterSecurityGroupIngress.yaml"

testCreateClusterSecurityGroup :: CreateClusterSecurityGroup -> TestTree
testCreateClusterSecurityGroup = req
    "CreateClusterSecurityGroup"
    "fixture/CreateClusterSecurityGroup.yaml"

testDescribeResize :: DescribeResize -> TestTree
testDescribeResize = req
    "DescribeResize"
    "fixture/DescribeResize.yaml"

testDescribeEventCategories :: DescribeEventCategories -> TestTree
testDescribeEventCategories = req
    "DescribeEventCategories"
    "fixture/DescribeEventCategories.yaml"

testDeleteHSMConfiguration :: DeleteHSMConfiguration -> TestTree
testDeleteHSMConfiguration = req
    "DeleteHSMConfiguration"
    "fixture/DeleteHSMConfiguration.yaml"

testDeleteClusterSecurityGroup :: DeleteClusterSecurityGroup -> TestTree
testDeleteClusterSecurityGroup = req
    "DeleteClusterSecurityGroup"
    "fixture/DeleteClusterSecurityGroup.yaml"

testCreateHSMConfiguration :: CreateHSMConfiguration -> TestTree
testCreateHSMConfiguration = req
    "CreateHSMConfiguration"
    "fixture/CreateHSMConfiguration.yaml"

testModifyCluster :: ModifyCluster -> TestTree
testModifyCluster = req
    "ModifyCluster"
    "fixture/ModifyCluster.yaml"

testCreateClusterSnapshot :: CreateClusterSnapshot -> TestTree
testCreateClusterSnapshot = req
    "CreateClusterSnapshot"
    "fixture/CreateClusterSnapshot.yaml"

testDescribeLoggingStatus :: DescribeLoggingStatus -> TestTree
testDescribeLoggingStatus = req
    "DescribeLoggingStatus"
    "fixture/DescribeLoggingStatus.yaml"

testDescribeClusterParameters :: DescribeClusterParameters -> TestTree
testDescribeClusterParameters = req
    "DescribeClusterParameters"
    "fixture/DescribeClusterParameters.yaml"

testDisableSnapshotCopy :: DisableSnapshotCopy -> TestTree
testDisableSnapshotCopy = req
    "DisableSnapshotCopy"
    "fixture/DisableSnapshotCopy.yaml"

testRestoreFromClusterSnapshot :: RestoreFromClusterSnapshot -> TestTree
testRestoreFromClusterSnapshot = req
    "RestoreFromClusterSnapshot"
    "fixture/RestoreFromClusterSnapshot.yaml"

testDescribeHSMConfigurations :: DescribeHSMConfigurations -> TestTree
testDescribeHSMConfigurations = req
    "DescribeHSMConfigurations"
    "fixture/DescribeHSMConfigurations.yaml"

testCreateClusterParameterGroup :: CreateClusterParameterGroup -> TestTree
testCreateClusterParameterGroup = req
    "CreateClusterParameterGroup"
    "fixture/CreateClusterParameterGroup.yaml"

testRevokeSnapshotAccess :: RevokeSnapshotAccess -> TestTree
testRevokeSnapshotAccess = req
    "RevokeSnapshotAccess"
    "fixture/RevokeSnapshotAccess.yaml"

testDeleteHSMClientCertificate :: DeleteHSMClientCertificate -> TestTree
testDeleteHSMClientCertificate = req
    "DeleteHSMClientCertificate"
    "fixture/DeleteHSMClientCertificate.yaml"

testCreateSnapshotCopyGrant :: CreateSnapshotCopyGrant -> TestTree
testCreateSnapshotCopyGrant = req
    "CreateSnapshotCopyGrant"
    "fixture/CreateSnapshotCopyGrant.yaml"

testCopyClusterSnapshot :: CopyClusterSnapshot -> TestTree
testCopyClusterSnapshot = req
    "CopyClusterSnapshot"
    "fixture/CopyClusterSnapshot.yaml"

testDescribeClusterVersions :: DescribeClusterVersions -> TestTree
testDescribeClusterVersions = req
    "DescribeClusterVersions"
    "fixture/DescribeClusterVersions.yaml"

testModifyClusterSubnetGroup :: ModifyClusterSubnetGroup -> TestTree
testModifyClusterSubnetGroup = req
    "ModifyClusterSubnetGroup"
    "fixture/ModifyClusterSubnetGroup.yaml"

testDeleteSnapshotCopyGrant :: DeleteSnapshotCopyGrant -> TestTree
testDeleteSnapshotCopyGrant = req
    "DeleteSnapshotCopyGrant"
    "fixture/DeleteSnapshotCopyGrant.yaml"

testDescribeSnapshotCopyGrants :: DescribeSnapshotCopyGrants -> TestTree
testDescribeSnapshotCopyGrants = req
    "DescribeSnapshotCopyGrants"
    "fixture/DescribeSnapshotCopyGrants.yaml"

testRotateEncryptionKey :: RotateEncryptionKey -> TestTree
testRotateEncryptionKey = req
    "RotateEncryptionKey"
    "fixture/RotateEncryptionKey.yaml"

-- Responses

testDescribeClustersResponse :: DescribeClustersResponse -> TestTree
testDescribeClustersResponse = res
    "DescribeClustersResponse"
    "fixture/DescribeClustersResponse.proto"
    redshift
    (Proxy :: Proxy DescribeClusters)

testDescribeTagsResponse :: DescribeTagsResponse -> TestTree
testDescribeTagsResponse = res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeTags)

testModifyEventSubscriptionResponse :: ModifyEventSubscriptionResponse -> TestTree
testModifyEventSubscriptionResponse = res
    "ModifyEventSubscriptionResponse"
    "fixture/ModifyEventSubscriptionResponse.proto"
    redshift
    (Proxy :: Proxy ModifyEventSubscription)

testDisableLoggingResponse :: LoggingStatus -> TestTree
testDisableLoggingResponse = res
    "DisableLoggingResponse"
    "fixture/DisableLoggingResponse.proto"
    redshift
    (Proxy :: Proxy DisableLogging)

testPurchaseReservedNodeOfferingResponse :: PurchaseReservedNodeOfferingResponse -> TestTree
testPurchaseReservedNodeOfferingResponse = res
    "PurchaseReservedNodeOfferingResponse"
    "fixture/PurchaseReservedNodeOfferingResponse.proto"
    redshift
    (Proxy :: Proxy PurchaseReservedNodeOffering)

testDeleteClusterSubnetGroupResponse :: DeleteClusterSubnetGroupResponse -> TestTree
testDeleteClusterSubnetGroupResponse = res
    "DeleteClusterSubnetGroupResponse"
    "fixture/DeleteClusterSubnetGroupResponse.proto"
    redshift
    (Proxy :: Proxy DeleteClusterSubnetGroup)

testDeleteClusterSnapshotResponse :: DeleteClusterSnapshotResponse -> TestTree
testDeleteClusterSnapshotResponse = res
    "DeleteClusterSnapshotResponse"
    "fixture/DeleteClusterSnapshotResponse.proto"
    redshift
    (Proxy :: Proxy DeleteClusterSnapshot)

testDescribeEventsResponse :: DescribeEventsResponse -> TestTree
testDescribeEventsResponse = res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeEvents)

testDescribeReservedNodeOfferingsResponse :: DescribeReservedNodeOfferingsResponse -> TestTree
testDescribeReservedNodeOfferingsResponse = res
    "DescribeReservedNodeOfferingsResponse"
    "fixture/DescribeReservedNodeOfferingsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeReservedNodeOfferings)

testDescribeClusterParameterGroupsResponse :: DescribeClusterParameterGroupsResponse -> TestTree
testDescribeClusterParameterGroupsResponse = res
    "DescribeClusterParameterGroupsResponse"
    "fixture/DescribeClusterParameterGroupsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeClusterParameterGroups)

testCreateClusterSubnetGroupResponse :: CreateClusterSubnetGroupResponse -> TestTree
testCreateClusterSubnetGroupResponse = res
    "CreateClusterSubnetGroupResponse"
    "fixture/CreateClusterSubnetGroupResponse.proto"
    redshift
    (Proxy :: Proxy CreateClusterSubnetGroup)

testDescribeReservedNodesResponse :: DescribeReservedNodesResponse -> TestTree
testDescribeReservedNodesResponse = res
    "DescribeReservedNodesResponse"
    "fixture/DescribeReservedNodesResponse.proto"
    redshift
    (Proxy :: Proxy DescribeReservedNodes)

testEnableLoggingResponse :: LoggingStatus -> TestTree
testEnableLoggingResponse = res
    "EnableLoggingResponse"
    "fixture/EnableLoggingResponse.proto"
    redshift
    (Proxy :: Proxy EnableLogging)

testCreateTagsResponse :: CreateTagsResponse -> TestTree
testCreateTagsResponse = res
    "CreateTagsResponse"
    "fixture/CreateTagsResponse.proto"
    redshift
    (Proxy :: Proxy CreateTags)

testDescribeClusterSecurityGroupsResponse :: DescribeClusterSecurityGroupsResponse -> TestTree
testDescribeClusterSecurityGroupsResponse = res
    "DescribeClusterSecurityGroupsResponse"
    "fixture/DescribeClusterSecurityGroupsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeClusterSecurityGroups)

testDeleteClusterParameterGroupResponse :: DeleteClusterParameterGroupResponse -> TestTree
testDeleteClusterParameterGroupResponse = res
    "DeleteClusterParameterGroupResponse"
    "fixture/DeleteClusterParameterGroupResponse.proto"
    redshift
    (Proxy :: Proxy DeleteClusterParameterGroup)

testDeleteTagsResponse :: DeleteTagsResponse -> TestTree
testDeleteTagsResponse = res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    redshift
    (Proxy :: Proxy DeleteTags)

testEnableSnapshotCopyResponse :: EnableSnapshotCopyResponse -> TestTree
testEnableSnapshotCopyResponse = res
    "EnableSnapshotCopyResponse"
    "fixture/EnableSnapshotCopyResponse.proto"
    redshift
    (Proxy :: Proxy EnableSnapshotCopy)

testModifySnapshotCopyRetentionPeriodResponse :: ModifySnapshotCopyRetentionPeriodResponse -> TestTree
testModifySnapshotCopyRetentionPeriodResponse = res
    "ModifySnapshotCopyRetentionPeriodResponse"
    "fixture/ModifySnapshotCopyRetentionPeriodResponse.proto"
    redshift
    (Proxy :: Proxy ModifySnapshotCopyRetentionPeriod)

testDescribeClusterSnapshotsResponse :: DescribeClusterSnapshotsResponse -> TestTree
testDescribeClusterSnapshotsResponse = res
    "DescribeClusterSnapshotsResponse"
    "fixture/DescribeClusterSnapshotsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeClusterSnapshots)

testDescribeClusterSubnetGroupsResponse :: DescribeClusterSubnetGroupsResponse -> TestTree
testDescribeClusterSubnetGroupsResponse = res
    "DescribeClusterSubnetGroupsResponse"
    "fixture/DescribeClusterSubnetGroupsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeClusterSubnetGroups)

testAuthorizeSnapshotAccessResponse :: AuthorizeSnapshotAccessResponse -> TestTree
testAuthorizeSnapshotAccessResponse = res
    "AuthorizeSnapshotAccessResponse"
    "fixture/AuthorizeSnapshotAccessResponse.proto"
    redshift
    (Proxy :: Proxy AuthorizeSnapshotAccess)

testCreateEventSubscriptionResponse :: CreateEventSubscriptionResponse -> TestTree
testCreateEventSubscriptionResponse = res
    "CreateEventSubscriptionResponse"
    "fixture/CreateEventSubscriptionResponse.proto"
    redshift
    (Proxy :: Proxy CreateEventSubscription)

testRebootClusterResponse :: RebootClusterResponse -> TestTree
testRebootClusterResponse = res
    "RebootClusterResponse"
    "fixture/RebootClusterResponse.proto"
    redshift
    (Proxy :: Proxy RebootCluster)

testDescribeOrderableClusterOptionsResponse :: DescribeOrderableClusterOptionsResponse -> TestTree
testDescribeOrderableClusterOptionsResponse = res
    "DescribeOrderableClusterOptionsResponse"
    "fixture/DescribeOrderableClusterOptionsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeOrderableClusterOptions)

testDeleteClusterResponse :: DeleteClusterResponse -> TestTree
testDeleteClusterResponse = res
    "DeleteClusterResponse"
    "fixture/DeleteClusterResponse.proto"
    redshift
    (Proxy :: Proxy DeleteCluster)

testDeleteEventSubscriptionResponse :: DeleteEventSubscriptionResponse -> TestTree
testDeleteEventSubscriptionResponse = res
    "DeleteEventSubscriptionResponse"
    "fixture/DeleteEventSubscriptionResponse.proto"
    redshift
    (Proxy :: Proxy DeleteEventSubscription)

testDescribeDefaultClusterParametersResponse :: DescribeDefaultClusterParametersResponse -> TestTree
testDescribeDefaultClusterParametersResponse = res
    "DescribeDefaultClusterParametersResponse"
    "fixture/DescribeDefaultClusterParametersResponse.proto"
    redshift
    (Proxy :: Proxy DescribeDefaultClusterParameters)

testCreateClusterResponse :: CreateClusterResponse -> TestTree
testCreateClusterResponse = res
    "CreateClusterResponse"
    "fixture/CreateClusterResponse.proto"
    redshift
    (Proxy :: Proxy CreateCluster)

testCreateHSMClientCertificateResponse :: CreateHSMClientCertificateResponse -> TestTree
testCreateHSMClientCertificateResponse = res
    "CreateHSMClientCertificateResponse"
    "fixture/CreateHSMClientCertificateResponse.proto"
    redshift
    (Proxy :: Proxy CreateHSMClientCertificate)

testResetClusterParameterGroupResponse :: ClusterParameterGroupNameMessage -> TestTree
testResetClusterParameterGroupResponse = res
    "ResetClusterParameterGroupResponse"
    "fixture/ResetClusterParameterGroupResponse.proto"
    redshift
    (Proxy :: Proxy ResetClusterParameterGroup)

testDescribeEventSubscriptionsResponse :: DescribeEventSubscriptionsResponse -> TestTree
testDescribeEventSubscriptionsResponse = res
    "DescribeEventSubscriptionsResponse"
    "fixture/DescribeEventSubscriptionsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeEventSubscriptions)

testDescribeHSMClientCertificatesResponse :: DescribeHSMClientCertificatesResponse -> TestTree
testDescribeHSMClientCertificatesResponse = res
    "DescribeHSMClientCertificatesResponse"
    "fixture/DescribeHSMClientCertificatesResponse.proto"
    redshift
    (Proxy :: Proxy DescribeHSMClientCertificates)

testModifyClusterParameterGroupResponse :: ClusterParameterGroupNameMessage -> TestTree
testModifyClusterParameterGroupResponse = res
    "ModifyClusterParameterGroupResponse"
    "fixture/ModifyClusterParameterGroupResponse.proto"
    redshift
    (Proxy :: Proxy ModifyClusterParameterGroup)

testRevokeClusterSecurityGroupIngressResponse :: RevokeClusterSecurityGroupIngressResponse -> TestTree
testRevokeClusterSecurityGroupIngressResponse = res
    "RevokeClusterSecurityGroupIngressResponse"
    "fixture/RevokeClusterSecurityGroupIngressResponse.proto"
    redshift
    (Proxy :: Proxy RevokeClusterSecurityGroupIngress)

testAuthorizeClusterSecurityGroupIngressResponse :: AuthorizeClusterSecurityGroupIngressResponse -> TestTree
testAuthorizeClusterSecurityGroupIngressResponse = res
    "AuthorizeClusterSecurityGroupIngressResponse"
    "fixture/AuthorizeClusterSecurityGroupIngressResponse.proto"
    redshift
    (Proxy :: Proxy AuthorizeClusterSecurityGroupIngress)

testCreateClusterSecurityGroupResponse :: CreateClusterSecurityGroupResponse -> TestTree
testCreateClusterSecurityGroupResponse = res
    "CreateClusterSecurityGroupResponse"
    "fixture/CreateClusterSecurityGroupResponse.proto"
    redshift
    (Proxy :: Proxy CreateClusterSecurityGroup)

testDescribeResizeResponse :: DescribeResizeResponse -> TestTree
testDescribeResizeResponse = res
    "DescribeResizeResponse"
    "fixture/DescribeResizeResponse.proto"
    redshift
    (Proxy :: Proxy DescribeResize)

testDescribeEventCategoriesResponse :: DescribeEventCategoriesResponse -> TestTree
testDescribeEventCategoriesResponse = res
    "DescribeEventCategoriesResponse"
    "fixture/DescribeEventCategoriesResponse.proto"
    redshift
    (Proxy :: Proxy DescribeEventCategories)

testDeleteHSMConfigurationResponse :: DeleteHSMConfigurationResponse -> TestTree
testDeleteHSMConfigurationResponse = res
    "DeleteHSMConfigurationResponse"
    "fixture/DeleteHSMConfigurationResponse.proto"
    redshift
    (Proxy :: Proxy DeleteHSMConfiguration)

testDeleteClusterSecurityGroupResponse :: DeleteClusterSecurityGroupResponse -> TestTree
testDeleteClusterSecurityGroupResponse = res
    "DeleteClusterSecurityGroupResponse"
    "fixture/DeleteClusterSecurityGroupResponse.proto"
    redshift
    (Proxy :: Proxy DeleteClusterSecurityGroup)

testCreateHSMConfigurationResponse :: CreateHSMConfigurationResponse -> TestTree
testCreateHSMConfigurationResponse = res
    "CreateHSMConfigurationResponse"
    "fixture/CreateHSMConfigurationResponse.proto"
    redshift
    (Proxy :: Proxy CreateHSMConfiguration)

testModifyClusterResponse :: ModifyClusterResponse -> TestTree
testModifyClusterResponse = res
    "ModifyClusterResponse"
    "fixture/ModifyClusterResponse.proto"
    redshift
    (Proxy :: Proxy ModifyCluster)

testCreateClusterSnapshotResponse :: CreateClusterSnapshotResponse -> TestTree
testCreateClusterSnapshotResponse = res
    "CreateClusterSnapshotResponse"
    "fixture/CreateClusterSnapshotResponse.proto"
    redshift
    (Proxy :: Proxy CreateClusterSnapshot)

testDescribeLoggingStatusResponse :: LoggingStatus -> TestTree
testDescribeLoggingStatusResponse = res
    "DescribeLoggingStatusResponse"
    "fixture/DescribeLoggingStatusResponse.proto"
    redshift
    (Proxy :: Proxy DescribeLoggingStatus)

testDescribeClusterParametersResponse :: DescribeClusterParametersResponse -> TestTree
testDescribeClusterParametersResponse = res
    "DescribeClusterParametersResponse"
    "fixture/DescribeClusterParametersResponse.proto"
    redshift
    (Proxy :: Proxy DescribeClusterParameters)

testDisableSnapshotCopyResponse :: DisableSnapshotCopyResponse -> TestTree
testDisableSnapshotCopyResponse = res
    "DisableSnapshotCopyResponse"
    "fixture/DisableSnapshotCopyResponse.proto"
    redshift
    (Proxy :: Proxy DisableSnapshotCopy)

testRestoreFromClusterSnapshotResponse :: RestoreFromClusterSnapshotResponse -> TestTree
testRestoreFromClusterSnapshotResponse = res
    "RestoreFromClusterSnapshotResponse"
    "fixture/RestoreFromClusterSnapshotResponse.proto"
    redshift
    (Proxy :: Proxy RestoreFromClusterSnapshot)

testDescribeHSMConfigurationsResponse :: DescribeHSMConfigurationsResponse -> TestTree
testDescribeHSMConfigurationsResponse = res
    "DescribeHSMConfigurationsResponse"
    "fixture/DescribeHSMConfigurationsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeHSMConfigurations)

testCreateClusterParameterGroupResponse :: CreateClusterParameterGroupResponse -> TestTree
testCreateClusterParameterGroupResponse = res
    "CreateClusterParameterGroupResponse"
    "fixture/CreateClusterParameterGroupResponse.proto"
    redshift
    (Proxy :: Proxy CreateClusterParameterGroup)

testRevokeSnapshotAccessResponse :: RevokeSnapshotAccessResponse -> TestTree
testRevokeSnapshotAccessResponse = res
    "RevokeSnapshotAccessResponse"
    "fixture/RevokeSnapshotAccessResponse.proto"
    redshift
    (Proxy :: Proxy RevokeSnapshotAccess)

testDeleteHSMClientCertificateResponse :: DeleteHSMClientCertificateResponse -> TestTree
testDeleteHSMClientCertificateResponse = res
    "DeleteHSMClientCertificateResponse"
    "fixture/DeleteHSMClientCertificateResponse.proto"
    redshift
    (Proxy :: Proxy DeleteHSMClientCertificate)

testCreateSnapshotCopyGrantResponse :: CreateSnapshotCopyGrantResponse -> TestTree
testCreateSnapshotCopyGrantResponse = res
    "CreateSnapshotCopyGrantResponse"
    "fixture/CreateSnapshotCopyGrantResponse.proto"
    redshift
    (Proxy :: Proxy CreateSnapshotCopyGrant)

testCopyClusterSnapshotResponse :: CopyClusterSnapshotResponse -> TestTree
testCopyClusterSnapshotResponse = res
    "CopyClusterSnapshotResponse"
    "fixture/CopyClusterSnapshotResponse.proto"
    redshift
    (Proxy :: Proxy CopyClusterSnapshot)

testDescribeClusterVersionsResponse :: DescribeClusterVersionsResponse -> TestTree
testDescribeClusterVersionsResponse = res
    "DescribeClusterVersionsResponse"
    "fixture/DescribeClusterVersionsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeClusterVersions)

testModifyClusterSubnetGroupResponse :: ModifyClusterSubnetGroupResponse -> TestTree
testModifyClusterSubnetGroupResponse = res
    "ModifyClusterSubnetGroupResponse"
    "fixture/ModifyClusterSubnetGroupResponse.proto"
    redshift
    (Proxy :: Proxy ModifyClusterSubnetGroup)

testDeleteSnapshotCopyGrantResponse :: DeleteSnapshotCopyGrantResponse -> TestTree
testDeleteSnapshotCopyGrantResponse = res
    "DeleteSnapshotCopyGrantResponse"
    "fixture/DeleteSnapshotCopyGrantResponse.proto"
    redshift
    (Proxy :: Proxy DeleteSnapshotCopyGrant)

testDescribeSnapshotCopyGrantsResponse :: DescribeSnapshotCopyGrantsResponse -> TestTree
testDescribeSnapshotCopyGrantsResponse = res
    "DescribeSnapshotCopyGrantsResponse"
    "fixture/DescribeSnapshotCopyGrantsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeSnapshotCopyGrants)

testRotateEncryptionKeyResponse :: RotateEncryptionKeyResponse -> TestTree
testRotateEncryptionKeyResponse = res
    "RotateEncryptionKeyResponse"
    "fixture/RotateEncryptionKeyResponse.proto"
    redshift
    (Proxy :: Proxy RotateEncryptionKey)
