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
--         , testDeleteClusterSubnetGroup $
--             deleteClusterSubnetGroup
--
--         , testDisableLogging $
--             disableLogging
--
--         , testModifyEventSubscription $
--             modifyEventSubscription
--
--         , testDeleteClusterSnapshot $
--             deleteClusterSnapshot
--
--         , testPurchaseReservedNodeOffering $
--             purchaseReservedNodeOffering
--
--         , testDescribeReservedNodeOfferings $
--             describeReservedNodeOfferings
--
--         , testDescribeEvents $
--             describeEvents
--
--         , testDescribeReservedNodes $
--             describeReservedNodes
--
--         , testDescribeClusterParameterGroups $
--             describeClusterParameterGroups
--
--         , testEnableLogging $
--             enableLogging
--
--         , testCreateClusterSubnetGroup $
--             createClusterSubnetGroup
--
--         , testDeleteClusterParameterGroup $
--             deleteClusterParameterGroup
--
--         , testDescribeClusterSecurityGroups $
--             describeClusterSecurityGroups
--
--         , testCreateTags $
--             createTags
--
--         , testEnableSnapshotCopy $
--             enableSnapshotCopy
--
--         , testDescribeClusterSnapshots $
--             describeClusterSnapshots
--
--         , testDeleteTags $
--             deleteTags
--
--         , testDescribeClusterSubnetGroups $
--             describeClusterSubnetGroups
--
--         , testModifySnapshotCopyRetentionPeriod $
--             modifySnapshotCopyRetentionPeriod
--
--         , testAuthorizeSnapshotAccess $
--             authorizeSnapshotAccess
--
--         , testRebootCluster $
--             rebootCluster
--
--         , testDeleteCluster $
--             deleteCluster
--
--         , testCreateEventSubscription $
--             createEventSubscription
--
--         , testDescribeOrderableClusterOptions $
--             describeOrderableClusterOptions
--
--         , testCreateCluster $
--             createCluster
--
--         , testCreateHSMClientCertificate $
--             createHSMClientCertificate
--
--         , testDescribeDefaultClusterParameters $
--             describeDefaultClusterParameters
--
--         , testDeleteEventSubscription $
--             deleteEventSubscription
--
--         , testResetClusterParameterGroup $
--             resetClusterParameterGroup
--
--         , testDescribeEventSubscriptions $
--             describeEventSubscriptions
--
--         , testRevokeClusterSecurityGroupIngress $
--             revokeClusterSecurityGroupIngress
--
--         , testDescribeHSMClientCertificates $
--             describeHSMClientCertificates
--
--         , testModifyClusterParameterGroup $
--             modifyClusterParameterGroup
--
--         , testCreateClusterSecurityGroup $
--             createClusterSecurityGroup
--
--         , testDescribeEventCategories $
--             describeEventCategories
--
--         , testDescribeResize $
--             describeResize
--
--         , testDeleteHSMConfiguration $
--             deleteHSMConfiguration
--
--         , testAuthorizeClusterSecurityGroupIngress $
--             authorizeClusterSecurityGroupIngress
--
--         , testCreateClusterSnapshot $
--             createClusterSnapshot
--
--         , testCreateHSMConfiguration $
--             createHSMConfiguration
--
--         , testDescribeLoggingStatus $
--             describeLoggingStatus
--
--         , testModifyCluster $
--             modifyCluster
--
--         , testDeleteClusterSecurityGroup $
--             deleteClusterSecurityGroup
--
--         , testDisableSnapshotCopy $
--             disableSnapshotCopy
--
--         , testDescribeClusterParameters $
--             describeClusterParameters
--
--         , testRestoreFromClusterSnapshot $
--             restoreFromClusterSnapshot
--
--         , testCreateClusterParameterGroup $
--             createClusterParameterGroup
--
--         , testRevokeSnapshotAccess $
--             revokeSnapshotAccess
--
--         , testDescribeHSMConfigurations $
--             describeHSMConfigurations
--
--         , testCreateSnapshotCopyGrant $
--             createSnapshotCopyGrant
--
--         , testCopyClusterSnapshot $
--             copyClusterSnapshot
--
--         , testDeleteHSMClientCertificate $
--             deleteHSMClientCertificate
--
--         , testDeleteSnapshotCopyGrant $
--             deleteSnapshotCopyGrant
--
--         , testDescribeClusterVersions $
--             describeClusterVersions
--
--         , testModifyClusterSubnetGroup $
--             modifyClusterSubnetGroup
--
--         , testRotateEncryptionKey $
--             rotateEncryptionKey
--
--         , testDescribeSnapshotCopyGrants $
--             describeSnapshotCopyGrants
--
--           ]

--     , testGroup "response"
--         [ testDescribeClustersResponse $
--             describeClustersResponse
--
--         , testDescribeTagsResponse $
--             describeTagsResponse
--
--         , testDeleteClusterSubnetGroupResponse $
--             deleteClusterSubnetGroupResponse
--
--         , testDisableLoggingResponse $
--             loggingStatus
--
--         , testModifyEventSubscriptionResponse $
--             modifyEventSubscriptionResponse
--
--         , testDeleteClusterSnapshotResponse $
--             deleteClusterSnapshotResponse
--
--         , testPurchaseReservedNodeOfferingResponse $
--             purchaseReservedNodeOfferingResponse
--
--         , testDescribeReservedNodeOfferingsResponse $
--             describeReservedNodeOfferingsResponse
--
--         , testDescribeEventsResponse $
--             describeEventsResponse
--
--         , testDescribeReservedNodesResponse $
--             describeReservedNodesResponse
--
--         , testDescribeClusterParameterGroupsResponse $
--             describeClusterParameterGroupsResponse
--
--         , testEnableLoggingResponse $
--             loggingStatus
--
--         , testCreateClusterSubnetGroupResponse $
--             createClusterSubnetGroupResponse
--
--         , testDeleteClusterParameterGroupResponse $
--             deleteClusterParameterGroupResponse
--
--         , testDescribeClusterSecurityGroupsResponse $
--             describeClusterSecurityGroupsResponse
--
--         , testCreateTagsResponse $
--             createTagsResponse
--
--         , testEnableSnapshotCopyResponse $
--             enableSnapshotCopyResponse
--
--         , testDescribeClusterSnapshotsResponse $
--             describeClusterSnapshotsResponse
--
--         , testDeleteTagsResponse $
--             deleteTagsResponse
--
--         , testDescribeClusterSubnetGroupsResponse $
--             describeClusterSubnetGroupsResponse
--
--         , testModifySnapshotCopyRetentionPeriodResponse $
--             modifySnapshotCopyRetentionPeriodResponse
--
--         , testAuthorizeSnapshotAccessResponse $
--             authorizeSnapshotAccessResponse
--
--         , testRebootClusterResponse $
--             rebootClusterResponse
--
--         , testDeleteClusterResponse $
--             deleteClusterResponse
--
--         , testCreateEventSubscriptionResponse $
--             createEventSubscriptionResponse
--
--         , testDescribeOrderableClusterOptionsResponse $
--             describeOrderableClusterOptionsResponse
--
--         , testCreateClusterResponse $
--             createClusterResponse
--
--         , testCreateHSMClientCertificateResponse $
--             createHSMClientCertificateResponse
--
--         , testDescribeDefaultClusterParametersResponse $
--             describeDefaultClusterParametersResponse
--
--         , testDeleteEventSubscriptionResponse $
--             deleteEventSubscriptionResponse
--
--         , testResetClusterParameterGroupResponse $
--             clusterParameterGroupNameMessage
--
--         , testDescribeEventSubscriptionsResponse $
--             describeEventSubscriptionsResponse
--
--         , testRevokeClusterSecurityGroupIngressResponse $
--             revokeClusterSecurityGroupIngressResponse
--
--         , testDescribeHSMClientCertificatesResponse $
--             describeHSMClientCertificatesResponse
--
--         , testModifyClusterParameterGroupResponse $
--             clusterParameterGroupNameMessage
--
--         , testCreateClusterSecurityGroupResponse $
--             createClusterSecurityGroupResponse
--
--         , testDescribeEventCategoriesResponse $
--             describeEventCategoriesResponse
--
--         , testDescribeResizeResponse $
--             describeResizeResponse
--
--         , testDeleteHSMConfigurationResponse $
--             deleteHSMConfigurationResponse
--
--         , testAuthorizeClusterSecurityGroupIngressResponse $
--             authorizeClusterSecurityGroupIngressResponse
--
--         , testCreateClusterSnapshotResponse $
--             createClusterSnapshotResponse
--
--         , testCreateHSMConfigurationResponse $
--             createHSMConfigurationResponse
--
--         , testDescribeLoggingStatusResponse $
--             loggingStatus
--
--         , testModifyClusterResponse $
--             modifyClusterResponse
--
--         , testDeleteClusterSecurityGroupResponse $
--             deleteClusterSecurityGroupResponse
--
--         , testDisableSnapshotCopyResponse $
--             disableSnapshotCopyResponse
--
--         , testDescribeClusterParametersResponse $
--             describeClusterParametersResponse
--
--         , testRestoreFromClusterSnapshotResponse $
--             restoreFromClusterSnapshotResponse
--
--         , testCreateClusterParameterGroupResponse $
--             createClusterParameterGroupResponse
--
--         , testRevokeSnapshotAccessResponse $
--             revokeSnapshotAccessResponse
--
--         , testDescribeHSMConfigurationsResponse $
--             describeHSMConfigurationsResponse
--
--         , testCreateSnapshotCopyGrantResponse $
--             createSnapshotCopyGrantResponse
--
--         , testCopyClusterSnapshotResponse $
--             copyClusterSnapshotResponse
--
--         , testDeleteHSMClientCertificateResponse $
--             deleteHSMClientCertificateResponse
--
--         , testDeleteSnapshotCopyGrantResponse $
--             deleteSnapshotCopyGrantResponse
--
--         , testDescribeClusterVersionsResponse $
--             describeClusterVersionsResponse
--
--         , testModifyClusterSubnetGroupResponse $
--             modifyClusterSubnetGroupResponse
--
--         , testRotateEncryptionKeyResponse $
--             rotateEncryptionKeyResponse
--
--         , testDescribeSnapshotCopyGrantsResponse $
--             describeSnapshotCopyGrantsResponse
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

testDeleteClusterSubnetGroup :: DeleteClusterSubnetGroup -> TestTree
testDeleteClusterSubnetGroup = req
    "DeleteClusterSubnetGroup"
    "fixture/DeleteClusterSubnetGroup.yaml"

testDisableLogging :: DisableLogging -> TestTree
testDisableLogging = req
    "DisableLogging"
    "fixture/DisableLogging.yaml"

testModifyEventSubscription :: ModifyEventSubscription -> TestTree
testModifyEventSubscription = req
    "ModifyEventSubscription"
    "fixture/ModifyEventSubscription.yaml"

testDeleteClusterSnapshot :: DeleteClusterSnapshot -> TestTree
testDeleteClusterSnapshot = req
    "DeleteClusterSnapshot"
    "fixture/DeleteClusterSnapshot.yaml"

testPurchaseReservedNodeOffering :: PurchaseReservedNodeOffering -> TestTree
testPurchaseReservedNodeOffering = req
    "PurchaseReservedNodeOffering"
    "fixture/PurchaseReservedNodeOffering.yaml"

testDescribeReservedNodeOfferings :: DescribeReservedNodeOfferings -> TestTree
testDescribeReservedNodeOfferings = req
    "DescribeReservedNodeOfferings"
    "fixture/DescribeReservedNodeOfferings.yaml"

testDescribeEvents :: DescribeEvents -> TestTree
testDescribeEvents = req
    "DescribeEvents"
    "fixture/DescribeEvents.yaml"

testDescribeReservedNodes :: DescribeReservedNodes -> TestTree
testDescribeReservedNodes = req
    "DescribeReservedNodes"
    "fixture/DescribeReservedNodes.yaml"

testDescribeClusterParameterGroups :: DescribeClusterParameterGroups -> TestTree
testDescribeClusterParameterGroups = req
    "DescribeClusterParameterGroups"
    "fixture/DescribeClusterParameterGroups.yaml"

testEnableLogging :: EnableLogging -> TestTree
testEnableLogging = req
    "EnableLogging"
    "fixture/EnableLogging.yaml"

testCreateClusterSubnetGroup :: CreateClusterSubnetGroup -> TestTree
testCreateClusterSubnetGroup = req
    "CreateClusterSubnetGroup"
    "fixture/CreateClusterSubnetGroup.yaml"

testDeleteClusterParameterGroup :: DeleteClusterParameterGroup -> TestTree
testDeleteClusterParameterGroup = req
    "DeleteClusterParameterGroup"
    "fixture/DeleteClusterParameterGroup.yaml"

testDescribeClusterSecurityGroups :: DescribeClusterSecurityGroups -> TestTree
testDescribeClusterSecurityGroups = req
    "DescribeClusterSecurityGroups"
    "fixture/DescribeClusterSecurityGroups.yaml"

testCreateTags :: CreateTags -> TestTree
testCreateTags = req
    "CreateTags"
    "fixture/CreateTags.yaml"

testEnableSnapshotCopy :: EnableSnapshotCopy -> TestTree
testEnableSnapshotCopy = req
    "EnableSnapshotCopy"
    "fixture/EnableSnapshotCopy.yaml"

testDescribeClusterSnapshots :: DescribeClusterSnapshots -> TestTree
testDescribeClusterSnapshots = req
    "DescribeClusterSnapshots"
    "fixture/DescribeClusterSnapshots.yaml"

testDeleteTags :: DeleteTags -> TestTree
testDeleteTags = req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

testDescribeClusterSubnetGroups :: DescribeClusterSubnetGroups -> TestTree
testDescribeClusterSubnetGroups = req
    "DescribeClusterSubnetGroups"
    "fixture/DescribeClusterSubnetGroups.yaml"

testModifySnapshotCopyRetentionPeriod :: ModifySnapshotCopyRetentionPeriod -> TestTree
testModifySnapshotCopyRetentionPeriod = req
    "ModifySnapshotCopyRetentionPeriod"
    "fixture/ModifySnapshotCopyRetentionPeriod.yaml"

testAuthorizeSnapshotAccess :: AuthorizeSnapshotAccess -> TestTree
testAuthorizeSnapshotAccess = req
    "AuthorizeSnapshotAccess"
    "fixture/AuthorizeSnapshotAccess.yaml"

testRebootCluster :: RebootCluster -> TestTree
testRebootCluster = req
    "RebootCluster"
    "fixture/RebootCluster.yaml"

testDeleteCluster :: DeleteCluster -> TestTree
testDeleteCluster = req
    "DeleteCluster"
    "fixture/DeleteCluster.yaml"

testCreateEventSubscription :: CreateEventSubscription -> TestTree
testCreateEventSubscription = req
    "CreateEventSubscription"
    "fixture/CreateEventSubscription.yaml"

testDescribeOrderableClusterOptions :: DescribeOrderableClusterOptions -> TestTree
testDescribeOrderableClusterOptions = req
    "DescribeOrderableClusterOptions"
    "fixture/DescribeOrderableClusterOptions.yaml"

testCreateCluster :: CreateCluster -> TestTree
testCreateCluster = req
    "CreateCluster"
    "fixture/CreateCluster.yaml"

testCreateHSMClientCertificate :: CreateHSMClientCertificate -> TestTree
testCreateHSMClientCertificate = req
    "CreateHSMClientCertificate"
    "fixture/CreateHSMClientCertificate.yaml"

testDescribeDefaultClusterParameters :: DescribeDefaultClusterParameters -> TestTree
testDescribeDefaultClusterParameters = req
    "DescribeDefaultClusterParameters"
    "fixture/DescribeDefaultClusterParameters.yaml"

testDeleteEventSubscription :: DeleteEventSubscription -> TestTree
testDeleteEventSubscription = req
    "DeleteEventSubscription"
    "fixture/DeleteEventSubscription.yaml"

testResetClusterParameterGroup :: ResetClusterParameterGroup -> TestTree
testResetClusterParameterGroup = req
    "ResetClusterParameterGroup"
    "fixture/ResetClusterParameterGroup.yaml"

testDescribeEventSubscriptions :: DescribeEventSubscriptions -> TestTree
testDescribeEventSubscriptions = req
    "DescribeEventSubscriptions"
    "fixture/DescribeEventSubscriptions.yaml"

testRevokeClusterSecurityGroupIngress :: RevokeClusterSecurityGroupIngress -> TestTree
testRevokeClusterSecurityGroupIngress = req
    "RevokeClusterSecurityGroupIngress"
    "fixture/RevokeClusterSecurityGroupIngress.yaml"

testDescribeHSMClientCertificates :: DescribeHSMClientCertificates -> TestTree
testDescribeHSMClientCertificates = req
    "DescribeHSMClientCertificates"
    "fixture/DescribeHSMClientCertificates.yaml"

testModifyClusterParameterGroup :: ModifyClusterParameterGroup -> TestTree
testModifyClusterParameterGroup = req
    "ModifyClusterParameterGroup"
    "fixture/ModifyClusterParameterGroup.yaml"

testCreateClusterSecurityGroup :: CreateClusterSecurityGroup -> TestTree
testCreateClusterSecurityGroup = req
    "CreateClusterSecurityGroup"
    "fixture/CreateClusterSecurityGroup.yaml"

testDescribeEventCategories :: DescribeEventCategories -> TestTree
testDescribeEventCategories = req
    "DescribeEventCategories"
    "fixture/DescribeEventCategories.yaml"

testDescribeResize :: DescribeResize -> TestTree
testDescribeResize = req
    "DescribeResize"
    "fixture/DescribeResize.yaml"

testDeleteHSMConfiguration :: DeleteHSMConfiguration -> TestTree
testDeleteHSMConfiguration = req
    "DeleteHSMConfiguration"
    "fixture/DeleteHSMConfiguration.yaml"

testAuthorizeClusterSecurityGroupIngress :: AuthorizeClusterSecurityGroupIngress -> TestTree
testAuthorizeClusterSecurityGroupIngress = req
    "AuthorizeClusterSecurityGroupIngress"
    "fixture/AuthorizeClusterSecurityGroupIngress.yaml"

testCreateClusterSnapshot :: CreateClusterSnapshot -> TestTree
testCreateClusterSnapshot = req
    "CreateClusterSnapshot"
    "fixture/CreateClusterSnapshot.yaml"

testCreateHSMConfiguration :: CreateHSMConfiguration -> TestTree
testCreateHSMConfiguration = req
    "CreateHSMConfiguration"
    "fixture/CreateHSMConfiguration.yaml"

testDescribeLoggingStatus :: DescribeLoggingStatus -> TestTree
testDescribeLoggingStatus = req
    "DescribeLoggingStatus"
    "fixture/DescribeLoggingStatus.yaml"

testModifyCluster :: ModifyCluster -> TestTree
testModifyCluster = req
    "ModifyCluster"
    "fixture/ModifyCluster.yaml"

testDeleteClusterSecurityGroup :: DeleteClusterSecurityGroup -> TestTree
testDeleteClusterSecurityGroup = req
    "DeleteClusterSecurityGroup"
    "fixture/DeleteClusterSecurityGroup.yaml"

testDisableSnapshotCopy :: DisableSnapshotCopy -> TestTree
testDisableSnapshotCopy = req
    "DisableSnapshotCopy"
    "fixture/DisableSnapshotCopy.yaml"

testDescribeClusterParameters :: DescribeClusterParameters -> TestTree
testDescribeClusterParameters = req
    "DescribeClusterParameters"
    "fixture/DescribeClusterParameters.yaml"

testRestoreFromClusterSnapshot :: RestoreFromClusterSnapshot -> TestTree
testRestoreFromClusterSnapshot = req
    "RestoreFromClusterSnapshot"
    "fixture/RestoreFromClusterSnapshot.yaml"

testCreateClusterParameterGroup :: CreateClusterParameterGroup -> TestTree
testCreateClusterParameterGroup = req
    "CreateClusterParameterGroup"
    "fixture/CreateClusterParameterGroup.yaml"

testRevokeSnapshotAccess :: RevokeSnapshotAccess -> TestTree
testRevokeSnapshotAccess = req
    "RevokeSnapshotAccess"
    "fixture/RevokeSnapshotAccess.yaml"

testDescribeHSMConfigurations :: DescribeHSMConfigurations -> TestTree
testDescribeHSMConfigurations = req
    "DescribeHSMConfigurations"
    "fixture/DescribeHSMConfigurations.yaml"

testCreateSnapshotCopyGrant :: CreateSnapshotCopyGrant -> TestTree
testCreateSnapshotCopyGrant = req
    "CreateSnapshotCopyGrant"
    "fixture/CreateSnapshotCopyGrant.yaml"

testCopyClusterSnapshot :: CopyClusterSnapshot -> TestTree
testCopyClusterSnapshot = req
    "CopyClusterSnapshot"
    "fixture/CopyClusterSnapshot.yaml"

testDeleteHSMClientCertificate :: DeleteHSMClientCertificate -> TestTree
testDeleteHSMClientCertificate = req
    "DeleteHSMClientCertificate"
    "fixture/DeleteHSMClientCertificate.yaml"

testDeleteSnapshotCopyGrant :: DeleteSnapshotCopyGrant -> TestTree
testDeleteSnapshotCopyGrant = req
    "DeleteSnapshotCopyGrant"
    "fixture/DeleteSnapshotCopyGrant.yaml"

testDescribeClusterVersions :: DescribeClusterVersions -> TestTree
testDescribeClusterVersions = req
    "DescribeClusterVersions"
    "fixture/DescribeClusterVersions.yaml"

testModifyClusterSubnetGroup :: ModifyClusterSubnetGroup -> TestTree
testModifyClusterSubnetGroup = req
    "ModifyClusterSubnetGroup"
    "fixture/ModifyClusterSubnetGroup.yaml"

testRotateEncryptionKey :: RotateEncryptionKey -> TestTree
testRotateEncryptionKey = req
    "RotateEncryptionKey"
    "fixture/RotateEncryptionKey.yaml"

testDescribeSnapshotCopyGrants :: DescribeSnapshotCopyGrants -> TestTree
testDescribeSnapshotCopyGrants = req
    "DescribeSnapshotCopyGrants"
    "fixture/DescribeSnapshotCopyGrants.yaml"

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

testDeleteClusterSubnetGroupResponse :: DeleteClusterSubnetGroupResponse -> TestTree
testDeleteClusterSubnetGroupResponse = res
    "DeleteClusterSubnetGroupResponse"
    "fixture/DeleteClusterSubnetGroupResponse.proto"
    redshift
    (Proxy :: Proxy DeleteClusterSubnetGroup)

testDisableLoggingResponse :: LoggingStatus -> TestTree
testDisableLoggingResponse = res
    "DisableLoggingResponse"
    "fixture/DisableLoggingResponse.proto"
    redshift
    (Proxy :: Proxy DisableLogging)

testModifyEventSubscriptionResponse :: ModifyEventSubscriptionResponse -> TestTree
testModifyEventSubscriptionResponse = res
    "ModifyEventSubscriptionResponse"
    "fixture/ModifyEventSubscriptionResponse.proto"
    redshift
    (Proxy :: Proxy ModifyEventSubscription)

testDeleteClusterSnapshotResponse :: DeleteClusterSnapshotResponse -> TestTree
testDeleteClusterSnapshotResponse = res
    "DeleteClusterSnapshotResponse"
    "fixture/DeleteClusterSnapshotResponse.proto"
    redshift
    (Proxy :: Proxy DeleteClusterSnapshot)

testPurchaseReservedNodeOfferingResponse :: PurchaseReservedNodeOfferingResponse -> TestTree
testPurchaseReservedNodeOfferingResponse = res
    "PurchaseReservedNodeOfferingResponse"
    "fixture/PurchaseReservedNodeOfferingResponse.proto"
    redshift
    (Proxy :: Proxy PurchaseReservedNodeOffering)

testDescribeReservedNodeOfferingsResponse :: DescribeReservedNodeOfferingsResponse -> TestTree
testDescribeReservedNodeOfferingsResponse = res
    "DescribeReservedNodeOfferingsResponse"
    "fixture/DescribeReservedNodeOfferingsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeReservedNodeOfferings)

testDescribeEventsResponse :: DescribeEventsResponse -> TestTree
testDescribeEventsResponse = res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeEvents)

testDescribeReservedNodesResponse :: DescribeReservedNodesResponse -> TestTree
testDescribeReservedNodesResponse = res
    "DescribeReservedNodesResponse"
    "fixture/DescribeReservedNodesResponse.proto"
    redshift
    (Proxy :: Proxy DescribeReservedNodes)

testDescribeClusterParameterGroupsResponse :: DescribeClusterParameterGroupsResponse -> TestTree
testDescribeClusterParameterGroupsResponse = res
    "DescribeClusterParameterGroupsResponse"
    "fixture/DescribeClusterParameterGroupsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeClusterParameterGroups)

testEnableLoggingResponse :: LoggingStatus -> TestTree
testEnableLoggingResponse = res
    "EnableLoggingResponse"
    "fixture/EnableLoggingResponse.proto"
    redshift
    (Proxy :: Proxy EnableLogging)

testCreateClusterSubnetGroupResponse :: CreateClusterSubnetGroupResponse -> TestTree
testCreateClusterSubnetGroupResponse = res
    "CreateClusterSubnetGroupResponse"
    "fixture/CreateClusterSubnetGroupResponse.proto"
    redshift
    (Proxy :: Proxy CreateClusterSubnetGroup)

testDeleteClusterParameterGroupResponse :: DeleteClusterParameterGroupResponse -> TestTree
testDeleteClusterParameterGroupResponse = res
    "DeleteClusterParameterGroupResponse"
    "fixture/DeleteClusterParameterGroupResponse.proto"
    redshift
    (Proxy :: Proxy DeleteClusterParameterGroup)

testDescribeClusterSecurityGroupsResponse :: DescribeClusterSecurityGroupsResponse -> TestTree
testDescribeClusterSecurityGroupsResponse = res
    "DescribeClusterSecurityGroupsResponse"
    "fixture/DescribeClusterSecurityGroupsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeClusterSecurityGroups)

testCreateTagsResponse :: CreateTagsResponse -> TestTree
testCreateTagsResponse = res
    "CreateTagsResponse"
    "fixture/CreateTagsResponse.proto"
    redshift
    (Proxy :: Proxy CreateTags)

testEnableSnapshotCopyResponse :: EnableSnapshotCopyResponse -> TestTree
testEnableSnapshotCopyResponse = res
    "EnableSnapshotCopyResponse"
    "fixture/EnableSnapshotCopyResponse.proto"
    redshift
    (Proxy :: Proxy EnableSnapshotCopy)

testDescribeClusterSnapshotsResponse :: DescribeClusterSnapshotsResponse -> TestTree
testDescribeClusterSnapshotsResponse = res
    "DescribeClusterSnapshotsResponse"
    "fixture/DescribeClusterSnapshotsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeClusterSnapshots)

testDeleteTagsResponse :: DeleteTagsResponse -> TestTree
testDeleteTagsResponse = res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    redshift
    (Proxy :: Proxy DeleteTags)

testDescribeClusterSubnetGroupsResponse :: DescribeClusterSubnetGroupsResponse -> TestTree
testDescribeClusterSubnetGroupsResponse = res
    "DescribeClusterSubnetGroupsResponse"
    "fixture/DescribeClusterSubnetGroupsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeClusterSubnetGroups)

testModifySnapshotCopyRetentionPeriodResponse :: ModifySnapshotCopyRetentionPeriodResponse -> TestTree
testModifySnapshotCopyRetentionPeriodResponse = res
    "ModifySnapshotCopyRetentionPeriodResponse"
    "fixture/ModifySnapshotCopyRetentionPeriodResponse.proto"
    redshift
    (Proxy :: Proxy ModifySnapshotCopyRetentionPeriod)

testAuthorizeSnapshotAccessResponse :: AuthorizeSnapshotAccessResponse -> TestTree
testAuthorizeSnapshotAccessResponse = res
    "AuthorizeSnapshotAccessResponse"
    "fixture/AuthorizeSnapshotAccessResponse.proto"
    redshift
    (Proxy :: Proxy AuthorizeSnapshotAccess)

testRebootClusterResponse :: RebootClusterResponse -> TestTree
testRebootClusterResponse = res
    "RebootClusterResponse"
    "fixture/RebootClusterResponse.proto"
    redshift
    (Proxy :: Proxy RebootCluster)

testDeleteClusterResponse :: DeleteClusterResponse -> TestTree
testDeleteClusterResponse = res
    "DeleteClusterResponse"
    "fixture/DeleteClusterResponse.proto"
    redshift
    (Proxy :: Proxy DeleteCluster)

testCreateEventSubscriptionResponse :: CreateEventSubscriptionResponse -> TestTree
testCreateEventSubscriptionResponse = res
    "CreateEventSubscriptionResponse"
    "fixture/CreateEventSubscriptionResponse.proto"
    redshift
    (Proxy :: Proxy CreateEventSubscription)

testDescribeOrderableClusterOptionsResponse :: DescribeOrderableClusterOptionsResponse -> TestTree
testDescribeOrderableClusterOptionsResponse = res
    "DescribeOrderableClusterOptionsResponse"
    "fixture/DescribeOrderableClusterOptionsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeOrderableClusterOptions)

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

testDescribeDefaultClusterParametersResponse :: DescribeDefaultClusterParametersResponse -> TestTree
testDescribeDefaultClusterParametersResponse = res
    "DescribeDefaultClusterParametersResponse"
    "fixture/DescribeDefaultClusterParametersResponse.proto"
    redshift
    (Proxy :: Proxy DescribeDefaultClusterParameters)

testDeleteEventSubscriptionResponse :: DeleteEventSubscriptionResponse -> TestTree
testDeleteEventSubscriptionResponse = res
    "DeleteEventSubscriptionResponse"
    "fixture/DeleteEventSubscriptionResponse.proto"
    redshift
    (Proxy :: Proxy DeleteEventSubscription)

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

testRevokeClusterSecurityGroupIngressResponse :: RevokeClusterSecurityGroupIngressResponse -> TestTree
testRevokeClusterSecurityGroupIngressResponse = res
    "RevokeClusterSecurityGroupIngressResponse"
    "fixture/RevokeClusterSecurityGroupIngressResponse.proto"
    redshift
    (Proxy :: Proxy RevokeClusterSecurityGroupIngress)

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

testCreateClusterSecurityGroupResponse :: CreateClusterSecurityGroupResponse -> TestTree
testCreateClusterSecurityGroupResponse = res
    "CreateClusterSecurityGroupResponse"
    "fixture/CreateClusterSecurityGroupResponse.proto"
    redshift
    (Proxy :: Proxy CreateClusterSecurityGroup)

testDescribeEventCategoriesResponse :: DescribeEventCategoriesResponse -> TestTree
testDescribeEventCategoriesResponse = res
    "DescribeEventCategoriesResponse"
    "fixture/DescribeEventCategoriesResponse.proto"
    redshift
    (Proxy :: Proxy DescribeEventCategories)

testDescribeResizeResponse :: DescribeResizeResponse -> TestTree
testDescribeResizeResponse = res
    "DescribeResizeResponse"
    "fixture/DescribeResizeResponse.proto"
    redshift
    (Proxy :: Proxy DescribeResize)

testDeleteHSMConfigurationResponse :: DeleteHSMConfigurationResponse -> TestTree
testDeleteHSMConfigurationResponse = res
    "DeleteHSMConfigurationResponse"
    "fixture/DeleteHSMConfigurationResponse.proto"
    redshift
    (Proxy :: Proxy DeleteHSMConfiguration)

testAuthorizeClusterSecurityGroupIngressResponse :: AuthorizeClusterSecurityGroupIngressResponse -> TestTree
testAuthorizeClusterSecurityGroupIngressResponse = res
    "AuthorizeClusterSecurityGroupIngressResponse"
    "fixture/AuthorizeClusterSecurityGroupIngressResponse.proto"
    redshift
    (Proxy :: Proxy AuthorizeClusterSecurityGroupIngress)

testCreateClusterSnapshotResponse :: CreateClusterSnapshotResponse -> TestTree
testCreateClusterSnapshotResponse = res
    "CreateClusterSnapshotResponse"
    "fixture/CreateClusterSnapshotResponse.proto"
    redshift
    (Proxy :: Proxy CreateClusterSnapshot)

testCreateHSMConfigurationResponse :: CreateHSMConfigurationResponse -> TestTree
testCreateHSMConfigurationResponse = res
    "CreateHSMConfigurationResponse"
    "fixture/CreateHSMConfigurationResponse.proto"
    redshift
    (Proxy :: Proxy CreateHSMConfiguration)

testDescribeLoggingStatusResponse :: LoggingStatus -> TestTree
testDescribeLoggingStatusResponse = res
    "DescribeLoggingStatusResponse"
    "fixture/DescribeLoggingStatusResponse.proto"
    redshift
    (Proxy :: Proxy DescribeLoggingStatus)

testModifyClusterResponse :: ModifyClusterResponse -> TestTree
testModifyClusterResponse = res
    "ModifyClusterResponse"
    "fixture/ModifyClusterResponse.proto"
    redshift
    (Proxy :: Proxy ModifyCluster)

testDeleteClusterSecurityGroupResponse :: DeleteClusterSecurityGroupResponse -> TestTree
testDeleteClusterSecurityGroupResponse = res
    "DeleteClusterSecurityGroupResponse"
    "fixture/DeleteClusterSecurityGroupResponse.proto"
    redshift
    (Proxy :: Proxy DeleteClusterSecurityGroup)

testDisableSnapshotCopyResponse :: DisableSnapshotCopyResponse -> TestTree
testDisableSnapshotCopyResponse = res
    "DisableSnapshotCopyResponse"
    "fixture/DisableSnapshotCopyResponse.proto"
    redshift
    (Proxy :: Proxy DisableSnapshotCopy)

testDescribeClusterParametersResponse :: DescribeClusterParametersResponse -> TestTree
testDescribeClusterParametersResponse = res
    "DescribeClusterParametersResponse"
    "fixture/DescribeClusterParametersResponse.proto"
    redshift
    (Proxy :: Proxy DescribeClusterParameters)

testRestoreFromClusterSnapshotResponse :: RestoreFromClusterSnapshotResponse -> TestTree
testRestoreFromClusterSnapshotResponse = res
    "RestoreFromClusterSnapshotResponse"
    "fixture/RestoreFromClusterSnapshotResponse.proto"
    redshift
    (Proxy :: Proxy RestoreFromClusterSnapshot)

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

testDescribeHSMConfigurationsResponse :: DescribeHSMConfigurationsResponse -> TestTree
testDescribeHSMConfigurationsResponse = res
    "DescribeHSMConfigurationsResponse"
    "fixture/DescribeHSMConfigurationsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeHSMConfigurations)

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

testDeleteHSMClientCertificateResponse :: DeleteHSMClientCertificateResponse -> TestTree
testDeleteHSMClientCertificateResponse = res
    "DeleteHSMClientCertificateResponse"
    "fixture/DeleteHSMClientCertificateResponse.proto"
    redshift
    (Proxy :: Proxy DeleteHSMClientCertificate)

testDeleteSnapshotCopyGrantResponse :: DeleteSnapshotCopyGrantResponse -> TestTree
testDeleteSnapshotCopyGrantResponse = res
    "DeleteSnapshotCopyGrantResponse"
    "fixture/DeleteSnapshotCopyGrantResponse.proto"
    redshift
    (Proxy :: Proxy DeleteSnapshotCopyGrant)

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

testRotateEncryptionKeyResponse :: RotateEncryptionKeyResponse -> TestTree
testRotateEncryptionKeyResponse = res
    "RotateEncryptionKeyResponse"
    "fixture/RotateEncryptionKeyResponse.proto"
    redshift
    (Proxy :: Proxy RotateEncryptionKey)

testDescribeSnapshotCopyGrantsResponse :: DescribeSnapshotCopyGrantsResponse -> TestTree
testDescribeSnapshotCopyGrantsResponse = res
    "DescribeSnapshotCopyGrantsResponse"
    "fixture/DescribeSnapshotCopyGrantsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeSnapshotCopyGrants)
