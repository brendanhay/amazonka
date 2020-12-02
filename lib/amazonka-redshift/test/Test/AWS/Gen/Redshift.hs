{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Redshift
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.Redshift where

import Data.Proxy
import Network.AWS.Redshift
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.Redshift.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDescribeClusters $
--             describeClusters
--
--         , requestDescribeTags $
--             describeTags
--
--         , requestDeleteClusterSubnetGroup $
--             deleteClusterSubnetGroup
--
--         , requestDisableLogging $
--             disableLogging
--
--         , requestModifyEventSubscription $
--             modifyEventSubscription
--
--         , requestDeleteClusterSnapshot $
--             deleteClusterSnapshot
--
--         , requestPurchaseReservedNodeOffering $
--             purchaseReservedNodeOffering
--
--         , requestDescribeReservedNodeOfferings $
--             describeReservedNodeOfferings
--
--         , requestDescribeEvents $
--             describeEvents
--
--         , requestDescribeReservedNodes $
--             describeReservedNodes
--
--         , requestDescribeClusterParameterGroups $
--             describeClusterParameterGroups
--
--         , requestEnableLogging $
--             enableLogging
--
--         , requestCreateClusterSubnetGroup $
--             createClusterSubnetGroup
--
--         , requestDeleteClusterParameterGroup $
--             deleteClusterParameterGroup
--
--         , requestDescribeClusterSecurityGroups $
--             describeClusterSecurityGroups
--
--         , requestCreateTags $
--             createTags
--
--         , requestEnableSnapshotCopy $
--             enableSnapshotCopy
--
--         , requestDescribeClusterSnapshots $
--             describeClusterSnapshots
--
--         , requestDeleteTags $
--             deleteTags
--
--         , requestDescribeClusterSubnetGroups $
--             describeClusterSubnetGroups
--
--         , requestModifySnapshotCopyRetentionPeriod $
--             modifySnapshotCopyRetentionPeriod
--
--         , requestModifyClusterIAMRoles $
--             modifyClusterIAMRoles
--
--         , requestAuthorizeSnapshotAccess $
--             authorizeSnapshotAccess
--
--         , requestRebootCluster $
--             rebootCluster
--
--         , requestDeleteCluster $
--             deleteCluster
--
--         , requestCreateEventSubscription $
--             createEventSubscription
--
--         , requestDescribeOrderableClusterOptions $
--             describeOrderableClusterOptions
--
--         , requestCreateCluster $
--             createCluster
--
--         , requestCreateHSMClientCertificate $
--             createHSMClientCertificate
--
--         , requestRestoreTableFromClusterSnapshot $
--             restoreTableFromClusterSnapshot
--
--         , requestDescribeDefaultClusterParameters $
--             describeDefaultClusterParameters
--
--         , requestDeleteEventSubscription $
--             deleteEventSubscription
--
--         , requestResetClusterParameterGroup $
--             resetClusterParameterGroup
--
--         , requestDescribeEventSubscriptions $
--             describeEventSubscriptions
--
--         , requestRevokeClusterSecurityGroupIngress $
--             revokeClusterSecurityGroupIngress
--
--         , requestDescribeHSMClientCertificates $
--             describeHSMClientCertificates
--
--         , requestModifyClusterParameterGroup $
--             modifyClusterParameterGroup
--
--         , requestGetClusterCredentials $
--             getClusterCredentials
--
--         , requestCreateClusterSecurityGroup $
--             createClusterSecurityGroup
--
--         , requestDescribeEventCategories $
--             describeEventCategories
--
--         , requestDescribeResize $
--             describeResize
--
--         , requestDeleteHSMConfiguration $
--             deleteHSMConfiguration
--
--         , requestAuthorizeClusterSecurityGroupIngress $
--             authorizeClusterSecurityGroupIngress
--
--         , requestDescribeTableRestoreStatus $
--             describeTableRestoreStatus
--
--         , requestCreateClusterSnapshot $
--             createClusterSnapshot
--
--         , requestCreateHSMConfiguration $
--             createHSMConfiguration
--
--         , requestDescribeLoggingStatus $
--             describeLoggingStatus
--
--         , requestModifyCluster $
--             modifyCluster
--
--         , requestDeleteClusterSecurityGroup $
--             deleteClusterSecurityGroup
--
--         , requestDisableSnapshotCopy $
--             disableSnapshotCopy
--
--         , requestDescribeClusterParameters $
--             describeClusterParameters
--
--         , requestRestoreFromClusterSnapshot $
--             restoreFromClusterSnapshot
--
--         , requestCreateClusterParameterGroup $
--             createClusterParameterGroup
--
--         , requestRevokeSnapshotAccess $
--             revokeSnapshotAccess
--
--         , requestDescribeHSMConfigurations $
--             describeHSMConfigurations
--
--         , requestCreateSnapshotCopyGrant $
--             createSnapshotCopyGrant
--
--         , requestCopyClusterSnapshot $
--             copyClusterSnapshot
--
--         , requestDeleteHSMClientCertificate $
--             deleteHSMClientCertificate
--
--         , requestDeleteSnapshotCopyGrant $
--             deleteSnapshotCopyGrant
--
--         , requestDescribeClusterVersions $
--             describeClusterVersions
--
--         , requestModifyClusterSubnetGroup $
--             modifyClusterSubnetGroup
--
--         , requestRotateEncryptionKey $
--             rotateEncryptionKey
--
--         , requestDescribeSnapshotCopyGrants $
--             describeSnapshotCopyGrants
--
--           ]

--     , testGroup "response"
--         [ responseDescribeClusters $
--             describeClustersResponse
--
--         , responseDescribeTags $
--             describeTagsResponse
--
--         , responseDeleteClusterSubnetGroup $
--             deleteClusterSubnetGroupResponse
--
--         , responseDisableLogging $
--             loggingStatus
--
--         , responseModifyEventSubscription $
--             modifyEventSubscriptionResponse
--
--         , responseDeleteClusterSnapshot $
--             deleteClusterSnapshotResponse
--
--         , responsePurchaseReservedNodeOffering $
--             purchaseReservedNodeOfferingResponse
--
--         , responseDescribeReservedNodeOfferings $
--             describeReservedNodeOfferingsResponse
--
--         , responseDescribeEvents $
--             describeEventsResponse
--
--         , responseDescribeReservedNodes $
--             describeReservedNodesResponse
--
--         , responseDescribeClusterParameterGroups $
--             describeClusterParameterGroupsResponse
--
--         , responseEnableLogging $
--             loggingStatus
--
--         , responseCreateClusterSubnetGroup $
--             createClusterSubnetGroupResponse
--
--         , responseDeleteClusterParameterGroup $
--             deleteClusterParameterGroupResponse
--
--         , responseDescribeClusterSecurityGroups $
--             describeClusterSecurityGroupsResponse
--
--         , responseCreateTags $
--             createTagsResponse
--
--         , responseEnableSnapshotCopy $
--             enableSnapshotCopyResponse
--
--         , responseDescribeClusterSnapshots $
--             describeClusterSnapshotsResponse
--
--         , responseDeleteTags $
--             deleteTagsResponse
--
--         , responseDescribeClusterSubnetGroups $
--             describeClusterSubnetGroupsResponse
--
--         , responseModifySnapshotCopyRetentionPeriod $
--             modifySnapshotCopyRetentionPeriodResponse
--
--         , responseModifyClusterIAMRoles $
--             modifyClusterIAMRolesResponse
--
--         , responseAuthorizeSnapshotAccess $
--             authorizeSnapshotAccessResponse
--
--         , responseRebootCluster $
--             rebootClusterResponse
--
--         , responseDeleteCluster $
--             deleteClusterResponse
--
--         , responseCreateEventSubscription $
--             createEventSubscriptionResponse
--
--         , responseDescribeOrderableClusterOptions $
--             describeOrderableClusterOptionsResponse
--
--         , responseCreateCluster $
--             createClusterResponse
--
--         , responseCreateHSMClientCertificate $
--             createHSMClientCertificateResponse
--
--         , responseRestoreTableFromClusterSnapshot $
--             restoreTableFromClusterSnapshotResponse
--
--         , responseDescribeDefaultClusterParameters $
--             describeDefaultClusterParametersResponse
--
--         , responseDeleteEventSubscription $
--             deleteEventSubscriptionResponse
--
--         , responseResetClusterParameterGroup $
--             clusterParameterGroupNameMessage
--
--         , responseDescribeEventSubscriptions $
--             describeEventSubscriptionsResponse
--
--         , responseRevokeClusterSecurityGroupIngress $
--             revokeClusterSecurityGroupIngressResponse
--
--         , responseDescribeHSMClientCertificates $
--             describeHSMClientCertificatesResponse
--
--         , responseModifyClusterParameterGroup $
--             clusterParameterGroupNameMessage
--
--         , responseGetClusterCredentials $
--             getClusterCredentialsResponse
--
--         , responseCreateClusterSecurityGroup $
--             createClusterSecurityGroupResponse
--
--         , responseDescribeEventCategories $
--             describeEventCategoriesResponse
--
--         , responseDescribeResize $
--             describeResizeResponse
--
--         , responseDeleteHSMConfiguration $
--             deleteHSMConfigurationResponse
--
--         , responseAuthorizeClusterSecurityGroupIngress $
--             authorizeClusterSecurityGroupIngressResponse
--
--         , responseDescribeTableRestoreStatus $
--             describeTableRestoreStatusResponse
--
--         , responseCreateClusterSnapshot $
--             createClusterSnapshotResponse
--
--         , responseCreateHSMConfiguration $
--             createHSMConfigurationResponse
--
--         , responseDescribeLoggingStatus $
--             loggingStatus
--
--         , responseModifyCluster $
--             modifyClusterResponse
--
--         , responseDeleteClusterSecurityGroup $
--             deleteClusterSecurityGroupResponse
--
--         , responseDisableSnapshotCopy $
--             disableSnapshotCopyResponse
--
--         , responseDescribeClusterParameters $
--             describeClusterParametersResponse
--
--         , responseRestoreFromClusterSnapshot $
--             restoreFromClusterSnapshotResponse
--
--         , responseCreateClusterParameterGroup $
--             createClusterParameterGroupResponse
--
--         , responseRevokeSnapshotAccess $
--             revokeSnapshotAccessResponse
--
--         , responseDescribeHSMConfigurations $
--             describeHSMConfigurationsResponse
--
--         , responseCreateSnapshotCopyGrant $
--             createSnapshotCopyGrantResponse
--
--         , responseCopyClusterSnapshot $
--             copyClusterSnapshotResponse
--
--         , responseDeleteHSMClientCertificate $
--             deleteHSMClientCertificateResponse
--
--         , responseDeleteSnapshotCopyGrant $
--             deleteSnapshotCopyGrantResponse
--
--         , responseDescribeClusterVersions $
--             describeClusterVersionsResponse
--
--         , responseModifyClusterSubnetGroup $
--             modifyClusterSubnetGroupResponse
--
--         , responseRotateEncryptionKey $
--             rotateEncryptionKeyResponse
--
--         , responseDescribeSnapshotCopyGrants $
--             describeSnapshotCopyGrantsResponse
--
--           ]
--     ]

-- Requests

requestDescribeClusters :: DescribeClusters -> TestTree
requestDescribeClusters = req
    "DescribeClusters"
    "fixture/DescribeClusters.yaml"

requestDescribeTags :: DescribeTags -> TestTree
requestDescribeTags = req
    "DescribeTags"
    "fixture/DescribeTags.yaml"

requestDeleteClusterSubnetGroup :: DeleteClusterSubnetGroup -> TestTree
requestDeleteClusterSubnetGroup = req
    "DeleteClusterSubnetGroup"
    "fixture/DeleteClusterSubnetGroup.yaml"

requestDisableLogging :: DisableLogging -> TestTree
requestDisableLogging = req
    "DisableLogging"
    "fixture/DisableLogging.yaml"

requestModifyEventSubscription :: ModifyEventSubscription -> TestTree
requestModifyEventSubscription = req
    "ModifyEventSubscription"
    "fixture/ModifyEventSubscription.yaml"

requestDeleteClusterSnapshot :: DeleteClusterSnapshot -> TestTree
requestDeleteClusterSnapshot = req
    "DeleteClusterSnapshot"
    "fixture/DeleteClusterSnapshot.yaml"

requestPurchaseReservedNodeOffering :: PurchaseReservedNodeOffering -> TestTree
requestPurchaseReservedNodeOffering = req
    "PurchaseReservedNodeOffering"
    "fixture/PurchaseReservedNodeOffering.yaml"

requestDescribeReservedNodeOfferings :: DescribeReservedNodeOfferings -> TestTree
requestDescribeReservedNodeOfferings = req
    "DescribeReservedNodeOfferings"
    "fixture/DescribeReservedNodeOfferings.yaml"

requestDescribeEvents :: DescribeEvents -> TestTree
requestDescribeEvents = req
    "DescribeEvents"
    "fixture/DescribeEvents.yaml"

requestDescribeReservedNodes :: DescribeReservedNodes -> TestTree
requestDescribeReservedNodes = req
    "DescribeReservedNodes"
    "fixture/DescribeReservedNodes.yaml"

requestDescribeClusterParameterGroups :: DescribeClusterParameterGroups -> TestTree
requestDescribeClusterParameterGroups = req
    "DescribeClusterParameterGroups"
    "fixture/DescribeClusterParameterGroups.yaml"

requestEnableLogging :: EnableLogging -> TestTree
requestEnableLogging = req
    "EnableLogging"
    "fixture/EnableLogging.yaml"

requestCreateClusterSubnetGroup :: CreateClusterSubnetGroup -> TestTree
requestCreateClusterSubnetGroup = req
    "CreateClusterSubnetGroup"
    "fixture/CreateClusterSubnetGroup.yaml"

requestDeleteClusterParameterGroup :: DeleteClusterParameterGroup -> TestTree
requestDeleteClusterParameterGroup = req
    "DeleteClusterParameterGroup"
    "fixture/DeleteClusterParameterGroup.yaml"

requestDescribeClusterSecurityGroups :: DescribeClusterSecurityGroups -> TestTree
requestDescribeClusterSecurityGroups = req
    "DescribeClusterSecurityGroups"
    "fixture/DescribeClusterSecurityGroups.yaml"

requestCreateTags :: CreateTags -> TestTree
requestCreateTags = req
    "CreateTags"
    "fixture/CreateTags.yaml"

requestEnableSnapshotCopy :: EnableSnapshotCopy -> TestTree
requestEnableSnapshotCopy = req
    "EnableSnapshotCopy"
    "fixture/EnableSnapshotCopy.yaml"

requestDescribeClusterSnapshots :: DescribeClusterSnapshots -> TestTree
requestDescribeClusterSnapshots = req
    "DescribeClusterSnapshots"
    "fixture/DescribeClusterSnapshots.yaml"

requestDeleteTags :: DeleteTags -> TestTree
requestDeleteTags = req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

requestDescribeClusterSubnetGroups :: DescribeClusterSubnetGroups -> TestTree
requestDescribeClusterSubnetGroups = req
    "DescribeClusterSubnetGroups"
    "fixture/DescribeClusterSubnetGroups.yaml"

requestModifySnapshotCopyRetentionPeriod :: ModifySnapshotCopyRetentionPeriod -> TestTree
requestModifySnapshotCopyRetentionPeriod = req
    "ModifySnapshotCopyRetentionPeriod"
    "fixture/ModifySnapshotCopyRetentionPeriod.yaml"

requestModifyClusterIAMRoles :: ModifyClusterIAMRoles -> TestTree
requestModifyClusterIAMRoles = req
    "ModifyClusterIAMRoles"
    "fixture/ModifyClusterIAMRoles.yaml"

requestAuthorizeSnapshotAccess :: AuthorizeSnapshotAccess -> TestTree
requestAuthorizeSnapshotAccess = req
    "AuthorizeSnapshotAccess"
    "fixture/AuthorizeSnapshotAccess.yaml"

requestRebootCluster :: RebootCluster -> TestTree
requestRebootCluster = req
    "RebootCluster"
    "fixture/RebootCluster.yaml"

requestDeleteCluster :: DeleteCluster -> TestTree
requestDeleteCluster = req
    "DeleteCluster"
    "fixture/DeleteCluster.yaml"

requestCreateEventSubscription :: CreateEventSubscription -> TestTree
requestCreateEventSubscription = req
    "CreateEventSubscription"
    "fixture/CreateEventSubscription.yaml"

requestDescribeOrderableClusterOptions :: DescribeOrderableClusterOptions -> TestTree
requestDescribeOrderableClusterOptions = req
    "DescribeOrderableClusterOptions"
    "fixture/DescribeOrderableClusterOptions.yaml"

requestCreateCluster :: CreateCluster -> TestTree
requestCreateCluster = req
    "CreateCluster"
    "fixture/CreateCluster.yaml"

requestCreateHSMClientCertificate :: CreateHSMClientCertificate -> TestTree
requestCreateHSMClientCertificate = req
    "CreateHSMClientCertificate"
    "fixture/CreateHSMClientCertificate.yaml"

requestRestoreTableFromClusterSnapshot :: RestoreTableFromClusterSnapshot -> TestTree
requestRestoreTableFromClusterSnapshot = req
    "RestoreTableFromClusterSnapshot"
    "fixture/RestoreTableFromClusterSnapshot.yaml"

requestDescribeDefaultClusterParameters :: DescribeDefaultClusterParameters -> TestTree
requestDescribeDefaultClusterParameters = req
    "DescribeDefaultClusterParameters"
    "fixture/DescribeDefaultClusterParameters.yaml"

requestDeleteEventSubscription :: DeleteEventSubscription -> TestTree
requestDeleteEventSubscription = req
    "DeleteEventSubscription"
    "fixture/DeleteEventSubscription.yaml"

requestResetClusterParameterGroup :: ResetClusterParameterGroup -> TestTree
requestResetClusterParameterGroup = req
    "ResetClusterParameterGroup"
    "fixture/ResetClusterParameterGroup.yaml"

requestDescribeEventSubscriptions :: DescribeEventSubscriptions -> TestTree
requestDescribeEventSubscriptions = req
    "DescribeEventSubscriptions"
    "fixture/DescribeEventSubscriptions.yaml"

requestRevokeClusterSecurityGroupIngress :: RevokeClusterSecurityGroupIngress -> TestTree
requestRevokeClusterSecurityGroupIngress = req
    "RevokeClusterSecurityGroupIngress"
    "fixture/RevokeClusterSecurityGroupIngress.yaml"

requestDescribeHSMClientCertificates :: DescribeHSMClientCertificates -> TestTree
requestDescribeHSMClientCertificates = req
    "DescribeHSMClientCertificates"
    "fixture/DescribeHSMClientCertificates.yaml"

requestModifyClusterParameterGroup :: ModifyClusterParameterGroup -> TestTree
requestModifyClusterParameterGroup = req
    "ModifyClusterParameterGroup"
    "fixture/ModifyClusterParameterGroup.yaml"

requestGetClusterCredentials :: GetClusterCredentials -> TestTree
requestGetClusterCredentials = req
    "GetClusterCredentials"
    "fixture/GetClusterCredentials.yaml"

requestCreateClusterSecurityGroup :: CreateClusterSecurityGroup -> TestTree
requestCreateClusterSecurityGroup = req
    "CreateClusterSecurityGroup"
    "fixture/CreateClusterSecurityGroup.yaml"

requestDescribeEventCategories :: DescribeEventCategories -> TestTree
requestDescribeEventCategories = req
    "DescribeEventCategories"
    "fixture/DescribeEventCategories.yaml"

requestDescribeResize :: DescribeResize -> TestTree
requestDescribeResize = req
    "DescribeResize"
    "fixture/DescribeResize.yaml"

requestDeleteHSMConfiguration :: DeleteHSMConfiguration -> TestTree
requestDeleteHSMConfiguration = req
    "DeleteHSMConfiguration"
    "fixture/DeleteHSMConfiguration.yaml"

requestAuthorizeClusterSecurityGroupIngress :: AuthorizeClusterSecurityGroupIngress -> TestTree
requestAuthorizeClusterSecurityGroupIngress = req
    "AuthorizeClusterSecurityGroupIngress"
    "fixture/AuthorizeClusterSecurityGroupIngress.yaml"

requestDescribeTableRestoreStatus :: DescribeTableRestoreStatus -> TestTree
requestDescribeTableRestoreStatus = req
    "DescribeTableRestoreStatus"
    "fixture/DescribeTableRestoreStatus.yaml"

requestCreateClusterSnapshot :: CreateClusterSnapshot -> TestTree
requestCreateClusterSnapshot = req
    "CreateClusterSnapshot"
    "fixture/CreateClusterSnapshot.yaml"

requestCreateHSMConfiguration :: CreateHSMConfiguration -> TestTree
requestCreateHSMConfiguration = req
    "CreateHSMConfiguration"
    "fixture/CreateHSMConfiguration.yaml"

requestDescribeLoggingStatus :: DescribeLoggingStatus -> TestTree
requestDescribeLoggingStatus = req
    "DescribeLoggingStatus"
    "fixture/DescribeLoggingStatus.yaml"

requestModifyCluster :: ModifyCluster -> TestTree
requestModifyCluster = req
    "ModifyCluster"
    "fixture/ModifyCluster.yaml"

requestDeleteClusterSecurityGroup :: DeleteClusterSecurityGroup -> TestTree
requestDeleteClusterSecurityGroup = req
    "DeleteClusterSecurityGroup"
    "fixture/DeleteClusterSecurityGroup.yaml"

requestDisableSnapshotCopy :: DisableSnapshotCopy -> TestTree
requestDisableSnapshotCopy = req
    "DisableSnapshotCopy"
    "fixture/DisableSnapshotCopy.yaml"

requestDescribeClusterParameters :: DescribeClusterParameters -> TestTree
requestDescribeClusterParameters = req
    "DescribeClusterParameters"
    "fixture/DescribeClusterParameters.yaml"

requestRestoreFromClusterSnapshot :: RestoreFromClusterSnapshot -> TestTree
requestRestoreFromClusterSnapshot = req
    "RestoreFromClusterSnapshot"
    "fixture/RestoreFromClusterSnapshot.yaml"

requestCreateClusterParameterGroup :: CreateClusterParameterGroup -> TestTree
requestCreateClusterParameterGroup = req
    "CreateClusterParameterGroup"
    "fixture/CreateClusterParameterGroup.yaml"

requestRevokeSnapshotAccess :: RevokeSnapshotAccess -> TestTree
requestRevokeSnapshotAccess = req
    "RevokeSnapshotAccess"
    "fixture/RevokeSnapshotAccess.yaml"

requestDescribeHSMConfigurations :: DescribeHSMConfigurations -> TestTree
requestDescribeHSMConfigurations = req
    "DescribeHSMConfigurations"
    "fixture/DescribeHSMConfigurations.yaml"

requestCreateSnapshotCopyGrant :: CreateSnapshotCopyGrant -> TestTree
requestCreateSnapshotCopyGrant = req
    "CreateSnapshotCopyGrant"
    "fixture/CreateSnapshotCopyGrant.yaml"

requestCopyClusterSnapshot :: CopyClusterSnapshot -> TestTree
requestCopyClusterSnapshot = req
    "CopyClusterSnapshot"
    "fixture/CopyClusterSnapshot.yaml"

requestDeleteHSMClientCertificate :: DeleteHSMClientCertificate -> TestTree
requestDeleteHSMClientCertificate = req
    "DeleteHSMClientCertificate"
    "fixture/DeleteHSMClientCertificate.yaml"

requestDeleteSnapshotCopyGrant :: DeleteSnapshotCopyGrant -> TestTree
requestDeleteSnapshotCopyGrant = req
    "DeleteSnapshotCopyGrant"
    "fixture/DeleteSnapshotCopyGrant.yaml"

requestDescribeClusterVersions :: DescribeClusterVersions -> TestTree
requestDescribeClusterVersions = req
    "DescribeClusterVersions"
    "fixture/DescribeClusterVersions.yaml"

requestModifyClusterSubnetGroup :: ModifyClusterSubnetGroup -> TestTree
requestModifyClusterSubnetGroup = req
    "ModifyClusterSubnetGroup"
    "fixture/ModifyClusterSubnetGroup.yaml"

requestRotateEncryptionKey :: RotateEncryptionKey -> TestTree
requestRotateEncryptionKey = req
    "RotateEncryptionKey"
    "fixture/RotateEncryptionKey.yaml"

requestDescribeSnapshotCopyGrants :: DescribeSnapshotCopyGrants -> TestTree
requestDescribeSnapshotCopyGrants = req
    "DescribeSnapshotCopyGrants"
    "fixture/DescribeSnapshotCopyGrants.yaml"

-- Responses

responseDescribeClusters :: DescribeClustersResponse -> TestTree
responseDescribeClusters = res
    "DescribeClustersResponse"
    "fixture/DescribeClustersResponse.proto"
    redshift
    (Proxy :: Proxy DescribeClusters)

responseDescribeTags :: DescribeTagsResponse -> TestTree
responseDescribeTags = res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeTags)

responseDeleteClusterSubnetGroup :: DeleteClusterSubnetGroupResponse -> TestTree
responseDeleteClusterSubnetGroup = res
    "DeleteClusterSubnetGroupResponse"
    "fixture/DeleteClusterSubnetGroupResponse.proto"
    redshift
    (Proxy :: Proxy DeleteClusterSubnetGroup)

responseDisableLogging :: LoggingStatus -> TestTree
responseDisableLogging = res
    "DisableLoggingResponse"
    "fixture/DisableLoggingResponse.proto"
    redshift
    (Proxy :: Proxy DisableLogging)

responseModifyEventSubscription :: ModifyEventSubscriptionResponse -> TestTree
responseModifyEventSubscription = res
    "ModifyEventSubscriptionResponse"
    "fixture/ModifyEventSubscriptionResponse.proto"
    redshift
    (Proxy :: Proxy ModifyEventSubscription)

responseDeleteClusterSnapshot :: DeleteClusterSnapshotResponse -> TestTree
responseDeleteClusterSnapshot = res
    "DeleteClusterSnapshotResponse"
    "fixture/DeleteClusterSnapshotResponse.proto"
    redshift
    (Proxy :: Proxy DeleteClusterSnapshot)

responsePurchaseReservedNodeOffering :: PurchaseReservedNodeOfferingResponse -> TestTree
responsePurchaseReservedNodeOffering = res
    "PurchaseReservedNodeOfferingResponse"
    "fixture/PurchaseReservedNodeOfferingResponse.proto"
    redshift
    (Proxy :: Proxy PurchaseReservedNodeOffering)

responseDescribeReservedNodeOfferings :: DescribeReservedNodeOfferingsResponse -> TestTree
responseDescribeReservedNodeOfferings = res
    "DescribeReservedNodeOfferingsResponse"
    "fixture/DescribeReservedNodeOfferingsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeReservedNodeOfferings)

responseDescribeEvents :: DescribeEventsResponse -> TestTree
responseDescribeEvents = res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeEvents)

responseDescribeReservedNodes :: DescribeReservedNodesResponse -> TestTree
responseDescribeReservedNodes = res
    "DescribeReservedNodesResponse"
    "fixture/DescribeReservedNodesResponse.proto"
    redshift
    (Proxy :: Proxy DescribeReservedNodes)

responseDescribeClusterParameterGroups :: DescribeClusterParameterGroupsResponse -> TestTree
responseDescribeClusterParameterGroups = res
    "DescribeClusterParameterGroupsResponse"
    "fixture/DescribeClusterParameterGroupsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeClusterParameterGroups)

responseEnableLogging :: LoggingStatus -> TestTree
responseEnableLogging = res
    "EnableLoggingResponse"
    "fixture/EnableLoggingResponse.proto"
    redshift
    (Proxy :: Proxy EnableLogging)

responseCreateClusterSubnetGroup :: CreateClusterSubnetGroupResponse -> TestTree
responseCreateClusterSubnetGroup = res
    "CreateClusterSubnetGroupResponse"
    "fixture/CreateClusterSubnetGroupResponse.proto"
    redshift
    (Proxy :: Proxy CreateClusterSubnetGroup)

responseDeleteClusterParameterGroup :: DeleteClusterParameterGroupResponse -> TestTree
responseDeleteClusterParameterGroup = res
    "DeleteClusterParameterGroupResponse"
    "fixture/DeleteClusterParameterGroupResponse.proto"
    redshift
    (Proxy :: Proxy DeleteClusterParameterGroup)

responseDescribeClusterSecurityGroups :: DescribeClusterSecurityGroupsResponse -> TestTree
responseDescribeClusterSecurityGroups = res
    "DescribeClusterSecurityGroupsResponse"
    "fixture/DescribeClusterSecurityGroupsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeClusterSecurityGroups)

responseCreateTags :: CreateTagsResponse -> TestTree
responseCreateTags = res
    "CreateTagsResponse"
    "fixture/CreateTagsResponse.proto"
    redshift
    (Proxy :: Proxy CreateTags)

responseEnableSnapshotCopy :: EnableSnapshotCopyResponse -> TestTree
responseEnableSnapshotCopy = res
    "EnableSnapshotCopyResponse"
    "fixture/EnableSnapshotCopyResponse.proto"
    redshift
    (Proxy :: Proxy EnableSnapshotCopy)

responseDescribeClusterSnapshots :: DescribeClusterSnapshotsResponse -> TestTree
responseDescribeClusterSnapshots = res
    "DescribeClusterSnapshotsResponse"
    "fixture/DescribeClusterSnapshotsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeClusterSnapshots)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags = res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    redshift
    (Proxy :: Proxy DeleteTags)

responseDescribeClusterSubnetGroups :: DescribeClusterSubnetGroupsResponse -> TestTree
responseDescribeClusterSubnetGroups = res
    "DescribeClusterSubnetGroupsResponse"
    "fixture/DescribeClusterSubnetGroupsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeClusterSubnetGroups)

responseModifySnapshotCopyRetentionPeriod :: ModifySnapshotCopyRetentionPeriodResponse -> TestTree
responseModifySnapshotCopyRetentionPeriod = res
    "ModifySnapshotCopyRetentionPeriodResponse"
    "fixture/ModifySnapshotCopyRetentionPeriodResponse.proto"
    redshift
    (Proxy :: Proxy ModifySnapshotCopyRetentionPeriod)

responseModifyClusterIAMRoles :: ModifyClusterIAMRolesResponse -> TestTree
responseModifyClusterIAMRoles = res
    "ModifyClusterIAMRolesResponse"
    "fixture/ModifyClusterIAMRolesResponse.proto"
    redshift
    (Proxy :: Proxy ModifyClusterIAMRoles)

responseAuthorizeSnapshotAccess :: AuthorizeSnapshotAccessResponse -> TestTree
responseAuthorizeSnapshotAccess = res
    "AuthorizeSnapshotAccessResponse"
    "fixture/AuthorizeSnapshotAccessResponse.proto"
    redshift
    (Proxy :: Proxy AuthorizeSnapshotAccess)

responseRebootCluster :: RebootClusterResponse -> TestTree
responseRebootCluster = res
    "RebootClusterResponse"
    "fixture/RebootClusterResponse.proto"
    redshift
    (Proxy :: Proxy RebootCluster)

responseDeleteCluster :: DeleteClusterResponse -> TestTree
responseDeleteCluster = res
    "DeleteClusterResponse"
    "fixture/DeleteClusterResponse.proto"
    redshift
    (Proxy :: Proxy DeleteCluster)

responseCreateEventSubscription :: CreateEventSubscriptionResponse -> TestTree
responseCreateEventSubscription = res
    "CreateEventSubscriptionResponse"
    "fixture/CreateEventSubscriptionResponse.proto"
    redshift
    (Proxy :: Proxy CreateEventSubscription)

responseDescribeOrderableClusterOptions :: DescribeOrderableClusterOptionsResponse -> TestTree
responseDescribeOrderableClusterOptions = res
    "DescribeOrderableClusterOptionsResponse"
    "fixture/DescribeOrderableClusterOptionsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeOrderableClusterOptions)

responseCreateCluster :: CreateClusterResponse -> TestTree
responseCreateCluster = res
    "CreateClusterResponse"
    "fixture/CreateClusterResponse.proto"
    redshift
    (Proxy :: Proxy CreateCluster)

responseCreateHSMClientCertificate :: CreateHSMClientCertificateResponse -> TestTree
responseCreateHSMClientCertificate = res
    "CreateHSMClientCertificateResponse"
    "fixture/CreateHSMClientCertificateResponse.proto"
    redshift
    (Proxy :: Proxy CreateHSMClientCertificate)

responseRestoreTableFromClusterSnapshot :: RestoreTableFromClusterSnapshotResponse -> TestTree
responseRestoreTableFromClusterSnapshot = res
    "RestoreTableFromClusterSnapshotResponse"
    "fixture/RestoreTableFromClusterSnapshotResponse.proto"
    redshift
    (Proxy :: Proxy RestoreTableFromClusterSnapshot)

responseDescribeDefaultClusterParameters :: DescribeDefaultClusterParametersResponse -> TestTree
responseDescribeDefaultClusterParameters = res
    "DescribeDefaultClusterParametersResponse"
    "fixture/DescribeDefaultClusterParametersResponse.proto"
    redshift
    (Proxy :: Proxy DescribeDefaultClusterParameters)

responseDeleteEventSubscription :: DeleteEventSubscriptionResponse -> TestTree
responseDeleteEventSubscription = res
    "DeleteEventSubscriptionResponse"
    "fixture/DeleteEventSubscriptionResponse.proto"
    redshift
    (Proxy :: Proxy DeleteEventSubscription)

responseResetClusterParameterGroup :: ClusterParameterGroupNameMessage -> TestTree
responseResetClusterParameterGroup = res
    "ResetClusterParameterGroupResponse"
    "fixture/ResetClusterParameterGroupResponse.proto"
    redshift
    (Proxy :: Proxy ResetClusterParameterGroup)

responseDescribeEventSubscriptions :: DescribeEventSubscriptionsResponse -> TestTree
responseDescribeEventSubscriptions = res
    "DescribeEventSubscriptionsResponse"
    "fixture/DescribeEventSubscriptionsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeEventSubscriptions)

responseRevokeClusterSecurityGroupIngress :: RevokeClusterSecurityGroupIngressResponse -> TestTree
responseRevokeClusterSecurityGroupIngress = res
    "RevokeClusterSecurityGroupIngressResponse"
    "fixture/RevokeClusterSecurityGroupIngressResponse.proto"
    redshift
    (Proxy :: Proxy RevokeClusterSecurityGroupIngress)

responseDescribeHSMClientCertificates :: DescribeHSMClientCertificatesResponse -> TestTree
responseDescribeHSMClientCertificates = res
    "DescribeHSMClientCertificatesResponse"
    "fixture/DescribeHSMClientCertificatesResponse.proto"
    redshift
    (Proxy :: Proxy DescribeHSMClientCertificates)

responseModifyClusterParameterGroup :: ClusterParameterGroupNameMessage -> TestTree
responseModifyClusterParameterGroup = res
    "ModifyClusterParameterGroupResponse"
    "fixture/ModifyClusterParameterGroupResponse.proto"
    redshift
    (Proxy :: Proxy ModifyClusterParameterGroup)

responseGetClusterCredentials :: GetClusterCredentialsResponse -> TestTree
responseGetClusterCredentials = res
    "GetClusterCredentialsResponse"
    "fixture/GetClusterCredentialsResponse.proto"
    redshift
    (Proxy :: Proxy GetClusterCredentials)

responseCreateClusterSecurityGroup :: CreateClusterSecurityGroupResponse -> TestTree
responseCreateClusterSecurityGroup = res
    "CreateClusterSecurityGroupResponse"
    "fixture/CreateClusterSecurityGroupResponse.proto"
    redshift
    (Proxy :: Proxy CreateClusterSecurityGroup)

responseDescribeEventCategories :: DescribeEventCategoriesResponse -> TestTree
responseDescribeEventCategories = res
    "DescribeEventCategoriesResponse"
    "fixture/DescribeEventCategoriesResponse.proto"
    redshift
    (Proxy :: Proxy DescribeEventCategories)

responseDescribeResize :: DescribeResizeResponse -> TestTree
responseDescribeResize = res
    "DescribeResizeResponse"
    "fixture/DescribeResizeResponse.proto"
    redshift
    (Proxy :: Proxy DescribeResize)

responseDeleteHSMConfiguration :: DeleteHSMConfigurationResponse -> TestTree
responseDeleteHSMConfiguration = res
    "DeleteHSMConfigurationResponse"
    "fixture/DeleteHSMConfigurationResponse.proto"
    redshift
    (Proxy :: Proxy DeleteHSMConfiguration)

responseAuthorizeClusterSecurityGroupIngress :: AuthorizeClusterSecurityGroupIngressResponse -> TestTree
responseAuthorizeClusterSecurityGroupIngress = res
    "AuthorizeClusterSecurityGroupIngressResponse"
    "fixture/AuthorizeClusterSecurityGroupIngressResponse.proto"
    redshift
    (Proxy :: Proxy AuthorizeClusterSecurityGroupIngress)

responseDescribeTableRestoreStatus :: DescribeTableRestoreStatusResponse -> TestTree
responseDescribeTableRestoreStatus = res
    "DescribeTableRestoreStatusResponse"
    "fixture/DescribeTableRestoreStatusResponse.proto"
    redshift
    (Proxy :: Proxy DescribeTableRestoreStatus)

responseCreateClusterSnapshot :: CreateClusterSnapshotResponse -> TestTree
responseCreateClusterSnapshot = res
    "CreateClusterSnapshotResponse"
    "fixture/CreateClusterSnapshotResponse.proto"
    redshift
    (Proxy :: Proxy CreateClusterSnapshot)

responseCreateHSMConfiguration :: CreateHSMConfigurationResponse -> TestTree
responseCreateHSMConfiguration = res
    "CreateHSMConfigurationResponse"
    "fixture/CreateHSMConfigurationResponse.proto"
    redshift
    (Proxy :: Proxy CreateHSMConfiguration)

responseDescribeLoggingStatus :: LoggingStatus -> TestTree
responseDescribeLoggingStatus = res
    "DescribeLoggingStatusResponse"
    "fixture/DescribeLoggingStatusResponse.proto"
    redshift
    (Proxy :: Proxy DescribeLoggingStatus)

responseModifyCluster :: ModifyClusterResponse -> TestTree
responseModifyCluster = res
    "ModifyClusterResponse"
    "fixture/ModifyClusterResponse.proto"
    redshift
    (Proxy :: Proxy ModifyCluster)

responseDeleteClusterSecurityGroup :: DeleteClusterSecurityGroupResponse -> TestTree
responseDeleteClusterSecurityGroup = res
    "DeleteClusterSecurityGroupResponse"
    "fixture/DeleteClusterSecurityGroupResponse.proto"
    redshift
    (Proxy :: Proxy DeleteClusterSecurityGroup)

responseDisableSnapshotCopy :: DisableSnapshotCopyResponse -> TestTree
responseDisableSnapshotCopy = res
    "DisableSnapshotCopyResponse"
    "fixture/DisableSnapshotCopyResponse.proto"
    redshift
    (Proxy :: Proxy DisableSnapshotCopy)

responseDescribeClusterParameters :: DescribeClusterParametersResponse -> TestTree
responseDescribeClusterParameters = res
    "DescribeClusterParametersResponse"
    "fixture/DescribeClusterParametersResponse.proto"
    redshift
    (Proxy :: Proxy DescribeClusterParameters)

responseRestoreFromClusterSnapshot :: RestoreFromClusterSnapshotResponse -> TestTree
responseRestoreFromClusterSnapshot = res
    "RestoreFromClusterSnapshotResponse"
    "fixture/RestoreFromClusterSnapshotResponse.proto"
    redshift
    (Proxy :: Proxy RestoreFromClusterSnapshot)

responseCreateClusterParameterGroup :: CreateClusterParameterGroupResponse -> TestTree
responseCreateClusterParameterGroup = res
    "CreateClusterParameterGroupResponse"
    "fixture/CreateClusterParameterGroupResponse.proto"
    redshift
    (Proxy :: Proxy CreateClusterParameterGroup)

responseRevokeSnapshotAccess :: RevokeSnapshotAccessResponse -> TestTree
responseRevokeSnapshotAccess = res
    "RevokeSnapshotAccessResponse"
    "fixture/RevokeSnapshotAccessResponse.proto"
    redshift
    (Proxy :: Proxy RevokeSnapshotAccess)

responseDescribeHSMConfigurations :: DescribeHSMConfigurationsResponse -> TestTree
responseDescribeHSMConfigurations = res
    "DescribeHSMConfigurationsResponse"
    "fixture/DescribeHSMConfigurationsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeHSMConfigurations)

responseCreateSnapshotCopyGrant :: CreateSnapshotCopyGrantResponse -> TestTree
responseCreateSnapshotCopyGrant = res
    "CreateSnapshotCopyGrantResponse"
    "fixture/CreateSnapshotCopyGrantResponse.proto"
    redshift
    (Proxy :: Proxy CreateSnapshotCopyGrant)

responseCopyClusterSnapshot :: CopyClusterSnapshotResponse -> TestTree
responseCopyClusterSnapshot = res
    "CopyClusterSnapshotResponse"
    "fixture/CopyClusterSnapshotResponse.proto"
    redshift
    (Proxy :: Proxy CopyClusterSnapshot)

responseDeleteHSMClientCertificate :: DeleteHSMClientCertificateResponse -> TestTree
responseDeleteHSMClientCertificate = res
    "DeleteHSMClientCertificateResponse"
    "fixture/DeleteHSMClientCertificateResponse.proto"
    redshift
    (Proxy :: Proxy DeleteHSMClientCertificate)

responseDeleteSnapshotCopyGrant :: DeleteSnapshotCopyGrantResponse -> TestTree
responseDeleteSnapshotCopyGrant = res
    "DeleteSnapshotCopyGrantResponse"
    "fixture/DeleteSnapshotCopyGrantResponse.proto"
    redshift
    (Proxy :: Proxy DeleteSnapshotCopyGrant)

responseDescribeClusterVersions :: DescribeClusterVersionsResponse -> TestTree
responseDescribeClusterVersions = res
    "DescribeClusterVersionsResponse"
    "fixture/DescribeClusterVersionsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeClusterVersions)

responseModifyClusterSubnetGroup :: ModifyClusterSubnetGroupResponse -> TestTree
responseModifyClusterSubnetGroup = res
    "ModifyClusterSubnetGroupResponse"
    "fixture/ModifyClusterSubnetGroupResponse.proto"
    redshift
    (Proxy :: Proxy ModifyClusterSubnetGroup)

responseRotateEncryptionKey :: RotateEncryptionKeyResponse -> TestTree
responseRotateEncryptionKey = res
    "RotateEncryptionKeyResponse"
    "fixture/RotateEncryptionKeyResponse.proto"
    redshift
    (Proxy :: Proxy RotateEncryptionKey)

responseDescribeSnapshotCopyGrants :: DescribeSnapshotCopyGrantsResponse -> TestTree
responseDescribeSnapshotCopyGrants = res
    "DescribeSnapshotCopyGrantsResponse"
    "fixture/DescribeSnapshotCopyGrantsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeSnapshotCopyGrants)
