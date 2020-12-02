{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Redshift
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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
--         [ requestCancelResize $
--             cancelResize
--
--         , requestDescribeStorage $
--             describeStorage
--
--         , requestDescribeClusters $
--             describeClusters
--
--         , requestDescribeTags $
--             describeTags
--
--         , requestCreateUsageLimit $
--             createUsageLimit
--
--         , requestDeleteClusterSubnetGroup $
--             deleteClusterSubnetGroup
--
--         , requestModifyScheduledAction $
--             modifyScheduledAction
--
--         , requestDisableLogging $
--             disableLogging
--
--         , requestDescribeSnapshotSchedules $
--             describeSnapshotSchedules
--
--         , requestModifyEventSubscription $
--             modifyEventSubscription
--
--         , requestModifyClusterDBRevision $
--             modifyClusterDBRevision
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
--         , requestGetReservedNodeExchangeOfferings $
--             getReservedNodeExchangeOfferings
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
--         , requestBatchDeleteClusterSnapshots $
--             batchDeleteClusterSnapshots
--
--         , requestDeleteTags $
--             deleteTags
--
--         , requestModifyUsageLimit $
--             modifyUsageLimit
--
--         , requestDescribeClusterSubnetGroups $
--             describeClusterSubnetGroups
--
--         , requestResizeCluster $
--             resizeCluster
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
--         , requestResumeCluster $
--             resumeCluster
--
--         , requestDeleteCluster $
--             deleteCluster
--
--         , requestCreateEventSubscription $
--             createEventSubscription
--
--         , requestCreateScheduledAction $
--             createScheduledAction
--
--         , requestDescribeOrderableClusterOptions $
--             describeOrderableClusterOptions
--
--         , requestDescribeClusterTracks $
--             describeClusterTracks
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
--         , requestDeleteScheduledAction $
--             deleteScheduledAction
--
--         , requestDescribeDefaultClusterParameters $
--             describeDefaultClusterParameters
--
--         , requestDeleteEventSubscription $
--             deleteEventSubscription
--
--         , requestModifyClusterSnapshot $
--             modifyClusterSnapshot
--
--         , requestResetClusterParameterGroup $
--             resetClusterParameterGroup
--
--         , requestDescribeScheduledActions $
--             describeScheduledActions
--
--         , requestDescribeEventSubscriptions $
--             describeEventSubscriptions
--
--         , requestDescribeClusterDBRevisions $
--             describeClusterDBRevisions
--
--         , requestBatchModifyClusterSnapshots $
--             batchModifyClusterSnapshots
--
--         , requestDeleteUsageLimit $
--             deleteUsageLimit
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
--         , requestModifyClusterMaintenance $
--             modifyClusterMaintenance
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
--         , requestAcceptReservedNodeExchange $
--             acceptReservedNodeExchange
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
--         , requestCreateSnapshotSchedule $
--             createSnapshotSchedule
--
--         , requestDescribeNodeConfigurationOptions $
--             describeNodeConfigurationOptions
--
--         , requestDisableSnapshotCopy $
--             disableSnapshotCopy
--
--         , requestDescribeClusterParameters $
--             describeClusterParameters
--
--         , requestPauseCluster $
--             pauseCluster
--
--         , requestDeleteSnapshotSchedule $
--             deleteSnapshotSchedule
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
--         , requestDescribeAccountAttributes $
--             describeAccountAttributes
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
--         , requestModifyClusterSnapshotSchedule $
--             modifyClusterSnapshotSchedule
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
--         , requestDescribeUsageLimits $
--             describeUsageLimits
--
--         , requestModifySnapshotSchedule $
--             modifySnapshotSchedule
--
--         , requestRotateEncryptionKey $
--             rotateEncryptionKey
--
--         , requestDescribeSnapshotCopyGrants $
--             describeSnapshotCopyGrants
--
--           ]

--     , testGroup "response"
--         [ responseCancelResize $
--             resizeProgressMessage
--
--         , responseDescribeStorage $
--             describeStorageResponse
--
--         , responseDescribeClusters $
--             describeClustersResponse
--
--         , responseDescribeTags $
--             describeTagsResponse
--
--         , responseCreateUsageLimit $
--             usageLimit
--
--         , responseDeleteClusterSubnetGroup $
--             deleteClusterSubnetGroupResponse
--
--         , responseModifyScheduledAction $
--             scheduledAction
--
--         , responseDisableLogging $
--             loggingStatus
--
--         , responseDescribeSnapshotSchedules $
--             describeSnapshotSchedulesResponse
--
--         , responseModifyEventSubscription $
--             modifyEventSubscriptionResponse
--
--         , responseModifyClusterDBRevision $
--             modifyClusterDBRevisionResponse
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
--         , responseGetReservedNodeExchangeOfferings $
--             getReservedNodeExchangeOfferingsResponse
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
--         , responseBatchDeleteClusterSnapshots $
--             batchDeleteClusterSnapshotsResponse
--
--         , responseDeleteTags $
--             deleteTagsResponse
--
--         , responseModifyUsageLimit $
--             usageLimit
--
--         , responseDescribeClusterSubnetGroups $
--             describeClusterSubnetGroupsResponse
--
--         , responseResizeCluster $
--             resizeClusterResponse
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
--         , responseResumeCluster $
--             resumeClusterResponse
--
--         , responseDeleteCluster $
--             deleteClusterResponse
--
--         , responseCreateEventSubscription $
--             createEventSubscriptionResponse
--
--         , responseCreateScheduledAction $
--             scheduledAction
--
--         , responseDescribeOrderableClusterOptions $
--             describeOrderableClusterOptionsResponse
--
--         , responseDescribeClusterTracks $
--             describeClusterTracksResponse
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
--         , responseDeleteScheduledAction $
--             deleteScheduledActionResponse
--
--         , responseDescribeDefaultClusterParameters $
--             describeDefaultClusterParametersResponse
--
--         , responseDeleteEventSubscription $
--             deleteEventSubscriptionResponse
--
--         , responseModifyClusterSnapshot $
--             modifyClusterSnapshotResponse
--
--         , responseResetClusterParameterGroup $
--             clusterParameterGroupNameMessage
--
--         , responseDescribeScheduledActions $
--             describeScheduledActionsResponse
--
--         , responseDescribeEventSubscriptions $
--             describeEventSubscriptionsResponse
--
--         , responseDescribeClusterDBRevisions $
--             describeClusterDBRevisionsResponse
--
--         , responseBatchModifyClusterSnapshots $
--             batchModifyClusterSnapshotsResponse
--
--         , responseDeleteUsageLimit $
--             deleteUsageLimitResponse
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
--         , responseModifyClusterMaintenance $
--             modifyClusterMaintenanceResponse
--
--         , responseCreateClusterSecurityGroup $
--             createClusterSecurityGroupResponse
--
--         , responseDescribeEventCategories $
--             describeEventCategoriesResponse
--
--         , responseDescribeResize $
--             resizeProgressMessage
--
--         , responseDeleteHSMConfiguration $
--             deleteHSMConfigurationResponse
--
--         , responseAcceptReservedNodeExchange $
--             acceptReservedNodeExchangeResponse
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
--         , responseCreateSnapshotSchedule $
--             snapshotSchedule
--
--         , responseDescribeNodeConfigurationOptions $
--             describeNodeConfigurationOptionsResponse
--
--         , responseDisableSnapshotCopy $
--             disableSnapshotCopyResponse
--
--         , responseDescribeClusterParameters $
--             describeClusterParametersResponse
--
--         , responsePauseCluster $
--             pauseClusterResponse
--
--         , responseDeleteSnapshotSchedule $
--             deleteSnapshotScheduleResponse
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
--         , responseDescribeAccountAttributes $
--             describeAccountAttributesResponse
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
--         , responseModifyClusterSnapshotSchedule $
--             modifyClusterSnapshotScheduleResponse
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
--         , responseDescribeUsageLimits $
--             describeUsageLimitsResponse
--
--         , responseModifySnapshotSchedule $
--             snapshotSchedule
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

requestCancelResize :: CancelResize -> TestTree
requestCancelResize =
  req
    "CancelResize"
    "fixture/CancelResize.yaml"

requestDescribeStorage :: DescribeStorage -> TestTree
requestDescribeStorage =
  req
    "DescribeStorage"
    "fixture/DescribeStorage.yaml"

requestDescribeClusters :: DescribeClusters -> TestTree
requestDescribeClusters =
  req
    "DescribeClusters"
    "fixture/DescribeClusters.yaml"

requestDescribeTags :: DescribeTags -> TestTree
requestDescribeTags =
  req
    "DescribeTags"
    "fixture/DescribeTags.yaml"

requestCreateUsageLimit :: CreateUsageLimit -> TestTree
requestCreateUsageLimit =
  req
    "CreateUsageLimit"
    "fixture/CreateUsageLimit.yaml"

requestDeleteClusterSubnetGroup :: DeleteClusterSubnetGroup -> TestTree
requestDeleteClusterSubnetGroup =
  req
    "DeleteClusterSubnetGroup"
    "fixture/DeleteClusterSubnetGroup.yaml"

requestModifyScheduledAction :: ModifyScheduledAction -> TestTree
requestModifyScheduledAction =
  req
    "ModifyScheduledAction"
    "fixture/ModifyScheduledAction.yaml"

requestDisableLogging :: DisableLogging -> TestTree
requestDisableLogging =
  req
    "DisableLogging"
    "fixture/DisableLogging.yaml"

requestDescribeSnapshotSchedules :: DescribeSnapshotSchedules -> TestTree
requestDescribeSnapshotSchedules =
  req
    "DescribeSnapshotSchedules"
    "fixture/DescribeSnapshotSchedules.yaml"

requestModifyEventSubscription :: ModifyEventSubscription -> TestTree
requestModifyEventSubscription =
  req
    "ModifyEventSubscription"
    "fixture/ModifyEventSubscription.yaml"

requestModifyClusterDBRevision :: ModifyClusterDBRevision -> TestTree
requestModifyClusterDBRevision =
  req
    "ModifyClusterDBRevision"
    "fixture/ModifyClusterDBRevision.yaml"

requestDeleteClusterSnapshot :: DeleteClusterSnapshot -> TestTree
requestDeleteClusterSnapshot =
  req
    "DeleteClusterSnapshot"
    "fixture/DeleteClusterSnapshot.yaml"

requestPurchaseReservedNodeOffering :: PurchaseReservedNodeOffering -> TestTree
requestPurchaseReservedNodeOffering =
  req
    "PurchaseReservedNodeOffering"
    "fixture/PurchaseReservedNodeOffering.yaml"

requestDescribeReservedNodeOfferings :: DescribeReservedNodeOfferings -> TestTree
requestDescribeReservedNodeOfferings =
  req
    "DescribeReservedNodeOfferings"
    "fixture/DescribeReservedNodeOfferings.yaml"

requestDescribeEvents :: DescribeEvents -> TestTree
requestDescribeEvents =
  req
    "DescribeEvents"
    "fixture/DescribeEvents.yaml"

requestDescribeReservedNodes :: DescribeReservedNodes -> TestTree
requestDescribeReservedNodes =
  req
    "DescribeReservedNodes"
    "fixture/DescribeReservedNodes.yaml"

requestGetReservedNodeExchangeOfferings :: GetReservedNodeExchangeOfferings -> TestTree
requestGetReservedNodeExchangeOfferings =
  req
    "GetReservedNodeExchangeOfferings"
    "fixture/GetReservedNodeExchangeOfferings.yaml"

requestDescribeClusterParameterGroups :: DescribeClusterParameterGroups -> TestTree
requestDescribeClusterParameterGroups =
  req
    "DescribeClusterParameterGroups"
    "fixture/DescribeClusterParameterGroups.yaml"

requestEnableLogging :: EnableLogging -> TestTree
requestEnableLogging =
  req
    "EnableLogging"
    "fixture/EnableLogging.yaml"

requestCreateClusterSubnetGroup :: CreateClusterSubnetGroup -> TestTree
requestCreateClusterSubnetGroup =
  req
    "CreateClusterSubnetGroup"
    "fixture/CreateClusterSubnetGroup.yaml"

requestDeleteClusterParameterGroup :: DeleteClusterParameterGroup -> TestTree
requestDeleteClusterParameterGroup =
  req
    "DeleteClusterParameterGroup"
    "fixture/DeleteClusterParameterGroup.yaml"

requestDescribeClusterSecurityGroups :: DescribeClusterSecurityGroups -> TestTree
requestDescribeClusterSecurityGroups =
  req
    "DescribeClusterSecurityGroups"
    "fixture/DescribeClusterSecurityGroups.yaml"

requestCreateTags :: CreateTags -> TestTree
requestCreateTags =
  req
    "CreateTags"
    "fixture/CreateTags.yaml"

requestEnableSnapshotCopy :: EnableSnapshotCopy -> TestTree
requestEnableSnapshotCopy =
  req
    "EnableSnapshotCopy"
    "fixture/EnableSnapshotCopy.yaml"

requestDescribeClusterSnapshots :: DescribeClusterSnapshots -> TestTree
requestDescribeClusterSnapshots =
  req
    "DescribeClusterSnapshots"
    "fixture/DescribeClusterSnapshots.yaml"

requestBatchDeleteClusterSnapshots :: BatchDeleteClusterSnapshots -> TestTree
requestBatchDeleteClusterSnapshots =
  req
    "BatchDeleteClusterSnapshots"
    "fixture/BatchDeleteClusterSnapshots.yaml"

requestDeleteTags :: DeleteTags -> TestTree
requestDeleteTags =
  req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

requestModifyUsageLimit :: ModifyUsageLimit -> TestTree
requestModifyUsageLimit =
  req
    "ModifyUsageLimit"
    "fixture/ModifyUsageLimit.yaml"

requestDescribeClusterSubnetGroups :: DescribeClusterSubnetGroups -> TestTree
requestDescribeClusterSubnetGroups =
  req
    "DescribeClusterSubnetGroups"
    "fixture/DescribeClusterSubnetGroups.yaml"

requestResizeCluster :: ResizeCluster -> TestTree
requestResizeCluster =
  req
    "ResizeCluster"
    "fixture/ResizeCluster.yaml"

requestModifySnapshotCopyRetentionPeriod :: ModifySnapshotCopyRetentionPeriod -> TestTree
requestModifySnapshotCopyRetentionPeriod =
  req
    "ModifySnapshotCopyRetentionPeriod"
    "fixture/ModifySnapshotCopyRetentionPeriod.yaml"

requestModifyClusterIAMRoles :: ModifyClusterIAMRoles -> TestTree
requestModifyClusterIAMRoles =
  req
    "ModifyClusterIAMRoles"
    "fixture/ModifyClusterIAMRoles.yaml"

requestAuthorizeSnapshotAccess :: AuthorizeSnapshotAccess -> TestTree
requestAuthorizeSnapshotAccess =
  req
    "AuthorizeSnapshotAccess"
    "fixture/AuthorizeSnapshotAccess.yaml"

requestRebootCluster :: RebootCluster -> TestTree
requestRebootCluster =
  req
    "RebootCluster"
    "fixture/RebootCluster.yaml"

requestResumeCluster :: ResumeCluster -> TestTree
requestResumeCluster =
  req
    "ResumeCluster"
    "fixture/ResumeCluster.yaml"

requestDeleteCluster :: DeleteCluster -> TestTree
requestDeleteCluster =
  req
    "DeleteCluster"
    "fixture/DeleteCluster.yaml"

requestCreateEventSubscription :: CreateEventSubscription -> TestTree
requestCreateEventSubscription =
  req
    "CreateEventSubscription"
    "fixture/CreateEventSubscription.yaml"

requestCreateScheduledAction :: CreateScheduledAction -> TestTree
requestCreateScheduledAction =
  req
    "CreateScheduledAction"
    "fixture/CreateScheduledAction.yaml"

requestDescribeOrderableClusterOptions :: DescribeOrderableClusterOptions -> TestTree
requestDescribeOrderableClusterOptions =
  req
    "DescribeOrderableClusterOptions"
    "fixture/DescribeOrderableClusterOptions.yaml"

requestDescribeClusterTracks :: DescribeClusterTracks -> TestTree
requestDescribeClusterTracks =
  req
    "DescribeClusterTracks"
    "fixture/DescribeClusterTracks.yaml"

requestCreateCluster :: CreateCluster -> TestTree
requestCreateCluster =
  req
    "CreateCluster"
    "fixture/CreateCluster.yaml"

requestCreateHSMClientCertificate :: CreateHSMClientCertificate -> TestTree
requestCreateHSMClientCertificate =
  req
    "CreateHSMClientCertificate"
    "fixture/CreateHSMClientCertificate.yaml"

requestRestoreTableFromClusterSnapshot :: RestoreTableFromClusterSnapshot -> TestTree
requestRestoreTableFromClusterSnapshot =
  req
    "RestoreTableFromClusterSnapshot"
    "fixture/RestoreTableFromClusterSnapshot.yaml"

requestDeleteScheduledAction :: DeleteScheduledAction -> TestTree
requestDeleteScheduledAction =
  req
    "DeleteScheduledAction"
    "fixture/DeleteScheduledAction.yaml"

requestDescribeDefaultClusterParameters :: DescribeDefaultClusterParameters -> TestTree
requestDescribeDefaultClusterParameters =
  req
    "DescribeDefaultClusterParameters"
    "fixture/DescribeDefaultClusterParameters.yaml"

requestDeleteEventSubscription :: DeleteEventSubscription -> TestTree
requestDeleteEventSubscription =
  req
    "DeleteEventSubscription"
    "fixture/DeleteEventSubscription.yaml"

requestModifyClusterSnapshot :: ModifyClusterSnapshot -> TestTree
requestModifyClusterSnapshot =
  req
    "ModifyClusterSnapshot"
    "fixture/ModifyClusterSnapshot.yaml"

requestResetClusterParameterGroup :: ResetClusterParameterGroup -> TestTree
requestResetClusterParameterGroup =
  req
    "ResetClusterParameterGroup"
    "fixture/ResetClusterParameterGroup.yaml"

requestDescribeScheduledActions :: DescribeScheduledActions -> TestTree
requestDescribeScheduledActions =
  req
    "DescribeScheduledActions"
    "fixture/DescribeScheduledActions.yaml"

requestDescribeEventSubscriptions :: DescribeEventSubscriptions -> TestTree
requestDescribeEventSubscriptions =
  req
    "DescribeEventSubscriptions"
    "fixture/DescribeEventSubscriptions.yaml"

requestDescribeClusterDBRevisions :: DescribeClusterDBRevisions -> TestTree
requestDescribeClusterDBRevisions =
  req
    "DescribeClusterDBRevisions"
    "fixture/DescribeClusterDBRevisions.yaml"

requestBatchModifyClusterSnapshots :: BatchModifyClusterSnapshots -> TestTree
requestBatchModifyClusterSnapshots =
  req
    "BatchModifyClusterSnapshots"
    "fixture/BatchModifyClusterSnapshots.yaml"

requestDeleteUsageLimit :: DeleteUsageLimit -> TestTree
requestDeleteUsageLimit =
  req
    "DeleteUsageLimit"
    "fixture/DeleteUsageLimit.yaml"

requestRevokeClusterSecurityGroupIngress :: RevokeClusterSecurityGroupIngress -> TestTree
requestRevokeClusterSecurityGroupIngress =
  req
    "RevokeClusterSecurityGroupIngress"
    "fixture/RevokeClusterSecurityGroupIngress.yaml"

requestDescribeHSMClientCertificates :: DescribeHSMClientCertificates -> TestTree
requestDescribeHSMClientCertificates =
  req
    "DescribeHSMClientCertificates"
    "fixture/DescribeHSMClientCertificates.yaml"

requestModifyClusterParameterGroup :: ModifyClusterParameterGroup -> TestTree
requestModifyClusterParameterGroup =
  req
    "ModifyClusterParameterGroup"
    "fixture/ModifyClusterParameterGroup.yaml"

requestGetClusterCredentials :: GetClusterCredentials -> TestTree
requestGetClusterCredentials =
  req
    "GetClusterCredentials"
    "fixture/GetClusterCredentials.yaml"

requestModifyClusterMaintenance :: ModifyClusterMaintenance -> TestTree
requestModifyClusterMaintenance =
  req
    "ModifyClusterMaintenance"
    "fixture/ModifyClusterMaintenance.yaml"

requestCreateClusterSecurityGroup :: CreateClusterSecurityGroup -> TestTree
requestCreateClusterSecurityGroup =
  req
    "CreateClusterSecurityGroup"
    "fixture/CreateClusterSecurityGroup.yaml"

requestDescribeEventCategories :: DescribeEventCategories -> TestTree
requestDescribeEventCategories =
  req
    "DescribeEventCategories"
    "fixture/DescribeEventCategories.yaml"

requestDescribeResize :: DescribeResize -> TestTree
requestDescribeResize =
  req
    "DescribeResize"
    "fixture/DescribeResize.yaml"

requestDeleteHSMConfiguration :: DeleteHSMConfiguration -> TestTree
requestDeleteHSMConfiguration =
  req
    "DeleteHSMConfiguration"
    "fixture/DeleteHSMConfiguration.yaml"

requestAcceptReservedNodeExchange :: AcceptReservedNodeExchange -> TestTree
requestAcceptReservedNodeExchange =
  req
    "AcceptReservedNodeExchange"
    "fixture/AcceptReservedNodeExchange.yaml"

requestAuthorizeClusterSecurityGroupIngress :: AuthorizeClusterSecurityGroupIngress -> TestTree
requestAuthorizeClusterSecurityGroupIngress =
  req
    "AuthorizeClusterSecurityGroupIngress"
    "fixture/AuthorizeClusterSecurityGroupIngress.yaml"

requestDescribeTableRestoreStatus :: DescribeTableRestoreStatus -> TestTree
requestDescribeTableRestoreStatus =
  req
    "DescribeTableRestoreStatus"
    "fixture/DescribeTableRestoreStatus.yaml"

requestCreateClusterSnapshot :: CreateClusterSnapshot -> TestTree
requestCreateClusterSnapshot =
  req
    "CreateClusterSnapshot"
    "fixture/CreateClusterSnapshot.yaml"

requestCreateHSMConfiguration :: CreateHSMConfiguration -> TestTree
requestCreateHSMConfiguration =
  req
    "CreateHSMConfiguration"
    "fixture/CreateHSMConfiguration.yaml"

requestDescribeLoggingStatus :: DescribeLoggingStatus -> TestTree
requestDescribeLoggingStatus =
  req
    "DescribeLoggingStatus"
    "fixture/DescribeLoggingStatus.yaml"

requestModifyCluster :: ModifyCluster -> TestTree
requestModifyCluster =
  req
    "ModifyCluster"
    "fixture/ModifyCluster.yaml"

requestDeleteClusterSecurityGroup :: DeleteClusterSecurityGroup -> TestTree
requestDeleteClusterSecurityGroup =
  req
    "DeleteClusterSecurityGroup"
    "fixture/DeleteClusterSecurityGroup.yaml"

requestCreateSnapshotSchedule :: CreateSnapshotSchedule -> TestTree
requestCreateSnapshotSchedule =
  req
    "CreateSnapshotSchedule"
    "fixture/CreateSnapshotSchedule.yaml"

requestDescribeNodeConfigurationOptions :: DescribeNodeConfigurationOptions -> TestTree
requestDescribeNodeConfigurationOptions =
  req
    "DescribeNodeConfigurationOptions"
    "fixture/DescribeNodeConfigurationOptions.yaml"

requestDisableSnapshotCopy :: DisableSnapshotCopy -> TestTree
requestDisableSnapshotCopy =
  req
    "DisableSnapshotCopy"
    "fixture/DisableSnapshotCopy.yaml"

requestDescribeClusterParameters :: DescribeClusterParameters -> TestTree
requestDescribeClusterParameters =
  req
    "DescribeClusterParameters"
    "fixture/DescribeClusterParameters.yaml"

requestPauseCluster :: PauseCluster -> TestTree
requestPauseCluster =
  req
    "PauseCluster"
    "fixture/PauseCluster.yaml"

requestDeleteSnapshotSchedule :: DeleteSnapshotSchedule -> TestTree
requestDeleteSnapshotSchedule =
  req
    "DeleteSnapshotSchedule"
    "fixture/DeleteSnapshotSchedule.yaml"

requestRestoreFromClusterSnapshot :: RestoreFromClusterSnapshot -> TestTree
requestRestoreFromClusterSnapshot =
  req
    "RestoreFromClusterSnapshot"
    "fixture/RestoreFromClusterSnapshot.yaml"

requestCreateClusterParameterGroup :: CreateClusterParameterGroup -> TestTree
requestCreateClusterParameterGroup =
  req
    "CreateClusterParameterGroup"
    "fixture/CreateClusterParameterGroup.yaml"

requestRevokeSnapshotAccess :: RevokeSnapshotAccess -> TestTree
requestRevokeSnapshotAccess =
  req
    "RevokeSnapshotAccess"
    "fixture/RevokeSnapshotAccess.yaml"

requestDescribeHSMConfigurations :: DescribeHSMConfigurations -> TestTree
requestDescribeHSMConfigurations =
  req
    "DescribeHSMConfigurations"
    "fixture/DescribeHSMConfigurations.yaml"

requestDescribeAccountAttributes :: DescribeAccountAttributes -> TestTree
requestDescribeAccountAttributes =
  req
    "DescribeAccountAttributes"
    "fixture/DescribeAccountAttributes.yaml"

requestCreateSnapshotCopyGrant :: CreateSnapshotCopyGrant -> TestTree
requestCreateSnapshotCopyGrant =
  req
    "CreateSnapshotCopyGrant"
    "fixture/CreateSnapshotCopyGrant.yaml"

requestCopyClusterSnapshot :: CopyClusterSnapshot -> TestTree
requestCopyClusterSnapshot =
  req
    "CopyClusterSnapshot"
    "fixture/CopyClusterSnapshot.yaml"

requestDeleteHSMClientCertificate :: DeleteHSMClientCertificate -> TestTree
requestDeleteHSMClientCertificate =
  req
    "DeleteHSMClientCertificate"
    "fixture/DeleteHSMClientCertificate.yaml"

requestModifyClusterSnapshotSchedule :: ModifyClusterSnapshotSchedule -> TestTree
requestModifyClusterSnapshotSchedule =
  req
    "ModifyClusterSnapshotSchedule"
    "fixture/ModifyClusterSnapshotSchedule.yaml"

requestDeleteSnapshotCopyGrant :: DeleteSnapshotCopyGrant -> TestTree
requestDeleteSnapshotCopyGrant =
  req
    "DeleteSnapshotCopyGrant"
    "fixture/DeleteSnapshotCopyGrant.yaml"

requestDescribeClusterVersions :: DescribeClusterVersions -> TestTree
requestDescribeClusterVersions =
  req
    "DescribeClusterVersions"
    "fixture/DescribeClusterVersions.yaml"

requestModifyClusterSubnetGroup :: ModifyClusterSubnetGroup -> TestTree
requestModifyClusterSubnetGroup =
  req
    "ModifyClusterSubnetGroup"
    "fixture/ModifyClusterSubnetGroup.yaml"

requestDescribeUsageLimits :: DescribeUsageLimits -> TestTree
requestDescribeUsageLimits =
  req
    "DescribeUsageLimits"
    "fixture/DescribeUsageLimits.yaml"

requestModifySnapshotSchedule :: ModifySnapshotSchedule -> TestTree
requestModifySnapshotSchedule =
  req
    "ModifySnapshotSchedule"
    "fixture/ModifySnapshotSchedule.yaml"

requestRotateEncryptionKey :: RotateEncryptionKey -> TestTree
requestRotateEncryptionKey =
  req
    "RotateEncryptionKey"
    "fixture/RotateEncryptionKey.yaml"

requestDescribeSnapshotCopyGrants :: DescribeSnapshotCopyGrants -> TestTree
requestDescribeSnapshotCopyGrants =
  req
    "DescribeSnapshotCopyGrants"
    "fixture/DescribeSnapshotCopyGrants.yaml"

-- Responses

responseCancelResize :: ResizeProgressMessage -> TestTree
responseCancelResize =
  res
    "CancelResizeResponse"
    "fixture/CancelResizeResponse.proto"
    redshift
    (Proxy :: Proxy CancelResize)

responseDescribeStorage :: DescribeStorageResponse -> TestTree
responseDescribeStorage =
  res
    "DescribeStorageResponse"
    "fixture/DescribeStorageResponse.proto"
    redshift
    (Proxy :: Proxy DescribeStorage)

responseDescribeClusters :: DescribeClustersResponse -> TestTree
responseDescribeClusters =
  res
    "DescribeClustersResponse"
    "fixture/DescribeClustersResponse.proto"
    redshift
    (Proxy :: Proxy DescribeClusters)

responseDescribeTags :: DescribeTagsResponse -> TestTree
responseDescribeTags =
  res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeTags)

responseCreateUsageLimit :: UsageLimit -> TestTree
responseCreateUsageLimit =
  res
    "CreateUsageLimitResponse"
    "fixture/CreateUsageLimitResponse.proto"
    redshift
    (Proxy :: Proxy CreateUsageLimit)

responseDeleteClusterSubnetGroup :: DeleteClusterSubnetGroupResponse -> TestTree
responseDeleteClusterSubnetGroup =
  res
    "DeleteClusterSubnetGroupResponse"
    "fixture/DeleteClusterSubnetGroupResponse.proto"
    redshift
    (Proxy :: Proxy DeleteClusterSubnetGroup)

responseModifyScheduledAction :: ScheduledAction -> TestTree
responseModifyScheduledAction =
  res
    "ModifyScheduledActionResponse"
    "fixture/ModifyScheduledActionResponse.proto"
    redshift
    (Proxy :: Proxy ModifyScheduledAction)

responseDisableLogging :: LoggingStatus -> TestTree
responseDisableLogging =
  res
    "DisableLoggingResponse"
    "fixture/DisableLoggingResponse.proto"
    redshift
    (Proxy :: Proxy DisableLogging)

responseDescribeSnapshotSchedules :: DescribeSnapshotSchedulesResponse -> TestTree
responseDescribeSnapshotSchedules =
  res
    "DescribeSnapshotSchedulesResponse"
    "fixture/DescribeSnapshotSchedulesResponse.proto"
    redshift
    (Proxy :: Proxy DescribeSnapshotSchedules)

responseModifyEventSubscription :: ModifyEventSubscriptionResponse -> TestTree
responseModifyEventSubscription =
  res
    "ModifyEventSubscriptionResponse"
    "fixture/ModifyEventSubscriptionResponse.proto"
    redshift
    (Proxy :: Proxy ModifyEventSubscription)

responseModifyClusterDBRevision :: ModifyClusterDBRevisionResponse -> TestTree
responseModifyClusterDBRevision =
  res
    "ModifyClusterDBRevisionResponse"
    "fixture/ModifyClusterDBRevisionResponse.proto"
    redshift
    (Proxy :: Proxy ModifyClusterDBRevision)

responseDeleteClusterSnapshot :: DeleteClusterSnapshotResponse -> TestTree
responseDeleteClusterSnapshot =
  res
    "DeleteClusterSnapshotResponse"
    "fixture/DeleteClusterSnapshotResponse.proto"
    redshift
    (Proxy :: Proxy DeleteClusterSnapshot)

responsePurchaseReservedNodeOffering :: PurchaseReservedNodeOfferingResponse -> TestTree
responsePurchaseReservedNodeOffering =
  res
    "PurchaseReservedNodeOfferingResponse"
    "fixture/PurchaseReservedNodeOfferingResponse.proto"
    redshift
    (Proxy :: Proxy PurchaseReservedNodeOffering)

responseDescribeReservedNodeOfferings :: DescribeReservedNodeOfferingsResponse -> TestTree
responseDescribeReservedNodeOfferings =
  res
    "DescribeReservedNodeOfferingsResponse"
    "fixture/DescribeReservedNodeOfferingsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeReservedNodeOfferings)

responseDescribeEvents :: DescribeEventsResponse -> TestTree
responseDescribeEvents =
  res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeEvents)

responseDescribeReservedNodes :: DescribeReservedNodesResponse -> TestTree
responseDescribeReservedNodes =
  res
    "DescribeReservedNodesResponse"
    "fixture/DescribeReservedNodesResponse.proto"
    redshift
    (Proxy :: Proxy DescribeReservedNodes)

responseGetReservedNodeExchangeOfferings :: GetReservedNodeExchangeOfferingsResponse -> TestTree
responseGetReservedNodeExchangeOfferings =
  res
    "GetReservedNodeExchangeOfferingsResponse"
    "fixture/GetReservedNodeExchangeOfferingsResponse.proto"
    redshift
    (Proxy :: Proxy GetReservedNodeExchangeOfferings)

responseDescribeClusterParameterGroups :: DescribeClusterParameterGroupsResponse -> TestTree
responseDescribeClusterParameterGroups =
  res
    "DescribeClusterParameterGroupsResponse"
    "fixture/DescribeClusterParameterGroupsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeClusterParameterGroups)

responseEnableLogging :: LoggingStatus -> TestTree
responseEnableLogging =
  res
    "EnableLoggingResponse"
    "fixture/EnableLoggingResponse.proto"
    redshift
    (Proxy :: Proxy EnableLogging)

responseCreateClusterSubnetGroup :: CreateClusterSubnetGroupResponse -> TestTree
responseCreateClusterSubnetGroup =
  res
    "CreateClusterSubnetGroupResponse"
    "fixture/CreateClusterSubnetGroupResponse.proto"
    redshift
    (Proxy :: Proxy CreateClusterSubnetGroup)

responseDeleteClusterParameterGroup :: DeleteClusterParameterGroupResponse -> TestTree
responseDeleteClusterParameterGroup =
  res
    "DeleteClusterParameterGroupResponse"
    "fixture/DeleteClusterParameterGroupResponse.proto"
    redshift
    (Proxy :: Proxy DeleteClusterParameterGroup)

responseDescribeClusterSecurityGroups :: DescribeClusterSecurityGroupsResponse -> TestTree
responseDescribeClusterSecurityGroups =
  res
    "DescribeClusterSecurityGroupsResponse"
    "fixture/DescribeClusterSecurityGroupsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeClusterSecurityGroups)

responseCreateTags :: CreateTagsResponse -> TestTree
responseCreateTags =
  res
    "CreateTagsResponse"
    "fixture/CreateTagsResponse.proto"
    redshift
    (Proxy :: Proxy CreateTags)

responseEnableSnapshotCopy :: EnableSnapshotCopyResponse -> TestTree
responseEnableSnapshotCopy =
  res
    "EnableSnapshotCopyResponse"
    "fixture/EnableSnapshotCopyResponse.proto"
    redshift
    (Proxy :: Proxy EnableSnapshotCopy)

responseDescribeClusterSnapshots :: DescribeClusterSnapshotsResponse -> TestTree
responseDescribeClusterSnapshots =
  res
    "DescribeClusterSnapshotsResponse"
    "fixture/DescribeClusterSnapshotsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeClusterSnapshots)

responseBatchDeleteClusterSnapshots :: BatchDeleteClusterSnapshotsResponse -> TestTree
responseBatchDeleteClusterSnapshots =
  res
    "BatchDeleteClusterSnapshotsResponse"
    "fixture/BatchDeleteClusterSnapshotsResponse.proto"
    redshift
    (Proxy :: Proxy BatchDeleteClusterSnapshots)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags =
  res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    redshift
    (Proxy :: Proxy DeleteTags)

responseModifyUsageLimit :: UsageLimit -> TestTree
responseModifyUsageLimit =
  res
    "ModifyUsageLimitResponse"
    "fixture/ModifyUsageLimitResponse.proto"
    redshift
    (Proxy :: Proxy ModifyUsageLimit)

responseDescribeClusterSubnetGroups :: DescribeClusterSubnetGroupsResponse -> TestTree
responseDescribeClusterSubnetGroups =
  res
    "DescribeClusterSubnetGroupsResponse"
    "fixture/DescribeClusterSubnetGroupsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeClusterSubnetGroups)

responseResizeCluster :: ResizeClusterResponse -> TestTree
responseResizeCluster =
  res
    "ResizeClusterResponse"
    "fixture/ResizeClusterResponse.proto"
    redshift
    (Proxy :: Proxy ResizeCluster)

responseModifySnapshotCopyRetentionPeriod :: ModifySnapshotCopyRetentionPeriodResponse -> TestTree
responseModifySnapshotCopyRetentionPeriod =
  res
    "ModifySnapshotCopyRetentionPeriodResponse"
    "fixture/ModifySnapshotCopyRetentionPeriodResponse.proto"
    redshift
    (Proxy :: Proxy ModifySnapshotCopyRetentionPeriod)

responseModifyClusterIAMRoles :: ModifyClusterIAMRolesResponse -> TestTree
responseModifyClusterIAMRoles =
  res
    "ModifyClusterIAMRolesResponse"
    "fixture/ModifyClusterIAMRolesResponse.proto"
    redshift
    (Proxy :: Proxy ModifyClusterIAMRoles)

responseAuthorizeSnapshotAccess :: AuthorizeSnapshotAccessResponse -> TestTree
responseAuthorizeSnapshotAccess =
  res
    "AuthorizeSnapshotAccessResponse"
    "fixture/AuthorizeSnapshotAccessResponse.proto"
    redshift
    (Proxy :: Proxy AuthorizeSnapshotAccess)

responseRebootCluster :: RebootClusterResponse -> TestTree
responseRebootCluster =
  res
    "RebootClusterResponse"
    "fixture/RebootClusterResponse.proto"
    redshift
    (Proxy :: Proxy RebootCluster)

responseResumeCluster :: ResumeClusterResponse -> TestTree
responseResumeCluster =
  res
    "ResumeClusterResponse"
    "fixture/ResumeClusterResponse.proto"
    redshift
    (Proxy :: Proxy ResumeCluster)

responseDeleteCluster :: DeleteClusterResponse -> TestTree
responseDeleteCluster =
  res
    "DeleteClusterResponse"
    "fixture/DeleteClusterResponse.proto"
    redshift
    (Proxy :: Proxy DeleteCluster)

responseCreateEventSubscription :: CreateEventSubscriptionResponse -> TestTree
responseCreateEventSubscription =
  res
    "CreateEventSubscriptionResponse"
    "fixture/CreateEventSubscriptionResponse.proto"
    redshift
    (Proxy :: Proxy CreateEventSubscription)

responseCreateScheduledAction :: ScheduledAction -> TestTree
responseCreateScheduledAction =
  res
    "CreateScheduledActionResponse"
    "fixture/CreateScheduledActionResponse.proto"
    redshift
    (Proxy :: Proxy CreateScheduledAction)

responseDescribeOrderableClusterOptions :: DescribeOrderableClusterOptionsResponse -> TestTree
responseDescribeOrderableClusterOptions =
  res
    "DescribeOrderableClusterOptionsResponse"
    "fixture/DescribeOrderableClusterOptionsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeOrderableClusterOptions)

responseDescribeClusterTracks :: DescribeClusterTracksResponse -> TestTree
responseDescribeClusterTracks =
  res
    "DescribeClusterTracksResponse"
    "fixture/DescribeClusterTracksResponse.proto"
    redshift
    (Proxy :: Proxy DescribeClusterTracks)

responseCreateCluster :: CreateClusterResponse -> TestTree
responseCreateCluster =
  res
    "CreateClusterResponse"
    "fixture/CreateClusterResponse.proto"
    redshift
    (Proxy :: Proxy CreateCluster)

responseCreateHSMClientCertificate :: CreateHSMClientCertificateResponse -> TestTree
responseCreateHSMClientCertificate =
  res
    "CreateHSMClientCertificateResponse"
    "fixture/CreateHSMClientCertificateResponse.proto"
    redshift
    (Proxy :: Proxy CreateHSMClientCertificate)

responseRestoreTableFromClusterSnapshot :: RestoreTableFromClusterSnapshotResponse -> TestTree
responseRestoreTableFromClusterSnapshot =
  res
    "RestoreTableFromClusterSnapshotResponse"
    "fixture/RestoreTableFromClusterSnapshotResponse.proto"
    redshift
    (Proxy :: Proxy RestoreTableFromClusterSnapshot)

responseDeleteScheduledAction :: DeleteScheduledActionResponse -> TestTree
responseDeleteScheduledAction =
  res
    "DeleteScheduledActionResponse"
    "fixture/DeleteScheduledActionResponse.proto"
    redshift
    (Proxy :: Proxy DeleteScheduledAction)

responseDescribeDefaultClusterParameters :: DescribeDefaultClusterParametersResponse -> TestTree
responseDescribeDefaultClusterParameters =
  res
    "DescribeDefaultClusterParametersResponse"
    "fixture/DescribeDefaultClusterParametersResponse.proto"
    redshift
    (Proxy :: Proxy DescribeDefaultClusterParameters)

responseDeleteEventSubscription :: DeleteEventSubscriptionResponse -> TestTree
responseDeleteEventSubscription =
  res
    "DeleteEventSubscriptionResponse"
    "fixture/DeleteEventSubscriptionResponse.proto"
    redshift
    (Proxy :: Proxy DeleteEventSubscription)

responseModifyClusterSnapshot :: ModifyClusterSnapshotResponse -> TestTree
responseModifyClusterSnapshot =
  res
    "ModifyClusterSnapshotResponse"
    "fixture/ModifyClusterSnapshotResponse.proto"
    redshift
    (Proxy :: Proxy ModifyClusterSnapshot)

responseResetClusterParameterGroup :: ClusterParameterGroupNameMessage -> TestTree
responseResetClusterParameterGroup =
  res
    "ResetClusterParameterGroupResponse"
    "fixture/ResetClusterParameterGroupResponse.proto"
    redshift
    (Proxy :: Proxy ResetClusterParameterGroup)

responseDescribeScheduledActions :: DescribeScheduledActionsResponse -> TestTree
responseDescribeScheduledActions =
  res
    "DescribeScheduledActionsResponse"
    "fixture/DescribeScheduledActionsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeScheduledActions)

responseDescribeEventSubscriptions :: DescribeEventSubscriptionsResponse -> TestTree
responseDescribeEventSubscriptions =
  res
    "DescribeEventSubscriptionsResponse"
    "fixture/DescribeEventSubscriptionsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeEventSubscriptions)

responseDescribeClusterDBRevisions :: DescribeClusterDBRevisionsResponse -> TestTree
responseDescribeClusterDBRevisions =
  res
    "DescribeClusterDBRevisionsResponse"
    "fixture/DescribeClusterDBRevisionsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeClusterDBRevisions)

responseBatchModifyClusterSnapshots :: BatchModifyClusterSnapshotsResponse -> TestTree
responseBatchModifyClusterSnapshots =
  res
    "BatchModifyClusterSnapshotsResponse"
    "fixture/BatchModifyClusterSnapshotsResponse.proto"
    redshift
    (Proxy :: Proxy BatchModifyClusterSnapshots)

responseDeleteUsageLimit :: DeleteUsageLimitResponse -> TestTree
responseDeleteUsageLimit =
  res
    "DeleteUsageLimitResponse"
    "fixture/DeleteUsageLimitResponse.proto"
    redshift
    (Proxy :: Proxy DeleteUsageLimit)

responseRevokeClusterSecurityGroupIngress :: RevokeClusterSecurityGroupIngressResponse -> TestTree
responseRevokeClusterSecurityGroupIngress =
  res
    "RevokeClusterSecurityGroupIngressResponse"
    "fixture/RevokeClusterSecurityGroupIngressResponse.proto"
    redshift
    (Proxy :: Proxy RevokeClusterSecurityGroupIngress)

responseDescribeHSMClientCertificates :: DescribeHSMClientCertificatesResponse -> TestTree
responseDescribeHSMClientCertificates =
  res
    "DescribeHSMClientCertificatesResponse"
    "fixture/DescribeHSMClientCertificatesResponse.proto"
    redshift
    (Proxy :: Proxy DescribeHSMClientCertificates)

responseModifyClusterParameterGroup :: ClusterParameterGroupNameMessage -> TestTree
responseModifyClusterParameterGroup =
  res
    "ModifyClusterParameterGroupResponse"
    "fixture/ModifyClusterParameterGroupResponse.proto"
    redshift
    (Proxy :: Proxy ModifyClusterParameterGroup)

responseGetClusterCredentials :: GetClusterCredentialsResponse -> TestTree
responseGetClusterCredentials =
  res
    "GetClusterCredentialsResponse"
    "fixture/GetClusterCredentialsResponse.proto"
    redshift
    (Proxy :: Proxy GetClusterCredentials)

responseModifyClusterMaintenance :: ModifyClusterMaintenanceResponse -> TestTree
responseModifyClusterMaintenance =
  res
    "ModifyClusterMaintenanceResponse"
    "fixture/ModifyClusterMaintenanceResponse.proto"
    redshift
    (Proxy :: Proxy ModifyClusterMaintenance)

responseCreateClusterSecurityGroup :: CreateClusterSecurityGroupResponse -> TestTree
responseCreateClusterSecurityGroup =
  res
    "CreateClusterSecurityGroupResponse"
    "fixture/CreateClusterSecurityGroupResponse.proto"
    redshift
    (Proxy :: Proxy CreateClusterSecurityGroup)

responseDescribeEventCategories :: DescribeEventCategoriesResponse -> TestTree
responseDescribeEventCategories =
  res
    "DescribeEventCategoriesResponse"
    "fixture/DescribeEventCategoriesResponse.proto"
    redshift
    (Proxy :: Proxy DescribeEventCategories)

responseDescribeResize :: ResizeProgressMessage -> TestTree
responseDescribeResize =
  res
    "DescribeResizeResponse"
    "fixture/DescribeResizeResponse.proto"
    redshift
    (Proxy :: Proxy DescribeResize)

responseDeleteHSMConfiguration :: DeleteHSMConfigurationResponse -> TestTree
responseDeleteHSMConfiguration =
  res
    "DeleteHSMConfigurationResponse"
    "fixture/DeleteHSMConfigurationResponse.proto"
    redshift
    (Proxy :: Proxy DeleteHSMConfiguration)

responseAcceptReservedNodeExchange :: AcceptReservedNodeExchangeResponse -> TestTree
responseAcceptReservedNodeExchange =
  res
    "AcceptReservedNodeExchangeResponse"
    "fixture/AcceptReservedNodeExchangeResponse.proto"
    redshift
    (Proxy :: Proxy AcceptReservedNodeExchange)

responseAuthorizeClusterSecurityGroupIngress :: AuthorizeClusterSecurityGroupIngressResponse -> TestTree
responseAuthorizeClusterSecurityGroupIngress =
  res
    "AuthorizeClusterSecurityGroupIngressResponse"
    "fixture/AuthorizeClusterSecurityGroupIngressResponse.proto"
    redshift
    (Proxy :: Proxy AuthorizeClusterSecurityGroupIngress)

responseDescribeTableRestoreStatus :: DescribeTableRestoreStatusResponse -> TestTree
responseDescribeTableRestoreStatus =
  res
    "DescribeTableRestoreStatusResponse"
    "fixture/DescribeTableRestoreStatusResponse.proto"
    redshift
    (Proxy :: Proxy DescribeTableRestoreStatus)

responseCreateClusterSnapshot :: CreateClusterSnapshotResponse -> TestTree
responseCreateClusterSnapshot =
  res
    "CreateClusterSnapshotResponse"
    "fixture/CreateClusterSnapshotResponse.proto"
    redshift
    (Proxy :: Proxy CreateClusterSnapshot)

responseCreateHSMConfiguration :: CreateHSMConfigurationResponse -> TestTree
responseCreateHSMConfiguration =
  res
    "CreateHSMConfigurationResponse"
    "fixture/CreateHSMConfigurationResponse.proto"
    redshift
    (Proxy :: Proxy CreateHSMConfiguration)

responseDescribeLoggingStatus :: LoggingStatus -> TestTree
responseDescribeLoggingStatus =
  res
    "DescribeLoggingStatusResponse"
    "fixture/DescribeLoggingStatusResponse.proto"
    redshift
    (Proxy :: Proxy DescribeLoggingStatus)

responseModifyCluster :: ModifyClusterResponse -> TestTree
responseModifyCluster =
  res
    "ModifyClusterResponse"
    "fixture/ModifyClusterResponse.proto"
    redshift
    (Proxy :: Proxy ModifyCluster)

responseDeleteClusterSecurityGroup :: DeleteClusterSecurityGroupResponse -> TestTree
responseDeleteClusterSecurityGroup =
  res
    "DeleteClusterSecurityGroupResponse"
    "fixture/DeleteClusterSecurityGroupResponse.proto"
    redshift
    (Proxy :: Proxy DeleteClusterSecurityGroup)

responseCreateSnapshotSchedule :: SnapshotSchedule -> TestTree
responseCreateSnapshotSchedule =
  res
    "CreateSnapshotScheduleResponse"
    "fixture/CreateSnapshotScheduleResponse.proto"
    redshift
    (Proxy :: Proxy CreateSnapshotSchedule)

responseDescribeNodeConfigurationOptions :: DescribeNodeConfigurationOptionsResponse -> TestTree
responseDescribeNodeConfigurationOptions =
  res
    "DescribeNodeConfigurationOptionsResponse"
    "fixture/DescribeNodeConfigurationOptionsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeNodeConfigurationOptions)

responseDisableSnapshotCopy :: DisableSnapshotCopyResponse -> TestTree
responseDisableSnapshotCopy =
  res
    "DisableSnapshotCopyResponse"
    "fixture/DisableSnapshotCopyResponse.proto"
    redshift
    (Proxy :: Proxy DisableSnapshotCopy)

responseDescribeClusterParameters :: DescribeClusterParametersResponse -> TestTree
responseDescribeClusterParameters =
  res
    "DescribeClusterParametersResponse"
    "fixture/DescribeClusterParametersResponse.proto"
    redshift
    (Proxy :: Proxy DescribeClusterParameters)

responsePauseCluster :: PauseClusterResponse -> TestTree
responsePauseCluster =
  res
    "PauseClusterResponse"
    "fixture/PauseClusterResponse.proto"
    redshift
    (Proxy :: Proxy PauseCluster)

responseDeleteSnapshotSchedule :: DeleteSnapshotScheduleResponse -> TestTree
responseDeleteSnapshotSchedule =
  res
    "DeleteSnapshotScheduleResponse"
    "fixture/DeleteSnapshotScheduleResponse.proto"
    redshift
    (Proxy :: Proxy DeleteSnapshotSchedule)

responseRestoreFromClusterSnapshot :: RestoreFromClusterSnapshotResponse -> TestTree
responseRestoreFromClusterSnapshot =
  res
    "RestoreFromClusterSnapshotResponse"
    "fixture/RestoreFromClusterSnapshotResponse.proto"
    redshift
    (Proxy :: Proxy RestoreFromClusterSnapshot)

responseCreateClusterParameterGroup :: CreateClusterParameterGroupResponse -> TestTree
responseCreateClusterParameterGroup =
  res
    "CreateClusterParameterGroupResponse"
    "fixture/CreateClusterParameterGroupResponse.proto"
    redshift
    (Proxy :: Proxy CreateClusterParameterGroup)

responseRevokeSnapshotAccess :: RevokeSnapshotAccessResponse -> TestTree
responseRevokeSnapshotAccess =
  res
    "RevokeSnapshotAccessResponse"
    "fixture/RevokeSnapshotAccessResponse.proto"
    redshift
    (Proxy :: Proxy RevokeSnapshotAccess)

responseDescribeHSMConfigurations :: DescribeHSMConfigurationsResponse -> TestTree
responseDescribeHSMConfigurations =
  res
    "DescribeHSMConfigurationsResponse"
    "fixture/DescribeHSMConfigurationsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeHSMConfigurations)

responseDescribeAccountAttributes :: DescribeAccountAttributesResponse -> TestTree
responseDescribeAccountAttributes =
  res
    "DescribeAccountAttributesResponse"
    "fixture/DescribeAccountAttributesResponse.proto"
    redshift
    (Proxy :: Proxy DescribeAccountAttributes)

responseCreateSnapshotCopyGrant :: CreateSnapshotCopyGrantResponse -> TestTree
responseCreateSnapshotCopyGrant =
  res
    "CreateSnapshotCopyGrantResponse"
    "fixture/CreateSnapshotCopyGrantResponse.proto"
    redshift
    (Proxy :: Proxy CreateSnapshotCopyGrant)

responseCopyClusterSnapshot :: CopyClusterSnapshotResponse -> TestTree
responseCopyClusterSnapshot =
  res
    "CopyClusterSnapshotResponse"
    "fixture/CopyClusterSnapshotResponse.proto"
    redshift
    (Proxy :: Proxy CopyClusterSnapshot)

responseDeleteHSMClientCertificate :: DeleteHSMClientCertificateResponse -> TestTree
responseDeleteHSMClientCertificate =
  res
    "DeleteHSMClientCertificateResponse"
    "fixture/DeleteHSMClientCertificateResponse.proto"
    redshift
    (Proxy :: Proxy DeleteHSMClientCertificate)

responseModifyClusterSnapshotSchedule :: ModifyClusterSnapshotScheduleResponse -> TestTree
responseModifyClusterSnapshotSchedule =
  res
    "ModifyClusterSnapshotScheduleResponse"
    "fixture/ModifyClusterSnapshotScheduleResponse.proto"
    redshift
    (Proxy :: Proxy ModifyClusterSnapshotSchedule)

responseDeleteSnapshotCopyGrant :: DeleteSnapshotCopyGrantResponse -> TestTree
responseDeleteSnapshotCopyGrant =
  res
    "DeleteSnapshotCopyGrantResponse"
    "fixture/DeleteSnapshotCopyGrantResponse.proto"
    redshift
    (Proxy :: Proxy DeleteSnapshotCopyGrant)

responseDescribeClusterVersions :: DescribeClusterVersionsResponse -> TestTree
responseDescribeClusterVersions =
  res
    "DescribeClusterVersionsResponse"
    "fixture/DescribeClusterVersionsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeClusterVersions)

responseModifyClusterSubnetGroup :: ModifyClusterSubnetGroupResponse -> TestTree
responseModifyClusterSubnetGroup =
  res
    "ModifyClusterSubnetGroupResponse"
    "fixture/ModifyClusterSubnetGroupResponse.proto"
    redshift
    (Proxy :: Proxy ModifyClusterSubnetGroup)

responseDescribeUsageLimits :: DescribeUsageLimitsResponse -> TestTree
responseDescribeUsageLimits =
  res
    "DescribeUsageLimitsResponse"
    "fixture/DescribeUsageLimitsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeUsageLimits)

responseModifySnapshotSchedule :: SnapshotSchedule -> TestTree
responseModifySnapshotSchedule =
  res
    "ModifySnapshotScheduleResponse"
    "fixture/ModifySnapshotScheduleResponse.proto"
    redshift
    (Proxy :: Proxy ModifySnapshotSchedule)

responseRotateEncryptionKey :: RotateEncryptionKeyResponse -> TestTree
responseRotateEncryptionKey =
  res
    "RotateEncryptionKeyResponse"
    "fixture/RotateEncryptionKeyResponse.proto"
    redshift
    (Proxy :: Proxy RotateEncryptionKey)

responseDescribeSnapshotCopyGrants :: DescribeSnapshotCopyGrantsResponse -> TestTree
responseDescribeSnapshotCopyGrants =
  res
    "DescribeSnapshotCopyGrantsResponse"
    "fixture/DescribeSnapshotCopyGrantsResponse.proto"
    redshift
    (Proxy :: Proxy DescribeSnapshotCopyGrants)
