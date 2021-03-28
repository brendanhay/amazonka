{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Redshift
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
--         [ requestCancelResize $
--             mkCancelResize
--
--         , requestDescribeStorage $
--             mkDescribeStorage
--
--         , requestDescribeClusters $
--             mkDescribeClusters
--
--         , requestDescribeTags $
--             mkDescribeTags
--
--         , requestCreateUsageLimit $
--             mkCreateUsageLimit
--
--         , requestDeleteClusterSubnetGroup $
--             mkDeleteClusterSubnetGroup
--
--         , requestModifyScheduledAction $
--             mkModifyScheduledAction
--
--         , requestDisableLogging $
--             mkDisableLogging
--
--         , requestDescribeSnapshotSchedules $
--             mkDescribeSnapshotSchedules
--
--         , requestModifyEventSubscription $
--             mkModifyEventSubscription
--
--         , requestModifyClusterDbRevision $
--             mkModifyClusterDbRevision
--
--         , requestDeleteClusterSnapshot $
--             mkDeleteClusterSnapshot
--
--         , requestPurchaseReservedNodeOffering $
--             mkPurchaseReservedNodeOffering
--
--         , requestDescribeReservedNodeOfferings $
--             mkDescribeReservedNodeOfferings
--
--         , requestDescribeEvents $
--             mkDescribeEvents
--
--         , requestDescribeReservedNodes $
--             mkDescribeReservedNodes
--
--         , requestGetReservedNodeExchangeOfferings $
--             mkGetReservedNodeExchangeOfferings
--
--         , requestDescribeClusterParameterGroups $
--             mkDescribeClusterParameterGroups
--
--         , requestEnableLogging $
--             mkEnableLogging
--
--         , requestCreateClusterSubnetGroup $
--             mkCreateClusterSubnetGroup
--
--         , requestDeleteClusterParameterGroup $
--             mkDeleteClusterParameterGroup
--
--         , requestDescribeClusterSecurityGroups $
--             mkDescribeClusterSecurityGroups
--
--         , requestCreateTags $
--             mkCreateTags
--
--         , requestEnableSnapshotCopy $
--             mkEnableSnapshotCopy
--
--         , requestDescribeClusterSnapshots $
--             mkDescribeClusterSnapshots
--
--         , requestBatchDeleteClusterSnapshots $
--             mkBatchDeleteClusterSnapshots
--
--         , requestDeleteTags $
--             mkDeleteTags
--
--         , requestModifyUsageLimit $
--             mkModifyUsageLimit
--
--         , requestDescribeClusterSubnetGroups $
--             mkDescribeClusterSubnetGroups
--
--         , requestResizeCluster $
--             mkResizeCluster
--
--         , requestModifySnapshotCopyRetentionPeriod $
--             mkModifySnapshotCopyRetentionPeriod
--
--         , requestModifyClusterIamRoles $
--             mkModifyClusterIamRoles
--
--         , requestAuthorizeSnapshotAccess $
--             mkAuthorizeSnapshotAccess
--
--         , requestRebootCluster $
--             mkRebootCluster
--
--         , requestResumeCluster $
--             mkResumeCluster
--
--         , requestDeleteCluster $
--             mkDeleteCluster
--
--         , requestCreateEventSubscription $
--             mkCreateEventSubscription
--
--         , requestCreateScheduledAction $
--             mkCreateScheduledAction
--
--         , requestDescribeOrderableClusterOptions $
--             mkDescribeOrderableClusterOptions
--
--         , requestDescribeClusterTracks $
--             mkDescribeClusterTracks
--
--         , requestCreateCluster $
--             mkCreateCluster
--
--         , requestCreateHsmClientCertificate $
--             mkCreateHsmClientCertificate
--
--         , requestRestoreTableFromClusterSnapshot $
--             mkRestoreTableFromClusterSnapshot
--
--         , requestDeleteScheduledAction $
--             mkDeleteScheduledAction
--
--         , requestDescribeDefaultClusterParameters $
--             mkDescribeDefaultClusterParameters
--
--         , requestDeleteEventSubscription $
--             mkDeleteEventSubscription
--
--         , requestModifyClusterSnapshot $
--             mkModifyClusterSnapshot
--
--         , requestResetClusterParameterGroup $
--             mkResetClusterParameterGroup
--
--         , requestDescribeScheduledActions $
--             mkDescribeScheduledActions
--
--         , requestDescribeEventSubscriptions $
--             mkDescribeEventSubscriptions
--
--         , requestDescribeClusterDbRevisions $
--             mkDescribeClusterDbRevisions
--
--         , requestBatchModifyClusterSnapshots $
--             mkBatchModifyClusterSnapshots
--
--         , requestDeleteUsageLimit $
--             mkDeleteUsageLimit
--
--         , requestRevokeClusterSecurityGroupIngress $
--             mkRevokeClusterSecurityGroupIngress
--
--         , requestDescribeHsmClientCertificates $
--             mkDescribeHsmClientCertificates
--
--         , requestModifyClusterParameterGroup $
--             mkModifyClusterParameterGroup
--
--         , requestGetClusterCredentials $
--             mkGetClusterCredentials
--
--         , requestModifyClusterMaintenance $
--             mkModifyClusterMaintenance
--
--         , requestCreateClusterSecurityGroup $
--             mkCreateClusterSecurityGroup
--
--         , requestDescribeEventCategories $
--             mkDescribeEventCategories
--
--         , requestDescribeResize $
--             mkDescribeResize
--
--         , requestDeleteHsmConfiguration $
--             mkDeleteHsmConfiguration
--
--         , requestAcceptReservedNodeExchange $
--             mkAcceptReservedNodeExchange
--
--         , requestAuthorizeClusterSecurityGroupIngress $
--             mkAuthorizeClusterSecurityGroupIngress
--
--         , requestDescribeTableRestoreStatus $
--             mkDescribeTableRestoreStatus
--
--         , requestCreateClusterSnapshot $
--             mkCreateClusterSnapshot
--
--         , requestCreateHsmConfiguration $
--             mkCreateHsmConfiguration
--
--         , requestDescribeLoggingStatus $
--             mkDescribeLoggingStatus
--
--         , requestModifyCluster $
--             mkModifyCluster
--
--         , requestDeleteClusterSecurityGroup $
--             mkDeleteClusterSecurityGroup
--
--         , requestCreateSnapshotSchedule $
--             mkCreateSnapshotSchedule
--
--         , requestDescribeNodeConfigurationOptions $
--             mkDescribeNodeConfigurationOptions
--
--         , requestDisableSnapshotCopy $
--             mkDisableSnapshotCopy
--
--         , requestDescribeClusterParameters $
--             mkDescribeClusterParameters
--
--         , requestPauseCluster $
--             mkPauseCluster
--
--         , requestDeleteSnapshotSchedule $
--             mkDeleteSnapshotSchedule
--
--         , requestRestoreFromClusterSnapshot $
--             mkRestoreFromClusterSnapshot
--
--         , requestCreateClusterParameterGroup $
--             mkCreateClusterParameterGroup
--
--         , requestRevokeSnapshotAccess $
--             mkRevokeSnapshotAccess
--
--         , requestDescribeHsmConfigurations $
--             mkDescribeHsmConfigurations
--
--         , requestDescribeAccountAttributes $
--             mkDescribeAccountAttributes
--
--         , requestCreateSnapshotCopyGrant $
--             mkCreateSnapshotCopyGrant
--
--         , requestCopyClusterSnapshot $
--             mkCopyClusterSnapshot
--
--         , requestDeleteHsmClientCertificate $
--             mkDeleteHsmClientCertificate
--
--         , requestModifyClusterSnapshotSchedule $
--             mkModifyClusterSnapshotSchedule
--
--         , requestDeleteSnapshotCopyGrant $
--             mkDeleteSnapshotCopyGrant
--
--         , requestDescribeClusterVersions $
--             mkDescribeClusterVersions
--
--         , requestModifyClusterSubnetGroup $
--             mkModifyClusterSubnetGroup
--
--         , requestDescribeUsageLimits $
--             mkDescribeUsageLimits
--
--         , requestModifySnapshotSchedule $
--             mkModifySnapshotSchedule
--
--         , requestRotateEncryptionKey $
--             mkRotateEncryptionKey
--
--         , requestDescribeSnapshotCopyGrants $
--             mkDescribeSnapshotCopyGrants
--
--           ]

--     , testGroup "response"
--         [ responseCancelResize $
--             mkResizeProgressMessage
--
--         , responseDescribeStorage $
--             mkDescribeStorageResponse
--
--         , responseDescribeClusters $
--             mkDescribeClustersResponse
--
--         , responseDescribeTags $
--             mkDescribeTagsResponse
--
--         , responseCreateUsageLimit $
--             mkUsageLimit
--
--         , responseDeleteClusterSubnetGroup $
--             mkDeleteClusterSubnetGroupResponse
--
--         , responseModifyScheduledAction $
--             mkScheduledAction
--
--         , responseDisableLogging $
--             mkLoggingStatus
--
--         , responseDescribeSnapshotSchedules $
--             mkDescribeSnapshotSchedulesResponse
--
--         , responseModifyEventSubscription $
--             mkModifyEventSubscriptionResponse
--
--         , responseModifyClusterDbRevision $
--             mkModifyClusterDbRevisionResponse
--
--         , responseDeleteClusterSnapshot $
--             mkDeleteClusterSnapshotResponse
--
--         , responsePurchaseReservedNodeOffering $
--             mkPurchaseReservedNodeOfferingResponse
--
--         , responseDescribeReservedNodeOfferings $
--             mkDescribeReservedNodeOfferingsResponse
--
--         , responseDescribeEvents $
--             mkDescribeEventsResponse
--
--         , responseDescribeReservedNodes $
--             mkDescribeReservedNodesResponse
--
--         , responseGetReservedNodeExchangeOfferings $
--             mkGetReservedNodeExchangeOfferingsResponse
--
--         , responseDescribeClusterParameterGroups $
--             mkDescribeClusterParameterGroupsResponse
--
--         , responseEnableLogging $
--             mkLoggingStatus
--
--         , responseCreateClusterSubnetGroup $
--             mkCreateClusterSubnetGroupResponse
--
--         , responseDeleteClusterParameterGroup $
--             mkDeleteClusterParameterGroupResponse
--
--         , responseDescribeClusterSecurityGroups $
--             mkDescribeClusterSecurityGroupsResponse
--
--         , responseCreateTags $
--             mkCreateTagsResponse
--
--         , responseEnableSnapshotCopy $
--             mkEnableSnapshotCopyResponse
--
--         , responseDescribeClusterSnapshots $
--             mkDescribeClusterSnapshotsResponse
--
--         , responseBatchDeleteClusterSnapshots $
--             mkBatchDeleteClusterSnapshotsResponse
--
--         , responseDeleteTags $
--             mkDeleteTagsResponse
--
--         , responseModifyUsageLimit $
--             mkUsageLimit
--
--         , responseDescribeClusterSubnetGroups $
--             mkDescribeClusterSubnetGroupsResponse
--
--         , responseResizeCluster $
--             mkResizeClusterResponse
--
--         , responseModifySnapshotCopyRetentionPeriod $
--             mkModifySnapshotCopyRetentionPeriodResponse
--
--         , responseModifyClusterIamRoles $
--             mkModifyClusterIamRolesResponse
--
--         , responseAuthorizeSnapshotAccess $
--             mkAuthorizeSnapshotAccessResponse
--
--         , responseRebootCluster $
--             mkRebootClusterResponse
--
--         , responseResumeCluster $
--             mkResumeClusterResponse
--
--         , responseDeleteCluster $
--             mkDeleteClusterResponse
--
--         , responseCreateEventSubscription $
--             mkCreateEventSubscriptionResponse
--
--         , responseCreateScheduledAction $
--             mkScheduledAction
--
--         , responseDescribeOrderableClusterOptions $
--             mkDescribeOrderableClusterOptionsResponse
--
--         , responseDescribeClusterTracks $
--             mkDescribeClusterTracksResponse
--
--         , responseCreateCluster $
--             mkCreateClusterResponse
--
--         , responseCreateHsmClientCertificate $
--             mkCreateHsmClientCertificateResponse
--
--         , responseRestoreTableFromClusterSnapshot $
--             mkRestoreTableFromClusterSnapshotResponse
--
--         , responseDeleteScheduledAction $
--             mkDeleteScheduledActionResponse
--
--         , responseDescribeDefaultClusterParameters $
--             mkDescribeDefaultClusterParametersResponse
--
--         , responseDeleteEventSubscription $
--             mkDeleteEventSubscriptionResponse
--
--         , responseModifyClusterSnapshot $
--             mkModifyClusterSnapshotResponse
--
--         , responseResetClusterParameterGroup $
--             mkClusterParameterGroupNameMessage
--
--         , responseDescribeScheduledActions $
--             mkDescribeScheduledActionsResponse
--
--         , responseDescribeEventSubscriptions $
--             mkDescribeEventSubscriptionsResponse
--
--         , responseDescribeClusterDbRevisions $
--             mkDescribeClusterDbRevisionsResponse
--
--         , responseBatchModifyClusterSnapshots $
--             mkBatchModifyClusterSnapshotsResponse
--
--         , responseDeleteUsageLimit $
--             mkDeleteUsageLimitResponse
--
--         , responseRevokeClusterSecurityGroupIngress $
--             mkRevokeClusterSecurityGroupIngressResponse
--
--         , responseDescribeHsmClientCertificates $
--             mkDescribeHsmClientCertificatesResponse
--
--         , responseModifyClusterParameterGroup $
--             mkClusterParameterGroupNameMessage
--
--         , responseGetClusterCredentials $
--             mkGetClusterCredentialsResponse
--
--         , responseModifyClusterMaintenance $
--             mkModifyClusterMaintenanceResponse
--
--         , responseCreateClusterSecurityGroup $
--             mkCreateClusterSecurityGroupResponse
--
--         , responseDescribeEventCategories $
--             mkDescribeEventCategoriesResponse
--
--         , responseDescribeResize $
--             mkResizeProgressMessage
--
--         , responseDeleteHsmConfiguration $
--             mkDeleteHsmConfigurationResponse
--
--         , responseAcceptReservedNodeExchange $
--             mkAcceptReservedNodeExchangeResponse
--
--         , responseAuthorizeClusterSecurityGroupIngress $
--             mkAuthorizeClusterSecurityGroupIngressResponse
--
--         , responseDescribeTableRestoreStatus $
--             mkDescribeTableRestoreStatusResponse
--
--         , responseCreateClusterSnapshot $
--             mkCreateClusterSnapshotResponse
--
--         , responseCreateHsmConfiguration $
--             mkCreateHsmConfigurationResponse
--
--         , responseDescribeLoggingStatus $
--             mkLoggingStatus
--
--         , responseModifyCluster $
--             mkModifyClusterResponse
--
--         , responseDeleteClusterSecurityGroup $
--             mkDeleteClusterSecurityGroupResponse
--
--         , responseCreateSnapshotSchedule $
--             mkSnapshotSchedule
--
--         , responseDescribeNodeConfigurationOptions $
--             mkDescribeNodeConfigurationOptionsResponse
--
--         , responseDisableSnapshotCopy $
--             mkDisableSnapshotCopyResponse
--
--         , responseDescribeClusterParameters $
--             mkDescribeClusterParametersResponse
--
--         , responsePauseCluster $
--             mkPauseClusterResponse
--
--         , responseDeleteSnapshotSchedule $
--             mkDeleteSnapshotScheduleResponse
--
--         , responseRestoreFromClusterSnapshot $
--             mkRestoreFromClusterSnapshotResponse
--
--         , responseCreateClusterParameterGroup $
--             mkCreateClusterParameterGroupResponse
--
--         , responseRevokeSnapshotAccess $
--             mkRevokeSnapshotAccessResponse
--
--         , responseDescribeHsmConfigurations $
--             mkDescribeHsmConfigurationsResponse
--
--         , responseDescribeAccountAttributes $
--             mkDescribeAccountAttributesResponse
--
--         , responseCreateSnapshotCopyGrant $
--             mkCreateSnapshotCopyGrantResponse
--
--         , responseCopyClusterSnapshot $
--             mkCopyClusterSnapshotResponse
--
--         , responseDeleteHsmClientCertificate $
--             mkDeleteHsmClientCertificateResponse
--
--         , responseModifyClusterSnapshotSchedule $
--             mkModifyClusterSnapshotScheduleResponse
--
--         , responseDeleteSnapshotCopyGrant $
--             mkDeleteSnapshotCopyGrantResponse
--
--         , responseDescribeClusterVersions $
--             mkDescribeClusterVersionsResponse
--
--         , responseModifyClusterSubnetGroup $
--             mkModifyClusterSubnetGroupResponse
--
--         , responseDescribeUsageLimits $
--             mkDescribeUsageLimitsResponse
--
--         , responseModifySnapshotSchedule $
--             mkSnapshotSchedule
--
--         , responseRotateEncryptionKey $
--             mkRotateEncryptionKeyResponse
--
--         , responseDescribeSnapshotCopyGrants $
--             mkDescribeSnapshotCopyGrantsResponse
--
--           ]
--     ]

-- Requests

requestCancelResize :: CancelResize -> TestTree
requestCancelResize = req
    "CancelResize"
    "fixture/CancelResize.yaml"

requestDescribeStorage :: DescribeStorage -> TestTree
requestDescribeStorage = req
    "DescribeStorage"
    "fixture/DescribeStorage.yaml"

requestDescribeClusters :: DescribeClusters -> TestTree
requestDescribeClusters = req
    "DescribeClusters"
    "fixture/DescribeClusters.yaml"

requestDescribeTags :: DescribeTags -> TestTree
requestDescribeTags = req
    "DescribeTags"
    "fixture/DescribeTags.yaml"

requestCreateUsageLimit :: CreateUsageLimit -> TestTree
requestCreateUsageLimit = req
    "CreateUsageLimit"
    "fixture/CreateUsageLimit.yaml"

requestDeleteClusterSubnetGroup :: DeleteClusterSubnetGroup -> TestTree
requestDeleteClusterSubnetGroup = req
    "DeleteClusterSubnetGroup"
    "fixture/DeleteClusterSubnetGroup.yaml"

requestModifyScheduledAction :: ModifyScheduledAction -> TestTree
requestModifyScheduledAction = req
    "ModifyScheduledAction"
    "fixture/ModifyScheduledAction.yaml"

requestDisableLogging :: DisableLogging -> TestTree
requestDisableLogging = req
    "DisableLogging"
    "fixture/DisableLogging.yaml"

requestDescribeSnapshotSchedules :: DescribeSnapshotSchedules -> TestTree
requestDescribeSnapshotSchedules = req
    "DescribeSnapshotSchedules"
    "fixture/DescribeSnapshotSchedules.yaml"

requestModifyEventSubscription :: ModifyEventSubscription -> TestTree
requestModifyEventSubscription = req
    "ModifyEventSubscription"
    "fixture/ModifyEventSubscription.yaml"

requestModifyClusterDbRevision :: ModifyClusterDbRevision -> TestTree
requestModifyClusterDbRevision = req
    "ModifyClusterDbRevision"
    "fixture/ModifyClusterDbRevision.yaml"

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

requestGetReservedNodeExchangeOfferings :: GetReservedNodeExchangeOfferings -> TestTree
requestGetReservedNodeExchangeOfferings = req
    "GetReservedNodeExchangeOfferings"
    "fixture/GetReservedNodeExchangeOfferings.yaml"

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

requestBatchDeleteClusterSnapshots :: BatchDeleteClusterSnapshots -> TestTree
requestBatchDeleteClusterSnapshots = req
    "BatchDeleteClusterSnapshots"
    "fixture/BatchDeleteClusterSnapshots.yaml"

requestDeleteTags :: DeleteTags -> TestTree
requestDeleteTags = req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

requestModifyUsageLimit :: ModifyUsageLimit -> TestTree
requestModifyUsageLimit = req
    "ModifyUsageLimit"
    "fixture/ModifyUsageLimit.yaml"

requestDescribeClusterSubnetGroups :: DescribeClusterSubnetGroups -> TestTree
requestDescribeClusterSubnetGroups = req
    "DescribeClusterSubnetGroups"
    "fixture/DescribeClusterSubnetGroups.yaml"

requestResizeCluster :: ResizeCluster -> TestTree
requestResizeCluster = req
    "ResizeCluster"
    "fixture/ResizeCluster.yaml"

requestModifySnapshotCopyRetentionPeriod :: ModifySnapshotCopyRetentionPeriod -> TestTree
requestModifySnapshotCopyRetentionPeriod = req
    "ModifySnapshotCopyRetentionPeriod"
    "fixture/ModifySnapshotCopyRetentionPeriod.yaml"

requestModifyClusterIamRoles :: ModifyClusterIamRoles -> TestTree
requestModifyClusterIamRoles = req
    "ModifyClusterIamRoles"
    "fixture/ModifyClusterIamRoles.yaml"

requestAuthorizeSnapshotAccess :: AuthorizeSnapshotAccess -> TestTree
requestAuthorizeSnapshotAccess = req
    "AuthorizeSnapshotAccess"
    "fixture/AuthorizeSnapshotAccess.yaml"

requestRebootCluster :: RebootCluster -> TestTree
requestRebootCluster = req
    "RebootCluster"
    "fixture/RebootCluster.yaml"

requestResumeCluster :: ResumeCluster -> TestTree
requestResumeCluster = req
    "ResumeCluster"
    "fixture/ResumeCluster.yaml"

requestDeleteCluster :: DeleteCluster -> TestTree
requestDeleteCluster = req
    "DeleteCluster"
    "fixture/DeleteCluster.yaml"

requestCreateEventSubscription :: CreateEventSubscription -> TestTree
requestCreateEventSubscription = req
    "CreateEventSubscription"
    "fixture/CreateEventSubscription.yaml"

requestCreateScheduledAction :: CreateScheduledAction -> TestTree
requestCreateScheduledAction = req
    "CreateScheduledAction"
    "fixture/CreateScheduledAction.yaml"

requestDescribeOrderableClusterOptions :: DescribeOrderableClusterOptions -> TestTree
requestDescribeOrderableClusterOptions = req
    "DescribeOrderableClusterOptions"
    "fixture/DescribeOrderableClusterOptions.yaml"

requestDescribeClusterTracks :: DescribeClusterTracks -> TestTree
requestDescribeClusterTracks = req
    "DescribeClusterTracks"
    "fixture/DescribeClusterTracks.yaml"

requestCreateCluster :: CreateCluster -> TestTree
requestCreateCluster = req
    "CreateCluster"
    "fixture/CreateCluster.yaml"

requestCreateHsmClientCertificate :: CreateHsmClientCertificate -> TestTree
requestCreateHsmClientCertificate = req
    "CreateHsmClientCertificate"
    "fixture/CreateHsmClientCertificate.yaml"

requestRestoreTableFromClusterSnapshot :: RestoreTableFromClusterSnapshot -> TestTree
requestRestoreTableFromClusterSnapshot = req
    "RestoreTableFromClusterSnapshot"
    "fixture/RestoreTableFromClusterSnapshot.yaml"

requestDeleteScheduledAction :: DeleteScheduledAction -> TestTree
requestDeleteScheduledAction = req
    "DeleteScheduledAction"
    "fixture/DeleteScheduledAction.yaml"

requestDescribeDefaultClusterParameters :: DescribeDefaultClusterParameters -> TestTree
requestDescribeDefaultClusterParameters = req
    "DescribeDefaultClusterParameters"
    "fixture/DescribeDefaultClusterParameters.yaml"

requestDeleteEventSubscription :: DeleteEventSubscription -> TestTree
requestDeleteEventSubscription = req
    "DeleteEventSubscription"
    "fixture/DeleteEventSubscription.yaml"

requestModifyClusterSnapshot :: ModifyClusterSnapshot -> TestTree
requestModifyClusterSnapshot = req
    "ModifyClusterSnapshot"
    "fixture/ModifyClusterSnapshot.yaml"

requestResetClusterParameterGroup :: ResetClusterParameterGroup -> TestTree
requestResetClusterParameterGroup = req
    "ResetClusterParameterGroup"
    "fixture/ResetClusterParameterGroup.yaml"

requestDescribeScheduledActions :: DescribeScheduledActions -> TestTree
requestDescribeScheduledActions = req
    "DescribeScheduledActions"
    "fixture/DescribeScheduledActions.yaml"

requestDescribeEventSubscriptions :: DescribeEventSubscriptions -> TestTree
requestDescribeEventSubscriptions = req
    "DescribeEventSubscriptions"
    "fixture/DescribeEventSubscriptions.yaml"

requestDescribeClusterDbRevisions :: DescribeClusterDbRevisions -> TestTree
requestDescribeClusterDbRevisions = req
    "DescribeClusterDbRevisions"
    "fixture/DescribeClusterDbRevisions.yaml"

requestBatchModifyClusterSnapshots :: BatchModifyClusterSnapshots -> TestTree
requestBatchModifyClusterSnapshots = req
    "BatchModifyClusterSnapshots"
    "fixture/BatchModifyClusterSnapshots.yaml"

requestDeleteUsageLimit :: DeleteUsageLimit -> TestTree
requestDeleteUsageLimit = req
    "DeleteUsageLimit"
    "fixture/DeleteUsageLimit.yaml"

requestRevokeClusterSecurityGroupIngress :: RevokeClusterSecurityGroupIngress -> TestTree
requestRevokeClusterSecurityGroupIngress = req
    "RevokeClusterSecurityGroupIngress"
    "fixture/RevokeClusterSecurityGroupIngress.yaml"

requestDescribeHsmClientCertificates :: DescribeHsmClientCertificates -> TestTree
requestDescribeHsmClientCertificates = req
    "DescribeHsmClientCertificates"
    "fixture/DescribeHsmClientCertificates.yaml"

requestModifyClusterParameterGroup :: ModifyClusterParameterGroup -> TestTree
requestModifyClusterParameterGroup = req
    "ModifyClusterParameterGroup"
    "fixture/ModifyClusterParameterGroup.yaml"

requestGetClusterCredentials :: GetClusterCredentials -> TestTree
requestGetClusterCredentials = req
    "GetClusterCredentials"
    "fixture/GetClusterCredentials.yaml"

requestModifyClusterMaintenance :: ModifyClusterMaintenance -> TestTree
requestModifyClusterMaintenance = req
    "ModifyClusterMaintenance"
    "fixture/ModifyClusterMaintenance.yaml"

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

requestDeleteHsmConfiguration :: DeleteHsmConfiguration -> TestTree
requestDeleteHsmConfiguration = req
    "DeleteHsmConfiguration"
    "fixture/DeleteHsmConfiguration.yaml"

requestAcceptReservedNodeExchange :: AcceptReservedNodeExchange -> TestTree
requestAcceptReservedNodeExchange = req
    "AcceptReservedNodeExchange"
    "fixture/AcceptReservedNodeExchange.yaml"

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

requestCreateHsmConfiguration :: CreateHsmConfiguration -> TestTree
requestCreateHsmConfiguration = req
    "CreateHsmConfiguration"
    "fixture/CreateHsmConfiguration.yaml"

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

requestCreateSnapshotSchedule :: CreateSnapshotSchedule -> TestTree
requestCreateSnapshotSchedule = req
    "CreateSnapshotSchedule"
    "fixture/CreateSnapshotSchedule.yaml"

requestDescribeNodeConfigurationOptions :: DescribeNodeConfigurationOptions -> TestTree
requestDescribeNodeConfigurationOptions = req
    "DescribeNodeConfigurationOptions"
    "fixture/DescribeNodeConfigurationOptions.yaml"

requestDisableSnapshotCopy :: DisableSnapshotCopy -> TestTree
requestDisableSnapshotCopy = req
    "DisableSnapshotCopy"
    "fixture/DisableSnapshotCopy.yaml"

requestDescribeClusterParameters :: DescribeClusterParameters -> TestTree
requestDescribeClusterParameters = req
    "DescribeClusterParameters"
    "fixture/DescribeClusterParameters.yaml"

requestPauseCluster :: PauseCluster -> TestTree
requestPauseCluster = req
    "PauseCluster"
    "fixture/PauseCluster.yaml"

requestDeleteSnapshotSchedule :: DeleteSnapshotSchedule -> TestTree
requestDeleteSnapshotSchedule = req
    "DeleteSnapshotSchedule"
    "fixture/DeleteSnapshotSchedule.yaml"

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

requestDescribeHsmConfigurations :: DescribeHsmConfigurations -> TestTree
requestDescribeHsmConfigurations = req
    "DescribeHsmConfigurations"
    "fixture/DescribeHsmConfigurations.yaml"

requestDescribeAccountAttributes :: DescribeAccountAttributes -> TestTree
requestDescribeAccountAttributes = req
    "DescribeAccountAttributes"
    "fixture/DescribeAccountAttributes.yaml"

requestCreateSnapshotCopyGrant :: CreateSnapshotCopyGrant -> TestTree
requestCreateSnapshotCopyGrant = req
    "CreateSnapshotCopyGrant"
    "fixture/CreateSnapshotCopyGrant.yaml"

requestCopyClusterSnapshot :: CopyClusterSnapshot -> TestTree
requestCopyClusterSnapshot = req
    "CopyClusterSnapshot"
    "fixture/CopyClusterSnapshot.yaml"

requestDeleteHsmClientCertificate :: DeleteHsmClientCertificate -> TestTree
requestDeleteHsmClientCertificate = req
    "DeleteHsmClientCertificate"
    "fixture/DeleteHsmClientCertificate.yaml"

requestModifyClusterSnapshotSchedule :: ModifyClusterSnapshotSchedule -> TestTree
requestModifyClusterSnapshotSchedule = req
    "ModifyClusterSnapshotSchedule"
    "fixture/ModifyClusterSnapshotSchedule.yaml"

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

requestDescribeUsageLimits :: DescribeUsageLimits -> TestTree
requestDescribeUsageLimits = req
    "DescribeUsageLimits"
    "fixture/DescribeUsageLimits.yaml"

requestModifySnapshotSchedule :: ModifySnapshotSchedule -> TestTree
requestModifySnapshotSchedule = req
    "ModifySnapshotSchedule"
    "fixture/ModifySnapshotSchedule.yaml"

requestRotateEncryptionKey :: RotateEncryptionKey -> TestTree
requestRotateEncryptionKey = req
    "RotateEncryptionKey"
    "fixture/RotateEncryptionKey.yaml"

requestDescribeSnapshotCopyGrants :: DescribeSnapshotCopyGrants -> TestTree
requestDescribeSnapshotCopyGrants = req
    "DescribeSnapshotCopyGrants"
    "fixture/DescribeSnapshotCopyGrants.yaml"

-- Responses

responseCancelResize :: ResizeProgressMessage -> TestTree
responseCancelResize = res
    "CancelResizeResponse"
    "fixture/CancelResizeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CancelResize)

responseDescribeStorage :: DescribeStorageResponse -> TestTree
responseDescribeStorage = res
    "DescribeStorageResponse"
    "fixture/DescribeStorageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeStorage)

responseDescribeClusters :: DescribeClustersResponse -> TestTree
responseDescribeClusters = res
    "DescribeClustersResponse"
    "fixture/DescribeClustersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeClusters)

responseDescribeTags :: DescribeTagsResponse -> TestTree
responseDescribeTags = res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeTags)

responseCreateUsageLimit :: UsageLimit -> TestTree
responseCreateUsageLimit = res
    "CreateUsageLimitResponse"
    "fixture/CreateUsageLimitResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateUsageLimit)

responseDeleteClusterSubnetGroup :: DeleteClusterSubnetGroupResponse -> TestTree
responseDeleteClusterSubnetGroup = res
    "DeleteClusterSubnetGroupResponse"
    "fixture/DeleteClusterSubnetGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteClusterSubnetGroup)

responseModifyScheduledAction :: ScheduledAction -> TestTree
responseModifyScheduledAction = res
    "ModifyScheduledActionResponse"
    "fixture/ModifyScheduledActionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyScheduledAction)

responseDisableLogging :: LoggingStatus -> TestTree
responseDisableLogging = res
    "DisableLoggingResponse"
    "fixture/DisableLoggingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisableLogging)

responseDescribeSnapshotSchedules :: DescribeSnapshotSchedulesResponse -> TestTree
responseDescribeSnapshotSchedules = res
    "DescribeSnapshotSchedulesResponse"
    "fixture/DescribeSnapshotSchedulesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeSnapshotSchedules)

responseModifyEventSubscription :: ModifyEventSubscriptionResponse -> TestTree
responseModifyEventSubscription = res
    "ModifyEventSubscriptionResponse"
    "fixture/ModifyEventSubscriptionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyEventSubscription)

responseModifyClusterDbRevision :: ModifyClusterDbRevisionResponse -> TestTree
responseModifyClusterDbRevision = res
    "ModifyClusterDbRevisionResponse"
    "fixture/ModifyClusterDbRevisionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyClusterDbRevision)

responseDeleteClusterSnapshot :: DeleteClusterSnapshotResponse -> TestTree
responseDeleteClusterSnapshot = res
    "DeleteClusterSnapshotResponse"
    "fixture/DeleteClusterSnapshotResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteClusterSnapshot)

responsePurchaseReservedNodeOffering :: PurchaseReservedNodeOfferingResponse -> TestTree
responsePurchaseReservedNodeOffering = res
    "PurchaseReservedNodeOfferingResponse"
    "fixture/PurchaseReservedNodeOfferingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PurchaseReservedNodeOffering)

responseDescribeReservedNodeOfferings :: DescribeReservedNodeOfferingsResponse -> TestTree
responseDescribeReservedNodeOfferings = res
    "DescribeReservedNodeOfferingsResponse"
    "fixture/DescribeReservedNodeOfferingsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeReservedNodeOfferings)

responseDescribeEvents :: DescribeEventsResponse -> TestTree
responseDescribeEvents = res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeEvents)

responseDescribeReservedNodes :: DescribeReservedNodesResponse -> TestTree
responseDescribeReservedNodes = res
    "DescribeReservedNodesResponse"
    "fixture/DescribeReservedNodesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeReservedNodes)

responseGetReservedNodeExchangeOfferings :: GetReservedNodeExchangeOfferingsResponse -> TestTree
responseGetReservedNodeExchangeOfferings = res
    "GetReservedNodeExchangeOfferingsResponse"
    "fixture/GetReservedNodeExchangeOfferingsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetReservedNodeExchangeOfferings)

responseDescribeClusterParameterGroups :: DescribeClusterParameterGroupsResponse -> TestTree
responseDescribeClusterParameterGroups = res
    "DescribeClusterParameterGroupsResponse"
    "fixture/DescribeClusterParameterGroupsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeClusterParameterGroups)

responseEnableLogging :: LoggingStatus -> TestTree
responseEnableLogging = res
    "EnableLoggingResponse"
    "fixture/EnableLoggingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy EnableLogging)

responseCreateClusterSubnetGroup :: CreateClusterSubnetGroupResponse -> TestTree
responseCreateClusterSubnetGroup = res
    "CreateClusterSubnetGroupResponse"
    "fixture/CreateClusterSubnetGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateClusterSubnetGroup)

responseDeleteClusterParameterGroup :: DeleteClusterParameterGroupResponse -> TestTree
responseDeleteClusterParameterGroup = res
    "DeleteClusterParameterGroupResponse"
    "fixture/DeleteClusterParameterGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteClusterParameterGroup)

responseDescribeClusterSecurityGroups :: DescribeClusterSecurityGroupsResponse -> TestTree
responseDescribeClusterSecurityGroups = res
    "DescribeClusterSecurityGroupsResponse"
    "fixture/DescribeClusterSecurityGroupsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeClusterSecurityGroups)

responseCreateTags :: CreateTagsResponse -> TestTree
responseCreateTags = res
    "CreateTagsResponse"
    "fixture/CreateTagsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateTags)

responseEnableSnapshotCopy :: EnableSnapshotCopyResponse -> TestTree
responseEnableSnapshotCopy = res
    "EnableSnapshotCopyResponse"
    "fixture/EnableSnapshotCopyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy EnableSnapshotCopy)

responseDescribeClusterSnapshots :: DescribeClusterSnapshotsResponse -> TestTree
responseDescribeClusterSnapshots = res
    "DescribeClusterSnapshotsResponse"
    "fixture/DescribeClusterSnapshotsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeClusterSnapshots)

responseBatchDeleteClusterSnapshots :: BatchDeleteClusterSnapshotsResponse -> TestTree
responseBatchDeleteClusterSnapshots = res
    "BatchDeleteClusterSnapshotsResponse"
    "fixture/BatchDeleteClusterSnapshotsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy BatchDeleteClusterSnapshots)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags = res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteTags)

responseModifyUsageLimit :: UsageLimit -> TestTree
responseModifyUsageLimit = res
    "ModifyUsageLimitResponse"
    "fixture/ModifyUsageLimitResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyUsageLimit)

responseDescribeClusterSubnetGroups :: DescribeClusterSubnetGroupsResponse -> TestTree
responseDescribeClusterSubnetGroups = res
    "DescribeClusterSubnetGroupsResponse"
    "fixture/DescribeClusterSubnetGroupsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeClusterSubnetGroups)

responseResizeCluster :: ResizeClusterResponse -> TestTree
responseResizeCluster = res
    "ResizeClusterResponse"
    "fixture/ResizeClusterResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ResizeCluster)

responseModifySnapshotCopyRetentionPeriod :: ModifySnapshotCopyRetentionPeriodResponse -> TestTree
responseModifySnapshotCopyRetentionPeriod = res
    "ModifySnapshotCopyRetentionPeriodResponse"
    "fixture/ModifySnapshotCopyRetentionPeriodResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifySnapshotCopyRetentionPeriod)

responseModifyClusterIamRoles :: ModifyClusterIamRolesResponse -> TestTree
responseModifyClusterIamRoles = res
    "ModifyClusterIamRolesResponse"
    "fixture/ModifyClusterIamRolesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyClusterIamRoles)

responseAuthorizeSnapshotAccess :: AuthorizeSnapshotAccessResponse -> TestTree
responseAuthorizeSnapshotAccess = res
    "AuthorizeSnapshotAccessResponse"
    "fixture/AuthorizeSnapshotAccessResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AuthorizeSnapshotAccess)

responseRebootCluster :: RebootClusterResponse -> TestTree
responseRebootCluster = res
    "RebootClusterResponse"
    "fixture/RebootClusterResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RebootCluster)

responseResumeCluster :: ResumeClusterResponse -> TestTree
responseResumeCluster = res
    "ResumeClusterResponse"
    "fixture/ResumeClusterResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ResumeCluster)

responseDeleteCluster :: DeleteClusterResponse -> TestTree
responseDeleteCluster = res
    "DeleteClusterResponse"
    "fixture/DeleteClusterResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteCluster)

responseCreateEventSubscription :: CreateEventSubscriptionResponse -> TestTree
responseCreateEventSubscription = res
    "CreateEventSubscriptionResponse"
    "fixture/CreateEventSubscriptionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateEventSubscription)

responseCreateScheduledAction :: ScheduledAction -> TestTree
responseCreateScheduledAction = res
    "CreateScheduledActionResponse"
    "fixture/CreateScheduledActionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateScheduledAction)

responseDescribeOrderableClusterOptions :: DescribeOrderableClusterOptionsResponse -> TestTree
responseDescribeOrderableClusterOptions = res
    "DescribeOrderableClusterOptionsResponse"
    "fixture/DescribeOrderableClusterOptionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeOrderableClusterOptions)

responseDescribeClusterTracks :: DescribeClusterTracksResponse -> TestTree
responseDescribeClusterTracks = res
    "DescribeClusterTracksResponse"
    "fixture/DescribeClusterTracksResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeClusterTracks)

responseCreateCluster :: CreateClusterResponse -> TestTree
responseCreateCluster = res
    "CreateClusterResponse"
    "fixture/CreateClusterResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateCluster)

responseCreateHsmClientCertificate :: CreateHsmClientCertificateResponse -> TestTree
responseCreateHsmClientCertificate = res
    "CreateHsmClientCertificateResponse"
    "fixture/CreateHsmClientCertificateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateHsmClientCertificate)

responseRestoreTableFromClusterSnapshot :: RestoreTableFromClusterSnapshotResponse -> TestTree
responseRestoreTableFromClusterSnapshot = res
    "RestoreTableFromClusterSnapshotResponse"
    "fixture/RestoreTableFromClusterSnapshotResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RestoreTableFromClusterSnapshot)

responseDeleteScheduledAction :: DeleteScheduledActionResponse -> TestTree
responseDeleteScheduledAction = res
    "DeleteScheduledActionResponse"
    "fixture/DeleteScheduledActionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteScheduledAction)

responseDescribeDefaultClusterParameters :: DescribeDefaultClusterParametersResponse -> TestTree
responseDescribeDefaultClusterParameters = res
    "DescribeDefaultClusterParametersResponse"
    "fixture/DescribeDefaultClusterParametersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeDefaultClusterParameters)

responseDeleteEventSubscription :: DeleteEventSubscriptionResponse -> TestTree
responseDeleteEventSubscription = res
    "DeleteEventSubscriptionResponse"
    "fixture/DeleteEventSubscriptionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteEventSubscription)

responseModifyClusterSnapshot :: ModifyClusterSnapshotResponse -> TestTree
responseModifyClusterSnapshot = res
    "ModifyClusterSnapshotResponse"
    "fixture/ModifyClusterSnapshotResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyClusterSnapshot)

responseResetClusterParameterGroup :: ClusterParameterGroupNameMessage -> TestTree
responseResetClusterParameterGroup = res
    "ResetClusterParameterGroupResponse"
    "fixture/ResetClusterParameterGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ResetClusterParameterGroup)

responseDescribeScheduledActions :: DescribeScheduledActionsResponse -> TestTree
responseDescribeScheduledActions = res
    "DescribeScheduledActionsResponse"
    "fixture/DescribeScheduledActionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeScheduledActions)

responseDescribeEventSubscriptions :: DescribeEventSubscriptionsResponse -> TestTree
responseDescribeEventSubscriptions = res
    "DescribeEventSubscriptionsResponse"
    "fixture/DescribeEventSubscriptionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeEventSubscriptions)

responseDescribeClusterDbRevisions :: DescribeClusterDbRevisionsResponse -> TestTree
responseDescribeClusterDbRevisions = res
    "DescribeClusterDbRevisionsResponse"
    "fixture/DescribeClusterDbRevisionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeClusterDbRevisions)

responseBatchModifyClusterSnapshots :: BatchModifyClusterSnapshotsResponse -> TestTree
responseBatchModifyClusterSnapshots = res
    "BatchModifyClusterSnapshotsResponse"
    "fixture/BatchModifyClusterSnapshotsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy BatchModifyClusterSnapshots)

responseDeleteUsageLimit :: DeleteUsageLimitResponse -> TestTree
responseDeleteUsageLimit = res
    "DeleteUsageLimitResponse"
    "fixture/DeleteUsageLimitResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteUsageLimit)

responseRevokeClusterSecurityGroupIngress :: RevokeClusterSecurityGroupIngressResponse -> TestTree
responseRevokeClusterSecurityGroupIngress = res
    "RevokeClusterSecurityGroupIngressResponse"
    "fixture/RevokeClusterSecurityGroupIngressResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RevokeClusterSecurityGroupIngress)

responseDescribeHsmClientCertificates :: DescribeHsmClientCertificatesResponse -> TestTree
responseDescribeHsmClientCertificates = res
    "DescribeHsmClientCertificatesResponse"
    "fixture/DescribeHsmClientCertificatesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeHsmClientCertificates)

responseModifyClusterParameterGroup :: ClusterParameterGroupNameMessage -> TestTree
responseModifyClusterParameterGroup = res
    "ModifyClusterParameterGroupResponse"
    "fixture/ModifyClusterParameterGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyClusterParameterGroup)

responseGetClusterCredentials :: GetClusterCredentialsResponse -> TestTree
responseGetClusterCredentials = res
    "GetClusterCredentialsResponse"
    "fixture/GetClusterCredentialsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetClusterCredentials)

responseModifyClusterMaintenance :: ModifyClusterMaintenanceResponse -> TestTree
responseModifyClusterMaintenance = res
    "ModifyClusterMaintenanceResponse"
    "fixture/ModifyClusterMaintenanceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyClusterMaintenance)

responseCreateClusterSecurityGroup :: CreateClusterSecurityGroupResponse -> TestTree
responseCreateClusterSecurityGroup = res
    "CreateClusterSecurityGroupResponse"
    "fixture/CreateClusterSecurityGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateClusterSecurityGroup)

responseDescribeEventCategories :: DescribeEventCategoriesResponse -> TestTree
responseDescribeEventCategories = res
    "DescribeEventCategoriesResponse"
    "fixture/DescribeEventCategoriesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeEventCategories)

responseDescribeResize :: ResizeProgressMessage -> TestTree
responseDescribeResize = res
    "DescribeResizeResponse"
    "fixture/DescribeResizeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeResize)

responseDeleteHsmConfiguration :: DeleteHsmConfigurationResponse -> TestTree
responseDeleteHsmConfiguration = res
    "DeleteHsmConfigurationResponse"
    "fixture/DeleteHsmConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteHsmConfiguration)

responseAcceptReservedNodeExchange :: AcceptReservedNodeExchangeResponse -> TestTree
responseAcceptReservedNodeExchange = res
    "AcceptReservedNodeExchangeResponse"
    "fixture/AcceptReservedNodeExchangeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AcceptReservedNodeExchange)

responseAuthorizeClusterSecurityGroupIngress :: AuthorizeClusterSecurityGroupIngressResponse -> TestTree
responseAuthorizeClusterSecurityGroupIngress = res
    "AuthorizeClusterSecurityGroupIngressResponse"
    "fixture/AuthorizeClusterSecurityGroupIngressResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AuthorizeClusterSecurityGroupIngress)

responseDescribeTableRestoreStatus :: DescribeTableRestoreStatusResponse -> TestTree
responseDescribeTableRestoreStatus = res
    "DescribeTableRestoreStatusResponse"
    "fixture/DescribeTableRestoreStatusResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeTableRestoreStatus)

responseCreateClusterSnapshot :: CreateClusterSnapshotResponse -> TestTree
responseCreateClusterSnapshot = res
    "CreateClusterSnapshotResponse"
    "fixture/CreateClusterSnapshotResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateClusterSnapshot)

responseCreateHsmConfiguration :: CreateHsmConfigurationResponse -> TestTree
responseCreateHsmConfiguration = res
    "CreateHsmConfigurationResponse"
    "fixture/CreateHsmConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateHsmConfiguration)

responseDescribeLoggingStatus :: LoggingStatus -> TestTree
responseDescribeLoggingStatus = res
    "DescribeLoggingStatusResponse"
    "fixture/DescribeLoggingStatusResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeLoggingStatus)

responseModifyCluster :: ModifyClusterResponse -> TestTree
responseModifyCluster = res
    "ModifyClusterResponse"
    "fixture/ModifyClusterResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyCluster)

responseDeleteClusterSecurityGroup :: DeleteClusterSecurityGroupResponse -> TestTree
responseDeleteClusterSecurityGroup = res
    "DeleteClusterSecurityGroupResponse"
    "fixture/DeleteClusterSecurityGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteClusterSecurityGroup)

responseCreateSnapshotSchedule :: SnapshotSchedule -> TestTree
responseCreateSnapshotSchedule = res
    "CreateSnapshotScheduleResponse"
    "fixture/CreateSnapshotScheduleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateSnapshotSchedule)

responseDescribeNodeConfigurationOptions :: DescribeNodeConfigurationOptionsResponse -> TestTree
responseDescribeNodeConfigurationOptions = res
    "DescribeNodeConfigurationOptionsResponse"
    "fixture/DescribeNodeConfigurationOptionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeNodeConfigurationOptions)

responseDisableSnapshotCopy :: DisableSnapshotCopyResponse -> TestTree
responseDisableSnapshotCopy = res
    "DisableSnapshotCopyResponse"
    "fixture/DisableSnapshotCopyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisableSnapshotCopy)

responseDescribeClusterParameters :: DescribeClusterParametersResponse -> TestTree
responseDescribeClusterParameters = res
    "DescribeClusterParametersResponse"
    "fixture/DescribeClusterParametersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeClusterParameters)

responsePauseCluster :: PauseClusterResponse -> TestTree
responsePauseCluster = res
    "PauseClusterResponse"
    "fixture/PauseClusterResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PauseCluster)

responseDeleteSnapshotSchedule :: DeleteSnapshotScheduleResponse -> TestTree
responseDeleteSnapshotSchedule = res
    "DeleteSnapshotScheduleResponse"
    "fixture/DeleteSnapshotScheduleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteSnapshotSchedule)

responseRestoreFromClusterSnapshot :: RestoreFromClusterSnapshotResponse -> TestTree
responseRestoreFromClusterSnapshot = res
    "RestoreFromClusterSnapshotResponse"
    "fixture/RestoreFromClusterSnapshotResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RestoreFromClusterSnapshot)

responseCreateClusterParameterGroup :: CreateClusterParameterGroupResponse -> TestTree
responseCreateClusterParameterGroup = res
    "CreateClusterParameterGroupResponse"
    "fixture/CreateClusterParameterGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateClusterParameterGroup)

responseRevokeSnapshotAccess :: RevokeSnapshotAccessResponse -> TestTree
responseRevokeSnapshotAccess = res
    "RevokeSnapshotAccessResponse"
    "fixture/RevokeSnapshotAccessResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RevokeSnapshotAccess)

responseDescribeHsmConfigurations :: DescribeHsmConfigurationsResponse -> TestTree
responseDescribeHsmConfigurations = res
    "DescribeHsmConfigurationsResponse"
    "fixture/DescribeHsmConfigurationsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeHsmConfigurations)

responseDescribeAccountAttributes :: DescribeAccountAttributesResponse -> TestTree
responseDescribeAccountAttributes = res
    "DescribeAccountAttributesResponse"
    "fixture/DescribeAccountAttributesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeAccountAttributes)

responseCreateSnapshotCopyGrant :: CreateSnapshotCopyGrantResponse -> TestTree
responseCreateSnapshotCopyGrant = res
    "CreateSnapshotCopyGrantResponse"
    "fixture/CreateSnapshotCopyGrantResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateSnapshotCopyGrant)

responseCopyClusterSnapshot :: CopyClusterSnapshotResponse -> TestTree
responseCopyClusterSnapshot = res
    "CopyClusterSnapshotResponse"
    "fixture/CopyClusterSnapshotResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CopyClusterSnapshot)

responseDeleteHsmClientCertificate :: DeleteHsmClientCertificateResponse -> TestTree
responseDeleteHsmClientCertificate = res
    "DeleteHsmClientCertificateResponse"
    "fixture/DeleteHsmClientCertificateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteHsmClientCertificate)

responseModifyClusterSnapshotSchedule :: ModifyClusterSnapshotScheduleResponse -> TestTree
responseModifyClusterSnapshotSchedule = res
    "ModifyClusterSnapshotScheduleResponse"
    "fixture/ModifyClusterSnapshotScheduleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyClusterSnapshotSchedule)

responseDeleteSnapshotCopyGrant :: DeleteSnapshotCopyGrantResponse -> TestTree
responseDeleteSnapshotCopyGrant = res
    "DeleteSnapshotCopyGrantResponse"
    "fixture/DeleteSnapshotCopyGrantResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteSnapshotCopyGrant)

responseDescribeClusterVersions :: DescribeClusterVersionsResponse -> TestTree
responseDescribeClusterVersions = res
    "DescribeClusterVersionsResponse"
    "fixture/DescribeClusterVersionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeClusterVersions)

responseModifyClusterSubnetGroup :: ModifyClusterSubnetGroupResponse -> TestTree
responseModifyClusterSubnetGroup = res
    "ModifyClusterSubnetGroupResponse"
    "fixture/ModifyClusterSubnetGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyClusterSubnetGroup)

responseDescribeUsageLimits :: DescribeUsageLimitsResponse -> TestTree
responseDescribeUsageLimits = res
    "DescribeUsageLimitsResponse"
    "fixture/DescribeUsageLimitsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeUsageLimits)

responseModifySnapshotSchedule :: SnapshotSchedule -> TestTree
responseModifySnapshotSchedule = res
    "ModifySnapshotScheduleResponse"
    "fixture/ModifySnapshotScheduleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifySnapshotSchedule)

responseRotateEncryptionKey :: RotateEncryptionKeyResponse -> TestTree
responseRotateEncryptionKey = res
    "RotateEncryptionKeyResponse"
    "fixture/RotateEncryptionKeyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RotateEncryptionKey)

responseDescribeSnapshotCopyGrants :: DescribeSnapshotCopyGrantsResponse -> TestTree
responseDescribeSnapshotCopyGrants = res
    "DescribeSnapshotCopyGrantsResponse"
    "fixture/DescribeSnapshotCopyGrantsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeSnapshotCopyGrants)
