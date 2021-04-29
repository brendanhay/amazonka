{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Redshift
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--         [ requestPurchaseReservedNodeOffering $
--             newPurchaseReservedNodeOffering
--
--         , requestDescribeEventCategories $
--             newDescribeEventCategories
--
--         , requestDeleteClusterSubnetGroup $
--             newDeleteClusterSubnetGroup
--
--         , requestDisableLogging $
--             newDisableLogging
--
--         , requestDescribeHsmClientCertificates $
--             newDescribeHsmClientCertificates
--
--         , requestModifyClusterParameterGroup $
--             newModifyClusterParameterGroup
--
--         , requestCancelResize $
--             newCancelResize
--
--         , requestDescribeClusters $
--             newDescribeClusters
--
--         , requestGetClusterCredentials $
--             newGetClusterCredentials
--
--         , requestRevokeClusterSecurityGroupIngress $
--             newRevokeClusterSecurityGroupIngress
--
--         , requestCreateUsageLimit $
--             newCreateUsageLimit
--
--         , requestDescribeTags $
--             newDescribeTags
--
--         , requestDeleteUsageLimit $
--             newDeleteUsageLimit
--
--         , requestDescribeScheduledActions $
--             newDescribeScheduledActions
--
--         , requestRotateEncryptionKey $
--             newRotateEncryptionKey
--
--         , requestDescribeClusterDbRevisions $
--             newDescribeClusterDbRevisions
--
--         , requestDescribeEventSubscriptions $
--             newDescribeEventSubscriptions
--
--         , requestResetClusterParameterGroup $
--             newResetClusterParameterGroup
--
--         , requestModifyClusterSubnetGroup $
--             newModifyClusterSubnetGroup
--
--         , requestRestoreTableFromClusterSnapshot $
--             newRestoreTableFromClusterSnapshot
--
--         , requestDeleteEventSubscription $
--             newDeleteEventSubscription
--
--         , requestCreateCluster $
--             newCreateCluster
--
--         , requestModifyClusterSnapshot $
--             newModifyClusterSnapshot
--
--         , requestDeleteScheduledAction $
--             newDeleteScheduledAction
--
--         , requestDescribeAccountAttributes $
--             newDescribeAccountAttributes
--
--         , requestCopyClusterSnapshot $
--             newCopyClusterSnapshot
--
--         , requestCreateSnapshotCopyGrant $
--             newCreateSnapshotCopyGrant
--
--         , requestModifyClusterSnapshotSchedule $
--             newModifyClusterSnapshotSchedule
--
--         , requestCreateClusterParameterGroup $
--             newCreateClusterParameterGroup
--
--         , requestDeleteTags $
--             newDeleteTags
--
--         , requestEnableSnapshotCopy $
--             newEnableSnapshotCopy
--
--         , requestDescribeClusterSnapshots $
--             newDescribeClusterSnapshots
--
--         , requestDescribeHsmConfigurations $
--             newDescribeHsmConfigurations
--
--         , requestModifyUsageLimit $
--             newModifyUsageLimit
--
--         , requestDescribeClusterSecurityGroups $
--             newDescribeClusterSecurityGroups
--
--         , requestDeleteClusterParameterGroup $
--             newDeleteClusterParameterGroup
--
--         , requestCreateSnapshotSchedule $
--             newCreateSnapshotSchedule
--
--         , requestDescribeNodeConfigurationOptions $
--             newDescribeNodeConfigurationOptions
--
--         , requestDescribeClusterParameterGroups $
--             newDescribeClusterParameterGroups
--
--         , requestDescribeLoggingStatus $
--             newDescribeLoggingStatus
--
--         , requestDescribeEvents $
--             newDescribeEvents
--
--         , requestCreateClusterSnapshot $
--             newCreateClusterSnapshot
--
--         , requestCreateClusterSubnetGroup $
--             newCreateClusterSubnetGroup
--
--         , requestModifyCluster $
--             newModifyCluster
--
--         , requestGetReservedNodeExchangeOfferings $
--             newGetReservedNodeExchangeOfferings
--
--         , requestDescribeResize $
--             newDescribeResize
--
--         , requestDeleteHsmConfiguration $
--             newDeleteHsmConfiguration
--
--         , requestDeleteClusterSnapshot $
--             newDeleteClusterSnapshot
--
--         , requestCreateClusterSecurityGroup $
--             newCreateClusterSecurityGroup
--
--         , requestAcceptReservedNodeExchange $
--             newAcceptReservedNodeExchange
--
--         , requestModifyScheduledAction $
--             newModifyScheduledAction
--
--         , requestModifyEventSubscription $
--             newModifyEventSubscription
--
--         , requestDescribeSnapshotSchedules $
--             newDescribeSnapshotSchedules
--
--         , requestDescribeTableRestoreStatus $
--             newDescribeTableRestoreStatus
--
--         , requestModifyClusterMaintenance $
--             newModifyClusterMaintenance
--
--         , requestAuthorizeClusterSecurityGroupIngress $
--             newAuthorizeClusterSecurityGroupIngress
--
--         , requestModifyClusterDbRevision $
--             newModifyClusterDbRevision
--
--         , requestDescribeStorage $
--             newDescribeStorage
--
--         , requestDescribeSnapshotCopyGrants $
--             newDescribeSnapshotCopyGrants
--
--         , requestBatchModifyClusterSnapshots $
--             newBatchModifyClusterSnapshots
--
--         , requestModifySnapshotSchedule $
--             newModifySnapshotSchedule
--
--         , requestDeleteSnapshotCopyGrant $
--             newDeleteSnapshotCopyGrant
--
--         , requestDescribeUsageLimits $
--             newDescribeUsageLimits
--
--         , requestDescribeDefaultClusterParameters $
--             newDescribeDefaultClusterParameters
--
--         , requestCreateHsmClientCertificate $
--             newCreateHsmClientCertificate
--
--         , requestDescribeClusterVersions $
--             newDescribeClusterVersions
--
--         , requestDescribeOrderableClusterOptions $
--             newDescribeOrderableClusterOptions
--
--         , requestDeleteHsmClientCertificate $
--             newDeleteHsmClientCertificate
--
--         , requestRebootCluster $
--             newRebootCluster
--
--         , requestDeleteCluster $
--             newDeleteCluster
--
--         , requestCreateEventSubscription $
--             newCreateEventSubscription
--
--         , requestAuthorizeSnapshotAccess $
--             newAuthorizeSnapshotAccess
--
--         , requestResumeCluster $
--             newResumeCluster
--
--         , requestDescribeClusterTracks $
--             newDescribeClusterTracks
--
--         , requestCreateScheduledAction $
--             newCreateScheduledAction
--
--         , requestDeleteSnapshotSchedule $
--             newDeleteSnapshotSchedule
--
--         , requestDescribeClusterSubnetGroups $
--             newDescribeClusterSubnetGroups
--
--         , requestBatchDeleteClusterSnapshots $
--             newBatchDeleteClusterSnapshots
--
--         , requestModifyClusterIamRoles $
--             newModifyClusterIamRoles
--
--         , requestResizeCluster $
--             newResizeCluster
--
--         , requestModifySnapshotCopyRetentionPeriod $
--             newModifySnapshotCopyRetentionPeriod
--
--         , requestRestoreFromClusterSnapshot $
--             newRestoreFromClusterSnapshot
--
--         , requestRevokeSnapshotAccess $
--             newRevokeSnapshotAccess
--
--         , requestPauseCluster $
--             newPauseCluster
--
--         , requestDescribeClusterParameters $
--             newDescribeClusterParameters
--
--         , requestDisableSnapshotCopy $
--             newDisableSnapshotCopy
--
--         , requestCreateTags $
--             newCreateTags
--
--         , requestDescribeReservedNodeOfferings $
--             newDescribeReservedNodeOfferings
--
--         , requestDeleteClusterSecurityGroup $
--             newDeleteClusterSecurityGroup
--
--         , requestEnableLogging $
--             newEnableLogging
--
--         , requestCreateHsmConfiguration $
--             newCreateHsmConfiguration
--
--         , requestDescribeReservedNodes $
--             newDescribeReservedNodes
--
--           ]

--     , testGroup "response"
--         [ responsePurchaseReservedNodeOffering $
--             newPurchaseReservedNodeOfferingResponse
--
--         , responseDescribeEventCategories $
--             newDescribeEventCategoriesResponse
--
--         , responseDeleteClusterSubnetGroup $
--             newDeleteClusterSubnetGroupResponse
--
--         , responseDisableLogging $
--             newLoggingStatus
--
--         , responseDescribeHsmClientCertificates $
--             newDescribeHsmClientCertificatesResponse
--
--         , responseModifyClusterParameterGroup $
--             newClusterParameterGroupNameMessage
--
--         , responseCancelResize $
--             newResizeProgressMessage
--
--         , responseDescribeClusters $
--             newDescribeClustersResponse
--
--         , responseGetClusterCredentials $
--             newGetClusterCredentialsResponse
--
--         , responseRevokeClusterSecurityGroupIngress $
--             newRevokeClusterSecurityGroupIngressResponse
--
--         , responseCreateUsageLimit $
--             newUsageLimit
--
--         , responseDescribeTags $
--             newDescribeTagsResponse
--
--         , responseDeleteUsageLimit $
--             newDeleteUsageLimitResponse
--
--         , responseDescribeScheduledActions $
--             newDescribeScheduledActionsResponse
--
--         , responseRotateEncryptionKey $
--             newRotateEncryptionKeyResponse
--
--         , responseDescribeClusterDbRevisions $
--             newDescribeClusterDbRevisionsResponse
--
--         , responseDescribeEventSubscriptions $
--             newDescribeEventSubscriptionsResponse
--
--         , responseResetClusterParameterGroup $
--             newClusterParameterGroupNameMessage
--
--         , responseModifyClusterSubnetGroup $
--             newModifyClusterSubnetGroupResponse
--
--         , responseRestoreTableFromClusterSnapshot $
--             newRestoreTableFromClusterSnapshotResponse
--
--         , responseDeleteEventSubscription $
--             newDeleteEventSubscriptionResponse
--
--         , responseCreateCluster $
--             newCreateClusterResponse
--
--         , responseModifyClusterSnapshot $
--             newModifyClusterSnapshotResponse
--
--         , responseDeleteScheduledAction $
--             newDeleteScheduledActionResponse
--
--         , responseDescribeAccountAttributes $
--             newDescribeAccountAttributesResponse
--
--         , responseCopyClusterSnapshot $
--             newCopyClusterSnapshotResponse
--
--         , responseCreateSnapshotCopyGrant $
--             newCreateSnapshotCopyGrantResponse
--
--         , responseModifyClusterSnapshotSchedule $
--             newModifyClusterSnapshotScheduleResponse
--
--         , responseCreateClusterParameterGroup $
--             newCreateClusterParameterGroupResponse
--
--         , responseDeleteTags $
--             newDeleteTagsResponse
--
--         , responseEnableSnapshotCopy $
--             newEnableSnapshotCopyResponse
--
--         , responseDescribeClusterSnapshots $
--             newDescribeClusterSnapshotsResponse
--
--         , responseDescribeHsmConfigurations $
--             newDescribeHsmConfigurationsResponse
--
--         , responseModifyUsageLimit $
--             newUsageLimit
--
--         , responseDescribeClusterSecurityGroups $
--             newDescribeClusterSecurityGroupsResponse
--
--         , responseDeleteClusterParameterGroup $
--             newDeleteClusterParameterGroupResponse
--
--         , responseCreateSnapshotSchedule $
--             newSnapshotSchedule
--
--         , responseDescribeNodeConfigurationOptions $
--             newDescribeNodeConfigurationOptionsResponse
--
--         , responseDescribeClusterParameterGroups $
--             newDescribeClusterParameterGroupsResponse
--
--         , responseDescribeLoggingStatus $
--             newLoggingStatus
--
--         , responseDescribeEvents $
--             newDescribeEventsResponse
--
--         , responseCreateClusterSnapshot $
--             newCreateClusterSnapshotResponse
--
--         , responseCreateClusterSubnetGroup $
--             newCreateClusterSubnetGroupResponse
--
--         , responseModifyCluster $
--             newModifyClusterResponse
--
--         , responseGetReservedNodeExchangeOfferings $
--             newGetReservedNodeExchangeOfferingsResponse
--
--         , responseDescribeResize $
--             newResizeProgressMessage
--
--         , responseDeleteHsmConfiguration $
--             newDeleteHsmConfigurationResponse
--
--         , responseDeleteClusterSnapshot $
--             newDeleteClusterSnapshotResponse
--
--         , responseCreateClusterSecurityGroup $
--             newCreateClusterSecurityGroupResponse
--
--         , responseAcceptReservedNodeExchange $
--             newAcceptReservedNodeExchangeResponse
--
--         , responseModifyScheduledAction $
--             newScheduledAction
--
--         , responseModifyEventSubscription $
--             newModifyEventSubscriptionResponse
--
--         , responseDescribeSnapshotSchedules $
--             newDescribeSnapshotSchedulesResponse
--
--         , responseDescribeTableRestoreStatus $
--             newDescribeTableRestoreStatusResponse
--
--         , responseModifyClusterMaintenance $
--             newModifyClusterMaintenanceResponse
--
--         , responseAuthorizeClusterSecurityGroupIngress $
--             newAuthorizeClusterSecurityGroupIngressResponse
--
--         , responseModifyClusterDbRevision $
--             newModifyClusterDbRevisionResponse
--
--         , responseDescribeStorage $
--             newDescribeStorageResponse
--
--         , responseDescribeSnapshotCopyGrants $
--             newDescribeSnapshotCopyGrantsResponse
--
--         , responseBatchModifyClusterSnapshots $
--             newBatchModifyClusterSnapshotsResponse
--
--         , responseModifySnapshotSchedule $
--             newSnapshotSchedule
--
--         , responseDeleteSnapshotCopyGrant $
--             newDeleteSnapshotCopyGrantResponse
--
--         , responseDescribeUsageLimits $
--             newDescribeUsageLimitsResponse
--
--         , responseDescribeDefaultClusterParameters $
--             newDescribeDefaultClusterParametersResponse
--
--         , responseCreateHsmClientCertificate $
--             newCreateHsmClientCertificateResponse
--
--         , responseDescribeClusterVersions $
--             newDescribeClusterVersionsResponse
--
--         , responseDescribeOrderableClusterOptions $
--             newDescribeOrderableClusterOptionsResponse
--
--         , responseDeleteHsmClientCertificate $
--             newDeleteHsmClientCertificateResponse
--
--         , responseRebootCluster $
--             newRebootClusterResponse
--
--         , responseDeleteCluster $
--             newDeleteClusterResponse
--
--         , responseCreateEventSubscription $
--             newCreateEventSubscriptionResponse
--
--         , responseAuthorizeSnapshotAccess $
--             newAuthorizeSnapshotAccessResponse
--
--         , responseResumeCluster $
--             newResumeClusterResponse
--
--         , responseDescribeClusterTracks $
--             newDescribeClusterTracksResponse
--
--         , responseCreateScheduledAction $
--             newScheduledAction
--
--         , responseDeleteSnapshotSchedule $
--             newDeleteSnapshotScheduleResponse
--
--         , responseDescribeClusterSubnetGroups $
--             newDescribeClusterSubnetGroupsResponse
--
--         , responseBatchDeleteClusterSnapshots $
--             newBatchDeleteClusterSnapshotsResponse
--
--         , responseModifyClusterIamRoles $
--             newModifyClusterIamRolesResponse
--
--         , responseResizeCluster $
--             newResizeClusterResponse
--
--         , responseModifySnapshotCopyRetentionPeriod $
--             newModifySnapshotCopyRetentionPeriodResponse
--
--         , responseRestoreFromClusterSnapshot $
--             newRestoreFromClusterSnapshotResponse
--
--         , responseRevokeSnapshotAccess $
--             newRevokeSnapshotAccessResponse
--
--         , responsePauseCluster $
--             newPauseClusterResponse
--
--         , responseDescribeClusterParameters $
--             newDescribeClusterParametersResponse
--
--         , responseDisableSnapshotCopy $
--             newDisableSnapshotCopyResponse
--
--         , responseCreateTags $
--             newCreateTagsResponse
--
--         , responseDescribeReservedNodeOfferings $
--             newDescribeReservedNodeOfferingsResponse
--
--         , responseDeleteClusterSecurityGroup $
--             newDeleteClusterSecurityGroupResponse
--
--         , responseEnableLogging $
--             newLoggingStatus
--
--         , responseCreateHsmConfiguration $
--             newCreateHsmConfigurationResponse
--
--         , responseDescribeReservedNodes $
--             newDescribeReservedNodesResponse
--
--           ]
--     ]

-- Requests

requestPurchaseReservedNodeOffering :: PurchaseReservedNodeOffering -> TestTree
requestPurchaseReservedNodeOffering =
  req
    "PurchaseReservedNodeOffering"
    "fixture/PurchaseReservedNodeOffering.yaml"

requestDescribeEventCategories :: DescribeEventCategories -> TestTree
requestDescribeEventCategories =
  req
    "DescribeEventCategories"
    "fixture/DescribeEventCategories.yaml"

requestDeleteClusterSubnetGroup :: DeleteClusterSubnetGroup -> TestTree
requestDeleteClusterSubnetGroup =
  req
    "DeleteClusterSubnetGroup"
    "fixture/DeleteClusterSubnetGroup.yaml"

requestDisableLogging :: DisableLogging -> TestTree
requestDisableLogging =
  req
    "DisableLogging"
    "fixture/DisableLogging.yaml"

requestDescribeHsmClientCertificates :: DescribeHsmClientCertificates -> TestTree
requestDescribeHsmClientCertificates =
  req
    "DescribeHsmClientCertificates"
    "fixture/DescribeHsmClientCertificates.yaml"

requestModifyClusterParameterGroup :: ModifyClusterParameterGroup -> TestTree
requestModifyClusterParameterGroup =
  req
    "ModifyClusterParameterGroup"
    "fixture/ModifyClusterParameterGroup.yaml"

requestCancelResize :: CancelResize -> TestTree
requestCancelResize =
  req
    "CancelResize"
    "fixture/CancelResize.yaml"

requestDescribeClusters :: DescribeClusters -> TestTree
requestDescribeClusters =
  req
    "DescribeClusters"
    "fixture/DescribeClusters.yaml"

requestGetClusterCredentials :: GetClusterCredentials -> TestTree
requestGetClusterCredentials =
  req
    "GetClusterCredentials"
    "fixture/GetClusterCredentials.yaml"

requestRevokeClusterSecurityGroupIngress :: RevokeClusterSecurityGroupIngress -> TestTree
requestRevokeClusterSecurityGroupIngress =
  req
    "RevokeClusterSecurityGroupIngress"
    "fixture/RevokeClusterSecurityGroupIngress.yaml"

requestCreateUsageLimit :: CreateUsageLimit -> TestTree
requestCreateUsageLimit =
  req
    "CreateUsageLimit"
    "fixture/CreateUsageLimit.yaml"

requestDescribeTags :: DescribeTags -> TestTree
requestDescribeTags =
  req
    "DescribeTags"
    "fixture/DescribeTags.yaml"

requestDeleteUsageLimit :: DeleteUsageLimit -> TestTree
requestDeleteUsageLimit =
  req
    "DeleteUsageLimit"
    "fixture/DeleteUsageLimit.yaml"

requestDescribeScheduledActions :: DescribeScheduledActions -> TestTree
requestDescribeScheduledActions =
  req
    "DescribeScheduledActions"
    "fixture/DescribeScheduledActions.yaml"

requestRotateEncryptionKey :: RotateEncryptionKey -> TestTree
requestRotateEncryptionKey =
  req
    "RotateEncryptionKey"
    "fixture/RotateEncryptionKey.yaml"

requestDescribeClusterDbRevisions :: DescribeClusterDbRevisions -> TestTree
requestDescribeClusterDbRevisions =
  req
    "DescribeClusterDbRevisions"
    "fixture/DescribeClusterDbRevisions.yaml"

requestDescribeEventSubscriptions :: DescribeEventSubscriptions -> TestTree
requestDescribeEventSubscriptions =
  req
    "DescribeEventSubscriptions"
    "fixture/DescribeEventSubscriptions.yaml"

requestResetClusterParameterGroup :: ResetClusterParameterGroup -> TestTree
requestResetClusterParameterGroup =
  req
    "ResetClusterParameterGroup"
    "fixture/ResetClusterParameterGroup.yaml"

requestModifyClusterSubnetGroup :: ModifyClusterSubnetGroup -> TestTree
requestModifyClusterSubnetGroup =
  req
    "ModifyClusterSubnetGroup"
    "fixture/ModifyClusterSubnetGroup.yaml"

requestRestoreTableFromClusterSnapshot :: RestoreTableFromClusterSnapshot -> TestTree
requestRestoreTableFromClusterSnapshot =
  req
    "RestoreTableFromClusterSnapshot"
    "fixture/RestoreTableFromClusterSnapshot.yaml"

requestDeleteEventSubscription :: DeleteEventSubscription -> TestTree
requestDeleteEventSubscription =
  req
    "DeleteEventSubscription"
    "fixture/DeleteEventSubscription.yaml"

requestCreateCluster :: CreateCluster -> TestTree
requestCreateCluster =
  req
    "CreateCluster"
    "fixture/CreateCluster.yaml"

requestModifyClusterSnapshot :: ModifyClusterSnapshot -> TestTree
requestModifyClusterSnapshot =
  req
    "ModifyClusterSnapshot"
    "fixture/ModifyClusterSnapshot.yaml"

requestDeleteScheduledAction :: DeleteScheduledAction -> TestTree
requestDeleteScheduledAction =
  req
    "DeleteScheduledAction"
    "fixture/DeleteScheduledAction.yaml"

requestDescribeAccountAttributes :: DescribeAccountAttributes -> TestTree
requestDescribeAccountAttributes =
  req
    "DescribeAccountAttributes"
    "fixture/DescribeAccountAttributes.yaml"

requestCopyClusterSnapshot :: CopyClusterSnapshot -> TestTree
requestCopyClusterSnapshot =
  req
    "CopyClusterSnapshot"
    "fixture/CopyClusterSnapshot.yaml"

requestCreateSnapshotCopyGrant :: CreateSnapshotCopyGrant -> TestTree
requestCreateSnapshotCopyGrant =
  req
    "CreateSnapshotCopyGrant"
    "fixture/CreateSnapshotCopyGrant.yaml"

requestModifyClusterSnapshotSchedule :: ModifyClusterSnapshotSchedule -> TestTree
requestModifyClusterSnapshotSchedule =
  req
    "ModifyClusterSnapshotSchedule"
    "fixture/ModifyClusterSnapshotSchedule.yaml"

requestCreateClusterParameterGroup :: CreateClusterParameterGroup -> TestTree
requestCreateClusterParameterGroup =
  req
    "CreateClusterParameterGroup"
    "fixture/CreateClusterParameterGroup.yaml"

requestDeleteTags :: DeleteTags -> TestTree
requestDeleteTags =
  req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

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

requestDescribeHsmConfigurations :: DescribeHsmConfigurations -> TestTree
requestDescribeHsmConfigurations =
  req
    "DescribeHsmConfigurations"
    "fixture/DescribeHsmConfigurations.yaml"

requestModifyUsageLimit :: ModifyUsageLimit -> TestTree
requestModifyUsageLimit =
  req
    "ModifyUsageLimit"
    "fixture/ModifyUsageLimit.yaml"

requestDescribeClusterSecurityGroups :: DescribeClusterSecurityGroups -> TestTree
requestDescribeClusterSecurityGroups =
  req
    "DescribeClusterSecurityGroups"
    "fixture/DescribeClusterSecurityGroups.yaml"

requestDeleteClusterParameterGroup :: DeleteClusterParameterGroup -> TestTree
requestDeleteClusterParameterGroup =
  req
    "DeleteClusterParameterGroup"
    "fixture/DeleteClusterParameterGroup.yaml"

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

requestDescribeClusterParameterGroups :: DescribeClusterParameterGroups -> TestTree
requestDescribeClusterParameterGroups =
  req
    "DescribeClusterParameterGroups"
    "fixture/DescribeClusterParameterGroups.yaml"

requestDescribeLoggingStatus :: DescribeLoggingStatus -> TestTree
requestDescribeLoggingStatus =
  req
    "DescribeLoggingStatus"
    "fixture/DescribeLoggingStatus.yaml"

requestDescribeEvents :: DescribeEvents -> TestTree
requestDescribeEvents =
  req
    "DescribeEvents"
    "fixture/DescribeEvents.yaml"

requestCreateClusterSnapshot :: CreateClusterSnapshot -> TestTree
requestCreateClusterSnapshot =
  req
    "CreateClusterSnapshot"
    "fixture/CreateClusterSnapshot.yaml"

requestCreateClusterSubnetGroup :: CreateClusterSubnetGroup -> TestTree
requestCreateClusterSubnetGroup =
  req
    "CreateClusterSubnetGroup"
    "fixture/CreateClusterSubnetGroup.yaml"

requestModifyCluster :: ModifyCluster -> TestTree
requestModifyCluster =
  req
    "ModifyCluster"
    "fixture/ModifyCluster.yaml"

requestGetReservedNodeExchangeOfferings :: GetReservedNodeExchangeOfferings -> TestTree
requestGetReservedNodeExchangeOfferings =
  req
    "GetReservedNodeExchangeOfferings"
    "fixture/GetReservedNodeExchangeOfferings.yaml"

requestDescribeResize :: DescribeResize -> TestTree
requestDescribeResize =
  req
    "DescribeResize"
    "fixture/DescribeResize.yaml"

requestDeleteHsmConfiguration :: DeleteHsmConfiguration -> TestTree
requestDeleteHsmConfiguration =
  req
    "DeleteHsmConfiguration"
    "fixture/DeleteHsmConfiguration.yaml"

requestDeleteClusterSnapshot :: DeleteClusterSnapshot -> TestTree
requestDeleteClusterSnapshot =
  req
    "DeleteClusterSnapshot"
    "fixture/DeleteClusterSnapshot.yaml"

requestCreateClusterSecurityGroup :: CreateClusterSecurityGroup -> TestTree
requestCreateClusterSecurityGroup =
  req
    "CreateClusterSecurityGroup"
    "fixture/CreateClusterSecurityGroup.yaml"

requestAcceptReservedNodeExchange :: AcceptReservedNodeExchange -> TestTree
requestAcceptReservedNodeExchange =
  req
    "AcceptReservedNodeExchange"
    "fixture/AcceptReservedNodeExchange.yaml"

requestModifyScheduledAction :: ModifyScheduledAction -> TestTree
requestModifyScheduledAction =
  req
    "ModifyScheduledAction"
    "fixture/ModifyScheduledAction.yaml"

requestModifyEventSubscription :: ModifyEventSubscription -> TestTree
requestModifyEventSubscription =
  req
    "ModifyEventSubscription"
    "fixture/ModifyEventSubscription.yaml"

requestDescribeSnapshotSchedules :: DescribeSnapshotSchedules -> TestTree
requestDescribeSnapshotSchedules =
  req
    "DescribeSnapshotSchedules"
    "fixture/DescribeSnapshotSchedules.yaml"

requestDescribeTableRestoreStatus :: DescribeTableRestoreStatus -> TestTree
requestDescribeTableRestoreStatus =
  req
    "DescribeTableRestoreStatus"
    "fixture/DescribeTableRestoreStatus.yaml"

requestModifyClusterMaintenance :: ModifyClusterMaintenance -> TestTree
requestModifyClusterMaintenance =
  req
    "ModifyClusterMaintenance"
    "fixture/ModifyClusterMaintenance.yaml"

requestAuthorizeClusterSecurityGroupIngress :: AuthorizeClusterSecurityGroupIngress -> TestTree
requestAuthorizeClusterSecurityGroupIngress =
  req
    "AuthorizeClusterSecurityGroupIngress"
    "fixture/AuthorizeClusterSecurityGroupIngress.yaml"

requestModifyClusterDbRevision :: ModifyClusterDbRevision -> TestTree
requestModifyClusterDbRevision =
  req
    "ModifyClusterDbRevision"
    "fixture/ModifyClusterDbRevision.yaml"

requestDescribeStorage :: DescribeStorage -> TestTree
requestDescribeStorage =
  req
    "DescribeStorage"
    "fixture/DescribeStorage.yaml"

requestDescribeSnapshotCopyGrants :: DescribeSnapshotCopyGrants -> TestTree
requestDescribeSnapshotCopyGrants =
  req
    "DescribeSnapshotCopyGrants"
    "fixture/DescribeSnapshotCopyGrants.yaml"

requestBatchModifyClusterSnapshots :: BatchModifyClusterSnapshots -> TestTree
requestBatchModifyClusterSnapshots =
  req
    "BatchModifyClusterSnapshots"
    "fixture/BatchModifyClusterSnapshots.yaml"

requestModifySnapshotSchedule :: ModifySnapshotSchedule -> TestTree
requestModifySnapshotSchedule =
  req
    "ModifySnapshotSchedule"
    "fixture/ModifySnapshotSchedule.yaml"

requestDeleteSnapshotCopyGrant :: DeleteSnapshotCopyGrant -> TestTree
requestDeleteSnapshotCopyGrant =
  req
    "DeleteSnapshotCopyGrant"
    "fixture/DeleteSnapshotCopyGrant.yaml"

requestDescribeUsageLimits :: DescribeUsageLimits -> TestTree
requestDescribeUsageLimits =
  req
    "DescribeUsageLimits"
    "fixture/DescribeUsageLimits.yaml"

requestDescribeDefaultClusterParameters :: DescribeDefaultClusterParameters -> TestTree
requestDescribeDefaultClusterParameters =
  req
    "DescribeDefaultClusterParameters"
    "fixture/DescribeDefaultClusterParameters.yaml"

requestCreateHsmClientCertificate :: CreateHsmClientCertificate -> TestTree
requestCreateHsmClientCertificate =
  req
    "CreateHsmClientCertificate"
    "fixture/CreateHsmClientCertificate.yaml"

requestDescribeClusterVersions :: DescribeClusterVersions -> TestTree
requestDescribeClusterVersions =
  req
    "DescribeClusterVersions"
    "fixture/DescribeClusterVersions.yaml"

requestDescribeOrderableClusterOptions :: DescribeOrderableClusterOptions -> TestTree
requestDescribeOrderableClusterOptions =
  req
    "DescribeOrderableClusterOptions"
    "fixture/DescribeOrderableClusterOptions.yaml"

requestDeleteHsmClientCertificate :: DeleteHsmClientCertificate -> TestTree
requestDeleteHsmClientCertificate =
  req
    "DeleteHsmClientCertificate"
    "fixture/DeleteHsmClientCertificate.yaml"

requestRebootCluster :: RebootCluster -> TestTree
requestRebootCluster =
  req
    "RebootCluster"
    "fixture/RebootCluster.yaml"

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

requestAuthorizeSnapshotAccess :: AuthorizeSnapshotAccess -> TestTree
requestAuthorizeSnapshotAccess =
  req
    "AuthorizeSnapshotAccess"
    "fixture/AuthorizeSnapshotAccess.yaml"

requestResumeCluster :: ResumeCluster -> TestTree
requestResumeCluster =
  req
    "ResumeCluster"
    "fixture/ResumeCluster.yaml"

requestDescribeClusterTracks :: DescribeClusterTracks -> TestTree
requestDescribeClusterTracks =
  req
    "DescribeClusterTracks"
    "fixture/DescribeClusterTracks.yaml"

requestCreateScheduledAction :: CreateScheduledAction -> TestTree
requestCreateScheduledAction =
  req
    "CreateScheduledAction"
    "fixture/CreateScheduledAction.yaml"

requestDeleteSnapshotSchedule :: DeleteSnapshotSchedule -> TestTree
requestDeleteSnapshotSchedule =
  req
    "DeleteSnapshotSchedule"
    "fixture/DeleteSnapshotSchedule.yaml"

requestDescribeClusterSubnetGroups :: DescribeClusterSubnetGroups -> TestTree
requestDescribeClusterSubnetGroups =
  req
    "DescribeClusterSubnetGroups"
    "fixture/DescribeClusterSubnetGroups.yaml"

requestBatchDeleteClusterSnapshots :: BatchDeleteClusterSnapshots -> TestTree
requestBatchDeleteClusterSnapshots =
  req
    "BatchDeleteClusterSnapshots"
    "fixture/BatchDeleteClusterSnapshots.yaml"

requestModifyClusterIamRoles :: ModifyClusterIamRoles -> TestTree
requestModifyClusterIamRoles =
  req
    "ModifyClusterIamRoles"
    "fixture/ModifyClusterIamRoles.yaml"

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

requestRestoreFromClusterSnapshot :: RestoreFromClusterSnapshot -> TestTree
requestRestoreFromClusterSnapshot =
  req
    "RestoreFromClusterSnapshot"
    "fixture/RestoreFromClusterSnapshot.yaml"

requestRevokeSnapshotAccess :: RevokeSnapshotAccess -> TestTree
requestRevokeSnapshotAccess =
  req
    "RevokeSnapshotAccess"
    "fixture/RevokeSnapshotAccess.yaml"

requestPauseCluster :: PauseCluster -> TestTree
requestPauseCluster =
  req
    "PauseCluster"
    "fixture/PauseCluster.yaml"

requestDescribeClusterParameters :: DescribeClusterParameters -> TestTree
requestDescribeClusterParameters =
  req
    "DescribeClusterParameters"
    "fixture/DescribeClusterParameters.yaml"

requestDisableSnapshotCopy :: DisableSnapshotCopy -> TestTree
requestDisableSnapshotCopy =
  req
    "DisableSnapshotCopy"
    "fixture/DisableSnapshotCopy.yaml"

requestCreateTags :: CreateTags -> TestTree
requestCreateTags =
  req
    "CreateTags"
    "fixture/CreateTags.yaml"

requestDescribeReservedNodeOfferings :: DescribeReservedNodeOfferings -> TestTree
requestDescribeReservedNodeOfferings =
  req
    "DescribeReservedNodeOfferings"
    "fixture/DescribeReservedNodeOfferings.yaml"

requestDeleteClusterSecurityGroup :: DeleteClusterSecurityGroup -> TestTree
requestDeleteClusterSecurityGroup =
  req
    "DeleteClusterSecurityGroup"
    "fixture/DeleteClusterSecurityGroup.yaml"

requestEnableLogging :: EnableLogging -> TestTree
requestEnableLogging =
  req
    "EnableLogging"
    "fixture/EnableLogging.yaml"

requestCreateHsmConfiguration :: CreateHsmConfiguration -> TestTree
requestCreateHsmConfiguration =
  req
    "CreateHsmConfiguration"
    "fixture/CreateHsmConfiguration.yaml"

requestDescribeReservedNodes :: DescribeReservedNodes -> TestTree
requestDescribeReservedNodes =
  req
    "DescribeReservedNodes"
    "fixture/DescribeReservedNodes.yaml"

-- Responses

responsePurchaseReservedNodeOffering :: PurchaseReservedNodeOfferingResponse -> TestTree
responsePurchaseReservedNodeOffering =
  res
    "PurchaseReservedNodeOfferingResponse"
    "fixture/PurchaseReservedNodeOfferingResponse.proto"
    defaultService
    (Proxy :: Proxy PurchaseReservedNodeOffering)

responseDescribeEventCategories :: DescribeEventCategoriesResponse -> TestTree
responseDescribeEventCategories =
  res
    "DescribeEventCategoriesResponse"
    "fixture/DescribeEventCategoriesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEventCategories)

responseDeleteClusterSubnetGroup :: DeleteClusterSubnetGroupResponse -> TestTree
responseDeleteClusterSubnetGroup =
  res
    "DeleteClusterSubnetGroupResponse"
    "fixture/DeleteClusterSubnetGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteClusterSubnetGroup)

responseDisableLogging :: LoggingStatus -> TestTree
responseDisableLogging =
  res
    "DisableLoggingResponse"
    "fixture/DisableLoggingResponse.proto"
    defaultService
    (Proxy :: Proxy DisableLogging)

responseDescribeHsmClientCertificates :: DescribeHsmClientCertificatesResponse -> TestTree
responseDescribeHsmClientCertificates =
  res
    "DescribeHsmClientCertificatesResponse"
    "fixture/DescribeHsmClientCertificatesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeHsmClientCertificates)

responseModifyClusterParameterGroup :: ClusterParameterGroupNameMessage -> TestTree
responseModifyClusterParameterGroup =
  res
    "ModifyClusterParameterGroupResponse"
    "fixture/ModifyClusterParameterGroupResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyClusterParameterGroup)

responseCancelResize :: ResizeProgressMessage -> TestTree
responseCancelResize =
  res
    "CancelResizeResponse"
    "fixture/CancelResizeResponse.proto"
    defaultService
    (Proxy :: Proxy CancelResize)

responseDescribeClusters :: DescribeClustersResponse -> TestTree
responseDescribeClusters =
  res
    "DescribeClustersResponse"
    "fixture/DescribeClustersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeClusters)

responseGetClusterCredentials :: GetClusterCredentialsResponse -> TestTree
responseGetClusterCredentials =
  res
    "GetClusterCredentialsResponse"
    "fixture/GetClusterCredentialsResponse.proto"
    defaultService
    (Proxy :: Proxy GetClusterCredentials)

responseRevokeClusterSecurityGroupIngress :: RevokeClusterSecurityGroupIngressResponse -> TestTree
responseRevokeClusterSecurityGroupIngress =
  res
    "RevokeClusterSecurityGroupIngressResponse"
    "fixture/RevokeClusterSecurityGroupIngressResponse.proto"
    defaultService
    (Proxy :: Proxy RevokeClusterSecurityGroupIngress)

responseCreateUsageLimit :: UsageLimit -> TestTree
responseCreateUsageLimit =
  res
    "CreateUsageLimitResponse"
    "fixture/CreateUsageLimitResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUsageLimit)

responseDescribeTags :: DescribeTagsResponse -> TestTree
responseDescribeTags =
  res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTags)

responseDeleteUsageLimit :: DeleteUsageLimitResponse -> TestTree
responseDeleteUsageLimit =
  res
    "DeleteUsageLimitResponse"
    "fixture/DeleteUsageLimitResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUsageLimit)

responseDescribeScheduledActions :: DescribeScheduledActionsResponse -> TestTree
responseDescribeScheduledActions =
  res
    "DescribeScheduledActionsResponse"
    "fixture/DescribeScheduledActionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeScheduledActions)

responseRotateEncryptionKey :: RotateEncryptionKeyResponse -> TestTree
responseRotateEncryptionKey =
  res
    "RotateEncryptionKeyResponse"
    "fixture/RotateEncryptionKeyResponse.proto"
    defaultService
    (Proxy :: Proxy RotateEncryptionKey)

responseDescribeClusterDbRevisions :: DescribeClusterDbRevisionsResponse -> TestTree
responseDescribeClusterDbRevisions =
  res
    "DescribeClusterDbRevisionsResponse"
    "fixture/DescribeClusterDbRevisionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeClusterDbRevisions)

responseDescribeEventSubscriptions :: DescribeEventSubscriptionsResponse -> TestTree
responseDescribeEventSubscriptions =
  res
    "DescribeEventSubscriptionsResponse"
    "fixture/DescribeEventSubscriptionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEventSubscriptions)

responseResetClusterParameterGroup :: ClusterParameterGroupNameMessage -> TestTree
responseResetClusterParameterGroup =
  res
    "ResetClusterParameterGroupResponse"
    "fixture/ResetClusterParameterGroupResponse.proto"
    defaultService
    (Proxy :: Proxy ResetClusterParameterGroup)

responseModifyClusterSubnetGroup :: ModifyClusterSubnetGroupResponse -> TestTree
responseModifyClusterSubnetGroup =
  res
    "ModifyClusterSubnetGroupResponse"
    "fixture/ModifyClusterSubnetGroupResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyClusterSubnetGroup)

responseRestoreTableFromClusterSnapshot :: RestoreTableFromClusterSnapshotResponse -> TestTree
responseRestoreTableFromClusterSnapshot =
  res
    "RestoreTableFromClusterSnapshotResponse"
    "fixture/RestoreTableFromClusterSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy RestoreTableFromClusterSnapshot)

responseDeleteEventSubscription :: DeleteEventSubscriptionResponse -> TestTree
responseDeleteEventSubscription =
  res
    "DeleteEventSubscriptionResponse"
    "fixture/DeleteEventSubscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteEventSubscription)

responseCreateCluster :: CreateClusterResponse -> TestTree
responseCreateCluster =
  res
    "CreateClusterResponse"
    "fixture/CreateClusterResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCluster)

responseModifyClusterSnapshot :: ModifyClusterSnapshotResponse -> TestTree
responseModifyClusterSnapshot =
  res
    "ModifyClusterSnapshotResponse"
    "fixture/ModifyClusterSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyClusterSnapshot)

responseDeleteScheduledAction :: DeleteScheduledActionResponse -> TestTree
responseDeleteScheduledAction =
  res
    "DeleteScheduledActionResponse"
    "fixture/DeleteScheduledActionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteScheduledAction)

responseDescribeAccountAttributes :: DescribeAccountAttributesResponse -> TestTree
responseDescribeAccountAttributes =
  res
    "DescribeAccountAttributesResponse"
    "fixture/DescribeAccountAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAccountAttributes)

responseCopyClusterSnapshot :: CopyClusterSnapshotResponse -> TestTree
responseCopyClusterSnapshot =
  res
    "CopyClusterSnapshotResponse"
    "fixture/CopyClusterSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy CopyClusterSnapshot)

responseCreateSnapshotCopyGrant :: CreateSnapshotCopyGrantResponse -> TestTree
responseCreateSnapshotCopyGrant =
  res
    "CreateSnapshotCopyGrantResponse"
    "fixture/CreateSnapshotCopyGrantResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSnapshotCopyGrant)

responseModifyClusterSnapshotSchedule :: ModifyClusterSnapshotScheduleResponse -> TestTree
responseModifyClusterSnapshotSchedule =
  res
    "ModifyClusterSnapshotScheduleResponse"
    "fixture/ModifyClusterSnapshotScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyClusterSnapshotSchedule)

responseCreateClusterParameterGroup :: CreateClusterParameterGroupResponse -> TestTree
responseCreateClusterParameterGroup =
  res
    "CreateClusterParameterGroupResponse"
    "fixture/CreateClusterParameterGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateClusterParameterGroup)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags =
  res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTags)

responseEnableSnapshotCopy :: EnableSnapshotCopyResponse -> TestTree
responseEnableSnapshotCopy =
  res
    "EnableSnapshotCopyResponse"
    "fixture/EnableSnapshotCopyResponse.proto"
    defaultService
    (Proxy :: Proxy EnableSnapshotCopy)

responseDescribeClusterSnapshots :: DescribeClusterSnapshotsResponse -> TestTree
responseDescribeClusterSnapshots =
  res
    "DescribeClusterSnapshotsResponse"
    "fixture/DescribeClusterSnapshotsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeClusterSnapshots)

responseDescribeHsmConfigurations :: DescribeHsmConfigurationsResponse -> TestTree
responseDescribeHsmConfigurations =
  res
    "DescribeHsmConfigurationsResponse"
    "fixture/DescribeHsmConfigurationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeHsmConfigurations)

responseModifyUsageLimit :: UsageLimit -> TestTree
responseModifyUsageLimit =
  res
    "ModifyUsageLimitResponse"
    "fixture/ModifyUsageLimitResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyUsageLimit)

responseDescribeClusterSecurityGroups :: DescribeClusterSecurityGroupsResponse -> TestTree
responseDescribeClusterSecurityGroups =
  res
    "DescribeClusterSecurityGroupsResponse"
    "fixture/DescribeClusterSecurityGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeClusterSecurityGroups)

responseDeleteClusterParameterGroup :: DeleteClusterParameterGroupResponse -> TestTree
responseDeleteClusterParameterGroup =
  res
    "DeleteClusterParameterGroupResponse"
    "fixture/DeleteClusterParameterGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteClusterParameterGroup)

responseCreateSnapshotSchedule :: SnapshotSchedule -> TestTree
responseCreateSnapshotSchedule =
  res
    "CreateSnapshotScheduleResponse"
    "fixture/CreateSnapshotScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSnapshotSchedule)

responseDescribeNodeConfigurationOptions :: DescribeNodeConfigurationOptionsResponse -> TestTree
responseDescribeNodeConfigurationOptions =
  res
    "DescribeNodeConfigurationOptionsResponse"
    "fixture/DescribeNodeConfigurationOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeNodeConfigurationOptions)

responseDescribeClusterParameterGroups :: DescribeClusterParameterGroupsResponse -> TestTree
responseDescribeClusterParameterGroups =
  res
    "DescribeClusterParameterGroupsResponse"
    "fixture/DescribeClusterParameterGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeClusterParameterGroups)

responseDescribeLoggingStatus :: LoggingStatus -> TestTree
responseDescribeLoggingStatus =
  res
    "DescribeLoggingStatusResponse"
    "fixture/DescribeLoggingStatusResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLoggingStatus)

responseDescribeEvents :: DescribeEventsResponse -> TestTree
responseDescribeEvents =
  res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEvents)

responseCreateClusterSnapshot :: CreateClusterSnapshotResponse -> TestTree
responseCreateClusterSnapshot =
  res
    "CreateClusterSnapshotResponse"
    "fixture/CreateClusterSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy CreateClusterSnapshot)

responseCreateClusterSubnetGroup :: CreateClusterSubnetGroupResponse -> TestTree
responseCreateClusterSubnetGroup =
  res
    "CreateClusterSubnetGroupResponse"
    "fixture/CreateClusterSubnetGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateClusterSubnetGroup)

responseModifyCluster :: ModifyClusterResponse -> TestTree
responseModifyCluster =
  res
    "ModifyClusterResponse"
    "fixture/ModifyClusterResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyCluster)

responseGetReservedNodeExchangeOfferings :: GetReservedNodeExchangeOfferingsResponse -> TestTree
responseGetReservedNodeExchangeOfferings =
  res
    "GetReservedNodeExchangeOfferingsResponse"
    "fixture/GetReservedNodeExchangeOfferingsResponse.proto"
    defaultService
    (Proxy :: Proxy GetReservedNodeExchangeOfferings)

responseDescribeResize :: ResizeProgressMessage -> TestTree
responseDescribeResize =
  res
    "DescribeResizeResponse"
    "fixture/DescribeResizeResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeResize)

responseDeleteHsmConfiguration :: DeleteHsmConfigurationResponse -> TestTree
responseDeleteHsmConfiguration =
  res
    "DeleteHsmConfigurationResponse"
    "fixture/DeleteHsmConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteHsmConfiguration)

responseDeleteClusterSnapshot :: DeleteClusterSnapshotResponse -> TestTree
responseDeleteClusterSnapshot =
  res
    "DeleteClusterSnapshotResponse"
    "fixture/DeleteClusterSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteClusterSnapshot)

responseCreateClusterSecurityGroup :: CreateClusterSecurityGroupResponse -> TestTree
responseCreateClusterSecurityGroup =
  res
    "CreateClusterSecurityGroupResponse"
    "fixture/CreateClusterSecurityGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateClusterSecurityGroup)

responseAcceptReservedNodeExchange :: AcceptReservedNodeExchangeResponse -> TestTree
responseAcceptReservedNodeExchange =
  res
    "AcceptReservedNodeExchangeResponse"
    "fixture/AcceptReservedNodeExchangeResponse.proto"
    defaultService
    (Proxy :: Proxy AcceptReservedNodeExchange)

responseModifyScheduledAction :: ScheduledAction -> TestTree
responseModifyScheduledAction =
  res
    "ModifyScheduledActionResponse"
    "fixture/ModifyScheduledActionResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyScheduledAction)

responseModifyEventSubscription :: ModifyEventSubscriptionResponse -> TestTree
responseModifyEventSubscription =
  res
    "ModifyEventSubscriptionResponse"
    "fixture/ModifyEventSubscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyEventSubscription)

responseDescribeSnapshotSchedules :: DescribeSnapshotSchedulesResponse -> TestTree
responseDescribeSnapshotSchedules =
  res
    "DescribeSnapshotSchedulesResponse"
    "fixture/DescribeSnapshotSchedulesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSnapshotSchedules)

responseDescribeTableRestoreStatus :: DescribeTableRestoreStatusResponse -> TestTree
responseDescribeTableRestoreStatus =
  res
    "DescribeTableRestoreStatusResponse"
    "fixture/DescribeTableRestoreStatusResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTableRestoreStatus)

responseModifyClusterMaintenance :: ModifyClusterMaintenanceResponse -> TestTree
responseModifyClusterMaintenance =
  res
    "ModifyClusterMaintenanceResponse"
    "fixture/ModifyClusterMaintenanceResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyClusterMaintenance)

responseAuthorizeClusterSecurityGroupIngress :: AuthorizeClusterSecurityGroupIngressResponse -> TestTree
responseAuthorizeClusterSecurityGroupIngress =
  res
    "AuthorizeClusterSecurityGroupIngressResponse"
    "fixture/AuthorizeClusterSecurityGroupIngressResponse.proto"
    defaultService
    (Proxy :: Proxy AuthorizeClusterSecurityGroupIngress)

responseModifyClusterDbRevision :: ModifyClusterDbRevisionResponse -> TestTree
responseModifyClusterDbRevision =
  res
    "ModifyClusterDbRevisionResponse"
    "fixture/ModifyClusterDbRevisionResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyClusterDbRevision)

responseDescribeStorage :: DescribeStorageResponse -> TestTree
responseDescribeStorage =
  res
    "DescribeStorageResponse"
    "fixture/DescribeStorageResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeStorage)

responseDescribeSnapshotCopyGrants :: DescribeSnapshotCopyGrantsResponse -> TestTree
responseDescribeSnapshotCopyGrants =
  res
    "DescribeSnapshotCopyGrantsResponse"
    "fixture/DescribeSnapshotCopyGrantsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSnapshotCopyGrants)

responseBatchModifyClusterSnapshots :: BatchModifyClusterSnapshotsResponse -> TestTree
responseBatchModifyClusterSnapshots =
  res
    "BatchModifyClusterSnapshotsResponse"
    "fixture/BatchModifyClusterSnapshotsResponse.proto"
    defaultService
    (Proxy :: Proxy BatchModifyClusterSnapshots)

responseModifySnapshotSchedule :: SnapshotSchedule -> TestTree
responseModifySnapshotSchedule =
  res
    "ModifySnapshotScheduleResponse"
    "fixture/ModifySnapshotScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy ModifySnapshotSchedule)

responseDeleteSnapshotCopyGrant :: DeleteSnapshotCopyGrantResponse -> TestTree
responseDeleteSnapshotCopyGrant =
  res
    "DeleteSnapshotCopyGrantResponse"
    "fixture/DeleteSnapshotCopyGrantResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSnapshotCopyGrant)

responseDescribeUsageLimits :: DescribeUsageLimitsResponse -> TestTree
responseDescribeUsageLimits =
  res
    "DescribeUsageLimitsResponse"
    "fixture/DescribeUsageLimitsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeUsageLimits)

responseDescribeDefaultClusterParameters :: DescribeDefaultClusterParametersResponse -> TestTree
responseDescribeDefaultClusterParameters =
  res
    "DescribeDefaultClusterParametersResponse"
    "fixture/DescribeDefaultClusterParametersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDefaultClusterParameters)

responseCreateHsmClientCertificate :: CreateHsmClientCertificateResponse -> TestTree
responseCreateHsmClientCertificate =
  res
    "CreateHsmClientCertificateResponse"
    "fixture/CreateHsmClientCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy CreateHsmClientCertificate)

responseDescribeClusterVersions :: DescribeClusterVersionsResponse -> TestTree
responseDescribeClusterVersions =
  res
    "DescribeClusterVersionsResponse"
    "fixture/DescribeClusterVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeClusterVersions)

responseDescribeOrderableClusterOptions :: DescribeOrderableClusterOptionsResponse -> TestTree
responseDescribeOrderableClusterOptions =
  res
    "DescribeOrderableClusterOptionsResponse"
    "fixture/DescribeOrderableClusterOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeOrderableClusterOptions)

responseDeleteHsmClientCertificate :: DeleteHsmClientCertificateResponse -> TestTree
responseDeleteHsmClientCertificate =
  res
    "DeleteHsmClientCertificateResponse"
    "fixture/DeleteHsmClientCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteHsmClientCertificate)

responseRebootCluster :: RebootClusterResponse -> TestTree
responseRebootCluster =
  res
    "RebootClusterResponse"
    "fixture/RebootClusterResponse.proto"
    defaultService
    (Proxy :: Proxy RebootCluster)

responseDeleteCluster :: DeleteClusterResponse -> TestTree
responseDeleteCluster =
  res
    "DeleteClusterResponse"
    "fixture/DeleteClusterResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCluster)

responseCreateEventSubscription :: CreateEventSubscriptionResponse -> TestTree
responseCreateEventSubscription =
  res
    "CreateEventSubscriptionResponse"
    "fixture/CreateEventSubscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateEventSubscription)

responseAuthorizeSnapshotAccess :: AuthorizeSnapshotAccessResponse -> TestTree
responseAuthorizeSnapshotAccess =
  res
    "AuthorizeSnapshotAccessResponse"
    "fixture/AuthorizeSnapshotAccessResponse.proto"
    defaultService
    (Proxy :: Proxy AuthorizeSnapshotAccess)

responseResumeCluster :: ResumeClusterResponse -> TestTree
responseResumeCluster =
  res
    "ResumeClusterResponse"
    "fixture/ResumeClusterResponse.proto"
    defaultService
    (Proxy :: Proxy ResumeCluster)

responseDescribeClusterTracks :: DescribeClusterTracksResponse -> TestTree
responseDescribeClusterTracks =
  res
    "DescribeClusterTracksResponse"
    "fixture/DescribeClusterTracksResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeClusterTracks)

responseCreateScheduledAction :: ScheduledAction -> TestTree
responseCreateScheduledAction =
  res
    "CreateScheduledActionResponse"
    "fixture/CreateScheduledActionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateScheduledAction)

responseDeleteSnapshotSchedule :: DeleteSnapshotScheduleResponse -> TestTree
responseDeleteSnapshotSchedule =
  res
    "DeleteSnapshotScheduleResponse"
    "fixture/DeleteSnapshotScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSnapshotSchedule)

responseDescribeClusterSubnetGroups :: DescribeClusterSubnetGroupsResponse -> TestTree
responseDescribeClusterSubnetGroups =
  res
    "DescribeClusterSubnetGroupsResponse"
    "fixture/DescribeClusterSubnetGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeClusterSubnetGroups)

responseBatchDeleteClusterSnapshots :: BatchDeleteClusterSnapshotsResponse -> TestTree
responseBatchDeleteClusterSnapshots =
  res
    "BatchDeleteClusterSnapshotsResponse"
    "fixture/BatchDeleteClusterSnapshotsResponse.proto"
    defaultService
    (Proxy :: Proxy BatchDeleteClusterSnapshots)

responseModifyClusterIamRoles :: ModifyClusterIamRolesResponse -> TestTree
responseModifyClusterIamRoles =
  res
    "ModifyClusterIamRolesResponse"
    "fixture/ModifyClusterIamRolesResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyClusterIamRoles)

responseResizeCluster :: ResizeClusterResponse -> TestTree
responseResizeCluster =
  res
    "ResizeClusterResponse"
    "fixture/ResizeClusterResponse.proto"
    defaultService
    (Proxy :: Proxy ResizeCluster)

responseModifySnapshotCopyRetentionPeriod :: ModifySnapshotCopyRetentionPeriodResponse -> TestTree
responseModifySnapshotCopyRetentionPeriod =
  res
    "ModifySnapshotCopyRetentionPeriodResponse"
    "fixture/ModifySnapshotCopyRetentionPeriodResponse.proto"
    defaultService
    (Proxy :: Proxy ModifySnapshotCopyRetentionPeriod)

responseRestoreFromClusterSnapshot :: RestoreFromClusterSnapshotResponse -> TestTree
responseRestoreFromClusterSnapshot =
  res
    "RestoreFromClusterSnapshotResponse"
    "fixture/RestoreFromClusterSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy RestoreFromClusterSnapshot)

responseRevokeSnapshotAccess :: RevokeSnapshotAccessResponse -> TestTree
responseRevokeSnapshotAccess =
  res
    "RevokeSnapshotAccessResponse"
    "fixture/RevokeSnapshotAccessResponse.proto"
    defaultService
    (Proxy :: Proxy RevokeSnapshotAccess)

responsePauseCluster :: PauseClusterResponse -> TestTree
responsePauseCluster =
  res
    "PauseClusterResponse"
    "fixture/PauseClusterResponse.proto"
    defaultService
    (Proxy :: Proxy PauseCluster)

responseDescribeClusterParameters :: DescribeClusterParametersResponse -> TestTree
responseDescribeClusterParameters =
  res
    "DescribeClusterParametersResponse"
    "fixture/DescribeClusterParametersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeClusterParameters)

responseDisableSnapshotCopy :: DisableSnapshotCopyResponse -> TestTree
responseDisableSnapshotCopy =
  res
    "DisableSnapshotCopyResponse"
    "fixture/DisableSnapshotCopyResponse.proto"
    defaultService
    (Proxy :: Proxy DisableSnapshotCopy)

responseCreateTags :: CreateTagsResponse -> TestTree
responseCreateTags =
  res
    "CreateTagsResponse"
    "fixture/CreateTagsResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTags)

responseDescribeReservedNodeOfferings :: DescribeReservedNodeOfferingsResponse -> TestTree
responseDescribeReservedNodeOfferings =
  res
    "DescribeReservedNodeOfferingsResponse"
    "fixture/DescribeReservedNodeOfferingsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeReservedNodeOfferings)

responseDeleteClusterSecurityGroup :: DeleteClusterSecurityGroupResponse -> TestTree
responseDeleteClusterSecurityGroup =
  res
    "DeleteClusterSecurityGroupResponse"
    "fixture/DeleteClusterSecurityGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteClusterSecurityGroup)

responseEnableLogging :: LoggingStatus -> TestTree
responseEnableLogging =
  res
    "EnableLoggingResponse"
    "fixture/EnableLoggingResponse.proto"
    defaultService
    (Proxy :: Proxy EnableLogging)

responseCreateHsmConfiguration :: CreateHsmConfigurationResponse -> TestTree
responseCreateHsmConfiguration =
  res
    "CreateHsmConfigurationResponse"
    "fixture/CreateHsmConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateHsmConfiguration)

responseDescribeReservedNodes :: DescribeReservedNodesResponse -> TestTree
responseDescribeReservedNodes =
  res
    "DescribeReservedNodesResponse"
    "fixture/DescribeReservedNodesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeReservedNodes)
