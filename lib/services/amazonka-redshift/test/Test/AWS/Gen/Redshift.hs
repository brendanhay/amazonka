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

import qualified Data.Proxy as Proxy
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
--             newCancelResize
--
--         , requestDescribeStorage $
--             newDescribeStorage
--
--         , requestDescribeClusters $
--             newDescribeClusters
--
--         , requestDescribeTags $
--             newDescribeTags
--
--         , requestCreateUsageLimit $
--             newCreateUsageLimit
--
--         , requestModifyEndpointAccess $
--             newModifyEndpointAccess
--
--         , requestAssociateDataShareConsumer $
--             newAssociateDataShareConsumer
--
--         , requestDeleteClusterSubnetGroup $
--             newDeleteClusterSubnetGroup
--
--         , requestModifyScheduledAction $
--             newModifyScheduledAction
--
--         , requestDisableLogging $
--             newDisableLogging
--
--         , requestDescribeSnapshotSchedules $
--             newDescribeSnapshotSchedules
--
--         , requestRevokeEndpointAccess $
--             newRevokeEndpointAccess
--
--         , requestModifyEventSubscription $
--             newModifyEventSubscription
--
--         , requestModifyClusterDbRevision $
--             newModifyClusterDbRevision
--
--         , requestDeleteClusterSnapshot $
--             newDeleteClusterSnapshot
--
--         , requestAddPartner $
--             newAddPartner
--
--         , requestPurchaseReservedNodeOffering $
--             newPurchaseReservedNodeOffering
--
--         , requestDescribeReservedNodeOfferings $
--             newDescribeReservedNodeOfferings
--
--         , requestDescribeEndpointAccess $
--             newDescribeEndpointAccess
--
--         , requestDescribeEvents $
--             newDescribeEvents
--
--         , requestDescribeReservedNodes $
--             newDescribeReservedNodes
--
--         , requestGetReservedNodeExchangeOfferings $
--             newGetReservedNodeExchangeOfferings
--
--         , requestDeleteAuthenticationProfile $
--             newDeleteAuthenticationProfile
--
--         , requestDescribeClusterParameterGroups $
--             newDescribeClusterParameterGroups
--
--         , requestEnableLogging $
--             newEnableLogging
--
--         , requestCreateClusterSubnetGroup $
--             newCreateClusterSubnetGroup
--
--         , requestDeleteClusterParameterGroup $
--             newDeleteClusterParameterGroup
--
--         , requestDescribeClusterSecurityGroups $
--             newDescribeClusterSecurityGroups
--
--         , requestCreateTags $
--             newCreateTags
--
--         , requestDescribeEndpointAuthorization $
--             newDescribeEndpointAuthorization
--
--         , requestEnableSnapshotCopy $
--             newEnableSnapshotCopy
--
--         , requestDescribeClusterSnapshots $
--             newDescribeClusterSnapshots
--
--         , requestBatchDeleteClusterSnapshots $
--             newBatchDeleteClusterSnapshots
--
--         , requestDeleteTags $
--             newDeleteTags
--
--         , requestModifyUsageLimit $
--             newModifyUsageLimit
--
--         , requestDescribeClusterSubnetGroups $
--             newDescribeClusterSubnetGroups
--
--         , requestResizeCluster $
--             newResizeCluster
--
--         , requestModifySnapshotCopyRetentionPeriod $
--             newModifySnapshotCopyRetentionPeriod
--
--         , requestModifyClusterIamRoles $
--             newModifyClusterIamRoles
--
--         , requestAuthorizeSnapshotAccess $
--             newAuthorizeSnapshotAccess
--
--         , requestRebootCluster $
--             newRebootCluster
--
--         , requestResumeCluster $
--             newResumeCluster
--
--         , requestDeleteCluster $
--             newDeleteCluster
--
--         , requestCreateEventSubscription $
--             newCreateEventSubscription
--
--         , requestCreateScheduledAction $
--             newCreateScheduledAction
--
--         , requestDescribeOrderableClusterOptions $
--             newDescribeOrderableClusterOptions
--
--         , requestCreateEndpointAccess $
--             newCreateEndpointAccess
--
--         , requestDescribeClusterTracks $
--             newDescribeClusterTracks
--
--         , requestCreateCluster $
--             newCreateCluster
--
--         , requestCreateHsmClientCertificate $
--             newCreateHsmClientCertificate
--
--         , requestRestoreTableFromClusterSnapshot $
--             newRestoreTableFromClusterSnapshot
--
--         , requestDeleteScheduledAction $
--             newDeleteScheduledAction
--
--         , requestDescribeDefaultClusterParameters $
--             newDescribeDefaultClusterParameters
--
--         , requestDeleteEventSubscription $
--             newDeleteEventSubscription
--
--         , requestModifyClusterSnapshot $
--             newModifyClusterSnapshot
--
--         , requestDescribeDataSharesForConsumer $
--             newDescribeDataSharesForConsumer
--
--         , requestAuthorizeDataShare $
--             newAuthorizeDataShare
--
--         , requestResetClusterParameterGroup $
--             newResetClusterParameterGroup
--
--         , requestDescribeScheduledActions $
--             newDescribeScheduledActions
--
--         , requestDisassociateDataShareConsumer $
--             newDisassociateDataShareConsumer
--
--         , requestDescribeEventSubscriptions $
--             newDescribeEventSubscriptions
--
--         , requestDescribeClusterDbRevisions $
--             newDescribeClusterDbRevisions
--
--         , requestBatchModifyClusterSnapshots $
--             newBatchModifyClusterSnapshots
--
--         , requestDeleteUsageLimit $
--             newDeleteUsageLimit
--
--         , requestRevokeClusterSecurityGroupIngress $
--             newRevokeClusterSecurityGroupIngress
--
--         , requestDescribeHsmClientCertificates $
--             newDescribeHsmClientCertificates
--
--         , requestModifyClusterParameterGroup $
--             newModifyClusterParameterGroup
--
--         , requestAuthorizeEndpointAccess $
--             newAuthorizeEndpointAccess
--
--         , requestModifyAquaConfiguration $
--             newModifyAquaConfiguration
--
--         , requestGetClusterCredentials $
--             newGetClusterCredentials
--
--         , requestModifyClusterMaintenance $
--             newModifyClusterMaintenance
--
--         , requestCreateClusterSecurityGroup $
--             newCreateClusterSecurityGroup
--
--         , requestDescribeEventCategories $
--             newDescribeEventCategories
--
--         , requestDescribeResize $
--             newDescribeResize
--
--         , requestDeleteHsmConfiguration $
--             newDeleteHsmConfiguration
--
--         , requestCreateAuthenticationProfile $
--             newCreateAuthenticationProfile
--
--         , requestDeauthorizeDataShare $
--             newDeauthorizeDataShare
--
--         , requestAcceptReservedNodeExchange $
--             newAcceptReservedNodeExchange
--
--         , requestAuthorizeClusterSecurityGroupIngress $
--             newAuthorizeClusterSecurityGroupIngress
--
--         , requestDeletePartner $
--             newDeletePartner
--
--         , requestDescribeTableRestoreStatus $
--             newDescribeTableRestoreStatus
--
--         , requestCreateClusterSnapshot $
--             newCreateClusterSnapshot
--
--         , requestRejectDataShare $
--             newRejectDataShare
--
--         , requestCreateHsmConfiguration $
--             newCreateHsmConfiguration
--
--         , requestDescribeLoggingStatus $
--             newDescribeLoggingStatus
--
--         , requestModifyCluster $
--             newModifyCluster
--
--         , requestDeleteClusterSecurityGroup $
--             newDeleteClusterSecurityGroup
--
--         , requestCreateSnapshotSchedule $
--             newCreateSnapshotSchedule
--
--         , requestDescribeAuthenticationProfiles $
--             newDescribeAuthenticationProfiles
--
--         , requestDescribeNodeConfigurationOptions $
--             newDescribeNodeConfigurationOptions
--
--         , requestDisableSnapshotCopy $
--             newDisableSnapshotCopy
--
--         , requestDescribeClusterParameters $
--             newDescribeClusterParameters
--
--         , requestPauseCluster $
--             newPauseCluster
--
--         , requestDescribeDataSharesForProducer $
--             newDescribeDataSharesForProducer
--
--         , requestDeleteSnapshotSchedule $
--             newDeleteSnapshotSchedule
--
--         , requestRestoreFromClusterSnapshot $
--             newRestoreFromClusterSnapshot
--
--         , requestCreateClusterParameterGroup $
--             newCreateClusterParameterGroup
--
--         , requestDescribePartners $
--             newDescribePartners
--
--         , requestRevokeSnapshotAccess $
--             newRevokeSnapshotAccess
--
--         , requestDescribeHsmConfigurations $
--             newDescribeHsmConfigurations
--
--         , requestDescribeAccountAttributes $
--             newDescribeAccountAttributes
--
--         , requestCreateSnapshotCopyGrant $
--             newCreateSnapshotCopyGrant
--
--         , requestCopyClusterSnapshot $
--             newCopyClusterSnapshot
--
--         , requestDescribeDataShares $
--             newDescribeDataShares
--
--         , requestDeleteHsmClientCertificate $
--             newDeleteHsmClientCertificate
--
--         , requestModifyAuthenticationProfile $
--             newModifyAuthenticationProfile
--
--         , requestUpdatePartnerStatus $
--             newUpdatePartnerStatus
--
--         , requestModifyClusterSnapshotSchedule $
--             newModifyClusterSnapshotSchedule
--
--         , requestDeleteEndpointAccess $
--             newDeleteEndpointAccess
--
--         , requestDeleteSnapshotCopyGrant $
--             newDeleteSnapshotCopyGrant
--
--         , requestDescribeClusterVersions $
--             newDescribeClusterVersions
--
--         , requestModifyClusterSubnetGroup $
--             newModifyClusterSubnetGroup
--
--         , requestDescribeUsageLimits $
--             newDescribeUsageLimits
--
--         , requestModifySnapshotSchedule $
--             newModifySnapshotSchedule
--
--         , requestRotateEncryptionKey $
--             newRotateEncryptionKey
--
--         , requestDescribeSnapshotCopyGrants $
--             newDescribeSnapshotCopyGrants
--
--           ]

--     , testGroup "response"
--         [ responseCancelResize $
--             newResizeProgressMessage
--
--         , responseDescribeStorage $
--             newDescribeStorageResponse
--
--         , responseDescribeClusters $
--             newDescribeClustersResponse
--
--         , responseDescribeTags $
--             newDescribeTagsResponse
--
--         , responseCreateUsageLimit $
--             newUsageLimit
--
--         , responseModifyEndpointAccess $
--             newEndpointAccess
--
--         , responseAssociateDataShareConsumer $
--             newDataShare
--
--         , responseDeleteClusterSubnetGroup $
--             newDeleteClusterSubnetGroupResponse
--
--         , responseModifyScheduledAction $
--             newScheduledAction
--
--         , responseDisableLogging $
--             newLoggingStatus
--
--         , responseDescribeSnapshotSchedules $
--             newDescribeSnapshotSchedulesResponse
--
--         , responseRevokeEndpointAccess $
--             newEndpointAuthorization
--
--         , responseModifyEventSubscription $
--             newModifyEventSubscriptionResponse
--
--         , responseModifyClusterDbRevision $
--             newModifyClusterDbRevisionResponse
--
--         , responseDeleteClusterSnapshot $
--             newDeleteClusterSnapshotResponse
--
--         , responseAddPartner $
--             newPartnerIntegrationOutputMessage
--
--         , responsePurchaseReservedNodeOffering $
--             newPurchaseReservedNodeOfferingResponse
--
--         , responseDescribeReservedNodeOfferings $
--             newDescribeReservedNodeOfferingsResponse
--
--         , responseDescribeEndpointAccess $
--             newDescribeEndpointAccessResponse
--
--         , responseDescribeEvents $
--             newDescribeEventsResponse
--
--         , responseDescribeReservedNodes $
--             newDescribeReservedNodesResponse
--
--         , responseGetReservedNodeExchangeOfferings $
--             newGetReservedNodeExchangeOfferingsResponse
--
--         , responseDeleteAuthenticationProfile $
--             newDeleteAuthenticationProfileResponse
--
--         , responseDescribeClusterParameterGroups $
--             newDescribeClusterParameterGroupsResponse
--
--         , responseEnableLogging $
--             newLoggingStatus
--
--         , responseCreateClusterSubnetGroup $
--             newCreateClusterSubnetGroupResponse
--
--         , responseDeleteClusterParameterGroup $
--             newDeleteClusterParameterGroupResponse
--
--         , responseDescribeClusterSecurityGroups $
--             newDescribeClusterSecurityGroupsResponse
--
--         , responseCreateTags $
--             newCreateTagsResponse
--
--         , responseDescribeEndpointAuthorization $
--             newDescribeEndpointAuthorizationResponse
--
--         , responseEnableSnapshotCopy $
--             newEnableSnapshotCopyResponse
--
--         , responseDescribeClusterSnapshots $
--             newDescribeClusterSnapshotsResponse
--
--         , responseBatchDeleteClusterSnapshots $
--             newBatchDeleteClusterSnapshotsResponse
--
--         , responseDeleteTags $
--             newDeleteTagsResponse
--
--         , responseModifyUsageLimit $
--             newUsageLimit
--
--         , responseDescribeClusterSubnetGroups $
--             newDescribeClusterSubnetGroupsResponse
--
--         , responseResizeCluster $
--             newResizeClusterResponse
--
--         , responseModifySnapshotCopyRetentionPeriod $
--             newModifySnapshotCopyRetentionPeriodResponse
--
--         , responseModifyClusterIamRoles $
--             newModifyClusterIamRolesResponse
--
--         , responseAuthorizeSnapshotAccess $
--             newAuthorizeSnapshotAccessResponse
--
--         , responseRebootCluster $
--             newRebootClusterResponse
--
--         , responseResumeCluster $
--             newResumeClusterResponse
--
--         , responseDeleteCluster $
--             newDeleteClusterResponse
--
--         , responseCreateEventSubscription $
--             newCreateEventSubscriptionResponse
--
--         , responseCreateScheduledAction $
--             newScheduledAction
--
--         , responseDescribeOrderableClusterOptions $
--             newDescribeOrderableClusterOptionsResponse
--
--         , responseCreateEndpointAccess $
--             newEndpointAccess
--
--         , responseDescribeClusterTracks $
--             newDescribeClusterTracksResponse
--
--         , responseCreateCluster $
--             newCreateClusterResponse
--
--         , responseCreateHsmClientCertificate $
--             newCreateHsmClientCertificateResponse
--
--         , responseRestoreTableFromClusterSnapshot $
--             newRestoreTableFromClusterSnapshotResponse
--
--         , responseDeleteScheduledAction $
--             newDeleteScheduledActionResponse
--
--         , responseDescribeDefaultClusterParameters $
--             newDescribeDefaultClusterParametersResponse
--
--         , responseDeleteEventSubscription $
--             newDeleteEventSubscriptionResponse
--
--         , responseModifyClusterSnapshot $
--             newModifyClusterSnapshotResponse
--
--         , responseDescribeDataSharesForConsumer $
--             newDescribeDataSharesForConsumerResponse
--
--         , responseAuthorizeDataShare $
--             newDataShare
--
--         , responseResetClusterParameterGroup $
--             newClusterParameterGroupNameMessage
--
--         , responseDescribeScheduledActions $
--             newDescribeScheduledActionsResponse
--
--         , responseDisassociateDataShareConsumer $
--             newDataShare
--
--         , responseDescribeEventSubscriptions $
--             newDescribeEventSubscriptionsResponse
--
--         , responseDescribeClusterDbRevisions $
--             newDescribeClusterDbRevisionsResponse
--
--         , responseBatchModifyClusterSnapshots $
--             newBatchModifyClusterSnapshotsResponse
--
--         , responseDeleteUsageLimit $
--             newDeleteUsageLimitResponse
--
--         , responseRevokeClusterSecurityGroupIngress $
--             newRevokeClusterSecurityGroupIngressResponse
--
--         , responseDescribeHsmClientCertificates $
--             newDescribeHsmClientCertificatesResponse
--
--         , responseModifyClusterParameterGroup $
--             newClusterParameterGroupNameMessage
--
--         , responseAuthorizeEndpointAccess $
--             newEndpointAuthorization
--
--         , responseModifyAquaConfiguration $
--             newModifyAquaConfigurationResponse
--
--         , responseGetClusterCredentials $
--             newGetClusterCredentialsResponse
--
--         , responseModifyClusterMaintenance $
--             newModifyClusterMaintenanceResponse
--
--         , responseCreateClusterSecurityGroup $
--             newCreateClusterSecurityGroupResponse
--
--         , responseDescribeEventCategories $
--             newDescribeEventCategoriesResponse
--
--         , responseDescribeResize $
--             newResizeProgressMessage
--
--         , responseDeleteHsmConfiguration $
--             newDeleteHsmConfigurationResponse
--
--         , responseCreateAuthenticationProfile $
--             newCreateAuthenticationProfileResponse
--
--         , responseDeauthorizeDataShare $
--             newDataShare
--
--         , responseAcceptReservedNodeExchange $
--             newAcceptReservedNodeExchangeResponse
--
--         , responseAuthorizeClusterSecurityGroupIngress $
--             newAuthorizeClusterSecurityGroupIngressResponse
--
--         , responseDeletePartner $
--             newPartnerIntegrationOutputMessage
--
--         , responseDescribeTableRestoreStatus $
--             newDescribeTableRestoreStatusResponse
--
--         , responseCreateClusterSnapshot $
--             newCreateClusterSnapshotResponse
--
--         , responseRejectDataShare $
--             newDataShare
--
--         , responseCreateHsmConfiguration $
--             newCreateHsmConfigurationResponse
--
--         , responseDescribeLoggingStatus $
--             newLoggingStatus
--
--         , responseModifyCluster $
--             newModifyClusterResponse
--
--         , responseDeleteClusterSecurityGroup $
--             newDeleteClusterSecurityGroupResponse
--
--         , responseCreateSnapshotSchedule $
--             newSnapshotSchedule
--
--         , responseDescribeAuthenticationProfiles $
--             newDescribeAuthenticationProfilesResponse
--
--         , responseDescribeNodeConfigurationOptions $
--             newDescribeNodeConfigurationOptionsResponse
--
--         , responseDisableSnapshotCopy $
--             newDisableSnapshotCopyResponse
--
--         , responseDescribeClusterParameters $
--             newDescribeClusterParametersResponse
--
--         , responsePauseCluster $
--             newPauseClusterResponse
--
--         , responseDescribeDataSharesForProducer $
--             newDescribeDataSharesForProducerResponse
--
--         , responseDeleteSnapshotSchedule $
--             newDeleteSnapshotScheduleResponse
--
--         , responseRestoreFromClusterSnapshot $
--             newRestoreFromClusterSnapshotResponse
--
--         , responseCreateClusterParameterGroup $
--             newCreateClusterParameterGroupResponse
--
--         , responseDescribePartners $
--             newDescribePartnersResponse
--
--         , responseRevokeSnapshotAccess $
--             newRevokeSnapshotAccessResponse
--
--         , responseDescribeHsmConfigurations $
--             newDescribeHsmConfigurationsResponse
--
--         , responseDescribeAccountAttributes $
--             newDescribeAccountAttributesResponse
--
--         , responseCreateSnapshotCopyGrant $
--             newCreateSnapshotCopyGrantResponse
--
--         , responseCopyClusterSnapshot $
--             newCopyClusterSnapshotResponse
--
--         , responseDescribeDataShares $
--             newDescribeDataSharesResponse
--
--         , responseDeleteHsmClientCertificate $
--             newDeleteHsmClientCertificateResponse
--
--         , responseModifyAuthenticationProfile $
--             newModifyAuthenticationProfileResponse
--
--         , responseUpdatePartnerStatus $
--             newPartnerIntegrationOutputMessage
--
--         , responseModifyClusterSnapshotSchedule $
--             newModifyClusterSnapshotScheduleResponse
--
--         , responseDeleteEndpointAccess $
--             newEndpointAccess
--
--         , responseDeleteSnapshotCopyGrant $
--             newDeleteSnapshotCopyGrantResponse
--
--         , responseDescribeClusterVersions $
--             newDescribeClusterVersionsResponse
--
--         , responseModifyClusterSubnetGroup $
--             newModifyClusterSubnetGroupResponse
--
--         , responseDescribeUsageLimits $
--             newDescribeUsageLimitsResponse
--
--         , responseModifySnapshotSchedule $
--             newSnapshotSchedule
--
--         , responseRotateEncryptionKey $
--             newRotateEncryptionKeyResponse
--
--         , responseDescribeSnapshotCopyGrants $
--             newDescribeSnapshotCopyGrantsResponse
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

requestModifyEndpointAccess :: ModifyEndpointAccess -> TestTree
requestModifyEndpointAccess =
  req
    "ModifyEndpointAccess"
    "fixture/ModifyEndpointAccess.yaml"

requestAssociateDataShareConsumer :: AssociateDataShareConsumer -> TestTree
requestAssociateDataShareConsumer =
  req
    "AssociateDataShareConsumer"
    "fixture/AssociateDataShareConsumer.yaml"

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

requestRevokeEndpointAccess :: RevokeEndpointAccess -> TestTree
requestRevokeEndpointAccess =
  req
    "RevokeEndpointAccess"
    "fixture/RevokeEndpointAccess.yaml"

requestModifyEventSubscription :: ModifyEventSubscription -> TestTree
requestModifyEventSubscription =
  req
    "ModifyEventSubscription"
    "fixture/ModifyEventSubscription.yaml"

requestModifyClusterDbRevision :: ModifyClusterDbRevision -> TestTree
requestModifyClusterDbRevision =
  req
    "ModifyClusterDbRevision"
    "fixture/ModifyClusterDbRevision.yaml"

requestDeleteClusterSnapshot :: DeleteClusterSnapshot -> TestTree
requestDeleteClusterSnapshot =
  req
    "DeleteClusterSnapshot"
    "fixture/DeleteClusterSnapshot.yaml"

requestAddPartner :: AddPartner -> TestTree
requestAddPartner =
  req
    "AddPartner"
    "fixture/AddPartner.yaml"

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

requestDescribeEndpointAccess :: DescribeEndpointAccess -> TestTree
requestDescribeEndpointAccess =
  req
    "DescribeEndpointAccess"
    "fixture/DescribeEndpointAccess.yaml"

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

requestDeleteAuthenticationProfile :: DeleteAuthenticationProfile -> TestTree
requestDeleteAuthenticationProfile =
  req
    "DeleteAuthenticationProfile"
    "fixture/DeleteAuthenticationProfile.yaml"

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

requestDescribeEndpointAuthorization :: DescribeEndpointAuthorization -> TestTree
requestDescribeEndpointAuthorization =
  req
    "DescribeEndpointAuthorization"
    "fixture/DescribeEndpointAuthorization.yaml"

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

requestModifyClusterIamRoles :: ModifyClusterIamRoles -> TestTree
requestModifyClusterIamRoles =
  req
    "ModifyClusterIamRoles"
    "fixture/ModifyClusterIamRoles.yaml"

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

requestCreateEndpointAccess :: CreateEndpointAccess -> TestTree
requestCreateEndpointAccess =
  req
    "CreateEndpointAccess"
    "fixture/CreateEndpointAccess.yaml"

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

requestCreateHsmClientCertificate :: CreateHsmClientCertificate -> TestTree
requestCreateHsmClientCertificate =
  req
    "CreateHsmClientCertificate"
    "fixture/CreateHsmClientCertificate.yaml"

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

requestDescribeDataSharesForConsumer :: DescribeDataSharesForConsumer -> TestTree
requestDescribeDataSharesForConsumer =
  req
    "DescribeDataSharesForConsumer"
    "fixture/DescribeDataSharesForConsumer.yaml"

requestAuthorizeDataShare :: AuthorizeDataShare -> TestTree
requestAuthorizeDataShare =
  req
    "AuthorizeDataShare"
    "fixture/AuthorizeDataShare.yaml"

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

requestDisassociateDataShareConsumer :: DisassociateDataShareConsumer -> TestTree
requestDisassociateDataShareConsumer =
  req
    "DisassociateDataShareConsumer"
    "fixture/DisassociateDataShareConsumer.yaml"

requestDescribeEventSubscriptions :: DescribeEventSubscriptions -> TestTree
requestDescribeEventSubscriptions =
  req
    "DescribeEventSubscriptions"
    "fixture/DescribeEventSubscriptions.yaml"

requestDescribeClusterDbRevisions :: DescribeClusterDbRevisions -> TestTree
requestDescribeClusterDbRevisions =
  req
    "DescribeClusterDbRevisions"
    "fixture/DescribeClusterDbRevisions.yaml"

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

requestAuthorizeEndpointAccess :: AuthorizeEndpointAccess -> TestTree
requestAuthorizeEndpointAccess =
  req
    "AuthorizeEndpointAccess"
    "fixture/AuthorizeEndpointAccess.yaml"

requestModifyAquaConfiguration :: ModifyAquaConfiguration -> TestTree
requestModifyAquaConfiguration =
  req
    "ModifyAquaConfiguration"
    "fixture/ModifyAquaConfiguration.yaml"

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

requestDeleteHsmConfiguration :: DeleteHsmConfiguration -> TestTree
requestDeleteHsmConfiguration =
  req
    "DeleteHsmConfiguration"
    "fixture/DeleteHsmConfiguration.yaml"

requestCreateAuthenticationProfile :: CreateAuthenticationProfile -> TestTree
requestCreateAuthenticationProfile =
  req
    "CreateAuthenticationProfile"
    "fixture/CreateAuthenticationProfile.yaml"

requestDeauthorizeDataShare :: DeauthorizeDataShare -> TestTree
requestDeauthorizeDataShare =
  req
    "DeauthorizeDataShare"
    "fixture/DeauthorizeDataShare.yaml"

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

requestDeletePartner :: DeletePartner -> TestTree
requestDeletePartner =
  req
    "DeletePartner"
    "fixture/DeletePartner.yaml"

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

requestRejectDataShare :: RejectDataShare -> TestTree
requestRejectDataShare =
  req
    "RejectDataShare"
    "fixture/RejectDataShare.yaml"

requestCreateHsmConfiguration :: CreateHsmConfiguration -> TestTree
requestCreateHsmConfiguration =
  req
    "CreateHsmConfiguration"
    "fixture/CreateHsmConfiguration.yaml"

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

requestDescribeAuthenticationProfiles :: DescribeAuthenticationProfiles -> TestTree
requestDescribeAuthenticationProfiles =
  req
    "DescribeAuthenticationProfiles"
    "fixture/DescribeAuthenticationProfiles.yaml"

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

requestDescribeDataSharesForProducer :: DescribeDataSharesForProducer -> TestTree
requestDescribeDataSharesForProducer =
  req
    "DescribeDataSharesForProducer"
    "fixture/DescribeDataSharesForProducer.yaml"

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

requestDescribePartners :: DescribePartners -> TestTree
requestDescribePartners =
  req
    "DescribePartners"
    "fixture/DescribePartners.yaml"

requestRevokeSnapshotAccess :: RevokeSnapshotAccess -> TestTree
requestRevokeSnapshotAccess =
  req
    "RevokeSnapshotAccess"
    "fixture/RevokeSnapshotAccess.yaml"

requestDescribeHsmConfigurations :: DescribeHsmConfigurations -> TestTree
requestDescribeHsmConfigurations =
  req
    "DescribeHsmConfigurations"
    "fixture/DescribeHsmConfigurations.yaml"

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

requestDescribeDataShares :: DescribeDataShares -> TestTree
requestDescribeDataShares =
  req
    "DescribeDataShares"
    "fixture/DescribeDataShares.yaml"

requestDeleteHsmClientCertificate :: DeleteHsmClientCertificate -> TestTree
requestDeleteHsmClientCertificate =
  req
    "DeleteHsmClientCertificate"
    "fixture/DeleteHsmClientCertificate.yaml"

requestModifyAuthenticationProfile :: ModifyAuthenticationProfile -> TestTree
requestModifyAuthenticationProfile =
  req
    "ModifyAuthenticationProfile"
    "fixture/ModifyAuthenticationProfile.yaml"

requestUpdatePartnerStatus :: UpdatePartnerStatus -> TestTree
requestUpdatePartnerStatus =
  req
    "UpdatePartnerStatus"
    "fixture/UpdatePartnerStatus.yaml"

requestModifyClusterSnapshotSchedule :: ModifyClusterSnapshotSchedule -> TestTree
requestModifyClusterSnapshotSchedule =
  req
    "ModifyClusterSnapshotSchedule"
    "fixture/ModifyClusterSnapshotSchedule.yaml"

requestDeleteEndpointAccess :: DeleteEndpointAccess -> TestTree
requestDeleteEndpointAccess =
  req
    "DeleteEndpointAccess"
    "fixture/DeleteEndpointAccess.yaml"

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
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelResize)

responseDescribeStorage :: DescribeStorageResponse -> TestTree
responseDescribeStorage =
  res
    "DescribeStorageResponse"
    "fixture/DescribeStorageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStorage)

responseDescribeClusters :: DescribeClustersResponse -> TestTree
responseDescribeClusters =
  res
    "DescribeClustersResponse"
    "fixture/DescribeClustersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeClusters)

responseDescribeTags :: DescribeTagsResponse -> TestTree
responseDescribeTags =
  res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTags)

responseCreateUsageLimit :: UsageLimit -> TestTree
responseCreateUsageLimit =
  res
    "CreateUsageLimitResponse"
    "fixture/CreateUsageLimitResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUsageLimit)

responseModifyEndpointAccess :: EndpointAccess -> TestTree
responseModifyEndpointAccess =
  res
    "ModifyEndpointAccessResponse"
    "fixture/ModifyEndpointAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyEndpointAccess)

responseAssociateDataShareConsumer :: DataShare -> TestTree
responseAssociateDataShareConsumer =
  res
    "AssociateDataShareConsumerResponse"
    "fixture/AssociateDataShareConsumerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateDataShareConsumer)

responseDeleteClusterSubnetGroup :: DeleteClusterSubnetGroupResponse -> TestTree
responseDeleteClusterSubnetGroup =
  res
    "DeleteClusterSubnetGroupResponse"
    "fixture/DeleteClusterSubnetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteClusterSubnetGroup)

responseModifyScheduledAction :: ScheduledAction -> TestTree
responseModifyScheduledAction =
  res
    "ModifyScheduledActionResponse"
    "fixture/ModifyScheduledActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyScheduledAction)

responseDisableLogging :: LoggingStatus -> TestTree
responseDisableLogging =
  res
    "DisableLoggingResponse"
    "fixture/DisableLoggingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableLogging)

responseDescribeSnapshotSchedules :: DescribeSnapshotSchedulesResponse -> TestTree
responseDescribeSnapshotSchedules =
  res
    "DescribeSnapshotSchedulesResponse"
    "fixture/DescribeSnapshotSchedulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSnapshotSchedules)

responseRevokeEndpointAccess :: EndpointAuthorization -> TestTree
responseRevokeEndpointAccess =
  res
    "RevokeEndpointAccessResponse"
    "fixture/RevokeEndpointAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RevokeEndpointAccess)

responseModifyEventSubscription :: ModifyEventSubscriptionResponse -> TestTree
responseModifyEventSubscription =
  res
    "ModifyEventSubscriptionResponse"
    "fixture/ModifyEventSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyEventSubscription)

responseModifyClusterDbRevision :: ModifyClusterDbRevisionResponse -> TestTree
responseModifyClusterDbRevision =
  res
    "ModifyClusterDbRevisionResponse"
    "fixture/ModifyClusterDbRevisionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyClusterDbRevision)

responseDeleteClusterSnapshot :: DeleteClusterSnapshotResponse -> TestTree
responseDeleteClusterSnapshot =
  res
    "DeleteClusterSnapshotResponse"
    "fixture/DeleteClusterSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteClusterSnapshot)

responseAddPartner :: PartnerIntegrationOutputMessage -> TestTree
responseAddPartner =
  res
    "AddPartnerResponse"
    "fixture/AddPartnerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddPartner)

responsePurchaseReservedNodeOffering :: PurchaseReservedNodeOfferingResponse -> TestTree
responsePurchaseReservedNodeOffering =
  res
    "PurchaseReservedNodeOfferingResponse"
    "fixture/PurchaseReservedNodeOfferingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PurchaseReservedNodeOffering)

responseDescribeReservedNodeOfferings :: DescribeReservedNodeOfferingsResponse -> TestTree
responseDescribeReservedNodeOfferings =
  res
    "DescribeReservedNodeOfferingsResponse"
    "fixture/DescribeReservedNodeOfferingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReservedNodeOfferings)

responseDescribeEndpointAccess :: DescribeEndpointAccessResponse -> TestTree
responseDescribeEndpointAccess =
  res
    "DescribeEndpointAccessResponse"
    "fixture/DescribeEndpointAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEndpointAccess)

responseDescribeEvents :: DescribeEventsResponse -> TestTree
responseDescribeEvents =
  res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEvents)

responseDescribeReservedNodes :: DescribeReservedNodesResponse -> TestTree
responseDescribeReservedNodes =
  res
    "DescribeReservedNodesResponse"
    "fixture/DescribeReservedNodesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReservedNodes)

responseGetReservedNodeExchangeOfferings :: GetReservedNodeExchangeOfferingsResponse -> TestTree
responseGetReservedNodeExchangeOfferings =
  res
    "GetReservedNodeExchangeOfferingsResponse"
    "fixture/GetReservedNodeExchangeOfferingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetReservedNodeExchangeOfferings)

responseDeleteAuthenticationProfile :: DeleteAuthenticationProfileResponse -> TestTree
responseDeleteAuthenticationProfile =
  res
    "DeleteAuthenticationProfileResponse"
    "fixture/DeleteAuthenticationProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAuthenticationProfile)

responseDescribeClusterParameterGroups :: DescribeClusterParameterGroupsResponse -> TestTree
responseDescribeClusterParameterGroups =
  res
    "DescribeClusterParameterGroupsResponse"
    "fixture/DescribeClusterParameterGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeClusterParameterGroups)

responseEnableLogging :: LoggingStatus -> TestTree
responseEnableLogging =
  res
    "EnableLoggingResponse"
    "fixture/EnableLoggingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableLogging)

responseCreateClusterSubnetGroup :: CreateClusterSubnetGroupResponse -> TestTree
responseCreateClusterSubnetGroup =
  res
    "CreateClusterSubnetGroupResponse"
    "fixture/CreateClusterSubnetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateClusterSubnetGroup)

responseDeleteClusterParameterGroup :: DeleteClusterParameterGroupResponse -> TestTree
responseDeleteClusterParameterGroup =
  res
    "DeleteClusterParameterGroupResponse"
    "fixture/DeleteClusterParameterGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteClusterParameterGroup)

responseDescribeClusterSecurityGroups :: DescribeClusterSecurityGroupsResponse -> TestTree
responseDescribeClusterSecurityGroups =
  res
    "DescribeClusterSecurityGroupsResponse"
    "fixture/DescribeClusterSecurityGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeClusterSecurityGroups)

responseCreateTags :: CreateTagsResponse -> TestTree
responseCreateTags =
  res
    "CreateTagsResponse"
    "fixture/CreateTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTags)

responseDescribeEndpointAuthorization :: DescribeEndpointAuthorizationResponse -> TestTree
responseDescribeEndpointAuthorization =
  res
    "DescribeEndpointAuthorizationResponse"
    "fixture/DescribeEndpointAuthorizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEndpointAuthorization)

responseEnableSnapshotCopy :: EnableSnapshotCopyResponse -> TestTree
responseEnableSnapshotCopy =
  res
    "EnableSnapshotCopyResponse"
    "fixture/EnableSnapshotCopyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableSnapshotCopy)

responseDescribeClusterSnapshots :: DescribeClusterSnapshotsResponse -> TestTree
responseDescribeClusterSnapshots =
  res
    "DescribeClusterSnapshotsResponse"
    "fixture/DescribeClusterSnapshotsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeClusterSnapshots)

responseBatchDeleteClusterSnapshots :: BatchDeleteClusterSnapshotsResponse -> TestTree
responseBatchDeleteClusterSnapshots =
  res
    "BatchDeleteClusterSnapshotsResponse"
    "fixture/BatchDeleteClusterSnapshotsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDeleteClusterSnapshots)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags =
  res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTags)

responseModifyUsageLimit :: UsageLimit -> TestTree
responseModifyUsageLimit =
  res
    "ModifyUsageLimitResponse"
    "fixture/ModifyUsageLimitResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyUsageLimit)

responseDescribeClusterSubnetGroups :: DescribeClusterSubnetGroupsResponse -> TestTree
responseDescribeClusterSubnetGroups =
  res
    "DescribeClusterSubnetGroupsResponse"
    "fixture/DescribeClusterSubnetGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeClusterSubnetGroups)

responseResizeCluster :: ResizeClusterResponse -> TestTree
responseResizeCluster =
  res
    "ResizeClusterResponse"
    "fixture/ResizeClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResizeCluster)

responseModifySnapshotCopyRetentionPeriod :: ModifySnapshotCopyRetentionPeriodResponse -> TestTree
responseModifySnapshotCopyRetentionPeriod =
  res
    "ModifySnapshotCopyRetentionPeriodResponse"
    "fixture/ModifySnapshotCopyRetentionPeriodResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifySnapshotCopyRetentionPeriod)

responseModifyClusterIamRoles :: ModifyClusterIamRolesResponse -> TestTree
responseModifyClusterIamRoles =
  res
    "ModifyClusterIamRolesResponse"
    "fixture/ModifyClusterIamRolesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyClusterIamRoles)

responseAuthorizeSnapshotAccess :: AuthorizeSnapshotAccessResponse -> TestTree
responseAuthorizeSnapshotAccess =
  res
    "AuthorizeSnapshotAccessResponse"
    "fixture/AuthorizeSnapshotAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AuthorizeSnapshotAccess)

responseRebootCluster :: RebootClusterResponse -> TestTree
responseRebootCluster =
  res
    "RebootClusterResponse"
    "fixture/RebootClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RebootCluster)

responseResumeCluster :: ResumeClusterResponse -> TestTree
responseResumeCluster =
  res
    "ResumeClusterResponse"
    "fixture/ResumeClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResumeCluster)

responseDeleteCluster :: DeleteClusterResponse -> TestTree
responseDeleteCluster =
  res
    "DeleteClusterResponse"
    "fixture/DeleteClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCluster)

responseCreateEventSubscription :: CreateEventSubscriptionResponse -> TestTree
responseCreateEventSubscription =
  res
    "CreateEventSubscriptionResponse"
    "fixture/CreateEventSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEventSubscription)

responseCreateScheduledAction :: ScheduledAction -> TestTree
responseCreateScheduledAction =
  res
    "CreateScheduledActionResponse"
    "fixture/CreateScheduledActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateScheduledAction)

responseDescribeOrderableClusterOptions :: DescribeOrderableClusterOptionsResponse -> TestTree
responseDescribeOrderableClusterOptions =
  res
    "DescribeOrderableClusterOptionsResponse"
    "fixture/DescribeOrderableClusterOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeOrderableClusterOptions)

responseCreateEndpointAccess :: EndpointAccess -> TestTree
responseCreateEndpointAccess =
  res
    "CreateEndpointAccessResponse"
    "fixture/CreateEndpointAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEndpointAccess)

responseDescribeClusterTracks :: DescribeClusterTracksResponse -> TestTree
responseDescribeClusterTracks =
  res
    "DescribeClusterTracksResponse"
    "fixture/DescribeClusterTracksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeClusterTracks)

responseCreateCluster :: CreateClusterResponse -> TestTree
responseCreateCluster =
  res
    "CreateClusterResponse"
    "fixture/CreateClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCluster)

responseCreateHsmClientCertificate :: CreateHsmClientCertificateResponse -> TestTree
responseCreateHsmClientCertificate =
  res
    "CreateHsmClientCertificateResponse"
    "fixture/CreateHsmClientCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateHsmClientCertificate)

responseRestoreTableFromClusterSnapshot :: RestoreTableFromClusterSnapshotResponse -> TestTree
responseRestoreTableFromClusterSnapshot =
  res
    "RestoreTableFromClusterSnapshotResponse"
    "fixture/RestoreTableFromClusterSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestoreTableFromClusterSnapshot)

responseDeleteScheduledAction :: DeleteScheduledActionResponse -> TestTree
responseDeleteScheduledAction =
  res
    "DeleteScheduledActionResponse"
    "fixture/DeleteScheduledActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteScheduledAction)

responseDescribeDefaultClusterParameters :: DescribeDefaultClusterParametersResponse -> TestTree
responseDescribeDefaultClusterParameters =
  res
    "DescribeDefaultClusterParametersResponse"
    "fixture/DescribeDefaultClusterParametersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDefaultClusterParameters)

responseDeleteEventSubscription :: DeleteEventSubscriptionResponse -> TestTree
responseDeleteEventSubscription =
  res
    "DeleteEventSubscriptionResponse"
    "fixture/DeleteEventSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEventSubscription)

responseModifyClusterSnapshot :: ModifyClusterSnapshotResponse -> TestTree
responseModifyClusterSnapshot =
  res
    "ModifyClusterSnapshotResponse"
    "fixture/ModifyClusterSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyClusterSnapshot)

responseDescribeDataSharesForConsumer :: DescribeDataSharesForConsumerResponse -> TestTree
responseDescribeDataSharesForConsumer =
  res
    "DescribeDataSharesForConsumerResponse"
    "fixture/DescribeDataSharesForConsumerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDataSharesForConsumer)

responseAuthorizeDataShare :: DataShare -> TestTree
responseAuthorizeDataShare =
  res
    "AuthorizeDataShareResponse"
    "fixture/AuthorizeDataShareResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AuthorizeDataShare)

responseResetClusterParameterGroup :: ClusterParameterGroupNameMessage -> TestTree
responseResetClusterParameterGroup =
  res
    "ResetClusterParameterGroupResponse"
    "fixture/ResetClusterParameterGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResetClusterParameterGroup)

responseDescribeScheduledActions :: DescribeScheduledActionsResponse -> TestTree
responseDescribeScheduledActions =
  res
    "DescribeScheduledActionsResponse"
    "fixture/DescribeScheduledActionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeScheduledActions)

responseDisassociateDataShareConsumer :: DataShare -> TestTree
responseDisassociateDataShareConsumer =
  res
    "DisassociateDataShareConsumerResponse"
    "fixture/DisassociateDataShareConsumerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateDataShareConsumer)

responseDescribeEventSubscriptions :: DescribeEventSubscriptionsResponse -> TestTree
responseDescribeEventSubscriptions =
  res
    "DescribeEventSubscriptionsResponse"
    "fixture/DescribeEventSubscriptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEventSubscriptions)

responseDescribeClusterDbRevisions :: DescribeClusterDbRevisionsResponse -> TestTree
responseDescribeClusterDbRevisions =
  res
    "DescribeClusterDbRevisionsResponse"
    "fixture/DescribeClusterDbRevisionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeClusterDbRevisions)

responseBatchModifyClusterSnapshots :: BatchModifyClusterSnapshotsResponse -> TestTree
responseBatchModifyClusterSnapshots =
  res
    "BatchModifyClusterSnapshotsResponse"
    "fixture/BatchModifyClusterSnapshotsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchModifyClusterSnapshots)

responseDeleteUsageLimit :: DeleteUsageLimitResponse -> TestTree
responseDeleteUsageLimit =
  res
    "DeleteUsageLimitResponse"
    "fixture/DeleteUsageLimitResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUsageLimit)

responseRevokeClusterSecurityGroupIngress :: RevokeClusterSecurityGroupIngressResponse -> TestTree
responseRevokeClusterSecurityGroupIngress =
  res
    "RevokeClusterSecurityGroupIngressResponse"
    "fixture/RevokeClusterSecurityGroupIngressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RevokeClusterSecurityGroupIngress)

responseDescribeHsmClientCertificates :: DescribeHsmClientCertificatesResponse -> TestTree
responseDescribeHsmClientCertificates =
  res
    "DescribeHsmClientCertificatesResponse"
    "fixture/DescribeHsmClientCertificatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeHsmClientCertificates)

responseModifyClusterParameterGroup :: ClusterParameterGroupNameMessage -> TestTree
responseModifyClusterParameterGroup =
  res
    "ModifyClusterParameterGroupResponse"
    "fixture/ModifyClusterParameterGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyClusterParameterGroup)

responseAuthorizeEndpointAccess :: EndpointAuthorization -> TestTree
responseAuthorizeEndpointAccess =
  res
    "AuthorizeEndpointAccessResponse"
    "fixture/AuthorizeEndpointAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AuthorizeEndpointAccess)

responseModifyAquaConfiguration :: ModifyAquaConfigurationResponse -> TestTree
responseModifyAquaConfiguration =
  res
    "ModifyAquaConfigurationResponse"
    "fixture/ModifyAquaConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyAquaConfiguration)

responseGetClusterCredentials :: GetClusterCredentialsResponse -> TestTree
responseGetClusterCredentials =
  res
    "GetClusterCredentialsResponse"
    "fixture/GetClusterCredentialsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetClusterCredentials)

responseModifyClusterMaintenance :: ModifyClusterMaintenanceResponse -> TestTree
responseModifyClusterMaintenance =
  res
    "ModifyClusterMaintenanceResponse"
    "fixture/ModifyClusterMaintenanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyClusterMaintenance)

responseCreateClusterSecurityGroup :: CreateClusterSecurityGroupResponse -> TestTree
responseCreateClusterSecurityGroup =
  res
    "CreateClusterSecurityGroupResponse"
    "fixture/CreateClusterSecurityGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateClusterSecurityGroup)

responseDescribeEventCategories :: DescribeEventCategoriesResponse -> TestTree
responseDescribeEventCategories =
  res
    "DescribeEventCategoriesResponse"
    "fixture/DescribeEventCategoriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEventCategories)

responseDescribeResize :: ResizeProgressMessage -> TestTree
responseDescribeResize =
  res
    "DescribeResizeResponse"
    "fixture/DescribeResizeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeResize)

responseDeleteHsmConfiguration :: DeleteHsmConfigurationResponse -> TestTree
responseDeleteHsmConfiguration =
  res
    "DeleteHsmConfigurationResponse"
    "fixture/DeleteHsmConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteHsmConfiguration)

responseCreateAuthenticationProfile :: CreateAuthenticationProfileResponse -> TestTree
responseCreateAuthenticationProfile =
  res
    "CreateAuthenticationProfileResponse"
    "fixture/CreateAuthenticationProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAuthenticationProfile)

responseDeauthorizeDataShare :: DataShare -> TestTree
responseDeauthorizeDataShare =
  res
    "DeauthorizeDataShareResponse"
    "fixture/DeauthorizeDataShareResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeauthorizeDataShare)

responseAcceptReservedNodeExchange :: AcceptReservedNodeExchangeResponse -> TestTree
responseAcceptReservedNodeExchange =
  res
    "AcceptReservedNodeExchangeResponse"
    "fixture/AcceptReservedNodeExchangeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptReservedNodeExchange)

responseAuthorizeClusterSecurityGroupIngress :: AuthorizeClusterSecurityGroupIngressResponse -> TestTree
responseAuthorizeClusterSecurityGroupIngress =
  res
    "AuthorizeClusterSecurityGroupIngressResponse"
    "fixture/AuthorizeClusterSecurityGroupIngressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AuthorizeClusterSecurityGroupIngress)

responseDeletePartner :: PartnerIntegrationOutputMessage -> TestTree
responseDeletePartner =
  res
    "DeletePartnerResponse"
    "fixture/DeletePartnerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePartner)

responseDescribeTableRestoreStatus :: DescribeTableRestoreStatusResponse -> TestTree
responseDescribeTableRestoreStatus =
  res
    "DescribeTableRestoreStatusResponse"
    "fixture/DescribeTableRestoreStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTableRestoreStatus)

responseCreateClusterSnapshot :: CreateClusterSnapshotResponse -> TestTree
responseCreateClusterSnapshot =
  res
    "CreateClusterSnapshotResponse"
    "fixture/CreateClusterSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateClusterSnapshot)

responseRejectDataShare :: DataShare -> TestTree
responseRejectDataShare =
  res
    "RejectDataShareResponse"
    "fixture/RejectDataShareResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RejectDataShare)

responseCreateHsmConfiguration :: CreateHsmConfigurationResponse -> TestTree
responseCreateHsmConfiguration =
  res
    "CreateHsmConfigurationResponse"
    "fixture/CreateHsmConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateHsmConfiguration)

responseDescribeLoggingStatus :: LoggingStatus -> TestTree
responseDescribeLoggingStatus =
  res
    "DescribeLoggingStatusResponse"
    "fixture/DescribeLoggingStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLoggingStatus)

responseModifyCluster :: ModifyClusterResponse -> TestTree
responseModifyCluster =
  res
    "ModifyClusterResponse"
    "fixture/ModifyClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyCluster)

responseDeleteClusterSecurityGroup :: DeleteClusterSecurityGroupResponse -> TestTree
responseDeleteClusterSecurityGroup =
  res
    "DeleteClusterSecurityGroupResponse"
    "fixture/DeleteClusterSecurityGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteClusterSecurityGroup)

responseCreateSnapshotSchedule :: SnapshotSchedule -> TestTree
responseCreateSnapshotSchedule =
  res
    "CreateSnapshotScheduleResponse"
    "fixture/CreateSnapshotScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSnapshotSchedule)

responseDescribeAuthenticationProfiles :: DescribeAuthenticationProfilesResponse -> TestTree
responseDescribeAuthenticationProfiles =
  res
    "DescribeAuthenticationProfilesResponse"
    "fixture/DescribeAuthenticationProfilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAuthenticationProfiles)

responseDescribeNodeConfigurationOptions :: DescribeNodeConfigurationOptionsResponse -> TestTree
responseDescribeNodeConfigurationOptions =
  res
    "DescribeNodeConfigurationOptionsResponse"
    "fixture/DescribeNodeConfigurationOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeNodeConfigurationOptions)

responseDisableSnapshotCopy :: DisableSnapshotCopyResponse -> TestTree
responseDisableSnapshotCopy =
  res
    "DisableSnapshotCopyResponse"
    "fixture/DisableSnapshotCopyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableSnapshotCopy)

responseDescribeClusterParameters :: DescribeClusterParametersResponse -> TestTree
responseDescribeClusterParameters =
  res
    "DescribeClusterParametersResponse"
    "fixture/DescribeClusterParametersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeClusterParameters)

responsePauseCluster :: PauseClusterResponse -> TestTree
responsePauseCluster =
  res
    "PauseClusterResponse"
    "fixture/PauseClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PauseCluster)

responseDescribeDataSharesForProducer :: DescribeDataSharesForProducerResponse -> TestTree
responseDescribeDataSharesForProducer =
  res
    "DescribeDataSharesForProducerResponse"
    "fixture/DescribeDataSharesForProducerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDataSharesForProducer)

responseDeleteSnapshotSchedule :: DeleteSnapshotScheduleResponse -> TestTree
responseDeleteSnapshotSchedule =
  res
    "DeleteSnapshotScheduleResponse"
    "fixture/DeleteSnapshotScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSnapshotSchedule)

responseRestoreFromClusterSnapshot :: RestoreFromClusterSnapshotResponse -> TestTree
responseRestoreFromClusterSnapshot =
  res
    "RestoreFromClusterSnapshotResponse"
    "fixture/RestoreFromClusterSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestoreFromClusterSnapshot)

responseCreateClusterParameterGroup :: CreateClusterParameterGroupResponse -> TestTree
responseCreateClusterParameterGroup =
  res
    "CreateClusterParameterGroupResponse"
    "fixture/CreateClusterParameterGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateClusterParameterGroup)

responseDescribePartners :: DescribePartnersResponse -> TestTree
responseDescribePartners =
  res
    "DescribePartnersResponse"
    "fixture/DescribePartnersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePartners)

responseRevokeSnapshotAccess :: RevokeSnapshotAccessResponse -> TestTree
responseRevokeSnapshotAccess =
  res
    "RevokeSnapshotAccessResponse"
    "fixture/RevokeSnapshotAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RevokeSnapshotAccess)

responseDescribeHsmConfigurations :: DescribeHsmConfigurationsResponse -> TestTree
responseDescribeHsmConfigurations =
  res
    "DescribeHsmConfigurationsResponse"
    "fixture/DescribeHsmConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeHsmConfigurations)

responseDescribeAccountAttributes :: DescribeAccountAttributesResponse -> TestTree
responseDescribeAccountAttributes =
  res
    "DescribeAccountAttributesResponse"
    "fixture/DescribeAccountAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAccountAttributes)

responseCreateSnapshotCopyGrant :: CreateSnapshotCopyGrantResponse -> TestTree
responseCreateSnapshotCopyGrant =
  res
    "CreateSnapshotCopyGrantResponse"
    "fixture/CreateSnapshotCopyGrantResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSnapshotCopyGrant)

responseCopyClusterSnapshot :: CopyClusterSnapshotResponse -> TestTree
responseCopyClusterSnapshot =
  res
    "CopyClusterSnapshotResponse"
    "fixture/CopyClusterSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CopyClusterSnapshot)

responseDescribeDataShares :: DescribeDataSharesResponse -> TestTree
responseDescribeDataShares =
  res
    "DescribeDataSharesResponse"
    "fixture/DescribeDataSharesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDataShares)

responseDeleteHsmClientCertificate :: DeleteHsmClientCertificateResponse -> TestTree
responseDeleteHsmClientCertificate =
  res
    "DeleteHsmClientCertificateResponse"
    "fixture/DeleteHsmClientCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteHsmClientCertificate)

responseModifyAuthenticationProfile :: ModifyAuthenticationProfileResponse -> TestTree
responseModifyAuthenticationProfile =
  res
    "ModifyAuthenticationProfileResponse"
    "fixture/ModifyAuthenticationProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyAuthenticationProfile)

responseUpdatePartnerStatus :: PartnerIntegrationOutputMessage -> TestTree
responseUpdatePartnerStatus =
  res
    "UpdatePartnerStatusResponse"
    "fixture/UpdatePartnerStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePartnerStatus)

responseModifyClusterSnapshotSchedule :: ModifyClusterSnapshotScheduleResponse -> TestTree
responseModifyClusterSnapshotSchedule =
  res
    "ModifyClusterSnapshotScheduleResponse"
    "fixture/ModifyClusterSnapshotScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyClusterSnapshotSchedule)

responseDeleteEndpointAccess :: EndpointAccess -> TestTree
responseDeleteEndpointAccess =
  res
    "DeleteEndpointAccessResponse"
    "fixture/DeleteEndpointAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEndpointAccess)

responseDeleteSnapshotCopyGrant :: DeleteSnapshotCopyGrantResponse -> TestTree
responseDeleteSnapshotCopyGrant =
  res
    "DeleteSnapshotCopyGrantResponse"
    "fixture/DeleteSnapshotCopyGrantResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSnapshotCopyGrant)

responseDescribeClusterVersions :: DescribeClusterVersionsResponse -> TestTree
responseDescribeClusterVersions =
  res
    "DescribeClusterVersionsResponse"
    "fixture/DescribeClusterVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeClusterVersions)

responseModifyClusterSubnetGroup :: ModifyClusterSubnetGroupResponse -> TestTree
responseModifyClusterSubnetGroup =
  res
    "ModifyClusterSubnetGroupResponse"
    "fixture/ModifyClusterSubnetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyClusterSubnetGroup)

responseDescribeUsageLimits :: DescribeUsageLimitsResponse -> TestTree
responseDescribeUsageLimits =
  res
    "DescribeUsageLimitsResponse"
    "fixture/DescribeUsageLimitsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeUsageLimits)

responseModifySnapshotSchedule :: SnapshotSchedule -> TestTree
responseModifySnapshotSchedule =
  res
    "ModifySnapshotScheduleResponse"
    "fixture/ModifySnapshotScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifySnapshotSchedule)

responseRotateEncryptionKey :: RotateEncryptionKeyResponse -> TestTree
responseRotateEncryptionKey =
  res
    "RotateEncryptionKeyResponse"
    "fixture/RotateEncryptionKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RotateEncryptionKey)

responseDescribeSnapshotCopyGrants :: DescribeSnapshotCopyGrantsResponse -> TestTree
responseDescribeSnapshotCopyGrants =
  res
    "DescribeSnapshotCopyGrantsResponse"
    "fixture/DescribeSnapshotCopyGrantsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSnapshotCopyGrants)
