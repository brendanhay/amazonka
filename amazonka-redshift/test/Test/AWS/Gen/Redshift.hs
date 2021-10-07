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
--         [ requestDisableLogging $
--             newDisableLogging
--
--         , requestAddPartner $
--             newAddPartner
--
--         , requestPurchaseReservedNodeOffering $
--             newPurchaseReservedNodeOffering
--
--         , requestDescribeEventCategories $
--             newDescribeEventCategories
--
--         , requestModifyEndpointAccess $
--             newModifyEndpointAccess
--
--         , requestDeleteClusterSubnetGroup $
--             newDeleteClusterSubnetGroup
--
--         , requestAssociateDataShareConsumer $
--             newAssociateDataShareConsumer
--
--         , requestRevokeEndpointAccess $
--             newRevokeEndpointAccess
--
--         , requestDeletePartner $
--             newDeletePartner
--
--         , requestCreateAuthenticationProfile $
--             newCreateAuthenticationProfile
--
--         , requestModifyAquaConfiguration $
--             newModifyAquaConfiguration
--
--         , requestCreateUsageLimit $
--             newCreateUsageLimit
--
--         , requestAuthorizeEndpointAccess $
--             newAuthorizeEndpointAccess
--
--         , requestDescribeHsmClientCertificates $
--             newDescribeHsmClientCertificates
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
--         , requestDescribeTags $
--             newDescribeTags
--
--         , requestModifyClusterParameterGroup $
--             newModifyClusterParameterGroup
--
--         , requestRevokeClusterSecurityGroupIngress $
--             newRevokeClusterSecurityGroupIngress
--
--         , requestResetClusterParameterGroup $
--             newResetClusterParameterGroup
--
--         , requestDeleteUsageLimit $
--             newDeleteUsageLimit
--
--         , requestDescribeClusterDbRevisions $
--             newDescribeClusterDbRevisions
--
--         , requestRotateEncryptionKey $
--             newRotateEncryptionKey
--
--         , requestDescribeScheduledActions $
--             newDescribeScheduledActions
--
--         , requestDescribeEventSubscriptions $
--             newDescribeEventSubscriptions
--
--         , requestDeleteEventSubscription $
--             newDeleteEventSubscription
--
--         , requestDescribeDataSharesForConsumer $
--             newDescribeDataSharesForConsumer
--
--         , requestModifyClusterSnapshot $
--             newModifyClusterSnapshot
--
--         , requestModifyClusterSubnetGroup $
--             newModifyClusterSubnetGroup
--
--         , requestDeleteScheduledAction $
--             newDeleteScheduledAction
--
--         , requestDeleteEndpointAccess $
--             newDeleteEndpointAccess
--
--         , requestRestoreTableFromClusterSnapshot $
--             newRestoreTableFromClusterSnapshot
--
--         , requestCreateCluster $
--             newCreateCluster
--
--         , requestCreateEndpointAccess $
--             newCreateEndpointAccess
--
--         , requestModifyClusterSnapshotSchedule $
--             newModifyClusterSnapshotSchedule
--
--         , requestDescribeAccountAttributes $
--             newDescribeAccountAttributes
--
--         , requestModifyAuthenticationProfile $
--             newModifyAuthenticationProfile
--
--         , requestCopyClusterSnapshot $
--             newCopyClusterSnapshot
--
--         , requestCreateSnapshotCopyGrant $
--             newCreateSnapshotCopyGrant
--
--         , requestUpdatePartnerStatus $
--             newUpdatePartnerStatus
--
--         , requestDescribeDataSharesForProducer $
--             newDescribeDataSharesForProducer
--
--         , requestDescribeHsmConfigurations $
--             newDescribeHsmConfigurations
--
--         , requestDescribeClusterSnapshots $
--             newDescribeClusterSnapshots
--
--         , requestDeleteTags $
--             newDeleteTags
--
--         , requestEnableSnapshotCopy $
--             newEnableSnapshotCopy
--
--         , requestModifyUsageLimit $
--             newModifyUsageLimit
--
--         , requestCreateClusterParameterGroup $
--             newCreateClusterParameterGroup
--
--         , requestCreateSnapshotSchedule $
--             newCreateSnapshotSchedule
--
--         , requestDescribeEndpointAuthorization $
--             newDescribeEndpointAuthorization
--
--         , requestDeleteClusterParameterGroup $
--             newDeleteClusterParameterGroup
--
--         , requestDescribeClusterSecurityGroups $
--             newDescribeClusterSecurityGroups
--
--         , requestDescribeNodeConfigurationOptions $
--             newDescribeNodeConfigurationOptions
--
--         , requestDescribeAuthenticationProfiles $
--             newDescribeAuthenticationProfiles
--
--         , requestDescribeEndpointAccess $
--             newDescribeEndpointAccess
--
--         , requestDescribeEvents $
--             newDescribeEvents
--
--         , requestCreateClusterSnapshot $
--             newCreateClusterSnapshot
--
--         , requestDescribeLoggingStatus $
--             newDescribeLoggingStatus
--
--         , requestDescribeClusterParameterGroups $
--             newDescribeClusterParameterGroups
--
--         , requestModifyCluster $
--             newModifyCluster
--
--         , requestGetReservedNodeExchangeOfferings $
--             newGetReservedNodeExchangeOfferings
--
--         , requestRejectDataShare $
--             newRejectDataShare
--
--         , requestCreateClusterSubnetGroup $
--             newCreateClusterSubnetGroup
--
--         , requestDeleteHsmConfiguration $
--             newDeleteHsmConfiguration
--
--         , requestDescribeTableRestoreStatus $
--             newDescribeTableRestoreStatus
--
--         , requestDeleteClusterSnapshot $
--             newDeleteClusterSnapshot
--
--         , requestModifyClusterDbRevision $
--             newModifyClusterDbRevision
--
--         , requestAuthorizeClusterSecurityGroupIngress $
--             newAuthorizeClusterSecurityGroupIngress
--
--         , requestDeauthorizeDataShare $
--             newDeauthorizeDataShare
--
--         , requestModifyScheduledAction $
--             newModifyScheduledAction
--
--         , requestModifyEventSubscription $
--             newModifyEventSubscription
--
--         , requestCreateClusterSecurityGroup $
--             newCreateClusterSecurityGroup
--
--         , requestDescribeResize $
--             newDescribeResize
--
--         , requestAcceptReservedNodeExchange $
--             newAcceptReservedNodeExchange
--
--         , requestDescribeSnapshotSchedules $
--             newDescribeSnapshotSchedules
--
--         , requestModifyClusterMaintenance $
--             newModifyClusterMaintenance
--
--         , requestDescribeStorage $
--             newDescribeStorage
--
--         , requestDisassociateDataShareConsumer $
--             newDisassociateDataShareConsumer
--
--         , requestBatchModifyClusterSnapshots $
--             newBatchModifyClusterSnapshots
--
--         , requestDescribeSnapshotCopyGrants $
--             newDescribeSnapshotCopyGrants
--
--         , requestModifySnapshotSchedule $
--             newModifySnapshotSchedule
--
--         , requestCreateHsmClientCertificate $
--             newCreateHsmClientCertificate
--
--         , requestDescribeClusterVersions $
--             newDescribeClusterVersions
--
--         , requestAuthorizeDataShare $
--             newAuthorizeDataShare
--
--         , requestDescribeDefaultClusterParameters $
--             newDescribeDefaultClusterParameters
--
--         , requestDeleteSnapshotCopyGrant $
--             newDeleteSnapshotCopyGrant
--
--         , requestDescribeUsageLimits $
--             newDescribeUsageLimits
--
--         , requestDescribeClusterTracks $
--             newDescribeClusterTracks
--
--         , requestDescribeOrderableClusterOptions $
--             newDescribeOrderableClusterOptions
--
--         , requestCreateEventSubscription $
--             newCreateEventSubscription
--
--         , requestCreateScheduledAction $
--             newCreateScheduledAction
--
--         , requestAuthorizeSnapshotAccess $
--             newAuthorizeSnapshotAccess
--
--         , requestDeleteHsmClientCertificate $
--             newDeleteHsmClientCertificate
--
--         , requestDeleteCluster $
--             newDeleteCluster
--
--         , requestDescribeDataShares $
--             newDescribeDataShares
--
--         , requestRebootCluster $
--             newRebootCluster
--
--         , requestResumeCluster $
--             newResumeCluster
--
--         , requestModifyClusterIamRoles $
--             newModifyClusterIamRoles
--
--         , requestRestoreFromClusterSnapshot $
--             newRestoreFromClusterSnapshot
--
--         , requestPauseCluster $
--             newPauseCluster
--
--         , requestDeleteSnapshotSchedule $
--             newDeleteSnapshotSchedule
--
--         , requestModifySnapshotCopyRetentionPeriod $
--             newModifySnapshotCopyRetentionPeriod
--
--         , requestDescribeClusterSubnetGroups $
--             newDescribeClusterSubnetGroups
--
--         , requestResizeCluster $
--             newResizeCluster
--
--         , requestBatchDeleteClusterSnapshots $
--             newBatchDeleteClusterSnapshots
--
--         , requestDescribePartners $
--             newDescribePartners
--
--         , requestRevokeSnapshotAccess $
--             newRevokeSnapshotAccess
--
--         , requestCreateTags $
--             newCreateTags
--
--         , requestDisableSnapshotCopy $
--             newDisableSnapshotCopy
--
--         , requestDescribeClusterParameters $
--             newDescribeClusterParameters
--
--         , requestDeleteAuthenticationProfile $
--             newDeleteAuthenticationProfile
--
--         , requestDeleteClusterSecurityGroup $
--             newDeleteClusterSecurityGroup
--
--         , requestDescribeReservedNodeOfferings $
--             newDescribeReservedNodeOfferings
--
--         , requestEnableLogging $
--             newEnableLogging
--
--         , requestDescribeReservedNodes $
--             newDescribeReservedNodes
--
--         , requestCreateHsmConfiguration $
--             newCreateHsmConfiguration
--
--           ]

--     , testGroup "response"
--         [ responseDisableLogging $
--             newLoggingStatus
--
--         , responseAddPartner $
--             newPartnerIntegrationOutputMessage
--
--         , responsePurchaseReservedNodeOffering $
--             newPurchaseReservedNodeOfferingResponse
--
--         , responseDescribeEventCategories $
--             newDescribeEventCategoriesResponse
--
--         , responseModifyEndpointAccess $
--             newEndpointAccess
--
--         , responseDeleteClusterSubnetGroup $
--             newDeleteClusterSubnetGroupResponse
--
--         , responseAssociateDataShareConsumer $
--             newDataShare
--
--         , responseRevokeEndpointAccess $
--             newEndpointAuthorization
--
--         , responseDeletePartner $
--             newPartnerIntegrationOutputMessage
--
--         , responseCreateAuthenticationProfile $
--             newCreateAuthenticationProfileResponse
--
--         , responseModifyAquaConfiguration $
--             newModifyAquaConfigurationResponse
--
--         , responseCreateUsageLimit $
--             newUsageLimit
--
--         , responseAuthorizeEndpointAccess $
--             newEndpointAuthorization
--
--         , responseDescribeHsmClientCertificates $
--             newDescribeHsmClientCertificatesResponse
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
--         , responseDescribeTags $
--             newDescribeTagsResponse
--
--         , responseModifyClusterParameterGroup $
--             newClusterParameterGroupNameMessage
--
--         , responseRevokeClusterSecurityGroupIngress $
--             newRevokeClusterSecurityGroupIngressResponse
--
--         , responseResetClusterParameterGroup $
--             newClusterParameterGroupNameMessage
--
--         , responseDeleteUsageLimit $
--             newDeleteUsageLimitResponse
--
--         , responseDescribeClusterDbRevisions $
--             newDescribeClusterDbRevisionsResponse
--
--         , responseRotateEncryptionKey $
--             newRotateEncryptionKeyResponse
--
--         , responseDescribeScheduledActions $
--             newDescribeScheduledActionsResponse
--
--         , responseDescribeEventSubscriptions $
--             newDescribeEventSubscriptionsResponse
--
--         , responseDeleteEventSubscription $
--             newDeleteEventSubscriptionResponse
--
--         , responseDescribeDataSharesForConsumer $
--             newDescribeDataSharesForConsumerResponse
--
--         , responseModifyClusterSnapshot $
--             newModifyClusterSnapshotResponse
--
--         , responseModifyClusterSubnetGroup $
--             newModifyClusterSubnetGroupResponse
--
--         , responseDeleteScheduledAction $
--             newDeleteScheduledActionResponse
--
--         , responseDeleteEndpointAccess $
--             newEndpointAccess
--
--         , responseRestoreTableFromClusterSnapshot $
--             newRestoreTableFromClusterSnapshotResponse
--
--         , responseCreateCluster $
--             newCreateClusterResponse
--
--         , responseCreateEndpointAccess $
--             newEndpointAccess
--
--         , responseModifyClusterSnapshotSchedule $
--             newModifyClusterSnapshotScheduleResponse
--
--         , responseDescribeAccountAttributes $
--             newDescribeAccountAttributesResponse
--
--         , responseModifyAuthenticationProfile $
--             newModifyAuthenticationProfileResponse
--
--         , responseCopyClusterSnapshot $
--             newCopyClusterSnapshotResponse
--
--         , responseCreateSnapshotCopyGrant $
--             newCreateSnapshotCopyGrantResponse
--
--         , responseUpdatePartnerStatus $
--             newPartnerIntegrationOutputMessage
--
--         , responseDescribeDataSharesForProducer $
--             newDescribeDataSharesForProducerResponse
--
--         , responseDescribeHsmConfigurations $
--             newDescribeHsmConfigurationsResponse
--
--         , responseDescribeClusterSnapshots $
--             newDescribeClusterSnapshotsResponse
--
--         , responseDeleteTags $
--             newDeleteTagsResponse
--
--         , responseEnableSnapshotCopy $
--             newEnableSnapshotCopyResponse
--
--         , responseModifyUsageLimit $
--             newUsageLimit
--
--         , responseCreateClusterParameterGroup $
--             newCreateClusterParameterGroupResponse
--
--         , responseCreateSnapshotSchedule $
--             newSnapshotSchedule
--
--         , responseDescribeEndpointAuthorization $
--             newDescribeEndpointAuthorizationResponse
--
--         , responseDeleteClusterParameterGroup $
--             newDeleteClusterParameterGroupResponse
--
--         , responseDescribeClusterSecurityGroups $
--             newDescribeClusterSecurityGroupsResponse
--
--         , responseDescribeNodeConfigurationOptions $
--             newDescribeNodeConfigurationOptionsResponse
--
--         , responseDescribeAuthenticationProfiles $
--             newDescribeAuthenticationProfilesResponse
--
--         , responseDescribeEndpointAccess $
--             newDescribeEndpointAccessResponse
--
--         , responseDescribeEvents $
--             newDescribeEventsResponse
--
--         , responseCreateClusterSnapshot $
--             newCreateClusterSnapshotResponse
--
--         , responseDescribeLoggingStatus $
--             newLoggingStatus
--
--         , responseDescribeClusterParameterGroups $
--             newDescribeClusterParameterGroupsResponse
--
--         , responseModifyCluster $
--             newModifyClusterResponse
--
--         , responseGetReservedNodeExchangeOfferings $
--             newGetReservedNodeExchangeOfferingsResponse
--
--         , responseRejectDataShare $
--             newDataShare
--
--         , responseCreateClusterSubnetGroup $
--             newCreateClusterSubnetGroupResponse
--
--         , responseDeleteHsmConfiguration $
--             newDeleteHsmConfigurationResponse
--
--         , responseDescribeTableRestoreStatus $
--             newDescribeTableRestoreStatusResponse
--
--         , responseDeleteClusterSnapshot $
--             newDeleteClusterSnapshotResponse
--
--         , responseModifyClusterDbRevision $
--             newModifyClusterDbRevisionResponse
--
--         , responseAuthorizeClusterSecurityGroupIngress $
--             newAuthorizeClusterSecurityGroupIngressResponse
--
--         , responseDeauthorizeDataShare $
--             newDataShare
--
--         , responseModifyScheduledAction $
--             newScheduledAction
--
--         , responseModifyEventSubscription $
--             newModifyEventSubscriptionResponse
--
--         , responseCreateClusterSecurityGroup $
--             newCreateClusterSecurityGroupResponse
--
--         , responseDescribeResize $
--             newResizeProgressMessage
--
--         , responseAcceptReservedNodeExchange $
--             newAcceptReservedNodeExchangeResponse
--
--         , responseDescribeSnapshotSchedules $
--             newDescribeSnapshotSchedulesResponse
--
--         , responseModifyClusterMaintenance $
--             newModifyClusterMaintenanceResponse
--
--         , responseDescribeStorage $
--             newDescribeStorageResponse
--
--         , responseDisassociateDataShareConsumer $
--             newDataShare
--
--         , responseBatchModifyClusterSnapshots $
--             newBatchModifyClusterSnapshotsResponse
--
--         , responseDescribeSnapshotCopyGrants $
--             newDescribeSnapshotCopyGrantsResponse
--
--         , responseModifySnapshotSchedule $
--             newSnapshotSchedule
--
--         , responseCreateHsmClientCertificate $
--             newCreateHsmClientCertificateResponse
--
--         , responseDescribeClusterVersions $
--             newDescribeClusterVersionsResponse
--
--         , responseAuthorizeDataShare $
--             newDataShare
--
--         , responseDescribeDefaultClusterParameters $
--             newDescribeDefaultClusterParametersResponse
--
--         , responseDeleteSnapshotCopyGrant $
--             newDeleteSnapshotCopyGrantResponse
--
--         , responseDescribeUsageLimits $
--             newDescribeUsageLimitsResponse
--
--         , responseDescribeClusterTracks $
--             newDescribeClusterTracksResponse
--
--         , responseDescribeOrderableClusterOptions $
--             newDescribeOrderableClusterOptionsResponse
--
--         , responseCreateEventSubscription $
--             newCreateEventSubscriptionResponse
--
--         , responseCreateScheduledAction $
--             newScheduledAction
--
--         , responseAuthorizeSnapshotAccess $
--             newAuthorizeSnapshotAccessResponse
--
--         , responseDeleteHsmClientCertificate $
--             newDeleteHsmClientCertificateResponse
--
--         , responseDeleteCluster $
--             newDeleteClusterResponse
--
--         , responseDescribeDataShares $
--             newDescribeDataSharesResponse
--
--         , responseRebootCluster $
--             newRebootClusterResponse
--
--         , responseResumeCluster $
--             newResumeClusterResponse
--
--         , responseModifyClusterIamRoles $
--             newModifyClusterIamRolesResponse
--
--         , responseRestoreFromClusterSnapshot $
--             newRestoreFromClusterSnapshotResponse
--
--         , responsePauseCluster $
--             newPauseClusterResponse
--
--         , responseDeleteSnapshotSchedule $
--             newDeleteSnapshotScheduleResponse
--
--         , responseModifySnapshotCopyRetentionPeriod $
--             newModifySnapshotCopyRetentionPeriodResponse
--
--         , responseDescribeClusterSubnetGroups $
--             newDescribeClusterSubnetGroupsResponse
--
--         , responseResizeCluster $
--             newResizeClusterResponse
--
--         , responseBatchDeleteClusterSnapshots $
--             newBatchDeleteClusterSnapshotsResponse
--
--         , responseDescribePartners $
--             newDescribePartnersResponse
--
--         , responseRevokeSnapshotAccess $
--             newRevokeSnapshotAccessResponse
--
--         , responseCreateTags $
--             newCreateTagsResponse
--
--         , responseDisableSnapshotCopy $
--             newDisableSnapshotCopyResponse
--
--         , responseDescribeClusterParameters $
--             newDescribeClusterParametersResponse
--
--         , responseDeleteAuthenticationProfile $
--             newDeleteAuthenticationProfileResponse
--
--         , responseDeleteClusterSecurityGroup $
--             newDeleteClusterSecurityGroupResponse
--
--         , responseDescribeReservedNodeOfferings $
--             newDescribeReservedNodeOfferingsResponse
--
--         , responseEnableLogging $
--             newLoggingStatus
--
--         , responseDescribeReservedNodes $
--             newDescribeReservedNodesResponse
--
--         , responseCreateHsmConfiguration $
--             newCreateHsmConfigurationResponse
--
--           ]
--     ]

-- Requests

requestDisableLogging :: DisableLogging -> TestTree
requestDisableLogging =
  req
    "DisableLogging"
    "fixture/DisableLogging.yaml"

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

requestDescribeEventCategories :: DescribeEventCategories -> TestTree
requestDescribeEventCategories =
  req
    "DescribeEventCategories"
    "fixture/DescribeEventCategories.yaml"

requestModifyEndpointAccess :: ModifyEndpointAccess -> TestTree
requestModifyEndpointAccess =
  req
    "ModifyEndpointAccess"
    "fixture/ModifyEndpointAccess.yaml"

requestDeleteClusterSubnetGroup :: DeleteClusterSubnetGroup -> TestTree
requestDeleteClusterSubnetGroup =
  req
    "DeleteClusterSubnetGroup"
    "fixture/DeleteClusterSubnetGroup.yaml"

requestAssociateDataShareConsumer :: AssociateDataShareConsumer -> TestTree
requestAssociateDataShareConsumer =
  req
    "AssociateDataShareConsumer"
    "fixture/AssociateDataShareConsumer.yaml"

requestRevokeEndpointAccess :: RevokeEndpointAccess -> TestTree
requestRevokeEndpointAccess =
  req
    "RevokeEndpointAccess"
    "fixture/RevokeEndpointAccess.yaml"

requestDeletePartner :: DeletePartner -> TestTree
requestDeletePartner =
  req
    "DeletePartner"
    "fixture/DeletePartner.yaml"

requestCreateAuthenticationProfile :: CreateAuthenticationProfile -> TestTree
requestCreateAuthenticationProfile =
  req
    "CreateAuthenticationProfile"
    "fixture/CreateAuthenticationProfile.yaml"

requestModifyAquaConfiguration :: ModifyAquaConfiguration -> TestTree
requestModifyAquaConfiguration =
  req
    "ModifyAquaConfiguration"
    "fixture/ModifyAquaConfiguration.yaml"

requestCreateUsageLimit :: CreateUsageLimit -> TestTree
requestCreateUsageLimit =
  req
    "CreateUsageLimit"
    "fixture/CreateUsageLimit.yaml"

requestAuthorizeEndpointAccess :: AuthorizeEndpointAccess -> TestTree
requestAuthorizeEndpointAccess =
  req
    "AuthorizeEndpointAccess"
    "fixture/AuthorizeEndpointAccess.yaml"

requestDescribeHsmClientCertificates :: DescribeHsmClientCertificates -> TestTree
requestDescribeHsmClientCertificates =
  req
    "DescribeHsmClientCertificates"
    "fixture/DescribeHsmClientCertificates.yaml"

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

requestDescribeTags :: DescribeTags -> TestTree
requestDescribeTags =
  req
    "DescribeTags"
    "fixture/DescribeTags.yaml"

requestModifyClusterParameterGroup :: ModifyClusterParameterGroup -> TestTree
requestModifyClusterParameterGroup =
  req
    "ModifyClusterParameterGroup"
    "fixture/ModifyClusterParameterGroup.yaml"

requestRevokeClusterSecurityGroupIngress :: RevokeClusterSecurityGroupIngress -> TestTree
requestRevokeClusterSecurityGroupIngress =
  req
    "RevokeClusterSecurityGroupIngress"
    "fixture/RevokeClusterSecurityGroupIngress.yaml"

requestResetClusterParameterGroup :: ResetClusterParameterGroup -> TestTree
requestResetClusterParameterGroup =
  req
    "ResetClusterParameterGroup"
    "fixture/ResetClusterParameterGroup.yaml"

requestDeleteUsageLimit :: DeleteUsageLimit -> TestTree
requestDeleteUsageLimit =
  req
    "DeleteUsageLimit"
    "fixture/DeleteUsageLimit.yaml"

requestDescribeClusterDbRevisions :: DescribeClusterDbRevisions -> TestTree
requestDescribeClusterDbRevisions =
  req
    "DescribeClusterDbRevisions"
    "fixture/DescribeClusterDbRevisions.yaml"

requestRotateEncryptionKey :: RotateEncryptionKey -> TestTree
requestRotateEncryptionKey =
  req
    "RotateEncryptionKey"
    "fixture/RotateEncryptionKey.yaml"

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

requestDeleteEventSubscription :: DeleteEventSubscription -> TestTree
requestDeleteEventSubscription =
  req
    "DeleteEventSubscription"
    "fixture/DeleteEventSubscription.yaml"

requestDescribeDataSharesForConsumer :: DescribeDataSharesForConsumer -> TestTree
requestDescribeDataSharesForConsumer =
  req
    "DescribeDataSharesForConsumer"
    "fixture/DescribeDataSharesForConsumer.yaml"

requestModifyClusterSnapshot :: ModifyClusterSnapshot -> TestTree
requestModifyClusterSnapshot =
  req
    "ModifyClusterSnapshot"
    "fixture/ModifyClusterSnapshot.yaml"

requestModifyClusterSubnetGroup :: ModifyClusterSubnetGroup -> TestTree
requestModifyClusterSubnetGroup =
  req
    "ModifyClusterSubnetGroup"
    "fixture/ModifyClusterSubnetGroup.yaml"

requestDeleteScheduledAction :: DeleteScheduledAction -> TestTree
requestDeleteScheduledAction =
  req
    "DeleteScheduledAction"
    "fixture/DeleteScheduledAction.yaml"

requestDeleteEndpointAccess :: DeleteEndpointAccess -> TestTree
requestDeleteEndpointAccess =
  req
    "DeleteEndpointAccess"
    "fixture/DeleteEndpointAccess.yaml"

requestRestoreTableFromClusterSnapshot :: RestoreTableFromClusterSnapshot -> TestTree
requestRestoreTableFromClusterSnapshot =
  req
    "RestoreTableFromClusterSnapshot"
    "fixture/RestoreTableFromClusterSnapshot.yaml"

requestCreateCluster :: CreateCluster -> TestTree
requestCreateCluster =
  req
    "CreateCluster"
    "fixture/CreateCluster.yaml"

requestCreateEndpointAccess :: CreateEndpointAccess -> TestTree
requestCreateEndpointAccess =
  req
    "CreateEndpointAccess"
    "fixture/CreateEndpointAccess.yaml"

requestModifyClusterSnapshotSchedule :: ModifyClusterSnapshotSchedule -> TestTree
requestModifyClusterSnapshotSchedule =
  req
    "ModifyClusterSnapshotSchedule"
    "fixture/ModifyClusterSnapshotSchedule.yaml"

requestDescribeAccountAttributes :: DescribeAccountAttributes -> TestTree
requestDescribeAccountAttributes =
  req
    "DescribeAccountAttributes"
    "fixture/DescribeAccountAttributes.yaml"

requestModifyAuthenticationProfile :: ModifyAuthenticationProfile -> TestTree
requestModifyAuthenticationProfile =
  req
    "ModifyAuthenticationProfile"
    "fixture/ModifyAuthenticationProfile.yaml"

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

requestUpdatePartnerStatus :: UpdatePartnerStatus -> TestTree
requestUpdatePartnerStatus =
  req
    "UpdatePartnerStatus"
    "fixture/UpdatePartnerStatus.yaml"

requestDescribeDataSharesForProducer :: DescribeDataSharesForProducer -> TestTree
requestDescribeDataSharesForProducer =
  req
    "DescribeDataSharesForProducer"
    "fixture/DescribeDataSharesForProducer.yaml"

requestDescribeHsmConfigurations :: DescribeHsmConfigurations -> TestTree
requestDescribeHsmConfigurations =
  req
    "DescribeHsmConfigurations"
    "fixture/DescribeHsmConfigurations.yaml"

requestDescribeClusterSnapshots :: DescribeClusterSnapshots -> TestTree
requestDescribeClusterSnapshots =
  req
    "DescribeClusterSnapshots"
    "fixture/DescribeClusterSnapshots.yaml"

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

requestModifyUsageLimit :: ModifyUsageLimit -> TestTree
requestModifyUsageLimit =
  req
    "ModifyUsageLimit"
    "fixture/ModifyUsageLimit.yaml"

requestCreateClusterParameterGroup :: CreateClusterParameterGroup -> TestTree
requestCreateClusterParameterGroup =
  req
    "CreateClusterParameterGroup"
    "fixture/CreateClusterParameterGroup.yaml"

requestCreateSnapshotSchedule :: CreateSnapshotSchedule -> TestTree
requestCreateSnapshotSchedule =
  req
    "CreateSnapshotSchedule"
    "fixture/CreateSnapshotSchedule.yaml"

requestDescribeEndpointAuthorization :: DescribeEndpointAuthorization -> TestTree
requestDescribeEndpointAuthorization =
  req
    "DescribeEndpointAuthorization"
    "fixture/DescribeEndpointAuthorization.yaml"

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

requestDescribeNodeConfigurationOptions :: DescribeNodeConfigurationOptions -> TestTree
requestDescribeNodeConfigurationOptions =
  req
    "DescribeNodeConfigurationOptions"
    "fixture/DescribeNodeConfigurationOptions.yaml"

requestDescribeAuthenticationProfiles :: DescribeAuthenticationProfiles -> TestTree
requestDescribeAuthenticationProfiles =
  req
    "DescribeAuthenticationProfiles"
    "fixture/DescribeAuthenticationProfiles.yaml"

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

requestCreateClusterSnapshot :: CreateClusterSnapshot -> TestTree
requestCreateClusterSnapshot =
  req
    "CreateClusterSnapshot"
    "fixture/CreateClusterSnapshot.yaml"

requestDescribeLoggingStatus :: DescribeLoggingStatus -> TestTree
requestDescribeLoggingStatus =
  req
    "DescribeLoggingStatus"
    "fixture/DescribeLoggingStatus.yaml"

requestDescribeClusterParameterGroups :: DescribeClusterParameterGroups -> TestTree
requestDescribeClusterParameterGroups =
  req
    "DescribeClusterParameterGroups"
    "fixture/DescribeClusterParameterGroups.yaml"

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

requestRejectDataShare :: RejectDataShare -> TestTree
requestRejectDataShare =
  req
    "RejectDataShare"
    "fixture/RejectDataShare.yaml"

requestCreateClusterSubnetGroup :: CreateClusterSubnetGroup -> TestTree
requestCreateClusterSubnetGroup =
  req
    "CreateClusterSubnetGroup"
    "fixture/CreateClusterSubnetGroup.yaml"

requestDeleteHsmConfiguration :: DeleteHsmConfiguration -> TestTree
requestDeleteHsmConfiguration =
  req
    "DeleteHsmConfiguration"
    "fixture/DeleteHsmConfiguration.yaml"

requestDescribeTableRestoreStatus :: DescribeTableRestoreStatus -> TestTree
requestDescribeTableRestoreStatus =
  req
    "DescribeTableRestoreStatus"
    "fixture/DescribeTableRestoreStatus.yaml"

requestDeleteClusterSnapshot :: DeleteClusterSnapshot -> TestTree
requestDeleteClusterSnapshot =
  req
    "DeleteClusterSnapshot"
    "fixture/DeleteClusterSnapshot.yaml"

requestModifyClusterDbRevision :: ModifyClusterDbRevision -> TestTree
requestModifyClusterDbRevision =
  req
    "ModifyClusterDbRevision"
    "fixture/ModifyClusterDbRevision.yaml"

requestAuthorizeClusterSecurityGroupIngress :: AuthorizeClusterSecurityGroupIngress -> TestTree
requestAuthorizeClusterSecurityGroupIngress =
  req
    "AuthorizeClusterSecurityGroupIngress"
    "fixture/AuthorizeClusterSecurityGroupIngress.yaml"

requestDeauthorizeDataShare :: DeauthorizeDataShare -> TestTree
requestDeauthorizeDataShare =
  req
    "DeauthorizeDataShare"
    "fixture/DeauthorizeDataShare.yaml"

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

requestCreateClusterSecurityGroup :: CreateClusterSecurityGroup -> TestTree
requestCreateClusterSecurityGroup =
  req
    "CreateClusterSecurityGroup"
    "fixture/CreateClusterSecurityGroup.yaml"

requestDescribeResize :: DescribeResize -> TestTree
requestDescribeResize =
  req
    "DescribeResize"
    "fixture/DescribeResize.yaml"

requestAcceptReservedNodeExchange :: AcceptReservedNodeExchange -> TestTree
requestAcceptReservedNodeExchange =
  req
    "AcceptReservedNodeExchange"
    "fixture/AcceptReservedNodeExchange.yaml"

requestDescribeSnapshotSchedules :: DescribeSnapshotSchedules -> TestTree
requestDescribeSnapshotSchedules =
  req
    "DescribeSnapshotSchedules"
    "fixture/DescribeSnapshotSchedules.yaml"

requestModifyClusterMaintenance :: ModifyClusterMaintenance -> TestTree
requestModifyClusterMaintenance =
  req
    "ModifyClusterMaintenance"
    "fixture/ModifyClusterMaintenance.yaml"

requestDescribeStorage :: DescribeStorage -> TestTree
requestDescribeStorage =
  req
    "DescribeStorage"
    "fixture/DescribeStorage.yaml"

requestDisassociateDataShareConsumer :: DisassociateDataShareConsumer -> TestTree
requestDisassociateDataShareConsumer =
  req
    "DisassociateDataShareConsumer"
    "fixture/DisassociateDataShareConsumer.yaml"

requestBatchModifyClusterSnapshots :: BatchModifyClusterSnapshots -> TestTree
requestBatchModifyClusterSnapshots =
  req
    "BatchModifyClusterSnapshots"
    "fixture/BatchModifyClusterSnapshots.yaml"

requestDescribeSnapshotCopyGrants :: DescribeSnapshotCopyGrants -> TestTree
requestDescribeSnapshotCopyGrants =
  req
    "DescribeSnapshotCopyGrants"
    "fixture/DescribeSnapshotCopyGrants.yaml"

requestModifySnapshotSchedule :: ModifySnapshotSchedule -> TestTree
requestModifySnapshotSchedule =
  req
    "ModifySnapshotSchedule"
    "fixture/ModifySnapshotSchedule.yaml"

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

requestAuthorizeDataShare :: AuthorizeDataShare -> TestTree
requestAuthorizeDataShare =
  req
    "AuthorizeDataShare"
    "fixture/AuthorizeDataShare.yaml"

requestDescribeDefaultClusterParameters :: DescribeDefaultClusterParameters -> TestTree
requestDescribeDefaultClusterParameters =
  req
    "DescribeDefaultClusterParameters"
    "fixture/DescribeDefaultClusterParameters.yaml"

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

requestDescribeClusterTracks :: DescribeClusterTracks -> TestTree
requestDescribeClusterTracks =
  req
    "DescribeClusterTracks"
    "fixture/DescribeClusterTracks.yaml"

requestDescribeOrderableClusterOptions :: DescribeOrderableClusterOptions -> TestTree
requestDescribeOrderableClusterOptions =
  req
    "DescribeOrderableClusterOptions"
    "fixture/DescribeOrderableClusterOptions.yaml"

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

requestAuthorizeSnapshotAccess :: AuthorizeSnapshotAccess -> TestTree
requestAuthorizeSnapshotAccess =
  req
    "AuthorizeSnapshotAccess"
    "fixture/AuthorizeSnapshotAccess.yaml"

requestDeleteHsmClientCertificate :: DeleteHsmClientCertificate -> TestTree
requestDeleteHsmClientCertificate =
  req
    "DeleteHsmClientCertificate"
    "fixture/DeleteHsmClientCertificate.yaml"

requestDeleteCluster :: DeleteCluster -> TestTree
requestDeleteCluster =
  req
    "DeleteCluster"
    "fixture/DeleteCluster.yaml"

requestDescribeDataShares :: DescribeDataShares -> TestTree
requestDescribeDataShares =
  req
    "DescribeDataShares"
    "fixture/DescribeDataShares.yaml"

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

requestModifyClusterIamRoles :: ModifyClusterIamRoles -> TestTree
requestModifyClusterIamRoles =
  req
    "ModifyClusterIamRoles"
    "fixture/ModifyClusterIamRoles.yaml"

requestRestoreFromClusterSnapshot :: RestoreFromClusterSnapshot -> TestTree
requestRestoreFromClusterSnapshot =
  req
    "RestoreFromClusterSnapshot"
    "fixture/RestoreFromClusterSnapshot.yaml"

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

requestModifySnapshotCopyRetentionPeriod :: ModifySnapshotCopyRetentionPeriod -> TestTree
requestModifySnapshotCopyRetentionPeriod =
  req
    "ModifySnapshotCopyRetentionPeriod"
    "fixture/ModifySnapshotCopyRetentionPeriod.yaml"

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

requestBatchDeleteClusterSnapshots :: BatchDeleteClusterSnapshots -> TestTree
requestBatchDeleteClusterSnapshots =
  req
    "BatchDeleteClusterSnapshots"
    "fixture/BatchDeleteClusterSnapshots.yaml"

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

requestCreateTags :: CreateTags -> TestTree
requestCreateTags =
  req
    "CreateTags"
    "fixture/CreateTags.yaml"

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

requestDeleteAuthenticationProfile :: DeleteAuthenticationProfile -> TestTree
requestDeleteAuthenticationProfile =
  req
    "DeleteAuthenticationProfile"
    "fixture/DeleteAuthenticationProfile.yaml"

requestDeleteClusterSecurityGroup :: DeleteClusterSecurityGroup -> TestTree
requestDeleteClusterSecurityGroup =
  req
    "DeleteClusterSecurityGroup"
    "fixture/DeleteClusterSecurityGroup.yaml"

requestDescribeReservedNodeOfferings :: DescribeReservedNodeOfferings -> TestTree
requestDescribeReservedNodeOfferings =
  req
    "DescribeReservedNodeOfferings"
    "fixture/DescribeReservedNodeOfferings.yaml"

requestEnableLogging :: EnableLogging -> TestTree
requestEnableLogging =
  req
    "EnableLogging"
    "fixture/EnableLogging.yaml"

requestDescribeReservedNodes :: DescribeReservedNodes -> TestTree
requestDescribeReservedNodes =
  req
    "DescribeReservedNodes"
    "fixture/DescribeReservedNodes.yaml"

requestCreateHsmConfiguration :: CreateHsmConfiguration -> TestTree
requestCreateHsmConfiguration =
  req
    "CreateHsmConfiguration"
    "fixture/CreateHsmConfiguration.yaml"

-- Responses

responseDisableLogging :: LoggingStatus -> TestTree
responseDisableLogging =
  res
    "DisableLoggingResponse"
    "fixture/DisableLoggingResponse.proto"
    defaultService
    (Proxy :: Proxy DisableLogging)

responseAddPartner :: PartnerIntegrationOutputMessage -> TestTree
responseAddPartner =
  res
    "AddPartnerResponse"
    "fixture/AddPartnerResponse.proto"
    defaultService
    (Proxy :: Proxy AddPartner)

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

responseModifyEndpointAccess :: EndpointAccess -> TestTree
responseModifyEndpointAccess =
  res
    "ModifyEndpointAccessResponse"
    "fixture/ModifyEndpointAccessResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyEndpointAccess)

responseDeleteClusterSubnetGroup :: DeleteClusterSubnetGroupResponse -> TestTree
responseDeleteClusterSubnetGroup =
  res
    "DeleteClusterSubnetGroupResponse"
    "fixture/DeleteClusterSubnetGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteClusterSubnetGroup)

responseAssociateDataShareConsumer :: DataShare -> TestTree
responseAssociateDataShareConsumer =
  res
    "AssociateDataShareConsumerResponse"
    "fixture/AssociateDataShareConsumerResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateDataShareConsumer)

responseRevokeEndpointAccess :: EndpointAuthorization -> TestTree
responseRevokeEndpointAccess =
  res
    "RevokeEndpointAccessResponse"
    "fixture/RevokeEndpointAccessResponse.proto"
    defaultService
    (Proxy :: Proxy RevokeEndpointAccess)

responseDeletePartner :: PartnerIntegrationOutputMessage -> TestTree
responseDeletePartner =
  res
    "DeletePartnerResponse"
    "fixture/DeletePartnerResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePartner)

responseCreateAuthenticationProfile :: CreateAuthenticationProfileResponse -> TestTree
responseCreateAuthenticationProfile =
  res
    "CreateAuthenticationProfileResponse"
    "fixture/CreateAuthenticationProfileResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAuthenticationProfile)

responseModifyAquaConfiguration :: ModifyAquaConfigurationResponse -> TestTree
responseModifyAquaConfiguration =
  res
    "ModifyAquaConfigurationResponse"
    "fixture/ModifyAquaConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyAquaConfiguration)

responseCreateUsageLimit :: UsageLimit -> TestTree
responseCreateUsageLimit =
  res
    "CreateUsageLimitResponse"
    "fixture/CreateUsageLimitResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUsageLimit)

responseAuthorizeEndpointAccess :: EndpointAuthorization -> TestTree
responseAuthorizeEndpointAccess =
  res
    "AuthorizeEndpointAccessResponse"
    "fixture/AuthorizeEndpointAccessResponse.proto"
    defaultService
    (Proxy :: Proxy AuthorizeEndpointAccess)

responseDescribeHsmClientCertificates :: DescribeHsmClientCertificatesResponse -> TestTree
responseDescribeHsmClientCertificates =
  res
    "DescribeHsmClientCertificatesResponse"
    "fixture/DescribeHsmClientCertificatesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeHsmClientCertificates)

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

responseDescribeTags :: DescribeTagsResponse -> TestTree
responseDescribeTags =
  res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTags)

responseModifyClusterParameterGroup :: ClusterParameterGroupNameMessage -> TestTree
responseModifyClusterParameterGroup =
  res
    "ModifyClusterParameterGroupResponse"
    "fixture/ModifyClusterParameterGroupResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyClusterParameterGroup)

responseRevokeClusterSecurityGroupIngress :: RevokeClusterSecurityGroupIngressResponse -> TestTree
responseRevokeClusterSecurityGroupIngress =
  res
    "RevokeClusterSecurityGroupIngressResponse"
    "fixture/RevokeClusterSecurityGroupIngressResponse.proto"
    defaultService
    (Proxy :: Proxy RevokeClusterSecurityGroupIngress)

responseResetClusterParameterGroup :: ClusterParameterGroupNameMessage -> TestTree
responseResetClusterParameterGroup =
  res
    "ResetClusterParameterGroupResponse"
    "fixture/ResetClusterParameterGroupResponse.proto"
    defaultService
    (Proxy :: Proxy ResetClusterParameterGroup)

responseDeleteUsageLimit :: DeleteUsageLimitResponse -> TestTree
responseDeleteUsageLimit =
  res
    "DeleteUsageLimitResponse"
    "fixture/DeleteUsageLimitResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUsageLimit)

responseDescribeClusterDbRevisions :: DescribeClusterDbRevisionsResponse -> TestTree
responseDescribeClusterDbRevisions =
  res
    "DescribeClusterDbRevisionsResponse"
    "fixture/DescribeClusterDbRevisionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeClusterDbRevisions)

responseRotateEncryptionKey :: RotateEncryptionKeyResponse -> TestTree
responseRotateEncryptionKey =
  res
    "RotateEncryptionKeyResponse"
    "fixture/RotateEncryptionKeyResponse.proto"
    defaultService
    (Proxy :: Proxy RotateEncryptionKey)

responseDescribeScheduledActions :: DescribeScheduledActionsResponse -> TestTree
responseDescribeScheduledActions =
  res
    "DescribeScheduledActionsResponse"
    "fixture/DescribeScheduledActionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeScheduledActions)

responseDescribeEventSubscriptions :: DescribeEventSubscriptionsResponse -> TestTree
responseDescribeEventSubscriptions =
  res
    "DescribeEventSubscriptionsResponse"
    "fixture/DescribeEventSubscriptionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEventSubscriptions)

responseDeleteEventSubscription :: DeleteEventSubscriptionResponse -> TestTree
responseDeleteEventSubscription =
  res
    "DeleteEventSubscriptionResponse"
    "fixture/DeleteEventSubscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteEventSubscription)

responseDescribeDataSharesForConsumer :: DescribeDataSharesForConsumerResponse -> TestTree
responseDescribeDataSharesForConsumer =
  res
    "DescribeDataSharesForConsumerResponse"
    "fixture/DescribeDataSharesForConsumerResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDataSharesForConsumer)

responseModifyClusterSnapshot :: ModifyClusterSnapshotResponse -> TestTree
responseModifyClusterSnapshot =
  res
    "ModifyClusterSnapshotResponse"
    "fixture/ModifyClusterSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyClusterSnapshot)

responseModifyClusterSubnetGroup :: ModifyClusterSubnetGroupResponse -> TestTree
responseModifyClusterSubnetGroup =
  res
    "ModifyClusterSubnetGroupResponse"
    "fixture/ModifyClusterSubnetGroupResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyClusterSubnetGroup)

responseDeleteScheduledAction :: DeleteScheduledActionResponse -> TestTree
responseDeleteScheduledAction =
  res
    "DeleteScheduledActionResponse"
    "fixture/DeleteScheduledActionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteScheduledAction)

responseDeleteEndpointAccess :: EndpointAccess -> TestTree
responseDeleteEndpointAccess =
  res
    "DeleteEndpointAccessResponse"
    "fixture/DeleteEndpointAccessResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteEndpointAccess)

responseRestoreTableFromClusterSnapshot :: RestoreTableFromClusterSnapshotResponse -> TestTree
responseRestoreTableFromClusterSnapshot =
  res
    "RestoreTableFromClusterSnapshotResponse"
    "fixture/RestoreTableFromClusterSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy RestoreTableFromClusterSnapshot)

responseCreateCluster :: CreateClusterResponse -> TestTree
responseCreateCluster =
  res
    "CreateClusterResponse"
    "fixture/CreateClusterResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCluster)

responseCreateEndpointAccess :: EndpointAccess -> TestTree
responseCreateEndpointAccess =
  res
    "CreateEndpointAccessResponse"
    "fixture/CreateEndpointAccessResponse.proto"
    defaultService
    (Proxy :: Proxy CreateEndpointAccess)

responseModifyClusterSnapshotSchedule :: ModifyClusterSnapshotScheduleResponse -> TestTree
responseModifyClusterSnapshotSchedule =
  res
    "ModifyClusterSnapshotScheduleResponse"
    "fixture/ModifyClusterSnapshotScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyClusterSnapshotSchedule)

responseDescribeAccountAttributes :: DescribeAccountAttributesResponse -> TestTree
responseDescribeAccountAttributes =
  res
    "DescribeAccountAttributesResponse"
    "fixture/DescribeAccountAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAccountAttributes)

responseModifyAuthenticationProfile :: ModifyAuthenticationProfileResponse -> TestTree
responseModifyAuthenticationProfile =
  res
    "ModifyAuthenticationProfileResponse"
    "fixture/ModifyAuthenticationProfileResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyAuthenticationProfile)

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

responseUpdatePartnerStatus :: PartnerIntegrationOutputMessage -> TestTree
responseUpdatePartnerStatus =
  res
    "UpdatePartnerStatusResponse"
    "fixture/UpdatePartnerStatusResponse.proto"
    defaultService
    (Proxy :: Proxy UpdatePartnerStatus)

responseDescribeDataSharesForProducer :: DescribeDataSharesForProducerResponse -> TestTree
responseDescribeDataSharesForProducer =
  res
    "DescribeDataSharesForProducerResponse"
    "fixture/DescribeDataSharesForProducerResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDataSharesForProducer)

responseDescribeHsmConfigurations :: DescribeHsmConfigurationsResponse -> TestTree
responseDescribeHsmConfigurations =
  res
    "DescribeHsmConfigurationsResponse"
    "fixture/DescribeHsmConfigurationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeHsmConfigurations)

responseDescribeClusterSnapshots :: DescribeClusterSnapshotsResponse -> TestTree
responseDescribeClusterSnapshots =
  res
    "DescribeClusterSnapshotsResponse"
    "fixture/DescribeClusterSnapshotsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeClusterSnapshots)

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

responseModifyUsageLimit :: UsageLimit -> TestTree
responseModifyUsageLimit =
  res
    "ModifyUsageLimitResponse"
    "fixture/ModifyUsageLimitResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyUsageLimit)

responseCreateClusterParameterGroup :: CreateClusterParameterGroupResponse -> TestTree
responseCreateClusterParameterGroup =
  res
    "CreateClusterParameterGroupResponse"
    "fixture/CreateClusterParameterGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateClusterParameterGroup)

responseCreateSnapshotSchedule :: SnapshotSchedule -> TestTree
responseCreateSnapshotSchedule =
  res
    "CreateSnapshotScheduleResponse"
    "fixture/CreateSnapshotScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSnapshotSchedule)

responseDescribeEndpointAuthorization :: DescribeEndpointAuthorizationResponse -> TestTree
responseDescribeEndpointAuthorization =
  res
    "DescribeEndpointAuthorizationResponse"
    "fixture/DescribeEndpointAuthorizationResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEndpointAuthorization)

responseDeleteClusterParameterGroup :: DeleteClusterParameterGroupResponse -> TestTree
responseDeleteClusterParameterGroup =
  res
    "DeleteClusterParameterGroupResponse"
    "fixture/DeleteClusterParameterGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteClusterParameterGroup)

responseDescribeClusterSecurityGroups :: DescribeClusterSecurityGroupsResponse -> TestTree
responseDescribeClusterSecurityGroups =
  res
    "DescribeClusterSecurityGroupsResponse"
    "fixture/DescribeClusterSecurityGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeClusterSecurityGroups)

responseDescribeNodeConfigurationOptions :: DescribeNodeConfigurationOptionsResponse -> TestTree
responseDescribeNodeConfigurationOptions =
  res
    "DescribeNodeConfigurationOptionsResponse"
    "fixture/DescribeNodeConfigurationOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeNodeConfigurationOptions)

responseDescribeAuthenticationProfiles :: DescribeAuthenticationProfilesResponse -> TestTree
responseDescribeAuthenticationProfiles =
  res
    "DescribeAuthenticationProfilesResponse"
    "fixture/DescribeAuthenticationProfilesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAuthenticationProfiles)

responseDescribeEndpointAccess :: DescribeEndpointAccessResponse -> TestTree
responseDescribeEndpointAccess =
  res
    "DescribeEndpointAccessResponse"
    "fixture/DescribeEndpointAccessResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEndpointAccess)

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

responseDescribeLoggingStatus :: LoggingStatus -> TestTree
responseDescribeLoggingStatus =
  res
    "DescribeLoggingStatusResponse"
    "fixture/DescribeLoggingStatusResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLoggingStatus)

responseDescribeClusterParameterGroups :: DescribeClusterParameterGroupsResponse -> TestTree
responseDescribeClusterParameterGroups =
  res
    "DescribeClusterParameterGroupsResponse"
    "fixture/DescribeClusterParameterGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeClusterParameterGroups)

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

responseRejectDataShare :: DataShare -> TestTree
responseRejectDataShare =
  res
    "RejectDataShareResponse"
    "fixture/RejectDataShareResponse.proto"
    defaultService
    (Proxy :: Proxy RejectDataShare)

responseCreateClusterSubnetGroup :: CreateClusterSubnetGroupResponse -> TestTree
responseCreateClusterSubnetGroup =
  res
    "CreateClusterSubnetGroupResponse"
    "fixture/CreateClusterSubnetGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateClusterSubnetGroup)

responseDeleteHsmConfiguration :: DeleteHsmConfigurationResponse -> TestTree
responseDeleteHsmConfiguration =
  res
    "DeleteHsmConfigurationResponse"
    "fixture/DeleteHsmConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteHsmConfiguration)

responseDescribeTableRestoreStatus :: DescribeTableRestoreStatusResponse -> TestTree
responseDescribeTableRestoreStatus =
  res
    "DescribeTableRestoreStatusResponse"
    "fixture/DescribeTableRestoreStatusResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTableRestoreStatus)

responseDeleteClusterSnapshot :: DeleteClusterSnapshotResponse -> TestTree
responseDeleteClusterSnapshot =
  res
    "DeleteClusterSnapshotResponse"
    "fixture/DeleteClusterSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteClusterSnapshot)

responseModifyClusterDbRevision :: ModifyClusterDbRevisionResponse -> TestTree
responseModifyClusterDbRevision =
  res
    "ModifyClusterDbRevisionResponse"
    "fixture/ModifyClusterDbRevisionResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyClusterDbRevision)

responseAuthorizeClusterSecurityGroupIngress :: AuthorizeClusterSecurityGroupIngressResponse -> TestTree
responseAuthorizeClusterSecurityGroupIngress =
  res
    "AuthorizeClusterSecurityGroupIngressResponse"
    "fixture/AuthorizeClusterSecurityGroupIngressResponse.proto"
    defaultService
    (Proxy :: Proxy AuthorizeClusterSecurityGroupIngress)

responseDeauthorizeDataShare :: DataShare -> TestTree
responseDeauthorizeDataShare =
  res
    "DeauthorizeDataShareResponse"
    "fixture/DeauthorizeDataShareResponse.proto"
    defaultService
    (Proxy :: Proxy DeauthorizeDataShare)

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

responseCreateClusterSecurityGroup :: CreateClusterSecurityGroupResponse -> TestTree
responseCreateClusterSecurityGroup =
  res
    "CreateClusterSecurityGroupResponse"
    "fixture/CreateClusterSecurityGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateClusterSecurityGroup)

responseDescribeResize :: ResizeProgressMessage -> TestTree
responseDescribeResize =
  res
    "DescribeResizeResponse"
    "fixture/DescribeResizeResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeResize)

responseAcceptReservedNodeExchange :: AcceptReservedNodeExchangeResponse -> TestTree
responseAcceptReservedNodeExchange =
  res
    "AcceptReservedNodeExchangeResponse"
    "fixture/AcceptReservedNodeExchangeResponse.proto"
    defaultService
    (Proxy :: Proxy AcceptReservedNodeExchange)

responseDescribeSnapshotSchedules :: DescribeSnapshotSchedulesResponse -> TestTree
responseDescribeSnapshotSchedules =
  res
    "DescribeSnapshotSchedulesResponse"
    "fixture/DescribeSnapshotSchedulesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSnapshotSchedules)

responseModifyClusterMaintenance :: ModifyClusterMaintenanceResponse -> TestTree
responseModifyClusterMaintenance =
  res
    "ModifyClusterMaintenanceResponse"
    "fixture/ModifyClusterMaintenanceResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyClusterMaintenance)

responseDescribeStorage :: DescribeStorageResponse -> TestTree
responseDescribeStorage =
  res
    "DescribeStorageResponse"
    "fixture/DescribeStorageResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeStorage)

responseDisassociateDataShareConsumer :: DataShare -> TestTree
responseDisassociateDataShareConsumer =
  res
    "DisassociateDataShareConsumerResponse"
    "fixture/DisassociateDataShareConsumerResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateDataShareConsumer)

responseBatchModifyClusterSnapshots :: BatchModifyClusterSnapshotsResponse -> TestTree
responseBatchModifyClusterSnapshots =
  res
    "BatchModifyClusterSnapshotsResponse"
    "fixture/BatchModifyClusterSnapshotsResponse.proto"
    defaultService
    (Proxy :: Proxy BatchModifyClusterSnapshots)

responseDescribeSnapshotCopyGrants :: DescribeSnapshotCopyGrantsResponse -> TestTree
responseDescribeSnapshotCopyGrants =
  res
    "DescribeSnapshotCopyGrantsResponse"
    "fixture/DescribeSnapshotCopyGrantsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSnapshotCopyGrants)

responseModifySnapshotSchedule :: SnapshotSchedule -> TestTree
responseModifySnapshotSchedule =
  res
    "ModifySnapshotScheduleResponse"
    "fixture/ModifySnapshotScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy ModifySnapshotSchedule)

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

responseAuthorizeDataShare :: DataShare -> TestTree
responseAuthorizeDataShare =
  res
    "AuthorizeDataShareResponse"
    "fixture/AuthorizeDataShareResponse.proto"
    defaultService
    (Proxy :: Proxy AuthorizeDataShare)

responseDescribeDefaultClusterParameters :: DescribeDefaultClusterParametersResponse -> TestTree
responseDescribeDefaultClusterParameters =
  res
    "DescribeDefaultClusterParametersResponse"
    "fixture/DescribeDefaultClusterParametersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDefaultClusterParameters)

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

responseDescribeClusterTracks :: DescribeClusterTracksResponse -> TestTree
responseDescribeClusterTracks =
  res
    "DescribeClusterTracksResponse"
    "fixture/DescribeClusterTracksResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeClusterTracks)

responseDescribeOrderableClusterOptions :: DescribeOrderableClusterOptionsResponse -> TestTree
responseDescribeOrderableClusterOptions =
  res
    "DescribeOrderableClusterOptionsResponse"
    "fixture/DescribeOrderableClusterOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeOrderableClusterOptions)

responseCreateEventSubscription :: CreateEventSubscriptionResponse -> TestTree
responseCreateEventSubscription =
  res
    "CreateEventSubscriptionResponse"
    "fixture/CreateEventSubscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateEventSubscription)

responseCreateScheduledAction :: ScheduledAction -> TestTree
responseCreateScheduledAction =
  res
    "CreateScheduledActionResponse"
    "fixture/CreateScheduledActionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateScheduledAction)

responseAuthorizeSnapshotAccess :: AuthorizeSnapshotAccessResponse -> TestTree
responseAuthorizeSnapshotAccess =
  res
    "AuthorizeSnapshotAccessResponse"
    "fixture/AuthorizeSnapshotAccessResponse.proto"
    defaultService
    (Proxy :: Proxy AuthorizeSnapshotAccess)

responseDeleteHsmClientCertificate :: DeleteHsmClientCertificateResponse -> TestTree
responseDeleteHsmClientCertificate =
  res
    "DeleteHsmClientCertificateResponse"
    "fixture/DeleteHsmClientCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteHsmClientCertificate)

responseDeleteCluster :: DeleteClusterResponse -> TestTree
responseDeleteCluster =
  res
    "DeleteClusterResponse"
    "fixture/DeleteClusterResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCluster)

responseDescribeDataShares :: DescribeDataSharesResponse -> TestTree
responseDescribeDataShares =
  res
    "DescribeDataSharesResponse"
    "fixture/DescribeDataSharesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDataShares)

responseRebootCluster :: RebootClusterResponse -> TestTree
responseRebootCluster =
  res
    "RebootClusterResponse"
    "fixture/RebootClusterResponse.proto"
    defaultService
    (Proxy :: Proxy RebootCluster)

responseResumeCluster :: ResumeClusterResponse -> TestTree
responseResumeCluster =
  res
    "ResumeClusterResponse"
    "fixture/ResumeClusterResponse.proto"
    defaultService
    (Proxy :: Proxy ResumeCluster)

responseModifyClusterIamRoles :: ModifyClusterIamRolesResponse -> TestTree
responseModifyClusterIamRoles =
  res
    "ModifyClusterIamRolesResponse"
    "fixture/ModifyClusterIamRolesResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyClusterIamRoles)

responseRestoreFromClusterSnapshot :: RestoreFromClusterSnapshotResponse -> TestTree
responseRestoreFromClusterSnapshot =
  res
    "RestoreFromClusterSnapshotResponse"
    "fixture/RestoreFromClusterSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy RestoreFromClusterSnapshot)

responsePauseCluster :: PauseClusterResponse -> TestTree
responsePauseCluster =
  res
    "PauseClusterResponse"
    "fixture/PauseClusterResponse.proto"
    defaultService
    (Proxy :: Proxy PauseCluster)

responseDeleteSnapshotSchedule :: DeleteSnapshotScheduleResponse -> TestTree
responseDeleteSnapshotSchedule =
  res
    "DeleteSnapshotScheduleResponse"
    "fixture/DeleteSnapshotScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSnapshotSchedule)

responseModifySnapshotCopyRetentionPeriod :: ModifySnapshotCopyRetentionPeriodResponse -> TestTree
responseModifySnapshotCopyRetentionPeriod =
  res
    "ModifySnapshotCopyRetentionPeriodResponse"
    "fixture/ModifySnapshotCopyRetentionPeriodResponse.proto"
    defaultService
    (Proxy :: Proxy ModifySnapshotCopyRetentionPeriod)

responseDescribeClusterSubnetGroups :: DescribeClusterSubnetGroupsResponse -> TestTree
responseDescribeClusterSubnetGroups =
  res
    "DescribeClusterSubnetGroupsResponse"
    "fixture/DescribeClusterSubnetGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeClusterSubnetGroups)

responseResizeCluster :: ResizeClusterResponse -> TestTree
responseResizeCluster =
  res
    "ResizeClusterResponse"
    "fixture/ResizeClusterResponse.proto"
    defaultService
    (Proxy :: Proxy ResizeCluster)

responseBatchDeleteClusterSnapshots :: BatchDeleteClusterSnapshotsResponse -> TestTree
responseBatchDeleteClusterSnapshots =
  res
    "BatchDeleteClusterSnapshotsResponse"
    "fixture/BatchDeleteClusterSnapshotsResponse.proto"
    defaultService
    (Proxy :: Proxy BatchDeleteClusterSnapshots)

responseDescribePartners :: DescribePartnersResponse -> TestTree
responseDescribePartners =
  res
    "DescribePartnersResponse"
    "fixture/DescribePartnersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePartners)

responseRevokeSnapshotAccess :: RevokeSnapshotAccessResponse -> TestTree
responseRevokeSnapshotAccess =
  res
    "RevokeSnapshotAccessResponse"
    "fixture/RevokeSnapshotAccessResponse.proto"
    defaultService
    (Proxy :: Proxy RevokeSnapshotAccess)

responseCreateTags :: CreateTagsResponse -> TestTree
responseCreateTags =
  res
    "CreateTagsResponse"
    "fixture/CreateTagsResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTags)

responseDisableSnapshotCopy :: DisableSnapshotCopyResponse -> TestTree
responseDisableSnapshotCopy =
  res
    "DisableSnapshotCopyResponse"
    "fixture/DisableSnapshotCopyResponse.proto"
    defaultService
    (Proxy :: Proxy DisableSnapshotCopy)

responseDescribeClusterParameters :: DescribeClusterParametersResponse -> TestTree
responseDescribeClusterParameters =
  res
    "DescribeClusterParametersResponse"
    "fixture/DescribeClusterParametersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeClusterParameters)

responseDeleteAuthenticationProfile :: DeleteAuthenticationProfileResponse -> TestTree
responseDeleteAuthenticationProfile =
  res
    "DeleteAuthenticationProfileResponse"
    "fixture/DeleteAuthenticationProfileResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAuthenticationProfile)

responseDeleteClusterSecurityGroup :: DeleteClusterSecurityGroupResponse -> TestTree
responseDeleteClusterSecurityGroup =
  res
    "DeleteClusterSecurityGroupResponse"
    "fixture/DeleteClusterSecurityGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteClusterSecurityGroup)

responseDescribeReservedNodeOfferings :: DescribeReservedNodeOfferingsResponse -> TestTree
responseDescribeReservedNodeOfferings =
  res
    "DescribeReservedNodeOfferingsResponse"
    "fixture/DescribeReservedNodeOfferingsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeReservedNodeOfferings)

responseEnableLogging :: LoggingStatus -> TestTree
responseEnableLogging =
  res
    "EnableLoggingResponse"
    "fixture/EnableLoggingResponse.proto"
    defaultService
    (Proxy :: Proxy EnableLogging)

responseDescribeReservedNodes :: DescribeReservedNodesResponse -> TestTree
responseDescribeReservedNodes =
  res
    "DescribeReservedNodesResponse"
    "fixture/DescribeReservedNodesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeReservedNodes)

responseCreateHsmConfiguration :: CreateHsmConfigurationResponse -> TestTree
responseCreateHsmConfiguration =
  res
    "CreateHsmConfigurationResponse"
    "fixture/CreateHsmConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateHsmConfiguration)
