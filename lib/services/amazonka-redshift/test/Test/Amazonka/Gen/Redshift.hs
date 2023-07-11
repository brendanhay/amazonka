{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Redshift
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Redshift where

import Amazonka.Redshift
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.Redshift.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAcceptReservedNodeExchange $
--             newAcceptReservedNodeExchange
--
--         , requestAddPartner $
--             newAddPartner
--
--         , requestAssociateDataShareConsumer $
--             newAssociateDataShareConsumer
--
--         , requestAuthorizeClusterSecurityGroupIngress $
--             newAuthorizeClusterSecurityGroupIngress
--
--         , requestAuthorizeDataShare $
--             newAuthorizeDataShare
--
--         , requestAuthorizeEndpointAccess $
--             newAuthorizeEndpointAccess
--
--         , requestAuthorizeSnapshotAccess $
--             newAuthorizeSnapshotAccess
--
--         , requestBatchDeleteClusterSnapshots $
--             newBatchDeleteClusterSnapshots
--
--         , requestBatchModifyClusterSnapshots $
--             newBatchModifyClusterSnapshots
--
--         , requestCancelResize $
--             newCancelResize
--
--         , requestCopyClusterSnapshot $
--             newCopyClusterSnapshot
--
--         , requestCreateAuthenticationProfile $
--             newCreateAuthenticationProfile
--
--         , requestCreateCluster $
--             newCreateCluster
--
--         , requestCreateClusterParameterGroup $
--             newCreateClusterParameterGroup
--
--         , requestCreateClusterSecurityGroup $
--             newCreateClusterSecurityGroup
--
--         , requestCreateClusterSnapshot $
--             newCreateClusterSnapshot
--
--         , requestCreateClusterSubnetGroup $
--             newCreateClusterSubnetGroup
--
--         , requestCreateEndpointAccess $
--             newCreateEndpointAccess
--
--         , requestCreateEventSubscription $
--             newCreateEventSubscription
--
--         , requestCreateHsmClientCertificate $
--             newCreateHsmClientCertificate
--
--         , requestCreateHsmConfiguration $
--             newCreateHsmConfiguration
--
--         , requestCreateScheduledAction $
--             newCreateScheduledAction
--
--         , requestCreateSnapshotCopyGrant $
--             newCreateSnapshotCopyGrant
--
--         , requestCreateSnapshotSchedule $
--             newCreateSnapshotSchedule
--
--         , requestCreateTags $
--             newCreateTags
--
--         , requestCreateUsageLimit $
--             newCreateUsageLimit
--
--         , requestDeauthorizeDataShare $
--             newDeauthorizeDataShare
--
--         , requestDeleteAuthenticationProfile $
--             newDeleteAuthenticationProfile
--
--         , requestDeleteCluster $
--             newDeleteCluster
--
--         , requestDeleteClusterParameterGroup $
--             newDeleteClusterParameterGroup
--
--         , requestDeleteClusterSecurityGroup $
--             newDeleteClusterSecurityGroup
--
--         , requestDeleteClusterSnapshot $
--             newDeleteClusterSnapshot
--
--         , requestDeleteClusterSubnetGroup $
--             newDeleteClusterSubnetGroup
--
--         , requestDeleteEndpointAccess $
--             newDeleteEndpointAccess
--
--         , requestDeleteEventSubscription $
--             newDeleteEventSubscription
--
--         , requestDeleteHsmClientCertificate $
--             newDeleteHsmClientCertificate
--
--         , requestDeleteHsmConfiguration $
--             newDeleteHsmConfiguration
--
--         , requestDeletePartner $
--             newDeletePartner
--
--         , requestDeleteScheduledAction $
--             newDeleteScheduledAction
--
--         , requestDeleteSnapshotCopyGrant $
--             newDeleteSnapshotCopyGrant
--
--         , requestDeleteSnapshotSchedule $
--             newDeleteSnapshotSchedule
--
--         , requestDeleteTags $
--             newDeleteTags
--
--         , requestDeleteUsageLimit $
--             newDeleteUsageLimit
--
--         , requestDescribeAccountAttributes $
--             newDescribeAccountAttributes
--
--         , requestDescribeAuthenticationProfiles $
--             newDescribeAuthenticationProfiles
--
--         , requestDescribeClusterDbRevisions $
--             newDescribeClusterDbRevisions
--
--         , requestDescribeClusterParameterGroups $
--             newDescribeClusterParameterGroups
--
--         , requestDescribeClusterParameters $
--             newDescribeClusterParameters
--
--         , requestDescribeClusterSecurityGroups $
--             newDescribeClusterSecurityGroups
--
--         , requestDescribeClusterSnapshots $
--             newDescribeClusterSnapshots
--
--         , requestDescribeClusterSubnetGroups $
--             newDescribeClusterSubnetGroups
--
--         , requestDescribeClusterTracks $
--             newDescribeClusterTracks
--
--         , requestDescribeClusterVersions $
--             newDescribeClusterVersions
--
--         , requestDescribeClusters $
--             newDescribeClusters
--
--         , requestDescribeDataShares $
--             newDescribeDataShares
--
--         , requestDescribeDataSharesForConsumer $
--             newDescribeDataSharesForConsumer
--
--         , requestDescribeDataSharesForProducer $
--             newDescribeDataSharesForProducer
--
--         , requestDescribeDefaultClusterParameters $
--             newDescribeDefaultClusterParameters
--
--         , requestDescribeEndpointAccess $
--             newDescribeEndpointAccess
--
--         , requestDescribeEndpointAuthorization $
--             newDescribeEndpointAuthorization
--
--         , requestDescribeEventCategories $
--             newDescribeEventCategories
--
--         , requestDescribeEventSubscriptions $
--             newDescribeEventSubscriptions
--
--         , requestDescribeEvents $
--             newDescribeEvents
--
--         , requestDescribeHsmClientCertificates $
--             newDescribeHsmClientCertificates
--
--         , requestDescribeHsmConfigurations $
--             newDescribeHsmConfigurations
--
--         , requestDescribeLoggingStatus $
--             newDescribeLoggingStatus
--
--         , requestDescribeNodeConfigurationOptions $
--             newDescribeNodeConfigurationOptions
--
--         , requestDescribeOrderableClusterOptions $
--             newDescribeOrderableClusterOptions
--
--         , requestDescribePartners $
--             newDescribePartners
--
--         , requestDescribeReservedNodeExchangeStatus $
--             newDescribeReservedNodeExchangeStatus
--
--         , requestDescribeReservedNodeOfferings $
--             newDescribeReservedNodeOfferings
--
--         , requestDescribeReservedNodes $
--             newDescribeReservedNodes
--
--         , requestDescribeResize $
--             newDescribeResize
--
--         , requestDescribeScheduledActions $
--             newDescribeScheduledActions
--
--         , requestDescribeSnapshotCopyGrants $
--             newDescribeSnapshotCopyGrants
--
--         , requestDescribeSnapshotSchedules $
--             newDescribeSnapshotSchedules
--
--         , requestDescribeStorage $
--             newDescribeStorage
--
--         , requestDescribeTableRestoreStatus $
--             newDescribeTableRestoreStatus
--
--         , requestDescribeTags $
--             newDescribeTags
--
--         , requestDescribeUsageLimits $
--             newDescribeUsageLimits
--
--         , requestDisableLogging $
--             newDisableLogging
--
--         , requestDisableSnapshotCopy $
--             newDisableSnapshotCopy
--
--         , requestDisassociateDataShareConsumer $
--             newDisassociateDataShareConsumer
--
--         , requestEnableLogging $
--             newEnableLogging
--
--         , requestEnableSnapshotCopy $
--             newEnableSnapshotCopy
--
--         , requestGetClusterCredentials $
--             newGetClusterCredentials
--
--         , requestGetClusterCredentialsWithIAM $
--             newGetClusterCredentialsWithIAM
--
--         , requestGetReservedNodeExchangeConfigurationOptions $
--             newGetReservedNodeExchangeConfigurationOptions
--
--         , requestGetReservedNodeExchangeOfferings $
--             newGetReservedNodeExchangeOfferings
--
--         , requestModifyAquaConfiguration $
--             newModifyAquaConfiguration
--
--         , requestModifyAuthenticationProfile $
--             newModifyAuthenticationProfile
--
--         , requestModifyCluster $
--             newModifyCluster
--
--         , requestModifyClusterDbRevision $
--             newModifyClusterDbRevision
--
--         , requestModifyClusterIamRoles $
--             newModifyClusterIamRoles
--
--         , requestModifyClusterMaintenance $
--             newModifyClusterMaintenance
--
--         , requestModifyClusterParameterGroup $
--             newModifyClusterParameterGroup
--
--         , requestModifyClusterSnapshot $
--             newModifyClusterSnapshot
--
--         , requestModifyClusterSnapshotSchedule $
--             newModifyClusterSnapshotSchedule
--
--         , requestModifyClusterSubnetGroup $
--             newModifyClusterSubnetGroup
--
--         , requestModifyEndpointAccess $
--             newModifyEndpointAccess
--
--         , requestModifyEventSubscription $
--             newModifyEventSubscription
--
--         , requestModifyScheduledAction $
--             newModifyScheduledAction
--
--         , requestModifySnapshotCopyRetentionPeriod $
--             newModifySnapshotCopyRetentionPeriod
--
--         , requestModifySnapshotSchedule $
--             newModifySnapshotSchedule
--
--         , requestModifyUsageLimit $
--             newModifyUsageLimit
--
--         , requestPauseCluster $
--             newPauseCluster
--
--         , requestPurchaseReservedNodeOffering $
--             newPurchaseReservedNodeOffering
--
--         , requestRebootCluster $
--             newRebootCluster
--
--         , requestRejectDataShare $
--             newRejectDataShare
--
--         , requestResetClusterParameterGroup $
--             newResetClusterParameterGroup
--
--         , requestResizeCluster $
--             newResizeCluster
--
--         , requestRestoreFromClusterSnapshot $
--             newRestoreFromClusterSnapshot
--
--         , requestRestoreTableFromClusterSnapshot $
--             newRestoreTableFromClusterSnapshot
--
--         , requestResumeCluster $
--             newResumeCluster
--
--         , requestRevokeClusterSecurityGroupIngress $
--             newRevokeClusterSecurityGroupIngress
--
--         , requestRevokeEndpointAccess $
--             newRevokeEndpointAccess
--
--         , requestRevokeSnapshotAccess $
--             newRevokeSnapshotAccess
--
--         , requestRotateEncryptionKey $
--             newRotateEncryptionKey
--
--         , requestUpdatePartnerStatus $
--             newUpdatePartnerStatus
--
--           ]

--     , testGroup "response"
--         [ responseAcceptReservedNodeExchange $
--             newAcceptReservedNodeExchangeResponse
--
--         , responseAddPartner $
--             newPartnerIntegrationOutputMessage
--
--         , responseAssociateDataShareConsumer $
--             newDataShare
--
--         , responseAuthorizeClusterSecurityGroupIngress $
--             newAuthorizeClusterSecurityGroupIngressResponse
--
--         , responseAuthorizeDataShare $
--             newDataShare
--
--         , responseAuthorizeEndpointAccess $
--             newEndpointAuthorization
--
--         , responseAuthorizeSnapshotAccess $
--             newAuthorizeSnapshotAccessResponse
--
--         , responseBatchDeleteClusterSnapshots $
--             newBatchDeleteClusterSnapshotsResponse
--
--         , responseBatchModifyClusterSnapshots $
--             newBatchModifyClusterSnapshotsResponse
--
--         , responseCancelResize $
--             newResizeProgressMessage
--
--         , responseCopyClusterSnapshot $
--             newCopyClusterSnapshotResponse
--
--         , responseCreateAuthenticationProfile $
--             newCreateAuthenticationProfileResponse
--
--         , responseCreateCluster $
--             newCreateClusterResponse
--
--         , responseCreateClusterParameterGroup $
--             newCreateClusterParameterGroupResponse
--
--         , responseCreateClusterSecurityGroup $
--             newCreateClusterSecurityGroupResponse
--
--         , responseCreateClusterSnapshot $
--             newCreateClusterSnapshotResponse
--
--         , responseCreateClusterSubnetGroup $
--             newCreateClusterSubnetGroupResponse
--
--         , responseCreateEndpointAccess $
--             newEndpointAccess
--
--         , responseCreateEventSubscription $
--             newCreateEventSubscriptionResponse
--
--         , responseCreateHsmClientCertificate $
--             newCreateHsmClientCertificateResponse
--
--         , responseCreateHsmConfiguration $
--             newCreateHsmConfigurationResponse
--
--         , responseCreateScheduledAction $
--             newScheduledAction
--
--         , responseCreateSnapshotCopyGrant $
--             newCreateSnapshotCopyGrantResponse
--
--         , responseCreateSnapshotSchedule $
--             newSnapshotSchedule
--
--         , responseCreateTags $
--             newCreateTagsResponse
--
--         , responseCreateUsageLimit $
--             newUsageLimit
--
--         , responseDeauthorizeDataShare $
--             newDataShare
--
--         , responseDeleteAuthenticationProfile $
--             newDeleteAuthenticationProfileResponse
--
--         , responseDeleteCluster $
--             newDeleteClusterResponse
--
--         , responseDeleteClusterParameterGroup $
--             newDeleteClusterParameterGroupResponse
--
--         , responseDeleteClusterSecurityGroup $
--             newDeleteClusterSecurityGroupResponse
--
--         , responseDeleteClusterSnapshot $
--             newDeleteClusterSnapshotResponse
--
--         , responseDeleteClusterSubnetGroup $
--             newDeleteClusterSubnetGroupResponse
--
--         , responseDeleteEndpointAccess $
--             newEndpointAccess
--
--         , responseDeleteEventSubscription $
--             newDeleteEventSubscriptionResponse
--
--         , responseDeleteHsmClientCertificate $
--             newDeleteHsmClientCertificateResponse
--
--         , responseDeleteHsmConfiguration $
--             newDeleteHsmConfigurationResponse
--
--         , responseDeletePartner $
--             newPartnerIntegrationOutputMessage
--
--         , responseDeleteScheduledAction $
--             newDeleteScheduledActionResponse
--
--         , responseDeleteSnapshotCopyGrant $
--             newDeleteSnapshotCopyGrantResponse
--
--         , responseDeleteSnapshotSchedule $
--             newDeleteSnapshotScheduleResponse
--
--         , responseDeleteTags $
--             newDeleteTagsResponse
--
--         , responseDeleteUsageLimit $
--             newDeleteUsageLimitResponse
--
--         , responseDescribeAccountAttributes $
--             newDescribeAccountAttributesResponse
--
--         , responseDescribeAuthenticationProfiles $
--             newDescribeAuthenticationProfilesResponse
--
--         , responseDescribeClusterDbRevisions $
--             newDescribeClusterDbRevisionsResponse
--
--         , responseDescribeClusterParameterGroups $
--             newDescribeClusterParameterGroupsResponse
--
--         , responseDescribeClusterParameters $
--             newDescribeClusterParametersResponse
--
--         , responseDescribeClusterSecurityGroups $
--             newDescribeClusterSecurityGroupsResponse
--
--         , responseDescribeClusterSnapshots $
--             newDescribeClusterSnapshotsResponse
--
--         , responseDescribeClusterSubnetGroups $
--             newDescribeClusterSubnetGroupsResponse
--
--         , responseDescribeClusterTracks $
--             newDescribeClusterTracksResponse
--
--         , responseDescribeClusterVersions $
--             newDescribeClusterVersionsResponse
--
--         , responseDescribeClusters $
--             newDescribeClustersResponse
--
--         , responseDescribeDataShares $
--             newDescribeDataSharesResponse
--
--         , responseDescribeDataSharesForConsumer $
--             newDescribeDataSharesForConsumerResponse
--
--         , responseDescribeDataSharesForProducer $
--             newDescribeDataSharesForProducerResponse
--
--         , responseDescribeDefaultClusterParameters $
--             newDescribeDefaultClusterParametersResponse
--
--         , responseDescribeEndpointAccess $
--             newDescribeEndpointAccessResponse
--
--         , responseDescribeEndpointAuthorization $
--             newDescribeEndpointAuthorizationResponse
--
--         , responseDescribeEventCategories $
--             newDescribeEventCategoriesResponse
--
--         , responseDescribeEventSubscriptions $
--             newDescribeEventSubscriptionsResponse
--
--         , responseDescribeEvents $
--             newDescribeEventsResponse
--
--         , responseDescribeHsmClientCertificates $
--             newDescribeHsmClientCertificatesResponse
--
--         , responseDescribeHsmConfigurations $
--             newDescribeHsmConfigurationsResponse
--
--         , responseDescribeLoggingStatus $
--             newLoggingStatus
--
--         , responseDescribeNodeConfigurationOptions $
--             newDescribeNodeConfigurationOptionsResponse
--
--         , responseDescribeOrderableClusterOptions $
--             newDescribeOrderableClusterOptionsResponse
--
--         , responseDescribePartners $
--             newDescribePartnersResponse
--
--         , responseDescribeReservedNodeExchangeStatus $
--             newDescribeReservedNodeExchangeStatusResponse
--
--         , responseDescribeReservedNodeOfferings $
--             newDescribeReservedNodeOfferingsResponse
--
--         , responseDescribeReservedNodes $
--             newDescribeReservedNodesResponse
--
--         , responseDescribeResize $
--             newResizeProgressMessage
--
--         , responseDescribeScheduledActions $
--             newDescribeScheduledActionsResponse
--
--         , responseDescribeSnapshotCopyGrants $
--             newDescribeSnapshotCopyGrantsResponse
--
--         , responseDescribeSnapshotSchedules $
--             newDescribeSnapshotSchedulesResponse
--
--         , responseDescribeStorage $
--             newDescribeStorageResponse
--
--         , responseDescribeTableRestoreStatus $
--             newDescribeTableRestoreStatusResponse
--
--         , responseDescribeTags $
--             newDescribeTagsResponse
--
--         , responseDescribeUsageLimits $
--             newDescribeUsageLimitsResponse
--
--         , responseDisableLogging $
--             newLoggingStatus
--
--         , responseDisableSnapshotCopy $
--             newDisableSnapshotCopyResponse
--
--         , responseDisassociateDataShareConsumer $
--             newDataShare
--
--         , responseEnableLogging $
--             newLoggingStatus
--
--         , responseEnableSnapshotCopy $
--             newEnableSnapshotCopyResponse
--
--         , responseGetClusterCredentials $
--             newGetClusterCredentialsResponse
--
--         , responseGetClusterCredentialsWithIAM $
--             newGetClusterCredentialsWithIAMResponse
--
--         , responseGetReservedNodeExchangeConfigurationOptions $
--             newGetReservedNodeExchangeConfigurationOptionsResponse
--
--         , responseGetReservedNodeExchangeOfferings $
--             newGetReservedNodeExchangeOfferingsResponse
--
--         , responseModifyAquaConfiguration $
--             newModifyAquaConfigurationResponse
--
--         , responseModifyAuthenticationProfile $
--             newModifyAuthenticationProfileResponse
--
--         , responseModifyCluster $
--             newModifyClusterResponse
--
--         , responseModifyClusterDbRevision $
--             newModifyClusterDbRevisionResponse
--
--         , responseModifyClusterIamRoles $
--             newModifyClusterIamRolesResponse
--
--         , responseModifyClusterMaintenance $
--             newModifyClusterMaintenanceResponse
--
--         , responseModifyClusterParameterGroup $
--             newClusterParameterGroupNameMessage
--
--         , responseModifyClusterSnapshot $
--             newModifyClusterSnapshotResponse
--
--         , responseModifyClusterSnapshotSchedule $
--             newModifyClusterSnapshotScheduleResponse
--
--         , responseModifyClusterSubnetGroup $
--             newModifyClusterSubnetGroupResponse
--
--         , responseModifyEndpointAccess $
--             newEndpointAccess
--
--         , responseModifyEventSubscription $
--             newModifyEventSubscriptionResponse
--
--         , responseModifyScheduledAction $
--             newScheduledAction
--
--         , responseModifySnapshotCopyRetentionPeriod $
--             newModifySnapshotCopyRetentionPeriodResponse
--
--         , responseModifySnapshotSchedule $
--             newSnapshotSchedule
--
--         , responseModifyUsageLimit $
--             newUsageLimit
--
--         , responsePauseCluster $
--             newPauseClusterResponse
--
--         , responsePurchaseReservedNodeOffering $
--             newPurchaseReservedNodeOfferingResponse
--
--         , responseRebootCluster $
--             newRebootClusterResponse
--
--         , responseRejectDataShare $
--             newDataShare
--
--         , responseResetClusterParameterGroup $
--             newClusterParameterGroupNameMessage
--
--         , responseResizeCluster $
--             newResizeClusterResponse
--
--         , responseRestoreFromClusterSnapshot $
--             newRestoreFromClusterSnapshotResponse
--
--         , responseRestoreTableFromClusterSnapshot $
--             newRestoreTableFromClusterSnapshotResponse
--
--         , responseResumeCluster $
--             newResumeClusterResponse
--
--         , responseRevokeClusterSecurityGroupIngress $
--             newRevokeClusterSecurityGroupIngressResponse
--
--         , responseRevokeEndpointAccess $
--             newEndpointAuthorization
--
--         , responseRevokeSnapshotAccess $
--             newRevokeSnapshotAccessResponse
--
--         , responseRotateEncryptionKey $
--             newRotateEncryptionKeyResponse
--
--         , responseUpdatePartnerStatus $
--             newPartnerIntegrationOutputMessage
--
--           ]
--     ]

-- Requests

requestAcceptReservedNodeExchange :: AcceptReservedNodeExchange -> TestTree
requestAcceptReservedNodeExchange =
  req
    "AcceptReservedNodeExchange"
    "fixture/AcceptReservedNodeExchange.yaml"

requestAddPartner :: AddPartner -> TestTree
requestAddPartner =
  req
    "AddPartner"
    "fixture/AddPartner.yaml"

requestAssociateDataShareConsumer :: AssociateDataShareConsumer -> TestTree
requestAssociateDataShareConsumer =
  req
    "AssociateDataShareConsumer"
    "fixture/AssociateDataShareConsumer.yaml"

requestAuthorizeClusterSecurityGroupIngress :: AuthorizeClusterSecurityGroupIngress -> TestTree
requestAuthorizeClusterSecurityGroupIngress =
  req
    "AuthorizeClusterSecurityGroupIngress"
    "fixture/AuthorizeClusterSecurityGroupIngress.yaml"

requestAuthorizeDataShare :: AuthorizeDataShare -> TestTree
requestAuthorizeDataShare =
  req
    "AuthorizeDataShare"
    "fixture/AuthorizeDataShare.yaml"

requestAuthorizeEndpointAccess :: AuthorizeEndpointAccess -> TestTree
requestAuthorizeEndpointAccess =
  req
    "AuthorizeEndpointAccess"
    "fixture/AuthorizeEndpointAccess.yaml"

requestAuthorizeSnapshotAccess :: AuthorizeSnapshotAccess -> TestTree
requestAuthorizeSnapshotAccess =
  req
    "AuthorizeSnapshotAccess"
    "fixture/AuthorizeSnapshotAccess.yaml"

requestBatchDeleteClusterSnapshots :: BatchDeleteClusterSnapshots -> TestTree
requestBatchDeleteClusterSnapshots =
  req
    "BatchDeleteClusterSnapshots"
    "fixture/BatchDeleteClusterSnapshots.yaml"

requestBatchModifyClusterSnapshots :: BatchModifyClusterSnapshots -> TestTree
requestBatchModifyClusterSnapshots =
  req
    "BatchModifyClusterSnapshots"
    "fixture/BatchModifyClusterSnapshots.yaml"

requestCancelResize :: CancelResize -> TestTree
requestCancelResize =
  req
    "CancelResize"
    "fixture/CancelResize.yaml"

requestCopyClusterSnapshot :: CopyClusterSnapshot -> TestTree
requestCopyClusterSnapshot =
  req
    "CopyClusterSnapshot"
    "fixture/CopyClusterSnapshot.yaml"

requestCreateAuthenticationProfile :: CreateAuthenticationProfile -> TestTree
requestCreateAuthenticationProfile =
  req
    "CreateAuthenticationProfile"
    "fixture/CreateAuthenticationProfile.yaml"

requestCreateCluster :: CreateCluster -> TestTree
requestCreateCluster =
  req
    "CreateCluster"
    "fixture/CreateCluster.yaml"

requestCreateClusterParameterGroup :: CreateClusterParameterGroup -> TestTree
requestCreateClusterParameterGroup =
  req
    "CreateClusterParameterGroup"
    "fixture/CreateClusterParameterGroup.yaml"

requestCreateClusterSecurityGroup :: CreateClusterSecurityGroup -> TestTree
requestCreateClusterSecurityGroup =
  req
    "CreateClusterSecurityGroup"
    "fixture/CreateClusterSecurityGroup.yaml"

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

requestCreateEndpointAccess :: CreateEndpointAccess -> TestTree
requestCreateEndpointAccess =
  req
    "CreateEndpointAccess"
    "fixture/CreateEndpointAccess.yaml"

requestCreateEventSubscription :: CreateEventSubscription -> TestTree
requestCreateEventSubscription =
  req
    "CreateEventSubscription"
    "fixture/CreateEventSubscription.yaml"

requestCreateHsmClientCertificate :: CreateHsmClientCertificate -> TestTree
requestCreateHsmClientCertificate =
  req
    "CreateHsmClientCertificate"
    "fixture/CreateHsmClientCertificate.yaml"

requestCreateHsmConfiguration :: CreateHsmConfiguration -> TestTree
requestCreateHsmConfiguration =
  req
    "CreateHsmConfiguration"
    "fixture/CreateHsmConfiguration.yaml"

requestCreateScheduledAction :: CreateScheduledAction -> TestTree
requestCreateScheduledAction =
  req
    "CreateScheduledAction"
    "fixture/CreateScheduledAction.yaml"

requestCreateSnapshotCopyGrant :: CreateSnapshotCopyGrant -> TestTree
requestCreateSnapshotCopyGrant =
  req
    "CreateSnapshotCopyGrant"
    "fixture/CreateSnapshotCopyGrant.yaml"

requestCreateSnapshotSchedule :: CreateSnapshotSchedule -> TestTree
requestCreateSnapshotSchedule =
  req
    "CreateSnapshotSchedule"
    "fixture/CreateSnapshotSchedule.yaml"

requestCreateTags :: CreateTags -> TestTree
requestCreateTags =
  req
    "CreateTags"
    "fixture/CreateTags.yaml"

requestCreateUsageLimit :: CreateUsageLimit -> TestTree
requestCreateUsageLimit =
  req
    "CreateUsageLimit"
    "fixture/CreateUsageLimit.yaml"

requestDeauthorizeDataShare :: DeauthorizeDataShare -> TestTree
requestDeauthorizeDataShare =
  req
    "DeauthorizeDataShare"
    "fixture/DeauthorizeDataShare.yaml"

requestDeleteAuthenticationProfile :: DeleteAuthenticationProfile -> TestTree
requestDeleteAuthenticationProfile =
  req
    "DeleteAuthenticationProfile"
    "fixture/DeleteAuthenticationProfile.yaml"

requestDeleteCluster :: DeleteCluster -> TestTree
requestDeleteCluster =
  req
    "DeleteCluster"
    "fixture/DeleteCluster.yaml"

requestDeleteClusterParameterGroup :: DeleteClusterParameterGroup -> TestTree
requestDeleteClusterParameterGroup =
  req
    "DeleteClusterParameterGroup"
    "fixture/DeleteClusterParameterGroup.yaml"

requestDeleteClusterSecurityGroup :: DeleteClusterSecurityGroup -> TestTree
requestDeleteClusterSecurityGroup =
  req
    "DeleteClusterSecurityGroup"
    "fixture/DeleteClusterSecurityGroup.yaml"

requestDeleteClusterSnapshot :: DeleteClusterSnapshot -> TestTree
requestDeleteClusterSnapshot =
  req
    "DeleteClusterSnapshot"
    "fixture/DeleteClusterSnapshot.yaml"

requestDeleteClusterSubnetGroup :: DeleteClusterSubnetGroup -> TestTree
requestDeleteClusterSubnetGroup =
  req
    "DeleteClusterSubnetGroup"
    "fixture/DeleteClusterSubnetGroup.yaml"

requestDeleteEndpointAccess :: DeleteEndpointAccess -> TestTree
requestDeleteEndpointAccess =
  req
    "DeleteEndpointAccess"
    "fixture/DeleteEndpointAccess.yaml"

requestDeleteEventSubscription :: DeleteEventSubscription -> TestTree
requestDeleteEventSubscription =
  req
    "DeleteEventSubscription"
    "fixture/DeleteEventSubscription.yaml"

requestDeleteHsmClientCertificate :: DeleteHsmClientCertificate -> TestTree
requestDeleteHsmClientCertificate =
  req
    "DeleteHsmClientCertificate"
    "fixture/DeleteHsmClientCertificate.yaml"

requestDeleteHsmConfiguration :: DeleteHsmConfiguration -> TestTree
requestDeleteHsmConfiguration =
  req
    "DeleteHsmConfiguration"
    "fixture/DeleteHsmConfiguration.yaml"

requestDeletePartner :: DeletePartner -> TestTree
requestDeletePartner =
  req
    "DeletePartner"
    "fixture/DeletePartner.yaml"

requestDeleteScheduledAction :: DeleteScheduledAction -> TestTree
requestDeleteScheduledAction =
  req
    "DeleteScheduledAction"
    "fixture/DeleteScheduledAction.yaml"

requestDeleteSnapshotCopyGrant :: DeleteSnapshotCopyGrant -> TestTree
requestDeleteSnapshotCopyGrant =
  req
    "DeleteSnapshotCopyGrant"
    "fixture/DeleteSnapshotCopyGrant.yaml"

requestDeleteSnapshotSchedule :: DeleteSnapshotSchedule -> TestTree
requestDeleteSnapshotSchedule =
  req
    "DeleteSnapshotSchedule"
    "fixture/DeleteSnapshotSchedule.yaml"

requestDeleteTags :: DeleteTags -> TestTree
requestDeleteTags =
  req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

requestDeleteUsageLimit :: DeleteUsageLimit -> TestTree
requestDeleteUsageLimit =
  req
    "DeleteUsageLimit"
    "fixture/DeleteUsageLimit.yaml"

requestDescribeAccountAttributes :: DescribeAccountAttributes -> TestTree
requestDescribeAccountAttributes =
  req
    "DescribeAccountAttributes"
    "fixture/DescribeAccountAttributes.yaml"

requestDescribeAuthenticationProfiles :: DescribeAuthenticationProfiles -> TestTree
requestDescribeAuthenticationProfiles =
  req
    "DescribeAuthenticationProfiles"
    "fixture/DescribeAuthenticationProfiles.yaml"

requestDescribeClusterDbRevisions :: DescribeClusterDbRevisions -> TestTree
requestDescribeClusterDbRevisions =
  req
    "DescribeClusterDbRevisions"
    "fixture/DescribeClusterDbRevisions.yaml"

requestDescribeClusterParameterGroups :: DescribeClusterParameterGroups -> TestTree
requestDescribeClusterParameterGroups =
  req
    "DescribeClusterParameterGroups"
    "fixture/DescribeClusterParameterGroups.yaml"

requestDescribeClusterParameters :: DescribeClusterParameters -> TestTree
requestDescribeClusterParameters =
  req
    "DescribeClusterParameters"
    "fixture/DescribeClusterParameters.yaml"

requestDescribeClusterSecurityGroups :: DescribeClusterSecurityGroups -> TestTree
requestDescribeClusterSecurityGroups =
  req
    "DescribeClusterSecurityGroups"
    "fixture/DescribeClusterSecurityGroups.yaml"

requestDescribeClusterSnapshots :: DescribeClusterSnapshots -> TestTree
requestDescribeClusterSnapshots =
  req
    "DescribeClusterSnapshots"
    "fixture/DescribeClusterSnapshots.yaml"

requestDescribeClusterSubnetGroups :: DescribeClusterSubnetGroups -> TestTree
requestDescribeClusterSubnetGroups =
  req
    "DescribeClusterSubnetGroups"
    "fixture/DescribeClusterSubnetGroups.yaml"

requestDescribeClusterTracks :: DescribeClusterTracks -> TestTree
requestDescribeClusterTracks =
  req
    "DescribeClusterTracks"
    "fixture/DescribeClusterTracks.yaml"

requestDescribeClusterVersions :: DescribeClusterVersions -> TestTree
requestDescribeClusterVersions =
  req
    "DescribeClusterVersions"
    "fixture/DescribeClusterVersions.yaml"

requestDescribeClusters :: DescribeClusters -> TestTree
requestDescribeClusters =
  req
    "DescribeClusters"
    "fixture/DescribeClusters.yaml"

requestDescribeDataShares :: DescribeDataShares -> TestTree
requestDescribeDataShares =
  req
    "DescribeDataShares"
    "fixture/DescribeDataShares.yaml"

requestDescribeDataSharesForConsumer :: DescribeDataSharesForConsumer -> TestTree
requestDescribeDataSharesForConsumer =
  req
    "DescribeDataSharesForConsumer"
    "fixture/DescribeDataSharesForConsumer.yaml"

requestDescribeDataSharesForProducer :: DescribeDataSharesForProducer -> TestTree
requestDescribeDataSharesForProducer =
  req
    "DescribeDataSharesForProducer"
    "fixture/DescribeDataSharesForProducer.yaml"

requestDescribeDefaultClusterParameters :: DescribeDefaultClusterParameters -> TestTree
requestDescribeDefaultClusterParameters =
  req
    "DescribeDefaultClusterParameters"
    "fixture/DescribeDefaultClusterParameters.yaml"

requestDescribeEndpointAccess :: DescribeEndpointAccess -> TestTree
requestDescribeEndpointAccess =
  req
    "DescribeEndpointAccess"
    "fixture/DescribeEndpointAccess.yaml"

requestDescribeEndpointAuthorization :: DescribeEndpointAuthorization -> TestTree
requestDescribeEndpointAuthorization =
  req
    "DescribeEndpointAuthorization"
    "fixture/DescribeEndpointAuthorization.yaml"

requestDescribeEventCategories :: DescribeEventCategories -> TestTree
requestDescribeEventCategories =
  req
    "DescribeEventCategories"
    "fixture/DescribeEventCategories.yaml"

requestDescribeEventSubscriptions :: DescribeEventSubscriptions -> TestTree
requestDescribeEventSubscriptions =
  req
    "DescribeEventSubscriptions"
    "fixture/DescribeEventSubscriptions.yaml"

requestDescribeEvents :: DescribeEvents -> TestTree
requestDescribeEvents =
  req
    "DescribeEvents"
    "fixture/DescribeEvents.yaml"

requestDescribeHsmClientCertificates :: DescribeHsmClientCertificates -> TestTree
requestDescribeHsmClientCertificates =
  req
    "DescribeHsmClientCertificates"
    "fixture/DescribeHsmClientCertificates.yaml"

requestDescribeHsmConfigurations :: DescribeHsmConfigurations -> TestTree
requestDescribeHsmConfigurations =
  req
    "DescribeHsmConfigurations"
    "fixture/DescribeHsmConfigurations.yaml"

requestDescribeLoggingStatus :: DescribeLoggingStatus -> TestTree
requestDescribeLoggingStatus =
  req
    "DescribeLoggingStatus"
    "fixture/DescribeLoggingStatus.yaml"

requestDescribeNodeConfigurationOptions :: DescribeNodeConfigurationOptions -> TestTree
requestDescribeNodeConfigurationOptions =
  req
    "DescribeNodeConfigurationOptions"
    "fixture/DescribeNodeConfigurationOptions.yaml"

requestDescribeOrderableClusterOptions :: DescribeOrderableClusterOptions -> TestTree
requestDescribeOrderableClusterOptions =
  req
    "DescribeOrderableClusterOptions"
    "fixture/DescribeOrderableClusterOptions.yaml"

requestDescribePartners :: DescribePartners -> TestTree
requestDescribePartners =
  req
    "DescribePartners"
    "fixture/DescribePartners.yaml"

requestDescribeReservedNodeExchangeStatus :: DescribeReservedNodeExchangeStatus -> TestTree
requestDescribeReservedNodeExchangeStatus =
  req
    "DescribeReservedNodeExchangeStatus"
    "fixture/DescribeReservedNodeExchangeStatus.yaml"

requestDescribeReservedNodeOfferings :: DescribeReservedNodeOfferings -> TestTree
requestDescribeReservedNodeOfferings =
  req
    "DescribeReservedNodeOfferings"
    "fixture/DescribeReservedNodeOfferings.yaml"

requestDescribeReservedNodes :: DescribeReservedNodes -> TestTree
requestDescribeReservedNodes =
  req
    "DescribeReservedNodes"
    "fixture/DescribeReservedNodes.yaml"

requestDescribeResize :: DescribeResize -> TestTree
requestDescribeResize =
  req
    "DescribeResize"
    "fixture/DescribeResize.yaml"

requestDescribeScheduledActions :: DescribeScheduledActions -> TestTree
requestDescribeScheduledActions =
  req
    "DescribeScheduledActions"
    "fixture/DescribeScheduledActions.yaml"

requestDescribeSnapshotCopyGrants :: DescribeSnapshotCopyGrants -> TestTree
requestDescribeSnapshotCopyGrants =
  req
    "DescribeSnapshotCopyGrants"
    "fixture/DescribeSnapshotCopyGrants.yaml"

requestDescribeSnapshotSchedules :: DescribeSnapshotSchedules -> TestTree
requestDescribeSnapshotSchedules =
  req
    "DescribeSnapshotSchedules"
    "fixture/DescribeSnapshotSchedules.yaml"

requestDescribeStorage :: DescribeStorage -> TestTree
requestDescribeStorage =
  req
    "DescribeStorage"
    "fixture/DescribeStorage.yaml"

requestDescribeTableRestoreStatus :: DescribeTableRestoreStatus -> TestTree
requestDescribeTableRestoreStatus =
  req
    "DescribeTableRestoreStatus"
    "fixture/DescribeTableRestoreStatus.yaml"

requestDescribeTags :: DescribeTags -> TestTree
requestDescribeTags =
  req
    "DescribeTags"
    "fixture/DescribeTags.yaml"

requestDescribeUsageLimits :: DescribeUsageLimits -> TestTree
requestDescribeUsageLimits =
  req
    "DescribeUsageLimits"
    "fixture/DescribeUsageLimits.yaml"

requestDisableLogging :: DisableLogging -> TestTree
requestDisableLogging =
  req
    "DisableLogging"
    "fixture/DisableLogging.yaml"

requestDisableSnapshotCopy :: DisableSnapshotCopy -> TestTree
requestDisableSnapshotCopy =
  req
    "DisableSnapshotCopy"
    "fixture/DisableSnapshotCopy.yaml"

requestDisassociateDataShareConsumer :: DisassociateDataShareConsumer -> TestTree
requestDisassociateDataShareConsumer =
  req
    "DisassociateDataShareConsumer"
    "fixture/DisassociateDataShareConsumer.yaml"

requestEnableLogging :: EnableLogging -> TestTree
requestEnableLogging =
  req
    "EnableLogging"
    "fixture/EnableLogging.yaml"

requestEnableSnapshotCopy :: EnableSnapshotCopy -> TestTree
requestEnableSnapshotCopy =
  req
    "EnableSnapshotCopy"
    "fixture/EnableSnapshotCopy.yaml"

requestGetClusterCredentials :: GetClusterCredentials -> TestTree
requestGetClusterCredentials =
  req
    "GetClusterCredentials"
    "fixture/GetClusterCredentials.yaml"

requestGetClusterCredentialsWithIAM :: GetClusterCredentialsWithIAM -> TestTree
requestGetClusterCredentialsWithIAM =
  req
    "GetClusterCredentialsWithIAM"
    "fixture/GetClusterCredentialsWithIAM.yaml"

requestGetReservedNodeExchangeConfigurationOptions :: GetReservedNodeExchangeConfigurationOptions -> TestTree
requestGetReservedNodeExchangeConfigurationOptions =
  req
    "GetReservedNodeExchangeConfigurationOptions"
    "fixture/GetReservedNodeExchangeConfigurationOptions.yaml"

requestGetReservedNodeExchangeOfferings :: GetReservedNodeExchangeOfferings -> TestTree
requestGetReservedNodeExchangeOfferings =
  req
    "GetReservedNodeExchangeOfferings"
    "fixture/GetReservedNodeExchangeOfferings.yaml"

requestModifyAquaConfiguration :: ModifyAquaConfiguration -> TestTree
requestModifyAquaConfiguration =
  req
    "ModifyAquaConfiguration"
    "fixture/ModifyAquaConfiguration.yaml"

requestModifyAuthenticationProfile :: ModifyAuthenticationProfile -> TestTree
requestModifyAuthenticationProfile =
  req
    "ModifyAuthenticationProfile"
    "fixture/ModifyAuthenticationProfile.yaml"

requestModifyCluster :: ModifyCluster -> TestTree
requestModifyCluster =
  req
    "ModifyCluster"
    "fixture/ModifyCluster.yaml"

requestModifyClusterDbRevision :: ModifyClusterDbRevision -> TestTree
requestModifyClusterDbRevision =
  req
    "ModifyClusterDbRevision"
    "fixture/ModifyClusterDbRevision.yaml"

requestModifyClusterIamRoles :: ModifyClusterIamRoles -> TestTree
requestModifyClusterIamRoles =
  req
    "ModifyClusterIamRoles"
    "fixture/ModifyClusterIamRoles.yaml"

requestModifyClusterMaintenance :: ModifyClusterMaintenance -> TestTree
requestModifyClusterMaintenance =
  req
    "ModifyClusterMaintenance"
    "fixture/ModifyClusterMaintenance.yaml"

requestModifyClusterParameterGroup :: ModifyClusterParameterGroup -> TestTree
requestModifyClusterParameterGroup =
  req
    "ModifyClusterParameterGroup"
    "fixture/ModifyClusterParameterGroup.yaml"

requestModifyClusterSnapshot :: ModifyClusterSnapshot -> TestTree
requestModifyClusterSnapshot =
  req
    "ModifyClusterSnapshot"
    "fixture/ModifyClusterSnapshot.yaml"

requestModifyClusterSnapshotSchedule :: ModifyClusterSnapshotSchedule -> TestTree
requestModifyClusterSnapshotSchedule =
  req
    "ModifyClusterSnapshotSchedule"
    "fixture/ModifyClusterSnapshotSchedule.yaml"

requestModifyClusterSubnetGroup :: ModifyClusterSubnetGroup -> TestTree
requestModifyClusterSubnetGroup =
  req
    "ModifyClusterSubnetGroup"
    "fixture/ModifyClusterSubnetGroup.yaml"

requestModifyEndpointAccess :: ModifyEndpointAccess -> TestTree
requestModifyEndpointAccess =
  req
    "ModifyEndpointAccess"
    "fixture/ModifyEndpointAccess.yaml"

requestModifyEventSubscription :: ModifyEventSubscription -> TestTree
requestModifyEventSubscription =
  req
    "ModifyEventSubscription"
    "fixture/ModifyEventSubscription.yaml"

requestModifyScheduledAction :: ModifyScheduledAction -> TestTree
requestModifyScheduledAction =
  req
    "ModifyScheduledAction"
    "fixture/ModifyScheduledAction.yaml"

requestModifySnapshotCopyRetentionPeriod :: ModifySnapshotCopyRetentionPeriod -> TestTree
requestModifySnapshotCopyRetentionPeriod =
  req
    "ModifySnapshotCopyRetentionPeriod"
    "fixture/ModifySnapshotCopyRetentionPeriod.yaml"

requestModifySnapshotSchedule :: ModifySnapshotSchedule -> TestTree
requestModifySnapshotSchedule =
  req
    "ModifySnapshotSchedule"
    "fixture/ModifySnapshotSchedule.yaml"

requestModifyUsageLimit :: ModifyUsageLimit -> TestTree
requestModifyUsageLimit =
  req
    "ModifyUsageLimit"
    "fixture/ModifyUsageLimit.yaml"

requestPauseCluster :: PauseCluster -> TestTree
requestPauseCluster =
  req
    "PauseCluster"
    "fixture/PauseCluster.yaml"

requestPurchaseReservedNodeOffering :: PurchaseReservedNodeOffering -> TestTree
requestPurchaseReservedNodeOffering =
  req
    "PurchaseReservedNodeOffering"
    "fixture/PurchaseReservedNodeOffering.yaml"

requestRebootCluster :: RebootCluster -> TestTree
requestRebootCluster =
  req
    "RebootCluster"
    "fixture/RebootCluster.yaml"

requestRejectDataShare :: RejectDataShare -> TestTree
requestRejectDataShare =
  req
    "RejectDataShare"
    "fixture/RejectDataShare.yaml"

requestResetClusterParameterGroup :: ResetClusterParameterGroup -> TestTree
requestResetClusterParameterGroup =
  req
    "ResetClusterParameterGroup"
    "fixture/ResetClusterParameterGroup.yaml"

requestResizeCluster :: ResizeCluster -> TestTree
requestResizeCluster =
  req
    "ResizeCluster"
    "fixture/ResizeCluster.yaml"

requestRestoreFromClusterSnapshot :: RestoreFromClusterSnapshot -> TestTree
requestRestoreFromClusterSnapshot =
  req
    "RestoreFromClusterSnapshot"
    "fixture/RestoreFromClusterSnapshot.yaml"

requestRestoreTableFromClusterSnapshot :: RestoreTableFromClusterSnapshot -> TestTree
requestRestoreTableFromClusterSnapshot =
  req
    "RestoreTableFromClusterSnapshot"
    "fixture/RestoreTableFromClusterSnapshot.yaml"

requestResumeCluster :: ResumeCluster -> TestTree
requestResumeCluster =
  req
    "ResumeCluster"
    "fixture/ResumeCluster.yaml"

requestRevokeClusterSecurityGroupIngress :: RevokeClusterSecurityGroupIngress -> TestTree
requestRevokeClusterSecurityGroupIngress =
  req
    "RevokeClusterSecurityGroupIngress"
    "fixture/RevokeClusterSecurityGroupIngress.yaml"

requestRevokeEndpointAccess :: RevokeEndpointAccess -> TestTree
requestRevokeEndpointAccess =
  req
    "RevokeEndpointAccess"
    "fixture/RevokeEndpointAccess.yaml"

requestRevokeSnapshotAccess :: RevokeSnapshotAccess -> TestTree
requestRevokeSnapshotAccess =
  req
    "RevokeSnapshotAccess"
    "fixture/RevokeSnapshotAccess.yaml"

requestRotateEncryptionKey :: RotateEncryptionKey -> TestTree
requestRotateEncryptionKey =
  req
    "RotateEncryptionKey"
    "fixture/RotateEncryptionKey.yaml"

requestUpdatePartnerStatus :: UpdatePartnerStatus -> TestTree
requestUpdatePartnerStatus =
  req
    "UpdatePartnerStatus"
    "fixture/UpdatePartnerStatus.yaml"

-- Responses

responseAcceptReservedNodeExchange :: AcceptReservedNodeExchangeResponse -> TestTree
responseAcceptReservedNodeExchange =
  res
    "AcceptReservedNodeExchangeResponse"
    "fixture/AcceptReservedNodeExchangeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptReservedNodeExchange)

responseAddPartner :: PartnerIntegrationOutputMessage -> TestTree
responseAddPartner =
  res
    "AddPartnerResponse"
    "fixture/AddPartnerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddPartner)

responseAssociateDataShareConsumer :: DataShare -> TestTree
responseAssociateDataShareConsumer =
  res
    "AssociateDataShareConsumerResponse"
    "fixture/AssociateDataShareConsumerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateDataShareConsumer)

responseAuthorizeClusterSecurityGroupIngress :: AuthorizeClusterSecurityGroupIngressResponse -> TestTree
responseAuthorizeClusterSecurityGroupIngress =
  res
    "AuthorizeClusterSecurityGroupIngressResponse"
    "fixture/AuthorizeClusterSecurityGroupIngressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AuthorizeClusterSecurityGroupIngress)

responseAuthorizeDataShare :: DataShare -> TestTree
responseAuthorizeDataShare =
  res
    "AuthorizeDataShareResponse"
    "fixture/AuthorizeDataShareResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AuthorizeDataShare)

responseAuthorizeEndpointAccess :: EndpointAuthorization -> TestTree
responseAuthorizeEndpointAccess =
  res
    "AuthorizeEndpointAccessResponse"
    "fixture/AuthorizeEndpointAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AuthorizeEndpointAccess)

responseAuthorizeSnapshotAccess :: AuthorizeSnapshotAccessResponse -> TestTree
responseAuthorizeSnapshotAccess =
  res
    "AuthorizeSnapshotAccessResponse"
    "fixture/AuthorizeSnapshotAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AuthorizeSnapshotAccess)

responseBatchDeleteClusterSnapshots :: BatchDeleteClusterSnapshotsResponse -> TestTree
responseBatchDeleteClusterSnapshots =
  res
    "BatchDeleteClusterSnapshotsResponse"
    "fixture/BatchDeleteClusterSnapshotsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDeleteClusterSnapshots)

responseBatchModifyClusterSnapshots :: BatchModifyClusterSnapshotsResponse -> TestTree
responseBatchModifyClusterSnapshots =
  res
    "BatchModifyClusterSnapshotsResponse"
    "fixture/BatchModifyClusterSnapshotsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchModifyClusterSnapshots)

responseCancelResize :: ResizeProgressMessage -> TestTree
responseCancelResize =
  res
    "CancelResizeResponse"
    "fixture/CancelResizeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelResize)

responseCopyClusterSnapshot :: CopyClusterSnapshotResponse -> TestTree
responseCopyClusterSnapshot =
  res
    "CopyClusterSnapshotResponse"
    "fixture/CopyClusterSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CopyClusterSnapshot)

responseCreateAuthenticationProfile :: CreateAuthenticationProfileResponse -> TestTree
responseCreateAuthenticationProfile =
  res
    "CreateAuthenticationProfileResponse"
    "fixture/CreateAuthenticationProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAuthenticationProfile)

responseCreateCluster :: CreateClusterResponse -> TestTree
responseCreateCluster =
  res
    "CreateClusterResponse"
    "fixture/CreateClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCluster)

responseCreateClusterParameterGroup :: CreateClusterParameterGroupResponse -> TestTree
responseCreateClusterParameterGroup =
  res
    "CreateClusterParameterGroupResponse"
    "fixture/CreateClusterParameterGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateClusterParameterGroup)

responseCreateClusterSecurityGroup :: CreateClusterSecurityGroupResponse -> TestTree
responseCreateClusterSecurityGroup =
  res
    "CreateClusterSecurityGroupResponse"
    "fixture/CreateClusterSecurityGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateClusterSecurityGroup)

responseCreateClusterSnapshot :: CreateClusterSnapshotResponse -> TestTree
responseCreateClusterSnapshot =
  res
    "CreateClusterSnapshotResponse"
    "fixture/CreateClusterSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateClusterSnapshot)

responseCreateClusterSubnetGroup :: CreateClusterSubnetGroupResponse -> TestTree
responseCreateClusterSubnetGroup =
  res
    "CreateClusterSubnetGroupResponse"
    "fixture/CreateClusterSubnetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateClusterSubnetGroup)

responseCreateEndpointAccess :: EndpointAccess -> TestTree
responseCreateEndpointAccess =
  res
    "CreateEndpointAccessResponse"
    "fixture/CreateEndpointAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEndpointAccess)

responseCreateEventSubscription :: CreateEventSubscriptionResponse -> TestTree
responseCreateEventSubscription =
  res
    "CreateEventSubscriptionResponse"
    "fixture/CreateEventSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEventSubscription)

responseCreateHsmClientCertificate :: CreateHsmClientCertificateResponse -> TestTree
responseCreateHsmClientCertificate =
  res
    "CreateHsmClientCertificateResponse"
    "fixture/CreateHsmClientCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateHsmClientCertificate)

responseCreateHsmConfiguration :: CreateHsmConfigurationResponse -> TestTree
responseCreateHsmConfiguration =
  res
    "CreateHsmConfigurationResponse"
    "fixture/CreateHsmConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateHsmConfiguration)

responseCreateScheduledAction :: ScheduledAction -> TestTree
responseCreateScheduledAction =
  res
    "CreateScheduledActionResponse"
    "fixture/CreateScheduledActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateScheduledAction)

responseCreateSnapshotCopyGrant :: CreateSnapshotCopyGrantResponse -> TestTree
responseCreateSnapshotCopyGrant =
  res
    "CreateSnapshotCopyGrantResponse"
    "fixture/CreateSnapshotCopyGrantResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSnapshotCopyGrant)

responseCreateSnapshotSchedule :: SnapshotSchedule -> TestTree
responseCreateSnapshotSchedule =
  res
    "CreateSnapshotScheduleResponse"
    "fixture/CreateSnapshotScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSnapshotSchedule)

responseCreateTags :: CreateTagsResponse -> TestTree
responseCreateTags =
  res
    "CreateTagsResponse"
    "fixture/CreateTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTags)

responseCreateUsageLimit :: UsageLimit -> TestTree
responseCreateUsageLimit =
  res
    "CreateUsageLimitResponse"
    "fixture/CreateUsageLimitResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUsageLimit)

responseDeauthorizeDataShare :: DataShare -> TestTree
responseDeauthorizeDataShare =
  res
    "DeauthorizeDataShareResponse"
    "fixture/DeauthorizeDataShareResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeauthorizeDataShare)

responseDeleteAuthenticationProfile :: DeleteAuthenticationProfileResponse -> TestTree
responseDeleteAuthenticationProfile =
  res
    "DeleteAuthenticationProfileResponse"
    "fixture/DeleteAuthenticationProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAuthenticationProfile)

responseDeleteCluster :: DeleteClusterResponse -> TestTree
responseDeleteCluster =
  res
    "DeleteClusterResponse"
    "fixture/DeleteClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCluster)

responseDeleteClusterParameterGroup :: DeleteClusterParameterGroupResponse -> TestTree
responseDeleteClusterParameterGroup =
  res
    "DeleteClusterParameterGroupResponse"
    "fixture/DeleteClusterParameterGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteClusterParameterGroup)

responseDeleteClusterSecurityGroup :: DeleteClusterSecurityGroupResponse -> TestTree
responseDeleteClusterSecurityGroup =
  res
    "DeleteClusterSecurityGroupResponse"
    "fixture/DeleteClusterSecurityGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteClusterSecurityGroup)

responseDeleteClusterSnapshot :: DeleteClusterSnapshotResponse -> TestTree
responseDeleteClusterSnapshot =
  res
    "DeleteClusterSnapshotResponse"
    "fixture/DeleteClusterSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteClusterSnapshot)

responseDeleteClusterSubnetGroup :: DeleteClusterSubnetGroupResponse -> TestTree
responseDeleteClusterSubnetGroup =
  res
    "DeleteClusterSubnetGroupResponse"
    "fixture/DeleteClusterSubnetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteClusterSubnetGroup)

responseDeleteEndpointAccess :: EndpointAccess -> TestTree
responseDeleteEndpointAccess =
  res
    "DeleteEndpointAccessResponse"
    "fixture/DeleteEndpointAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEndpointAccess)

responseDeleteEventSubscription :: DeleteEventSubscriptionResponse -> TestTree
responseDeleteEventSubscription =
  res
    "DeleteEventSubscriptionResponse"
    "fixture/DeleteEventSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEventSubscription)

responseDeleteHsmClientCertificate :: DeleteHsmClientCertificateResponse -> TestTree
responseDeleteHsmClientCertificate =
  res
    "DeleteHsmClientCertificateResponse"
    "fixture/DeleteHsmClientCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteHsmClientCertificate)

responseDeleteHsmConfiguration :: DeleteHsmConfigurationResponse -> TestTree
responseDeleteHsmConfiguration =
  res
    "DeleteHsmConfigurationResponse"
    "fixture/DeleteHsmConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteHsmConfiguration)

responseDeletePartner :: PartnerIntegrationOutputMessage -> TestTree
responseDeletePartner =
  res
    "DeletePartnerResponse"
    "fixture/DeletePartnerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePartner)

responseDeleteScheduledAction :: DeleteScheduledActionResponse -> TestTree
responseDeleteScheduledAction =
  res
    "DeleteScheduledActionResponse"
    "fixture/DeleteScheduledActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteScheduledAction)

responseDeleteSnapshotCopyGrant :: DeleteSnapshotCopyGrantResponse -> TestTree
responseDeleteSnapshotCopyGrant =
  res
    "DeleteSnapshotCopyGrantResponse"
    "fixture/DeleteSnapshotCopyGrantResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSnapshotCopyGrant)

responseDeleteSnapshotSchedule :: DeleteSnapshotScheduleResponse -> TestTree
responseDeleteSnapshotSchedule =
  res
    "DeleteSnapshotScheduleResponse"
    "fixture/DeleteSnapshotScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSnapshotSchedule)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags =
  res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTags)

responseDeleteUsageLimit :: DeleteUsageLimitResponse -> TestTree
responseDeleteUsageLimit =
  res
    "DeleteUsageLimitResponse"
    "fixture/DeleteUsageLimitResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUsageLimit)

responseDescribeAccountAttributes :: DescribeAccountAttributesResponse -> TestTree
responseDescribeAccountAttributes =
  res
    "DescribeAccountAttributesResponse"
    "fixture/DescribeAccountAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAccountAttributes)

responseDescribeAuthenticationProfiles :: DescribeAuthenticationProfilesResponse -> TestTree
responseDescribeAuthenticationProfiles =
  res
    "DescribeAuthenticationProfilesResponse"
    "fixture/DescribeAuthenticationProfilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAuthenticationProfiles)

responseDescribeClusterDbRevisions :: DescribeClusterDbRevisionsResponse -> TestTree
responseDescribeClusterDbRevisions =
  res
    "DescribeClusterDbRevisionsResponse"
    "fixture/DescribeClusterDbRevisionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeClusterDbRevisions)

responseDescribeClusterParameterGroups :: DescribeClusterParameterGroupsResponse -> TestTree
responseDescribeClusterParameterGroups =
  res
    "DescribeClusterParameterGroupsResponse"
    "fixture/DescribeClusterParameterGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeClusterParameterGroups)

responseDescribeClusterParameters :: DescribeClusterParametersResponse -> TestTree
responseDescribeClusterParameters =
  res
    "DescribeClusterParametersResponse"
    "fixture/DescribeClusterParametersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeClusterParameters)

responseDescribeClusterSecurityGroups :: DescribeClusterSecurityGroupsResponse -> TestTree
responseDescribeClusterSecurityGroups =
  res
    "DescribeClusterSecurityGroupsResponse"
    "fixture/DescribeClusterSecurityGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeClusterSecurityGroups)

responseDescribeClusterSnapshots :: DescribeClusterSnapshotsResponse -> TestTree
responseDescribeClusterSnapshots =
  res
    "DescribeClusterSnapshotsResponse"
    "fixture/DescribeClusterSnapshotsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeClusterSnapshots)

responseDescribeClusterSubnetGroups :: DescribeClusterSubnetGroupsResponse -> TestTree
responseDescribeClusterSubnetGroups =
  res
    "DescribeClusterSubnetGroupsResponse"
    "fixture/DescribeClusterSubnetGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeClusterSubnetGroups)

responseDescribeClusterTracks :: DescribeClusterTracksResponse -> TestTree
responseDescribeClusterTracks =
  res
    "DescribeClusterTracksResponse"
    "fixture/DescribeClusterTracksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeClusterTracks)

responseDescribeClusterVersions :: DescribeClusterVersionsResponse -> TestTree
responseDescribeClusterVersions =
  res
    "DescribeClusterVersionsResponse"
    "fixture/DescribeClusterVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeClusterVersions)

responseDescribeClusters :: DescribeClustersResponse -> TestTree
responseDescribeClusters =
  res
    "DescribeClustersResponse"
    "fixture/DescribeClustersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeClusters)

responseDescribeDataShares :: DescribeDataSharesResponse -> TestTree
responseDescribeDataShares =
  res
    "DescribeDataSharesResponse"
    "fixture/DescribeDataSharesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDataShares)

responseDescribeDataSharesForConsumer :: DescribeDataSharesForConsumerResponse -> TestTree
responseDescribeDataSharesForConsumer =
  res
    "DescribeDataSharesForConsumerResponse"
    "fixture/DescribeDataSharesForConsumerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDataSharesForConsumer)

responseDescribeDataSharesForProducer :: DescribeDataSharesForProducerResponse -> TestTree
responseDescribeDataSharesForProducer =
  res
    "DescribeDataSharesForProducerResponse"
    "fixture/DescribeDataSharesForProducerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDataSharesForProducer)

responseDescribeDefaultClusterParameters :: DescribeDefaultClusterParametersResponse -> TestTree
responseDescribeDefaultClusterParameters =
  res
    "DescribeDefaultClusterParametersResponse"
    "fixture/DescribeDefaultClusterParametersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDefaultClusterParameters)

responseDescribeEndpointAccess :: DescribeEndpointAccessResponse -> TestTree
responseDescribeEndpointAccess =
  res
    "DescribeEndpointAccessResponse"
    "fixture/DescribeEndpointAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEndpointAccess)

responseDescribeEndpointAuthorization :: DescribeEndpointAuthorizationResponse -> TestTree
responseDescribeEndpointAuthorization =
  res
    "DescribeEndpointAuthorizationResponse"
    "fixture/DescribeEndpointAuthorizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEndpointAuthorization)

responseDescribeEventCategories :: DescribeEventCategoriesResponse -> TestTree
responseDescribeEventCategories =
  res
    "DescribeEventCategoriesResponse"
    "fixture/DescribeEventCategoriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEventCategories)

responseDescribeEventSubscriptions :: DescribeEventSubscriptionsResponse -> TestTree
responseDescribeEventSubscriptions =
  res
    "DescribeEventSubscriptionsResponse"
    "fixture/DescribeEventSubscriptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEventSubscriptions)

responseDescribeEvents :: DescribeEventsResponse -> TestTree
responseDescribeEvents =
  res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEvents)

responseDescribeHsmClientCertificates :: DescribeHsmClientCertificatesResponse -> TestTree
responseDescribeHsmClientCertificates =
  res
    "DescribeHsmClientCertificatesResponse"
    "fixture/DescribeHsmClientCertificatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeHsmClientCertificates)

responseDescribeHsmConfigurations :: DescribeHsmConfigurationsResponse -> TestTree
responseDescribeHsmConfigurations =
  res
    "DescribeHsmConfigurationsResponse"
    "fixture/DescribeHsmConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeHsmConfigurations)

responseDescribeLoggingStatus :: LoggingStatus -> TestTree
responseDescribeLoggingStatus =
  res
    "DescribeLoggingStatusResponse"
    "fixture/DescribeLoggingStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLoggingStatus)

responseDescribeNodeConfigurationOptions :: DescribeNodeConfigurationOptionsResponse -> TestTree
responseDescribeNodeConfigurationOptions =
  res
    "DescribeNodeConfigurationOptionsResponse"
    "fixture/DescribeNodeConfigurationOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeNodeConfigurationOptions)

responseDescribeOrderableClusterOptions :: DescribeOrderableClusterOptionsResponse -> TestTree
responseDescribeOrderableClusterOptions =
  res
    "DescribeOrderableClusterOptionsResponse"
    "fixture/DescribeOrderableClusterOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeOrderableClusterOptions)

responseDescribePartners :: DescribePartnersResponse -> TestTree
responseDescribePartners =
  res
    "DescribePartnersResponse"
    "fixture/DescribePartnersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePartners)

responseDescribeReservedNodeExchangeStatus :: DescribeReservedNodeExchangeStatusResponse -> TestTree
responseDescribeReservedNodeExchangeStatus =
  res
    "DescribeReservedNodeExchangeStatusResponse"
    "fixture/DescribeReservedNodeExchangeStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReservedNodeExchangeStatus)

responseDescribeReservedNodeOfferings :: DescribeReservedNodeOfferingsResponse -> TestTree
responseDescribeReservedNodeOfferings =
  res
    "DescribeReservedNodeOfferingsResponse"
    "fixture/DescribeReservedNodeOfferingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReservedNodeOfferings)

responseDescribeReservedNodes :: DescribeReservedNodesResponse -> TestTree
responseDescribeReservedNodes =
  res
    "DescribeReservedNodesResponse"
    "fixture/DescribeReservedNodesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReservedNodes)

responseDescribeResize :: ResizeProgressMessage -> TestTree
responseDescribeResize =
  res
    "DescribeResizeResponse"
    "fixture/DescribeResizeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeResize)

responseDescribeScheduledActions :: DescribeScheduledActionsResponse -> TestTree
responseDescribeScheduledActions =
  res
    "DescribeScheduledActionsResponse"
    "fixture/DescribeScheduledActionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeScheduledActions)

responseDescribeSnapshotCopyGrants :: DescribeSnapshotCopyGrantsResponse -> TestTree
responseDescribeSnapshotCopyGrants =
  res
    "DescribeSnapshotCopyGrantsResponse"
    "fixture/DescribeSnapshotCopyGrantsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSnapshotCopyGrants)

responseDescribeSnapshotSchedules :: DescribeSnapshotSchedulesResponse -> TestTree
responseDescribeSnapshotSchedules =
  res
    "DescribeSnapshotSchedulesResponse"
    "fixture/DescribeSnapshotSchedulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSnapshotSchedules)

responseDescribeStorage :: DescribeStorageResponse -> TestTree
responseDescribeStorage =
  res
    "DescribeStorageResponse"
    "fixture/DescribeStorageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStorage)

responseDescribeTableRestoreStatus :: DescribeTableRestoreStatusResponse -> TestTree
responseDescribeTableRestoreStatus =
  res
    "DescribeTableRestoreStatusResponse"
    "fixture/DescribeTableRestoreStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTableRestoreStatus)

responseDescribeTags :: DescribeTagsResponse -> TestTree
responseDescribeTags =
  res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTags)

responseDescribeUsageLimits :: DescribeUsageLimitsResponse -> TestTree
responseDescribeUsageLimits =
  res
    "DescribeUsageLimitsResponse"
    "fixture/DescribeUsageLimitsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeUsageLimits)

responseDisableLogging :: LoggingStatus -> TestTree
responseDisableLogging =
  res
    "DisableLoggingResponse"
    "fixture/DisableLoggingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableLogging)

responseDisableSnapshotCopy :: DisableSnapshotCopyResponse -> TestTree
responseDisableSnapshotCopy =
  res
    "DisableSnapshotCopyResponse"
    "fixture/DisableSnapshotCopyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableSnapshotCopy)

responseDisassociateDataShareConsumer :: DataShare -> TestTree
responseDisassociateDataShareConsumer =
  res
    "DisassociateDataShareConsumerResponse"
    "fixture/DisassociateDataShareConsumerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateDataShareConsumer)

responseEnableLogging :: LoggingStatus -> TestTree
responseEnableLogging =
  res
    "EnableLoggingResponse"
    "fixture/EnableLoggingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableLogging)

responseEnableSnapshotCopy :: EnableSnapshotCopyResponse -> TestTree
responseEnableSnapshotCopy =
  res
    "EnableSnapshotCopyResponse"
    "fixture/EnableSnapshotCopyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableSnapshotCopy)

responseGetClusterCredentials :: GetClusterCredentialsResponse -> TestTree
responseGetClusterCredentials =
  res
    "GetClusterCredentialsResponse"
    "fixture/GetClusterCredentialsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetClusterCredentials)

responseGetClusterCredentialsWithIAM :: GetClusterCredentialsWithIAMResponse -> TestTree
responseGetClusterCredentialsWithIAM =
  res
    "GetClusterCredentialsWithIAMResponse"
    "fixture/GetClusterCredentialsWithIAMResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetClusterCredentialsWithIAM)

responseGetReservedNodeExchangeConfigurationOptions :: GetReservedNodeExchangeConfigurationOptionsResponse -> TestTree
responseGetReservedNodeExchangeConfigurationOptions =
  res
    "GetReservedNodeExchangeConfigurationOptionsResponse"
    "fixture/GetReservedNodeExchangeConfigurationOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetReservedNodeExchangeConfigurationOptions)

responseGetReservedNodeExchangeOfferings :: GetReservedNodeExchangeOfferingsResponse -> TestTree
responseGetReservedNodeExchangeOfferings =
  res
    "GetReservedNodeExchangeOfferingsResponse"
    "fixture/GetReservedNodeExchangeOfferingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetReservedNodeExchangeOfferings)

responseModifyAquaConfiguration :: ModifyAquaConfigurationResponse -> TestTree
responseModifyAquaConfiguration =
  res
    "ModifyAquaConfigurationResponse"
    "fixture/ModifyAquaConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyAquaConfiguration)

responseModifyAuthenticationProfile :: ModifyAuthenticationProfileResponse -> TestTree
responseModifyAuthenticationProfile =
  res
    "ModifyAuthenticationProfileResponse"
    "fixture/ModifyAuthenticationProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyAuthenticationProfile)

responseModifyCluster :: ModifyClusterResponse -> TestTree
responseModifyCluster =
  res
    "ModifyClusterResponse"
    "fixture/ModifyClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyCluster)

responseModifyClusterDbRevision :: ModifyClusterDbRevisionResponse -> TestTree
responseModifyClusterDbRevision =
  res
    "ModifyClusterDbRevisionResponse"
    "fixture/ModifyClusterDbRevisionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyClusterDbRevision)

responseModifyClusterIamRoles :: ModifyClusterIamRolesResponse -> TestTree
responseModifyClusterIamRoles =
  res
    "ModifyClusterIamRolesResponse"
    "fixture/ModifyClusterIamRolesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyClusterIamRoles)

responseModifyClusterMaintenance :: ModifyClusterMaintenanceResponse -> TestTree
responseModifyClusterMaintenance =
  res
    "ModifyClusterMaintenanceResponse"
    "fixture/ModifyClusterMaintenanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyClusterMaintenance)

responseModifyClusterParameterGroup :: ClusterParameterGroupNameMessage -> TestTree
responseModifyClusterParameterGroup =
  res
    "ModifyClusterParameterGroupResponse"
    "fixture/ModifyClusterParameterGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyClusterParameterGroup)

responseModifyClusterSnapshot :: ModifyClusterSnapshotResponse -> TestTree
responseModifyClusterSnapshot =
  res
    "ModifyClusterSnapshotResponse"
    "fixture/ModifyClusterSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyClusterSnapshot)

responseModifyClusterSnapshotSchedule :: ModifyClusterSnapshotScheduleResponse -> TestTree
responseModifyClusterSnapshotSchedule =
  res
    "ModifyClusterSnapshotScheduleResponse"
    "fixture/ModifyClusterSnapshotScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyClusterSnapshotSchedule)

responseModifyClusterSubnetGroup :: ModifyClusterSubnetGroupResponse -> TestTree
responseModifyClusterSubnetGroup =
  res
    "ModifyClusterSubnetGroupResponse"
    "fixture/ModifyClusterSubnetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyClusterSubnetGroup)

responseModifyEndpointAccess :: EndpointAccess -> TestTree
responseModifyEndpointAccess =
  res
    "ModifyEndpointAccessResponse"
    "fixture/ModifyEndpointAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyEndpointAccess)

responseModifyEventSubscription :: ModifyEventSubscriptionResponse -> TestTree
responseModifyEventSubscription =
  res
    "ModifyEventSubscriptionResponse"
    "fixture/ModifyEventSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyEventSubscription)

responseModifyScheduledAction :: ScheduledAction -> TestTree
responseModifyScheduledAction =
  res
    "ModifyScheduledActionResponse"
    "fixture/ModifyScheduledActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyScheduledAction)

responseModifySnapshotCopyRetentionPeriod :: ModifySnapshotCopyRetentionPeriodResponse -> TestTree
responseModifySnapshotCopyRetentionPeriod =
  res
    "ModifySnapshotCopyRetentionPeriodResponse"
    "fixture/ModifySnapshotCopyRetentionPeriodResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifySnapshotCopyRetentionPeriod)

responseModifySnapshotSchedule :: SnapshotSchedule -> TestTree
responseModifySnapshotSchedule =
  res
    "ModifySnapshotScheduleResponse"
    "fixture/ModifySnapshotScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifySnapshotSchedule)

responseModifyUsageLimit :: UsageLimit -> TestTree
responseModifyUsageLimit =
  res
    "ModifyUsageLimitResponse"
    "fixture/ModifyUsageLimitResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyUsageLimit)

responsePauseCluster :: PauseClusterResponse -> TestTree
responsePauseCluster =
  res
    "PauseClusterResponse"
    "fixture/PauseClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PauseCluster)

responsePurchaseReservedNodeOffering :: PurchaseReservedNodeOfferingResponse -> TestTree
responsePurchaseReservedNodeOffering =
  res
    "PurchaseReservedNodeOfferingResponse"
    "fixture/PurchaseReservedNodeOfferingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PurchaseReservedNodeOffering)

responseRebootCluster :: RebootClusterResponse -> TestTree
responseRebootCluster =
  res
    "RebootClusterResponse"
    "fixture/RebootClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RebootCluster)

responseRejectDataShare :: DataShare -> TestTree
responseRejectDataShare =
  res
    "RejectDataShareResponse"
    "fixture/RejectDataShareResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RejectDataShare)

responseResetClusterParameterGroup :: ClusterParameterGroupNameMessage -> TestTree
responseResetClusterParameterGroup =
  res
    "ResetClusterParameterGroupResponse"
    "fixture/ResetClusterParameterGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResetClusterParameterGroup)

responseResizeCluster :: ResizeClusterResponse -> TestTree
responseResizeCluster =
  res
    "ResizeClusterResponse"
    "fixture/ResizeClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResizeCluster)

responseRestoreFromClusterSnapshot :: RestoreFromClusterSnapshotResponse -> TestTree
responseRestoreFromClusterSnapshot =
  res
    "RestoreFromClusterSnapshotResponse"
    "fixture/RestoreFromClusterSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestoreFromClusterSnapshot)

responseRestoreTableFromClusterSnapshot :: RestoreTableFromClusterSnapshotResponse -> TestTree
responseRestoreTableFromClusterSnapshot =
  res
    "RestoreTableFromClusterSnapshotResponse"
    "fixture/RestoreTableFromClusterSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestoreTableFromClusterSnapshot)

responseResumeCluster :: ResumeClusterResponse -> TestTree
responseResumeCluster =
  res
    "ResumeClusterResponse"
    "fixture/ResumeClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResumeCluster)

responseRevokeClusterSecurityGroupIngress :: RevokeClusterSecurityGroupIngressResponse -> TestTree
responseRevokeClusterSecurityGroupIngress =
  res
    "RevokeClusterSecurityGroupIngressResponse"
    "fixture/RevokeClusterSecurityGroupIngressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RevokeClusterSecurityGroupIngress)

responseRevokeEndpointAccess :: EndpointAuthorization -> TestTree
responseRevokeEndpointAccess =
  res
    "RevokeEndpointAccessResponse"
    "fixture/RevokeEndpointAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RevokeEndpointAccess)

responseRevokeSnapshotAccess :: RevokeSnapshotAccessResponse -> TestTree
responseRevokeSnapshotAccess =
  res
    "RevokeSnapshotAccessResponse"
    "fixture/RevokeSnapshotAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RevokeSnapshotAccess)

responseRotateEncryptionKey :: RotateEncryptionKeyResponse -> TestTree
responseRotateEncryptionKey =
  res
    "RotateEncryptionKeyResponse"
    "fixture/RotateEncryptionKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RotateEncryptionKey)

responseUpdatePartnerStatus :: PartnerIntegrationOutputMessage -> TestTree
responseUpdatePartnerStatus =
  res
    "UpdatePartnerStatusResponse"
    "fixture/UpdatePartnerStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePartnerStatus)
