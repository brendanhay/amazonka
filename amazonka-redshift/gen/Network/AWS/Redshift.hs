{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.Redshift
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2012-12-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Redshift
--
-- __Overview__
--
-- This is an interface reference for Amazon Redshift. It contains
-- documentation for one of the programming or command line interfaces you
-- can use to manage Amazon Redshift clusters. Note that Amazon Redshift is
-- asynchronous, which means that some interfaces may require techniques,
-- such as polling or asynchronous callback handlers, to determine when a
-- command has been applied. In this reference, the parameter descriptions
-- indicate whether a change is applied immediately, on the next instance
-- reboot, or during the next maintenance window. For a summary of the
-- Amazon Redshift cluster management interfaces, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/using-aws-sdk.html Using the Amazon Redshift Management Interfaces>.
--
-- Amazon Redshift manages all the work of setting up, operating, and
-- scaling a data warehouse: provisioning capacity, monitoring and backing
-- up the cluster, and applying patches and upgrades to the Amazon Redshift
-- engine. You can focus on using your data to acquire new insights for
-- your business and customers.
--
-- If you are a first-time user of Amazon Redshift, we recommend that you
-- begin by reading the
-- <https://docs.aws.amazon.com/redshift/latest/gsg/getting-started.html Amazon Redshift Getting Started Guide>.
--
-- If you are a database developer, the
-- <https://docs.aws.amazon.com/redshift/latest/dg/welcome.html Amazon Redshift Database Developer Guide>
-- explains how to design, build, query, and maintain the databases that
-- make up your data warehouse.
module Network.AWS.Redshift
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** BucketNotFoundFault
    _BucketNotFoundFault,

    -- ** InvalidSubscriptionStateFault
    _InvalidSubscriptionStateFault,

    -- ** InvalidVPCNetworkStateFault
    _InvalidVPCNetworkStateFault,

    -- ** ScheduleDefinitionTypeUnsupportedFault
    _ScheduleDefinitionTypeUnsupportedFault,

    -- ** ClusterSecurityGroupAlreadyExistsFault
    _ClusterSecurityGroupAlreadyExistsFault,

    -- ** ClusterSnapshotNotFoundFault
    _ClusterSnapshotNotFoundFault,

    -- ** InvalidEndpointStateFault
    _InvalidEndpointStateFault,

    -- ** ClusterSubnetGroupNotFoundFault
    _ClusterSubnetGroupNotFoundFault,

    -- ** InvalidScheduledActionFault
    _InvalidScheduledActionFault,

    -- ** ClusterSecurityGroupQuotaExceededFault
    _ClusterSecurityGroupQuotaExceededFault,

    -- ** LimitExceededFault
    _LimitExceededFault,

    -- ** SubscriptionAlreadyExistFault
    _SubscriptionAlreadyExistFault,

    -- ** HsmConfigurationQuotaExceededFault
    _HsmConfigurationQuotaExceededFault,

    -- ** EndpointAuthorizationNotFoundFault
    _EndpointAuthorizationNotFoundFault,

    -- ** EndpointsPerClusterLimitExceededFault
    _EndpointsPerClusterLimitExceededFault,

    -- ** InvalidAuthenticationProfileRequestFault
    _InvalidAuthenticationProfileRequestFault,

    -- ** InsufficientS3BucketPolicyFault
    _InsufficientS3BucketPolicyFault,

    -- ** SubscriptionSeverityNotFoundFault
    _SubscriptionSeverityNotFoundFault,

    -- ** InsufficientClusterCapacityFault
    _InsufficientClusterCapacityFault,

    -- ** UnauthorizedOperation
    _UnauthorizedOperation,

    -- ** UsageLimitNotFoundFault
    _UsageLimitNotFoundFault,

    -- ** InvalidTagFault
    _InvalidTagFault,

    -- ** SnapshotScheduleQuotaExceededFault
    _SnapshotScheduleQuotaExceededFault,

    -- ** ScheduledActionNotFoundFault
    _ScheduledActionNotFoundFault,

    -- ** HsmClientCertificateAlreadyExistsFault
    _HsmClientCertificateAlreadyExistsFault,

    -- ** SNSNoAuthorizationFault
    _SNSNoAuthorizationFault,

    -- ** InvalidRestoreFault
    _InvalidRestoreFault,

    -- ** ClusterNotFoundFault
    _ClusterNotFoundFault,

    -- ** ResourceNotFoundFault
    _ResourceNotFoundFault,

    -- ** ScheduledActionAlreadyExistsFault
    _ScheduledActionAlreadyExistsFault,

    -- ** SNSTopicArnNotFoundFault
    _SNSTopicArnNotFoundFault,

    -- ** SnapshotScheduleUpdateInProgressFault
    _SnapshotScheduleUpdateInProgressFault,

    -- ** InvalidReservedNodeStateFault
    _InvalidReservedNodeStateFault,

    -- ** SnapshotScheduleNotFoundFault
    _SnapshotScheduleNotFoundFault,

    -- ** ScheduledActionQuotaExceededFault
    _ScheduledActionQuotaExceededFault,

    -- ** EventSubscriptionQuotaExceededFault
    _EventSubscriptionQuotaExceededFault,

    -- ** ScheduledActionTypeUnsupportedFault
    _ScheduledActionTypeUnsupportedFault,

    -- ** EndpointsPerAuthorizationLimitExceededFault
    _EndpointsPerAuthorizationLimitExceededFault,

    -- ** IncompatibleOrderableOptions
    _IncompatibleOrderableOptions,

    -- ** ClusterQuotaExceededFault
    _ClusterQuotaExceededFault,

    -- ** InvalidClusterSubnetStateFault
    _InvalidClusterSubnetStateFault,

    -- ** ReservedNodeAlreadyMigratedFault
    _ReservedNodeAlreadyMigratedFault,

    -- ** BatchModifyClusterSnapshotsLimitExceededFault
    _BatchModifyClusterSnapshotsLimitExceededFault,

    -- ** UnsupportedOperationFault
    _UnsupportedOperationFault,

    -- ** HsmConfigurationAlreadyExistsFault
    _HsmConfigurationAlreadyExistsFault,

    -- ** InvalidS3BucketNameFault
    _InvalidS3BucketNameFault,

    -- ** ClusterSecurityGroupNotFoundFault
    _ClusterSecurityGroupNotFoundFault,

    -- ** InvalidClusterSubnetGroupStateFault
    _InvalidClusterSubnetGroupStateFault,

    -- ** InvalidElasticIpFault
    _InvalidElasticIpFault,

    -- ** EndpointAlreadyExistsFault
    _EndpointAlreadyExistsFault,

    -- ** InvalidAuthorizationStateFault
    _InvalidAuthorizationStateFault,

    -- ** TableRestoreNotFoundFault
    _TableRestoreNotFoundFault,

    -- ** AuthenticationProfileAlreadyExistsFault
    _AuthenticationProfileAlreadyExistsFault,

    -- ** HsmConfigurationNotFoundFault
    _HsmConfigurationNotFoundFault,

    -- ** PartnerNotFoundFault
    _PartnerNotFoundFault,

    -- ** AuthorizationAlreadyExistsFault
    _AuthorizationAlreadyExistsFault,

    -- ** InvalidClusterSecurityGroupStateFault
    _InvalidClusterSecurityGroupStateFault,

    -- ** InvalidClusterSnapshotScheduleStateFault
    _InvalidClusterSnapshotScheduleStateFault,

    -- ** EndpointAuthorizationAlreadyExistsFault
    _EndpointAuthorizationAlreadyExistsFault,

    -- ** InvalidS3KeyPrefixFault
    _InvalidS3KeyPrefixFault,

    -- ** SourceNotFoundFault
    _SourceNotFoundFault,

    -- ** UsageLimitAlreadyExistsFault
    _UsageLimitAlreadyExistsFault,

    -- ** CopyToRegionDisabledFault
    _CopyToRegionDisabledFault,

    -- ** SNSInvalidTopicFault
    _SNSInvalidTopicFault,

    -- ** DependentServiceUnavailableFault
    _DependentServiceUnavailableFault,

    -- ** AuthenticationProfileQuotaExceededFault
    _AuthenticationProfileQuotaExceededFault,

    -- ** AuthorizationQuotaExceededFault
    _AuthorizationQuotaExceededFault,

    -- ** UnsupportedOptionFault
    _UnsupportedOptionFault,

    -- ** ClusterSnapshotQuotaExceededFault
    _ClusterSnapshotQuotaExceededFault,

    -- ** SubnetAlreadyInUse
    _SubnetAlreadyInUse,

    -- ** ClusterOnLatestRevisionFault
    _ClusterOnLatestRevisionFault,

    -- ** ClusterSubnetGroupQuotaExceededFault
    _ClusterSubnetGroupQuotaExceededFault,

    -- ** InvalidClusterTrackFault
    _InvalidClusterTrackFault,

    -- ** TagLimitExceededFault
    _TagLimitExceededFault,

    -- ** InvalidScheduleFault
    _InvalidScheduleFault,

    -- ** InvalidHsmClientCertificateStateFault
    _InvalidHsmClientCertificateStateFault,

    -- ** ClusterAlreadyExistsFault
    _ClusterAlreadyExistsFault,

    -- ** InvalidTableRestoreArgumentFault
    _InvalidTableRestoreArgumentFault,

    -- ** SnapshotCopyGrantNotFoundFault
    _SnapshotCopyGrantNotFoundFault,

    -- ** UnauthorizedPartnerIntegrationFault
    _UnauthorizedPartnerIntegrationFault,

    -- ** InvalidClusterStateFault
    _InvalidClusterStateFault,

    -- ** InvalidDataShareFault
    _InvalidDataShareFault,

    -- ** ClusterSubnetQuotaExceededFault
    _ClusterSubnetQuotaExceededFault,

    -- ** AccessToSnapshotDeniedFault
    _AccessToSnapshotDeniedFault,

    -- ** NumberOfNodesQuotaExceededFault
    _NumberOfNodesQuotaExceededFault,

    -- ** SubscriptionEventIdNotFoundFault
    _SubscriptionEventIdNotFoundFault,

    -- ** InvalidSnapshotCopyGrantStateFault
    _InvalidSnapshotCopyGrantStateFault,

    -- ** InProgressTableRestoreQuotaExceededFault
    _InProgressTableRestoreQuotaExceededFault,

    -- ** ClusterParameterGroupQuotaExceededFault
    _ClusterParameterGroupQuotaExceededFault,

    -- ** SnapshotCopyGrantAlreadyExistsFault
    _SnapshotCopyGrantAlreadyExistsFault,

    -- ** SnapshotCopyDisabledFault
    _SnapshotCopyDisabledFault,

    -- ** HsmClientCertificateNotFoundFault
    _HsmClientCertificateNotFoundFault,

    -- ** InvalidUsageLimitFault
    _InvalidUsageLimitFault,

    -- ** ResizeNotFoundFault
    _ResizeNotFoundFault,

    -- ** ReservedNodeQuotaExceededFault
    _ReservedNodeQuotaExceededFault,

    -- ** UnknownSnapshotCopyRegionFault
    _UnknownSnapshotCopyRegionFault,

    -- ** BatchDeleteRequestSizeExceededFault
    _BatchDeleteRequestSizeExceededFault,

    -- ** InvalidNamespaceFault
    _InvalidNamespaceFault,

    -- ** NumberOfNodesPerClusterLimitExceededFault
    _NumberOfNodesPerClusterLimitExceededFault,

    -- ** SnapshotCopyGrantQuotaExceededFault
    _SnapshotCopyGrantQuotaExceededFault,

    -- ** InvalidClusterParameterGroupStateFault
    _InvalidClusterParameterGroupStateFault,

    -- ** SnapshotCopyAlreadyEnabledFault
    _SnapshotCopyAlreadyEnabledFault,

    -- ** ClusterParameterGroupAlreadyExistsFault
    _ClusterParameterGroupAlreadyExistsFault,

    -- ** AccessToClusterDeniedFault
    _AccessToClusterDeniedFault,

    -- ** ReservedNodeAlreadyExistsFault
    _ReservedNodeAlreadyExistsFault,

    -- ** SnapshotScheduleAlreadyExistsFault
    _SnapshotScheduleAlreadyExistsFault,

    -- ** ClusterParameterGroupNotFoundFault
    _ClusterParameterGroupNotFoundFault,

    -- ** ReservedNodeOfferingNotFoundFault
    _ReservedNodeOfferingNotFoundFault,

    -- ** ReservedNodeNotFoundFault
    _ReservedNodeNotFoundFault,

    -- ** SnapshotCopyAlreadyDisabledFault
    _SnapshotCopyAlreadyDisabledFault,

    -- ** HsmClientCertificateQuotaExceededFault
    _HsmClientCertificateQuotaExceededFault,

    -- ** EndpointNotFoundFault
    _EndpointNotFoundFault,

    -- ** SubscriptionCategoryNotFoundFault
    _SubscriptionCategoryNotFoundFault,

    -- ** EndpointAuthorizationsPerClusterLimitExceededFault
    _EndpointAuthorizationsPerClusterLimitExceededFault,

    -- ** InvalidClusterSnapshotStateFault
    _InvalidClusterSnapshotStateFault,

    -- ** DependentServiceRequestThrottlingFault
    _DependentServiceRequestThrottlingFault,

    -- ** InvalidRetentionPeriodFault
    _InvalidRetentionPeriodFault,

    -- ** ClusterSnapshotAlreadyExistsFault
    _ClusterSnapshotAlreadyExistsFault,

    -- ** InvalidHsmConfigurationStateFault
    _InvalidHsmConfigurationStateFault,

    -- ** SubscriptionNotFoundFault
    _SubscriptionNotFoundFault,

    -- ** AuthorizationNotFoundFault
    _AuthorizationNotFoundFault,

    -- ** TableLimitExceededFault
    _TableLimitExceededFault,

    -- ** AuthenticationProfileNotFoundFault
    _AuthenticationProfileNotFoundFault,

    -- ** InvalidSubnet
    _InvalidSubnet,

    -- ** ClusterSubnetGroupAlreadyExistsFault
    _ClusterSubnetGroupAlreadyExistsFault,

    -- * Waiters
    -- $waiters

    -- ** ClusterRestored
    newClusterRestored,

    -- ** ClusterDeleted
    newClusterDeleted,

    -- ** ClusterAvailable
    newClusterAvailable,

    -- ** SnapshotAvailable
    newSnapshotAvailable,

    -- * Operations
    -- $operations

    -- ** DisableLogging
    DisableLogging (DisableLogging'),
    newDisableLogging,
    LoggingStatus (LoggingStatus'),
    newLoggingStatus,

    -- ** AddPartner
    AddPartner (AddPartner'),
    newAddPartner,
    PartnerIntegrationOutputMessage (PartnerIntegrationOutputMessage'),
    newPartnerIntegrationOutputMessage,

    -- ** PurchaseReservedNodeOffering
    PurchaseReservedNodeOffering (PurchaseReservedNodeOffering'),
    newPurchaseReservedNodeOffering,
    PurchaseReservedNodeOfferingResponse (PurchaseReservedNodeOfferingResponse'),
    newPurchaseReservedNodeOfferingResponse,

    -- ** DescribeEventCategories
    DescribeEventCategories (DescribeEventCategories'),
    newDescribeEventCategories,
    DescribeEventCategoriesResponse (DescribeEventCategoriesResponse'),
    newDescribeEventCategoriesResponse,

    -- ** ModifyEndpointAccess
    ModifyEndpointAccess (ModifyEndpointAccess'),
    newModifyEndpointAccess,
    EndpointAccess (EndpointAccess'),
    newEndpointAccess,

    -- ** DeleteClusterSubnetGroup
    DeleteClusterSubnetGroup (DeleteClusterSubnetGroup'),
    newDeleteClusterSubnetGroup,
    DeleteClusterSubnetGroupResponse (DeleteClusterSubnetGroupResponse'),
    newDeleteClusterSubnetGroupResponse,

    -- ** AssociateDataShareConsumer
    AssociateDataShareConsumer (AssociateDataShareConsumer'),
    newAssociateDataShareConsumer,
    DataShare (DataShare'),
    newDataShare,

    -- ** RevokeEndpointAccess
    RevokeEndpointAccess (RevokeEndpointAccess'),
    newRevokeEndpointAccess,
    EndpointAuthorization (EndpointAuthorization'),
    newEndpointAuthorization,

    -- ** DeletePartner
    DeletePartner (DeletePartner'),
    newDeletePartner,
    PartnerIntegrationOutputMessage (PartnerIntegrationOutputMessage'),
    newPartnerIntegrationOutputMessage,

    -- ** CreateAuthenticationProfile
    CreateAuthenticationProfile (CreateAuthenticationProfile'),
    newCreateAuthenticationProfile,
    CreateAuthenticationProfileResponse (CreateAuthenticationProfileResponse'),
    newCreateAuthenticationProfileResponse,

    -- ** ModifyAquaConfiguration
    ModifyAquaConfiguration (ModifyAquaConfiguration'),
    newModifyAquaConfiguration,
    ModifyAquaConfigurationResponse (ModifyAquaConfigurationResponse'),
    newModifyAquaConfigurationResponse,

    -- ** CreateUsageLimit
    CreateUsageLimit (CreateUsageLimit'),
    newCreateUsageLimit,
    UsageLimit (UsageLimit'),
    newUsageLimit,

    -- ** AuthorizeEndpointAccess
    AuthorizeEndpointAccess (AuthorizeEndpointAccess'),
    newAuthorizeEndpointAccess,
    EndpointAuthorization (EndpointAuthorization'),
    newEndpointAuthorization,

    -- ** DescribeHsmClientCertificates (Paginated)
    DescribeHsmClientCertificates (DescribeHsmClientCertificates'),
    newDescribeHsmClientCertificates,
    DescribeHsmClientCertificatesResponse (DescribeHsmClientCertificatesResponse'),
    newDescribeHsmClientCertificatesResponse,

    -- ** CancelResize
    CancelResize (CancelResize'),
    newCancelResize,
    ResizeProgressMessage (ResizeProgressMessage'),
    newResizeProgressMessage,

    -- ** DescribeClusters (Paginated)
    DescribeClusters (DescribeClusters'),
    newDescribeClusters,
    DescribeClustersResponse (DescribeClustersResponse'),
    newDescribeClustersResponse,

    -- ** GetClusterCredentials
    GetClusterCredentials (GetClusterCredentials'),
    newGetClusterCredentials,
    GetClusterCredentialsResponse (GetClusterCredentialsResponse'),
    newGetClusterCredentialsResponse,

    -- ** DescribeTags (Paginated)
    DescribeTags (DescribeTags'),
    newDescribeTags,
    DescribeTagsResponse (DescribeTagsResponse'),
    newDescribeTagsResponse,

    -- ** ModifyClusterParameterGroup
    ModifyClusterParameterGroup (ModifyClusterParameterGroup'),
    newModifyClusterParameterGroup,
    ClusterParameterGroupNameMessage (ClusterParameterGroupNameMessage'),
    newClusterParameterGroupNameMessage,

    -- ** RevokeClusterSecurityGroupIngress
    RevokeClusterSecurityGroupIngress (RevokeClusterSecurityGroupIngress'),
    newRevokeClusterSecurityGroupIngress,
    RevokeClusterSecurityGroupIngressResponse (RevokeClusterSecurityGroupIngressResponse'),
    newRevokeClusterSecurityGroupIngressResponse,

    -- ** ResetClusterParameterGroup
    ResetClusterParameterGroup (ResetClusterParameterGroup'),
    newResetClusterParameterGroup,
    ClusterParameterGroupNameMessage (ClusterParameterGroupNameMessage'),
    newClusterParameterGroupNameMessage,

    -- ** DeleteUsageLimit
    DeleteUsageLimit (DeleteUsageLimit'),
    newDeleteUsageLimit,
    DeleteUsageLimitResponse (DeleteUsageLimitResponse'),
    newDeleteUsageLimitResponse,

    -- ** DescribeClusterDbRevisions (Paginated)
    DescribeClusterDbRevisions (DescribeClusterDbRevisions'),
    newDescribeClusterDbRevisions,
    DescribeClusterDbRevisionsResponse (DescribeClusterDbRevisionsResponse'),
    newDescribeClusterDbRevisionsResponse,

    -- ** RotateEncryptionKey
    RotateEncryptionKey (RotateEncryptionKey'),
    newRotateEncryptionKey,
    RotateEncryptionKeyResponse (RotateEncryptionKeyResponse'),
    newRotateEncryptionKeyResponse,

    -- ** DescribeScheduledActions (Paginated)
    DescribeScheduledActions (DescribeScheduledActions'),
    newDescribeScheduledActions,
    DescribeScheduledActionsResponse (DescribeScheduledActionsResponse'),
    newDescribeScheduledActionsResponse,

    -- ** DescribeEventSubscriptions (Paginated)
    DescribeEventSubscriptions (DescribeEventSubscriptions'),
    newDescribeEventSubscriptions,
    DescribeEventSubscriptionsResponse (DescribeEventSubscriptionsResponse'),
    newDescribeEventSubscriptionsResponse,

    -- ** DeleteEventSubscription
    DeleteEventSubscription (DeleteEventSubscription'),
    newDeleteEventSubscription,
    DeleteEventSubscriptionResponse (DeleteEventSubscriptionResponse'),
    newDeleteEventSubscriptionResponse,

    -- ** DescribeDataSharesForConsumer
    DescribeDataSharesForConsumer (DescribeDataSharesForConsumer'),
    newDescribeDataSharesForConsumer,
    DescribeDataSharesForConsumerResponse (DescribeDataSharesForConsumerResponse'),
    newDescribeDataSharesForConsumerResponse,

    -- ** ModifyClusterSnapshot
    ModifyClusterSnapshot (ModifyClusterSnapshot'),
    newModifyClusterSnapshot,
    ModifyClusterSnapshotResponse (ModifyClusterSnapshotResponse'),
    newModifyClusterSnapshotResponse,

    -- ** ModifyClusterSubnetGroup
    ModifyClusterSubnetGroup (ModifyClusterSubnetGroup'),
    newModifyClusterSubnetGroup,
    ModifyClusterSubnetGroupResponse (ModifyClusterSubnetGroupResponse'),
    newModifyClusterSubnetGroupResponse,

    -- ** DeleteScheduledAction
    DeleteScheduledAction (DeleteScheduledAction'),
    newDeleteScheduledAction,
    DeleteScheduledActionResponse (DeleteScheduledActionResponse'),
    newDeleteScheduledActionResponse,

    -- ** DeleteEndpointAccess
    DeleteEndpointAccess (DeleteEndpointAccess'),
    newDeleteEndpointAccess,
    EndpointAccess (EndpointAccess'),
    newEndpointAccess,

    -- ** RestoreTableFromClusterSnapshot
    RestoreTableFromClusterSnapshot (RestoreTableFromClusterSnapshot'),
    newRestoreTableFromClusterSnapshot,
    RestoreTableFromClusterSnapshotResponse (RestoreTableFromClusterSnapshotResponse'),
    newRestoreTableFromClusterSnapshotResponse,

    -- ** CreateCluster
    CreateCluster (CreateCluster'),
    newCreateCluster,
    CreateClusterResponse (CreateClusterResponse'),
    newCreateClusterResponse,

    -- ** CreateEndpointAccess
    CreateEndpointAccess (CreateEndpointAccess'),
    newCreateEndpointAccess,
    EndpointAccess (EndpointAccess'),
    newEndpointAccess,

    -- ** ModifyClusterSnapshotSchedule
    ModifyClusterSnapshotSchedule (ModifyClusterSnapshotSchedule'),
    newModifyClusterSnapshotSchedule,
    ModifyClusterSnapshotScheduleResponse (ModifyClusterSnapshotScheduleResponse'),
    newModifyClusterSnapshotScheduleResponse,

    -- ** DescribeAccountAttributes
    DescribeAccountAttributes (DescribeAccountAttributes'),
    newDescribeAccountAttributes,
    DescribeAccountAttributesResponse (DescribeAccountAttributesResponse'),
    newDescribeAccountAttributesResponse,

    -- ** ModifyAuthenticationProfile
    ModifyAuthenticationProfile (ModifyAuthenticationProfile'),
    newModifyAuthenticationProfile,
    ModifyAuthenticationProfileResponse (ModifyAuthenticationProfileResponse'),
    newModifyAuthenticationProfileResponse,

    -- ** CopyClusterSnapshot
    CopyClusterSnapshot (CopyClusterSnapshot'),
    newCopyClusterSnapshot,
    CopyClusterSnapshotResponse (CopyClusterSnapshotResponse'),
    newCopyClusterSnapshotResponse,

    -- ** CreateSnapshotCopyGrant
    CreateSnapshotCopyGrant (CreateSnapshotCopyGrant'),
    newCreateSnapshotCopyGrant,
    CreateSnapshotCopyGrantResponse (CreateSnapshotCopyGrantResponse'),
    newCreateSnapshotCopyGrantResponse,

    -- ** UpdatePartnerStatus
    UpdatePartnerStatus (UpdatePartnerStatus'),
    newUpdatePartnerStatus,
    PartnerIntegrationOutputMessage (PartnerIntegrationOutputMessage'),
    newPartnerIntegrationOutputMessage,

    -- ** DescribeDataSharesForProducer
    DescribeDataSharesForProducer (DescribeDataSharesForProducer'),
    newDescribeDataSharesForProducer,
    DescribeDataSharesForProducerResponse (DescribeDataSharesForProducerResponse'),
    newDescribeDataSharesForProducerResponse,

    -- ** DescribeHsmConfigurations (Paginated)
    DescribeHsmConfigurations (DescribeHsmConfigurations'),
    newDescribeHsmConfigurations,
    DescribeHsmConfigurationsResponse (DescribeHsmConfigurationsResponse'),
    newDescribeHsmConfigurationsResponse,

    -- ** DescribeClusterSnapshots (Paginated)
    DescribeClusterSnapshots (DescribeClusterSnapshots'),
    newDescribeClusterSnapshots,
    DescribeClusterSnapshotsResponse (DescribeClusterSnapshotsResponse'),
    newDescribeClusterSnapshotsResponse,

    -- ** DeleteTags
    DeleteTags (DeleteTags'),
    newDeleteTags,
    DeleteTagsResponse (DeleteTagsResponse'),
    newDeleteTagsResponse,

    -- ** EnableSnapshotCopy
    EnableSnapshotCopy (EnableSnapshotCopy'),
    newEnableSnapshotCopy,
    EnableSnapshotCopyResponse (EnableSnapshotCopyResponse'),
    newEnableSnapshotCopyResponse,

    -- ** ModifyUsageLimit
    ModifyUsageLimit (ModifyUsageLimit'),
    newModifyUsageLimit,
    UsageLimit (UsageLimit'),
    newUsageLimit,

    -- ** CreateClusterParameterGroup
    CreateClusterParameterGroup (CreateClusterParameterGroup'),
    newCreateClusterParameterGroup,
    CreateClusterParameterGroupResponse (CreateClusterParameterGroupResponse'),
    newCreateClusterParameterGroupResponse,

    -- ** CreateSnapshotSchedule
    CreateSnapshotSchedule (CreateSnapshotSchedule'),
    newCreateSnapshotSchedule,
    SnapshotSchedule (SnapshotSchedule'),
    newSnapshotSchedule,

    -- ** DescribeEndpointAuthorization (Paginated)
    DescribeEndpointAuthorization (DescribeEndpointAuthorization'),
    newDescribeEndpointAuthorization,
    DescribeEndpointAuthorizationResponse (DescribeEndpointAuthorizationResponse'),
    newDescribeEndpointAuthorizationResponse,

    -- ** DeleteClusterParameterGroup
    DeleteClusterParameterGroup (DeleteClusterParameterGroup'),
    newDeleteClusterParameterGroup,
    DeleteClusterParameterGroupResponse (DeleteClusterParameterGroupResponse'),
    newDeleteClusterParameterGroupResponse,

    -- ** DescribeClusterSecurityGroups (Paginated)
    DescribeClusterSecurityGroups (DescribeClusterSecurityGroups'),
    newDescribeClusterSecurityGroups,
    DescribeClusterSecurityGroupsResponse (DescribeClusterSecurityGroupsResponse'),
    newDescribeClusterSecurityGroupsResponse,

    -- ** DescribeNodeConfigurationOptions (Paginated)
    DescribeNodeConfigurationOptions (DescribeNodeConfigurationOptions'),
    newDescribeNodeConfigurationOptions,
    DescribeNodeConfigurationOptionsResponse (DescribeNodeConfigurationOptionsResponse'),
    newDescribeNodeConfigurationOptionsResponse,

    -- ** DescribeAuthenticationProfiles
    DescribeAuthenticationProfiles (DescribeAuthenticationProfiles'),
    newDescribeAuthenticationProfiles,
    DescribeAuthenticationProfilesResponse (DescribeAuthenticationProfilesResponse'),
    newDescribeAuthenticationProfilesResponse,

    -- ** DescribeEndpointAccess (Paginated)
    DescribeEndpointAccess (DescribeEndpointAccess'),
    newDescribeEndpointAccess,
    DescribeEndpointAccessResponse (DescribeEndpointAccessResponse'),
    newDescribeEndpointAccessResponse,

    -- ** DescribeEvents (Paginated)
    DescribeEvents (DescribeEvents'),
    newDescribeEvents,
    DescribeEventsResponse (DescribeEventsResponse'),
    newDescribeEventsResponse,

    -- ** CreateClusterSnapshot
    CreateClusterSnapshot (CreateClusterSnapshot'),
    newCreateClusterSnapshot,
    CreateClusterSnapshotResponse (CreateClusterSnapshotResponse'),
    newCreateClusterSnapshotResponse,

    -- ** DescribeLoggingStatus
    DescribeLoggingStatus (DescribeLoggingStatus'),
    newDescribeLoggingStatus,
    LoggingStatus (LoggingStatus'),
    newLoggingStatus,

    -- ** DescribeClusterParameterGroups (Paginated)
    DescribeClusterParameterGroups (DescribeClusterParameterGroups'),
    newDescribeClusterParameterGroups,
    DescribeClusterParameterGroupsResponse (DescribeClusterParameterGroupsResponse'),
    newDescribeClusterParameterGroupsResponse,

    -- ** ModifyCluster
    ModifyCluster (ModifyCluster'),
    newModifyCluster,
    ModifyClusterResponse (ModifyClusterResponse'),
    newModifyClusterResponse,

    -- ** GetReservedNodeExchangeOfferings (Paginated)
    GetReservedNodeExchangeOfferings (GetReservedNodeExchangeOfferings'),
    newGetReservedNodeExchangeOfferings,
    GetReservedNodeExchangeOfferingsResponse (GetReservedNodeExchangeOfferingsResponse'),
    newGetReservedNodeExchangeOfferingsResponse,

    -- ** RejectDataShare
    RejectDataShare (RejectDataShare'),
    newRejectDataShare,
    DataShare (DataShare'),
    newDataShare,

    -- ** CreateClusterSubnetGroup
    CreateClusterSubnetGroup (CreateClusterSubnetGroup'),
    newCreateClusterSubnetGroup,
    CreateClusterSubnetGroupResponse (CreateClusterSubnetGroupResponse'),
    newCreateClusterSubnetGroupResponse,

    -- ** DeleteHsmConfiguration
    DeleteHsmConfiguration (DeleteHsmConfiguration'),
    newDeleteHsmConfiguration,
    DeleteHsmConfigurationResponse (DeleteHsmConfigurationResponse'),
    newDeleteHsmConfigurationResponse,

    -- ** DescribeTableRestoreStatus (Paginated)
    DescribeTableRestoreStatus (DescribeTableRestoreStatus'),
    newDescribeTableRestoreStatus,
    DescribeTableRestoreStatusResponse (DescribeTableRestoreStatusResponse'),
    newDescribeTableRestoreStatusResponse,

    -- ** DeleteClusterSnapshot
    DeleteClusterSnapshot (DeleteClusterSnapshot'),
    newDeleteClusterSnapshot,
    DeleteClusterSnapshotResponse (DeleteClusterSnapshotResponse'),
    newDeleteClusterSnapshotResponse,

    -- ** ModifyClusterDbRevision
    ModifyClusterDbRevision (ModifyClusterDbRevision'),
    newModifyClusterDbRevision,
    ModifyClusterDbRevisionResponse (ModifyClusterDbRevisionResponse'),
    newModifyClusterDbRevisionResponse,

    -- ** AuthorizeClusterSecurityGroupIngress
    AuthorizeClusterSecurityGroupIngress (AuthorizeClusterSecurityGroupIngress'),
    newAuthorizeClusterSecurityGroupIngress,
    AuthorizeClusterSecurityGroupIngressResponse (AuthorizeClusterSecurityGroupIngressResponse'),
    newAuthorizeClusterSecurityGroupIngressResponse,

    -- ** DeauthorizeDataShare
    DeauthorizeDataShare (DeauthorizeDataShare'),
    newDeauthorizeDataShare,
    DataShare (DataShare'),
    newDataShare,

    -- ** ModifyScheduledAction
    ModifyScheduledAction (ModifyScheduledAction'),
    newModifyScheduledAction,
    ScheduledAction (ScheduledAction'),
    newScheduledAction,

    -- ** ModifyEventSubscription
    ModifyEventSubscription (ModifyEventSubscription'),
    newModifyEventSubscription,
    ModifyEventSubscriptionResponse (ModifyEventSubscriptionResponse'),
    newModifyEventSubscriptionResponse,

    -- ** CreateClusterSecurityGroup
    CreateClusterSecurityGroup (CreateClusterSecurityGroup'),
    newCreateClusterSecurityGroup,
    CreateClusterSecurityGroupResponse (CreateClusterSecurityGroupResponse'),
    newCreateClusterSecurityGroupResponse,

    -- ** DescribeResize
    DescribeResize (DescribeResize'),
    newDescribeResize,
    ResizeProgressMessage (ResizeProgressMessage'),
    newResizeProgressMessage,

    -- ** AcceptReservedNodeExchange
    AcceptReservedNodeExchange (AcceptReservedNodeExchange'),
    newAcceptReservedNodeExchange,
    AcceptReservedNodeExchangeResponse (AcceptReservedNodeExchangeResponse'),
    newAcceptReservedNodeExchangeResponse,

    -- ** DescribeSnapshotSchedules (Paginated)
    DescribeSnapshotSchedules (DescribeSnapshotSchedules'),
    newDescribeSnapshotSchedules,
    DescribeSnapshotSchedulesResponse (DescribeSnapshotSchedulesResponse'),
    newDescribeSnapshotSchedulesResponse,

    -- ** ModifyClusterMaintenance
    ModifyClusterMaintenance (ModifyClusterMaintenance'),
    newModifyClusterMaintenance,
    ModifyClusterMaintenanceResponse (ModifyClusterMaintenanceResponse'),
    newModifyClusterMaintenanceResponse,

    -- ** DescribeStorage
    DescribeStorage (DescribeStorage'),
    newDescribeStorage,
    DescribeStorageResponse (DescribeStorageResponse'),
    newDescribeStorageResponse,

    -- ** DisassociateDataShareConsumer
    DisassociateDataShareConsumer (DisassociateDataShareConsumer'),
    newDisassociateDataShareConsumer,
    DataShare (DataShare'),
    newDataShare,

    -- ** BatchModifyClusterSnapshots
    BatchModifyClusterSnapshots (BatchModifyClusterSnapshots'),
    newBatchModifyClusterSnapshots,
    BatchModifyClusterSnapshotsResponse (BatchModifyClusterSnapshotsResponse'),
    newBatchModifyClusterSnapshotsResponse,

    -- ** DescribeSnapshotCopyGrants (Paginated)
    DescribeSnapshotCopyGrants (DescribeSnapshotCopyGrants'),
    newDescribeSnapshotCopyGrants,
    DescribeSnapshotCopyGrantsResponse (DescribeSnapshotCopyGrantsResponse'),
    newDescribeSnapshotCopyGrantsResponse,

    -- ** ModifySnapshotSchedule
    ModifySnapshotSchedule (ModifySnapshotSchedule'),
    newModifySnapshotSchedule,
    SnapshotSchedule (SnapshotSchedule'),
    newSnapshotSchedule,

    -- ** CreateHsmClientCertificate
    CreateHsmClientCertificate (CreateHsmClientCertificate'),
    newCreateHsmClientCertificate,
    CreateHsmClientCertificateResponse (CreateHsmClientCertificateResponse'),
    newCreateHsmClientCertificateResponse,

    -- ** DescribeClusterVersions (Paginated)
    DescribeClusterVersions (DescribeClusterVersions'),
    newDescribeClusterVersions,
    DescribeClusterVersionsResponse (DescribeClusterVersionsResponse'),
    newDescribeClusterVersionsResponse,

    -- ** AuthorizeDataShare
    AuthorizeDataShare (AuthorizeDataShare'),
    newAuthorizeDataShare,
    DataShare (DataShare'),
    newDataShare,

    -- ** DescribeDefaultClusterParameters (Paginated)
    DescribeDefaultClusterParameters (DescribeDefaultClusterParameters'),
    newDescribeDefaultClusterParameters,
    DescribeDefaultClusterParametersResponse (DescribeDefaultClusterParametersResponse'),
    newDescribeDefaultClusterParametersResponse,

    -- ** DeleteSnapshotCopyGrant
    DeleteSnapshotCopyGrant (DeleteSnapshotCopyGrant'),
    newDeleteSnapshotCopyGrant,
    DeleteSnapshotCopyGrantResponse (DeleteSnapshotCopyGrantResponse'),
    newDeleteSnapshotCopyGrantResponse,

    -- ** DescribeUsageLimits (Paginated)
    DescribeUsageLimits (DescribeUsageLimits'),
    newDescribeUsageLimits,
    DescribeUsageLimitsResponse (DescribeUsageLimitsResponse'),
    newDescribeUsageLimitsResponse,

    -- ** DescribeClusterTracks (Paginated)
    DescribeClusterTracks (DescribeClusterTracks'),
    newDescribeClusterTracks,
    DescribeClusterTracksResponse (DescribeClusterTracksResponse'),
    newDescribeClusterTracksResponse,

    -- ** DescribeOrderableClusterOptions (Paginated)
    DescribeOrderableClusterOptions (DescribeOrderableClusterOptions'),
    newDescribeOrderableClusterOptions,
    DescribeOrderableClusterOptionsResponse (DescribeOrderableClusterOptionsResponse'),
    newDescribeOrderableClusterOptionsResponse,

    -- ** CreateEventSubscription
    CreateEventSubscription (CreateEventSubscription'),
    newCreateEventSubscription,
    CreateEventSubscriptionResponse (CreateEventSubscriptionResponse'),
    newCreateEventSubscriptionResponse,

    -- ** CreateScheduledAction
    CreateScheduledAction (CreateScheduledAction'),
    newCreateScheduledAction,
    ScheduledAction (ScheduledAction'),
    newScheduledAction,

    -- ** AuthorizeSnapshotAccess
    AuthorizeSnapshotAccess (AuthorizeSnapshotAccess'),
    newAuthorizeSnapshotAccess,
    AuthorizeSnapshotAccessResponse (AuthorizeSnapshotAccessResponse'),
    newAuthorizeSnapshotAccessResponse,

    -- ** DeleteHsmClientCertificate
    DeleteHsmClientCertificate (DeleteHsmClientCertificate'),
    newDeleteHsmClientCertificate,
    DeleteHsmClientCertificateResponse (DeleteHsmClientCertificateResponse'),
    newDeleteHsmClientCertificateResponse,

    -- ** DeleteCluster
    DeleteCluster (DeleteCluster'),
    newDeleteCluster,
    DeleteClusterResponse (DeleteClusterResponse'),
    newDeleteClusterResponse,

    -- ** DescribeDataShares
    DescribeDataShares (DescribeDataShares'),
    newDescribeDataShares,
    DescribeDataSharesResponse (DescribeDataSharesResponse'),
    newDescribeDataSharesResponse,

    -- ** RebootCluster
    RebootCluster (RebootCluster'),
    newRebootCluster,
    RebootClusterResponse (RebootClusterResponse'),
    newRebootClusterResponse,

    -- ** ResumeCluster
    ResumeCluster (ResumeCluster'),
    newResumeCluster,
    ResumeClusterResponse (ResumeClusterResponse'),
    newResumeClusterResponse,

    -- ** ModifyClusterIamRoles
    ModifyClusterIamRoles (ModifyClusterIamRoles'),
    newModifyClusterIamRoles,
    ModifyClusterIamRolesResponse (ModifyClusterIamRolesResponse'),
    newModifyClusterIamRolesResponse,

    -- ** RestoreFromClusterSnapshot
    RestoreFromClusterSnapshot (RestoreFromClusterSnapshot'),
    newRestoreFromClusterSnapshot,
    RestoreFromClusterSnapshotResponse (RestoreFromClusterSnapshotResponse'),
    newRestoreFromClusterSnapshotResponse,

    -- ** PauseCluster
    PauseCluster (PauseCluster'),
    newPauseCluster,
    PauseClusterResponse (PauseClusterResponse'),
    newPauseClusterResponse,

    -- ** DeleteSnapshotSchedule
    DeleteSnapshotSchedule (DeleteSnapshotSchedule'),
    newDeleteSnapshotSchedule,
    DeleteSnapshotScheduleResponse (DeleteSnapshotScheduleResponse'),
    newDeleteSnapshotScheduleResponse,

    -- ** ModifySnapshotCopyRetentionPeriod
    ModifySnapshotCopyRetentionPeriod (ModifySnapshotCopyRetentionPeriod'),
    newModifySnapshotCopyRetentionPeriod,
    ModifySnapshotCopyRetentionPeriodResponse (ModifySnapshotCopyRetentionPeriodResponse'),
    newModifySnapshotCopyRetentionPeriodResponse,

    -- ** DescribeClusterSubnetGroups (Paginated)
    DescribeClusterSubnetGroups (DescribeClusterSubnetGroups'),
    newDescribeClusterSubnetGroups,
    DescribeClusterSubnetGroupsResponse (DescribeClusterSubnetGroupsResponse'),
    newDescribeClusterSubnetGroupsResponse,

    -- ** ResizeCluster
    ResizeCluster (ResizeCluster'),
    newResizeCluster,
    ResizeClusterResponse (ResizeClusterResponse'),
    newResizeClusterResponse,

    -- ** BatchDeleteClusterSnapshots
    BatchDeleteClusterSnapshots (BatchDeleteClusterSnapshots'),
    newBatchDeleteClusterSnapshots,
    BatchDeleteClusterSnapshotsResponse (BatchDeleteClusterSnapshotsResponse'),
    newBatchDeleteClusterSnapshotsResponse,

    -- ** DescribePartners
    DescribePartners (DescribePartners'),
    newDescribePartners,
    DescribePartnersResponse (DescribePartnersResponse'),
    newDescribePartnersResponse,

    -- ** RevokeSnapshotAccess
    RevokeSnapshotAccess (RevokeSnapshotAccess'),
    newRevokeSnapshotAccess,
    RevokeSnapshotAccessResponse (RevokeSnapshotAccessResponse'),
    newRevokeSnapshotAccessResponse,

    -- ** CreateTags
    CreateTags (CreateTags'),
    newCreateTags,
    CreateTagsResponse (CreateTagsResponse'),
    newCreateTagsResponse,

    -- ** DisableSnapshotCopy
    DisableSnapshotCopy (DisableSnapshotCopy'),
    newDisableSnapshotCopy,
    DisableSnapshotCopyResponse (DisableSnapshotCopyResponse'),
    newDisableSnapshotCopyResponse,

    -- ** DescribeClusterParameters (Paginated)
    DescribeClusterParameters (DescribeClusterParameters'),
    newDescribeClusterParameters,
    DescribeClusterParametersResponse (DescribeClusterParametersResponse'),
    newDescribeClusterParametersResponse,

    -- ** DeleteAuthenticationProfile
    DeleteAuthenticationProfile (DeleteAuthenticationProfile'),
    newDeleteAuthenticationProfile,
    DeleteAuthenticationProfileResponse (DeleteAuthenticationProfileResponse'),
    newDeleteAuthenticationProfileResponse,

    -- ** DeleteClusterSecurityGroup
    DeleteClusterSecurityGroup (DeleteClusterSecurityGroup'),
    newDeleteClusterSecurityGroup,
    DeleteClusterSecurityGroupResponse (DeleteClusterSecurityGroupResponse'),
    newDeleteClusterSecurityGroupResponse,

    -- ** DescribeReservedNodeOfferings (Paginated)
    DescribeReservedNodeOfferings (DescribeReservedNodeOfferings'),
    newDescribeReservedNodeOfferings,
    DescribeReservedNodeOfferingsResponse (DescribeReservedNodeOfferingsResponse'),
    newDescribeReservedNodeOfferingsResponse,

    -- ** EnableLogging
    EnableLogging (EnableLogging'),
    newEnableLogging,
    LoggingStatus (LoggingStatus'),
    newLoggingStatus,

    -- ** DescribeReservedNodes (Paginated)
    DescribeReservedNodes (DescribeReservedNodes'),
    newDescribeReservedNodes,
    DescribeReservedNodesResponse (DescribeReservedNodesResponse'),
    newDescribeReservedNodesResponse,

    -- ** CreateHsmConfiguration
    CreateHsmConfiguration (CreateHsmConfiguration'),
    newCreateHsmConfiguration,
    CreateHsmConfigurationResponse (CreateHsmConfigurationResponse'),
    newCreateHsmConfigurationResponse,

    -- * Types

    -- ** Common
    module Network.AWS.Redshift.Internal,

    -- ** ActionType
    ActionType (..),

    -- ** AquaConfigurationStatus
    AquaConfigurationStatus (..),

    -- ** AquaStatus
    AquaStatus (..),

    -- ** AuthorizationStatus
    AuthorizationStatus (..),

    -- ** DataShareStatus
    DataShareStatus (..),

    -- ** DataShareStatusForConsumer
    DataShareStatusForConsumer (..),

    -- ** DataShareStatusForProducer
    DataShareStatusForProducer (..),

    -- ** Mode
    Mode (..),

    -- ** NodeConfigurationOptionsFilterName
    NodeConfigurationOptionsFilterName (..),

    -- ** OperatorType
    OperatorType (..),

    -- ** ParameterApplyType
    ParameterApplyType (..),

    -- ** PartnerIntegrationStatus
    PartnerIntegrationStatus (..),

    -- ** ReservedNodeOfferingType
    ReservedNodeOfferingType (..),

    -- ** ScheduleState
    ScheduleState (..),

    -- ** ScheduledActionFilterName
    ScheduledActionFilterName (..),

    -- ** ScheduledActionState
    ScheduledActionState (..),

    -- ** ScheduledActionTypeValues
    ScheduledActionTypeValues (..),

    -- ** SnapshotAttributeToSortBy
    SnapshotAttributeToSortBy (..),

    -- ** SortByOrder
    SortByOrder (..),

    -- ** SourceType
    SourceType (..),

    -- ** TableRestoreStatusType
    TableRestoreStatusType (..),

    -- ** UsageLimitBreachAction
    UsageLimitBreachAction (..),

    -- ** UsageLimitFeatureType
    UsageLimitFeatureType (..),

    -- ** UsageLimitLimitType
    UsageLimitLimitType (..),

    -- ** UsageLimitPeriod
    UsageLimitPeriod (..),

    -- ** AccountAttribute
    AccountAttribute (AccountAttribute'),
    newAccountAttribute,

    -- ** AccountWithRestoreAccess
    AccountWithRestoreAccess (AccountWithRestoreAccess'),
    newAccountWithRestoreAccess,

    -- ** AquaConfiguration
    AquaConfiguration (AquaConfiguration'),
    newAquaConfiguration,

    -- ** AttributeValueTarget
    AttributeValueTarget (AttributeValueTarget'),
    newAttributeValueTarget,

    -- ** AuthenticationProfile
    AuthenticationProfile (AuthenticationProfile'),
    newAuthenticationProfile,

    -- ** AvailabilityZone
    AvailabilityZone (AvailabilityZone'),
    newAvailabilityZone,

    -- ** Cluster
    Cluster (Cluster'),
    newCluster,

    -- ** ClusterAssociatedToSchedule
    ClusterAssociatedToSchedule (ClusterAssociatedToSchedule'),
    newClusterAssociatedToSchedule,

    -- ** ClusterDbRevision
    ClusterDbRevision (ClusterDbRevision'),
    newClusterDbRevision,

    -- ** ClusterIamRole
    ClusterIamRole (ClusterIamRole'),
    newClusterIamRole,

    -- ** ClusterNode
    ClusterNode (ClusterNode'),
    newClusterNode,

    -- ** ClusterParameterGroup
    ClusterParameterGroup (ClusterParameterGroup'),
    newClusterParameterGroup,

    -- ** ClusterParameterGroupNameMessage
    ClusterParameterGroupNameMessage (ClusterParameterGroupNameMessage'),
    newClusterParameterGroupNameMessage,

    -- ** ClusterParameterGroupStatus
    ClusterParameterGroupStatus (ClusterParameterGroupStatus'),
    newClusterParameterGroupStatus,

    -- ** ClusterParameterStatus
    ClusterParameterStatus (ClusterParameterStatus'),
    newClusterParameterStatus,

    -- ** ClusterSecurityGroup
    ClusterSecurityGroup (ClusterSecurityGroup'),
    newClusterSecurityGroup,

    -- ** ClusterSecurityGroupMembership
    ClusterSecurityGroupMembership (ClusterSecurityGroupMembership'),
    newClusterSecurityGroupMembership,

    -- ** ClusterSnapshotCopyStatus
    ClusterSnapshotCopyStatus (ClusterSnapshotCopyStatus'),
    newClusterSnapshotCopyStatus,

    -- ** ClusterSubnetGroup
    ClusterSubnetGroup (ClusterSubnetGroup'),
    newClusterSubnetGroup,

    -- ** ClusterVersion
    ClusterVersion (ClusterVersion'),
    newClusterVersion,

    -- ** DataShare
    DataShare (DataShare'),
    newDataShare,

    -- ** DataShareAssociation
    DataShareAssociation (DataShareAssociation'),
    newDataShareAssociation,

    -- ** DataTransferProgress
    DataTransferProgress (DataTransferProgress'),
    newDataTransferProgress,

    -- ** DefaultClusterParameters
    DefaultClusterParameters (DefaultClusterParameters'),
    newDefaultClusterParameters,

    -- ** DeferredMaintenanceWindow
    DeferredMaintenanceWindow (DeferredMaintenanceWindow'),
    newDeferredMaintenanceWindow,

    -- ** DeleteClusterSnapshotMessage
    DeleteClusterSnapshotMessage (DeleteClusterSnapshotMessage'),
    newDeleteClusterSnapshotMessage,

    -- ** EC2SecurityGroup
    EC2SecurityGroup (EC2SecurityGroup'),
    newEC2SecurityGroup,

    -- ** ElasticIpStatus
    ElasticIpStatus (ElasticIpStatus'),
    newElasticIpStatus,

    -- ** Endpoint
    Endpoint (Endpoint'),
    newEndpoint,

    -- ** EndpointAccess
    EndpointAccess (EndpointAccess'),
    newEndpointAccess,

    -- ** EndpointAuthorization
    EndpointAuthorization (EndpointAuthorization'),
    newEndpointAuthorization,

    -- ** Event
    Event (Event'),
    newEvent,

    -- ** EventCategoriesMap
    EventCategoriesMap (EventCategoriesMap'),
    newEventCategoriesMap,

    -- ** EventInfoMap
    EventInfoMap (EventInfoMap'),
    newEventInfoMap,

    -- ** EventSubscription
    EventSubscription (EventSubscription'),
    newEventSubscription,

    -- ** HsmClientCertificate
    HsmClientCertificate (HsmClientCertificate'),
    newHsmClientCertificate,

    -- ** HsmConfiguration
    HsmConfiguration (HsmConfiguration'),
    newHsmConfiguration,

    -- ** HsmStatus
    HsmStatus (HsmStatus'),
    newHsmStatus,

    -- ** IPRange
    IPRange (IPRange'),
    newIPRange,

    -- ** LoggingStatus
    LoggingStatus (LoggingStatus'),
    newLoggingStatus,

    -- ** MaintenanceTrack
    MaintenanceTrack (MaintenanceTrack'),
    newMaintenanceTrack,

    -- ** NetworkInterface
    NetworkInterface (NetworkInterface'),
    newNetworkInterface,

    -- ** NodeConfigurationOption
    NodeConfigurationOption (NodeConfigurationOption'),
    newNodeConfigurationOption,

    -- ** NodeConfigurationOptionsFilter
    NodeConfigurationOptionsFilter (NodeConfigurationOptionsFilter'),
    newNodeConfigurationOptionsFilter,

    -- ** OrderableClusterOption
    OrderableClusterOption (OrderableClusterOption'),
    newOrderableClusterOption,

    -- ** Parameter
    Parameter (Parameter'),
    newParameter,

    -- ** PartnerIntegrationInfo
    PartnerIntegrationInfo (PartnerIntegrationInfo'),
    newPartnerIntegrationInfo,

    -- ** PartnerIntegrationInputMessage
    PartnerIntegrationInputMessage (PartnerIntegrationInputMessage'),
    newPartnerIntegrationInputMessage,

    -- ** PartnerIntegrationOutputMessage
    PartnerIntegrationOutputMessage (PartnerIntegrationOutputMessage'),
    newPartnerIntegrationOutputMessage,

    -- ** PauseClusterMessage
    PauseClusterMessage (PauseClusterMessage'),
    newPauseClusterMessage,

    -- ** PendingModifiedValues
    PendingModifiedValues (PendingModifiedValues'),
    newPendingModifiedValues,

    -- ** RecurringCharge
    RecurringCharge (RecurringCharge'),
    newRecurringCharge,

    -- ** ReservedNode
    ReservedNode (ReservedNode'),
    newReservedNode,

    -- ** ReservedNodeOffering
    ReservedNodeOffering (ReservedNodeOffering'),
    newReservedNodeOffering,

    -- ** ResizeClusterMessage
    ResizeClusterMessage (ResizeClusterMessage'),
    newResizeClusterMessage,

    -- ** ResizeInfo
    ResizeInfo (ResizeInfo'),
    newResizeInfo,

    -- ** ResizeProgressMessage
    ResizeProgressMessage (ResizeProgressMessage'),
    newResizeProgressMessage,

    -- ** RestoreStatus
    RestoreStatus (RestoreStatus'),
    newRestoreStatus,

    -- ** ResumeClusterMessage
    ResumeClusterMessage (ResumeClusterMessage'),
    newResumeClusterMessage,

    -- ** RevisionTarget
    RevisionTarget (RevisionTarget'),
    newRevisionTarget,

    -- ** ScheduledAction
    ScheduledAction (ScheduledAction'),
    newScheduledAction,

    -- ** ScheduledActionFilter
    ScheduledActionFilter (ScheduledActionFilter'),
    newScheduledActionFilter,

    -- ** ScheduledActionType
    ScheduledActionType (ScheduledActionType'),
    newScheduledActionType,

    -- ** Snapshot
    Snapshot (Snapshot'),
    newSnapshot,

    -- ** SnapshotCopyGrant
    SnapshotCopyGrant (SnapshotCopyGrant'),
    newSnapshotCopyGrant,

    -- ** SnapshotErrorMessage
    SnapshotErrorMessage (SnapshotErrorMessage'),
    newSnapshotErrorMessage,

    -- ** SnapshotSchedule
    SnapshotSchedule (SnapshotSchedule'),
    newSnapshotSchedule,

    -- ** SnapshotSortingEntity
    SnapshotSortingEntity (SnapshotSortingEntity'),
    newSnapshotSortingEntity,

    -- ** Subnet
    Subnet (Subnet'),
    newSubnet,

    -- ** SupportedOperation
    SupportedOperation (SupportedOperation'),
    newSupportedOperation,

    -- ** SupportedPlatform
    SupportedPlatform (SupportedPlatform'),
    newSupportedPlatform,

    -- ** TableRestoreStatus
    TableRestoreStatus (TableRestoreStatus'),
    newTableRestoreStatus,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TaggedResource
    TaggedResource (TaggedResource'),
    newTaggedResource,

    -- ** UpdateTarget
    UpdateTarget (UpdateTarget'),
    newUpdateTarget,

    -- ** UsageLimit
    UsageLimit (UsageLimit'),
    newUsageLimit,

    -- ** VpcEndpoint
    VpcEndpoint (VpcEndpoint'),
    newVpcEndpoint,

    -- ** VpcSecurityGroupMembership
    VpcSecurityGroupMembership (VpcSecurityGroupMembership'),
    newVpcSecurityGroupMembership,
  )
where

import Network.AWS.Redshift.AcceptReservedNodeExchange
import Network.AWS.Redshift.AddPartner
import Network.AWS.Redshift.AssociateDataShareConsumer
import Network.AWS.Redshift.AuthorizeClusterSecurityGroupIngress
import Network.AWS.Redshift.AuthorizeDataShare
import Network.AWS.Redshift.AuthorizeEndpointAccess
import Network.AWS.Redshift.AuthorizeSnapshotAccess
import Network.AWS.Redshift.BatchDeleteClusterSnapshots
import Network.AWS.Redshift.BatchModifyClusterSnapshots
import Network.AWS.Redshift.CancelResize
import Network.AWS.Redshift.CopyClusterSnapshot
import Network.AWS.Redshift.CreateAuthenticationProfile
import Network.AWS.Redshift.CreateCluster
import Network.AWS.Redshift.CreateClusterParameterGroup
import Network.AWS.Redshift.CreateClusterSecurityGroup
import Network.AWS.Redshift.CreateClusterSnapshot
import Network.AWS.Redshift.CreateClusterSubnetGroup
import Network.AWS.Redshift.CreateEndpointAccess
import Network.AWS.Redshift.CreateEventSubscription
import Network.AWS.Redshift.CreateHsmClientCertificate
import Network.AWS.Redshift.CreateHsmConfiguration
import Network.AWS.Redshift.CreateScheduledAction
import Network.AWS.Redshift.CreateSnapshotCopyGrant
import Network.AWS.Redshift.CreateSnapshotSchedule
import Network.AWS.Redshift.CreateTags
import Network.AWS.Redshift.CreateUsageLimit
import Network.AWS.Redshift.DeauthorizeDataShare
import Network.AWS.Redshift.DeleteAuthenticationProfile
import Network.AWS.Redshift.DeleteCluster
import Network.AWS.Redshift.DeleteClusterParameterGroup
import Network.AWS.Redshift.DeleteClusterSecurityGroup
import Network.AWS.Redshift.DeleteClusterSnapshot
import Network.AWS.Redshift.DeleteClusterSubnetGroup
import Network.AWS.Redshift.DeleteEndpointAccess
import Network.AWS.Redshift.DeleteEventSubscription
import Network.AWS.Redshift.DeleteHsmClientCertificate
import Network.AWS.Redshift.DeleteHsmConfiguration
import Network.AWS.Redshift.DeletePartner
import Network.AWS.Redshift.DeleteScheduledAction
import Network.AWS.Redshift.DeleteSnapshotCopyGrant
import Network.AWS.Redshift.DeleteSnapshotSchedule
import Network.AWS.Redshift.DeleteTags
import Network.AWS.Redshift.DeleteUsageLimit
import Network.AWS.Redshift.DescribeAccountAttributes
import Network.AWS.Redshift.DescribeAuthenticationProfiles
import Network.AWS.Redshift.DescribeClusterDbRevisions
import Network.AWS.Redshift.DescribeClusterParameterGroups
import Network.AWS.Redshift.DescribeClusterParameters
import Network.AWS.Redshift.DescribeClusterSecurityGroups
import Network.AWS.Redshift.DescribeClusterSnapshots
import Network.AWS.Redshift.DescribeClusterSubnetGroups
import Network.AWS.Redshift.DescribeClusterTracks
import Network.AWS.Redshift.DescribeClusterVersions
import Network.AWS.Redshift.DescribeClusters
import Network.AWS.Redshift.DescribeDataShares
import Network.AWS.Redshift.DescribeDataSharesForConsumer
import Network.AWS.Redshift.DescribeDataSharesForProducer
import Network.AWS.Redshift.DescribeDefaultClusterParameters
import Network.AWS.Redshift.DescribeEndpointAccess
import Network.AWS.Redshift.DescribeEndpointAuthorization
import Network.AWS.Redshift.DescribeEventCategories
import Network.AWS.Redshift.DescribeEventSubscriptions
import Network.AWS.Redshift.DescribeEvents
import Network.AWS.Redshift.DescribeHsmClientCertificates
import Network.AWS.Redshift.DescribeHsmConfigurations
import Network.AWS.Redshift.DescribeLoggingStatus
import Network.AWS.Redshift.DescribeNodeConfigurationOptions
import Network.AWS.Redshift.DescribeOrderableClusterOptions
import Network.AWS.Redshift.DescribePartners
import Network.AWS.Redshift.DescribeReservedNodeOfferings
import Network.AWS.Redshift.DescribeReservedNodes
import Network.AWS.Redshift.DescribeResize
import Network.AWS.Redshift.DescribeScheduledActions
import Network.AWS.Redshift.DescribeSnapshotCopyGrants
import Network.AWS.Redshift.DescribeSnapshotSchedules
import Network.AWS.Redshift.DescribeStorage
import Network.AWS.Redshift.DescribeTableRestoreStatus
import Network.AWS.Redshift.DescribeTags
import Network.AWS.Redshift.DescribeUsageLimits
import Network.AWS.Redshift.DisableLogging
import Network.AWS.Redshift.DisableSnapshotCopy
import Network.AWS.Redshift.DisassociateDataShareConsumer
import Network.AWS.Redshift.EnableLogging
import Network.AWS.Redshift.EnableSnapshotCopy
import Network.AWS.Redshift.GetClusterCredentials
import Network.AWS.Redshift.GetReservedNodeExchangeOfferings
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Lens
import Network.AWS.Redshift.ModifyAquaConfiguration
import Network.AWS.Redshift.ModifyAuthenticationProfile
import Network.AWS.Redshift.ModifyCluster
import Network.AWS.Redshift.ModifyClusterDbRevision
import Network.AWS.Redshift.ModifyClusterIamRoles
import Network.AWS.Redshift.ModifyClusterMaintenance
import Network.AWS.Redshift.ModifyClusterParameterGroup
import Network.AWS.Redshift.ModifyClusterSnapshot
import Network.AWS.Redshift.ModifyClusterSnapshotSchedule
import Network.AWS.Redshift.ModifyClusterSubnetGroup
import Network.AWS.Redshift.ModifyEndpointAccess
import Network.AWS.Redshift.ModifyEventSubscription
import Network.AWS.Redshift.ModifyScheduledAction
import Network.AWS.Redshift.ModifySnapshotCopyRetentionPeriod
import Network.AWS.Redshift.ModifySnapshotSchedule
import Network.AWS.Redshift.ModifyUsageLimit
import Network.AWS.Redshift.PauseCluster
import Network.AWS.Redshift.PurchaseReservedNodeOffering
import Network.AWS.Redshift.RebootCluster
import Network.AWS.Redshift.RejectDataShare
import Network.AWS.Redshift.ResetClusterParameterGroup
import Network.AWS.Redshift.ResizeCluster
import Network.AWS.Redshift.RestoreFromClusterSnapshot
import Network.AWS.Redshift.RestoreTableFromClusterSnapshot
import Network.AWS.Redshift.ResumeCluster
import Network.AWS.Redshift.RevokeClusterSecurityGroupIngress
import Network.AWS.Redshift.RevokeEndpointAccess
import Network.AWS.Redshift.RevokeSnapshotAccess
import Network.AWS.Redshift.RotateEncryptionKey
import Network.AWS.Redshift.Types
import Network.AWS.Redshift.UpdatePartnerStatus
import Network.AWS.Redshift.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Redshift'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
