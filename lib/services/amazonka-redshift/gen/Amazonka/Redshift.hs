{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Redshift
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.Redshift
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessToClusterDeniedFault
    _AccessToClusterDeniedFault,

    -- ** AccessToSnapshotDeniedFault
    _AccessToSnapshotDeniedFault,

    -- ** AuthenticationProfileAlreadyExistsFault
    _AuthenticationProfileAlreadyExistsFault,

    -- ** AuthenticationProfileNotFoundFault
    _AuthenticationProfileNotFoundFault,

    -- ** AuthenticationProfileQuotaExceededFault
    _AuthenticationProfileQuotaExceededFault,

    -- ** AuthorizationAlreadyExistsFault
    _AuthorizationAlreadyExistsFault,

    -- ** AuthorizationNotFoundFault
    _AuthorizationNotFoundFault,

    -- ** AuthorizationQuotaExceededFault
    _AuthorizationQuotaExceededFault,

    -- ** BatchDeleteRequestSizeExceededFault
    _BatchDeleteRequestSizeExceededFault,

    -- ** BatchModifyClusterSnapshotsLimitExceededFault
    _BatchModifyClusterSnapshotsLimitExceededFault,

    -- ** BucketNotFoundFault
    _BucketNotFoundFault,

    -- ** ClusterAlreadyExistsFault
    _ClusterAlreadyExistsFault,

    -- ** ClusterNotFoundFault
    _ClusterNotFoundFault,

    -- ** ClusterOnLatestRevisionFault
    _ClusterOnLatestRevisionFault,

    -- ** ClusterParameterGroupAlreadyExistsFault
    _ClusterParameterGroupAlreadyExistsFault,

    -- ** ClusterParameterGroupNotFoundFault
    _ClusterParameterGroupNotFoundFault,

    -- ** ClusterParameterGroupQuotaExceededFault
    _ClusterParameterGroupQuotaExceededFault,

    -- ** ClusterQuotaExceededFault
    _ClusterQuotaExceededFault,

    -- ** ClusterSecurityGroupAlreadyExistsFault
    _ClusterSecurityGroupAlreadyExistsFault,

    -- ** ClusterSecurityGroupNotFoundFault
    _ClusterSecurityGroupNotFoundFault,

    -- ** ClusterSecurityGroupQuotaExceededFault
    _ClusterSecurityGroupQuotaExceededFault,

    -- ** ClusterSnapshotAlreadyExistsFault
    _ClusterSnapshotAlreadyExistsFault,

    -- ** ClusterSnapshotNotFoundFault
    _ClusterSnapshotNotFoundFault,

    -- ** ClusterSnapshotQuotaExceededFault
    _ClusterSnapshotQuotaExceededFault,

    -- ** ClusterSubnetGroupAlreadyExistsFault
    _ClusterSubnetGroupAlreadyExistsFault,

    -- ** ClusterSubnetGroupNotFoundFault
    _ClusterSubnetGroupNotFoundFault,

    -- ** ClusterSubnetGroupQuotaExceededFault
    _ClusterSubnetGroupQuotaExceededFault,

    -- ** ClusterSubnetQuotaExceededFault
    _ClusterSubnetQuotaExceededFault,

    -- ** CopyToRegionDisabledFault
    _CopyToRegionDisabledFault,

    -- ** DependentServiceRequestThrottlingFault
    _DependentServiceRequestThrottlingFault,

    -- ** DependentServiceUnavailableFault
    _DependentServiceUnavailableFault,

    -- ** EndpointAlreadyExistsFault
    _EndpointAlreadyExistsFault,

    -- ** EndpointAuthorizationAlreadyExistsFault
    _EndpointAuthorizationAlreadyExistsFault,

    -- ** EndpointAuthorizationNotFoundFault
    _EndpointAuthorizationNotFoundFault,

    -- ** EndpointAuthorizationsPerClusterLimitExceededFault
    _EndpointAuthorizationsPerClusterLimitExceededFault,

    -- ** EndpointNotFoundFault
    _EndpointNotFoundFault,

    -- ** EndpointsPerAuthorizationLimitExceededFault
    _EndpointsPerAuthorizationLimitExceededFault,

    -- ** EndpointsPerClusterLimitExceededFault
    _EndpointsPerClusterLimitExceededFault,

    -- ** EventSubscriptionQuotaExceededFault
    _EventSubscriptionQuotaExceededFault,

    -- ** HsmClientCertificateAlreadyExistsFault
    _HsmClientCertificateAlreadyExistsFault,

    -- ** HsmClientCertificateNotFoundFault
    _HsmClientCertificateNotFoundFault,

    -- ** HsmClientCertificateQuotaExceededFault
    _HsmClientCertificateQuotaExceededFault,

    -- ** HsmConfigurationAlreadyExistsFault
    _HsmConfigurationAlreadyExistsFault,

    -- ** HsmConfigurationNotFoundFault
    _HsmConfigurationNotFoundFault,

    -- ** HsmConfigurationQuotaExceededFault
    _HsmConfigurationQuotaExceededFault,

    -- ** InProgressTableRestoreQuotaExceededFault
    _InProgressTableRestoreQuotaExceededFault,

    -- ** IncompatibleOrderableOptions
    _IncompatibleOrderableOptions,

    -- ** InsufficientClusterCapacityFault
    _InsufficientClusterCapacityFault,

    -- ** InsufficientS3BucketPolicyFault
    _InsufficientS3BucketPolicyFault,

    -- ** InvalidAuthenticationProfileRequestFault
    _InvalidAuthenticationProfileRequestFault,

    -- ** InvalidAuthorizationStateFault
    _InvalidAuthorizationStateFault,

    -- ** InvalidClusterParameterGroupStateFault
    _InvalidClusterParameterGroupStateFault,

    -- ** InvalidClusterSecurityGroupStateFault
    _InvalidClusterSecurityGroupStateFault,

    -- ** InvalidClusterSnapshotScheduleStateFault
    _InvalidClusterSnapshotScheduleStateFault,

    -- ** InvalidClusterSnapshotStateFault
    _InvalidClusterSnapshotStateFault,

    -- ** InvalidClusterStateFault
    _InvalidClusterStateFault,

    -- ** InvalidClusterSubnetGroupStateFault
    _InvalidClusterSubnetGroupStateFault,

    -- ** InvalidClusterSubnetStateFault
    _InvalidClusterSubnetStateFault,

    -- ** InvalidClusterTrackFault
    _InvalidClusterTrackFault,

    -- ** InvalidDataShareFault
    _InvalidDataShareFault,

    -- ** InvalidElasticIpFault
    _InvalidElasticIpFault,

    -- ** InvalidEndpointStateFault
    _InvalidEndpointStateFault,

    -- ** InvalidHsmClientCertificateStateFault
    _InvalidHsmClientCertificateStateFault,

    -- ** InvalidHsmConfigurationStateFault
    _InvalidHsmConfigurationStateFault,

    -- ** InvalidNamespaceFault
    _InvalidNamespaceFault,

    -- ** InvalidReservedNodeStateFault
    _InvalidReservedNodeStateFault,

    -- ** InvalidRestoreFault
    _InvalidRestoreFault,

    -- ** InvalidRetentionPeriodFault
    _InvalidRetentionPeriodFault,

    -- ** InvalidS3BucketNameFault
    _InvalidS3BucketNameFault,

    -- ** InvalidS3KeyPrefixFault
    _InvalidS3KeyPrefixFault,

    -- ** InvalidScheduleFault
    _InvalidScheduleFault,

    -- ** InvalidScheduledActionFault
    _InvalidScheduledActionFault,

    -- ** InvalidSnapshotCopyGrantStateFault
    _InvalidSnapshotCopyGrantStateFault,

    -- ** InvalidSubnet
    _InvalidSubnet,

    -- ** InvalidSubscriptionStateFault
    _InvalidSubscriptionStateFault,

    -- ** InvalidTableRestoreArgumentFault
    _InvalidTableRestoreArgumentFault,

    -- ** InvalidTagFault
    _InvalidTagFault,

    -- ** InvalidUsageLimitFault
    _InvalidUsageLimitFault,

    -- ** InvalidVPCNetworkStateFault
    _InvalidVPCNetworkStateFault,

    -- ** LimitExceededFault
    _LimitExceededFault,

    -- ** NumberOfNodesPerClusterLimitExceededFault
    _NumberOfNodesPerClusterLimitExceededFault,

    -- ** NumberOfNodesQuotaExceededFault
    _NumberOfNodesQuotaExceededFault,

    -- ** PartnerNotFoundFault
    _PartnerNotFoundFault,

    -- ** ReservedNodeAlreadyExistsFault
    _ReservedNodeAlreadyExistsFault,

    -- ** ReservedNodeAlreadyMigratedFault
    _ReservedNodeAlreadyMigratedFault,

    -- ** ReservedNodeExchangeNotFoundFault
    _ReservedNodeExchangeNotFoundFault,

    -- ** ReservedNodeNotFoundFault
    _ReservedNodeNotFoundFault,

    -- ** ReservedNodeOfferingNotFoundFault
    _ReservedNodeOfferingNotFoundFault,

    -- ** ReservedNodeQuotaExceededFault
    _ReservedNodeQuotaExceededFault,

    -- ** ResizeNotFoundFault
    _ResizeNotFoundFault,

    -- ** ResourceNotFoundFault
    _ResourceNotFoundFault,

    -- ** SNSInvalidTopicFault
    _SNSInvalidTopicFault,

    -- ** SNSNoAuthorizationFault
    _SNSNoAuthorizationFault,

    -- ** SNSTopicArnNotFoundFault
    _SNSTopicArnNotFoundFault,

    -- ** ScheduleDefinitionTypeUnsupportedFault
    _ScheduleDefinitionTypeUnsupportedFault,

    -- ** ScheduledActionAlreadyExistsFault
    _ScheduledActionAlreadyExistsFault,

    -- ** ScheduledActionNotFoundFault
    _ScheduledActionNotFoundFault,

    -- ** ScheduledActionQuotaExceededFault
    _ScheduledActionQuotaExceededFault,

    -- ** ScheduledActionTypeUnsupportedFault
    _ScheduledActionTypeUnsupportedFault,

    -- ** SnapshotCopyAlreadyDisabledFault
    _SnapshotCopyAlreadyDisabledFault,

    -- ** SnapshotCopyAlreadyEnabledFault
    _SnapshotCopyAlreadyEnabledFault,

    -- ** SnapshotCopyDisabledFault
    _SnapshotCopyDisabledFault,

    -- ** SnapshotCopyGrantAlreadyExistsFault
    _SnapshotCopyGrantAlreadyExistsFault,

    -- ** SnapshotCopyGrantNotFoundFault
    _SnapshotCopyGrantNotFoundFault,

    -- ** SnapshotCopyGrantQuotaExceededFault
    _SnapshotCopyGrantQuotaExceededFault,

    -- ** SnapshotScheduleAlreadyExistsFault
    _SnapshotScheduleAlreadyExistsFault,

    -- ** SnapshotScheduleNotFoundFault
    _SnapshotScheduleNotFoundFault,

    -- ** SnapshotScheduleQuotaExceededFault
    _SnapshotScheduleQuotaExceededFault,

    -- ** SnapshotScheduleUpdateInProgressFault
    _SnapshotScheduleUpdateInProgressFault,

    -- ** SourceNotFoundFault
    _SourceNotFoundFault,

    -- ** SubnetAlreadyInUse
    _SubnetAlreadyInUse,

    -- ** SubscriptionAlreadyExistFault
    _SubscriptionAlreadyExistFault,

    -- ** SubscriptionCategoryNotFoundFault
    _SubscriptionCategoryNotFoundFault,

    -- ** SubscriptionEventIdNotFoundFault
    _SubscriptionEventIdNotFoundFault,

    -- ** SubscriptionNotFoundFault
    _SubscriptionNotFoundFault,

    -- ** SubscriptionSeverityNotFoundFault
    _SubscriptionSeverityNotFoundFault,

    -- ** TableLimitExceededFault
    _TableLimitExceededFault,

    -- ** TableRestoreNotFoundFault
    _TableRestoreNotFoundFault,

    -- ** TagLimitExceededFault
    _TagLimitExceededFault,

    -- ** UnauthorizedOperation
    _UnauthorizedOperation,

    -- ** UnauthorizedPartnerIntegrationFault
    _UnauthorizedPartnerIntegrationFault,

    -- ** UnknownSnapshotCopyRegionFault
    _UnknownSnapshotCopyRegionFault,

    -- ** UnsupportedOperationFault
    _UnsupportedOperationFault,

    -- ** UnsupportedOptionFault
    _UnsupportedOptionFault,

    -- ** UsageLimitAlreadyExistsFault
    _UsageLimitAlreadyExistsFault,

    -- ** UsageLimitNotFoundFault
    _UsageLimitNotFoundFault,

    -- * Waiters
    -- $waiters

    -- ** ClusterAvailable
    newClusterAvailable,

    -- ** ClusterDeleted
    newClusterDeleted,

    -- ** ClusterRestored
    newClusterRestored,

    -- ** SnapshotAvailable
    newSnapshotAvailable,

    -- * Operations
    -- $operations

    -- ** AcceptReservedNodeExchange
    AcceptReservedNodeExchange (AcceptReservedNodeExchange'),
    newAcceptReservedNodeExchange,
    AcceptReservedNodeExchangeResponse (AcceptReservedNodeExchangeResponse'),
    newAcceptReservedNodeExchangeResponse,

    -- ** AddPartner
    AddPartner (AddPartner'),
    newAddPartner,
    PartnerIntegrationOutputMessage (PartnerIntegrationOutputMessage'),
    newPartnerIntegrationOutputMessage,

    -- ** AssociateDataShareConsumer
    AssociateDataShareConsumer (AssociateDataShareConsumer'),
    newAssociateDataShareConsumer,
    DataShare (DataShare'),
    newDataShare,

    -- ** AuthorizeClusterSecurityGroupIngress
    AuthorizeClusterSecurityGroupIngress (AuthorizeClusterSecurityGroupIngress'),
    newAuthorizeClusterSecurityGroupIngress,
    AuthorizeClusterSecurityGroupIngressResponse (AuthorizeClusterSecurityGroupIngressResponse'),
    newAuthorizeClusterSecurityGroupIngressResponse,

    -- ** AuthorizeDataShare
    AuthorizeDataShare (AuthorizeDataShare'),
    newAuthorizeDataShare,
    DataShare (DataShare'),
    newDataShare,

    -- ** AuthorizeEndpointAccess
    AuthorizeEndpointAccess (AuthorizeEndpointAccess'),
    newAuthorizeEndpointAccess,
    EndpointAuthorization (EndpointAuthorization'),
    newEndpointAuthorization,

    -- ** AuthorizeSnapshotAccess
    AuthorizeSnapshotAccess (AuthorizeSnapshotAccess'),
    newAuthorizeSnapshotAccess,
    AuthorizeSnapshotAccessResponse (AuthorizeSnapshotAccessResponse'),
    newAuthorizeSnapshotAccessResponse,

    -- ** BatchDeleteClusterSnapshots
    BatchDeleteClusterSnapshots (BatchDeleteClusterSnapshots'),
    newBatchDeleteClusterSnapshots,
    BatchDeleteClusterSnapshotsResponse (BatchDeleteClusterSnapshotsResponse'),
    newBatchDeleteClusterSnapshotsResponse,

    -- ** BatchModifyClusterSnapshots
    BatchModifyClusterSnapshots (BatchModifyClusterSnapshots'),
    newBatchModifyClusterSnapshots,
    BatchModifyClusterSnapshotsResponse (BatchModifyClusterSnapshotsResponse'),
    newBatchModifyClusterSnapshotsResponse,

    -- ** CancelResize
    CancelResize (CancelResize'),
    newCancelResize,
    ResizeProgressMessage (ResizeProgressMessage'),
    newResizeProgressMessage,

    -- ** CopyClusterSnapshot
    CopyClusterSnapshot (CopyClusterSnapshot'),
    newCopyClusterSnapshot,
    CopyClusterSnapshotResponse (CopyClusterSnapshotResponse'),
    newCopyClusterSnapshotResponse,

    -- ** CreateAuthenticationProfile
    CreateAuthenticationProfile (CreateAuthenticationProfile'),
    newCreateAuthenticationProfile,
    CreateAuthenticationProfileResponse (CreateAuthenticationProfileResponse'),
    newCreateAuthenticationProfileResponse,

    -- ** CreateCluster
    CreateCluster (CreateCluster'),
    newCreateCluster,
    CreateClusterResponse (CreateClusterResponse'),
    newCreateClusterResponse,

    -- ** CreateClusterParameterGroup
    CreateClusterParameterGroup (CreateClusterParameterGroup'),
    newCreateClusterParameterGroup,
    CreateClusterParameterGroupResponse (CreateClusterParameterGroupResponse'),
    newCreateClusterParameterGroupResponse,

    -- ** CreateClusterSecurityGroup
    CreateClusterSecurityGroup (CreateClusterSecurityGroup'),
    newCreateClusterSecurityGroup,
    CreateClusterSecurityGroupResponse (CreateClusterSecurityGroupResponse'),
    newCreateClusterSecurityGroupResponse,

    -- ** CreateClusterSnapshot
    CreateClusterSnapshot (CreateClusterSnapshot'),
    newCreateClusterSnapshot,
    CreateClusterSnapshotResponse (CreateClusterSnapshotResponse'),
    newCreateClusterSnapshotResponse,

    -- ** CreateClusterSubnetGroup
    CreateClusterSubnetGroup (CreateClusterSubnetGroup'),
    newCreateClusterSubnetGroup,
    CreateClusterSubnetGroupResponse (CreateClusterSubnetGroupResponse'),
    newCreateClusterSubnetGroupResponse,

    -- ** CreateEndpointAccess
    CreateEndpointAccess (CreateEndpointAccess'),
    newCreateEndpointAccess,
    EndpointAccess (EndpointAccess'),
    newEndpointAccess,

    -- ** CreateEventSubscription
    CreateEventSubscription (CreateEventSubscription'),
    newCreateEventSubscription,
    CreateEventSubscriptionResponse (CreateEventSubscriptionResponse'),
    newCreateEventSubscriptionResponse,

    -- ** CreateHsmClientCertificate
    CreateHsmClientCertificate (CreateHsmClientCertificate'),
    newCreateHsmClientCertificate,
    CreateHsmClientCertificateResponse (CreateHsmClientCertificateResponse'),
    newCreateHsmClientCertificateResponse,

    -- ** CreateHsmConfiguration
    CreateHsmConfiguration (CreateHsmConfiguration'),
    newCreateHsmConfiguration,
    CreateHsmConfigurationResponse (CreateHsmConfigurationResponse'),
    newCreateHsmConfigurationResponse,

    -- ** CreateScheduledAction
    CreateScheduledAction (CreateScheduledAction'),
    newCreateScheduledAction,
    ScheduledAction (ScheduledAction'),
    newScheduledAction,

    -- ** CreateSnapshotCopyGrant
    CreateSnapshotCopyGrant (CreateSnapshotCopyGrant'),
    newCreateSnapshotCopyGrant,
    CreateSnapshotCopyGrantResponse (CreateSnapshotCopyGrantResponse'),
    newCreateSnapshotCopyGrantResponse,

    -- ** CreateSnapshotSchedule
    CreateSnapshotSchedule (CreateSnapshotSchedule'),
    newCreateSnapshotSchedule,
    SnapshotSchedule (SnapshotSchedule'),
    newSnapshotSchedule,

    -- ** CreateTags
    CreateTags (CreateTags'),
    newCreateTags,
    CreateTagsResponse (CreateTagsResponse'),
    newCreateTagsResponse,

    -- ** CreateUsageLimit
    CreateUsageLimit (CreateUsageLimit'),
    newCreateUsageLimit,
    UsageLimit (UsageLimit'),
    newUsageLimit,

    -- ** DeauthorizeDataShare
    DeauthorizeDataShare (DeauthorizeDataShare'),
    newDeauthorizeDataShare,
    DataShare (DataShare'),
    newDataShare,

    -- ** DeleteAuthenticationProfile
    DeleteAuthenticationProfile (DeleteAuthenticationProfile'),
    newDeleteAuthenticationProfile,
    DeleteAuthenticationProfileResponse (DeleteAuthenticationProfileResponse'),
    newDeleteAuthenticationProfileResponse,

    -- ** DeleteCluster
    DeleteCluster (DeleteCluster'),
    newDeleteCluster,
    DeleteClusterResponse (DeleteClusterResponse'),
    newDeleteClusterResponse,

    -- ** DeleteClusterParameterGroup
    DeleteClusterParameterGroup (DeleteClusterParameterGroup'),
    newDeleteClusterParameterGroup,
    DeleteClusterParameterGroupResponse (DeleteClusterParameterGroupResponse'),
    newDeleteClusterParameterGroupResponse,

    -- ** DeleteClusterSecurityGroup
    DeleteClusterSecurityGroup (DeleteClusterSecurityGroup'),
    newDeleteClusterSecurityGroup,
    DeleteClusterSecurityGroupResponse (DeleteClusterSecurityGroupResponse'),
    newDeleteClusterSecurityGroupResponse,

    -- ** DeleteClusterSnapshot
    DeleteClusterSnapshot (DeleteClusterSnapshot'),
    newDeleteClusterSnapshot,
    DeleteClusterSnapshotResponse (DeleteClusterSnapshotResponse'),
    newDeleteClusterSnapshotResponse,

    -- ** DeleteClusterSubnetGroup
    DeleteClusterSubnetGroup (DeleteClusterSubnetGroup'),
    newDeleteClusterSubnetGroup,
    DeleteClusterSubnetGroupResponse (DeleteClusterSubnetGroupResponse'),
    newDeleteClusterSubnetGroupResponse,

    -- ** DeleteEndpointAccess
    DeleteEndpointAccess (DeleteEndpointAccess'),
    newDeleteEndpointAccess,
    EndpointAccess (EndpointAccess'),
    newEndpointAccess,

    -- ** DeleteEventSubscription
    DeleteEventSubscription (DeleteEventSubscription'),
    newDeleteEventSubscription,
    DeleteEventSubscriptionResponse (DeleteEventSubscriptionResponse'),
    newDeleteEventSubscriptionResponse,

    -- ** DeleteHsmClientCertificate
    DeleteHsmClientCertificate (DeleteHsmClientCertificate'),
    newDeleteHsmClientCertificate,
    DeleteHsmClientCertificateResponse (DeleteHsmClientCertificateResponse'),
    newDeleteHsmClientCertificateResponse,

    -- ** DeleteHsmConfiguration
    DeleteHsmConfiguration (DeleteHsmConfiguration'),
    newDeleteHsmConfiguration,
    DeleteHsmConfigurationResponse (DeleteHsmConfigurationResponse'),
    newDeleteHsmConfigurationResponse,

    -- ** DeletePartner
    DeletePartner (DeletePartner'),
    newDeletePartner,
    PartnerIntegrationOutputMessage (PartnerIntegrationOutputMessage'),
    newPartnerIntegrationOutputMessage,

    -- ** DeleteScheduledAction
    DeleteScheduledAction (DeleteScheduledAction'),
    newDeleteScheduledAction,
    DeleteScheduledActionResponse (DeleteScheduledActionResponse'),
    newDeleteScheduledActionResponse,

    -- ** DeleteSnapshotCopyGrant
    DeleteSnapshotCopyGrant (DeleteSnapshotCopyGrant'),
    newDeleteSnapshotCopyGrant,
    DeleteSnapshotCopyGrantResponse (DeleteSnapshotCopyGrantResponse'),
    newDeleteSnapshotCopyGrantResponse,

    -- ** DeleteSnapshotSchedule
    DeleteSnapshotSchedule (DeleteSnapshotSchedule'),
    newDeleteSnapshotSchedule,
    DeleteSnapshotScheduleResponse (DeleteSnapshotScheduleResponse'),
    newDeleteSnapshotScheduleResponse,

    -- ** DeleteTags
    DeleteTags (DeleteTags'),
    newDeleteTags,
    DeleteTagsResponse (DeleteTagsResponse'),
    newDeleteTagsResponse,

    -- ** DeleteUsageLimit
    DeleteUsageLimit (DeleteUsageLimit'),
    newDeleteUsageLimit,
    DeleteUsageLimitResponse (DeleteUsageLimitResponse'),
    newDeleteUsageLimitResponse,

    -- ** DescribeAccountAttributes
    DescribeAccountAttributes (DescribeAccountAttributes'),
    newDescribeAccountAttributes,
    DescribeAccountAttributesResponse (DescribeAccountAttributesResponse'),
    newDescribeAccountAttributesResponse,

    -- ** DescribeAuthenticationProfiles
    DescribeAuthenticationProfiles (DescribeAuthenticationProfiles'),
    newDescribeAuthenticationProfiles,
    DescribeAuthenticationProfilesResponse (DescribeAuthenticationProfilesResponse'),
    newDescribeAuthenticationProfilesResponse,

    -- ** DescribeClusterDbRevisions (Paginated)
    DescribeClusterDbRevisions (DescribeClusterDbRevisions'),
    newDescribeClusterDbRevisions,
    DescribeClusterDbRevisionsResponse (DescribeClusterDbRevisionsResponse'),
    newDescribeClusterDbRevisionsResponse,

    -- ** DescribeClusterParameterGroups (Paginated)
    DescribeClusterParameterGroups (DescribeClusterParameterGroups'),
    newDescribeClusterParameterGroups,
    DescribeClusterParameterGroupsResponse (DescribeClusterParameterGroupsResponse'),
    newDescribeClusterParameterGroupsResponse,

    -- ** DescribeClusterParameters (Paginated)
    DescribeClusterParameters (DescribeClusterParameters'),
    newDescribeClusterParameters,
    DescribeClusterParametersResponse (DescribeClusterParametersResponse'),
    newDescribeClusterParametersResponse,

    -- ** DescribeClusterSecurityGroups (Paginated)
    DescribeClusterSecurityGroups (DescribeClusterSecurityGroups'),
    newDescribeClusterSecurityGroups,
    DescribeClusterSecurityGroupsResponse (DescribeClusterSecurityGroupsResponse'),
    newDescribeClusterSecurityGroupsResponse,

    -- ** DescribeClusterSnapshots (Paginated)
    DescribeClusterSnapshots (DescribeClusterSnapshots'),
    newDescribeClusterSnapshots,
    DescribeClusterSnapshotsResponse (DescribeClusterSnapshotsResponse'),
    newDescribeClusterSnapshotsResponse,

    -- ** DescribeClusterSubnetGroups (Paginated)
    DescribeClusterSubnetGroups (DescribeClusterSubnetGroups'),
    newDescribeClusterSubnetGroups,
    DescribeClusterSubnetGroupsResponse (DescribeClusterSubnetGroupsResponse'),
    newDescribeClusterSubnetGroupsResponse,

    -- ** DescribeClusterTracks (Paginated)
    DescribeClusterTracks (DescribeClusterTracks'),
    newDescribeClusterTracks,
    DescribeClusterTracksResponse (DescribeClusterTracksResponse'),
    newDescribeClusterTracksResponse,

    -- ** DescribeClusterVersions (Paginated)
    DescribeClusterVersions (DescribeClusterVersions'),
    newDescribeClusterVersions,
    DescribeClusterVersionsResponse (DescribeClusterVersionsResponse'),
    newDescribeClusterVersionsResponse,

    -- ** DescribeClusters (Paginated)
    DescribeClusters (DescribeClusters'),
    newDescribeClusters,
    DescribeClustersResponse (DescribeClustersResponse'),
    newDescribeClustersResponse,

    -- ** DescribeDataShares (Paginated)
    DescribeDataShares (DescribeDataShares'),
    newDescribeDataShares,
    DescribeDataSharesResponse (DescribeDataSharesResponse'),
    newDescribeDataSharesResponse,

    -- ** DescribeDataSharesForConsumer (Paginated)
    DescribeDataSharesForConsumer (DescribeDataSharesForConsumer'),
    newDescribeDataSharesForConsumer,
    DescribeDataSharesForConsumerResponse (DescribeDataSharesForConsumerResponse'),
    newDescribeDataSharesForConsumerResponse,

    -- ** DescribeDataSharesForProducer (Paginated)
    DescribeDataSharesForProducer (DescribeDataSharesForProducer'),
    newDescribeDataSharesForProducer,
    DescribeDataSharesForProducerResponse (DescribeDataSharesForProducerResponse'),
    newDescribeDataSharesForProducerResponse,

    -- ** DescribeDefaultClusterParameters (Paginated)
    DescribeDefaultClusterParameters (DescribeDefaultClusterParameters'),
    newDescribeDefaultClusterParameters,
    DescribeDefaultClusterParametersResponse (DescribeDefaultClusterParametersResponse'),
    newDescribeDefaultClusterParametersResponse,

    -- ** DescribeEndpointAccess (Paginated)
    DescribeEndpointAccess (DescribeEndpointAccess'),
    newDescribeEndpointAccess,
    DescribeEndpointAccessResponse (DescribeEndpointAccessResponse'),
    newDescribeEndpointAccessResponse,

    -- ** DescribeEndpointAuthorization (Paginated)
    DescribeEndpointAuthorization (DescribeEndpointAuthorization'),
    newDescribeEndpointAuthorization,
    DescribeEndpointAuthorizationResponse (DescribeEndpointAuthorizationResponse'),
    newDescribeEndpointAuthorizationResponse,

    -- ** DescribeEventCategories
    DescribeEventCategories (DescribeEventCategories'),
    newDescribeEventCategories,
    DescribeEventCategoriesResponse (DescribeEventCategoriesResponse'),
    newDescribeEventCategoriesResponse,

    -- ** DescribeEventSubscriptions (Paginated)
    DescribeEventSubscriptions (DescribeEventSubscriptions'),
    newDescribeEventSubscriptions,
    DescribeEventSubscriptionsResponse (DescribeEventSubscriptionsResponse'),
    newDescribeEventSubscriptionsResponse,

    -- ** DescribeEvents (Paginated)
    DescribeEvents (DescribeEvents'),
    newDescribeEvents,
    DescribeEventsResponse (DescribeEventsResponse'),
    newDescribeEventsResponse,

    -- ** DescribeHsmClientCertificates (Paginated)
    DescribeHsmClientCertificates (DescribeHsmClientCertificates'),
    newDescribeHsmClientCertificates,
    DescribeHsmClientCertificatesResponse (DescribeHsmClientCertificatesResponse'),
    newDescribeHsmClientCertificatesResponse,

    -- ** DescribeHsmConfigurations (Paginated)
    DescribeHsmConfigurations (DescribeHsmConfigurations'),
    newDescribeHsmConfigurations,
    DescribeHsmConfigurationsResponse (DescribeHsmConfigurationsResponse'),
    newDescribeHsmConfigurationsResponse,

    -- ** DescribeLoggingStatus
    DescribeLoggingStatus (DescribeLoggingStatus'),
    newDescribeLoggingStatus,
    LoggingStatus (LoggingStatus'),
    newLoggingStatus,

    -- ** DescribeNodeConfigurationOptions (Paginated)
    DescribeNodeConfigurationOptions (DescribeNodeConfigurationOptions'),
    newDescribeNodeConfigurationOptions,
    DescribeNodeConfigurationOptionsResponse (DescribeNodeConfigurationOptionsResponse'),
    newDescribeNodeConfigurationOptionsResponse,

    -- ** DescribeOrderableClusterOptions (Paginated)
    DescribeOrderableClusterOptions (DescribeOrderableClusterOptions'),
    newDescribeOrderableClusterOptions,
    DescribeOrderableClusterOptionsResponse (DescribeOrderableClusterOptionsResponse'),
    newDescribeOrderableClusterOptionsResponse,

    -- ** DescribePartners
    DescribePartners (DescribePartners'),
    newDescribePartners,
    DescribePartnersResponse (DescribePartnersResponse'),
    newDescribePartnersResponse,

    -- ** DescribeReservedNodeExchangeStatus (Paginated)
    DescribeReservedNodeExchangeStatus (DescribeReservedNodeExchangeStatus'),
    newDescribeReservedNodeExchangeStatus,
    DescribeReservedNodeExchangeStatusResponse (DescribeReservedNodeExchangeStatusResponse'),
    newDescribeReservedNodeExchangeStatusResponse,

    -- ** DescribeReservedNodeOfferings (Paginated)
    DescribeReservedNodeOfferings (DescribeReservedNodeOfferings'),
    newDescribeReservedNodeOfferings,
    DescribeReservedNodeOfferingsResponse (DescribeReservedNodeOfferingsResponse'),
    newDescribeReservedNodeOfferingsResponse,

    -- ** DescribeReservedNodes (Paginated)
    DescribeReservedNodes (DescribeReservedNodes'),
    newDescribeReservedNodes,
    DescribeReservedNodesResponse (DescribeReservedNodesResponse'),
    newDescribeReservedNodesResponse,

    -- ** DescribeResize
    DescribeResize (DescribeResize'),
    newDescribeResize,
    ResizeProgressMessage (ResizeProgressMessage'),
    newResizeProgressMessage,

    -- ** DescribeScheduledActions (Paginated)
    DescribeScheduledActions (DescribeScheduledActions'),
    newDescribeScheduledActions,
    DescribeScheduledActionsResponse (DescribeScheduledActionsResponse'),
    newDescribeScheduledActionsResponse,

    -- ** DescribeSnapshotCopyGrants (Paginated)
    DescribeSnapshotCopyGrants (DescribeSnapshotCopyGrants'),
    newDescribeSnapshotCopyGrants,
    DescribeSnapshotCopyGrantsResponse (DescribeSnapshotCopyGrantsResponse'),
    newDescribeSnapshotCopyGrantsResponse,

    -- ** DescribeSnapshotSchedules (Paginated)
    DescribeSnapshotSchedules (DescribeSnapshotSchedules'),
    newDescribeSnapshotSchedules,
    DescribeSnapshotSchedulesResponse (DescribeSnapshotSchedulesResponse'),
    newDescribeSnapshotSchedulesResponse,

    -- ** DescribeStorage
    DescribeStorage (DescribeStorage'),
    newDescribeStorage,
    DescribeStorageResponse (DescribeStorageResponse'),
    newDescribeStorageResponse,

    -- ** DescribeTableRestoreStatus (Paginated)
    DescribeTableRestoreStatus (DescribeTableRestoreStatus'),
    newDescribeTableRestoreStatus,
    DescribeTableRestoreStatusResponse (DescribeTableRestoreStatusResponse'),
    newDescribeTableRestoreStatusResponse,

    -- ** DescribeTags (Paginated)
    DescribeTags (DescribeTags'),
    newDescribeTags,
    DescribeTagsResponse (DescribeTagsResponse'),
    newDescribeTagsResponse,

    -- ** DescribeUsageLimits (Paginated)
    DescribeUsageLimits (DescribeUsageLimits'),
    newDescribeUsageLimits,
    DescribeUsageLimitsResponse (DescribeUsageLimitsResponse'),
    newDescribeUsageLimitsResponse,

    -- ** DisableLogging
    DisableLogging (DisableLogging'),
    newDisableLogging,
    LoggingStatus (LoggingStatus'),
    newLoggingStatus,

    -- ** DisableSnapshotCopy
    DisableSnapshotCopy (DisableSnapshotCopy'),
    newDisableSnapshotCopy,
    DisableSnapshotCopyResponse (DisableSnapshotCopyResponse'),
    newDisableSnapshotCopyResponse,

    -- ** DisassociateDataShareConsumer
    DisassociateDataShareConsumer (DisassociateDataShareConsumer'),
    newDisassociateDataShareConsumer,
    DataShare (DataShare'),
    newDataShare,

    -- ** EnableLogging
    EnableLogging (EnableLogging'),
    newEnableLogging,
    LoggingStatus (LoggingStatus'),
    newLoggingStatus,

    -- ** EnableSnapshotCopy
    EnableSnapshotCopy (EnableSnapshotCopy'),
    newEnableSnapshotCopy,
    EnableSnapshotCopyResponse (EnableSnapshotCopyResponse'),
    newEnableSnapshotCopyResponse,

    -- ** GetClusterCredentials
    GetClusterCredentials (GetClusterCredentials'),
    newGetClusterCredentials,
    GetClusterCredentialsResponse (GetClusterCredentialsResponse'),
    newGetClusterCredentialsResponse,

    -- ** GetClusterCredentialsWithIAM
    GetClusterCredentialsWithIAM (GetClusterCredentialsWithIAM'),
    newGetClusterCredentialsWithIAM,
    GetClusterCredentialsWithIAMResponse (GetClusterCredentialsWithIAMResponse'),
    newGetClusterCredentialsWithIAMResponse,

    -- ** GetReservedNodeExchangeConfigurationOptions (Paginated)
    GetReservedNodeExchangeConfigurationOptions (GetReservedNodeExchangeConfigurationOptions'),
    newGetReservedNodeExchangeConfigurationOptions,
    GetReservedNodeExchangeConfigurationOptionsResponse (GetReservedNodeExchangeConfigurationOptionsResponse'),
    newGetReservedNodeExchangeConfigurationOptionsResponse,

    -- ** GetReservedNodeExchangeOfferings (Paginated)
    GetReservedNodeExchangeOfferings (GetReservedNodeExchangeOfferings'),
    newGetReservedNodeExchangeOfferings,
    GetReservedNodeExchangeOfferingsResponse (GetReservedNodeExchangeOfferingsResponse'),
    newGetReservedNodeExchangeOfferingsResponse,

    -- ** ModifyAquaConfiguration
    ModifyAquaConfiguration (ModifyAquaConfiguration'),
    newModifyAquaConfiguration,
    ModifyAquaConfigurationResponse (ModifyAquaConfigurationResponse'),
    newModifyAquaConfigurationResponse,

    -- ** ModifyAuthenticationProfile
    ModifyAuthenticationProfile (ModifyAuthenticationProfile'),
    newModifyAuthenticationProfile,
    ModifyAuthenticationProfileResponse (ModifyAuthenticationProfileResponse'),
    newModifyAuthenticationProfileResponse,

    -- ** ModifyCluster
    ModifyCluster (ModifyCluster'),
    newModifyCluster,
    ModifyClusterResponse (ModifyClusterResponse'),
    newModifyClusterResponse,

    -- ** ModifyClusterDbRevision
    ModifyClusterDbRevision (ModifyClusterDbRevision'),
    newModifyClusterDbRevision,
    ModifyClusterDbRevisionResponse (ModifyClusterDbRevisionResponse'),
    newModifyClusterDbRevisionResponse,

    -- ** ModifyClusterIamRoles
    ModifyClusterIamRoles (ModifyClusterIamRoles'),
    newModifyClusterIamRoles,
    ModifyClusterIamRolesResponse (ModifyClusterIamRolesResponse'),
    newModifyClusterIamRolesResponse,

    -- ** ModifyClusterMaintenance
    ModifyClusterMaintenance (ModifyClusterMaintenance'),
    newModifyClusterMaintenance,
    ModifyClusterMaintenanceResponse (ModifyClusterMaintenanceResponse'),
    newModifyClusterMaintenanceResponse,

    -- ** ModifyClusterParameterGroup
    ModifyClusterParameterGroup (ModifyClusterParameterGroup'),
    newModifyClusterParameterGroup,
    ClusterParameterGroupNameMessage (ClusterParameterGroupNameMessage'),
    newClusterParameterGroupNameMessage,

    -- ** ModifyClusterSnapshot
    ModifyClusterSnapshot (ModifyClusterSnapshot'),
    newModifyClusterSnapshot,
    ModifyClusterSnapshotResponse (ModifyClusterSnapshotResponse'),
    newModifyClusterSnapshotResponse,

    -- ** ModifyClusterSnapshotSchedule
    ModifyClusterSnapshotSchedule (ModifyClusterSnapshotSchedule'),
    newModifyClusterSnapshotSchedule,
    ModifyClusterSnapshotScheduleResponse (ModifyClusterSnapshotScheduleResponse'),
    newModifyClusterSnapshotScheduleResponse,

    -- ** ModifyClusterSubnetGroup
    ModifyClusterSubnetGroup (ModifyClusterSubnetGroup'),
    newModifyClusterSubnetGroup,
    ModifyClusterSubnetGroupResponse (ModifyClusterSubnetGroupResponse'),
    newModifyClusterSubnetGroupResponse,

    -- ** ModifyEndpointAccess
    ModifyEndpointAccess (ModifyEndpointAccess'),
    newModifyEndpointAccess,
    EndpointAccess (EndpointAccess'),
    newEndpointAccess,

    -- ** ModifyEventSubscription
    ModifyEventSubscription (ModifyEventSubscription'),
    newModifyEventSubscription,
    ModifyEventSubscriptionResponse (ModifyEventSubscriptionResponse'),
    newModifyEventSubscriptionResponse,

    -- ** ModifyScheduledAction
    ModifyScheduledAction (ModifyScheduledAction'),
    newModifyScheduledAction,
    ScheduledAction (ScheduledAction'),
    newScheduledAction,

    -- ** ModifySnapshotCopyRetentionPeriod
    ModifySnapshotCopyRetentionPeriod (ModifySnapshotCopyRetentionPeriod'),
    newModifySnapshotCopyRetentionPeriod,
    ModifySnapshotCopyRetentionPeriodResponse (ModifySnapshotCopyRetentionPeriodResponse'),
    newModifySnapshotCopyRetentionPeriodResponse,

    -- ** ModifySnapshotSchedule
    ModifySnapshotSchedule (ModifySnapshotSchedule'),
    newModifySnapshotSchedule,
    SnapshotSchedule (SnapshotSchedule'),
    newSnapshotSchedule,

    -- ** ModifyUsageLimit
    ModifyUsageLimit (ModifyUsageLimit'),
    newModifyUsageLimit,
    UsageLimit (UsageLimit'),
    newUsageLimit,

    -- ** PauseCluster
    PauseCluster (PauseCluster'),
    newPauseCluster,
    PauseClusterResponse (PauseClusterResponse'),
    newPauseClusterResponse,

    -- ** PurchaseReservedNodeOffering
    PurchaseReservedNodeOffering (PurchaseReservedNodeOffering'),
    newPurchaseReservedNodeOffering,
    PurchaseReservedNodeOfferingResponse (PurchaseReservedNodeOfferingResponse'),
    newPurchaseReservedNodeOfferingResponse,

    -- ** RebootCluster
    RebootCluster (RebootCluster'),
    newRebootCluster,
    RebootClusterResponse (RebootClusterResponse'),
    newRebootClusterResponse,

    -- ** RejectDataShare
    RejectDataShare (RejectDataShare'),
    newRejectDataShare,
    DataShare (DataShare'),
    newDataShare,

    -- ** ResetClusterParameterGroup
    ResetClusterParameterGroup (ResetClusterParameterGroup'),
    newResetClusterParameterGroup,
    ClusterParameterGroupNameMessage (ClusterParameterGroupNameMessage'),
    newClusterParameterGroupNameMessage,

    -- ** ResizeCluster
    ResizeCluster (ResizeCluster'),
    newResizeCluster,
    ResizeClusterResponse (ResizeClusterResponse'),
    newResizeClusterResponse,

    -- ** RestoreFromClusterSnapshot
    RestoreFromClusterSnapshot (RestoreFromClusterSnapshot'),
    newRestoreFromClusterSnapshot,
    RestoreFromClusterSnapshotResponse (RestoreFromClusterSnapshotResponse'),
    newRestoreFromClusterSnapshotResponse,

    -- ** RestoreTableFromClusterSnapshot
    RestoreTableFromClusterSnapshot (RestoreTableFromClusterSnapshot'),
    newRestoreTableFromClusterSnapshot,
    RestoreTableFromClusterSnapshotResponse (RestoreTableFromClusterSnapshotResponse'),
    newRestoreTableFromClusterSnapshotResponse,

    -- ** ResumeCluster
    ResumeCluster (ResumeCluster'),
    newResumeCluster,
    ResumeClusterResponse (ResumeClusterResponse'),
    newResumeClusterResponse,

    -- ** RevokeClusterSecurityGroupIngress
    RevokeClusterSecurityGroupIngress (RevokeClusterSecurityGroupIngress'),
    newRevokeClusterSecurityGroupIngress,
    RevokeClusterSecurityGroupIngressResponse (RevokeClusterSecurityGroupIngressResponse'),
    newRevokeClusterSecurityGroupIngressResponse,

    -- ** RevokeEndpointAccess
    RevokeEndpointAccess (RevokeEndpointAccess'),
    newRevokeEndpointAccess,
    EndpointAuthorization (EndpointAuthorization'),
    newEndpointAuthorization,

    -- ** RevokeSnapshotAccess
    RevokeSnapshotAccess (RevokeSnapshotAccess'),
    newRevokeSnapshotAccess,
    RevokeSnapshotAccessResponse (RevokeSnapshotAccessResponse'),
    newRevokeSnapshotAccessResponse,

    -- ** RotateEncryptionKey
    RotateEncryptionKey (RotateEncryptionKey'),
    newRotateEncryptionKey,
    RotateEncryptionKeyResponse (RotateEncryptionKeyResponse'),
    newRotateEncryptionKeyResponse,

    -- ** UpdatePartnerStatus
    UpdatePartnerStatus (UpdatePartnerStatus'),
    newUpdatePartnerStatus,
    PartnerIntegrationOutputMessage (PartnerIntegrationOutputMessage'),
    newPartnerIntegrationOutputMessage,

    -- * Types

    -- ** Common
    module Amazonka.Redshift.Internal,

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

    -- ** LogDestinationType
    LogDestinationType (..),

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

    -- ** ReservedNodeExchangeActionType
    ReservedNodeExchangeActionType (..),

    -- ** ReservedNodeExchangeStatusType
    ReservedNodeExchangeStatusType (..),

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

    -- ** ReservedNodeConfigurationOption
    ReservedNodeConfigurationOption (ReservedNodeConfigurationOption'),
    newReservedNodeConfigurationOption,

    -- ** ReservedNodeExchangeStatus
    ReservedNodeExchangeStatus (ReservedNodeExchangeStatus'),
    newReservedNodeExchangeStatus,

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

import Amazonka.Redshift.AcceptReservedNodeExchange
import Amazonka.Redshift.AddPartner
import Amazonka.Redshift.AssociateDataShareConsumer
import Amazonka.Redshift.AuthorizeClusterSecurityGroupIngress
import Amazonka.Redshift.AuthorizeDataShare
import Amazonka.Redshift.AuthorizeEndpointAccess
import Amazonka.Redshift.AuthorizeSnapshotAccess
import Amazonka.Redshift.BatchDeleteClusterSnapshots
import Amazonka.Redshift.BatchModifyClusterSnapshots
import Amazonka.Redshift.CancelResize
import Amazonka.Redshift.CopyClusterSnapshot
import Amazonka.Redshift.CreateAuthenticationProfile
import Amazonka.Redshift.CreateCluster
import Amazonka.Redshift.CreateClusterParameterGroup
import Amazonka.Redshift.CreateClusterSecurityGroup
import Amazonka.Redshift.CreateClusterSnapshot
import Amazonka.Redshift.CreateClusterSubnetGroup
import Amazonka.Redshift.CreateEndpointAccess
import Amazonka.Redshift.CreateEventSubscription
import Amazonka.Redshift.CreateHsmClientCertificate
import Amazonka.Redshift.CreateHsmConfiguration
import Amazonka.Redshift.CreateScheduledAction
import Amazonka.Redshift.CreateSnapshotCopyGrant
import Amazonka.Redshift.CreateSnapshotSchedule
import Amazonka.Redshift.CreateTags
import Amazonka.Redshift.CreateUsageLimit
import Amazonka.Redshift.DeauthorizeDataShare
import Amazonka.Redshift.DeleteAuthenticationProfile
import Amazonka.Redshift.DeleteCluster
import Amazonka.Redshift.DeleteClusterParameterGroup
import Amazonka.Redshift.DeleteClusterSecurityGroup
import Amazonka.Redshift.DeleteClusterSnapshot
import Amazonka.Redshift.DeleteClusterSubnetGroup
import Amazonka.Redshift.DeleteEndpointAccess
import Amazonka.Redshift.DeleteEventSubscription
import Amazonka.Redshift.DeleteHsmClientCertificate
import Amazonka.Redshift.DeleteHsmConfiguration
import Amazonka.Redshift.DeletePartner
import Amazonka.Redshift.DeleteScheduledAction
import Amazonka.Redshift.DeleteSnapshotCopyGrant
import Amazonka.Redshift.DeleteSnapshotSchedule
import Amazonka.Redshift.DeleteTags
import Amazonka.Redshift.DeleteUsageLimit
import Amazonka.Redshift.DescribeAccountAttributes
import Amazonka.Redshift.DescribeAuthenticationProfiles
import Amazonka.Redshift.DescribeClusterDbRevisions
import Amazonka.Redshift.DescribeClusterParameterGroups
import Amazonka.Redshift.DescribeClusterParameters
import Amazonka.Redshift.DescribeClusterSecurityGroups
import Amazonka.Redshift.DescribeClusterSnapshots
import Amazonka.Redshift.DescribeClusterSubnetGroups
import Amazonka.Redshift.DescribeClusterTracks
import Amazonka.Redshift.DescribeClusterVersions
import Amazonka.Redshift.DescribeClusters
import Amazonka.Redshift.DescribeDataShares
import Amazonka.Redshift.DescribeDataSharesForConsumer
import Amazonka.Redshift.DescribeDataSharesForProducer
import Amazonka.Redshift.DescribeDefaultClusterParameters
import Amazonka.Redshift.DescribeEndpointAccess
import Amazonka.Redshift.DescribeEndpointAuthorization
import Amazonka.Redshift.DescribeEventCategories
import Amazonka.Redshift.DescribeEventSubscriptions
import Amazonka.Redshift.DescribeEvents
import Amazonka.Redshift.DescribeHsmClientCertificates
import Amazonka.Redshift.DescribeHsmConfigurations
import Amazonka.Redshift.DescribeLoggingStatus
import Amazonka.Redshift.DescribeNodeConfigurationOptions
import Amazonka.Redshift.DescribeOrderableClusterOptions
import Amazonka.Redshift.DescribePartners
import Amazonka.Redshift.DescribeReservedNodeExchangeStatus
import Amazonka.Redshift.DescribeReservedNodeOfferings
import Amazonka.Redshift.DescribeReservedNodes
import Amazonka.Redshift.DescribeResize
import Amazonka.Redshift.DescribeScheduledActions
import Amazonka.Redshift.DescribeSnapshotCopyGrants
import Amazonka.Redshift.DescribeSnapshotSchedules
import Amazonka.Redshift.DescribeStorage
import Amazonka.Redshift.DescribeTableRestoreStatus
import Amazonka.Redshift.DescribeTags
import Amazonka.Redshift.DescribeUsageLimits
import Amazonka.Redshift.DisableLogging
import Amazonka.Redshift.DisableSnapshotCopy
import Amazonka.Redshift.DisassociateDataShareConsumer
import Amazonka.Redshift.EnableLogging
import Amazonka.Redshift.EnableSnapshotCopy
import Amazonka.Redshift.GetClusterCredentials
import Amazonka.Redshift.GetClusterCredentialsWithIAM
import Amazonka.Redshift.GetReservedNodeExchangeConfigurationOptions
import Amazonka.Redshift.GetReservedNodeExchangeOfferings
import Amazonka.Redshift.Internal
import Amazonka.Redshift.Lens
import Amazonka.Redshift.ModifyAquaConfiguration
import Amazonka.Redshift.ModifyAuthenticationProfile
import Amazonka.Redshift.ModifyCluster
import Amazonka.Redshift.ModifyClusterDbRevision
import Amazonka.Redshift.ModifyClusterIamRoles
import Amazonka.Redshift.ModifyClusterMaintenance
import Amazonka.Redshift.ModifyClusterParameterGroup
import Amazonka.Redshift.ModifyClusterSnapshot
import Amazonka.Redshift.ModifyClusterSnapshotSchedule
import Amazonka.Redshift.ModifyClusterSubnetGroup
import Amazonka.Redshift.ModifyEndpointAccess
import Amazonka.Redshift.ModifyEventSubscription
import Amazonka.Redshift.ModifyScheduledAction
import Amazonka.Redshift.ModifySnapshotCopyRetentionPeriod
import Amazonka.Redshift.ModifySnapshotSchedule
import Amazonka.Redshift.ModifyUsageLimit
import Amazonka.Redshift.PauseCluster
import Amazonka.Redshift.PurchaseReservedNodeOffering
import Amazonka.Redshift.RebootCluster
import Amazonka.Redshift.RejectDataShare
import Amazonka.Redshift.ResetClusterParameterGroup
import Amazonka.Redshift.ResizeCluster
import Amazonka.Redshift.RestoreFromClusterSnapshot
import Amazonka.Redshift.RestoreTableFromClusterSnapshot
import Amazonka.Redshift.ResumeCluster
import Amazonka.Redshift.RevokeClusterSecurityGroupIngress
import Amazonka.Redshift.RevokeEndpointAccess
import Amazonka.Redshift.RevokeSnapshotAccess
import Amazonka.Redshift.RotateEncryptionKey
import Amazonka.Redshift.Types
import Amazonka.Redshift.UpdatePartnerStatus
import Amazonka.Redshift.Waiters

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
