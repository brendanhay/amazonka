{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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

    -- ** ClusterSecurityGroupAlreadyExistsFault
    _ClusterSecurityGroupAlreadyExistsFault,

    -- ** ClusterSnapshotNotFoundFault
    _ClusterSnapshotNotFoundFault,

    -- ** ScheduleDefinitionTypeUnsupportedFault
    _ScheduleDefinitionTypeUnsupportedFault,

    -- ** ClusterSubnetGroupNotFoundFault
    _ClusterSubnetGroupNotFoundFault,

    -- ** InvalidVPCNetworkStateFault
    _InvalidVPCNetworkStateFault,

    -- ** InvalidSubscriptionStateFault
    _InvalidSubscriptionStateFault,

    -- ** BucketNotFoundFault
    _BucketNotFoundFault,

    -- ** LimitExceededFault
    _LimitExceededFault,

    -- ** SubscriptionAlreadyExistFault
    _SubscriptionAlreadyExistFault,

    -- ** ClusterSecurityGroupQuotaExceededFault
    _ClusterSecurityGroupQuotaExceededFault,

    -- ** InvalidScheduledActionFault
    _InvalidScheduledActionFault,

    -- ** SubscriptionSeverityNotFoundFault
    _SubscriptionSeverityNotFoundFault,

    -- ** UnauthorizedOperation
    _UnauthorizedOperation,

    -- ** InsufficientS3BucketPolicyFault
    _InsufficientS3BucketPolicyFault,

    -- ** InsufficientClusterCapacityFault
    _InsufficientClusterCapacityFault,

    -- ** HsmConfigurationQuotaExceededFault
    _HsmConfigurationQuotaExceededFault,

    -- ** UsageLimitNotFoundFault
    _UsageLimitNotFoundFault,

    -- ** InvalidTagFault
    _InvalidTagFault,

    -- ** SnapshotScheduleQuotaExceededFault
    _SnapshotScheduleQuotaExceededFault,

    -- ** SNSNoAuthorizationFault
    _SNSNoAuthorizationFault,

    -- ** ScheduledActionNotFoundFault
    _ScheduledActionNotFoundFault,

    -- ** HsmClientCertificateAlreadyExistsFault
    _HsmClientCertificateAlreadyExistsFault,

    -- ** ResourceNotFoundFault
    _ResourceNotFoundFault,

    -- ** SNSTopicArnNotFoundFault
    _SNSTopicArnNotFoundFault,

    -- ** InvalidRestoreFault
    _InvalidRestoreFault,

    -- ** ScheduledActionAlreadyExistsFault
    _ScheduledActionAlreadyExistsFault,

    -- ** ClusterNotFoundFault
    _ClusterNotFoundFault,

    -- ** ScheduledActionQuotaExceededFault
    _ScheduledActionQuotaExceededFault,

    -- ** ScheduledActionTypeUnsupportedFault
    _ScheduledActionTypeUnsupportedFault,

    -- ** EventSubscriptionQuotaExceededFault
    _EventSubscriptionQuotaExceededFault,

    -- ** SnapshotScheduleNotFoundFault
    _SnapshotScheduleNotFoundFault,

    -- ** InvalidReservedNodeStateFault
    _InvalidReservedNodeStateFault,

    -- ** SnapshotScheduleUpdateInProgressFault
    _SnapshotScheduleUpdateInProgressFault,

    -- ** BatchModifyClusterSnapshotsLimitExceededFault
    _BatchModifyClusterSnapshotsLimitExceededFault,

    -- ** ReservedNodeAlreadyMigratedFault
    _ReservedNodeAlreadyMigratedFault,

    -- ** IncompatibleOrderableOptions
    _IncompatibleOrderableOptions,

    -- ** InvalidClusterSubnetStateFault
    _InvalidClusterSubnetStateFault,

    -- ** ClusterQuotaExceededFault
    _ClusterQuotaExceededFault,

    -- ** InvalidS3BucketNameFault
    _InvalidS3BucketNameFault,

    -- ** InvalidClusterSubnetGroupStateFault
    _InvalidClusterSubnetGroupStateFault,

    -- ** HsmConfigurationAlreadyExistsFault
    _HsmConfigurationAlreadyExistsFault,

    -- ** ClusterSecurityGroupNotFoundFault
    _ClusterSecurityGroupNotFoundFault,

    -- ** UnsupportedOperationFault
    _UnsupportedOperationFault,

    -- ** InvalidElasticIpFault
    _InvalidElasticIpFault,

    -- ** InvalidClusterSnapshotScheduleStateFault
    _InvalidClusterSnapshotScheduleStateFault,

    -- ** TableRestoreNotFoundFault
    _TableRestoreNotFoundFault,

    -- ** HsmConfigurationNotFoundFault
    _HsmConfigurationNotFoundFault,

    -- ** AuthorizationAlreadyExistsFault
    _AuthorizationAlreadyExistsFault,

    -- ** InvalidClusterSecurityGroupStateFault
    _InvalidClusterSecurityGroupStateFault,

    -- ** UsageLimitAlreadyExistsFault
    _UsageLimitAlreadyExistsFault,

    -- ** SNSInvalidTopicFault
    _SNSInvalidTopicFault,

    -- ** SourceNotFoundFault
    _SourceNotFoundFault,

    -- ** InvalidS3KeyPrefixFault
    _InvalidS3KeyPrefixFault,

    -- ** UnsupportedOptionFault
    _UnsupportedOptionFault,

    -- ** AuthorizationQuotaExceededFault
    _AuthorizationQuotaExceededFault,

    -- ** DependentServiceUnavailableFault
    _DependentServiceUnavailableFault,

    -- ** CopyToRegionDisabledFault
    _CopyToRegionDisabledFault,

    -- ** ClusterOnLatestRevisionFault
    _ClusterOnLatestRevisionFault,

    -- ** SubnetAlreadyInUse
    _SubnetAlreadyInUse,

    -- ** ClusterSubnetGroupQuotaExceededFault
    _ClusterSubnetGroupQuotaExceededFault,

    -- ** ClusterSnapshotQuotaExceededFault
    _ClusterSnapshotQuotaExceededFault,

    -- ** InvalidScheduleFault
    _InvalidScheduleFault,

    -- ** ClusterSubnetQuotaExceededFault
    _ClusterSubnetQuotaExceededFault,

    -- ** AccessToSnapshotDeniedFault
    _AccessToSnapshotDeniedFault,

    -- ** NumberOfNodesQuotaExceededFault
    _NumberOfNodesQuotaExceededFault,

    -- ** InvalidTableRestoreArgumentFault
    _InvalidTableRestoreArgumentFault,

    -- ** InvalidClusterTrackFault
    _InvalidClusterTrackFault,

    -- ** InvalidHsmClientCertificateStateFault
    _InvalidHsmClientCertificateStateFault,

    -- ** TagLimitExceededFault
    _TagLimitExceededFault,

    -- ** SnapshotCopyGrantNotFoundFault
    _SnapshotCopyGrantNotFoundFault,

    -- ** ClusterAlreadyExistsFault
    _ClusterAlreadyExistsFault,

    -- ** InvalidClusterStateFault
    _InvalidClusterStateFault,

    -- ** ClusterParameterGroupQuotaExceededFault
    _ClusterParameterGroupQuotaExceededFault,

    -- ** UnknownSnapshotCopyRegionFault
    _UnknownSnapshotCopyRegionFault,

    -- ** SnapshotCopyDisabledFault
    _SnapshotCopyDisabledFault,

    -- ** InvalidUsageLimitFault
    _InvalidUsageLimitFault,

    -- ** InProgressTableRestoreQuotaExceededFault
    _InProgressTableRestoreQuotaExceededFault,

    -- ** ReservedNodeQuotaExceededFault
    _ReservedNodeQuotaExceededFault,

    -- ** HsmClientCertificateNotFoundFault
    _HsmClientCertificateNotFoundFault,

    -- ** SubscriptionEventIdNotFoundFault
    _SubscriptionEventIdNotFoundFault,

    -- ** InvalidSnapshotCopyGrantStateFault
    _InvalidSnapshotCopyGrantStateFault,

    -- ** SnapshotCopyGrantAlreadyExistsFault
    _SnapshotCopyGrantAlreadyExistsFault,

    -- ** ResizeNotFoundFault
    _ResizeNotFoundFault,

    -- ** SnapshotCopyAlreadyEnabledFault
    _SnapshotCopyAlreadyEnabledFault,

    -- ** BatchDeleteRequestSizeExceededFault
    _BatchDeleteRequestSizeExceededFault,

    -- ** NumberOfNodesPerClusterLimitExceededFault
    _NumberOfNodesPerClusterLimitExceededFault,

    -- ** ReservedNodeAlreadyExistsFault
    _ReservedNodeAlreadyExistsFault,

    -- ** ClusterParameterGroupAlreadyExistsFault
    _ClusterParameterGroupAlreadyExistsFault,

    -- ** SnapshotCopyGrantQuotaExceededFault
    _SnapshotCopyGrantQuotaExceededFault,

    -- ** InvalidClusterParameterGroupStateFault
    _InvalidClusterParameterGroupStateFault,

    -- ** SnapshotCopyAlreadyDisabledFault
    _SnapshotCopyAlreadyDisabledFault,

    -- ** SnapshotScheduleAlreadyExistsFault
    _SnapshotScheduleAlreadyExistsFault,

    -- ** ReservedNodeOfferingNotFoundFault
    _ReservedNodeOfferingNotFoundFault,

    -- ** ReservedNodeNotFoundFault
    _ReservedNodeNotFoundFault,

    -- ** HsmClientCertificateQuotaExceededFault
    _HsmClientCertificateQuotaExceededFault,

    -- ** ClusterParameterGroupNotFoundFault
    _ClusterParameterGroupNotFoundFault,

    -- ** SubscriptionCategoryNotFoundFault
    _SubscriptionCategoryNotFoundFault,

    -- ** AuthorizationNotFoundFault
    _AuthorizationNotFoundFault,

    -- ** TableLimitExceededFault
    _TableLimitExceededFault,

    -- ** InvalidClusterSnapshotStateFault
    _InvalidClusterSnapshotStateFault,

    -- ** InvalidRetentionPeriodFault
    _InvalidRetentionPeriodFault,

    -- ** ClusterSubnetGroupAlreadyExistsFault
    _ClusterSubnetGroupAlreadyExistsFault,

    -- ** InvalidSubnet
    _InvalidSubnet,

    -- ** SubscriptionNotFoundFault
    _SubscriptionNotFoundFault,

    -- ** ClusterSnapshotAlreadyExistsFault
    _ClusterSnapshotAlreadyExistsFault,

    -- ** InvalidHsmConfigurationStateFault
    _InvalidHsmConfigurationStateFault,

    -- ** DependentServiceRequestThrottlingFault
    _DependentServiceRequestThrottlingFault,

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

    -- ** DeleteClusterSubnetGroup
    DeleteClusterSubnetGroup (DeleteClusterSubnetGroup'),
    newDeleteClusterSubnetGroup,
    DeleteClusterSubnetGroupResponse (DeleteClusterSubnetGroupResponse'),
    newDeleteClusterSubnetGroupResponse,

    -- ** DisableLogging
    DisableLogging (DisableLogging'),
    newDisableLogging,
    LoggingStatus (LoggingStatus'),
    newLoggingStatus,

    -- ** DescribeHsmClientCertificates (Paginated)
    DescribeHsmClientCertificates (DescribeHsmClientCertificates'),
    newDescribeHsmClientCertificates,
    DescribeHsmClientCertificatesResponse (DescribeHsmClientCertificatesResponse'),
    newDescribeHsmClientCertificatesResponse,

    -- ** ModifyClusterParameterGroup
    ModifyClusterParameterGroup (ModifyClusterParameterGroup'),
    newModifyClusterParameterGroup,
    ClusterParameterGroupNameMessage (ClusterParameterGroupNameMessage'),
    newClusterParameterGroupNameMessage,

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

    -- ** RevokeClusterSecurityGroupIngress
    RevokeClusterSecurityGroupIngress (RevokeClusterSecurityGroupIngress'),
    newRevokeClusterSecurityGroupIngress,
    RevokeClusterSecurityGroupIngressResponse (RevokeClusterSecurityGroupIngressResponse'),
    newRevokeClusterSecurityGroupIngressResponse,

    -- ** CreateUsageLimit
    CreateUsageLimit (CreateUsageLimit'),
    newCreateUsageLimit,
    UsageLimit (UsageLimit'),
    newUsageLimit,

    -- ** DescribeTags (Paginated)
    DescribeTags (DescribeTags'),
    newDescribeTags,
    DescribeTagsResponse (DescribeTagsResponse'),
    newDescribeTagsResponse,

    -- ** DeleteUsageLimit
    DeleteUsageLimit (DeleteUsageLimit'),
    newDeleteUsageLimit,
    DeleteUsageLimitResponse (DeleteUsageLimitResponse'),
    newDeleteUsageLimitResponse,

    -- ** DescribeScheduledActions (Paginated)
    DescribeScheduledActions (DescribeScheduledActions'),
    newDescribeScheduledActions,
    DescribeScheduledActionsResponse (DescribeScheduledActionsResponse'),
    newDescribeScheduledActionsResponse,

    -- ** RotateEncryptionKey
    RotateEncryptionKey (RotateEncryptionKey'),
    newRotateEncryptionKey,
    RotateEncryptionKeyResponse (RotateEncryptionKeyResponse'),
    newRotateEncryptionKeyResponse,

    -- ** DescribeClusterDbRevisions (Paginated)
    DescribeClusterDbRevisions (DescribeClusterDbRevisions'),
    newDescribeClusterDbRevisions,
    DescribeClusterDbRevisionsResponse (DescribeClusterDbRevisionsResponse'),
    newDescribeClusterDbRevisionsResponse,

    -- ** DescribeEventSubscriptions (Paginated)
    DescribeEventSubscriptions (DescribeEventSubscriptions'),
    newDescribeEventSubscriptions,
    DescribeEventSubscriptionsResponse (DescribeEventSubscriptionsResponse'),
    newDescribeEventSubscriptionsResponse,

    -- ** ResetClusterParameterGroup
    ResetClusterParameterGroup (ResetClusterParameterGroup'),
    newResetClusterParameterGroup,
    ClusterParameterGroupNameMessage (ClusterParameterGroupNameMessage'),
    newClusterParameterGroupNameMessage,

    -- ** ModifyClusterSubnetGroup
    ModifyClusterSubnetGroup (ModifyClusterSubnetGroup'),
    newModifyClusterSubnetGroup,
    ModifyClusterSubnetGroupResponse (ModifyClusterSubnetGroupResponse'),
    newModifyClusterSubnetGroupResponse,

    -- ** RestoreTableFromClusterSnapshot
    RestoreTableFromClusterSnapshot (RestoreTableFromClusterSnapshot'),
    newRestoreTableFromClusterSnapshot,
    RestoreTableFromClusterSnapshotResponse (RestoreTableFromClusterSnapshotResponse'),
    newRestoreTableFromClusterSnapshotResponse,

    -- ** DeleteEventSubscription
    DeleteEventSubscription (DeleteEventSubscription'),
    newDeleteEventSubscription,
    DeleteEventSubscriptionResponse (DeleteEventSubscriptionResponse'),
    newDeleteEventSubscriptionResponse,

    -- ** CreateCluster
    CreateCluster (CreateCluster'),
    newCreateCluster,
    CreateClusterResponse (CreateClusterResponse'),
    newCreateClusterResponse,

    -- ** ModifyClusterSnapshot
    ModifyClusterSnapshot (ModifyClusterSnapshot'),
    newModifyClusterSnapshot,
    ModifyClusterSnapshotResponse (ModifyClusterSnapshotResponse'),
    newModifyClusterSnapshotResponse,

    -- ** DeleteScheduledAction
    DeleteScheduledAction (DeleteScheduledAction'),
    newDeleteScheduledAction,
    DeleteScheduledActionResponse (DeleteScheduledActionResponse'),
    newDeleteScheduledActionResponse,

    -- ** DescribeAccountAttributes
    DescribeAccountAttributes (DescribeAccountAttributes'),
    newDescribeAccountAttributes,
    DescribeAccountAttributesResponse (DescribeAccountAttributesResponse'),
    newDescribeAccountAttributesResponse,

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

    -- ** ModifyClusterSnapshotSchedule
    ModifyClusterSnapshotSchedule (ModifyClusterSnapshotSchedule'),
    newModifyClusterSnapshotSchedule,
    ModifyClusterSnapshotScheduleResponse (ModifyClusterSnapshotScheduleResponse'),
    newModifyClusterSnapshotScheduleResponse,

    -- ** CreateClusterParameterGroup
    CreateClusterParameterGroup (CreateClusterParameterGroup'),
    newCreateClusterParameterGroup,
    CreateClusterParameterGroupResponse (CreateClusterParameterGroupResponse'),
    newCreateClusterParameterGroupResponse,

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

    -- ** DescribeClusterSnapshots (Paginated)
    DescribeClusterSnapshots (DescribeClusterSnapshots'),
    newDescribeClusterSnapshots,
    DescribeClusterSnapshotsResponse (DescribeClusterSnapshotsResponse'),
    newDescribeClusterSnapshotsResponse,

    -- ** DescribeHsmConfigurations (Paginated)
    DescribeHsmConfigurations (DescribeHsmConfigurations'),
    newDescribeHsmConfigurations,
    DescribeHsmConfigurationsResponse (DescribeHsmConfigurationsResponse'),
    newDescribeHsmConfigurationsResponse,

    -- ** ModifyUsageLimit
    ModifyUsageLimit (ModifyUsageLimit'),
    newModifyUsageLimit,
    UsageLimit (UsageLimit'),
    newUsageLimit,

    -- ** DescribeClusterSecurityGroups (Paginated)
    DescribeClusterSecurityGroups (DescribeClusterSecurityGroups'),
    newDescribeClusterSecurityGroups,
    DescribeClusterSecurityGroupsResponse (DescribeClusterSecurityGroupsResponse'),
    newDescribeClusterSecurityGroupsResponse,

    -- ** DeleteClusterParameterGroup
    DeleteClusterParameterGroup (DeleteClusterParameterGroup'),
    newDeleteClusterParameterGroup,
    DeleteClusterParameterGroupResponse (DeleteClusterParameterGroupResponse'),
    newDeleteClusterParameterGroupResponse,

    -- ** CreateSnapshotSchedule
    CreateSnapshotSchedule (CreateSnapshotSchedule'),
    newCreateSnapshotSchedule,
    SnapshotSchedule (SnapshotSchedule'),
    newSnapshotSchedule,

    -- ** DescribeNodeConfigurationOptions (Paginated)
    DescribeNodeConfigurationOptions (DescribeNodeConfigurationOptions'),
    newDescribeNodeConfigurationOptions,
    DescribeNodeConfigurationOptionsResponse (DescribeNodeConfigurationOptionsResponse'),
    newDescribeNodeConfigurationOptionsResponse,

    -- ** DescribeClusterParameterGroups (Paginated)
    DescribeClusterParameterGroups (DescribeClusterParameterGroups'),
    newDescribeClusterParameterGroups,
    DescribeClusterParameterGroupsResponse (DescribeClusterParameterGroupsResponse'),
    newDescribeClusterParameterGroupsResponse,

    -- ** DescribeLoggingStatus
    DescribeLoggingStatus (DescribeLoggingStatus'),
    newDescribeLoggingStatus,
    LoggingStatus (LoggingStatus'),
    newLoggingStatus,

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

    -- ** CreateClusterSubnetGroup
    CreateClusterSubnetGroup (CreateClusterSubnetGroup'),
    newCreateClusterSubnetGroup,
    CreateClusterSubnetGroupResponse (CreateClusterSubnetGroupResponse'),
    newCreateClusterSubnetGroupResponse,

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

    -- ** DescribeResize
    DescribeResize (DescribeResize'),
    newDescribeResize,
    ResizeProgressMessage (ResizeProgressMessage'),
    newResizeProgressMessage,

    -- ** DeleteHsmConfiguration
    DeleteHsmConfiguration (DeleteHsmConfiguration'),
    newDeleteHsmConfiguration,
    DeleteHsmConfigurationResponse (DeleteHsmConfigurationResponse'),
    newDeleteHsmConfigurationResponse,

    -- ** DeleteClusterSnapshot
    DeleteClusterSnapshot (DeleteClusterSnapshot'),
    newDeleteClusterSnapshot,
    DeleteClusterSnapshotResponse (DeleteClusterSnapshotResponse'),
    newDeleteClusterSnapshotResponse,

    -- ** CreateClusterSecurityGroup
    CreateClusterSecurityGroup (CreateClusterSecurityGroup'),
    newCreateClusterSecurityGroup,
    CreateClusterSecurityGroupResponse (CreateClusterSecurityGroupResponse'),
    newCreateClusterSecurityGroupResponse,

    -- ** AcceptReservedNodeExchange
    AcceptReservedNodeExchange (AcceptReservedNodeExchange'),
    newAcceptReservedNodeExchange,
    AcceptReservedNodeExchangeResponse (AcceptReservedNodeExchangeResponse'),
    newAcceptReservedNodeExchangeResponse,

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

    -- ** DescribeSnapshotSchedules (Paginated)
    DescribeSnapshotSchedules (DescribeSnapshotSchedules'),
    newDescribeSnapshotSchedules,
    DescribeSnapshotSchedulesResponse (DescribeSnapshotSchedulesResponse'),
    newDescribeSnapshotSchedulesResponse,

    -- ** DescribeTableRestoreStatus (Paginated)
    DescribeTableRestoreStatus (DescribeTableRestoreStatus'),
    newDescribeTableRestoreStatus,
    DescribeTableRestoreStatusResponse (DescribeTableRestoreStatusResponse'),
    newDescribeTableRestoreStatusResponse,

    -- ** ModifyClusterMaintenance
    ModifyClusterMaintenance (ModifyClusterMaintenance'),
    newModifyClusterMaintenance,
    ModifyClusterMaintenanceResponse (ModifyClusterMaintenanceResponse'),
    newModifyClusterMaintenanceResponse,

    -- ** AuthorizeClusterSecurityGroupIngress
    AuthorizeClusterSecurityGroupIngress (AuthorizeClusterSecurityGroupIngress'),
    newAuthorizeClusterSecurityGroupIngress,
    AuthorizeClusterSecurityGroupIngressResponse (AuthorizeClusterSecurityGroupIngressResponse'),
    newAuthorizeClusterSecurityGroupIngressResponse,

    -- ** ModifyClusterDbRevision
    ModifyClusterDbRevision (ModifyClusterDbRevision'),
    newModifyClusterDbRevision,
    ModifyClusterDbRevisionResponse (ModifyClusterDbRevisionResponse'),
    newModifyClusterDbRevisionResponse,

    -- ** DescribeStorage
    DescribeStorage (DescribeStorage'),
    newDescribeStorage,
    DescribeStorageResponse (DescribeStorageResponse'),
    newDescribeStorageResponse,

    -- ** DescribeSnapshotCopyGrants (Paginated)
    DescribeSnapshotCopyGrants (DescribeSnapshotCopyGrants'),
    newDescribeSnapshotCopyGrants,
    DescribeSnapshotCopyGrantsResponse (DescribeSnapshotCopyGrantsResponse'),
    newDescribeSnapshotCopyGrantsResponse,

    -- ** BatchModifyClusterSnapshots
    BatchModifyClusterSnapshots (BatchModifyClusterSnapshots'),
    newBatchModifyClusterSnapshots,
    BatchModifyClusterSnapshotsResponse (BatchModifyClusterSnapshotsResponse'),
    newBatchModifyClusterSnapshotsResponse,

    -- ** ModifySnapshotSchedule
    ModifySnapshotSchedule (ModifySnapshotSchedule'),
    newModifySnapshotSchedule,
    SnapshotSchedule (SnapshotSchedule'),
    newSnapshotSchedule,

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

    -- ** DescribeDefaultClusterParameters (Paginated)
    DescribeDefaultClusterParameters (DescribeDefaultClusterParameters'),
    newDescribeDefaultClusterParameters,
    DescribeDefaultClusterParametersResponse (DescribeDefaultClusterParametersResponse'),
    newDescribeDefaultClusterParametersResponse,

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

    -- ** DescribeOrderableClusterOptions (Paginated)
    DescribeOrderableClusterOptions (DescribeOrderableClusterOptions'),
    newDescribeOrderableClusterOptions,
    DescribeOrderableClusterOptionsResponse (DescribeOrderableClusterOptionsResponse'),
    newDescribeOrderableClusterOptionsResponse,

    -- ** DeleteHsmClientCertificate
    DeleteHsmClientCertificate (DeleteHsmClientCertificate'),
    newDeleteHsmClientCertificate,
    DeleteHsmClientCertificateResponse (DeleteHsmClientCertificateResponse'),
    newDeleteHsmClientCertificateResponse,

    -- ** RebootCluster
    RebootCluster (RebootCluster'),
    newRebootCluster,
    RebootClusterResponse (RebootClusterResponse'),
    newRebootClusterResponse,

    -- ** DeleteCluster
    DeleteCluster (DeleteCluster'),
    newDeleteCluster,
    DeleteClusterResponse (DeleteClusterResponse'),
    newDeleteClusterResponse,

    -- ** CreateEventSubscription
    CreateEventSubscription (CreateEventSubscription'),
    newCreateEventSubscription,
    CreateEventSubscriptionResponse (CreateEventSubscriptionResponse'),
    newCreateEventSubscriptionResponse,

    -- ** AuthorizeSnapshotAccess
    AuthorizeSnapshotAccess (AuthorizeSnapshotAccess'),
    newAuthorizeSnapshotAccess,
    AuthorizeSnapshotAccessResponse (AuthorizeSnapshotAccessResponse'),
    newAuthorizeSnapshotAccessResponse,

    -- ** ResumeCluster
    ResumeCluster (ResumeCluster'),
    newResumeCluster,
    ResumeClusterResponse (ResumeClusterResponse'),
    newResumeClusterResponse,

    -- ** DescribeClusterTracks (Paginated)
    DescribeClusterTracks (DescribeClusterTracks'),
    newDescribeClusterTracks,
    DescribeClusterTracksResponse (DescribeClusterTracksResponse'),
    newDescribeClusterTracksResponse,

    -- ** CreateScheduledAction
    CreateScheduledAction (CreateScheduledAction'),
    newCreateScheduledAction,
    ScheduledAction (ScheduledAction'),
    newScheduledAction,

    -- ** DeleteSnapshotSchedule
    DeleteSnapshotSchedule (DeleteSnapshotSchedule'),
    newDeleteSnapshotSchedule,
    DeleteSnapshotScheduleResponse (DeleteSnapshotScheduleResponse'),
    newDeleteSnapshotScheduleResponse,

    -- ** DescribeClusterSubnetGroups (Paginated)
    DescribeClusterSubnetGroups (DescribeClusterSubnetGroups'),
    newDescribeClusterSubnetGroups,
    DescribeClusterSubnetGroupsResponse (DescribeClusterSubnetGroupsResponse'),
    newDescribeClusterSubnetGroupsResponse,

    -- ** BatchDeleteClusterSnapshots
    BatchDeleteClusterSnapshots (BatchDeleteClusterSnapshots'),
    newBatchDeleteClusterSnapshots,
    BatchDeleteClusterSnapshotsResponse (BatchDeleteClusterSnapshotsResponse'),
    newBatchDeleteClusterSnapshotsResponse,

    -- ** ModifyClusterIamRoles
    ModifyClusterIamRoles (ModifyClusterIamRoles'),
    newModifyClusterIamRoles,
    ModifyClusterIamRolesResponse (ModifyClusterIamRolesResponse'),
    newModifyClusterIamRolesResponse,

    -- ** ResizeCluster
    ResizeCluster (ResizeCluster'),
    newResizeCluster,
    ResizeClusterResponse (ResizeClusterResponse'),
    newResizeClusterResponse,

    -- ** ModifySnapshotCopyRetentionPeriod
    ModifySnapshotCopyRetentionPeriod (ModifySnapshotCopyRetentionPeriod'),
    newModifySnapshotCopyRetentionPeriod,
    ModifySnapshotCopyRetentionPeriodResponse (ModifySnapshotCopyRetentionPeriodResponse'),
    newModifySnapshotCopyRetentionPeriodResponse,

    -- ** RestoreFromClusterSnapshot
    RestoreFromClusterSnapshot (RestoreFromClusterSnapshot'),
    newRestoreFromClusterSnapshot,
    RestoreFromClusterSnapshotResponse (RestoreFromClusterSnapshotResponse'),
    newRestoreFromClusterSnapshotResponse,

    -- ** RevokeSnapshotAccess
    RevokeSnapshotAccess (RevokeSnapshotAccess'),
    newRevokeSnapshotAccess,
    RevokeSnapshotAccessResponse (RevokeSnapshotAccessResponse'),
    newRevokeSnapshotAccessResponse,

    -- ** PauseCluster
    PauseCluster (PauseCluster'),
    newPauseCluster,
    PauseClusterResponse (PauseClusterResponse'),
    newPauseClusterResponse,

    -- ** DescribeClusterParameters (Paginated)
    DescribeClusterParameters (DescribeClusterParameters'),
    newDescribeClusterParameters,
    DescribeClusterParametersResponse (DescribeClusterParametersResponse'),
    newDescribeClusterParametersResponse,

    -- ** DisableSnapshotCopy
    DisableSnapshotCopy (DisableSnapshotCopy'),
    newDisableSnapshotCopy,
    DisableSnapshotCopyResponse (DisableSnapshotCopyResponse'),
    newDisableSnapshotCopyResponse,

    -- ** CreateTags
    CreateTags (CreateTags'),
    newCreateTags,
    CreateTagsResponse (CreateTagsResponse'),
    newCreateTagsResponse,

    -- ** DescribeReservedNodeOfferings (Paginated)
    DescribeReservedNodeOfferings (DescribeReservedNodeOfferings'),
    newDescribeReservedNodeOfferings,
    DescribeReservedNodeOfferingsResponse (DescribeReservedNodeOfferingsResponse'),
    newDescribeReservedNodeOfferingsResponse,

    -- ** DeleteClusterSecurityGroup
    DeleteClusterSecurityGroup (DeleteClusterSecurityGroup'),
    newDeleteClusterSecurityGroup,
    DeleteClusterSecurityGroupResponse (DeleteClusterSecurityGroupResponse'),
    newDeleteClusterSecurityGroupResponse,

    -- ** EnableLogging
    EnableLogging (EnableLogging'),
    newEnableLogging,
    LoggingStatus (LoggingStatus'),
    newLoggingStatus,

    -- ** CreateHsmConfiguration
    CreateHsmConfiguration (CreateHsmConfiguration'),
    newCreateHsmConfiguration,
    CreateHsmConfigurationResponse (CreateHsmConfigurationResponse'),
    newCreateHsmConfigurationResponse,

    -- ** DescribeReservedNodes (Paginated)
    DescribeReservedNodes (DescribeReservedNodes'),
    newDescribeReservedNodes,
    DescribeReservedNodesResponse (DescribeReservedNodesResponse'),
    newDescribeReservedNodesResponse,

    -- * Types

    -- ** Common
    module Network.AWS.Redshift.Internal,

    -- ** ActionType
    ActionType (..),

    -- ** Mode
    Mode (..),

    -- ** NodeConfigurationOptionsFilterName
    NodeConfigurationOptionsFilterName (..),

    -- ** OperatorType
    OperatorType (..),

    -- ** ParameterApplyType
    ParameterApplyType (..),

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

    -- ** AttributeValueTarget
    AttributeValueTarget (AttributeValueTarget'),
    newAttributeValueTarget,

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
import Network.AWS.Redshift.AuthorizeClusterSecurityGroupIngress
import Network.AWS.Redshift.AuthorizeSnapshotAccess
import Network.AWS.Redshift.BatchDeleteClusterSnapshots
import Network.AWS.Redshift.BatchModifyClusterSnapshots
import Network.AWS.Redshift.CancelResize
import Network.AWS.Redshift.CopyClusterSnapshot
import Network.AWS.Redshift.CreateCluster
import Network.AWS.Redshift.CreateClusterParameterGroup
import Network.AWS.Redshift.CreateClusterSecurityGroup
import Network.AWS.Redshift.CreateClusterSnapshot
import Network.AWS.Redshift.CreateClusterSubnetGroup
import Network.AWS.Redshift.CreateEventSubscription
import Network.AWS.Redshift.CreateHsmClientCertificate
import Network.AWS.Redshift.CreateHsmConfiguration
import Network.AWS.Redshift.CreateScheduledAction
import Network.AWS.Redshift.CreateSnapshotCopyGrant
import Network.AWS.Redshift.CreateSnapshotSchedule
import Network.AWS.Redshift.CreateTags
import Network.AWS.Redshift.CreateUsageLimit
import Network.AWS.Redshift.DeleteCluster
import Network.AWS.Redshift.DeleteClusterParameterGroup
import Network.AWS.Redshift.DeleteClusterSecurityGroup
import Network.AWS.Redshift.DeleteClusterSnapshot
import Network.AWS.Redshift.DeleteClusterSubnetGroup
import Network.AWS.Redshift.DeleteEventSubscription
import Network.AWS.Redshift.DeleteHsmClientCertificate
import Network.AWS.Redshift.DeleteHsmConfiguration
import Network.AWS.Redshift.DeleteScheduledAction
import Network.AWS.Redshift.DeleteSnapshotCopyGrant
import Network.AWS.Redshift.DeleteSnapshotSchedule
import Network.AWS.Redshift.DeleteTags
import Network.AWS.Redshift.DeleteUsageLimit
import Network.AWS.Redshift.DescribeAccountAttributes
import Network.AWS.Redshift.DescribeClusterDbRevisions
import Network.AWS.Redshift.DescribeClusterParameterGroups
import Network.AWS.Redshift.DescribeClusterParameters
import Network.AWS.Redshift.DescribeClusterSecurityGroups
import Network.AWS.Redshift.DescribeClusterSnapshots
import Network.AWS.Redshift.DescribeClusterSubnetGroups
import Network.AWS.Redshift.DescribeClusterTracks
import Network.AWS.Redshift.DescribeClusterVersions
import Network.AWS.Redshift.DescribeClusters
import Network.AWS.Redshift.DescribeDefaultClusterParameters
import Network.AWS.Redshift.DescribeEventCategories
import Network.AWS.Redshift.DescribeEventSubscriptions
import Network.AWS.Redshift.DescribeEvents
import Network.AWS.Redshift.DescribeHsmClientCertificates
import Network.AWS.Redshift.DescribeHsmConfigurations
import Network.AWS.Redshift.DescribeLoggingStatus
import Network.AWS.Redshift.DescribeNodeConfigurationOptions
import Network.AWS.Redshift.DescribeOrderableClusterOptions
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
import Network.AWS.Redshift.EnableLogging
import Network.AWS.Redshift.EnableSnapshotCopy
import Network.AWS.Redshift.GetClusterCredentials
import Network.AWS.Redshift.GetReservedNodeExchangeOfferings
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Lens
import Network.AWS.Redshift.ModifyCluster
import Network.AWS.Redshift.ModifyClusterDbRevision
import Network.AWS.Redshift.ModifyClusterIamRoles
import Network.AWS.Redshift.ModifyClusterMaintenance
import Network.AWS.Redshift.ModifyClusterParameterGroup
import Network.AWS.Redshift.ModifyClusterSnapshot
import Network.AWS.Redshift.ModifyClusterSnapshotSchedule
import Network.AWS.Redshift.ModifyClusterSubnetGroup
import Network.AWS.Redshift.ModifyEventSubscription
import Network.AWS.Redshift.ModifyScheduledAction
import Network.AWS.Redshift.ModifySnapshotCopyRetentionPeriod
import Network.AWS.Redshift.ModifySnapshotSchedule
import Network.AWS.Redshift.ModifyUsageLimit
import Network.AWS.Redshift.PauseCluster
import Network.AWS.Redshift.PurchaseReservedNodeOffering
import Network.AWS.Redshift.RebootCluster
import Network.AWS.Redshift.ResetClusterParameterGroup
import Network.AWS.Redshift.ResizeCluster
import Network.AWS.Redshift.RestoreFromClusterSnapshot
import Network.AWS.Redshift.RestoreTableFromClusterSnapshot
import Network.AWS.Redshift.ResumeCluster
import Network.AWS.Redshift.RevokeClusterSecurityGroupIngress
import Network.AWS.Redshift.RevokeSnapshotAccess
import Network.AWS.Redshift.RotateEncryptionKey
import Network.AWS.Redshift.Types
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
