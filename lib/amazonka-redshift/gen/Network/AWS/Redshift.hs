{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Amazon Redshift__
--
-- __Overview__
-- This is an interface reference for Amazon Redshift. It contains documentation for one of the programming or command line interfaces you can use to manage Amazon Redshift clusters. Note that Amazon Redshift is asynchronous, which means that some interfaces may require techniques, such as polling or asynchronous callback handlers, to determine when a command has been applied. In this reference, the parameter descriptions indicate whether a change is applied immediately, on the next instance reboot, or during the next maintenance window. For a summary of the Amazon Redshift cluster management interfaces, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/using-aws-sdk.html Using the Amazon Redshift Management Interfaces> .
-- Amazon Redshift manages all the work of setting up, operating, and scaling a data warehouse: provisioning capacity, monitoring and backing up the cluster, and applying patches and upgrades to the Amazon Redshift engine. You can focus on using your data to acquire new insights for your business and customers.
-- If you are a first-time user of Amazon Redshift, we recommend that you begin by reading the <https://docs.aws.amazon.com/redshift/latest/gsg/getting-started.html Amazon Redshift Getting Started Guide> .
-- If you are a database developer, the <https://docs.aws.amazon.com/redshift/latest/dg/welcome.html Amazon Redshift Database Developer Guide> explains how to design, build, query, and maintain the databases that make up your data warehouse.
module Network.AWS.Redshift
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    -- $errors

    -- ** ClusterSecurityGroupQuotaExceededFault
    _ClusterSecurityGroupQuotaExceededFault,

    -- ** InvalidS3KeyPrefixFault
    _InvalidS3KeyPrefixFault,

    -- ** SourceNotFoundFault
    _SourceNotFoundFault,

    -- ** AuthorizationQuotaExceededFault
    _AuthorizationQuotaExceededFault,

    -- ** CopyToRegionDisabledFault
    _CopyToRegionDisabledFault,

    -- ** LimitExceededFault
    _LimitExceededFault,

    -- ** InvalidClusterSecurityGroupStateFault
    _InvalidClusterSecurityGroupStateFault,

    -- ** ClusterSecurityGroupAlreadyExistsFault
    _ClusterSecurityGroupAlreadyExistsFault,

    -- ** ClusterSnapshotNotFoundFault
    _ClusterSnapshotNotFoundFault,

    -- ** InvalidElasticIpFault
    _InvalidElasticIpFault,

    -- ** TableRestoreNotFoundFault
    _TableRestoreNotFoundFault,

    -- ** HsmConfigurationNotFoundFault
    _HsmConfigurationNotFoundFault,

    -- ** ScheduleDefinitionTypeUnsupportedFault
    _ScheduleDefinitionTypeUnsupportedFault,

    -- ** AuthorizationAlreadyExistsFault
    _AuthorizationAlreadyExistsFault,

    -- ** SubscriptionCategoryNotFoundFault
    _SubscriptionCategoryNotFoundFault,

    -- ** InvalidRetentionPeriodFault
    _InvalidRetentionPeriodFault,

    -- ** HsmConfigurationAlreadyExistsFault
    _HsmConfigurationAlreadyExistsFault,

    -- ** SubscriptionNotFoundFault
    _SubscriptionNotFoundFault,

    -- ** InvalidS3BucketNameFault
    _InvalidS3BucketNameFault,

    -- ** ClusterSnapshotAlreadyExistsFault
    _ClusterSnapshotAlreadyExistsFault,

    -- ** InvalidSubnet
    _InvalidSubnet,

    -- ** TableLimitExceededFault
    _TableLimitExceededFault,

    -- ** InvalidHsmConfigurationStateFault
    _InvalidHsmConfigurationStateFault,

    -- ** SnapshotCopyAlreadyDisabledFault
    _SnapshotCopyAlreadyDisabledFault,

    -- ** ClusterQuotaExceededFault
    _ClusterQuotaExceededFault,

    -- ** HsmClientCertificateQuotaExceededFault
    _HsmClientCertificateQuotaExceededFault,

    -- ** SnapshotScheduleAlreadyExistsFault
    _SnapshotScheduleAlreadyExistsFault,

    -- ** BatchModifyClusterSnapshotsLimitExceededFault
    _BatchModifyClusterSnapshotsLimitExceededFault,

    -- ** ClusterParameterGroupNotFoundFault
    _ClusterParameterGroupNotFoundFault,

    -- ** SnapshotCopyGrantQuotaExceededFault
    _SnapshotCopyGrantQuotaExceededFault,

    -- ** NumberOfNodesPerClusterLimitExceededFault
    _NumberOfNodesPerClusterLimitExceededFault,

    -- ** SnapshotCopyAlreadyEnabledFault
    _SnapshotCopyAlreadyEnabledFault,

    -- ** ClusterParameterGroupAlreadyExistsFault
    _ClusterParameterGroupAlreadyExistsFault,

    -- ** BatchDeleteRequestSizeExceededFault
    _BatchDeleteRequestSizeExceededFault,

    -- ** SnapshotCopyDisabledFault
    _SnapshotCopyDisabledFault,

    -- ** ResizeNotFoundFault
    _ResizeNotFoundFault,

    -- ** HsmClientCertificateNotFoundFault
    _HsmClientCertificateNotFoundFault,

    -- ** SNSTopicArnNotFoundFault
    _SNSTopicArnNotFoundFault,

    -- ** ClusterNotFoundFault
    _ClusterNotFoundFault,

    -- ** ClusterParameterGroupQuotaExceededFault
    _ClusterParameterGroupQuotaExceededFault,

    -- ** SnapshotCopyGrantAlreadyExistsFault
    _SnapshotCopyGrantAlreadyExistsFault,

    -- ** SNSNoAuthorizationFault
    _SNSNoAuthorizationFault,

    -- ** InvalidClusterStateFault
    _InvalidClusterStateFault,

    -- ** InvalidTableRestoreArgumentFault
    _InvalidTableRestoreArgumentFault,

    -- ** SnapshotCopyGrantNotFoundFault
    _SnapshotCopyGrantNotFoundFault,

    -- ** InvalidScheduleFault
    _InvalidScheduleFault,

    -- ** InvalidClusterTrackFault
    _InvalidClusterTrackFault,

    -- ** HsmConfigurationQuotaExceededFault
    _HsmConfigurationQuotaExceededFault,

    -- ** ClusterSnapshotQuotaExceededFault
    _ClusterSnapshotQuotaExceededFault,

    -- ** InsufficientClusterCapacityFault
    _InsufficientClusterCapacityFault,

    -- ** InvalidScheduledActionFault
    _InvalidScheduledActionFault,

    -- ** SNSInvalidTopicFault
    _SNSInvalidTopicFault,

    -- ** UsageLimitAlreadyExistsFault
    _UsageLimitAlreadyExistsFault,

    -- ** DependentServiceUnavailableFault
    _DependentServiceUnavailableFault,

    -- ** UnsupportedOptionFault
    _UnsupportedOptionFault,

    -- ** SubscriptionAlreadyExistFault
    _SubscriptionAlreadyExistFault,

    -- ** InvalidClusterSnapshotScheduleStateFault
    _InvalidClusterSnapshotScheduleStateFault,

    -- ** InvalidVPCNetworkStateFault
    _InvalidVPCNetworkStateFault,

    -- ** ClusterSubnetGroupNotFoundFault
    _ClusterSubnetGroupNotFoundFault,

    -- ** BucketNotFoundFault
    _BucketNotFoundFault,

    -- ** InvalidSubscriptionStateFault
    _InvalidSubscriptionStateFault,

    -- ** DependentServiceRequestThrottlingFault
    _DependentServiceRequestThrottlingFault,

    -- ** AuthorizationNotFoundFault
    _AuthorizationNotFoundFault,

    -- ** InvalidClusterSubnetGroupStateFault
    _InvalidClusterSubnetGroupStateFault,

    -- ** UnsupportedOperationFault
    _UnsupportedOperationFault,

    -- ** ClusterSubnetGroupAlreadyExistsFault
    _ClusterSubnetGroupAlreadyExistsFault,

    -- ** InvalidClusterSnapshotStateFault
    _InvalidClusterSnapshotStateFault,

    -- ** ClusterSecurityGroupNotFoundFault
    _ClusterSecurityGroupNotFoundFault,

    -- ** ReservedNodeNotFoundFault
    _ReservedNodeNotFoundFault,

    -- ** ReservedNodeOfferingNotFoundFault
    _ReservedNodeOfferingNotFoundFault,

    -- ** InvalidClusterSubnetStateFault
    _InvalidClusterSubnetStateFault,

    -- ** IncompatibleOrderableOptions
    _IncompatibleOrderableOptions,

    -- ** ReservedNodeAlreadyMigratedFault
    _ReservedNodeAlreadyMigratedFault,

    -- ** EventSubscriptionQuotaExceededFault
    _EventSubscriptionQuotaExceededFault,

    -- ** SnapshotScheduleNotFoundFault
    _SnapshotScheduleNotFoundFault,

    -- ** InvalidClusterParameterGroupStateFault
    _InvalidClusterParameterGroupStateFault,

    -- ** ScheduledActionQuotaExceededFault
    _ScheduledActionQuotaExceededFault,

    -- ** InvalidReservedNodeStateFault
    _InvalidReservedNodeStateFault,

    -- ** ReservedNodeAlreadyExistsFault
    _ReservedNodeAlreadyExistsFault,

    -- ** ScheduledActionTypeUnsupportedFault
    _ScheduledActionTypeUnsupportedFault,

    -- ** SnapshotScheduleUpdateInProgressFault
    _SnapshotScheduleUpdateInProgressFault,

    -- ** InProgressTableRestoreQuotaExceededFault
    _InProgressTableRestoreQuotaExceededFault,

    -- ** InvalidRestoreFault
    _InvalidRestoreFault,

    -- ** ResourceNotFoundFault
    _ResourceNotFoundFault,

    -- ** SubscriptionEventIdNotFoundFault
    _SubscriptionEventIdNotFoundFault,

    -- ** InvalidUsageLimitFault
    _InvalidUsageLimitFault,

    -- ** InvalidSnapshotCopyGrantStateFault
    _InvalidSnapshotCopyGrantStateFault,

    -- ** UnknownSnapshotCopyRegionFault
    _UnknownSnapshotCopyRegionFault,

    -- ** ReservedNodeQuotaExceededFault
    _ReservedNodeQuotaExceededFault,

    -- ** ScheduledActionAlreadyExistsFault
    _ScheduledActionAlreadyExistsFault,

    -- ** ClusterSubnetQuotaExceededFault
    _ClusterSubnetQuotaExceededFault,

    -- ** ClusterAlreadyExistsFault
    _ClusterAlreadyExistsFault,

    -- ** AccessToSnapshotDeniedFault
    _AccessToSnapshotDeniedFault,

    -- ** TagLimitExceededFault
    _TagLimitExceededFault,

    -- ** NumberOfNodesQuotaExceededFault
    _NumberOfNodesQuotaExceededFault,

    -- ** ScheduledActionNotFoundFault
    _ScheduledActionNotFoundFault,

    -- ** HsmClientCertificateAlreadyExistsFault
    _HsmClientCertificateAlreadyExistsFault,

    -- ** SnapshotScheduleQuotaExceededFault
    _SnapshotScheduleQuotaExceededFault,

    -- ** InvalidHsmClientCertificateStateFault
    _InvalidHsmClientCertificateStateFault,

    -- ** ClusterOnLatestRevisionFault
    _ClusterOnLatestRevisionFault,

    -- ** SubnetAlreadyInUse
    _SubnetAlreadyInUse,

    -- ** SubscriptionSeverityNotFoundFault
    _SubscriptionSeverityNotFoundFault,

    -- ** UsageLimitNotFoundFault
    _UsageLimitNotFoundFault,

    -- ** UnauthorizedOperation
    _UnauthorizedOperation,

    -- ** InvalidTagFault
    _InvalidTagFault,

    -- ** InsufficientS3BucketPolicyFault
    _InsufficientS3BucketPolicyFault,

    -- ** ClusterSubnetGroupQuotaExceededFault
    _ClusterSubnetGroupQuotaExceededFault,

    -- * Waiters
    -- $waiters

    -- ** ClusterRestored
    mkClusterRestored,

    -- ** ClusterDeleted
    mkClusterDeleted,

    -- ** SnapshotAvailable
    mkSnapshotAvailable,

    -- ** ClusterAvailable
    mkClusterAvailable,

    -- * Operations
    -- $operations

    -- ** CancelResize
    module Network.AWS.Redshift.CancelResize,

    -- ** DescribeStorage
    module Network.AWS.Redshift.DescribeStorage,

    -- ** DescribeClusters (Paginated)
    module Network.AWS.Redshift.DescribeClusters,

    -- ** DescribeTags (Paginated)
    module Network.AWS.Redshift.DescribeTags,

    -- ** CreateUsageLimit
    module Network.AWS.Redshift.CreateUsageLimit,

    -- ** DeleteClusterSubnetGroup
    module Network.AWS.Redshift.DeleteClusterSubnetGroup,

    -- ** ModifyScheduledAction
    module Network.AWS.Redshift.ModifyScheduledAction,

    -- ** DisableLogging
    module Network.AWS.Redshift.DisableLogging,

    -- ** DescribeSnapshotSchedules (Paginated)
    module Network.AWS.Redshift.DescribeSnapshotSchedules,

    -- ** ModifyEventSubscription
    module Network.AWS.Redshift.ModifyEventSubscription,

    -- ** ModifyClusterDbRevision
    module Network.AWS.Redshift.ModifyClusterDbRevision,

    -- ** DeleteClusterSnapshot
    module Network.AWS.Redshift.DeleteClusterSnapshot,

    -- ** PurchaseReservedNodeOffering
    module Network.AWS.Redshift.PurchaseReservedNodeOffering,

    -- ** DescribeReservedNodeOfferings (Paginated)
    module Network.AWS.Redshift.DescribeReservedNodeOfferings,

    -- ** DescribeEvents (Paginated)
    module Network.AWS.Redshift.DescribeEvents,

    -- ** DescribeReservedNodes (Paginated)
    module Network.AWS.Redshift.DescribeReservedNodes,

    -- ** GetReservedNodeExchangeOfferings (Paginated)
    module Network.AWS.Redshift.GetReservedNodeExchangeOfferings,

    -- ** DescribeClusterParameterGroups (Paginated)
    module Network.AWS.Redshift.DescribeClusterParameterGroups,

    -- ** EnableLogging
    module Network.AWS.Redshift.EnableLogging,

    -- ** CreateClusterSubnetGroup
    module Network.AWS.Redshift.CreateClusterSubnetGroup,

    -- ** DeleteClusterParameterGroup
    module Network.AWS.Redshift.DeleteClusterParameterGroup,

    -- ** DescribeClusterSecurityGroups (Paginated)
    module Network.AWS.Redshift.DescribeClusterSecurityGroups,

    -- ** CreateTags
    module Network.AWS.Redshift.CreateTags,

    -- ** EnableSnapshotCopy
    module Network.AWS.Redshift.EnableSnapshotCopy,

    -- ** DescribeClusterSnapshots (Paginated)
    module Network.AWS.Redshift.DescribeClusterSnapshots,

    -- ** BatchDeleteClusterSnapshots
    module Network.AWS.Redshift.BatchDeleteClusterSnapshots,

    -- ** DeleteTags
    module Network.AWS.Redshift.DeleteTags,

    -- ** ModifyUsageLimit
    module Network.AWS.Redshift.ModifyUsageLimit,

    -- ** DescribeClusterSubnetGroups (Paginated)
    module Network.AWS.Redshift.DescribeClusterSubnetGroups,

    -- ** ResizeCluster
    module Network.AWS.Redshift.ResizeCluster,

    -- ** ModifySnapshotCopyRetentionPeriod
    module Network.AWS.Redshift.ModifySnapshotCopyRetentionPeriod,

    -- ** ModifyClusterIamRoles
    module Network.AWS.Redshift.ModifyClusterIamRoles,

    -- ** AuthorizeSnapshotAccess
    module Network.AWS.Redshift.AuthorizeSnapshotAccess,

    -- ** RebootCluster
    module Network.AWS.Redshift.RebootCluster,

    -- ** ResumeCluster
    module Network.AWS.Redshift.ResumeCluster,

    -- ** DeleteCluster
    module Network.AWS.Redshift.DeleteCluster,

    -- ** CreateEventSubscription
    module Network.AWS.Redshift.CreateEventSubscription,

    -- ** CreateScheduledAction
    module Network.AWS.Redshift.CreateScheduledAction,

    -- ** DescribeOrderableClusterOptions (Paginated)
    module Network.AWS.Redshift.DescribeOrderableClusterOptions,

    -- ** DescribeClusterTracks (Paginated)
    module Network.AWS.Redshift.DescribeClusterTracks,

    -- ** CreateCluster
    module Network.AWS.Redshift.CreateCluster,

    -- ** CreateHsmClientCertificate
    module Network.AWS.Redshift.CreateHsmClientCertificate,

    -- ** RestoreTableFromClusterSnapshot
    module Network.AWS.Redshift.RestoreTableFromClusterSnapshot,

    -- ** DeleteScheduledAction
    module Network.AWS.Redshift.DeleteScheduledAction,

    -- ** DescribeDefaultClusterParameters (Paginated)
    module Network.AWS.Redshift.DescribeDefaultClusterParameters,

    -- ** DeleteEventSubscription
    module Network.AWS.Redshift.DeleteEventSubscription,

    -- ** ModifyClusterSnapshot
    module Network.AWS.Redshift.ModifyClusterSnapshot,

    -- ** ResetClusterParameterGroup
    module Network.AWS.Redshift.ResetClusterParameterGroup,

    -- ** DescribeScheduledActions (Paginated)
    module Network.AWS.Redshift.DescribeScheduledActions,

    -- ** DescribeEventSubscriptions (Paginated)
    module Network.AWS.Redshift.DescribeEventSubscriptions,

    -- ** DescribeClusterDbRevisions (Paginated)
    module Network.AWS.Redshift.DescribeClusterDbRevisions,

    -- ** BatchModifyClusterSnapshots
    module Network.AWS.Redshift.BatchModifyClusterSnapshots,

    -- ** DeleteUsageLimit
    module Network.AWS.Redshift.DeleteUsageLimit,

    -- ** RevokeClusterSecurityGroupIngress
    module Network.AWS.Redshift.RevokeClusterSecurityGroupIngress,

    -- ** DescribeHsmClientCertificates (Paginated)
    module Network.AWS.Redshift.DescribeHsmClientCertificates,

    -- ** ModifyClusterParameterGroup
    module Network.AWS.Redshift.ModifyClusterParameterGroup,

    -- ** GetClusterCredentials
    module Network.AWS.Redshift.GetClusterCredentials,

    -- ** ModifyClusterMaintenance
    module Network.AWS.Redshift.ModifyClusterMaintenance,

    -- ** CreateClusterSecurityGroup
    module Network.AWS.Redshift.CreateClusterSecurityGroup,

    -- ** DescribeEventCategories
    module Network.AWS.Redshift.DescribeEventCategories,

    -- ** DescribeResize
    module Network.AWS.Redshift.DescribeResize,

    -- ** DeleteHsmConfiguration
    module Network.AWS.Redshift.DeleteHsmConfiguration,

    -- ** AcceptReservedNodeExchange
    module Network.AWS.Redshift.AcceptReservedNodeExchange,

    -- ** AuthorizeClusterSecurityGroupIngress
    module Network.AWS.Redshift.AuthorizeClusterSecurityGroupIngress,

    -- ** DescribeTableRestoreStatus (Paginated)
    module Network.AWS.Redshift.DescribeTableRestoreStatus,

    -- ** CreateClusterSnapshot
    module Network.AWS.Redshift.CreateClusterSnapshot,

    -- ** CreateHsmConfiguration
    module Network.AWS.Redshift.CreateHsmConfiguration,

    -- ** DescribeLoggingStatus
    module Network.AWS.Redshift.DescribeLoggingStatus,

    -- ** ModifyCluster
    module Network.AWS.Redshift.ModifyCluster,

    -- ** DeleteClusterSecurityGroup
    module Network.AWS.Redshift.DeleteClusterSecurityGroup,

    -- ** CreateSnapshotSchedule
    module Network.AWS.Redshift.CreateSnapshotSchedule,

    -- ** DescribeNodeConfigurationOptions (Paginated)
    module Network.AWS.Redshift.DescribeNodeConfigurationOptions,

    -- ** DisableSnapshotCopy
    module Network.AWS.Redshift.DisableSnapshotCopy,

    -- ** DescribeClusterParameters (Paginated)
    module Network.AWS.Redshift.DescribeClusterParameters,

    -- ** PauseCluster
    module Network.AWS.Redshift.PauseCluster,

    -- ** DeleteSnapshotSchedule
    module Network.AWS.Redshift.DeleteSnapshotSchedule,

    -- ** RestoreFromClusterSnapshot
    module Network.AWS.Redshift.RestoreFromClusterSnapshot,

    -- ** CreateClusterParameterGroup
    module Network.AWS.Redshift.CreateClusterParameterGroup,

    -- ** RevokeSnapshotAccess
    module Network.AWS.Redshift.RevokeSnapshotAccess,

    -- ** DescribeHsmConfigurations (Paginated)
    module Network.AWS.Redshift.DescribeHsmConfigurations,

    -- ** DescribeAccountAttributes
    module Network.AWS.Redshift.DescribeAccountAttributes,

    -- ** CreateSnapshotCopyGrant
    module Network.AWS.Redshift.CreateSnapshotCopyGrant,

    -- ** CopyClusterSnapshot
    module Network.AWS.Redshift.CopyClusterSnapshot,

    -- ** DeleteHsmClientCertificate
    module Network.AWS.Redshift.DeleteHsmClientCertificate,

    -- ** ModifyClusterSnapshotSchedule
    module Network.AWS.Redshift.ModifyClusterSnapshotSchedule,

    -- ** DeleteSnapshotCopyGrant
    module Network.AWS.Redshift.DeleteSnapshotCopyGrant,

    -- ** DescribeClusterVersions (Paginated)
    module Network.AWS.Redshift.DescribeClusterVersions,

    -- ** ModifyClusterSubnetGroup
    module Network.AWS.Redshift.ModifyClusterSubnetGroup,

    -- ** DescribeUsageLimits (Paginated)
    module Network.AWS.Redshift.DescribeUsageLimits,

    -- ** ModifySnapshotSchedule
    module Network.AWS.Redshift.ModifySnapshotSchedule,

    -- ** RotateEncryptionKey
    module Network.AWS.Redshift.RotateEncryptionKey,

    -- ** DescribeSnapshotCopyGrants (Paginated)
    module Network.AWS.Redshift.DescribeSnapshotCopyGrants,

    -- * Types

    -- ** Common
    module Network.AWS.Redshift.Internal,

    -- ** ResizeProgressMessage
    ResizeProgressMessage (..),
    mkResizeProgressMessage,
    rpmAvgResizeRateInMegaBytesPerSecond,
    rpmDataTransferProgressPercent,
    rpmElapsedTimeInSeconds,
    rpmEstimatedTimeToCompletionInSeconds,
    rpmImportTablesCompleted,
    rpmImportTablesInProgress,
    rpmImportTablesNotStarted,
    rpmMessage,
    rpmProgressInMegaBytes,
    rpmResizeType,
    rpmStatus,
    rpmTargetClusterType,
    rpmTargetEncryptionType,
    rpmTargetNodeType,
    rpmTargetNumberOfNodes,
    rpmTotalResizeDataInMegaBytes,

    -- ** Snapshot
    Snapshot (..),
    mkSnapshot,
    sAccountsWithRestoreAccess,
    sActualIncrementalBackupSizeInMegaBytes,
    sAvailabilityZone,
    sBackupProgressInMegaBytes,
    sClusterCreateTime,
    sClusterIdentifier,
    sClusterVersion,
    sCurrentBackupRateInMegaBytesPerSecond,
    sDBName,
    sElapsedTimeInSeconds,
    sEncrypted,
    sEncryptedWithHSM,
    sEnhancedVpcRouting,
    sEstimatedSecondsToCompletion,
    sKmsKeyId,
    sMaintenanceTrackName,
    sManualSnapshotRemainingDays,
    sManualSnapshotRetentionPeriod,
    sMasterUsername,
    sNodeType,
    sNumberOfNodes,
    sOwnerAccount,
    sPort,
    sRestorableNodeTypes,
    sSnapshotCreateTime,
    sSnapshotIdentifier,
    sSnapshotRetentionStartTime,
    sSnapshotType,
    sSourceRegion,
    sStatus,
    sTags,
    sTotalBackupSizeInMegaBytes,
    sVpcId,

    -- ** ClusterParameterGroup
    ClusterParameterGroup (..),
    mkClusterParameterGroup,
    cpgDescription,
    cpgParameterGroupFamily,
    cpgParameterGroupName,
    cpgTags,

    -- ** UsageLimitFeatureType
    UsageLimitFeatureType (..),

    -- ** ReservedNodeOfferingType
    ReservedNodeOfferingType (..),

    -- ** ResizeInfo
    ResizeInfo (..),
    mkResizeInfo,
    riAllowCancelResize,
    riResizeType,

    -- ** DeleteClusterSnapshotMessage
    DeleteClusterSnapshotMessage (..),
    mkDeleteClusterSnapshotMessage,
    dcsmSnapshotIdentifier,
    dcsmSnapshotClusterIdentifier,

    -- ** RestoreStatus
    RestoreStatus (..),
    mkRestoreStatus,
    rsCurrentRestoreRateInMegaBytesPerSecond,
    rsElapsedTimeInSeconds,
    rsEstimatedTimeToCompletionInSeconds,
    rsProgressInMegaBytes,
    rsSnapshotSizeInMegaBytes,
    rsStatus,

    -- ** Event
    Event (..),
    mkEvent,
    eDate,
    eEventCategories,
    eEventId,
    eMessage,
    eSeverity,
    eSourceIdentifier,
    eSourceType,

    -- ** ScheduledActionFilter
    ScheduledActionFilter (..),
    mkScheduledActionFilter,
    safName,
    safValues,

    -- ** ClusterSnapshotCopyStatus
    ClusterSnapshotCopyStatus (..),
    mkClusterSnapshotCopyStatus,
    cscsDestinationRegion,
    cscsManualSnapshotRetentionPeriod,
    cscsRetentionPeriod,
    cscsSnapshotCopyGrantName,

    -- ** ScheduledActionType
    ScheduledActionType (..),
    mkScheduledActionType,
    satPauseCluster,
    satResizeCluster,
    satResumeCluster,

    -- ** SortByOrder
    SortByOrder (..),

    -- ** NodeConfigurationOptionsFilterName
    NodeConfigurationOptionsFilterName (..),

    -- ** SnapshotCopyGrant
    SnapshotCopyGrant (..),
    mkSnapshotCopyGrant,
    scgKmsKeyId,
    scgSnapshotCopyGrantName,
    scgTags,

    -- ** ScheduledActionTypeValues
    ScheduledActionTypeValues (..),

    -- ** Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- ** HsmClientCertificate
    HsmClientCertificate (..),
    mkHsmClientCertificate,
    hccHsmClientCertificateIdentifier,
    hccHsmClientCertificatePublicKey,
    hccTags,

    -- ** Cluster
    Cluster (..),
    mkCluster,
    cAllowVersionUpgrade,
    cAutomatedSnapshotRetentionPeriod,
    cAvailabilityZone,
    cClusterAvailabilityStatus,
    cClusterCreateTime,
    cClusterIdentifier,
    cClusterNamespaceArn,
    cClusterNodes,
    cClusterParameterGroups,
    cClusterPublicKey,
    cClusterRevisionNumber,
    cClusterSecurityGroups,
    cClusterSnapshotCopyStatus,
    cClusterStatus,
    cClusterSubnetGroupName,
    cClusterVersion,
    cDBName,
    cDataTransferProgress,
    cDeferredMaintenanceWindows,
    cElasticIpStatus,
    cElasticResizeNumberOfNodeOptions,
    cEncrypted,
    cEndpoint,
    cEnhancedVpcRouting,
    cExpectedNextSnapshotScheduleTime,
    cExpectedNextSnapshotScheduleTimeStatus,
    cHsmStatus,
    cIamRoles,
    cKmsKeyId,
    cMaintenanceTrackName,
    cManualSnapshotRetentionPeriod,
    cMasterUsername,
    cModifyStatus,
    cNextMaintenanceWindowStartTime,
    cNodeType,
    cNumberOfNodes,
    cPendingActions,
    cPendingModifiedValues,
    cPreferredMaintenanceWindow,
    cPubliclyAccessible,
    cResizeInfo,
    cRestoreStatus,
    cSnapshotScheduleIdentifier,
    cSnapshotScheduleState,
    cTags,
    cVpcId,
    cVpcSecurityGroups,

    -- ** ClusterNode
    ClusterNode (..),
    mkClusterNode,
    cnNodeRole,
    cnPrivateIPAddress,
    cnPublicIPAddress,

    -- ** RevisionTarget
    RevisionTarget (..),
    mkRevisionTarget,
    rtDatabaseRevision,
    rtDatabaseRevisionReleaseDate,
    rtDescription,

    -- ** TableRestoreStatus
    TableRestoreStatus (..),
    mkTableRestoreStatus,
    trsClusterIdentifier,
    trsMessage,
    trsNewTableName,
    trsProgressInMegaBytes,
    trsRequestTime,
    trsSnapshotIdentifier,
    trsSourceDatabaseName,
    trsSourceSchemaName,
    trsSourceTableName,
    trsStatus,
    trsTableRestoreRequestId,
    trsTargetDatabaseName,
    trsTargetSchemaName,
    trsTotalDataInMegaBytes,

    -- ** SnapshotSortingEntity
    SnapshotSortingEntity (..),
    mkSnapshotSortingEntity,
    sseAttribute,
    sseSortOrder,

    -- ** ParameterApplyType
    ParameterApplyType (..),

    -- ** EC2SecurityGroup
    EC2SecurityGroup (..),
    mkEC2SecurityGroup,
    ecsgEC2SecurityGroupName,
    ecsgEC2SecurityGroupOwnerId,
    ecsgStatus,
    ecsgTags,

    -- ** String
    String (..),

    -- ** OrderableClusterOption
    OrderableClusterOption (..),
    mkOrderableClusterOption,
    ocoAvailabilityZones,
    ocoClusterType,
    ocoClusterVersion,
    ocoNodeType,

    -- ** SourceType
    SourceType (..),

    -- ** ScheduledActionState
    ScheduledActionState (..),

    -- ** UpdateTarget
    UpdateTarget (..),
    mkUpdateTarget,
    utDatabaseVersion,
    utMaintenanceTrackName,
    utSupportedOperations,

    -- ** ResumeClusterMessage
    ResumeClusterMessage (..),
    mkResumeClusterMessage,
    rClusterIdentifier,

    -- ** ClusterParameterGroupStatus
    ClusterParameterGroupStatus (..),
    mkClusterParameterGroupStatus,
    cpgsClusterParameterStatusList,
    cpgsParameterApplyStatus,
    cpgsParameterGroupName,

    -- ** Subnet
    Subnet (..),
    mkSubnet,
    sSubnetAvailabilityZone,
    sSubnetIdentifier,
    sSubnetStatus,

    -- ** ClusterSecurityGroup
    ClusterSecurityGroup (..),
    mkClusterSecurityGroup,
    csgfClusterSecurityGroupName,
    csgfDescription,
    csgfEC2SecurityGroups,
    csgfIPRanges,
    csgfTags,

    -- ** DefaultClusterParameters
    DefaultClusterParameters (..),
    mkDefaultClusterParameters,
    dcpMarker,
    dcpParameterGroupFamily,
    dcpParameters,

    -- ** NodeConfigurationOption
    NodeConfigurationOption (..),
    mkNodeConfigurationOption,
    ncoEstimatedDiskUtilizationPercent,
    ncoMode,
    ncoNodeType,
    ncoNumberOfNodes,

    -- ** PauseClusterMessage
    PauseClusterMessage (..),
    mkPauseClusterMessage,
    pcmClusterIdentifier,

    -- ** Mode
    Mode (..),

    -- ** AttributeValueTarget
    AttributeValueTarget (..),
    mkAttributeValueTarget,
    avtAttributeValue,

    -- ** ClusterSubnetGroup
    ClusterSubnetGroup (..),
    mkClusterSubnetGroup,
    csgClusterSubnetGroupName,
    csgDescription,
    csgSubnetGroupStatus,
    csgSubnets,
    csgTags,
    csgVpcId,

    -- ** DataTransferProgress
    DataTransferProgress (..),
    mkDataTransferProgress,
    dtpCurrentRateInMegaBytesPerSecond,
    dtpDataTransferredInMegaBytes,
    dtpElapsedTimeInSeconds,
    dtpEstimatedTimeToCompletionInSeconds,
    dtpStatus,
    dtpTotalDataInMegaBytes,

    -- ** EventInfoMap
    EventInfoMap (..),
    mkEventInfoMap,
    eimEventCategories,
    eimEventDescription,
    eimEventId,
    eimSeverity,

    -- ** SnapshotSchedule
    SnapshotSchedule (..),
    mkSnapshotSchedule,
    ssAssociatedClusterCount,
    ssAssociatedClusters,
    ssNextInvocations,
    ssScheduleDefinitions,
    ssScheduleDescription,
    ssScheduleIdentifier,
    ssTags,

    -- ** ClusterSecurityGroupMembership
    ClusterSecurityGroupMembership (..),
    mkClusterSecurityGroupMembership,
    csgmClusterSecurityGroupName,
    csgmStatus,

    -- ** ClusterAssociatedToSchedule
    ClusterAssociatedToSchedule (..),
    mkClusterAssociatedToSchedule,
    catsClusterIdentifier,
    catsScheduleAssociationState,

    -- ** ReservedNodeOffering
    ReservedNodeOffering (..),
    mkReservedNodeOffering,
    rnoCurrencyCode,
    rnoDuration,
    rnoFixedPrice,
    rnoNodeType,
    rnoOfferingType,
    rnoRecurringCharges,
    rnoReservedNodeOfferingId,
    rnoReservedNodeOfferingType,
    rnoUsagePrice,

    -- ** ReservedNode
    ReservedNode (..),
    mkReservedNode,
    rnCurrencyCode,
    rnDuration,
    rnFixedPrice,
    rnNodeCount,
    rnNodeType,
    rnOfferingType,
    rnRecurringCharges,
    rnReservedNodeId,
    rnReservedNodeOfferingId,
    rnReservedNodeOfferingType,
    rnStartTime,
    rnState,
    rnUsagePrice,

    -- ** ScheduledAction
    ScheduledAction (..),
    mkScheduledAction,
    saEndTime,
    saIamRole,
    saNextInvocations,
    saSchedule,
    saScheduledActionDescription,
    saScheduledActionName,
    saStartTime,
    saState,
    saTargetAction,

    -- ** SnapshotErrorMessage
    SnapshotErrorMessage (..),
    mkSnapshotErrorMessage,
    semFailureCode,
    semFailureReason,
    semSnapshotClusterIdentifier,
    semSnapshotIdentifier,

    -- ** LoggingStatus
    LoggingStatus (..),
    mkLoggingStatus,
    lsBucketName,
    lsLastFailureMessage,
    lsLastFailureTime,
    lsLastSuccessfulDeliveryTime,
    lsLoggingEnabled,
    lsS3KeyPrefix,

    -- ** AccountWithRestoreAccess
    AccountWithRestoreAccess (..),
    mkAccountWithRestoreAccess,
    awraAccountAlias,
    awraAccountId,

    -- ** ClusterParameterStatus
    ClusterParameterStatus (..),
    mkClusterParameterStatus,
    cpsParameterApplyErrorDescription,
    cpsParameterApplyStatus,
    cpsParameterName,

    -- ** AvailabilityZone
    AvailabilityZone (..),
    mkAvailabilityZone,
    azName,
    azSupportedPlatforms,

    -- ** ClusterDbRevision
    ClusterDbRevision (..),
    mkClusterDbRevision,
    cdrClusterIdentifier,
    cdrCurrentDatabaseRevision,
    cdrDatabaseRevisionReleaseDate,
    cdrRevisionTargets,

    -- ** EventSubscription
    EventSubscription (..),
    mkEventSubscription,
    esCustSubscriptionId,
    esCustomerAwsId,
    esEnabled,
    esEventCategoriesList,
    esSeverity,
    esSnsTopicArn,
    esSourceIdsList,
    esSourceType,
    esStatus,
    esSubscriptionCreationTime,
    esTags,

    -- ** HsmStatus
    HsmStatus (..),
    mkHsmStatus,
    hsHsmClientCertificateIdentifier,
    hsHsmConfigurationIdentifier,
    hsStatus,

    -- ** DeferredMaintenanceWindow
    DeferredMaintenanceWindow (..),
    mkDeferredMaintenanceWindow,
    dmwDeferMaintenanceEndTime,
    dmwDeferMaintenanceIdentifier,
    dmwDeferMaintenanceStartTime,

    -- ** MaintenanceTrack
    MaintenanceTrack (..),
    mkMaintenanceTrack,
    mtDatabaseVersion,
    mtMaintenanceTrackName,
    mtUpdateTargets,

    -- ** SnapshotAttributeToSortBy
    SnapshotAttributeToSortBy (..),

    -- ** ScheduleState
    ScheduleState (..),

    -- ** TableRestoreStatusType
    TableRestoreStatusType (..),

    -- ** UsageLimitLimitType
    UsageLimitLimitType (..),

    -- ** AccountAttribute
    AccountAttribute (..),
    mkAccountAttribute,
    aaAttributeName,
    aaAttributeValues,

    -- ** UsageLimitPeriod
    UsageLimitPeriod (..),

    -- ** UsageLimit
    UsageLimit (..),
    mkUsageLimit,
    ulAmount,
    ulBreachAction,
    ulClusterIdentifier,
    ulFeatureType,
    ulLimitType,
    ulPeriod,
    ulTags,
    ulUsageLimitId,

    -- ** NodeConfigurationOptionsFilter
    NodeConfigurationOptionsFilter (..),
    mkNodeConfigurationOptionsFilter,
    ncofName,
    ncofOperator,
    ncofValues,

    -- ** ScheduledActionFilterName
    ScheduledActionFilterName (..),

    -- ** ClusterParameterGroupNameMessage
    ClusterParameterGroupNameMessage (..),
    mkClusterParameterGroupNameMessage,
    cpgnmParameterGroupName,
    cpgnmParameterGroupStatus,

    -- ** OperatorType
    OperatorType (..),

    -- ** ElasticIpStatus
    ElasticIpStatus (..),
    mkElasticIpStatus,
    eisElasticIp,
    eisStatus,

    -- ** ClusterVersion
    ClusterVersion (..),
    mkClusterVersion,
    cvClusterParameterGroupFamily,
    cvClusterVersion,
    cvDescription,

    -- ** RecurringCharge
    RecurringCharge (..),
    mkRecurringCharge,
    rcRecurringChargeAmount,
    rcRecurringChargeFrequency,

    -- ** SupportedOperation
    SupportedOperation (..),
    mkSupportedOperation,
    soOperationName,

    -- ** ClusterIamRole
    ClusterIamRole (..),
    mkClusterIamRole,
    cirApplyStatus,
    cirIamRoleArn,

    -- ** ResizeClusterMessage
    ResizeClusterMessage (..),
    mkResizeClusterMessage,
    rcmClusterIdentifier,
    rcmClassic,
    rcmClusterType,
    rcmNodeType,
    rcmNumberOfNodes,

    -- ** Endpoint
    Endpoint (..),
    mkEndpoint,
    eAddress,
    ePort,

    -- ** IPRange
    IPRange (..),
    mkIPRange,
    iprCIDRIP,
    iprStatus,
    iprTags,

    -- ** TaggedResource
    TaggedResource (..),
    mkTaggedResource,
    trResourceName,
    trResourceType,
    trTag,

    -- ** EventCategoriesMap
    EventCategoriesMap (..),
    mkEventCategoriesMap,
    ecmEvents,
    ecmSourceType,

    -- ** ActionType
    ActionType (..),

    -- ** HsmConfiguration
    HsmConfiguration (..),
    mkHsmConfiguration,
    hcDescription,
    hcHsmConfigurationIdentifier,
    hcHsmIpAddress,
    hcHsmPartitionName,
    hcTags,

    -- ** PendingModifiedValues
    PendingModifiedValues (..),
    mkPendingModifiedValues,
    pmvAutomatedSnapshotRetentionPeriod,
    pmvClusterIdentifier,
    pmvClusterType,
    pmvClusterVersion,
    pmvEncryptionType,
    pmvEnhancedVpcRouting,
    pmvMaintenanceTrackName,
    pmvMasterUserPassword,
    pmvNodeType,
    pmvNumberOfNodes,
    pmvPubliclyAccessible,

    -- ** VpcSecurityGroupMembership
    VpcSecurityGroupMembership (..),
    mkVpcSecurityGroupMembership,
    vsgmStatus,
    vsgmVpcSecurityGroupId,

    -- ** SupportedPlatform
    SupportedPlatform (..),
    mkSupportedPlatform,
    spName,

    -- ** Parameter
    Parameter (..),
    mkParameter,
    pAllowedValues,
    pApplyType,
    pDataType,
    pDescription,
    pIsModifiable,
    pMinimumEngineVersion,
    pParameterName,
    pParameterValue,
    pSource,

    -- ** UsageLimitBreachAction
    UsageLimitBreachAction (..),

    -- ** ClusterSubnetGroupName
    ClusterSubnetGroupName (..),

    -- ** SubscriptionName
    SubscriptionName (..),

    -- ** Severity
    Severity (..),

    -- ** SnsTopicArn
    SnsTopicArn (..),

    -- ** Message
    Message (..),

    -- ** ResizeType
    ResizeType (..),

    -- ** Status
    Status (..),

    -- ** TargetClusterType
    TargetClusterType (..),

    -- ** TargetEncryptionType
    TargetEncryptionType (..),

    -- ** TargetNodeType
    TargetNodeType (..),

    -- ** ReservedNodeOfferingId
    ReservedNodeOfferingId (..),

    -- ** ClusterIdentifier
    ClusterIdentifier (..),

    -- ** Marker
    Marker (..),

    -- ** ScheduleIdentifier
    ScheduleIdentifier (..),

    -- ** DBName
    DBName (..),

    -- ** KmsKeyId
    KmsKeyId (..),

    -- ** MaintenanceTrackName
    MaintenanceTrackName (..),

    -- ** MasterUsername
    MasterUsername (..),

    -- ** NodeType
    NodeType (..),

    -- ** OwnerAccount
    OwnerAccount (..),

    -- ** SnapshotIdentifier
    SnapshotIdentifier (..),

    -- ** SnapshotType
    SnapshotType (..),

    -- ** SourceRegion
    SourceRegion (..),

    -- ** VpcId
    VpcId (..),

    -- ** Description
    Description (..),

    -- ** ParameterGroupFamily
    ParameterGroupFamily (..),

    -- ** ParameterGroupName
    ParameterGroupName (..),

    -- ** SnapshotClusterIdentifier
    SnapshotClusterIdentifier (..),

    -- ** ScheduledActionName
    ScheduledActionName (..),

    -- ** IamRole
    IamRole (..),

    -- ** Schedule
    Schedule (..),

    -- ** ScheduledActionDescription
    ScheduledActionDescription (..),

    -- ** EventId
    EventId (..),

    -- ** SourceIdentifier
    SourceIdentifier (..),

    -- ** HsmClientCertificateIdentifier
    HsmClientCertificateIdentifier (..),

    -- ** DestinationRegion
    DestinationRegion (..),

    -- ** SnapshotCopyGrantName
    SnapshotCopyGrantName (..),

    -- ** ReservedNodeId
    ReservedNodeId (..),

    -- ** ClusterSecurityGroupName
    ClusterSecurityGroupName (..),

    -- ** CIDRIP
    CIDRIP (..),

    -- ** EC2SecurityGroupName
    EC2SecurityGroupName (..),

    -- ** EC2SecurityGroupOwnerId
    EC2SecurityGroupOwnerId (..),

    -- ** DbUser
    DbUser (..),

    -- ** DbName
    DbName (..),

    -- ** Key
    Key (..),

    -- ** Value
    Value (..),

    -- ** HsmClientCertificatePublicKey
    HsmClientCertificatePublicKey (..),

    -- ** UsageLimitId
    UsageLimitId (..),

    -- ** ClusterAvailabilityStatus
    ClusterAvailabilityStatus (..),

    -- ** ClusterNamespaceArn
    ClusterNamespaceArn (..),

    -- ** ClusterPublicKey
    ClusterPublicKey (..),

    -- ** ClusterRevisionNumber
    ClusterRevisionNumber (..),

    -- ** ClusterStatus
    ClusterStatus (..),

    -- ** ElasticResizeNumberOfNodeOptions
    ElasticResizeNumberOfNodeOptions (..),

    -- ** ExpectedNextSnapshotScheduleTimeStatus
    ExpectedNextSnapshotScheduleTimeStatus (..),

    -- ** ModifyStatus
    ModifyStatus (..),

    -- ** PreferredMaintenanceWindow
    PreferredMaintenanceWindow (..),

    -- ** SnapshotScheduleIdentifier
    SnapshotScheduleIdentifier (..),

    -- ** NodeRole
    NodeRole (..),

    -- ** PrivateIPAddress
    PrivateIPAddress (..),

    -- ** PublicIPAddress
    PublicIPAddress (..),

    -- ** DatabaseRevision
    DatabaseRevision (..),

    -- ** DbPassword
    DbPassword (..),

    -- ** NewTableName
    NewTableName (..),

    -- ** SourceDatabaseName
    SourceDatabaseName (..),

    -- ** SourceSchemaName
    SourceSchemaName (..),

    -- ** SourceTableName
    SourceTableName (..),

    -- ** TableRestoreRequestId
    TableRestoreRequestId (..),

    -- ** TargetDatabaseName
    TargetDatabaseName (..),

    -- ** TargetSchemaName
    TargetSchemaName (..),

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.UTCTime,
    Lude.NominalDiffTime,
  )
where

import qualified Network.AWS.Prelude as Lude
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
