{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
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
    redshiftService,

    -- * Errors
    -- $errors

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

    -- ** ModifyClusterDBRevision
    module Network.AWS.Redshift.ModifyClusterDBRevision,

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

    -- ** ModifyClusterIAMRoles
    module Network.AWS.Redshift.ModifyClusterIAMRoles,

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

    -- ** CreateHSMClientCertificate
    module Network.AWS.Redshift.CreateHSMClientCertificate,

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

    -- ** DescribeClusterDBRevisions (Paginated)
    module Network.AWS.Redshift.DescribeClusterDBRevisions,

    -- ** BatchModifyClusterSnapshots
    module Network.AWS.Redshift.BatchModifyClusterSnapshots,

    -- ** DeleteUsageLimit
    module Network.AWS.Redshift.DeleteUsageLimit,

    -- ** RevokeClusterSecurityGroupIngress
    module Network.AWS.Redshift.RevokeClusterSecurityGroupIngress,

    -- ** DescribeHSMClientCertificates (Paginated)
    module Network.AWS.Redshift.DescribeHSMClientCertificates,

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

    -- ** DeleteHSMConfiguration
    module Network.AWS.Redshift.DeleteHSMConfiguration,

    -- ** AcceptReservedNodeExchange
    module Network.AWS.Redshift.AcceptReservedNodeExchange,

    -- ** AuthorizeClusterSecurityGroupIngress
    module Network.AWS.Redshift.AuthorizeClusterSecurityGroupIngress,

    -- ** DescribeTableRestoreStatus (Paginated)
    module Network.AWS.Redshift.DescribeTableRestoreStatus,

    -- ** CreateClusterSnapshot
    module Network.AWS.Redshift.CreateClusterSnapshot,

    -- ** CreateHSMConfiguration
    module Network.AWS.Redshift.CreateHSMConfiguration,

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

    -- ** DescribeHSMConfigurations (Paginated)
    module Network.AWS.Redshift.DescribeHSMConfigurations,

    -- ** DescribeAccountAttributes
    module Network.AWS.Redshift.DescribeAccountAttributes,

    -- ** CreateSnapshotCopyGrant
    module Network.AWS.Redshift.CreateSnapshotCopyGrant,

    -- ** CopyClusterSnapshot
    module Network.AWS.Redshift.CopyClusterSnapshot,

    -- ** DeleteHSMClientCertificate
    module Network.AWS.Redshift.DeleteHSMClientCertificate,

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
    AccountAttribute (..),
    mkAccountAttribute,
    aaAttributeValues,
    aaAttributeName,

    -- ** AccountWithRestoreAccess
    AccountWithRestoreAccess (..),
    mkAccountWithRestoreAccess,
    awraAccountAlias,
    awraAccountId,

    -- ** AttributeValueTarget
    AttributeValueTarget (..),
    mkAttributeValueTarget,
    avtAttributeValue,

    -- ** AvailabilityZone
    AvailabilityZone (..),
    mkAvailabilityZone,
    azName,
    azSupportedPlatforms,

    -- ** Cluster
    Cluster (..),
    mkCluster,
    cResizeInfo,
    cRestoreStatus,
    cManualSnapshotRetentionPeriod,
    cEnhancedVPCRouting,
    cClusterSnapshotCopyStatus,
    cClusterAvailabilityStatus,
    cClusterRevisionNumber,
    cSnapshotScheduleIdentifier,
    cPubliclyAccessible,
    cMasterUsername,
    cMaintenanceTrackName,
    cExpectedNextSnapshotScheduleTime,
    cElasticResizeNumberOfNodeOptions,
    cVPCId,
    cClusterSecurityGroups,
    cAutomatedSnapshotRetentionPeriod,
    cSnapshotScheduleState,
    cDataTransferProgress,
    cEncrypted,
    cClusterSubnetGroupName,
    cExpectedNextSnapshotScheduleTimeStatus,
    cClusterIdentifier,
    cDeferredMaintenanceWindows,
    cNumberOfNodes,
    cClusterPublicKey,
    cPreferredMaintenanceWindow,
    cModifyStatus,
    cClusterNamespaceARN,
    cKMSKeyId,
    cClusterParameterGroups,
    cAvailabilityZone,
    cVPCSecurityGroups,
    cHSMStatus,
    cIAMRoles,
    cPendingActions,
    cElasticIPStatus,
    cClusterVersion,
    cNodeType,
    cNextMaintenanceWindowStartTime,
    cClusterCreateTime,
    cEndpoint,
    cAllowVersionUpgrade,
    cClusterStatus,
    cPendingModifiedValues,
    cTags,
    cClusterNodes,
    cDBName,

    -- ** ClusterAssociatedToSchedule
    ClusterAssociatedToSchedule (..),
    mkClusterAssociatedToSchedule,
    catsScheduleAssociationState,
    catsClusterIdentifier,

    -- ** ClusterDBRevision
    ClusterDBRevision (..),
    mkClusterDBRevision,
    cdrDatabaseRevisionReleaseDate,
    cdrClusterIdentifier,
    cdrCurrentDatabaseRevision,
    cdrRevisionTargets,

    -- ** ClusterIAMRole
    ClusterIAMRole (..),
    mkClusterIAMRole,
    cirIAMRoleARN,
    cirApplyStatus,

    -- ** ClusterNode
    ClusterNode (..),
    mkClusterNode,
    cnNodeRole,
    cnPrivateIPAddress,
    cnPublicIPAddress,

    -- ** ClusterParameterGroup
    ClusterParameterGroup (..),
    mkClusterParameterGroup,
    cpgParameterGroupFamily,
    cpgDescription,
    cpgTags,
    cpgParameterGroupName,

    -- ** ClusterParameterGroupNameMessage
    ClusterParameterGroupNameMessage (..),
    mkClusterParameterGroupNameMessage,
    cpgnmParameterGroupStatus,
    cpgnmParameterGroupName,

    -- ** ClusterParameterGroupStatus
    ClusterParameterGroupStatus (..),
    mkClusterParameterGroupStatus,
    cpgsClusterParameterStatusList,
    cpgsParameterApplyStatus,
    cpgsParameterGroupName,

    -- ** ClusterParameterStatus
    ClusterParameterStatus (..),
    mkClusterParameterStatus,
    cpsParameterApplyErrorDescription,
    cpsParameterName,
    cpsParameterApplyStatus,

    -- ** ClusterSecurityGroup
    ClusterSecurityGroup (..),
    mkClusterSecurityGroup,
    cluClusterSecurityGroupName,
    cluIPRanges,
    cluEC2SecurityGroups,
    cluDescription,
    cluTags,

    -- ** ClusterSecurityGroupMembership
    ClusterSecurityGroupMembership (..),
    mkClusterSecurityGroupMembership,
    csgmStatus,
    csgmClusterSecurityGroupName,

    -- ** ClusterSnapshotCopyStatus
    ClusterSnapshotCopyStatus (..),
    mkClusterSnapshotCopyStatus,
    cscsManualSnapshotRetentionPeriod,
    cscsRetentionPeriod,
    cscsDestinationRegion,
    cscsSnapshotCopyGrantName,

    -- ** ClusterSubnetGroup
    ClusterSubnetGroup (..),
    mkClusterSubnetGroup,
    csgVPCId,
    csgSubnets,
    csgClusterSubnetGroupName,
    csgSubnetGroupStatus,
    csgDescription,
    csgTags,

    -- ** ClusterVersion
    ClusterVersion (..),
    mkClusterVersion,
    cvClusterParameterGroupFamily,
    cvClusterVersion,
    cvDescription,

    -- ** DataTransferProgress
    DataTransferProgress (..),
    mkDataTransferProgress,
    dtpCurrentRateInMegaBytesPerSecond,
    dtpStatus,
    dtpEstimatedTimeToCompletionInSeconds,
    dtpDataTransferredInMegaBytes,
    dtpTotalDataInMegaBytes,
    dtpElapsedTimeInSeconds,

    -- ** DefaultClusterParameters
    DefaultClusterParameters (..),
    mkDefaultClusterParameters,
    dcpMarker,
    dcpParameters,
    dcpParameterGroupFamily,

    -- ** DeferredMaintenanceWindow
    DeferredMaintenanceWindow (..),
    mkDeferredMaintenanceWindow,
    dmwDeferMaintenanceEndTime,
    dmwDeferMaintenanceStartTime,
    dmwDeferMaintenanceIdentifier,

    -- ** DeleteClusterSnapshotMessage
    DeleteClusterSnapshotMessage (..),
    mkDeleteClusterSnapshotMessage,
    dcsmSnapshotClusterIdentifier,
    dcsmSnapshotIdentifier,

    -- ** EC2SecurityGroup
    EC2SecurityGroup (..),
    mkEC2SecurityGroup,
    esgStatus,
    esgEC2SecurityGroupOwnerId,
    esgEC2SecurityGroupName,
    esgTags,

    -- ** ElasticIPStatus
    ElasticIPStatus (..),
    mkElasticIPStatus,
    eisStatus,
    eisElasticIP,

    -- ** Endpoint
    Endpoint (..),
    mkEndpoint,
    eAddress,
    ePort,

    -- ** Event
    Event (..),
    mkEvent,
    eSourceType,
    eSeverity,
    eSourceIdentifier,
    eDate,
    eEventCategories,
    eMessage,
    eEventId,

    -- ** EventCategoriesMap
    EventCategoriesMap (..),
    mkEventCategoriesMap,
    ecmSourceType,
    ecmEvents,

    -- ** EventInfoMap
    EventInfoMap (..),
    mkEventInfoMap,
    eimEventDescription,
    eimSeverity,
    eimEventCategories,
    eimEventId,

    -- ** EventSubscription
    EventSubscription (..),
    mkEventSubscription,
    esStatus,
    esCustomerAWSId,
    esCustSubscriptionId,
    esSNSTopicARN,
    esEnabled,
    esSourceType,
    esSeverity,
    esSubscriptionCreationTime,
    esEventCategoriesList,
    esTags,
    esSourceIdsList,

    -- ** HSMClientCertificate
    HSMClientCertificate (..),
    mkHSMClientCertificate,
    hccHSMClientCertificateIdentifier,
    hccHSMClientCertificatePublicKey,
    hccTags,

    -- ** HSMConfiguration
    HSMConfiguration (..),
    mkHSMConfiguration,
    hcHSMConfigurationIdentifier,
    hcHSMPartitionName,
    hcDescription,
    hcTags,
    hcHSMIPAddress,

    -- ** HSMStatus
    HSMStatus (..),
    mkHSMStatus,
    hsStatus,
    hsHSMConfigurationIdentifier,
    hsHSMClientCertificateIdentifier,

    -- ** IPRange
    IPRange (..),
    mkIPRange,
    irStatus,
    irCIdRIP,
    irTags,

    -- ** LoggingStatus
    LoggingStatus (..),
    mkLoggingStatus,
    lsLastFailureTime,
    lsLastSuccessfulDeliveryTime,
    lsS3KeyPrefix,
    lsBucketName,
    lsLoggingEnabled,
    lsLastFailureMessage,

    -- ** MaintenanceTrack
    MaintenanceTrack (..),
    mkMaintenanceTrack,
    mtDatabaseVersion,
    mtMaintenanceTrackName,
    mtUpdateTargets,

    -- ** NodeConfigurationOption
    NodeConfigurationOption (..),
    mkNodeConfigurationOption,
    ncoMode,
    ncoNumberOfNodes,
    ncoNodeType,
    ncoEstimatedDiskUtilizationPercent,

    -- ** NodeConfigurationOptionsFilter
    NodeConfigurationOptionsFilter (..),
    mkNodeConfigurationOptionsFilter,
    ncofValues,
    ncofOperator,
    ncofName,

    -- ** OrderableClusterOption
    OrderableClusterOption (..),
    mkOrderableClusterOption,
    ocoAvailabilityZones,
    ocoClusterType,
    ocoClusterVersion,
    ocoNodeType,

    -- ** Parameter
    Parameter (..),
    mkParameter,
    pApplyType,
    pParameterValue,
    pMinimumEngineVersion,
    pSource,
    pIsModifiable,
    pDataType,
    pAllowedValues,
    pParameterName,
    pDescription,

    -- ** PauseClusterMessage
    PauseClusterMessage (..),
    mkPauseClusterMessage,
    pcmClusterIdentifier,

    -- ** PendingModifiedValues
    PendingModifiedValues (..),
    mkPendingModifiedValues,
    pmvEncryptionType,
    pmvEnhancedVPCRouting,
    pmvMasterUserPassword,
    pmvPubliclyAccessible,
    pmvMaintenanceTrackName,
    pmvAutomatedSnapshotRetentionPeriod,
    pmvClusterIdentifier,
    pmvNumberOfNodes,
    pmvClusterType,
    pmvClusterVersion,
    pmvNodeType,

    -- ** RecurringCharge
    RecurringCharge (..),
    mkRecurringCharge,
    rcRecurringChargeFrequency,
    rcRecurringChargeAmount,

    -- ** ReservedNode
    ReservedNode (..),
    mkReservedNode,
    rnReservedNodeOfferingType,
    rnState,
    rnCurrencyCode,
    rnStartTime,
    rnNodeCount,
    rnReservedNodeId,
    rnReservedNodeOfferingId,
    rnRecurringCharges,
    rnOfferingType,
    rnUsagePrice,
    rnNodeType,
    rnFixedPrice,
    rnDuration,

    -- ** ReservedNodeOffering
    ReservedNodeOffering (..),
    mkReservedNodeOffering,
    rnoReservedNodeOfferingType,
    rnoCurrencyCode,
    rnoReservedNodeOfferingId,
    rnoRecurringCharges,
    rnoOfferingType,
    rnoUsagePrice,
    rnoNodeType,
    rnoFixedPrice,
    rnoDuration,

    -- ** ResizeClusterMessage
    ResizeClusterMessage (..),
    mkResizeClusterMessage,
    rcmNumberOfNodes,
    rcmClassic,
    rcmClusterType,
    rcmNodeType,
    rcmClusterIdentifier,

    -- ** ResizeInfo
    ResizeInfo (..),
    mkResizeInfo,
    riAllowCancelResize,
    riResizeType,

    -- ** ResizeProgressMessage
    ResizeProgressMessage (..),
    mkResizeProgressMessage,
    rpmImportTablesNotStarted,
    rpmStatus,
    rpmEstimatedTimeToCompletionInSeconds,
    rpmAvgResizeRateInMegaBytesPerSecond,
    rpmTargetNumberOfNodes,
    rpmTargetEncryptionType,
    rpmTargetNodeType,
    rpmImportTablesInProgress,
    rpmResizeType,
    rpmImportTablesCompleted,
    rpmProgressInMegaBytes,
    rpmDataTransferProgressPercent,
    rpmTotalResizeDataInMegaBytes,
    rpmTargetClusterType,
    rpmMessage,
    rpmElapsedTimeInSeconds,

    -- ** RestoreStatus
    RestoreStatus (..),
    mkRestoreStatus,
    rsStatus,
    rsEstimatedTimeToCompletionInSeconds,
    rsCurrentRestoreRateInMegaBytesPerSecond,
    rsProgressInMegaBytes,
    rsElapsedTimeInSeconds,
    rsSnapshotSizeInMegaBytes,

    -- ** ResumeClusterMessage
    ResumeClusterMessage (..),
    mkResumeClusterMessage,
    rClusterIdentifier,

    -- ** RevisionTarget
    RevisionTarget (..),
    mkRevisionTarget,
    rtDatabaseRevisionReleaseDate,
    rtDatabaseRevision,
    rtDescription,

    -- ** ScheduledAction
    ScheduledAction (..),
    mkScheduledAction,
    saState,
    saTargetAction,
    saStartTime,
    saSchedule,
    saScheduledActionName,
    saScheduledActionDescription,
    saNextInvocations,
    saEndTime,
    saIAMRole,

    -- ** ScheduledActionFilter
    ScheduledActionFilter (..),
    mkScheduledActionFilter,
    safName,
    safValues,

    -- ** ScheduledActionType
    ScheduledActionType (..),
    mkScheduledActionType,
    satResizeCluster,
    satResumeCluster,
    satPauseCluster,

    -- ** Snapshot
    Snapshot (..),
    mkSnapshot,
    sStatus,
    sRestorableNodeTypes,
    sAccountsWithRestoreAccess,
    sManualSnapshotRetentionPeriod,
    sEnhancedVPCRouting,
    sSnapshotIdentifier,
    sEncryptedWithHSM,
    sMasterUsername,
    sSourceRegion,
    sMaintenanceTrackName,
    sSnapshotRetentionStartTime,
    sManualSnapshotRemainingDays,
    sVPCId,
    sBackupProgressInMegaBytes,
    sEncrypted,
    sClusterIdentifier,
    sNumberOfNodes,
    sSnapshotType,
    sKMSKeyId,
    sAvailabilityZone,
    sCurrentBackupRateInMegaBytesPerSecond,
    sSnapshotCreateTime,
    sClusterVersion,
    sOwnerAccount,
    sNodeType,
    sElapsedTimeInSeconds,
    sClusterCreateTime,
    sEstimatedSecondsToCompletion,
    sActualIncrementalBackupSizeInMegaBytes,
    sTags,
    sPort,
    sTotalBackupSizeInMegaBytes,
    sDBName,

    -- ** SnapshotCopyGrant
    SnapshotCopyGrant (..),
    mkSnapshotCopyGrant,
    scgKMSKeyId,
    scgSnapshotCopyGrantName,
    scgTags,

    -- ** SnapshotErrorMessage
    SnapshotErrorMessage (..),
    mkSnapshotErrorMessage,
    semFailureReason,
    semSnapshotIdentifier,
    semSnapshotClusterIdentifier,
    semFailureCode,

    -- ** SnapshotSchedule
    SnapshotSchedule (..),
    mkSnapshotSchedule,
    ssAssociatedClusters,
    ssNextInvocations,
    ssScheduleDefinitions,
    ssScheduleDescription,
    ssScheduleIdentifier,
    ssAssociatedClusterCount,
    ssTags,

    -- ** SnapshotSortingEntity
    SnapshotSortingEntity (..),
    mkSnapshotSortingEntity,
    sseSortOrder,
    sseAttribute,

    -- ** Subnet
    Subnet (..),
    mkSubnet,
    sSubnetStatus,
    sSubnetIdentifier,
    sSubnetAvailabilityZone,

    -- ** SupportedOperation
    SupportedOperation (..),
    mkSupportedOperation,
    soOperationName,

    -- ** SupportedPlatform
    SupportedPlatform (..),
    mkSupportedPlatform,
    spName,

    -- ** TableRestoreStatus
    TableRestoreStatus (..),
    mkTableRestoreStatus,
    trsStatus,
    trsTargetSchemaName,
    trsSnapshotIdentifier,
    trsSourceDatabaseName,
    trsTableRestoreRequestId,
    trsNewTableName,
    trsTargetDatabaseName,
    trsSourceSchemaName,
    trsClusterIdentifier,
    trsRequestTime,
    trsSourceTableName,
    trsTotalDataInMegaBytes,
    trsProgressInMegaBytes,
    trsMessage,

    -- ** Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- ** TaggedResource
    TaggedResource (..),
    mkTaggedResource,
    trTag,
    trResourceType,
    trResourceName,

    -- ** UpdateTarget
    UpdateTarget (..),
    mkUpdateTarget,
    utDatabaseVersion,
    utMaintenanceTrackName,
    utSupportedOperations,

    -- ** UsageLimit
    UsageLimit (..),
    mkUsageLimit,
    ulAmount,
    ulLimitType,
    ulUsageLimitId,
    ulPeriod,
    ulClusterIdentifier,
    ulBreachAction,
    ulFeatureType,
    ulTags,

    -- ** VPCSecurityGroupMembership
    VPCSecurityGroupMembership (..),
    mkVPCSecurityGroupMembership,
    vsgmStatus,
    vsgmVPCSecurityGroupId,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.DateTime,
    Lude.Timestamp,
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
import Network.AWS.Redshift.CreateHSMClientCertificate
import Network.AWS.Redshift.CreateHSMConfiguration
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
import Network.AWS.Redshift.DeleteHSMClientCertificate
import Network.AWS.Redshift.DeleteHSMConfiguration
import Network.AWS.Redshift.DeleteScheduledAction
import Network.AWS.Redshift.DeleteSnapshotCopyGrant
import Network.AWS.Redshift.DeleteSnapshotSchedule
import Network.AWS.Redshift.DeleteTags
import Network.AWS.Redshift.DeleteUsageLimit
import Network.AWS.Redshift.DescribeAccountAttributes
import Network.AWS.Redshift.DescribeClusterDBRevisions
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
import Network.AWS.Redshift.DescribeHSMClientCertificates
import Network.AWS.Redshift.DescribeHSMConfigurations
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
import Network.AWS.Redshift.ModifyClusterDBRevision
import Network.AWS.Redshift.ModifyClusterIAMRoles
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
