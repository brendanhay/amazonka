-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _ClusterSecurityGroupQuotaExceededFault,
    _InvalidS3KeyPrefixFault,
    _SourceNotFoundFault,
    _AuthorizationQuotaExceededFault,
    _CopyToRegionDisabledFault,
    _LimitExceededFault,
    _InvalidClusterSecurityGroupStateFault,
    _ClusterSecurityGroupAlreadyExistsFault,
    _ClusterSnapshotNotFoundFault,
    _InvalidElasticIpFault,
    _TableRestoreNotFoundFault,
    _HsmConfigurationNotFoundFault,
    _ScheduleDefinitionTypeUnsupportedFault,
    _AuthorizationAlreadyExistsFault,
    _SubscriptionCategoryNotFoundFault,
    _InvalidRetentionPeriodFault,
    _HsmConfigurationAlreadyExistsFault,
    _SubscriptionNotFoundFault,
    _InvalidS3BucketNameFault,
    _ClusterSnapshotAlreadyExistsFault,
    _InvalidSubnet,
    _TableLimitExceededFault,
    _InvalidHsmConfigurationStateFault,
    _SnapshotCopyAlreadyDisabledFault,
    _ClusterQuotaExceededFault,
    _HsmClientCertificateQuotaExceededFault,
    _SnapshotScheduleAlreadyExistsFault,
    _BatchModifyClusterSnapshotsLimitExceededFault,
    _ClusterParameterGroupNotFoundFault,
    _SnapshotCopyGrantQuotaExceededFault,
    _NumberOfNodesPerClusterLimitExceededFault,
    _SnapshotCopyAlreadyEnabledFault,
    _ClusterParameterGroupAlreadyExistsFault,
    _BatchDeleteRequestSizeExceededFault,
    _SnapshotCopyDisabledFault,
    _ResizeNotFoundFault,
    _HsmClientCertificateNotFoundFault,
    _SNSTopicArnNotFoundFault,
    _ClusterNotFoundFault,
    _ClusterParameterGroupQuotaExceededFault,
    _SnapshotCopyGrantAlreadyExistsFault,
    _SNSNoAuthorizationFault,
    _InvalidClusterStateFault,
    _InvalidTableRestoreArgumentFault,
    _SnapshotCopyGrantNotFoundFault,
    _InvalidScheduleFault,
    _InvalidClusterTrackFault,
    _HsmConfigurationQuotaExceededFault,
    _ClusterSnapshotQuotaExceededFault,
    _InsufficientClusterCapacityFault,
    _InvalidScheduledActionFault,
    _SNSInvalidTopicFault,
    _UsageLimitAlreadyExistsFault,
    _DependentServiceUnavailableFault,
    _UnsupportedOptionFault,
    _SubscriptionAlreadyExistFault,
    _InvalidClusterSnapshotScheduleStateFault,
    _InvalidVPCNetworkStateFault,
    _ClusterSubnetGroupNotFoundFault,
    _BucketNotFoundFault,
    _InvalidSubscriptionStateFault,
    _DependentServiceRequestThrottlingFault,
    _AuthorizationNotFoundFault,
    _InvalidClusterSubnetGroupStateFault,
    _UnsupportedOperationFault,
    _ClusterSubnetGroupAlreadyExistsFault,
    _InvalidClusterSnapshotStateFault,
    _ClusterSecurityGroupNotFoundFault,
    _ReservedNodeNotFoundFault,
    _ReservedNodeOfferingNotFoundFault,
    _InvalidClusterSubnetStateFault,
    _IncompatibleOrderableOptions,
    _ReservedNodeAlreadyMigratedFault,
    _EventSubscriptionQuotaExceededFault,
    _SnapshotScheduleNotFoundFault,
    _InvalidClusterParameterGroupStateFault,
    _ScheduledActionQuotaExceededFault,
    _InvalidReservedNodeStateFault,
    _ReservedNodeAlreadyExistsFault,
    _ScheduledActionTypeUnsupportedFault,
    _SnapshotScheduleUpdateInProgressFault,
    _InProgressTableRestoreQuotaExceededFault,
    _InvalidRestoreFault,
    _ResourceNotFoundFault,
    _SubscriptionEventIdNotFoundFault,
    _InvalidUsageLimitFault,
    _InvalidSnapshotCopyGrantStateFault,
    _UnknownSnapshotCopyRegionFault,
    _ReservedNodeQuotaExceededFault,
    _ScheduledActionAlreadyExistsFault,
    _ClusterSubnetQuotaExceededFault,
    _ClusterAlreadyExistsFault,
    _AccessToSnapshotDeniedFault,
    _TagLimitExceededFault,
    _NumberOfNodesQuotaExceededFault,
    _ScheduledActionNotFoundFault,
    _HsmClientCertificateAlreadyExistsFault,
    _SnapshotScheduleQuotaExceededFault,
    _InvalidHsmClientCertificateStateFault,
    _ClusterOnLatestRevisionFault,
    _SubnetAlreadyInUse,
    _SubscriptionSeverityNotFoundFault,
    _UsageLimitNotFoundFault,
    _UnauthorizedOperation,
    _InvalidTagFault,
    _InsufficientS3BucketPolicyFault,
    _ClusterSubnetGroupQuotaExceededFault,

    -- * Re-exported types
    module Network.AWS.Redshift.Internal,

    -- * ResizeProgressMessage
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

    -- * Snapshot
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

    -- * ClusterParameterGroup
    ClusterParameterGroup (..),
    mkClusterParameterGroup,
    cpgDescription,
    cpgParameterGroupFamily,
    cpgParameterGroupName,
    cpgTags,

    -- * UsageLimitFeatureType
    UsageLimitFeatureType (..),

    -- * ReservedNodeOfferingType
    ReservedNodeOfferingType (..),

    -- * ResizeInfo
    ResizeInfo (..),
    mkResizeInfo,
    riAllowCancelResize,
    riResizeType,

    -- * DeleteClusterSnapshotMessage
    DeleteClusterSnapshotMessage (..),
    mkDeleteClusterSnapshotMessage,
    dcsmSnapshotIdentifier,
    dcsmSnapshotClusterIdentifier,

    -- * RestoreStatus
    RestoreStatus (..),
    mkRestoreStatus,
    rsCurrentRestoreRateInMegaBytesPerSecond,
    rsElapsedTimeInSeconds,
    rsEstimatedTimeToCompletionInSeconds,
    rsProgressInMegaBytes,
    rsSnapshotSizeInMegaBytes,
    rsStatus,

    -- * Event
    Event (..),
    mkEvent,
    eDate,
    eEventCategories,
    eEventId,
    eMessage,
    eSeverity,
    eSourceIdentifier,
    eSourceType,

    -- * ScheduledActionFilter
    ScheduledActionFilter (..),
    mkScheduledActionFilter,
    safName,
    safValues,

    -- * ClusterSnapshotCopyStatus
    ClusterSnapshotCopyStatus (..),
    mkClusterSnapshotCopyStatus,
    cscsDestinationRegion,
    cscsManualSnapshotRetentionPeriod,
    cscsRetentionPeriod,
    cscsSnapshotCopyGrantName,

    -- * ScheduledActionType
    ScheduledActionType (..),
    mkScheduledActionType,
    satPauseCluster,
    satResizeCluster,
    satResumeCluster,

    -- * SortByOrder
    SortByOrder (..),

    -- * NodeConfigurationOptionsFilterName
    NodeConfigurationOptionsFilterName (..),

    -- * SnapshotCopyGrant
    SnapshotCopyGrant (..),
    mkSnapshotCopyGrant,
    scgKmsKeyId,
    scgSnapshotCopyGrantName,
    scgTags,

    -- * ScheduledActionTypeValues
    ScheduledActionTypeValues (..),

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * HsmClientCertificate
    HsmClientCertificate (..),
    mkHsmClientCertificate,
    hccHsmClientCertificateIdentifier,
    hccHsmClientCertificatePublicKey,
    hccTags,

    -- * Cluster
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

    -- * ClusterNode
    ClusterNode (..),
    mkClusterNode,
    cnNodeRole,
    cnPrivateIPAddress,
    cnPublicIPAddress,

    -- * RevisionTarget
    RevisionTarget (..),
    mkRevisionTarget,
    rtDatabaseRevision,
    rtDatabaseRevisionReleaseDate,
    rtDescription,

    -- * TableRestoreStatus
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

    -- * SnapshotSortingEntity
    SnapshotSortingEntity (..),
    mkSnapshotSortingEntity,
    sseAttribute,
    sseSortOrder,

    -- * ParameterApplyType
    ParameterApplyType (..),

    -- * EC2SecurityGroup
    EC2SecurityGroup (..),
    mkEC2SecurityGroup,
    ecsgEC2SecurityGroupName,
    ecsgEC2SecurityGroupOwnerId,
    ecsgStatus,
    ecsgTags,

    -- * String
    String (..),

    -- * OrderableClusterOption
    OrderableClusterOption (..),
    mkOrderableClusterOption,
    ocoAvailabilityZones,
    ocoClusterType,
    ocoClusterVersion,
    ocoNodeType,

    -- * SourceType
    SourceType (..),

    -- * ScheduledActionState
    ScheduledActionState (..),

    -- * UpdateTarget
    UpdateTarget (..),
    mkUpdateTarget,
    utDatabaseVersion,
    utMaintenanceTrackName,
    utSupportedOperations,

    -- * ResumeClusterMessage
    ResumeClusterMessage (..),
    mkResumeClusterMessage,
    rClusterIdentifier,

    -- * ClusterParameterGroupStatus
    ClusterParameterGroupStatus (..),
    mkClusterParameterGroupStatus,
    cpgsClusterParameterStatusList,
    cpgsParameterApplyStatus,
    cpgsParameterGroupName,

    -- * Subnet
    Subnet (..),
    mkSubnet,
    sSubnetAvailabilityZone,
    sSubnetIdentifier,
    sSubnetStatus,

    -- * ClusterSecurityGroup
    ClusterSecurityGroup (..),
    mkClusterSecurityGroup,
    csgfClusterSecurityGroupName,
    csgfDescription,
    csgfEC2SecurityGroups,
    csgfIPRanges,
    csgfTags,

    -- * DefaultClusterParameters
    DefaultClusterParameters (..),
    mkDefaultClusterParameters,
    dcpMarker,
    dcpParameterGroupFamily,
    dcpParameters,

    -- * NodeConfigurationOption
    NodeConfigurationOption (..),
    mkNodeConfigurationOption,
    ncoEstimatedDiskUtilizationPercent,
    ncoMode,
    ncoNodeType,
    ncoNumberOfNodes,

    -- * PauseClusterMessage
    PauseClusterMessage (..),
    mkPauseClusterMessage,
    pcmClusterIdentifier,

    -- * Mode
    Mode (..),

    -- * AttributeValueTarget
    AttributeValueTarget (..),
    mkAttributeValueTarget,
    avtAttributeValue,

    -- * ClusterSubnetGroup
    ClusterSubnetGroup (..),
    mkClusterSubnetGroup,
    csgClusterSubnetGroupName,
    csgDescription,
    csgSubnetGroupStatus,
    csgSubnets,
    csgTags,
    csgVpcId,

    -- * DataTransferProgress
    DataTransferProgress (..),
    mkDataTransferProgress,
    dtpCurrentRateInMegaBytesPerSecond,
    dtpDataTransferredInMegaBytes,
    dtpElapsedTimeInSeconds,
    dtpEstimatedTimeToCompletionInSeconds,
    dtpStatus,
    dtpTotalDataInMegaBytes,

    -- * EventInfoMap
    EventInfoMap (..),
    mkEventInfoMap,
    eimEventCategories,
    eimEventDescription,
    eimEventId,
    eimSeverity,

    -- * SnapshotSchedule
    SnapshotSchedule (..),
    mkSnapshotSchedule,
    ssAssociatedClusterCount,
    ssAssociatedClusters,
    ssNextInvocations,
    ssScheduleDefinitions,
    ssScheduleDescription,
    ssScheduleIdentifier,
    ssTags,

    -- * ClusterSecurityGroupMembership
    ClusterSecurityGroupMembership (..),
    mkClusterSecurityGroupMembership,
    csgmClusterSecurityGroupName,
    csgmStatus,

    -- * ClusterAssociatedToSchedule
    ClusterAssociatedToSchedule (..),
    mkClusterAssociatedToSchedule,
    catsClusterIdentifier,
    catsScheduleAssociationState,

    -- * ReservedNodeOffering
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

    -- * ReservedNode
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

    -- * ScheduledAction
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

    -- * SnapshotErrorMessage
    SnapshotErrorMessage (..),
    mkSnapshotErrorMessage,
    semFailureCode,
    semFailureReason,
    semSnapshotClusterIdentifier,
    semSnapshotIdentifier,

    -- * LoggingStatus
    LoggingStatus (..),
    mkLoggingStatus,
    lsBucketName,
    lsLastFailureMessage,
    lsLastFailureTime,
    lsLastSuccessfulDeliveryTime,
    lsLoggingEnabled,
    lsS3KeyPrefix,

    -- * AccountWithRestoreAccess
    AccountWithRestoreAccess (..),
    mkAccountWithRestoreAccess,
    awraAccountAlias,
    awraAccountId,

    -- * ClusterParameterStatus
    ClusterParameterStatus (..),
    mkClusterParameterStatus,
    cpsParameterApplyErrorDescription,
    cpsParameterApplyStatus,
    cpsParameterName,

    -- * AvailabilityZone
    AvailabilityZone (..),
    mkAvailabilityZone,
    azName,
    azSupportedPlatforms,

    -- * ClusterDbRevision
    ClusterDbRevision (..),
    mkClusterDbRevision,
    cdrClusterIdentifier,
    cdrCurrentDatabaseRevision,
    cdrDatabaseRevisionReleaseDate,
    cdrRevisionTargets,

    -- * EventSubscription
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

    -- * HsmStatus
    HsmStatus (..),
    mkHsmStatus,
    hsHsmClientCertificateIdentifier,
    hsHsmConfigurationIdentifier,
    hsStatus,

    -- * DeferredMaintenanceWindow
    DeferredMaintenanceWindow (..),
    mkDeferredMaintenanceWindow,
    dmwDeferMaintenanceEndTime,
    dmwDeferMaintenanceIdentifier,
    dmwDeferMaintenanceStartTime,

    -- * MaintenanceTrack
    MaintenanceTrack (..),
    mkMaintenanceTrack,
    mtDatabaseVersion,
    mtMaintenanceTrackName,
    mtUpdateTargets,

    -- * SnapshotAttributeToSortBy
    SnapshotAttributeToSortBy (..),

    -- * ScheduleState
    ScheduleState (..),

    -- * TableRestoreStatusType
    TableRestoreStatusType (..),

    -- * UsageLimitLimitType
    UsageLimitLimitType (..),

    -- * AccountAttribute
    AccountAttribute (..),
    mkAccountAttribute,
    aaAttributeName,
    aaAttributeValues,

    -- * UsageLimitPeriod
    UsageLimitPeriod (..),

    -- * UsageLimit
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

    -- * NodeConfigurationOptionsFilter
    NodeConfigurationOptionsFilter (..),
    mkNodeConfigurationOptionsFilter,
    ncofName,
    ncofOperator,
    ncofValues,

    -- * ScheduledActionFilterName
    ScheduledActionFilterName (..),

    -- * ClusterParameterGroupNameMessage
    ClusterParameterGroupNameMessage (..),
    mkClusterParameterGroupNameMessage,
    cpgnmParameterGroupName,
    cpgnmParameterGroupStatus,

    -- * OperatorType
    OperatorType (..),

    -- * ElasticIpStatus
    ElasticIpStatus (..),
    mkElasticIpStatus,
    eisElasticIp,
    eisStatus,

    -- * ClusterVersion
    ClusterVersion (..),
    mkClusterVersion,
    cvClusterParameterGroupFamily,
    cvClusterVersion,
    cvDescription,

    -- * RecurringCharge
    RecurringCharge (..),
    mkRecurringCharge,
    rcRecurringChargeAmount,
    rcRecurringChargeFrequency,

    -- * SupportedOperation
    SupportedOperation (..),
    mkSupportedOperation,
    soOperationName,

    -- * ClusterIamRole
    ClusterIamRole (..),
    mkClusterIamRole,
    cirApplyStatus,
    cirIamRoleArn,

    -- * ResizeClusterMessage
    ResizeClusterMessage (..),
    mkResizeClusterMessage,
    rcmClusterIdentifier,
    rcmClassic,
    rcmClusterType,
    rcmNodeType,
    rcmNumberOfNodes,

    -- * Endpoint
    Endpoint (..),
    mkEndpoint,
    eAddress,
    ePort,

    -- * IPRange
    IPRange (..),
    mkIPRange,
    iprCIDRIP,
    iprStatus,
    iprTags,

    -- * TaggedResource
    TaggedResource (..),
    mkTaggedResource,
    trResourceName,
    trResourceType,
    trTag,

    -- * EventCategoriesMap
    EventCategoriesMap (..),
    mkEventCategoriesMap,
    ecmEvents,
    ecmSourceType,

    -- * ActionType
    ActionType (..),

    -- * HsmConfiguration
    HsmConfiguration (..),
    mkHsmConfiguration,
    hcDescription,
    hcHsmConfigurationIdentifier,
    hcHsmIpAddress,
    hcHsmPartitionName,
    hcTags,

    -- * PendingModifiedValues
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

    -- * VpcSecurityGroupMembership
    VpcSecurityGroupMembership (..),
    mkVpcSecurityGroupMembership,
    vsgmStatus,
    vsgmVpcSecurityGroupId,

    -- * SupportedPlatform
    SupportedPlatform (..),
    mkSupportedPlatform,
    spName,

    -- * Parameter
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

    -- * UsageLimitBreachAction
    UsageLimitBreachAction (..),

    -- * ClusterSubnetGroupName
    ClusterSubnetGroupName (..),

    -- * SubscriptionName
    SubscriptionName (..),

    -- * Severity
    Severity (..),

    -- * SnsTopicArn
    SnsTopicArn (..),

    -- * Message
    Message (..),

    -- * ResizeType
    ResizeType (..),

    -- * Status
    Status (..),

    -- * TargetClusterType
    TargetClusterType (..),

    -- * TargetEncryptionType
    TargetEncryptionType (..),

    -- * TargetNodeType
    TargetNodeType (..),

    -- * ReservedNodeOfferingId
    ReservedNodeOfferingId (..),

    -- * ClusterIdentifier
    ClusterIdentifier (..),

    -- * Marker
    Marker (..),

    -- * ScheduleIdentifier
    ScheduleIdentifier (..),

    -- * DBName
    DBName (..),

    -- * KmsKeyId
    KmsKeyId (..),

    -- * MaintenanceTrackName
    MaintenanceTrackName (..),

    -- * MasterUsername
    MasterUsername (..),

    -- * NodeType
    NodeType (..),

    -- * OwnerAccount
    OwnerAccount (..),

    -- * SnapshotIdentifier
    SnapshotIdentifier (..),

    -- * SnapshotType
    SnapshotType (..),

    -- * SourceRegion
    SourceRegion (..),

    -- * VpcId
    VpcId (..),

    -- * Description
    Description (..),

    -- * ParameterGroupFamily
    ParameterGroupFamily (..),

    -- * ParameterGroupName
    ParameterGroupName (..),

    -- * SnapshotClusterIdentifier
    SnapshotClusterIdentifier (..),

    -- * ScheduledActionName
    ScheduledActionName (..),

    -- * IamRole
    IamRole (..),

    -- * Schedule
    Schedule (..),

    -- * ScheduledActionDescription
    ScheduledActionDescription (..),

    -- * EventId
    EventId (..),

    -- * SourceIdentifier
    SourceIdentifier (..),

    -- * HsmClientCertificateIdentifier
    HsmClientCertificateIdentifier (..),

    -- * DestinationRegion
    DestinationRegion (..),

    -- * SnapshotCopyGrantName
    SnapshotCopyGrantName (..),

    -- * ReservedNodeId
    ReservedNodeId (..),

    -- * ClusterSecurityGroupName
    ClusterSecurityGroupName (..),

    -- * CIDRIP
    CIDRIP (..),

    -- * EC2SecurityGroupName
    EC2SecurityGroupName (..),

    -- * EC2SecurityGroupOwnerId
    EC2SecurityGroupOwnerId (..),

    -- * DbUser
    DbUser (..),

    -- * DbName
    DbName (..),

    -- * Key
    Key (..),

    -- * Value
    Value (..),

    -- * HsmClientCertificatePublicKey
    HsmClientCertificatePublicKey (..),

    -- * UsageLimitId
    UsageLimitId (..),

    -- * ClusterAvailabilityStatus
    ClusterAvailabilityStatus (..),

    -- * ClusterNamespaceArn
    ClusterNamespaceArn (..),

    -- * ClusterPublicKey
    ClusterPublicKey (..),

    -- * ClusterRevisionNumber
    ClusterRevisionNumber (..),

    -- * ClusterStatus
    ClusterStatus (..),

    -- * ElasticResizeNumberOfNodeOptions
    ElasticResizeNumberOfNodeOptions (..),

    -- * ExpectedNextSnapshotScheduleTimeStatus
    ExpectedNextSnapshotScheduleTimeStatus (..),

    -- * ModifyStatus
    ModifyStatus (..),

    -- * PreferredMaintenanceWindow
    PreferredMaintenanceWindow (..),

    -- * SnapshotScheduleIdentifier
    SnapshotScheduleIdentifier (..),

    -- * NodeRole
    NodeRole (..),

    -- * PrivateIPAddress
    PrivateIPAddress (..),

    -- * PublicIPAddress
    PublicIPAddress (..),

    -- * DatabaseRevision
    DatabaseRevision (..),

    -- * DbPassword
    DbPassword (..),

    -- * NewTableName
    NewTableName (..),

    -- * SourceDatabaseName
    SourceDatabaseName (..),

    -- * SourceSchemaName
    SourceSchemaName (..),

    -- * SourceTableName
    SourceTableName (..),

    -- * TableRestoreRequestId
    TableRestoreRequestId (..),

    -- * TargetDatabaseName
    TargetDatabaseName (..),

    -- * TargetSchemaName
    TargetSchemaName (..),
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.AccountAttribute
import Network.AWS.Redshift.Types.AccountWithRestoreAccess
import Network.AWS.Redshift.Types.ActionType
import Network.AWS.Redshift.Types.AttributeValueTarget
import Network.AWS.Redshift.Types.AvailabilityZone
import Network.AWS.Redshift.Types.CIDRIP
import Network.AWS.Redshift.Types.Cluster
import Network.AWS.Redshift.Types.ClusterAssociatedToSchedule
import Network.AWS.Redshift.Types.ClusterAvailabilityStatus
import Network.AWS.Redshift.Types.ClusterDbRevision
import Network.AWS.Redshift.Types.ClusterIamRole
import Network.AWS.Redshift.Types.ClusterIdentifier
import Network.AWS.Redshift.Types.ClusterNamespaceArn
import Network.AWS.Redshift.Types.ClusterNode
import Network.AWS.Redshift.Types.ClusterParameterGroup
import Network.AWS.Redshift.Types.ClusterParameterGroupNameMessage
import Network.AWS.Redshift.Types.ClusterParameterGroupStatus
import Network.AWS.Redshift.Types.ClusterParameterStatus
import Network.AWS.Redshift.Types.ClusterPublicKey
import Network.AWS.Redshift.Types.ClusterRevisionNumber
import Network.AWS.Redshift.Types.ClusterSecurityGroup
import Network.AWS.Redshift.Types.ClusterSecurityGroupMembership
import Network.AWS.Redshift.Types.ClusterSecurityGroupName
import Network.AWS.Redshift.Types.ClusterSnapshotCopyStatus
import Network.AWS.Redshift.Types.ClusterStatus
import Network.AWS.Redshift.Types.ClusterSubnetGroup
import Network.AWS.Redshift.Types.ClusterSubnetGroupName
import Network.AWS.Redshift.Types.ClusterVersion
import Network.AWS.Redshift.Types.DBName
import Network.AWS.Redshift.Types.DataTransferProgress
import Network.AWS.Redshift.Types.DatabaseRevision
import Network.AWS.Redshift.Types.DbName
import Network.AWS.Redshift.Types.DbPassword
import Network.AWS.Redshift.Types.DbUser
import Network.AWS.Redshift.Types.DefaultClusterParameters
import Network.AWS.Redshift.Types.DeferredMaintenanceWindow
import Network.AWS.Redshift.Types.DeleteClusterSnapshotMessage
import Network.AWS.Redshift.Types.Description
import Network.AWS.Redshift.Types.DestinationRegion
import Network.AWS.Redshift.Types.EC2SecurityGroup
import Network.AWS.Redshift.Types.EC2SecurityGroupName
import Network.AWS.Redshift.Types.EC2SecurityGroupOwnerId
import Network.AWS.Redshift.Types.ElasticIpStatus
import Network.AWS.Redshift.Types.ElasticResizeNumberOfNodeOptions
import Network.AWS.Redshift.Types.Endpoint
import Network.AWS.Redshift.Types.Event
import Network.AWS.Redshift.Types.EventCategoriesMap
import Network.AWS.Redshift.Types.EventId
import Network.AWS.Redshift.Types.EventInfoMap
import Network.AWS.Redshift.Types.EventSubscription
import Network.AWS.Redshift.Types.ExpectedNextSnapshotScheduleTimeStatus
import Network.AWS.Redshift.Types.HsmClientCertificate
import Network.AWS.Redshift.Types.HsmClientCertificateIdentifier
import Network.AWS.Redshift.Types.HsmClientCertificatePublicKey
import Network.AWS.Redshift.Types.HsmConfiguration
import Network.AWS.Redshift.Types.HsmStatus
import Network.AWS.Redshift.Types.IPRange
import Network.AWS.Redshift.Types.IamRole
import Network.AWS.Redshift.Types.Key
import Network.AWS.Redshift.Types.KmsKeyId
import Network.AWS.Redshift.Types.LoggingStatus
import Network.AWS.Redshift.Types.MaintenanceTrack
import Network.AWS.Redshift.Types.MaintenanceTrackName
import Network.AWS.Redshift.Types.Marker
import Network.AWS.Redshift.Types.MasterUsername
import Network.AWS.Redshift.Types.Message
import Network.AWS.Redshift.Types.Mode
import Network.AWS.Redshift.Types.ModifyStatus
import Network.AWS.Redshift.Types.NewTableName
import Network.AWS.Redshift.Types.NodeConfigurationOption
import Network.AWS.Redshift.Types.NodeConfigurationOptionsFilter
import Network.AWS.Redshift.Types.NodeConfigurationOptionsFilterName
import Network.AWS.Redshift.Types.NodeRole
import Network.AWS.Redshift.Types.NodeType
import Network.AWS.Redshift.Types.OperatorType
import Network.AWS.Redshift.Types.OrderableClusterOption
import Network.AWS.Redshift.Types.OwnerAccount
import Network.AWS.Redshift.Types.Parameter
import Network.AWS.Redshift.Types.ParameterApplyType
import Network.AWS.Redshift.Types.ParameterGroupFamily
import Network.AWS.Redshift.Types.ParameterGroupName
import Network.AWS.Redshift.Types.PauseClusterMessage
import Network.AWS.Redshift.Types.PendingModifiedValues
import Network.AWS.Redshift.Types.PreferredMaintenanceWindow
import Network.AWS.Redshift.Types.PrivateIPAddress
import Network.AWS.Redshift.Types.PublicIPAddress
import Network.AWS.Redshift.Types.RecurringCharge
import Network.AWS.Redshift.Types.ReservedNode
import Network.AWS.Redshift.Types.ReservedNodeId
import Network.AWS.Redshift.Types.ReservedNodeOffering
import Network.AWS.Redshift.Types.ReservedNodeOfferingId
import Network.AWS.Redshift.Types.ReservedNodeOfferingType
import Network.AWS.Redshift.Types.ResizeClusterMessage
import Network.AWS.Redshift.Types.ResizeInfo
import Network.AWS.Redshift.Types.ResizeProgressMessage
import Network.AWS.Redshift.Types.ResizeType
import Network.AWS.Redshift.Types.RestoreStatus
import Network.AWS.Redshift.Types.ResumeClusterMessage
import Network.AWS.Redshift.Types.RevisionTarget
import Network.AWS.Redshift.Types.Schedule
import Network.AWS.Redshift.Types.ScheduleIdentifier
import Network.AWS.Redshift.Types.ScheduleState
import Network.AWS.Redshift.Types.ScheduledAction
import Network.AWS.Redshift.Types.ScheduledActionDescription
import Network.AWS.Redshift.Types.ScheduledActionFilter
import Network.AWS.Redshift.Types.ScheduledActionFilterName
import Network.AWS.Redshift.Types.ScheduledActionName
import Network.AWS.Redshift.Types.ScheduledActionState
import Network.AWS.Redshift.Types.ScheduledActionType
import Network.AWS.Redshift.Types.ScheduledActionTypeValues
import Network.AWS.Redshift.Types.Severity
import Network.AWS.Redshift.Types.Snapshot
import Network.AWS.Redshift.Types.SnapshotAttributeToSortBy
import Network.AWS.Redshift.Types.SnapshotClusterIdentifier
import Network.AWS.Redshift.Types.SnapshotCopyGrant
import Network.AWS.Redshift.Types.SnapshotCopyGrantName
import Network.AWS.Redshift.Types.SnapshotErrorMessage
import Network.AWS.Redshift.Types.SnapshotIdentifier
import Network.AWS.Redshift.Types.SnapshotSchedule
import Network.AWS.Redshift.Types.SnapshotScheduleIdentifier
import Network.AWS.Redshift.Types.SnapshotSortingEntity
import Network.AWS.Redshift.Types.SnapshotType
import Network.AWS.Redshift.Types.SnsTopicArn
import Network.AWS.Redshift.Types.SortByOrder
import Network.AWS.Redshift.Types.SourceDatabaseName
import Network.AWS.Redshift.Types.SourceIdentifier
import Network.AWS.Redshift.Types.SourceRegion
import Network.AWS.Redshift.Types.SourceSchemaName
import Network.AWS.Redshift.Types.SourceTableName
import Network.AWS.Redshift.Types.SourceType
import Network.AWS.Redshift.Types.Status
import Network.AWS.Redshift.Types.String
import Network.AWS.Redshift.Types.Subnet
import Network.AWS.Redshift.Types.SubscriptionName
import Network.AWS.Redshift.Types.SupportedOperation
import Network.AWS.Redshift.Types.SupportedPlatform
import Network.AWS.Redshift.Types.TableRestoreRequestId
import Network.AWS.Redshift.Types.TableRestoreStatus
import Network.AWS.Redshift.Types.TableRestoreStatusType
import Network.AWS.Redshift.Types.Tag
import Network.AWS.Redshift.Types.TaggedResource
import Network.AWS.Redshift.Types.TargetClusterType
import Network.AWS.Redshift.Types.TargetDatabaseName
import Network.AWS.Redshift.Types.TargetEncryptionType
import Network.AWS.Redshift.Types.TargetNodeType
import Network.AWS.Redshift.Types.TargetSchemaName
import Network.AWS.Redshift.Types.UpdateTarget
import Network.AWS.Redshift.Types.UsageLimit
import Network.AWS.Redshift.Types.UsageLimitBreachAction
import Network.AWS.Redshift.Types.UsageLimitFeatureType
import Network.AWS.Redshift.Types.UsageLimitId
import Network.AWS.Redshift.Types.UsageLimitLimitType
import Network.AWS.Redshift.Types.UsageLimitPeriod
import Network.AWS.Redshift.Types.Value
import Network.AWS.Redshift.Types.VpcId
import Network.AWS.Redshift.Types.VpcSecurityGroupMembership
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2012-12-01@ of the Amazon Redshift SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "Redshift",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "redshift",
      Core._svcVersion = "2012-12-01",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseXMLError "Redshift",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | The request would result in the user exceeding the allowed number of cluster security groups. For information about increasing your quota, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift> in the /Amazon Redshift Cluster Management Guide/ .
_ClusterSecurityGroupQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ClusterSecurityGroupQuotaExceededFault =
  Core._MatchServiceError
    mkServiceConfig
    "QuotaExceeded.ClusterSecurityGroup"
    Core.. Core.hasStatues 400
{-# DEPRECATED _ClusterSecurityGroupQuotaExceededFault "Use generic-lens or generic-optics instead." #-}

-- | The string specified for the logging S3 key prefix does not comply with the documented constraints.
_InvalidS3KeyPrefixFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidS3KeyPrefixFault =
  Core._MatchServiceError mkServiceConfig "InvalidS3KeyPrefixFault"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidS3KeyPrefixFault "Use generic-lens or generic-optics instead." #-}

-- | The specified Amazon Redshift event source could not be found.
_SourceNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SourceNotFoundFault =
  Core._MatchServiceError mkServiceConfig "SourceNotFound"
    Core.. Core.hasStatues 404
{-# DEPRECATED _SourceNotFoundFault "Use generic-lens or generic-optics instead." #-}

-- | The authorization quota for the cluster security group has been reached.
_AuthorizationQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AuthorizationQuotaExceededFault =
  Core._MatchServiceError
    mkServiceConfig
    "AuthorizationQuotaExceeded"
    Core.. Core.hasStatues 400
{-# DEPRECATED _AuthorizationQuotaExceededFault "Use generic-lens or generic-optics instead." #-}

-- | Cross-region snapshot copy was temporarily disabled. Try your request again.
_CopyToRegionDisabledFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CopyToRegionDisabledFault =
  Core._MatchServiceError
    mkServiceConfig
    "CopyToRegionDisabledFault"
    Core.. Core.hasStatues 400
{-# DEPRECATED _CopyToRegionDisabledFault "Use generic-lens or generic-optics instead." #-}

-- | The encryption key has exceeded its grant limit in AWS KMS.
_LimitExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededFault =
  Core._MatchServiceError mkServiceConfig "LimitExceededFault"
    Core.. Core.hasStatues 400
{-# DEPRECATED _LimitExceededFault "Use generic-lens or generic-optics instead." #-}

-- | The state of the cluster security group is not @available@ .
_InvalidClusterSecurityGroupStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidClusterSecurityGroupStateFault =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidClusterSecurityGroupState"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidClusterSecurityGroupStateFault "Use generic-lens or generic-optics instead." #-}

-- | A cluster security group with the same name already exists.
_ClusterSecurityGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ClusterSecurityGroupAlreadyExistsFault =
  Core._MatchServiceError
    mkServiceConfig
    "ClusterSecurityGroupAlreadyExists"
    Core.. Core.hasStatues 400
{-# DEPRECATED _ClusterSecurityGroupAlreadyExistsFault "Use generic-lens or generic-optics instead." #-}

-- | The snapshot identifier does not refer to an existing cluster snapshot.
_ClusterSnapshotNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ClusterSnapshotNotFoundFault =
  Core._MatchServiceError mkServiceConfig "ClusterSnapshotNotFound"
    Core.. Core.hasStatues 404
{-# DEPRECATED _ClusterSnapshotNotFoundFault "Use generic-lens or generic-optics instead." #-}

-- | The Elastic IP (EIP) is invalid or cannot be found.
_InvalidElasticIpFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidElasticIpFault =
  Core._MatchServiceError mkServiceConfig "InvalidElasticIpFault"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidElasticIpFault "Use generic-lens or generic-optics instead." #-}

-- | The specified @TableRestoreRequestId@ value was not found.
_TableRestoreNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TableRestoreNotFoundFault =
  Core._MatchServiceError
    mkServiceConfig
    "TableRestoreNotFoundFault"
    Core.. Core.hasStatues 400
{-# DEPRECATED _TableRestoreNotFoundFault "Use generic-lens or generic-optics instead." #-}

-- | There is no Amazon Redshift HSM configuration with the specified identifier.
_HsmConfigurationNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_HsmConfigurationNotFoundFault =
  Core._MatchServiceError
    mkServiceConfig
    "HsmConfigurationNotFoundFault"
    Core.. Core.hasStatues 400
{-# DEPRECATED _HsmConfigurationNotFoundFault "Use generic-lens or generic-optics instead." #-}

-- | The definition you submitted is not supported.
_ScheduleDefinitionTypeUnsupportedFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ScheduleDefinitionTypeUnsupportedFault =
  Core._MatchServiceError
    mkServiceConfig
    "ScheduleDefinitionTypeUnsupported"
    Core.. Core.hasStatues 400
{-# DEPRECATED _ScheduleDefinitionTypeUnsupportedFault "Use generic-lens or generic-optics instead." #-}

-- | The specified CIDR block or EC2 security group is already authorized for the specified cluster security group.
_AuthorizationAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AuthorizationAlreadyExistsFault =
  Core._MatchServiceError
    mkServiceConfig
    "AuthorizationAlreadyExists"
    Core.. Core.hasStatues 400
{-# DEPRECATED _AuthorizationAlreadyExistsFault "Use generic-lens or generic-optics instead." #-}

-- | The value specified for the event category was not one of the allowed values, or it specified a category that does not apply to the specified source type. The allowed values are Configuration, Management, Monitoring, and Security.
_SubscriptionCategoryNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SubscriptionCategoryNotFoundFault =
  Core._MatchServiceError
    mkServiceConfig
    "SubscriptionCategoryNotFound"
    Core.. Core.hasStatues 404
{-# DEPRECATED _SubscriptionCategoryNotFoundFault "Use generic-lens or generic-optics instead." #-}

-- | The retention period specified is either in the past or is not a valid value.
--
-- The value must be either -1 or an integer between 1 and 3,653.
_InvalidRetentionPeriodFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRetentionPeriodFault =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidRetentionPeriodFault"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidRetentionPeriodFault "Use generic-lens or generic-optics instead." #-}

-- | There is already an existing Amazon Redshift HSM configuration with the specified identifier.
_HsmConfigurationAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_HsmConfigurationAlreadyExistsFault =
  Core._MatchServiceError
    mkServiceConfig
    "HsmConfigurationAlreadyExistsFault"
    Core.. Core.hasStatues 400
{-# DEPRECATED _HsmConfigurationAlreadyExistsFault "Use generic-lens or generic-optics instead." #-}

-- | An Amazon Redshift event notification subscription with the specified name does not exist.
_SubscriptionNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SubscriptionNotFoundFault =
  Core._MatchServiceError mkServiceConfig "SubscriptionNotFound"
    Core.. Core.hasStatues 404
{-# DEPRECATED _SubscriptionNotFoundFault "Use generic-lens or generic-optics instead." #-}

-- | The S3 bucket name is invalid. For more information about naming rules, go to <https://docs.aws.amazon.com/AmazonS3/latest/dev/BucketRestrictions.html Bucket Restrictions and Limitations> in the Amazon Simple Storage Service (S3) Developer Guide.
_InvalidS3BucketNameFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidS3BucketNameFault =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidS3BucketNameFault"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidS3BucketNameFault "Use generic-lens or generic-optics instead." #-}

-- | The value specified as a snapshot identifier is already used by an existing snapshot.
_ClusterSnapshotAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ClusterSnapshotAlreadyExistsFault =
  Core._MatchServiceError
    mkServiceConfig
    "ClusterSnapshotAlreadyExists"
    Core.. Core.hasStatues 400
{-# DEPRECATED _ClusterSnapshotAlreadyExistsFault "Use generic-lens or generic-optics instead." #-}

-- | The requested subnet is not valid, or not all of the subnets are in the same VPC.
_InvalidSubnet :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidSubnet =
  Core._MatchServiceError mkServiceConfig "InvalidSubnet"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidSubnet "Use generic-lens or generic-optics instead." #-}

-- | The number of tables in the cluster exceeds the limit for the requested new cluster node type.
_TableLimitExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TableLimitExceededFault =
  Core._MatchServiceError mkServiceConfig "TableLimitExceeded"
    Core.. Core.hasStatues 400
{-# DEPRECATED _TableLimitExceededFault "Use generic-lens or generic-optics instead." #-}

-- | The specified HSM configuration is not in the @available@ state, or it is still in use by one or more Amazon Redshift clusters.
_InvalidHsmConfigurationStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidHsmConfigurationStateFault =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidHsmConfigurationStateFault"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidHsmConfigurationStateFault "Use generic-lens or generic-optics instead." #-}

-- | The cluster already has cross-region snapshot copy disabled.
_SnapshotCopyAlreadyDisabledFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SnapshotCopyAlreadyDisabledFault =
  Core._MatchServiceError
    mkServiceConfig
    "SnapshotCopyAlreadyDisabledFault"
    Core.. Core.hasStatues 400
{-# DEPRECATED _SnapshotCopyAlreadyDisabledFault "Use generic-lens or generic-optics instead." #-}

-- | The request would exceed the allowed number of cluster instances for this account. For information about increasing your quota, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift> in the /Amazon Redshift Cluster Management Guide/ .
_ClusterQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ClusterQuotaExceededFault =
  Core._MatchServiceError mkServiceConfig "ClusterQuotaExceeded"
    Core.. Core.hasStatues 400
{-# DEPRECATED _ClusterQuotaExceededFault "Use generic-lens or generic-optics instead." #-}

-- | The quota for HSM client certificates has been reached. For information about increasing your quota, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift> in the /Amazon Redshift Cluster Management Guide/ .
_HsmClientCertificateQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_HsmClientCertificateQuotaExceededFault =
  Core._MatchServiceError
    mkServiceConfig
    "HsmClientCertificateQuotaExceededFault"
    Core.. Core.hasStatues 400
{-# DEPRECATED _HsmClientCertificateQuotaExceededFault "Use generic-lens or generic-optics instead." #-}

-- | The specified snapshot schedule already exists.
_SnapshotScheduleAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SnapshotScheduleAlreadyExistsFault =
  Core._MatchServiceError
    mkServiceConfig
    "SnapshotScheduleAlreadyExists"
    Core.. Core.hasStatues 400
{-# DEPRECATED _SnapshotScheduleAlreadyExistsFault "Use generic-lens or generic-optics instead." #-}

-- | The maximum number for snapshot identifiers has been reached. The limit is 100.
_BatchModifyClusterSnapshotsLimitExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BatchModifyClusterSnapshotsLimitExceededFault =
  Core._MatchServiceError
    mkServiceConfig
    "BatchModifyClusterSnapshotsLimitExceededFault"
    Core.. Core.hasStatues 400
{-# DEPRECATED _BatchModifyClusterSnapshotsLimitExceededFault "Use generic-lens or generic-optics instead." #-}

-- | The parameter group name does not refer to an existing parameter group.
_ClusterParameterGroupNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ClusterParameterGroupNotFoundFault =
  Core._MatchServiceError
    mkServiceConfig
    "ClusterParameterGroupNotFound"
    Core.. Core.hasStatues 404
{-# DEPRECATED _ClusterParameterGroupNotFoundFault "Use generic-lens or generic-optics instead." #-}

-- | The AWS account has exceeded the maximum number of snapshot copy grants in this region.
_SnapshotCopyGrantQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SnapshotCopyGrantQuotaExceededFault =
  Core._MatchServiceError
    mkServiceConfig
    "SnapshotCopyGrantQuotaExceededFault"
    Core.. Core.hasStatues 400
{-# DEPRECATED _SnapshotCopyGrantQuotaExceededFault "Use generic-lens or generic-optics instead." #-}

-- | The operation would exceed the number of nodes allowed for a cluster.
_NumberOfNodesPerClusterLimitExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NumberOfNodesPerClusterLimitExceededFault =
  Core._MatchServiceError
    mkServiceConfig
    "NumberOfNodesPerClusterLimitExceeded"
    Core.. Core.hasStatues 400
{-# DEPRECATED _NumberOfNodesPerClusterLimitExceededFault "Use generic-lens or generic-optics instead." #-}

-- | The cluster already has cross-region snapshot copy enabled.
_SnapshotCopyAlreadyEnabledFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SnapshotCopyAlreadyEnabledFault =
  Core._MatchServiceError
    mkServiceConfig
    "SnapshotCopyAlreadyEnabledFault"
    Core.. Core.hasStatues 400
{-# DEPRECATED _SnapshotCopyAlreadyEnabledFault "Use generic-lens or generic-optics instead." #-}

-- | A cluster parameter group with the same name already exists.
_ClusterParameterGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ClusterParameterGroupAlreadyExistsFault =
  Core._MatchServiceError
    mkServiceConfig
    "ClusterParameterGroupAlreadyExists"
    Core.. Core.hasStatues 400
{-# DEPRECATED _ClusterParameterGroupAlreadyExistsFault "Use generic-lens or generic-optics instead." #-}

-- | The maximum number for a batch delete of snapshots has been reached. The limit is 100.
_BatchDeleteRequestSizeExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BatchDeleteRequestSizeExceededFault =
  Core._MatchServiceError
    mkServiceConfig
    "BatchDeleteRequestSizeExceeded"
    Core.. Core.hasStatues 400
{-# DEPRECATED _BatchDeleteRequestSizeExceededFault "Use generic-lens or generic-optics instead." #-}

-- | Cross-region snapshot copy was temporarily disabled. Try your request again.
_SnapshotCopyDisabledFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SnapshotCopyDisabledFault =
  Core._MatchServiceError
    mkServiceConfig
    "SnapshotCopyDisabledFault"
    Core.. Core.hasStatues 400
{-# DEPRECATED _SnapshotCopyDisabledFault "Use generic-lens or generic-optics instead." #-}

-- | A resize operation for the specified cluster is not found.
_ResizeNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResizeNotFoundFault =
  Core._MatchServiceError mkServiceConfig "ResizeNotFound"
    Core.. Core.hasStatues 404
{-# DEPRECATED _ResizeNotFoundFault "Use generic-lens or generic-optics instead." #-}

-- | There is no Amazon Redshift HSM client certificate with the specified identifier.
_HsmClientCertificateNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_HsmClientCertificateNotFoundFault =
  Core._MatchServiceError
    mkServiceConfig
    "HsmClientCertificateNotFoundFault"
    Core.. Core.hasStatues 400
{-# DEPRECATED _HsmClientCertificateNotFoundFault "Use generic-lens or generic-optics instead." #-}

-- | An Amazon SNS topic with the specified Amazon Resource Name (ARN) does not exist.
_SNSTopicArnNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SNSTopicArnNotFoundFault =
  Core._MatchServiceError mkServiceConfig "SNSTopicArnNotFound"
    Core.. Core.hasStatues 404
{-# DEPRECATED _SNSTopicArnNotFoundFault "Use generic-lens or generic-optics instead." #-}

-- | The @ClusterIdentifier@ parameter does not refer to an existing cluster.
_ClusterNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ClusterNotFoundFault =
  Core._MatchServiceError mkServiceConfig "ClusterNotFound"
    Core.. Core.hasStatues 404
{-# DEPRECATED _ClusterNotFoundFault "Use generic-lens or generic-optics instead." #-}

-- | The request would result in the user exceeding the allowed number of cluster parameter groups. For information about increasing your quota, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift> in the /Amazon Redshift Cluster Management Guide/ .
_ClusterParameterGroupQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ClusterParameterGroupQuotaExceededFault =
  Core._MatchServiceError
    mkServiceConfig
    "ClusterParameterGroupQuotaExceeded"
    Core.. Core.hasStatues 400
{-# DEPRECATED _ClusterParameterGroupQuotaExceededFault "Use generic-lens or generic-optics instead." #-}

-- | The snapshot copy grant can't be created because a grant with the same name already exists.
_SnapshotCopyGrantAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SnapshotCopyGrantAlreadyExistsFault =
  Core._MatchServiceError
    mkServiceConfig
    "SnapshotCopyGrantAlreadyExistsFault"
    Core.. Core.hasStatues 400
{-# DEPRECATED _SnapshotCopyGrantAlreadyExistsFault "Use generic-lens or generic-optics instead." #-}

-- | You do not have permission to publish to the specified Amazon SNS topic.
_SNSNoAuthorizationFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SNSNoAuthorizationFault =
  Core._MatchServiceError mkServiceConfig "SNSNoAuthorization"
    Core.. Core.hasStatues 400
{-# DEPRECATED _SNSNoAuthorizationFault "Use generic-lens or generic-optics instead." #-}

-- | The specified cluster is not in the @available@ state.
_InvalidClusterStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidClusterStateFault =
  Core._MatchServiceError mkServiceConfig "InvalidClusterState"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidClusterStateFault "Use generic-lens or generic-optics instead." #-}

-- | The value specified for the @sourceDatabaseName@ , @sourceSchemaName@ , or @sourceTableName@ parameter, or a combination of these, doesn't exist in the snapshot.
_InvalidTableRestoreArgumentFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTableRestoreArgumentFault =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidTableRestoreArgument"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidTableRestoreArgumentFault "Use generic-lens or generic-optics instead." #-}

-- | The specified snapshot copy grant can't be found. Make sure that the name is typed correctly and that the grant exists in the destination region.
_SnapshotCopyGrantNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SnapshotCopyGrantNotFoundFault =
  Core._MatchServiceError
    mkServiceConfig
    "SnapshotCopyGrantNotFoundFault"
    Core.. Core.hasStatues 400
{-# DEPRECATED _SnapshotCopyGrantNotFoundFault "Use generic-lens or generic-optics instead." #-}

-- | The schedule you submitted isn't valid.
_InvalidScheduleFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidScheduleFault =
  Core._MatchServiceError mkServiceConfig "InvalidSchedule"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidScheduleFault "Use generic-lens or generic-optics instead." #-}

-- | The provided cluster track name is not valid.
_InvalidClusterTrackFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidClusterTrackFault =
  Core._MatchServiceError mkServiceConfig "InvalidClusterTrack"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidClusterTrackFault "Use generic-lens or generic-optics instead." #-}

-- | The quota for HSM configurations has been reached. For information about increasing your quota, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift> in the /Amazon Redshift Cluster Management Guide/ .
_HsmConfigurationQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_HsmConfigurationQuotaExceededFault =
  Core._MatchServiceError
    mkServiceConfig
    "HsmConfigurationQuotaExceededFault"
    Core.. Core.hasStatues 400
{-# DEPRECATED _HsmConfigurationQuotaExceededFault "Use generic-lens or generic-optics instead." #-}

-- | The request would result in the user exceeding the allowed number of cluster snapshots.
_ClusterSnapshotQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ClusterSnapshotQuotaExceededFault =
  Core._MatchServiceError
    mkServiceConfig
    "ClusterSnapshotQuotaExceeded"
    Core.. Core.hasStatues 400
{-# DEPRECATED _ClusterSnapshotQuotaExceededFault "Use generic-lens or generic-optics instead." #-}

-- | The number of nodes specified exceeds the allotted capacity of the cluster.
_InsufficientClusterCapacityFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InsufficientClusterCapacityFault =
  Core._MatchServiceError
    mkServiceConfig
    "InsufficientClusterCapacity"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InsufficientClusterCapacityFault "Use generic-lens or generic-optics instead." #-}

-- | The scheduled action is not valid.
_InvalidScheduledActionFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidScheduledActionFault =
  Core._MatchServiceError mkServiceConfig "InvalidScheduledAction"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidScheduledActionFault "Use generic-lens or generic-optics instead." #-}

-- | Amazon SNS has responded that there is a problem with the specified Amazon SNS topic.
_SNSInvalidTopicFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SNSInvalidTopicFault =
  Core._MatchServiceError mkServiceConfig "SNSInvalidTopic"
    Core.. Core.hasStatues 400
{-# DEPRECATED _SNSInvalidTopicFault "Use generic-lens or generic-optics instead." #-}

-- | The usage limit already exists.
_UsageLimitAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UsageLimitAlreadyExistsFault =
  Core._MatchServiceError mkServiceConfig "UsageLimitAlreadyExists"
    Core.. Core.hasStatues 400
{-# DEPRECATED _UsageLimitAlreadyExistsFault "Use generic-lens or generic-optics instead." #-}

-- | Your request cannot be completed because a dependent internal service is temporarily unavailable. Wait 30 to 60 seconds and try again.
_DependentServiceUnavailableFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DependentServiceUnavailableFault =
  Core._MatchServiceError
    mkServiceConfig
    "DependentServiceUnavailableFault"
    Core.. Core.hasStatues 400
{-# DEPRECATED _DependentServiceUnavailableFault "Use generic-lens or generic-optics instead." #-}

-- | A request option was specified that is not supported.
_UnsupportedOptionFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnsupportedOptionFault =
  Core._MatchServiceError mkServiceConfig "UnsupportedOptionFault"
    Core.. Core.hasStatues 400
{-# DEPRECATED _UnsupportedOptionFault "Use generic-lens or generic-optics instead." #-}

-- | There is already an existing event notification subscription with the specified name.
_SubscriptionAlreadyExistFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SubscriptionAlreadyExistFault =
  Core._MatchServiceError
    mkServiceConfig
    "SubscriptionAlreadyExist"
    Core.. Core.hasStatues 400
{-# DEPRECATED _SubscriptionAlreadyExistFault "Use generic-lens or generic-optics instead." #-}

-- | The cluster snapshot schedule state is not valid.
_InvalidClusterSnapshotScheduleStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidClusterSnapshotScheduleStateFault =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidClusterSnapshotScheduleState"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidClusterSnapshotScheduleStateFault "Use generic-lens or generic-optics instead." #-}

-- | The cluster subnet group does not cover all Availability Zones.
_InvalidVPCNetworkStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidVPCNetworkStateFault =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidVPCNetworkStateFault"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidVPCNetworkStateFault "Use generic-lens or generic-optics instead." #-}

-- | The cluster subnet group name does not refer to an existing cluster subnet group.
_ClusterSubnetGroupNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ClusterSubnetGroupNotFoundFault =
  Core._MatchServiceError
    mkServiceConfig
    "ClusterSubnetGroupNotFoundFault"
    Core.. Core.hasStatues 400
{-# DEPRECATED _ClusterSubnetGroupNotFoundFault "Use generic-lens or generic-optics instead." #-}

-- | Could not find the specified S3 bucket.
_BucketNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BucketNotFoundFault =
  Core._MatchServiceError mkServiceConfig "BucketNotFoundFault"
    Core.. Core.hasStatues 400
{-# DEPRECATED _BucketNotFoundFault "Use generic-lens or generic-optics instead." #-}

-- | The subscription request is invalid because it is a duplicate request. This subscription request is already in progress.
_InvalidSubscriptionStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidSubscriptionStateFault =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidSubscriptionStateFault"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidSubscriptionStateFault "Use generic-lens or generic-optics instead." #-}

-- | The request cannot be completed because a dependent service is throttling requests made by Amazon Redshift on your behalf. Wait and retry the request.
_DependentServiceRequestThrottlingFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DependentServiceRequestThrottlingFault =
  Core._MatchServiceError
    mkServiceConfig
    "DependentServiceRequestThrottlingFault"
    Core.. Core.hasStatues 400
{-# DEPRECATED _DependentServiceRequestThrottlingFault "Use generic-lens or generic-optics instead." #-}

-- | The specified CIDR IP range or EC2 security group is not authorized for the specified cluster security group.
_AuthorizationNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AuthorizationNotFoundFault =
  Core._MatchServiceError mkServiceConfig "AuthorizationNotFound"
    Core.. Core.hasStatues 404
{-# DEPRECATED _AuthorizationNotFoundFault "Use generic-lens or generic-optics instead." #-}

-- | The cluster subnet group cannot be deleted because it is in use.
_InvalidClusterSubnetGroupStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidClusterSubnetGroupStateFault =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidClusterSubnetGroupStateFault"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidClusterSubnetGroupStateFault "Use generic-lens or generic-optics instead." #-}

-- | The requested operation isn't supported.
_UnsupportedOperationFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnsupportedOperationFault =
  Core._MatchServiceError mkServiceConfig "UnsupportedOperation"
    Core.. Core.hasStatues 400
{-# DEPRECATED _UnsupportedOperationFault "Use generic-lens or generic-optics instead." #-}

-- | A /ClusterSubnetGroupName/ is already used by an existing cluster subnet group.
_ClusterSubnetGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ClusterSubnetGroupAlreadyExistsFault =
  Core._MatchServiceError
    mkServiceConfig
    "ClusterSubnetGroupAlreadyExists"
    Core.. Core.hasStatues 400
{-# DEPRECATED _ClusterSubnetGroupAlreadyExistsFault "Use generic-lens or generic-optics instead." #-}

-- | The specified cluster snapshot is not in the @available@ state, or other accounts are authorized to access the snapshot.
_InvalidClusterSnapshotStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidClusterSnapshotStateFault =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidClusterSnapshotState"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidClusterSnapshotStateFault "Use generic-lens or generic-optics instead." #-}

-- | The cluster security group name does not refer to an existing cluster security group.
_ClusterSecurityGroupNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ClusterSecurityGroupNotFoundFault =
  Core._MatchServiceError
    mkServiceConfig
    "ClusterSecurityGroupNotFound"
    Core.. Core.hasStatues 404
{-# DEPRECATED _ClusterSecurityGroupNotFoundFault "Use generic-lens or generic-optics instead." #-}

-- | The specified reserved compute node not found.
_ReservedNodeNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ReservedNodeNotFoundFault =
  Core._MatchServiceError mkServiceConfig "ReservedNodeNotFound"
    Core.. Core.hasStatues 404
{-# DEPRECATED _ReservedNodeNotFoundFault "Use generic-lens or generic-optics instead." #-}

-- | Specified offering does not exist.
_ReservedNodeOfferingNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ReservedNodeOfferingNotFoundFault =
  Core._MatchServiceError
    mkServiceConfig
    "ReservedNodeOfferingNotFound"
    Core.. Core.hasStatues 404
{-# DEPRECATED _ReservedNodeOfferingNotFoundFault "Use generic-lens or generic-optics instead." #-}

-- | The state of the subnet is invalid.
_InvalidClusterSubnetStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidClusterSubnetStateFault =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidClusterSubnetStateFault"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidClusterSubnetStateFault "Use generic-lens or generic-optics instead." #-}

-- | The specified options are incompatible.
_IncompatibleOrderableOptions :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_IncompatibleOrderableOptions =
  Core._MatchServiceError
    mkServiceConfig
    "IncompatibleOrderableOptions"
    Core.. Core.hasStatues 400
{-# DEPRECATED _IncompatibleOrderableOptions "Use generic-lens or generic-optics instead." #-}

-- | Indicates that the reserved node has already been exchanged.
_ReservedNodeAlreadyMigratedFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ReservedNodeAlreadyMigratedFault =
  Core._MatchServiceError
    mkServiceConfig
    "ReservedNodeAlreadyMigrated"
    Core.. Core.hasStatues 400
{-# DEPRECATED _ReservedNodeAlreadyMigratedFault "Use generic-lens or generic-optics instead." #-}

-- | The request would exceed the allowed number of event subscriptions for this account. For information about increasing your quota, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift> in the /Amazon Redshift Cluster Management Guide/ .
_EventSubscriptionQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_EventSubscriptionQuotaExceededFault =
  Core._MatchServiceError
    mkServiceConfig
    "EventSubscriptionQuotaExceeded"
    Core.. Core.hasStatues 400
{-# DEPRECATED _EventSubscriptionQuotaExceededFault "Use generic-lens or generic-optics instead." #-}

-- | We could not find the specified snapshot schedule.
_SnapshotScheduleNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SnapshotScheduleNotFoundFault =
  Core._MatchServiceError
    mkServiceConfig
    "SnapshotScheduleNotFound"
    Core.. Core.hasStatues 400
{-# DEPRECATED _SnapshotScheduleNotFoundFault "Use generic-lens or generic-optics instead." #-}

-- | The cluster parameter group action can not be completed because another task is in progress that involves the parameter group. Wait a few moments and try the operation again.
_InvalidClusterParameterGroupStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidClusterParameterGroupStateFault =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidClusterParameterGroupState"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidClusterParameterGroupStateFault "Use generic-lens or generic-optics instead." #-}

-- | The quota for scheduled actions exceeded.
_ScheduledActionQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ScheduledActionQuotaExceededFault =
  Core._MatchServiceError
    mkServiceConfig
    "ScheduledActionQuotaExceeded"
    Core.. Core.hasStatues 400
{-# DEPRECATED _ScheduledActionQuotaExceededFault "Use generic-lens or generic-optics instead." #-}

-- | Indicates that the Reserved Node being exchanged is not in an active state.
_InvalidReservedNodeStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidReservedNodeStateFault =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidReservedNodeState"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidReservedNodeStateFault "Use generic-lens or generic-optics instead." #-}

-- | User already has a reservation with the given identifier.
_ReservedNodeAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ReservedNodeAlreadyExistsFault =
  Core._MatchServiceError
    mkServiceConfig
    "ReservedNodeAlreadyExists"
    Core.. Core.hasStatues 404
{-# DEPRECATED _ReservedNodeAlreadyExistsFault "Use generic-lens or generic-optics instead." #-}

-- | The action type specified for a scheduled action is not supported.
_ScheduledActionTypeUnsupportedFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ScheduledActionTypeUnsupportedFault =
  Core._MatchServiceError
    mkServiceConfig
    "ScheduledActionTypeUnsupported"
    Core.. Core.hasStatues 400
{-# DEPRECATED _ScheduledActionTypeUnsupportedFault "Use generic-lens or generic-optics instead." #-}

-- | The specified snapshot schedule is already being updated.
_SnapshotScheduleUpdateInProgressFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SnapshotScheduleUpdateInProgressFault =
  Core._MatchServiceError
    mkServiceConfig
    "SnapshotScheduleUpdateInProgress"
    Core.. Core.hasStatues 400
{-# DEPRECATED _SnapshotScheduleUpdateInProgressFault "Use generic-lens or generic-optics instead." #-}

-- | You have exceeded the allowed number of table restore requests. Wait for your current table restore requests to complete before making a new request.
_InProgressTableRestoreQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InProgressTableRestoreQuotaExceededFault =
  Core._MatchServiceError
    mkServiceConfig
    "InProgressTableRestoreQuotaExceededFault"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InProgressTableRestoreQuotaExceededFault "Use generic-lens or generic-optics instead." #-}

-- | The restore is invalid.
_InvalidRestoreFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRestoreFault =
  Core._MatchServiceError mkServiceConfig "InvalidRestore"
    Core.. Core.hasStatues 406
{-# DEPRECATED _InvalidRestoreFault "Use generic-lens or generic-optics instead." #-}

-- | The resource could not be found.
_ResourceNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundFault =
  Core._MatchServiceError mkServiceConfig "ResourceNotFoundFault"
    Core.. Core.hasStatues 404
{-# DEPRECATED _ResourceNotFoundFault "Use generic-lens or generic-optics instead." #-}

-- | An Amazon Redshift event with the specified event ID does not exist.
_SubscriptionEventIdNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SubscriptionEventIdNotFoundFault =
  Core._MatchServiceError
    mkServiceConfig
    "SubscriptionEventIdNotFound"
    Core.. Core.hasStatues 404
{-# DEPRECATED _SubscriptionEventIdNotFoundFault "Use generic-lens or generic-optics instead." #-}

-- | The usage limit is not valid.
_InvalidUsageLimitFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidUsageLimitFault =
  Core._MatchServiceError mkServiceConfig "InvalidUsageLimit"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidUsageLimitFault "Use generic-lens or generic-optics instead." #-}

-- | The snapshot copy grant can't be deleted because it is used by one or more clusters.
_InvalidSnapshotCopyGrantStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidSnapshotCopyGrantStateFault =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidSnapshotCopyGrantStateFault"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidSnapshotCopyGrantStateFault "Use generic-lens or generic-optics instead." #-}

-- | The specified region is incorrect or does not exist.
_UnknownSnapshotCopyRegionFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnknownSnapshotCopyRegionFault =
  Core._MatchServiceError
    mkServiceConfig
    "UnknownSnapshotCopyRegionFault"
    Core.. Core.hasStatues 404
{-# DEPRECATED _UnknownSnapshotCopyRegionFault "Use generic-lens or generic-optics instead." #-}

-- | Request would exceed the user's compute node quota. For information about increasing your quota, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift> in the /Amazon Redshift Cluster Management Guide/ .
_ReservedNodeQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ReservedNodeQuotaExceededFault =
  Core._MatchServiceError
    mkServiceConfig
    "ReservedNodeQuotaExceeded"
    Core.. Core.hasStatues 400
{-# DEPRECATED _ReservedNodeQuotaExceededFault "Use generic-lens or generic-optics instead." #-}

-- | The scheduled action already exists.
_ScheduledActionAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ScheduledActionAlreadyExistsFault =
  Core._MatchServiceError
    mkServiceConfig
    "ScheduledActionAlreadyExists"
    Core.. Core.hasStatues 400
{-# DEPRECATED _ScheduledActionAlreadyExistsFault "Use generic-lens or generic-optics instead." #-}

-- | The request would result in user exceeding the allowed number of subnets in a cluster subnet groups. For information about increasing your quota, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift> in the /Amazon Redshift Cluster Management Guide/ .
_ClusterSubnetQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ClusterSubnetQuotaExceededFault =
  Core._MatchServiceError
    mkServiceConfig
    "ClusterSubnetQuotaExceededFault"
    Core.. Core.hasStatues 400
{-# DEPRECATED _ClusterSubnetQuotaExceededFault "Use generic-lens or generic-optics instead." #-}

-- | The account already has a cluster with the given identifier.
_ClusterAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ClusterAlreadyExistsFault =
  Core._MatchServiceError mkServiceConfig "ClusterAlreadyExists"
    Core.. Core.hasStatues 400
{-# DEPRECATED _ClusterAlreadyExistsFault "Use generic-lens or generic-optics instead." #-}

-- | The owner of the specified snapshot has not authorized your account to access the snapshot.
_AccessToSnapshotDeniedFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AccessToSnapshotDeniedFault =
  Core._MatchServiceError mkServiceConfig "AccessToSnapshotDenied"
    Core.. Core.hasStatues 400
{-# DEPRECATED _AccessToSnapshotDeniedFault "Use generic-lens or generic-optics instead." #-}

-- | You have exceeded the number of tags allowed.
_TagLimitExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TagLimitExceededFault =
  Core._MatchServiceError mkServiceConfig "TagLimitExceededFault"
    Core.. Core.hasStatues 400
{-# DEPRECATED _TagLimitExceededFault "Use generic-lens or generic-optics instead." #-}

-- | The operation would exceed the number of nodes allotted to the account. For information about increasing your quota, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift> in the /Amazon Redshift Cluster Management Guide/ .
_NumberOfNodesQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NumberOfNodesQuotaExceededFault =
  Core._MatchServiceError
    mkServiceConfig
    "NumberOfNodesQuotaExceeded"
    Core.. Core.hasStatues 400
{-# DEPRECATED _NumberOfNodesQuotaExceededFault "Use generic-lens or generic-optics instead." #-}

-- | The scheduled action cannot be found.
_ScheduledActionNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ScheduledActionNotFoundFault =
  Core._MatchServiceError mkServiceConfig "ScheduledActionNotFound"
    Core.. Core.hasStatues 400
{-# DEPRECATED _ScheduledActionNotFoundFault "Use generic-lens or generic-optics instead." #-}

-- | There is already an existing Amazon Redshift HSM client certificate with the specified identifier.
_HsmClientCertificateAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_HsmClientCertificateAlreadyExistsFault =
  Core._MatchServiceError
    mkServiceConfig
    "HsmClientCertificateAlreadyExistsFault"
    Core.. Core.hasStatues 400
{-# DEPRECATED _HsmClientCertificateAlreadyExistsFault "Use generic-lens or generic-optics instead." #-}

-- | You have exceeded the quota of snapshot schedules.
_SnapshotScheduleQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SnapshotScheduleQuotaExceededFault =
  Core._MatchServiceError
    mkServiceConfig
    "SnapshotScheduleQuotaExceeded"
    Core.. Core.hasStatues 400
{-# DEPRECATED _SnapshotScheduleQuotaExceededFault "Use generic-lens or generic-optics instead." #-}

-- | The specified HSM client certificate is not in the @available@ state, or it is still in use by one or more Amazon Redshift clusters.
_InvalidHsmClientCertificateStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidHsmClientCertificateStateFault =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidHsmClientCertificateStateFault"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidHsmClientCertificateStateFault "Use generic-lens or generic-optics instead." #-}

-- | Cluster is already on the latest database revision.
_ClusterOnLatestRevisionFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ClusterOnLatestRevisionFault =
  Core._MatchServiceError mkServiceConfig "ClusterOnLatestRevision"
    Core.. Core.hasStatues 400
{-# DEPRECATED _ClusterOnLatestRevisionFault "Use generic-lens or generic-optics instead." #-}

-- | A specified subnet is already in use by another cluster.
_SubnetAlreadyInUse :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SubnetAlreadyInUse =
  Core._MatchServiceError mkServiceConfig "SubnetAlreadyInUse"
    Core.. Core.hasStatues 400
{-# DEPRECATED _SubnetAlreadyInUse "Use generic-lens or generic-optics instead." #-}

-- | The value specified for the event severity was not one of the allowed values, or it specified a severity that does not apply to the specified source type. The allowed values are ERROR and INFO.
_SubscriptionSeverityNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SubscriptionSeverityNotFoundFault =
  Core._MatchServiceError
    mkServiceConfig
    "SubscriptionSeverityNotFound"
    Core.. Core.hasStatues 404
{-# DEPRECATED _SubscriptionSeverityNotFoundFault "Use generic-lens or generic-optics instead." #-}

-- | The usage limit identifier can't be found.
_UsageLimitNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UsageLimitNotFoundFault =
  Core._MatchServiceError mkServiceConfig "UsageLimitNotFound"
    Core.. Core.hasStatues 404
{-# DEPRECATED _UsageLimitNotFoundFault "Use generic-lens or generic-optics instead." #-}

-- | Your account is not authorized to perform the requested operation.
_UnauthorizedOperation :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnauthorizedOperation =
  Core._MatchServiceError mkServiceConfig "UnauthorizedOperation"
    Core.. Core.hasStatues 400
{-# DEPRECATED _UnauthorizedOperation "Use generic-lens or generic-optics instead." #-}

-- | The tag is invalid.
_InvalidTagFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTagFault =
  Core._MatchServiceError mkServiceConfig "InvalidTagFault"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidTagFault "Use generic-lens or generic-optics instead." #-}

-- | The cluster does not have read bucket or put object permissions on the S3 bucket specified when enabling logging.
_InsufficientS3BucketPolicyFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InsufficientS3BucketPolicyFault =
  Core._MatchServiceError
    mkServiceConfig
    "InsufficientS3BucketPolicyFault"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InsufficientS3BucketPolicyFault "Use generic-lens or generic-optics instead." #-}

-- | The request would result in user exceeding the allowed number of cluster subnet groups. For information about increasing your quota, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift> in the /Amazon Redshift Cluster Management Guide/ .
_ClusterSubnetGroupQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ClusterSubnetGroupQuotaExceededFault =
  Core._MatchServiceError
    mkServiceConfig
    "ClusterSubnetGroupQuotaExceeded"
    Core.. Core.hasStatues 400
{-# DEPRECATED _ClusterSubnetGroupQuotaExceededFault "Use generic-lens or generic-optics instead." #-}
