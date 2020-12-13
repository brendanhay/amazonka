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
    redshiftService,

    -- * Errors

    -- * Re-exported types
    module Network.AWS.Redshift.Internal,

    -- * ActionType
    ActionType (..),

    -- * Mode
    Mode (..),

    -- * NodeConfigurationOptionsFilterName
    NodeConfigurationOptionsFilterName (..),

    -- * OperatorType
    OperatorType (..),

    -- * ParameterApplyType
    ParameterApplyType (..),

    -- * ReservedNodeOfferingType
    ReservedNodeOfferingType (..),

    -- * ScheduleState
    ScheduleState (..),

    -- * ScheduledActionFilterName
    ScheduledActionFilterName (..),

    -- * ScheduledActionState
    ScheduledActionState (..),

    -- * ScheduledActionTypeValues
    ScheduledActionTypeValues (..),

    -- * SnapshotAttributeToSortBy
    SnapshotAttributeToSortBy (..),

    -- * SortByOrder
    SortByOrder (..),

    -- * SourceType
    SourceType (..),

    -- * TableRestoreStatusType
    TableRestoreStatusType (..),

    -- * UsageLimitBreachAction
    UsageLimitBreachAction (..),

    -- * UsageLimitFeatureType
    UsageLimitFeatureType (..),

    -- * UsageLimitLimitType
    UsageLimitLimitType (..),

    -- * UsageLimitPeriod
    UsageLimitPeriod (..),

    -- * AccountAttribute
    AccountAttribute (..),
    mkAccountAttribute,
    aaAttributeValues,
    aaAttributeName,

    -- * AccountWithRestoreAccess
    AccountWithRestoreAccess (..),
    mkAccountWithRestoreAccess,
    awraAccountAlias,
    awraAccountId,

    -- * AttributeValueTarget
    AttributeValueTarget (..),
    mkAttributeValueTarget,
    avtAttributeValue,

    -- * AvailabilityZone
    AvailabilityZone (..),
    mkAvailabilityZone,
    azName,
    azSupportedPlatforms,

    -- * Cluster
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

    -- * ClusterAssociatedToSchedule
    ClusterAssociatedToSchedule (..),
    mkClusterAssociatedToSchedule,
    catsScheduleAssociationState,
    catsClusterIdentifier,

    -- * ClusterDBRevision
    ClusterDBRevision (..),
    mkClusterDBRevision,
    cdrDatabaseRevisionReleaseDate,
    cdrClusterIdentifier,
    cdrCurrentDatabaseRevision,
    cdrRevisionTargets,

    -- * ClusterIAMRole
    ClusterIAMRole (..),
    mkClusterIAMRole,
    cirIAMRoleARN,
    cirApplyStatus,

    -- * ClusterNode
    ClusterNode (..),
    mkClusterNode,
    cnNodeRole,
    cnPrivateIPAddress,
    cnPublicIPAddress,

    -- * ClusterParameterGroup
    ClusterParameterGroup (..),
    mkClusterParameterGroup,
    cpgParameterGroupFamily,
    cpgDescription,
    cpgTags,
    cpgParameterGroupName,

    -- * ClusterParameterGroupNameMessage
    ClusterParameterGroupNameMessage (..),
    mkClusterParameterGroupNameMessage,
    cpgnmParameterGroupStatus,
    cpgnmParameterGroupName,

    -- * ClusterParameterGroupStatus
    ClusterParameterGroupStatus (..),
    mkClusterParameterGroupStatus,
    cpgsClusterParameterStatusList,
    cpgsParameterApplyStatus,
    cpgsParameterGroupName,

    -- * ClusterParameterStatus
    ClusterParameterStatus (..),
    mkClusterParameterStatus,
    cpsParameterApplyErrorDescription,
    cpsParameterName,
    cpsParameterApplyStatus,

    -- * ClusterSecurityGroup
    ClusterSecurityGroup (..),
    mkClusterSecurityGroup,
    csgfClusterSecurityGroupName,
    csgfIPRanges,
    csgfEC2SecurityGroups,
    csgfDescription,
    csgfTags,

    -- * ClusterSecurityGroupMembership
    ClusterSecurityGroupMembership (..),
    mkClusterSecurityGroupMembership,
    csgmStatus,
    csgmClusterSecurityGroupName,

    -- * ClusterSnapshotCopyStatus
    ClusterSnapshotCopyStatus (..),
    mkClusterSnapshotCopyStatus,
    cscsManualSnapshotRetentionPeriod,
    cscsRetentionPeriod,
    cscsDestinationRegion,
    cscsSnapshotCopyGrantName,

    -- * ClusterSubnetGroup
    ClusterSubnetGroup (..),
    mkClusterSubnetGroup,
    csgVPCId,
    csgSubnets,
    csgClusterSubnetGroupName,
    csgSubnetGroupStatus,
    csgDescription,
    csgTags,

    -- * ClusterVersion
    ClusterVersion (..),
    mkClusterVersion,
    cvClusterParameterGroupFamily,
    cvClusterVersion,
    cvDescription,

    -- * DataTransferProgress
    DataTransferProgress (..),
    mkDataTransferProgress,
    dtpCurrentRateInMegaBytesPerSecond,
    dtpStatus,
    dtpEstimatedTimeToCompletionInSeconds,
    dtpDataTransferredInMegaBytes,
    dtpTotalDataInMegaBytes,
    dtpElapsedTimeInSeconds,

    -- * DefaultClusterParameters
    DefaultClusterParameters (..),
    mkDefaultClusterParameters,
    dcpMarker,
    dcpParameters,
    dcpParameterGroupFamily,

    -- * DeferredMaintenanceWindow
    DeferredMaintenanceWindow (..),
    mkDeferredMaintenanceWindow,
    dmwDeferMaintenanceEndTime,
    dmwDeferMaintenanceStartTime,
    dmwDeferMaintenanceIdentifier,

    -- * DeleteClusterSnapshotMessage
    DeleteClusterSnapshotMessage (..),
    mkDeleteClusterSnapshotMessage,
    dcsmSnapshotIdentifier,
    dcsmSnapshotClusterIdentifier,

    -- * EC2SecurityGroup
    EC2SecurityGroup (..),
    mkEC2SecurityGroup,
    esgStatus,
    esgEC2SecurityGroupOwnerId,
    esgEC2SecurityGroupName,
    esgTags,

    -- * ElasticIPStatus
    ElasticIPStatus (..),
    mkElasticIPStatus,
    eisStatus,
    eisElasticIP,

    -- * Endpoint
    Endpoint (..),
    mkEndpoint,
    eAddress,
    ePort,

    -- * Event
    Event (..),
    mkEvent,
    eSourceType,
    eSeverity,
    eSourceIdentifier,
    eDate,
    eEventCategories,
    eMessage,
    eEventId,

    -- * EventCategoriesMap
    EventCategoriesMap (..),
    mkEventCategoriesMap,
    ecmSourceType,
    ecmEvents,

    -- * EventInfoMap
    EventInfoMap (..),
    mkEventInfoMap,
    eimEventDescription,
    eimSeverity,
    eimEventCategories,
    eimEventId,

    -- * EventSubscription
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

    -- * HSMClientCertificate
    HSMClientCertificate (..),
    mkHSMClientCertificate,
    hccHSMClientCertificateIdentifier,
    hccHSMClientCertificatePublicKey,
    hccTags,

    -- * HSMConfiguration
    HSMConfiguration (..),
    mkHSMConfiguration,
    hcHSMConfigurationIdentifier,
    hcHSMPartitionName,
    hcDescription,
    hcTags,
    hcHSMIPAddress,

    -- * HSMStatus
    HSMStatus (..),
    mkHSMStatus,
    hsStatus,
    hsHSMConfigurationIdentifier,
    hsHSMClientCertificateIdentifier,

    -- * IPRange
    IPRange (..),
    mkIPRange,
    irStatus,
    irCIdRIP,
    irTags,

    -- * LoggingStatus
    LoggingStatus (..),
    mkLoggingStatus,
    lsLastFailureTime,
    lsLastSuccessfulDeliveryTime,
    lsS3KeyPrefix,
    lsBucketName,
    lsLoggingEnabled,
    lsLastFailureMessage,

    -- * MaintenanceTrack
    MaintenanceTrack (..),
    mkMaintenanceTrack,
    mtDatabaseVersion,
    mtMaintenanceTrackName,
    mtUpdateTargets,

    -- * NodeConfigurationOption
    NodeConfigurationOption (..),
    mkNodeConfigurationOption,
    ncoMode,
    ncoNumberOfNodes,
    ncoNodeType,
    ncoEstimatedDiskUtilizationPercent,

    -- * NodeConfigurationOptionsFilter
    NodeConfigurationOptionsFilter (..),
    mkNodeConfigurationOptionsFilter,
    ncofValues,
    ncofOperator,
    ncofName,

    -- * OrderableClusterOption
    OrderableClusterOption (..),
    mkOrderableClusterOption,
    ocoAvailabilityZones,
    ocoClusterType,
    ocoClusterVersion,
    ocoNodeType,

    -- * Parameter
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

    -- * PauseClusterMessage
    PauseClusterMessage (..),
    mkPauseClusterMessage,
    pcmClusterIdentifier,

    -- * PendingModifiedValues
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

    -- * RecurringCharge
    RecurringCharge (..),
    mkRecurringCharge,
    rcRecurringChargeFrequency,
    rcRecurringChargeAmount,

    -- * ReservedNode
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

    -- * ReservedNodeOffering
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

    -- * ResizeClusterMessage
    ResizeClusterMessage (..),
    mkResizeClusterMessage,
    rcmClusterIdentifier,
    rcmNumberOfNodes,
    rcmClassic,
    rcmClusterType,
    rcmNodeType,

    -- * ResizeInfo
    ResizeInfo (..),
    mkResizeInfo,
    riAllowCancelResize,
    riResizeType,

    -- * ResizeProgressMessage
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

    -- * RestoreStatus
    RestoreStatus (..),
    mkRestoreStatus,
    rsStatus,
    rsEstimatedTimeToCompletionInSeconds,
    rsCurrentRestoreRateInMegaBytesPerSecond,
    rsProgressInMegaBytes,
    rsElapsedTimeInSeconds,
    rsSnapshotSizeInMegaBytes,

    -- * ResumeClusterMessage
    ResumeClusterMessage (..),
    mkResumeClusterMessage,
    rClusterIdentifier,

    -- * RevisionTarget
    RevisionTarget (..),
    mkRevisionTarget,
    rtDatabaseRevisionReleaseDate,
    rtDatabaseRevision,
    rtDescription,

    -- * ScheduledAction
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

    -- * ScheduledActionFilter
    ScheduledActionFilter (..),
    mkScheduledActionFilter,
    safValues,
    safName,

    -- * ScheduledActionType
    ScheduledActionType (..),
    mkScheduledActionType,
    satResizeCluster,
    satResumeCluster,
    satPauseCluster,

    -- * Snapshot
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

    -- * SnapshotCopyGrant
    SnapshotCopyGrant (..),
    mkSnapshotCopyGrant,
    scgKMSKeyId,
    scgSnapshotCopyGrantName,
    scgTags,

    -- * SnapshotErrorMessage
    SnapshotErrorMessage (..),
    mkSnapshotErrorMessage,
    semFailureReason,
    semSnapshotIdentifier,
    semSnapshotClusterIdentifier,
    semFailureCode,

    -- * SnapshotSchedule
    SnapshotSchedule (..),
    mkSnapshotSchedule,
    ssAssociatedClusters,
    ssNextInvocations,
    ssScheduleDefinitions,
    ssScheduleDescription,
    ssScheduleIdentifier,
    ssAssociatedClusterCount,
    ssTags,

    -- * SnapshotSortingEntity
    SnapshotSortingEntity (..),
    mkSnapshotSortingEntity,
    sseAttribute,
    sseSortOrder,

    -- * Subnet
    Subnet (..),
    mkSubnet,
    sSubnetStatus,
    sSubnetIdentifier,
    sSubnetAvailabilityZone,

    -- * SupportedOperation
    SupportedOperation (..),
    mkSupportedOperation,
    soOperationName,

    -- * SupportedPlatform
    SupportedPlatform (..),
    mkSupportedPlatform,
    spName,

    -- * TableRestoreStatus
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

    -- * Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- * TaggedResource
    TaggedResource (..),
    mkTaggedResource,
    trTag,
    trResourceType,
    trResourceName,

    -- * UpdateTarget
    UpdateTarget (..),
    mkUpdateTarget,
    utDatabaseVersion,
    utMaintenanceTrackName,
    utSupportedOperations,

    -- * UsageLimit
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

    -- * VPCSecurityGroupMembership
    VPCSecurityGroupMembership (..),
    mkVPCSecurityGroupMembership,
    vsgmStatus,
    vsgmVPCSecurityGroupId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.AccountAttribute
import Network.AWS.Redshift.Types.AccountWithRestoreAccess
import Network.AWS.Redshift.Types.ActionType
import Network.AWS.Redshift.Types.AttributeValueTarget
import Network.AWS.Redshift.Types.AvailabilityZone
import Network.AWS.Redshift.Types.Cluster
import Network.AWS.Redshift.Types.ClusterAssociatedToSchedule
import Network.AWS.Redshift.Types.ClusterDBRevision
import Network.AWS.Redshift.Types.ClusterIAMRole
import Network.AWS.Redshift.Types.ClusterNode
import Network.AWS.Redshift.Types.ClusterParameterGroup
import Network.AWS.Redshift.Types.ClusterParameterGroupNameMessage
import Network.AWS.Redshift.Types.ClusterParameterGroupStatus
import Network.AWS.Redshift.Types.ClusterParameterStatus
import Network.AWS.Redshift.Types.ClusterSecurityGroup
import Network.AWS.Redshift.Types.ClusterSecurityGroupMembership
import Network.AWS.Redshift.Types.ClusterSnapshotCopyStatus
import Network.AWS.Redshift.Types.ClusterSubnetGroup
import Network.AWS.Redshift.Types.ClusterVersion
import Network.AWS.Redshift.Types.DataTransferProgress
import Network.AWS.Redshift.Types.DefaultClusterParameters
import Network.AWS.Redshift.Types.DeferredMaintenanceWindow
import Network.AWS.Redshift.Types.DeleteClusterSnapshotMessage
import Network.AWS.Redshift.Types.EC2SecurityGroup
import Network.AWS.Redshift.Types.ElasticIPStatus
import Network.AWS.Redshift.Types.Endpoint
import Network.AWS.Redshift.Types.Event
import Network.AWS.Redshift.Types.EventCategoriesMap
import Network.AWS.Redshift.Types.EventInfoMap
import Network.AWS.Redshift.Types.EventSubscription
import Network.AWS.Redshift.Types.HSMClientCertificate
import Network.AWS.Redshift.Types.HSMConfiguration
import Network.AWS.Redshift.Types.HSMStatus
import Network.AWS.Redshift.Types.IPRange
import Network.AWS.Redshift.Types.LoggingStatus
import Network.AWS.Redshift.Types.MaintenanceTrack
import Network.AWS.Redshift.Types.Mode
import Network.AWS.Redshift.Types.NodeConfigurationOption
import Network.AWS.Redshift.Types.NodeConfigurationOptionsFilter
import Network.AWS.Redshift.Types.NodeConfigurationOptionsFilterName
import Network.AWS.Redshift.Types.OperatorType
import Network.AWS.Redshift.Types.OrderableClusterOption
import Network.AWS.Redshift.Types.Parameter
import Network.AWS.Redshift.Types.ParameterApplyType
import Network.AWS.Redshift.Types.PauseClusterMessage
import Network.AWS.Redshift.Types.PendingModifiedValues
import Network.AWS.Redshift.Types.RecurringCharge
import Network.AWS.Redshift.Types.ReservedNode
import Network.AWS.Redshift.Types.ReservedNodeOffering
import Network.AWS.Redshift.Types.ReservedNodeOfferingType
import Network.AWS.Redshift.Types.ResizeClusterMessage
import Network.AWS.Redshift.Types.ResizeInfo
import Network.AWS.Redshift.Types.ResizeProgressMessage
import Network.AWS.Redshift.Types.RestoreStatus
import Network.AWS.Redshift.Types.ResumeClusterMessage
import Network.AWS.Redshift.Types.RevisionTarget
import Network.AWS.Redshift.Types.ScheduleState
import Network.AWS.Redshift.Types.ScheduledAction
import Network.AWS.Redshift.Types.ScheduledActionFilter
import Network.AWS.Redshift.Types.ScheduledActionFilterName
import Network.AWS.Redshift.Types.ScheduledActionState
import Network.AWS.Redshift.Types.ScheduledActionType
import Network.AWS.Redshift.Types.ScheduledActionTypeValues
import Network.AWS.Redshift.Types.Snapshot
import Network.AWS.Redshift.Types.SnapshotAttributeToSortBy
import Network.AWS.Redshift.Types.SnapshotCopyGrant
import Network.AWS.Redshift.Types.SnapshotErrorMessage
import Network.AWS.Redshift.Types.SnapshotSchedule
import Network.AWS.Redshift.Types.SnapshotSortingEntity
import Network.AWS.Redshift.Types.SortByOrder
import Network.AWS.Redshift.Types.SourceType
import Network.AWS.Redshift.Types.Subnet
import Network.AWS.Redshift.Types.SupportedOperation
import Network.AWS.Redshift.Types.SupportedPlatform
import Network.AWS.Redshift.Types.TableRestoreStatus
import Network.AWS.Redshift.Types.TableRestoreStatusType
import Network.AWS.Redshift.Types.Tag
import Network.AWS.Redshift.Types.TaggedResource
import Network.AWS.Redshift.Types.UpdateTarget
import Network.AWS.Redshift.Types.UsageLimit
import Network.AWS.Redshift.Types.UsageLimitBreachAction
import Network.AWS.Redshift.Types.UsageLimitFeatureType
import Network.AWS.Redshift.Types.UsageLimitLimitType
import Network.AWS.Redshift.Types.UsageLimitPeriod
import Network.AWS.Redshift.Types.VPCSecurityGroupMembership
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2012-12-01@ of the Amazon Redshift SDK configuration.
redshiftService :: Lude.Service
redshiftService =
  Lude.Service
    { Lude._svcAbbrev = "Redshift",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "redshift",
      Lude._svcVersion = "2012-12-01",
      Lude._svcEndpoint = Lude.defaultEndpoint redshiftService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseXMLError "Redshift",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
