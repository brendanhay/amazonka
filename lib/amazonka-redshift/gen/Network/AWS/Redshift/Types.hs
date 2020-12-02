{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types
  ( -- * Service Configuration
    redshift,

    -- * Errors

    -- * Re-exported Types
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
    AccountAttribute,
    accountAttribute,
    aaAttributeValues,
    aaAttributeName,

    -- * AccountWithRestoreAccess
    AccountWithRestoreAccess,
    accountWithRestoreAccess,
    awraAccountAlias,
    awraAccountId,

    -- * AttributeValueTarget
    AttributeValueTarget,
    attributeValueTarget,
    avtAttributeValue,

    -- * AvailabilityZone
    AvailabilityZone,
    availabilityZone,
    azName,
    azSupportedPlatforms,

    -- * Cluster
    Cluster,
    cluster,
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
    ClusterAssociatedToSchedule,
    clusterAssociatedToSchedule,
    catsScheduleAssociationState,
    catsClusterIdentifier,

    -- * ClusterDBRevision
    ClusterDBRevision,
    clusterDBRevision,
    cdrDatabaseRevisionReleaseDate,
    cdrClusterIdentifier,
    cdrCurrentDatabaseRevision,
    cdrRevisionTargets,

    -- * ClusterIAMRole
    ClusterIAMRole,
    clusterIAMRole,
    cirIAMRoleARN,
    cirApplyStatus,

    -- * ClusterNode
    ClusterNode,
    clusterNode,
    cnNodeRole,
    cnPrivateIPAddress,
    cnPublicIPAddress,

    -- * ClusterParameterGroup
    ClusterParameterGroup,
    clusterParameterGroup,
    cpgParameterGroupFamily,
    cpgDescription,
    cpgTags,
    cpgParameterGroupName,

    -- * ClusterParameterGroupNameMessage
    ClusterParameterGroupNameMessage,
    clusterParameterGroupNameMessage,
    cpgnmParameterGroupStatus,
    cpgnmParameterGroupName,

    -- * ClusterParameterGroupStatus
    ClusterParameterGroupStatus,
    clusterParameterGroupStatus,
    cpgsClusterParameterStatusList,
    cpgsParameterApplyStatus,
    cpgsParameterGroupName,

    -- * ClusterParameterStatus
    ClusterParameterStatus,
    clusterParameterStatus,
    cpsParameterApplyErrorDescription,
    cpsParameterName,
    cpsParameterApplyStatus,

    -- * ClusterSecurityGroup
    ClusterSecurityGroup,
    clusterSecurityGroup,
    cluClusterSecurityGroupName,
    cluIPRanges,
    cluEC2SecurityGroups,
    cluDescription,
    cluTags,

    -- * ClusterSecurityGroupMembership
    ClusterSecurityGroupMembership,
    clusterSecurityGroupMembership,
    csgmStatus,
    csgmClusterSecurityGroupName,

    -- * ClusterSnapshotCopyStatus
    ClusterSnapshotCopyStatus,
    clusterSnapshotCopyStatus,
    cscsManualSnapshotRetentionPeriod,
    cscsRetentionPeriod,
    cscsDestinationRegion,
    cscsSnapshotCopyGrantName,

    -- * ClusterSubnetGroup
    ClusterSubnetGroup,
    clusterSubnetGroup,
    csgVPCId,
    csgSubnets,
    csgClusterSubnetGroupName,
    csgSubnetGroupStatus,
    csgDescription,
    csgTags,

    -- * ClusterVersion
    ClusterVersion,
    clusterVersion,
    cvClusterParameterGroupFamily,
    cvClusterVersion,
    cvDescription,

    -- * DataTransferProgress
    DataTransferProgress,
    dataTransferProgress,
    dtpCurrentRateInMegaBytesPerSecond,
    dtpStatus,
    dtpEstimatedTimeToCompletionInSeconds,
    dtpDataTransferredInMegaBytes,
    dtpTotalDataInMegaBytes,
    dtpElapsedTimeInSeconds,

    -- * DefaultClusterParameters
    DefaultClusterParameters,
    defaultClusterParameters,
    dcpMarker,
    dcpParameters,
    dcpParameterGroupFamily,

    -- * DeferredMaintenanceWindow
    DeferredMaintenanceWindow,
    deferredMaintenanceWindow,
    dmwDeferMaintenanceEndTime,
    dmwDeferMaintenanceStartTime,
    dmwDeferMaintenanceIdentifier,

    -- * DeleteClusterSnapshotMessage
    DeleteClusterSnapshotMessage,
    deleteClusterSnapshotMessage,
    dcsmSnapshotClusterIdentifier,
    dcsmSnapshotIdentifier,

    -- * EC2SecurityGroup
    EC2SecurityGroup,
    ec2SecurityGroup,
    esgStatus,
    esgEC2SecurityGroupOwnerId,
    esgEC2SecurityGroupName,
    esgTags,

    -- * ElasticIPStatus
    ElasticIPStatus,
    elasticIPStatus,
    eisStatus,
    eisElasticIP,

    -- * Endpoint
    Endpoint,
    endpoint,
    eAddress,
    ePort,

    -- * Event
    Event,
    event,
    eSourceType,
    eSeverity,
    eSourceIdentifier,
    eDate,
    eEventCategories,
    eMessage,
    eEventId,

    -- * EventCategoriesMap
    EventCategoriesMap,
    eventCategoriesMap,
    ecmSourceType,
    ecmEvents,

    -- * EventInfoMap
    EventInfoMap,
    eventInfoMap,
    eimEventDescription,
    eimSeverity,
    eimEventCategories,
    eimEventId,

    -- * EventSubscription
    EventSubscription,
    eventSubscription,
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
    HSMClientCertificate,
    hsmClientCertificate,
    hccHSMClientCertificateIdentifier,
    hccHSMClientCertificatePublicKey,
    hccTags,

    -- * HSMConfiguration
    HSMConfiguration,
    hsmConfiguration,
    hcHSMConfigurationIdentifier,
    hcHSMPartitionName,
    hcDescription,
    hcTags,
    hcHSMIPAddress,

    -- * HSMStatus
    HSMStatus,
    hsmStatus,
    hsStatus,
    hsHSMConfigurationIdentifier,
    hsHSMClientCertificateIdentifier,

    -- * IPRange
    IPRange,
    ipRange,
    irStatus,
    irCIdRIP,
    irTags,

    -- * LoggingStatus
    LoggingStatus,
    loggingStatus,
    lsLastFailureTime,
    lsLastSuccessfulDeliveryTime,
    lsS3KeyPrefix,
    lsBucketName,
    lsLoggingEnabled,
    lsLastFailureMessage,

    -- * MaintenanceTrack
    MaintenanceTrack,
    maintenanceTrack,
    mtDatabaseVersion,
    mtMaintenanceTrackName,
    mtUpdateTargets,

    -- * NodeConfigurationOption
    NodeConfigurationOption,
    nodeConfigurationOption,
    ncoMode,
    ncoNumberOfNodes,
    ncoNodeType,
    ncoEstimatedDiskUtilizationPercent,

    -- * NodeConfigurationOptionsFilter
    NodeConfigurationOptionsFilter,
    nodeConfigurationOptionsFilter,
    ncofValues,
    ncofOperator,
    ncofName,

    -- * OrderableClusterOption
    OrderableClusterOption,
    orderableClusterOption,
    ocoAvailabilityZones,
    ocoClusterType,
    ocoClusterVersion,
    ocoNodeType,

    -- * Parameter
    Parameter,
    parameter,
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
    PauseClusterMessage,
    pauseClusterMessage,
    pcmClusterIdentifier,

    -- * PendingModifiedValues
    PendingModifiedValues,
    pendingModifiedValues,
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
    RecurringCharge,
    recurringCharge,
    rcRecurringChargeFrequency,
    rcRecurringChargeAmount,

    -- * ReservedNode
    ReservedNode,
    reservedNode,
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
    ReservedNodeOffering,
    reservedNodeOffering,
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
    ResizeClusterMessage,
    resizeClusterMessage,
    rcmNumberOfNodes,
    rcmClassic,
    rcmClusterType,
    rcmNodeType,
    rcmClusterIdentifier,

    -- * ResizeInfo
    ResizeInfo,
    resizeInfo,
    riAllowCancelResize,
    riResizeType,

    -- * ResizeProgressMessage
    ResizeProgressMessage,
    resizeProgressMessage,
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
    RestoreStatus,
    restoreStatus,
    rsStatus,
    rsEstimatedTimeToCompletionInSeconds,
    rsCurrentRestoreRateInMegaBytesPerSecond,
    rsProgressInMegaBytes,
    rsElapsedTimeInSeconds,
    rsSnapshotSizeInMegaBytes,

    -- * ResumeClusterMessage
    ResumeClusterMessage,
    resumeClusterMessage,
    rClusterIdentifier,

    -- * RevisionTarget
    RevisionTarget,
    revisionTarget,
    rtDatabaseRevisionReleaseDate,
    rtDatabaseRevision,
    rtDescription,

    -- * ScheduledAction
    ScheduledAction,
    scheduledAction,
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
    ScheduledActionFilter,
    scheduledActionFilter,
    safName,
    safValues,

    -- * ScheduledActionType
    ScheduledActionType,
    scheduledActionType,
    satResizeCluster,
    satResumeCluster,
    satPauseCluster,

    -- * Snapshot
    Snapshot,
    snapshot,
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
    SnapshotCopyGrant,
    snapshotCopyGrant,
    scgKMSKeyId,
    scgSnapshotCopyGrantName,
    scgTags,

    -- * SnapshotErrorMessage
    SnapshotErrorMessage,
    snapshotErrorMessage,
    semFailureReason,
    semSnapshotIdentifier,
    semSnapshotClusterIdentifier,
    semFailureCode,

    -- * SnapshotSchedule
    SnapshotSchedule,
    snapshotSchedule,
    ssAssociatedClusters,
    ssNextInvocations,
    ssScheduleDefinitions,
    ssScheduleDescription,
    ssScheduleIdentifier,
    ssAssociatedClusterCount,
    ssTags,

    -- * SnapshotSortingEntity
    SnapshotSortingEntity,
    snapshotSortingEntity,
    sseSortOrder,
    sseAttribute,

    -- * Subnet
    Subnet,
    subnet,
    sSubnetStatus,
    sSubnetIdentifier,
    sSubnetAvailabilityZone,

    -- * SupportedOperation
    SupportedOperation,
    supportedOperation,
    soOperationName,

    -- * SupportedPlatform
    SupportedPlatform,
    supportedPlatform,
    spName,

    -- * TableRestoreStatus
    TableRestoreStatus,
    tableRestoreStatus,
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
    Tag,
    tag,
    tagValue,
    tagKey,

    -- * TaggedResource
    TaggedResource,
    taggedResource,
    trTag,
    trResourceType,
    trResourceName,

    -- * UpdateTarget
    UpdateTarget,
    updateTarget,
    utDatabaseVersion,
    utMaintenanceTrackName,
    utSupportedOperations,

    -- * UsageLimit
    UsageLimit,
    usageLimit,
    ulAmount,
    ulLimitType,
    ulUsageLimitId,
    ulPeriod,
    ulClusterIdentifier,
    ulBreachAction,
    ulFeatureType,
    ulTags,

    -- * VPCSecurityGroupMembership
    VPCSecurityGroupMembership,
    vpcSecurityGroupMembership,
    vsgmStatus,
    vsgmVPCSecurityGroupId,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
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
import Network.AWS.Sign.V4

-- | API version @2012-12-01@ of the Amazon Redshift SDK configuration.
redshift :: Service
redshift =
  Service
    { _svcAbbrev = "Redshift",
      _svcSigner = v4,
      _svcPrefix = "redshift",
      _svcVersion = "2012-12-01",
      _svcEndpoint = defaultEndpoint redshift,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseXMLError "Redshift",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
