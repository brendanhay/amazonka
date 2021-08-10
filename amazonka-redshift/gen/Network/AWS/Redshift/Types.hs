{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ClusterSecurityGroupAlreadyExistsFault,
    _ClusterSnapshotNotFoundFault,
    _ScheduleDefinitionTypeUnsupportedFault,
    _ClusterSubnetGroupNotFoundFault,
    _InvalidVPCNetworkStateFault,
    _InvalidSubscriptionStateFault,
    _BucketNotFoundFault,
    _LimitExceededFault,
    _SubscriptionAlreadyExistFault,
    _ClusterSecurityGroupQuotaExceededFault,
    _InvalidScheduledActionFault,
    _SubscriptionSeverityNotFoundFault,
    _UnauthorizedOperation,
    _InsufficientS3BucketPolicyFault,
    _InsufficientClusterCapacityFault,
    _HsmConfigurationQuotaExceededFault,
    _UsageLimitNotFoundFault,
    _InvalidTagFault,
    _SnapshotScheduleQuotaExceededFault,
    _SNSNoAuthorizationFault,
    _ScheduledActionNotFoundFault,
    _HsmClientCertificateAlreadyExistsFault,
    _ResourceNotFoundFault,
    _SNSTopicArnNotFoundFault,
    _InvalidRestoreFault,
    _ScheduledActionAlreadyExistsFault,
    _ClusterNotFoundFault,
    _ScheduledActionQuotaExceededFault,
    _ScheduledActionTypeUnsupportedFault,
    _EventSubscriptionQuotaExceededFault,
    _SnapshotScheduleNotFoundFault,
    _InvalidReservedNodeStateFault,
    _SnapshotScheduleUpdateInProgressFault,
    _BatchModifyClusterSnapshotsLimitExceededFault,
    _ReservedNodeAlreadyMigratedFault,
    _IncompatibleOrderableOptions,
    _InvalidClusterSubnetStateFault,
    _ClusterQuotaExceededFault,
    _InvalidS3BucketNameFault,
    _InvalidClusterSubnetGroupStateFault,
    _HsmConfigurationAlreadyExistsFault,
    _ClusterSecurityGroupNotFoundFault,
    _UnsupportedOperationFault,
    _InvalidElasticIpFault,
    _InvalidClusterSnapshotScheduleStateFault,
    _TableRestoreNotFoundFault,
    _HsmConfigurationNotFoundFault,
    _AuthorizationAlreadyExistsFault,
    _InvalidClusterSecurityGroupStateFault,
    _UsageLimitAlreadyExistsFault,
    _SNSInvalidTopicFault,
    _SourceNotFoundFault,
    _InvalidS3KeyPrefixFault,
    _UnsupportedOptionFault,
    _AuthorizationQuotaExceededFault,
    _DependentServiceUnavailableFault,
    _CopyToRegionDisabledFault,
    _ClusterOnLatestRevisionFault,
    _SubnetAlreadyInUse,
    _ClusterSubnetGroupQuotaExceededFault,
    _ClusterSnapshotQuotaExceededFault,
    _InvalidScheduleFault,
    _ClusterSubnetQuotaExceededFault,
    _AccessToSnapshotDeniedFault,
    _NumberOfNodesQuotaExceededFault,
    _InvalidTableRestoreArgumentFault,
    _InvalidClusterTrackFault,
    _InvalidHsmClientCertificateStateFault,
    _TagLimitExceededFault,
    _SnapshotCopyGrantNotFoundFault,
    _ClusterAlreadyExistsFault,
    _InvalidClusterStateFault,
    _ClusterParameterGroupQuotaExceededFault,
    _UnknownSnapshotCopyRegionFault,
    _SnapshotCopyDisabledFault,
    _InvalidUsageLimitFault,
    _InProgressTableRestoreQuotaExceededFault,
    _ReservedNodeQuotaExceededFault,
    _HsmClientCertificateNotFoundFault,
    _SubscriptionEventIdNotFoundFault,
    _InvalidSnapshotCopyGrantStateFault,
    _SnapshotCopyGrantAlreadyExistsFault,
    _ResizeNotFoundFault,
    _SnapshotCopyAlreadyEnabledFault,
    _BatchDeleteRequestSizeExceededFault,
    _NumberOfNodesPerClusterLimitExceededFault,
    _ReservedNodeAlreadyExistsFault,
    _ClusterParameterGroupAlreadyExistsFault,
    _SnapshotCopyGrantQuotaExceededFault,
    _InvalidClusterParameterGroupStateFault,
    _SnapshotCopyAlreadyDisabledFault,
    _SnapshotScheduleAlreadyExistsFault,
    _ReservedNodeOfferingNotFoundFault,
    _ReservedNodeNotFoundFault,
    _HsmClientCertificateQuotaExceededFault,
    _ClusterParameterGroupNotFoundFault,
    _SubscriptionCategoryNotFoundFault,
    _AuthorizationNotFoundFault,
    _TableLimitExceededFault,
    _InvalidClusterSnapshotStateFault,
    _InvalidRetentionPeriodFault,
    _ClusterSubnetGroupAlreadyExistsFault,
    _InvalidSubnet,
    _SubscriptionNotFoundFault,
    _ClusterSnapshotAlreadyExistsFault,
    _InvalidHsmConfigurationStateFault,
    _DependentServiceRequestThrottlingFault,

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
    AccountAttribute (..),
    newAccountAttribute,
    accountAttribute_attributeName,
    accountAttribute_attributeValues,

    -- * AccountWithRestoreAccess
    AccountWithRestoreAccess (..),
    newAccountWithRestoreAccess,
    accountWithRestoreAccess_accountAlias,
    accountWithRestoreAccess_accountId,

    -- * AttributeValueTarget
    AttributeValueTarget (..),
    newAttributeValueTarget,
    attributeValueTarget_attributeValue,

    -- * AvailabilityZone
    AvailabilityZone (..),
    newAvailabilityZone,
    availabilityZone_name,
    availabilityZone_supportedPlatforms,

    -- * Cluster
    Cluster (..),
    newCluster,
    cluster_enhancedVpcRouting,
    cluster_vpcSecurityGroups,
    cluster_resizeInfo,
    cluster_clusterNamespaceArn,
    cluster_clusterSubnetGroupName,
    cluster_deferredMaintenanceWindows,
    cluster_expectedNextSnapshotScheduleTimeStatus,
    cluster_snapshotScheduleState,
    cluster_encrypted,
    cluster_allowVersionUpgrade,
    cluster_clusterCreateTime,
    cluster_automatedSnapshotRetentionPeriod,
    cluster_elasticIpStatus,
    cluster_hsmStatus,
    cluster_snapshotScheduleIdentifier,
    cluster_masterUsername,
    cluster_publiclyAccessible,
    cluster_clusterAvailabilityStatus,
    cluster_manualSnapshotRetentionPeriod,
    cluster_kmsKeyId,
    cluster_clusterSnapshotCopyStatus,
    cluster_availabilityZone,
    cluster_clusterParameterGroups,
    cluster_preferredMaintenanceWindow,
    cluster_clusterPublicKey,
    cluster_restoreStatus,
    cluster_modifyStatus,
    cluster_clusterIdentifier,
    cluster_tags,
    cluster_clusterNodes,
    cluster_numberOfNodes,
    cluster_availabilityZoneRelocationStatus,
    cluster_dbName,
    cluster_dataTransferProgress,
    cluster_clusterStatus,
    cluster_pendingModifiedValues,
    cluster_endpoint,
    cluster_nextMaintenanceWindowStartTime,
    cluster_nodeType,
    cluster_clusterVersion,
    cluster_vpcId,
    cluster_clusterSecurityGroups,
    cluster_elasticResizeNumberOfNodeOptions,
    cluster_expectedNextSnapshotScheduleTime,
    cluster_maintenanceTrackName,
    cluster_clusterRevisionNumber,
    cluster_iamRoles,
    cluster_pendingActions,

    -- * ClusterAssociatedToSchedule
    ClusterAssociatedToSchedule (..),
    newClusterAssociatedToSchedule,
    clusterAssociatedToSchedule_scheduleAssociationState,
    clusterAssociatedToSchedule_clusterIdentifier,

    -- * ClusterDbRevision
    ClusterDbRevision (..),
    newClusterDbRevision,
    clusterDbRevision_currentDatabaseRevision,
    clusterDbRevision_revisionTargets,
    clusterDbRevision_clusterIdentifier,
    clusterDbRevision_databaseRevisionReleaseDate,

    -- * ClusterIamRole
    ClusterIamRole (..),
    newClusterIamRole,
    clusterIamRole_iamRoleArn,
    clusterIamRole_applyStatus,

    -- * ClusterNode
    ClusterNode (..),
    newClusterNode,
    clusterNode_nodeRole,
    clusterNode_publicIPAddress,
    clusterNode_privateIPAddress,

    -- * ClusterParameterGroup
    ClusterParameterGroup (..),
    newClusterParameterGroup,
    clusterParameterGroup_tags,
    clusterParameterGroup_parameterGroupName,
    clusterParameterGroup_description,
    clusterParameterGroup_parameterGroupFamily,

    -- * ClusterParameterGroupNameMessage
    ClusterParameterGroupNameMessage (..),
    newClusterParameterGroupNameMessage,
    clusterParameterGroupNameMessage_parameterGroupStatus,
    clusterParameterGroupNameMessage_parameterGroupName,

    -- * ClusterParameterGroupStatus
    ClusterParameterGroupStatus (..),
    newClusterParameterGroupStatus,
    clusterParameterGroupStatus_clusterParameterStatusList,
    clusterParameterGroupStatus_parameterGroupName,
    clusterParameterGroupStatus_parameterApplyStatus,

    -- * ClusterParameterStatus
    ClusterParameterStatus (..),
    newClusterParameterStatus,
    clusterParameterStatus_parameterApplyStatus,
    clusterParameterStatus_parameterName,
    clusterParameterStatus_parameterApplyErrorDescription,

    -- * ClusterSecurityGroup
    ClusterSecurityGroup (..),
    newClusterSecurityGroup,
    clusterSecurityGroup_iPRanges,
    clusterSecurityGroup_clusterSecurityGroupName,
    clusterSecurityGroup_tags,
    clusterSecurityGroup_eC2SecurityGroups,
    clusterSecurityGroup_description,

    -- * ClusterSecurityGroupMembership
    ClusterSecurityGroupMembership (..),
    newClusterSecurityGroupMembership,
    clusterSecurityGroupMembership_status,
    clusterSecurityGroupMembership_clusterSecurityGroupName,

    -- * ClusterSnapshotCopyStatus
    ClusterSnapshotCopyStatus (..),
    newClusterSnapshotCopyStatus,
    clusterSnapshotCopyStatus_destinationRegion,
    clusterSnapshotCopyStatus_snapshotCopyGrantName,
    clusterSnapshotCopyStatus_manualSnapshotRetentionPeriod,
    clusterSnapshotCopyStatus_retentionPeriod,

    -- * ClusterSubnetGroup
    ClusterSubnetGroup (..),
    newClusterSubnetGroup,
    clusterSubnetGroup_clusterSubnetGroupName,
    clusterSubnetGroup_subnetGroupStatus,
    clusterSubnetGroup_tags,
    clusterSubnetGroup_description,
    clusterSubnetGroup_subnets,
    clusterSubnetGroup_vpcId,

    -- * ClusterVersion
    ClusterVersion (..),
    newClusterVersion,
    clusterVersion_clusterParameterGroupFamily,
    clusterVersion_description,
    clusterVersion_clusterVersion,

    -- * DataTransferProgress
    DataTransferProgress (..),
    newDataTransferProgress,
    dataTransferProgress_status,
    dataTransferProgress_estimatedTimeToCompletionInSeconds,
    dataTransferProgress_dataTransferredInMegaBytes,
    dataTransferProgress_currentRateInMegaBytesPerSecond,
    dataTransferProgress_elapsedTimeInSeconds,
    dataTransferProgress_totalDataInMegaBytes,

    -- * DefaultClusterParameters
    DefaultClusterParameters (..),
    newDefaultClusterParameters,
    defaultClusterParameters_parameterGroupFamily,
    defaultClusterParameters_parameters,
    defaultClusterParameters_marker,

    -- * DeferredMaintenanceWindow
    DeferredMaintenanceWindow (..),
    newDeferredMaintenanceWindow,
    deferredMaintenanceWindow_deferMaintenanceIdentifier,
    deferredMaintenanceWindow_deferMaintenanceStartTime,
    deferredMaintenanceWindow_deferMaintenanceEndTime,

    -- * DeleteClusterSnapshotMessage
    DeleteClusterSnapshotMessage (..),
    newDeleteClusterSnapshotMessage,
    deleteClusterSnapshotMessage_snapshotClusterIdentifier,
    deleteClusterSnapshotMessage_snapshotIdentifier,

    -- * EC2SecurityGroup
    EC2SecurityGroup (..),
    newEC2SecurityGroup,
    eC2SecurityGroup_status,
    eC2SecurityGroup_tags,
    eC2SecurityGroup_eC2SecurityGroupOwnerId,
    eC2SecurityGroup_eC2SecurityGroupName,

    -- * ElasticIpStatus
    ElasticIpStatus (..),
    newElasticIpStatus,
    elasticIpStatus_elasticIp,
    elasticIpStatus_status,

    -- * Endpoint
    Endpoint (..),
    newEndpoint,
    endpoint_address,
    endpoint_vpcEndpoints,
    endpoint_port,

    -- * Event
    Event (..),
    newEvent,
    event_eventId,
    event_severity,
    event_message,
    event_eventCategories,
    event_date,
    event_sourceIdentifier,
    event_sourceType,

    -- * EventCategoriesMap
    EventCategoriesMap (..),
    newEventCategoriesMap,
    eventCategoriesMap_events,
    eventCategoriesMap_sourceType,

    -- * EventInfoMap
    EventInfoMap (..),
    newEventInfoMap,
    eventInfoMap_eventId,
    eventInfoMap_severity,
    eventInfoMap_eventDescription,
    eventInfoMap_eventCategories,

    -- * EventSubscription
    EventSubscription (..),
    newEventSubscription,
    eventSubscription_custSubscriptionId,
    eventSubscription_status,
    eventSubscription_sourceIdsList,
    eventSubscription_severity,
    eventSubscription_eventCategoriesList,
    eventSubscription_enabled,
    eventSubscription_subscriptionCreationTime,
    eventSubscription_customerAwsId,
    eventSubscription_tags,
    eventSubscription_sourceType,
    eventSubscription_snsTopicArn,

    -- * HsmClientCertificate
    HsmClientCertificate (..),
    newHsmClientCertificate,
    hsmClientCertificate_hsmClientCertificatePublicKey,
    hsmClientCertificate_hsmClientCertificateIdentifier,
    hsmClientCertificate_tags,

    -- * HsmConfiguration
    HsmConfiguration (..),
    newHsmConfiguration,
    hsmConfiguration_tags,
    hsmConfiguration_hsmIpAddress,
    hsmConfiguration_description,
    hsmConfiguration_hsmPartitionName,
    hsmConfiguration_hsmConfigurationIdentifier,

    -- * HsmStatus
    HsmStatus (..),
    newHsmStatus,
    hsmStatus_status,
    hsmStatus_hsmClientCertificateIdentifier,
    hsmStatus_hsmConfigurationIdentifier,

    -- * IPRange
    IPRange (..),
    newIPRange,
    iPRange_status,
    iPRange_cidrip,
    iPRange_tags,

    -- * LoggingStatus
    LoggingStatus (..),
    newLoggingStatus,
    loggingStatus_lastSuccessfulDeliveryTime,
    loggingStatus_bucketName,
    loggingStatus_loggingEnabled,
    loggingStatus_lastFailureTime,
    loggingStatus_s3KeyPrefix,
    loggingStatus_lastFailureMessage,

    -- * MaintenanceTrack
    MaintenanceTrack (..),
    newMaintenanceTrack,
    maintenanceTrack_updateTargets,
    maintenanceTrack_databaseVersion,
    maintenanceTrack_maintenanceTrackName,

    -- * NodeConfigurationOption
    NodeConfigurationOption (..),
    newNodeConfigurationOption,
    nodeConfigurationOption_mode,
    nodeConfigurationOption_numberOfNodes,
    nodeConfigurationOption_estimatedDiskUtilizationPercent,
    nodeConfigurationOption_nodeType,

    -- * NodeConfigurationOptionsFilter
    NodeConfigurationOptionsFilter (..),
    newNodeConfigurationOptionsFilter,
    nodeConfigurationOptionsFilter_values,
    nodeConfigurationOptionsFilter_operator,
    nodeConfigurationOptionsFilter_name,

    -- * OrderableClusterOption
    OrderableClusterOption (..),
    newOrderableClusterOption,
    orderableClusterOption_availabilityZones,
    orderableClusterOption_clusterType,
    orderableClusterOption_nodeType,
    orderableClusterOption_clusterVersion,

    -- * Parameter
    Parameter (..),
    newParameter,
    parameter_allowedValues,
    parameter_source,
    parameter_parameterValue,
    parameter_applyType,
    parameter_parameterName,
    parameter_description,
    parameter_dataType,
    parameter_isModifiable,
    parameter_minimumEngineVersion,

    -- * PauseClusterMessage
    PauseClusterMessage (..),
    newPauseClusterMessage,
    pauseClusterMessage_clusterIdentifier,

    -- * PendingModifiedValues
    PendingModifiedValues (..),
    newPendingModifiedValues,
    pendingModifiedValues_encryptionType,
    pendingModifiedValues_enhancedVpcRouting,
    pendingModifiedValues_automatedSnapshotRetentionPeriod,
    pendingModifiedValues_masterUserPassword,
    pendingModifiedValues_publiclyAccessible,
    pendingModifiedValues_clusterType,
    pendingModifiedValues_clusterIdentifier,
    pendingModifiedValues_numberOfNodes,
    pendingModifiedValues_nodeType,
    pendingModifiedValues_clusterVersion,
    pendingModifiedValues_maintenanceTrackName,

    -- * RecurringCharge
    RecurringCharge (..),
    newRecurringCharge,
    recurringCharge_recurringChargeFrequency,
    recurringCharge_recurringChargeAmount,

    -- * ReservedNode
    ReservedNode (..),
    newReservedNode,
    reservedNode_reservedNodeOfferingType,
    reservedNode_reservedNodeId,
    reservedNode_reservedNodeOfferingId,
    reservedNode_duration,
    reservedNode_startTime,
    reservedNode_currencyCode,
    reservedNode_state,
    reservedNode_fixedPrice,
    reservedNode_nodeCount,
    reservedNode_usagePrice,
    reservedNode_offeringType,
    reservedNode_nodeType,
    reservedNode_recurringCharges,

    -- * ReservedNodeOffering
    ReservedNodeOffering (..),
    newReservedNodeOffering,
    reservedNodeOffering_reservedNodeOfferingType,
    reservedNodeOffering_reservedNodeOfferingId,
    reservedNodeOffering_duration,
    reservedNodeOffering_currencyCode,
    reservedNodeOffering_fixedPrice,
    reservedNodeOffering_usagePrice,
    reservedNodeOffering_offeringType,
    reservedNodeOffering_nodeType,
    reservedNodeOffering_recurringCharges,

    -- * ResizeClusterMessage
    ResizeClusterMessage (..),
    newResizeClusterMessage,
    resizeClusterMessage_classic,
    resizeClusterMessage_clusterType,
    resizeClusterMessage_numberOfNodes,
    resizeClusterMessage_nodeType,
    resizeClusterMessage_clusterIdentifier,

    -- * ResizeInfo
    ResizeInfo (..),
    newResizeInfo,
    resizeInfo_allowCancelResize,
    resizeInfo_resizeType,

    -- * ResizeProgressMessage
    ResizeProgressMessage (..),
    newResizeProgressMessage,
    resizeProgressMessage_status,
    resizeProgressMessage_estimatedTimeToCompletionInSeconds,
    resizeProgressMessage_importTablesNotStarted,
    resizeProgressMessage_targetNodeType,
    resizeProgressMessage_message,
    resizeProgressMessage_targetClusterType,
    resizeProgressMessage_avgResizeRateInMegaBytesPerSecond,
    resizeProgressMessage_targetEncryptionType,
    resizeProgressMessage_elapsedTimeInSeconds,
    resizeProgressMessage_targetNumberOfNodes,
    resizeProgressMessage_totalResizeDataInMegaBytes,
    resizeProgressMessage_progressInMegaBytes,
    resizeProgressMessage_dataTransferProgressPercent,
    resizeProgressMessage_importTablesCompleted,
    resizeProgressMessage_importTablesInProgress,
    resizeProgressMessage_resizeType,

    -- * RestoreStatus
    RestoreStatus (..),
    newRestoreStatus,
    restoreStatus_status,
    restoreStatus_estimatedTimeToCompletionInSeconds,
    restoreStatus_snapshotSizeInMegaBytes,
    restoreStatus_currentRestoreRateInMegaBytesPerSecond,
    restoreStatus_elapsedTimeInSeconds,
    restoreStatus_progressInMegaBytes,

    -- * ResumeClusterMessage
    ResumeClusterMessage (..),
    newResumeClusterMessage,
    resumeClusterMessage_clusterIdentifier,

    -- * RevisionTarget
    RevisionTarget (..),
    newRevisionTarget,
    revisionTarget_description,
    revisionTarget_databaseRevision,
    revisionTarget_databaseRevisionReleaseDate,

    -- * ScheduledAction
    ScheduledAction (..),
    newScheduledAction,
    scheduledAction_targetAction,
    scheduledAction_nextInvocations,
    scheduledAction_iamRole,
    scheduledAction_scheduledActionDescription,
    scheduledAction_startTime,
    scheduledAction_endTime,
    scheduledAction_state,
    scheduledAction_scheduledActionName,
    scheduledAction_schedule,

    -- * ScheduledActionFilter
    ScheduledActionFilter (..),
    newScheduledActionFilter,
    scheduledActionFilter_name,
    scheduledActionFilter_values,

    -- * ScheduledActionType
    ScheduledActionType (..),
    newScheduledActionType,
    scheduledActionType_resumeCluster,
    scheduledActionType_resizeCluster,
    scheduledActionType_pauseCluster,

    -- * Snapshot
    Snapshot (..),
    newSnapshot,
    snapshot_enhancedVpcRouting,
    snapshot_snapshotIdentifier,
    snapshot_status,
    snapshot_estimatedSecondsToCompletion,
    snapshot_encrypted,
    snapshot_clusterCreateTime,
    snapshot_manualSnapshotRemainingDays,
    snapshot_snapshotCreateTime,
    snapshot_currentBackupRateInMegaBytesPerSecond,
    snapshot_masterUsername,
    snapshot_encryptedWithHSM,
    snapshot_manualSnapshotRetentionPeriod,
    snapshot_kmsKeyId,
    snapshot_engineFullVersion,
    snapshot_availabilityZone,
    snapshot_restorableNodeTypes,
    snapshot_snapshotType,
    snapshot_accountsWithRestoreAccess,
    snapshot_actualIncrementalBackupSizeInMegaBytes,
    snapshot_clusterIdentifier,
    snapshot_tags,
    snapshot_numberOfNodes,
    snapshot_port,
    snapshot_totalBackupSizeInMegaBytes,
    snapshot_dbName,
    snapshot_backupProgressInMegaBytes,
    snapshot_elapsedTimeInSeconds,
    snapshot_nodeType,
    snapshot_ownerAccount,
    snapshot_clusterVersion,
    snapshot_vpcId,
    snapshot_sourceRegion,
    snapshot_snapshotRetentionStartTime,
    snapshot_maintenanceTrackName,

    -- * SnapshotCopyGrant
    SnapshotCopyGrant (..),
    newSnapshotCopyGrant,
    snapshotCopyGrant_snapshotCopyGrantName,
    snapshotCopyGrant_kmsKeyId,
    snapshotCopyGrant_tags,

    -- * SnapshotErrorMessage
    SnapshotErrorMessage (..),
    newSnapshotErrorMessage,
    snapshotErrorMessage_snapshotIdentifier,
    snapshotErrorMessage_failureCode,
    snapshotErrorMessage_snapshotClusterIdentifier,
    snapshotErrorMessage_failureReason,

    -- * SnapshotSchedule
    SnapshotSchedule (..),
    newSnapshotSchedule,
    snapshotSchedule_nextInvocations,
    snapshotSchedule_associatedClusters,
    snapshotSchedule_scheduleIdentifier,
    snapshotSchedule_scheduleDescription,
    snapshotSchedule_scheduleDefinitions,
    snapshotSchedule_associatedClusterCount,
    snapshotSchedule_tags,

    -- * SnapshotSortingEntity
    SnapshotSortingEntity (..),
    newSnapshotSortingEntity,
    snapshotSortingEntity_sortOrder,
    snapshotSortingEntity_attribute,

    -- * Subnet
    Subnet (..),
    newSubnet,
    subnet_subnetStatus,
    subnet_subnetIdentifier,
    subnet_subnetAvailabilityZone,

    -- * SupportedOperation
    SupportedOperation (..),
    newSupportedOperation,
    supportedOperation_operationName,

    -- * SupportedPlatform
    SupportedPlatform (..),
    newSupportedPlatform,
    supportedPlatform_name,

    -- * TableRestoreStatus
    TableRestoreStatus (..),
    newTableRestoreStatus,
    tableRestoreStatus_sourceDatabaseName,
    tableRestoreStatus_snapshotIdentifier,
    tableRestoreStatus_targetSchemaName,
    tableRestoreStatus_status,
    tableRestoreStatus_requestTime,
    tableRestoreStatus_sourceTableName,
    tableRestoreStatus_targetDatabaseName,
    tableRestoreStatus_message,
    tableRestoreStatus_sourceSchemaName,
    tableRestoreStatus_clusterIdentifier,
    tableRestoreStatus_progressInMegaBytes,
    tableRestoreStatus_newTableName,
    tableRestoreStatus_totalDataInMegaBytes,
    tableRestoreStatus_tableRestoreRequestId,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TaggedResource
    TaggedResource (..),
    newTaggedResource,
    taggedResource_resourceType,
    taggedResource_resourceName,
    taggedResource_tag,

    -- * UpdateTarget
    UpdateTarget (..),
    newUpdateTarget,
    updateTarget_supportedOperations,
    updateTarget_databaseVersion,
    updateTarget_maintenanceTrackName,

    -- * UsageLimit
    UsageLimit (..),
    newUsageLimit,
    usageLimit_amount,
    usageLimit_featureType,
    usageLimit_breachAction,
    usageLimit_limitType,
    usageLimit_clusterIdentifier,
    usageLimit_tags,
    usageLimit_period,
    usageLimit_usageLimitId,

    -- * VpcEndpoint
    VpcEndpoint (..),
    newVpcEndpoint,
    vpcEndpoint_vpcEndpointId,

    -- * VpcSecurityGroupMembership
    VpcSecurityGroupMembership (..),
    newVpcSecurityGroupMembership,
    vpcSecurityGroupMembership_status,
    vpcSecurityGroupMembership_vpcSecurityGroupId,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.AccountAttribute
import Network.AWS.Redshift.Types.AccountWithRestoreAccess
import Network.AWS.Redshift.Types.ActionType
import Network.AWS.Redshift.Types.AttributeValueTarget
import Network.AWS.Redshift.Types.AvailabilityZone
import Network.AWS.Redshift.Types.Cluster
import Network.AWS.Redshift.Types.ClusterAssociatedToSchedule
import Network.AWS.Redshift.Types.ClusterDbRevision
import Network.AWS.Redshift.Types.ClusterIamRole
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
import Network.AWS.Redshift.Types.ElasticIpStatus
import Network.AWS.Redshift.Types.Endpoint
import Network.AWS.Redshift.Types.Event
import Network.AWS.Redshift.Types.EventCategoriesMap
import Network.AWS.Redshift.Types.EventInfoMap
import Network.AWS.Redshift.Types.EventSubscription
import Network.AWS.Redshift.Types.HsmClientCertificate
import Network.AWS.Redshift.Types.HsmConfiguration
import Network.AWS.Redshift.Types.HsmStatus
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
import Network.AWS.Redshift.Types.VpcEndpoint
import Network.AWS.Redshift.Types.VpcSecurityGroupMembership
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2012-12-01@ of the Amazon Redshift SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "Redshift",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "redshift",
      Core._serviceSigningName = "redshift",
      Core._serviceVersion = "2012-12-01",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseXMLError "Redshift",
      Core._serviceRetry = retry
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
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | A cluster security group with the same name already exists.
_ClusterSecurityGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ClusterSecurityGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "ClusterSecurityGroupAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The snapshot identifier does not refer to an existing cluster snapshot.
_ClusterSnapshotNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ClusterSnapshotNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ClusterSnapshotNotFound"
    Prelude.. Core.hasStatus 404

-- | The definition you submitted is not supported.
_ScheduleDefinitionTypeUnsupportedFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ScheduleDefinitionTypeUnsupportedFault =
  Core._MatchServiceError
    defaultService
    "ScheduleDefinitionTypeUnsupported"
    Prelude.. Core.hasStatus 400

-- | The cluster subnet group name does not refer to an existing cluster
-- subnet group.
_ClusterSubnetGroupNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ClusterSubnetGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ClusterSubnetGroupNotFoundFault"
    Prelude.. Core.hasStatus 400

-- | The cluster subnet group does not cover all Availability Zones.
_InvalidVPCNetworkStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidVPCNetworkStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidVPCNetworkStateFault"
    Prelude.. Core.hasStatus 400

-- | The subscription request is invalid because it is a duplicate request.
-- This subscription request is already in progress.
_InvalidSubscriptionStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSubscriptionStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidSubscriptionStateFault"
    Prelude.. Core.hasStatus 400

-- | Could not find the specified S3 bucket.
_BucketNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BucketNotFoundFault =
  Core._MatchServiceError
    defaultService
    "BucketNotFoundFault"
    Prelude.. Core.hasStatus 400

-- | The encryption key has exceeded its grant limit in AWS KMS.
_LimitExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededFault =
  Core._MatchServiceError
    defaultService
    "LimitExceededFault"
    Prelude.. Core.hasStatus 400

-- | There is already an existing event notification subscription with the
-- specified name.
_SubscriptionAlreadyExistFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubscriptionAlreadyExistFault =
  Core._MatchServiceError
    defaultService
    "SubscriptionAlreadyExist"
    Prelude.. Core.hasStatus 400

-- | The request would result in the user exceeding the allowed number of
-- cluster security groups. For information about increasing your quota, go
-- to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_ClusterSecurityGroupQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ClusterSecurityGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "QuotaExceeded.ClusterSecurityGroup"
    Prelude.. Core.hasStatus 400

-- | The scheduled action is not valid.
_InvalidScheduledActionFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidScheduledActionFault =
  Core._MatchServiceError
    defaultService
    "InvalidScheduledAction"
    Prelude.. Core.hasStatus 400

-- | The value specified for the event severity was not one of the allowed
-- values, or it specified a severity that does not apply to the specified
-- source type. The allowed values are ERROR and INFO.
_SubscriptionSeverityNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubscriptionSeverityNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SubscriptionSeverityNotFound"
    Prelude.. Core.hasStatus 404

-- | Your account is not authorized to perform the requested operation.
_UnauthorizedOperation :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnauthorizedOperation =
  Core._MatchServiceError
    defaultService
    "UnauthorizedOperation"
    Prelude.. Core.hasStatus 400

-- | The cluster does not have read bucket or put object permissions on the
-- S3 bucket specified when enabling logging.
_InsufficientS3BucketPolicyFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InsufficientS3BucketPolicyFault =
  Core._MatchServiceError
    defaultService
    "InsufficientS3BucketPolicyFault"
    Prelude.. Core.hasStatus 400

-- | The number of nodes specified exceeds the allotted capacity of the
-- cluster.
_InsufficientClusterCapacityFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InsufficientClusterCapacityFault =
  Core._MatchServiceError
    defaultService
    "InsufficientClusterCapacity"
    Prelude.. Core.hasStatus 400

-- | The quota for HSM configurations has been reached. For information about
-- increasing your quota, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_HsmConfigurationQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_HsmConfigurationQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "HsmConfigurationQuotaExceededFault"
    Prelude.. Core.hasStatus 400

-- | The usage limit identifier can\'t be found.
_UsageLimitNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UsageLimitNotFoundFault =
  Core._MatchServiceError
    defaultService
    "UsageLimitNotFound"
    Prelude.. Core.hasStatus 404

-- | The tag is invalid.
_InvalidTagFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTagFault =
  Core._MatchServiceError
    defaultService
    "InvalidTagFault"
    Prelude.. Core.hasStatus 400

-- | You have exceeded the quota of snapshot schedules.
_SnapshotScheduleQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SnapshotScheduleQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "SnapshotScheduleQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | You do not have permission to publish to the specified Amazon SNS topic.
_SNSNoAuthorizationFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SNSNoAuthorizationFault =
  Core._MatchServiceError
    defaultService
    "SNSNoAuthorization"
    Prelude.. Core.hasStatus 400

-- | The scheduled action cannot be found.
_ScheduledActionNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ScheduledActionNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ScheduledActionNotFound"
    Prelude.. Core.hasStatus 400

-- | There is already an existing Amazon Redshift HSM client certificate with
-- the specified identifier.
_HsmClientCertificateAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_HsmClientCertificateAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "HsmClientCertificateAlreadyExistsFault"
    Prelude.. Core.hasStatus 400

-- | The resource could not be found.
_ResourceNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | An Amazon SNS topic with the specified Amazon Resource Name (ARN) does
-- not exist.
_SNSTopicArnNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SNSTopicArnNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SNSTopicArnNotFound"
    Prelude.. Core.hasStatus 404

-- | The restore is invalid.
_InvalidRestoreFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRestoreFault =
  Core._MatchServiceError
    defaultService
    "InvalidRestore"
    Prelude.. Core.hasStatus 406

-- | The scheduled action already exists.
_ScheduledActionAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ScheduledActionAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "ScheduledActionAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The @ClusterIdentifier@ parameter does not refer to an existing cluster.
_ClusterNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ClusterNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ClusterNotFound"
    Prelude.. Core.hasStatus 404

-- | The quota for scheduled actions exceeded.
_ScheduledActionQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ScheduledActionQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "ScheduledActionQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The action type specified for a scheduled action is not supported.
_ScheduledActionTypeUnsupportedFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ScheduledActionTypeUnsupportedFault =
  Core._MatchServiceError
    defaultService
    "ScheduledActionTypeUnsupported"
    Prelude.. Core.hasStatus 400

-- | The request would exceed the allowed number of event subscriptions for
-- this account. For information about increasing your quota, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_EventSubscriptionQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EventSubscriptionQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "EventSubscriptionQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | We could not find the specified snapshot schedule.
_SnapshotScheduleNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SnapshotScheduleNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SnapshotScheduleNotFound"
    Prelude.. Core.hasStatus 400

-- | Indicates that the Reserved Node being exchanged is not in an active
-- state.
_InvalidReservedNodeStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidReservedNodeStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidReservedNodeState"
    Prelude.. Core.hasStatus 400

-- | The specified snapshot schedule is already being updated.
_SnapshotScheduleUpdateInProgressFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SnapshotScheduleUpdateInProgressFault =
  Core._MatchServiceError
    defaultService
    "SnapshotScheduleUpdateInProgress"
    Prelude.. Core.hasStatus 400

-- | The maximum number for snapshot identifiers has been reached. The limit
-- is 100.
_BatchModifyClusterSnapshotsLimitExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BatchModifyClusterSnapshotsLimitExceededFault =
  Core._MatchServiceError
    defaultService
    "BatchModifyClusterSnapshotsLimitExceededFault"
    Prelude.. Core.hasStatus 400

-- | Indicates that the reserved node has already been exchanged.
_ReservedNodeAlreadyMigratedFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReservedNodeAlreadyMigratedFault =
  Core._MatchServiceError
    defaultService
    "ReservedNodeAlreadyMigrated"
    Prelude.. Core.hasStatus 400

-- | The specified options are incompatible.
_IncompatibleOrderableOptions :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IncompatibleOrderableOptions =
  Core._MatchServiceError
    defaultService
    "IncompatibleOrderableOptions"
    Prelude.. Core.hasStatus 400

-- | The state of the subnet is invalid.
_InvalidClusterSubnetStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidClusterSubnetStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidClusterSubnetStateFault"
    Prelude.. Core.hasStatus 400

-- | The request would exceed the allowed number of cluster instances for
-- this account. For information about increasing your quota, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_ClusterQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ClusterQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "ClusterQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The S3 bucket name is invalid. For more information about naming rules,
-- go to
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/BucketRestrictions.html Bucket Restrictions and Limitations>
-- in the Amazon Simple Storage Service (S3) Developer Guide.
_InvalidS3BucketNameFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidS3BucketNameFault =
  Core._MatchServiceError
    defaultService
    "InvalidS3BucketNameFault"
    Prelude.. Core.hasStatus 400

-- | The cluster subnet group cannot be deleted because it is in use.
_InvalidClusterSubnetGroupStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidClusterSubnetGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidClusterSubnetGroupStateFault"
    Prelude.. Core.hasStatus 400

-- | There is already an existing Amazon Redshift HSM configuration with the
-- specified identifier.
_HsmConfigurationAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_HsmConfigurationAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "HsmConfigurationAlreadyExistsFault"
    Prelude.. Core.hasStatus 400

-- | The cluster security group name does not refer to an existing cluster
-- security group.
_ClusterSecurityGroupNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ClusterSecurityGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ClusterSecurityGroupNotFound"
    Prelude.. Core.hasStatus 404

-- | The requested operation isn\'t supported.
_UnsupportedOperationFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedOperationFault =
  Core._MatchServiceError
    defaultService
    "UnsupportedOperation"
    Prelude.. Core.hasStatus 400

-- | The Elastic IP (EIP) is invalid or cannot be found.
_InvalidElasticIpFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidElasticIpFault =
  Core._MatchServiceError
    defaultService
    "InvalidElasticIpFault"
    Prelude.. Core.hasStatus 400

-- | The cluster snapshot schedule state is not valid.
_InvalidClusterSnapshotScheduleStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidClusterSnapshotScheduleStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidClusterSnapshotScheduleState"
    Prelude.. Core.hasStatus 400

-- | The specified @TableRestoreRequestId@ value was not found.
_TableRestoreNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TableRestoreNotFoundFault =
  Core._MatchServiceError
    defaultService
    "TableRestoreNotFoundFault"
    Prelude.. Core.hasStatus 400

-- | There is no Amazon Redshift HSM configuration with the specified
-- identifier.
_HsmConfigurationNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_HsmConfigurationNotFoundFault =
  Core._MatchServiceError
    defaultService
    "HsmConfigurationNotFoundFault"
    Prelude.. Core.hasStatus 400

-- | The specified CIDR block or EC2 security group is already authorized for
-- the specified cluster security group.
_AuthorizationAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AuthorizationAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "AuthorizationAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The state of the cluster security group is not @available@.
_InvalidClusterSecurityGroupStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidClusterSecurityGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidClusterSecurityGroupState"
    Prelude.. Core.hasStatus 400

-- | The usage limit already exists.
_UsageLimitAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UsageLimitAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "UsageLimitAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | Amazon SNS has responded that there is a problem with the specified
-- Amazon SNS topic.
_SNSInvalidTopicFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SNSInvalidTopicFault =
  Core._MatchServiceError
    defaultService
    "SNSInvalidTopic"
    Prelude.. Core.hasStatus 400

-- | The specified Amazon Redshift event source could not be found.
_SourceNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SourceNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SourceNotFound"
    Prelude.. Core.hasStatus 404

-- | The string specified for the logging S3 key prefix does not comply with
-- the documented constraints.
_InvalidS3KeyPrefixFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidS3KeyPrefixFault =
  Core._MatchServiceError
    defaultService
    "InvalidS3KeyPrefixFault"
    Prelude.. Core.hasStatus 400

-- | A request option was specified that is not supported.
_UnsupportedOptionFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedOptionFault =
  Core._MatchServiceError
    defaultService
    "UnsupportedOptionFault"
    Prelude.. Core.hasStatus 400

-- | The authorization quota for the cluster security group has been reached.
_AuthorizationQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AuthorizationQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "AuthorizationQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | Your request cannot be completed because a dependent internal service is
-- temporarily unavailable. Wait 30 to 60 seconds and try again.
_DependentServiceUnavailableFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DependentServiceUnavailableFault =
  Core._MatchServiceError
    defaultService
    "DependentServiceUnavailableFault"
    Prelude.. Core.hasStatus 400

-- | Cross-region snapshot copy was temporarily disabled. Try your request
-- again.
_CopyToRegionDisabledFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CopyToRegionDisabledFault =
  Core._MatchServiceError
    defaultService
    "CopyToRegionDisabledFault"
    Prelude.. Core.hasStatus 400

-- | Cluster is already on the latest database revision.
_ClusterOnLatestRevisionFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ClusterOnLatestRevisionFault =
  Core._MatchServiceError
    defaultService
    "ClusterOnLatestRevision"
    Prelude.. Core.hasStatus 400

-- | A specified subnet is already in use by another cluster.
_SubnetAlreadyInUse :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubnetAlreadyInUse =
  Core._MatchServiceError
    defaultService
    "SubnetAlreadyInUse"
    Prelude.. Core.hasStatus 400

-- | The request would result in user exceeding the allowed number of cluster
-- subnet groups. For information about increasing your quota, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_ClusterSubnetGroupQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ClusterSubnetGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "ClusterSubnetGroupQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The request would result in the user exceeding the allowed number of
-- cluster snapshots.
_ClusterSnapshotQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ClusterSnapshotQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "ClusterSnapshotQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The schedule you submitted isn\'t valid.
_InvalidScheduleFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidScheduleFault =
  Core._MatchServiceError
    defaultService
    "InvalidSchedule"
    Prelude.. Core.hasStatus 400

-- | The request would result in user exceeding the allowed number of subnets
-- in a cluster subnet groups. For information about increasing your quota,
-- go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_ClusterSubnetQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ClusterSubnetQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "ClusterSubnetQuotaExceededFault"
    Prelude.. Core.hasStatus 400

-- | The owner of the specified snapshot has not authorized your account to
-- access the snapshot.
_AccessToSnapshotDeniedFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessToSnapshotDeniedFault =
  Core._MatchServiceError
    defaultService
    "AccessToSnapshotDenied"
    Prelude.. Core.hasStatus 400

-- | The operation would exceed the number of nodes allotted to the account.
-- For information about increasing your quota, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_NumberOfNodesQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NumberOfNodesQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "NumberOfNodesQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The value specified for the @sourceDatabaseName@, @sourceSchemaName@, or
-- @sourceTableName@ parameter, or a combination of these, doesn\'t exist
-- in the snapshot.
_InvalidTableRestoreArgumentFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTableRestoreArgumentFault =
  Core._MatchServiceError
    defaultService
    "InvalidTableRestoreArgument"
    Prelude.. Core.hasStatus 400

-- | The provided cluster track name is not valid.
_InvalidClusterTrackFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidClusterTrackFault =
  Core._MatchServiceError
    defaultService
    "InvalidClusterTrack"
    Prelude.. Core.hasStatus 400

-- | The specified HSM client certificate is not in the @available@ state, or
-- it is still in use by one or more Amazon Redshift clusters.
_InvalidHsmClientCertificateStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidHsmClientCertificateStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidHsmClientCertificateStateFault"
    Prelude.. Core.hasStatus 400

-- | You have exceeded the number of tags allowed.
_TagLimitExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TagLimitExceededFault =
  Core._MatchServiceError
    defaultService
    "TagLimitExceededFault"
    Prelude.. Core.hasStatus 400

-- | The specified snapshot copy grant can\'t be found. Make sure that the
-- name is typed correctly and that the grant exists in the destination
-- region.
_SnapshotCopyGrantNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SnapshotCopyGrantNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SnapshotCopyGrantNotFoundFault"
    Prelude.. Core.hasStatus 400

-- | The account already has a cluster with the given identifier.
_ClusterAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ClusterAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "ClusterAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The specified cluster is not in the @available@ state.
_InvalidClusterStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidClusterStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidClusterState"
    Prelude.. Core.hasStatus 400

-- | The request would result in the user exceeding the allowed number of
-- cluster parameter groups. For information about increasing your quota,
-- go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_ClusterParameterGroupQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ClusterParameterGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "ClusterParameterGroupQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The specified region is incorrect or does not exist.
_UnknownSnapshotCopyRegionFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnknownSnapshotCopyRegionFault =
  Core._MatchServiceError
    defaultService
    "UnknownSnapshotCopyRegionFault"
    Prelude.. Core.hasStatus 404

-- | Cross-region snapshot copy was temporarily disabled. Try your request
-- again.
_SnapshotCopyDisabledFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SnapshotCopyDisabledFault =
  Core._MatchServiceError
    defaultService
    "SnapshotCopyDisabledFault"
    Prelude.. Core.hasStatus 400

-- | The usage limit is not valid.
_InvalidUsageLimitFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidUsageLimitFault =
  Core._MatchServiceError
    defaultService
    "InvalidUsageLimit"
    Prelude.. Core.hasStatus 400

-- | You have exceeded the allowed number of table restore requests. Wait for
-- your current table restore requests to complete before making a new
-- request.
_InProgressTableRestoreQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InProgressTableRestoreQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "InProgressTableRestoreQuotaExceededFault"
    Prelude.. Core.hasStatus 400

-- | Request would exceed the user\'s compute node quota. For information
-- about increasing your quota, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_ReservedNodeQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReservedNodeQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "ReservedNodeQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | There is no Amazon Redshift HSM client certificate with the specified
-- identifier.
_HsmClientCertificateNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_HsmClientCertificateNotFoundFault =
  Core._MatchServiceError
    defaultService
    "HsmClientCertificateNotFoundFault"
    Prelude.. Core.hasStatus 400

-- | An Amazon Redshift event with the specified event ID does not exist.
_SubscriptionEventIdNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubscriptionEventIdNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SubscriptionEventIdNotFound"
    Prelude.. Core.hasStatus 404

-- | The snapshot copy grant can\'t be deleted because it is used by one or
-- more clusters.
_InvalidSnapshotCopyGrantStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSnapshotCopyGrantStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidSnapshotCopyGrantStateFault"
    Prelude.. Core.hasStatus 400

-- | The snapshot copy grant can\'t be created because a grant with the same
-- name already exists.
_SnapshotCopyGrantAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SnapshotCopyGrantAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "SnapshotCopyGrantAlreadyExistsFault"
    Prelude.. Core.hasStatus 400

-- | A resize operation for the specified cluster is not found.
_ResizeNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResizeNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ResizeNotFound"
    Prelude.. Core.hasStatus 404

-- | The cluster already has cross-region snapshot copy enabled.
_SnapshotCopyAlreadyEnabledFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SnapshotCopyAlreadyEnabledFault =
  Core._MatchServiceError
    defaultService
    "SnapshotCopyAlreadyEnabledFault"
    Prelude.. Core.hasStatus 400

-- | The maximum number for a batch delete of snapshots has been reached. The
-- limit is 100.
_BatchDeleteRequestSizeExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BatchDeleteRequestSizeExceededFault =
  Core._MatchServiceError
    defaultService
    "BatchDeleteRequestSizeExceeded"
    Prelude.. Core.hasStatus 400

-- | The operation would exceed the number of nodes allowed for a cluster.
_NumberOfNodesPerClusterLimitExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NumberOfNodesPerClusterLimitExceededFault =
  Core._MatchServiceError
    defaultService
    "NumberOfNodesPerClusterLimitExceeded"
    Prelude.. Core.hasStatus 400

-- | User already has a reservation with the given identifier.
_ReservedNodeAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReservedNodeAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "ReservedNodeAlreadyExists"
    Prelude.. Core.hasStatus 404

-- | A cluster parameter group with the same name already exists.
_ClusterParameterGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ClusterParameterGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "ClusterParameterGroupAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The AWS account has exceeded the maximum number of snapshot copy grants
-- in this region.
_SnapshotCopyGrantQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SnapshotCopyGrantQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "SnapshotCopyGrantQuotaExceededFault"
    Prelude.. Core.hasStatus 400

-- | The cluster parameter group action can not be completed because another
-- task is in progress that involves the parameter group. Wait a few
-- moments and try the operation again.
_InvalidClusterParameterGroupStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidClusterParameterGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidClusterParameterGroupState"
    Prelude.. Core.hasStatus 400

-- | The cluster already has cross-region snapshot copy disabled.
_SnapshotCopyAlreadyDisabledFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SnapshotCopyAlreadyDisabledFault =
  Core._MatchServiceError
    defaultService
    "SnapshotCopyAlreadyDisabledFault"
    Prelude.. Core.hasStatus 400

-- | The specified snapshot schedule already exists.
_SnapshotScheduleAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SnapshotScheduleAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "SnapshotScheduleAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | Specified offering does not exist.
_ReservedNodeOfferingNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReservedNodeOfferingNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ReservedNodeOfferingNotFound"
    Prelude.. Core.hasStatus 404

-- | The specified reserved compute node not found.
_ReservedNodeNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReservedNodeNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ReservedNodeNotFound"
    Prelude.. Core.hasStatus 404

-- | The quota for HSM client certificates has been reached. For information
-- about increasing your quota, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_HsmClientCertificateQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_HsmClientCertificateQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "HsmClientCertificateQuotaExceededFault"
    Prelude.. Core.hasStatus 400

-- | The parameter group name does not refer to an existing parameter group.
_ClusterParameterGroupNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ClusterParameterGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ClusterParameterGroupNotFound"
    Prelude.. Core.hasStatus 404

-- | The value specified for the event category was not one of the allowed
-- values, or it specified a category that does not apply to the specified
-- source type. The allowed values are Configuration, Management,
-- Monitoring, and Security.
_SubscriptionCategoryNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubscriptionCategoryNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SubscriptionCategoryNotFound"
    Prelude.. Core.hasStatus 404

-- | The specified CIDR IP range or EC2 security group is not authorized for
-- the specified cluster security group.
_AuthorizationNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AuthorizationNotFoundFault =
  Core._MatchServiceError
    defaultService
    "AuthorizationNotFound"
    Prelude.. Core.hasStatus 404

-- | The number of tables in the cluster exceeds the limit for the requested
-- new cluster node type.
_TableLimitExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TableLimitExceededFault =
  Core._MatchServiceError
    defaultService
    "TableLimitExceeded"
    Prelude.. Core.hasStatus 400

-- | The specified cluster snapshot is not in the @available@ state, or other
-- accounts are authorized to access the snapshot.
_InvalidClusterSnapshotStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidClusterSnapshotStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidClusterSnapshotState"
    Prelude.. Core.hasStatus 400

-- | The retention period specified is either in the past or is not a valid
-- value.
--
-- The value must be either -1 or an integer between 1 and 3,653.
_InvalidRetentionPeriodFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRetentionPeriodFault =
  Core._MatchServiceError
    defaultService
    "InvalidRetentionPeriodFault"
    Prelude.. Core.hasStatus 400

-- | A /ClusterSubnetGroupName/ is already used by an existing cluster subnet
-- group.
_ClusterSubnetGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ClusterSubnetGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "ClusterSubnetGroupAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The requested subnet is not valid, or not all of the subnets are in the
-- same VPC.
_InvalidSubnet :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSubnet =
  Core._MatchServiceError
    defaultService
    "InvalidSubnet"
    Prelude.. Core.hasStatus 400

-- | An Amazon Redshift event notification subscription with the specified
-- name does not exist.
_SubscriptionNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubscriptionNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SubscriptionNotFound"
    Prelude.. Core.hasStatus 404

-- | The value specified as a snapshot identifier is already used by an
-- existing snapshot.
_ClusterSnapshotAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ClusterSnapshotAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "ClusterSnapshotAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The specified HSM configuration is not in the @available@ state, or it
-- is still in use by one or more Amazon Redshift clusters.
_InvalidHsmConfigurationStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidHsmConfigurationStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidHsmConfigurationStateFault"
    Prelude.. Core.hasStatus 400

-- | The request cannot be completed because a dependent service is
-- throttling requests made by Amazon Redshift on your behalf. Wait and
-- retry the request.
_DependentServiceRequestThrottlingFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DependentServiceRequestThrottlingFault =
  Core._MatchServiceError
    defaultService
    "DependentServiceRequestThrottlingFault"
    Prelude.. Core.hasStatus 400
