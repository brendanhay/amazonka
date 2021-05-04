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
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev = "Redshift",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcEndpointPrefix = "redshift",
      Prelude._svcSigningName = "redshift",
      Prelude._svcVersion = "2012-12-01",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError = Prelude.parseXMLError "Redshift",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | A cluster security group with the same name already exists.
_ClusterSecurityGroupAlreadyExistsFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ClusterSecurityGroupAlreadyExistsFault =
  Prelude._MatchServiceError
    defaultService
    "ClusterSecurityGroupAlreadyExists"
    Prelude.. Prelude.hasStatus 400

-- | The snapshot identifier does not refer to an existing cluster snapshot.
_ClusterSnapshotNotFoundFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ClusterSnapshotNotFoundFault =
  Prelude._MatchServiceError
    defaultService
    "ClusterSnapshotNotFound"
    Prelude.. Prelude.hasStatus 404

-- | The definition you submitted is not supported.
_ScheduleDefinitionTypeUnsupportedFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ScheduleDefinitionTypeUnsupportedFault =
  Prelude._MatchServiceError
    defaultService
    "ScheduleDefinitionTypeUnsupported"
    Prelude.. Prelude.hasStatus 400

-- | The cluster subnet group name does not refer to an existing cluster
-- subnet group.
_ClusterSubnetGroupNotFoundFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ClusterSubnetGroupNotFoundFault =
  Prelude._MatchServiceError
    defaultService
    "ClusterSubnetGroupNotFoundFault"
    Prelude.. Prelude.hasStatus 400

-- | The cluster subnet group does not cover all Availability Zones.
_InvalidVPCNetworkStateFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidVPCNetworkStateFault =
  Prelude._MatchServiceError
    defaultService
    "InvalidVPCNetworkStateFault"
    Prelude.. Prelude.hasStatus 400

-- | The subscription request is invalid because it is a duplicate request.
-- This subscription request is already in progress.
_InvalidSubscriptionStateFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidSubscriptionStateFault =
  Prelude._MatchServiceError
    defaultService
    "InvalidSubscriptionStateFault"
    Prelude.. Prelude.hasStatus 400

-- | Could not find the specified S3 bucket.
_BucketNotFoundFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_BucketNotFoundFault =
  Prelude._MatchServiceError
    defaultService
    "BucketNotFoundFault"
    Prelude.. Prelude.hasStatus 400

-- | The encryption key has exceeded its grant limit in AWS KMS.
_LimitExceededFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_LimitExceededFault =
  Prelude._MatchServiceError
    defaultService
    "LimitExceededFault"
    Prelude.. Prelude.hasStatus 400

-- | There is already an existing event notification subscription with the
-- specified name.
_SubscriptionAlreadyExistFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_SubscriptionAlreadyExistFault =
  Prelude._MatchServiceError
    defaultService
    "SubscriptionAlreadyExist"
    Prelude.. Prelude.hasStatus 400

-- | The request would result in the user exceeding the allowed number of
-- cluster security groups. For information about increasing your quota, go
-- to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_ClusterSecurityGroupQuotaExceededFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ClusterSecurityGroupQuotaExceededFault =
  Prelude._MatchServiceError
    defaultService
    "QuotaExceeded.ClusterSecurityGroup"
    Prelude.. Prelude.hasStatus 400

-- | The scheduled action is not valid.
_InvalidScheduledActionFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidScheduledActionFault =
  Prelude._MatchServiceError
    defaultService
    "InvalidScheduledAction"
    Prelude.. Prelude.hasStatus 400

-- | The value specified for the event severity was not one of the allowed
-- values, or it specified a severity that does not apply to the specified
-- source type. The allowed values are ERROR and INFO.
_SubscriptionSeverityNotFoundFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_SubscriptionSeverityNotFoundFault =
  Prelude._MatchServiceError
    defaultService
    "SubscriptionSeverityNotFound"
    Prelude.. Prelude.hasStatus 404

-- | Your account is not authorized to perform the requested operation.
_UnauthorizedOperation :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UnauthorizedOperation =
  Prelude._MatchServiceError
    defaultService
    "UnauthorizedOperation"
    Prelude.. Prelude.hasStatus 400

-- | The cluster does not have read bucket or put object permissions on the
-- S3 bucket specified when enabling logging.
_InsufficientS3BucketPolicyFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InsufficientS3BucketPolicyFault =
  Prelude._MatchServiceError
    defaultService
    "InsufficientS3BucketPolicyFault"
    Prelude.. Prelude.hasStatus 400

-- | The number of nodes specified exceeds the allotted capacity of the
-- cluster.
_InsufficientClusterCapacityFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InsufficientClusterCapacityFault =
  Prelude._MatchServiceError
    defaultService
    "InsufficientClusterCapacity"
    Prelude.. Prelude.hasStatus 400

-- | The quota for HSM configurations has been reached. For information about
-- increasing your quota, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_HsmConfigurationQuotaExceededFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_HsmConfigurationQuotaExceededFault =
  Prelude._MatchServiceError
    defaultService
    "HsmConfigurationQuotaExceededFault"
    Prelude.. Prelude.hasStatus 400

-- | The usage limit identifier can\'t be found.
_UsageLimitNotFoundFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UsageLimitNotFoundFault =
  Prelude._MatchServiceError
    defaultService
    "UsageLimitNotFound"
    Prelude.. Prelude.hasStatus 404

-- | The tag is invalid.
_InvalidTagFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidTagFault =
  Prelude._MatchServiceError
    defaultService
    "InvalidTagFault"
    Prelude.. Prelude.hasStatus 400

-- | You have exceeded the quota of snapshot schedules.
_SnapshotScheduleQuotaExceededFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_SnapshotScheduleQuotaExceededFault =
  Prelude._MatchServiceError
    defaultService
    "SnapshotScheduleQuotaExceeded"
    Prelude.. Prelude.hasStatus 400

-- | You do not have permission to publish to the specified Amazon SNS topic.
_SNSNoAuthorizationFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_SNSNoAuthorizationFault =
  Prelude._MatchServiceError
    defaultService
    "SNSNoAuthorization"
    Prelude.. Prelude.hasStatus 400

-- | The scheduled action cannot be found.
_ScheduledActionNotFoundFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ScheduledActionNotFoundFault =
  Prelude._MatchServiceError
    defaultService
    "ScheduledActionNotFound"
    Prelude.. Prelude.hasStatus 400

-- | There is already an existing Amazon Redshift HSM client certificate with
-- the specified identifier.
_HsmClientCertificateAlreadyExistsFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_HsmClientCertificateAlreadyExistsFault =
  Prelude._MatchServiceError
    defaultService
    "HsmClientCertificateAlreadyExistsFault"
    Prelude.. Prelude.hasStatus 400

-- | The resource could not be found.
_ResourceNotFoundFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceNotFoundFault =
  Prelude._MatchServiceError
    defaultService
    "ResourceNotFoundFault"
    Prelude.. Prelude.hasStatus 404

-- | An Amazon SNS topic with the specified Amazon Resource Name (ARN) does
-- not exist.
_SNSTopicArnNotFoundFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_SNSTopicArnNotFoundFault =
  Prelude._MatchServiceError
    defaultService
    "SNSTopicArnNotFound"
    Prelude.. Prelude.hasStatus 404

-- | The restore is invalid.
_InvalidRestoreFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidRestoreFault =
  Prelude._MatchServiceError
    defaultService
    "InvalidRestore"
    Prelude.. Prelude.hasStatus 406

-- | The scheduled action already exists.
_ScheduledActionAlreadyExistsFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ScheduledActionAlreadyExistsFault =
  Prelude._MatchServiceError
    defaultService
    "ScheduledActionAlreadyExists"
    Prelude.. Prelude.hasStatus 400

-- | The @ClusterIdentifier@ parameter does not refer to an existing cluster.
_ClusterNotFoundFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ClusterNotFoundFault =
  Prelude._MatchServiceError
    defaultService
    "ClusterNotFound"
    Prelude.. Prelude.hasStatus 404

-- | The quota for scheduled actions exceeded.
_ScheduledActionQuotaExceededFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ScheduledActionQuotaExceededFault =
  Prelude._MatchServiceError
    defaultService
    "ScheduledActionQuotaExceeded"
    Prelude.. Prelude.hasStatus 400

-- | The action type specified for a scheduled action is not supported.
_ScheduledActionTypeUnsupportedFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ScheduledActionTypeUnsupportedFault =
  Prelude._MatchServiceError
    defaultService
    "ScheduledActionTypeUnsupported"
    Prelude.. Prelude.hasStatus 400

-- | The request would exceed the allowed number of event subscriptions for
-- this account. For information about increasing your quota, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_EventSubscriptionQuotaExceededFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_EventSubscriptionQuotaExceededFault =
  Prelude._MatchServiceError
    defaultService
    "EventSubscriptionQuotaExceeded"
    Prelude.. Prelude.hasStatus 400

-- | We could not find the specified snapshot schedule.
_SnapshotScheduleNotFoundFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_SnapshotScheduleNotFoundFault =
  Prelude._MatchServiceError
    defaultService
    "SnapshotScheduleNotFound"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that the Reserved Node being exchanged is not in an active
-- state.
_InvalidReservedNodeStateFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidReservedNodeStateFault =
  Prelude._MatchServiceError
    defaultService
    "InvalidReservedNodeState"
    Prelude.. Prelude.hasStatus 400

-- | The specified snapshot schedule is already being updated.
_SnapshotScheduleUpdateInProgressFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_SnapshotScheduleUpdateInProgressFault =
  Prelude._MatchServiceError
    defaultService
    "SnapshotScheduleUpdateInProgress"
    Prelude.. Prelude.hasStatus 400

-- | The maximum number for snapshot identifiers has been reached. The limit
-- is 100.
_BatchModifyClusterSnapshotsLimitExceededFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_BatchModifyClusterSnapshotsLimitExceededFault =
  Prelude._MatchServiceError
    defaultService
    "BatchModifyClusterSnapshotsLimitExceededFault"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that the reserved node has already been exchanged.
_ReservedNodeAlreadyMigratedFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ReservedNodeAlreadyMigratedFault =
  Prelude._MatchServiceError
    defaultService
    "ReservedNodeAlreadyMigrated"
    Prelude.. Prelude.hasStatus 400

-- | The specified options are incompatible.
_IncompatibleOrderableOptions :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_IncompatibleOrderableOptions =
  Prelude._MatchServiceError
    defaultService
    "IncompatibleOrderableOptions"
    Prelude.. Prelude.hasStatus 400

-- | The state of the subnet is invalid.
_InvalidClusterSubnetStateFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidClusterSubnetStateFault =
  Prelude._MatchServiceError
    defaultService
    "InvalidClusterSubnetStateFault"
    Prelude.. Prelude.hasStatus 400

-- | The request would exceed the allowed number of cluster instances for
-- this account. For information about increasing your quota, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_ClusterQuotaExceededFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ClusterQuotaExceededFault =
  Prelude._MatchServiceError
    defaultService
    "ClusterQuotaExceeded"
    Prelude.. Prelude.hasStatus 400

-- | The S3 bucket name is invalid. For more information about naming rules,
-- go to
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/BucketRestrictions.html Bucket Restrictions and Limitations>
-- in the Amazon Simple Storage Service (S3) Developer Guide.
_InvalidS3BucketNameFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidS3BucketNameFault =
  Prelude._MatchServiceError
    defaultService
    "InvalidS3BucketNameFault"
    Prelude.. Prelude.hasStatus 400

-- | The cluster subnet group cannot be deleted because it is in use.
_InvalidClusterSubnetGroupStateFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidClusterSubnetGroupStateFault =
  Prelude._MatchServiceError
    defaultService
    "InvalidClusterSubnetGroupStateFault"
    Prelude.. Prelude.hasStatus 400

-- | There is already an existing Amazon Redshift HSM configuration with the
-- specified identifier.
_HsmConfigurationAlreadyExistsFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_HsmConfigurationAlreadyExistsFault =
  Prelude._MatchServiceError
    defaultService
    "HsmConfigurationAlreadyExistsFault"
    Prelude.. Prelude.hasStatus 400

-- | The cluster security group name does not refer to an existing cluster
-- security group.
_ClusterSecurityGroupNotFoundFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ClusterSecurityGroupNotFoundFault =
  Prelude._MatchServiceError
    defaultService
    "ClusterSecurityGroupNotFound"
    Prelude.. Prelude.hasStatus 404

-- | The requested operation isn\'t supported.
_UnsupportedOperationFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UnsupportedOperationFault =
  Prelude._MatchServiceError
    defaultService
    "UnsupportedOperation"
    Prelude.. Prelude.hasStatus 400

-- | The Elastic IP (EIP) is invalid or cannot be found.
_InvalidElasticIpFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidElasticIpFault =
  Prelude._MatchServiceError
    defaultService
    "InvalidElasticIpFault"
    Prelude.. Prelude.hasStatus 400

-- | The cluster snapshot schedule state is not valid.
_InvalidClusterSnapshotScheduleStateFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidClusterSnapshotScheduleStateFault =
  Prelude._MatchServiceError
    defaultService
    "InvalidClusterSnapshotScheduleState"
    Prelude.. Prelude.hasStatus 400

-- | The specified @TableRestoreRequestId@ value was not found.
_TableRestoreNotFoundFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TableRestoreNotFoundFault =
  Prelude._MatchServiceError
    defaultService
    "TableRestoreNotFoundFault"
    Prelude.. Prelude.hasStatus 400

-- | There is no Amazon Redshift HSM configuration with the specified
-- identifier.
_HsmConfigurationNotFoundFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_HsmConfigurationNotFoundFault =
  Prelude._MatchServiceError
    defaultService
    "HsmConfigurationNotFoundFault"
    Prelude.. Prelude.hasStatus 400

-- | The specified CIDR block or EC2 security group is already authorized for
-- the specified cluster security group.
_AuthorizationAlreadyExistsFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_AuthorizationAlreadyExistsFault =
  Prelude._MatchServiceError
    defaultService
    "AuthorizationAlreadyExists"
    Prelude.. Prelude.hasStatus 400

-- | The state of the cluster security group is not @available@.
_InvalidClusterSecurityGroupStateFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidClusterSecurityGroupStateFault =
  Prelude._MatchServiceError
    defaultService
    "InvalidClusterSecurityGroupState"
    Prelude.. Prelude.hasStatus 400

-- | The usage limit already exists.
_UsageLimitAlreadyExistsFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UsageLimitAlreadyExistsFault =
  Prelude._MatchServiceError
    defaultService
    "UsageLimitAlreadyExists"
    Prelude.. Prelude.hasStatus 400

-- | Amazon SNS has responded that there is a problem with the specified
-- Amazon SNS topic.
_SNSInvalidTopicFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_SNSInvalidTopicFault =
  Prelude._MatchServiceError
    defaultService
    "SNSInvalidTopic"
    Prelude.. Prelude.hasStatus 400

-- | The specified Amazon Redshift event source could not be found.
_SourceNotFoundFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_SourceNotFoundFault =
  Prelude._MatchServiceError
    defaultService
    "SourceNotFound"
    Prelude.. Prelude.hasStatus 404

-- | The string specified for the logging S3 key prefix does not comply with
-- the documented constraints.
_InvalidS3KeyPrefixFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidS3KeyPrefixFault =
  Prelude._MatchServiceError
    defaultService
    "InvalidS3KeyPrefixFault"
    Prelude.. Prelude.hasStatus 400

-- | A request option was specified that is not supported.
_UnsupportedOptionFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UnsupportedOptionFault =
  Prelude._MatchServiceError
    defaultService
    "UnsupportedOptionFault"
    Prelude.. Prelude.hasStatus 400

-- | The authorization quota for the cluster security group has been reached.
_AuthorizationQuotaExceededFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_AuthorizationQuotaExceededFault =
  Prelude._MatchServiceError
    defaultService
    "AuthorizationQuotaExceeded"
    Prelude.. Prelude.hasStatus 400

-- | Your request cannot be completed because a dependent internal service is
-- temporarily unavailable. Wait 30 to 60 seconds and try again.
_DependentServiceUnavailableFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DependentServiceUnavailableFault =
  Prelude._MatchServiceError
    defaultService
    "DependentServiceUnavailableFault"
    Prelude.. Prelude.hasStatus 400

-- | Cross-region snapshot copy was temporarily disabled. Try your request
-- again.
_CopyToRegionDisabledFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CopyToRegionDisabledFault =
  Prelude._MatchServiceError
    defaultService
    "CopyToRegionDisabledFault"
    Prelude.. Prelude.hasStatus 400

-- | Cluster is already on the latest database revision.
_ClusterOnLatestRevisionFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ClusterOnLatestRevisionFault =
  Prelude._MatchServiceError
    defaultService
    "ClusterOnLatestRevision"
    Prelude.. Prelude.hasStatus 400

-- | A specified subnet is already in use by another cluster.
_SubnetAlreadyInUse :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_SubnetAlreadyInUse =
  Prelude._MatchServiceError
    defaultService
    "SubnetAlreadyInUse"
    Prelude.. Prelude.hasStatus 400

-- | The request would result in user exceeding the allowed number of cluster
-- subnet groups. For information about increasing your quota, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_ClusterSubnetGroupQuotaExceededFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ClusterSubnetGroupQuotaExceededFault =
  Prelude._MatchServiceError
    defaultService
    "ClusterSubnetGroupQuotaExceeded"
    Prelude.. Prelude.hasStatus 400

-- | The request would result in the user exceeding the allowed number of
-- cluster snapshots.
_ClusterSnapshotQuotaExceededFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ClusterSnapshotQuotaExceededFault =
  Prelude._MatchServiceError
    defaultService
    "ClusterSnapshotQuotaExceeded"
    Prelude.. Prelude.hasStatus 400

-- | The schedule you submitted isn\'t valid.
_InvalidScheduleFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidScheduleFault =
  Prelude._MatchServiceError
    defaultService
    "InvalidSchedule"
    Prelude.. Prelude.hasStatus 400

-- | The request would result in user exceeding the allowed number of subnets
-- in a cluster subnet groups. For information about increasing your quota,
-- go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_ClusterSubnetQuotaExceededFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ClusterSubnetQuotaExceededFault =
  Prelude._MatchServiceError
    defaultService
    "ClusterSubnetQuotaExceededFault"
    Prelude.. Prelude.hasStatus 400

-- | The owner of the specified snapshot has not authorized your account to
-- access the snapshot.
_AccessToSnapshotDeniedFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_AccessToSnapshotDeniedFault =
  Prelude._MatchServiceError
    defaultService
    "AccessToSnapshotDenied"
    Prelude.. Prelude.hasStatus 400

-- | The operation would exceed the number of nodes allotted to the account.
-- For information about increasing your quota, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_NumberOfNodesQuotaExceededFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NumberOfNodesQuotaExceededFault =
  Prelude._MatchServiceError
    defaultService
    "NumberOfNodesQuotaExceeded"
    Prelude.. Prelude.hasStatus 400

-- | The value specified for the @sourceDatabaseName@, @sourceSchemaName@, or
-- @sourceTableName@ parameter, or a combination of these, doesn\'t exist
-- in the snapshot.
_InvalidTableRestoreArgumentFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidTableRestoreArgumentFault =
  Prelude._MatchServiceError
    defaultService
    "InvalidTableRestoreArgument"
    Prelude.. Prelude.hasStatus 400

-- | The provided cluster track name is not valid.
_InvalidClusterTrackFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidClusterTrackFault =
  Prelude._MatchServiceError
    defaultService
    "InvalidClusterTrack"
    Prelude.. Prelude.hasStatus 400

-- | The specified HSM client certificate is not in the @available@ state, or
-- it is still in use by one or more Amazon Redshift clusters.
_InvalidHsmClientCertificateStateFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidHsmClientCertificateStateFault =
  Prelude._MatchServiceError
    defaultService
    "InvalidHsmClientCertificateStateFault"
    Prelude.. Prelude.hasStatus 400

-- | You have exceeded the number of tags allowed.
_TagLimitExceededFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TagLimitExceededFault =
  Prelude._MatchServiceError
    defaultService
    "TagLimitExceededFault"
    Prelude.. Prelude.hasStatus 400

-- | The specified snapshot copy grant can\'t be found. Make sure that the
-- name is typed correctly and that the grant exists in the destination
-- region.
_SnapshotCopyGrantNotFoundFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_SnapshotCopyGrantNotFoundFault =
  Prelude._MatchServiceError
    defaultService
    "SnapshotCopyGrantNotFoundFault"
    Prelude.. Prelude.hasStatus 400

-- | The account already has a cluster with the given identifier.
_ClusterAlreadyExistsFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ClusterAlreadyExistsFault =
  Prelude._MatchServiceError
    defaultService
    "ClusterAlreadyExists"
    Prelude.. Prelude.hasStatus 400

-- | The specified cluster is not in the @available@ state.
_InvalidClusterStateFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidClusterStateFault =
  Prelude._MatchServiceError
    defaultService
    "InvalidClusterState"
    Prelude.. Prelude.hasStatus 400

-- | The request would result in the user exceeding the allowed number of
-- cluster parameter groups. For information about increasing your quota,
-- go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_ClusterParameterGroupQuotaExceededFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ClusterParameterGroupQuotaExceededFault =
  Prelude._MatchServiceError
    defaultService
    "ClusterParameterGroupQuotaExceeded"
    Prelude.. Prelude.hasStatus 400

-- | The specified region is incorrect or does not exist.
_UnknownSnapshotCopyRegionFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UnknownSnapshotCopyRegionFault =
  Prelude._MatchServiceError
    defaultService
    "UnknownSnapshotCopyRegionFault"
    Prelude.. Prelude.hasStatus 404

-- | Cross-region snapshot copy was temporarily disabled. Try your request
-- again.
_SnapshotCopyDisabledFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_SnapshotCopyDisabledFault =
  Prelude._MatchServiceError
    defaultService
    "SnapshotCopyDisabledFault"
    Prelude.. Prelude.hasStatus 400

-- | The usage limit is not valid.
_InvalidUsageLimitFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidUsageLimitFault =
  Prelude._MatchServiceError
    defaultService
    "InvalidUsageLimit"
    Prelude.. Prelude.hasStatus 400

-- | You have exceeded the allowed number of table restore requests. Wait for
-- your current table restore requests to complete before making a new
-- request.
_InProgressTableRestoreQuotaExceededFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InProgressTableRestoreQuotaExceededFault =
  Prelude._MatchServiceError
    defaultService
    "InProgressTableRestoreQuotaExceededFault"
    Prelude.. Prelude.hasStatus 400

-- | Request would exceed the user\'s compute node quota. For information
-- about increasing your quota, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_ReservedNodeQuotaExceededFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ReservedNodeQuotaExceededFault =
  Prelude._MatchServiceError
    defaultService
    "ReservedNodeQuotaExceeded"
    Prelude.. Prelude.hasStatus 400

-- | There is no Amazon Redshift HSM client certificate with the specified
-- identifier.
_HsmClientCertificateNotFoundFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_HsmClientCertificateNotFoundFault =
  Prelude._MatchServiceError
    defaultService
    "HsmClientCertificateNotFoundFault"
    Prelude.. Prelude.hasStatus 400

-- | An Amazon Redshift event with the specified event ID does not exist.
_SubscriptionEventIdNotFoundFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_SubscriptionEventIdNotFoundFault =
  Prelude._MatchServiceError
    defaultService
    "SubscriptionEventIdNotFound"
    Prelude.. Prelude.hasStatus 404

-- | The snapshot copy grant can\'t be deleted because it is used by one or
-- more clusters.
_InvalidSnapshotCopyGrantStateFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidSnapshotCopyGrantStateFault =
  Prelude._MatchServiceError
    defaultService
    "InvalidSnapshotCopyGrantStateFault"
    Prelude.. Prelude.hasStatus 400

-- | The snapshot copy grant can\'t be created because a grant with the same
-- name already exists.
_SnapshotCopyGrantAlreadyExistsFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_SnapshotCopyGrantAlreadyExistsFault =
  Prelude._MatchServiceError
    defaultService
    "SnapshotCopyGrantAlreadyExistsFault"
    Prelude.. Prelude.hasStatus 400

-- | A resize operation for the specified cluster is not found.
_ResizeNotFoundFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResizeNotFoundFault =
  Prelude._MatchServiceError
    defaultService
    "ResizeNotFound"
    Prelude.. Prelude.hasStatus 404

-- | The cluster already has cross-region snapshot copy enabled.
_SnapshotCopyAlreadyEnabledFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_SnapshotCopyAlreadyEnabledFault =
  Prelude._MatchServiceError
    defaultService
    "SnapshotCopyAlreadyEnabledFault"
    Prelude.. Prelude.hasStatus 400

-- | The maximum number for a batch delete of snapshots has been reached. The
-- limit is 100.
_BatchDeleteRequestSizeExceededFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_BatchDeleteRequestSizeExceededFault =
  Prelude._MatchServiceError
    defaultService
    "BatchDeleteRequestSizeExceeded"
    Prelude.. Prelude.hasStatus 400

-- | The operation would exceed the number of nodes allowed for a cluster.
_NumberOfNodesPerClusterLimitExceededFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NumberOfNodesPerClusterLimitExceededFault =
  Prelude._MatchServiceError
    defaultService
    "NumberOfNodesPerClusterLimitExceeded"
    Prelude.. Prelude.hasStatus 400

-- | User already has a reservation with the given identifier.
_ReservedNodeAlreadyExistsFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ReservedNodeAlreadyExistsFault =
  Prelude._MatchServiceError
    defaultService
    "ReservedNodeAlreadyExists"
    Prelude.. Prelude.hasStatus 404

-- | A cluster parameter group with the same name already exists.
_ClusterParameterGroupAlreadyExistsFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ClusterParameterGroupAlreadyExistsFault =
  Prelude._MatchServiceError
    defaultService
    "ClusterParameterGroupAlreadyExists"
    Prelude.. Prelude.hasStatus 400

-- | The AWS account has exceeded the maximum number of snapshot copy grants
-- in this region.
_SnapshotCopyGrantQuotaExceededFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_SnapshotCopyGrantQuotaExceededFault =
  Prelude._MatchServiceError
    defaultService
    "SnapshotCopyGrantQuotaExceededFault"
    Prelude.. Prelude.hasStatus 400

-- | The cluster parameter group action can not be completed because another
-- task is in progress that involves the parameter group. Wait a few
-- moments and try the operation again.
_InvalidClusterParameterGroupStateFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidClusterParameterGroupStateFault =
  Prelude._MatchServiceError
    defaultService
    "InvalidClusterParameterGroupState"
    Prelude.. Prelude.hasStatus 400

-- | The cluster already has cross-region snapshot copy disabled.
_SnapshotCopyAlreadyDisabledFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_SnapshotCopyAlreadyDisabledFault =
  Prelude._MatchServiceError
    defaultService
    "SnapshotCopyAlreadyDisabledFault"
    Prelude.. Prelude.hasStatus 400

-- | The specified snapshot schedule already exists.
_SnapshotScheduleAlreadyExistsFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_SnapshotScheduleAlreadyExistsFault =
  Prelude._MatchServiceError
    defaultService
    "SnapshotScheduleAlreadyExists"
    Prelude.. Prelude.hasStatus 400

-- | Specified offering does not exist.
_ReservedNodeOfferingNotFoundFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ReservedNodeOfferingNotFoundFault =
  Prelude._MatchServiceError
    defaultService
    "ReservedNodeOfferingNotFound"
    Prelude.. Prelude.hasStatus 404

-- | The specified reserved compute node not found.
_ReservedNodeNotFoundFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ReservedNodeNotFoundFault =
  Prelude._MatchServiceError
    defaultService
    "ReservedNodeNotFound"
    Prelude.. Prelude.hasStatus 404

-- | The quota for HSM client certificates has been reached. For information
-- about increasing your quota, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_HsmClientCertificateQuotaExceededFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_HsmClientCertificateQuotaExceededFault =
  Prelude._MatchServiceError
    defaultService
    "HsmClientCertificateQuotaExceededFault"
    Prelude.. Prelude.hasStatus 400

-- | The parameter group name does not refer to an existing parameter group.
_ClusterParameterGroupNotFoundFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ClusterParameterGroupNotFoundFault =
  Prelude._MatchServiceError
    defaultService
    "ClusterParameterGroupNotFound"
    Prelude.. Prelude.hasStatus 404

-- | The value specified for the event category was not one of the allowed
-- values, or it specified a category that does not apply to the specified
-- source type. The allowed values are Configuration, Management,
-- Monitoring, and Security.
_SubscriptionCategoryNotFoundFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_SubscriptionCategoryNotFoundFault =
  Prelude._MatchServiceError
    defaultService
    "SubscriptionCategoryNotFound"
    Prelude.. Prelude.hasStatus 404

-- | The specified CIDR IP range or EC2 security group is not authorized for
-- the specified cluster security group.
_AuthorizationNotFoundFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_AuthorizationNotFoundFault =
  Prelude._MatchServiceError
    defaultService
    "AuthorizationNotFound"
    Prelude.. Prelude.hasStatus 404

-- | The number of tables in the cluster exceeds the limit for the requested
-- new cluster node type.
_TableLimitExceededFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TableLimitExceededFault =
  Prelude._MatchServiceError
    defaultService
    "TableLimitExceeded"
    Prelude.. Prelude.hasStatus 400

-- | The specified cluster snapshot is not in the @available@ state, or other
-- accounts are authorized to access the snapshot.
_InvalidClusterSnapshotStateFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidClusterSnapshotStateFault =
  Prelude._MatchServiceError
    defaultService
    "InvalidClusterSnapshotState"
    Prelude.. Prelude.hasStatus 400

-- | The retention period specified is either in the past or is not a valid
-- value.
--
-- The value must be either -1 or an integer between 1 and 3,653.
_InvalidRetentionPeriodFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidRetentionPeriodFault =
  Prelude._MatchServiceError
    defaultService
    "InvalidRetentionPeriodFault"
    Prelude.. Prelude.hasStatus 400

-- | A /ClusterSubnetGroupName/ is already used by an existing cluster subnet
-- group.
_ClusterSubnetGroupAlreadyExistsFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ClusterSubnetGroupAlreadyExistsFault =
  Prelude._MatchServiceError
    defaultService
    "ClusterSubnetGroupAlreadyExists"
    Prelude.. Prelude.hasStatus 400

-- | The requested subnet is not valid, or not all of the subnets are in the
-- same VPC.
_InvalidSubnet :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidSubnet =
  Prelude._MatchServiceError
    defaultService
    "InvalidSubnet"
    Prelude.. Prelude.hasStatus 400

-- | An Amazon Redshift event notification subscription with the specified
-- name does not exist.
_SubscriptionNotFoundFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_SubscriptionNotFoundFault =
  Prelude._MatchServiceError
    defaultService
    "SubscriptionNotFound"
    Prelude.. Prelude.hasStatus 404

-- | The value specified as a snapshot identifier is already used by an
-- existing snapshot.
_ClusterSnapshotAlreadyExistsFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ClusterSnapshotAlreadyExistsFault =
  Prelude._MatchServiceError
    defaultService
    "ClusterSnapshotAlreadyExists"
    Prelude.. Prelude.hasStatus 400

-- | The specified HSM configuration is not in the @available@ state, or it
-- is still in use by one or more Amazon Redshift clusters.
_InvalidHsmConfigurationStateFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidHsmConfigurationStateFault =
  Prelude._MatchServiceError
    defaultService
    "InvalidHsmConfigurationStateFault"
    Prelude.. Prelude.hasStatus 400

-- | The request cannot be completed because a dependent service is
-- throttling requests made by Amazon Redshift on your behalf. Wait and
-- retry the request.
_DependentServiceRequestThrottlingFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DependentServiceRequestThrottlingFault =
  Prelude._MatchServiceError
    defaultService
    "DependentServiceRequestThrottlingFault"
    Prelude.. Prelude.hasStatus 400
