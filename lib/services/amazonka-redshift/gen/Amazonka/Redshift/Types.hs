{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Redshift.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessToClusterDeniedFault,
    _AccessToSnapshotDeniedFault,
    _AuthenticationProfileAlreadyExistsFault,
    _AuthenticationProfileNotFoundFault,
    _AuthenticationProfileQuotaExceededFault,
    _AuthorizationAlreadyExistsFault,
    _AuthorizationNotFoundFault,
    _AuthorizationQuotaExceededFault,
    _BatchDeleteRequestSizeExceededFault,
    _BatchModifyClusterSnapshotsLimitExceededFault,
    _BucketNotFoundFault,
    _ClusterAlreadyExistsFault,
    _ClusterNotFoundFault,
    _ClusterOnLatestRevisionFault,
    _ClusterParameterGroupAlreadyExistsFault,
    _ClusterParameterGroupNotFoundFault,
    _ClusterParameterGroupQuotaExceededFault,
    _ClusterQuotaExceededFault,
    _ClusterSecurityGroupAlreadyExistsFault,
    _ClusterSecurityGroupNotFoundFault,
    _ClusterSecurityGroupQuotaExceededFault,
    _ClusterSnapshotAlreadyExistsFault,
    _ClusterSnapshotNotFoundFault,
    _ClusterSnapshotQuotaExceededFault,
    _ClusterSubnetGroupAlreadyExistsFault,
    _ClusterSubnetGroupNotFoundFault,
    _ClusterSubnetGroupQuotaExceededFault,
    _ClusterSubnetQuotaExceededFault,
    _CopyToRegionDisabledFault,
    _DependentServiceRequestThrottlingFault,
    _DependentServiceUnavailableFault,
    _EndpointAlreadyExistsFault,
    _EndpointAuthorizationAlreadyExistsFault,
    _EndpointAuthorizationNotFoundFault,
    _EndpointAuthorizationsPerClusterLimitExceededFault,
    _EndpointNotFoundFault,
    _EndpointsPerAuthorizationLimitExceededFault,
    _EndpointsPerClusterLimitExceededFault,
    _EventSubscriptionQuotaExceededFault,
    _HsmClientCertificateAlreadyExistsFault,
    _HsmClientCertificateNotFoundFault,
    _HsmClientCertificateQuotaExceededFault,
    _HsmConfigurationAlreadyExistsFault,
    _HsmConfigurationNotFoundFault,
    _HsmConfigurationQuotaExceededFault,
    _InProgressTableRestoreQuotaExceededFault,
    _IncompatibleOrderableOptions,
    _InsufficientClusterCapacityFault,
    _InsufficientS3BucketPolicyFault,
    _InvalidAuthenticationProfileRequestFault,
    _InvalidAuthorizationStateFault,
    _InvalidClusterParameterGroupStateFault,
    _InvalidClusterSecurityGroupStateFault,
    _InvalidClusterSnapshotScheduleStateFault,
    _InvalidClusterSnapshotStateFault,
    _InvalidClusterStateFault,
    _InvalidClusterSubnetGroupStateFault,
    _InvalidClusterSubnetStateFault,
    _InvalidClusterTrackFault,
    _InvalidDataShareFault,
    _InvalidElasticIpFault,
    _InvalidEndpointStateFault,
    _InvalidHsmClientCertificateStateFault,
    _InvalidHsmConfigurationStateFault,
    _InvalidNamespaceFault,
    _InvalidReservedNodeStateFault,
    _InvalidRestoreFault,
    _InvalidRetentionPeriodFault,
    _InvalidS3BucketNameFault,
    _InvalidS3KeyPrefixFault,
    _InvalidScheduleFault,
    _InvalidScheduledActionFault,
    _InvalidSnapshotCopyGrantStateFault,
    _InvalidSubnet,
    _InvalidSubscriptionStateFault,
    _InvalidTableRestoreArgumentFault,
    _InvalidTagFault,
    _InvalidUsageLimitFault,
    _InvalidVPCNetworkStateFault,
    _LimitExceededFault,
    _NumberOfNodesPerClusterLimitExceededFault,
    _NumberOfNodesQuotaExceededFault,
    _PartnerNotFoundFault,
    _ReservedNodeAlreadyExistsFault,
    _ReservedNodeAlreadyMigratedFault,
    _ReservedNodeExchangeNotFoundFault,
    _ReservedNodeNotFoundFault,
    _ReservedNodeOfferingNotFoundFault,
    _ReservedNodeQuotaExceededFault,
    _ResizeNotFoundFault,
    _ResourceNotFoundFault,
    _SNSInvalidTopicFault,
    _SNSNoAuthorizationFault,
    _SNSTopicArnNotFoundFault,
    _ScheduleDefinitionTypeUnsupportedFault,
    _ScheduledActionAlreadyExistsFault,
    _ScheduledActionNotFoundFault,
    _ScheduledActionQuotaExceededFault,
    _ScheduledActionTypeUnsupportedFault,
    _SnapshotCopyAlreadyDisabledFault,
    _SnapshotCopyAlreadyEnabledFault,
    _SnapshotCopyDisabledFault,
    _SnapshotCopyGrantAlreadyExistsFault,
    _SnapshotCopyGrantNotFoundFault,
    _SnapshotCopyGrantQuotaExceededFault,
    _SnapshotScheduleAlreadyExistsFault,
    _SnapshotScheduleNotFoundFault,
    _SnapshotScheduleQuotaExceededFault,
    _SnapshotScheduleUpdateInProgressFault,
    _SourceNotFoundFault,
    _SubnetAlreadyInUse,
    _SubscriptionAlreadyExistFault,
    _SubscriptionCategoryNotFoundFault,
    _SubscriptionEventIdNotFoundFault,
    _SubscriptionNotFoundFault,
    _SubscriptionSeverityNotFoundFault,
    _TableLimitExceededFault,
    _TableRestoreNotFoundFault,
    _TagLimitExceededFault,
    _UnauthorizedOperation,
    _UnauthorizedPartnerIntegrationFault,
    _UnknownSnapshotCopyRegionFault,
    _UnsupportedOperationFault,
    _UnsupportedOptionFault,
    _UsageLimitAlreadyExistsFault,
    _UsageLimitNotFoundFault,

    -- * Re-exported Types
    module Amazonka.Redshift.Internal,

    -- * ActionType
    ActionType (..),

    -- * AquaConfigurationStatus
    AquaConfigurationStatus (..),

    -- * AquaStatus
    AquaStatus (..),

    -- * AuthorizationStatus
    AuthorizationStatus (..),

    -- * DataShareStatus
    DataShareStatus (..),

    -- * DataShareStatusForConsumer
    DataShareStatusForConsumer (..),

    -- * DataShareStatusForProducer
    DataShareStatusForProducer (..),

    -- * LogDestinationType
    LogDestinationType (..),

    -- * Mode
    Mode (..),

    -- * NodeConfigurationOptionsFilterName
    NodeConfigurationOptionsFilterName (..),

    -- * OperatorType
    OperatorType (..),

    -- * ParameterApplyType
    ParameterApplyType (..),

    -- * PartnerIntegrationStatus
    PartnerIntegrationStatus (..),

    -- * ReservedNodeExchangeActionType
    ReservedNodeExchangeActionType (..),

    -- * ReservedNodeExchangeStatusType
    ReservedNodeExchangeStatusType (..),

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

    -- * AquaConfiguration
    AquaConfiguration (..),
    newAquaConfiguration,
    aquaConfiguration_aquaConfigurationStatus,
    aquaConfiguration_aquaStatus,

    -- * AttributeValueTarget
    AttributeValueTarget (..),
    newAttributeValueTarget,
    attributeValueTarget_attributeValue,

    -- * AuthenticationProfile
    AuthenticationProfile (..),
    newAuthenticationProfile,
    authenticationProfile_authenticationProfileContent,
    authenticationProfile_authenticationProfileName,

    -- * AvailabilityZone
    AvailabilityZone (..),
    newAvailabilityZone,
    availabilityZone_name,
    availabilityZone_supportedPlatforms,

    -- * Cluster
    Cluster (..),
    newCluster,
    cluster_allowVersionUpgrade,
    cluster_aquaConfiguration,
    cluster_automatedSnapshotRetentionPeriod,
    cluster_availabilityZone,
    cluster_availabilityZoneRelocationStatus,
    cluster_clusterAvailabilityStatus,
    cluster_clusterCreateTime,
    cluster_clusterIdentifier,
    cluster_clusterNamespaceArn,
    cluster_clusterNodes,
    cluster_clusterParameterGroups,
    cluster_clusterPublicKey,
    cluster_clusterRevisionNumber,
    cluster_clusterSecurityGroups,
    cluster_clusterSnapshotCopyStatus,
    cluster_clusterStatus,
    cluster_clusterSubnetGroupName,
    cluster_clusterVersion,
    cluster_dbName,
    cluster_dataTransferProgress,
    cluster_defaultIamRoleArn,
    cluster_deferredMaintenanceWindows,
    cluster_elasticIpStatus,
    cluster_elasticResizeNumberOfNodeOptions,
    cluster_encrypted,
    cluster_endpoint,
    cluster_enhancedVpcRouting,
    cluster_expectedNextSnapshotScheduleTime,
    cluster_expectedNextSnapshotScheduleTimeStatus,
    cluster_hsmStatus,
    cluster_iamRoles,
    cluster_kmsKeyId,
    cluster_maintenanceTrackName,
    cluster_manualSnapshotRetentionPeriod,
    cluster_masterUsername,
    cluster_modifyStatus,
    cluster_nextMaintenanceWindowStartTime,
    cluster_nodeType,
    cluster_numberOfNodes,
    cluster_pendingActions,
    cluster_pendingModifiedValues,
    cluster_preferredMaintenanceWindow,
    cluster_publiclyAccessible,
    cluster_reservedNodeExchangeStatus,
    cluster_resizeInfo,
    cluster_restoreStatus,
    cluster_snapshotScheduleIdentifier,
    cluster_snapshotScheduleState,
    cluster_tags,
    cluster_totalStorageCapacityInMegaBytes,
    cluster_vpcId,
    cluster_vpcSecurityGroups,

    -- * ClusterAssociatedToSchedule
    ClusterAssociatedToSchedule (..),
    newClusterAssociatedToSchedule,
    clusterAssociatedToSchedule_clusterIdentifier,
    clusterAssociatedToSchedule_scheduleAssociationState,

    -- * ClusterDbRevision
    ClusterDbRevision (..),
    newClusterDbRevision,
    clusterDbRevision_clusterIdentifier,
    clusterDbRevision_currentDatabaseRevision,
    clusterDbRevision_databaseRevisionReleaseDate,
    clusterDbRevision_revisionTargets,

    -- * ClusterIamRole
    ClusterIamRole (..),
    newClusterIamRole,
    clusterIamRole_applyStatus,
    clusterIamRole_iamRoleArn,

    -- * ClusterNode
    ClusterNode (..),
    newClusterNode,
    clusterNode_nodeRole,
    clusterNode_privateIPAddress,
    clusterNode_publicIPAddress,

    -- * ClusterParameterGroup
    ClusterParameterGroup (..),
    newClusterParameterGroup,
    clusterParameterGroup_description,
    clusterParameterGroup_parameterGroupFamily,
    clusterParameterGroup_parameterGroupName,
    clusterParameterGroup_tags,

    -- * ClusterParameterGroupNameMessage
    ClusterParameterGroupNameMessage (..),
    newClusterParameterGroupNameMessage,
    clusterParameterGroupNameMessage_parameterGroupName,
    clusterParameterGroupNameMessage_parameterGroupStatus,

    -- * ClusterParameterGroupStatus
    ClusterParameterGroupStatus (..),
    newClusterParameterGroupStatus,
    clusterParameterGroupStatus_clusterParameterStatusList,
    clusterParameterGroupStatus_parameterApplyStatus,
    clusterParameterGroupStatus_parameterGroupName,

    -- * ClusterParameterStatus
    ClusterParameterStatus (..),
    newClusterParameterStatus,
    clusterParameterStatus_parameterApplyErrorDescription,
    clusterParameterStatus_parameterApplyStatus,
    clusterParameterStatus_parameterName,

    -- * ClusterSecurityGroup
    ClusterSecurityGroup (..),
    newClusterSecurityGroup,
    clusterSecurityGroup_clusterSecurityGroupName,
    clusterSecurityGroup_description,
    clusterSecurityGroup_eC2SecurityGroups,
    clusterSecurityGroup_iPRanges,
    clusterSecurityGroup_tags,

    -- * ClusterSecurityGroupMembership
    ClusterSecurityGroupMembership (..),
    newClusterSecurityGroupMembership,
    clusterSecurityGroupMembership_clusterSecurityGroupName,
    clusterSecurityGroupMembership_status,

    -- * ClusterSnapshotCopyStatus
    ClusterSnapshotCopyStatus (..),
    newClusterSnapshotCopyStatus,
    clusterSnapshotCopyStatus_destinationRegion,
    clusterSnapshotCopyStatus_manualSnapshotRetentionPeriod,
    clusterSnapshotCopyStatus_retentionPeriod,
    clusterSnapshotCopyStatus_snapshotCopyGrantName,

    -- * ClusterSubnetGroup
    ClusterSubnetGroup (..),
    newClusterSubnetGroup,
    clusterSubnetGroup_clusterSubnetGroupName,
    clusterSubnetGroup_description,
    clusterSubnetGroup_subnetGroupStatus,
    clusterSubnetGroup_subnets,
    clusterSubnetGroup_tags,
    clusterSubnetGroup_vpcId,

    -- * ClusterVersion
    ClusterVersion (..),
    newClusterVersion,
    clusterVersion_clusterParameterGroupFamily,
    clusterVersion_clusterVersion,
    clusterVersion_description,

    -- * DataShare
    DataShare (..),
    newDataShare,
    dataShare_allowPubliclyAccessibleConsumers,
    dataShare_dataShareArn,
    dataShare_dataShareAssociations,
    dataShare_managedBy,
    dataShare_producerArn,

    -- * DataShareAssociation
    DataShareAssociation (..),
    newDataShareAssociation,
    dataShareAssociation_consumerIdentifier,
    dataShareAssociation_consumerRegion,
    dataShareAssociation_createdDate,
    dataShareAssociation_status,
    dataShareAssociation_statusChangeDate,

    -- * DataTransferProgress
    DataTransferProgress (..),
    newDataTransferProgress,
    dataTransferProgress_currentRateInMegaBytesPerSecond,
    dataTransferProgress_dataTransferredInMegaBytes,
    dataTransferProgress_elapsedTimeInSeconds,
    dataTransferProgress_estimatedTimeToCompletionInSeconds,
    dataTransferProgress_status,
    dataTransferProgress_totalDataInMegaBytes,

    -- * DefaultClusterParameters
    DefaultClusterParameters (..),
    newDefaultClusterParameters,
    defaultClusterParameters_marker,
    defaultClusterParameters_parameterGroupFamily,
    defaultClusterParameters_parameters,

    -- * DeferredMaintenanceWindow
    DeferredMaintenanceWindow (..),
    newDeferredMaintenanceWindow,
    deferredMaintenanceWindow_deferMaintenanceEndTime,
    deferredMaintenanceWindow_deferMaintenanceIdentifier,
    deferredMaintenanceWindow_deferMaintenanceStartTime,

    -- * DeleteClusterSnapshotMessage
    DeleteClusterSnapshotMessage (..),
    newDeleteClusterSnapshotMessage,
    deleteClusterSnapshotMessage_snapshotClusterIdentifier,
    deleteClusterSnapshotMessage_snapshotIdentifier,

    -- * EC2SecurityGroup
    EC2SecurityGroup (..),
    newEC2SecurityGroup,
    eC2SecurityGroup_eC2SecurityGroupName,
    eC2SecurityGroup_eC2SecurityGroupOwnerId,
    eC2SecurityGroup_status,
    eC2SecurityGroup_tags,

    -- * ElasticIpStatus
    ElasticIpStatus (..),
    newElasticIpStatus,
    elasticIpStatus_elasticIp,
    elasticIpStatus_status,

    -- * Endpoint
    Endpoint (..),
    newEndpoint,
    endpoint_address,
    endpoint_port,
    endpoint_vpcEndpoints,

    -- * EndpointAccess
    EndpointAccess (..),
    newEndpointAccess,
    endpointAccess_address,
    endpointAccess_clusterIdentifier,
    endpointAccess_endpointCreateTime,
    endpointAccess_endpointName,
    endpointAccess_endpointStatus,
    endpointAccess_port,
    endpointAccess_resourceOwner,
    endpointAccess_subnetGroupName,
    endpointAccess_vpcEndpoint,
    endpointAccess_vpcSecurityGroups,

    -- * EndpointAuthorization
    EndpointAuthorization (..),
    newEndpointAuthorization,
    endpointAuthorization_allowedAllVPCs,
    endpointAuthorization_allowedVPCs,
    endpointAuthorization_authorizeTime,
    endpointAuthorization_clusterIdentifier,
    endpointAuthorization_clusterStatus,
    endpointAuthorization_endpointCount,
    endpointAuthorization_grantee,
    endpointAuthorization_grantor,
    endpointAuthorization_status,

    -- * Event
    Event (..),
    newEvent,
    event_date,
    event_eventCategories,
    event_eventId,
    event_message,
    event_severity,
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
    eventInfoMap_eventCategories,
    eventInfoMap_eventDescription,
    eventInfoMap_eventId,
    eventInfoMap_severity,

    -- * EventSubscription
    EventSubscription (..),
    newEventSubscription,
    eventSubscription_custSubscriptionId,
    eventSubscription_customerAwsId,
    eventSubscription_enabled,
    eventSubscription_eventCategoriesList,
    eventSubscription_severity,
    eventSubscription_snsTopicArn,
    eventSubscription_sourceIdsList,
    eventSubscription_sourceType,
    eventSubscription_status,
    eventSubscription_subscriptionCreationTime,
    eventSubscription_tags,

    -- * HsmClientCertificate
    HsmClientCertificate (..),
    newHsmClientCertificate,
    hsmClientCertificate_hsmClientCertificateIdentifier,
    hsmClientCertificate_hsmClientCertificatePublicKey,
    hsmClientCertificate_tags,

    -- * HsmConfiguration
    HsmConfiguration (..),
    newHsmConfiguration,
    hsmConfiguration_description,
    hsmConfiguration_hsmConfigurationIdentifier,
    hsmConfiguration_hsmIpAddress,
    hsmConfiguration_hsmPartitionName,
    hsmConfiguration_tags,

    -- * HsmStatus
    HsmStatus (..),
    newHsmStatus,
    hsmStatus_hsmClientCertificateIdentifier,
    hsmStatus_hsmConfigurationIdentifier,
    hsmStatus_status,

    -- * IPRange
    IPRange (..),
    newIPRange,
    iPRange_cidrip,
    iPRange_status,
    iPRange_tags,

    -- * LoggingStatus
    LoggingStatus (..),
    newLoggingStatus,
    loggingStatus_bucketName,
    loggingStatus_lastFailureMessage,
    loggingStatus_lastFailureTime,
    loggingStatus_lastSuccessfulDeliveryTime,
    loggingStatus_logDestinationType,
    loggingStatus_logExports,
    loggingStatus_loggingEnabled,
    loggingStatus_s3KeyPrefix,

    -- * MaintenanceTrack
    MaintenanceTrack (..),
    newMaintenanceTrack,
    maintenanceTrack_databaseVersion,
    maintenanceTrack_maintenanceTrackName,
    maintenanceTrack_updateTargets,

    -- * NetworkInterface
    NetworkInterface (..),
    newNetworkInterface,
    networkInterface_availabilityZone,
    networkInterface_networkInterfaceId,
    networkInterface_privateIpAddress,
    networkInterface_subnetId,

    -- * NodeConfigurationOption
    NodeConfigurationOption (..),
    newNodeConfigurationOption,
    nodeConfigurationOption_estimatedDiskUtilizationPercent,
    nodeConfigurationOption_mode,
    nodeConfigurationOption_nodeType,
    nodeConfigurationOption_numberOfNodes,

    -- * NodeConfigurationOptionsFilter
    NodeConfigurationOptionsFilter (..),
    newNodeConfigurationOptionsFilter,
    nodeConfigurationOptionsFilter_name,
    nodeConfigurationOptionsFilter_operator,
    nodeConfigurationOptionsFilter_values,

    -- * OrderableClusterOption
    OrderableClusterOption (..),
    newOrderableClusterOption,
    orderableClusterOption_availabilityZones,
    orderableClusterOption_clusterType,
    orderableClusterOption_clusterVersion,
    orderableClusterOption_nodeType,

    -- * Parameter
    Parameter (..),
    newParameter,
    parameter_allowedValues,
    parameter_applyType,
    parameter_dataType,
    parameter_description,
    parameter_isModifiable,
    parameter_minimumEngineVersion,
    parameter_parameterName,
    parameter_parameterValue,
    parameter_source,

    -- * PartnerIntegrationInfo
    PartnerIntegrationInfo (..),
    newPartnerIntegrationInfo,
    partnerIntegrationInfo_createdAt,
    partnerIntegrationInfo_databaseName,
    partnerIntegrationInfo_partnerName,
    partnerIntegrationInfo_status,
    partnerIntegrationInfo_statusMessage,
    partnerIntegrationInfo_updatedAt,

    -- * PartnerIntegrationInputMessage
    PartnerIntegrationInputMessage (..),
    newPartnerIntegrationInputMessage,
    partnerIntegrationInputMessage_accountId,
    partnerIntegrationInputMessage_clusterIdentifier,
    partnerIntegrationInputMessage_databaseName,
    partnerIntegrationInputMessage_partnerName,

    -- * PartnerIntegrationOutputMessage
    PartnerIntegrationOutputMessage (..),
    newPartnerIntegrationOutputMessage,
    partnerIntegrationOutputMessage_databaseName,
    partnerIntegrationOutputMessage_partnerName,

    -- * PauseClusterMessage
    PauseClusterMessage (..),
    newPauseClusterMessage,
    pauseClusterMessage_clusterIdentifier,

    -- * PendingModifiedValues
    PendingModifiedValues (..),
    newPendingModifiedValues,
    pendingModifiedValues_automatedSnapshotRetentionPeriod,
    pendingModifiedValues_clusterIdentifier,
    pendingModifiedValues_clusterType,
    pendingModifiedValues_clusterVersion,
    pendingModifiedValues_encryptionType,
    pendingModifiedValues_enhancedVpcRouting,
    pendingModifiedValues_maintenanceTrackName,
    pendingModifiedValues_masterUserPassword,
    pendingModifiedValues_nodeType,
    pendingModifiedValues_numberOfNodes,
    pendingModifiedValues_publiclyAccessible,

    -- * RecurringCharge
    RecurringCharge (..),
    newRecurringCharge,
    recurringCharge_recurringChargeAmount,
    recurringCharge_recurringChargeFrequency,

    -- * ReservedNode
    ReservedNode (..),
    newReservedNode,
    reservedNode_currencyCode,
    reservedNode_duration,
    reservedNode_fixedPrice,
    reservedNode_nodeCount,
    reservedNode_nodeType,
    reservedNode_offeringType,
    reservedNode_recurringCharges,
    reservedNode_reservedNodeId,
    reservedNode_reservedNodeOfferingId,
    reservedNode_reservedNodeOfferingType,
    reservedNode_startTime,
    reservedNode_state,
    reservedNode_usagePrice,

    -- * ReservedNodeConfigurationOption
    ReservedNodeConfigurationOption (..),
    newReservedNodeConfigurationOption,
    reservedNodeConfigurationOption_sourceReservedNode,
    reservedNodeConfigurationOption_targetReservedNodeCount,
    reservedNodeConfigurationOption_targetReservedNodeOffering,

    -- * ReservedNodeExchangeStatus
    ReservedNodeExchangeStatus (..),
    newReservedNodeExchangeStatus,
    reservedNodeExchangeStatus_requestTime,
    reservedNodeExchangeStatus_reservedNodeExchangeRequestId,
    reservedNodeExchangeStatus_sourceReservedNodeCount,
    reservedNodeExchangeStatus_sourceReservedNodeId,
    reservedNodeExchangeStatus_sourceReservedNodeType,
    reservedNodeExchangeStatus_status,
    reservedNodeExchangeStatus_targetReservedNodeCount,
    reservedNodeExchangeStatus_targetReservedNodeOfferingId,
    reservedNodeExchangeStatus_targetReservedNodeType,

    -- * ReservedNodeOffering
    ReservedNodeOffering (..),
    newReservedNodeOffering,
    reservedNodeOffering_currencyCode,
    reservedNodeOffering_duration,
    reservedNodeOffering_fixedPrice,
    reservedNodeOffering_nodeType,
    reservedNodeOffering_offeringType,
    reservedNodeOffering_recurringCharges,
    reservedNodeOffering_reservedNodeOfferingId,
    reservedNodeOffering_reservedNodeOfferingType,
    reservedNodeOffering_usagePrice,

    -- * ResizeClusterMessage
    ResizeClusterMessage (..),
    newResizeClusterMessage,
    resizeClusterMessage_classic,
    resizeClusterMessage_clusterType,
    resizeClusterMessage_nodeType,
    resizeClusterMessage_numberOfNodes,
    resizeClusterMessage_reservedNodeId,
    resizeClusterMessage_targetReservedNodeOfferingId,
    resizeClusterMessage_clusterIdentifier,

    -- * ResizeInfo
    ResizeInfo (..),
    newResizeInfo,
    resizeInfo_allowCancelResize,
    resizeInfo_resizeType,

    -- * ResizeProgressMessage
    ResizeProgressMessage (..),
    newResizeProgressMessage,
    resizeProgressMessage_avgResizeRateInMegaBytesPerSecond,
    resizeProgressMessage_dataTransferProgressPercent,
    resizeProgressMessage_elapsedTimeInSeconds,
    resizeProgressMessage_estimatedTimeToCompletionInSeconds,
    resizeProgressMessage_importTablesCompleted,
    resizeProgressMessage_importTablesInProgress,
    resizeProgressMessage_importTablesNotStarted,
    resizeProgressMessage_message,
    resizeProgressMessage_progressInMegaBytes,
    resizeProgressMessage_resizeType,
    resizeProgressMessage_status,
    resizeProgressMessage_targetClusterType,
    resizeProgressMessage_targetEncryptionType,
    resizeProgressMessage_targetNodeType,
    resizeProgressMessage_targetNumberOfNodes,
    resizeProgressMessage_totalResizeDataInMegaBytes,

    -- * RestoreStatus
    RestoreStatus (..),
    newRestoreStatus,
    restoreStatus_currentRestoreRateInMegaBytesPerSecond,
    restoreStatus_elapsedTimeInSeconds,
    restoreStatus_estimatedTimeToCompletionInSeconds,
    restoreStatus_progressInMegaBytes,
    restoreStatus_snapshotSizeInMegaBytes,
    restoreStatus_status,

    -- * ResumeClusterMessage
    ResumeClusterMessage (..),
    newResumeClusterMessage,
    resumeClusterMessage_clusterIdentifier,

    -- * RevisionTarget
    RevisionTarget (..),
    newRevisionTarget,
    revisionTarget_databaseRevision,
    revisionTarget_databaseRevisionReleaseDate,
    revisionTarget_description,

    -- * ScheduledAction
    ScheduledAction (..),
    newScheduledAction,
    scheduledAction_endTime,
    scheduledAction_iamRole,
    scheduledAction_nextInvocations,
    scheduledAction_schedule,
    scheduledAction_scheduledActionDescription,
    scheduledAction_scheduledActionName,
    scheduledAction_startTime,
    scheduledAction_state,
    scheduledAction_targetAction,

    -- * ScheduledActionFilter
    ScheduledActionFilter (..),
    newScheduledActionFilter,
    scheduledActionFilter_name,
    scheduledActionFilter_values,

    -- * ScheduledActionType
    ScheduledActionType (..),
    newScheduledActionType,
    scheduledActionType_pauseCluster,
    scheduledActionType_resizeCluster,
    scheduledActionType_resumeCluster,

    -- * Snapshot
    Snapshot (..),
    newSnapshot,
    snapshot_accountsWithRestoreAccess,
    snapshot_actualIncrementalBackupSizeInMegaBytes,
    snapshot_availabilityZone,
    snapshot_backupProgressInMegaBytes,
    snapshot_clusterCreateTime,
    snapshot_clusterIdentifier,
    snapshot_clusterVersion,
    snapshot_currentBackupRateInMegaBytesPerSecond,
    snapshot_dbName,
    snapshot_elapsedTimeInSeconds,
    snapshot_encrypted,
    snapshot_encryptedWithHSM,
    snapshot_engineFullVersion,
    snapshot_enhancedVpcRouting,
    snapshot_estimatedSecondsToCompletion,
    snapshot_kmsKeyId,
    snapshot_maintenanceTrackName,
    snapshot_manualSnapshotRemainingDays,
    snapshot_manualSnapshotRetentionPeriod,
    snapshot_masterUsername,
    snapshot_nodeType,
    snapshot_numberOfNodes,
    snapshot_ownerAccount,
    snapshot_port,
    snapshot_restorableNodeTypes,
    snapshot_snapshotCreateTime,
    snapshot_snapshotIdentifier,
    snapshot_snapshotRetentionStartTime,
    snapshot_snapshotType,
    snapshot_sourceRegion,
    snapshot_status,
    snapshot_tags,
    snapshot_totalBackupSizeInMegaBytes,
    snapshot_vpcId,

    -- * SnapshotCopyGrant
    SnapshotCopyGrant (..),
    newSnapshotCopyGrant,
    snapshotCopyGrant_kmsKeyId,
    snapshotCopyGrant_snapshotCopyGrantName,
    snapshotCopyGrant_tags,

    -- * SnapshotErrorMessage
    SnapshotErrorMessage (..),
    newSnapshotErrorMessage,
    snapshotErrorMessage_failureCode,
    snapshotErrorMessage_failureReason,
    snapshotErrorMessage_snapshotClusterIdentifier,
    snapshotErrorMessage_snapshotIdentifier,

    -- * SnapshotSchedule
    SnapshotSchedule (..),
    newSnapshotSchedule,
    snapshotSchedule_associatedClusterCount,
    snapshotSchedule_associatedClusters,
    snapshotSchedule_nextInvocations,
    snapshotSchedule_scheduleDefinitions,
    snapshotSchedule_scheduleDescription,
    snapshotSchedule_scheduleIdentifier,
    snapshotSchedule_tags,

    -- * SnapshotSortingEntity
    SnapshotSortingEntity (..),
    newSnapshotSortingEntity,
    snapshotSortingEntity_sortOrder,
    snapshotSortingEntity_attribute,

    -- * Subnet
    Subnet (..),
    newSubnet,
    subnet_subnetAvailabilityZone,
    subnet_subnetIdentifier,
    subnet_subnetStatus,

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
    tableRestoreStatus_clusterIdentifier,
    tableRestoreStatus_message,
    tableRestoreStatus_newTableName,
    tableRestoreStatus_progressInMegaBytes,
    tableRestoreStatus_requestTime,
    tableRestoreStatus_snapshotIdentifier,
    tableRestoreStatus_sourceDatabaseName,
    tableRestoreStatus_sourceSchemaName,
    tableRestoreStatus_sourceTableName,
    tableRestoreStatus_status,
    tableRestoreStatus_tableRestoreRequestId,
    tableRestoreStatus_targetDatabaseName,
    tableRestoreStatus_targetSchemaName,
    tableRestoreStatus_totalDataInMegaBytes,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TaggedResource
    TaggedResource (..),
    newTaggedResource,
    taggedResource_resourceName,
    taggedResource_resourceType,
    taggedResource_tag,

    -- * UpdateTarget
    UpdateTarget (..),
    newUpdateTarget,
    updateTarget_databaseVersion,
    updateTarget_maintenanceTrackName,
    updateTarget_supportedOperations,

    -- * UsageLimit
    UsageLimit (..),
    newUsageLimit,
    usageLimit_amount,
    usageLimit_breachAction,
    usageLimit_clusterIdentifier,
    usageLimit_featureType,
    usageLimit_limitType,
    usageLimit_period,
    usageLimit_tags,
    usageLimit_usageLimitId,

    -- * VpcEndpoint
    VpcEndpoint (..),
    newVpcEndpoint,
    vpcEndpoint_networkInterfaces,
    vpcEndpoint_vpcEndpointId,
    vpcEndpoint_vpcId,

    -- * VpcSecurityGroupMembership
    VpcSecurityGroupMembership (..),
    newVpcSecurityGroupMembership,
    vpcSecurityGroupMembership_status,
    vpcSecurityGroupMembership_vpcSecurityGroupId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal
import Amazonka.Redshift.Types.AccountAttribute
import Amazonka.Redshift.Types.AccountWithRestoreAccess
import Amazonka.Redshift.Types.ActionType
import Amazonka.Redshift.Types.AquaConfiguration
import Amazonka.Redshift.Types.AquaConfigurationStatus
import Amazonka.Redshift.Types.AquaStatus
import Amazonka.Redshift.Types.AttributeValueTarget
import Amazonka.Redshift.Types.AuthenticationProfile
import Amazonka.Redshift.Types.AuthorizationStatus
import Amazonka.Redshift.Types.AvailabilityZone
import Amazonka.Redshift.Types.Cluster
import Amazonka.Redshift.Types.ClusterAssociatedToSchedule
import Amazonka.Redshift.Types.ClusterDbRevision
import Amazonka.Redshift.Types.ClusterIamRole
import Amazonka.Redshift.Types.ClusterNode
import Amazonka.Redshift.Types.ClusterParameterGroup
import Amazonka.Redshift.Types.ClusterParameterGroupNameMessage
import Amazonka.Redshift.Types.ClusterParameterGroupStatus
import Amazonka.Redshift.Types.ClusterParameterStatus
import Amazonka.Redshift.Types.ClusterSecurityGroup
import Amazonka.Redshift.Types.ClusterSecurityGroupMembership
import Amazonka.Redshift.Types.ClusterSnapshotCopyStatus
import Amazonka.Redshift.Types.ClusterSubnetGroup
import Amazonka.Redshift.Types.ClusterVersion
import Amazonka.Redshift.Types.DataShare
import Amazonka.Redshift.Types.DataShareAssociation
import Amazonka.Redshift.Types.DataShareStatus
import Amazonka.Redshift.Types.DataShareStatusForConsumer
import Amazonka.Redshift.Types.DataShareStatusForProducer
import Amazonka.Redshift.Types.DataTransferProgress
import Amazonka.Redshift.Types.DefaultClusterParameters
import Amazonka.Redshift.Types.DeferredMaintenanceWindow
import Amazonka.Redshift.Types.DeleteClusterSnapshotMessage
import Amazonka.Redshift.Types.EC2SecurityGroup
import Amazonka.Redshift.Types.ElasticIpStatus
import Amazonka.Redshift.Types.Endpoint
import Amazonka.Redshift.Types.EndpointAccess
import Amazonka.Redshift.Types.EndpointAuthorization
import Amazonka.Redshift.Types.Event
import Amazonka.Redshift.Types.EventCategoriesMap
import Amazonka.Redshift.Types.EventInfoMap
import Amazonka.Redshift.Types.EventSubscription
import Amazonka.Redshift.Types.HsmClientCertificate
import Amazonka.Redshift.Types.HsmConfiguration
import Amazonka.Redshift.Types.HsmStatus
import Amazonka.Redshift.Types.IPRange
import Amazonka.Redshift.Types.LogDestinationType
import Amazonka.Redshift.Types.LoggingStatus
import Amazonka.Redshift.Types.MaintenanceTrack
import Amazonka.Redshift.Types.Mode
import Amazonka.Redshift.Types.NetworkInterface
import Amazonka.Redshift.Types.NodeConfigurationOption
import Amazonka.Redshift.Types.NodeConfigurationOptionsFilter
import Amazonka.Redshift.Types.NodeConfigurationOptionsFilterName
import Amazonka.Redshift.Types.OperatorType
import Amazonka.Redshift.Types.OrderableClusterOption
import Amazonka.Redshift.Types.Parameter
import Amazonka.Redshift.Types.ParameterApplyType
import Amazonka.Redshift.Types.PartnerIntegrationInfo
import Amazonka.Redshift.Types.PartnerIntegrationInputMessage
import Amazonka.Redshift.Types.PartnerIntegrationOutputMessage
import Amazonka.Redshift.Types.PartnerIntegrationStatus
import Amazonka.Redshift.Types.PauseClusterMessage
import Amazonka.Redshift.Types.PendingModifiedValues
import Amazonka.Redshift.Types.RecurringCharge
import Amazonka.Redshift.Types.ReservedNode
import Amazonka.Redshift.Types.ReservedNodeConfigurationOption
import Amazonka.Redshift.Types.ReservedNodeExchangeActionType
import Amazonka.Redshift.Types.ReservedNodeExchangeStatus
import Amazonka.Redshift.Types.ReservedNodeExchangeStatusType
import Amazonka.Redshift.Types.ReservedNodeOffering
import Amazonka.Redshift.Types.ReservedNodeOfferingType
import Amazonka.Redshift.Types.ResizeClusterMessage
import Amazonka.Redshift.Types.ResizeInfo
import Amazonka.Redshift.Types.ResizeProgressMessage
import Amazonka.Redshift.Types.RestoreStatus
import Amazonka.Redshift.Types.ResumeClusterMessage
import Amazonka.Redshift.Types.RevisionTarget
import Amazonka.Redshift.Types.ScheduleState
import Amazonka.Redshift.Types.ScheduledAction
import Amazonka.Redshift.Types.ScheduledActionFilter
import Amazonka.Redshift.Types.ScheduledActionFilterName
import Amazonka.Redshift.Types.ScheduledActionState
import Amazonka.Redshift.Types.ScheduledActionType
import Amazonka.Redshift.Types.ScheduledActionTypeValues
import Amazonka.Redshift.Types.Snapshot
import Amazonka.Redshift.Types.SnapshotAttributeToSortBy
import Amazonka.Redshift.Types.SnapshotCopyGrant
import Amazonka.Redshift.Types.SnapshotErrorMessage
import Amazonka.Redshift.Types.SnapshotSchedule
import Amazonka.Redshift.Types.SnapshotSortingEntity
import Amazonka.Redshift.Types.SortByOrder
import Amazonka.Redshift.Types.SourceType
import Amazonka.Redshift.Types.Subnet
import Amazonka.Redshift.Types.SupportedOperation
import Amazonka.Redshift.Types.SupportedPlatform
import Amazonka.Redshift.Types.TableRestoreStatus
import Amazonka.Redshift.Types.TableRestoreStatusType
import Amazonka.Redshift.Types.Tag
import Amazonka.Redshift.Types.TaggedResource
import Amazonka.Redshift.Types.UpdateTarget
import Amazonka.Redshift.Types.UsageLimit
import Amazonka.Redshift.Types.UsageLimitBreachAction
import Amazonka.Redshift.Types.UsageLimitFeatureType
import Amazonka.Redshift.Types.UsageLimitLimitType
import Amazonka.Redshift.Types.UsageLimitPeriod
import Amazonka.Redshift.Types.VpcEndpoint
import Amazonka.Redshift.Types.VpcSecurityGroupMembership
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2012-12-01@ of the Amazon Redshift SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Redshift",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "redshift",
      Core.signingName = "redshift",
      Core.version = "2012-12-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseXMLError "Redshift",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | You are not authorized to access the cluster.
_AccessToClusterDeniedFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessToClusterDeniedFault =
  Core._MatchServiceError
    defaultService
    "AccessToClusterDenied"
    Prelude.. Core.hasStatus 400

-- | The owner of the specified snapshot has not authorized your account to
-- access the snapshot.
_AccessToSnapshotDeniedFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessToSnapshotDeniedFault =
  Core._MatchServiceError
    defaultService
    "AccessToSnapshotDenied"
    Prelude.. Core.hasStatus 400

-- | The authentication profile already exists.
_AuthenticationProfileAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AuthenticationProfileAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "AuthenticationProfileAlreadyExistsFault"
    Prelude.. Core.hasStatus 400

-- | The authentication profile can\'t be found.
_AuthenticationProfileNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AuthenticationProfileNotFoundFault =
  Core._MatchServiceError
    defaultService
    "AuthenticationProfileNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | The size or number of authentication profiles has exceeded the quota.
-- The maximum length of the JSON string and maximum number of
-- authentication profiles is determined by a quota for your account.
_AuthenticationProfileQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AuthenticationProfileQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "AuthenticationProfileQuotaExceededFault"
    Prelude.. Core.hasStatus 400

-- | The specified CIDR block or EC2 security group is already authorized for
-- the specified cluster security group.
_AuthorizationAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AuthorizationAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "AuthorizationAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The specified CIDR IP range or EC2 security group is not authorized for
-- the specified cluster security group.
_AuthorizationNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AuthorizationNotFoundFault =
  Core._MatchServiceError
    defaultService
    "AuthorizationNotFound"
    Prelude.. Core.hasStatus 404

-- | The authorization quota for the cluster security group has been reached.
_AuthorizationQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AuthorizationQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "AuthorizationQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The maximum number for a batch delete of snapshots has been reached. The
-- limit is 100.
_BatchDeleteRequestSizeExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_BatchDeleteRequestSizeExceededFault =
  Core._MatchServiceError
    defaultService
    "BatchDeleteRequestSizeExceeded"
    Prelude.. Core.hasStatus 400

-- | The maximum number for snapshot identifiers has been reached. The limit
-- is 100.
_BatchModifyClusterSnapshotsLimitExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_BatchModifyClusterSnapshotsLimitExceededFault =
  Core._MatchServiceError
    defaultService
    "BatchModifyClusterSnapshotsLimitExceededFault"
    Prelude.. Core.hasStatus 400

-- | Could not find the specified S3 bucket.
_BucketNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_BucketNotFoundFault =
  Core._MatchServiceError
    defaultService
    "BucketNotFoundFault"
    Prelude.. Core.hasStatus 400

-- | The account already has a cluster with the given identifier.
_ClusterAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ClusterAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "ClusterAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The @ClusterIdentifier@ parameter does not refer to an existing cluster.
_ClusterNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ClusterNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ClusterNotFound"
    Prelude.. Core.hasStatus 404

-- | Cluster is already on the latest database revision.
_ClusterOnLatestRevisionFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ClusterOnLatestRevisionFault =
  Core._MatchServiceError
    defaultService
    "ClusterOnLatestRevision"
    Prelude.. Core.hasStatus 400

-- | A cluster parameter group with the same name already exists.
_ClusterParameterGroupAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ClusterParameterGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "ClusterParameterGroupAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The parameter group name does not refer to an existing parameter group.
_ClusterParameterGroupNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ClusterParameterGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ClusterParameterGroupNotFound"
    Prelude.. Core.hasStatus 404

-- | The request would result in the user exceeding the allowed number of
-- cluster parameter groups. For information about increasing your quota,
-- go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_ClusterParameterGroupQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ClusterParameterGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "ClusterParameterGroupQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The request would exceed the allowed number of cluster instances for
-- this account. For information about increasing your quota, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_ClusterQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ClusterQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "ClusterQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | A cluster security group with the same name already exists.
_ClusterSecurityGroupAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ClusterSecurityGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "ClusterSecurityGroupAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The cluster security group name does not refer to an existing cluster
-- security group.
_ClusterSecurityGroupNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ClusterSecurityGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ClusterSecurityGroupNotFound"
    Prelude.. Core.hasStatus 404

-- | The request would result in the user exceeding the allowed number of
-- cluster security groups. For information about increasing your quota, go
-- to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_ClusterSecurityGroupQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ClusterSecurityGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "QuotaExceeded.ClusterSecurityGroup"
    Prelude.. Core.hasStatus 400

-- | The value specified as a snapshot identifier is already used by an
-- existing snapshot.
_ClusterSnapshotAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ClusterSnapshotAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "ClusterSnapshotAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The snapshot identifier does not refer to an existing cluster snapshot.
_ClusterSnapshotNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ClusterSnapshotNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ClusterSnapshotNotFound"
    Prelude.. Core.hasStatus 404

-- | The request would result in the user exceeding the allowed number of
-- cluster snapshots.
_ClusterSnapshotQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ClusterSnapshotQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "ClusterSnapshotQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | A /ClusterSubnetGroupName/ is already used by an existing cluster subnet
-- group.
_ClusterSubnetGroupAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ClusterSubnetGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "ClusterSubnetGroupAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The cluster subnet group name does not refer to an existing cluster
-- subnet group.
_ClusterSubnetGroupNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ClusterSubnetGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ClusterSubnetGroupNotFoundFault"
    Prelude.. Core.hasStatus 400

-- | The request would result in user exceeding the allowed number of cluster
-- subnet groups. For information about increasing your quota, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_ClusterSubnetGroupQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ClusterSubnetGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "ClusterSubnetGroupQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The request would result in user exceeding the allowed number of subnets
-- in a cluster subnet groups. For information about increasing your quota,
-- go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_ClusterSubnetQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ClusterSubnetQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "ClusterSubnetQuotaExceededFault"
    Prelude.. Core.hasStatus 400

-- | Cross-region snapshot copy was temporarily disabled. Try your request
-- again.
_CopyToRegionDisabledFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CopyToRegionDisabledFault =
  Core._MatchServiceError
    defaultService
    "CopyToRegionDisabledFault"
    Prelude.. Core.hasStatus 400

-- | The request cannot be completed because a dependent service is
-- throttling requests made by Amazon Redshift on your behalf. Wait and
-- retry the request.
_DependentServiceRequestThrottlingFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DependentServiceRequestThrottlingFault =
  Core._MatchServiceError
    defaultService
    "DependentServiceRequestThrottlingFault"
    Prelude.. Core.hasStatus 400

-- | Your request cannot be completed because a dependent internal service is
-- temporarily unavailable. Wait 30 to 60 seconds and try again.
_DependentServiceUnavailableFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DependentServiceUnavailableFault =
  Core._MatchServiceError
    defaultService
    "DependentServiceUnavailableFault"
    Prelude.. Core.hasStatus 400

-- | The account already has a Redshift-managed VPC endpoint with the given
-- identifier.
_EndpointAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_EndpointAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "EndpointAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The authorization already exists for this endpoint.
_EndpointAuthorizationAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_EndpointAuthorizationAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "EndpointAuthorizationAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The authorization for this endpoint can\'t be found.
_EndpointAuthorizationNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_EndpointAuthorizationNotFoundFault =
  Core._MatchServiceError
    defaultService
    "EndpointAuthorizationNotFound"
    Prelude.. Core.hasStatus 404

-- | The number of endpoint authorizations per cluster has exceeded its
-- limit.
_EndpointAuthorizationsPerClusterLimitExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_EndpointAuthorizationsPerClusterLimitExceededFault =
  Core._MatchServiceError
    defaultService
    "EndpointAuthorizationsPerClusterLimitExceeded"
    Prelude.. Core.hasStatus 400

-- | The endpoint name doesn\'t refer to an existing endpoint.
_EndpointNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_EndpointNotFoundFault =
  Core._MatchServiceError
    defaultService
    "EndpointNotFound"
    Prelude.. Core.hasStatus 404

-- | The number of Redshift-managed VPC endpoints per authorization has
-- exceeded its limit.
_EndpointsPerAuthorizationLimitExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_EndpointsPerAuthorizationLimitExceededFault =
  Core._MatchServiceError
    defaultService
    "EndpointsPerAuthorizationLimitExceeded"
    Prelude.. Core.hasStatus 400

-- | The number of Redshift-managed VPC endpoints per cluster has exceeded
-- its limit.
_EndpointsPerClusterLimitExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_EndpointsPerClusterLimitExceededFault =
  Core._MatchServiceError
    defaultService
    "EndpointsPerClusterLimitExceeded"
    Prelude.. Core.hasStatus 400

-- | The request would exceed the allowed number of event subscriptions for
-- this account. For information about increasing your quota, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_EventSubscriptionQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_EventSubscriptionQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "EventSubscriptionQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | There is already an existing Amazon Redshift HSM client certificate with
-- the specified identifier.
_HsmClientCertificateAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_HsmClientCertificateAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "HsmClientCertificateAlreadyExistsFault"
    Prelude.. Core.hasStatus 400

-- | There is no Amazon Redshift HSM client certificate with the specified
-- identifier.
_HsmClientCertificateNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_HsmClientCertificateNotFoundFault =
  Core._MatchServiceError
    defaultService
    "HsmClientCertificateNotFoundFault"
    Prelude.. Core.hasStatus 400

-- | The quota for HSM client certificates has been reached. For information
-- about increasing your quota, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_HsmClientCertificateQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_HsmClientCertificateQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "HsmClientCertificateQuotaExceededFault"
    Prelude.. Core.hasStatus 400

-- | There is already an existing Amazon Redshift HSM configuration with the
-- specified identifier.
_HsmConfigurationAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_HsmConfigurationAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "HsmConfigurationAlreadyExistsFault"
    Prelude.. Core.hasStatus 400

-- | There is no Amazon Redshift HSM configuration with the specified
-- identifier.
_HsmConfigurationNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_HsmConfigurationNotFoundFault =
  Core._MatchServiceError
    defaultService
    "HsmConfigurationNotFoundFault"
    Prelude.. Core.hasStatus 400

-- | The quota for HSM configurations has been reached. For information about
-- increasing your quota, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_HsmConfigurationQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_HsmConfigurationQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "HsmConfigurationQuotaExceededFault"
    Prelude.. Core.hasStatus 400

-- | You have exceeded the allowed number of table restore requests. Wait for
-- your current table restore requests to complete before making a new
-- request.
_InProgressTableRestoreQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InProgressTableRestoreQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "InProgressTableRestoreQuotaExceededFault"
    Prelude.. Core.hasStatus 400

-- | The specified options are incompatible.
_IncompatibleOrderableOptions :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_IncompatibleOrderableOptions =
  Core._MatchServiceError
    defaultService
    "IncompatibleOrderableOptions"
    Prelude.. Core.hasStatus 400

-- | The number of nodes specified exceeds the allotted capacity of the
-- cluster.
_InsufficientClusterCapacityFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InsufficientClusterCapacityFault =
  Core._MatchServiceError
    defaultService
    "InsufficientClusterCapacity"
    Prelude.. Core.hasStatus 400

-- | The cluster does not have read bucket or put object permissions on the
-- S3 bucket specified when enabling logging.
_InsufficientS3BucketPolicyFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InsufficientS3BucketPolicyFault =
  Core._MatchServiceError
    defaultService
    "InsufficientS3BucketPolicyFault"
    Prelude.. Core.hasStatus 400

-- | The authentication profile request is not valid. The profile name can\'t
-- be null or empty. The authentication profile API operation must be
-- available in the Amazon Web Services Region.
_InvalidAuthenticationProfileRequestFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidAuthenticationProfileRequestFault =
  Core._MatchServiceError
    defaultService
    "InvalidAuthenticationProfileRequestFault"
    Prelude.. Core.hasStatus 400

-- | The status of the authorization is not valid.
_InvalidAuthorizationStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidAuthorizationStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidAuthorizationState"
    Prelude.. Core.hasStatus 400

-- | The cluster parameter group action can not be completed because another
-- task is in progress that involves the parameter group. Wait a few
-- moments and try the operation again.
_InvalidClusterParameterGroupStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidClusterParameterGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidClusterParameterGroupState"
    Prelude.. Core.hasStatus 400

-- | The state of the cluster security group is not @available@.
_InvalidClusterSecurityGroupStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidClusterSecurityGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidClusterSecurityGroupState"
    Prelude.. Core.hasStatus 400

-- | The cluster snapshot schedule state is not valid.
_InvalidClusterSnapshotScheduleStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidClusterSnapshotScheduleStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidClusterSnapshotScheduleState"
    Prelude.. Core.hasStatus 400

-- | The specified cluster snapshot is not in the @available@ state, or other
-- accounts are authorized to access the snapshot.
_InvalidClusterSnapshotStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidClusterSnapshotStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidClusterSnapshotState"
    Prelude.. Core.hasStatus 400

-- | The specified cluster is not in the @available@ state.
_InvalidClusterStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidClusterStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidClusterState"
    Prelude.. Core.hasStatus 400

-- | The cluster subnet group cannot be deleted because it is in use.
_InvalidClusterSubnetGroupStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidClusterSubnetGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidClusterSubnetGroupStateFault"
    Prelude.. Core.hasStatus 400

-- | The state of the subnet is invalid.
_InvalidClusterSubnetStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidClusterSubnetStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidClusterSubnetStateFault"
    Prelude.. Core.hasStatus 400

-- | The provided cluster track name is not valid.
_InvalidClusterTrackFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidClusterTrackFault =
  Core._MatchServiceError
    defaultService
    "InvalidClusterTrack"
    Prelude.. Core.hasStatus 400

-- | There is an error with the datashare.
_InvalidDataShareFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidDataShareFault =
  Core._MatchServiceError
    defaultService
    "InvalidDataShareFault"
    Prelude.. Core.hasStatus 400

-- | The Elastic IP (EIP) is invalid or cannot be found.
_InvalidElasticIpFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidElasticIpFault =
  Core._MatchServiceError
    defaultService
    "InvalidElasticIpFault"
    Prelude.. Core.hasStatus 400

-- | The status of the endpoint is not valid.
_InvalidEndpointStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidEndpointStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidEndpointState"
    Prelude.. Core.hasStatus 400

-- | The specified HSM client certificate is not in the @available@ state, or
-- it is still in use by one or more Amazon Redshift clusters.
_InvalidHsmClientCertificateStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidHsmClientCertificateStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidHsmClientCertificateStateFault"
    Prelude.. Core.hasStatus 400

-- | The specified HSM configuration is not in the @available@ state, or it
-- is still in use by one or more Amazon Redshift clusters.
_InvalidHsmConfigurationStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidHsmConfigurationStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidHsmConfigurationStateFault"
    Prelude.. Core.hasStatus 400

-- | The namespace isn\'t valid because the namespace doesn\'t exist. Provide
-- a valid namespace.
_InvalidNamespaceFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidNamespaceFault =
  Core._MatchServiceError
    defaultService
    "InvalidNamespaceFault"
    Prelude.. Core.hasStatus 400

-- | Indicates that the Reserved Node being exchanged is not in an active
-- state.
_InvalidReservedNodeStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidReservedNodeStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidReservedNodeState"
    Prelude.. Core.hasStatus 400

-- | The restore is invalid.
_InvalidRestoreFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidRestoreFault =
  Core._MatchServiceError
    defaultService
    "InvalidRestore"
    Prelude.. Core.hasStatus 406

-- | The retention period specified is either in the past or is not a valid
-- value.
--
-- The value must be either -1 or an integer between 1 and 3,653.
_InvalidRetentionPeriodFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidRetentionPeriodFault =
  Core._MatchServiceError
    defaultService
    "InvalidRetentionPeriodFault"
    Prelude.. Core.hasStatus 400

-- | The S3 bucket name is invalid. For more information about naming rules,
-- go to
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/BucketRestrictions.html Bucket Restrictions and Limitations>
-- in the Amazon Simple Storage Service (S3) Developer Guide.
_InvalidS3BucketNameFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidS3BucketNameFault =
  Core._MatchServiceError
    defaultService
    "InvalidS3BucketNameFault"
    Prelude.. Core.hasStatus 400

-- | The string specified for the logging S3 key prefix does not comply with
-- the documented constraints.
_InvalidS3KeyPrefixFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidS3KeyPrefixFault =
  Core._MatchServiceError
    defaultService
    "InvalidS3KeyPrefixFault"
    Prelude.. Core.hasStatus 400

-- | The schedule you submitted isn\'t valid.
_InvalidScheduleFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidScheduleFault =
  Core._MatchServiceError
    defaultService
    "InvalidSchedule"
    Prelude.. Core.hasStatus 400

-- | The scheduled action is not valid.
_InvalidScheduledActionFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidScheduledActionFault =
  Core._MatchServiceError
    defaultService
    "InvalidScheduledAction"
    Prelude.. Core.hasStatus 400

-- | The snapshot copy grant can\'t be deleted because it is used by one or
-- more clusters.
_InvalidSnapshotCopyGrantStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidSnapshotCopyGrantStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidSnapshotCopyGrantStateFault"
    Prelude.. Core.hasStatus 400

-- | The requested subnet is not valid, or not all of the subnets are in the
-- same VPC.
_InvalidSubnet :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidSubnet =
  Core._MatchServiceError
    defaultService
    "InvalidSubnet"
    Prelude.. Core.hasStatus 400

-- | The subscription request is invalid because it is a duplicate request.
-- This subscription request is already in progress.
_InvalidSubscriptionStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidSubscriptionStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidSubscriptionStateFault"
    Prelude.. Core.hasStatus 400

-- | The value specified for the @sourceDatabaseName@, @sourceSchemaName@, or
-- @sourceTableName@ parameter, or a combination of these, doesn\'t exist
-- in the snapshot.
_InvalidTableRestoreArgumentFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidTableRestoreArgumentFault =
  Core._MatchServiceError
    defaultService
    "InvalidTableRestoreArgument"
    Prelude.. Core.hasStatus 400

-- | The tag is invalid.
_InvalidTagFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidTagFault =
  Core._MatchServiceError
    defaultService
    "InvalidTagFault"
    Prelude.. Core.hasStatus 400

-- | The usage limit is not valid.
_InvalidUsageLimitFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidUsageLimitFault =
  Core._MatchServiceError
    defaultService
    "InvalidUsageLimit"
    Prelude.. Core.hasStatus 400

-- | The cluster subnet group does not cover all Availability Zones.
_InvalidVPCNetworkStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidVPCNetworkStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidVPCNetworkStateFault"
    Prelude.. Core.hasStatus 400

-- | The encryption key has exceeded its grant limit in Amazon Web Services
-- KMS.
_LimitExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LimitExceededFault =
  Core._MatchServiceError
    defaultService
    "LimitExceededFault"
    Prelude.. Core.hasStatus 400

-- | The operation would exceed the number of nodes allowed for a cluster.
_NumberOfNodesPerClusterLimitExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NumberOfNodesPerClusterLimitExceededFault =
  Core._MatchServiceError
    defaultService
    "NumberOfNodesPerClusterLimitExceeded"
    Prelude.. Core.hasStatus 400

-- | The operation would exceed the number of nodes allotted to the account.
-- For information about increasing your quota, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_NumberOfNodesQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NumberOfNodesQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "NumberOfNodesQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The name of the partner was not found.
_PartnerNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_PartnerNotFoundFault =
  Core._MatchServiceError
    defaultService
    "PartnerNotFound"
    Prelude.. Core.hasStatus 404

-- | User already has a reservation with the given identifier.
_ReservedNodeAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ReservedNodeAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "ReservedNodeAlreadyExists"
    Prelude.. Core.hasStatus 404

-- | Indicates that the reserved node has already been exchanged.
_ReservedNodeAlreadyMigratedFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ReservedNodeAlreadyMigratedFault =
  Core._MatchServiceError
    defaultService
    "ReservedNodeAlreadyMigrated"
    Prelude.. Core.hasStatus 400

-- | The reserved-node exchange status wasn\'t found.
_ReservedNodeExchangeNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ReservedNodeExchangeNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ReservedNodeExchangeNotFond"
    Prelude.. Core.hasStatus 404

-- | The specified reserved compute node not found.
_ReservedNodeNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ReservedNodeNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ReservedNodeNotFound"
    Prelude.. Core.hasStatus 404

-- | Specified offering does not exist.
_ReservedNodeOfferingNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ReservedNodeOfferingNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ReservedNodeOfferingNotFound"
    Prelude.. Core.hasStatus 404

-- | Request would exceed the user\'s compute node quota. For information
-- about increasing your quota, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html Limits in Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
_ReservedNodeQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ReservedNodeQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "ReservedNodeQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | A resize operation for the specified cluster is not found.
_ResizeNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResizeNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ResizeNotFound"
    Prelude.. Core.hasStatus 404

-- | The resource could not be found.
_ResourceNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | Amazon SNS has responded that there is a problem with the specified
-- Amazon SNS topic.
_SNSInvalidTopicFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SNSInvalidTopicFault =
  Core._MatchServiceError
    defaultService
    "SNSInvalidTopic"
    Prelude.. Core.hasStatus 400

-- | You do not have permission to publish to the specified Amazon SNS topic.
_SNSNoAuthorizationFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SNSNoAuthorizationFault =
  Core._MatchServiceError
    defaultService
    "SNSNoAuthorization"
    Prelude.. Core.hasStatus 400

-- | An Amazon SNS topic with the specified Amazon Resource Name (ARN) does
-- not exist.
_SNSTopicArnNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SNSTopicArnNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SNSTopicArnNotFound"
    Prelude.. Core.hasStatus 404

-- | The definition you submitted is not supported.
_ScheduleDefinitionTypeUnsupportedFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ScheduleDefinitionTypeUnsupportedFault =
  Core._MatchServiceError
    defaultService
    "ScheduleDefinitionTypeUnsupported"
    Prelude.. Core.hasStatus 400

-- | The scheduled action already exists.
_ScheduledActionAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ScheduledActionAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "ScheduledActionAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The scheduled action cannot be found.
_ScheduledActionNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ScheduledActionNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ScheduledActionNotFound"
    Prelude.. Core.hasStatus 400

-- | The quota for scheduled actions exceeded.
_ScheduledActionQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ScheduledActionQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "ScheduledActionQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The action type specified for a scheduled action is not supported.
_ScheduledActionTypeUnsupportedFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ScheduledActionTypeUnsupportedFault =
  Core._MatchServiceError
    defaultService
    "ScheduledActionTypeUnsupported"
    Prelude.. Core.hasStatus 400

-- | The cluster already has cross-region snapshot copy disabled.
_SnapshotCopyAlreadyDisabledFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SnapshotCopyAlreadyDisabledFault =
  Core._MatchServiceError
    defaultService
    "SnapshotCopyAlreadyDisabledFault"
    Prelude.. Core.hasStatus 400

-- | The cluster already has cross-region snapshot copy enabled.
_SnapshotCopyAlreadyEnabledFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SnapshotCopyAlreadyEnabledFault =
  Core._MatchServiceError
    defaultService
    "SnapshotCopyAlreadyEnabledFault"
    Prelude.. Core.hasStatus 400

-- | Cross-region snapshot copy was temporarily disabled. Try your request
-- again.
_SnapshotCopyDisabledFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SnapshotCopyDisabledFault =
  Core._MatchServiceError
    defaultService
    "SnapshotCopyDisabledFault"
    Prelude.. Core.hasStatus 400

-- | The snapshot copy grant can\'t be created because a grant with the same
-- name already exists.
_SnapshotCopyGrantAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SnapshotCopyGrantAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "SnapshotCopyGrantAlreadyExistsFault"
    Prelude.. Core.hasStatus 400

-- | The specified snapshot copy grant can\'t be found. Make sure that the
-- name is typed correctly and that the grant exists in the destination
-- region.
_SnapshotCopyGrantNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SnapshotCopyGrantNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SnapshotCopyGrantNotFoundFault"
    Prelude.. Core.hasStatus 400

-- | The Amazon Web Services account has exceeded the maximum number of
-- snapshot copy grants in this region.
_SnapshotCopyGrantQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SnapshotCopyGrantQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "SnapshotCopyGrantQuotaExceededFault"
    Prelude.. Core.hasStatus 400

-- | The specified snapshot schedule already exists.
_SnapshotScheduleAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SnapshotScheduleAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "SnapshotScheduleAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | We could not find the specified snapshot schedule.
_SnapshotScheduleNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SnapshotScheduleNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SnapshotScheduleNotFound"
    Prelude.. Core.hasStatus 400

-- | You have exceeded the quota of snapshot schedules.
_SnapshotScheduleQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SnapshotScheduleQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "SnapshotScheduleQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The specified snapshot schedule is already being updated.
_SnapshotScheduleUpdateInProgressFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SnapshotScheduleUpdateInProgressFault =
  Core._MatchServiceError
    defaultService
    "SnapshotScheduleUpdateInProgress"
    Prelude.. Core.hasStatus 400

-- | The specified Amazon Redshift event source could not be found.
_SourceNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SourceNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SourceNotFound"
    Prelude.. Core.hasStatus 404

-- | A specified subnet is already in use by another cluster.
_SubnetAlreadyInUse :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SubnetAlreadyInUse =
  Core._MatchServiceError
    defaultService
    "SubnetAlreadyInUse"
    Prelude.. Core.hasStatus 400

-- | There is already an existing event notification subscription with the
-- specified name.
_SubscriptionAlreadyExistFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SubscriptionAlreadyExistFault =
  Core._MatchServiceError
    defaultService
    "SubscriptionAlreadyExist"
    Prelude.. Core.hasStatus 400

-- | The value specified for the event category was not one of the allowed
-- values, or it specified a category that does not apply to the specified
-- source type. The allowed values are Configuration, Management,
-- Monitoring, and Security.
_SubscriptionCategoryNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SubscriptionCategoryNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SubscriptionCategoryNotFound"
    Prelude.. Core.hasStatus 404

-- | An Amazon Redshift event with the specified event ID does not exist.
_SubscriptionEventIdNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SubscriptionEventIdNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SubscriptionEventIdNotFound"
    Prelude.. Core.hasStatus 404

-- | An Amazon Redshift event notification subscription with the specified
-- name does not exist.
_SubscriptionNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SubscriptionNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SubscriptionNotFound"
    Prelude.. Core.hasStatus 404

-- | The value specified for the event severity was not one of the allowed
-- values, or it specified a severity that does not apply to the specified
-- source type. The allowed values are ERROR and INFO.
_SubscriptionSeverityNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SubscriptionSeverityNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SubscriptionSeverityNotFound"
    Prelude.. Core.hasStatus 404

-- | The number of tables in the cluster exceeds the limit for the requested
-- new cluster node type.
_TableLimitExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TableLimitExceededFault =
  Core._MatchServiceError
    defaultService
    "TableLimitExceeded"
    Prelude.. Core.hasStatus 400

-- | The specified @TableRestoreRequestId@ value was not found.
_TableRestoreNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TableRestoreNotFoundFault =
  Core._MatchServiceError
    defaultService
    "TableRestoreNotFoundFault"
    Prelude.. Core.hasStatus 400

-- | You have exceeded the number of tags allowed.
_TagLimitExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TagLimitExceededFault =
  Core._MatchServiceError
    defaultService
    "TagLimitExceededFault"
    Prelude.. Core.hasStatus 400

-- | Your account is not authorized to perform the requested operation.
_UnauthorizedOperation :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UnauthorizedOperation =
  Core._MatchServiceError
    defaultService
    "UnauthorizedOperation"
    Prelude.. Core.hasStatus 400

-- | The partner integration is not authorized.
_UnauthorizedPartnerIntegrationFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UnauthorizedPartnerIntegrationFault =
  Core._MatchServiceError
    defaultService
    "UnauthorizedPartnerIntegration"
    Prelude.. Core.hasStatus 401

-- | The specified region is incorrect or does not exist.
_UnknownSnapshotCopyRegionFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UnknownSnapshotCopyRegionFault =
  Core._MatchServiceError
    defaultService
    "UnknownSnapshotCopyRegionFault"
    Prelude.. Core.hasStatus 404

-- | The requested operation isn\'t supported.
_UnsupportedOperationFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UnsupportedOperationFault =
  Core._MatchServiceError
    defaultService
    "UnsupportedOperation"
    Prelude.. Core.hasStatus 400

-- | A request option was specified that is not supported.
_UnsupportedOptionFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UnsupportedOptionFault =
  Core._MatchServiceError
    defaultService
    "UnsupportedOptionFault"
    Prelude.. Core.hasStatus 400

-- | The usage limit already exists.
_UsageLimitAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UsageLimitAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "UsageLimitAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The usage limit identifier can\'t be found.
_UsageLimitNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UsageLimitNotFoundFault =
  Core._MatchServiceError
    defaultService
    "UsageLimitNotFound"
    Prelude.. Core.hasStatus 404
