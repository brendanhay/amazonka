{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Lens
  ( -- * Operations

    -- ** CancelResize
    cancelResize_clusterIdentifier,
    resizeProgressMessage_importTablesNotStarted,
    resizeProgressMessage_status,
    resizeProgressMessage_estimatedTimeToCompletionInSeconds,
    resizeProgressMessage_avgResizeRateInMegaBytesPerSecond,
    resizeProgressMessage_targetNumberOfNodes,
    resizeProgressMessage_targetEncryptionType,
    resizeProgressMessage_targetNodeType,
    resizeProgressMessage_importTablesInProgress,
    resizeProgressMessage_resizeType,
    resizeProgressMessage_importTablesCompleted,
    resizeProgressMessage_progressInMegaBytes,
    resizeProgressMessage_dataTransferProgressPercent,
    resizeProgressMessage_totalResizeDataInMegaBytes,
    resizeProgressMessage_targetClusterType,
    resizeProgressMessage_message,
    resizeProgressMessage_elapsedTimeInSeconds,

    -- ** DescribeStorage
    describeStorageResponse_totalProvisionedStorageInMegaBytes,
    describeStorageResponse_totalBackupSizeInMegaBytes,
    describeStorageResponse_httpStatus,

    -- ** DescribeClusters
    describeClusters_tagValues,
    describeClusters_tagKeys,
    describeClusters_clusterIdentifier,
    describeClusters_marker,
    describeClusters_maxRecords,
    describeClustersResponse_marker,
    describeClustersResponse_clusters,
    describeClustersResponse_httpStatus,

    -- ** DescribeTags
    describeTags_tagValues,
    describeTags_resourceType,
    describeTags_resourceName,
    describeTags_tagKeys,
    describeTags_marker,
    describeTags_maxRecords,
    describeTagsResponse_marker,
    describeTagsResponse_taggedResources,
    describeTagsResponse_httpStatus,

    -- ** CreateUsageLimit
    createUsageLimit_period,
    createUsageLimit_breachAction,
    createUsageLimit_tags,
    createUsageLimit_clusterIdentifier,
    createUsageLimit_featureType,
    createUsageLimit_limitType,
    createUsageLimit_amount,
    usageLimit_amount,
    usageLimit_limitType,
    usageLimit_usageLimitId,
    usageLimit_period,
    usageLimit_clusterIdentifier,
    usageLimit_breachAction,
    usageLimit_featureType,
    usageLimit_tags,

    -- ** ModifyEndpointAccess
    modifyEndpointAccess_vpcSecurityGroupIds,
    modifyEndpointAccess_endpointName,
    endpointAccess_endpointName,
    endpointAccess_endpointCreateTime,
    endpointAccess_subnetGroupName,
    endpointAccess_address,
    endpointAccess_clusterIdentifier,
    endpointAccess_endpointStatus,
    endpointAccess_vpcSecurityGroups,
    endpointAccess_resourceOwner,
    endpointAccess_vpcEndpoint,
    endpointAccess_port,

    -- ** AssociateDataShareConsumer
    associateDataShareConsumer_associateEntireAccount,
    associateDataShareConsumer_consumerArn,
    associateDataShareConsumer_dataShareArn,
    dataShare_producerArn,
    dataShare_dataShareAssociations,
    dataShare_dataShareArn,
    dataShare_allowPubliclyAccessibleConsumers,

    -- ** DeleteClusterSubnetGroup
    deleteClusterSubnetGroup_clusterSubnetGroupName,

    -- ** ModifyScheduledAction
    modifyScheduledAction_targetAction,
    modifyScheduledAction_startTime,
    modifyScheduledAction_schedule,
    modifyScheduledAction_scheduledActionDescription,
    modifyScheduledAction_enable,
    modifyScheduledAction_endTime,
    modifyScheduledAction_iamRole,
    modifyScheduledAction_scheduledActionName,
    scheduledAction_state,
    scheduledAction_targetAction,
    scheduledAction_startTime,
    scheduledAction_schedule,
    scheduledAction_scheduledActionName,
    scheduledAction_scheduledActionDescription,
    scheduledAction_nextInvocations,
    scheduledAction_endTime,
    scheduledAction_iamRole,

    -- ** DisableLogging
    disableLogging_clusterIdentifier,
    loggingStatus_lastFailureTime,
    loggingStatus_lastSuccessfulDeliveryTime,
    loggingStatus_s3KeyPrefix,
    loggingStatus_bucketName,
    loggingStatus_loggingEnabled,
    loggingStatus_lastFailureMessage,

    -- ** DescribeSnapshotSchedules
    describeSnapshotSchedules_tagValues,
    describeSnapshotSchedules_tagKeys,
    describeSnapshotSchedules_clusterIdentifier,
    describeSnapshotSchedules_marker,
    describeSnapshotSchedules_maxRecords,
    describeSnapshotSchedules_scheduleIdentifier,
    describeSnapshotSchedulesResponse_snapshotSchedules,
    describeSnapshotSchedulesResponse_marker,
    describeSnapshotSchedulesResponse_httpStatus,

    -- ** RevokeEndpointAccess
    revokeEndpointAccess_force,
    revokeEndpointAccess_clusterIdentifier,
    revokeEndpointAccess_account,
    revokeEndpointAccess_vpcIds,
    endpointAuthorization_status,
    endpointAuthorization_allowedAllVPCs,
    endpointAuthorization_endpointCount,
    endpointAuthorization_grantor,
    endpointAuthorization_clusterIdentifier,
    endpointAuthorization_grantee,
    endpointAuthorization_allowedVPCs,
    endpointAuthorization_clusterStatus,
    endpointAuthorization_authorizeTime,

    -- ** ModifyEventSubscription
    modifyEventSubscription_snsTopicArn,
    modifyEventSubscription_enabled,
    modifyEventSubscription_sourceType,
    modifyEventSubscription_severity,
    modifyEventSubscription_eventCategories,
    modifyEventSubscription_sourceIds,
    modifyEventSubscription_subscriptionName,
    modifyEventSubscriptionResponse_eventSubscription,
    modifyEventSubscriptionResponse_httpStatus,

    -- ** ModifyClusterDbRevision
    modifyClusterDbRevision_clusterIdentifier,
    modifyClusterDbRevision_revisionTarget,
    modifyClusterDbRevisionResponse_cluster,
    modifyClusterDbRevisionResponse_httpStatus,

    -- ** DeleteClusterSnapshot
    deleteClusterSnapshot_snapshotClusterIdentifier,
    deleteClusterSnapshot_snapshotIdentifier,
    deleteClusterSnapshotResponse_snapshot,
    deleteClusterSnapshotResponse_httpStatus,

    -- ** AddPartner
    addPartner_accountId,
    addPartner_clusterIdentifier,
    addPartner_databaseName,
    addPartner_partnerName,
    partnerIntegrationOutputMessage_partnerName,
    partnerIntegrationOutputMessage_databaseName,

    -- ** PurchaseReservedNodeOffering
    purchaseReservedNodeOffering_nodeCount,
    purchaseReservedNodeOffering_reservedNodeOfferingId,
    purchaseReservedNodeOfferingResponse_reservedNode,
    purchaseReservedNodeOfferingResponse_httpStatus,

    -- ** DescribeReservedNodeOfferings
    describeReservedNodeOfferings_reservedNodeOfferingId,
    describeReservedNodeOfferings_marker,
    describeReservedNodeOfferings_maxRecords,
    describeReservedNodeOfferingsResponse_reservedNodeOfferings,
    describeReservedNodeOfferingsResponse_marker,
    describeReservedNodeOfferingsResponse_httpStatus,

    -- ** DescribeEndpointAccess
    describeEndpointAccess_endpointName,
    describeEndpointAccess_vpcId,
    describeEndpointAccess_clusterIdentifier,
    describeEndpointAccess_marker,
    describeEndpointAccess_maxRecords,
    describeEndpointAccess_resourceOwner,
    describeEndpointAccessResponse_marker,
    describeEndpointAccessResponse_endpointAccessList,
    describeEndpointAccessResponse_httpStatus,

    -- ** DescribeEvents
    describeEvents_startTime,
    describeEvents_sourceType,
    describeEvents_sourceIdentifier,
    describeEvents_marker,
    describeEvents_maxRecords,
    describeEvents_endTime,
    describeEvents_duration,
    describeEventsResponse_events,
    describeEventsResponse_marker,
    describeEventsResponse_httpStatus,

    -- ** DescribeReservedNodes
    describeReservedNodes_reservedNodeId,
    describeReservedNodes_marker,
    describeReservedNodes_maxRecords,
    describeReservedNodesResponse_reservedNodes,
    describeReservedNodesResponse_marker,
    describeReservedNodesResponse_httpStatus,

    -- ** GetReservedNodeExchangeOfferings
    getReservedNodeExchangeOfferings_marker,
    getReservedNodeExchangeOfferings_maxRecords,
    getReservedNodeExchangeOfferings_reservedNodeId,
    getReservedNodeExchangeOfferingsResponse_reservedNodeOfferings,
    getReservedNodeExchangeOfferingsResponse_marker,
    getReservedNodeExchangeOfferingsResponse_httpStatus,

    -- ** DeleteAuthenticationProfile
    deleteAuthenticationProfile_authenticationProfileName,
    deleteAuthenticationProfileResponse_authenticationProfileName,
    deleteAuthenticationProfileResponse_httpStatus,

    -- ** DescribeClusterParameterGroups
    describeClusterParameterGroups_tagValues,
    describeClusterParameterGroups_tagKeys,
    describeClusterParameterGroups_marker,
    describeClusterParameterGroups_maxRecords,
    describeClusterParameterGroups_parameterGroupName,
    describeClusterParameterGroupsResponse_marker,
    describeClusterParameterGroupsResponse_parameterGroups,
    describeClusterParameterGroupsResponse_httpStatus,

    -- ** EnableLogging
    enableLogging_s3KeyPrefix,
    enableLogging_clusterIdentifier,
    enableLogging_bucketName,
    loggingStatus_lastFailureTime,
    loggingStatus_lastSuccessfulDeliveryTime,
    loggingStatus_s3KeyPrefix,
    loggingStatus_bucketName,
    loggingStatus_loggingEnabled,
    loggingStatus_lastFailureMessage,

    -- ** CreateClusterSubnetGroup
    createClusterSubnetGroup_tags,
    createClusterSubnetGroup_clusterSubnetGroupName,
    createClusterSubnetGroup_description,
    createClusterSubnetGroup_subnetIds,
    createClusterSubnetGroupResponse_clusterSubnetGroup,
    createClusterSubnetGroupResponse_httpStatus,

    -- ** DeleteClusterParameterGroup
    deleteClusterParameterGroup_parameterGroupName,

    -- ** DescribeClusterSecurityGroups
    describeClusterSecurityGroups_tagValues,
    describeClusterSecurityGroups_tagKeys,
    describeClusterSecurityGroups_clusterSecurityGroupName,
    describeClusterSecurityGroups_marker,
    describeClusterSecurityGroups_maxRecords,
    describeClusterSecurityGroupsResponse_clusterSecurityGroups,
    describeClusterSecurityGroupsResponse_marker,
    describeClusterSecurityGroupsResponse_httpStatus,

    -- ** CreateTags
    createTags_resourceName,
    createTags_tags,

    -- ** DescribeEndpointAuthorization
    describeEndpointAuthorization_clusterIdentifier,
    describeEndpointAuthorization_account,
    describeEndpointAuthorization_marker,
    describeEndpointAuthorization_maxRecords,
    describeEndpointAuthorization_grantee,
    describeEndpointAuthorizationResponse_endpointAuthorizationList,
    describeEndpointAuthorizationResponse_marker,
    describeEndpointAuthorizationResponse_httpStatus,

    -- ** EnableSnapshotCopy
    enableSnapshotCopy_manualSnapshotRetentionPeriod,
    enableSnapshotCopy_retentionPeriod,
    enableSnapshotCopy_snapshotCopyGrantName,
    enableSnapshotCopy_clusterIdentifier,
    enableSnapshotCopy_destinationRegion,
    enableSnapshotCopyResponse_cluster,
    enableSnapshotCopyResponse_httpStatus,

    -- ** DescribeClusterSnapshots
    describeClusterSnapshots_snapshotIdentifier,
    describeClusterSnapshots_tagValues,
    describeClusterSnapshots_clusterExists,
    describeClusterSnapshots_startTime,
    describeClusterSnapshots_tagKeys,
    describeClusterSnapshots_clusterIdentifier,
    describeClusterSnapshots_snapshotType,
    describeClusterSnapshots_sortingEntities,
    describeClusterSnapshots_marker,
    describeClusterSnapshots_maxRecords,
    describeClusterSnapshots_endTime,
    describeClusterSnapshots_ownerAccount,
    describeClusterSnapshotsResponse_snapshots,
    describeClusterSnapshotsResponse_marker,
    describeClusterSnapshotsResponse_httpStatus,

    -- ** BatchDeleteClusterSnapshots
    batchDeleteClusterSnapshots_identifiers,
    batchDeleteClusterSnapshotsResponse_resources,
    batchDeleteClusterSnapshotsResponse_errors,
    batchDeleteClusterSnapshotsResponse_httpStatus,

    -- ** DeleteTags
    deleteTags_resourceName,
    deleteTags_tagKeys,

    -- ** ModifyUsageLimit
    modifyUsageLimit_amount,
    modifyUsageLimit_breachAction,
    modifyUsageLimit_usageLimitId,
    usageLimit_amount,
    usageLimit_limitType,
    usageLimit_usageLimitId,
    usageLimit_period,
    usageLimit_clusterIdentifier,
    usageLimit_breachAction,
    usageLimit_featureType,
    usageLimit_tags,

    -- ** DescribeClusterSubnetGroups
    describeClusterSubnetGroups_tagValues,
    describeClusterSubnetGroups_tagKeys,
    describeClusterSubnetGroups_clusterSubnetGroupName,
    describeClusterSubnetGroups_marker,
    describeClusterSubnetGroups_maxRecords,
    describeClusterSubnetGroupsResponse_clusterSubnetGroups,
    describeClusterSubnetGroupsResponse_marker,
    describeClusterSubnetGroupsResponse_httpStatus,

    -- ** ResizeCluster
    resizeCluster_numberOfNodes,
    resizeCluster_classic,
    resizeCluster_clusterType,
    resizeCluster_nodeType,
    resizeCluster_clusterIdentifier,
    resizeClusterResponse_cluster,
    resizeClusterResponse_httpStatus,

    -- ** ModifySnapshotCopyRetentionPeriod
    modifySnapshotCopyRetentionPeriod_manual,
    modifySnapshotCopyRetentionPeriod_clusterIdentifier,
    modifySnapshotCopyRetentionPeriod_retentionPeriod,
    modifySnapshotCopyRetentionPeriodResponse_cluster,
    modifySnapshotCopyRetentionPeriodResponse_httpStatus,

    -- ** ModifyClusterIamRoles
    modifyClusterIamRoles_removeIamRoles,
    modifyClusterIamRoles_addIamRoles,
    modifyClusterIamRoles_clusterIdentifier,
    modifyClusterIamRolesResponse_cluster,
    modifyClusterIamRolesResponse_httpStatus,

    -- ** AuthorizeSnapshotAccess
    authorizeSnapshotAccess_snapshotClusterIdentifier,
    authorizeSnapshotAccess_snapshotIdentifier,
    authorizeSnapshotAccess_accountWithRestoreAccess,
    authorizeSnapshotAccessResponse_snapshot,
    authorizeSnapshotAccessResponse_httpStatus,

    -- ** RebootCluster
    rebootCluster_clusterIdentifier,
    rebootClusterResponse_cluster,
    rebootClusterResponse_httpStatus,

    -- ** ResumeCluster
    resumeCluster_clusterIdentifier,
    resumeClusterResponse_cluster,
    resumeClusterResponse_httpStatus,

    -- ** DeleteCluster
    deleteCluster_skipFinalClusterSnapshot,
    deleteCluster_finalClusterSnapshotRetentionPeriod,
    deleteCluster_finalClusterSnapshotIdentifier,
    deleteCluster_clusterIdentifier,
    deleteClusterResponse_cluster,
    deleteClusterResponse_httpStatus,

    -- ** CreateEventSubscription
    createEventSubscription_enabled,
    createEventSubscription_sourceType,
    createEventSubscription_severity,
    createEventSubscription_eventCategories,
    createEventSubscription_sourceIds,
    createEventSubscription_tags,
    createEventSubscription_subscriptionName,
    createEventSubscription_snsTopicArn,
    createEventSubscriptionResponse_eventSubscription,
    createEventSubscriptionResponse_httpStatus,

    -- ** CreateScheduledAction
    createScheduledAction_startTime,
    createScheduledAction_scheduledActionDescription,
    createScheduledAction_enable,
    createScheduledAction_endTime,
    createScheduledAction_scheduledActionName,
    createScheduledAction_targetAction,
    createScheduledAction_schedule,
    createScheduledAction_iamRole,
    scheduledAction_state,
    scheduledAction_targetAction,
    scheduledAction_startTime,
    scheduledAction_schedule,
    scheduledAction_scheduledActionName,
    scheduledAction_scheduledActionDescription,
    scheduledAction_nextInvocations,
    scheduledAction_endTime,
    scheduledAction_iamRole,

    -- ** DescribeOrderableClusterOptions
    describeOrderableClusterOptions_marker,
    describeOrderableClusterOptions_maxRecords,
    describeOrderableClusterOptions_clusterVersion,
    describeOrderableClusterOptions_nodeType,
    describeOrderableClusterOptionsResponse_marker,
    describeOrderableClusterOptionsResponse_orderableClusterOptions,
    describeOrderableClusterOptionsResponse_httpStatus,

    -- ** CreateEndpointAccess
    createEndpointAccess_clusterIdentifier,
    createEndpointAccess_vpcSecurityGroupIds,
    createEndpointAccess_resourceOwner,
    createEndpointAccess_endpointName,
    createEndpointAccess_subnetGroupName,
    endpointAccess_endpointName,
    endpointAccess_endpointCreateTime,
    endpointAccess_subnetGroupName,
    endpointAccess_address,
    endpointAccess_clusterIdentifier,
    endpointAccess_endpointStatus,
    endpointAccess_vpcSecurityGroups,
    endpointAccess_resourceOwner,
    endpointAccess_vpcEndpoint,
    endpointAccess_port,

    -- ** DescribeClusterTracks
    describeClusterTracks_maintenanceTrackName,
    describeClusterTracks_marker,
    describeClusterTracks_maxRecords,
    describeClusterTracksResponse_maintenanceTracks,
    describeClusterTracksResponse_marker,
    describeClusterTracksResponse_httpStatus,

    -- ** CreateCluster
    createCluster_manualSnapshotRetentionPeriod,
    createCluster_enhancedVpcRouting,
    createCluster_additionalInfo,
    createCluster_snapshotScheduleIdentifier,
    createCluster_publiclyAccessible,
    createCluster_maintenanceTrackName,
    createCluster_hsmConfigurationIdentifier,
    createCluster_aquaConfigurationStatus,
    createCluster_clusterSecurityGroups,
    createCluster_automatedSnapshotRetentionPeriod,
    createCluster_encrypted,
    createCluster_clusterSubnetGroupName,
    createCluster_hsmClientCertificateIdentifier,
    createCluster_numberOfNodes,
    createCluster_elasticIp,
    createCluster_preferredMaintenanceWindow,
    createCluster_kmsKeyId,
    createCluster_availabilityZone,
    createCluster_vpcSecurityGroupIds,
    createCluster_iamRoles,
    createCluster_clusterType,
    createCluster_availabilityZoneRelocation,
    createCluster_clusterVersion,
    createCluster_allowVersionUpgrade,
    createCluster_clusterParameterGroupName,
    createCluster_tags,
    createCluster_port,
    createCluster_dbName,
    createCluster_clusterIdentifier,
    createCluster_nodeType,
    createCluster_masterUsername,
    createCluster_masterUserPassword,
    createClusterResponse_cluster,
    createClusterResponse_httpStatus,

    -- ** CreateHsmClientCertificate
    createHsmClientCertificate_tags,
    createHsmClientCertificate_hsmClientCertificateIdentifier,
    createHsmClientCertificateResponse_hsmClientCertificate,
    createHsmClientCertificateResponse_httpStatus,

    -- ** RestoreTableFromClusterSnapshot
    restoreTableFromClusterSnapshot_targetSchemaName,
    restoreTableFromClusterSnapshot_enableCaseSensitiveIdentifier,
    restoreTableFromClusterSnapshot_targetDatabaseName,
    restoreTableFromClusterSnapshot_sourceSchemaName,
    restoreTableFromClusterSnapshot_clusterIdentifier,
    restoreTableFromClusterSnapshot_snapshotIdentifier,
    restoreTableFromClusterSnapshot_sourceDatabaseName,
    restoreTableFromClusterSnapshot_sourceTableName,
    restoreTableFromClusterSnapshot_newTableName,
    restoreTableFromClusterSnapshotResponse_tableRestoreStatus,
    restoreTableFromClusterSnapshotResponse_httpStatus,

    -- ** DeleteScheduledAction
    deleteScheduledAction_scheduledActionName,

    -- ** DescribeDefaultClusterParameters
    describeDefaultClusterParameters_marker,
    describeDefaultClusterParameters_maxRecords,
    describeDefaultClusterParameters_parameterGroupFamily,
    describeDefaultClusterParametersResponse_httpStatus,
    describeDefaultClusterParametersResponse_defaultClusterParameters,

    -- ** DeleteEventSubscription
    deleteEventSubscription_subscriptionName,

    -- ** ModifyClusterSnapshot
    modifyClusterSnapshot_manualSnapshotRetentionPeriod,
    modifyClusterSnapshot_force,
    modifyClusterSnapshot_snapshotIdentifier,
    modifyClusterSnapshotResponse_snapshot,
    modifyClusterSnapshotResponse_httpStatus,

    -- ** DescribeDataSharesForConsumer
    describeDataSharesForConsumer_status,
    describeDataSharesForConsumer_consumerArn,
    describeDataSharesForConsumer_marker,
    describeDataSharesForConsumer_maxRecords,
    describeDataSharesForConsumerResponse_marker,
    describeDataSharesForConsumerResponse_dataShares,
    describeDataSharesForConsumerResponse_httpStatus,

    -- ** AuthorizeDataShare
    authorizeDataShare_dataShareArn,
    authorizeDataShare_consumerIdentifier,
    dataShare_producerArn,
    dataShare_dataShareAssociations,
    dataShare_dataShareArn,
    dataShare_allowPubliclyAccessibleConsumers,

    -- ** ResetClusterParameterGroup
    resetClusterParameterGroup_resetAllParameters,
    resetClusterParameterGroup_parameters,
    resetClusterParameterGroup_parameterGroupName,
    clusterParameterGroupNameMessage_parameterGroupStatus,
    clusterParameterGroupNameMessage_parameterGroupName,

    -- ** DescribeScheduledActions
    describeScheduledActions_startTime,
    describeScheduledActions_scheduledActionName,
    describeScheduledActions_filters,
    describeScheduledActions_active,
    describeScheduledActions_targetActionType,
    describeScheduledActions_marker,
    describeScheduledActions_maxRecords,
    describeScheduledActions_endTime,
    describeScheduledActionsResponse_scheduledActions,
    describeScheduledActionsResponse_marker,
    describeScheduledActionsResponse_httpStatus,

    -- ** DisassociateDataShareConsumer
    disassociateDataShareConsumer_disassociateEntireAccount,
    disassociateDataShareConsumer_consumerArn,
    disassociateDataShareConsumer_dataShareArn,
    dataShare_producerArn,
    dataShare_dataShareAssociations,
    dataShare_dataShareArn,
    dataShare_allowPubliclyAccessibleConsumers,

    -- ** DescribeEventSubscriptions
    describeEventSubscriptions_subscriptionName,
    describeEventSubscriptions_tagValues,
    describeEventSubscriptions_tagKeys,
    describeEventSubscriptions_marker,
    describeEventSubscriptions_maxRecords,
    describeEventSubscriptionsResponse_eventSubscriptionsList,
    describeEventSubscriptionsResponse_marker,
    describeEventSubscriptionsResponse_httpStatus,

    -- ** DescribeClusterDbRevisions
    describeClusterDbRevisions_clusterIdentifier,
    describeClusterDbRevisions_marker,
    describeClusterDbRevisions_maxRecords,
    describeClusterDbRevisionsResponse_clusterDbRevisions,
    describeClusterDbRevisionsResponse_marker,
    describeClusterDbRevisionsResponse_httpStatus,

    -- ** BatchModifyClusterSnapshots
    batchModifyClusterSnapshots_manualSnapshotRetentionPeriod,
    batchModifyClusterSnapshots_force,
    batchModifyClusterSnapshots_snapshotIdentifierList,
    batchModifyClusterSnapshotsResponse_resources,
    batchModifyClusterSnapshotsResponse_errors,
    batchModifyClusterSnapshotsResponse_httpStatus,

    -- ** DeleteUsageLimit
    deleteUsageLimit_usageLimitId,

    -- ** RevokeClusterSecurityGroupIngress
    revokeClusterSecurityGroupIngress_eC2SecurityGroupOwnerId,
    revokeClusterSecurityGroupIngress_eC2SecurityGroupName,
    revokeClusterSecurityGroupIngress_cidrip,
    revokeClusterSecurityGroupIngress_clusterSecurityGroupName,
    revokeClusterSecurityGroupIngressResponse_clusterSecurityGroup,
    revokeClusterSecurityGroupIngressResponse_httpStatus,

    -- ** DescribeHsmClientCertificates
    describeHsmClientCertificates_tagValues,
    describeHsmClientCertificates_tagKeys,
    describeHsmClientCertificates_hsmClientCertificateIdentifier,
    describeHsmClientCertificates_marker,
    describeHsmClientCertificates_maxRecords,
    describeHsmClientCertificatesResponse_marker,
    describeHsmClientCertificatesResponse_hsmClientCertificates,
    describeHsmClientCertificatesResponse_httpStatus,

    -- ** ModifyClusterParameterGroup
    modifyClusterParameterGroup_parameterGroupName,
    modifyClusterParameterGroup_parameters,
    clusterParameterGroupNameMessage_parameterGroupStatus,
    clusterParameterGroupNameMessage_parameterGroupName,

    -- ** AuthorizeEndpointAccess
    authorizeEndpointAccess_clusterIdentifier,
    authorizeEndpointAccess_vpcIds,
    authorizeEndpointAccess_account,
    endpointAuthorization_status,
    endpointAuthorization_allowedAllVPCs,
    endpointAuthorization_endpointCount,
    endpointAuthorization_grantor,
    endpointAuthorization_clusterIdentifier,
    endpointAuthorization_grantee,
    endpointAuthorization_allowedVPCs,
    endpointAuthorization_clusterStatus,
    endpointAuthorization_authorizeTime,

    -- ** ModifyAquaConfiguration
    modifyAquaConfiguration_aquaConfigurationStatus,
    modifyAquaConfiguration_clusterIdentifier,
    modifyAquaConfigurationResponse_aquaConfiguration,
    modifyAquaConfigurationResponse_httpStatus,

    -- ** GetClusterCredentials
    getClusterCredentials_dbGroups,
    getClusterCredentials_durationSeconds,
    getClusterCredentials_autoCreate,
    getClusterCredentials_dbName,
    getClusterCredentials_dbUser,
    getClusterCredentials_clusterIdentifier,
    getClusterCredentialsResponse_dbUser,
    getClusterCredentialsResponse_expiration,
    getClusterCredentialsResponse_dbPassword,
    getClusterCredentialsResponse_httpStatus,

    -- ** ModifyClusterMaintenance
    modifyClusterMaintenance_deferMaintenanceEndTime,
    modifyClusterMaintenance_deferMaintenance,
    modifyClusterMaintenance_deferMaintenanceDuration,
    modifyClusterMaintenance_deferMaintenanceStartTime,
    modifyClusterMaintenance_deferMaintenanceIdentifier,
    modifyClusterMaintenance_clusterIdentifier,
    modifyClusterMaintenanceResponse_cluster,
    modifyClusterMaintenanceResponse_httpStatus,

    -- ** CreateClusterSecurityGroup
    createClusterSecurityGroup_tags,
    createClusterSecurityGroup_clusterSecurityGroupName,
    createClusterSecurityGroup_description,
    createClusterSecurityGroupResponse_clusterSecurityGroup,
    createClusterSecurityGroupResponse_httpStatus,

    -- ** DescribeEventCategories
    describeEventCategories_sourceType,
    describeEventCategoriesResponse_eventCategoriesMapList,
    describeEventCategoriesResponse_httpStatus,

    -- ** DescribeResize
    describeResize_clusterIdentifier,
    resizeProgressMessage_importTablesNotStarted,
    resizeProgressMessage_status,
    resizeProgressMessage_estimatedTimeToCompletionInSeconds,
    resizeProgressMessage_avgResizeRateInMegaBytesPerSecond,
    resizeProgressMessage_targetNumberOfNodes,
    resizeProgressMessage_targetEncryptionType,
    resizeProgressMessage_targetNodeType,
    resizeProgressMessage_importTablesInProgress,
    resizeProgressMessage_resizeType,
    resizeProgressMessage_importTablesCompleted,
    resizeProgressMessage_progressInMegaBytes,
    resizeProgressMessage_dataTransferProgressPercent,
    resizeProgressMessage_totalResizeDataInMegaBytes,
    resizeProgressMessage_targetClusterType,
    resizeProgressMessage_message,
    resizeProgressMessage_elapsedTimeInSeconds,

    -- ** DeleteHsmConfiguration
    deleteHsmConfiguration_hsmConfigurationIdentifier,

    -- ** CreateAuthenticationProfile
    createAuthenticationProfile_authenticationProfileName,
    createAuthenticationProfile_authenticationProfileContent,
    createAuthenticationProfileResponse_authenticationProfileName,
    createAuthenticationProfileResponse_authenticationProfileContent,
    createAuthenticationProfileResponse_httpStatus,

    -- ** DeauthorizeDataShare
    deauthorizeDataShare_dataShareArn,
    deauthorizeDataShare_consumerIdentifier,
    dataShare_producerArn,
    dataShare_dataShareAssociations,
    dataShare_dataShareArn,
    dataShare_allowPubliclyAccessibleConsumers,

    -- ** AcceptReservedNodeExchange
    acceptReservedNodeExchange_reservedNodeId,
    acceptReservedNodeExchange_targetReservedNodeOfferingId,
    acceptReservedNodeExchangeResponse_exchangedReservedNode,
    acceptReservedNodeExchangeResponse_httpStatus,

    -- ** AuthorizeClusterSecurityGroupIngress
    authorizeClusterSecurityGroupIngress_eC2SecurityGroupOwnerId,
    authorizeClusterSecurityGroupIngress_eC2SecurityGroupName,
    authorizeClusterSecurityGroupIngress_cidrip,
    authorizeClusterSecurityGroupIngress_clusterSecurityGroupName,
    authorizeClusterSecurityGroupIngressResponse_clusterSecurityGroup,
    authorizeClusterSecurityGroupIngressResponse_httpStatus,

    -- ** DeletePartner
    deletePartner_accountId,
    deletePartner_clusterIdentifier,
    deletePartner_databaseName,
    deletePartner_partnerName,
    partnerIntegrationOutputMessage_partnerName,
    partnerIntegrationOutputMessage_databaseName,

    -- ** DescribeTableRestoreStatus
    describeTableRestoreStatus_tableRestoreRequestId,
    describeTableRestoreStatus_clusterIdentifier,
    describeTableRestoreStatus_marker,
    describeTableRestoreStatus_maxRecords,
    describeTableRestoreStatusResponse_marker,
    describeTableRestoreStatusResponse_tableRestoreStatusDetails,
    describeTableRestoreStatusResponse_httpStatus,

    -- ** CreateClusterSnapshot
    createClusterSnapshot_manualSnapshotRetentionPeriod,
    createClusterSnapshot_tags,
    createClusterSnapshot_snapshotIdentifier,
    createClusterSnapshot_clusterIdentifier,
    createClusterSnapshotResponse_snapshot,
    createClusterSnapshotResponse_httpStatus,

    -- ** RejectDataShare
    rejectDataShare_dataShareArn,
    dataShare_producerArn,
    dataShare_dataShareAssociations,
    dataShare_dataShareArn,
    dataShare_allowPubliclyAccessibleConsumers,

    -- ** CreateHsmConfiguration
    createHsmConfiguration_tags,
    createHsmConfiguration_hsmConfigurationIdentifier,
    createHsmConfiguration_description,
    createHsmConfiguration_hsmIpAddress,
    createHsmConfiguration_hsmPartitionName,
    createHsmConfiguration_hsmPartitionPassword,
    createHsmConfiguration_hsmServerPublicCertificate,
    createHsmConfigurationResponse_hsmConfiguration,
    createHsmConfigurationResponse_httpStatus,

    -- ** DescribeLoggingStatus
    describeLoggingStatus_clusterIdentifier,
    loggingStatus_lastFailureTime,
    loggingStatus_lastSuccessfulDeliveryTime,
    loggingStatus_s3KeyPrefix,
    loggingStatus_bucketName,
    loggingStatus_loggingEnabled,
    loggingStatus_lastFailureMessage,

    -- ** ModifyCluster
    modifyCluster_manualSnapshotRetentionPeriod,
    modifyCluster_enhancedVpcRouting,
    modifyCluster_masterUserPassword,
    modifyCluster_publiclyAccessible,
    modifyCluster_maintenanceTrackName,
    modifyCluster_hsmConfigurationIdentifier,
    modifyCluster_clusterSecurityGroups,
    modifyCluster_automatedSnapshotRetentionPeriod,
    modifyCluster_encrypted,
    modifyCluster_hsmClientCertificateIdentifier,
    modifyCluster_numberOfNodes,
    modifyCluster_elasticIp,
    modifyCluster_preferredMaintenanceWindow,
    modifyCluster_kmsKeyId,
    modifyCluster_availabilityZone,
    modifyCluster_vpcSecurityGroupIds,
    modifyCluster_clusterType,
    modifyCluster_newClusterIdentifier,
    modifyCluster_availabilityZoneRelocation,
    modifyCluster_clusterVersion,
    modifyCluster_nodeType,
    modifyCluster_allowVersionUpgrade,
    modifyCluster_clusterParameterGroupName,
    modifyCluster_port,
    modifyCluster_clusterIdentifier,
    modifyClusterResponse_cluster,
    modifyClusterResponse_httpStatus,

    -- ** DeleteClusterSecurityGroup
    deleteClusterSecurityGroup_clusterSecurityGroupName,

    -- ** CreateSnapshotSchedule
    createSnapshotSchedule_nextInvocations,
    createSnapshotSchedule_scheduleDefinitions,
    createSnapshotSchedule_scheduleDescription,
    createSnapshotSchedule_scheduleIdentifier,
    createSnapshotSchedule_dryRun,
    createSnapshotSchedule_tags,
    snapshotSchedule_associatedClusters,
    snapshotSchedule_nextInvocations,
    snapshotSchedule_scheduleDefinitions,
    snapshotSchedule_scheduleDescription,
    snapshotSchedule_scheduleIdentifier,
    snapshotSchedule_associatedClusterCount,
    snapshotSchedule_tags,

    -- ** DescribeAuthenticationProfiles
    describeAuthenticationProfiles_authenticationProfileName,
    describeAuthenticationProfilesResponse_authenticationProfiles,
    describeAuthenticationProfilesResponse_httpStatus,

    -- ** DescribeNodeConfigurationOptions
    describeNodeConfigurationOptions_snapshotIdentifier,
    describeNodeConfigurationOptions_filters,
    describeNodeConfigurationOptions_clusterIdentifier,
    describeNodeConfigurationOptions_marker,
    describeNodeConfigurationOptions_maxRecords,
    describeNodeConfigurationOptions_ownerAccount,
    describeNodeConfigurationOptions_actionType,
    describeNodeConfigurationOptionsResponse_nodeConfigurationOptionList,
    describeNodeConfigurationOptionsResponse_marker,
    describeNodeConfigurationOptionsResponse_httpStatus,

    -- ** DisableSnapshotCopy
    disableSnapshotCopy_clusterIdentifier,
    disableSnapshotCopyResponse_cluster,
    disableSnapshotCopyResponse_httpStatus,

    -- ** DescribeClusterParameters
    describeClusterParameters_marker,
    describeClusterParameters_maxRecords,
    describeClusterParameters_source,
    describeClusterParameters_parameterGroupName,
    describeClusterParametersResponse_marker,
    describeClusterParametersResponse_parameters,
    describeClusterParametersResponse_httpStatus,

    -- ** PauseCluster
    pauseCluster_clusterIdentifier,
    pauseClusterResponse_cluster,
    pauseClusterResponse_httpStatus,

    -- ** DescribeDataSharesForProducer
    describeDataSharesForProducer_status,
    describeDataSharesForProducer_producerArn,
    describeDataSharesForProducer_marker,
    describeDataSharesForProducer_maxRecords,
    describeDataSharesForProducerResponse_marker,
    describeDataSharesForProducerResponse_dataShares,
    describeDataSharesForProducerResponse_httpStatus,

    -- ** DeleteSnapshotSchedule
    deleteSnapshotSchedule_scheduleIdentifier,

    -- ** RestoreFromClusterSnapshot
    restoreFromClusterSnapshot_manualSnapshotRetentionPeriod,
    restoreFromClusterSnapshot_enhancedVpcRouting,
    restoreFromClusterSnapshot_additionalInfo,
    restoreFromClusterSnapshot_snapshotScheduleIdentifier,
    restoreFromClusterSnapshot_publiclyAccessible,
    restoreFromClusterSnapshot_snapshotClusterIdentifier,
    restoreFromClusterSnapshot_maintenanceTrackName,
    restoreFromClusterSnapshot_hsmConfigurationIdentifier,
    restoreFromClusterSnapshot_aquaConfigurationStatus,
    restoreFromClusterSnapshot_clusterSecurityGroups,
    restoreFromClusterSnapshot_automatedSnapshotRetentionPeriod,
    restoreFromClusterSnapshot_clusterSubnetGroupName,
    restoreFromClusterSnapshot_hsmClientCertificateIdentifier,
    restoreFromClusterSnapshot_numberOfNodes,
    restoreFromClusterSnapshot_elasticIp,
    restoreFromClusterSnapshot_preferredMaintenanceWindow,
    restoreFromClusterSnapshot_kmsKeyId,
    restoreFromClusterSnapshot_availabilityZone,
    restoreFromClusterSnapshot_vpcSecurityGroupIds,
    restoreFromClusterSnapshot_iamRoles,
    restoreFromClusterSnapshot_availabilityZoneRelocation,
    restoreFromClusterSnapshot_ownerAccount,
    restoreFromClusterSnapshot_nodeType,
    restoreFromClusterSnapshot_allowVersionUpgrade,
    restoreFromClusterSnapshot_clusterParameterGroupName,
    restoreFromClusterSnapshot_port,
    restoreFromClusterSnapshot_clusterIdentifier,
    restoreFromClusterSnapshot_snapshotIdentifier,
    restoreFromClusterSnapshotResponse_cluster,
    restoreFromClusterSnapshotResponse_httpStatus,

    -- ** CreateClusterParameterGroup
    createClusterParameterGroup_tags,
    createClusterParameterGroup_parameterGroupName,
    createClusterParameterGroup_parameterGroupFamily,
    createClusterParameterGroup_description,
    createClusterParameterGroupResponse_clusterParameterGroup,
    createClusterParameterGroupResponse_httpStatus,

    -- ** DescribePartners
    describePartners_partnerName,
    describePartners_databaseName,
    describePartners_accountId,
    describePartners_clusterIdentifier,
    describePartnersResponse_partnerIntegrationInfoList,
    describePartnersResponse_httpStatus,

    -- ** RevokeSnapshotAccess
    revokeSnapshotAccess_snapshotClusterIdentifier,
    revokeSnapshotAccess_snapshotIdentifier,
    revokeSnapshotAccess_accountWithRestoreAccess,
    revokeSnapshotAccessResponse_snapshot,
    revokeSnapshotAccessResponse_httpStatus,

    -- ** DescribeHsmConfigurations
    describeHsmConfigurations_tagValues,
    describeHsmConfigurations_hsmConfigurationIdentifier,
    describeHsmConfigurations_tagKeys,
    describeHsmConfigurations_marker,
    describeHsmConfigurations_maxRecords,
    describeHsmConfigurationsResponse_marker,
    describeHsmConfigurationsResponse_hsmConfigurations,
    describeHsmConfigurationsResponse_httpStatus,

    -- ** DescribeAccountAttributes
    describeAccountAttributes_attributeNames,
    describeAccountAttributesResponse_accountAttributes,
    describeAccountAttributesResponse_httpStatus,

    -- ** CreateSnapshotCopyGrant
    createSnapshotCopyGrant_kmsKeyId,
    createSnapshotCopyGrant_tags,
    createSnapshotCopyGrant_snapshotCopyGrantName,
    createSnapshotCopyGrantResponse_snapshotCopyGrant,
    createSnapshotCopyGrantResponse_httpStatus,

    -- ** CopyClusterSnapshot
    copyClusterSnapshot_manualSnapshotRetentionPeriod,
    copyClusterSnapshot_sourceSnapshotClusterIdentifier,
    copyClusterSnapshot_sourceSnapshotIdentifier,
    copyClusterSnapshot_targetSnapshotIdentifier,
    copyClusterSnapshotResponse_snapshot,
    copyClusterSnapshotResponse_httpStatus,

    -- ** DescribeDataShares
    describeDataShares_marker,
    describeDataShares_maxRecords,
    describeDataShares_dataShareArn,
    describeDataSharesResponse_marker,
    describeDataSharesResponse_dataShares,
    describeDataSharesResponse_httpStatus,

    -- ** DeleteHsmClientCertificate
    deleteHsmClientCertificate_hsmClientCertificateIdentifier,

    -- ** ModifyAuthenticationProfile
    modifyAuthenticationProfile_authenticationProfileName,
    modifyAuthenticationProfile_authenticationProfileContent,
    modifyAuthenticationProfileResponse_authenticationProfileName,
    modifyAuthenticationProfileResponse_authenticationProfileContent,
    modifyAuthenticationProfileResponse_httpStatus,

    -- ** UpdatePartnerStatus
    updatePartnerStatus_statusMessage,
    updatePartnerStatus_accountId,
    updatePartnerStatus_clusterIdentifier,
    updatePartnerStatus_databaseName,
    updatePartnerStatus_partnerName,
    updatePartnerStatus_status,
    partnerIntegrationOutputMessage_partnerName,
    partnerIntegrationOutputMessage_databaseName,

    -- ** ModifyClusterSnapshotSchedule
    modifyClusterSnapshotSchedule_disassociateSchedule,
    modifyClusterSnapshotSchedule_scheduleIdentifier,
    modifyClusterSnapshotSchedule_clusterIdentifier,

    -- ** DeleteEndpointAccess
    deleteEndpointAccess_endpointName,
    endpointAccess_endpointName,
    endpointAccess_endpointCreateTime,
    endpointAccess_subnetGroupName,
    endpointAccess_address,
    endpointAccess_clusterIdentifier,
    endpointAccess_endpointStatus,
    endpointAccess_vpcSecurityGroups,
    endpointAccess_resourceOwner,
    endpointAccess_vpcEndpoint,
    endpointAccess_port,

    -- ** DeleteSnapshotCopyGrant
    deleteSnapshotCopyGrant_snapshotCopyGrantName,

    -- ** DescribeClusterVersions
    describeClusterVersions_clusterParameterGroupFamily,
    describeClusterVersions_marker,
    describeClusterVersions_maxRecords,
    describeClusterVersions_clusterVersion,
    describeClusterVersionsResponse_clusterVersions,
    describeClusterVersionsResponse_marker,
    describeClusterVersionsResponse_httpStatus,

    -- ** ModifyClusterSubnetGroup
    modifyClusterSubnetGroup_description,
    modifyClusterSubnetGroup_clusterSubnetGroupName,
    modifyClusterSubnetGroup_subnetIds,
    modifyClusterSubnetGroupResponse_clusterSubnetGroup,
    modifyClusterSubnetGroupResponse_httpStatus,

    -- ** DescribeUsageLimits
    describeUsageLimits_tagValues,
    describeUsageLimits_usageLimitId,
    describeUsageLimits_tagKeys,
    describeUsageLimits_clusterIdentifier,
    describeUsageLimits_featureType,
    describeUsageLimits_marker,
    describeUsageLimits_maxRecords,
    describeUsageLimitsResponse_usageLimits,
    describeUsageLimitsResponse_marker,
    describeUsageLimitsResponse_httpStatus,

    -- ** ModifySnapshotSchedule
    modifySnapshotSchedule_scheduleIdentifier,
    modifySnapshotSchedule_scheduleDefinitions,
    snapshotSchedule_associatedClusters,
    snapshotSchedule_nextInvocations,
    snapshotSchedule_scheduleDefinitions,
    snapshotSchedule_scheduleDescription,
    snapshotSchedule_scheduleIdentifier,
    snapshotSchedule_associatedClusterCount,
    snapshotSchedule_tags,

    -- ** RotateEncryptionKey
    rotateEncryptionKey_clusterIdentifier,
    rotateEncryptionKeyResponse_cluster,
    rotateEncryptionKeyResponse_httpStatus,

    -- ** DescribeSnapshotCopyGrants
    describeSnapshotCopyGrants_tagValues,
    describeSnapshotCopyGrants_tagKeys,
    describeSnapshotCopyGrants_marker,
    describeSnapshotCopyGrants_maxRecords,
    describeSnapshotCopyGrants_snapshotCopyGrantName,
    describeSnapshotCopyGrantsResponse_snapshotCopyGrants,
    describeSnapshotCopyGrantsResponse_marker,
    describeSnapshotCopyGrantsResponse_httpStatus,

    -- * Types

    -- ** AccountAttribute
    accountAttribute_attributeValues,
    accountAttribute_attributeName,

    -- ** AccountWithRestoreAccess
    accountWithRestoreAccess_accountAlias,
    accountWithRestoreAccess_accountId,

    -- ** AquaConfiguration
    aquaConfiguration_aquaConfigurationStatus,
    aquaConfiguration_aquaStatus,

    -- ** AttributeValueTarget
    attributeValueTarget_attributeValue,

    -- ** AuthenticationProfile
    authenticationProfile_authenticationProfileName,
    authenticationProfile_authenticationProfileContent,

    -- ** AvailabilityZone
    availabilityZone_name,
    availabilityZone_supportedPlatforms,

    -- ** Cluster
    cluster_aquaConfiguration,
    cluster_resizeInfo,
    cluster_restoreStatus,
    cluster_manualSnapshotRetentionPeriod,
    cluster_enhancedVpcRouting,
    cluster_clusterSnapshotCopyStatus,
    cluster_clusterAvailabilityStatus,
    cluster_clusterRevisionNumber,
    cluster_snapshotScheduleIdentifier,
    cluster_publiclyAccessible,
    cluster_masterUsername,
    cluster_maintenanceTrackName,
    cluster_expectedNextSnapshotScheduleTime,
    cluster_elasticResizeNumberOfNodeOptions,
    cluster_vpcId,
    cluster_clusterSecurityGroups,
    cluster_automatedSnapshotRetentionPeriod,
    cluster_snapshotScheduleState,
    cluster_dataTransferProgress,
    cluster_encrypted,
    cluster_clusterSubnetGroupName,
    cluster_expectedNextSnapshotScheduleTimeStatus,
    cluster_clusterIdentifier,
    cluster_deferredMaintenanceWindows,
    cluster_numberOfNodes,
    cluster_clusterPublicKey,
    cluster_preferredMaintenanceWindow,
    cluster_modifyStatus,
    cluster_clusterNamespaceArn,
    cluster_kmsKeyId,
    cluster_clusterParameterGroups,
    cluster_totalStorageCapacityInMegaBytes,
    cluster_availabilityZone,
    cluster_vpcSecurityGroups,
    cluster_hsmStatus,
    cluster_iamRoles,
    cluster_pendingActions,
    cluster_elasticIpStatus,
    cluster_clusterVersion,
    cluster_nodeType,
    cluster_nextMaintenanceWindowStartTime,
    cluster_clusterCreateTime,
    cluster_endpoint,
    cluster_allowVersionUpgrade,
    cluster_clusterStatus,
    cluster_pendingModifiedValues,
    cluster_tags,
    cluster_availabilityZoneRelocationStatus,
    cluster_clusterNodes,
    cluster_dbName,

    -- ** ClusterAssociatedToSchedule
    clusterAssociatedToSchedule_scheduleAssociationState,
    clusterAssociatedToSchedule_clusterIdentifier,

    -- ** ClusterDbRevision
    clusterDbRevision_databaseRevisionReleaseDate,
    clusterDbRevision_clusterIdentifier,
    clusterDbRevision_currentDatabaseRevision,
    clusterDbRevision_revisionTargets,

    -- ** ClusterIamRole
    clusterIamRole_iamRoleArn,
    clusterIamRole_applyStatus,

    -- ** ClusterNode
    clusterNode_nodeRole,
    clusterNode_privateIPAddress,
    clusterNode_publicIPAddress,

    -- ** ClusterParameterGroup
    clusterParameterGroup_parameterGroupFamily,
    clusterParameterGroup_description,
    clusterParameterGroup_tags,
    clusterParameterGroup_parameterGroupName,

    -- ** ClusterParameterGroupNameMessage
    clusterParameterGroupNameMessage_parameterGroupStatus,
    clusterParameterGroupNameMessage_parameterGroupName,

    -- ** ClusterParameterGroupStatus
    clusterParameterGroupStatus_clusterParameterStatusList,
    clusterParameterGroupStatus_parameterApplyStatus,
    clusterParameterGroupStatus_parameterGroupName,

    -- ** ClusterParameterStatus
    clusterParameterStatus_parameterApplyErrorDescription,
    clusterParameterStatus_parameterName,
    clusterParameterStatus_parameterApplyStatus,

    -- ** ClusterSecurityGroup
    clusterSecurityGroup_clusterSecurityGroupName,
    clusterSecurityGroup_iPRanges,
    clusterSecurityGroup_eC2SecurityGroups,
    clusterSecurityGroup_description,
    clusterSecurityGroup_tags,

    -- ** ClusterSecurityGroupMembership
    clusterSecurityGroupMembership_status,
    clusterSecurityGroupMembership_clusterSecurityGroupName,

    -- ** ClusterSnapshotCopyStatus
    clusterSnapshotCopyStatus_manualSnapshotRetentionPeriod,
    clusterSnapshotCopyStatus_retentionPeriod,
    clusterSnapshotCopyStatus_destinationRegion,
    clusterSnapshotCopyStatus_snapshotCopyGrantName,

    -- ** ClusterSubnetGroup
    clusterSubnetGroup_vpcId,
    clusterSubnetGroup_subnets,
    clusterSubnetGroup_clusterSubnetGroupName,
    clusterSubnetGroup_subnetGroupStatus,
    clusterSubnetGroup_description,
    clusterSubnetGroup_tags,

    -- ** ClusterVersion
    clusterVersion_clusterParameterGroupFamily,
    clusterVersion_clusterVersion,
    clusterVersion_description,

    -- ** DataShare
    dataShare_producerArn,
    dataShare_dataShareAssociations,
    dataShare_dataShareArn,
    dataShare_allowPubliclyAccessibleConsumers,

    -- ** DataShareAssociation
    dataShareAssociation_status,
    dataShareAssociation_consumerIdentifier,
    dataShareAssociation_createdDate,
    dataShareAssociation_statusChangeDate,

    -- ** DataTransferProgress
    dataTransferProgress_currentRateInMegaBytesPerSecond,
    dataTransferProgress_status,
    dataTransferProgress_estimatedTimeToCompletionInSeconds,
    dataTransferProgress_dataTransferredInMegaBytes,
    dataTransferProgress_totalDataInMegaBytes,
    dataTransferProgress_elapsedTimeInSeconds,

    -- ** DefaultClusterParameters
    defaultClusterParameters_marker,
    defaultClusterParameters_parameters,
    defaultClusterParameters_parameterGroupFamily,

    -- ** DeferredMaintenanceWindow
    deferredMaintenanceWindow_deferMaintenanceEndTime,
    deferredMaintenanceWindow_deferMaintenanceStartTime,
    deferredMaintenanceWindow_deferMaintenanceIdentifier,

    -- ** DeleteClusterSnapshotMessage
    deleteClusterSnapshotMessage_snapshotClusterIdentifier,
    deleteClusterSnapshotMessage_snapshotIdentifier,

    -- ** EC2SecurityGroup
    eC2SecurityGroup_status,
    eC2SecurityGroup_eC2SecurityGroupOwnerId,
    eC2SecurityGroup_eC2SecurityGroupName,
    eC2SecurityGroup_tags,

    -- ** ElasticIpStatus
    elasticIpStatus_status,
    elasticIpStatus_elasticIp,

    -- ** Endpoint
    endpoint_address,
    endpoint_vpcEndpoints,
    endpoint_port,

    -- ** EndpointAccess
    endpointAccess_endpointName,
    endpointAccess_endpointCreateTime,
    endpointAccess_subnetGroupName,
    endpointAccess_address,
    endpointAccess_clusterIdentifier,
    endpointAccess_endpointStatus,
    endpointAccess_vpcSecurityGroups,
    endpointAccess_resourceOwner,
    endpointAccess_vpcEndpoint,
    endpointAccess_port,

    -- ** EndpointAuthorization
    endpointAuthorization_status,
    endpointAuthorization_allowedAllVPCs,
    endpointAuthorization_endpointCount,
    endpointAuthorization_grantor,
    endpointAuthorization_clusterIdentifier,
    endpointAuthorization_grantee,
    endpointAuthorization_allowedVPCs,
    endpointAuthorization_clusterStatus,
    endpointAuthorization_authorizeTime,

    -- ** Event
    event_sourceType,
    event_severity,
    event_sourceIdentifier,
    event_date,
    event_eventCategories,
    event_message,
    event_eventId,

    -- ** EventCategoriesMap
    eventCategoriesMap_sourceType,
    eventCategoriesMap_events,

    -- ** EventInfoMap
    eventInfoMap_eventDescription,
    eventInfoMap_severity,
    eventInfoMap_eventCategories,
    eventInfoMap_eventId,

    -- ** EventSubscription
    eventSubscription_status,
    eventSubscription_customerAwsId,
    eventSubscription_custSubscriptionId,
    eventSubscription_snsTopicArn,
    eventSubscription_enabled,
    eventSubscription_sourceType,
    eventSubscription_severity,
    eventSubscription_subscriptionCreationTime,
    eventSubscription_eventCategoriesList,
    eventSubscription_tags,
    eventSubscription_sourceIdsList,

    -- ** HsmClientCertificate
    hsmClientCertificate_hsmClientCertificateIdentifier,
    hsmClientCertificate_hsmClientCertificatePublicKey,
    hsmClientCertificate_tags,

    -- ** HsmConfiguration
    hsmConfiguration_hsmConfigurationIdentifier,
    hsmConfiguration_hsmPartitionName,
    hsmConfiguration_description,
    hsmConfiguration_tags,
    hsmConfiguration_hsmIpAddress,

    -- ** HsmStatus
    hsmStatus_status,
    hsmStatus_hsmConfigurationIdentifier,
    hsmStatus_hsmClientCertificateIdentifier,

    -- ** IPRange
    iPRange_status,
    iPRange_cidrip,
    iPRange_tags,

    -- ** LoggingStatus
    loggingStatus_lastFailureTime,
    loggingStatus_lastSuccessfulDeliveryTime,
    loggingStatus_s3KeyPrefix,
    loggingStatus_bucketName,
    loggingStatus_loggingEnabled,
    loggingStatus_lastFailureMessage,

    -- ** MaintenanceTrack
    maintenanceTrack_databaseVersion,
    maintenanceTrack_maintenanceTrackName,
    maintenanceTrack_updateTargets,

    -- ** NetworkInterface
    networkInterface_networkInterfaceId,
    networkInterface_subnetId,
    networkInterface_availabilityZone,
    networkInterface_privateIpAddress,

    -- ** NodeConfigurationOption
    nodeConfigurationOption_mode,
    nodeConfigurationOption_numberOfNodes,
    nodeConfigurationOption_nodeType,
    nodeConfigurationOption_estimatedDiskUtilizationPercent,

    -- ** NodeConfigurationOptionsFilter
    nodeConfigurationOptionsFilter_values,
    nodeConfigurationOptionsFilter_operator,
    nodeConfigurationOptionsFilter_name,

    -- ** OrderableClusterOption
    orderableClusterOption_availabilityZones,
    orderableClusterOption_clusterType,
    orderableClusterOption_clusterVersion,
    orderableClusterOption_nodeType,

    -- ** Parameter
    parameter_applyType,
    parameter_parameterValue,
    parameter_minimumEngineVersion,
    parameter_source,
    parameter_isModifiable,
    parameter_dataType,
    parameter_allowedValues,
    parameter_parameterName,
    parameter_description,

    -- ** PartnerIntegrationInfo
    partnerIntegrationInfo_status,
    partnerIntegrationInfo_createdAt,
    partnerIntegrationInfo_partnerName,
    partnerIntegrationInfo_statusMessage,
    partnerIntegrationInfo_databaseName,
    partnerIntegrationInfo_updatedAt,

    -- ** PartnerIntegrationInputMessage
    partnerIntegrationInputMessage_accountId,
    partnerIntegrationInputMessage_clusterIdentifier,
    partnerIntegrationInputMessage_databaseName,
    partnerIntegrationInputMessage_partnerName,

    -- ** PartnerIntegrationOutputMessage
    partnerIntegrationOutputMessage_partnerName,
    partnerIntegrationOutputMessage_databaseName,

    -- ** PauseClusterMessage
    pauseClusterMessage_clusterIdentifier,

    -- ** PendingModifiedValues
    pendingModifiedValues_encryptionType,
    pendingModifiedValues_enhancedVpcRouting,
    pendingModifiedValues_masterUserPassword,
    pendingModifiedValues_publiclyAccessible,
    pendingModifiedValues_maintenanceTrackName,
    pendingModifiedValues_automatedSnapshotRetentionPeriod,
    pendingModifiedValues_clusterIdentifier,
    pendingModifiedValues_numberOfNodes,
    pendingModifiedValues_clusterType,
    pendingModifiedValues_clusterVersion,
    pendingModifiedValues_nodeType,

    -- ** RecurringCharge
    recurringCharge_recurringChargeFrequency,
    recurringCharge_recurringChargeAmount,

    -- ** ReservedNode
    reservedNode_reservedNodeOfferingType,
    reservedNode_state,
    reservedNode_currencyCode,
    reservedNode_startTime,
    reservedNode_nodeCount,
    reservedNode_reservedNodeId,
    reservedNode_reservedNodeOfferingId,
    reservedNode_recurringCharges,
    reservedNode_offeringType,
    reservedNode_usagePrice,
    reservedNode_nodeType,
    reservedNode_fixedPrice,
    reservedNode_duration,

    -- ** ReservedNodeOffering
    reservedNodeOffering_reservedNodeOfferingType,
    reservedNodeOffering_currencyCode,
    reservedNodeOffering_reservedNodeOfferingId,
    reservedNodeOffering_recurringCharges,
    reservedNodeOffering_offeringType,
    reservedNodeOffering_usagePrice,
    reservedNodeOffering_nodeType,
    reservedNodeOffering_fixedPrice,
    reservedNodeOffering_duration,

    -- ** ResizeClusterMessage
    resizeClusterMessage_numberOfNodes,
    resizeClusterMessage_classic,
    resizeClusterMessage_clusterType,
    resizeClusterMessage_nodeType,
    resizeClusterMessage_clusterIdentifier,

    -- ** ResizeInfo
    resizeInfo_allowCancelResize,
    resizeInfo_resizeType,

    -- ** ResizeProgressMessage
    resizeProgressMessage_importTablesNotStarted,
    resizeProgressMessage_status,
    resizeProgressMessage_estimatedTimeToCompletionInSeconds,
    resizeProgressMessage_avgResizeRateInMegaBytesPerSecond,
    resizeProgressMessage_targetNumberOfNodes,
    resizeProgressMessage_targetEncryptionType,
    resizeProgressMessage_targetNodeType,
    resizeProgressMessage_importTablesInProgress,
    resizeProgressMessage_resizeType,
    resizeProgressMessage_importTablesCompleted,
    resizeProgressMessage_progressInMegaBytes,
    resizeProgressMessage_dataTransferProgressPercent,
    resizeProgressMessage_totalResizeDataInMegaBytes,
    resizeProgressMessage_targetClusterType,
    resizeProgressMessage_message,
    resizeProgressMessage_elapsedTimeInSeconds,

    -- ** RestoreStatus
    restoreStatus_status,
    restoreStatus_estimatedTimeToCompletionInSeconds,
    restoreStatus_currentRestoreRateInMegaBytesPerSecond,
    restoreStatus_progressInMegaBytes,
    restoreStatus_elapsedTimeInSeconds,
    restoreStatus_snapshotSizeInMegaBytes,

    -- ** ResumeClusterMessage
    resumeClusterMessage_clusterIdentifier,

    -- ** RevisionTarget
    revisionTarget_databaseRevisionReleaseDate,
    revisionTarget_databaseRevision,
    revisionTarget_description,

    -- ** ScheduledAction
    scheduledAction_state,
    scheduledAction_targetAction,
    scheduledAction_startTime,
    scheduledAction_schedule,
    scheduledAction_scheduledActionName,
    scheduledAction_scheduledActionDescription,
    scheduledAction_nextInvocations,
    scheduledAction_endTime,
    scheduledAction_iamRole,

    -- ** ScheduledActionFilter
    scheduledActionFilter_name,
    scheduledActionFilter_values,

    -- ** ScheduledActionType
    scheduledActionType_resizeCluster,
    scheduledActionType_resumeCluster,
    scheduledActionType_pauseCluster,

    -- ** Snapshot
    snapshot_status,
    snapshot_restorableNodeTypes,
    snapshot_accountsWithRestoreAccess,
    snapshot_manualSnapshotRetentionPeriod,
    snapshot_enhancedVpcRouting,
    snapshot_snapshotIdentifier,
    snapshot_engineFullVersion,
    snapshot_encryptedWithHSM,
    snapshot_masterUsername,
    snapshot_sourceRegion,
    snapshot_maintenanceTrackName,
    snapshot_snapshotRetentionStartTime,
    snapshot_manualSnapshotRemainingDays,
    snapshot_vpcId,
    snapshot_backupProgressInMegaBytes,
    snapshot_encrypted,
    snapshot_clusterIdentifier,
    snapshot_numberOfNodes,
    snapshot_snapshotType,
    snapshot_kmsKeyId,
    snapshot_availabilityZone,
    snapshot_currentBackupRateInMegaBytesPerSecond,
    snapshot_snapshotCreateTime,
    snapshot_clusterVersion,
    snapshot_ownerAccount,
    snapshot_nodeType,
    snapshot_elapsedTimeInSeconds,
    snapshot_clusterCreateTime,
    snapshot_estimatedSecondsToCompletion,
    snapshot_actualIncrementalBackupSizeInMegaBytes,
    snapshot_tags,
    snapshot_port,
    snapshot_totalBackupSizeInMegaBytes,
    snapshot_dbName,

    -- ** SnapshotCopyGrant
    snapshotCopyGrant_kmsKeyId,
    snapshotCopyGrant_snapshotCopyGrantName,
    snapshotCopyGrant_tags,

    -- ** SnapshotErrorMessage
    snapshotErrorMessage_failureReason,
    snapshotErrorMessage_snapshotIdentifier,
    snapshotErrorMessage_snapshotClusterIdentifier,
    snapshotErrorMessage_failureCode,

    -- ** SnapshotSchedule
    snapshotSchedule_associatedClusters,
    snapshotSchedule_nextInvocations,
    snapshotSchedule_scheduleDefinitions,
    snapshotSchedule_scheduleDescription,
    snapshotSchedule_scheduleIdentifier,
    snapshotSchedule_associatedClusterCount,
    snapshotSchedule_tags,

    -- ** SnapshotSortingEntity
    snapshotSortingEntity_sortOrder,
    snapshotSortingEntity_attribute,

    -- ** Subnet
    subnet_subnetStatus,
    subnet_subnetIdentifier,
    subnet_subnetAvailabilityZone,

    -- ** SupportedOperation
    supportedOperation_operationName,

    -- ** SupportedPlatform
    supportedPlatform_name,

    -- ** TableRestoreStatus
    tableRestoreStatus_status,
    tableRestoreStatus_targetSchemaName,
    tableRestoreStatus_snapshotIdentifier,
    tableRestoreStatus_sourceDatabaseName,
    tableRestoreStatus_tableRestoreRequestId,
    tableRestoreStatus_newTableName,
    tableRestoreStatus_targetDatabaseName,
    tableRestoreStatus_sourceSchemaName,
    tableRestoreStatus_clusterIdentifier,
    tableRestoreStatus_requestTime,
    tableRestoreStatus_sourceTableName,
    tableRestoreStatus_totalDataInMegaBytes,
    tableRestoreStatus_progressInMegaBytes,
    tableRestoreStatus_message,

    -- ** Tag
    tag_value,
    tag_key,

    -- ** TaggedResource
    taggedResource_tag,
    taggedResource_resourceType,
    taggedResource_resourceName,

    -- ** UpdateTarget
    updateTarget_databaseVersion,
    updateTarget_maintenanceTrackName,
    updateTarget_supportedOperations,

    -- ** UsageLimit
    usageLimit_amount,
    usageLimit_limitType,
    usageLimit_usageLimitId,
    usageLimit_period,
    usageLimit_clusterIdentifier,
    usageLimit_breachAction,
    usageLimit_featureType,
    usageLimit_tags,

    -- ** VpcEndpoint
    vpcEndpoint_vpcId,
    vpcEndpoint_networkInterfaces,
    vpcEndpoint_vpcEndpointId,

    -- ** VpcSecurityGroupMembership
    vpcSecurityGroupMembership_status,
    vpcSecurityGroupMembership_vpcSecurityGroupId,
  )
where

import Network.AWS.Redshift.AcceptReservedNodeExchange
import Network.AWS.Redshift.AddPartner
import Network.AWS.Redshift.AssociateDataShareConsumer
import Network.AWS.Redshift.AuthorizeClusterSecurityGroupIngress
import Network.AWS.Redshift.AuthorizeDataShare
import Network.AWS.Redshift.AuthorizeEndpointAccess
import Network.AWS.Redshift.AuthorizeSnapshotAccess
import Network.AWS.Redshift.BatchDeleteClusterSnapshots
import Network.AWS.Redshift.BatchModifyClusterSnapshots
import Network.AWS.Redshift.CancelResize
import Network.AWS.Redshift.CopyClusterSnapshot
import Network.AWS.Redshift.CreateAuthenticationProfile
import Network.AWS.Redshift.CreateCluster
import Network.AWS.Redshift.CreateClusterParameterGroup
import Network.AWS.Redshift.CreateClusterSecurityGroup
import Network.AWS.Redshift.CreateClusterSnapshot
import Network.AWS.Redshift.CreateClusterSubnetGroup
import Network.AWS.Redshift.CreateEndpointAccess
import Network.AWS.Redshift.CreateEventSubscription
import Network.AWS.Redshift.CreateHsmClientCertificate
import Network.AWS.Redshift.CreateHsmConfiguration
import Network.AWS.Redshift.CreateScheduledAction
import Network.AWS.Redshift.CreateSnapshotCopyGrant
import Network.AWS.Redshift.CreateSnapshotSchedule
import Network.AWS.Redshift.CreateTags
import Network.AWS.Redshift.CreateUsageLimit
import Network.AWS.Redshift.DeauthorizeDataShare
import Network.AWS.Redshift.DeleteAuthenticationProfile
import Network.AWS.Redshift.DeleteCluster
import Network.AWS.Redshift.DeleteClusterParameterGroup
import Network.AWS.Redshift.DeleteClusterSecurityGroup
import Network.AWS.Redshift.DeleteClusterSnapshot
import Network.AWS.Redshift.DeleteClusterSubnetGroup
import Network.AWS.Redshift.DeleteEndpointAccess
import Network.AWS.Redshift.DeleteEventSubscription
import Network.AWS.Redshift.DeleteHsmClientCertificate
import Network.AWS.Redshift.DeleteHsmConfiguration
import Network.AWS.Redshift.DeletePartner
import Network.AWS.Redshift.DeleteScheduledAction
import Network.AWS.Redshift.DeleteSnapshotCopyGrant
import Network.AWS.Redshift.DeleteSnapshotSchedule
import Network.AWS.Redshift.DeleteTags
import Network.AWS.Redshift.DeleteUsageLimit
import Network.AWS.Redshift.DescribeAccountAttributes
import Network.AWS.Redshift.DescribeAuthenticationProfiles
import Network.AWS.Redshift.DescribeClusterDbRevisions
import Network.AWS.Redshift.DescribeClusterParameterGroups
import Network.AWS.Redshift.DescribeClusterParameters
import Network.AWS.Redshift.DescribeClusterSecurityGroups
import Network.AWS.Redshift.DescribeClusterSnapshots
import Network.AWS.Redshift.DescribeClusterSubnetGroups
import Network.AWS.Redshift.DescribeClusterTracks
import Network.AWS.Redshift.DescribeClusterVersions
import Network.AWS.Redshift.DescribeClusters
import Network.AWS.Redshift.DescribeDataShares
import Network.AWS.Redshift.DescribeDataSharesForConsumer
import Network.AWS.Redshift.DescribeDataSharesForProducer
import Network.AWS.Redshift.DescribeDefaultClusterParameters
import Network.AWS.Redshift.DescribeEndpointAccess
import Network.AWS.Redshift.DescribeEndpointAuthorization
import Network.AWS.Redshift.DescribeEventCategories
import Network.AWS.Redshift.DescribeEventSubscriptions
import Network.AWS.Redshift.DescribeEvents
import Network.AWS.Redshift.DescribeHsmClientCertificates
import Network.AWS.Redshift.DescribeHsmConfigurations
import Network.AWS.Redshift.DescribeLoggingStatus
import Network.AWS.Redshift.DescribeNodeConfigurationOptions
import Network.AWS.Redshift.DescribeOrderableClusterOptions
import Network.AWS.Redshift.DescribePartners
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
import Network.AWS.Redshift.DisassociateDataShareConsumer
import Network.AWS.Redshift.EnableLogging
import Network.AWS.Redshift.EnableSnapshotCopy
import Network.AWS.Redshift.GetClusterCredentials
import Network.AWS.Redshift.GetReservedNodeExchangeOfferings
import Network.AWS.Redshift.ModifyAquaConfiguration
import Network.AWS.Redshift.ModifyAuthenticationProfile
import Network.AWS.Redshift.ModifyCluster
import Network.AWS.Redshift.ModifyClusterDbRevision
import Network.AWS.Redshift.ModifyClusterIamRoles
import Network.AWS.Redshift.ModifyClusterMaintenance
import Network.AWS.Redshift.ModifyClusterParameterGroup
import Network.AWS.Redshift.ModifyClusterSnapshot
import Network.AWS.Redshift.ModifyClusterSnapshotSchedule
import Network.AWS.Redshift.ModifyClusterSubnetGroup
import Network.AWS.Redshift.ModifyEndpointAccess
import Network.AWS.Redshift.ModifyEventSubscription
import Network.AWS.Redshift.ModifyScheduledAction
import Network.AWS.Redshift.ModifySnapshotCopyRetentionPeriod
import Network.AWS.Redshift.ModifySnapshotSchedule
import Network.AWS.Redshift.ModifyUsageLimit
import Network.AWS.Redshift.PauseCluster
import Network.AWS.Redshift.PurchaseReservedNodeOffering
import Network.AWS.Redshift.RebootCluster
import Network.AWS.Redshift.RejectDataShare
import Network.AWS.Redshift.ResetClusterParameterGroup
import Network.AWS.Redshift.ResizeCluster
import Network.AWS.Redshift.RestoreFromClusterSnapshot
import Network.AWS.Redshift.RestoreTableFromClusterSnapshot
import Network.AWS.Redshift.ResumeCluster
import Network.AWS.Redshift.RevokeClusterSecurityGroupIngress
import Network.AWS.Redshift.RevokeEndpointAccess
import Network.AWS.Redshift.RevokeSnapshotAccess
import Network.AWS.Redshift.RotateEncryptionKey
import Network.AWS.Redshift.Types.AccountAttribute
import Network.AWS.Redshift.Types.AccountWithRestoreAccess
import Network.AWS.Redshift.Types.AquaConfiguration
import Network.AWS.Redshift.Types.AttributeValueTarget
import Network.AWS.Redshift.Types.AuthenticationProfile
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
import Network.AWS.Redshift.Types.DataShare
import Network.AWS.Redshift.Types.DataShareAssociation
import Network.AWS.Redshift.Types.DataTransferProgress
import Network.AWS.Redshift.Types.DefaultClusterParameters
import Network.AWS.Redshift.Types.DeferredMaintenanceWindow
import Network.AWS.Redshift.Types.DeleteClusterSnapshotMessage
import Network.AWS.Redshift.Types.EC2SecurityGroup
import Network.AWS.Redshift.Types.ElasticIpStatus
import Network.AWS.Redshift.Types.Endpoint
import Network.AWS.Redshift.Types.EndpointAccess
import Network.AWS.Redshift.Types.EndpointAuthorization
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
import Network.AWS.Redshift.Types.NetworkInterface
import Network.AWS.Redshift.Types.NodeConfigurationOption
import Network.AWS.Redshift.Types.NodeConfigurationOptionsFilter
import Network.AWS.Redshift.Types.OrderableClusterOption
import Network.AWS.Redshift.Types.Parameter
import Network.AWS.Redshift.Types.PartnerIntegrationInfo
import Network.AWS.Redshift.Types.PartnerIntegrationInputMessage
import Network.AWS.Redshift.Types.PartnerIntegrationOutputMessage
import Network.AWS.Redshift.Types.PauseClusterMessage
import Network.AWS.Redshift.Types.PendingModifiedValues
import Network.AWS.Redshift.Types.RecurringCharge
import Network.AWS.Redshift.Types.ReservedNode
import Network.AWS.Redshift.Types.ReservedNodeOffering
import Network.AWS.Redshift.Types.ResizeClusterMessage
import Network.AWS.Redshift.Types.ResizeInfo
import Network.AWS.Redshift.Types.ResizeProgressMessage
import Network.AWS.Redshift.Types.RestoreStatus
import Network.AWS.Redshift.Types.ResumeClusterMessage
import Network.AWS.Redshift.Types.RevisionTarget
import Network.AWS.Redshift.Types.ScheduledAction
import Network.AWS.Redshift.Types.ScheduledActionFilter
import Network.AWS.Redshift.Types.ScheduledActionType
import Network.AWS.Redshift.Types.Snapshot
import Network.AWS.Redshift.Types.SnapshotCopyGrant
import Network.AWS.Redshift.Types.SnapshotErrorMessage
import Network.AWS.Redshift.Types.SnapshotSchedule
import Network.AWS.Redshift.Types.SnapshotSortingEntity
import Network.AWS.Redshift.Types.Subnet
import Network.AWS.Redshift.Types.SupportedOperation
import Network.AWS.Redshift.Types.SupportedPlatform
import Network.AWS.Redshift.Types.TableRestoreStatus
import Network.AWS.Redshift.Types.Tag
import Network.AWS.Redshift.Types.TaggedResource
import Network.AWS.Redshift.Types.UpdateTarget
import Network.AWS.Redshift.Types.UsageLimit
import Network.AWS.Redshift.Types.VpcEndpoint
import Network.AWS.Redshift.Types.VpcSecurityGroupMembership
import Network.AWS.Redshift.UpdatePartnerStatus
