{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Redshift.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Lens
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

import Amazonka.Redshift.AcceptReservedNodeExchange
import Amazonka.Redshift.AddPartner
import Amazonka.Redshift.AssociateDataShareConsumer
import Amazonka.Redshift.AuthorizeClusterSecurityGroupIngress
import Amazonka.Redshift.AuthorizeDataShare
import Amazonka.Redshift.AuthorizeEndpointAccess
import Amazonka.Redshift.AuthorizeSnapshotAccess
import Amazonka.Redshift.BatchDeleteClusterSnapshots
import Amazonka.Redshift.BatchModifyClusterSnapshots
import Amazonka.Redshift.CancelResize
import Amazonka.Redshift.CopyClusterSnapshot
import Amazonka.Redshift.CreateAuthenticationProfile
import Amazonka.Redshift.CreateCluster
import Amazonka.Redshift.CreateClusterParameterGroup
import Amazonka.Redshift.CreateClusterSecurityGroup
import Amazonka.Redshift.CreateClusterSnapshot
import Amazonka.Redshift.CreateClusterSubnetGroup
import Amazonka.Redshift.CreateEndpointAccess
import Amazonka.Redshift.CreateEventSubscription
import Amazonka.Redshift.CreateHsmClientCertificate
import Amazonka.Redshift.CreateHsmConfiguration
import Amazonka.Redshift.CreateScheduledAction
import Amazonka.Redshift.CreateSnapshotCopyGrant
import Amazonka.Redshift.CreateSnapshotSchedule
import Amazonka.Redshift.CreateTags
import Amazonka.Redshift.CreateUsageLimit
import Amazonka.Redshift.DeauthorizeDataShare
import Amazonka.Redshift.DeleteAuthenticationProfile
import Amazonka.Redshift.DeleteCluster
import Amazonka.Redshift.DeleteClusterParameterGroup
import Amazonka.Redshift.DeleteClusterSecurityGroup
import Amazonka.Redshift.DeleteClusterSnapshot
import Amazonka.Redshift.DeleteClusterSubnetGroup
import Amazonka.Redshift.DeleteEndpointAccess
import Amazonka.Redshift.DeleteEventSubscription
import Amazonka.Redshift.DeleteHsmClientCertificate
import Amazonka.Redshift.DeleteHsmConfiguration
import Amazonka.Redshift.DeletePartner
import Amazonka.Redshift.DeleteScheduledAction
import Amazonka.Redshift.DeleteSnapshotCopyGrant
import Amazonka.Redshift.DeleteSnapshotSchedule
import Amazonka.Redshift.DeleteTags
import Amazonka.Redshift.DeleteUsageLimit
import Amazonka.Redshift.DescribeAccountAttributes
import Amazonka.Redshift.DescribeAuthenticationProfiles
import Amazonka.Redshift.DescribeClusterDbRevisions
import Amazonka.Redshift.DescribeClusterParameterGroups
import Amazonka.Redshift.DescribeClusterParameters
import Amazonka.Redshift.DescribeClusterSecurityGroups
import Amazonka.Redshift.DescribeClusterSnapshots
import Amazonka.Redshift.DescribeClusterSubnetGroups
import Amazonka.Redshift.DescribeClusterTracks
import Amazonka.Redshift.DescribeClusterVersions
import Amazonka.Redshift.DescribeClusters
import Amazonka.Redshift.DescribeDataShares
import Amazonka.Redshift.DescribeDataSharesForConsumer
import Amazonka.Redshift.DescribeDataSharesForProducer
import Amazonka.Redshift.DescribeDefaultClusterParameters
import Amazonka.Redshift.DescribeEndpointAccess
import Amazonka.Redshift.DescribeEndpointAuthorization
import Amazonka.Redshift.DescribeEventCategories
import Amazonka.Redshift.DescribeEventSubscriptions
import Amazonka.Redshift.DescribeEvents
import Amazonka.Redshift.DescribeHsmClientCertificates
import Amazonka.Redshift.DescribeHsmConfigurations
import Amazonka.Redshift.DescribeLoggingStatus
import Amazonka.Redshift.DescribeNodeConfigurationOptions
import Amazonka.Redshift.DescribeOrderableClusterOptions
import Amazonka.Redshift.DescribePartners
import Amazonka.Redshift.DescribeReservedNodeOfferings
import Amazonka.Redshift.DescribeReservedNodes
import Amazonka.Redshift.DescribeResize
import Amazonka.Redshift.DescribeScheduledActions
import Amazonka.Redshift.DescribeSnapshotCopyGrants
import Amazonka.Redshift.DescribeSnapshotSchedules
import Amazonka.Redshift.DescribeStorage
import Amazonka.Redshift.DescribeTableRestoreStatus
import Amazonka.Redshift.DescribeTags
import Amazonka.Redshift.DescribeUsageLimits
import Amazonka.Redshift.DisableLogging
import Amazonka.Redshift.DisableSnapshotCopy
import Amazonka.Redshift.DisassociateDataShareConsumer
import Amazonka.Redshift.EnableLogging
import Amazonka.Redshift.EnableSnapshotCopy
import Amazonka.Redshift.GetClusterCredentials
import Amazonka.Redshift.GetReservedNodeExchangeOfferings
import Amazonka.Redshift.ModifyAquaConfiguration
import Amazonka.Redshift.ModifyAuthenticationProfile
import Amazonka.Redshift.ModifyCluster
import Amazonka.Redshift.ModifyClusterDbRevision
import Amazonka.Redshift.ModifyClusterIamRoles
import Amazonka.Redshift.ModifyClusterMaintenance
import Amazonka.Redshift.ModifyClusterParameterGroup
import Amazonka.Redshift.ModifyClusterSnapshot
import Amazonka.Redshift.ModifyClusterSnapshotSchedule
import Amazonka.Redshift.ModifyClusterSubnetGroup
import Amazonka.Redshift.ModifyEndpointAccess
import Amazonka.Redshift.ModifyEventSubscription
import Amazonka.Redshift.ModifyScheduledAction
import Amazonka.Redshift.ModifySnapshotCopyRetentionPeriod
import Amazonka.Redshift.ModifySnapshotSchedule
import Amazonka.Redshift.ModifyUsageLimit
import Amazonka.Redshift.PauseCluster
import Amazonka.Redshift.PurchaseReservedNodeOffering
import Amazonka.Redshift.RebootCluster
import Amazonka.Redshift.RejectDataShare
import Amazonka.Redshift.ResetClusterParameterGroup
import Amazonka.Redshift.ResizeCluster
import Amazonka.Redshift.RestoreFromClusterSnapshot
import Amazonka.Redshift.RestoreTableFromClusterSnapshot
import Amazonka.Redshift.ResumeCluster
import Amazonka.Redshift.RevokeClusterSecurityGroupIngress
import Amazonka.Redshift.RevokeEndpointAccess
import Amazonka.Redshift.RevokeSnapshotAccess
import Amazonka.Redshift.RotateEncryptionKey
import Amazonka.Redshift.Types.AccountAttribute
import Amazonka.Redshift.Types.AccountWithRestoreAccess
import Amazonka.Redshift.Types.AquaConfiguration
import Amazonka.Redshift.Types.AttributeValueTarget
import Amazonka.Redshift.Types.AuthenticationProfile
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
import Amazonka.Redshift.Types.LoggingStatus
import Amazonka.Redshift.Types.MaintenanceTrack
import Amazonka.Redshift.Types.NetworkInterface
import Amazonka.Redshift.Types.NodeConfigurationOption
import Amazonka.Redshift.Types.NodeConfigurationOptionsFilter
import Amazonka.Redshift.Types.OrderableClusterOption
import Amazonka.Redshift.Types.Parameter
import Amazonka.Redshift.Types.PartnerIntegrationInfo
import Amazonka.Redshift.Types.PartnerIntegrationInputMessage
import Amazonka.Redshift.Types.PartnerIntegrationOutputMessage
import Amazonka.Redshift.Types.PauseClusterMessage
import Amazonka.Redshift.Types.PendingModifiedValues
import Amazonka.Redshift.Types.RecurringCharge
import Amazonka.Redshift.Types.ReservedNode
import Amazonka.Redshift.Types.ReservedNodeOffering
import Amazonka.Redshift.Types.ResizeClusterMessage
import Amazonka.Redshift.Types.ResizeInfo
import Amazonka.Redshift.Types.ResizeProgressMessage
import Amazonka.Redshift.Types.RestoreStatus
import Amazonka.Redshift.Types.ResumeClusterMessage
import Amazonka.Redshift.Types.RevisionTarget
import Amazonka.Redshift.Types.ScheduledAction
import Amazonka.Redshift.Types.ScheduledActionFilter
import Amazonka.Redshift.Types.ScheduledActionType
import Amazonka.Redshift.Types.Snapshot
import Amazonka.Redshift.Types.SnapshotCopyGrant
import Amazonka.Redshift.Types.SnapshotErrorMessage
import Amazonka.Redshift.Types.SnapshotSchedule
import Amazonka.Redshift.Types.SnapshotSortingEntity
import Amazonka.Redshift.Types.Subnet
import Amazonka.Redshift.Types.SupportedOperation
import Amazonka.Redshift.Types.SupportedPlatform
import Amazonka.Redshift.Types.TableRestoreStatus
import Amazonka.Redshift.Types.Tag
import Amazonka.Redshift.Types.TaggedResource
import Amazonka.Redshift.Types.UpdateTarget
import Amazonka.Redshift.Types.UsageLimit
import Amazonka.Redshift.Types.VpcEndpoint
import Amazonka.Redshift.Types.VpcSecurityGroupMembership
import Amazonka.Redshift.UpdatePartnerStatus
