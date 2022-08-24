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

    -- ** AcceptReservedNodeExchange
    acceptReservedNodeExchange_reservedNodeId,
    acceptReservedNodeExchange_targetReservedNodeOfferingId,
    acceptReservedNodeExchangeResponse_exchangedReservedNode,
    acceptReservedNodeExchangeResponse_httpStatus,

    -- ** AddPartner
    addPartner_accountId,
    addPartner_clusterIdentifier,
    addPartner_databaseName,
    addPartner_partnerName,
    partnerIntegrationOutputMessage_databaseName,
    partnerIntegrationOutputMessage_partnerName,

    -- ** AssociateDataShareConsumer
    associateDataShareConsumer_associateEntireAccount,
    associateDataShareConsumer_consumerArn,
    associateDataShareConsumer_consumerRegion,
    associateDataShareConsumer_dataShareArn,
    dataShare_dataShareAssociations,
    dataShare_producerArn,
    dataShare_allowPubliclyAccessibleConsumers,
    dataShare_dataShareArn,
    dataShare_managedBy,

    -- ** AuthorizeClusterSecurityGroupIngress
    authorizeClusterSecurityGroupIngress_eC2SecurityGroupOwnerId,
    authorizeClusterSecurityGroupIngress_eC2SecurityGroupName,
    authorizeClusterSecurityGroupIngress_cidrip,
    authorizeClusterSecurityGroupIngress_clusterSecurityGroupName,
    authorizeClusterSecurityGroupIngressResponse_clusterSecurityGroup,
    authorizeClusterSecurityGroupIngressResponse_httpStatus,

    -- ** AuthorizeDataShare
    authorizeDataShare_dataShareArn,
    authorizeDataShare_consumerIdentifier,
    dataShare_dataShareAssociations,
    dataShare_producerArn,
    dataShare_allowPubliclyAccessibleConsumers,
    dataShare_dataShareArn,
    dataShare_managedBy,

    -- ** AuthorizeEndpointAccess
    authorizeEndpointAccess_clusterIdentifier,
    authorizeEndpointAccess_vpcIds,
    authorizeEndpointAccess_account,
    endpointAuthorization_clusterIdentifier,
    endpointAuthorization_endpointCount,
    endpointAuthorization_allowedAllVPCs,
    endpointAuthorization_status,
    endpointAuthorization_authorizeTime,
    endpointAuthorization_clusterStatus,
    endpointAuthorization_grantor,
    endpointAuthorization_grantee,
    endpointAuthorization_allowedVPCs,

    -- ** AuthorizeSnapshotAccess
    authorizeSnapshotAccess_snapshotArn,
    authorizeSnapshotAccess_snapshotIdentifier,
    authorizeSnapshotAccess_snapshotClusterIdentifier,
    authorizeSnapshotAccess_accountWithRestoreAccess,
    authorizeSnapshotAccessResponse_snapshot,
    authorizeSnapshotAccessResponse_httpStatus,

    -- ** BatchDeleteClusterSnapshots
    batchDeleteClusterSnapshots_identifiers,
    batchDeleteClusterSnapshotsResponse_errors,
    batchDeleteClusterSnapshotsResponse_resources,
    batchDeleteClusterSnapshotsResponse_httpStatus,

    -- ** BatchModifyClusterSnapshots
    batchModifyClusterSnapshots_manualSnapshotRetentionPeriod,
    batchModifyClusterSnapshots_force,
    batchModifyClusterSnapshots_snapshotIdentifierList,
    batchModifyClusterSnapshotsResponse_errors,
    batchModifyClusterSnapshotsResponse_resources,
    batchModifyClusterSnapshotsResponse_httpStatus,

    -- ** CancelResize
    cancelResize_clusterIdentifier,
    resizeProgressMessage_message,
    resizeProgressMessage_targetEncryptionType,
    resizeProgressMessage_avgResizeRateInMegaBytesPerSecond,
    resizeProgressMessage_targetClusterType,
    resizeProgressMessage_dataTransferProgressPercent,
    resizeProgressMessage_targetNumberOfNodes,
    resizeProgressMessage_totalResizeDataInMegaBytes,
    resizeProgressMessage_importTablesNotStarted,
    resizeProgressMessage_status,
    resizeProgressMessage_elapsedTimeInSeconds,
    resizeProgressMessage_importTablesCompleted,
    resizeProgressMessage_progressInMegaBytes,
    resizeProgressMessage_importTablesInProgress,
    resizeProgressMessage_estimatedTimeToCompletionInSeconds,
    resizeProgressMessage_targetNodeType,
    resizeProgressMessage_resizeType,

    -- ** CopyClusterSnapshot
    copyClusterSnapshot_sourceSnapshotClusterIdentifier,
    copyClusterSnapshot_manualSnapshotRetentionPeriod,
    copyClusterSnapshot_sourceSnapshotIdentifier,
    copyClusterSnapshot_targetSnapshotIdentifier,
    copyClusterSnapshotResponse_snapshot,
    copyClusterSnapshotResponse_httpStatus,

    -- ** CreateAuthenticationProfile
    createAuthenticationProfile_authenticationProfileName,
    createAuthenticationProfile_authenticationProfileContent,
    createAuthenticationProfileResponse_authenticationProfileName,
    createAuthenticationProfileResponse_authenticationProfileContent,
    createAuthenticationProfileResponse_httpStatus,

    -- ** CreateCluster
    createCluster_tags,
    createCluster_port,
    createCluster_vpcSecurityGroupIds,
    createCluster_elasticIp,
    createCluster_manualSnapshotRetentionPeriod,
    createCluster_additionalInfo,
    createCluster_allowVersionUpgrade,
    createCluster_clusterSubnetGroupName,
    createCluster_snapshotScheduleIdentifier,
    createCluster_aquaConfigurationStatus,
    createCluster_clusterVersion,
    createCluster_loadSampleData,
    createCluster_maintenanceTrackName,
    createCluster_iamRoles,
    createCluster_hsmClientCertificateIdentifier,
    createCluster_availabilityZone,
    createCluster_publiclyAccessible,
    createCluster_clusterParameterGroupName,
    createCluster_encrypted,
    createCluster_numberOfNodes,
    createCluster_kmsKeyId,
    createCluster_availabilityZoneRelocation,
    createCluster_defaultIamRoleArn,
    createCluster_enhancedVpcRouting,
    createCluster_preferredMaintenanceWindow,
    createCluster_clusterType,
    createCluster_clusterSecurityGroups,
    createCluster_automatedSnapshotRetentionPeriod,
    createCluster_hsmConfigurationIdentifier,
    createCluster_dbName,
    createCluster_clusterIdentifier,
    createCluster_nodeType,
    createCluster_masterUsername,
    createCluster_masterUserPassword,
    createClusterResponse_cluster,
    createClusterResponse_httpStatus,

    -- ** CreateClusterParameterGroup
    createClusterParameterGroup_tags,
    createClusterParameterGroup_parameterGroupName,
    createClusterParameterGroup_parameterGroupFamily,
    createClusterParameterGroup_description,
    createClusterParameterGroupResponse_clusterParameterGroup,
    createClusterParameterGroupResponse_httpStatus,

    -- ** CreateClusterSecurityGroup
    createClusterSecurityGroup_tags,
    createClusterSecurityGroup_clusterSecurityGroupName,
    createClusterSecurityGroup_description,
    createClusterSecurityGroupResponse_clusterSecurityGroup,
    createClusterSecurityGroupResponse_httpStatus,

    -- ** CreateClusterSnapshot
    createClusterSnapshot_tags,
    createClusterSnapshot_manualSnapshotRetentionPeriod,
    createClusterSnapshot_snapshotIdentifier,
    createClusterSnapshot_clusterIdentifier,
    createClusterSnapshotResponse_snapshot,
    createClusterSnapshotResponse_httpStatus,

    -- ** CreateClusterSubnetGroup
    createClusterSubnetGroup_tags,
    createClusterSubnetGroup_clusterSubnetGroupName,
    createClusterSubnetGroup_description,
    createClusterSubnetGroup_subnetIds,
    createClusterSubnetGroupResponse_clusterSubnetGroup,
    createClusterSubnetGroupResponse_httpStatus,

    -- ** CreateEndpointAccess
    createEndpointAccess_clusterIdentifier,
    createEndpointAccess_vpcSecurityGroupIds,
    createEndpointAccess_resourceOwner,
    createEndpointAccess_endpointName,
    createEndpointAccess_subnetGroupName,
    endpointAccess_port,
    endpointAccess_subnetGroupName,
    endpointAccess_clusterIdentifier,
    endpointAccess_endpointName,
    endpointAccess_resourceOwner,
    endpointAccess_address,
    endpointAccess_endpointStatus,
    endpointAccess_vpcEndpoint,
    endpointAccess_endpointCreateTime,
    endpointAccess_vpcSecurityGroups,

    -- ** CreateEventSubscription
    createEventSubscription_tags,
    createEventSubscription_severity,
    createEventSubscription_sourceIds,
    createEventSubscription_sourceType,
    createEventSubscription_enabled,
    createEventSubscription_eventCategories,
    createEventSubscription_subscriptionName,
    createEventSubscription_snsTopicArn,
    createEventSubscriptionResponse_eventSubscription,
    createEventSubscriptionResponse_httpStatus,

    -- ** CreateHsmClientCertificate
    createHsmClientCertificate_tags,
    createHsmClientCertificate_hsmClientCertificateIdentifier,
    createHsmClientCertificateResponse_hsmClientCertificate,
    createHsmClientCertificateResponse_httpStatus,

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

    -- ** CreateScheduledAction
    createScheduledAction_enable,
    createScheduledAction_endTime,
    createScheduledAction_scheduledActionDescription,
    createScheduledAction_startTime,
    createScheduledAction_scheduledActionName,
    createScheduledAction_targetAction,
    createScheduledAction_schedule,
    createScheduledAction_iamRole,
    scheduledAction_schedule,
    scheduledAction_targetAction,
    scheduledAction_iamRole,
    scheduledAction_state,
    scheduledAction_nextInvocations,
    scheduledAction_endTime,
    scheduledAction_scheduledActionDescription,
    scheduledAction_scheduledActionName,
    scheduledAction_startTime,

    -- ** CreateSnapshotCopyGrant
    createSnapshotCopyGrant_tags,
    createSnapshotCopyGrant_kmsKeyId,
    createSnapshotCopyGrant_snapshotCopyGrantName,
    createSnapshotCopyGrantResponse_snapshotCopyGrant,
    createSnapshotCopyGrantResponse_httpStatus,

    -- ** CreateSnapshotSchedule
    createSnapshotSchedule_tags,
    createSnapshotSchedule_scheduleDescription,
    createSnapshotSchedule_scheduleIdentifier,
    createSnapshotSchedule_nextInvocations,
    createSnapshotSchedule_dryRun,
    createSnapshotSchedule_scheduleDefinitions,
    snapshotSchedule_tags,
    snapshotSchedule_scheduleDescription,
    snapshotSchedule_associatedClusters,
    snapshotSchedule_scheduleIdentifier,
    snapshotSchedule_nextInvocations,
    snapshotSchedule_associatedClusterCount,
    snapshotSchedule_scheduleDefinitions,

    -- ** CreateTags
    createTags_resourceName,
    createTags_tags,

    -- ** CreateUsageLimit
    createUsageLimit_tags,
    createUsageLimit_period,
    createUsageLimit_breachAction,
    createUsageLimit_clusterIdentifier,
    createUsageLimit_featureType,
    createUsageLimit_limitType,
    createUsageLimit_amount,
    usageLimit_tags,
    usageLimit_clusterIdentifier,
    usageLimit_usageLimitId,
    usageLimit_featureType,
    usageLimit_period,
    usageLimit_breachAction,
    usageLimit_amount,
    usageLimit_limitType,

    -- ** DeauthorizeDataShare
    deauthorizeDataShare_dataShareArn,
    deauthorizeDataShare_consumerIdentifier,
    dataShare_dataShareAssociations,
    dataShare_producerArn,
    dataShare_allowPubliclyAccessibleConsumers,
    dataShare_dataShareArn,
    dataShare_managedBy,

    -- ** DeleteAuthenticationProfile
    deleteAuthenticationProfile_authenticationProfileName,
    deleteAuthenticationProfileResponse_authenticationProfileName,
    deleteAuthenticationProfileResponse_httpStatus,

    -- ** DeleteCluster
    deleteCluster_skipFinalClusterSnapshot,
    deleteCluster_finalClusterSnapshotRetentionPeriod,
    deleteCluster_finalClusterSnapshotIdentifier,
    deleteCluster_clusterIdentifier,
    deleteClusterResponse_cluster,
    deleteClusterResponse_httpStatus,

    -- ** DeleteClusterParameterGroup
    deleteClusterParameterGroup_parameterGroupName,

    -- ** DeleteClusterSecurityGroup
    deleteClusterSecurityGroup_clusterSecurityGroupName,

    -- ** DeleteClusterSnapshot
    deleteClusterSnapshot_snapshotClusterIdentifier,
    deleteClusterSnapshot_snapshotIdentifier,
    deleteClusterSnapshotResponse_snapshot,
    deleteClusterSnapshotResponse_httpStatus,

    -- ** DeleteClusterSubnetGroup
    deleteClusterSubnetGroup_clusterSubnetGroupName,

    -- ** DeleteEndpointAccess
    deleteEndpointAccess_endpointName,
    endpointAccess_port,
    endpointAccess_subnetGroupName,
    endpointAccess_clusterIdentifier,
    endpointAccess_endpointName,
    endpointAccess_resourceOwner,
    endpointAccess_address,
    endpointAccess_endpointStatus,
    endpointAccess_vpcEndpoint,
    endpointAccess_endpointCreateTime,
    endpointAccess_vpcSecurityGroups,

    -- ** DeleteEventSubscription
    deleteEventSubscription_subscriptionName,

    -- ** DeleteHsmClientCertificate
    deleteHsmClientCertificate_hsmClientCertificateIdentifier,

    -- ** DeleteHsmConfiguration
    deleteHsmConfiguration_hsmConfigurationIdentifier,

    -- ** DeletePartner
    deletePartner_accountId,
    deletePartner_clusterIdentifier,
    deletePartner_databaseName,
    deletePartner_partnerName,
    partnerIntegrationOutputMessage_databaseName,
    partnerIntegrationOutputMessage_partnerName,

    -- ** DeleteScheduledAction
    deleteScheduledAction_scheduledActionName,

    -- ** DeleteSnapshotCopyGrant
    deleteSnapshotCopyGrant_snapshotCopyGrantName,

    -- ** DeleteSnapshotSchedule
    deleteSnapshotSchedule_scheduleIdentifier,

    -- ** DeleteTags
    deleteTags_resourceName,
    deleteTags_tagKeys,

    -- ** DeleteUsageLimit
    deleteUsageLimit_usageLimitId,

    -- ** DescribeAccountAttributes
    describeAccountAttributes_attributeNames,
    describeAccountAttributesResponse_accountAttributes,
    describeAccountAttributesResponse_httpStatus,

    -- ** DescribeAuthenticationProfiles
    describeAuthenticationProfiles_authenticationProfileName,
    describeAuthenticationProfilesResponse_authenticationProfiles,
    describeAuthenticationProfilesResponse_httpStatus,

    -- ** DescribeClusterDbRevisions
    describeClusterDbRevisions_clusterIdentifier,
    describeClusterDbRevisions_marker,
    describeClusterDbRevisions_maxRecords,
    describeClusterDbRevisionsResponse_clusterDbRevisions,
    describeClusterDbRevisionsResponse_marker,
    describeClusterDbRevisionsResponse_httpStatus,

    -- ** DescribeClusterParameterGroups
    describeClusterParameterGroups_tagKeys,
    describeClusterParameterGroups_parameterGroupName,
    describeClusterParameterGroups_marker,
    describeClusterParameterGroups_tagValues,
    describeClusterParameterGroups_maxRecords,
    describeClusterParameterGroupsResponse_marker,
    describeClusterParameterGroupsResponse_parameterGroups,
    describeClusterParameterGroupsResponse_httpStatus,

    -- ** DescribeClusterParameters
    describeClusterParameters_marker,
    describeClusterParameters_maxRecords,
    describeClusterParameters_source,
    describeClusterParameters_parameterGroupName,
    describeClusterParametersResponse_marker,
    describeClusterParametersResponse_parameters,
    describeClusterParametersResponse_httpStatus,

    -- ** DescribeClusterSecurityGroups
    describeClusterSecurityGroups_tagKeys,
    describeClusterSecurityGroups_marker,
    describeClusterSecurityGroups_tagValues,
    describeClusterSecurityGroups_clusterSecurityGroupName,
    describeClusterSecurityGroups_maxRecords,
    describeClusterSecurityGroupsResponse_marker,
    describeClusterSecurityGroupsResponse_clusterSecurityGroups,
    describeClusterSecurityGroupsResponse_httpStatus,

    -- ** DescribeClusterSnapshots
    describeClusterSnapshots_clusterIdentifier,
    describeClusterSnapshots_tagKeys,
    describeClusterSnapshots_marker,
    describeClusterSnapshots_clusterExists,
    describeClusterSnapshots_snapshotArn,
    describeClusterSnapshots_tagValues,
    describeClusterSnapshots_snapshotIdentifier,
    describeClusterSnapshots_endTime,
    describeClusterSnapshots_maxRecords,
    describeClusterSnapshots_ownerAccount,
    describeClusterSnapshots_sortingEntities,
    describeClusterSnapshots_startTime,
    describeClusterSnapshots_snapshotType,
    describeClusterSnapshotsResponse_marker,
    describeClusterSnapshotsResponse_snapshots,
    describeClusterSnapshotsResponse_httpStatus,

    -- ** DescribeClusterSubnetGroups
    describeClusterSubnetGroups_tagKeys,
    describeClusterSubnetGroups_marker,
    describeClusterSubnetGroups_clusterSubnetGroupName,
    describeClusterSubnetGroups_tagValues,
    describeClusterSubnetGroups_maxRecords,
    describeClusterSubnetGroupsResponse_marker,
    describeClusterSubnetGroupsResponse_clusterSubnetGroups,
    describeClusterSubnetGroupsResponse_httpStatus,

    -- ** DescribeClusterTracks
    describeClusterTracks_marker,
    describeClusterTracks_maintenanceTrackName,
    describeClusterTracks_maxRecords,
    describeClusterTracksResponse_marker,
    describeClusterTracksResponse_maintenanceTracks,
    describeClusterTracksResponse_httpStatus,

    -- ** DescribeClusterVersions
    describeClusterVersions_marker,
    describeClusterVersions_clusterVersion,
    describeClusterVersions_clusterParameterGroupFamily,
    describeClusterVersions_maxRecords,
    describeClusterVersionsResponse_clusterVersions,
    describeClusterVersionsResponse_marker,
    describeClusterVersionsResponse_httpStatus,

    -- ** DescribeClusters
    describeClusters_clusterIdentifier,
    describeClusters_tagKeys,
    describeClusters_marker,
    describeClusters_tagValues,
    describeClusters_maxRecords,
    describeClustersResponse_marker,
    describeClustersResponse_clusters,
    describeClustersResponse_httpStatus,

    -- ** DescribeDataShares
    describeDataShares_marker,
    describeDataShares_maxRecords,
    describeDataShares_dataShareArn,
    describeDataSharesResponse_marker,
    describeDataSharesResponse_dataShares,
    describeDataSharesResponse_httpStatus,

    -- ** DescribeDataSharesForConsumer
    describeDataSharesForConsumer_marker,
    describeDataSharesForConsumer_status,
    describeDataSharesForConsumer_maxRecords,
    describeDataSharesForConsumer_consumerArn,
    describeDataSharesForConsumerResponse_marker,
    describeDataSharesForConsumerResponse_dataShares,
    describeDataSharesForConsumerResponse_httpStatus,

    -- ** DescribeDataSharesForProducer
    describeDataSharesForProducer_marker,
    describeDataSharesForProducer_status,
    describeDataSharesForProducer_maxRecords,
    describeDataSharesForProducer_producerArn,
    describeDataSharesForProducerResponse_marker,
    describeDataSharesForProducerResponse_dataShares,
    describeDataSharesForProducerResponse_httpStatus,

    -- ** DescribeDefaultClusterParameters
    describeDefaultClusterParameters_marker,
    describeDefaultClusterParameters_maxRecords,
    describeDefaultClusterParameters_parameterGroupFamily,
    describeDefaultClusterParametersResponse_httpStatus,
    describeDefaultClusterParametersResponse_defaultClusterParameters,

    -- ** DescribeEndpointAccess
    describeEndpointAccess_clusterIdentifier,
    describeEndpointAccess_marker,
    describeEndpointAccess_endpointName,
    describeEndpointAccess_maxRecords,
    describeEndpointAccess_resourceOwner,
    describeEndpointAccess_vpcId,
    describeEndpointAccessResponse_marker,
    describeEndpointAccessResponse_endpointAccessList,
    describeEndpointAccessResponse_httpStatus,

    -- ** DescribeEndpointAuthorization
    describeEndpointAuthorization_clusterIdentifier,
    describeEndpointAuthorization_marker,
    describeEndpointAuthorization_account,
    describeEndpointAuthorization_maxRecords,
    describeEndpointAuthorization_grantee,
    describeEndpointAuthorizationResponse_marker,
    describeEndpointAuthorizationResponse_endpointAuthorizationList,
    describeEndpointAuthorizationResponse_httpStatus,

    -- ** DescribeEventCategories
    describeEventCategories_sourceType,
    describeEventCategoriesResponse_eventCategoriesMapList,
    describeEventCategoriesResponse_httpStatus,

    -- ** DescribeEventSubscriptions
    describeEventSubscriptions_tagKeys,
    describeEventSubscriptions_marker,
    describeEventSubscriptions_tagValues,
    describeEventSubscriptions_subscriptionName,
    describeEventSubscriptions_maxRecords,
    describeEventSubscriptionsResponse_marker,
    describeEventSubscriptionsResponse_eventSubscriptionsList,
    describeEventSubscriptionsResponse_httpStatus,

    -- ** DescribeEvents
    describeEvents_marker,
    describeEvents_sourceType,
    describeEvents_endTime,
    describeEvents_maxRecords,
    describeEvents_duration,
    describeEvents_sourceIdentifier,
    describeEvents_startTime,
    describeEventsResponse_marker,
    describeEventsResponse_events,
    describeEventsResponse_httpStatus,

    -- ** DescribeHsmClientCertificates
    describeHsmClientCertificates_tagKeys,
    describeHsmClientCertificates_marker,
    describeHsmClientCertificates_tagValues,
    describeHsmClientCertificates_hsmClientCertificateIdentifier,
    describeHsmClientCertificates_maxRecords,
    describeHsmClientCertificatesResponse_marker,
    describeHsmClientCertificatesResponse_hsmClientCertificates,
    describeHsmClientCertificatesResponse_httpStatus,

    -- ** DescribeHsmConfigurations
    describeHsmConfigurations_tagKeys,
    describeHsmConfigurations_marker,
    describeHsmConfigurations_tagValues,
    describeHsmConfigurations_maxRecords,
    describeHsmConfigurations_hsmConfigurationIdentifier,
    describeHsmConfigurationsResponse_marker,
    describeHsmConfigurationsResponse_hsmConfigurations,
    describeHsmConfigurationsResponse_httpStatus,

    -- ** DescribeLoggingStatus
    describeLoggingStatus_clusterIdentifier,
    loggingStatus_lastSuccessfulDeliveryTime,
    loggingStatus_s3KeyPrefix,
    loggingStatus_lastFailureMessage,
    loggingStatus_loggingEnabled,
    loggingStatus_logExports,
    loggingStatus_bucketName,
    loggingStatus_lastFailureTime,
    loggingStatus_logDestinationType,

    -- ** DescribeNodeConfigurationOptions
    describeNodeConfigurationOptions_clusterIdentifier,
    describeNodeConfigurationOptions_marker,
    describeNodeConfigurationOptions_snapshotArn,
    describeNodeConfigurationOptions_snapshotIdentifier,
    describeNodeConfigurationOptions_filters,
    describeNodeConfigurationOptions_maxRecords,
    describeNodeConfigurationOptions_ownerAccount,
    describeNodeConfigurationOptions_actionType,
    describeNodeConfigurationOptionsResponse_marker,
    describeNodeConfigurationOptionsResponse_nodeConfigurationOptionList,
    describeNodeConfigurationOptionsResponse_httpStatus,

    -- ** DescribeOrderableClusterOptions
    describeOrderableClusterOptions_marker,
    describeOrderableClusterOptions_clusterVersion,
    describeOrderableClusterOptions_nodeType,
    describeOrderableClusterOptions_maxRecords,
    describeOrderableClusterOptionsResponse_marker,
    describeOrderableClusterOptionsResponse_orderableClusterOptions,
    describeOrderableClusterOptionsResponse_httpStatus,

    -- ** DescribePartners
    describePartners_databaseName,
    describePartners_partnerName,
    describePartners_accountId,
    describePartners_clusterIdentifier,
    describePartnersResponse_partnerIntegrationInfoList,
    describePartnersResponse_httpStatus,

    -- ** DescribeReservedNodeExchangeStatus
    describeReservedNodeExchangeStatus_marker,
    describeReservedNodeExchangeStatus_reservedNodeExchangeRequestId,
    describeReservedNodeExchangeStatus_reservedNodeId,
    describeReservedNodeExchangeStatus_maxRecords,
    describeReservedNodeExchangeStatusResponse_marker,
    describeReservedNodeExchangeStatusResponse_reservedNodeExchangeStatusDetails,
    describeReservedNodeExchangeStatusResponse_httpStatus,

    -- ** DescribeReservedNodeOfferings
    describeReservedNodeOfferings_marker,
    describeReservedNodeOfferings_maxRecords,
    describeReservedNodeOfferings_reservedNodeOfferingId,
    describeReservedNodeOfferingsResponse_marker,
    describeReservedNodeOfferingsResponse_reservedNodeOfferings,
    describeReservedNodeOfferingsResponse_httpStatus,

    -- ** DescribeReservedNodes
    describeReservedNodes_marker,
    describeReservedNodes_reservedNodeId,
    describeReservedNodes_maxRecords,
    describeReservedNodesResponse_reservedNodes,
    describeReservedNodesResponse_marker,
    describeReservedNodesResponse_httpStatus,

    -- ** DescribeResize
    describeResize_clusterIdentifier,
    resizeProgressMessage_message,
    resizeProgressMessage_targetEncryptionType,
    resizeProgressMessage_avgResizeRateInMegaBytesPerSecond,
    resizeProgressMessage_targetClusterType,
    resizeProgressMessage_dataTransferProgressPercent,
    resizeProgressMessage_targetNumberOfNodes,
    resizeProgressMessage_totalResizeDataInMegaBytes,
    resizeProgressMessage_importTablesNotStarted,
    resizeProgressMessage_status,
    resizeProgressMessage_elapsedTimeInSeconds,
    resizeProgressMessage_importTablesCompleted,
    resizeProgressMessage_progressInMegaBytes,
    resizeProgressMessage_importTablesInProgress,
    resizeProgressMessage_estimatedTimeToCompletionInSeconds,
    resizeProgressMessage_targetNodeType,
    resizeProgressMessage_resizeType,

    -- ** DescribeScheduledActions
    describeScheduledActions_marker,
    describeScheduledActions_active,
    describeScheduledActions_filters,
    describeScheduledActions_endTime,
    describeScheduledActions_maxRecords,
    describeScheduledActions_scheduledActionName,
    describeScheduledActions_startTime,
    describeScheduledActions_targetActionType,
    describeScheduledActionsResponse_marker,
    describeScheduledActionsResponse_scheduledActions,
    describeScheduledActionsResponse_httpStatus,

    -- ** DescribeSnapshotCopyGrants
    describeSnapshotCopyGrants_tagKeys,
    describeSnapshotCopyGrants_marker,
    describeSnapshotCopyGrants_tagValues,
    describeSnapshotCopyGrants_maxRecords,
    describeSnapshotCopyGrants_snapshotCopyGrantName,
    describeSnapshotCopyGrantsResponse_marker,
    describeSnapshotCopyGrantsResponse_snapshotCopyGrants,
    describeSnapshotCopyGrantsResponse_httpStatus,

    -- ** DescribeSnapshotSchedules
    describeSnapshotSchedules_clusterIdentifier,
    describeSnapshotSchedules_tagKeys,
    describeSnapshotSchedules_marker,
    describeSnapshotSchedules_tagValues,
    describeSnapshotSchedules_scheduleIdentifier,
    describeSnapshotSchedules_maxRecords,
    describeSnapshotSchedulesResponse_marker,
    describeSnapshotSchedulesResponse_snapshotSchedules,
    describeSnapshotSchedulesResponse_httpStatus,

    -- ** DescribeStorage
    describeStorageResponse_totalBackupSizeInMegaBytes,
    describeStorageResponse_totalProvisionedStorageInMegaBytes,
    describeStorageResponse_httpStatus,

    -- ** DescribeTableRestoreStatus
    describeTableRestoreStatus_clusterIdentifier,
    describeTableRestoreStatus_marker,
    describeTableRestoreStatus_tableRestoreRequestId,
    describeTableRestoreStatus_maxRecords,
    describeTableRestoreStatusResponse_marker,
    describeTableRestoreStatusResponse_tableRestoreStatusDetails,
    describeTableRestoreStatusResponse_httpStatus,

    -- ** DescribeTags
    describeTags_resourceType,
    describeTags_tagKeys,
    describeTags_marker,
    describeTags_resourceName,
    describeTags_tagValues,
    describeTags_maxRecords,
    describeTagsResponse_marker,
    describeTagsResponse_taggedResources,
    describeTagsResponse_httpStatus,

    -- ** DescribeUsageLimits
    describeUsageLimits_clusterIdentifier,
    describeUsageLimits_tagKeys,
    describeUsageLimits_marker,
    describeUsageLimits_usageLimitId,
    describeUsageLimits_tagValues,
    describeUsageLimits_featureType,
    describeUsageLimits_maxRecords,
    describeUsageLimitsResponse_marker,
    describeUsageLimitsResponse_usageLimits,
    describeUsageLimitsResponse_httpStatus,

    -- ** DisableLogging
    disableLogging_clusterIdentifier,
    loggingStatus_lastSuccessfulDeliveryTime,
    loggingStatus_s3KeyPrefix,
    loggingStatus_lastFailureMessage,
    loggingStatus_loggingEnabled,
    loggingStatus_logExports,
    loggingStatus_bucketName,
    loggingStatus_lastFailureTime,
    loggingStatus_logDestinationType,

    -- ** DisableSnapshotCopy
    disableSnapshotCopy_clusterIdentifier,
    disableSnapshotCopyResponse_cluster,
    disableSnapshotCopyResponse_httpStatus,

    -- ** DisassociateDataShareConsumer
    disassociateDataShareConsumer_disassociateEntireAccount,
    disassociateDataShareConsumer_consumerArn,
    disassociateDataShareConsumer_consumerRegion,
    disassociateDataShareConsumer_dataShareArn,
    dataShare_dataShareAssociations,
    dataShare_producerArn,
    dataShare_allowPubliclyAccessibleConsumers,
    dataShare_dataShareArn,
    dataShare_managedBy,

    -- ** EnableLogging
    enableLogging_s3KeyPrefix,
    enableLogging_logExports,
    enableLogging_bucketName,
    enableLogging_logDestinationType,
    enableLogging_clusterIdentifier,
    loggingStatus_lastSuccessfulDeliveryTime,
    loggingStatus_s3KeyPrefix,
    loggingStatus_lastFailureMessage,
    loggingStatus_loggingEnabled,
    loggingStatus_logExports,
    loggingStatus_bucketName,
    loggingStatus_lastFailureTime,
    loggingStatus_logDestinationType,

    -- ** EnableSnapshotCopy
    enableSnapshotCopy_manualSnapshotRetentionPeriod,
    enableSnapshotCopy_snapshotCopyGrantName,
    enableSnapshotCopy_retentionPeriod,
    enableSnapshotCopy_clusterIdentifier,
    enableSnapshotCopy_destinationRegion,
    enableSnapshotCopyResponse_cluster,
    enableSnapshotCopyResponse_httpStatus,

    -- ** GetClusterCredentials
    getClusterCredentials_durationSeconds,
    getClusterCredentials_dbGroups,
    getClusterCredentials_autoCreate,
    getClusterCredentials_dbName,
    getClusterCredentials_dbUser,
    getClusterCredentials_clusterIdentifier,
    getClusterCredentialsResponse_expiration,
    getClusterCredentialsResponse_dbPassword,
    getClusterCredentialsResponse_dbUser,
    getClusterCredentialsResponse_httpStatus,

    -- ** GetClusterCredentialsWithIAM
    getClusterCredentialsWithIAM_durationSeconds,
    getClusterCredentialsWithIAM_dbName,
    getClusterCredentialsWithIAM_clusterIdentifier,
    getClusterCredentialsWithIAMResponse_expiration,
    getClusterCredentialsWithIAMResponse_dbPassword,
    getClusterCredentialsWithIAMResponse_nextRefreshTime,
    getClusterCredentialsWithIAMResponse_dbUser,
    getClusterCredentialsWithIAMResponse_httpStatus,

    -- ** GetReservedNodeExchangeConfigurationOptions
    getReservedNodeExchangeConfigurationOptions_clusterIdentifier,
    getReservedNodeExchangeConfigurationOptions_marker,
    getReservedNodeExchangeConfigurationOptions_snapshotIdentifier,
    getReservedNodeExchangeConfigurationOptions_maxRecords,
    getReservedNodeExchangeConfigurationOptions_actionType,
    getReservedNodeExchangeConfigurationOptionsResponse_marker,
    getReservedNodeExchangeConfigurationOptionsResponse_reservedNodeConfigurationOptionList,
    getReservedNodeExchangeConfigurationOptionsResponse_httpStatus,

    -- ** GetReservedNodeExchangeOfferings
    getReservedNodeExchangeOfferings_marker,
    getReservedNodeExchangeOfferings_maxRecords,
    getReservedNodeExchangeOfferings_reservedNodeId,
    getReservedNodeExchangeOfferingsResponse_marker,
    getReservedNodeExchangeOfferingsResponse_reservedNodeOfferings,
    getReservedNodeExchangeOfferingsResponse_httpStatus,

    -- ** ModifyAquaConfiguration
    modifyAquaConfiguration_aquaConfigurationStatus,
    modifyAquaConfiguration_clusterIdentifier,
    modifyAquaConfigurationResponse_aquaConfiguration,
    modifyAquaConfigurationResponse_httpStatus,

    -- ** ModifyAuthenticationProfile
    modifyAuthenticationProfile_authenticationProfileName,
    modifyAuthenticationProfile_authenticationProfileContent,
    modifyAuthenticationProfileResponse_authenticationProfileName,
    modifyAuthenticationProfileResponse_authenticationProfileContent,
    modifyAuthenticationProfileResponse_httpStatus,

    -- ** ModifyCluster
    modifyCluster_port,
    modifyCluster_vpcSecurityGroupIds,
    modifyCluster_elasticIp,
    modifyCluster_manualSnapshotRetentionPeriod,
    modifyCluster_allowVersionUpgrade,
    modifyCluster_clusterVersion,
    modifyCluster_maintenanceTrackName,
    modifyCluster_hsmClientCertificateIdentifier,
    modifyCluster_availabilityZone,
    modifyCluster_masterUserPassword,
    modifyCluster_nodeType,
    modifyCluster_publiclyAccessible,
    modifyCluster_clusterParameterGroupName,
    modifyCluster_newClusterIdentifier,
    modifyCluster_encrypted,
    modifyCluster_numberOfNodes,
    modifyCluster_kmsKeyId,
    modifyCluster_availabilityZoneRelocation,
    modifyCluster_enhancedVpcRouting,
    modifyCluster_preferredMaintenanceWindow,
    modifyCluster_clusterType,
    modifyCluster_clusterSecurityGroups,
    modifyCluster_automatedSnapshotRetentionPeriod,
    modifyCluster_hsmConfigurationIdentifier,
    modifyCluster_clusterIdentifier,
    modifyClusterResponse_cluster,
    modifyClusterResponse_httpStatus,

    -- ** ModifyClusterDbRevision
    modifyClusterDbRevision_clusterIdentifier,
    modifyClusterDbRevision_revisionTarget,
    modifyClusterDbRevisionResponse_cluster,
    modifyClusterDbRevisionResponse_httpStatus,

    -- ** ModifyClusterIamRoles
    modifyClusterIamRoles_removeIamRoles,
    modifyClusterIamRoles_addIamRoles,
    modifyClusterIamRoles_defaultIamRoleArn,
    modifyClusterIamRoles_clusterIdentifier,
    modifyClusterIamRolesResponse_cluster,
    modifyClusterIamRolesResponse_httpStatus,

    -- ** ModifyClusterMaintenance
    modifyClusterMaintenance_deferMaintenanceDuration,
    modifyClusterMaintenance_deferMaintenanceIdentifier,
    modifyClusterMaintenance_deferMaintenance,
    modifyClusterMaintenance_deferMaintenanceEndTime,
    modifyClusterMaintenance_deferMaintenanceStartTime,
    modifyClusterMaintenance_clusterIdentifier,
    modifyClusterMaintenanceResponse_cluster,
    modifyClusterMaintenanceResponse_httpStatus,

    -- ** ModifyClusterParameterGroup
    modifyClusterParameterGroup_parameterGroupName,
    modifyClusterParameterGroup_parameters,
    clusterParameterGroupNameMessage_parameterGroupName,
    clusterParameterGroupNameMessage_parameterGroupStatus,

    -- ** ModifyClusterSnapshot
    modifyClusterSnapshot_manualSnapshotRetentionPeriod,
    modifyClusterSnapshot_force,
    modifyClusterSnapshot_snapshotIdentifier,
    modifyClusterSnapshotResponse_snapshot,
    modifyClusterSnapshotResponse_httpStatus,

    -- ** ModifyClusterSnapshotSchedule
    modifyClusterSnapshotSchedule_disassociateSchedule,
    modifyClusterSnapshotSchedule_scheduleIdentifier,
    modifyClusterSnapshotSchedule_clusterIdentifier,

    -- ** ModifyClusterSubnetGroup
    modifyClusterSubnetGroup_description,
    modifyClusterSubnetGroup_clusterSubnetGroupName,
    modifyClusterSubnetGroup_subnetIds,
    modifyClusterSubnetGroupResponse_clusterSubnetGroup,
    modifyClusterSubnetGroupResponse_httpStatus,

    -- ** ModifyEndpointAccess
    modifyEndpointAccess_vpcSecurityGroupIds,
    modifyEndpointAccess_endpointName,
    endpointAccess_port,
    endpointAccess_subnetGroupName,
    endpointAccess_clusterIdentifier,
    endpointAccess_endpointName,
    endpointAccess_resourceOwner,
    endpointAccess_address,
    endpointAccess_endpointStatus,
    endpointAccess_vpcEndpoint,
    endpointAccess_endpointCreateTime,
    endpointAccess_vpcSecurityGroups,

    -- ** ModifyEventSubscription
    modifyEventSubscription_severity,
    modifyEventSubscription_sourceIds,
    modifyEventSubscription_sourceType,
    modifyEventSubscription_enabled,
    modifyEventSubscription_snsTopicArn,
    modifyEventSubscription_eventCategories,
    modifyEventSubscription_subscriptionName,
    modifyEventSubscriptionResponse_eventSubscription,
    modifyEventSubscriptionResponse_httpStatus,

    -- ** ModifyScheduledAction
    modifyScheduledAction_schedule,
    modifyScheduledAction_targetAction,
    modifyScheduledAction_iamRole,
    modifyScheduledAction_enable,
    modifyScheduledAction_endTime,
    modifyScheduledAction_scheduledActionDescription,
    modifyScheduledAction_startTime,
    modifyScheduledAction_scheduledActionName,
    scheduledAction_schedule,
    scheduledAction_targetAction,
    scheduledAction_iamRole,
    scheduledAction_state,
    scheduledAction_nextInvocations,
    scheduledAction_endTime,
    scheduledAction_scheduledActionDescription,
    scheduledAction_scheduledActionName,
    scheduledAction_startTime,

    -- ** ModifySnapshotCopyRetentionPeriod
    modifySnapshotCopyRetentionPeriod_manual,
    modifySnapshotCopyRetentionPeriod_clusterIdentifier,
    modifySnapshotCopyRetentionPeriod_retentionPeriod,
    modifySnapshotCopyRetentionPeriodResponse_cluster,
    modifySnapshotCopyRetentionPeriodResponse_httpStatus,

    -- ** ModifySnapshotSchedule
    modifySnapshotSchedule_scheduleIdentifier,
    modifySnapshotSchedule_scheduleDefinitions,
    snapshotSchedule_tags,
    snapshotSchedule_scheduleDescription,
    snapshotSchedule_associatedClusters,
    snapshotSchedule_scheduleIdentifier,
    snapshotSchedule_nextInvocations,
    snapshotSchedule_associatedClusterCount,
    snapshotSchedule_scheduleDefinitions,

    -- ** ModifyUsageLimit
    modifyUsageLimit_breachAction,
    modifyUsageLimit_amount,
    modifyUsageLimit_usageLimitId,
    usageLimit_tags,
    usageLimit_clusterIdentifier,
    usageLimit_usageLimitId,
    usageLimit_featureType,
    usageLimit_period,
    usageLimit_breachAction,
    usageLimit_amount,
    usageLimit_limitType,

    -- ** PauseCluster
    pauseCluster_clusterIdentifier,
    pauseClusterResponse_cluster,
    pauseClusterResponse_httpStatus,

    -- ** PurchaseReservedNodeOffering
    purchaseReservedNodeOffering_nodeCount,
    purchaseReservedNodeOffering_reservedNodeOfferingId,
    purchaseReservedNodeOfferingResponse_reservedNode,
    purchaseReservedNodeOfferingResponse_httpStatus,

    -- ** RebootCluster
    rebootCluster_clusterIdentifier,
    rebootClusterResponse_cluster,
    rebootClusterResponse_httpStatus,

    -- ** RejectDataShare
    rejectDataShare_dataShareArn,
    dataShare_dataShareAssociations,
    dataShare_producerArn,
    dataShare_allowPubliclyAccessibleConsumers,
    dataShare_dataShareArn,
    dataShare_managedBy,

    -- ** ResetClusterParameterGroup
    resetClusterParameterGroup_resetAllParameters,
    resetClusterParameterGroup_parameters,
    resetClusterParameterGroup_parameterGroupName,
    clusterParameterGroupNameMessage_parameterGroupName,
    clusterParameterGroupNameMessage_parameterGroupStatus,

    -- ** ResizeCluster
    resizeCluster_reservedNodeId,
    resizeCluster_nodeType,
    resizeCluster_classic,
    resizeCluster_numberOfNodes,
    resizeCluster_clusterType,
    resizeCluster_targetReservedNodeOfferingId,
    resizeCluster_clusterIdentifier,
    resizeClusterResponse_cluster,
    resizeClusterResponse_httpStatus,

    -- ** RestoreFromClusterSnapshot
    restoreFromClusterSnapshot_port,
    restoreFromClusterSnapshot_vpcSecurityGroupIds,
    restoreFromClusterSnapshot_elasticIp,
    restoreFromClusterSnapshot_manualSnapshotRetentionPeriod,
    restoreFromClusterSnapshot_additionalInfo,
    restoreFromClusterSnapshot_allowVersionUpgrade,
    restoreFromClusterSnapshot_clusterSubnetGroupName,
    restoreFromClusterSnapshot_snapshotScheduleIdentifier,
    restoreFromClusterSnapshot_snapshotArn,
    restoreFromClusterSnapshot_aquaConfigurationStatus,
    restoreFromClusterSnapshot_snapshotIdentifier,
    restoreFromClusterSnapshot_maintenanceTrackName,
    restoreFromClusterSnapshot_snapshotClusterIdentifier,
    restoreFromClusterSnapshot_iamRoles,
    restoreFromClusterSnapshot_reservedNodeId,
    restoreFromClusterSnapshot_hsmClientCertificateIdentifier,
    restoreFromClusterSnapshot_availabilityZone,
    restoreFromClusterSnapshot_nodeType,
    restoreFromClusterSnapshot_publiclyAccessible,
    restoreFromClusterSnapshot_clusterParameterGroupName,
    restoreFromClusterSnapshot_encrypted,
    restoreFromClusterSnapshot_numberOfNodes,
    restoreFromClusterSnapshot_kmsKeyId,
    restoreFromClusterSnapshot_availabilityZoneRelocation,
    restoreFromClusterSnapshot_defaultIamRoleArn,
    restoreFromClusterSnapshot_enhancedVpcRouting,
    restoreFromClusterSnapshot_preferredMaintenanceWindow,
    restoreFromClusterSnapshot_clusterSecurityGroups,
    restoreFromClusterSnapshot_automatedSnapshotRetentionPeriod,
    restoreFromClusterSnapshot_ownerAccount,
    restoreFromClusterSnapshot_targetReservedNodeOfferingId,
    restoreFromClusterSnapshot_hsmConfigurationIdentifier,
    restoreFromClusterSnapshot_clusterIdentifier,
    restoreFromClusterSnapshotResponse_cluster,
    restoreFromClusterSnapshotResponse_httpStatus,

    -- ** RestoreTableFromClusterSnapshot
    restoreTableFromClusterSnapshot_targetSchemaName,
    restoreTableFromClusterSnapshot_sourceSchemaName,
    restoreTableFromClusterSnapshot_targetDatabaseName,
    restoreTableFromClusterSnapshot_enableCaseSensitiveIdentifier,
    restoreTableFromClusterSnapshot_clusterIdentifier,
    restoreTableFromClusterSnapshot_snapshotIdentifier,
    restoreTableFromClusterSnapshot_sourceDatabaseName,
    restoreTableFromClusterSnapshot_sourceTableName,
    restoreTableFromClusterSnapshot_newTableName,
    restoreTableFromClusterSnapshotResponse_tableRestoreStatus,
    restoreTableFromClusterSnapshotResponse_httpStatus,

    -- ** ResumeCluster
    resumeCluster_clusterIdentifier,
    resumeClusterResponse_cluster,
    resumeClusterResponse_httpStatus,

    -- ** RevokeClusterSecurityGroupIngress
    revokeClusterSecurityGroupIngress_eC2SecurityGroupOwnerId,
    revokeClusterSecurityGroupIngress_eC2SecurityGroupName,
    revokeClusterSecurityGroupIngress_cidrip,
    revokeClusterSecurityGroupIngress_clusterSecurityGroupName,
    revokeClusterSecurityGroupIngressResponse_clusterSecurityGroup,
    revokeClusterSecurityGroupIngressResponse_httpStatus,

    -- ** RevokeEndpointAccess
    revokeEndpointAccess_clusterIdentifier,
    revokeEndpointAccess_account,
    revokeEndpointAccess_vpcIds,
    revokeEndpointAccess_force,
    endpointAuthorization_clusterIdentifier,
    endpointAuthorization_endpointCount,
    endpointAuthorization_allowedAllVPCs,
    endpointAuthorization_status,
    endpointAuthorization_authorizeTime,
    endpointAuthorization_clusterStatus,
    endpointAuthorization_grantor,
    endpointAuthorization_grantee,
    endpointAuthorization_allowedVPCs,

    -- ** RevokeSnapshotAccess
    revokeSnapshotAccess_snapshotArn,
    revokeSnapshotAccess_snapshotIdentifier,
    revokeSnapshotAccess_snapshotClusterIdentifier,
    revokeSnapshotAccess_accountWithRestoreAccess,
    revokeSnapshotAccessResponse_snapshot,
    revokeSnapshotAccessResponse_httpStatus,

    -- ** RotateEncryptionKey
    rotateEncryptionKey_clusterIdentifier,
    rotateEncryptionKeyResponse_cluster,
    rotateEncryptionKeyResponse_httpStatus,

    -- ** UpdatePartnerStatus
    updatePartnerStatus_statusMessage,
    updatePartnerStatus_accountId,
    updatePartnerStatus_clusterIdentifier,
    updatePartnerStatus_databaseName,
    updatePartnerStatus_partnerName,
    updatePartnerStatus_status,
    partnerIntegrationOutputMessage_databaseName,
    partnerIntegrationOutputMessage_partnerName,

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
    cluster_tags,
    cluster_clusterIdentifier,
    cluster_aquaConfiguration,
    cluster_clusterPublicKey,
    cluster_manualSnapshotRetentionPeriod,
    cluster_masterUsername,
    cluster_availabilityZoneRelocationStatus,
    cluster_clusterParameterGroups,
    cluster_allowVersionUpgrade,
    cluster_expectedNextSnapshotScheduleTimeStatus,
    cluster_clusterSubnetGroupName,
    cluster_snapshotScheduleIdentifier,
    cluster_nextMaintenanceWindowStartTime,
    cluster_elasticIpStatus,
    cluster_clusterNodes,
    cluster_clusterVersion,
    cluster_pendingActions,
    cluster_clusterRevisionNumber,
    cluster_maintenanceTrackName,
    cluster_iamRoles,
    cluster_availabilityZone,
    cluster_nodeType,
    cluster_publiclyAccessible,
    cluster_modifyStatus,
    cluster_clusterSnapshotCopyStatus,
    cluster_clusterNamespaceArn,
    cluster_snapshotScheduleState,
    cluster_hsmStatus,
    cluster_clusterStatus,
    cluster_resizeInfo,
    cluster_deferredMaintenanceWindows,
    cluster_encrypted,
    cluster_dataTransferProgress,
    cluster_numberOfNodes,
    cluster_kmsKeyId,
    cluster_defaultIamRoleArn,
    cluster_expectedNextSnapshotScheduleTime,
    cluster_elasticResizeNumberOfNodeOptions,
    cluster_pendingModifiedValues,
    cluster_enhancedVpcRouting,
    cluster_preferredMaintenanceWindow,
    cluster_clusterSecurityGroups,
    cluster_endpoint,
    cluster_vpcId,
    cluster_automatedSnapshotRetentionPeriod,
    cluster_clusterCreateTime,
    cluster_reservedNodeExchangeStatus,
    cluster_totalStorageCapacityInMegaBytes,
    cluster_dbName,
    cluster_restoreStatus,
    cluster_clusterAvailabilityStatus,
    cluster_vpcSecurityGroups,

    -- ** ClusterAssociatedToSchedule
    clusterAssociatedToSchedule_clusterIdentifier,
    clusterAssociatedToSchedule_scheduleAssociationState,

    -- ** ClusterDbRevision
    clusterDbRevision_clusterIdentifier,
    clusterDbRevision_currentDatabaseRevision,
    clusterDbRevision_databaseRevisionReleaseDate,
    clusterDbRevision_revisionTargets,

    -- ** ClusterIamRole
    clusterIamRole_iamRoleArn,
    clusterIamRole_applyStatus,

    -- ** ClusterNode
    clusterNode_nodeRole,
    clusterNode_publicIPAddress,
    clusterNode_privateIPAddress,

    -- ** ClusterParameterGroup
    clusterParameterGroup_tags,
    clusterParameterGroup_parameterGroupName,
    clusterParameterGroup_parameterGroupFamily,
    clusterParameterGroup_description,

    -- ** ClusterParameterGroupNameMessage
    clusterParameterGroupNameMessage_parameterGroupName,
    clusterParameterGroupNameMessage_parameterGroupStatus,

    -- ** ClusterParameterGroupStatus
    clusterParameterGroupStatus_parameterGroupName,
    clusterParameterGroupStatus_clusterParameterStatusList,
    clusterParameterGroupStatus_parameterApplyStatus,

    -- ** ClusterParameterStatus
    clusterParameterStatus_parameterApplyErrorDescription,
    clusterParameterStatus_parameterName,
    clusterParameterStatus_parameterApplyStatus,

    -- ** ClusterSecurityGroup
    clusterSecurityGroup_tags,
    clusterSecurityGroup_clusterSecurityGroupName,
    clusterSecurityGroup_description,
    clusterSecurityGroup_eC2SecurityGroups,
    clusterSecurityGroup_iPRanges,

    -- ** ClusterSecurityGroupMembership
    clusterSecurityGroupMembership_clusterSecurityGroupName,
    clusterSecurityGroupMembership_status,

    -- ** ClusterSnapshotCopyStatus
    clusterSnapshotCopyStatus_manualSnapshotRetentionPeriod,
    clusterSnapshotCopyStatus_snapshotCopyGrantName,
    clusterSnapshotCopyStatus_retentionPeriod,
    clusterSnapshotCopyStatus_destinationRegion,

    -- ** ClusterSubnetGroup
    clusterSubnetGroup_tags,
    clusterSubnetGroup_clusterSubnetGroupName,
    clusterSubnetGroup_subnetGroupStatus,
    clusterSubnetGroup_subnets,
    clusterSubnetGroup_description,
    clusterSubnetGroup_vpcId,

    -- ** ClusterVersion
    clusterVersion_clusterVersion,
    clusterVersion_clusterParameterGroupFamily,
    clusterVersion_description,

    -- ** DataShare
    dataShare_dataShareAssociations,
    dataShare_producerArn,
    dataShare_allowPubliclyAccessibleConsumers,
    dataShare_dataShareArn,
    dataShare_managedBy,

    -- ** DataShareAssociation
    dataShareAssociation_consumerIdentifier,
    dataShareAssociation_statusChangeDate,
    dataShareAssociation_status,
    dataShareAssociation_createdDate,
    dataShareAssociation_consumerRegion,

    -- ** DataTransferProgress
    dataTransferProgress_totalDataInMegaBytes,
    dataTransferProgress_currentRateInMegaBytesPerSecond,
    dataTransferProgress_dataTransferredInMegaBytes,
    dataTransferProgress_status,
    dataTransferProgress_elapsedTimeInSeconds,
    dataTransferProgress_estimatedTimeToCompletionInSeconds,

    -- ** DefaultClusterParameters
    defaultClusterParameters_parameterGroupFamily,
    defaultClusterParameters_marker,
    defaultClusterParameters_parameters,

    -- ** DeferredMaintenanceWindow
    deferredMaintenanceWindow_deferMaintenanceIdentifier,
    deferredMaintenanceWindow_deferMaintenanceEndTime,
    deferredMaintenanceWindow_deferMaintenanceStartTime,

    -- ** DeleteClusterSnapshotMessage
    deleteClusterSnapshotMessage_snapshotClusterIdentifier,
    deleteClusterSnapshotMessage_snapshotIdentifier,

    -- ** EC2SecurityGroup
    eC2SecurityGroup_tags,
    eC2SecurityGroup_eC2SecurityGroupOwnerId,
    eC2SecurityGroup_status,
    eC2SecurityGroup_eC2SecurityGroupName,

    -- ** ElasticIpStatus
    elasticIpStatus_elasticIp,
    elasticIpStatus_status,

    -- ** Endpoint
    endpoint_port,
    endpoint_address,
    endpoint_vpcEndpoints,

    -- ** EndpointAccess
    endpointAccess_port,
    endpointAccess_subnetGroupName,
    endpointAccess_clusterIdentifier,
    endpointAccess_endpointName,
    endpointAccess_resourceOwner,
    endpointAccess_address,
    endpointAccess_endpointStatus,
    endpointAccess_vpcEndpoint,
    endpointAccess_endpointCreateTime,
    endpointAccess_vpcSecurityGroups,

    -- ** EndpointAuthorization
    endpointAuthorization_clusterIdentifier,
    endpointAuthorization_endpointCount,
    endpointAuthorization_allowedAllVPCs,
    endpointAuthorization_status,
    endpointAuthorization_authorizeTime,
    endpointAuthorization_clusterStatus,
    endpointAuthorization_grantor,
    endpointAuthorization_grantee,
    endpointAuthorization_allowedVPCs,

    -- ** Event
    event_message,
    event_severity,
    event_date,
    event_sourceType,
    event_sourceIdentifier,
    event_eventId,
    event_eventCategories,

    -- ** EventCategoriesMap
    eventCategoriesMap_sourceType,
    eventCategoriesMap_events,

    -- ** EventInfoMap
    eventInfoMap_severity,
    eventInfoMap_eventId,
    eventInfoMap_eventCategories,
    eventInfoMap_eventDescription,

    -- ** EventSubscription
    eventSubscription_tags,
    eventSubscription_severity,
    eventSubscription_subscriptionCreationTime,
    eventSubscription_custSubscriptionId,
    eventSubscription_sourceIdsList,
    eventSubscription_status,
    eventSubscription_sourceType,
    eventSubscription_enabled,
    eventSubscription_snsTopicArn,
    eventSubscription_eventCategoriesList,
    eventSubscription_customerAwsId,

    -- ** HsmClientCertificate
    hsmClientCertificate_tags,
    hsmClientCertificate_hsmClientCertificateIdentifier,
    hsmClientCertificate_hsmClientCertificatePublicKey,

    -- ** HsmConfiguration
    hsmConfiguration_tags,
    hsmConfiguration_hsmIpAddress,
    hsmConfiguration_description,
    hsmConfiguration_hsmPartitionName,
    hsmConfiguration_hsmConfigurationIdentifier,

    -- ** HsmStatus
    hsmStatus_status,
    hsmStatus_hsmClientCertificateIdentifier,
    hsmStatus_hsmConfigurationIdentifier,

    -- ** IPRange
    iPRange_tags,
    iPRange_status,
    iPRange_cidrip,

    -- ** LoggingStatus
    loggingStatus_lastSuccessfulDeliveryTime,
    loggingStatus_s3KeyPrefix,
    loggingStatus_lastFailureMessage,
    loggingStatus_loggingEnabled,
    loggingStatus_logExports,
    loggingStatus_bucketName,
    loggingStatus_lastFailureTime,
    loggingStatus_logDestinationType,

    -- ** MaintenanceTrack
    maintenanceTrack_maintenanceTrackName,
    maintenanceTrack_databaseVersion,
    maintenanceTrack_updateTargets,

    -- ** NetworkInterface
    networkInterface_subnetId,
    networkInterface_availabilityZone,
    networkInterface_networkInterfaceId,
    networkInterface_privateIpAddress,

    -- ** NodeConfigurationOption
    nodeConfigurationOption_estimatedDiskUtilizationPercent,
    nodeConfigurationOption_nodeType,
    nodeConfigurationOption_numberOfNodes,
    nodeConfigurationOption_mode,

    -- ** NodeConfigurationOptionsFilter
    nodeConfigurationOptionsFilter_name,
    nodeConfigurationOptionsFilter_operator,
    nodeConfigurationOptionsFilter_values,

    -- ** OrderableClusterOption
    orderableClusterOption_clusterVersion,
    orderableClusterOption_availabilityZones,
    orderableClusterOption_nodeType,
    orderableClusterOption_clusterType,

    -- ** Parameter
    parameter_parameterValue,
    parameter_applyType,
    parameter_isModifiable,
    parameter_description,
    parameter_parameterName,
    parameter_minimumEngineVersion,
    parameter_source,
    parameter_allowedValues,
    parameter_dataType,

    -- ** PartnerIntegrationInfo
    partnerIntegrationInfo_databaseName,
    partnerIntegrationInfo_status,
    partnerIntegrationInfo_partnerName,
    partnerIntegrationInfo_statusMessage,
    partnerIntegrationInfo_createdAt,
    partnerIntegrationInfo_updatedAt,

    -- ** PartnerIntegrationInputMessage
    partnerIntegrationInputMessage_accountId,
    partnerIntegrationInputMessage_clusterIdentifier,
    partnerIntegrationInputMessage_databaseName,
    partnerIntegrationInputMessage_partnerName,

    -- ** PartnerIntegrationOutputMessage
    partnerIntegrationOutputMessage_databaseName,
    partnerIntegrationOutputMessage_partnerName,

    -- ** PauseClusterMessage
    pauseClusterMessage_clusterIdentifier,

    -- ** PendingModifiedValues
    pendingModifiedValues_clusterIdentifier,
    pendingModifiedValues_clusterVersion,
    pendingModifiedValues_maintenanceTrackName,
    pendingModifiedValues_masterUserPassword,
    pendingModifiedValues_nodeType,
    pendingModifiedValues_publiclyAccessible,
    pendingModifiedValues_encryptionType,
    pendingModifiedValues_numberOfNodes,
    pendingModifiedValues_enhancedVpcRouting,
    pendingModifiedValues_clusterType,
    pendingModifiedValues_automatedSnapshotRetentionPeriod,

    -- ** RecurringCharge
    recurringCharge_recurringChargeAmount,
    recurringCharge_recurringChargeFrequency,

    -- ** ReservedNode
    reservedNode_recurringCharges,
    reservedNode_nodeCount,
    reservedNode_state,
    reservedNode_offeringType,
    reservedNode_reservedNodeId,
    reservedNode_nodeType,
    reservedNode_duration,
    reservedNode_currencyCode,
    reservedNode_reservedNodeOfferingType,
    reservedNode_reservedNodeOfferingId,
    reservedNode_fixedPrice,
    reservedNode_startTime,
    reservedNode_usagePrice,

    -- ** ReservedNodeConfigurationOption
    reservedNodeConfigurationOption_targetReservedNodeCount,
    reservedNodeConfigurationOption_sourceReservedNode,
    reservedNodeConfigurationOption_targetReservedNodeOffering,

    -- ** ReservedNodeExchangeStatus
    reservedNodeExchangeStatus_targetReservedNodeCount,
    reservedNodeExchangeStatus_targetReservedNodeType,
    reservedNodeExchangeStatus_reservedNodeExchangeRequestId,
    reservedNodeExchangeStatus_requestTime,
    reservedNodeExchangeStatus_status,
    reservedNodeExchangeStatus_sourceReservedNodeId,
    reservedNodeExchangeStatus_targetReservedNodeOfferingId,
    reservedNodeExchangeStatus_sourceReservedNodeCount,
    reservedNodeExchangeStatus_sourceReservedNodeType,

    -- ** ReservedNodeOffering
    reservedNodeOffering_recurringCharges,
    reservedNodeOffering_offeringType,
    reservedNodeOffering_nodeType,
    reservedNodeOffering_duration,
    reservedNodeOffering_currencyCode,
    reservedNodeOffering_reservedNodeOfferingType,
    reservedNodeOffering_reservedNodeOfferingId,
    reservedNodeOffering_fixedPrice,
    reservedNodeOffering_usagePrice,

    -- ** ResizeClusterMessage
    resizeClusterMessage_reservedNodeId,
    resizeClusterMessage_nodeType,
    resizeClusterMessage_classic,
    resizeClusterMessage_numberOfNodes,
    resizeClusterMessage_clusterType,
    resizeClusterMessage_targetReservedNodeOfferingId,
    resizeClusterMessage_clusterIdentifier,

    -- ** ResizeInfo
    resizeInfo_allowCancelResize,
    resizeInfo_resizeType,

    -- ** ResizeProgressMessage
    resizeProgressMessage_message,
    resizeProgressMessage_targetEncryptionType,
    resizeProgressMessage_avgResizeRateInMegaBytesPerSecond,
    resizeProgressMessage_targetClusterType,
    resizeProgressMessage_dataTransferProgressPercent,
    resizeProgressMessage_targetNumberOfNodes,
    resizeProgressMessage_totalResizeDataInMegaBytes,
    resizeProgressMessage_importTablesNotStarted,
    resizeProgressMessage_status,
    resizeProgressMessage_elapsedTimeInSeconds,
    resizeProgressMessage_importTablesCompleted,
    resizeProgressMessage_progressInMegaBytes,
    resizeProgressMessage_importTablesInProgress,
    resizeProgressMessage_estimatedTimeToCompletionInSeconds,
    resizeProgressMessage_targetNodeType,
    resizeProgressMessage_resizeType,

    -- ** RestoreStatus
    restoreStatus_snapshotSizeInMegaBytes,
    restoreStatus_currentRestoreRateInMegaBytesPerSecond,
    restoreStatus_status,
    restoreStatus_elapsedTimeInSeconds,
    restoreStatus_progressInMegaBytes,
    restoreStatus_estimatedTimeToCompletionInSeconds,

    -- ** ResumeClusterMessage
    resumeClusterMessage_clusterIdentifier,

    -- ** RevisionTarget
    revisionTarget_description,
    revisionTarget_databaseRevision,
    revisionTarget_databaseRevisionReleaseDate,

    -- ** ScheduledAction
    scheduledAction_schedule,
    scheduledAction_targetAction,
    scheduledAction_iamRole,
    scheduledAction_state,
    scheduledAction_nextInvocations,
    scheduledAction_endTime,
    scheduledAction_scheduledActionDescription,
    scheduledAction_scheduledActionName,
    scheduledAction_startTime,

    -- ** ScheduledActionFilter
    scheduledActionFilter_name,
    scheduledActionFilter_values,

    -- ** ScheduledActionType
    scheduledActionType_resizeCluster,
    scheduledActionType_resumeCluster,
    scheduledActionType_pauseCluster,

    -- ** Snapshot
    snapshot_tags,
    snapshot_port,
    snapshot_clusterIdentifier,
    snapshot_manualSnapshotRemainingDays,
    snapshot_manualSnapshotRetentionPeriod,
    snapshot_masterUsername,
    snapshot_sourceRegion,
    snapshot_currentBackupRateInMegaBytesPerSecond,
    snapshot_engineFullVersion,
    snapshot_snapshotIdentifier,
    snapshot_restorableNodeTypes,
    snapshot_clusterVersion,
    snapshot_maintenanceTrackName,
    snapshot_encryptedWithHSM,
    snapshot_status,
    snapshot_elapsedTimeInSeconds,
    snapshot_snapshotRetentionStartTime,
    snapshot_availabilityZone,
    snapshot_snapshotCreateTime,
    snapshot_nodeType,
    snapshot_encrypted,
    snapshot_estimatedSecondsToCompletion,
    snapshot_numberOfNodes,
    snapshot_kmsKeyId,
    snapshot_enhancedVpcRouting,
    snapshot_vpcId,
    snapshot_totalBackupSizeInMegaBytes,
    snapshot_accountsWithRestoreAccess,
    snapshot_ownerAccount,
    snapshot_clusterCreateTime,
    snapshot_backupProgressInMegaBytes,
    snapshot_dbName,
    snapshot_actualIncrementalBackupSizeInMegaBytes,
    snapshot_snapshotType,

    -- ** SnapshotCopyGrant
    snapshotCopyGrant_tags,
    snapshotCopyGrant_snapshotCopyGrantName,
    snapshotCopyGrant_kmsKeyId,

    -- ** SnapshotErrorMessage
    snapshotErrorMessage_failureCode,
    snapshotErrorMessage_snapshotIdentifier,
    snapshotErrorMessage_snapshotClusterIdentifier,
    snapshotErrorMessage_failureReason,

    -- ** SnapshotSchedule
    snapshotSchedule_tags,
    snapshotSchedule_scheduleDescription,
    snapshotSchedule_associatedClusters,
    snapshotSchedule_scheduleIdentifier,
    snapshotSchedule_nextInvocations,
    snapshotSchedule_associatedClusterCount,
    snapshotSchedule_scheduleDefinitions,

    -- ** SnapshotSortingEntity
    snapshotSortingEntity_sortOrder,
    snapshotSortingEntity_attribute,

    -- ** Subnet
    subnet_subnetIdentifier,
    subnet_subnetStatus,
    subnet_subnetAvailabilityZone,

    -- ** SupportedOperation
    supportedOperation_operationName,

    -- ** SupportedPlatform
    supportedPlatform_name,

    -- ** TableRestoreStatus
    tableRestoreStatus_totalDataInMegaBytes,
    tableRestoreStatus_clusterIdentifier,
    tableRestoreStatus_message,
    tableRestoreStatus_newTableName,
    tableRestoreStatus_targetSchemaName,
    tableRestoreStatus_requestTime,
    tableRestoreStatus_snapshotIdentifier,
    tableRestoreStatus_sourceDatabaseName,
    tableRestoreStatus_sourceSchemaName,
    tableRestoreStatus_tableRestoreRequestId,
    tableRestoreStatus_targetDatabaseName,
    tableRestoreStatus_status,
    tableRestoreStatus_progressInMegaBytes,
    tableRestoreStatus_sourceTableName,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TaggedResource
    taggedResource_resourceType,
    taggedResource_resourceName,
    taggedResource_tag,

    -- ** UpdateTarget
    updateTarget_supportedOperations,
    updateTarget_maintenanceTrackName,
    updateTarget_databaseVersion,

    -- ** UsageLimit
    usageLimit_tags,
    usageLimit_clusterIdentifier,
    usageLimit_usageLimitId,
    usageLimit_featureType,
    usageLimit_period,
    usageLimit_breachAction,
    usageLimit_amount,
    usageLimit_limitType,

    -- ** VpcEndpoint
    vpcEndpoint_vpcEndpointId,
    vpcEndpoint_vpcId,
    vpcEndpoint_networkInterfaces,

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
import Amazonka.Redshift.DescribeReservedNodeExchangeStatus
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
import Amazonka.Redshift.GetClusterCredentialsWithIAM
import Amazonka.Redshift.GetReservedNodeExchangeConfigurationOptions
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
import Amazonka.Redshift.Types.ReservedNodeConfigurationOption
import Amazonka.Redshift.Types.ReservedNodeExchangeStatus
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
