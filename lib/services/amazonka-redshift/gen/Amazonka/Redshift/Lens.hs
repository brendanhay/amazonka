{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Redshift.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    dataShare_allowPubliclyAccessibleConsumers,
    dataShare_dataShareArn,
    dataShare_dataShareAssociations,
    dataShare_managedBy,
    dataShare_producerArn,

    -- ** AuthorizeClusterSecurityGroupIngress
    authorizeClusterSecurityGroupIngress_cidrip,
    authorizeClusterSecurityGroupIngress_eC2SecurityGroupName,
    authorizeClusterSecurityGroupIngress_eC2SecurityGroupOwnerId,
    authorizeClusterSecurityGroupIngress_clusterSecurityGroupName,
    authorizeClusterSecurityGroupIngressResponse_clusterSecurityGroup,
    authorizeClusterSecurityGroupIngressResponse_httpStatus,

    -- ** AuthorizeDataShare
    authorizeDataShare_dataShareArn,
    authorizeDataShare_consumerIdentifier,
    dataShare_allowPubliclyAccessibleConsumers,
    dataShare_dataShareArn,
    dataShare_dataShareAssociations,
    dataShare_managedBy,
    dataShare_producerArn,

    -- ** AuthorizeEndpointAccess
    authorizeEndpointAccess_clusterIdentifier,
    authorizeEndpointAccess_vpcIds,
    authorizeEndpointAccess_account,
    endpointAuthorization_allowedAllVPCs,
    endpointAuthorization_allowedVPCs,
    endpointAuthorization_authorizeTime,
    endpointAuthorization_clusterIdentifier,
    endpointAuthorization_clusterStatus,
    endpointAuthorization_endpointCount,
    endpointAuthorization_grantee,
    endpointAuthorization_grantor,
    endpointAuthorization_status,

    -- ** AuthorizeSnapshotAccess
    authorizeSnapshotAccess_snapshotArn,
    authorizeSnapshotAccess_snapshotClusterIdentifier,
    authorizeSnapshotAccess_snapshotIdentifier,
    authorizeSnapshotAccess_accountWithRestoreAccess,
    authorizeSnapshotAccessResponse_snapshot,
    authorizeSnapshotAccessResponse_httpStatus,

    -- ** BatchDeleteClusterSnapshots
    batchDeleteClusterSnapshots_identifiers,
    batchDeleteClusterSnapshotsResponse_errors,
    batchDeleteClusterSnapshotsResponse_resources,
    batchDeleteClusterSnapshotsResponse_httpStatus,

    -- ** BatchModifyClusterSnapshots
    batchModifyClusterSnapshots_force,
    batchModifyClusterSnapshots_manualSnapshotRetentionPeriod,
    batchModifyClusterSnapshots_snapshotIdentifierList,
    batchModifyClusterSnapshotsResponse_errors,
    batchModifyClusterSnapshotsResponse_resources,
    batchModifyClusterSnapshotsResponse_httpStatus,

    -- ** CancelResize
    cancelResize_clusterIdentifier,
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

    -- ** CopyClusterSnapshot
    copyClusterSnapshot_manualSnapshotRetentionPeriod,
    copyClusterSnapshot_sourceSnapshotClusterIdentifier,
    copyClusterSnapshot_sourceSnapshotIdentifier,
    copyClusterSnapshot_targetSnapshotIdentifier,
    copyClusterSnapshotResponse_snapshot,
    copyClusterSnapshotResponse_httpStatus,

    -- ** CreateAuthenticationProfile
    createAuthenticationProfile_authenticationProfileName,
    createAuthenticationProfile_authenticationProfileContent,
    createAuthenticationProfileResponse_authenticationProfileContent,
    createAuthenticationProfileResponse_authenticationProfileName,
    createAuthenticationProfileResponse_httpStatus,

    -- ** CreateCluster
    createCluster_additionalInfo,
    createCluster_allowVersionUpgrade,
    createCluster_aquaConfigurationStatus,
    createCluster_automatedSnapshotRetentionPeriod,
    createCluster_availabilityZone,
    createCluster_availabilityZoneRelocation,
    createCluster_clusterParameterGroupName,
    createCluster_clusterSecurityGroups,
    createCluster_clusterSubnetGroupName,
    createCluster_clusterType,
    createCluster_clusterVersion,
    createCluster_dbName,
    createCluster_defaultIamRoleArn,
    createCluster_elasticIp,
    createCluster_encrypted,
    createCluster_enhancedVpcRouting,
    createCluster_hsmClientCertificateIdentifier,
    createCluster_hsmConfigurationIdentifier,
    createCluster_iamRoles,
    createCluster_kmsKeyId,
    createCluster_loadSampleData,
    createCluster_maintenanceTrackName,
    createCluster_manualSnapshotRetentionPeriod,
    createCluster_numberOfNodes,
    createCluster_port,
    createCluster_preferredMaintenanceWindow,
    createCluster_publiclyAccessible,
    createCluster_snapshotScheduleIdentifier,
    createCluster_tags,
    createCluster_vpcSecurityGroupIds,
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
    createClusterSnapshot_manualSnapshotRetentionPeriod,
    createClusterSnapshot_tags,
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

    -- ** CreateCustomDomainAssociation
    createCustomDomainAssociation_customDomainName,
    createCustomDomainAssociation_customDomainCertificateArn,
    createCustomDomainAssociation_clusterIdentifier,
    createCustomDomainAssociationResponse_clusterIdentifier,
    createCustomDomainAssociationResponse_customDomainCertExpiryTime,
    createCustomDomainAssociationResponse_customDomainCertificateArn,
    createCustomDomainAssociationResponse_customDomainName,
    createCustomDomainAssociationResponse_httpStatus,

    -- ** CreateEndpointAccess
    createEndpointAccess_clusterIdentifier,
    createEndpointAccess_resourceOwner,
    createEndpointAccess_vpcSecurityGroupIds,
    createEndpointAccess_endpointName,
    createEndpointAccess_subnetGroupName,
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

    -- ** CreateEventSubscription
    createEventSubscription_enabled,
    createEventSubscription_eventCategories,
    createEventSubscription_severity,
    createEventSubscription_sourceIds,
    createEventSubscription_sourceType,
    createEventSubscription_tags,
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
    scheduledAction_endTime,
    scheduledAction_iamRole,
    scheduledAction_nextInvocations,
    scheduledAction_schedule,
    scheduledAction_scheduledActionDescription,
    scheduledAction_scheduledActionName,
    scheduledAction_startTime,
    scheduledAction_state,
    scheduledAction_targetAction,

    -- ** CreateSnapshotCopyGrant
    createSnapshotCopyGrant_kmsKeyId,
    createSnapshotCopyGrant_tags,
    createSnapshotCopyGrant_snapshotCopyGrantName,
    createSnapshotCopyGrantResponse_snapshotCopyGrant,
    createSnapshotCopyGrantResponse_httpStatus,

    -- ** CreateSnapshotSchedule
    createSnapshotSchedule_dryRun,
    createSnapshotSchedule_nextInvocations,
    createSnapshotSchedule_scheduleDefinitions,
    createSnapshotSchedule_scheduleDescription,
    createSnapshotSchedule_scheduleIdentifier,
    createSnapshotSchedule_tags,
    snapshotSchedule_associatedClusterCount,
    snapshotSchedule_associatedClusters,
    snapshotSchedule_nextInvocations,
    snapshotSchedule_scheduleDefinitions,
    snapshotSchedule_scheduleDescription,
    snapshotSchedule_scheduleIdentifier,
    snapshotSchedule_tags,

    -- ** CreateTags
    createTags_resourceName,
    createTags_tags,

    -- ** CreateUsageLimit
    createUsageLimit_breachAction,
    createUsageLimit_period,
    createUsageLimit_tags,
    createUsageLimit_clusterIdentifier,
    createUsageLimit_featureType,
    createUsageLimit_limitType,
    createUsageLimit_amount,
    usageLimit_amount,
    usageLimit_breachAction,
    usageLimit_clusterIdentifier,
    usageLimit_featureType,
    usageLimit_limitType,
    usageLimit_period,
    usageLimit_tags,
    usageLimit_usageLimitId,

    -- ** DeauthorizeDataShare
    deauthorizeDataShare_dataShareArn,
    deauthorizeDataShare_consumerIdentifier,
    dataShare_allowPubliclyAccessibleConsumers,
    dataShare_dataShareArn,
    dataShare_dataShareAssociations,
    dataShare_managedBy,
    dataShare_producerArn,

    -- ** DeleteAuthenticationProfile
    deleteAuthenticationProfile_authenticationProfileName,
    deleteAuthenticationProfileResponse_authenticationProfileName,
    deleteAuthenticationProfileResponse_httpStatus,

    -- ** DeleteCluster
    deleteCluster_finalClusterSnapshotIdentifier,
    deleteCluster_finalClusterSnapshotRetentionPeriod,
    deleteCluster_skipFinalClusterSnapshot,
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

    -- ** DeleteCustomDomainAssociation
    deleteCustomDomainAssociation_clusterIdentifier,

    -- ** DeleteEndpointAccess
    deleteEndpointAccess_endpointName,
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
    describeClusterParameterGroups_marker,
    describeClusterParameterGroups_maxRecords,
    describeClusterParameterGroups_parameterGroupName,
    describeClusterParameterGroups_tagKeys,
    describeClusterParameterGroups_tagValues,
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
    describeClusterSecurityGroups_clusterSecurityGroupName,
    describeClusterSecurityGroups_marker,
    describeClusterSecurityGroups_maxRecords,
    describeClusterSecurityGroups_tagKeys,
    describeClusterSecurityGroups_tagValues,
    describeClusterSecurityGroupsResponse_clusterSecurityGroups,
    describeClusterSecurityGroupsResponse_marker,
    describeClusterSecurityGroupsResponse_httpStatus,

    -- ** DescribeClusterSnapshots
    describeClusterSnapshots_clusterExists,
    describeClusterSnapshots_clusterIdentifier,
    describeClusterSnapshots_endTime,
    describeClusterSnapshots_marker,
    describeClusterSnapshots_maxRecords,
    describeClusterSnapshots_ownerAccount,
    describeClusterSnapshots_snapshotArn,
    describeClusterSnapshots_snapshotIdentifier,
    describeClusterSnapshots_snapshotType,
    describeClusterSnapshots_sortingEntities,
    describeClusterSnapshots_startTime,
    describeClusterSnapshots_tagKeys,
    describeClusterSnapshots_tagValues,
    describeClusterSnapshotsResponse_marker,
    describeClusterSnapshotsResponse_snapshots,
    describeClusterSnapshotsResponse_httpStatus,

    -- ** DescribeClusterSubnetGroups
    describeClusterSubnetGroups_clusterSubnetGroupName,
    describeClusterSubnetGroups_marker,
    describeClusterSubnetGroups_maxRecords,
    describeClusterSubnetGroups_tagKeys,
    describeClusterSubnetGroups_tagValues,
    describeClusterSubnetGroupsResponse_clusterSubnetGroups,
    describeClusterSubnetGroupsResponse_marker,
    describeClusterSubnetGroupsResponse_httpStatus,

    -- ** DescribeClusterTracks
    describeClusterTracks_maintenanceTrackName,
    describeClusterTracks_marker,
    describeClusterTracks_maxRecords,
    describeClusterTracksResponse_maintenanceTracks,
    describeClusterTracksResponse_marker,
    describeClusterTracksResponse_httpStatus,

    -- ** DescribeClusterVersions
    describeClusterVersions_clusterParameterGroupFamily,
    describeClusterVersions_clusterVersion,
    describeClusterVersions_marker,
    describeClusterVersions_maxRecords,
    describeClusterVersionsResponse_clusterVersions,
    describeClusterVersionsResponse_marker,
    describeClusterVersionsResponse_httpStatus,

    -- ** DescribeClusters
    describeClusters_clusterIdentifier,
    describeClusters_marker,
    describeClusters_maxRecords,
    describeClusters_tagKeys,
    describeClusters_tagValues,
    describeClustersResponse_clusters,
    describeClustersResponse_marker,
    describeClustersResponse_httpStatus,

    -- ** DescribeCustomDomainAssociations
    describeCustomDomainAssociations_customDomainCertificateArn,
    describeCustomDomainAssociations_customDomainName,
    describeCustomDomainAssociations_marker,
    describeCustomDomainAssociations_maxRecords,
    describeCustomDomainAssociationsResponse_associations,
    describeCustomDomainAssociationsResponse_marker,
    describeCustomDomainAssociationsResponse_httpStatus,

    -- ** DescribeDataShares
    describeDataShares_dataShareArn,
    describeDataShares_marker,
    describeDataShares_maxRecords,
    describeDataSharesResponse_dataShares,
    describeDataSharesResponse_marker,
    describeDataSharesResponse_httpStatus,

    -- ** DescribeDataSharesForConsumer
    describeDataSharesForConsumer_consumerArn,
    describeDataSharesForConsumer_marker,
    describeDataSharesForConsumer_maxRecords,
    describeDataSharesForConsumer_status,
    describeDataSharesForConsumerResponse_dataShares,
    describeDataSharesForConsumerResponse_marker,
    describeDataSharesForConsumerResponse_httpStatus,

    -- ** DescribeDataSharesForProducer
    describeDataSharesForProducer_marker,
    describeDataSharesForProducer_maxRecords,
    describeDataSharesForProducer_producerArn,
    describeDataSharesForProducer_status,
    describeDataSharesForProducerResponse_dataShares,
    describeDataSharesForProducerResponse_marker,
    describeDataSharesForProducerResponse_httpStatus,

    -- ** DescribeDefaultClusterParameters
    describeDefaultClusterParameters_marker,
    describeDefaultClusterParameters_maxRecords,
    describeDefaultClusterParameters_parameterGroupFamily,
    describeDefaultClusterParametersResponse_httpStatus,
    describeDefaultClusterParametersResponse_defaultClusterParameters,

    -- ** DescribeEndpointAccess
    describeEndpointAccess_clusterIdentifier,
    describeEndpointAccess_endpointName,
    describeEndpointAccess_marker,
    describeEndpointAccess_maxRecords,
    describeEndpointAccess_resourceOwner,
    describeEndpointAccess_vpcId,
    describeEndpointAccessResponse_endpointAccessList,
    describeEndpointAccessResponse_marker,
    describeEndpointAccessResponse_httpStatus,

    -- ** DescribeEndpointAuthorization
    describeEndpointAuthorization_account,
    describeEndpointAuthorization_clusterIdentifier,
    describeEndpointAuthorization_grantee,
    describeEndpointAuthorization_marker,
    describeEndpointAuthorization_maxRecords,
    describeEndpointAuthorizationResponse_endpointAuthorizationList,
    describeEndpointAuthorizationResponse_marker,
    describeEndpointAuthorizationResponse_httpStatus,

    -- ** DescribeEventCategories
    describeEventCategories_sourceType,
    describeEventCategoriesResponse_eventCategoriesMapList,
    describeEventCategoriesResponse_httpStatus,

    -- ** DescribeEventSubscriptions
    describeEventSubscriptions_marker,
    describeEventSubscriptions_maxRecords,
    describeEventSubscriptions_subscriptionName,
    describeEventSubscriptions_tagKeys,
    describeEventSubscriptions_tagValues,
    describeEventSubscriptionsResponse_eventSubscriptionsList,
    describeEventSubscriptionsResponse_marker,
    describeEventSubscriptionsResponse_httpStatus,

    -- ** DescribeEvents
    describeEvents_duration,
    describeEvents_endTime,
    describeEvents_marker,
    describeEvents_maxRecords,
    describeEvents_sourceIdentifier,
    describeEvents_sourceType,
    describeEvents_startTime,
    describeEventsResponse_events,
    describeEventsResponse_marker,
    describeEventsResponse_httpStatus,

    -- ** DescribeHsmClientCertificates
    describeHsmClientCertificates_hsmClientCertificateIdentifier,
    describeHsmClientCertificates_marker,
    describeHsmClientCertificates_maxRecords,
    describeHsmClientCertificates_tagKeys,
    describeHsmClientCertificates_tagValues,
    describeHsmClientCertificatesResponse_hsmClientCertificates,
    describeHsmClientCertificatesResponse_marker,
    describeHsmClientCertificatesResponse_httpStatus,

    -- ** DescribeHsmConfigurations
    describeHsmConfigurations_hsmConfigurationIdentifier,
    describeHsmConfigurations_marker,
    describeHsmConfigurations_maxRecords,
    describeHsmConfigurations_tagKeys,
    describeHsmConfigurations_tagValues,
    describeHsmConfigurationsResponse_hsmConfigurations,
    describeHsmConfigurationsResponse_marker,
    describeHsmConfigurationsResponse_httpStatus,

    -- ** DescribeLoggingStatus
    describeLoggingStatus_clusterIdentifier,
    loggingStatus_bucketName,
    loggingStatus_lastFailureMessage,
    loggingStatus_lastFailureTime,
    loggingStatus_lastSuccessfulDeliveryTime,
    loggingStatus_logDestinationType,
    loggingStatus_logExports,
    loggingStatus_loggingEnabled,
    loggingStatus_s3KeyPrefix,

    -- ** DescribeNodeConfigurationOptions
    describeNodeConfigurationOptions_clusterIdentifier,
    describeNodeConfigurationOptions_filters,
    describeNodeConfigurationOptions_marker,
    describeNodeConfigurationOptions_maxRecords,
    describeNodeConfigurationOptions_ownerAccount,
    describeNodeConfigurationOptions_snapshotArn,
    describeNodeConfigurationOptions_snapshotIdentifier,
    describeNodeConfigurationOptions_actionType,
    describeNodeConfigurationOptionsResponse_marker,
    describeNodeConfigurationOptionsResponse_nodeConfigurationOptionList,
    describeNodeConfigurationOptionsResponse_httpStatus,

    -- ** DescribeOrderableClusterOptions
    describeOrderableClusterOptions_clusterVersion,
    describeOrderableClusterOptions_marker,
    describeOrderableClusterOptions_maxRecords,
    describeOrderableClusterOptions_nodeType,
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
    describeReservedNodeExchangeStatus_maxRecords,
    describeReservedNodeExchangeStatus_reservedNodeExchangeRequestId,
    describeReservedNodeExchangeStatus_reservedNodeId,
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
    describeReservedNodes_maxRecords,
    describeReservedNodes_reservedNodeId,
    describeReservedNodesResponse_marker,
    describeReservedNodesResponse_reservedNodes,
    describeReservedNodesResponse_httpStatus,

    -- ** DescribeResize
    describeResize_clusterIdentifier,
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

    -- ** DescribeScheduledActions
    describeScheduledActions_active,
    describeScheduledActions_endTime,
    describeScheduledActions_filters,
    describeScheduledActions_marker,
    describeScheduledActions_maxRecords,
    describeScheduledActions_scheduledActionName,
    describeScheduledActions_startTime,
    describeScheduledActions_targetActionType,
    describeScheduledActionsResponse_marker,
    describeScheduledActionsResponse_scheduledActions,
    describeScheduledActionsResponse_httpStatus,

    -- ** DescribeSnapshotCopyGrants
    describeSnapshotCopyGrants_marker,
    describeSnapshotCopyGrants_maxRecords,
    describeSnapshotCopyGrants_snapshotCopyGrantName,
    describeSnapshotCopyGrants_tagKeys,
    describeSnapshotCopyGrants_tagValues,
    describeSnapshotCopyGrantsResponse_marker,
    describeSnapshotCopyGrantsResponse_snapshotCopyGrants,
    describeSnapshotCopyGrantsResponse_httpStatus,

    -- ** DescribeSnapshotSchedules
    describeSnapshotSchedules_clusterIdentifier,
    describeSnapshotSchedules_marker,
    describeSnapshotSchedules_maxRecords,
    describeSnapshotSchedules_scheduleIdentifier,
    describeSnapshotSchedules_tagKeys,
    describeSnapshotSchedules_tagValues,
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
    describeTableRestoreStatus_maxRecords,
    describeTableRestoreStatus_tableRestoreRequestId,
    describeTableRestoreStatusResponse_marker,
    describeTableRestoreStatusResponse_tableRestoreStatusDetails,
    describeTableRestoreStatusResponse_httpStatus,

    -- ** DescribeTags
    describeTags_marker,
    describeTags_maxRecords,
    describeTags_resourceName,
    describeTags_resourceType,
    describeTags_tagKeys,
    describeTags_tagValues,
    describeTagsResponse_marker,
    describeTagsResponse_taggedResources,
    describeTagsResponse_httpStatus,

    -- ** DescribeUsageLimits
    describeUsageLimits_clusterIdentifier,
    describeUsageLimits_featureType,
    describeUsageLimits_marker,
    describeUsageLimits_maxRecords,
    describeUsageLimits_tagKeys,
    describeUsageLimits_tagValues,
    describeUsageLimits_usageLimitId,
    describeUsageLimitsResponse_marker,
    describeUsageLimitsResponse_usageLimits,
    describeUsageLimitsResponse_httpStatus,

    -- ** DisableLogging
    disableLogging_clusterIdentifier,
    loggingStatus_bucketName,
    loggingStatus_lastFailureMessage,
    loggingStatus_lastFailureTime,
    loggingStatus_lastSuccessfulDeliveryTime,
    loggingStatus_logDestinationType,
    loggingStatus_logExports,
    loggingStatus_loggingEnabled,
    loggingStatus_s3KeyPrefix,

    -- ** DisableSnapshotCopy
    disableSnapshotCopy_clusterIdentifier,
    disableSnapshotCopyResponse_cluster,
    disableSnapshotCopyResponse_httpStatus,

    -- ** DisassociateDataShareConsumer
    disassociateDataShareConsumer_consumerArn,
    disassociateDataShareConsumer_consumerRegion,
    disassociateDataShareConsumer_disassociateEntireAccount,
    disassociateDataShareConsumer_dataShareArn,
    dataShare_allowPubliclyAccessibleConsumers,
    dataShare_dataShareArn,
    dataShare_dataShareAssociations,
    dataShare_managedBy,
    dataShare_producerArn,

    -- ** EnableLogging
    enableLogging_bucketName,
    enableLogging_logDestinationType,
    enableLogging_logExports,
    enableLogging_s3KeyPrefix,
    enableLogging_clusterIdentifier,
    loggingStatus_bucketName,
    loggingStatus_lastFailureMessage,
    loggingStatus_lastFailureTime,
    loggingStatus_lastSuccessfulDeliveryTime,
    loggingStatus_logDestinationType,
    loggingStatus_logExports,
    loggingStatus_loggingEnabled,
    loggingStatus_s3KeyPrefix,

    -- ** EnableSnapshotCopy
    enableSnapshotCopy_manualSnapshotRetentionPeriod,
    enableSnapshotCopy_retentionPeriod,
    enableSnapshotCopy_snapshotCopyGrantName,
    enableSnapshotCopy_clusterIdentifier,
    enableSnapshotCopy_destinationRegion,
    enableSnapshotCopyResponse_cluster,
    enableSnapshotCopyResponse_httpStatus,

    -- ** GetClusterCredentials
    getClusterCredentials_autoCreate,
    getClusterCredentials_clusterIdentifier,
    getClusterCredentials_customDomainName,
    getClusterCredentials_dbGroups,
    getClusterCredentials_dbName,
    getClusterCredentials_durationSeconds,
    getClusterCredentials_dbUser,
    getClusterCredentialsResponse_dbPassword,
    getClusterCredentialsResponse_dbUser,
    getClusterCredentialsResponse_expiration,
    getClusterCredentialsResponse_httpStatus,

    -- ** GetClusterCredentialsWithIAM
    getClusterCredentialsWithIAM_clusterIdentifier,
    getClusterCredentialsWithIAM_customDomainName,
    getClusterCredentialsWithIAM_dbName,
    getClusterCredentialsWithIAM_durationSeconds,
    getClusterCredentialsWithIAMResponse_dbPassword,
    getClusterCredentialsWithIAMResponse_dbUser,
    getClusterCredentialsWithIAMResponse_expiration,
    getClusterCredentialsWithIAMResponse_nextRefreshTime,
    getClusterCredentialsWithIAMResponse_httpStatus,

    -- ** GetReservedNodeExchangeConfigurationOptions
    getReservedNodeExchangeConfigurationOptions_clusterIdentifier,
    getReservedNodeExchangeConfigurationOptions_marker,
    getReservedNodeExchangeConfigurationOptions_maxRecords,
    getReservedNodeExchangeConfigurationOptions_snapshotIdentifier,
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
    modifyAuthenticationProfileResponse_authenticationProfileContent,
    modifyAuthenticationProfileResponse_authenticationProfileName,
    modifyAuthenticationProfileResponse_httpStatus,

    -- ** ModifyCluster
    modifyCluster_allowVersionUpgrade,
    modifyCluster_automatedSnapshotRetentionPeriod,
    modifyCluster_availabilityZone,
    modifyCluster_availabilityZoneRelocation,
    modifyCluster_clusterParameterGroupName,
    modifyCluster_clusterSecurityGroups,
    modifyCluster_clusterType,
    modifyCluster_clusterVersion,
    modifyCluster_elasticIp,
    modifyCluster_encrypted,
    modifyCluster_enhancedVpcRouting,
    modifyCluster_hsmClientCertificateIdentifier,
    modifyCluster_hsmConfigurationIdentifier,
    modifyCluster_kmsKeyId,
    modifyCluster_maintenanceTrackName,
    modifyCluster_manualSnapshotRetentionPeriod,
    modifyCluster_masterUserPassword,
    modifyCluster_newClusterIdentifier,
    modifyCluster_nodeType,
    modifyCluster_numberOfNodes,
    modifyCluster_port,
    modifyCluster_preferredMaintenanceWindow,
    modifyCluster_publiclyAccessible,
    modifyCluster_vpcSecurityGroupIds,
    modifyCluster_clusterIdentifier,
    modifyClusterResponse_cluster,
    modifyClusterResponse_httpStatus,

    -- ** ModifyClusterDbRevision
    modifyClusterDbRevision_clusterIdentifier,
    modifyClusterDbRevision_revisionTarget,
    modifyClusterDbRevisionResponse_cluster,
    modifyClusterDbRevisionResponse_httpStatus,

    -- ** ModifyClusterIamRoles
    modifyClusterIamRoles_addIamRoles,
    modifyClusterIamRoles_defaultIamRoleArn,
    modifyClusterIamRoles_removeIamRoles,
    modifyClusterIamRoles_clusterIdentifier,
    modifyClusterIamRolesResponse_cluster,
    modifyClusterIamRolesResponse_httpStatus,

    -- ** ModifyClusterMaintenance
    modifyClusterMaintenance_deferMaintenance,
    modifyClusterMaintenance_deferMaintenanceDuration,
    modifyClusterMaintenance_deferMaintenanceEndTime,
    modifyClusterMaintenance_deferMaintenanceIdentifier,
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
    modifyClusterSnapshot_force,
    modifyClusterSnapshot_manualSnapshotRetentionPeriod,
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

    -- ** ModifyCustomDomainAssociation
    modifyCustomDomainAssociation_customDomainCertificateArn,
    modifyCustomDomainAssociation_customDomainName,
    modifyCustomDomainAssociation_clusterIdentifier,
    modifyCustomDomainAssociationResponse_clusterIdentifier,
    modifyCustomDomainAssociationResponse_customDomainCertExpiryTime,
    modifyCustomDomainAssociationResponse_customDomainCertificateArn,
    modifyCustomDomainAssociationResponse_customDomainName,
    modifyCustomDomainAssociationResponse_httpStatus,

    -- ** ModifyEndpointAccess
    modifyEndpointAccess_vpcSecurityGroupIds,
    modifyEndpointAccess_endpointName,
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

    -- ** ModifyEventSubscription
    modifyEventSubscription_enabled,
    modifyEventSubscription_eventCategories,
    modifyEventSubscription_severity,
    modifyEventSubscription_snsTopicArn,
    modifyEventSubscription_sourceIds,
    modifyEventSubscription_sourceType,
    modifyEventSubscription_subscriptionName,
    modifyEventSubscriptionResponse_eventSubscription,
    modifyEventSubscriptionResponse_httpStatus,

    -- ** ModifyScheduledAction
    modifyScheduledAction_enable,
    modifyScheduledAction_endTime,
    modifyScheduledAction_iamRole,
    modifyScheduledAction_schedule,
    modifyScheduledAction_scheduledActionDescription,
    modifyScheduledAction_startTime,
    modifyScheduledAction_targetAction,
    modifyScheduledAction_scheduledActionName,
    scheduledAction_endTime,
    scheduledAction_iamRole,
    scheduledAction_nextInvocations,
    scheduledAction_schedule,
    scheduledAction_scheduledActionDescription,
    scheduledAction_scheduledActionName,
    scheduledAction_startTime,
    scheduledAction_state,
    scheduledAction_targetAction,

    -- ** ModifySnapshotCopyRetentionPeriod
    modifySnapshotCopyRetentionPeriod_manual,
    modifySnapshotCopyRetentionPeriod_clusterIdentifier,
    modifySnapshotCopyRetentionPeriod_retentionPeriod,
    modifySnapshotCopyRetentionPeriodResponse_cluster,
    modifySnapshotCopyRetentionPeriodResponse_httpStatus,

    -- ** ModifySnapshotSchedule
    modifySnapshotSchedule_scheduleIdentifier,
    modifySnapshotSchedule_scheduleDefinitions,
    snapshotSchedule_associatedClusterCount,
    snapshotSchedule_associatedClusters,
    snapshotSchedule_nextInvocations,
    snapshotSchedule_scheduleDefinitions,
    snapshotSchedule_scheduleDescription,
    snapshotSchedule_scheduleIdentifier,
    snapshotSchedule_tags,

    -- ** ModifyUsageLimit
    modifyUsageLimit_amount,
    modifyUsageLimit_breachAction,
    modifyUsageLimit_usageLimitId,
    usageLimit_amount,
    usageLimit_breachAction,
    usageLimit_clusterIdentifier,
    usageLimit_featureType,
    usageLimit_limitType,
    usageLimit_period,
    usageLimit_tags,
    usageLimit_usageLimitId,

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
    dataShare_allowPubliclyAccessibleConsumers,
    dataShare_dataShareArn,
    dataShare_dataShareAssociations,
    dataShare_managedBy,
    dataShare_producerArn,

    -- ** ResetClusterParameterGroup
    resetClusterParameterGroup_parameters,
    resetClusterParameterGroup_resetAllParameters,
    resetClusterParameterGroup_parameterGroupName,
    clusterParameterGroupNameMessage_parameterGroupName,
    clusterParameterGroupNameMessage_parameterGroupStatus,

    -- ** ResizeCluster
    resizeCluster_classic,
    resizeCluster_clusterType,
    resizeCluster_nodeType,
    resizeCluster_numberOfNodes,
    resizeCluster_reservedNodeId,
    resizeCluster_targetReservedNodeOfferingId,
    resizeCluster_clusterIdentifier,
    resizeClusterResponse_cluster,
    resizeClusterResponse_httpStatus,

    -- ** RestoreFromClusterSnapshot
    restoreFromClusterSnapshot_additionalInfo,
    restoreFromClusterSnapshot_allowVersionUpgrade,
    restoreFromClusterSnapshot_aquaConfigurationStatus,
    restoreFromClusterSnapshot_automatedSnapshotRetentionPeriod,
    restoreFromClusterSnapshot_availabilityZone,
    restoreFromClusterSnapshot_availabilityZoneRelocation,
    restoreFromClusterSnapshot_clusterParameterGroupName,
    restoreFromClusterSnapshot_clusterSecurityGroups,
    restoreFromClusterSnapshot_clusterSubnetGroupName,
    restoreFromClusterSnapshot_defaultIamRoleArn,
    restoreFromClusterSnapshot_elasticIp,
    restoreFromClusterSnapshot_encrypted,
    restoreFromClusterSnapshot_enhancedVpcRouting,
    restoreFromClusterSnapshot_hsmClientCertificateIdentifier,
    restoreFromClusterSnapshot_hsmConfigurationIdentifier,
    restoreFromClusterSnapshot_iamRoles,
    restoreFromClusterSnapshot_kmsKeyId,
    restoreFromClusterSnapshot_maintenanceTrackName,
    restoreFromClusterSnapshot_manualSnapshotRetentionPeriod,
    restoreFromClusterSnapshot_nodeType,
    restoreFromClusterSnapshot_numberOfNodes,
    restoreFromClusterSnapshot_ownerAccount,
    restoreFromClusterSnapshot_port,
    restoreFromClusterSnapshot_preferredMaintenanceWindow,
    restoreFromClusterSnapshot_publiclyAccessible,
    restoreFromClusterSnapshot_reservedNodeId,
    restoreFromClusterSnapshot_snapshotArn,
    restoreFromClusterSnapshot_snapshotClusterIdentifier,
    restoreFromClusterSnapshot_snapshotIdentifier,
    restoreFromClusterSnapshot_snapshotScheduleIdentifier,
    restoreFromClusterSnapshot_targetReservedNodeOfferingId,
    restoreFromClusterSnapshot_vpcSecurityGroupIds,
    restoreFromClusterSnapshot_clusterIdentifier,
    restoreFromClusterSnapshotResponse_cluster,
    restoreFromClusterSnapshotResponse_httpStatus,

    -- ** RestoreTableFromClusterSnapshot
    restoreTableFromClusterSnapshot_enableCaseSensitiveIdentifier,
    restoreTableFromClusterSnapshot_sourceSchemaName,
    restoreTableFromClusterSnapshot_targetDatabaseName,
    restoreTableFromClusterSnapshot_targetSchemaName,
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
    revokeClusterSecurityGroupIngress_cidrip,
    revokeClusterSecurityGroupIngress_eC2SecurityGroupName,
    revokeClusterSecurityGroupIngress_eC2SecurityGroupOwnerId,
    revokeClusterSecurityGroupIngress_clusterSecurityGroupName,
    revokeClusterSecurityGroupIngressResponse_clusterSecurityGroup,
    revokeClusterSecurityGroupIngressResponse_httpStatus,

    -- ** RevokeEndpointAccess
    revokeEndpointAccess_account,
    revokeEndpointAccess_clusterIdentifier,
    revokeEndpointAccess_force,
    revokeEndpointAccess_vpcIds,
    endpointAuthorization_allowedAllVPCs,
    endpointAuthorization_allowedVPCs,
    endpointAuthorization_authorizeTime,
    endpointAuthorization_clusterIdentifier,
    endpointAuthorization_clusterStatus,
    endpointAuthorization_endpointCount,
    endpointAuthorization_grantee,
    endpointAuthorization_grantor,
    endpointAuthorization_status,

    -- ** RevokeSnapshotAccess
    revokeSnapshotAccess_snapshotArn,
    revokeSnapshotAccess_snapshotClusterIdentifier,
    revokeSnapshotAccess_snapshotIdentifier,
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
    accountAttribute_attributeName,
    accountAttribute_attributeValues,

    -- ** AccountWithRestoreAccess
    accountWithRestoreAccess_accountAlias,
    accountWithRestoreAccess_accountId,

    -- ** AquaConfiguration
    aquaConfiguration_aquaConfigurationStatus,
    aquaConfiguration_aquaStatus,

    -- ** Association
    association_certificateAssociations,
    association_customDomainCertificateArn,
    association_customDomainCertificateExpiryDate,

    -- ** AttributeValueTarget
    attributeValueTarget_attributeValue,

    -- ** AuthenticationProfile
    authenticationProfile_authenticationProfileContent,
    authenticationProfile_authenticationProfileName,

    -- ** AvailabilityZone
    availabilityZone_name,
    availabilityZone_supportedPlatforms,

    -- ** CertificateAssociation
    certificateAssociation_clusterIdentifier,
    certificateAssociation_customDomainName,

    -- ** Cluster
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
    cluster_customDomainCertificateArn,
    cluster_customDomainCertificateExpiryDate,
    cluster_customDomainName,
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

    -- ** ClusterAssociatedToSchedule
    clusterAssociatedToSchedule_clusterIdentifier,
    clusterAssociatedToSchedule_scheduleAssociationState,

    -- ** ClusterDbRevision
    clusterDbRevision_clusterIdentifier,
    clusterDbRevision_currentDatabaseRevision,
    clusterDbRevision_databaseRevisionReleaseDate,
    clusterDbRevision_revisionTargets,

    -- ** ClusterIamRole
    clusterIamRole_applyStatus,
    clusterIamRole_iamRoleArn,

    -- ** ClusterNode
    clusterNode_nodeRole,
    clusterNode_privateIPAddress,
    clusterNode_publicIPAddress,

    -- ** ClusterParameterGroup
    clusterParameterGroup_description,
    clusterParameterGroup_parameterGroupFamily,
    clusterParameterGroup_parameterGroupName,
    clusterParameterGroup_tags,

    -- ** ClusterParameterGroupNameMessage
    clusterParameterGroupNameMessage_parameterGroupName,
    clusterParameterGroupNameMessage_parameterGroupStatus,

    -- ** ClusterParameterGroupStatus
    clusterParameterGroupStatus_clusterParameterStatusList,
    clusterParameterGroupStatus_parameterApplyStatus,
    clusterParameterGroupStatus_parameterGroupName,

    -- ** ClusterParameterStatus
    clusterParameterStatus_parameterApplyErrorDescription,
    clusterParameterStatus_parameterApplyStatus,
    clusterParameterStatus_parameterName,

    -- ** ClusterSecurityGroup
    clusterSecurityGroup_clusterSecurityGroupName,
    clusterSecurityGroup_description,
    clusterSecurityGroup_eC2SecurityGroups,
    clusterSecurityGroup_iPRanges,
    clusterSecurityGroup_tags,

    -- ** ClusterSecurityGroupMembership
    clusterSecurityGroupMembership_clusterSecurityGroupName,
    clusterSecurityGroupMembership_status,

    -- ** ClusterSnapshotCopyStatus
    clusterSnapshotCopyStatus_destinationRegion,
    clusterSnapshotCopyStatus_manualSnapshotRetentionPeriod,
    clusterSnapshotCopyStatus_retentionPeriod,
    clusterSnapshotCopyStatus_snapshotCopyGrantName,

    -- ** ClusterSubnetGroup
    clusterSubnetGroup_clusterSubnetGroupName,
    clusterSubnetGroup_description,
    clusterSubnetGroup_subnetGroupStatus,
    clusterSubnetGroup_subnets,
    clusterSubnetGroup_tags,
    clusterSubnetGroup_vpcId,

    -- ** ClusterVersion
    clusterVersion_clusterParameterGroupFamily,
    clusterVersion_clusterVersion,
    clusterVersion_description,

    -- ** DataShare
    dataShare_allowPubliclyAccessibleConsumers,
    dataShare_dataShareArn,
    dataShare_dataShareAssociations,
    dataShare_managedBy,
    dataShare_producerArn,

    -- ** DataShareAssociation
    dataShareAssociation_consumerIdentifier,
    dataShareAssociation_consumerRegion,
    dataShareAssociation_createdDate,
    dataShareAssociation_status,
    dataShareAssociation_statusChangeDate,

    -- ** DataTransferProgress
    dataTransferProgress_currentRateInMegaBytesPerSecond,
    dataTransferProgress_dataTransferredInMegaBytes,
    dataTransferProgress_elapsedTimeInSeconds,
    dataTransferProgress_estimatedTimeToCompletionInSeconds,
    dataTransferProgress_status,
    dataTransferProgress_totalDataInMegaBytes,

    -- ** DefaultClusterParameters
    defaultClusterParameters_marker,
    defaultClusterParameters_parameterGroupFamily,
    defaultClusterParameters_parameters,

    -- ** DeferredMaintenanceWindow
    deferredMaintenanceWindow_deferMaintenanceEndTime,
    deferredMaintenanceWindow_deferMaintenanceIdentifier,
    deferredMaintenanceWindow_deferMaintenanceStartTime,

    -- ** DeleteClusterSnapshotMessage
    deleteClusterSnapshotMessage_snapshotClusterIdentifier,
    deleteClusterSnapshotMessage_snapshotIdentifier,

    -- ** EC2SecurityGroup
    eC2SecurityGroup_eC2SecurityGroupName,
    eC2SecurityGroup_eC2SecurityGroupOwnerId,
    eC2SecurityGroup_status,
    eC2SecurityGroup_tags,

    -- ** ElasticIpStatus
    elasticIpStatus_elasticIp,
    elasticIpStatus_status,

    -- ** Endpoint
    endpoint_address,
    endpoint_port,
    endpoint_vpcEndpoints,

    -- ** EndpointAccess
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

    -- ** EndpointAuthorization
    endpointAuthorization_allowedAllVPCs,
    endpointAuthorization_allowedVPCs,
    endpointAuthorization_authorizeTime,
    endpointAuthorization_clusterIdentifier,
    endpointAuthorization_clusterStatus,
    endpointAuthorization_endpointCount,
    endpointAuthorization_grantee,
    endpointAuthorization_grantor,
    endpointAuthorization_status,

    -- ** Event
    event_date,
    event_eventCategories,
    event_eventId,
    event_message,
    event_severity,
    event_sourceIdentifier,
    event_sourceType,

    -- ** EventCategoriesMap
    eventCategoriesMap_events,
    eventCategoriesMap_sourceType,

    -- ** EventInfoMap
    eventInfoMap_eventCategories,
    eventInfoMap_eventDescription,
    eventInfoMap_eventId,
    eventInfoMap_severity,

    -- ** EventSubscription
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

    -- ** HsmClientCertificate
    hsmClientCertificate_hsmClientCertificateIdentifier,
    hsmClientCertificate_hsmClientCertificatePublicKey,
    hsmClientCertificate_tags,

    -- ** HsmConfiguration
    hsmConfiguration_description,
    hsmConfiguration_hsmConfigurationIdentifier,
    hsmConfiguration_hsmIpAddress,
    hsmConfiguration_hsmPartitionName,
    hsmConfiguration_tags,

    -- ** HsmStatus
    hsmStatus_hsmClientCertificateIdentifier,
    hsmStatus_hsmConfigurationIdentifier,
    hsmStatus_status,

    -- ** IPRange
    iPRange_cidrip,
    iPRange_status,
    iPRange_tags,

    -- ** LoggingStatus
    loggingStatus_bucketName,
    loggingStatus_lastFailureMessage,
    loggingStatus_lastFailureTime,
    loggingStatus_lastSuccessfulDeliveryTime,
    loggingStatus_logDestinationType,
    loggingStatus_logExports,
    loggingStatus_loggingEnabled,
    loggingStatus_s3KeyPrefix,

    -- ** MaintenanceTrack
    maintenanceTrack_databaseVersion,
    maintenanceTrack_maintenanceTrackName,
    maintenanceTrack_updateTargets,

    -- ** NetworkInterface
    networkInterface_availabilityZone,
    networkInterface_networkInterfaceId,
    networkInterface_privateIpAddress,
    networkInterface_subnetId,

    -- ** NodeConfigurationOption
    nodeConfigurationOption_estimatedDiskUtilizationPercent,
    nodeConfigurationOption_mode,
    nodeConfigurationOption_nodeType,
    nodeConfigurationOption_numberOfNodes,

    -- ** NodeConfigurationOptionsFilter
    nodeConfigurationOptionsFilter_name,
    nodeConfigurationOptionsFilter_operator,
    nodeConfigurationOptionsFilter_values,

    -- ** OrderableClusterOption
    orderableClusterOption_availabilityZones,
    orderableClusterOption_clusterType,
    orderableClusterOption_clusterVersion,
    orderableClusterOption_nodeType,

    -- ** Parameter
    parameter_allowedValues,
    parameter_applyType,
    parameter_dataType,
    parameter_description,
    parameter_isModifiable,
    parameter_minimumEngineVersion,
    parameter_parameterName,
    parameter_parameterValue,
    parameter_source,

    -- ** PartnerIntegrationInfo
    partnerIntegrationInfo_createdAt,
    partnerIntegrationInfo_databaseName,
    partnerIntegrationInfo_partnerName,
    partnerIntegrationInfo_status,
    partnerIntegrationInfo_statusMessage,
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

    -- ** RecurringCharge
    recurringCharge_recurringChargeAmount,
    recurringCharge_recurringChargeFrequency,

    -- ** ReservedNode
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

    -- ** ReservedNodeConfigurationOption
    reservedNodeConfigurationOption_sourceReservedNode,
    reservedNodeConfigurationOption_targetReservedNodeCount,
    reservedNodeConfigurationOption_targetReservedNodeOffering,

    -- ** ReservedNodeExchangeStatus
    reservedNodeExchangeStatus_requestTime,
    reservedNodeExchangeStatus_reservedNodeExchangeRequestId,
    reservedNodeExchangeStatus_sourceReservedNodeCount,
    reservedNodeExchangeStatus_sourceReservedNodeId,
    reservedNodeExchangeStatus_sourceReservedNodeType,
    reservedNodeExchangeStatus_status,
    reservedNodeExchangeStatus_targetReservedNodeCount,
    reservedNodeExchangeStatus_targetReservedNodeOfferingId,
    reservedNodeExchangeStatus_targetReservedNodeType,

    -- ** ReservedNodeOffering
    reservedNodeOffering_currencyCode,
    reservedNodeOffering_duration,
    reservedNodeOffering_fixedPrice,
    reservedNodeOffering_nodeType,
    reservedNodeOffering_offeringType,
    reservedNodeOffering_recurringCharges,
    reservedNodeOffering_reservedNodeOfferingId,
    reservedNodeOffering_reservedNodeOfferingType,
    reservedNodeOffering_usagePrice,

    -- ** ResizeClusterMessage
    resizeClusterMessage_classic,
    resizeClusterMessage_clusterType,
    resizeClusterMessage_nodeType,
    resizeClusterMessage_numberOfNodes,
    resizeClusterMessage_reservedNodeId,
    resizeClusterMessage_targetReservedNodeOfferingId,
    resizeClusterMessage_clusterIdentifier,

    -- ** ResizeInfo
    resizeInfo_allowCancelResize,
    resizeInfo_resizeType,

    -- ** ResizeProgressMessage
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

    -- ** RestoreStatus
    restoreStatus_currentRestoreRateInMegaBytesPerSecond,
    restoreStatus_elapsedTimeInSeconds,
    restoreStatus_estimatedTimeToCompletionInSeconds,
    restoreStatus_progressInMegaBytes,
    restoreStatus_snapshotSizeInMegaBytes,
    restoreStatus_status,

    -- ** ResumeClusterMessage
    resumeClusterMessage_clusterIdentifier,

    -- ** RevisionTarget
    revisionTarget_databaseRevision,
    revisionTarget_databaseRevisionReleaseDate,
    revisionTarget_description,

    -- ** ScheduledAction
    scheduledAction_endTime,
    scheduledAction_iamRole,
    scheduledAction_nextInvocations,
    scheduledAction_schedule,
    scheduledAction_scheduledActionDescription,
    scheduledAction_scheduledActionName,
    scheduledAction_startTime,
    scheduledAction_state,
    scheduledAction_targetAction,

    -- ** ScheduledActionFilter
    scheduledActionFilter_name,
    scheduledActionFilter_values,

    -- ** ScheduledActionType
    scheduledActionType_pauseCluster,
    scheduledActionType_resizeCluster,
    scheduledActionType_resumeCluster,

    -- ** Snapshot
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

    -- ** SnapshotCopyGrant
    snapshotCopyGrant_kmsKeyId,
    snapshotCopyGrant_snapshotCopyGrantName,
    snapshotCopyGrant_tags,

    -- ** SnapshotErrorMessage
    snapshotErrorMessage_failureCode,
    snapshotErrorMessage_failureReason,
    snapshotErrorMessage_snapshotClusterIdentifier,
    snapshotErrorMessage_snapshotIdentifier,

    -- ** SnapshotSchedule
    snapshotSchedule_associatedClusterCount,
    snapshotSchedule_associatedClusters,
    snapshotSchedule_nextInvocations,
    snapshotSchedule_scheduleDefinitions,
    snapshotSchedule_scheduleDescription,
    snapshotSchedule_scheduleIdentifier,
    snapshotSchedule_tags,

    -- ** SnapshotSortingEntity
    snapshotSortingEntity_sortOrder,
    snapshotSortingEntity_attribute,

    -- ** Subnet
    subnet_subnetAvailabilityZone,
    subnet_subnetIdentifier,
    subnet_subnetStatus,

    -- ** SupportedOperation
    supportedOperation_operationName,

    -- ** SupportedPlatform
    supportedPlatform_name,

    -- ** TableRestoreStatus
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

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TaggedResource
    taggedResource_resourceName,
    taggedResource_resourceType,
    taggedResource_tag,

    -- ** UpdateTarget
    updateTarget_databaseVersion,
    updateTarget_maintenanceTrackName,
    updateTarget_supportedOperations,

    -- ** UsageLimit
    usageLimit_amount,
    usageLimit_breachAction,
    usageLimit_clusterIdentifier,
    usageLimit_featureType,
    usageLimit_limitType,
    usageLimit_period,
    usageLimit_tags,
    usageLimit_usageLimitId,

    -- ** VpcEndpoint
    vpcEndpoint_networkInterfaces,
    vpcEndpoint_vpcEndpointId,
    vpcEndpoint_vpcId,

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
import Amazonka.Redshift.CreateCustomDomainAssociation
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
import Amazonka.Redshift.DeleteCustomDomainAssociation
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
import Amazonka.Redshift.DescribeCustomDomainAssociations
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
import Amazonka.Redshift.ModifyCustomDomainAssociation
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
import Amazonka.Redshift.Types.Association
import Amazonka.Redshift.Types.AttributeValueTarget
import Amazonka.Redshift.Types.AuthenticationProfile
import Amazonka.Redshift.Types.AvailabilityZone
import Amazonka.Redshift.Types.CertificateAssociation
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
