{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Lens
  ( -- * Operations

    -- ** AcceptReservedInstancesExchangeQuote
    acceptReservedInstancesExchangeQuote_dryRun,
    acceptReservedInstancesExchangeQuote_targetConfigurations,
    acceptReservedInstancesExchangeQuote_reservedInstanceIds,
    acceptReservedInstancesExchangeQuoteResponse_exchangeId,
    acceptReservedInstancesExchangeQuoteResponse_httpStatus,

    -- ** DescribeInstanceCreditSpecifications
    describeInstanceCreditSpecifications_instanceIds,
    describeInstanceCreditSpecifications_nextToken,
    describeInstanceCreditSpecifications_dryRun,
    describeInstanceCreditSpecifications_maxResults,
    describeInstanceCreditSpecifications_filters,
    describeInstanceCreditSpecificationsResponse_nextToken,
    describeInstanceCreditSpecificationsResponse_instanceCreditSpecifications,
    describeInstanceCreditSpecificationsResponse_httpStatus,

    -- ** DescribeByoipCidrs
    describeByoipCidrs_nextToken,
    describeByoipCidrs_dryRun,
    describeByoipCidrs_maxResults,
    describeByoipCidrsResponse_nextToken,
    describeByoipCidrsResponse_byoipCidrs,
    describeByoipCidrsResponse_httpStatus,

    -- ** DescribeLocalGatewayVirtualInterfaces
    describeLocalGatewayVirtualInterfaces_localGatewayVirtualInterfaceIds,
    describeLocalGatewayVirtualInterfaces_nextToken,
    describeLocalGatewayVirtualInterfaces_dryRun,
    describeLocalGatewayVirtualInterfaces_maxResults,
    describeLocalGatewayVirtualInterfaces_filters,
    describeLocalGatewayVirtualInterfacesResponse_nextToken,
    describeLocalGatewayVirtualInterfacesResponse_localGatewayVirtualInterfaces,
    describeLocalGatewayVirtualInterfacesResponse_httpStatus,

    -- ** DeleteLocalGatewayRouteTableVpcAssociation
    deleteLocalGatewayRouteTableVpcAssociation_dryRun,
    deleteLocalGatewayRouteTableVpcAssociation_localGatewayRouteTableVpcAssociationId,
    deleteLocalGatewayRouteTableVpcAssociationResponse_localGatewayRouteTableVpcAssociation,
    deleteLocalGatewayRouteTableVpcAssociationResponse_httpStatus,

    -- ** DetachVolume
    detachVolume_instanceId,
    detachVolume_dryRun,
    detachVolume_device,
    detachVolume_force,
    detachVolume_volumeId,
    volumeAttachment_instanceId,
    volumeAttachment_attachTime,
    volumeAttachment_device,
    volumeAttachment_volumeId,
    volumeAttachment_state,
    volumeAttachment_deleteOnTermination,

    -- ** CreateTransitGatewayVpcAttachment
    createTransitGatewayVpcAttachment_tagSpecifications,
    createTransitGatewayVpcAttachment_dryRun,
    createTransitGatewayVpcAttachment_options,
    createTransitGatewayVpcAttachment_transitGatewayId,
    createTransitGatewayVpcAttachment_vpcId,
    createTransitGatewayVpcAttachment_subnetIds,
    createTransitGatewayVpcAttachmentResponse_transitGatewayVpcAttachment,
    createTransitGatewayVpcAttachmentResponse_httpStatus,

    -- ** DeleteVpcEndpointConnectionNotifications
    deleteVpcEndpointConnectionNotifications_dryRun,
    deleteVpcEndpointConnectionNotifications_connectionNotificationIds,
    deleteVpcEndpointConnectionNotificationsResponse_unsuccessful,
    deleteVpcEndpointConnectionNotificationsResponse_httpStatus,

    -- ** DeleteNetworkInsightsPath
    deleteNetworkInsightsPath_dryRun,
    deleteNetworkInsightsPath_networkInsightsPathId,
    deleteNetworkInsightsPathResponse_networkInsightsPathId,
    deleteNetworkInsightsPathResponse_httpStatus,

    -- ** AuthorizeSecurityGroupEgress
    authorizeSecurityGroupEgress_fromPort,
    authorizeSecurityGroupEgress_dryRun,
    authorizeSecurityGroupEgress_sourceSecurityGroupName,
    authorizeSecurityGroupEgress_cidrIp,
    authorizeSecurityGroupEgress_ipProtocol,
    authorizeSecurityGroupEgress_ipPermissions,
    authorizeSecurityGroupEgress_sourceSecurityGroupOwnerId,
    authorizeSecurityGroupEgress_toPort,
    authorizeSecurityGroupEgress_groupId,

    -- ** ModifyManagedPrefixList
    modifyManagedPrefixList_removeEntries,
    modifyManagedPrefixList_dryRun,
    modifyManagedPrefixList_currentVersion,
    modifyManagedPrefixList_prefixListName,
    modifyManagedPrefixList_addEntries,
    modifyManagedPrefixList_prefixListId,
    modifyManagedPrefixListResponse_prefixList,
    modifyManagedPrefixListResponse_httpStatus,

    -- ** DeleteTransitGatewayPrefixListReference
    deleteTransitGatewayPrefixListReference_dryRun,
    deleteTransitGatewayPrefixListReference_transitGatewayRouteTableId,
    deleteTransitGatewayPrefixListReference_prefixListId,
    deleteTransitGatewayPrefixListReferenceResponse_transitGatewayPrefixListReference,
    deleteTransitGatewayPrefixListReferenceResponse_httpStatus,

    -- ** DeleteTransitGatewayRoute
    deleteTransitGatewayRoute_dryRun,
    deleteTransitGatewayRoute_transitGatewayRouteTableId,
    deleteTransitGatewayRoute_destinationCidrBlock,
    deleteTransitGatewayRouteResponse_route,
    deleteTransitGatewayRouteResponse_httpStatus,

    -- ** DescribeVpcPeeringConnections
    describeVpcPeeringConnections_vpcPeeringConnectionIds,
    describeVpcPeeringConnections_nextToken,
    describeVpcPeeringConnections_dryRun,
    describeVpcPeeringConnections_maxResults,
    describeVpcPeeringConnections_filters,
    describeVpcPeeringConnectionsResponse_nextToken,
    describeVpcPeeringConnectionsResponse_vpcPeeringConnections,
    describeVpcPeeringConnectionsResponse_httpStatus,

    -- ** DescribeInstances
    describeInstances_instanceIds,
    describeInstances_nextToken,
    describeInstances_dryRun,
    describeInstances_maxResults,
    describeInstances_filters,
    describeInstancesResponse_nextToken,
    describeInstancesResponse_reservations,
    describeInstancesResponse_httpStatus,

    -- ** DeregisterInstanceEventNotificationAttributes
    deregisterInstanceEventNotificationAttributes_dryRun,
    deregisterInstanceEventNotificationAttributes_instanceTagAttribute,
    deregisterInstanceEventNotificationAttributesResponse_instanceTagAttribute,
    deregisterInstanceEventNotificationAttributesResponse_httpStatus,

    -- ** CreateTransitGatewayMulticastDomain
    createTransitGatewayMulticastDomain_tagSpecifications,
    createTransitGatewayMulticastDomain_dryRun,
    createTransitGatewayMulticastDomain_options,
    createTransitGatewayMulticastDomain_transitGatewayId,
    createTransitGatewayMulticastDomainResponse_transitGatewayMulticastDomain,
    createTransitGatewayMulticastDomainResponse_httpStatus,

    -- ** AssociateTransitGatewayMulticastDomain
    associateTransitGatewayMulticastDomain_dryRun,
    associateTransitGatewayMulticastDomain_transitGatewayMulticastDomainId,
    associateTransitGatewayMulticastDomain_subnetIds,
    associateTransitGatewayMulticastDomain_transitGatewayAttachmentId,
    associateTransitGatewayMulticastDomainResponse_associations,
    associateTransitGatewayMulticastDomainResponse_httpStatus,

    -- ** ReleaseAddress
    releaseAddress_dryRun,
    releaseAddress_publicIp,
    releaseAddress_allocationId,
    releaseAddress_networkBorderGroup,

    -- ** DeregisterTransitGatewayMulticastGroupMembers
    deregisterTransitGatewayMulticastGroupMembers_dryRun,
    deregisterTransitGatewayMulticastGroupMembers_transitGatewayMulticastDomainId,
    deregisterTransitGatewayMulticastGroupMembers_networkInterfaceIds,
    deregisterTransitGatewayMulticastGroupMembers_groupIpAddress,
    deregisterTransitGatewayMulticastGroupMembersResponse_deregisteredMulticastGroupMembers,
    deregisterTransitGatewayMulticastGroupMembersResponse_httpStatus,

    -- ** GetHostReservationPurchasePreview
    getHostReservationPurchasePreview_hostIdSet,
    getHostReservationPurchasePreview_offeringId,
    getHostReservationPurchasePreviewResponse_totalUpfrontPrice,
    getHostReservationPurchasePreviewResponse_currencyCode,
    getHostReservationPurchasePreviewResponse_purchase,
    getHostReservationPurchasePreviewResponse_totalHourlyPrice,
    getHostReservationPurchasePreviewResponse_httpStatus,

    -- ** CancelBundleTask
    cancelBundleTask_dryRun,
    cancelBundleTask_bundleId,
    cancelBundleTaskResponse_bundleTask,
    cancelBundleTaskResponse_httpStatus,

    -- ** GetCapacityReservationUsage
    getCapacityReservationUsage_nextToken,
    getCapacityReservationUsage_dryRun,
    getCapacityReservationUsage_maxResults,
    getCapacityReservationUsage_capacityReservationId,
    getCapacityReservationUsageResponse_instanceUsages,
    getCapacityReservationUsageResponse_nextToken,
    getCapacityReservationUsageResponse_instanceType,
    getCapacityReservationUsageResponse_availableInstanceCount,
    getCapacityReservationUsageResponse_state,
    getCapacityReservationUsageResponse_capacityReservationId,
    getCapacityReservationUsageResponse_totalInstanceCount,
    getCapacityReservationUsageResponse_httpStatus,

    -- ** CreateTransitGatewayConnectPeer
    createTransitGatewayConnectPeer_tagSpecifications,
    createTransitGatewayConnectPeer_dryRun,
    createTransitGatewayConnectPeer_transitGatewayAddress,
    createTransitGatewayConnectPeer_bgpOptions,
    createTransitGatewayConnectPeer_transitGatewayAttachmentId,
    createTransitGatewayConnectPeer_peerAddress,
    createTransitGatewayConnectPeer_insideCidrBlocks,
    createTransitGatewayConnectPeerResponse_transitGatewayConnectPeer,
    createTransitGatewayConnectPeerResponse_httpStatus,

    -- ** ModifyVpcTenancy
    modifyVpcTenancy_dryRun,
    modifyVpcTenancy_vpcId,
    modifyVpcTenancy_instanceTenancy,
    modifyVpcTenancyResponse_returnValue,
    modifyVpcTenancyResponse_httpStatus,

    -- ** CreateVpcEndpointServiceConfiguration
    createVpcEndpointServiceConfiguration_gatewayLoadBalancerArns,
    createVpcEndpointServiceConfiguration_tagSpecifications,
    createVpcEndpointServiceConfiguration_dryRun,
    createVpcEndpointServiceConfiguration_privateDnsName,
    createVpcEndpointServiceConfiguration_acceptanceRequired,
    createVpcEndpointServiceConfiguration_networkLoadBalancerArns,
    createVpcEndpointServiceConfiguration_clientToken,
    createVpcEndpointServiceConfigurationResponse_serviceConfiguration,
    createVpcEndpointServiceConfigurationResponse_clientToken,
    createVpcEndpointServiceConfigurationResponse_httpStatus,

    -- ** DescribeExportTasks
    describeExportTasks_exportTaskIds,
    describeExportTasks_filters,
    describeExportTasksResponse_exportTasks,
    describeExportTasksResponse_httpStatus,

    -- ** GetTransitGatewayMulticastDomainAssociations
    getTransitGatewayMulticastDomainAssociations_nextToken,
    getTransitGatewayMulticastDomainAssociations_dryRun,
    getTransitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId,
    getTransitGatewayMulticastDomainAssociations_maxResults,
    getTransitGatewayMulticastDomainAssociations_filters,
    getTransitGatewayMulticastDomainAssociationsResponse_nextToken,
    getTransitGatewayMulticastDomainAssociationsResponse_multicastDomainAssociations,
    getTransitGatewayMulticastDomainAssociationsResponse_httpStatus,

    -- ** DisableEbsEncryptionByDefault
    disableEbsEncryptionByDefault_dryRun,
    disableEbsEncryptionByDefaultResponse_ebsEncryptionByDefault,
    disableEbsEncryptionByDefaultResponse_httpStatus,

    -- ** AssociateVpcCidrBlock
    associateVpcCidrBlock_ipv6Pool,
    associateVpcCidrBlock_ipv6CidrBlock,
    associateVpcCidrBlock_ipv6CidrBlockNetworkBorderGroup,
    associateVpcCidrBlock_amazonProvidedIpv6CidrBlock,
    associateVpcCidrBlock_cidrBlock,
    associateVpcCidrBlock_vpcId,
    associateVpcCidrBlockResponse_ipv6CidrBlockAssociation,
    associateVpcCidrBlockResponse_cidrBlockAssociation,
    associateVpcCidrBlockResponse_vpcId,
    associateVpcCidrBlockResponse_httpStatus,

    -- ** CreateNetworkAcl
    createNetworkAcl_tagSpecifications,
    createNetworkAcl_dryRun,
    createNetworkAcl_vpcId,
    createNetworkAclResponse_networkAcl,
    createNetworkAclResponse_httpStatus,

    -- ** AcceptTransitGatewayPeeringAttachment
    acceptTransitGatewayPeeringAttachment_dryRun,
    acceptTransitGatewayPeeringAttachment_transitGatewayAttachmentId,
    acceptTransitGatewayPeeringAttachmentResponse_transitGatewayPeeringAttachment,
    acceptTransitGatewayPeeringAttachmentResponse_httpStatus,

    -- ** DeleteLaunchTemplate
    deleteLaunchTemplate_dryRun,
    deleteLaunchTemplate_launchTemplateId,
    deleteLaunchTemplate_launchTemplateName,
    deleteLaunchTemplateResponse_launchTemplate,
    deleteLaunchTemplateResponse_httpStatus,

    -- ** DeleteVpc
    deleteVpc_dryRun,
    deleteVpc_vpcId,

    -- ** DeleteFleets
    deleteFleets_dryRun,
    deleteFleets_fleetIds,
    deleteFleets_terminateInstances,
    deleteFleetsResponse_unsuccessfulFleetDeletions,
    deleteFleetsResponse_successfulFleetDeletions,
    deleteFleetsResponse_httpStatus,

    -- ** GetAssociatedIpv6PoolCidrs
    getAssociatedIpv6PoolCidrs_nextToken,
    getAssociatedIpv6PoolCidrs_dryRun,
    getAssociatedIpv6PoolCidrs_maxResults,
    getAssociatedIpv6PoolCidrs_poolId,
    getAssociatedIpv6PoolCidrsResponse_ipv6CidrAssociations,
    getAssociatedIpv6PoolCidrsResponse_nextToken,
    getAssociatedIpv6PoolCidrsResponse_httpStatus,

    -- ** DescribeTrafficMirrorSessions
    describeTrafficMirrorSessions_nextToken,
    describeTrafficMirrorSessions_trafficMirrorSessionIds,
    describeTrafficMirrorSessions_dryRun,
    describeTrafficMirrorSessions_maxResults,
    describeTrafficMirrorSessions_filters,
    describeTrafficMirrorSessionsResponse_nextToken,
    describeTrafficMirrorSessionsResponse_trafficMirrorSessions,
    describeTrafficMirrorSessionsResponse_httpStatus,

    -- ** ImportInstance
    importInstance_diskImages,
    importInstance_dryRun,
    importInstance_description,
    importInstance_launchSpecification,
    importInstance_platform,
    importInstanceResponse_conversionTask,
    importInstanceResponse_httpStatus,

    -- ** DescribeLocalGatewayRouteTables
    describeLocalGatewayRouteTables_localGatewayRouteTableIds,
    describeLocalGatewayRouteTables_nextToken,
    describeLocalGatewayRouteTables_dryRun,
    describeLocalGatewayRouteTables_maxResults,
    describeLocalGatewayRouteTables_filters,
    describeLocalGatewayRouteTablesResponse_nextToken,
    describeLocalGatewayRouteTablesResponse_localGatewayRouteTables,
    describeLocalGatewayRouteTablesResponse_httpStatus,

    -- ** CreateNetworkInterfacePermission
    createNetworkInterfacePermission_awsAccountId,
    createNetworkInterfacePermission_dryRun,
    createNetworkInterfacePermission_awsService,
    createNetworkInterfacePermission_networkInterfaceId,
    createNetworkInterfacePermission_permission,
    createNetworkInterfacePermissionResponse_interfacePermission,
    createNetworkInterfacePermissionResponse_httpStatus,

    -- ** CreateVpnGateway
    createVpnGateway_tagSpecifications,
    createVpnGateway_dryRun,
    createVpnGateway_availabilityZone,
    createVpnGateway_amazonSideAsn,
    createVpnGateway_type,
    createVpnGatewayResponse_vpnGateway,
    createVpnGatewayResponse_httpStatus,

    -- ** GetTransitGatewayRouteTableAssociations
    getTransitGatewayRouteTableAssociations_nextToken,
    getTransitGatewayRouteTableAssociations_dryRun,
    getTransitGatewayRouteTableAssociations_maxResults,
    getTransitGatewayRouteTableAssociations_filters,
    getTransitGatewayRouteTableAssociations_transitGatewayRouteTableId,
    getTransitGatewayRouteTableAssociationsResponse_nextToken,
    getTransitGatewayRouteTableAssociationsResponse_associations,
    getTransitGatewayRouteTableAssociationsResponse_httpStatus,

    -- ** RejectTransitGatewayVpcAttachment
    rejectTransitGatewayVpcAttachment_dryRun,
    rejectTransitGatewayVpcAttachment_transitGatewayAttachmentId,
    rejectTransitGatewayVpcAttachmentResponse_transitGatewayVpcAttachment,
    rejectTransitGatewayVpcAttachmentResponse_httpStatus,

    -- ** ModifyVolumeAttribute
    modifyVolumeAttribute_dryRun,
    modifyVolumeAttribute_autoEnableIO,
    modifyVolumeAttribute_volumeId,

    -- ** DescribePrefixLists
    describePrefixLists_nextToken,
    describePrefixLists_prefixListIds,
    describePrefixLists_dryRun,
    describePrefixLists_maxResults,
    describePrefixLists_filters,
    describePrefixListsResponse_nextToken,
    describePrefixListsResponse_prefixLists,
    describePrefixListsResponse_httpStatus,

    -- ** DetachNetworkInterface
    detachNetworkInterface_dryRun,
    detachNetworkInterface_force,
    detachNetworkInterface_attachmentId,

    -- ** DeleteVpcEndpoints
    deleteVpcEndpoints_dryRun,
    deleteVpcEndpoints_vpcEndpointIds,
    deleteVpcEndpointsResponse_unsuccessful,
    deleteVpcEndpointsResponse_httpStatus,

    -- ** DescribeVpcClassicLink
    describeVpcClassicLink_dryRun,
    describeVpcClassicLink_filters,
    describeVpcClassicLink_vpcIds,
    describeVpcClassicLinkResponse_vpcs,
    describeVpcClassicLinkResponse_httpStatus,

    -- ** UpdateSecurityGroupRuleDescriptionsIngress
    updateSecurityGroupRuleDescriptionsIngress_dryRun,
    updateSecurityGroupRuleDescriptionsIngress_groupName,
    updateSecurityGroupRuleDescriptionsIngress_groupId,
    updateSecurityGroupRuleDescriptionsIngress_ipPermissions,
    updateSecurityGroupRuleDescriptionsIngressResponse_return,
    updateSecurityGroupRuleDescriptionsIngressResponse_httpStatus,

    -- ** DescribeClientVpnEndpoints
    describeClientVpnEndpoints_nextToken,
    describeClientVpnEndpoints_dryRun,
    describeClientVpnEndpoints_maxResults,
    describeClientVpnEndpoints_clientVpnEndpointIds,
    describeClientVpnEndpoints_filters,
    describeClientVpnEndpointsResponse_nextToken,
    describeClientVpnEndpointsResponse_clientVpnEndpoints,
    describeClientVpnEndpointsResponse_httpStatus,

    -- ** DisassociateAddress
    disassociateAddress_dryRun,
    disassociateAddress_associationId,
    disassociateAddress_publicIp,

    -- ** DescribeScheduledInstanceAvailability
    describeScheduledInstanceAvailability_minSlotDurationInHours,
    describeScheduledInstanceAvailability_nextToken,
    describeScheduledInstanceAvailability_dryRun,
    describeScheduledInstanceAvailability_maxResults,
    describeScheduledInstanceAvailability_filters,
    describeScheduledInstanceAvailability_maxSlotDurationInHours,
    describeScheduledInstanceAvailability_firstSlotStartTimeRange,
    describeScheduledInstanceAvailability_recurrence,
    describeScheduledInstanceAvailabilityResponse_nextToken,
    describeScheduledInstanceAvailabilityResponse_scheduledInstanceAvailabilitySet,
    describeScheduledInstanceAvailabilityResponse_httpStatus,

    -- ** RejectVpcEndpointConnections
    rejectVpcEndpointConnections_dryRun,
    rejectVpcEndpointConnections_serviceId,
    rejectVpcEndpointConnections_vpcEndpointIds,
    rejectVpcEndpointConnectionsResponse_unsuccessful,
    rejectVpcEndpointConnectionsResponse_httpStatus,

    -- ** CreateTransitGatewayRouteTable
    createTransitGatewayRouteTable_tagSpecifications,
    createTransitGatewayRouteTable_dryRun,
    createTransitGatewayRouteTable_transitGatewayId,
    createTransitGatewayRouteTableResponse_transitGatewayRouteTable,
    createTransitGatewayRouteTableResponse_httpStatus,

    -- ** DescribeTags
    describeTags_nextToken,
    describeTags_dryRun,
    describeTags_maxResults,
    describeTags_filters,
    describeTagsResponse_nextToken,
    describeTagsResponse_tags,
    describeTagsResponse_httpStatus,

    -- ** AssociateTransitGatewayRouteTable
    associateTransitGatewayRouteTable_dryRun,
    associateTransitGatewayRouteTable_transitGatewayRouteTableId,
    associateTransitGatewayRouteTable_transitGatewayAttachmentId,
    associateTransitGatewayRouteTableResponse_association,
    associateTransitGatewayRouteTableResponse_httpStatus,

    -- ** DeleteFlowLogs
    deleteFlowLogs_dryRun,
    deleteFlowLogs_flowLogIds,
    deleteFlowLogsResponse_unsuccessful,
    deleteFlowLogsResponse_httpStatus,

    -- ** CreateDefaultSubnet
    createDefaultSubnet_dryRun,
    createDefaultSubnet_availabilityZone,
    createDefaultSubnetResponse_subnet,
    createDefaultSubnetResponse_httpStatus,

    -- ** DeleteTrafficMirrorTarget
    deleteTrafficMirrorTarget_dryRun,
    deleteTrafficMirrorTarget_trafficMirrorTargetId,
    deleteTrafficMirrorTargetResponse_trafficMirrorTargetId,
    deleteTrafficMirrorTargetResponse_httpStatus,

    -- ** AcceptTransitGatewayMulticastDomainAssociations
    acceptTransitGatewayMulticastDomainAssociations_dryRun,
    acceptTransitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId,
    acceptTransitGatewayMulticastDomainAssociations_subnetIds,
    acceptTransitGatewayMulticastDomainAssociations_transitGatewayAttachmentId,
    acceptTransitGatewayMulticastDomainAssociationsResponse_associations,
    acceptTransitGatewayMulticastDomainAssociationsResponse_httpStatus,

    -- ** DescribeLaunchTemplateVersions
    describeLaunchTemplateVersions_nextToken,
    describeLaunchTemplateVersions_versions,
    describeLaunchTemplateVersions_dryRun,
    describeLaunchTemplateVersions_maxResults,
    describeLaunchTemplateVersions_minVersion,
    describeLaunchTemplateVersions_launchTemplateId,
    describeLaunchTemplateVersions_launchTemplateName,
    describeLaunchTemplateVersions_maxVersion,
    describeLaunchTemplateVersions_filters,
    describeLaunchTemplateVersionsResponse_nextToken,
    describeLaunchTemplateVersionsResponse_launchTemplateVersions,
    describeLaunchTemplateVersionsResponse_httpStatus,

    -- ** DescribeAvailabilityZones
    describeAvailabilityZones_dryRun,
    describeAvailabilityZones_filters,
    describeAvailabilityZones_zoneIds,
    describeAvailabilityZones_allAvailabilityZones,
    describeAvailabilityZones_zoneNames,
    describeAvailabilityZonesResponse_availabilityZones,
    describeAvailabilityZonesResponse_httpStatus,

    -- ** GetReservedInstancesExchangeQuote
    getReservedInstancesExchangeQuote_dryRun,
    getReservedInstancesExchangeQuote_targetConfigurations,
    getReservedInstancesExchangeQuote_reservedInstanceIds,
    getReservedInstancesExchangeQuoteResponse_isValidExchange,
    getReservedInstancesExchangeQuoteResponse_reservedInstanceValueRollup,
    getReservedInstancesExchangeQuoteResponse_paymentDue,
    getReservedInstancesExchangeQuoteResponse_currencyCode,
    getReservedInstancesExchangeQuoteResponse_targetConfigurationValueRollup,
    getReservedInstancesExchangeQuoteResponse_validationFailureReason,
    getReservedInstancesExchangeQuoteResponse_reservedInstanceValueSet,
    getReservedInstancesExchangeQuoteResponse_outputReservedInstancesWillExpireAt,
    getReservedInstancesExchangeQuoteResponse_targetConfigurationValueSet,
    getReservedInstancesExchangeQuoteResponse_httpStatus,

    -- ** DeleteVpnGateway
    deleteVpnGateway_dryRun,
    deleteVpnGateway_vpnGatewayId,

    -- ** CreateKeyPair
    createKeyPair_tagSpecifications,
    createKeyPair_dryRun,
    createKeyPair_keyName,
    createKeyPairResponse_keyPairId,
    createKeyPairResponse_tags,
    createKeyPairResponse_httpStatus,
    createKeyPairResponse_keyName,
    createKeyPairResponse_keyFingerprint,
    createKeyPairResponse_keyMaterial,

    -- ** ExportTransitGatewayRoutes
    exportTransitGatewayRoutes_dryRun,
    exportTransitGatewayRoutes_filters,
    exportTransitGatewayRoutes_transitGatewayRouteTableId,
    exportTransitGatewayRoutes_s3Bucket,
    exportTransitGatewayRoutesResponse_s3Location,
    exportTransitGatewayRoutesResponse_httpStatus,

    -- ** CopySnapshot
    copySnapshot_tagSpecifications,
    copySnapshot_destinationRegion,
    copySnapshot_dryRun,
    copySnapshot_encrypted,
    copySnapshot_kmsKeyId,
    copySnapshot_destinationOutpostArn,
    copySnapshot_presignedUrl,
    copySnapshot_description,
    copySnapshot_sourceRegion,
    copySnapshot_sourceSnapshotId,
    copySnapshotResponse_snapshotId,
    copySnapshotResponse_tags,
    copySnapshotResponse_httpStatus,

    -- ** DescribeElasticGpus
    describeElasticGpus_nextToken,
    describeElasticGpus_elasticGpuIds,
    describeElasticGpus_dryRun,
    describeElasticGpus_maxResults,
    describeElasticGpus_filters,
    describeElasticGpusResponse_nextToken,
    describeElasticGpusResponse_maxResults,
    describeElasticGpusResponse_elasticGpuSet,
    describeElasticGpusResponse_httpStatus,

    -- ** StartNetworkInsightsAnalysis
    startNetworkInsightsAnalysis_tagSpecifications,
    startNetworkInsightsAnalysis_filterInArns,
    startNetworkInsightsAnalysis_dryRun,
    startNetworkInsightsAnalysis_networkInsightsPathId,
    startNetworkInsightsAnalysis_clientToken,
    startNetworkInsightsAnalysisResponse_networkInsightsAnalysis,
    startNetworkInsightsAnalysisResponse_httpStatus,

    -- ** DescribeFpgaImages
    describeFpgaImages_nextToken,
    describeFpgaImages_dryRun,
    describeFpgaImages_maxResults,
    describeFpgaImages_owners,
    describeFpgaImages_fpgaImageIds,
    describeFpgaImages_filters,
    describeFpgaImagesResponse_nextToken,
    describeFpgaImagesResponse_fpgaImages,
    describeFpgaImagesResponse_httpStatus,

    -- ** CreateFlowLogs
    createFlowLogs_maxAggregationInterval,
    createFlowLogs_tagSpecifications,
    createFlowLogs_dryRun,
    createFlowLogs_logDestination,
    createFlowLogs_logFormat,
    createFlowLogs_logGroupName,
    createFlowLogs_deliverLogsPermissionArn,
    createFlowLogs_logDestinationType,
    createFlowLogs_clientToken,
    createFlowLogs_resourceIds,
    createFlowLogs_resourceType,
    createFlowLogs_trafficType,
    createFlowLogsResponse_unsuccessful,
    createFlowLogsResponse_flowLogIds,
    createFlowLogsResponse_clientToken,
    createFlowLogsResponse_httpStatus,

    -- ** CreateLaunchTemplate
    createLaunchTemplate_tagSpecifications,
    createLaunchTemplate_dryRun,
    createLaunchTemplate_versionDescription,
    createLaunchTemplate_clientToken,
    createLaunchTemplate_launchTemplateName,
    createLaunchTemplate_launchTemplateData,
    createLaunchTemplateResponse_launchTemplate,
    createLaunchTemplateResponse_warning,
    createLaunchTemplateResponse_httpStatus,

    -- ** DescribeImportImageTasks
    describeImportImageTasks_nextToken,
    describeImportImageTasks_dryRun,
    describeImportImageTasks_importTaskIds,
    describeImportImageTasks_maxResults,
    describeImportImageTasks_filters,
    describeImportImageTasksResponse_nextToken,
    describeImportImageTasksResponse_importImageTasks,
    describeImportImageTasksResponse_httpStatus,

    -- ** DeleteTransitGatewayRouteTable
    deleteTransitGatewayRouteTable_dryRun,
    deleteTransitGatewayRouteTable_transitGatewayRouteTableId,
    deleteTransitGatewayRouteTableResponse_transitGatewayRouteTable,
    deleteTransitGatewayRouteTableResponse_httpStatus,

    -- ** DeleteNetworkAcl
    deleteNetworkAcl_dryRun,
    deleteNetworkAcl_networkAclId,

    -- ** MoveAddressToVpc
    moveAddressToVpc_dryRun,
    moveAddressToVpc_publicIp,
    moveAddressToVpcResponse_status,
    moveAddressToVpcResponse_allocationId,
    moveAddressToVpcResponse_httpStatus,

    -- ** DescribeFleetInstances
    describeFleetInstances_nextToken,
    describeFleetInstances_dryRun,
    describeFleetInstances_maxResults,
    describeFleetInstances_filters,
    describeFleetInstances_fleetId,
    describeFleetInstancesResponse_nextToken,
    describeFleetInstancesResponse_fleetId,
    describeFleetInstancesResponse_activeInstances,
    describeFleetInstancesResponse_httpStatus,

    -- ** RestoreAddressToClassic
    restoreAddressToClassic_dryRun,
    restoreAddressToClassic_publicIp,
    restoreAddressToClassicResponse_status,
    restoreAddressToClassicResponse_publicIp,
    restoreAddressToClassicResponse_httpStatus,

    -- ** DeleteNetworkInterfacePermission
    deleteNetworkInterfacePermission_dryRun,
    deleteNetworkInterfacePermission_force,
    deleteNetworkInterfacePermission_networkInterfacePermissionId,
    deleteNetworkInterfacePermissionResponse_return,
    deleteNetworkInterfacePermissionResponse_httpStatus,

    -- ** DescribeRouteTables
    describeRouteTables_nextToken,
    describeRouteTables_routeTableIds,
    describeRouteTables_dryRun,
    describeRouteTables_maxResults,
    describeRouteTables_filters,
    describeRouteTablesResponse_nextToken,
    describeRouteTablesResponse_routeTables,
    describeRouteTablesResponse_httpStatus,

    -- ** UpdateSecurityGroupRuleDescriptionsEgress
    updateSecurityGroupRuleDescriptionsEgress_dryRun,
    updateSecurityGroupRuleDescriptionsEgress_groupName,
    updateSecurityGroupRuleDescriptionsEgress_groupId,
    updateSecurityGroupRuleDescriptionsEgress_ipPermissions,
    updateSecurityGroupRuleDescriptionsEgressResponse_return,
    updateSecurityGroupRuleDescriptionsEgressResponse_httpStatus,

    -- ** ResetFpgaImageAttribute
    resetFpgaImageAttribute_dryRun,
    resetFpgaImageAttribute_attribute,
    resetFpgaImageAttribute_fpgaImageId,
    resetFpgaImageAttributeResponse_return,
    resetFpgaImageAttributeResponse_httpStatus,

    -- ** StartVpcEndpointServicePrivateDnsVerification
    startVpcEndpointServicePrivateDnsVerification_dryRun,
    startVpcEndpointServicePrivateDnsVerification_serviceId,
    startVpcEndpointServicePrivateDnsVerificationResponse_returnValue,
    startVpcEndpointServicePrivateDnsVerificationResponse_httpStatus,

    -- ** DescribeVolumes
    describeVolumes_nextToken,
    describeVolumes_dryRun,
    describeVolumes_volumeIds,
    describeVolumes_maxResults,
    describeVolumes_filters,
    describeVolumesResponse_nextToken,
    describeVolumesResponse_volumes,
    describeVolumesResponse_httpStatus,

    -- ** CreateClientVpnEndpoint
    createClientVpnEndpoint_securityGroupIds,
    createClientVpnEndpoint_tagSpecifications,
    createClientVpnEndpoint_dryRun,
    createClientVpnEndpoint_transportProtocol,
    createClientVpnEndpoint_clientConnectOptions,
    createClientVpnEndpoint_dnsServers,
    createClientVpnEndpoint_vpnPort,
    createClientVpnEndpoint_description,
    createClientVpnEndpoint_vpcId,
    createClientVpnEndpoint_selfServicePortal,
    createClientVpnEndpoint_clientToken,
    createClientVpnEndpoint_splitTunnel,
    createClientVpnEndpoint_clientCidrBlock,
    createClientVpnEndpoint_serverCertificateArn,
    createClientVpnEndpoint_authenticationOptions,
    createClientVpnEndpoint_connectionLogOptions,
    createClientVpnEndpointResponse_clientVpnEndpointId,
    createClientVpnEndpointResponse_status,
    createClientVpnEndpointResponse_dnsName,
    createClientVpnEndpointResponse_httpStatus,

    -- ** RevokeClientVpnIngress
    revokeClientVpnIngress_accessGroupId,
    revokeClientVpnIngress_dryRun,
    revokeClientVpnIngress_revokeAllGroups,
    revokeClientVpnIngress_clientVpnEndpointId,
    revokeClientVpnIngress_targetNetworkCidr,
    revokeClientVpnIngressResponse_status,
    revokeClientVpnIngressResponse_httpStatus,

    -- ** DeleteFpgaImage
    deleteFpgaImage_dryRun,
    deleteFpgaImage_fpgaImageId,
    deleteFpgaImageResponse_return,
    deleteFpgaImageResponse_httpStatus,

    -- ** ModifyVpcEndpoint
    modifyVpcEndpoint_policyDocument,
    modifyVpcEndpoint_dryRun,
    modifyVpcEndpoint_removeSubnetIds,
    modifyVpcEndpoint_addRouteTableIds,
    modifyVpcEndpoint_resetPolicy,
    modifyVpcEndpoint_removeRouteTableIds,
    modifyVpcEndpoint_addSubnetIds,
    modifyVpcEndpoint_privateDnsEnabled,
    modifyVpcEndpoint_removeSecurityGroupIds,
    modifyVpcEndpoint_addSecurityGroupIds,
    modifyVpcEndpoint_vpcEndpointId,
    modifyVpcEndpointResponse_return,
    modifyVpcEndpointResponse_httpStatus,

    -- ** DescribeReservedInstancesModifications
    describeReservedInstancesModifications_nextToken,
    describeReservedInstancesModifications_reservedInstancesModificationIds,
    describeReservedInstancesModifications_filters,
    describeReservedInstancesModificationsResponse_nextToken,
    describeReservedInstancesModificationsResponse_reservedInstancesModifications,
    describeReservedInstancesModificationsResponse_httpStatus,

    -- ** DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
    describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_nextToken,
    describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_dryRun,
    describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_localGatewayRouteTableVirtualInterfaceGroupAssociationIds,
    describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_maxResults,
    describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_filters,
    describeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse_nextToken,
    describeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse_localGatewayRouteTableVirtualInterfaceGroupAssociations,
    describeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse_httpStatus,

    -- ** EnableFastSnapshotRestores
    enableFastSnapshotRestores_dryRun,
    enableFastSnapshotRestores_availabilityZones,
    enableFastSnapshotRestores_sourceSnapshotIds,
    enableFastSnapshotRestoresResponse_unsuccessful,
    enableFastSnapshotRestoresResponse_successful,
    enableFastSnapshotRestoresResponse_httpStatus,

    -- ** DescribeClientVpnRoutes
    describeClientVpnRoutes_nextToken,
    describeClientVpnRoutes_dryRun,
    describeClientVpnRoutes_maxResults,
    describeClientVpnRoutes_filters,
    describeClientVpnRoutes_clientVpnEndpointId,
    describeClientVpnRoutesResponse_nextToken,
    describeClientVpnRoutesResponse_routes,
    describeClientVpnRoutesResponse_httpStatus,

    -- ** GetEbsDefaultKmsKeyId
    getEbsDefaultKmsKeyId_dryRun,
    getEbsDefaultKmsKeyIdResponse_kmsKeyId,
    getEbsDefaultKmsKeyIdResponse_httpStatus,

    -- ** ModifyIdFormat
    modifyIdFormat_resource,
    modifyIdFormat_useLongIds,

    -- ** DetachClassicLinkVpc
    detachClassicLinkVpc_dryRun,
    detachClassicLinkVpc_instanceId,
    detachClassicLinkVpc_vpcId,
    detachClassicLinkVpcResponse_return,
    detachClassicLinkVpcResponse_httpStatus,

    -- ** UnassignPrivateIpAddresses
    unassignPrivateIpAddresses_networkInterfaceId,
    unassignPrivateIpAddresses_privateIpAddresses,

    -- ** AllocateHosts
    allocateHosts_instanceFamily,
    allocateHosts_tagSpecifications,
    allocateHosts_instanceType,
    allocateHosts_autoPlacement,
    allocateHosts_hostRecovery,
    allocateHosts_clientToken,
    allocateHosts_availabilityZone,
    allocateHosts_quantity,
    allocateHostsResponse_hostIds,
    allocateHostsResponse_httpStatus,

    -- ** GetConsoleOutput
    getConsoleOutput_dryRun,
    getConsoleOutput_latest,
    getConsoleOutput_instanceId,
    getConsoleOutputResponse_instanceId,
    getConsoleOutputResponse_output,
    getConsoleOutputResponse_timestamp,
    getConsoleOutputResponse_httpStatus,

    -- ** ModifyVpnConnectionOptions
    modifyVpnConnectionOptions_remoteIpv6NetworkCidr,
    modifyVpnConnectionOptions_dryRun,
    modifyVpnConnectionOptions_localIpv6NetworkCidr,
    modifyVpnConnectionOptions_remoteIpv4NetworkCidr,
    modifyVpnConnectionOptions_localIpv4NetworkCidr,
    modifyVpnConnectionOptions_vpnConnectionId,
    modifyVpnConnectionOptionsResponse_vpnConnection,
    modifyVpnConnectionOptionsResponse_httpStatus,

    -- ** CancelImportTask
    cancelImportTask_dryRun,
    cancelImportTask_importTaskId,
    cancelImportTask_cancelReason,
    cancelImportTaskResponse_importTaskId,
    cancelImportTaskResponse_state,
    cancelImportTaskResponse_previousState,
    cancelImportTaskResponse_httpStatus,

    -- ** RegisterImage
    registerImage_virtualizationType,
    registerImage_rootDeviceName,
    registerImage_dryRun,
    registerImage_ramdiskId,
    registerImage_architecture,
    registerImage_sriovNetSupport,
    registerImage_blockDeviceMappings,
    registerImage_kernelId,
    registerImage_description,
    registerImage_billingProducts,
    registerImage_enaSupport,
    registerImage_imageLocation,
    registerImage_name,
    registerImageResponse_imageId,
    registerImageResponse_httpStatus,

    -- ** ModifyFleet
    modifyFleet_launchTemplateConfigs,
    modifyFleet_dryRun,
    modifyFleet_excessCapacityTerminationPolicy,
    modifyFleet_targetCapacitySpecification,
    modifyFleet_fleetId,
    modifyFleetResponse_return,
    modifyFleetResponse_httpStatus,

    -- ** DeleteRouteTable
    deleteRouteTable_dryRun,
    deleteRouteTable_routeTableId,

    -- ** ModifyReservedInstances
    modifyReservedInstances_clientToken,
    modifyReservedInstances_reservedInstancesIds,
    modifyReservedInstances_targetConfigurations,
    modifyReservedInstancesResponse_reservedInstancesModificationId,
    modifyReservedInstancesResponse_httpStatus,

    -- ** DescribeImageAttribute
    describeImageAttribute_dryRun,
    describeImageAttribute_attribute,
    describeImageAttribute_imageId,
    describeImageAttributeResponse_ramdiskId,
    describeImageAttributeResponse_productCodes,
    describeImageAttributeResponse_launchPermissions,
    describeImageAttributeResponse_imageId,
    describeImageAttributeResponse_sriovNetSupport,
    describeImageAttributeResponse_blockDeviceMappings,
    describeImageAttributeResponse_kernelId,
    describeImageAttributeResponse_description,
    describeImageAttributeResponse_httpStatus,

    -- ** CreateTrafficMirrorFilterRule
    createTrafficMirrorFilterRule_dryRun,
    createTrafficMirrorFilterRule_sourcePortRange,
    createTrafficMirrorFilterRule_protocol,
    createTrafficMirrorFilterRule_description,
    createTrafficMirrorFilterRule_clientToken,
    createTrafficMirrorFilterRule_destinationPortRange,
    createTrafficMirrorFilterRule_trafficMirrorFilterId,
    createTrafficMirrorFilterRule_trafficDirection,
    createTrafficMirrorFilterRule_ruleNumber,
    createTrafficMirrorFilterRule_ruleAction,
    createTrafficMirrorFilterRule_destinationCidrBlock,
    createTrafficMirrorFilterRule_sourceCidrBlock,
    createTrafficMirrorFilterRuleResponse_clientToken,
    createTrafficMirrorFilterRuleResponse_trafficMirrorFilterRule,
    createTrafficMirrorFilterRuleResponse_httpStatus,

    -- ** MonitorInstances
    monitorInstances_dryRun,
    monitorInstances_instanceIds,
    monitorInstancesResponse_instanceMonitorings,
    monitorInstancesResponse_httpStatus,

    -- ** ModifyVpnConnection
    modifyVpnConnection_dryRun,
    modifyVpnConnection_customerGatewayId,
    modifyVpnConnection_vpnGatewayId,
    modifyVpnConnection_transitGatewayId,
    modifyVpnConnection_vpnConnectionId,
    modifyVpnConnectionResponse_vpnConnection,
    modifyVpnConnectionResponse_httpStatus,

    -- ** DescribeSpotInstanceRequests
    describeSpotInstanceRequests_nextToken,
    describeSpotInstanceRequests_dryRun,
    describeSpotInstanceRequests_maxResults,
    describeSpotInstanceRequests_spotInstanceRequestIds,
    describeSpotInstanceRequests_filters,
    describeSpotInstanceRequestsResponse_nextToken,
    describeSpotInstanceRequestsResponse_spotInstanceRequests,
    describeSpotInstanceRequestsResponse_httpStatus,

    -- ** CancelConversionTask
    cancelConversionTask_dryRun,
    cancelConversionTask_reasonMessage,
    cancelConversionTask_conversionTaskId,

    -- ** ModifyVpcEndpointServiceConfiguration
    modifyVpcEndpointServiceConfiguration_removeNetworkLoadBalancerArns,
    modifyVpcEndpointServiceConfiguration_dryRun,
    modifyVpcEndpointServiceConfiguration_addNetworkLoadBalancerArns,
    modifyVpcEndpointServiceConfiguration_privateDnsName,
    modifyVpcEndpointServiceConfiguration_acceptanceRequired,
    modifyVpcEndpointServiceConfiguration_removePrivateDnsName,
    modifyVpcEndpointServiceConfiguration_addGatewayLoadBalancerArns,
    modifyVpcEndpointServiceConfiguration_removeGatewayLoadBalancerArns,
    modifyVpcEndpointServiceConfiguration_serviceId,
    modifyVpcEndpointServiceConfigurationResponse_return,
    modifyVpcEndpointServiceConfigurationResponse_httpStatus,

    -- ** ModifyTransitGatewayVpcAttachment
    modifyTransitGatewayVpcAttachment_dryRun,
    modifyTransitGatewayVpcAttachment_removeSubnetIds,
    modifyTransitGatewayVpcAttachment_options,
    modifyTransitGatewayVpcAttachment_addSubnetIds,
    modifyTransitGatewayVpcAttachment_transitGatewayAttachmentId,
    modifyTransitGatewayVpcAttachmentResponse_transitGatewayVpcAttachment,
    modifyTransitGatewayVpcAttachmentResponse_httpStatus,

    -- ** AssociateRouteTable
    associateRouteTable_dryRun,
    associateRouteTable_subnetId,
    associateRouteTable_gatewayId,
    associateRouteTable_routeTableId,
    associateRouteTableResponse_associationState,
    associateRouteTableResponse_associationId,
    associateRouteTableResponse_httpStatus,

    -- ** DescribeAccountAttributes
    describeAccountAttributes_dryRun,
    describeAccountAttributes_attributeNames,
    describeAccountAttributesResponse_accountAttributes,
    describeAccountAttributesResponse_httpStatus,

    -- ** DescribeLaunchTemplates
    describeLaunchTemplates_nextToken,
    describeLaunchTemplates_launchTemplateNames,
    describeLaunchTemplates_dryRun,
    describeLaunchTemplates_maxResults,
    describeLaunchTemplates_launchTemplateIds,
    describeLaunchTemplates_filters,
    describeLaunchTemplatesResponse_nextToken,
    describeLaunchTemplatesResponse_launchTemplates,
    describeLaunchTemplatesResponse_httpStatus,

    -- ** DescribeIpv6Pools
    describeIpv6Pools_nextToken,
    describeIpv6Pools_dryRun,
    describeIpv6Pools_maxResults,
    describeIpv6Pools_filters,
    describeIpv6Pools_poolIds,
    describeIpv6PoolsResponse_nextToken,
    describeIpv6PoolsResponse_ipv6Pools,
    describeIpv6PoolsResponse_httpStatus,

    -- ** DescribeLocalGateways
    describeLocalGateways_nextToken,
    describeLocalGateways_dryRun,
    describeLocalGateways_maxResults,
    describeLocalGateways_localGatewayIds,
    describeLocalGateways_filters,
    describeLocalGatewaysResponse_nextToken,
    describeLocalGatewaysResponse_localGateways,
    describeLocalGatewaysResponse_httpStatus,

    -- ** PurchaseHostReservation
    purchaseHostReservation_tagSpecifications,
    purchaseHostReservation_currencyCode,
    purchaseHostReservation_limitPrice,
    purchaseHostReservation_clientToken,
    purchaseHostReservation_hostIdSet,
    purchaseHostReservation_offeringId,
    purchaseHostReservationResponse_totalUpfrontPrice,
    purchaseHostReservationResponse_currencyCode,
    purchaseHostReservationResponse_purchase,
    purchaseHostReservationResponse_totalHourlyPrice,
    purchaseHostReservationResponse_clientToken,
    purchaseHostReservationResponse_httpStatus,

    -- ** ReportInstanceStatus
    reportInstanceStatus_dryRun,
    reportInstanceStatus_startTime,
    reportInstanceStatus_endTime,
    reportInstanceStatus_description,
    reportInstanceStatus_instances,
    reportInstanceStatus_reasonCodes,
    reportInstanceStatus_status,

    -- ** ModifyVpcEndpointServicePermissions
    modifyVpcEndpointServicePermissions_dryRun,
    modifyVpcEndpointServicePermissions_addAllowedPrincipals,
    modifyVpcEndpointServicePermissions_removeAllowedPrincipals,
    modifyVpcEndpointServicePermissions_serviceId,
    modifyVpcEndpointServicePermissionsResponse_returnValue,
    modifyVpcEndpointServicePermissionsResponse_httpStatus,

    -- ** ModifyHosts
    modifyHosts_instanceFamily,
    modifyHosts_instanceType,
    modifyHosts_autoPlacement,
    modifyHosts_hostRecovery,
    modifyHosts_hostIds,
    modifyHostsResponse_unsuccessful,
    modifyHostsResponse_successful,
    modifyHostsResponse_httpStatus,

    -- ** UnassignIpv6Addresses
    unassignIpv6Addresses_ipv6Addresses,
    unassignIpv6Addresses_networkInterfaceId,
    unassignIpv6AddressesResponse_unassignedIpv6Addresses,
    unassignIpv6AddressesResponse_networkInterfaceId,
    unassignIpv6AddressesResponse_httpStatus,

    -- ** GetManagedPrefixListAssociations
    getManagedPrefixListAssociations_nextToken,
    getManagedPrefixListAssociations_dryRun,
    getManagedPrefixListAssociations_maxResults,
    getManagedPrefixListAssociations_prefixListId,
    getManagedPrefixListAssociationsResponse_nextToken,
    getManagedPrefixListAssociationsResponse_prefixListAssociations,
    getManagedPrefixListAssociationsResponse_httpStatus,

    -- ** DisableFastSnapshotRestores
    disableFastSnapshotRestores_dryRun,
    disableFastSnapshotRestores_availabilityZones,
    disableFastSnapshotRestores_sourceSnapshotIds,
    disableFastSnapshotRestoresResponse_unsuccessful,
    disableFastSnapshotRestoresResponse_successful,
    disableFastSnapshotRestoresResponse_httpStatus,

    -- ** DeleteEgressOnlyInternetGateway
    deleteEgressOnlyInternetGateway_dryRun,
    deleteEgressOnlyInternetGateway_egressOnlyInternetGatewayId,
    deleteEgressOnlyInternetGatewayResponse_returnCode,
    deleteEgressOnlyInternetGatewayResponse_httpStatus,

    -- ** RequestSpotInstances
    requestSpotInstances_tagSpecifications,
    requestSpotInstances_dryRun,
    requestSpotInstances_validFrom,
    requestSpotInstances_spotPrice,
    requestSpotInstances_blockDurationMinutes,
    requestSpotInstances_launchGroup,
    requestSpotInstances_instanceInterruptionBehavior,
    requestSpotInstances_validUntil,
    requestSpotInstances_launchSpecification,
    requestSpotInstances_type,
    requestSpotInstances_availabilityZoneGroup,
    requestSpotInstances_clientToken,
    requestSpotInstances_instanceCount,
    requestSpotInstancesResponse_spotInstanceRequests,
    requestSpotInstancesResponse_httpStatus,

    -- ** RunInstances
    runInstances_securityGroupIds,
    runInstances_additionalInfo,
    runInstances_tagSpecifications,
    runInstances_instanceType,
    runInstances_capacityReservationSpecification,
    runInstances_ebsOptimized,
    runInstances_userData,
    runInstances_placement,
    runInstances_ipv6Addresses,
    runInstances_dryRun,
    runInstances_ramdiskId,
    runInstances_creditSpecification,
    runInstances_instanceMarketOptions,
    runInstances_launchTemplate,
    runInstances_licenseSpecifications,
    runInstances_instanceInitiatedShutdownBehavior,
    runInstances_imageId,
    runInstances_securityGroups,
    runInstances_elasticGpuSpecification,
    runInstances_elasticInferenceAccelerators,
    runInstances_iamInstanceProfile,
    runInstances_hibernationOptions,
    runInstances_ipv6AddressCount,
    runInstances_monitoring,
    runInstances_blockDeviceMappings,
    runInstances_subnetId,
    runInstances_enclaveOptions,
    runInstances_kernelId,
    runInstances_cpuOptions,
    runInstances_keyName,
    runInstances_networkInterfaces,
    runInstances_disableApiTermination,
    runInstances_metadataOptions,
    runInstances_clientToken,
    runInstances_privateIpAddress,
    runInstances_maxCount,
    runInstances_minCount,
    reservation_groups,
    reservation_requesterId,
    reservation_instances,
    reservation_reservationId,
    reservation_ownerId,

    -- ** GetTransitGatewayRouteTablePropagations
    getTransitGatewayRouteTablePropagations_nextToken,
    getTransitGatewayRouteTablePropagations_dryRun,
    getTransitGatewayRouteTablePropagations_maxResults,
    getTransitGatewayRouteTablePropagations_filters,
    getTransitGatewayRouteTablePropagations_transitGatewayRouteTableId,
    getTransitGatewayRouteTablePropagationsResponse_nextToken,
    getTransitGatewayRouteTablePropagationsResponse_transitGatewayRouteTablePropagations,
    getTransitGatewayRouteTablePropagationsResponse_httpStatus,

    -- ** AttachVolume
    attachVolume_dryRun,
    attachVolume_device,
    attachVolume_instanceId,
    attachVolume_volumeId,
    volumeAttachment_instanceId,
    volumeAttachment_attachTime,
    volumeAttachment_device,
    volumeAttachment_volumeId,
    volumeAttachment_state,
    volumeAttachment_deleteOnTermination,

    -- ** AcceptVpcEndpointConnections
    acceptVpcEndpointConnections_dryRun,
    acceptVpcEndpointConnections_serviceId,
    acceptVpcEndpointConnections_vpcEndpointIds,
    acceptVpcEndpointConnectionsResponse_unsuccessful,
    acceptVpcEndpointConnectionsResponse_httpStatus,

    -- ** CreateDhcpOptions
    createDhcpOptions_tagSpecifications,
    createDhcpOptions_dryRun,
    createDhcpOptions_dhcpConfigurations,
    createDhcpOptionsResponse_dhcpOptions,
    createDhcpOptionsResponse_httpStatus,

    -- ** RebootInstances
    rebootInstances_dryRun,
    rebootInstances_instanceIds,

    -- ** ModifyImageAttribute
    modifyImageAttribute_dryRun,
    modifyImageAttribute_productCodes,
    modifyImageAttribute_userIds,
    modifyImageAttribute_attribute,
    modifyImageAttribute_launchPermission,
    modifyImageAttribute_description,
    modifyImageAttribute_value,
    modifyImageAttribute_userGroups,
    modifyImageAttribute_operationType,
    modifyImageAttribute_imageId,

    -- ** CreateManagedPrefixList
    createManagedPrefixList_tagSpecifications,
    createManagedPrefixList_dryRun,
    createManagedPrefixList_clientToken,
    createManagedPrefixList_entries,
    createManagedPrefixList_prefixListName,
    createManagedPrefixList_maxEntries,
    createManagedPrefixList_addressFamily,
    createManagedPrefixListResponse_prefixList,
    createManagedPrefixListResponse_httpStatus,

    -- ** SearchTransitGatewayRoutes
    searchTransitGatewayRoutes_dryRun,
    searchTransitGatewayRoutes_maxResults,
    searchTransitGatewayRoutes_transitGatewayRouteTableId,
    searchTransitGatewayRoutes_filters,
    searchTransitGatewayRoutesResponse_routes,
    searchTransitGatewayRoutesResponse_additionalRoutesAvailable,
    searchTransitGatewayRoutesResponse_httpStatus,

    -- ** DescribeIdFormat
    describeIdFormat_resource,
    describeIdFormatResponse_statuses,
    describeIdFormatResponse_httpStatus,

    -- ** RegisterTransitGatewayMulticastGroupSources
    registerTransitGatewayMulticastGroupSources_dryRun,
    registerTransitGatewayMulticastGroupSources_transitGatewayMulticastDomainId,
    registerTransitGatewayMulticastGroupSources_networkInterfaceIds,
    registerTransitGatewayMulticastGroupSources_groupIpAddress,
    registerTransitGatewayMulticastGroupSourcesResponse_registeredMulticastGroupSources,
    registerTransitGatewayMulticastGroupSourcesResponse_httpStatus,

    -- ** DescribeVpcEndpointConnectionNotifications
    describeVpcEndpointConnectionNotifications_nextToken,
    describeVpcEndpointConnectionNotifications_dryRun,
    describeVpcEndpointConnectionNotifications_connectionNotificationId,
    describeVpcEndpointConnectionNotifications_maxResults,
    describeVpcEndpointConnectionNotifications_filters,
    describeVpcEndpointConnectionNotificationsResponse_nextToken,
    describeVpcEndpointConnectionNotificationsResponse_connectionNotificationSet,
    describeVpcEndpointConnectionNotificationsResponse_httpStatus,

    -- ** DescribeVpcs
    describeVpcs_nextToken,
    describeVpcs_dryRun,
    describeVpcs_maxResults,
    describeVpcs_filters,
    describeVpcs_vpcIds,
    describeVpcsResponse_nextToken,
    describeVpcsResponse_vpcs,
    describeVpcsResponse_httpStatus,

    -- ** GetTransitGatewayPrefixListReferences
    getTransitGatewayPrefixListReferences_nextToken,
    getTransitGatewayPrefixListReferences_dryRun,
    getTransitGatewayPrefixListReferences_maxResults,
    getTransitGatewayPrefixListReferences_filters,
    getTransitGatewayPrefixListReferences_transitGatewayRouteTableId,
    getTransitGatewayPrefixListReferencesResponse_nextToken,
    getTransitGatewayPrefixListReferencesResponse_transitGatewayPrefixListReferences,
    getTransitGatewayPrefixListReferencesResponse_httpStatus,

    -- ** CreateRouteTable
    createRouteTable_tagSpecifications,
    createRouteTable_dryRun,
    createRouteTable_vpcId,
    createRouteTableResponse_routeTable,
    createRouteTableResponse_httpStatus,

    -- ** DescribeVolumesModifications
    describeVolumesModifications_nextToken,
    describeVolumesModifications_dryRun,
    describeVolumesModifications_volumeIds,
    describeVolumesModifications_maxResults,
    describeVolumesModifications_filters,
    describeVolumesModificationsResponse_nextToken,
    describeVolumesModificationsResponse_volumesModifications,
    describeVolumesModificationsResponse_httpStatus,

    -- ** AssociateIamInstanceProfile
    associateIamInstanceProfile_iamInstanceProfile,
    associateIamInstanceProfile_instanceId,
    associateIamInstanceProfileResponse_iamInstanceProfileAssociation,
    associateIamInstanceProfileResponse_httpStatus,

    -- ** CreateImage
    createImage_tagSpecifications,
    createImage_dryRun,
    createImage_blockDeviceMappings,
    createImage_description,
    createImage_noReboot,
    createImage_instanceId,
    createImage_name,
    createImageResponse_imageId,
    createImageResponse_httpStatus,

    -- ** DescribeTrafficMirrorTargets
    describeTrafficMirrorTargets_nextToken,
    describeTrafficMirrorTargets_dryRun,
    describeTrafficMirrorTargets_maxResults,
    describeTrafficMirrorTargets_filters,
    describeTrafficMirrorTargets_trafficMirrorTargetIds,
    describeTrafficMirrorTargetsResponse_nextToken,
    describeTrafficMirrorTargetsResponse_trafficMirrorTargets,
    describeTrafficMirrorTargetsResponse_httpStatus,

    -- ** AssociateDhcpOptions
    associateDhcpOptions_dryRun,
    associateDhcpOptions_dhcpOptionsId,
    associateDhcpOptions_vpcId,

    -- ** DescribeSpotFleetRequestHistory
    describeSpotFleetRequestHistory_nextToken,
    describeSpotFleetRequestHistory_eventType,
    describeSpotFleetRequestHistory_dryRun,
    describeSpotFleetRequestHistory_maxResults,
    describeSpotFleetRequestHistory_spotFleetRequestId,
    describeSpotFleetRequestHistory_startTime,
    describeSpotFleetRequestHistoryResponse_nextToken,
    describeSpotFleetRequestHistoryResponse_startTime,
    describeSpotFleetRequestHistoryResponse_historyRecords,
    describeSpotFleetRequestHistoryResponse_lastEvaluatedTime,
    describeSpotFleetRequestHistoryResponse_spotFleetRequestId,
    describeSpotFleetRequestHistoryResponse_httpStatus,

    -- ** ModifyInstanceEventStartTime
    modifyInstanceEventStartTime_dryRun,
    modifyInstanceEventStartTime_instanceId,
    modifyInstanceEventStartTime_instanceEventId,
    modifyInstanceEventStartTime_notBefore,
    modifyInstanceEventStartTimeResponse_event,
    modifyInstanceEventStartTimeResponse_httpStatus,

    -- ** DisassociateEnclaveCertificateIamRole
    disassociateEnclaveCertificateIamRole_roleArn,
    disassociateEnclaveCertificateIamRole_dryRun,
    disassociateEnclaveCertificateIamRole_certificateArn,
    disassociateEnclaveCertificateIamRoleResponse_return,
    disassociateEnclaveCertificateIamRoleResponse_httpStatus,

    -- ** DeleteVpcPeeringConnection
    deleteVpcPeeringConnection_dryRun,
    deleteVpcPeeringConnection_vpcPeeringConnectionId,
    deleteVpcPeeringConnectionResponse_return,
    deleteVpcPeeringConnectionResponse_httpStatus,

    -- ** ResetInstanceAttribute
    resetInstanceAttribute_dryRun,
    resetInstanceAttribute_attribute,
    resetInstanceAttribute_instanceId,

    -- ** DescribeInstanceStatus
    describeInstanceStatus_instanceIds,
    describeInstanceStatus_nextToken,
    describeInstanceStatus_dryRun,
    describeInstanceStatus_maxResults,
    describeInstanceStatus_includeAllInstances,
    describeInstanceStatus_filters,
    describeInstanceStatusResponse_nextToken,
    describeInstanceStatusResponse_instanceStatuses,
    describeInstanceStatusResponse_httpStatus,

    -- ** AttachNetworkInterface
    attachNetworkInterface_dryRun,
    attachNetworkInterface_networkCardIndex,
    attachNetworkInterface_deviceIndex,
    attachNetworkInterface_instanceId,
    attachNetworkInterface_networkInterfaceId,
    attachNetworkInterfaceResponse_attachmentId,
    attachNetworkInterfaceResponse_networkCardIndex,
    attachNetworkInterfaceResponse_httpStatus,

    -- ** AssignIpv6Addresses
    assignIpv6Addresses_ipv6Addresses,
    assignIpv6Addresses_ipv6AddressCount,
    assignIpv6Addresses_networkInterfaceId,
    assignIpv6AddressesResponse_assignedIpv6Addresses,
    assignIpv6AddressesResponse_networkInterfaceId,
    assignIpv6AddressesResponse_httpStatus,

    -- ** CreateLocalGatewayRoute
    createLocalGatewayRoute_dryRun,
    createLocalGatewayRoute_destinationCidrBlock,
    createLocalGatewayRoute_localGatewayRouteTableId,
    createLocalGatewayRoute_localGatewayVirtualInterfaceGroupId,
    createLocalGatewayRouteResponse_route,
    createLocalGatewayRouteResponse_httpStatus,

    -- ** EnableVgwRoutePropagation
    enableVgwRoutePropagation_dryRun,
    enableVgwRoutePropagation_gatewayId,
    enableVgwRoutePropagation_routeTableId,

    -- ** DescribeVpcEndpoints
    describeVpcEndpoints_nextToken,
    describeVpcEndpoints_dryRun,
    describeVpcEndpoints_maxResults,
    describeVpcEndpoints_filters,
    describeVpcEndpoints_vpcEndpointIds,
    describeVpcEndpointsResponse_nextToken,
    describeVpcEndpointsResponse_vpcEndpoints,
    describeVpcEndpointsResponse_httpStatus,

    -- ** CreateNetworkAclEntry
    createNetworkAclEntry_dryRun,
    createNetworkAclEntry_portRange,
    createNetworkAclEntry_icmpTypeCode,
    createNetworkAclEntry_ipv6CidrBlock,
    createNetworkAclEntry_cidrBlock,
    createNetworkAclEntry_egress,
    createNetworkAclEntry_networkAclId,
    createNetworkAclEntry_protocol,
    createNetworkAclEntry_ruleAction,
    createNetworkAclEntry_ruleNumber,

    -- ** DescribeStaleSecurityGroups
    describeStaleSecurityGroups_nextToken,
    describeStaleSecurityGroups_dryRun,
    describeStaleSecurityGroups_maxResults,
    describeStaleSecurityGroups_vpcId,
    describeStaleSecurityGroupsResponse_nextToken,
    describeStaleSecurityGroupsResponse_staleSecurityGroupSet,
    describeStaleSecurityGroupsResponse_httpStatus,

    -- ** DescribeFlowLogs
    describeFlowLogs_nextToken,
    describeFlowLogs_dryRun,
    describeFlowLogs_maxResults,
    describeFlowLogs_flowLogIds,
    describeFlowLogs_filter,
    describeFlowLogsResponse_nextToken,
    describeFlowLogsResponse_flowLogs,
    describeFlowLogsResponse_httpStatus,

    -- ** DescribePlacementGroups
    describePlacementGroups_groupIds,
    describePlacementGroups_dryRun,
    describePlacementGroups_groupNames,
    describePlacementGroups_filters,
    describePlacementGroupsResponse_placementGroups,
    describePlacementGroupsResponse_httpStatus,

    -- ** DescribeFleets
    describeFleets_nextToken,
    describeFleets_dryRun,
    describeFleets_maxResults,
    describeFleets_filters,
    describeFleets_fleetIds,
    describeFleetsResponse_nextToken,
    describeFleetsResponse_fleets,
    describeFleetsResponse_httpStatus,

    -- ** ModifyIdentityIdFormat
    modifyIdentityIdFormat_principalArn,
    modifyIdentityIdFormat_resource,
    modifyIdentityIdFormat_useLongIds,

    -- ** DescribeLocalGatewayVirtualInterfaceGroups
    describeLocalGatewayVirtualInterfaceGroups_nextToken,
    describeLocalGatewayVirtualInterfaceGroups_dryRun,
    describeLocalGatewayVirtualInterfaceGroups_maxResults,
    describeLocalGatewayVirtualInterfaceGroups_filters,
    describeLocalGatewayVirtualInterfaceGroups_localGatewayVirtualInterfaceGroupIds,
    describeLocalGatewayVirtualInterfaceGroupsResponse_nextToken,
    describeLocalGatewayVirtualInterfaceGroupsResponse_localGatewayVirtualInterfaceGroups,
    describeLocalGatewayVirtualInterfaceGroupsResponse_httpStatus,

    -- ** ReplaceNetworkAclEntry
    replaceNetworkAclEntry_dryRun,
    replaceNetworkAclEntry_portRange,
    replaceNetworkAclEntry_icmpTypeCode,
    replaceNetworkAclEntry_ipv6CidrBlock,
    replaceNetworkAclEntry_cidrBlock,
    replaceNetworkAclEntry_egress,
    replaceNetworkAclEntry_networkAclId,
    replaceNetworkAclEntry_protocol,
    replaceNetworkAclEntry_ruleAction,
    replaceNetworkAclEntry_ruleNumber,

    -- ** DeleteTags
    deleteTags_dryRun,
    deleteTags_tags,
    deleteTags_resources,

    -- ** DescribeTransitGatewayAttachments
    describeTransitGatewayAttachments_nextToken,
    describeTransitGatewayAttachments_dryRun,
    describeTransitGatewayAttachments_maxResults,
    describeTransitGatewayAttachments_transitGatewayAttachmentIds,
    describeTransitGatewayAttachments_filters,
    describeTransitGatewayAttachmentsResponse_nextToken,
    describeTransitGatewayAttachmentsResponse_transitGatewayAttachments,
    describeTransitGatewayAttachmentsResponse_httpStatus,

    -- ** DescribeReservedInstancesOfferings
    describeReservedInstancesOfferings_minDuration,
    describeReservedInstancesOfferings_nextToken,
    describeReservedInstancesOfferings_instanceType,
    describeReservedInstancesOfferings_dryRun,
    describeReservedInstancesOfferings_maxInstanceCount,
    describeReservedInstancesOfferings_maxResults,
    describeReservedInstancesOfferings_includeMarketplace,
    describeReservedInstancesOfferings_instanceTenancy,
    describeReservedInstancesOfferings_availabilityZone,
    describeReservedInstancesOfferings_offeringClass,
    describeReservedInstancesOfferings_filters,
    describeReservedInstancesOfferings_offeringType,
    describeReservedInstancesOfferings_reservedInstancesOfferingIds,
    describeReservedInstancesOfferings_productDescription,
    describeReservedInstancesOfferings_maxDuration,
    describeReservedInstancesOfferingsResponse_nextToken,
    describeReservedInstancesOfferingsResponse_reservedInstancesOfferings,
    describeReservedInstancesOfferingsResponse_httpStatus,

    -- ** ModifySnapshotAttribute
    modifySnapshotAttribute_createVolumePermission,
    modifySnapshotAttribute_dryRun,
    modifySnapshotAttribute_groupNames,
    modifySnapshotAttribute_userIds,
    modifySnapshotAttribute_attribute,
    modifySnapshotAttribute_operationType,
    modifySnapshotAttribute_snapshotId,

    -- ** ConfirmProductInstance
    confirmProductInstance_dryRun,
    confirmProductInstance_instanceId,
    confirmProductInstance_productCode,
    confirmProductInstanceResponse_ownerId,
    confirmProductInstanceResponse_return,
    confirmProductInstanceResponse_httpStatus,

    -- ** DescribeVpnConnections
    describeVpnConnections_dryRun,
    describeVpnConnections_filters,
    describeVpnConnections_vpnConnectionIds,
    describeVpnConnectionsResponse_vpnConnections,
    describeVpnConnectionsResponse_httpStatus,

    -- ** ModifyAvailabilityZoneGroup
    modifyAvailabilityZoneGroup_dryRun,
    modifyAvailabilityZoneGroup_groupName,
    modifyAvailabilityZoneGroup_optInStatus,
    modifyAvailabilityZoneGroupResponse_return,
    modifyAvailabilityZoneGroupResponse_httpStatus,

    -- ** DisassociateIamInstanceProfile
    disassociateIamInstanceProfile_associationId,
    disassociateIamInstanceProfileResponse_iamInstanceProfileAssociation,
    disassociateIamInstanceProfileResponse_httpStatus,

    -- ** DisableVpcClassicLink
    disableVpcClassicLink_dryRun,
    disableVpcClassicLink_vpcId,
    disableVpcClassicLinkResponse_return,
    disableVpcClassicLinkResponse_httpStatus,

    -- ** GetGroupsForCapacityReservation
    getGroupsForCapacityReservation_nextToken,
    getGroupsForCapacityReservation_dryRun,
    getGroupsForCapacityReservation_maxResults,
    getGroupsForCapacityReservation_capacityReservationId,
    getGroupsForCapacityReservationResponse_nextToken,
    getGroupsForCapacityReservationResponse_capacityReservationGroups,
    getGroupsForCapacityReservationResponse_httpStatus,

    -- ** ImportVolume
    importVolume_dryRun,
    importVolume_description,
    importVolume_availabilityZone,
    importVolume_image,
    importVolume_volume,
    importVolumeResponse_conversionTask,
    importVolumeResponse_httpStatus,

    -- ** DescribeAddresses
    describeAddresses_dryRun,
    describeAddresses_allocationIds,
    describeAddresses_publicIps,
    describeAddresses_filters,
    describeAddressesResponse_addresses,
    describeAddressesResponse_httpStatus,

    -- ** DeleteLocalGatewayRoute
    deleteLocalGatewayRoute_dryRun,
    deleteLocalGatewayRoute_destinationCidrBlock,
    deleteLocalGatewayRoute_localGatewayRouteTableId,
    deleteLocalGatewayRouteResponse_route,
    deleteLocalGatewayRouteResponse_httpStatus,

    -- ** DescribeVpcEndpointServiceConfigurations
    describeVpcEndpointServiceConfigurations_nextToken,
    describeVpcEndpointServiceConfigurations_dryRun,
    describeVpcEndpointServiceConfigurations_maxResults,
    describeVpcEndpointServiceConfigurations_serviceIds,
    describeVpcEndpointServiceConfigurations_filters,
    describeVpcEndpointServiceConfigurationsResponse_nextToken,
    describeVpcEndpointServiceConfigurationsResponse_serviceConfigurations,
    describeVpcEndpointServiceConfigurationsResponse_httpStatus,

    -- ** DescribeNetworkInterfaces
    describeNetworkInterfaces_nextToken,
    describeNetworkInterfaces_dryRun,
    describeNetworkInterfaces_maxResults,
    describeNetworkInterfaces_networkInterfaceIds,
    describeNetworkInterfaces_filters,
    describeNetworkInterfacesResponse_nextToken,
    describeNetworkInterfacesResponse_networkInterfaces,
    describeNetworkInterfacesResponse_httpStatus,

    -- ** DescribeVpcEndpointServices
    describeVpcEndpointServices_nextToken,
    describeVpcEndpointServices_serviceNames,
    describeVpcEndpointServices_dryRun,
    describeVpcEndpointServices_maxResults,
    describeVpcEndpointServices_filters,
    describeVpcEndpointServicesResponse_serviceDetails,
    describeVpcEndpointServicesResponse_nextToken,
    describeVpcEndpointServicesResponse_serviceNames,
    describeVpcEndpointServicesResponse_httpStatus,

    -- ** DeleteNetworkAclEntry
    deleteNetworkAclEntry_dryRun,
    deleteNetworkAclEntry_egress,
    deleteNetworkAclEntry_networkAclId,
    deleteNetworkAclEntry_ruleNumber,

    -- ** GetTransitGatewayAttachmentPropagations
    getTransitGatewayAttachmentPropagations_nextToken,
    getTransitGatewayAttachmentPropagations_dryRun,
    getTransitGatewayAttachmentPropagations_maxResults,
    getTransitGatewayAttachmentPropagations_filters,
    getTransitGatewayAttachmentPropagations_transitGatewayAttachmentId,
    getTransitGatewayAttachmentPropagationsResponse_nextToken,
    getTransitGatewayAttachmentPropagationsResponse_transitGatewayAttachmentPropagations,
    getTransitGatewayAttachmentPropagationsResponse_httpStatus,

    -- ** AssignPrivateIpAddresses
    assignPrivateIpAddresses_privateIpAddresses,
    assignPrivateIpAddresses_secondaryPrivateIpAddressCount,
    assignPrivateIpAddresses_allowReassignment,
    assignPrivateIpAddresses_networkInterfaceId,
    assignPrivateIpAddressesResponse_assignedPrivateIpAddresses,
    assignPrivateIpAddressesResponse_networkInterfaceId,
    assignPrivateIpAddressesResponse_httpStatus,

    -- ** DescribeNatGateways
    describeNatGateways_nextToken,
    describeNatGateways_dryRun,
    describeNatGateways_maxResults,
    describeNatGateways_natGatewayIds,
    describeNatGateways_filter,
    describeNatGatewaysResponse_nextToken,
    describeNatGatewaysResponse_natGateways,
    describeNatGatewaysResponse_httpStatus,

    -- ** DescribeSnapshotAttribute
    describeSnapshotAttribute_dryRun,
    describeSnapshotAttribute_attribute,
    describeSnapshotAttribute_snapshotId,
    describeSnapshotAttributeResponse_productCodes,
    describeSnapshotAttributeResponse_createVolumePermissions,
    describeSnapshotAttributeResponse_snapshotId,
    describeSnapshotAttributeResponse_httpStatus,

    -- ** DeleteSnapshot
    deleteSnapshot_dryRun,
    deleteSnapshot_snapshotId,

    -- ** DeleteCarrierGateway
    deleteCarrierGateway_dryRun,
    deleteCarrierGateway_carrierGatewayId,
    deleteCarrierGatewayResponse_carrierGateway,
    deleteCarrierGatewayResponse_httpStatus,

    -- ** DescribeTransitGatewayVpcAttachments
    describeTransitGatewayVpcAttachments_nextToken,
    describeTransitGatewayVpcAttachments_dryRun,
    describeTransitGatewayVpcAttachments_maxResults,
    describeTransitGatewayVpcAttachments_transitGatewayAttachmentIds,
    describeTransitGatewayVpcAttachments_filters,
    describeTransitGatewayVpcAttachmentsResponse_nextToken,
    describeTransitGatewayVpcAttachmentsResponse_transitGatewayVpcAttachments,
    describeTransitGatewayVpcAttachmentsResponse_httpStatus,

    -- ** ModifyVpcEndpointConnectionNotification
    modifyVpcEndpointConnectionNotification_connectionEvents,
    modifyVpcEndpointConnectionNotification_dryRun,
    modifyVpcEndpointConnectionNotification_connectionNotificationArn,
    modifyVpcEndpointConnectionNotification_connectionNotificationId,
    modifyVpcEndpointConnectionNotificationResponse_returnValue,
    modifyVpcEndpointConnectionNotificationResponse_httpStatus,

    -- ** PurchaseReservedInstancesOffering
    purchaseReservedInstancesOffering_dryRun,
    purchaseReservedInstancesOffering_purchaseTime,
    purchaseReservedInstancesOffering_limitPrice,
    purchaseReservedInstancesOffering_instanceCount,
    purchaseReservedInstancesOffering_reservedInstancesOfferingId,
    purchaseReservedInstancesOfferingResponse_reservedInstancesId,
    purchaseReservedInstancesOfferingResponse_httpStatus,

    -- ** AuthorizeSecurityGroupIngress
    authorizeSecurityGroupIngress_fromPort,
    authorizeSecurityGroupIngress_dryRun,
    authorizeSecurityGroupIngress_sourceSecurityGroupName,
    authorizeSecurityGroupIngress_groupName,
    authorizeSecurityGroupIngress_cidrIp,
    authorizeSecurityGroupIngress_groupId,
    authorizeSecurityGroupIngress_ipProtocol,
    authorizeSecurityGroupIngress_ipPermissions,
    authorizeSecurityGroupIngress_sourceSecurityGroupOwnerId,
    authorizeSecurityGroupIngress_toPort,

    -- ** GetConsoleScreenshot
    getConsoleScreenshot_dryRun,
    getConsoleScreenshot_wakeUp,
    getConsoleScreenshot_instanceId,
    getConsoleScreenshotResponse_instanceId,
    getConsoleScreenshotResponse_imageData,
    getConsoleScreenshotResponse_httpStatus,

    -- ** DisableVgwRoutePropagation
    disableVgwRoutePropagation_dryRun,
    disableVgwRoutePropagation_gatewayId,
    disableVgwRoutePropagation_routeTableId,

    -- ** DescribeTransitGatewayMulticastDomains
    describeTransitGatewayMulticastDomains_nextToken,
    describeTransitGatewayMulticastDomains_dryRun,
    describeTransitGatewayMulticastDomains_maxResults,
    describeTransitGatewayMulticastDomains_filters,
    describeTransitGatewayMulticastDomains_transitGatewayMulticastDomainIds,
    describeTransitGatewayMulticastDomainsResponse_nextToken,
    describeTransitGatewayMulticastDomainsResponse_transitGatewayMulticastDomains,
    describeTransitGatewayMulticastDomainsResponse_httpStatus,

    -- ** DescribeSubnets
    describeSubnets_nextToken,
    describeSubnets_dryRun,
    describeSubnets_maxResults,
    describeSubnets_subnetIds,
    describeSubnets_filters,
    describeSubnetsResponse_nextToken,
    describeSubnetsResponse_subnets,
    describeSubnetsResponse_httpStatus,

    -- ** UnmonitorInstances
    unmonitorInstances_dryRun,
    unmonitorInstances_instanceIds,
    unmonitorInstancesResponse_instanceMonitorings,
    unmonitorInstancesResponse_httpStatus,

    -- ** CancelSpotInstanceRequests
    cancelSpotInstanceRequests_dryRun,
    cancelSpotInstanceRequests_spotInstanceRequestIds,
    cancelSpotInstanceRequestsResponse_cancelledSpotInstanceRequests,
    cancelSpotInstanceRequestsResponse_httpStatus,

    -- ** CreateSpotDatafeedSubscription
    createSpotDatafeedSubscription_dryRun,
    createSpotDatafeedSubscription_prefix,
    createSpotDatafeedSubscription_bucket,
    createSpotDatafeedSubscriptionResponse_spotDatafeedSubscription,
    createSpotDatafeedSubscriptionResponse_httpStatus,

    -- ** DisassociateRouteTable
    disassociateRouteTable_dryRun,
    disassociateRouteTable_associationId,

    -- ** DescribeTransitGatewayConnectPeers
    describeTransitGatewayConnectPeers_nextToken,
    describeTransitGatewayConnectPeers_dryRun,
    describeTransitGatewayConnectPeers_maxResults,
    describeTransitGatewayConnectPeers_filters,
    describeTransitGatewayConnectPeers_transitGatewayConnectPeerIds,
    describeTransitGatewayConnectPeersResponse_nextToken,
    describeTransitGatewayConnectPeersResponse_transitGatewayConnectPeers,
    describeTransitGatewayConnectPeersResponse_httpStatus,

    -- ** ModifyVpnTunnelCertificate
    modifyVpnTunnelCertificate_dryRun,
    modifyVpnTunnelCertificate_vpnConnectionId,
    modifyVpnTunnelCertificate_vpnTunnelOutsideIpAddress,
    modifyVpnTunnelCertificateResponse_vpnConnection,
    modifyVpnTunnelCertificateResponse_httpStatus,

    -- ** RestoreManagedPrefixListVersion
    restoreManagedPrefixListVersion_dryRun,
    restoreManagedPrefixListVersion_prefixListId,
    restoreManagedPrefixListVersion_previousVersion,
    restoreManagedPrefixListVersion_currentVersion,
    restoreManagedPrefixListVersionResponse_prefixList,
    restoreManagedPrefixListVersionResponse_httpStatus,

    -- ** ModifyAddressAttribute
    modifyAddressAttribute_dryRun,
    modifyAddressAttribute_domainName,
    modifyAddressAttribute_allocationId,
    modifyAddressAttributeResponse_address,
    modifyAddressAttributeResponse_httpStatus,

    -- ** CreateVpnConnection
    createVpnConnection_tagSpecifications,
    createVpnConnection_dryRun,
    createVpnConnection_options,
    createVpnConnection_vpnGatewayId,
    createVpnConnection_transitGatewayId,
    createVpnConnection_customerGatewayId,
    createVpnConnection_type,
    createVpnConnectionResponse_vpnConnection,
    createVpnConnectionResponse_httpStatus,

    -- ** AssociateSubnetCidrBlock
    associateSubnetCidrBlock_ipv6CidrBlock,
    associateSubnetCidrBlock_subnetId,
    associateSubnetCidrBlockResponse_ipv6CidrBlockAssociation,
    associateSubnetCidrBlockResponse_subnetId,
    associateSubnetCidrBlockResponse_httpStatus,

    -- ** AttachClassicLinkVpc
    attachClassicLinkVpc_dryRun,
    attachClassicLinkVpc_groups,
    attachClassicLinkVpc_instanceId,
    attachClassicLinkVpc_vpcId,
    attachClassicLinkVpcResponse_return,
    attachClassicLinkVpcResponse_httpStatus,

    -- ** DescribeSpotPriceHistory
    describeSpotPriceHistory_nextToken,
    describeSpotPriceHistory_dryRun,
    describeSpotPriceHistory_maxResults,
    describeSpotPriceHistory_startTime,
    describeSpotPriceHistory_endTime,
    describeSpotPriceHistory_availabilityZone,
    describeSpotPriceHistory_productDescriptions,
    describeSpotPriceHistory_filters,
    describeSpotPriceHistory_instanceTypes,
    describeSpotPriceHistoryResponse_nextToken,
    describeSpotPriceHistoryResponse_spotPriceHistory,
    describeSpotPriceHistoryResponse_httpStatus,

    -- ** DeleteQueuedReservedInstances
    deleteQueuedReservedInstances_dryRun,
    deleteQueuedReservedInstances_reservedInstancesIds,
    deleteQueuedReservedInstancesResponse_successfulQueuedPurchaseDeletions,
    deleteQueuedReservedInstancesResponse_failedQueuedPurchaseDeletions,
    deleteQueuedReservedInstancesResponse_httpStatus,

    -- ** DescribeAggregateIdFormat
    describeAggregateIdFormat_dryRun,
    describeAggregateIdFormatResponse_useLongIdsAggregated,
    describeAggregateIdFormatResponse_statuses,
    describeAggregateIdFormatResponse_httpStatus,

    -- ** DescribeReservedInstancesListings
    describeReservedInstancesListings_reservedInstancesId,
    describeReservedInstancesListings_reservedInstancesListingId,
    describeReservedInstancesListings_filters,
    describeReservedInstancesListingsResponse_reservedInstancesListings,
    describeReservedInstancesListingsResponse_httpStatus,

    -- ** CopyImage
    copyImage_dryRun,
    copyImage_encrypted,
    copyImage_kmsKeyId,
    copyImage_destinationOutpostArn,
    copyImage_description,
    copyImage_clientToken,
    copyImage_name,
    copyImage_sourceImageId,
    copyImage_sourceRegion,
    copyImageResponse_imageId,
    copyImageResponse_httpStatus,

    -- ** CreateLocalGatewayRouteTableVpcAssociation
    createLocalGatewayRouteTableVpcAssociation_tagSpecifications,
    createLocalGatewayRouteTableVpcAssociation_dryRun,
    createLocalGatewayRouteTableVpcAssociation_localGatewayRouteTableId,
    createLocalGatewayRouteTableVpcAssociation_vpcId,
    createLocalGatewayRouteTableVpcAssociationResponse_localGatewayRouteTableVpcAssociation,
    createLocalGatewayRouteTableVpcAssociationResponse_httpStatus,

    -- ** DescribeCarrierGateways
    describeCarrierGateways_nextToken,
    describeCarrierGateways_dryRun,
    describeCarrierGateways_maxResults,
    describeCarrierGateways_carrierGatewayIds,
    describeCarrierGateways_filters,
    describeCarrierGatewaysResponse_carrierGateways,
    describeCarrierGatewaysResponse_nextToken,
    describeCarrierGatewaysResponse_httpStatus,

    -- ** DeleteInternetGateway
    deleteInternetGateway_dryRun,
    deleteInternetGateway_internetGatewayId,

    -- ** CreateFleet
    createFleet_tagSpecifications,
    createFleet_dryRun,
    createFleet_onDemandOptions,
    createFleet_validFrom,
    createFleet_replaceUnhealthyInstances,
    createFleet_validUntil,
    createFleet_excessCapacityTerminationPolicy,
    createFleet_type,
    createFleet_spotOptions,
    createFleet_clientToken,
    createFleet_terminateInstancesWithExpiration,
    createFleet_launchTemplateConfigs,
    createFleet_targetCapacitySpecification,
    createFleetResponse_fleetId,
    createFleetResponse_instances,
    createFleetResponse_errors,
    createFleetResponse_httpStatus,

    -- ** ModifyClientVpnEndpoint
    modifyClientVpnEndpoint_securityGroupIds,
    modifyClientVpnEndpoint_dryRun,
    modifyClientVpnEndpoint_serverCertificateArn,
    modifyClientVpnEndpoint_connectionLogOptions,
    modifyClientVpnEndpoint_clientConnectOptions,
    modifyClientVpnEndpoint_dnsServers,
    modifyClientVpnEndpoint_vpnPort,
    modifyClientVpnEndpoint_description,
    modifyClientVpnEndpoint_vpcId,
    modifyClientVpnEndpoint_selfServicePortal,
    modifyClientVpnEndpoint_splitTunnel,
    modifyClientVpnEndpoint_clientVpnEndpointId,
    modifyClientVpnEndpointResponse_return,
    modifyClientVpnEndpointResponse_httpStatus,

    -- ** ModifyInstanceCapacityReservationAttributes
    modifyInstanceCapacityReservationAttributes_dryRun,
    modifyInstanceCapacityReservationAttributes_instanceId,
    modifyInstanceCapacityReservationAttributes_capacityReservationSpecification,
    modifyInstanceCapacityReservationAttributesResponse_return,
    modifyInstanceCapacityReservationAttributesResponse_httpStatus,

    -- ** ImportClientVpnClientCertificateRevocationList
    importClientVpnClientCertificateRevocationList_dryRun,
    importClientVpnClientCertificateRevocationList_clientVpnEndpointId,
    importClientVpnClientCertificateRevocationList_certificateRevocationList,
    importClientVpnClientCertificateRevocationListResponse_return,
    importClientVpnClientCertificateRevocationListResponse_httpStatus,

    -- ** AssociateClientVpnTargetNetwork
    associateClientVpnTargetNetwork_dryRun,
    associateClientVpnTargetNetwork_clientToken,
    associateClientVpnTargetNetwork_clientVpnEndpointId,
    associateClientVpnTargetNetwork_subnetId,
    associateClientVpnTargetNetworkResponse_status,
    associateClientVpnTargetNetworkResponse_associationId,
    associateClientVpnTargetNetworkResponse_httpStatus,

    -- ** CancelCapacityReservation
    cancelCapacityReservation_dryRun,
    cancelCapacityReservation_capacityReservationId,
    cancelCapacityReservationResponse_return,
    cancelCapacityReservationResponse_httpStatus,

    -- ** CancelReservedInstancesListing
    cancelReservedInstancesListing_reservedInstancesListingId,
    cancelReservedInstancesListingResponse_reservedInstancesListings,
    cancelReservedInstancesListingResponse_httpStatus,

    -- ** DisableTransitGatewayRouteTablePropagation
    disableTransitGatewayRouteTablePropagation_dryRun,
    disableTransitGatewayRouteTablePropagation_transitGatewayRouteTableId,
    disableTransitGatewayRouteTablePropagation_transitGatewayAttachmentId,
    disableTransitGatewayRouteTablePropagationResponse_propagation,
    disableTransitGatewayRouteTablePropagationResponse_httpStatus,

    -- ** DescribeVpcClassicLinkDnsSupport
    describeVpcClassicLinkDnsSupport_nextToken,
    describeVpcClassicLinkDnsSupport_maxResults,
    describeVpcClassicLinkDnsSupport_vpcIds,
    describeVpcClassicLinkDnsSupportResponse_nextToken,
    describeVpcClassicLinkDnsSupportResponse_vpcs,
    describeVpcClassicLinkDnsSupportResponse_httpStatus,

    -- ** CreateVpcEndpoint
    createVpcEndpoint_securityGroupIds,
    createVpcEndpoint_policyDocument,
    createVpcEndpoint_tagSpecifications,
    createVpcEndpoint_routeTableIds,
    createVpcEndpoint_dryRun,
    createVpcEndpoint_vpcEndpointType,
    createVpcEndpoint_subnetIds,
    createVpcEndpoint_privateDnsEnabled,
    createVpcEndpoint_clientToken,
    createVpcEndpoint_vpcId,
    createVpcEndpoint_serviceName,
    createVpcEndpointResponse_vpcEndpoint,
    createVpcEndpointResponse_clientToken,
    createVpcEndpointResponse_httpStatus,

    -- ** DescribeSnapshots
    describeSnapshots_ownerIds,
    describeSnapshots_nextToken,
    describeSnapshots_dryRun,
    describeSnapshots_maxResults,
    describeSnapshots_restorableByUserIds,
    describeSnapshots_snapshotIds,
    describeSnapshots_filters,
    describeSnapshotsResponse_snapshots,
    describeSnapshotsResponse_nextToken,
    describeSnapshotsResponse_httpStatus,

    -- ** DescribeImportSnapshotTasks
    describeImportSnapshotTasks_nextToken,
    describeImportSnapshotTasks_dryRun,
    describeImportSnapshotTasks_importTaskIds,
    describeImportSnapshotTasks_maxResults,
    describeImportSnapshotTasks_filters,
    describeImportSnapshotTasksResponse_importSnapshotTasks,
    describeImportSnapshotTasksResponse_nextToken,
    describeImportSnapshotTasksResponse_httpStatus,

    -- ** DescribeNetworkInterfaceAttribute
    describeNetworkInterfaceAttribute_dryRun,
    describeNetworkInterfaceAttribute_attribute,
    describeNetworkInterfaceAttribute_networkInterfaceId,
    describeNetworkInterfaceAttributeResponse_groups,
    describeNetworkInterfaceAttributeResponse_attachment,
    describeNetworkInterfaceAttributeResponse_sourceDestCheck,
    describeNetworkInterfaceAttributeResponse_networkInterfaceId,
    describeNetworkInterfaceAttributeResponse_description,
    describeNetworkInterfaceAttributeResponse_httpStatus,

    -- ** DescribeInstanceEventNotificationAttributes
    describeInstanceEventNotificationAttributes_dryRun,
    describeInstanceEventNotificationAttributesResponse_instanceTagAttribute,
    describeInstanceEventNotificationAttributesResponse_httpStatus,

    -- ** EnableEbsEncryptionByDefault
    enableEbsEncryptionByDefault_dryRun,
    enableEbsEncryptionByDefaultResponse_ebsEncryptionByDefault,
    enableEbsEncryptionByDefaultResponse_httpStatus,

    -- ** ModifyTrafficMirrorFilterRule
    modifyTrafficMirrorFilterRule_removeFields,
    modifyTrafficMirrorFilterRule_dryRun,
    modifyTrafficMirrorFilterRule_sourcePortRange,
    modifyTrafficMirrorFilterRule_trafficDirection,
    modifyTrafficMirrorFilterRule_ruleAction,
    modifyTrafficMirrorFilterRule_sourceCidrBlock,
    modifyTrafficMirrorFilterRule_destinationCidrBlock,
    modifyTrafficMirrorFilterRule_protocol,
    modifyTrafficMirrorFilterRule_description,
    modifyTrafficMirrorFilterRule_ruleNumber,
    modifyTrafficMirrorFilterRule_destinationPortRange,
    modifyTrafficMirrorFilterRule_trafficMirrorFilterRuleId,
    modifyTrafficMirrorFilterRuleResponse_trafficMirrorFilterRule,
    modifyTrafficMirrorFilterRuleResponse_httpStatus,

    -- ** DescribeCoipPools
    describeCoipPools_nextToken,
    describeCoipPools_dryRun,
    describeCoipPools_maxResults,
    describeCoipPools_filters,
    describeCoipPools_poolIds,
    describeCoipPoolsResponse_nextToken,
    describeCoipPoolsResponse_coipPools,
    describeCoipPoolsResponse_httpStatus,

    -- ** CancelExportTask
    cancelExportTask_exportTaskId,

    -- ** EnableVolumeIO
    enableVolumeIO_dryRun,
    enableVolumeIO_volumeId,

    -- ** ModifyTransitGateway
    modifyTransitGateway_dryRun,
    modifyTransitGateway_options,
    modifyTransitGateway_description,
    modifyTransitGateway_transitGatewayId,
    modifyTransitGatewayResponse_transitGateway,
    modifyTransitGatewayResponse_httpStatus,

    -- ** DescribeInstanceTypeOfferings
    describeInstanceTypeOfferings_nextToken,
    describeInstanceTypeOfferings_dryRun,
    describeInstanceTypeOfferings_maxResults,
    describeInstanceTypeOfferings_locationType,
    describeInstanceTypeOfferings_filters,
    describeInstanceTypeOfferingsResponse_nextToken,
    describeInstanceTypeOfferingsResponse_instanceTypeOfferings,
    describeInstanceTypeOfferingsResponse_httpStatus,

    -- ** CreateSubnet
    createSubnet_tagSpecifications,
    createSubnet_dryRun,
    createSubnet_outpostArn,
    createSubnet_availabilityZoneId,
    createSubnet_availabilityZone,
    createSubnet_ipv6CidrBlock,
    createSubnet_cidrBlock,
    createSubnet_vpcId,
    createSubnetResponse_subnet,
    createSubnetResponse_httpStatus,

    -- ** RequestSpotFleet
    requestSpotFleet_dryRun,
    requestSpotFleet_spotFleetRequestConfig,
    requestSpotFleetResponse_spotFleetRequestId,
    requestSpotFleetResponse_httpStatus,

    -- ** DeleteVpnConnection
    deleteVpnConnection_dryRun,
    deleteVpnConnection_vpnConnectionId,

    -- ** ModifySpotFleetRequest
    modifySpotFleetRequest_launchTemplateConfigs,
    modifySpotFleetRequest_onDemandTargetCapacity,
    modifySpotFleetRequest_excessCapacityTerminationPolicy,
    modifySpotFleetRequest_targetCapacity,
    modifySpotFleetRequest_spotFleetRequestId,
    modifySpotFleetRequestResponse_return,
    modifySpotFleetRequestResponse_httpStatus,

    -- ** DeregisterImage
    deregisterImage_dryRun,
    deregisterImage_imageId,

    -- ** DetachVpnGateway
    detachVpnGateway_dryRun,
    detachVpnGateway_vpcId,
    detachVpnGateway_vpnGatewayId,

    -- ** CreateNetworkInterface
    createNetworkInterface_groups,
    createNetworkInterface_tagSpecifications,
    createNetworkInterface_privateIpAddresses,
    createNetworkInterface_ipv6Addresses,
    createNetworkInterface_dryRun,
    createNetworkInterface_interfaceType,
    createNetworkInterface_ipv6AddressCount,
    createNetworkInterface_description,
    createNetworkInterface_secondaryPrivateIpAddressCount,
    createNetworkInterface_privateIpAddress,
    createNetworkInterface_subnetId,
    createNetworkInterfaceResponse_networkInterface,
    createNetworkInterfaceResponse_httpStatus,

    -- ** ModifyNetworkInterfaceAttribute
    modifyNetworkInterfaceAttribute_groups,
    modifyNetworkInterfaceAttribute_attachment,
    modifyNetworkInterfaceAttribute_dryRun,
    modifyNetworkInterfaceAttribute_sourceDestCheck,
    modifyNetworkInterfaceAttribute_description,
    modifyNetworkInterfaceAttribute_networkInterfaceId,

    -- ** CreateNatGateway
    createNatGateway_tagSpecifications,
    createNatGateway_dryRun,
    createNatGateway_clientToken,
    createNatGateway_allocationId,
    createNatGateway_subnetId,
    createNatGatewayResponse_natGateway,
    createNatGatewayResponse_clientToken,
    createNatGatewayResponse_httpStatus,

    -- ** GetAssociatedEnclaveCertificateIamRoles
    getAssociatedEnclaveCertificateIamRoles_dryRun,
    getAssociatedEnclaveCertificateIamRoles_certificateArn,
    getAssociatedEnclaveCertificateIamRolesResponse_associatedRoles,
    getAssociatedEnclaveCertificateIamRolesResponse_httpStatus,

    -- ** CreateInternetGateway
    createInternetGateway_tagSpecifications,
    createInternetGateway_dryRun,
    createInternetGatewayResponse_internetGateway,
    createInternetGatewayResponse_httpStatus,

    -- ** EnableTransitGatewayRouteTablePropagation
    enableTransitGatewayRouteTablePropagation_dryRun,
    enableTransitGatewayRouteTablePropagation_transitGatewayRouteTableId,
    enableTransitGatewayRouteTablePropagation_transitGatewayAttachmentId,
    enableTransitGatewayRouteTablePropagationResponse_propagation,
    enableTransitGatewayRouteTablePropagationResponse_httpStatus,

    -- ** ResetAddressAttribute
    resetAddressAttribute_dryRun,
    resetAddressAttribute_allocationId,
    resetAddressAttribute_attribute,
    resetAddressAttributeResponse_address,
    resetAddressAttributeResponse_httpStatus,

    -- ** DescribeTransitGatewayConnects
    describeTransitGatewayConnects_nextToken,
    describeTransitGatewayConnects_dryRun,
    describeTransitGatewayConnects_maxResults,
    describeTransitGatewayConnects_transitGatewayAttachmentIds,
    describeTransitGatewayConnects_filters,
    describeTransitGatewayConnectsResponse_nextToken,
    describeTransitGatewayConnectsResponse_transitGatewayConnects,
    describeTransitGatewayConnectsResponse_httpStatus,

    -- ** DeletePlacementGroup
    deletePlacementGroup_dryRun,
    deletePlacementGroup_groupName,

    -- ** DescribeInstanceTypes
    describeInstanceTypes_nextToken,
    describeInstanceTypes_dryRun,
    describeInstanceTypes_maxResults,
    describeInstanceTypes_filters,
    describeInstanceTypes_instanceTypes,
    describeInstanceTypesResponse_nextToken,
    describeInstanceTypesResponse_instanceTypes,
    describeInstanceTypesResponse_httpStatus,

    -- ** DescribeBundleTasks
    describeBundleTasks_dryRun,
    describeBundleTasks_filters,
    describeBundleTasks_bundleIds,
    describeBundleTasksResponse_bundleTasks,
    describeBundleTasksResponse_httpStatus,

    -- ** ModifySubnetAttribute
    modifySubnetAttribute_customerOwnedIpv4Pool,
    modifySubnetAttribute_assignIpv6AddressOnCreation,
    modifySubnetAttribute_mapPublicIpOnLaunch,
    modifySubnetAttribute_mapCustomerOwnedIpOnLaunch,
    modifySubnetAttribute_subnetId,

    -- ** DescribeSecurityGroups
    describeSecurityGroups_nextToken,
    describeSecurityGroups_groupIds,
    describeSecurityGroups_dryRun,
    describeSecurityGroups_maxResults,
    describeSecurityGroups_groupNames,
    describeSecurityGroups_filters,
    describeSecurityGroupsResponse_nextToken,
    describeSecurityGroupsResponse_securityGroups,
    describeSecurityGroupsResponse_httpStatus,

    -- ** CreateClientVpnRoute
    createClientVpnRoute_dryRun,
    createClientVpnRoute_description,
    createClientVpnRoute_clientToken,
    createClientVpnRoute_clientVpnEndpointId,
    createClientVpnRoute_destinationCidrBlock,
    createClientVpnRoute_targetVpcSubnetId,
    createClientVpnRouteResponse_status,
    createClientVpnRouteResponse_httpStatus,

    -- ** DisassociateSubnetCidrBlock
    disassociateSubnetCidrBlock_associationId,
    disassociateSubnetCidrBlockResponse_ipv6CidrBlockAssociation,
    disassociateSubnetCidrBlockResponse_subnetId,
    disassociateSubnetCidrBlockResponse_httpStatus,

    -- ** DescribeSpotDatafeedSubscription
    describeSpotDatafeedSubscription_dryRun,
    describeSpotDatafeedSubscriptionResponse_spotDatafeedSubscription,
    describeSpotDatafeedSubscriptionResponse_httpStatus,

    -- ** CreateInstanceExportTask
    createInstanceExportTask_tagSpecifications,
    createInstanceExportTask_description,
    createInstanceExportTask_exportToS3Task,
    createInstanceExportTask_instanceId,
    createInstanceExportTask_targetEnvironment,
    createInstanceExportTaskResponse_exportTask,
    createInstanceExportTaskResponse_httpStatus,

    -- ** DisassociateClientVpnTargetNetwork
    disassociateClientVpnTargetNetwork_dryRun,
    disassociateClientVpnTargetNetwork_clientVpnEndpointId,
    disassociateClientVpnTargetNetwork_associationId,
    disassociateClientVpnTargetNetworkResponse_status,
    disassociateClientVpnTargetNetworkResponse_associationId,
    disassociateClientVpnTargetNetworkResponse_httpStatus,

    -- ** SendDiagnosticInterrupt
    sendDiagnosticInterrupt_dryRun,
    sendDiagnosticInterrupt_instanceId,

    -- ** DescribeVpcAttribute
    describeVpcAttribute_dryRun,
    describeVpcAttribute_attribute,
    describeVpcAttribute_vpcId,
    describeVpcAttributeResponse_enableDnsSupport,
    describeVpcAttributeResponse_enableDnsHostnames,
    describeVpcAttributeResponse_vpcId,
    describeVpcAttributeResponse_httpStatus,

    -- ** DescribeSecurityGroupReferences
    describeSecurityGroupReferences_dryRun,
    describeSecurityGroupReferences_groupId,
    describeSecurityGroupReferencesResponse_securityGroupReferenceSet,
    describeSecurityGroupReferencesResponse_httpStatus,

    -- ** ModifyCapacityReservation
    modifyCapacityReservation_dryRun,
    modifyCapacityReservation_endDateType,
    modifyCapacityReservation_accept,
    modifyCapacityReservation_endDate,
    modifyCapacityReservation_instanceCount,
    modifyCapacityReservation_capacityReservationId,
    modifyCapacityReservationResponse_return,
    modifyCapacityReservationResponse_httpStatus,

    -- ** DetachInternetGateway
    detachInternetGateway_dryRun,
    detachInternetGateway_internetGatewayId,
    detachInternetGateway_vpcId,

    -- ** CreateVolume
    createVolume_multiAttachEnabled,
    createVolume_tagSpecifications,
    createVolume_dryRun,
    createVolume_encrypted,
    createVolume_outpostArn,
    createVolume_throughput,
    createVolume_kmsKeyId,
    createVolume_snapshotId,
    createVolume_volumeType,
    createVolume_iops,
    createVolume_size,
    createVolume_availabilityZone,
    volume_multiAttachEnabled,
    volume_fastRestored,
    volume_outpostArn,
    volume_throughput,
    volume_kmsKeyId,
    volume_tags,
    volume_iops,
    volume_attachments,
    volume_availabilityZone,
    volume_createTime,
    volume_encrypted,
    volume_size,
    volume_snapshotId,
    volume_state,
    volume_volumeId,
    volume_volumeType,

    -- ** ExportClientVpnClientConfiguration
    exportClientVpnClientConfiguration_dryRun,
    exportClientVpnClientConfiguration_clientVpnEndpointId,
    exportClientVpnClientConfigurationResponse_clientConfiguration,
    exportClientVpnClientConfigurationResponse_httpStatus,

    -- ** RevokeSecurityGroupEgress
    revokeSecurityGroupEgress_fromPort,
    revokeSecurityGroupEgress_dryRun,
    revokeSecurityGroupEgress_sourceSecurityGroupName,
    revokeSecurityGroupEgress_cidrIp,
    revokeSecurityGroupEgress_ipProtocol,
    revokeSecurityGroupEgress_ipPermissions,
    revokeSecurityGroupEgress_sourceSecurityGroupOwnerId,
    revokeSecurityGroupEgress_toPort,
    revokeSecurityGroupEgress_groupId,
    revokeSecurityGroupEgressResponse_unknownIpPermissions,
    revokeSecurityGroupEgressResponse_return,
    revokeSecurityGroupEgressResponse_httpStatus,

    -- ** DeleteKeyPair
    deleteKeyPair_dryRun,
    deleteKeyPair_keyPairId,
    deleteKeyPair_keyName,

    -- ** ModifyInstanceMetadataOptions
    modifyInstanceMetadataOptions_dryRun,
    modifyInstanceMetadataOptions_httpEndpoint,
    modifyInstanceMetadataOptions_httpPutResponseHopLimit,
    modifyInstanceMetadataOptions_httpTokens,
    modifyInstanceMetadataOptions_instanceId,
    modifyInstanceMetadataOptionsResponse_instanceMetadataOptions,
    modifyInstanceMetadataOptionsResponse_instanceId,
    modifyInstanceMetadataOptionsResponse_httpStatus,

    -- ** DescribeEgressOnlyInternetGateways
    describeEgressOnlyInternetGateways_nextToken,
    describeEgressOnlyInternetGateways_egressOnlyInternetGatewayIds,
    describeEgressOnlyInternetGateways_dryRun,
    describeEgressOnlyInternetGateways_maxResults,
    describeEgressOnlyInternetGateways_filters,
    describeEgressOnlyInternetGatewaysResponse_nextToken,
    describeEgressOnlyInternetGatewaysResponse_egressOnlyInternetGateways,
    describeEgressOnlyInternetGatewaysResponse_httpStatus,

    -- ** ModifyTrafficMirrorFilterNetworkServices
    modifyTrafficMirrorFilterNetworkServices_addNetworkServices,
    modifyTrafficMirrorFilterNetworkServices_dryRun,
    modifyTrafficMirrorFilterNetworkServices_removeNetworkServices,
    modifyTrafficMirrorFilterNetworkServices_trafficMirrorFilterId,
    modifyTrafficMirrorFilterNetworkServicesResponse_trafficMirrorFilter,
    modifyTrafficMirrorFilterNetworkServicesResponse_httpStatus,

    -- ** ImportSnapshot
    importSnapshot_tagSpecifications,
    importSnapshot_dryRun,
    importSnapshot_encrypted,
    importSnapshot_roleName,
    importSnapshot_kmsKeyId,
    importSnapshot_clientData,
    importSnapshot_description,
    importSnapshot_clientToken,
    importSnapshot_diskContainer,
    importSnapshotResponse_snapshotTaskDetail,
    importSnapshotResponse_importTaskId,
    importSnapshotResponse_tags,
    importSnapshotResponse_description,
    importSnapshotResponse_httpStatus,

    -- ** DescribeImages
    describeImages_imageIds,
    describeImages_dryRun,
    describeImages_owners,
    describeImages_filters,
    describeImages_executableUsers,
    describeImagesResponse_images,
    describeImagesResponse_httpStatus,

    -- ** DeprovisionByoipCidr
    deprovisionByoipCidr_dryRun,
    deprovisionByoipCidr_cidr,
    deprovisionByoipCidrResponse_byoipCidr,
    deprovisionByoipCidrResponse_httpStatus,

    -- ** DescribeAddressesAttribute
    describeAddressesAttribute_nextToken,
    describeAddressesAttribute_dryRun,
    describeAddressesAttribute_maxResults,
    describeAddressesAttribute_attribute,
    describeAddressesAttribute_allocationIds,
    describeAddressesAttributeResponse_nextToken,
    describeAddressesAttributeResponse_addresses,
    describeAddressesAttributeResponse_httpStatus,

    -- ** AcceptVpcPeeringConnection
    acceptVpcPeeringConnection_vpcPeeringConnectionId,
    acceptVpcPeeringConnection_dryRun,
    acceptVpcPeeringConnectionResponse_vpcPeeringConnection,
    acceptVpcPeeringConnectionResponse_httpStatus,

    -- ** DescribeMovingAddresses
    describeMovingAddresses_nextToken,
    describeMovingAddresses_dryRun,
    describeMovingAddresses_maxResults,
    describeMovingAddresses_publicIps,
    describeMovingAddresses_filters,
    describeMovingAddressesResponse_nextToken,
    describeMovingAddressesResponse_movingAddressStatuses,
    describeMovingAddressesResponse_httpStatus,

    -- ** CreateVpcEndpointConnectionNotification
    createVpcEndpointConnectionNotification_dryRun,
    createVpcEndpointConnectionNotification_vpcEndpointId,
    createVpcEndpointConnectionNotification_serviceId,
    createVpcEndpointConnectionNotification_clientToken,
    createVpcEndpointConnectionNotification_connectionNotificationArn,
    createVpcEndpointConnectionNotification_connectionEvents,
    createVpcEndpointConnectionNotificationResponse_connectionNotification,
    createVpcEndpointConnectionNotificationResponse_clientToken,
    createVpcEndpointConnectionNotificationResponse_httpStatus,

    -- ** DescribeFleetHistory
    describeFleetHistory_nextToken,
    describeFleetHistory_eventType,
    describeFleetHistory_dryRun,
    describeFleetHistory_maxResults,
    describeFleetHistory_fleetId,
    describeFleetHistory_startTime,
    describeFleetHistoryResponse_nextToken,
    describeFleetHistoryResponse_fleetId,
    describeFleetHistoryResponse_startTime,
    describeFleetHistoryResponse_historyRecords,
    describeFleetHistoryResponse_lastEvaluatedTime,
    describeFleetHistoryResponse_httpStatus,

    -- ** DeleteVpcEndpointServiceConfigurations
    deleteVpcEndpointServiceConfigurations_dryRun,
    deleteVpcEndpointServiceConfigurations_serviceIds,
    deleteVpcEndpointServiceConfigurationsResponse_unsuccessful,
    deleteVpcEndpointServiceConfigurationsResponse_httpStatus,

    -- ** CreateVpc
    createVpc_tagSpecifications,
    createVpc_dryRun,
    createVpc_instanceTenancy,
    createVpc_ipv6Pool,
    createVpc_ipv6CidrBlock,
    createVpc_ipv6CidrBlockNetworkBorderGroup,
    createVpc_amazonProvidedIpv6CidrBlock,
    createVpc_cidrBlock,
    createVpcResponse_vpc,
    createVpcResponse_httpStatus,

    -- ** SearchLocalGatewayRoutes
    searchLocalGatewayRoutes_nextToken,
    searchLocalGatewayRoutes_dryRun,
    searchLocalGatewayRoutes_maxResults,
    searchLocalGatewayRoutes_localGatewayRouteTableId,
    searchLocalGatewayRoutes_filters,
    searchLocalGatewayRoutesResponse_nextToken,
    searchLocalGatewayRoutesResponse_routes,
    searchLocalGatewayRoutesResponse_httpStatus,

    -- ** CreateTrafficMirrorTarget
    createTrafficMirrorTarget_tagSpecifications,
    createTrafficMirrorTarget_networkLoadBalancerArn,
    createTrafficMirrorTarget_dryRun,
    createTrafficMirrorTarget_networkInterfaceId,
    createTrafficMirrorTarget_description,
    createTrafficMirrorTarget_clientToken,
    createTrafficMirrorTargetResponse_trafficMirrorTarget,
    createTrafficMirrorTargetResponse_clientToken,
    createTrafficMirrorTargetResponse_httpStatus,

    -- ** DescribeVolumeStatus
    describeVolumeStatus_nextToken,
    describeVolumeStatus_dryRun,
    describeVolumeStatus_volumeIds,
    describeVolumeStatus_maxResults,
    describeVolumeStatus_filters,
    describeVolumeStatusResponse_nextToken,
    describeVolumeStatusResponse_volumeStatuses,
    describeVolumeStatusResponse_httpStatus,

    -- ** DescribeVolumeAttribute
    describeVolumeAttribute_dryRun,
    describeVolumeAttribute_attribute,
    describeVolumeAttribute_volumeId,
    describeVolumeAttributeResponse_productCodes,
    describeVolumeAttributeResponse_volumeId,
    describeVolumeAttributeResponse_autoEnableIO,
    describeVolumeAttributeResponse_httpStatus,

    -- ** DeleteClientVpnRoute
    deleteClientVpnRoute_dryRun,
    deleteClientVpnRoute_targetVpcSubnetId,
    deleteClientVpnRoute_clientVpnEndpointId,
    deleteClientVpnRoute_destinationCidrBlock,
    deleteClientVpnRouteResponse_status,
    deleteClientVpnRouteResponse_httpStatus,

    -- ** ModifyVpcPeeringConnectionOptions
    modifyVpcPeeringConnectionOptions_dryRun,
    modifyVpcPeeringConnectionOptions_accepterPeeringConnectionOptions,
    modifyVpcPeeringConnectionOptions_requesterPeeringConnectionOptions,
    modifyVpcPeeringConnectionOptions_vpcPeeringConnectionId,
    modifyVpcPeeringConnectionOptionsResponse_accepterPeeringConnectionOptions,
    modifyVpcPeeringConnectionOptionsResponse_requesterPeeringConnectionOptions,
    modifyVpcPeeringConnectionOptionsResponse_httpStatus,

    -- ** DescribeSpotFleetInstances
    describeSpotFleetInstances_nextToken,
    describeSpotFleetInstances_dryRun,
    describeSpotFleetInstances_maxResults,
    describeSpotFleetInstances_spotFleetRequestId,
    describeSpotFleetInstancesResponse_nextToken,
    describeSpotFleetInstancesResponse_activeInstances,
    describeSpotFleetInstancesResponse_spotFleetRequestId,
    describeSpotFleetInstancesResponse_httpStatus,

    -- ** DescribePrincipalIdFormat
    describePrincipalIdFormat_nextToken,
    describePrincipalIdFormat_dryRun,
    describePrincipalIdFormat_maxResults,
    describePrincipalIdFormat_resources,
    describePrincipalIdFormatResponse_nextToken,
    describePrincipalIdFormatResponse_principals,
    describePrincipalIdFormatResponse_httpStatus,

    -- ** ModifyInstanceCreditSpecification
    modifyInstanceCreditSpecification_dryRun,
    modifyInstanceCreditSpecification_clientToken,
    modifyInstanceCreditSpecification_instanceCreditSpecifications,
    modifyInstanceCreditSpecificationResponse_successfulInstanceCreditSpecifications,
    modifyInstanceCreditSpecificationResponse_unsuccessfulInstanceCreditSpecifications,
    modifyInstanceCreditSpecificationResponse_httpStatus,

    -- ** DisassociateTransitGatewayMulticastDomain
    disassociateTransitGatewayMulticastDomain_dryRun,
    disassociateTransitGatewayMulticastDomain_transitGatewayMulticastDomainId,
    disassociateTransitGatewayMulticastDomain_subnetIds,
    disassociateTransitGatewayMulticastDomain_transitGatewayAttachmentId,
    disassociateTransitGatewayMulticastDomainResponse_associations,
    disassociateTransitGatewayMulticastDomainResponse_httpStatus,

    -- ** DescribeManagedPrefixLists
    describeManagedPrefixLists_nextToken,
    describeManagedPrefixLists_prefixListIds,
    describeManagedPrefixLists_dryRun,
    describeManagedPrefixLists_maxResults,
    describeManagedPrefixLists_filters,
    describeManagedPrefixListsResponse_nextToken,
    describeManagedPrefixListsResponse_prefixLists,
    describeManagedPrefixListsResponse_httpStatus,

    -- ** GetPasswordData
    getPasswordData_dryRun,
    getPasswordData_instanceId,
    getPasswordDataResponse_httpStatus,
    getPasswordDataResponse_instanceId,
    getPasswordDataResponse_passwordData,
    getPasswordDataResponse_timestamp,

    -- ** DeleteVolume
    deleteVolume_dryRun,
    deleteVolume_volumeId,

    -- ** DescribeTransitGateways
    describeTransitGateways_nextToken,
    describeTransitGateways_dryRun,
    describeTransitGateways_maxResults,
    describeTransitGateways_transitGatewayIds,
    describeTransitGateways_filters,
    describeTransitGatewaysResponse_nextToken,
    describeTransitGatewaysResponse_transitGateways,
    describeTransitGatewaysResponse_httpStatus,

    -- ** DescribeSpotFleetRequests
    describeSpotFleetRequests_nextToken,
    describeSpotFleetRequests_dryRun,
    describeSpotFleetRequests_maxResults,
    describeSpotFleetRequests_spotFleetRequestIds,
    describeSpotFleetRequestsResponse_nextToken,
    describeSpotFleetRequestsResponse_spotFleetRequestConfigs,
    describeSpotFleetRequestsResponse_httpStatus,

    -- ** DescribeClientVpnConnections
    describeClientVpnConnections_nextToken,
    describeClientVpnConnections_dryRun,
    describeClientVpnConnections_maxResults,
    describeClientVpnConnections_filters,
    describeClientVpnConnections_clientVpnEndpointId,
    describeClientVpnConnectionsResponse_nextToken,
    describeClientVpnConnectionsResponse_connections,
    describeClientVpnConnectionsResponse_httpStatus,

    -- ** SearchTransitGatewayMulticastGroups
    searchTransitGatewayMulticastGroups_nextToken,
    searchTransitGatewayMulticastGroups_dryRun,
    searchTransitGatewayMulticastGroups_transitGatewayMulticastDomainId,
    searchTransitGatewayMulticastGroups_maxResults,
    searchTransitGatewayMulticastGroups_filters,
    searchTransitGatewayMulticastGroupsResponse_nextToken,
    searchTransitGatewayMulticastGroupsResponse_multicastGroups,
    searchTransitGatewayMulticastGroupsResponse_httpStatus,

    -- ** ModifyVpcAttribute
    modifyVpcAttribute_enableDnsSupport,
    modifyVpcAttribute_enableDnsHostnames,
    modifyVpcAttribute_vpcId,

    -- ** RevokeSecurityGroupIngress
    revokeSecurityGroupIngress_fromPort,
    revokeSecurityGroupIngress_dryRun,
    revokeSecurityGroupIngress_sourceSecurityGroupName,
    revokeSecurityGroupIngress_groupName,
    revokeSecurityGroupIngress_cidrIp,
    revokeSecurityGroupIngress_groupId,
    revokeSecurityGroupIngress_ipProtocol,
    revokeSecurityGroupIngress_ipPermissions,
    revokeSecurityGroupIngress_sourceSecurityGroupOwnerId,
    revokeSecurityGroupIngress_toPort,
    revokeSecurityGroupIngressResponse_unknownIpPermissions,
    revokeSecurityGroupIngressResponse_return,
    revokeSecurityGroupIngressResponse_httpStatus,

    -- ** DescribeHostReservationOfferings
    describeHostReservationOfferings_minDuration,
    describeHostReservationOfferings_nextToken,
    describeHostReservationOfferings_maxResults,
    describeHostReservationOfferings_offeringId,
    describeHostReservationOfferings_filter,
    describeHostReservationOfferings_maxDuration,
    describeHostReservationOfferingsResponse_nextToken,
    describeHostReservationOfferingsResponse_offeringSet,
    describeHostReservationOfferingsResponse_httpStatus,

    -- ** DescribeTransitGatewayRouteTables
    describeTransitGatewayRouteTables_nextToken,
    describeTransitGatewayRouteTables_dryRun,
    describeTransitGatewayRouteTables_maxResults,
    describeTransitGatewayRouteTables_transitGatewayRouteTableIds,
    describeTransitGatewayRouteTables_filters,
    describeTransitGatewayRouteTablesResponse_nextToken,
    describeTransitGatewayRouteTablesResponse_transitGatewayRouteTables,
    describeTransitGatewayRouteTablesResponse_httpStatus,

    -- ** DescribeNetworkAcls
    describeNetworkAcls_nextToken,
    describeNetworkAcls_dryRun,
    describeNetworkAcls_maxResults,
    describeNetworkAcls_networkAclIds,
    describeNetworkAcls_filters,
    describeNetworkAclsResponse_nextToken,
    describeNetworkAclsResponse_networkAcls,
    describeNetworkAclsResponse_httpStatus,

    -- ** RegisterTransitGatewayMulticastGroupMembers
    registerTransitGatewayMulticastGroupMembers_dryRun,
    registerTransitGatewayMulticastGroupMembers_transitGatewayMulticastDomainId,
    registerTransitGatewayMulticastGroupMembers_networkInterfaceIds,
    registerTransitGatewayMulticastGroupMembers_groupIpAddress,
    registerTransitGatewayMulticastGroupMembersResponse_registeredMulticastGroupMembers,
    registerTransitGatewayMulticastGroupMembersResponse_httpStatus,

    -- ** DescribeHosts
    describeHosts_nextToken,
    describeHosts_maxResults,
    describeHosts_hostIds,
    describeHosts_filter,
    describeHostsResponse_nextToken,
    describeHostsResponse_hosts,
    describeHostsResponse_httpStatus,

    -- ** DescribeVpnGateways
    describeVpnGateways_dryRun,
    describeVpnGateways_vpnGatewayIds,
    describeVpnGateways_filters,
    describeVpnGatewaysResponse_vpnGateways,
    describeVpnGatewaysResponse_httpStatus,

    -- ** DescribeHostReservations
    describeHostReservations_nextToken,
    describeHostReservations_maxResults,
    describeHostReservations_hostReservationIdSet,
    describeHostReservations_filter,
    describeHostReservationsResponse_nextToken,
    describeHostReservationsResponse_hostReservationSet,
    describeHostReservationsResponse_httpStatus,

    -- ** DeleteManagedPrefixList
    deleteManagedPrefixList_dryRun,
    deleteManagedPrefixList_prefixListId,
    deleteManagedPrefixListResponse_prefixList,
    deleteManagedPrefixListResponse_httpStatus,

    -- ** RejectVpcPeeringConnection
    rejectVpcPeeringConnection_dryRun,
    rejectVpcPeeringConnection_vpcPeeringConnectionId,
    rejectVpcPeeringConnectionResponse_return,
    rejectVpcPeeringConnectionResponse_httpStatus,

    -- ** ResetImageAttribute
    resetImageAttribute_dryRun,
    resetImageAttribute_attribute,
    resetImageAttribute_imageId,

    -- ** DescribeScheduledInstances
    describeScheduledInstances_nextToken,
    describeScheduledInstances_dryRun,
    describeScheduledInstances_scheduledInstanceIds,
    describeScheduledInstances_maxResults,
    describeScheduledInstances_slotStartTimeRange,
    describeScheduledInstances_filters,
    describeScheduledInstancesResponse_nextToken,
    describeScheduledInstancesResponse_scheduledInstanceSet,
    describeScheduledInstancesResponse_httpStatus,

    -- ** AssociateEnclaveCertificateIamRole
    associateEnclaveCertificateIamRole_roleArn,
    associateEnclaveCertificateIamRole_dryRun,
    associateEnclaveCertificateIamRole_certificateArn,
    associateEnclaveCertificateIamRoleResponse_certificateS3ObjectKey,
    associateEnclaveCertificateIamRoleResponse_encryptionKmsKeyId,
    associateEnclaveCertificateIamRoleResponse_certificateS3BucketName,
    associateEnclaveCertificateIamRoleResponse_httpStatus,

    -- ** ModifyTransitGatewayPrefixListReference
    modifyTransitGatewayPrefixListReference_dryRun,
    modifyTransitGatewayPrefixListReference_blackhole,
    modifyTransitGatewayPrefixListReference_transitGatewayAttachmentId,
    modifyTransitGatewayPrefixListReference_transitGatewayRouteTableId,
    modifyTransitGatewayPrefixListReference_prefixListId,
    modifyTransitGatewayPrefixListReferenceResponse_transitGatewayPrefixListReference,
    modifyTransitGatewayPrefixListReferenceResponse_httpStatus,

    -- ** DescribeFpgaImageAttribute
    describeFpgaImageAttribute_dryRun,
    describeFpgaImageAttribute_fpgaImageId,
    describeFpgaImageAttribute_attribute,
    describeFpgaImageAttributeResponse_fpgaImageAttribute,
    describeFpgaImageAttributeResponse_httpStatus,

    -- ** AdvertiseByoipCidr
    advertiseByoipCidr_dryRun,
    advertiseByoipCidr_cidr,
    advertiseByoipCidrResponse_byoipCidr,
    advertiseByoipCidrResponse_httpStatus,

    -- ** DeleteVpnConnectionRoute
    deleteVpnConnectionRoute_destinationCidrBlock,
    deleteVpnConnectionRoute_vpnConnectionId,

    -- ** DescribeVpcEndpointServicePermissions
    describeVpcEndpointServicePermissions_nextToken,
    describeVpcEndpointServicePermissions_dryRun,
    describeVpcEndpointServicePermissions_maxResults,
    describeVpcEndpointServicePermissions_filters,
    describeVpcEndpointServicePermissions_serviceId,
    describeVpcEndpointServicePermissionsResponse_nextToken,
    describeVpcEndpointServicePermissionsResponse_allowedPrincipals,
    describeVpcEndpointServicePermissionsResponse_httpStatus,

    -- ** DescribeVpcEndpointConnections
    describeVpcEndpointConnections_nextToken,
    describeVpcEndpointConnections_dryRun,
    describeVpcEndpointConnections_maxResults,
    describeVpcEndpointConnections_filters,
    describeVpcEndpointConnectionsResponse_nextToken,
    describeVpcEndpointConnectionsResponse_vpcEndpointConnections,
    describeVpcEndpointConnectionsResponse_httpStatus,

    -- ** DescribeNetworkInterfacePermissions
    describeNetworkInterfacePermissions_nextToken,
    describeNetworkInterfacePermissions_maxResults,
    describeNetworkInterfacePermissions_networkInterfacePermissionIds,
    describeNetworkInterfacePermissions_filters,
    describeNetworkInterfacePermissionsResponse_nextToken,
    describeNetworkInterfacePermissionsResponse_networkInterfacePermissions,
    describeNetworkInterfacePermissionsResponse_httpStatus,

    -- ** CreateTrafficMirrorSession
    createTrafficMirrorSession_tagSpecifications,
    createTrafficMirrorSession_dryRun,
    createTrafficMirrorSession_packetLength,
    createTrafficMirrorSession_description,
    createTrafficMirrorSession_virtualNetworkId,
    createTrafficMirrorSession_clientToken,
    createTrafficMirrorSession_networkInterfaceId,
    createTrafficMirrorSession_trafficMirrorTargetId,
    createTrafficMirrorSession_trafficMirrorFilterId,
    createTrafficMirrorSession_sessionNumber,
    createTrafficMirrorSessionResponse_trafficMirrorSession,
    createTrafficMirrorSessionResponse_clientToken,
    createTrafficMirrorSessionResponse_httpStatus,

    -- ** RegisterInstanceEventNotificationAttributes
    registerInstanceEventNotificationAttributes_dryRun,
    registerInstanceEventNotificationAttributes_instanceTagAttribute,
    registerInstanceEventNotificationAttributesResponse_instanceTagAttribute,
    registerInstanceEventNotificationAttributesResponse_httpStatus,

    -- ** RejectTransitGatewayMulticastDomainAssociations
    rejectTransitGatewayMulticastDomainAssociations_dryRun,
    rejectTransitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId,
    rejectTransitGatewayMulticastDomainAssociations_subnetIds,
    rejectTransitGatewayMulticastDomainAssociations_transitGatewayAttachmentId,
    rejectTransitGatewayMulticastDomainAssociationsResponse_associations,
    rejectTransitGatewayMulticastDomainAssociationsResponse_httpStatus,

    -- ** DeleteDhcpOptions
    deleteDhcpOptions_dryRun,
    deleteDhcpOptions_dhcpOptionsId,

    -- ** DeleteTransitGateway
    deleteTransitGateway_dryRun,
    deleteTransitGateway_transitGatewayId,
    deleteTransitGatewayResponse_transitGateway,
    deleteTransitGatewayResponse_httpStatus,

    -- ** EnableVpcClassicLinkDnsSupport
    enableVpcClassicLinkDnsSupport_vpcId,
    enableVpcClassicLinkDnsSupportResponse_return,
    enableVpcClassicLinkDnsSupportResponse_httpStatus,

    -- ** DescribeRegions
    describeRegions_dryRun,
    describeRegions_regionNames,
    describeRegions_filters,
    describeRegions_allRegions,
    describeRegionsResponse_regions,
    describeRegionsResponse_httpStatus,

    -- ** CreateEgressOnlyInternetGateway
    createEgressOnlyInternetGateway_tagSpecifications,
    createEgressOnlyInternetGateway_dryRun,
    createEgressOnlyInternetGateway_clientToken,
    createEgressOnlyInternetGateway_vpcId,
    createEgressOnlyInternetGatewayResponse_egressOnlyInternetGateway,
    createEgressOnlyInternetGatewayResponse_clientToken,
    createEgressOnlyInternetGatewayResponse_httpStatus,

    -- ** CreateTransitGateway
    createTransitGateway_tagSpecifications,
    createTransitGateway_dryRun,
    createTransitGateway_options,
    createTransitGateway_description,
    createTransitGatewayResponse_transitGateway,
    createTransitGatewayResponse_httpStatus,

    -- ** DeleteClientVpnEndpoint
    deleteClientVpnEndpoint_dryRun,
    deleteClientVpnEndpoint_clientVpnEndpointId,
    deleteClientVpnEndpointResponse_status,
    deleteClientVpnEndpointResponse_httpStatus,

    -- ** ExportClientVpnClientCertificateRevocationList
    exportClientVpnClientCertificateRevocationList_dryRun,
    exportClientVpnClientCertificateRevocationList_clientVpnEndpointId,
    exportClientVpnClientCertificateRevocationListResponse_certificateRevocationList,
    exportClientVpnClientCertificateRevocationListResponse_status,
    exportClientVpnClientCertificateRevocationListResponse_httpStatus,

    -- ** CreateLaunchTemplateVersion
    createLaunchTemplateVersion_sourceVersion,
    createLaunchTemplateVersion_dryRun,
    createLaunchTemplateVersion_launchTemplateId,
    createLaunchTemplateVersion_launchTemplateName,
    createLaunchTemplateVersion_versionDescription,
    createLaunchTemplateVersion_clientToken,
    createLaunchTemplateVersion_launchTemplateData,
    createLaunchTemplateVersionResponse_launchTemplateVersion,
    createLaunchTemplateVersionResponse_warning,
    createLaunchTemplateVersionResponse_httpStatus,

    -- ** CreateSnapshots
    createSnapshots_tagSpecifications,
    createSnapshots_dryRun,
    createSnapshots_outpostArn,
    createSnapshots_copyTagsFromSource,
    createSnapshots_description,
    createSnapshots_instanceSpecification,
    createSnapshotsResponse_snapshots,
    createSnapshotsResponse_httpStatus,

    -- ** ModifyDefaultCreditSpecification
    modifyDefaultCreditSpecification_dryRun,
    modifyDefaultCreditSpecification_instanceFamily,
    modifyDefaultCreditSpecification_cpuCredits,
    modifyDefaultCreditSpecificationResponse_instanceFamilyCreditSpecification,
    modifyDefaultCreditSpecificationResponse_httpStatus,

    -- ** ApplySecurityGroupsToClientVpnTargetNetwork
    applySecurityGroupsToClientVpnTargetNetwork_dryRun,
    applySecurityGroupsToClientVpnTargetNetwork_clientVpnEndpointId,
    applySecurityGroupsToClientVpnTargetNetwork_vpcId,
    applySecurityGroupsToClientVpnTargetNetwork_securityGroupIds,
    applySecurityGroupsToClientVpnTargetNetworkResponse_securityGroupIds,
    applySecurityGroupsToClientVpnTargetNetworkResponse_httpStatus,

    -- ** AttachVpnGateway
    attachVpnGateway_dryRun,
    attachVpnGateway_vpcId,
    attachVpnGateway_vpnGatewayId,
    attachVpnGatewayResponse_vpcAttachment,
    attachVpnGatewayResponse_httpStatus,

    -- ** CreateVpnConnectionRoute
    createVpnConnectionRoute_destinationCidrBlock,
    createVpnConnectionRoute_vpnConnectionId,

    -- ** DescribeKeyPairs
    describeKeyPairs_dryRun,
    describeKeyPairs_keyPairIds,
    describeKeyPairs_filters,
    describeKeyPairs_keyNames,
    describeKeyPairsResponse_keyPairs,
    describeKeyPairsResponse_httpStatus,

    -- ** AllocateAddress
    allocateAddress_tagSpecifications,
    allocateAddress_customerOwnedIpv4Pool,
    allocateAddress_dryRun,
    allocateAddress_address,
    allocateAddress_domain,
    allocateAddress_publicIpv4Pool,
    allocateAddress_networkBorderGroup,
    allocateAddressResponse_customerOwnedIpv4Pool,
    allocateAddressResponse_domain,
    allocateAddressResponse_carrierIp,
    allocateAddressResponse_customerOwnedIp,
    allocateAddressResponse_publicIpv4Pool,
    allocateAddressResponse_publicIp,
    allocateAddressResponse_allocationId,
    allocateAddressResponse_networkBorderGroup,
    allocateAddressResponse_httpStatus,

    -- ** DeleteTrafficMirrorSession
    deleteTrafficMirrorSession_dryRun,
    deleteTrafficMirrorSession_trafficMirrorSessionId,
    deleteTrafficMirrorSessionResponse_trafficMirrorSessionId,
    deleteTrafficMirrorSessionResponse_httpStatus,

    -- ** GetManagedPrefixListEntries
    getManagedPrefixListEntries_nextToken,
    getManagedPrefixListEntries_dryRun,
    getManagedPrefixListEntries_maxResults,
    getManagedPrefixListEntries_targetVersion,
    getManagedPrefixListEntries_prefixListId,
    getManagedPrefixListEntriesResponse_nextToken,
    getManagedPrefixListEntriesResponse_entries,
    getManagedPrefixListEntriesResponse_httpStatus,

    -- ** CreateFpgaImage
    createFpgaImage_tagSpecifications,
    createFpgaImage_dryRun,
    createFpgaImage_name,
    createFpgaImage_description,
    createFpgaImage_logsStorageLocation,
    createFpgaImage_clientToken,
    createFpgaImage_inputStorageLocation,
    createFpgaImageResponse_fpgaImageGlobalId,
    createFpgaImageResponse_fpgaImageId,
    createFpgaImageResponse_httpStatus,

    -- ** ExportImage
    exportImage_tagSpecifications,
    exportImage_dryRun,
    exportImage_roleName,
    exportImage_description,
    exportImage_clientToken,
    exportImage_diskImageFormat,
    exportImage_imageId,
    exportImage_s3ExportLocation,
    exportImageResponse_statusMessage,
    exportImageResponse_status,
    exportImageResponse_diskImageFormat,
    exportImageResponse_roleName,
    exportImageResponse_imageId,
    exportImageResponse_tags,
    exportImageResponse_s3ExportLocation,
    exportImageResponse_description,
    exportImageResponse_exportImageTaskId,
    exportImageResponse_progress,
    exportImageResponse_httpStatus,

    -- ** RejectTransitGatewayPeeringAttachment
    rejectTransitGatewayPeeringAttachment_dryRun,
    rejectTransitGatewayPeeringAttachment_transitGatewayAttachmentId,
    rejectTransitGatewayPeeringAttachmentResponse_transitGatewayPeeringAttachment,
    rejectTransitGatewayPeeringAttachmentResponse_httpStatus,

    -- ** DescribeConversionTasks
    describeConversionTasks_dryRun,
    describeConversionTasks_conversionTaskIds,
    describeConversionTasksResponse_conversionTasks,
    describeConversionTasksResponse_httpStatus,

    -- ** WithdrawByoipCidr
    withdrawByoipCidr_dryRun,
    withdrawByoipCidr_cidr,
    withdrawByoipCidrResponse_byoipCidr,
    withdrawByoipCidrResponse_httpStatus,

    -- ** DeleteTrafficMirrorFilterRule
    deleteTrafficMirrorFilterRule_dryRun,
    deleteTrafficMirrorFilterRule_trafficMirrorFilterRuleId,
    deleteTrafficMirrorFilterRuleResponse_trafficMirrorFilterRuleId,
    deleteTrafficMirrorFilterRuleResponse_httpStatus,

    -- ** DescribeClassicLinkInstances
    describeClassicLinkInstances_instanceIds,
    describeClassicLinkInstances_nextToken,
    describeClassicLinkInstances_dryRun,
    describeClassicLinkInstances_maxResults,
    describeClassicLinkInstances_filters,
    describeClassicLinkInstancesResponse_nextToken,
    describeClassicLinkInstancesResponse_instances,
    describeClassicLinkInstancesResponse_httpStatus,

    -- ** TerminateInstances
    terminateInstances_dryRun,
    terminateInstances_instanceIds,
    terminateInstancesResponse_terminatingInstances,
    terminateInstancesResponse_httpStatus,

    -- ** AcceptTransitGatewayVpcAttachment
    acceptTransitGatewayVpcAttachment_dryRun,
    acceptTransitGatewayVpcAttachment_transitGatewayAttachmentId,
    acceptTransitGatewayVpcAttachmentResponse_transitGatewayVpcAttachment,
    acceptTransitGatewayVpcAttachmentResponse_httpStatus,

    -- ** DisableVpcClassicLinkDnsSupport
    disableVpcClassicLinkDnsSupport_vpcId,
    disableVpcClassicLinkDnsSupportResponse_return,
    disableVpcClassicLinkDnsSupportResponse_httpStatus,

    -- ** GetLaunchTemplateData
    getLaunchTemplateData_dryRun,
    getLaunchTemplateData_instanceId,
    getLaunchTemplateDataResponse_launchTemplateData,
    getLaunchTemplateDataResponse_httpStatus,

    -- ** DescribeReservedInstances
    describeReservedInstances_dryRun,
    describeReservedInstances_offeringClass,
    describeReservedInstances_filters,
    describeReservedInstances_offeringType,
    describeReservedInstances_reservedInstancesIds,
    describeReservedInstancesResponse_reservedInstances,
    describeReservedInstancesResponse_httpStatus,

    -- ** ModifyFpgaImageAttribute
    modifyFpgaImageAttribute_dryRun,
    modifyFpgaImageAttribute_productCodes,
    modifyFpgaImageAttribute_userIds,
    modifyFpgaImageAttribute_name,
    modifyFpgaImageAttribute_attribute,
    modifyFpgaImageAttribute_description,
    modifyFpgaImageAttribute_userGroups,
    modifyFpgaImageAttribute_loadPermission,
    modifyFpgaImageAttribute_operationType,
    modifyFpgaImageAttribute_fpgaImageId,
    modifyFpgaImageAttributeResponse_fpgaImageAttribute,
    modifyFpgaImageAttributeResponse_httpStatus,

    -- ** EnableVpcClassicLink
    enableVpcClassicLink_dryRun,
    enableVpcClassicLink_vpcId,
    enableVpcClassicLinkResponse_return,
    enableVpcClassicLinkResponse_httpStatus,

    -- ** AttachInternetGateway
    attachInternetGateway_dryRun,
    attachInternetGateway_internetGatewayId,
    attachInternetGateway_vpcId,

    -- ** DescribePublicIpv4Pools
    describePublicIpv4Pools_nextToken,
    describePublicIpv4Pools_maxResults,
    describePublicIpv4Pools_filters,
    describePublicIpv4Pools_poolIds,
    describePublicIpv4PoolsResponse_nextToken,
    describePublicIpv4PoolsResponse_publicIpv4Pools,
    describePublicIpv4PoolsResponse_httpStatus,

    -- ** CreateCustomerGateway
    createCustomerGateway_tagSpecifications,
    createCustomerGateway_dryRun,
    createCustomerGateway_certificateArn,
    createCustomerGateway_deviceName,
    createCustomerGateway_publicIp,
    createCustomerGateway_bgpAsn,
    createCustomerGateway_type,
    createCustomerGatewayResponse_customerGateway,
    createCustomerGatewayResponse_httpStatus,

    -- ** DescribeIamInstanceProfileAssociations
    describeIamInstanceProfileAssociations_nextToken,
    describeIamInstanceProfileAssociations_maxResults,
    describeIamInstanceProfileAssociations_associationIds,
    describeIamInstanceProfileAssociations_filters,
    describeIamInstanceProfileAssociationsResponse_nextToken,
    describeIamInstanceProfileAssociationsResponse_iamInstanceProfileAssociations,
    describeIamInstanceProfileAssociationsResponse_httpStatus,

    -- ** DescribeExportImageTasks
    describeExportImageTasks_nextToken,
    describeExportImageTasks_dryRun,
    describeExportImageTasks_maxResults,
    describeExportImageTasks_exportImageTaskIds,
    describeExportImageTasks_filters,
    describeExportImageTasksResponse_nextToken,
    describeExportImageTasksResponse_exportImageTasks,
    describeExportImageTasksResponse_httpStatus,

    -- ** ProvisionByoipCidr
    provisionByoipCidr_dryRun,
    provisionByoipCidr_cidrAuthorizationContext,
    provisionByoipCidr_publiclyAdvertisable,
    provisionByoipCidr_description,
    provisionByoipCidr_poolTagSpecifications,
    provisionByoipCidr_cidr,
    provisionByoipCidrResponse_byoipCidr,
    provisionByoipCidrResponse_httpStatus,

    -- ** CreateReservedInstancesListing
    createReservedInstancesListing_clientToken,
    createReservedInstancesListing_instanceCount,
    createReservedInstancesListing_priceSchedules,
    createReservedInstancesListing_reservedInstancesId,
    createReservedInstancesListingResponse_reservedInstancesListings,
    createReservedInstancesListingResponse_httpStatus,

    -- ** DescribeClientVpnTargetNetworks
    describeClientVpnTargetNetworks_nextToken,
    describeClientVpnTargetNetworks_dryRun,
    describeClientVpnTargetNetworks_maxResults,
    describeClientVpnTargetNetworks_associationIds,
    describeClientVpnTargetNetworks_filters,
    describeClientVpnTargetNetworks_clientVpnEndpointId,
    describeClientVpnTargetNetworksResponse_nextToken,
    describeClientVpnTargetNetworksResponse_clientVpnTargetNetworks,
    describeClientVpnTargetNetworksResponse_httpStatus,

    -- ** ModifyVpnTunnelOptions
    modifyVpnTunnelOptions_dryRun,
    modifyVpnTunnelOptions_vpnConnectionId,
    modifyVpnTunnelOptions_vpnTunnelOutsideIpAddress,
    modifyVpnTunnelOptions_tunnelOptions,
    modifyVpnTunnelOptionsResponse_vpnConnection,
    modifyVpnTunnelOptionsResponse_httpStatus,

    -- ** ModifyInstancePlacement
    modifyInstancePlacement_groupName,
    modifyInstancePlacement_tenancy,
    modifyInstancePlacement_affinity,
    modifyInstancePlacement_partitionNumber,
    modifyInstancePlacement_hostResourceGroupArn,
    modifyInstancePlacement_hostId,
    modifyInstancePlacement_instanceId,
    modifyInstancePlacementResponse_return,
    modifyInstancePlacementResponse_httpStatus,

    -- ** ImportKeyPair
    importKeyPair_tagSpecifications,
    importKeyPair_dryRun,
    importKeyPair_keyName,
    importKeyPair_publicKeyMaterial,
    importKeyPairResponse_keyFingerprint,
    importKeyPairResponse_keyPairId,
    importKeyPairResponse_tags,
    importKeyPairResponse_keyName,
    importKeyPairResponse_httpStatus,

    -- ** DescribeNetworkInsightsAnalyses
    describeNetworkInsightsAnalyses_analysisStartTime,
    describeNetworkInsightsAnalyses_networkInsightsAnalysisIds,
    describeNetworkInsightsAnalyses_nextToken,
    describeNetworkInsightsAnalyses_dryRun,
    describeNetworkInsightsAnalyses_maxResults,
    describeNetworkInsightsAnalyses_filters,
    describeNetworkInsightsAnalyses_networkInsightsPathId,
    describeNetworkInsightsAnalyses_analysisEndTime,
    describeNetworkInsightsAnalysesResponse_nextToken,
    describeNetworkInsightsAnalysesResponse_networkInsightsAnalyses,
    describeNetworkInsightsAnalysesResponse_httpStatus,

    -- ** DeleteSecurityGroup
    deleteSecurityGroup_dryRun,
    deleteSecurityGroup_groupName,
    deleteSecurityGroup_groupId,

    -- ** CreateCarrierGateway
    createCarrierGateway_tagSpecifications,
    createCarrierGateway_dryRun,
    createCarrierGateway_clientToken,
    createCarrierGateway_vpcId,
    createCarrierGatewayResponse_carrierGateway,
    createCarrierGatewayResponse_httpStatus,

    -- ** CreateSnapshot
    createSnapshot_tagSpecifications,
    createSnapshot_dryRun,
    createSnapshot_outpostArn,
    createSnapshot_description,
    createSnapshot_volumeId,
    snapshot_ownerAlias,
    snapshot_stateMessage,
    snapshot_outpostArn,
    snapshot_dataEncryptionKeyId,
    snapshot_kmsKeyId,
    snapshot_tags,
    snapshot_snapshotId,
    snapshot_ownerId,
    snapshot_volumeId,
    snapshot_volumeSize,
    snapshot_description,
    snapshot_startTime,
    snapshot_progress,
    snapshot_state,
    snapshot_encrypted,

    -- ** ModifyVolume
    modifyVolume_multiAttachEnabled,
    modifyVolume_dryRun,
    modifyVolume_throughput,
    modifyVolume_volumeType,
    modifyVolume_iops,
    modifyVolume_size,
    modifyVolume_volumeId,
    modifyVolumeResponse_volumeModification,
    modifyVolumeResponse_httpStatus,

    -- ** DeleteNetworkInsightsAnalysis
    deleteNetworkInsightsAnalysis_dryRun,
    deleteNetworkInsightsAnalysis_networkInsightsAnalysisId,
    deleteNetworkInsightsAnalysisResponse_networkInsightsAnalysisId,
    deleteNetworkInsightsAnalysisResponse_httpStatus,

    -- ** DescribeLocalGatewayRouteTableVpcAssociations
    describeLocalGatewayRouteTableVpcAssociations_nextToken,
    describeLocalGatewayRouteTableVpcAssociations_dryRun,
    describeLocalGatewayRouteTableVpcAssociations_maxResults,
    describeLocalGatewayRouteTableVpcAssociations_filters,
    describeLocalGatewayRouteTableVpcAssociations_localGatewayRouteTableVpcAssociationIds,
    describeLocalGatewayRouteTableVpcAssociationsResponse_nextToken,
    describeLocalGatewayRouteTableVpcAssociationsResponse_localGatewayRouteTableVpcAssociations,
    describeLocalGatewayRouteTableVpcAssociationsResponse_httpStatus,

    -- ** CreateTrafficMirrorFilter
    createTrafficMirrorFilter_tagSpecifications,
    createTrafficMirrorFilter_dryRun,
    createTrafficMirrorFilter_description,
    createTrafficMirrorFilter_clientToken,
    createTrafficMirrorFilterResponse_trafficMirrorFilter,
    createTrafficMirrorFilterResponse_clientToken,
    createTrafficMirrorFilterResponse_httpStatus,

    -- ** DeleteSpotDatafeedSubscription
    deleteSpotDatafeedSubscription_dryRun,

    -- ** DescribeInstanceAttribute
    describeInstanceAttribute_dryRun,
    describeInstanceAttribute_attribute,
    describeInstanceAttribute_instanceId,
    describeInstanceAttributeResponse_groups,
    describeInstanceAttributeResponse_instanceId,
    describeInstanceAttributeResponse_instanceType,
    describeInstanceAttributeResponse_rootDeviceName,
    describeInstanceAttributeResponse_ebsOptimized,
    describeInstanceAttributeResponse_userData,
    describeInstanceAttributeResponse_ramdiskId,
    describeInstanceAttributeResponse_sourceDestCheck,
    describeInstanceAttributeResponse_productCodes,
    describeInstanceAttributeResponse_instanceInitiatedShutdownBehavior,
    describeInstanceAttributeResponse_sriovNetSupport,
    describeInstanceAttributeResponse_blockDeviceMappings,
    describeInstanceAttributeResponse_enclaveOptions,
    describeInstanceAttributeResponse_kernelId,
    describeInstanceAttributeResponse_disableApiTermination,
    describeInstanceAttributeResponse_enaSupport,
    describeInstanceAttributeResponse_httpStatus,

    -- ** CreateCapacityReservation
    createCapacityReservation_tagSpecifications,
    createCapacityReservation_ebsOptimized,
    createCapacityReservation_dryRun,
    createCapacityReservation_endDateType,
    createCapacityReservation_tenancy,
    createCapacityReservation_availabilityZoneId,
    createCapacityReservation_availabilityZone,
    createCapacityReservation_instanceMatchCriteria,
    createCapacityReservation_ephemeralStorage,
    createCapacityReservation_endDate,
    createCapacityReservation_clientToken,
    createCapacityReservation_instanceType,
    createCapacityReservation_instancePlatform,
    createCapacityReservation_instanceCount,
    createCapacityReservationResponse_capacityReservation,
    createCapacityReservationResponse_httpStatus,

    -- ** DeleteTransitGatewayConnect
    deleteTransitGatewayConnect_dryRun,
    deleteTransitGatewayConnect_transitGatewayAttachmentId,
    deleteTransitGatewayConnectResponse_transitGatewayConnect,
    deleteTransitGatewayConnectResponse_httpStatus,

    -- ** ModifyEbsDefaultKmsKeyId
    modifyEbsDefaultKmsKeyId_dryRun,
    modifyEbsDefaultKmsKeyId_kmsKeyId,
    modifyEbsDefaultKmsKeyIdResponse_kmsKeyId,
    modifyEbsDefaultKmsKeyIdResponse_httpStatus,

    -- ** DeleteRoute
    deleteRoute_dryRun,
    deleteRoute_destinationPrefixListId,
    deleteRoute_destinationIpv6CidrBlock,
    deleteRoute_destinationCidrBlock,
    deleteRoute_routeTableId,

    -- ** DescribeNetworkInsightsPaths
    describeNetworkInsightsPaths_nextToken,
    describeNetworkInsightsPaths_dryRun,
    describeNetworkInsightsPaths_maxResults,
    describeNetworkInsightsPaths_networkInsightsPathIds,
    describeNetworkInsightsPaths_filters,
    describeNetworkInsightsPathsResponse_nextToken,
    describeNetworkInsightsPathsResponse_networkInsightsPaths,
    describeNetworkInsightsPathsResponse_httpStatus,

    -- ** PurchaseScheduledInstances
    purchaseScheduledInstances_dryRun,
    purchaseScheduledInstances_clientToken,
    purchaseScheduledInstances_purchaseRequests,
    purchaseScheduledInstancesResponse_scheduledInstanceSet,
    purchaseScheduledInstancesResponse_httpStatus,

    -- ** CreateTransitGatewayPeeringAttachment
    createTransitGatewayPeeringAttachment_tagSpecifications,
    createTransitGatewayPeeringAttachment_dryRun,
    createTransitGatewayPeeringAttachment_transitGatewayId,
    createTransitGatewayPeeringAttachment_peerTransitGatewayId,
    createTransitGatewayPeeringAttachment_peerAccountId,
    createTransitGatewayPeeringAttachment_peerRegion,
    createTransitGatewayPeeringAttachmentResponse_transitGatewayPeeringAttachment,
    createTransitGatewayPeeringAttachmentResponse_httpStatus,

    -- ** GetDefaultCreditSpecification
    getDefaultCreditSpecification_dryRun,
    getDefaultCreditSpecification_instanceFamily,
    getDefaultCreditSpecificationResponse_instanceFamilyCreditSpecification,
    getDefaultCreditSpecificationResponse_httpStatus,

    -- ** DescribeInternetGateways
    describeInternetGateways_nextToken,
    describeInternetGateways_dryRun,
    describeInternetGateways_maxResults,
    describeInternetGateways_internetGatewayIds,
    describeInternetGateways_filters,
    describeInternetGatewaysResponse_nextToken,
    describeInternetGatewaysResponse_internetGateways,
    describeInternetGatewaysResponse_httpStatus,

    -- ** ModifyInstanceAttribute
    modifyInstanceAttribute_groups,
    modifyInstanceAttribute_instanceType,
    modifyInstanceAttribute_ebsOptimized,
    modifyInstanceAttribute_userData,
    modifyInstanceAttribute_dryRun,
    modifyInstanceAttribute_kernel,
    modifyInstanceAttribute_sourceDestCheck,
    modifyInstanceAttribute_instanceInitiatedShutdownBehavior,
    modifyInstanceAttribute_attribute,
    modifyInstanceAttribute_sriovNetSupport,
    modifyInstanceAttribute_blockDeviceMappings,
    modifyInstanceAttribute_value,
    modifyInstanceAttribute_ramdisk,
    modifyInstanceAttribute_disableApiTermination,
    modifyInstanceAttribute_enaSupport,
    modifyInstanceAttribute_instanceId,

    -- ** CreateSecurityGroup
    createSecurityGroup_tagSpecifications,
    createSecurityGroup_dryRun,
    createSecurityGroup_vpcId,
    createSecurityGroup_description,
    createSecurityGroup_groupName,
    createSecurityGroupResponse_tags,
    createSecurityGroupResponse_httpStatus,
    createSecurityGroupResponse_groupId,

    -- ** CreateTransitGatewayConnect
    createTransitGatewayConnect_tagSpecifications,
    createTransitGatewayConnect_dryRun,
    createTransitGatewayConnect_transportTransitGatewayAttachmentId,
    createTransitGatewayConnect_options,
    createTransitGatewayConnectResponse_transitGatewayConnect,
    createTransitGatewayConnectResponse_httpStatus,

    -- ** ReplaceNetworkAclAssociation
    replaceNetworkAclAssociation_dryRun,
    replaceNetworkAclAssociation_associationId,
    replaceNetworkAclAssociation_networkAclId,
    replaceNetworkAclAssociationResponse_newAssociationId,
    replaceNetworkAclAssociationResponse_httpStatus,

    -- ** CreateRoute
    createRoute_instanceId,
    createRoute_vpcPeeringConnectionId,
    createRoute_dryRun,
    createRoute_vpcEndpointId,
    createRoute_destinationPrefixListId,
    createRoute_destinationIpv6CidrBlock,
    createRoute_localGatewayId,
    createRoute_egressOnlyInternetGatewayId,
    createRoute_carrierGatewayId,
    createRoute_destinationCidrBlock,
    createRoute_networkInterfaceId,
    createRoute_natGatewayId,
    createRoute_gatewayId,
    createRoute_transitGatewayId,
    createRoute_routeTableId,
    createRouteResponse_return,
    createRouteResponse_httpStatus,

    -- ** DeleteLaunchTemplateVersions
    deleteLaunchTemplateVersions_dryRun,
    deleteLaunchTemplateVersions_launchTemplateId,
    deleteLaunchTemplateVersions_launchTemplateName,
    deleteLaunchTemplateVersions_versions,
    deleteLaunchTemplateVersionsResponse_unsuccessfullyDeletedLaunchTemplateVersions,
    deleteLaunchTemplateVersionsResponse_successfullyDeletedLaunchTemplateVersions,
    deleteLaunchTemplateVersionsResponse_httpStatus,

    -- ** DescribeIdentityIdFormat
    describeIdentityIdFormat_resource,
    describeIdentityIdFormat_principalArn,
    describeIdentityIdFormatResponse_statuses,
    describeIdentityIdFormatResponse_httpStatus,

    -- ** DeleteTrafficMirrorFilter
    deleteTrafficMirrorFilter_dryRun,
    deleteTrafficMirrorFilter_trafficMirrorFilterId,
    deleteTrafficMirrorFilterResponse_trafficMirrorFilterId,
    deleteTrafficMirrorFilterResponse_httpStatus,

    -- ** ReplaceRoute
    replaceRoute_instanceId,
    replaceRoute_localTarget,
    replaceRoute_vpcPeeringConnectionId,
    replaceRoute_dryRun,
    replaceRoute_vpcEndpointId,
    replaceRoute_destinationPrefixListId,
    replaceRoute_destinationIpv6CidrBlock,
    replaceRoute_localGatewayId,
    replaceRoute_egressOnlyInternetGatewayId,
    replaceRoute_carrierGatewayId,
    replaceRoute_destinationCidrBlock,
    replaceRoute_networkInterfaceId,
    replaceRoute_natGatewayId,
    replaceRoute_gatewayId,
    replaceRoute_transitGatewayId,
    replaceRoute_routeTableId,

    -- ** ResetSnapshotAttribute
    resetSnapshotAttribute_dryRun,
    resetSnapshotAttribute_attribute,
    resetSnapshotAttribute_snapshotId,

    -- ** ResetEbsDefaultKmsKeyId
    resetEbsDefaultKmsKeyId_dryRun,
    resetEbsDefaultKmsKeyIdResponse_kmsKeyId,
    resetEbsDefaultKmsKeyIdResponse_httpStatus,

    -- ** CreateTags
    createTags_dryRun,
    createTags_resources,
    createTags_tags,

    -- ** BundleInstance
    bundleInstance_dryRun,
    bundleInstance_instanceId,
    bundleInstance_storage,
    bundleInstanceResponse_bundleTask,
    bundleInstanceResponse_httpStatus,

    -- ** DeleteTransitGatewayPeeringAttachment
    deleteTransitGatewayPeeringAttachment_dryRun,
    deleteTransitGatewayPeeringAttachment_transitGatewayAttachmentId,
    deleteTransitGatewayPeeringAttachmentResponse_transitGatewayPeeringAttachment,
    deleteTransitGatewayPeeringAttachmentResponse_httpStatus,

    -- ** AuthorizeClientVpnIngress
    authorizeClientVpnIngress_accessGroupId,
    authorizeClientVpnIngress_dryRun,
    authorizeClientVpnIngress_authorizeAllGroups,
    authorizeClientVpnIngress_description,
    authorizeClientVpnIngress_clientToken,
    authorizeClientVpnIngress_clientVpnEndpointId,
    authorizeClientVpnIngress_targetNetworkCidr,
    authorizeClientVpnIngressResponse_status,
    authorizeClientVpnIngressResponse_httpStatus,

    -- ** ModifyLaunchTemplate
    modifyLaunchTemplate_defaultVersion,
    modifyLaunchTemplate_dryRun,
    modifyLaunchTemplate_launchTemplateId,
    modifyLaunchTemplate_launchTemplateName,
    modifyLaunchTemplate_clientToken,
    modifyLaunchTemplateResponse_launchTemplate,
    modifyLaunchTemplateResponse_httpStatus,

    -- ** DeleteCustomerGateway
    deleteCustomerGateway_dryRun,
    deleteCustomerGateway_customerGatewayId,

    -- ** TerminateClientVpnConnections
    terminateClientVpnConnections_dryRun,
    terminateClientVpnConnections_connectionId,
    terminateClientVpnConnections_username,
    terminateClientVpnConnections_clientVpnEndpointId,
    terminateClientVpnConnectionsResponse_clientVpnEndpointId,
    terminateClientVpnConnectionsResponse_connectionStatuses,
    terminateClientVpnConnectionsResponse_username,
    terminateClientVpnConnectionsResponse_httpStatus,

    -- ** GetEbsEncryptionByDefault
    getEbsEncryptionByDefault_dryRun,
    getEbsEncryptionByDefaultResponse_ebsEncryptionByDefault,
    getEbsEncryptionByDefaultResponse_httpStatus,

    -- ** CreateVpcPeeringConnection
    createVpcPeeringConnection_tagSpecifications,
    createVpcPeeringConnection_dryRun,
    createVpcPeeringConnection_peerOwnerId,
    createVpcPeeringConnection_vpcId,
    createVpcPeeringConnection_peerRegion,
    createVpcPeeringConnection_peerVpcId,
    createVpcPeeringConnectionResponse_vpcPeeringConnection,
    createVpcPeeringConnectionResponse_httpStatus,

    -- ** DeleteTransitGatewayVpcAttachment
    deleteTransitGatewayVpcAttachment_dryRun,
    deleteTransitGatewayVpcAttachment_transitGatewayAttachmentId,
    deleteTransitGatewayVpcAttachmentResponse_transitGatewayVpcAttachment,
    deleteTransitGatewayVpcAttachmentResponse_httpStatus,

    -- ** ReplaceIamInstanceProfileAssociation
    replaceIamInstanceProfileAssociation_iamInstanceProfile,
    replaceIamInstanceProfileAssociation_associationId,
    replaceIamInstanceProfileAssociationResponse_iamInstanceProfileAssociation,
    replaceIamInstanceProfileAssociationResponse_httpStatus,

    -- ** DeleteTransitGatewayConnectPeer
    deleteTransitGatewayConnectPeer_dryRun,
    deleteTransitGatewayConnectPeer_transitGatewayConnectPeerId,
    deleteTransitGatewayConnectPeerResponse_transitGatewayConnectPeer,
    deleteTransitGatewayConnectPeerResponse_httpStatus,

    -- ** AssociateAddress
    associateAddress_instanceId,
    associateAddress_dryRun,
    associateAddress_allowReassociation,
    associateAddress_networkInterfaceId,
    associateAddress_publicIp,
    associateAddress_allocationId,
    associateAddress_privateIpAddress,
    associateAddressResponse_associationId,
    associateAddressResponse_httpStatus,

    -- ** CancelSpotFleetRequests
    cancelSpotFleetRequests_dryRun,
    cancelSpotFleetRequests_spotFleetRequestIds,
    cancelSpotFleetRequests_terminateInstances,
    cancelSpotFleetRequestsResponse_unsuccessfulFleetRequests,
    cancelSpotFleetRequestsResponse_successfulFleetRequests,
    cancelSpotFleetRequestsResponse_httpStatus,

    -- ** ResetNetworkInterfaceAttribute
    resetNetworkInterfaceAttribute_dryRun,
    resetNetworkInterfaceAttribute_sourceDestCheck,
    resetNetworkInterfaceAttribute_networkInterfaceId,

    -- ** StartInstances
    startInstances_additionalInfo,
    startInstances_dryRun,
    startInstances_instanceIds,
    startInstancesResponse_startingInstances,
    startInstancesResponse_httpStatus,

    -- ** DisassociateTransitGatewayRouteTable
    disassociateTransitGatewayRouteTable_dryRun,
    disassociateTransitGatewayRouteTable_transitGatewayRouteTableId,
    disassociateTransitGatewayRouteTable_transitGatewayAttachmentId,
    disassociateTransitGatewayRouteTableResponse_association,
    disassociateTransitGatewayRouteTableResponse_httpStatus,

    -- ** CopyFpgaImage
    copyFpgaImage_dryRun,
    copyFpgaImage_name,
    copyFpgaImage_description,
    copyFpgaImage_clientToken,
    copyFpgaImage_sourceFpgaImageId,
    copyFpgaImage_sourceRegion,
    copyFpgaImageResponse_fpgaImageId,
    copyFpgaImageResponse_httpStatus,

    -- ** ReleaseHosts
    releaseHosts_hostIds,
    releaseHostsResponse_unsuccessful,
    releaseHostsResponse_successful,
    releaseHostsResponse_httpStatus,

    -- ** DescribeFastSnapshotRestores
    describeFastSnapshotRestores_nextToken,
    describeFastSnapshotRestores_dryRun,
    describeFastSnapshotRestores_maxResults,
    describeFastSnapshotRestores_filters,
    describeFastSnapshotRestoresResponse_fastSnapshotRestores,
    describeFastSnapshotRestoresResponse_nextToken,
    describeFastSnapshotRestoresResponse_httpStatus,

    -- ** DescribeTrafficMirrorFilters
    describeTrafficMirrorFilters_nextToken,
    describeTrafficMirrorFilters_dryRun,
    describeTrafficMirrorFilters_maxResults,
    describeTrafficMirrorFilters_trafficMirrorFilterIds,
    describeTrafficMirrorFilters_filters,
    describeTrafficMirrorFiltersResponse_trafficMirrorFilters,
    describeTrafficMirrorFiltersResponse_nextToken,
    describeTrafficMirrorFiltersResponse_httpStatus,

    -- ** CreateTransitGatewayPrefixListReference
    createTransitGatewayPrefixListReference_dryRun,
    createTransitGatewayPrefixListReference_blackhole,
    createTransitGatewayPrefixListReference_transitGatewayAttachmentId,
    createTransitGatewayPrefixListReference_transitGatewayRouteTableId,
    createTransitGatewayPrefixListReference_prefixListId,
    createTransitGatewayPrefixListReferenceResponse_transitGatewayPrefixListReference,
    createTransitGatewayPrefixListReferenceResponse_httpStatus,

    -- ** DeleteNetworkInterface
    deleteNetworkInterface_dryRun,
    deleteNetworkInterface_networkInterfaceId,

    -- ** CreateTransitGatewayRoute
    createTransitGatewayRoute_dryRun,
    createTransitGatewayRoute_blackhole,
    createTransitGatewayRoute_transitGatewayAttachmentId,
    createTransitGatewayRoute_destinationCidrBlock,
    createTransitGatewayRoute_transitGatewayRouteTableId,
    createTransitGatewayRouteResponse_route,
    createTransitGatewayRouteResponse_httpStatus,

    -- ** DeregisterTransitGatewayMulticastGroupSources
    deregisterTransitGatewayMulticastGroupSources_dryRun,
    deregisterTransitGatewayMulticastGroupSources_transitGatewayMulticastDomainId,
    deregisterTransitGatewayMulticastGroupSources_networkInterfaceIds,
    deregisterTransitGatewayMulticastGroupSources_groupIpAddress,
    deregisterTransitGatewayMulticastGroupSourcesResponse_deregisteredMulticastGroupSources,
    deregisterTransitGatewayMulticastGroupSourcesResponse_httpStatus,

    -- ** DisassociateVpcCidrBlock
    disassociateVpcCidrBlock_associationId,
    disassociateVpcCidrBlockResponse_ipv6CidrBlockAssociation,
    disassociateVpcCidrBlockResponse_cidrBlockAssociation,
    disassociateVpcCidrBlockResponse_vpcId,
    disassociateVpcCidrBlockResponse_httpStatus,

    -- ** DescribeTransitGatewayPeeringAttachments
    describeTransitGatewayPeeringAttachments_nextToken,
    describeTransitGatewayPeeringAttachments_dryRun,
    describeTransitGatewayPeeringAttachments_maxResults,
    describeTransitGatewayPeeringAttachments_transitGatewayAttachmentIds,
    describeTransitGatewayPeeringAttachments_filters,
    describeTransitGatewayPeeringAttachmentsResponse_transitGatewayPeeringAttachments,
    describeTransitGatewayPeeringAttachmentsResponse_nextToken,
    describeTransitGatewayPeeringAttachmentsResponse_httpStatus,

    -- ** GetCoipPoolUsage
    getCoipPoolUsage_nextToken,
    getCoipPoolUsage_dryRun,
    getCoipPoolUsage_maxResults,
    getCoipPoolUsage_filters,
    getCoipPoolUsage_poolId,
    getCoipPoolUsageResponse_coipAddressUsages,
    getCoipPoolUsageResponse_localGatewayRouteTableId,
    getCoipPoolUsageResponse_coipPoolId,
    getCoipPoolUsageResponse_httpStatus,

    -- ** ImportImage
    importImage_hypervisor,
    importImage_platform,
    importImage_tagSpecifications,
    importImage_dryRun,
    importImage_encrypted,
    importImage_roleName,
    importImage_licenseSpecifications,
    importImage_architecture,
    importImage_kmsKeyId,
    importImage_diskContainers,
    importImage_clientData,
    importImage_description,
    importImage_licenseType,
    importImage_clientToken,
    importImageResponse_hypervisor,
    importImageResponse_platform,
    importImageResponse_statusMessage,
    importImageResponse_status,
    importImageResponse_snapshotDetails,
    importImageResponse_encrypted,
    importImageResponse_importTaskId,
    importImageResponse_licenseSpecifications,
    importImageResponse_architecture,
    importImageResponse_imageId,
    importImageResponse_kmsKeyId,
    importImageResponse_tags,
    importImageResponse_description,
    importImageResponse_licenseType,
    importImageResponse_progress,
    importImageResponse_httpStatus,

    -- ** ReplaceTransitGatewayRoute
    replaceTransitGatewayRoute_dryRun,
    replaceTransitGatewayRoute_blackhole,
    replaceTransitGatewayRoute_transitGatewayAttachmentId,
    replaceTransitGatewayRoute_destinationCidrBlock,
    replaceTransitGatewayRoute_transitGatewayRouteTableId,
    replaceTransitGatewayRouteResponse_route,
    replaceTransitGatewayRouteResponse_httpStatus,

    -- ** CreatePlacementGroup
    createPlacementGroup_tagSpecifications,
    createPlacementGroup_dryRun,
    createPlacementGroup_strategy,
    createPlacementGroup_groupName,
    createPlacementGroup_partitionCount,
    createPlacementGroupResponse_placementGroup,
    createPlacementGroupResponse_httpStatus,

    -- ** CreateDefaultVpc
    createDefaultVpc_dryRun,
    createDefaultVpcResponse_vpc,
    createDefaultVpcResponse_httpStatus,

    -- ** CreateNetworkInsightsPath
    createNetworkInsightsPath_tagSpecifications,
    createNetworkInsightsPath_dryRun,
    createNetworkInsightsPath_destinationIp,
    createNetworkInsightsPath_sourceIp,
    createNetworkInsightsPath_destinationPort,
    createNetworkInsightsPath_source,
    createNetworkInsightsPath_destination,
    createNetworkInsightsPath_protocol,
    createNetworkInsightsPath_clientToken,
    createNetworkInsightsPathResponse_networkInsightsPath,
    createNetworkInsightsPathResponse_httpStatus,

    -- ** ModifyTrafficMirrorSession
    modifyTrafficMirrorSession_removeFields,
    modifyTrafficMirrorSession_dryRun,
    modifyTrafficMirrorSession_packetLength,
    modifyTrafficMirrorSession_trafficMirrorFilterId,
    modifyTrafficMirrorSession_description,
    modifyTrafficMirrorSession_trafficMirrorTargetId,
    modifyTrafficMirrorSession_sessionNumber,
    modifyTrafficMirrorSession_virtualNetworkId,
    modifyTrafficMirrorSession_trafficMirrorSessionId,
    modifyTrafficMirrorSessionResponse_trafficMirrorSession,
    modifyTrafficMirrorSessionResponse_httpStatus,

    -- ** RunScheduledInstances
    runScheduledInstances_dryRun,
    runScheduledInstances_clientToken,
    runScheduledInstances_instanceCount,
    runScheduledInstances_launchSpecification,
    runScheduledInstances_scheduledInstanceId,
    runScheduledInstancesResponse_instanceIdSet,
    runScheduledInstancesResponse_httpStatus,

    -- ** DescribeDhcpOptions
    describeDhcpOptions_nextToken,
    describeDhcpOptions_dhcpOptionsIds,
    describeDhcpOptions_dryRun,
    describeDhcpOptions_maxResults,
    describeDhcpOptions_filters,
    describeDhcpOptionsResponse_nextToken,
    describeDhcpOptionsResponse_dhcpOptions,
    describeDhcpOptionsResponse_httpStatus,

    -- ** DescribeCapacityReservations
    describeCapacityReservations_nextToken,
    describeCapacityReservations_dryRun,
    describeCapacityReservations_maxResults,
    describeCapacityReservations_capacityReservationIds,
    describeCapacityReservations_filters,
    describeCapacityReservationsResponse_capacityReservations,
    describeCapacityReservationsResponse_nextToken,
    describeCapacityReservationsResponse_httpStatus,

    -- ** DescribeCustomerGateways
    describeCustomerGateways_dryRun,
    describeCustomerGateways_customerGatewayIds,
    describeCustomerGateways_filters,
    describeCustomerGatewaysResponse_customerGateways,
    describeCustomerGatewaysResponse_httpStatus,

    -- ** DeleteNatGateway
    deleteNatGateway_dryRun,
    deleteNatGateway_natGatewayId,
    deleteNatGatewayResponse_natGatewayId,
    deleteNatGatewayResponse_httpStatus,

    -- ** DescribeClientVpnAuthorizationRules
    describeClientVpnAuthorizationRules_nextToken,
    describeClientVpnAuthorizationRules_dryRun,
    describeClientVpnAuthorizationRules_maxResults,
    describeClientVpnAuthorizationRules_filters,
    describeClientVpnAuthorizationRules_clientVpnEndpointId,
    describeClientVpnAuthorizationRulesResponse_nextToken,
    describeClientVpnAuthorizationRulesResponse_authorizationRules,
    describeClientVpnAuthorizationRulesResponse_httpStatus,

    -- ** StopInstances
    stopInstances_dryRun,
    stopInstances_force,
    stopInstances_hibernate,
    stopInstances_instanceIds,
    stopInstancesResponse_stoppingInstances,
    stopInstancesResponse_httpStatus,

    -- ** ReplaceRouteTableAssociation
    replaceRouteTableAssociation_dryRun,
    replaceRouteTableAssociation_associationId,
    replaceRouteTableAssociation_routeTableId,
    replaceRouteTableAssociationResponse_associationState,
    replaceRouteTableAssociationResponse_newAssociationId,
    replaceRouteTableAssociationResponse_httpStatus,

    -- ** DeleteTransitGatewayMulticastDomain
    deleteTransitGatewayMulticastDomain_dryRun,
    deleteTransitGatewayMulticastDomain_transitGatewayMulticastDomainId,
    deleteTransitGatewayMulticastDomainResponse_transitGatewayMulticastDomain,
    deleteTransitGatewayMulticastDomainResponse_httpStatus,

    -- ** DeleteSubnet
    deleteSubnet_dryRun,
    deleteSubnet_subnetId,

    -- * Types

    -- ** AccountAttribute
    accountAttribute_attributeName,
    accountAttribute_attributeValues,

    -- ** AccountAttributeValue
    accountAttributeValue_attributeValue,

    -- ** ActiveInstance
    activeInstance_instanceId,
    activeInstance_instanceType,
    activeInstance_spotInstanceRequestId,
    activeInstance_instanceHealth,

    -- ** AddPrefixListEntry
    addPrefixListEntry_description,
    addPrefixListEntry_cidr,

    -- ** Address
    address_customerOwnedIpv4Pool,
    address_instanceId,
    address_domain,
    address_carrierIp,
    address_customerOwnedIp,
    address_networkInterfaceOwnerId,
    address_associationId,
    address_tags,
    address_networkInterfaceId,
    address_publicIpv4Pool,
    address_publicIp,
    address_allocationId,
    address_networkBorderGroup,
    address_privateIpAddress,

    -- ** AddressAttribute
    addressAttribute_ptrRecord,
    addressAttribute_publicIp,
    addressAttribute_allocationId,
    addressAttribute_ptrRecordUpdate,

    -- ** AllowedPrincipal
    allowedPrincipal_principal,
    allowedPrincipal_principalType,

    -- ** AlternatePathHint
    alternatePathHint_componentId,
    alternatePathHint_componentArn,

    -- ** AnalysisAclRule
    analysisAclRule_portRange,
    analysisAclRule_ruleAction,
    analysisAclRule_egress,
    analysisAclRule_cidr,
    analysisAclRule_protocol,
    analysisAclRule_ruleNumber,

    -- ** AnalysisComponent
    analysisComponent_arn,
    analysisComponent_id,

    -- ** AnalysisLoadBalancerListener
    analysisLoadBalancerListener_loadBalancerPort,
    analysisLoadBalancerListener_instancePort,

    -- ** AnalysisLoadBalancerTarget
    analysisLoadBalancerTarget_address,
    analysisLoadBalancerTarget_availabilityZone,
    analysisLoadBalancerTarget_instance,
    analysisLoadBalancerTarget_port,

    -- ** AnalysisPacketHeader
    analysisPacketHeader_destinationAddresses,
    analysisPacketHeader_sourceAddresses,
    analysisPacketHeader_destinationPortRanges,
    analysisPacketHeader_protocol,
    analysisPacketHeader_sourcePortRanges,

    -- ** AnalysisRouteTableRoute
    analysisRouteTableRoute_instanceId,
    analysisRouteTableRoute_origin,
    analysisRouteTableRoute_vpcPeeringConnectionId,
    analysisRouteTableRoute_destinationPrefixListId,
    analysisRouteTableRoute_destinationCidr,
    analysisRouteTableRoute_egressOnlyInternetGatewayId,
    analysisRouteTableRoute_networkInterfaceId,
    analysisRouteTableRoute_natGatewayId,
    analysisRouteTableRoute_gatewayId,
    analysisRouteTableRoute_transitGatewayId,

    -- ** AnalysisSecurityGroupRule
    analysisSecurityGroupRule_securityGroupId,
    analysisSecurityGroupRule_portRange,
    analysisSecurityGroupRule_direction,
    analysisSecurityGroupRule_prefixListId,
    analysisSecurityGroupRule_cidr,
    analysisSecurityGroupRule_protocol,

    -- ** AssignedPrivateIpAddress
    assignedPrivateIpAddress_privateIpAddress,

    -- ** AssociatedRole
    associatedRole_certificateS3ObjectKey,
    associatedRole_encryptionKmsKeyId,
    associatedRole_certificateS3BucketName,
    associatedRole_associatedRoleArn,

    -- ** AssociatedTargetNetwork
    associatedTargetNetwork_networkType,
    associatedTargetNetwork_networkId,

    -- ** AssociationStatus
    associationStatus_message,
    associationStatus_code,

    -- ** AttributeBooleanValue
    attributeBooleanValue_value,

    -- ** AttributeValue
    attributeValue_value,

    -- ** AuthorizationRule
    authorizationRule_clientVpnEndpointId,
    authorizationRule_status,
    authorizationRule_destinationCidr,
    authorizationRule_accessAll,
    authorizationRule_groupId,
    authorizationRule_description,

    -- ** AvailabilityZone
    availabilityZone_regionName,
    availabilityZone_parentZoneId,
    availabilityZone_zoneName,
    availabilityZone_zoneType,
    availabilityZone_zoneId,
    availabilityZone_groupName,
    availabilityZone_optInStatus,
    availabilityZone_state,
    availabilityZone_messages,
    availabilityZone_networkBorderGroup,
    availabilityZone_parentZoneName,

    -- ** AvailabilityZoneMessage
    availabilityZoneMessage_message,

    -- ** AvailableCapacity
    availableCapacity_availableInstanceCapacity,
    availableCapacity_availableVCpus,

    -- ** BlobAttributeValue
    blobAttributeValue_value,

    -- ** BlockDeviceMapping
    blockDeviceMapping_ebs,
    blockDeviceMapping_noDevice,
    blockDeviceMapping_virtualName,
    blockDeviceMapping_deviceName,

    -- ** BundleTask
    bundleTask_bundleTaskError,
    bundleTask_bundleId,
    bundleTask_instanceId,
    bundleTask_progress,
    bundleTask_startTime,
    bundleTask_state,
    bundleTask_storage,
    bundleTask_updateTime,

    -- ** BundleTaskError
    bundleTaskError_message,
    bundleTaskError_code,

    -- ** ByoipCidr
    byoipCidr_statusMessage,
    byoipCidr_state,
    byoipCidr_cidr,
    byoipCidr_description,

    -- ** CancelSpotFleetRequestsError
    cancelSpotFleetRequestsError_message,
    cancelSpotFleetRequestsError_code,

    -- ** CancelSpotFleetRequestsErrorItem
    cancelSpotFleetRequestsErrorItem_error,
    cancelSpotFleetRequestsErrorItem_spotFleetRequestId,

    -- ** CancelSpotFleetRequestsSuccessItem
    cancelSpotFleetRequestsSuccessItem_currentSpotFleetRequestState,
    cancelSpotFleetRequestsSuccessItem_previousSpotFleetRequestState,
    cancelSpotFleetRequestsSuccessItem_spotFleetRequestId,

    -- ** CancelledSpotInstanceRequest
    cancelledSpotInstanceRequest_state,
    cancelledSpotInstanceRequest_spotInstanceRequestId,

    -- ** CapacityReservation
    capacityReservation_ownerId,
    capacityReservation_startDate,
    capacityReservation_instanceType,
    capacityReservation_ebsOptimized,
    capacityReservation_endDateType,
    capacityReservation_availableInstanceCount,
    capacityReservation_createDate,
    capacityReservation_tenancy,
    capacityReservation_availabilityZoneId,
    capacityReservation_state,
    capacityReservation_availabilityZone,
    capacityReservation_capacityReservationId,
    capacityReservation_tags,
    capacityReservation_capacityReservationArn,
    capacityReservation_instanceMatchCriteria,
    capacityReservation_ephemeralStorage,
    capacityReservation_instancePlatform,
    capacityReservation_endDate,
    capacityReservation_totalInstanceCount,

    -- ** CapacityReservationGroup
    capacityReservationGroup_ownerId,
    capacityReservationGroup_groupArn,

    -- ** CapacityReservationOptions
    capacityReservationOptions_usageStrategy,

    -- ** CapacityReservationOptionsRequest
    capacityReservationOptionsRequest_usageStrategy,

    -- ** CapacityReservationSpecification
    capacityReservationSpecification_capacityReservationPreference,
    capacityReservationSpecification_capacityReservationTarget,

    -- ** CapacityReservationSpecificationResponse
    capacityReservationSpecificationResponse_capacityReservationPreference,
    capacityReservationSpecificationResponse_capacityReservationTarget,

    -- ** CapacityReservationTarget
    capacityReservationTarget_capacityReservationResourceGroupArn,
    capacityReservationTarget_capacityReservationId,

    -- ** CapacityReservationTargetResponse
    capacityReservationTargetResponse_capacityReservationResourceGroupArn,
    capacityReservationTargetResponse_capacityReservationId,

    -- ** CarrierGateway
    carrierGateway_ownerId,
    carrierGateway_state,
    carrierGateway_tags,
    carrierGateway_carrierGatewayId,
    carrierGateway_vpcId,

    -- ** CertificateAuthentication
    certificateAuthentication_clientRootCertificateChain,

    -- ** CertificateAuthenticationRequest
    certificateAuthenticationRequest_clientRootCertificateChainArn,

    -- ** CidrAuthorizationContext
    cidrAuthorizationContext_message,
    cidrAuthorizationContext_signature,

    -- ** CidrBlock
    cidrBlock_cidrBlock,

    -- ** ClassicLinkDnsSupport
    classicLinkDnsSupport_classicLinkDnsSupported,
    classicLinkDnsSupport_vpcId,

    -- ** ClassicLinkInstance
    classicLinkInstance_groups,
    classicLinkInstance_instanceId,
    classicLinkInstance_tags,
    classicLinkInstance_vpcId,

    -- ** ClassicLoadBalancer
    classicLoadBalancer_name,

    -- ** ClassicLoadBalancersConfig
    classicLoadBalancersConfig_classicLoadBalancers,

    -- ** ClientCertificateRevocationListStatus
    clientCertificateRevocationListStatus_message,
    clientCertificateRevocationListStatus_code,

    -- ** ClientConnectOptions
    clientConnectOptions_enabled,
    clientConnectOptions_lambdaFunctionArn,

    -- ** ClientConnectResponseOptions
    clientConnectResponseOptions_status,
    clientConnectResponseOptions_enabled,
    clientConnectResponseOptions_lambdaFunctionArn,

    -- ** ClientData
    clientData_uploadEnd,
    clientData_comment,
    clientData_uploadSize,
    clientData_uploadStart,

    -- ** ClientVpnAuthentication
    clientVpnAuthentication_federatedAuthentication,
    clientVpnAuthentication_activeDirectory,
    clientVpnAuthentication_mutualAuthentication,
    clientVpnAuthentication_type,

    -- ** ClientVpnAuthenticationRequest
    clientVpnAuthenticationRequest_federatedAuthentication,
    clientVpnAuthenticationRequest_activeDirectory,
    clientVpnAuthenticationRequest_mutualAuthentication,
    clientVpnAuthenticationRequest_type,

    -- ** ClientVpnAuthorizationRuleStatus
    clientVpnAuthorizationRuleStatus_message,
    clientVpnAuthorizationRuleStatus_code,

    -- ** ClientVpnConnection
    clientVpnConnection_clientVpnEndpointId,
    clientVpnConnection_status,
    clientVpnConnection_ingressPackets,
    clientVpnConnection_egressBytes,
    clientVpnConnection_connectionId,
    clientVpnConnection_connectionEstablishedTime,
    clientVpnConnection_postureComplianceStatuses,
    clientVpnConnection_commonName,
    clientVpnConnection_connectionEndTime,
    clientVpnConnection_ingressBytes,
    clientVpnConnection_egressPackets,
    clientVpnConnection_timestamp,
    clientVpnConnection_username,
    clientVpnConnection_clientIp,

    -- ** ClientVpnConnectionStatus
    clientVpnConnectionStatus_message,
    clientVpnConnectionStatus_code,

    -- ** ClientVpnEndpoint
    clientVpnEndpoint_clientVpnEndpointId,
    clientVpnEndpoint_securityGroupIds,
    clientVpnEndpoint_clientCidrBlock,
    clientVpnEndpoint_status,
    clientVpnEndpoint_creationTime,
    clientVpnEndpoint_associatedTargetNetworks,
    clientVpnEndpoint_deletionTime,
    clientVpnEndpoint_selfServicePortalUrl,
    clientVpnEndpoint_authenticationOptions,
    clientVpnEndpoint_serverCertificateArn,
    clientVpnEndpoint_transportProtocol,
    clientVpnEndpoint_connectionLogOptions,
    clientVpnEndpoint_clientConnectOptions,
    clientVpnEndpoint_dnsServers,
    clientVpnEndpoint_tags,
    clientVpnEndpoint_vpnPort,
    clientVpnEndpoint_dnsName,
    clientVpnEndpoint_vpnProtocol,
    clientVpnEndpoint_description,
    clientVpnEndpoint_vpcId,
    clientVpnEndpoint_splitTunnel,

    -- ** ClientVpnEndpointAttributeStatus
    clientVpnEndpointAttributeStatus_message,
    clientVpnEndpointAttributeStatus_code,

    -- ** ClientVpnEndpointStatus
    clientVpnEndpointStatus_message,
    clientVpnEndpointStatus_code,

    -- ** ClientVpnRoute
    clientVpnRoute_clientVpnEndpointId,
    clientVpnRoute_status,
    clientVpnRoute_origin,
    clientVpnRoute_destinationCidr,
    clientVpnRoute_description,
    clientVpnRoute_type,
    clientVpnRoute_targetSubnet,

    -- ** ClientVpnRouteStatus
    clientVpnRouteStatus_message,
    clientVpnRouteStatus_code,

    -- ** CoipAddressUsage
    coipAddressUsage_awsAccountId,
    coipAddressUsage_coIp,
    coipAddressUsage_awsService,
    coipAddressUsage_allocationId,

    -- ** CoipPool
    coipPool_poolId,
    coipPool_poolArn,
    coipPool_poolCidrs,
    coipPool_localGatewayRouteTableId,
    coipPool_tags,

    -- ** ConnectionLogOptions
    connectionLogOptions_cloudwatchLogStream,
    connectionLogOptions_enabled,
    connectionLogOptions_cloudwatchLogGroup,

    -- ** ConnectionLogResponseOptions
    connectionLogResponseOptions_cloudwatchLogStream,
    connectionLogResponseOptions_enabled,
    connectionLogResponseOptions_cloudwatchLogGroup,

    -- ** ConnectionNotification
    connectionNotification_connectionEvents,
    connectionNotification_connectionNotificationId,
    connectionNotification_connectionNotificationType,
    connectionNotification_vpcEndpointId,
    connectionNotification_serviceId,
    connectionNotification_connectionNotificationState,
    connectionNotification_connectionNotificationArn,

    -- ** ConversionTask
    conversionTask_statusMessage,
    conversionTask_importInstance,
    conversionTask_expirationTime,
    conversionTask_importVolume,
    conversionTask_state,
    conversionTask_tags,
    conversionTask_conversionTaskId,

    -- ** CpuOptions
    cpuOptions_threadsPerCore,
    cpuOptions_coreCount,

    -- ** CpuOptionsRequest
    cpuOptionsRequest_threadsPerCore,
    cpuOptionsRequest_coreCount,

    -- ** CreateFleetError
    createFleetError_launchTemplateAndOverrides,
    createFleetError_lifecycle,
    createFleetError_errorMessage,
    createFleetError_errorCode,

    -- ** CreateFleetInstance
    createFleetInstance_instanceIds,
    createFleetInstance_platform,
    createFleetInstance_instanceType,
    createFleetInstance_launchTemplateAndOverrides,
    createFleetInstance_lifecycle,

    -- ** CreateTransitGatewayConnectRequestOptions
    createTransitGatewayConnectRequestOptions_protocol,

    -- ** CreateTransitGatewayMulticastDomainRequestOptions
    createTransitGatewayMulticastDomainRequestOptions_igmpv2Support,
    createTransitGatewayMulticastDomainRequestOptions_autoAcceptSharedAssociations,
    createTransitGatewayMulticastDomainRequestOptions_staticSourcesSupport,

    -- ** CreateTransitGatewayVpcAttachmentRequestOptions
    createTransitGatewayVpcAttachmentRequestOptions_applianceModeSupport,
    createTransitGatewayVpcAttachmentRequestOptions_dnsSupport,
    createTransitGatewayVpcAttachmentRequestOptions_ipv6Support,

    -- ** CreateVolumePermission
    createVolumePermission_group,
    createVolumePermission_userId,

    -- ** CreateVolumePermissionModifications
    createVolumePermissionModifications_add,
    createVolumePermissionModifications_remove,

    -- ** CreditSpecification
    creditSpecification_cpuCredits,

    -- ** CreditSpecificationRequest
    creditSpecificationRequest_cpuCredits,

    -- ** CustomerGateway
    customerGateway_certificateArn,
    customerGateway_deviceName,
    customerGateway_tags,
    customerGateway_bgpAsn,
    customerGateway_customerGatewayId,
    customerGateway_ipAddress,
    customerGateway_state,
    customerGateway_type,

    -- ** DeleteFleetError
    deleteFleetError_message,
    deleteFleetError_code,

    -- ** DeleteFleetErrorItem
    deleteFleetErrorItem_fleetId,
    deleteFleetErrorItem_error,

    -- ** DeleteFleetSuccessItem
    deleteFleetSuccessItem_fleetId,
    deleteFleetSuccessItem_currentFleetState,
    deleteFleetSuccessItem_previousFleetState,

    -- ** DeleteLaunchTemplateVersionsResponseErrorItem
    deleteLaunchTemplateVersionsResponseErrorItem_responseError,
    deleteLaunchTemplateVersionsResponseErrorItem_launchTemplateId,
    deleteLaunchTemplateVersionsResponseErrorItem_launchTemplateName,
    deleteLaunchTemplateVersionsResponseErrorItem_versionNumber,

    -- ** DeleteLaunchTemplateVersionsResponseSuccessItem
    deleteLaunchTemplateVersionsResponseSuccessItem_launchTemplateId,
    deleteLaunchTemplateVersionsResponseSuccessItem_launchTemplateName,
    deleteLaunchTemplateVersionsResponseSuccessItem_versionNumber,

    -- ** DeleteQueuedReservedInstancesError
    deleteQueuedReservedInstancesError_message,
    deleteQueuedReservedInstancesError_code,

    -- ** DeregisterInstanceTagAttributeRequest
    deregisterInstanceTagAttributeRequest_instanceTagKeys,
    deregisterInstanceTagAttributeRequest_includeAllTagsOfInstance,

    -- ** DescribeFastSnapshotRestoreSuccessItem
    describeFastSnapshotRestoreSuccessItem_ownerAlias,
    describeFastSnapshotRestoreSuccessItem_ownerId,
    describeFastSnapshotRestoreSuccessItem_stateTransitionReason,
    describeFastSnapshotRestoreSuccessItem_optimizingTime,
    describeFastSnapshotRestoreSuccessItem_state,
    describeFastSnapshotRestoreSuccessItem_availabilityZone,
    describeFastSnapshotRestoreSuccessItem_disablingTime,
    describeFastSnapshotRestoreSuccessItem_snapshotId,
    describeFastSnapshotRestoreSuccessItem_enablingTime,
    describeFastSnapshotRestoreSuccessItem_enabledTime,
    describeFastSnapshotRestoreSuccessItem_disabledTime,

    -- ** DescribeFleetError
    describeFleetError_launchTemplateAndOverrides,
    describeFleetError_lifecycle,
    describeFleetError_errorMessage,
    describeFleetError_errorCode,

    -- ** DescribeFleetsInstances
    describeFleetsInstances_instanceIds,
    describeFleetsInstances_platform,
    describeFleetsInstances_instanceType,
    describeFleetsInstances_launchTemplateAndOverrides,
    describeFleetsInstances_lifecycle,

    -- ** DhcpConfiguration
    dhcpConfiguration_key,
    dhcpConfiguration_values,

    -- ** DhcpOptions
    dhcpOptions_ownerId,
    dhcpOptions_dhcpConfigurations,
    dhcpOptions_dhcpOptionsId,
    dhcpOptions_tags,

    -- ** DirectoryServiceAuthentication
    directoryServiceAuthentication_directoryId,

    -- ** DirectoryServiceAuthenticationRequest
    directoryServiceAuthenticationRequest_directoryId,

    -- ** DisableFastSnapshotRestoreErrorItem
    disableFastSnapshotRestoreErrorItem_fastSnapshotRestoreStateErrors,
    disableFastSnapshotRestoreErrorItem_snapshotId,

    -- ** DisableFastSnapshotRestoreStateError
    disableFastSnapshotRestoreStateError_message,
    disableFastSnapshotRestoreStateError_code,

    -- ** DisableFastSnapshotRestoreStateErrorItem
    disableFastSnapshotRestoreStateErrorItem_availabilityZone,
    disableFastSnapshotRestoreStateErrorItem_error,

    -- ** DisableFastSnapshotRestoreSuccessItem
    disableFastSnapshotRestoreSuccessItem_ownerAlias,
    disableFastSnapshotRestoreSuccessItem_ownerId,
    disableFastSnapshotRestoreSuccessItem_stateTransitionReason,
    disableFastSnapshotRestoreSuccessItem_optimizingTime,
    disableFastSnapshotRestoreSuccessItem_state,
    disableFastSnapshotRestoreSuccessItem_availabilityZone,
    disableFastSnapshotRestoreSuccessItem_disablingTime,
    disableFastSnapshotRestoreSuccessItem_snapshotId,
    disableFastSnapshotRestoreSuccessItem_enablingTime,
    disableFastSnapshotRestoreSuccessItem_enabledTime,
    disableFastSnapshotRestoreSuccessItem_disabledTime,

    -- ** DiskImage
    diskImage_volume,
    diskImage_image,
    diskImage_description,

    -- ** DiskImageDescription
    diskImageDescription_format,
    diskImageDescription_importManifestUrl,
    diskImageDescription_checksum,
    diskImageDescription_size,

    -- ** DiskImageDetail
    diskImageDetail_bytes,
    diskImageDetail_format,
    diskImageDetail_importManifestUrl,

    -- ** DiskImageVolumeDescription
    diskImageVolumeDescription_id,
    diskImageVolumeDescription_size,

    -- ** DiskInfo
    diskInfo_sizeInGB,
    diskInfo_count,
    diskInfo_type,

    -- ** DnsEntry
    dnsEntry_hostedZoneId,
    dnsEntry_dnsName,

    -- ** DnsServersOptionsModifyStructure
    dnsServersOptionsModifyStructure_enabled,
    dnsServersOptionsModifyStructure_customDnsServers,

    -- ** EbsBlockDevice
    ebsBlockDevice_encrypted,
    ebsBlockDevice_outpostArn,
    ebsBlockDevice_throughput,
    ebsBlockDevice_kmsKeyId,
    ebsBlockDevice_deleteOnTermination,
    ebsBlockDevice_snapshotId,
    ebsBlockDevice_volumeType,
    ebsBlockDevice_volumeSize,
    ebsBlockDevice_iops,

    -- ** EbsInfo
    ebsInfo_nvmeSupport,
    ebsInfo_ebsOptimizedInfo,
    ebsInfo_ebsOptimizedSupport,
    ebsInfo_encryptionSupport,

    -- ** EbsInstanceBlockDevice
    ebsInstanceBlockDevice_status,
    ebsInstanceBlockDevice_attachTime,
    ebsInstanceBlockDevice_volumeId,
    ebsInstanceBlockDevice_deleteOnTermination,

    -- ** EbsInstanceBlockDeviceSpecification
    ebsInstanceBlockDeviceSpecification_volumeId,
    ebsInstanceBlockDeviceSpecification_deleteOnTermination,

    -- ** EbsOptimizedInfo
    ebsOptimizedInfo_baselineBandwidthInMbps,
    ebsOptimizedInfo_maximumIops,
    ebsOptimizedInfo_maximumBandwidthInMbps,
    ebsOptimizedInfo_maximumThroughputInMBps,
    ebsOptimizedInfo_baselineIops,
    ebsOptimizedInfo_baselineThroughputInMBps,

    -- ** EgressOnlyInternetGateway
    egressOnlyInternetGateway_egressOnlyInternetGatewayId,
    egressOnlyInternetGateway_tags,
    egressOnlyInternetGateway_attachments,

    -- ** ElasticGpuAssociation
    elasticGpuAssociation_elasticGpuAssociationTime,
    elasticGpuAssociation_elasticGpuId,
    elasticGpuAssociation_elasticGpuAssociationState,
    elasticGpuAssociation_elasticGpuAssociationId,

    -- ** ElasticGpuHealth
    elasticGpuHealth_status,

    -- ** ElasticGpuSpecification
    elasticGpuSpecification_type,

    -- ** ElasticGpuSpecificationResponse
    elasticGpuSpecificationResponse_type,

    -- ** ElasticGpus
    elasticGpus_elasticGpuType,
    elasticGpus_instanceId,
    elasticGpus_elasticGpuHealth,
    elasticGpus_elasticGpuId,
    elasticGpus_availabilityZone,
    elasticGpus_tags,
    elasticGpus_elasticGpuState,

    -- ** ElasticInferenceAccelerator
    elasticInferenceAccelerator_count,
    elasticInferenceAccelerator_type,

    -- ** ElasticInferenceAcceleratorAssociation
    elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorAssociationId,
    elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorAssociationState,
    elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorArn,
    elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorAssociationTime,

    -- ** EnableFastSnapshotRestoreErrorItem
    enableFastSnapshotRestoreErrorItem_fastSnapshotRestoreStateErrors,
    enableFastSnapshotRestoreErrorItem_snapshotId,

    -- ** EnableFastSnapshotRestoreStateError
    enableFastSnapshotRestoreStateError_message,
    enableFastSnapshotRestoreStateError_code,

    -- ** EnableFastSnapshotRestoreStateErrorItem
    enableFastSnapshotRestoreStateErrorItem_availabilityZone,
    enableFastSnapshotRestoreStateErrorItem_error,

    -- ** EnableFastSnapshotRestoreSuccessItem
    enableFastSnapshotRestoreSuccessItem_ownerAlias,
    enableFastSnapshotRestoreSuccessItem_ownerId,
    enableFastSnapshotRestoreSuccessItem_stateTransitionReason,
    enableFastSnapshotRestoreSuccessItem_optimizingTime,
    enableFastSnapshotRestoreSuccessItem_state,
    enableFastSnapshotRestoreSuccessItem_availabilityZone,
    enableFastSnapshotRestoreSuccessItem_disablingTime,
    enableFastSnapshotRestoreSuccessItem_snapshotId,
    enableFastSnapshotRestoreSuccessItem_enablingTime,
    enableFastSnapshotRestoreSuccessItem_enabledTime,
    enableFastSnapshotRestoreSuccessItem_disabledTime,

    -- ** EnclaveOptions
    enclaveOptions_enabled,

    -- ** EnclaveOptionsRequest
    enclaveOptionsRequest_enabled,

    -- ** EventInformation
    eventInformation_instanceId,
    eventInformation_eventDescription,
    eventInformation_eventSubType,

    -- ** Explanation
    explanation_loadBalancerTargetGroups,
    explanation_loadBalancerArn,
    explanation_securityGroupRule,
    explanation_customerGateway,
    explanation_availabilityZones,
    explanation_loadBalancerTargetGroup,
    explanation_explanationCode,
    explanation_address,
    explanation_natGateway,
    explanation_loadBalancerTarget,
    explanation_internetGateway,
    explanation_networkInterface,
    explanation_subnet,
    explanation_sourceVpc,
    explanation_aclRule,
    explanation_protocols,
    explanation_direction,
    explanation_securityGroups,
    explanation_prefixList,
    explanation_packetField,
    explanation_cidrs,
    explanation_component,
    explanation_state,
    explanation_routeTable,
    explanation_destination,
    explanation_subnetRouteTable,
    explanation_attachedTo,
    explanation_securityGroup,
    explanation_vpcPeeringConnection,
    explanation_destinationVpc,
    explanation_elasticLoadBalancerListener,
    explanation_port,
    explanation_acl,
    explanation_vpcEndpoint,
    explanation_routeTableRoute,
    explanation_vpnConnection,
    explanation_missingComponent,
    explanation_addresses,
    explanation_classicLoadBalancerListener,
    explanation_ingressRouteTable,
    explanation_vpnGateway,
    explanation_portRanges,
    explanation_loadBalancerListenerPort,
    explanation_vpc,
    explanation_loadBalancerTargetPort,

    -- ** ExportImageTask
    exportImageTask_statusMessage,
    exportImageTask_status,
    exportImageTask_imageId,
    exportImageTask_tags,
    exportImageTask_s3ExportLocation,
    exportImageTask_description,
    exportImageTask_exportImageTaskId,
    exportImageTask_progress,

    -- ** ExportTask
    exportTask_tags,
    exportTask_description,
    exportTask_exportTaskId,
    exportTask_exportToS3Task,
    exportTask_instanceExportDetails,
    exportTask_state,
    exportTask_statusMessage,

    -- ** ExportTaskS3Location
    exportTaskS3Location_s3Bucket,
    exportTaskS3Location_s3Prefix,

    -- ** ExportTaskS3LocationRequest
    exportTaskS3LocationRequest_s3Prefix,
    exportTaskS3LocationRequest_s3Bucket,

    -- ** ExportToS3Task
    exportToS3Task_containerFormat,
    exportToS3Task_diskImageFormat,
    exportToS3Task_s3Bucket,
    exportToS3Task_s3Key,

    -- ** ExportToS3TaskSpecification
    exportToS3TaskSpecification_containerFormat,
    exportToS3TaskSpecification_diskImageFormat,
    exportToS3TaskSpecification_s3Bucket,
    exportToS3TaskSpecification_s3Prefix,

    -- ** FailedQueuedPurchaseDeletion
    failedQueuedPurchaseDeletion_reservedInstancesId,
    failedQueuedPurchaseDeletion_error,

    -- ** FederatedAuthentication
    federatedAuthentication_selfServiceSamlProviderArn,
    federatedAuthentication_samlProviderArn,

    -- ** FederatedAuthenticationRequest
    federatedAuthenticationRequest_selfServiceSAMLProviderArn,
    federatedAuthenticationRequest_sAMLProviderArn,

    -- ** Filter
    filter_values,
    filter_name,

    -- ** FleetData
    fleetData_launchTemplateConfigs,
    fleetData_fleetState,
    fleetData_onDemandOptions,
    fleetData_fleetId,
    fleetData_fulfilledOnDemandCapacity,
    fleetData_validFrom,
    fleetData_replaceUnhealthyInstances,
    fleetData_instances,
    fleetData_validUntil,
    fleetData_activityStatus,
    fleetData_tags,
    fleetData_createTime,
    fleetData_excessCapacityTerminationPolicy,
    fleetData_errors,
    fleetData_type,
    fleetData_spotOptions,
    fleetData_targetCapacitySpecification,
    fleetData_fulfilledCapacity,
    fleetData_clientToken,
    fleetData_terminateInstancesWithExpiration,

    -- ** FleetLaunchTemplateConfig
    fleetLaunchTemplateConfig_launchTemplateSpecification,
    fleetLaunchTemplateConfig_overrides,

    -- ** FleetLaunchTemplateConfigRequest
    fleetLaunchTemplateConfigRequest_launchTemplateSpecification,
    fleetLaunchTemplateConfigRequest_overrides,

    -- ** FleetLaunchTemplateOverrides
    fleetLaunchTemplateOverrides_instanceType,
    fleetLaunchTemplateOverrides_placement,
    fleetLaunchTemplateOverrides_priority,
    fleetLaunchTemplateOverrides_availabilityZone,
    fleetLaunchTemplateOverrides_maxPrice,
    fleetLaunchTemplateOverrides_subnetId,
    fleetLaunchTemplateOverrides_weightedCapacity,

    -- ** FleetLaunchTemplateOverridesRequest
    fleetLaunchTemplateOverridesRequest_instanceType,
    fleetLaunchTemplateOverridesRequest_placement,
    fleetLaunchTemplateOverridesRequest_priority,
    fleetLaunchTemplateOverridesRequest_availabilityZone,
    fleetLaunchTemplateOverridesRequest_maxPrice,
    fleetLaunchTemplateOverridesRequest_subnetId,
    fleetLaunchTemplateOverridesRequest_weightedCapacity,

    -- ** FleetLaunchTemplateSpecification
    fleetLaunchTemplateSpecification_launchTemplateId,
    fleetLaunchTemplateSpecification_launchTemplateName,
    fleetLaunchTemplateSpecification_version,

    -- ** FleetLaunchTemplateSpecificationRequest
    fleetLaunchTemplateSpecificationRequest_launchTemplateId,
    fleetLaunchTemplateSpecificationRequest_launchTemplateName,
    fleetLaunchTemplateSpecificationRequest_version,

    -- ** FleetSpotCapacityRebalance
    fleetSpotCapacityRebalance_replacementStrategy,

    -- ** FleetSpotCapacityRebalanceRequest
    fleetSpotCapacityRebalanceRequest_replacementStrategy,

    -- ** FleetSpotMaintenanceStrategies
    fleetSpotMaintenanceStrategies_capacityRebalance,

    -- ** FleetSpotMaintenanceStrategiesRequest
    fleetSpotMaintenanceStrategiesRequest_capacityRebalance,

    -- ** FlowLog
    flowLog_resourceId,
    flowLog_maxAggregationInterval,
    flowLog_creationTime,
    flowLog_deliverLogsStatus,
    flowLog_flowLogId,
    flowLog_logDestination,
    flowLog_trafficType,
    flowLog_logFormat,
    flowLog_logGroupName,
    flowLog_deliverLogsPermissionArn,
    flowLog_deliverLogsErrorMessage,
    flowLog_tags,
    flowLog_logDestinationType,
    flowLog_flowLogStatus,

    -- ** FpgaDeviceInfo
    fpgaDeviceInfo_memoryInfo,
    fpgaDeviceInfo_manufacturer,
    fpgaDeviceInfo_name,
    fpgaDeviceInfo_count,

    -- ** FpgaDeviceMemoryInfo
    fpgaDeviceMemoryInfo_sizeInMiB,

    -- ** FpgaImage
    fpgaImage_ownerAlias,
    fpgaImage_shellVersion,
    fpgaImage_ownerId,
    fpgaImage_fpgaImageGlobalId,
    fpgaImage_dataRetentionSupport,
    fpgaImage_productCodes,
    fpgaImage_state,
    fpgaImage_pciId,
    fpgaImage_name,
    fpgaImage_updateTime,
    fpgaImage_tags,
    fpgaImage_public,
    fpgaImage_createTime,
    fpgaImage_description,
    fpgaImage_fpgaImageId,

    -- ** FpgaImageAttribute
    fpgaImageAttribute_productCodes,
    fpgaImageAttribute_name,
    fpgaImageAttribute_loadPermissions,
    fpgaImageAttribute_description,
    fpgaImageAttribute_fpgaImageId,

    -- ** FpgaImageState
    fpgaImageState_message,
    fpgaImageState_code,

    -- ** FpgaInfo
    fpgaInfo_totalFpgaMemoryInMiB,
    fpgaInfo_fpgas,

    -- ** GpuDeviceInfo
    gpuDeviceInfo_memoryInfo,
    gpuDeviceInfo_manufacturer,
    gpuDeviceInfo_name,
    gpuDeviceInfo_count,

    -- ** GpuDeviceMemoryInfo
    gpuDeviceMemoryInfo_sizeInMiB,

    -- ** GpuInfo
    gpuInfo_gpus,
    gpuInfo_totalGpuMemoryInMiB,

    -- ** GroupIdentifier
    groupIdentifier_groupName,
    groupIdentifier_groupId,

    -- ** HibernationOptions
    hibernationOptions_configured,

    -- ** HibernationOptionsRequest
    hibernationOptionsRequest_configured,

    -- ** HistoryRecord
    historyRecord_eventType,
    historyRecord_timestamp,
    historyRecord_eventInformation,

    -- ** HistoryRecordEntry
    historyRecordEntry_eventType,
    historyRecordEntry_timestamp,
    historyRecordEntry_eventInformation,

    -- ** Host
    host_ownerId,
    host_hostProperties,
    host_availableCapacity,
    host_hostReservationId,
    host_allocationTime,
    host_instances,
    host_availabilityZoneId,
    host_state,
    host_availabilityZone,
    host_releaseTime,
    host_tags,
    host_autoPlacement,
    host_hostRecovery,
    host_memberOfServiceLinkedResourceGroup,
    host_allowsMultipleInstanceTypes,
    host_hostId,
    host_clientToken,

    -- ** HostInstance
    hostInstance_ownerId,
    hostInstance_instanceId,
    hostInstance_instanceType,

    -- ** HostOffering
    hostOffering_instanceFamily,
    hostOffering_upfrontPrice,
    hostOffering_paymentOption,
    hostOffering_duration,
    hostOffering_offeringId,
    hostOffering_currencyCode,
    hostOffering_hourlyPrice,

    -- ** HostProperties
    hostProperties_instanceFamily,
    hostProperties_totalVCpus,
    hostProperties_instanceType,
    hostProperties_cores,
    hostProperties_sockets,

    -- ** HostReservation
    hostReservation_instanceFamily,
    hostReservation_end,
    hostReservation_hostIdSet,
    hostReservation_upfrontPrice,
    hostReservation_paymentOption,
    hostReservation_duration,
    hostReservation_offeringId,
    hostReservation_hostReservationId,
    hostReservation_currencyCode,
    hostReservation_state,
    hostReservation_tags,
    hostReservation_count,
    hostReservation_hourlyPrice,
    hostReservation_start,

    -- ** IKEVersionsListValue
    iKEVersionsListValue_value,

    -- ** IKEVersionsRequestListValue
    iKEVersionsRequestListValue_value,

    -- ** IamInstanceProfile
    iamInstanceProfile_arn,
    iamInstanceProfile_id,

    -- ** IamInstanceProfileAssociation
    iamInstanceProfileAssociation_instanceId,
    iamInstanceProfileAssociation_iamInstanceProfile,
    iamInstanceProfileAssociation_state,
    iamInstanceProfileAssociation_associationId,
    iamInstanceProfileAssociation_timestamp,

    -- ** IamInstanceProfileSpecification
    iamInstanceProfileSpecification_arn,
    iamInstanceProfileSpecification_name,

    -- ** IcmpTypeCode
    icmpTypeCode_code,
    icmpTypeCode_type,

    -- ** IdFormat
    idFormat_useLongIds,
    idFormat_resource,
    idFormat_deadline,

    -- ** Image
    image_platform,
    image_rootDeviceName,
    image_ramdiskId,
    image_stateReason,
    image_usageOperation,
    image_creationDate,
    image_productCodes,
    image_platformDetails,
    image_name,
    image_tags,
    image_sriovNetSupport,
    image_blockDeviceMappings,
    image_kernelId,
    image_description,
    image_imageOwnerAlias,
    image_enaSupport,
    image_imageId,
    image_imageLocation,
    image_state,
    image_ownerId,
    image_public,
    image_architecture,
    image_imageType,
    image_rootDeviceType,
    image_virtualizationType,
    image_hypervisor,

    -- ** ImageDiskContainer
    imageDiskContainer_format,
    imageDiskContainer_userBucket,
    imageDiskContainer_deviceName,
    imageDiskContainer_snapshotId,
    imageDiskContainer_description,
    imageDiskContainer_url,

    -- ** ImportImageLicenseConfigurationRequest
    importImageLicenseConfigurationRequest_licenseConfigurationArn,

    -- ** ImportImageLicenseConfigurationResponse
    importImageLicenseConfigurationResponse_licenseConfigurationArn,

    -- ** ImportImageTask
    importImageTask_hypervisor,
    importImageTask_platform,
    importImageTask_statusMessage,
    importImageTask_status,
    importImageTask_snapshotDetails,
    importImageTask_encrypted,
    importImageTask_importTaskId,
    importImageTask_licenseSpecifications,
    importImageTask_architecture,
    importImageTask_imageId,
    importImageTask_kmsKeyId,
    importImageTask_tags,
    importImageTask_description,
    importImageTask_licenseType,
    importImageTask_progress,

    -- ** ImportInstanceLaunchSpecification
    importInstanceLaunchSpecification_additionalInfo,
    importInstanceLaunchSpecification_instanceType,
    importInstanceLaunchSpecification_userData,
    importInstanceLaunchSpecification_placement,
    importInstanceLaunchSpecification_groupIds,
    importInstanceLaunchSpecification_groupNames,
    importInstanceLaunchSpecification_architecture,
    importInstanceLaunchSpecification_instanceInitiatedShutdownBehavior,
    importInstanceLaunchSpecification_monitoring,
    importInstanceLaunchSpecification_subnetId,
    importInstanceLaunchSpecification_privateIpAddress,

    -- ** ImportInstanceTaskDetails
    importInstanceTaskDetails_platform,
    importInstanceTaskDetails_instanceId,
    importInstanceTaskDetails_volumes,
    importInstanceTaskDetails_description,

    -- ** ImportInstanceVolumeDetailItem
    importInstanceVolumeDetailItem_statusMessage,
    importInstanceVolumeDetailItem_bytesConverted,
    importInstanceVolumeDetailItem_status,
    importInstanceVolumeDetailItem_volume,
    importInstanceVolumeDetailItem_image,
    importInstanceVolumeDetailItem_availabilityZone,
    importInstanceVolumeDetailItem_description,

    -- ** ImportSnapshotTask
    importSnapshotTask_snapshotTaskDetail,
    importSnapshotTask_importTaskId,
    importSnapshotTask_tags,
    importSnapshotTask_description,

    -- ** ImportVolumeTaskDetails
    importVolumeTaskDetails_bytesConverted,
    importVolumeTaskDetails_volume,
    importVolumeTaskDetails_image,
    importVolumeTaskDetails_availabilityZone,
    importVolumeTaskDetails_description,

    -- ** InferenceAcceleratorInfo
    inferenceAcceleratorInfo_accelerators,

    -- ** InferenceDeviceInfo
    inferenceDeviceInfo_manufacturer,
    inferenceDeviceInfo_name,
    inferenceDeviceInfo_count,

    -- ** Instance
    instance_platform,
    instance_instanceLifecycle,
    instance_stateTransitionReason,
    instance_rootDeviceName,
    instance_capacityReservationSpecification,
    instance_ebsOptimized,
    instance_ramdiskId,
    instance_elasticInferenceAcceleratorAssociations,
    instance_stateReason,
    instance_outpostArn,
    instance_sourceDestCheck,
    instance_productCodes,
    instance_securityGroups,
    instance_iamInstanceProfile,
    instance_publicDnsName,
    instance_hibernationOptions,
    instance_capacityReservationId,
    instance_tags,
    instance_sriovNetSupport,
    instance_blockDeviceMappings,
    instance_publicIpAddress,
    instance_subnetId,
    instance_enclaveOptions,
    instance_kernelId,
    instance_cpuOptions,
    instance_privateDnsName,
    instance_keyName,
    instance_networkInterfaces,
    instance_licenses,
    instance_vpcId,
    instance_elasticGpuAssociations,
    instance_metadataOptions,
    instance_enaSupport,
    instance_spotInstanceRequestId,
    instance_clientToken,
    instance_privateIpAddress,
    instance_instanceId,
    instance_imageId,
    instance_amiLaunchIndex,
    instance_instanceType,
    instance_launchTime,
    instance_placement,
    instance_monitoring,
    instance_architecture,
    instance_rootDeviceType,
    instance_virtualizationType,
    instance_hypervisor,
    instance_state,

    -- ** InstanceBlockDeviceMapping
    instanceBlockDeviceMapping_ebs,
    instanceBlockDeviceMapping_deviceName,

    -- ** InstanceBlockDeviceMappingSpecification
    instanceBlockDeviceMappingSpecification_ebs,
    instanceBlockDeviceMappingSpecification_noDevice,
    instanceBlockDeviceMappingSpecification_virtualName,
    instanceBlockDeviceMappingSpecification_deviceName,

    -- ** InstanceCapacity
    instanceCapacity_instanceType,
    instanceCapacity_availableCapacity,
    instanceCapacity_totalCapacity,

    -- ** InstanceCount
    instanceCount_state,
    instanceCount_instanceCount,

    -- ** InstanceCreditSpecification
    instanceCreditSpecification_instanceId,
    instanceCreditSpecification_cpuCredits,

    -- ** InstanceCreditSpecificationRequest
    instanceCreditSpecificationRequest_instanceId,
    instanceCreditSpecificationRequest_cpuCredits,

    -- ** InstanceExportDetails
    instanceExportDetails_instanceId,
    instanceExportDetails_targetEnvironment,

    -- ** InstanceFamilyCreditSpecification
    instanceFamilyCreditSpecification_instanceFamily,
    instanceFamilyCreditSpecification_cpuCredits,

    -- ** InstanceIpv6Address
    instanceIpv6Address_ipv6Address,

    -- ** InstanceIpv6AddressRequest
    instanceIpv6AddressRequest_ipv6Address,

    -- ** InstanceMarketOptionsRequest
    instanceMarketOptionsRequest_marketType,
    instanceMarketOptionsRequest_spotOptions,

    -- ** InstanceMetadataOptionsRequest
    instanceMetadataOptionsRequest_httpEndpoint,
    instanceMetadataOptionsRequest_httpPutResponseHopLimit,
    instanceMetadataOptionsRequest_httpTokens,

    -- ** InstanceMetadataOptionsResponse
    instanceMetadataOptionsResponse_httpEndpoint,
    instanceMetadataOptionsResponse_httpPutResponseHopLimit,
    instanceMetadataOptionsResponse_state,
    instanceMetadataOptionsResponse_httpTokens,

    -- ** InstanceMonitoring
    instanceMonitoring_instanceId,
    instanceMonitoring_monitoring,

    -- ** InstanceNetworkInterface
    instanceNetworkInterface_groups,
    instanceNetworkInterface_status,
    instanceNetworkInterface_ownerId,
    instanceNetworkInterface_privateIpAddresses,
    instanceNetworkInterface_attachment,
    instanceNetworkInterface_macAddress,
    instanceNetworkInterface_association,
    instanceNetworkInterface_ipv6Addresses,
    instanceNetworkInterface_interfaceType,
    instanceNetworkInterface_sourceDestCheck,
    instanceNetworkInterface_networkInterfaceId,
    instanceNetworkInterface_subnetId,
    instanceNetworkInterface_description,
    instanceNetworkInterface_privateDnsName,
    instanceNetworkInterface_vpcId,
    instanceNetworkInterface_privateIpAddress,

    -- ** InstanceNetworkInterfaceAssociation
    instanceNetworkInterfaceAssociation_ipOwnerId,
    instanceNetworkInterfaceAssociation_carrierIp,
    instanceNetworkInterfaceAssociation_publicDnsName,
    instanceNetworkInterfaceAssociation_publicIp,

    -- ** InstanceNetworkInterfaceAttachment
    instanceNetworkInterfaceAttachment_status,
    instanceNetworkInterfaceAttachment_attachTime,
    instanceNetworkInterfaceAttachment_attachmentId,
    instanceNetworkInterfaceAttachment_networkCardIndex,
    instanceNetworkInterfaceAttachment_deleteOnTermination,
    instanceNetworkInterfaceAttachment_deviceIndex,

    -- ** InstanceNetworkInterfaceSpecification
    instanceNetworkInterfaceSpecification_groups,
    instanceNetworkInterfaceSpecification_privateIpAddresses,
    instanceNetworkInterfaceSpecification_ipv6Addresses,
    instanceNetworkInterfaceSpecification_interfaceType,
    instanceNetworkInterfaceSpecification_associatePublicIpAddress,
    instanceNetworkInterfaceSpecification_associateCarrierIpAddress,
    instanceNetworkInterfaceSpecification_ipv6AddressCount,
    instanceNetworkInterfaceSpecification_networkCardIndex,
    instanceNetworkInterfaceSpecification_deleteOnTermination,
    instanceNetworkInterfaceSpecification_networkInterfaceId,
    instanceNetworkInterfaceSpecification_subnetId,
    instanceNetworkInterfaceSpecification_description,
    instanceNetworkInterfaceSpecification_deviceIndex,
    instanceNetworkInterfaceSpecification_secondaryPrivateIpAddressCount,
    instanceNetworkInterfaceSpecification_privateIpAddress,

    -- ** InstancePrivateIpAddress
    instancePrivateIpAddress_primary,
    instancePrivateIpAddress_association,
    instancePrivateIpAddress_privateDnsName,
    instancePrivateIpAddress_privateIpAddress,

    -- ** InstanceSpecification
    instanceSpecification_instanceId,
    instanceSpecification_excludeBootVolume,

    -- ** InstanceState
    instanceState_name,
    instanceState_code,

    -- ** InstanceStateChange
    instanceStateChange_instanceId,
    instanceStateChange_currentState,
    instanceStateChange_previousState,

    -- ** InstanceStatus
    instanceStatus_instanceId,
    instanceStatus_systemStatus,
    instanceStatus_outpostArn,
    instanceStatus_instanceStatus,
    instanceStatus_events,
    instanceStatus_availabilityZone,
    instanceStatus_instanceState,

    -- ** InstanceStatusDetails
    instanceStatusDetails_status,
    instanceStatusDetails_impairedSince,
    instanceStatusDetails_name,

    -- ** InstanceStatusEvent
    instanceStatusEvent_notBefore,
    instanceStatusEvent_instanceEventId,
    instanceStatusEvent_code,
    instanceStatusEvent_notAfter,
    instanceStatusEvent_notBeforeDeadline,
    instanceStatusEvent_description,

    -- ** InstanceStatusSummary
    instanceStatusSummary_details,
    instanceStatusSummary_status,

    -- ** InstanceStorageInfo
    instanceStorageInfo_nvmeSupport,
    instanceStorageInfo_totalSizeInGB,
    instanceStorageInfo_disks,

    -- ** InstanceTagNotificationAttribute
    instanceTagNotificationAttribute_instanceTagKeys,
    instanceTagNotificationAttribute_includeAllTagsOfInstance,

    -- ** InstanceTypeInfo
    instanceTypeInfo_memoryInfo,
    instanceTypeInfo_hypervisor,
    instanceTypeInfo_gpuInfo,
    instanceTypeInfo_instanceType,
    instanceTypeInfo_burstablePerformanceSupported,
    instanceTypeInfo_vCpuInfo,
    instanceTypeInfo_supportedRootDeviceTypes,
    instanceTypeInfo_placementGroupInfo,
    instanceTypeInfo_networkInfo,
    instanceTypeInfo_processorInfo,
    instanceTypeInfo_ebsInfo,
    instanceTypeInfo_autoRecoverySupported,
    instanceTypeInfo_currentGeneration,
    instanceTypeInfo_hibernationSupported,
    instanceTypeInfo_dedicatedHostsSupported,
    instanceTypeInfo_instanceStorageSupported,
    instanceTypeInfo_instanceStorageInfo,
    instanceTypeInfo_fpgaInfo,
    instanceTypeInfo_supportedUsageClasses,
    instanceTypeInfo_freeTierEligible,
    instanceTypeInfo_bareMetal,
    instanceTypeInfo_inferenceAcceleratorInfo,
    instanceTypeInfo_supportedVirtualizationTypes,

    -- ** InstanceTypeOffering
    instanceTypeOffering_instanceType,
    instanceTypeOffering_locationType,
    instanceTypeOffering_location,

    -- ** InstanceUsage
    instanceUsage_accountId,
    instanceUsage_usedInstanceCount,

    -- ** InternetGateway
    internetGateway_ownerId,
    internetGateway_tags,
    internetGateway_attachments,
    internetGateway_internetGatewayId,

    -- ** InternetGatewayAttachment
    internetGatewayAttachment_state,
    internetGatewayAttachment_vpcId,

    -- ** IpPermission
    ipPermission_fromPort,
    ipPermission_prefixListIds,
    ipPermission_ipRanges,
    ipPermission_ipv6Ranges,
    ipPermission_userIdGroupPairs,
    ipPermission_toPort,
    ipPermission_ipProtocol,

    -- ** IpRange
    ipRange_description,
    ipRange_cidrIp,

    -- ** Ipv6CidrAssociation
    ipv6CidrAssociation_ipv6Cidr,
    ipv6CidrAssociation_associatedResource,

    -- ** Ipv6CidrBlock
    ipv6CidrBlock_ipv6CidrBlock,

    -- ** Ipv6Pool
    ipv6Pool_poolId,
    ipv6Pool_poolCidrBlocks,
    ipv6Pool_tags,
    ipv6Pool_description,

    -- ** Ipv6Range
    ipv6Range_cidrIpv6,
    ipv6Range_description,

    -- ** KeyPairInfo
    keyPairInfo_keyFingerprint,
    keyPairInfo_keyPairId,
    keyPairInfo_tags,
    keyPairInfo_keyName,

    -- ** LastError
    lastError_message,
    lastError_code,

    -- ** LaunchPermission
    launchPermission_group,
    launchPermission_userId,

    -- ** LaunchPermissionModifications
    launchPermissionModifications_add,
    launchPermissionModifications_remove,

    -- ** LaunchSpecification
    launchSpecification_instanceType,
    launchSpecification_ebsOptimized,
    launchSpecification_userData,
    launchSpecification_placement,
    launchSpecification_addressingType,
    launchSpecification_ramdiskId,
    launchSpecification_imageId,
    launchSpecification_securityGroups,
    launchSpecification_iamInstanceProfile,
    launchSpecification_monitoring,
    launchSpecification_blockDeviceMappings,
    launchSpecification_subnetId,
    launchSpecification_kernelId,
    launchSpecification_keyName,
    launchSpecification_networkInterfaces,

    -- ** LaunchTemplate
    launchTemplate_launchTemplateId,
    launchTemplate_launchTemplateName,
    launchTemplate_tags,
    launchTemplate_createTime,
    launchTemplate_createdBy,
    launchTemplate_defaultVersionNumber,
    launchTemplate_latestVersionNumber,

    -- ** LaunchTemplateAndOverridesResponse
    launchTemplateAndOverridesResponse_launchTemplateSpecification,
    launchTemplateAndOverridesResponse_overrides,

    -- ** LaunchTemplateBlockDeviceMapping
    launchTemplateBlockDeviceMapping_ebs,
    launchTemplateBlockDeviceMapping_noDevice,
    launchTemplateBlockDeviceMapping_virtualName,
    launchTemplateBlockDeviceMapping_deviceName,

    -- ** LaunchTemplateBlockDeviceMappingRequest
    launchTemplateBlockDeviceMappingRequest_ebs,
    launchTemplateBlockDeviceMappingRequest_noDevice,
    launchTemplateBlockDeviceMappingRequest_virtualName,
    launchTemplateBlockDeviceMappingRequest_deviceName,

    -- ** LaunchTemplateCapacityReservationSpecificationRequest
    launchTemplateCapacityReservationSpecificationRequest_capacityReservationPreference,
    launchTemplateCapacityReservationSpecificationRequest_capacityReservationTarget,

    -- ** LaunchTemplateCapacityReservationSpecificationResponse
    launchTemplateCapacityReservationSpecificationResponse_capacityReservationPreference,
    launchTemplateCapacityReservationSpecificationResponse_capacityReservationTarget,

    -- ** LaunchTemplateConfig
    launchTemplateConfig_launchTemplateSpecification,
    launchTemplateConfig_overrides,

    -- ** LaunchTemplateCpuOptions
    launchTemplateCpuOptions_threadsPerCore,
    launchTemplateCpuOptions_coreCount,

    -- ** LaunchTemplateCpuOptionsRequest
    launchTemplateCpuOptionsRequest_threadsPerCore,
    launchTemplateCpuOptionsRequest_coreCount,

    -- ** LaunchTemplateEbsBlockDevice
    launchTemplateEbsBlockDevice_encrypted,
    launchTemplateEbsBlockDevice_throughput,
    launchTemplateEbsBlockDevice_kmsKeyId,
    launchTemplateEbsBlockDevice_deleteOnTermination,
    launchTemplateEbsBlockDevice_snapshotId,
    launchTemplateEbsBlockDevice_volumeType,
    launchTemplateEbsBlockDevice_volumeSize,
    launchTemplateEbsBlockDevice_iops,

    -- ** LaunchTemplateEbsBlockDeviceRequest
    launchTemplateEbsBlockDeviceRequest_encrypted,
    launchTemplateEbsBlockDeviceRequest_throughput,
    launchTemplateEbsBlockDeviceRequest_kmsKeyId,
    launchTemplateEbsBlockDeviceRequest_deleteOnTermination,
    launchTemplateEbsBlockDeviceRequest_snapshotId,
    launchTemplateEbsBlockDeviceRequest_volumeType,
    launchTemplateEbsBlockDeviceRequest_volumeSize,
    launchTemplateEbsBlockDeviceRequest_iops,

    -- ** LaunchTemplateElasticInferenceAccelerator
    launchTemplateElasticInferenceAccelerator_count,
    launchTemplateElasticInferenceAccelerator_type,

    -- ** LaunchTemplateElasticInferenceAcceleratorResponse
    launchTemplateElasticInferenceAcceleratorResponse_count,
    launchTemplateElasticInferenceAcceleratorResponse_type,

    -- ** LaunchTemplateEnclaveOptions
    launchTemplateEnclaveOptions_enabled,

    -- ** LaunchTemplateEnclaveOptionsRequest
    launchTemplateEnclaveOptionsRequest_enabled,

    -- ** LaunchTemplateHibernationOptions
    launchTemplateHibernationOptions_configured,

    -- ** LaunchTemplateHibernationOptionsRequest
    launchTemplateHibernationOptionsRequest_configured,

    -- ** LaunchTemplateIamInstanceProfileSpecification
    launchTemplateIamInstanceProfileSpecification_arn,
    launchTemplateIamInstanceProfileSpecification_name,

    -- ** LaunchTemplateIamInstanceProfileSpecificationRequest
    launchTemplateIamInstanceProfileSpecificationRequest_arn,
    launchTemplateIamInstanceProfileSpecificationRequest_name,

    -- ** LaunchTemplateInstanceMarketOptions
    launchTemplateInstanceMarketOptions_marketType,
    launchTemplateInstanceMarketOptions_spotOptions,

    -- ** LaunchTemplateInstanceMarketOptionsRequest
    launchTemplateInstanceMarketOptionsRequest_marketType,
    launchTemplateInstanceMarketOptionsRequest_spotOptions,

    -- ** LaunchTemplateInstanceMetadataOptions
    launchTemplateInstanceMetadataOptions_httpEndpoint,
    launchTemplateInstanceMetadataOptions_httpPutResponseHopLimit,
    launchTemplateInstanceMetadataOptions_state,
    launchTemplateInstanceMetadataOptions_httpTokens,

    -- ** LaunchTemplateInstanceMetadataOptionsRequest
    launchTemplateInstanceMetadataOptionsRequest_httpEndpoint,
    launchTemplateInstanceMetadataOptionsRequest_httpPutResponseHopLimit,
    launchTemplateInstanceMetadataOptionsRequest_httpTokens,

    -- ** LaunchTemplateInstanceNetworkInterfaceSpecification
    launchTemplateInstanceNetworkInterfaceSpecification_groups,
    launchTemplateInstanceNetworkInterfaceSpecification_privateIpAddresses,
    launchTemplateInstanceNetworkInterfaceSpecification_ipv6Addresses,
    launchTemplateInstanceNetworkInterfaceSpecification_interfaceType,
    launchTemplateInstanceNetworkInterfaceSpecification_associatePublicIpAddress,
    launchTemplateInstanceNetworkInterfaceSpecification_associateCarrierIpAddress,
    launchTemplateInstanceNetworkInterfaceSpecification_ipv6AddressCount,
    launchTemplateInstanceNetworkInterfaceSpecification_networkCardIndex,
    launchTemplateInstanceNetworkInterfaceSpecification_deleteOnTermination,
    launchTemplateInstanceNetworkInterfaceSpecification_networkInterfaceId,
    launchTemplateInstanceNetworkInterfaceSpecification_subnetId,
    launchTemplateInstanceNetworkInterfaceSpecification_description,
    launchTemplateInstanceNetworkInterfaceSpecification_deviceIndex,
    launchTemplateInstanceNetworkInterfaceSpecification_secondaryPrivateIpAddressCount,
    launchTemplateInstanceNetworkInterfaceSpecification_privateIpAddress,

    -- ** LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_groups,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_privateIpAddresses,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6Addresses,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_interfaceType,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_associatePublicIpAddress,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_associateCarrierIpAddress,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6AddressCount,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_networkCardIndex,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_deleteOnTermination,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_networkInterfaceId,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_subnetId,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_description,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_deviceIndex,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_secondaryPrivateIpAddressCount,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_privateIpAddress,

    -- ** LaunchTemplateLicenseConfiguration
    launchTemplateLicenseConfiguration_licenseConfigurationArn,

    -- ** LaunchTemplateLicenseConfigurationRequest
    launchTemplateLicenseConfigurationRequest_licenseConfigurationArn,

    -- ** LaunchTemplateOverrides
    launchTemplateOverrides_instanceType,
    launchTemplateOverrides_spotPrice,
    launchTemplateOverrides_priority,
    launchTemplateOverrides_availabilityZone,
    launchTemplateOverrides_subnetId,
    launchTemplateOverrides_weightedCapacity,

    -- ** LaunchTemplatePlacement
    launchTemplatePlacement_spreadDomain,
    launchTemplatePlacement_groupName,
    launchTemplatePlacement_tenancy,
    launchTemplatePlacement_affinity,
    launchTemplatePlacement_availabilityZone,
    launchTemplatePlacement_partitionNumber,
    launchTemplatePlacement_hostResourceGroupArn,
    launchTemplatePlacement_hostId,

    -- ** LaunchTemplatePlacementRequest
    launchTemplatePlacementRequest_spreadDomain,
    launchTemplatePlacementRequest_groupName,
    launchTemplatePlacementRequest_tenancy,
    launchTemplatePlacementRequest_affinity,
    launchTemplatePlacementRequest_availabilityZone,
    launchTemplatePlacementRequest_partitionNumber,
    launchTemplatePlacementRequest_hostResourceGroupArn,
    launchTemplatePlacementRequest_hostId,

    -- ** LaunchTemplateSpecification
    launchTemplateSpecification_launchTemplateId,
    launchTemplateSpecification_launchTemplateName,
    launchTemplateSpecification_version,

    -- ** LaunchTemplateSpotMarketOptions
    launchTemplateSpotMarketOptions_blockDurationMinutes,
    launchTemplateSpotMarketOptions_instanceInterruptionBehavior,
    launchTemplateSpotMarketOptions_validUntil,
    launchTemplateSpotMarketOptions_spotInstanceType,
    launchTemplateSpotMarketOptions_maxPrice,

    -- ** LaunchTemplateSpotMarketOptionsRequest
    launchTemplateSpotMarketOptionsRequest_blockDurationMinutes,
    launchTemplateSpotMarketOptionsRequest_instanceInterruptionBehavior,
    launchTemplateSpotMarketOptionsRequest_validUntil,
    launchTemplateSpotMarketOptionsRequest_spotInstanceType,
    launchTemplateSpotMarketOptionsRequest_maxPrice,

    -- ** LaunchTemplateTagSpecification
    launchTemplateTagSpecification_resourceType,
    launchTemplateTagSpecification_tags,

    -- ** LaunchTemplateTagSpecificationRequest
    launchTemplateTagSpecificationRequest_resourceType,
    launchTemplateTagSpecificationRequest_tags,

    -- ** LaunchTemplateVersion
    launchTemplateVersion_defaultVersion,
    launchTemplateVersion_launchTemplateId,
    launchTemplateVersion_launchTemplateData,
    launchTemplateVersion_launchTemplateName,
    launchTemplateVersion_versionDescription,
    launchTemplateVersion_versionNumber,
    launchTemplateVersion_createTime,
    launchTemplateVersion_createdBy,

    -- ** LaunchTemplatesMonitoring
    launchTemplatesMonitoring_enabled,

    -- ** LaunchTemplatesMonitoringRequest
    launchTemplatesMonitoringRequest_enabled,

    -- ** LicenseConfiguration
    licenseConfiguration_licenseConfigurationArn,

    -- ** LicenseConfigurationRequest
    licenseConfigurationRequest_licenseConfigurationArn,

    -- ** LoadBalancersConfig
    loadBalancersConfig_classicLoadBalancersConfig,
    loadBalancersConfig_targetGroupsConfig,

    -- ** LoadPermission
    loadPermission_group,
    loadPermission_userId,

    -- ** LoadPermissionModifications
    loadPermissionModifications_add,
    loadPermissionModifications_remove,

    -- ** LoadPermissionRequest
    loadPermissionRequest_group,
    loadPermissionRequest_userId,

    -- ** LocalGateway
    localGateway_ownerId,
    localGateway_outpostArn,
    localGateway_localGatewayId,
    localGateway_state,
    localGateway_tags,

    -- ** LocalGatewayRoute
    localGatewayRoute_ownerId,
    localGatewayRoute_localGatewayVirtualInterfaceGroupId,
    localGatewayRoute_localGatewayRouteTableArn,
    localGatewayRoute_state,
    localGatewayRoute_localGatewayRouteTableId,
    localGatewayRoute_destinationCidrBlock,
    localGatewayRoute_type,

    -- ** LocalGatewayRouteTable
    localGatewayRouteTable_ownerId,
    localGatewayRouteTable_outpostArn,
    localGatewayRouteTable_localGatewayId,
    localGatewayRouteTable_localGatewayRouteTableArn,
    localGatewayRouteTable_state,
    localGatewayRouteTable_localGatewayRouteTableId,
    localGatewayRouteTable_tags,

    -- ** LocalGatewayRouteTableVirtualInterfaceGroupAssociation
    localGatewayRouteTableVirtualInterfaceGroupAssociation_ownerId,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayVirtualInterfaceGroupId,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayId,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableVirtualInterfaceGroupAssociationId,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableArn,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_state,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableId,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_tags,

    -- ** LocalGatewayRouteTableVpcAssociation
    localGatewayRouteTableVpcAssociation_ownerId,
    localGatewayRouteTableVpcAssociation_localGatewayRouteTableVpcAssociationId,
    localGatewayRouteTableVpcAssociation_localGatewayId,
    localGatewayRouteTableVpcAssociation_localGatewayRouteTableArn,
    localGatewayRouteTableVpcAssociation_state,
    localGatewayRouteTableVpcAssociation_localGatewayRouteTableId,
    localGatewayRouteTableVpcAssociation_tags,
    localGatewayRouteTableVpcAssociation_vpcId,

    -- ** LocalGatewayVirtualInterface
    localGatewayVirtualInterface_peerAddress,
    localGatewayVirtualInterface_ownerId,
    localGatewayVirtualInterface_localGatewayVirtualInterfaceId,
    localGatewayVirtualInterface_peerBgpAsn,
    localGatewayVirtualInterface_localAddress,
    localGatewayVirtualInterface_localGatewayId,
    localGatewayVirtualInterface_localBgpAsn,
    localGatewayVirtualInterface_tags,
    localGatewayVirtualInterface_vlan,

    -- ** LocalGatewayVirtualInterfaceGroup
    localGatewayVirtualInterfaceGroup_localGatewayVirtualInterfaceIds,
    localGatewayVirtualInterfaceGroup_ownerId,
    localGatewayVirtualInterfaceGroup_localGatewayVirtualInterfaceGroupId,
    localGatewayVirtualInterfaceGroup_localGatewayId,
    localGatewayVirtualInterfaceGroup_tags,

    -- ** ManagedPrefixList
    managedPrefixList_stateMessage,
    managedPrefixList_ownerId,
    managedPrefixList_maxEntries,
    managedPrefixList_prefixListName,
    managedPrefixList_version,
    managedPrefixList_prefixListId,
    managedPrefixList_prefixListArn,
    managedPrefixList_state,
    managedPrefixList_tags,
    managedPrefixList_addressFamily,

    -- ** MemoryInfo
    memoryInfo_sizeInMiB,

    -- ** ModifyTransitGatewayOptions
    modifyTransitGatewayOptions_removeTransitGatewayCidrBlocks,
    modifyTransitGatewayOptions_propagationDefaultRouteTableId,
    modifyTransitGatewayOptions_vpnEcmpSupport,
    modifyTransitGatewayOptions_dnsSupport,
    modifyTransitGatewayOptions_addTransitGatewayCidrBlocks,
    modifyTransitGatewayOptions_associationDefaultRouteTableId,
    modifyTransitGatewayOptions_autoAcceptSharedAttachments,
    modifyTransitGatewayOptions_defaultRouteTableAssociation,
    modifyTransitGatewayOptions_defaultRouteTablePropagation,

    -- ** ModifyTransitGatewayVpcAttachmentRequestOptions
    modifyTransitGatewayVpcAttachmentRequestOptions_applianceModeSupport,
    modifyTransitGatewayVpcAttachmentRequestOptions_dnsSupport,
    modifyTransitGatewayVpcAttachmentRequestOptions_ipv6Support,

    -- ** ModifyVpnTunnelOptionsSpecification
    modifyVpnTunnelOptionsSpecification_phase1LifetimeSeconds,
    modifyVpnTunnelOptionsSpecification_phase2DHGroupNumbers,
    modifyVpnTunnelOptionsSpecification_iKEVersions,
    modifyVpnTunnelOptionsSpecification_phase2EncryptionAlgorithms,
    modifyVpnTunnelOptionsSpecification_phase2IntegrityAlgorithms,
    modifyVpnTunnelOptionsSpecification_startupAction,
    modifyVpnTunnelOptionsSpecification_dPDTimeoutSeconds,
    modifyVpnTunnelOptionsSpecification_phase1DHGroupNumbers,
    modifyVpnTunnelOptionsSpecification_phase1EncryptionAlgorithms,
    modifyVpnTunnelOptionsSpecification_replayWindowSize,
    modifyVpnTunnelOptionsSpecification_preSharedKey,
    modifyVpnTunnelOptionsSpecification_tunnelInsideIpv6Cidr,
    modifyVpnTunnelOptionsSpecification_rekeyFuzzPercentage,
    modifyVpnTunnelOptionsSpecification_rekeyMarginTimeSeconds,
    modifyVpnTunnelOptionsSpecification_tunnelInsideCidr,
    modifyVpnTunnelOptionsSpecification_phase1IntegrityAlgorithms,
    modifyVpnTunnelOptionsSpecification_dPDTimeoutAction,
    modifyVpnTunnelOptionsSpecification_phase2LifetimeSeconds,

    -- ** Monitoring
    monitoring_state,

    -- ** MovingAddressStatus
    movingAddressStatus_moveStatus,
    movingAddressStatus_publicIp,

    -- ** NatGateway
    natGateway_natGatewayAddresses,
    natGateway_failureMessage,
    natGateway_failureCode,
    natGateway_provisionedBandwidth,
    natGateway_state,
    natGateway_deleteTime,
    natGateway_tags,
    natGateway_createTime,
    natGateway_subnetId,
    natGateway_natGatewayId,
    natGateway_vpcId,

    -- ** NatGatewayAddress
    natGatewayAddress_privateIp,
    natGatewayAddress_networkInterfaceId,
    natGatewayAddress_publicIp,
    natGatewayAddress_allocationId,

    -- ** NetworkAcl
    networkAcl_ownerId,
    networkAcl_isDefault,
    networkAcl_tags,
    networkAcl_vpcId,
    networkAcl_networkAclId,
    networkAcl_associations,
    networkAcl_entries,

    -- ** NetworkAclAssociation
    networkAclAssociation_networkAclAssociationId,
    networkAclAssociation_subnetId,
    networkAclAssociation_networkAclId,

    -- ** NetworkAclEntry
    networkAclEntry_portRange,
    networkAclEntry_ruleAction,
    networkAclEntry_egress,
    networkAclEntry_icmpTypeCode,
    networkAclEntry_ipv6CidrBlock,
    networkAclEntry_protocol,
    networkAclEntry_ruleNumber,
    networkAclEntry_cidrBlock,

    -- ** NetworkCardInfo
    networkCardInfo_maximumNetworkInterfaces,
    networkCardInfo_networkCardIndex,
    networkCardInfo_networkPerformance,

    -- ** NetworkInfo
    networkInfo_defaultNetworkCardIndex,
    networkInfo_efaSupported,
    networkInfo_networkCards,
    networkInfo_ipv4AddressesPerInterface,
    networkInfo_maximumNetworkInterfaces,
    networkInfo_ipv6Supported,
    networkInfo_maximumNetworkCards,
    networkInfo_networkPerformance,
    networkInfo_ipv6AddressesPerInterface,
    networkInfo_enaSupport,

    -- ** NetworkInsightsAnalysis
    networkInsightsAnalysis_statusMessage,
    networkInsightsAnalysis_status,
    networkInsightsAnalysis_networkInsightsAnalysisId,
    networkInsightsAnalysis_startDate,
    networkInsightsAnalysis_filterInArns,
    networkInsightsAnalysis_returnPathComponents,
    networkInsightsAnalysis_explanations,
    networkInsightsAnalysis_networkPathFound,
    networkInsightsAnalysis_networkInsightsAnalysisArn,
    networkInsightsAnalysis_tags,
    networkInsightsAnalysis_networkInsightsPathId,
    networkInsightsAnalysis_alternatePathHints,
    networkInsightsAnalysis_forwardPathComponents,

    -- ** NetworkInsightsPath
    networkInsightsPath_createdDate,
    networkInsightsPath_source,
    networkInsightsPath_destinationIp,
    networkInsightsPath_destination,
    networkInsightsPath_sourceIp,
    networkInsightsPath_tags,
    networkInsightsPath_protocol,
    networkInsightsPath_networkInsightsPathArn,
    networkInsightsPath_networkInsightsPathId,
    networkInsightsPath_destinationPort,

    -- ** NetworkInterface
    networkInterface_groups,
    networkInterface_status,
    networkInterface_ownerId,
    networkInterface_privateIpAddresses,
    networkInterface_attachment,
    networkInterface_macAddress,
    networkInterface_association,
    networkInterface_ipv6Addresses,
    networkInterface_requesterManaged,
    networkInterface_requesterId,
    networkInterface_outpostArn,
    networkInterface_tagSet,
    networkInterface_interfaceType,
    networkInterface_sourceDestCheck,
    networkInterface_availabilityZone,
    networkInterface_networkInterfaceId,
    networkInterface_subnetId,
    networkInterface_description,
    networkInterface_privateDnsName,
    networkInterface_vpcId,
    networkInterface_privateIpAddress,

    -- ** NetworkInterfaceAssociation
    networkInterfaceAssociation_ipOwnerId,
    networkInterfaceAssociation_carrierIp,
    networkInterfaceAssociation_customerOwnedIp,
    networkInterfaceAssociation_publicDnsName,
    networkInterfaceAssociation_associationId,
    networkInterfaceAssociation_publicIp,
    networkInterfaceAssociation_allocationId,

    -- ** NetworkInterfaceAttachment
    networkInterfaceAttachment_status,
    networkInterfaceAttachment_instanceId,
    networkInterfaceAttachment_attachTime,
    networkInterfaceAttachment_attachmentId,
    networkInterfaceAttachment_networkCardIndex,
    networkInterfaceAttachment_deleteOnTermination,
    networkInterfaceAttachment_deviceIndex,
    networkInterfaceAttachment_instanceOwnerId,

    -- ** NetworkInterfaceAttachmentChanges
    networkInterfaceAttachmentChanges_attachmentId,
    networkInterfaceAttachmentChanges_deleteOnTermination,

    -- ** NetworkInterfaceIpv6Address
    networkInterfaceIpv6Address_ipv6Address,

    -- ** NetworkInterfacePermission
    networkInterfacePermission_awsAccountId,
    networkInterfacePermission_permissionState,
    networkInterfacePermission_networkInterfaceId,
    networkInterfacePermission_permission,
    networkInterfacePermission_awsService,
    networkInterfacePermission_networkInterfacePermissionId,

    -- ** NetworkInterfacePermissionState
    networkInterfacePermissionState_statusMessage,
    networkInterfacePermissionState_state,

    -- ** NetworkInterfacePrivateIpAddress
    networkInterfacePrivateIpAddress_primary,
    networkInterfacePrivateIpAddress_association,
    networkInterfacePrivateIpAddress_privateDnsName,
    networkInterfacePrivateIpAddress_privateIpAddress,

    -- ** NewDhcpConfiguration
    newDhcpConfiguration_key,
    newDhcpConfiguration_values,

    -- ** OnDemandOptions
    onDemandOptions_minTargetCapacity,
    onDemandOptions_capacityReservationOptions,
    onDemandOptions_singleInstanceType,
    onDemandOptions_allocationStrategy,
    onDemandOptions_maxTotalPrice,
    onDemandOptions_singleAvailabilityZone,

    -- ** OnDemandOptionsRequest
    onDemandOptionsRequest_minTargetCapacity,
    onDemandOptionsRequest_capacityReservationOptions,
    onDemandOptionsRequest_singleInstanceType,
    onDemandOptionsRequest_allocationStrategy,
    onDemandOptionsRequest_maxTotalPrice,
    onDemandOptionsRequest_singleAvailabilityZone,

    -- ** PathComponent
    pathComponent_securityGroupRule,
    pathComponent_sequenceNumber,
    pathComponent_subnet,
    pathComponent_sourceVpc,
    pathComponent_aclRule,
    pathComponent_inboundHeader,
    pathComponent_component,
    pathComponent_destinationVpc,
    pathComponent_routeTableRoute,
    pathComponent_outboundHeader,
    pathComponent_vpc,

    -- ** PciId
    pciId_subsystemId,
    pciId_subsystemVendorId,
    pciId_deviceId,
    pciId_vendorId,

    -- ** PeeringAttachmentStatus
    peeringAttachmentStatus_message,
    peeringAttachmentStatus_code,

    -- ** PeeringConnectionOptions
    peeringConnectionOptions_allowDnsResolutionFromRemoteVpc,
    peeringConnectionOptions_allowEgressFromLocalVpcToRemoteClassicLink,
    peeringConnectionOptions_allowEgressFromLocalClassicLinkToRemoteVpc,

    -- ** PeeringConnectionOptionsRequest
    peeringConnectionOptionsRequest_allowDnsResolutionFromRemoteVpc,
    peeringConnectionOptionsRequest_allowEgressFromLocalVpcToRemoteClassicLink,
    peeringConnectionOptionsRequest_allowEgressFromLocalClassicLinkToRemoteVpc,

    -- ** PeeringTgwInfo
    peeringTgwInfo_ownerId,
    peeringTgwInfo_region,
    peeringTgwInfo_transitGatewayId,

    -- ** Phase1DHGroupNumbersListValue
    phase1DHGroupNumbersListValue_value,

    -- ** Phase1DHGroupNumbersRequestListValue
    phase1DHGroupNumbersRequestListValue_value,

    -- ** Phase1EncryptionAlgorithmsListValue
    phase1EncryptionAlgorithmsListValue_value,

    -- ** Phase1EncryptionAlgorithmsRequestListValue
    phase1EncryptionAlgorithmsRequestListValue_value,

    -- ** Phase1IntegrityAlgorithmsListValue
    phase1IntegrityAlgorithmsListValue_value,

    -- ** Phase1IntegrityAlgorithmsRequestListValue
    phase1IntegrityAlgorithmsRequestListValue_value,

    -- ** Phase2DHGroupNumbersListValue
    phase2DHGroupNumbersListValue_value,

    -- ** Phase2DHGroupNumbersRequestListValue
    phase2DHGroupNumbersRequestListValue_value,

    -- ** Phase2EncryptionAlgorithmsListValue
    phase2EncryptionAlgorithmsListValue_value,

    -- ** Phase2EncryptionAlgorithmsRequestListValue
    phase2EncryptionAlgorithmsRequestListValue_value,

    -- ** Phase2IntegrityAlgorithmsListValue
    phase2IntegrityAlgorithmsListValue_value,

    -- ** Phase2IntegrityAlgorithmsRequestListValue
    phase2IntegrityAlgorithmsRequestListValue_value,

    -- ** Placement
    placement_spreadDomain,
    placement_groupName,
    placement_tenancy,
    placement_affinity,
    placement_availabilityZone,
    placement_partitionNumber,
    placement_hostResourceGroupArn,
    placement_hostId,

    -- ** PlacementGroup
    placementGroup_strategy,
    placementGroup_groupName,
    placementGroup_groupId,
    placementGroup_state,
    placementGroup_tags,
    placementGroup_partitionCount,

    -- ** PlacementGroupInfo
    placementGroupInfo_supportedStrategies,

    -- ** PlacementResponse
    placementResponse_groupName,

    -- ** PoolCidrBlock
    poolCidrBlock_cidr,

    -- ** PortRange
    portRange_to,
    portRange_from,

    -- ** PrefixList
    prefixList_prefixListName,
    prefixList_cidrs,
    prefixList_prefixListId,

    -- ** PrefixListAssociation
    prefixListAssociation_resourceId,
    prefixListAssociation_resourceOwner,

    -- ** PrefixListEntry
    prefixListEntry_cidr,
    prefixListEntry_description,

    -- ** PrefixListId
    prefixListId_prefixListId,
    prefixListId_description,

    -- ** PriceSchedule
    priceSchedule_currencyCode,
    priceSchedule_active,
    priceSchedule_term,
    priceSchedule_price,

    -- ** PriceScheduleSpecification
    priceScheduleSpecification_currencyCode,
    priceScheduleSpecification_term,
    priceScheduleSpecification_price,

    -- ** PricingDetail
    pricingDetail_count,
    pricingDetail_price,

    -- ** PrincipalIdFormat
    principalIdFormat_arn,
    principalIdFormat_statuses,

    -- ** PrivateDnsDetails
    privateDnsDetails_privateDnsName,

    -- ** PrivateDnsNameConfiguration
    privateDnsNameConfiguration_state,
    privateDnsNameConfiguration_name,
    privateDnsNameConfiguration_value,
    privateDnsNameConfiguration_type,

    -- ** PrivateIpAddressSpecification
    privateIpAddressSpecification_primary,
    privateIpAddressSpecification_privateIpAddress,

    -- ** ProcessorInfo
    processorInfo_sustainedClockSpeedInGhz,
    processorInfo_supportedArchitectures,

    -- ** ProductCode
    productCode_productCodeType,
    productCode_productCodeId,

    -- ** PropagatingVgw
    propagatingVgw_gatewayId,

    -- ** ProvisionedBandwidth
    provisionedBandwidth_provisionTime,
    provisionedBandwidth_status,
    provisionedBandwidth_requestTime,
    provisionedBandwidth_requested,
    provisionedBandwidth_provisioned,

    -- ** PtrUpdateStatus
    ptrUpdateStatus_status,
    ptrUpdateStatus_reason,
    ptrUpdateStatus_value,

    -- ** PublicIpv4Pool
    publicIpv4Pool_poolId,
    publicIpv4Pool_poolAddressRanges,
    publicIpv4Pool_totalAddressCount,
    publicIpv4Pool_tags,
    publicIpv4Pool_totalAvailableAddressCount,
    publicIpv4Pool_description,
    publicIpv4Pool_networkBorderGroup,

    -- ** PublicIpv4PoolRange
    publicIpv4PoolRange_addressCount,
    publicIpv4PoolRange_firstAddress,
    publicIpv4PoolRange_lastAddress,
    publicIpv4PoolRange_availableAddressCount,

    -- ** Purchase
    purchase_instanceFamily,
    purchase_hostIdSet,
    purchase_upfrontPrice,
    purchase_paymentOption,
    purchase_duration,
    purchase_hostReservationId,
    purchase_currencyCode,
    purchase_hourlyPrice,

    -- ** PurchaseRequest
    purchaseRequest_instanceCount,
    purchaseRequest_purchaseToken,

    -- ** RecurringCharge
    recurringCharge_amount,
    recurringCharge_frequency,

    -- ** RegionInfo
    regionInfo_regionName,
    regionInfo_optInStatus,
    regionInfo_endpoint,

    -- ** RegisterInstanceTagAttributeRequest
    registerInstanceTagAttributeRequest_instanceTagKeys,
    registerInstanceTagAttributeRequest_includeAllTagsOfInstance,

    -- ** RemovePrefixListEntry
    removePrefixListEntry_cidr,

    -- ** RequestLaunchTemplateData
    requestLaunchTemplateData_securityGroupIds,
    requestLaunchTemplateData_tagSpecifications,
    requestLaunchTemplateData_elasticGpuSpecifications,
    requestLaunchTemplateData_instanceType,
    requestLaunchTemplateData_capacityReservationSpecification,
    requestLaunchTemplateData_ebsOptimized,
    requestLaunchTemplateData_userData,
    requestLaunchTemplateData_placement,
    requestLaunchTemplateData_ramDiskId,
    requestLaunchTemplateData_creditSpecification,
    requestLaunchTemplateData_instanceMarketOptions,
    requestLaunchTemplateData_licenseSpecifications,
    requestLaunchTemplateData_instanceInitiatedShutdownBehavior,
    requestLaunchTemplateData_imageId,
    requestLaunchTemplateData_securityGroups,
    requestLaunchTemplateData_elasticInferenceAccelerators,
    requestLaunchTemplateData_iamInstanceProfile,
    requestLaunchTemplateData_hibernationOptions,
    requestLaunchTemplateData_monitoring,
    requestLaunchTemplateData_blockDeviceMappings,
    requestLaunchTemplateData_enclaveOptions,
    requestLaunchTemplateData_kernelId,
    requestLaunchTemplateData_cpuOptions,
    requestLaunchTemplateData_keyName,
    requestLaunchTemplateData_networkInterfaces,
    requestLaunchTemplateData_disableApiTermination,
    requestLaunchTemplateData_metadataOptions,

    -- ** RequestSpotLaunchSpecification
    requestSpotLaunchSpecification_securityGroupIds,
    requestSpotLaunchSpecification_instanceType,
    requestSpotLaunchSpecification_ebsOptimized,
    requestSpotLaunchSpecification_userData,
    requestSpotLaunchSpecification_placement,
    requestSpotLaunchSpecification_addressingType,
    requestSpotLaunchSpecification_ramdiskId,
    requestSpotLaunchSpecification_imageId,
    requestSpotLaunchSpecification_securityGroups,
    requestSpotLaunchSpecification_iamInstanceProfile,
    requestSpotLaunchSpecification_monitoring,
    requestSpotLaunchSpecification_blockDeviceMappings,
    requestSpotLaunchSpecification_subnetId,
    requestSpotLaunchSpecification_kernelId,
    requestSpotLaunchSpecification_keyName,
    requestSpotLaunchSpecification_networkInterfaces,

    -- ** Reservation
    reservation_groups,
    reservation_requesterId,
    reservation_instances,
    reservation_reservationId,
    reservation_ownerId,

    -- ** ReservationValue
    reservationValue_remainingUpfrontValue,
    reservationValue_hourlyPrice,
    reservationValue_remainingTotalValue,

    -- ** ReservedInstanceLimitPrice
    reservedInstanceLimitPrice_amount,
    reservedInstanceLimitPrice_currencyCode,

    -- ** ReservedInstanceReservationValue
    reservedInstanceReservationValue_reservationValue,
    reservedInstanceReservationValue_reservedInstanceId,

    -- ** ReservedInstances
    reservedInstances_end,
    reservedInstances_instanceType,
    reservedInstances_duration,
    reservedInstances_reservedInstancesId,
    reservedInstances_instanceTenancy,
    reservedInstances_currencyCode,
    reservedInstances_scope,
    reservedInstances_state,
    reservedInstances_availabilityZone,
    reservedInstances_tags,
    reservedInstances_offeringClass,
    reservedInstances_fixedPrice,
    reservedInstances_usagePrice,
    reservedInstances_offeringType,
    reservedInstances_start,
    reservedInstances_recurringCharges,
    reservedInstances_productDescription,
    reservedInstances_instanceCount,

    -- ** ReservedInstancesConfiguration
    reservedInstancesConfiguration_platform,
    reservedInstancesConfiguration_instanceType,
    reservedInstancesConfiguration_scope,
    reservedInstancesConfiguration_availabilityZone,
    reservedInstancesConfiguration_instanceCount,

    -- ** ReservedInstancesId
    reservedInstancesId_reservedInstancesId,

    -- ** ReservedInstancesListing
    reservedInstancesListing_statusMessage,
    reservedInstancesListing_status,
    reservedInstancesListing_priceSchedules,
    reservedInstancesListing_reservedInstancesId,
    reservedInstancesListing_createDate,
    reservedInstancesListing_tags,
    reservedInstancesListing_instanceCounts,
    reservedInstancesListing_reservedInstancesListingId,
    reservedInstancesListing_updateDate,
    reservedInstancesListing_clientToken,

    -- ** ReservedInstancesModification
    reservedInstancesModification_statusMessage,
    reservedInstancesModification_status,
    reservedInstancesModification_createDate,
    reservedInstancesModification_modificationResults,
    reservedInstancesModification_effectiveDate,
    reservedInstancesModification_reservedInstancesIds,
    reservedInstancesModification_reservedInstancesModificationId,
    reservedInstancesModification_updateDate,
    reservedInstancesModification_clientToken,

    -- ** ReservedInstancesModificationResult
    reservedInstancesModificationResult_targetConfiguration,
    reservedInstancesModificationResult_reservedInstancesId,

    -- ** ReservedInstancesOffering
    reservedInstancesOffering_instanceType,
    reservedInstancesOffering_duration,
    reservedInstancesOffering_instanceTenancy,
    reservedInstancesOffering_reservedInstancesOfferingId,
    reservedInstancesOffering_currencyCode,
    reservedInstancesOffering_scope,
    reservedInstancesOffering_availabilityZone,
    reservedInstancesOffering_marketplace,
    reservedInstancesOffering_offeringClass,
    reservedInstancesOffering_fixedPrice,
    reservedInstancesOffering_usagePrice,
    reservedInstancesOffering_offeringType,
    reservedInstancesOffering_recurringCharges,
    reservedInstancesOffering_productDescription,
    reservedInstancesOffering_pricingDetails,

    -- ** ResponseError
    responseError_message,
    responseError_code,

    -- ** ResponseLaunchTemplateData
    responseLaunchTemplateData_securityGroupIds,
    responseLaunchTemplateData_tagSpecifications,
    responseLaunchTemplateData_elasticGpuSpecifications,
    responseLaunchTemplateData_instanceType,
    responseLaunchTemplateData_capacityReservationSpecification,
    responseLaunchTemplateData_ebsOptimized,
    responseLaunchTemplateData_userData,
    responseLaunchTemplateData_placement,
    responseLaunchTemplateData_ramDiskId,
    responseLaunchTemplateData_creditSpecification,
    responseLaunchTemplateData_instanceMarketOptions,
    responseLaunchTemplateData_licenseSpecifications,
    responseLaunchTemplateData_instanceInitiatedShutdownBehavior,
    responseLaunchTemplateData_imageId,
    responseLaunchTemplateData_securityGroups,
    responseLaunchTemplateData_elasticInferenceAccelerators,
    responseLaunchTemplateData_iamInstanceProfile,
    responseLaunchTemplateData_hibernationOptions,
    responseLaunchTemplateData_monitoring,
    responseLaunchTemplateData_blockDeviceMappings,
    responseLaunchTemplateData_enclaveOptions,
    responseLaunchTemplateData_kernelId,
    responseLaunchTemplateData_cpuOptions,
    responseLaunchTemplateData_keyName,
    responseLaunchTemplateData_networkInterfaces,
    responseLaunchTemplateData_disableApiTermination,
    responseLaunchTemplateData_metadataOptions,

    -- ** Route
    route_instanceId,
    route_origin,
    route_vpcPeeringConnectionId,
    route_destinationPrefixListId,
    route_destinationIpv6CidrBlock,
    route_localGatewayId,
    route_state,
    route_egressOnlyInternetGatewayId,
    route_carrierGatewayId,
    route_destinationCidrBlock,
    route_networkInterfaceId,
    route_natGatewayId,
    route_instanceOwnerId,
    route_gatewayId,
    route_transitGatewayId,

    -- ** RouteTable
    routeTable_ownerId,
    routeTable_routeTableId,
    routeTable_routes,
    routeTable_tags,
    routeTable_propagatingVgws,
    routeTable_vpcId,
    routeTable_associations,

    -- ** RouteTableAssociation
    routeTableAssociation_associationState,
    routeTableAssociation_main,
    routeTableAssociation_routeTableId,
    routeTableAssociation_subnetId,
    routeTableAssociation_routeTableAssociationId,
    routeTableAssociation_gatewayId,

    -- ** RouteTableAssociationState
    routeTableAssociationState_statusMessage,
    routeTableAssociationState_state,

    -- ** RunInstancesMonitoringEnabled
    runInstancesMonitoringEnabled_enabled,

    -- ** S3Storage
    s3Storage_uploadPolicySignature,
    s3Storage_uploadPolicy,
    s3Storage_prefix,
    s3Storage_bucket,
    s3Storage_aWSAccessKeyId,

    -- ** ScheduledInstance
    scheduledInstance_platform,
    scheduledInstance_instanceType,
    scheduledInstance_networkPlatform,
    scheduledInstance_slotDurationInHours,
    scheduledInstance_createDate,
    scheduledInstance_scheduledInstanceId,
    scheduledInstance_previousSlotEndTime,
    scheduledInstance_availabilityZone,
    scheduledInstance_recurrence,
    scheduledInstance_totalScheduledInstanceHours,
    scheduledInstance_nextSlotStartTime,
    scheduledInstance_hourlyPrice,
    scheduledInstance_termEndDate,
    scheduledInstance_termStartDate,
    scheduledInstance_instanceCount,

    -- ** ScheduledInstanceAvailability
    scheduledInstanceAvailability_platform,
    scheduledInstanceAvailability_instanceType,
    scheduledInstanceAvailability_networkPlatform,
    scheduledInstanceAvailability_slotDurationInHours,
    scheduledInstanceAvailability_availableInstanceCount,
    scheduledInstanceAvailability_minTermDurationInDays,
    scheduledInstanceAvailability_availabilityZone,
    scheduledInstanceAvailability_recurrence,
    scheduledInstanceAvailability_maxTermDurationInDays,
    scheduledInstanceAvailability_totalScheduledInstanceHours,
    scheduledInstanceAvailability_firstSlotStartTime,
    scheduledInstanceAvailability_hourlyPrice,
    scheduledInstanceAvailability_purchaseToken,

    -- ** ScheduledInstanceRecurrence
    scheduledInstanceRecurrence_occurrenceUnit,
    scheduledInstanceRecurrence_interval,
    scheduledInstanceRecurrence_occurrenceRelativeToEnd,
    scheduledInstanceRecurrence_frequency,
    scheduledInstanceRecurrence_occurrenceDaySet,

    -- ** ScheduledInstanceRecurrenceRequest
    scheduledInstanceRecurrenceRequest_occurrenceUnit,
    scheduledInstanceRecurrenceRequest_occurrenceDays,
    scheduledInstanceRecurrenceRequest_interval,
    scheduledInstanceRecurrenceRequest_occurrenceRelativeToEnd,
    scheduledInstanceRecurrenceRequest_frequency,

    -- ** ScheduledInstancesBlockDeviceMapping
    scheduledInstancesBlockDeviceMapping_ebs,
    scheduledInstancesBlockDeviceMapping_noDevice,
    scheduledInstancesBlockDeviceMapping_virtualName,
    scheduledInstancesBlockDeviceMapping_deviceName,

    -- ** ScheduledInstancesEbs
    scheduledInstancesEbs_encrypted,
    scheduledInstancesEbs_deleteOnTermination,
    scheduledInstancesEbs_snapshotId,
    scheduledInstancesEbs_volumeType,
    scheduledInstancesEbs_volumeSize,
    scheduledInstancesEbs_iops,

    -- ** ScheduledInstancesIamInstanceProfile
    scheduledInstancesIamInstanceProfile_arn,
    scheduledInstancesIamInstanceProfile_name,

    -- ** ScheduledInstancesIpv6Address
    scheduledInstancesIpv6Address_ipv6Address,

    -- ** ScheduledInstancesLaunchSpecification
    scheduledInstancesLaunchSpecification_securityGroupIds,
    scheduledInstancesLaunchSpecification_instanceType,
    scheduledInstancesLaunchSpecification_ebsOptimized,
    scheduledInstancesLaunchSpecification_userData,
    scheduledInstancesLaunchSpecification_placement,
    scheduledInstancesLaunchSpecification_ramdiskId,
    scheduledInstancesLaunchSpecification_iamInstanceProfile,
    scheduledInstancesLaunchSpecification_monitoring,
    scheduledInstancesLaunchSpecification_blockDeviceMappings,
    scheduledInstancesLaunchSpecification_subnetId,
    scheduledInstancesLaunchSpecification_kernelId,
    scheduledInstancesLaunchSpecification_keyName,
    scheduledInstancesLaunchSpecification_networkInterfaces,
    scheduledInstancesLaunchSpecification_imageId,

    -- ** ScheduledInstancesMonitoring
    scheduledInstancesMonitoring_enabled,

    -- ** ScheduledInstancesNetworkInterface
    scheduledInstancesNetworkInterface_groups,
    scheduledInstancesNetworkInterface_ipv6Addresses,
    scheduledInstancesNetworkInterface_associatePublicIpAddress,
    scheduledInstancesNetworkInterface_ipv6AddressCount,
    scheduledInstancesNetworkInterface_deleteOnTermination,
    scheduledInstancesNetworkInterface_networkInterfaceId,
    scheduledInstancesNetworkInterface_subnetId,
    scheduledInstancesNetworkInterface_description,
    scheduledInstancesNetworkInterface_deviceIndex,
    scheduledInstancesNetworkInterface_secondaryPrivateIpAddressCount,
    scheduledInstancesNetworkInterface_privateIpAddressConfigs,
    scheduledInstancesNetworkInterface_privateIpAddress,

    -- ** ScheduledInstancesPlacement
    scheduledInstancesPlacement_groupName,
    scheduledInstancesPlacement_availabilityZone,

    -- ** ScheduledInstancesPrivateIpAddressConfig
    scheduledInstancesPrivateIpAddressConfig_primary,
    scheduledInstancesPrivateIpAddressConfig_privateIpAddress,

    -- ** SecurityGroup
    securityGroup_ipPermissionsEgress,
    securityGroup_tags,
    securityGroup_ipPermissions,
    securityGroup_vpcId,
    securityGroup_ownerId,
    securityGroup_groupId,
    securityGroup_groupName,
    securityGroup_description,

    -- ** SecurityGroupIdentifier
    securityGroupIdentifier_groupName,
    securityGroupIdentifier_groupId,

    -- ** SecurityGroupReference
    securityGroupReference_vpcPeeringConnectionId,
    securityGroupReference_groupId,
    securityGroupReference_referencingVpcId,

    -- ** ServiceConfiguration
    serviceConfiguration_managesVpcEndpoints,
    serviceConfiguration_gatewayLoadBalancerArns,
    serviceConfiguration_availabilityZones,
    serviceConfiguration_privateDnsNameConfiguration,
    serviceConfiguration_baseEndpointDnsNames,
    serviceConfiguration_serviceId,
    serviceConfiguration_serviceName,
    serviceConfiguration_tags,
    serviceConfiguration_privateDnsName,
    serviceConfiguration_acceptanceRequired,
    serviceConfiguration_serviceType,
    serviceConfiguration_networkLoadBalancerArns,
    serviceConfiguration_serviceState,

    -- ** ServiceDetail
    serviceDetail_managesVpcEndpoints,
    serviceDetail_availabilityZones,
    serviceDetail_baseEndpointDnsNames,
    serviceDetail_serviceId,
    serviceDetail_vpcEndpointPolicySupported,
    serviceDetail_privateDnsNames,
    serviceDetail_privateDnsNameVerificationState,
    serviceDetail_serviceName,
    serviceDetail_tags,
    serviceDetail_owner,
    serviceDetail_privateDnsName,
    serviceDetail_acceptanceRequired,
    serviceDetail_serviceType,

    -- ** ServiceTypeDetail
    serviceTypeDetail_serviceType,

    -- ** SlotDateTimeRangeRequest
    slotDateTimeRangeRequest_earliestTime,
    slotDateTimeRangeRequest_latestTime,

    -- ** SlotStartTimeRangeRequest
    slotStartTimeRangeRequest_earliestTime,
    slotStartTimeRangeRequest_latestTime,

    -- ** Snapshot
    snapshot_ownerAlias,
    snapshot_stateMessage,
    snapshot_outpostArn,
    snapshot_dataEncryptionKeyId,
    snapshot_kmsKeyId,
    snapshot_tags,
    snapshot_snapshotId,
    snapshot_ownerId,
    snapshot_volumeId,
    snapshot_volumeSize,
    snapshot_description,
    snapshot_startTime,
    snapshot_progress,
    snapshot_state,
    snapshot_encrypted,

    -- ** SnapshotDetail
    snapshotDetail_diskImageSize,
    snapshotDetail_statusMessage,
    snapshotDetail_status,
    snapshotDetail_format,
    snapshotDetail_userBucket,
    snapshotDetail_deviceName,
    snapshotDetail_snapshotId,
    snapshotDetail_description,
    snapshotDetail_url,
    snapshotDetail_progress,

    -- ** SnapshotDiskContainer
    snapshotDiskContainer_format,
    snapshotDiskContainer_userBucket,
    snapshotDiskContainer_description,
    snapshotDiskContainer_url,

    -- ** SnapshotInfo
    snapshotInfo_ownerId,
    snapshotInfo_encrypted,
    snapshotInfo_outpostArn,
    snapshotInfo_startTime,
    snapshotInfo_volumeId,
    snapshotInfo_state,
    snapshotInfo_snapshotId,
    snapshotInfo_tags,
    snapshotInfo_description,
    snapshotInfo_progress,
    snapshotInfo_volumeSize,

    -- ** SnapshotTaskDetail
    snapshotTaskDetail_diskImageSize,
    snapshotTaskDetail_statusMessage,
    snapshotTaskDetail_status,
    snapshotTaskDetail_encrypted,
    snapshotTaskDetail_format,
    snapshotTaskDetail_userBucket,
    snapshotTaskDetail_kmsKeyId,
    snapshotTaskDetail_snapshotId,
    snapshotTaskDetail_description,
    snapshotTaskDetail_url,
    snapshotTaskDetail_progress,

    -- ** SpotCapacityRebalance
    spotCapacityRebalance_replacementStrategy,

    -- ** SpotDatafeedSubscription
    spotDatafeedSubscription_ownerId,
    spotDatafeedSubscription_prefix,
    spotDatafeedSubscription_fault,
    spotDatafeedSubscription_state,
    spotDatafeedSubscription_bucket,

    -- ** SpotFleetLaunchSpecification
    spotFleetLaunchSpecification_tagSpecifications,
    spotFleetLaunchSpecification_instanceType,
    spotFleetLaunchSpecification_ebsOptimized,
    spotFleetLaunchSpecification_userData,
    spotFleetLaunchSpecification_placement,
    spotFleetLaunchSpecification_addressingType,
    spotFleetLaunchSpecification_ramdiskId,
    spotFleetLaunchSpecification_spotPrice,
    spotFleetLaunchSpecification_imageId,
    spotFleetLaunchSpecification_securityGroups,
    spotFleetLaunchSpecification_iamInstanceProfile,
    spotFleetLaunchSpecification_monitoring,
    spotFleetLaunchSpecification_blockDeviceMappings,
    spotFleetLaunchSpecification_subnetId,
    spotFleetLaunchSpecification_kernelId,
    spotFleetLaunchSpecification_keyName,
    spotFleetLaunchSpecification_networkInterfaces,
    spotFleetLaunchSpecification_weightedCapacity,

    -- ** SpotFleetMonitoring
    spotFleetMonitoring_enabled,

    -- ** SpotFleetRequestConfig
    spotFleetRequestConfig_spotFleetRequestState,
    spotFleetRequestConfig_activityStatus,
    spotFleetRequestConfig_tags,
    spotFleetRequestConfig_createTime,
    spotFleetRequestConfig_spotFleetRequestConfig,
    spotFleetRequestConfig_spotFleetRequestId,

    -- ** SpotFleetRequestConfigData
    spotFleetRequestConfigData_launchTemplateConfigs,
    spotFleetRequestConfigData_tagSpecifications,
    spotFleetRequestConfigData_spotMaxTotalPrice,
    spotFleetRequestConfigData_onDemandTargetCapacity,
    spotFleetRequestConfigData_onDemandFulfilledCapacity,
    spotFleetRequestConfigData_validFrom,
    spotFleetRequestConfigData_replaceUnhealthyInstances,
    spotFleetRequestConfigData_onDemandAllocationStrategy,
    spotFleetRequestConfigData_spotPrice,
    spotFleetRequestConfigData_onDemandMaxTotalPrice,
    spotFleetRequestConfigData_instanceInterruptionBehavior,
    spotFleetRequestConfigData_validUntil,
    spotFleetRequestConfigData_loadBalancersConfig,
    spotFleetRequestConfigData_excessCapacityTerminationPolicy,
    spotFleetRequestConfigData_allocationStrategy,
    spotFleetRequestConfigData_launchSpecifications,
    spotFleetRequestConfigData_type,
    spotFleetRequestConfigData_spotMaintenanceStrategies,
    spotFleetRequestConfigData_instancePoolsToUseCount,
    spotFleetRequestConfigData_fulfilledCapacity,
    spotFleetRequestConfigData_clientToken,
    spotFleetRequestConfigData_terminateInstancesWithExpiration,
    spotFleetRequestConfigData_iamFleetRole,
    spotFleetRequestConfigData_targetCapacity,

    -- ** SpotFleetTagSpecification
    spotFleetTagSpecification_resourceType,
    spotFleetTagSpecification_tags,

    -- ** SpotInstanceRequest
    spotInstanceRequest_actualBlockHourlyPrice,
    spotInstanceRequest_status,
    spotInstanceRequest_instanceId,
    spotInstanceRequest_launchedAvailabilityZone,
    spotInstanceRequest_validFrom,
    spotInstanceRequest_spotPrice,
    spotInstanceRequest_fault,
    spotInstanceRequest_blockDurationMinutes,
    spotInstanceRequest_launchGroup,
    spotInstanceRequest_instanceInterruptionBehavior,
    spotInstanceRequest_state,
    spotInstanceRequest_validUntil,
    spotInstanceRequest_tags,
    spotInstanceRequest_createTime,
    spotInstanceRequest_launchSpecification,
    spotInstanceRequest_type,
    spotInstanceRequest_availabilityZoneGroup,
    spotInstanceRequest_productDescription,
    spotInstanceRequest_spotInstanceRequestId,

    -- ** SpotInstanceStateFault
    spotInstanceStateFault_message,
    spotInstanceStateFault_code,

    -- ** SpotInstanceStatus
    spotInstanceStatus_message,
    spotInstanceStatus_code,
    spotInstanceStatus_updateTime,

    -- ** SpotMaintenanceStrategies
    spotMaintenanceStrategies_capacityRebalance,

    -- ** SpotMarketOptions
    spotMarketOptions_blockDurationMinutes,
    spotMarketOptions_instanceInterruptionBehavior,
    spotMarketOptions_validUntil,
    spotMarketOptions_spotInstanceType,
    spotMarketOptions_maxPrice,

    -- ** SpotOptions
    spotOptions_minTargetCapacity,
    spotOptions_maintenanceStrategies,
    spotOptions_instanceInterruptionBehavior,
    spotOptions_singleInstanceType,
    spotOptions_allocationStrategy,
    spotOptions_maxTotalPrice,
    spotOptions_instancePoolsToUseCount,
    spotOptions_singleAvailabilityZone,

    -- ** SpotOptionsRequest
    spotOptionsRequest_minTargetCapacity,
    spotOptionsRequest_maintenanceStrategies,
    spotOptionsRequest_instanceInterruptionBehavior,
    spotOptionsRequest_singleInstanceType,
    spotOptionsRequest_allocationStrategy,
    spotOptionsRequest_maxTotalPrice,
    spotOptionsRequest_instancePoolsToUseCount,
    spotOptionsRequest_singleAvailabilityZone,

    -- ** SpotPlacement
    spotPlacement_groupName,
    spotPlacement_tenancy,
    spotPlacement_availabilityZone,

    -- ** SpotPrice
    spotPrice_instanceType,
    spotPrice_spotPrice,
    spotPrice_availabilityZone,
    spotPrice_timestamp,
    spotPrice_productDescription,

    -- ** StaleIpPermission
    staleIpPermission_fromPort,
    staleIpPermission_prefixListIds,
    staleIpPermission_ipRanges,
    staleIpPermission_userIdGroupPairs,
    staleIpPermission_ipProtocol,
    staleIpPermission_toPort,

    -- ** StaleSecurityGroup
    staleSecurityGroup_staleIpPermissions,
    staleSecurityGroup_groupName,
    staleSecurityGroup_groupId,
    staleSecurityGroup_description,
    staleSecurityGroup_staleIpPermissionsEgress,
    staleSecurityGroup_vpcId,

    -- ** StateReason
    stateReason_message,
    stateReason_code,

    -- ** Storage
    storage_s3,

    -- ** StorageLocation
    storageLocation_key,
    storageLocation_bucket,

    -- ** Subnet
    subnet_ownerId,
    subnet_customerOwnedIpv4Pool,
    subnet_subnetArn,
    subnet_assignIpv6AddressOnCreation,
    subnet_outpostArn,
    subnet_mapPublicIpOnLaunch,
    subnet_availabilityZoneId,
    subnet_ipv6CidrBlockAssociationSet,
    subnet_tags,
    subnet_defaultForAz,
    subnet_mapCustomerOwnedIpOnLaunch,
    subnet_availabilityZone,
    subnet_availableIpAddressCount,
    subnet_cidrBlock,
    subnet_state,
    subnet_subnetId,
    subnet_vpcId,

    -- ** SubnetAssociation
    subnetAssociation_state,
    subnetAssociation_subnetId,

    -- ** SubnetCidrBlockState
    subnetCidrBlockState_statusMessage,
    subnetCidrBlockState_state,

    -- ** SubnetIpv6CidrBlockAssociation
    subnetIpv6CidrBlockAssociation_ipv6CidrBlockState,
    subnetIpv6CidrBlockAssociation_associationId,
    subnetIpv6CidrBlockAssociation_ipv6CidrBlock,

    -- ** SuccessfulInstanceCreditSpecificationItem
    successfulInstanceCreditSpecificationItem_instanceId,

    -- ** SuccessfulQueuedPurchaseDeletion
    successfulQueuedPurchaseDeletion_reservedInstancesId,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TagDescription
    tagDescription_resourceId,
    tagDescription_resourceType,
    tagDescription_key,
    tagDescription_value,

    -- ** TagSpecification
    tagSpecification_resourceType,
    tagSpecification_tags,

    -- ** TargetCapacitySpecification
    targetCapacitySpecification_totalTargetCapacity,
    targetCapacitySpecification_defaultTargetCapacityType,
    targetCapacitySpecification_onDemandTargetCapacity,
    targetCapacitySpecification_spotTargetCapacity,

    -- ** TargetCapacitySpecificationRequest
    targetCapacitySpecificationRequest_defaultTargetCapacityType,
    targetCapacitySpecificationRequest_onDemandTargetCapacity,
    targetCapacitySpecificationRequest_spotTargetCapacity,
    targetCapacitySpecificationRequest_totalTargetCapacity,

    -- ** TargetConfiguration
    targetConfiguration_offeringId,
    targetConfiguration_instanceCount,

    -- ** TargetConfigurationRequest
    targetConfigurationRequest_instanceCount,
    targetConfigurationRequest_offeringId,

    -- ** TargetGroup
    targetGroup_arn,

    -- ** TargetGroupsConfig
    targetGroupsConfig_targetGroups,

    -- ** TargetNetwork
    targetNetwork_clientVpnEndpointId,
    targetNetwork_status,
    targetNetwork_securityGroups,
    targetNetwork_associationId,
    targetNetwork_vpcId,
    targetNetwork_targetNetworkId,

    -- ** TargetReservationValue
    targetReservationValue_targetConfiguration,
    targetReservationValue_reservationValue,

    -- ** TerminateConnectionStatus
    terminateConnectionStatus_connectionId,
    terminateConnectionStatus_previousStatus,
    terminateConnectionStatus_currentStatus,

    -- ** TrafficMirrorFilter
    trafficMirrorFilter_egressFilterRules,
    trafficMirrorFilter_networkServices,
    trafficMirrorFilter_tags,
    trafficMirrorFilter_trafficMirrorFilterId,
    trafficMirrorFilter_description,
    trafficMirrorFilter_ingressFilterRules,

    -- ** TrafficMirrorFilterRule
    trafficMirrorFilterRule_trafficMirrorFilterRuleId,
    trafficMirrorFilterRule_sourcePortRange,
    trafficMirrorFilterRule_trafficDirection,
    trafficMirrorFilterRule_ruleAction,
    trafficMirrorFilterRule_sourceCidrBlock,
    trafficMirrorFilterRule_trafficMirrorFilterId,
    trafficMirrorFilterRule_destinationCidrBlock,
    trafficMirrorFilterRule_protocol,
    trafficMirrorFilterRule_description,
    trafficMirrorFilterRule_ruleNumber,
    trafficMirrorFilterRule_destinationPortRange,

    -- ** TrafficMirrorPortRange
    trafficMirrorPortRange_fromPort,
    trafficMirrorPortRange_toPort,

    -- ** TrafficMirrorPortRangeRequest
    trafficMirrorPortRangeRequest_fromPort,
    trafficMirrorPortRangeRequest_toPort,

    -- ** TrafficMirrorSession
    trafficMirrorSession_ownerId,
    trafficMirrorSession_trafficMirrorSessionId,
    trafficMirrorSession_packetLength,
    trafficMirrorSession_tags,
    trafficMirrorSession_trafficMirrorFilterId,
    trafficMirrorSession_networkInterfaceId,
    trafficMirrorSession_description,
    trafficMirrorSession_trafficMirrorTargetId,
    trafficMirrorSession_sessionNumber,
    trafficMirrorSession_virtualNetworkId,

    -- ** TrafficMirrorTarget
    trafficMirrorTarget_ownerId,
    trafficMirrorTarget_networkLoadBalancerArn,
    trafficMirrorTarget_tags,
    trafficMirrorTarget_networkInterfaceId,
    trafficMirrorTarget_description,
    trafficMirrorTarget_type,
    trafficMirrorTarget_trafficMirrorTargetId,

    -- ** TransitGateway
    transitGateway_ownerId,
    transitGateway_creationTime,
    transitGateway_options,
    transitGateway_transitGatewayArn,
    transitGateway_state,
    transitGateway_tags,
    transitGateway_description,
    transitGateway_transitGatewayId,

    -- ** TransitGatewayAssociation
    transitGatewayAssociation_resourceId,
    transitGatewayAssociation_resourceType,
    transitGatewayAssociation_state,
    transitGatewayAssociation_transitGatewayAttachmentId,
    transitGatewayAssociation_transitGatewayRouteTableId,

    -- ** TransitGatewayAttachment
    transitGatewayAttachment_resourceId,
    transitGatewayAttachment_creationTime,
    transitGatewayAttachment_association,
    transitGatewayAttachment_resourceType,
    transitGatewayAttachment_state,
    transitGatewayAttachment_tags,
    transitGatewayAttachment_resourceOwnerId,
    transitGatewayAttachment_transitGatewayAttachmentId,
    transitGatewayAttachment_transitGatewayOwnerId,
    transitGatewayAttachment_transitGatewayId,

    -- ** TransitGatewayAttachmentAssociation
    transitGatewayAttachmentAssociation_state,
    transitGatewayAttachmentAssociation_transitGatewayRouteTableId,

    -- ** TransitGatewayAttachmentBgpConfiguration
    transitGatewayAttachmentBgpConfiguration_peerAddress,
    transitGatewayAttachmentBgpConfiguration_peerAsn,
    transitGatewayAttachmentBgpConfiguration_bgpStatus,
    transitGatewayAttachmentBgpConfiguration_transitGatewayAddress,
    transitGatewayAttachmentBgpConfiguration_transitGatewayAsn,

    -- ** TransitGatewayAttachmentPropagation
    transitGatewayAttachmentPropagation_state,
    transitGatewayAttachmentPropagation_transitGatewayRouteTableId,

    -- ** TransitGatewayConnect
    transitGatewayConnect_creationTime,
    transitGatewayConnect_options,
    transitGatewayConnect_state,
    transitGatewayConnect_tags,
    transitGatewayConnect_transitGatewayAttachmentId,
    transitGatewayConnect_transportTransitGatewayAttachmentId,
    transitGatewayConnect_transitGatewayId,

    -- ** TransitGatewayConnectOptions
    transitGatewayConnectOptions_protocol,

    -- ** TransitGatewayConnectPeer
    transitGatewayConnectPeer_connectPeerConfiguration,
    transitGatewayConnectPeer_creationTime,
    transitGatewayConnectPeer_transitGatewayConnectPeerId,
    transitGatewayConnectPeer_state,
    transitGatewayConnectPeer_tags,
    transitGatewayConnectPeer_transitGatewayAttachmentId,

    -- ** TransitGatewayConnectPeerConfiguration
    transitGatewayConnectPeerConfiguration_peerAddress,
    transitGatewayConnectPeerConfiguration_transitGatewayAddress,
    transitGatewayConnectPeerConfiguration_bgpConfigurations,
    transitGatewayConnectPeerConfiguration_protocol,
    transitGatewayConnectPeerConfiguration_insideCidrBlocks,

    -- ** TransitGatewayConnectRequestBgpOptions
    transitGatewayConnectRequestBgpOptions_peerAsn,

    -- ** TransitGatewayMulticastDeregisteredGroupMembers
    transitGatewayMulticastDeregisteredGroupMembers_transitGatewayMulticastDomainId,
    transitGatewayMulticastDeregisteredGroupMembers_deregisteredNetworkInterfaceIds,
    transitGatewayMulticastDeregisteredGroupMembers_groupIpAddress,

    -- ** TransitGatewayMulticastDeregisteredGroupSources
    transitGatewayMulticastDeregisteredGroupSources_transitGatewayMulticastDomainId,
    transitGatewayMulticastDeregisteredGroupSources_deregisteredNetworkInterfaceIds,
    transitGatewayMulticastDeregisteredGroupSources_groupIpAddress,

    -- ** TransitGatewayMulticastDomain
    transitGatewayMulticastDomain_ownerId,
    transitGatewayMulticastDomain_creationTime,
    transitGatewayMulticastDomain_transitGatewayMulticastDomainId,
    transitGatewayMulticastDomain_options,
    transitGatewayMulticastDomain_state,
    transitGatewayMulticastDomain_tags,
    transitGatewayMulticastDomain_transitGatewayMulticastDomainArn,
    transitGatewayMulticastDomain_transitGatewayId,

    -- ** TransitGatewayMulticastDomainAssociation
    transitGatewayMulticastDomainAssociation_resourceId,
    transitGatewayMulticastDomainAssociation_subnet,
    transitGatewayMulticastDomainAssociation_resourceType,
    transitGatewayMulticastDomainAssociation_resourceOwnerId,
    transitGatewayMulticastDomainAssociation_transitGatewayAttachmentId,

    -- ** TransitGatewayMulticastDomainAssociations
    transitGatewayMulticastDomainAssociations_resourceId,
    transitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId,
    transitGatewayMulticastDomainAssociations_resourceType,
    transitGatewayMulticastDomainAssociations_resourceOwnerId,
    transitGatewayMulticastDomainAssociations_transitGatewayAttachmentId,
    transitGatewayMulticastDomainAssociations_subnets,

    -- ** TransitGatewayMulticastDomainOptions
    transitGatewayMulticastDomainOptions_igmpv2Support,
    transitGatewayMulticastDomainOptions_autoAcceptSharedAssociations,
    transitGatewayMulticastDomainOptions_staticSourcesSupport,

    -- ** TransitGatewayMulticastGroup
    transitGatewayMulticastGroup_resourceId,
    transitGatewayMulticastGroup_groupMember,
    transitGatewayMulticastGroup_memberType,
    transitGatewayMulticastGroup_groupSource,
    transitGatewayMulticastGroup_resourceType,
    transitGatewayMulticastGroup_resourceOwnerId,
    transitGatewayMulticastGroup_networkInterfaceId,
    transitGatewayMulticastGroup_subnetId,
    transitGatewayMulticastGroup_transitGatewayAttachmentId,
    transitGatewayMulticastGroup_groupIpAddress,
    transitGatewayMulticastGroup_sourceType,

    -- ** TransitGatewayMulticastRegisteredGroupMembers
    transitGatewayMulticastRegisteredGroupMembers_transitGatewayMulticastDomainId,
    transitGatewayMulticastRegisteredGroupMembers_groupIpAddress,
    transitGatewayMulticastRegisteredGroupMembers_registeredNetworkInterfaceIds,

    -- ** TransitGatewayMulticastRegisteredGroupSources
    transitGatewayMulticastRegisteredGroupSources_transitGatewayMulticastDomainId,
    transitGatewayMulticastRegisteredGroupSources_groupIpAddress,
    transitGatewayMulticastRegisteredGroupSources_registeredNetworkInterfaceIds,

    -- ** TransitGatewayOptions
    transitGatewayOptions_propagationDefaultRouteTableId,
    transitGatewayOptions_vpnEcmpSupport,
    transitGatewayOptions_dnsSupport,
    transitGatewayOptions_associationDefaultRouteTableId,
    transitGatewayOptions_autoAcceptSharedAttachments,
    transitGatewayOptions_amazonSideAsn,
    transitGatewayOptions_transitGatewayCidrBlocks,
    transitGatewayOptions_multicastSupport,
    transitGatewayOptions_defaultRouteTableAssociation,
    transitGatewayOptions_defaultRouteTablePropagation,

    -- ** TransitGatewayPeeringAttachment
    transitGatewayPeeringAttachment_status,
    transitGatewayPeeringAttachment_creationTime,
    transitGatewayPeeringAttachment_requesterTgwInfo,
    transitGatewayPeeringAttachment_accepterTgwInfo,
    transitGatewayPeeringAttachment_state,
    transitGatewayPeeringAttachment_tags,
    transitGatewayPeeringAttachment_transitGatewayAttachmentId,

    -- ** TransitGatewayPrefixListAttachment
    transitGatewayPrefixListAttachment_resourceId,
    transitGatewayPrefixListAttachment_resourceType,
    transitGatewayPrefixListAttachment_transitGatewayAttachmentId,

    -- ** TransitGatewayPrefixListReference
    transitGatewayPrefixListReference_transitGatewayAttachment,
    transitGatewayPrefixListReference_prefixListOwnerId,
    transitGatewayPrefixListReference_prefixListId,
    transitGatewayPrefixListReference_state,
    transitGatewayPrefixListReference_blackhole,
    transitGatewayPrefixListReference_transitGatewayRouteTableId,

    -- ** TransitGatewayPropagation
    transitGatewayPropagation_resourceId,
    transitGatewayPropagation_resourceType,
    transitGatewayPropagation_state,
    transitGatewayPropagation_transitGatewayAttachmentId,
    transitGatewayPropagation_transitGatewayRouteTableId,

    -- ** TransitGatewayRequestOptions
    transitGatewayRequestOptions_vpnEcmpSupport,
    transitGatewayRequestOptions_dnsSupport,
    transitGatewayRequestOptions_autoAcceptSharedAttachments,
    transitGatewayRequestOptions_amazonSideAsn,
    transitGatewayRequestOptions_transitGatewayCidrBlocks,
    transitGatewayRequestOptions_multicastSupport,
    transitGatewayRequestOptions_defaultRouteTableAssociation,
    transitGatewayRequestOptions_defaultRouteTablePropagation,

    -- ** TransitGatewayRoute
    transitGatewayRoute_prefixListId,
    transitGatewayRoute_state,
    transitGatewayRoute_destinationCidrBlock,
    transitGatewayRoute_type,
    transitGatewayRoute_transitGatewayAttachments,

    -- ** TransitGatewayRouteAttachment
    transitGatewayRouteAttachment_resourceId,
    transitGatewayRouteAttachment_resourceType,
    transitGatewayRouteAttachment_transitGatewayAttachmentId,

    -- ** TransitGatewayRouteTable
    transitGatewayRouteTable_creationTime,
    transitGatewayRouteTable_defaultAssociationRouteTable,
    transitGatewayRouteTable_defaultPropagationRouteTable,
    transitGatewayRouteTable_state,
    transitGatewayRouteTable_tags,
    transitGatewayRouteTable_transitGatewayRouteTableId,
    transitGatewayRouteTable_transitGatewayId,

    -- ** TransitGatewayRouteTableAssociation
    transitGatewayRouteTableAssociation_resourceId,
    transitGatewayRouteTableAssociation_resourceType,
    transitGatewayRouteTableAssociation_state,
    transitGatewayRouteTableAssociation_transitGatewayAttachmentId,

    -- ** TransitGatewayRouteTablePropagation
    transitGatewayRouteTablePropagation_resourceId,
    transitGatewayRouteTablePropagation_resourceType,
    transitGatewayRouteTablePropagation_state,
    transitGatewayRouteTablePropagation_transitGatewayAttachmentId,

    -- ** TransitGatewayVpcAttachment
    transitGatewayVpcAttachment_creationTime,
    transitGatewayVpcAttachment_options,
    transitGatewayVpcAttachment_subnetIds,
    transitGatewayVpcAttachment_state,
    transitGatewayVpcAttachment_vpcOwnerId,
    transitGatewayVpcAttachment_tags,
    transitGatewayVpcAttachment_transitGatewayAttachmentId,
    transitGatewayVpcAttachment_vpcId,
    transitGatewayVpcAttachment_transitGatewayId,

    -- ** TransitGatewayVpcAttachmentOptions
    transitGatewayVpcAttachmentOptions_applianceModeSupport,
    transitGatewayVpcAttachmentOptions_dnsSupport,
    transitGatewayVpcAttachmentOptions_ipv6Support,

    -- ** TunnelOption
    tunnelOption_phase1LifetimeSeconds,
    tunnelOption_phase2DHGroupNumbers,
    tunnelOption_ikeVersions,
    tunnelOption_phase2EncryptionAlgorithms,
    tunnelOption_phase2IntegrityAlgorithms,
    tunnelOption_startupAction,
    tunnelOption_dpdTimeoutSeconds,
    tunnelOption_phase1DHGroupNumbers,
    tunnelOption_phase1EncryptionAlgorithms,
    tunnelOption_replayWindowSize,
    tunnelOption_outsideIpAddress,
    tunnelOption_preSharedKey,
    tunnelOption_tunnelInsideIpv6Cidr,
    tunnelOption_rekeyFuzzPercentage,
    tunnelOption_rekeyMarginTimeSeconds,
    tunnelOption_tunnelInsideCidr,
    tunnelOption_phase1IntegrityAlgorithms,
    tunnelOption_dpdTimeoutAction,
    tunnelOption_phase2LifetimeSeconds,

    -- ** UnsuccessfulInstanceCreditSpecificationItem
    unsuccessfulInstanceCreditSpecificationItem_instanceId,
    unsuccessfulInstanceCreditSpecificationItem_error,

    -- ** UnsuccessfulInstanceCreditSpecificationItemError
    unsuccessfulInstanceCreditSpecificationItemError_message,
    unsuccessfulInstanceCreditSpecificationItemError_code,

    -- ** UnsuccessfulItem
    unsuccessfulItem_resourceId,
    unsuccessfulItem_error,

    -- ** UnsuccessfulItemError
    unsuccessfulItemError_message,
    unsuccessfulItemError_code,

    -- ** UserBucket
    userBucket_s3Bucket,
    userBucket_s3Key,

    -- ** UserBucketDetails
    userBucketDetails_s3Bucket,
    userBucketDetails_s3Key,

    -- ** UserData
    userData_data,

    -- ** UserIdGroupPair
    userIdGroupPair_vpcPeeringConnectionId,
    userIdGroupPair_groupName,
    userIdGroupPair_groupId,
    userIdGroupPair_userId,
    userIdGroupPair_peeringStatus,
    userIdGroupPair_description,
    userIdGroupPair_vpcId,

    -- ** VCpuInfo
    vCpuInfo_defaultCores,
    vCpuInfo_defaultVCpus,
    vCpuInfo_validThreadsPerCore,
    vCpuInfo_validCores,
    vCpuInfo_defaultThreadsPerCore,

    -- ** ValidationError
    validationError_message,
    validationError_code,

    -- ** ValidationWarning
    validationWarning_errors,

    -- ** VgwTelemetry
    vgwTelemetry_statusMessage,
    vgwTelemetry_status,
    vgwTelemetry_acceptedRouteCount,
    vgwTelemetry_lastStatusChange,
    vgwTelemetry_certificateArn,
    vgwTelemetry_outsideIpAddress,

    -- ** Volume
    volume_multiAttachEnabled,
    volume_fastRestored,
    volume_outpostArn,
    volume_throughput,
    volume_kmsKeyId,
    volume_tags,
    volume_iops,
    volume_attachments,
    volume_availabilityZone,
    volume_createTime,
    volume_encrypted,
    volume_size,
    volume_snapshotId,
    volume_state,
    volume_volumeId,
    volume_volumeType,

    -- ** VolumeAttachment
    volumeAttachment_instanceId,
    volumeAttachment_attachTime,
    volumeAttachment_device,
    volumeAttachment_volumeId,
    volumeAttachment_state,
    volumeAttachment_deleteOnTermination,

    -- ** VolumeDetail
    volumeDetail_size,

    -- ** VolumeModification
    volumeModification_statusMessage,
    volumeModification_targetVolumeType,
    volumeModification_originalVolumeType,
    volumeModification_startTime,
    volumeModification_volumeId,
    volumeModification_endTime,
    volumeModification_originalIops,
    volumeModification_targetSize,
    volumeModification_originalSize,
    volumeModification_targetIops,
    volumeModification_modificationState,
    volumeModification_targetMultiAttachEnabled,
    volumeModification_originalMultiAttachEnabled,
    volumeModification_targetThroughput,
    volumeModification_originalThroughput,
    volumeModification_progress,

    -- ** VolumeStatusAction
    volumeStatusAction_eventType,
    volumeStatusAction_eventId,
    volumeStatusAction_code,
    volumeStatusAction_description,

    -- ** VolumeStatusAttachmentStatus
    volumeStatusAttachmentStatus_instanceId,
    volumeStatusAttachmentStatus_ioPerformance,

    -- ** VolumeStatusDetails
    volumeStatusDetails_status,
    volumeStatusDetails_name,

    -- ** VolumeStatusEvent
    volumeStatusEvent_notBefore,
    volumeStatusEvent_eventType,
    volumeStatusEvent_instanceId,
    volumeStatusEvent_eventId,
    volumeStatusEvent_notAfter,
    volumeStatusEvent_description,

    -- ** VolumeStatusInfo
    volumeStatusInfo_status,
    volumeStatusInfo_details,

    -- ** VolumeStatusItem
    volumeStatusItem_volumeStatus,
    volumeStatusItem_outpostArn,
    volumeStatusItem_volumeId,
    volumeStatusItem_actions,
    volumeStatusItem_events,
    volumeStatusItem_availabilityZone,
    volumeStatusItem_attachmentStatuses,

    -- ** Vpc
    vpc_ownerId,
    vpc_isDefault,
    vpc_cidrBlockAssociationSet,
    vpc_ipv6CidrBlockAssociationSet,
    vpc_tags,
    vpc_cidrBlock,
    vpc_dhcpOptionsId,
    vpc_instanceTenancy,
    vpc_state,
    vpc_vpcId,

    -- ** VpcAttachment
    vpcAttachment_state,
    vpcAttachment_vpcId,

    -- ** VpcCidrBlockAssociation
    vpcCidrBlockAssociation_cidrBlockState,
    vpcCidrBlockAssociation_associationId,
    vpcCidrBlockAssociation_cidrBlock,

    -- ** VpcCidrBlockState
    vpcCidrBlockState_statusMessage,
    vpcCidrBlockState_state,

    -- ** VpcClassicLink
    vpcClassicLink_tags,
    vpcClassicLink_classicLinkEnabled,
    vpcClassicLink_vpcId,

    -- ** VpcEndpoint
    vpcEndpoint_creationTimestamp,
    vpcEndpoint_policyDocument,
    vpcEndpoint_groups,
    vpcEndpoint_ownerId,
    vpcEndpoint_routeTableIds,
    vpcEndpoint_vpcEndpointType,
    vpcEndpoint_requesterManaged,
    vpcEndpoint_dnsEntries,
    vpcEndpoint_vpcEndpointId,
    vpcEndpoint_subnetIds,
    vpcEndpoint_networkInterfaceIds,
    vpcEndpoint_serviceName,
    vpcEndpoint_lastError,
    vpcEndpoint_state,
    vpcEndpoint_privateDnsEnabled,
    vpcEndpoint_tags,
    vpcEndpoint_vpcId,

    -- ** VpcEndpointConnection
    vpcEndpointConnection_creationTimestamp,
    vpcEndpointConnection_vpcEndpointOwner,
    vpcEndpointConnection_gatewayLoadBalancerArns,
    vpcEndpointConnection_vpcEndpointState,
    vpcEndpointConnection_dnsEntries,
    vpcEndpointConnection_vpcEndpointId,
    vpcEndpointConnection_serviceId,
    vpcEndpointConnection_networkLoadBalancerArns,

    -- ** VpcIpv6CidrBlockAssociation
    vpcIpv6CidrBlockAssociation_ipv6CidrBlockState,
    vpcIpv6CidrBlockAssociation_ipv6Pool,
    vpcIpv6CidrBlockAssociation_associationId,
    vpcIpv6CidrBlockAssociation_ipv6CidrBlock,
    vpcIpv6CidrBlockAssociation_networkBorderGroup,

    -- ** VpcPeeringConnection
    vpcPeeringConnection_status,
    vpcPeeringConnection_accepterVpcInfo,
    vpcPeeringConnection_vpcPeeringConnectionId,
    vpcPeeringConnection_expirationTime,
    vpcPeeringConnection_requesterVpcInfo,
    vpcPeeringConnection_tags,

    -- ** VpcPeeringConnectionOptionsDescription
    vpcPeeringConnectionOptionsDescription_allowDnsResolutionFromRemoteVpc,
    vpcPeeringConnectionOptionsDescription_allowEgressFromLocalVpcToRemoteClassicLink,
    vpcPeeringConnectionOptionsDescription_allowEgressFromLocalClassicLinkToRemoteVpc,

    -- ** VpcPeeringConnectionStateReason
    vpcPeeringConnectionStateReason_message,
    vpcPeeringConnectionStateReason_code,

    -- ** VpcPeeringConnectionVpcInfo
    vpcPeeringConnectionVpcInfo_cidrBlockSet,
    vpcPeeringConnectionVpcInfo_ownerId,
    vpcPeeringConnectionVpcInfo_ipv6CidrBlockSet,
    vpcPeeringConnectionVpcInfo_region,
    vpcPeeringConnectionVpcInfo_vpcId,
    vpcPeeringConnectionVpcInfo_cidrBlock,
    vpcPeeringConnectionVpcInfo_peeringOptions,

    -- ** VpnConnection
    vpnConnection_customerGatewayConfiguration,
    vpnConnection_options,
    vpnConnection_category,
    vpnConnection_routes,
    vpnConnection_tags,
    vpnConnection_vgwTelemetry,
    vpnConnection_vpnGatewayId,
    vpnConnection_transitGatewayId,
    vpnConnection_vpnConnectionId,
    vpnConnection_customerGatewayId,
    vpnConnection_state,
    vpnConnection_type,

    -- ** VpnConnectionOptions
    vpnConnectionOptions_remoteIpv6NetworkCidr,
    vpnConnectionOptions_staticRoutesOnly,
    vpnConnectionOptions_localIpv6NetworkCidr,
    vpnConnectionOptions_enableAcceleration,
    vpnConnectionOptions_tunnelOptions,
    vpnConnectionOptions_remoteIpv4NetworkCidr,
    vpnConnectionOptions_tunnelInsideIpVersion,
    vpnConnectionOptions_localIpv4NetworkCidr,

    -- ** VpnConnectionOptionsSpecification
    vpnConnectionOptionsSpecification_remoteIpv6NetworkCidr,
    vpnConnectionOptionsSpecification_staticRoutesOnly,
    vpnConnectionOptionsSpecification_localIpv6NetworkCidr,
    vpnConnectionOptionsSpecification_enableAcceleration,
    vpnConnectionOptionsSpecification_tunnelOptions,
    vpnConnectionOptionsSpecification_remoteIpv4NetworkCidr,
    vpnConnectionOptionsSpecification_tunnelInsideIpVersion,
    vpnConnectionOptionsSpecification_localIpv4NetworkCidr,

    -- ** VpnGateway
    vpnGateway_vpcAttachments,
    vpnGateway_state,
    vpnGateway_availabilityZone,
    vpnGateway_amazonSideAsn,
    vpnGateway_tags,
    vpnGateway_type,
    vpnGateway_vpnGatewayId,

    -- ** VpnStaticRoute
    vpnStaticRoute_source,
    vpnStaticRoute_state,
    vpnStaticRoute_destinationCidrBlock,

    -- ** VpnTunnelOptionsSpecification
    vpnTunnelOptionsSpecification_phase1LifetimeSeconds,
    vpnTunnelOptionsSpecification_phase2DHGroupNumbers,
    vpnTunnelOptionsSpecification_iKEVersions,
    vpnTunnelOptionsSpecification_phase2EncryptionAlgorithms,
    vpnTunnelOptionsSpecification_phase2IntegrityAlgorithms,
    vpnTunnelOptionsSpecification_startupAction,
    vpnTunnelOptionsSpecification_dPDTimeoutSeconds,
    vpnTunnelOptionsSpecification_phase1DHGroupNumbers,
    vpnTunnelOptionsSpecification_phase1EncryptionAlgorithms,
    vpnTunnelOptionsSpecification_replayWindowSize,
    vpnTunnelOptionsSpecification_preSharedKey,
    vpnTunnelOptionsSpecification_tunnelInsideIpv6Cidr,
    vpnTunnelOptionsSpecification_rekeyFuzzPercentage,
    vpnTunnelOptionsSpecification_rekeyMarginTimeSeconds,
    vpnTunnelOptionsSpecification_tunnelInsideCidr,
    vpnTunnelOptionsSpecification_phase1IntegrityAlgorithms,
    vpnTunnelOptionsSpecification_dPDTimeoutAction,
    vpnTunnelOptionsSpecification_phase2LifetimeSeconds,
  )
where

import Network.AWS.EC2.AcceptReservedInstancesExchangeQuote
import Network.AWS.EC2.AcceptTransitGatewayMulticastDomainAssociations
import Network.AWS.EC2.AcceptTransitGatewayPeeringAttachment
import Network.AWS.EC2.AcceptTransitGatewayVpcAttachment
import Network.AWS.EC2.AcceptVpcEndpointConnections
import Network.AWS.EC2.AcceptVpcPeeringConnection
import Network.AWS.EC2.AdvertiseByoipCidr
import Network.AWS.EC2.AllocateAddress
import Network.AWS.EC2.AllocateHosts
import Network.AWS.EC2.ApplySecurityGroupsToClientVpnTargetNetwork
import Network.AWS.EC2.AssignIpv6Addresses
import Network.AWS.EC2.AssignPrivateIpAddresses
import Network.AWS.EC2.AssociateAddress
import Network.AWS.EC2.AssociateClientVpnTargetNetwork
import Network.AWS.EC2.AssociateDhcpOptions
import Network.AWS.EC2.AssociateEnclaveCertificateIamRole
import Network.AWS.EC2.AssociateIamInstanceProfile
import Network.AWS.EC2.AssociateRouteTable
import Network.AWS.EC2.AssociateSubnetCidrBlock
import Network.AWS.EC2.AssociateTransitGatewayMulticastDomain
import Network.AWS.EC2.AssociateTransitGatewayRouteTable
import Network.AWS.EC2.AssociateVpcCidrBlock
import Network.AWS.EC2.AttachClassicLinkVpc
import Network.AWS.EC2.AttachInternetGateway
import Network.AWS.EC2.AttachNetworkInterface
import Network.AWS.EC2.AttachVolume
import Network.AWS.EC2.AttachVpnGateway
import Network.AWS.EC2.AuthorizeClientVpnIngress
import Network.AWS.EC2.AuthorizeSecurityGroupEgress
import Network.AWS.EC2.AuthorizeSecurityGroupIngress
import Network.AWS.EC2.BundleInstance
import Network.AWS.EC2.CancelBundleTask
import Network.AWS.EC2.CancelCapacityReservation
import Network.AWS.EC2.CancelConversionTask
import Network.AWS.EC2.CancelExportTask
import Network.AWS.EC2.CancelImportTask
import Network.AWS.EC2.CancelReservedInstancesListing
import Network.AWS.EC2.CancelSpotFleetRequests
import Network.AWS.EC2.CancelSpotInstanceRequests
import Network.AWS.EC2.ConfirmProductInstance
import Network.AWS.EC2.CopyFpgaImage
import Network.AWS.EC2.CopyImage
import Network.AWS.EC2.CopySnapshot
import Network.AWS.EC2.CreateCapacityReservation
import Network.AWS.EC2.CreateCarrierGateway
import Network.AWS.EC2.CreateClientVpnEndpoint
import Network.AWS.EC2.CreateClientVpnRoute
import Network.AWS.EC2.CreateCustomerGateway
import Network.AWS.EC2.CreateDefaultSubnet
import Network.AWS.EC2.CreateDefaultVpc
import Network.AWS.EC2.CreateDhcpOptions
import Network.AWS.EC2.CreateEgressOnlyInternetGateway
import Network.AWS.EC2.CreateFleet
import Network.AWS.EC2.CreateFlowLogs
import Network.AWS.EC2.CreateFpgaImage
import Network.AWS.EC2.CreateImage
import Network.AWS.EC2.CreateInstanceExportTask
import Network.AWS.EC2.CreateInternetGateway
import Network.AWS.EC2.CreateKeyPair
import Network.AWS.EC2.CreateLaunchTemplate
import Network.AWS.EC2.CreateLaunchTemplateVersion
import Network.AWS.EC2.CreateLocalGatewayRoute
import Network.AWS.EC2.CreateLocalGatewayRouteTableVpcAssociation
import Network.AWS.EC2.CreateManagedPrefixList
import Network.AWS.EC2.CreateNatGateway
import Network.AWS.EC2.CreateNetworkAcl
import Network.AWS.EC2.CreateNetworkAclEntry
import Network.AWS.EC2.CreateNetworkInsightsPath
import Network.AWS.EC2.CreateNetworkInterface
import Network.AWS.EC2.CreateNetworkInterfacePermission
import Network.AWS.EC2.CreatePlacementGroup
import Network.AWS.EC2.CreateReservedInstancesListing
import Network.AWS.EC2.CreateRoute
import Network.AWS.EC2.CreateRouteTable
import Network.AWS.EC2.CreateSecurityGroup
import Network.AWS.EC2.CreateSnapshot
import Network.AWS.EC2.CreateSnapshots
import Network.AWS.EC2.CreateSpotDatafeedSubscription
import Network.AWS.EC2.CreateSubnet
import Network.AWS.EC2.CreateTags
import Network.AWS.EC2.CreateTrafficMirrorFilter
import Network.AWS.EC2.CreateTrafficMirrorFilterRule
import Network.AWS.EC2.CreateTrafficMirrorSession
import Network.AWS.EC2.CreateTrafficMirrorTarget
import Network.AWS.EC2.CreateTransitGateway
import Network.AWS.EC2.CreateTransitGatewayConnect
import Network.AWS.EC2.CreateTransitGatewayConnectPeer
import Network.AWS.EC2.CreateTransitGatewayMulticastDomain
import Network.AWS.EC2.CreateTransitGatewayPeeringAttachment
import Network.AWS.EC2.CreateTransitGatewayPrefixListReference
import Network.AWS.EC2.CreateTransitGatewayRoute
import Network.AWS.EC2.CreateTransitGatewayRouteTable
import Network.AWS.EC2.CreateTransitGatewayVpcAttachment
import Network.AWS.EC2.CreateVolume
import Network.AWS.EC2.CreateVpc
import Network.AWS.EC2.CreateVpcEndpoint
import Network.AWS.EC2.CreateVpcEndpointConnectionNotification
import Network.AWS.EC2.CreateVpcEndpointServiceConfiguration
import Network.AWS.EC2.CreateVpcPeeringConnection
import Network.AWS.EC2.CreateVpnConnection
import Network.AWS.EC2.CreateVpnConnectionRoute
import Network.AWS.EC2.CreateVpnGateway
import Network.AWS.EC2.DeleteCarrierGateway
import Network.AWS.EC2.DeleteClientVpnEndpoint
import Network.AWS.EC2.DeleteClientVpnRoute
import Network.AWS.EC2.DeleteCustomerGateway
import Network.AWS.EC2.DeleteDhcpOptions
import Network.AWS.EC2.DeleteEgressOnlyInternetGateway
import Network.AWS.EC2.DeleteFleets
import Network.AWS.EC2.DeleteFlowLogs
import Network.AWS.EC2.DeleteFpgaImage
import Network.AWS.EC2.DeleteInternetGateway
import Network.AWS.EC2.DeleteKeyPair
import Network.AWS.EC2.DeleteLaunchTemplate
import Network.AWS.EC2.DeleteLaunchTemplateVersions
import Network.AWS.EC2.DeleteLocalGatewayRoute
import Network.AWS.EC2.DeleteLocalGatewayRouteTableVpcAssociation
import Network.AWS.EC2.DeleteManagedPrefixList
import Network.AWS.EC2.DeleteNatGateway
import Network.AWS.EC2.DeleteNetworkAcl
import Network.AWS.EC2.DeleteNetworkAclEntry
import Network.AWS.EC2.DeleteNetworkInsightsAnalysis
import Network.AWS.EC2.DeleteNetworkInsightsPath
import Network.AWS.EC2.DeleteNetworkInterface
import Network.AWS.EC2.DeleteNetworkInterfacePermission
import Network.AWS.EC2.DeletePlacementGroup
import Network.AWS.EC2.DeleteQueuedReservedInstances
import Network.AWS.EC2.DeleteRoute
import Network.AWS.EC2.DeleteRouteTable
import Network.AWS.EC2.DeleteSecurityGroup
import Network.AWS.EC2.DeleteSnapshot
import Network.AWS.EC2.DeleteSpotDatafeedSubscription
import Network.AWS.EC2.DeleteSubnet
import Network.AWS.EC2.DeleteTags
import Network.AWS.EC2.DeleteTrafficMirrorFilter
import Network.AWS.EC2.DeleteTrafficMirrorFilterRule
import Network.AWS.EC2.DeleteTrafficMirrorSession
import Network.AWS.EC2.DeleteTrafficMirrorTarget
import Network.AWS.EC2.DeleteTransitGateway
import Network.AWS.EC2.DeleteTransitGatewayConnect
import Network.AWS.EC2.DeleteTransitGatewayConnectPeer
import Network.AWS.EC2.DeleteTransitGatewayMulticastDomain
import Network.AWS.EC2.DeleteTransitGatewayPeeringAttachment
import Network.AWS.EC2.DeleteTransitGatewayPrefixListReference
import Network.AWS.EC2.DeleteTransitGatewayRoute
import Network.AWS.EC2.DeleteTransitGatewayRouteTable
import Network.AWS.EC2.DeleteTransitGatewayVpcAttachment
import Network.AWS.EC2.DeleteVolume
import Network.AWS.EC2.DeleteVpc
import Network.AWS.EC2.DeleteVpcEndpointConnectionNotifications
import Network.AWS.EC2.DeleteVpcEndpointServiceConfigurations
import Network.AWS.EC2.DeleteVpcEndpoints
import Network.AWS.EC2.DeleteVpcPeeringConnection
import Network.AWS.EC2.DeleteVpnConnection
import Network.AWS.EC2.DeleteVpnConnectionRoute
import Network.AWS.EC2.DeleteVpnGateway
import Network.AWS.EC2.DeprovisionByoipCidr
import Network.AWS.EC2.DeregisterImage
import Network.AWS.EC2.DeregisterInstanceEventNotificationAttributes
import Network.AWS.EC2.DeregisterTransitGatewayMulticastGroupMembers
import Network.AWS.EC2.DeregisterTransitGatewayMulticastGroupSources
import Network.AWS.EC2.DescribeAccountAttributes
import Network.AWS.EC2.DescribeAddresses
import Network.AWS.EC2.DescribeAddressesAttribute
import Network.AWS.EC2.DescribeAggregateIdFormat
import Network.AWS.EC2.DescribeAvailabilityZones
import Network.AWS.EC2.DescribeBundleTasks
import Network.AWS.EC2.DescribeByoipCidrs
import Network.AWS.EC2.DescribeCapacityReservations
import Network.AWS.EC2.DescribeCarrierGateways
import Network.AWS.EC2.DescribeClassicLinkInstances
import Network.AWS.EC2.DescribeClientVpnAuthorizationRules
import Network.AWS.EC2.DescribeClientVpnConnections
import Network.AWS.EC2.DescribeClientVpnEndpoints
import Network.AWS.EC2.DescribeClientVpnRoutes
import Network.AWS.EC2.DescribeClientVpnTargetNetworks
import Network.AWS.EC2.DescribeCoipPools
import Network.AWS.EC2.DescribeConversionTasks
import Network.AWS.EC2.DescribeCustomerGateways
import Network.AWS.EC2.DescribeDhcpOptions
import Network.AWS.EC2.DescribeEgressOnlyInternetGateways
import Network.AWS.EC2.DescribeElasticGpus
import Network.AWS.EC2.DescribeExportImageTasks
import Network.AWS.EC2.DescribeExportTasks
import Network.AWS.EC2.DescribeFastSnapshotRestores
import Network.AWS.EC2.DescribeFleetHistory
import Network.AWS.EC2.DescribeFleetInstances
import Network.AWS.EC2.DescribeFleets
import Network.AWS.EC2.DescribeFlowLogs
import Network.AWS.EC2.DescribeFpgaImageAttribute
import Network.AWS.EC2.DescribeFpgaImages
import Network.AWS.EC2.DescribeHostReservationOfferings
import Network.AWS.EC2.DescribeHostReservations
import Network.AWS.EC2.DescribeHosts
import Network.AWS.EC2.DescribeIamInstanceProfileAssociations
import Network.AWS.EC2.DescribeIdFormat
import Network.AWS.EC2.DescribeIdentityIdFormat
import Network.AWS.EC2.DescribeImageAttribute
import Network.AWS.EC2.DescribeImages
import Network.AWS.EC2.DescribeImportImageTasks
import Network.AWS.EC2.DescribeImportSnapshotTasks
import Network.AWS.EC2.DescribeInstanceAttribute
import Network.AWS.EC2.DescribeInstanceCreditSpecifications
import Network.AWS.EC2.DescribeInstanceEventNotificationAttributes
import Network.AWS.EC2.DescribeInstanceStatus
import Network.AWS.EC2.DescribeInstanceTypeOfferings
import Network.AWS.EC2.DescribeInstanceTypes
import Network.AWS.EC2.DescribeInstances
import Network.AWS.EC2.DescribeInternetGateways
import Network.AWS.EC2.DescribeIpv6Pools
import Network.AWS.EC2.DescribeKeyPairs
import Network.AWS.EC2.DescribeLaunchTemplateVersions
import Network.AWS.EC2.DescribeLaunchTemplates
import Network.AWS.EC2.DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
import Network.AWS.EC2.DescribeLocalGatewayRouteTableVpcAssociations
import Network.AWS.EC2.DescribeLocalGatewayRouteTables
import Network.AWS.EC2.DescribeLocalGatewayVirtualInterfaceGroups
import Network.AWS.EC2.DescribeLocalGatewayVirtualInterfaces
import Network.AWS.EC2.DescribeLocalGateways
import Network.AWS.EC2.DescribeManagedPrefixLists
import Network.AWS.EC2.DescribeMovingAddresses
import Network.AWS.EC2.DescribeNatGateways
import Network.AWS.EC2.DescribeNetworkAcls
import Network.AWS.EC2.DescribeNetworkInsightsAnalyses
import Network.AWS.EC2.DescribeNetworkInsightsPaths
import Network.AWS.EC2.DescribeNetworkInterfaceAttribute
import Network.AWS.EC2.DescribeNetworkInterfacePermissions
import Network.AWS.EC2.DescribeNetworkInterfaces
import Network.AWS.EC2.DescribePlacementGroups
import Network.AWS.EC2.DescribePrefixLists
import Network.AWS.EC2.DescribePrincipalIdFormat
import Network.AWS.EC2.DescribePublicIpv4Pools
import Network.AWS.EC2.DescribeRegions
import Network.AWS.EC2.DescribeReservedInstances
import Network.AWS.EC2.DescribeReservedInstancesListings
import Network.AWS.EC2.DescribeReservedInstancesModifications
import Network.AWS.EC2.DescribeReservedInstancesOfferings
import Network.AWS.EC2.DescribeRouteTables
import Network.AWS.EC2.DescribeScheduledInstanceAvailability
import Network.AWS.EC2.DescribeScheduledInstances
import Network.AWS.EC2.DescribeSecurityGroupReferences
import Network.AWS.EC2.DescribeSecurityGroups
import Network.AWS.EC2.DescribeSnapshotAttribute
import Network.AWS.EC2.DescribeSnapshots
import Network.AWS.EC2.DescribeSpotDatafeedSubscription
import Network.AWS.EC2.DescribeSpotFleetInstances
import Network.AWS.EC2.DescribeSpotFleetRequestHistory
import Network.AWS.EC2.DescribeSpotFleetRequests
import Network.AWS.EC2.DescribeSpotInstanceRequests
import Network.AWS.EC2.DescribeSpotPriceHistory
import Network.AWS.EC2.DescribeStaleSecurityGroups
import Network.AWS.EC2.DescribeSubnets
import Network.AWS.EC2.DescribeTags
import Network.AWS.EC2.DescribeTrafficMirrorFilters
import Network.AWS.EC2.DescribeTrafficMirrorSessions
import Network.AWS.EC2.DescribeTrafficMirrorTargets
import Network.AWS.EC2.DescribeTransitGatewayAttachments
import Network.AWS.EC2.DescribeTransitGatewayConnectPeers
import Network.AWS.EC2.DescribeTransitGatewayConnects
import Network.AWS.EC2.DescribeTransitGatewayMulticastDomains
import Network.AWS.EC2.DescribeTransitGatewayPeeringAttachments
import Network.AWS.EC2.DescribeTransitGatewayRouteTables
import Network.AWS.EC2.DescribeTransitGatewayVpcAttachments
import Network.AWS.EC2.DescribeTransitGateways
import Network.AWS.EC2.DescribeVolumeAttribute
import Network.AWS.EC2.DescribeVolumeStatus
import Network.AWS.EC2.DescribeVolumes
import Network.AWS.EC2.DescribeVolumesModifications
import Network.AWS.EC2.DescribeVpcAttribute
import Network.AWS.EC2.DescribeVpcClassicLink
import Network.AWS.EC2.DescribeVpcClassicLinkDnsSupport
import Network.AWS.EC2.DescribeVpcEndpointConnectionNotifications
import Network.AWS.EC2.DescribeVpcEndpointConnections
import Network.AWS.EC2.DescribeVpcEndpointServiceConfigurations
import Network.AWS.EC2.DescribeVpcEndpointServicePermissions
import Network.AWS.EC2.DescribeVpcEndpointServices
import Network.AWS.EC2.DescribeVpcEndpoints
import Network.AWS.EC2.DescribeVpcPeeringConnections
import Network.AWS.EC2.DescribeVpcs
import Network.AWS.EC2.DescribeVpnConnections
import Network.AWS.EC2.DescribeVpnGateways
import Network.AWS.EC2.DetachClassicLinkVpc
import Network.AWS.EC2.DetachInternetGateway
import Network.AWS.EC2.DetachNetworkInterface
import Network.AWS.EC2.DetachVolume
import Network.AWS.EC2.DetachVpnGateway
import Network.AWS.EC2.DisableEbsEncryptionByDefault
import Network.AWS.EC2.DisableFastSnapshotRestores
import Network.AWS.EC2.DisableTransitGatewayRouteTablePropagation
import Network.AWS.EC2.DisableVgwRoutePropagation
import Network.AWS.EC2.DisableVpcClassicLink
import Network.AWS.EC2.DisableVpcClassicLinkDnsSupport
import Network.AWS.EC2.DisassociateAddress
import Network.AWS.EC2.DisassociateClientVpnTargetNetwork
import Network.AWS.EC2.DisassociateEnclaveCertificateIamRole
import Network.AWS.EC2.DisassociateIamInstanceProfile
import Network.AWS.EC2.DisassociateRouteTable
import Network.AWS.EC2.DisassociateSubnetCidrBlock
import Network.AWS.EC2.DisassociateTransitGatewayMulticastDomain
import Network.AWS.EC2.DisassociateTransitGatewayRouteTable
import Network.AWS.EC2.DisassociateVpcCidrBlock
import Network.AWS.EC2.EnableEbsEncryptionByDefault
import Network.AWS.EC2.EnableFastSnapshotRestores
import Network.AWS.EC2.EnableTransitGatewayRouteTablePropagation
import Network.AWS.EC2.EnableVgwRoutePropagation
import Network.AWS.EC2.EnableVolumeIO
import Network.AWS.EC2.EnableVpcClassicLink
import Network.AWS.EC2.EnableVpcClassicLinkDnsSupport
import Network.AWS.EC2.ExportClientVpnClientCertificateRevocationList
import Network.AWS.EC2.ExportClientVpnClientConfiguration
import Network.AWS.EC2.ExportImage
import Network.AWS.EC2.ExportTransitGatewayRoutes
import Network.AWS.EC2.GetAssociatedEnclaveCertificateIamRoles
import Network.AWS.EC2.GetAssociatedIpv6PoolCidrs
import Network.AWS.EC2.GetCapacityReservationUsage
import Network.AWS.EC2.GetCoipPoolUsage
import Network.AWS.EC2.GetConsoleOutput
import Network.AWS.EC2.GetConsoleScreenshot
import Network.AWS.EC2.GetDefaultCreditSpecification
import Network.AWS.EC2.GetEbsDefaultKmsKeyId
import Network.AWS.EC2.GetEbsEncryptionByDefault
import Network.AWS.EC2.GetGroupsForCapacityReservation
import Network.AWS.EC2.GetHostReservationPurchasePreview
import Network.AWS.EC2.GetLaunchTemplateData
import Network.AWS.EC2.GetManagedPrefixListAssociations
import Network.AWS.EC2.GetManagedPrefixListEntries
import Network.AWS.EC2.GetPasswordData
import Network.AWS.EC2.GetReservedInstancesExchangeQuote
import Network.AWS.EC2.GetTransitGatewayAttachmentPropagations
import Network.AWS.EC2.GetTransitGatewayMulticastDomainAssociations
import Network.AWS.EC2.GetTransitGatewayPrefixListReferences
import Network.AWS.EC2.GetTransitGatewayRouteTableAssociations
import Network.AWS.EC2.GetTransitGatewayRouteTablePropagations
import Network.AWS.EC2.ImportClientVpnClientCertificateRevocationList
import Network.AWS.EC2.ImportImage
import Network.AWS.EC2.ImportInstance
import Network.AWS.EC2.ImportKeyPair
import Network.AWS.EC2.ImportSnapshot
import Network.AWS.EC2.ImportVolume
import Network.AWS.EC2.ModifyAddressAttribute
import Network.AWS.EC2.ModifyAvailabilityZoneGroup
import Network.AWS.EC2.ModifyCapacityReservation
import Network.AWS.EC2.ModifyClientVpnEndpoint
import Network.AWS.EC2.ModifyDefaultCreditSpecification
import Network.AWS.EC2.ModifyEbsDefaultKmsKeyId
import Network.AWS.EC2.ModifyFleet
import Network.AWS.EC2.ModifyFpgaImageAttribute
import Network.AWS.EC2.ModifyHosts
import Network.AWS.EC2.ModifyIdFormat
import Network.AWS.EC2.ModifyIdentityIdFormat
import Network.AWS.EC2.ModifyImageAttribute
import Network.AWS.EC2.ModifyInstanceAttribute
import Network.AWS.EC2.ModifyInstanceCapacityReservationAttributes
import Network.AWS.EC2.ModifyInstanceCreditSpecification
import Network.AWS.EC2.ModifyInstanceEventStartTime
import Network.AWS.EC2.ModifyInstanceMetadataOptions
import Network.AWS.EC2.ModifyInstancePlacement
import Network.AWS.EC2.ModifyLaunchTemplate
import Network.AWS.EC2.ModifyManagedPrefixList
import Network.AWS.EC2.ModifyNetworkInterfaceAttribute
import Network.AWS.EC2.ModifyReservedInstances
import Network.AWS.EC2.ModifySnapshotAttribute
import Network.AWS.EC2.ModifySpotFleetRequest
import Network.AWS.EC2.ModifySubnetAttribute
import Network.AWS.EC2.ModifyTrafficMirrorFilterNetworkServices
import Network.AWS.EC2.ModifyTrafficMirrorFilterRule
import Network.AWS.EC2.ModifyTrafficMirrorSession
import Network.AWS.EC2.ModifyTransitGateway
import Network.AWS.EC2.ModifyTransitGatewayPrefixListReference
import Network.AWS.EC2.ModifyTransitGatewayVpcAttachment
import Network.AWS.EC2.ModifyVolume
import Network.AWS.EC2.ModifyVolumeAttribute
import Network.AWS.EC2.ModifyVpcAttribute
import Network.AWS.EC2.ModifyVpcEndpoint
import Network.AWS.EC2.ModifyVpcEndpointConnectionNotification
import Network.AWS.EC2.ModifyVpcEndpointServiceConfiguration
import Network.AWS.EC2.ModifyVpcEndpointServicePermissions
import Network.AWS.EC2.ModifyVpcPeeringConnectionOptions
import Network.AWS.EC2.ModifyVpcTenancy
import Network.AWS.EC2.ModifyVpnConnection
import Network.AWS.EC2.ModifyVpnConnectionOptions
import Network.AWS.EC2.ModifyVpnTunnelCertificate
import Network.AWS.EC2.ModifyVpnTunnelOptions
import Network.AWS.EC2.MonitorInstances
import Network.AWS.EC2.MoveAddressToVpc
import Network.AWS.EC2.ProvisionByoipCidr
import Network.AWS.EC2.PurchaseHostReservation
import Network.AWS.EC2.PurchaseReservedInstancesOffering
import Network.AWS.EC2.PurchaseScheduledInstances
import Network.AWS.EC2.RebootInstances
import Network.AWS.EC2.RegisterImage
import Network.AWS.EC2.RegisterInstanceEventNotificationAttributes
import Network.AWS.EC2.RegisterTransitGatewayMulticastGroupMembers
import Network.AWS.EC2.RegisterTransitGatewayMulticastGroupSources
import Network.AWS.EC2.RejectTransitGatewayMulticastDomainAssociations
import Network.AWS.EC2.RejectTransitGatewayPeeringAttachment
import Network.AWS.EC2.RejectTransitGatewayVpcAttachment
import Network.AWS.EC2.RejectVpcEndpointConnections
import Network.AWS.EC2.RejectVpcPeeringConnection
import Network.AWS.EC2.ReleaseAddress
import Network.AWS.EC2.ReleaseHosts
import Network.AWS.EC2.ReplaceIamInstanceProfileAssociation
import Network.AWS.EC2.ReplaceNetworkAclAssociation
import Network.AWS.EC2.ReplaceNetworkAclEntry
import Network.AWS.EC2.ReplaceRoute
import Network.AWS.EC2.ReplaceRouteTableAssociation
import Network.AWS.EC2.ReplaceTransitGatewayRoute
import Network.AWS.EC2.ReportInstanceStatus
import Network.AWS.EC2.RequestSpotFleet
import Network.AWS.EC2.RequestSpotInstances
import Network.AWS.EC2.ResetAddressAttribute
import Network.AWS.EC2.ResetEbsDefaultKmsKeyId
import Network.AWS.EC2.ResetFpgaImageAttribute
import Network.AWS.EC2.ResetImageAttribute
import Network.AWS.EC2.ResetInstanceAttribute
import Network.AWS.EC2.ResetNetworkInterfaceAttribute
import Network.AWS.EC2.ResetSnapshotAttribute
import Network.AWS.EC2.RestoreAddressToClassic
import Network.AWS.EC2.RestoreManagedPrefixListVersion
import Network.AWS.EC2.RevokeClientVpnIngress
import Network.AWS.EC2.RevokeSecurityGroupEgress
import Network.AWS.EC2.RevokeSecurityGroupIngress
import Network.AWS.EC2.RunInstances
import Network.AWS.EC2.RunScheduledInstances
import Network.AWS.EC2.SearchLocalGatewayRoutes
import Network.AWS.EC2.SearchTransitGatewayMulticastGroups
import Network.AWS.EC2.SearchTransitGatewayRoutes
import Network.AWS.EC2.SendDiagnosticInterrupt
import Network.AWS.EC2.StartInstances
import Network.AWS.EC2.StartNetworkInsightsAnalysis
import Network.AWS.EC2.StartVpcEndpointServicePrivateDnsVerification
import Network.AWS.EC2.StopInstances
import Network.AWS.EC2.TerminateClientVpnConnections
import Network.AWS.EC2.TerminateInstances
import Network.AWS.EC2.Types.AccountAttribute
import Network.AWS.EC2.Types.AccountAttributeValue
import Network.AWS.EC2.Types.ActiveInstance
import Network.AWS.EC2.Types.AddPrefixListEntry
import Network.AWS.EC2.Types.Address
import Network.AWS.EC2.Types.AddressAttribute
import Network.AWS.EC2.Types.AllowedPrincipal
import Network.AWS.EC2.Types.AlternatePathHint
import Network.AWS.EC2.Types.AnalysisAclRule
import Network.AWS.EC2.Types.AnalysisComponent
import Network.AWS.EC2.Types.AnalysisLoadBalancerListener
import Network.AWS.EC2.Types.AnalysisLoadBalancerTarget
import Network.AWS.EC2.Types.AnalysisPacketHeader
import Network.AWS.EC2.Types.AnalysisRouteTableRoute
import Network.AWS.EC2.Types.AnalysisSecurityGroupRule
import Network.AWS.EC2.Types.AssignedPrivateIpAddress
import Network.AWS.EC2.Types.AssociatedRole
import Network.AWS.EC2.Types.AssociatedTargetNetwork
import Network.AWS.EC2.Types.AssociationStatus
import Network.AWS.EC2.Types.AttributeBooleanValue
import Network.AWS.EC2.Types.AttributeValue
import Network.AWS.EC2.Types.AuthorizationRule
import Network.AWS.EC2.Types.AvailabilityZone
import Network.AWS.EC2.Types.AvailabilityZoneMessage
import Network.AWS.EC2.Types.AvailableCapacity
import Network.AWS.EC2.Types.BlobAttributeValue
import Network.AWS.EC2.Types.BlockDeviceMapping
import Network.AWS.EC2.Types.BundleTask
import Network.AWS.EC2.Types.BundleTaskError
import Network.AWS.EC2.Types.ByoipCidr
import Network.AWS.EC2.Types.CancelSpotFleetRequestsError
import Network.AWS.EC2.Types.CancelSpotFleetRequestsErrorItem
import Network.AWS.EC2.Types.CancelSpotFleetRequestsSuccessItem
import Network.AWS.EC2.Types.CancelledSpotInstanceRequest
import Network.AWS.EC2.Types.CapacityReservation
import Network.AWS.EC2.Types.CapacityReservationGroup
import Network.AWS.EC2.Types.CapacityReservationOptions
import Network.AWS.EC2.Types.CapacityReservationOptionsRequest
import Network.AWS.EC2.Types.CapacityReservationSpecification
import Network.AWS.EC2.Types.CapacityReservationSpecificationResponse
import Network.AWS.EC2.Types.CapacityReservationTarget
import Network.AWS.EC2.Types.CapacityReservationTargetResponse
import Network.AWS.EC2.Types.CarrierGateway
import Network.AWS.EC2.Types.CertificateAuthentication
import Network.AWS.EC2.Types.CertificateAuthenticationRequest
import Network.AWS.EC2.Types.CidrAuthorizationContext
import Network.AWS.EC2.Types.CidrBlock
import Network.AWS.EC2.Types.ClassicLinkDnsSupport
import Network.AWS.EC2.Types.ClassicLinkInstance
import Network.AWS.EC2.Types.ClassicLoadBalancer
import Network.AWS.EC2.Types.ClassicLoadBalancersConfig
import Network.AWS.EC2.Types.ClientCertificateRevocationListStatus
import Network.AWS.EC2.Types.ClientConnectOptions
import Network.AWS.EC2.Types.ClientConnectResponseOptions
import Network.AWS.EC2.Types.ClientData
import Network.AWS.EC2.Types.ClientVpnAuthentication
import Network.AWS.EC2.Types.ClientVpnAuthenticationRequest
import Network.AWS.EC2.Types.ClientVpnAuthorizationRuleStatus
import Network.AWS.EC2.Types.ClientVpnConnection
import Network.AWS.EC2.Types.ClientVpnConnectionStatus
import Network.AWS.EC2.Types.ClientVpnEndpoint
import Network.AWS.EC2.Types.ClientVpnEndpointAttributeStatus
import Network.AWS.EC2.Types.ClientVpnEndpointStatus
import Network.AWS.EC2.Types.ClientVpnRoute
import Network.AWS.EC2.Types.ClientVpnRouteStatus
import Network.AWS.EC2.Types.CoipAddressUsage
import Network.AWS.EC2.Types.CoipPool
import Network.AWS.EC2.Types.ConnectionLogOptions
import Network.AWS.EC2.Types.ConnectionLogResponseOptions
import Network.AWS.EC2.Types.ConnectionNotification
import Network.AWS.EC2.Types.ConversionTask
import Network.AWS.EC2.Types.CpuOptions
import Network.AWS.EC2.Types.CpuOptionsRequest
import Network.AWS.EC2.Types.CreateFleetError
import Network.AWS.EC2.Types.CreateFleetInstance
import Network.AWS.EC2.Types.CreateTransitGatewayConnectRequestOptions
import Network.AWS.EC2.Types.CreateTransitGatewayMulticastDomainRequestOptions
import Network.AWS.EC2.Types.CreateTransitGatewayVpcAttachmentRequestOptions
import Network.AWS.EC2.Types.CreateVolumePermission
import Network.AWS.EC2.Types.CreateVolumePermissionModifications
import Network.AWS.EC2.Types.CreditSpecification
import Network.AWS.EC2.Types.CreditSpecificationRequest
import Network.AWS.EC2.Types.CustomerGateway
import Network.AWS.EC2.Types.DeleteFleetError
import Network.AWS.EC2.Types.DeleteFleetErrorItem
import Network.AWS.EC2.Types.DeleteFleetSuccessItem
import Network.AWS.EC2.Types.DeleteLaunchTemplateVersionsResponseErrorItem
import Network.AWS.EC2.Types.DeleteLaunchTemplateVersionsResponseSuccessItem
import Network.AWS.EC2.Types.DeleteQueuedReservedInstancesError
import Network.AWS.EC2.Types.DeregisterInstanceTagAttributeRequest
import Network.AWS.EC2.Types.DescribeFastSnapshotRestoreSuccessItem
import Network.AWS.EC2.Types.DescribeFleetError
import Network.AWS.EC2.Types.DescribeFleetsInstances
import Network.AWS.EC2.Types.DhcpConfiguration
import Network.AWS.EC2.Types.DhcpOptions
import Network.AWS.EC2.Types.DirectoryServiceAuthentication
import Network.AWS.EC2.Types.DirectoryServiceAuthenticationRequest
import Network.AWS.EC2.Types.DisableFastSnapshotRestoreErrorItem
import Network.AWS.EC2.Types.DisableFastSnapshotRestoreStateError
import Network.AWS.EC2.Types.DisableFastSnapshotRestoreStateErrorItem
import Network.AWS.EC2.Types.DisableFastSnapshotRestoreSuccessItem
import Network.AWS.EC2.Types.DiskImage
import Network.AWS.EC2.Types.DiskImageDescription
import Network.AWS.EC2.Types.DiskImageDetail
import Network.AWS.EC2.Types.DiskImageVolumeDescription
import Network.AWS.EC2.Types.DiskInfo
import Network.AWS.EC2.Types.DnsEntry
import Network.AWS.EC2.Types.DnsServersOptionsModifyStructure
import Network.AWS.EC2.Types.EbsBlockDevice
import Network.AWS.EC2.Types.EbsInfo
import Network.AWS.EC2.Types.EbsInstanceBlockDevice
import Network.AWS.EC2.Types.EbsInstanceBlockDeviceSpecification
import Network.AWS.EC2.Types.EbsOptimizedInfo
import Network.AWS.EC2.Types.EgressOnlyInternetGateway
import Network.AWS.EC2.Types.ElasticGpuAssociation
import Network.AWS.EC2.Types.ElasticGpuHealth
import Network.AWS.EC2.Types.ElasticGpuSpecification
import Network.AWS.EC2.Types.ElasticGpuSpecificationResponse
import Network.AWS.EC2.Types.ElasticGpus
import Network.AWS.EC2.Types.ElasticInferenceAccelerator
import Network.AWS.EC2.Types.ElasticInferenceAcceleratorAssociation
import Network.AWS.EC2.Types.EnableFastSnapshotRestoreErrorItem
import Network.AWS.EC2.Types.EnableFastSnapshotRestoreStateError
import Network.AWS.EC2.Types.EnableFastSnapshotRestoreStateErrorItem
import Network.AWS.EC2.Types.EnableFastSnapshotRestoreSuccessItem
import Network.AWS.EC2.Types.EnclaveOptions
import Network.AWS.EC2.Types.EnclaveOptionsRequest
import Network.AWS.EC2.Types.EventInformation
import Network.AWS.EC2.Types.Explanation
import Network.AWS.EC2.Types.ExportImageTask
import Network.AWS.EC2.Types.ExportTask
import Network.AWS.EC2.Types.ExportTaskS3Location
import Network.AWS.EC2.Types.ExportTaskS3LocationRequest
import Network.AWS.EC2.Types.ExportToS3Task
import Network.AWS.EC2.Types.ExportToS3TaskSpecification
import Network.AWS.EC2.Types.FailedQueuedPurchaseDeletion
import Network.AWS.EC2.Types.FederatedAuthentication
import Network.AWS.EC2.Types.FederatedAuthenticationRequest
import Network.AWS.EC2.Types.Filter
import Network.AWS.EC2.Types.FleetData
import Network.AWS.EC2.Types.FleetLaunchTemplateConfig
import Network.AWS.EC2.Types.FleetLaunchTemplateConfigRequest
import Network.AWS.EC2.Types.FleetLaunchTemplateOverrides
import Network.AWS.EC2.Types.FleetLaunchTemplateOverridesRequest
import Network.AWS.EC2.Types.FleetLaunchTemplateSpecification
import Network.AWS.EC2.Types.FleetLaunchTemplateSpecificationRequest
import Network.AWS.EC2.Types.FleetSpotCapacityRebalance
import Network.AWS.EC2.Types.FleetSpotCapacityRebalanceRequest
import Network.AWS.EC2.Types.FleetSpotMaintenanceStrategies
import Network.AWS.EC2.Types.FleetSpotMaintenanceStrategiesRequest
import Network.AWS.EC2.Types.FlowLog
import Network.AWS.EC2.Types.FpgaDeviceInfo
import Network.AWS.EC2.Types.FpgaDeviceMemoryInfo
import Network.AWS.EC2.Types.FpgaImage
import Network.AWS.EC2.Types.FpgaImageAttribute
import Network.AWS.EC2.Types.FpgaImageState
import Network.AWS.EC2.Types.FpgaInfo
import Network.AWS.EC2.Types.GpuDeviceInfo
import Network.AWS.EC2.Types.GpuDeviceMemoryInfo
import Network.AWS.EC2.Types.GpuInfo
import Network.AWS.EC2.Types.GroupIdentifier
import Network.AWS.EC2.Types.HibernationOptions
import Network.AWS.EC2.Types.HibernationOptionsRequest
import Network.AWS.EC2.Types.HistoryRecord
import Network.AWS.EC2.Types.HistoryRecordEntry
import Network.AWS.EC2.Types.Host
import Network.AWS.EC2.Types.HostInstance
import Network.AWS.EC2.Types.HostOffering
import Network.AWS.EC2.Types.HostProperties
import Network.AWS.EC2.Types.HostReservation
import Network.AWS.EC2.Types.IKEVersionsListValue
import Network.AWS.EC2.Types.IKEVersionsRequestListValue
import Network.AWS.EC2.Types.IamInstanceProfile
import Network.AWS.EC2.Types.IamInstanceProfileAssociation
import Network.AWS.EC2.Types.IamInstanceProfileSpecification
import Network.AWS.EC2.Types.IcmpTypeCode
import Network.AWS.EC2.Types.IdFormat
import Network.AWS.EC2.Types.Image
import Network.AWS.EC2.Types.ImageDiskContainer
import Network.AWS.EC2.Types.ImportImageLicenseConfigurationRequest
import Network.AWS.EC2.Types.ImportImageLicenseConfigurationResponse
import Network.AWS.EC2.Types.ImportImageTask
import Network.AWS.EC2.Types.ImportInstanceLaunchSpecification
import Network.AWS.EC2.Types.ImportInstanceTaskDetails
import Network.AWS.EC2.Types.ImportInstanceVolumeDetailItem
import Network.AWS.EC2.Types.ImportSnapshotTask
import Network.AWS.EC2.Types.ImportVolumeTaskDetails
import Network.AWS.EC2.Types.InferenceAcceleratorInfo
import Network.AWS.EC2.Types.InferenceDeviceInfo
import Network.AWS.EC2.Types.Instance
import Network.AWS.EC2.Types.InstanceBlockDeviceMapping
import Network.AWS.EC2.Types.InstanceBlockDeviceMappingSpecification
import Network.AWS.EC2.Types.InstanceCapacity
import Network.AWS.EC2.Types.InstanceCount
import Network.AWS.EC2.Types.InstanceCreditSpecification
import Network.AWS.EC2.Types.InstanceCreditSpecificationRequest
import Network.AWS.EC2.Types.InstanceExportDetails
import Network.AWS.EC2.Types.InstanceFamilyCreditSpecification
import Network.AWS.EC2.Types.InstanceIpv6Address
import Network.AWS.EC2.Types.InstanceIpv6AddressRequest
import Network.AWS.EC2.Types.InstanceMarketOptionsRequest
import Network.AWS.EC2.Types.InstanceMetadataOptionsRequest
import Network.AWS.EC2.Types.InstanceMetadataOptionsResponse
import Network.AWS.EC2.Types.InstanceMonitoring
import Network.AWS.EC2.Types.InstanceNetworkInterface
import Network.AWS.EC2.Types.InstanceNetworkInterfaceAssociation
import Network.AWS.EC2.Types.InstanceNetworkInterfaceAttachment
import Network.AWS.EC2.Types.InstanceNetworkInterfaceSpecification
import Network.AWS.EC2.Types.InstancePrivateIpAddress
import Network.AWS.EC2.Types.InstanceSpecification
import Network.AWS.EC2.Types.InstanceState
import Network.AWS.EC2.Types.InstanceStateChange
import Network.AWS.EC2.Types.InstanceStatus
import Network.AWS.EC2.Types.InstanceStatusDetails
import Network.AWS.EC2.Types.InstanceStatusEvent
import Network.AWS.EC2.Types.InstanceStatusSummary
import Network.AWS.EC2.Types.InstanceStorageInfo
import Network.AWS.EC2.Types.InstanceTagNotificationAttribute
import Network.AWS.EC2.Types.InstanceTypeInfo
import Network.AWS.EC2.Types.InstanceTypeOffering
import Network.AWS.EC2.Types.InstanceUsage
import Network.AWS.EC2.Types.InternetGateway
import Network.AWS.EC2.Types.InternetGatewayAttachment
import Network.AWS.EC2.Types.IpPermission
import Network.AWS.EC2.Types.IpRange
import Network.AWS.EC2.Types.Ipv6CidrAssociation
import Network.AWS.EC2.Types.Ipv6CidrBlock
import Network.AWS.EC2.Types.Ipv6Pool
import Network.AWS.EC2.Types.Ipv6Range
import Network.AWS.EC2.Types.KeyPairInfo
import Network.AWS.EC2.Types.LastError
import Network.AWS.EC2.Types.LaunchPermission
import Network.AWS.EC2.Types.LaunchPermissionModifications
import Network.AWS.EC2.Types.LaunchSpecification
import Network.AWS.EC2.Types.LaunchTemplate
import Network.AWS.EC2.Types.LaunchTemplateAndOverridesResponse
import Network.AWS.EC2.Types.LaunchTemplateBlockDeviceMapping
import Network.AWS.EC2.Types.LaunchTemplateBlockDeviceMappingRequest
import Network.AWS.EC2.Types.LaunchTemplateCapacityReservationSpecificationRequest
import Network.AWS.EC2.Types.LaunchTemplateCapacityReservationSpecificationResponse
import Network.AWS.EC2.Types.LaunchTemplateConfig
import Network.AWS.EC2.Types.LaunchTemplateCpuOptions
import Network.AWS.EC2.Types.LaunchTemplateCpuOptionsRequest
import Network.AWS.EC2.Types.LaunchTemplateEbsBlockDevice
import Network.AWS.EC2.Types.LaunchTemplateEbsBlockDeviceRequest
import Network.AWS.EC2.Types.LaunchTemplateElasticInferenceAccelerator
import Network.AWS.EC2.Types.LaunchTemplateElasticInferenceAcceleratorResponse
import Network.AWS.EC2.Types.LaunchTemplateEnclaveOptions
import Network.AWS.EC2.Types.LaunchTemplateEnclaveOptionsRequest
import Network.AWS.EC2.Types.LaunchTemplateHibernationOptions
import Network.AWS.EC2.Types.LaunchTemplateHibernationOptionsRequest
import Network.AWS.EC2.Types.LaunchTemplateIamInstanceProfileSpecification
import Network.AWS.EC2.Types.LaunchTemplateIamInstanceProfileSpecificationRequest
import Network.AWS.EC2.Types.LaunchTemplateInstanceMarketOptions
import Network.AWS.EC2.Types.LaunchTemplateInstanceMarketOptionsRequest
import Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataOptions
import Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataOptionsRequest
import Network.AWS.EC2.Types.LaunchTemplateInstanceNetworkInterfaceSpecification
import Network.AWS.EC2.Types.LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
import Network.AWS.EC2.Types.LaunchTemplateLicenseConfiguration
import Network.AWS.EC2.Types.LaunchTemplateLicenseConfigurationRequest
import Network.AWS.EC2.Types.LaunchTemplateOverrides
import Network.AWS.EC2.Types.LaunchTemplatePlacement
import Network.AWS.EC2.Types.LaunchTemplatePlacementRequest
import Network.AWS.EC2.Types.LaunchTemplateSpecification
import Network.AWS.EC2.Types.LaunchTemplateSpotMarketOptions
import Network.AWS.EC2.Types.LaunchTemplateSpotMarketOptionsRequest
import Network.AWS.EC2.Types.LaunchTemplateTagSpecification
import Network.AWS.EC2.Types.LaunchTemplateTagSpecificationRequest
import Network.AWS.EC2.Types.LaunchTemplateVersion
import Network.AWS.EC2.Types.LaunchTemplatesMonitoring
import Network.AWS.EC2.Types.LaunchTemplatesMonitoringRequest
import Network.AWS.EC2.Types.LicenseConfiguration
import Network.AWS.EC2.Types.LicenseConfigurationRequest
import Network.AWS.EC2.Types.LoadBalancersConfig
import Network.AWS.EC2.Types.LoadPermission
import Network.AWS.EC2.Types.LoadPermissionModifications
import Network.AWS.EC2.Types.LoadPermissionRequest
import Network.AWS.EC2.Types.LocalGateway
import Network.AWS.EC2.Types.LocalGatewayRoute
import Network.AWS.EC2.Types.LocalGatewayRouteTable
import Network.AWS.EC2.Types.LocalGatewayRouteTableVirtualInterfaceGroupAssociation
import Network.AWS.EC2.Types.LocalGatewayRouteTableVpcAssociation
import Network.AWS.EC2.Types.LocalGatewayVirtualInterface
import Network.AWS.EC2.Types.LocalGatewayVirtualInterfaceGroup
import Network.AWS.EC2.Types.ManagedPrefixList
import Network.AWS.EC2.Types.MemoryInfo
import Network.AWS.EC2.Types.ModifyTransitGatewayOptions
import Network.AWS.EC2.Types.ModifyTransitGatewayVpcAttachmentRequestOptions
import Network.AWS.EC2.Types.ModifyVpnTunnelOptionsSpecification
import Network.AWS.EC2.Types.Monitoring
import Network.AWS.EC2.Types.MovingAddressStatus
import Network.AWS.EC2.Types.NatGateway
import Network.AWS.EC2.Types.NatGatewayAddress
import Network.AWS.EC2.Types.NetworkAcl
import Network.AWS.EC2.Types.NetworkAclAssociation
import Network.AWS.EC2.Types.NetworkAclEntry
import Network.AWS.EC2.Types.NetworkCardInfo
import Network.AWS.EC2.Types.NetworkInfo
import Network.AWS.EC2.Types.NetworkInsightsAnalysis
import Network.AWS.EC2.Types.NetworkInsightsPath
import Network.AWS.EC2.Types.NetworkInterface
import Network.AWS.EC2.Types.NetworkInterfaceAssociation
import Network.AWS.EC2.Types.NetworkInterfaceAttachment
import Network.AWS.EC2.Types.NetworkInterfaceAttachmentChanges
import Network.AWS.EC2.Types.NetworkInterfaceIpv6Address
import Network.AWS.EC2.Types.NetworkInterfacePermission
import Network.AWS.EC2.Types.NetworkInterfacePermissionState
import Network.AWS.EC2.Types.NetworkInterfacePrivateIpAddress
import Network.AWS.EC2.Types.NewDhcpConfiguration
import Network.AWS.EC2.Types.OnDemandOptions
import Network.AWS.EC2.Types.OnDemandOptionsRequest
import Network.AWS.EC2.Types.PathComponent
import Network.AWS.EC2.Types.PciId
import Network.AWS.EC2.Types.PeeringAttachmentStatus
import Network.AWS.EC2.Types.PeeringConnectionOptions
import Network.AWS.EC2.Types.PeeringConnectionOptionsRequest
import Network.AWS.EC2.Types.PeeringTgwInfo
import Network.AWS.EC2.Types.Phase1DHGroupNumbersListValue
import Network.AWS.EC2.Types.Phase1DHGroupNumbersRequestListValue
import Network.AWS.EC2.Types.Phase1EncryptionAlgorithmsListValue
import Network.AWS.EC2.Types.Phase1EncryptionAlgorithmsRequestListValue
import Network.AWS.EC2.Types.Phase1IntegrityAlgorithmsListValue
import Network.AWS.EC2.Types.Phase1IntegrityAlgorithmsRequestListValue
import Network.AWS.EC2.Types.Phase2DHGroupNumbersListValue
import Network.AWS.EC2.Types.Phase2DHGroupNumbersRequestListValue
import Network.AWS.EC2.Types.Phase2EncryptionAlgorithmsListValue
import Network.AWS.EC2.Types.Phase2EncryptionAlgorithmsRequestListValue
import Network.AWS.EC2.Types.Phase2IntegrityAlgorithmsListValue
import Network.AWS.EC2.Types.Phase2IntegrityAlgorithmsRequestListValue
import Network.AWS.EC2.Types.Placement
import Network.AWS.EC2.Types.PlacementGroup
import Network.AWS.EC2.Types.PlacementGroupInfo
import Network.AWS.EC2.Types.PlacementResponse
import Network.AWS.EC2.Types.PoolCidrBlock
import Network.AWS.EC2.Types.PortRange
import Network.AWS.EC2.Types.PrefixList
import Network.AWS.EC2.Types.PrefixListAssociation
import Network.AWS.EC2.Types.PrefixListEntry
import Network.AWS.EC2.Types.PrefixListId
import Network.AWS.EC2.Types.PriceSchedule
import Network.AWS.EC2.Types.PriceScheduleSpecification
import Network.AWS.EC2.Types.PricingDetail
import Network.AWS.EC2.Types.PrincipalIdFormat
import Network.AWS.EC2.Types.PrivateDnsDetails
import Network.AWS.EC2.Types.PrivateDnsNameConfiguration
import Network.AWS.EC2.Types.PrivateIpAddressSpecification
import Network.AWS.EC2.Types.ProcessorInfo
import Network.AWS.EC2.Types.ProductCode
import Network.AWS.EC2.Types.PropagatingVgw
import Network.AWS.EC2.Types.ProvisionedBandwidth
import Network.AWS.EC2.Types.PtrUpdateStatus
import Network.AWS.EC2.Types.PublicIpv4Pool
import Network.AWS.EC2.Types.PublicIpv4PoolRange
import Network.AWS.EC2.Types.Purchase
import Network.AWS.EC2.Types.PurchaseRequest
import Network.AWS.EC2.Types.RecurringCharge
import Network.AWS.EC2.Types.RegionInfo
import Network.AWS.EC2.Types.RegisterInstanceTagAttributeRequest
import Network.AWS.EC2.Types.RemovePrefixListEntry
import Network.AWS.EC2.Types.RequestLaunchTemplateData
import Network.AWS.EC2.Types.RequestSpotLaunchSpecification
import Network.AWS.EC2.Types.Reservation
import Network.AWS.EC2.Types.ReservationValue
import Network.AWS.EC2.Types.ReservedInstanceLimitPrice
import Network.AWS.EC2.Types.ReservedInstanceReservationValue
import Network.AWS.EC2.Types.ReservedInstances
import Network.AWS.EC2.Types.ReservedInstancesConfiguration
import Network.AWS.EC2.Types.ReservedInstancesId
import Network.AWS.EC2.Types.ReservedInstancesListing
import Network.AWS.EC2.Types.ReservedInstancesModification
import Network.AWS.EC2.Types.ReservedInstancesModificationResult
import Network.AWS.EC2.Types.ReservedInstancesOffering
import Network.AWS.EC2.Types.ResponseError
import Network.AWS.EC2.Types.ResponseLaunchTemplateData
import Network.AWS.EC2.Types.Route
import Network.AWS.EC2.Types.RouteTable
import Network.AWS.EC2.Types.RouteTableAssociation
import Network.AWS.EC2.Types.RouteTableAssociationState
import Network.AWS.EC2.Types.RunInstancesMonitoringEnabled
import Network.AWS.EC2.Types.S3Storage
import Network.AWS.EC2.Types.ScheduledInstance
import Network.AWS.EC2.Types.ScheduledInstanceAvailability
import Network.AWS.EC2.Types.ScheduledInstanceRecurrence
import Network.AWS.EC2.Types.ScheduledInstanceRecurrenceRequest
import Network.AWS.EC2.Types.ScheduledInstancesBlockDeviceMapping
import Network.AWS.EC2.Types.ScheduledInstancesEbs
import Network.AWS.EC2.Types.ScheduledInstancesIamInstanceProfile
import Network.AWS.EC2.Types.ScheduledInstancesIpv6Address
import Network.AWS.EC2.Types.ScheduledInstancesLaunchSpecification
import Network.AWS.EC2.Types.ScheduledInstancesMonitoring
import Network.AWS.EC2.Types.ScheduledInstancesNetworkInterface
import Network.AWS.EC2.Types.ScheduledInstancesPlacement
import Network.AWS.EC2.Types.ScheduledInstancesPrivateIpAddressConfig
import Network.AWS.EC2.Types.SecurityGroup
import Network.AWS.EC2.Types.SecurityGroupIdentifier
import Network.AWS.EC2.Types.SecurityGroupReference
import Network.AWS.EC2.Types.ServiceConfiguration
import Network.AWS.EC2.Types.ServiceDetail
import Network.AWS.EC2.Types.ServiceTypeDetail
import Network.AWS.EC2.Types.SlotDateTimeRangeRequest
import Network.AWS.EC2.Types.SlotStartTimeRangeRequest
import Network.AWS.EC2.Types.Snapshot
import Network.AWS.EC2.Types.SnapshotDetail
import Network.AWS.EC2.Types.SnapshotDiskContainer
import Network.AWS.EC2.Types.SnapshotInfo
import Network.AWS.EC2.Types.SnapshotTaskDetail
import Network.AWS.EC2.Types.SpotCapacityRebalance
import Network.AWS.EC2.Types.SpotDatafeedSubscription
import Network.AWS.EC2.Types.SpotFleetLaunchSpecification
import Network.AWS.EC2.Types.SpotFleetMonitoring
import Network.AWS.EC2.Types.SpotFleetRequestConfig
import Network.AWS.EC2.Types.SpotFleetRequestConfigData
import Network.AWS.EC2.Types.SpotFleetTagSpecification
import Network.AWS.EC2.Types.SpotInstanceRequest
import Network.AWS.EC2.Types.SpotInstanceStateFault
import Network.AWS.EC2.Types.SpotInstanceStatus
import Network.AWS.EC2.Types.SpotMaintenanceStrategies
import Network.AWS.EC2.Types.SpotMarketOptions
import Network.AWS.EC2.Types.SpotOptions
import Network.AWS.EC2.Types.SpotOptionsRequest
import Network.AWS.EC2.Types.SpotPlacement
import Network.AWS.EC2.Types.SpotPrice
import Network.AWS.EC2.Types.StaleIpPermission
import Network.AWS.EC2.Types.StaleSecurityGroup
import Network.AWS.EC2.Types.StateReason
import Network.AWS.EC2.Types.Storage
import Network.AWS.EC2.Types.StorageLocation
import Network.AWS.EC2.Types.Subnet
import Network.AWS.EC2.Types.SubnetAssociation
import Network.AWS.EC2.Types.SubnetCidrBlockState
import Network.AWS.EC2.Types.SubnetIpv6CidrBlockAssociation
import Network.AWS.EC2.Types.SuccessfulInstanceCreditSpecificationItem
import Network.AWS.EC2.Types.SuccessfulQueuedPurchaseDeletion
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.TagDescription
import Network.AWS.EC2.Types.TagSpecification
import Network.AWS.EC2.Types.TargetCapacitySpecification
import Network.AWS.EC2.Types.TargetCapacitySpecificationRequest
import Network.AWS.EC2.Types.TargetConfiguration
import Network.AWS.EC2.Types.TargetConfigurationRequest
import Network.AWS.EC2.Types.TargetGroup
import Network.AWS.EC2.Types.TargetGroupsConfig
import Network.AWS.EC2.Types.TargetNetwork
import Network.AWS.EC2.Types.TargetReservationValue
import Network.AWS.EC2.Types.TerminateConnectionStatus
import Network.AWS.EC2.Types.TrafficMirrorFilter
import Network.AWS.EC2.Types.TrafficMirrorFilterRule
import Network.AWS.EC2.Types.TrafficMirrorPortRange
import Network.AWS.EC2.Types.TrafficMirrorPortRangeRequest
import Network.AWS.EC2.Types.TrafficMirrorSession
import Network.AWS.EC2.Types.TrafficMirrorTarget
import Network.AWS.EC2.Types.TransitGateway
import Network.AWS.EC2.Types.TransitGatewayAssociation
import Network.AWS.EC2.Types.TransitGatewayAttachment
import Network.AWS.EC2.Types.TransitGatewayAttachmentAssociation
import Network.AWS.EC2.Types.TransitGatewayAttachmentBgpConfiguration
import Network.AWS.EC2.Types.TransitGatewayAttachmentPropagation
import Network.AWS.EC2.Types.TransitGatewayConnect
import Network.AWS.EC2.Types.TransitGatewayConnectOptions
import Network.AWS.EC2.Types.TransitGatewayConnectPeer
import Network.AWS.EC2.Types.TransitGatewayConnectPeerConfiguration
import Network.AWS.EC2.Types.TransitGatewayConnectRequestBgpOptions
import Network.AWS.EC2.Types.TransitGatewayMulticastDeregisteredGroupMembers
import Network.AWS.EC2.Types.TransitGatewayMulticastDeregisteredGroupSources
import Network.AWS.EC2.Types.TransitGatewayMulticastDomain
import Network.AWS.EC2.Types.TransitGatewayMulticastDomainAssociation
import Network.AWS.EC2.Types.TransitGatewayMulticastDomainAssociations
import Network.AWS.EC2.Types.TransitGatewayMulticastDomainOptions
import Network.AWS.EC2.Types.TransitGatewayMulticastGroup
import Network.AWS.EC2.Types.TransitGatewayMulticastRegisteredGroupMembers
import Network.AWS.EC2.Types.TransitGatewayMulticastRegisteredGroupSources
import Network.AWS.EC2.Types.TransitGatewayOptions
import Network.AWS.EC2.Types.TransitGatewayPeeringAttachment
import Network.AWS.EC2.Types.TransitGatewayPrefixListAttachment
import Network.AWS.EC2.Types.TransitGatewayPrefixListReference
import Network.AWS.EC2.Types.TransitGatewayPropagation
import Network.AWS.EC2.Types.TransitGatewayRequestOptions
import Network.AWS.EC2.Types.TransitGatewayRoute
import Network.AWS.EC2.Types.TransitGatewayRouteAttachment
import Network.AWS.EC2.Types.TransitGatewayRouteTable
import Network.AWS.EC2.Types.TransitGatewayRouteTableAssociation
import Network.AWS.EC2.Types.TransitGatewayRouteTablePropagation
import Network.AWS.EC2.Types.TransitGatewayVpcAttachment
import Network.AWS.EC2.Types.TransitGatewayVpcAttachmentOptions
import Network.AWS.EC2.Types.TunnelOption
import Network.AWS.EC2.Types.UnsuccessfulInstanceCreditSpecificationItem
import Network.AWS.EC2.Types.UnsuccessfulInstanceCreditSpecificationItemError
import Network.AWS.EC2.Types.UnsuccessfulItem
import Network.AWS.EC2.Types.UnsuccessfulItemError
import Network.AWS.EC2.Types.UserBucket
import Network.AWS.EC2.Types.UserBucketDetails
import Network.AWS.EC2.Types.UserData
import Network.AWS.EC2.Types.UserIdGroupPair
import Network.AWS.EC2.Types.VCpuInfo
import Network.AWS.EC2.Types.ValidationError
import Network.AWS.EC2.Types.ValidationWarning
import Network.AWS.EC2.Types.VgwTelemetry
import Network.AWS.EC2.Types.Volume
import Network.AWS.EC2.Types.VolumeAttachment
import Network.AWS.EC2.Types.VolumeDetail
import Network.AWS.EC2.Types.VolumeModification
import Network.AWS.EC2.Types.VolumeStatusAction
import Network.AWS.EC2.Types.VolumeStatusAttachmentStatus
import Network.AWS.EC2.Types.VolumeStatusDetails
import Network.AWS.EC2.Types.VolumeStatusEvent
import Network.AWS.EC2.Types.VolumeStatusInfo
import Network.AWS.EC2.Types.VolumeStatusItem
import Network.AWS.EC2.Types.Vpc
import Network.AWS.EC2.Types.VpcAttachment
import Network.AWS.EC2.Types.VpcCidrBlockAssociation
import Network.AWS.EC2.Types.VpcCidrBlockState
import Network.AWS.EC2.Types.VpcClassicLink
import Network.AWS.EC2.Types.VpcEndpoint
import Network.AWS.EC2.Types.VpcEndpointConnection
import Network.AWS.EC2.Types.VpcIpv6CidrBlockAssociation
import Network.AWS.EC2.Types.VpcPeeringConnection
import Network.AWS.EC2.Types.VpcPeeringConnectionOptionsDescription
import Network.AWS.EC2.Types.VpcPeeringConnectionStateReason
import Network.AWS.EC2.Types.VpcPeeringConnectionVpcInfo
import Network.AWS.EC2.Types.VpnConnection
import Network.AWS.EC2.Types.VpnConnectionOptions
import Network.AWS.EC2.Types.VpnConnectionOptionsSpecification
import Network.AWS.EC2.Types.VpnGateway
import Network.AWS.EC2.Types.VpnStaticRoute
import Network.AWS.EC2.Types.VpnTunnelOptionsSpecification
import Network.AWS.EC2.UnassignIpv6Addresses
import Network.AWS.EC2.UnassignPrivateIpAddresses
import Network.AWS.EC2.UnmonitorInstances
import Network.AWS.EC2.UpdateSecurityGroupRuleDescriptionsEgress
import Network.AWS.EC2.UpdateSecurityGroupRuleDescriptionsIngress
import Network.AWS.EC2.WithdrawByoipCidr
