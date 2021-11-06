{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EC2.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Lens
  ( -- * Operations

    -- ** ModifyCapacityReservation
    modifyCapacityReservation_instanceCount,
    modifyCapacityReservation_accept,
    modifyCapacityReservation_endDate,
    modifyCapacityReservation_endDateType,
    modifyCapacityReservation_dryRun,
    modifyCapacityReservation_capacityReservationId,
    modifyCapacityReservationResponse_return,
    modifyCapacityReservationResponse_httpStatus,

    -- ** GetAssociatedIpv6PoolCidrs
    getAssociatedIpv6PoolCidrs_nextToken,
    getAssociatedIpv6PoolCidrs_dryRun,
    getAssociatedIpv6PoolCidrs_maxResults,
    getAssociatedIpv6PoolCidrs_poolId,
    getAssociatedIpv6PoolCidrsResponse_ipv6CidrAssociations,
    getAssociatedIpv6PoolCidrsResponse_nextToken,
    getAssociatedIpv6PoolCidrsResponse_httpStatus,

    -- ** ImportInstance
    importInstance_launchSpecification,
    importInstance_diskImages,
    importInstance_description,
    importInstance_dryRun,
    importInstance_platform,
    importInstanceResponse_conversionTask,
    importInstanceResponse_httpStatus,

    -- ** DescribeCapacityReservationFleets
    describeCapacityReservationFleets_capacityReservationFleetIds,
    describeCapacityReservationFleets_filters,
    describeCapacityReservationFleets_nextToken,
    describeCapacityReservationFleets_dryRun,
    describeCapacityReservationFleets_maxResults,
    describeCapacityReservationFleetsResponse_capacityReservationFleets,
    describeCapacityReservationFleetsResponse_nextToken,
    describeCapacityReservationFleetsResponse_httpStatus,

    -- ** ModifySecurityGroupRules
    modifySecurityGroupRules_dryRun,
    modifySecurityGroupRules_groupId,
    modifySecurityGroupRules_securityGroupRules,
    modifySecurityGroupRulesResponse_return,
    modifySecurityGroupRulesResponse_httpStatus,

    -- ** RevokeSecurityGroupEgress
    revokeSecurityGroupEgress_fromPort,
    revokeSecurityGroupEgress_ipPermissions,
    revokeSecurityGroupEgress_ipProtocol,
    revokeSecurityGroupEgress_toPort,
    revokeSecurityGroupEgress_cidrIp,
    revokeSecurityGroupEgress_sourceSecurityGroupOwnerId,
    revokeSecurityGroupEgress_sourceSecurityGroupName,
    revokeSecurityGroupEgress_dryRun,
    revokeSecurityGroupEgress_securityGroupRuleIds,
    revokeSecurityGroupEgress_groupId,
    revokeSecurityGroupEgressResponse_return,
    revokeSecurityGroupEgressResponse_unknownIpPermissions,
    revokeSecurityGroupEgressResponse_httpStatus,

    -- ** CreateNetworkInterfacePermission
    createNetworkInterfacePermission_awsAccountId,
    createNetworkInterfacePermission_awsService,
    createNetworkInterfacePermission_dryRun,
    createNetworkInterfacePermission_networkInterfaceId,
    createNetworkInterfacePermission_permission,
    createNetworkInterfacePermissionResponse_interfacePermission,
    createNetworkInterfacePermissionResponse_httpStatus,

    -- ** SendDiagnosticInterrupt
    sendDiagnosticInterrupt_dryRun,
    sendDiagnosticInterrupt_instanceId,

    -- ** DeleteLaunchTemplate
    deleteLaunchTemplate_launchTemplateName,
    deleteLaunchTemplate_launchTemplateId,
    deleteLaunchTemplate_dryRun,
    deleteLaunchTemplateResponse_launchTemplate,
    deleteLaunchTemplateResponse_httpStatus,

    -- ** RejectVpcEndpointConnections
    rejectVpcEndpointConnections_dryRun,
    rejectVpcEndpointConnections_serviceId,
    rejectVpcEndpointConnections_vpcEndpointIds,
    rejectVpcEndpointConnectionsResponse_unsuccessful,
    rejectVpcEndpointConnectionsResponse_httpStatus,

    -- ** CreateVpnGateway
    createVpnGateway_amazonSideAsn,
    createVpnGateway_tagSpecifications,
    createVpnGateway_availabilityZone,
    createVpnGateway_dryRun,
    createVpnGateway_type,
    createVpnGatewayResponse_vpnGateway,
    createVpnGatewayResponse_httpStatus,

    -- ** CreateNetworkAcl
    createNetworkAcl_tagSpecifications,
    createNetworkAcl_dryRun,
    createNetworkAcl_vpcId,
    createNetworkAclResponse_networkAcl,
    createNetworkAclResponse_httpStatus,

    -- ** DeleteKeyPair
    deleteKeyPair_keyName,
    deleteKeyPair_keyPairId,
    deleteKeyPair_dryRun,

    -- ** DescribeSecurityGroupReferences
    describeSecurityGroupReferences_dryRun,
    describeSecurityGroupReferences_groupId,
    describeSecurityGroupReferencesResponse_securityGroupReferenceSet,
    describeSecurityGroupReferencesResponse_httpStatus,

    -- ** DeleteFleets
    deleteFleets_dryRun,
    deleteFleets_fleetIds,
    deleteFleets_terminateInstances,
    deleteFleetsResponse_successfulFleetDeletions,
    deleteFleetsResponse_unsuccessfulFleetDeletions,
    deleteFleetsResponse_httpStatus,

    -- ** DescribeTags
    describeTags_filters,
    describeTags_nextToken,
    describeTags_dryRun,
    describeTags_maxResults,
    describeTagsResponse_nextToken,
    describeTagsResponse_tags,
    describeTagsResponse_httpStatus,

    -- ** CreateTransitGatewayRouteTable
    createTransitGatewayRouteTable_tagSpecifications,
    createTransitGatewayRouteTable_dryRun,
    createTransitGatewayRouteTable_transitGatewayId,
    createTransitGatewayRouteTableResponse_transitGatewayRouteTable,
    createTransitGatewayRouteTableResponse_httpStatus,

    -- ** ModifyInstanceMetadataOptions
    modifyInstanceMetadataOptions_httpProtocolIpv6,
    modifyInstanceMetadataOptions_httpEndpoint,
    modifyInstanceMetadataOptions_httpPutResponseHopLimit,
    modifyInstanceMetadataOptions_httpTokens,
    modifyInstanceMetadataOptions_dryRun,
    modifyInstanceMetadataOptions_instanceId,
    modifyInstanceMetadataOptionsResponse_instanceId,
    modifyInstanceMetadataOptionsResponse_instanceMetadataOptions,
    modifyInstanceMetadataOptionsResponse_httpStatus,

    -- ** UpdateSecurityGroupRuleDescriptionsIngress
    updateSecurityGroupRuleDescriptionsIngress_securityGroupRuleDescriptions,
    updateSecurityGroupRuleDescriptionsIngress_ipPermissions,
    updateSecurityGroupRuleDescriptionsIngress_groupId,
    updateSecurityGroupRuleDescriptionsIngress_groupName,
    updateSecurityGroupRuleDescriptionsIngress_dryRun,
    updateSecurityGroupRuleDescriptionsIngressResponse_return,
    updateSecurityGroupRuleDescriptionsIngressResponse_httpStatus,

    -- ** DisassociateSubnetCidrBlock
    disassociateSubnetCidrBlock_associationId,
    disassociateSubnetCidrBlockResponse_subnetId,
    disassociateSubnetCidrBlockResponse_ipv6CidrBlockAssociation,
    disassociateSubnetCidrBlockResponse_httpStatus,

    -- ** DetachNetworkInterface
    detachNetworkInterface_force,
    detachNetworkInterface_dryRun,
    detachNetworkInterface_attachmentId,

    -- ** DetachInternetGateway
    detachInternetGateway_dryRun,
    detachInternetGateway_internetGatewayId,
    detachInternetGateway_vpcId,

    -- ** DeleteVpcEndpoints
    deleteVpcEndpoints_dryRun,
    deleteVpcEndpoints_vpcEndpointIds,
    deleteVpcEndpointsResponse_unsuccessful,
    deleteVpcEndpointsResponse_httpStatus,

    -- ** DescribeClientVpnEndpoints
    describeClientVpnEndpoints_filters,
    describeClientVpnEndpoints_clientVpnEndpointIds,
    describeClientVpnEndpoints_nextToken,
    describeClientVpnEndpoints_dryRun,
    describeClientVpnEndpoints_maxResults,
    describeClientVpnEndpointsResponse_nextToken,
    describeClientVpnEndpointsResponse_clientVpnEndpoints,
    describeClientVpnEndpointsResponse_httpStatus,

    -- ** DeleteFlowLogs
    deleteFlowLogs_dryRun,
    deleteFlowLogs_flowLogIds,
    deleteFlowLogsResponse_unsuccessful,
    deleteFlowLogsResponse_httpStatus,

    -- ** DescribeVpcClassicLink
    describeVpcClassicLink_filters,
    describeVpcClassicLink_vpcIds,
    describeVpcClassicLink_dryRun,
    describeVpcClassicLinkResponse_vpcs,
    describeVpcClassicLinkResponse_httpStatus,

    -- ** GetAssociatedEnclaveCertificateIamRoles
    getAssociatedEnclaveCertificateIamRoles_certificateArn,
    getAssociatedEnclaveCertificateIamRoles_dryRun,
    getAssociatedEnclaveCertificateIamRolesResponse_associatedRoles,
    getAssociatedEnclaveCertificateIamRolesResponse_httpStatus,

    -- ** AssociateTransitGatewayMulticastDomain
    associateTransitGatewayMulticastDomain_subnetIds,
    associateTransitGatewayMulticastDomain_transitGatewayMulticastDomainId,
    associateTransitGatewayMulticastDomain_transitGatewayAttachmentId,
    associateTransitGatewayMulticastDomain_dryRun,
    associateTransitGatewayMulticastDomainResponse_associations,
    associateTransitGatewayMulticastDomainResponse_httpStatus,

    -- ** ModifySubnetAttribute
    modifySubnetAttribute_assignIpv6AddressOnCreation,
    modifySubnetAttribute_customerOwnedIpv4Pool,
    modifySubnetAttribute_mapCustomerOwnedIpOnLaunch,
    modifySubnetAttribute_mapPublicIpOnLaunch,
    modifySubnetAttribute_subnetId,

    -- ** DetachVolume
    detachVolume_instanceId,
    detachVolume_force,
    detachVolume_device,
    detachVolume_dryRun,
    detachVolume_volumeId,
    volumeAttachment_instanceId,
    volumeAttachment_deleteOnTermination,
    volumeAttachment_state,
    volumeAttachment_device,
    volumeAttachment_volumeId,
    volumeAttachment_attachTime,

    -- ** DescribeInstanceCreditSpecifications
    describeInstanceCreditSpecifications_filters,
    describeInstanceCreditSpecifications_nextToken,
    describeInstanceCreditSpecifications_instanceIds,
    describeInstanceCreditSpecifications_dryRun,
    describeInstanceCreditSpecifications_maxResults,
    describeInstanceCreditSpecificationsResponse_nextToken,
    describeInstanceCreditSpecificationsResponse_instanceCreditSpecifications,
    describeInstanceCreditSpecificationsResponse_httpStatus,

    -- ** CancelBundleTask
    cancelBundleTask_dryRun,
    cancelBundleTask_bundleId,
    cancelBundleTaskResponse_bundleTask,
    cancelBundleTaskResponse_httpStatus,

    -- ** DescribeByoipCidrs
    describeByoipCidrs_nextToken,
    describeByoipCidrs_dryRun,
    describeByoipCidrs_maxResults,
    describeByoipCidrsResponse_nextToken,
    describeByoipCidrsResponse_byoipCidrs,
    describeByoipCidrsResponse_httpStatus,

    -- ** AcceptReservedInstancesExchangeQuote
    acceptReservedInstancesExchangeQuote_targetConfigurations,
    acceptReservedInstancesExchangeQuote_dryRun,
    acceptReservedInstancesExchangeQuote_reservedInstanceIds,
    acceptReservedInstancesExchangeQuoteResponse_exchangeId,
    acceptReservedInstancesExchangeQuoteResponse_httpStatus,

    -- ** ReleaseAddress
    releaseAddress_allocationId,
    releaseAddress_networkBorderGroup,
    releaseAddress_publicIp,
    releaseAddress_dryRun,

    -- ** DescribeInstanceTypeOfferings
    describeInstanceTypeOfferings_filters,
    describeInstanceTypeOfferings_nextToken,
    describeInstanceTypeOfferings_locationType,
    describeInstanceTypeOfferings_dryRun,
    describeInstanceTypeOfferings_maxResults,
    describeInstanceTypeOfferingsResponse_instanceTypeOfferings,
    describeInstanceTypeOfferingsResponse_nextToken,
    describeInstanceTypeOfferingsResponse_httpStatus,

    -- ** CreateInternetGateway
    createInternetGateway_tagSpecifications,
    createInternetGateway_dryRun,
    createInternetGatewayResponse_internetGateway,
    createInternetGatewayResponse_httpStatus,

    -- ** DeleteVpnConnection
    deleteVpnConnection_dryRun,
    deleteVpnConnection_vpnConnectionId,

    -- ** DescribeBundleTasks
    describeBundleTasks_bundleIds,
    describeBundleTasks_filters,
    describeBundleTasks_dryRun,
    describeBundleTasksResponse_bundleTasks,
    describeBundleTasksResponse_httpStatus,

    -- ** AuthorizeSecurityGroupEgress
    authorizeSecurityGroupEgress_fromPort,
    authorizeSecurityGroupEgress_ipPermissions,
    authorizeSecurityGroupEgress_tagSpecifications,
    authorizeSecurityGroupEgress_ipProtocol,
    authorizeSecurityGroupEgress_toPort,
    authorizeSecurityGroupEgress_cidrIp,
    authorizeSecurityGroupEgress_sourceSecurityGroupOwnerId,
    authorizeSecurityGroupEgress_sourceSecurityGroupName,
    authorizeSecurityGroupEgress_dryRun,
    authorizeSecurityGroupEgress_groupId,
    authorizeSecurityGroupEgressResponse_return,
    authorizeSecurityGroupEgressResponse_securityGroupRules,
    authorizeSecurityGroupEgressResponse_httpStatus,

    -- ** EnableTransitGatewayRouteTablePropagation
    enableTransitGatewayRouteTablePropagation_dryRun,
    enableTransitGatewayRouteTablePropagation_transitGatewayRouteTableId,
    enableTransitGatewayRouteTablePropagation_transitGatewayAttachmentId,
    enableTransitGatewayRouteTablePropagationResponse_propagation,
    enableTransitGatewayRouteTablePropagationResponse_httpStatus,

    -- ** DeregisterImage
    deregisterImage_dryRun,
    deregisterImage_imageId,

    -- ** DeleteVpcEndpointConnectionNotifications
    deleteVpcEndpointConnectionNotifications_dryRun,
    deleteVpcEndpointConnectionNotifications_connectionNotificationIds,
    deleteVpcEndpointConnectionNotificationsResponse_unsuccessful,
    deleteVpcEndpointConnectionNotificationsResponse_httpStatus,

    -- ** DescribeCoipPools
    describeCoipPools_poolIds,
    describeCoipPools_filters,
    describeCoipPools_nextToken,
    describeCoipPools_dryRun,
    describeCoipPools_maxResults,
    describeCoipPoolsResponse_coipPools,
    describeCoipPoolsResponse_nextToken,
    describeCoipPoolsResponse_httpStatus,

    -- ** ResetAddressAttribute
    resetAddressAttribute_dryRun,
    resetAddressAttribute_allocationId,
    resetAddressAttribute_attribute,
    resetAddressAttributeResponse_address,
    resetAddressAttributeResponse_httpStatus,

    -- ** GetTransitGatewayMulticastDomainAssociations
    getTransitGatewayMulticastDomainAssociations_filters,
    getTransitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId,
    getTransitGatewayMulticastDomainAssociations_nextToken,
    getTransitGatewayMulticastDomainAssociations_dryRun,
    getTransitGatewayMulticastDomainAssociations_maxResults,
    getTransitGatewayMulticastDomainAssociationsResponse_nextToken,
    getTransitGatewayMulticastDomainAssociationsResponse_multicastDomainAssociations,
    getTransitGatewayMulticastDomainAssociationsResponse_httpStatus,

    -- ** DeleteLocalGatewayRouteTableVpcAssociation
    deleteLocalGatewayRouteTableVpcAssociation_dryRun,
    deleteLocalGatewayRouteTableVpcAssociation_localGatewayRouteTableVpcAssociationId,
    deleteLocalGatewayRouteTableVpcAssociationResponse_localGatewayRouteTableVpcAssociation,
    deleteLocalGatewayRouteTableVpcAssociationResponse_httpStatus,

    -- ** ModifyNetworkInterfaceAttribute
    modifyNetworkInterfaceAttribute_groups,
    modifyNetworkInterfaceAttribute_sourceDestCheck,
    modifyNetworkInterfaceAttribute_attachment,
    modifyNetworkInterfaceAttribute_description,
    modifyNetworkInterfaceAttribute_dryRun,
    modifyNetworkInterfaceAttribute_networkInterfaceId,

    -- ** ModifyVpcTenancy
    modifyVpcTenancy_dryRun,
    modifyVpcTenancy_vpcId,
    modifyVpcTenancy_instanceTenancy,
    modifyVpcTenancyResponse_returnValue,
    modifyVpcTenancyResponse_httpStatus,

    -- ** DescribeInstanceTypes
    describeInstanceTypes_instanceTypes,
    describeInstanceTypes_filters,
    describeInstanceTypes_nextToken,
    describeInstanceTypes_dryRun,
    describeInstanceTypes_maxResults,
    describeInstanceTypesResponse_instanceTypes,
    describeInstanceTypesResponse_nextToken,
    describeInstanceTypesResponse_httpStatus,

    -- ** CancelCapacityReservationFleets
    cancelCapacityReservationFleets_dryRun,
    cancelCapacityReservationFleets_capacityReservationFleetIds,
    cancelCapacityReservationFleetsResponse_failedFleetCancellations,
    cancelCapacityReservationFleetsResponse_successfulFleetCancellations,
    cancelCapacityReservationFleetsResponse_httpStatus,

    -- ** DescribeClientVpnAuthorizationRules
    describeClientVpnAuthorizationRules_filters,
    describeClientVpnAuthorizationRules_nextToken,
    describeClientVpnAuthorizationRules_dryRun,
    describeClientVpnAuthorizationRules_maxResults,
    describeClientVpnAuthorizationRules_clientVpnEndpointId,
    describeClientVpnAuthorizationRulesResponse_authorizationRules,
    describeClientVpnAuthorizationRulesResponse_nextToken,
    describeClientVpnAuthorizationRulesResponse_httpStatus,

    -- ** DeleteTransitGatewayVpcAttachment
    deleteTransitGatewayVpcAttachment_dryRun,
    deleteTransitGatewayVpcAttachment_transitGatewayAttachmentId,
    deleteTransitGatewayVpcAttachmentResponse_transitGatewayVpcAttachment,
    deleteTransitGatewayVpcAttachmentResponse_httpStatus,

    -- ** DeleteTransitGatewayMulticastDomain
    deleteTransitGatewayMulticastDomain_dryRun,
    deleteTransitGatewayMulticastDomain_transitGatewayMulticastDomainId,
    deleteTransitGatewayMulticastDomainResponse_transitGatewayMulticastDomain,
    deleteTransitGatewayMulticastDomainResponse_httpStatus,

    -- ** CancelReservedInstancesListing
    cancelReservedInstancesListing_reservedInstancesListingId,
    cancelReservedInstancesListingResponse_reservedInstancesListings,
    cancelReservedInstancesListingResponse_httpStatus,

    -- ** AttachClassicLinkVpc
    attachClassicLinkVpc_dryRun,
    attachClassicLinkVpc_groups,
    attachClassicLinkVpc_instanceId,
    attachClassicLinkVpc_vpcId,
    attachClassicLinkVpcResponse_return,
    attachClassicLinkVpcResponse_httpStatus,

    -- ** DisableTransitGatewayRouteTablePropagation
    disableTransitGatewayRouteTablePropagation_dryRun,
    disableTransitGatewayRouteTablePropagation_transitGatewayRouteTableId,
    disableTransitGatewayRouteTablePropagation_transitGatewayAttachmentId,
    disableTransitGatewayRouteTablePropagationResponse_propagation,
    disableTransitGatewayRouteTablePropagationResponse_httpStatus,

    -- ** DescribeVpcClassicLinkDnsSupport
    describeVpcClassicLinkDnsSupport_nextToken,
    describeVpcClassicLinkDnsSupport_vpcIds,
    describeVpcClassicLinkDnsSupport_maxResults,
    describeVpcClassicLinkDnsSupportResponse_vpcs,
    describeVpcClassicLinkDnsSupportResponse_nextToken,
    describeVpcClassicLinkDnsSupportResponse_httpStatus,

    -- ** AssociateSubnetCidrBlock
    associateSubnetCidrBlock_ipv6CidrBlock,
    associateSubnetCidrBlock_subnetId,
    associateSubnetCidrBlockResponse_subnetId,
    associateSubnetCidrBlockResponse_ipv6CidrBlockAssociation,
    associateSubnetCidrBlockResponse_httpStatus,

    -- ** CreateNetworkInsightsPath
    createNetworkInsightsPath_destinationIp,
    createNetworkInsightsPath_tagSpecifications,
    createNetworkInsightsPath_sourceIp,
    createNetworkInsightsPath_destinationPort,
    createNetworkInsightsPath_dryRun,
    createNetworkInsightsPath_source,
    createNetworkInsightsPath_destination,
    createNetworkInsightsPath_protocol,
    createNetworkInsightsPath_clientToken,
    createNetworkInsightsPathResponse_networkInsightsPath,
    createNetworkInsightsPathResponse_httpStatus,

    -- ** RunScheduledInstances
    runScheduledInstances_clientToken,
    runScheduledInstances_instanceCount,
    runScheduledInstances_dryRun,
    runScheduledInstances_launchSpecification,
    runScheduledInstances_scheduledInstanceId,
    runScheduledInstancesResponse_instanceIdSet,
    runScheduledInstancesResponse_httpStatus,

    -- ** CreateTransitGatewayRoute
    createTransitGatewayRoute_blackhole,
    createTransitGatewayRoute_transitGatewayAttachmentId,
    createTransitGatewayRoute_dryRun,
    createTransitGatewayRoute_destinationCidrBlock,
    createTransitGatewayRoute_transitGatewayRouteTableId,
    createTransitGatewayRouteResponse_route,
    createTransitGatewayRouteResponse_httpStatus,

    -- ** CreateTransitGatewayPrefixListReference
    createTransitGatewayPrefixListReference_blackhole,
    createTransitGatewayPrefixListReference_transitGatewayAttachmentId,
    createTransitGatewayPrefixListReference_dryRun,
    createTransitGatewayPrefixListReference_transitGatewayRouteTableId,
    createTransitGatewayPrefixListReference_prefixListId,
    createTransitGatewayPrefixListReferenceResponse_transitGatewayPrefixListReference,
    createTransitGatewayPrefixListReferenceResponse_httpStatus,

    -- ** CancelSpotFleetRequests
    cancelSpotFleetRequests_dryRun,
    cancelSpotFleetRequests_spotFleetRequestIds,
    cancelSpotFleetRequests_terminateInstances,
    cancelSpotFleetRequestsResponse_successfulFleetRequests,
    cancelSpotFleetRequestsResponse_unsuccessfulFleetRequests,
    cancelSpotFleetRequestsResponse_httpStatus,

    -- ** ModifyCapacityReservationFleet
    modifyCapacityReservationFleet_endDate,
    modifyCapacityReservationFleet_totalTargetCapacity,
    modifyCapacityReservationFleet_removeEndDate,
    modifyCapacityReservationFleet_dryRun,
    modifyCapacityReservationFleet_capacityReservationFleetId,
    modifyCapacityReservationFleetResponse_return,
    modifyCapacityReservationFleetResponse_httpStatus,

    -- ** DescribeSpotPriceHistory
    describeSpotPriceHistory_instanceTypes,
    describeSpotPriceHistory_startTime,
    describeSpotPriceHistory_filters,
    describeSpotPriceHistory_nextToken,
    describeSpotPriceHistory_availabilityZone,
    describeSpotPriceHistory_endTime,
    describeSpotPriceHistory_productDescriptions,
    describeSpotPriceHistory_dryRun,
    describeSpotPriceHistory_maxResults,
    describeSpotPriceHistoryResponse_nextToken,
    describeSpotPriceHistoryResponse_spotPriceHistory,
    describeSpotPriceHistoryResponse_httpStatus,

    -- ** DeleteTransitGatewayConnectPeer
    deleteTransitGatewayConnectPeer_dryRun,
    deleteTransitGatewayConnectPeer_transitGatewayConnectPeerId,
    deleteTransitGatewayConnectPeerResponse_transitGatewayConnectPeer,
    deleteTransitGatewayConnectPeerResponse_httpStatus,

    -- ** DescribeDhcpOptions
    describeDhcpOptions_filters,
    describeDhcpOptions_dhcpOptionsIds,
    describeDhcpOptions_nextToken,
    describeDhcpOptions_dryRun,
    describeDhcpOptions_maxResults,
    describeDhcpOptionsResponse_dhcpOptions,
    describeDhcpOptionsResponse_nextToken,
    describeDhcpOptionsResponse_httpStatus,

    -- ** ImportImage
    importImage_hypervisor,
    importImage_platform,
    importImage_clientToken,
    importImage_licenseSpecifications,
    importImage_usageOperation,
    importImage_licenseType,
    importImage_roleName,
    importImage_encrypted,
    importImage_tagSpecifications,
    importImage_kmsKeyId,
    importImage_bootMode,
    importImage_architecture,
    importImage_description,
    importImage_dryRun,
    importImage_clientData,
    importImage_diskContainers,
    importImageResponse_status,
    importImageResponse_hypervisor,
    importImageResponse_platform,
    importImageResponse_progress,
    importImageResponse_licenseSpecifications,
    importImageResponse_usageOperation,
    importImageResponse_licenseType,
    importImageResponse_snapshotDetails,
    importImageResponse_encrypted,
    importImageResponse_kmsKeyId,
    importImageResponse_statusMessage,
    importImageResponse_imageId,
    importImageResponse_importTaskId,
    importImageResponse_architecture,
    importImageResponse_description,
    importImageResponse_tags,
    importImageResponse_httpStatus,

    -- ** CreateLocalGatewayRouteTableVpcAssociation
    createLocalGatewayRouteTableVpcAssociation_tagSpecifications,
    createLocalGatewayRouteTableVpcAssociation_dryRun,
    createLocalGatewayRouteTableVpcAssociation_localGatewayRouteTableId,
    createLocalGatewayRouteTableVpcAssociation_vpcId,
    createLocalGatewayRouteTableVpcAssociationResponse_localGatewayRouteTableVpcAssociation,
    createLocalGatewayRouteTableVpcAssociationResponse_httpStatus,

    -- ** CopyFpgaImage
    copyFpgaImage_clientToken,
    copyFpgaImage_name,
    copyFpgaImage_description,
    copyFpgaImage_dryRun,
    copyFpgaImage_sourceFpgaImageId,
    copyFpgaImage_sourceRegion,
    copyFpgaImageResponse_fpgaImageId,
    copyFpgaImageResponse_httpStatus,

    -- ** ImportClientVpnClientCertificateRevocationList
    importClientVpnClientCertificateRevocationList_dryRun,
    importClientVpnClientCertificateRevocationList_clientVpnEndpointId,
    importClientVpnClientCertificateRevocationList_certificateRevocationList,
    importClientVpnClientCertificateRevocationListResponse_return,
    importClientVpnClientCertificateRevocationListResponse_httpStatus,

    -- ** StopInstances
    stopInstances_hibernate,
    stopInstances_force,
    stopInstances_dryRun,
    stopInstances_instanceIds,
    stopInstancesResponse_stoppingInstances,
    stopInstancesResponse_httpStatus,

    -- ** EnableEbsEncryptionByDefault
    enableEbsEncryptionByDefault_dryRun,
    enableEbsEncryptionByDefaultResponse_ebsEncryptionByDefault,
    enableEbsEncryptionByDefaultResponse_httpStatus,

    -- ** ModifyAddressAttribute
    modifyAddressAttribute_domainName,
    modifyAddressAttribute_dryRun,
    modifyAddressAttribute_allocationId,
    modifyAddressAttributeResponse_address,
    modifyAddressAttributeResponse_httpStatus,

    -- ** DeregisterTransitGatewayMulticastGroupSources
    deregisterTransitGatewayMulticastGroupSources_networkInterfaceIds,
    deregisterTransitGatewayMulticastGroupSources_transitGatewayMulticastDomainId,
    deregisterTransitGatewayMulticastGroupSources_groupIpAddress,
    deregisterTransitGatewayMulticastGroupSources_dryRun,
    deregisterTransitGatewayMulticastGroupSourcesResponse_deregisteredMulticastGroupSources,
    deregisterTransitGatewayMulticastGroupSourcesResponse_httpStatus,

    -- ** ModifyLaunchTemplate
    modifyLaunchTemplate_launchTemplateName,
    modifyLaunchTemplate_clientToken,
    modifyLaunchTemplate_launchTemplateId,
    modifyLaunchTemplate_defaultVersion,
    modifyLaunchTemplate_dryRun,
    modifyLaunchTemplateResponse_launchTemplate,
    modifyLaunchTemplateResponse_httpStatus,

    -- ** ModifyVpcEndpointConnectionNotification
    modifyVpcEndpointConnectionNotification_connectionEvents,
    modifyVpcEndpointConnectionNotification_connectionNotificationArn,
    modifyVpcEndpointConnectionNotification_dryRun,
    modifyVpcEndpointConnectionNotification_connectionNotificationId,
    modifyVpcEndpointConnectionNotificationResponse_returnValue,
    modifyVpcEndpointConnectionNotificationResponse_httpStatus,

    -- ** DescribeInternetGateways
    describeInternetGateways_filters,
    describeInternetGateways_nextToken,
    describeInternetGateways_internetGatewayIds,
    describeInternetGateways_dryRun,
    describeInternetGateways_maxResults,
    describeInternetGatewaysResponse_nextToken,
    describeInternetGatewaysResponse_internetGateways,
    describeInternetGatewaysResponse_httpStatus,

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

    -- ** DeleteLaunchTemplateVersions
    deleteLaunchTemplateVersions_launchTemplateName,
    deleteLaunchTemplateVersions_launchTemplateId,
    deleteLaunchTemplateVersions_dryRun,
    deleteLaunchTemplateVersions_versions,
    deleteLaunchTemplateVersionsResponse_successfullyDeletedLaunchTemplateVersions,
    deleteLaunchTemplateVersionsResponse_unsuccessfullyDeletedLaunchTemplateVersions,
    deleteLaunchTemplateVersionsResponse_httpStatus,

    -- ** BundleInstance
    bundleInstance_dryRun,
    bundleInstance_instanceId,
    bundleInstance_storage,
    bundleInstanceResponse_bundleTask,
    bundleInstanceResponse_httpStatus,

    -- ** DescribeNetworkInterfaces
    describeNetworkInterfaces_networkInterfaceIds,
    describeNetworkInterfaces_filters,
    describeNetworkInterfaces_nextToken,
    describeNetworkInterfaces_dryRun,
    describeNetworkInterfaces_maxResults,
    describeNetworkInterfacesResponse_networkInterfaces,
    describeNetworkInterfacesResponse_nextToken,
    describeNetworkInterfacesResponse_httpStatus,

    -- ** ReplaceNetworkAclAssociation
    replaceNetworkAclAssociation_dryRun,
    replaceNetworkAclAssociation_associationId,
    replaceNetworkAclAssociation_networkAclId,
    replaceNetworkAclAssociationResponse_newAssociationId,
    replaceNetworkAclAssociationResponse_httpStatus,

    -- ** AssociateInstanceEventWindow
    associateInstanceEventWindow_dryRun,
    associateInstanceEventWindow_instanceEventWindowId,
    associateInstanceEventWindow_associationTarget,
    associateInstanceEventWindowResponse_instanceEventWindow,
    associateInstanceEventWindowResponse_httpStatus,

    -- ** DescribeNatGateways
    describeNatGateways_natGatewayIds,
    describeNatGateways_nextToken,
    describeNatGateways_filter,
    describeNatGateways_dryRun,
    describeNatGateways_maxResults,
    describeNatGatewaysResponse_natGateways,
    describeNatGatewaysResponse_nextToken,
    describeNatGatewaysResponse_httpStatus,

    -- ** DescribeAddresses
    describeAddresses_filters,
    describeAddresses_publicIps,
    describeAddresses_allocationIds,
    describeAddresses_dryRun,
    describeAddressesResponse_addresses,
    describeAddressesResponse_httpStatus,

    -- ** RestoreManagedPrefixListVersion
    restoreManagedPrefixListVersion_dryRun,
    restoreManagedPrefixListVersion_prefixListId,
    restoreManagedPrefixListVersion_previousVersion,
    restoreManagedPrefixListVersion_currentVersion,
    restoreManagedPrefixListVersionResponse_prefixList,
    restoreManagedPrefixListVersionResponse_httpStatus,

    -- ** DescribeSnapshotAttribute
    describeSnapshotAttribute_dryRun,
    describeSnapshotAttribute_attribute,
    describeSnapshotAttribute_snapshotId,
    describeSnapshotAttributeResponse_createVolumePermissions,
    describeSnapshotAttributeResponse_productCodes,
    describeSnapshotAttributeResponse_snapshotId,
    describeSnapshotAttributeResponse_httpStatus,

    -- ** DescribeIdentityIdFormat
    describeIdentityIdFormat_resource,
    describeIdentityIdFormat_principalArn,
    describeIdentityIdFormatResponse_statuses,
    describeIdentityIdFormatResponse_httpStatus,

    -- ** ReplaceRoute
    replaceRoute_vpcPeeringConnectionId,
    replaceRoute_instanceId,
    replaceRoute_egressOnlyInternetGatewayId,
    replaceRoute_destinationIpv6CidrBlock,
    replaceRoute_localGatewayId,
    replaceRoute_natGatewayId,
    replaceRoute_networkInterfaceId,
    replaceRoute_localTarget,
    replaceRoute_transitGatewayId,
    replaceRoute_gatewayId,
    replaceRoute_vpcEndpointId,
    replaceRoute_destinationPrefixListId,
    replaceRoute_dryRun,
    replaceRoute_carrierGatewayId,
    replaceRoute_destinationCidrBlock,
    replaceRoute_routeTableId,

    -- ** DescribeVpcEndpointServices
    describeVpcEndpointServices_filters,
    describeVpcEndpointServices_serviceNames,
    describeVpcEndpointServices_nextToken,
    describeVpcEndpointServices_dryRun,
    describeVpcEndpointServices_maxResults,
    describeVpcEndpointServicesResponse_serviceDetails,
    describeVpcEndpointServicesResponse_serviceNames,
    describeVpcEndpointServicesResponse_nextToken,
    describeVpcEndpointServicesResponse_httpStatus,

    -- ** DeleteLocalGatewayRoute
    deleteLocalGatewayRoute_dryRun,
    deleteLocalGatewayRoute_destinationCidrBlock,
    deleteLocalGatewayRoute_localGatewayRouteTableId,
    deleteLocalGatewayRouteResponse_route,
    deleteLocalGatewayRouteResponse_httpStatus,

    -- ** AuthorizeSecurityGroupIngress
    authorizeSecurityGroupIngress_fromPort,
    authorizeSecurityGroupIngress_ipPermissions,
    authorizeSecurityGroupIngress_tagSpecifications,
    authorizeSecurityGroupIngress_ipProtocol,
    authorizeSecurityGroupIngress_groupId,
    authorizeSecurityGroupIngress_toPort,
    authorizeSecurityGroupIngress_cidrIp,
    authorizeSecurityGroupIngress_sourceSecurityGroupOwnerId,
    authorizeSecurityGroupIngress_groupName,
    authorizeSecurityGroupIngress_sourceSecurityGroupName,
    authorizeSecurityGroupIngress_dryRun,
    authorizeSecurityGroupIngressResponse_return,
    authorizeSecurityGroupIngressResponse_securityGroupRules,
    authorizeSecurityGroupIngressResponse_httpStatus,

    -- ** CreateVpcPeeringConnection
    createVpcPeeringConnection_peerVpcId,
    createVpcPeeringConnection_vpcId,
    createVpcPeeringConnection_peerOwnerId,
    createVpcPeeringConnection_tagSpecifications,
    createVpcPeeringConnection_peerRegion,
    createVpcPeeringConnection_dryRun,
    createVpcPeeringConnectionResponse_vpcPeeringConnection,
    createVpcPeeringConnectionResponse_httpStatus,

    -- ** DescribeSubnets
    describeSubnets_subnetIds,
    describeSubnets_filters,
    describeSubnets_nextToken,
    describeSubnets_dryRun,
    describeSubnets_maxResults,
    describeSubnetsResponse_subnets,
    describeSubnetsResponse_nextToken,
    describeSubnetsResponse_httpStatus,

    -- ** GetTransitGatewayAttachmentPropagations
    getTransitGatewayAttachmentPropagations_filters,
    getTransitGatewayAttachmentPropagations_nextToken,
    getTransitGatewayAttachmentPropagations_dryRun,
    getTransitGatewayAttachmentPropagations_maxResults,
    getTransitGatewayAttachmentPropagations_transitGatewayAttachmentId,
    getTransitGatewayAttachmentPropagationsResponse_nextToken,
    getTransitGatewayAttachmentPropagationsResponse_transitGatewayAttachmentPropagations,
    getTransitGatewayAttachmentPropagationsResponse_httpStatus,

    -- ** CreateTags
    createTags_dryRun,
    createTags_resources,
    createTags_tags,

    -- ** PurchaseReservedInstancesOffering
    purchaseReservedInstancesOffering_purchaseTime,
    purchaseReservedInstancesOffering_limitPrice,
    purchaseReservedInstancesOffering_dryRun,
    purchaseReservedInstancesOffering_instanceCount,
    purchaseReservedInstancesOffering_reservedInstancesOfferingId,
    purchaseReservedInstancesOfferingResponse_reservedInstancesId,
    purchaseReservedInstancesOfferingResponse_httpStatus,

    -- ** DeleteNetworkAclEntry
    deleteNetworkAclEntry_dryRun,
    deleteNetworkAclEntry_egress,
    deleteNetworkAclEntry_networkAclId,
    deleteNetworkAclEntry_ruleNumber,

    -- ** ResetSnapshotAttribute
    resetSnapshotAttribute_dryRun,
    resetSnapshotAttribute_attribute,
    resetSnapshotAttribute_snapshotId,

    -- ** DescribeVpnConnections
    describeVpnConnections_filters,
    describeVpnConnections_vpnConnectionIds,
    describeVpnConnections_dryRun,
    describeVpnConnectionsResponse_vpnConnections,
    describeVpnConnectionsResponse_httpStatus,

    -- ** ModifyInstanceEventStartTime
    modifyInstanceEventStartTime_dryRun,
    modifyInstanceEventStartTime_instanceId,
    modifyInstanceEventStartTime_instanceEventId,
    modifyInstanceEventStartTime_notBefore,
    modifyInstanceEventStartTimeResponse_event,
    modifyInstanceEventStartTimeResponse_httpStatus,

    -- ** DeleteRoute
    deleteRoute_destinationIpv6CidrBlock,
    deleteRoute_destinationPrefixListId,
    deleteRoute_dryRun,
    deleteRoute_destinationCidrBlock,
    deleteRoute_routeTableId,

    -- ** ReplaceNetworkAclEntry
    replaceNetworkAclEntry_ipv6CidrBlock,
    replaceNetworkAclEntry_icmpTypeCode,
    replaceNetworkAclEntry_portRange,
    replaceNetworkAclEntry_cidrBlock,
    replaceNetworkAclEntry_dryRun,
    replaceNetworkAclEntry_egress,
    replaceNetworkAclEntry_networkAclId,
    replaceNetworkAclEntry_protocol,
    replaceNetworkAclEntry_ruleAction,
    replaceNetworkAclEntry_ruleNumber,

    -- ** DeleteInstanceEventWindow
    deleteInstanceEventWindow_forceDelete,
    deleteInstanceEventWindow_dryRun,
    deleteInstanceEventWindow_instanceEventWindowId,
    deleteInstanceEventWindowResponse_instanceEventWindowState,
    deleteInstanceEventWindowResponse_httpStatus,

    -- ** DescribeVpcEndpoints
    describeVpcEndpoints_filters,
    describeVpcEndpoints_nextToken,
    describeVpcEndpoints_vpcEndpointIds,
    describeVpcEndpoints_dryRun,
    describeVpcEndpoints_maxResults,
    describeVpcEndpointsResponse_nextToken,
    describeVpcEndpointsResponse_vpcEndpoints,
    describeVpcEndpointsResponse_httpStatus,

    -- ** CreateTrafficMirrorFilter
    createTrafficMirrorFilter_clientToken,
    createTrafficMirrorFilter_tagSpecifications,
    createTrafficMirrorFilter_description,
    createTrafficMirrorFilter_dryRun,
    createTrafficMirrorFilterResponse_clientToken,
    createTrafficMirrorFilterResponse_trafficMirrorFilter,
    createTrafficMirrorFilterResponse_httpStatus,

    -- ** ResetInstanceAttribute
    resetInstanceAttribute_dryRun,
    resetInstanceAttribute_attribute,
    resetInstanceAttribute_instanceId,

    -- ** ModifyIdentityIdFormat
    modifyIdentityIdFormat_principalArn,
    modifyIdentityIdFormat_resource,
    modifyIdentityIdFormat_useLongIds,

    -- ** AttachNetworkInterface
    attachNetworkInterface_networkCardIndex,
    attachNetworkInterface_dryRun,
    attachNetworkInterface_deviceIndex,
    attachNetworkInterface_instanceId,
    attachNetworkInterface_networkInterfaceId,
    attachNetworkInterfaceResponse_attachmentId,
    attachNetworkInterfaceResponse_networkCardIndex,
    attachNetworkInterfaceResponse_httpStatus,

    -- ** CreateCapacityReservation
    createCapacityReservation_clientToken,
    createCapacityReservation_availabilityZoneId,
    createCapacityReservation_outpostArn,
    createCapacityReservation_endDate,
    createCapacityReservation_ephemeralStorage,
    createCapacityReservation_instanceMatchCriteria,
    createCapacityReservation_ebsOptimized,
    createCapacityReservation_tagSpecifications,
    createCapacityReservation_availabilityZone,
    createCapacityReservation_tenancy,
    createCapacityReservation_endDateType,
    createCapacityReservation_dryRun,
    createCapacityReservation_instanceType,
    createCapacityReservation_instancePlatform,
    createCapacityReservation_instanceCount,
    createCapacityReservationResponse_capacityReservation,
    createCapacityReservationResponse_httpStatus,

    -- ** DescribeInstanceStatus
    describeInstanceStatus_includeAllInstances,
    describeInstanceStatus_filters,
    describeInstanceStatus_nextToken,
    describeInstanceStatus_instanceIds,
    describeInstanceStatus_dryRun,
    describeInstanceStatus_maxResults,
    describeInstanceStatusResponse_instanceStatuses,
    describeInstanceStatusResponse_nextToken,
    describeInstanceStatusResponse_httpStatus,

    -- ** ImportKeyPair
    importKeyPair_tagSpecifications,
    importKeyPair_dryRun,
    importKeyPair_keyName,
    importKeyPair_publicKeyMaterial,
    importKeyPairResponse_keyFingerprint,
    importKeyPairResponse_keyName,
    importKeyPairResponse_keyPairId,
    importKeyPairResponse_tags,
    importKeyPairResponse_httpStatus,

    -- ** DeleteTags
    deleteTags_dryRun,
    deleteTags_tags,
    deleteTags_resources,

    -- ** ConfirmProductInstance
    confirmProductInstance_dryRun,
    confirmProductInstance_instanceId,
    confirmProductInstance_productCode,
    confirmProductInstanceResponse_return,
    confirmProductInstanceResponse_ownerId,
    confirmProductInstanceResponse_httpStatus,

    -- ** DescribeInstanceAttribute
    describeInstanceAttribute_dryRun,
    describeInstanceAttribute_attribute,
    describeInstanceAttribute_instanceId,
    describeInstanceAttributeResponse_instanceId,
    describeInstanceAttributeResponse_groups,
    describeInstanceAttributeResponse_enaSupport,
    describeInstanceAttributeResponse_sourceDestCheck,
    describeInstanceAttributeResponse_disableApiTermination,
    describeInstanceAttributeResponse_enclaveOptions,
    describeInstanceAttributeResponse_ramdiskId,
    describeInstanceAttributeResponse_kernelId,
    describeInstanceAttributeResponse_rootDeviceName,
    describeInstanceAttributeResponse_instanceType,
    describeInstanceAttributeResponse_sriovNetSupport,
    describeInstanceAttributeResponse_ebsOptimized,
    describeInstanceAttributeResponse_userData,
    describeInstanceAttributeResponse_instanceInitiatedShutdownBehavior,
    describeInstanceAttributeResponse_productCodes,
    describeInstanceAttributeResponse_blockDeviceMappings,
    describeInstanceAttributeResponse_httpStatus,

    -- ** DescribeReservedInstancesOfferings
    describeReservedInstancesOfferings_maxDuration,
    describeReservedInstancesOfferings_productDescription,
    describeReservedInstancesOfferings_filters,
    describeReservedInstancesOfferings_includeMarketplace,
    describeReservedInstancesOfferings_instanceType,
    describeReservedInstancesOfferings_nextToken,
    describeReservedInstancesOfferings_minDuration,
    describeReservedInstancesOfferings_availabilityZone,
    describeReservedInstancesOfferings_offeringType,
    describeReservedInstancesOfferings_reservedInstancesOfferingIds,
    describeReservedInstancesOfferings_instanceTenancy,
    describeReservedInstancesOfferings_offeringClass,
    describeReservedInstancesOfferings_maxInstanceCount,
    describeReservedInstancesOfferings_dryRun,
    describeReservedInstancesOfferings_maxResults,
    describeReservedInstancesOfferingsResponse_nextToken,
    describeReservedInstancesOfferingsResponse_reservedInstancesOfferings,
    describeReservedInstancesOfferingsResponse_httpStatus,

    -- ** CreateCustomerGateway
    createCustomerGateway_certificateArn,
    createCustomerGateway_tagSpecifications,
    createCustomerGateway_deviceName,
    createCustomerGateway_publicIp,
    createCustomerGateway_dryRun,
    createCustomerGateway_bgpAsn,
    createCustomerGateway_type,
    createCustomerGatewayResponse_customerGateway,
    createCustomerGatewayResponse_httpStatus,

    -- ** DescribeNetworkInsightsAnalyses
    describeNetworkInsightsAnalyses_networkInsightsAnalysisIds,
    describeNetworkInsightsAnalyses_analysisEndTime,
    describeNetworkInsightsAnalyses_filters,
    describeNetworkInsightsAnalyses_networkInsightsPathId,
    describeNetworkInsightsAnalyses_nextToken,
    describeNetworkInsightsAnalyses_analysisStartTime,
    describeNetworkInsightsAnalyses_dryRun,
    describeNetworkInsightsAnalyses_maxResults,
    describeNetworkInsightsAnalysesResponse_networkInsightsAnalyses,
    describeNetworkInsightsAnalysesResponse_nextToken,
    describeNetworkInsightsAnalysesResponse_httpStatus,

    -- ** DescribeFleets
    describeFleets_filters,
    describeFleets_nextToken,
    describeFleets_fleetIds,
    describeFleets_dryRun,
    describeFleets_maxResults,
    describeFleetsResponse_nextToken,
    describeFleetsResponse_fleets,
    describeFleetsResponse_httpStatus,

    -- ** DeleteNetworkInsightsAnalysis
    deleteNetworkInsightsAnalysis_dryRun,
    deleteNetworkInsightsAnalysis_networkInsightsAnalysisId,
    deleteNetworkInsightsAnalysisResponse_networkInsightsAnalysisId,
    deleteNetworkInsightsAnalysisResponse_httpStatus,

    -- ** CreateTransitGatewayPeeringAttachment
    createTransitGatewayPeeringAttachment_tagSpecifications,
    createTransitGatewayPeeringAttachment_dryRun,
    createTransitGatewayPeeringAttachment_transitGatewayId,
    createTransitGatewayPeeringAttachment_peerTransitGatewayId,
    createTransitGatewayPeeringAttachment_peerAccountId,
    createTransitGatewayPeeringAttachment_peerRegion,
    createTransitGatewayPeeringAttachmentResponse_transitGatewayPeeringAttachment,
    createTransitGatewayPeeringAttachmentResponse_httpStatus,

    -- ** DeleteSecurityGroup
    deleteSecurityGroup_groupId,
    deleteSecurityGroup_groupName,
    deleteSecurityGroup_dryRun,

    -- ** DescribePublicIpv4Pools
    describePublicIpv4Pools_poolIds,
    describePublicIpv4Pools_filters,
    describePublicIpv4Pools_nextToken,
    describePublicIpv4Pools_maxResults,
    describePublicIpv4PoolsResponse_publicIpv4Pools,
    describePublicIpv4PoolsResponse_nextToken,
    describePublicIpv4PoolsResponse_httpStatus,

    -- ** DescribeClientVpnTargetNetworks
    describeClientVpnTargetNetworks_filters,
    describeClientVpnTargetNetworks_nextToken,
    describeClientVpnTargetNetworks_associationIds,
    describeClientVpnTargetNetworks_dryRun,
    describeClientVpnTargetNetworks_maxResults,
    describeClientVpnTargetNetworks_clientVpnEndpointId,
    describeClientVpnTargetNetworksResponse_clientVpnTargetNetworks,
    describeClientVpnTargetNetworksResponse_nextToken,
    describeClientVpnTargetNetworksResponse_httpStatus,

    -- ** DeleteVpcPeeringConnection
    deleteVpcPeeringConnection_dryRun,
    deleteVpcPeeringConnection_vpcPeeringConnectionId,
    deleteVpcPeeringConnectionResponse_return,
    deleteVpcPeeringConnectionResponse_httpStatus,

    -- ** AttachInternetGateway
    attachInternetGateway_dryRun,
    attachInternetGateway_internetGatewayId,
    attachInternetGateway_vpcId,

    -- ** ModifyInstancePlacement
    modifyInstancePlacement_affinity,
    modifyInstancePlacement_hostId,
    modifyInstancePlacement_partitionNumber,
    modifyInstancePlacement_tenancy,
    modifyInstancePlacement_groupName,
    modifyInstancePlacement_hostResourceGroupArn,
    modifyInstancePlacement_instanceId,
    modifyInstancePlacementResponse_return,
    modifyInstancePlacementResponse_httpStatus,

    -- ** DescribeFlowLogs
    describeFlowLogs_nextToken,
    describeFlowLogs_flowLogIds,
    describeFlowLogs_filter,
    describeFlowLogs_dryRun,
    describeFlowLogs_maxResults,
    describeFlowLogsResponse_nextToken,
    describeFlowLogsResponse_flowLogs,
    describeFlowLogsResponse_httpStatus,

    -- ** DescribeLocalGatewayVirtualInterfaceGroups
    describeLocalGatewayVirtualInterfaceGroups_filters,
    describeLocalGatewayVirtualInterfaceGroups_nextToken,
    describeLocalGatewayVirtualInterfaceGroups_localGatewayVirtualInterfaceGroupIds,
    describeLocalGatewayVirtualInterfaceGroups_dryRun,
    describeLocalGatewayVirtualInterfaceGroups_maxResults,
    describeLocalGatewayVirtualInterfaceGroupsResponse_nextToken,
    describeLocalGatewayVirtualInterfaceGroupsResponse_localGatewayVirtualInterfaceGroups,
    describeLocalGatewayVirtualInterfaceGroupsResponse_httpStatus,

    -- ** DeleteTransitGatewayConnect
    deleteTransitGatewayConnect_dryRun,
    deleteTransitGatewayConnect_transitGatewayAttachmentId,
    deleteTransitGatewayConnectResponse_transitGatewayConnect,
    deleteTransitGatewayConnectResponse_httpStatus,

    -- ** DescribeLocalGatewayRouteTableVpcAssociations
    describeLocalGatewayRouteTableVpcAssociations_localGatewayRouteTableVpcAssociationIds,
    describeLocalGatewayRouteTableVpcAssociations_filters,
    describeLocalGatewayRouteTableVpcAssociations_nextToken,
    describeLocalGatewayRouteTableVpcAssociations_dryRun,
    describeLocalGatewayRouteTableVpcAssociations_maxResults,
    describeLocalGatewayRouteTableVpcAssociationsResponse_localGatewayRouteTableVpcAssociations,
    describeLocalGatewayRouteTableVpcAssociationsResponse_nextToken,
    describeLocalGatewayRouteTableVpcAssociationsResponse_httpStatus,

    -- ** DescribeVpcEndpointConnectionNotifications
    describeVpcEndpointConnectionNotifications_filters,
    describeVpcEndpointConnectionNotifications_nextToken,
    describeVpcEndpointConnectionNotifications_connectionNotificationId,
    describeVpcEndpointConnectionNotifications_dryRun,
    describeVpcEndpointConnectionNotifications_maxResults,
    describeVpcEndpointConnectionNotificationsResponse_connectionNotificationSet,
    describeVpcEndpointConnectionNotificationsResponse_nextToken,
    describeVpcEndpointConnectionNotificationsResponse_httpStatus,

    -- ** GetManagedPrefixListEntries
    getManagedPrefixListEntries_nextToken,
    getManagedPrefixListEntries_targetVersion,
    getManagedPrefixListEntries_dryRun,
    getManagedPrefixListEntries_maxResults,
    getManagedPrefixListEntries_prefixListId,
    getManagedPrefixListEntriesResponse_entries,
    getManagedPrefixListEntriesResponse_nextToken,
    getManagedPrefixListEntriesResponse_httpStatus,

    -- ** DisassociateInstanceEventWindow
    disassociateInstanceEventWindow_dryRun,
    disassociateInstanceEventWindow_instanceEventWindowId,
    disassociateInstanceEventWindow_associationTarget,
    disassociateInstanceEventWindowResponse_instanceEventWindow,
    disassociateInstanceEventWindowResponse_httpStatus,

    -- ** RunInstances
    runInstances_additionalInfo,
    runInstances_securityGroupIds,
    runInstances_securityGroups,
    runInstances_clientToken,
    runInstances_elasticInferenceAccelerators,
    runInstances_instanceMarketOptions,
    runInstances_licenseSpecifications,
    runInstances_disableApiTermination,
    runInstances_keyName,
    runInstances_networkInterfaces,
    runInstances_enclaveOptions,
    runInstances_ramdiskId,
    runInstances_cpuOptions,
    runInstances_subnetId,
    runInstances_kernelId,
    runInstances_instanceType,
    runInstances_capacityReservationSpecification,
    runInstances_ebsOptimized,
    runInstances_userData,
    runInstances_monitoring,
    runInstances_tagSpecifications,
    runInstances_ipv6AddressCount,
    runInstances_hibernationOptions,
    runInstances_iamInstanceProfile,
    runInstances_elasticGpuSpecification,
    runInstances_imageId,
    runInstances_privateIpAddress,
    runInstances_instanceInitiatedShutdownBehavior,
    runInstances_metadataOptions,
    runInstances_launchTemplate,
    runInstances_creditSpecification,
    runInstances_blockDeviceMappings,
    runInstances_dryRun,
    runInstances_placement,
    runInstances_ipv6Addresses,
    runInstances_maxCount,
    runInstances_minCount,
    reservation_groups,
    reservation_instances,
    reservation_requesterId,
    reservation_reservationId,
    reservation_ownerId,

    -- ** CreateSnapshots
    createSnapshots_outpostArn,
    createSnapshots_tagSpecifications,
    createSnapshots_copyTagsFromSource,
    createSnapshots_description,
    createSnapshots_dryRun,
    createSnapshots_instanceSpecification,
    createSnapshotsResponse_snapshots,
    createSnapshotsResponse_httpStatus,

    -- ** AssociateDhcpOptions
    associateDhcpOptions_dryRun,
    associateDhcpOptions_dhcpOptionsId,
    associateDhcpOptions_vpcId,

    -- ** DeleteTrafficMirrorFilterRule
    deleteTrafficMirrorFilterRule_dryRun,
    deleteTrafficMirrorFilterRule_trafficMirrorFilterRuleId,
    deleteTrafficMirrorFilterRuleResponse_trafficMirrorFilterRuleId,
    deleteTrafficMirrorFilterRuleResponse_httpStatus,

    -- ** DescribeReservedInstances
    describeReservedInstances_filters,
    describeReservedInstances_reservedInstancesIds,
    describeReservedInstances_offeringType,
    describeReservedInstances_offeringClass,
    describeReservedInstances_dryRun,
    describeReservedInstancesResponse_reservedInstances,
    describeReservedInstancesResponse_httpStatus,

    -- ** DescribeIdFormat
    describeIdFormat_resource,
    describeIdFormatResponse_statuses,
    describeIdFormatResponse_httpStatus,

    -- ** DescribeVpcs
    describeVpcs_filters,
    describeVpcs_nextToken,
    describeVpcs_vpcIds,
    describeVpcs_dryRun,
    describeVpcs_maxResults,
    describeVpcsResponse_vpcs,
    describeVpcsResponse_nextToken,
    describeVpcsResponse_httpStatus,

    -- ** DescribeConversionTasks
    describeConversionTasks_conversionTaskIds,
    describeConversionTasks_dryRun,
    describeConversionTasksResponse_conversionTasks,
    describeConversionTasksResponse_httpStatus,

    -- ** DisableImageDeprecation
    disableImageDeprecation_dryRun,
    disableImageDeprecation_imageId,
    disableImageDeprecationResponse_return,
    disableImageDeprecationResponse_httpStatus,

    -- ** CreateLaunchTemplateVersion
    createLaunchTemplateVersion_launchTemplateName,
    createLaunchTemplateVersion_clientToken,
    createLaunchTemplateVersion_launchTemplateId,
    createLaunchTemplateVersion_versionDescription,
    createLaunchTemplateVersion_sourceVersion,
    createLaunchTemplateVersion_dryRun,
    createLaunchTemplateVersion_launchTemplateData,
    createLaunchTemplateVersionResponse_launchTemplateVersion,
    createLaunchTemplateVersionResponse_warning,
    createLaunchTemplateVersionResponse_httpStatus,

    -- ** GetManagedPrefixListAssociations
    getManagedPrefixListAssociations_nextToken,
    getManagedPrefixListAssociations_dryRun,
    getManagedPrefixListAssociations_maxResults,
    getManagedPrefixListAssociations_prefixListId,
    getManagedPrefixListAssociationsResponse_nextToken,
    getManagedPrefixListAssociationsResponse_prefixListAssociations,
    getManagedPrefixListAssociationsResponse_httpStatus,

    -- ** DisableVpcClassicLinkDnsSupport
    disableVpcClassicLinkDnsSupport_vpcId,
    disableVpcClassicLinkDnsSupportResponse_return,
    disableVpcClassicLinkDnsSupportResponse_httpStatus,

    -- ** ApplySecurityGroupsToClientVpnTargetNetwork
    applySecurityGroupsToClientVpnTargetNetwork_dryRun,
    applySecurityGroupsToClientVpnTargetNetwork_clientVpnEndpointId,
    applySecurityGroupsToClientVpnTargetNetwork_vpcId,
    applySecurityGroupsToClientVpnTargetNetwork_securityGroupIds,
    applySecurityGroupsToClientVpnTargetNetworkResponse_securityGroupIds,
    applySecurityGroupsToClientVpnTargetNetworkResponse_httpStatus,

    -- ** DescribeTrafficMirrorTargets
    describeTrafficMirrorTargets_filters,
    describeTrafficMirrorTargets_nextToken,
    describeTrafficMirrorTargets_trafficMirrorTargetIds,
    describeTrafficMirrorTargets_dryRun,
    describeTrafficMirrorTargets_maxResults,
    describeTrafficMirrorTargetsResponse_trafficMirrorTargets,
    describeTrafficMirrorTargetsResponse_nextToken,
    describeTrafficMirrorTargetsResponse_httpStatus,

    -- ** DescribeVolumesModifications
    describeVolumesModifications_filters,
    describeVolumesModifications_volumeIds,
    describeVolumesModifications_nextToken,
    describeVolumesModifications_dryRun,
    describeVolumesModifications_maxResults,
    describeVolumesModificationsResponse_volumesModifications,
    describeVolumesModificationsResponse_nextToken,
    describeVolumesModificationsResponse_httpStatus,

    -- ** ExportImage
    exportImage_clientToken,
    exportImage_roleName,
    exportImage_tagSpecifications,
    exportImage_description,
    exportImage_dryRun,
    exportImage_diskImageFormat,
    exportImage_imageId,
    exportImage_s3ExportLocation,
    exportImageResponse_status,
    exportImageResponse_progress,
    exportImageResponse_exportImageTaskId,
    exportImageResponse_roleName,
    exportImageResponse_statusMessage,
    exportImageResponse_imageId,
    exportImageResponse_description,
    exportImageResponse_tags,
    exportImageResponse_s3ExportLocation,
    exportImageResponse_diskImageFormat,
    exportImageResponse_httpStatus,

    -- ** CreateFpgaImage
    createFpgaImage_clientToken,
    createFpgaImage_logsStorageLocation,
    createFpgaImage_tagSpecifications,
    createFpgaImage_name,
    createFpgaImage_description,
    createFpgaImage_dryRun,
    createFpgaImage_inputStorageLocation,
    createFpgaImageResponse_fpgaImageId,
    createFpgaImageResponse_fpgaImageGlobalId,
    createFpgaImageResponse_httpStatus,

    -- ** AcceptVpcEndpointConnections
    acceptVpcEndpointConnections_dryRun,
    acceptVpcEndpointConnections_serviceId,
    acceptVpcEndpointConnections_vpcEndpointIds,
    acceptVpcEndpointConnectionsResponse_unsuccessful,
    acceptVpcEndpointConnectionsResponse_httpStatus,

    -- ** DeleteClientVpnEndpoint
    deleteClientVpnEndpoint_dryRun,
    deleteClientVpnEndpoint_clientVpnEndpointId,
    deleteClientVpnEndpointResponse_status,
    deleteClientVpnEndpointResponse_httpStatus,

    -- ** SearchTransitGatewayRoutes
    searchTransitGatewayRoutes_dryRun,
    searchTransitGatewayRoutes_maxResults,
    searchTransitGatewayRoutes_transitGatewayRouteTableId,
    searchTransitGatewayRoutes_filters,
    searchTransitGatewayRoutesResponse_additionalRoutesAvailable,
    searchTransitGatewayRoutesResponse_routes,
    searchTransitGatewayRoutesResponse_httpStatus,

    -- ** GetLaunchTemplateData
    getLaunchTemplateData_dryRun,
    getLaunchTemplateData_instanceId,
    getLaunchTemplateDataResponse_launchTemplateData,
    getLaunchTemplateDataResponse_httpStatus,

    -- ** AllocateAddress
    allocateAddress_networkBorderGroup,
    allocateAddress_domain,
    allocateAddress_address,
    allocateAddress_publicIpv4Pool,
    allocateAddress_tagSpecifications,
    allocateAddress_customerOwnedIpv4Pool,
    allocateAddress_dryRun,
    allocateAddressResponse_allocationId,
    allocateAddressResponse_carrierIp,
    allocateAddressResponse_networkBorderGroup,
    allocateAddressResponse_domain,
    allocateAddressResponse_publicIpv4Pool,
    allocateAddressResponse_customerOwnedIpv4Pool,
    allocateAddressResponse_customerOwnedIp,
    allocateAddressResponse_publicIp,
    allocateAddressResponse_httpStatus,

    -- ** AcceptTransitGatewayVpcAttachment
    acceptTransitGatewayVpcAttachment_dryRun,
    acceptTransitGatewayVpcAttachment_transitGatewayAttachmentId,
    acceptTransitGatewayVpcAttachmentResponse_transitGatewayVpcAttachment,
    acceptTransitGatewayVpcAttachmentResponse_httpStatus,

    -- ** CancelConversionTask
    cancelConversionTask_reasonMessage,
    cancelConversionTask_dryRun,
    cancelConversionTask_conversionTaskId,

    -- ** ModifyImageAttribute
    modifyImageAttribute_attribute,
    modifyImageAttribute_userIds,
    modifyImageAttribute_userGroups,
    modifyImageAttribute_value,
    modifyImageAttribute_launchPermission,
    modifyImageAttribute_operationType,
    modifyImageAttribute_productCodes,
    modifyImageAttribute_description,
    modifyImageAttribute_dryRun,
    modifyImageAttribute_imageId,

    -- ** CreateRouteTable
    createRouteTable_tagSpecifications,
    createRouteTable_dryRun,
    createRouteTable_vpcId,
    createRouteTableResponse_routeTable,
    createRouteTableResponse_httpStatus,

    -- ** RejectTransitGatewayPeeringAttachment
    rejectTransitGatewayPeeringAttachment_dryRun,
    rejectTransitGatewayPeeringAttachment_transitGatewayAttachmentId,
    rejectTransitGatewayPeeringAttachmentResponse_transitGatewayPeeringAttachment,
    rejectTransitGatewayPeeringAttachmentResponse_httpStatus,

    -- ** ReportInstanceStatus
    reportInstanceStatus_startTime,
    reportInstanceStatus_endTime,
    reportInstanceStatus_description,
    reportInstanceStatus_dryRun,
    reportInstanceStatus_instances,
    reportInstanceStatus_reasonCodes,
    reportInstanceStatus_status,

    -- ** AttachVolume
    attachVolume_dryRun,
    attachVolume_device,
    attachVolume_instanceId,
    attachVolume_volumeId,
    volumeAttachment_instanceId,
    volumeAttachment_deleteOnTermination,
    volumeAttachment_state,
    volumeAttachment_device,
    volumeAttachment_volumeId,
    volumeAttachment_attachTime,

    -- ** RequestSpotInstances
    requestSpotInstances_blockDurationMinutes,
    requestSpotInstances_clientToken,
    requestSpotInstances_instanceCount,
    requestSpotInstances_instanceInterruptionBehavior,
    requestSpotInstances_spotPrice,
    requestSpotInstances_launchSpecification,
    requestSpotInstances_availabilityZoneGroup,
    requestSpotInstances_tagSpecifications,
    requestSpotInstances_validUntil,
    requestSpotInstances_launchGroup,
    requestSpotInstances_type,
    requestSpotInstances_validFrom,
    requestSpotInstances_dryRun,
    requestSpotInstancesResponse_spotInstanceRequests,
    requestSpotInstancesResponse_httpStatus,

    -- ** WithdrawByoipCidr
    withdrawByoipCidr_dryRun,
    withdrawByoipCidr_cidr,
    withdrawByoipCidrResponse_byoipCidr,
    withdrawByoipCidrResponse_httpStatus,

    -- ** DescribeHostReservationOfferings
    describeHostReservationOfferings_maxDuration,
    describeHostReservationOfferings_nextToken,
    describeHostReservationOfferings_minDuration,
    describeHostReservationOfferings_offeringId,
    describeHostReservationOfferings_filter,
    describeHostReservationOfferings_maxResults,
    describeHostReservationOfferingsResponse_offeringSet,
    describeHostReservationOfferingsResponse_nextToken,
    describeHostReservationOfferingsResponse_httpStatus,

    -- ** ResetFpgaImageAttribute
    resetFpgaImageAttribute_attribute,
    resetFpgaImageAttribute_dryRun,
    resetFpgaImageAttribute_fpgaImageId,
    resetFpgaImageAttributeResponse_return,
    resetFpgaImageAttributeResponse_httpStatus,

    -- ** ModifyVpnConnection
    modifyVpnConnection_vpnGatewayId,
    modifyVpnConnection_customerGatewayId,
    modifyVpnConnection_transitGatewayId,
    modifyVpnConnection_dryRun,
    modifyVpnConnection_vpnConnectionId,
    modifyVpnConnectionResponse_vpnConnection,
    modifyVpnConnectionResponse_httpStatus,

    -- ** CreateTrafficMirrorFilterRule
    createTrafficMirrorFilterRule_clientToken,
    createTrafficMirrorFilterRule_protocol,
    createTrafficMirrorFilterRule_destinationPortRange,
    createTrafficMirrorFilterRule_sourcePortRange,
    createTrafficMirrorFilterRule_description,
    createTrafficMirrorFilterRule_dryRun,
    createTrafficMirrorFilterRule_trafficMirrorFilterId,
    createTrafficMirrorFilterRule_trafficDirection,
    createTrafficMirrorFilterRule_ruleNumber,
    createTrafficMirrorFilterRule_ruleAction,
    createTrafficMirrorFilterRule_destinationCidrBlock,
    createTrafficMirrorFilterRule_sourceCidrBlock,
    createTrafficMirrorFilterRuleResponse_trafficMirrorFilterRule,
    createTrafficMirrorFilterRuleResponse_clientToken,
    createTrafficMirrorFilterRuleResponse_httpStatus,

    -- ** DeleteTransitGateway
    deleteTransitGateway_dryRun,
    deleteTransitGateway_transitGatewayId,
    deleteTransitGatewayResponse_transitGateway,
    deleteTransitGatewayResponse_httpStatus,

    -- ** StartVpcEndpointServicePrivateDnsVerification
    startVpcEndpointServicePrivateDnsVerification_dryRun,
    startVpcEndpointServicePrivateDnsVerification_serviceId,
    startVpcEndpointServicePrivateDnsVerificationResponse_returnValue,
    startVpcEndpointServicePrivateDnsVerificationResponse_httpStatus,

    -- ** DescribeVolumes
    describeVolumes_filters,
    describeVolumes_volumeIds,
    describeVolumes_nextToken,
    describeVolumes_dryRun,
    describeVolumes_maxResults,
    describeVolumesResponse_nextToken,
    describeVolumesResponse_volumes,
    describeVolumesResponse_httpStatus,

    -- ** RejectVpcPeeringConnection
    rejectVpcPeeringConnection_dryRun,
    rejectVpcPeeringConnection_vpcPeeringConnectionId,
    rejectVpcPeeringConnectionResponse_return,
    rejectVpcPeeringConnectionResponse_httpStatus,

    -- ** DescribeClientVpnRoutes
    describeClientVpnRoutes_filters,
    describeClientVpnRoutes_nextToken,
    describeClientVpnRoutes_dryRun,
    describeClientVpnRoutes_maxResults,
    describeClientVpnRoutes_clientVpnEndpointId,
    describeClientVpnRoutesResponse_routes,
    describeClientVpnRoutesResponse_nextToken,
    describeClientVpnRoutesResponse_httpStatus,

    -- ** DeleteVpnConnectionRoute
    deleteVpnConnectionRoute_destinationCidrBlock,
    deleteVpnConnectionRoute_vpnConnectionId,

    -- ** AssociateEnclaveCertificateIamRole
    associateEnclaveCertificateIamRole_certificateArn,
    associateEnclaveCertificateIamRole_dryRun,
    associateEnclaveCertificateIamRole_roleArn,
    associateEnclaveCertificateIamRoleResponse_certificateS3BucketName,
    associateEnclaveCertificateIamRoleResponse_certificateS3ObjectKey,
    associateEnclaveCertificateIamRoleResponse_encryptionKmsKeyId,
    associateEnclaveCertificateIamRoleResponse_httpStatus,

    -- ** ModifyVpcEndpoint
    modifyVpcEndpoint_policyDocument,
    modifyVpcEndpoint_removeRouteTableIds,
    modifyVpcEndpoint_resetPolicy,
    modifyVpcEndpoint_addRouteTableIds,
    modifyVpcEndpoint_privateDnsEnabled,
    modifyVpcEndpoint_addSubnetIds,
    modifyVpcEndpoint_removeSubnetIds,
    modifyVpcEndpoint_addSecurityGroupIds,
    modifyVpcEndpoint_dryRun,
    modifyVpcEndpoint_removeSecurityGroupIds,
    modifyVpcEndpoint_vpcEndpointId,
    modifyVpcEndpointResponse_return,
    modifyVpcEndpointResponse_httpStatus,

    -- ** DescribeFpgaImageAttribute
    describeFpgaImageAttribute_dryRun,
    describeFpgaImageAttribute_fpgaImageId,
    describeFpgaImageAttribute_attribute,
    describeFpgaImageAttributeResponse_fpgaImageAttribute,
    describeFpgaImageAttributeResponse_httpStatus,

    -- ** AllocateHosts
    allocateHosts_instanceFamily,
    allocateHosts_clientToken,
    allocateHosts_instanceType,
    allocateHosts_tagSpecifications,
    allocateHosts_hostRecovery,
    allocateHosts_autoPlacement,
    allocateHosts_availabilityZone,
    allocateHosts_quantity,
    allocateHostsResponse_hostIds,
    allocateHostsResponse_httpStatus,

    -- ** CreateClientVpnEndpoint
    createClientVpnEndpoint_securityGroupIds,
    createClientVpnEndpoint_splitTunnel,
    createClientVpnEndpoint_clientToken,
    createClientVpnEndpoint_transportProtocol,
    createClientVpnEndpoint_vpcId,
    createClientVpnEndpoint_vpnPort,
    createClientVpnEndpoint_tagSpecifications,
    createClientVpnEndpoint_dnsServers,
    createClientVpnEndpoint_clientConnectOptions,
    createClientVpnEndpoint_selfServicePortal,
    createClientVpnEndpoint_description,
    createClientVpnEndpoint_dryRun,
    createClientVpnEndpoint_clientCidrBlock,
    createClientVpnEndpoint_serverCertificateArn,
    createClientVpnEndpoint_authenticationOptions,
    createClientVpnEndpoint_connectionLogOptions,
    createClientVpnEndpointResponse_status,
    createClientVpnEndpointResponse_clientVpnEndpointId,
    createClientVpnEndpointResponse_dnsName,
    createClientVpnEndpointResponse_httpStatus,

    -- ** CreateTrafficMirrorSession
    createTrafficMirrorSession_clientToken,
    createTrafficMirrorSession_packetLength,
    createTrafficMirrorSession_tagSpecifications,
    createTrafficMirrorSession_virtualNetworkId,
    createTrafficMirrorSession_description,
    createTrafficMirrorSession_dryRun,
    createTrafficMirrorSession_networkInterfaceId,
    createTrafficMirrorSession_trafficMirrorTargetId,
    createTrafficMirrorSession_trafficMirrorFilterId,
    createTrafficMirrorSession_sessionNumber,
    createTrafficMirrorSessionResponse_trafficMirrorSession,
    createTrafficMirrorSessionResponse_clientToken,
    createTrafficMirrorSessionResponse_httpStatus,

    -- ** RegisterImage
    registerImage_virtualizationType,
    registerImage_imageLocation,
    registerImage_enaSupport,
    registerImage_billingProducts,
    registerImage_ramdiskId,
    registerImage_kernelId,
    registerImage_rootDeviceName,
    registerImage_sriovNetSupport,
    registerImage_bootMode,
    registerImage_architecture,
    registerImage_description,
    registerImage_blockDeviceMappings,
    registerImage_dryRun,
    registerImage_name,
    registerImageResponse_imageId,
    registerImageResponse_httpStatus,

    -- ** AdvertiseByoipCidr
    advertiseByoipCidr_dryRun,
    advertiseByoipCidr_cidr,
    advertiseByoipCidrResponse_byoipCidr,
    advertiseByoipCidrResponse_httpStatus,

    -- ** ModifyFleet
    modifyFleet_context,
    modifyFleet_targetCapacitySpecification,
    modifyFleet_excessCapacityTerminationPolicy,
    modifyFleet_launchTemplateConfigs,
    modifyFleet_dryRun,
    modifyFleet_fleetId,
    modifyFleetResponse_return,
    modifyFleetResponse_httpStatus,

    -- ** RevokeSecurityGroupIngress
    revokeSecurityGroupIngress_fromPort,
    revokeSecurityGroupIngress_ipPermissions,
    revokeSecurityGroupIngress_ipProtocol,
    revokeSecurityGroupIngress_groupId,
    revokeSecurityGroupIngress_toPort,
    revokeSecurityGroupIngress_cidrIp,
    revokeSecurityGroupIngress_sourceSecurityGroupOwnerId,
    revokeSecurityGroupIngress_groupName,
    revokeSecurityGroupIngress_sourceSecurityGroupName,
    revokeSecurityGroupIngress_dryRun,
    revokeSecurityGroupIngress_securityGroupRuleIds,
    revokeSecurityGroupIngressResponse_return,
    revokeSecurityGroupIngressResponse_unknownIpPermissions,
    revokeSecurityGroupIngressResponse_httpStatus,

    -- ** GetEbsDefaultKmsKeyId
    getEbsDefaultKmsKeyId_dryRun,
    getEbsDefaultKmsKeyIdResponse_kmsKeyId,
    getEbsDefaultKmsKeyIdResponse_httpStatus,

    -- ** DescribeHostReservations
    describeHostReservations_nextToken,
    describeHostReservations_hostReservationIdSet,
    describeHostReservations_filter,
    describeHostReservations_maxResults,
    describeHostReservationsResponse_nextToken,
    describeHostReservationsResponse_hostReservationSet,
    describeHostReservationsResponse_httpStatus,

    -- ** UpdateSecurityGroupRuleDescriptionsEgress
    updateSecurityGroupRuleDescriptionsEgress_securityGroupRuleDescriptions,
    updateSecurityGroupRuleDescriptionsEgress_ipPermissions,
    updateSecurityGroupRuleDescriptionsEgress_groupId,
    updateSecurityGroupRuleDescriptionsEgress_groupName,
    updateSecurityGroupRuleDescriptionsEgress_dryRun,
    updateSecurityGroupRuleDescriptionsEgressResponse_return,
    updateSecurityGroupRuleDescriptionsEgressResponse_httpStatus,

    -- ** EnableVpcClassicLinkDnsSupport
    enableVpcClassicLinkDnsSupport_vpcId,
    enableVpcClassicLinkDnsSupportResponse_return,
    enableVpcClassicLinkDnsSupportResponse_httpStatus,

    -- ** DescribeVpcEndpointConnections
    describeVpcEndpointConnections_filters,
    describeVpcEndpointConnections_nextToken,
    describeVpcEndpointConnections_dryRun,
    describeVpcEndpointConnections_maxResults,
    describeVpcEndpointConnectionsResponse_vpcEndpointConnections,
    describeVpcEndpointConnectionsResponse_nextToken,
    describeVpcEndpointConnectionsResponse_httpStatus,

    -- ** ModifyReservedInstances
    modifyReservedInstances_clientToken,
    modifyReservedInstances_reservedInstancesIds,
    modifyReservedInstances_targetConfigurations,
    modifyReservedInstancesResponse_reservedInstancesModificationId,
    modifyReservedInstancesResponse_httpStatus,

    -- ** DeleteFpgaImage
    deleteFpgaImage_dryRun,
    deleteFpgaImage_fpgaImageId,
    deleteFpgaImageResponse_return,
    deleteFpgaImageResponse_httpStatus,

    -- ** DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
    describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_filters,
    describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_localGatewayRouteTableVirtualInterfaceGroupAssociationIds,
    describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_nextToken,
    describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_dryRun,
    describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_maxResults,
    describeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse_nextToken,
    describeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse_localGatewayRouteTableVirtualInterfaceGroupAssociations,
    describeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse_httpStatus,

    -- ** EnableImageDeprecation
    enableImageDeprecation_dryRun,
    enableImageDeprecation_imageId,
    enableImageDeprecation_deprecateAt,
    enableImageDeprecationResponse_return,
    enableImageDeprecationResponse_httpStatus,

    -- ** DescribeScheduledInstances
    describeScheduledInstances_filters,
    describeScheduledInstances_slotStartTimeRange,
    describeScheduledInstances_nextToken,
    describeScheduledInstances_scheduledInstanceIds,
    describeScheduledInstances_dryRun,
    describeScheduledInstances_maxResults,
    describeScheduledInstancesResponse_nextToken,
    describeScheduledInstancesResponse_scheduledInstanceSet,
    describeScheduledInstancesResponse_httpStatus,

    -- ** SearchTransitGatewayMulticastGroups
    searchTransitGatewayMulticastGroups_filters,
    searchTransitGatewayMulticastGroups_transitGatewayMulticastDomainId,
    searchTransitGatewayMulticastGroups_nextToken,
    searchTransitGatewayMulticastGroups_dryRun,
    searchTransitGatewayMulticastGroups_maxResults,
    searchTransitGatewayMulticastGroupsResponse_nextToken,
    searchTransitGatewayMulticastGroupsResponse_multicastGroups,
    searchTransitGatewayMulticastGroupsResponse_httpStatus,

    -- ** CreateFlowLogs
    createFlowLogs_logFormat,
    createFlowLogs_maxAggregationInterval,
    createFlowLogs_clientToken,
    createFlowLogs_logDestination,
    createFlowLogs_logGroupName,
    createFlowLogs_tagSpecifications,
    createFlowLogs_destinationOptions,
    createFlowLogs_deliverLogsPermissionArn,
    createFlowLogs_logDestinationType,
    createFlowLogs_dryRun,
    createFlowLogs_resourceIds,
    createFlowLogs_resourceType,
    createFlowLogs_trafficType,
    createFlowLogsResponse_unsuccessful,
    createFlowLogsResponse_clientToken,
    createFlowLogsResponse_flowLogIds,
    createFlowLogsResponse_httpStatus,

    -- ** DescribeSpotFleetRequests
    describeSpotFleetRequests_spotFleetRequestIds,
    describeSpotFleetRequests_nextToken,
    describeSpotFleetRequests_dryRun,
    describeSpotFleetRequests_maxResults,
    describeSpotFleetRequestsResponse_nextToken,
    describeSpotFleetRequestsResponse_spotFleetRequestConfigs,
    describeSpotFleetRequestsResponse_httpStatus,

    -- ** MoveAddressToVpc
    moveAddressToVpc_dryRun,
    moveAddressToVpc_publicIp,
    moveAddressToVpcResponse_status,
    moveAddressToVpcResponse_allocationId,
    moveAddressToVpcResponse_httpStatus,

    -- ** DescribeFleetInstances
    describeFleetInstances_filters,
    describeFleetInstances_nextToken,
    describeFleetInstances_dryRun,
    describeFleetInstances_maxResults,
    describeFleetInstances_fleetId,
    describeFleetInstancesResponse_nextToken,
    describeFleetInstancesResponse_fleetId,
    describeFleetInstancesResponse_activeInstances,
    describeFleetInstancesResponse_httpStatus,

    -- ** DescribeLaunchTemplateVersions
    describeLaunchTemplateVersions_launchTemplateName,
    describeLaunchTemplateVersions_launchTemplateId,
    describeLaunchTemplateVersions_minVersion,
    describeLaunchTemplateVersions_filters,
    describeLaunchTemplateVersions_maxVersion,
    describeLaunchTemplateVersions_versions,
    describeLaunchTemplateVersions_nextToken,
    describeLaunchTemplateVersions_dryRun,
    describeLaunchTemplateVersions_maxResults,
    describeLaunchTemplateVersionsResponse_nextToken,
    describeLaunchTemplateVersionsResponse_launchTemplateVersions,
    describeLaunchTemplateVersionsResponse_httpStatus,

    -- ** StartNetworkInsightsAnalysis
    startNetworkInsightsAnalysis_filterInArns,
    startNetworkInsightsAnalysis_tagSpecifications,
    startNetworkInsightsAnalysis_dryRun,
    startNetworkInsightsAnalysis_networkInsightsPathId,
    startNetworkInsightsAnalysis_clientToken,
    startNetworkInsightsAnalysisResponse_networkInsightsAnalysis,
    startNetworkInsightsAnalysisResponse_httpStatus,

    -- ** ModifyInstanceCreditSpecification
    modifyInstanceCreditSpecification_clientToken,
    modifyInstanceCreditSpecification_dryRun,
    modifyInstanceCreditSpecification_instanceCreditSpecifications,
    modifyInstanceCreditSpecificationResponse_unsuccessfulInstanceCreditSpecifications,
    modifyInstanceCreditSpecificationResponse_successfulInstanceCreditSpecifications,
    modifyInstanceCreditSpecificationResponse_httpStatus,

    -- ** DescribePrincipalIdFormat
    describePrincipalIdFormat_resources,
    describePrincipalIdFormat_nextToken,
    describePrincipalIdFormat_dryRun,
    describePrincipalIdFormat_maxResults,
    describePrincipalIdFormatResponse_principals,
    describePrincipalIdFormatResponse_nextToken,
    describePrincipalIdFormatResponse_httpStatus,

    -- ** DescribeTransitGateways
    describeTransitGateways_filters,
    describeTransitGateways_transitGatewayIds,
    describeTransitGateways_nextToken,
    describeTransitGateways_dryRun,
    describeTransitGateways_maxResults,
    describeTransitGatewaysResponse_transitGateways,
    describeTransitGatewaysResponse_nextToken,
    describeTransitGatewaysResponse_httpStatus,

    -- ** DeleteNetworkAcl
    deleteNetworkAcl_dryRun,
    deleteNetworkAcl_networkAclId,

    -- ** DisassociateTransitGatewayMulticastDomain
    disassociateTransitGatewayMulticastDomain_subnetIds,
    disassociateTransitGatewayMulticastDomain_transitGatewayMulticastDomainId,
    disassociateTransitGatewayMulticastDomain_transitGatewayAttachmentId,
    disassociateTransitGatewayMulticastDomain_dryRun,
    disassociateTransitGatewayMulticastDomainResponse_associations,
    disassociateTransitGatewayMulticastDomainResponse_httpStatus,

    -- ** DeleteTransitGatewayRouteTable
    deleteTransitGatewayRouteTable_dryRun,
    deleteTransitGatewayRouteTable_transitGatewayRouteTableId,
    deleteTransitGatewayRouteTableResponse_transitGatewayRouteTable,
    deleteTransitGatewayRouteTableResponse_httpStatus,

    -- ** DescribeSecurityGroupRules
    describeSecurityGroupRules_filters,
    describeSecurityGroupRules_nextToken,
    describeSecurityGroupRules_dryRun,
    describeSecurityGroupRules_maxResults,
    describeSecurityGroupRules_securityGroupRuleIds,
    describeSecurityGroupRulesResponse_securityGroupRules,
    describeSecurityGroupRulesResponse_nextToken,
    describeSecurityGroupRulesResponse_httpStatus,

    -- ** CreateLaunchTemplate
    createLaunchTemplate_clientToken,
    createLaunchTemplate_versionDescription,
    createLaunchTemplate_tagSpecifications,
    createLaunchTemplate_dryRun,
    createLaunchTemplate_launchTemplateName,
    createLaunchTemplate_launchTemplateData,
    createLaunchTemplateResponse_warning,
    createLaunchTemplateResponse_launchTemplate,
    createLaunchTemplateResponse_httpStatus,

    -- ** CreateVpcEndpointConnectionNotification
    createVpcEndpointConnectionNotification_clientToken,
    createVpcEndpointConnectionNotification_serviceId,
    createVpcEndpointConnectionNotification_vpcEndpointId,
    createVpcEndpointConnectionNotification_dryRun,
    createVpcEndpointConnectionNotification_connectionNotificationArn,
    createVpcEndpointConnectionNotification_connectionEvents,
    createVpcEndpointConnectionNotificationResponse_clientToken,
    createVpcEndpointConnectionNotificationResponse_connectionNotification,
    createVpcEndpointConnectionNotificationResponse_httpStatus,

    -- ** DeleteNetworkInterfacePermission
    deleteNetworkInterfacePermission_force,
    deleteNetworkInterfacePermission_dryRun,
    deleteNetworkInterfacePermission_networkInterfacePermissionId,
    deleteNetworkInterfacePermissionResponse_return,
    deleteNetworkInterfacePermissionResponse_httpStatus,

    -- ** DeleteVpnGateway
    deleteVpnGateway_dryRun,
    deleteVpnGateway_vpnGatewayId,

    -- ** CreateStoreImageTask
    createStoreImageTask_s3ObjectTags,
    createStoreImageTask_dryRun,
    createStoreImageTask_imageId,
    createStoreImageTask_bucket,
    createStoreImageTaskResponse_objectKey,
    createStoreImageTaskResponse_httpStatus,

    -- ** CreateTrafficMirrorTarget
    createTrafficMirrorTarget_clientToken,
    createTrafficMirrorTarget_networkInterfaceId,
    createTrafficMirrorTarget_networkLoadBalancerArn,
    createTrafficMirrorTarget_tagSpecifications,
    createTrafficMirrorTarget_description,
    createTrafficMirrorTarget_dryRun,
    createTrafficMirrorTargetResponse_clientToken,
    createTrafficMirrorTargetResponse_trafficMirrorTarget,
    createTrafficMirrorTargetResponse_httpStatus,

    -- ** DescribeImportImageTasks
    describeImportImageTasks_filters,
    describeImportImageTasks_importTaskIds,
    describeImportImageTasks_nextToken,
    describeImportImageTasks_dryRun,
    describeImportImageTasks_maxResults,
    describeImportImageTasksResponse_nextToken,
    describeImportImageTasksResponse_importImageTasks,
    describeImportImageTasksResponse_httpStatus,

    -- ** DescribeVolumeAttribute
    describeVolumeAttribute_dryRun,
    describeVolumeAttribute_attribute,
    describeVolumeAttribute_volumeId,
    describeVolumeAttributeResponse_volumeId,
    describeVolumeAttributeResponse_productCodes,
    describeVolumeAttributeResponse_autoEnableIO,
    describeVolumeAttributeResponse_httpStatus,

    -- ** DescribeMovingAddresses
    describeMovingAddresses_filters,
    describeMovingAddresses_publicIps,
    describeMovingAddresses_nextToken,
    describeMovingAddresses_dryRun,
    describeMovingAddresses_maxResults,
    describeMovingAddressesResponse_movingAddressStatuses,
    describeMovingAddressesResponse_nextToken,
    describeMovingAddressesResponse_httpStatus,

    -- ** ExportTransitGatewayRoutes
    exportTransitGatewayRoutes_filters,
    exportTransitGatewayRoutes_dryRun,
    exportTransitGatewayRoutes_transitGatewayRouteTableId,
    exportTransitGatewayRoutes_s3Bucket,
    exportTransitGatewayRoutesResponse_s3Location,
    exportTransitGatewayRoutesResponse_httpStatus,

    -- ** GetPasswordData
    getPasswordData_dryRun,
    getPasswordData_instanceId,
    getPasswordDataResponse_httpStatus,
    getPasswordDataResponse_instanceId,
    getPasswordDataResponse_passwordData,
    getPasswordDataResponse_timestamp,

    -- ** CreateVpc
    createVpc_ipv6CidrBlock,
    createVpc_ipv6CidrBlockNetworkBorderGroup,
    createVpc_tagSpecifications,
    createVpc_ipv6Pool,
    createVpc_amazonProvidedIpv6CidrBlock,
    createVpc_instanceTenancy,
    createVpc_dryRun,
    createVpc_cidrBlock,
    createVpcResponse_vpc,
    createVpcResponse_httpStatus,

    -- ** ModifyVpcPeeringConnectionOptions
    modifyVpcPeeringConnectionOptions_requesterPeeringConnectionOptions,
    modifyVpcPeeringConnectionOptions_accepterPeeringConnectionOptions,
    modifyVpcPeeringConnectionOptions_dryRun,
    modifyVpcPeeringConnectionOptions_vpcPeeringConnectionId,
    modifyVpcPeeringConnectionOptionsResponse_requesterPeeringConnectionOptions,
    modifyVpcPeeringConnectionOptionsResponse_accepterPeeringConnectionOptions,
    modifyVpcPeeringConnectionOptionsResponse_httpStatus,

    -- ** DescribeFpgaImages
    describeFpgaImages_owners,
    describeFpgaImages_filters,
    describeFpgaImages_nextToken,
    describeFpgaImages_dryRun,
    describeFpgaImages_maxResults,
    describeFpgaImages_fpgaImageIds,
    describeFpgaImagesResponse_fpgaImages,
    describeFpgaImagesResponse_nextToken,
    describeFpgaImagesResponse_httpStatus,

    -- ** CopySnapshot
    copySnapshot_destinationOutpostArn,
    copySnapshot_presignedUrl,
    copySnapshot_encrypted,
    copySnapshot_tagSpecifications,
    copySnapshot_destinationRegion,
    copySnapshot_kmsKeyId,
    copySnapshot_description,
    copySnapshot_dryRun,
    copySnapshot_sourceRegion,
    copySnapshot_sourceSnapshotId,
    copySnapshotResponse_tags,
    copySnapshotResponse_snapshotId,
    copySnapshotResponse_httpStatus,

    -- ** AcceptTransitGatewayPeeringAttachment
    acceptTransitGatewayPeeringAttachment_dryRun,
    acceptTransitGatewayPeeringAttachment_transitGatewayAttachmentId,
    acceptTransitGatewayPeeringAttachmentResponse_transitGatewayPeeringAttachment,
    acceptTransitGatewayPeeringAttachmentResponse_httpStatus,

    -- ** DisassociateAddress
    disassociateAddress_associationId,
    disassociateAddress_publicIp,
    disassociateAddress_dryRun,

    -- ** ModifyTrafficMirrorFilterNetworkServices
    modifyTrafficMirrorFilterNetworkServices_addNetworkServices,
    modifyTrafficMirrorFilterNetworkServices_removeNetworkServices,
    modifyTrafficMirrorFilterNetworkServices_dryRun,
    modifyTrafficMirrorFilterNetworkServices_trafficMirrorFilterId,
    modifyTrafficMirrorFilterNetworkServicesResponse_trafficMirrorFilter,
    modifyTrafficMirrorFilterNetworkServicesResponse_httpStatus,

    -- ** DescribeEgressOnlyInternetGateways
    describeEgressOnlyInternetGateways_egressOnlyInternetGatewayIds,
    describeEgressOnlyInternetGateways_filters,
    describeEgressOnlyInternetGateways_nextToken,
    describeEgressOnlyInternetGateways_dryRun,
    describeEgressOnlyInternetGateways_maxResults,
    describeEgressOnlyInternetGatewaysResponse_egressOnlyInternetGateways,
    describeEgressOnlyInternetGatewaysResponse_nextToken,
    describeEgressOnlyInternetGatewaysResponse_httpStatus,

    -- ** DeleteVpc
    deleteVpc_dryRun,
    deleteVpc_vpcId,

    -- ** CreateInstanceExportTask
    createInstanceExportTask_tagSpecifications,
    createInstanceExportTask_description,
    createInstanceExportTask_exportToS3Task,
    createInstanceExportTask_instanceId,
    createInstanceExportTask_targetEnvironment,
    createInstanceExportTaskResponse_exportTask,
    createInstanceExportTaskResponse_httpStatus,

    -- ** RejectTransitGatewayVpcAttachment
    rejectTransitGatewayVpcAttachment_dryRun,
    rejectTransitGatewayVpcAttachment_transitGatewayAttachmentId,
    rejectTransitGatewayVpcAttachmentResponse_transitGatewayVpcAttachment,
    rejectTransitGatewayVpcAttachmentResponse_httpStatus,

    -- ** DescribeTrafficMirrorSessions
    describeTrafficMirrorSessions_filters,
    describeTrafficMirrorSessions_nextToken,
    describeTrafficMirrorSessions_trafficMirrorSessionIds,
    describeTrafficMirrorSessions_dryRun,
    describeTrafficMirrorSessions_maxResults,
    describeTrafficMirrorSessionsResponse_nextToken,
    describeTrafficMirrorSessionsResponse_trafficMirrorSessions,
    describeTrafficMirrorSessionsResponse_httpStatus,

    -- ** GetTransitGatewayRouteTableAssociations
    getTransitGatewayRouteTableAssociations_filters,
    getTransitGatewayRouteTableAssociations_nextToken,
    getTransitGatewayRouteTableAssociations_dryRun,
    getTransitGatewayRouteTableAssociations_maxResults,
    getTransitGatewayRouteTableAssociations_transitGatewayRouteTableId,
    getTransitGatewayRouteTableAssociationsResponse_nextToken,
    getTransitGatewayRouteTableAssociationsResponse_associations,
    getTransitGatewayRouteTableAssociationsResponse_httpStatus,

    -- ** AssociateVpcCidrBlock
    associateVpcCidrBlock_ipv6CidrBlock,
    associateVpcCidrBlock_ipv6CidrBlockNetworkBorderGroup,
    associateVpcCidrBlock_cidrBlock,
    associateVpcCidrBlock_ipv6Pool,
    associateVpcCidrBlock_amazonProvidedIpv6CidrBlock,
    associateVpcCidrBlock_vpcId,
    associateVpcCidrBlockResponse_vpcId,
    associateVpcCidrBlockResponse_cidrBlockAssociation,
    associateVpcCidrBlockResponse_ipv6CidrBlockAssociation,
    associateVpcCidrBlockResponse_httpStatus,

    -- ** DescribeVpcAttribute
    describeVpcAttribute_dryRun,
    describeVpcAttribute_attribute,
    describeVpcAttribute_vpcId,
    describeVpcAttributeResponse_enableDnsHostnames,
    describeVpcAttributeResponse_enableDnsSupport,
    describeVpcAttributeResponse_vpcId,
    describeVpcAttributeResponse_httpStatus,

    -- ** CreateVolume
    createVolume_multiAttachEnabled,
    createVolume_clientToken,
    createVolume_throughput,
    createVolume_size,
    createVolume_iops,
    createVolume_outpostArn,
    createVolume_encrypted,
    createVolume_tagSpecifications,
    createVolume_kmsKeyId,
    createVolume_volumeType,
    createVolume_dryRun,
    createVolume_snapshotId,
    createVolume_availabilityZone,
    volume_fastRestored,
    volume_multiAttachEnabled,
    volume_attachments,
    volume_throughput,
    volume_iops,
    volume_outpostArn,
    volume_kmsKeyId,
    volume_tags,
    volume_availabilityZone,
    volume_createTime,
    volume_encrypted,
    volume_size,
    volume_snapshotId,
    volume_state,
    volume_volumeId,
    volume_volumeType,

    -- ** CreateDefaultSubnet
    createDefaultSubnet_dryRun,
    createDefaultSubnet_availabilityZone,
    createDefaultSubnetResponse_subnet,
    createDefaultSubnetResponse_httpStatus,

    -- ** DescribeScheduledInstanceAvailability
    describeScheduledInstanceAvailability_minSlotDurationInHours,
    describeScheduledInstanceAvailability_filters,
    describeScheduledInstanceAvailability_nextToken,
    describeScheduledInstanceAvailability_maxSlotDurationInHours,
    describeScheduledInstanceAvailability_dryRun,
    describeScheduledInstanceAvailability_maxResults,
    describeScheduledInstanceAvailability_firstSlotStartTimeRange,
    describeScheduledInstanceAvailability_recurrence,
    describeScheduledInstanceAvailabilityResponse_scheduledInstanceAvailabilitySet,
    describeScheduledInstanceAvailabilityResponse_nextToken,
    describeScheduledInstanceAvailabilityResponse_httpStatus,

    -- ** DisassociateClientVpnTargetNetwork
    disassociateClientVpnTargetNetwork_dryRun,
    disassociateClientVpnTargetNetwork_clientVpnEndpointId,
    disassociateClientVpnTargetNetwork_associationId,
    disassociateClientVpnTargetNetworkResponse_associationId,
    disassociateClientVpnTargetNetworkResponse_status,
    disassociateClientVpnTargetNetworkResponse_httpStatus,

    -- ** CreateClientVpnRoute
    createClientVpnRoute_clientToken,
    createClientVpnRoute_description,
    createClientVpnRoute_dryRun,
    createClientVpnRoute_clientVpnEndpointId,
    createClientVpnRoute_destinationCidrBlock,
    createClientVpnRoute_targetVpcSubnetId,
    createClientVpnRouteResponse_status,
    createClientVpnRouteResponse_httpStatus,

    -- ** ModifyVolumeAttribute
    modifyVolumeAttribute_autoEnableIO,
    modifyVolumeAttribute_dryRun,
    modifyVolumeAttribute_volumeId,

    -- ** ExportClientVpnClientConfiguration
    exportClientVpnClientConfiguration_dryRun,
    exportClientVpnClientConfiguration_clientVpnEndpointId,
    exportClientVpnClientConfigurationResponse_clientConfiguration,
    exportClientVpnClientConfigurationResponse_httpStatus,

    -- ** DescribeTrunkInterfaceAssociations
    describeTrunkInterfaceAssociations_filters,
    describeTrunkInterfaceAssociations_nextToken,
    describeTrunkInterfaceAssociations_associationIds,
    describeTrunkInterfaceAssociations_dryRun,
    describeTrunkInterfaceAssociations_maxResults,
    describeTrunkInterfaceAssociationsResponse_interfaceAssociations,
    describeTrunkInterfaceAssociationsResponse_nextToken,
    describeTrunkInterfaceAssociationsResponse_httpStatus,

    -- ** DeleteTrafficMirrorTarget
    deleteTrafficMirrorTarget_dryRun,
    deleteTrafficMirrorTarget_trafficMirrorTargetId,
    deleteTrafficMirrorTargetResponse_trafficMirrorTargetId,
    deleteTrafficMirrorTargetResponse_httpStatus,

    -- ** DescribeSpotDatafeedSubscription
    describeSpotDatafeedSubscription_dryRun,
    describeSpotDatafeedSubscriptionResponse_spotDatafeedSubscription,
    describeSpotDatafeedSubscriptionResponse_httpStatus,

    -- ** DescribeLocalGatewayRouteTables
    describeLocalGatewayRouteTables_filters,
    describeLocalGatewayRouteTables_nextToken,
    describeLocalGatewayRouteTables_localGatewayRouteTableIds,
    describeLocalGatewayRouteTables_dryRun,
    describeLocalGatewayRouteTables_maxResults,
    describeLocalGatewayRouteTablesResponse_nextToken,
    describeLocalGatewayRouteTablesResponse_localGatewayRouteTables,
    describeLocalGatewayRouteTablesResponse_httpStatus,

    -- ** DescribePrefixLists
    describePrefixLists_filters,
    describePrefixLists_prefixListIds,
    describePrefixLists_nextToken,
    describePrefixLists_dryRun,
    describePrefixLists_maxResults,
    describePrefixListsResponse_nextToken,
    describePrefixListsResponse_prefixLists,
    describePrefixListsResponse_httpStatus,

    -- ** AssociateTransitGatewayRouteTable
    associateTransitGatewayRouteTable_dryRun,
    associateTransitGatewayRouteTable_transitGatewayRouteTableId,
    associateTransitGatewayRouteTable_transitGatewayAttachmentId,
    associateTransitGatewayRouteTableResponse_association,
    associateTransitGatewayRouteTableResponse_httpStatus,

    -- ** DeletePlacementGroup
    deletePlacementGroup_dryRun,
    deletePlacementGroup_groupName,

    -- ** ModifyTransitGateway
    modifyTransitGateway_options,
    modifyTransitGateway_description,
    modifyTransitGateway_dryRun,
    modifyTransitGateway_transitGatewayId,
    modifyTransitGatewayResponse_transitGateway,
    modifyTransitGatewayResponse_httpStatus,

    -- ** DeleteTransitGatewayPrefixListReference
    deleteTransitGatewayPrefixListReference_dryRun,
    deleteTransitGatewayPrefixListReference_transitGatewayRouteTableId,
    deleteTransitGatewayPrefixListReference_prefixListId,
    deleteTransitGatewayPrefixListReferenceResponse_transitGatewayPrefixListReference,
    deleteTransitGatewayPrefixListReferenceResponse_httpStatus,

    -- ** CreateTransitGatewayMulticastDomain
    createTransitGatewayMulticastDomain_tagSpecifications,
    createTransitGatewayMulticastDomain_options,
    createTransitGatewayMulticastDomain_dryRun,
    createTransitGatewayMulticastDomain_transitGatewayId,
    createTransitGatewayMulticastDomainResponse_transitGatewayMulticastDomain,
    createTransitGatewayMulticastDomainResponse_httpStatus,

    -- ** DeregisterInstanceEventNotificationAttributes
    deregisterInstanceEventNotificationAttributes_instanceTagAttribute,
    deregisterInstanceEventNotificationAttributes_dryRun,
    deregisterInstanceEventNotificationAttributesResponse_instanceTagAttribute,
    deregisterInstanceEventNotificationAttributesResponse_httpStatus,

    -- ** RequestSpotFleet
    requestSpotFleet_dryRun,
    requestSpotFleet_spotFleetRequestConfig,
    requestSpotFleetResponse_spotFleetRequestId,
    requestSpotFleetResponse_httpStatus,

    -- ** DeleteNetworkInsightsPath
    deleteNetworkInsightsPath_dryRun,
    deleteNetworkInsightsPath_networkInsightsPathId,
    deleteNetworkInsightsPathResponse_networkInsightsPathId,
    deleteNetworkInsightsPathResponse_httpStatus,

    -- ** DescribeTransitGatewayConnects
    describeTransitGatewayConnects_filters,
    describeTransitGatewayConnects_nextToken,
    describeTransitGatewayConnects_transitGatewayAttachmentIds,
    describeTransitGatewayConnects_dryRun,
    describeTransitGatewayConnects_maxResults,
    describeTransitGatewayConnectsResponse_transitGatewayConnects,
    describeTransitGatewayConnectsResponse_nextToken,
    describeTransitGatewayConnectsResponse_httpStatus,

    -- ** DeleteTransitGatewayRoute
    deleteTransitGatewayRoute_dryRun,
    deleteTransitGatewayRoute_transitGatewayRouteTableId,
    deleteTransitGatewayRoute_destinationCidrBlock,
    deleteTransitGatewayRouteResponse_route,
    deleteTransitGatewayRouteResponse_httpStatus,

    -- ** CreateTransitGatewayConnectPeer
    createTransitGatewayConnectPeer_bgpOptions,
    createTransitGatewayConnectPeer_transitGatewayAddress,
    createTransitGatewayConnectPeer_tagSpecifications,
    createTransitGatewayConnectPeer_dryRun,
    createTransitGatewayConnectPeer_transitGatewayAttachmentId,
    createTransitGatewayConnectPeer_peerAddress,
    createTransitGatewayConnectPeer_insideCidrBlocks,
    createTransitGatewayConnectPeerResponse_transitGatewayConnectPeer,
    createTransitGatewayConnectPeerResponse_httpStatus,

    -- ** DisableEbsEncryptionByDefault
    disableEbsEncryptionByDefault_dryRun,
    disableEbsEncryptionByDefaultResponse_ebsEncryptionByDefault,
    disableEbsEncryptionByDefaultResponse_httpStatus,

    -- ** DeregisterTransitGatewayMulticastGroupMembers
    deregisterTransitGatewayMulticastGroupMembers_networkInterfaceIds,
    deregisterTransitGatewayMulticastGroupMembers_transitGatewayMulticastDomainId,
    deregisterTransitGatewayMulticastGroupMembers_groupIpAddress,
    deregisterTransitGatewayMulticastGroupMembers_dryRun,
    deregisterTransitGatewayMulticastGroupMembersResponse_deregisteredMulticastGroupMembers,
    deregisterTransitGatewayMulticastGroupMembersResponse_httpStatus,

    -- ** AssociateTrunkInterface
    associateTrunkInterface_clientToken,
    associateTrunkInterface_greKey,
    associateTrunkInterface_vlanId,
    associateTrunkInterface_dryRun,
    associateTrunkInterface_branchInterfaceId,
    associateTrunkInterface_trunkInterfaceId,
    associateTrunkInterfaceResponse_clientToken,
    associateTrunkInterfaceResponse_interfaceAssociation,
    associateTrunkInterfaceResponse_httpStatus,

    -- ** CreateSubnet
    createSubnet_ipv6CidrBlock,
    createSubnet_availabilityZoneId,
    createSubnet_outpostArn,
    createSubnet_tagSpecifications,
    createSubnet_availabilityZone,
    createSubnet_dryRun,
    createSubnet_cidrBlock,
    createSubnet_vpcId,
    createSubnetResponse_subnet,
    createSubnetResponse_httpStatus,

    -- ** CreateNetworkInterface
    createNetworkInterface_groups,
    createNetworkInterface_privateIpAddresses,
    createNetworkInterface_clientToken,
    createNetworkInterface_ipv4Prefixes,
    createNetworkInterface_interfaceType,
    createNetworkInterface_ipv4PrefixCount,
    createNetworkInterface_tagSpecifications,
    createNetworkInterface_ipv6AddressCount,
    createNetworkInterface_ipv6Prefixes,
    createNetworkInterface_privateIpAddress,
    createNetworkInterface_ipv6PrefixCount,
    createNetworkInterface_secondaryPrivateIpAddressCount,
    createNetworkInterface_description,
    createNetworkInterface_dryRun,
    createNetworkInterface_ipv6Addresses,
    createNetworkInterface_subnetId,
    createNetworkInterfaceResponse_clientToken,
    createNetworkInterfaceResponse_networkInterface,
    createNetworkInterfaceResponse_httpStatus,

    -- ** DescribeSecurityGroups
    describeSecurityGroups_filters,
    describeSecurityGroups_groupNames,
    describeSecurityGroups_groupIds,
    describeSecurityGroups_nextToken,
    describeSecurityGroups_dryRun,
    describeSecurityGroups_maxResults,
    describeSecurityGroupsResponse_securityGroups,
    describeSecurityGroupsResponse_nextToken,
    describeSecurityGroupsResponse_httpStatus,

    -- ** GetCapacityReservationUsage
    getCapacityReservationUsage_nextToken,
    getCapacityReservationUsage_dryRun,
    getCapacityReservationUsage_maxResults,
    getCapacityReservationUsage_capacityReservationId,
    getCapacityReservationUsageResponse_state,
    getCapacityReservationUsageResponse_instanceUsages,
    getCapacityReservationUsageResponse_availableInstanceCount,
    getCapacityReservationUsageResponse_capacityReservationId,
    getCapacityReservationUsageResponse_instanceType,
    getCapacityReservationUsageResponse_nextToken,
    getCapacityReservationUsageResponse_totalInstanceCount,
    getCapacityReservationUsageResponse_httpStatus,

    -- ** CreateTransitGatewayVpcAttachment
    createTransitGatewayVpcAttachment_tagSpecifications,
    createTransitGatewayVpcAttachment_options,
    createTransitGatewayVpcAttachment_dryRun,
    createTransitGatewayVpcAttachment_transitGatewayId,
    createTransitGatewayVpcAttachment_vpcId,
    createTransitGatewayVpcAttachment_subnetIds,
    createTransitGatewayVpcAttachmentResponse_transitGatewayVpcAttachment,
    createTransitGatewayVpcAttachmentResponse_httpStatus,

    -- ** DescribeExportTasks
    describeExportTasks_filters,
    describeExportTasks_exportTaskIds,
    describeExportTasksResponse_exportTasks,
    describeExportTasksResponse_httpStatus,

    -- ** ModifySpotFleetRequest
    modifySpotFleetRequest_context,
    modifySpotFleetRequest_targetCapacity,
    modifySpotFleetRequest_excessCapacityTerminationPolicy,
    modifySpotFleetRequest_onDemandTargetCapacity,
    modifySpotFleetRequest_launchTemplateConfigs,
    modifySpotFleetRequest_spotFleetRequestId,
    modifySpotFleetRequestResponse_return,
    modifySpotFleetRequestResponse_httpStatus,

    -- ** DetachVpnGateway
    detachVpnGateway_dryRun,
    detachVpnGateway_vpcId,
    detachVpnGateway_vpnGatewayId,

    -- ** ModifyManagedPrefixList
    modifyManagedPrefixList_currentVersion,
    modifyManagedPrefixList_removeEntries,
    modifyManagedPrefixList_prefixListName,
    modifyManagedPrefixList_addEntries,
    modifyManagedPrefixList_maxEntries,
    modifyManagedPrefixList_dryRun,
    modifyManagedPrefixList_prefixListId,
    modifyManagedPrefixListResponse_prefixList,
    modifyManagedPrefixListResponse_httpStatus,

    -- ** GetHostReservationPurchasePreview
    getHostReservationPurchasePreview_hostIdSet,
    getHostReservationPurchasePreview_offeringId,
    getHostReservationPurchasePreviewResponse_currencyCode,
    getHostReservationPurchasePreviewResponse_totalHourlyPrice,
    getHostReservationPurchasePreviewResponse_totalUpfrontPrice,
    getHostReservationPurchasePreviewResponse_purchase,
    getHostReservationPurchasePreviewResponse_httpStatus,

    -- ** EnableVolumeIO
    enableVolumeIO_dryRun,
    enableVolumeIO_volumeId,

    -- ** DescribeInstances
    describeInstances_filters,
    describeInstances_nextToken,
    describeInstances_instanceIds,
    describeInstances_dryRun,
    describeInstances_maxResults,
    describeInstancesResponse_nextToken,
    describeInstancesResponse_reservations,
    describeInstancesResponse_httpStatus,

    -- ** DescribeInstanceEventWindows
    describeInstanceEventWindows_instanceEventWindowIds,
    describeInstanceEventWindows_filters,
    describeInstanceEventWindows_nextToken,
    describeInstanceEventWindows_dryRun,
    describeInstanceEventWindows_maxResults,
    describeInstanceEventWindowsResponse_instanceEventWindows,
    describeInstanceEventWindowsResponse_nextToken,
    describeInstanceEventWindowsResponse_httpStatus,

    -- ** DisableSerialConsoleAccess
    disableSerialConsoleAccess_dryRun,
    disableSerialConsoleAccessResponse_serialConsoleAccessEnabled,
    disableSerialConsoleAccessResponse_httpStatus,

    -- ** CreateNatGateway
    createNatGateway_clientToken,
    createNatGateway_allocationId,
    createNatGateway_connectivityType,
    createNatGateway_tagSpecifications,
    createNatGateway_dryRun,
    createNatGateway_subnetId,
    createNatGatewayResponse_clientToken,
    createNatGatewayResponse_natGateway,
    createNatGatewayResponse_httpStatus,

    -- ** DescribeLocalGatewayVirtualInterfaces
    describeLocalGatewayVirtualInterfaces_filters,
    describeLocalGatewayVirtualInterfaces_nextToken,
    describeLocalGatewayVirtualInterfaces_localGatewayVirtualInterfaceIds,
    describeLocalGatewayVirtualInterfaces_dryRun,
    describeLocalGatewayVirtualInterfaces_maxResults,
    describeLocalGatewayVirtualInterfacesResponse_nextToken,
    describeLocalGatewayVirtualInterfacesResponse_localGatewayVirtualInterfaces,
    describeLocalGatewayVirtualInterfacesResponse_httpStatus,

    -- ** DescribeVpcPeeringConnections
    describeVpcPeeringConnections_filters,
    describeVpcPeeringConnections_nextToken,
    describeVpcPeeringConnections_vpcPeeringConnectionIds,
    describeVpcPeeringConnections_dryRun,
    describeVpcPeeringConnections_maxResults,
    describeVpcPeeringConnectionsResponse_nextToken,
    describeVpcPeeringConnectionsResponse_vpcPeeringConnections,
    describeVpcPeeringConnectionsResponse_httpStatus,

    -- ** CancelExportTask
    cancelExportTask_exportTaskId,

    -- ** CreateVpcEndpointServiceConfiguration
    createVpcEndpointServiceConfiguration_networkLoadBalancerArns,
    createVpcEndpointServiceConfiguration_clientToken,
    createVpcEndpointServiceConfiguration_tagSpecifications,
    createVpcEndpointServiceConfiguration_gatewayLoadBalancerArns,
    createVpcEndpointServiceConfiguration_acceptanceRequired,
    createVpcEndpointServiceConfiguration_privateDnsName,
    createVpcEndpointServiceConfiguration_dryRun,
    createVpcEndpointServiceConfigurationResponse_clientToken,
    createVpcEndpointServiceConfigurationResponse_serviceConfiguration,
    createVpcEndpointServiceConfigurationResponse_httpStatus,

    -- ** CreateDefaultVpc
    createDefaultVpc_dryRun,
    createDefaultVpcResponse_vpc,
    createDefaultVpcResponse_httpStatus,

    -- ** DisassociateVpcCidrBlock
    disassociateVpcCidrBlock_associationId,
    disassociateVpcCidrBlockResponse_vpcId,
    disassociateVpcCidrBlockResponse_cidrBlockAssociation,
    disassociateVpcCidrBlockResponse_ipv6CidrBlockAssociation,
    disassociateVpcCidrBlockResponse_httpStatus,

    -- ** DescribeTrafficMirrorFilters
    describeTrafficMirrorFilters_trafficMirrorFilterIds,
    describeTrafficMirrorFilters_filters,
    describeTrafficMirrorFilters_nextToken,
    describeTrafficMirrorFilters_dryRun,
    describeTrafficMirrorFilters_maxResults,
    describeTrafficMirrorFiltersResponse_trafficMirrorFilters,
    describeTrafficMirrorFiltersResponse_nextToken,
    describeTrafficMirrorFiltersResponse_httpStatus,

    -- ** DescribeFastSnapshotRestores
    describeFastSnapshotRestores_filters,
    describeFastSnapshotRestores_nextToken,
    describeFastSnapshotRestores_dryRun,
    describeFastSnapshotRestores_maxResults,
    describeFastSnapshotRestoresResponse_fastSnapshotRestores,
    describeFastSnapshotRestoresResponse_nextToken,
    describeFastSnapshotRestoresResponse_httpStatus,

    -- ** CancelCapacityReservation
    cancelCapacityReservation_dryRun,
    cancelCapacityReservation_capacityReservationId,
    cancelCapacityReservationResponse_return,
    cancelCapacityReservationResponse_httpStatus,

    -- ** DeleteNetworkInterface
    deleteNetworkInterface_dryRun,
    deleteNetworkInterface_networkInterfaceId,

    -- ** DisassociateTransitGatewayRouteTable
    disassociateTransitGatewayRouteTable_dryRun,
    disassociateTransitGatewayRouteTable_transitGatewayRouteTableId,
    disassociateTransitGatewayRouteTable_transitGatewayAttachmentId,
    disassociateTransitGatewayRouteTableResponse_association,
    disassociateTransitGatewayRouteTableResponse_httpStatus,

    -- ** ReplaceRouteTableAssociation
    replaceRouteTableAssociation_dryRun,
    replaceRouteTableAssociation_associationId,
    replaceRouteTableAssociation_routeTableId,
    replaceRouteTableAssociationResponse_newAssociationId,
    replaceRouteTableAssociationResponse_associationState,
    replaceRouteTableAssociationResponse_httpStatus,

    -- ** StartInstances
    startInstances_additionalInfo,
    startInstances_dryRun,
    startInstances_instanceIds,
    startInstancesResponse_startingInstances,
    startInstancesResponse_httpStatus,

    -- ** CreatePlacementGroup
    createPlacementGroup_strategy,
    createPlacementGroup_tagSpecifications,
    createPlacementGroup_groupName,
    createPlacementGroup_dryRun,
    createPlacementGroup_partitionCount,
    createPlacementGroupResponse_placementGroup,
    createPlacementGroupResponse_httpStatus,

    -- ** DescribeInstanceEventNotificationAttributes
    describeInstanceEventNotificationAttributes_dryRun,
    describeInstanceEventNotificationAttributesResponse_instanceTagAttribute,
    describeInstanceEventNotificationAttributesResponse_httpStatus,

    -- ** DescribeCapacityReservations
    describeCapacityReservations_capacityReservationIds,
    describeCapacityReservations_filters,
    describeCapacityReservations_nextToken,
    describeCapacityReservations_dryRun,
    describeCapacityReservations_maxResults,
    describeCapacityReservationsResponse_capacityReservations,
    describeCapacityReservationsResponse_nextToken,
    describeCapacityReservationsResponse_httpStatus,

    -- ** ModifyClientVpnEndpoint
    modifyClientVpnEndpoint_securityGroupIds,
    modifyClientVpnEndpoint_connectionLogOptions,
    modifyClientVpnEndpoint_splitTunnel,
    modifyClientVpnEndpoint_vpcId,
    modifyClientVpnEndpoint_vpnPort,
    modifyClientVpnEndpoint_dnsServers,
    modifyClientVpnEndpoint_clientConnectOptions,
    modifyClientVpnEndpoint_selfServicePortal,
    modifyClientVpnEndpoint_serverCertificateArn,
    modifyClientVpnEndpoint_description,
    modifyClientVpnEndpoint_dryRun,
    modifyClientVpnEndpoint_clientVpnEndpointId,
    modifyClientVpnEndpointResponse_return,
    modifyClientVpnEndpointResponse_httpStatus,

    -- ** ModifyInstanceCapacityReservationAttributes
    modifyInstanceCapacityReservationAttributes_dryRun,
    modifyInstanceCapacityReservationAttributes_instanceId,
    modifyInstanceCapacityReservationAttributes_capacityReservationSpecification,
    modifyInstanceCapacityReservationAttributesResponse_return,
    modifyInstanceCapacityReservationAttributesResponse_httpStatus,

    -- ** DescribeAggregateIdFormat
    describeAggregateIdFormat_dryRun,
    describeAggregateIdFormatResponse_useLongIdsAggregated,
    describeAggregateIdFormatResponse_statuses,
    describeAggregateIdFormatResponse_httpStatus,

    -- ** DescribeSnapshots
    describeSnapshots_ownerIds,
    describeSnapshots_filters,
    describeSnapshots_nextToken,
    describeSnapshots_snapshotIds,
    describeSnapshots_restorableByUserIds,
    describeSnapshots_dryRun,
    describeSnapshots_maxResults,
    describeSnapshotsResponse_nextToken,
    describeSnapshotsResponse_snapshots,
    describeSnapshotsResponse_httpStatus,

    -- ** GetSubnetCidrReservations
    getSubnetCidrReservations_filters,
    getSubnetCidrReservations_nextToken,
    getSubnetCidrReservations_dryRun,
    getSubnetCidrReservations_maxResults,
    getSubnetCidrReservations_subnetId,
    getSubnetCidrReservationsResponse_subnetIpv6CidrReservations,
    getSubnetCidrReservationsResponse_nextToken,
    getSubnetCidrReservationsResponse_subnetIpv4CidrReservations,
    getSubnetCidrReservationsResponse_httpStatus,

    -- ** AssociateAddress
    associateAddress_instanceId,
    associateAddress_allocationId,
    associateAddress_networkInterfaceId,
    associateAddress_allowReassociation,
    associateAddress_privateIpAddress,
    associateAddress_publicIp,
    associateAddress_dryRun,
    associateAddressResponse_associationId,
    associateAddressResponse_httpStatus,

    -- ** ModifyTrafficMirrorFilterRule
    modifyTrafficMirrorFilterRule_removeFields,
    modifyTrafficMirrorFilterRule_ruleNumber,
    modifyTrafficMirrorFilterRule_trafficDirection,
    modifyTrafficMirrorFilterRule_ruleAction,
    modifyTrafficMirrorFilterRule_protocol,
    modifyTrafficMirrorFilterRule_destinationPortRange,
    modifyTrafficMirrorFilterRule_sourceCidrBlock,
    modifyTrafficMirrorFilterRule_sourcePortRange,
    modifyTrafficMirrorFilterRule_description,
    modifyTrafficMirrorFilterRule_dryRun,
    modifyTrafficMirrorFilterRule_destinationCidrBlock,
    modifyTrafficMirrorFilterRule_trafficMirrorFilterRuleId,
    modifyTrafficMirrorFilterRuleResponse_trafficMirrorFilterRule,
    modifyTrafficMirrorFilterRuleResponse_httpStatus,

    -- ** DescribeNetworkInterfaceAttribute
    describeNetworkInterfaceAttribute_attribute,
    describeNetworkInterfaceAttribute_dryRun,
    describeNetworkInterfaceAttribute_networkInterfaceId,
    describeNetworkInterfaceAttributeResponse_groups,
    describeNetworkInterfaceAttributeResponse_sourceDestCheck,
    describeNetworkInterfaceAttributeResponse_networkInterfaceId,
    describeNetworkInterfaceAttributeResponse_attachment,
    describeNetworkInterfaceAttributeResponse_description,
    describeNetworkInterfaceAttributeResponse_httpStatus,

    -- ** ReplaceIamInstanceProfileAssociation
    replaceIamInstanceProfileAssociation_iamInstanceProfile,
    replaceIamInstanceProfileAssociation_associationId,
    replaceIamInstanceProfileAssociationResponse_iamInstanceProfileAssociation,
    replaceIamInstanceProfileAssociationResponse_httpStatus,

    -- ** AssociateClientVpnTargetNetwork
    associateClientVpnTargetNetwork_clientToken,
    associateClientVpnTargetNetwork_dryRun,
    associateClientVpnTargetNetwork_clientVpnEndpointId,
    associateClientVpnTargetNetwork_subnetId,
    associateClientVpnTargetNetworkResponse_associationId,
    associateClientVpnTargetNetworkResponse_status,
    associateClientVpnTargetNetworkResponse_httpStatus,

    -- ** ReleaseHosts
    releaseHosts_hostIds,
    releaseHostsResponse_unsuccessful,
    releaseHostsResponse_successful,
    releaseHostsResponse_httpStatus,

    -- ** EnableSerialConsoleAccess
    enableSerialConsoleAccess_dryRun,
    enableSerialConsoleAccessResponse_serialConsoleAccessEnabled,
    enableSerialConsoleAccessResponse_httpStatus,

    -- ** ResetNetworkInterfaceAttribute
    resetNetworkInterfaceAttribute_sourceDestCheck,
    resetNetworkInterfaceAttribute_dryRun,
    resetNetworkInterfaceAttribute_networkInterfaceId,

    -- ** DeleteInternetGateway
    deleteInternetGateway_dryRun,
    deleteInternetGateway_internetGatewayId,

    -- ** DescribeReservedInstancesListings
    describeReservedInstancesListings_filters,
    describeReservedInstancesListings_reservedInstancesId,
    describeReservedInstancesListings_reservedInstancesListingId,
    describeReservedInstancesListingsResponse_reservedInstancesListings,
    describeReservedInstancesListingsResponse_httpStatus,

    -- ** CreateVpnConnection
    createVpnConnection_vpnGatewayId,
    createVpnConnection_tagSpecifications,
    createVpnConnection_transitGatewayId,
    createVpnConnection_options,
    createVpnConnection_dryRun,
    createVpnConnection_customerGatewayId,
    createVpnConnection_type,
    createVpnConnectionResponse_vpnConnection,
    createVpnConnectionResponse_httpStatus,

    -- ** ReplaceTransitGatewayRoute
    replaceTransitGatewayRoute_blackhole,
    replaceTransitGatewayRoute_transitGatewayAttachmentId,
    replaceTransitGatewayRoute_dryRun,
    replaceTransitGatewayRoute_destinationCidrBlock,
    replaceTransitGatewayRoute_transitGatewayRouteTableId,
    replaceTransitGatewayRouteResponse_route,
    replaceTransitGatewayRouteResponse_httpStatus,

    -- ** CreateFleet
    createFleet_context,
    createFleet_clientToken,
    createFleet_spotOptions,
    createFleet_excessCapacityTerminationPolicy,
    createFleet_onDemandOptions,
    createFleet_tagSpecifications,
    createFleet_validUntil,
    createFleet_terminateInstancesWithExpiration,
    createFleet_type,
    createFleet_validFrom,
    createFleet_replaceUnhealthyInstances,
    createFleet_dryRun,
    createFleet_launchTemplateConfigs,
    createFleet_targetCapacitySpecification,
    createFleetResponse_instances,
    createFleetResponse_fleetId,
    createFleetResponse_errors,
    createFleetResponse_httpStatus,

    -- ** DeleteNatGateway
    deleteNatGateway_dryRun,
    deleteNatGateway_natGatewayId,
    deleteNatGatewayResponse_natGatewayId,
    deleteNatGatewayResponse_httpStatus,

    -- ** DescribeImportSnapshotTasks
    describeImportSnapshotTasks_filters,
    describeImportSnapshotTasks_importTaskIds,
    describeImportSnapshotTasks_nextToken,
    describeImportSnapshotTasks_dryRun,
    describeImportSnapshotTasks_maxResults,
    describeImportSnapshotTasksResponse_nextToken,
    describeImportSnapshotTasksResponse_importSnapshotTasks,
    describeImportSnapshotTasksResponse_httpStatus,

    -- ** GetCoipPoolUsage
    getCoipPoolUsage_filters,
    getCoipPoolUsage_nextToken,
    getCoipPoolUsage_dryRun,
    getCoipPoolUsage_maxResults,
    getCoipPoolUsage_poolId,
    getCoipPoolUsageResponse_coipAddressUsages,
    getCoipPoolUsageResponse_coipPoolId,
    getCoipPoolUsageResponse_localGatewayRouteTableId,
    getCoipPoolUsageResponse_httpStatus,

    -- ** DescribeCustomerGateways
    describeCustomerGateways_customerGatewayIds,
    describeCustomerGateways_filters,
    describeCustomerGateways_dryRun,
    describeCustomerGatewaysResponse_customerGateways,
    describeCustomerGatewaysResponse_httpStatus,

    -- ** DeleteSubnet
    deleteSubnet_dryRun,
    deleteSubnet_subnetId,

    -- ** CopyImage
    copyImage_destinationOutpostArn,
    copyImage_clientToken,
    copyImage_encrypted,
    copyImage_kmsKeyId,
    copyImage_description,
    copyImage_dryRun,
    copyImage_name,
    copyImage_sourceImageId,
    copyImage_sourceRegion,
    copyImageResponse_imageId,
    copyImageResponse_httpStatus,

    -- ** CreateVpcEndpoint
    createVpcEndpoint_policyDocument,
    createVpcEndpoint_securityGroupIds,
    createVpcEndpoint_clientToken,
    createVpcEndpoint_subnetIds,
    createVpcEndpoint_vpcEndpointType,
    createVpcEndpoint_privateDnsEnabled,
    createVpcEndpoint_tagSpecifications,
    createVpcEndpoint_dryRun,
    createVpcEndpoint_routeTableIds,
    createVpcEndpoint_vpcId,
    createVpcEndpoint_serviceName,
    createVpcEndpointResponse_clientToken,
    createVpcEndpointResponse_vpcEndpoint,
    createVpcEndpointResponse_httpStatus,

    -- ** ModifyTrafficMirrorSession
    modifyTrafficMirrorSession_removeFields,
    modifyTrafficMirrorSession_trafficMirrorTargetId,
    modifyTrafficMirrorSession_trafficMirrorFilterId,
    modifyTrafficMirrorSession_packetLength,
    modifyTrafficMirrorSession_virtualNetworkId,
    modifyTrafficMirrorSession_sessionNumber,
    modifyTrafficMirrorSession_description,
    modifyTrafficMirrorSession_dryRun,
    modifyTrafficMirrorSession_trafficMirrorSessionId,
    modifyTrafficMirrorSessionResponse_trafficMirrorSession,
    modifyTrafficMirrorSessionResponse_httpStatus,

    -- ** DescribeCarrierGateways
    describeCarrierGateways_filters,
    describeCarrierGateways_nextToken,
    describeCarrierGateways_carrierGatewayIds,
    describeCarrierGateways_dryRun,
    describeCarrierGateways_maxResults,
    describeCarrierGatewaysResponse_nextToken,
    describeCarrierGatewaysResponse_carrierGateways,
    describeCarrierGatewaysResponse_httpStatus,

    -- ** DescribeTransitGatewayPeeringAttachments
    describeTransitGatewayPeeringAttachments_filters,
    describeTransitGatewayPeeringAttachments_nextToken,
    describeTransitGatewayPeeringAttachments_transitGatewayAttachmentIds,
    describeTransitGatewayPeeringAttachments_dryRun,
    describeTransitGatewayPeeringAttachments_maxResults,
    describeTransitGatewayPeeringAttachmentsResponse_transitGatewayPeeringAttachments,
    describeTransitGatewayPeeringAttachmentsResponse_nextToken,
    describeTransitGatewayPeeringAttachmentsResponse_httpStatus,

    -- ** DeleteQueuedReservedInstances
    deleteQueuedReservedInstances_dryRun,
    deleteQueuedReservedInstances_reservedInstancesIds,
    deleteQueuedReservedInstancesResponse_failedQueuedPurchaseDeletions,
    deleteQueuedReservedInstancesResponse_successfulQueuedPurchaseDeletions,
    deleteQueuedReservedInstancesResponse_httpStatus,

    -- ** DescribeTransitGatewayMulticastDomains
    describeTransitGatewayMulticastDomains_transitGatewayMulticastDomainIds,
    describeTransitGatewayMulticastDomains_filters,
    describeTransitGatewayMulticastDomains_nextToken,
    describeTransitGatewayMulticastDomains_dryRun,
    describeTransitGatewayMulticastDomains_maxResults,
    describeTransitGatewayMulticastDomainsResponse_transitGatewayMulticastDomains,
    describeTransitGatewayMulticastDomainsResponse_nextToken,
    describeTransitGatewayMulticastDomainsResponse_httpStatus,

    -- ** GetDefaultCreditSpecification
    getDefaultCreditSpecification_dryRun,
    getDefaultCreditSpecification_instanceFamily,
    getDefaultCreditSpecificationResponse_instanceFamilyCreditSpecification,
    getDefaultCreditSpecificationResponse_httpStatus,

    -- ** UnmonitorInstances
    unmonitorInstances_dryRun,
    unmonitorInstances_instanceIds,
    unmonitorInstancesResponse_instanceMonitorings,
    unmonitorInstancesResponse_httpStatus,

    -- ** DescribeTransitGatewayVpcAttachments
    describeTransitGatewayVpcAttachments_filters,
    describeTransitGatewayVpcAttachments_nextToken,
    describeTransitGatewayVpcAttachments_transitGatewayAttachmentIds,
    describeTransitGatewayVpcAttachments_dryRun,
    describeTransitGatewayVpcAttachments_maxResults,
    describeTransitGatewayVpcAttachmentsResponse_transitGatewayVpcAttachments,
    describeTransitGatewayVpcAttachmentsResponse_nextToken,
    describeTransitGatewayVpcAttachmentsResponse_httpStatus,

    -- ** DescribeTransitGatewayConnectPeers
    describeTransitGatewayConnectPeers_transitGatewayConnectPeerIds,
    describeTransitGatewayConnectPeers_filters,
    describeTransitGatewayConnectPeers_nextToken,
    describeTransitGatewayConnectPeers_dryRun,
    describeTransitGatewayConnectPeers_maxResults,
    describeTransitGatewayConnectPeersResponse_transitGatewayConnectPeers,
    describeTransitGatewayConnectPeersResponse_nextToken,
    describeTransitGatewayConnectPeersResponse_httpStatus,

    -- ** CreateSecurityGroup
    createSecurityGroup_vpcId,
    createSecurityGroup_tagSpecifications,
    createSecurityGroup_dryRun,
    createSecurityGroup_description,
    createSecurityGroup_groupName,
    createSecurityGroupResponse_tags,
    createSecurityGroupResponse_httpStatus,
    createSecurityGroupResponse_groupId,

    -- ** CreateInstanceEventWindow
    createInstanceEventWindow_tagSpecifications,
    createInstanceEventWindow_name,
    createInstanceEventWindow_cronExpression,
    createInstanceEventWindow_dryRun,
    createInstanceEventWindow_timeRanges,
    createInstanceEventWindowResponse_instanceEventWindow,
    createInstanceEventWindowResponse_httpStatus,

    -- ** GetEbsEncryptionByDefault
    getEbsEncryptionByDefault_dryRun,
    getEbsEncryptionByDefaultResponse_ebsEncryptionByDefault,
    getEbsEncryptionByDefaultResponse_httpStatus,

    -- ** ImportVolume
    importVolume_description,
    importVolume_dryRun,
    importVolume_availabilityZone,
    importVolume_image,
    importVolume_volume,
    importVolumeResponse_conversionTask,
    importVolumeResponse_httpStatus,

    -- ** DeleteCarrierGateway
    deleteCarrierGateway_dryRun,
    deleteCarrierGateway_carrierGatewayId,
    deleteCarrierGatewayResponse_carrierGateway,
    deleteCarrierGatewayResponse_httpStatus,

    -- ** DisableVgwRoutePropagation
    disableVgwRoutePropagation_dryRun,
    disableVgwRoutePropagation_gatewayId,
    disableVgwRoutePropagation_routeTableId,

    -- ** DeleteTrafficMirrorFilter
    deleteTrafficMirrorFilter_dryRun,
    deleteTrafficMirrorFilter_trafficMirrorFilterId,
    deleteTrafficMirrorFilterResponse_trafficMirrorFilterId,
    deleteTrafficMirrorFilterResponse_httpStatus,

    -- ** ModifyVpnTunnelCertificate
    modifyVpnTunnelCertificate_dryRun,
    modifyVpnTunnelCertificate_vpnConnectionId,
    modifyVpnTunnelCertificate_vpnTunnelOutsideIpAddress,
    modifyVpnTunnelCertificateResponse_vpnConnection,
    modifyVpnTunnelCertificateResponse_httpStatus,

    -- ** CreateSpotDatafeedSubscription
    createSpotDatafeedSubscription_prefix,
    createSpotDatafeedSubscription_dryRun,
    createSpotDatafeedSubscription_bucket,
    createSpotDatafeedSubscriptionResponse_spotDatafeedSubscription,
    createSpotDatafeedSubscriptionResponse_httpStatus,

    -- ** CancelSpotInstanceRequests
    cancelSpotInstanceRequests_dryRun,
    cancelSpotInstanceRequests_spotInstanceRequestIds,
    cancelSpotInstanceRequestsResponse_cancelledSpotInstanceRequests,
    cancelSpotInstanceRequestsResponse_httpStatus,

    -- ** CreateRoute
    createRoute_vpcPeeringConnectionId,
    createRoute_instanceId,
    createRoute_egressOnlyInternetGatewayId,
    createRoute_destinationIpv6CidrBlock,
    createRoute_localGatewayId,
    createRoute_natGatewayId,
    createRoute_networkInterfaceId,
    createRoute_transitGatewayId,
    createRoute_gatewayId,
    createRoute_vpcEndpointId,
    createRoute_destinationPrefixListId,
    createRoute_dryRun,
    createRoute_carrierGatewayId,
    createRoute_destinationCidrBlock,
    createRoute_routeTableId,
    createRouteResponse_return,
    createRouteResponse_httpStatus,

    -- ** DescribeVpcEndpointServiceConfigurations
    describeVpcEndpointServiceConfigurations_filters,
    describeVpcEndpointServiceConfigurations_serviceIds,
    describeVpcEndpointServiceConfigurations_nextToken,
    describeVpcEndpointServiceConfigurations_dryRun,
    describeVpcEndpointServiceConfigurations_maxResults,
    describeVpcEndpointServiceConfigurationsResponse_nextToken,
    describeVpcEndpointServiceConfigurationsResponse_serviceConfigurations,
    describeVpcEndpointServiceConfigurationsResponse_httpStatus,

    -- ** DeleteSnapshot
    deleteSnapshot_dryRun,
    deleteSnapshot_snapshotId,

    -- ** AssignPrivateIpAddresses
    assignPrivateIpAddresses_privateIpAddresses,
    assignPrivateIpAddresses_ipv4Prefixes,
    assignPrivateIpAddresses_ipv4PrefixCount,
    assignPrivateIpAddresses_allowReassignment,
    assignPrivateIpAddresses_secondaryPrivateIpAddressCount,
    assignPrivateIpAddresses_networkInterfaceId,
    assignPrivateIpAddressesResponse_assignedPrivateIpAddresses,
    assignPrivateIpAddressesResponse_assignedIpv4Prefixes,
    assignPrivateIpAddressesResponse_networkInterfaceId,
    assignPrivateIpAddressesResponse_httpStatus,

    -- ** AuthorizeClientVpnIngress
    authorizeClientVpnIngress_clientToken,
    authorizeClientVpnIngress_accessGroupId,
    authorizeClientVpnIngress_authorizeAllGroups,
    authorizeClientVpnIngress_description,
    authorizeClientVpnIngress_dryRun,
    authorizeClientVpnIngress_clientVpnEndpointId,
    authorizeClientVpnIngress_targetNetworkCidr,
    authorizeClientVpnIngressResponse_status,
    authorizeClientVpnIngressResponse_httpStatus,

    -- ** DeleteTransitGatewayPeeringAttachment
    deleteTransitGatewayPeeringAttachment_dryRun,
    deleteTransitGatewayPeeringAttachment_transitGatewayAttachmentId,
    deleteTransitGatewayPeeringAttachmentResponse_transitGatewayPeeringAttachment,
    deleteTransitGatewayPeeringAttachmentResponse_httpStatus,

    -- ** ModifyInstanceAttribute
    modifyInstanceAttribute_groups,
    modifyInstanceAttribute_attribute,
    modifyInstanceAttribute_enaSupport,
    modifyInstanceAttribute_sourceDestCheck,
    modifyInstanceAttribute_disableApiTermination,
    modifyInstanceAttribute_kernel,
    modifyInstanceAttribute_ramdisk,
    modifyInstanceAttribute_value,
    modifyInstanceAttribute_instanceType,
    modifyInstanceAttribute_sriovNetSupport,
    modifyInstanceAttribute_ebsOptimized,
    modifyInstanceAttribute_userData,
    modifyInstanceAttribute_instanceInitiatedShutdownBehavior,
    modifyInstanceAttribute_blockDeviceMappings,
    modifyInstanceAttribute_dryRun,
    modifyInstanceAttribute_instanceId,

    -- ** DeleteCustomerGateway
    deleteCustomerGateway_dryRun,
    deleteCustomerGateway_customerGatewayId,

    -- ** DisassociateIamInstanceProfile
    disassociateIamInstanceProfile_associationId,
    disassociateIamInstanceProfileResponse_iamInstanceProfileAssociation,
    disassociateIamInstanceProfileResponse_httpStatus,

    -- ** TerminateClientVpnConnections
    terminateClientVpnConnections_connectionId,
    terminateClientVpnConnections_username,
    terminateClientVpnConnections_dryRun,
    terminateClientVpnConnections_clientVpnEndpointId,
    terminateClientVpnConnectionsResponse_connectionStatuses,
    terminateClientVpnConnectionsResponse_username,
    terminateClientVpnConnectionsResponse_clientVpnEndpointId,
    terminateClientVpnConnectionsResponse_httpStatus,

    -- ** CreateTransitGatewayConnect
    createTransitGatewayConnect_tagSpecifications,
    createTransitGatewayConnect_dryRun,
    createTransitGatewayConnect_transportTransitGatewayAttachmentId,
    createTransitGatewayConnect_options,
    createTransitGatewayConnectResponse_transitGatewayConnect,
    createTransitGatewayConnectResponse_httpStatus,

    -- ** DisassociateRouteTable
    disassociateRouteTable_dryRun,
    disassociateRouteTable_associationId,

    -- ** GetConsoleScreenshot
    getConsoleScreenshot_wakeUp,
    getConsoleScreenshot_dryRun,
    getConsoleScreenshot_instanceId,
    getConsoleScreenshotResponse_instanceId,
    getConsoleScreenshotResponse_imageData,
    getConsoleScreenshotResponse_httpStatus,

    -- ** GetFlowLogsIntegrationTemplate
    getFlowLogsIntegrationTemplate_dryRun,
    getFlowLogsIntegrationTemplate_flowLogId,
    getFlowLogsIntegrationTemplate_configDeliveryS3DestinationArn,
    getFlowLogsIntegrationTemplate_integrateServices,
    getFlowLogsIntegrationTemplateResponse_result,
    getFlowLogsIntegrationTemplateResponse_httpStatus,

    -- ** ResetEbsDefaultKmsKeyId
    resetEbsDefaultKmsKeyId_dryRun,
    resetEbsDefaultKmsKeyIdResponse_kmsKeyId,
    resetEbsDefaultKmsKeyIdResponse_httpStatus,

    -- ** AssignIpv6Addresses
    assignIpv6Addresses_ipv6AddressCount,
    assignIpv6Addresses_ipv6Prefixes,
    assignIpv6Addresses_ipv6PrefixCount,
    assignIpv6Addresses_ipv6Addresses,
    assignIpv6Addresses_networkInterfaceId,
    assignIpv6AddressesResponse_networkInterfaceId,
    assignIpv6AddressesResponse_assignedIpv6Prefixes,
    assignIpv6AddressesResponse_assignedIpv6Addresses,
    assignIpv6AddressesResponse_httpStatus,

    -- ** ModifyVpnTunnelOptions
    modifyVpnTunnelOptions_dryRun,
    modifyVpnTunnelOptions_vpnConnectionId,
    modifyVpnTunnelOptions_vpnTunnelOutsideIpAddress,
    modifyVpnTunnelOptions_tunnelOptions,
    modifyVpnTunnelOptionsResponse_vpnConnection,
    modifyVpnTunnelOptionsResponse_httpStatus,

    -- ** ModifyEbsDefaultKmsKeyId
    modifyEbsDefaultKmsKeyId_dryRun,
    modifyEbsDefaultKmsKeyId_kmsKeyId,
    modifyEbsDefaultKmsKeyIdResponse_kmsKeyId,
    modifyEbsDefaultKmsKeyIdResponse_httpStatus,

    -- ** DeleteSpotDatafeedSubscription
    deleteSpotDatafeedSubscription_dryRun,

    -- ** ModifyVolume
    modifyVolume_multiAttachEnabled,
    modifyVolume_throughput,
    modifyVolume_size,
    modifyVolume_iops,
    modifyVolume_volumeType,
    modifyVolume_dryRun,
    modifyVolume_volumeId,
    modifyVolumeResponse_volumeModification,
    modifyVolumeResponse_httpStatus,

    -- ** EnableVpcClassicLink
    enableVpcClassicLink_dryRun,
    enableVpcClassicLink_vpcId,
    enableVpcClassicLinkResponse_return,
    enableVpcClassicLinkResponse_httpStatus,

    -- ** DescribePlacementGroups
    describePlacementGroups_filters,
    describePlacementGroups_groupNames,
    describePlacementGroups_groupIds,
    describePlacementGroups_dryRun,
    describePlacementGroupsResponse_placementGroups,
    describePlacementGroupsResponse_httpStatus,

    -- ** ProvisionByoipCidr
    provisionByoipCidr_cidrAuthorizationContext,
    provisionByoipCidr_poolTagSpecifications,
    provisionByoipCidr_publiclyAdvertisable,
    provisionByoipCidr_description,
    provisionByoipCidr_dryRun,
    provisionByoipCidr_multiRegion,
    provisionByoipCidr_cidr,
    provisionByoipCidrResponse_byoipCidr,
    provisionByoipCidrResponse_httpStatus,

    -- ** DisassociateEnclaveCertificateIamRole
    disassociateEnclaveCertificateIamRole_certificateArn,
    disassociateEnclaveCertificateIamRole_dryRun,
    disassociateEnclaveCertificateIamRole_roleArn,
    disassociateEnclaveCertificateIamRoleResponse_return,
    disassociateEnclaveCertificateIamRoleResponse_httpStatus,

    -- ** ModifyAvailabilityZoneGroup
    modifyAvailabilityZoneGroup_dryRun,
    modifyAvailabilityZoneGroup_groupName,
    modifyAvailabilityZoneGroup_optInStatus,
    modifyAvailabilityZoneGroupResponse_return,
    modifyAvailabilityZoneGroupResponse_httpStatus,

    -- ** DescribeStaleSecurityGroups
    describeStaleSecurityGroups_nextToken,
    describeStaleSecurityGroups_dryRun,
    describeStaleSecurityGroups_maxResults,
    describeStaleSecurityGroups_vpcId,
    describeStaleSecurityGroupsResponse_staleSecurityGroupSet,
    describeStaleSecurityGroupsResponse_nextToken,
    describeStaleSecurityGroupsResponse_httpStatus,

    -- ** CreateCarrierGateway
    createCarrierGateway_clientToken,
    createCarrierGateway_tagSpecifications,
    createCarrierGateway_dryRun,
    createCarrierGateway_vpcId,
    createCarrierGatewayResponse_carrierGateway,
    createCarrierGatewayResponse_httpStatus,

    -- ** DescribeExportImageTasks
    describeExportImageTasks_exportImageTaskIds,
    describeExportImageTasks_filters,
    describeExportImageTasks_nextToken,
    describeExportImageTasks_dryRun,
    describeExportImageTasks_maxResults,
    describeExportImageTasksResponse_exportImageTasks,
    describeExportImageTasksResponse_nextToken,
    describeExportImageTasksResponse_httpStatus,

    -- ** PurchaseScheduledInstances
    purchaseScheduledInstances_clientToken,
    purchaseScheduledInstances_dryRun,
    purchaseScheduledInstances_purchaseRequests,
    purchaseScheduledInstancesResponse_scheduledInstanceSet,
    purchaseScheduledInstancesResponse_httpStatus,

    -- ** EnableVgwRoutePropagation
    enableVgwRoutePropagation_dryRun,
    enableVgwRoutePropagation_gatewayId,
    enableVgwRoutePropagation_routeTableId,

    -- ** DescribeSpotFleetRequestHistory
    describeSpotFleetRequestHistory_nextToken,
    describeSpotFleetRequestHistory_eventType,
    describeSpotFleetRequestHistory_dryRun,
    describeSpotFleetRequestHistory_maxResults,
    describeSpotFleetRequestHistory_spotFleetRequestId,
    describeSpotFleetRequestHistory_startTime,
    describeSpotFleetRequestHistoryResponse_startTime,
    describeSpotFleetRequestHistoryResponse_lastEvaluatedTime,
    describeSpotFleetRequestHistoryResponse_nextToken,
    describeSpotFleetRequestHistoryResponse_historyRecords,
    describeSpotFleetRequestHistoryResponse_spotFleetRequestId,
    describeSpotFleetRequestHistoryResponse_httpStatus,

    -- ** ModifySnapshotAttribute
    modifySnapshotAttribute_attribute,
    modifySnapshotAttribute_createVolumePermission,
    modifySnapshotAttribute_userIds,
    modifySnapshotAttribute_groupNames,
    modifySnapshotAttribute_operationType,
    modifySnapshotAttribute_dryRun,
    modifySnapshotAttribute_snapshotId,

    -- ** DescribeIamInstanceProfileAssociations
    describeIamInstanceProfileAssociations_filters,
    describeIamInstanceProfileAssociations_nextToken,
    describeIamInstanceProfileAssociations_associationIds,
    describeIamInstanceProfileAssociations_maxResults,
    describeIamInstanceProfileAssociationsResponse_iamInstanceProfileAssociations,
    describeIamInstanceProfileAssociationsResponse_nextToken,
    describeIamInstanceProfileAssociationsResponse_httpStatus,

    -- ** DescribeNetworkInsightsPaths
    describeNetworkInsightsPaths_networkInsightsPathIds,
    describeNetworkInsightsPaths_filters,
    describeNetworkInsightsPaths_nextToken,
    describeNetworkInsightsPaths_dryRun,
    describeNetworkInsightsPaths_maxResults,
    describeNetworkInsightsPathsResponse_networkInsightsPaths,
    describeNetworkInsightsPathsResponse_nextToken,
    describeNetworkInsightsPathsResponse_httpStatus,

    -- ** CreateSnapshot
    createSnapshot_outpostArn,
    createSnapshot_tagSpecifications,
    createSnapshot_description,
    createSnapshot_dryRun,
    createSnapshot_volumeId,
    snapshot_stateMessage,
    snapshot_ownerAlias,
    snapshot_dataEncryptionKeyId,
    snapshot_outpostArn,
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

    -- ** CreateLocalGatewayRoute
    createLocalGatewayRoute_dryRun,
    createLocalGatewayRoute_destinationCidrBlock,
    createLocalGatewayRoute_localGatewayRouteTableId,
    createLocalGatewayRoute_localGatewayVirtualInterfaceGroupId,
    createLocalGatewayRouteResponse_route,
    createLocalGatewayRouteResponse_httpStatus,

    -- ** CreateNetworkAclEntry
    createNetworkAclEntry_ipv6CidrBlock,
    createNetworkAclEntry_icmpTypeCode,
    createNetworkAclEntry_portRange,
    createNetworkAclEntry_cidrBlock,
    createNetworkAclEntry_dryRun,
    createNetworkAclEntry_egress,
    createNetworkAclEntry_networkAclId,
    createNetworkAclEntry_protocol,
    createNetworkAclEntry_ruleAction,
    createNetworkAclEntry_ruleNumber,

    -- ** DescribeTransitGatewayAttachments
    describeTransitGatewayAttachments_filters,
    describeTransitGatewayAttachments_nextToken,
    describeTransitGatewayAttachments_transitGatewayAttachmentIds,
    describeTransitGatewayAttachments_dryRun,
    describeTransitGatewayAttachments_maxResults,
    describeTransitGatewayAttachmentsResponse_nextToken,
    describeTransitGatewayAttachmentsResponse_transitGatewayAttachments,
    describeTransitGatewayAttachmentsResponse_httpStatus,

    -- ** CreateReservedInstancesListing
    createReservedInstancesListing_clientToken,
    createReservedInstancesListing_instanceCount,
    createReservedInstancesListing_priceSchedules,
    createReservedInstancesListing_reservedInstancesId,
    createReservedInstancesListingResponse_reservedInstancesListings,
    createReservedInstancesListingResponse_httpStatus,

    -- ** DescribeIpv6Pools
    describeIpv6Pools_poolIds,
    describeIpv6Pools_filters,
    describeIpv6Pools_nextToken,
    describeIpv6Pools_dryRun,
    describeIpv6Pools_maxResults,
    describeIpv6PoolsResponse_ipv6Pools,
    describeIpv6PoolsResponse_nextToken,
    describeIpv6PoolsResponse_httpStatus,

    -- ** AttachVpnGateway
    attachVpnGateway_dryRun,
    attachVpnGateway_vpcId,
    attachVpnGateway_vpnGatewayId,
    attachVpnGatewayResponse_vpcAttachment,
    attachVpnGatewayResponse_httpStatus,

    -- ** DescribeLocalGateways
    describeLocalGateways_filters,
    describeLocalGateways_nextToken,
    describeLocalGateways_localGatewayIds,
    describeLocalGateways_dryRun,
    describeLocalGateways_maxResults,
    describeLocalGatewaysResponse_localGateways,
    describeLocalGatewaysResponse_nextToken,
    describeLocalGatewaysResponse_httpStatus,

    -- ** ModifyVpcEndpointServicePermissions
    modifyVpcEndpointServicePermissions_removeAllowedPrincipals,
    modifyVpcEndpointServicePermissions_addAllowedPrincipals,
    modifyVpcEndpointServicePermissions_dryRun,
    modifyVpcEndpointServicePermissions_serviceId,
    modifyVpcEndpointServicePermissionsResponse_returnValue,
    modifyVpcEndpointServicePermissionsResponse_httpStatus,

    -- ** ExportClientVpnClientCertificateRevocationList
    exportClientVpnClientCertificateRevocationList_dryRun,
    exportClientVpnClientCertificateRevocationList_clientVpnEndpointId,
    exportClientVpnClientCertificateRevocationListResponse_status,
    exportClientVpnClientCertificateRevocationListResponse_certificateRevocationList,
    exportClientVpnClientCertificateRevocationListResponse_httpStatus,

    -- ** CreateDhcpOptions
    createDhcpOptions_tagSpecifications,
    createDhcpOptions_dryRun,
    createDhcpOptions_dhcpConfigurations,
    createDhcpOptionsResponse_dhcpOptions,
    createDhcpOptionsResponse_httpStatus,

    -- ** RegisterTransitGatewayMulticastGroupSources
    registerTransitGatewayMulticastGroupSources_networkInterfaceIds,
    registerTransitGatewayMulticastGroupSources_transitGatewayMulticastDomainId,
    registerTransitGatewayMulticastGroupSources_groupIpAddress,
    registerTransitGatewayMulticastGroupSources_dryRun,
    registerTransitGatewayMulticastGroupSourcesResponse_registeredMulticastGroupSources,
    registerTransitGatewayMulticastGroupSourcesResponse_httpStatus,

    -- ** DescribeAccountAttributes
    describeAccountAttributes_attributeNames,
    describeAccountAttributes_dryRun,
    describeAccountAttributesResponse_accountAttributes,
    describeAccountAttributesResponse_httpStatus,

    -- ** GetTransitGatewayRouteTablePropagations
    getTransitGatewayRouteTablePropagations_filters,
    getTransitGatewayRouteTablePropagations_nextToken,
    getTransitGatewayRouteTablePropagations_dryRun,
    getTransitGatewayRouteTablePropagations_maxResults,
    getTransitGatewayRouteTablePropagations_transitGatewayRouteTableId,
    getTransitGatewayRouteTablePropagationsResponse_transitGatewayRouteTablePropagations,
    getTransitGatewayRouteTablePropagationsResponse_nextToken,
    getTransitGatewayRouteTablePropagationsResponse_httpStatus,

    -- ** ModifyFpgaImageAttribute
    modifyFpgaImageAttribute_attribute,
    modifyFpgaImageAttribute_userIds,
    modifyFpgaImageAttribute_userGroups,
    modifyFpgaImageAttribute_loadPermission,
    modifyFpgaImageAttribute_name,
    modifyFpgaImageAttribute_operationType,
    modifyFpgaImageAttribute_productCodes,
    modifyFpgaImageAttribute_description,
    modifyFpgaImageAttribute_dryRun,
    modifyFpgaImageAttribute_fpgaImageId,
    modifyFpgaImageAttributeResponse_fpgaImageAttribute,
    modifyFpgaImageAttributeResponse_httpStatus,

    -- ** ModifyHosts
    modifyHosts_instanceFamily,
    modifyHosts_instanceType,
    modifyHosts_hostRecovery,
    modifyHosts_autoPlacement,
    modifyHosts_hostIds,
    modifyHostsResponse_unsuccessful,
    modifyHostsResponse_successful,
    modifyHostsResponse_httpStatus,

    -- ** RebootInstances
    rebootInstances_dryRun,
    rebootInstances_instanceIds,

    -- ** ModifyVpcEndpointServiceConfiguration
    modifyVpcEndpointServiceConfiguration_removeGatewayLoadBalancerArns,
    modifyVpcEndpointServiceConfiguration_removePrivateDnsName,
    modifyVpcEndpointServiceConfiguration_addGatewayLoadBalancerArns,
    modifyVpcEndpointServiceConfiguration_removeNetworkLoadBalancerArns,
    modifyVpcEndpointServiceConfiguration_acceptanceRequired,
    modifyVpcEndpointServiceConfiguration_addNetworkLoadBalancerArns,
    modifyVpcEndpointServiceConfiguration_privateDnsName,
    modifyVpcEndpointServiceConfiguration_dryRun,
    modifyVpcEndpointServiceConfiguration_serviceId,
    modifyVpcEndpointServiceConfigurationResponse_return,
    modifyVpcEndpointServiceConfigurationResponse_httpStatus,

    -- ** CreateTransitGateway
    createTransitGateway_tagSpecifications,
    createTransitGateway_options,
    createTransitGateway_description,
    createTransitGateway_dryRun,
    createTransitGatewayResponse_transitGateway,
    createTransitGatewayResponse_httpStatus,

    -- ** UnassignIpv6Addresses
    unassignIpv6Addresses_ipv6Prefixes,
    unassignIpv6Addresses_ipv6Addresses,
    unassignIpv6Addresses_networkInterfaceId,
    unassignIpv6AddressesResponse_unassignedIpv6Prefixes,
    unassignIpv6AddressesResponse_networkInterfaceId,
    unassignIpv6AddressesResponse_unassignedIpv6Addresses,
    unassignIpv6AddressesResponse_httpStatus,

    -- ** DeleteTrafficMirrorSession
    deleteTrafficMirrorSession_dryRun,
    deleteTrafficMirrorSession_trafficMirrorSessionId,
    deleteTrafficMirrorSessionResponse_trafficMirrorSessionId,
    deleteTrafficMirrorSessionResponse_httpStatus,

    -- ** CreateManagedPrefixList
    createManagedPrefixList_clientToken,
    createManagedPrefixList_entries,
    createManagedPrefixList_tagSpecifications,
    createManagedPrefixList_dryRun,
    createManagedPrefixList_prefixListName,
    createManagedPrefixList_maxEntries,
    createManagedPrefixList_addressFamily,
    createManagedPrefixListResponse_prefixList,
    createManagedPrefixListResponse_httpStatus,

    -- ** CreateReplaceRootVolumeTask
    createReplaceRootVolumeTask_clientToken,
    createReplaceRootVolumeTask_tagSpecifications,
    createReplaceRootVolumeTask_dryRun,
    createReplaceRootVolumeTask_snapshotId,
    createReplaceRootVolumeTask_instanceId,
    createReplaceRootVolumeTaskResponse_replaceRootVolumeTask,
    createReplaceRootVolumeTaskResponse_httpStatus,

    -- ** AssociateIamInstanceProfile
    associateIamInstanceProfile_iamInstanceProfile,
    associateIamInstanceProfile_instanceId,
    associateIamInstanceProfileResponse_iamInstanceProfileAssociation,
    associateIamInstanceProfileResponse_httpStatus,

    -- ** ModifyDefaultCreditSpecification
    modifyDefaultCreditSpecification_dryRun,
    modifyDefaultCreditSpecification_instanceFamily,
    modifyDefaultCreditSpecification_cpuCredits,
    modifyDefaultCreditSpecificationResponse_instanceFamilyCreditSpecification,
    modifyDefaultCreditSpecificationResponse_httpStatus,

    -- ** DeleteEgressOnlyInternetGateway
    deleteEgressOnlyInternetGateway_dryRun,
    deleteEgressOnlyInternetGateway_egressOnlyInternetGatewayId,
    deleteEgressOnlyInternetGatewayResponse_returnCode,
    deleteEgressOnlyInternetGatewayResponse_httpStatus,

    -- ** PurchaseHostReservation
    purchaseHostReservation_currencyCode,
    purchaseHostReservation_clientToken,
    purchaseHostReservation_tagSpecifications,
    purchaseHostReservation_limitPrice,
    purchaseHostReservation_hostIdSet,
    purchaseHostReservation_offeringId,
    purchaseHostReservationResponse_currencyCode,
    purchaseHostReservationResponse_clientToken,
    purchaseHostReservationResponse_totalHourlyPrice,
    purchaseHostReservationResponse_totalUpfrontPrice,
    purchaseHostReservationResponse_purchase,
    purchaseHostReservationResponse_httpStatus,

    -- ** ModifyTransitGatewayVpcAttachment
    modifyTransitGatewayVpcAttachment_addSubnetIds,
    modifyTransitGatewayVpcAttachment_options,
    modifyTransitGatewayVpcAttachment_removeSubnetIds,
    modifyTransitGatewayVpcAttachment_dryRun,
    modifyTransitGatewayVpcAttachment_transitGatewayAttachmentId,
    modifyTransitGatewayVpcAttachmentResponse_transitGatewayVpcAttachment,
    modifyTransitGatewayVpcAttachmentResponse_httpStatus,

    -- ** CreateImage
    createImage_tagSpecifications,
    createImage_noReboot,
    createImage_description,
    createImage_blockDeviceMappings,
    createImage_dryRun,
    createImage_instanceId,
    createImage_name,
    createImageResponse_imageId,
    createImageResponse_httpStatus,

    -- ** DescribeClassicLinkInstances
    describeClassicLinkInstances_filters,
    describeClassicLinkInstances_nextToken,
    describeClassicLinkInstances_instanceIds,
    describeClassicLinkInstances_dryRun,
    describeClassicLinkInstances_maxResults,
    describeClassicLinkInstancesResponse_nextToken,
    describeClassicLinkInstancesResponse_instances,
    describeClassicLinkInstancesResponse_httpStatus,

    -- ** TerminateInstances
    terminateInstances_dryRun,
    terminateInstances_instanceIds,
    terminateInstancesResponse_terminatingInstances,
    terminateInstancesResponse_httpStatus,

    -- ** DescribeStoreImageTasks
    describeStoreImageTasks_filters,
    describeStoreImageTasks_imageIds,
    describeStoreImageTasks_nextToken,
    describeStoreImageTasks_dryRun,
    describeStoreImageTasks_maxResults,
    describeStoreImageTasksResponse_storeImageTaskResults,
    describeStoreImageTasksResponse_nextToken,
    describeStoreImageTasksResponse_httpStatus,

    -- ** GetVpnConnectionDeviceTypes
    getVpnConnectionDeviceTypes_nextToken,
    getVpnConnectionDeviceTypes_dryRun,
    getVpnConnectionDeviceTypes_maxResults,
    getVpnConnectionDeviceTypesResponse_vpnConnectionDeviceTypes,
    getVpnConnectionDeviceTypesResponse_nextToken,
    getVpnConnectionDeviceTypesResponse_httpStatus,

    -- ** GetTransitGatewayPrefixListReferences
    getTransitGatewayPrefixListReferences_filters,
    getTransitGatewayPrefixListReferences_nextToken,
    getTransitGatewayPrefixListReferences_dryRun,
    getTransitGatewayPrefixListReferences_maxResults,
    getTransitGatewayPrefixListReferences_transitGatewayRouteTableId,
    getTransitGatewayPrefixListReferencesResponse_transitGatewayPrefixListReferences,
    getTransitGatewayPrefixListReferencesResponse_nextToken,
    getTransitGatewayPrefixListReferencesResponse_httpStatus,

    -- ** DescribeKeyPairs
    describeKeyPairs_filters,
    describeKeyPairs_keyPairIds,
    describeKeyPairs_keyNames,
    describeKeyPairs_dryRun,
    describeKeyPairsResponse_keyPairs,
    describeKeyPairsResponse_httpStatus,

    -- ** DisableFastSnapshotRestores
    disableFastSnapshotRestores_dryRun,
    disableFastSnapshotRestores_availabilityZones,
    disableFastSnapshotRestores_sourceSnapshotIds,
    disableFastSnapshotRestoresResponse_unsuccessful,
    disableFastSnapshotRestoresResponse_successful,
    disableFastSnapshotRestoresResponse_httpStatus,

    -- ** DescribeLaunchTemplates
    describeLaunchTemplates_filters,
    describeLaunchTemplates_nextToken,
    describeLaunchTemplates_launchTemplateIds,
    describeLaunchTemplates_dryRun,
    describeLaunchTemplates_maxResults,
    describeLaunchTemplates_launchTemplateNames,
    describeLaunchTemplatesResponse_launchTemplates,
    describeLaunchTemplatesResponse_nextToken,
    describeLaunchTemplatesResponse_httpStatus,

    -- ** CreateVpnConnectionRoute
    createVpnConnectionRoute_destinationCidrBlock,
    createVpnConnectionRoute_vpnConnectionId,

    -- ** AssociateRouteTable
    associateRouteTable_subnetId,
    associateRouteTable_gatewayId,
    associateRouteTable_dryRun,
    associateRouteTable_routeTableId,
    associateRouteTableResponse_associationId,
    associateRouteTableResponse_associationState,
    associateRouteTableResponse_httpStatus,

    -- ** CreateSubnetCidrReservation
    createSubnetCidrReservation_tagSpecifications,
    createSubnetCidrReservation_description,
    createSubnetCidrReservation_dryRun,
    createSubnetCidrReservation_subnetId,
    createSubnetCidrReservation_cidr,
    createSubnetCidrReservation_reservationType,
    createSubnetCidrReservationResponse_subnetCidrReservation,
    createSubnetCidrReservationResponse_httpStatus,

    -- ** DescribeVpnGateways
    describeVpnGateways_filters,
    describeVpnGateways_vpnGatewayIds,
    describeVpnGateways_dryRun,
    describeVpnGatewaysResponse_vpnGateways,
    describeVpnGatewaysResponse_httpStatus,

    -- ** ModifyVpnConnectionOptions
    modifyVpnConnectionOptions_remoteIpv4NetworkCidr,
    modifyVpnConnectionOptions_localIpv4NetworkCidr,
    modifyVpnConnectionOptions_remoteIpv6NetworkCidr,
    modifyVpnConnectionOptions_localIpv6NetworkCidr,
    modifyVpnConnectionOptions_dryRun,
    modifyVpnConnectionOptions_vpnConnectionId,
    modifyVpnConnectionOptionsResponse_vpnConnection,
    modifyVpnConnectionOptionsResponse_httpStatus,

    -- ** GetConsoleOutput
    getConsoleOutput_latest,
    getConsoleOutput_dryRun,
    getConsoleOutput_instanceId,
    getConsoleOutputResponse_instanceId,
    getConsoleOutputResponse_output,
    getConsoleOutputResponse_timestamp,
    getConsoleOutputResponse_httpStatus,

    -- ** DescribeHosts
    describeHosts_nextToken,
    describeHosts_filter,
    describeHosts_hostIds,
    describeHosts_maxResults,
    describeHostsResponse_hosts,
    describeHostsResponse_nextToken,
    describeHostsResponse_httpStatus,

    -- ** DescribeImageAttribute
    describeImageAttribute_dryRun,
    describeImageAttribute_attribute,
    describeImageAttribute_imageId,
    describeImageAttributeResponse_launchPermissions,
    describeImageAttributeResponse_ramdiskId,
    describeImageAttributeResponse_kernelId,
    describeImageAttributeResponse_sriovNetSupport,
    describeImageAttributeResponse_imageId,
    describeImageAttributeResponse_bootMode,
    describeImageAttributeResponse_productCodes,
    describeImageAttributeResponse_description,
    describeImageAttributeResponse_blockDeviceMappings,
    describeImageAttributeResponse_httpStatus,

    -- ** ModifyIdFormat
    modifyIdFormat_resource,
    modifyIdFormat_useLongIds,

    -- ** RegisterTransitGatewayMulticastGroupMembers
    registerTransitGatewayMulticastGroupMembers_networkInterfaceIds,
    registerTransitGatewayMulticastGroupMembers_transitGatewayMulticastDomainId,
    registerTransitGatewayMulticastGroupMembers_groupIpAddress,
    registerTransitGatewayMulticastGroupMembers_dryRun,
    registerTransitGatewayMulticastGroupMembersResponse_registeredMulticastGroupMembers,
    registerTransitGatewayMulticastGroupMembersResponse_httpStatus,

    -- ** DeleteManagedPrefixList
    deleteManagedPrefixList_dryRun,
    deleteManagedPrefixList_prefixListId,
    deleteManagedPrefixListResponse_prefixList,
    deleteManagedPrefixListResponse_httpStatus,

    -- ** DeleteRouteTable
    deleteRouteTable_dryRun,
    deleteRouteTable_routeTableId,

    -- ** ResetImageAttribute
    resetImageAttribute_dryRun,
    resetImageAttribute_attribute,
    resetImageAttribute_imageId,

    -- ** ModifyTransitGatewayPrefixListReference
    modifyTransitGatewayPrefixListReference_blackhole,
    modifyTransitGatewayPrefixListReference_transitGatewayAttachmentId,
    modifyTransitGatewayPrefixListReference_dryRun,
    modifyTransitGatewayPrefixListReference_transitGatewayRouteTableId,
    modifyTransitGatewayPrefixListReference_prefixListId,
    modifyTransitGatewayPrefixListReferenceResponse_transitGatewayPrefixListReference,
    modifyTransitGatewayPrefixListReferenceResponse_httpStatus,

    -- ** DescribeTransitGatewayRouteTables
    describeTransitGatewayRouteTables_filters,
    describeTransitGatewayRouteTables_nextToken,
    describeTransitGatewayRouteTables_dryRun,
    describeTransitGatewayRouteTables_transitGatewayRouteTableIds,
    describeTransitGatewayRouteTables_maxResults,
    describeTransitGatewayRouteTablesResponse_transitGatewayRouteTables,
    describeTransitGatewayRouteTablesResponse_nextToken,
    describeTransitGatewayRouteTablesResponse_httpStatus,

    -- ** CreateEgressOnlyInternetGateway
    createEgressOnlyInternetGateway_clientToken,
    createEgressOnlyInternetGateway_tagSpecifications,
    createEgressOnlyInternetGateway_dryRun,
    createEgressOnlyInternetGateway_vpcId,
    createEgressOnlyInternetGatewayResponse_clientToken,
    createEgressOnlyInternetGatewayResponse_egressOnlyInternetGateway,
    createEgressOnlyInternetGatewayResponse_httpStatus,

    -- ** DescribeReservedInstancesModifications
    describeReservedInstancesModifications_filters,
    describeReservedInstancesModifications_reservedInstancesModificationIds,
    describeReservedInstancesModifications_nextToken,
    describeReservedInstancesModificationsResponse_nextToken,
    describeReservedInstancesModificationsResponse_reservedInstancesModifications,
    describeReservedInstancesModificationsResponse_httpStatus,

    -- ** DescribeSpotInstanceRequests
    describeSpotInstanceRequests_filters,
    describeSpotInstanceRequests_spotInstanceRequestIds,
    describeSpotInstanceRequests_nextToken,
    describeSpotInstanceRequests_dryRun,
    describeSpotInstanceRequests_maxResults,
    describeSpotInstanceRequestsResponse_nextToken,
    describeSpotInstanceRequestsResponse_spotInstanceRequests,
    describeSpotInstanceRequestsResponse_httpStatus,

    -- ** RevokeClientVpnIngress
    revokeClientVpnIngress_accessGroupId,
    revokeClientVpnIngress_revokeAllGroups,
    revokeClientVpnIngress_dryRun,
    revokeClientVpnIngress_clientVpnEndpointId,
    revokeClientVpnIngress_targetNetworkCidr,
    revokeClientVpnIngressResponse_status,
    revokeClientVpnIngressResponse_httpStatus,

    -- ** UnassignPrivateIpAddresses
    unassignPrivateIpAddresses_privateIpAddresses,
    unassignPrivateIpAddresses_ipv4Prefixes,
    unassignPrivateIpAddresses_networkInterfaceId,

    -- ** DescribeNetworkInterfacePermissions
    describeNetworkInterfacePermissions_filters,
    describeNetworkInterfacePermissions_nextToken,
    describeNetworkInterfacePermissions_networkInterfacePermissionIds,
    describeNetworkInterfacePermissions_maxResults,
    describeNetworkInterfacePermissionsResponse_networkInterfacePermissions,
    describeNetworkInterfacePermissionsResponse_nextToken,
    describeNetworkInterfacePermissionsResponse_httpStatus,

    -- ** EnableFastSnapshotRestores
    enableFastSnapshotRestores_dryRun,
    enableFastSnapshotRestores_availabilityZones,
    enableFastSnapshotRestores_sourceSnapshotIds,
    enableFastSnapshotRestoresResponse_unsuccessful,
    enableFastSnapshotRestoresResponse_successful,
    enableFastSnapshotRestoresResponse_httpStatus,

    -- ** DescribeVpcEndpointServicePermissions
    describeVpcEndpointServicePermissions_filters,
    describeVpcEndpointServicePermissions_nextToken,
    describeVpcEndpointServicePermissions_dryRun,
    describeVpcEndpointServicePermissions_maxResults,
    describeVpcEndpointServicePermissions_serviceId,
    describeVpcEndpointServicePermissionsResponse_nextToken,
    describeVpcEndpointServicePermissionsResponse_allowedPrincipals,
    describeVpcEndpointServicePermissionsResponse_httpStatus,

    -- ** DeleteDhcpOptions
    deleteDhcpOptions_dryRun,
    deleteDhcpOptions_dhcpOptionsId,

    -- ** CreateRestoreImageTask
    createRestoreImageTask_tagSpecifications,
    createRestoreImageTask_name,
    createRestoreImageTask_dryRun,
    createRestoreImageTask_bucket,
    createRestoreImageTask_objectKey,
    createRestoreImageTaskResponse_imageId,
    createRestoreImageTaskResponse_httpStatus,

    -- ** RegisterInstanceEventNotificationAttributes
    registerInstanceEventNotificationAttributes_instanceTagAttribute,
    registerInstanceEventNotificationAttributes_dryRun,
    registerInstanceEventNotificationAttributesResponse_instanceTagAttribute,
    registerInstanceEventNotificationAttributesResponse_httpStatus,

    -- ** GetVpnConnectionDeviceSampleConfiguration
    getVpnConnectionDeviceSampleConfiguration_internetKeyExchangeVersion,
    getVpnConnectionDeviceSampleConfiguration_dryRun,
    getVpnConnectionDeviceSampleConfiguration_vpnConnectionId,
    getVpnConnectionDeviceSampleConfiguration_vpnConnectionDeviceTypeId,
    getVpnConnectionDeviceSampleConfigurationResponse_vpnConnectionDeviceSampleConfiguration,
    getVpnConnectionDeviceSampleConfigurationResponse_httpStatus,

    -- ** DeleteSubnetCidrReservation
    deleteSubnetCidrReservation_dryRun,
    deleteSubnetCidrReservation_subnetCidrReservationId,
    deleteSubnetCidrReservationResponse_deletedSubnetCidrReservation,
    deleteSubnetCidrReservationResponse_httpStatus,

    -- ** DescribeNetworkAcls
    describeNetworkAcls_filters,
    describeNetworkAcls_nextToken,
    describeNetworkAcls_networkAclIds,
    describeNetworkAcls_dryRun,
    describeNetworkAcls_maxResults,
    describeNetworkAclsResponse_networkAcls,
    describeNetworkAclsResponse_nextToken,
    describeNetworkAclsResponse_httpStatus,

    -- ** CancelImportTask
    cancelImportTask_cancelReason,
    cancelImportTask_importTaskId,
    cancelImportTask_dryRun,
    cancelImportTaskResponse_state,
    cancelImportTaskResponse_importTaskId,
    cancelImportTaskResponse_previousState,
    cancelImportTaskResponse_httpStatus,

    -- ** DetachClassicLinkVpc
    detachClassicLinkVpc_dryRun,
    detachClassicLinkVpc_instanceId,
    detachClassicLinkVpc_vpcId,
    detachClassicLinkVpcResponse_return,
    detachClassicLinkVpcResponse_httpStatus,

    -- ** CreateCapacityReservationFleet
    createCapacityReservationFleet_clientToken,
    createCapacityReservationFleet_endDate,
    createCapacityReservationFleet_instanceMatchCriteria,
    createCapacityReservationFleet_tagSpecifications,
    createCapacityReservationFleet_tenancy,
    createCapacityReservationFleet_allocationStrategy,
    createCapacityReservationFleet_dryRun,
    createCapacityReservationFleet_instanceTypeSpecifications,
    createCapacityReservationFleet_totalTargetCapacity,
    createCapacityReservationFleetResponse_capacityReservationFleetId,
    createCapacityReservationFleetResponse_state,
    createCapacityReservationFleetResponse_totalFulfilledCapacity,
    createCapacityReservationFleetResponse_endDate,
    createCapacityReservationFleetResponse_instanceMatchCriteria,
    createCapacityReservationFleetResponse_fleetCapacityReservations,
    createCapacityReservationFleetResponse_totalTargetCapacity,
    createCapacityReservationFleetResponse_tenancy,
    createCapacityReservationFleetResponse_allocationStrategy,
    createCapacityReservationFleetResponse_createTime,
    createCapacityReservationFleetResponse_tags,
    createCapacityReservationFleetResponse_httpStatus,

    -- ** DescribeRegions
    describeRegions_regionNames,
    describeRegions_filters,
    describeRegions_allRegions,
    describeRegions_dryRun,
    describeRegionsResponse_regions,
    describeRegionsResponse_httpStatus,

    -- ** MonitorInstances
    monitorInstances_dryRun,
    monitorInstances_instanceIds,
    monitorInstancesResponse_instanceMonitorings,
    monitorInstancesResponse_httpStatus,

    -- ** RejectTransitGatewayMulticastDomainAssociations
    rejectTransitGatewayMulticastDomainAssociations_subnetIds,
    rejectTransitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId,
    rejectTransitGatewayMulticastDomainAssociations_transitGatewayAttachmentId,
    rejectTransitGatewayMulticastDomainAssociations_dryRun,
    rejectTransitGatewayMulticastDomainAssociationsResponse_associations,
    rejectTransitGatewayMulticastDomainAssociationsResponse_httpStatus,

    -- ** AcceptTransitGatewayMulticastDomainAssociations
    acceptTransitGatewayMulticastDomainAssociations_subnetIds,
    acceptTransitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId,
    acceptTransitGatewayMulticastDomainAssociations_transitGatewayAttachmentId,
    acceptTransitGatewayMulticastDomainAssociations_dryRun,
    acceptTransitGatewayMulticastDomainAssociationsResponse_associations,
    acceptTransitGatewayMulticastDomainAssociationsResponse_httpStatus,

    -- ** SearchLocalGatewayRoutes
    searchLocalGatewayRoutes_filters,
    searchLocalGatewayRoutes_nextToken,
    searchLocalGatewayRoutes_dryRun,
    searchLocalGatewayRoutes_maxResults,
    searchLocalGatewayRoutes_localGatewayRouteTableId,
    searchLocalGatewayRoutesResponse_routes,
    searchLocalGatewayRoutesResponse_nextToken,
    searchLocalGatewayRoutesResponse_httpStatus,

    -- ** DeleteClientVpnRoute
    deleteClientVpnRoute_targetVpcSubnetId,
    deleteClientVpnRoute_dryRun,
    deleteClientVpnRoute_clientVpnEndpointId,
    deleteClientVpnRoute_destinationCidrBlock,
    deleteClientVpnRouteResponse_status,
    deleteClientVpnRouteResponse_httpStatus,

    -- ** AcceptVpcPeeringConnection
    acceptVpcPeeringConnection_vpcPeeringConnectionId,
    acceptVpcPeeringConnection_dryRun,
    acceptVpcPeeringConnectionResponse_vpcPeeringConnection,
    acceptVpcPeeringConnectionResponse_httpStatus,

    -- ** ImportSnapshot
    importSnapshot_diskContainer,
    importSnapshot_clientToken,
    importSnapshot_roleName,
    importSnapshot_encrypted,
    importSnapshot_tagSpecifications,
    importSnapshot_kmsKeyId,
    importSnapshot_description,
    importSnapshot_dryRun,
    importSnapshot_clientData,
    importSnapshotResponse_snapshotTaskDetail,
    importSnapshotResponse_importTaskId,
    importSnapshotResponse_description,
    importSnapshotResponse_tags,
    importSnapshotResponse_httpStatus,

    -- ** DescribeAddressesAttribute
    describeAddressesAttribute_attribute,
    describeAddressesAttribute_nextToken,
    describeAddressesAttribute_allocationIds,
    describeAddressesAttribute_dryRun,
    describeAddressesAttribute_maxResults,
    describeAddressesAttributeResponse_addresses,
    describeAddressesAttributeResponse_nextToken,
    describeAddressesAttributeResponse_httpStatus,

    -- ** DescribeVolumeStatus
    describeVolumeStatus_filters,
    describeVolumeStatus_volumeIds,
    describeVolumeStatus_nextToken,
    describeVolumeStatus_dryRun,
    describeVolumeStatus_maxResults,
    describeVolumeStatusResponse_nextToken,
    describeVolumeStatusResponse_volumeStatuses,
    describeVolumeStatusResponse_httpStatus,

    -- ** DescribeReplaceRootVolumeTasks
    describeReplaceRootVolumeTasks_filters,
    describeReplaceRootVolumeTasks_replaceRootVolumeTaskIds,
    describeReplaceRootVolumeTasks_nextToken,
    describeReplaceRootVolumeTasks_dryRun,
    describeReplaceRootVolumeTasks_maxResults,
    describeReplaceRootVolumeTasksResponse_replaceRootVolumeTasks,
    describeReplaceRootVolumeTasksResponse_nextToken,
    describeReplaceRootVolumeTasksResponse_httpStatus,

    -- ** ModifyInstanceEventWindow
    modifyInstanceEventWindow_name,
    modifyInstanceEventWindow_cronExpression,
    modifyInstanceEventWindow_dryRun,
    modifyInstanceEventWindow_timeRanges,
    modifyInstanceEventWindow_instanceEventWindowId,
    modifyInstanceEventWindowResponse_instanceEventWindow,
    modifyInstanceEventWindowResponse_httpStatus,

    -- ** DescribeRouteTables
    describeRouteTables_filters,
    describeRouteTables_nextToken,
    describeRouteTables_dryRun,
    describeRouteTables_maxResults,
    describeRouteTables_routeTableIds,
    describeRouteTablesResponse_nextToken,
    describeRouteTablesResponse_routeTables,
    describeRouteTablesResponse_httpStatus,

    -- ** DescribeAvailabilityZones
    describeAvailabilityZones_zoneNames,
    describeAvailabilityZones_allAvailabilityZones,
    describeAvailabilityZones_zoneIds,
    describeAvailabilityZones_filters,
    describeAvailabilityZones_dryRun,
    describeAvailabilityZonesResponse_availabilityZones,
    describeAvailabilityZonesResponse_httpStatus,

    -- ** ModifyVpcAttribute
    modifyVpcAttribute_enableDnsHostnames,
    modifyVpcAttribute_enableDnsSupport,
    modifyVpcAttribute_vpcId,

    -- ** DescribeClientVpnConnections
    describeClientVpnConnections_filters,
    describeClientVpnConnections_nextToken,
    describeClientVpnConnections_dryRun,
    describeClientVpnConnections_maxResults,
    describeClientVpnConnections_clientVpnEndpointId,
    describeClientVpnConnectionsResponse_connections,
    describeClientVpnConnectionsResponse_nextToken,
    describeClientVpnConnectionsResponse_httpStatus,

    -- ** DescribeFleetHistory
    describeFleetHistory_nextToken,
    describeFleetHistory_eventType,
    describeFleetHistory_dryRun,
    describeFleetHistory_maxResults,
    describeFleetHistory_fleetId,
    describeFleetHistory_startTime,
    describeFleetHistoryResponse_startTime,
    describeFleetHistoryResponse_lastEvaluatedTime,
    describeFleetHistoryResponse_nextToken,
    describeFleetHistoryResponse_historyRecords,
    describeFleetHistoryResponse_fleetId,
    describeFleetHistoryResponse_httpStatus,

    -- ** DescribeImages
    describeImages_owners,
    describeImages_executableUsers,
    describeImages_filters,
    describeImages_imageIds,
    describeImages_includeDeprecated,
    describeImages_dryRun,
    describeImagesResponse_images,
    describeImagesResponse_httpStatus,

    -- ** DescribeElasticGpus
    describeElasticGpus_filters,
    describeElasticGpus_nextToken,
    describeElasticGpus_dryRun,
    describeElasticGpus_maxResults,
    describeElasticGpus_elasticGpuIds,
    describeElasticGpusResponse_elasticGpuSet,
    describeElasticGpusResponse_nextToken,
    describeElasticGpusResponse_maxResults,
    describeElasticGpusResponse_httpStatus,

    -- ** DisassociateTrunkInterface
    disassociateTrunkInterface_clientToken,
    disassociateTrunkInterface_dryRun,
    disassociateTrunkInterface_associationId,
    disassociateTrunkInterfaceResponse_clientToken,
    disassociateTrunkInterfaceResponse_return,
    disassociateTrunkInterfaceResponse_httpStatus,

    -- ** RestoreAddressToClassic
    restoreAddressToClassic_dryRun,
    restoreAddressToClassic_publicIp,
    restoreAddressToClassicResponse_status,
    restoreAddressToClassicResponse_publicIp,
    restoreAddressToClassicResponse_httpStatus,

    -- ** DescribeManagedPrefixLists
    describeManagedPrefixLists_filters,
    describeManagedPrefixLists_prefixListIds,
    describeManagedPrefixLists_nextToken,
    describeManagedPrefixLists_dryRun,
    describeManagedPrefixLists_maxResults,
    describeManagedPrefixListsResponse_nextToken,
    describeManagedPrefixListsResponse_prefixLists,
    describeManagedPrefixListsResponse_httpStatus,

    -- ** CreateKeyPair
    createKeyPair_keyType,
    createKeyPair_tagSpecifications,
    createKeyPair_dryRun,
    createKeyPair_keyName,
    createKeyPairResponse_keyPairId,
    createKeyPairResponse_tags,
    createKeyPairResponse_httpStatus,
    createKeyPairResponse_keyName,
    createKeyPairResponse_keyFingerprint,
    createKeyPairResponse_keyMaterial,

    -- ** GetReservedInstancesExchangeQuote
    getReservedInstancesExchangeQuote_targetConfigurations,
    getReservedInstancesExchangeQuote_dryRun,
    getReservedInstancesExchangeQuote_reservedInstanceIds,
    getReservedInstancesExchangeQuoteResponse_validationFailureReason,
    getReservedInstancesExchangeQuoteResponse_targetConfigurationValueRollup,
    getReservedInstancesExchangeQuoteResponse_currencyCode,
    getReservedInstancesExchangeQuoteResponse_targetConfigurationValueSet,
    getReservedInstancesExchangeQuoteResponse_reservedInstanceValueRollup,
    getReservedInstancesExchangeQuoteResponse_outputReservedInstancesWillExpireAt,
    getReservedInstancesExchangeQuoteResponse_reservedInstanceValueSet,
    getReservedInstancesExchangeQuoteResponse_isValidExchange,
    getReservedInstancesExchangeQuoteResponse_paymentDue,
    getReservedInstancesExchangeQuoteResponse_httpStatus,

    -- ** DeleteVolume
    deleteVolume_dryRun,
    deleteVolume_volumeId,

    -- ** DeprovisionByoipCidr
    deprovisionByoipCidr_dryRun,
    deprovisionByoipCidr_cidr,
    deprovisionByoipCidrResponse_byoipCidr,
    deprovisionByoipCidrResponse_httpStatus,

    -- ** GetSerialConsoleAccessStatus
    getSerialConsoleAccessStatus_dryRun,
    getSerialConsoleAccessStatusResponse_serialConsoleAccessEnabled,
    getSerialConsoleAccessStatusResponse_httpStatus,

    -- ** DeleteVpcEndpointServiceConfigurations
    deleteVpcEndpointServiceConfigurations_dryRun,
    deleteVpcEndpointServiceConfigurations_serviceIds,
    deleteVpcEndpointServiceConfigurationsResponse_unsuccessful,
    deleteVpcEndpointServiceConfigurationsResponse_httpStatus,

    -- ** DescribeSpotFleetInstances
    describeSpotFleetInstances_nextToken,
    describeSpotFleetInstances_dryRun,
    describeSpotFleetInstances_maxResults,
    describeSpotFleetInstances_spotFleetRequestId,
    describeSpotFleetInstancesResponse_nextToken,
    describeSpotFleetInstancesResponse_spotFleetRequestId,
    describeSpotFleetInstancesResponse_activeInstances,
    describeSpotFleetInstancesResponse_httpStatus,

    -- * Types

    -- ** AccountAttribute
    accountAttribute_attributeValues,
    accountAttribute_attributeName,

    -- ** AccountAttributeValue
    accountAttributeValue_attributeValue,

    -- ** ActiveInstance
    activeInstance_instanceId,
    activeInstance_instanceHealth,
    activeInstance_instanceType,
    activeInstance_spotInstanceRequestId,

    -- ** AddPrefixListEntry
    addPrefixListEntry_description,
    addPrefixListEntry_cidr,

    -- ** Address
    address_associationId,
    address_instanceId,
    address_networkInterfaceOwnerId,
    address_allocationId,
    address_carrierIp,
    address_networkBorderGroup,
    address_domain,
    address_networkInterfaceId,
    address_publicIpv4Pool,
    address_customerOwnedIpv4Pool,
    address_customerOwnedIp,
    address_privateIpAddress,
    address_publicIp,
    address_tags,

    -- ** AddressAttribute
    addressAttribute_ptrRecordUpdate,
    addressAttribute_allocationId,
    addressAttribute_publicIp,
    addressAttribute_ptrRecord,

    -- ** AllowedPrincipal
    allowedPrincipal_principalType,
    allowedPrincipal_principal,

    -- ** AlternatePathHint
    alternatePathHint_componentArn,
    alternatePathHint_componentId,

    -- ** AnalysisAclRule
    analysisAclRule_ruleNumber,
    analysisAclRule_ruleAction,
    analysisAclRule_protocol,
    analysisAclRule_portRange,
    analysisAclRule_cidr,
    analysisAclRule_egress,

    -- ** AnalysisComponent
    analysisComponent_arn,
    analysisComponent_id,

    -- ** AnalysisLoadBalancerListener
    analysisLoadBalancerListener_instancePort,
    analysisLoadBalancerListener_loadBalancerPort,

    -- ** AnalysisLoadBalancerTarget
    analysisLoadBalancerTarget_address,
    analysisLoadBalancerTarget_availabilityZone,
    analysisLoadBalancerTarget_port,
    analysisLoadBalancerTarget_instance,

    -- ** AnalysisPacketHeader
    analysisPacketHeader_destinationAddresses,
    analysisPacketHeader_sourceAddresses,
    analysisPacketHeader_protocol,
    analysisPacketHeader_destinationPortRanges,
    analysisPacketHeader_sourcePortRanges,

    -- ** AnalysisRouteTableRoute
    analysisRouteTableRoute_vpcPeeringConnectionId,
    analysisRouteTableRoute_instanceId,
    analysisRouteTableRoute_origin,
    analysisRouteTableRoute_egressOnlyInternetGatewayId,
    analysisRouteTableRoute_natGatewayId,
    analysisRouteTableRoute_networkInterfaceId,
    analysisRouteTableRoute_transitGatewayId,
    analysisRouteTableRoute_gatewayId,
    analysisRouteTableRoute_destinationCidr,
    analysisRouteTableRoute_destinationPrefixListId,

    -- ** AnalysisSecurityGroupRule
    analysisSecurityGroupRule_direction,
    analysisSecurityGroupRule_protocol,
    analysisSecurityGroupRule_portRange,
    analysisSecurityGroupRule_securityGroupId,
    analysisSecurityGroupRule_cidr,
    analysisSecurityGroupRule_prefixListId,

    -- ** AssignedPrivateIpAddress
    assignedPrivateIpAddress_privateIpAddress,

    -- ** AssociatedRole
    associatedRole_certificateS3BucketName,
    associatedRole_certificateS3ObjectKey,
    associatedRole_encryptionKmsKeyId,
    associatedRole_associatedRoleArn,

    -- ** AssociatedTargetNetwork
    associatedTargetNetwork_networkId,
    associatedTargetNetwork_networkType,

    -- ** AssociationStatus
    associationStatus_code,
    associationStatus_message,

    -- ** AthenaIntegration
    athenaIntegration_partitionStartDate,
    athenaIntegration_partitionEndDate,
    athenaIntegration_integrationResultS3DestinationArn,
    athenaIntegration_partitionLoadFrequency,

    -- ** AttributeBooleanValue
    attributeBooleanValue_value,

    -- ** AttributeValue
    attributeValue_value,

    -- ** AuthorizationRule
    authorizationRule_status,
    authorizationRule_accessAll,
    authorizationRule_clientVpnEndpointId,
    authorizationRule_groupId,
    authorizationRule_destinationCidr,
    authorizationRule_description,

    -- ** AvailabilityZone
    availabilityZone_state,
    availabilityZone_parentZoneId,
    availabilityZone_regionName,
    availabilityZone_parentZoneName,
    availabilityZone_networkBorderGroup,
    availabilityZone_zoneId,
    availabilityZone_zoneName,
    availabilityZone_optInStatus,
    availabilityZone_messages,
    availabilityZone_groupName,
    availabilityZone_zoneType,

    -- ** AvailabilityZoneMessage
    availabilityZoneMessage_message,

    -- ** AvailableCapacity
    availableCapacity_availableInstanceCapacity,
    availableCapacity_availableVCpus,

    -- ** BlobAttributeValue
    blobAttributeValue_value,

    -- ** BlockDeviceMapping
    blockDeviceMapping_virtualName,
    blockDeviceMapping_noDevice,
    blockDeviceMapping_ebs,
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
    bundleTaskError_code,
    bundleTaskError_message,

    -- ** ByoipCidr
    byoipCidr_state,
    byoipCidr_cidr,
    byoipCidr_statusMessage,
    byoipCidr_description,

    -- ** CancelCapacityReservationFleetError
    cancelCapacityReservationFleetError_code,
    cancelCapacityReservationFleetError_message,

    -- ** CancelSpotFleetRequestsError
    cancelSpotFleetRequestsError_code,
    cancelSpotFleetRequestsError_message,

    -- ** CancelSpotFleetRequestsErrorItem
    cancelSpotFleetRequestsErrorItem_error,
    cancelSpotFleetRequestsErrorItem_spotFleetRequestId,

    -- ** CancelSpotFleetRequestsSuccessItem
    cancelSpotFleetRequestsSuccessItem_currentSpotFleetRequestState,
    cancelSpotFleetRequestsSuccessItem_spotFleetRequestId,
    cancelSpotFleetRequestsSuccessItem_previousSpotFleetRequestState,

    -- ** CancelledSpotInstanceRequest
    cancelledSpotInstanceRequest_state,
    cancelledSpotInstanceRequest_spotInstanceRequestId,

    -- ** CapacityReservation
    capacityReservation_capacityReservationFleetId,
    capacityReservation_state,
    capacityReservation_availabilityZoneId,
    capacityReservation_createDate,
    capacityReservation_outpostArn,
    capacityReservation_endDate,
    capacityReservation_availableInstanceCount,
    capacityReservation_ephemeralStorage,
    capacityReservation_instancePlatform,
    capacityReservation_instanceMatchCriteria,
    capacityReservation_capacityReservationId,
    capacityReservation_instanceType,
    capacityReservation_ebsOptimized,
    capacityReservation_ownerId,
    capacityReservation_startDate,
    capacityReservation_availabilityZone,
    capacityReservation_tenancy,
    capacityReservation_totalInstanceCount,
    capacityReservation_endDateType,
    capacityReservation_tags,
    capacityReservation_capacityReservationArn,

    -- ** CapacityReservationFleet
    capacityReservationFleet_capacityReservationFleetId,
    capacityReservationFleet_state,
    capacityReservationFleet_totalFulfilledCapacity,
    capacityReservationFleet_endDate,
    capacityReservationFleet_instanceMatchCriteria,
    capacityReservationFleet_totalTargetCapacity,
    capacityReservationFleet_capacityReservationFleetArn,
    capacityReservationFleet_instanceTypeSpecifications,
    capacityReservationFleet_tenancy,
    capacityReservationFleet_allocationStrategy,
    capacityReservationFleet_createTime,
    capacityReservationFleet_tags,

    -- ** CapacityReservationFleetCancellationState
    capacityReservationFleetCancellationState_capacityReservationFleetId,
    capacityReservationFleetCancellationState_currentFleetState,
    capacityReservationFleetCancellationState_previousFleetState,

    -- ** CapacityReservationGroup
    capacityReservationGroup_ownerId,
    capacityReservationGroup_groupArn,

    -- ** CapacityReservationOptions
    capacityReservationOptions_usageStrategy,

    -- ** CapacityReservationOptionsRequest
    capacityReservationOptionsRequest_usageStrategy,

    -- ** CapacityReservationSpecification
    capacityReservationSpecification_capacityReservationTarget,
    capacityReservationSpecification_capacityReservationPreference,

    -- ** CapacityReservationSpecificationResponse
    capacityReservationSpecificationResponse_capacityReservationTarget,
    capacityReservationSpecificationResponse_capacityReservationPreference,

    -- ** CapacityReservationTarget
    capacityReservationTarget_capacityReservationId,
    capacityReservationTarget_capacityReservationResourceGroupArn,

    -- ** CapacityReservationTargetResponse
    capacityReservationTargetResponse_capacityReservationId,
    capacityReservationTargetResponse_capacityReservationResourceGroupArn,

    -- ** CarrierGateway
    carrierGateway_state,
    carrierGateway_vpcId,
    carrierGateway_ownerId,
    carrierGateway_tags,
    carrierGateway_carrierGatewayId,

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
    classicLinkDnsSupport_vpcId,
    classicLinkDnsSupport_classicLinkDnsSupported,

    -- ** ClassicLinkInstance
    classicLinkInstance_instanceId,
    classicLinkInstance_groups,
    classicLinkInstance_vpcId,
    classicLinkInstance_tags,

    -- ** ClassicLoadBalancer
    classicLoadBalancer_name,

    -- ** ClassicLoadBalancersConfig
    classicLoadBalancersConfig_classicLoadBalancers,

    -- ** ClientCertificateRevocationListStatus
    clientCertificateRevocationListStatus_code,
    clientCertificateRevocationListStatus_message,

    -- ** ClientConnectOptions
    clientConnectOptions_enabled,
    clientConnectOptions_lambdaFunctionArn,

    -- ** ClientConnectResponseOptions
    clientConnectResponseOptions_status,
    clientConnectResponseOptions_enabled,
    clientConnectResponseOptions_lambdaFunctionArn,

    -- ** ClientData
    clientData_uploadStart,
    clientData_uploadSize,
    clientData_uploadEnd,
    clientData_comment,

    -- ** ClientVpnAuthentication
    clientVpnAuthentication_activeDirectory,
    clientVpnAuthentication_federatedAuthentication,
    clientVpnAuthentication_mutualAuthentication,
    clientVpnAuthentication_type,

    -- ** ClientVpnAuthenticationRequest
    clientVpnAuthenticationRequest_activeDirectory,
    clientVpnAuthenticationRequest_federatedAuthentication,
    clientVpnAuthenticationRequest_mutualAuthentication,
    clientVpnAuthenticationRequest_type,

    -- ** ClientVpnAuthorizationRuleStatus
    clientVpnAuthorizationRuleStatus_code,
    clientVpnAuthorizationRuleStatus_message,

    -- ** ClientVpnConnection
    clientVpnConnection_ingressPackets,
    clientVpnConnection_status,
    clientVpnConnection_connectionEndTime,
    clientVpnConnection_commonName,
    clientVpnConnection_postureComplianceStatuses,
    clientVpnConnection_connectionEstablishedTime,
    clientVpnConnection_connectionId,
    clientVpnConnection_ingressBytes,
    clientVpnConnection_username,
    clientVpnConnection_egressBytes,
    clientVpnConnection_clientVpnEndpointId,
    clientVpnConnection_clientIp,
    clientVpnConnection_egressPackets,
    clientVpnConnection_timestamp,

    -- ** ClientVpnConnectionStatus
    clientVpnConnectionStatus_code,
    clientVpnConnectionStatus_message,

    -- ** ClientVpnEndpoint
    clientVpnEndpoint_creationTime,
    clientVpnEndpoint_status,
    clientVpnEndpoint_associatedTargetNetworks,
    clientVpnEndpoint_securityGroupIds,
    clientVpnEndpoint_connectionLogOptions,
    clientVpnEndpoint_splitTunnel,
    clientVpnEndpoint_transportProtocol,
    clientVpnEndpoint_vpcId,
    clientVpnEndpoint_vpnPort,
    clientVpnEndpoint_deletionTime,
    clientVpnEndpoint_clientCidrBlock,
    clientVpnEndpoint_dnsServers,
    clientVpnEndpoint_clientVpnEndpointId,
    clientVpnEndpoint_clientConnectOptions,
    clientVpnEndpoint_serverCertificateArn,
    clientVpnEndpoint_authenticationOptions,
    clientVpnEndpoint_selfServicePortalUrl,
    clientVpnEndpoint_description,
    clientVpnEndpoint_dnsName,
    clientVpnEndpoint_vpnProtocol,
    clientVpnEndpoint_tags,

    -- ** ClientVpnEndpointAttributeStatus
    clientVpnEndpointAttributeStatus_code,
    clientVpnEndpointAttributeStatus_message,

    -- ** ClientVpnEndpointStatus
    clientVpnEndpointStatus_code,
    clientVpnEndpointStatus_message,

    -- ** ClientVpnRoute
    clientVpnRoute_status,
    clientVpnRoute_origin,
    clientVpnRoute_clientVpnEndpointId,
    clientVpnRoute_targetSubnet,
    clientVpnRoute_destinationCidr,
    clientVpnRoute_type,
    clientVpnRoute_description,

    -- ** ClientVpnRouteStatus
    clientVpnRouteStatus_code,
    clientVpnRouteStatus_message,

    -- ** CoipAddressUsage
    coipAddressUsage_allocationId,
    coipAddressUsage_awsAccountId,
    coipAddressUsage_coIp,
    coipAddressUsage_awsService,

    -- ** CoipPool
    coipPool_poolId,
    coipPool_localGatewayRouteTableId,
    coipPool_poolCidrs,
    coipPool_tags,
    coipPool_poolArn,

    -- ** ConnectionLogOptions
    connectionLogOptions_enabled,
    connectionLogOptions_cloudwatchLogStream,
    connectionLogOptions_cloudwatchLogGroup,

    -- ** ConnectionLogResponseOptions
    connectionLogResponseOptions_enabled,
    connectionLogResponseOptions_cloudwatchLogStream,
    connectionLogResponseOptions_cloudwatchLogGroup,

    -- ** ConnectionNotification
    connectionNotification_connectionNotificationState,
    connectionNotification_connectionNotificationType,
    connectionNotification_connectionEvents,
    connectionNotification_serviceId,
    connectionNotification_vpcEndpointId,
    connectionNotification_connectionNotificationId,
    connectionNotification_connectionNotificationArn,

    -- ** ConversionTask
    conversionTask_importInstance,
    conversionTask_state,
    conversionTask_statusMessage,
    conversionTask_importVolume,
    conversionTask_conversionTaskId,
    conversionTask_expirationTime,
    conversionTask_tags,

    -- ** CpuOptions
    cpuOptions_coreCount,
    cpuOptions_threadsPerCore,

    -- ** CpuOptionsRequest
    cpuOptionsRequest_coreCount,
    cpuOptionsRequest_threadsPerCore,

    -- ** CreateFleetError
    createFleetError_lifecycle,
    createFleetError_launchTemplateAndOverrides,
    createFleetError_errorCode,
    createFleetError_errorMessage,

    -- ** CreateFleetInstance
    createFleetInstance_platform,
    createFleetInstance_lifecycle,
    createFleetInstance_launchTemplateAndOverrides,
    createFleetInstance_instanceType,
    createFleetInstance_instanceIds,

    -- ** CreateTransitGatewayConnectRequestOptions
    createTransitGatewayConnectRequestOptions_protocol,

    -- ** CreateTransitGatewayMulticastDomainRequestOptions
    createTransitGatewayMulticastDomainRequestOptions_autoAcceptSharedAssociations,
    createTransitGatewayMulticastDomainRequestOptions_igmpv2Support,
    createTransitGatewayMulticastDomainRequestOptions_staticSourcesSupport,

    -- ** CreateTransitGatewayVpcAttachmentRequestOptions
    createTransitGatewayVpcAttachmentRequestOptions_ipv6Support,
    createTransitGatewayVpcAttachmentRequestOptions_applianceModeSupport,
    createTransitGatewayVpcAttachmentRequestOptions_dnsSupport,

    -- ** CreateVolumePermission
    createVolumePermission_group,
    createVolumePermission_userId,

    -- ** CreateVolumePermissionModifications
    createVolumePermissionModifications_remove,
    createVolumePermissionModifications_add,

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
    deleteFleetError_code,
    deleteFleetError_message,

    -- ** DeleteFleetErrorItem
    deleteFleetErrorItem_error,
    deleteFleetErrorItem_fleetId,

    -- ** DeleteFleetSuccessItem
    deleteFleetSuccessItem_currentFleetState,
    deleteFleetSuccessItem_previousFleetState,
    deleteFleetSuccessItem_fleetId,

    -- ** DeleteLaunchTemplateVersionsResponseErrorItem
    deleteLaunchTemplateVersionsResponseErrorItem_launchTemplateName,
    deleteLaunchTemplateVersionsResponseErrorItem_launchTemplateId,
    deleteLaunchTemplateVersionsResponseErrorItem_versionNumber,
    deleteLaunchTemplateVersionsResponseErrorItem_responseError,

    -- ** DeleteLaunchTemplateVersionsResponseSuccessItem
    deleteLaunchTemplateVersionsResponseSuccessItem_launchTemplateName,
    deleteLaunchTemplateVersionsResponseSuccessItem_launchTemplateId,
    deleteLaunchTemplateVersionsResponseSuccessItem_versionNumber,

    -- ** DeleteQueuedReservedInstancesError
    deleteQueuedReservedInstancesError_code,
    deleteQueuedReservedInstancesError_message,

    -- ** DeregisterInstanceTagAttributeRequest
    deregisterInstanceTagAttributeRequest_includeAllTagsOfInstance,
    deregisterInstanceTagAttributeRequest_instanceTagKeys,

    -- ** DescribeFastSnapshotRestoreSuccessItem
    describeFastSnapshotRestoreSuccessItem_disablingTime,
    describeFastSnapshotRestoreSuccessItem_state,
    describeFastSnapshotRestoreSuccessItem_ownerAlias,
    describeFastSnapshotRestoreSuccessItem_disabledTime,
    describeFastSnapshotRestoreSuccessItem_enabledTime,
    describeFastSnapshotRestoreSuccessItem_optimizingTime,
    describeFastSnapshotRestoreSuccessItem_ownerId,
    describeFastSnapshotRestoreSuccessItem_stateTransitionReason,
    describeFastSnapshotRestoreSuccessItem_availabilityZone,
    describeFastSnapshotRestoreSuccessItem_snapshotId,
    describeFastSnapshotRestoreSuccessItem_enablingTime,

    -- ** DescribeFleetError
    describeFleetError_lifecycle,
    describeFleetError_launchTemplateAndOverrides,
    describeFleetError_errorCode,
    describeFleetError_errorMessage,

    -- ** DescribeFleetsInstances
    describeFleetsInstances_platform,
    describeFleetsInstances_lifecycle,
    describeFleetsInstances_launchTemplateAndOverrides,
    describeFleetsInstances_instanceType,
    describeFleetsInstances_instanceIds,

    -- ** DestinationOptionsRequest
    destinationOptionsRequest_perHourPartition,
    destinationOptionsRequest_fileFormat,
    destinationOptionsRequest_hiveCompatiblePartitions,

    -- ** DestinationOptionsResponse
    destinationOptionsResponse_perHourPartition,
    destinationOptionsResponse_fileFormat,
    destinationOptionsResponse_hiveCompatiblePartitions,

    -- ** DhcpConfiguration
    dhcpConfiguration_values,
    dhcpConfiguration_key,

    -- ** DhcpOptions
    dhcpOptions_dhcpConfigurations,
    dhcpOptions_ownerId,
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
    disableFastSnapshotRestoreStateError_code,
    disableFastSnapshotRestoreStateError_message,

    -- ** DisableFastSnapshotRestoreStateErrorItem
    disableFastSnapshotRestoreStateErrorItem_error,
    disableFastSnapshotRestoreStateErrorItem_availabilityZone,

    -- ** DisableFastSnapshotRestoreSuccessItem
    disableFastSnapshotRestoreSuccessItem_disablingTime,
    disableFastSnapshotRestoreSuccessItem_state,
    disableFastSnapshotRestoreSuccessItem_ownerAlias,
    disableFastSnapshotRestoreSuccessItem_disabledTime,
    disableFastSnapshotRestoreSuccessItem_enabledTime,
    disableFastSnapshotRestoreSuccessItem_optimizingTime,
    disableFastSnapshotRestoreSuccessItem_ownerId,
    disableFastSnapshotRestoreSuccessItem_stateTransitionReason,
    disableFastSnapshotRestoreSuccessItem_availabilityZone,
    disableFastSnapshotRestoreSuccessItem_snapshotId,
    disableFastSnapshotRestoreSuccessItem_enablingTime,

    -- ** DiskImage
    diskImage_image,
    diskImage_volume,
    diskImage_description,

    -- ** DiskImageDescription
    diskImageDescription_size,
    diskImageDescription_checksum,
    diskImageDescription_format,
    diskImageDescription_importManifestUrl,

    -- ** DiskImageDetail
    diskImageDetail_bytes,
    diskImageDetail_format,
    diskImageDetail_importManifestUrl,

    -- ** DiskImageVolumeDescription
    diskImageVolumeDescription_size,
    diskImageVolumeDescription_id,

    -- ** DiskInfo
    diskInfo_count,
    diskInfo_sizeInGB,
    diskInfo_type,

    -- ** DnsEntry
    dnsEntry_hostedZoneId,
    dnsEntry_dnsName,

    -- ** DnsServersOptionsModifyStructure
    dnsServersOptionsModifyStructure_enabled,
    dnsServersOptionsModifyStructure_customDnsServers,

    -- ** EbsBlockDevice
    ebsBlockDevice_deleteOnTermination,
    ebsBlockDevice_throughput,
    ebsBlockDevice_volumeSize,
    ebsBlockDevice_iops,
    ebsBlockDevice_outpostArn,
    ebsBlockDevice_encrypted,
    ebsBlockDevice_kmsKeyId,
    ebsBlockDevice_volumeType,
    ebsBlockDevice_snapshotId,

    -- ** EbsInfo
    ebsInfo_ebsOptimizedInfo,
    ebsInfo_encryptionSupport,
    ebsInfo_ebsOptimizedSupport,
    ebsInfo_nvmeSupport,

    -- ** EbsInstanceBlockDevice
    ebsInstanceBlockDevice_status,
    ebsInstanceBlockDevice_deleteOnTermination,
    ebsInstanceBlockDevice_volumeId,
    ebsInstanceBlockDevice_attachTime,

    -- ** EbsInstanceBlockDeviceSpecification
    ebsInstanceBlockDeviceSpecification_deleteOnTermination,
    ebsInstanceBlockDeviceSpecification_volumeId,

    -- ** EbsOptimizedInfo
    ebsOptimizedInfo_maximumIops,
    ebsOptimizedInfo_baselineIops,
    ebsOptimizedInfo_maximumThroughputInMBps,
    ebsOptimizedInfo_maximumBandwidthInMbps,
    ebsOptimizedInfo_baselineBandwidthInMbps,
    ebsOptimizedInfo_baselineThroughputInMBps,

    -- ** EfaInfo
    efaInfo_maximumEfaInterfaces,

    -- ** EgressOnlyInternetGateway
    egressOnlyInternetGateway_egressOnlyInternetGatewayId,
    egressOnlyInternetGateway_attachments,
    egressOnlyInternetGateway_tags,

    -- ** ElasticGpuAssociation
    elasticGpuAssociation_elasticGpuId,
    elasticGpuAssociation_elasticGpuAssociationId,
    elasticGpuAssociation_elasticGpuAssociationTime,
    elasticGpuAssociation_elasticGpuAssociationState,

    -- ** ElasticGpuHealth
    elasticGpuHealth_status,

    -- ** ElasticGpuSpecification
    elasticGpuSpecification_type,

    -- ** ElasticGpuSpecificationResponse
    elasticGpuSpecificationResponse_type,

    -- ** ElasticGpus
    elasticGpus_instanceId,
    elasticGpus_elasticGpuType,
    elasticGpus_elasticGpuId,
    elasticGpus_elasticGpuState,
    elasticGpus_elasticGpuHealth,
    elasticGpus_availabilityZone,
    elasticGpus_tags,

    -- ** ElasticInferenceAccelerator
    elasticInferenceAccelerator_count,
    elasticInferenceAccelerator_type,

    -- ** ElasticInferenceAcceleratorAssociation
    elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorAssociationState,
    elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorAssociationTime,
    elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorArn,
    elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorAssociationId,

    -- ** EnableFastSnapshotRestoreErrorItem
    enableFastSnapshotRestoreErrorItem_fastSnapshotRestoreStateErrors,
    enableFastSnapshotRestoreErrorItem_snapshotId,

    -- ** EnableFastSnapshotRestoreStateError
    enableFastSnapshotRestoreStateError_code,
    enableFastSnapshotRestoreStateError_message,

    -- ** EnableFastSnapshotRestoreStateErrorItem
    enableFastSnapshotRestoreStateErrorItem_error,
    enableFastSnapshotRestoreStateErrorItem_availabilityZone,

    -- ** EnableFastSnapshotRestoreSuccessItem
    enableFastSnapshotRestoreSuccessItem_disablingTime,
    enableFastSnapshotRestoreSuccessItem_state,
    enableFastSnapshotRestoreSuccessItem_ownerAlias,
    enableFastSnapshotRestoreSuccessItem_disabledTime,
    enableFastSnapshotRestoreSuccessItem_enabledTime,
    enableFastSnapshotRestoreSuccessItem_optimizingTime,
    enableFastSnapshotRestoreSuccessItem_ownerId,
    enableFastSnapshotRestoreSuccessItem_stateTransitionReason,
    enableFastSnapshotRestoreSuccessItem_availabilityZone,
    enableFastSnapshotRestoreSuccessItem_snapshotId,
    enableFastSnapshotRestoreSuccessItem_enablingTime,

    -- ** EnclaveOptions
    enclaveOptions_enabled,

    -- ** EnclaveOptionsRequest
    enclaveOptionsRequest_enabled,

    -- ** EventInformation
    eventInformation_instanceId,
    eventInformation_eventDescription,
    eventInformation_eventSubType,

    -- ** Explanation
    explanation_destination,
    explanation_state,
    explanation_cidrs,
    explanation_component,
    explanation_loadBalancerTargetGroups,
    explanation_securityGroups,
    explanation_prefixList,
    explanation_direction,
    explanation_protocols,
    explanation_loadBalancerListenerPort,
    explanation_portRanges,
    explanation_addresses,
    explanation_classicLoadBalancerListener,
    explanation_ingressRouteTable,
    explanation_networkInterface,
    explanation_loadBalancerTarget,
    explanation_subnet,
    explanation_natGateway,
    explanation_address,
    explanation_explanationCode,
    explanation_securityGroup,
    explanation_elasticLoadBalancerListener,
    explanation_loadBalancerTargetGroup,
    explanation_customerGateway,
    explanation_subnetRouteTable,
    explanation_availabilityZones,
    explanation_loadBalancerArn,
    explanation_routeTable,
    explanation_securityGroupRule,
    explanation_packetField,
    explanation_loadBalancerTargetPort,
    explanation_vpc,
    explanation_vpnGateway,
    explanation_sourceVpc,
    explanation_aclRule,
    explanation_internetGateway,
    explanation_missingComponent,
    explanation_acl,
    explanation_vpnConnection,
    explanation_routeTableRoute,
    explanation_vpcEndpoint,
    explanation_vpcPeeringConnection,
    explanation_port,
    explanation_destinationVpc,
    explanation_attachedTo,

    -- ** ExportImageTask
    exportImageTask_status,
    exportImageTask_progress,
    exportImageTask_exportImageTaskId,
    exportImageTask_statusMessage,
    exportImageTask_imageId,
    exportImageTask_description,
    exportImageTask_tags,
    exportImageTask_s3ExportLocation,

    -- ** ExportTask
    exportTask_tags,
    exportTask_description,
    exportTask_exportTaskId,
    exportTask_exportToS3Task,
    exportTask_instanceExportDetails,
    exportTask_state,
    exportTask_statusMessage,

    -- ** ExportTaskS3Location
    exportTaskS3Location_s3Prefix,
    exportTaskS3Location_s3Bucket,

    -- ** ExportTaskS3LocationRequest
    exportTaskS3LocationRequest_s3Prefix,
    exportTaskS3LocationRequest_s3Bucket,

    -- ** ExportToS3Task
    exportToS3Task_s3Key,
    exportToS3Task_containerFormat,
    exportToS3Task_s3Bucket,
    exportToS3Task_diskImageFormat,

    -- ** ExportToS3TaskSpecification
    exportToS3TaskSpecification_containerFormat,
    exportToS3TaskSpecification_s3Prefix,
    exportToS3TaskSpecification_s3Bucket,
    exportToS3TaskSpecification_diskImageFormat,

    -- ** FailedCapacityReservationFleetCancellationResult
    failedCapacityReservationFleetCancellationResult_capacityReservationFleetId,
    failedCapacityReservationFleetCancellationResult_cancelCapacityReservationFleetError,

    -- ** FailedQueuedPurchaseDeletion
    failedQueuedPurchaseDeletion_error,
    failedQueuedPurchaseDeletion_reservedInstancesId,

    -- ** FederatedAuthentication
    federatedAuthentication_samlProviderArn,
    federatedAuthentication_selfServiceSamlProviderArn,

    -- ** FederatedAuthenticationRequest
    federatedAuthenticationRequest_sAMLProviderArn,
    federatedAuthenticationRequest_selfServiceSAMLProviderArn,

    -- ** Filter
    filter_values,
    filter_name,

    -- ** FleetCapacityReservation
    fleetCapacityReservation_priority,
    fleetCapacityReservation_availabilityZoneId,
    fleetCapacityReservation_createDate,
    fleetCapacityReservation_weight,
    fleetCapacityReservation_instancePlatform,
    fleetCapacityReservation_capacityReservationId,
    fleetCapacityReservation_instanceType,
    fleetCapacityReservation_ebsOptimized,
    fleetCapacityReservation_availabilityZone,
    fleetCapacityReservation_fulfilledCapacity,
    fleetCapacityReservation_totalInstanceCount,

    -- ** FleetData
    fleetData_context,
    fleetData_clientToken,
    fleetData_targetCapacitySpecification,
    fleetData_spotOptions,
    fleetData_excessCapacityTerminationPolicy,
    fleetData_onDemandOptions,
    fleetData_fleetState,
    fleetData_launchTemplateConfigs,
    fleetData_validUntil,
    fleetData_terminateInstancesWithExpiration,
    fleetData_instances,
    fleetData_fulfilledCapacity,
    fleetData_type,
    fleetData_validFrom,
    fleetData_replaceUnhealthyInstances,
    fleetData_fulfilledOnDemandCapacity,
    fleetData_fleetId,
    fleetData_errors,
    fleetData_createTime,
    fleetData_tags,
    fleetData_activityStatus,

    -- ** FleetLaunchTemplateConfig
    fleetLaunchTemplateConfig_overrides,
    fleetLaunchTemplateConfig_launchTemplateSpecification,

    -- ** FleetLaunchTemplateConfigRequest
    fleetLaunchTemplateConfigRequest_overrides,
    fleetLaunchTemplateConfigRequest_launchTemplateSpecification,

    -- ** FleetLaunchTemplateOverrides
    fleetLaunchTemplateOverrides_priority,
    fleetLaunchTemplateOverrides_weightedCapacity,
    fleetLaunchTemplateOverrides_subnetId,
    fleetLaunchTemplateOverrides_instanceType,
    fleetLaunchTemplateOverrides_availabilityZone,
    fleetLaunchTemplateOverrides_placement,
    fleetLaunchTemplateOverrides_maxPrice,

    -- ** FleetLaunchTemplateOverridesRequest
    fleetLaunchTemplateOverridesRequest_priority,
    fleetLaunchTemplateOverridesRequest_weightedCapacity,
    fleetLaunchTemplateOverridesRequest_subnetId,
    fleetLaunchTemplateOverridesRequest_instanceType,
    fleetLaunchTemplateOverridesRequest_availabilityZone,
    fleetLaunchTemplateOverridesRequest_placement,
    fleetLaunchTemplateOverridesRequest_maxPrice,

    -- ** FleetLaunchTemplateSpecification
    fleetLaunchTemplateSpecification_launchTemplateName,
    fleetLaunchTemplateSpecification_launchTemplateId,
    fleetLaunchTemplateSpecification_version,

    -- ** FleetLaunchTemplateSpecificationRequest
    fleetLaunchTemplateSpecificationRequest_launchTemplateName,
    fleetLaunchTemplateSpecificationRequest_launchTemplateId,
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
    flowLog_creationTime,
    flowLog_logFormat,
    flowLog_maxAggregationInterval,
    flowLog_resourceId,
    flowLog_flowLogStatus,
    flowLog_trafficType,
    flowLog_logDestination,
    flowLog_deliverLogsStatus,
    flowLog_deliverLogsErrorMessage,
    flowLog_logGroupName,
    flowLog_destinationOptions,
    flowLog_deliverLogsPermissionArn,
    flowLog_logDestinationType,
    flowLog_flowLogId,
    flowLog_tags,

    -- ** FpgaDeviceInfo
    fpgaDeviceInfo_memoryInfo,
    fpgaDeviceInfo_manufacturer,
    fpgaDeviceInfo_count,
    fpgaDeviceInfo_name,

    -- ** FpgaDeviceMemoryInfo
    fpgaDeviceMemoryInfo_sizeInMiB,

    -- ** FpgaImage
    fpgaImage_shellVersion,
    fpgaImage_pciId,
    fpgaImage_state,
    fpgaImage_ownerAlias,
    fpgaImage_fpgaImageId,
    fpgaImage_dataRetentionSupport,
    fpgaImage_ownerId,
    fpgaImage_updateTime,
    fpgaImage_name,
    fpgaImage_productCodes,
    fpgaImage_description,
    fpgaImage_createTime,
    fpgaImage_tags,
    fpgaImage_public,
    fpgaImage_fpgaImageGlobalId,

    -- ** FpgaImageAttribute
    fpgaImageAttribute_fpgaImageId,
    fpgaImageAttribute_name,
    fpgaImageAttribute_productCodes,
    fpgaImageAttribute_description,
    fpgaImageAttribute_loadPermissions,

    -- ** FpgaImageState
    fpgaImageState_code,
    fpgaImageState_message,

    -- ** FpgaInfo
    fpgaInfo_totalFpgaMemoryInMiB,
    fpgaInfo_fpgas,

    -- ** GpuDeviceInfo
    gpuDeviceInfo_memoryInfo,
    gpuDeviceInfo_manufacturer,
    gpuDeviceInfo_count,
    gpuDeviceInfo_name,

    -- ** GpuDeviceMemoryInfo
    gpuDeviceMemoryInfo_sizeInMiB,

    -- ** GpuInfo
    gpuInfo_totalGpuMemoryInMiB,
    gpuInfo_gpus,

    -- ** GroupIdentifier
    groupIdentifier_groupId,
    groupIdentifier_groupName,

    -- ** HibernationOptions
    hibernationOptions_configured,

    -- ** HibernationOptionsRequest
    hibernationOptionsRequest_configured,

    -- ** HistoryRecord
    historyRecord_eventType,
    historyRecord_eventInformation,
    historyRecord_timestamp,

    -- ** HistoryRecordEntry
    historyRecordEntry_eventType,
    historyRecordEntry_eventInformation,
    historyRecordEntry_timestamp,

    -- ** Host
    host_releaseTime,
    host_state,
    host_clientToken,
    host_availabilityZoneId,
    host_hostId,
    host_availableCapacity,
    host_hostReservationId,
    host_allowsMultipleInstanceTypes,
    host_hostProperties,
    host_ownerId,
    host_availabilityZone,
    host_instances,
    host_allocationTime,
    host_memberOfServiceLinkedResourceGroup,
    host_hostRecovery,
    host_autoPlacement,
    host_tags,

    -- ** HostInstance
    hostInstance_instanceId,
    hostInstance_instanceType,
    hostInstance_ownerId,

    -- ** HostOffering
    hostOffering_instanceFamily,
    hostOffering_currencyCode,
    hostOffering_hourlyPrice,
    hostOffering_upfrontPrice,
    hostOffering_offeringId,
    hostOffering_duration,
    hostOffering_paymentOption,

    -- ** HostProperties
    hostProperties_instanceFamily,
    hostProperties_instanceType,
    hostProperties_totalVCpus,
    hostProperties_cores,
    hostProperties_sockets,

    -- ** HostReservation
    hostReservation_state,
    hostReservation_instanceFamily,
    hostReservation_currencyCode,
    hostReservation_hostReservationId,
    hostReservation_start,
    hostReservation_hourlyPrice,
    hostReservation_count,
    hostReservation_upfrontPrice,
    hostReservation_end,
    hostReservation_hostIdSet,
    hostReservation_offeringId,
    hostReservation_duration,
    hostReservation_tags,
    hostReservation_paymentOption,

    -- ** IKEVersionsListValue
    iKEVersionsListValue_value,

    -- ** IKEVersionsRequestListValue
    iKEVersionsRequestListValue_value,

    -- ** IamInstanceProfile
    iamInstanceProfile_arn,
    iamInstanceProfile_id,

    -- ** IamInstanceProfileAssociation
    iamInstanceProfileAssociation_associationId,
    iamInstanceProfileAssociation_instanceId,
    iamInstanceProfileAssociation_state,
    iamInstanceProfileAssociation_iamInstanceProfile,
    iamInstanceProfileAssociation_timestamp,

    -- ** IamInstanceProfileSpecification
    iamInstanceProfileSpecification_arn,
    iamInstanceProfileSpecification_name,

    -- ** IcmpTypeCode
    icmpTypeCode_code,
    icmpTypeCode_type,

    -- ** IdFormat
    idFormat_useLongIds,
    idFormat_deadline,
    idFormat_resource,

    -- ** Image
    image_platform,
    image_platformDetails,
    image_deprecationTime,
    image_enaSupport,
    image_imageOwnerAlias,
    image_usageOperation,
    image_ramdiskId,
    image_kernelId,
    image_rootDeviceName,
    image_sriovNetSupport,
    image_name,
    image_bootMode,
    image_creationDate,
    image_productCodes,
    image_stateReason,
    image_description,
    image_blockDeviceMappings,
    image_tags,
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
    imageDiskContainer_url,
    imageDiskContainer_deviceName,
    imageDiskContainer_userBucket,
    imageDiskContainer_description,
    imageDiskContainer_snapshotId,

    -- ** ImportImageLicenseConfigurationRequest
    importImageLicenseConfigurationRequest_licenseConfigurationArn,

    -- ** ImportImageLicenseConfigurationResponse
    importImageLicenseConfigurationResponse_licenseConfigurationArn,

    -- ** ImportImageTask
    importImageTask_status,
    importImageTask_hypervisor,
    importImageTask_platform,
    importImageTask_progress,
    importImageTask_licenseSpecifications,
    importImageTask_usageOperation,
    importImageTask_licenseType,
    importImageTask_snapshotDetails,
    importImageTask_encrypted,
    importImageTask_kmsKeyId,
    importImageTask_statusMessage,
    importImageTask_imageId,
    importImageTask_bootMode,
    importImageTask_importTaskId,
    importImageTask_architecture,
    importImageTask_description,
    importImageTask_tags,

    -- ** ImportInstanceLaunchSpecification
    importInstanceLaunchSpecification_additionalInfo,
    importInstanceLaunchSpecification_groupNames,
    importInstanceLaunchSpecification_subnetId,
    importInstanceLaunchSpecification_instanceType,
    importInstanceLaunchSpecification_groupIds,
    importInstanceLaunchSpecification_userData,
    importInstanceLaunchSpecification_monitoring,
    importInstanceLaunchSpecification_privateIpAddress,
    importInstanceLaunchSpecification_instanceInitiatedShutdownBehavior,
    importInstanceLaunchSpecification_architecture,
    importInstanceLaunchSpecification_placement,

    -- ** ImportInstanceTaskDetails
    importInstanceTaskDetails_instanceId,
    importInstanceTaskDetails_platform,
    importInstanceTaskDetails_volumes,
    importInstanceTaskDetails_description,

    -- ** ImportInstanceVolumeDetailItem
    importInstanceVolumeDetailItem_status,
    importInstanceVolumeDetailItem_bytesConverted,
    importInstanceVolumeDetailItem_image,
    importInstanceVolumeDetailItem_volume,
    importInstanceVolumeDetailItem_availabilityZone,
    importInstanceVolumeDetailItem_statusMessage,
    importInstanceVolumeDetailItem_description,

    -- ** ImportSnapshotTask
    importSnapshotTask_snapshotTaskDetail,
    importSnapshotTask_importTaskId,
    importSnapshotTask_description,
    importSnapshotTask_tags,

    -- ** ImportVolumeTaskDetails
    importVolumeTaskDetails_bytesConverted,
    importVolumeTaskDetails_image,
    importVolumeTaskDetails_volume,
    importVolumeTaskDetails_availabilityZone,
    importVolumeTaskDetails_description,

    -- ** InferenceAcceleratorInfo
    inferenceAcceleratorInfo_accelerators,

    -- ** InferenceDeviceInfo
    inferenceDeviceInfo_manufacturer,
    inferenceDeviceInfo_count,
    inferenceDeviceInfo_name,

    -- ** Instance
    instance_publicDnsName,
    instance_platform,
    instance_securityGroups,
    instance_clientToken,
    instance_platformDetails,
    instance_enaSupport,
    instance_sourceDestCheck,
    instance_elasticGpuAssociations,
    instance_vpcId,
    instance_keyName,
    instance_networkInterfaces,
    instance_usageOperation,
    instance_outpostArn,
    instance_enclaveOptions,
    instance_ramdiskId,
    instance_cpuOptions,
    instance_subnetId,
    instance_kernelId,
    instance_rootDeviceName,
    instance_capacityReservationId,
    instance_capacityReservationSpecification,
    instance_sriovNetSupport,
    instance_ebsOptimized,
    instance_usageOperationUpdateTime,
    instance_stateTransitionReason,
    instance_hibernationOptions,
    instance_instanceLifecycle,
    instance_iamInstanceProfile,
    instance_privateIpAddress,
    instance_metadataOptions,
    instance_bootMode,
    instance_productCodes,
    instance_spotInstanceRequestId,
    instance_licenses,
    instance_elasticInferenceAcceleratorAssociations,
    instance_privateDnsName,
    instance_stateReason,
    instance_blockDeviceMappings,
    instance_publicIpAddress,
    instance_tags,
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
    instanceBlockDeviceMappingSpecification_virtualName,
    instanceBlockDeviceMappingSpecification_noDevice,
    instanceBlockDeviceMappingSpecification_ebs,
    instanceBlockDeviceMappingSpecification_deviceName,

    -- ** InstanceCapacity
    instanceCapacity_availableCapacity,
    instanceCapacity_instanceType,
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

    -- ** InstanceEventWindow
    instanceEventWindow_state,
    instanceEventWindow_associationTarget,
    instanceEventWindow_instanceEventWindowId,
    instanceEventWindow_name,
    instanceEventWindow_cronExpression,
    instanceEventWindow_tags,
    instanceEventWindow_timeRanges,

    -- ** InstanceEventWindowAssociationRequest
    instanceEventWindowAssociationRequest_instanceTags,
    instanceEventWindowAssociationRequest_instanceIds,
    instanceEventWindowAssociationRequest_dedicatedHostIds,

    -- ** InstanceEventWindowAssociationTarget
    instanceEventWindowAssociationTarget_instanceIds,
    instanceEventWindowAssociationTarget_dedicatedHostIds,
    instanceEventWindowAssociationTarget_tags,

    -- ** InstanceEventWindowDisassociationRequest
    instanceEventWindowDisassociationRequest_instanceTags,
    instanceEventWindowDisassociationRequest_instanceIds,
    instanceEventWindowDisassociationRequest_dedicatedHostIds,

    -- ** InstanceEventWindowStateChange
    instanceEventWindowStateChange_state,
    instanceEventWindowStateChange_instanceEventWindowId,

    -- ** InstanceEventWindowTimeRange
    instanceEventWindowTimeRange_endHour,
    instanceEventWindowTimeRange_endWeekDay,
    instanceEventWindowTimeRange_startWeekDay,
    instanceEventWindowTimeRange_startHour,

    -- ** InstanceEventWindowTimeRangeRequest
    instanceEventWindowTimeRangeRequest_endHour,
    instanceEventWindowTimeRangeRequest_endWeekDay,
    instanceEventWindowTimeRangeRequest_startWeekDay,
    instanceEventWindowTimeRangeRequest_startHour,

    -- ** InstanceExportDetails
    instanceExportDetails_targetEnvironment,
    instanceExportDetails_instanceId,

    -- ** InstanceFamilyCreditSpecification
    instanceFamilyCreditSpecification_instanceFamily,
    instanceFamilyCreditSpecification_cpuCredits,

    -- ** InstanceIpv4Prefix
    instanceIpv4Prefix_ipv4Prefix,

    -- ** InstanceIpv6Address
    instanceIpv6Address_ipv6Address,

    -- ** InstanceIpv6AddressRequest
    instanceIpv6AddressRequest_ipv6Address,

    -- ** InstanceIpv6Prefix
    instanceIpv6Prefix_ipv6Prefix,

    -- ** InstanceMarketOptionsRequest
    instanceMarketOptionsRequest_marketType,
    instanceMarketOptionsRequest_spotOptions,

    -- ** InstanceMetadataOptionsRequest
    instanceMetadataOptionsRequest_httpProtocolIpv6,
    instanceMetadataOptionsRequest_httpEndpoint,
    instanceMetadataOptionsRequest_httpPutResponseHopLimit,
    instanceMetadataOptionsRequest_httpTokens,

    -- ** InstanceMetadataOptionsResponse
    instanceMetadataOptionsResponse_state,
    instanceMetadataOptionsResponse_httpProtocolIpv6,
    instanceMetadataOptionsResponse_httpEndpoint,
    instanceMetadataOptionsResponse_httpPutResponseHopLimit,
    instanceMetadataOptionsResponse_httpTokens,

    -- ** InstanceMonitoring
    instanceMonitoring_instanceId,
    instanceMonitoring_monitoring,

    -- ** InstanceNetworkInterface
    instanceNetworkInterface_groups,
    instanceNetworkInterface_status,
    instanceNetworkInterface_privateIpAddresses,
    instanceNetworkInterface_ipv4Prefixes,
    instanceNetworkInterface_sourceDestCheck,
    instanceNetworkInterface_interfaceType,
    instanceNetworkInterface_vpcId,
    instanceNetworkInterface_networkInterfaceId,
    instanceNetworkInterface_subnetId,
    instanceNetworkInterface_macAddress,
    instanceNetworkInterface_attachment,
    instanceNetworkInterface_ownerId,
    instanceNetworkInterface_ipv6Prefixes,
    instanceNetworkInterface_privateIpAddress,
    instanceNetworkInterface_privateDnsName,
    instanceNetworkInterface_description,
    instanceNetworkInterface_association,
    instanceNetworkInterface_ipv6Addresses,

    -- ** InstanceNetworkInterfaceAssociation
    instanceNetworkInterfaceAssociation_publicDnsName,
    instanceNetworkInterfaceAssociation_carrierIp,
    instanceNetworkInterfaceAssociation_ipOwnerId,
    instanceNetworkInterfaceAssociation_publicIp,

    -- ** InstanceNetworkInterfaceAttachment
    instanceNetworkInterfaceAttachment_status,
    instanceNetworkInterfaceAttachment_deleteOnTermination,
    instanceNetworkInterfaceAttachment_attachmentId,
    instanceNetworkInterfaceAttachment_networkCardIndex,
    instanceNetworkInterfaceAttachment_attachTime,
    instanceNetworkInterfaceAttachment_deviceIndex,

    -- ** InstanceNetworkInterfaceSpecification
    instanceNetworkInterfaceSpecification_groups,
    instanceNetworkInterfaceSpecification_privateIpAddresses,
    instanceNetworkInterfaceSpecification_deleteOnTermination,
    instanceNetworkInterfaceSpecification_associateCarrierIpAddress,
    instanceNetworkInterfaceSpecification_associatePublicIpAddress,
    instanceNetworkInterfaceSpecification_ipv4Prefixes,
    instanceNetworkInterfaceSpecification_interfaceType,
    instanceNetworkInterfaceSpecification_ipv4PrefixCount,
    instanceNetworkInterfaceSpecification_networkInterfaceId,
    instanceNetworkInterfaceSpecification_subnetId,
    instanceNetworkInterfaceSpecification_ipv6AddressCount,
    instanceNetworkInterfaceSpecification_networkCardIndex,
    instanceNetworkInterfaceSpecification_ipv6Prefixes,
    instanceNetworkInterfaceSpecification_privateIpAddress,
    instanceNetworkInterfaceSpecification_ipv6PrefixCount,
    instanceNetworkInterfaceSpecification_secondaryPrivateIpAddressCount,
    instanceNetworkInterfaceSpecification_description,
    instanceNetworkInterfaceSpecification_deviceIndex,
    instanceNetworkInterfaceSpecification_ipv6Addresses,

    -- ** InstancePrivateIpAddress
    instancePrivateIpAddress_primary,
    instancePrivateIpAddress_privateIpAddress,
    instancePrivateIpAddress_privateDnsName,
    instancePrivateIpAddress_association,

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
    instanceStatus_outpostArn,
    instanceStatus_systemStatus,
    instanceStatus_events,
    instanceStatus_availabilityZone,
    instanceStatus_instanceStatus,
    instanceStatus_instanceState,

    -- ** InstanceStatusDetails
    instanceStatusDetails_status,
    instanceStatusDetails_impairedSince,
    instanceStatusDetails_name,

    -- ** InstanceStatusEvent
    instanceStatusEvent_notBefore,
    instanceStatusEvent_code,
    instanceStatusEvent_instanceEventId,
    instanceStatusEvent_description,
    instanceStatusEvent_notBeforeDeadline,
    instanceStatusEvent_notAfter,

    -- ** InstanceStatusSummary
    instanceStatusSummary_details,
    instanceStatusSummary_status,

    -- ** InstanceStorageInfo
    instanceStorageInfo_totalSizeInGB,
    instanceStorageInfo_encryptionSupport,
    instanceStorageInfo_nvmeSupport,
    instanceStorageInfo_disks,

    -- ** InstanceTagNotificationAttribute
    instanceTagNotificationAttribute_includeAllTagsOfInstance,
    instanceTagNotificationAttribute_instanceTagKeys,

    -- ** InstanceTypeInfo
    instanceTypeInfo_hypervisor,
    instanceTypeInfo_currentGeneration,
    instanceTypeInfo_memoryInfo,
    instanceTypeInfo_placementGroupInfo,
    instanceTypeInfo_supportedBootModes,
    instanceTypeInfo_supportedRootDeviceTypes,
    instanceTypeInfo_supportedUsageClasses,
    instanceTypeInfo_instanceStorageSupported,
    instanceTypeInfo_fpgaInfo,
    instanceTypeInfo_burstablePerformanceSupported,
    instanceTypeInfo_instanceType,
    instanceTypeInfo_gpuInfo,
    instanceTypeInfo_supportedVirtualizationTypes,
    instanceTypeInfo_ebsInfo,
    instanceTypeInfo_autoRecoverySupported,
    instanceTypeInfo_inferenceAcceleratorInfo,
    instanceTypeInfo_bareMetal,
    instanceTypeInfo_networkInfo,
    instanceTypeInfo_processorInfo,
    instanceTypeInfo_freeTierEligible,
    instanceTypeInfo_vCpuInfo,
    instanceTypeInfo_instanceStorageInfo,
    instanceTypeInfo_dedicatedHostsSupported,
    instanceTypeInfo_hibernationSupported,

    -- ** InstanceTypeOffering
    instanceTypeOffering_location,
    instanceTypeOffering_instanceType,
    instanceTypeOffering_locationType,

    -- ** InstanceUsage
    instanceUsage_accountId,
    instanceUsage_usedInstanceCount,

    -- ** IntegrateServices
    integrateServices_athenaIntegrations,

    -- ** InternetGateway
    internetGateway_attachments,
    internetGateway_ownerId,
    internetGateway_tags,
    internetGateway_internetGatewayId,

    -- ** InternetGatewayAttachment
    internetGatewayAttachment_state,
    internetGatewayAttachment_vpcId,

    -- ** IpPermission
    ipPermission_fromPort,
    ipPermission_userIdGroupPairs,
    ipPermission_prefixListIds,
    ipPermission_toPort,
    ipPermission_ipv6Ranges,
    ipPermission_ipRanges,
    ipPermission_ipProtocol,

    -- ** IpRange
    ipRange_description,
    ipRange_cidrIp,

    -- ** Ipv4PrefixSpecification
    ipv4PrefixSpecification_ipv4Prefix,

    -- ** Ipv4PrefixSpecificationRequest
    ipv4PrefixSpecificationRequest_ipv4Prefix,

    -- ** Ipv4PrefixSpecificationResponse
    ipv4PrefixSpecificationResponse_ipv4Prefix,

    -- ** Ipv6CidrAssociation
    ipv6CidrAssociation_associatedResource,
    ipv6CidrAssociation_ipv6Cidr,

    -- ** Ipv6CidrBlock
    ipv6CidrBlock_ipv6CidrBlock,

    -- ** Ipv6Pool
    ipv6Pool_poolCidrBlocks,
    ipv6Pool_poolId,
    ipv6Pool_description,
    ipv6Pool_tags,

    -- ** Ipv6PrefixSpecification
    ipv6PrefixSpecification_ipv6Prefix,

    -- ** Ipv6PrefixSpecificationRequest
    ipv6PrefixSpecificationRequest_ipv6Prefix,

    -- ** Ipv6PrefixSpecificationResponse
    ipv6PrefixSpecificationResponse_ipv6Prefix,

    -- ** Ipv6Range
    ipv6Range_cidrIpv6,
    ipv6Range_description,

    -- ** KeyPairInfo
    keyPairInfo_keyFingerprint,
    keyPairInfo_keyType,
    keyPairInfo_keyName,
    keyPairInfo_keyPairId,
    keyPairInfo_tags,

    -- ** LastError
    lastError_code,
    lastError_message,

    -- ** LaunchPermission
    launchPermission_group,
    launchPermission_userId,

    -- ** LaunchPermissionModifications
    launchPermissionModifications_remove,
    launchPermissionModifications_add,

    -- ** LaunchSpecification
    launchSpecification_securityGroups,
    launchSpecification_keyName,
    launchSpecification_networkInterfaces,
    launchSpecification_ramdiskId,
    launchSpecification_subnetId,
    launchSpecification_kernelId,
    launchSpecification_instanceType,
    launchSpecification_ebsOptimized,
    launchSpecification_userData,
    launchSpecification_monitoring,
    launchSpecification_iamInstanceProfile,
    launchSpecification_imageId,
    launchSpecification_addressingType,
    launchSpecification_blockDeviceMappings,
    launchSpecification_placement,

    -- ** LaunchTemplate
    launchTemplate_launchTemplateName,
    launchTemplate_latestVersionNumber,
    launchTemplate_launchTemplateId,
    launchTemplate_createdBy,
    launchTemplate_defaultVersionNumber,
    launchTemplate_createTime,
    launchTemplate_tags,

    -- ** LaunchTemplateAndOverridesResponse
    launchTemplateAndOverridesResponse_overrides,
    launchTemplateAndOverridesResponse_launchTemplateSpecification,

    -- ** LaunchTemplateBlockDeviceMapping
    launchTemplateBlockDeviceMapping_virtualName,
    launchTemplateBlockDeviceMapping_noDevice,
    launchTemplateBlockDeviceMapping_ebs,
    launchTemplateBlockDeviceMapping_deviceName,

    -- ** LaunchTemplateBlockDeviceMappingRequest
    launchTemplateBlockDeviceMappingRequest_virtualName,
    launchTemplateBlockDeviceMappingRequest_noDevice,
    launchTemplateBlockDeviceMappingRequest_ebs,
    launchTemplateBlockDeviceMappingRequest_deviceName,

    -- ** LaunchTemplateCapacityReservationSpecificationRequest
    launchTemplateCapacityReservationSpecificationRequest_capacityReservationTarget,
    launchTemplateCapacityReservationSpecificationRequest_capacityReservationPreference,

    -- ** LaunchTemplateCapacityReservationSpecificationResponse
    launchTemplateCapacityReservationSpecificationResponse_capacityReservationTarget,
    launchTemplateCapacityReservationSpecificationResponse_capacityReservationPreference,

    -- ** LaunchTemplateConfig
    launchTemplateConfig_overrides,
    launchTemplateConfig_launchTemplateSpecification,

    -- ** LaunchTemplateCpuOptions
    launchTemplateCpuOptions_coreCount,
    launchTemplateCpuOptions_threadsPerCore,

    -- ** LaunchTemplateCpuOptionsRequest
    launchTemplateCpuOptionsRequest_coreCount,
    launchTemplateCpuOptionsRequest_threadsPerCore,

    -- ** LaunchTemplateEbsBlockDevice
    launchTemplateEbsBlockDevice_deleteOnTermination,
    launchTemplateEbsBlockDevice_throughput,
    launchTemplateEbsBlockDevice_volumeSize,
    launchTemplateEbsBlockDevice_iops,
    launchTemplateEbsBlockDevice_encrypted,
    launchTemplateEbsBlockDevice_kmsKeyId,
    launchTemplateEbsBlockDevice_volumeType,
    launchTemplateEbsBlockDevice_snapshotId,

    -- ** LaunchTemplateEbsBlockDeviceRequest
    launchTemplateEbsBlockDeviceRequest_deleteOnTermination,
    launchTemplateEbsBlockDeviceRequest_throughput,
    launchTemplateEbsBlockDeviceRequest_volumeSize,
    launchTemplateEbsBlockDeviceRequest_iops,
    launchTemplateEbsBlockDeviceRequest_encrypted,
    launchTemplateEbsBlockDeviceRequest_kmsKeyId,
    launchTemplateEbsBlockDeviceRequest_volumeType,
    launchTemplateEbsBlockDeviceRequest_snapshotId,

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
    launchTemplateInstanceMetadataOptions_state,
    launchTemplateInstanceMetadataOptions_httpProtocolIpv6,
    launchTemplateInstanceMetadataOptions_httpEndpoint,
    launchTemplateInstanceMetadataOptions_httpPutResponseHopLimit,
    launchTemplateInstanceMetadataOptions_httpTokens,

    -- ** LaunchTemplateInstanceMetadataOptionsRequest
    launchTemplateInstanceMetadataOptionsRequest_httpProtocolIpv6,
    launchTemplateInstanceMetadataOptionsRequest_httpEndpoint,
    launchTemplateInstanceMetadataOptionsRequest_httpPutResponseHopLimit,
    launchTemplateInstanceMetadataOptionsRequest_httpTokens,

    -- ** LaunchTemplateInstanceNetworkInterfaceSpecification
    launchTemplateInstanceNetworkInterfaceSpecification_groups,
    launchTemplateInstanceNetworkInterfaceSpecification_privateIpAddresses,
    launchTemplateInstanceNetworkInterfaceSpecification_deleteOnTermination,
    launchTemplateInstanceNetworkInterfaceSpecification_associateCarrierIpAddress,
    launchTemplateInstanceNetworkInterfaceSpecification_associatePublicIpAddress,
    launchTemplateInstanceNetworkInterfaceSpecification_ipv4Prefixes,
    launchTemplateInstanceNetworkInterfaceSpecification_interfaceType,
    launchTemplateInstanceNetworkInterfaceSpecification_ipv4PrefixCount,
    launchTemplateInstanceNetworkInterfaceSpecification_networkInterfaceId,
    launchTemplateInstanceNetworkInterfaceSpecification_subnetId,
    launchTemplateInstanceNetworkInterfaceSpecification_ipv6AddressCount,
    launchTemplateInstanceNetworkInterfaceSpecification_networkCardIndex,
    launchTemplateInstanceNetworkInterfaceSpecification_ipv6Prefixes,
    launchTemplateInstanceNetworkInterfaceSpecification_privateIpAddress,
    launchTemplateInstanceNetworkInterfaceSpecification_ipv6PrefixCount,
    launchTemplateInstanceNetworkInterfaceSpecification_secondaryPrivateIpAddressCount,
    launchTemplateInstanceNetworkInterfaceSpecification_description,
    launchTemplateInstanceNetworkInterfaceSpecification_deviceIndex,
    launchTemplateInstanceNetworkInterfaceSpecification_ipv6Addresses,

    -- ** LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_groups,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_privateIpAddresses,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_deleteOnTermination,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_associateCarrierIpAddress,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_associatePublicIpAddress,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv4Prefixes,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_interfaceType,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv4PrefixCount,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_networkInterfaceId,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_subnetId,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6AddressCount,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_networkCardIndex,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6Prefixes,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_privateIpAddress,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6PrefixCount,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_secondaryPrivateIpAddressCount,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_description,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_deviceIndex,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6Addresses,

    -- ** LaunchTemplateLicenseConfiguration
    launchTemplateLicenseConfiguration_licenseConfigurationArn,

    -- ** LaunchTemplateLicenseConfigurationRequest
    launchTemplateLicenseConfigurationRequest_licenseConfigurationArn,

    -- ** LaunchTemplateOverrides
    launchTemplateOverrides_priority,
    launchTemplateOverrides_spotPrice,
    launchTemplateOverrides_weightedCapacity,
    launchTemplateOverrides_subnetId,
    launchTemplateOverrides_instanceType,
    launchTemplateOverrides_availabilityZone,

    -- ** LaunchTemplatePlacement
    launchTemplatePlacement_affinity,
    launchTemplatePlacement_hostId,
    launchTemplatePlacement_partitionNumber,
    launchTemplatePlacement_spreadDomain,
    launchTemplatePlacement_availabilityZone,
    launchTemplatePlacement_tenancy,
    launchTemplatePlacement_groupName,
    launchTemplatePlacement_hostResourceGroupArn,

    -- ** LaunchTemplatePlacementRequest
    launchTemplatePlacementRequest_affinity,
    launchTemplatePlacementRequest_hostId,
    launchTemplatePlacementRequest_partitionNumber,
    launchTemplatePlacementRequest_spreadDomain,
    launchTemplatePlacementRequest_availabilityZone,
    launchTemplatePlacementRequest_tenancy,
    launchTemplatePlacementRequest_groupName,
    launchTemplatePlacementRequest_hostResourceGroupArn,

    -- ** LaunchTemplateSpecification
    launchTemplateSpecification_launchTemplateName,
    launchTemplateSpecification_launchTemplateId,
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
    launchTemplateVersion_launchTemplateName,
    launchTemplateVersion_launchTemplateId,
    launchTemplateVersion_createdBy,
    launchTemplateVersion_defaultVersion,
    launchTemplateVersion_versionNumber,
    launchTemplateVersion_versionDescription,
    launchTemplateVersion_launchTemplateData,
    launchTemplateVersion_createTime,

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
    loadPermissionModifications_remove,
    loadPermissionModifications_add,

    -- ** LoadPermissionRequest
    loadPermissionRequest_group,
    loadPermissionRequest_userId,

    -- ** LocalGateway
    localGateway_state,
    localGateway_localGatewayId,
    localGateway_outpostArn,
    localGateway_ownerId,
    localGateway_tags,

    -- ** LocalGatewayRoute
    localGatewayRoute_state,
    localGatewayRoute_localGatewayRouteTableArn,
    localGatewayRoute_ownerId,
    localGatewayRoute_localGatewayRouteTableId,
    localGatewayRoute_type,
    localGatewayRoute_localGatewayVirtualInterfaceGroupId,
    localGatewayRoute_destinationCidrBlock,

    -- ** LocalGatewayRouteTable
    localGatewayRouteTable_state,
    localGatewayRouteTable_localGatewayRouteTableArn,
    localGatewayRouteTable_localGatewayId,
    localGatewayRouteTable_outpostArn,
    localGatewayRouteTable_ownerId,
    localGatewayRouteTable_localGatewayRouteTableId,
    localGatewayRouteTable_tags,

    -- ** LocalGatewayRouteTableVirtualInterfaceGroupAssociation
    localGatewayRouteTableVirtualInterfaceGroupAssociation_state,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableArn,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayId,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_ownerId,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableId,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableVirtualInterfaceGroupAssociationId,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayVirtualInterfaceGroupId,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_tags,

    -- ** LocalGatewayRouteTableVpcAssociation
    localGatewayRouteTableVpcAssociation_state,
    localGatewayRouteTableVpcAssociation_localGatewayRouteTableArn,
    localGatewayRouteTableVpcAssociation_vpcId,
    localGatewayRouteTableVpcAssociation_localGatewayId,
    localGatewayRouteTableVpcAssociation_localGatewayRouteTableVpcAssociationId,
    localGatewayRouteTableVpcAssociation_ownerId,
    localGatewayRouteTableVpcAssociation_localGatewayRouteTableId,
    localGatewayRouteTableVpcAssociation_tags,

    -- ** LocalGatewayVirtualInterface
    localGatewayVirtualInterface_localGatewayVirtualInterfaceId,
    localGatewayVirtualInterface_localBgpAsn,
    localGatewayVirtualInterface_vlan,
    localGatewayVirtualInterface_localGatewayId,
    localGatewayVirtualInterface_localAddress,
    localGatewayVirtualInterface_peerBgpAsn,
    localGatewayVirtualInterface_ownerId,
    localGatewayVirtualInterface_peerAddress,
    localGatewayVirtualInterface_tags,

    -- ** LocalGatewayVirtualInterfaceGroup
    localGatewayVirtualInterfaceGroup_localGatewayId,
    localGatewayVirtualInterfaceGroup_ownerId,
    localGatewayVirtualInterfaceGroup_localGatewayVirtualInterfaceIds,
    localGatewayVirtualInterfaceGroup_localGatewayVirtualInterfaceGroupId,
    localGatewayVirtualInterfaceGroup_tags,

    -- ** ManagedPrefixList
    managedPrefixList_stateMessage,
    managedPrefixList_state,
    managedPrefixList_prefixListArn,
    managedPrefixList_addressFamily,
    managedPrefixList_ownerId,
    managedPrefixList_prefixListId,
    managedPrefixList_version,
    managedPrefixList_prefixListName,
    managedPrefixList_maxEntries,
    managedPrefixList_tags,

    -- ** MemoryInfo
    memoryInfo_sizeInMiB,

    -- ** ModifyTransitGatewayOptions
    modifyTransitGatewayOptions_vpnEcmpSupport,
    modifyTransitGatewayOptions_autoAcceptSharedAttachments,
    modifyTransitGatewayOptions_propagationDefaultRouteTableId,
    modifyTransitGatewayOptions_removeTransitGatewayCidrBlocks,
    modifyTransitGatewayOptions_defaultRouteTableAssociation,
    modifyTransitGatewayOptions_associationDefaultRouteTableId,
    modifyTransitGatewayOptions_defaultRouteTablePropagation,
    modifyTransitGatewayOptions_addTransitGatewayCidrBlocks,
    modifyTransitGatewayOptions_dnsSupport,

    -- ** ModifyTransitGatewayVpcAttachmentRequestOptions
    modifyTransitGatewayVpcAttachmentRequestOptions_ipv6Support,
    modifyTransitGatewayVpcAttachmentRequestOptions_applianceModeSupport,
    modifyTransitGatewayVpcAttachmentRequestOptions_dnsSupport,

    -- ** ModifyVpnTunnelOptionsSpecification
    modifyVpnTunnelOptionsSpecification_replayWindowSize,
    modifyVpnTunnelOptionsSpecification_dPDTimeoutAction,
    modifyVpnTunnelOptionsSpecification_rekeyFuzzPercentage,
    modifyVpnTunnelOptionsSpecification_phase1LifetimeSeconds,
    modifyVpnTunnelOptionsSpecification_iKEVersions,
    modifyVpnTunnelOptionsSpecification_phase2IntegrityAlgorithms,
    modifyVpnTunnelOptionsSpecification_phase2LifetimeSeconds,
    modifyVpnTunnelOptionsSpecification_phase1EncryptionAlgorithms,
    modifyVpnTunnelOptionsSpecification_phase1DHGroupNumbers,
    modifyVpnTunnelOptionsSpecification_phase1IntegrityAlgorithms,
    modifyVpnTunnelOptionsSpecification_rekeyMarginTimeSeconds,
    modifyVpnTunnelOptionsSpecification_dPDTimeoutSeconds,
    modifyVpnTunnelOptionsSpecification_tunnelInsideCidr,
    modifyVpnTunnelOptionsSpecification_startupAction,
    modifyVpnTunnelOptionsSpecification_phase2EncryptionAlgorithms,
    modifyVpnTunnelOptionsSpecification_phase2DHGroupNumbers,
    modifyVpnTunnelOptionsSpecification_preSharedKey,
    modifyVpnTunnelOptionsSpecification_tunnelInsideIpv6Cidr,

    -- ** Monitoring
    monitoring_state,

    -- ** MovingAddressStatus
    movingAddressStatus_moveStatus,
    movingAddressStatus_publicIp,

    -- ** NatGateway
    natGateway_state,
    natGateway_connectivityType,
    natGateway_failureCode,
    natGateway_vpcId,
    natGateway_failureMessage,
    natGateway_natGatewayId,
    natGateway_subnetId,
    natGateway_deleteTime,
    natGateway_provisionedBandwidth,
    natGateway_natGatewayAddresses,
    natGateway_createTime,
    natGateway_tags,

    -- ** NatGatewayAddress
    natGatewayAddress_privateIp,
    natGatewayAddress_allocationId,
    natGatewayAddress_networkInterfaceId,
    natGatewayAddress_publicIp,

    -- ** NetworkAcl
    networkAcl_entries,
    networkAcl_networkAclId,
    networkAcl_vpcId,
    networkAcl_ownerId,
    networkAcl_associations,
    networkAcl_tags,
    networkAcl_isDefault,

    -- ** NetworkAclAssociation
    networkAclAssociation_networkAclId,
    networkAclAssociation_subnetId,
    networkAclAssociation_networkAclAssociationId,

    -- ** NetworkAclEntry
    networkAclEntry_ipv6CidrBlock,
    networkAclEntry_icmpTypeCode,
    networkAclEntry_ruleNumber,
    networkAclEntry_ruleAction,
    networkAclEntry_protocol,
    networkAclEntry_portRange,
    networkAclEntry_cidrBlock,
    networkAclEntry_egress,

    -- ** NetworkCardInfo
    networkCardInfo_maximumNetworkInterfaces,
    networkCardInfo_networkPerformance,
    networkCardInfo_networkCardIndex,

    -- ** NetworkInfo
    networkInfo_efaSupported,
    networkInfo_ipv6Supported,
    networkInfo_enaSupport,
    networkInfo_maximumNetworkInterfaces,
    networkInfo_ipv6AddressesPerInterface,
    networkInfo_networkPerformance,
    networkInfo_maximumNetworkCards,
    networkInfo_networkCards,
    networkInfo_efaInfo,
    networkInfo_defaultNetworkCardIndex,
    networkInfo_ipv4AddressesPerInterface,
    networkInfo_encryptionInTransitSupported,

    -- ** NetworkInsightsAnalysis
    networkInsightsAnalysis_status,
    networkInsightsAnalysis_forwardPathComponents,
    networkInsightsAnalysis_alternatePathHints,
    networkInsightsAnalysis_explanations,
    networkInsightsAnalysis_returnPathComponents,
    networkInsightsAnalysis_networkInsightsPathId,
    networkInsightsAnalysis_filterInArns,
    networkInsightsAnalysis_networkInsightsAnalysisId,
    networkInsightsAnalysis_startDate,
    networkInsightsAnalysis_networkInsightsAnalysisArn,
    networkInsightsAnalysis_statusMessage,
    networkInsightsAnalysis_networkPathFound,
    networkInsightsAnalysis_tags,

    -- ** NetworkInsightsPath
    networkInsightsPath_destination,
    networkInsightsPath_destinationIp,
    networkInsightsPath_networkInsightsPathId,
    networkInsightsPath_protocol,
    networkInsightsPath_createdDate,
    networkInsightsPath_sourceIp,
    networkInsightsPath_source,
    networkInsightsPath_destinationPort,
    networkInsightsPath_networkInsightsPathArn,
    networkInsightsPath_tags,

    -- ** NetworkInterface
    networkInterface_groups,
    networkInterface_status,
    networkInterface_privateIpAddresses,
    networkInterface_ipv4Prefixes,
    networkInterface_sourceDestCheck,
    networkInterface_interfaceType,
    networkInterface_vpcId,
    networkInterface_tagSet,
    networkInterface_requesterManaged,
    networkInterface_outpostArn,
    networkInterface_networkInterfaceId,
    networkInterface_subnetId,
    networkInterface_macAddress,
    networkInterface_attachment,
    networkInterface_ownerId,
    networkInterface_ipv6Prefixes,
    networkInterface_availabilityZone,
    networkInterface_privateIpAddress,
    networkInterface_privateDnsName,
    networkInterface_requesterId,
    networkInterface_description,
    networkInterface_association,
    networkInterface_ipv6Addresses,

    -- ** NetworkInterfaceAssociation
    networkInterfaceAssociation_associationId,
    networkInterfaceAssociation_publicDnsName,
    networkInterfaceAssociation_allocationId,
    networkInterfaceAssociation_carrierIp,
    networkInterfaceAssociation_ipOwnerId,
    networkInterfaceAssociation_customerOwnedIp,
    networkInterfaceAssociation_publicIp,

    -- ** NetworkInterfaceAttachment
    networkInterfaceAttachment_instanceId,
    networkInterfaceAttachment_status,
    networkInterfaceAttachment_deleteOnTermination,
    networkInterfaceAttachment_attachmentId,
    networkInterfaceAttachment_networkCardIndex,
    networkInterfaceAttachment_instanceOwnerId,
    networkInterfaceAttachment_attachTime,
    networkInterfaceAttachment_deviceIndex,

    -- ** NetworkInterfaceAttachmentChanges
    networkInterfaceAttachmentChanges_deleteOnTermination,
    networkInterfaceAttachmentChanges_attachmentId,

    -- ** NetworkInterfaceIpv6Address
    networkInterfaceIpv6Address_ipv6Address,

    -- ** NetworkInterfacePermission
    networkInterfacePermission_permissionState,
    networkInterfacePermission_networkInterfacePermissionId,
    networkInterfacePermission_networkInterfaceId,
    networkInterfacePermission_awsAccountId,
    networkInterfacePermission_awsService,
    networkInterfacePermission_permission,

    -- ** NetworkInterfacePermissionState
    networkInterfacePermissionState_state,
    networkInterfacePermissionState_statusMessage,

    -- ** NetworkInterfacePrivateIpAddress
    networkInterfacePrivateIpAddress_primary,
    networkInterfacePrivateIpAddress_privateIpAddress,
    networkInterfacePrivateIpAddress_privateDnsName,
    networkInterfacePrivateIpAddress_association,

    -- ** NewDhcpConfiguration
    newDhcpConfiguration_values,
    newDhcpConfiguration_key,

    -- ** OnDemandOptions
    onDemandOptions_capacityReservationOptions,
    onDemandOptions_singleAvailabilityZone,
    onDemandOptions_maxTotalPrice,
    onDemandOptions_minTargetCapacity,
    onDemandOptions_singleInstanceType,
    onDemandOptions_allocationStrategy,

    -- ** OnDemandOptionsRequest
    onDemandOptionsRequest_capacityReservationOptions,
    onDemandOptionsRequest_singleAvailabilityZone,
    onDemandOptionsRequest_maxTotalPrice,
    onDemandOptionsRequest_minTargetCapacity,
    onDemandOptionsRequest_singleInstanceType,
    onDemandOptionsRequest_allocationStrategy,

    -- ** PathComponent
    pathComponent_sequenceNumber,
    pathComponent_component,
    pathComponent_subnet,
    pathComponent_securityGroupRule,
    pathComponent_inboundHeader,
    pathComponent_vpc,
    pathComponent_sourceVpc,
    pathComponent_aclRule,
    pathComponent_outboundHeader,
    pathComponent_routeTableRoute,
    pathComponent_destinationVpc,

    -- ** PciId
    pciId_subsystemId,
    pciId_deviceId,
    pciId_subsystemVendorId,
    pciId_vendorId,

    -- ** PeeringAttachmentStatus
    peeringAttachmentStatus_code,
    peeringAttachmentStatus_message,

    -- ** PeeringConnectionOptions
    peeringConnectionOptions_allowEgressFromLocalVpcToRemoteClassicLink,
    peeringConnectionOptions_allowEgressFromLocalClassicLinkToRemoteVpc,
    peeringConnectionOptions_allowDnsResolutionFromRemoteVpc,

    -- ** PeeringConnectionOptionsRequest
    peeringConnectionOptionsRequest_allowEgressFromLocalVpcToRemoteClassicLink,
    peeringConnectionOptionsRequest_allowEgressFromLocalClassicLinkToRemoteVpc,
    peeringConnectionOptionsRequest_allowDnsResolutionFromRemoteVpc,

    -- ** PeeringTgwInfo
    peeringTgwInfo_ownerId,
    peeringTgwInfo_transitGatewayId,
    peeringTgwInfo_region,

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
    placement_affinity,
    placement_hostId,
    placement_partitionNumber,
    placement_spreadDomain,
    placement_availabilityZone,
    placement_tenancy,
    placement_groupName,
    placement_hostResourceGroupArn,

    -- ** PlacementGroup
    placementGroup_state,
    placementGroup_strategy,
    placementGroup_groupId,
    placementGroup_groupName,
    placementGroup_partitionCount,
    placementGroup_tags,

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
    prefixList_cidrs,
    prefixList_prefixListId,
    prefixList_prefixListName,

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
    priceSchedule_term,
    priceSchedule_active,
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
    privateDnsNameConfiguration_value,
    privateDnsNameConfiguration_name,
    privateDnsNameConfiguration_type,

    -- ** PrivateIpAddressSpecification
    privateIpAddressSpecification_primary,
    privateIpAddressSpecification_privateIpAddress,

    -- ** ProcessorInfo
    processorInfo_supportedArchitectures,
    processorInfo_sustainedClockSpeedInGhz,

    -- ** ProductCode
    productCode_productCodeType,
    productCode_productCodeId,

    -- ** PropagatingVgw
    propagatingVgw_gatewayId,

    -- ** ProvisionedBandwidth
    provisionedBandwidth_status,
    provisionedBandwidth_requested,
    provisionedBandwidth_provisioned,
    provisionedBandwidth_requestTime,
    provisionedBandwidth_provisionTime,

    -- ** PtrUpdateStatus
    ptrUpdateStatus_status,
    ptrUpdateStatus_value,
    ptrUpdateStatus_reason,

    -- ** PublicIpv4Pool
    publicIpv4Pool_totalAddressCount,
    publicIpv4Pool_networkBorderGroup,
    publicIpv4Pool_totalAvailableAddressCount,
    publicIpv4Pool_poolAddressRanges,
    publicIpv4Pool_poolId,
    publicIpv4Pool_description,
    publicIpv4Pool_tags,

    -- ** PublicIpv4PoolRange
    publicIpv4PoolRange_availableAddressCount,
    publicIpv4PoolRange_lastAddress,
    publicIpv4PoolRange_firstAddress,
    publicIpv4PoolRange_addressCount,

    -- ** Purchase
    purchase_instanceFamily,
    purchase_currencyCode,
    purchase_hostReservationId,
    purchase_hourlyPrice,
    purchase_upfrontPrice,
    purchase_hostIdSet,
    purchase_duration,
    purchase_paymentOption,

    -- ** PurchaseRequest
    purchaseRequest_instanceCount,
    purchaseRequest_purchaseToken,

    -- ** RecurringCharge
    recurringCharge_amount,
    recurringCharge_frequency,

    -- ** ReferencedSecurityGroup
    referencedSecurityGroup_vpcPeeringConnectionId,
    referencedSecurityGroup_vpcId,
    referencedSecurityGroup_userId,
    referencedSecurityGroup_groupId,
    referencedSecurityGroup_peeringStatus,

    -- ** RegionInfo
    regionInfo_regionName,
    regionInfo_optInStatus,
    regionInfo_endpoint,

    -- ** RegisterInstanceTagAttributeRequest
    registerInstanceTagAttributeRequest_includeAllTagsOfInstance,
    registerInstanceTagAttributeRequest_instanceTagKeys,

    -- ** RemovePrefixListEntry
    removePrefixListEntry_cidr,

    -- ** ReplaceRootVolumeTask
    replaceRootVolumeTask_instanceId,
    replaceRootVolumeTask_taskState,
    replaceRootVolumeTask_startTime,
    replaceRootVolumeTask_completeTime,
    replaceRootVolumeTask_replaceRootVolumeTaskId,
    replaceRootVolumeTask_tags,

    -- ** RequestLaunchTemplateData
    requestLaunchTemplateData_securityGroupIds,
    requestLaunchTemplateData_securityGroups,
    requestLaunchTemplateData_elasticInferenceAccelerators,
    requestLaunchTemplateData_instanceMarketOptions,
    requestLaunchTemplateData_licenseSpecifications,
    requestLaunchTemplateData_disableApiTermination,
    requestLaunchTemplateData_keyName,
    requestLaunchTemplateData_networkInterfaces,
    requestLaunchTemplateData_enclaveOptions,
    requestLaunchTemplateData_cpuOptions,
    requestLaunchTemplateData_ramDiskId,
    requestLaunchTemplateData_kernelId,
    requestLaunchTemplateData_elasticGpuSpecifications,
    requestLaunchTemplateData_instanceType,
    requestLaunchTemplateData_capacityReservationSpecification,
    requestLaunchTemplateData_ebsOptimized,
    requestLaunchTemplateData_userData,
    requestLaunchTemplateData_monitoring,
    requestLaunchTemplateData_tagSpecifications,
    requestLaunchTemplateData_hibernationOptions,
    requestLaunchTemplateData_iamInstanceProfile,
    requestLaunchTemplateData_imageId,
    requestLaunchTemplateData_instanceInitiatedShutdownBehavior,
    requestLaunchTemplateData_metadataOptions,
    requestLaunchTemplateData_creditSpecification,
    requestLaunchTemplateData_blockDeviceMappings,
    requestLaunchTemplateData_placement,

    -- ** RequestSpotLaunchSpecification
    requestSpotLaunchSpecification_securityGroupIds,
    requestSpotLaunchSpecification_securityGroups,
    requestSpotLaunchSpecification_keyName,
    requestSpotLaunchSpecification_networkInterfaces,
    requestSpotLaunchSpecification_ramdiskId,
    requestSpotLaunchSpecification_subnetId,
    requestSpotLaunchSpecification_kernelId,
    requestSpotLaunchSpecification_instanceType,
    requestSpotLaunchSpecification_ebsOptimized,
    requestSpotLaunchSpecification_userData,
    requestSpotLaunchSpecification_monitoring,
    requestSpotLaunchSpecification_iamInstanceProfile,
    requestSpotLaunchSpecification_imageId,
    requestSpotLaunchSpecification_addressingType,
    requestSpotLaunchSpecification_blockDeviceMappings,
    requestSpotLaunchSpecification_placement,

    -- ** Reservation
    reservation_groups,
    reservation_instances,
    reservation_requesterId,
    reservation_reservationId,
    reservation_ownerId,

    -- ** ReservationFleetInstanceSpecification
    reservationFleetInstanceSpecification_priority,
    reservationFleetInstanceSpecification_availabilityZoneId,
    reservationFleetInstanceSpecification_weight,
    reservationFleetInstanceSpecification_instancePlatform,
    reservationFleetInstanceSpecification_instanceType,
    reservationFleetInstanceSpecification_ebsOptimized,
    reservationFleetInstanceSpecification_availabilityZone,

    -- ** ReservationValue
    reservationValue_hourlyPrice,
    reservationValue_remainingTotalValue,
    reservationValue_remainingUpfrontValue,

    -- ** ReservedInstanceLimitPrice
    reservedInstanceLimitPrice_amount,
    reservedInstanceLimitPrice_currencyCode,

    -- ** ReservedInstanceReservationValue
    reservedInstanceReservationValue_reservationValue,
    reservedInstanceReservationValue_reservedInstanceId,

    -- ** ReservedInstances
    reservedInstances_state,
    reservedInstances_currencyCode,
    reservedInstances_instanceCount,
    reservedInstances_productDescription,
    reservedInstances_start,
    reservedInstances_instanceType,
    reservedInstances_end,
    reservedInstances_availabilityZone,
    reservedInstances_scope,
    reservedInstances_recurringCharges,
    reservedInstances_offeringType,
    reservedInstances_usagePrice,
    reservedInstances_fixedPrice,
    reservedInstances_reservedInstancesId,
    reservedInstances_instanceTenancy,
    reservedInstances_offeringClass,
    reservedInstances_duration,
    reservedInstances_tags,

    -- ** ReservedInstancesConfiguration
    reservedInstancesConfiguration_platform,
    reservedInstancesConfiguration_instanceCount,
    reservedInstancesConfiguration_instanceType,
    reservedInstancesConfiguration_availabilityZone,
    reservedInstancesConfiguration_scope,

    -- ** ReservedInstancesId
    reservedInstancesId_reservedInstancesId,

    -- ** ReservedInstancesListing
    reservedInstancesListing_status,
    reservedInstancesListing_clientToken,
    reservedInstancesListing_updateDate,
    reservedInstancesListing_createDate,
    reservedInstancesListing_priceSchedules,
    reservedInstancesListing_statusMessage,
    reservedInstancesListing_reservedInstancesId,
    reservedInstancesListing_tags,
    reservedInstancesListing_instanceCounts,
    reservedInstancesListing_reservedInstancesListingId,

    -- ** ReservedInstancesModification
    reservedInstancesModification_modificationResults,
    reservedInstancesModification_status,
    reservedInstancesModification_clientToken,
    reservedInstancesModification_updateDate,
    reservedInstancesModification_createDate,
    reservedInstancesModification_effectiveDate,
    reservedInstancesModification_statusMessage,
    reservedInstancesModification_reservedInstancesModificationId,
    reservedInstancesModification_reservedInstancesIds,

    -- ** ReservedInstancesModificationResult
    reservedInstancesModificationResult_reservedInstancesId,
    reservedInstancesModificationResult_targetConfiguration,

    -- ** ReservedInstancesOffering
    reservedInstancesOffering_marketplace,
    reservedInstancesOffering_currencyCode,
    reservedInstancesOffering_productDescription,
    reservedInstancesOffering_instanceType,
    reservedInstancesOffering_availabilityZone,
    reservedInstancesOffering_pricingDetails,
    reservedInstancesOffering_scope,
    reservedInstancesOffering_recurringCharges,
    reservedInstancesOffering_offeringType,
    reservedInstancesOffering_usagePrice,
    reservedInstancesOffering_fixedPrice,
    reservedInstancesOffering_instanceTenancy,
    reservedInstancesOffering_reservedInstancesOfferingId,
    reservedInstancesOffering_offeringClass,
    reservedInstancesOffering_duration,

    -- ** ResponseError
    responseError_code,
    responseError_message,

    -- ** ResponseLaunchTemplateData
    responseLaunchTemplateData_securityGroupIds,
    responseLaunchTemplateData_securityGroups,
    responseLaunchTemplateData_elasticInferenceAccelerators,
    responseLaunchTemplateData_instanceMarketOptions,
    responseLaunchTemplateData_licenseSpecifications,
    responseLaunchTemplateData_disableApiTermination,
    responseLaunchTemplateData_keyName,
    responseLaunchTemplateData_networkInterfaces,
    responseLaunchTemplateData_enclaveOptions,
    responseLaunchTemplateData_cpuOptions,
    responseLaunchTemplateData_ramDiskId,
    responseLaunchTemplateData_kernelId,
    responseLaunchTemplateData_elasticGpuSpecifications,
    responseLaunchTemplateData_instanceType,
    responseLaunchTemplateData_capacityReservationSpecification,
    responseLaunchTemplateData_ebsOptimized,
    responseLaunchTemplateData_userData,
    responseLaunchTemplateData_monitoring,
    responseLaunchTemplateData_tagSpecifications,
    responseLaunchTemplateData_hibernationOptions,
    responseLaunchTemplateData_iamInstanceProfile,
    responseLaunchTemplateData_imageId,
    responseLaunchTemplateData_instanceInitiatedShutdownBehavior,
    responseLaunchTemplateData_metadataOptions,
    responseLaunchTemplateData_creditSpecification,
    responseLaunchTemplateData_blockDeviceMappings,
    responseLaunchTemplateData_placement,

    -- ** Route
    route_vpcPeeringConnectionId,
    route_instanceId,
    route_origin,
    route_state,
    route_egressOnlyInternetGatewayId,
    route_destinationIpv6CidrBlock,
    route_localGatewayId,
    route_natGatewayId,
    route_networkInterfaceId,
    route_transitGatewayId,
    route_gatewayId,
    route_instanceOwnerId,
    route_destinationPrefixListId,
    route_carrierGatewayId,
    route_destinationCidrBlock,

    -- ** RouteTable
    routeTable_routeTableId,
    routeTable_routes,
    routeTable_vpcId,
    routeTable_propagatingVgws,
    routeTable_ownerId,
    routeTable_associations,
    routeTable_tags,

    -- ** RouteTableAssociation
    routeTableAssociation_routeTableId,
    routeTableAssociation_routeTableAssociationId,
    routeTableAssociation_main,
    routeTableAssociation_subnetId,
    routeTableAssociation_gatewayId,
    routeTableAssociation_associationState,

    -- ** RouteTableAssociationState
    routeTableAssociationState_state,
    routeTableAssociationState_statusMessage,

    -- ** RunInstancesMonitoringEnabled
    runInstancesMonitoringEnabled_enabled,

    -- ** S3ObjectTag
    s3ObjectTag_value,
    s3ObjectTag_key,

    -- ** S3Storage
    s3Storage_prefix,
    s3Storage_uploadPolicy,
    s3Storage_bucket,
    s3Storage_uploadPolicySignature,
    s3Storage_aWSAccessKeyId,

    -- ** ScheduledInstance
    scheduledInstance_previousSlotEndTime,
    scheduledInstance_platform,
    scheduledInstance_termStartDate,
    scheduledInstance_instanceCount,
    scheduledInstance_scheduledInstanceId,
    scheduledInstance_hourlyPrice,
    scheduledInstance_createDate,
    scheduledInstance_slotDurationInHours,
    scheduledInstance_totalScheduledInstanceHours,
    scheduledInstance_instanceType,
    scheduledInstance_recurrence,
    scheduledInstance_availabilityZone,
    scheduledInstance_termEndDate,
    scheduledInstance_nextSlotStartTime,
    scheduledInstance_networkPlatform,

    -- ** ScheduledInstanceAvailability
    scheduledInstanceAvailability_maxTermDurationInDays,
    scheduledInstanceAvailability_platform,
    scheduledInstanceAvailability_purchaseToken,
    scheduledInstanceAvailability_hourlyPrice,
    scheduledInstanceAvailability_availableInstanceCount,
    scheduledInstanceAvailability_slotDurationInHours,
    scheduledInstanceAvailability_totalScheduledInstanceHours,
    scheduledInstanceAvailability_instanceType,
    scheduledInstanceAvailability_recurrence,
    scheduledInstanceAvailability_availabilityZone,
    scheduledInstanceAvailability_minTermDurationInDays,
    scheduledInstanceAvailability_firstSlotStartTime,
    scheduledInstanceAvailability_networkPlatform,

    -- ** ScheduledInstanceRecurrence
    scheduledInstanceRecurrence_frequency,
    scheduledInstanceRecurrence_occurrenceRelativeToEnd,
    scheduledInstanceRecurrence_occurrenceUnit,
    scheduledInstanceRecurrence_interval,
    scheduledInstanceRecurrence_occurrenceDaySet,

    -- ** ScheduledInstanceRecurrenceRequest
    scheduledInstanceRecurrenceRequest_frequency,
    scheduledInstanceRecurrenceRequest_occurrenceRelativeToEnd,
    scheduledInstanceRecurrenceRequest_occurrenceDays,
    scheduledInstanceRecurrenceRequest_occurrenceUnit,
    scheduledInstanceRecurrenceRequest_interval,

    -- ** ScheduledInstancesBlockDeviceMapping
    scheduledInstancesBlockDeviceMapping_virtualName,
    scheduledInstancesBlockDeviceMapping_noDevice,
    scheduledInstancesBlockDeviceMapping_ebs,
    scheduledInstancesBlockDeviceMapping_deviceName,

    -- ** ScheduledInstancesEbs
    scheduledInstancesEbs_deleteOnTermination,
    scheduledInstancesEbs_volumeSize,
    scheduledInstancesEbs_iops,
    scheduledInstancesEbs_encrypted,
    scheduledInstancesEbs_volumeType,
    scheduledInstancesEbs_snapshotId,

    -- ** ScheduledInstancesIamInstanceProfile
    scheduledInstancesIamInstanceProfile_arn,
    scheduledInstancesIamInstanceProfile_name,

    -- ** ScheduledInstancesIpv6Address
    scheduledInstancesIpv6Address_ipv6Address,

    -- ** ScheduledInstancesLaunchSpecification
    scheduledInstancesLaunchSpecification_securityGroupIds,
    scheduledInstancesLaunchSpecification_keyName,
    scheduledInstancesLaunchSpecification_networkInterfaces,
    scheduledInstancesLaunchSpecification_ramdiskId,
    scheduledInstancesLaunchSpecification_subnetId,
    scheduledInstancesLaunchSpecification_kernelId,
    scheduledInstancesLaunchSpecification_instanceType,
    scheduledInstancesLaunchSpecification_ebsOptimized,
    scheduledInstancesLaunchSpecification_userData,
    scheduledInstancesLaunchSpecification_monitoring,
    scheduledInstancesLaunchSpecification_iamInstanceProfile,
    scheduledInstancesLaunchSpecification_blockDeviceMappings,
    scheduledInstancesLaunchSpecification_placement,
    scheduledInstancesLaunchSpecification_imageId,

    -- ** ScheduledInstancesMonitoring
    scheduledInstancesMonitoring_enabled,

    -- ** ScheduledInstancesNetworkInterface
    scheduledInstancesNetworkInterface_groups,
    scheduledInstancesNetworkInterface_deleteOnTermination,
    scheduledInstancesNetworkInterface_associatePublicIpAddress,
    scheduledInstancesNetworkInterface_privateIpAddressConfigs,
    scheduledInstancesNetworkInterface_networkInterfaceId,
    scheduledInstancesNetworkInterface_subnetId,
    scheduledInstancesNetworkInterface_ipv6AddressCount,
    scheduledInstancesNetworkInterface_privateIpAddress,
    scheduledInstancesNetworkInterface_secondaryPrivateIpAddressCount,
    scheduledInstancesNetworkInterface_description,
    scheduledInstancesNetworkInterface_deviceIndex,
    scheduledInstancesNetworkInterface_ipv6Addresses,

    -- ** ScheduledInstancesPlacement
    scheduledInstancesPlacement_availabilityZone,
    scheduledInstancesPlacement_groupName,

    -- ** ScheduledInstancesPrivateIpAddressConfig
    scheduledInstancesPrivateIpAddressConfig_primary,
    scheduledInstancesPrivateIpAddressConfig_privateIpAddress,

    -- ** SecurityGroup
    securityGroup_vpcId,
    securityGroup_ipPermissions,
    securityGroup_ipPermissionsEgress,
    securityGroup_tags,
    securityGroup_ownerId,
    securityGroup_groupId,
    securityGroup_groupName,
    securityGroup_description,

    -- ** SecurityGroupIdentifier
    securityGroupIdentifier_groupId,
    securityGroupIdentifier_groupName,

    -- ** SecurityGroupReference
    securityGroupReference_vpcPeeringConnectionId,
    securityGroupReference_referencingVpcId,
    securityGroupReference_groupId,

    -- ** SecurityGroupRule
    securityGroupRule_cidrIpv4,
    securityGroupRule_isEgress,
    securityGroupRule_fromPort,
    securityGroupRule_securityGroupRuleId,
    securityGroupRule_groupOwnerId,
    securityGroupRule_referencedGroupInfo,
    securityGroupRule_prefixListId,
    securityGroupRule_ipProtocol,
    securityGroupRule_groupId,
    securityGroupRule_toPort,
    securityGroupRule_cidrIpv6,
    securityGroupRule_description,
    securityGroupRule_tags,

    -- ** SecurityGroupRuleDescription
    securityGroupRuleDescription_securityGroupRuleId,
    securityGroupRuleDescription_description,

    -- ** SecurityGroupRuleRequest
    securityGroupRuleRequest_cidrIpv4,
    securityGroupRuleRequest_fromPort,
    securityGroupRuleRequest_referencedGroupId,
    securityGroupRuleRequest_prefixListId,
    securityGroupRuleRequest_ipProtocol,
    securityGroupRuleRequest_toPort,
    securityGroupRuleRequest_cidrIpv6,
    securityGroupRuleRequest_description,

    -- ** SecurityGroupRuleUpdate
    securityGroupRuleUpdate_securityGroupRuleId,
    securityGroupRuleUpdate_securityGroupRule,

    -- ** ServiceConfiguration
    serviceConfiguration_networkLoadBalancerArns,
    serviceConfiguration_baseEndpointDnsNames,
    serviceConfiguration_availabilityZones,
    serviceConfiguration_gatewayLoadBalancerArns,
    serviceConfiguration_managesVpcEndpoints,
    serviceConfiguration_serviceName,
    serviceConfiguration_serviceState,
    serviceConfiguration_serviceType,
    serviceConfiguration_acceptanceRequired,
    serviceConfiguration_serviceId,
    serviceConfiguration_privateDnsName,
    serviceConfiguration_privateDnsNameConfiguration,
    serviceConfiguration_tags,

    -- ** ServiceDetail
    serviceDetail_privateDnsNameVerificationState,
    serviceDetail_vpcEndpointPolicySupported,
    serviceDetail_baseEndpointDnsNames,
    serviceDetail_owner,
    serviceDetail_availabilityZones,
    serviceDetail_managesVpcEndpoints,
    serviceDetail_serviceName,
    serviceDetail_serviceType,
    serviceDetail_acceptanceRequired,
    serviceDetail_privateDnsNames,
    serviceDetail_serviceId,
    serviceDetail_privateDnsName,
    serviceDetail_tags,

    -- ** ServiceTypeDetail
    serviceTypeDetail_serviceType,

    -- ** SlotDateTimeRangeRequest
    slotDateTimeRangeRequest_earliestTime,
    slotDateTimeRangeRequest_latestTime,

    -- ** SlotStartTimeRangeRequest
    slotStartTimeRangeRequest_latestTime,
    slotStartTimeRangeRequest_earliestTime,

    -- ** Snapshot
    snapshot_stateMessage,
    snapshot_ownerAlias,
    snapshot_dataEncryptionKeyId,
    snapshot_outpostArn,
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
    snapshotDetail_status,
    snapshotDetail_progress,
    snapshotDetail_format,
    snapshotDetail_url,
    snapshotDetail_deviceName,
    snapshotDetail_statusMessage,
    snapshotDetail_userBucket,
    snapshotDetail_diskImageSize,
    snapshotDetail_description,
    snapshotDetail_snapshotId,

    -- ** SnapshotDiskContainer
    snapshotDiskContainer_format,
    snapshotDiskContainer_url,
    snapshotDiskContainer_userBucket,
    snapshotDiskContainer_description,

    -- ** SnapshotInfo
    snapshotInfo_state,
    snapshotInfo_progress,
    snapshotInfo_startTime,
    snapshotInfo_volumeSize,
    snapshotInfo_outpostArn,
    snapshotInfo_encrypted,
    snapshotInfo_ownerId,
    snapshotInfo_volumeId,
    snapshotInfo_description,
    snapshotInfo_tags,
    snapshotInfo_snapshotId,

    -- ** SnapshotTaskDetail
    snapshotTaskDetail_status,
    snapshotTaskDetail_progress,
    snapshotTaskDetail_format,
    snapshotTaskDetail_url,
    snapshotTaskDetail_encrypted,
    snapshotTaskDetail_kmsKeyId,
    snapshotTaskDetail_statusMessage,
    snapshotTaskDetail_userBucket,
    snapshotTaskDetail_diskImageSize,
    snapshotTaskDetail_description,
    snapshotTaskDetail_snapshotId,

    -- ** SpotCapacityRebalance
    spotCapacityRebalance_replacementStrategy,

    -- ** SpotDatafeedSubscription
    spotDatafeedSubscription_state,
    spotDatafeedSubscription_prefix,
    spotDatafeedSubscription_bucket,
    spotDatafeedSubscription_ownerId,
    spotDatafeedSubscription_fault,

    -- ** SpotFleetLaunchSpecification
    spotFleetLaunchSpecification_securityGroups,
    spotFleetLaunchSpecification_spotPrice,
    spotFleetLaunchSpecification_weightedCapacity,
    spotFleetLaunchSpecification_keyName,
    spotFleetLaunchSpecification_networkInterfaces,
    spotFleetLaunchSpecification_ramdiskId,
    spotFleetLaunchSpecification_subnetId,
    spotFleetLaunchSpecification_kernelId,
    spotFleetLaunchSpecification_instanceType,
    spotFleetLaunchSpecification_ebsOptimized,
    spotFleetLaunchSpecification_userData,
    spotFleetLaunchSpecification_monitoring,
    spotFleetLaunchSpecification_tagSpecifications,
    spotFleetLaunchSpecification_iamInstanceProfile,
    spotFleetLaunchSpecification_imageId,
    spotFleetLaunchSpecification_addressingType,
    spotFleetLaunchSpecification_blockDeviceMappings,
    spotFleetLaunchSpecification_placement,

    -- ** SpotFleetMonitoring
    spotFleetMonitoring_enabled,

    -- ** SpotFleetRequestConfig
    spotFleetRequestConfig_spotFleetRequestConfig,
    spotFleetRequestConfig_spotFleetRequestId,
    spotFleetRequestConfig_spotFleetRequestState,
    spotFleetRequestConfig_createTime,
    spotFleetRequestConfig_tags,
    spotFleetRequestConfig_activityStatus,

    -- ** SpotFleetRequestConfigData
    spotFleetRequestConfigData_context,
    spotFleetRequestConfigData_clientToken,
    spotFleetRequestConfigData_instanceInterruptionBehavior,
    spotFleetRequestConfigData_onDemandMaxTotalPrice,
    spotFleetRequestConfigData_spotPrice,
    spotFleetRequestConfigData_spotMaintenanceStrategies,
    spotFleetRequestConfigData_loadBalancersConfig,
    spotFleetRequestConfigData_excessCapacityTerminationPolicy,
    spotFleetRequestConfigData_onDemandTargetCapacity,
    spotFleetRequestConfigData_launchTemplateConfigs,
    spotFleetRequestConfigData_tagSpecifications,
    spotFleetRequestConfigData_validUntil,
    spotFleetRequestConfigData_terminateInstancesWithExpiration,
    spotFleetRequestConfigData_onDemandAllocationStrategy,
    spotFleetRequestConfigData_instancePoolsToUseCount,
    spotFleetRequestConfigData_fulfilledCapacity,
    spotFleetRequestConfigData_type,
    spotFleetRequestConfigData_validFrom,
    spotFleetRequestConfigData_replaceUnhealthyInstances,
    spotFleetRequestConfigData_launchSpecifications,
    spotFleetRequestConfigData_onDemandFulfilledCapacity,
    spotFleetRequestConfigData_spotMaxTotalPrice,
    spotFleetRequestConfigData_allocationStrategy,
    spotFleetRequestConfigData_iamFleetRole,
    spotFleetRequestConfigData_targetCapacity,

    -- ** SpotFleetTagSpecification
    spotFleetTagSpecification_resourceType,
    spotFleetTagSpecification_tags,

    -- ** SpotInstanceRequest
    spotInstanceRequest_instanceId,
    spotInstanceRequest_status,
    spotInstanceRequest_state,
    spotInstanceRequest_actualBlockHourlyPrice,
    spotInstanceRequest_blockDurationMinutes,
    spotInstanceRequest_instanceInterruptionBehavior,
    spotInstanceRequest_productDescription,
    spotInstanceRequest_spotPrice,
    spotInstanceRequest_launchSpecification,
    spotInstanceRequest_availabilityZoneGroup,
    spotInstanceRequest_launchedAvailabilityZone,
    spotInstanceRequest_validUntil,
    spotInstanceRequest_launchGroup,
    spotInstanceRequest_fault,
    spotInstanceRequest_spotInstanceRequestId,
    spotInstanceRequest_type,
    spotInstanceRequest_validFrom,
    spotInstanceRequest_createTime,
    spotInstanceRequest_tags,

    -- ** SpotInstanceStateFault
    spotInstanceStateFault_code,
    spotInstanceStateFault_message,

    -- ** SpotInstanceStatus
    spotInstanceStatus_updateTime,
    spotInstanceStatus_code,
    spotInstanceStatus_message,

    -- ** SpotMaintenanceStrategies
    spotMaintenanceStrategies_capacityRebalance,

    -- ** SpotMarketOptions
    spotMarketOptions_blockDurationMinutes,
    spotMarketOptions_instanceInterruptionBehavior,
    spotMarketOptions_validUntil,
    spotMarketOptions_spotInstanceType,
    spotMarketOptions_maxPrice,

    -- ** SpotOptions
    spotOptions_instanceInterruptionBehavior,
    spotOptions_singleAvailabilityZone,
    spotOptions_maxTotalPrice,
    spotOptions_minTargetCapacity,
    spotOptions_instancePoolsToUseCount,
    spotOptions_maintenanceStrategies,
    spotOptions_singleInstanceType,
    spotOptions_allocationStrategy,

    -- ** SpotOptionsRequest
    spotOptionsRequest_instanceInterruptionBehavior,
    spotOptionsRequest_singleAvailabilityZone,
    spotOptionsRequest_maxTotalPrice,
    spotOptionsRequest_minTargetCapacity,
    spotOptionsRequest_instancePoolsToUseCount,
    spotOptionsRequest_maintenanceStrategies,
    spotOptionsRequest_singleInstanceType,
    spotOptionsRequest_allocationStrategy,

    -- ** SpotPlacement
    spotPlacement_availabilityZone,
    spotPlacement_tenancy,
    spotPlacement_groupName,

    -- ** SpotPrice
    spotPrice_productDescription,
    spotPrice_spotPrice,
    spotPrice_instanceType,
    spotPrice_availabilityZone,
    spotPrice_timestamp,

    -- ** StaleIpPermission
    staleIpPermission_fromPort,
    staleIpPermission_userIdGroupPairs,
    staleIpPermission_prefixListIds,
    staleIpPermission_ipProtocol,
    staleIpPermission_toPort,
    staleIpPermission_ipRanges,

    -- ** StaleSecurityGroup
    staleSecurityGroup_vpcId,
    staleSecurityGroup_groupId,
    staleSecurityGroup_groupName,
    staleSecurityGroup_staleIpPermissionsEgress,
    staleSecurityGroup_staleIpPermissions,
    staleSecurityGroup_description,

    -- ** StateReason
    stateReason_code,
    stateReason_message,

    -- ** Storage
    storage_s3,

    -- ** StorageLocation
    storageLocation_bucket,
    storageLocation_key,

    -- ** StoreImageTaskResult
    storeImageTaskResult_s3objectKey,
    storeImageTaskResult_storeTaskState,
    storeImageTaskResult_taskStartTime,
    storeImageTaskResult_bucket,
    storeImageTaskResult_progressPercentage,
    storeImageTaskResult_amiId,
    storeImageTaskResult_storeTaskFailureReason,

    -- ** Subnet
    subnet_ipv6CidrBlockAssociationSet,
    subnet_availabilityZoneId,
    subnet_outpostArn,
    subnet_assignIpv6AddressOnCreation,
    subnet_subnetArn,
    subnet_ownerId,
    subnet_customerOwnedIpv4Pool,
    subnet_mapCustomerOwnedIpOnLaunch,
    subnet_mapPublicIpOnLaunch,
    subnet_defaultForAz,
    subnet_tags,
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
    subnetCidrBlockState_state,
    subnetCidrBlockState_statusMessage,

    -- ** SubnetCidrReservation
    subnetCidrReservation_subnetId,
    subnetCidrReservation_ownerId,
    subnetCidrReservation_cidr,
    subnetCidrReservation_subnetCidrReservationId,
    subnetCidrReservation_reservationType,
    subnetCidrReservation_description,
    subnetCidrReservation_tags,

    -- ** SubnetIpv6CidrBlockAssociation
    subnetIpv6CidrBlockAssociation_associationId,
    subnetIpv6CidrBlockAssociation_ipv6CidrBlock,
    subnetIpv6CidrBlockAssociation_ipv6CidrBlockState,

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
    targetCapacitySpecification_onDemandTargetCapacity,
    targetCapacitySpecification_defaultTargetCapacityType,
    targetCapacitySpecification_totalTargetCapacity,
    targetCapacitySpecification_spotTargetCapacity,

    -- ** TargetCapacitySpecificationRequest
    targetCapacitySpecificationRequest_onDemandTargetCapacity,
    targetCapacitySpecificationRequest_defaultTargetCapacityType,
    targetCapacitySpecificationRequest_spotTargetCapacity,
    targetCapacitySpecificationRequest_totalTargetCapacity,

    -- ** TargetConfiguration
    targetConfiguration_instanceCount,
    targetConfiguration_offeringId,

    -- ** TargetConfigurationRequest
    targetConfigurationRequest_instanceCount,
    targetConfigurationRequest_offeringId,

    -- ** TargetGroup
    targetGroup_arn,

    -- ** TargetGroupsConfig
    targetGroupsConfig_targetGroups,

    -- ** TargetNetwork
    targetNetwork_associationId,
    targetNetwork_status,
    targetNetwork_securityGroups,
    targetNetwork_targetNetworkId,
    targetNetwork_vpcId,
    targetNetwork_clientVpnEndpointId,

    -- ** TargetReservationValue
    targetReservationValue_reservationValue,
    targetReservationValue_targetConfiguration,

    -- ** TerminateConnectionStatus
    terminateConnectionStatus_currentStatus,
    terminateConnectionStatus_connectionId,
    terminateConnectionStatus_previousStatus,

    -- ** TrafficMirrorFilter
    trafficMirrorFilter_trafficMirrorFilterId,
    trafficMirrorFilter_ingressFilterRules,
    trafficMirrorFilter_networkServices,
    trafficMirrorFilter_egressFilterRules,
    trafficMirrorFilter_description,
    trafficMirrorFilter_tags,

    -- ** TrafficMirrorFilterRule
    trafficMirrorFilterRule_ruleNumber,
    trafficMirrorFilterRule_trafficDirection,
    trafficMirrorFilterRule_ruleAction,
    trafficMirrorFilterRule_protocol,
    trafficMirrorFilterRule_trafficMirrorFilterId,
    trafficMirrorFilterRule_trafficMirrorFilterRuleId,
    trafficMirrorFilterRule_destinationPortRange,
    trafficMirrorFilterRule_sourceCidrBlock,
    trafficMirrorFilterRule_sourcePortRange,
    trafficMirrorFilterRule_description,
    trafficMirrorFilterRule_destinationCidrBlock,

    -- ** TrafficMirrorPortRange
    trafficMirrorPortRange_fromPort,
    trafficMirrorPortRange_toPort,

    -- ** TrafficMirrorPortRangeRequest
    trafficMirrorPortRangeRequest_fromPort,
    trafficMirrorPortRangeRequest_toPort,

    -- ** TrafficMirrorSession
    trafficMirrorSession_trafficMirrorTargetId,
    trafficMirrorSession_networkInterfaceId,
    trafficMirrorSession_trafficMirrorFilterId,
    trafficMirrorSession_packetLength,
    trafficMirrorSession_ownerId,
    trafficMirrorSession_trafficMirrorSessionId,
    trafficMirrorSession_virtualNetworkId,
    trafficMirrorSession_sessionNumber,
    trafficMirrorSession_description,
    trafficMirrorSession_tags,

    -- ** TrafficMirrorTarget
    trafficMirrorTarget_trafficMirrorTargetId,
    trafficMirrorTarget_networkInterfaceId,
    trafficMirrorTarget_networkLoadBalancerArn,
    trafficMirrorTarget_ownerId,
    trafficMirrorTarget_type,
    trafficMirrorTarget_description,
    trafficMirrorTarget_tags,

    -- ** TransitGateway
    transitGateway_creationTime,
    transitGateway_state,
    transitGateway_ownerId,
    transitGateway_transitGatewayArn,
    transitGateway_transitGatewayId,
    transitGateway_options,
    transitGateway_description,
    transitGateway_tags,

    -- ** TransitGatewayAssociation
    transitGatewayAssociation_state,
    transitGatewayAssociation_resourceId,
    transitGatewayAssociation_resourceType,
    transitGatewayAssociation_transitGatewayRouteTableId,
    transitGatewayAssociation_transitGatewayAttachmentId,

    -- ** TransitGatewayAttachment
    transitGatewayAttachment_creationTime,
    transitGatewayAttachment_state,
    transitGatewayAttachment_resourceId,
    transitGatewayAttachment_resourceType,
    transitGatewayAttachment_transitGatewayOwnerId,
    transitGatewayAttachment_transitGatewayId,
    transitGatewayAttachment_transitGatewayAttachmentId,
    transitGatewayAttachment_resourceOwnerId,
    transitGatewayAttachment_tags,
    transitGatewayAttachment_association,

    -- ** TransitGatewayAttachmentAssociation
    transitGatewayAttachmentAssociation_state,
    transitGatewayAttachmentAssociation_transitGatewayRouteTableId,

    -- ** TransitGatewayAttachmentBgpConfiguration
    transitGatewayAttachmentBgpConfiguration_transitGatewayAsn,
    transitGatewayAttachmentBgpConfiguration_peerAsn,
    transitGatewayAttachmentBgpConfiguration_transitGatewayAddress,
    transitGatewayAttachmentBgpConfiguration_bgpStatus,
    transitGatewayAttachmentBgpConfiguration_peerAddress,

    -- ** TransitGatewayAttachmentPropagation
    transitGatewayAttachmentPropagation_state,
    transitGatewayAttachmentPropagation_transitGatewayRouteTableId,

    -- ** TransitGatewayConnect
    transitGatewayConnect_creationTime,
    transitGatewayConnect_state,
    transitGatewayConnect_transportTransitGatewayAttachmentId,
    transitGatewayConnect_transitGatewayId,
    transitGatewayConnect_options,
    transitGatewayConnect_transitGatewayAttachmentId,
    transitGatewayConnect_tags,

    -- ** TransitGatewayConnectOptions
    transitGatewayConnectOptions_protocol,

    -- ** TransitGatewayConnectPeer
    transitGatewayConnectPeer_connectPeerConfiguration,
    transitGatewayConnectPeer_creationTime,
    transitGatewayConnectPeer_state,
    transitGatewayConnectPeer_transitGatewayConnectPeerId,
    transitGatewayConnectPeer_transitGatewayAttachmentId,
    transitGatewayConnectPeer_tags,

    -- ** TransitGatewayConnectPeerConfiguration
    transitGatewayConnectPeerConfiguration_protocol,
    transitGatewayConnectPeerConfiguration_transitGatewayAddress,
    transitGatewayConnectPeerConfiguration_peerAddress,
    transitGatewayConnectPeerConfiguration_insideCidrBlocks,
    transitGatewayConnectPeerConfiguration_bgpConfigurations,

    -- ** TransitGatewayConnectRequestBgpOptions
    transitGatewayConnectRequestBgpOptions_peerAsn,

    -- ** TransitGatewayMulticastDeregisteredGroupMembers
    transitGatewayMulticastDeregisteredGroupMembers_deregisteredNetworkInterfaceIds,
    transitGatewayMulticastDeregisteredGroupMembers_transitGatewayMulticastDomainId,
    transitGatewayMulticastDeregisteredGroupMembers_groupIpAddress,

    -- ** TransitGatewayMulticastDeregisteredGroupSources
    transitGatewayMulticastDeregisteredGroupSources_deregisteredNetworkInterfaceIds,
    transitGatewayMulticastDeregisteredGroupSources_transitGatewayMulticastDomainId,
    transitGatewayMulticastDeregisteredGroupSources_groupIpAddress,

    -- ** TransitGatewayMulticastDomain
    transitGatewayMulticastDomain_creationTime,
    transitGatewayMulticastDomain_state,
    transitGatewayMulticastDomain_transitGatewayMulticastDomainId,
    transitGatewayMulticastDomain_transitGatewayMulticastDomainArn,
    transitGatewayMulticastDomain_ownerId,
    transitGatewayMulticastDomain_transitGatewayId,
    transitGatewayMulticastDomain_options,
    transitGatewayMulticastDomain_tags,

    -- ** TransitGatewayMulticastDomainAssociation
    transitGatewayMulticastDomainAssociation_resourceId,
    transitGatewayMulticastDomainAssociation_resourceType,
    transitGatewayMulticastDomainAssociation_subnet,
    transitGatewayMulticastDomainAssociation_transitGatewayAttachmentId,
    transitGatewayMulticastDomainAssociation_resourceOwnerId,

    -- ** TransitGatewayMulticastDomainAssociations
    transitGatewayMulticastDomainAssociations_resourceId,
    transitGatewayMulticastDomainAssociations_resourceType,
    transitGatewayMulticastDomainAssociations_subnets,
    transitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId,
    transitGatewayMulticastDomainAssociations_transitGatewayAttachmentId,
    transitGatewayMulticastDomainAssociations_resourceOwnerId,

    -- ** TransitGatewayMulticastDomainOptions
    transitGatewayMulticastDomainOptions_autoAcceptSharedAssociations,
    transitGatewayMulticastDomainOptions_igmpv2Support,
    transitGatewayMulticastDomainOptions_staticSourcesSupport,

    -- ** TransitGatewayMulticastGroup
    transitGatewayMulticastGroup_resourceId,
    transitGatewayMulticastGroup_resourceType,
    transitGatewayMulticastGroup_sourceType,
    transitGatewayMulticastGroup_memberType,
    transitGatewayMulticastGroup_networkInterfaceId,
    transitGatewayMulticastGroup_subnetId,
    transitGatewayMulticastGroup_groupMember,
    transitGatewayMulticastGroup_groupSource,
    transitGatewayMulticastGroup_groupIpAddress,
    transitGatewayMulticastGroup_transitGatewayAttachmentId,
    transitGatewayMulticastGroup_resourceOwnerId,

    -- ** TransitGatewayMulticastRegisteredGroupMembers
    transitGatewayMulticastRegisteredGroupMembers_transitGatewayMulticastDomainId,
    transitGatewayMulticastRegisteredGroupMembers_registeredNetworkInterfaceIds,
    transitGatewayMulticastRegisteredGroupMembers_groupIpAddress,

    -- ** TransitGatewayMulticastRegisteredGroupSources
    transitGatewayMulticastRegisteredGroupSources_transitGatewayMulticastDomainId,
    transitGatewayMulticastRegisteredGroupSources_registeredNetworkInterfaceIds,
    transitGatewayMulticastRegisteredGroupSources_groupIpAddress,

    -- ** TransitGatewayOptions
    transitGatewayOptions_vpnEcmpSupport,
    transitGatewayOptions_autoAcceptSharedAttachments,
    transitGatewayOptions_propagationDefaultRouteTableId,
    transitGatewayOptions_defaultRouteTableAssociation,
    transitGatewayOptions_associationDefaultRouteTableId,
    transitGatewayOptions_amazonSideAsn,
    transitGatewayOptions_defaultRouteTablePropagation,
    transitGatewayOptions_multicastSupport,
    transitGatewayOptions_dnsSupport,
    transitGatewayOptions_transitGatewayCidrBlocks,

    -- ** TransitGatewayPeeringAttachment
    transitGatewayPeeringAttachment_creationTime,
    transitGatewayPeeringAttachment_requesterTgwInfo,
    transitGatewayPeeringAttachment_status,
    transitGatewayPeeringAttachment_state,
    transitGatewayPeeringAttachment_accepterTgwInfo,
    transitGatewayPeeringAttachment_transitGatewayAttachmentId,
    transitGatewayPeeringAttachment_tags,

    -- ** TransitGatewayPrefixListAttachment
    transitGatewayPrefixListAttachment_resourceId,
    transitGatewayPrefixListAttachment_resourceType,
    transitGatewayPrefixListAttachment_transitGatewayAttachmentId,

    -- ** TransitGatewayPrefixListReference
    transitGatewayPrefixListReference_state,
    transitGatewayPrefixListReference_transitGatewayRouteTableId,
    transitGatewayPrefixListReference_prefixListOwnerId,
    transitGatewayPrefixListReference_blackhole,
    transitGatewayPrefixListReference_prefixListId,
    transitGatewayPrefixListReference_transitGatewayAttachment,

    -- ** TransitGatewayPropagation
    transitGatewayPropagation_state,
    transitGatewayPropagation_resourceId,
    transitGatewayPropagation_resourceType,
    transitGatewayPropagation_transitGatewayRouteTableId,
    transitGatewayPropagation_transitGatewayAttachmentId,

    -- ** TransitGatewayRequestOptions
    transitGatewayRequestOptions_vpnEcmpSupport,
    transitGatewayRequestOptions_autoAcceptSharedAttachments,
    transitGatewayRequestOptions_defaultRouteTableAssociation,
    transitGatewayRequestOptions_amazonSideAsn,
    transitGatewayRequestOptions_defaultRouteTablePropagation,
    transitGatewayRequestOptions_multicastSupport,
    transitGatewayRequestOptions_dnsSupport,
    transitGatewayRequestOptions_transitGatewayCidrBlocks,

    -- ** TransitGatewayRoute
    transitGatewayRoute_state,
    transitGatewayRoute_prefixListId,
    transitGatewayRoute_transitGatewayAttachments,
    transitGatewayRoute_type,
    transitGatewayRoute_destinationCidrBlock,

    -- ** TransitGatewayRouteAttachment
    transitGatewayRouteAttachment_resourceId,
    transitGatewayRouteAttachment_resourceType,
    transitGatewayRouteAttachment_transitGatewayAttachmentId,

    -- ** TransitGatewayRouteTable
    transitGatewayRouteTable_creationTime,
    transitGatewayRouteTable_state,
    transitGatewayRouteTable_defaultPropagationRouteTable,
    transitGatewayRouteTable_transitGatewayRouteTableId,
    transitGatewayRouteTable_transitGatewayId,
    transitGatewayRouteTable_defaultAssociationRouteTable,
    transitGatewayRouteTable_tags,

    -- ** TransitGatewayRouteTableAssociation
    transitGatewayRouteTableAssociation_state,
    transitGatewayRouteTableAssociation_resourceId,
    transitGatewayRouteTableAssociation_resourceType,
    transitGatewayRouteTableAssociation_transitGatewayAttachmentId,

    -- ** TransitGatewayRouteTablePropagation
    transitGatewayRouteTablePropagation_state,
    transitGatewayRouteTablePropagation_resourceId,
    transitGatewayRouteTablePropagation_resourceType,
    transitGatewayRouteTablePropagation_transitGatewayAttachmentId,

    -- ** TransitGatewayVpcAttachment
    transitGatewayVpcAttachment_creationTime,
    transitGatewayVpcAttachment_state,
    transitGatewayVpcAttachment_subnetIds,
    transitGatewayVpcAttachment_vpcId,
    transitGatewayVpcAttachment_transitGatewayId,
    transitGatewayVpcAttachment_options,
    transitGatewayVpcAttachment_transitGatewayAttachmentId,
    transitGatewayVpcAttachment_tags,
    transitGatewayVpcAttachment_vpcOwnerId,

    -- ** TransitGatewayVpcAttachmentOptions
    transitGatewayVpcAttachmentOptions_ipv6Support,
    transitGatewayVpcAttachmentOptions_applianceModeSupport,
    transitGatewayVpcAttachmentOptions_dnsSupport,

    -- ** TrunkInterfaceAssociation
    trunkInterfaceAssociation_associationId,
    trunkInterfaceAssociation_interfaceProtocol,
    trunkInterfaceAssociation_branchInterfaceId,
    trunkInterfaceAssociation_greKey,
    trunkInterfaceAssociation_vlanId,
    trunkInterfaceAssociation_trunkInterfaceId,
    trunkInterfaceAssociation_tags,

    -- ** TunnelOption
    tunnelOption_outsideIpAddress,
    tunnelOption_replayWindowSize,
    tunnelOption_dpdTimeoutAction,
    tunnelOption_rekeyFuzzPercentage,
    tunnelOption_phase1LifetimeSeconds,
    tunnelOption_ikeVersions,
    tunnelOption_phase2IntegrityAlgorithms,
    tunnelOption_phase2LifetimeSeconds,
    tunnelOption_phase1EncryptionAlgorithms,
    tunnelOption_phase1DHGroupNumbers,
    tunnelOption_phase1IntegrityAlgorithms,
    tunnelOption_rekeyMarginTimeSeconds,
    tunnelOption_dpdTimeoutSeconds,
    tunnelOption_tunnelInsideCidr,
    tunnelOption_startupAction,
    tunnelOption_phase2EncryptionAlgorithms,
    tunnelOption_phase2DHGroupNumbers,
    tunnelOption_preSharedKey,
    tunnelOption_tunnelInsideIpv6Cidr,

    -- ** UnsuccessfulInstanceCreditSpecificationItem
    unsuccessfulInstanceCreditSpecificationItem_instanceId,
    unsuccessfulInstanceCreditSpecificationItem_error,

    -- ** UnsuccessfulInstanceCreditSpecificationItemError
    unsuccessfulInstanceCreditSpecificationItemError_code,
    unsuccessfulInstanceCreditSpecificationItemError_message,

    -- ** UnsuccessfulItem
    unsuccessfulItem_resourceId,
    unsuccessfulItem_error,

    -- ** UnsuccessfulItemError
    unsuccessfulItemError_code,
    unsuccessfulItemError_message,

    -- ** UserBucket
    userBucket_s3Key,
    userBucket_s3Bucket,

    -- ** UserBucketDetails
    userBucketDetails_s3Key,
    userBucketDetails_s3Bucket,

    -- ** UserData
    userData_data,

    -- ** UserIdGroupPair
    userIdGroupPair_vpcPeeringConnectionId,
    userIdGroupPair_vpcId,
    userIdGroupPair_userId,
    userIdGroupPair_groupId,
    userIdGroupPair_groupName,
    userIdGroupPair_description,
    userIdGroupPair_peeringStatus,

    -- ** VCpuInfo
    vCpuInfo_validThreadsPerCore,
    vCpuInfo_defaultThreadsPerCore,
    vCpuInfo_defaultVCpus,
    vCpuInfo_defaultCores,
    vCpuInfo_validCores,

    -- ** ValidationError
    validationError_code,
    validationError_message,

    -- ** ValidationWarning
    validationWarning_errors,

    -- ** VgwTelemetry
    vgwTelemetry_status,
    vgwTelemetry_outsideIpAddress,
    vgwTelemetry_certificateArn,
    vgwTelemetry_lastStatusChange,
    vgwTelemetry_acceptedRouteCount,
    vgwTelemetry_statusMessage,

    -- ** Volume
    volume_fastRestored,
    volume_multiAttachEnabled,
    volume_attachments,
    volume_throughput,
    volume_iops,
    volume_outpostArn,
    volume_kmsKeyId,
    volume_tags,
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
    volumeAttachment_deleteOnTermination,
    volumeAttachment_state,
    volumeAttachment_device,
    volumeAttachment_volumeId,
    volumeAttachment_attachTime,

    -- ** VolumeDetail
    volumeDetail_size,

    -- ** VolumeModification
    volumeModification_progress,
    volumeModification_startTime,
    volumeModification_targetMultiAttachEnabled,
    volumeModification_originalMultiAttachEnabled,
    volumeModification_modificationState,
    volumeModification_targetVolumeType,
    volumeModification_originalVolumeType,
    volumeModification_targetSize,
    volumeModification_targetIops,
    volumeModification_originalSize,
    volumeModification_originalIops,
    volumeModification_statusMessage,
    volumeModification_endTime,
    volumeModification_volumeId,
    volumeModification_originalThroughput,
    volumeModification_targetThroughput,

    -- ** VolumeStatusAction
    volumeStatusAction_eventType,
    volumeStatusAction_code,
    volumeStatusAction_description,
    volumeStatusAction_eventId,

    -- ** VolumeStatusAttachmentStatus
    volumeStatusAttachmentStatus_instanceId,
    volumeStatusAttachmentStatus_ioPerformance,

    -- ** VolumeStatusDetails
    volumeStatusDetails_status,
    volumeStatusDetails_name,

    -- ** VolumeStatusEvent
    volumeStatusEvent_instanceId,
    volumeStatusEvent_notBefore,
    volumeStatusEvent_eventType,
    volumeStatusEvent_description,
    volumeStatusEvent_notAfter,
    volumeStatusEvent_eventId,

    -- ** VolumeStatusInfo
    volumeStatusInfo_status,
    volumeStatusInfo_details,

    -- ** VolumeStatusItem
    volumeStatusItem_volumeStatus,
    volumeStatusItem_actions,
    volumeStatusItem_outpostArn,
    volumeStatusItem_events,
    volumeStatusItem_availabilityZone,
    volumeStatusItem_volumeId,
    volumeStatusItem_attachmentStatuses,

    -- ** Vpc
    vpc_ipv6CidrBlockAssociationSet,
    vpc_cidrBlockAssociationSet,
    vpc_ownerId,
    vpc_tags,
    vpc_isDefault,
    vpc_cidrBlock,
    vpc_dhcpOptionsId,
    vpc_instanceTenancy,
    vpc_state,
    vpc_vpcId,

    -- ** VpcAttachment
    vpcAttachment_state,
    vpcAttachment_vpcId,

    -- ** VpcCidrBlockAssociation
    vpcCidrBlockAssociation_associationId,
    vpcCidrBlockAssociation_cidrBlockState,
    vpcCidrBlockAssociation_cidrBlock,

    -- ** VpcCidrBlockState
    vpcCidrBlockState_state,
    vpcCidrBlockState_statusMessage,

    -- ** VpcClassicLink
    vpcClassicLink_vpcId,
    vpcClassicLink_tags,
    vpcClassicLink_classicLinkEnabled,

    -- ** VpcEndpoint
    vpcEndpoint_groups,
    vpcEndpoint_state,
    vpcEndpoint_policyDocument,
    vpcEndpoint_subnetIds,
    vpcEndpoint_networkInterfaceIds,
    vpcEndpoint_vpcId,
    vpcEndpoint_requesterManaged,
    vpcEndpoint_dnsEntries,
    vpcEndpoint_vpcEndpointType,
    vpcEndpoint_privateDnsEnabled,
    vpcEndpoint_ownerId,
    vpcEndpoint_creationTimestamp,
    vpcEndpoint_serviceName,
    vpcEndpoint_lastError,
    vpcEndpoint_vpcEndpointId,
    vpcEndpoint_tags,
    vpcEndpoint_routeTableIds,

    -- ** VpcEndpointConnection
    vpcEndpointConnection_vpcEndpointOwner,
    vpcEndpointConnection_networkLoadBalancerArns,
    vpcEndpointConnection_dnsEntries,
    vpcEndpointConnection_vpcEndpointState,
    vpcEndpointConnection_gatewayLoadBalancerArns,
    vpcEndpointConnection_creationTimestamp,
    vpcEndpointConnection_serviceId,
    vpcEndpointConnection_vpcEndpointId,

    -- ** VpcIpv6CidrBlockAssociation
    vpcIpv6CidrBlockAssociation_associationId,
    vpcIpv6CidrBlockAssociation_ipv6CidrBlock,
    vpcIpv6CidrBlockAssociation_networkBorderGroup,
    vpcIpv6CidrBlockAssociation_ipv6CidrBlockState,
    vpcIpv6CidrBlockAssociation_ipv6Pool,

    -- ** VpcPeeringConnection
    vpcPeeringConnection_vpcPeeringConnectionId,
    vpcPeeringConnection_status,
    vpcPeeringConnection_accepterVpcInfo,
    vpcPeeringConnection_requesterVpcInfo,
    vpcPeeringConnection_expirationTime,
    vpcPeeringConnection_tags,

    -- ** VpcPeeringConnectionOptionsDescription
    vpcPeeringConnectionOptionsDescription_allowEgressFromLocalVpcToRemoteClassicLink,
    vpcPeeringConnectionOptionsDescription_allowEgressFromLocalClassicLinkToRemoteVpc,
    vpcPeeringConnectionOptionsDescription_allowDnsResolutionFromRemoteVpc,

    -- ** VpcPeeringConnectionStateReason
    vpcPeeringConnectionStateReason_code,
    vpcPeeringConnectionStateReason_message,

    -- ** VpcPeeringConnectionVpcInfo
    vpcPeeringConnectionVpcInfo_cidrBlockSet,
    vpcPeeringConnectionVpcInfo_vpcId,
    vpcPeeringConnectionVpcInfo_ownerId,
    vpcPeeringConnectionVpcInfo_peeringOptions,
    vpcPeeringConnectionVpcInfo_cidrBlock,
    vpcPeeringConnectionVpcInfo_region,
    vpcPeeringConnectionVpcInfo_ipv6CidrBlockSet,

    -- ** VpnConnection
    vpnConnection_customerGatewayConfiguration,
    vpnConnection_routes,
    vpnConnection_vpnGatewayId,
    vpnConnection_category,
    vpnConnection_transitGatewayId,
    vpnConnection_options,
    vpnConnection_tags,
    vpnConnection_vgwTelemetry,
    vpnConnection_vpnConnectionId,
    vpnConnection_customerGatewayId,
    vpnConnection_state,
    vpnConnection_type,

    -- ** VpnConnectionDeviceType
    vpnConnectionDeviceType_vendor,
    vpnConnectionDeviceType_platform,
    vpnConnectionDeviceType_vpnConnectionDeviceTypeId,
    vpnConnectionDeviceType_software,

    -- ** VpnConnectionOptions
    vpnConnectionOptions_tunnelInsideIpVersion,
    vpnConnectionOptions_remoteIpv4NetworkCidr,
    vpnConnectionOptions_enableAcceleration,
    vpnConnectionOptions_localIpv4NetworkCidr,
    vpnConnectionOptions_remoteIpv6NetworkCidr,
    vpnConnectionOptions_tunnelOptions,
    vpnConnectionOptions_localIpv6NetworkCidr,
    vpnConnectionOptions_staticRoutesOnly,

    -- ** VpnConnectionOptionsSpecification
    vpnConnectionOptionsSpecification_tunnelInsideIpVersion,
    vpnConnectionOptionsSpecification_remoteIpv4NetworkCidr,
    vpnConnectionOptionsSpecification_enableAcceleration,
    vpnConnectionOptionsSpecification_localIpv4NetworkCidr,
    vpnConnectionOptionsSpecification_remoteIpv6NetworkCidr,
    vpnConnectionOptionsSpecification_tunnelOptions,
    vpnConnectionOptionsSpecification_localIpv6NetworkCidr,
    vpnConnectionOptionsSpecification_staticRoutesOnly,

    -- ** VpnGateway
    vpnGateway_state,
    vpnGateway_vpcAttachments,
    vpnGateway_vpnGatewayId,
    vpnGateway_amazonSideAsn,
    vpnGateway_availabilityZone,
    vpnGateway_type,
    vpnGateway_tags,

    -- ** VpnStaticRoute
    vpnStaticRoute_state,
    vpnStaticRoute_source,
    vpnStaticRoute_destinationCidrBlock,

    -- ** VpnTunnelOptionsSpecification
    vpnTunnelOptionsSpecification_replayWindowSize,
    vpnTunnelOptionsSpecification_dPDTimeoutAction,
    vpnTunnelOptionsSpecification_rekeyFuzzPercentage,
    vpnTunnelOptionsSpecification_phase1LifetimeSeconds,
    vpnTunnelOptionsSpecification_iKEVersions,
    vpnTunnelOptionsSpecification_phase2IntegrityAlgorithms,
    vpnTunnelOptionsSpecification_phase2LifetimeSeconds,
    vpnTunnelOptionsSpecification_phase1EncryptionAlgorithms,
    vpnTunnelOptionsSpecification_phase1DHGroupNumbers,
    vpnTunnelOptionsSpecification_phase1IntegrityAlgorithms,
    vpnTunnelOptionsSpecification_rekeyMarginTimeSeconds,
    vpnTunnelOptionsSpecification_dPDTimeoutSeconds,
    vpnTunnelOptionsSpecification_tunnelInsideCidr,
    vpnTunnelOptionsSpecification_startupAction,
    vpnTunnelOptionsSpecification_phase2EncryptionAlgorithms,
    vpnTunnelOptionsSpecification_phase2DHGroupNumbers,
    vpnTunnelOptionsSpecification_preSharedKey,
    vpnTunnelOptionsSpecification_tunnelInsideIpv6Cidr,
  )
where

import Amazonka.EC2.AcceptReservedInstancesExchangeQuote
import Amazonka.EC2.AcceptTransitGatewayMulticastDomainAssociations
import Amazonka.EC2.AcceptTransitGatewayPeeringAttachment
import Amazonka.EC2.AcceptTransitGatewayVpcAttachment
import Amazonka.EC2.AcceptVpcEndpointConnections
import Amazonka.EC2.AcceptVpcPeeringConnection
import Amazonka.EC2.AdvertiseByoipCidr
import Amazonka.EC2.AllocateAddress
import Amazonka.EC2.AllocateHosts
import Amazonka.EC2.ApplySecurityGroupsToClientVpnTargetNetwork
import Amazonka.EC2.AssignIpv6Addresses
import Amazonka.EC2.AssignPrivateIpAddresses
import Amazonka.EC2.AssociateAddress
import Amazonka.EC2.AssociateClientVpnTargetNetwork
import Amazonka.EC2.AssociateDhcpOptions
import Amazonka.EC2.AssociateEnclaveCertificateIamRole
import Amazonka.EC2.AssociateIamInstanceProfile
import Amazonka.EC2.AssociateInstanceEventWindow
import Amazonka.EC2.AssociateRouteTable
import Amazonka.EC2.AssociateSubnetCidrBlock
import Amazonka.EC2.AssociateTransitGatewayMulticastDomain
import Amazonka.EC2.AssociateTransitGatewayRouteTable
import Amazonka.EC2.AssociateTrunkInterface
import Amazonka.EC2.AssociateVpcCidrBlock
import Amazonka.EC2.AttachClassicLinkVpc
import Amazonka.EC2.AttachInternetGateway
import Amazonka.EC2.AttachNetworkInterface
import Amazonka.EC2.AttachVolume
import Amazonka.EC2.AttachVpnGateway
import Amazonka.EC2.AuthorizeClientVpnIngress
import Amazonka.EC2.AuthorizeSecurityGroupEgress
import Amazonka.EC2.AuthorizeSecurityGroupIngress
import Amazonka.EC2.BundleInstance
import Amazonka.EC2.CancelBundleTask
import Amazonka.EC2.CancelCapacityReservation
import Amazonka.EC2.CancelCapacityReservationFleets
import Amazonka.EC2.CancelConversionTask
import Amazonka.EC2.CancelExportTask
import Amazonka.EC2.CancelImportTask
import Amazonka.EC2.CancelReservedInstancesListing
import Amazonka.EC2.CancelSpotFleetRequests
import Amazonka.EC2.CancelSpotInstanceRequests
import Amazonka.EC2.ConfirmProductInstance
import Amazonka.EC2.CopyFpgaImage
import Amazonka.EC2.CopyImage
import Amazonka.EC2.CopySnapshot
import Amazonka.EC2.CreateCapacityReservation
import Amazonka.EC2.CreateCapacityReservationFleet
import Amazonka.EC2.CreateCarrierGateway
import Amazonka.EC2.CreateClientVpnEndpoint
import Amazonka.EC2.CreateClientVpnRoute
import Amazonka.EC2.CreateCustomerGateway
import Amazonka.EC2.CreateDefaultSubnet
import Amazonka.EC2.CreateDefaultVpc
import Amazonka.EC2.CreateDhcpOptions
import Amazonka.EC2.CreateEgressOnlyInternetGateway
import Amazonka.EC2.CreateFleet
import Amazonka.EC2.CreateFlowLogs
import Amazonka.EC2.CreateFpgaImage
import Amazonka.EC2.CreateImage
import Amazonka.EC2.CreateInstanceEventWindow
import Amazonka.EC2.CreateInstanceExportTask
import Amazonka.EC2.CreateInternetGateway
import Amazonka.EC2.CreateKeyPair
import Amazonka.EC2.CreateLaunchTemplate
import Amazonka.EC2.CreateLaunchTemplateVersion
import Amazonka.EC2.CreateLocalGatewayRoute
import Amazonka.EC2.CreateLocalGatewayRouteTableVpcAssociation
import Amazonka.EC2.CreateManagedPrefixList
import Amazonka.EC2.CreateNatGateway
import Amazonka.EC2.CreateNetworkAcl
import Amazonka.EC2.CreateNetworkAclEntry
import Amazonka.EC2.CreateNetworkInsightsPath
import Amazonka.EC2.CreateNetworkInterface
import Amazonka.EC2.CreateNetworkInterfacePermission
import Amazonka.EC2.CreatePlacementGroup
import Amazonka.EC2.CreateReplaceRootVolumeTask
import Amazonka.EC2.CreateReservedInstancesListing
import Amazonka.EC2.CreateRestoreImageTask
import Amazonka.EC2.CreateRoute
import Amazonka.EC2.CreateRouteTable
import Amazonka.EC2.CreateSecurityGroup
import Amazonka.EC2.CreateSnapshot
import Amazonka.EC2.CreateSnapshots
import Amazonka.EC2.CreateSpotDatafeedSubscription
import Amazonka.EC2.CreateStoreImageTask
import Amazonka.EC2.CreateSubnet
import Amazonka.EC2.CreateSubnetCidrReservation
import Amazonka.EC2.CreateTags
import Amazonka.EC2.CreateTrafficMirrorFilter
import Amazonka.EC2.CreateTrafficMirrorFilterRule
import Amazonka.EC2.CreateTrafficMirrorSession
import Amazonka.EC2.CreateTrafficMirrorTarget
import Amazonka.EC2.CreateTransitGateway
import Amazonka.EC2.CreateTransitGatewayConnect
import Amazonka.EC2.CreateTransitGatewayConnectPeer
import Amazonka.EC2.CreateTransitGatewayMulticastDomain
import Amazonka.EC2.CreateTransitGatewayPeeringAttachment
import Amazonka.EC2.CreateTransitGatewayPrefixListReference
import Amazonka.EC2.CreateTransitGatewayRoute
import Amazonka.EC2.CreateTransitGatewayRouteTable
import Amazonka.EC2.CreateTransitGatewayVpcAttachment
import Amazonka.EC2.CreateVolume
import Amazonka.EC2.CreateVpc
import Amazonka.EC2.CreateVpcEndpoint
import Amazonka.EC2.CreateVpcEndpointConnectionNotification
import Amazonka.EC2.CreateVpcEndpointServiceConfiguration
import Amazonka.EC2.CreateVpcPeeringConnection
import Amazonka.EC2.CreateVpnConnection
import Amazonka.EC2.CreateVpnConnectionRoute
import Amazonka.EC2.CreateVpnGateway
import Amazonka.EC2.DeleteCarrierGateway
import Amazonka.EC2.DeleteClientVpnEndpoint
import Amazonka.EC2.DeleteClientVpnRoute
import Amazonka.EC2.DeleteCustomerGateway
import Amazonka.EC2.DeleteDhcpOptions
import Amazonka.EC2.DeleteEgressOnlyInternetGateway
import Amazonka.EC2.DeleteFleets
import Amazonka.EC2.DeleteFlowLogs
import Amazonka.EC2.DeleteFpgaImage
import Amazonka.EC2.DeleteInstanceEventWindow
import Amazonka.EC2.DeleteInternetGateway
import Amazonka.EC2.DeleteKeyPair
import Amazonka.EC2.DeleteLaunchTemplate
import Amazonka.EC2.DeleteLaunchTemplateVersions
import Amazonka.EC2.DeleteLocalGatewayRoute
import Amazonka.EC2.DeleteLocalGatewayRouteTableVpcAssociation
import Amazonka.EC2.DeleteManagedPrefixList
import Amazonka.EC2.DeleteNatGateway
import Amazonka.EC2.DeleteNetworkAcl
import Amazonka.EC2.DeleteNetworkAclEntry
import Amazonka.EC2.DeleteNetworkInsightsAnalysis
import Amazonka.EC2.DeleteNetworkInsightsPath
import Amazonka.EC2.DeleteNetworkInterface
import Amazonka.EC2.DeleteNetworkInterfacePermission
import Amazonka.EC2.DeletePlacementGroup
import Amazonka.EC2.DeleteQueuedReservedInstances
import Amazonka.EC2.DeleteRoute
import Amazonka.EC2.DeleteRouteTable
import Amazonka.EC2.DeleteSecurityGroup
import Amazonka.EC2.DeleteSnapshot
import Amazonka.EC2.DeleteSpotDatafeedSubscription
import Amazonka.EC2.DeleteSubnet
import Amazonka.EC2.DeleteSubnetCidrReservation
import Amazonka.EC2.DeleteTags
import Amazonka.EC2.DeleteTrafficMirrorFilter
import Amazonka.EC2.DeleteTrafficMirrorFilterRule
import Amazonka.EC2.DeleteTrafficMirrorSession
import Amazonka.EC2.DeleteTrafficMirrorTarget
import Amazonka.EC2.DeleteTransitGateway
import Amazonka.EC2.DeleteTransitGatewayConnect
import Amazonka.EC2.DeleteTransitGatewayConnectPeer
import Amazonka.EC2.DeleteTransitGatewayMulticastDomain
import Amazonka.EC2.DeleteTransitGatewayPeeringAttachment
import Amazonka.EC2.DeleteTransitGatewayPrefixListReference
import Amazonka.EC2.DeleteTransitGatewayRoute
import Amazonka.EC2.DeleteTransitGatewayRouteTable
import Amazonka.EC2.DeleteTransitGatewayVpcAttachment
import Amazonka.EC2.DeleteVolume
import Amazonka.EC2.DeleteVpc
import Amazonka.EC2.DeleteVpcEndpointConnectionNotifications
import Amazonka.EC2.DeleteVpcEndpointServiceConfigurations
import Amazonka.EC2.DeleteVpcEndpoints
import Amazonka.EC2.DeleteVpcPeeringConnection
import Amazonka.EC2.DeleteVpnConnection
import Amazonka.EC2.DeleteVpnConnectionRoute
import Amazonka.EC2.DeleteVpnGateway
import Amazonka.EC2.DeprovisionByoipCidr
import Amazonka.EC2.DeregisterImage
import Amazonka.EC2.DeregisterInstanceEventNotificationAttributes
import Amazonka.EC2.DeregisterTransitGatewayMulticastGroupMembers
import Amazonka.EC2.DeregisterTransitGatewayMulticastGroupSources
import Amazonka.EC2.DescribeAccountAttributes
import Amazonka.EC2.DescribeAddresses
import Amazonka.EC2.DescribeAddressesAttribute
import Amazonka.EC2.DescribeAggregateIdFormat
import Amazonka.EC2.DescribeAvailabilityZones
import Amazonka.EC2.DescribeBundleTasks
import Amazonka.EC2.DescribeByoipCidrs
import Amazonka.EC2.DescribeCapacityReservationFleets
import Amazonka.EC2.DescribeCapacityReservations
import Amazonka.EC2.DescribeCarrierGateways
import Amazonka.EC2.DescribeClassicLinkInstances
import Amazonka.EC2.DescribeClientVpnAuthorizationRules
import Amazonka.EC2.DescribeClientVpnConnections
import Amazonka.EC2.DescribeClientVpnEndpoints
import Amazonka.EC2.DescribeClientVpnRoutes
import Amazonka.EC2.DescribeClientVpnTargetNetworks
import Amazonka.EC2.DescribeCoipPools
import Amazonka.EC2.DescribeConversionTasks
import Amazonka.EC2.DescribeCustomerGateways
import Amazonka.EC2.DescribeDhcpOptions
import Amazonka.EC2.DescribeEgressOnlyInternetGateways
import Amazonka.EC2.DescribeElasticGpus
import Amazonka.EC2.DescribeExportImageTasks
import Amazonka.EC2.DescribeExportTasks
import Amazonka.EC2.DescribeFastSnapshotRestores
import Amazonka.EC2.DescribeFleetHistory
import Amazonka.EC2.DescribeFleetInstances
import Amazonka.EC2.DescribeFleets
import Amazonka.EC2.DescribeFlowLogs
import Amazonka.EC2.DescribeFpgaImageAttribute
import Amazonka.EC2.DescribeFpgaImages
import Amazonka.EC2.DescribeHostReservationOfferings
import Amazonka.EC2.DescribeHostReservations
import Amazonka.EC2.DescribeHosts
import Amazonka.EC2.DescribeIamInstanceProfileAssociations
import Amazonka.EC2.DescribeIdFormat
import Amazonka.EC2.DescribeIdentityIdFormat
import Amazonka.EC2.DescribeImageAttribute
import Amazonka.EC2.DescribeImages
import Amazonka.EC2.DescribeImportImageTasks
import Amazonka.EC2.DescribeImportSnapshotTasks
import Amazonka.EC2.DescribeInstanceAttribute
import Amazonka.EC2.DescribeInstanceCreditSpecifications
import Amazonka.EC2.DescribeInstanceEventNotificationAttributes
import Amazonka.EC2.DescribeInstanceEventWindows
import Amazonka.EC2.DescribeInstanceStatus
import Amazonka.EC2.DescribeInstanceTypeOfferings
import Amazonka.EC2.DescribeInstanceTypes
import Amazonka.EC2.DescribeInstances
import Amazonka.EC2.DescribeInternetGateways
import Amazonka.EC2.DescribeIpv6Pools
import Amazonka.EC2.DescribeKeyPairs
import Amazonka.EC2.DescribeLaunchTemplateVersions
import Amazonka.EC2.DescribeLaunchTemplates
import Amazonka.EC2.DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
import Amazonka.EC2.DescribeLocalGatewayRouteTableVpcAssociations
import Amazonka.EC2.DescribeLocalGatewayRouteTables
import Amazonka.EC2.DescribeLocalGatewayVirtualInterfaceGroups
import Amazonka.EC2.DescribeLocalGatewayVirtualInterfaces
import Amazonka.EC2.DescribeLocalGateways
import Amazonka.EC2.DescribeManagedPrefixLists
import Amazonka.EC2.DescribeMovingAddresses
import Amazonka.EC2.DescribeNatGateways
import Amazonka.EC2.DescribeNetworkAcls
import Amazonka.EC2.DescribeNetworkInsightsAnalyses
import Amazonka.EC2.DescribeNetworkInsightsPaths
import Amazonka.EC2.DescribeNetworkInterfaceAttribute
import Amazonka.EC2.DescribeNetworkInterfacePermissions
import Amazonka.EC2.DescribeNetworkInterfaces
import Amazonka.EC2.DescribePlacementGroups
import Amazonka.EC2.DescribePrefixLists
import Amazonka.EC2.DescribePrincipalIdFormat
import Amazonka.EC2.DescribePublicIpv4Pools
import Amazonka.EC2.DescribeRegions
import Amazonka.EC2.DescribeReplaceRootVolumeTasks
import Amazonka.EC2.DescribeReservedInstances
import Amazonka.EC2.DescribeReservedInstancesListings
import Amazonka.EC2.DescribeReservedInstancesModifications
import Amazonka.EC2.DescribeReservedInstancesOfferings
import Amazonka.EC2.DescribeRouteTables
import Amazonka.EC2.DescribeScheduledInstanceAvailability
import Amazonka.EC2.DescribeScheduledInstances
import Amazonka.EC2.DescribeSecurityGroupReferences
import Amazonka.EC2.DescribeSecurityGroupRules
import Amazonka.EC2.DescribeSecurityGroups
import Amazonka.EC2.DescribeSnapshotAttribute
import Amazonka.EC2.DescribeSnapshots
import Amazonka.EC2.DescribeSpotDatafeedSubscription
import Amazonka.EC2.DescribeSpotFleetInstances
import Amazonka.EC2.DescribeSpotFleetRequestHistory
import Amazonka.EC2.DescribeSpotFleetRequests
import Amazonka.EC2.DescribeSpotInstanceRequests
import Amazonka.EC2.DescribeSpotPriceHistory
import Amazonka.EC2.DescribeStaleSecurityGroups
import Amazonka.EC2.DescribeStoreImageTasks
import Amazonka.EC2.DescribeSubnets
import Amazonka.EC2.DescribeTags
import Amazonka.EC2.DescribeTrafficMirrorFilters
import Amazonka.EC2.DescribeTrafficMirrorSessions
import Amazonka.EC2.DescribeTrafficMirrorTargets
import Amazonka.EC2.DescribeTransitGatewayAttachments
import Amazonka.EC2.DescribeTransitGatewayConnectPeers
import Amazonka.EC2.DescribeTransitGatewayConnects
import Amazonka.EC2.DescribeTransitGatewayMulticastDomains
import Amazonka.EC2.DescribeTransitGatewayPeeringAttachments
import Amazonka.EC2.DescribeTransitGatewayRouteTables
import Amazonka.EC2.DescribeTransitGatewayVpcAttachments
import Amazonka.EC2.DescribeTransitGateways
import Amazonka.EC2.DescribeTrunkInterfaceAssociations
import Amazonka.EC2.DescribeVolumeAttribute
import Amazonka.EC2.DescribeVolumeStatus
import Amazonka.EC2.DescribeVolumes
import Amazonka.EC2.DescribeVolumesModifications
import Amazonka.EC2.DescribeVpcAttribute
import Amazonka.EC2.DescribeVpcClassicLink
import Amazonka.EC2.DescribeVpcClassicLinkDnsSupport
import Amazonka.EC2.DescribeVpcEndpointConnectionNotifications
import Amazonka.EC2.DescribeVpcEndpointConnections
import Amazonka.EC2.DescribeVpcEndpointServiceConfigurations
import Amazonka.EC2.DescribeVpcEndpointServicePermissions
import Amazonka.EC2.DescribeVpcEndpointServices
import Amazonka.EC2.DescribeVpcEndpoints
import Amazonka.EC2.DescribeVpcPeeringConnections
import Amazonka.EC2.DescribeVpcs
import Amazonka.EC2.DescribeVpnConnections
import Amazonka.EC2.DescribeVpnGateways
import Amazonka.EC2.DetachClassicLinkVpc
import Amazonka.EC2.DetachInternetGateway
import Amazonka.EC2.DetachNetworkInterface
import Amazonka.EC2.DetachVolume
import Amazonka.EC2.DetachVpnGateway
import Amazonka.EC2.DisableEbsEncryptionByDefault
import Amazonka.EC2.DisableFastSnapshotRestores
import Amazonka.EC2.DisableImageDeprecation
import Amazonka.EC2.DisableSerialConsoleAccess
import Amazonka.EC2.DisableTransitGatewayRouteTablePropagation
import Amazonka.EC2.DisableVgwRoutePropagation
import Amazonka.EC2.DisableVpcClassicLink
import Amazonka.EC2.DisableVpcClassicLinkDnsSupport
import Amazonka.EC2.DisassociateAddress
import Amazonka.EC2.DisassociateClientVpnTargetNetwork
import Amazonka.EC2.DisassociateEnclaveCertificateIamRole
import Amazonka.EC2.DisassociateIamInstanceProfile
import Amazonka.EC2.DisassociateInstanceEventWindow
import Amazonka.EC2.DisassociateRouteTable
import Amazonka.EC2.DisassociateSubnetCidrBlock
import Amazonka.EC2.DisassociateTransitGatewayMulticastDomain
import Amazonka.EC2.DisassociateTransitGatewayRouteTable
import Amazonka.EC2.DisassociateTrunkInterface
import Amazonka.EC2.DisassociateVpcCidrBlock
import Amazonka.EC2.EnableEbsEncryptionByDefault
import Amazonka.EC2.EnableFastSnapshotRestores
import Amazonka.EC2.EnableImageDeprecation
import Amazonka.EC2.EnableSerialConsoleAccess
import Amazonka.EC2.EnableTransitGatewayRouteTablePropagation
import Amazonka.EC2.EnableVgwRoutePropagation
import Amazonka.EC2.EnableVolumeIO
import Amazonka.EC2.EnableVpcClassicLink
import Amazonka.EC2.EnableVpcClassicLinkDnsSupport
import Amazonka.EC2.ExportClientVpnClientCertificateRevocationList
import Amazonka.EC2.ExportClientVpnClientConfiguration
import Amazonka.EC2.ExportImage
import Amazonka.EC2.ExportTransitGatewayRoutes
import Amazonka.EC2.GetAssociatedEnclaveCertificateIamRoles
import Amazonka.EC2.GetAssociatedIpv6PoolCidrs
import Amazonka.EC2.GetCapacityReservationUsage
import Amazonka.EC2.GetCoipPoolUsage
import Amazonka.EC2.GetConsoleOutput
import Amazonka.EC2.GetConsoleScreenshot
import Amazonka.EC2.GetDefaultCreditSpecification
import Amazonka.EC2.GetEbsDefaultKmsKeyId
import Amazonka.EC2.GetEbsEncryptionByDefault
import Amazonka.EC2.GetFlowLogsIntegrationTemplate
import Amazonka.EC2.GetGroupsForCapacityReservation
import Amazonka.EC2.GetHostReservationPurchasePreview
import Amazonka.EC2.GetLaunchTemplateData
import Amazonka.EC2.GetManagedPrefixListAssociations
import Amazonka.EC2.GetManagedPrefixListEntries
import Amazonka.EC2.GetPasswordData
import Amazonka.EC2.GetReservedInstancesExchangeQuote
import Amazonka.EC2.GetSerialConsoleAccessStatus
import Amazonka.EC2.GetSubnetCidrReservations
import Amazonka.EC2.GetTransitGatewayAttachmentPropagations
import Amazonka.EC2.GetTransitGatewayMulticastDomainAssociations
import Amazonka.EC2.GetTransitGatewayPrefixListReferences
import Amazonka.EC2.GetTransitGatewayRouteTableAssociations
import Amazonka.EC2.GetTransitGatewayRouteTablePropagations
import Amazonka.EC2.GetVpnConnectionDeviceSampleConfiguration
import Amazonka.EC2.GetVpnConnectionDeviceTypes
import Amazonka.EC2.ImportClientVpnClientCertificateRevocationList
import Amazonka.EC2.ImportImage
import Amazonka.EC2.ImportInstance
import Amazonka.EC2.ImportKeyPair
import Amazonka.EC2.ImportSnapshot
import Amazonka.EC2.ImportVolume
import Amazonka.EC2.ModifyAddressAttribute
import Amazonka.EC2.ModifyAvailabilityZoneGroup
import Amazonka.EC2.ModifyCapacityReservation
import Amazonka.EC2.ModifyCapacityReservationFleet
import Amazonka.EC2.ModifyClientVpnEndpoint
import Amazonka.EC2.ModifyDefaultCreditSpecification
import Amazonka.EC2.ModifyEbsDefaultKmsKeyId
import Amazonka.EC2.ModifyFleet
import Amazonka.EC2.ModifyFpgaImageAttribute
import Amazonka.EC2.ModifyHosts
import Amazonka.EC2.ModifyIdFormat
import Amazonka.EC2.ModifyIdentityIdFormat
import Amazonka.EC2.ModifyImageAttribute
import Amazonka.EC2.ModifyInstanceAttribute
import Amazonka.EC2.ModifyInstanceCapacityReservationAttributes
import Amazonka.EC2.ModifyInstanceCreditSpecification
import Amazonka.EC2.ModifyInstanceEventStartTime
import Amazonka.EC2.ModifyInstanceEventWindow
import Amazonka.EC2.ModifyInstanceMetadataOptions
import Amazonka.EC2.ModifyInstancePlacement
import Amazonka.EC2.ModifyLaunchTemplate
import Amazonka.EC2.ModifyManagedPrefixList
import Amazonka.EC2.ModifyNetworkInterfaceAttribute
import Amazonka.EC2.ModifyReservedInstances
import Amazonka.EC2.ModifySecurityGroupRules
import Amazonka.EC2.ModifySnapshotAttribute
import Amazonka.EC2.ModifySpotFleetRequest
import Amazonka.EC2.ModifySubnetAttribute
import Amazonka.EC2.ModifyTrafficMirrorFilterNetworkServices
import Amazonka.EC2.ModifyTrafficMirrorFilterRule
import Amazonka.EC2.ModifyTrafficMirrorSession
import Amazonka.EC2.ModifyTransitGateway
import Amazonka.EC2.ModifyTransitGatewayPrefixListReference
import Amazonka.EC2.ModifyTransitGatewayVpcAttachment
import Amazonka.EC2.ModifyVolume
import Amazonka.EC2.ModifyVolumeAttribute
import Amazonka.EC2.ModifyVpcAttribute
import Amazonka.EC2.ModifyVpcEndpoint
import Amazonka.EC2.ModifyVpcEndpointConnectionNotification
import Amazonka.EC2.ModifyVpcEndpointServiceConfiguration
import Amazonka.EC2.ModifyVpcEndpointServicePermissions
import Amazonka.EC2.ModifyVpcPeeringConnectionOptions
import Amazonka.EC2.ModifyVpcTenancy
import Amazonka.EC2.ModifyVpnConnection
import Amazonka.EC2.ModifyVpnConnectionOptions
import Amazonka.EC2.ModifyVpnTunnelCertificate
import Amazonka.EC2.ModifyVpnTunnelOptions
import Amazonka.EC2.MonitorInstances
import Amazonka.EC2.MoveAddressToVpc
import Amazonka.EC2.ProvisionByoipCidr
import Amazonka.EC2.PurchaseHostReservation
import Amazonka.EC2.PurchaseReservedInstancesOffering
import Amazonka.EC2.PurchaseScheduledInstances
import Amazonka.EC2.RebootInstances
import Amazonka.EC2.RegisterImage
import Amazonka.EC2.RegisterInstanceEventNotificationAttributes
import Amazonka.EC2.RegisterTransitGatewayMulticastGroupMembers
import Amazonka.EC2.RegisterTransitGatewayMulticastGroupSources
import Amazonka.EC2.RejectTransitGatewayMulticastDomainAssociations
import Amazonka.EC2.RejectTransitGatewayPeeringAttachment
import Amazonka.EC2.RejectTransitGatewayVpcAttachment
import Amazonka.EC2.RejectVpcEndpointConnections
import Amazonka.EC2.RejectVpcPeeringConnection
import Amazonka.EC2.ReleaseAddress
import Amazonka.EC2.ReleaseHosts
import Amazonka.EC2.ReplaceIamInstanceProfileAssociation
import Amazonka.EC2.ReplaceNetworkAclAssociation
import Amazonka.EC2.ReplaceNetworkAclEntry
import Amazonka.EC2.ReplaceRoute
import Amazonka.EC2.ReplaceRouteTableAssociation
import Amazonka.EC2.ReplaceTransitGatewayRoute
import Amazonka.EC2.ReportInstanceStatus
import Amazonka.EC2.RequestSpotFleet
import Amazonka.EC2.RequestSpotInstances
import Amazonka.EC2.ResetAddressAttribute
import Amazonka.EC2.ResetEbsDefaultKmsKeyId
import Amazonka.EC2.ResetFpgaImageAttribute
import Amazonka.EC2.ResetImageAttribute
import Amazonka.EC2.ResetInstanceAttribute
import Amazonka.EC2.ResetNetworkInterfaceAttribute
import Amazonka.EC2.ResetSnapshotAttribute
import Amazonka.EC2.RestoreAddressToClassic
import Amazonka.EC2.RestoreManagedPrefixListVersion
import Amazonka.EC2.RevokeClientVpnIngress
import Amazonka.EC2.RevokeSecurityGroupEgress
import Amazonka.EC2.RevokeSecurityGroupIngress
import Amazonka.EC2.RunInstances
import Amazonka.EC2.RunScheduledInstances
import Amazonka.EC2.SearchLocalGatewayRoutes
import Amazonka.EC2.SearchTransitGatewayMulticastGroups
import Amazonka.EC2.SearchTransitGatewayRoutes
import Amazonka.EC2.SendDiagnosticInterrupt
import Amazonka.EC2.StartInstances
import Amazonka.EC2.StartNetworkInsightsAnalysis
import Amazonka.EC2.StartVpcEndpointServicePrivateDnsVerification
import Amazonka.EC2.StopInstances
import Amazonka.EC2.TerminateClientVpnConnections
import Amazonka.EC2.TerminateInstances
import Amazonka.EC2.Types.AccountAttribute
import Amazonka.EC2.Types.AccountAttributeValue
import Amazonka.EC2.Types.ActiveInstance
import Amazonka.EC2.Types.AddPrefixListEntry
import Amazonka.EC2.Types.Address
import Amazonka.EC2.Types.AddressAttribute
import Amazonka.EC2.Types.AllowedPrincipal
import Amazonka.EC2.Types.AlternatePathHint
import Amazonka.EC2.Types.AnalysisAclRule
import Amazonka.EC2.Types.AnalysisComponent
import Amazonka.EC2.Types.AnalysisLoadBalancerListener
import Amazonka.EC2.Types.AnalysisLoadBalancerTarget
import Amazonka.EC2.Types.AnalysisPacketHeader
import Amazonka.EC2.Types.AnalysisRouteTableRoute
import Amazonka.EC2.Types.AnalysisSecurityGroupRule
import Amazonka.EC2.Types.AssignedPrivateIpAddress
import Amazonka.EC2.Types.AssociatedRole
import Amazonka.EC2.Types.AssociatedTargetNetwork
import Amazonka.EC2.Types.AssociationStatus
import Amazonka.EC2.Types.AthenaIntegration
import Amazonka.EC2.Types.AttributeBooleanValue
import Amazonka.EC2.Types.AttributeValue
import Amazonka.EC2.Types.AuthorizationRule
import Amazonka.EC2.Types.AvailabilityZone
import Amazonka.EC2.Types.AvailabilityZoneMessage
import Amazonka.EC2.Types.AvailableCapacity
import Amazonka.EC2.Types.BlobAttributeValue
import Amazonka.EC2.Types.BlockDeviceMapping
import Amazonka.EC2.Types.BundleTask
import Amazonka.EC2.Types.BundleTaskError
import Amazonka.EC2.Types.ByoipCidr
import Amazonka.EC2.Types.CancelCapacityReservationFleetError
import Amazonka.EC2.Types.CancelSpotFleetRequestsError
import Amazonka.EC2.Types.CancelSpotFleetRequestsErrorItem
import Amazonka.EC2.Types.CancelSpotFleetRequestsSuccessItem
import Amazonka.EC2.Types.CancelledSpotInstanceRequest
import Amazonka.EC2.Types.CapacityReservation
import Amazonka.EC2.Types.CapacityReservationFleet
import Amazonka.EC2.Types.CapacityReservationFleetCancellationState
import Amazonka.EC2.Types.CapacityReservationGroup
import Amazonka.EC2.Types.CapacityReservationOptions
import Amazonka.EC2.Types.CapacityReservationOptionsRequest
import Amazonka.EC2.Types.CapacityReservationSpecification
import Amazonka.EC2.Types.CapacityReservationSpecificationResponse
import Amazonka.EC2.Types.CapacityReservationTarget
import Amazonka.EC2.Types.CapacityReservationTargetResponse
import Amazonka.EC2.Types.CarrierGateway
import Amazonka.EC2.Types.CertificateAuthentication
import Amazonka.EC2.Types.CertificateAuthenticationRequest
import Amazonka.EC2.Types.CidrAuthorizationContext
import Amazonka.EC2.Types.CidrBlock
import Amazonka.EC2.Types.ClassicLinkDnsSupport
import Amazonka.EC2.Types.ClassicLinkInstance
import Amazonka.EC2.Types.ClassicLoadBalancer
import Amazonka.EC2.Types.ClassicLoadBalancersConfig
import Amazonka.EC2.Types.ClientCertificateRevocationListStatus
import Amazonka.EC2.Types.ClientConnectOptions
import Amazonka.EC2.Types.ClientConnectResponseOptions
import Amazonka.EC2.Types.ClientData
import Amazonka.EC2.Types.ClientVpnAuthentication
import Amazonka.EC2.Types.ClientVpnAuthenticationRequest
import Amazonka.EC2.Types.ClientVpnAuthorizationRuleStatus
import Amazonka.EC2.Types.ClientVpnConnection
import Amazonka.EC2.Types.ClientVpnConnectionStatus
import Amazonka.EC2.Types.ClientVpnEndpoint
import Amazonka.EC2.Types.ClientVpnEndpointAttributeStatus
import Amazonka.EC2.Types.ClientVpnEndpointStatus
import Amazonka.EC2.Types.ClientVpnRoute
import Amazonka.EC2.Types.ClientVpnRouteStatus
import Amazonka.EC2.Types.CoipAddressUsage
import Amazonka.EC2.Types.CoipPool
import Amazonka.EC2.Types.ConnectionLogOptions
import Amazonka.EC2.Types.ConnectionLogResponseOptions
import Amazonka.EC2.Types.ConnectionNotification
import Amazonka.EC2.Types.ConversionTask
import Amazonka.EC2.Types.CpuOptions
import Amazonka.EC2.Types.CpuOptionsRequest
import Amazonka.EC2.Types.CreateFleetError
import Amazonka.EC2.Types.CreateFleetInstance
import Amazonka.EC2.Types.CreateTransitGatewayConnectRequestOptions
import Amazonka.EC2.Types.CreateTransitGatewayMulticastDomainRequestOptions
import Amazonka.EC2.Types.CreateTransitGatewayVpcAttachmentRequestOptions
import Amazonka.EC2.Types.CreateVolumePermission
import Amazonka.EC2.Types.CreateVolumePermissionModifications
import Amazonka.EC2.Types.CreditSpecification
import Amazonka.EC2.Types.CreditSpecificationRequest
import Amazonka.EC2.Types.CustomerGateway
import Amazonka.EC2.Types.DeleteFleetError
import Amazonka.EC2.Types.DeleteFleetErrorItem
import Amazonka.EC2.Types.DeleteFleetSuccessItem
import Amazonka.EC2.Types.DeleteLaunchTemplateVersionsResponseErrorItem
import Amazonka.EC2.Types.DeleteLaunchTemplateVersionsResponseSuccessItem
import Amazonka.EC2.Types.DeleteQueuedReservedInstancesError
import Amazonka.EC2.Types.DeregisterInstanceTagAttributeRequest
import Amazonka.EC2.Types.DescribeFastSnapshotRestoreSuccessItem
import Amazonka.EC2.Types.DescribeFleetError
import Amazonka.EC2.Types.DescribeFleetsInstances
import Amazonka.EC2.Types.DestinationOptionsRequest
import Amazonka.EC2.Types.DestinationOptionsResponse
import Amazonka.EC2.Types.DhcpConfiguration
import Amazonka.EC2.Types.DhcpOptions
import Amazonka.EC2.Types.DirectoryServiceAuthentication
import Amazonka.EC2.Types.DirectoryServiceAuthenticationRequest
import Amazonka.EC2.Types.DisableFastSnapshotRestoreErrorItem
import Amazonka.EC2.Types.DisableFastSnapshotRestoreStateError
import Amazonka.EC2.Types.DisableFastSnapshotRestoreStateErrorItem
import Amazonka.EC2.Types.DisableFastSnapshotRestoreSuccessItem
import Amazonka.EC2.Types.DiskImage
import Amazonka.EC2.Types.DiskImageDescription
import Amazonka.EC2.Types.DiskImageDetail
import Amazonka.EC2.Types.DiskImageVolumeDescription
import Amazonka.EC2.Types.DiskInfo
import Amazonka.EC2.Types.DnsEntry
import Amazonka.EC2.Types.DnsServersOptionsModifyStructure
import Amazonka.EC2.Types.EbsBlockDevice
import Amazonka.EC2.Types.EbsInfo
import Amazonka.EC2.Types.EbsInstanceBlockDevice
import Amazonka.EC2.Types.EbsInstanceBlockDeviceSpecification
import Amazonka.EC2.Types.EbsOptimizedInfo
import Amazonka.EC2.Types.EfaInfo
import Amazonka.EC2.Types.EgressOnlyInternetGateway
import Amazonka.EC2.Types.ElasticGpuAssociation
import Amazonka.EC2.Types.ElasticGpuHealth
import Amazonka.EC2.Types.ElasticGpuSpecification
import Amazonka.EC2.Types.ElasticGpuSpecificationResponse
import Amazonka.EC2.Types.ElasticGpus
import Amazonka.EC2.Types.ElasticInferenceAccelerator
import Amazonka.EC2.Types.ElasticInferenceAcceleratorAssociation
import Amazonka.EC2.Types.EnableFastSnapshotRestoreErrorItem
import Amazonka.EC2.Types.EnableFastSnapshotRestoreStateError
import Amazonka.EC2.Types.EnableFastSnapshotRestoreStateErrorItem
import Amazonka.EC2.Types.EnableFastSnapshotRestoreSuccessItem
import Amazonka.EC2.Types.EnclaveOptions
import Amazonka.EC2.Types.EnclaveOptionsRequest
import Amazonka.EC2.Types.EventInformation
import Amazonka.EC2.Types.Explanation
import Amazonka.EC2.Types.ExportImageTask
import Amazonka.EC2.Types.ExportTask
import Amazonka.EC2.Types.ExportTaskS3Location
import Amazonka.EC2.Types.ExportTaskS3LocationRequest
import Amazonka.EC2.Types.ExportToS3Task
import Amazonka.EC2.Types.ExportToS3TaskSpecification
import Amazonka.EC2.Types.FailedCapacityReservationFleetCancellationResult
import Amazonka.EC2.Types.FailedQueuedPurchaseDeletion
import Amazonka.EC2.Types.FederatedAuthentication
import Amazonka.EC2.Types.FederatedAuthenticationRequest
import Amazonka.EC2.Types.Filter
import Amazonka.EC2.Types.FleetCapacityReservation
import Amazonka.EC2.Types.FleetData
import Amazonka.EC2.Types.FleetLaunchTemplateConfig
import Amazonka.EC2.Types.FleetLaunchTemplateConfigRequest
import Amazonka.EC2.Types.FleetLaunchTemplateOverrides
import Amazonka.EC2.Types.FleetLaunchTemplateOverridesRequest
import Amazonka.EC2.Types.FleetLaunchTemplateSpecification
import Amazonka.EC2.Types.FleetLaunchTemplateSpecificationRequest
import Amazonka.EC2.Types.FleetSpotCapacityRebalance
import Amazonka.EC2.Types.FleetSpotCapacityRebalanceRequest
import Amazonka.EC2.Types.FleetSpotMaintenanceStrategies
import Amazonka.EC2.Types.FleetSpotMaintenanceStrategiesRequest
import Amazonka.EC2.Types.FlowLog
import Amazonka.EC2.Types.FpgaDeviceInfo
import Amazonka.EC2.Types.FpgaDeviceMemoryInfo
import Amazonka.EC2.Types.FpgaImage
import Amazonka.EC2.Types.FpgaImageAttribute
import Amazonka.EC2.Types.FpgaImageState
import Amazonka.EC2.Types.FpgaInfo
import Amazonka.EC2.Types.GpuDeviceInfo
import Amazonka.EC2.Types.GpuDeviceMemoryInfo
import Amazonka.EC2.Types.GpuInfo
import Amazonka.EC2.Types.GroupIdentifier
import Amazonka.EC2.Types.HibernationOptions
import Amazonka.EC2.Types.HibernationOptionsRequest
import Amazonka.EC2.Types.HistoryRecord
import Amazonka.EC2.Types.HistoryRecordEntry
import Amazonka.EC2.Types.Host
import Amazonka.EC2.Types.HostInstance
import Amazonka.EC2.Types.HostOffering
import Amazonka.EC2.Types.HostProperties
import Amazonka.EC2.Types.HostReservation
import Amazonka.EC2.Types.IKEVersionsListValue
import Amazonka.EC2.Types.IKEVersionsRequestListValue
import Amazonka.EC2.Types.IamInstanceProfile
import Amazonka.EC2.Types.IamInstanceProfileAssociation
import Amazonka.EC2.Types.IamInstanceProfileSpecification
import Amazonka.EC2.Types.IcmpTypeCode
import Amazonka.EC2.Types.IdFormat
import Amazonka.EC2.Types.Image
import Amazonka.EC2.Types.ImageDiskContainer
import Amazonka.EC2.Types.ImportImageLicenseConfigurationRequest
import Amazonka.EC2.Types.ImportImageLicenseConfigurationResponse
import Amazonka.EC2.Types.ImportImageTask
import Amazonka.EC2.Types.ImportInstanceLaunchSpecification
import Amazonka.EC2.Types.ImportInstanceTaskDetails
import Amazonka.EC2.Types.ImportInstanceVolumeDetailItem
import Amazonka.EC2.Types.ImportSnapshotTask
import Amazonka.EC2.Types.ImportVolumeTaskDetails
import Amazonka.EC2.Types.InferenceAcceleratorInfo
import Amazonka.EC2.Types.InferenceDeviceInfo
import Amazonka.EC2.Types.Instance
import Amazonka.EC2.Types.InstanceBlockDeviceMapping
import Amazonka.EC2.Types.InstanceBlockDeviceMappingSpecification
import Amazonka.EC2.Types.InstanceCapacity
import Amazonka.EC2.Types.InstanceCount
import Amazonka.EC2.Types.InstanceCreditSpecification
import Amazonka.EC2.Types.InstanceCreditSpecificationRequest
import Amazonka.EC2.Types.InstanceEventWindow
import Amazonka.EC2.Types.InstanceEventWindowAssociationRequest
import Amazonka.EC2.Types.InstanceEventWindowAssociationTarget
import Amazonka.EC2.Types.InstanceEventWindowDisassociationRequest
import Amazonka.EC2.Types.InstanceEventWindowStateChange
import Amazonka.EC2.Types.InstanceEventWindowTimeRange
import Amazonka.EC2.Types.InstanceEventWindowTimeRangeRequest
import Amazonka.EC2.Types.InstanceExportDetails
import Amazonka.EC2.Types.InstanceFamilyCreditSpecification
import Amazonka.EC2.Types.InstanceIpv4Prefix
import Amazonka.EC2.Types.InstanceIpv6Address
import Amazonka.EC2.Types.InstanceIpv6AddressRequest
import Amazonka.EC2.Types.InstanceIpv6Prefix
import Amazonka.EC2.Types.InstanceMarketOptionsRequest
import Amazonka.EC2.Types.InstanceMetadataOptionsRequest
import Amazonka.EC2.Types.InstanceMetadataOptionsResponse
import Amazonka.EC2.Types.InstanceMonitoring
import Amazonka.EC2.Types.InstanceNetworkInterface
import Amazonka.EC2.Types.InstanceNetworkInterfaceAssociation
import Amazonka.EC2.Types.InstanceNetworkInterfaceAttachment
import Amazonka.EC2.Types.InstanceNetworkInterfaceSpecification
import Amazonka.EC2.Types.InstancePrivateIpAddress
import Amazonka.EC2.Types.InstanceSpecification
import Amazonka.EC2.Types.InstanceState
import Amazonka.EC2.Types.InstanceStateChange
import Amazonka.EC2.Types.InstanceStatus
import Amazonka.EC2.Types.InstanceStatusDetails
import Amazonka.EC2.Types.InstanceStatusEvent
import Amazonka.EC2.Types.InstanceStatusSummary
import Amazonka.EC2.Types.InstanceStorageInfo
import Amazonka.EC2.Types.InstanceTagNotificationAttribute
import Amazonka.EC2.Types.InstanceTypeInfo
import Amazonka.EC2.Types.InstanceTypeOffering
import Amazonka.EC2.Types.InstanceUsage
import Amazonka.EC2.Types.IntegrateServices
import Amazonka.EC2.Types.InternetGateway
import Amazonka.EC2.Types.InternetGatewayAttachment
import Amazonka.EC2.Types.IpPermission
import Amazonka.EC2.Types.IpRange
import Amazonka.EC2.Types.Ipv4PrefixSpecification
import Amazonka.EC2.Types.Ipv4PrefixSpecificationRequest
import Amazonka.EC2.Types.Ipv4PrefixSpecificationResponse
import Amazonka.EC2.Types.Ipv6CidrAssociation
import Amazonka.EC2.Types.Ipv6CidrBlock
import Amazonka.EC2.Types.Ipv6Pool
import Amazonka.EC2.Types.Ipv6PrefixSpecification
import Amazonka.EC2.Types.Ipv6PrefixSpecificationRequest
import Amazonka.EC2.Types.Ipv6PrefixSpecificationResponse
import Amazonka.EC2.Types.Ipv6Range
import Amazonka.EC2.Types.KeyPairInfo
import Amazonka.EC2.Types.LastError
import Amazonka.EC2.Types.LaunchPermission
import Amazonka.EC2.Types.LaunchPermissionModifications
import Amazonka.EC2.Types.LaunchSpecification
import Amazonka.EC2.Types.LaunchTemplate
import Amazonka.EC2.Types.LaunchTemplateAndOverridesResponse
import Amazonka.EC2.Types.LaunchTemplateBlockDeviceMapping
import Amazonka.EC2.Types.LaunchTemplateBlockDeviceMappingRequest
import Amazonka.EC2.Types.LaunchTemplateCapacityReservationSpecificationRequest
import Amazonka.EC2.Types.LaunchTemplateCapacityReservationSpecificationResponse
import Amazonka.EC2.Types.LaunchTemplateConfig
import Amazonka.EC2.Types.LaunchTemplateCpuOptions
import Amazonka.EC2.Types.LaunchTemplateCpuOptionsRequest
import Amazonka.EC2.Types.LaunchTemplateEbsBlockDevice
import Amazonka.EC2.Types.LaunchTemplateEbsBlockDeviceRequest
import Amazonka.EC2.Types.LaunchTemplateElasticInferenceAccelerator
import Amazonka.EC2.Types.LaunchTemplateElasticInferenceAcceleratorResponse
import Amazonka.EC2.Types.LaunchTemplateEnclaveOptions
import Amazonka.EC2.Types.LaunchTemplateEnclaveOptionsRequest
import Amazonka.EC2.Types.LaunchTemplateHibernationOptions
import Amazonka.EC2.Types.LaunchTemplateHibernationOptionsRequest
import Amazonka.EC2.Types.LaunchTemplateIamInstanceProfileSpecification
import Amazonka.EC2.Types.LaunchTemplateIamInstanceProfileSpecificationRequest
import Amazonka.EC2.Types.LaunchTemplateInstanceMarketOptions
import Amazonka.EC2.Types.LaunchTemplateInstanceMarketOptionsRequest
import Amazonka.EC2.Types.LaunchTemplateInstanceMetadataOptions
import Amazonka.EC2.Types.LaunchTemplateInstanceMetadataOptionsRequest
import Amazonka.EC2.Types.LaunchTemplateInstanceNetworkInterfaceSpecification
import Amazonka.EC2.Types.LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
import Amazonka.EC2.Types.LaunchTemplateLicenseConfiguration
import Amazonka.EC2.Types.LaunchTemplateLicenseConfigurationRequest
import Amazonka.EC2.Types.LaunchTemplateOverrides
import Amazonka.EC2.Types.LaunchTemplatePlacement
import Amazonka.EC2.Types.LaunchTemplatePlacementRequest
import Amazonka.EC2.Types.LaunchTemplateSpecification
import Amazonka.EC2.Types.LaunchTemplateSpotMarketOptions
import Amazonka.EC2.Types.LaunchTemplateSpotMarketOptionsRequest
import Amazonka.EC2.Types.LaunchTemplateTagSpecification
import Amazonka.EC2.Types.LaunchTemplateTagSpecificationRequest
import Amazonka.EC2.Types.LaunchTemplateVersion
import Amazonka.EC2.Types.LaunchTemplatesMonitoring
import Amazonka.EC2.Types.LaunchTemplatesMonitoringRequest
import Amazonka.EC2.Types.LicenseConfiguration
import Amazonka.EC2.Types.LicenseConfigurationRequest
import Amazonka.EC2.Types.LoadBalancersConfig
import Amazonka.EC2.Types.LoadPermission
import Amazonka.EC2.Types.LoadPermissionModifications
import Amazonka.EC2.Types.LoadPermissionRequest
import Amazonka.EC2.Types.LocalGateway
import Amazonka.EC2.Types.LocalGatewayRoute
import Amazonka.EC2.Types.LocalGatewayRouteTable
import Amazonka.EC2.Types.LocalGatewayRouteTableVirtualInterfaceGroupAssociation
import Amazonka.EC2.Types.LocalGatewayRouteTableVpcAssociation
import Amazonka.EC2.Types.LocalGatewayVirtualInterface
import Amazonka.EC2.Types.LocalGatewayVirtualInterfaceGroup
import Amazonka.EC2.Types.ManagedPrefixList
import Amazonka.EC2.Types.MemoryInfo
import Amazonka.EC2.Types.ModifyTransitGatewayOptions
import Amazonka.EC2.Types.ModifyTransitGatewayVpcAttachmentRequestOptions
import Amazonka.EC2.Types.ModifyVpnTunnelOptionsSpecification
import Amazonka.EC2.Types.Monitoring
import Amazonka.EC2.Types.MovingAddressStatus
import Amazonka.EC2.Types.NatGateway
import Amazonka.EC2.Types.NatGatewayAddress
import Amazonka.EC2.Types.NetworkAcl
import Amazonka.EC2.Types.NetworkAclAssociation
import Amazonka.EC2.Types.NetworkAclEntry
import Amazonka.EC2.Types.NetworkCardInfo
import Amazonka.EC2.Types.NetworkInfo
import Amazonka.EC2.Types.NetworkInsightsAnalysis
import Amazonka.EC2.Types.NetworkInsightsPath
import Amazonka.EC2.Types.NetworkInterface
import Amazonka.EC2.Types.NetworkInterfaceAssociation
import Amazonka.EC2.Types.NetworkInterfaceAttachment
import Amazonka.EC2.Types.NetworkInterfaceAttachmentChanges
import Amazonka.EC2.Types.NetworkInterfaceIpv6Address
import Amazonka.EC2.Types.NetworkInterfacePermission
import Amazonka.EC2.Types.NetworkInterfacePermissionState
import Amazonka.EC2.Types.NetworkInterfacePrivateIpAddress
import Amazonka.EC2.Types.NewDhcpConfiguration
import Amazonka.EC2.Types.OnDemandOptions
import Amazonka.EC2.Types.OnDemandOptionsRequest
import Amazonka.EC2.Types.PathComponent
import Amazonka.EC2.Types.PciId
import Amazonka.EC2.Types.PeeringAttachmentStatus
import Amazonka.EC2.Types.PeeringConnectionOptions
import Amazonka.EC2.Types.PeeringConnectionOptionsRequest
import Amazonka.EC2.Types.PeeringTgwInfo
import Amazonka.EC2.Types.Phase1DHGroupNumbersListValue
import Amazonka.EC2.Types.Phase1DHGroupNumbersRequestListValue
import Amazonka.EC2.Types.Phase1EncryptionAlgorithmsListValue
import Amazonka.EC2.Types.Phase1EncryptionAlgorithmsRequestListValue
import Amazonka.EC2.Types.Phase1IntegrityAlgorithmsListValue
import Amazonka.EC2.Types.Phase1IntegrityAlgorithmsRequestListValue
import Amazonka.EC2.Types.Phase2DHGroupNumbersListValue
import Amazonka.EC2.Types.Phase2DHGroupNumbersRequestListValue
import Amazonka.EC2.Types.Phase2EncryptionAlgorithmsListValue
import Amazonka.EC2.Types.Phase2EncryptionAlgorithmsRequestListValue
import Amazonka.EC2.Types.Phase2IntegrityAlgorithmsListValue
import Amazonka.EC2.Types.Phase2IntegrityAlgorithmsRequestListValue
import Amazonka.EC2.Types.Placement
import Amazonka.EC2.Types.PlacementGroup
import Amazonka.EC2.Types.PlacementGroupInfo
import Amazonka.EC2.Types.PlacementResponse
import Amazonka.EC2.Types.PoolCidrBlock
import Amazonka.EC2.Types.PortRange
import Amazonka.EC2.Types.PrefixList
import Amazonka.EC2.Types.PrefixListAssociation
import Amazonka.EC2.Types.PrefixListEntry
import Amazonka.EC2.Types.PrefixListId
import Amazonka.EC2.Types.PriceSchedule
import Amazonka.EC2.Types.PriceScheduleSpecification
import Amazonka.EC2.Types.PricingDetail
import Amazonka.EC2.Types.PrincipalIdFormat
import Amazonka.EC2.Types.PrivateDnsDetails
import Amazonka.EC2.Types.PrivateDnsNameConfiguration
import Amazonka.EC2.Types.PrivateIpAddressSpecification
import Amazonka.EC2.Types.ProcessorInfo
import Amazonka.EC2.Types.ProductCode
import Amazonka.EC2.Types.PropagatingVgw
import Amazonka.EC2.Types.ProvisionedBandwidth
import Amazonka.EC2.Types.PtrUpdateStatus
import Amazonka.EC2.Types.PublicIpv4Pool
import Amazonka.EC2.Types.PublicIpv4PoolRange
import Amazonka.EC2.Types.Purchase
import Amazonka.EC2.Types.PurchaseRequest
import Amazonka.EC2.Types.RecurringCharge
import Amazonka.EC2.Types.ReferencedSecurityGroup
import Amazonka.EC2.Types.RegionInfo
import Amazonka.EC2.Types.RegisterInstanceTagAttributeRequest
import Amazonka.EC2.Types.RemovePrefixListEntry
import Amazonka.EC2.Types.ReplaceRootVolumeTask
import Amazonka.EC2.Types.RequestLaunchTemplateData
import Amazonka.EC2.Types.RequestSpotLaunchSpecification
import Amazonka.EC2.Types.Reservation
import Amazonka.EC2.Types.ReservationFleetInstanceSpecification
import Amazonka.EC2.Types.ReservationValue
import Amazonka.EC2.Types.ReservedInstanceLimitPrice
import Amazonka.EC2.Types.ReservedInstanceReservationValue
import Amazonka.EC2.Types.ReservedInstances
import Amazonka.EC2.Types.ReservedInstancesConfiguration
import Amazonka.EC2.Types.ReservedInstancesId
import Amazonka.EC2.Types.ReservedInstancesListing
import Amazonka.EC2.Types.ReservedInstancesModification
import Amazonka.EC2.Types.ReservedInstancesModificationResult
import Amazonka.EC2.Types.ReservedInstancesOffering
import Amazonka.EC2.Types.ResponseError
import Amazonka.EC2.Types.ResponseLaunchTemplateData
import Amazonka.EC2.Types.Route
import Amazonka.EC2.Types.RouteTable
import Amazonka.EC2.Types.RouteTableAssociation
import Amazonka.EC2.Types.RouteTableAssociationState
import Amazonka.EC2.Types.RunInstancesMonitoringEnabled
import Amazonka.EC2.Types.S3ObjectTag
import Amazonka.EC2.Types.S3Storage
import Amazonka.EC2.Types.ScheduledInstance
import Amazonka.EC2.Types.ScheduledInstanceAvailability
import Amazonka.EC2.Types.ScheduledInstanceRecurrence
import Amazonka.EC2.Types.ScheduledInstanceRecurrenceRequest
import Amazonka.EC2.Types.ScheduledInstancesBlockDeviceMapping
import Amazonka.EC2.Types.ScheduledInstancesEbs
import Amazonka.EC2.Types.ScheduledInstancesIamInstanceProfile
import Amazonka.EC2.Types.ScheduledInstancesIpv6Address
import Amazonka.EC2.Types.ScheduledInstancesLaunchSpecification
import Amazonka.EC2.Types.ScheduledInstancesMonitoring
import Amazonka.EC2.Types.ScheduledInstancesNetworkInterface
import Amazonka.EC2.Types.ScheduledInstancesPlacement
import Amazonka.EC2.Types.ScheduledInstancesPrivateIpAddressConfig
import Amazonka.EC2.Types.SecurityGroup
import Amazonka.EC2.Types.SecurityGroupIdentifier
import Amazonka.EC2.Types.SecurityGroupReference
import Amazonka.EC2.Types.SecurityGroupRule
import Amazonka.EC2.Types.SecurityGroupRuleDescription
import Amazonka.EC2.Types.SecurityGroupRuleRequest
import Amazonka.EC2.Types.SecurityGroupRuleUpdate
import Amazonka.EC2.Types.ServiceConfiguration
import Amazonka.EC2.Types.ServiceDetail
import Amazonka.EC2.Types.ServiceTypeDetail
import Amazonka.EC2.Types.SlotDateTimeRangeRequest
import Amazonka.EC2.Types.SlotStartTimeRangeRequest
import Amazonka.EC2.Types.Snapshot
import Amazonka.EC2.Types.SnapshotDetail
import Amazonka.EC2.Types.SnapshotDiskContainer
import Amazonka.EC2.Types.SnapshotInfo
import Amazonka.EC2.Types.SnapshotTaskDetail
import Amazonka.EC2.Types.SpotCapacityRebalance
import Amazonka.EC2.Types.SpotDatafeedSubscription
import Amazonka.EC2.Types.SpotFleetLaunchSpecification
import Amazonka.EC2.Types.SpotFleetMonitoring
import Amazonka.EC2.Types.SpotFleetRequestConfig
import Amazonka.EC2.Types.SpotFleetRequestConfigData
import Amazonka.EC2.Types.SpotFleetTagSpecification
import Amazonka.EC2.Types.SpotInstanceRequest
import Amazonka.EC2.Types.SpotInstanceStateFault
import Amazonka.EC2.Types.SpotInstanceStatus
import Amazonka.EC2.Types.SpotMaintenanceStrategies
import Amazonka.EC2.Types.SpotMarketOptions
import Amazonka.EC2.Types.SpotOptions
import Amazonka.EC2.Types.SpotOptionsRequest
import Amazonka.EC2.Types.SpotPlacement
import Amazonka.EC2.Types.SpotPrice
import Amazonka.EC2.Types.StaleIpPermission
import Amazonka.EC2.Types.StaleSecurityGroup
import Amazonka.EC2.Types.StateReason
import Amazonka.EC2.Types.Storage
import Amazonka.EC2.Types.StorageLocation
import Amazonka.EC2.Types.StoreImageTaskResult
import Amazonka.EC2.Types.Subnet
import Amazonka.EC2.Types.SubnetAssociation
import Amazonka.EC2.Types.SubnetCidrBlockState
import Amazonka.EC2.Types.SubnetCidrReservation
import Amazonka.EC2.Types.SubnetIpv6CidrBlockAssociation
import Amazonka.EC2.Types.SuccessfulInstanceCreditSpecificationItem
import Amazonka.EC2.Types.SuccessfulQueuedPurchaseDeletion
import Amazonka.EC2.Types.Tag
import Amazonka.EC2.Types.TagDescription
import Amazonka.EC2.Types.TagSpecification
import Amazonka.EC2.Types.TargetCapacitySpecification
import Amazonka.EC2.Types.TargetCapacitySpecificationRequest
import Amazonka.EC2.Types.TargetConfiguration
import Amazonka.EC2.Types.TargetConfigurationRequest
import Amazonka.EC2.Types.TargetGroup
import Amazonka.EC2.Types.TargetGroupsConfig
import Amazonka.EC2.Types.TargetNetwork
import Amazonka.EC2.Types.TargetReservationValue
import Amazonka.EC2.Types.TerminateConnectionStatus
import Amazonka.EC2.Types.TrafficMirrorFilter
import Amazonka.EC2.Types.TrafficMirrorFilterRule
import Amazonka.EC2.Types.TrafficMirrorPortRange
import Amazonka.EC2.Types.TrafficMirrorPortRangeRequest
import Amazonka.EC2.Types.TrafficMirrorSession
import Amazonka.EC2.Types.TrafficMirrorTarget
import Amazonka.EC2.Types.TransitGateway
import Amazonka.EC2.Types.TransitGatewayAssociation
import Amazonka.EC2.Types.TransitGatewayAttachment
import Amazonka.EC2.Types.TransitGatewayAttachmentAssociation
import Amazonka.EC2.Types.TransitGatewayAttachmentBgpConfiguration
import Amazonka.EC2.Types.TransitGatewayAttachmentPropagation
import Amazonka.EC2.Types.TransitGatewayConnect
import Amazonka.EC2.Types.TransitGatewayConnectOptions
import Amazonka.EC2.Types.TransitGatewayConnectPeer
import Amazonka.EC2.Types.TransitGatewayConnectPeerConfiguration
import Amazonka.EC2.Types.TransitGatewayConnectRequestBgpOptions
import Amazonka.EC2.Types.TransitGatewayMulticastDeregisteredGroupMembers
import Amazonka.EC2.Types.TransitGatewayMulticastDeregisteredGroupSources
import Amazonka.EC2.Types.TransitGatewayMulticastDomain
import Amazonka.EC2.Types.TransitGatewayMulticastDomainAssociation
import Amazonka.EC2.Types.TransitGatewayMulticastDomainAssociations
import Amazonka.EC2.Types.TransitGatewayMulticastDomainOptions
import Amazonka.EC2.Types.TransitGatewayMulticastGroup
import Amazonka.EC2.Types.TransitGatewayMulticastRegisteredGroupMembers
import Amazonka.EC2.Types.TransitGatewayMulticastRegisteredGroupSources
import Amazonka.EC2.Types.TransitGatewayOptions
import Amazonka.EC2.Types.TransitGatewayPeeringAttachment
import Amazonka.EC2.Types.TransitGatewayPrefixListAttachment
import Amazonka.EC2.Types.TransitGatewayPrefixListReference
import Amazonka.EC2.Types.TransitGatewayPropagation
import Amazonka.EC2.Types.TransitGatewayRequestOptions
import Amazonka.EC2.Types.TransitGatewayRoute
import Amazonka.EC2.Types.TransitGatewayRouteAttachment
import Amazonka.EC2.Types.TransitGatewayRouteTable
import Amazonka.EC2.Types.TransitGatewayRouteTableAssociation
import Amazonka.EC2.Types.TransitGatewayRouteTablePropagation
import Amazonka.EC2.Types.TransitGatewayVpcAttachment
import Amazonka.EC2.Types.TransitGatewayVpcAttachmentOptions
import Amazonka.EC2.Types.TrunkInterfaceAssociation
import Amazonka.EC2.Types.TunnelOption
import Amazonka.EC2.Types.UnsuccessfulInstanceCreditSpecificationItem
import Amazonka.EC2.Types.UnsuccessfulInstanceCreditSpecificationItemError
import Amazonka.EC2.Types.UnsuccessfulItem
import Amazonka.EC2.Types.UnsuccessfulItemError
import Amazonka.EC2.Types.UserBucket
import Amazonka.EC2.Types.UserBucketDetails
import Amazonka.EC2.Types.UserData
import Amazonka.EC2.Types.UserIdGroupPair
import Amazonka.EC2.Types.VCpuInfo
import Amazonka.EC2.Types.ValidationError
import Amazonka.EC2.Types.ValidationWarning
import Amazonka.EC2.Types.VgwTelemetry
import Amazonka.EC2.Types.Volume
import Amazonka.EC2.Types.VolumeAttachment
import Amazonka.EC2.Types.VolumeDetail
import Amazonka.EC2.Types.VolumeModification
import Amazonka.EC2.Types.VolumeStatusAction
import Amazonka.EC2.Types.VolumeStatusAttachmentStatus
import Amazonka.EC2.Types.VolumeStatusDetails
import Amazonka.EC2.Types.VolumeStatusEvent
import Amazonka.EC2.Types.VolumeStatusInfo
import Amazonka.EC2.Types.VolumeStatusItem
import Amazonka.EC2.Types.Vpc
import Amazonka.EC2.Types.VpcAttachment
import Amazonka.EC2.Types.VpcCidrBlockAssociation
import Amazonka.EC2.Types.VpcCidrBlockState
import Amazonka.EC2.Types.VpcClassicLink
import Amazonka.EC2.Types.VpcEndpoint
import Amazonka.EC2.Types.VpcEndpointConnection
import Amazonka.EC2.Types.VpcIpv6CidrBlockAssociation
import Amazonka.EC2.Types.VpcPeeringConnection
import Amazonka.EC2.Types.VpcPeeringConnectionOptionsDescription
import Amazonka.EC2.Types.VpcPeeringConnectionStateReason
import Amazonka.EC2.Types.VpcPeeringConnectionVpcInfo
import Amazonka.EC2.Types.VpnConnection
import Amazonka.EC2.Types.VpnConnectionDeviceType
import Amazonka.EC2.Types.VpnConnectionOptions
import Amazonka.EC2.Types.VpnConnectionOptionsSpecification
import Amazonka.EC2.Types.VpnGateway
import Amazonka.EC2.Types.VpnStaticRoute
import Amazonka.EC2.Types.VpnTunnelOptionsSpecification
import Amazonka.EC2.UnassignIpv6Addresses
import Amazonka.EC2.UnassignPrivateIpAddresses
import Amazonka.EC2.UnmonitorInstances
import Amazonka.EC2.UpdateSecurityGroupRuleDescriptionsEgress
import Amazonka.EC2.UpdateSecurityGroupRuleDescriptionsIngress
import Amazonka.EC2.WithdrawByoipCidr
