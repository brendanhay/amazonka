{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EC2.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Lens
  ( -- * Operations

    -- ** AcceptAddressTransfer
    acceptAddressTransfer_dryRun,
    acceptAddressTransfer_tagSpecifications,
    acceptAddressTransfer_address,
    acceptAddressTransferResponse_addressTransfer,
    acceptAddressTransferResponse_httpStatus,

    -- ** AcceptReservedInstancesExchangeQuote
    acceptReservedInstancesExchangeQuote_dryRun,
    acceptReservedInstancesExchangeQuote_targetConfigurations,
    acceptReservedInstancesExchangeQuote_reservedInstanceIds,
    acceptReservedInstancesExchangeQuoteResponse_exchangeId,
    acceptReservedInstancesExchangeQuoteResponse_httpStatus,

    -- ** AcceptTransitGatewayMulticastDomainAssociations
    acceptTransitGatewayMulticastDomainAssociations_transitGatewayAttachmentId,
    acceptTransitGatewayMulticastDomainAssociations_dryRun,
    acceptTransitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId,
    acceptTransitGatewayMulticastDomainAssociations_subnetIds,
    acceptTransitGatewayMulticastDomainAssociationsResponse_associations,
    acceptTransitGatewayMulticastDomainAssociationsResponse_httpStatus,

    -- ** AcceptTransitGatewayPeeringAttachment
    acceptTransitGatewayPeeringAttachment_dryRun,
    acceptTransitGatewayPeeringAttachment_transitGatewayAttachmentId,
    acceptTransitGatewayPeeringAttachmentResponse_transitGatewayPeeringAttachment,
    acceptTransitGatewayPeeringAttachmentResponse_httpStatus,

    -- ** AcceptTransitGatewayVpcAttachment
    acceptTransitGatewayVpcAttachment_dryRun,
    acceptTransitGatewayVpcAttachment_transitGatewayAttachmentId,
    acceptTransitGatewayVpcAttachmentResponse_transitGatewayVpcAttachment,
    acceptTransitGatewayVpcAttachmentResponse_httpStatus,

    -- ** AcceptVpcEndpointConnections
    acceptVpcEndpointConnections_dryRun,
    acceptVpcEndpointConnections_serviceId,
    acceptVpcEndpointConnections_vpcEndpointIds,
    acceptVpcEndpointConnectionsResponse_unsuccessful,
    acceptVpcEndpointConnectionsResponse_httpStatus,

    -- ** AcceptVpcPeeringConnection
    acceptVpcPeeringConnection_vpcPeeringConnectionId,
    acceptVpcPeeringConnection_dryRun,
    acceptVpcPeeringConnectionResponse_vpcPeeringConnection,
    acceptVpcPeeringConnectionResponse_httpStatus,

    -- ** AdvertiseByoipCidr
    advertiseByoipCidr_dryRun,
    advertiseByoipCidr_cidr,
    advertiseByoipCidrResponse_byoipCidr,
    advertiseByoipCidrResponse_httpStatus,

    -- ** AllocateAddress
    allocateAddress_networkBorderGroup,
    allocateAddress_domain,
    allocateAddress_customerOwnedIpv4Pool,
    allocateAddress_dryRun,
    allocateAddress_address,
    allocateAddress_tagSpecifications,
    allocateAddress_publicIpv4Pool,
    allocateAddressResponse_allocationId,
    allocateAddressResponse_networkBorderGroup,
    allocateAddressResponse_domain,
    allocateAddressResponse_carrierIp,
    allocateAddressResponse_customerOwnedIpv4Pool,
    allocateAddressResponse_publicIp,
    allocateAddressResponse_customerOwnedIp,
    allocateAddressResponse_publicIpv4Pool,
    allocateAddressResponse_httpStatus,

    -- ** AllocateHosts
    allocateHosts_autoPlacement,
    allocateHosts_clientToken,
    allocateHosts_outpostArn,
    allocateHosts_hostRecovery,
    allocateHosts_instanceType,
    allocateHosts_instanceFamily,
    allocateHosts_tagSpecifications,
    allocateHosts_availabilityZone,
    allocateHosts_quantity,
    allocateHostsResponse_hostIds,
    allocateHostsResponse_httpStatus,

    -- ** AllocateIpamPoolCidr
    allocateIpamPoolCidr_clientToken,
    allocateIpamPoolCidr_previewNextCidr,
    allocateIpamPoolCidr_cidr,
    allocateIpamPoolCidr_netmaskLength,
    allocateIpamPoolCidr_description,
    allocateIpamPoolCidr_dryRun,
    allocateIpamPoolCidr_disallowedCidrs,
    allocateIpamPoolCidr_ipamPoolId,
    allocateIpamPoolCidrResponse_ipamPoolAllocation,
    allocateIpamPoolCidrResponse_httpStatus,

    -- ** ApplySecurityGroupsToClientVpnTargetNetwork
    applySecurityGroupsToClientVpnTargetNetwork_dryRun,
    applySecurityGroupsToClientVpnTargetNetwork_clientVpnEndpointId,
    applySecurityGroupsToClientVpnTargetNetwork_vpcId,
    applySecurityGroupsToClientVpnTargetNetwork_securityGroupIds,
    applySecurityGroupsToClientVpnTargetNetworkResponse_securityGroupIds,
    applySecurityGroupsToClientVpnTargetNetworkResponse_httpStatus,

    -- ** AssignIpv6Addresses
    assignIpv6Addresses_ipv6AddressCount,
    assignIpv6Addresses_ipv6PrefixCount,
    assignIpv6Addresses_ipv6Prefixes,
    assignIpv6Addresses_ipv6Addresses,
    assignIpv6Addresses_networkInterfaceId,
    assignIpv6AddressesResponse_assignedIpv6Prefixes,
    assignIpv6AddressesResponse_networkInterfaceId,
    assignIpv6AddressesResponse_assignedIpv6Addresses,
    assignIpv6AddressesResponse_httpStatus,

    -- ** AssignPrivateIpAddresses
    assignPrivateIpAddresses_ipv4PrefixCount,
    assignPrivateIpAddresses_privateIpAddresses,
    assignPrivateIpAddresses_allowReassignment,
    assignPrivateIpAddresses_ipv4Prefixes,
    assignPrivateIpAddresses_secondaryPrivateIpAddressCount,
    assignPrivateIpAddresses_networkInterfaceId,
    assignPrivateIpAddressesResponse_assignedIpv4Prefixes,
    assignPrivateIpAddressesResponse_assignedPrivateIpAddresses,
    assignPrivateIpAddressesResponse_networkInterfaceId,
    assignPrivateIpAddressesResponse_httpStatus,

    -- ** AssociateAddress
    associateAddress_allowReassociation,
    associateAddress_allocationId,
    associateAddress_publicIp,
    associateAddress_dryRun,
    associateAddress_instanceId,
    associateAddress_networkInterfaceId,
    associateAddress_privateIpAddress,
    associateAddressResponse_associationId,
    associateAddressResponse_httpStatus,

    -- ** AssociateClientVpnTargetNetwork
    associateClientVpnTargetNetwork_clientToken,
    associateClientVpnTargetNetwork_dryRun,
    associateClientVpnTargetNetwork_clientVpnEndpointId,
    associateClientVpnTargetNetwork_subnetId,
    associateClientVpnTargetNetworkResponse_status,
    associateClientVpnTargetNetworkResponse_associationId,
    associateClientVpnTargetNetworkResponse_httpStatus,

    -- ** AssociateDhcpOptions
    associateDhcpOptions_dryRun,
    associateDhcpOptions_dhcpOptionsId,
    associateDhcpOptions_vpcId,

    -- ** AssociateEnclaveCertificateIamRole
    associateEnclaveCertificateIamRole_roleArn,
    associateEnclaveCertificateIamRole_dryRun,
    associateEnclaveCertificateIamRole_certificateArn,
    associateEnclaveCertificateIamRoleResponse_certificateS3ObjectKey,
    associateEnclaveCertificateIamRoleResponse_encryptionKmsKeyId,
    associateEnclaveCertificateIamRoleResponse_certificateS3BucketName,
    associateEnclaveCertificateIamRoleResponse_httpStatus,

    -- ** AssociateIamInstanceProfile
    associateIamInstanceProfile_iamInstanceProfile,
    associateIamInstanceProfile_instanceId,
    associateIamInstanceProfileResponse_iamInstanceProfileAssociation,
    associateIamInstanceProfileResponse_httpStatus,

    -- ** AssociateInstanceEventWindow
    associateInstanceEventWindow_dryRun,
    associateInstanceEventWindow_instanceEventWindowId,
    associateInstanceEventWindow_associationTarget,
    associateInstanceEventWindowResponse_instanceEventWindow,
    associateInstanceEventWindowResponse_httpStatus,

    -- ** AssociateRouteTable
    associateRouteTable_subnetId,
    associateRouteTable_dryRun,
    associateRouteTable_gatewayId,
    associateRouteTable_routeTableId,
    associateRouteTableResponse_associationState,
    associateRouteTableResponse_associationId,
    associateRouteTableResponse_httpStatus,

    -- ** AssociateSubnetCidrBlock
    associateSubnetCidrBlock_ipv6CidrBlock,
    associateSubnetCidrBlock_subnetId,
    associateSubnetCidrBlockResponse_subnetId,
    associateSubnetCidrBlockResponse_ipv6CidrBlockAssociation,
    associateSubnetCidrBlockResponse_httpStatus,

    -- ** AssociateTransitGatewayMulticastDomain
    associateTransitGatewayMulticastDomain_transitGatewayAttachmentId,
    associateTransitGatewayMulticastDomain_dryRun,
    associateTransitGatewayMulticastDomain_transitGatewayMulticastDomainId,
    associateTransitGatewayMulticastDomain_subnetIds,
    associateTransitGatewayMulticastDomainResponse_associations,
    associateTransitGatewayMulticastDomainResponse_httpStatus,

    -- ** AssociateTransitGatewayPolicyTable
    associateTransitGatewayPolicyTable_dryRun,
    associateTransitGatewayPolicyTable_transitGatewayPolicyTableId,
    associateTransitGatewayPolicyTable_transitGatewayAttachmentId,
    associateTransitGatewayPolicyTableResponse_association,
    associateTransitGatewayPolicyTableResponse_httpStatus,

    -- ** AssociateTransitGatewayRouteTable
    associateTransitGatewayRouteTable_dryRun,
    associateTransitGatewayRouteTable_transitGatewayRouteTableId,
    associateTransitGatewayRouteTable_transitGatewayAttachmentId,
    associateTransitGatewayRouteTableResponse_association,
    associateTransitGatewayRouteTableResponse_httpStatus,

    -- ** AssociateTrunkInterface
    associateTrunkInterface_clientToken,
    associateTrunkInterface_dryRun,
    associateTrunkInterface_vlanId,
    associateTrunkInterface_greKey,
    associateTrunkInterface_branchInterfaceId,
    associateTrunkInterface_trunkInterfaceId,
    associateTrunkInterfaceResponse_clientToken,
    associateTrunkInterfaceResponse_interfaceAssociation,
    associateTrunkInterfaceResponse_httpStatus,

    -- ** AssociateVpcCidrBlock
    associateVpcCidrBlock_ipv6CidrBlockNetworkBorderGroup,
    associateVpcCidrBlock_ipv4IpamPoolId,
    associateVpcCidrBlock_ipv6Pool,
    associateVpcCidrBlock_ipv4NetmaskLength,
    associateVpcCidrBlock_ipv6IpamPoolId,
    associateVpcCidrBlock_amazonProvidedIpv6CidrBlock,
    associateVpcCidrBlock_ipv6NetmaskLength,
    associateVpcCidrBlock_cidrBlock,
    associateVpcCidrBlock_ipv6CidrBlock,
    associateVpcCidrBlock_vpcId,
    associateVpcCidrBlockResponse_cidrBlockAssociation,
    associateVpcCidrBlockResponse_ipv6CidrBlockAssociation,
    associateVpcCidrBlockResponse_vpcId,
    associateVpcCidrBlockResponse_httpStatus,

    -- ** AttachClassicLinkVpc
    attachClassicLinkVpc_dryRun,
    attachClassicLinkVpc_groups,
    attachClassicLinkVpc_instanceId,
    attachClassicLinkVpc_vpcId,
    attachClassicLinkVpcResponse_return,
    attachClassicLinkVpcResponse_httpStatus,

    -- ** AttachInternetGateway
    attachInternetGateway_dryRun,
    attachInternetGateway_internetGatewayId,
    attachInternetGateway_vpcId,

    -- ** AttachNetworkInterface
    attachNetworkInterface_networkCardIndex,
    attachNetworkInterface_dryRun,
    attachNetworkInterface_deviceIndex,
    attachNetworkInterface_instanceId,
    attachNetworkInterface_networkInterfaceId,
    attachNetworkInterfaceResponse_networkCardIndex,
    attachNetworkInterfaceResponse_attachmentId,
    attachNetworkInterfaceResponse_httpStatus,

    -- ** AttachVolume
    attachVolume_dryRun,
    attachVolume_device,
    attachVolume_instanceId,
    attachVolume_volumeId,
    volumeAttachment_deleteOnTermination,
    volumeAttachment_device,
    volumeAttachment_state,
    volumeAttachment_instanceId,
    volumeAttachment_attachTime,
    volumeAttachment_volumeId,

    -- ** AttachVpnGateway
    attachVpnGateway_dryRun,
    attachVpnGateway_vpcId,
    attachVpnGateway_vpnGatewayId,
    attachVpnGatewayResponse_vpcAttachment,
    attachVpnGatewayResponse_httpStatus,

    -- ** AuthorizeClientVpnIngress
    authorizeClientVpnIngress_clientToken,
    authorizeClientVpnIngress_authorizeAllGroups,
    authorizeClientVpnIngress_description,
    authorizeClientVpnIngress_dryRun,
    authorizeClientVpnIngress_accessGroupId,
    authorizeClientVpnIngress_clientVpnEndpointId,
    authorizeClientVpnIngress_targetNetworkCidr,
    authorizeClientVpnIngressResponse_status,
    authorizeClientVpnIngressResponse_httpStatus,

    -- ** AuthorizeSecurityGroupEgress
    authorizeSecurityGroupEgress_sourceSecurityGroupName,
    authorizeSecurityGroupEgress_toPort,
    authorizeSecurityGroupEgress_ipPermissions,
    authorizeSecurityGroupEgress_ipProtocol,
    authorizeSecurityGroupEgress_dryRun,
    authorizeSecurityGroupEgress_cidrIp,
    authorizeSecurityGroupEgress_tagSpecifications,
    authorizeSecurityGroupEgress_sourceSecurityGroupOwnerId,
    authorizeSecurityGroupEgress_fromPort,
    authorizeSecurityGroupEgress_groupId,
    authorizeSecurityGroupEgressResponse_securityGroupRules,
    authorizeSecurityGroupEgressResponse_return,
    authorizeSecurityGroupEgressResponse_httpStatus,

    -- ** AuthorizeSecurityGroupIngress
    authorizeSecurityGroupIngress_sourceSecurityGroupName,
    authorizeSecurityGroupIngress_toPort,
    authorizeSecurityGroupIngress_ipPermissions,
    authorizeSecurityGroupIngress_ipProtocol,
    authorizeSecurityGroupIngress_groupName,
    authorizeSecurityGroupIngress_dryRun,
    authorizeSecurityGroupIngress_cidrIp,
    authorizeSecurityGroupIngress_tagSpecifications,
    authorizeSecurityGroupIngress_sourceSecurityGroupOwnerId,
    authorizeSecurityGroupIngress_fromPort,
    authorizeSecurityGroupIngress_groupId,
    authorizeSecurityGroupIngressResponse_securityGroupRules,
    authorizeSecurityGroupIngressResponse_return,
    authorizeSecurityGroupIngressResponse_httpStatus,

    -- ** BundleInstance
    bundleInstance_dryRun,
    bundleInstance_instanceId,
    bundleInstance_storage,
    bundleInstanceResponse_bundleTask,
    bundleInstanceResponse_httpStatus,

    -- ** CancelBundleTask
    cancelBundleTask_dryRun,
    cancelBundleTask_bundleId,
    cancelBundleTaskResponse_bundleTask,
    cancelBundleTaskResponse_httpStatus,

    -- ** CancelCapacityReservation
    cancelCapacityReservation_dryRun,
    cancelCapacityReservation_capacityReservationId,
    cancelCapacityReservationResponse_return,
    cancelCapacityReservationResponse_httpStatus,

    -- ** CancelCapacityReservationFleets
    cancelCapacityReservationFleets_dryRun,
    cancelCapacityReservationFleets_capacityReservationFleetIds,
    cancelCapacityReservationFleetsResponse_failedFleetCancellations,
    cancelCapacityReservationFleetsResponse_successfulFleetCancellations,
    cancelCapacityReservationFleetsResponse_httpStatus,

    -- ** CancelConversionTask
    cancelConversionTask_dryRun,
    cancelConversionTask_reasonMessage,
    cancelConversionTask_conversionTaskId,

    -- ** CancelExportTask
    cancelExportTask_exportTaskId,

    -- ** CancelImageLaunchPermission
    cancelImageLaunchPermission_dryRun,
    cancelImageLaunchPermission_imageId,
    cancelImageLaunchPermissionResponse_return,
    cancelImageLaunchPermissionResponse_httpStatus,

    -- ** CancelImportTask
    cancelImportTask_cancelReason,
    cancelImportTask_importTaskId,
    cancelImportTask_dryRun,
    cancelImportTaskResponse_previousState,
    cancelImportTaskResponse_state,
    cancelImportTaskResponse_importTaskId,
    cancelImportTaskResponse_httpStatus,

    -- ** CancelReservedInstancesListing
    cancelReservedInstancesListing_reservedInstancesListingId,
    cancelReservedInstancesListingResponse_reservedInstancesListings,
    cancelReservedInstancesListingResponse_httpStatus,

    -- ** CancelSpotFleetRequests
    cancelSpotFleetRequests_dryRun,
    cancelSpotFleetRequests_spotFleetRequestIds,
    cancelSpotFleetRequests_terminateInstances,
    cancelSpotFleetRequestsResponse_successfulFleetRequests,
    cancelSpotFleetRequestsResponse_unsuccessfulFleetRequests,
    cancelSpotFleetRequestsResponse_httpStatus,

    -- ** CancelSpotInstanceRequests
    cancelSpotInstanceRequests_dryRun,
    cancelSpotInstanceRequests_spotInstanceRequestIds,
    cancelSpotInstanceRequestsResponse_cancelledSpotInstanceRequests,
    cancelSpotInstanceRequestsResponse_httpStatus,

    -- ** ConfirmProductInstance
    confirmProductInstance_dryRun,
    confirmProductInstance_instanceId,
    confirmProductInstance_productCode,
    confirmProductInstanceResponse_ownerId,
    confirmProductInstanceResponse_return,
    confirmProductInstanceResponse_httpStatus,

    -- ** CopyFpgaImage
    copyFpgaImage_clientToken,
    copyFpgaImage_name,
    copyFpgaImage_description,
    copyFpgaImage_dryRun,
    copyFpgaImage_sourceFpgaImageId,
    copyFpgaImage_sourceRegion,
    copyFpgaImageResponse_fpgaImageId,
    copyFpgaImageResponse_httpStatus,

    -- ** CopyImage
    copyImage_clientToken,
    copyImage_destinationOutpostArn,
    copyImage_description,
    copyImage_dryRun,
    copyImage_encrypted,
    copyImage_kmsKeyId,
    copyImage_copyImageTags,
    copyImage_name,
    copyImage_sourceImageId,
    copyImage_sourceRegion,
    copyImageResponse_imageId,
    copyImageResponse_httpStatus,

    -- ** CopySnapshot
    copySnapshot_destinationOutpostArn,
    copySnapshot_description,
    copySnapshot_dryRun,
    copySnapshot_encrypted,
    copySnapshot_kmsKeyId,
    copySnapshot_tagSpecifications,
    copySnapshot_destinationRegion,
    copySnapshot_presignedUrl,
    copySnapshot_sourceRegion,
    copySnapshot_sourceSnapshotId,
    copySnapshotResponse_tags,
    copySnapshotResponse_snapshotId,
    copySnapshotResponse_httpStatus,

    -- ** CreateCapacityReservation
    createCapacityReservation_ebsOptimized,
    createCapacityReservation_ephemeralStorage,
    createCapacityReservation_clientToken,
    createCapacityReservation_placementGroupArn,
    createCapacityReservation_outpostArn,
    createCapacityReservation_endDate,
    createCapacityReservation_availabilityZone,
    createCapacityReservation_dryRun,
    createCapacityReservation_tagSpecifications,
    createCapacityReservation_instanceMatchCriteria,
    createCapacityReservation_endDateType,
    createCapacityReservation_tenancy,
    createCapacityReservation_availabilityZoneId,
    createCapacityReservation_instanceType,
    createCapacityReservation_instancePlatform,
    createCapacityReservation_instanceCount,
    createCapacityReservationResponse_capacityReservation,
    createCapacityReservationResponse_httpStatus,

    -- ** CreateCapacityReservationFleet
    createCapacityReservationFleet_clientToken,
    createCapacityReservationFleet_endDate,
    createCapacityReservationFleet_dryRun,
    createCapacityReservationFleet_allocationStrategy,
    createCapacityReservationFleet_tagSpecifications,
    createCapacityReservationFleet_instanceMatchCriteria,
    createCapacityReservationFleet_tenancy,
    createCapacityReservationFleet_instanceTypeSpecifications,
    createCapacityReservationFleet_totalTargetCapacity,
    createCapacityReservationFleetResponse_tags,
    createCapacityReservationFleetResponse_capacityReservationFleetId,
    createCapacityReservationFleetResponse_endDate,
    createCapacityReservationFleetResponse_fleetCapacityReservations,
    createCapacityReservationFleetResponse_state,
    createCapacityReservationFleetResponse_totalFulfilledCapacity,
    createCapacityReservationFleetResponse_totalTargetCapacity,
    createCapacityReservationFleetResponse_allocationStrategy,
    createCapacityReservationFleetResponse_createTime,
    createCapacityReservationFleetResponse_instanceMatchCriteria,
    createCapacityReservationFleetResponse_tenancy,
    createCapacityReservationFleetResponse_httpStatus,

    -- ** CreateCarrierGateway
    createCarrierGateway_clientToken,
    createCarrierGateway_dryRun,
    createCarrierGateway_tagSpecifications,
    createCarrierGateway_vpcId,
    createCarrierGatewayResponse_carrierGateway,
    createCarrierGatewayResponse_httpStatus,

    -- ** CreateClientVpnEndpoint
    createClientVpnEndpoint_clientToken,
    createClientVpnEndpoint_transportProtocol,
    createClientVpnEndpoint_sessionTimeoutHours,
    createClientVpnEndpoint_securityGroupIds,
    createClientVpnEndpoint_dnsServers,
    createClientVpnEndpoint_description,
    createClientVpnEndpoint_dryRun,
    createClientVpnEndpoint_clientLoginBannerOptions,
    createClientVpnEndpoint_clientConnectOptions,
    createClientVpnEndpoint_splitTunnel,
    createClientVpnEndpoint_vpcId,
    createClientVpnEndpoint_tagSpecifications,
    createClientVpnEndpoint_vpnPort,
    createClientVpnEndpoint_selfServicePortal,
    createClientVpnEndpoint_clientCidrBlock,
    createClientVpnEndpoint_serverCertificateArn,
    createClientVpnEndpoint_authenticationOptions,
    createClientVpnEndpoint_connectionLogOptions,
    createClientVpnEndpointResponse_status,
    createClientVpnEndpointResponse_clientVpnEndpointId,
    createClientVpnEndpointResponse_dnsName,
    createClientVpnEndpointResponse_httpStatus,

    -- ** CreateClientVpnRoute
    createClientVpnRoute_clientToken,
    createClientVpnRoute_description,
    createClientVpnRoute_dryRun,
    createClientVpnRoute_clientVpnEndpointId,
    createClientVpnRoute_destinationCidrBlock,
    createClientVpnRoute_targetVpcSubnetId,
    createClientVpnRouteResponse_status,
    createClientVpnRouteResponse_httpStatus,

    -- ** CreateCoipCidr
    createCoipCidr_dryRun,
    createCoipCidr_cidr,
    createCoipCidr_coipPoolId,
    createCoipCidrResponse_coipCidr,
    createCoipCidrResponse_httpStatus,

    -- ** CreateCoipPool
    createCoipPool_dryRun,
    createCoipPool_tagSpecifications,
    createCoipPool_localGatewayRouteTableId,
    createCoipPoolResponse_coipPool,
    createCoipPoolResponse_httpStatus,

    -- ** CreateCustomerGateway
    createCustomerGateway_deviceName,
    createCustomerGateway_publicIp,
    createCustomerGateway_dryRun,
    createCustomerGateway_certificateArn,
    createCustomerGateway_tagSpecifications,
    createCustomerGateway_ipAddress,
    createCustomerGateway_bgpAsn,
    createCustomerGateway_type,
    createCustomerGatewayResponse_customerGateway,
    createCustomerGatewayResponse_httpStatus,

    -- ** CreateDefaultSubnet
    createDefaultSubnet_ipv6Native,
    createDefaultSubnet_dryRun,
    createDefaultSubnet_availabilityZone,
    createDefaultSubnetResponse_subnet,
    createDefaultSubnetResponse_httpStatus,

    -- ** CreateDefaultVpc
    createDefaultVpc_dryRun,
    createDefaultVpcResponse_vpc,
    createDefaultVpcResponse_httpStatus,

    -- ** CreateDhcpOptions
    createDhcpOptions_dryRun,
    createDhcpOptions_tagSpecifications,
    createDhcpOptions_dhcpConfigurations,
    createDhcpOptionsResponse_dhcpOptions,
    createDhcpOptionsResponse_httpStatus,

    -- ** CreateEgressOnlyInternetGateway
    createEgressOnlyInternetGateway_clientToken,
    createEgressOnlyInternetGateway_dryRun,
    createEgressOnlyInternetGateway_tagSpecifications,
    createEgressOnlyInternetGateway_vpcId,
    createEgressOnlyInternetGatewayResponse_clientToken,
    createEgressOnlyInternetGatewayResponse_egressOnlyInternetGateway,
    createEgressOnlyInternetGatewayResponse_httpStatus,

    -- ** CreateFleet
    createFleet_excessCapacityTerminationPolicy,
    createFleet_clientToken,
    createFleet_type,
    createFleet_onDemandOptions,
    createFleet_context,
    createFleet_validFrom,
    createFleet_replaceUnhealthyInstances,
    createFleet_dryRun,
    createFleet_spotOptions,
    createFleet_terminateInstancesWithExpiration,
    createFleet_validUntil,
    createFleet_tagSpecifications,
    createFleet_launchTemplateConfigs,
    createFleet_targetCapacitySpecification,
    createFleetResponse_instances,
    createFleetResponse_fleetId,
    createFleetResponse_errors,
    createFleetResponse_httpStatus,

    -- ** CreateFlowLogs
    createFlowLogs_destinationOptions,
    createFlowLogs_clientToken,
    createFlowLogs_trafficType,
    createFlowLogs_deliverLogsPermissionArn,
    createFlowLogs_logFormat,
    createFlowLogs_dryRun,
    createFlowLogs_logDestination,
    createFlowLogs_deliverCrossAccountRole,
    createFlowLogs_logDestinationType,
    createFlowLogs_tagSpecifications,
    createFlowLogs_maxAggregationInterval,
    createFlowLogs_logGroupName,
    createFlowLogs_resourceIds,
    createFlowLogs_resourceType,
    createFlowLogsResponse_clientToken,
    createFlowLogsResponse_unsuccessful,
    createFlowLogsResponse_flowLogIds,
    createFlowLogsResponse_httpStatus,

    -- ** CreateFpgaImage
    createFpgaImage_clientToken,
    createFpgaImage_name,
    createFpgaImage_description,
    createFpgaImage_dryRun,
    createFpgaImage_tagSpecifications,
    createFpgaImage_logsStorageLocation,
    createFpgaImage_inputStorageLocation,
    createFpgaImageResponse_fpgaImageGlobalId,
    createFpgaImageResponse_fpgaImageId,
    createFpgaImageResponse_httpStatus,

    -- ** CreateImage
    createImage_blockDeviceMappings,
    createImage_description,
    createImage_dryRun,
    createImage_tagSpecifications,
    createImage_noReboot,
    createImage_instanceId,
    createImage_name,
    createImageResponse_imageId,
    createImageResponse_httpStatus,

    -- ** CreateInstanceEventWindow
    createInstanceEventWindow_name,
    createInstanceEventWindow_dryRun,
    createInstanceEventWindow_timeRanges,
    createInstanceEventWindow_tagSpecifications,
    createInstanceEventWindow_cronExpression,
    createInstanceEventWindowResponse_instanceEventWindow,
    createInstanceEventWindowResponse_httpStatus,

    -- ** CreateInstanceExportTask
    createInstanceExportTask_description,
    createInstanceExportTask_tagSpecifications,
    createInstanceExportTask_exportToS3Task,
    createInstanceExportTask_instanceId,
    createInstanceExportTask_targetEnvironment,
    createInstanceExportTaskResponse_exportTask,
    createInstanceExportTaskResponse_httpStatus,

    -- ** CreateInternetGateway
    createInternetGateway_dryRun,
    createInternetGateway_tagSpecifications,
    createInternetGatewayResponse_internetGateway,
    createInternetGatewayResponse_httpStatus,

    -- ** CreateIpam
    createIpam_clientToken,
    createIpam_description,
    createIpam_dryRun,
    createIpam_operatingRegions,
    createIpam_tagSpecifications,
    createIpamResponse_ipam,
    createIpamResponse_httpStatus,

    -- ** CreateIpamPool
    createIpamPool_clientToken,
    createIpamPool_allocationMaxNetmaskLength,
    createIpamPool_publiclyAdvertisable,
    createIpamPool_locale,
    createIpamPool_sourceIpamPoolId,
    createIpamPool_description,
    createIpamPool_dryRun,
    createIpamPool_allocationResourceTags,
    createIpamPool_allocationMinNetmaskLength,
    createIpamPool_tagSpecifications,
    createIpamPool_allocationDefaultNetmaskLength,
    createIpamPool_awsService,
    createIpamPool_autoImport,
    createIpamPool_ipamScopeId,
    createIpamPool_addressFamily,
    createIpamPoolResponse_ipamPool,
    createIpamPoolResponse_httpStatus,

    -- ** CreateIpamScope
    createIpamScope_clientToken,
    createIpamScope_description,
    createIpamScope_dryRun,
    createIpamScope_tagSpecifications,
    createIpamScope_ipamId,
    createIpamScopeResponse_ipamScope,
    createIpamScopeResponse_httpStatus,

    -- ** CreateKeyPair
    createKeyPair_keyType,
    createKeyPair_dryRun,
    createKeyPair_keyFormat,
    createKeyPair_tagSpecifications,
    createKeyPair_keyName,
    createKeyPairResponse_tags,
    createKeyPairResponse_keyPairId,
    createKeyPairResponse_httpStatus,
    createKeyPairResponse_keyName,
    createKeyPairResponse_keyFingerprint,
    createKeyPairResponse_keyMaterial,

    -- ** CreateLaunchTemplate
    createLaunchTemplate_clientToken,
    createLaunchTemplate_dryRun,
    createLaunchTemplate_versionDescription,
    createLaunchTemplate_tagSpecifications,
    createLaunchTemplate_launchTemplateName,
    createLaunchTemplate_launchTemplateData,
    createLaunchTemplateResponse_launchTemplate,
    createLaunchTemplateResponse_warning,
    createLaunchTemplateResponse_httpStatus,

    -- ** CreateLaunchTemplateVersion
    createLaunchTemplateVersion_clientToken,
    createLaunchTemplateVersion_sourceVersion,
    createLaunchTemplateVersion_dryRun,
    createLaunchTemplateVersion_launchTemplateId,
    createLaunchTemplateVersion_versionDescription,
    createLaunchTemplateVersion_launchTemplateName,
    createLaunchTemplateVersion_launchTemplateData,
    createLaunchTemplateVersionResponse_launchTemplateVersion,
    createLaunchTemplateVersionResponse_warning,
    createLaunchTemplateVersionResponse_httpStatus,

    -- ** CreateLocalGatewayRoute
    createLocalGatewayRoute_dryRun,
    createLocalGatewayRoute_networkInterfaceId,
    createLocalGatewayRoute_localGatewayVirtualInterfaceGroupId,
    createLocalGatewayRoute_destinationCidrBlock,
    createLocalGatewayRoute_localGatewayRouteTableId,
    createLocalGatewayRouteResponse_route,
    createLocalGatewayRouteResponse_httpStatus,

    -- ** CreateLocalGatewayRouteTable
    createLocalGatewayRouteTable_dryRun,
    createLocalGatewayRouteTable_mode,
    createLocalGatewayRouteTable_tagSpecifications,
    createLocalGatewayRouteTable_localGatewayId,
    createLocalGatewayRouteTableResponse_localGatewayRouteTable,
    createLocalGatewayRouteTableResponse_httpStatus,

    -- ** CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation
    createLocalGatewayRouteTableVirtualInterfaceGroupAssociation_dryRun,
    createLocalGatewayRouteTableVirtualInterfaceGroupAssociation_tagSpecifications,
    createLocalGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableId,
    createLocalGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayVirtualInterfaceGroupId,
    createLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse_localGatewayRouteTableVirtualInterfaceGroupAssociation,
    createLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse_httpStatus,

    -- ** CreateLocalGatewayRouteTableVpcAssociation
    createLocalGatewayRouteTableVpcAssociation_dryRun,
    createLocalGatewayRouteTableVpcAssociation_tagSpecifications,
    createLocalGatewayRouteTableVpcAssociation_localGatewayRouteTableId,
    createLocalGatewayRouteTableVpcAssociation_vpcId,
    createLocalGatewayRouteTableVpcAssociationResponse_localGatewayRouteTableVpcAssociation,
    createLocalGatewayRouteTableVpcAssociationResponse_httpStatus,

    -- ** CreateManagedPrefixList
    createManagedPrefixList_clientToken,
    createManagedPrefixList_dryRun,
    createManagedPrefixList_entries,
    createManagedPrefixList_tagSpecifications,
    createManagedPrefixList_prefixListName,
    createManagedPrefixList_maxEntries,
    createManagedPrefixList_addressFamily,
    createManagedPrefixListResponse_prefixList,
    createManagedPrefixListResponse_httpStatus,

    -- ** CreateNatGateway
    createNatGateway_clientToken,
    createNatGateway_allocationId,
    createNatGateway_dryRun,
    createNatGateway_privateIpAddress,
    createNatGateway_connectivityType,
    createNatGateway_tagSpecifications,
    createNatGateway_subnetId,
    createNatGatewayResponse_clientToken,
    createNatGatewayResponse_natGateway,
    createNatGatewayResponse_httpStatus,

    -- ** CreateNetworkAcl
    createNetworkAcl_dryRun,
    createNetworkAcl_tagSpecifications,
    createNetworkAcl_vpcId,
    createNetworkAclResponse_networkAcl,
    createNetworkAclResponse_httpStatus,

    -- ** CreateNetworkAclEntry
    createNetworkAclEntry_icmpTypeCode,
    createNetworkAclEntry_portRange,
    createNetworkAclEntry_dryRun,
    createNetworkAclEntry_cidrBlock,
    createNetworkAclEntry_ipv6CidrBlock,
    createNetworkAclEntry_egress,
    createNetworkAclEntry_networkAclId,
    createNetworkAclEntry_protocol,
    createNetworkAclEntry_ruleAction,
    createNetworkAclEntry_ruleNumber,

    -- ** CreateNetworkInsightsAccessScope
    createNetworkInsightsAccessScope_excludePaths,
    createNetworkInsightsAccessScope_matchPaths,
    createNetworkInsightsAccessScope_dryRun,
    createNetworkInsightsAccessScope_tagSpecifications,
    createNetworkInsightsAccessScope_clientToken,
    createNetworkInsightsAccessScopeResponse_networkInsightsAccessScopeContent,
    createNetworkInsightsAccessScopeResponse_networkInsightsAccessScope,
    createNetworkInsightsAccessScopeResponse_httpStatus,

    -- ** CreateNetworkInsightsPath
    createNetworkInsightsPath_destinationIp,
    createNetworkInsightsPath_sourceIp,
    createNetworkInsightsPath_destinationPort,
    createNetworkInsightsPath_dryRun,
    createNetworkInsightsPath_tagSpecifications,
    createNetworkInsightsPath_source,
    createNetworkInsightsPath_destination,
    createNetworkInsightsPath_protocol,
    createNetworkInsightsPath_clientToken,
    createNetworkInsightsPathResponse_networkInsightsPath,
    createNetworkInsightsPathResponse_httpStatus,

    -- ** CreateNetworkInterface
    createNetworkInterface_ipv4PrefixCount,
    createNetworkInterface_interfaceType,
    createNetworkInterface_clientToken,
    createNetworkInterface_privateIpAddresses,
    createNetworkInterface_description,
    createNetworkInterface_dryRun,
    createNetworkInterface_ipv6AddressCount,
    createNetworkInterface_ipv4Prefixes,
    createNetworkInterface_privateIpAddress,
    createNetworkInterface_ipv6PrefixCount,
    createNetworkInterface_secondaryPrivateIpAddressCount,
    createNetworkInterface_ipv6Prefixes,
    createNetworkInterface_tagSpecifications,
    createNetworkInterface_groups,
    createNetworkInterface_ipv6Addresses,
    createNetworkInterface_subnetId,
    createNetworkInterfaceResponse_clientToken,
    createNetworkInterfaceResponse_networkInterface,
    createNetworkInterfaceResponse_httpStatus,

    -- ** CreateNetworkInterfacePermission
    createNetworkInterfacePermission_awsAccountId,
    createNetworkInterfacePermission_dryRun,
    createNetworkInterfacePermission_awsService,
    createNetworkInterfacePermission_networkInterfaceId,
    createNetworkInterfacePermission_permission,
    createNetworkInterfacePermissionResponse_interfacePermission,
    createNetworkInterfacePermissionResponse_httpStatus,

    -- ** CreatePlacementGroup
    createPlacementGroup_partitionCount,
    createPlacementGroup_spreadLevel,
    createPlacementGroup_groupName,
    createPlacementGroup_dryRun,
    createPlacementGroup_strategy,
    createPlacementGroup_tagSpecifications,
    createPlacementGroupResponse_placementGroup,
    createPlacementGroupResponse_httpStatus,

    -- ** CreatePublicIpv4Pool
    createPublicIpv4Pool_dryRun,
    createPublicIpv4Pool_tagSpecifications,
    createPublicIpv4PoolResponse_poolId,
    createPublicIpv4PoolResponse_httpStatus,

    -- ** CreateReplaceRootVolumeTask
    createReplaceRootVolumeTask_clientToken,
    createReplaceRootVolumeTask_snapshotId,
    createReplaceRootVolumeTask_dryRun,
    createReplaceRootVolumeTask_deleteReplacedRootVolume,
    createReplaceRootVolumeTask_tagSpecifications,
    createReplaceRootVolumeTask_imageId,
    createReplaceRootVolumeTask_instanceId,
    createReplaceRootVolumeTaskResponse_replaceRootVolumeTask,
    createReplaceRootVolumeTaskResponse_httpStatus,

    -- ** CreateReservedInstancesListing
    createReservedInstancesListing_clientToken,
    createReservedInstancesListing_instanceCount,
    createReservedInstancesListing_priceSchedules,
    createReservedInstancesListing_reservedInstancesId,
    createReservedInstancesListingResponse_reservedInstancesListings,
    createReservedInstancesListingResponse_httpStatus,

    -- ** CreateRestoreImageTask
    createRestoreImageTask_name,
    createRestoreImageTask_dryRun,
    createRestoreImageTask_tagSpecifications,
    createRestoreImageTask_bucket,
    createRestoreImageTask_objectKey,
    createRestoreImageTaskResponse_imageId,
    createRestoreImageTaskResponse_httpStatus,

    -- ** CreateRoute
    createRoute_localGatewayId,
    createRoute_destinationPrefixListId,
    createRoute_carrierGatewayId,
    createRoute_transitGatewayId,
    createRoute_natGatewayId,
    createRoute_vpcPeeringConnectionId,
    createRoute_vpcEndpointId,
    createRoute_dryRun,
    createRoute_destinationCidrBlock,
    createRoute_coreNetworkArn,
    createRoute_instanceId,
    createRoute_egressOnlyInternetGatewayId,
    createRoute_networkInterfaceId,
    createRoute_gatewayId,
    createRoute_destinationIpv6CidrBlock,
    createRoute_routeTableId,
    createRouteResponse_return,
    createRouteResponse_httpStatus,

    -- ** CreateRouteTable
    createRouteTable_dryRun,
    createRouteTable_tagSpecifications,
    createRouteTable_vpcId,
    createRouteTableResponse_routeTable,
    createRouteTableResponse_httpStatus,

    -- ** CreateSecurityGroup
    createSecurityGroup_dryRun,
    createSecurityGroup_vpcId,
    createSecurityGroup_tagSpecifications,
    createSecurityGroup_description,
    createSecurityGroup_groupName,
    createSecurityGroupResponse_tags,
    createSecurityGroupResponse_httpStatus,
    createSecurityGroupResponse_groupId,

    -- ** CreateSnapshot
    createSnapshot_outpostArn,
    createSnapshot_description,
    createSnapshot_dryRun,
    createSnapshot_tagSpecifications,
    createSnapshot_volumeId,
    snapshot_tags,
    snapshot_ownerAlias,
    snapshot_outpostArn,
    snapshot_dataEncryptionKeyId,
    snapshot_restoreExpiryTime,
    snapshot_kmsKeyId,
    snapshot_stateMessage,
    snapshot_storageTier,
    snapshot_snapshotId,
    snapshot_ownerId,
    snapshot_volumeId,
    snapshot_volumeSize,
    snapshot_description,
    snapshot_startTime,
    snapshot_progress,
    snapshot_state,
    snapshot_encrypted,

    -- ** CreateSnapshots
    createSnapshots_copyTagsFromSource,
    createSnapshots_outpostArn,
    createSnapshots_description,
    createSnapshots_dryRun,
    createSnapshots_tagSpecifications,
    createSnapshots_instanceSpecification,
    createSnapshotsResponse_snapshots,
    createSnapshotsResponse_httpStatus,

    -- ** CreateSpotDatafeedSubscription
    createSpotDatafeedSubscription_dryRun,
    createSpotDatafeedSubscription_prefix,
    createSpotDatafeedSubscription_bucket,
    createSpotDatafeedSubscriptionResponse_spotDatafeedSubscription,
    createSpotDatafeedSubscriptionResponse_httpStatus,

    -- ** CreateStoreImageTask
    createStoreImageTask_dryRun,
    createStoreImageTask_s3ObjectTags,
    createStoreImageTask_imageId,
    createStoreImageTask_bucket,
    createStoreImageTaskResponse_objectKey,
    createStoreImageTaskResponse_httpStatus,

    -- ** CreateSubnet
    createSubnet_outpostArn,
    createSubnet_ipv6Native,
    createSubnet_availabilityZone,
    createSubnet_dryRun,
    createSubnet_cidrBlock,
    createSubnet_tagSpecifications,
    createSubnet_ipv6CidrBlock,
    createSubnet_availabilityZoneId,
    createSubnet_vpcId,
    createSubnetResponse_subnet,
    createSubnetResponse_httpStatus,

    -- ** CreateSubnetCidrReservation
    createSubnetCidrReservation_description,
    createSubnetCidrReservation_dryRun,
    createSubnetCidrReservation_tagSpecifications,
    createSubnetCidrReservation_subnetId,
    createSubnetCidrReservation_cidr,
    createSubnetCidrReservation_reservationType,
    createSubnetCidrReservationResponse_subnetCidrReservation,
    createSubnetCidrReservationResponse_httpStatus,

    -- ** CreateTags
    createTags_dryRun,
    createTags_resources,
    createTags_tags,

    -- ** CreateTrafficMirrorFilter
    createTrafficMirrorFilter_clientToken,
    createTrafficMirrorFilter_description,
    createTrafficMirrorFilter_dryRun,
    createTrafficMirrorFilter_tagSpecifications,
    createTrafficMirrorFilterResponse_trafficMirrorFilter,
    createTrafficMirrorFilterResponse_clientToken,
    createTrafficMirrorFilterResponse_httpStatus,

    -- ** CreateTrafficMirrorFilterRule
    createTrafficMirrorFilterRule_clientToken,
    createTrafficMirrorFilterRule_description,
    createTrafficMirrorFilterRule_dryRun,
    createTrafficMirrorFilterRule_destinationPortRange,
    createTrafficMirrorFilterRule_protocol,
    createTrafficMirrorFilterRule_sourcePortRange,
    createTrafficMirrorFilterRule_trafficMirrorFilterId,
    createTrafficMirrorFilterRule_trafficDirection,
    createTrafficMirrorFilterRule_ruleNumber,
    createTrafficMirrorFilterRule_ruleAction,
    createTrafficMirrorFilterRule_destinationCidrBlock,
    createTrafficMirrorFilterRule_sourceCidrBlock,
    createTrafficMirrorFilterRuleResponse_clientToken,
    createTrafficMirrorFilterRuleResponse_trafficMirrorFilterRule,
    createTrafficMirrorFilterRuleResponse_httpStatus,

    -- ** CreateTrafficMirrorSession
    createTrafficMirrorSession_clientToken,
    createTrafficMirrorSession_description,
    createTrafficMirrorSession_packetLength,
    createTrafficMirrorSession_dryRun,
    createTrafficMirrorSession_virtualNetworkId,
    createTrafficMirrorSession_tagSpecifications,
    createTrafficMirrorSession_networkInterfaceId,
    createTrafficMirrorSession_trafficMirrorTargetId,
    createTrafficMirrorSession_trafficMirrorFilterId,
    createTrafficMirrorSession_sessionNumber,
    createTrafficMirrorSessionResponse_clientToken,
    createTrafficMirrorSessionResponse_trafficMirrorSession,
    createTrafficMirrorSessionResponse_httpStatus,

    -- ** CreateTrafficMirrorTarget
    createTrafficMirrorTarget_clientToken,
    createTrafficMirrorTarget_networkLoadBalancerArn,
    createTrafficMirrorTarget_description,
    createTrafficMirrorTarget_dryRun,
    createTrafficMirrorTarget_networkInterfaceId,
    createTrafficMirrorTarget_gatewayLoadBalancerEndpointId,
    createTrafficMirrorTarget_tagSpecifications,
    createTrafficMirrorTargetResponse_clientToken,
    createTrafficMirrorTargetResponse_trafficMirrorTarget,
    createTrafficMirrorTargetResponse_httpStatus,

    -- ** CreateTransitGateway
    createTransitGateway_description,
    createTransitGateway_dryRun,
    createTransitGateway_options,
    createTransitGateway_tagSpecifications,
    createTransitGatewayResponse_transitGateway,
    createTransitGatewayResponse_httpStatus,

    -- ** CreateTransitGatewayConnect
    createTransitGatewayConnect_dryRun,
    createTransitGatewayConnect_tagSpecifications,
    createTransitGatewayConnect_transportTransitGatewayAttachmentId,
    createTransitGatewayConnect_options,
    createTransitGatewayConnectResponse_transitGatewayConnect,
    createTransitGatewayConnectResponse_httpStatus,

    -- ** CreateTransitGatewayConnectPeer
    createTransitGatewayConnectPeer_transitGatewayAddress,
    createTransitGatewayConnectPeer_dryRun,
    createTransitGatewayConnectPeer_tagSpecifications,
    createTransitGatewayConnectPeer_bgpOptions,
    createTransitGatewayConnectPeer_transitGatewayAttachmentId,
    createTransitGatewayConnectPeer_peerAddress,
    createTransitGatewayConnectPeer_insideCidrBlocks,
    createTransitGatewayConnectPeerResponse_transitGatewayConnectPeer,
    createTransitGatewayConnectPeerResponse_httpStatus,

    -- ** CreateTransitGatewayMulticastDomain
    createTransitGatewayMulticastDomain_dryRun,
    createTransitGatewayMulticastDomain_options,
    createTransitGatewayMulticastDomain_tagSpecifications,
    createTransitGatewayMulticastDomain_transitGatewayId,
    createTransitGatewayMulticastDomainResponse_transitGatewayMulticastDomain,
    createTransitGatewayMulticastDomainResponse_httpStatus,

    -- ** CreateTransitGatewayPeeringAttachment
    createTransitGatewayPeeringAttachment_dryRun,
    createTransitGatewayPeeringAttachment_options,
    createTransitGatewayPeeringAttachment_tagSpecifications,
    createTransitGatewayPeeringAttachment_transitGatewayId,
    createTransitGatewayPeeringAttachment_peerTransitGatewayId,
    createTransitGatewayPeeringAttachment_peerAccountId,
    createTransitGatewayPeeringAttachment_peerRegion,
    createTransitGatewayPeeringAttachmentResponse_transitGatewayPeeringAttachment,
    createTransitGatewayPeeringAttachmentResponse_httpStatus,

    -- ** CreateTransitGatewayPolicyTable
    createTransitGatewayPolicyTable_dryRun,
    createTransitGatewayPolicyTable_tagSpecifications,
    createTransitGatewayPolicyTable_transitGatewayId,
    createTransitGatewayPolicyTableResponse_transitGatewayPolicyTable,
    createTransitGatewayPolicyTableResponse_httpStatus,

    -- ** CreateTransitGatewayPrefixListReference
    createTransitGatewayPrefixListReference_transitGatewayAttachmentId,
    createTransitGatewayPrefixListReference_dryRun,
    createTransitGatewayPrefixListReference_blackhole,
    createTransitGatewayPrefixListReference_transitGatewayRouteTableId,
    createTransitGatewayPrefixListReference_prefixListId,
    createTransitGatewayPrefixListReferenceResponse_transitGatewayPrefixListReference,
    createTransitGatewayPrefixListReferenceResponse_httpStatus,

    -- ** CreateTransitGatewayRoute
    createTransitGatewayRoute_transitGatewayAttachmentId,
    createTransitGatewayRoute_dryRun,
    createTransitGatewayRoute_blackhole,
    createTransitGatewayRoute_destinationCidrBlock,
    createTransitGatewayRoute_transitGatewayRouteTableId,
    createTransitGatewayRouteResponse_route,
    createTransitGatewayRouteResponse_httpStatus,

    -- ** CreateTransitGatewayRouteTable
    createTransitGatewayRouteTable_dryRun,
    createTransitGatewayRouteTable_tagSpecifications,
    createTransitGatewayRouteTable_transitGatewayId,
    createTransitGatewayRouteTableResponse_transitGatewayRouteTable,
    createTransitGatewayRouteTableResponse_httpStatus,

    -- ** CreateTransitGatewayRouteTableAnnouncement
    createTransitGatewayRouteTableAnnouncement_dryRun,
    createTransitGatewayRouteTableAnnouncement_tagSpecifications,
    createTransitGatewayRouteTableAnnouncement_transitGatewayRouteTableId,
    createTransitGatewayRouteTableAnnouncement_peeringAttachmentId,
    createTransitGatewayRouteTableAnnouncementResponse_transitGatewayRouteTableAnnouncement,
    createTransitGatewayRouteTableAnnouncementResponse_httpStatus,

    -- ** CreateTransitGatewayVpcAttachment
    createTransitGatewayVpcAttachment_dryRun,
    createTransitGatewayVpcAttachment_options,
    createTransitGatewayVpcAttachment_tagSpecifications,
    createTransitGatewayVpcAttachment_transitGatewayId,
    createTransitGatewayVpcAttachment_vpcId,
    createTransitGatewayVpcAttachment_subnetIds,
    createTransitGatewayVpcAttachmentResponse_transitGatewayVpcAttachment,
    createTransitGatewayVpcAttachmentResponse_httpStatus,

    -- ** CreateVolume
    createVolume_clientToken,
    createVolume_outpostArn,
    createVolume_snapshotId,
    createVolume_size,
    createVolume_volumeType,
    createVolume_dryRun,
    createVolume_encrypted,
    createVolume_kmsKeyId,
    createVolume_throughput,
    createVolume_multiAttachEnabled,
    createVolume_tagSpecifications,
    createVolume_iops,
    createVolume_availabilityZone,
    volume_tags,
    volume_outpostArn,
    volume_attachments,
    volume_kmsKeyId,
    volume_fastRestored,
    volume_throughput,
    volume_multiAttachEnabled,
    volume_iops,
    volume_availabilityZone,
    volume_createTime,
    volume_encrypted,
    volume_size,
    volume_snapshotId,
    volume_state,
    volume_volumeId,
    volume_volumeType,

    -- ** CreateVpc
    createVpc_ipv6CidrBlockNetworkBorderGroup,
    createVpc_ipv4IpamPoolId,
    createVpc_ipv6Pool,
    createVpc_instanceTenancy,
    createVpc_ipv4NetmaskLength,
    createVpc_ipv6IpamPoolId,
    createVpc_amazonProvidedIpv6CidrBlock,
    createVpc_dryRun,
    createVpc_ipv6NetmaskLength,
    createVpc_cidrBlock,
    createVpc_tagSpecifications,
    createVpc_ipv6CidrBlock,
    createVpcResponse_vpc,
    createVpcResponse_httpStatus,

    -- ** CreateVpcEndpoint
    createVpcEndpoint_privateDnsEnabled,
    createVpcEndpoint_clientToken,
    createVpcEndpoint_securityGroupIds,
    createVpcEndpoint_vpcEndpointType,
    createVpcEndpoint_routeTableIds,
    createVpcEndpoint_dryRun,
    createVpcEndpoint_policyDocument,
    createVpcEndpoint_tagSpecifications,
    createVpcEndpoint_dnsOptions,
    createVpcEndpoint_ipAddressType,
    createVpcEndpoint_subnetIds,
    createVpcEndpoint_vpcId,
    createVpcEndpoint_serviceName,
    createVpcEndpointResponse_clientToken,
    createVpcEndpointResponse_vpcEndpoint,
    createVpcEndpointResponse_httpStatus,

    -- ** CreateVpcEndpointConnectionNotification
    createVpcEndpointConnectionNotification_clientToken,
    createVpcEndpointConnectionNotification_vpcEndpointId,
    createVpcEndpointConnectionNotification_dryRun,
    createVpcEndpointConnectionNotification_serviceId,
    createVpcEndpointConnectionNotification_connectionNotificationArn,
    createVpcEndpointConnectionNotification_connectionEvents,
    createVpcEndpointConnectionNotificationResponse_connectionNotification,
    createVpcEndpointConnectionNotificationResponse_clientToken,
    createVpcEndpointConnectionNotificationResponse_httpStatus,

    -- ** CreateVpcEndpointServiceConfiguration
    createVpcEndpointServiceConfiguration_clientToken,
    createVpcEndpointServiceConfiguration_gatewayLoadBalancerArns,
    createVpcEndpointServiceConfiguration_acceptanceRequired,
    createVpcEndpointServiceConfiguration_supportedIpAddressTypes,
    createVpcEndpointServiceConfiguration_dryRun,
    createVpcEndpointServiceConfiguration_networkLoadBalancerArns,
    createVpcEndpointServiceConfiguration_privateDnsName,
    createVpcEndpointServiceConfiguration_tagSpecifications,
    createVpcEndpointServiceConfigurationResponse_clientToken,
    createVpcEndpointServiceConfigurationResponse_serviceConfiguration,
    createVpcEndpointServiceConfigurationResponse_httpStatus,

    -- ** CreateVpcPeeringConnection
    createVpcPeeringConnection_peerOwnerId,
    createVpcPeeringConnection_dryRun,
    createVpcPeeringConnection_peerVpcId,
    createVpcPeeringConnection_vpcId,
    createVpcPeeringConnection_tagSpecifications,
    createVpcPeeringConnection_peerRegion,
    createVpcPeeringConnectionResponse_vpcPeeringConnection,
    createVpcPeeringConnectionResponse_httpStatus,

    -- ** CreateVpnConnection
    createVpnConnection_transitGatewayId,
    createVpnConnection_dryRun,
    createVpnConnection_options,
    createVpnConnection_vpnGatewayId,
    createVpnConnection_tagSpecifications,
    createVpnConnection_customerGatewayId,
    createVpnConnection_type,
    createVpnConnectionResponse_vpnConnection,
    createVpnConnectionResponse_httpStatus,

    -- ** CreateVpnConnectionRoute
    createVpnConnectionRoute_destinationCidrBlock,
    createVpnConnectionRoute_vpnConnectionId,

    -- ** CreateVpnGateway
    createVpnGateway_availabilityZone,
    createVpnGateway_dryRun,
    createVpnGateway_amazonSideAsn,
    createVpnGateway_tagSpecifications,
    createVpnGateway_type,
    createVpnGatewayResponse_vpnGateway,
    createVpnGatewayResponse_httpStatus,

    -- ** DeleteCarrierGateway
    deleteCarrierGateway_dryRun,
    deleteCarrierGateway_carrierGatewayId,
    deleteCarrierGatewayResponse_carrierGateway,
    deleteCarrierGatewayResponse_httpStatus,

    -- ** DeleteClientVpnEndpoint
    deleteClientVpnEndpoint_dryRun,
    deleteClientVpnEndpoint_clientVpnEndpointId,
    deleteClientVpnEndpointResponse_status,
    deleteClientVpnEndpointResponse_httpStatus,

    -- ** DeleteClientVpnRoute
    deleteClientVpnRoute_targetVpcSubnetId,
    deleteClientVpnRoute_dryRun,
    deleteClientVpnRoute_clientVpnEndpointId,
    deleteClientVpnRoute_destinationCidrBlock,
    deleteClientVpnRouteResponse_status,
    deleteClientVpnRouteResponse_httpStatus,

    -- ** DeleteCoipCidr
    deleteCoipCidr_dryRun,
    deleteCoipCidr_cidr,
    deleteCoipCidr_coipPoolId,
    deleteCoipCidrResponse_coipCidr,
    deleteCoipCidrResponse_httpStatus,

    -- ** DeleteCoipPool
    deleteCoipPool_dryRun,
    deleteCoipPool_coipPoolId,
    deleteCoipPoolResponse_coipPool,
    deleteCoipPoolResponse_httpStatus,

    -- ** DeleteCustomerGateway
    deleteCustomerGateway_dryRun,
    deleteCustomerGateway_customerGatewayId,

    -- ** DeleteDhcpOptions
    deleteDhcpOptions_dryRun,
    deleteDhcpOptions_dhcpOptionsId,

    -- ** DeleteEgressOnlyInternetGateway
    deleteEgressOnlyInternetGateway_dryRun,
    deleteEgressOnlyInternetGateway_egressOnlyInternetGatewayId,
    deleteEgressOnlyInternetGatewayResponse_returnCode,
    deleteEgressOnlyInternetGatewayResponse_httpStatus,

    -- ** DeleteFleets
    deleteFleets_dryRun,
    deleteFleets_fleetIds,
    deleteFleets_terminateInstances,
    deleteFleetsResponse_successfulFleetDeletions,
    deleteFleetsResponse_unsuccessfulFleetDeletions,
    deleteFleetsResponse_httpStatus,

    -- ** DeleteFlowLogs
    deleteFlowLogs_dryRun,
    deleteFlowLogs_flowLogIds,
    deleteFlowLogsResponse_unsuccessful,
    deleteFlowLogsResponse_httpStatus,

    -- ** DeleteFpgaImage
    deleteFpgaImage_dryRun,
    deleteFpgaImage_fpgaImageId,
    deleteFpgaImageResponse_return,
    deleteFpgaImageResponse_httpStatus,

    -- ** DeleteInstanceEventWindow
    deleteInstanceEventWindow_dryRun,
    deleteInstanceEventWindow_forceDelete,
    deleteInstanceEventWindow_instanceEventWindowId,
    deleteInstanceEventWindowResponse_instanceEventWindowState,
    deleteInstanceEventWindowResponse_httpStatus,

    -- ** DeleteInternetGateway
    deleteInternetGateway_dryRun,
    deleteInternetGateway_internetGatewayId,

    -- ** DeleteIpam
    deleteIpam_dryRun,
    deleteIpam_cascade,
    deleteIpam_ipamId,
    deleteIpamResponse_ipam,
    deleteIpamResponse_httpStatus,

    -- ** DeleteIpamPool
    deleteIpamPool_dryRun,
    deleteIpamPool_ipamPoolId,
    deleteIpamPoolResponse_ipamPool,
    deleteIpamPoolResponse_httpStatus,

    -- ** DeleteIpamScope
    deleteIpamScope_dryRun,
    deleteIpamScope_ipamScopeId,
    deleteIpamScopeResponse_ipamScope,
    deleteIpamScopeResponse_httpStatus,

    -- ** DeleteKeyPair
    deleteKeyPair_dryRun,
    deleteKeyPair_keyName,
    deleteKeyPair_keyPairId,

    -- ** DeleteLaunchTemplate
    deleteLaunchTemplate_dryRun,
    deleteLaunchTemplate_launchTemplateId,
    deleteLaunchTemplate_launchTemplateName,
    deleteLaunchTemplateResponse_launchTemplate,
    deleteLaunchTemplateResponse_httpStatus,

    -- ** DeleteLaunchTemplateVersions
    deleteLaunchTemplateVersions_dryRun,
    deleteLaunchTemplateVersions_launchTemplateId,
    deleteLaunchTemplateVersions_launchTemplateName,
    deleteLaunchTemplateVersions_versions,
    deleteLaunchTemplateVersionsResponse_unsuccessfullyDeletedLaunchTemplateVersions,
    deleteLaunchTemplateVersionsResponse_successfullyDeletedLaunchTemplateVersions,
    deleteLaunchTemplateVersionsResponse_httpStatus,

    -- ** DeleteLocalGatewayRoute
    deleteLocalGatewayRoute_dryRun,
    deleteLocalGatewayRoute_destinationCidrBlock,
    deleteLocalGatewayRoute_localGatewayRouteTableId,
    deleteLocalGatewayRouteResponse_route,
    deleteLocalGatewayRouteResponse_httpStatus,

    -- ** DeleteLocalGatewayRouteTable
    deleteLocalGatewayRouteTable_dryRun,
    deleteLocalGatewayRouteTable_localGatewayRouteTableId,
    deleteLocalGatewayRouteTableResponse_localGatewayRouteTable,
    deleteLocalGatewayRouteTableResponse_httpStatus,

    -- ** DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation
    deleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation_dryRun,
    deleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableVirtualInterfaceGroupAssociationId,
    deleteLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse_localGatewayRouteTableVirtualInterfaceGroupAssociation,
    deleteLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse_httpStatus,

    -- ** DeleteLocalGatewayRouteTableVpcAssociation
    deleteLocalGatewayRouteTableVpcAssociation_dryRun,
    deleteLocalGatewayRouteTableVpcAssociation_localGatewayRouteTableVpcAssociationId,
    deleteLocalGatewayRouteTableVpcAssociationResponse_localGatewayRouteTableVpcAssociation,
    deleteLocalGatewayRouteTableVpcAssociationResponse_httpStatus,

    -- ** DeleteManagedPrefixList
    deleteManagedPrefixList_dryRun,
    deleteManagedPrefixList_prefixListId,
    deleteManagedPrefixListResponse_prefixList,
    deleteManagedPrefixListResponse_httpStatus,

    -- ** DeleteNatGateway
    deleteNatGateway_dryRun,
    deleteNatGateway_natGatewayId,
    deleteNatGatewayResponse_natGatewayId,
    deleteNatGatewayResponse_httpStatus,

    -- ** DeleteNetworkAcl
    deleteNetworkAcl_dryRun,
    deleteNetworkAcl_networkAclId,

    -- ** DeleteNetworkAclEntry
    deleteNetworkAclEntry_dryRun,
    deleteNetworkAclEntry_egress,
    deleteNetworkAclEntry_networkAclId,
    deleteNetworkAclEntry_ruleNumber,

    -- ** DeleteNetworkInsightsAccessScope
    deleteNetworkInsightsAccessScope_dryRun,
    deleteNetworkInsightsAccessScope_networkInsightsAccessScopeId,
    deleteNetworkInsightsAccessScopeResponse_networkInsightsAccessScopeId,
    deleteNetworkInsightsAccessScopeResponse_httpStatus,

    -- ** DeleteNetworkInsightsAccessScopeAnalysis
    deleteNetworkInsightsAccessScopeAnalysis_dryRun,
    deleteNetworkInsightsAccessScopeAnalysis_networkInsightsAccessScopeAnalysisId,
    deleteNetworkInsightsAccessScopeAnalysisResponse_networkInsightsAccessScopeAnalysisId,
    deleteNetworkInsightsAccessScopeAnalysisResponse_httpStatus,

    -- ** DeleteNetworkInsightsAnalysis
    deleteNetworkInsightsAnalysis_dryRun,
    deleteNetworkInsightsAnalysis_networkInsightsAnalysisId,
    deleteNetworkInsightsAnalysisResponse_networkInsightsAnalysisId,
    deleteNetworkInsightsAnalysisResponse_httpStatus,

    -- ** DeleteNetworkInsightsPath
    deleteNetworkInsightsPath_dryRun,
    deleteNetworkInsightsPath_networkInsightsPathId,
    deleteNetworkInsightsPathResponse_networkInsightsPathId,
    deleteNetworkInsightsPathResponse_httpStatus,

    -- ** DeleteNetworkInterface
    deleteNetworkInterface_dryRun,
    deleteNetworkInterface_networkInterfaceId,

    -- ** DeleteNetworkInterfacePermission
    deleteNetworkInterfacePermission_dryRun,
    deleteNetworkInterfacePermission_force,
    deleteNetworkInterfacePermission_networkInterfacePermissionId,
    deleteNetworkInterfacePermissionResponse_return,
    deleteNetworkInterfacePermissionResponse_httpStatus,

    -- ** DeletePlacementGroup
    deletePlacementGroup_dryRun,
    deletePlacementGroup_groupName,

    -- ** DeletePublicIpv4Pool
    deletePublicIpv4Pool_dryRun,
    deletePublicIpv4Pool_poolId,
    deletePublicIpv4PoolResponse_returnValue,
    deletePublicIpv4PoolResponse_httpStatus,

    -- ** DeleteQueuedReservedInstances
    deleteQueuedReservedInstances_dryRun,
    deleteQueuedReservedInstances_reservedInstancesIds,
    deleteQueuedReservedInstancesResponse_failedQueuedPurchaseDeletions,
    deleteQueuedReservedInstancesResponse_successfulQueuedPurchaseDeletions,
    deleteQueuedReservedInstancesResponse_httpStatus,

    -- ** DeleteRoute
    deleteRoute_destinationPrefixListId,
    deleteRoute_dryRun,
    deleteRoute_destinationCidrBlock,
    deleteRoute_destinationIpv6CidrBlock,
    deleteRoute_routeTableId,

    -- ** DeleteRouteTable
    deleteRouteTable_dryRun,
    deleteRouteTable_routeTableId,

    -- ** DeleteSecurityGroup
    deleteSecurityGroup_groupName,
    deleteSecurityGroup_dryRun,
    deleteSecurityGroup_groupId,

    -- ** DeleteSnapshot
    deleteSnapshot_dryRun,
    deleteSnapshot_snapshotId,

    -- ** DeleteSpotDatafeedSubscription
    deleteSpotDatafeedSubscription_dryRun,

    -- ** DeleteSubnet
    deleteSubnet_dryRun,
    deleteSubnet_subnetId,

    -- ** DeleteSubnetCidrReservation
    deleteSubnetCidrReservation_dryRun,
    deleteSubnetCidrReservation_subnetCidrReservationId,
    deleteSubnetCidrReservationResponse_deletedSubnetCidrReservation,
    deleteSubnetCidrReservationResponse_httpStatus,

    -- ** DeleteTags
    deleteTags_tags,
    deleteTags_dryRun,
    deleteTags_resources,

    -- ** DeleteTrafficMirrorFilter
    deleteTrafficMirrorFilter_dryRun,
    deleteTrafficMirrorFilter_trafficMirrorFilterId,
    deleteTrafficMirrorFilterResponse_trafficMirrorFilterId,
    deleteTrafficMirrorFilterResponse_httpStatus,

    -- ** DeleteTrafficMirrorFilterRule
    deleteTrafficMirrorFilterRule_dryRun,
    deleteTrafficMirrorFilterRule_trafficMirrorFilterRuleId,
    deleteTrafficMirrorFilterRuleResponse_trafficMirrorFilterRuleId,
    deleteTrafficMirrorFilterRuleResponse_httpStatus,

    -- ** DeleteTrafficMirrorSession
    deleteTrafficMirrorSession_dryRun,
    deleteTrafficMirrorSession_trafficMirrorSessionId,
    deleteTrafficMirrorSessionResponse_trafficMirrorSessionId,
    deleteTrafficMirrorSessionResponse_httpStatus,

    -- ** DeleteTrafficMirrorTarget
    deleteTrafficMirrorTarget_dryRun,
    deleteTrafficMirrorTarget_trafficMirrorTargetId,
    deleteTrafficMirrorTargetResponse_trafficMirrorTargetId,
    deleteTrafficMirrorTargetResponse_httpStatus,

    -- ** DeleteTransitGateway
    deleteTransitGateway_dryRun,
    deleteTransitGateway_transitGatewayId,
    deleteTransitGatewayResponse_transitGateway,
    deleteTransitGatewayResponse_httpStatus,

    -- ** DeleteTransitGatewayConnect
    deleteTransitGatewayConnect_dryRun,
    deleteTransitGatewayConnect_transitGatewayAttachmentId,
    deleteTransitGatewayConnectResponse_transitGatewayConnect,
    deleteTransitGatewayConnectResponse_httpStatus,

    -- ** DeleteTransitGatewayConnectPeer
    deleteTransitGatewayConnectPeer_dryRun,
    deleteTransitGatewayConnectPeer_transitGatewayConnectPeerId,
    deleteTransitGatewayConnectPeerResponse_transitGatewayConnectPeer,
    deleteTransitGatewayConnectPeerResponse_httpStatus,

    -- ** DeleteTransitGatewayMulticastDomain
    deleteTransitGatewayMulticastDomain_dryRun,
    deleteTransitGatewayMulticastDomain_transitGatewayMulticastDomainId,
    deleteTransitGatewayMulticastDomainResponse_transitGatewayMulticastDomain,
    deleteTransitGatewayMulticastDomainResponse_httpStatus,

    -- ** DeleteTransitGatewayPeeringAttachment
    deleteTransitGatewayPeeringAttachment_dryRun,
    deleteTransitGatewayPeeringAttachment_transitGatewayAttachmentId,
    deleteTransitGatewayPeeringAttachmentResponse_transitGatewayPeeringAttachment,
    deleteTransitGatewayPeeringAttachmentResponse_httpStatus,

    -- ** DeleteTransitGatewayPolicyTable
    deleteTransitGatewayPolicyTable_dryRun,
    deleteTransitGatewayPolicyTable_transitGatewayPolicyTableId,
    deleteTransitGatewayPolicyTableResponse_transitGatewayPolicyTable,
    deleteTransitGatewayPolicyTableResponse_httpStatus,

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

    -- ** DeleteTransitGatewayRouteTable
    deleteTransitGatewayRouteTable_dryRun,
    deleteTransitGatewayRouteTable_transitGatewayRouteTableId,
    deleteTransitGatewayRouteTableResponse_transitGatewayRouteTable,
    deleteTransitGatewayRouteTableResponse_httpStatus,

    -- ** DeleteTransitGatewayRouteTableAnnouncement
    deleteTransitGatewayRouteTableAnnouncement_dryRun,
    deleteTransitGatewayRouteTableAnnouncement_transitGatewayRouteTableAnnouncementId,
    deleteTransitGatewayRouteTableAnnouncementResponse_transitGatewayRouteTableAnnouncement,
    deleteTransitGatewayRouteTableAnnouncementResponse_httpStatus,

    -- ** DeleteTransitGatewayVpcAttachment
    deleteTransitGatewayVpcAttachment_dryRun,
    deleteTransitGatewayVpcAttachment_transitGatewayAttachmentId,
    deleteTransitGatewayVpcAttachmentResponse_transitGatewayVpcAttachment,
    deleteTransitGatewayVpcAttachmentResponse_httpStatus,

    -- ** DeleteVolume
    deleteVolume_dryRun,
    deleteVolume_volumeId,

    -- ** DeleteVpc
    deleteVpc_dryRun,
    deleteVpc_vpcId,

    -- ** DeleteVpcEndpointConnectionNotifications
    deleteVpcEndpointConnectionNotifications_dryRun,
    deleteVpcEndpointConnectionNotifications_connectionNotificationIds,
    deleteVpcEndpointConnectionNotificationsResponse_unsuccessful,
    deleteVpcEndpointConnectionNotificationsResponse_httpStatus,

    -- ** DeleteVpcEndpointServiceConfigurations
    deleteVpcEndpointServiceConfigurations_dryRun,
    deleteVpcEndpointServiceConfigurations_serviceIds,
    deleteVpcEndpointServiceConfigurationsResponse_unsuccessful,
    deleteVpcEndpointServiceConfigurationsResponse_httpStatus,

    -- ** DeleteVpcEndpoints
    deleteVpcEndpoints_dryRun,
    deleteVpcEndpoints_vpcEndpointIds,
    deleteVpcEndpointsResponse_unsuccessful,
    deleteVpcEndpointsResponse_httpStatus,

    -- ** DeleteVpcPeeringConnection
    deleteVpcPeeringConnection_dryRun,
    deleteVpcPeeringConnection_vpcPeeringConnectionId,
    deleteVpcPeeringConnectionResponse_return,
    deleteVpcPeeringConnectionResponse_httpStatus,

    -- ** DeleteVpnConnection
    deleteVpnConnection_dryRun,
    deleteVpnConnection_vpnConnectionId,

    -- ** DeleteVpnConnectionRoute
    deleteVpnConnectionRoute_destinationCidrBlock,
    deleteVpnConnectionRoute_vpnConnectionId,

    -- ** DeleteVpnGateway
    deleteVpnGateway_dryRun,
    deleteVpnGateway_vpnGatewayId,

    -- ** DeprovisionByoipCidr
    deprovisionByoipCidr_dryRun,
    deprovisionByoipCidr_cidr,
    deprovisionByoipCidrResponse_byoipCidr,
    deprovisionByoipCidrResponse_httpStatus,

    -- ** DeprovisionIpamPoolCidr
    deprovisionIpamPoolCidr_cidr,
    deprovisionIpamPoolCidr_dryRun,
    deprovisionIpamPoolCidr_ipamPoolId,
    deprovisionIpamPoolCidrResponse_ipamPoolCidr,
    deprovisionIpamPoolCidrResponse_httpStatus,

    -- ** DeprovisionPublicIpv4PoolCidr
    deprovisionPublicIpv4PoolCidr_dryRun,
    deprovisionPublicIpv4PoolCidr_poolId,
    deprovisionPublicIpv4PoolCidr_cidr,
    deprovisionPublicIpv4PoolCidrResponse_deprovisionedAddresses,
    deprovisionPublicIpv4PoolCidrResponse_poolId,
    deprovisionPublicIpv4PoolCidrResponse_httpStatus,

    -- ** DeregisterImage
    deregisterImage_dryRun,
    deregisterImage_imageId,

    -- ** DeregisterInstanceEventNotificationAttributes
    deregisterInstanceEventNotificationAttributes_instanceTagAttribute,
    deregisterInstanceEventNotificationAttributes_dryRun,
    deregisterInstanceEventNotificationAttributesResponse_instanceTagAttribute,
    deregisterInstanceEventNotificationAttributesResponse_httpStatus,

    -- ** DeregisterTransitGatewayMulticastGroupMembers
    deregisterTransitGatewayMulticastGroupMembers_groupIpAddress,
    deregisterTransitGatewayMulticastGroupMembers_dryRun,
    deregisterTransitGatewayMulticastGroupMembers_transitGatewayMulticastDomainId,
    deregisterTransitGatewayMulticastGroupMembers_networkInterfaceIds,
    deregisterTransitGatewayMulticastGroupMembersResponse_deregisteredMulticastGroupMembers,
    deregisterTransitGatewayMulticastGroupMembersResponse_httpStatus,

    -- ** DeregisterTransitGatewayMulticastGroupSources
    deregisterTransitGatewayMulticastGroupSources_groupIpAddress,
    deregisterTransitGatewayMulticastGroupSources_dryRun,
    deregisterTransitGatewayMulticastGroupSources_transitGatewayMulticastDomainId,
    deregisterTransitGatewayMulticastGroupSources_networkInterfaceIds,
    deregisterTransitGatewayMulticastGroupSourcesResponse_deregisteredMulticastGroupSources,
    deregisterTransitGatewayMulticastGroupSourcesResponse_httpStatus,

    -- ** DescribeAccountAttributes
    describeAccountAttributes_attributeNames,
    describeAccountAttributes_dryRun,
    describeAccountAttributesResponse_accountAttributes,
    describeAccountAttributesResponse_httpStatus,

    -- ** DescribeAddressTransfers
    describeAddressTransfers_allocationIds,
    describeAddressTransfers_nextToken,
    describeAddressTransfers_dryRun,
    describeAddressTransfers_maxResults,
    describeAddressTransfersResponse_nextToken,
    describeAddressTransfersResponse_addressTransfers,
    describeAddressTransfersResponse_httpStatus,

    -- ** DescribeAddresses
    describeAddresses_allocationIds,
    describeAddresses_filters,
    describeAddresses_publicIps,
    describeAddresses_dryRun,
    describeAddressesResponse_addresses,
    describeAddressesResponse_httpStatus,

    -- ** DescribeAddressesAttribute
    describeAddressesAttribute_allocationIds,
    describeAddressesAttribute_nextToken,
    describeAddressesAttribute_attribute,
    describeAddressesAttribute_dryRun,
    describeAddressesAttribute_maxResults,
    describeAddressesAttributeResponse_nextToken,
    describeAddressesAttributeResponse_addresses,
    describeAddressesAttributeResponse_httpStatus,

    -- ** DescribeAggregateIdFormat
    describeAggregateIdFormat_dryRun,
    describeAggregateIdFormatResponse_useLongIdsAggregated,
    describeAggregateIdFormatResponse_statuses,
    describeAggregateIdFormatResponse_httpStatus,

    -- ** DescribeAvailabilityZones
    describeAvailabilityZones_zoneNames,
    describeAvailabilityZones_filters,
    describeAvailabilityZones_allAvailabilityZones,
    describeAvailabilityZones_dryRun,
    describeAvailabilityZones_zoneIds,
    describeAvailabilityZonesResponse_availabilityZones,
    describeAvailabilityZonesResponse_httpStatus,

    -- ** DescribeBundleTasks
    describeBundleTasks_bundleIds,
    describeBundleTasks_filters,
    describeBundleTasks_dryRun,
    describeBundleTasksResponse_bundleTasks,
    describeBundleTasksResponse_httpStatus,

    -- ** DescribeByoipCidrs
    describeByoipCidrs_nextToken,
    describeByoipCidrs_dryRun,
    describeByoipCidrs_maxResults,
    describeByoipCidrsResponse_nextToken,
    describeByoipCidrsResponse_byoipCidrs,
    describeByoipCidrsResponse_httpStatus,

    -- ** DescribeCapacityReservationFleets
    describeCapacityReservationFleets_capacityReservationFleetIds,
    describeCapacityReservationFleets_nextToken,
    describeCapacityReservationFleets_filters,
    describeCapacityReservationFleets_dryRun,
    describeCapacityReservationFleets_maxResults,
    describeCapacityReservationFleetsResponse_nextToken,
    describeCapacityReservationFleetsResponse_capacityReservationFleets,
    describeCapacityReservationFleetsResponse_httpStatus,

    -- ** DescribeCapacityReservations
    describeCapacityReservations_nextToken,
    describeCapacityReservations_filters,
    describeCapacityReservations_capacityReservationIds,
    describeCapacityReservations_dryRun,
    describeCapacityReservations_maxResults,
    describeCapacityReservationsResponse_nextToken,
    describeCapacityReservationsResponse_capacityReservations,
    describeCapacityReservationsResponse_httpStatus,

    -- ** DescribeCarrierGateways
    describeCarrierGateways_nextToken,
    describeCarrierGateways_filters,
    describeCarrierGateways_dryRun,
    describeCarrierGateways_maxResults,
    describeCarrierGateways_carrierGatewayIds,
    describeCarrierGatewaysResponse_nextToken,
    describeCarrierGatewaysResponse_carrierGateways,
    describeCarrierGatewaysResponse_httpStatus,

    -- ** DescribeClassicLinkInstances
    describeClassicLinkInstances_nextToken,
    describeClassicLinkInstances_filters,
    describeClassicLinkInstances_dryRun,
    describeClassicLinkInstances_maxResults,
    describeClassicLinkInstances_instanceIds,
    describeClassicLinkInstancesResponse_instances,
    describeClassicLinkInstancesResponse_nextToken,
    describeClassicLinkInstancesResponse_httpStatus,

    -- ** DescribeClientVpnAuthorizationRules
    describeClientVpnAuthorizationRules_nextToken,
    describeClientVpnAuthorizationRules_filters,
    describeClientVpnAuthorizationRules_dryRun,
    describeClientVpnAuthorizationRules_maxResults,
    describeClientVpnAuthorizationRules_clientVpnEndpointId,
    describeClientVpnAuthorizationRulesResponse_nextToken,
    describeClientVpnAuthorizationRulesResponse_authorizationRules,
    describeClientVpnAuthorizationRulesResponse_httpStatus,

    -- ** DescribeClientVpnConnections
    describeClientVpnConnections_nextToken,
    describeClientVpnConnections_filters,
    describeClientVpnConnections_dryRun,
    describeClientVpnConnections_maxResults,
    describeClientVpnConnections_clientVpnEndpointId,
    describeClientVpnConnectionsResponse_nextToken,
    describeClientVpnConnectionsResponse_connections,
    describeClientVpnConnectionsResponse_httpStatus,

    -- ** DescribeClientVpnEndpoints
    describeClientVpnEndpoints_nextToken,
    describeClientVpnEndpoints_filters,
    describeClientVpnEndpoints_dryRun,
    describeClientVpnEndpoints_maxResults,
    describeClientVpnEndpoints_clientVpnEndpointIds,
    describeClientVpnEndpointsResponse_clientVpnEndpoints,
    describeClientVpnEndpointsResponse_nextToken,
    describeClientVpnEndpointsResponse_httpStatus,

    -- ** DescribeClientVpnRoutes
    describeClientVpnRoutes_nextToken,
    describeClientVpnRoutes_filters,
    describeClientVpnRoutes_dryRun,
    describeClientVpnRoutes_maxResults,
    describeClientVpnRoutes_clientVpnEndpointId,
    describeClientVpnRoutesResponse_nextToken,
    describeClientVpnRoutesResponse_routes,
    describeClientVpnRoutesResponse_httpStatus,

    -- ** DescribeClientVpnTargetNetworks
    describeClientVpnTargetNetworks_nextToken,
    describeClientVpnTargetNetworks_filters,
    describeClientVpnTargetNetworks_dryRun,
    describeClientVpnTargetNetworks_maxResults,
    describeClientVpnTargetNetworks_associationIds,
    describeClientVpnTargetNetworks_clientVpnEndpointId,
    describeClientVpnTargetNetworksResponse_nextToken,
    describeClientVpnTargetNetworksResponse_clientVpnTargetNetworks,
    describeClientVpnTargetNetworksResponse_httpStatus,

    -- ** DescribeCoipPools
    describeCoipPools_nextToken,
    describeCoipPools_poolIds,
    describeCoipPools_filters,
    describeCoipPools_dryRun,
    describeCoipPools_maxResults,
    describeCoipPoolsResponse_nextToken,
    describeCoipPoolsResponse_coipPools,
    describeCoipPoolsResponse_httpStatus,

    -- ** DescribeConversionTasks
    describeConversionTasks_dryRun,
    describeConversionTasks_conversionTaskIds,
    describeConversionTasksResponse_conversionTasks,
    describeConversionTasksResponse_httpStatus,

    -- ** DescribeCustomerGateways
    describeCustomerGateways_filters,
    describeCustomerGateways_dryRun,
    describeCustomerGateways_customerGatewayIds,
    describeCustomerGatewaysResponse_customerGateways,
    describeCustomerGatewaysResponse_httpStatus,

    -- ** DescribeDhcpOptions
    describeDhcpOptions_nextToken,
    describeDhcpOptions_filters,
    describeDhcpOptions_dryRun,
    describeDhcpOptions_maxResults,
    describeDhcpOptions_dhcpOptionsIds,
    describeDhcpOptionsResponse_dhcpOptions,
    describeDhcpOptionsResponse_nextToken,
    describeDhcpOptionsResponse_httpStatus,

    -- ** DescribeEgressOnlyInternetGateways
    describeEgressOnlyInternetGateways_nextToken,
    describeEgressOnlyInternetGateways_filters,
    describeEgressOnlyInternetGateways_dryRun,
    describeEgressOnlyInternetGateways_maxResults,
    describeEgressOnlyInternetGateways_egressOnlyInternetGatewayIds,
    describeEgressOnlyInternetGatewaysResponse_egressOnlyInternetGateways,
    describeEgressOnlyInternetGatewaysResponse_nextToken,
    describeEgressOnlyInternetGatewaysResponse_httpStatus,

    -- ** DescribeElasticGpus
    describeElasticGpus_nextToken,
    describeElasticGpus_filters,
    describeElasticGpus_dryRun,
    describeElasticGpus_maxResults,
    describeElasticGpus_elasticGpuIds,
    describeElasticGpusResponse_elasticGpuSet,
    describeElasticGpusResponse_nextToken,
    describeElasticGpusResponse_maxResults,
    describeElasticGpusResponse_httpStatus,

    -- ** DescribeExportImageTasks
    describeExportImageTasks_nextToken,
    describeExportImageTasks_filters,
    describeExportImageTasks_exportImageTaskIds,
    describeExportImageTasks_dryRun,
    describeExportImageTasks_maxResults,
    describeExportImageTasksResponse_nextToken,
    describeExportImageTasksResponse_exportImageTasks,
    describeExportImageTasksResponse_httpStatus,

    -- ** DescribeExportTasks
    describeExportTasks_filters,
    describeExportTasks_exportTaskIds,
    describeExportTasksResponse_exportTasks,
    describeExportTasksResponse_httpStatus,

    -- ** DescribeFastLaunchImages
    describeFastLaunchImages_nextToken,
    describeFastLaunchImages_imageIds,
    describeFastLaunchImages_filters,
    describeFastLaunchImages_dryRun,
    describeFastLaunchImages_maxResults,
    describeFastLaunchImagesResponse_nextToken,
    describeFastLaunchImagesResponse_fastLaunchImages,
    describeFastLaunchImagesResponse_httpStatus,

    -- ** DescribeFastSnapshotRestores
    describeFastSnapshotRestores_nextToken,
    describeFastSnapshotRestores_filters,
    describeFastSnapshotRestores_dryRun,
    describeFastSnapshotRestores_maxResults,
    describeFastSnapshotRestoresResponse_nextToken,
    describeFastSnapshotRestoresResponse_fastSnapshotRestores,
    describeFastSnapshotRestoresResponse_httpStatus,

    -- ** DescribeFleetHistory
    describeFleetHistory_eventType,
    describeFleetHistory_nextToken,
    describeFleetHistory_dryRun,
    describeFleetHistory_maxResults,
    describeFleetHistory_fleetId,
    describeFleetHistory_startTime,
    describeFleetHistoryResponse_fleetId,
    describeFleetHistoryResponse_nextToken,
    describeFleetHistoryResponse_historyRecords,
    describeFleetHistoryResponse_startTime,
    describeFleetHistoryResponse_lastEvaluatedTime,
    describeFleetHistoryResponse_httpStatus,

    -- ** DescribeFleetInstances
    describeFleetInstances_nextToken,
    describeFleetInstances_filters,
    describeFleetInstances_dryRun,
    describeFleetInstances_maxResults,
    describeFleetInstances_fleetId,
    describeFleetInstancesResponse_fleetId,
    describeFleetInstancesResponse_activeInstances,
    describeFleetInstancesResponse_nextToken,
    describeFleetInstancesResponse_httpStatus,

    -- ** DescribeFleets
    describeFleets_nextToken,
    describeFleets_filters,
    describeFleets_dryRun,
    describeFleets_fleetIds,
    describeFleets_maxResults,
    describeFleetsResponse_nextToken,
    describeFleetsResponse_fleets,
    describeFleetsResponse_httpStatus,

    -- ** DescribeFlowLogs
    describeFlowLogs_nextToken,
    describeFlowLogs_flowLogIds,
    describeFlowLogs_dryRun,
    describeFlowLogs_filter,
    describeFlowLogs_maxResults,
    describeFlowLogsResponse_nextToken,
    describeFlowLogsResponse_flowLogs,
    describeFlowLogsResponse_httpStatus,

    -- ** DescribeFpgaImageAttribute
    describeFpgaImageAttribute_dryRun,
    describeFpgaImageAttribute_fpgaImageId,
    describeFpgaImageAttribute_attribute,
    describeFpgaImageAttributeResponse_fpgaImageAttribute,
    describeFpgaImageAttributeResponse_httpStatus,

    -- ** DescribeFpgaImages
    describeFpgaImages_nextToken,
    describeFpgaImages_filters,
    describeFpgaImages_owners,
    describeFpgaImages_dryRun,
    describeFpgaImages_maxResults,
    describeFpgaImages_fpgaImageIds,
    describeFpgaImagesResponse_nextToken,
    describeFpgaImagesResponse_fpgaImages,
    describeFpgaImagesResponse_httpStatus,

    -- ** DescribeHostReservationOfferings
    describeHostReservationOfferings_maxDuration,
    describeHostReservationOfferings_nextToken,
    describeHostReservationOfferings_minDuration,
    describeHostReservationOfferings_filter,
    describeHostReservationOfferings_maxResults,
    describeHostReservationOfferings_offeringId,
    describeHostReservationOfferingsResponse_nextToken,
    describeHostReservationOfferingsResponse_offeringSet,
    describeHostReservationOfferingsResponse_httpStatus,

    -- ** DescribeHostReservations
    describeHostReservations_nextToken,
    describeHostReservations_filter,
    describeHostReservations_hostReservationIdSet,
    describeHostReservations_maxResults,
    describeHostReservationsResponse_nextToken,
    describeHostReservationsResponse_hostReservationSet,
    describeHostReservationsResponse_httpStatus,

    -- ** DescribeHosts
    describeHosts_nextToken,
    describeHosts_hostIds,
    describeHosts_filter,
    describeHosts_maxResults,
    describeHostsResponse_nextToken,
    describeHostsResponse_hosts,
    describeHostsResponse_httpStatus,

    -- ** DescribeIamInstanceProfileAssociations
    describeIamInstanceProfileAssociations_nextToken,
    describeIamInstanceProfileAssociations_filters,
    describeIamInstanceProfileAssociations_maxResults,
    describeIamInstanceProfileAssociations_associationIds,
    describeIamInstanceProfileAssociationsResponse_nextToken,
    describeIamInstanceProfileAssociationsResponse_iamInstanceProfileAssociations,
    describeIamInstanceProfileAssociationsResponse_httpStatus,

    -- ** DescribeIdFormat
    describeIdFormat_resource,
    describeIdFormatResponse_statuses,
    describeIdFormatResponse_httpStatus,

    -- ** DescribeIdentityIdFormat
    describeIdentityIdFormat_resource,
    describeIdentityIdFormat_principalArn,
    describeIdentityIdFormatResponse_statuses,
    describeIdentityIdFormatResponse_httpStatus,

    -- ** DescribeImageAttribute
    describeImageAttribute_dryRun,
    describeImageAttribute_attribute,
    describeImageAttribute_imageId,
    describeImageAttributeResponse_sriovNetSupport,
    describeImageAttributeResponse_blockDeviceMappings,
    describeImageAttributeResponse_lastLaunchedTime,
    describeImageAttributeResponse_productCodes,
    describeImageAttributeResponse_description,
    describeImageAttributeResponse_tpmSupport,
    describeImageAttributeResponse_ramdiskId,
    describeImageAttributeResponse_launchPermissions,
    describeImageAttributeResponse_imdsSupport,
    describeImageAttributeResponse_bootMode,
    describeImageAttributeResponse_kernelId,
    describeImageAttributeResponse_uefiData,
    describeImageAttributeResponse_imageId,
    describeImageAttributeResponse_httpStatus,

    -- ** DescribeImages
    describeImages_imageIds,
    describeImages_filters,
    describeImages_owners,
    describeImages_dryRun,
    describeImages_includeDeprecated,
    describeImages_executableUsers,
    describeImagesResponse_images,
    describeImagesResponse_httpStatus,

    -- ** DescribeImportImageTasks
    describeImportImageTasks_nextToken,
    describeImportImageTasks_importTaskIds,
    describeImportImageTasks_filters,
    describeImportImageTasks_dryRun,
    describeImportImageTasks_maxResults,
    describeImportImageTasksResponse_nextToken,
    describeImportImageTasksResponse_importImageTasks,
    describeImportImageTasksResponse_httpStatus,

    -- ** DescribeImportSnapshotTasks
    describeImportSnapshotTasks_nextToken,
    describeImportSnapshotTasks_importTaskIds,
    describeImportSnapshotTasks_filters,
    describeImportSnapshotTasks_dryRun,
    describeImportSnapshotTasks_maxResults,
    describeImportSnapshotTasksResponse_nextToken,
    describeImportSnapshotTasksResponse_importSnapshotTasks,
    describeImportSnapshotTasksResponse_httpStatus,

    -- ** DescribeInstanceAttribute
    describeInstanceAttribute_dryRun,
    describeInstanceAttribute_attribute,
    describeInstanceAttribute_instanceId,
    describeInstanceAttributeResponse_ebsOptimized,
    describeInstanceAttributeResponse_sriovNetSupport,
    describeInstanceAttributeResponse_userData,
    describeInstanceAttributeResponse_blockDeviceMappings,
    describeInstanceAttributeResponse_sourceDestCheck,
    describeInstanceAttributeResponse_instanceInitiatedShutdownBehavior,
    describeInstanceAttributeResponse_productCodes,
    describeInstanceAttributeResponse_instanceType,
    describeInstanceAttributeResponse_instanceId,
    describeInstanceAttributeResponse_ramdiskId,
    describeInstanceAttributeResponse_kernelId,
    describeInstanceAttributeResponse_disableApiTermination,
    describeInstanceAttributeResponse_groups,
    describeInstanceAttributeResponse_disableApiStop,
    describeInstanceAttributeResponse_enaSupport,
    describeInstanceAttributeResponse_rootDeviceName,
    describeInstanceAttributeResponse_enclaveOptions,
    describeInstanceAttributeResponse_httpStatus,

    -- ** DescribeInstanceCreditSpecifications
    describeInstanceCreditSpecifications_nextToken,
    describeInstanceCreditSpecifications_filters,
    describeInstanceCreditSpecifications_dryRun,
    describeInstanceCreditSpecifications_maxResults,
    describeInstanceCreditSpecifications_instanceIds,
    describeInstanceCreditSpecificationsResponse_nextToken,
    describeInstanceCreditSpecificationsResponse_instanceCreditSpecifications,
    describeInstanceCreditSpecificationsResponse_httpStatus,

    -- ** DescribeInstanceEventNotificationAttributes
    describeInstanceEventNotificationAttributes_dryRun,
    describeInstanceEventNotificationAttributesResponse_instanceTagAttribute,
    describeInstanceEventNotificationAttributesResponse_httpStatus,

    -- ** DescribeInstanceEventWindows
    describeInstanceEventWindows_nextToken,
    describeInstanceEventWindows_filters,
    describeInstanceEventWindows_instanceEventWindowIds,
    describeInstanceEventWindows_dryRun,
    describeInstanceEventWindows_maxResults,
    describeInstanceEventWindowsResponse_nextToken,
    describeInstanceEventWindowsResponse_instanceEventWindows,
    describeInstanceEventWindowsResponse_httpStatus,

    -- ** DescribeInstanceStatus
    describeInstanceStatus_nextToken,
    describeInstanceStatus_filters,
    describeInstanceStatus_includeAllInstances,
    describeInstanceStatus_dryRun,
    describeInstanceStatus_maxResults,
    describeInstanceStatus_instanceIds,
    describeInstanceStatusResponse_nextToken,
    describeInstanceStatusResponse_instanceStatuses,
    describeInstanceStatusResponse_httpStatus,

    -- ** DescribeInstanceTypeOfferings
    describeInstanceTypeOfferings_nextToken,
    describeInstanceTypeOfferings_filters,
    describeInstanceTypeOfferings_dryRun,
    describeInstanceTypeOfferings_maxResults,
    describeInstanceTypeOfferings_locationType,
    describeInstanceTypeOfferingsResponse_nextToken,
    describeInstanceTypeOfferingsResponse_instanceTypeOfferings,
    describeInstanceTypeOfferingsResponse_httpStatus,

    -- ** DescribeInstanceTypes
    describeInstanceTypes_nextToken,
    describeInstanceTypes_instanceTypes,
    describeInstanceTypes_filters,
    describeInstanceTypes_dryRun,
    describeInstanceTypes_maxResults,
    describeInstanceTypesResponse_nextToken,
    describeInstanceTypesResponse_instanceTypes,
    describeInstanceTypesResponse_httpStatus,

    -- ** DescribeInstances
    describeInstances_nextToken,
    describeInstances_filters,
    describeInstances_dryRun,
    describeInstances_maxResults,
    describeInstances_instanceIds,
    describeInstancesResponse_nextToken,
    describeInstancesResponse_reservations,
    describeInstancesResponse_httpStatus,

    -- ** DescribeInternetGateways
    describeInternetGateways_nextToken,
    describeInternetGateways_filters,
    describeInternetGateways_dryRun,
    describeInternetGateways_maxResults,
    describeInternetGateways_internetGatewayIds,
    describeInternetGatewaysResponse_nextToken,
    describeInternetGatewaysResponse_internetGateways,
    describeInternetGatewaysResponse_httpStatus,

    -- ** DescribeIpamPools
    describeIpamPools_ipamPoolIds,
    describeIpamPools_nextToken,
    describeIpamPools_filters,
    describeIpamPools_dryRun,
    describeIpamPools_maxResults,
    describeIpamPoolsResponse_nextToken,
    describeIpamPoolsResponse_ipamPools,
    describeIpamPoolsResponse_httpStatus,

    -- ** DescribeIpamScopes
    describeIpamScopes_nextToken,
    describeIpamScopes_filters,
    describeIpamScopes_dryRun,
    describeIpamScopes_maxResults,
    describeIpamScopes_ipamScopeIds,
    describeIpamScopesResponse_nextToken,
    describeIpamScopesResponse_ipamScopes,
    describeIpamScopesResponse_httpStatus,

    -- ** DescribeIpams
    describeIpams_nextToken,
    describeIpams_filters,
    describeIpams_dryRun,
    describeIpams_ipamIds,
    describeIpams_maxResults,
    describeIpamsResponse_ipams,
    describeIpamsResponse_nextToken,
    describeIpamsResponse_httpStatus,

    -- ** DescribeIpv6Pools
    describeIpv6Pools_nextToken,
    describeIpv6Pools_poolIds,
    describeIpv6Pools_filters,
    describeIpv6Pools_dryRun,
    describeIpv6Pools_maxResults,
    describeIpv6PoolsResponse_nextToken,
    describeIpv6PoolsResponse_ipv6Pools,
    describeIpv6PoolsResponse_httpStatus,

    -- ** DescribeKeyPairs
    describeKeyPairs_keyPairIds,
    describeKeyPairs_includePublicKey,
    describeKeyPairs_filters,
    describeKeyPairs_dryRun,
    describeKeyPairs_keyNames,
    describeKeyPairsResponse_keyPairs,
    describeKeyPairsResponse_httpStatus,

    -- ** DescribeLaunchTemplateVersions
    describeLaunchTemplateVersions_nextToken,
    describeLaunchTemplateVersions_minVersion,
    describeLaunchTemplateVersions_filters,
    describeLaunchTemplateVersions_dryRun,
    describeLaunchTemplateVersions_launchTemplateId,
    describeLaunchTemplateVersions_maxResults,
    describeLaunchTemplateVersions_versions,
    describeLaunchTemplateVersions_maxVersion,
    describeLaunchTemplateVersions_launchTemplateName,
    describeLaunchTemplateVersionsResponse_nextToken,
    describeLaunchTemplateVersionsResponse_launchTemplateVersions,
    describeLaunchTemplateVersionsResponse_httpStatus,

    -- ** DescribeLaunchTemplates
    describeLaunchTemplates_nextToken,
    describeLaunchTemplates_launchTemplateNames,
    describeLaunchTemplates_filters,
    describeLaunchTemplates_dryRun,
    describeLaunchTemplates_maxResults,
    describeLaunchTemplates_launchTemplateIds,
    describeLaunchTemplatesResponse_nextToken,
    describeLaunchTemplatesResponse_launchTemplates,
    describeLaunchTemplatesResponse_httpStatus,

    -- ** DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
    describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_nextToken,
    describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_filters,
    describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_dryRun,
    describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_maxResults,
    describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_localGatewayRouteTableVirtualInterfaceGroupAssociationIds,
    describeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse_nextToken,
    describeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse_localGatewayRouteTableVirtualInterfaceGroupAssociations,
    describeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse_httpStatus,

    -- ** DescribeLocalGatewayRouteTableVpcAssociations
    describeLocalGatewayRouteTableVpcAssociations_nextToken,
    describeLocalGatewayRouteTableVpcAssociations_localGatewayRouteTableVpcAssociationIds,
    describeLocalGatewayRouteTableVpcAssociations_filters,
    describeLocalGatewayRouteTableVpcAssociations_dryRun,
    describeLocalGatewayRouteTableVpcAssociations_maxResults,
    describeLocalGatewayRouteTableVpcAssociationsResponse_nextToken,
    describeLocalGatewayRouteTableVpcAssociationsResponse_localGatewayRouteTableVpcAssociations,
    describeLocalGatewayRouteTableVpcAssociationsResponse_httpStatus,

    -- ** DescribeLocalGatewayRouteTables
    describeLocalGatewayRouteTables_localGatewayRouteTableIds,
    describeLocalGatewayRouteTables_nextToken,
    describeLocalGatewayRouteTables_filters,
    describeLocalGatewayRouteTables_dryRun,
    describeLocalGatewayRouteTables_maxResults,
    describeLocalGatewayRouteTablesResponse_nextToken,
    describeLocalGatewayRouteTablesResponse_localGatewayRouteTables,
    describeLocalGatewayRouteTablesResponse_httpStatus,

    -- ** DescribeLocalGatewayVirtualInterfaceGroups
    describeLocalGatewayVirtualInterfaceGroups_nextToken,
    describeLocalGatewayVirtualInterfaceGroups_filters,
    describeLocalGatewayVirtualInterfaceGroups_dryRun,
    describeLocalGatewayVirtualInterfaceGroups_maxResults,
    describeLocalGatewayVirtualInterfaceGroups_localGatewayVirtualInterfaceGroupIds,
    describeLocalGatewayVirtualInterfaceGroupsResponse_nextToken,
    describeLocalGatewayVirtualInterfaceGroupsResponse_localGatewayVirtualInterfaceGroups,
    describeLocalGatewayVirtualInterfaceGroupsResponse_httpStatus,

    -- ** DescribeLocalGatewayVirtualInterfaces
    describeLocalGatewayVirtualInterfaces_nextToken,
    describeLocalGatewayVirtualInterfaces_filters,
    describeLocalGatewayVirtualInterfaces_localGatewayVirtualInterfaceIds,
    describeLocalGatewayVirtualInterfaces_dryRun,
    describeLocalGatewayVirtualInterfaces_maxResults,
    describeLocalGatewayVirtualInterfacesResponse_nextToken,
    describeLocalGatewayVirtualInterfacesResponse_localGatewayVirtualInterfaces,
    describeLocalGatewayVirtualInterfacesResponse_httpStatus,

    -- ** DescribeLocalGateways
    describeLocalGateways_nextToken,
    describeLocalGateways_filters,
    describeLocalGateways_localGatewayIds,
    describeLocalGateways_dryRun,
    describeLocalGateways_maxResults,
    describeLocalGatewaysResponse_nextToken,
    describeLocalGatewaysResponse_localGateways,
    describeLocalGatewaysResponse_httpStatus,

    -- ** DescribeManagedPrefixLists
    describeManagedPrefixLists_nextToken,
    describeManagedPrefixLists_filters,
    describeManagedPrefixLists_dryRun,
    describeManagedPrefixLists_maxResults,
    describeManagedPrefixLists_prefixListIds,
    describeManagedPrefixListsResponse_nextToken,
    describeManagedPrefixListsResponse_prefixLists,
    describeManagedPrefixListsResponse_httpStatus,

    -- ** DescribeMovingAddresses
    describeMovingAddresses_nextToken,
    describeMovingAddresses_filters,
    describeMovingAddresses_publicIps,
    describeMovingAddresses_dryRun,
    describeMovingAddresses_maxResults,
    describeMovingAddressesResponse_nextToken,
    describeMovingAddressesResponse_movingAddressStatuses,
    describeMovingAddressesResponse_httpStatus,

    -- ** DescribeNatGateways
    describeNatGateways_nextToken,
    describeNatGateways_dryRun,
    describeNatGateways_filter,
    describeNatGateways_natGatewayIds,
    describeNatGateways_maxResults,
    describeNatGatewaysResponse_nextToken,
    describeNatGatewaysResponse_natGateways,
    describeNatGatewaysResponse_httpStatus,

    -- ** DescribeNetworkAcls
    describeNetworkAcls_nextToken,
    describeNetworkAcls_filters,
    describeNetworkAcls_dryRun,
    describeNetworkAcls_networkAclIds,
    describeNetworkAcls_maxResults,
    describeNetworkAclsResponse_nextToken,
    describeNetworkAclsResponse_networkAcls,
    describeNetworkAclsResponse_httpStatus,

    -- ** DescribeNetworkInsightsAccessScopeAnalyses
    describeNetworkInsightsAccessScopeAnalyses_nextToken,
    describeNetworkInsightsAccessScopeAnalyses_analysisStartTimeEnd,
    describeNetworkInsightsAccessScopeAnalyses_networkInsightsAccessScopeId,
    describeNetworkInsightsAccessScopeAnalyses_filters,
    describeNetworkInsightsAccessScopeAnalyses_analysisStartTimeBegin,
    describeNetworkInsightsAccessScopeAnalyses_dryRun,
    describeNetworkInsightsAccessScopeAnalyses_maxResults,
    describeNetworkInsightsAccessScopeAnalyses_networkInsightsAccessScopeAnalysisIds,
    describeNetworkInsightsAccessScopeAnalysesResponse_nextToken,
    describeNetworkInsightsAccessScopeAnalysesResponse_networkInsightsAccessScopeAnalyses,
    describeNetworkInsightsAccessScopeAnalysesResponse_httpStatus,

    -- ** DescribeNetworkInsightsAccessScopes
    describeNetworkInsightsAccessScopes_nextToken,
    describeNetworkInsightsAccessScopes_filters,
    describeNetworkInsightsAccessScopes_dryRun,
    describeNetworkInsightsAccessScopes_networkInsightsAccessScopeIds,
    describeNetworkInsightsAccessScopes_maxResults,
    describeNetworkInsightsAccessScopesResponse_nextToken,
    describeNetworkInsightsAccessScopesResponse_networkInsightsAccessScopes,
    describeNetworkInsightsAccessScopesResponse_httpStatus,

    -- ** DescribeNetworkInsightsAnalyses
    describeNetworkInsightsAnalyses_nextToken,
    describeNetworkInsightsAnalyses_filters,
    describeNetworkInsightsAnalyses_dryRun,
    describeNetworkInsightsAnalyses_analysisStartTime,
    describeNetworkInsightsAnalyses_analysisEndTime,
    describeNetworkInsightsAnalyses_networkInsightsAnalysisIds,
    describeNetworkInsightsAnalyses_maxResults,
    describeNetworkInsightsAnalyses_networkInsightsPathId,
    describeNetworkInsightsAnalysesResponse_nextToken,
    describeNetworkInsightsAnalysesResponse_networkInsightsAnalyses,
    describeNetworkInsightsAnalysesResponse_httpStatus,

    -- ** DescribeNetworkInsightsPaths
    describeNetworkInsightsPaths_nextToken,
    describeNetworkInsightsPaths_filters,
    describeNetworkInsightsPaths_dryRun,
    describeNetworkInsightsPaths_maxResults,
    describeNetworkInsightsPaths_networkInsightsPathIds,
    describeNetworkInsightsPathsResponse_nextToken,
    describeNetworkInsightsPathsResponse_networkInsightsPaths,
    describeNetworkInsightsPathsResponse_httpStatus,

    -- ** DescribeNetworkInterfaceAttribute
    describeNetworkInterfaceAttribute_attribute,
    describeNetworkInterfaceAttribute_dryRun,
    describeNetworkInterfaceAttribute_networkInterfaceId,
    describeNetworkInterfaceAttributeResponse_attachment,
    describeNetworkInterfaceAttributeResponse_sourceDestCheck,
    describeNetworkInterfaceAttributeResponse_description,
    describeNetworkInterfaceAttributeResponse_networkInterfaceId,
    describeNetworkInterfaceAttributeResponse_groups,
    describeNetworkInterfaceAttributeResponse_httpStatus,

    -- ** DescribeNetworkInterfacePermissions
    describeNetworkInterfacePermissions_networkInterfacePermissionIds,
    describeNetworkInterfacePermissions_nextToken,
    describeNetworkInterfacePermissions_filters,
    describeNetworkInterfacePermissions_maxResults,
    describeNetworkInterfacePermissionsResponse_nextToken,
    describeNetworkInterfacePermissionsResponse_networkInterfacePermissions,
    describeNetworkInterfacePermissionsResponse_httpStatus,

    -- ** DescribeNetworkInterfaces
    describeNetworkInterfaces_nextToken,
    describeNetworkInterfaces_filters,
    describeNetworkInterfaces_dryRun,
    describeNetworkInterfaces_maxResults,
    describeNetworkInterfaces_networkInterfaceIds,
    describeNetworkInterfacesResponse_nextToken,
    describeNetworkInterfacesResponse_networkInterfaces,
    describeNetworkInterfacesResponse_httpStatus,

    -- ** DescribePlacementGroups
    describePlacementGroups_filters,
    describePlacementGroups_dryRun,
    describePlacementGroups_groupIds,
    describePlacementGroups_groupNames,
    describePlacementGroupsResponse_placementGroups,
    describePlacementGroupsResponse_httpStatus,

    -- ** DescribePrefixLists
    describePrefixLists_nextToken,
    describePrefixLists_filters,
    describePrefixLists_dryRun,
    describePrefixLists_maxResults,
    describePrefixLists_prefixListIds,
    describePrefixListsResponse_nextToken,
    describePrefixListsResponse_prefixLists,
    describePrefixListsResponse_httpStatus,

    -- ** DescribePrincipalIdFormat
    describePrincipalIdFormat_nextToken,
    describePrincipalIdFormat_dryRun,
    describePrincipalIdFormat_maxResults,
    describePrincipalIdFormat_resources,
    describePrincipalIdFormatResponse_nextToken,
    describePrincipalIdFormatResponse_principals,
    describePrincipalIdFormatResponse_httpStatus,

    -- ** DescribePublicIpv4Pools
    describePublicIpv4Pools_nextToken,
    describePublicIpv4Pools_poolIds,
    describePublicIpv4Pools_filters,
    describePublicIpv4Pools_maxResults,
    describePublicIpv4PoolsResponse_nextToken,
    describePublicIpv4PoolsResponse_publicIpv4Pools,
    describePublicIpv4PoolsResponse_httpStatus,

    -- ** DescribeRegions
    describeRegions_regionNames,
    describeRegions_filters,
    describeRegions_dryRun,
    describeRegions_allRegions,
    describeRegionsResponse_regions,
    describeRegionsResponse_httpStatus,

    -- ** DescribeReplaceRootVolumeTasks
    describeReplaceRootVolumeTasks_nextToken,
    describeReplaceRootVolumeTasks_replaceRootVolumeTaskIds,
    describeReplaceRootVolumeTasks_filters,
    describeReplaceRootVolumeTasks_dryRun,
    describeReplaceRootVolumeTasks_maxResults,
    describeReplaceRootVolumeTasksResponse_nextToken,
    describeReplaceRootVolumeTasksResponse_replaceRootVolumeTasks,
    describeReplaceRootVolumeTasksResponse_httpStatus,

    -- ** DescribeReservedInstances
    describeReservedInstances_offeringClass,
    describeReservedInstances_reservedInstancesIds,
    describeReservedInstances_filters,
    describeReservedInstances_offeringType,
    describeReservedInstances_dryRun,
    describeReservedInstancesResponse_reservedInstances,
    describeReservedInstancesResponse_httpStatus,

    -- ** DescribeReservedInstancesListings
    describeReservedInstancesListings_reservedInstancesId,
    describeReservedInstancesListings_filters,
    describeReservedInstancesListings_reservedInstancesListingId,
    describeReservedInstancesListingsResponse_reservedInstancesListings,
    describeReservedInstancesListingsResponse_httpStatus,

    -- ** DescribeReservedInstancesModifications
    describeReservedInstancesModifications_reservedInstancesModificationIds,
    describeReservedInstancesModifications_nextToken,
    describeReservedInstancesModifications_filters,
    describeReservedInstancesModificationsResponse_nextToken,
    describeReservedInstancesModificationsResponse_reservedInstancesModifications,
    describeReservedInstancesModificationsResponse_httpStatus,

    -- ** DescribeReservedInstancesOfferings
    describeReservedInstancesOfferings_offeringClass,
    describeReservedInstancesOfferings_maxDuration,
    describeReservedInstancesOfferings_nextToken,
    describeReservedInstancesOfferings_minDuration,
    describeReservedInstancesOfferings_instanceTenancy,
    describeReservedInstancesOfferings_filters,
    describeReservedInstancesOfferings_reservedInstancesOfferingIds,
    describeReservedInstancesOfferings_offeringType,
    describeReservedInstancesOfferings_availabilityZone,
    describeReservedInstancesOfferings_dryRun,
    describeReservedInstancesOfferings_instanceType,
    describeReservedInstancesOfferings_maxResults,
    describeReservedInstancesOfferings_productDescription,
    describeReservedInstancesOfferings_maxInstanceCount,
    describeReservedInstancesOfferings_includeMarketplace,
    describeReservedInstancesOfferingsResponse_nextToken,
    describeReservedInstancesOfferingsResponse_reservedInstancesOfferings,
    describeReservedInstancesOfferingsResponse_httpStatus,

    -- ** DescribeRouteTables
    describeRouteTables_nextToken,
    describeRouteTables_filters,
    describeRouteTables_routeTableIds,
    describeRouteTables_dryRun,
    describeRouteTables_maxResults,
    describeRouteTablesResponse_nextToken,
    describeRouteTablesResponse_routeTables,
    describeRouteTablesResponse_httpStatus,

    -- ** DescribeScheduledInstanceAvailability
    describeScheduledInstanceAvailability_nextToken,
    describeScheduledInstanceAvailability_filters,
    describeScheduledInstanceAvailability_dryRun,
    describeScheduledInstanceAvailability_maxResults,
    describeScheduledInstanceAvailability_maxSlotDurationInHours,
    describeScheduledInstanceAvailability_minSlotDurationInHours,
    describeScheduledInstanceAvailability_firstSlotStartTimeRange,
    describeScheduledInstanceAvailability_recurrence,
    describeScheduledInstanceAvailabilityResponse_nextToken,
    describeScheduledInstanceAvailabilityResponse_scheduledInstanceAvailabilitySet,
    describeScheduledInstanceAvailabilityResponse_httpStatus,

    -- ** DescribeScheduledInstances
    describeScheduledInstances_nextToken,
    describeScheduledInstances_slotStartTimeRange,
    describeScheduledInstances_filters,
    describeScheduledInstances_dryRun,
    describeScheduledInstances_maxResults,
    describeScheduledInstances_scheduledInstanceIds,
    describeScheduledInstancesResponse_scheduledInstanceSet,
    describeScheduledInstancesResponse_nextToken,
    describeScheduledInstancesResponse_httpStatus,

    -- ** DescribeSecurityGroupReferences
    describeSecurityGroupReferences_dryRun,
    describeSecurityGroupReferences_groupId,
    describeSecurityGroupReferencesResponse_securityGroupReferenceSet,
    describeSecurityGroupReferencesResponse_httpStatus,

    -- ** DescribeSecurityGroupRules
    describeSecurityGroupRules_nextToken,
    describeSecurityGroupRules_filters,
    describeSecurityGroupRules_dryRun,
    describeSecurityGroupRules_securityGroupRuleIds,
    describeSecurityGroupRules_maxResults,
    describeSecurityGroupRulesResponse_securityGroupRules,
    describeSecurityGroupRulesResponse_nextToken,
    describeSecurityGroupRulesResponse_httpStatus,

    -- ** DescribeSecurityGroups
    describeSecurityGroups_nextToken,
    describeSecurityGroups_filters,
    describeSecurityGroups_dryRun,
    describeSecurityGroups_maxResults,
    describeSecurityGroups_groupIds,
    describeSecurityGroups_groupNames,
    describeSecurityGroupsResponse_nextToken,
    describeSecurityGroupsResponse_securityGroups,
    describeSecurityGroupsResponse_httpStatus,

    -- ** DescribeSnapshotAttribute
    describeSnapshotAttribute_dryRun,
    describeSnapshotAttribute_attribute,
    describeSnapshotAttribute_snapshotId,
    describeSnapshotAttributeResponse_snapshotId,
    describeSnapshotAttributeResponse_productCodes,
    describeSnapshotAttributeResponse_createVolumePermissions,
    describeSnapshotAttributeResponse_httpStatus,

    -- ** DescribeSnapshotTierStatus
    describeSnapshotTierStatus_nextToken,
    describeSnapshotTierStatus_filters,
    describeSnapshotTierStatus_dryRun,
    describeSnapshotTierStatus_maxResults,
    describeSnapshotTierStatusResponse_nextToken,
    describeSnapshotTierStatusResponse_snapshotTierStatuses,
    describeSnapshotTierStatusResponse_httpStatus,

    -- ** DescribeSnapshots
    describeSnapshots_nextToken,
    describeSnapshots_restorableByUserIds,
    describeSnapshots_filters,
    describeSnapshots_snapshotIds,
    describeSnapshots_dryRun,
    describeSnapshots_maxResults,
    describeSnapshots_ownerIds,
    describeSnapshotsResponse_nextToken,
    describeSnapshotsResponse_snapshots,
    describeSnapshotsResponse_httpStatus,

    -- ** DescribeSpotDatafeedSubscription
    describeSpotDatafeedSubscription_dryRun,
    describeSpotDatafeedSubscriptionResponse_spotDatafeedSubscription,
    describeSpotDatafeedSubscriptionResponse_httpStatus,

    -- ** DescribeSpotFleetInstances
    describeSpotFleetInstances_nextToken,
    describeSpotFleetInstances_dryRun,
    describeSpotFleetInstances_maxResults,
    describeSpotFleetInstances_spotFleetRequestId,
    describeSpotFleetInstancesResponse_activeInstances,
    describeSpotFleetInstancesResponse_nextToken,
    describeSpotFleetInstancesResponse_spotFleetRequestId,
    describeSpotFleetInstancesResponse_httpStatus,

    -- ** DescribeSpotFleetRequestHistory
    describeSpotFleetRequestHistory_eventType,
    describeSpotFleetRequestHistory_nextToken,
    describeSpotFleetRequestHistory_dryRun,
    describeSpotFleetRequestHistory_maxResults,
    describeSpotFleetRequestHistory_spotFleetRequestId,
    describeSpotFleetRequestHistory_startTime,
    describeSpotFleetRequestHistoryResponse_nextToken,
    describeSpotFleetRequestHistoryResponse_historyRecords,
    describeSpotFleetRequestHistoryResponse_spotFleetRequestId,
    describeSpotFleetRequestHistoryResponse_startTime,
    describeSpotFleetRequestHistoryResponse_lastEvaluatedTime,
    describeSpotFleetRequestHistoryResponse_httpStatus,

    -- ** DescribeSpotFleetRequests
    describeSpotFleetRequests_nextToken,
    describeSpotFleetRequests_dryRun,
    describeSpotFleetRequests_maxResults,
    describeSpotFleetRequests_spotFleetRequestIds,
    describeSpotFleetRequestsResponse_nextToken,
    describeSpotFleetRequestsResponse_spotFleetRequestConfigs,
    describeSpotFleetRequestsResponse_httpStatus,

    -- ** DescribeSpotInstanceRequests
    describeSpotInstanceRequests_nextToken,
    describeSpotInstanceRequests_filters,
    describeSpotInstanceRequests_dryRun,
    describeSpotInstanceRequests_maxResults,
    describeSpotInstanceRequests_spotInstanceRequestIds,
    describeSpotInstanceRequestsResponse_nextToken,
    describeSpotInstanceRequestsResponse_spotInstanceRequests,
    describeSpotInstanceRequestsResponse_httpStatus,

    -- ** DescribeSpotPriceHistory
    describeSpotPriceHistory_nextToken,
    describeSpotPriceHistory_instanceTypes,
    describeSpotPriceHistory_productDescriptions,
    describeSpotPriceHistory_filters,
    describeSpotPriceHistory_availabilityZone,
    describeSpotPriceHistory_endTime,
    describeSpotPriceHistory_dryRun,
    describeSpotPriceHistory_maxResults,
    describeSpotPriceHistory_startTime,
    describeSpotPriceHistoryResponse_nextToken,
    describeSpotPriceHistoryResponse_spotPriceHistory,
    describeSpotPriceHistoryResponse_httpStatus,

    -- ** DescribeStaleSecurityGroups
    describeStaleSecurityGroups_nextToken,
    describeStaleSecurityGroups_dryRun,
    describeStaleSecurityGroups_maxResults,
    describeStaleSecurityGroups_vpcId,
    describeStaleSecurityGroupsResponse_nextToken,
    describeStaleSecurityGroupsResponse_staleSecurityGroupSet,
    describeStaleSecurityGroupsResponse_httpStatus,

    -- ** DescribeStoreImageTasks
    describeStoreImageTasks_nextToken,
    describeStoreImageTasks_imageIds,
    describeStoreImageTasks_filters,
    describeStoreImageTasks_dryRun,
    describeStoreImageTasks_maxResults,
    describeStoreImageTasksResponse_nextToken,
    describeStoreImageTasksResponse_storeImageTaskResults,
    describeStoreImageTasksResponse_httpStatus,

    -- ** DescribeSubnets
    describeSubnets_nextToken,
    describeSubnets_filters,
    describeSubnets_dryRun,
    describeSubnets_maxResults,
    describeSubnets_subnetIds,
    describeSubnetsResponse_nextToken,
    describeSubnetsResponse_subnets,
    describeSubnetsResponse_httpStatus,

    -- ** DescribeTags
    describeTags_nextToken,
    describeTags_filters,
    describeTags_dryRun,
    describeTags_maxResults,
    describeTagsResponse_tags,
    describeTagsResponse_nextToken,
    describeTagsResponse_httpStatus,

    -- ** DescribeTrafficMirrorFilters
    describeTrafficMirrorFilters_nextToken,
    describeTrafficMirrorFilters_filters,
    describeTrafficMirrorFilters_trafficMirrorFilterIds,
    describeTrafficMirrorFilters_dryRun,
    describeTrafficMirrorFilters_maxResults,
    describeTrafficMirrorFiltersResponse_nextToken,
    describeTrafficMirrorFiltersResponse_trafficMirrorFilters,
    describeTrafficMirrorFiltersResponse_httpStatus,

    -- ** DescribeTrafficMirrorSessions
    describeTrafficMirrorSessions_nextToken,
    describeTrafficMirrorSessions_trafficMirrorSessionIds,
    describeTrafficMirrorSessions_filters,
    describeTrafficMirrorSessions_dryRun,
    describeTrafficMirrorSessions_maxResults,
    describeTrafficMirrorSessionsResponse_nextToken,
    describeTrafficMirrorSessionsResponse_trafficMirrorSessions,
    describeTrafficMirrorSessionsResponse_httpStatus,

    -- ** DescribeTrafficMirrorTargets
    describeTrafficMirrorTargets_nextToken,
    describeTrafficMirrorTargets_filters,
    describeTrafficMirrorTargets_dryRun,
    describeTrafficMirrorTargets_trafficMirrorTargetIds,
    describeTrafficMirrorTargets_maxResults,
    describeTrafficMirrorTargetsResponse_nextToken,
    describeTrafficMirrorTargetsResponse_trafficMirrorTargets,
    describeTrafficMirrorTargetsResponse_httpStatus,

    -- ** DescribeTransitGatewayAttachments
    describeTransitGatewayAttachments_nextToken,
    describeTransitGatewayAttachments_filters,
    describeTransitGatewayAttachments_dryRun,
    describeTransitGatewayAttachments_maxResults,
    describeTransitGatewayAttachments_transitGatewayAttachmentIds,
    describeTransitGatewayAttachmentsResponse_nextToken,
    describeTransitGatewayAttachmentsResponse_transitGatewayAttachments,
    describeTransitGatewayAttachmentsResponse_httpStatus,

    -- ** DescribeTransitGatewayConnectPeers
    describeTransitGatewayConnectPeers_nextToken,
    describeTransitGatewayConnectPeers_transitGatewayConnectPeerIds,
    describeTransitGatewayConnectPeers_filters,
    describeTransitGatewayConnectPeers_dryRun,
    describeTransitGatewayConnectPeers_maxResults,
    describeTransitGatewayConnectPeersResponse_nextToken,
    describeTransitGatewayConnectPeersResponse_transitGatewayConnectPeers,
    describeTransitGatewayConnectPeersResponse_httpStatus,

    -- ** DescribeTransitGatewayConnects
    describeTransitGatewayConnects_nextToken,
    describeTransitGatewayConnects_filters,
    describeTransitGatewayConnects_dryRun,
    describeTransitGatewayConnects_maxResults,
    describeTransitGatewayConnects_transitGatewayAttachmentIds,
    describeTransitGatewayConnectsResponse_nextToken,
    describeTransitGatewayConnectsResponse_transitGatewayConnects,
    describeTransitGatewayConnectsResponse_httpStatus,

    -- ** DescribeTransitGatewayMulticastDomains
    describeTransitGatewayMulticastDomains_nextToken,
    describeTransitGatewayMulticastDomains_filters,
    describeTransitGatewayMulticastDomains_dryRun,
    describeTransitGatewayMulticastDomains_maxResults,
    describeTransitGatewayMulticastDomains_transitGatewayMulticastDomainIds,
    describeTransitGatewayMulticastDomainsResponse_nextToken,
    describeTransitGatewayMulticastDomainsResponse_transitGatewayMulticastDomains,
    describeTransitGatewayMulticastDomainsResponse_httpStatus,

    -- ** DescribeTransitGatewayPeeringAttachments
    describeTransitGatewayPeeringAttachments_nextToken,
    describeTransitGatewayPeeringAttachments_filters,
    describeTransitGatewayPeeringAttachments_dryRun,
    describeTransitGatewayPeeringAttachments_maxResults,
    describeTransitGatewayPeeringAttachments_transitGatewayAttachmentIds,
    describeTransitGatewayPeeringAttachmentsResponse_nextToken,
    describeTransitGatewayPeeringAttachmentsResponse_transitGatewayPeeringAttachments,
    describeTransitGatewayPeeringAttachmentsResponse_httpStatus,

    -- ** DescribeTransitGatewayPolicyTables
    describeTransitGatewayPolicyTables_nextToken,
    describeTransitGatewayPolicyTables_filters,
    describeTransitGatewayPolicyTables_dryRun,
    describeTransitGatewayPolicyTables_transitGatewayPolicyTableIds,
    describeTransitGatewayPolicyTables_maxResults,
    describeTransitGatewayPolicyTablesResponse_nextToken,
    describeTransitGatewayPolicyTablesResponse_transitGatewayPolicyTables,
    describeTransitGatewayPolicyTablesResponse_httpStatus,

    -- ** DescribeTransitGatewayRouteTableAnnouncements
    describeTransitGatewayRouteTableAnnouncements_nextToken,
    describeTransitGatewayRouteTableAnnouncements_filters,
    describeTransitGatewayRouteTableAnnouncements_dryRun,
    describeTransitGatewayRouteTableAnnouncements_maxResults,
    describeTransitGatewayRouteTableAnnouncements_transitGatewayRouteTableAnnouncementIds,
    describeTransitGatewayRouteTableAnnouncementsResponse_nextToken,
    describeTransitGatewayRouteTableAnnouncementsResponse_transitGatewayRouteTableAnnouncements,
    describeTransitGatewayRouteTableAnnouncementsResponse_httpStatus,

    -- ** DescribeTransitGatewayRouteTables
    describeTransitGatewayRouteTables_nextToken,
    describeTransitGatewayRouteTables_filters,
    describeTransitGatewayRouteTables_dryRun,
    describeTransitGatewayRouteTables_maxResults,
    describeTransitGatewayRouteTables_transitGatewayRouteTableIds,
    describeTransitGatewayRouteTablesResponse_nextToken,
    describeTransitGatewayRouteTablesResponse_transitGatewayRouteTables,
    describeTransitGatewayRouteTablesResponse_httpStatus,

    -- ** DescribeTransitGatewayVpcAttachments
    describeTransitGatewayVpcAttachments_nextToken,
    describeTransitGatewayVpcAttachments_filters,
    describeTransitGatewayVpcAttachments_dryRun,
    describeTransitGatewayVpcAttachments_maxResults,
    describeTransitGatewayVpcAttachments_transitGatewayAttachmentIds,
    describeTransitGatewayVpcAttachmentsResponse_nextToken,
    describeTransitGatewayVpcAttachmentsResponse_transitGatewayVpcAttachments,
    describeTransitGatewayVpcAttachmentsResponse_httpStatus,

    -- ** DescribeTransitGateways
    describeTransitGateways_nextToken,
    describeTransitGateways_filters,
    describeTransitGateways_dryRun,
    describeTransitGateways_transitGatewayIds,
    describeTransitGateways_maxResults,
    describeTransitGatewaysResponse_nextToken,
    describeTransitGatewaysResponse_transitGateways,
    describeTransitGatewaysResponse_httpStatus,

    -- ** DescribeTrunkInterfaceAssociations
    describeTrunkInterfaceAssociations_nextToken,
    describeTrunkInterfaceAssociations_filters,
    describeTrunkInterfaceAssociations_dryRun,
    describeTrunkInterfaceAssociations_maxResults,
    describeTrunkInterfaceAssociations_associationIds,
    describeTrunkInterfaceAssociationsResponse_nextToken,
    describeTrunkInterfaceAssociationsResponse_interfaceAssociations,
    describeTrunkInterfaceAssociationsResponse_httpStatus,

    -- ** DescribeVolumeAttribute
    describeVolumeAttribute_dryRun,
    describeVolumeAttribute_attribute,
    describeVolumeAttribute_volumeId,
    describeVolumeAttributeResponse_productCodes,
    describeVolumeAttributeResponse_volumeId,
    describeVolumeAttributeResponse_autoEnableIO,
    describeVolumeAttributeResponse_httpStatus,

    -- ** DescribeVolumeStatus
    describeVolumeStatus_nextToken,
    describeVolumeStatus_volumeIds,
    describeVolumeStatus_filters,
    describeVolumeStatus_dryRun,
    describeVolumeStatus_maxResults,
    describeVolumeStatusResponse_nextToken,
    describeVolumeStatusResponse_volumeStatuses,
    describeVolumeStatusResponse_httpStatus,

    -- ** DescribeVolumes
    describeVolumes_nextToken,
    describeVolumes_volumeIds,
    describeVolumes_filters,
    describeVolumes_dryRun,
    describeVolumes_maxResults,
    describeVolumesResponse_nextToken,
    describeVolumesResponse_volumes,
    describeVolumesResponse_httpStatus,

    -- ** DescribeVolumesModifications
    describeVolumesModifications_nextToken,
    describeVolumesModifications_volumeIds,
    describeVolumesModifications_filters,
    describeVolumesModifications_dryRun,
    describeVolumesModifications_maxResults,
    describeVolumesModificationsResponse_nextToken,
    describeVolumesModificationsResponse_volumesModifications,
    describeVolumesModificationsResponse_httpStatus,

    -- ** DescribeVpcAttribute
    describeVpcAttribute_dryRun,
    describeVpcAttribute_attribute,
    describeVpcAttribute_vpcId,
    describeVpcAttributeResponse_enableDnsSupport,
    describeVpcAttributeResponse_enableDnsHostnames,
    describeVpcAttributeResponse_vpcId,
    describeVpcAttributeResponse_enableNetworkAddressUsageMetrics,
    describeVpcAttributeResponse_httpStatus,

    -- ** DescribeVpcClassicLink
    describeVpcClassicLink_filters,
    describeVpcClassicLink_dryRun,
    describeVpcClassicLink_vpcIds,
    describeVpcClassicLinkResponse_vpcs,
    describeVpcClassicLinkResponse_httpStatus,

    -- ** DescribeVpcClassicLinkDnsSupport
    describeVpcClassicLinkDnsSupport_nextToken,
    describeVpcClassicLinkDnsSupport_vpcIds,
    describeVpcClassicLinkDnsSupport_maxResults,
    describeVpcClassicLinkDnsSupportResponse_nextToken,
    describeVpcClassicLinkDnsSupportResponse_vpcs,
    describeVpcClassicLinkDnsSupportResponse_httpStatus,

    -- ** DescribeVpcEndpointConnectionNotifications
    describeVpcEndpointConnectionNotifications_connectionNotificationId,
    describeVpcEndpointConnectionNotifications_nextToken,
    describeVpcEndpointConnectionNotifications_filters,
    describeVpcEndpointConnectionNotifications_dryRun,
    describeVpcEndpointConnectionNotifications_maxResults,
    describeVpcEndpointConnectionNotificationsResponse_nextToken,
    describeVpcEndpointConnectionNotificationsResponse_connectionNotificationSet,
    describeVpcEndpointConnectionNotificationsResponse_httpStatus,

    -- ** DescribeVpcEndpointConnections
    describeVpcEndpointConnections_nextToken,
    describeVpcEndpointConnections_filters,
    describeVpcEndpointConnections_dryRun,
    describeVpcEndpointConnections_maxResults,
    describeVpcEndpointConnectionsResponse_nextToken,
    describeVpcEndpointConnectionsResponse_vpcEndpointConnections,
    describeVpcEndpointConnectionsResponse_httpStatus,

    -- ** DescribeVpcEndpointServiceConfigurations
    describeVpcEndpointServiceConfigurations_nextToken,
    describeVpcEndpointServiceConfigurations_filters,
    describeVpcEndpointServiceConfigurations_dryRun,
    describeVpcEndpointServiceConfigurations_maxResults,
    describeVpcEndpointServiceConfigurations_serviceIds,
    describeVpcEndpointServiceConfigurationsResponse_nextToken,
    describeVpcEndpointServiceConfigurationsResponse_serviceConfigurations,
    describeVpcEndpointServiceConfigurationsResponse_httpStatus,

    -- ** DescribeVpcEndpointServicePermissions
    describeVpcEndpointServicePermissions_nextToken,
    describeVpcEndpointServicePermissions_filters,
    describeVpcEndpointServicePermissions_dryRun,
    describeVpcEndpointServicePermissions_maxResults,
    describeVpcEndpointServicePermissions_serviceId,
    describeVpcEndpointServicePermissionsResponse_nextToken,
    describeVpcEndpointServicePermissionsResponse_allowedPrincipals,
    describeVpcEndpointServicePermissionsResponse_httpStatus,

    -- ** DescribeVpcEndpointServices
    describeVpcEndpointServices_nextToken,
    describeVpcEndpointServices_filters,
    describeVpcEndpointServices_dryRun,
    describeVpcEndpointServices_serviceNames,
    describeVpcEndpointServices_maxResults,
    describeVpcEndpointServicesResponse_nextToken,
    describeVpcEndpointServicesResponse_serviceDetails,
    describeVpcEndpointServicesResponse_serviceNames,
    describeVpcEndpointServicesResponse_httpStatus,

    -- ** DescribeVpcEndpoints
    describeVpcEndpoints_nextToken,
    describeVpcEndpoints_filters,
    describeVpcEndpoints_vpcEndpointIds,
    describeVpcEndpoints_dryRun,
    describeVpcEndpoints_maxResults,
    describeVpcEndpointsResponse_nextToken,
    describeVpcEndpointsResponse_vpcEndpoints,
    describeVpcEndpointsResponse_httpStatus,

    -- ** DescribeVpcPeeringConnections
    describeVpcPeeringConnections_nextToken,
    describeVpcPeeringConnections_filters,
    describeVpcPeeringConnections_dryRun,
    describeVpcPeeringConnections_maxResults,
    describeVpcPeeringConnections_vpcPeeringConnectionIds,
    describeVpcPeeringConnectionsResponse_nextToken,
    describeVpcPeeringConnectionsResponse_vpcPeeringConnections,
    describeVpcPeeringConnectionsResponse_httpStatus,

    -- ** DescribeVpcs
    describeVpcs_nextToken,
    describeVpcs_filters,
    describeVpcs_dryRun,
    describeVpcs_vpcIds,
    describeVpcs_maxResults,
    describeVpcsResponse_nextToken,
    describeVpcsResponse_vpcs,
    describeVpcsResponse_httpStatus,

    -- ** DescribeVpnConnections
    describeVpnConnections_vpnConnectionIds,
    describeVpnConnections_filters,
    describeVpnConnections_dryRun,
    describeVpnConnectionsResponse_vpnConnections,
    describeVpnConnectionsResponse_httpStatus,

    -- ** DescribeVpnGateways
    describeVpnGateways_vpnGatewayIds,
    describeVpnGateways_filters,
    describeVpnGateways_dryRun,
    describeVpnGatewaysResponse_vpnGateways,
    describeVpnGatewaysResponse_httpStatus,

    -- ** DetachClassicLinkVpc
    detachClassicLinkVpc_dryRun,
    detachClassicLinkVpc_instanceId,
    detachClassicLinkVpc_vpcId,
    detachClassicLinkVpcResponse_return,
    detachClassicLinkVpcResponse_httpStatus,

    -- ** DetachInternetGateway
    detachInternetGateway_dryRun,
    detachInternetGateway_internetGatewayId,
    detachInternetGateway_vpcId,

    -- ** DetachNetworkInterface
    detachNetworkInterface_dryRun,
    detachNetworkInterface_force,
    detachNetworkInterface_attachmentId,

    -- ** DetachVolume
    detachVolume_device,
    detachVolume_dryRun,
    detachVolume_instanceId,
    detachVolume_force,
    detachVolume_volumeId,
    volumeAttachment_deleteOnTermination,
    volumeAttachment_device,
    volumeAttachment_state,
    volumeAttachment_instanceId,
    volumeAttachment_attachTime,
    volumeAttachment_volumeId,

    -- ** DetachVpnGateway
    detachVpnGateway_dryRun,
    detachVpnGateway_vpcId,
    detachVpnGateway_vpnGatewayId,

    -- ** DisableAddressTransfer
    disableAddressTransfer_dryRun,
    disableAddressTransfer_allocationId,
    disableAddressTransferResponse_addressTransfer,
    disableAddressTransferResponse_httpStatus,

    -- ** DisableEbsEncryptionByDefault
    disableEbsEncryptionByDefault_dryRun,
    disableEbsEncryptionByDefaultResponse_ebsEncryptionByDefault,
    disableEbsEncryptionByDefaultResponse_httpStatus,

    -- ** DisableFastLaunch
    disableFastLaunch_dryRun,
    disableFastLaunch_force,
    disableFastLaunch_imageId,
    disableFastLaunchResponse_resourceType,
    disableFastLaunchResponse_ownerId,
    disableFastLaunchResponse_launchTemplate,
    disableFastLaunchResponse_stateTransitionTime,
    disableFastLaunchResponse_state,
    disableFastLaunchResponse_stateTransitionReason,
    disableFastLaunchResponse_snapshotConfiguration,
    disableFastLaunchResponse_maxParallelLaunches,
    disableFastLaunchResponse_imageId,
    disableFastLaunchResponse_httpStatus,

    -- ** DisableFastSnapshotRestores
    disableFastSnapshotRestores_dryRun,
    disableFastSnapshotRestores_availabilityZones,
    disableFastSnapshotRestores_sourceSnapshotIds,
    disableFastSnapshotRestoresResponse_unsuccessful,
    disableFastSnapshotRestoresResponse_successful,
    disableFastSnapshotRestoresResponse_httpStatus,

    -- ** DisableImageDeprecation
    disableImageDeprecation_dryRun,
    disableImageDeprecation_imageId,
    disableImageDeprecationResponse_return,
    disableImageDeprecationResponse_httpStatus,

    -- ** DisableIpamOrganizationAdminAccount
    disableIpamOrganizationAdminAccount_dryRun,
    disableIpamOrganizationAdminAccount_delegatedAdminAccountId,
    disableIpamOrganizationAdminAccountResponse_success,
    disableIpamOrganizationAdminAccountResponse_httpStatus,

    -- ** DisableSerialConsoleAccess
    disableSerialConsoleAccess_dryRun,
    disableSerialConsoleAccessResponse_serialConsoleAccessEnabled,
    disableSerialConsoleAccessResponse_httpStatus,

    -- ** DisableTransitGatewayRouteTablePropagation
    disableTransitGatewayRouteTablePropagation_transitGatewayAttachmentId,
    disableTransitGatewayRouteTablePropagation_dryRun,
    disableTransitGatewayRouteTablePropagation_transitGatewayRouteTableAnnouncementId,
    disableTransitGatewayRouteTablePropagation_transitGatewayRouteTableId,
    disableTransitGatewayRouteTablePropagationResponse_propagation,
    disableTransitGatewayRouteTablePropagationResponse_httpStatus,

    -- ** DisableVgwRoutePropagation
    disableVgwRoutePropagation_dryRun,
    disableVgwRoutePropagation_gatewayId,
    disableVgwRoutePropagation_routeTableId,

    -- ** DisableVpcClassicLink
    disableVpcClassicLink_dryRun,
    disableVpcClassicLink_vpcId,
    disableVpcClassicLinkResponse_return,
    disableVpcClassicLinkResponse_httpStatus,

    -- ** DisableVpcClassicLinkDnsSupport
    disableVpcClassicLinkDnsSupport_vpcId,
    disableVpcClassicLinkDnsSupportResponse_return,
    disableVpcClassicLinkDnsSupportResponse_httpStatus,

    -- ** DisassociateAddress
    disassociateAddress_publicIp,
    disassociateAddress_dryRun,
    disassociateAddress_associationId,

    -- ** DisassociateClientVpnTargetNetwork
    disassociateClientVpnTargetNetwork_dryRun,
    disassociateClientVpnTargetNetwork_clientVpnEndpointId,
    disassociateClientVpnTargetNetwork_associationId,
    disassociateClientVpnTargetNetworkResponse_status,
    disassociateClientVpnTargetNetworkResponse_associationId,
    disassociateClientVpnTargetNetworkResponse_httpStatus,

    -- ** DisassociateEnclaveCertificateIamRole
    disassociateEnclaveCertificateIamRole_roleArn,
    disassociateEnclaveCertificateIamRole_dryRun,
    disassociateEnclaveCertificateIamRole_certificateArn,
    disassociateEnclaveCertificateIamRoleResponse_return,
    disassociateEnclaveCertificateIamRoleResponse_httpStatus,

    -- ** DisassociateIamInstanceProfile
    disassociateIamInstanceProfile_associationId,
    disassociateIamInstanceProfileResponse_iamInstanceProfileAssociation,
    disassociateIamInstanceProfileResponse_httpStatus,

    -- ** DisassociateInstanceEventWindow
    disassociateInstanceEventWindow_dryRun,
    disassociateInstanceEventWindow_instanceEventWindowId,
    disassociateInstanceEventWindow_associationTarget,
    disassociateInstanceEventWindowResponse_instanceEventWindow,
    disassociateInstanceEventWindowResponse_httpStatus,

    -- ** DisassociateRouteTable
    disassociateRouteTable_dryRun,
    disassociateRouteTable_associationId,

    -- ** DisassociateSubnetCidrBlock
    disassociateSubnetCidrBlock_associationId,
    disassociateSubnetCidrBlockResponse_subnetId,
    disassociateSubnetCidrBlockResponse_ipv6CidrBlockAssociation,
    disassociateSubnetCidrBlockResponse_httpStatus,

    -- ** DisassociateTransitGatewayMulticastDomain
    disassociateTransitGatewayMulticastDomain_transitGatewayAttachmentId,
    disassociateTransitGatewayMulticastDomain_dryRun,
    disassociateTransitGatewayMulticastDomain_transitGatewayMulticastDomainId,
    disassociateTransitGatewayMulticastDomain_subnetIds,
    disassociateTransitGatewayMulticastDomainResponse_associations,
    disassociateTransitGatewayMulticastDomainResponse_httpStatus,

    -- ** DisassociateTransitGatewayPolicyTable
    disassociateTransitGatewayPolicyTable_dryRun,
    disassociateTransitGatewayPolicyTable_transitGatewayPolicyTableId,
    disassociateTransitGatewayPolicyTable_transitGatewayAttachmentId,
    disassociateTransitGatewayPolicyTableResponse_association,
    disassociateTransitGatewayPolicyTableResponse_httpStatus,

    -- ** DisassociateTransitGatewayRouteTable
    disassociateTransitGatewayRouteTable_dryRun,
    disassociateTransitGatewayRouteTable_transitGatewayRouteTableId,
    disassociateTransitGatewayRouteTable_transitGatewayAttachmentId,
    disassociateTransitGatewayRouteTableResponse_association,
    disassociateTransitGatewayRouteTableResponse_httpStatus,

    -- ** DisassociateTrunkInterface
    disassociateTrunkInterface_clientToken,
    disassociateTrunkInterface_dryRun,
    disassociateTrunkInterface_associationId,
    disassociateTrunkInterfaceResponse_clientToken,
    disassociateTrunkInterfaceResponse_return,
    disassociateTrunkInterfaceResponse_httpStatus,

    -- ** DisassociateVpcCidrBlock
    disassociateVpcCidrBlock_associationId,
    disassociateVpcCidrBlockResponse_cidrBlockAssociation,
    disassociateVpcCidrBlockResponse_ipv6CidrBlockAssociation,
    disassociateVpcCidrBlockResponse_vpcId,
    disassociateVpcCidrBlockResponse_httpStatus,

    -- ** EnableAddressTransfer
    enableAddressTransfer_dryRun,
    enableAddressTransfer_allocationId,
    enableAddressTransfer_transferAccountId,
    enableAddressTransferResponse_addressTransfer,
    enableAddressTransferResponse_httpStatus,

    -- ** EnableEbsEncryptionByDefault
    enableEbsEncryptionByDefault_dryRun,
    enableEbsEncryptionByDefaultResponse_ebsEncryptionByDefault,
    enableEbsEncryptionByDefaultResponse_httpStatus,

    -- ** EnableFastLaunch
    enableFastLaunch_resourceType,
    enableFastLaunch_launchTemplate,
    enableFastLaunch_snapshotConfiguration,
    enableFastLaunch_dryRun,
    enableFastLaunch_maxParallelLaunches,
    enableFastLaunch_imageId,
    enableFastLaunchResponse_resourceType,
    enableFastLaunchResponse_ownerId,
    enableFastLaunchResponse_launchTemplate,
    enableFastLaunchResponse_stateTransitionTime,
    enableFastLaunchResponse_state,
    enableFastLaunchResponse_stateTransitionReason,
    enableFastLaunchResponse_snapshotConfiguration,
    enableFastLaunchResponse_maxParallelLaunches,
    enableFastLaunchResponse_imageId,
    enableFastLaunchResponse_httpStatus,

    -- ** EnableFastSnapshotRestores
    enableFastSnapshotRestores_dryRun,
    enableFastSnapshotRestores_availabilityZones,
    enableFastSnapshotRestores_sourceSnapshotIds,
    enableFastSnapshotRestoresResponse_unsuccessful,
    enableFastSnapshotRestoresResponse_successful,
    enableFastSnapshotRestoresResponse_httpStatus,

    -- ** EnableImageDeprecation
    enableImageDeprecation_dryRun,
    enableImageDeprecation_imageId,
    enableImageDeprecation_deprecateAt,
    enableImageDeprecationResponse_return,
    enableImageDeprecationResponse_httpStatus,

    -- ** EnableIpamOrganizationAdminAccount
    enableIpamOrganizationAdminAccount_dryRun,
    enableIpamOrganizationAdminAccount_delegatedAdminAccountId,
    enableIpamOrganizationAdminAccountResponse_success,
    enableIpamOrganizationAdminAccountResponse_httpStatus,

    -- ** EnableSerialConsoleAccess
    enableSerialConsoleAccess_dryRun,
    enableSerialConsoleAccessResponse_serialConsoleAccessEnabled,
    enableSerialConsoleAccessResponse_httpStatus,

    -- ** EnableTransitGatewayRouteTablePropagation
    enableTransitGatewayRouteTablePropagation_transitGatewayAttachmentId,
    enableTransitGatewayRouteTablePropagation_dryRun,
    enableTransitGatewayRouteTablePropagation_transitGatewayRouteTableAnnouncementId,
    enableTransitGatewayRouteTablePropagation_transitGatewayRouteTableId,
    enableTransitGatewayRouteTablePropagationResponse_propagation,
    enableTransitGatewayRouteTablePropagationResponse_httpStatus,

    -- ** EnableVgwRoutePropagation
    enableVgwRoutePropagation_dryRun,
    enableVgwRoutePropagation_gatewayId,
    enableVgwRoutePropagation_routeTableId,

    -- ** EnableVolumeIO
    enableVolumeIO_dryRun,
    enableVolumeIO_volumeId,

    -- ** EnableVpcClassicLink
    enableVpcClassicLink_dryRun,
    enableVpcClassicLink_vpcId,
    enableVpcClassicLinkResponse_return,
    enableVpcClassicLinkResponse_httpStatus,

    -- ** EnableVpcClassicLinkDnsSupport
    enableVpcClassicLinkDnsSupport_vpcId,
    enableVpcClassicLinkDnsSupportResponse_return,
    enableVpcClassicLinkDnsSupportResponse_httpStatus,

    -- ** ExportClientVpnClientCertificateRevocationList
    exportClientVpnClientCertificateRevocationList_dryRun,
    exportClientVpnClientCertificateRevocationList_clientVpnEndpointId,
    exportClientVpnClientCertificateRevocationListResponse_certificateRevocationList,
    exportClientVpnClientCertificateRevocationListResponse_status,
    exportClientVpnClientCertificateRevocationListResponse_httpStatus,

    -- ** ExportClientVpnClientConfiguration
    exportClientVpnClientConfiguration_dryRun,
    exportClientVpnClientConfiguration_clientVpnEndpointId,
    exportClientVpnClientConfigurationResponse_clientConfiguration,
    exportClientVpnClientConfigurationResponse_httpStatus,

    -- ** ExportImage
    exportImage_roleName,
    exportImage_clientToken,
    exportImage_description,
    exportImage_dryRun,
    exportImage_tagSpecifications,
    exportImage_diskImageFormat,
    exportImage_imageId,
    exportImage_s3ExportLocation,
    exportImageResponse_tags,
    exportImageResponse_progress,
    exportImageResponse_roleName,
    exportImageResponse_exportImageTaskId,
    exportImageResponse_s3ExportLocation,
    exportImageResponse_diskImageFormat,
    exportImageResponse_status,
    exportImageResponse_description,
    exportImageResponse_statusMessage,
    exportImageResponse_imageId,
    exportImageResponse_httpStatus,

    -- ** ExportTransitGatewayRoutes
    exportTransitGatewayRoutes_filters,
    exportTransitGatewayRoutes_dryRun,
    exportTransitGatewayRoutes_transitGatewayRouteTableId,
    exportTransitGatewayRoutes_s3Bucket,
    exportTransitGatewayRoutesResponse_s3Location,
    exportTransitGatewayRoutesResponse_httpStatus,

    -- ** GetAssociatedEnclaveCertificateIamRoles
    getAssociatedEnclaveCertificateIamRoles_dryRun,
    getAssociatedEnclaveCertificateIamRoles_certificateArn,
    getAssociatedEnclaveCertificateIamRolesResponse_associatedRoles,
    getAssociatedEnclaveCertificateIamRolesResponse_httpStatus,

    -- ** GetAssociatedIpv6PoolCidrs
    getAssociatedIpv6PoolCidrs_nextToken,
    getAssociatedIpv6PoolCidrs_dryRun,
    getAssociatedIpv6PoolCidrs_maxResults,
    getAssociatedIpv6PoolCidrs_poolId,
    getAssociatedIpv6PoolCidrsResponse_nextToken,
    getAssociatedIpv6PoolCidrsResponse_ipv6CidrAssociations,
    getAssociatedIpv6PoolCidrsResponse_httpStatus,

    -- ** GetCapacityReservationUsage
    getCapacityReservationUsage_nextToken,
    getCapacityReservationUsage_dryRun,
    getCapacityReservationUsage_maxResults,
    getCapacityReservationUsage_capacityReservationId,
    getCapacityReservationUsageResponse_nextToken,
    getCapacityReservationUsageResponse_totalInstanceCount,
    getCapacityReservationUsageResponse_state,
    getCapacityReservationUsageResponse_capacityReservationId,
    getCapacityReservationUsageResponse_instanceType,
    getCapacityReservationUsageResponse_instanceUsages,
    getCapacityReservationUsageResponse_availableInstanceCount,
    getCapacityReservationUsageResponse_httpStatus,

    -- ** GetCoipPoolUsage
    getCoipPoolUsage_nextToken,
    getCoipPoolUsage_filters,
    getCoipPoolUsage_dryRun,
    getCoipPoolUsage_maxResults,
    getCoipPoolUsage_poolId,
    getCoipPoolUsageResponse_coipAddressUsages,
    getCoipPoolUsageResponse_localGatewayRouteTableId,
    getCoipPoolUsageResponse_coipPoolId,
    getCoipPoolUsageResponse_httpStatus,

    -- ** GetConsoleOutput
    getConsoleOutput_dryRun,
    getConsoleOutput_latest,
    getConsoleOutput_instanceId,
    getConsoleOutputResponse_timestamp,
    getConsoleOutputResponse_instanceId,
    getConsoleOutputResponse_output,
    getConsoleOutputResponse_httpStatus,

    -- ** GetConsoleScreenshot
    getConsoleScreenshot_wakeUp,
    getConsoleScreenshot_dryRun,
    getConsoleScreenshot_instanceId,
    getConsoleScreenshotResponse_imageData,
    getConsoleScreenshotResponse_instanceId,
    getConsoleScreenshotResponse_httpStatus,

    -- ** GetDefaultCreditSpecification
    getDefaultCreditSpecification_dryRun,
    getDefaultCreditSpecification_instanceFamily,
    getDefaultCreditSpecificationResponse_instanceFamilyCreditSpecification,
    getDefaultCreditSpecificationResponse_httpStatus,

    -- ** GetEbsDefaultKmsKeyId
    getEbsDefaultKmsKeyId_dryRun,
    getEbsDefaultKmsKeyIdResponse_kmsKeyId,
    getEbsDefaultKmsKeyIdResponse_httpStatus,

    -- ** GetEbsEncryptionByDefault
    getEbsEncryptionByDefault_dryRun,
    getEbsEncryptionByDefaultResponse_ebsEncryptionByDefault,
    getEbsEncryptionByDefaultResponse_httpStatus,

    -- ** GetFlowLogsIntegrationTemplate
    getFlowLogsIntegrationTemplate_dryRun,
    getFlowLogsIntegrationTemplate_flowLogId,
    getFlowLogsIntegrationTemplate_configDeliveryS3DestinationArn,
    getFlowLogsIntegrationTemplate_integrateServices,
    getFlowLogsIntegrationTemplateResponse_result,
    getFlowLogsIntegrationTemplateResponse_httpStatus,

    -- ** GetGroupsForCapacityReservation
    getGroupsForCapacityReservation_nextToken,
    getGroupsForCapacityReservation_dryRun,
    getGroupsForCapacityReservation_maxResults,
    getGroupsForCapacityReservation_capacityReservationId,
    getGroupsForCapacityReservationResponse_nextToken,
    getGroupsForCapacityReservationResponse_capacityReservationGroups,
    getGroupsForCapacityReservationResponse_httpStatus,

    -- ** GetHostReservationPurchasePreview
    getHostReservationPurchasePreview_hostIdSet,
    getHostReservationPurchasePreview_offeringId,
    getHostReservationPurchasePreviewResponse_totalHourlyPrice,
    getHostReservationPurchasePreviewResponse_totalUpfrontPrice,
    getHostReservationPurchasePreviewResponse_currencyCode,
    getHostReservationPurchasePreviewResponse_purchase,
    getHostReservationPurchasePreviewResponse_httpStatus,

    -- ** GetInstanceTypesFromInstanceRequirements
    getInstanceTypesFromInstanceRequirements_nextToken,
    getInstanceTypesFromInstanceRequirements_dryRun,
    getInstanceTypesFromInstanceRequirements_maxResults,
    getInstanceTypesFromInstanceRequirements_architectureTypes,
    getInstanceTypesFromInstanceRequirements_virtualizationTypes,
    getInstanceTypesFromInstanceRequirements_instanceRequirements,
    getInstanceTypesFromInstanceRequirementsResponse_nextToken,
    getInstanceTypesFromInstanceRequirementsResponse_instanceTypes,
    getInstanceTypesFromInstanceRequirementsResponse_httpStatus,

    -- ** GetInstanceUefiData
    getInstanceUefiData_dryRun,
    getInstanceUefiData_instanceId,
    getInstanceUefiDataResponse_instanceId,
    getInstanceUefiDataResponse_uefiData,
    getInstanceUefiDataResponse_httpStatus,

    -- ** GetIpamAddressHistory
    getIpamAddressHistory_nextToken,
    getIpamAddressHistory_endTime,
    getIpamAddressHistory_dryRun,
    getIpamAddressHistory_maxResults,
    getIpamAddressHistory_vpcId,
    getIpamAddressHistory_startTime,
    getIpamAddressHistory_cidr,
    getIpamAddressHistory_ipamScopeId,
    getIpamAddressHistoryResponse_nextToken,
    getIpamAddressHistoryResponse_historyRecords,
    getIpamAddressHistoryResponse_httpStatus,

    -- ** GetIpamPoolAllocations
    getIpamPoolAllocations_nextToken,
    getIpamPoolAllocations_filters,
    getIpamPoolAllocations_dryRun,
    getIpamPoolAllocations_maxResults,
    getIpamPoolAllocations_ipamPoolAllocationId,
    getIpamPoolAllocations_ipamPoolId,
    getIpamPoolAllocationsResponse_nextToken,
    getIpamPoolAllocationsResponse_ipamPoolAllocations,
    getIpamPoolAllocationsResponse_httpStatus,

    -- ** GetIpamPoolCidrs
    getIpamPoolCidrs_nextToken,
    getIpamPoolCidrs_filters,
    getIpamPoolCidrs_dryRun,
    getIpamPoolCidrs_maxResults,
    getIpamPoolCidrs_ipamPoolId,
    getIpamPoolCidrsResponse_nextToken,
    getIpamPoolCidrsResponse_ipamPoolCidrs,
    getIpamPoolCidrsResponse_httpStatus,

    -- ** GetIpamResourceCidrs
    getIpamResourceCidrs_resourceId,
    getIpamResourceCidrs_resourceType,
    getIpamResourceCidrs_nextToken,
    getIpamResourceCidrs_filters,
    getIpamResourceCidrs_resourceOwner,
    getIpamResourceCidrs_dryRun,
    getIpamResourceCidrs_maxResults,
    getIpamResourceCidrs_ipamPoolId,
    getIpamResourceCidrs_resourceTag,
    getIpamResourceCidrs_ipamScopeId,
    getIpamResourceCidrsResponse_nextToken,
    getIpamResourceCidrsResponse_ipamResourceCidrs,
    getIpamResourceCidrsResponse_httpStatus,

    -- ** GetLaunchTemplateData
    getLaunchTemplateData_dryRun,
    getLaunchTemplateData_instanceId,
    getLaunchTemplateDataResponse_launchTemplateData,
    getLaunchTemplateDataResponse_httpStatus,

    -- ** GetManagedPrefixListAssociations
    getManagedPrefixListAssociations_nextToken,
    getManagedPrefixListAssociations_dryRun,
    getManagedPrefixListAssociations_maxResults,
    getManagedPrefixListAssociations_prefixListId,
    getManagedPrefixListAssociationsResponse_nextToken,
    getManagedPrefixListAssociationsResponse_prefixListAssociations,
    getManagedPrefixListAssociationsResponse_httpStatus,

    -- ** GetManagedPrefixListEntries
    getManagedPrefixListEntries_nextToken,
    getManagedPrefixListEntries_targetVersion,
    getManagedPrefixListEntries_dryRun,
    getManagedPrefixListEntries_maxResults,
    getManagedPrefixListEntries_prefixListId,
    getManagedPrefixListEntriesResponse_nextToken,
    getManagedPrefixListEntriesResponse_entries,
    getManagedPrefixListEntriesResponse_httpStatus,

    -- ** GetNetworkInsightsAccessScopeAnalysisFindings
    getNetworkInsightsAccessScopeAnalysisFindings_nextToken,
    getNetworkInsightsAccessScopeAnalysisFindings_dryRun,
    getNetworkInsightsAccessScopeAnalysisFindings_maxResults,
    getNetworkInsightsAccessScopeAnalysisFindings_networkInsightsAccessScopeAnalysisId,
    getNetworkInsightsAccessScopeAnalysisFindingsResponse_networkInsightsAccessScopeAnalysisId,
    getNetworkInsightsAccessScopeAnalysisFindingsResponse_nextToken,
    getNetworkInsightsAccessScopeAnalysisFindingsResponse_analysisFindings,
    getNetworkInsightsAccessScopeAnalysisFindingsResponse_analysisStatus,
    getNetworkInsightsAccessScopeAnalysisFindingsResponse_httpStatus,

    -- ** GetNetworkInsightsAccessScopeContent
    getNetworkInsightsAccessScopeContent_dryRun,
    getNetworkInsightsAccessScopeContent_networkInsightsAccessScopeId,
    getNetworkInsightsAccessScopeContentResponse_networkInsightsAccessScopeContent,
    getNetworkInsightsAccessScopeContentResponse_httpStatus,

    -- ** GetPasswordData
    getPasswordData_dryRun,
    getPasswordData_instanceId,
    getPasswordDataResponse_httpStatus,
    getPasswordDataResponse_instanceId,
    getPasswordDataResponse_passwordData,
    getPasswordDataResponse_timestamp,

    -- ** GetReservedInstancesExchangeQuote
    getReservedInstancesExchangeQuote_dryRun,
    getReservedInstancesExchangeQuote_targetConfigurations,
    getReservedInstancesExchangeQuote_reservedInstanceIds,
    getReservedInstancesExchangeQuoteResponse_targetConfigurationValueSet,
    getReservedInstancesExchangeQuoteResponse_reservedInstanceValueRollup,
    getReservedInstancesExchangeQuoteResponse_validationFailureReason,
    getReservedInstancesExchangeQuoteResponse_outputReservedInstancesWillExpireAt,
    getReservedInstancesExchangeQuoteResponse_currencyCode,
    getReservedInstancesExchangeQuoteResponse_paymentDue,
    getReservedInstancesExchangeQuoteResponse_targetConfigurationValueRollup,
    getReservedInstancesExchangeQuoteResponse_reservedInstanceValueSet,
    getReservedInstancesExchangeQuoteResponse_isValidExchange,
    getReservedInstancesExchangeQuoteResponse_httpStatus,

    -- ** GetSerialConsoleAccessStatus
    getSerialConsoleAccessStatus_dryRun,
    getSerialConsoleAccessStatusResponse_serialConsoleAccessEnabled,
    getSerialConsoleAccessStatusResponse_httpStatus,

    -- ** GetSpotPlacementScores
    getSpotPlacementScores_regionNames,
    getSpotPlacementScores_nextToken,
    getSpotPlacementScores_instanceTypes,
    getSpotPlacementScores_singleAvailabilityZone,
    getSpotPlacementScores_dryRun,
    getSpotPlacementScores_targetCapacityUnitType,
    getSpotPlacementScores_maxResults,
    getSpotPlacementScores_instanceRequirementsWithMetadata,
    getSpotPlacementScores_targetCapacity,
    getSpotPlacementScoresResponse_nextToken,
    getSpotPlacementScoresResponse_spotPlacementScores,
    getSpotPlacementScoresResponse_httpStatus,

    -- ** GetSubnetCidrReservations
    getSubnetCidrReservations_nextToken,
    getSubnetCidrReservations_filters,
    getSubnetCidrReservations_dryRun,
    getSubnetCidrReservations_maxResults,
    getSubnetCidrReservations_subnetId,
    getSubnetCidrReservationsResponse_nextToken,
    getSubnetCidrReservationsResponse_subnetIpv4CidrReservations,
    getSubnetCidrReservationsResponse_subnetIpv6CidrReservations,
    getSubnetCidrReservationsResponse_httpStatus,

    -- ** GetTransitGatewayAttachmentPropagations
    getTransitGatewayAttachmentPropagations_nextToken,
    getTransitGatewayAttachmentPropagations_filters,
    getTransitGatewayAttachmentPropagations_dryRun,
    getTransitGatewayAttachmentPropagations_maxResults,
    getTransitGatewayAttachmentPropagations_transitGatewayAttachmentId,
    getTransitGatewayAttachmentPropagationsResponse_nextToken,
    getTransitGatewayAttachmentPropagationsResponse_transitGatewayAttachmentPropagations,
    getTransitGatewayAttachmentPropagationsResponse_httpStatus,

    -- ** GetTransitGatewayMulticastDomainAssociations
    getTransitGatewayMulticastDomainAssociations_nextToken,
    getTransitGatewayMulticastDomainAssociations_filters,
    getTransitGatewayMulticastDomainAssociations_dryRun,
    getTransitGatewayMulticastDomainAssociations_maxResults,
    getTransitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId,
    getTransitGatewayMulticastDomainAssociationsResponse_nextToken,
    getTransitGatewayMulticastDomainAssociationsResponse_multicastDomainAssociations,
    getTransitGatewayMulticastDomainAssociationsResponse_httpStatus,

    -- ** GetTransitGatewayPolicyTableAssociations
    getTransitGatewayPolicyTableAssociations_nextToken,
    getTransitGatewayPolicyTableAssociations_filters,
    getTransitGatewayPolicyTableAssociations_dryRun,
    getTransitGatewayPolicyTableAssociations_maxResults,
    getTransitGatewayPolicyTableAssociations_transitGatewayPolicyTableId,
    getTransitGatewayPolicyTableAssociationsResponse_nextToken,
    getTransitGatewayPolicyTableAssociationsResponse_associations,
    getTransitGatewayPolicyTableAssociationsResponse_httpStatus,

    -- ** GetTransitGatewayPolicyTableEntries
    getTransitGatewayPolicyTableEntries_nextToken,
    getTransitGatewayPolicyTableEntries_filters,
    getTransitGatewayPolicyTableEntries_dryRun,
    getTransitGatewayPolicyTableEntries_maxResults,
    getTransitGatewayPolicyTableEntries_transitGatewayPolicyTableId,
    getTransitGatewayPolicyTableEntriesResponse_transitGatewayPolicyTableEntries,
    getTransitGatewayPolicyTableEntriesResponse_httpStatus,

    -- ** GetTransitGatewayPrefixListReferences
    getTransitGatewayPrefixListReferences_nextToken,
    getTransitGatewayPrefixListReferences_filters,
    getTransitGatewayPrefixListReferences_dryRun,
    getTransitGatewayPrefixListReferences_maxResults,
    getTransitGatewayPrefixListReferences_transitGatewayRouteTableId,
    getTransitGatewayPrefixListReferencesResponse_nextToken,
    getTransitGatewayPrefixListReferencesResponse_transitGatewayPrefixListReferences,
    getTransitGatewayPrefixListReferencesResponse_httpStatus,

    -- ** GetTransitGatewayRouteTableAssociations
    getTransitGatewayRouteTableAssociations_nextToken,
    getTransitGatewayRouteTableAssociations_filters,
    getTransitGatewayRouteTableAssociations_dryRun,
    getTransitGatewayRouteTableAssociations_maxResults,
    getTransitGatewayRouteTableAssociations_transitGatewayRouteTableId,
    getTransitGatewayRouteTableAssociationsResponse_nextToken,
    getTransitGatewayRouteTableAssociationsResponse_associations,
    getTransitGatewayRouteTableAssociationsResponse_httpStatus,

    -- ** GetTransitGatewayRouteTablePropagations
    getTransitGatewayRouteTablePropagations_nextToken,
    getTransitGatewayRouteTablePropagations_filters,
    getTransitGatewayRouteTablePropagations_dryRun,
    getTransitGatewayRouteTablePropagations_maxResults,
    getTransitGatewayRouteTablePropagations_transitGatewayRouteTableId,
    getTransitGatewayRouteTablePropagationsResponse_nextToken,
    getTransitGatewayRouteTablePropagationsResponse_transitGatewayRouteTablePropagations,
    getTransitGatewayRouteTablePropagationsResponse_httpStatus,

    -- ** GetVpnConnectionDeviceSampleConfiguration
    getVpnConnectionDeviceSampleConfiguration_internetKeyExchangeVersion,
    getVpnConnectionDeviceSampleConfiguration_dryRun,
    getVpnConnectionDeviceSampleConfiguration_vpnConnectionId,
    getVpnConnectionDeviceSampleConfiguration_vpnConnectionDeviceTypeId,
    getVpnConnectionDeviceSampleConfigurationResponse_vpnConnectionDeviceSampleConfiguration,
    getVpnConnectionDeviceSampleConfigurationResponse_httpStatus,

    -- ** GetVpnConnectionDeviceTypes
    getVpnConnectionDeviceTypes_nextToken,
    getVpnConnectionDeviceTypes_dryRun,
    getVpnConnectionDeviceTypes_maxResults,
    getVpnConnectionDeviceTypesResponse_vpnConnectionDeviceTypes,
    getVpnConnectionDeviceTypesResponse_nextToken,
    getVpnConnectionDeviceTypesResponse_httpStatus,

    -- ** ImportClientVpnClientCertificateRevocationList
    importClientVpnClientCertificateRevocationList_dryRun,
    importClientVpnClientCertificateRevocationList_clientVpnEndpointId,
    importClientVpnClientCertificateRevocationList_certificateRevocationList,
    importClientVpnClientCertificateRevocationListResponse_return,
    importClientVpnClientCertificateRevocationListResponse_httpStatus,

    -- ** ImportImage
    importImage_hypervisor,
    importImage_roleName,
    importImage_clientToken,
    importImage_licenseType,
    importImage_diskContainers,
    importImage_platform,
    importImage_description,
    importImage_dryRun,
    importImage_usageOperation,
    importImage_encrypted,
    importImage_kmsKeyId,
    importImage_bootMode,
    importImage_licenseSpecifications,
    importImage_tagSpecifications,
    importImage_clientData,
    importImage_architecture,
    importImageResponse_tags,
    importImageResponse_hypervisor,
    importImageResponse_progress,
    importImageResponse_licenseType,
    importImageResponse_importTaskId,
    importImageResponse_status,
    importImageResponse_platform,
    importImageResponse_description,
    importImageResponse_usageOperation,
    importImageResponse_encrypted,
    importImageResponse_kmsKeyId,
    importImageResponse_licenseSpecifications,
    importImageResponse_snapshotDetails,
    importImageResponse_statusMessage,
    importImageResponse_architecture,
    importImageResponse_imageId,
    importImageResponse_httpStatus,

    -- ** ImportInstance
    importInstance_diskImages,
    importInstance_description,
    importInstance_dryRun,
    importInstance_launchSpecification,
    importInstance_platform,
    importInstanceResponse_conversionTask,
    importInstanceResponse_httpStatus,

    -- ** ImportKeyPair
    importKeyPair_dryRun,
    importKeyPair_tagSpecifications,
    importKeyPair_keyName,
    importKeyPair_publicKeyMaterial,
    importKeyPairResponse_tags,
    importKeyPairResponse_keyFingerprint,
    importKeyPairResponse_keyName,
    importKeyPairResponse_keyPairId,
    importKeyPairResponse_httpStatus,

    -- ** ImportSnapshot
    importSnapshot_roleName,
    importSnapshot_clientToken,
    importSnapshot_diskContainer,
    importSnapshot_description,
    importSnapshot_dryRun,
    importSnapshot_encrypted,
    importSnapshot_kmsKeyId,
    importSnapshot_tagSpecifications,
    importSnapshot_clientData,
    importSnapshotResponse_tags,
    importSnapshotResponse_importTaskId,
    importSnapshotResponse_description,
    importSnapshotResponse_snapshotTaskDetail,
    importSnapshotResponse_httpStatus,

    -- ** ImportVolume
    importVolume_description,
    importVolume_dryRun,
    importVolume_availabilityZone,
    importVolume_image,
    importVolume_volume,
    importVolumeResponse_conversionTask,
    importVolumeResponse_httpStatus,

    -- ** ListImagesInRecycleBin
    listImagesInRecycleBin_nextToken,
    listImagesInRecycleBin_imageIds,
    listImagesInRecycleBin_dryRun,
    listImagesInRecycleBin_maxResults,
    listImagesInRecycleBinResponse_nextToken,
    listImagesInRecycleBinResponse_images,
    listImagesInRecycleBinResponse_httpStatus,

    -- ** ListSnapshotsInRecycleBin
    listSnapshotsInRecycleBin_nextToken,
    listSnapshotsInRecycleBin_snapshotIds,
    listSnapshotsInRecycleBin_dryRun,
    listSnapshotsInRecycleBin_maxResults,
    listSnapshotsInRecycleBinResponse_nextToken,
    listSnapshotsInRecycleBinResponse_snapshots,
    listSnapshotsInRecycleBinResponse_httpStatus,

    -- ** ModifyAddressAttribute
    modifyAddressAttribute_domainName,
    modifyAddressAttribute_dryRun,
    modifyAddressAttribute_allocationId,
    modifyAddressAttributeResponse_address,
    modifyAddressAttributeResponse_httpStatus,

    -- ** ModifyAvailabilityZoneGroup
    modifyAvailabilityZoneGroup_dryRun,
    modifyAvailabilityZoneGroup_groupName,
    modifyAvailabilityZoneGroup_optInStatus,
    modifyAvailabilityZoneGroupResponse_return,
    modifyAvailabilityZoneGroupResponse_httpStatus,

    -- ** ModifyCapacityReservation
    modifyCapacityReservation_additionalInfo,
    modifyCapacityReservation_endDate,
    modifyCapacityReservation_accept,
    modifyCapacityReservation_dryRun,
    modifyCapacityReservation_instanceCount,
    modifyCapacityReservation_endDateType,
    modifyCapacityReservation_capacityReservationId,
    modifyCapacityReservationResponse_return,
    modifyCapacityReservationResponse_httpStatus,

    -- ** ModifyCapacityReservationFleet
    modifyCapacityReservationFleet_endDate,
    modifyCapacityReservationFleet_dryRun,
    modifyCapacityReservationFleet_totalTargetCapacity,
    modifyCapacityReservationFleet_removeEndDate,
    modifyCapacityReservationFleet_capacityReservationFleetId,
    modifyCapacityReservationFleetResponse_return,
    modifyCapacityReservationFleetResponse_httpStatus,

    -- ** ModifyClientVpnEndpoint
    modifyClientVpnEndpoint_serverCertificateArn,
    modifyClientVpnEndpoint_sessionTimeoutHours,
    modifyClientVpnEndpoint_securityGroupIds,
    modifyClientVpnEndpoint_dnsServers,
    modifyClientVpnEndpoint_connectionLogOptions,
    modifyClientVpnEndpoint_description,
    modifyClientVpnEndpoint_dryRun,
    modifyClientVpnEndpoint_clientLoginBannerOptions,
    modifyClientVpnEndpoint_clientConnectOptions,
    modifyClientVpnEndpoint_splitTunnel,
    modifyClientVpnEndpoint_vpcId,
    modifyClientVpnEndpoint_vpnPort,
    modifyClientVpnEndpoint_selfServicePortal,
    modifyClientVpnEndpoint_clientVpnEndpointId,
    modifyClientVpnEndpointResponse_return,
    modifyClientVpnEndpointResponse_httpStatus,

    -- ** ModifyDefaultCreditSpecification
    modifyDefaultCreditSpecification_dryRun,
    modifyDefaultCreditSpecification_instanceFamily,
    modifyDefaultCreditSpecification_cpuCredits,
    modifyDefaultCreditSpecificationResponse_instanceFamilyCreditSpecification,
    modifyDefaultCreditSpecificationResponse_httpStatus,

    -- ** ModifyEbsDefaultKmsKeyId
    modifyEbsDefaultKmsKeyId_dryRun,
    modifyEbsDefaultKmsKeyId_kmsKeyId,
    modifyEbsDefaultKmsKeyIdResponse_kmsKeyId,
    modifyEbsDefaultKmsKeyIdResponse_httpStatus,

    -- ** ModifyFleet
    modifyFleet_excessCapacityTerminationPolicy,
    modifyFleet_targetCapacitySpecification,
    modifyFleet_context,
    modifyFleet_launchTemplateConfigs,
    modifyFleet_dryRun,
    modifyFleet_fleetId,
    modifyFleetResponse_return,
    modifyFleetResponse_httpStatus,

    -- ** ModifyFpgaImageAttribute
    modifyFpgaImageAttribute_name,
    modifyFpgaImageAttribute_operationType,
    modifyFpgaImageAttribute_attribute,
    modifyFpgaImageAttribute_productCodes,
    modifyFpgaImageAttribute_userGroups,
    modifyFpgaImageAttribute_description,
    modifyFpgaImageAttribute_dryRun,
    modifyFpgaImageAttribute_loadPermission,
    modifyFpgaImageAttribute_userIds,
    modifyFpgaImageAttribute_fpgaImageId,
    modifyFpgaImageAttributeResponse_fpgaImageAttribute,
    modifyFpgaImageAttributeResponse_httpStatus,

    -- ** ModifyHosts
    modifyHosts_autoPlacement,
    modifyHosts_hostRecovery,
    modifyHosts_instanceType,
    modifyHosts_instanceFamily,
    modifyHosts_hostIds,
    modifyHostsResponse_unsuccessful,
    modifyHostsResponse_successful,
    modifyHostsResponse_httpStatus,

    -- ** ModifyIdFormat
    modifyIdFormat_resource,
    modifyIdFormat_useLongIds,

    -- ** ModifyIdentityIdFormat
    modifyIdentityIdFormat_principalArn,
    modifyIdentityIdFormat_resource,
    modifyIdentityIdFormat_useLongIds,

    -- ** ModifyImageAttribute
    modifyImageAttribute_launchPermission,
    modifyImageAttribute_operationType,
    modifyImageAttribute_attribute,
    modifyImageAttribute_productCodes,
    modifyImageAttribute_userGroups,
    modifyImageAttribute_description,
    modifyImageAttribute_dryRun,
    modifyImageAttribute_organizationArns,
    modifyImageAttribute_userIds,
    modifyImageAttribute_organizationalUnitArns,
    modifyImageAttribute_value,
    modifyImageAttribute_imageId,

    -- ** ModifyInstanceAttribute
    modifyInstanceAttribute_ebsOptimized,
    modifyInstanceAttribute_sriovNetSupport,
    modifyInstanceAttribute_userData,
    modifyInstanceAttribute_blockDeviceMappings,
    modifyInstanceAttribute_sourceDestCheck,
    modifyInstanceAttribute_kernel,
    modifyInstanceAttribute_instanceInitiatedShutdownBehavior,
    modifyInstanceAttribute_attribute,
    modifyInstanceAttribute_dryRun,
    modifyInstanceAttribute_instanceType,
    modifyInstanceAttribute_ramdisk,
    modifyInstanceAttribute_disableApiTermination,
    modifyInstanceAttribute_groups,
    modifyInstanceAttribute_disableApiStop,
    modifyInstanceAttribute_enaSupport,
    modifyInstanceAttribute_value,
    modifyInstanceAttribute_instanceId,

    -- ** ModifyInstanceCapacityReservationAttributes
    modifyInstanceCapacityReservationAttributes_dryRun,
    modifyInstanceCapacityReservationAttributes_instanceId,
    modifyInstanceCapacityReservationAttributes_capacityReservationSpecification,
    modifyInstanceCapacityReservationAttributesResponse_return,
    modifyInstanceCapacityReservationAttributesResponse_httpStatus,

    -- ** ModifyInstanceCreditSpecification
    modifyInstanceCreditSpecification_clientToken,
    modifyInstanceCreditSpecification_dryRun,
    modifyInstanceCreditSpecification_instanceCreditSpecifications,
    modifyInstanceCreditSpecificationResponse_successfulInstanceCreditSpecifications,
    modifyInstanceCreditSpecificationResponse_unsuccessfulInstanceCreditSpecifications,
    modifyInstanceCreditSpecificationResponse_httpStatus,

    -- ** ModifyInstanceEventStartTime
    modifyInstanceEventStartTime_dryRun,
    modifyInstanceEventStartTime_instanceId,
    modifyInstanceEventStartTime_instanceEventId,
    modifyInstanceEventStartTime_notBefore,
    modifyInstanceEventStartTimeResponse_event,
    modifyInstanceEventStartTimeResponse_httpStatus,

    -- ** ModifyInstanceEventWindow
    modifyInstanceEventWindow_name,
    modifyInstanceEventWindow_dryRun,
    modifyInstanceEventWindow_timeRanges,
    modifyInstanceEventWindow_cronExpression,
    modifyInstanceEventWindow_instanceEventWindowId,
    modifyInstanceEventWindowResponse_instanceEventWindow,
    modifyInstanceEventWindowResponse_httpStatus,

    -- ** ModifyInstanceMaintenanceOptions
    modifyInstanceMaintenanceOptions_dryRun,
    modifyInstanceMaintenanceOptions_autoRecovery,
    modifyInstanceMaintenanceOptions_instanceId,
    modifyInstanceMaintenanceOptionsResponse_instanceId,
    modifyInstanceMaintenanceOptionsResponse_autoRecovery,
    modifyInstanceMaintenanceOptionsResponse_httpStatus,

    -- ** ModifyInstanceMetadataOptions
    modifyInstanceMetadataOptions_httpPutResponseHopLimit,
    modifyInstanceMetadataOptions_httpTokens,
    modifyInstanceMetadataOptions_httpEndpoint,
    modifyInstanceMetadataOptions_dryRun,
    modifyInstanceMetadataOptions_instanceMetadataTags,
    modifyInstanceMetadataOptions_httpProtocolIpv6,
    modifyInstanceMetadataOptions_instanceId,
    modifyInstanceMetadataOptionsResponse_instanceId,
    modifyInstanceMetadataOptionsResponse_instanceMetadataOptions,
    modifyInstanceMetadataOptionsResponse_httpStatus,

    -- ** ModifyInstancePlacement
    modifyInstancePlacement_partitionNumber,
    modifyInstancePlacement_hostResourceGroupArn,
    modifyInstancePlacement_hostId,
    modifyInstancePlacement_groupName,
    modifyInstancePlacement_affinity,
    modifyInstancePlacement_tenancy,
    modifyInstancePlacement_groupId,
    modifyInstancePlacement_instanceId,
    modifyInstancePlacementResponse_return,
    modifyInstancePlacementResponse_httpStatus,

    -- ** ModifyIpam
    modifyIpam_removeOperatingRegions,
    modifyIpam_description,
    modifyIpam_dryRun,
    modifyIpam_addOperatingRegions,
    modifyIpam_ipamId,
    modifyIpamResponse_ipam,
    modifyIpamResponse_httpStatus,

    -- ** ModifyIpamPool
    modifyIpamPool_addAllocationResourceTags,
    modifyIpamPool_clearAllocationDefaultNetmaskLength,
    modifyIpamPool_removeAllocationResourceTags,
    modifyIpamPool_allocationMaxNetmaskLength,
    modifyIpamPool_description,
    modifyIpamPool_dryRun,
    modifyIpamPool_allocationMinNetmaskLength,
    modifyIpamPool_allocationDefaultNetmaskLength,
    modifyIpamPool_autoImport,
    modifyIpamPool_ipamPoolId,
    modifyIpamPoolResponse_ipamPool,
    modifyIpamPoolResponse_httpStatus,

    -- ** ModifyIpamResourceCidr
    modifyIpamResourceCidr_destinationIpamScopeId,
    modifyIpamResourceCidr_dryRun,
    modifyIpamResourceCidr_resourceId,
    modifyIpamResourceCidr_resourceCidr,
    modifyIpamResourceCidr_resourceRegion,
    modifyIpamResourceCidr_currentIpamScopeId,
    modifyIpamResourceCidr_monitored,
    modifyIpamResourceCidrResponse_ipamResourceCidr,
    modifyIpamResourceCidrResponse_httpStatus,

    -- ** ModifyIpamScope
    modifyIpamScope_description,
    modifyIpamScope_dryRun,
    modifyIpamScope_ipamScopeId,
    modifyIpamScopeResponse_ipamScope,
    modifyIpamScopeResponse_httpStatus,

    -- ** ModifyLaunchTemplate
    modifyLaunchTemplate_clientToken,
    modifyLaunchTemplate_defaultVersion,
    modifyLaunchTemplate_dryRun,
    modifyLaunchTemplate_launchTemplateId,
    modifyLaunchTemplate_launchTemplateName,
    modifyLaunchTemplateResponse_launchTemplate,
    modifyLaunchTemplateResponse_httpStatus,

    -- ** ModifyLocalGatewayRoute
    modifyLocalGatewayRoute_dryRun,
    modifyLocalGatewayRoute_networkInterfaceId,
    modifyLocalGatewayRoute_localGatewayVirtualInterfaceGroupId,
    modifyLocalGatewayRoute_destinationCidrBlock,
    modifyLocalGatewayRoute_localGatewayRouteTableId,
    modifyLocalGatewayRouteResponse_route,
    modifyLocalGatewayRouteResponse_httpStatus,

    -- ** ModifyManagedPrefixList
    modifyManagedPrefixList_removeEntries,
    modifyManagedPrefixList_addEntries,
    modifyManagedPrefixList_dryRun,
    modifyManagedPrefixList_currentVersion,
    modifyManagedPrefixList_maxEntries,
    modifyManagedPrefixList_prefixListName,
    modifyManagedPrefixList_prefixListId,
    modifyManagedPrefixListResponse_prefixList,
    modifyManagedPrefixListResponse_httpStatus,

    -- ** ModifyNetworkInterfaceAttribute
    modifyNetworkInterfaceAttribute_attachment,
    modifyNetworkInterfaceAttribute_sourceDestCheck,
    modifyNetworkInterfaceAttribute_description,
    modifyNetworkInterfaceAttribute_dryRun,
    modifyNetworkInterfaceAttribute_groups,
    modifyNetworkInterfaceAttribute_networkInterfaceId,

    -- ** ModifyPrivateDnsNameOptions
    modifyPrivateDnsNameOptions_privateDnsHostnameType,
    modifyPrivateDnsNameOptions_enableResourceNameDnsARecord,
    modifyPrivateDnsNameOptions_dryRun,
    modifyPrivateDnsNameOptions_enableResourceNameDnsAAAARecord,
    modifyPrivateDnsNameOptions_instanceId,
    modifyPrivateDnsNameOptionsResponse_return,
    modifyPrivateDnsNameOptionsResponse_httpStatus,

    -- ** ModifyReservedInstances
    modifyReservedInstances_clientToken,
    modifyReservedInstances_reservedInstancesIds,
    modifyReservedInstances_targetConfigurations,
    modifyReservedInstancesResponse_reservedInstancesModificationId,
    modifyReservedInstancesResponse_httpStatus,

    -- ** ModifySecurityGroupRules
    modifySecurityGroupRules_dryRun,
    modifySecurityGroupRules_groupId,
    modifySecurityGroupRules_securityGroupRules,
    modifySecurityGroupRulesResponse_return,
    modifySecurityGroupRulesResponse_httpStatus,

    -- ** ModifySnapshotAttribute
    modifySnapshotAttribute_operationType,
    modifySnapshotAttribute_attribute,
    modifySnapshotAttribute_createVolumePermission,
    modifySnapshotAttribute_dryRun,
    modifySnapshotAttribute_groupNames,
    modifySnapshotAttribute_userIds,
    modifySnapshotAttribute_snapshotId,

    -- ** ModifySnapshotTier
    modifySnapshotTier_dryRun,
    modifySnapshotTier_storageTier,
    modifySnapshotTier_snapshotId,
    modifySnapshotTierResponse_snapshotId,
    modifySnapshotTierResponse_tieringStartTime,
    modifySnapshotTierResponse_httpStatus,

    -- ** ModifySpotFleetRequest
    modifySpotFleetRequest_excessCapacityTerminationPolicy,
    modifySpotFleetRequest_targetCapacity,
    modifySpotFleetRequest_context,
    modifySpotFleetRequest_launchTemplateConfigs,
    modifySpotFleetRequest_onDemandTargetCapacity,
    modifySpotFleetRequest_spotFleetRequestId,
    modifySpotFleetRequestResponse_return,
    modifySpotFleetRequestResponse_httpStatus,

    -- ** ModifySubnetAttribute
    modifySubnetAttribute_privateDnsHostnameTypeOnLaunch,
    modifySubnetAttribute_mapPublicIpOnLaunch,
    modifySubnetAttribute_enableLniAtDeviceIndex,
    modifySubnetAttribute_customerOwnedIpv4Pool,
    modifySubnetAttribute_mapCustomerOwnedIpOnLaunch,
    modifySubnetAttribute_disableLniAtDeviceIndex,
    modifySubnetAttribute_enableResourceNameDnsAAAARecordOnLaunch,
    modifySubnetAttribute_assignIpv6AddressOnCreation,
    modifySubnetAttribute_enableDns64,
    modifySubnetAttribute_enableResourceNameDnsARecordOnLaunch,
    modifySubnetAttribute_subnetId,

    -- ** ModifyTrafficMirrorFilterNetworkServices
    modifyTrafficMirrorFilterNetworkServices_removeNetworkServices,
    modifyTrafficMirrorFilterNetworkServices_dryRun,
    modifyTrafficMirrorFilterNetworkServices_addNetworkServices,
    modifyTrafficMirrorFilterNetworkServices_trafficMirrorFilterId,
    modifyTrafficMirrorFilterNetworkServicesResponse_trafficMirrorFilter,
    modifyTrafficMirrorFilterNetworkServicesResponse_httpStatus,

    -- ** ModifyTrafficMirrorFilterRule
    modifyTrafficMirrorFilterRule_ruleNumber,
    modifyTrafficMirrorFilterRule_description,
    modifyTrafficMirrorFilterRule_dryRun,
    modifyTrafficMirrorFilterRule_trafficDirection,
    modifyTrafficMirrorFilterRule_destinationCidrBlock,
    modifyTrafficMirrorFilterRule_destinationPortRange,
    modifyTrafficMirrorFilterRule_removeFields,
    modifyTrafficMirrorFilterRule_ruleAction,
    modifyTrafficMirrorFilterRule_sourceCidrBlock,
    modifyTrafficMirrorFilterRule_protocol,
    modifyTrafficMirrorFilterRule_sourcePortRange,
    modifyTrafficMirrorFilterRule_trafficMirrorFilterRuleId,
    modifyTrafficMirrorFilterRuleResponse_trafficMirrorFilterRule,
    modifyTrafficMirrorFilterRuleResponse_httpStatus,

    -- ** ModifyTrafficMirrorSession
    modifyTrafficMirrorSession_sessionNumber,
    modifyTrafficMirrorSession_trafficMirrorTargetId,
    modifyTrafficMirrorSession_trafficMirrorFilterId,
    modifyTrafficMirrorSession_description,
    modifyTrafficMirrorSession_packetLength,
    modifyTrafficMirrorSession_dryRun,
    modifyTrafficMirrorSession_removeFields,
    modifyTrafficMirrorSession_virtualNetworkId,
    modifyTrafficMirrorSession_trafficMirrorSessionId,
    modifyTrafficMirrorSessionResponse_trafficMirrorSession,
    modifyTrafficMirrorSessionResponse_httpStatus,

    -- ** ModifyTransitGateway
    modifyTransitGateway_description,
    modifyTransitGateway_dryRun,
    modifyTransitGateway_options,
    modifyTransitGateway_transitGatewayId,
    modifyTransitGatewayResponse_transitGateway,
    modifyTransitGatewayResponse_httpStatus,

    -- ** ModifyTransitGatewayPrefixListReference
    modifyTransitGatewayPrefixListReference_transitGatewayAttachmentId,
    modifyTransitGatewayPrefixListReference_dryRun,
    modifyTransitGatewayPrefixListReference_blackhole,
    modifyTransitGatewayPrefixListReference_transitGatewayRouteTableId,
    modifyTransitGatewayPrefixListReference_prefixListId,
    modifyTransitGatewayPrefixListReferenceResponse_transitGatewayPrefixListReference,
    modifyTransitGatewayPrefixListReferenceResponse_httpStatus,

    -- ** ModifyTransitGatewayVpcAttachment
    modifyTransitGatewayVpcAttachment_addSubnetIds,
    modifyTransitGatewayVpcAttachment_dryRun,
    modifyTransitGatewayVpcAttachment_options,
    modifyTransitGatewayVpcAttachment_removeSubnetIds,
    modifyTransitGatewayVpcAttachment_transitGatewayAttachmentId,
    modifyTransitGatewayVpcAttachmentResponse_transitGatewayVpcAttachment,
    modifyTransitGatewayVpcAttachmentResponse_httpStatus,

    -- ** ModifyVolume
    modifyVolume_size,
    modifyVolume_volumeType,
    modifyVolume_dryRun,
    modifyVolume_throughput,
    modifyVolume_multiAttachEnabled,
    modifyVolume_iops,
    modifyVolume_volumeId,
    modifyVolumeResponse_volumeModification,
    modifyVolumeResponse_httpStatus,

    -- ** ModifyVolumeAttribute
    modifyVolumeAttribute_dryRun,
    modifyVolumeAttribute_autoEnableIO,
    modifyVolumeAttribute_volumeId,

    -- ** ModifyVpcAttribute
    modifyVpcAttribute_enableDnsSupport,
    modifyVpcAttribute_enableDnsHostnames,
    modifyVpcAttribute_enableNetworkAddressUsageMetrics,
    modifyVpcAttribute_vpcId,

    -- ** ModifyVpcEndpoint
    modifyVpcEndpoint_privateDnsEnabled,
    modifyVpcEndpoint_resetPolicy,
    modifyVpcEndpoint_addSecurityGroupIds,
    modifyVpcEndpoint_addSubnetIds,
    modifyVpcEndpoint_addRouteTableIds,
    modifyVpcEndpoint_dryRun,
    modifyVpcEndpoint_policyDocument,
    modifyVpcEndpoint_removeSubnetIds,
    modifyVpcEndpoint_removeRouteTableIds,
    modifyVpcEndpoint_dnsOptions,
    modifyVpcEndpoint_ipAddressType,
    modifyVpcEndpoint_removeSecurityGroupIds,
    modifyVpcEndpoint_vpcEndpointId,
    modifyVpcEndpointResponse_return,
    modifyVpcEndpointResponse_httpStatus,

    -- ** ModifyVpcEndpointConnectionNotification
    modifyVpcEndpointConnectionNotification_connectionEvents,
    modifyVpcEndpointConnectionNotification_dryRun,
    modifyVpcEndpointConnectionNotification_connectionNotificationArn,
    modifyVpcEndpointConnectionNotification_connectionNotificationId,
    modifyVpcEndpointConnectionNotificationResponse_returnValue,
    modifyVpcEndpointConnectionNotificationResponse_httpStatus,

    -- ** ModifyVpcEndpointServiceConfiguration
    modifyVpcEndpointServiceConfiguration_acceptanceRequired,
    modifyVpcEndpointServiceConfiguration_removeNetworkLoadBalancerArns,
    modifyVpcEndpointServiceConfiguration_addNetworkLoadBalancerArns,
    modifyVpcEndpointServiceConfiguration_addGatewayLoadBalancerArns,
    modifyVpcEndpointServiceConfiguration_dryRun,
    modifyVpcEndpointServiceConfiguration_removeGatewayLoadBalancerArns,
    modifyVpcEndpointServiceConfiguration_removeSupportedIpAddressTypes,
    modifyVpcEndpointServiceConfiguration_privateDnsName,
    modifyVpcEndpointServiceConfiguration_addSupportedIpAddressTypes,
    modifyVpcEndpointServiceConfiguration_removePrivateDnsName,
    modifyVpcEndpointServiceConfiguration_serviceId,
    modifyVpcEndpointServiceConfigurationResponse_return,
    modifyVpcEndpointServiceConfigurationResponse_httpStatus,

    -- ** ModifyVpcEndpointServicePayerResponsibility
    modifyVpcEndpointServicePayerResponsibility_dryRun,
    modifyVpcEndpointServicePayerResponsibility_serviceId,
    modifyVpcEndpointServicePayerResponsibility_payerResponsibility,
    modifyVpcEndpointServicePayerResponsibilityResponse_returnValue,
    modifyVpcEndpointServicePayerResponsibilityResponse_httpStatus,

    -- ** ModifyVpcEndpointServicePermissions
    modifyVpcEndpointServicePermissions_dryRun,
    modifyVpcEndpointServicePermissions_addAllowedPrincipals,
    modifyVpcEndpointServicePermissions_removeAllowedPrincipals,
    modifyVpcEndpointServicePermissions_serviceId,
    modifyVpcEndpointServicePermissionsResponse_returnValue,
    modifyVpcEndpointServicePermissionsResponse_addedPrincipals,
    modifyVpcEndpointServicePermissionsResponse_httpStatus,

    -- ** ModifyVpcPeeringConnectionOptions
    modifyVpcPeeringConnectionOptions_requesterPeeringConnectionOptions,
    modifyVpcPeeringConnectionOptions_dryRun,
    modifyVpcPeeringConnectionOptions_accepterPeeringConnectionOptions,
    modifyVpcPeeringConnectionOptions_vpcPeeringConnectionId,
    modifyVpcPeeringConnectionOptionsResponse_requesterPeeringConnectionOptions,
    modifyVpcPeeringConnectionOptionsResponse_accepterPeeringConnectionOptions,
    modifyVpcPeeringConnectionOptionsResponse_httpStatus,

    -- ** ModifyVpcTenancy
    modifyVpcTenancy_dryRun,
    modifyVpcTenancy_vpcId,
    modifyVpcTenancy_instanceTenancy,
    modifyVpcTenancyResponse_returnValue,
    modifyVpcTenancyResponse_httpStatus,

    -- ** ModifyVpnConnection
    modifyVpnConnection_transitGatewayId,
    modifyVpnConnection_dryRun,
    modifyVpnConnection_customerGatewayId,
    modifyVpnConnection_vpnGatewayId,
    modifyVpnConnection_vpnConnectionId,
    modifyVpnConnectionResponse_vpnConnection,
    modifyVpnConnectionResponse_httpStatus,

    -- ** ModifyVpnConnectionOptions
    modifyVpnConnectionOptions_remoteIpv6NetworkCidr,
    modifyVpnConnectionOptions_localIpv4NetworkCidr,
    modifyVpnConnectionOptions_dryRun,
    modifyVpnConnectionOptions_remoteIpv4NetworkCidr,
    modifyVpnConnectionOptions_localIpv6NetworkCidr,
    modifyVpnConnectionOptions_vpnConnectionId,
    modifyVpnConnectionOptionsResponse_vpnConnection,
    modifyVpnConnectionOptionsResponse_httpStatus,

    -- ** ModifyVpnTunnelCertificate
    modifyVpnTunnelCertificate_dryRun,
    modifyVpnTunnelCertificate_vpnConnectionId,
    modifyVpnTunnelCertificate_vpnTunnelOutsideIpAddress,
    modifyVpnTunnelCertificateResponse_vpnConnection,
    modifyVpnTunnelCertificateResponse_httpStatus,

    -- ** ModifyVpnTunnelOptions
    modifyVpnTunnelOptions_dryRun,
    modifyVpnTunnelOptions_vpnConnectionId,
    modifyVpnTunnelOptions_vpnTunnelOutsideIpAddress,
    modifyVpnTunnelOptions_tunnelOptions,
    modifyVpnTunnelOptionsResponse_vpnConnection,
    modifyVpnTunnelOptionsResponse_httpStatus,

    -- ** MonitorInstances
    monitorInstances_dryRun,
    monitorInstances_instanceIds,
    monitorInstancesResponse_instanceMonitorings,
    monitorInstancesResponse_httpStatus,

    -- ** MoveAddressToVpc
    moveAddressToVpc_dryRun,
    moveAddressToVpc_publicIp,
    moveAddressToVpcResponse_allocationId,
    moveAddressToVpcResponse_status,
    moveAddressToVpcResponse_httpStatus,

    -- ** MoveByoipCidrToIpam
    moveByoipCidrToIpam_dryRun,
    moveByoipCidrToIpam_cidr,
    moveByoipCidrToIpam_ipamPoolId,
    moveByoipCidrToIpam_ipamPoolOwner,
    moveByoipCidrToIpamResponse_byoipCidr,
    moveByoipCidrToIpamResponse_httpStatus,

    -- ** ProvisionByoipCidr
    provisionByoipCidr_publiclyAdvertisable,
    provisionByoipCidr_poolTagSpecifications,
    provisionByoipCidr_description,
    provisionByoipCidr_dryRun,
    provisionByoipCidr_multiRegion,
    provisionByoipCidr_cidrAuthorizationContext,
    provisionByoipCidr_cidr,
    provisionByoipCidrResponse_byoipCidr,
    provisionByoipCidrResponse_httpStatus,

    -- ** ProvisionIpamPoolCidr
    provisionIpamPoolCidr_cidr,
    provisionIpamPoolCidr_dryRun,
    provisionIpamPoolCidr_cidrAuthorizationContext,
    provisionIpamPoolCidr_ipamPoolId,
    provisionIpamPoolCidrResponse_ipamPoolCidr,
    provisionIpamPoolCidrResponse_httpStatus,

    -- ** ProvisionPublicIpv4PoolCidr
    provisionPublicIpv4PoolCidr_dryRun,
    provisionPublicIpv4PoolCidr_ipamPoolId,
    provisionPublicIpv4PoolCidr_poolId,
    provisionPublicIpv4PoolCidr_netmaskLength,
    provisionPublicIpv4PoolCidrResponse_poolAddressRange,
    provisionPublicIpv4PoolCidrResponse_poolId,
    provisionPublicIpv4PoolCidrResponse_httpStatus,

    -- ** PurchaseHostReservation
    purchaseHostReservation_clientToken,
    purchaseHostReservation_limitPrice,
    purchaseHostReservation_currencyCode,
    purchaseHostReservation_tagSpecifications,
    purchaseHostReservation_hostIdSet,
    purchaseHostReservation_offeringId,
    purchaseHostReservationResponse_clientToken,
    purchaseHostReservationResponse_totalHourlyPrice,
    purchaseHostReservationResponse_totalUpfrontPrice,
    purchaseHostReservationResponse_currencyCode,
    purchaseHostReservationResponse_purchase,
    purchaseHostReservationResponse_httpStatus,

    -- ** PurchaseReservedInstancesOffering
    purchaseReservedInstancesOffering_limitPrice,
    purchaseReservedInstancesOffering_dryRun,
    purchaseReservedInstancesOffering_purchaseTime,
    purchaseReservedInstancesOffering_instanceCount,
    purchaseReservedInstancesOffering_reservedInstancesOfferingId,
    purchaseReservedInstancesOfferingResponse_reservedInstancesId,
    purchaseReservedInstancesOfferingResponse_httpStatus,

    -- ** PurchaseScheduledInstances
    purchaseScheduledInstances_clientToken,
    purchaseScheduledInstances_dryRun,
    purchaseScheduledInstances_purchaseRequests,
    purchaseScheduledInstancesResponse_scheduledInstanceSet,
    purchaseScheduledInstancesResponse_httpStatus,

    -- ** RebootInstances
    rebootInstances_dryRun,
    rebootInstances_instanceIds,

    -- ** RegisterImage
    registerImage_sriovNetSupport,
    registerImage_blockDeviceMappings,
    registerImage_virtualizationType,
    registerImage_description,
    registerImage_dryRun,
    registerImage_billingProducts,
    registerImage_tpmSupport,
    registerImage_ramdiskId,
    registerImage_imageLocation,
    registerImage_imdsSupport,
    registerImage_bootMode,
    registerImage_kernelId,
    registerImage_architecture,
    registerImage_enaSupport,
    registerImage_rootDeviceName,
    registerImage_uefiData,
    registerImage_name,
    registerImageResponse_imageId,
    registerImageResponse_httpStatus,

    -- ** RegisterInstanceEventNotificationAttributes
    registerInstanceEventNotificationAttributes_instanceTagAttribute,
    registerInstanceEventNotificationAttributes_dryRun,
    registerInstanceEventNotificationAttributesResponse_instanceTagAttribute,
    registerInstanceEventNotificationAttributesResponse_httpStatus,

    -- ** RegisterTransitGatewayMulticastGroupMembers
    registerTransitGatewayMulticastGroupMembers_groupIpAddress,
    registerTransitGatewayMulticastGroupMembers_dryRun,
    registerTransitGatewayMulticastGroupMembers_transitGatewayMulticastDomainId,
    registerTransitGatewayMulticastGroupMembers_networkInterfaceIds,
    registerTransitGatewayMulticastGroupMembersResponse_registeredMulticastGroupMembers,
    registerTransitGatewayMulticastGroupMembersResponse_httpStatus,

    -- ** RegisterTransitGatewayMulticastGroupSources
    registerTransitGatewayMulticastGroupSources_groupIpAddress,
    registerTransitGatewayMulticastGroupSources_dryRun,
    registerTransitGatewayMulticastGroupSources_transitGatewayMulticastDomainId,
    registerTransitGatewayMulticastGroupSources_networkInterfaceIds,
    registerTransitGatewayMulticastGroupSourcesResponse_registeredMulticastGroupSources,
    registerTransitGatewayMulticastGroupSourcesResponse_httpStatus,

    -- ** RejectTransitGatewayMulticastDomainAssociations
    rejectTransitGatewayMulticastDomainAssociations_transitGatewayAttachmentId,
    rejectTransitGatewayMulticastDomainAssociations_dryRun,
    rejectTransitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId,
    rejectTransitGatewayMulticastDomainAssociations_subnetIds,
    rejectTransitGatewayMulticastDomainAssociationsResponse_associations,
    rejectTransitGatewayMulticastDomainAssociationsResponse_httpStatus,

    -- ** RejectTransitGatewayPeeringAttachment
    rejectTransitGatewayPeeringAttachment_dryRun,
    rejectTransitGatewayPeeringAttachment_transitGatewayAttachmentId,
    rejectTransitGatewayPeeringAttachmentResponse_transitGatewayPeeringAttachment,
    rejectTransitGatewayPeeringAttachmentResponse_httpStatus,

    -- ** RejectTransitGatewayVpcAttachment
    rejectTransitGatewayVpcAttachment_dryRun,
    rejectTransitGatewayVpcAttachment_transitGatewayAttachmentId,
    rejectTransitGatewayVpcAttachmentResponse_transitGatewayVpcAttachment,
    rejectTransitGatewayVpcAttachmentResponse_httpStatus,

    -- ** RejectVpcEndpointConnections
    rejectVpcEndpointConnections_dryRun,
    rejectVpcEndpointConnections_serviceId,
    rejectVpcEndpointConnections_vpcEndpointIds,
    rejectVpcEndpointConnectionsResponse_unsuccessful,
    rejectVpcEndpointConnectionsResponse_httpStatus,

    -- ** RejectVpcPeeringConnection
    rejectVpcPeeringConnection_dryRun,
    rejectVpcPeeringConnection_vpcPeeringConnectionId,
    rejectVpcPeeringConnectionResponse_return,
    rejectVpcPeeringConnectionResponse_httpStatus,

    -- ** ReleaseAddress
    releaseAddress_allocationId,
    releaseAddress_networkBorderGroup,
    releaseAddress_publicIp,
    releaseAddress_dryRun,

    -- ** ReleaseHosts
    releaseHosts_hostIds,
    releaseHostsResponse_unsuccessful,
    releaseHostsResponse_successful,
    releaseHostsResponse_httpStatus,

    -- ** ReleaseIpamPoolAllocation
    releaseIpamPoolAllocation_dryRun,
    releaseIpamPoolAllocation_ipamPoolId,
    releaseIpamPoolAllocation_cidr,
    releaseIpamPoolAllocation_ipamPoolAllocationId,
    releaseIpamPoolAllocationResponse_success,
    releaseIpamPoolAllocationResponse_httpStatus,

    -- ** ReplaceIamInstanceProfileAssociation
    replaceIamInstanceProfileAssociation_iamInstanceProfile,
    replaceIamInstanceProfileAssociation_associationId,
    replaceIamInstanceProfileAssociationResponse_iamInstanceProfileAssociation,
    replaceIamInstanceProfileAssociationResponse_httpStatus,

    -- ** ReplaceNetworkAclAssociation
    replaceNetworkAclAssociation_dryRun,
    replaceNetworkAclAssociation_associationId,
    replaceNetworkAclAssociation_networkAclId,
    replaceNetworkAclAssociationResponse_newAssociationId,
    replaceNetworkAclAssociationResponse_httpStatus,

    -- ** ReplaceNetworkAclEntry
    replaceNetworkAclEntry_icmpTypeCode,
    replaceNetworkAclEntry_portRange,
    replaceNetworkAclEntry_dryRun,
    replaceNetworkAclEntry_cidrBlock,
    replaceNetworkAclEntry_ipv6CidrBlock,
    replaceNetworkAclEntry_egress,
    replaceNetworkAclEntry_networkAclId,
    replaceNetworkAclEntry_protocol,
    replaceNetworkAclEntry_ruleAction,
    replaceNetworkAclEntry_ruleNumber,

    -- ** ReplaceRoute
    replaceRoute_localGatewayId,
    replaceRoute_destinationPrefixListId,
    replaceRoute_carrierGatewayId,
    replaceRoute_transitGatewayId,
    replaceRoute_natGatewayId,
    replaceRoute_vpcPeeringConnectionId,
    replaceRoute_vpcEndpointId,
    replaceRoute_dryRun,
    replaceRoute_destinationCidrBlock,
    replaceRoute_coreNetworkArn,
    replaceRoute_instanceId,
    replaceRoute_egressOnlyInternetGatewayId,
    replaceRoute_networkInterfaceId,
    replaceRoute_gatewayId,
    replaceRoute_destinationIpv6CidrBlock,
    replaceRoute_localTarget,
    replaceRoute_routeTableId,

    -- ** ReplaceRouteTableAssociation
    replaceRouteTableAssociation_dryRun,
    replaceRouteTableAssociation_associationId,
    replaceRouteTableAssociation_routeTableId,
    replaceRouteTableAssociationResponse_newAssociationId,
    replaceRouteTableAssociationResponse_associationState,
    replaceRouteTableAssociationResponse_httpStatus,

    -- ** ReplaceTransitGatewayRoute
    replaceTransitGatewayRoute_transitGatewayAttachmentId,
    replaceTransitGatewayRoute_dryRun,
    replaceTransitGatewayRoute_blackhole,
    replaceTransitGatewayRoute_destinationCidrBlock,
    replaceTransitGatewayRoute_transitGatewayRouteTableId,
    replaceTransitGatewayRouteResponse_route,
    replaceTransitGatewayRouteResponse_httpStatus,

    -- ** ReportInstanceStatus
    reportInstanceStatus_endTime,
    reportInstanceStatus_description,
    reportInstanceStatus_dryRun,
    reportInstanceStatus_startTime,
    reportInstanceStatus_instances,
    reportInstanceStatus_reasonCodes,
    reportInstanceStatus_status,

    -- ** RequestSpotFleet
    requestSpotFleet_dryRun,
    requestSpotFleet_spotFleetRequestConfig,
    requestSpotFleetResponse_spotFleetRequestId,
    requestSpotFleetResponse_httpStatus,

    -- ** RequestSpotInstances
    requestSpotInstances_clientToken,
    requestSpotInstances_type,
    requestSpotInstances_blockDurationMinutes,
    requestSpotInstances_availabilityZoneGroup,
    requestSpotInstances_validFrom,
    requestSpotInstances_dryRun,
    requestSpotInstances_instanceInterruptionBehavior,
    requestSpotInstances_instanceCount,
    requestSpotInstances_validUntil,
    requestSpotInstances_launchGroup,
    requestSpotInstances_tagSpecifications,
    requestSpotInstances_launchSpecification,
    requestSpotInstances_spotPrice,
    requestSpotInstancesResponse_spotInstanceRequests,
    requestSpotInstancesResponse_httpStatus,

    -- ** ResetAddressAttribute
    resetAddressAttribute_dryRun,
    resetAddressAttribute_allocationId,
    resetAddressAttribute_attribute,
    resetAddressAttributeResponse_address,
    resetAddressAttributeResponse_httpStatus,

    -- ** ResetEbsDefaultKmsKeyId
    resetEbsDefaultKmsKeyId_dryRun,
    resetEbsDefaultKmsKeyIdResponse_kmsKeyId,
    resetEbsDefaultKmsKeyIdResponse_httpStatus,

    -- ** ResetFpgaImageAttribute
    resetFpgaImageAttribute_attribute,
    resetFpgaImageAttribute_dryRun,
    resetFpgaImageAttribute_fpgaImageId,
    resetFpgaImageAttributeResponse_return,
    resetFpgaImageAttributeResponse_httpStatus,

    -- ** ResetImageAttribute
    resetImageAttribute_dryRun,
    resetImageAttribute_attribute,
    resetImageAttribute_imageId,

    -- ** ResetInstanceAttribute
    resetInstanceAttribute_dryRun,
    resetInstanceAttribute_attribute,
    resetInstanceAttribute_instanceId,

    -- ** ResetNetworkInterfaceAttribute
    resetNetworkInterfaceAttribute_sourceDestCheck,
    resetNetworkInterfaceAttribute_dryRun,
    resetNetworkInterfaceAttribute_networkInterfaceId,

    -- ** ResetSnapshotAttribute
    resetSnapshotAttribute_dryRun,
    resetSnapshotAttribute_attribute,
    resetSnapshotAttribute_snapshotId,

    -- ** RestoreAddressToClassic
    restoreAddressToClassic_dryRun,
    restoreAddressToClassic_publicIp,
    restoreAddressToClassicResponse_status,
    restoreAddressToClassicResponse_publicIp,
    restoreAddressToClassicResponse_httpStatus,

    -- ** RestoreImageFromRecycleBin
    restoreImageFromRecycleBin_dryRun,
    restoreImageFromRecycleBin_imageId,
    restoreImageFromRecycleBinResponse_return,
    restoreImageFromRecycleBinResponse_httpStatus,

    -- ** RestoreManagedPrefixListVersion
    restoreManagedPrefixListVersion_dryRun,
    restoreManagedPrefixListVersion_prefixListId,
    restoreManagedPrefixListVersion_previousVersion,
    restoreManagedPrefixListVersion_currentVersion,
    restoreManagedPrefixListVersionResponse_prefixList,
    restoreManagedPrefixListVersionResponse_httpStatus,

    -- ** RestoreSnapshotFromRecycleBin
    restoreSnapshotFromRecycleBin_dryRun,
    restoreSnapshotFromRecycleBin_snapshotId,
    restoreSnapshotFromRecycleBinResponse_progress,
    restoreSnapshotFromRecycleBinResponse_outpostArn,
    restoreSnapshotFromRecycleBinResponse_ownerId,
    restoreSnapshotFromRecycleBinResponse_snapshotId,
    restoreSnapshotFromRecycleBinResponse_state,
    restoreSnapshotFromRecycleBinResponse_volumeSize,
    restoreSnapshotFromRecycleBinResponse_description,
    restoreSnapshotFromRecycleBinResponse_encrypted,
    restoreSnapshotFromRecycleBinResponse_volumeId,
    restoreSnapshotFromRecycleBinResponse_startTime,
    restoreSnapshotFromRecycleBinResponse_httpStatus,

    -- ** RestoreSnapshotTier
    restoreSnapshotTier_temporaryRestoreDays,
    restoreSnapshotTier_dryRun,
    restoreSnapshotTier_permanentRestore,
    restoreSnapshotTier_snapshotId,
    restoreSnapshotTierResponse_restoreStartTime,
    restoreSnapshotTierResponse_snapshotId,
    restoreSnapshotTierResponse_restoreDuration,
    restoreSnapshotTierResponse_isPermanentRestore,
    restoreSnapshotTierResponse_httpStatus,

    -- ** RevokeClientVpnIngress
    revokeClientVpnIngress_revokeAllGroups,
    revokeClientVpnIngress_dryRun,
    revokeClientVpnIngress_accessGroupId,
    revokeClientVpnIngress_clientVpnEndpointId,
    revokeClientVpnIngress_targetNetworkCidr,
    revokeClientVpnIngressResponse_status,
    revokeClientVpnIngressResponse_httpStatus,

    -- ** RevokeSecurityGroupEgress
    revokeSecurityGroupEgress_sourceSecurityGroupName,
    revokeSecurityGroupEgress_toPort,
    revokeSecurityGroupEgress_ipPermissions,
    revokeSecurityGroupEgress_ipProtocol,
    revokeSecurityGroupEgress_dryRun,
    revokeSecurityGroupEgress_securityGroupRuleIds,
    revokeSecurityGroupEgress_cidrIp,
    revokeSecurityGroupEgress_sourceSecurityGroupOwnerId,
    revokeSecurityGroupEgress_fromPort,
    revokeSecurityGroupEgress_groupId,
    revokeSecurityGroupEgressResponse_return,
    revokeSecurityGroupEgressResponse_unknownIpPermissions,
    revokeSecurityGroupEgressResponse_httpStatus,

    -- ** RevokeSecurityGroupIngress
    revokeSecurityGroupIngress_sourceSecurityGroupName,
    revokeSecurityGroupIngress_toPort,
    revokeSecurityGroupIngress_ipPermissions,
    revokeSecurityGroupIngress_ipProtocol,
    revokeSecurityGroupIngress_groupName,
    revokeSecurityGroupIngress_dryRun,
    revokeSecurityGroupIngress_securityGroupRuleIds,
    revokeSecurityGroupIngress_cidrIp,
    revokeSecurityGroupIngress_sourceSecurityGroupOwnerId,
    revokeSecurityGroupIngress_fromPort,
    revokeSecurityGroupIngress_groupId,
    revokeSecurityGroupIngressResponse_return,
    revokeSecurityGroupIngressResponse_unknownIpPermissions,
    revokeSecurityGroupIngressResponse_httpStatus,

    -- ** RunInstances
    runInstances_ebsOptimized,
    runInstances_hibernationOptions,
    runInstances_iamInstanceProfile,
    runInstances_elasticInferenceAccelerators,
    runInstances_placement,
    runInstances_clientToken,
    runInstances_userData,
    runInstances_additionalInfo,
    runInstances_creditSpecification,
    runInstances_blockDeviceMappings,
    runInstances_securityGroupIds,
    runInstances_instanceInitiatedShutdownBehavior,
    runInstances_monitoring,
    runInstances_instanceMarketOptions,
    runInstances_subnetId,
    runInstances_launchTemplate,
    runInstances_capacityReservationSpecification,
    runInstances_dryRun,
    runInstances_instanceType,
    runInstances_ipv6AddressCount,
    runInstances_securityGroups,
    runInstances_ramdiskId,
    runInstances_privateIpAddress,
    runInstances_maintenanceOptions,
    runInstances_privateDnsNameOptions,
    runInstances_keyName,
    runInstances_licenseSpecifications,
    runInstances_kernelId,
    runInstances_disableApiTermination,
    runInstances_tagSpecifications,
    runInstances_cpuOptions,
    runInstances_ipv6Addresses,
    runInstances_disableApiStop,
    runInstances_elasticGpuSpecification,
    runInstances_imageId,
    runInstances_networkInterfaces,
    runInstances_enclaveOptions,
    runInstances_metadataOptions,
    runInstances_maxCount,
    runInstances_minCount,
    reservation_instances,
    reservation_groups,
    reservation_requesterId,
    reservation_reservationId,
    reservation_ownerId,

    -- ** RunScheduledInstances
    runScheduledInstances_clientToken,
    runScheduledInstances_dryRun,
    runScheduledInstances_instanceCount,
    runScheduledInstances_launchSpecification,
    runScheduledInstances_scheduledInstanceId,
    runScheduledInstancesResponse_instanceIdSet,
    runScheduledInstancesResponse_httpStatus,

    -- ** SearchLocalGatewayRoutes
    searchLocalGatewayRoutes_nextToken,
    searchLocalGatewayRoutes_filters,
    searchLocalGatewayRoutes_dryRun,
    searchLocalGatewayRoutes_maxResults,
    searchLocalGatewayRoutes_localGatewayRouteTableId,
    searchLocalGatewayRoutesResponse_nextToken,
    searchLocalGatewayRoutesResponse_routes,
    searchLocalGatewayRoutesResponse_httpStatus,

    -- ** SearchTransitGatewayMulticastGroups
    searchTransitGatewayMulticastGroups_nextToken,
    searchTransitGatewayMulticastGroups_filters,
    searchTransitGatewayMulticastGroups_dryRun,
    searchTransitGatewayMulticastGroups_maxResults,
    searchTransitGatewayMulticastGroups_transitGatewayMulticastDomainId,
    searchTransitGatewayMulticastGroupsResponse_nextToken,
    searchTransitGatewayMulticastGroupsResponse_multicastGroups,
    searchTransitGatewayMulticastGroupsResponse_httpStatus,

    -- ** SearchTransitGatewayRoutes
    searchTransitGatewayRoutes_dryRun,
    searchTransitGatewayRoutes_maxResults,
    searchTransitGatewayRoutes_transitGatewayRouteTableId,
    searchTransitGatewayRoutes_filters,
    searchTransitGatewayRoutesResponse_additionalRoutesAvailable,
    searchTransitGatewayRoutesResponse_routes,
    searchTransitGatewayRoutesResponse_httpStatus,

    -- ** SendDiagnosticInterrupt
    sendDiagnosticInterrupt_dryRun,
    sendDiagnosticInterrupt_instanceId,

    -- ** StartInstances
    startInstances_additionalInfo,
    startInstances_dryRun,
    startInstances_instanceIds,
    startInstancesResponse_startingInstances,
    startInstancesResponse_httpStatus,

    -- ** StartNetworkInsightsAccessScopeAnalysis
    startNetworkInsightsAccessScopeAnalysis_dryRun,
    startNetworkInsightsAccessScopeAnalysis_tagSpecifications,
    startNetworkInsightsAccessScopeAnalysis_networkInsightsAccessScopeId,
    startNetworkInsightsAccessScopeAnalysis_clientToken,
    startNetworkInsightsAccessScopeAnalysisResponse_networkInsightsAccessScopeAnalysis,
    startNetworkInsightsAccessScopeAnalysisResponse_httpStatus,

    -- ** StartNetworkInsightsAnalysis
    startNetworkInsightsAnalysis_filterInArns,
    startNetworkInsightsAnalysis_dryRun,
    startNetworkInsightsAnalysis_tagSpecifications,
    startNetworkInsightsAnalysis_networkInsightsPathId,
    startNetworkInsightsAnalysis_clientToken,
    startNetworkInsightsAnalysisResponse_networkInsightsAnalysis,
    startNetworkInsightsAnalysisResponse_httpStatus,

    -- ** StartVpcEndpointServicePrivateDnsVerification
    startVpcEndpointServicePrivateDnsVerification_dryRun,
    startVpcEndpointServicePrivateDnsVerification_serviceId,
    startVpcEndpointServicePrivateDnsVerificationResponse_returnValue,
    startVpcEndpointServicePrivateDnsVerificationResponse_httpStatus,

    -- ** StopInstances
    stopInstances_hibernate,
    stopInstances_dryRun,
    stopInstances_force,
    stopInstances_instanceIds,
    stopInstancesResponse_stoppingInstances,
    stopInstancesResponse_httpStatus,

    -- ** TerminateClientVpnConnections
    terminateClientVpnConnections_username,
    terminateClientVpnConnections_connectionId,
    terminateClientVpnConnections_dryRun,
    terminateClientVpnConnections_clientVpnEndpointId,
    terminateClientVpnConnectionsResponse_connectionStatuses,
    terminateClientVpnConnectionsResponse_username,
    terminateClientVpnConnectionsResponse_clientVpnEndpointId,
    terminateClientVpnConnectionsResponse_httpStatus,

    -- ** TerminateInstances
    terminateInstances_dryRun,
    terminateInstances_instanceIds,
    terminateInstancesResponse_terminatingInstances,
    terminateInstancesResponse_httpStatus,

    -- ** UnassignIpv6Addresses
    unassignIpv6Addresses_ipv6Prefixes,
    unassignIpv6Addresses_ipv6Addresses,
    unassignIpv6Addresses_networkInterfaceId,
    unassignIpv6AddressesResponse_unassignedIpv6Addresses,
    unassignIpv6AddressesResponse_networkInterfaceId,
    unassignIpv6AddressesResponse_unassignedIpv6Prefixes,
    unassignIpv6AddressesResponse_httpStatus,

    -- ** UnassignPrivateIpAddresses
    unassignPrivateIpAddresses_privateIpAddresses,
    unassignPrivateIpAddresses_ipv4Prefixes,
    unassignPrivateIpAddresses_networkInterfaceId,

    -- ** UnmonitorInstances
    unmonitorInstances_dryRun,
    unmonitorInstances_instanceIds,
    unmonitorInstancesResponse_instanceMonitorings,
    unmonitorInstancesResponse_httpStatus,

    -- ** UpdateSecurityGroupRuleDescriptionsEgress
    updateSecurityGroupRuleDescriptionsEgress_ipPermissions,
    updateSecurityGroupRuleDescriptionsEgress_groupName,
    updateSecurityGroupRuleDescriptionsEgress_dryRun,
    updateSecurityGroupRuleDescriptionsEgress_securityGroupRuleDescriptions,
    updateSecurityGroupRuleDescriptionsEgress_groupId,
    updateSecurityGroupRuleDescriptionsEgressResponse_return,
    updateSecurityGroupRuleDescriptionsEgressResponse_httpStatus,

    -- ** UpdateSecurityGroupRuleDescriptionsIngress
    updateSecurityGroupRuleDescriptionsIngress_ipPermissions,
    updateSecurityGroupRuleDescriptionsIngress_groupName,
    updateSecurityGroupRuleDescriptionsIngress_dryRun,
    updateSecurityGroupRuleDescriptionsIngress_securityGroupRuleDescriptions,
    updateSecurityGroupRuleDescriptionsIngress_groupId,
    updateSecurityGroupRuleDescriptionsIngressResponse_return,
    updateSecurityGroupRuleDescriptionsIngressResponse_httpStatus,

    -- ** WithdrawByoipCidr
    withdrawByoipCidr_dryRun,
    withdrawByoipCidr_cidr,
    withdrawByoipCidrResponse_byoipCidr,
    withdrawByoipCidrResponse_httpStatus,

    -- * Types

    -- ** AcceleratorCount
    acceleratorCount_max,
    acceleratorCount_min,

    -- ** AcceleratorCountRequest
    acceleratorCountRequest_max,
    acceleratorCountRequest_min,

    -- ** AcceleratorTotalMemoryMiB
    acceleratorTotalMemoryMiB_max,
    acceleratorTotalMemoryMiB_min,

    -- ** AcceleratorTotalMemoryMiBRequest
    acceleratorTotalMemoryMiBRequest_max,
    acceleratorTotalMemoryMiBRequest_min,

    -- ** AccessScopeAnalysisFinding
    accessScopeAnalysisFinding_findingComponents,
    accessScopeAnalysisFinding_networkInsightsAccessScopeAnalysisId,
    accessScopeAnalysisFinding_networkInsightsAccessScopeId,
    accessScopeAnalysisFinding_findingId,

    -- ** AccessScopePath
    accessScopePath_destination,
    accessScopePath_source,
    accessScopePath_throughResources,

    -- ** AccessScopePathRequest
    accessScopePathRequest_destination,
    accessScopePathRequest_source,
    accessScopePathRequest_throughResources,

    -- ** AccountAttribute
    accountAttribute_attributeValues,
    accountAttribute_attributeName,

    -- ** AccountAttributeValue
    accountAttributeValue_attributeValue,

    -- ** ActiveInstance
    activeInstance_spotInstanceRequestId,
    activeInstance_instanceHealth,
    activeInstance_instanceType,
    activeInstance_instanceId,

    -- ** AddIpamOperatingRegion
    addIpamOperatingRegion_regionName,

    -- ** AddPrefixListEntry
    addPrefixListEntry_description,
    addPrefixListEntry_cidr,

    -- ** AddedPrincipal
    addedPrincipal_principal,
    addedPrincipal_servicePermissionId,
    addedPrincipal_principalType,
    addedPrincipal_serviceId,

    -- ** AdditionalDetail
    additionalDetail_additionalDetailType,
    additionalDetail_component,

    -- ** Address
    address_tags,
    address_allocationId,
    address_networkBorderGroup,
    address_domain,
    address_networkInterfaceOwnerId,
    address_carrierIp,
    address_customerOwnedIpv4Pool,
    address_publicIp,
    address_instanceId,
    address_networkInterfaceId,
    address_privateIpAddress,
    address_customerOwnedIp,
    address_associationId,
    address_publicIpv4Pool,

    -- ** AddressAttribute
    addressAttribute_ptrRecord,
    addressAttribute_allocationId,
    addressAttribute_publicIp,
    addressAttribute_ptrRecordUpdate,

    -- ** AddressTransfer
    addressTransfer_allocationId,
    addressTransfer_transferOfferExpirationTimestamp,
    addressTransfer_transferOfferAcceptedTimestamp,
    addressTransfer_transferAccountId,
    addressTransfer_publicIp,
    addressTransfer_addressTransferStatus,

    -- ** AllowedPrincipal
    allowedPrincipal_tags,
    allowedPrincipal_principal,
    allowedPrincipal_servicePermissionId,
    allowedPrincipal_principalType,
    allowedPrincipal_serviceId,

    -- ** AlternatePathHint
    alternatePathHint_componentArn,
    alternatePathHint_componentId,

    -- ** AnalysisAclRule
    analysisAclRule_egress,
    analysisAclRule_portRange,
    analysisAclRule_cidr,
    analysisAclRule_ruleNumber,
    analysisAclRule_ruleAction,
    analysisAclRule_protocol,

    -- ** AnalysisComponent
    analysisComponent_name,
    analysisComponent_arn,
    analysisComponent_id,

    -- ** AnalysisLoadBalancerListener
    analysisLoadBalancerListener_instancePort,
    analysisLoadBalancerListener_loadBalancerPort,

    -- ** AnalysisLoadBalancerTarget
    analysisLoadBalancerTarget_port,
    analysisLoadBalancerTarget_availabilityZone,
    analysisLoadBalancerTarget_address,
    analysisLoadBalancerTarget_instance,

    -- ** AnalysisPacketHeader
    analysisPacketHeader_sourcePortRanges,
    analysisPacketHeader_destinationAddresses,
    analysisPacketHeader_sourceAddresses,
    analysisPacketHeader_protocol,
    analysisPacketHeader_destinationPortRanges,

    -- ** AnalysisRouteTableRoute
    analysisRouteTableRoute_destinationPrefixListId,
    analysisRouteTableRoute_transitGatewayId,
    analysisRouteTableRoute_destinationCidr,
    analysisRouteTableRoute_state,
    analysisRouteTableRoute_natGatewayId,
    analysisRouteTableRoute_vpcPeeringConnectionId,
    analysisRouteTableRoute_instanceId,
    analysisRouteTableRoute_egressOnlyInternetGatewayId,
    analysisRouteTableRoute_networkInterfaceId,
    analysisRouteTableRoute_gatewayId,
    analysisRouteTableRoute_origin,

    -- ** AnalysisSecurityGroupRule
    analysisSecurityGroupRule_portRange,
    analysisSecurityGroupRule_cidr,
    analysisSecurityGroupRule_prefixListId,
    analysisSecurityGroupRule_securityGroupId,
    analysisSecurityGroupRule_protocol,
    analysisSecurityGroupRule_direction,

    -- ** AssignedPrivateIpAddress
    assignedPrivateIpAddress_privateIpAddress,

    -- ** AssociatedRole
    associatedRole_certificateS3ObjectKey,
    associatedRole_encryptionKmsKeyId,
    associatedRole_associatedRoleArn,
    associatedRole_certificateS3BucketName,

    -- ** AssociatedTargetNetwork
    associatedTargetNetwork_networkId,
    associatedTargetNetwork_networkType,

    -- ** AssociationStatus
    associationStatus_message,
    associationStatus_code,

    -- ** AthenaIntegration
    athenaIntegration_partitionEndDate,
    athenaIntegration_partitionStartDate,
    athenaIntegration_integrationResultS3DestinationArn,
    athenaIntegration_partitionLoadFrequency,

    -- ** AttributeBooleanValue
    attributeBooleanValue_value,

    -- ** AttributeValue
    attributeValue_value,

    -- ** AuthorizationRule
    authorizationRule_destinationCidr,
    authorizationRule_accessAll,
    authorizationRule_status,
    authorizationRule_description,
    authorizationRule_clientVpnEndpointId,
    authorizationRule_groupId,

    -- ** AvailabilityZone
    availabilityZone_zoneType,
    availabilityZone_parentZoneId,
    availabilityZone_parentZoneName,
    availabilityZone_zoneName,
    availabilityZone_networkBorderGroup,
    availabilityZone_state,
    availabilityZone_optInStatus,
    availabilityZone_groupName,
    availabilityZone_messages,
    availabilityZone_regionName,
    availabilityZone_zoneId,

    -- ** AvailabilityZoneMessage
    availabilityZoneMessage_message,

    -- ** AvailableCapacity
    availableCapacity_availableVCpus,
    availableCapacity_availableInstanceCapacity,

    -- ** BaselineEbsBandwidthMbps
    baselineEbsBandwidthMbps_max,
    baselineEbsBandwidthMbps_min,

    -- ** BaselineEbsBandwidthMbpsRequest
    baselineEbsBandwidthMbpsRequest_max,
    baselineEbsBandwidthMbpsRequest_min,

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
    byoipCidr_cidr,
    byoipCidr_state,
    byoipCidr_description,
    byoipCidr_statusMessage,

    -- ** CancelCapacityReservationFleetError
    cancelCapacityReservationFleetError_message,
    cancelCapacityReservationFleetError_code,

    -- ** CancelSpotFleetRequestsError
    cancelSpotFleetRequestsError_message,
    cancelSpotFleetRequestsError_code,

    -- ** CancelSpotFleetRequestsErrorItem
    cancelSpotFleetRequestsErrorItem_spotFleetRequestId,
    cancelSpotFleetRequestsErrorItem_error,

    -- ** CancelSpotFleetRequestsSuccessItem
    cancelSpotFleetRequestsSuccessItem_currentSpotFleetRequestState,
    cancelSpotFleetRequestsSuccessItem_spotFleetRequestId,
    cancelSpotFleetRequestsSuccessItem_previousSpotFleetRequestState,

    -- ** CancelledSpotInstanceRequest
    cancelledSpotInstanceRequest_spotInstanceRequestId,
    cancelledSpotInstanceRequest_state,

    -- ** CapacityAllocation
    capacityAllocation_count,
    capacityAllocation_allocationType,

    -- ** CapacityReservation
    capacityReservation_ebsOptimized,
    capacityReservation_tags,
    capacityReservation_capacityReservationFleetId,
    capacityReservation_capacityAllocations,
    capacityReservation_ephemeralStorage,
    capacityReservation_placementGroupArn,
    capacityReservation_outpostArn,
    capacityReservation_ownerId,
    capacityReservation_totalInstanceCount,
    capacityReservation_endDate,
    capacityReservation_state,
    capacityReservation_capacityReservationId,
    capacityReservation_availabilityZone,
    capacityReservation_capacityReservationArn,
    capacityReservation_instanceType,
    capacityReservation_instancePlatform,
    capacityReservation_startDate,
    capacityReservation_createDate,
    capacityReservation_availableInstanceCount,
    capacityReservation_instanceMatchCriteria,
    capacityReservation_endDateType,
    capacityReservation_tenancy,
    capacityReservation_availabilityZoneId,

    -- ** CapacityReservationFleet
    capacityReservationFleet_tags,
    capacityReservationFleet_capacityReservationFleetId,
    capacityReservationFleet_endDate,
    capacityReservationFleet_capacityReservationFleetArn,
    capacityReservationFleet_state,
    capacityReservationFleet_totalFulfilledCapacity,
    capacityReservationFleet_totalTargetCapacity,
    capacityReservationFleet_allocationStrategy,
    capacityReservationFleet_instanceTypeSpecifications,
    capacityReservationFleet_createTime,
    capacityReservationFleet_instanceMatchCriteria,
    capacityReservationFleet_tenancy,

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
    capacityReservationSpecification_capacityReservationPreference,
    capacityReservationSpecification_capacityReservationTarget,

    -- ** CapacityReservationSpecificationResponse
    capacityReservationSpecificationResponse_capacityReservationPreference,
    capacityReservationSpecificationResponse_capacityReservationTarget,

    -- ** CapacityReservationTarget
    capacityReservationTarget_capacityReservationId,
    capacityReservationTarget_capacityReservationResourceGroupArn,

    -- ** CapacityReservationTargetResponse
    capacityReservationTargetResponse_capacityReservationId,
    capacityReservationTargetResponse_capacityReservationResourceGroupArn,

    -- ** CarrierGateway
    carrierGateway_tags,
    carrierGateway_carrierGatewayId,
    carrierGateway_ownerId,
    carrierGateway_state,
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
    classicLinkInstance_tags,
    classicLinkInstance_instanceId,
    classicLinkInstance_vpcId,
    classicLinkInstance_groups,

    -- ** ClassicLoadBalancer
    classicLoadBalancer_name,

    -- ** ClassicLoadBalancersConfig
    classicLoadBalancersConfig_classicLoadBalancers,

    -- ** ClientCertificateRevocationListStatus
    clientCertificateRevocationListStatus_message,
    clientCertificateRevocationListStatus_code,

    -- ** ClientConnectOptions
    clientConnectOptions_lambdaFunctionArn,
    clientConnectOptions_enabled,

    -- ** ClientConnectResponseOptions
    clientConnectResponseOptions_status,
    clientConnectResponseOptions_lambdaFunctionArn,
    clientConnectResponseOptions_enabled,

    -- ** ClientData
    clientData_uploadEnd,
    clientData_uploadStart,
    clientData_uploadSize,
    clientData_comment,

    -- ** ClientLoginBannerOptions
    clientLoginBannerOptions_bannerText,
    clientLoginBannerOptions_enabled,

    -- ** ClientLoginBannerResponseOptions
    clientLoginBannerResponseOptions_bannerText,
    clientLoginBannerResponseOptions_enabled,

    -- ** ClientVpnAuthentication
    clientVpnAuthentication_type,
    clientVpnAuthentication_federatedAuthentication,
    clientVpnAuthentication_mutualAuthentication,
    clientVpnAuthentication_activeDirectory,

    -- ** ClientVpnAuthenticationRequest
    clientVpnAuthenticationRequest_type,
    clientVpnAuthenticationRequest_federatedAuthentication,
    clientVpnAuthenticationRequest_mutualAuthentication,
    clientVpnAuthenticationRequest_activeDirectory,

    -- ** ClientVpnAuthorizationRuleStatus
    clientVpnAuthorizationRuleStatus_message,
    clientVpnAuthorizationRuleStatus_code,

    -- ** ClientVpnConnection
    clientVpnConnection_egressBytes,
    clientVpnConnection_username,
    clientVpnConnection_timestamp,
    clientVpnConnection_ingressBytes,
    clientVpnConnection_postureComplianceStatuses,
    clientVpnConnection_status,
    clientVpnConnection_connectionId,
    clientVpnConnection_egressPackets,
    clientVpnConnection_clientVpnEndpointId,
    clientVpnConnection_connectionEndTime,
    clientVpnConnection_ingressPackets,
    clientVpnConnection_commonName,
    clientVpnConnection_connectionEstablishedTime,
    clientVpnConnection_clientIp,

    -- ** ClientVpnConnectionStatus
    clientVpnConnectionStatus_message,
    clientVpnConnectionStatus_code,

    -- ** ClientVpnEndpoint
    clientVpnEndpoint_deletionTime,
    clientVpnEndpoint_tags,
    clientVpnEndpoint_authenticationOptions,
    clientVpnEndpoint_serverCertificateArn,
    clientVpnEndpoint_transportProtocol,
    clientVpnEndpoint_sessionTimeoutHours,
    clientVpnEndpoint_securityGroupIds,
    clientVpnEndpoint_dnsServers,
    clientVpnEndpoint_connectionLogOptions,
    clientVpnEndpoint_clientCidrBlock,
    clientVpnEndpoint_selfServicePortalUrl,
    clientVpnEndpoint_status,
    clientVpnEndpoint_description,
    clientVpnEndpoint_clientLoginBannerOptions,
    clientVpnEndpoint_clientVpnEndpointId,
    clientVpnEndpoint_clientConnectOptions,
    clientVpnEndpoint_splitTunnel,
    clientVpnEndpoint_associatedTargetNetworks,
    clientVpnEndpoint_creationTime,
    clientVpnEndpoint_vpcId,
    clientVpnEndpoint_dnsName,
    clientVpnEndpoint_vpnPort,
    clientVpnEndpoint_vpnProtocol,

    -- ** ClientVpnEndpointAttributeStatus
    clientVpnEndpointAttributeStatus_message,
    clientVpnEndpointAttributeStatus_code,

    -- ** ClientVpnEndpointStatus
    clientVpnEndpointStatus_message,
    clientVpnEndpointStatus_code,

    -- ** ClientVpnRoute
    clientVpnRoute_type,
    clientVpnRoute_destinationCidr,
    clientVpnRoute_status,
    clientVpnRoute_description,
    clientVpnRoute_clientVpnEndpointId,
    clientVpnRoute_targetSubnet,
    clientVpnRoute_origin,

    -- ** ClientVpnRouteStatus
    clientVpnRouteStatus_message,
    clientVpnRouteStatus_code,

    -- ** CloudWatchLogOptions
    cloudWatchLogOptions_logGroupArn,
    cloudWatchLogOptions_logEnabled,
    cloudWatchLogOptions_logOutputFormat,

    -- ** CloudWatchLogOptionsSpecification
    cloudWatchLogOptionsSpecification_logGroupArn,
    cloudWatchLogOptionsSpecification_logEnabled,
    cloudWatchLogOptionsSpecification_logOutputFormat,

    -- ** CoipAddressUsage
    coipAddressUsage_awsAccountId,
    coipAddressUsage_allocationId,
    coipAddressUsage_coIp,
    coipAddressUsage_awsService,

    -- ** CoipCidr
    coipCidr_localGatewayRouteTableId,
    coipCidr_cidr,
    coipCidr_coipPoolId,

    -- ** CoipPool
    coipPool_tags,
    coipPool_poolArn,
    coipPool_localGatewayRouteTableId,
    coipPool_poolCidrs,
    coipPool_poolId,

    -- ** ConnectionLogOptions
    connectionLogOptions_cloudwatchLogGroup,
    connectionLogOptions_enabled,
    connectionLogOptions_cloudwatchLogStream,

    -- ** ConnectionLogResponseOptions
    connectionLogResponseOptions_cloudwatchLogGroup,
    connectionLogResponseOptions_enabled,
    connectionLogResponseOptions_cloudwatchLogStream,

    -- ** ConnectionNotification
    connectionNotification_connectionNotificationState,
    connectionNotification_connectionNotificationId,
    connectionNotification_connectionEvents,
    connectionNotification_vpcEndpointId,
    connectionNotification_connectionNotificationType,
    connectionNotification_connectionNotificationArn,
    connectionNotification_serviceId,

    -- ** ConversionTask
    conversionTask_tags,
    conversionTask_importInstance,
    conversionTask_conversionTaskId,
    conversionTask_expirationTime,
    conversionTask_state,
    conversionTask_statusMessage,
    conversionTask_importVolume,

    -- ** CpuOptions
    cpuOptions_coreCount,
    cpuOptions_threadsPerCore,

    -- ** CpuOptionsRequest
    cpuOptionsRequest_coreCount,
    cpuOptionsRequest_threadsPerCore,

    -- ** CreateFleetError
    createFleetError_launchTemplateAndOverrides,
    createFleetError_lifecycle,
    createFleetError_errorMessage,
    createFleetError_errorCode,

    -- ** CreateFleetInstance
    createFleetInstance_launchTemplateAndOverrides,
    createFleetInstance_lifecycle,
    createFleetInstance_platform,
    createFleetInstance_instanceType,
    createFleetInstance_instanceIds,

    -- ** CreateTransitGatewayConnectRequestOptions
    createTransitGatewayConnectRequestOptions_protocol,

    -- ** CreateTransitGatewayMulticastDomainRequestOptions
    createTransitGatewayMulticastDomainRequestOptions_staticSourcesSupport,
    createTransitGatewayMulticastDomainRequestOptions_igmpv2Support,
    createTransitGatewayMulticastDomainRequestOptions_autoAcceptSharedAssociations,

    -- ** CreateTransitGatewayPeeringAttachmentRequestOptions
    createTransitGatewayPeeringAttachmentRequestOptions_dynamicRouting,

    -- ** CreateTransitGatewayVpcAttachmentRequestOptions
    createTransitGatewayVpcAttachmentRequestOptions_dnsSupport,
    createTransitGatewayVpcAttachmentRequestOptions_ipv6Support,
    createTransitGatewayVpcAttachmentRequestOptions_applianceModeSupport,

    -- ** CreateVolumePermission
    createVolumePermission_userId,
    createVolumePermission_group,

    -- ** CreateVolumePermissionModifications
    createVolumePermissionModifications_remove,
    createVolumePermissionModifications_add,

    -- ** CreditSpecification
    creditSpecification_cpuCredits,

    -- ** CreditSpecificationRequest
    creditSpecificationRequest_cpuCredits,

    -- ** CustomerGateway
    customerGateway_tags,
    customerGateway_deviceName,
    customerGateway_certificateArn,
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
    deleteLaunchTemplateVersionsResponseErrorItem_versionNumber,
    deleteLaunchTemplateVersionsResponseErrorItem_launchTemplateName,

    -- ** DeleteLaunchTemplateVersionsResponseSuccessItem
    deleteLaunchTemplateVersionsResponseSuccessItem_launchTemplateId,
    deleteLaunchTemplateVersionsResponseSuccessItem_versionNumber,
    deleteLaunchTemplateVersionsResponseSuccessItem_launchTemplateName,

    -- ** DeleteQueuedReservedInstancesError
    deleteQueuedReservedInstancesError_message,
    deleteQueuedReservedInstancesError_code,

    -- ** DeregisterInstanceTagAttributeRequest
    deregisterInstanceTagAttributeRequest_includeAllTagsOfInstance,
    deregisterInstanceTagAttributeRequest_instanceTagKeys,

    -- ** DescribeFastLaunchImagesSuccessItem
    describeFastLaunchImagesSuccessItem_resourceType,
    describeFastLaunchImagesSuccessItem_ownerId,
    describeFastLaunchImagesSuccessItem_launchTemplate,
    describeFastLaunchImagesSuccessItem_stateTransitionTime,
    describeFastLaunchImagesSuccessItem_state,
    describeFastLaunchImagesSuccessItem_stateTransitionReason,
    describeFastLaunchImagesSuccessItem_snapshotConfiguration,
    describeFastLaunchImagesSuccessItem_maxParallelLaunches,
    describeFastLaunchImagesSuccessItem_imageId,

    -- ** DescribeFastSnapshotRestoreSuccessItem
    describeFastSnapshotRestoreSuccessItem_ownerAlias,
    describeFastSnapshotRestoreSuccessItem_enabledTime,
    describeFastSnapshotRestoreSuccessItem_disabledTime,
    describeFastSnapshotRestoreSuccessItem_ownerId,
    describeFastSnapshotRestoreSuccessItem_enablingTime,
    describeFastSnapshotRestoreSuccessItem_snapshotId,
    describeFastSnapshotRestoreSuccessItem_state,
    describeFastSnapshotRestoreSuccessItem_availabilityZone,
    describeFastSnapshotRestoreSuccessItem_stateTransitionReason,
    describeFastSnapshotRestoreSuccessItem_optimizingTime,
    describeFastSnapshotRestoreSuccessItem_disablingTime,

    -- ** DescribeFleetError
    describeFleetError_launchTemplateAndOverrides,
    describeFleetError_lifecycle,
    describeFleetError_errorMessage,
    describeFleetError_errorCode,

    -- ** DescribeFleetsInstances
    describeFleetsInstances_launchTemplateAndOverrides,
    describeFleetsInstances_lifecycle,
    describeFleetsInstances_platform,
    describeFleetsInstances_instanceType,
    describeFleetsInstances_instanceIds,

    -- ** DestinationOptionsRequest
    destinationOptionsRequest_perHourPartition,
    destinationOptionsRequest_hiveCompatiblePartitions,
    destinationOptionsRequest_fileFormat,

    -- ** DestinationOptionsResponse
    destinationOptionsResponse_perHourPartition,
    destinationOptionsResponse_hiveCompatiblePartitions,
    destinationOptionsResponse_fileFormat,

    -- ** DhcpConfiguration
    dhcpConfiguration_key,
    dhcpConfiguration_values,

    -- ** DhcpOptions
    dhcpOptions_tags,
    dhcpOptions_ownerId,
    dhcpOptions_dhcpConfigurations,
    dhcpOptions_dhcpOptionsId,

    -- ** DirectoryServiceAuthentication
    directoryServiceAuthentication_directoryId,

    -- ** DirectoryServiceAuthenticationRequest
    directoryServiceAuthenticationRequest_directoryId,

    -- ** DisableFastSnapshotRestoreErrorItem
    disableFastSnapshotRestoreErrorItem_snapshotId,
    disableFastSnapshotRestoreErrorItem_fastSnapshotRestoreStateErrors,

    -- ** DisableFastSnapshotRestoreStateError
    disableFastSnapshotRestoreStateError_message,
    disableFastSnapshotRestoreStateError_code,

    -- ** DisableFastSnapshotRestoreStateErrorItem
    disableFastSnapshotRestoreStateErrorItem_availabilityZone,
    disableFastSnapshotRestoreStateErrorItem_error,

    -- ** DisableFastSnapshotRestoreSuccessItem
    disableFastSnapshotRestoreSuccessItem_ownerAlias,
    disableFastSnapshotRestoreSuccessItem_enabledTime,
    disableFastSnapshotRestoreSuccessItem_disabledTime,
    disableFastSnapshotRestoreSuccessItem_ownerId,
    disableFastSnapshotRestoreSuccessItem_enablingTime,
    disableFastSnapshotRestoreSuccessItem_snapshotId,
    disableFastSnapshotRestoreSuccessItem_state,
    disableFastSnapshotRestoreSuccessItem_availabilityZone,
    disableFastSnapshotRestoreSuccessItem_stateTransitionReason,
    disableFastSnapshotRestoreSuccessItem_optimizingTime,
    disableFastSnapshotRestoreSuccessItem_disablingTime,

    -- ** DiskImage
    diskImage_description,
    diskImage_volume,
    diskImage_image,

    -- ** DiskImageDescription
    diskImageDescription_importManifestUrl,
    diskImageDescription_format,
    diskImageDescription_size,
    diskImageDescription_checksum,

    -- ** DiskImageDetail
    diskImageDetail_bytes,
    diskImageDetail_format,
    diskImageDetail_importManifestUrl,

    -- ** DiskImageVolumeDescription
    diskImageVolumeDescription_size,
    diskImageVolumeDescription_id,

    -- ** DiskInfo
    diskInfo_type,
    diskInfo_sizeInGB,
    diskInfo_count,

    -- ** DnsEntry
    dnsEntry_hostedZoneId,
    dnsEntry_dnsName,

    -- ** DnsOptions
    dnsOptions_dnsRecordIpType,

    -- ** DnsOptionsSpecification
    dnsOptionsSpecification_dnsRecordIpType,

    -- ** DnsServersOptionsModifyStructure
    dnsServersOptionsModifyStructure_enabled,
    dnsServersOptionsModifyStructure_customDnsServers,

    -- ** EbsBlockDevice
    ebsBlockDevice_outpostArn,
    ebsBlockDevice_deleteOnTermination,
    ebsBlockDevice_snapshotId,
    ebsBlockDevice_volumeType,
    ebsBlockDevice_volumeSize,
    ebsBlockDevice_encrypted,
    ebsBlockDevice_kmsKeyId,
    ebsBlockDevice_throughput,
    ebsBlockDevice_iops,

    -- ** EbsInfo
    ebsInfo_ebsOptimizedInfo,
    ebsInfo_nvmeSupport,
    ebsInfo_ebsOptimizedSupport,
    ebsInfo_encryptionSupport,

    -- ** EbsInstanceBlockDevice
    ebsInstanceBlockDevice_deleteOnTermination,
    ebsInstanceBlockDevice_status,
    ebsInstanceBlockDevice_attachTime,
    ebsInstanceBlockDevice_volumeId,

    -- ** EbsInstanceBlockDeviceSpecification
    ebsInstanceBlockDeviceSpecification_deleteOnTermination,
    ebsInstanceBlockDeviceSpecification_volumeId,

    -- ** EbsOptimizedInfo
    ebsOptimizedInfo_maximumIops,
    ebsOptimizedInfo_baselineBandwidthInMbps,
    ebsOptimizedInfo_maximumThroughputInMBps,
    ebsOptimizedInfo_maximumBandwidthInMbps,
    ebsOptimizedInfo_baselineIops,
    ebsOptimizedInfo_baselineThroughputInMBps,

    -- ** EfaInfo
    efaInfo_maximumEfaInterfaces,

    -- ** EgressOnlyInternetGateway
    egressOnlyInternetGateway_tags,
    egressOnlyInternetGateway_egressOnlyInternetGatewayId,
    egressOnlyInternetGateway_attachments,

    -- ** ElasticGpuAssociation
    elasticGpuAssociation_elasticGpuAssociationState,
    elasticGpuAssociation_elasticGpuId,
    elasticGpuAssociation_elasticGpuAssociationTime,
    elasticGpuAssociation_elasticGpuAssociationId,

    -- ** ElasticGpuHealth
    elasticGpuHealth_status,

    -- ** ElasticGpuSpecification
    elasticGpuSpecification_type,

    -- ** ElasticGpuSpecificationResponse
    elasticGpuSpecificationResponse_type,

    -- ** ElasticGpus
    elasticGpus_tags,
    elasticGpus_elasticGpuHealth,
    elasticGpus_availabilityZone,
    elasticGpus_elasticGpuState,
    elasticGpus_instanceId,
    elasticGpus_elasticGpuId,
    elasticGpus_elasticGpuType,

    -- ** ElasticInferenceAccelerator
    elasticInferenceAccelerator_count,
    elasticInferenceAccelerator_type,

    -- ** ElasticInferenceAcceleratorAssociation
    elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorArn,
    elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorAssociationState,
    elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorAssociationId,
    elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorAssociationTime,

    -- ** EnableFastSnapshotRestoreErrorItem
    enableFastSnapshotRestoreErrorItem_snapshotId,
    enableFastSnapshotRestoreErrorItem_fastSnapshotRestoreStateErrors,

    -- ** EnableFastSnapshotRestoreStateError
    enableFastSnapshotRestoreStateError_message,
    enableFastSnapshotRestoreStateError_code,

    -- ** EnableFastSnapshotRestoreStateErrorItem
    enableFastSnapshotRestoreStateErrorItem_availabilityZone,
    enableFastSnapshotRestoreStateErrorItem_error,

    -- ** EnableFastSnapshotRestoreSuccessItem
    enableFastSnapshotRestoreSuccessItem_ownerAlias,
    enableFastSnapshotRestoreSuccessItem_enabledTime,
    enableFastSnapshotRestoreSuccessItem_disabledTime,
    enableFastSnapshotRestoreSuccessItem_ownerId,
    enableFastSnapshotRestoreSuccessItem_enablingTime,
    enableFastSnapshotRestoreSuccessItem_snapshotId,
    enableFastSnapshotRestoreSuccessItem_state,
    enableFastSnapshotRestoreSuccessItem_availabilityZone,
    enableFastSnapshotRestoreSuccessItem_stateTransitionReason,
    enableFastSnapshotRestoreSuccessItem_optimizingTime,
    enableFastSnapshotRestoreSuccessItem_disablingTime,

    -- ** EnclaveOptions
    enclaveOptions_enabled,

    -- ** EnclaveOptionsRequest
    enclaveOptionsRequest_enabled,

    -- ** EventInformation
    eventInformation_instanceId,
    eventInformation_eventSubType,
    eventInformation_eventDescription,

    -- ** Explanation
    explanation_destination,
    explanation_port,
    explanation_vpnConnection,
    explanation_loadBalancerListenerPort,
    explanation_transitGatewayAttachment,
    explanation_componentAccount,
    explanation_missingComponent,
    explanation_routeTableRoute,
    explanation_subnetRouteTable,
    explanation_vpc,
    explanation_natGateway,
    explanation_classicLoadBalancerListener,
    explanation_loadBalancerTargetGroup,
    explanation_securityGroup,
    explanation_acl,
    explanation_routeTable,
    explanation_internetGateway,
    explanation_attachedTo,
    explanation_availabilityZones,
    explanation_cidrs,
    explanation_state,
    explanation_loadBalancerTarget,
    explanation_customerGateway,
    explanation_vpnGateway,
    explanation_protocols,
    explanation_componentRegion,
    explanation_loadBalancerArn,
    explanation_sourceVpc,
    explanation_ingressRouteTable,
    explanation_aclRule,
    explanation_vpcPeeringConnection,
    explanation_address,
    explanation_loadBalancerTargetPort,
    explanation_securityGroups,
    explanation_transitGateway,
    explanation_destinationVpc,
    explanation_addresses,
    explanation_prefixList,
    explanation_elasticLoadBalancerListener,
    explanation_subnet,
    explanation_packetField,
    explanation_explanationCode,
    explanation_networkInterface,
    explanation_direction,
    explanation_transitGatewayRouteTableRoute,
    explanation_loadBalancerTargetGroups,
    explanation_portRanges,
    explanation_vpcEndpoint,
    explanation_transitGatewayRouteTable,
    explanation_component,
    explanation_securityGroupRule,

    -- ** ExportImageTask
    exportImageTask_tags,
    exportImageTask_progress,
    exportImageTask_exportImageTaskId,
    exportImageTask_s3ExportLocation,
    exportImageTask_status,
    exportImageTask_description,
    exportImageTask_statusMessage,
    exportImageTask_imageId,

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
    exportToS3Task_s3Bucket,
    exportToS3Task_s3Key,
    exportToS3Task_diskImageFormat,
    exportToS3Task_containerFormat,

    -- ** ExportToS3TaskSpecification
    exportToS3TaskSpecification_s3Bucket,
    exportToS3TaskSpecification_diskImageFormat,
    exportToS3TaskSpecification_containerFormat,
    exportToS3TaskSpecification_s3Prefix,

    -- ** FailedCapacityReservationFleetCancellationResult
    failedCapacityReservationFleetCancellationResult_cancelCapacityReservationFleetError,
    failedCapacityReservationFleetCancellationResult_capacityReservationFleetId,

    -- ** FailedQueuedPurchaseDeletion
    failedQueuedPurchaseDeletion_reservedInstancesId,
    failedQueuedPurchaseDeletion_error,

    -- ** FastLaunchLaunchTemplateSpecificationRequest
    fastLaunchLaunchTemplateSpecificationRequest_launchTemplateId,
    fastLaunchLaunchTemplateSpecificationRequest_launchTemplateName,
    fastLaunchLaunchTemplateSpecificationRequest_version,

    -- ** FastLaunchLaunchTemplateSpecificationResponse
    fastLaunchLaunchTemplateSpecificationResponse_launchTemplateId,
    fastLaunchLaunchTemplateSpecificationResponse_version,
    fastLaunchLaunchTemplateSpecificationResponse_launchTemplateName,

    -- ** FastLaunchSnapshotConfigurationRequest
    fastLaunchSnapshotConfigurationRequest_targetResourceCount,

    -- ** FastLaunchSnapshotConfigurationResponse
    fastLaunchSnapshotConfigurationResponse_targetResourceCount,

    -- ** FederatedAuthentication
    federatedAuthentication_selfServiceSamlProviderArn,
    federatedAuthentication_samlProviderArn,

    -- ** FederatedAuthenticationRequest
    federatedAuthenticationRequest_selfServiceSAMLProviderArn,
    federatedAuthenticationRequest_sAMLProviderArn,

    -- ** Filter
    filter_values,
    filter_name,

    -- ** FleetCapacityReservation
    fleetCapacityReservation_ebsOptimized,
    fleetCapacityReservation_totalInstanceCount,
    fleetCapacityReservation_capacityReservationId,
    fleetCapacityReservation_fulfilledCapacity,
    fleetCapacityReservation_availabilityZone,
    fleetCapacityReservation_instanceType,
    fleetCapacityReservation_instancePlatform,
    fleetCapacityReservation_priority,
    fleetCapacityReservation_createDate,
    fleetCapacityReservation_weight,
    fleetCapacityReservation_availabilityZoneId,

    -- ** FleetData
    fleetData_tags,
    fleetData_instances,
    fleetData_excessCapacityTerminationPolicy,
    fleetData_fleetId,
    fleetData_clientToken,
    fleetData_type,
    fleetData_fulfilledOnDemandCapacity,
    fleetData_targetCapacitySpecification,
    fleetData_activityStatus,
    fleetData_onDemandOptions,
    fleetData_context,
    fleetData_fulfilledCapacity,
    fleetData_validFrom,
    fleetData_replaceUnhealthyInstances,
    fleetData_launchTemplateConfigs,
    fleetData_errors,
    fleetData_fleetState,
    fleetData_spotOptions,
    fleetData_terminateInstancesWithExpiration,
    fleetData_validUntil,
    fleetData_createTime,

    -- ** FleetLaunchTemplateConfig
    fleetLaunchTemplateConfig_launchTemplateSpecification,
    fleetLaunchTemplateConfig_overrides,

    -- ** FleetLaunchTemplateConfigRequest
    fleetLaunchTemplateConfigRequest_launchTemplateSpecification,
    fleetLaunchTemplateConfigRequest_overrides,

    -- ** FleetLaunchTemplateOverrides
    fleetLaunchTemplateOverrides_placement,
    fleetLaunchTemplateOverrides_instanceRequirements,
    fleetLaunchTemplateOverrides_subnetId,
    fleetLaunchTemplateOverrides_maxPrice,
    fleetLaunchTemplateOverrides_availabilityZone,
    fleetLaunchTemplateOverrides_instanceType,
    fleetLaunchTemplateOverrides_priority,
    fleetLaunchTemplateOverrides_weightedCapacity,
    fleetLaunchTemplateOverrides_imageId,

    -- ** FleetLaunchTemplateOverridesRequest
    fleetLaunchTemplateOverridesRequest_placement,
    fleetLaunchTemplateOverridesRequest_instanceRequirements,
    fleetLaunchTemplateOverridesRequest_subnetId,
    fleetLaunchTemplateOverridesRequest_maxPrice,
    fleetLaunchTemplateOverridesRequest_availabilityZone,
    fleetLaunchTemplateOverridesRequest_instanceType,
    fleetLaunchTemplateOverridesRequest_priority,
    fleetLaunchTemplateOverridesRequest_weightedCapacity,
    fleetLaunchTemplateOverridesRequest_imageId,

    -- ** FleetLaunchTemplateSpecification
    fleetLaunchTemplateSpecification_launchTemplateId,
    fleetLaunchTemplateSpecification_version,
    fleetLaunchTemplateSpecification_launchTemplateName,

    -- ** FleetLaunchTemplateSpecificationRequest
    fleetLaunchTemplateSpecificationRequest_launchTemplateId,
    fleetLaunchTemplateSpecificationRequest_version,
    fleetLaunchTemplateSpecificationRequest_launchTemplateName,

    -- ** FleetSpotCapacityRebalance
    fleetSpotCapacityRebalance_terminationDelay,
    fleetSpotCapacityRebalance_replacementStrategy,

    -- ** FleetSpotCapacityRebalanceRequest
    fleetSpotCapacityRebalanceRequest_terminationDelay,
    fleetSpotCapacityRebalanceRequest_replacementStrategy,

    -- ** FleetSpotMaintenanceStrategies
    fleetSpotMaintenanceStrategies_capacityRebalance,

    -- ** FleetSpotMaintenanceStrategiesRequest
    fleetSpotMaintenanceStrategiesRequest_capacityRebalance,

    -- ** FlowLog
    flowLog_resourceId,
    flowLog_tags,
    flowLog_destinationOptions,
    flowLog_flowLogId,
    flowLog_trafficType,
    flowLog_flowLogStatus,
    flowLog_deliverLogsPermissionArn,
    flowLog_deliverLogsErrorMessage,
    flowLog_logFormat,
    flowLog_logDestination,
    flowLog_deliverCrossAccountRole,
    flowLog_logDestinationType,
    flowLog_creationTime,
    flowLog_maxAggregationInterval,
    flowLog_deliverLogsStatus,
    flowLog_logGroupName,

    -- ** FpgaDeviceInfo
    fpgaDeviceInfo_memoryInfo,
    fpgaDeviceInfo_name,
    fpgaDeviceInfo_count,
    fpgaDeviceInfo_manufacturer,

    -- ** FpgaDeviceMemoryInfo
    fpgaDeviceMemoryInfo_sizeInMiB,

    -- ** FpgaImage
    fpgaImage_tags,
    fpgaImage_ownerAlias,
    fpgaImage_name,
    fpgaImage_ownerId,
    fpgaImage_shellVersion,
    fpgaImage_fpgaImageGlobalId,
    fpgaImage_state,
    fpgaImage_productCodes,
    fpgaImage_description,
    fpgaImage_fpgaImageId,
    fpgaImage_dataRetentionSupport,
    fpgaImage_updateTime,
    fpgaImage_createTime,
    fpgaImage_pciId,
    fpgaImage_public,

    -- ** FpgaImageAttribute
    fpgaImageAttribute_name,
    fpgaImageAttribute_loadPermissions,
    fpgaImageAttribute_productCodes,
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
    gpuDeviceInfo_name,
    gpuDeviceInfo_count,
    gpuDeviceInfo_manufacturer,

    -- ** GpuDeviceMemoryInfo
    gpuDeviceMemoryInfo_sizeInMiB,

    -- ** GpuInfo
    gpuInfo_totalGpuMemoryInMiB,
    gpuInfo_gpus,

    -- ** GroupIdentifier
    groupIdentifier_groupName,
    groupIdentifier_groupId,

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
    host_tags,
    host_instances,
    host_autoPlacement,
    host_clientToken,
    host_outpostArn,
    host_ownerId,
    host_availableCapacity,
    host_hostRecovery,
    host_hostId,
    host_allocationTime,
    host_state,
    host_availabilityZone,
    host_hostProperties,
    host_releaseTime,
    host_allowsMultipleInstanceTypes,
    host_memberOfServiceLinkedResourceGroup,
    host_hostReservationId,
    host_availabilityZoneId,

    -- ** HostInstance
    hostInstance_ownerId,
    hostInstance_instanceType,
    hostInstance_instanceId,

    -- ** HostOffering
    hostOffering_hourlyPrice,
    hostOffering_upfrontPrice,
    hostOffering_duration,
    hostOffering_currencyCode,
    hostOffering_instanceFamily,
    hostOffering_offeringId,
    hostOffering_paymentOption,

    -- ** HostProperties
    hostProperties_cores,
    hostProperties_sockets,
    hostProperties_instanceType,
    hostProperties_totalVCpus,
    hostProperties_instanceFamily,

    -- ** HostReservation
    hostReservation_tags,
    hostReservation_hourlyPrice,
    hostReservation_start,
    hostReservation_state,
    hostReservation_count,
    hostReservation_upfrontPrice,
    hostReservation_duration,
    hostReservation_currencyCode,
    hostReservation_end,
    hostReservation_instanceFamily,
    hostReservation_hostReservationId,
    hostReservation_offeringId,
    hostReservation_hostIdSet,
    hostReservation_paymentOption,

    -- ** IKEVersionsListValue
    iKEVersionsListValue_value,

    -- ** IKEVersionsRequestListValue
    iKEVersionsRequestListValue_value,

    -- ** IamInstanceProfile
    iamInstanceProfile_arn,
    iamInstanceProfile_id,

    -- ** IamInstanceProfileAssociation
    iamInstanceProfileAssociation_iamInstanceProfile,
    iamInstanceProfileAssociation_state,
    iamInstanceProfileAssociation_timestamp,
    iamInstanceProfileAssociation_instanceId,
    iamInstanceProfileAssociation_associationId,

    -- ** IamInstanceProfileSpecification
    iamInstanceProfileSpecification_name,
    iamInstanceProfileSpecification_arn,

    -- ** IcmpTypeCode
    icmpTypeCode_type,
    icmpTypeCode_code,

    -- ** IdFormat
    idFormat_useLongIds,
    idFormat_deadline,
    idFormat_resource,

    -- ** Image
    image_tags,
    image_name,
    image_sriovNetSupport,
    image_blockDeviceMappings,
    image_creationDate,
    image_productCodes,
    image_platform,
    image_description,
    image_imageOwnerAlias,
    image_usageOperation,
    image_tpmSupport,
    image_ramdiskId,
    image_deprecationTime,
    image_platformDetails,
    image_imdsSupport,
    image_bootMode,
    image_kernelId,
    image_stateReason,
    image_enaSupport,
    image_rootDeviceName,
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
    imageDiskContainer_url,
    imageDiskContainer_description,

    -- ** ImageRecycleBinInfo
    imageRecycleBinInfo_recycleBinEnterTime,
    imageRecycleBinInfo_name,
    imageRecycleBinInfo_description,
    imageRecycleBinInfo_recycleBinExitTime,
    imageRecycleBinInfo_imageId,

    -- ** ImportImageLicenseConfigurationRequest
    importImageLicenseConfigurationRequest_licenseConfigurationArn,

    -- ** ImportImageLicenseConfigurationResponse
    importImageLicenseConfigurationResponse_licenseConfigurationArn,

    -- ** ImportImageTask
    importImageTask_tags,
    importImageTask_hypervisor,
    importImageTask_progress,
    importImageTask_licenseType,
    importImageTask_importTaskId,
    importImageTask_status,
    importImageTask_platform,
    importImageTask_description,
    importImageTask_usageOperation,
    importImageTask_encrypted,
    importImageTask_kmsKeyId,
    importImageTask_bootMode,
    importImageTask_licenseSpecifications,
    importImageTask_snapshotDetails,
    importImageTask_statusMessage,
    importImageTask_architecture,
    importImageTask_imageId,

    -- ** ImportInstanceLaunchSpecification
    importInstanceLaunchSpecification_placement,
    importInstanceLaunchSpecification_userData,
    importInstanceLaunchSpecification_additionalInfo,
    importInstanceLaunchSpecification_instanceInitiatedShutdownBehavior,
    importInstanceLaunchSpecification_monitoring,
    importInstanceLaunchSpecification_subnetId,
    importInstanceLaunchSpecification_instanceType,
    importInstanceLaunchSpecification_groupIds,
    importInstanceLaunchSpecification_groupNames,
    importInstanceLaunchSpecification_privateIpAddress,
    importInstanceLaunchSpecification_architecture,

    -- ** ImportInstanceTaskDetails
    importInstanceTaskDetails_platform,
    importInstanceTaskDetails_description,
    importInstanceTaskDetails_volumes,
    importInstanceTaskDetails_instanceId,

    -- ** ImportInstanceVolumeDetailItem
    importInstanceVolumeDetailItem_bytesConverted,
    importInstanceVolumeDetailItem_status,
    importInstanceVolumeDetailItem_availabilityZone,
    importInstanceVolumeDetailItem_description,
    importInstanceVolumeDetailItem_volume,
    importInstanceVolumeDetailItem_image,
    importInstanceVolumeDetailItem_statusMessage,

    -- ** ImportSnapshotTask
    importSnapshotTask_tags,
    importSnapshotTask_importTaskId,
    importSnapshotTask_description,
    importSnapshotTask_snapshotTaskDetail,

    -- ** ImportVolumeTaskDetails
    importVolumeTaskDetails_bytesConverted,
    importVolumeTaskDetails_availabilityZone,
    importVolumeTaskDetails_description,
    importVolumeTaskDetails_volume,
    importVolumeTaskDetails_image,

    -- ** InferenceAcceleratorInfo
    inferenceAcceleratorInfo_accelerators,

    -- ** InferenceDeviceInfo
    inferenceDeviceInfo_name,
    inferenceDeviceInfo_count,
    inferenceDeviceInfo_manufacturer,

    -- ** Instance
    instance_ebsOptimized,
    instance_tags,
    instance_hibernationOptions,
    instance_iamInstanceProfile,
    instance_spotInstanceRequestId,
    instance_clientToken,
    instance_sriovNetSupport,
    instance_outpostArn,
    instance_blockDeviceMappings,
    instance_sourceDestCheck,
    instance_subnetId,
    instance_elasticGpuAssociations,
    instance_usageOperationUpdateTime,
    instance_productCodes,
    instance_capacityReservationSpecification,
    instance_capacityReservationId,
    instance_platform,
    instance_stateTransitionReason,
    instance_instanceLifecycle,
    instance_ipv6Address,
    instance_elasticInferenceAcceleratorAssociations,
    instance_usageOperation,
    instance_publicIpAddress,
    instance_publicDnsName,
    instance_securityGroups,
    instance_tpmSupport,
    instance_ramdiskId,
    instance_privateIpAddress,
    instance_maintenanceOptions,
    instance_privateDnsNameOptions,
    instance_platformDetails,
    instance_bootMode,
    instance_keyName,
    instance_privateDnsName,
    instance_kernelId,
    instance_vpcId,
    instance_cpuOptions,
    instance_stateReason,
    instance_enaSupport,
    instance_rootDeviceName,
    instance_networkInterfaces,
    instance_enclaveOptions,
    instance_licenses,
    instance_metadataOptions,
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
    instanceBlockDeviceMappingSpecification_deviceName,
    instanceBlockDeviceMappingSpecification_noDevice,
    instanceBlockDeviceMappingSpecification_virtualName,

    -- ** InstanceCapacity
    instanceCapacity_totalCapacity,
    instanceCapacity_availableCapacity,
    instanceCapacity_instanceType,

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
    instanceEventWindow_tags,
    instanceEventWindow_name,
    instanceEventWindow_state,
    instanceEventWindow_associationTarget,
    instanceEventWindow_timeRanges,
    instanceEventWindow_cronExpression,
    instanceEventWindow_instanceEventWindowId,

    -- ** InstanceEventWindowAssociationRequest
    instanceEventWindowAssociationRequest_instanceTags,
    instanceEventWindowAssociationRequest_dedicatedHostIds,
    instanceEventWindowAssociationRequest_instanceIds,

    -- ** InstanceEventWindowAssociationTarget
    instanceEventWindowAssociationTarget_tags,
    instanceEventWindowAssociationTarget_dedicatedHostIds,
    instanceEventWindowAssociationTarget_instanceIds,

    -- ** InstanceEventWindowDisassociationRequest
    instanceEventWindowDisassociationRequest_instanceTags,
    instanceEventWindowDisassociationRequest_dedicatedHostIds,
    instanceEventWindowDisassociationRequest_instanceIds,

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

    -- ** InstanceMaintenanceOptions
    instanceMaintenanceOptions_autoRecovery,

    -- ** InstanceMaintenanceOptionsRequest
    instanceMaintenanceOptionsRequest_autoRecovery,

    -- ** InstanceMarketOptionsRequest
    instanceMarketOptionsRequest_marketType,
    instanceMarketOptionsRequest_spotOptions,

    -- ** InstanceMetadataOptionsRequest
    instanceMetadataOptionsRequest_httpPutResponseHopLimit,
    instanceMetadataOptionsRequest_httpTokens,
    instanceMetadataOptionsRequest_httpEndpoint,
    instanceMetadataOptionsRequest_instanceMetadataTags,
    instanceMetadataOptionsRequest_httpProtocolIpv6,

    -- ** InstanceMetadataOptionsResponse
    instanceMetadataOptionsResponse_httpPutResponseHopLimit,
    instanceMetadataOptionsResponse_state,
    instanceMetadataOptionsResponse_httpTokens,
    instanceMetadataOptionsResponse_httpEndpoint,
    instanceMetadataOptionsResponse_instanceMetadataTags,
    instanceMetadataOptionsResponse_httpProtocolIpv6,

    -- ** InstanceMonitoring
    instanceMonitoring_monitoring,
    instanceMonitoring_instanceId,

    -- ** InstanceNetworkInterface
    instanceNetworkInterface_interfaceType,
    instanceNetworkInterface_attachment,
    instanceNetworkInterface_ownerId,
    instanceNetworkInterface_sourceDestCheck,
    instanceNetworkInterface_privateIpAddresses,
    instanceNetworkInterface_subnetId,
    instanceNetworkInterface_status,
    instanceNetworkInterface_description,
    instanceNetworkInterface_association,
    instanceNetworkInterface_macAddress,
    instanceNetworkInterface_networkInterfaceId,
    instanceNetworkInterface_ipv4Prefixes,
    instanceNetworkInterface_privateIpAddress,
    instanceNetworkInterface_privateDnsName,
    instanceNetworkInterface_ipv6Prefixes,
    instanceNetworkInterface_vpcId,
    instanceNetworkInterface_groups,
    instanceNetworkInterface_ipv6Addresses,

    -- ** InstanceNetworkInterfaceAssociation
    instanceNetworkInterfaceAssociation_ipOwnerId,
    instanceNetworkInterfaceAssociation_carrierIp,
    instanceNetworkInterfaceAssociation_publicIp,
    instanceNetworkInterfaceAssociation_publicDnsName,
    instanceNetworkInterfaceAssociation_customerOwnedIp,

    -- ** InstanceNetworkInterfaceAttachment
    instanceNetworkInterfaceAttachment_networkCardIndex,
    instanceNetworkInterfaceAttachment_deleteOnTermination,
    instanceNetworkInterfaceAttachment_status,
    instanceNetworkInterfaceAttachment_attachmentId,
    instanceNetworkInterfaceAttachment_attachTime,
    instanceNetworkInterfaceAttachment_deviceIndex,

    -- ** InstanceNetworkInterfaceSpecification
    instanceNetworkInterfaceSpecification_ipv4PrefixCount,
    instanceNetworkInterfaceSpecification_interfaceType,
    instanceNetworkInterfaceSpecification_networkCardIndex,
    instanceNetworkInterfaceSpecification_associatePublicIpAddress,
    instanceNetworkInterfaceSpecification_deleteOnTermination,
    instanceNetworkInterfaceSpecification_privateIpAddresses,
    instanceNetworkInterfaceSpecification_subnetId,
    instanceNetworkInterfaceSpecification_description,
    instanceNetworkInterfaceSpecification_associateCarrierIpAddress,
    instanceNetworkInterfaceSpecification_networkInterfaceId,
    instanceNetworkInterfaceSpecification_ipv6AddressCount,
    instanceNetworkInterfaceSpecification_ipv4Prefixes,
    instanceNetworkInterfaceSpecification_privateIpAddress,
    instanceNetworkInterfaceSpecification_ipv6PrefixCount,
    instanceNetworkInterfaceSpecification_secondaryPrivateIpAddressCount,
    instanceNetworkInterfaceSpecification_ipv6Prefixes,
    instanceNetworkInterfaceSpecification_groups,
    instanceNetworkInterfaceSpecification_ipv6Addresses,
    instanceNetworkInterfaceSpecification_deviceIndex,

    -- ** InstancePrivateIpAddress
    instancePrivateIpAddress_association,
    instancePrivateIpAddress_primary,
    instancePrivateIpAddress_privateIpAddress,
    instancePrivateIpAddress_privateDnsName,

    -- ** InstanceRequirements
    instanceRequirements_instanceGenerations,
    instanceRequirements_baselineEbsBandwidthMbps,
    instanceRequirements_bareMetal,
    instanceRequirements_spotMaxPricePercentageOverLowestPrice,
    instanceRequirements_acceleratorTypes,
    instanceRequirements_totalLocalStorageGB,
    instanceRequirements_localStorageTypes,
    instanceRequirements_onDemandMaxPricePercentageOverLowestPrice,
    instanceRequirements_allowedInstanceTypes,
    instanceRequirements_acceleratorNames,
    instanceRequirements_networkBandwidthGbps,
    instanceRequirements_acceleratorManufacturers,
    instanceRequirements_excludedInstanceTypes,
    instanceRequirements_networkInterfaceCount,
    instanceRequirements_requireHibernateSupport,
    instanceRequirements_acceleratorTotalMemoryMiB,
    instanceRequirements_vCpuCount,
    instanceRequirements_acceleratorCount,
    instanceRequirements_burstablePerformance,
    instanceRequirements_cpuManufacturers,
    instanceRequirements_memoryGiBPerVCpu,
    instanceRequirements_localStorage,
    instanceRequirements_memoryMiB,

    -- ** InstanceRequirementsRequest
    instanceRequirementsRequest_instanceGenerations,
    instanceRequirementsRequest_baselineEbsBandwidthMbps,
    instanceRequirementsRequest_bareMetal,
    instanceRequirementsRequest_spotMaxPricePercentageOverLowestPrice,
    instanceRequirementsRequest_acceleratorTypes,
    instanceRequirementsRequest_totalLocalStorageGB,
    instanceRequirementsRequest_localStorageTypes,
    instanceRequirementsRequest_onDemandMaxPricePercentageOverLowestPrice,
    instanceRequirementsRequest_allowedInstanceTypes,
    instanceRequirementsRequest_acceleratorNames,
    instanceRequirementsRequest_networkBandwidthGbps,
    instanceRequirementsRequest_acceleratorManufacturers,
    instanceRequirementsRequest_excludedInstanceTypes,
    instanceRequirementsRequest_networkInterfaceCount,
    instanceRequirementsRequest_requireHibernateSupport,
    instanceRequirementsRequest_acceleratorTotalMemoryMiB,
    instanceRequirementsRequest_acceleratorCount,
    instanceRequirementsRequest_burstablePerformance,
    instanceRequirementsRequest_cpuManufacturers,
    instanceRequirementsRequest_memoryGiBPerVCpu,
    instanceRequirementsRequest_localStorage,
    instanceRequirementsRequest_vCpuCount,
    instanceRequirementsRequest_memoryMiB,

    -- ** InstanceRequirementsWithMetadataRequest
    instanceRequirementsWithMetadataRequest_instanceRequirements,
    instanceRequirementsWithMetadataRequest_architectureTypes,
    instanceRequirementsWithMetadataRequest_virtualizationTypes,

    -- ** InstanceSpecification
    instanceSpecification_excludeDataVolumeIds,
    instanceSpecification_excludeBootVolume,
    instanceSpecification_instanceId,

    -- ** InstanceState
    instanceState_name,
    instanceState_code,

    -- ** InstanceStateChange
    instanceStateChange_previousState,
    instanceStateChange_instanceId,
    instanceStateChange_currentState,

    -- ** InstanceStatus
    instanceStatus_instanceState,
    instanceStatus_outpostArn,
    instanceStatus_instanceStatus,
    instanceStatus_availabilityZone,
    instanceStatus_systemStatus,
    instanceStatus_instanceId,
    instanceStatus_events,

    -- ** InstanceStatusDetails
    instanceStatusDetails_impairedSince,
    instanceStatusDetails_name,
    instanceStatusDetails_status,

    -- ** InstanceStatusEvent
    instanceStatusEvent_code,
    instanceStatusEvent_instanceEventId,
    instanceStatusEvent_description,
    instanceStatusEvent_notBefore,
    instanceStatusEvent_notBeforeDeadline,
    instanceStatusEvent_notAfter,

    -- ** InstanceStatusSummary
    instanceStatusSummary_details,
    instanceStatusSummary_status,

    -- ** InstanceStorageInfo
    instanceStorageInfo_totalSizeInGB,
    instanceStorageInfo_disks,
    instanceStorageInfo_nvmeSupport,
    instanceStorageInfo_encryptionSupport,

    -- ** InstanceTagNotificationAttribute
    instanceTagNotificationAttribute_includeAllTagsOfInstance,
    instanceTagNotificationAttribute_instanceTagKeys,

    -- ** InstanceTypeInfo
    instanceTypeInfo_memoryInfo,
    instanceTypeInfo_hibernationSupported,
    instanceTypeInfo_hypervisor,
    instanceTypeInfo_vCpuInfo,
    instanceTypeInfo_supportedRootDeviceTypes,
    instanceTypeInfo_bareMetal,
    instanceTypeInfo_burstablePerformanceSupported,
    instanceTypeInfo_instanceStorageSupported,
    instanceTypeInfo_processorInfo,
    instanceTypeInfo_dedicatedHostsSupported,
    instanceTypeInfo_inferenceAcceleratorInfo,
    instanceTypeInfo_placementGroupInfo,
    instanceTypeInfo_ebsInfo,
    instanceTypeInfo_supportedUsageClasses,
    instanceTypeInfo_instanceType,
    instanceTypeInfo_freeTierEligible,
    instanceTypeInfo_autoRecoverySupported,
    instanceTypeInfo_gpuInfo,
    instanceTypeInfo_instanceStorageInfo,
    instanceTypeInfo_currentGeneration,
    instanceTypeInfo_fpgaInfo,
    instanceTypeInfo_supportedVirtualizationTypes,
    instanceTypeInfo_networkInfo,
    instanceTypeInfo_supportedBootModes,

    -- ** InstanceTypeInfoFromInstanceRequirements
    instanceTypeInfoFromInstanceRequirements_instanceType,

    -- ** InstanceTypeOffering
    instanceTypeOffering_location,
    instanceTypeOffering_instanceType,
    instanceTypeOffering_locationType,

    -- ** InstanceUsage
    instanceUsage_usedInstanceCount,
    instanceUsage_accountId,

    -- ** IntegrateServices
    integrateServices_athenaIntegrations,

    -- ** InternetGateway
    internetGateway_tags,
    internetGateway_ownerId,
    internetGateway_attachments,
    internetGateway_internetGatewayId,

    -- ** InternetGatewayAttachment
    internetGatewayAttachment_state,
    internetGatewayAttachment_vpcId,

    -- ** IpPermission
    ipPermission_toPort,
    ipPermission_ipv6Ranges,
    ipPermission_prefixListIds,
    ipPermission_ipRanges,
    ipPermission_userIdGroupPairs,
    ipPermission_fromPort,
    ipPermission_ipProtocol,

    -- ** IpRange
    ipRange_description,
    ipRange_cidrIp,

    -- ** Ipam
    ipam_tags,
    ipam_ownerId,
    ipam_ipamArn,
    ipam_scopeCount,
    ipam_state,
    ipam_description,
    ipam_operatingRegions,
    ipam_privateDefaultScopeId,
    ipam_ipamId,
    ipam_publicDefaultScopeId,
    ipam_ipamRegion,

    -- ** IpamAddressHistoryRecord
    ipamAddressHistoryRecord_resourceId,
    ipamAddressHistoryRecord_resourceType,
    ipamAddressHistoryRecord_resourceOverlapStatus,
    ipamAddressHistoryRecord_resourceOwnerId,
    ipamAddressHistoryRecord_resourceName,
    ipamAddressHistoryRecord_resourceCidr,
    ipamAddressHistoryRecord_sampledEndTime,
    ipamAddressHistoryRecord_resourceRegion,
    ipamAddressHistoryRecord_resourceComplianceStatus,
    ipamAddressHistoryRecord_vpcId,
    ipamAddressHistoryRecord_sampledStartTime,

    -- ** IpamCidrAuthorizationContext
    ipamCidrAuthorizationContext_message,
    ipamCidrAuthorizationContext_signature,

    -- ** IpamOperatingRegion
    ipamOperatingRegion_regionName,

    -- ** IpamPool
    ipamPool_tags,
    ipamPool_ipamScopeArn,
    ipamPool_addressFamily,
    ipamPool_ipamScopeType,
    ipamPool_ownerId,
    ipamPool_allocationMaxNetmaskLength,
    ipamPool_publiclyAdvertisable,
    ipamPool_ipamArn,
    ipamPool_locale,
    ipamPool_state,
    ipamPool_sourceIpamPoolId,
    ipamPool_description,
    ipamPool_allocationResourceTags,
    ipamPool_allocationMinNetmaskLength,
    ipamPool_stateMessage,
    ipamPool_ipamPoolArn,
    ipamPool_poolDepth,
    ipamPool_ipamPoolId,
    ipamPool_allocationDefaultNetmaskLength,
    ipamPool_awsService,
    ipamPool_ipamRegion,
    ipamPool_autoImport,

    -- ** IpamPoolAllocation
    ipamPoolAllocation_resourceId,
    ipamPoolAllocation_resourceType,
    ipamPoolAllocation_cidr,
    ipamPoolAllocation_description,
    ipamPoolAllocation_resourceOwner,
    ipamPoolAllocation_resourceRegion,
    ipamPoolAllocation_ipamPoolAllocationId,

    -- ** IpamPoolCidr
    ipamPoolCidr_cidr,
    ipamPoolCidr_state,
    ipamPoolCidr_failureReason,

    -- ** IpamPoolCidrFailureReason
    ipamPoolCidrFailureReason_message,
    ipamPoolCidrFailureReason_code,

    -- ** IpamResourceCidr
    ipamResourceCidr_resourceId,
    ipamResourceCidr_resourceType,
    ipamResourceCidr_ipamScopeId,
    ipamResourceCidr_resourceOwnerId,
    ipamResourceCidr_overlapStatus,
    ipamResourceCidr_resourceName,
    ipamResourceCidr_resourceCidr,
    ipamResourceCidr_complianceStatus,
    ipamResourceCidr_resourceTags,
    ipamResourceCidr_resourceRegion,
    ipamResourceCidr_ipamPoolId,
    ipamResourceCidr_ipamId,
    ipamResourceCidr_vpcId,
    ipamResourceCidr_managementState,
    ipamResourceCidr_ipUsage,

    -- ** IpamResourceTag
    ipamResourceTag_key,
    ipamResourceTag_value,

    -- ** IpamScope
    ipamScope_tags,
    ipamScope_ipamScopeArn,
    ipamScope_ipamScopeId,
    ipamScope_ipamScopeType,
    ipamScope_ownerId,
    ipamScope_poolCount,
    ipamScope_ipamArn,
    ipamScope_state,
    ipamScope_description,
    ipamScope_isDefault,
    ipamScope_ipamRegion,

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
    ipv6Pool_tags,
    ipv6Pool_poolCidrBlocks,
    ipv6Pool_description,
    ipv6Pool_poolId,

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
    keyPairInfo_tags,
    keyPairInfo_publicKey,
    keyPairInfo_keyFingerprint,
    keyPairInfo_keyType,
    keyPairInfo_keyName,
    keyPairInfo_createTime,
    keyPairInfo_keyPairId,

    -- ** LastError
    lastError_message,
    lastError_code,

    -- ** LaunchPermission
    launchPermission_organizationArn,
    launchPermission_userId,
    launchPermission_organizationalUnitArn,
    launchPermission_group,

    -- ** LaunchPermissionModifications
    launchPermissionModifications_remove,
    launchPermissionModifications_add,

    -- ** LaunchSpecification
    launchSpecification_ebsOptimized,
    launchSpecification_iamInstanceProfile,
    launchSpecification_placement,
    launchSpecification_userData,
    launchSpecification_blockDeviceMappings,
    launchSpecification_addressingType,
    launchSpecification_monitoring,
    launchSpecification_subnetId,
    launchSpecification_instanceType,
    launchSpecification_securityGroups,
    launchSpecification_ramdiskId,
    launchSpecification_keyName,
    launchSpecification_kernelId,
    launchSpecification_imageId,
    launchSpecification_networkInterfaces,

    -- ** LaunchTemplate
    launchTemplate_tags,
    launchTemplate_latestVersionNumber,
    launchTemplate_launchTemplateId,
    launchTemplate_defaultVersionNumber,
    launchTemplate_createTime,
    launchTemplate_createdBy,
    launchTemplate_launchTemplateName,

    -- ** LaunchTemplateAndOverridesResponse
    launchTemplateAndOverridesResponse_launchTemplateSpecification,
    launchTemplateAndOverridesResponse_overrides,

    -- ** LaunchTemplateBlockDeviceMapping
    launchTemplateBlockDeviceMapping_ebs,
    launchTemplateBlockDeviceMapping_deviceName,
    launchTemplateBlockDeviceMapping_noDevice,
    launchTemplateBlockDeviceMapping_virtualName,

    -- ** LaunchTemplateBlockDeviceMappingRequest
    launchTemplateBlockDeviceMappingRequest_ebs,
    launchTemplateBlockDeviceMappingRequest_deviceName,
    launchTemplateBlockDeviceMappingRequest_noDevice,
    launchTemplateBlockDeviceMappingRequest_virtualName,

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
    launchTemplateCpuOptions_coreCount,
    launchTemplateCpuOptions_threadsPerCore,

    -- ** LaunchTemplateCpuOptionsRequest
    launchTemplateCpuOptionsRequest_coreCount,
    launchTemplateCpuOptionsRequest_threadsPerCore,

    -- ** LaunchTemplateEbsBlockDevice
    launchTemplateEbsBlockDevice_deleteOnTermination,
    launchTemplateEbsBlockDevice_snapshotId,
    launchTemplateEbsBlockDevice_volumeType,
    launchTemplateEbsBlockDevice_volumeSize,
    launchTemplateEbsBlockDevice_encrypted,
    launchTemplateEbsBlockDevice_kmsKeyId,
    launchTemplateEbsBlockDevice_throughput,
    launchTemplateEbsBlockDevice_iops,

    -- ** LaunchTemplateEbsBlockDeviceRequest
    launchTemplateEbsBlockDeviceRequest_deleteOnTermination,
    launchTemplateEbsBlockDeviceRequest_snapshotId,
    launchTemplateEbsBlockDeviceRequest_volumeType,
    launchTemplateEbsBlockDeviceRequest_volumeSize,
    launchTemplateEbsBlockDeviceRequest_encrypted,
    launchTemplateEbsBlockDeviceRequest_kmsKeyId,
    launchTemplateEbsBlockDeviceRequest_throughput,
    launchTemplateEbsBlockDeviceRequest_iops,

    -- ** LaunchTemplateElasticInferenceAccelerator
    launchTemplateElasticInferenceAccelerator_count,
    launchTemplateElasticInferenceAccelerator_type,

    -- ** LaunchTemplateElasticInferenceAcceleratorResponse
    launchTemplateElasticInferenceAcceleratorResponse_type,
    launchTemplateElasticInferenceAcceleratorResponse_count,

    -- ** LaunchTemplateEnclaveOptions
    launchTemplateEnclaveOptions_enabled,

    -- ** LaunchTemplateEnclaveOptionsRequest
    launchTemplateEnclaveOptionsRequest_enabled,

    -- ** LaunchTemplateHibernationOptions
    launchTemplateHibernationOptions_configured,

    -- ** LaunchTemplateHibernationOptionsRequest
    launchTemplateHibernationOptionsRequest_configured,

    -- ** LaunchTemplateIamInstanceProfileSpecification
    launchTemplateIamInstanceProfileSpecification_name,
    launchTemplateIamInstanceProfileSpecification_arn,

    -- ** LaunchTemplateIamInstanceProfileSpecificationRequest
    launchTemplateIamInstanceProfileSpecificationRequest_name,
    launchTemplateIamInstanceProfileSpecificationRequest_arn,

    -- ** LaunchTemplateInstanceMaintenanceOptions
    launchTemplateInstanceMaintenanceOptions_autoRecovery,

    -- ** LaunchTemplateInstanceMaintenanceOptionsRequest
    launchTemplateInstanceMaintenanceOptionsRequest_autoRecovery,

    -- ** LaunchTemplateInstanceMarketOptions
    launchTemplateInstanceMarketOptions_marketType,
    launchTemplateInstanceMarketOptions_spotOptions,

    -- ** LaunchTemplateInstanceMarketOptionsRequest
    launchTemplateInstanceMarketOptionsRequest_marketType,
    launchTemplateInstanceMarketOptionsRequest_spotOptions,

    -- ** LaunchTemplateInstanceMetadataOptions
    launchTemplateInstanceMetadataOptions_httpPutResponseHopLimit,
    launchTemplateInstanceMetadataOptions_state,
    launchTemplateInstanceMetadataOptions_httpTokens,
    launchTemplateInstanceMetadataOptions_httpEndpoint,
    launchTemplateInstanceMetadataOptions_instanceMetadataTags,
    launchTemplateInstanceMetadataOptions_httpProtocolIpv6,

    -- ** LaunchTemplateInstanceMetadataOptionsRequest
    launchTemplateInstanceMetadataOptionsRequest_httpPutResponseHopLimit,
    launchTemplateInstanceMetadataOptionsRequest_httpTokens,
    launchTemplateInstanceMetadataOptionsRequest_httpEndpoint,
    launchTemplateInstanceMetadataOptionsRequest_instanceMetadataTags,
    launchTemplateInstanceMetadataOptionsRequest_httpProtocolIpv6,

    -- ** LaunchTemplateInstanceNetworkInterfaceSpecification
    launchTemplateInstanceNetworkInterfaceSpecification_ipv4PrefixCount,
    launchTemplateInstanceNetworkInterfaceSpecification_interfaceType,
    launchTemplateInstanceNetworkInterfaceSpecification_networkCardIndex,
    launchTemplateInstanceNetworkInterfaceSpecification_associatePublicIpAddress,
    launchTemplateInstanceNetworkInterfaceSpecification_deleteOnTermination,
    launchTemplateInstanceNetworkInterfaceSpecification_privateIpAddresses,
    launchTemplateInstanceNetworkInterfaceSpecification_subnetId,
    launchTemplateInstanceNetworkInterfaceSpecification_description,
    launchTemplateInstanceNetworkInterfaceSpecification_associateCarrierIpAddress,
    launchTemplateInstanceNetworkInterfaceSpecification_networkInterfaceId,
    launchTemplateInstanceNetworkInterfaceSpecification_ipv6AddressCount,
    launchTemplateInstanceNetworkInterfaceSpecification_ipv4Prefixes,
    launchTemplateInstanceNetworkInterfaceSpecification_privateIpAddress,
    launchTemplateInstanceNetworkInterfaceSpecification_ipv6PrefixCount,
    launchTemplateInstanceNetworkInterfaceSpecification_secondaryPrivateIpAddressCount,
    launchTemplateInstanceNetworkInterfaceSpecification_ipv6Prefixes,
    launchTemplateInstanceNetworkInterfaceSpecification_groups,
    launchTemplateInstanceNetworkInterfaceSpecification_ipv6Addresses,
    launchTemplateInstanceNetworkInterfaceSpecification_deviceIndex,

    -- ** LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv4PrefixCount,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_interfaceType,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_networkCardIndex,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_associatePublicIpAddress,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_deleteOnTermination,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_privateIpAddresses,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_subnetId,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_description,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_associateCarrierIpAddress,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_networkInterfaceId,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6AddressCount,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv4Prefixes,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_privateIpAddress,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6PrefixCount,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_secondaryPrivateIpAddressCount,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6Prefixes,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_groups,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6Addresses,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_deviceIndex,

    -- ** LaunchTemplateLicenseConfiguration
    launchTemplateLicenseConfiguration_licenseConfigurationArn,

    -- ** LaunchTemplateLicenseConfigurationRequest
    launchTemplateLicenseConfigurationRequest_licenseConfigurationArn,

    -- ** LaunchTemplateOverrides
    launchTemplateOverrides_instanceRequirements,
    launchTemplateOverrides_subnetId,
    launchTemplateOverrides_availabilityZone,
    launchTemplateOverrides_instanceType,
    launchTemplateOverrides_priority,
    launchTemplateOverrides_weightedCapacity,
    launchTemplateOverrides_spotPrice,

    -- ** LaunchTemplatePlacement
    launchTemplatePlacement_spreadDomain,
    launchTemplatePlacement_partitionNumber,
    launchTemplatePlacement_hostResourceGroupArn,
    launchTemplatePlacement_hostId,
    launchTemplatePlacement_availabilityZone,
    launchTemplatePlacement_groupName,
    launchTemplatePlacement_affinity,
    launchTemplatePlacement_tenancy,
    launchTemplatePlacement_groupId,

    -- ** LaunchTemplatePlacementRequest
    launchTemplatePlacementRequest_spreadDomain,
    launchTemplatePlacementRequest_partitionNumber,
    launchTemplatePlacementRequest_hostResourceGroupArn,
    launchTemplatePlacementRequest_hostId,
    launchTemplatePlacementRequest_availabilityZone,
    launchTemplatePlacementRequest_groupName,
    launchTemplatePlacementRequest_affinity,
    launchTemplatePlacementRequest_tenancy,
    launchTemplatePlacementRequest_groupId,

    -- ** LaunchTemplatePrivateDnsNameOptions
    launchTemplatePrivateDnsNameOptions_enableResourceNameDnsARecord,
    launchTemplatePrivateDnsNameOptions_enableResourceNameDnsAAAARecord,
    launchTemplatePrivateDnsNameOptions_hostnameType,

    -- ** LaunchTemplatePrivateDnsNameOptionsRequest
    launchTemplatePrivateDnsNameOptionsRequest_enableResourceNameDnsARecord,
    launchTemplatePrivateDnsNameOptionsRequest_enableResourceNameDnsAAAARecord,
    launchTemplatePrivateDnsNameOptionsRequest_hostnameType,

    -- ** LaunchTemplateSpecification
    launchTemplateSpecification_launchTemplateId,
    launchTemplateSpecification_version,
    launchTemplateSpecification_launchTemplateName,

    -- ** LaunchTemplateSpotMarketOptions
    launchTemplateSpotMarketOptions_blockDurationMinutes,
    launchTemplateSpotMarketOptions_maxPrice,
    launchTemplateSpotMarketOptions_instanceInterruptionBehavior,
    launchTemplateSpotMarketOptions_spotInstanceType,
    launchTemplateSpotMarketOptions_validUntil,

    -- ** LaunchTemplateSpotMarketOptionsRequest
    launchTemplateSpotMarketOptionsRequest_blockDurationMinutes,
    launchTemplateSpotMarketOptionsRequest_maxPrice,
    launchTemplateSpotMarketOptionsRequest_instanceInterruptionBehavior,
    launchTemplateSpotMarketOptionsRequest_spotInstanceType,
    launchTemplateSpotMarketOptionsRequest_validUntil,

    -- ** LaunchTemplateTagSpecification
    launchTemplateTagSpecification_tags,
    launchTemplateTagSpecification_resourceType,

    -- ** LaunchTemplateTagSpecificationRequest
    launchTemplateTagSpecificationRequest_tags,
    launchTemplateTagSpecificationRequest_resourceType,

    -- ** LaunchTemplateVersion
    launchTemplateVersion_launchTemplateData,
    launchTemplateVersion_defaultVersion,
    launchTemplateVersion_launchTemplateId,
    launchTemplateVersion_versionDescription,
    launchTemplateVersion_versionNumber,
    launchTemplateVersion_createTime,
    launchTemplateVersion_createdBy,
    launchTemplateVersion_launchTemplateName,

    -- ** LaunchTemplatesMonitoring
    launchTemplatesMonitoring_enabled,

    -- ** LaunchTemplatesMonitoringRequest
    launchTemplatesMonitoringRequest_enabled,

    -- ** LicenseConfiguration
    licenseConfiguration_licenseConfigurationArn,

    -- ** LicenseConfigurationRequest
    licenseConfigurationRequest_licenseConfigurationArn,

    -- ** LoadBalancersConfig
    loadBalancersConfig_targetGroupsConfig,
    loadBalancersConfig_classicLoadBalancersConfig,

    -- ** LoadPermission
    loadPermission_userId,
    loadPermission_group,

    -- ** LoadPermissionModifications
    loadPermissionModifications_remove,
    loadPermissionModifications_add,

    -- ** LoadPermissionRequest
    loadPermissionRequest_userId,
    loadPermissionRequest_group,

    -- ** LocalGateway
    localGateway_tags,
    localGateway_localGatewayId,
    localGateway_outpostArn,
    localGateway_ownerId,
    localGateway_state,

    -- ** LocalGatewayRoute
    localGatewayRoute_type,
    localGatewayRoute_ownerId,
    localGatewayRoute_localGatewayRouteTableId,
    localGatewayRoute_subnetId,
    localGatewayRoute_coipPoolId,
    localGatewayRoute_state,
    localGatewayRoute_destinationCidrBlock,
    localGatewayRoute_networkInterfaceId,
    localGatewayRoute_localGatewayVirtualInterfaceGroupId,
    localGatewayRoute_localGatewayRouteTableArn,

    -- ** LocalGatewayRouteTable
    localGatewayRouteTable_tags,
    localGatewayRouteTable_localGatewayId,
    localGatewayRouteTable_outpostArn,
    localGatewayRouteTable_ownerId,
    localGatewayRouteTable_localGatewayRouteTableId,
    localGatewayRouteTable_state,
    localGatewayRouteTable_mode,
    localGatewayRouteTable_stateReason,
    localGatewayRouteTable_localGatewayRouteTableArn,

    -- ** LocalGatewayRouteTableVirtualInterfaceGroupAssociation
    localGatewayRouteTableVirtualInterfaceGroupAssociation_tags,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayId,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_ownerId,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableId,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_state,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayVirtualInterfaceGroupId,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableVirtualInterfaceGroupAssociationId,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableArn,

    -- ** LocalGatewayRouteTableVpcAssociation
    localGatewayRouteTableVpcAssociation_tags,
    localGatewayRouteTableVpcAssociation_localGatewayId,
    localGatewayRouteTableVpcAssociation_ownerId,
    localGatewayRouteTableVpcAssociation_localGatewayRouteTableId,
    localGatewayRouteTableVpcAssociation_localGatewayRouteTableVpcAssociationId,
    localGatewayRouteTableVpcAssociation_state,
    localGatewayRouteTableVpcAssociation_vpcId,
    localGatewayRouteTableVpcAssociation_localGatewayRouteTableArn,

    -- ** LocalGatewayVirtualInterface
    localGatewayVirtualInterface_localBgpAsn,
    localGatewayVirtualInterface_tags,
    localGatewayVirtualInterface_localGatewayId,
    localGatewayVirtualInterface_ownerId,
    localGatewayVirtualInterface_peerBgpAsn,
    localGatewayVirtualInterface_vlan,
    localGatewayVirtualInterface_localGatewayVirtualInterfaceId,
    localGatewayVirtualInterface_peerAddress,
    localGatewayVirtualInterface_localAddress,

    -- ** LocalGatewayVirtualInterfaceGroup
    localGatewayVirtualInterfaceGroup_tags,
    localGatewayVirtualInterfaceGroup_localGatewayId,
    localGatewayVirtualInterfaceGroup_ownerId,
    localGatewayVirtualInterfaceGroup_localGatewayVirtualInterfaceIds,
    localGatewayVirtualInterfaceGroup_localGatewayVirtualInterfaceGroupId,

    -- ** ManagedPrefixList
    managedPrefixList_tags,
    managedPrefixList_addressFamily,
    managedPrefixList_ownerId,
    managedPrefixList_prefixListId,
    managedPrefixList_state,
    managedPrefixList_maxEntries,
    managedPrefixList_prefixListArn,
    managedPrefixList_stateMessage,
    managedPrefixList_prefixListName,
    managedPrefixList_version,

    -- ** MemoryGiBPerVCpu
    memoryGiBPerVCpu_max,
    memoryGiBPerVCpu_min,

    -- ** MemoryGiBPerVCpuRequest
    memoryGiBPerVCpuRequest_max,
    memoryGiBPerVCpuRequest_min,

    -- ** MemoryInfo
    memoryInfo_sizeInMiB,

    -- ** MemoryMiB
    memoryMiB_max,
    memoryMiB_min,

    -- ** MemoryMiBRequest
    memoryMiBRequest_max,
    memoryMiBRequest_min,

    -- ** ModifyTransitGatewayOptions
    modifyTransitGatewayOptions_associationDefaultRouteTableId,
    modifyTransitGatewayOptions_dnsSupport,
    modifyTransitGatewayOptions_defaultRouteTableAssociation,
    modifyTransitGatewayOptions_removeTransitGatewayCidrBlocks,
    modifyTransitGatewayOptions_propagationDefaultRouteTableId,
    modifyTransitGatewayOptions_autoAcceptSharedAttachments,
    modifyTransitGatewayOptions_amazonSideAsn,
    modifyTransitGatewayOptions_vpnEcmpSupport,
    modifyTransitGatewayOptions_defaultRouteTablePropagation,
    modifyTransitGatewayOptions_addTransitGatewayCidrBlocks,

    -- ** ModifyTransitGatewayVpcAttachmentRequestOptions
    modifyTransitGatewayVpcAttachmentRequestOptions_dnsSupport,
    modifyTransitGatewayVpcAttachmentRequestOptions_ipv6Support,
    modifyTransitGatewayVpcAttachmentRequestOptions_applianceModeSupport,

    -- ** ModifyVpnTunnelOptionsSpecification
    modifyVpnTunnelOptionsSpecification_tunnelInsideIpv6Cidr,
    modifyVpnTunnelOptionsSpecification_phase1LifetimeSeconds,
    modifyVpnTunnelOptionsSpecification_phase2LifetimeSeconds,
    modifyVpnTunnelOptionsSpecification_phase2EncryptionAlgorithms,
    modifyVpnTunnelOptionsSpecification_phase1DHGroupNumbers,
    modifyVpnTunnelOptionsSpecification_phase1IntegrityAlgorithms,
    modifyVpnTunnelOptionsSpecification_dPDTimeoutSeconds,
    modifyVpnTunnelOptionsSpecification_iKEVersions,
    modifyVpnTunnelOptionsSpecification_preSharedKey,
    modifyVpnTunnelOptionsSpecification_dPDTimeoutAction,
    modifyVpnTunnelOptionsSpecification_logOptions,
    modifyVpnTunnelOptionsSpecification_phase2DHGroupNumbers,
    modifyVpnTunnelOptionsSpecification_rekeyFuzzPercentage,
    modifyVpnTunnelOptionsSpecification_startupAction,
    modifyVpnTunnelOptionsSpecification_rekeyMarginTimeSeconds,
    modifyVpnTunnelOptionsSpecification_phase2IntegrityAlgorithms,
    modifyVpnTunnelOptionsSpecification_phase1EncryptionAlgorithms,
    modifyVpnTunnelOptionsSpecification_tunnelInsideCidr,
    modifyVpnTunnelOptionsSpecification_replayWindowSize,

    -- ** Monitoring
    monitoring_state,

    -- ** MovingAddressStatus
    movingAddressStatus_moveStatus,
    movingAddressStatus_publicIp,

    -- ** NatGateway
    natGateway_tags,
    natGateway_provisionedBandwidth,
    natGateway_failureCode,
    natGateway_deleteTime,
    natGateway_subnetId,
    natGateway_state,
    natGateway_natGatewayId,
    natGateway_failureMessage,
    natGateway_natGatewayAddresses,
    natGateway_connectivityType,
    natGateway_createTime,
    natGateway_vpcId,

    -- ** NatGatewayAddress
    natGatewayAddress_allocationId,
    natGatewayAddress_publicIp,
    natGatewayAddress_networkInterfaceId,
    natGatewayAddress_privateIp,

    -- ** NetworkAcl
    networkAcl_tags,
    networkAcl_ownerId,
    networkAcl_networkAclId,
    networkAcl_associations,
    networkAcl_isDefault,
    networkAcl_entries,
    networkAcl_vpcId,

    -- ** NetworkAclAssociation
    networkAclAssociation_networkAclId,
    networkAclAssociation_subnetId,
    networkAclAssociation_networkAclAssociationId,

    -- ** NetworkAclEntry
    networkAclEntry_icmpTypeCode,
    networkAclEntry_egress,
    networkAclEntry_portRange,
    networkAclEntry_ruleNumber,
    networkAclEntry_cidrBlock,
    networkAclEntry_ruleAction,
    networkAclEntry_protocol,
    networkAclEntry_ipv6CidrBlock,

    -- ** NetworkBandwidthGbps
    networkBandwidthGbps_max,
    networkBandwidthGbps_min,

    -- ** NetworkBandwidthGbpsRequest
    networkBandwidthGbpsRequest_max,
    networkBandwidthGbpsRequest_min,

    -- ** NetworkCardInfo
    networkCardInfo_networkCardIndex,
    networkCardInfo_networkPerformance,
    networkCardInfo_maximumNetworkInterfaces,

    -- ** NetworkInfo
    networkInfo_ipv4AddressesPerInterface,
    networkInfo_ipv6Supported,
    networkInfo_networkPerformance,
    networkInfo_efaInfo,
    networkInfo_defaultNetworkCardIndex,
    networkInfo_efaSupported,
    networkInfo_ipv6AddressesPerInterface,
    networkInfo_networkCards,
    networkInfo_maximumNetworkInterfaces,
    networkInfo_encryptionInTransitSupported,
    networkInfo_enaSupport,
    networkInfo_maximumNetworkCards,

    -- ** NetworkInsightsAccessScope
    networkInsightsAccessScope_tags,
    networkInsightsAccessScope_networkInsightsAccessScopeId,
    networkInsightsAccessScope_networkInsightsAccessScopeArn,
    networkInsightsAccessScope_updatedDate,
    networkInsightsAccessScope_createdDate,

    -- ** NetworkInsightsAccessScopeAnalysis
    networkInsightsAccessScopeAnalysis_tags,
    networkInsightsAccessScopeAnalysis_networkInsightsAccessScopeAnalysisId,
    networkInsightsAccessScopeAnalysis_networkInsightsAccessScopeId,
    networkInsightsAccessScopeAnalysis_analyzedEniCount,
    networkInsightsAccessScopeAnalysis_endDate,
    networkInsightsAccessScopeAnalysis_status,
    networkInsightsAccessScopeAnalysis_warningMessage,
    networkInsightsAccessScopeAnalysis_startDate,
    networkInsightsAccessScopeAnalysis_networkInsightsAccessScopeAnalysisArn,
    networkInsightsAccessScopeAnalysis_findingsFound,
    networkInsightsAccessScopeAnalysis_statusMessage,

    -- ** NetworkInsightsAccessScopeContent
    networkInsightsAccessScopeContent_networkInsightsAccessScopeId,
    networkInsightsAccessScopeContent_excludePaths,
    networkInsightsAccessScopeContent_matchPaths,

    -- ** NetworkInsightsAnalysis
    networkInsightsAnalysis_filterInArns,
    networkInsightsAnalysis_tags,
    networkInsightsAnalysis_networkPathFound,
    networkInsightsAnalysis_forwardPathComponents,
    networkInsightsAnalysis_status,
    networkInsightsAnalysis_warningMessage,
    networkInsightsAnalysis_networkInsightsAnalysisId,
    networkInsightsAnalysis_startDate,
    networkInsightsAnalysis_explanations,
    networkInsightsAnalysis_alternatePathHints,
    networkInsightsAnalysis_returnPathComponents,
    networkInsightsAnalysis_networkInsightsAnalysisArn,
    networkInsightsAnalysis_statusMessage,
    networkInsightsAnalysis_networkInsightsPathId,

    -- ** NetworkInsightsPath
    networkInsightsPath_tags,
    networkInsightsPath_destination,
    networkInsightsPath_destinationIp,
    networkInsightsPath_sourceIp,
    networkInsightsPath_destinationPort,
    networkInsightsPath_networkInsightsPathArn,
    networkInsightsPath_source,
    networkInsightsPath_createdDate,
    networkInsightsPath_protocol,
    networkInsightsPath_networkInsightsPathId,

    -- ** NetworkInterface
    networkInterface_interfaceType,
    networkInterface_attachment,
    networkInterface_outpostArn,
    networkInterface_ownerId,
    networkInterface_ipv6Native,
    networkInterface_sourceDestCheck,
    networkInterface_privateIpAddresses,
    networkInterface_subnetId,
    networkInterface_requesterManaged,
    networkInterface_status,
    networkInterface_availabilityZone,
    networkInterface_description,
    networkInterface_association,
    networkInterface_ipv6Address,
    networkInterface_macAddress,
    networkInterface_networkInterfaceId,
    networkInterface_denyAllIgwTraffic,
    networkInterface_ipv4Prefixes,
    networkInterface_privateIpAddress,
    networkInterface_privateDnsName,
    networkInterface_tagSet,
    networkInterface_ipv6Prefixes,
    networkInterface_vpcId,
    networkInterface_groups,
    networkInterface_ipv6Addresses,
    networkInterface_requesterId,

    -- ** NetworkInterfaceAssociation
    networkInterfaceAssociation_allocationId,
    networkInterfaceAssociation_ipOwnerId,
    networkInterfaceAssociation_carrierIp,
    networkInterfaceAssociation_publicIp,
    networkInterfaceAssociation_publicDnsName,
    networkInterfaceAssociation_customerOwnedIp,
    networkInterfaceAssociation_associationId,

    -- ** NetworkInterfaceAttachment
    networkInterfaceAttachment_networkCardIndex,
    networkInterfaceAttachment_deleteOnTermination,
    networkInterfaceAttachment_status,
    networkInterfaceAttachment_attachmentId,
    networkInterfaceAttachment_instanceId,
    networkInterfaceAttachment_attachTime,
    networkInterfaceAttachment_deviceIndex,
    networkInterfaceAttachment_instanceOwnerId,

    -- ** NetworkInterfaceAttachmentChanges
    networkInterfaceAttachmentChanges_deleteOnTermination,
    networkInterfaceAttachmentChanges_attachmentId,

    -- ** NetworkInterfaceCount
    networkInterfaceCount_max,
    networkInterfaceCount_min,

    -- ** NetworkInterfaceCountRequest
    networkInterfaceCountRequest_max,
    networkInterfaceCountRequest_min,

    -- ** NetworkInterfaceIpv6Address
    networkInterfaceIpv6Address_ipv6Address,

    -- ** NetworkInterfacePermission
    networkInterfacePermission_awsAccountId,
    networkInterfacePermission_networkInterfacePermissionId,
    networkInterfacePermission_permission,
    networkInterfacePermission_networkInterfaceId,
    networkInterfacePermission_permissionState,
    networkInterfacePermission_awsService,

    -- ** NetworkInterfacePermissionState
    networkInterfacePermissionState_state,
    networkInterfacePermissionState_statusMessage,

    -- ** NetworkInterfacePrivateIpAddress
    networkInterfacePrivateIpAddress_association,
    networkInterfacePrivateIpAddress_primary,
    networkInterfacePrivateIpAddress_privateIpAddress,
    networkInterfacePrivateIpAddress_privateDnsName,

    -- ** NewDhcpConfiguration
    newDhcpConfiguration_key,
    newDhcpConfiguration_values,

    -- ** OnDemandOptions
    onDemandOptions_singleInstanceType,
    onDemandOptions_singleAvailabilityZone,
    onDemandOptions_minTargetCapacity,
    onDemandOptions_capacityReservationOptions,
    onDemandOptions_allocationStrategy,
    onDemandOptions_maxTotalPrice,

    -- ** OnDemandOptionsRequest
    onDemandOptionsRequest_singleInstanceType,
    onDemandOptionsRequest_singleAvailabilityZone,
    onDemandOptionsRequest_minTargetCapacity,
    onDemandOptionsRequest_capacityReservationOptions,
    onDemandOptionsRequest_allocationStrategy,
    onDemandOptionsRequest_maxTotalPrice,

    -- ** PacketHeaderStatement
    packetHeaderStatement_destinationPorts,
    packetHeaderStatement_destinationPrefixLists,
    packetHeaderStatement_protocols,
    packetHeaderStatement_destinationAddresses,
    packetHeaderStatement_sourceAddresses,
    packetHeaderStatement_sourcePorts,
    packetHeaderStatement_sourcePrefixLists,

    -- ** PacketHeaderStatementRequest
    packetHeaderStatementRequest_destinationPorts,
    packetHeaderStatementRequest_destinationPrefixLists,
    packetHeaderStatementRequest_protocols,
    packetHeaderStatementRequest_destinationAddresses,
    packetHeaderStatementRequest_sourceAddresses,
    packetHeaderStatementRequest_sourcePorts,
    packetHeaderStatementRequest_sourcePrefixLists,

    -- ** PathComponent
    pathComponent_routeTableRoute,
    pathComponent_vpc,
    pathComponent_additionalDetails,
    pathComponent_outboundHeader,
    pathComponent_attachedTo,
    pathComponent_sourceVpc,
    pathComponent_aclRule,
    pathComponent_explanations,
    pathComponent_transitGateway,
    pathComponent_sequenceNumber,
    pathComponent_destinationVpc,
    pathComponent_elasticLoadBalancerListener,
    pathComponent_subnet,
    pathComponent_inboundHeader,
    pathComponent_transitGatewayRouteTableRoute,
    pathComponent_component,
    pathComponent_securityGroupRule,

    -- ** PathStatement
    pathStatement_resourceStatement,
    pathStatement_packetHeaderStatement,

    -- ** PathStatementRequest
    pathStatementRequest_resourceStatement,
    pathStatementRequest_packetHeaderStatement,

    -- ** PciId
    pciId_deviceId,
    pciId_subsystemVendorId,
    pciId_subsystemId,
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
    peeringTgwInfo_coreNetworkId,
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
    placement_spreadDomain,
    placement_partitionNumber,
    placement_hostResourceGroupArn,
    placement_hostId,
    placement_availabilityZone,
    placement_groupName,
    placement_affinity,
    placement_tenancy,
    placement_groupId,

    -- ** PlacementGroup
    placementGroup_tags,
    placementGroup_partitionCount,
    placementGroup_spreadLevel,
    placementGroup_state,
    placementGroup_groupName,
    placementGroup_strategy,
    placementGroup_groupArn,
    placementGroup_groupId,

    -- ** PlacementGroupInfo
    placementGroupInfo_supportedStrategies,

    -- ** PlacementResponse
    placementResponse_groupName,

    -- ** PoolCidrBlock
    poolCidrBlock_cidr,

    -- ** PortRange
    portRange_from,
    portRange_to,

    -- ** PrefixList
    prefixList_prefixListId,
    prefixList_cidrs,
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
    priceSchedule_active,
    priceSchedule_currencyCode,
    priceSchedule_price,
    priceSchedule_term,

    -- ** PriceScheduleSpecification
    priceScheduleSpecification_currencyCode,
    priceScheduleSpecification_price,
    priceScheduleSpecification_term,

    -- ** PricingDetail
    pricingDetail_count,
    pricingDetail_price,

    -- ** PrincipalIdFormat
    principalIdFormat_arn,
    principalIdFormat_statuses,

    -- ** PrivateDnsDetails
    privateDnsDetails_privateDnsName,

    -- ** PrivateDnsNameConfiguration
    privateDnsNameConfiguration_name,
    privateDnsNameConfiguration_type,
    privateDnsNameConfiguration_state,
    privateDnsNameConfiguration_value,

    -- ** PrivateDnsNameOptionsOnLaunch
    privateDnsNameOptionsOnLaunch_enableResourceNameDnsARecord,
    privateDnsNameOptionsOnLaunch_enableResourceNameDnsAAAARecord,
    privateDnsNameOptionsOnLaunch_hostnameType,

    -- ** PrivateDnsNameOptionsRequest
    privateDnsNameOptionsRequest_enableResourceNameDnsARecord,
    privateDnsNameOptionsRequest_enableResourceNameDnsAAAARecord,
    privateDnsNameOptionsRequest_hostnameType,

    -- ** PrivateDnsNameOptionsResponse
    privateDnsNameOptionsResponse_enableResourceNameDnsARecord,
    privateDnsNameOptionsResponse_enableResourceNameDnsAAAARecord,
    privateDnsNameOptionsResponse_hostnameType,

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
    provisionedBandwidth_requested,
    provisionedBandwidth_provisionTime,
    provisionedBandwidth_requestTime,
    provisionedBandwidth_status,
    provisionedBandwidth_provisioned,

    -- ** PtrUpdateStatus
    ptrUpdateStatus_status,
    ptrUpdateStatus_reason,
    ptrUpdateStatus_value,

    -- ** PublicIpv4Pool
    publicIpv4Pool_tags,
    publicIpv4Pool_networkBorderGroup,
    publicIpv4Pool_totalAddressCount,
    publicIpv4Pool_description,
    publicIpv4Pool_poolId,
    publicIpv4Pool_totalAvailableAddressCount,
    publicIpv4Pool_poolAddressRanges,

    -- ** PublicIpv4PoolRange
    publicIpv4PoolRange_firstAddress,
    publicIpv4PoolRange_lastAddress,
    publicIpv4PoolRange_availableAddressCount,
    publicIpv4PoolRange_addressCount,

    -- ** Purchase
    purchase_hourlyPrice,
    purchase_upfrontPrice,
    purchase_duration,
    purchase_currencyCode,
    purchase_instanceFamily,
    purchase_hostReservationId,
    purchase_hostIdSet,
    purchase_paymentOption,

    -- ** PurchaseRequest
    purchaseRequest_instanceCount,
    purchaseRequest_purchaseToken,

    -- ** RecurringCharge
    recurringCharge_frequency,
    recurringCharge_amount,

    -- ** ReferencedSecurityGroup
    referencedSecurityGroup_vpcPeeringConnectionId,
    referencedSecurityGroup_peeringStatus,
    referencedSecurityGroup_userId,
    referencedSecurityGroup_vpcId,
    referencedSecurityGroup_groupId,

    -- ** RegionInfo
    regionInfo_optInStatus,
    regionInfo_regionName,
    regionInfo_endpoint,

    -- ** RegisterInstanceTagAttributeRequest
    registerInstanceTagAttributeRequest_includeAllTagsOfInstance,
    registerInstanceTagAttributeRequest_instanceTagKeys,

    -- ** RemoveIpamOperatingRegion
    removeIpamOperatingRegion_regionName,

    -- ** RemovePrefixListEntry
    removePrefixListEntry_cidr,

    -- ** ReplaceRootVolumeTask
    replaceRootVolumeTask_tags,
    replaceRootVolumeTask_taskState,
    replaceRootVolumeTask_snapshotId,
    replaceRootVolumeTask_replaceRootVolumeTaskId,
    replaceRootVolumeTask_completeTime,
    replaceRootVolumeTask_instanceId,
    replaceRootVolumeTask_deleteReplacedRootVolume,
    replaceRootVolumeTask_startTime,
    replaceRootVolumeTask_imageId,

    -- ** RequestIpamResourceTag
    requestIpamResourceTag_key,
    requestIpamResourceTag_value,

    -- ** RequestLaunchTemplateData
    requestLaunchTemplateData_ebsOptimized,
    requestLaunchTemplateData_hibernationOptions,
    requestLaunchTemplateData_iamInstanceProfile,
    requestLaunchTemplateData_elasticInferenceAccelerators,
    requestLaunchTemplateData_placement,
    requestLaunchTemplateData_userData,
    requestLaunchTemplateData_instanceRequirements,
    requestLaunchTemplateData_creditSpecification,
    requestLaunchTemplateData_blockDeviceMappings,
    requestLaunchTemplateData_securityGroupIds,
    requestLaunchTemplateData_instanceInitiatedShutdownBehavior,
    requestLaunchTemplateData_monitoring,
    requestLaunchTemplateData_instanceMarketOptions,
    requestLaunchTemplateData_capacityReservationSpecification,
    requestLaunchTemplateData_instanceType,
    requestLaunchTemplateData_securityGroups,
    requestLaunchTemplateData_elasticGpuSpecifications,
    requestLaunchTemplateData_ramDiskId,
    requestLaunchTemplateData_maintenanceOptions,
    requestLaunchTemplateData_privateDnsNameOptions,
    requestLaunchTemplateData_keyName,
    requestLaunchTemplateData_licenseSpecifications,
    requestLaunchTemplateData_kernelId,
    requestLaunchTemplateData_disableApiTermination,
    requestLaunchTemplateData_tagSpecifications,
    requestLaunchTemplateData_cpuOptions,
    requestLaunchTemplateData_disableApiStop,
    requestLaunchTemplateData_imageId,
    requestLaunchTemplateData_networkInterfaces,
    requestLaunchTemplateData_enclaveOptions,
    requestLaunchTemplateData_metadataOptions,

    -- ** RequestSpotLaunchSpecification
    requestSpotLaunchSpecification_ebsOptimized,
    requestSpotLaunchSpecification_iamInstanceProfile,
    requestSpotLaunchSpecification_placement,
    requestSpotLaunchSpecification_userData,
    requestSpotLaunchSpecification_blockDeviceMappings,
    requestSpotLaunchSpecification_securityGroupIds,
    requestSpotLaunchSpecification_addressingType,
    requestSpotLaunchSpecification_monitoring,
    requestSpotLaunchSpecification_subnetId,
    requestSpotLaunchSpecification_instanceType,
    requestSpotLaunchSpecification_securityGroups,
    requestSpotLaunchSpecification_ramdiskId,
    requestSpotLaunchSpecification_keyName,
    requestSpotLaunchSpecification_kernelId,
    requestSpotLaunchSpecification_imageId,
    requestSpotLaunchSpecification_networkInterfaces,

    -- ** Reservation
    reservation_instances,
    reservation_groups,
    reservation_requesterId,
    reservation_reservationId,
    reservation_ownerId,

    -- ** ReservationFleetInstanceSpecification
    reservationFleetInstanceSpecification_ebsOptimized,
    reservationFleetInstanceSpecification_availabilityZone,
    reservationFleetInstanceSpecification_instanceType,
    reservationFleetInstanceSpecification_instancePlatform,
    reservationFleetInstanceSpecification_priority,
    reservationFleetInstanceSpecification_weight,
    reservationFleetInstanceSpecification_availabilityZoneId,

    -- ** ReservationValue
    reservationValue_hourlyPrice,
    reservationValue_remainingTotalValue,
    reservationValue_remainingUpfrontValue,

    -- ** ReservedInstanceLimitPrice
    reservedInstanceLimitPrice_currencyCode,
    reservedInstanceLimitPrice_amount,

    -- ** ReservedInstanceReservationValue
    reservedInstanceReservationValue_reservedInstanceId,
    reservedInstanceReservationValue_reservationValue,

    -- ** ReservedInstances
    reservedInstances_tags,
    reservedInstances_offeringClass,
    reservedInstances_reservedInstancesId,
    reservedInstances_recurringCharges,
    reservedInstances_instanceTenancy,
    reservedInstances_start,
    reservedInstances_state,
    reservedInstances_offeringType,
    reservedInstances_availabilityZone,
    reservedInstances_instanceType,
    reservedInstances_duration,
    reservedInstances_currencyCode,
    reservedInstances_end,
    reservedInstances_scope,
    reservedInstances_instanceCount,
    reservedInstances_productDescription,
    reservedInstances_fixedPrice,
    reservedInstances_usagePrice,

    -- ** ReservedInstancesConfiguration
    reservedInstancesConfiguration_platform,
    reservedInstancesConfiguration_availabilityZone,
    reservedInstancesConfiguration_instanceType,
    reservedInstancesConfiguration_scope,
    reservedInstancesConfiguration_instanceCount,

    -- ** ReservedInstancesId
    reservedInstancesId_reservedInstancesId,

    -- ** ReservedInstancesListing
    reservedInstancesListing_tags,
    reservedInstancesListing_clientToken,
    reservedInstancesListing_reservedInstancesId,
    reservedInstancesListing_instanceCounts,
    reservedInstancesListing_status,
    reservedInstancesListing_updateDate,
    reservedInstancesListing_priceSchedules,
    reservedInstancesListing_createDate,
    reservedInstancesListing_reservedInstancesListingId,
    reservedInstancesListing_statusMessage,

    -- ** ReservedInstancesModification
    reservedInstancesModification_clientToken,
    reservedInstancesModification_reservedInstancesIds,
    reservedInstancesModification_status,
    reservedInstancesModification_updateDate,
    reservedInstancesModification_effectiveDate,
    reservedInstancesModification_createDate,
    reservedInstancesModification_modificationResults,
    reservedInstancesModification_statusMessage,
    reservedInstancesModification_reservedInstancesModificationId,

    -- ** ReservedInstancesModificationResult
    reservedInstancesModificationResult_reservedInstancesId,
    reservedInstancesModificationResult_targetConfiguration,

    -- ** ReservedInstancesOffering
    reservedInstancesOffering_offeringClass,
    reservedInstancesOffering_recurringCharges,
    reservedInstancesOffering_instanceTenancy,
    reservedInstancesOffering_offeringType,
    reservedInstancesOffering_availabilityZone,
    reservedInstancesOffering_instanceType,
    reservedInstancesOffering_duration,
    reservedInstancesOffering_currencyCode,
    reservedInstancesOffering_marketplace,
    reservedInstancesOffering_scope,
    reservedInstancesOffering_productDescription,
    reservedInstancesOffering_pricingDetails,
    reservedInstancesOffering_fixedPrice,
    reservedInstancesOffering_reservedInstancesOfferingId,
    reservedInstancesOffering_usagePrice,

    -- ** ResourceStatement
    resourceStatement_resourceTypes,
    resourceStatement_resources,

    -- ** ResourceStatementRequest
    resourceStatementRequest_resourceTypes,
    resourceStatementRequest_resources,

    -- ** ResponseError
    responseError_message,
    responseError_code,

    -- ** ResponseLaunchTemplateData
    responseLaunchTemplateData_ebsOptimized,
    responseLaunchTemplateData_hibernationOptions,
    responseLaunchTemplateData_iamInstanceProfile,
    responseLaunchTemplateData_elasticInferenceAccelerators,
    responseLaunchTemplateData_placement,
    responseLaunchTemplateData_userData,
    responseLaunchTemplateData_instanceRequirements,
    responseLaunchTemplateData_creditSpecification,
    responseLaunchTemplateData_blockDeviceMappings,
    responseLaunchTemplateData_securityGroupIds,
    responseLaunchTemplateData_instanceInitiatedShutdownBehavior,
    responseLaunchTemplateData_monitoring,
    responseLaunchTemplateData_instanceMarketOptions,
    responseLaunchTemplateData_capacityReservationSpecification,
    responseLaunchTemplateData_instanceType,
    responseLaunchTemplateData_securityGroups,
    responseLaunchTemplateData_elasticGpuSpecifications,
    responseLaunchTemplateData_ramDiskId,
    responseLaunchTemplateData_maintenanceOptions,
    responseLaunchTemplateData_privateDnsNameOptions,
    responseLaunchTemplateData_keyName,
    responseLaunchTemplateData_licenseSpecifications,
    responseLaunchTemplateData_kernelId,
    responseLaunchTemplateData_disableApiTermination,
    responseLaunchTemplateData_tagSpecifications,
    responseLaunchTemplateData_cpuOptions,
    responseLaunchTemplateData_disableApiStop,
    responseLaunchTemplateData_imageId,
    responseLaunchTemplateData_networkInterfaces,
    responseLaunchTemplateData_enclaveOptions,
    responseLaunchTemplateData_metadataOptions,

    -- ** Route
    route_localGatewayId,
    route_destinationPrefixListId,
    route_carrierGatewayId,
    route_transitGatewayId,
    route_state,
    route_natGatewayId,
    route_vpcPeeringConnectionId,
    route_destinationCidrBlock,
    route_coreNetworkArn,
    route_instanceId,
    route_egressOnlyInternetGatewayId,
    route_networkInterfaceId,
    route_gatewayId,
    route_destinationIpv6CidrBlock,
    route_origin,
    route_instanceOwnerId,

    -- ** RouteTable
    routeTable_tags,
    routeTable_propagatingVgws,
    routeTable_ownerId,
    routeTable_associations,
    routeTable_routeTableId,
    routeTable_vpcId,
    routeTable_routes,

    -- ** RouteTableAssociation
    routeTableAssociation_routeTableAssociationId,
    routeTableAssociation_associationState,
    routeTableAssociation_subnetId,
    routeTableAssociation_routeTableId,
    routeTableAssociation_gatewayId,
    routeTableAssociation_main,

    -- ** RouteTableAssociationState
    routeTableAssociationState_state,
    routeTableAssociationState_statusMessage,

    -- ** RunInstancesMonitoringEnabled
    runInstancesMonitoringEnabled_enabled,

    -- ** S3ObjectTag
    s3ObjectTag_key,
    s3ObjectTag_value,

    -- ** S3Storage
    s3Storage_bucket,
    s3Storage_aWSAccessKeyId,
    s3Storage_uploadPolicy,
    s3Storage_uploadPolicySignature,
    s3Storage_prefix,

    -- ** ScheduledInstance
    scheduledInstance_termStartDate,
    scheduledInstance_nextSlotStartTime,
    scheduledInstance_hourlyPrice,
    scheduledInstance_totalScheduledInstanceHours,
    scheduledInstance_scheduledInstanceId,
    scheduledInstance_networkPlatform,
    scheduledInstance_platform,
    scheduledInstance_availabilityZone,
    scheduledInstance_instanceType,
    scheduledInstance_instanceCount,
    scheduledInstance_createDate,
    scheduledInstance_recurrence,
    scheduledInstance_previousSlotEndTime,
    scheduledInstance_termEndDate,
    scheduledInstance_slotDurationInHours,

    -- ** ScheduledInstanceAvailability
    scheduledInstanceAvailability_purchaseToken,
    scheduledInstanceAvailability_minTermDurationInDays,
    scheduledInstanceAvailability_hourlyPrice,
    scheduledInstanceAvailability_maxTermDurationInDays,
    scheduledInstanceAvailability_totalScheduledInstanceHours,
    scheduledInstanceAvailability_networkPlatform,
    scheduledInstanceAvailability_platform,
    scheduledInstanceAvailability_availabilityZone,
    scheduledInstanceAvailability_instanceType,
    scheduledInstanceAvailability_firstSlotStartTime,
    scheduledInstanceAvailability_availableInstanceCount,
    scheduledInstanceAvailability_recurrence,
    scheduledInstanceAvailability_slotDurationInHours,

    -- ** ScheduledInstanceRecurrence
    scheduledInstanceRecurrence_occurrenceDaySet,
    scheduledInstanceRecurrence_interval,
    scheduledInstanceRecurrence_frequency,
    scheduledInstanceRecurrence_occurrenceRelativeToEnd,
    scheduledInstanceRecurrence_occurrenceUnit,

    -- ** ScheduledInstanceRecurrenceRequest
    scheduledInstanceRecurrenceRequest_occurrenceDays,
    scheduledInstanceRecurrenceRequest_interval,
    scheduledInstanceRecurrenceRequest_frequency,
    scheduledInstanceRecurrenceRequest_occurrenceRelativeToEnd,
    scheduledInstanceRecurrenceRequest_occurrenceUnit,

    -- ** ScheduledInstancesBlockDeviceMapping
    scheduledInstancesBlockDeviceMapping_ebs,
    scheduledInstancesBlockDeviceMapping_deviceName,
    scheduledInstancesBlockDeviceMapping_noDevice,
    scheduledInstancesBlockDeviceMapping_virtualName,

    -- ** ScheduledInstancesEbs
    scheduledInstancesEbs_deleteOnTermination,
    scheduledInstancesEbs_snapshotId,
    scheduledInstancesEbs_volumeType,
    scheduledInstancesEbs_volumeSize,
    scheduledInstancesEbs_encrypted,
    scheduledInstancesEbs_iops,

    -- ** ScheduledInstancesIamInstanceProfile
    scheduledInstancesIamInstanceProfile_name,
    scheduledInstancesIamInstanceProfile_arn,

    -- ** ScheduledInstancesIpv6Address
    scheduledInstancesIpv6Address_ipv6Address,

    -- ** ScheduledInstancesLaunchSpecification
    scheduledInstancesLaunchSpecification_ebsOptimized,
    scheduledInstancesLaunchSpecification_iamInstanceProfile,
    scheduledInstancesLaunchSpecification_placement,
    scheduledInstancesLaunchSpecification_userData,
    scheduledInstancesLaunchSpecification_blockDeviceMappings,
    scheduledInstancesLaunchSpecification_securityGroupIds,
    scheduledInstancesLaunchSpecification_monitoring,
    scheduledInstancesLaunchSpecification_subnetId,
    scheduledInstancesLaunchSpecification_instanceType,
    scheduledInstancesLaunchSpecification_ramdiskId,
    scheduledInstancesLaunchSpecification_keyName,
    scheduledInstancesLaunchSpecification_kernelId,
    scheduledInstancesLaunchSpecification_networkInterfaces,
    scheduledInstancesLaunchSpecification_imageId,

    -- ** ScheduledInstancesMonitoring
    scheduledInstancesMonitoring_enabled,

    -- ** ScheduledInstancesNetworkInterface
    scheduledInstancesNetworkInterface_associatePublicIpAddress,
    scheduledInstancesNetworkInterface_deleteOnTermination,
    scheduledInstancesNetworkInterface_privateIpAddressConfigs,
    scheduledInstancesNetworkInterface_subnetId,
    scheduledInstancesNetworkInterface_description,
    scheduledInstancesNetworkInterface_networkInterfaceId,
    scheduledInstancesNetworkInterface_ipv6AddressCount,
    scheduledInstancesNetworkInterface_privateIpAddress,
    scheduledInstancesNetworkInterface_secondaryPrivateIpAddressCount,
    scheduledInstancesNetworkInterface_groups,
    scheduledInstancesNetworkInterface_ipv6Addresses,
    scheduledInstancesNetworkInterface_deviceIndex,

    -- ** ScheduledInstancesPlacement
    scheduledInstancesPlacement_availabilityZone,
    scheduledInstancesPlacement_groupName,

    -- ** ScheduledInstancesPrivateIpAddressConfig
    scheduledInstancesPrivateIpAddressConfig_primary,
    scheduledInstancesPrivateIpAddressConfig_privateIpAddress,

    -- ** SecurityGroup
    securityGroup_tags,
    securityGroup_ipPermissionsEgress,
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
    securityGroupReference_referencingVpcId,
    securityGroupReference_groupId,

    -- ** SecurityGroupRule
    securityGroupRule_tags,
    securityGroupRule_toPort,
    securityGroupRule_ipProtocol,
    securityGroupRule_cidrIpv6,
    securityGroupRule_prefixListId,
    securityGroupRule_securityGroupRuleId,
    securityGroupRule_groupOwnerId,
    securityGroupRule_isEgress,
    securityGroupRule_referencedGroupInfo,
    securityGroupRule_cidrIpv4,
    securityGroupRule_description,
    securityGroupRule_fromPort,
    securityGroupRule_groupId,

    -- ** SecurityGroupRuleDescription
    securityGroupRuleDescription_securityGroupRuleId,
    securityGroupRuleDescription_description,

    -- ** SecurityGroupRuleRequest
    securityGroupRuleRequest_toPort,
    securityGroupRuleRequest_ipProtocol,
    securityGroupRuleRequest_cidrIpv6,
    securityGroupRuleRequest_prefixListId,
    securityGroupRuleRequest_cidrIpv4,
    securityGroupRuleRequest_description,
    securityGroupRuleRequest_referencedGroupId,
    securityGroupRuleRequest_fromPort,

    -- ** SecurityGroupRuleUpdate
    securityGroupRuleUpdate_securityGroupRuleId,
    securityGroupRuleUpdate_securityGroupRule,

    -- ** ServiceConfiguration
    serviceConfiguration_tags,
    serviceConfiguration_gatewayLoadBalancerArns,
    serviceConfiguration_acceptanceRequired,
    serviceConfiguration_supportedIpAddressTypes,
    serviceConfiguration_availabilityZones,
    serviceConfiguration_serviceType,
    serviceConfiguration_privateDnsNameConfiguration,
    serviceConfiguration_payerResponsibility,
    serviceConfiguration_managesVpcEndpoints,
    serviceConfiguration_baseEndpointDnsNames,
    serviceConfiguration_networkLoadBalancerArns,
    serviceConfiguration_privateDnsName,
    serviceConfiguration_serviceState,
    serviceConfiguration_serviceName,
    serviceConfiguration_serviceId,

    -- ** ServiceDetail
    serviceDetail_tags,
    serviceDetail_acceptanceRequired,
    serviceDetail_supportedIpAddressTypes,
    serviceDetail_availabilityZones,
    serviceDetail_serviceType,
    serviceDetail_owner,
    serviceDetail_payerResponsibility,
    serviceDetail_managesVpcEndpoints,
    serviceDetail_vpcEndpointPolicySupported,
    serviceDetail_baseEndpointDnsNames,
    serviceDetail_privateDnsNameVerificationState,
    serviceDetail_privateDnsNames,
    serviceDetail_privateDnsName,
    serviceDetail_serviceName,
    serviceDetail_serviceId,

    -- ** ServiceTypeDetail
    serviceTypeDetail_serviceType,

    -- ** SlotDateTimeRangeRequest
    slotDateTimeRangeRequest_earliestTime,
    slotDateTimeRangeRequest_latestTime,

    -- ** SlotStartTimeRangeRequest
    slotStartTimeRangeRequest_latestTime,
    slotStartTimeRangeRequest_earliestTime,

    -- ** Snapshot
    snapshot_tags,
    snapshot_ownerAlias,
    snapshot_outpostArn,
    snapshot_dataEncryptionKeyId,
    snapshot_restoreExpiryTime,
    snapshot_kmsKeyId,
    snapshot_stateMessage,
    snapshot_storageTier,
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
    snapshotDetail_progress,
    snapshotDetail_format,
    snapshotDetail_userBucket,
    snapshotDetail_deviceName,
    snapshotDetail_snapshotId,
    snapshotDetail_url,
    snapshotDetail_status,
    snapshotDetail_description,
    snapshotDetail_statusMessage,
    snapshotDetail_diskImageSize,

    -- ** SnapshotDiskContainer
    snapshotDiskContainer_format,
    snapshotDiskContainer_userBucket,
    snapshotDiskContainer_url,
    snapshotDiskContainer_description,

    -- ** SnapshotInfo
    snapshotInfo_tags,
    snapshotInfo_progress,
    snapshotInfo_outpostArn,
    snapshotInfo_ownerId,
    snapshotInfo_snapshotId,
    snapshotInfo_state,
    snapshotInfo_volumeSize,
    snapshotInfo_description,
    snapshotInfo_encrypted,
    snapshotInfo_volumeId,
    snapshotInfo_startTime,

    -- ** SnapshotRecycleBinInfo
    snapshotRecycleBinInfo_recycleBinEnterTime,
    snapshotRecycleBinInfo_snapshotId,
    snapshotRecycleBinInfo_description,
    snapshotRecycleBinInfo_recycleBinExitTime,
    snapshotRecycleBinInfo_volumeId,

    -- ** SnapshotTaskDetail
    snapshotTaskDetail_progress,
    snapshotTaskDetail_format,
    snapshotTaskDetail_userBucket,
    snapshotTaskDetail_snapshotId,
    snapshotTaskDetail_url,
    snapshotTaskDetail_status,
    snapshotTaskDetail_description,
    snapshotTaskDetail_encrypted,
    snapshotTaskDetail_kmsKeyId,
    snapshotTaskDetail_statusMessage,
    snapshotTaskDetail_diskImageSize,

    -- ** SnapshotTierStatus
    snapshotTierStatus_tags,
    snapshotTierStatus_ownerId,
    snapshotTierStatus_lastTieringOperationStatusDetail,
    snapshotTierStatus_snapshotId,
    snapshotTierStatus_status,
    snapshotTierStatus_restoreExpiryTime,
    snapshotTierStatus_lastTieringOperationStatus,
    snapshotTierStatus_archivalCompleteTime,
    snapshotTierStatus_volumeId,
    snapshotTierStatus_lastTieringProgress,
    snapshotTierStatus_lastTieringStartTime,
    snapshotTierStatus_storageTier,

    -- ** SpotCapacityRebalance
    spotCapacityRebalance_terminationDelay,
    spotCapacityRebalance_replacementStrategy,

    -- ** SpotDatafeedSubscription
    spotDatafeedSubscription_ownerId,
    spotDatafeedSubscription_bucket,
    spotDatafeedSubscription_state,
    spotDatafeedSubscription_fault,
    spotDatafeedSubscription_prefix,

    -- ** SpotFleetLaunchSpecification
    spotFleetLaunchSpecification_ebsOptimized,
    spotFleetLaunchSpecification_iamInstanceProfile,
    spotFleetLaunchSpecification_placement,
    spotFleetLaunchSpecification_userData,
    spotFleetLaunchSpecification_instanceRequirements,
    spotFleetLaunchSpecification_blockDeviceMappings,
    spotFleetLaunchSpecification_addressingType,
    spotFleetLaunchSpecification_monitoring,
    spotFleetLaunchSpecification_subnetId,
    spotFleetLaunchSpecification_instanceType,
    spotFleetLaunchSpecification_securityGroups,
    spotFleetLaunchSpecification_ramdiskId,
    spotFleetLaunchSpecification_weightedCapacity,
    spotFleetLaunchSpecification_keyName,
    spotFleetLaunchSpecification_kernelId,
    spotFleetLaunchSpecification_tagSpecifications,
    spotFleetLaunchSpecification_spotPrice,
    spotFleetLaunchSpecification_imageId,
    spotFleetLaunchSpecification_networkInterfaces,

    -- ** SpotFleetMonitoring
    spotFleetMonitoring_enabled,

    -- ** SpotFleetRequestConfig
    spotFleetRequestConfig_tags,
    spotFleetRequestConfig_spotFleetRequestState,
    spotFleetRequestConfig_activityStatus,
    spotFleetRequestConfig_spotFleetRequestConfig,
    spotFleetRequestConfig_spotFleetRequestId,
    spotFleetRequestConfig_createTime,

    -- ** SpotFleetRequestConfigData
    spotFleetRequestConfigData_excessCapacityTerminationPolicy,
    spotFleetRequestConfigData_clientToken,
    spotFleetRequestConfigData_type,
    spotFleetRequestConfigData_onDemandFulfilledCapacity,
    spotFleetRequestConfigData_spotMaintenanceStrategies,
    spotFleetRequestConfigData_onDemandAllocationStrategy,
    spotFleetRequestConfigData_instancePoolsToUseCount,
    spotFleetRequestConfigData_onDemandMaxTotalPrice,
    spotFleetRequestConfigData_context,
    spotFleetRequestConfigData_loadBalancersConfig,
    spotFleetRequestConfigData_fulfilledCapacity,
    spotFleetRequestConfigData_launchSpecifications,
    spotFleetRequestConfigData_validFrom,
    spotFleetRequestConfigData_replaceUnhealthyInstances,
    spotFleetRequestConfigData_launchTemplateConfigs,
    spotFleetRequestConfigData_targetCapacityUnitType,
    spotFleetRequestConfigData_instanceInterruptionBehavior,
    spotFleetRequestConfigData_allocationStrategy,
    spotFleetRequestConfigData_terminateInstancesWithExpiration,
    spotFleetRequestConfigData_onDemandTargetCapacity,
    spotFleetRequestConfigData_validUntil,
    spotFleetRequestConfigData_tagSpecifications,
    spotFleetRequestConfigData_spotMaxTotalPrice,
    spotFleetRequestConfigData_spotPrice,
    spotFleetRequestConfigData_iamFleetRole,
    spotFleetRequestConfigData_targetCapacity,

    -- ** SpotFleetTagSpecification
    spotFleetTagSpecification_tags,
    spotFleetTagSpecification_resourceType,

    -- ** SpotInstanceRequest
    spotInstanceRequest_tags,
    spotInstanceRequest_spotInstanceRequestId,
    spotInstanceRequest_launchedAvailabilityZone,
    spotInstanceRequest_type,
    spotInstanceRequest_blockDurationMinutes,
    spotInstanceRequest_availabilityZoneGroup,
    spotInstanceRequest_state,
    spotInstanceRequest_status,
    spotInstanceRequest_validFrom,
    spotInstanceRequest_instanceInterruptionBehavior,
    spotInstanceRequest_instanceId,
    spotInstanceRequest_fault,
    spotInstanceRequest_productDescription,
    spotInstanceRequest_validUntil,
    spotInstanceRequest_createTime,
    spotInstanceRequest_launchGroup,
    spotInstanceRequest_launchSpecification,
    spotInstanceRequest_actualBlockHourlyPrice,
    spotInstanceRequest_spotPrice,

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
    spotMarketOptions_maxPrice,
    spotMarketOptions_instanceInterruptionBehavior,
    spotMarketOptions_spotInstanceType,
    spotMarketOptions_validUntil,

    -- ** SpotOptions
    spotOptions_singleInstanceType,
    spotOptions_instancePoolsToUseCount,
    spotOptions_singleAvailabilityZone,
    spotOptions_minTargetCapacity,
    spotOptions_instanceInterruptionBehavior,
    spotOptions_maintenanceStrategies,
    spotOptions_allocationStrategy,
    spotOptions_maxTotalPrice,

    -- ** SpotOptionsRequest
    spotOptionsRequest_singleInstanceType,
    spotOptionsRequest_instancePoolsToUseCount,
    spotOptionsRequest_singleAvailabilityZone,
    spotOptionsRequest_minTargetCapacity,
    spotOptionsRequest_instanceInterruptionBehavior,
    spotOptionsRequest_maintenanceStrategies,
    spotOptionsRequest_allocationStrategy,
    spotOptionsRequest_maxTotalPrice,

    -- ** SpotPlacement
    spotPlacement_availabilityZone,
    spotPlacement_groupName,
    spotPlacement_tenancy,

    -- ** SpotPlacementScore
    spotPlacementScore_score,
    spotPlacementScore_region,
    spotPlacementScore_availabilityZoneId,

    -- ** SpotPrice
    spotPrice_timestamp,
    spotPrice_availabilityZone,
    spotPrice_instanceType,
    spotPrice_productDescription,
    spotPrice_spotPrice,

    -- ** StaleIpPermission
    staleIpPermission_toPort,
    staleIpPermission_ipProtocol,
    staleIpPermission_prefixListIds,
    staleIpPermission_ipRanges,
    staleIpPermission_userIdGroupPairs,
    staleIpPermission_fromPort,

    -- ** StaleSecurityGroup
    staleSecurityGroup_staleIpPermissions,
    staleSecurityGroup_groupName,
    staleSecurityGroup_description,
    staleSecurityGroup_staleIpPermissionsEgress,
    staleSecurityGroup_vpcId,
    staleSecurityGroup_groupId,

    -- ** StateReason
    stateReason_message,
    stateReason_code,

    -- ** Storage
    storage_s3,

    -- ** StorageLocation
    storageLocation_key,
    storageLocation_bucket,

    -- ** StoreImageTaskResult
    storeImageTaskResult_amiId,
    storeImageTaskResult_taskStartTime,
    storeImageTaskResult_bucket,
    storeImageTaskResult_storeTaskFailureReason,
    storeImageTaskResult_s3objectKey,
    storeImageTaskResult_storeTaskState,
    storeImageTaskResult_progressPercentage,

    -- ** Subnet
    subnet_tags,
    subnet_outpostArn,
    subnet_ownerId,
    subnet_mapPublicIpOnLaunch,
    subnet_ipv6Native,
    subnet_enableLniAtDeviceIndex,
    subnet_defaultForAz,
    subnet_customerOwnedIpv4Pool,
    subnet_mapCustomerOwnedIpOnLaunch,
    subnet_ipv6CidrBlockAssociationSet,
    subnet_privateDnsNameOptionsOnLaunch,
    subnet_subnetArn,
    subnet_assignIpv6AddressOnCreation,
    subnet_enableDns64,
    subnet_availabilityZoneId,
    subnet_availabilityZone,
    subnet_availableIpAddressCount,
    subnet_cidrBlock,
    subnet_state,
    subnet_subnetId,
    subnet_vpcId,

    -- ** SubnetAssociation
    subnetAssociation_subnetId,
    subnetAssociation_state,

    -- ** SubnetCidrBlockState
    subnetCidrBlockState_state,
    subnetCidrBlockState_statusMessage,

    -- ** SubnetCidrReservation
    subnetCidrReservation_tags,
    subnetCidrReservation_ownerId,
    subnetCidrReservation_cidr,
    subnetCidrReservation_reservationType,
    subnetCidrReservation_subnetId,
    subnetCidrReservation_subnetCidrReservationId,
    subnetCidrReservation_description,

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
    tagSpecification_tags,
    tagSpecification_resourceType,

    -- ** TargetCapacitySpecification
    targetCapacitySpecification_targetCapacityUnitType,
    targetCapacitySpecification_totalTargetCapacity,
    targetCapacitySpecification_defaultTargetCapacityType,
    targetCapacitySpecification_onDemandTargetCapacity,
    targetCapacitySpecification_spotTargetCapacity,

    -- ** TargetCapacitySpecificationRequest
    targetCapacitySpecificationRequest_targetCapacityUnitType,
    targetCapacitySpecificationRequest_defaultTargetCapacityType,
    targetCapacitySpecificationRequest_onDemandTargetCapacity,
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
    targetNetwork_status,
    targetNetwork_clientVpnEndpointId,
    targetNetwork_securityGroups,
    targetNetwork_vpcId,
    targetNetwork_associationId,
    targetNetwork_targetNetworkId,

    -- ** TargetReservationValue
    targetReservationValue_reservationValue,
    targetReservationValue_targetConfiguration,

    -- ** TerminateConnectionStatus
    terminateConnectionStatus_previousStatus,
    terminateConnectionStatus_connectionId,
    terminateConnectionStatus_currentStatus,

    -- ** ThroughResourcesStatement
    throughResourcesStatement_resourceStatement,

    -- ** ThroughResourcesStatementRequest
    throughResourcesStatementRequest_resourceStatement,

    -- ** TotalLocalStorageGB
    totalLocalStorageGB_max,
    totalLocalStorageGB_min,

    -- ** TotalLocalStorageGBRequest
    totalLocalStorageGBRequest_max,
    totalLocalStorageGBRequest_min,

    -- ** TrafficMirrorFilter
    trafficMirrorFilter_tags,
    trafficMirrorFilter_egressFilterRules,
    trafficMirrorFilter_ingressFilterRules,
    trafficMirrorFilter_trafficMirrorFilterId,
    trafficMirrorFilter_description,
    trafficMirrorFilter_networkServices,

    -- ** TrafficMirrorFilterRule
    trafficMirrorFilterRule_trafficMirrorFilterId,
    trafficMirrorFilterRule_ruleNumber,
    trafficMirrorFilterRule_description,
    trafficMirrorFilterRule_trafficDirection,
    trafficMirrorFilterRule_destinationCidrBlock,
    trafficMirrorFilterRule_destinationPortRange,
    trafficMirrorFilterRule_ruleAction,
    trafficMirrorFilterRule_trafficMirrorFilterRuleId,
    trafficMirrorFilterRule_sourceCidrBlock,
    trafficMirrorFilterRule_protocol,
    trafficMirrorFilterRule_sourcePortRange,

    -- ** TrafficMirrorPortRange
    trafficMirrorPortRange_toPort,
    trafficMirrorPortRange_fromPort,

    -- ** TrafficMirrorPortRangeRequest
    trafficMirrorPortRangeRequest_toPort,
    trafficMirrorPortRangeRequest_fromPort,

    -- ** TrafficMirrorSession
    trafficMirrorSession_tags,
    trafficMirrorSession_trafficMirrorSessionId,
    trafficMirrorSession_ownerId,
    trafficMirrorSession_sessionNumber,
    trafficMirrorSession_trafficMirrorTargetId,
    trafficMirrorSession_trafficMirrorFilterId,
    trafficMirrorSession_description,
    trafficMirrorSession_packetLength,
    trafficMirrorSession_networkInterfaceId,
    trafficMirrorSession_virtualNetworkId,

    -- ** TrafficMirrorTarget
    trafficMirrorTarget_tags,
    trafficMirrorTarget_type,
    trafficMirrorTarget_ownerId,
    trafficMirrorTarget_networkLoadBalancerArn,
    trafficMirrorTarget_trafficMirrorTargetId,
    trafficMirrorTarget_description,
    trafficMirrorTarget_networkInterfaceId,
    trafficMirrorTarget_gatewayLoadBalancerEndpointId,

    -- ** TransitGateway
    transitGateway_tags,
    transitGateway_ownerId,
    transitGateway_transitGatewayId,
    transitGateway_transitGatewayArn,
    transitGateway_state,
    transitGateway_description,
    transitGateway_options,
    transitGateway_creationTime,

    -- ** TransitGatewayAssociation
    transitGatewayAssociation_resourceId,
    transitGatewayAssociation_resourceType,
    transitGatewayAssociation_state,
    transitGatewayAssociation_transitGatewayAttachmentId,
    transitGatewayAssociation_transitGatewayRouteTableId,

    -- ** TransitGatewayAttachment
    transitGatewayAttachment_resourceId,
    transitGatewayAttachment_tags,
    transitGatewayAttachment_resourceType,
    transitGatewayAttachment_resourceOwnerId,
    transitGatewayAttachment_transitGatewayId,
    transitGatewayAttachment_state,
    transitGatewayAttachment_transitGatewayAttachmentId,
    transitGatewayAttachment_association,
    transitGatewayAttachment_creationTime,
    transitGatewayAttachment_transitGatewayOwnerId,

    -- ** TransitGatewayAttachmentAssociation
    transitGatewayAttachmentAssociation_state,
    transitGatewayAttachmentAssociation_transitGatewayRouteTableId,

    -- ** TransitGatewayAttachmentBgpConfiguration
    transitGatewayAttachmentBgpConfiguration_transitGatewayAsn,
    transitGatewayAttachmentBgpConfiguration_transitGatewayAddress,
    transitGatewayAttachmentBgpConfiguration_peerAsn,
    transitGatewayAttachmentBgpConfiguration_peerAddress,
    transitGatewayAttachmentBgpConfiguration_bgpStatus,

    -- ** TransitGatewayAttachmentPropagation
    transitGatewayAttachmentPropagation_state,
    transitGatewayAttachmentPropagation_transitGatewayRouteTableId,

    -- ** TransitGatewayConnect
    transitGatewayConnect_tags,
    transitGatewayConnect_transitGatewayId,
    transitGatewayConnect_state,
    transitGatewayConnect_transitGatewayAttachmentId,
    transitGatewayConnect_options,
    transitGatewayConnect_creationTime,
    transitGatewayConnect_transportTransitGatewayAttachmentId,

    -- ** TransitGatewayConnectOptions
    transitGatewayConnectOptions_protocol,

    -- ** TransitGatewayConnectPeer
    transitGatewayConnectPeer_tags,
    transitGatewayConnectPeer_transitGatewayConnectPeerId,
    transitGatewayConnectPeer_state,
    transitGatewayConnectPeer_transitGatewayAttachmentId,
    transitGatewayConnectPeer_connectPeerConfiguration,
    transitGatewayConnectPeer_creationTime,

    -- ** TransitGatewayConnectPeerConfiguration
    transitGatewayConnectPeerConfiguration_transitGatewayAddress,
    transitGatewayConnectPeerConfiguration_bgpConfigurations,
    transitGatewayConnectPeerConfiguration_insideCidrBlocks,
    transitGatewayConnectPeerConfiguration_peerAddress,
    transitGatewayConnectPeerConfiguration_protocol,

    -- ** TransitGatewayConnectRequestBgpOptions
    transitGatewayConnectRequestBgpOptions_peerAsn,

    -- ** TransitGatewayMulticastDeregisteredGroupMembers
    transitGatewayMulticastDeregisteredGroupMembers_deregisteredNetworkInterfaceIds,
    transitGatewayMulticastDeregisteredGroupMembers_groupIpAddress,
    transitGatewayMulticastDeregisteredGroupMembers_transitGatewayMulticastDomainId,

    -- ** TransitGatewayMulticastDeregisteredGroupSources
    transitGatewayMulticastDeregisteredGroupSources_deregisteredNetworkInterfaceIds,
    transitGatewayMulticastDeregisteredGroupSources_groupIpAddress,
    transitGatewayMulticastDeregisteredGroupSources_transitGatewayMulticastDomainId,

    -- ** TransitGatewayMulticastDomain
    transitGatewayMulticastDomain_tags,
    transitGatewayMulticastDomain_transitGatewayMulticastDomainArn,
    transitGatewayMulticastDomain_ownerId,
    transitGatewayMulticastDomain_transitGatewayId,
    transitGatewayMulticastDomain_state,
    transitGatewayMulticastDomain_options,
    transitGatewayMulticastDomain_creationTime,
    transitGatewayMulticastDomain_transitGatewayMulticastDomainId,

    -- ** TransitGatewayMulticastDomainAssociation
    transitGatewayMulticastDomainAssociation_resourceId,
    transitGatewayMulticastDomainAssociation_resourceType,
    transitGatewayMulticastDomainAssociation_resourceOwnerId,
    transitGatewayMulticastDomainAssociation_transitGatewayAttachmentId,
    transitGatewayMulticastDomainAssociation_subnet,

    -- ** TransitGatewayMulticastDomainAssociations
    transitGatewayMulticastDomainAssociations_resourceId,
    transitGatewayMulticastDomainAssociations_resourceType,
    transitGatewayMulticastDomainAssociations_resourceOwnerId,
    transitGatewayMulticastDomainAssociations_subnets,
    transitGatewayMulticastDomainAssociations_transitGatewayAttachmentId,
    transitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId,

    -- ** TransitGatewayMulticastDomainOptions
    transitGatewayMulticastDomainOptions_staticSourcesSupport,
    transitGatewayMulticastDomainOptions_igmpv2Support,
    transitGatewayMulticastDomainOptions_autoAcceptSharedAssociations,

    -- ** TransitGatewayMulticastGroup
    transitGatewayMulticastGroup_resourceId,
    transitGatewayMulticastGroup_resourceType,
    transitGatewayMulticastGroup_resourceOwnerId,
    transitGatewayMulticastGroup_memberType,
    transitGatewayMulticastGroup_subnetId,
    transitGatewayMulticastGroup_groupIpAddress,
    transitGatewayMulticastGroup_transitGatewayAttachmentId,
    transitGatewayMulticastGroup_sourceType,
    transitGatewayMulticastGroup_networkInterfaceId,
    transitGatewayMulticastGroup_groupMember,
    transitGatewayMulticastGroup_groupSource,

    -- ** TransitGatewayMulticastRegisteredGroupMembers
    transitGatewayMulticastRegisteredGroupMembers_groupIpAddress,
    transitGatewayMulticastRegisteredGroupMembers_registeredNetworkInterfaceIds,
    transitGatewayMulticastRegisteredGroupMembers_transitGatewayMulticastDomainId,

    -- ** TransitGatewayMulticastRegisteredGroupSources
    transitGatewayMulticastRegisteredGroupSources_groupIpAddress,
    transitGatewayMulticastRegisteredGroupSources_registeredNetworkInterfaceIds,
    transitGatewayMulticastRegisteredGroupSources_transitGatewayMulticastDomainId,

    -- ** TransitGatewayOptions
    transitGatewayOptions_associationDefaultRouteTableId,
    transitGatewayOptions_dnsSupport,
    transitGatewayOptions_defaultRouteTableAssociation,
    transitGatewayOptions_propagationDefaultRouteTableId,
    transitGatewayOptions_autoAcceptSharedAttachments,
    transitGatewayOptions_multicastSupport,
    transitGatewayOptions_amazonSideAsn,
    transitGatewayOptions_vpnEcmpSupport,
    transitGatewayOptions_defaultRouteTablePropagation,
    transitGatewayOptions_transitGatewayCidrBlocks,

    -- ** TransitGatewayPeeringAttachment
    transitGatewayPeeringAttachment_tags,
    transitGatewayPeeringAttachment_accepterTgwInfo,
    transitGatewayPeeringAttachment_state,
    transitGatewayPeeringAttachment_transitGatewayAttachmentId,
    transitGatewayPeeringAttachment_status,
    transitGatewayPeeringAttachment_options,
    transitGatewayPeeringAttachment_creationTime,
    transitGatewayPeeringAttachment_accepterTransitGatewayAttachmentId,
    transitGatewayPeeringAttachment_requesterTgwInfo,

    -- ** TransitGatewayPeeringAttachmentOptions
    transitGatewayPeeringAttachmentOptions_dynamicRouting,

    -- ** TransitGatewayPolicyRule
    transitGatewayPolicyRule_metaData,
    transitGatewayPolicyRule_destinationCidrBlock,
    transitGatewayPolicyRule_destinationPortRange,
    transitGatewayPolicyRule_sourceCidrBlock,
    transitGatewayPolicyRule_protocol,
    transitGatewayPolicyRule_sourcePortRange,

    -- ** TransitGatewayPolicyRuleMetaData
    transitGatewayPolicyRuleMetaData_metaDataValue,
    transitGatewayPolicyRuleMetaData_metaDataKey,

    -- ** TransitGatewayPolicyTable
    transitGatewayPolicyTable_tags,
    transitGatewayPolicyTable_transitGatewayId,
    transitGatewayPolicyTable_transitGatewayPolicyTableId,
    transitGatewayPolicyTable_state,
    transitGatewayPolicyTable_creationTime,

    -- ** TransitGatewayPolicyTableAssociation
    transitGatewayPolicyTableAssociation_resourceId,
    transitGatewayPolicyTableAssociation_resourceType,
    transitGatewayPolicyTableAssociation_transitGatewayPolicyTableId,
    transitGatewayPolicyTableAssociation_state,
    transitGatewayPolicyTableAssociation_transitGatewayAttachmentId,

    -- ** TransitGatewayPolicyTableEntry
    transitGatewayPolicyTableEntry_policyRuleNumber,
    transitGatewayPolicyTableEntry_policyRule,
    transitGatewayPolicyTableEntry_targetRouteTableId,

    -- ** TransitGatewayPrefixListAttachment
    transitGatewayPrefixListAttachment_resourceId,
    transitGatewayPrefixListAttachment_resourceType,
    transitGatewayPrefixListAttachment_transitGatewayAttachmentId,

    -- ** TransitGatewayPrefixListReference
    transitGatewayPrefixListReference_transitGatewayAttachment,
    transitGatewayPrefixListReference_prefixListId,
    transitGatewayPrefixListReference_state,
    transitGatewayPrefixListReference_prefixListOwnerId,
    transitGatewayPrefixListReference_blackhole,
    transitGatewayPrefixListReference_transitGatewayRouteTableId,

    -- ** TransitGatewayPropagation
    transitGatewayPropagation_resourceId,
    transitGatewayPropagation_resourceType,
    transitGatewayPropagation_state,
    transitGatewayPropagation_transitGatewayAttachmentId,
    transitGatewayPropagation_transitGatewayRouteTableAnnouncementId,
    transitGatewayPropagation_transitGatewayRouteTableId,

    -- ** TransitGatewayRequestOptions
    transitGatewayRequestOptions_dnsSupport,
    transitGatewayRequestOptions_defaultRouteTableAssociation,
    transitGatewayRequestOptions_autoAcceptSharedAttachments,
    transitGatewayRequestOptions_multicastSupport,
    transitGatewayRequestOptions_amazonSideAsn,
    transitGatewayRequestOptions_vpnEcmpSupport,
    transitGatewayRequestOptions_defaultRouteTablePropagation,
    transitGatewayRequestOptions_transitGatewayCidrBlocks,

    -- ** TransitGatewayRoute
    transitGatewayRoute_type,
    transitGatewayRoute_prefixListId,
    transitGatewayRoute_state,
    transitGatewayRoute_destinationCidrBlock,
    transitGatewayRoute_transitGatewayAttachments,
    transitGatewayRoute_transitGatewayRouteTableAnnouncementId,

    -- ** TransitGatewayRouteAttachment
    transitGatewayRouteAttachment_resourceId,
    transitGatewayRouteAttachment_resourceType,
    transitGatewayRouteAttachment_transitGatewayAttachmentId,

    -- ** TransitGatewayRouteTable
    transitGatewayRouteTable_tags,
    transitGatewayRouteTable_transitGatewayId,
    transitGatewayRouteTable_state,
    transitGatewayRouteTable_defaultAssociationRouteTable,
    transitGatewayRouteTable_defaultPropagationRouteTable,
    transitGatewayRouteTable_creationTime,
    transitGatewayRouteTable_transitGatewayRouteTableId,

    -- ** TransitGatewayRouteTableAnnouncement
    transitGatewayRouteTableAnnouncement_tags,
    transitGatewayRouteTableAnnouncement_peerCoreNetworkId,
    transitGatewayRouteTableAnnouncement_coreNetworkId,
    transitGatewayRouteTableAnnouncement_peerTransitGatewayId,
    transitGatewayRouteTableAnnouncement_transitGatewayId,
    transitGatewayRouteTableAnnouncement_state,
    transitGatewayRouteTableAnnouncement_announcementDirection,
    transitGatewayRouteTableAnnouncement_creationTime,
    transitGatewayRouteTableAnnouncement_transitGatewayRouteTableAnnouncementId,
    transitGatewayRouteTableAnnouncement_transitGatewayRouteTableId,
    transitGatewayRouteTableAnnouncement_peeringAttachmentId,

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
    transitGatewayRouteTablePropagation_transitGatewayRouteTableAnnouncementId,

    -- ** TransitGatewayRouteTableRoute
    transitGatewayRouteTableRoute_resourceId,
    transitGatewayRouteTableRoute_resourceType,
    transitGatewayRouteTableRoute_destinationCidr,
    transitGatewayRouteTableRoute_prefixListId,
    transitGatewayRouteTableRoute_state,
    transitGatewayRouteTableRoute_attachmentId,
    transitGatewayRouteTableRoute_routeOrigin,

    -- ** TransitGatewayVpcAttachment
    transitGatewayVpcAttachment_tags,
    transitGatewayVpcAttachment_transitGatewayId,
    transitGatewayVpcAttachment_vpcOwnerId,
    transitGatewayVpcAttachment_state,
    transitGatewayVpcAttachment_transitGatewayAttachmentId,
    transitGatewayVpcAttachment_options,
    transitGatewayVpcAttachment_creationTime,
    transitGatewayVpcAttachment_vpcId,
    transitGatewayVpcAttachment_subnetIds,

    -- ** TransitGatewayVpcAttachmentOptions
    transitGatewayVpcAttachmentOptions_dnsSupport,
    transitGatewayVpcAttachmentOptions_ipv6Support,
    transitGatewayVpcAttachmentOptions_applianceModeSupport,

    -- ** TrunkInterfaceAssociation
    trunkInterfaceAssociation_tags,
    trunkInterfaceAssociation_trunkInterfaceId,
    trunkInterfaceAssociation_branchInterfaceId,
    trunkInterfaceAssociation_vlanId,
    trunkInterfaceAssociation_interfaceProtocol,
    trunkInterfaceAssociation_greKey,
    trunkInterfaceAssociation_associationId,

    -- ** TunnelOption
    tunnelOption_tunnelInsideIpv6Cidr,
    tunnelOption_phase1LifetimeSeconds,
    tunnelOption_phase2LifetimeSeconds,
    tunnelOption_phase2EncryptionAlgorithms,
    tunnelOption_phase1DHGroupNumbers,
    tunnelOption_phase1IntegrityAlgorithms,
    tunnelOption_dpdTimeoutSeconds,
    tunnelOption_ikeVersions,
    tunnelOption_preSharedKey,
    tunnelOption_dpdTimeoutAction,
    tunnelOption_logOptions,
    tunnelOption_phase2DHGroupNumbers,
    tunnelOption_rekeyFuzzPercentage,
    tunnelOption_startupAction,
    tunnelOption_rekeyMarginTimeSeconds,
    tunnelOption_phase2IntegrityAlgorithms,
    tunnelOption_phase1EncryptionAlgorithms,
    tunnelOption_tunnelInsideCidr,
    tunnelOption_outsideIpAddress,
    tunnelOption_replayWindowSize,

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
    userIdGroupPair_description,
    userIdGroupPair_peeringStatus,
    userIdGroupPair_userId,
    userIdGroupPair_vpcId,
    userIdGroupPair_groupId,

    -- ** VCpuCountRange
    vCpuCountRange_max,
    vCpuCountRange_min,

    -- ** VCpuCountRangeRequest
    vCpuCountRangeRequest_max,
    vCpuCountRangeRequest_min,

    -- ** VCpuInfo
    vCpuInfo_validThreadsPerCore,
    vCpuInfo_defaultCores,
    vCpuInfo_defaultVCpus,
    vCpuInfo_validCores,
    vCpuInfo_defaultThreadsPerCore,

    -- ** ValidationError
    validationError_message,
    validationError_code,

    -- ** ValidationWarning
    validationWarning_errors,

    -- ** VgwTelemetry
    vgwTelemetry_acceptedRouteCount,
    vgwTelemetry_status,
    vgwTelemetry_lastStatusChange,
    vgwTelemetry_certificateArn,
    vgwTelemetry_statusMessage,
    vgwTelemetry_outsideIpAddress,

    -- ** Volume
    volume_tags,
    volume_outpostArn,
    volume_attachments,
    volume_kmsKeyId,
    volume_fastRestored,
    volume_throughput,
    volume_multiAttachEnabled,
    volume_iops,
    volume_availabilityZone,
    volume_createTime,
    volume_encrypted,
    volume_size,
    volume_snapshotId,
    volume_state,
    volume_volumeId,
    volume_volumeType,

    -- ** VolumeAttachment
    volumeAttachment_deleteOnTermination,
    volumeAttachment_device,
    volumeAttachment_state,
    volumeAttachment_instanceId,
    volumeAttachment_attachTime,
    volumeAttachment_volumeId,

    -- ** VolumeDetail
    volumeDetail_size,

    -- ** VolumeModification
    volumeModification_progress,
    volumeModification_targetThroughput,
    volumeModification_targetSize,
    volumeModification_originalVolumeType,
    volumeModification_originalMultiAttachEnabled,
    volumeModification_endTime,
    volumeModification_originalIops,
    volumeModification_modificationState,
    volumeModification_originalThroughput,
    volumeModification_volumeId,
    volumeModification_targetMultiAttachEnabled,
    volumeModification_targetIops,
    volumeModification_statusMessage,
    volumeModification_targetVolumeType,
    volumeModification_startTime,
    volumeModification_originalSize,

    -- ** VolumeStatusAction
    volumeStatusAction_eventType,
    volumeStatusAction_code,
    volumeStatusAction_description,
    volumeStatusAction_eventId,

    -- ** VolumeStatusAttachmentStatus
    volumeStatusAttachmentStatus_ioPerformance,
    volumeStatusAttachmentStatus_instanceId,

    -- ** VolumeStatusDetails
    volumeStatusDetails_name,
    volumeStatusDetails_status,

    -- ** VolumeStatusEvent
    volumeStatusEvent_eventType,
    volumeStatusEvent_description,
    volumeStatusEvent_instanceId,
    volumeStatusEvent_notBefore,
    volumeStatusEvent_eventId,
    volumeStatusEvent_notAfter,

    -- ** VolumeStatusInfo
    volumeStatusInfo_status,
    volumeStatusInfo_details,

    -- ** VolumeStatusItem
    volumeStatusItem_outpostArn,
    volumeStatusItem_volumeStatus,
    volumeStatusItem_availabilityZone,
    volumeStatusItem_attachmentStatuses,
    volumeStatusItem_events,
    volumeStatusItem_volumeId,
    volumeStatusItem_actions,

    -- ** Vpc
    vpc_tags,
    vpc_ownerId,
    vpc_ipv6CidrBlockAssociationSet,
    vpc_isDefault,
    vpc_cidrBlockAssociationSet,
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
    vpcCidrBlockAssociation_cidrBlock,
    vpcCidrBlockAssociation_associationId,

    -- ** VpcCidrBlockState
    vpcCidrBlockState_state,
    vpcCidrBlockState_statusMessage,

    -- ** VpcClassicLink
    vpcClassicLink_tags,
    vpcClassicLink_classicLinkEnabled,
    vpcClassicLink_vpcId,

    -- ** VpcEndpoint
    vpcEndpoint_tags,
    vpcEndpoint_privateDnsEnabled,
    vpcEndpoint_ownerId,
    vpcEndpoint_requesterManaged,
    vpcEndpoint_vpcEndpointType,
    vpcEndpoint_state,
    vpcEndpoint_routeTableIds,
    vpcEndpoint_vpcEndpointId,
    vpcEndpoint_creationTimestamp,
    vpcEndpoint_lastError,
    vpcEndpoint_policyDocument,
    vpcEndpoint_dnsEntries,
    vpcEndpoint_vpcId,
    vpcEndpoint_serviceName,
    vpcEndpoint_dnsOptions,
    vpcEndpoint_networkInterfaceIds,
    vpcEndpoint_ipAddressType,
    vpcEndpoint_subnetIds,
    vpcEndpoint_groups,

    -- ** VpcEndpointConnection
    vpcEndpointConnection_tags,
    vpcEndpointConnection_gatewayLoadBalancerArns,
    vpcEndpointConnection_vpcEndpointOwner,
    vpcEndpointConnection_vpcEndpointId,
    vpcEndpointConnection_creationTimestamp,
    vpcEndpointConnection_dnsEntries,
    vpcEndpointConnection_networkLoadBalancerArns,
    vpcEndpointConnection_ipAddressType,
    vpcEndpointConnection_vpcEndpointState,
    vpcEndpointConnection_serviceId,
    vpcEndpointConnection_vpcEndpointConnectionId,

    -- ** VpcIpv6CidrBlockAssociation
    vpcIpv6CidrBlockAssociation_networkBorderGroup,
    vpcIpv6CidrBlockAssociation_ipv6Pool,
    vpcIpv6CidrBlockAssociation_ipv6CidrBlockState,
    vpcIpv6CidrBlockAssociation_associationId,
    vpcIpv6CidrBlockAssociation_ipv6CidrBlock,

    -- ** VpcPeeringConnection
    vpcPeeringConnection_tags,
    vpcPeeringConnection_requesterVpcInfo,
    vpcPeeringConnection_expirationTime,
    vpcPeeringConnection_accepterVpcInfo,
    vpcPeeringConnection_vpcPeeringConnectionId,
    vpcPeeringConnection_status,

    -- ** VpcPeeringConnectionOptionsDescription
    vpcPeeringConnectionOptionsDescription_allowDnsResolutionFromRemoteVpc,
    vpcPeeringConnectionOptionsDescription_allowEgressFromLocalVpcToRemoteClassicLink,
    vpcPeeringConnectionOptionsDescription_allowEgressFromLocalClassicLinkToRemoteVpc,

    -- ** VpcPeeringConnectionStateReason
    vpcPeeringConnectionStateReason_message,
    vpcPeeringConnectionStateReason_code,

    -- ** VpcPeeringConnectionVpcInfo
    vpcPeeringConnectionVpcInfo_ownerId,
    vpcPeeringConnectionVpcInfo_ipv6CidrBlockSet,
    vpcPeeringConnectionVpcInfo_peeringOptions,
    vpcPeeringConnectionVpcInfo_region,
    vpcPeeringConnectionVpcInfo_cidrBlockSet,
    vpcPeeringConnectionVpcInfo_cidrBlock,
    vpcPeeringConnectionVpcInfo_vpcId,

    -- ** VpnConnection
    vpnConnection_tags,
    vpnConnection_transitGatewayId,
    vpnConnection_customerGatewayConfiguration,
    vpnConnection_gatewayAssociationState,
    vpnConnection_coreNetworkAttachmentArn,
    vpnConnection_options,
    vpnConnection_coreNetworkArn,
    vpnConnection_vpnGatewayId,
    vpnConnection_category,
    vpnConnection_vgwTelemetry,
    vpnConnection_routes,
    vpnConnection_vpnConnectionId,
    vpnConnection_customerGatewayId,
    vpnConnection_state,
    vpnConnection_type,

    -- ** VpnConnectionDeviceType
    vpnConnectionDeviceType_software,
    vpnConnectionDeviceType_platform,
    vpnConnectionDeviceType_vpnConnectionDeviceTypeId,
    vpnConnectionDeviceType_vendor,

    -- ** VpnConnectionOptions
    vpnConnectionOptions_outsideIpAddressType,
    vpnConnectionOptions_remoteIpv6NetworkCidr,
    vpnConnectionOptions_localIpv4NetworkCidr,
    vpnConnectionOptions_tunnelOptions,
    vpnConnectionOptions_staticRoutesOnly,
    vpnConnectionOptions_remoteIpv4NetworkCidr,
    vpnConnectionOptions_tunnelInsideIpVersion,
    vpnConnectionOptions_transportTransitGatewayAttachmentId,
    vpnConnectionOptions_enableAcceleration,
    vpnConnectionOptions_localIpv6NetworkCidr,

    -- ** VpnConnectionOptionsSpecification
    vpnConnectionOptionsSpecification_outsideIpAddressType,
    vpnConnectionOptionsSpecification_remoteIpv6NetworkCidr,
    vpnConnectionOptionsSpecification_localIpv4NetworkCidr,
    vpnConnectionOptionsSpecification_tunnelOptions,
    vpnConnectionOptionsSpecification_staticRoutesOnly,
    vpnConnectionOptionsSpecification_remoteIpv4NetworkCidr,
    vpnConnectionOptionsSpecification_tunnelInsideIpVersion,
    vpnConnectionOptionsSpecification_transportTransitGatewayAttachmentId,
    vpnConnectionOptionsSpecification_enableAcceleration,
    vpnConnectionOptionsSpecification_localIpv6NetworkCidr,

    -- ** VpnGateway
    vpnGateway_tags,
    vpnGateway_type,
    vpnGateway_state,
    vpnGateway_availabilityZone,
    vpnGateway_vpnGatewayId,
    vpnGateway_vpcAttachments,
    vpnGateway_amazonSideAsn,

    -- ** VpnStaticRoute
    vpnStaticRoute_state,
    vpnStaticRoute_destinationCidrBlock,
    vpnStaticRoute_source,

    -- ** VpnTunnelLogOptions
    vpnTunnelLogOptions_cloudWatchLogOptions,

    -- ** VpnTunnelLogOptionsSpecification
    vpnTunnelLogOptionsSpecification_cloudWatchLogOptions,

    -- ** VpnTunnelOptionsSpecification
    vpnTunnelOptionsSpecification_tunnelInsideIpv6Cidr,
    vpnTunnelOptionsSpecification_phase1LifetimeSeconds,
    vpnTunnelOptionsSpecification_phase2LifetimeSeconds,
    vpnTunnelOptionsSpecification_phase2EncryptionAlgorithms,
    vpnTunnelOptionsSpecification_phase1DHGroupNumbers,
    vpnTunnelOptionsSpecification_phase1IntegrityAlgorithms,
    vpnTunnelOptionsSpecification_dPDTimeoutSeconds,
    vpnTunnelOptionsSpecification_iKEVersions,
    vpnTunnelOptionsSpecification_preSharedKey,
    vpnTunnelOptionsSpecification_dPDTimeoutAction,
    vpnTunnelOptionsSpecification_logOptions,
    vpnTunnelOptionsSpecification_phase2DHGroupNumbers,
    vpnTunnelOptionsSpecification_rekeyFuzzPercentage,
    vpnTunnelOptionsSpecification_startupAction,
    vpnTunnelOptionsSpecification_rekeyMarginTimeSeconds,
    vpnTunnelOptionsSpecification_phase2IntegrityAlgorithms,
    vpnTunnelOptionsSpecification_phase1EncryptionAlgorithms,
    vpnTunnelOptionsSpecification_tunnelInsideCidr,
    vpnTunnelOptionsSpecification_replayWindowSize,
  )
where

import Amazonka.EC2.AcceptAddressTransfer
import Amazonka.EC2.AcceptReservedInstancesExchangeQuote
import Amazonka.EC2.AcceptTransitGatewayMulticastDomainAssociations
import Amazonka.EC2.AcceptTransitGatewayPeeringAttachment
import Amazonka.EC2.AcceptTransitGatewayVpcAttachment
import Amazonka.EC2.AcceptVpcEndpointConnections
import Amazonka.EC2.AcceptVpcPeeringConnection
import Amazonka.EC2.AdvertiseByoipCidr
import Amazonka.EC2.AllocateAddress
import Amazonka.EC2.AllocateHosts
import Amazonka.EC2.AllocateIpamPoolCidr
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
import Amazonka.EC2.AssociateTransitGatewayPolicyTable
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
import Amazonka.EC2.CancelImageLaunchPermission
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
import Amazonka.EC2.CreateCoipCidr
import Amazonka.EC2.CreateCoipPool
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
import Amazonka.EC2.CreateIpam
import Amazonka.EC2.CreateIpamPool
import Amazonka.EC2.CreateIpamScope
import Amazonka.EC2.CreateKeyPair
import Amazonka.EC2.CreateLaunchTemplate
import Amazonka.EC2.CreateLaunchTemplateVersion
import Amazonka.EC2.CreateLocalGatewayRoute
import Amazonka.EC2.CreateLocalGatewayRouteTable
import Amazonka.EC2.CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation
import Amazonka.EC2.CreateLocalGatewayRouteTableVpcAssociation
import Amazonka.EC2.CreateManagedPrefixList
import Amazonka.EC2.CreateNatGateway
import Amazonka.EC2.CreateNetworkAcl
import Amazonka.EC2.CreateNetworkAclEntry
import Amazonka.EC2.CreateNetworkInsightsAccessScope
import Amazonka.EC2.CreateNetworkInsightsPath
import Amazonka.EC2.CreateNetworkInterface
import Amazonka.EC2.CreateNetworkInterfacePermission
import Amazonka.EC2.CreatePlacementGroup
import Amazonka.EC2.CreatePublicIpv4Pool
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
import Amazonka.EC2.CreateTransitGatewayPolicyTable
import Amazonka.EC2.CreateTransitGatewayPrefixListReference
import Amazonka.EC2.CreateTransitGatewayRoute
import Amazonka.EC2.CreateTransitGatewayRouteTable
import Amazonka.EC2.CreateTransitGatewayRouteTableAnnouncement
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
import Amazonka.EC2.DeleteCoipCidr
import Amazonka.EC2.DeleteCoipPool
import Amazonka.EC2.DeleteCustomerGateway
import Amazonka.EC2.DeleteDhcpOptions
import Amazonka.EC2.DeleteEgressOnlyInternetGateway
import Amazonka.EC2.DeleteFleets
import Amazonka.EC2.DeleteFlowLogs
import Amazonka.EC2.DeleteFpgaImage
import Amazonka.EC2.DeleteInstanceEventWindow
import Amazonka.EC2.DeleteInternetGateway
import Amazonka.EC2.DeleteIpam
import Amazonka.EC2.DeleteIpamPool
import Amazonka.EC2.DeleteIpamScope
import Amazonka.EC2.DeleteKeyPair
import Amazonka.EC2.DeleteLaunchTemplate
import Amazonka.EC2.DeleteLaunchTemplateVersions
import Amazonka.EC2.DeleteLocalGatewayRoute
import Amazonka.EC2.DeleteLocalGatewayRouteTable
import Amazonka.EC2.DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation
import Amazonka.EC2.DeleteLocalGatewayRouteTableVpcAssociation
import Amazonka.EC2.DeleteManagedPrefixList
import Amazonka.EC2.DeleteNatGateway
import Amazonka.EC2.DeleteNetworkAcl
import Amazonka.EC2.DeleteNetworkAclEntry
import Amazonka.EC2.DeleteNetworkInsightsAccessScope
import Amazonka.EC2.DeleteNetworkInsightsAccessScopeAnalysis
import Amazonka.EC2.DeleteNetworkInsightsAnalysis
import Amazonka.EC2.DeleteNetworkInsightsPath
import Amazonka.EC2.DeleteNetworkInterface
import Amazonka.EC2.DeleteNetworkInterfacePermission
import Amazonka.EC2.DeletePlacementGroup
import Amazonka.EC2.DeletePublicIpv4Pool
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
import Amazonka.EC2.DeleteTransitGatewayPolicyTable
import Amazonka.EC2.DeleteTransitGatewayPrefixListReference
import Amazonka.EC2.DeleteTransitGatewayRoute
import Amazonka.EC2.DeleteTransitGatewayRouteTable
import Amazonka.EC2.DeleteTransitGatewayRouteTableAnnouncement
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
import Amazonka.EC2.DeprovisionIpamPoolCidr
import Amazonka.EC2.DeprovisionPublicIpv4PoolCidr
import Amazonka.EC2.DeregisterImage
import Amazonka.EC2.DeregisterInstanceEventNotificationAttributes
import Amazonka.EC2.DeregisterTransitGatewayMulticastGroupMembers
import Amazonka.EC2.DeregisterTransitGatewayMulticastGroupSources
import Amazonka.EC2.DescribeAccountAttributes
import Amazonka.EC2.DescribeAddressTransfers
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
import Amazonka.EC2.DescribeFastLaunchImages
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
import Amazonka.EC2.DescribeIpamPools
import Amazonka.EC2.DescribeIpamScopes
import Amazonka.EC2.DescribeIpams
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
import Amazonka.EC2.DescribeNetworkInsightsAccessScopeAnalyses
import Amazonka.EC2.DescribeNetworkInsightsAccessScopes
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
import Amazonka.EC2.DescribeSnapshotTierStatus
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
import Amazonka.EC2.DescribeTransitGatewayPolicyTables
import Amazonka.EC2.DescribeTransitGatewayRouteTableAnnouncements
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
import Amazonka.EC2.DisableAddressTransfer
import Amazonka.EC2.DisableEbsEncryptionByDefault
import Amazonka.EC2.DisableFastLaunch
import Amazonka.EC2.DisableFastSnapshotRestores
import Amazonka.EC2.DisableImageDeprecation
import Amazonka.EC2.DisableIpamOrganizationAdminAccount
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
import Amazonka.EC2.DisassociateTransitGatewayPolicyTable
import Amazonka.EC2.DisassociateTransitGatewayRouteTable
import Amazonka.EC2.DisassociateTrunkInterface
import Amazonka.EC2.DisassociateVpcCidrBlock
import Amazonka.EC2.EnableAddressTransfer
import Amazonka.EC2.EnableEbsEncryptionByDefault
import Amazonka.EC2.EnableFastLaunch
import Amazonka.EC2.EnableFastSnapshotRestores
import Amazonka.EC2.EnableImageDeprecation
import Amazonka.EC2.EnableIpamOrganizationAdminAccount
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
import Amazonka.EC2.GetInstanceTypesFromInstanceRequirements
import Amazonka.EC2.GetInstanceUefiData
import Amazonka.EC2.GetIpamAddressHistory
import Amazonka.EC2.GetIpamPoolAllocations
import Amazonka.EC2.GetIpamPoolCidrs
import Amazonka.EC2.GetIpamResourceCidrs
import Amazonka.EC2.GetLaunchTemplateData
import Amazonka.EC2.GetManagedPrefixListAssociations
import Amazonka.EC2.GetManagedPrefixListEntries
import Amazonka.EC2.GetNetworkInsightsAccessScopeAnalysisFindings
import Amazonka.EC2.GetNetworkInsightsAccessScopeContent
import Amazonka.EC2.GetPasswordData
import Amazonka.EC2.GetReservedInstancesExchangeQuote
import Amazonka.EC2.GetSerialConsoleAccessStatus
import Amazonka.EC2.GetSpotPlacementScores
import Amazonka.EC2.GetSubnetCidrReservations
import Amazonka.EC2.GetTransitGatewayAttachmentPropagations
import Amazonka.EC2.GetTransitGatewayMulticastDomainAssociations
import Amazonka.EC2.GetTransitGatewayPolicyTableAssociations
import Amazonka.EC2.GetTransitGatewayPolicyTableEntries
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
import Amazonka.EC2.ListImagesInRecycleBin
import Amazonka.EC2.ListSnapshotsInRecycleBin
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
import Amazonka.EC2.ModifyInstanceMaintenanceOptions
import Amazonka.EC2.ModifyInstanceMetadataOptions
import Amazonka.EC2.ModifyInstancePlacement
import Amazonka.EC2.ModifyIpam
import Amazonka.EC2.ModifyIpamPool
import Amazonka.EC2.ModifyIpamResourceCidr
import Amazonka.EC2.ModifyIpamScope
import Amazonka.EC2.ModifyLaunchTemplate
import Amazonka.EC2.ModifyLocalGatewayRoute
import Amazonka.EC2.ModifyManagedPrefixList
import Amazonka.EC2.ModifyNetworkInterfaceAttribute
import Amazonka.EC2.ModifyPrivateDnsNameOptions
import Amazonka.EC2.ModifyReservedInstances
import Amazonka.EC2.ModifySecurityGroupRules
import Amazonka.EC2.ModifySnapshotAttribute
import Amazonka.EC2.ModifySnapshotTier
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
import Amazonka.EC2.ModifyVpcEndpointServicePayerResponsibility
import Amazonka.EC2.ModifyVpcEndpointServicePermissions
import Amazonka.EC2.ModifyVpcPeeringConnectionOptions
import Amazonka.EC2.ModifyVpcTenancy
import Amazonka.EC2.ModifyVpnConnection
import Amazonka.EC2.ModifyVpnConnectionOptions
import Amazonka.EC2.ModifyVpnTunnelCertificate
import Amazonka.EC2.ModifyVpnTunnelOptions
import Amazonka.EC2.MonitorInstances
import Amazonka.EC2.MoveAddressToVpc
import Amazonka.EC2.MoveByoipCidrToIpam
import Amazonka.EC2.ProvisionByoipCidr
import Amazonka.EC2.ProvisionIpamPoolCidr
import Amazonka.EC2.ProvisionPublicIpv4PoolCidr
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
import Amazonka.EC2.ReleaseIpamPoolAllocation
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
import Amazonka.EC2.RestoreImageFromRecycleBin
import Amazonka.EC2.RestoreManagedPrefixListVersion
import Amazonka.EC2.RestoreSnapshotFromRecycleBin
import Amazonka.EC2.RestoreSnapshotTier
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
import Amazonka.EC2.StartNetworkInsightsAccessScopeAnalysis
import Amazonka.EC2.StartNetworkInsightsAnalysis
import Amazonka.EC2.StartVpcEndpointServicePrivateDnsVerification
import Amazonka.EC2.StopInstances
import Amazonka.EC2.TerminateClientVpnConnections
import Amazonka.EC2.TerminateInstances
import Amazonka.EC2.Types.AcceleratorCount
import Amazonka.EC2.Types.AcceleratorCountRequest
import Amazonka.EC2.Types.AcceleratorTotalMemoryMiB
import Amazonka.EC2.Types.AcceleratorTotalMemoryMiBRequest
import Amazonka.EC2.Types.AccessScopeAnalysisFinding
import Amazonka.EC2.Types.AccessScopePath
import Amazonka.EC2.Types.AccessScopePathRequest
import Amazonka.EC2.Types.AccountAttribute
import Amazonka.EC2.Types.AccountAttributeValue
import Amazonka.EC2.Types.ActiveInstance
import Amazonka.EC2.Types.AddIpamOperatingRegion
import Amazonka.EC2.Types.AddPrefixListEntry
import Amazonka.EC2.Types.AddedPrincipal
import Amazonka.EC2.Types.AdditionalDetail
import Amazonka.EC2.Types.Address
import Amazonka.EC2.Types.AddressAttribute
import Amazonka.EC2.Types.AddressTransfer
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
import Amazonka.EC2.Types.BaselineEbsBandwidthMbps
import Amazonka.EC2.Types.BaselineEbsBandwidthMbpsRequest
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
import Amazonka.EC2.Types.CapacityAllocation
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
import Amazonka.EC2.Types.ClientLoginBannerOptions
import Amazonka.EC2.Types.ClientLoginBannerResponseOptions
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
import Amazonka.EC2.Types.CloudWatchLogOptions
import Amazonka.EC2.Types.CloudWatchLogOptionsSpecification
import Amazonka.EC2.Types.CoipAddressUsage
import Amazonka.EC2.Types.CoipCidr
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
import Amazonka.EC2.Types.CreateTransitGatewayPeeringAttachmentRequestOptions
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
import Amazonka.EC2.Types.DescribeFastLaunchImagesSuccessItem
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
import Amazonka.EC2.Types.DnsOptions
import Amazonka.EC2.Types.DnsOptionsSpecification
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
import Amazonka.EC2.Types.FastLaunchLaunchTemplateSpecificationRequest
import Amazonka.EC2.Types.FastLaunchLaunchTemplateSpecificationResponse
import Amazonka.EC2.Types.FastLaunchSnapshotConfigurationRequest
import Amazonka.EC2.Types.FastLaunchSnapshotConfigurationResponse
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
import Amazonka.EC2.Types.ImageRecycleBinInfo
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
import Amazonka.EC2.Types.InstanceMaintenanceOptions
import Amazonka.EC2.Types.InstanceMaintenanceOptionsRequest
import Amazonka.EC2.Types.InstanceMarketOptionsRequest
import Amazonka.EC2.Types.InstanceMetadataOptionsRequest
import Amazonka.EC2.Types.InstanceMetadataOptionsResponse
import Amazonka.EC2.Types.InstanceMonitoring
import Amazonka.EC2.Types.InstanceNetworkInterface
import Amazonka.EC2.Types.InstanceNetworkInterfaceAssociation
import Amazonka.EC2.Types.InstanceNetworkInterfaceAttachment
import Amazonka.EC2.Types.InstanceNetworkInterfaceSpecification
import Amazonka.EC2.Types.InstancePrivateIpAddress
import Amazonka.EC2.Types.InstanceRequirements
import Amazonka.EC2.Types.InstanceRequirementsRequest
import Amazonka.EC2.Types.InstanceRequirementsWithMetadataRequest
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
import Amazonka.EC2.Types.InstanceTypeInfoFromInstanceRequirements
import Amazonka.EC2.Types.InstanceTypeOffering
import Amazonka.EC2.Types.InstanceUsage
import Amazonka.EC2.Types.IntegrateServices
import Amazonka.EC2.Types.InternetGateway
import Amazonka.EC2.Types.InternetGatewayAttachment
import Amazonka.EC2.Types.IpPermission
import Amazonka.EC2.Types.IpRange
import Amazonka.EC2.Types.Ipam
import Amazonka.EC2.Types.IpamAddressHistoryRecord
import Amazonka.EC2.Types.IpamCidrAuthorizationContext
import Amazonka.EC2.Types.IpamOperatingRegion
import Amazonka.EC2.Types.IpamPool
import Amazonka.EC2.Types.IpamPoolAllocation
import Amazonka.EC2.Types.IpamPoolCidr
import Amazonka.EC2.Types.IpamPoolCidrFailureReason
import Amazonka.EC2.Types.IpamResourceCidr
import Amazonka.EC2.Types.IpamResourceTag
import Amazonka.EC2.Types.IpamScope
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
import Amazonka.EC2.Types.LaunchTemplateInstanceMaintenanceOptions
import Amazonka.EC2.Types.LaunchTemplateInstanceMaintenanceOptionsRequest
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
import Amazonka.EC2.Types.LaunchTemplatePrivateDnsNameOptions
import Amazonka.EC2.Types.LaunchTemplatePrivateDnsNameOptionsRequest
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
import Amazonka.EC2.Types.MemoryGiBPerVCpu
import Amazonka.EC2.Types.MemoryGiBPerVCpuRequest
import Amazonka.EC2.Types.MemoryInfo
import Amazonka.EC2.Types.MemoryMiB
import Amazonka.EC2.Types.MemoryMiBRequest
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
import Amazonka.EC2.Types.NetworkBandwidthGbps
import Amazonka.EC2.Types.NetworkBandwidthGbpsRequest
import Amazonka.EC2.Types.NetworkCardInfo
import Amazonka.EC2.Types.NetworkInfo
import Amazonka.EC2.Types.NetworkInsightsAccessScope
import Amazonka.EC2.Types.NetworkInsightsAccessScopeAnalysis
import Amazonka.EC2.Types.NetworkInsightsAccessScopeContent
import Amazonka.EC2.Types.NetworkInsightsAnalysis
import Amazonka.EC2.Types.NetworkInsightsPath
import Amazonka.EC2.Types.NetworkInterface
import Amazonka.EC2.Types.NetworkInterfaceAssociation
import Amazonka.EC2.Types.NetworkInterfaceAttachment
import Amazonka.EC2.Types.NetworkInterfaceAttachmentChanges
import Amazonka.EC2.Types.NetworkInterfaceCount
import Amazonka.EC2.Types.NetworkInterfaceCountRequest
import Amazonka.EC2.Types.NetworkInterfaceIpv6Address
import Amazonka.EC2.Types.NetworkInterfacePermission
import Amazonka.EC2.Types.NetworkInterfacePermissionState
import Amazonka.EC2.Types.NetworkInterfacePrivateIpAddress
import Amazonka.EC2.Types.NewDhcpConfiguration
import Amazonka.EC2.Types.OnDemandOptions
import Amazonka.EC2.Types.OnDemandOptionsRequest
import Amazonka.EC2.Types.PacketHeaderStatement
import Amazonka.EC2.Types.PacketHeaderStatementRequest
import Amazonka.EC2.Types.PathComponent
import Amazonka.EC2.Types.PathStatement
import Amazonka.EC2.Types.PathStatementRequest
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
import Amazonka.EC2.Types.PrivateDnsNameOptionsOnLaunch
import Amazonka.EC2.Types.PrivateDnsNameOptionsRequest
import Amazonka.EC2.Types.PrivateDnsNameOptionsResponse
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
import Amazonka.EC2.Types.RemoveIpamOperatingRegion
import Amazonka.EC2.Types.RemovePrefixListEntry
import Amazonka.EC2.Types.ReplaceRootVolumeTask
import Amazonka.EC2.Types.RequestIpamResourceTag
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
import Amazonka.EC2.Types.ResourceStatement
import Amazonka.EC2.Types.ResourceStatementRequest
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
import Amazonka.EC2.Types.SnapshotRecycleBinInfo
import Amazonka.EC2.Types.SnapshotTaskDetail
import Amazonka.EC2.Types.SnapshotTierStatus
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
import Amazonka.EC2.Types.SpotPlacementScore
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
import Amazonka.EC2.Types.ThroughResourcesStatement
import Amazonka.EC2.Types.ThroughResourcesStatementRequest
import Amazonka.EC2.Types.TotalLocalStorageGB
import Amazonka.EC2.Types.TotalLocalStorageGBRequest
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
import Amazonka.EC2.Types.TransitGatewayPeeringAttachmentOptions
import Amazonka.EC2.Types.TransitGatewayPolicyRule
import Amazonka.EC2.Types.TransitGatewayPolicyRuleMetaData
import Amazonka.EC2.Types.TransitGatewayPolicyTable
import Amazonka.EC2.Types.TransitGatewayPolicyTableAssociation
import Amazonka.EC2.Types.TransitGatewayPolicyTableEntry
import Amazonka.EC2.Types.TransitGatewayPrefixListAttachment
import Amazonka.EC2.Types.TransitGatewayPrefixListReference
import Amazonka.EC2.Types.TransitGatewayPropagation
import Amazonka.EC2.Types.TransitGatewayRequestOptions
import Amazonka.EC2.Types.TransitGatewayRoute
import Amazonka.EC2.Types.TransitGatewayRouteAttachment
import Amazonka.EC2.Types.TransitGatewayRouteTable
import Amazonka.EC2.Types.TransitGatewayRouteTableAnnouncement
import Amazonka.EC2.Types.TransitGatewayRouteTableAssociation
import Amazonka.EC2.Types.TransitGatewayRouteTablePropagation
import Amazonka.EC2.Types.TransitGatewayRouteTableRoute
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
import Amazonka.EC2.Types.VCpuCountRange
import Amazonka.EC2.Types.VCpuCountRangeRequest
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
import Amazonka.EC2.Types.VpnTunnelLogOptions
import Amazonka.EC2.Types.VpnTunnelLogOptionsSpecification
import Amazonka.EC2.Types.VpnTunnelOptionsSpecification
import Amazonka.EC2.UnassignIpv6Addresses
import Amazonka.EC2.UnassignPrivateIpAddresses
import Amazonka.EC2.UnmonitorInstances
import Amazonka.EC2.UpdateSecurityGroupRuleDescriptionsEgress
import Amazonka.EC2.UpdateSecurityGroupRuleDescriptionsIngress
import Amazonka.EC2.WithdrawByoipCidr
