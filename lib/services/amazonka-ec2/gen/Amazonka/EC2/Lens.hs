{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EC2.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    acceptTransitGatewayMulticastDomainAssociations_dryRun,
    acceptTransitGatewayMulticastDomainAssociations_subnetIds,
    acceptTransitGatewayMulticastDomainAssociations_transitGatewayAttachmentId,
    acceptTransitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId,
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
    acceptVpcPeeringConnection_dryRun,
    acceptVpcPeeringConnection_vpcPeeringConnectionId,
    acceptVpcPeeringConnectionResponse_vpcPeeringConnection,
    acceptVpcPeeringConnectionResponse_httpStatus,

    -- ** AdvertiseByoipCidr
    advertiseByoipCidr_dryRun,
    advertiseByoipCidr_cidr,
    advertiseByoipCidrResponse_byoipCidr,
    advertiseByoipCidrResponse_httpStatus,

    -- ** AllocateAddress
    allocateAddress_address,
    allocateAddress_customerOwnedIpv4Pool,
    allocateAddress_domain,
    allocateAddress_dryRun,
    allocateAddress_networkBorderGroup,
    allocateAddress_publicIpv4Pool,
    allocateAddress_tagSpecifications,
    allocateAddressResponse_allocationId,
    allocateAddressResponse_carrierIp,
    allocateAddressResponse_customerOwnedIp,
    allocateAddressResponse_customerOwnedIpv4Pool,
    allocateAddressResponse_domain,
    allocateAddressResponse_networkBorderGroup,
    allocateAddressResponse_publicIp,
    allocateAddressResponse_publicIpv4Pool,
    allocateAddressResponse_httpStatus,

    -- ** AllocateHosts
    allocateHosts_assetIds,
    allocateHosts_autoPlacement,
    allocateHosts_clientToken,
    allocateHosts_hostMaintenance,
    allocateHosts_hostRecovery,
    allocateHosts_instanceFamily,
    allocateHosts_instanceType,
    allocateHosts_outpostArn,
    allocateHosts_quantity,
    allocateHosts_tagSpecifications,
    allocateHosts_availabilityZone,
    allocateHostsResponse_hostIds,
    allocateHostsResponse_httpStatus,

    -- ** AllocateIpamPoolCidr
    allocateIpamPoolCidr_cidr,
    allocateIpamPoolCidr_clientToken,
    allocateIpamPoolCidr_description,
    allocateIpamPoolCidr_disallowedCidrs,
    allocateIpamPoolCidr_dryRun,
    allocateIpamPoolCidr_netmaskLength,
    allocateIpamPoolCidr_previewNextCidr,
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
    assignIpv6Addresses_ipv6Addresses,
    assignIpv6Addresses_ipv6PrefixCount,
    assignIpv6Addresses_ipv6Prefixes,
    assignIpv6Addresses_networkInterfaceId,
    assignIpv6AddressesResponse_assignedIpv6Addresses,
    assignIpv6AddressesResponse_assignedIpv6Prefixes,
    assignIpv6AddressesResponse_networkInterfaceId,
    assignIpv6AddressesResponse_httpStatus,

    -- ** AssignPrivateIpAddresses
    assignPrivateIpAddresses_allowReassignment,
    assignPrivateIpAddresses_ipv4PrefixCount,
    assignPrivateIpAddresses_ipv4Prefixes,
    assignPrivateIpAddresses_privateIpAddresses,
    assignPrivateIpAddresses_secondaryPrivateIpAddressCount,
    assignPrivateIpAddresses_networkInterfaceId,
    assignPrivateIpAddressesResponse_assignedIpv4Prefixes,
    assignPrivateIpAddressesResponse_assignedPrivateIpAddresses,
    assignPrivateIpAddressesResponse_networkInterfaceId,
    assignPrivateIpAddressesResponse_httpStatus,

    -- ** AssignPrivateNatGatewayAddress
    assignPrivateNatGatewayAddress_dryRun,
    assignPrivateNatGatewayAddress_privateIpAddressCount,
    assignPrivateNatGatewayAddress_privateIpAddresses,
    assignPrivateNatGatewayAddress_natGatewayId,
    assignPrivateNatGatewayAddressResponse_natGatewayAddresses,
    assignPrivateNatGatewayAddressResponse_natGatewayId,
    assignPrivateNatGatewayAddressResponse_httpStatus,

    -- ** AssociateAddress
    associateAddress_allocationId,
    associateAddress_allowReassociation,
    associateAddress_dryRun,
    associateAddress_instanceId,
    associateAddress_networkInterfaceId,
    associateAddress_privateIpAddress,
    associateAddress_publicIp,
    associateAddressResponse_associationId,
    associateAddressResponse_httpStatus,

    -- ** AssociateClientVpnTargetNetwork
    associateClientVpnTargetNetwork_clientToken,
    associateClientVpnTargetNetwork_dryRun,
    associateClientVpnTargetNetwork_clientVpnEndpointId,
    associateClientVpnTargetNetwork_subnetId,
    associateClientVpnTargetNetworkResponse_associationId,
    associateClientVpnTargetNetworkResponse_status,
    associateClientVpnTargetNetworkResponse_httpStatus,

    -- ** AssociateDhcpOptions
    associateDhcpOptions_dryRun,
    associateDhcpOptions_dhcpOptionsId,
    associateDhcpOptions_vpcId,

    -- ** AssociateEnclaveCertificateIamRole
    associateEnclaveCertificateIamRole_dryRun,
    associateEnclaveCertificateIamRole_certificateArn,
    associateEnclaveCertificateIamRole_roleArn,
    associateEnclaveCertificateIamRoleResponse_certificateS3BucketName,
    associateEnclaveCertificateIamRoleResponse_certificateS3ObjectKey,
    associateEnclaveCertificateIamRoleResponse_encryptionKmsKeyId,
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

    -- ** AssociateIpamResourceDiscovery
    associateIpamResourceDiscovery_clientToken,
    associateIpamResourceDiscovery_dryRun,
    associateIpamResourceDiscovery_tagSpecifications,
    associateIpamResourceDiscovery_ipamId,
    associateIpamResourceDiscovery_ipamResourceDiscoveryId,
    associateIpamResourceDiscoveryResponse_ipamResourceDiscoveryAssociation,
    associateIpamResourceDiscoveryResponse_httpStatus,

    -- ** AssociateNatGatewayAddress
    associateNatGatewayAddress_dryRun,
    associateNatGatewayAddress_privateIpAddresses,
    associateNatGatewayAddress_natGatewayId,
    associateNatGatewayAddress_allocationIds,
    associateNatGatewayAddressResponse_natGatewayAddresses,
    associateNatGatewayAddressResponse_natGatewayId,
    associateNatGatewayAddressResponse_httpStatus,

    -- ** AssociateRouteTable
    associateRouteTable_dryRun,
    associateRouteTable_gatewayId,
    associateRouteTable_subnetId,
    associateRouteTable_routeTableId,
    associateRouteTableResponse_associationId,
    associateRouteTableResponse_associationState,
    associateRouteTableResponse_httpStatus,

    -- ** AssociateSubnetCidrBlock
    associateSubnetCidrBlock_ipv6CidrBlock,
    associateSubnetCidrBlock_subnetId,
    associateSubnetCidrBlockResponse_ipv6CidrBlockAssociation,
    associateSubnetCidrBlockResponse_subnetId,
    associateSubnetCidrBlockResponse_httpStatus,

    -- ** AssociateTransitGatewayMulticastDomain
    associateTransitGatewayMulticastDomain_dryRun,
    associateTransitGatewayMulticastDomain_transitGatewayMulticastDomainId,
    associateTransitGatewayMulticastDomain_transitGatewayAttachmentId,
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
    associateTrunkInterface_greKey,
    associateTrunkInterface_vlanId,
    associateTrunkInterface_branchInterfaceId,
    associateTrunkInterface_trunkInterfaceId,
    associateTrunkInterfaceResponse_clientToken,
    associateTrunkInterfaceResponse_interfaceAssociation,
    associateTrunkInterfaceResponse_httpStatus,

    -- ** AssociateVpcCidrBlock
    associateVpcCidrBlock_amazonProvidedIpv6CidrBlock,
    associateVpcCidrBlock_cidrBlock,
    associateVpcCidrBlock_ipv4IpamPoolId,
    associateVpcCidrBlock_ipv4NetmaskLength,
    associateVpcCidrBlock_ipv6CidrBlock,
    associateVpcCidrBlock_ipv6CidrBlockNetworkBorderGroup,
    associateVpcCidrBlock_ipv6IpamPoolId,
    associateVpcCidrBlock_ipv6NetmaskLength,
    associateVpcCidrBlock_ipv6Pool,
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
    attachNetworkInterface_dryRun,
    attachNetworkInterface_enaSrdSpecification,
    attachNetworkInterface_networkCardIndex,
    attachNetworkInterface_deviceIndex,
    attachNetworkInterface_instanceId,
    attachNetworkInterface_networkInterfaceId,
    attachNetworkInterfaceResponse_attachmentId,
    attachNetworkInterfaceResponse_networkCardIndex,
    attachNetworkInterfaceResponse_httpStatus,

    -- ** AttachVerifiedAccessTrustProvider
    attachVerifiedAccessTrustProvider_clientToken,
    attachVerifiedAccessTrustProvider_dryRun,
    attachVerifiedAccessTrustProvider_verifiedAccessInstanceId,
    attachVerifiedAccessTrustProvider_verifiedAccessTrustProviderId,
    attachVerifiedAccessTrustProviderResponse_verifiedAccessInstance,
    attachVerifiedAccessTrustProviderResponse_verifiedAccessTrustProvider,
    attachVerifiedAccessTrustProviderResponse_httpStatus,

    -- ** AttachVolume
    attachVolume_dryRun,
    attachVolume_device,
    attachVolume_instanceId,
    attachVolume_volumeId,
    volumeAttachment_attachTime,
    volumeAttachment_deleteOnTermination,
    volumeAttachment_device,
    volumeAttachment_instanceId,
    volumeAttachment_state,
    volumeAttachment_volumeId,

    -- ** AttachVpnGateway
    attachVpnGateway_dryRun,
    attachVpnGateway_vpcId,
    attachVpnGateway_vpnGatewayId,
    attachVpnGatewayResponse_vpcAttachment,
    attachVpnGatewayResponse_httpStatus,

    -- ** AuthorizeClientVpnIngress
    authorizeClientVpnIngress_accessGroupId,
    authorizeClientVpnIngress_authorizeAllGroups,
    authorizeClientVpnIngress_clientToken,
    authorizeClientVpnIngress_description,
    authorizeClientVpnIngress_dryRun,
    authorizeClientVpnIngress_clientVpnEndpointId,
    authorizeClientVpnIngress_targetNetworkCidr,
    authorizeClientVpnIngressResponse_status,
    authorizeClientVpnIngressResponse_httpStatus,

    -- ** AuthorizeSecurityGroupEgress
    authorizeSecurityGroupEgress_cidrIp,
    authorizeSecurityGroupEgress_dryRun,
    authorizeSecurityGroupEgress_fromPort,
    authorizeSecurityGroupEgress_ipPermissions,
    authorizeSecurityGroupEgress_ipProtocol,
    authorizeSecurityGroupEgress_sourceSecurityGroupName,
    authorizeSecurityGroupEgress_sourceSecurityGroupOwnerId,
    authorizeSecurityGroupEgress_tagSpecifications,
    authorizeSecurityGroupEgress_toPort,
    authorizeSecurityGroupEgress_groupId,
    authorizeSecurityGroupEgressResponse_return,
    authorizeSecurityGroupEgressResponse_securityGroupRules,
    authorizeSecurityGroupEgressResponse_httpStatus,

    -- ** AuthorizeSecurityGroupIngress
    authorizeSecurityGroupIngress_cidrIp,
    authorizeSecurityGroupIngress_dryRun,
    authorizeSecurityGroupIngress_fromPort,
    authorizeSecurityGroupIngress_groupId,
    authorizeSecurityGroupIngress_groupName,
    authorizeSecurityGroupIngress_ipPermissions,
    authorizeSecurityGroupIngress_ipProtocol,
    authorizeSecurityGroupIngress_sourceSecurityGroupName,
    authorizeSecurityGroupIngress_sourceSecurityGroupOwnerId,
    authorizeSecurityGroupIngress_tagSpecifications,
    authorizeSecurityGroupIngress_toPort,
    authorizeSecurityGroupIngressResponse_return,
    authorizeSecurityGroupIngressResponse_securityGroupRules,
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
    cancelImportTask_dryRun,
    cancelImportTask_importTaskId,
    cancelImportTaskResponse_importTaskId,
    cancelImportTaskResponse_previousState,
    cancelImportTaskResponse_state,
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
    copyFpgaImage_description,
    copyFpgaImage_dryRun,
    copyFpgaImage_name,
    copyFpgaImage_sourceFpgaImageId,
    copyFpgaImage_sourceRegion,
    copyFpgaImageResponse_fpgaImageId,
    copyFpgaImageResponse_httpStatus,

    -- ** CopyImage
    copyImage_clientToken,
    copyImage_copyImageTags,
    copyImage_description,
    copyImage_destinationOutpostArn,
    copyImage_dryRun,
    copyImage_encrypted,
    copyImage_kmsKeyId,
    copyImage_name,
    copyImage_sourceImageId,
    copyImage_sourceRegion,
    copyImageResponse_imageId,
    copyImageResponse_httpStatus,

    -- ** CopySnapshot
    copySnapshot_description,
    copySnapshot_destinationOutpostArn,
    copySnapshot_destinationRegion,
    copySnapshot_dryRun,
    copySnapshot_encrypted,
    copySnapshot_kmsKeyId,
    copySnapshot_presignedUrl,
    copySnapshot_tagSpecifications,
    copySnapshot_sourceRegion,
    copySnapshot_sourceSnapshotId,
    copySnapshotResponse_snapshotId,
    copySnapshotResponse_tags,
    copySnapshotResponse_httpStatus,

    -- ** CreateCapacityReservation
    createCapacityReservation_availabilityZone,
    createCapacityReservation_availabilityZoneId,
    createCapacityReservation_clientToken,
    createCapacityReservation_dryRun,
    createCapacityReservation_ebsOptimized,
    createCapacityReservation_endDate,
    createCapacityReservation_endDateType,
    createCapacityReservation_ephemeralStorage,
    createCapacityReservation_instanceMatchCriteria,
    createCapacityReservation_outpostArn,
    createCapacityReservation_placementGroupArn,
    createCapacityReservation_tagSpecifications,
    createCapacityReservation_tenancy,
    createCapacityReservation_instanceType,
    createCapacityReservation_instancePlatform,
    createCapacityReservation_instanceCount,
    createCapacityReservationResponse_capacityReservation,
    createCapacityReservationResponse_httpStatus,

    -- ** CreateCapacityReservationFleet
    createCapacityReservationFleet_allocationStrategy,
    createCapacityReservationFleet_clientToken,
    createCapacityReservationFleet_dryRun,
    createCapacityReservationFleet_endDate,
    createCapacityReservationFleet_instanceMatchCriteria,
    createCapacityReservationFleet_tagSpecifications,
    createCapacityReservationFleet_tenancy,
    createCapacityReservationFleet_instanceTypeSpecifications,
    createCapacityReservationFleet_totalTargetCapacity,
    createCapacityReservationFleetResponse_allocationStrategy,
    createCapacityReservationFleetResponse_capacityReservationFleetId,
    createCapacityReservationFleetResponse_createTime,
    createCapacityReservationFleetResponse_endDate,
    createCapacityReservationFleetResponse_fleetCapacityReservations,
    createCapacityReservationFleetResponse_instanceMatchCriteria,
    createCapacityReservationFleetResponse_state,
    createCapacityReservationFleetResponse_tags,
    createCapacityReservationFleetResponse_tenancy,
    createCapacityReservationFleetResponse_totalFulfilledCapacity,
    createCapacityReservationFleetResponse_totalTargetCapacity,
    createCapacityReservationFleetResponse_httpStatus,

    -- ** CreateCarrierGateway
    createCarrierGateway_clientToken,
    createCarrierGateway_dryRun,
    createCarrierGateway_tagSpecifications,
    createCarrierGateway_vpcId,
    createCarrierGatewayResponse_carrierGateway,
    createCarrierGatewayResponse_httpStatus,

    -- ** CreateClientVpnEndpoint
    createClientVpnEndpoint_clientConnectOptions,
    createClientVpnEndpoint_clientLoginBannerOptions,
    createClientVpnEndpoint_clientToken,
    createClientVpnEndpoint_description,
    createClientVpnEndpoint_dnsServers,
    createClientVpnEndpoint_dryRun,
    createClientVpnEndpoint_securityGroupIds,
    createClientVpnEndpoint_selfServicePortal,
    createClientVpnEndpoint_sessionTimeoutHours,
    createClientVpnEndpoint_splitTunnel,
    createClientVpnEndpoint_tagSpecifications,
    createClientVpnEndpoint_transportProtocol,
    createClientVpnEndpoint_vpcId,
    createClientVpnEndpoint_vpnPort,
    createClientVpnEndpoint_clientCidrBlock,
    createClientVpnEndpoint_serverCertificateArn,
    createClientVpnEndpoint_authenticationOptions,
    createClientVpnEndpoint_connectionLogOptions,
    createClientVpnEndpointResponse_clientVpnEndpointId,
    createClientVpnEndpointResponse_dnsName,
    createClientVpnEndpointResponse_status,
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
    createCustomerGateway_bgpAsn,
    createCustomerGateway_certificateArn,
    createCustomerGateway_deviceName,
    createCustomerGateway_dryRun,
    createCustomerGateway_ipAddress,
    createCustomerGateway_publicIp,
    createCustomerGateway_tagSpecifications,
    createCustomerGateway_type,
    createCustomerGatewayResponse_customerGateway,
    createCustomerGatewayResponse_httpStatus,

    -- ** CreateDefaultSubnet
    createDefaultSubnet_dryRun,
    createDefaultSubnet_ipv6Native,
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
    createFleet_clientToken,
    createFleet_context,
    createFleet_dryRun,
    createFleet_excessCapacityTerminationPolicy,
    createFleet_onDemandOptions,
    createFleet_replaceUnhealthyInstances,
    createFleet_spotOptions,
    createFleet_tagSpecifications,
    createFleet_terminateInstancesWithExpiration,
    createFleet_type,
    createFleet_validFrom,
    createFleet_validUntil,
    createFleet_launchTemplateConfigs,
    createFleet_targetCapacitySpecification,
    createFleetResponse_errors,
    createFleetResponse_fleetId,
    createFleetResponse_instances,
    createFleetResponse_httpStatus,

    -- ** CreateFlowLogs
    createFlowLogs_clientToken,
    createFlowLogs_deliverCrossAccountRole,
    createFlowLogs_deliverLogsPermissionArn,
    createFlowLogs_destinationOptions,
    createFlowLogs_dryRun,
    createFlowLogs_logDestination,
    createFlowLogs_logDestinationType,
    createFlowLogs_logFormat,
    createFlowLogs_logGroupName,
    createFlowLogs_maxAggregationInterval,
    createFlowLogs_tagSpecifications,
    createFlowLogs_trafficType,
    createFlowLogs_resourceIds,
    createFlowLogs_resourceType,
    createFlowLogsResponse_clientToken,
    createFlowLogsResponse_flowLogIds,
    createFlowLogsResponse_unsuccessful,
    createFlowLogsResponse_httpStatus,

    -- ** CreateFpgaImage
    createFpgaImage_clientToken,
    createFpgaImage_description,
    createFpgaImage_dryRun,
    createFpgaImage_logsStorageLocation,
    createFpgaImage_name,
    createFpgaImage_tagSpecifications,
    createFpgaImage_inputStorageLocation,
    createFpgaImageResponse_fpgaImageGlobalId,
    createFpgaImageResponse_fpgaImageId,
    createFpgaImageResponse_httpStatus,

    -- ** CreateImage
    createImage_blockDeviceMappings,
    createImage_description,
    createImage_dryRun,
    createImage_noReboot,
    createImage_tagSpecifications,
    createImage_instanceId,
    createImage_name,
    createImageResponse_imageId,
    createImageResponse_httpStatus,

    -- ** CreateInstanceConnectEndpoint
    createInstanceConnectEndpoint_clientToken,
    createInstanceConnectEndpoint_dryRun,
    createInstanceConnectEndpoint_preserveClientIp,
    createInstanceConnectEndpoint_securityGroupIds,
    createInstanceConnectEndpoint_tagSpecifications,
    createInstanceConnectEndpoint_subnetId,
    createInstanceConnectEndpointResponse_clientToken,
    createInstanceConnectEndpointResponse_instanceConnectEndpoint,
    createInstanceConnectEndpointResponse_httpStatus,

    -- ** CreateInstanceEventWindow
    createInstanceEventWindow_cronExpression,
    createInstanceEventWindow_dryRun,
    createInstanceEventWindow_name,
    createInstanceEventWindow_tagSpecifications,
    createInstanceEventWindow_timeRanges,
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
    createIpamPool_allocationDefaultNetmaskLength,
    createIpamPool_allocationMaxNetmaskLength,
    createIpamPool_allocationMinNetmaskLength,
    createIpamPool_allocationResourceTags,
    createIpamPool_autoImport,
    createIpamPool_awsService,
    createIpamPool_clientToken,
    createIpamPool_description,
    createIpamPool_dryRun,
    createIpamPool_locale,
    createIpamPool_publicIpSource,
    createIpamPool_publiclyAdvertisable,
    createIpamPool_sourceIpamPoolId,
    createIpamPool_tagSpecifications,
    createIpamPool_ipamScopeId,
    createIpamPool_addressFamily,
    createIpamPoolResponse_ipamPool,
    createIpamPoolResponse_httpStatus,

    -- ** CreateIpamResourceDiscovery
    createIpamResourceDiscovery_clientToken,
    createIpamResourceDiscovery_description,
    createIpamResourceDiscovery_dryRun,
    createIpamResourceDiscovery_operatingRegions,
    createIpamResourceDiscovery_tagSpecifications,
    createIpamResourceDiscoveryResponse_ipamResourceDiscovery,
    createIpamResourceDiscoveryResponse_httpStatus,

    -- ** CreateIpamScope
    createIpamScope_clientToken,
    createIpamScope_description,
    createIpamScope_dryRun,
    createIpamScope_tagSpecifications,
    createIpamScope_ipamId,
    createIpamScopeResponse_ipamScope,
    createIpamScopeResponse_httpStatus,

    -- ** CreateKeyPair
    createKeyPair_dryRun,
    createKeyPair_keyFormat,
    createKeyPair_keyType,
    createKeyPair_tagSpecifications,
    createKeyPair_keyName,
    createKeyPairResponse_keyPairId,
    createKeyPairResponse_tags,
    createKeyPairResponse_httpStatus,
    createKeyPairResponse_keyName,
    createKeyPairResponse_keyFingerprint,
    createKeyPairResponse_keyMaterial,

    -- ** CreateLaunchTemplate
    createLaunchTemplate_clientToken,
    createLaunchTemplate_dryRun,
    createLaunchTemplate_tagSpecifications,
    createLaunchTemplate_versionDescription,
    createLaunchTemplate_launchTemplateName,
    createLaunchTemplate_launchTemplateData,
    createLaunchTemplateResponse_launchTemplate,
    createLaunchTemplateResponse_warning,
    createLaunchTemplateResponse_httpStatus,

    -- ** CreateLaunchTemplateVersion
    createLaunchTemplateVersion_clientToken,
    createLaunchTemplateVersion_dryRun,
    createLaunchTemplateVersion_launchTemplateId,
    createLaunchTemplateVersion_launchTemplateName,
    createLaunchTemplateVersion_resolveAlias,
    createLaunchTemplateVersion_sourceVersion,
    createLaunchTemplateVersion_versionDescription,
    createLaunchTemplateVersion_launchTemplateData,
    createLaunchTemplateVersionResponse_launchTemplateVersion,
    createLaunchTemplateVersionResponse_warning,
    createLaunchTemplateVersionResponse_httpStatus,

    -- ** CreateLocalGatewayRoute
    createLocalGatewayRoute_destinationCidrBlock,
    createLocalGatewayRoute_destinationPrefixListId,
    createLocalGatewayRoute_dryRun,
    createLocalGatewayRoute_localGatewayVirtualInterfaceGroupId,
    createLocalGatewayRoute_networkInterfaceId,
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
    createNatGateway_allocationId,
    createNatGateway_clientToken,
    createNatGateway_connectivityType,
    createNatGateway_dryRun,
    createNatGateway_privateIpAddress,
    createNatGateway_secondaryAllocationIds,
    createNatGateway_secondaryPrivateIpAddressCount,
    createNatGateway_secondaryPrivateIpAddresses,
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
    createNetworkAclEntry_cidrBlock,
    createNetworkAclEntry_dryRun,
    createNetworkAclEntry_icmpTypeCode,
    createNetworkAclEntry_ipv6CidrBlock,
    createNetworkAclEntry_portRange,
    createNetworkAclEntry_egress,
    createNetworkAclEntry_networkAclId,
    createNetworkAclEntry_protocol,
    createNetworkAclEntry_ruleAction,
    createNetworkAclEntry_ruleNumber,

    -- ** CreateNetworkInsightsAccessScope
    createNetworkInsightsAccessScope_dryRun,
    createNetworkInsightsAccessScope_excludePaths,
    createNetworkInsightsAccessScope_matchPaths,
    createNetworkInsightsAccessScope_tagSpecifications,
    createNetworkInsightsAccessScope_clientToken,
    createNetworkInsightsAccessScopeResponse_networkInsightsAccessScope,
    createNetworkInsightsAccessScopeResponse_networkInsightsAccessScopeContent,
    createNetworkInsightsAccessScopeResponse_httpStatus,

    -- ** CreateNetworkInsightsPath
    createNetworkInsightsPath_destination,
    createNetworkInsightsPath_destinationIp,
    createNetworkInsightsPath_destinationPort,
    createNetworkInsightsPath_dryRun,
    createNetworkInsightsPath_filterAtDestination,
    createNetworkInsightsPath_filterAtSource,
    createNetworkInsightsPath_sourceIp,
    createNetworkInsightsPath_tagSpecifications,
    createNetworkInsightsPath_source,
    createNetworkInsightsPath_protocol,
    createNetworkInsightsPath_clientToken,
    createNetworkInsightsPathResponse_networkInsightsPath,
    createNetworkInsightsPathResponse_httpStatus,

    -- ** CreateNetworkInterface
    createNetworkInterface_clientToken,
    createNetworkInterface_description,
    createNetworkInterface_dryRun,
    createNetworkInterface_groups,
    createNetworkInterface_interfaceType,
    createNetworkInterface_ipv4PrefixCount,
    createNetworkInterface_ipv4Prefixes,
    createNetworkInterface_ipv6AddressCount,
    createNetworkInterface_ipv6Addresses,
    createNetworkInterface_ipv6PrefixCount,
    createNetworkInterface_ipv6Prefixes,
    createNetworkInterface_privateIpAddress,
    createNetworkInterface_privateIpAddresses,
    createNetworkInterface_secondaryPrivateIpAddressCount,
    createNetworkInterface_tagSpecifications,
    createNetworkInterface_subnetId,
    createNetworkInterfaceResponse_clientToken,
    createNetworkInterfaceResponse_networkInterface,
    createNetworkInterfaceResponse_httpStatus,

    -- ** CreateNetworkInterfacePermission
    createNetworkInterfacePermission_awsAccountId,
    createNetworkInterfacePermission_awsService,
    createNetworkInterfacePermission_dryRun,
    createNetworkInterfacePermission_networkInterfaceId,
    createNetworkInterfacePermission_permission,
    createNetworkInterfacePermissionResponse_interfacePermission,
    createNetworkInterfacePermissionResponse_httpStatus,

    -- ** CreatePlacementGroup
    createPlacementGroup_dryRun,
    createPlacementGroup_groupName,
    createPlacementGroup_partitionCount,
    createPlacementGroup_spreadLevel,
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
    createReplaceRootVolumeTask_deleteReplacedRootVolume,
    createReplaceRootVolumeTask_dryRun,
    createReplaceRootVolumeTask_imageId,
    createReplaceRootVolumeTask_snapshotId,
    createReplaceRootVolumeTask_tagSpecifications,
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
    createRestoreImageTask_dryRun,
    createRestoreImageTask_name,
    createRestoreImageTask_tagSpecifications,
    createRestoreImageTask_bucket,
    createRestoreImageTask_objectKey,
    createRestoreImageTaskResponse_imageId,
    createRestoreImageTaskResponse_httpStatus,

    -- ** CreateRoute
    createRoute_carrierGatewayId,
    createRoute_coreNetworkArn,
    createRoute_destinationCidrBlock,
    createRoute_destinationIpv6CidrBlock,
    createRoute_destinationPrefixListId,
    createRoute_dryRun,
    createRoute_egressOnlyInternetGatewayId,
    createRoute_gatewayId,
    createRoute_instanceId,
    createRoute_localGatewayId,
    createRoute_natGatewayId,
    createRoute_networkInterfaceId,
    createRoute_transitGatewayId,
    createRoute_vpcEndpointId,
    createRoute_vpcPeeringConnectionId,
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
    createSecurityGroup_tagSpecifications,
    createSecurityGroup_vpcId,
    createSecurityGroup_description,
    createSecurityGroup_groupName,
    createSecurityGroupResponse_tags,
    createSecurityGroupResponse_httpStatus,
    createSecurityGroupResponse_groupId,

    -- ** CreateSnapshot
    createSnapshot_description,
    createSnapshot_dryRun,
    createSnapshot_outpostArn,
    createSnapshot_tagSpecifications,
    createSnapshot_volumeId,
    snapshot_dataEncryptionKeyId,
    snapshot_kmsKeyId,
    snapshot_outpostArn,
    snapshot_ownerAlias,
    snapshot_restoreExpiryTime,
    snapshot_stateMessage,
    snapshot_storageTier,
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

    -- ** CreateSnapshots
    createSnapshots_copyTagsFromSource,
    createSnapshots_description,
    createSnapshots_dryRun,
    createSnapshots_outpostArn,
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
    createSubnet_availabilityZone,
    createSubnet_availabilityZoneId,
    createSubnet_cidrBlock,
    createSubnet_dryRun,
    createSubnet_ipv6CidrBlock,
    createSubnet_ipv6Native,
    createSubnet_outpostArn,
    createSubnet_tagSpecifications,
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
    createTrafficMirrorFilterResponse_clientToken,
    createTrafficMirrorFilterResponse_trafficMirrorFilter,
    createTrafficMirrorFilterResponse_httpStatus,

    -- ** CreateTrafficMirrorFilterRule
    createTrafficMirrorFilterRule_clientToken,
    createTrafficMirrorFilterRule_description,
    createTrafficMirrorFilterRule_destinationPortRange,
    createTrafficMirrorFilterRule_dryRun,
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
    createTrafficMirrorSession_dryRun,
    createTrafficMirrorSession_packetLength,
    createTrafficMirrorSession_tagSpecifications,
    createTrafficMirrorSession_virtualNetworkId,
    createTrafficMirrorSession_networkInterfaceId,
    createTrafficMirrorSession_trafficMirrorTargetId,
    createTrafficMirrorSession_trafficMirrorFilterId,
    createTrafficMirrorSession_sessionNumber,
    createTrafficMirrorSessionResponse_clientToken,
    createTrafficMirrorSessionResponse_trafficMirrorSession,
    createTrafficMirrorSessionResponse_httpStatus,

    -- ** CreateTrafficMirrorTarget
    createTrafficMirrorTarget_clientToken,
    createTrafficMirrorTarget_description,
    createTrafficMirrorTarget_dryRun,
    createTrafficMirrorTarget_gatewayLoadBalancerEndpointId,
    createTrafficMirrorTarget_networkInterfaceId,
    createTrafficMirrorTarget_networkLoadBalancerArn,
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
    createTransitGatewayConnectPeer_bgpOptions,
    createTransitGatewayConnectPeer_dryRun,
    createTransitGatewayConnectPeer_tagSpecifications,
    createTransitGatewayConnectPeer_transitGatewayAddress,
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
    createTransitGatewayPrefixListReference_blackhole,
    createTransitGatewayPrefixListReference_dryRun,
    createTransitGatewayPrefixListReference_transitGatewayAttachmentId,
    createTransitGatewayPrefixListReference_transitGatewayRouteTableId,
    createTransitGatewayPrefixListReference_prefixListId,
    createTransitGatewayPrefixListReferenceResponse_transitGatewayPrefixListReference,
    createTransitGatewayPrefixListReferenceResponse_httpStatus,

    -- ** CreateTransitGatewayRoute
    createTransitGatewayRoute_blackhole,
    createTransitGatewayRoute_dryRun,
    createTransitGatewayRoute_transitGatewayAttachmentId,
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

    -- ** CreateVerifiedAccessEndpoint
    createVerifiedAccessEndpoint_clientToken,
    createVerifiedAccessEndpoint_description,
    createVerifiedAccessEndpoint_dryRun,
    createVerifiedAccessEndpoint_loadBalancerOptions,
    createVerifiedAccessEndpoint_networkInterfaceOptions,
    createVerifiedAccessEndpoint_policyDocument,
    createVerifiedAccessEndpoint_securityGroupIds,
    createVerifiedAccessEndpoint_tagSpecifications,
    createVerifiedAccessEndpoint_verifiedAccessGroupId,
    createVerifiedAccessEndpoint_endpointType,
    createVerifiedAccessEndpoint_attachmentType,
    createVerifiedAccessEndpoint_domainCertificateArn,
    createVerifiedAccessEndpoint_applicationDomain,
    createVerifiedAccessEndpoint_endpointDomainPrefix,
    createVerifiedAccessEndpointResponse_verifiedAccessEndpoint,
    createVerifiedAccessEndpointResponse_httpStatus,

    -- ** CreateVerifiedAccessGroup
    createVerifiedAccessGroup_clientToken,
    createVerifiedAccessGroup_description,
    createVerifiedAccessGroup_dryRun,
    createVerifiedAccessGroup_policyDocument,
    createVerifiedAccessGroup_tagSpecifications,
    createVerifiedAccessGroup_verifiedAccessInstanceId,
    createVerifiedAccessGroupResponse_verifiedAccessGroup,
    createVerifiedAccessGroupResponse_httpStatus,

    -- ** CreateVerifiedAccessInstance
    createVerifiedAccessInstance_clientToken,
    createVerifiedAccessInstance_description,
    createVerifiedAccessInstance_dryRun,
    createVerifiedAccessInstance_tagSpecifications,
    createVerifiedAccessInstanceResponse_verifiedAccessInstance,
    createVerifiedAccessInstanceResponse_httpStatus,

    -- ** CreateVerifiedAccessTrustProvider
    createVerifiedAccessTrustProvider_clientToken,
    createVerifiedAccessTrustProvider_description,
    createVerifiedAccessTrustProvider_deviceOptions,
    createVerifiedAccessTrustProvider_deviceTrustProviderType,
    createVerifiedAccessTrustProvider_dryRun,
    createVerifiedAccessTrustProvider_oidcOptions,
    createVerifiedAccessTrustProvider_tagSpecifications,
    createVerifiedAccessTrustProvider_userTrustProviderType,
    createVerifiedAccessTrustProvider_trustProviderType,
    createVerifiedAccessTrustProvider_policyReferenceName,
    createVerifiedAccessTrustProviderResponse_verifiedAccessTrustProvider,
    createVerifiedAccessTrustProviderResponse_httpStatus,

    -- ** CreateVolume
    createVolume_clientToken,
    createVolume_dryRun,
    createVolume_encrypted,
    createVolume_iops,
    createVolume_kmsKeyId,
    createVolume_multiAttachEnabled,
    createVolume_outpostArn,
    createVolume_size,
    createVolume_snapshotId,
    createVolume_tagSpecifications,
    createVolume_throughput,
    createVolume_volumeType,
    createVolume_availabilityZone,
    volume_attachments,
    volume_fastRestored,
    volume_iops,
    volume_kmsKeyId,
    volume_multiAttachEnabled,
    volume_outpostArn,
    volume_tags,
    volume_throughput,
    volume_availabilityZone,
    volume_createTime,
    volume_encrypted,
    volume_size,
    volume_snapshotId,
    volume_state,
    volume_volumeId,
    volume_volumeType,

    -- ** CreateVpc
    createVpc_amazonProvidedIpv6CidrBlock,
    createVpc_cidrBlock,
    createVpc_dryRun,
    createVpc_instanceTenancy,
    createVpc_ipv4IpamPoolId,
    createVpc_ipv4NetmaskLength,
    createVpc_ipv6CidrBlock,
    createVpc_ipv6CidrBlockNetworkBorderGroup,
    createVpc_ipv6IpamPoolId,
    createVpc_ipv6NetmaskLength,
    createVpc_ipv6Pool,
    createVpc_tagSpecifications,
    createVpcResponse_vpc,
    createVpcResponse_httpStatus,

    -- ** CreateVpcEndpoint
    createVpcEndpoint_clientToken,
    createVpcEndpoint_dnsOptions,
    createVpcEndpoint_dryRun,
    createVpcEndpoint_ipAddressType,
    createVpcEndpoint_policyDocument,
    createVpcEndpoint_privateDnsEnabled,
    createVpcEndpoint_routeTableIds,
    createVpcEndpoint_securityGroupIds,
    createVpcEndpoint_subnetIds,
    createVpcEndpoint_tagSpecifications,
    createVpcEndpoint_vpcEndpointType,
    createVpcEndpoint_vpcId,
    createVpcEndpoint_serviceName,
    createVpcEndpointResponse_clientToken,
    createVpcEndpointResponse_vpcEndpoint,
    createVpcEndpointResponse_httpStatus,

    -- ** CreateVpcEndpointConnectionNotification
    createVpcEndpointConnectionNotification_clientToken,
    createVpcEndpointConnectionNotification_dryRun,
    createVpcEndpointConnectionNotification_serviceId,
    createVpcEndpointConnectionNotification_vpcEndpointId,
    createVpcEndpointConnectionNotification_connectionNotificationArn,
    createVpcEndpointConnectionNotification_connectionEvents,
    createVpcEndpointConnectionNotificationResponse_clientToken,
    createVpcEndpointConnectionNotificationResponse_connectionNotification,
    createVpcEndpointConnectionNotificationResponse_httpStatus,

    -- ** CreateVpcEndpointServiceConfiguration
    createVpcEndpointServiceConfiguration_acceptanceRequired,
    createVpcEndpointServiceConfiguration_clientToken,
    createVpcEndpointServiceConfiguration_dryRun,
    createVpcEndpointServiceConfiguration_gatewayLoadBalancerArns,
    createVpcEndpointServiceConfiguration_networkLoadBalancerArns,
    createVpcEndpointServiceConfiguration_privateDnsName,
    createVpcEndpointServiceConfiguration_supportedIpAddressTypes,
    createVpcEndpointServiceConfiguration_tagSpecifications,
    createVpcEndpointServiceConfigurationResponse_clientToken,
    createVpcEndpointServiceConfigurationResponse_serviceConfiguration,
    createVpcEndpointServiceConfigurationResponse_httpStatus,

    -- ** CreateVpcPeeringConnection
    createVpcPeeringConnection_dryRun,
    createVpcPeeringConnection_peerOwnerId,
    createVpcPeeringConnection_peerRegion,
    createVpcPeeringConnection_peerVpcId,
    createVpcPeeringConnection_tagSpecifications,
    createVpcPeeringConnection_vpcId,
    createVpcPeeringConnectionResponse_vpcPeeringConnection,
    createVpcPeeringConnectionResponse_httpStatus,

    -- ** CreateVpnConnection
    createVpnConnection_dryRun,
    createVpnConnection_options,
    createVpnConnection_tagSpecifications,
    createVpnConnection_transitGatewayId,
    createVpnConnection_vpnGatewayId,
    createVpnConnection_customerGatewayId,
    createVpnConnection_type,
    createVpnConnectionResponse_vpnConnection,
    createVpnConnectionResponse_httpStatus,

    -- ** CreateVpnConnectionRoute
    createVpnConnectionRoute_destinationCidrBlock,
    createVpnConnectionRoute_vpnConnectionId,

    -- ** CreateVpnGateway
    createVpnGateway_amazonSideAsn,
    createVpnGateway_availabilityZone,
    createVpnGateway_dryRun,
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
    deleteClientVpnRoute_dryRun,
    deleteClientVpnRoute_targetVpcSubnetId,
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

    -- ** DeleteInstanceConnectEndpoint
    deleteInstanceConnectEndpoint_dryRun,
    deleteInstanceConnectEndpoint_instanceConnectEndpointId,
    deleteInstanceConnectEndpointResponse_instanceConnectEndpoint,
    deleteInstanceConnectEndpointResponse_httpStatus,

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
    deleteIpam_cascade,
    deleteIpam_dryRun,
    deleteIpam_ipamId,
    deleteIpamResponse_ipam,
    deleteIpamResponse_httpStatus,

    -- ** DeleteIpamPool
    deleteIpamPool_dryRun,
    deleteIpamPool_ipamPoolId,
    deleteIpamPoolResponse_ipamPool,
    deleteIpamPoolResponse_httpStatus,

    -- ** DeleteIpamResourceDiscovery
    deleteIpamResourceDiscovery_dryRun,
    deleteIpamResourceDiscovery_ipamResourceDiscoveryId,
    deleteIpamResourceDiscoveryResponse_ipamResourceDiscovery,
    deleteIpamResourceDiscoveryResponse_httpStatus,

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
    deleteLaunchTemplateVersionsResponse_successfullyDeletedLaunchTemplateVersions,
    deleteLaunchTemplateVersionsResponse_unsuccessfullyDeletedLaunchTemplateVersions,
    deleteLaunchTemplateVersionsResponse_httpStatus,

    -- ** DeleteLocalGatewayRoute
    deleteLocalGatewayRoute_destinationCidrBlock,
    deleteLocalGatewayRoute_destinationPrefixListId,
    deleteLocalGatewayRoute_dryRun,
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
    deleteRoute_destinationCidrBlock,
    deleteRoute_destinationIpv6CidrBlock,
    deleteRoute_destinationPrefixListId,
    deleteRoute_dryRun,
    deleteRoute_routeTableId,

    -- ** DeleteRouteTable
    deleteRouteTable_dryRun,
    deleteRouteTable_routeTableId,

    -- ** DeleteSecurityGroup
    deleteSecurityGroup_dryRun,
    deleteSecurityGroup_groupId,
    deleteSecurityGroup_groupName,

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
    deleteTags_dryRun,
    deleteTags_tags,
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

    -- ** DeleteVerifiedAccessEndpoint
    deleteVerifiedAccessEndpoint_clientToken,
    deleteVerifiedAccessEndpoint_dryRun,
    deleteVerifiedAccessEndpoint_verifiedAccessEndpointId,
    deleteVerifiedAccessEndpointResponse_verifiedAccessEndpoint,
    deleteVerifiedAccessEndpointResponse_httpStatus,

    -- ** DeleteVerifiedAccessGroup
    deleteVerifiedAccessGroup_clientToken,
    deleteVerifiedAccessGroup_dryRun,
    deleteVerifiedAccessGroup_verifiedAccessGroupId,
    deleteVerifiedAccessGroupResponse_verifiedAccessGroup,
    deleteVerifiedAccessGroupResponse_httpStatus,

    -- ** DeleteVerifiedAccessInstance
    deleteVerifiedAccessInstance_clientToken,
    deleteVerifiedAccessInstance_dryRun,
    deleteVerifiedAccessInstance_verifiedAccessInstanceId,
    deleteVerifiedAccessInstanceResponse_verifiedAccessInstance,
    deleteVerifiedAccessInstanceResponse_httpStatus,

    -- ** DeleteVerifiedAccessTrustProvider
    deleteVerifiedAccessTrustProvider_clientToken,
    deleteVerifiedAccessTrustProvider_dryRun,
    deleteVerifiedAccessTrustProvider_verifiedAccessTrustProviderId,
    deleteVerifiedAccessTrustProviderResponse_verifiedAccessTrustProvider,
    deleteVerifiedAccessTrustProviderResponse_httpStatus,

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
    deregisterInstanceEventNotificationAttributes_dryRun,
    deregisterInstanceEventNotificationAttributes_instanceTagAttribute,
    deregisterInstanceEventNotificationAttributesResponse_instanceTagAttribute,
    deregisterInstanceEventNotificationAttributesResponse_httpStatus,

    -- ** DeregisterTransitGatewayMulticastGroupMembers
    deregisterTransitGatewayMulticastGroupMembers_dryRun,
    deregisterTransitGatewayMulticastGroupMembers_groupIpAddress,
    deregisterTransitGatewayMulticastGroupMembers_networkInterfaceIds,
    deregisterTransitGatewayMulticastGroupMembers_transitGatewayMulticastDomainId,
    deregisterTransitGatewayMulticastGroupMembersResponse_deregisteredMulticastGroupMembers,
    deregisterTransitGatewayMulticastGroupMembersResponse_httpStatus,

    -- ** DeregisterTransitGatewayMulticastGroupSources
    deregisterTransitGatewayMulticastGroupSources_dryRun,
    deregisterTransitGatewayMulticastGroupSources_groupIpAddress,
    deregisterTransitGatewayMulticastGroupSources_networkInterfaceIds,
    deregisterTransitGatewayMulticastGroupSources_transitGatewayMulticastDomainId,
    deregisterTransitGatewayMulticastGroupSourcesResponse_deregisteredMulticastGroupSources,
    deregisterTransitGatewayMulticastGroupSourcesResponse_httpStatus,

    -- ** DescribeAccountAttributes
    describeAccountAttributes_attributeNames,
    describeAccountAttributes_dryRun,
    describeAccountAttributesResponse_accountAttributes,
    describeAccountAttributesResponse_httpStatus,

    -- ** DescribeAddressTransfers
    describeAddressTransfers_allocationIds,
    describeAddressTransfers_dryRun,
    describeAddressTransfers_maxResults,
    describeAddressTransfers_nextToken,
    describeAddressTransfersResponse_addressTransfers,
    describeAddressTransfersResponse_nextToken,
    describeAddressTransfersResponse_httpStatus,

    -- ** DescribeAddresses
    describeAddresses_allocationIds,
    describeAddresses_dryRun,
    describeAddresses_filters,
    describeAddresses_publicIps,
    describeAddressesResponse_addresses,
    describeAddressesResponse_httpStatus,

    -- ** DescribeAddressesAttribute
    describeAddressesAttribute_allocationIds,
    describeAddressesAttribute_attribute,
    describeAddressesAttribute_dryRun,
    describeAddressesAttribute_maxResults,
    describeAddressesAttribute_nextToken,
    describeAddressesAttributeResponse_addresses,
    describeAddressesAttributeResponse_nextToken,
    describeAddressesAttributeResponse_httpStatus,

    -- ** DescribeAggregateIdFormat
    describeAggregateIdFormat_dryRun,
    describeAggregateIdFormatResponse_statuses,
    describeAggregateIdFormatResponse_useLongIdsAggregated,
    describeAggregateIdFormatResponse_httpStatus,

    -- ** DescribeAvailabilityZones
    describeAvailabilityZones_allAvailabilityZones,
    describeAvailabilityZones_dryRun,
    describeAvailabilityZones_filters,
    describeAvailabilityZones_zoneIds,
    describeAvailabilityZones_zoneNames,
    describeAvailabilityZonesResponse_availabilityZones,
    describeAvailabilityZonesResponse_httpStatus,

    -- ** DescribeAwsNetworkPerformanceMetricSubscriptions
    describeAwsNetworkPerformanceMetricSubscriptions_dryRun,
    describeAwsNetworkPerformanceMetricSubscriptions_filters,
    describeAwsNetworkPerformanceMetricSubscriptions_maxResults,
    describeAwsNetworkPerformanceMetricSubscriptions_nextToken,
    describeAwsNetworkPerformanceMetricSubscriptionsResponse_nextToken,
    describeAwsNetworkPerformanceMetricSubscriptionsResponse_subscriptions,
    describeAwsNetworkPerformanceMetricSubscriptionsResponse_httpStatus,

    -- ** DescribeBundleTasks
    describeBundleTasks_bundleIds,
    describeBundleTasks_dryRun,
    describeBundleTasks_filters,
    describeBundleTasksResponse_bundleTasks,
    describeBundleTasksResponse_httpStatus,

    -- ** DescribeByoipCidrs
    describeByoipCidrs_dryRun,
    describeByoipCidrs_nextToken,
    describeByoipCidrs_maxResults,
    describeByoipCidrsResponse_byoipCidrs,
    describeByoipCidrsResponse_nextToken,
    describeByoipCidrsResponse_httpStatus,

    -- ** DescribeCapacityReservationFleets
    describeCapacityReservationFleets_capacityReservationFleetIds,
    describeCapacityReservationFleets_dryRun,
    describeCapacityReservationFleets_filters,
    describeCapacityReservationFleets_maxResults,
    describeCapacityReservationFleets_nextToken,
    describeCapacityReservationFleetsResponse_capacityReservationFleets,
    describeCapacityReservationFleetsResponse_nextToken,
    describeCapacityReservationFleetsResponse_httpStatus,

    -- ** DescribeCapacityReservations
    describeCapacityReservations_capacityReservationIds,
    describeCapacityReservations_dryRun,
    describeCapacityReservations_filters,
    describeCapacityReservations_maxResults,
    describeCapacityReservations_nextToken,
    describeCapacityReservationsResponse_capacityReservations,
    describeCapacityReservationsResponse_nextToken,
    describeCapacityReservationsResponse_httpStatus,

    -- ** DescribeCarrierGateways
    describeCarrierGateways_carrierGatewayIds,
    describeCarrierGateways_dryRun,
    describeCarrierGateways_filters,
    describeCarrierGateways_maxResults,
    describeCarrierGateways_nextToken,
    describeCarrierGatewaysResponse_carrierGateways,
    describeCarrierGatewaysResponse_nextToken,
    describeCarrierGatewaysResponse_httpStatus,

    -- ** DescribeClassicLinkInstances
    describeClassicLinkInstances_dryRun,
    describeClassicLinkInstances_filters,
    describeClassicLinkInstances_instanceIds,
    describeClassicLinkInstances_maxResults,
    describeClassicLinkInstances_nextToken,
    describeClassicLinkInstancesResponse_instances,
    describeClassicLinkInstancesResponse_nextToken,
    describeClassicLinkInstancesResponse_httpStatus,

    -- ** DescribeClientVpnAuthorizationRules
    describeClientVpnAuthorizationRules_dryRun,
    describeClientVpnAuthorizationRules_filters,
    describeClientVpnAuthorizationRules_maxResults,
    describeClientVpnAuthorizationRules_nextToken,
    describeClientVpnAuthorizationRules_clientVpnEndpointId,
    describeClientVpnAuthorizationRulesResponse_authorizationRules,
    describeClientVpnAuthorizationRulesResponse_nextToken,
    describeClientVpnAuthorizationRulesResponse_httpStatus,

    -- ** DescribeClientVpnConnections
    describeClientVpnConnections_dryRun,
    describeClientVpnConnections_filters,
    describeClientVpnConnections_maxResults,
    describeClientVpnConnections_nextToken,
    describeClientVpnConnections_clientVpnEndpointId,
    describeClientVpnConnectionsResponse_connections,
    describeClientVpnConnectionsResponse_nextToken,
    describeClientVpnConnectionsResponse_httpStatus,

    -- ** DescribeClientVpnEndpoints
    describeClientVpnEndpoints_clientVpnEndpointIds,
    describeClientVpnEndpoints_dryRun,
    describeClientVpnEndpoints_filters,
    describeClientVpnEndpoints_maxResults,
    describeClientVpnEndpoints_nextToken,
    describeClientVpnEndpointsResponse_clientVpnEndpoints,
    describeClientVpnEndpointsResponse_nextToken,
    describeClientVpnEndpointsResponse_httpStatus,

    -- ** DescribeClientVpnRoutes
    describeClientVpnRoutes_dryRun,
    describeClientVpnRoutes_filters,
    describeClientVpnRoutes_maxResults,
    describeClientVpnRoutes_nextToken,
    describeClientVpnRoutes_clientVpnEndpointId,
    describeClientVpnRoutesResponse_nextToken,
    describeClientVpnRoutesResponse_routes,
    describeClientVpnRoutesResponse_httpStatus,

    -- ** DescribeClientVpnTargetNetworks
    describeClientVpnTargetNetworks_associationIds,
    describeClientVpnTargetNetworks_dryRun,
    describeClientVpnTargetNetworks_filters,
    describeClientVpnTargetNetworks_maxResults,
    describeClientVpnTargetNetworks_nextToken,
    describeClientVpnTargetNetworks_clientVpnEndpointId,
    describeClientVpnTargetNetworksResponse_clientVpnTargetNetworks,
    describeClientVpnTargetNetworksResponse_nextToken,
    describeClientVpnTargetNetworksResponse_httpStatus,

    -- ** DescribeCoipPools
    describeCoipPools_dryRun,
    describeCoipPools_filters,
    describeCoipPools_maxResults,
    describeCoipPools_nextToken,
    describeCoipPools_poolIds,
    describeCoipPoolsResponse_coipPools,
    describeCoipPoolsResponse_nextToken,
    describeCoipPoolsResponse_httpStatus,

    -- ** DescribeConversionTasks
    describeConversionTasks_conversionTaskIds,
    describeConversionTasks_dryRun,
    describeConversionTasksResponse_conversionTasks,
    describeConversionTasksResponse_httpStatus,

    -- ** DescribeCustomerGateways
    describeCustomerGateways_customerGatewayIds,
    describeCustomerGateways_dryRun,
    describeCustomerGateways_filters,
    describeCustomerGatewaysResponse_customerGateways,
    describeCustomerGatewaysResponse_httpStatus,

    -- ** DescribeDhcpOptions
    describeDhcpOptions_dhcpOptionsIds,
    describeDhcpOptions_dryRun,
    describeDhcpOptions_filters,
    describeDhcpOptions_maxResults,
    describeDhcpOptions_nextToken,
    describeDhcpOptionsResponse_dhcpOptions,
    describeDhcpOptionsResponse_nextToken,
    describeDhcpOptionsResponse_httpStatus,

    -- ** DescribeEgressOnlyInternetGateways
    describeEgressOnlyInternetGateways_dryRun,
    describeEgressOnlyInternetGateways_egressOnlyInternetGatewayIds,
    describeEgressOnlyInternetGateways_filters,
    describeEgressOnlyInternetGateways_maxResults,
    describeEgressOnlyInternetGateways_nextToken,
    describeEgressOnlyInternetGatewaysResponse_egressOnlyInternetGateways,
    describeEgressOnlyInternetGatewaysResponse_nextToken,
    describeEgressOnlyInternetGatewaysResponse_httpStatus,

    -- ** DescribeElasticGpus
    describeElasticGpus_dryRun,
    describeElasticGpus_elasticGpuIds,
    describeElasticGpus_filters,
    describeElasticGpus_maxResults,
    describeElasticGpus_nextToken,
    describeElasticGpusResponse_elasticGpuSet,
    describeElasticGpusResponse_maxResults,
    describeElasticGpusResponse_nextToken,
    describeElasticGpusResponse_httpStatus,

    -- ** DescribeExportImageTasks
    describeExportImageTasks_dryRun,
    describeExportImageTasks_exportImageTaskIds,
    describeExportImageTasks_filters,
    describeExportImageTasks_maxResults,
    describeExportImageTasks_nextToken,
    describeExportImageTasksResponse_exportImageTasks,
    describeExportImageTasksResponse_nextToken,
    describeExportImageTasksResponse_httpStatus,

    -- ** DescribeExportTasks
    describeExportTasks_exportTaskIds,
    describeExportTasks_filters,
    describeExportTasksResponse_exportTasks,
    describeExportTasksResponse_httpStatus,

    -- ** DescribeFastLaunchImages
    describeFastLaunchImages_dryRun,
    describeFastLaunchImages_filters,
    describeFastLaunchImages_imageIds,
    describeFastLaunchImages_maxResults,
    describeFastLaunchImages_nextToken,
    describeFastLaunchImagesResponse_fastLaunchImages,
    describeFastLaunchImagesResponse_nextToken,
    describeFastLaunchImagesResponse_httpStatus,

    -- ** DescribeFastSnapshotRestores
    describeFastSnapshotRestores_dryRun,
    describeFastSnapshotRestores_filters,
    describeFastSnapshotRestores_maxResults,
    describeFastSnapshotRestores_nextToken,
    describeFastSnapshotRestoresResponse_fastSnapshotRestores,
    describeFastSnapshotRestoresResponse_nextToken,
    describeFastSnapshotRestoresResponse_httpStatus,

    -- ** DescribeFleetHistory
    describeFleetHistory_dryRun,
    describeFleetHistory_eventType,
    describeFleetHistory_maxResults,
    describeFleetHistory_nextToken,
    describeFleetHistory_fleetId,
    describeFleetHistory_startTime,
    describeFleetHistoryResponse_fleetId,
    describeFleetHistoryResponse_historyRecords,
    describeFleetHistoryResponse_lastEvaluatedTime,
    describeFleetHistoryResponse_nextToken,
    describeFleetHistoryResponse_startTime,
    describeFleetHistoryResponse_httpStatus,

    -- ** DescribeFleetInstances
    describeFleetInstances_dryRun,
    describeFleetInstances_filters,
    describeFleetInstances_maxResults,
    describeFleetInstances_nextToken,
    describeFleetInstances_fleetId,
    describeFleetInstancesResponse_activeInstances,
    describeFleetInstancesResponse_fleetId,
    describeFleetInstancesResponse_nextToken,
    describeFleetInstancesResponse_httpStatus,

    -- ** DescribeFleets
    describeFleets_dryRun,
    describeFleets_filters,
    describeFleets_fleetIds,
    describeFleets_maxResults,
    describeFleets_nextToken,
    describeFleetsResponse_fleets,
    describeFleetsResponse_nextToken,
    describeFleetsResponse_httpStatus,

    -- ** DescribeFlowLogs
    describeFlowLogs_dryRun,
    describeFlowLogs_filter,
    describeFlowLogs_flowLogIds,
    describeFlowLogs_maxResults,
    describeFlowLogs_nextToken,
    describeFlowLogsResponse_flowLogs,
    describeFlowLogsResponse_nextToken,
    describeFlowLogsResponse_httpStatus,

    -- ** DescribeFpgaImageAttribute
    describeFpgaImageAttribute_dryRun,
    describeFpgaImageAttribute_fpgaImageId,
    describeFpgaImageAttribute_attribute,
    describeFpgaImageAttributeResponse_fpgaImageAttribute,
    describeFpgaImageAttributeResponse_httpStatus,

    -- ** DescribeFpgaImages
    describeFpgaImages_dryRun,
    describeFpgaImages_filters,
    describeFpgaImages_fpgaImageIds,
    describeFpgaImages_maxResults,
    describeFpgaImages_nextToken,
    describeFpgaImages_owners,
    describeFpgaImagesResponse_fpgaImages,
    describeFpgaImagesResponse_nextToken,
    describeFpgaImagesResponse_httpStatus,

    -- ** DescribeHostReservationOfferings
    describeHostReservationOfferings_filter,
    describeHostReservationOfferings_maxDuration,
    describeHostReservationOfferings_maxResults,
    describeHostReservationOfferings_minDuration,
    describeHostReservationOfferings_nextToken,
    describeHostReservationOfferings_offeringId,
    describeHostReservationOfferingsResponse_nextToken,
    describeHostReservationOfferingsResponse_offeringSet,
    describeHostReservationOfferingsResponse_httpStatus,

    -- ** DescribeHostReservations
    describeHostReservations_filter,
    describeHostReservations_hostReservationIdSet,
    describeHostReservations_maxResults,
    describeHostReservations_nextToken,
    describeHostReservationsResponse_hostReservationSet,
    describeHostReservationsResponse_nextToken,
    describeHostReservationsResponse_httpStatus,

    -- ** DescribeHosts
    describeHosts_filter,
    describeHosts_hostIds,
    describeHosts_maxResults,
    describeHosts_nextToken,
    describeHostsResponse_hosts,
    describeHostsResponse_nextToken,
    describeHostsResponse_httpStatus,

    -- ** DescribeIamInstanceProfileAssociations
    describeIamInstanceProfileAssociations_associationIds,
    describeIamInstanceProfileAssociations_filters,
    describeIamInstanceProfileAssociations_maxResults,
    describeIamInstanceProfileAssociations_nextToken,
    describeIamInstanceProfileAssociationsResponse_iamInstanceProfileAssociations,
    describeIamInstanceProfileAssociationsResponse_nextToken,
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
    describeImageAttributeResponse_blockDeviceMappings,
    describeImageAttributeResponse_bootMode,
    describeImageAttributeResponse_description,
    describeImageAttributeResponse_imageId,
    describeImageAttributeResponse_imdsSupport,
    describeImageAttributeResponse_kernelId,
    describeImageAttributeResponse_lastLaunchedTime,
    describeImageAttributeResponse_launchPermissions,
    describeImageAttributeResponse_productCodes,
    describeImageAttributeResponse_ramdiskId,
    describeImageAttributeResponse_sriovNetSupport,
    describeImageAttributeResponse_tpmSupport,
    describeImageAttributeResponse_uefiData,
    describeImageAttributeResponse_httpStatus,

    -- ** DescribeImages
    describeImages_dryRun,
    describeImages_executableUsers,
    describeImages_filters,
    describeImages_imageIds,
    describeImages_includeDeprecated,
    describeImages_maxResults,
    describeImages_nextToken,
    describeImages_owners,
    describeImagesResponse_images,
    describeImagesResponse_nextToken,
    describeImagesResponse_httpStatus,

    -- ** DescribeImportImageTasks
    describeImportImageTasks_dryRun,
    describeImportImageTasks_filters,
    describeImportImageTasks_importTaskIds,
    describeImportImageTasks_maxResults,
    describeImportImageTasks_nextToken,
    describeImportImageTasksResponse_importImageTasks,
    describeImportImageTasksResponse_nextToken,
    describeImportImageTasksResponse_httpStatus,

    -- ** DescribeImportSnapshotTasks
    describeImportSnapshotTasks_dryRun,
    describeImportSnapshotTasks_filters,
    describeImportSnapshotTasks_importTaskIds,
    describeImportSnapshotTasks_maxResults,
    describeImportSnapshotTasks_nextToken,
    describeImportSnapshotTasksResponse_importSnapshotTasks,
    describeImportSnapshotTasksResponse_nextToken,
    describeImportSnapshotTasksResponse_httpStatus,

    -- ** DescribeInstanceAttribute
    describeInstanceAttribute_dryRun,
    describeInstanceAttribute_attribute,
    describeInstanceAttribute_instanceId,
    describeInstanceAttributeResponse_blockDeviceMappings,
    describeInstanceAttributeResponse_disableApiStop,
    describeInstanceAttributeResponse_disableApiTermination,
    describeInstanceAttributeResponse_ebsOptimized,
    describeInstanceAttributeResponse_enaSupport,
    describeInstanceAttributeResponse_enclaveOptions,
    describeInstanceAttributeResponse_groups,
    describeInstanceAttributeResponse_instanceId,
    describeInstanceAttributeResponse_instanceInitiatedShutdownBehavior,
    describeInstanceAttributeResponse_instanceType,
    describeInstanceAttributeResponse_kernelId,
    describeInstanceAttributeResponse_productCodes,
    describeInstanceAttributeResponse_ramdiskId,
    describeInstanceAttributeResponse_rootDeviceName,
    describeInstanceAttributeResponse_sourceDestCheck,
    describeInstanceAttributeResponse_sriovNetSupport,
    describeInstanceAttributeResponse_userData,
    describeInstanceAttributeResponse_httpStatus,

    -- ** DescribeInstanceConnectEndpoints
    describeInstanceConnectEndpoints_dryRun,
    describeInstanceConnectEndpoints_filters,
    describeInstanceConnectEndpoints_instanceConnectEndpointIds,
    describeInstanceConnectEndpoints_maxResults,
    describeInstanceConnectEndpoints_nextToken,
    describeInstanceConnectEndpointsResponse_instanceConnectEndpoints,
    describeInstanceConnectEndpointsResponse_nextToken,
    describeInstanceConnectEndpointsResponse_httpStatus,

    -- ** DescribeInstanceCreditSpecifications
    describeInstanceCreditSpecifications_dryRun,
    describeInstanceCreditSpecifications_filters,
    describeInstanceCreditSpecifications_instanceIds,
    describeInstanceCreditSpecifications_maxResults,
    describeInstanceCreditSpecifications_nextToken,
    describeInstanceCreditSpecificationsResponse_instanceCreditSpecifications,
    describeInstanceCreditSpecificationsResponse_nextToken,
    describeInstanceCreditSpecificationsResponse_httpStatus,

    -- ** DescribeInstanceEventNotificationAttributes
    describeInstanceEventNotificationAttributes_dryRun,
    describeInstanceEventNotificationAttributesResponse_instanceTagAttribute,
    describeInstanceEventNotificationAttributesResponse_httpStatus,

    -- ** DescribeInstanceEventWindows
    describeInstanceEventWindows_dryRun,
    describeInstanceEventWindows_filters,
    describeInstanceEventWindows_instanceEventWindowIds,
    describeInstanceEventWindows_maxResults,
    describeInstanceEventWindows_nextToken,
    describeInstanceEventWindowsResponse_instanceEventWindows,
    describeInstanceEventWindowsResponse_nextToken,
    describeInstanceEventWindowsResponse_httpStatus,

    -- ** DescribeInstanceStatus
    describeInstanceStatus_dryRun,
    describeInstanceStatus_filters,
    describeInstanceStatus_includeAllInstances,
    describeInstanceStatus_instanceIds,
    describeInstanceStatus_maxResults,
    describeInstanceStatus_nextToken,
    describeInstanceStatusResponse_instanceStatuses,
    describeInstanceStatusResponse_nextToken,
    describeInstanceStatusResponse_httpStatus,

    -- ** DescribeInstanceTypeOfferings
    describeInstanceTypeOfferings_dryRun,
    describeInstanceTypeOfferings_filters,
    describeInstanceTypeOfferings_locationType,
    describeInstanceTypeOfferings_maxResults,
    describeInstanceTypeOfferings_nextToken,
    describeInstanceTypeOfferingsResponse_instanceTypeOfferings,
    describeInstanceTypeOfferingsResponse_nextToken,
    describeInstanceTypeOfferingsResponse_httpStatus,

    -- ** DescribeInstanceTypes
    describeInstanceTypes_dryRun,
    describeInstanceTypes_filters,
    describeInstanceTypes_instanceTypes,
    describeInstanceTypes_maxResults,
    describeInstanceTypes_nextToken,
    describeInstanceTypesResponse_instanceTypes,
    describeInstanceTypesResponse_nextToken,
    describeInstanceTypesResponse_httpStatus,

    -- ** DescribeInstances
    describeInstances_dryRun,
    describeInstances_filters,
    describeInstances_instanceIds,
    describeInstances_maxResults,
    describeInstances_nextToken,
    describeInstancesResponse_nextToken,
    describeInstancesResponse_reservations,
    describeInstancesResponse_httpStatus,

    -- ** DescribeInternetGateways
    describeInternetGateways_dryRun,
    describeInternetGateways_filters,
    describeInternetGateways_internetGatewayIds,
    describeInternetGateways_maxResults,
    describeInternetGateways_nextToken,
    describeInternetGatewaysResponse_internetGateways,
    describeInternetGatewaysResponse_nextToken,
    describeInternetGatewaysResponse_httpStatus,

    -- ** DescribeIpamPools
    describeIpamPools_dryRun,
    describeIpamPools_filters,
    describeIpamPools_ipamPoolIds,
    describeIpamPools_maxResults,
    describeIpamPools_nextToken,
    describeIpamPoolsResponse_ipamPools,
    describeIpamPoolsResponse_nextToken,
    describeIpamPoolsResponse_httpStatus,

    -- ** DescribeIpamResourceDiscoveries
    describeIpamResourceDiscoveries_dryRun,
    describeIpamResourceDiscoveries_filters,
    describeIpamResourceDiscoveries_ipamResourceDiscoveryIds,
    describeIpamResourceDiscoveries_maxResults,
    describeIpamResourceDiscoveries_nextToken,
    describeIpamResourceDiscoveriesResponse_ipamResourceDiscoveries,
    describeIpamResourceDiscoveriesResponse_nextToken,
    describeIpamResourceDiscoveriesResponse_httpStatus,

    -- ** DescribeIpamResourceDiscoveryAssociations
    describeIpamResourceDiscoveryAssociations_dryRun,
    describeIpamResourceDiscoveryAssociations_filters,
    describeIpamResourceDiscoveryAssociations_ipamResourceDiscoveryAssociationIds,
    describeIpamResourceDiscoveryAssociations_maxResults,
    describeIpamResourceDiscoveryAssociations_nextToken,
    describeIpamResourceDiscoveryAssociationsResponse_ipamResourceDiscoveryAssociations,
    describeIpamResourceDiscoveryAssociationsResponse_nextToken,
    describeIpamResourceDiscoveryAssociationsResponse_httpStatus,

    -- ** DescribeIpamScopes
    describeIpamScopes_dryRun,
    describeIpamScopes_filters,
    describeIpamScopes_ipamScopeIds,
    describeIpamScopes_maxResults,
    describeIpamScopes_nextToken,
    describeIpamScopesResponse_ipamScopes,
    describeIpamScopesResponse_nextToken,
    describeIpamScopesResponse_httpStatus,

    -- ** DescribeIpams
    describeIpams_dryRun,
    describeIpams_filters,
    describeIpams_ipamIds,
    describeIpams_maxResults,
    describeIpams_nextToken,
    describeIpamsResponse_ipams,
    describeIpamsResponse_nextToken,
    describeIpamsResponse_httpStatus,

    -- ** DescribeIpv6Pools
    describeIpv6Pools_dryRun,
    describeIpv6Pools_filters,
    describeIpv6Pools_maxResults,
    describeIpv6Pools_nextToken,
    describeIpv6Pools_poolIds,
    describeIpv6PoolsResponse_ipv6Pools,
    describeIpv6PoolsResponse_nextToken,
    describeIpv6PoolsResponse_httpStatus,

    -- ** DescribeKeyPairs
    describeKeyPairs_dryRun,
    describeKeyPairs_filters,
    describeKeyPairs_includePublicKey,
    describeKeyPairs_keyNames,
    describeKeyPairs_keyPairIds,
    describeKeyPairsResponse_keyPairs,
    describeKeyPairsResponse_httpStatus,

    -- ** DescribeLaunchTemplateVersions
    describeLaunchTemplateVersions_dryRun,
    describeLaunchTemplateVersions_filters,
    describeLaunchTemplateVersions_launchTemplateId,
    describeLaunchTemplateVersions_launchTemplateName,
    describeLaunchTemplateVersions_maxResults,
    describeLaunchTemplateVersions_maxVersion,
    describeLaunchTemplateVersions_minVersion,
    describeLaunchTemplateVersions_nextToken,
    describeLaunchTemplateVersions_resolveAlias,
    describeLaunchTemplateVersions_versions,
    describeLaunchTemplateVersionsResponse_launchTemplateVersions,
    describeLaunchTemplateVersionsResponse_nextToken,
    describeLaunchTemplateVersionsResponse_httpStatus,

    -- ** DescribeLaunchTemplates
    describeLaunchTemplates_dryRun,
    describeLaunchTemplates_filters,
    describeLaunchTemplates_launchTemplateIds,
    describeLaunchTemplates_launchTemplateNames,
    describeLaunchTemplates_maxResults,
    describeLaunchTemplates_nextToken,
    describeLaunchTemplatesResponse_launchTemplates,
    describeLaunchTemplatesResponse_nextToken,
    describeLaunchTemplatesResponse_httpStatus,

    -- ** DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
    describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_dryRun,
    describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_filters,
    describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_localGatewayRouteTableVirtualInterfaceGroupAssociationIds,
    describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_maxResults,
    describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_nextToken,
    describeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse_localGatewayRouteTableVirtualInterfaceGroupAssociations,
    describeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse_nextToken,
    describeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse_httpStatus,

    -- ** DescribeLocalGatewayRouteTableVpcAssociations
    describeLocalGatewayRouteTableVpcAssociations_dryRun,
    describeLocalGatewayRouteTableVpcAssociations_filters,
    describeLocalGatewayRouteTableVpcAssociations_localGatewayRouteTableVpcAssociationIds,
    describeLocalGatewayRouteTableVpcAssociations_maxResults,
    describeLocalGatewayRouteTableVpcAssociations_nextToken,
    describeLocalGatewayRouteTableVpcAssociationsResponse_localGatewayRouteTableVpcAssociations,
    describeLocalGatewayRouteTableVpcAssociationsResponse_nextToken,
    describeLocalGatewayRouteTableVpcAssociationsResponse_httpStatus,

    -- ** DescribeLocalGatewayRouteTables
    describeLocalGatewayRouteTables_dryRun,
    describeLocalGatewayRouteTables_filters,
    describeLocalGatewayRouteTables_localGatewayRouteTableIds,
    describeLocalGatewayRouteTables_maxResults,
    describeLocalGatewayRouteTables_nextToken,
    describeLocalGatewayRouteTablesResponse_localGatewayRouteTables,
    describeLocalGatewayRouteTablesResponse_nextToken,
    describeLocalGatewayRouteTablesResponse_httpStatus,

    -- ** DescribeLocalGatewayVirtualInterfaceGroups
    describeLocalGatewayVirtualInterfaceGroups_dryRun,
    describeLocalGatewayVirtualInterfaceGroups_filters,
    describeLocalGatewayVirtualInterfaceGroups_localGatewayVirtualInterfaceGroupIds,
    describeLocalGatewayVirtualInterfaceGroups_maxResults,
    describeLocalGatewayVirtualInterfaceGroups_nextToken,
    describeLocalGatewayVirtualInterfaceGroupsResponse_localGatewayVirtualInterfaceGroups,
    describeLocalGatewayVirtualInterfaceGroupsResponse_nextToken,
    describeLocalGatewayVirtualInterfaceGroupsResponse_httpStatus,

    -- ** DescribeLocalGatewayVirtualInterfaces
    describeLocalGatewayVirtualInterfaces_dryRun,
    describeLocalGatewayVirtualInterfaces_filters,
    describeLocalGatewayVirtualInterfaces_localGatewayVirtualInterfaceIds,
    describeLocalGatewayVirtualInterfaces_maxResults,
    describeLocalGatewayVirtualInterfaces_nextToken,
    describeLocalGatewayVirtualInterfacesResponse_localGatewayVirtualInterfaces,
    describeLocalGatewayVirtualInterfacesResponse_nextToken,
    describeLocalGatewayVirtualInterfacesResponse_httpStatus,

    -- ** DescribeLocalGateways
    describeLocalGateways_dryRun,
    describeLocalGateways_filters,
    describeLocalGateways_localGatewayIds,
    describeLocalGateways_maxResults,
    describeLocalGateways_nextToken,
    describeLocalGatewaysResponse_localGateways,
    describeLocalGatewaysResponse_nextToken,
    describeLocalGatewaysResponse_httpStatus,

    -- ** DescribeManagedPrefixLists
    describeManagedPrefixLists_dryRun,
    describeManagedPrefixLists_filters,
    describeManagedPrefixLists_maxResults,
    describeManagedPrefixLists_nextToken,
    describeManagedPrefixLists_prefixListIds,
    describeManagedPrefixListsResponse_nextToken,
    describeManagedPrefixListsResponse_prefixLists,
    describeManagedPrefixListsResponse_httpStatus,

    -- ** DescribeMovingAddresses
    describeMovingAddresses_dryRun,
    describeMovingAddresses_filters,
    describeMovingAddresses_maxResults,
    describeMovingAddresses_nextToken,
    describeMovingAddresses_publicIps,
    describeMovingAddressesResponse_movingAddressStatuses,
    describeMovingAddressesResponse_nextToken,
    describeMovingAddressesResponse_httpStatus,

    -- ** DescribeNatGateways
    describeNatGateways_dryRun,
    describeNatGateways_filter,
    describeNatGateways_maxResults,
    describeNatGateways_natGatewayIds,
    describeNatGateways_nextToken,
    describeNatGatewaysResponse_natGateways,
    describeNatGatewaysResponse_nextToken,
    describeNatGatewaysResponse_httpStatus,

    -- ** DescribeNetworkAcls
    describeNetworkAcls_dryRun,
    describeNetworkAcls_filters,
    describeNetworkAcls_maxResults,
    describeNetworkAcls_networkAclIds,
    describeNetworkAcls_nextToken,
    describeNetworkAclsResponse_networkAcls,
    describeNetworkAclsResponse_nextToken,
    describeNetworkAclsResponse_httpStatus,

    -- ** DescribeNetworkInsightsAccessScopeAnalyses
    describeNetworkInsightsAccessScopeAnalyses_analysisStartTimeBegin,
    describeNetworkInsightsAccessScopeAnalyses_analysisStartTimeEnd,
    describeNetworkInsightsAccessScopeAnalyses_dryRun,
    describeNetworkInsightsAccessScopeAnalyses_filters,
    describeNetworkInsightsAccessScopeAnalyses_maxResults,
    describeNetworkInsightsAccessScopeAnalyses_networkInsightsAccessScopeAnalysisIds,
    describeNetworkInsightsAccessScopeAnalyses_networkInsightsAccessScopeId,
    describeNetworkInsightsAccessScopeAnalyses_nextToken,
    describeNetworkInsightsAccessScopeAnalysesResponse_networkInsightsAccessScopeAnalyses,
    describeNetworkInsightsAccessScopeAnalysesResponse_nextToken,
    describeNetworkInsightsAccessScopeAnalysesResponse_httpStatus,

    -- ** DescribeNetworkInsightsAccessScopes
    describeNetworkInsightsAccessScopes_dryRun,
    describeNetworkInsightsAccessScopes_filters,
    describeNetworkInsightsAccessScopes_maxResults,
    describeNetworkInsightsAccessScopes_networkInsightsAccessScopeIds,
    describeNetworkInsightsAccessScopes_nextToken,
    describeNetworkInsightsAccessScopesResponse_networkInsightsAccessScopes,
    describeNetworkInsightsAccessScopesResponse_nextToken,
    describeNetworkInsightsAccessScopesResponse_httpStatus,

    -- ** DescribeNetworkInsightsAnalyses
    describeNetworkInsightsAnalyses_analysisEndTime,
    describeNetworkInsightsAnalyses_analysisStartTime,
    describeNetworkInsightsAnalyses_dryRun,
    describeNetworkInsightsAnalyses_filters,
    describeNetworkInsightsAnalyses_maxResults,
    describeNetworkInsightsAnalyses_networkInsightsAnalysisIds,
    describeNetworkInsightsAnalyses_networkInsightsPathId,
    describeNetworkInsightsAnalyses_nextToken,
    describeNetworkInsightsAnalysesResponse_networkInsightsAnalyses,
    describeNetworkInsightsAnalysesResponse_nextToken,
    describeNetworkInsightsAnalysesResponse_httpStatus,

    -- ** DescribeNetworkInsightsPaths
    describeNetworkInsightsPaths_dryRun,
    describeNetworkInsightsPaths_filters,
    describeNetworkInsightsPaths_maxResults,
    describeNetworkInsightsPaths_networkInsightsPathIds,
    describeNetworkInsightsPaths_nextToken,
    describeNetworkInsightsPathsResponse_networkInsightsPaths,
    describeNetworkInsightsPathsResponse_nextToken,
    describeNetworkInsightsPathsResponse_httpStatus,

    -- ** DescribeNetworkInterfaceAttribute
    describeNetworkInterfaceAttribute_attribute,
    describeNetworkInterfaceAttribute_dryRun,
    describeNetworkInterfaceAttribute_networkInterfaceId,
    describeNetworkInterfaceAttributeResponse_attachment,
    describeNetworkInterfaceAttributeResponse_description,
    describeNetworkInterfaceAttributeResponse_groups,
    describeNetworkInterfaceAttributeResponse_networkInterfaceId,
    describeNetworkInterfaceAttributeResponse_sourceDestCheck,
    describeNetworkInterfaceAttributeResponse_httpStatus,

    -- ** DescribeNetworkInterfacePermissions
    describeNetworkInterfacePermissions_filters,
    describeNetworkInterfacePermissions_maxResults,
    describeNetworkInterfacePermissions_networkInterfacePermissionIds,
    describeNetworkInterfacePermissions_nextToken,
    describeNetworkInterfacePermissionsResponse_networkInterfacePermissions,
    describeNetworkInterfacePermissionsResponse_nextToken,
    describeNetworkInterfacePermissionsResponse_httpStatus,

    -- ** DescribeNetworkInterfaces
    describeNetworkInterfaces_dryRun,
    describeNetworkInterfaces_filters,
    describeNetworkInterfaces_maxResults,
    describeNetworkInterfaces_networkInterfaceIds,
    describeNetworkInterfaces_nextToken,
    describeNetworkInterfacesResponse_networkInterfaces,
    describeNetworkInterfacesResponse_nextToken,
    describeNetworkInterfacesResponse_httpStatus,

    -- ** DescribePlacementGroups
    describePlacementGroups_dryRun,
    describePlacementGroups_filters,
    describePlacementGroups_groupIds,
    describePlacementGroups_groupNames,
    describePlacementGroupsResponse_placementGroups,
    describePlacementGroupsResponse_httpStatus,

    -- ** DescribePrefixLists
    describePrefixLists_dryRun,
    describePrefixLists_filters,
    describePrefixLists_maxResults,
    describePrefixLists_nextToken,
    describePrefixLists_prefixListIds,
    describePrefixListsResponse_nextToken,
    describePrefixListsResponse_prefixLists,
    describePrefixListsResponse_httpStatus,

    -- ** DescribePrincipalIdFormat
    describePrincipalIdFormat_dryRun,
    describePrincipalIdFormat_maxResults,
    describePrincipalIdFormat_nextToken,
    describePrincipalIdFormat_resources,
    describePrincipalIdFormatResponse_nextToken,
    describePrincipalIdFormatResponse_principals,
    describePrincipalIdFormatResponse_httpStatus,

    -- ** DescribePublicIpv4Pools
    describePublicIpv4Pools_filters,
    describePublicIpv4Pools_maxResults,
    describePublicIpv4Pools_nextToken,
    describePublicIpv4Pools_poolIds,
    describePublicIpv4PoolsResponse_nextToken,
    describePublicIpv4PoolsResponse_publicIpv4Pools,
    describePublicIpv4PoolsResponse_httpStatus,

    -- ** DescribeRegions
    describeRegions_allRegions,
    describeRegions_dryRun,
    describeRegions_filters,
    describeRegions_regionNames,
    describeRegionsResponse_regions,
    describeRegionsResponse_httpStatus,

    -- ** DescribeReplaceRootVolumeTasks
    describeReplaceRootVolumeTasks_dryRun,
    describeReplaceRootVolumeTasks_filters,
    describeReplaceRootVolumeTasks_maxResults,
    describeReplaceRootVolumeTasks_nextToken,
    describeReplaceRootVolumeTasks_replaceRootVolumeTaskIds,
    describeReplaceRootVolumeTasksResponse_nextToken,
    describeReplaceRootVolumeTasksResponse_replaceRootVolumeTasks,
    describeReplaceRootVolumeTasksResponse_httpStatus,

    -- ** DescribeReservedInstances
    describeReservedInstances_dryRun,
    describeReservedInstances_filters,
    describeReservedInstances_offeringClass,
    describeReservedInstances_offeringType,
    describeReservedInstances_reservedInstancesIds,
    describeReservedInstancesResponse_reservedInstances,
    describeReservedInstancesResponse_httpStatus,

    -- ** DescribeReservedInstancesListings
    describeReservedInstancesListings_filters,
    describeReservedInstancesListings_reservedInstancesId,
    describeReservedInstancesListings_reservedInstancesListingId,
    describeReservedInstancesListingsResponse_reservedInstancesListings,
    describeReservedInstancesListingsResponse_httpStatus,

    -- ** DescribeReservedInstancesModifications
    describeReservedInstancesModifications_filters,
    describeReservedInstancesModifications_nextToken,
    describeReservedInstancesModifications_reservedInstancesModificationIds,
    describeReservedInstancesModificationsResponse_nextToken,
    describeReservedInstancesModificationsResponse_reservedInstancesModifications,
    describeReservedInstancesModificationsResponse_httpStatus,

    -- ** DescribeReservedInstancesOfferings
    describeReservedInstancesOfferings_availabilityZone,
    describeReservedInstancesOfferings_dryRun,
    describeReservedInstancesOfferings_filters,
    describeReservedInstancesOfferings_includeMarketplace,
    describeReservedInstancesOfferings_instanceTenancy,
    describeReservedInstancesOfferings_instanceType,
    describeReservedInstancesOfferings_maxDuration,
    describeReservedInstancesOfferings_maxInstanceCount,
    describeReservedInstancesOfferings_maxResults,
    describeReservedInstancesOfferings_minDuration,
    describeReservedInstancesOfferings_nextToken,
    describeReservedInstancesOfferings_offeringClass,
    describeReservedInstancesOfferings_offeringType,
    describeReservedInstancesOfferings_productDescription,
    describeReservedInstancesOfferings_reservedInstancesOfferingIds,
    describeReservedInstancesOfferingsResponse_nextToken,
    describeReservedInstancesOfferingsResponse_reservedInstancesOfferings,
    describeReservedInstancesOfferingsResponse_httpStatus,

    -- ** DescribeRouteTables
    describeRouteTables_dryRun,
    describeRouteTables_filters,
    describeRouteTables_maxResults,
    describeRouteTables_nextToken,
    describeRouteTables_routeTableIds,
    describeRouteTablesResponse_nextToken,
    describeRouteTablesResponse_routeTables,
    describeRouteTablesResponse_httpStatus,

    -- ** DescribeScheduledInstanceAvailability
    describeScheduledInstanceAvailability_dryRun,
    describeScheduledInstanceAvailability_filters,
    describeScheduledInstanceAvailability_maxResults,
    describeScheduledInstanceAvailability_maxSlotDurationInHours,
    describeScheduledInstanceAvailability_minSlotDurationInHours,
    describeScheduledInstanceAvailability_nextToken,
    describeScheduledInstanceAvailability_firstSlotStartTimeRange,
    describeScheduledInstanceAvailability_recurrence,
    describeScheduledInstanceAvailabilityResponse_nextToken,
    describeScheduledInstanceAvailabilityResponse_scheduledInstanceAvailabilitySet,
    describeScheduledInstanceAvailabilityResponse_httpStatus,

    -- ** DescribeScheduledInstances
    describeScheduledInstances_dryRun,
    describeScheduledInstances_filters,
    describeScheduledInstances_maxResults,
    describeScheduledInstances_nextToken,
    describeScheduledInstances_scheduledInstanceIds,
    describeScheduledInstances_slotStartTimeRange,
    describeScheduledInstancesResponse_nextToken,
    describeScheduledInstancesResponse_scheduledInstanceSet,
    describeScheduledInstancesResponse_httpStatus,

    -- ** DescribeSecurityGroupReferences
    describeSecurityGroupReferences_dryRun,
    describeSecurityGroupReferences_groupId,
    describeSecurityGroupReferencesResponse_securityGroupReferenceSet,
    describeSecurityGroupReferencesResponse_httpStatus,

    -- ** DescribeSecurityGroupRules
    describeSecurityGroupRules_dryRun,
    describeSecurityGroupRules_filters,
    describeSecurityGroupRules_maxResults,
    describeSecurityGroupRules_nextToken,
    describeSecurityGroupRules_securityGroupRuleIds,
    describeSecurityGroupRulesResponse_nextToken,
    describeSecurityGroupRulesResponse_securityGroupRules,
    describeSecurityGroupRulesResponse_httpStatus,

    -- ** DescribeSecurityGroups
    describeSecurityGroups_dryRun,
    describeSecurityGroups_filters,
    describeSecurityGroups_groupIds,
    describeSecurityGroups_groupNames,
    describeSecurityGroups_maxResults,
    describeSecurityGroups_nextToken,
    describeSecurityGroupsResponse_nextToken,
    describeSecurityGroupsResponse_securityGroups,
    describeSecurityGroupsResponse_httpStatus,

    -- ** DescribeSnapshotAttribute
    describeSnapshotAttribute_dryRun,
    describeSnapshotAttribute_attribute,
    describeSnapshotAttribute_snapshotId,
    describeSnapshotAttributeResponse_createVolumePermissions,
    describeSnapshotAttributeResponse_productCodes,
    describeSnapshotAttributeResponse_snapshotId,
    describeSnapshotAttributeResponse_httpStatus,

    -- ** DescribeSnapshotTierStatus
    describeSnapshotTierStatus_dryRun,
    describeSnapshotTierStatus_filters,
    describeSnapshotTierStatus_maxResults,
    describeSnapshotTierStatus_nextToken,
    describeSnapshotTierStatusResponse_nextToken,
    describeSnapshotTierStatusResponse_snapshotTierStatuses,
    describeSnapshotTierStatusResponse_httpStatus,

    -- ** DescribeSnapshots
    describeSnapshots_dryRun,
    describeSnapshots_filters,
    describeSnapshots_maxResults,
    describeSnapshots_nextToken,
    describeSnapshots_ownerIds,
    describeSnapshots_restorableByUserIds,
    describeSnapshots_snapshotIds,
    describeSnapshotsResponse_nextToken,
    describeSnapshotsResponse_snapshots,
    describeSnapshotsResponse_httpStatus,

    -- ** DescribeSpotDatafeedSubscription
    describeSpotDatafeedSubscription_dryRun,
    describeSpotDatafeedSubscriptionResponse_spotDatafeedSubscription,
    describeSpotDatafeedSubscriptionResponse_httpStatus,

    -- ** DescribeSpotFleetInstances
    describeSpotFleetInstances_dryRun,
    describeSpotFleetInstances_maxResults,
    describeSpotFleetInstances_nextToken,
    describeSpotFleetInstances_spotFleetRequestId,
    describeSpotFleetInstancesResponse_activeInstances,
    describeSpotFleetInstancesResponse_nextToken,
    describeSpotFleetInstancesResponse_spotFleetRequestId,
    describeSpotFleetInstancesResponse_httpStatus,

    -- ** DescribeSpotFleetRequestHistory
    describeSpotFleetRequestHistory_dryRun,
    describeSpotFleetRequestHistory_eventType,
    describeSpotFleetRequestHistory_maxResults,
    describeSpotFleetRequestHistory_nextToken,
    describeSpotFleetRequestHistory_spotFleetRequestId,
    describeSpotFleetRequestHistory_startTime,
    describeSpotFleetRequestHistoryResponse_historyRecords,
    describeSpotFleetRequestHistoryResponse_lastEvaluatedTime,
    describeSpotFleetRequestHistoryResponse_nextToken,
    describeSpotFleetRequestHistoryResponse_spotFleetRequestId,
    describeSpotFleetRequestHistoryResponse_startTime,
    describeSpotFleetRequestHistoryResponse_httpStatus,

    -- ** DescribeSpotFleetRequests
    describeSpotFleetRequests_dryRun,
    describeSpotFleetRequests_maxResults,
    describeSpotFleetRequests_nextToken,
    describeSpotFleetRequests_spotFleetRequestIds,
    describeSpotFleetRequestsResponse_nextToken,
    describeSpotFleetRequestsResponse_spotFleetRequestConfigs,
    describeSpotFleetRequestsResponse_httpStatus,

    -- ** DescribeSpotInstanceRequests
    describeSpotInstanceRequests_dryRun,
    describeSpotInstanceRequests_filters,
    describeSpotInstanceRequests_maxResults,
    describeSpotInstanceRequests_nextToken,
    describeSpotInstanceRequests_spotInstanceRequestIds,
    describeSpotInstanceRequestsResponse_nextToken,
    describeSpotInstanceRequestsResponse_spotInstanceRequests,
    describeSpotInstanceRequestsResponse_httpStatus,

    -- ** DescribeSpotPriceHistory
    describeSpotPriceHistory_availabilityZone,
    describeSpotPriceHistory_dryRun,
    describeSpotPriceHistory_endTime,
    describeSpotPriceHistory_filters,
    describeSpotPriceHistory_instanceTypes,
    describeSpotPriceHistory_maxResults,
    describeSpotPriceHistory_nextToken,
    describeSpotPriceHistory_productDescriptions,
    describeSpotPriceHistory_startTime,
    describeSpotPriceHistoryResponse_nextToken,
    describeSpotPriceHistoryResponse_spotPriceHistory,
    describeSpotPriceHistoryResponse_httpStatus,

    -- ** DescribeStaleSecurityGroups
    describeStaleSecurityGroups_dryRun,
    describeStaleSecurityGroups_maxResults,
    describeStaleSecurityGroups_nextToken,
    describeStaleSecurityGroups_vpcId,
    describeStaleSecurityGroupsResponse_nextToken,
    describeStaleSecurityGroupsResponse_staleSecurityGroupSet,
    describeStaleSecurityGroupsResponse_httpStatus,

    -- ** DescribeStoreImageTasks
    describeStoreImageTasks_dryRun,
    describeStoreImageTasks_filters,
    describeStoreImageTasks_imageIds,
    describeStoreImageTasks_maxResults,
    describeStoreImageTasks_nextToken,
    describeStoreImageTasksResponse_nextToken,
    describeStoreImageTasksResponse_storeImageTaskResults,
    describeStoreImageTasksResponse_httpStatus,

    -- ** DescribeSubnets
    describeSubnets_dryRun,
    describeSubnets_filters,
    describeSubnets_maxResults,
    describeSubnets_nextToken,
    describeSubnets_subnetIds,
    describeSubnetsResponse_nextToken,
    describeSubnetsResponse_subnets,
    describeSubnetsResponse_httpStatus,

    -- ** DescribeTags
    describeTags_dryRun,
    describeTags_filters,
    describeTags_maxResults,
    describeTags_nextToken,
    describeTagsResponse_nextToken,
    describeTagsResponse_tags,
    describeTagsResponse_httpStatus,

    -- ** DescribeTrafficMirrorFilters
    describeTrafficMirrorFilters_dryRun,
    describeTrafficMirrorFilters_filters,
    describeTrafficMirrorFilters_maxResults,
    describeTrafficMirrorFilters_nextToken,
    describeTrafficMirrorFilters_trafficMirrorFilterIds,
    describeTrafficMirrorFiltersResponse_nextToken,
    describeTrafficMirrorFiltersResponse_trafficMirrorFilters,
    describeTrafficMirrorFiltersResponse_httpStatus,

    -- ** DescribeTrafficMirrorSessions
    describeTrafficMirrorSessions_dryRun,
    describeTrafficMirrorSessions_filters,
    describeTrafficMirrorSessions_maxResults,
    describeTrafficMirrorSessions_nextToken,
    describeTrafficMirrorSessions_trafficMirrorSessionIds,
    describeTrafficMirrorSessionsResponse_nextToken,
    describeTrafficMirrorSessionsResponse_trafficMirrorSessions,
    describeTrafficMirrorSessionsResponse_httpStatus,

    -- ** DescribeTrafficMirrorTargets
    describeTrafficMirrorTargets_dryRun,
    describeTrafficMirrorTargets_filters,
    describeTrafficMirrorTargets_maxResults,
    describeTrafficMirrorTargets_nextToken,
    describeTrafficMirrorTargets_trafficMirrorTargetIds,
    describeTrafficMirrorTargetsResponse_nextToken,
    describeTrafficMirrorTargetsResponse_trafficMirrorTargets,
    describeTrafficMirrorTargetsResponse_httpStatus,

    -- ** DescribeTransitGatewayAttachments
    describeTransitGatewayAttachments_dryRun,
    describeTransitGatewayAttachments_filters,
    describeTransitGatewayAttachments_maxResults,
    describeTransitGatewayAttachments_nextToken,
    describeTransitGatewayAttachments_transitGatewayAttachmentIds,
    describeTransitGatewayAttachmentsResponse_nextToken,
    describeTransitGatewayAttachmentsResponse_transitGatewayAttachments,
    describeTransitGatewayAttachmentsResponse_httpStatus,

    -- ** DescribeTransitGatewayConnectPeers
    describeTransitGatewayConnectPeers_dryRun,
    describeTransitGatewayConnectPeers_filters,
    describeTransitGatewayConnectPeers_maxResults,
    describeTransitGatewayConnectPeers_nextToken,
    describeTransitGatewayConnectPeers_transitGatewayConnectPeerIds,
    describeTransitGatewayConnectPeersResponse_nextToken,
    describeTransitGatewayConnectPeersResponse_transitGatewayConnectPeers,
    describeTransitGatewayConnectPeersResponse_httpStatus,

    -- ** DescribeTransitGatewayConnects
    describeTransitGatewayConnects_dryRun,
    describeTransitGatewayConnects_filters,
    describeTransitGatewayConnects_maxResults,
    describeTransitGatewayConnects_nextToken,
    describeTransitGatewayConnects_transitGatewayAttachmentIds,
    describeTransitGatewayConnectsResponse_nextToken,
    describeTransitGatewayConnectsResponse_transitGatewayConnects,
    describeTransitGatewayConnectsResponse_httpStatus,

    -- ** DescribeTransitGatewayMulticastDomains
    describeTransitGatewayMulticastDomains_dryRun,
    describeTransitGatewayMulticastDomains_filters,
    describeTransitGatewayMulticastDomains_maxResults,
    describeTransitGatewayMulticastDomains_nextToken,
    describeTransitGatewayMulticastDomains_transitGatewayMulticastDomainIds,
    describeTransitGatewayMulticastDomainsResponse_nextToken,
    describeTransitGatewayMulticastDomainsResponse_transitGatewayMulticastDomains,
    describeTransitGatewayMulticastDomainsResponse_httpStatus,

    -- ** DescribeTransitGatewayPeeringAttachments
    describeTransitGatewayPeeringAttachments_dryRun,
    describeTransitGatewayPeeringAttachments_filters,
    describeTransitGatewayPeeringAttachments_maxResults,
    describeTransitGatewayPeeringAttachments_nextToken,
    describeTransitGatewayPeeringAttachments_transitGatewayAttachmentIds,
    describeTransitGatewayPeeringAttachmentsResponse_nextToken,
    describeTransitGatewayPeeringAttachmentsResponse_transitGatewayPeeringAttachments,
    describeTransitGatewayPeeringAttachmentsResponse_httpStatus,

    -- ** DescribeTransitGatewayPolicyTables
    describeTransitGatewayPolicyTables_dryRun,
    describeTransitGatewayPolicyTables_filters,
    describeTransitGatewayPolicyTables_maxResults,
    describeTransitGatewayPolicyTables_nextToken,
    describeTransitGatewayPolicyTables_transitGatewayPolicyTableIds,
    describeTransitGatewayPolicyTablesResponse_nextToken,
    describeTransitGatewayPolicyTablesResponse_transitGatewayPolicyTables,
    describeTransitGatewayPolicyTablesResponse_httpStatus,

    -- ** DescribeTransitGatewayRouteTableAnnouncements
    describeTransitGatewayRouteTableAnnouncements_dryRun,
    describeTransitGatewayRouteTableAnnouncements_filters,
    describeTransitGatewayRouteTableAnnouncements_maxResults,
    describeTransitGatewayRouteTableAnnouncements_nextToken,
    describeTransitGatewayRouteTableAnnouncements_transitGatewayRouteTableAnnouncementIds,
    describeTransitGatewayRouteTableAnnouncementsResponse_nextToken,
    describeTransitGatewayRouteTableAnnouncementsResponse_transitGatewayRouteTableAnnouncements,
    describeTransitGatewayRouteTableAnnouncementsResponse_httpStatus,

    -- ** DescribeTransitGatewayRouteTables
    describeTransitGatewayRouteTables_dryRun,
    describeTransitGatewayRouteTables_filters,
    describeTransitGatewayRouteTables_maxResults,
    describeTransitGatewayRouteTables_nextToken,
    describeTransitGatewayRouteTables_transitGatewayRouteTableIds,
    describeTransitGatewayRouteTablesResponse_nextToken,
    describeTransitGatewayRouteTablesResponse_transitGatewayRouteTables,
    describeTransitGatewayRouteTablesResponse_httpStatus,

    -- ** DescribeTransitGatewayVpcAttachments
    describeTransitGatewayVpcAttachments_dryRun,
    describeTransitGatewayVpcAttachments_filters,
    describeTransitGatewayVpcAttachments_maxResults,
    describeTransitGatewayVpcAttachments_nextToken,
    describeTransitGatewayVpcAttachments_transitGatewayAttachmentIds,
    describeTransitGatewayVpcAttachmentsResponse_nextToken,
    describeTransitGatewayVpcAttachmentsResponse_transitGatewayVpcAttachments,
    describeTransitGatewayVpcAttachmentsResponse_httpStatus,

    -- ** DescribeTransitGateways
    describeTransitGateways_dryRun,
    describeTransitGateways_filters,
    describeTransitGateways_maxResults,
    describeTransitGateways_nextToken,
    describeTransitGateways_transitGatewayIds,
    describeTransitGatewaysResponse_nextToken,
    describeTransitGatewaysResponse_transitGateways,
    describeTransitGatewaysResponse_httpStatus,

    -- ** DescribeTrunkInterfaceAssociations
    describeTrunkInterfaceAssociations_associationIds,
    describeTrunkInterfaceAssociations_dryRun,
    describeTrunkInterfaceAssociations_filters,
    describeTrunkInterfaceAssociations_maxResults,
    describeTrunkInterfaceAssociations_nextToken,
    describeTrunkInterfaceAssociationsResponse_interfaceAssociations,
    describeTrunkInterfaceAssociationsResponse_nextToken,
    describeTrunkInterfaceAssociationsResponse_httpStatus,

    -- ** DescribeVerifiedAccessEndpoints
    describeVerifiedAccessEndpoints_dryRun,
    describeVerifiedAccessEndpoints_filters,
    describeVerifiedAccessEndpoints_maxResults,
    describeVerifiedAccessEndpoints_nextToken,
    describeVerifiedAccessEndpoints_verifiedAccessEndpointIds,
    describeVerifiedAccessEndpoints_verifiedAccessGroupId,
    describeVerifiedAccessEndpoints_verifiedAccessInstanceId,
    describeVerifiedAccessEndpointsResponse_nextToken,
    describeVerifiedAccessEndpointsResponse_verifiedAccessEndpoints,
    describeVerifiedAccessEndpointsResponse_httpStatus,

    -- ** DescribeVerifiedAccessGroups
    describeVerifiedAccessGroups_dryRun,
    describeVerifiedAccessGroups_filters,
    describeVerifiedAccessGroups_maxResults,
    describeVerifiedAccessGroups_nextToken,
    describeVerifiedAccessGroups_verifiedAccessGroupIds,
    describeVerifiedAccessGroups_verifiedAccessInstanceId,
    describeVerifiedAccessGroupsResponse_nextToken,
    describeVerifiedAccessGroupsResponse_verifiedAccessGroups,
    describeVerifiedAccessGroupsResponse_httpStatus,

    -- ** DescribeVerifiedAccessInstanceLoggingConfigurations
    describeVerifiedAccessInstanceLoggingConfigurations_dryRun,
    describeVerifiedAccessInstanceLoggingConfigurations_filters,
    describeVerifiedAccessInstanceLoggingConfigurations_maxResults,
    describeVerifiedAccessInstanceLoggingConfigurations_nextToken,
    describeVerifiedAccessInstanceLoggingConfigurations_verifiedAccessInstanceIds,
    describeVerifiedAccessInstanceLoggingConfigurationsResponse_loggingConfigurations,
    describeVerifiedAccessInstanceLoggingConfigurationsResponse_nextToken,
    describeVerifiedAccessInstanceLoggingConfigurationsResponse_httpStatus,

    -- ** DescribeVerifiedAccessInstances
    describeVerifiedAccessInstances_dryRun,
    describeVerifiedAccessInstances_filters,
    describeVerifiedAccessInstances_maxResults,
    describeVerifiedAccessInstances_nextToken,
    describeVerifiedAccessInstances_verifiedAccessInstanceIds,
    describeVerifiedAccessInstancesResponse_nextToken,
    describeVerifiedAccessInstancesResponse_verifiedAccessInstances,
    describeVerifiedAccessInstancesResponse_httpStatus,

    -- ** DescribeVerifiedAccessTrustProviders
    describeVerifiedAccessTrustProviders_dryRun,
    describeVerifiedAccessTrustProviders_filters,
    describeVerifiedAccessTrustProviders_maxResults,
    describeVerifiedAccessTrustProviders_nextToken,
    describeVerifiedAccessTrustProviders_verifiedAccessTrustProviderIds,
    describeVerifiedAccessTrustProvidersResponse_nextToken,
    describeVerifiedAccessTrustProvidersResponse_verifiedAccessTrustProviders,
    describeVerifiedAccessTrustProvidersResponse_httpStatus,

    -- ** DescribeVolumeAttribute
    describeVolumeAttribute_dryRun,
    describeVolumeAttribute_attribute,
    describeVolumeAttribute_volumeId,
    describeVolumeAttributeResponse_autoEnableIO,
    describeVolumeAttributeResponse_productCodes,
    describeVolumeAttributeResponse_volumeId,
    describeVolumeAttributeResponse_httpStatus,

    -- ** DescribeVolumeStatus
    describeVolumeStatus_dryRun,
    describeVolumeStatus_filters,
    describeVolumeStatus_maxResults,
    describeVolumeStatus_nextToken,
    describeVolumeStatus_volumeIds,
    describeVolumeStatusResponse_nextToken,
    describeVolumeStatusResponse_volumeStatuses,
    describeVolumeStatusResponse_httpStatus,

    -- ** DescribeVolumes
    describeVolumes_dryRun,
    describeVolumes_filters,
    describeVolumes_maxResults,
    describeVolumes_nextToken,
    describeVolumes_volumeIds,
    describeVolumesResponse_nextToken,
    describeVolumesResponse_volumes,
    describeVolumesResponse_httpStatus,

    -- ** DescribeVolumesModifications
    describeVolumesModifications_dryRun,
    describeVolumesModifications_filters,
    describeVolumesModifications_maxResults,
    describeVolumesModifications_nextToken,
    describeVolumesModifications_volumeIds,
    describeVolumesModificationsResponse_nextToken,
    describeVolumesModificationsResponse_volumesModifications,
    describeVolumesModificationsResponse_httpStatus,

    -- ** DescribeVpcAttribute
    describeVpcAttribute_dryRun,
    describeVpcAttribute_attribute,
    describeVpcAttribute_vpcId,
    describeVpcAttributeResponse_enableDnsHostnames,
    describeVpcAttributeResponse_enableDnsSupport,
    describeVpcAttributeResponse_enableNetworkAddressUsageMetrics,
    describeVpcAttributeResponse_vpcId,
    describeVpcAttributeResponse_httpStatus,

    -- ** DescribeVpcClassicLink
    describeVpcClassicLink_dryRun,
    describeVpcClassicLink_filters,
    describeVpcClassicLink_vpcIds,
    describeVpcClassicLinkResponse_vpcs,
    describeVpcClassicLinkResponse_httpStatus,

    -- ** DescribeVpcClassicLinkDnsSupport
    describeVpcClassicLinkDnsSupport_maxResults,
    describeVpcClassicLinkDnsSupport_nextToken,
    describeVpcClassicLinkDnsSupport_vpcIds,
    describeVpcClassicLinkDnsSupportResponse_nextToken,
    describeVpcClassicLinkDnsSupportResponse_vpcs,
    describeVpcClassicLinkDnsSupportResponse_httpStatus,

    -- ** DescribeVpcEndpointConnectionNotifications
    describeVpcEndpointConnectionNotifications_connectionNotificationId,
    describeVpcEndpointConnectionNotifications_dryRun,
    describeVpcEndpointConnectionNotifications_filters,
    describeVpcEndpointConnectionNotifications_maxResults,
    describeVpcEndpointConnectionNotifications_nextToken,
    describeVpcEndpointConnectionNotificationsResponse_connectionNotificationSet,
    describeVpcEndpointConnectionNotificationsResponse_nextToken,
    describeVpcEndpointConnectionNotificationsResponse_httpStatus,

    -- ** DescribeVpcEndpointConnections
    describeVpcEndpointConnections_dryRun,
    describeVpcEndpointConnections_filters,
    describeVpcEndpointConnections_maxResults,
    describeVpcEndpointConnections_nextToken,
    describeVpcEndpointConnectionsResponse_nextToken,
    describeVpcEndpointConnectionsResponse_vpcEndpointConnections,
    describeVpcEndpointConnectionsResponse_httpStatus,

    -- ** DescribeVpcEndpointServiceConfigurations
    describeVpcEndpointServiceConfigurations_dryRun,
    describeVpcEndpointServiceConfigurations_filters,
    describeVpcEndpointServiceConfigurations_maxResults,
    describeVpcEndpointServiceConfigurations_nextToken,
    describeVpcEndpointServiceConfigurations_serviceIds,
    describeVpcEndpointServiceConfigurationsResponse_nextToken,
    describeVpcEndpointServiceConfigurationsResponse_serviceConfigurations,
    describeVpcEndpointServiceConfigurationsResponse_httpStatus,

    -- ** DescribeVpcEndpointServicePermissions
    describeVpcEndpointServicePermissions_dryRun,
    describeVpcEndpointServicePermissions_filters,
    describeVpcEndpointServicePermissions_maxResults,
    describeVpcEndpointServicePermissions_nextToken,
    describeVpcEndpointServicePermissions_serviceId,
    describeVpcEndpointServicePermissionsResponse_allowedPrincipals,
    describeVpcEndpointServicePermissionsResponse_nextToken,
    describeVpcEndpointServicePermissionsResponse_httpStatus,

    -- ** DescribeVpcEndpointServices
    describeVpcEndpointServices_dryRun,
    describeVpcEndpointServices_filters,
    describeVpcEndpointServices_maxResults,
    describeVpcEndpointServices_nextToken,
    describeVpcEndpointServices_serviceNames,
    describeVpcEndpointServicesResponse_nextToken,
    describeVpcEndpointServicesResponse_serviceDetails,
    describeVpcEndpointServicesResponse_serviceNames,
    describeVpcEndpointServicesResponse_httpStatus,

    -- ** DescribeVpcEndpoints
    describeVpcEndpoints_dryRun,
    describeVpcEndpoints_filters,
    describeVpcEndpoints_maxResults,
    describeVpcEndpoints_nextToken,
    describeVpcEndpoints_vpcEndpointIds,
    describeVpcEndpointsResponse_nextToken,
    describeVpcEndpointsResponse_vpcEndpoints,
    describeVpcEndpointsResponse_httpStatus,

    -- ** DescribeVpcPeeringConnections
    describeVpcPeeringConnections_dryRun,
    describeVpcPeeringConnections_filters,
    describeVpcPeeringConnections_maxResults,
    describeVpcPeeringConnections_nextToken,
    describeVpcPeeringConnections_vpcPeeringConnectionIds,
    describeVpcPeeringConnectionsResponse_nextToken,
    describeVpcPeeringConnectionsResponse_vpcPeeringConnections,
    describeVpcPeeringConnectionsResponse_httpStatus,

    -- ** DescribeVpcs
    describeVpcs_dryRun,
    describeVpcs_filters,
    describeVpcs_maxResults,
    describeVpcs_nextToken,
    describeVpcs_vpcIds,
    describeVpcsResponse_nextToken,
    describeVpcsResponse_vpcs,
    describeVpcsResponse_httpStatus,

    -- ** DescribeVpnConnections
    describeVpnConnections_dryRun,
    describeVpnConnections_filters,
    describeVpnConnections_vpnConnectionIds,
    describeVpnConnectionsResponse_vpnConnections,
    describeVpnConnectionsResponse_httpStatus,

    -- ** DescribeVpnGateways
    describeVpnGateways_dryRun,
    describeVpnGateways_filters,
    describeVpnGateways_vpnGatewayIds,
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

    -- ** DetachVerifiedAccessTrustProvider
    detachVerifiedAccessTrustProvider_clientToken,
    detachVerifiedAccessTrustProvider_dryRun,
    detachVerifiedAccessTrustProvider_verifiedAccessInstanceId,
    detachVerifiedAccessTrustProvider_verifiedAccessTrustProviderId,
    detachVerifiedAccessTrustProviderResponse_verifiedAccessInstance,
    detachVerifiedAccessTrustProviderResponse_verifiedAccessTrustProvider,
    detachVerifiedAccessTrustProviderResponse_httpStatus,

    -- ** DetachVolume
    detachVolume_device,
    detachVolume_dryRun,
    detachVolume_force,
    detachVolume_instanceId,
    detachVolume_volumeId,
    volumeAttachment_attachTime,
    volumeAttachment_deleteOnTermination,
    volumeAttachment_device,
    volumeAttachment_instanceId,
    volumeAttachment_state,
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

    -- ** DisableAwsNetworkPerformanceMetricSubscription
    disableAwsNetworkPerformanceMetricSubscription_destination,
    disableAwsNetworkPerformanceMetricSubscription_dryRun,
    disableAwsNetworkPerformanceMetricSubscription_metric,
    disableAwsNetworkPerformanceMetricSubscription_source,
    disableAwsNetworkPerformanceMetricSubscription_statistic,
    disableAwsNetworkPerformanceMetricSubscriptionResponse_output,
    disableAwsNetworkPerformanceMetricSubscriptionResponse_httpStatus,

    -- ** DisableEbsEncryptionByDefault
    disableEbsEncryptionByDefault_dryRun,
    disableEbsEncryptionByDefaultResponse_ebsEncryptionByDefault,
    disableEbsEncryptionByDefaultResponse_httpStatus,

    -- ** DisableFastLaunch
    disableFastLaunch_dryRun,
    disableFastLaunch_force,
    disableFastLaunch_imageId,
    disableFastLaunchResponse_imageId,
    disableFastLaunchResponse_launchTemplate,
    disableFastLaunchResponse_maxParallelLaunches,
    disableFastLaunchResponse_ownerId,
    disableFastLaunchResponse_resourceType,
    disableFastLaunchResponse_snapshotConfiguration,
    disableFastLaunchResponse_state,
    disableFastLaunchResponse_stateTransitionReason,
    disableFastLaunchResponse_stateTransitionTime,
    disableFastLaunchResponse_httpStatus,

    -- ** DisableFastSnapshotRestores
    disableFastSnapshotRestores_dryRun,
    disableFastSnapshotRestores_availabilityZones,
    disableFastSnapshotRestores_sourceSnapshotIds,
    disableFastSnapshotRestoresResponse_successful,
    disableFastSnapshotRestoresResponse_unsuccessful,
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
    disableTransitGatewayRouteTablePropagation_dryRun,
    disableTransitGatewayRouteTablePropagation_transitGatewayAttachmentId,
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
    disassociateAddress_associationId,
    disassociateAddress_dryRun,
    disassociateAddress_publicIp,

    -- ** DisassociateClientVpnTargetNetwork
    disassociateClientVpnTargetNetwork_dryRun,
    disassociateClientVpnTargetNetwork_clientVpnEndpointId,
    disassociateClientVpnTargetNetwork_associationId,
    disassociateClientVpnTargetNetworkResponse_associationId,
    disassociateClientVpnTargetNetworkResponse_status,
    disassociateClientVpnTargetNetworkResponse_httpStatus,

    -- ** DisassociateEnclaveCertificateIamRole
    disassociateEnclaveCertificateIamRole_dryRun,
    disassociateEnclaveCertificateIamRole_certificateArn,
    disassociateEnclaveCertificateIamRole_roleArn,
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

    -- ** DisassociateIpamResourceDiscovery
    disassociateIpamResourceDiscovery_dryRun,
    disassociateIpamResourceDiscovery_ipamResourceDiscoveryAssociationId,
    disassociateIpamResourceDiscoveryResponse_ipamResourceDiscoveryAssociation,
    disassociateIpamResourceDiscoveryResponse_httpStatus,

    -- ** DisassociateNatGatewayAddress
    disassociateNatGatewayAddress_dryRun,
    disassociateNatGatewayAddress_maxDrainDurationSeconds,
    disassociateNatGatewayAddress_natGatewayId,
    disassociateNatGatewayAddress_associationIds,
    disassociateNatGatewayAddressResponse_natGatewayAddresses,
    disassociateNatGatewayAddressResponse_natGatewayId,
    disassociateNatGatewayAddressResponse_httpStatus,

    -- ** DisassociateRouteTable
    disassociateRouteTable_dryRun,
    disassociateRouteTable_associationId,

    -- ** DisassociateSubnetCidrBlock
    disassociateSubnetCidrBlock_associationId,
    disassociateSubnetCidrBlockResponse_ipv6CidrBlockAssociation,
    disassociateSubnetCidrBlockResponse_subnetId,
    disassociateSubnetCidrBlockResponse_httpStatus,

    -- ** DisassociateTransitGatewayMulticastDomain
    disassociateTransitGatewayMulticastDomain_dryRun,
    disassociateTransitGatewayMulticastDomain_transitGatewayMulticastDomainId,
    disassociateTransitGatewayMulticastDomain_transitGatewayAttachmentId,
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

    -- ** EnableAwsNetworkPerformanceMetricSubscription
    enableAwsNetworkPerformanceMetricSubscription_destination,
    enableAwsNetworkPerformanceMetricSubscription_dryRun,
    enableAwsNetworkPerformanceMetricSubscription_metric,
    enableAwsNetworkPerformanceMetricSubscription_source,
    enableAwsNetworkPerformanceMetricSubscription_statistic,
    enableAwsNetworkPerformanceMetricSubscriptionResponse_output,
    enableAwsNetworkPerformanceMetricSubscriptionResponse_httpStatus,

    -- ** EnableEbsEncryptionByDefault
    enableEbsEncryptionByDefault_dryRun,
    enableEbsEncryptionByDefaultResponse_ebsEncryptionByDefault,
    enableEbsEncryptionByDefaultResponse_httpStatus,

    -- ** EnableFastLaunch
    enableFastLaunch_dryRun,
    enableFastLaunch_launchTemplate,
    enableFastLaunch_maxParallelLaunches,
    enableFastLaunch_resourceType,
    enableFastLaunch_snapshotConfiguration,
    enableFastLaunch_imageId,
    enableFastLaunchResponse_imageId,
    enableFastLaunchResponse_launchTemplate,
    enableFastLaunchResponse_maxParallelLaunches,
    enableFastLaunchResponse_ownerId,
    enableFastLaunchResponse_resourceType,
    enableFastLaunchResponse_snapshotConfiguration,
    enableFastLaunchResponse_state,
    enableFastLaunchResponse_stateTransitionReason,
    enableFastLaunchResponse_stateTransitionTime,
    enableFastLaunchResponse_httpStatus,

    -- ** EnableFastSnapshotRestores
    enableFastSnapshotRestores_dryRun,
    enableFastSnapshotRestores_availabilityZones,
    enableFastSnapshotRestores_sourceSnapshotIds,
    enableFastSnapshotRestoresResponse_successful,
    enableFastSnapshotRestoresResponse_unsuccessful,
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

    -- ** EnableReachabilityAnalyzerOrganizationSharing
    enableReachabilityAnalyzerOrganizationSharing_dryRun,
    enableReachabilityAnalyzerOrganizationSharingResponse_returnValue,
    enableReachabilityAnalyzerOrganizationSharingResponse_httpStatus,

    -- ** EnableSerialConsoleAccess
    enableSerialConsoleAccess_dryRun,
    enableSerialConsoleAccessResponse_serialConsoleAccessEnabled,
    enableSerialConsoleAccessResponse_httpStatus,

    -- ** EnableTransitGatewayRouteTablePropagation
    enableTransitGatewayRouteTablePropagation_dryRun,
    enableTransitGatewayRouteTablePropagation_transitGatewayAttachmentId,
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
    exportImage_clientToken,
    exportImage_description,
    exportImage_dryRun,
    exportImage_roleName,
    exportImage_tagSpecifications,
    exportImage_diskImageFormat,
    exportImage_imageId,
    exportImage_s3ExportLocation,
    exportImageResponse_description,
    exportImageResponse_diskImageFormat,
    exportImageResponse_exportImageTaskId,
    exportImageResponse_imageId,
    exportImageResponse_progress,
    exportImageResponse_roleName,
    exportImageResponse_s3ExportLocation,
    exportImageResponse_status,
    exportImageResponse_statusMessage,
    exportImageResponse_tags,
    exportImageResponse_httpStatus,

    -- ** ExportTransitGatewayRoutes
    exportTransitGatewayRoutes_dryRun,
    exportTransitGatewayRoutes_filters,
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
    getAssociatedIpv6PoolCidrs_dryRun,
    getAssociatedIpv6PoolCidrs_maxResults,
    getAssociatedIpv6PoolCidrs_nextToken,
    getAssociatedIpv6PoolCidrs_poolId,
    getAssociatedIpv6PoolCidrsResponse_ipv6CidrAssociations,
    getAssociatedIpv6PoolCidrsResponse_nextToken,
    getAssociatedIpv6PoolCidrsResponse_httpStatus,

    -- ** GetAwsNetworkPerformanceData
    getAwsNetworkPerformanceData_dataQueries,
    getAwsNetworkPerformanceData_dryRun,
    getAwsNetworkPerformanceData_endTime,
    getAwsNetworkPerformanceData_maxResults,
    getAwsNetworkPerformanceData_nextToken,
    getAwsNetworkPerformanceData_startTime,
    getAwsNetworkPerformanceDataResponse_dataResponses,
    getAwsNetworkPerformanceDataResponse_nextToken,
    getAwsNetworkPerformanceDataResponse_httpStatus,

    -- ** GetCapacityReservationUsage
    getCapacityReservationUsage_dryRun,
    getCapacityReservationUsage_maxResults,
    getCapacityReservationUsage_nextToken,
    getCapacityReservationUsage_capacityReservationId,
    getCapacityReservationUsageResponse_availableInstanceCount,
    getCapacityReservationUsageResponse_capacityReservationId,
    getCapacityReservationUsageResponse_instanceType,
    getCapacityReservationUsageResponse_instanceUsages,
    getCapacityReservationUsageResponse_nextToken,
    getCapacityReservationUsageResponse_state,
    getCapacityReservationUsageResponse_totalInstanceCount,
    getCapacityReservationUsageResponse_httpStatus,

    -- ** GetCoipPoolUsage
    getCoipPoolUsage_dryRun,
    getCoipPoolUsage_filters,
    getCoipPoolUsage_maxResults,
    getCoipPoolUsage_nextToken,
    getCoipPoolUsage_poolId,
    getCoipPoolUsageResponse_coipAddressUsages,
    getCoipPoolUsageResponse_coipPoolId,
    getCoipPoolUsageResponse_localGatewayRouteTableId,
    getCoipPoolUsageResponse_httpStatus,

    -- ** GetConsoleOutput
    getConsoleOutput_dryRun,
    getConsoleOutput_latest,
    getConsoleOutput_instanceId,
    getConsoleOutputResponse_instanceId,
    getConsoleOutputResponse_output,
    getConsoleOutputResponse_timestamp,
    getConsoleOutputResponse_httpStatus,

    -- ** GetConsoleScreenshot
    getConsoleScreenshot_dryRun,
    getConsoleScreenshot_wakeUp,
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
    getGroupsForCapacityReservation_dryRun,
    getGroupsForCapacityReservation_maxResults,
    getGroupsForCapacityReservation_nextToken,
    getGroupsForCapacityReservation_capacityReservationId,
    getGroupsForCapacityReservationResponse_capacityReservationGroups,
    getGroupsForCapacityReservationResponse_nextToken,
    getGroupsForCapacityReservationResponse_httpStatus,

    -- ** GetHostReservationPurchasePreview
    getHostReservationPurchasePreview_hostIdSet,
    getHostReservationPurchasePreview_offeringId,
    getHostReservationPurchasePreviewResponse_currencyCode,
    getHostReservationPurchasePreviewResponse_purchase,
    getHostReservationPurchasePreviewResponse_totalHourlyPrice,
    getHostReservationPurchasePreviewResponse_totalUpfrontPrice,
    getHostReservationPurchasePreviewResponse_httpStatus,

    -- ** GetInstanceTypesFromInstanceRequirements
    getInstanceTypesFromInstanceRequirements_dryRun,
    getInstanceTypesFromInstanceRequirements_maxResults,
    getInstanceTypesFromInstanceRequirements_nextToken,
    getInstanceTypesFromInstanceRequirements_architectureTypes,
    getInstanceTypesFromInstanceRequirements_virtualizationTypes,
    getInstanceTypesFromInstanceRequirements_instanceRequirements,
    getInstanceTypesFromInstanceRequirementsResponse_instanceTypes,
    getInstanceTypesFromInstanceRequirementsResponse_nextToken,
    getInstanceTypesFromInstanceRequirementsResponse_httpStatus,

    -- ** GetInstanceUefiData
    getInstanceUefiData_dryRun,
    getInstanceUefiData_instanceId,
    getInstanceUefiDataResponse_instanceId,
    getInstanceUefiDataResponse_uefiData,
    getInstanceUefiDataResponse_httpStatus,

    -- ** GetIpamAddressHistory
    getIpamAddressHistory_dryRun,
    getIpamAddressHistory_endTime,
    getIpamAddressHistory_maxResults,
    getIpamAddressHistory_nextToken,
    getIpamAddressHistory_startTime,
    getIpamAddressHistory_vpcId,
    getIpamAddressHistory_cidr,
    getIpamAddressHistory_ipamScopeId,
    getIpamAddressHistoryResponse_historyRecords,
    getIpamAddressHistoryResponse_nextToken,
    getIpamAddressHistoryResponse_httpStatus,

    -- ** GetIpamDiscoveredAccounts
    getIpamDiscoveredAccounts_dryRun,
    getIpamDiscoveredAccounts_filters,
    getIpamDiscoveredAccounts_maxResults,
    getIpamDiscoveredAccounts_nextToken,
    getIpamDiscoveredAccounts_ipamResourceDiscoveryId,
    getIpamDiscoveredAccounts_discoveryRegion,
    getIpamDiscoveredAccountsResponse_ipamDiscoveredAccounts,
    getIpamDiscoveredAccountsResponse_nextToken,
    getIpamDiscoveredAccountsResponse_httpStatus,

    -- ** GetIpamDiscoveredResourceCidrs
    getIpamDiscoveredResourceCidrs_dryRun,
    getIpamDiscoveredResourceCidrs_filters,
    getIpamDiscoveredResourceCidrs_maxResults,
    getIpamDiscoveredResourceCidrs_nextToken,
    getIpamDiscoveredResourceCidrs_ipamResourceDiscoveryId,
    getIpamDiscoveredResourceCidrs_resourceRegion,
    getIpamDiscoveredResourceCidrsResponse_ipamDiscoveredResourceCidrs,
    getIpamDiscoveredResourceCidrsResponse_nextToken,
    getIpamDiscoveredResourceCidrsResponse_httpStatus,

    -- ** GetIpamPoolAllocations
    getIpamPoolAllocations_dryRun,
    getIpamPoolAllocations_filters,
    getIpamPoolAllocations_ipamPoolAllocationId,
    getIpamPoolAllocations_maxResults,
    getIpamPoolAllocations_nextToken,
    getIpamPoolAllocations_ipamPoolId,
    getIpamPoolAllocationsResponse_ipamPoolAllocations,
    getIpamPoolAllocationsResponse_nextToken,
    getIpamPoolAllocationsResponse_httpStatus,

    -- ** GetIpamPoolCidrs
    getIpamPoolCidrs_dryRun,
    getIpamPoolCidrs_filters,
    getIpamPoolCidrs_maxResults,
    getIpamPoolCidrs_nextToken,
    getIpamPoolCidrs_ipamPoolId,
    getIpamPoolCidrsResponse_ipamPoolCidrs,
    getIpamPoolCidrsResponse_nextToken,
    getIpamPoolCidrsResponse_httpStatus,

    -- ** GetIpamResourceCidrs
    getIpamResourceCidrs_dryRun,
    getIpamResourceCidrs_filters,
    getIpamResourceCidrs_ipamPoolId,
    getIpamResourceCidrs_maxResults,
    getIpamResourceCidrs_nextToken,
    getIpamResourceCidrs_resourceId,
    getIpamResourceCidrs_resourceOwner,
    getIpamResourceCidrs_resourceTag,
    getIpamResourceCidrs_resourceType,
    getIpamResourceCidrs_ipamScopeId,
    getIpamResourceCidrsResponse_ipamResourceCidrs,
    getIpamResourceCidrsResponse_nextToken,
    getIpamResourceCidrsResponse_httpStatus,

    -- ** GetLaunchTemplateData
    getLaunchTemplateData_dryRun,
    getLaunchTemplateData_instanceId,
    getLaunchTemplateDataResponse_launchTemplateData,
    getLaunchTemplateDataResponse_httpStatus,

    -- ** GetManagedPrefixListAssociations
    getManagedPrefixListAssociations_dryRun,
    getManagedPrefixListAssociations_maxResults,
    getManagedPrefixListAssociations_nextToken,
    getManagedPrefixListAssociations_prefixListId,
    getManagedPrefixListAssociationsResponse_nextToken,
    getManagedPrefixListAssociationsResponse_prefixListAssociations,
    getManagedPrefixListAssociationsResponse_httpStatus,

    -- ** GetManagedPrefixListEntries
    getManagedPrefixListEntries_dryRun,
    getManagedPrefixListEntries_maxResults,
    getManagedPrefixListEntries_nextToken,
    getManagedPrefixListEntries_targetVersion,
    getManagedPrefixListEntries_prefixListId,
    getManagedPrefixListEntriesResponse_entries,
    getManagedPrefixListEntriesResponse_nextToken,
    getManagedPrefixListEntriesResponse_httpStatus,

    -- ** GetNetworkInsightsAccessScopeAnalysisFindings
    getNetworkInsightsAccessScopeAnalysisFindings_dryRun,
    getNetworkInsightsAccessScopeAnalysisFindings_maxResults,
    getNetworkInsightsAccessScopeAnalysisFindings_nextToken,
    getNetworkInsightsAccessScopeAnalysisFindings_networkInsightsAccessScopeAnalysisId,
    getNetworkInsightsAccessScopeAnalysisFindingsResponse_analysisFindings,
    getNetworkInsightsAccessScopeAnalysisFindingsResponse_analysisStatus,
    getNetworkInsightsAccessScopeAnalysisFindingsResponse_networkInsightsAccessScopeAnalysisId,
    getNetworkInsightsAccessScopeAnalysisFindingsResponse_nextToken,
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
    getReservedInstancesExchangeQuoteResponse_currencyCode,
    getReservedInstancesExchangeQuoteResponse_isValidExchange,
    getReservedInstancesExchangeQuoteResponse_outputReservedInstancesWillExpireAt,
    getReservedInstancesExchangeQuoteResponse_paymentDue,
    getReservedInstancesExchangeQuoteResponse_reservedInstanceValueRollup,
    getReservedInstancesExchangeQuoteResponse_reservedInstanceValueSet,
    getReservedInstancesExchangeQuoteResponse_targetConfigurationValueRollup,
    getReservedInstancesExchangeQuoteResponse_targetConfigurationValueSet,
    getReservedInstancesExchangeQuoteResponse_validationFailureReason,
    getReservedInstancesExchangeQuoteResponse_httpStatus,

    -- ** GetSerialConsoleAccessStatus
    getSerialConsoleAccessStatus_dryRun,
    getSerialConsoleAccessStatusResponse_serialConsoleAccessEnabled,
    getSerialConsoleAccessStatusResponse_httpStatus,

    -- ** GetSpotPlacementScores
    getSpotPlacementScores_dryRun,
    getSpotPlacementScores_instanceRequirementsWithMetadata,
    getSpotPlacementScores_instanceTypes,
    getSpotPlacementScores_maxResults,
    getSpotPlacementScores_nextToken,
    getSpotPlacementScores_regionNames,
    getSpotPlacementScores_singleAvailabilityZone,
    getSpotPlacementScores_targetCapacityUnitType,
    getSpotPlacementScores_targetCapacity,
    getSpotPlacementScoresResponse_nextToken,
    getSpotPlacementScoresResponse_spotPlacementScores,
    getSpotPlacementScoresResponse_httpStatus,

    -- ** GetSubnetCidrReservations
    getSubnetCidrReservations_dryRun,
    getSubnetCidrReservations_filters,
    getSubnetCidrReservations_maxResults,
    getSubnetCidrReservations_nextToken,
    getSubnetCidrReservations_subnetId,
    getSubnetCidrReservationsResponse_nextToken,
    getSubnetCidrReservationsResponse_subnetIpv4CidrReservations,
    getSubnetCidrReservationsResponse_subnetIpv6CidrReservations,
    getSubnetCidrReservationsResponse_httpStatus,

    -- ** GetTransitGatewayAttachmentPropagations
    getTransitGatewayAttachmentPropagations_dryRun,
    getTransitGatewayAttachmentPropagations_filters,
    getTransitGatewayAttachmentPropagations_maxResults,
    getTransitGatewayAttachmentPropagations_nextToken,
    getTransitGatewayAttachmentPropagations_transitGatewayAttachmentId,
    getTransitGatewayAttachmentPropagationsResponse_nextToken,
    getTransitGatewayAttachmentPropagationsResponse_transitGatewayAttachmentPropagations,
    getTransitGatewayAttachmentPropagationsResponse_httpStatus,

    -- ** GetTransitGatewayMulticastDomainAssociations
    getTransitGatewayMulticastDomainAssociations_dryRun,
    getTransitGatewayMulticastDomainAssociations_filters,
    getTransitGatewayMulticastDomainAssociations_maxResults,
    getTransitGatewayMulticastDomainAssociations_nextToken,
    getTransitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId,
    getTransitGatewayMulticastDomainAssociationsResponse_multicastDomainAssociations,
    getTransitGatewayMulticastDomainAssociationsResponse_nextToken,
    getTransitGatewayMulticastDomainAssociationsResponse_httpStatus,

    -- ** GetTransitGatewayPolicyTableAssociations
    getTransitGatewayPolicyTableAssociations_dryRun,
    getTransitGatewayPolicyTableAssociations_filters,
    getTransitGatewayPolicyTableAssociations_maxResults,
    getTransitGatewayPolicyTableAssociations_nextToken,
    getTransitGatewayPolicyTableAssociations_transitGatewayPolicyTableId,
    getTransitGatewayPolicyTableAssociationsResponse_associations,
    getTransitGatewayPolicyTableAssociationsResponse_nextToken,
    getTransitGatewayPolicyTableAssociationsResponse_httpStatus,

    -- ** GetTransitGatewayPolicyTableEntries
    getTransitGatewayPolicyTableEntries_dryRun,
    getTransitGatewayPolicyTableEntries_filters,
    getTransitGatewayPolicyTableEntries_maxResults,
    getTransitGatewayPolicyTableEntries_nextToken,
    getTransitGatewayPolicyTableEntries_transitGatewayPolicyTableId,
    getTransitGatewayPolicyTableEntriesResponse_transitGatewayPolicyTableEntries,
    getTransitGatewayPolicyTableEntriesResponse_httpStatus,

    -- ** GetTransitGatewayPrefixListReferences
    getTransitGatewayPrefixListReferences_dryRun,
    getTransitGatewayPrefixListReferences_filters,
    getTransitGatewayPrefixListReferences_maxResults,
    getTransitGatewayPrefixListReferences_nextToken,
    getTransitGatewayPrefixListReferences_transitGatewayRouteTableId,
    getTransitGatewayPrefixListReferencesResponse_nextToken,
    getTransitGatewayPrefixListReferencesResponse_transitGatewayPrefixListReferences,
    getTransitGatewayPrefixListReferencesResponse_httpStatus,

    -- ** GetTransitGatewayRouteTableAssociations
    getTransitGatewayRouteTableAssociations_dryRun,
    getTransitGatewayRouteTableAssociations_filters,
    getTransitGatewayRouteTableAssociations_maxResults,
    getTransitGatewayRouteTableAssociations_nextToken,
    getTransitGatewayRouteTableAssociations_transitGatewayRouteTableId,
    getTransitGatewayRouteTableAssociationsResponse_associations,
    getTransitGatewayRouteTableAssociationsResponse_nextToken,
    getTransitGatewayRouteTableAssociationsResponse_httpStatus,

    -- ** GetTransitGatewayRouteTablePropagations
    getTransitGatewayRouteTablePropagations_dryRun,
    getTransitGatewayRouteTablePropagations_filters,
    getTransitGatewayRouteTablePropagations_maxResults,
    getTransitGatewayRouteTablePropagations_nextToken,
    getTransitGatewayRouteTablePropagations_transitGatewayRouteTableId,
    getTransitGatewayRouteTablePropagationsResponse_nextToken,
    getTransitGatewayRouteTablePropagationsResponse_transitGatewayRouteTablePropagations,
    getTransitGatewayRouteTablePropagationsResponse_httpStatus,

    -- ** GetVerifiedAccessEndpointPolicy
    getVerifiedAccessEndpointPolicy_dryRun,
    getVerifiedAccessEndpointPolicy_verifiedAccessEndpointId,
    getVerifiedAccessEndpointPolicyResponse_policyDocument,
    getVerifiedAccessEndpointPolicyResponse_policyEnabled,
    getVerifiedAccessEndpointPolicyResponse_httpStatus,

    -- ** GetVerifiedAccessGroupPolicy
    getVerifiedAccessGroupPolicy_dryRun,
    getVerifiedAccessGroupPolicy_verifiedAccessGroupId,
    getVerifiedAccessGroupPolicyResponse_policyDocument,
    getVerifiedAccessGroupPolicyResponse_policyEnabled,
    getVerifiedAccessGroupPolicyResponse_httpStatus,

    -- ** GetVpnConnectionDeviceSampleConfiguration
    getVpnConnectionDeviceSampleConfiguration_dryRun,
    getVpnConnectionDeviceSampleConfiguration_internetKeyExchangeVersion,
    getVpnConnectionDeviceSampleConfiguration_vpnConnectionId,
    getVpnConnectionDeviceSampleConfiguration_vpnConnectionDeviceTypeId,
    getVpnConnectionDeviceSampleConfigurationResponse_vpnConnectionDeviceSampleConfiguration,
    getVpnConnectionDeviceSampleConfigurationResponse_httpStatus,

    -- ** GetVpnConnectionDeviceTypes
    getVpnConnectionDeviceTypes_dryRun,
    getVpnConnectionDeviceTypes_maxResults,
    getVpnConnectionDeviceTypes_nextToken,
    getVpnConnectionDeviceTypesResponse_nextToken,
    getVpnConnectionDeviceTypesResponse_vpnConnectionDeviceTypes,
    getVpnConnectionDeviceTypesResponse_httpStatus,

    -- ** GetVpnTunnelReplacementStatus
    getVpnTunnelReplacementStatus_dryRun,
    getVpnTunnelReplacementStatus_vpnConnectionId,
    getVpnTunnelReplacementStatus_vpnTunnelOutsideIpAddress,
    getVpnTunnelReplacementStatusResponse_customerGatewayId,
    getVpnTunnelReplacementStatusResponse_maintenanceDetails,
    getVpnTunnelReplacementStatusResponse_transitGatewayId,
    getVpnTunnelReplacementStatusResponse_vpnConnectionId,
    getVpnTunnelReplacementStatusResponse_vpnGatewayId,
    getVpnTunnelReplacementStatusResponse_vpnTunnelOutsideIpAddress,
    getVpnTunnelReplacementStatusResponse_httpStatus,

    -- ** ImportClientVpnClientCertificateRevocationList
    importClientVpnClientCertificateRevocationList_dryRun,
    importClientVpnClientCertificateRevocationList_clientVpnEndpointId,
    importClientVpnClientCertificateRevocationList_certificateRevocationList,
    importClientVpnClientCertificateRevocationListResponse_return,
    importClientVpnClientCertificateRevocationListResponse_httpStatus,

    -- ** ImportImage
    importImage_architecture,
    importImage_bootMode,
    importImage_clientData,
    importImage_clientToken,
    importImage_description,
    importImage_diskContainers,
    importImage_dryRun,
    importImage_encrypted,
    importImage_hypervisor,
    importImage_kmsKeyId,
    importImage_licenseSpecifications,
    importImage_licenseType,
    importImage_platform,
    importImage_roleName,
    importImage_tagSpecifications,
    importImage_usageOperation,
    importImageResponse_architecture,
    importImageResponse_description,
    importImageResponse_encrypted,
    importImageResponse_hypervisor,
    importImageResponse_imageId,
    importImageResponse_importTaskId,
    importImageResponse_kmsKeyId,
    importImageResponse_licenseSpecifications,
    importImageResponse_licenseType,
    importImageResponse_platform,
    importImageResponse_progress,
    importImageResponse_snapshotDetails,
    importImageResponse_status,
    importImageResponse_statusMessage,
    importImageResponse_tags,
    importImageResponse_usageOperation,
    importImageResponse_httpStatus,

    -- ** ImportInstance
    importInstance_description,
    importInstance_diskImages,
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
    importKeyPairResponse_keyFingerprint,
    importKeyPairResponse_keyName,
    importKeyPairResponse_keyPairId,
    importKeyPairResponse_tags,
    importKeyPairResponse_httpStatus,

    -- ** ImportSnapshot
    importSnapshot_clientData,
    importSnapshot_clientToken,
    importSnapshot_description,
    importSnapshot_diskContainer,
    importSnapshot_dryRun,
    importSnapshot_encrypted,
    importSnapshot_kmsKeyId,
    importSnapshot_roleName,
    importSnapshot_tagSpecifications,
    importSnapshotResponse_description,
    importSnapshotResponse_importTaskId,
    importSnapshotResponse_snapshotTaskDetail,
    importSnapshotResponse_tags,
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
    listImagesInRecycleBin_dryRun,
    listImagesInRecycleBin_imageIds,
    listImagesInRecycleBin_maxResults,
    listImagesInRecycleBin_nextToken,
    listImagesInRecycleBinResponse_images,
    listImagesInRecycleBinResponse_nextToken,
    listImagesInRecycleBinResponse_httpStatus,

    -- ** ListSnapshotsInRecycleBin
    listSnapshotsInRecycleBin_dryRun,
    listSnapshotsInRecycleBin_maxResults,
    listSnapshotsInRecycleBin_nextToken,
    listSnapshotsInRecycleBin_snapshotIds,
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
    modifyCapacityReservation_accept,
    modifyCapacityReservation_additionalInfo,
    modifyCapacityReservation_dryRun,
    modifyCapacityReservation_endDate,
    modifyCapacityReservation_endDateType,
    modifyCapacityReservation_instanceCount,
    modifyCapacityReservation_capacityReservationId,
    modifyCapacityReservationResponse_return,
    modifyCapacityReservationResponse_httpStatus,

    -- ** ModifyCapacityReservationFleet
    modifyCapacityReservationFleet_dryRun,
    modifyCapacityReservationFleet_endDate,
    modifyCapacityReservationFleet_removeEndDate,
    modifyCapacityReservationFleet_totalTargetCapacity,
    modifyCapacityReservationFleet_capacityReservationFleetId,
    modifyCapacityReservationFleetResponse_return,
    modifyCapacityReservationFleetResponse_httpStatus,

    -- ** ModifyClientVpnEndpoint
    modifyClientVpnEndpoint_clientConnectOptions,
    modifyClientVpnEndpoint_clientLoginBannerOptions,
    modifyClientVpnEndpoint_connectionLogOptions,
    modifyClientVpnEndpoint_description,
    modifyClientVpnEndpoint_dnsServers,
    modifyClientVpnEndpoint_dryRun,
    modifyClientVpnEndpoint_securityGroupIds,
    modifyClientVpnEndpoint_selfServicePortal,
    modifyClientVpnEndpoint_serverCertificateArn,
    modifyClientVpnEndpoint_sessionTimeoutHours,
    modifyClientVpnEndpoint_splitTunnel,
    modifyClientVpnEndpoint_vpcId,
    modifyClientVpnEndpoint_vpnPort,
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
    modifyFleet_context,
    modifyFleet_dryRun,
    modifyFleet_excessCapacityTerminationPolicy,
    modifyFleet_launchTemplateConfigs,
    modifyFleet_targetCapacitySpecification,
    modifyFleet_fleetId,
    modifyFleetResponse_return,
    modifyFleetResponse_httpStatus,

    -- ** ModifyFpgaImageAttribute
    modifyFpgaImageAttribute_attribute,
    modifyFpgaImageAttribute_description,
    modifyFpgaImageAttribute_dryRun,
    modifyFpgaImageAttribute_loadPermission,
    modifyFpgaImageAttribute_name,
    modifyFpgaImageAttribute_operationType,
    modifyFpgaImageAttribute_productCodes,
    modifyFpgaImageAttribute_userGroups,
    modifyFpgaImageAttribute_userIds,
    modifyFpgaImageAttribute_fpgaImageId,
    modifyFpgaImageAttributeResponse_fpgaImageAttribute,
    modifyFpgaImageAttributeResponse_httpStatus,

    -- ** ModifyHosts
    modifyHosts_autoPlacement,
    modifyHosts_hostMaintenance,
    modifyHosts_hostRecovery,
    modifyHosts_instanceFamily,
    modifyHosts_instanceType,
    modifyHosts_hostIds,
    modifyHostsResponse_successful,
    modifyHostsResponse_unsuccessful,
    modifyHostsResponse_httpStatus,

    -- ** ModifyIdFormat
    modifyIdFormat_resource,
    modifyIdFormat_useLongIds,

    -- ** ModifyIdentityIdFormat
    modifyIdentityIdFormat_principalArn,
    modifyIdentityIdFormat_resource,
    modifyIdentityIdFormat_useLongIds,

    -- ** ModifyImageAttribute
    modifyImageAttribute_attribute,
    modifyImageAttribute_description,
    modifyImageAttribute_dryRun,
    modifyImageAttribute_imdsSupport,
    modifyImageAttribute_launchPermission,
    modifyImageAttribute_operationType,
    modifyImageAttribute_organizationArns,
    modifyImageAttribute_organizationalUnitArns,
    modifyImageAttribute_productCodes,
    modifyImageAttribute_userGroups,
    modifyImageAttribute_userIds,
    modifyImageAttribute_value,
    modifyImageAttribute_imageId,

    -- ** ModifyInstanceAttribute
    modifyInstanceAttribute_attribute,
    modifyInstanceAttribute_blockDeviceMappings,
    modifyInstanceAttribute_disableApiStop,
    modifyInstanceAttribute_disableApiTermination,
    modifyInstanceAttribute_dryRun,
    modifyInstanceAttribute_ebsOptimized,
    modifyInstanceAttribute_enaSupport,
    modifyInstanceAttribute_groups,
    modifyInstanceAttribute_instanceInitiatedShutdownBehavior,
    modifyInstanceAttribute_instanceType,
    modifyInstanceAttribute_kernel,
    modifyInstanceAttribute_ramdisk,
    modifyInstanceAttribute_sourceDestCheck,
    modifyInstanceAttribute_sriovNetSupport,
    modifyInstanceAttribute_userData,
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
    modifyInstanceEventWindow_cronExpression,
    modifyInstanceEventWindow_dryRun,
    modifyInstanceEventWindow_name,
    modifyInstanceEventWindow_timeRanges,
    modifyInstanceEventWindow_instanceEventWindowId,
    modifyInstanceEventWindowResponse_instanceEventWindow,
    modifyInstanceEventWindowResponse_httpStatus,

    -- ** ModifyInstanceMaintenanceOptions
    modifyInstanceMaintenanceOptions_autoRecovery,
    modifyInstanceMaintenanceOptions_dryRun,
    modifyInstanceMaintenanceOptions_instanceId,
    modifyInstanceMaintenanceOptionsResponse_autoRecovery,
    modifyInstanceMaintenanceOptionsResponse_instanceId,
    modifyInstanceMaintenanceOptionsResponse_httpStatus,

    -- ** ModifyInstanceMetadataOptions
    modifyInstanceMetadataOptions_dryRun,
    modifyInstanceMetadataOptions_httpEndpoint,
    modifyInstanceMetadataOptions_httpProtocolIpv6,
    modifyInstanceMetadataOptions_httpPutResponseHopLimit,
    modifyInstanceMetadataOptions_httpTokens,
    modifyInstanceMetadataOptions_instanceMetadataTags,
    modifyInstanceMetadataOptions_instanceId,
    modifyInstanceMetadataOptionsResponse_instanceId,
    modifyInstanceMetadataOptionsResponse_instanceMetadataOptions,
    modifyInstanceMetadataOptionsResponse_httpStatus,

    -- ** ModifyInstancePlacement
    modifyInstancePlacement_affinity,
    modifyInstancePlacement_groupId,
    modifyInstancePlacement_groupName,
    modifyInstancePlacement_hostId,
    modifyInstancePlacement_hostResourceGroupArn,
    modifyInstancePlacement_partitionNumber,
    modifyInstancePlacement_tenancy,
    modifyInstancePlacement_instanceId,
    modifyInstancePlacementResponse_return,
    modifyInstancePlacementResponse_httpStatus,

    -- ** ModifyIpam
    modifyIpam_addOperatingRegions,
    modifyIpam_description,
    modifyIpam_dryRun,
    modifyIpam_removeOperatingRegions,
    modifyIpam_ipamId,
    modifyIpamResponse_ipam,
    modifyIpamResponse_httpStatus,

    -- ** ModifyIpamPool
    modifyIpamPool_addAllocationResourceTags,
    modifyIpamPool_allocationDefaultNetmaskLength,
    modifyIpamPool_allocationMaxNetmaskLength,
    modifyIpamPool_allocationMinNetmaskLength,
    modifyIpamPool_autoImport,
    modifyIpamPool_clearAllocationDefaultNetmaskLength,
    modifyIpamPool_description,
    modifyIpamPool_dryRun,
    modifyIpamPool_removeAllocationResourceTags,
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

    -- ** ModifyIpamResourceDiscovery
    modifyIpamResourceDiscovery_addOperatingRegions,
    modifyIpamResourceDiscovery_description,
    modifyIpamResourceDiscovery_dryRun,
    modifyIpamResourceDiscovery_removeOperatingRegions,
    modifyIpamResourceDiscovery_ipamResourceDiscoveryId,
    modifyIpamResourceDiscoveryResponse_ipamResourceDiscovery,
    modifyIpamResourceDiscoveryResponse_httpStatus,

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
    modifyLocalGatewayRoute_destinationCidrBlock,
    modifyLocalGatewayRoute_destinationPrefixListId,
    modifyLocalGatewayRoute_dryRun,
    modifyLocalGatewayRoute_localGatewayVirtualInterfaceGroupId,
    modifyLocalGatewayRoute_networkInterfaceId,
    modifyLocalGatewayRoute_localGatewayRouteTableId,
    modifyLocalGatewayRouteResponse_route,
    modifyLocalGatewayRouteResponse_httpStatus,

    -- ** ModifyManagedPrefixList
    modifyManagedPrefixList_addEntries,
    modifyManagedPrefixList_currentVersion,
    modifyManagedPrefixList_dryRun,
    modifyManagedPrefixList_maxEntries,
    modifyManagedPrefixList_prefixListName,
    modifyManagedPrefixList_removeEntries,
    modifyManagedPrefixList_prefixListId,
    modifyManagedPrefixListResponse_prefixList,
    modifyManagedPrefixListResponse_httpStatus,

    -- ** ModifyNetworkInterfaceAttribute
    modifyNetworkInterfaceAttribute_attachment,
    modifyNetworkInterfaceAttribute_description,
    modifyNetworkInterfaceAttribute_dryRun,
    modifyNetworkInterfaceAttribute_enaSrdSpecification,
    modifyNetworkInterfaceAttribute_groups,
    modifyNetworkInterfaceAttribute_sourceDestCheck,
    modifyNetworkInterfaceAttribute_networkInterfaceId,

    -- ** ModifyPrivateDnsNameOptions
    modifyPrivateDnsNameOptions_dryRun,
    modifyPrivateDnsNameOptions_enableResourceNameDnsAAAARecord,
    modifyPrivateDnsNameOptions_enableResourceNameDnsARecord,
    modifyPrivateDnsNameOptions_privateDnsHostnameType,
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
    modifySnapshotAttribute_attribute,
    modifySnapshotAttribute_createVolumePermission,
    modifySnapshotAttribute_dryRun,
    modifySnapshotAttribute_groupNames,
    modifySnapshotAttribute_operationType,
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
    modifySpotFleetRequest_context,
    modifySpotFleetRequest_excessCapacityTerminationPolicy,
    modifySpotFleetRequest_launchTemplateConfigs,
    modifySpotFleetRequest_onDemandTargetCapacity,
    modifySpotFleetRequest_targetCapacity,
    modifySpotFleetRequest_spotFleetRequestId,
    modifySpotFleetRequestResponse_return,
    modifySpotFleetRequestResponse_httpStatus,

    -- ** ModifySubnetAttribute
    modifySubnetAttribute_assignIpv6AddressOnCreation,
    modifySubnetAttribute_customerOwnedIpv4Pool,
    modifySubnetAttribute_disableLniAtDeviceIndex,
    modifySubnetAttribute_enableDns64,
    modifySubnetAttribute_enableLniAtDeviceIndex,
    modifySubnetAttribute_enableResourceNameDnsAAAARecordOnLaunch,
    modifySubnetAttribute_enableResourceNameDnsARecordOnLaunch,
    modifySubnetAttribute_mapCustomerOwnedIpOnLaunch,
    modifySubnetAttribute_mapPublicIpOnLaunch,
    modifySubnetAttribute_privateDnsHostnameTypeOnLaunch,
    modifySubnetAttribute_subnetId,

    -- ** ModifyTrafficMirrorFilterNetworkServices
    modifyTrafficMirrorFilterNetworkServices_addNetworkServices,
    modifyTrafficMirrorFilterNetworkServices_dryRun,
    modifyTrafficMirrorFilterNetworkServices_removeNetworkServices,
    modifyTrafficMirrorFilterNetworkServices_trafficMirrorFilterId,
    modifyTrafficMirrorFilterNetworkServicesResponse_trafficMirrorFilter,
    modifyTrafficMirrorFilterNetworkServicesResponse_httpStatus,

    -- ** ModifyTrafficMirrorFilterRule
    modifyTrafficMirrorFilterRule_description,
    modifyTrafficMirrorFilterRule_destinationCidrBlock,
    modifyTrafficMirrorFilterRule_destinationPortRange,
    modifyTrafficMirrorFilterRule_dryRun,
    modifyTrafficMirrorFilterRule_protocol,
    modifyTrafficMirrorFilterRule_removeFields,
    modifyTrafficMirrorFilterRule_ruleAction,
    modifyTrafficMirrorFilterRule_ruleNumber,
    modifyTrafficMirrorFilterRule_sourceCidrBlock,
    modifyTrafficMirrorFilterRule_sourcePortRange,
    modifyTrafficMirrorFilterRule_trafficDirection,
    modifyTrafficMirrorFilterRule_trafficMirrorFilterRuleId,
    modifyTrafficMirrorFilterRuleResponse_trafficMirrorFilterRule,
    modifyTrafficMirrorFilterRuleResponse_httpStatus,

    -- ** ModifyTrafficMirrorSession
    modifyTrafficMirrorSession_description,
    modifyTrafficMirrorSession_dryRun,
    modifyTrafficMirrorSession_packetLength,
    modifyTrafficMirrorSession_removeFields,
    modifyTrafficMirrorSession_sessionNumber,
    modifyTrafficMirrorSession_trafficMirrorFilterId,
    modifyTrafficMirrorSession_trafficMirrorTargetId,
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
    modifyTransitGatewayPrefixListReference_blackhole,
    modifyTransitGatewayPrefixListReference_dryRun,
    modifyTransitGatewayPrefixListReference_transitGatewayAttachmentId,
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

    -- ** ModifyVerifiedAccessEndpoint
    modifyVerifiedAccessEndpoint_clientToken,
    modifyVerifiedAccessEndpoint_description,
    modifyVerifiedAccessEndpoint_dryRun,
    modifyVerifiedAccessEndpoint_loadBalancerOptions,
    modifyVerifiedAccessEndpoint_networkInterfaceOptions,
    modifyVerifiedAccessEndpoint_verifiedAccessGroupId,
    modifyVerifiedAccessEndpoint_verifiedAccessEndpointId,
    modifyVerifiedAccessEndpointResponse_verifiedAccessEndpoint,
    modifyVerifiedAccessEndpointResponse_httpStatus,

    -- ** ModifyVerifiedAccessEndpointPolicy
    modifyVerifiedAccessEndpointPolicy_clientToken,
    modifyVerifiedAccessEndpointPolicy_dryRun,
    modifyVerifiedAccessEndpointPolicy_policyDocument,
    modifyVerifiedAccessEndpointPolicy_verifiedAccessEndpointId,
    modifyVerifiedAccessEndpointPolicy_policyEnabled,
    modifyVerifiedAccessEndpointPolicyResponse_policyDocument,
    modifyVerifiedAccessEndpointPolicyResponse_policyEnabled,
    modifyVerifiedAccessEndpointPolicyResponse_httpStatus,

    -- ** ModifyVerifiedAccessGroup
    modifyVerifiedAccessGroup_clientToken,
    modifyVerifiedAccessGroup_description,
    modifyVerifiedAccessGroup_dryRun,
    modifyVerifiedAccessGroup_verifiedAccessInstanceId,
    modifyVerifiedAccessGroup_verifiedAccessGroupId,
    modifyVerifiedAccessGroupResponse_verifiedAccessGroup,
    modifyVerifiedAccessGroupResponse_httpStatus,

    -- ** ModifyVerifiedAccessGroupPolicy
    modifyVerifiedAccessGroupPolicy_clientToken,
    modifyVerifiedAccessGroupPolicy_dryRun,
    modifyVerifiedAccessGroupPolicy_policyDocument,
    modifyVerifiedAccessGroupPolicy_verifiedAccessGroupId,
    modifyVerifiedAccessGroupPolicy_policyEnabled,
    modifyVerifiedAccessGroupPolicyResponse_policyDocument,
    modifyVerifiedAccessGroupPolicyResponse_policyEnabled,
    modifyVerifiedAccessGroupPolicyResponse_httpStatus,

    -- ** ModifyVerifiedAccessInstance
    modifyVerifiedAccessInstance_clientToken,
    modifyVerifiedAccessInstance_description,
    modifyVerifiedAccessInstance_dryRun,
    modifyVerifiedAccessInstance_verifiedAccessInstanceId,
    modifyVerifiedAccessInstanceResponse_verifiedAccessInstance,
    modifyVerifiedAccessInstanceResponse_httpStatus,

    -- ** ModifyVerifiedAccessInstanceLoggingConfiguration
    modifyVerifiedAccessInstanceLoggingConfiguration_clientToken,
    modifyVerifiedAccessInstanceLoggingConfiguration_dryRun,
    modifyVerifiedAccessInstanceLoggingConfiguration_verifiedAccessInstanceId,
    modifyVerifiedAccessInstanceLoggingConfiguration_accessLogs,
    modifyVerifiedAccessInstanceLoggingConfigurationResponse_loggingConfiguration,
    modifyVerifiedAccessInstanceLoggingConfigurationResponse_httpStatus,

    -- ** ModifyVerifiedAccessTrustProvider
    modifyVerifiedAccessTrustProvider_clientToken,
    modifyVerifiedAccessTrustProvider_description,
    modifyVerifiedAccessTrustProvider_dryRun,
    modifyVerifiedAccessTrustProvider_oidcOptions,
    modifyVerifiedAccessTrustProvider_verifiedAccessTrustProviderId,
    modifyVerifiedAccessTrustProviderResponse_verifiedAccessTrustProvider,
    modifyVerifiedAccessTrustProviderResponse_httpStatus,

    -- ** ModifyVolume
    modifyVolume_dryRun,
    modifyVolume_iops,
    modifyVolume_multiAttachEnabled,
    modifyVolume_size,
    modifyVolume_throughput,
    modifyVolume_volumeType,
    modifyVolume_volumeId,
    modifyVolumeResponse_volumeModification,
    modifyVolumeResponse_httpStatus,

    -- ** ModifyVolumeAttribute
    modifyVolumeAttribute_autoEnableIO,
    modifyVolumeAttribute_dryRun,
    modifyVolumeAttribute_volumeId,

    -- ** ModifyVpcAttribute
    modifyVpcAttribute_enableDnsHostnames,
    modifyVpcAttribute_enableDnsSupport,
    modifyVpcAttribute_enableNetworkAddressUsageMetrics,
    modifyVpcAttribute_vpcId,

    -- ** ModifyVpcEndpoint
    modifyVpcEndpoint_addRouteTableIds,
    modifyVpcEndpoint_addSecurityGroupIds,
    modifyVpcEndpoint_addSubnetIds,
    modifyVpcEndpoint_dnsOptions,
    modifyVpcEndpoint_dryRun,
    modifyVpcEndpoint_ipAddressType,
    modifyVpcEndpoint_policyDocument,
    modifyVpcEndpoint_privateDnsEnabled,
    modifyVpcEndpoint_removeRouteTableIds,
    modifyVpcEndpoint_removeSecurityGroupIds,
    modifyVpcEndpoint_removeSubnetIds,
    modifyVpcEndpoint_resetPolicy,
    modifyVpcEndpoint_vpcEndpointId,
    modifyVpcEndpointResponse_return,
    modifyVpcEndpointResponse_httpStatus,

    -- ** ModifyVpcEndpointConnectionNotification
    modifyVpcEndpointConnectionNotification_connectionEvents,
    modifyVpcEndpointConnectionNotification_connectionNotificationArn,
    modifyVpcEndpointConnectionNotification_dryRun,
    modifyVpcEndpointConnectionNotification_connectionNotificationId,
    modifyVpcEndpointConnectionNotificationResponse_returnValue,
    modifyVpcEndpointConnectionNotificationResponse_httpStatus,

    -- ** ModifyVpcEndpointServiceConfiguration
    modifyVpcEndpointServiceConfiguration_acceptanceRequired,
    modifyVpcEndpointServiceConfiguration_addGatewayLoadBalancerArns,
    modifyVpcEndpointServiceConfiguration_addNetworkLoadBalancerArns,
    modifyVpcEndpointServiceConfiguration_addSupportedIpAddressTypes,
    modifyVpcEndpointServiceConfiguration_dryRun,
    modifyVpcEndpointServiceConfiguration_privateDnsName,
    modifyVpcEndpointServiceConfiguration_removeGatewayLoadBalancerArns,
    modifyVpcEndpointServiceConfiguration_removeNetworkLoadBalancerArns,
    modifyVpcEndpointServiceConfiguration_removePrivateDnsName,
    modifyVpcEndpointServiceConfiguration_removeSupportedIpAddressTypes,
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
    modifyVpcEndpointServicePermissions_addAllowedPrincipals,
    modifyVpcEndpointServicePermissions_dryRun,
    modifyVpcEndpointServicePermissions_removeAllowedPrincipals,
    modifyVpcEndpointServicePermissions_serviceId,
    modifyVpcEndpointServicePermissionsResponse_addedPrincipals,
    modifyVpcEndpointServicePermissionsResponse_returnValue,
    modifyVpcEndpointServicePermissionsResponse_httpStatus,

    -- ** ModifyVpcPeeringConnectionOptions
    modifyVpcPeeringConnectionOptions_accepterPeeringConnectionOptions,
    modifyVpcPeeringConnectionOptions_dryRun,
    modifyVpcPeeringConnectionOptions_requesterPeeringConnectionOptions,
    modifyVpcPeeringConnectionOptions_vpcPeeringConnectionId,
    modifyVpcPeeringConnectionOptionsResponse_accepterPeeringConnectionOptions,
    modifyVpcPeeringConnectionOptionsResponse_requesterPeeringConnectionOptions,
    modifyVpcPeeringConnectionOptionsResponse_httpStatus,

    -- ** ModifyVpcTenancy
    modifyVpcTenancy_dryRun,
    modifyVpcTenancy_vpcId,
    modifyVpcTenancy_instanceTenancy,
    modifyVpcTenancyResponse_returnValue,
    modifyVpcTenancyResponse_httpStatus,

    -- ** ModifyVpnConnection
    modifyVpnConnection_customerGatewayId,
    modifyVpnConnection_dryRun,
    modifyVpnConnection_transitGatewayId,
    modifyVpnConnection_vpnGatewayId,
    modifyVpnConnection_vpnConnectionId,
    modifyVpnConnectionResponse_vpnConnection,
    modifyVpnConnectionResponse_httpStatus,

    -- ** ModifyVpnConnectionOptions
    modifyVpnConnectionOptions_dryRun,
    modifyVpnConnectionOptions_localIpv4NetworkCidr,
    modifyVpnConnectionOptions_localIpv6NetworkCidr,
    modifyVpnConnectionOptions_remoteIpv4NetworkCidr,
    modifyVpnConnectionOptions_remoteIpv6NetworkCidr,
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
    modifyVpnTunnelOptions_skipTunnelReplacement,
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
    provisionByoipCidr_cidrAuthorizationContext,
    provisionByoipCidr_description,
    provisionByoipCidr_dryRun,
    provisionByoipCidr_multiRegion,
    provisionByoipCidr_poolTagSpecifications,
    provisionByoipCidr_publiclyAdvertisable,
    provisionByoipCidr_cidr,
    provisionByoipCidrResponse_byoipCidr,
    provisionByoipCidrResponse_httpStatus,

    -- ** ProvisionIpamPoolCidr
    provisionIpamPoolCidr_cidr,
    provisionIpamPoolCidr_cidrAuthorizationContext,
    provisionIpamPoolCidr_clientToken,
    provisionIpamPoolCidr_dryRun,
    provisionIpamPoolCidr_netmaskLength,
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
    purchaseHostReservation_currencyCode,
    purchaseHostReservation_limitPrice,
    purchaseHostReservation_tagSpecifications,
    purchaseHostReservation_hostIdSet,
    purchaseHostReservation_offeringId,
    purchaseHostReservationResponse_clientToken,
    purchaseHostReservationResponse_currencyCode,
    purchaseHostReservationResponse_purchase,
    purchaseHostReservationResponse_totalHourlyPrice,
    purchaseHostReservationResponse_totalUpfrontPrice,
    purchaseHostReservationResponse_httpStatus,

    -- ** PurchaseReservedInstancesOffering
    purchaseReservedInstancesOffering_dryRun,
    purchaseReservedInstancesOffering_limitPrice,
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
    registerImage_architecture,
    registerImage_billingProducts,
    registerImage_blockDeviceMappings,
    registerImage_bootMode,
    registerImage_description,
    registerImage_dryRun,
    registerImage_enaSupport,
    registerImage_imageLocation,
    registerImage_imdsSupport,
    registerImage_kernelId,
    registerImage_ramdiskId,
    registerImage_rootDeviceName,
    registerImage_sriovNetSupport,
    registerImage_tpmSupport,
    registerImage_uefiData,
    registerImage_virtualizationType,
    registerImage_name,
    registerImageResponse_imageId,
    registerImageResponse_httpStatus,

    -- ** RegisterInstanceEventNotificationAttributes
    registerInstanceEventNotificationAttributes_dryRun,
    registerInstanceEventNotificationAttributes_instanceTagAttribute,
    registerInstanceEventNotificationAttributesResponse_instanceTagAttribute,
    registerInstanceEventNotificationAttributesResponse_httpStatus,

    -- ** RegisterTransitGatewayMulticastGroupMembers
    registerTransitGatewayMulticastGroupMembers_dryRun,
    registerTransitGatewayMulticastGroupMembers_groupIpAddress,
    registerTransitGatewayMulticastGroupMembers_transitGatewayMulticastDomainId,
    registerTransitGatewayMulticastGroupMembers_networkInterfaceIds,
    registerTransitGatewayMulticastGroupMembersResponse_registeredMulticastGroupMembers,
    registerTransitGatewayMulticastGroupMembersResponse_httpStatus,

    -- ** RegisterTransitGatewayMulticastGroupSources
    registerTransitGatewayMulticastGroupSources_dryRun,
    registerTransitGatewayMulticastGroupSources_groupIpAddress,
    registerTransitGatewayMulticastGroupSources_transitGatewayMulticastDomainId,
    registerTransitGatewayMulticastGroupSources_networkInterfaceIds,
    registerTransitGatewayMulticastGroupSourcesResponse_registeredMulticastGroupSources,
    registerTransitGatewayMulticastGroupSourcesResponse_httpStatus,

    -- ** RejectTransitGatewayMulticastDomainAssociations
    rejectTransitGatewayMulticastDomainAssociations_dryRun,
    rejectTransitGatewayMulticastDomainAssociations_subnetIds,
    rejectTransitGatewayMulticastDomainAssociations_transitGatewayAttachmentId,
    rejectTransitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId,
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
    releaseAddress_dryRun,
    releaseAddress_networkBorderGroup,
    releaseAddress_publicIp,

    -- ** ReleaseHosts
    releaseHosts_hostIds,
    releaseHostsResponse_successful,
    releaseHostsResponse_unsuccessful,
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
    replaceNetworkAclEntry_cidrBlock,
    replaceNetworkAclEntry_dryRun,
    replaceNetworkAclEntry_icmpTypeCode,
    replaceNetworkAclEntry_ipv6CidrBlock,
    replaceNetworkAclEntry_portRange,
    replaceNetworkAclEntry_egress,
    replaceNetworkAclEntry_networkAclId,
    replaceNetworkAclEntry_protocol,
    replaceNetworkAclEntry_ruleAction,
    replaceNetworkAclEntry_ruleNumber,

    -- ** ReplaceRoute
    replaceRoute_carrierGatewayId,
    replaceRoute_coreNetworkArn,
    replaceRoute_destinationCidrBlock,
    replaceRoute_destinationIpv6CidrBlock,
    replaceRoute_destinationPrefixListId,
    replaceRoute_dryRun,
    replaceRoute_egressOnlyInternetGatewayId,
    replaceRoute_gatewayId,
    replaceRoute_instanceId,
    replaceRoute_localGatewayId,
    replaceRoute_localTarget,
    replaceRoute_natGatewayId,
    replaceRoute_networkInterfaceId,
    replaceRoute_transitGatewayId,
    replaceRoute_vpcEndpointId,
    replaceRoute_vpcPeeringConnectionId,
    replaceRoute_routeTableId,

    -- ** ReplaceRouteTableAssociation
    replaceRouteTableAssociation_dryRun,
    replaceRouteTableAssociation_associationId,
    replaceRouteTableAssociation_routeTableId,
    replaceRouteTableAssociationResponse_associationState,
    replaceRouteTableAssociationResponse_newAssociationId,
    replaceRouteTableAssociationResponse_httpStatus,

    -- ** ReplaceTransitGatewayRoute
    replaceTransitGatewayRoute_blackhole,
    replaceTransitGatewayRoute_dryRun,
    replaceTransitGatewayRoute_transitGatewayAttachmentId,
    replaceTransitGatewayRoute_destinationCidrBlock,
    replaceTransitGatewayRoute_transitGatewayRouteTableId,
    replaceTransitGatewayRouteResponse_route,
    replaceTransitGatewayRouteResponse_httpStatus,

    -- ** ReplaceVpnTunnel
    replaceVpnTunnel_applyPendingMaintenance,
    replaceVpnTunnel_dryRun,
    replaceVpnTunnel_vpnConnectionId,
    replaceVpnTunnel_vpnTunnelOutsideIpAddress,
    replaceVpnTunnelResponse_return,
    replaceVpnTunnelResponse_httpStatus,

    -- ** ReportInstanceStatus
    reportInstanceStatus_description,
    reportInstanceStatus_dryRun,
    reportInstanceStatus_endTime,
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
    requestSpotInstances_availabilityZoneGroup,
    requestSpotInstances_blockDurationMinutes,
    requestSpotInstances_clientToken,
    requestSpotInstances_dryRun,
    requestSpotInstances_instanceCount,
    requestSpotInstances_instanceInterruptionBehavior,
    requestSpotInstances_launchGroup,
    requestSpotInstances_launchSpecification,
    requestSpotInstances_spotPrice,
    requestSpotInstances_tagSpecifications,
    requestSpotInstances_type,
    requestSpotInstances_validFrom,
    requestSpotInstances_validUntil,
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
    resetNetworkInterfaceAttribute_dryRun,
    resetNetworkInterfaceAttribute_sourceDestCheck,
    resetNetworkInterfaceAttribute_networkInterfaceId,

    -- ** ResetSnapshotAttribute
    resetSnapshotAttribute_dryRun,
    resetSnapshotAttribute_attribute,
    resetSnapshotAttribute_snapshotId,

    -- ** RestoreAddressToClassic
    restoreAddressToClassic_dryRun,
    restoreAddressToClassic_publicIp,
    restoreAddressToClassicResponse_publicIp,
    restoreAddressToClassicResponse_status,
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
    restoreSnapshotFromRecycleBinResponse_description,
    restoreSnapshotFromRecycleBinResponse_encrypted,
    restoreSnapshotFromRecycleBinResponse_outpostArn,
    restoreSnapshotFromRecycleBinResponse_ownerId,
    restoreSnapshotFromRecycleBinResponse_progress,
    restoreSnapshotFromRecycleBinResponse_snapshotId,
    restoreSnapshotFromRecycleBinResponse_startTime,
    restoreSnapshotFromRecycleBinResponse_state,
    restoreSnapshotFromRecycleBinResponse_volumeId,
    restoreSnapshotFromRecycleBinResponse_volumeSize,
    restoreSnapshotFromRecycleBinResponse_httpStatus,

    -- ** RestoreSnapshotTier
    restoreSnapshotTier_dryRun,
    restoreSnapshotTier_permanentRestore,
    restoreSnapshotTier_temporaryRestoreDays,
    restoreSnapshotTier_snapshotId,
    restoreSnapshotTierResponse_isPermanentRestore,
    restoreSnapshotTierResponse_restoreDuration,
    restoreSnapshotTierResponse_restoreStartTime,
    restoreSnapshotTierResponse_snapshotId,
    restoreSnapshotTierResponse_httpStatus,

    -- ** RevokeClientVpnIngress
    revokeClientVpnIngress_accessGroupId,
    revokeClientVpnIngress_dryRun,
    revokeClientVpnIngress_revokeAllGroups,
    revokeClientVpnIngress_clientVpnEndpointId,
    revokeClientVpnIngress_targetNetworkCidr,
    revokeClientVpnIngressResponse_status,
    revokeClientVpnIngressResponse_httpStatus,

    -- ** RevokeSecurityGroupEgress
    revokeSecurityGroupEgress_cidrIp,
    revokeSecurityGroupEgress_dryRun,
    revokeSecurityGroupEgress_fromPort,
    revokeSecurityGroupEgress_ipPermissions,
    revokeSecurityGroupEgress_ipProtocol,
    revokeSecurityGroupEgress_securityGroupRuleIds,
    revokeSecurityGroupEgress_sourceSecurityGroupName,
    revokeSecurityGroupEgress_sourceSecurityGroupOwnerId,
    revokeSecurityGroupEgress_toPort,
    revokeSecurityGroupEgress_groupId,
    revokeSecurityGroupEgressResponse_return,
    revokeSecurityGroupEgressResponse_unknownIpPermissions,
    revokeSecurityGroupEgressResponse_httpStatus,

    -- ** RevokeSecurityGroupIngress
    revokeSecurityGroupIngress_cidrIp,
    revokeSecurityGroupIngress_dryRun,
    revokeSecurityGroupIngress_fromPort,
    revokeSecurityGroupIngress_groupId,
    revokeSecurityGroupIngress_groupName,
    revokeSecurityGroupIngress_ipPermissions,
    revokeSecurityGroupIngress_ipProtocol,
    revokeSecurityGroupIngress_securityGroupRuleIds,
    revokeSecurityGroupIngress_sourceSecurityGroupName,
    revokeSecurityGroupIngress_sourceSecurityGroupOwnerId,
    revokeSecurityGroupIngress_toPort,
    revokeSecurityGroupIngressResponse_return,
    revokeSecurityGroupIngressResponse_unknownIpPermissions,
    revokeSecurityGroupIngressResponse_httpStatus,

    -- ** RunInstances
    runInstances_additionalInfo,
    runInstances_blockDeviceMappings,
    runInstances_capacityReservationSpecification,
    runInstances_clientToken,
    runInstances_cpuOptions,
    runInstances_creditSpecification,
    runInstances_disableApiStop,
    runInstances_disableApiTermination,
    runInstances_dryRun,
    runInstances_ebsOptimized,
    runInstances_elasticGpuSpecification,
    runInstances_elasticInferenceAccelerators,
    runInstances_enclaveOptions,
    runInstances_hibernationOptions,
    runInstances_iamInstanceProfile,
    runInstances_imageId,
    runInstances_instanceInitiatedShutdownBehavior,
    runInstances_instanceMarketOptions,
    runInstances_instanceType,
    runInstances_ipv6AddressCount,
    runInstances_ipv6Addresses,
    runInstances_kernelId,
    runInstances_keyName,
    runInstances_launchTemplate,
    runInstances_licenseSpecifications,
    runInstances_maintenanceOptions,
    runInstances_metadataOptions,
    runInstances_monitoring,
    runInstances_networkInterfaces,
    runInstances_placement,
    runInstances_privateDnsNameOptions,
    runInstances_privateIpAddress,
    runInstances_ramdiskId,
    runInstances_securityGroupIds,
    runInstances_securityGroups,
    runInstances_subnetId,
    runInstances_tagSpecifications,
    runInstances_userData,
    runInstances_maxCount,
    runInstances_minCount,
    reservation_groups,
    reservation_instances,
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
    searchLocalGatewayRoutes_dryRun,
    searchLocalGatewayRoutes_filters,
    searchLocalGatewayRoutes_maxResults,
    searchLocalGatewayRoutes_nextToken,
    searchLocalGatewayRoutes_localGatewayRouteTableId,
    searchLocalGatewayRoutesResponse_nextToken,
    searchLocalGatewayRoutesResponse_routes,
    searchLocalGatewayRoutesResponse_httpStatus,

    -- ** SearchTransitGatewayMulticastGroups
    searchTransitGatewayMulticastGroups_dryRun,
    searchTransitGatewayMulticastGroups_filters,
    searchTransitGatewayMulticastGroups_maxResults,
    searchTransitGatewayMulticastGroups_nextToken,
    searchTransitGatewayMulticastGroups_transitGatewayMulticastDomainId,
    searchTransitGatewayMulticastGroupsResponse_multicastGroups,
    searchTransitGatewayMulticastGroupsResponse_nextToken,
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
    startNetworkInsightsAnalysis_additionalAccounts,
    startNetworkInsightsAnalysis_dryRun,
    startNetworkInsightsAnalysis_filterInArns,
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
    stopInstances_dryRun,
    stopInstances_force,
    stopInstances_hibernate,
    stopInstances_instanceIds,
    stopInstancesResponse_stoppingInstances,
    stopInstancesResponse_httpStatus,

    -- ** TerminateClientVpnConnections
    terminateClientVpnConnections_connectionId,
    terminateClientVpnConnections_dryRun,
    terminateClientVpnConnections_username,
    terminateClientVpnConnections_clientVpnEndpointId,
    terminateClientVpnConnectionsResponse_clientVpnEndpointId,
    terminateClientVpnConnectionsResponse_connectionStatuses,
    terminateClientVpnConnectionsResponse_username,
    terminateClientVpnConnectionsResponse_httpStatus,

    -- ** TerminateInstances
    terminateInstances_dryRun,
    terminateInstances_instanceIds,
    terminateInstancesResponse_terminatingInstances,
    terminateInstancesResponse_httpStatus,

    -- ** UnassignIpv6Addresses
    unassignIpv6Addresses_ipv6Addresses,
    unassignIpv6Addresses_ipv6Prefixes,
    unassignIpv6Addresses_networkInterfaceId,
    unassignIpv6AddressesResponse_networkInterfaceId,
    unassignIpv6AddressesResponse_unassignedIpv6Addresses,
    unassignIpv6AddressesResponse_unassignedIpv6Prefixes,
    unassignIpv6AddressesResponse_httpStatus,

    -- ** UnassignPrivateIpAddresses
    unassignPrivateIpAddresses_ipv4Prefixes,
    unassignPrivateIpAddresses_privateIpAddresses,
    unassignPrivateIpAddresses_networkInterfaceId,

    -- ** UnassignPrivateNatGatewayAddress
    unassignPrivateNatGatewayAddress_dryRun,
    unassignPrivateNatGatewayAddress_maxDrainDurationSeconds,
    unassignPrivateNatGatewayAddress_natGatewayId,
    unassignPrivateNatGatewayAddress_privateIpAddresses,
    unassignPrivateNatGatewayAddressResponse_natGatewayAddresses,
    unassignPrivateNatGatewayAddressResponse_natGatewayId,
    unassignPrivateNatGatewayAddressResponse_httpStatus,

    -- ** UnmonitorInstances
    unmonitorInstances_dryRun,
    unmonitorInstances_instanceIds,
    unmonitorInstancesResponse_instanceMonitorings,
    unmonitorInstancesResponse_httpStatus,

    -- ** UpdateSecurityGroupRuleDescriptionsEgress
    updateSecurityGroupRuleDescriptionsEgress_dryRun,
    updateSecurityGroupRuleDescriptionsEgress_groupId,
    updateSecurityGroupRuleDescriptionsEgress_groupName,
    updateSecurityGroupRuleDescriptionsEgress_ipPermissions,
    updateSecurityGroupRuleDescriptionsEgress_securityGroupRuleDescriptions,
    updateSecurityGroupRuleDescriptionsEgressResponse_return,
    updateSecurityGroupRuleDescriptionsEgressResponse_httpStatus,

    -- ** UpdateSecurityGroupRuleDescriptionsIngress
    updateSecurityGroupRuleDescriptionsIngress_dryRun,
    updateSecurityGroupRuleDescriptionsIngress_groupId,
    updateSecurityGroupRuleDescriptionsIngress_groupName,
    updateSecurityGroupRuleDescriptionsIngress_ipPermissions,
    updateSecurityGroupRuleDescriptionsIngress_securityGroupRuleDescriptions,
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
    accessScopeAnalysisFinding_findingId,
    accessScopeAnalysisFinding_networkInsightsAccessScopeAnalysisId,
    accessScopeAnalysisFinding_networkInsightsAccessScopeId,

    -- ** AccessScopePath
    accessScopePath_destination,
    accessScopePath_source,
    accessScopePath_throughResources,

    -- ** AccessScopePathRequest
    accessScopePathRequest_destination,
    accessScopePathRequest_source,
    accessScopePathRequest_throughResources,

    -- ** AccountAttribute
    accountAttribute_attributeName,
    accountAttribute_attributeValues,

    -- ** AccountAttributeValue
    accountAttributeValue_attributeValue,

    -- ** ActiveInstance
    activeInstance_instanceHealth,
    activeInstance_instanceId,
    activeInstance_instanceType,
    activeInstance_spotInstanceRequestId,

    -- ** AddIpamOperatingRegion
    addIpamOperatingRegion_regionName,

    -- ** AddPrefixListEntry
    addPrefixListEntry_description,
    addPrefixListEntry_cidr,

    -- ** AddedPrincipal
    addedPrincipal_principal,
    addedPrincipal_principalType,
    addedPrincipal_serviceId,
    addedPrincipal_servicePermissionId,

    -- ** AdditionalDetail
    additionalDetail_additionalDetailType,
    additionalDetail_component,
    additionalDetail_loadBalancers,
    additionalDetail_ruleGroupRuleOptionsPairs,
    additionalDetail_ruleGroupTypePairs,
    additionalDetail_ruleOptions,
    additionalDetail_serviceName,
    additionalDetail_vpcEndpointService,

    -- ** Address
    address_allocationId,
    address_associationId,
    address_carrierIp,
    address_customerOwnedIp,
    address_customerOwnedIpv4Pool,
    address_domain,
    address_instanceId,
    address_networkBorderGroup,
    address_networkInterfaceId,
    address_networkInterfaceOwnerId,
    address_privateIpAddress,
    address_publicIp,
    address_publicIpv4Pool,
    address_tags,

    -- ** AddressAttribute
    addressAttribute_allocationId,
    addressAttribute_ptrRecord,
    addressAttribute_ptrRecordUpdate,
    addressAttribute_publicIp,

    -- ** AddressTransfer
    addressTransfer_addressTransferStatus,
    addressTransfer_allocationId,
    addressTransfer_publicIp,
    addressTransfer_transferAccountId,
    addressTransfer_transferOfferAcceptedTimestamp,
    addressTransfer_transferOfferExpirationTimestamp,

    -- ** AllowedPrincipal
    allowedPrincipal_principal,
    allowedPrincipal_principalType,
    allowedPrincipal_serviceId,
    allowedPrincipal_servicePermissionId,
    allowedPrincipal_tags,

    -- ** AlternatePathHint
    alternatePathHint_componentArn,
    alternatePathHint_componentId,

    -- ** AnalysisAclRule
    analysisAclRule_cidr,
    analysisAclRule_egress,
    analysisAclRule_portRange,
    analysisAclRule_protocol,
    analysisAclRule_ruleAction,
    analysisAclRule_ruleNumber,

    -- ** AnalysisComponent
    analysisComponent_arn,
    analysisComponent_id,
    analysisComponent_name,

    -- ** AnalysisLoadBalancerListener
    analysisLoadBalancerListener_instancePort,
    analysisLoadBalancerListener_loadBalancerPort,

    -- ** AnalysisLoadBalancerTarget
    analysisLoadBalancerTarget_address,
    analysisLoadBalancerTarget_availabilityZone,
    analysisLoadBalancerTarget_instance,
    analysisLoadBalancerTarget_port,

    -- ** AnalysisPacketHeader
    analysisPacketHeader_destinationAddresses,
    analysisPacketHeader_destinationPortRanges,
    analysisPacketHeader_protocol,
    analysisPacketHeader_sourceAddresses,
    analysisPacketHeader_sourcePortRanges,

    -- ** AnalysisRouteTableRoute
    analysisRouteTableRoute_carrierGatewayId,
    analysisRouteTableRoute_coreNetworkArn,
    analysisRouteTableRoute_destinationCidr,
    analysisRouteTableRoute_destinationPrefixListId,
    analysisRouteTableRoute_egressOnlyInternetGatewayId,
    analysisRouteTableRoute_gatewayId,
    analysisRouteTableRoute_instanceId,
    analysisRouteTableRoute_localGatewayId,
    analysisRouteTableRoute_natGatewayId,
    analysisRouteTableRoute_networkInterfaceId,
    analysisRouteTableRoute_origin,
    analysisRouteTableRoute_state,
    analysisRouteTableRoute_transitGatewayId,
    analysisRouteTableRoute_vpcPeeringConnectionId,

    -- ** AnalysisSecurityGroupRule
    analysisSecurityGroupRule_cidr,
    analysisSecurityGroupRule_direction,
    analysisSecurityGroupRule_portRange,
    analysisSecurityGroupRule_prefixListId,
    analysisSecurityGroupRule_protocol,
    analysisSecurityGroupRule_securityGroupId,

    -- ** AssignedPrivateIpAddress
    assignedPrivateIpAddress_privateIpAddress,

    -- ** AssociatedRole
    associatedRole_associatedRoleArn,
    associatedRole_certificateS3BucketName,
    associatedRole_certificateS3ObjectKey,
    associatedRole_encryptionKmsKeyId,

    -- ** AssociatedTargetNetwork
    associatedTargetNetwork_networkId,
    associatedTargetNetwork_networkType,

    -- ** AssociationStatus
    associationStatus_code,
    associationStatus_message,

    -- ** AthenaIntegration
    athenaIntegration_partitionEndDate,
    athenaIntegration_partitionStartDate,
    athenaIntegration_integrationResultS3DestinationArn,
    athenaIntegration_partitionLoadFrequency,

    -- ** AttachmentEnaSrdSpecification
    attachmentEnaSrdSpecification_enaSrdEnabled,
    attachmentEnaSrdSpecification_enaSrdUdpSpecification,

    -- ** AttachmentEnaSrdUdpSpecification
    attachmentEnaSrdUdpSpecification_enaSrdUdpEnabled,

    -- ** AttributeBooleanValue
    attributeBooleanValue_value,

    -- ** AttributeValue
    attributeValue_value,

    -- ** AuthorizationRule
    authorizationRule_accessAll,
    authorizationRule_clientVpnEndpointId,
    authorizationRule_description,
    authorizationRule_destinationCidr,
    authorizationRule_groupId,
    authorizationRule_status,

    -- ** AvailabilityZone
    availabilityZone_groupName,
    availabilityZone_messages,
    availabilityZone_networkBorderGroup,
    availabilityZone_optInStatus,
    availabilityZone_parentZoneId,
    availabilityZone_parentZoneName,
    availabilityZone_regionName,
    availabilityZone_state,
    availabilityZone_zoneId,
    availabilityZone_zoneName,
    availabilityZone_zoneType,

    -- ** AvailabilityZoneMessage
    availabilityZoneMessage_message,

    -- ** AvailableCapacity
    availableCapacity_availableInstanceCapacity,
    availableCapacity_availableVCpus,

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
    bundleTaskError_code,
    bundleTaskError_message,

    -- ** ByoipCidr
    byoipCidr_cidr,
    byoipCidr_description,
    byoipCidr_state,
    byoipCidr_statusMessage,

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
    cancelSpotFleetRequestsSuccessItem_previousSpotFleetRequestState,
    cancelSpotFleetRequestsSuccessItem_spotFleetRequestId,

    -- ** CancelledSpotInstanceRequest
    cancelledSpotInstanceRequest_spotInstanceRequestId,
    cancelledSpotInstanceRequest_state,

    -- ** CapacityAllocation
    capacityAllocation_allocationType,
    capacityAllocation_count,

    -- ** CapacityReservation
    capacityReservation_availabilityZone,
    capacityReservation_availabilityZoneId,
    capacityReservation_availableInstanceCount,
    capacityReservation_capacityAllocations,
    capacityReservation_capacityReservationArn,
    capacityReservation_capacityReservationFleetId,
    capacityReservation_capacityReservationId,
    capacityReservation_createDate,
    capacityReservation_ebsOptimized,
    capacityReservation_endDate,
    capacityReservation_endDateType,
    capacityReservation_ephemeralStorage,
    capacityReservation_instanceMatchCriteria,
    capacityReservation_instancePlatform,
    capacityReservation_instanceType,
    capacityReservation_outpostArn,
    capacityReservation_ownerId,
    capacityReservation_placementGroupArn,
    capacityReservation_startDate,
    capacityReservation_state,
    capacityReservation_tags,
    capacityReservation_tenancy,
    capacityReservation_totalInstanceCount,

    -- ** CapacityReservationFleet
    capacityReservationFleet_allocationStrategy,
    capacityReservationFleet_capacityReservationFleetArn,
    capacityReservationFleet_capacityReservationFleetId,
    capacityReservationFleet_createTime,
    capacityReservationFleet_endDate,
    capacityReservationFleet_instanceMatchCriteria,
    capacityReservationFleet_instanceTypeSpecifications,
    capacityReservationFleet_state,
    capacityReservationFleet_tags,
    capacityReservationFleet_tenancy,
    capacityReservationFleet_totalFulfilledCapacity,
    capacityReservationFleet_totalTargetCapacity,

    -- ** CapacityReservationFleetCancellationState
    capacityReservationFleetCancellationState_capacityReservationFleetId,
    capacityReservationFleetCancellationState_currentFleetState,
    capacityReservationFleetCancellationState_previousFleetState,

    -- ** CapacityReservationGroup
    capacityReservationGroup_groupArn,
    capacityReservationGroup_ownerId,

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
    carrierGateway_carrierGatewayId,
    carrierGateway_ownerId,
    carrierGateway_state,
    carrierGateway_tags,
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
    clientCertificateRevocationListStatus_code,
    clientCertificateRevocationListStatus_message,

    -- ** ClientConnectOptions
    clientConnectOptions_enabled,
    clientConnectOptions_lambdaFunctionArn,

    -- ** ClientConnectResponseOptions
    clientConnectResponseOptions_enabled,
    clientConnectResponseOptions_lambdaFunctionArn,
    clientConnectResponseOptions_status,

    -- ** ClientData
    clientData_comment,
    clientData_uploadEnd,
    clientData_uploadSize,
    clientData_uploadStart,

    -- ** ClientLoginBannerOptions
    clientLoginBannerOptions_bannerText,
    clientLoginBannerOptions_enabled,

    -- ** ClientLoginBannerResponseOptions
    clientLoginBannerResponseOptions_bannerText,
    clientLoginBannerResponseOptions_enabled,

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
    clientVpnConnection_clientIp,
    clientVpnConnection_clientVpnEndpointId,
    clientVpnConnection_commonName,
    clientVpnConnection_connectionEndTime,
    clientVpnConnection_connectionEstablishedTime,
    clientVpnConnection_connectionId,
    clientVpnConnection_egressBytes,
    clientVpnConnection_egressPackets,
    clientVpnConnection_ingressBytes,
    clientVpnConnection_ingressPackets,
    clientVpnConnection_postureComplianceStatuses,
    clientVpnConnection_status,
    clientVpnConnection_timestamp,
    clientVpnConnection_username,

    -- ** ClientVpnConnectionStatus
    clientVpnConnectionStatus_code,
    clientVpnConnectionStatus_message,

    -- ** ClientVpnEndpoint
    clientVpnEndpoint_associatedTargetNetworks,
    clientVpnEndpoint_authenticationOptions,
    clientVpnEndpoint_clientCidrBlock,
    clientVpnEndpoint_clientConnectOptions,
    clientVpnEndpoint_clientLoginBannerOptions,
    clientVpnEndpoint_clientVpnEndpointId,
    clientVpnEndpoint_connectionLogOptions,
    clientVpnEndpoint_creationTime,
    clientVpnEndpoint_deletionTime,
    clientVpnEndpoint_description,
    clientVpnEndpoint_dnsName,
    clientVpnEndpoint_dnsServers,
    clientVpnEndpoint_securityGroupIds,
    clientVpnEndpoint_selfServicePortalUrl,
    clientVpnEndpoint_serverCertificateArn,
    clientVpnEndpoint_sessionTimeoutHours,
    clientVpnEndpoint_splitTunnel,
    clientVpnEndpoint_status,
    clientVpnEndpoint_tags,
    clientVpnEndpoint_transportProtocol,
    clientVpnEndpoint_vpcId,
    clientVpnEndpoint_vpnPort,
    clientVpnEndpoint_vpnProtocol,

    -- ** ClientVpnEndpointAttributeStatus
    clientVpnEndpointAttributeStatus_code,
    clientVpnEndpointAttributeStatus_message,

    -- ** ClientVpnEndpointStatus
    clientVpnEndpointStatus_code,
    clientVpnEndpointStatus_message,

    -- ** ClientVpnRoute
    clientVpnRoute_clientVpnEndpointId,
    clientVpnRoute_description,
    clientVpnRoute_destinationCidr,
    clientVpnRoute_origin,
    clientVpnRoute_status,
    clientVpnRoute_targetSubnet,
    clientVpnRoute_type,

    -- ** ClientVpnRouteStatus
    clientVpnRouteStatus_code,
    clientVpnRouteStatus_message,

    -- ** CloudWatchLogOptions
    cloudWatchLogOptions_logEnabled,
    cloudWatchLogOptions_logGroupArn,
    cloudWatchLogOptions_logOutputFormat,

    -- ** CloudWatchLogOptionsSpecification
    cloudWatchLogOptionsSpecification_logEnabled,
    cloudWatchLogOptionsSpecification_logGroupArn,
    cloudWatchLogOptionsSpecification_logOutputFormat,

    -- ** CoipAddressUsage
    coipAddressUsage_allocationId,
    coipAddressUsage_awsAccountId,
    coipAddressUsage_awsService,
    coipAddressUsage_coIp,

    -- ** CoipCidr
    coipCidr_cidr,
    coipCidr_coipPoolId,
    coipCidr_localGatewayRouteTableId,

    -- ** CoipPool
    coipPool_localGatewayRouteTableId,
    coipPool_poolArn,
    coipPool_poolCidrs,
    coipPool_poolId,
    coipPool_tags,

    -- ** ConnectionLogOptions
    connectionLogOptions_cloudwatchLogGroup,
    connectionLogOptions_cloudwatchLogStream,
    connectionLogOptions_enabled,

    -- ** ConnectionLogResponseOptions
    connectionLogResponseOptions_cloudwatchLogGroup,
    connectionLogResponseOptions_cloudwatchLogStream,
    connectionLogResponseOptions_enabled,

    -- ** ConnectionNotification
    connectionNotification_connectionEvents,
    connectionNotification_connectionNotificationArn,
    connectionNotification_connectionNotificationId,
    connectionNotification_connectionNotificationState,
    connectionNotification_connectionNotificationType,
    connectionNotification_serviceId,
    connectionNotification_vpcEndpointId,

    -- ** ConversionTask
    conversionTask_conversionTaskId,
    conversionTask_expirationTime,
    conversionTask_importInstance,
    conversionTask_importVolume,
    conversionTask_state,
    conversionTask_statusMessage,
    conversionTask_tags,

    -- ** CpuOptions
    cpuOptions_amdSevSnp,
    cpuOptions_coreCount,
    cpuOptions_threadsPerCore,

    -- ** CpuOptionsRequest
    cpuOptionsRequest_amdSevSnp,
    cpuOptionsRequest_coreCount,
    cpuOptionsRequest_threadsPerCore,

    -- ** CreateFleetError
    createFleetError_errorCode,
    createFleetError_errorMessage,
    createFleetError_launchTemplateAndOverrides,
    createFleetError_lifecycle,

    -- ** CreateFleetInstance
    createFleetInstance_instanceIds,
    createFleetInstance_instanceType,
    createFleetInstance_launchTemplateAndOverrides,
    createFleetInstance_lifecycle,
    createFleetInstance_platform,

    -- ** CreateTransitGatewayConnectRequestOptions
    createTransitGatewayConnectRequestOptions_protocol,

    -- ** CreateTransitGatewayMulticastDomainRequestOptions
    createTransitGatewayMulticastDomainRequestOptions_autoAcceptSharedAssociations,
    createTransitGatewayMulticastDomainRequestOptions_igmpv2Support,
    createTransitGatewayMulticastDomainRequestOptions_staticSourcesSupport,

    -- ** CreateTransitGatewayPeeringAttachmentRequestOptions
    createTransitGatewayPeeringAttachmentRequestOptions_dynamicRouting,

    -- ** CreateTransitGatewayVpcAttachmentRequestOptions
    createTransitGatewayVpcAttachmentRequestOptions_applianceModeSupport,
    createTransitGatewayVpcAttachmentRequestOptions_dnsSupport,
    createTransitGatewayVpcAttachmentRequestOptions_ipv6Support,

    -- ** CreateVerifiedAccessEndpointEniOptions
    createVerifiedAccessEndpointEniOptions_networkInterfaceId,
    createVerifiedAccessEndpointEniOptions_port,
    createVerifiedAccessEndpointEniOptions_protocol,

    -- ** CreateVerifiedAccessEndpointLoadBalancerOptions
    createVerifiedAccessEndpointLoadBalancerOptions_loadBalancerArn,
    createVerifiedAccessEndpointLoadBalancerOptions_port,
    createVerifiedAccessEndpointLoadBalancerOptions_protocol,
    createVerifiedAccessEndpointLoadBalancerOptions_subnetIds,

    -- ** CreateVerifiedAccessTrustProviderDeviceOptions
    createVerifiedAccessTrustProviderDeviceOptions_tenantId,

    -- ** CreateVerifiedAccessTrustProviderOidcOptions
    createVerifiedAccessTrustProviderOidcOptions_authorizationEndpoint,
    createVerifiedAccessTrustProviderOidcOptions_clientId,
    createVerifiedAccessTrustProviderOidcOptions_clientSecret,
    createVerifiedAccessTrustProviderOidcOptions_issuer,
    createVerifiedAccessTrustProviderOidcOptions_scope,
    createVerifiedAccessTrustProviderOidcOptions_tokenEndpoint,
    createVerifiedAccessTrustProviderOidcOptions_userInfoEndpoint,

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

    -- ** DataQuery
    dataQuery_destination,
    dataQuery_id,
    dataQuery_metric,
    dataQuery_period,
    dataQuery_source,
    dataQuery_statistic,

    -- ** DataResponse
    dataResponse_destination,
    dataResponse_id,
    dataResponse_metric,
    dataResponse_metricPoints,
    dataResponse_period,
    dataResponse_source,
    dataResponse_statistic,

    -- ** DeleteFleetError
    deleteFleetError_code,
    deleteFleetError_message,

    -- ** DeleteFleetErrorItem
    deleteFleetErrorItem_error,
    deleteFleetErrorItem_fleetId,

    -- ** DeleteFleetSuccessItem
    deleteFleetSuccessItem_currentFleetState,
    deleteFleetSuccessItem_fleetId,
    deleteFleetSuccessItem_previousFleetState,

    -- ** DeleteLaunchTemplateVersionsResponseErrorItem
    deleteLaunchTemplateVersionsResponseErrorItem_launchTemplateId,
    deleteLaunchTemplateVersionsResponseErrorItem_launchTemplateName,
    deleteLaunchTemplateVersionsResponseErrorItem_responseError,
    deleteLaunchTemplateVersionsResponseErrorItem_versionNumber,

    -- ** DeleteLaunchTemplateVersionsResponseSuccessItem
    deleteLaunchTemplateVersionsResponseSuccessItem_launchTemplateId,
    deleteLaunchTemplateVersionsResponseSuccessItem_launchTemplateName,
    deleteLaunchTemplateVersionsResponseSuccessItem_versionNumber,

    -- ** DeleteQueuedReservedInstancesError
    deleteQueuedReservedInstancesError_code,
    deleteQueuedReservedInstancesError_message,

    -- ** DeregisterInstanceTagAttributeRequest
    deregisterInstanceTagAttributeRequest_includeAllTagsOfInstance,
    deregisterInstanceTagAttributeRequest_instanceTagKeys,

    -- ** DescribeFastLaunchImagesSuccessItem
    describeFastLaunchImagesSuccessItem_imageId,
    describeFastLaunchImagesSuccessItem_launchTemplate,
    describeFastLaunchImagesSuccessItem_maxParallelLaunches,
    describeFastLaunchImagesSuccessItem_ownerId,
    describeFastLaunchImagesSuccessItem_resourceType,
    describeFastLaunchImagesSuccessItem_snapshotConfiguration,
    describeFastLaunchImagesSuccessItem_state,
    describeFastLaunchImagesSuccessItem_stateTransitionReason,
    describeFastLaunchImagesSuccessItem_stateTransitionTime,

    -- ** DescribeFastSnapshotRestoreSuccessItem
    describeFastSnapshotRestoreSuccessItem_availabilityZone,
    describeFastSnapshotRestoreSuccessItem_disabledTime,
    describeFastSnapshotRestoreSuccessItem_disablingTime,
    describeFastSnapshotRestoreSuccessItem_enabledTime,
    describeFastSnapshotRestoreSuccessItem_enablingTime,
    describeFastSnapshotRestoreSuccessItem_optimizingTime,
    describeFastSnapshotRestoreSuccessItem_ownerAlias,
    describeFastSnapshotRestoreSuccessItem_ownerId,
    describeFastSnapshotRestoreSuccessItem_snapshotId,
    describeFastSnapshotRestoreSuccessItem_state,
    describeFastSnapshotRestoreSuccessItem_stateTransitionReason,

    -- ** DescribeFleetError
    describeFleetError_errorCode,
    describeFleetError_errorMessage,
    describeFleetError_launchTemplateAndOverrides,
    describeFleetError_lifecycle,

    -- ** DescribeFleetsInstances
    describeFleetsInstances_instanceIds,
    describeFleetsInstances_instanceType,
    describeFleetsInstances_launchTemplateAndOverrides,
    describeFleetsInstances_lifecycle,
    describeFleetsInstances_platform,

    -- ** DestinationOptionsRequest
    destinationOptionsRequest_fileFormat,
    destinationOptionsRequest_hiveCompatiblePartitions,
    destinationOptionsRequest_perHourPartition,

    -- ** DestinationOptionsResponse
    destinationOptionsResponse_fileFormat,
    destinationOptionsResponse_hiveCompatiblePartitions,
    destinationOptionsResponse_perHourPartition,

    -- ** DeviceOptions
    deviceOptions_tenantId,

    -- ** DhcpConfiguration
    dhcpConfiguration_key,
    dhcpConfiguration_values,

    -- ** DhcpOptions
    dhcpOptions_dhcpConfigurations,
    dhcpOptions_dhcpOptionsId,
    dhcpOptions_ownerId,
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
    disableFastSnapshotRestoreStateErrorItem_availabilityZone,
    disableFastSnapshotRestoreStateErrorItem_error,

    -- ** DisableFastSnapshotRestoreSuccessItem
    disableFastSnapshotRestoreSuccessItem_availabilityZone,
    disableFastSnapshotRestoreSuccessItem_disabledTime,
    disableFastSnapshotRestoreSuccessItem_disablingTime,
    disableFastSnapshotRestoreSuccessItem_enabledTime,
    disableFastSnapshotRestoreSuccessItem_enablingTime,
    disableFastSnapshotRestoreSuccessItem_optimizingTime,
    disableFastSnapshotRestoreSuccessItem_ownerAlias,
    disableFastSnapshotRestoreSuccessItem_ownerId,
    disableFastSnapshotRestoreSuccessItem_snapshotId,
    disableFastSnapshotRestoreSuccessItem_state,
    disableFastSnapshotRestoreSuccessItem_stateTransitionReason,

    -- ** DiskImage
    diskImage_description,
    diskImage_image,
    diskImage_volume,

    -- ** DiskImageDescription
    diskImageDescription_checksum,
    diskImageDescription_format,
    diskImageDescription_importManifestUrl,
    diskImageDescription_size,

    -- ** DiskImageDetail
    diskImageDetail_bytes,
    diskImageDetail_format,
    diskImageDetail_importManifestUrl,

    -- ** DiskImageVolumeDescription
    diskImageVolumeDescription_id,
    diskImageVolumeDescription_size,

    -- ** DiskInfo
    diskInfo_count,
    diskInfo_sizeInGB,
    diskInfo_type,

    -- ** DnsEntry
    dnsEntry_dnsName,
    dnsEntry_hostedZoneId,

    -- ** DnsOptions
    dnsOptions_dnsRecordIpType,
    dnsOptions_privateDnsOnlyForInboundResolverEndpoint,

    -- ** DnsOptionsSpecification
    dnsOptionsSpecification_dnsRecordIpType,
    dnsOptionsSpecification_privateDnsOnlyForInboundResolverEndpoint,

    -- ** DnsServersOptionsModifyStructure
    dnsServersOptionsModifyStructure_customDnsServers,
    dnsServersOptionsModifyStructure_enabled,

    -- ** EbsBlockDevice
    ebsBlockDevice_deleteOnTermination,
    ebsBlockDevice_encrypted,
    ebsBlockDevice_iops,
    ebsBlockDevice_kmsKeyId,
    ebsBlockDevice_outpostArn,
    ebsBlockDevice_snapshotId,
    ebsBlockDevice_throughput,
    ebsBlockDevice_volumeSize,
    ebsBlockDevice_volumeType,

    -- ** EbsInfo
    ebsInfo_ebsOptimizedInfo,
    ebsInfo_ebsOptimizedSupport,
    ebsInfo_encryptionSupport,
    ebsInfo_nvmeSupport,

    -- ** EbsInstanceBlockDevice
    ebsInstanceBlockDevice_attachTime,
    ebsInstanceBlockDevice_deleteOnTermination,
    ebsInstanceBlockDevice_status,
    ebsInstanceBlockDevice_volumeId,

    -- ** EbsInstanceBlockDeviceSpecification
    ebsInstanceBlockDeviceSpecification_deleteOnTermination,
    ebsInstanceBlockDeviceSpecification_volumeId,

    -- ** EbsOptimizedInfo
    ebsOptimizedInfo_baselineBandwidthInMbps,
    ebsOptimizedInfo_baselineIops,
    ebsOptimizedInfo_baselineThroughputInMBps,
    ebsOptimizedInfo_maximumBandwidthInMbps,
    ebsOptimizedInfo_maximumIops,
    ebsOptimizedInfo_maximumThroughputInMBps,

    -- ** Ec2InstanceConnectEndpoint
    ec2InstanceConnectEndpoint_availabilityZone,
    ec2InstanceConnectEndpoint_createdAt,
    ec2InstanceConnectEndpoint_dnsName,
    ec2InstanceConnectEndpoint_fipsDnsName,
    ec2InstanceConnectEndpoint_instanceConnectEndpointArn,
    ec2InstanceConnectEndpoint_instanceConnectEndpointId,
    ec2InstanceConnectEndpoint_networkInterfaceIds,
    ec2InstanceConnectEndpoint_ownerId,
    ec2InstanceConnectEndpoint_preserveClientIp,
    ec2InstanceConnectEndpoint_securityGroupIds,
    ec2InstanceConnectEndpoint_state,
    ec2InstanceConnectEndpoint_stateMessage,
    ec2InstanceConnectEndpoint_subnetId,
    ec2InstanceConnectEndpoint_tags,
    ec2InstanceConnectEndpoint_vpcId,

    -- ** EfaInfo
    efaInfo_maximumEfaInterfaces,

    -- ** EgressOnlyInternetGateway
    egressOnlyInternetGateway_attachments,
    egressOnlyInternetGateway_egressOnlyInternetGatewayId,
    egressOnlyInternetGateway_tags,

    -- ** ElasticGpuAssociation
    elasticGpuAssociation_elasticGpuAssociationId,
    elasticGpuAssociation_elasticGpuAssociationState,
    elasticGpuAssociation_elasticGpuAssociationTime,
    elasticGpuAssociation_elasticGpuId,

    -- ** ElasticGpuHealth
    elasticGpuHealth_status,

    -- ** ElasticGpuSpecification
    elasticGpuSpecification_type,

    -- ** ElasticGpuSpecificationResponse
    elasticGpuSpecificationResponse_type,

    -- ** ElasticGpus
    elasticGpus_availabilityZone,
    elasticGpus_elasticGpuHealth,
    elasticGpus_elasticGpuId,
    elasticGpus_elasticGpuState,
    elasticGpus_elasticGpuType,
    elasticGpus_instanceId,
    elasticGpus_tags,

    -- ** ElasticInferenceAccelerator
    elasticInferenceAccelerator_count,
    elasticInferenceAccelerator_type,

    -- ** ElasticInferenceAcceleratorAssociation
    elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorArn,
    elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorAssociationId,
    elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorAssociationState,
    elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorAssociationTime,

    -- ** EnaSrdSpecification
    enaSrdSpecification_enaSrdEnabled,
    enaSrdSpecification_enaSrdUdpSpecification,

    -- ** EnaSrdUdpSpecification
    enaSrdUdpSpecification_enaSrdUdpEnabled,

    -- ** EnableFastSnapshotRestoreErrorItem
    enableFastSnapshotRestoreErrorItem_fastSnapshotRestoreStateErrors,
    enableFastSnapshotRestoreErrorItem_snapshotId,

    -- ** EnableFastSnapshotRestoreStateError
    enableFastSnapshotRestoreStateError_code,
    enableFastSnapshotRestoreStateError_message,

    -- ** EnableFastSnapshotRestoreStateErrorItem
    enableFastSnapshotRestoreStateErrorItem_availabilityZone,
    enableFastSnapshotRestoreStateErrorItem_error,

    -- ** EnableFastSnapshotRestoreSuccessItem
    enableFastSnapshotRestoreSuccessItem_availabilityZone,
    enableFastSnapshotRestoreSuccessItem_disabledTime,
    enableFastSnapshotRestoreSuccessItem_disablingTime,
    enableFastSnapshotRestoreSuccessItem_enabledTime,
    enableFastSnapshotRestoreSuccessItem_enablingTime,
    enableFastSnapshotRestoreSuccessItem_optimizingTime,
    enableFastSnapshotRestoreSuccessItem_ownerAlias,
    enableFastSnapshotRestoreSuccessItem_ownerId,
    enableFastSnapshotRestoreSuccessItem_snapshotId,
    enableFastSnapshotRestoreSuccessItem_state,
    enableFastSnapshotRestoreSuccessItem_stateTransitionReason,

    -- ** EnclaveOptions
    enclaveOptions_enabled,

    -- ** EnclaveOptionsRequest
    enclaveOptionsRequest_enabled,

    -- ** EventInformation
    eventInformation_eventDescription,
    eventInformation_eventSubType,
    eventInformation_instanceId,

    -- ** Explanation
    explanation_acl,
    explanation_aclRule,
    explanation_address,
    explanation_addresses,
    explanation_attachedTo,
    explanation_availabilityZones,
    explanation_cidrs,
    explanation_classicLoadBalancerListener,
    explanation_component,
    explanation_componentAccount,
    explanation_componentRegion,
    explanation_customerGateway,
    explanation_destination,
    explanation_destinationVpc,
    explanation_direction,
    explanation_elasticLoadBalancerListener,
    explanation_explanationCode,
    explanation_firewallStatefulRule,
    explanation_firewallStatelessRule,
    explanation_ingressRouteTable,
    explanation_internetGateway,
    explanation_loadBalancerArn,
    explanation_loadBalancerListenerPort,
    explanation_loadBalancerTarget,
    explanation_loadBalancerTargetGroup,
    explanation_loadBalancerTargetGroups,
    explanation_loadBalancerTargetPort,
    explanation_missingComponent,
    explanation_natGateway,
    explanation_networkInterface,
    explanation_packetField,
    explanation_port,
    explanation_portRanges,
    explanation_prefixList,
    explanation_protocols,
    explanation_routeTable,
    explanation_routeTableRoute,
    explanation_securityGroup,
    explanation_securityGroupRule,
    explanation_securityGroups,
    explanation_sourceVpc,
    explanation_state,
    explanation_subnet,
    explanation_subnetRouteTable,
    explanation_transitGateway,
    explanation_transitGatewayAttachment,
    explanation_transitGatewayRouteTable,
    explanation_transitGatewayRouteTableRoute,
    explanation_vpc,
    explanation_vpcEndpoint,
    explanation_vpcPeeringConnection,
    explanation_vpnConnection,
    explanation_vpnGateway,

    -- ** ExportImageTask
    exportImageTask_description,
    exportImageTask_exportImageTaskId,
    exportImageTask_imageId,
    exportImageTask_progress,
    exportImageTask_s3ExportLocation,
    exportImageTask_status,
    exportImageTask_statusMessage,
    exportImageTask_tags,

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

    -- ** FailedCapacityReservationFleetCancellationResult
    failedCapacityReservationFleetCancellationResult_cancelCapacityReservationFleetError,
    failedCapacityReservationFleetCancellationResult_capacityReservationFleetId,

    -- ** FailedQueuedPurchaseDeletion
    failedQueuedPurchaseDeletion_error,
    failedQueuedPurchaseDeletion_reservedInstancesId,

    -- ** FastLaunchLaunchTemplateSpecificationRequest
    fastLaunchLaunchTemplateSpecificationRequest_launchTemplateId,
    fastLaunchLaunchTemplateSpecificationRequest_launchTemplateName,
    fastLaunchLaunchTemplateSpecificationRequest_version,

    -- ** FastLaunchLaunchTemplateSpecificationResponse
    fastLaunchLaunchTemplateSpecificationResponse_launchTemplateId,
    fastLaunchLaunchTemplateSpecificationResponse_launchTemplateName,
    fastLaunchLaunchTemplateSpecificationResponse_version,

    -- ** FastLaunchSnapshotConfigurationRequest
    fastLaunchSnapshotConfigurationRequest_targetResourceCount,

    -- ** FastLaunchSnapshotConfigurationResponse
    fastLaunchSnapshotConfigurationResponse_targetResourceCount,

    -- ** FederatedAuthentication
    federatedAuthentication_samlProviderArn,
    federatedAuthentication_selfServiceSamlProviderArn,

    -- ** FederatedAuthenticationRequest
    federatedAuthenticationRequest_sAMLProviderArn,
    federatedAuthenticationRequest_selfServiceSAMLProviderArn,

    -- ** Filter
    filter_values,
    filter_name,

    -- ** FilterPortRange
    filterPortRange_fromPort,
    filterPortRange_toPort,

    -- ** FirewallStatefulRule
    firewallStatefulRule_destinationPorts,
    firewallStatefulRule_destinations,
    firewallStatefulRule_direction,
    firewallStatefulRule_protocol,
    firewallStatefulRule_ruleAction,
    firewallStatefulRule_ruleGroupArn,
    firewallStatefulRule_sourcePorts,
    firewallStatefulRule_sources,

    -- ** FirewallStatelessRule
    firewallStatelessRule_destinationPorts,
    firewallStatelessRule_destinations,
    firewallStatelessRule_priority,
    firewallStatelessRule_protocols,
    firewallStatelessRule_ruleAction,
    firewallStatelessRule_ruleGroupArn,
    firewallStatelessRule_sourcePorts,
    firewallStatelessRule_sources,

    -- ** FleetCapacityReservation
    fleetCapacityReservation_availabilityZone,
    fleetCapacityReservation_availabilityZoneId,
    fleetCapacityReservation_capacityReservationId,
    fleetCapacityReservation_createDate,
    fleetCapacityReservation_ebsOptimized,
    fleetCapacityReservation_fulfilledCapacity,
    fleetCapacityReservation_instancePlatform,
    fleetCapacityReservation_instanceType,
    fleetCapacityReservation_priority,
    fleetCapacityReservation_totalInstanceCount,
    fleetCapacityReservation_weight,

    -- ** FleetData
    fleetData_activityStatus,
    fleetData_clientToken,
    fleetData_context,
    fleetData_createTime,
    fleetData_errors,
    fleetData_excessCapacityTerminationPolicy,
    fleetData_fleetId,
    fleetData_fleetState,
    fleetData_fulfilledCapacity,
    fleetData_fulfilledOnDemandCapacity,
    fleetData_instances,
    fleetData_launchTemplateConfigs,
    fleetData_onDemandOptions,
    fleetData_replaceUnhealthyInstances,
    fleetData_spotOptions,
    fleetData_tags,
    fleetData_targetCapacitySpecification,
    fleetData_terminateInstancesWithExpiration,
    fleetData_type,
    fleetData_validFrom,
    fleetData_validUntil,

    -- ** FleetLaunchTemplateConfig
    fleetLaunchTemplateConfig_launchTemplateSpecification,
    fleetLaunchTemplateConfig_overrides,

    -- ** FleetLaunchTemplateConfigRequest
    fleetLaunchTemplateConfigRequest_launchTemplateSpecification,
    fleetLaunchTemplateConfigRequest_overrides,

    -- ** FleetLaunchTemplateOverrides
    fleetLaunchTemplateOverrides_availabilityZone,
    fleetLaunchTemplateOverrides_imageId,
    fleetLaunchTemplateOverrides_instanceRequirements,
    fleetLaunchTemplateOverrides_instanceType,
    fleetLaunchTemplateOverrides_maxPrice,
    fleetLaunchTemplateOverrides_placement,
    fleetLaunchTemplateOverrides_priority,
    fleetLaunchTemplateOverrides_subnetId,
    fleetLaunchTemplateOverrides_weightedCapacity,

    -- ** FleetLaunchTemplateOverridesRequest
    fleetLaunchTemplateOverridesRequest_availabilityZone,
    fleetLaunchTemplateOverridesRequest_imageId,
    fleetLaunchTemplateOverridesRequest_instanceRequirements,
    fleetLaunchTemplateOverridesRequest_instanceType,
    fleetLaunchTemplateOverridesRequest_maxPrice,
    fleetLaunchTemplateOverridesRequest_placement,
    fleetLaunchTemplateOverridesRequest_priority,
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
    fleetSpotCapacityRebalance_terminationDelay,

    -- ** FleetSpotCapacityRebalanceRequest
    fleetSpotCapacityRebalanceRequest_replacementStrategy,
    fleetSpotCapacityRebalanceRequest_terminationDelay,

    -- ** FleetSpotMaintenanceStrategies
    fleetSpotMaintenanceStrategies_capacityRebalance,

    -- ** FleetSpotMaintenanceStrategiesRequest
    fleetSpotMaintenanceStrategiesRequest_capacityRebalance,

    -- ** FlowLog
    flowLog_creationTime,
    flowLog_deliverCrossAccountRole,
    flowLog_deliverLogsErrorMessage,
    flowLog_deliverLogsPermissionArn,
    flowLog_deliverLogsStatus,
    flowLog_destinationOptions,
    flowLog_flowLogId,
    flowLog_flowLogStatus,
    flowLog_logDestination,
    flowLog_logDestinationType,
    flowLog_logFormat,
    flowLog_logGroupName,
    flowLog_maxAggregationInterval,
    flowLog_resourceId,
    flowLog_tags,
    flowLog_trafficType,

    -- ** FpgaDeviceInfo
    fpgaDeviceInfo_count,
    fpgaDeviceInfo_manufacturer,
    fpgaDeviceInfo_memoryInfo,
    fpgaDeviceInfo_name,

    -- ** FpgaDeviceMemoryInfo
    fpgaDeviceMemoryInfo_sizeInMiB,

    -- ** FpgaImage
    fpgaImage_createTime,
    fpgaImage_dataRetentionSupport,
    fpgaImage_description,
    fpgaImage_fpgaImageGlobalId,
    fpgaImage_fpgaImageId,
    fpgaImage_instanceTypes,
    fpgaImage_name,
    fpgaImage_ownerAlias,
    fpgaImage_ownerId,
    fpgaImage_pciId,
    fpgaImage_productCodes,
    fpgaImage_public,
    fpgaImage_shellVersion,
    fpgaImage_state,
    fpgaImage_tags,
    fpgaImage_updateTime,

    -- ** FpgaImageAttribute
    fpgaImageAttribute_description,
    fpgaImageAttribute_fpgaImageId,
    fpgaImageAttribute_loadPermissions,
    fpgaImageAttribute_name,
    fpgaImageAttribute_productCodes,

    -- ** FpgaImageState
    fpgaImageState_code,
    fpgaImageState_message,

    -- ** FpgaInfo
    fpgaInfo_fpgas,
    fpgaInfo_totalFpgaMemoryInMiB,

    -- ** GpuDeviceInfo
    gpuDeviceInfo_count,
    gpuDeviceInfo_manufacturer,
    gpuDeviceInfo_memoryInfo,
    gpuDeviceInfo_name,

    -- ** GpuDeviceMemoryInfo
    gpuDeviceMemoryInfo_sizeInMiB,

    -- ** GpuInfo
    gpuInfo_gpus,
    gpuInfo_totalGpuMemoryInMiB,

    -- ** GroupIdentifier
    groupIdentifier_groupId,
    groupIdentifier_groupName,

    -- ** HibernationOptions
    hibernationOptions_configured,

    -- ** HibernationOptionsRequest
    hibernationOptionsRequest_configured,

    -- ** HistoryRecord
    historyRecord_eventInformation,
    historyRecord_eventType,
    historyRecord_timestamp,

    -- ** HistoryRecordEntry
    historyRecordEntry_eventInformation,
    historyRecordEntry_eventType,
    historyRecordEntry_timestamp,

    -- ** Host
    host_allocationTime,
    host_allowsMultipleInstanceTypes,
    host_assetId,
    host_autoPlacement,
    host_availabilityZone,
    host_availabilityZoneId,
    host_availableCapacity,
    host_clientToken,
    host_hostId,
    host_hostMaintenance,
    host_hostProperties,
    host_hostRecovery,
    host_hostReservationId,
    host_instances,
    host_memberOfServiceLinkedResourceGroup,
    host_outpostArn,
    host_ownerId,
    host_releaseTime,
    host_state,
    host_tags,

    -- ** HostInstance
    hostInstance_instanceId,
    hostInstance_instanceType,
    hostInstance_ownerId,

    -- ** HostOffering
    hostOffering_currencyCode,
    hostOffering_duration,
    hostOffering_hourlyPrice,
    hostOffering_instanceFamily,
    hostOffering_offeringId,
    hostOffering_paymentOption,
    hostOffering_upfrontPrice,

    -- ** HostProperties
    hostProperties_cores,
    hostProperties_instanceFamily,
    hostProperties_instanceType,
    hostProperties_sockets,
    hostProperties_totalVCpus,

    -- ** HostReservation
    hostReservation_count,
    hostReservation_currencyCode,
    hostReservation_duration,
    hostReservation_end,
    hostReservation_hostIdSet,
    hostReservation_hostReservationId,
    hostReservation_hourlyPrice,
    hostReservation_instanceFamily,
    hostReservation_offeringId,
    hostReservation_paymentOption,
    hostReservation_start,
    hostReservation_state,
    hostReservation_tags,
    hostReservation_upfrontPrice,

    -- ** IKEVersionsListValue
    iKEVersionsListValue_value,

    -- ** IKEVersionsRequestListValue
    iKEVersionsRequestListValue_value,

    -- ** IamInstanceProfile
    iamInstanceProfile_arn,
    iamInstanceProfile_id,

    -- ** IamInstanceProfileAssociation
    iamInstanceProfileAssociation_associationId,
    iamInstanceProfileAssociation_iamInstanceProfile,
    iamInstanceProfileAssociation_instanceId,
    iamInstanceProfileAssociation_state,
    iamInstanceProfileAssociation_timestamp,

    -- ** IamInstanceProfileSpecification
    iamInstanceProfileSpecification_arn,
    iamInstanceProfileSpecification_name,

    -- ** IcmpTypeCode
    icmpTypeCode_code,
    icmpTypeCode_type,

    -- ** IdFormat
    idFormat_deadline,
    idFormat_resource,
    idFormat_useLongIds,

    -- ** Image
    image_blockDeviceMappings,
    image_bootMode,
    image_creationDate,
    image_deprecationTime,
    image_description,
    image_enaSupport,
    image_imageOwnerAlias,
    image_imdsSupport,
    image_kernelId,
    image_name,
    image_platform,
    image_platformDetails,
    image_productCodes,
    image_ramdiskId,
    image_rootDeviceName,
    image_sriovNetSupport,
    image_stateReason,
    image_tags,
    image_tpmSupport,
    image_usageOperation,
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
    imageDiskContainer_description,
    imageDiskContainer_deviceName,
    imageDiskContainer_format,
    imageDiskContainer_snapshotId,
    imageDiskContainer_url,
    imageDiskContainer_userBucket,

    -- ** ImageRecycleBinInfo
    imageRecycleBinInfo_description,
    imageRecycleBinInfo_imageId,
    imageRecycleBinInfo_name,
    imageRecycleBinInfo_recycleBinEnterTime,
    imageRecycleBinInfo_recycleBinExitTime,

    -- ** ImportImageLicenseConfigurationRequest
    importImageLicenseConfigurationRequest_licenseConfigurationArn,

    -- ** ImportImageLicenseConfigurationResponse
    importImageLicenseConfigurationResponse_licenseConfigurationArn,

    -- ** ImportImageTask
    importImageTask_architecture,
    importImageTask_bootMode,
    importImageTask_description,
    importImageTask_encrypted,
    importImageTask_hypervisor,
    importImageTask_imageId,
    importImageTask_importTaskId,
    importImageTask_kmsKeyId,
    importImageTask_licenseSpecifications,
    importImageTask_licenseType,
    importImageTask_platform,
    importImageTask_progress,
    importImageTask_snapshotDetails,
    importImageTask_status,
    importImageTask_statusMessage,
    importImageTask_tags,
    importImageTask_usageOperation,

    -- ** ImportInstanceLaunchSpecification
    importInstanceLaunchSpecification_additionalInfo,
    importInstanceLaunchSpecification_architecture,
    importInstanceLaunchSpecification_groupIds,
    importInstanceLaunchSpecification_groupNames,
    importInstanceLaunchSpecification_instanceInitiatedShutdownBehavior,
    importInstanceLaunchSpecification_instanceType,
    importInstanceLaunchSpecification_monitoring,
    importInstanceLaunchSpecification_placement,
    importInstanceLaunchSpecification_privateIpAddress,
    importInstanceLaunchSpecification_subnetId,
    importInstanceLaunchSpecification_userData,

    -- ** ImportInstanceTaskDetails
    importInstanceTaskDetails_description,
    importInstanceTaskDetails_instanceId,
    importInstanceTaskDetails_platform,
    importInstanceTaskDetails_volumes,

    -- ** ImportInstanceVolumeDetailItem
    importInstanceVolumeDetailItem_availabilityZone,
    importInstanceVolumeDetailItem_bytesConverted,
    importInstanceVolumeDetailItem_description,
    importInstanceVolumeDetailItem_image,
    importInstanceVolumeDetailItem_status,
    importInstanceVolumeDetailItem_statusMessage,
    importInstanceVolumeDetailItem_volume,

    -- ** ImportSnapshotTask
    importSnapshotTask_description,
    importSnapshotTask_importTaskId,
    importSnapshotTask_snapshotTaskDetail,
    importSnapshotTask_tags,

    -- ** ImportVolumeTaskDetails
    importVolumeTaskDetails_availabilityZone,
    importVolumeTaskDetails_bytesConverted,
    importVolumeTaskDetails_description,
    importVolumeTaskDetails_image,
    importVolumeTaskDetails_volume,

    -- ** InferenceAcceleratorInfo
    inferenceAcceleratorInfo_accelerators,

    -- ** InferenceDeviceInfo
    inferenceDeviceInfo_count,
    inferenceDeviceInfo_manufacturer,
    inferenceDeviceInfo_name,

    -- ** Instance
    instance_blockDeviceMappings,
    instance_bootMode,
    instance_capacityReservationId,
    instance_capacityReservationSpecification,
    instance_clientToken,
    instance_cpuOptions,
    instance_currentInstanceBootMode,
    instance_ebsOptimized,
    instance_elasticGpuAssociations,
    instance_elasticInferenceAcceleratorAssociations,
    instance_enaSupport,
    instance_enclaveOptions,
    instance_hibernationOptions,
    instance_iamInstanceProfile,
    instance_instanceLifecycle,
    instance_ipv6Address,
    instance_kernelId,
    instance_keyName,
    instance_licenses,
    instance_maintenanceOptions,
    instance_metadataOptions,
    instance_networkInterfaces,
    instance_outpostArn,
    instance_platform,
    instance_platformDetails,
    instance_privateDnsName,
    instance_privateDnsNameOptions,
    instance_privateIpAddress,
    instance_productCodes,
    instance_publicDnsName,
    instance_publicIpAddress,
    instance_ramdiskId,
    instance_rootDeviceName,
    instance_securityGroups,
    instance_sourceDestCheck,
    instance_spotInstanceRequestId,
    instance_sriovNetSupport,
    instance_stateReason,
    instance_stateTransitionReason,
    instance_subnetId,
    instance_tags,
    instance_tpmSupport,
    instance_usageOperation,
    instance_usageOperationUpdateTime,
    instance_vpcId,
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
    instanceBlockDeviceMapping_deviceName,
    instanceBlockDeviceMapping_ebs,

    -- ** InstanceBlockDeviceMappingSpecification
    instanceBlockDeviceMappingSpecification_deviceName,
    instanceBlockDeviceMappingSpecification_ebs,
    instanceBlockDeviceMappingSpecification_noDevice,
    instanceBlockDeviceMappingSpecification_virtualName,

    -- ** InstanceCapacity
    instanceCapacity_availableCapacity,
    instanceCapacity_instanceType,
    instanceCapacity_totalCapacity,

    -- ** InstanceCount
    instanceCount_instanceCount,
    instanceCount_state,

    -- ** InstanceCreditSpecification
    instanceCreditSpecification_cpuCredits,
    instanceCreditSpecification_instanceId,

    -- ** InstanceCreditSpecificationRequest
    instanceCreditSpecificationRequest_cpuCredits,
    instanceCreditSpecificationRequest_instanceId,

    -- ** InstanceEventWindow
    instanceEventWindow_associationTarget,
    instanceEventWindow_cronExpression,
    instanceEventWindow_instanceEventWindowId,
    instanceEventWindow_name,
    instanceEventWindow_state,
    instanceEventWindow_tags,
    instanceEventWindow_timeRanges,

    -- ** InstanceEventWindowAssociationRequest
    instanceEventWindowAssociationRequest_dedicatedHostIds,
    instanceEventWindowAssociationRequest_instanceIds,
    instanceEventWindowAssociationRequest_instanceTags,

    -- ** InstanceEventWindowAssociationTarget
    instanceEventWindowAssociationTarget_dedicatedHostIds,
    instanceEventWindowAssociationTarget_instanceIds,
    instanceEventWindowAssociationTarget_tags,

    -- ** InstanceEventWindowDisassociationRequest
    instanceEventWindowDisassociationRequest_dedicatedHostIds,
    instanceEventWindowDisassociationRequest_instanceIds,
    instanceEventWindowDisassociationRequest_instanceTags,

    -- ** InstanceEventWindowStateChange
    instanceEventWindowStateChange_instanceEventWindowId,
    instanceEventWindowStateChange_state,

    -- ** InstanceEventWindowTimeRange
    instanceEventWindowTimeRange_endHour,
    instanceEventWindowTimeRange_endWeekDay,
    instanceEventWindowTimeRange_startHour,
    instanceEventWindowTimeRange_startWeekDay,

    -- ** InstanceEventWindowTimeRangeRequest
    instanceEventWindowTimeRangeRequest_endHour,
    instanceEventWindowTimeRangeRequest_endWeekDay,
    instanceEventWindowTimeRangeRequest_startHour,
    instanceEventWindowTimeRangeRequest_startWeekDay,

    -- ** InstanceExportDetails
    instanceExportDetails_instanceId,
    instanceExportDetails_targetEnvironment,

    -- ** InstanceFamilyCreditSpecification
    instanceFamilyCreditSpecification_cpuCredits,
    instanceFamilyCreditSpecification_instanceFamily,

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
    instanceMetadataOptionsRequest_httpEndpoint,
    instanceMetadataOptionsRequest_httpProtocolIpv6,
    instanceMetadataOptionsRequest_httpPutResponseHopLimit,
    instanceMetadataOptionsRequest_httpTokens,
    instanceMetadataOptionsRequest_instanceMetadataTags,

    -- ** InstanceMetadataOptionsResponse
    instanceMetadataOptionsResponse_httpEndpoint,
    instanceMetadataOptionsResponse_httpProtocolIpv6,
    instanceMetadataOptionsResponse_httpPutResponseHopLimit,
    instanceMetadataOptionsResponse_httpTokens,
    instanceMetadataOptionsResponse_instanceMetadataTags,
    instanceMetadataOptionsResponse_state,

    -- ** InstanceMonitoring
    instanceMonitoring_instanceId,
    instanceMonitoring_monitoring,

    -- ** InstanceNetworkInterface
    instanceNetworkInterface_association,
    instanceNetworkInterface_attachment,
    instanceNetworkInterface_description,
    instanceNetworkInterface_groups,
    instanceNetworkInterface_interfaceType,
    instanceNetworkInterface_ipv4Prefixes,
    instanceNetworkInterface_ipv6Addresses,
    instanceNetworkInterface_ipv6Prefixes,
    instanceNetworkInterface_macAddress,
    instanceNetworkInterface_networkInterfaceId,
    instanceNetworkInterface_ownerId,
    instanceNetworkInterface_privateDnsName,
    instanceNetworkInterface_privateIpAddress,
    instanceNetworkInterface_privateIpAddresses,
    instanceNetworkInterface_sourceDestCheck,
    instanceNetworkInterface_status,
    instanceNetworkInterface_subnetId,
    instanceNetworkInterface_vpcId,

    -- ** InstanceNetworkInterfaceAssociation
    instanceNetworkInterfaceAssociation_carrierIp,
    instanceNetworkInterfaceAssociation_customerOwnedIp,
    instanceNetworkInterfaceAssociation_ipOwnerId,
    instanceNetworkInterfaceAssociation_publicDnsName,
    instanceNetworkInterfaceAssociation_publicIp,

    -- ** InstanceNetworkInterfaceAttachment
    instanceNetworkInterfaceAttachment_attachTime,
    instanceNetworkInterfaceAttachment_attachmentId,
    instanceNetworkInterfaceAttachment_deleteOnTermination,
    instanceNetworkInterfaceAttachment_deviceIndex,
    instanceNetworkInterfaceAttachment_networkCardIndex,
    instanceNetworkInterfaceAttachment_status,

    -- ** InstanceNetworkInterfaceSpecification
    instanceNetworkInterfaceSpecification_associateCarrierIpAddress,
    instanceNetworkInterfaceSpecification_associatePublicIpAddress,
    instanceNetworkInterfaceSpecification_deleteOnTermination,
    instanceNetworkInterfaceSpecification_description,
    instanceNetworkInterfaceSpecification_deviceIndex,
    instanceNetworkInterfaceSpecification_groups,
    instanceNetworkInterfaceSpecification_interfaceType,
    instanceNetworkInterfaceSpecification_ipv4PrefixCount,
    instanceNetworkInterfaceSpecification_ipv4Prefixes,
    instanceNetworkInterfaceSpecification_ipv6AddressCount,
    instanceNetworkInterfaceSpecification_ipv6Addresses,
    instanceNetworkInterfaceSpecification_ipv6PrefixCount,
    instanceNetworkInterfaceSpecification_ipv6Prefixes,
    instanceNetworkInterfaceSpecification_networkCardIndex,
    instanceNetworkInterfaceSpecification_networkInterfaceId,
    instanceNetworkInterfaceSpecification_privateIpAddress,
    instanceNetworkInterfaceSpecification_privateIpAddresses,
    instanceNetworkInterfaceSpecification_secondaryPrivateIpAddressCount,
    instanceNetworkInterfaceSpecification_subnetId,

    -- ** InstancePrivateIpAddress
    instancePrivateIpAddress_association,
    instancePrivateIpAddress_primary,
    instancePrivateIpAddress_privateDnsName,
    instancePrivateIpAddress_privateIpAddress,

    -- ** InstanceRequirements
    instanceRequirements_acceleratorCount,
    instanceRequirements_acceleratorManufacturers,
    instanceRequirements_acceleratorNames,
    instanceRequirements_acceleratorTotalMemoryMiB,
    instanceRequirements_acceleratorTypes,
    instanceRequirements_allowedInstanceTypes,
    instanceRequirements_bareMetal,
    instanceRequirements_baselineEbsBandwidthMbps,
    instanceRequirements_burstablePerformance,
    instanceRequirements_cpuManufacturers,
    instanceRequirements_excludedInstanceTypes,
    instanceRequirements_instanceGenerations,
    instanceRequirements_localStorage,
    instanceRequirements_localStorageTypes,
    instanceRequirements_memoryGiBPerVCpu,
    instanceRequirements_memoryMiB,
    instanceRequirements_networkBandwidthGbps,
    instanceRequirements_networkInterfaceCount,
    instanceRequirements_onDemandMaxPricePercentageOverLowestPrice,
    instanceRequirements_requireHibernateSupport,
    instanceRequirements_spotMaxPricePercentageOverLowestPrice,
    instanceRequirements_totalLocalStorageGB,
    instanceRequirements_vCpuCount,

    -- ** InstanceRequirementsRequest
    instanceRequirementsRequest_acceleratorCount,
    instanceRequirementsRequest_acceleratorManufacturers,
    instanceRequirementsRequest_acceleratorNames,
    instanceRequirementsRequest_acceleratorTotalMemoryMiB,
    instanceRequirementsRequest_acceleratorTypes,
    instanceRequirementsRequest_allowedInstanceTypes,
    instanceRequirementsRequest_bareMetal,
    instanceRequirementsRequest_baselineEbsBandwidthMbps,
    instanceRequirementsRequest_burstablePerformance,
    instanceRequirementsRequest_cpuManufacturers,
    instanceRequirementsRequest_excludedInstanceTypes,
    instanceRequirementsRequest_instanceGenerations,
    instanceRequirementsRequest_localStorage,
    instanceRequirementsRequest_localStorageTypes,
    instanceRequirementsRequest_memoryGiBPerVCpu,
    instanceRequirementsRequest_networkBandwidthGbps,
    instanceRequirementsRequest_networkInterfaceCount,
    instanceRequirementsRequest_onDemandMaxPricePercentageOverLowestPrice,
    instanceRequirementsRequest_requireHibernateSupport,
    instanceRequirementsRequest_spotMaxPricePercentageOverLowestPrice,
    instanceRequirementsRequest_totalLocalStorageGB,
    instanceRequirementsRequest_vCpuCount,
    instanceRequirementsRequest_memoryMiB,

    -- ** InstanceRequirementsWithMetadataRequest
    instanceRequirementsWithMetadataRequest_architectureTypes,
    instanceRequirementsWithMetadataRequest_instanceRequirements,
    instanceRequirementsWithMetadataRequest_virtualizationTypes,

    -- ** InstanceSpecification
    instanceSpecification_excludeBootVolume,
    instanceSpecification_excludeDataVolumeIds,
    instanceSpecification_instanceId,

    -- ** InstanceState
    instanceState_name,
    instanceState_code,

    -- ** InstanceStateChange
    instanceStateChange_currentState,
    instanceStateChange_instanceId,
    instanceStateChange_previousState,

    -- ** InstanceStatus
    instanceStatus_availabilityZone,
    instanceStatus_events,
    instanceStatus_instanceId,
    instanceStatus_instanceState,
    instanceStatus_instanceStatus,
    instanceStatus_outpostArn,
    instanceStatus_systemStatus,

    -- ** InstanceStatusDetails
    instanceStatusDetails_impairedSince,
    instanceStatusDetails_name,
    instanceStatusDetails_status,

    -- ** InstanceStatusEvent
    instanceStatusEvent_code,
    instanceStatusEvent_description,
    instanceStatusEvent_instanceEventId,
    instanceStatusEvent_notAfter,
    instanceStatusEvent_notBefore,
    instanceStatusEvent_notBeforeDeadline,

    -- ** InstanceStatusSummary
    instanceStatusSummary_details,
    instanceStatusSummary_status,

    -- ** InstanceStorageInfo
    instanceStorageInfo_disks,
    instanceStorageInfo_encryptionSupport,
    instanceStorageInfo_nvmeSupport,
    instanceStorageInfo_totalSizeInGB,

    -- ** InstanceTagNotificationAttribute
    instanceTagNotificationAttribute_includeAllTagsOfInstance,
    instanceTagNotificationAttribute_instanceTagKeys,

    -- ** InstanceTypeInfo
    instanceTypeInfo_autoRecoverySupported,
    instanceTypeInfo_bareMetal,
    instanceTypeInfo_burstablePerformanceSupported,
    instanceTypeInfo_currentGeneration,
    instanceTypeInfo_dedicatedHostsSupported,
    instanceTypeInfo_ebsInfo,
    instanceTypeInfo_fpgaInfo,
    instanceTypeInfo_freeTierEligible,
    instanceTypeInfo_gpuInfo,
    instanceTypeInfo_hibernationSupported,
    instanceTypeInfo_hypervisor,
    instanceTypeInfo_inferenceAcceleratorInfo,
    instanceTypeInfo_instanceStorageInfo,
    instanceTypeInfo_instanceStorageSupported,
    instanceTypeInfo_instanceType,
    instanceTypeInfo_memoryInfo,
    instanceTypeInfo_networkInfo,
    instanceTypeInfo_placementGroupInfo,
    instanceTypeInfo_processorInfo,
    instanceTypeInfo_supportedBootModes,
    instanceTypeInfo_supportedRootDeviceTypes,
    instanceTypeInfo_supportedUsageClasses,
    instanceTypeInfo_supportedVirtualizationTypes,
    instanceTypeInfo_vCpuInfo,

    -- ** InstanceTypeInfoFromInstanceRequirements
    instanceTypeInfoFromInstanceRequirements_instanceType,

    -- ** InstanceTypeOffering
    instanceTypeOffering_instanceType,
    instanceTypeOffering_location,
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
    ipPermission_ipRanges,
    ipPermission_ipv6Ranges,
    ipPermission_prefixListIds,
    ipPermission_toPort,
    ipPermission_userIdGroupPairs,
    ipPermission_ipProtocol,

    -- ** IpRange
    ipRange_description,
    ipRange_cidrIp,

    -- ** Ipam
    ipam_defaultResourceDiscoveryAssociationId,
    ipam_defaultResourceDiscoveryId,
    ipam_description,
    ipam_ipamArn,
    ipam_ipamId,
    ipam_ipamRegion,
    ipam_operatingRegions,
    ipam_ownerId,
    ipam_privateDefaultScopeId,
    ipam_publicDefaultScopeId,
    ipam_resourceDiscoveryAssociationCount,
    ipam_scopeCount,
    ipam_state,
    ipam_tags,

    -- ** IpamAddressHistoryRecord
    ipamAddressHistoryRecord_resourceCidr,
    ipamAddressHistoryRecord_resourceComplianceStatus,
    ipamAddressHistoryRecord_resourceId,
    ipamAddressHistoryRecord_resourceName,
    ipamAddressHistoryRecord_resourceOverlapStatus,
    ipamAddressHistoryRecord_resourceOwnerId,
    ipamAddressHistoryRecord_resourceRegion,
    ipamAddressHistoryRecord_resourceType,
    ipamAddressHistoryRecord_sampledEndTime,
    ipamAddressHistoryRecord_sampledStartTime,
    ipamAddressHistoryRecord_vpcId,

    -- ** IpamCidrAuthorizationContext
    ipamCidrAuthorizationContext_message,
    ipamCidrAuthorizationContext_signature,

    -- ** IpamDiscoveredAccount
    ipamDiscoveredAccount_accountId,
    ipamDiscoveredAccount_discoveryRegion,
    ipamDiscoveredAccount_failureReason,
    ipamDiscoveredAccount_lastAttemptedDiscoveryTime,
    ipamDiscoveredAccount_lastSuccessfulDiscoveryTime,

    -- ** IpamDiscoveredResourceCidr
    ipamDiscoveredResourceCidr_ipUsage,
    ipamDiscoveredResourceCidr_ipamResourceDiscoveryId,
    ipamDiscoveredResourceCidr_resourceCidr,
    ipamDiscoveredResourceCidr_resourceId,
    ipamDiscoveredResourceCidr_resourceOwnerId,
    ipamDiscoveredResourceCidr_resourceRegion,
    ipamDiscoveredResourceCidr_resourceTags,
    ipamDiscoveredResourceCidr_resourceType,
    ipamDiscoveredResourceCidr_sampleTime,
    ipamDiscoveredResourceCidr_vpcId,

    -- ** IpamDiscoveryFailureReason
    ipamDiscoveryFailureReason_code,
    ipamDiscoveryFailureReason_message,

    -- ** IpamOperatingRegion
    ipamOperatingRegion_regionName,

    -- ** IpamPool
    ipamPool_addressFamily,
    ipamPool_allocationDefaultNetmaskLength,
    ipamPool_allocationMaxNetmaskLength,
    ipamPool_allocationMinNetmaskLength,
    ipamPool_allocationResourceTags,
    ipamPool_autoImport,
    ipamPool_awsService,
    ipamPool_description,
    ipamPool_ipamArn,
    ipamPool_ipamPoolArn,
    ipamPool_ipamPoolId,
    ipamPool_ipamRegion,
    ipamPool_ipamScopeArn,
    ipamPool_ipamScopeType,
    ipamPool_locale,
    ipamPool_ownerId,
    ipamPool_poolDepth,
    ipamPool_publicIpSource,
    ipamPool_publiclyAdvertisable,
    ipamPool_sourceIpamPoolId,
    ipamPool_state,
    ipamPool_stateMessage,
    ipamPool_tags,

    -- ** IpamPoolAllocation
    ipamPoolAllocation_cidr,
    ipamPoolAllocation_description,
    ipamPoolAllocation_ipamPoolAllocationId,
    ipamPoolAllocation_resourceId,
    ipamPoolAllocation_resourceOwner,
    ipamPoolAllocation_resourceRegion,
    ipamPoolAllocation_resourceType,

    -- ** IpamPoolCidr
    ipamPoolCidr_cidr,
    ipamPoolCidr_failureReason,
    ipamPoolCidr_ipamPoolCidrId,
    ipamPoolCidr_netmaskLength,
    ipamPoolCidr_state,

    -- ** IpamPoolCidrFailureReason
    ipamPoolCidrFailureReason_code,
    ipamPoolCidrFailureReason_message,

    -- ** IpamResourceCidr
    ipamResourceCidr_complianceStatus,
    ipamResourceCidr_ipUsage,
    ipamResourceCidr_ipamId,
    ipamResourceCidr_ipamPoolId,
    ipamResourceCidr_ipamScopeId,
    ipamResourceCidr_managementState,
    ipamResourceCidr_overlapStatus,
    ipamResourceCidr_resourceCidr,
    ipamResourceCidr_resourceId,
    ipamResourceCidr_resourceName,
    ipamResourceCidr_resourceOwnerId,
    ipamResourceCidr_resourceRegion,
    ipamResourceCidr_resourceTags,
    ipamResourceCidr_resourceType,
    ipamResourceCidr_vpcId,

    -- ** IpamResourceDiscovery
    ipamResourceDiscovery_description,
    ipamResourceDiscovery_ipamResourceDiscoveryArn,
    ipamResourceDiscovery_ipamResourceDiscoveryId,
    ipamResourceDiscovery_ipamResourceDiscoveryRegion,
    ipamResourceDiscovery_isDefault,
    ipamResourceDiscovery_operatingRegions,
    ipamResourceDiscovery_ownerId,
    ipamResourceDiscovery_state,
    ipamResourceDiscovery_tags,

    -- ** IpamResourceDiscoveryAssociation
    ipamResourceDiscoveryAssociation_ipamArn,
    ipamResourceDiscoveryAssociation_ipamId,
    ipamResourceDiscoveryAssociation_ipamRegion,
    ipamResourceDiscoveryAssociation_ipamResourceDiscoveryAssociationArn,
    ipamResourceDiscoveryAssociation_ipamResourceDiscoveryAssociationId,
    ipamResourceDiscoveryAssociation_ipamResourceDiscoveryId,
    ipamResourceDiscoveryAssociation_isDefault,
    ipamResourceDiscoveryAssociation_ownerId,
    ipamResourceDiscoveryAssociation_resourceDiscoveryStatus,
    ipamResourceDiscoveryAssociation_state,
    ipamResourceDiscoveryAssociation_tags,

    -- ** IpamResourceTag
    ipamResourceTag_key,
    ipamResourceTag_value,

    -- ** IpamScope
    ipamScope_description,
    ipamScope_ipamArn,
    ipamScope_ipamRegion,
    ipamScope_ipamScopeArn,
    ipamScope_ipamScopeId,
    ipamScope_ipamScopeType,
    ipamScope_isDefault,
    ipamScope_ownerId,
    ipamScope_poolCount,
    ipamScope_state,
    ipamScope_tags,

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
    ipv6Pool_description,
    ipv6Pool_poolCidrBlocks,
    ipv6Pool_poolId,
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
    keyPairInfo_createTime,
    keyPairInfo_keyFingerprint,
    keyPairInfo_keyName,
    keyPairInfo_keyPairId,
    keyPairInfo_keyType,
    keyPairInfo_publicKey,
    keyPairInfo_tags,

    -- ** LastError
    lastError_code,
    lastError_message,

    -- ** LaunchPermission
    launchPermission_group,
    launchPermission_organizationArn,
    launchPermission_organizationalUnitArn,
    launchPermission_userId,

    -- ** LaunchPermissionModifications
    launchPermissionModifications_add,
    launchPermissionModifications_remove,

    -- ** LaunchSpecification
    launchSpecification_addressingType,
    launchSpecification_blockDeviceMappings,
    launchSpecification_ebsOptimized,
    launchSpecification_iamInstanceProfile,
    launchSpecification_imageId,
    launchSpecification_instanceType,
    launchSpecification_kernelId,
    launchSpecification_keyName,
    launchSpecification_monitoring,
    launchSpecification_networkInterfaces,
    launchSpecification_placement,
    launchSpecification_ramdiskId,
    launchSpecification_securityGroups,
    launchSpecification_subnetId,
    launchSpecification_userData,

    -- ** LaunchTemplate
    launchTemplate_createTime,
    launchTemplate_createdBy,
    launchTemplate_defaultVersionNumber,
    launchTemplate_latestVersionNumber,
    launchTemplate_launchTemplateId,
    launchTemplate_launchTemplateName,
    launchTemplate_tags,

    -- ** LaunchTemplateAndOverridesResponse
    launchTemplateAndOverridesResponse_launchTemplateSpecification,
    launchTemplateAndOverridesResponse_overrides,

    -- ** LaunchTemplateBlockDeviceMapping
    launchTemplateBlockDeviceMapping_deviceName,
    launchTemplateBlockDeviceMapping_ebs,
    launchTemplateBlockDeviceMapping_noDevice,
    launchTemplateBlockDeviceMapping_virtualName,

    -- ** LaunchTemplateBlockDeviceMappingRequest
    launchTemplateBlockDeviceMappingRequest_deviceName,
    launchTemplateBlockDeviceMappingRequest_ebs,
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
    launchTemplateCpuOptions_amdSevSnp,
    launchTemplateCpuOptions_coreCount,
    launchTemplateCpuOptions_threadsPerCore,

    -- ** LaunchTemplateCpuOptionsRequest
    launchTemplateCpuOptionsRequest_amdSevSnp,
    launchTemplateCpuOptionsRequest_coreCount,
    launchTemplateCpuOptionsRequest_threadsPerCore,

    -- ** LaunchTemplateEbsBlockDevice
    launchTemplateEbsBlockDevice_deleteOnTermination,
    launchTemplateEbsBlockDevice_encrypted,
    launchTemplateEbsBlockDevice_iops,
    launchTemplateEbsBlockDevice_kmsKeyId,
    launchTemplateEbsBlockDevice_snapshotId,
    launchTemplateEbsBlockDevice_throughput,
    launchTemplateEbsBlockDevice_volumeSize,
    launchTemplateEbsBlockDevice_volumeType,

    -- ** LaunchTemplateEbsBlockDeviceRequest
    launchTemplateEbsBlockDeviceRequest_deleteOnTermination,
    launchTemplateEbsBlockDeviceRequest_encrypted,
    launchTemplateEbsBlockDeviceRequest_iops,
    launchTemplateEbsBlockDeviceRequest_kmsKeyId,
    launchTemplateEbsBlockDeviceRequest_snapshotId,
    launchTemplateEbsBlockDeviceRequest_throughput,
    launchTemplateEbsBlockDeviceRequest_volumeSize,
    launchTemplateEbsBlockDeviceRequest_volumeType,

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
    launchTemplateInstanceMetadataOptions_httpEndpoint,
    launchTemplateInstanceMetadataOptions_httpProtocolIpv6,
    launchTemplateInstanceMetadataOptions_httpPutResponseHopLimit,
    launchTemplateInstanceMetadataOptions_httpTokens,
    launchTemplateInstanceMetadataOptions_instanceMetadataTags,
    launchTemplateInstanceMetadataOptions_state,

    -- ** LaunchTemplateInstanceMetadataOptionsRequest
    launchTemplateInstanceMetadataOptionsRequest_httpEndpoint,
    launchTemplateInstanceMetadataOptionsRequest_httpProtocolIpv6,
    launchTemplateInstanceMetadataOptionsRequest_httpPutResponseHopLimit,
    launchTemplateInstanceMetadataOptionsRequest_httpTokens,
    launchTemplateInstanceMetadataOptionsRequest_instanceMetadataTags,

    -- ** LaunchTemplateInstanceNetworkInterfaceSpecification
    launchTemplateInstanceNetworkInterfaceSpecification_associateCarrierIpAddress,
    launchTemplateInstanceNetworkInterfaceSpecification_associatePublicIpAddress,
    launchTemplateInstanceNetworkInterfaceSpecification_deleteOnTermination,
    launchTemplateInstanceNetworkInterfaceSpecification_description,
    launchTemplateInstanceNetworkInterfaceSpecification_deviceIndex,
    launchTemplateInstanceNetworkInterfaceSpecification_groups,
    launchTemplateInstanceNetworkInterfaceSpecification_interfaceType,
    launchTemplateInstanceNetworkInterfaceSpecification_ipv4PrefixCount,
    launchTemplateInstanceNetworkInterfaceSpecification_ipv4Prefixes,
    launchTemplateInstanceNetworkInterfaceSpecification_ipv6AddressCount,
    launchTemplateInstanceNetworkInterfaceSpecification_ipv6Addresses,
    launchTemplateInstanceNetworkInterfaceSpecification_ipv6PrefixCount,
    launchTemplateInstanceNetworkInterfaceSpecification_ipv6Prefixes,
    launchTemplateInstanceNetworkInterfaceSpecification_networkCardIndex,
    launchTemplateInstanceNetworkInterfaceSpecification_networkInterfaceId,
    launchTemplateInstanceNetworkInterfaceSpecification_privateIpAddress,
    launchTemplateInstanceNetworkInterfaceSpecification_privateIpAddresses,
    launchTemplateInstanceNetworkInterfaceSpecification_secondaryPrivateIpAddressCount,
    launchTemplateInstanceNetworkInterfaceSpecification_subnetId,

    -- ** LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_associateCarrierIpAddress,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_associatePublicIpAddress,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_deleteOnTermination,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_description,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_deviceIndex,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_groups,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_interfaceType,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv4PrefixCount,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv4Prefixes,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6AddressCount,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6Addresses,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6PrefixCount,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6Prefixes,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_networkCardIndex,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_networkInterfaceId,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_privateIpAddress,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_privateIpAddresses,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_secondaryPrivateIpAddressCount,
    launchTemplateInstanceNetworkInterfaceSpecificationRequest_subnetId,

    -- ** LaunchTemplateLicenseConfiguration
    launchTemplateLicenseConfiguration_licenseConfigurationArn,

    -- ** LaunchTemplateLicenseConfigurationRequest
    launchTemplateLicenseConfigurationRequest_licenseConfigurationArn,

    -- ** LaunchTemplateOverrides
    launchTemplateOverrides_availabilityZone,
    launchTemplateOverrides_instanceRequirements,
    launchTemplateOverrides_instanceType,
    launchTemplateOverrides_priority,
    launchTemplateOverrides_spotPrice,
    launchTemplateOverrides_subnetId,
    launchTemplateOverrides_weightedCapacity,

    -- ** LaunchTemplatePlacement
    launchTemplatePlacement_affinity,
    launchTemplatePlacement_availabilityZone,
    launchTemplatePlacement_groupId,
    launchTemplatePlacement_groupName,
    launchTemplatePlacement_hostId,
    launchTemplatePlacement_hostResourceGroupArn,
    launchTemplatePlacement_partitionNumber,
    launchTemplatePlacement_spreadDomain,
    launchTemplatePlacement_tenancy,

    -- ** LaunchTemplatePlacementRequest
    launchTemplatePlacementRequest_affinity,
    launchTemplatePlacementRequest_availabilityZone,
    launchTemplatePlacementRequest_groupId,
    launchTemplatePlacementRequest_groupName,
    launchTemplatePlacementRequest_hostId,
    launchTemplatePlacementRequest_hostResourceGroupArn,
    launchTemplatePlacementRequest_partitionNumber,
    launchTemplatePlacementRequest_spreadDomain,
    launchTemplatePlacementRequest_tenancy,

    -- ** LaunchTemplatePrivateDnsNameOptions
    launchTemplatePrivateDnsNameOptions_enableResourceNameDnsAAAARecord,
    launchTemplatePrivateDnsNameOptions_enableResourceNameDnsARecord,
    launchTemplatePrivateDnsNameOptions_hostnameType,

    -- ** LaunchTemplatePrivateDnsNameOptionsRequest
    launchTemplatePrivateDnsNameOptionsRequest_enableResourceNameDnsAAAARecord,
    launchTemplatePrivateDnsNameOptionsRequest_enableResourceNameDnsARecord,
    launchTemplatePrivateDnsNameOptionsRequest_hostnameType,

    -- ** LaunchTemplateSpecification
    launchTemplateSpecification_launchTemplateId,
    launchTemplateSpecification_launchTemplateName,
    launchTemplateSpecification_version,

    -- ** LaunchTemplateSpotMarketOptions
    launchTemplateSpotMarketOptions_blockDurationMinutes,
    launchTemplateSpotMarketOptions_instanceInterruptionBehavior,
    launchTemplateSpotMarketOptions_maxPrice,
    launchTemplateSpotMarketOptions_spotInstanceType,
    launchTemplateSpotMarketOptions_validUntil,

    -- ** LaunchTemplateSpotMarketOptionsRequest
    launchTemplateSpotMarketOptionsRequest_blockDurationMinutes,
    launchTemplateSpotMarketOptionsRequest_instanceInterruptionBehavior,
    launchTemplateSpotMarketOptionsRequest_maxPrice,
    launchTemplateSpotMarketOptionsRequest_spotInstanceType,
    launchTemplateSpotMarketOptionsRequest_validUntil,

    -- ** LaunchTemplateTagSpecification
    launchTemplateTagSpecification_resourceType,
    launchTemplateTagSpecification_tags,

    -- ** LaunchTemplateTagSpecificationRequest
    launchTemplateTagSpecificationRequest_resourceType,
    launchTemplateTagSpecificationRequest_tags,

    -- ** LaunchTemplateVersion
    launchTemplateVersion_createTime,
    launchTemplateVersion_createdBy,
    launchTemplateVersion_defaultVersion,
    launchTemplateVersion_launchTemplateData,
    launchTemplateVersion_launchTemplateId,
    launchTemplateVersion_launchTemplateName,
    launchTemplateVersion_versionDescription,
    launchTemplateVersion_versionNumber,

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
    localGateway_localGatewayId,
    localGateway_outpostArn,
    localGateway_ownerId,
    localGateway_state,
    localGateway_tags,

    -- ** LocalGatewayRoute
    localGatewayRoute_coipPoolId,
    localGatewayRoute_destinationCidrBlock,
    localGatewayRoute_destinationPrefixListId,
    localGatewayRoute_localGatewayRouteTableArn,
    localGatewayRoute_localGatewayRouteTableId,
    localGatewayRoute_localGatewayVirtualInterfaceGroupId,
    localGatewayRoute_networkInterfaceId,
    localGatewayRoute_ownerId,
    localGatewayRoute_state,
    localGatewayRoute_subnetId,
    localGatewayRoute_type,

    -- ** LocalGatewayRouteTable
    localGatewayRouteTable_localGatewayId,
    localGatewayRouteTable_localGatewayRouteTableArn,
    localGatewayRouteTable_localGatewayRouteTableId,
    localGatewayRouteTable_mode,
    localGatewayRouteTable_outpostArn,
    localGatewayRouteTable_ownerId,
    localGatewayRouteTable_state,
    localGatewayRouteTable_stateReason,
    localGatewayRouteTable_tags,

    -- ** LocalGatewayRouteTableVirtualInterfaceGroupAssociation
    localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayId,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableArn,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableId,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableVirtualInterfaceGroupAssociationId,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayVirtualInterfaceGroupId,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_ownerId,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_state,
    localGatewayRouteTableVirtualInterfaceGroupAssociation_tags,

    -- ** LocalGatewayRouteTableVpcAssociation
    localGatewayRouteTableVpcAssociation_localGatewayId,
    localGatewayRouteTableVpcAssociation_localGatewayRouteTableArn,
    localGatewayRouteTableVpcAssociation_localGatewayRouteTableId,
    localGatewayRouteTableVpcAssociation_localGatewayRouteTableVpcAssociationId,
    localGatewayRouteTableVpcAssociation_ownerId,
    localGatewayRouteTableVpcAssociation_state,
    localGatewayRouteTableVpcAssociation_tags,
    localGatewayRouteTableVpcAssociation_vpcId,

    -- ** LocalGatewayVirtualInterface
    localGatewayVirtualInterface_localAddress,
    localGatewayVirtualInterface_localBgpAsn,
    localGatewayVirtualInterface_localGatewayId,
    localGatewayVirtualInterface_localGatewayVirtualInterfaceId,
    localGatewayVirtualInterface_ownerId,
    localGatewayVirtualInterface_peerAddress,
    localGatewayVirtualInterface_peerBgpAsn,
    localGatewayVirtualInterface_tags,
    localGatewayVirtualInterface_vlan,

    -- ** LocalGatewayVirtualInterfaceGroup
    localGatewayVirtualInterfaceGroup_localGatewayId,
    localGatewayVirtualInterfaceGroup_localGatewayVirtualInterfaceGroupId,
    localGatewayVirtualInterfaceGroup_localGatewayVirtualInterfaceIds,
    localGatewayVirtualInterfaceGroup_ownerId,
    localGatewayVirtualInterfaceGroup_tags,

    -- ** MaintenanceDetails
    maintenanceDetails_lastMaintenanceApplied,
    maintenanceDetails_maintenanceAutoAppliedAfter,
    maintenanceDetails_pendingMaintenance,

    -- ** ManagedPrefixList
    managedPrefixList_addressFamily,
    managedPrefixList_maxEntries,
    managedPrefixList_ownerId,
    managedPrefixList_prefixListArn,
    managedPrefixList_prefixListId,
    managedPrefixList_prefixListName,
    managedPrefixList_state,
    managedPrefixList_stateMessage,
    managedPrefixList_tags,
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

    -- ** MetricPoint
    metricPoint_endDate,
    metricPoint_startDate,
    metricPoint_status,
    metricPoint_value,

    -- ** ModifyTransitGatewayOptions
    modifyTransitGatewayOptions_addTransitGatewayCidrBlocks,
    modifyTransitGatewayOptions_amazonSideAsn,
    modifyTransitGatewayOptions_associationDefaultRouteTableId,
    modifyTransitGatewayOptions_autoAcceptSharedAttachments,
    modifyTransitGatewayOptions_defaultRouteTableAssociation,
    modifyTransitGatewayOptions_defaultRouteTablePropagation,
    modifyTransitGatewayOptions_dnsSupport,
    modifyTransitGatewayOptions_propagationDefaultRouteTableId,
    modifyTransitGatewayOptions_removeTransitGatewayCidrBlocks,
    modifyTransitGatewayOptions_vpnEcmpSupport,

    -- ** ModifyTransitGatewayVpcAttachmentRequestOptions
    modifyTransitGatewayVpcAttachmentRequestOptions_applianceModeSupport,
    modifyTransitGatewayVpcAttachmentRequestOptions_dnsSupport,
    modifyTransitGatewayVpcAttachmentRequestOptions_ipv6Support,

    -- ** ModifyVerifiedAccessEndpointEniOptions
    modifyVerifiedAccessEndpointEniOptions_port,
    modifyVerifiedAccessEndpointEniOptions_protocol,

    -- ** ModifyVerifiedAccessEndpointLoadBalancerOptions
    modifyVerifiedAccessEndpointLoadBalancerOptions_port,
    modifyVerifiedAccessEndpointLoadBalancerOptions_protocol,
    modifyVerifiedAccessEndpointLoadBalancerOptions_subnetIds,

    -- ** ModifyVerifiedAccessTrustProviderOidcOptions
    modifyVerifiedAccessTrustProviderOidcOptions_authorizationEndpoint,
    modifyVerifiedAccessTrustProviderOidcOptions_clientId,
    modifyVerifiedAccessTrustProviderOidcOptions_clientSecret,
    modifyVerifiedAccessTrustProviderOidcOptions_issuer,
    modifyVerifiedAccessTrustProviderOidcOptions_scope,
    modifyVerifiedAccessTrustProviderOidcOptions_tokenEndpoint,
    modifyVerifiedAccessTrustProviderOidcOptions_userInfoEndpoint,

    -- ** ModifyVpnTunnelOptionsSpecification
    modifyVpnTunnelOptionsSpecification_dPDTimeoutAction,
    modifyVpnTunnelOptionsSpecification_dPDTimeoutSeconds,
    modifyVpnTunnelOptionsSpecification_enableTunnelLifecycleControl,
    modifyVpnTunnelOptionsSpecification_iKEVersions,
    modifyVpnTunnelOptionsSpecification_logOptions,
    modifyVpnTunnelOptionsSpecification_phase1DHGroupNumbers,
    modifyVpnTunnelOptionsSpecification_phase1EncryptionAlgorithms,
    modifyVpnTunnelOptionsSpecification_phase1IntegrityAlgorithms,
    modifyVpnTunnelOptionsSpecification_phase1LifetimeSeconds,
    modifyVpnTunnelOptionsSpecification_phase2DHGroupNumbers,
    modifyVpnTunnelOptionsSpecification_phase2EncryptionAlgorithms,
    modifyVpnTunnelOptionsSpecification_phase2IntegrityAlgorithms,
    modifyVpnTunnelOptionsSpecification_phase2LifetimeSeconds,
    modifyVpnTunnelOptionsSpecification_preSharedKey,
    modifyVpnTunnelOptionsSpecification_rekeyFuzzPercentage,
    modifyVpnTunnelOptionsSpecification_rekeyMarginTimeSeconds,
    modifyVpnTunnelOptionsSpecification_replayWindowSize,
    modifyVpnTunnelOptionsSpecification_startupAction,
    modifyVpnTunnelOptionsSpecification_tunnelInsideCidr,
    modifyVpnTunnelOptionsSpecification_tunnelInsideIpv6Cidr,

    -- ** Monitoring
    monitoring_state,

    -- ** MovingAddressStatus
    movingAddressStatus_moveStatus,
    movingAddressStatus_publicIp,

    -- ** NatGateway
    natGateway_connectivityType,
    natGateway_createTime,
    natGateway_deleteTime,
    natGateway_failureCode,
    natGateway_failureMessage,
    natGateway_natGatewayAddresses,
    natGateway_natGatewayId,
    natGateway_provisionedBandwidth,
    natGateway_state,
    natGateway_subnetId,
    natGateway_tags,
    natGateway_vpcId,

    -- ** NatGatewayAddress
    natGatewayAddress_allocationId,
    natGatewayAddress_associationId,
    natGatewayAddress_failureMessage,
    natGatewayAddress_isPrimary,
    natGatewayAddress_networkInterfaceId,
    natGatewayAddress_privateIp,
    natGatewayAddress_publicIp,
    natGatewayAddress_status,

    -- ** NetworkAcl
    networkAcl_associations,
    networkAcl_entries,
    networkAcl_isDefault,
    networkAcl_networkAclId,
    networkAcl_ownerId,
    networkAcl_tags,
    networkAcl_vpcId,

    -- ** NetworkAclAssociation
    networkAclAssociation_networkAclAssociationId,
    networkAclAssociation_networkAclId,
    networkAclAssociation_subnetId,

    -- ** NetworkAclEntry
    networkAclEntry_cidrBlock,
    networkAclEntry_egress,
    networkAclEntry_icmpTypeCode,
    networkAclEntry_ipv6CidrBlock,
    networkAclEntry_portRange,
    networkAclEntry_protocol,
    networkAclEntry_ruleAction,
    networkAclEntry_ruleNumber,

    -- ** NetworkBandwidthGbps
    networkBandwidthGbps_max,
    networkBandwidthGbps_min,

    -- ** NetworkBandwidthGbpsRequest
    networkBandwidthGbpsRequest_max,
    networkBandwidthGbpsRequest_min,

    -- ** NetworkCardInfo
    networkCardInfo_maximumNetworkInterfaces,
    networkCardInfo_networkCardIndex,
    networkCardInfo_networkPerformance,

    -- ** NetworkInfo
    networkInfo_defaultNetworkCardIndex,
    networkInfo_efaInfo,
    networkInfo_efaSupported,
    networkInfo_enaSrdSupported,
    networkInfo_enaSupport,
    networkInfo_encryptionInTransitSupported,
    networkInfo_ipv4AddressesPerInterface,
    networkInfo_ipv6AddressesPerInterface,
    networkInfo_ipv6Supported,
    networkInfo_maximumNetworkCards,
    networkInfo_maximumNetworkInterfaces,
    networkInfo_networkCards,
    networkInfo_networkPerformance,

    -- ** NetworkInsightsAccessScope
    networkInsightsAccessScope_createdDate,
    networkInsightsAccessScope_networkInsightsAccessScopeArn,
    networkInsightsAccessScope_networkInsightsAccessScopeId,
    networkInsightsAccessScope_tags,
    networkInsightsAccessScope_updatedDate,

    -- ** NetworkInsightsAccessScopeAnalysis
    networkInsightsAccessScopeAnalysis_analyzedEniCount,
    networkInsightsAccessScopeAnalysis_endDate,
    networkInsightsAccessScopeAnalysis_findingsFound,
    networkInsightsAccessScopeAnalysis_networkInsightsAccessScopeAnalysisArn,
    networkInsightsAccessScopeAnalysis_networkInsightsAccessScopeAnalysisId,
    networkInsightsAccessScopeAnalysis_networkInsightsAccessScopeId,
    networkInsightsAccessScopeAnalysis_startDate,
    networkInsightsAccessScopeAnalysis_status,
    networkInsightsAccessScopeAnalysis_statusMessage,
    networkInsightsAccessScopeAnalysis_tags,
    networkInsightsAccessScopeAnalysis_warningMessage,

    -- ** NetworkInsightsAccessScopeContent
    networkInsightsAccessScopeContent_excludePaths,
    networkInsightsAccessScopeContent_matchPaths,
    networkInsightsAccessScopeContent_networkInsightsAccessScopeId,

    -- ** NetworkInsightsAnalysis
    networkInsightsAnalysis_additionalAccounts,
    networkInsightsAnalysis_alternatePathHints,
    networkInsightsAnalysis_explanations,
    networkInsightsAnalysis_filterInArns,
    networkInsightsAnalysis_forwardPathComponents,
    networkInsightsAnalysis_networkInsightsAnalysisArn,
    networkInsightsAnalysis_networkInsightsAnalysisId,
    networkInsightsAnalysis_networkInsightsPathId,
    networkInsightsAnalysis_networkPathFound,
    networkInsightsAnalysis_returnPathComponents,
    networkInsightsAnalysis_startDate,
    networkInsightsAnalysis_status,
    networkInsightsAnalysis_statusMessage,
    networkInsightsAnalysis_suggestedAccounts,
    networkInsightsAnalysis_tags,
    networkInsightsAnalysis_warningMessage,

    -- ** NetworkInsightsPath
    networkInsightsPath_createdDate,
    networkInsightsPath_destination,
    networkInsightsPath_destinationArn,
    networkInsightsPath_destinationIp,
    networkInsightsPath_destinationPort,
    networkInsightsPath_filterAtDestination,
    networkInsightsPath_filterAtSource,
    networkInsightsPath_networkInsightsPathArn,
    networkInsightsPath_networkInsightsPathId,
    networkInsightsPath_protocol,
    networkInsightsPath_source,
    networkInsightsPath_sourceArn,
    networkInsightsPath_sourceIp,
    networkInsightsPath_tags,

    -- ** NetworkInterface
    networkInterface_association,
    networkInterface_attachment,
    networkInterface_availabilityZone,
    networkInterface_denyAllIgwTraffic,
    networkInterface_description,
    networkInterface_groups,
    networkInterface_interfaceType,
    networkInterface_ipv4Prefixes,
    networkInterface_ipv6Address,
    networkInterface_ipv6Addresses,
    networkInterface_ipv6Native,
    networkInterface_ipv6Prefixes,
    networkInterface_macAddress,
    networkInterface_networkInterfaceId,
    networkInterface_outpostArn,
    networkInterface_ownerId,
    networkInterface_privateDnsName,
    networkInterface_privateIpAddress,
    networkInterface_privateIpAddresses,
    networkInterface_requesterId,
    networkInterface_requesterManaged,
    networkInterface_sourceDestCheck,
    networkInterface_status,
    networkInterface_subnetId,
    networkInterface_tagSet,
    networkInterface_vpcId,

    -- ** NetworkInterfaceAssociation
    networkInterfaceAssociation_allocationId,
    networkInterfaceAssociation_associationId,
    networkInterfaceAssociation_carrierIp,
    networkInterfaceAssociation_customerOwnedIp,
    networkInterfaceAssociation_ipOwnerId,
    networkInterfaceAssociation_publicDnsName,
    networkInterfaceAssociation_publicIp,

    -- ** NetworkInterfaceAttachment
    networkInterfaceAttachment_attachTime,
    networkInterfaceAttachment_attachmentId,
    networkInterfaceAttachment_deleteOnTermination,
    networkInterfaceAttachment_deviceIndex,
    networkInterfaceAttachment_enaSrdSpecification,
    networkInterfaceAttachment_instanceId,
    networkInterfaceAttachment_instanceOwnerId,
    networkInterfaceAttachment_networkCardIndex,
    networkInterfaceAttachment_status,

    -- ** NetworkInterfaceAttachmentChanges
    networkInterfaceAttachmentChanges_attachmentId,
    networkInterfaceAttachmentChanges_deleteOnTermination,

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
    networkInterfacePermission_awsService,
    networkInterfacePermission_networkInterfaceId,
    networkInterfacePermission_networkInterfacePermissionId,
    networkInterfacePermission_permission,
    networkInterfacePermission_permissionState,

    -- ** NetworkInterfacePermissionState
    networkInterfacePermissionState_state,
    networkInterfacePermissionState_statusMessage,

    -- ** NetworkInterfacePrivateIpAddress
    networkInterfacePrivateIpAddress_association,
    networkInterfacePrivateIpAddress_primary,
    networkInterfacePrivateIpAddress_privateDnsName,
    networkInterfacePrivateIpAddress_privateIpAddress,

    -- ** NewDhcpConfiguration
    newDhcpConfiguration_key,
    newDhcpConfiguration_values,

    -- ** OidcOptions
    oidcOptions_authorizationEndpoint,
    oidcOptions_clientId,
    oidcOptions_clientSecret,
    oidcOptions_issuer,
    oidcOptions_scope,
    oidcOptions_tokenEndpoint,
    oidcOptions_userInfoEndpoint,

    -- ** OnDemandOptions
    onDemandOptions_allocationStrategy,
    onDemandOptions_capacityReservationOptions,
    onDemandOptions_maxTotalPrice,
    onDemandOptions_minTargetCapacity,
    onDemandOptions_singleAvailabilityZone,
    onDemandOptions_singleInstanceType,

    -- ** OnDemandOptionsRequest
    onDemandOptionsRequest_allocationStrategy,
    onDemandOptionsRequest_capacityReservationOptions,
    onDemandOptionsRequest_maxTotalPrice,
    onDemandOptionsRequest_minTargetCapacity,
    onDemandOptionsRequest_singleAvailabilityZone,
    onDemandOptionsRequest_singleInstanceType,

    -- ** PacketHeaderStatement
    packetHeaderStatement_destinationAddresses,
    packetHeaderStatement_destinationPorts,
    packetHeaderStatement_destinationPrefixLists,
    packetHeaderStatement_protocols,
    packetHeaderStatement_sourceAddresses,
    packetHeaderStatement_sourcePorts,
    packetHeaderStatement_sourcePrefixLists,

    -- ** PacketHeaderStatementRequest
    packetHeaderStatementRequest_destinationAddresses,
    packetHeaderStatementRequest_destinationPorts,
    packetHeaderStatementRequest_destinationPrefixLists,
    packetHeaderStatementRequest_protocols,
    packetHeaderStatementRequest_sourceAddresses,
    packetHeaderStatementRequest_sourcePorts,
    packetHeaderStatementRequest_sourcePrefixLists,

    -- ** PathComponent
    pathComponent_aclRule,
    pathComponent_additionalDetails,
    pathComponent_attachedTo,
    pathComponent_component,
    pathComponent_destinationVpc,
    pathComponent_elasticLoadBalancerListener,
    pathComponent_explanations,
    pathComponent_firewallStatefulRule,
    pathComponent_firewallStatelessRule,
    pathComponent_inboundHeader,
    pathComponent_outboundHeader,
    pathComponent_routeTableRoute,
    pathComponent_securityGroupRule,
    pathComponent_sequenceNumber,
    pathComponent_serviceName,
    pathComponent_sourceVpc,
    pathComponent_subnet,
    pathComponent_transitGateway,
    pathComponent_transitGatewayRouteTableRoute,
    pathComponent_vpc,

    -- ** PathFilter
    pathFilter_destinationAddress,
    pathFilter_destinationPortRange,
    pathFilter_sourceAddress,
    pathFilter_sourcePortRange,

    -- ** PathRequestFilter
    pathRequestFilter_destinationAddress,
    pathRequestFilter_destinationPortRange,
    pathRequestFilter_sourceAddress,
    pathRequestFilter_sourcePortRange,

    -- ** PathStatement
    pathStatement_packetHeaderStatement,
    pathStatement_resourceStatement,

    -- ** PathStatementRequest
    pathStatementRequest_packetHeaderStatement,
    pathStatementRequest_resourceStatement,

    -- ** PciId
    pciId_deviceId,
    pciId_subsystemId,
    pciId_subsystemVendorId,
    pciId_vendorId,

    -- ** PeeringAttachmentStatus
    peeringAttachmentStatus_code,
    peeringAttachmentStatus_message,

    -- ** PeeringConnectionOptions
    peeringConnectionOptions_allowDnsResolutionFromRemoteVpc,
    peeringConnectionOptions_allowEgressFromLocalClassicLinkToRemoteVpc,
    peeringConnectionOptions_allowEgressFromLocalVpcToRemoteClassicLink,

    -- ** PeeringConnectionOptionsRequest
    peeringConnectionOptionsRequest_allowDnsResolutionFromRemoteVpc,
    peeringConnectionOptionsRequest_allowEgressFromLocalClassicLinkToRemoteVpc,
    peeringConnectionOptionsRequest_allowEgressFromLocalVpcToRemoteClassicLink,

    -- ** PeeringTgwInfo
    peeringTgwInfo_coreNetworkId,
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
    placement_affinity,
    placement_availabilityZone,
    placement_groupId,
    placement_groupName,
    placement_hostId,
    placement_hostResourceGroupArn,
    placement_partitionNumber,
    placement_spreadDomain,
    placement_tenancy,

    -- ** PlacementGroup
    placementGroup_groupArn,
    placementGroup_groupId,
    placementGroup_groupName,
    placementGroup_partitionCount,
    placementGroup_spreadLevel,
    placementGroup_state,
    placementGroup_strategy,
    placementGroup_tags,

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
    prefixListId_description,
    prefixListId_prefixListId,

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
    privateDnsNameConfiguration_state,
    privateDnsNameConfiguration_type,
    privateDnsNameConfiguration_value,

    -- ** PrivateDnsNameOptionsOnLaunch
    privateDnsNameOptionsOnLaunch_enableResourceNameDnsAAAARecord,
    privateDnsNameOptionsOnLaunch_enableResourceNameDnsARecord,
    privateDnsNameOptionsOnLaunch_hostnameType,

    -- ** PrivateDnsNameOptionsRequest
    privateDnsNameOptionsRequest_enableResourceNameDnsAAAARecord,
    privateDnsNameOptionsRequest_enableResourceNameDnsARecord,
    privateDnsNameOptionsRequest_hostnameType,

    -- ** PrivateDnsNameOptionsResponse
    privateDnsNameOptionsResponse_enableResourceNameDnsAAAARecord,
    privateDnsNameOptionsResponse_enableResourceNameDnsARecord,
    privateDnsNameOptionsResponse_hostnameType,

    -- ** PrivateIpAddressSpecification
    privateIpAddressSpecification_primary,
    privateIpAddressSpecification_privateIpAddress,

    -- ** ProcessorInfo
    processorInfo_supportedArchitectures,
    processorInfo_supportedFeatures,
    processorInfo_sustainedClockSpeedInGhz,

    -- ** ProductCode
    productCode_productCodeId,
    productCode_productCodeType,

    -- ** PropagatingVgw
    propagatingVgw_gatewayId,

    -- ** ProvisionedBandwidth
    provisionedBandwidth_provisionTime,
    provisionedBandwidth_provisioned,
    provisionedBandwidth_requestTime,
    provisionedBandwidth_requested,
    provisionedBandwidth_status,

    -- ** PtrUpdateStatus
    ptrUpdateStatus_reason,
    ptrUpdateStatus_status,
    ptrUpdateStatus_value,

    -- ** PublicIpv4Pool
    publicIpv4Pool_description,
    publicIpv4Pool_networkBorderGroup,
    publicIpv4Pool_poolAddressRanges,
    publicIpv4Pool_poolId,
    publicIpv4Pool_tags,
    publicIpv4Pool_totalAddressCount,
    publicIpv4Pool_totalAvailableAddressCount,

    -- ** PublicIpv4PoolRange
    publicIpv4PoolRange_addressCount,
    publicIpv4PoolRange_availableAddressCount,
    publicIpv4PoolRange_firstAddress,
    publicIpv4PoolRange_lastAddress,

    -- ** Purchase
    purchase_currencyCode,
    purchase_duration,
    purchase_hostIdSet,
    purchase_hostReservationId,
    purchase_hourlyPrice,
    purchase_instanceFamily,
    purchase_paymentOption,
    purchase_upfrontPrice,

    -- ** PurchaseRequest
    purchaseRequest_instanceCount,
    purchaseRequest_purchaseToken,

    -- ** RecurringCharge
    recurringCharge_amount,
    recurringCharge_frequency,

    -- ** ReferencedSecurityGroup
    referencedSecurityGroup_groupId,
    referencedSecurityGroup_peeringStatus,
    referencedSecurityGroup_userId,
    referencedSecurityGroup_vpcId,
    referencedSecurityGroup_vpcPeeringConnectionId,

    -- ** RegionInfo
    regionInfo_endpoint,
    regionInfo_optInStatus,
    regionInfo_regionName,

    -- ** RegisterInstanceTagAttributeRequest
    registerInstanceTagAttributeRequest_includeAllTagsOfInstance,
    registerInstanceTagAttributeRequest_instanceTagKeys,

    -- ** RemoveIpamOperatingRegion
    removeIpamOperatingRegion_regionName,

    -- ** RemovePrefixListEntry
    removePrefixListEntry_cidr,

    -- ** ReplaceRootVolumeTask
    replaceRootVolumeTask_completeTime,
    replaceRootVolumeTask_deleteReplacedRootVolume,
    replaceRootVolumeTask_imageId,
    replaceRootVolumeTask_instanceId,
    replaceRootVolumeTask_replaceRootVolumeTaskId,
    replaceRootVolumeTask_snapshotId,
    replaceRootVolumeTask_startTime,
    replaceRootVolumeTask_tags,
    replaceRootVolumeTask_taskState,

    -- ** RequestFilterPortRange
    requestFilterPortRange_fromPort,
    requestFilterPortRange_toPort,

    -- ** RequestIpamResourceTag
    requestIpamResourceTag_key,
    requestIpamResourceTag_value,

    -- ** RequestLaunchTemplateData
    requestLaunchTemplateData_blockDeviceMappings,
    requestLaunchTemplateData_capacityReservationSpecification,
    requestLaunchTemplateData_cpuOptions,
    requestLaunchTemplateData_creditSpecification,
    requestLaunchTemplateData_disableApiStop,
    requestLaunchTemplateData_disableApiTermination,
    requestLaunchTemplateData_ebsOptimized,
    requestLaunchTemplateData_elasticGpuSpecifications,
    requestLaunchTemplateData_elasticInferenceAccelerators,
    requestLaunchTemplateData_enclaveOptions,
    requestLaunchTemplateData_hibernationOptions,
    requestLaunchTemplateData_iamInstanceProfile,
    requestLaunchTemplateData_imageId,
    requestLaunchTemplateData_instanceInitiatedShutdownBehavior,
    requestLaunchTemplateData_instanceMarketOptions,
    requestLaunchTemplateData_instanceRequirements,
    requestLaunchTemplateData_instanceType,
    requestLaunchTemplateData_kernelId,
    requestLaunchTemplateData_keyName,
    requestLaunchTemplateData_licenseSpecifications,
    requestLaunchTemplateData_maintenanceOptions,
    requestLaunchTemplateData_metadataOptions,
    requestLaunchTemplateData_monitoring,
    requestLaunchTemplateData_networkInterfaces,
    requestLaunchTemplateData_placement,
    requestLaunchTemplateData_privateDnsNameOptions,
    requestLaunchTemplateData_ramDiskId,
    requestLaunchTemplateData_securityGroupIds,
    requestLaunchTemplateData_securityGroups,
    requestLaunchTemplateData_tagSpecifications,
    requestLaunchTemplateData_userData,

    -- ** RequestSpotLaunchSpecification
    requestSpotLaunchSpecification_addressingType,
    requestSpotLaunchSpecification_blockDeviceMappings,
    requestSpotLaunchSpecification_ebsOptimized,
    requestSpotLaunchSpecification_iamInstanceProfile,
    requestSpotLaunchSpecification_imageId,
    requestSpotLaunchSpecification_instanceType,
    requestSpotLaunchSpecification_kernelId,
    requestSpotLaunchSpecification_keyName,
    requestSpotLaunchSpecification_monitoring,
    requestSpotLaunchSpecification_networkInterfaces,
    requestSpotLaunchSpecification_placement,
    requestSpotLaunchSpecification_ramdiskId,
    requestSpotLaunchSpecification_securityGroupIds,
    requestSpotLaunchSpecification_securityGroups,
    requestSpotLaunchSpecification_subnetId,
    requestSpotLaunchSpecification_userData,

    -- ** Reservation
    reservation_groups,
    reservation_instances,
    reservation_requesterId,
    reservation_reservationId,
    reservation_ownerId,

    -- ** ReservationFleetInstanceSpecification
    reservationFleetInstanceSpecification_availabilityZone,
    reservationFleetInstanceSpecification_availabilityZoneId,
    reservationFleetInstanceSpecification_ebsOptimized,
    reservationFleetInstanceSpecification_instancePlatform,
    reservationFleetInstanceSpecification_instanceType,
    reservationFleetInstanceSpecification_priority,
    reservationFleetInstanceSpecification_weight,

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
    reservedInstances_availabilityZone,
    reservedInstances_currencyCode,
    reservedInstances_duration,
    reservedInstances_end,
    reservedInstances_fixedPrice,
    reservedInstances_instanceCount,
    reservedInstances_instanceTenancy,
    reservedInstances_instanceType,
    reservedInstances_offeringClass,
    reservedInstances_offeringType,
    reservedInstances_productDescription,
    reservedInstances_recurringCharges,
    reservedInstances_reservedInstancesId,
    reservedInstances_scope,
    reservedInstances_start,
    reservedInstances_state,
    reservedInstances_tags,
    reservedInstances_usagePrice,

    -- ** ReservedInstancesConfiguration
    reservedInstancesConfiguration_availabilityZone,
    reservedInstancesConfiguration_instanceCount,
    reservedInstancesConfiguration_instanceType,
    reservedInstancesConfiguration_platform,
    reservedInstancesConfiguration_scope,

    -- ** ReservedInstancesId
    reservedInstancesId_reservedInstancesId,

    -- ** ReservedInstancesListing
    reservedInstancesListing_clientToken,
    reservedInstancesListing_createDate,
    reservedInstancesListing_instanceCounts,
    reservedInstancesListing_priceSchedules,
    reservedInstancesListing_reservedInstancesId,
    reservedInstancesListing_reservedInstancesListingId,
    reservedInstancesListing_status,
    reservedInstancesListing_statusMessage,
    reservedInstancesListing_tags,
    reservedInstancesListing_updateDate,

    -- ** ReservedInstancesModification
    reservedInstancesModification_clientToken,
    reservedInstancesModification_createDate,
    reservedInstancesModification_effectiveDate,
    reservedInstancesModification_modificationResults,
    reservedInstancesModification_reservedInstancesIds,
    reservedInstancesModification_reservedInstancesModificationId,
    reservedInstancesModification_status,
    reservedInstancesModification_statusMessage,
    reservedInstancesModification_updateDate,

    -- ** ReservedInstancesModificationResult
    reservedInstancesModificationResult_reservedInstancesId,
    reservedInstancesModificationResult_targetConfiguration,

    -- ** ReservedInstancesOffering
    reservedInstancesOffering_availabilityZone,
    reservedInstancesOffering_currencyCode,
    reservedInstancesOffering_duration,
    reservedInstancesOffering_fixedPrice,
    reservedInstancesOffering_instanceTenancy,
    reservedInstancesOffering_instanceType,
    reservedInstancesOffering_marketplace,
    reservedInstancesOffering_offeringClass,
    reservedInstancesOffering_offeringType,
    reservedInstancesOffering_pricingDetails,
    reservedInstancesOffering_productDescription,
    reservedInstancesOffering_recurringCharges,
    reservedInstancesOffering_reservedInstancesOfferingId,
    reservedInstancesOffering_scope,
    reservedInstancesOffering_usagePrice,

    -- ** ResourceStatement
    resourceStatement_resourceTypes,
    resourceStatement_resources,

    -- ** ResourceStatementRequest
    resourceStatementRequest_resourceTypes,
    resourceStatementRequest_resources,

    -- ** ResponseError
    responseError_code,
    responseError_message,

    -- ** ResponseLaunchTemplateData
    responseLaunchTemplateData_blockDeviceMappings,
    responseLaunchTemplateData_capacityReservationSpecification,
    responseLaunchTemplateData_cpuOptions,
    responseLaunchTemplateData_creditSpecification,
    responseLaunchTemplateData_disableApiStop,
    responseLaunchTemplateData_disableApiTermination,
    responseLaunchTemplateData_ebsOptimized,
    responseLaunchTemplateData_elasticGpuSpecifications,
    responseLaunchTemplateData_elasticInferenceAccelerators,
    responseLaunchTemplateData_enclaveOptions,
    responseLaunchTemplateData_hibernationOptions,
    responseLaunchTemplateData_iamInstanceProfile,
    responseLaunchTemplateData_imageId,
    responseLaunchTemplateData_instanceInitiatedShutdownBehavior,
    responseLaunchTemplateData_instanceMarketOptions,
    responseLaunchTemplateData_instanceRequirements,
    responseLaunchTemplateData_instanceType,
    responseLaunchTemplateData_kernelId,
    responseLaunchTemplateData_keyName,
    responseLaunchTemplateData_licenseSpecifications,
    responseLaunchTemplateData_maintenanceOptions,
    responseLaunchTemplateData_metadataOptions,
    responseLaunchTemplateData_monitoring,
    responseLaunchTemplateData_networkInterfaces,
    responseLaunchTemplateData_placement,
    responseLaunchTemplateData_privateDnsNameOptions,
    responseLaunchTemplateData_ramDiskId,
    responseLaunchTemplateData_securityGroupIds,
    responseLaunchTemplateData_securityGroups,
    responseLaunchTemplateData_tagSpecifications,
    responseLaunchTemplateData_userData,

    -- ** Route
    route_carrierGatewayId,
    route_coreNetworkArn,
    route_destinationCidrBlock,
    route_destinationIpv6CidrBlock,
    route_destinationPrefixListId,
    route_egressOnlyInternetGatewayId,
    route_gatewayId,
    route_instanceId,
    route_instanceOwnerId,
    route_localGatewayId,
    route_natGatewayId,
    route_networkInterfaceId,
    route_origin,
    route_state,
    route_transitGatewayId,
    route_vpcPeeringConnectionId,

    -- ** RouteTable
    routeTable_associations,
    routeTable_ownerId,
    routeTable_propagatingVgws,
    routeTable_routeTableId,
    routeTable_routes,
    routeTable_tags,
    routeTable_vpcId,

    -- ** RouteTableAssociation
    routeTableAssociation_associationState,
    routeTableAssociation_gatewayId,
    routeTableAssociation_main,
    routeTableAssociation_routeTableAssociationId,
    routeTableAssociation_routeTableId,
    routeTableAssociation_subnetId,

    -- ** RouteTableAssociationState
    routeTableAssociationState_state,
    routeTableAssociationState_statusMessage,

    -- ** RuleGroupRuleOptionsPair
    ruleGroupRuleOptionsPair_ruleGroupArn,
    ruleGroupRuleOptionsPair_ruleOptions,

    -- ** RuleGroupTypePair
    ruleGroupTypePair_ruleGroupArn,
    ruleGroupTypePair_ruleGroupType,

    -- ** RuleOption
    ruleOption_keyword,
    ruleOption_settings,

    -- ** RunInstancesMonitoringEnabled
    runInstancesMonitoringEnabled_enabled,

    -- ** S3ObjectTag
    s3ObjectTag_key,
    s3ObjectTag_value,

    -- ** S3Storage
    s3Storage_aWSAccessKeyId,
    s3Storage_bucket,
    s3Storage_prefix,
    s3Storage_uploadPolicy,
    s3Storage_uploadPolicySignature,

    -- ** ScheduledInstance
    scheduledInstance_availabilityZone,
    scheduledInstance_createDate,
    scheduledInstance_hourlyPrice,
    scheduledInstance_instanceCount,
    scheduledInstance_instanceType,
    scheduledInstance_networkPlatform,
    scheduledInstance_nextSlotStartTime,
    scheduledInstance_platform,
    scheduledInstance_previousSlotEndTime,
    scheduledInstance_recurrence,
    scheduledInstance_scheduledInstanceId,
    scheduledInstance_slotDurationInHours,
    scheduledInstance_termEndDate,
    scheduledInstance_termStartDate,
    scheduledInstance_totalScheduledInstanceHours,

    -- ** ScheduledInstanceAvailability
    scheduledInstanceAvailability_availabilityZone,
    scheduledInstanceAvailability_availableInstanceCount,
    scheduledInstanceAvailability_firstSlotStartTime,
    scheduledInstanceAvailability_hourlyPrice,
    scheduledInstanceAvailability_instanceType,
    scheduledInstanceAvailability_maxTermDurationInDays,
    scheduledInstanceAvailability_minTermDurationInDays,
    scheduledInstanceAvailability_networkPlatform,
    scheduledInstanceAvailability_platform,
    scheduledInstanceAvailability_purchaseToken,
    scheduledInstanceAvailability_recurrence,
    scheduledInstanceAvailability_slotDurationInHours,
    scheduledInstanceAvailability_totalScheduledInstanceHours,

    -- ** ScheduledInstanceRecurrence
    scheduledInstanceRecurrence_frequency,
    scheduledInstanceRecurrence_interval,
    scheduledInstanceRecurrence_occurrenceDaySet,
    scheduledInstanceRecurrence_occurrenceRelativeToEnd,
    scheduledInstanceRecurrence_occurrenceUnit,

    -- ** ScheduledInstanceRecurrenceRequest
    scheduledInstanceRecurrenceRequest_frequency,
    scheduledInstanceRecurrenceRequest_interval,
    scheduledInstanceRecurrenceRequest_occurrenceDays,
    scheduledInstanceRecurrenceRequest_occurrenceRelativeToEnd,
    scheduledInstanceRecurrenceRequest_occurrenceUnit,

    -- ** ScheduledInstancesBlockDeviceMapping
    scheduledInstancesBlockDeviceMapping_deviceName,
    scheduledInstancesBlockDeviceMapping_ebs,
    scheduledInstancesBlockDeviceMapping_noDevice,
    scheduledInstancesBlockDeviceMapping_virtualName,

    -- ** ScheduledInstancesEbs
    scheduledInstancesEbs_deleteOnTermination,
    scheduledInstancesEbs_encrypted,
    scheduledInstancesEbs_iops,
    scheduledInstancesEbs_snapshotId,
    scheduledInstancesEbs_volumeSize,
    scheduledInstancesEbs_volumeType,

    -- ** ScheduledInstancesIamInstanceProfile
    scheduledInstancesIamInstanceProfile_arn,
    scheduledInstancesIamInstanceProfile_name,

    -- ** ScheduledInstancesIpv6Address
    scheduledInstancesIpv6Address_ipv6Address,

    -- ** ScheduledInstancesLaunchSpecification
    scheduledInstancesLaunchSpecification_blockDeviceMappings,
    scheduledInstancesLaunchSpecification_ebsOptimized,
    scheduledInstancesLaunchSpecification_iamInstanceProfile,
    scheduledInstancesLaunchSpecification_instanceType,
    scheduledInstancesLaunchSpecification_kernelId,
    scheduledInstancesLaunchSpecification_keyName,
    scheduledInstancesLaunchSpecification_monitoring,
    scheduledInstancesLaunchSpecification_networkInterfaces,
    scheduledInstancesLaunchSpecification_placement,
    scheduledInstancesLaunchSpecification_ramdiskId,
    scheduledInstancesLaunchSpecification_securityGroupIds,
    scheduledInstancesLaunchSpecification_subnetId,
    scheduledInstancesLaunchSpecification_userData,
    scheduledInstancesLaunchSpecification_imageId,

    -- ** ScheduledInstancesMonitoring
    scheduledInstancesMonitoring_enabled,

    -- ** ScheduledInstancesNetworkInterface
    scheduledInstancesNetworkInterface_associatePublicIpAddress,
    scheduledInstancesNetworkInterface_deleteOnTermination,
    scheduledInstancesNetworkInterface_description,
    scheduledInstancesNetworkInterface_deviceIndex,
    scheduledInstancesNetworkInterface_groups,
    scheduledInstancesNetworkInterface_ipv6AddressCount,
    scheduledInstancesNetworkInterface_ipv6Addresses,
    scheduledInstancesNetworkInterface_networkInterfaceId,
    scheduledInstancesNetworkInterface_privateIpAddress,
    scheduledInstancesNetworkInterface_privateIpAddressConfigs,
    scheduledInstancesNetworkInterface_secondaryPrivateIpAddressCount,
    scheduledInstancesNetworkInterface_subnetId,

    -- ** ScheduledInstancesPlacement
    scheduledInstancesPlacement_availabilityZone,
    scheduledInstancesPlacement_groupName,

    -- ** ScheduledInstancesPrivateIpAddressConfig
    scheduledInstancesPrivateIpAddressConfig_primary,
    scheduledInstancesPrivateIpAddressConfig_privateIpAddress,

    -- ** SecurityGroup
    securityGroup_ipPermissions,
    securityGroup_ipPermissionsEgress,
    securityGroup_tags,
    securityGroup_vpcId,
    securityGroup_ownerId,
    securityGroup_groupId,
    securityGroup_groupName,
    securityGroup_description,

    -- ** SecurityGroupIdentifier
    securityGroupIdentifier_groupId,
    securityGroupIdentifier_groupName,

    -- ** SecurityGroupReference
    securityGroupReference_groupId,
    securityGroupReference_referencingVpcId,
    securityGroupReference_vpcPeeringConnectionId,

    -- ** SecurityGroupRule
    securityGroupRule_cidrIpv4,
    securityGroupRule_cidrIpv6,
    securityGroupRule_description,
    securityGroupRule_fromPort,
    securityGroupRule_groupId,
    securityGroupRule_groupOwnerId,
    securityGroupRule_ipProtocol,
    securityGroupRule_isEgress,
    securityGroupRule_prefixListId,
    securityGroupRule_referencedGroupInfo,
    securityGroupRule_securityGroupRuleId,
    securityGroupRule_tags,
    securityGroupRule_toPort,

    -- ** SecurityGroupRuleDescription
    securityGroupRuleDescription_description,
    securityGroupRuleDescription_securityGroupRuleId,

    -- ** SecurityGroupRuleRequest
    securityGroupRuleRequest_cidrIpv4,
    securityGroupRuleRequest_cidrIpv6,
    securityGroupRuleRequest_description,
    securityGroupRuleRequest_fromPort,
    securityGroupRuleRequest_ipProtocol,
    securityGroupRuleRequest_prefixListId,
    securityGroupRuleRequest_referencedGroupId,
    securityGroupRuleRequest_toPort,

    -- ** SecurityGroupRuleUpdate
    securityGroupRuleUpdate_securityGroupRule,
    securityGroupRuleUpdate_securityGroupRuleId,

    -- ** ServiceConfiguration
    serviceConfiguration_acceptanceRequired,
    serviceConfiguration_availabilityZones,
    serviceConfiguration_baseEndpointDnsNames,
    serviceConfiguration_gatewayLoadBalancerArns,
    serviceConfiguration_managesVpcEndpoints,
    serviceConfiguration_networkLoadBalancerArns,
    serviceConfiguration_payerResponsibility,
    serviceConfiguration_privateDnsName,
    serviceConfiguration_privateDnsNameConfiguration,
    serviceConfiguration_serviceId,
    serviceConfiguration_serviceName,
    serviceConfiguration_serviceState,
    serviceConfiguration_serviceType,
    serviceConfiguration_supportedIpAddressTypes,
    serviceConfiguration_tags,

    -- ** ServiceDetail
    serviceDetail_acceptanceRequired,
    serviceDetail_availabilityZones,
    serviceDetail_baseEndpointDnsNames,
    serviceDetail_managesVpcEndpoints,
    serviceDetail_owner,
    serviceDetail_payerResponsibility,
    serviceDetail_privateDnsName,
    serviceDetail_privateDnsNameVerificationState,
    serviceDetail_privateDnsNames,
    serviceDetail_serviceId,
    serviceDetail_serviceName,
    serviceDetail_serviceType,
    serviceDetail_supportedIpAddressTypes,
    serviceDetail_tags,
    serviceDetail_vpcEndpointPolicySupported,

    -- ** ServiceTypeDetail
    serviceTypeDetail_serviceType,

    -- ** SlotDateTimeRangeRequest
    slotDateTimeRangeRequest_earliestTime,
    slotDateTimeRangeRequest_latestTime,

    -- ** SlotStartTimeRangeRequest
    slotStartTimeRangeRequest_earliestTime,
    slotStartTimeRangeRequest_latestTime,

    -- ** Snapshot
    snapshot_dataEncryptionKeyId,
    snapshot_kmsKeyId,
    snapshot_outpostArn,
    snapshot_ownerAlias,
    snapshot_restoreExpiryTime,
    snapshot_stateMessage,
    snapshot_storageTier,
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
    snapshotDetail_description,
    snapshotDetail_deviceName,
    snapshotDetail_diskImageSize,
    snapshotDetail_format,
    snapshotDetail_progress,
    snapshotDetail_snapshotId,
    snapshotDetail_status,
    snapshotDetail_statusMessage,
    snapshotDetail_url,
    snapshotDetail_userBucket,

    -- ** SnapshotDiskContainer
    snapshotDiskContainer_description,
    snapshotDiskContainer_format,
    snapshotDiskContainer_url,
    snapshotDiskContainer_userBucket,

    -- ** SnapshotInfo
    snapshotInfo_description,
    snapshotInfo_encrypted,
    snapshotInfo_outpostArn,
    snapshotInfo_ownerId,
    snapshotInfo_progress,
    snapshotInfo_snapshotId,
    snapshotInfo_startTime,
    snapshotInfo_state,
    snapshotInfo_tags,
    snapshotInfo_volumeId,
    snapshotInfo_volumeSize,

    -- ** SnapshotRecycleBinInfo
    snapshotRecycleBinInfo_description,
    snapshotRecycleBinInfo_recycleBinEnterTime,
    snapshotRecycleBinInfo_recycleBinExitTime,
    snapshotRecycleBinInfo_snapshotId,
    snapshotRecycleBinInfo_volumeId,

    -- ** SnapshotTaskDetail
    snapshotTaskDetail_description,
    snapshotTaskDetail_diskImageSize,
    snapshotTaskDetail_encrypted,
    snapshotTaskDetail_format,
    snapshotTaskDetail_kmsKeyId,
    snapshotTaskDetail_progress,
    snapshotTaskDetail_snapshotId,
    snapshotTaskDetail_status,
    snapshotTaskDetail_statusMessage,
    snapshotTaskDetail_url,
    snapshotTaskDetail_userBucket,

    -- ** SnapshotTierStatus
    snapshotTierStatus_archivalCompleteTime,
    snapshotTierStatus_lastTieringOperationStatus,
    snapshotTierStatus_lastTieringOperationStatusDetail,
    snapshotTierStatus_lastTieringProgress,
    snapshotTierStatus_lastTieringStartTime,
    snapshotTierStatus_ownerId,
    snapshotTierStatus_restoreExpiryTime,
    snapshotTierStatus_snapshotId,
    snapshotTierStatus_status,
    snapshotTierStatus_storageTier,
    snapshotTierStatus_tags,
    snapshotTierStatus_volumeId,

    -- ** SpotCapacityRebalance
    spotCapacityRebalance_replacementStrategy,
    spotCapacityRebalance_terminationDelay,

    -- ** SpotDatafeedSubscription
    spotDatafeedSubscription_bucket,
    spotDatafeedSubscription_fault,
    spotDatafeedSubscription_ownerId,
    spotDatafeedSubscription_prefix,
    spotDatafeedSubscription_state,

    -- ** SpotFleetLaunchSpecification
    spotFleetLaunchSpecification_addressingType,
    spotFleetLaunchSpecification_blockDeviceMappings,
    spotFleetLaunchSpecification_ebsOptimized,
    spotFleetLaunchSpecification_iamInstanceProfile,
    spotFleetLaunchSpecification_imageId,
    spotFleetLaunchSpecification_instanceRequirements,
    spotFleetLaunchSpecification_instanceType,
    spotFleetLaunchSpecification_kernelId,
    spotFleetLaunchSpecification_keyName,
    spotFleetLaunchSpecification_monitoring,
    spotFleetLaunchSpecification_networkInterfaces,
    spotFleetLaunchSpecification_placement,
    spotFleetLaunchSpecification_ramdiskId,
    spotFleetLaunchSpecification_securityGroups,
    spotFleetLaunchSpecification_spotPrice,
    spotFleetLaunchSpecification_subnetId,
    spotFleetLaunchSpecification_tagSpecifications,
    spotFleetLaunchSpecification_userData,
    spotFleetLaunchSpecification_weightedCapacity,

    -- ** SpotFleetMonitoring
    spotFleetMonitoring_enabled,

    -- ** SpotFleetRequestConfig
    spotFleetRequestConfig_activityStatus,
    spotFleetRequestConfig_createTime,
    spotFleetRequestConfig_spotFleetRequestConfig,
    spotFleetRequestConfig_spotFleetRequestId,
    spotFleetRequestConfig_spotFleetRequestState,
    spotFleetRequestConfig_tags,

    -- ** SpotFleetRequestConfigData
    spotFleetRequestConfigData_allocationStrategy,
    spotFleetRequestConfigData_clientToken,
    spotFleetRequestConfigData_context,
    spotFleetRequestConfigData_excessCapacityTerminationPolicy,
    spotFleetRequestConfigData_fulfilledCapacity,
    spotFleetRequestConfigData_instanceInterruptionBehavior,
    spotFleetRequestConfigData_instancePoolsToUseCount,
    spotFleetRequestConfigData_launchSpecifications,
    spotFleetRequestConfigData_launchTemplateConfigs,
    spotFleetRequestConfigData_loadBalancersConfig,
    spotFleetRequestConfigData_onDemandAllocationStrategy,
    spotFleetRequestConfigData_onDemandFulfilledCapacity,
    spotFleetRequestConfigData_onDemandMaxTotalPrice,
    spotFleetRequestConfigData_onDemandTargetCapacity,
    spotFleetRequestConfigData_replaceUnhealthyInstances,
    spotFleetRequestConfigData_spotMaintenanceStrategies,
    spotFleetRequestConfigData_spotMaxTotalPrice,
    spotFleetRequestConfigData_spotPrice,
    spotFleetRequestConfigData_tagSpecifications,
    spotFleetRequestConfigData_targetCapacityUnitType,
    spotFleetRequestConfigData_terminateInstancesWithExpiration,
    spotFleetRequestConfigData_type,
    spotFleetRequestConfigData_validFrom,
    spotFleetRequestConfigData_validUntil,
    spotFleetRequestConfigData_iamFleetRole,
    spotFleetRequestConfigData_targetCapacity,

    -- ** SpotFleetTagSpecification
    spotFleetTagSpecification_resourceType,
    spotFleetTagSpecification_tags,

    -- ** SpotInstanceRequest
    spotInstanceRequest_actualBlockHourlyPrice,
    spotInstanceRequest_availabilityZoneGroup,
    spotInstanceRequest_blockDurationMinutes,
    spotInstanceRequest_createTime,
    spotInstanceRequest_fault,
    spotInstanceRequest_instanceId,
    spotInstanceRequest_instanceInterruptionBehavior,
    spotInstanceRequest_launchGroup,
    spotInstanceRequest_launchSpecification,
    spotInstanceRequest_launchedAvailabilityZone,
    spotInstanceRequest_productDescription,
    spotInstanceRequest_spotInstanceRequestId,
    spotInstanceRequest_spotPrice,
    spotInstanceRequest_state,
    spotInstanceRequest_status,
    spotInstanceRequest_tags,
    spotInstanceRequest_type,
    spotInstanceRequest_validFrom,
    spotInstanceRequest_validUntil,

    -- ** SpotInstanceStateFault
    spotInstanceStateFault_code,
    spotInstanceStateFault_message,

    -- ** SpotInstanceStatus
    spotInstanceStatus_code,
    spotInstanceStatus_message,
    spotInstanceStatus_updateTime,

    -- ** SpotMaintenanceStrategies
    spotMaintenanceStrategies_capacityRebalance,

    -- ** SpotMarketOptions
    spotMarketOptions_blockDurationMinutes,
    spotMarketOptions_instanceInterruptionBehavior,
    spotMarketOptions_maxPrice,
    spotMarketOptions_spotInstanceType,
    spotMarketOptions_validUntil,

    -- ** SpotOptions
    spotOptions_allocationStrategy,
    spotOptions_instanceInterruptionBehavior,
    spotOptions_instancePoolsToUseCount,
    spotOptions_maintenanceStrategies,
    spotOptions_maxTotalPrice,
    spotOptions_minTargetCapacity,
    spotOptions_singleAvailabilityZone,
    spotOptions_singleInstanceType,

    -- ** SpotOptionsRequest
    spotOptionsRequest_allocationStrategy,
    spotOptionsRequest_instanceInterruptionBehavior,
    spotOptionsRequest_instancePoolsToUseCount,
    spotOptionsRequest_maintenanceStrategies,
    spotOptionsRequest_maxTotalPrice,
    spotOptionsRequest_minTargetCapacity,
    spotOptionsRequest_singleAvailabilityZone,
    spotOptionsRequest_singleInstanceType,

    -- ** SpotPlacement
    spotPlacement_availabilityZone,
    spotPlacement_groupName,
    spotPlacement_tenancy,

    -- ** SpotPlacementScore
    spotPlacementScore_availabilityZoneId,
    spotPlacementScore_region,
    spotPlacementScore_score,

    -- ** SpotPrice
    spotPrice_availabilityZone,
    spotPrice_instanceType,
    spotPrice_productDescription,
    spotPrice_spotPrice,
    spotPrice_timestamp,

    -- ** StaleIpPermission
    staleIpPermission_fromPort,
    staleIpPermission_ipProtocol,
    staleIpPermission_ipRanges,
    staleIpPermission_prefixListIds,
    staleIpPermission_toPort,
    staleIpPermission_userIdGroupPairs,

    -- ** StaleSecurityGroup
    staleSecurityGroup_description,
    staleSecurityGroup_groupId,
    staleSecurityGroup_groupName,
    staleSecurityGroup_staleIpPermissions,
    staleSecurityGroup_staleIpPermissionsEgress,
    staleSecurityGroup_vpcId,

    -- ** StateReason
    stateReason_code,
    stateReason_message,

    -- ** Storage
    storage_s3,

    -- ** StorageLocation
    storageLocation_bucket,
    storageLocation_key,

    -- ** StoreImageTaskResult
    storeImageTaskResult_amiId,
    storeImageTaskResult_bucket,
    storeImageTaskResult_progressPercentage,
    storeImageTaskResult_s3objectKey,
    storeImageTaskResult_storeTaskFailureReason,
    storeImageTaskResult_storeTaskState,
    storeImageTaskResult_taskStartTime,

    -- ** Subnet
    subnet_assignIpv6AddressOnCreation,
    subnet_availabilityZoneId,
    subnet_customerOwnedIpv4Pool,
    subnet_defaultForAz,
    subnet_enableDns64,
    subnet_enableLniAtDeviceIndex,
    subnet_ipv6CidrBlockAssociationSet,
    subnet_ipv6Native,
    subnet_mapCustomerOwnedIpOnLaunch,
    subnet_mapPublicIpOnLaunch,
    subnet_outpostArn,
    subnet_ownerId,
    subnet_privateDnsNameOptionsOnLaunch,
    subnet_subnetArn,
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
    subnetCidrReservation_cidr,
    subnetCidrReservation_description,
    subnetCidrReservation_ownerId,
    subnetCidrReservation_reservationType,
    subnetCidrReservation_subnetCidrReservationId,
    subnetCidrReservation_subnetId,
    subnetCidrReservation_tags,

    -- ** SubnetIpv6CidrBlockAssociation
    subnetIpv6CidrBlockAssociation_associationId,
    subnetIpv6CidrBlockAssociation_ipv6CidrBlock,
    subnetIpv6CidrBlockAssociation_ipv6CidrBlockState,

    -- ** Subscription
    subscription_destination,
    subscription_metric,
    subscription_period,
    subscription_source,
    subscription_statistic,

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
    targetCapacitySpecification_defaultTargetCapacityType,
    targetCapacitySpecification_onDemandTargetCapacity,
    targetCapacitySpecification_spotTargetCapacity,
    targetCapacitySpecification_targetCapacityUnitType,
    targetCapacitySpecification_totalTargetCapacity,

    -- ** TargetCapacitySpecificationRequest
    targetCapacitySpecificationRequest_defaultTargetCapacityType,
    targetCapacitySpecificationRequest_onDemandTargetCapacity,
    targetCapacitySpecificationRequest_spotTargetCapacity,
    targetCapacitySpecificationRequest_targetCapacityUnitType,
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
    targetNetwork_clientVpnEndpointId,
    targetNetwork_securityGroups,
    targetNetwork_status,
    targetNetwork_targetNetworkId,
    targetNetwork_vpcId,

    -- ** TargetReservationValue
    targetReservationValue_reservationValue,
    targetReservationValue_targetConfiguration,

    -- ** TerminateConnectionStatus
    terminateConnectionStatus_connectionId,
    terminateConnectionStatus_currentStatus,
    terminateConnectionStatus_previousStatus,

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
    trafficMirrorFilter_description,
    trafficMirrorFilter_egressFilterRules,
    trafficMirrorFilter_ingressFilterRules,
    trafficMirrorFilter_networkServices,
    trafficMirrorFilter_tags,
    trafficMirrorFilter_trafficMirrorFilterId,

    -- ** TrafficMirrorFilterRule
    trafficMirrorFilterRule_description,
    trafficMirrorFilterRule_destinationCidrBlock,
    trafficMirrorFilterRule_destinationPortRange,
    trafficMirrorFilterRule_protocol,
    trafficMirrorFilterRule_ruleAction,
    trafficMirrorFilterRule_ruleNumber,
    trafficMirrorFilterRule_sourceCidrBlock,
    trafficMirrorFilterRule_sourcePortRange,
    trafficMirrorFilterRule_trafficDirection,
    trafficMirrorFilterRule_trafficMirrorFilterId,
    trafficMirrorFilterRule_trafficMirrorFilterRuleId,

    -- ** TrafficMirrorPortRange
    trafficMirrorPortRange_fromPort,
    trafficMirrorPortRange_toPort,

    -- ** TrafficMirrorPortRangeRequest
    trafficMirrorPortRangeRequest_fromPort,
    trafficMirrorPortRangeRequest_toPort,

    -- ** TrafficMirrorSession
    trafficMirrorSession_description,
    trafficMirrorSession_networkInterfaceId,
    trafficMirrorSession_ownerId,
    trafficMirrorSession_packetLength,
    trafficMirrorSession_sessionNumber,
    trafficMirrorSession_tags,
    trafficMirrorSession_trafficMirrorFilterId,
    trafficMirrorSession_trafficMirrorSessionId,
    trafficMirrorSession_trafficMirrorTargetId,
    trafficMirrorSession_virtualNetworkId,

    -- ** TrafficMirrorTarget
    trafficMirrorTarget_description,
    trafficMirrorTarget_gatewayLoadBalancerEndpointId,
    trafficMirrorTarget_networkInterfaceId,
    trafficMirrorTarget_networkLoadBalancerArn,
    trafficMirrorTarget_ownerId,
    trafficMirrorTarget_tags,
    trafficMirrorTarget_trafficMirrorTargetId,
    trafficMirrorTarget_type,

    -- ** TransitGateway
    transitGateway_creationTime,
    transitGateway_description,
    transitGateway_options,
    transitGateway_ownerId,
    transitGateway_state,
    transitGateway_tags,
    transitGateway_transitGatewayArn,
    transitGateway_transitGatewayId,

    -- ** TransitGatewayAssociation
    transitGatewayAssociation_resourceId,
    transitGatewayAssociation_resourceType,
    transitGatewayAssociation_state,
    transitGatewayAssociation_transitGatewayAttachmentId,
    transitGatewayAssociation_transitGatewayRouteTableId,

    -- ** TransitGatewayAttachment
    transitGatewayAttachment_association,
    transitGatewayAttachment_creationTime,
    transitGatewayAttachment_resourceId,
    transitGatewayAttachment_resourceOwnerId,
    transitGatewayAttachment_resourceType,
    transitGatewayAttachment_state,
    transitGatewayAttachment_tags,
    transitGatewayAttachment_transitGatewayAttachmentId,
    transitGatewayAttachment_transitGatewayId,
    transitGatewayAttachment_transitGatewayOwnerId,

    -- ** TransitGatewayAttachmentAssociation
    transitGatewayAttachmentAssociation_state,
    transitGatewayAttachmentAssociation_transitGatewayRouteTableId,

    -- ** TransitGatewayAttachmentBgpConfiguration
    transitGatewayAttachmentBgpConfiguration_bgpStatus,
    transitGatewayAttachmentBgpConfiguration_peerAddress,
    transitGatewayAttachmentBgpConfiguration_peerAsn,
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
    transitGatewayConnect_transitGatewayId,
    transitGatewayConnect_transportTransitGatewayAttachmentId,

    -- ** TransitGatewayConnectOptions
    transitGatewayConnectOptions_protocol,

    -- ** TransitGatewayConnectPeer
    transitGatewayConnectPeer_connectPeerConfiguration,
    transitGatewayConnectPeer_creationTime,
    transitGatewayConnectPeer_state,
    transitGatewayConnectPeer_tags,
    transitGatewayConnectPeer_transitGatewayAttachmentId,
    transitGatewayConnectPeer_transitGatewayConnectPeerId,

    -- ** TransitGatewayConnectPeerConfiguration
    transitGatewayConnectPeerConfiguration_bgpConfigurations,
    transitGatewayConnectPeerConfiguration_insideCidrBlocks,
    transitGatewayConnectPeerConfiguration_peerAddress,
    transitGatewayConnectPeerConfiguration_protocol,
    transitGatewayConnectPeerConfiguration_transitGatewayAddress,

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
    transitGatewayMulticastDomain_creationTime,
    transitGatewayMulticastDomain_options,
    transitGatewayMulticastDomain_ownerId,
    transitGatewayMulticastDomain_state,
    transitGatewayMulticastDomain_tags,
    transitGatewayMulticastDomain_transitGatewayId,
    transitGatewayMulticastDomain_transitGatewayMulticastDomainArn,
    transitGatewayMulticastDomain_transitGatewayMulticastDomainId,

    -- ** TransitGatewayMulticastDomainAssociation
    transitGatewayMulticastDomainAssociation_resourceId,
    transitGatewayMulticastDomainAssociation_resourceOwnerId,
    transitGatewayMulticastDomainAssociation_resourceType,
    transitGatewayMulticastDomainAssociation_subnet,
    transitGatewayMulticastDomainAssociation_transitGatewayAttachmentId,

    -- ** TransitGatewayMulticastDomainAssociations
    transitGatewayMulticastDomainAssociations_resourceId,
    transitGatewayMulticastDomainAssociations_resourceOwnerId,
    transitGatewayMulticastDomainAssociations_resourceType,
    transitGatewayMulticastDomainAssociations_subnets,
    transitGatewayMulticastDomainAssociations_transitGatewayAttachmentId,
    transitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId,

    -- ** TransitGatewayMulticastDomainOptions
    transitGatewayMulticastDomainOptions_autoAcceptSharedAssociations,
    transitGatewayMulticastDomainOptions_igmpv2Support,
    transitGatewayMulticastDomainOptions_staticSourcesSupport,

    -- ** TransitGatewayMulticastGroup
    transitGatewayMulticastGroup_groupIpAddress,
    transitGatewayMulticastGroup_groupMember,
    transitGatewayMulticastGroup_groupSource,
    transitGatewayMulticastGroup_memberType,
    transitGatewayMulticastGroup_networkInterfaceId,
    transitGatewayMulticastGroup_resourceId,
    transitGatewayMulticastGroup_resourceOwnerId,
    transitGatewayMulticastGroup_resourceType,
    transitGatewayMulticastGroup_sourceType,
    transitGatewayMulticastGroup_subnetId,
    transitGatewayMulticastGroup_transitGatewayAttachmentId,

    -- ** TransitGatewayMulticastRegisteredGroupMembers
    transitGatewayMulticastRegisteredGroupMembers_groupIpAddress,
    transitGatewayMulticastRegisteredGroupMembers_registeredNetworkInterfaceIds,
    transitGatewayMulticastRegisteredGroupMembers_transitGatewayMulticastDomainId,

    -- ** TransitGatewayMulticastRegisteredGroupSources
    transitGatewayMulticastRegisteredGroupSources_groupIpAddress,
    transitGatewayMulticastRegisteredGroupSources_registeredNetworkInterfaceIds,
    transitGatewayMulticastRegisteredGroupSources_transitGatewayMulticastDomainId,

    -- ** TransitGatewayOptions
    transitGatewayOptions_amazonSideAsn,
    transitGatewayOptions_associationDefaultRouteTableId,
    transitGatewayOptions_autoAcceptSharedAttachments,
    transitGatewayOptions_defaultRouteTableAssociation,
    transitGatewayOptions_defaultRouteTablePropagation,
    transitGatewayOptions_dnsSupport,
    transitGatewayOptions_multicastSupport,
    transitGatewayOptions_propagationDefaultRouteTableId,
    transitGatewayOptions_transitGatewayCidrBlocks,
    transitGatewayOptions_vpnEcmpSupport,

    -- ** TransitGatewayPeeringAttachment
    transitGatewayPeeringAttachment_accepterTgwInfo,
    transitGatewayPeeringAttachment_accepterTransitGatewayAttachmentId,
    transitGatewayPeeringAttachment_creationTime,
    transitGatewayPeeringAttachment_options,
    transitGatewayPeeringAttachment_requesterTgwInfo,
    transitGatewayPeeringAttachment_state,
    transitGatewayPeeringAttachment_status,
    transitGatewayPeeringAttachment_tags,
    transitGatewayPeeringAttachment_transitGatewayAttachmentId,

    -- ** TransitGatewayPeeringAttachmentOptions
    transitGatewayPeeringAttachmentOptions_dynamicRouting,

    -- ** TransitGatewayPolicyRule
    transitGatewayPolicyRule_destinationCidrBlock,
    transitGatewayPolicyRule_destinationPortRange,
    transitGatewayPolicyRule_metaData,
    transitGatewayPolicyRule_protocol,
    transitGatewayPolicyRule_sourceCidrBlock,
    transitGatewayPolicyRule_sourcePortRange,

    -- ** TransitGatewayPolicyRuleMetaData
    transitGatewayPolicyRuleMetaData_metaDataKey,
    transitGatewayPolicyRuleMetaData_metaDataValue,

    -- ** TransitGatewayPolicyTable
    transitGatewayPolicyTable_creationTime,
    transitGatewayPolicyTable_state,
    transitGatewayPolicyTable_tags,
    transitGatewayPolicyTable_transitGatewayId,
    transitGatewayPolicyTable_transitGatewayPolicyTableId,

    -- ** TransitGatewayPolicyTableAssociation
    transitGatewayPolicyTableAssociation_resourceId,
    transitGatewayPolicyTableAssociation_resourceType,
    transitGatewayPolicyTableAssociation_state,
    transitGatewayPolicyTableAssociation_transitGatewayAttachmentId,
    transitGatewayPolicyTableAssociation_transitGatewayPolicyTableId,

    -- ** TransitGatewayPolicyTableEntry
    transitGatewayPolicyTableEntry_policyRule,
    transitGatewayPolicyTableEntry_policyRuleNumber,
    transitGatewayPolicyTableEntry_targetRouteTableId,

    -- ** TransitGatewayPrefixListAttachment
    transitGatewayPrefixListAttachment_resourceId,
    transitGatewayPrefixListAttachment_resourceType,
    transitGatewayPrefixListAttachment_transitGatewayAttachmentId,

    -- ** TransitGatewayPrefixListReference
    transitGatewayPrefixListReference_blackhole,
    transitGatewayPrefixListReference_prefixListId,
    transitGatewayPrefixListReference_prefixListOwnerId,
    transitGatewayPrefixListReference_state,
    transitGatewayPrefixListReference_transitGatewayAttachment,
    transitGatewayPrefixListReference_transitGatewayRouteTableId,

    -- ** TransitGatewayPropagation
    transitGatewayPropagation_resourceId,
    transitGatewayPropagation_resourceType,
    transitGatewayPropagation_state,
    transitGatewayPropagation_transitGatewayAttachmentId,
    transitGatewayPropagation_transitGatewayRouteTableAnnouncementId,
    transitGatewayPropagation_transitGatewayRouteTableId,

    -- ** TransitGatewayRequestOptions
    transitGatewayRequestOptions_amazonSideAsn,
    transitGatewayRequestOptions_autoAcceptSharedAttachments,
    transitGatewayRequestOptions_defaultRouteTableAssociation,
    transitGatewayRequestOptions_defaultRouteTablePropagation,
    transitGatewayRequestOptions_dnsSupport,
    transitGatewayRequestOptions_multicastSupport,
    transitGatewayRequestOptions_transitGatewayCidrBlocks,
    transitGatewayRequestOptions_vpnEcmpSupport,

    -- ** TransitGatewayRoute
    transitGatewayRoute_destinationCidrBlock,
    transitGatewayRoute_prefixListId,
    transitGatewayRoute_state,
    transitGatewayRoute_transitGatewayAttachments,
    transitGatewayRoute_transitGatewayRouteTableAnnouncementId,
    transitGatewayRoute_type,

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
    transitGatewayRouteTable_transitGatewayId,
    transitGatewayRouteTable_transitGatewayRouteTableId,

    -- ** TransitGatewayRouteTableAnnouncement
    transitGatewayRouteTableAnnouncement_announcementDirection,
    transitGatewayRouteTableAnnouncement_coreNetworkId,
    transitGatewayRouteTableAnnouncement_creationTime,
    transitGatewayRouteTableAnnouncement_peerCoreNetworkId,
    transitGatewayRouteTableAnnouncement_peerTransitGatewayId,
    transitGatewayRouteTableAnnouncement_peeringAttachmentId,
    transitGatewayRouteTableAnnouncement_state,
    transitGatewayRouteTableAnnouncement_tags,
    transitGatewayRouteTableAnnouncement_transitGatewayId,
    transitGatewayRouteTableAnnouncement_transitGatewayRouteTableAnnouncementId,
    transitGatewayRouteTableAnnouncement_transitGatewayRouteTableId,

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
    transitGatewayRouteTableRoute_attachmentId,
    transitGatewayRouteTableRoute_destinationCidr,
    transitGatewayRouteTableRoute_prefixListId,
    transitGatewayRouteTableRoute_resourceId,
    transitGatewayRouteTableRoute_resourceType,
    transitGatewayRouteTableRoute_routeOrigin,
    transitGatewayRouteTableRoute_state,

    -- ** TransitGatewayVpcAttachment
    transitGatewayVpcAttachment_creationTime,
    transitGatewayVpcAttachment_options,
    transitGatewayVpcAttachment_state,
    transitGatewayVpcAttachment_subnetIds,
    transitGatewayVpcAttachment_tags,
    transitGatewayVpcAttachment_transitGatewayAttachmentId,
    transitGatewayVpcAttachment_transitGatewayId,
    transitGatewayVpcAttachment_vpcId,
    transitGatewayVpcAttachment_vpcOwnerId,

    -- ** TransitGatewayVpcAttachmentOptions
    transitGatewayVpcAttachmentOptions_applianceModeSupport,
    transitGatewayVpcAttachmentOptions_dnsSupport,
    transitGatewayVpcAttachmentOptions_ipv6Support,

    -- ** TrunkInterfaceAssociation
    trunkInterfaceAssociation_associationId,
    trunkInterfaceAssociation_branchInterfaceId,
    trunkInterfaceAssociation_greKey,
    trunkInterfaceAssociation_interfaceProtocol,
    trunkInterfaceAssociation_tags,
    trunkInterfaceAssociation_trunkInterfaceId,
    trunkInterfaceAssociation_vlanId,

    -- ** TunnelOption
    tunnelOption_dpdTimeoutAction,
    tunnelOption_dpdTimeoutSeconds,
    tunnelOption_enableTunnelLifecycleControl,
    tunnelOption_ikeVersions,
    tunnelOption_logOptions,
    tunnelOption_outsideIpAddress,
    tunnelOption_phase1DHGroupNumbers,
    tunnelOption_phase1EncryptionAlgorithms,
    tunnelOption_phase1IntegrityAlgorithms,
    tunnelOption_phase1LifetimeSeconds,
    tunnelOption_phase2DHGroupNumbers,
    tunnelOption_phase2EncryptionAlgorithms,
    tunnelOption_phase2IntegrityAlgorithms,
    tunnelOption_phase2LifetimeSeconds,
    tunnelOption_preSharedKey,
    tunnelOption_rekeyFuzzPercentage,
    tunnelOption_rekeyMarginTimeSeconds,
    tunnelOption_replayWindowSize,
    tunnelOption_startupAction,
    tunnelOption_tunnelInsideCidr,
    tunnelOption_tunnelInsideIpv6Cidr,

    -- ** UnsuccessfulInstanceCreditSpecificationItem
    unsuccessfulInstanceCreditSpecificationItem_error,
    unsuccessfulInstanceCreditSpecificationItem_instanceId,

    -- ** UnsuccessfulInstanceCreditSpecificationItemError
    unsuccessfulInstanceCreditSpecificationItemError_code,
    unsuccessfulInstanceCreditSpecificationItemError_message,

    -- ** UnsuccessfulItem
    unsuccessfulItem_error,
    unsuccessfulItem_resourceId,

    -- ** UnsuccessfulItemError
    unsuccessfulItemError_code,
    unsuccessfulItemError_message,

    -- ** UserBucket
    userBucket_s3Bucket,
    userBucket_s3Key,

    -- ** UserBucketDetails
    userBucketDetails_s3Bucket,
    userBucketDetails_s3Key,

    -- ** UserData
    userData_data,

    -- ** UserIdGroupPair
    userIdGroupPair_description,
    userIdGroupPair_groupId,
    userIdGroupPair_groupName,
    userIdGroupPair_peeringStatus,
    userIdGroupPair_userId,
    userIdGroupPair_vpcId,
    userIdGroupPair_vpcPeeringConnectionId,

    -- ** VCpuCountRange
    vCpuCountRange_max,
    vCpuCountRange_min,

    -- ** VCpuCountRangeRequest
    vCpuCountRangeRequest_max,
    vCpuCountRangeRequest_min,

    -- ** VCpuInfo
    vCpuInfo_defaultCores,
    vCpuInfo_defaultThreadsPerCore,
    vCpuInfo_defaultVCpus,
    vCpuInfo_validCores,
    vCpuInfo_validThreadsPerCore,

    -- ** ValidationError
    validationError_code,
    validationError_message,

    -- ** ValidationWarning
    validationWarning_errors,

    -- ** VerifiedAccessEndpoint
    verifiedAccessEndpoint_applicationDomain,
    verifiedAccessEndpoint_attachmentType,
    verifiedAccessEndpoint_creationTime,
    verifiedAccessEndpoint_deletionTime,
    verifiedAccessEndpoint_description,
    verifiedAccessEndpoint_deviceValidationDomain,
    verifiedAccessEndpoint_domainCertificateArn,
    verifiedAccessEndpoint_endpointDomain,
    verifiedAccessEndpoint_endpointType,
    verifiedAccessEndpoint_lastUpdatedTime,
    verifiedAccessEndpoint_loadBalancerOptions,
    verifiedAccessEndpoint_networkInterfaceOptions,
    verifiedAccessEndpoint_securityGroupIds,
    verifiedAccessEndpoint_status,
    verifiedAccessEndpoint_tags,
    verifiedAccessEndpoint_verifiedAccessEndpointId,
    verifiedAccessEndpoint_verifiedAccessGroupId,
    verifiedAccessEndpoint_verifiedAccessInstanceId,

    -- ** VerifiedAccessEndpointEniOptions
    verifiedAccessEndpointEniOptions_networkInterfaceId,
    verifiedAccessEndpointEniOptions_port,
    verifiedAccessEndpointEniOptions_protocol,

    -- ** VerifiedAccessEndpointLoadBalancerOptions
    verifiedAccessEndpointLoadBalancerOptions_loadBalancerArn,
    verifiedAccessEndpointLoadBalancerOptions_port,
    verifiedAccessEndpointLoadBalancerOptions_protocol,
    verifiedAccessEndpointLoadBalancerOptions_subnetIds,

    -- ** VerifiedAccessEndpointStatus
    verifiedAccessEndpointStatus_code,
    verifiedAccessEndpointStatus_message,

    -- ** VerifiedAccessGroup
    verifiedAccessGroup_creationTime,
    verifiedAccessGroup_deletionTime,
    verifiedAccessGroup_description,
    verifiedAccessGroup_lastUpdatedTime,
    verifiedAccessGroup_owner,
    verifiedAccessGroup_tags,
    verifiedAccessGroup_verifiedAccessGroupArn,
    verifiedAccessGroup_verifiedAccessGroupId,
    verifiedAccessGroup_verifiedAccessInstanceId,

    -- ** VerifiedAccessInstance
    verifiedAccessInstance_creationTime,
    verifiedAccessInstance_description,
    verifiedAccessInstance_lastUpdatedTime,
    verifiedAccessInstance_tags,
    verifiedAccessInstance_verifiedAccessInstanceId,
    verifiedAccessInstance_verifiedAccessTrustProviders,

    -- ** VerifiedAccessInstanceLoggingConfiguration
    verifiedAccessInstanceLoggingConfiguration_accessLogs,
    verifiedAccessInstanceLoggingConfiguration_verifiedAccessInstanceId,

    -- ** VerifiedAccessLogCloudWatchLogsDestination
    verifiedAccessLogCloudWatchLogsDestination_deliveryStatus,
    verifiedAccessLogCloudWatchLogsDestination_enabled,
    verifiedAccessLogCloudWatchLogsDestination_logGroup,

    -- ** VerifiedAccessLogCloudWatchLogsDestinationOptions
    verifiedAccessLogCloudWatchLogsDestinationOptions_logGroup,
    verifiedAccessLogCloudWatchLogsDestinationOptions_enabled,

    -- ** VerifiedAccessLogDeliveryStatus
    verifiedAccessLogDeliveryStatus_code,
    verifiedAccessLogDeliveryStatus_message,

    -- ** VerifiedAccessLogKinesisDataFirehoseDestination
    verifiedAccessLogKinesisDataFirehoseDestination_deliveryStatus,
    verifiedAccessLogKinesisDataFirehoseDestination_deliveryStream,
    verifiedAccessLogKinesisDataFirehoseDestination_enabled,

    -- ** VerifiedAccessLogKinesisDataFirehoseDestinationOptions
    verifiedAccessLogKinesisDataFirehoseDestinationOptions_deliveryStream,
    verifiedAccessLogKinesisDataFirehoseDestinationOptions_enabled,

    -- ** VerifiedAccessLogOptions
    verifiedAccessLogOptions_cloudWatchLogs,
    verifiedAccessLogOptions_includeTrustContext,
    verifiedAccessLogOptions_kinesisDataFirehose,
    verifiedAccessLogOptions_logVersion,
    verifiedAccessLogOptions_s3,

    -- ** VerifiedAccessLogS3Destination
    verifiedAccessLogS3Destination_bucketName,
    verifiedAccessLogS3Destination_bucketOwner,
    verifiedAccessLogS3Destination_deliveryStatus,
    verifiedAccessLogS3Destination_enabled,
    verifiedAccessLogS3Destination_prefix,

    -- ** VerifiedAccessLogS3DestinationOptions
    verifiedAccessLogS3DestinationOptions_bucketName,
    verifiedAccessLogS3DestinationOptions_bucketOwner,
    verifiedAccessLogS3DestinationOptions_prefix,
    verifiedAccessLogS3DestinationOptions_enabled,

    -- ** VerifiedAccessLogs
    verifiedAccessLogs_cloudWatchLogs,
    verifiedAccessLogs_includeTrustContext,
    verifiedAccessLogs_kinesisDataFirehose,
    verifiedAccessLogs_logVersion,
    verifiedAccessLogs_s3,

    -- ** VerifiedAccessTrustProvider
    verifiedAccessTrustProvider_creationTime,
    verifiedAccessTrustProvider_description,
    verifiedAccessTrustProvider_deviceOptions,
    verifiedAccessTrustProvider_deviceTrustProviderType,
    verifiedAccessTrustProvider_lastUpdatedTime,
    verifiedAccessTrustProvider_oidcOptions,
    verifiedAccessTrustProvider_policyReferenceName,
    verifiedAccessTrustProvider_tags,
    verifiedAccessTrustProvider_trustProviderType,
    verifiedAccessTrustProvider_userTrustProviderType,
    verifiedAccessTrustProvider_verifiedAccessTrustProviderId,

    -- ** VerifiedAccessTrustProviderCondensed
    verifiedAccessTrustProviderCondensed_description,
    verifiedAccessTrustProviderCondensed_deviceTrustProviderType,
    verifiedAccessTrustProviderCondensed_trustProviderType,
    verifiedAccessTrustProviderCondensed_userTrustProviderType,
    verifiedAccessTrustProviderCondensed_verifiedAccessTrustProviderId,

    -- ** VgwTelemetry
    vgwTelemetry_acceptedRouteCount,
    vgwTelemetry_certificateArn,
    vgwTelemetry_lastStatusChange,
    vgwTelemetry_outsideIpAddress,
    vgwTelemetry_status,
    vgwTelemetry_statusMessage,

    -- ** Volume
    volume_attachments,
    volume_fastRestored,
    volume_iops,
    volume_kmsKeyId,
    volume_multiAttachEnabled,
    volume_outpostArn,
    volume_tags,
    volume_throughput,
    volume_availabilityZone,
    volume_createTime,
    volume_encrypted,
    volume_size,
    volume_snapshotId,
    volume_state,
    volume_volumeId,
    volume_volumeType,

    -- ** VolumeAttachment
    volumeAttachment_attachTime,
    volumeAttachment_deleteOnTermination,
    volumeAttachment_device,
    volumeAttachment_instanceId,
    volumeAttachment_state,
    volumeAttachment_volumeId,

    -- ** VolumeDetail
    volumeDetail_size,

    -- ** VolumeModification
    volumeModification_endTime,
    volumeModification_modificationState,
    volumeModification_originalIops,
    volumeModification_originalMultiAttachEnabled,
    volumeModification_originalSize,
    volumeModification_originalThroughput,
    volumeModification_originalVolumeType,
    volumeModification_progress,
    volumeModification_startTime,
    volumeModification_statusMessage,
    volumeModification_targetIops,
    volumeModification_targetMultiAttachEnabled,
    volumeModification_targetSize,
    volumeModification_targetThroughput,
    volumeModification_targetVolumeType,
    volumeModification_volumeId,

    -- ** VolumeStatusAction
    volumeStatusAction_code,
    volumeStatusAction_description,
    volumeStatusAction_eventId,
    volumeStatusAction_eventType,

    -- ** VolumeStatusAttachmentStatus
    volumeStatusAttachmentStatus_instanceId,
    volumeStatusAttachmentStatus_ioPerformance,

    -- ** VolumeStatusDetails
    volumeStatusDetails_name,
    volumeStatusDetails_status,

    -- ** VolumeStatusEvent
    volumeStatusEvent_description,
    volumeStatusEvent_eventId,
    volumeStatusEvent_eventType,
    volumeStatusEvent_instanceId,
    volumeStatusEvent_notAfter,
    volumeStatusEvent_notBefore,

    -- ** VolumeStatusInfo
    volumeStatusInfo_details,
    volumeStatusInfo_status,

    -- ** VolumeStatusItem
    volumeStatusItem_actions,
    volumeStatusItem_attachmentStatuses,
    volumeStatusItem_availabilityZone,
    volumeStatusItem_events,
    volumeStatusItem_outpostArn,
    volumeStatusItem_volumeId,
    volumeStatusItem_volumeStatus,

    -- ** Vpc
    vpc_cidrBlockAssociationSet,
    vpc_ipv6CidrBlockAssociationSet,
    vpc_isDefault,
    vpc_ownerId,
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
    vpcCidrBlockAssociation_associationId,
    vpcCidrBlockAssociation_cidrBlock,
    vpcCidrBlockAssociation_cidrBlockState,

    -- ** VpcCidrBlockState
    vpcCidrBlockState_state,
    vpcCidrBlockState_statusMessage,

    -- ** VpcClassicLink
    vpcClassicLink_classicLinkEnabled,
    vpcClassicLink_tags,
    vpcClassicLink_vpcId,

    -- ** VpcEndpoint
    vpcEndpoint_creationTimestamp,
    vpcEndpoint_dnsEntries,
    vpcEndpoint_dnsOptions,
    vpcEndpoint_groups,
    vpcEndpoint_ipAddressType,
    vpcEndpoint_lastError,
    vpcEndpoint_networkInterfaceIds,
    vpcEndpoint_ownerId,
    vpcEndpoint_policyDocument,
    vpcEndpoint_privateDnsEnabled,
    vpcEndpoint_requesterManaged,
    vpcEndpoint_routeTableIds,
    vpcEndpoint_serviceName,
    vpcEndpoint_state,
    vpcEndpoint_subnetIds,
    vpcEndpoint_tags,
    vpcEndpoint_vpcEndpointId,
    vpcEndpoint_vpcEndpointType,
    vpcEndpoint_vpcId,

    -- ** VpcEndpointConnection
    vpcEndpointConnection_creationTimestamp,
    vpcEndpointConnection_dnsEntries,
    vpcEndpointConnection_gatewayLoadBalancerArns,
    vpcEndpointConnection_ipAddressType,
    vpcEndpointConnection_networkLoadBalancerArns,
    vpcEndpointConnection_serviceId,
    vpcEndpointConnection_tags,
    vpcEndpointConnection_vpcEndpointConnectionId,
    vpcEndpointConnection_vpcEndpointId,
    vpcEndpointConnection_vpcEndpointOwner,
    vpcEndpointConnection_vpcEndpointState,

    -- ** VpcIpv6CidrBlockAssociation
    vpcIpv6CidrBlockAssociation_associationId,
    vpcIpv6CidrBlockAssociation_ipv6CidrBlock,
    vpcIpv6CidrBlockAssociation_ipv6CidrBlockState,
    vpcIpv6CidrBlockAssociation_ipv6Pool,
    vpcIpv6CidrBlockAssociation_networkBorderGroup,

    -- ** VpcPeeringConnection
    vpcPeeringConnection_accepterVpcInfo,
    vpcPeeringConnection_expirationTime,
    vpcPeeringConnection_requesterVpcInfo,
    vpcPeeringConnection_status,
    vpcPeeringConnection_tags,
    vpcPeeringConnection_vpcPeeringConnectionId,

    -- ** VpcPeeringConnectionOptionsDescription
    vpcPeeringConnectionOptionsDescription_allowDnsResolutionFromRemoteVpc,
    vpcPeeringConnectionOptionsDescription_allowEgressFromLocalClassicLinkToRemoteVpc,
    vpcPeeringConnectionOptionsDescription_allowEgressFromLocalVpcToRemoteClassicLink,

    -- ** VpcPeeringConnectionStateReason
    vpcPeeringConnectionStateReason_code,
    vpcPeeringConnectionStateReason_message,

    -- ** VpcPeeringConnectionVpcInfo
    vpcPeeringConnectionVpcInfo_cidrBlock,
    vpcPeeringConnectionVpcInfo_cidrBlockSet,
    vpcPeeringConnectionVpcInfo_ipv6CidrBlockSet,
    vpcPeeringConnectionVpcInfo_ownerId,
    vpcPeeringConnectionVpcInfo_peeringOptions,
    vpcPeeringConnectionVpcInfo_region,
    vpcPeeringConnectionVpcInfo_vpcId,

    -- ** VpnConnection
    vpnConnection_category,
    vpnConnection_coreNetworkArn,
    vpnConnection_coreNetworkAttachmentArn,
    vpnConnection_customerGatewayConfiguration,
    vpnConnection_gatewayAssociationState,
    vpnConnection_options,
    vpnConnection_routes,
    vpnConnection_tags,
    vpnConnection_transitGatewayId,
    vpnConnection_vgwTelemetry,
    vpnConnection_vpnGatewayId,
    vpnConnection_vpnConnectionId,
    vpnConnection_customerGatewayId,
    vpnConnection_state,
    vpnConnection_type,

    -- ** VpnConnectionDeviceType
    vpnConnectionDeviceType_platform,
    vpnConnectionDeviceType_software,
    vpnConnectionDeviceType_vendor,
    vpnConnectionDeviceType_vpnConnectionDeviceTypeId,

    -- ** VpnConnectionOptions
    vpnConnectionOptions_enableAcceleration,
    vpnConnectionOptions_localIpv4NetworkCidr,
    vpnConnectionOptions_localIpv6NetworkCidr,
    vpnConnectionOptions_outsideIpAddressType,
    vpnConnectionOptions_remoteIpv4NetworkCidr,
    vpnConnectionOptions_remoteIpv6NetworkCidr,
    vpnConnectionOptions_staticRoutesOnly,
    vpnConnectionOptions_transportTransitGatewayAttachmentId,
    vpnConnectionOptions_tunnelInsideIpVersion,
    vpnConnectionOptions_tunnelOptions,

    -- ** VpnConnectionOptionsSpecification
    vpnConnectionOptionsSpecification_enableAcceleration,
    vpnConnectionOptionsSpecification_localIpv4NetworkCidr,
    vpnConnectionOptionsSpecification_localIpv6NetworkCidr,
    vpnConnectionOptionsSpecification_outsideIpAddressType,
    vpnConnectionOptionsSpecification_remoteIpv4NetworkCidr,
    vpnConnectionOptionsSpecification_remoteIpv6NetworkCidr,
    vpnConnectionOptionsSpecification_staticRoutesOnly,
    vpnConnectionOptionsSpecification_transportTransitGatewayAttachmentId,
    vpnConnectionOptionsSpecification_tunnelInsideIpVersion,
    vpnConnectionOptionsSpecification_tunnelOptions,

    -- ** VpnGateway
    vpnGateway_amazonSideAsn,
    vpnGateway_availabilityZone,
    vpnGateway_state,
    vpnGateway_tags,
    vpnGateway_type,
    vpnGateway_vpcAttachments,
    vpnGateway_vpnGatewayId,

    -- ** VpnStaticRoute
    vpnStaticRoute_destinationCidrBlock,
    vpnStaticRoute_source,
    vpnStaticRoute_state,

    -- ** VpnTunnelLogOptions
    vpnTunnelLogOptions_cloudWatchLogOptions,

    -- ** VpnTunnelLogOptionsSpecification
    vpnTunnelLogOptionsSpecification_cloudWatchLogOptions,

    -- ** VpnTunnelOptionsSpecification
    vpnTunnelOptionsSpecification_dPDTimeoutAction,
    vpnTunnelOptionsSpecification_dPDTimeoutSeconds,
    vpnTunnelOptionsSpecification_enableTunnelLifecycleControl,
    vpnTunnelOptionsSpecification_iKEVersions,
    vpnTunnelOptionsSpecification_logOptions,
    vpnTunnelOptionsSpecification_phase1DHGroupNumbers,
    vpnTunnelOptionsSpecification_phase1EncryptionAlgorithms,
    vpnTunnelOptionsSpecification_phase1IntegrityAlgorithms,
    vpnTunnelOptionsSpecification_phase1LifetimeSeconds,
    vpnTunnelOptionsSpecification_phase2DHGroupNumbers,
    vpnTunnelOptionsSpecification_phase2EncryptionAlgorithms,
    vpnTunnelOptionsSpecification_phase2IntegrityAlgorithms,
    vpnTunnelOptionsSpecification_phase2LifetimeSeconds,
    vpnTunnelOptionsSpecification_preSharedKey,
    vpnTunnelOptionsSpecification_rekeyFuzzPercentage,
    vpnTunnelOptionsSpecification_rekeyMarginTimeSeconds,
    vpnTunnelOptionsSpecification_replayWindowSize,
    vpnTunnelOptionsSpecification_startupAction,
    vpnTunnelOptionsSpecification_tunnelInsideCidr,
    vpnTunnelOptionsSpecification_tunnelInsideIpv6Cidr,
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
import Amazonka.EC2.AssignPrivateNatGatewayAddress
import Amazonka.EC2.AssociateAddress
import Amazonka.EC2.AssociateClientVpnTargetNetwork
import Amazonka.EC2.AssociateDhcpOptions
import Amazonka.EC2.AssociateEnclaveCertificateIamRole
import Amazonka.EC2.AssociateIamInstanceProfile
import Amazonka.EC2.AssociateInstanceEventWindow
import Amazonka.EC2.AssociateIpamResourceDiscovery
import Amazonka.EC2.AssociateNatGatewayAddress
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
import Amazonka.EC2.AttachVerifiedAccessTrustProvider
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
import Amazonka.EC2.CreateInstanceConnectEndpoint
import Amazonka.EC2.CreateInstanceEventWindow
import Amazonka.EC2.CreateInstanceExportTask
import Amazonka.EC2.CreateInternetGateway
import Amazonka.EC2.CreateIpam
import Amazonka.EC2.CreateIpamPool
import Amazonka.EC2.CreateIpamResourceDiscovery
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
import Amazonka.EC2.CreateVerifiedAccessEndpoint
import Amazonka.EC2.CreateVerifiedAccessGroup
import Amazonka.EC2.CreateVerifiedAccessInstance
import Amazonka.EC2.CreateVerifiedAccessTrustProvider
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
import Amazonka.EC2.DeleteInstanceConnectEndpoint
import Amazonka.EC2.DeleteInstanceEventWindow
import Amazonka.EC2.DeleteInternetGateway
import Amazonka.EC2.DeleteIpam
import Amazonka.EC2.DeleteIpamPool
import Amazonka.EC2.DeleteIpamResourceDiscovery
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
import Amazonka.EC2.DeleteVerifiedAccessEndpoint
import Amazonka.EC2.DeleteVerifiedAccessGroup
import Amazonka.EC2.DeleteVerifiedAccessInstance
import Amazonka.EC2.DeleteVerifiedAccessTrustProvider
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
import Amazonka.EC2.DescribeAwsNetworkPerformanceMetricSubscriptions
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
import Amazonka.EC2.DescribeInstanceConnectEndpoints
import Amazonka.EC2.DescribeInstanceCreditSpecifications
import Amazonka.EC2.DescribeInstanceEventNotificationAttributes
import Amazonka.EC2.DescribeInstanceEventWindows
import Amazonka.EC2.DescribeInstanceStatus
import Amazonka.EC2.DescribeInstanceTypeOfferings
import Amazonka.EC2.DescribeInstanceTypes
import Amazonka.EC2.DescribeInstances
import Amazonka.EC2.DescribeInternetGateways
import Amazonka.EC2.DescribeIpamPools
import Amazonka.EC2.DescribeIpamResourceDiscoveries
import Amazonka.EC2.DescribeIpamResourceDiscoveryAssociations
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
import Amazonka.EC2.DescribeVerifiedAccessEndpoints
import Amazonka.EC2.DescribeVerifiedAccessGroups
import Amazonka.EC2.DescribeVerifiedAccessInstanceLoggingConfigurations
import Amazonka.EC2.DescribeVerifiedAccessInstances
import Amazonka.EC2.DescribeVerifiedAccessTrustProviders
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
import Amazonka.EC2.DetachVerifiedAccessTrustProvider
import Amazonka.EC2.DetachVolume
import Amazonka.EC2.DetachVpnGateway
import Amazonka.EC2.DisableAddressTransfer
import Amazonka.EC2.DisableAwsNetworkPerformanceMetricSubscription
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
import Amazonka.EC2.DisassociateIpamResourceDiscovery
import Amazonka.EC2.DisassociateNatGatewayAddress
import Amazonka.EC2.DisassociateRouteTable
import Amazonka.EC2.DisassociateSubnetCidrBlock
import Amazonka.EC2.DisassociateTransitGatewayMulticastDomain
import Amazonka.EC2.DisassociateTransitGatewayPolicyTable
import Amazonka.EC2.DisassociateTransitGatewayRouteTable
import Amazonka.EC2.DisassociateTrunkInterface
import Amazonka.EC2.DisassociateVpcCidrBlock
import Amazonka.EC2.EnableAddressTransfer
import Amazonka.EC2.EnableAwsNetworkPerformanceMetricSubscription
import Amazonka.EC2.EnableEbsEncryptionByDefault
import Amazonka.EC2.EnableFastLaunch
import Amazonka.EC2.EnableFastSnapshotRestores
import Amazonka.EC2.EnableImageDeprecation
import Amazonka.EC2.EnableIpamOrganizationAdminAccount
import Amazonka.EC2.EnableReachabilityAnalyzerOrganizationSharing
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
import Amazonka.EC2.GetAwsNetworkPerformanceData
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
import Amazonka.EC2.GetIpamDiscoveredAccounts
import Amazonka.EC2.GetIpamDiscoveredResourceCidrs
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
import Amazonka.EC2.GetVerifiedAccessEndpointPolicy
import Amazonka.EC2.GetVerifiedAccessGroupPolicy
import Amazonka.EC2.GetVpnConnectionDeviceSampleConfiguration
import Amazonka.EC2.GetVpnConnectionDeviceTypes
import Amazonka.EC2.GetVpnTunnelReplacementStatus
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
import Amazonka.EC2.ModifyIpamResourceDiscovery
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
import Amazonka.EC2.ModifyVerifiedAccessEndpoint
import Amazonka.EC2.ModifyVerifiedAccessEndpointPolicy
import Amazonka.EC2.ModifyVerifiedAccessGroup
import Amazonka.EC2.ModifyVerifiedAccessGroupPolicy
import Amazonka.EC2.ModifyVerifiedAccessInstance
import Amazonka.EC2.ModifyVerifiedAccessInstanceLoggingConfiguration
import Amazonka.EC2.ModifyVerifiedAccessTrustProvider
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
import Amazonka.EC2.ReplaceVpnTunnel
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
import Amazonka.EC2.Types.AttachmentEnaSrdSpecification
import Amazonka.EC2.Types.AttachmentEnaSrdUdpSpecification
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
import Amazonka.EC2.Types.CreateVerifiedAccessEndpointEniOptions
import Amazonka.EC2.Types.CreateVerifiedAccessEndpointLoadBalancerOptions
import Amazonka.EC2.Types.CreateVerifiedAccessTrustProviderDeviceOptions
import Amazonka.EC2.Types.CreateVerifiedAccessTrustProviderOidcOptions
import Amazonka.EC2.Types.CreateVolumePermission
import Amazonka.EC2.Types.CreateVolumePermissionModifications
import Amazonka.EC2.Types.CreditSpecification
import Amazonka.EC2.Types.CreditSpecificationRequest
import Amazonka.EC2.Types.CustomerGateway
import Amazonka.EC2.Types.DataQuery
import Amazonka.EC2.Types.DataResponse
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
import Amazonka.EC2.Types.DeviceOptions
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
import Amazonka.EC2.Types.Ec2InstanceConnectEndpoint
import Amazonka.EC2.Types.EfaInfo
import Amazonka.EC2.Types.EgressOnlyInternetGateway
import Amazonka.EC2.Types.ElasticGpuAssociation
import Amazonka.EC2.Types.ElasticGpuHealth
import Amazonka.EC2.Types.ElasticGpuSpecification
import Amazonka.EC2.Types.ElasticGpuSpecificationResponse
import Amazonka.EC2.Types.ElasticGpus
import Amazonka.EC2.Types.ElasticInferenceAccelerator
import Amazonka.EC2.Types.ElasticInferenceAcceleratorAssociation
import Amazonka.EC2.Types.EnaSrdSpecification
import Amazonka.EC2.Types.EnaSrdUdpSpecification
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
import Amazonka.EC2.Types.FilterPortRange
import Amazonka.EC2.Types.FirewallStatefulRule
import Amazonka.EC2.Types.FirewallStatelessRule
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
import Amazonka.EC2.Types.IpamDiscoveredAccount
import Amazonka.EC2.Types.IpamDiscoveredResourceCidr
import Amazonka.EC2.Types.IpamDiscoveryFailureReason
import Amazonka.EC2.Types.IpamOperatingRegion
import Amazonka.EC2.Types.IpamPool
import Amazonka.EC2.Types.IpamPoolAllocation
import Amazonka.EC2.Types.IpamPoolCidr
import Amazonka.EC2.Types.IpamPoolCidrFailureReason
import Amazonka.EC2.Types.IpamResourceCidr
import Amazonka.EC2.Types.IpamResourceDiscovery
import Amazonka.EC2.Types.IpamResourceDiscoveryAssociation
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
import Amazonka.EC2.Types.MaintenanceDetails
import Amazonka.EC2.Types.ManagedPrefixList
import Amazonka.EC2.Types.MemoryGiBPerVCpu
import Amazonka.EC2.Types.MemoryGiBPerVCpuRequest
import Amazonka.EC2.Types.MemoryInfo
import Amazonka.EC2.Types.MemoryMiB
import Amazonka.EC2.Types.MemoryMiBRequest
import Amazonka.EC2.Types.MetricPoint
import Amazonka.EC2.Types.ModifyTransitGatewayOptions
import Amazonka.EC2.Types.ModifyTransitGatewayVpcAttachmentRequestOptions
import Amazonka.EC2.Types.ModifyVerifiedAccessEndpointEniOptions
import Amazonka.EC2.Types.ModifyVerifiedAccessEndpointLoadBalancerOptions
import Amazonka.EC2.Types.ModifyVerifiedAccessTrustProviderOidcOptions
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
import Amazonka.EC2.Types.OidcOptions
import Amazonka.EC2.Types.OnDemandOptions
import Amazonka.EC2.Types.OnDemandOptionsRequest
import Amazonka.EC2.Types.PacketHeaderStatement
import Amazonka.EC2.Types.PacketHeaderStatementRequest
import Amazonka.EC2.Types.PathComponent
import Amazonka.EC2.Types.PathFilter
import Amazonka.EC2.Types.PathRequestFilter
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
import Amazonka.EC2.Types.RequestFilterPortRange
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
import Amazonka.EC2.Types.RuleGroupRuleOptionsPair
import Amazonka.EC2.Types.RuleGroupTypePair
import Amazonka.EC2.Types.RuleOption
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
import Amazonka.EC2.Types.Subscription
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
import Amazonka.EC2.Types.VerifiedAccessEndpoint
import Amazonka.EC2.Types.VerifiedAccessEndpointEniOptions
import Amazonka.EC2.Types.VerifiedAccessEndpointLoadBalancerOptions
import Amazonka.EC2.Types.VerifiedAccessEndpointStatus
import Amazonka.EC2.Types.VerifiedAccessGroup
import Amazonka.EC2.Types.VerifiedAccessInstance
import Amazonka.EC2.Types.VerifiedAccessInstanceLoggingConfiguration
import Amazonka.EC2.Types.VerifiedAccessLogCloudWatchLogsDestination
import Amazonka.EC2.Types.VerifiedAccessLogCloudWatchLogsDestinationOptions
import Amazonka.EC2.Types.VerifiedAccessLogDeliveryStatus
import Amazonka.EC2.Types.VerifiedAccessLogKinesisDataFirehoseDestination
import Amazonka.EC2.Types.VerifiedAccessLogKinesisDataFirehoseDestinationOptions
import Amazonka.EC2.Types.VerifiedAccessLogOptions
import Amazonka.EC2.Types.VerifiedAccessLogS3Destination
import Amazonka.EC2.Types.VerifiedAccessLogS3DestinationOptions
import Amazonka.EC2.Types.VerifiedAccessLogs
import Amazonka.EC2.Types.VerifiedAccessTrustProvider
import Amazonka.EC2.Types.VerifiedAccessTrustProviderCondensed
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
import Amazonka.EC2.UnassignPrivateNatGatewayAddress
import Amazonka.EC2.UnmonitorInstances
import Amazonka.EC2.UpdateSecurityGroupRuleDescriptionsEgress
import Amazonka.EC2.UpdateSecurityGroupRuleDescriptionsIngress
import Amazonka.EC2.WithdrawByoipCidr
