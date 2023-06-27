{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.EC2
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2016-11-15@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Elastic Compute Cloud
--
-- Amazon Elastic Compute Cloud (Amazon EC2) provides secure and resizable
-- computing capacity in the Amazon Web Services Cloud. Using Amazon EC2
-- eliminates the need to invest in hardware up front, so you can develop
-- and deploy applications faster. Amazon Virtual Private Cloud (Amazon
-- VPC) enables you to provision a logically isolated section of the Amazon
-- Web Services Cloud where you can launch Amazon Web Services resources in
-- a virtual network that you\'ve defined. Amazon Elastic Block Store
-- (Amazon EBS) provides block level storage volumes for use with EC2
-- instances. EBS volumes are highly available and reliable storage volumes
-- that can be attached to any running instance and used like a hard drive.
--
-- To learn more, see the following resources:
--
-- -   Amazon EC2: <http://aws.amazon.com/ec2 Amazon EC2 product page>,
--     <https://docs.aws.amazon.com/ec2/index.html Amazon EC2 documentation>
--
-- -   Amazon EBS: <http://aws.amazon.com/ebs Amazon EBS product page>,
--     <https://docs.aws.amazon.com/ebs/index.html Amazon EBS documentation>
--
-- -   Amazon VPC: <http://aws.amazon.com/vpc Amazon VPC product page>,
--     <https://docs.aws.amazon.com/vpc/index.html Amazon VPC documentation>
--
-- -   VPN: <http://aws.amazon.com/vpn VPN product page>,
--     <https://docs.aws.amazon.com/vpn/index.html VPN documentation>
module Amazonka.EC2
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- ** BundleTaskComplete
    newBundleTaskComplete,

    -- ** ConversionTaskCancelled
    newConversionTaskCancelled,

    -- ** ConversionTaskCompleted
    newConversionTaskCompleted,

    -- ** ConversionTaskDeleted
    newConversionTaskDeleted,

    -- ** CustomerGatewayAvailable
    newCustomerGatewayAvailable,

    -- ** ExportTaskCancelled
    newExportTaskCancelled,

    -- ** ExportTaskCompleted
    newExportTaskCompleted,

    -- ** ImageAvailable
    newImageAvailable,

    -- ** ImageExists
    newImageExists,

    -- ** InstanceExists
    newInstanceExists,

    -- ** InstanceRunning
    newInstanceRunning,

    -- ** InstanceStatusOk
    newInstanceStatusOk,

    -- ** InstanceStopped
    newInstanceStopped,

    -- ** InstanceTerminated
    newInstanceTerminated,

    -- ** InternetGatewayExists
    newInternetGatewayExists,

    -- ** KeyPairExists
    newKeyPairExists,

    -- ** NatGatewayAvailable
    newNatGatewayAvailable,

    -- ** NatGatewayDeleted
    newNatGatewayDeleted,

    -- ** NetworkInterfaceAvailable
    newNetworkInterfaceAvailable,

    -- ** PasswordDataAvailable
    newPasswordDataAvailable,

    -- ** SecurityGroupExists
    newSecurityGroupExists,

    -- ** SnapshotCompleted
    newSnapshotCompleted,

    -- ** SnapshotImported
    newSnapshotImported,

    -- ** SpotInstanceRequestFulfilled
    newSpotInstanceRequestFulfilled,

    -- ** SubnetAvailable
    newSubnetAvailable,

    -- ** SystemStatusOk
    newSystemStatusOk,

    -- ** VolumeAvailable
    newVolumeAvailable,

    -- ** VolumeDeleted
    newVolumeDeleted,

    -- ** VolumeInUse
    newVolumeInUse,

    -- ** VpcAvailable
    newVpcAvailable,

    -- ** VpcExists
    newVpcExists,

    -- ** VpcPeeringConnectionDeleted
    newVpcPeeringConnectionDeleted,

    -- ** VpcPeeringConnectionExists
    newVpcPeeringConnectionExists,

    -- ** VpnConnectionAvailable
    newVpnConnectionAvailable,

    -- ** VpnConnectionDeleted
    newVpnConnectionDeleted,

    -- * Operations
    -- $operations

    -- ** AcceptAddressTransfer
    AcceptAddressTransfer (AcceptAddressTransfer'),
    newAcceptAddressTransfer,
    AcceptAddressTransferResponse (AcceptAddressTransferResponse'),
    newAcceptAddressTransferResponse,

    -- ** AcceptReservedInstancesExchangeQuote
    AcceptReservedInstancesExchangeQuote (AcceptReservedInstancesExchangeQuote'),
    newAcceptReservedInstancesExchangeQuote,
    AcceptReservedInstancesExchangeQuoteResponse (AcceptReservedInstancesExchangeQuoteResponse'),
    newAcceptReservedInstancesExchangeQuoteResponse,

    -- ** AcceptTransitGatewayMulticastDomainAssociations
    AcceptTransitGatewayMulticastDomainAssociations (AcceptTransitGatewayMulticastDomainAssociations'),
    newAcceptTransitGatewayMulticastDomainAssociations,
    AcceptTransitGatewayMulticastDomainAssociationsResponse (AcceptTransitGatewayMulticastDomainAssociationsResponse'),
    newAcceptTransitGatewayMulticastDomainAssociationsResponse,

    -- ** AcceptTransitGatewayPeeringAttachment
    AcceptTransitGatewayPeeringAttachment (AcceptTransitGatewayPeeringAttachment'),
    newAcceptTransitGatewayPeeringAttachment,
    AcceptTransitGatewayPeeringAttachmentResponse (AcceptTransitGatewayPeeringAttachmentResponse'),
    newAcceptTransitGatewayPeeringAttachmentResponse,

    -- ** AcceptTransitGatewayVpcAttachment
    AcceptTransitGatewayVpcAttachment (AcceptTransitGatewayVpcAttachment'),
    newAcceptTransitGatewayVpcAttachment,
    AcceptTransitGatewayVpcAttachmentResponse (AcceptTransitGatewayVpcAttachmentResponse'),
    newAcceptTransitGatewayVpcAttachmentResponse,

    -- ** AcceptVpcEndpointConnections
    AcceptVpcEndpointConnections (AcceptVpcEndpointConnections'),
    newAcceptVpcEndpointConnections,
    AcceptVpcEndpointConnectionsResponse (AcceptVpcEndpointConnectionsResponse'),
    newAcceptVpcEndpointConnectionsResponse,

    -- ** AcceptVpcPeeringConnection
    AcceptVpcPeeringConnection (AcceptVpcPeeringConnection'),
    newAcceptVpcPeeringConnection,
    AcceptVpcPeeringConnectionResponse (AcceptVpcPeeringConnectionResponse'),
    newAcceptVpcPeeringConnectionResponse,

    -- ** AdvertiseByoipCidr
    AdvertiseByoipCidr (AdvertiseByoipCidr'),
    newAdvertiseByoipCidr,
    AdvertiseByoipCidrResponse (AdvertiseByoipCidrResponse'),
    newAdvertiseByoipCidrResponse,

    -- ** AllocateAddress
    AllocateAddress (AllocateAddress'),
    newAllocateAddress,
    AllocateAddressResponse (AllocateAddressResponse'),
    newAllocateAddressResponse,

    -- ** AllocateHosts
    AllocateHosts (AllocateHosts'),
    newAllocateHosts,
    AllocateHostsResponse (AllocateHostsResponse'),
    newAllocateHostsResponse,

    -- ** AllocateIpamPoolCidr
    AllocateIpamPoolCidr (AllocateIpamPoolCidr'),
    newAllocateIpamPoolCidr,
    AllocateIpamPoolCidrResponse (AllocateIpamPoolCidrResponse'),
    newAllocateIpamPoolCidrResponse,

    -- ** ApplySecurityGroupsToClientVpnTargetNetwork
    ApplySecurityGroupsToClientVpnTargetNetwork (ApplySecurityGroupsToClientVpnTargetNetwork'),
    newApplySecurityGroupsToClientVpnTargetNetwork,
    ApplySecurityGroupsToClientVpnTargetNetworkResponse (ApplySecurityGroupsToClientVpnTargetNetworkResponse'),
    newApplySecurityGroupsToClientVpnTargetNetworkResponse,

    -- ** AssignIpv6Addresses
    AssignIpv6Addresses (AssignIpv6Addresses'),
    newAssignIpv6Addresses,
    AssignIpv6AddressesResponse (AssignIpv6AddressesResponse'),
    newAssignIpv6AddressesResponse,

    -- ** AssignPrivateIpAddresses
    AssignPrivateIpAddresses (AssignPrivateIpAddresses'),
    newAssignPrivateIpAddresses,
    AssignPrivateIpAddressesResponse (AssignPrivateIpAddressesResponse'),
    newAssignPrivateIpAddressesResponse,

    -- ** AssignPrivateNatGatewayAddress
    AssignPrivateNatGatewayAddress (AssignPrivateNatGatewayAddress'),
    newAssignPrivateNatGatewayAddress,
    AssignPrivateNatGatewayAddressResponse (AssignPrivateNatGatewayAddressResponse'),
    newAssignPrivateNatGatewayAddressResponse,

    -- ** AssociateAddress
    AssociateAddress (AssociateAddress'),
    newAssociateAddress,
    AssociateAddressResponse (AssociateAddressResponse'),
    newAssociateAddressResponse,

    -- ** AssociateClientVpnTargetNetwork
    AssociateClientVpnTargetNetwork (AssociateClientVpnTargetNetwork'),
    newAssociateClientVpnTargetNetwork,
    AssociateClientVpnTargetNetworkResponse (AssociateClientVpnTargetNetworkResponse'),
    newAssociateClientVpnTargetNetworkResponse,

    -- ** AssociateDhcpOptions
    AssociateDhcpOptions (AssociateDhcpOptions'),
    newAssociateDhcpOptions,
    AssociateDhcpOptionsResponse (AssociateDhcpOptionsResponse'),
    newAssociateDhcpOptionsResponse,

    -- ** AssociateEnclaveCertificateIamRole
    AssociateEnclaveCertificateIamRole (AssociateEnclaveCertificateIamRole'),
    newAssociateEnclaveCertificateIamRole,
    AssociateEnclaveCertificateIamRoleResponse (AssociateEnclaveCertificateIamRoleResponse'),
    newAssociateEnclaveCertificateIamRoleResponse,

    -- ** AssociateIamInstanceProfile
    AssociateIamInstanceProfile (AssociateIamInstanceProfile'),
    newAssociateIamInstanceProfile,
    AssociateIamInstanceProfileResponse (AssociateIamInstanceProfileResponse'),
    newAssociateIamInstanceProfileResponse,

    -- ** AssociateInstanceEventWindow
    AssociateInstanceEventWindow (AssociateInstanceEventWindow'),
    newAssociateInstanceEventWindow,
    AssociateInstanceEventWindowResponse (AssociateInstanceEventWindowResponse'),
    newAssociateInstanceEventWindowResponse,

    -- ** AssociateIpamResourceDiscovery
    AssociateIpamResourceDiscovery (AssociateIpamResourceDiscovery'),
    newAssociateIpamResourceDiscovery,
    AssociateIpamResourceDiscoveryResponse (AssociateIpamResourceDiscoveryResponse'),
    newAssociateIpamResourceDiscoveryResponse,

    -- ** AssociateNatGatewayAddress
    AssociateNatGatewayAddress (AssociateNatGatewayAddress'),
    newAssociateNatGatewayAddress,
    AssociateNatGatewayAddressResponse (AssociateNatGatewayAddressResponse'),
    newAssociateNatGatewayAddressResponse,

    -- ** AssociateRouteTable
    AssociateRouteTable (AssociateRouteTable'),
    newAssociateRouteTable,
    AssociateRouteTableResponse (AssociateRouteTableResponse'),
    newAssociateRouteTableResponse,

    -- ** AssociateSubnetCidrBlock
    AssociateSubnetCidrBlock (AssociateSubnetCidrBlock'),
    newAssociateSubnetCidrBlock,
    AssociateSubnetCidrBlockResponse (AssociateSubnetCidrBlockResponse'),
    newAssociateSubnetCidrBlockResponse,

    -- ** AssociateTransitGatewayMulticastDomain
    AssociateTransitGatewayMulticastDomain (AssociateTransitGatewayMulticastDomain'),
    newAssociateTransitGatewayMulticastDomain,
    AssociateTransitGatewayMulticastDomainResponse (AssociateTransitGatewayMulticastDomainResponse'),
    newAssociateTransitGatewayMulticastDomainResponse,

    -- ** AssociateTransitGatewayPolicyTable
    AssociateTransitGatewayPolicyTable (AssociateTransitGatewayPolicyTable'),
    newAssociateTransitGatewayPolicyTable,
    AssociateTransitGatewayPolicyTableResponse (AssociateTransitGatewayPolicyTableResponse'),
    newAssociateTransitGatewayPolicyTableResponse,

    -- ** AssociateTransitGatewayRouteTable
    AssociateTransitGatewayRouteTable (AssociateTransitGatewayRouteTable'),
    newAssociateTransitGatewayRouteTable,
    AssociateTransitGatewayRouteTableResponse (AssociateTransitGatewayRouteTableResponse'),
    newAssociateTransitGatewayRouteTableResponse,

    -- ** AssociateTrunkInterface
    AssociateTrunkInterface (AssociateTrunkInterface'),
    newAssociateTrunkInterface,
    AssociateTrunkInterfaceResponse (AssociateTrunkInterfaceResponse'),
    newAssociateTrunkInterfaceResponse,

    -- ** AssociateVpcCidrBlock
    AssociateVpcCidrBlock (AssociateVpcCidrBlock'),
    newAssociateVpcCidrBlock,
    AssociateVpcCidrBlockResponse (AssociateVpcCidrBlockResponse'),
    newAssociateVpcCidrBlockResponse,

    -- ** AttachClassicLinkVpc
    AttachClassicLinkVpc (AttachClassicLinkVpc'),
    newAttachClassicLinkVpc,
    AttachClassicLinkVpcResponse (AttachClassicLinkVpcResponse'),
    newAttachClassicLinkVpcResponse,

    -- ** AttachInternetGateway
    AttachInternetGateway (AttachInternetGateway'),
    newAttachInternetGateway,
    AttachInternetGatewayResponse (AttachInternetGatewayResponse'),
    newAttachInternetGatewayResponse,

    -- ** AttachNetworkInterface
    AttachNetworkInterface (AttachNetworkInterface'),
    newAttachNetworkInterface,
    AttachNetworkInterfaceResponse (AttachNetworkInterfaceResponse'),
    newAttachNetworkInterfaceResponse,

    -- ** AttachVerifiedAccessTrustProvider
    AttachVerifiedAccessTrustProvider (AttachVerifiedAccessTrustProvider'),
    newAttachVerifiedAccessTrustProvider,
    AttachVerifiedAccessTrustProviderResponse (AttachVerifiedAccessTrustProviderResponse'),
    newAttachVerifiedAccessTrustProviderResponse,

    -- ** AttachVolume
    AttachVolume (AttachVolume'),
    newAttachVolume,
    VolumeAttachment (VolumeAttachment'),
    newVolumeAttachment,

    -- ** AttachVpnGateway
    AttachVpnGateway (AttachVpnGateway'),
    newAttachVpnGateway,
    AttachVpnGatewayResponse (AttachVpnGatewayResponse'),
    newAttachVpnGatewayResponse,

    -- ** AuthorizeClientVpnIngress
    AuthorizeClientVpnIngress (AuthorizeClientVpnIngress'),
    newAuthorizeClientVpnIngress,
    AuthorizeClientVpnIngressResponse (AuthorizeClientVpnIngressResponse'),
    newAuthorizeClientVpnIngressResponse,

    -- ** AuthorizeSecurityGroupEgress
    AuthorizeSecurityGroupEgress (AuthorizeSecurityGroupEgress'),
    newAuthorizeSecurityGroupEgress,
    AuthorizeSecurityGroupEgressResponse (AuthorizeSecurityGroupEgressResponse'),
    newAuthorizeSecurityGroupEgressResponse,

    -- ** AuthorizeSecurityGroupIngress
    AuthorizeSecurityGroupIngress (AuthorizeSecurityGroupIngress'),
    newAuthorizeSecurityGroupIngress,
    AuthorizeSecurityGroupIngressResponse (AuthorizeSecurityGroupIngressResponse'),
    newAuthorizeSecurityGroupIngressResponse,

    -- ** BundleInstance
    BundleInstance (BundleInstance'),
    newBundleInstance,
    BundleInstanceResponse (BundleInstanceResponse'),
    newBundleInstanceResponse,

    -- ** CancelBundleTask
    CancelBundleTask (CancelBundleTask'),
    newCancelBundleTask,
    CancelBundleTaskResponse (CancelBundleTaskResponse'),
    newCancelBundleTaskResponse,

    -- ** CancelCapacityReservation
    CancelCapacityReservation (CancelCapacityReservation'),
    newCancelCapacityReservation,
    CancelCapacityReservationResponse (CancelCapacityReservationResponse'),
    newCancelCapacityReservationResponse,

    -- ** CancelCapacityReservationFleets
    CancelCapacityReservationFleets (CancelCapacityReservationFleets'),
    newCancelCapacityReservationFleets,
    CancelCapacityReservationFleetsResponse (CancelCapacityReservationFleetsResponse'),
    newCancelCapacityReservationFleetsResponse,

    -- ** CancelConversionTask
    CancelConversionTask (CancelConversionTask'),
    newCancelConversionTask,
    CancelConversionTaskResponse (CancelConversionTaskResponse'),
    newCancelConversionTaskResponse,

    -- ** CancelExportTask
    CancelExportTask (CancelExportTask'),
    newCancelExportTask,
    CancelExportTaskResponse (CancelExportTaskResponse'),
    newCancelExportTaskResponse,

    -- ** CancelImageLaunchPermission
    CancelImageLaunchPermission (CancelImageLaunchPermission'),
    newCancelImageLaunchPermission,
    CancelImageLaunchPermissionResponse (CancelImageLaunchPermissionResponse'),
    newCancelImageLaunchPermissionResponse,

    -- ** CancelImportTask
    CancelImportTask (CancelImportTask'),
    newCancelImportTask,
    CancelImportTaskResponse (CancelImportTaskResponse'),
    newCancelImportTaskResponse,

    -- ** CancelReservedInstancesListing
    CancelReservedInstancesListing (CancelReservedInstancesListing'),
    newCancelReservedInstancesListing,
    CancelReservedInstancesListingResponse (CancelReservedInstancesListingResponse'),
    newCancelReservedInstancesListingResponse,

    -- ** CancelSpotFleetRequests
    CancelSpotFleetRequests (CancelSpotFleetRequests'),
    newCancelSpotFleetRequests,
    CancelSpotFleetRequestsResponse (CancelSpotFleetRequestsResponse'),
    newCancelSpotFleetRequestsResponse,

    -- ** CancelSpotInstanceRequests
    CancelSpotInstanceRequests (CancelSpotInstanceRequests'),
    newCancelSpotInstanceRequests,
    CancelSpotInstanceRequestsResponse (CancelSpotInstanceRequestsResponse'),
    newCancelSpotInstanceRequestsResponse,

    -- ** ConfirmProductInstance
    ConfirmProductInstance (ConfirmProductInstance'),
    newConfirmProductInstance,
    ConfirmProductInstanceResponse (ConfirmProductInstanceResponse'),
    newConfirmProductInstanceResponse,

    -- ** CopyFpgaImage
    CopyFpgaImage (CopyFpgaImage'),
    newCopyFpgaImage,
    CopyFpgaImageResponse (CopyFpgaImageResponse'),
    newCopyFpgaImageResponse,

    -- ** CopyImage
    CopyImage (CopyImage'),
    newCopyImage,
    CopyImageResponse (CopyImageResponse'),
    newCopyImageResponse,

    -- ** CopySnapshot
    CopySnapshot (CopySnapshot'),
    newCopySnapshot,
    CopySnapshotResponse (CopySnapshotResponse'),
    newCopySnapshotResponse,

    -- ** CreateCapacityReservation
    CreateCapacityReservation (CreateCapacityReservation'),
    newCreateCapacityReservation,
    CreateCapacityReservationResponse (CreateCapacityReservationResponse'),
    newCreateCapacityReservationResponse,

    -- ** CreateCapacityReservationFleet
    CreateCapacityReservationFleet (CreateCapacityReservationFleet'),
    newCreateCapacityReservationFleet,
    CreateCapacityReservationFleetResponse (CreateCapacityReservationFleetResponse'),
    newCreateCapacityReservationFleetResponse,

    -- ** CreateCarrierGateway
    CreateCarrierGateway (CreateCarrierGateway'),
    newCreateCarrierGateway,
    CreateCarrierGatewayResponse (CreateCarrierGatewayResponse'),
    newCreateCarrierGatewayResponse,

    -- ** CreateClientVpnEndpoint
    CreateClientVpnEndpoint (CreateClientVpnEndpoint'),
    newCreateClientVpnEndpoint,
    CreateClientVpnEndpointResponse (CreateClientVpnEndpointResponse'),
    newCreateClientVpnEndpointResponse,

    -- ** CreateClientVpnRoute
    CreateClientVpnRoute (CreateClientVpnRoute'),
    newCreateClientVpnRoute,
    CreateClientVpnRouteResponse (CreateClientVpnRouteResponse'),
    newCreateClientVpnRouteResponse,

    -- ** CreateCoipCidr
    CreateCoipCidr (CreateCoipCidr'),
    newCreateCoipCidr,
    CreateCoipCidrResponse (CreateCoipCidrResponse'),
    newCreateCoipCidrResponse,

    -- ** CreateCoipPool
    CreateCoipPool (CreateCoipPool'),
    newCreateCoipPool,
    CreateCoipPoolResponse (CreateCoipPoolResponse'),
    newCreateCoipPoolResponse,

    -- ** CreateCustomerGateway
    CreateCustomerGateway (CreateCustomerGateway'),
    newCreateCustomerGateway,
    CreateCustomerGatewayResponse (CreateCustomerGatewayResponse'),
    newCreateCustomerGatewayResponse,

    -- ** CreateDefaultSubnet
    CreateDefaultSubnet (CreateDefaultSubnet'),
    newCreateDefaultSubnet,
    CreateDefaultSubnetResponse (CreateDefaultSubnetResponse'),
    newCreateDefaultSubnetResponse,

    -- ** CreateDefaultVpc
    CreateDefaultVpc (CreateDefaultVpc'),
    newCreateDefaultVpc,
    CreateDefaultVpcResponse (CreateDefaultVpcResponse'),
    newCreateDefaultVpcResponse,

    -- ** CreateDhcpOptions
    CreateDhcpOptions (CreateDhcpOptions'),
    newCreateDhcpOptions,
    CreateDhcpOptionsResponse (CreateDhcpOptionsResponse'),
    newCreateDhcpOptionsResponse,

    -- ** CreateEgressOnlyInternetGateway
    CreateEgressOnlyInternetGateway (CreateEgressOnlyInternetGateway'),
    newCreateEgressOnlyInternetGateway,
    CreateEgressOnlyInternetGatewayResponse (CreateEgressOnlyInternetGatewayResponse'),
    newCreateEgressOnlyInternetGatewayResponse,

    -- ** CreateFleet
    CreateFleet (CreateFleet'),
    newCreateFleet,
    CreateFleetResponse (CreateFleetResponse'),
    newCreateFleetResponse,

    -- ** CreateFlowLogs
    CreateFlowLogs (CreateFlowLogs'),
    newCreateFlowLogs,
    CreateFlowLogsResponse (CreateFlowLogsResponse'),
    newCreateFlowLogsResponse,

    -- ** CreateFpgaImage
    CreateFpgaImage (CreateFpgaImage'),
    newCreateFpgaImage,
    CreateFpgaImageResponse (CreateFpgaImageResponse'),
    newCreateFpgaImageResponse,

    -- ** CreateImage
    CreateImage (CreateImage'),
    newCreateImage,
    CreateImageResponse (CreateImageResponse'),
    newCreateImageResponse,

    -- ** CreateInstanceConnectEndpoint
    CreateInstanceConnectEndpoint (CreateInstanceConnectEndpoint'),
    newCreateInstanceConnectEndpoint,
    CreateInstanceConnectEndpointResponse (CreateInstanceConnectEndpointResponse'),
    newCreateInstanceConnectEndpointResponse,

    -- ** CreateInstanceEventWindow
    CreateInstanceEventWindow (CreateInstanceEventWindow'),
    newCreateInstanceEventWindow,
    CreateInstanceEventWindowResponse (CreateInstanceEventWindowResponse'),
    newCreateInstanceEventWindowResponse,

    -- ** CreateInstanceExportTask
    CreateInstanceExportTask (CreateInstanceExportTask'),
    newCreateInstanceExportTask,
    CreateInstanceExportTaskResponse (CreateInstanceExportTaskResponse'),
    newCreateInstanceExportTaskResponse,

    -- ** CreateInternetGateway
    CreateInternetGateway (CreateInternetGateway'),
    newCreateInternetGateway,
    CreateInternetGatewayResponse (CreateInternetGatewayResponse'),
    newCreateInternetGatewayResponse,

    -- ** CreateIpam
    CreateIpam (CreateIpam'),
    newCreateIpam,
    CreateIpamResponse (CreateIpamResponse'),
    newCreateIpamResponse,

    -- ** CreateIpamPool
    CreateIpamPool (CreateIpamPool'),
    newCreateIpamPool,
    CreateIpamPoolResponse (CreateIpamPoolResponse'),
    newCreateIpamPoolResponse,

    -- ** CreateIpamResourceDiscovery
    CreateIpamResourceDiscovery (CreateIpamResourceDiscovery'),
    newCreateIpamResourceDiscovery,
    CreateIpamResourceDiscoveryResponse (CreateIpamResourceDiscoveryResponse'),
    newCreateIpamResourceDiscoveryResponse,

    -- ** CreateIpamScope
    CreateIpamScope (CreateIpamScope'),
    newCreateIpamScope,
    CreateIpamScopeResponse (CreateIpamScopeResponse'),
    newCreateIpamScopeResponse,

    -- ** CreateKeyPair
    CreateKeyPair (CreateKeyPair'),
    newCreateKeyPair,
    CreateKeyPairResponse (CreateKeyPairResponse'),
    newCreateKeyPairResponse,

    -- ** CreateLaunchTemplate
    CreateLaunchTemplate (CreateLaunchTemplate'),
    newCreateLaunchTemplate,
    CreateLaunchTemplateResponse (CreateLaunchTemplateResponse'),
    newCreateLaunchTemplateResponse,

    -- ** CreateLaunchTemplateVersion
    CreateLaunchTemplateVersion (CreateLaunchTemplateVersion'),
    newCreateLaunchTemplateVersion,
    CreateLaunchTemplateVersionResponse (CreateLaunchTemplateVersionResponse'),
    newCreateLaunchTemplateVersionResponse,

    -- ** CreateLocalGatewayRoute
    CreateLocalGatewayRoute (CreateLocalGatewayRoute'),
    newCreateLocalGatewayRoute,
    CreateLocalGatewayRouteResponse (CreateLocalGatewayRouteResponse'),
    newCreateLocalGatewayRouteResponse,

    -- ** CreateLocalGatewayRouteTable
    CreateLocalGatewayRouteTable (CreateLocalGatewayRouteTable'),
    newCreateLocalGatewayRouteTable,
    CreateLocalGatewayRouteTableResponse (CreateLocalGatewayRouteTableResponse'),
    newCreateLocalGatewayRouteTableResponse,

    -- ** CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation
    CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation (CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation'),
    newCreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation,
    CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse (CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse'),
    newCreateLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse,

    -- ** CreateLocalGatewayRouteTableVpcAssociation
    CreateLocalGatewayRouteTableVpcAssociation (CreateLocalGatewayRouteTableVpcAssociation'),
    newCreateLocalGatewayRouteTableVpcAssociation,
    CreateLocalGatewayRouteTableVpcAssociationResponse (CreateLocalGatewayRouteTableVpcAssociationResponse'),
    newCreateLocalGatewayRouteTableVpcAssociationResponse,

    -- ** CreateManagedPrefixList
    CreateManagedPrefixList (CreateManagedPrefixList'),
    newCreateManagedPrefixList,
    CreateManagedPrefixListResponse (CreateManagedPrefixListResponse'),
    newCreateManagedPrefixListResponse,

    -- ** CreateNatGateway
    CreateNatGateway (CreateNatGateway'),
    newCreateNatGateway,
    CreateNatGatewayResponse (CreateNatGatewayResponse'),
    newCreateNatGatewayResponse,

    -- ** CreateNetworkAcl
    CreateNetworkAcl (CreateNetworkAcl'),
    newCreateNetworkAcl,
    CreateNetworkAclResponse (CreateNetworkAclResponse'),
    newCreateNetworkAclResponse,

    -- ** CreateNetworkAclEntry
    CreateNetworkAclEntry (CreateNetworkAclEntry'),
    newCreateNetworkAclEntry,
    CreateNetworkAclEntryResponse (CreateNetworkAclEntryResponse'),
    newCreateNetworkAclEntryResponse,

    -- ** CreateNetworkInsightsAccessScope
    CreateNetworkInsightsAccessScope (CreateNetworkInsightsAccessScope'),
    newCreateNetworkInsightsAccessScope,
    CreateNetworkInsightsAccessScopeResponse (CreateNetworkInsightsAccessScopeResponse'),
    newCreateNetworkInsightsAccessScopeResponse,

    -- ** CreateNetworkInsightsPath
    CreateNetworkInsightsPath (CreateNetworkInsightsPath'),
    newCreateNetworkInsightsPath,
    CreateNetworkInsightsPathResponse (CreateNetworkInsightsPathResponse'),
    newCreateNetworkInsightsPathResponse,

    -- ** CreateNetworkInterface
    CreateNetworkInterface (CreateNetworkInterface'),
    newCreateNetworkInterface,
    CreateNetworkInterfaceResponse (CreateNetworkInterfaceResponse'),
    newCreateNetworkInterfaceResponse,

    -- ** CreateNetworkInterfacePermission
    CreateNetworkInterfacePermission (CreateNetworkInterfacePermission'),
    newCreateNetworkInterfacePermission,
    CreateNetworkInterfacePermissionResponse (CreateNetworkInterfacePermissionResponse'),
    newCreateNetworkInterfacePermissionResponse,

    -- ** CreatePlacementGroup
    CreatePlacementGroup (CreatePlacementGroup'),
    newCreatePlacementGroup,
    CreatePlacementGroupResponse (CreatePlacementGroupResponse'),
    newCreatePlacementGroupResponse,

    -- ** CreatePublicIpv4Pool
    CreatePublicIpv4Pool (CreatePublicIpv4Pool'),
    newCreatePublicIpv4Pool,
    CreatePublicIpv4PoolResponse (CreatePublicIpv4PoolResponse'),
    newCreatePublicIpv4PoolResponse,

    -- ** CreateReplaceRootVolumeTask
    CreateReplaceRootVolumeTask (CreateReplaceRootVolumeTask'),
    newCreateReplaceRootVolumeTask,
    CreateReplaceRootVolumeTaskResponse (CreateReplaceRootVolumeTaskResponse'),
    newCreateReplaceRootVolumeTaskResponse,

    -- ** CreateReservedInstancesListing
    CreateReservedInstancesListing (CreateReservedInstancesListing'),
    newCreateReservedInstancesListing,
    CreateReservedInstancesListingResponse (CreateReservedInstancesListingResponse'),
    newCreateReservedInstancesListingResponse,

    -- ** CreateRestoreImageTask
    CreateRestoreImageTask (CreateRestoreImageTask'),
    newCreateRestoreImageTask,
    CreateRestoreImageTaskResponse (CreateRestoreImageTaskResponse'),
    newCreateRestoreImageTaskResponse,

    -- ** CreateRoute
    CreateRoute (CreateRoute'),
    newCreateRoute,
    CreateRouteResponse (CreateRouteResponse'),
    newCreateRouteResponse,

    -- ** CreateRouteTable
    CreateRouteTable (CreateRouteTable'),
    newCreateRouteTable,
    CreateRouteTableResponse (CreateRouteTableResponse'),
    newCreateRouteTableResponse,

    -- ** CreateSecurityGroup
    CreateSecurityGroup (CreateSecurityGroup'),
    newCreateSecurityGroup,
    CreateSecurityGroupResponse (CreateSecurityGroupResponse'),
    newCreateSecurityGroupResponse,

    -- ** CreateSnapshot
    CreateSnapshot (CreateSnapshot'),
    newCreateSnapshot,
    Snapshot (Snapshot'),
    newSnapshot,

    -- ** CreateSnapshots
    CreateSnapshots (CreateSnapshots'),
    newCreateSnapshots,
    CreateSnapshotsResponse (CreateSnapshotsResponse'),
    newCreateSnapshotsResponse,

    -- ** CreateSpotDatafeedSubscription
    CreateSpotDatafeedSubscription (CreateSpotDatafeedSubscription'),
    newCreateSpotDatafeedSubscription,
    CreateSpotDatafeedSubscriptionResponse (CreateSpotDatafeedSubscriptionResponse'),
    newCreateSpotDatafeedSubscriptionResponse,

    -- ** CreateStoreImageTask
    CreateStoreImageTask (CreateStoreImageTask'),
    newCreateStoreImageTask,
    CreateStoreImageTaskResponse (CreateStoreImageTaskResponse'),
    newCreateStoreImageTaskResponse,

    -- ** CreateSubnet
    CreateSubnet (CreateSubnet'),
    newCreateSubnet,
    CreateSubnetResponse (CreateSubnetResponse'),
    newCreateSubnetResponse,

    -- ** CreateSubnetCidrReservation
    CreateSubnetCidrReservation (CreateSubnetCidrReservation'),
    newCreateSubnetCidrReservation,
    CreateSubnetCidrReservationResponse (CreateSubnetCidrReservationResponse'),
    newCreateSubnetCidrReservationResponse,

    -- ** CreateTags
    CreateTags (CreateTags'),
    newCreateTags,
    CreateTagsResponse (CreateTagsResponse'),
    newCreateTagsResponse,

    -- ** CreateTrafficMirrorFilter
    CreateTrafficMirrorFilter (CreateTrafficMirrorFilter'),
    newCreateTrafficMirrorFilter,
    CreateTrafficMirrorFilterResponse (CreateTrafficMirrorFilterResponse'),
    newCreateTrafficMirrorFilterResponse,

    -- ** CreateTrafficMirrorFilterRule
    CreateTrafficMirrorFilterRule (CreateTrafficMirrorFilterRule'),
    newCreateTrafficMirrorFilterRule,
    CreateTrafficMirrorFilterRuleResponse (CreateTrafficMirrorFilterRuleResponse'),
    newCreateTrafficMirrorFilterRuleResponse,

    -- ** CreateTrafficMirrorSession
    CreateTrafficMirrorSession (CreateTrafficMirrorSession'),
    newCreateTrafficMirrorSession,
    CreateTrafficMirrorSessionResponse (CreateTrafficMirrorSessionResponse'),
    newCreateTrafficMirrorSessionResponse,

    -- ** CreateTrafficMirrorTarget
    CreateTrafficMirrorTarget (CreateTrafficMirrorTarget'),
    newCreateTrafficMirrorTarget,
    CreateTrafficMirrorTargetResponse (CreateTrafficMirrorTargetResponse'),
    newCreateTrafficMirrorTargetResponse,

    -- ** CreateTransitGateway
    CreateTransitGateway (CreateTransitGateway'),
    newCreateTransitGateway,
    CreateTransitGatewayResponse (CreateTransitGatewayResponse'),
    newCreateTransitGatewayResponse,

    -- ** CreateTransitGatewayConnect
    CreateTransitGatewayConnect (CreateTransitGatewayConnect'),
    newCreateTransitGatewayConnect,
    CreateTransitGatewayConnectResponse (CreateTransitGatewayConnectResponse'),
    newCreateTransitGatewayConnectResponse,

    -- ** CreateTransitGatewayConnectPeer
    CreateTransitGatewayConnectPeer (CreateTransitGatewayConnectPeer'),
    newCreateTransitGatewayConnectPeer,
    CreateTransitGatewayConnectPeerResponse (CreateTransitGatewayConnectPeerResponse'),
    newCreateTransitGatewayConnectPeerResponse,

    -- ** CreateTransitGatewayMulticastDomain
    CreateTransitGatewayMulticastDomain (CreateTransitGatewayMulticastDomain'),
    newCreateTransitGatewayMulticastDomain,
    CreateTransitGatewayMulticastDomainResponse (CreateTransitGatewayMulticastDomainResponse'),
    newCreateTransitGatewayMulticastDomainResponse,

    -- ** CreateTransitGatewayPeeringAttachment
    CreateTransitGatewayPeeringAttachment (CreateTransitGatewayPeeringAttachment'),
    newCreateTransitGatewayPeeringAttachment,
    CreateTransitGatewayPeeringAttachmentResponse (CreateTransitGatewayPeeringAttachmentResponse'),
    newCreateTransitGatewayPeeringAttachmentResponse,

    -- ** CreateTransitGatewayPolicyTable
    CreateTransitGatewayPolicyTable (CreateTransitGatewayPolicyTable'),
    newCreateTransitGatewayPolicyTable,
    CreateTransitGatewayPolicyTableResponse (CreateTransitGatewayPolicyTableResponse'),
    newCreateTransitGatewayPolicyTableResponse,

    -- ** CreateTransitGatewayPrefixListReference
    CreateTransitGatewayPrefixListReference (CreateTransitGatewayPrefixListReference'),
    newCreateTransitGatewayPrefixListReference,
    CreateTransitGatewayPrefixListReferenceResponse (CreateTransitGatewayPrefixListReferenceResponse'),
    newCreateTransitGatewayPrefixListReferenceResponse,

    -- ** CreateTransitGatewayRoute
    CreateTransitGatewayRoute (CreateTransitGatewayRoute'),
    newCreateTransitGatewayRoute,
    CreateTransitGatewayRouteResponse (CreateTransitGatewayRouteResponse'),
    newCreateTransitGatewayRouteResponse,

    -- ** CreateTransitGatewayRouteTable
    CreateTransitGatewayRouteTable (CreateTransitGatewayRouteTable'),
    newCreateTransitGatewayRouteTable,
    CreateTransitGatewayRouteTableResponse (CreateTransitGatewayRouteTableResponse'),
    newCreateTransitGatewayRouteTableResponse,

    -- ** CreateTransitGatewayRouteTableAnnouncement
    CreateTransitGatewayRouteTableAnnouncement (CreateTransitGatewayRouteTableAnnouncement'),
    newCreateTransitGatewayRouteTableAnnouncement,
    CreateTransitGatewayRouteTableAnnouncementResponse (CreateTransitGatewayRouteTableAnnouncementResponse'),
    newCreateTransitGatewayRouteTableAnnouncementResponse,

    -- ** CreateTransitGatewayVpcAttachment
    CreateTransitGatewayVpcAttachment (CreateTransitGatewayVpcAttachment'),
    newCreateTransitGatewayVpcAttachment,
    CreateTransitGatewayVpcAttachmentResponse (CreateTransitGatewayVpcAttachmentResponse'),
    newCreateTransitGatewayVpcAttachmentResponse,

    -- ** CreateVerifiedAccessEndpoint
    CreateVerifiedAccessEndpoint (CreateVerifiedAccessEndpoint'),
    newCreateVerifiedAccessEndpoint,
    CreateVerifiedAccessEndpointResponse (CreateVerifiedAccessEndpointResponse'),
    newCreateVerifiedAccessEndpointResponse,

    -- ** CreateVerifiedAccessGroup
    CreateVerifiedAccessGroup (CreateVerifiedAccessGroup'),
    newCreateVerifiedAccessGroup,
    CreateVerifiedAccessGroupResponse (CreateVerifiedAccessGroupResponse'),
    newCreateVerifiedAccessGroupResponse,

    -- ** CreateVerifiedAccessInstance
    CreateVerifiedAccessInstance (CreateVerifiedAccessInstance'),
    newCreateVerifiedAccessInstance,
    CreateVerifiedAccessInstanceResponse (CreateVerifiedAccessInstanceResponse'),
    newCreateVerifiedAccessInstanceResponse,

    -- ** CreateVerifiedAccessTrustProvider
    CreateVerifiedAccessTrustProvider (CreateVerifiedAccessTrustProvider'),
    newCreateVerifiedAccessTrustProvider,
    CreateVerifiedAccessTrustProviderResponse (CreateVerifiedAccessTrustProviderResponse'),
    newCreateVerifiedAccessTrustProviderResponse,

    -- ** CreateVolume
    CreateVolume (CreateVolume'),
    newCreateVolume,
    Volume (Volume'),
    newVolume,

    -- ** CreateVpc
    CreateVpc (CreateVpc'),
    newCreateVpc,
    CreateVpcResponse (CreateVpcResponse'),
    newCreateVpcResponse,

    -- ** CreateVpcEndpoint
    CreateVpcEndpoint (CreateVpcEndpoint'),
    newCreateVpcEndpoint,
    CreateVpcEndpointResponse (CreateVpcEndpointResponse'),
    newCreateVpcEndpointResponse,

    -- ** CreateVpcEndpointConnectionNotification
    CreateVpcEndpointConnectionNotification (CreateVpcEndpointConnectionNotification'),
    newCreateVpcEndpointConnectionNotification,
    CreateVpcEndpointConnectionNotificationResponse (CreateVpcEndpointConnectionNotificationResponse'),
    newCreateVpcEndpointConnectionNotificationResponse,

    -- ** CreateVpcEndpointServiceConfiguration
    CreateVpcEndpointServiceConfiguration (CreateVpcEndpointServiceConfiguration'),
    newCreateVpcEndpointServiceConfiguration,
    CreateVpcEndpointServiceConfigurationResponse (CreateVpcEndpointServiceConfigurationResponse'),
    newCreateVpcEndpointServiceConfigurationResponse,

    -- ** CreateVpcPeeringConnection
    CreateVpcPeeringConnection (CreateVpcPeeringConnection'),
    newCreateVpcPeeringConnection,
    CreateVpcPeeringConnectionResponse (CreateVpcPeeringConnectionResponse'),
    newCreateVpcPeeringConnectionResponse,

    -- ** CreateVpnConnection
    CreateVpnConnection (CreateVpnConnection'),
    newCreateVpnConnection,
    CreateVpnConnectionResponse (CreateVpnConnectionResponse'),
    newCreateVpnConnectionResponse,

    -- ** CreateVpnConnectionRoute
    CreateVpnConnectionRoute (CreateVpnConnectionRoute'),
    newCreateVpnConnectionRoute,
    CreateVpnConnectionRouteResponse (CreateVpnConnectionRouteResponse'),
    newCreateVpnConnectionRouteResponse,

    -- ** CreateVpnGateway
    CreateVpnGateway (CreateVpnGateway'),
    newCreateVpnGateway,
    CreateVpnGatewayResponse (CreateVpnGatewayResponse'),
    newCreateVpnGatewayResponse,

    -- ** DeleteCarrierGateway
    DeleteCarrierGateway (DeleteCarrierGateway'),
    newDeleteCarrierGateway,
    DeleteCarrierGatewayResponse (DeleteCarrierGatewayResponse'),
    newDeleteCarrierGatewayResponse,

    -- ** DeleteClientVpnEndpoint
    DeleteClientVpnEndpoint (DeleteClientVpnEndpoint'),
    newDeleteClientVpnEndpoint,
    DeleteClientVpnEndpointResponse (DeleteClientVpnEndpointResponse'),
    newDeleteClientVpnEndpointResponse,

    -- ** DeleteClientVpnRoute
    DeleteClientVpnRoute (DeleteClientVpnRoute'),
    newDeleteClientVpnRoute,
    DeleteClientVpnRouteResponse (DeleteClientVpnRouteResponse'),
    newDeleteClientVpnRouteResponse,

    -- ** DeleteCoipCidr
    DeleteCoipCidr (DeleteCoipCidr'),
    newDeleteCoipCidr,
    DeleteCoipCidrResponse (DeleteCoipCidrResponse'),
    newDeleteCoipCidrResponse,

    -- ** DeleteCoipPool
    DeleteCoipPool (DeleteCoipPool'),
    newDeleteCoipPool,
    DeleteCoipPoolResponse (DeleteCoipPoolResponse'),
    newDeleteCoipPoolResponse,

    -- ** DeleteCustomerGateway
    DeleteCustomerGateway (DeleteCustomerGateway'),
    newDeleteCustomerGateway,
    DeleteCustomerGatewayResponse (DeleteCustomerGatewayResponse'),
    newDeleteCustomerGatewayResponse,

    -- ** DeleteDhcpOptions
    DeleteDhcpOptions (DeleteDhcpOptions'),
    newDeleteDhcpOptions,
    DeleteDhcpOptionsResponse (DeleteDhcpOptionsResponse'),
    newDeleteDhcpOptionsResponse,

    -- ** DeleteEgressOnlyInternetGateway
    DeleteEgressOnlyInternetGateway (DeleteEgressOnlyInternetGateway'),
    newDeleteEgressOnlyInternetGateway,
    DeleteEgressOnlyInternetGatewayResponse (DeleteEgressOnlyInternetGatewayResponse'),
    newDeleteEgressOnlyInternetGatewayResponse,

    -- ** DeleteFleets
    DeleteFleets (DeleteFleets'),
    newDeleteFleets,
    DeleteFleetsResponse (DeleteFleetsResponse'),
    newDeleteFleetsResponse,

    -- ** DeleteFlowLogs
    DeleteFlowLogs (DeleteFlowLogs'),
    newDeleteFlowLogs,
    DeleteFlowLogsResponse (DeleteFlowLogsResponse'),
    newDeleteFlowLogsResponse,

    -- ** DeleteFpgaImage
    DeleteFpgaImage (DeleteFpgaImage'),
    newDeleteFpgaImage,
    DeleteFpgaImageResponse (DeleteFpgaImageResponse'),
    newDeleteFpgaImageResponse,

    -- ** DeleteInstanceConnectEndpoint
    DeleteInstanceConnectEndpoint (DeleteInstanceConnectEndpoint'),
    newDeleteInstanceConnectEndpoint,
    DeleteInstanceConnectEndpointResponse (DeleteInstanceConnectEndpointResponse'),
    newDeleteInstanceConnectEndpointResponse,

    -- ** DeleteInstanceEventWindow
    DeleteInstanceEventWindow (DeleteInstanceEventWindow'),
    newDeleteInstanceEventWindow,
    DeleteInstanceEventWindowResponse (DeleteInstanceEventWindowResponse'),
    newDeleteInstanceEventWindowResponse,

    -- ** DeleteInternetGateway
    DeleteInternetGateway (DeleteInternetGateway'),
    newDeleteInternetGateway,
    DeleteInternetGatewayResponse (DeleteInternetGatewayResponse'),
    newDeleteInternetGatewayResponse,

    -- ** DeleteIpam
    DeleteIpam (DeleteIpam'),
    newDeleteIpam,
    DeleteIpamResponse (DeleteIpamResponse'),
    newDeleteIpamResponse,

    -- ** DeleteIpamPool
    DeleteIpamPool (DeleteIpamPool'),
    newDeleteIpamPool,
    DeleteIpamPoolResponse (DeleteIpamPoolResponse'),
    newDeleteIpamPoolResponse,

    -- ** DeleteIpamResourceDiscovery
    DeleteIpamResourceDiscovery (DeleteIpamResourceDiscovery'),
    newDeleteIpamResourceDiscovery,
    DeleteIpamResourceDiscoveryResponse (DeleteIpamResourceDiscoveryResponse'),
    newDeleteIpamResourceDiscoveryResponse,

    -- ** DeleteIpamScope
    DeleteIpamScope (DeleteIpamScope'),
    newDeleteIpamScope,
    DeleteIpamScopeResponse (DeleteIpamScopeResponse'),
    newDeleteIpamScopeResponse,

    -- ** DeleteKeyPair
    DeleteKeyPair (DeleteKeyPair'),
    newDeleteKeyPair,
    DeleteKeyPairResponse (DeleteKeyPairResponse'),
    newDeleteKeyPairResponse,

    -- ** DeleteLaunchTemplate
    DeleteLaunchTemplate (DeleteLaunchTemplate'),
    newDeleteLaunchTemplate,
    DeleteLaunchTemplateResponse (DeleteLaunchTemplateResponse'),
    newDeleteLaunchTemplateResponse,

    -- ** DeleteLaunchTemplateVersions
    DeleteLaunchTemplateVersions (DeleteLaunchTemplateVersions'),
    newDeleteLaunchTemplateVersions,
    DeleteLaunchTemplateVersionsResponse (DeleteLaunchTemplateVersionsResponse'),
    newDeleteLaunchTemplateVersionsResponse,

    -- ** DeleteLocalGatewayRoute
    DeleteLocalGatewayRoute (DeleteLocalGatewayRoute'),
    newDeleteLocalGatewayRoute,
    DeleteLocalGatewayRouteResponse (DeleteLocalGatewayRouteResponse'),
    newDeleteLocalGatewayRouteResponse,

    -- ** DeleteLocalGatewayRouteTable
    DeleteLocalGatewayRouteTable (DeleteLocalGatewayRouteTable'),
    newDeleteLocalGatewayRouteTable,
    DeleteLocalGatewayRouteTableResponse (DeleteLocalGatewayRouteTableResponse'),
    newDeleteLocalGatewayRouteTableResponse,

    -- ** DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation
    DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation (DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation'),
    newDeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation,
    DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse (DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse'),
    newDeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse,

    -- ** DeleteLocalGatewayRouteTableVpcAssociation
    DeleteLocalGatewayRouteTableVpcAssociation (DeleteLocalGatewayRouteTableVpcAssociation'),
    newDeleteLocalGatewayRouteTableVpcAssociation,
    DeleteLocalGatewayRouteTableVpcAssociationResponse (DeleteLocalGatewayRouteTableVpcAssociationResponse'),
    newDeleteLocalGatewayRouteTableVpcAssociationResponse,

    -- ** DeleteManagedPrefixList
    DeleteManagedPrefixList (DeleteManagedPrefixList'),
    newDeleteManagedPrefixList,
    DeleteManagedPrefixListResponse (DeleteManagedPrefixListResponse'),
    newDeleteManagedPrefixListResponse,

    -- ** DeleteNatGateway
    DeleteNatGateway (DeleteNatGateway'),
    newDeleteNatGateway,
    DeleteNatGatewayResponse (DeleteNatGatewayResponse'),
    newDeleteNatGatewayResponse,

    -- ** DeleteNetworkAcl
    DeleteNetworkAcl (DeleteNetworkAcl'),
    newDeleteNetworkAcl,
    DeleteNetworkAclResponse (DeleteNetworkAclResponse'),
    newDeleteNetworkAclResponse,

    -- ** DeleteNetworkAclEntry
    DeleteNetworkAclEntry (DeleteNetworkAclEntry'),
    newDeleteNetworkAclEntry,
    DeleteNetworkAclEntryResponse (DeleteNetworkAclEntryResponse'),
    newDeleteNetworkAclEntryResponse,

    -- ** DeleteNetworkInsightsAccessScope
    DeleteNetworkInsightsAccessScope (DeleteNetworkInsightsAccessScope'),
    newDeleteNetworkInsightsAccessScope,
    DeleteNetworkInsightsAccessScopeResponse (DeleteNetworkInsightsAccessScopeResponse'),
    newDeleteNetworkInsightsAccessScopeResponse,

    -- ** DeleteNetworkInsightsAccessScopeAnalysis
    DeleteNetworkInsightsAccessScopeAnalysis (DeleteNetworkInsightsAccessScopeAnalysis'),
    newDeleteNetworkInsightsAccessScopeAnalysis,
    DeleteNetworkInsightsAccessScopeAnalysisResponse (DeleteNetworkInsightsAccessScopeAnalysisResponse'),
    newDeleteNetworkInsightsAccessScopeAnalysisResponse,

    -- ** DeleteNetworkInsightsAnalysis
    DeleteNetworkInsightsAnalysis (DeleteNetworkInsightsAnalysis'),
    newDeleteNetworkInsightsAnalysis,
    DeleteNetworkInsightsAnalysisResponse (DeleteNetworkInsightsAnalysisResponse'),
    newDeleteNetworkInsightsAnalysisResponse,

    -- ** DeleteNetworkInsightsPath
    DeleteNetworkInsightsPath (DeleteNetworkInsightsPath'),
    newDeleteNetworkInsightsPath,
    DeleteNetworkInsightsPathResponse (DeleteNetworkInsightsPathResponse'),
    newDeleteNetworkInsightsPathResponse,

    -- ** DeleteNetworkInterface
    DeleteNetworkInterface (DeleteNetworkInterface'),
    newDeleteNetworkInterface,
    DeleteNetworkInterfaceResponse (DeleteNetworkInterfaceResponse'),
    newDeleteNetworkInterfaceResponse,

    -- ** DeleteNetworkInterfacePermission
    DeleteNetworkInterfacePermission (DeleteNetworkInterfacePermission'),
    newDeleteNetworkInterfacePermission,
    DeleteNetworkInterfacePermissionResponse (DeleteNetworkInterfacePermissionResponse'),
    newDeleteNetworkInterfacePermissionResponse,

    -- ** DeletePlacementGroup
    DeletePlacementGroup (DeletePlacementGroup'),
    newDeletePlacementGroup,
    DeletePlacementGroupResponse (DeletePlacementGroupResponse'),
    newDeletePlacementGroupResponse,

    -- ** DeletePublicIpv4Pool
    DeletePublicIpv4Pool (DeletePublicIpv4Pool'),
    newDeletePublicIpv4Pool,
    DeletePublicIpv4PoolResponse (DeletePublicIpv4PoolResponse'),
    newDeletePublicIpv4PoolResponse,

    -- ** DeleteQueuedReservedInstances
    DeleteQueuedReservedInstances (DeleteQueuedReservedInstances'),
    newDeleteQueuedReservedInstances,
    DeleteQueuedReservedInstancesResponse (DeleteQueuedReservedInstancesResponse'),
    newDeleteQueuedReservedInstancesResponse,

    -- ** DeleteRoute
    DeleteRoute (DeleteRoute'),
    newDeleteRoute,
    DeleteRouteResponse (DeleteRouteResponse'),
    newDeleteRouteResponse,

    -- ** DeleteRouteTable
    DeleteRouteTable (DeleteRouteTable'),
    newDeleteRouteTable,
    DeleteRouteTableResponse (DeleteRouteTableResponse'),
    newDeleteRouteTableResponse,

    -- ** DeleteSecurityGroup
    DeleteSecurityGroup (DeleteSecurityGroup'),
    newDeleteSecurityGroup,
    DeleteSecurityGroupResponse (DeleteSecurityGroupResponse'),
    newDeleteSecurityGroupResponse,

    -- ** DeleteSnapshot
    DeleteSnapshot (DeleteSnapshot'),
    newDeleteSnapshot,
    DeleteSnapshotResponse (DeleteSnapshotResponse'),
    newDeleteSnapshotResponse,

    -- ** DeleteSpotDatafeedSubscription
    DeleteSpotDatafeedSubscription (DeleteSpotDatafeedSubscription'),
    newDeleteSpotDatafeedSubscription,
    DeleteSpotDatafeedSubscriptionResponse (DeleteSpotDatafeedSubscriptionResponse'),
    newDeleteSpotDatafeedSubscriptionResponse,

    -- ** DeleteSubnet
    DeleteSubnet (DeleteSubnet'),
    newDeleteSubnet,
    DeleteSubnetResponse (DeleteSubnetResponse'),
    newDeleteSubnetResponse,

    -- ** DeleteSubnetCidrReservation
    DeleteSubnetCidrReservation (DeleteSubnetCidrReservation'),
    newDeleteSubnetCidrReservation,
    DeleteSubnetCidrReservationResponse (DeleteSubnetCidrReservationResponse'),
    newDeleteSubnetCidrReservationResponse,

    -- ** DeleteTags
    DeleteTags (DeleteTags'),
    newDeleteTags,
    DeleteTagsResponse (DeleteTagsResponse'),
    newDeleteTagsResponse,

    -- ** DeleteTrafficMirrorFilter
    DeleteTrafficMirrorFilter (DeleteTrafficMirrorFilter'),
    newDeleteTrafficMirrorFilter,
    DeleteTrafficMirrorFilterResponse (DeleteTrafficMirrorFilterResponse'),
    newDeleteTrafficMirrorFilterResponse,

    -- ** DeleteTrafficMirrorFilterRule
    DeleteTrafficMirrorFilterRule (DeleteTrafficMirrorFilterRule'),
    newDeleteTrafficMirrorFilterRule,
    DeleteTrafficMirrorFilterRuleResponse (DeleteTrafficMirrorFilterRuleResponse'),
    newDeleteTrafficMirrorFilterRuleResponse,

    -- ** DeleteTrafficMirrorSession
    DeleteTrafficMirrorSession (DeleteTrafficMirrorSession'),
    newDeleteTrafficMirrorSession,
    DeleteTrafficMirrorSessionResponse (DeleteTrafficMirrorSessionResponse'),
    newDeleteTrafficMirrorSessionResponse,

    -- ** DeleteTrafficMirrorTarget
    DeleteTrafficMirrorTarget (DeleteTrafficMirrorTarget'),
    newDeleteTrafficMirrorTarget,
    DeleteTrafficMirrorTargetResponse (DeleteTrafficMirrorTargetResponse'),
    newDeleteTrafficMirrorTargetResponse,

    -- ** DeleteTransitGateway
    DeleteTransitGateway (DeleteTransitGateway'),
    newDeleteTransitGateway,
    DeleteTransitGatewayResponse (DeleteTransitGatewayResponse'),
    newDeleteTransitGatewayResponse,

    -- ** DeleteTransitGatewayConnect
    DeleteTransitGatewayConnect (DeleteTransitGatewayConnect'),
    newDeleteTransitGatewayConnect,
    DeleteTransitGatewayConnectResponse (DeleteTransitGatewayConnectResponse'),
    newDeleteTransitGatewayConnectResponse,

    -- ** DeleteTransitGatewayConnectPeer
    DeleteTransitGatewayConnectPeer (DeleteTransitGatewayConnectPeer'),
    newDeleteTransitGatewayConnectPeer,
    DeleteTransitGatewayConnectPeerResponse (DeleteTransitGatewayConnectPeerResponse'),
    newDeleteTransitGatewayConnectPeerResponse,

    -- ** DeleteTransitGatewayMulticastDomain
    DeleteTransitGatewayMulticastDomain (DeleteTransitGatewayMulticastDomain'),
    newDeleteTransitGatewayMulticastDomain,
    DeleteTransitGatewayMulticastDomainResponse (DeleteTransitGatewayMulticastDomainResponse'),
    newDeleteTransitGatewayMulticastDomainResponse,

    -- ** DeleteTransitGatewayPeeringAttachment
    DeleteTransitGatewayPeeringAttachment (DeleteTransitGatewayPeeringAttachment'),
    newDeleteTransitGatewayPeeringAttachment,
    DeleteTransitGatewayPeeringAttachmentResponse (DeleteTransitGatewayPeeringAttachmentResponse'),
    newDeleteTransitGatewayPeeringAttachmentResponse,

    -- ** DeleteTransitGatewayPolicyTable
    DeleteTransitGatewayPolicyTable (DeleteTransitGatewayPolicyTable'),
    newDeleteTransitGatewayPolicyTable,
    DeleteTransitGatewayPolicyTableResponse (DeleteTransitGatewayPolicyTableResponse'),
    newDeleteTransitGatewayPolicyTableResponse,

    -- ** DeleteTransitGatewayPrefixListReference
    DeleteTransitGatewayPrefixListReference (DeleteTransitGatewayPrefixListReference'),
    newDeleteTransitGatewayPrefixListReference,
    DeleteTransitGatewayPrefixListReferenceResponse (DeleteTransitGatewayPrefixListReferenceResponse'),
    newDeleteTransitGatewayPrefixListReferenceResponse,

    -- ** DeleteTransitGatewayRoute
    DeleteTransitGatewayRoute (DeleteTransitGatewayRoute'),
    newDeleteTransitGatewayRoute,
    DeleteTransitGatewayRouteResponse (DeleteTransitGatewayRouteResponse'),
    newDeleteTransitGatewayRouteResponse,

    -- ** DeleteTransitGatewayRouteTable
    DeleteTransitGatewayRouteTable (DeleteTransitGatewayRouteTable'),
    newDeleteTransitGatewayRouteTable,
    DeleteTransitGatewayRouteTableResponse (DeleteTransitGatewayRouteTableResponse'),
    newDeleteTransitGatewayRouteTableResponse,

    -- ** DeleteTransitGatewayRouteTableAnnouncement
    DeleteTransitGatewayRouteTableAnnouncement (DeleteTransitGatewayRouteTableAnnouncement'),
    newDeleteTransitGatewayRouteTableAnnouncement,
    DeleteTransitGatewayRouteTableAnnouncementResponse (DeleteTransitGatewayRouteTableAnnouncementResponse'),
    newDeleteTransitGatewayRouteTableAnnouncementResponse,

    -- ** DeleteTransitGatewayVpcAttachment
    DeleteTransitGatewayVpcAttachment (DeleteTransitGatewayVpcAttachment'),
    newDeleteTransitGatewayVpcAttachment,
    DeleteTransitGatewayVpcAttachmentResponse (DeleteTransitGatewayVpcAttachmentResponse'),
    newDeleteTransitGatewayVpcAttachmentResponse,

    -- ** DeleteVerifiedAccessEndpoint
    DeleteVerifiedAccessEndpoint (DeleteVerifiedAccessEndpoint'),
    newDeleteVerifiedAccessEndpoint,
    DeleteVerifiedAccessEndpointResponse (DeleteVerifiedAccessEndpointResponse'),
    newDeleteVerifiedAccessEndpointResponse,

    -- ** DeleteVerifiedAccessGroup
    DeleteVerifiedAccessGroup (DeleteVerifiedAccessGroup'),
    newDeleteVerifiedAccessGroup,
    DeleteVerifiedAccessGroupResponse (DeleteVerifiedAccessGroupResponse'),
    newDeleteVerifiedAccessGroupResponse,

    -- ** DeleteVerifiedAccessInstance
    DeleteVerifiedAccessInstance (DeleteVerifiedAccessInstance'),
    newDeleteVerifiedAccessInstance,
    DeleteVerifiedAccessInstanceResponse (DeleteVerifiedAccessInstanceResponse'),
    newDeleteVerifiedAccessInstanceResponse,

    -- ** DeleteVerifiedAccessTrustProvider
    DeleteVerifiedAccessTrustProvider (DeleteVerifiedAccessTrustProvider'),
    newDeleteVerifiedAccessTrustProvider,
    DeleteVerifiedAccessTrustProviderResponse (DeleteVerifiedAccessTrustProviderResponse'),
    newDeleteVerifiedAccessTrustProviderResponse,

    -- ** DeleteVolume
    DeleteVolume (DeleteVolume'),
    newDeleteVolume,
    DeleteVolumeResponse (DeleteVolumeResponse'),
    newDeleteVolumeResponse,

    -- ** DeleteVpc
    DeleteVpc (DeleteVpc'),
    newDeleteVpc,
    DeleteVpcResponse (DeleteVpcResponse'),
    newDeleteVpcResponse,

    -- ** DeleteVpcEndpointConnectionNotifications
    DeleteVpcEndpointConnectionNotifications (DeleteVpcEndpointConnectionNotifications'),
    newDeleteVpcEndpointConnectionNotifications,
    DeleteVpcEndpointConnectionNotificationsResponse (DeleteVpcEndpointConnectionNotificationsResponse'),
    newDeleteVpcEndpointConnectionNotificationsResponse,

    -- ** DeleteVpcEndpointServiceConfigurations
    DeleteVpcEndpointServiceConfigurations (DeleteVpcEndpointServiceConfigurations'),
    newDeleteVpcEndpointServiceConfigurations,
    DeleteVpcEndpointServiceConfigurationsResponse (DeleteVpcEndpointServiceConfigurationsResponse'),
    newDeleteVpcEndpointServiceConfigurationsResponse,

    -- ** DeleteVpcEndpoints
    DeleteVpcEndpoints (DeleteVpcEndpoints'),
    newDeleteVpcEndpoints,
    DeleteVpcEndpointsResponse (DeleteVpcEndpointsResponse'),
    newDeleteVpcEndpointsResponse,

    -- ** DeleteVpcPeeringConnection
    DeleteVpcPeeringConnection (DeleteVpcPeeringConnection'),
    newDeleteVpcPeeringConnection,
    DeleteVpcPeeringConnectionResponse (DeleteVpcPeeringConnectionResponse'),
    newDeleteVpcPeeringConnectionResponse,

    -- ** DeleteVpnConnection
    DeleteVpnConnection (DeleteVpnConnection'),
    newDeleteVpnConnection,
    DeleteVpnConnectionResponse (DeleteVpnConnectionResponse'),
    newDeleteVpnConnectionResponse,

    -- ** DeleteVpnConnectionRoute
    DeleteVpnConnectionRoute (DeleteVpnConnectionRoute'),
    newDeleteVpnConnectionRoute,
    DeleteVpnConnectionRouteResponse (DeleteVpnConnectionRouteResponse'),
    newDeleteVpnConnectionRouteResponse,

    -- ** DeleteVpnGateway
    DeleteVpnGateway (DeleteVpnGateway'),
    newDeleteVpnGateway,
    DeleteVpnGatewayResponse (DeleteVpnGatewayResponse'),
    newDeleteVpnGatewayResponse,

    -- ** DeprovisionByoipCidr
    DeprovisionByoipCidr (DeprovisionByoipCidr'),
    newDeprovisionByoipCidr,
    DeprovisionByoipCidrResponse (DeprovisionByoipCidrResponse'),
    newDeprovisionByoipCidrResponse,

    -- ** DeprovisionIpamPoolCidr
    DeprovisionIpamPoolCidr (DeprovisionIpamPoolCidr'),
    newDeprovisionIpamPoolCidr,
    DeprovisionIpamPoolCidrResponse (DeprovisionIpamPoolCidrResponse'),
    newDeprovisionIpamPoolCidrResponse,

    -- ** DeprovisionPublicIpv4PoolCidr
    DeprovisionPublicIpv4PoolCidr (DeprovisionPublicIpv4PoolCidr'),
    newDeprovisionPublicIpv4PoolCidr,
    DeprovisionPublicIpv4PoolCidrResponse (DeprovisionPublicIpv4PoolCidrResponse'),
    newDeprovisionPublicIpv4PoolCidrResponse,

    -- ** DeregisterImage
    DeregisterImage (DeregisterImage'),
    newDeregisterImage,
    DeregisterImageResponse (DeregisterImageResponse'),
    newDeregisterImageResponse,

    -- ** DeregisterInstanceEventNotificationAttributes
    DeregisterInstanceEventNotificationAttributes (DeregisterInstanceEventNotificationAttributes'),
    newDeregisterInstanceEventNotificationAttributes,
    DeregisterInstanceEventNotificationAttributesResponse (DeregisterInstanceEventNotificationAttributesResponse'),
    newDeregisterInstanceEventNotificationAttributesResponse,

    -- ** DeregisterTransitGatewayMulticastGroupMembers
    DeregisterTransitGatewayMulticastGroupMembers (DeregisterTransitGatewayMulticastGroupMembers'),
    newDeregisterTransitGatewayMulticastGroupMembers,
    DeregisterTransitGatewayMulticastGroupMembersResponse (DeregisterTransitGatewayMulticastGroupMembersResponse'),
    newDeregisterTransitGatewayMulticastGroupMembersResponse,

    -- ** DeregisterTransitGatewayMulticastGroupSources
    DeregisterTransitGatewayMulticastGroupSources (DeregisterTransitGatewayMulticastGroupSources'),
    newDeregisterTransitGatewayMulticastGroupSources,
    DeregisterTransitGatewayMulticastGroupSourcesResponse (DeregisterTransitGatewayMulticastGroupSourcesResponse'),
    newDeregisterTransitGatewayMulticastGroupSourcesResponse,

    -- ** DescribeAccountAttributes
    DescribeAccountAttributes (DescribeAccountAttributes'),
    newDescribeAccountAttributes,
    DescribeAccountAttributesResponse (DescribeAccountAttributesResponse'),
    newDescribeAccountAttributesResponse,

    -- ** DescribeAddressTransfers (Paginated)
    DescribeAddressTransfers (DescribeAddressTransfers'),
    newDescribeAddressTransfers,
    DescribeAddressTransfersResponse (DescribeAddressTransfersResponse'),
    newDescribeAddressTransfersResponse,

    -- ** DescribeAddresses
    DescribeAddresses (DescribeAddresses'),
    newDescribeAddresses,
    DescribeAddressesResponse (DescribeAddressesResponse'),
    newDescribeAddressesResponse,

    -- ** DescribeAddressesAttribute (Paginated)
    DescribeAddressesAttribute (DescribeAddressesAttribute'),
    newDescribeAddressesAttribute,
    DescribeAddressesAttributeResponse (DescribeAddressesAttributeResponse'),
    newDescribeAddressesAttributeResponse,

    -- ** DescribeAggregateIdFormat
    DescribeAggregateIdFormat (DescribeAggregateIdFormat'),
    newDescribeAggregateIdFormat,
    DescribeAggregateIdFormatResponse (DescribeAggregateIdFormatResponse'),
    newDescribeAggregateIdFormatResponse,

    -- ** DescribeAvailabilityZones
    DescribeAvailabilityZones (DescribeAvailabilityZones'),
    newDescribeAvailabilityZones,
    DescribeAvailabilityZonesResponse (DescribeAvailabilityZonesResponse'),
    newDescribeAvailabilityZonesResponse,

    -- ** DescribeAwsNetworkPerformanceMetricSubscriptions (Paginated)
    DescribeAwsNetworkPerformanceMetricSubscriptions (DescribeAwsNetworkPerformanceMetricSubscriptions'),
    newDescribeAwsNetworkPerformanceMetricSubscriptions,
    DescribeAwsNetworkPerformanceMetricSubscriptionsResponse (DescribeAwsNetworkPerformanceMetricSubscriptionsResponse'),
    newDescribeAwsNetworkPerformanceMetricSubscriptionsResponse,

    -- ** DescribeBundleTasks
    DescribeBundleTasks (DescribeBundleTasks'),
    newDescribeBundleTasks,
    DescribeBundleTasksResponse (DescribeBundleTasksResponse'),
    newDescribeBundleTasksResponse,

    -- ** DescribeByoipCidrs (Paginated)
    DescribeByoipCidrs (DescribeByoipCidrs'),
    newDescribeByoipCidrs,
    DescribeByoipCidrsResponse (DescribeByoipCidrsResponse'),
    newDescribeByoipCidrsResponse,

    -- ** DescribeCapacityReservationFleets (Paginated)
    DescribeCapacityReservationFleets (DescribeCapacityReservationFleets'),
    newDescribeCapacityReservationFleets,
    DescribeCapacityReservationFleetsResponse (DescribeCapacityReservationFleetsResponse'),
    newDescribeCapacityReservationFleetsResponse,

    -- ** DescribeCapacityReservations (Paginated)
    DescribeCapacityReservations (DescribeCapacityReservations'),
    newDescribeCapacityReservations,
    DescribeCapacityReservationsResponse (DescribeCapacityReservationsResponse'),
    newDescribeCapacityReservationsResponse,

    -- ** DescribeCarrierGateways (Paginated)
    DescribeCarrierGateways (DescribeCarrierGateways'),
    newDescribeCarrierGateways,
    DescribeCarrierGatewaysResponse (DescribeCarrierGatewaysResponse'),
    newDescribeCarrierGatewaysResponse,

    -- ** DescribeClassicLinkInstances (Paginated)
    DescribeClassicLinkInstances (DescribeClassicLinkInstances'),
    newDescribeClassicLinkInstances,
    DescribeClassicLinkInstancesResponse (DescribeClassicLinkInstancesResponse'),
    newDescribeClassicLinkInstancesResponse,

    -- ** DescribeClientVpnAuthorizationRules (Paginated)
    DescribeClientVpnAuthorizationRules (DescribeClientVpnAuthorizationRules'),
    newDescribeClientVpnAuthorizationRules,
    DescribeClientVpnAuthorizationRulesResponse (DescribeClientVpnAuthorizationRulesResponse'),
    newDescribeClientVpnAuthorizationRulesResponse,

    -- ** DescribeClientVpnConnections (Paginated)
    DescribeClientVpnConnections (DescribeClientVpnConnections'),
    newDescribeClientVpnConnections,
    DescribeClientVpnConnectionsResponse (DescribeClientVpnConnectionsResponse'),
    newDescribeClientVpnConnectionsResponse,

    -- ** DescribeClientVpnEndpoints (Paginated)
    DescribeClientVpnEndpoints (DescribeClientVpnEndpoints'),
    newDescribeClientVpnEndpoints,
    DescribeClientVpnEndpointsResponse (DescribeClientVpnEndpointsResponse'),
    newDescribeClientVpnEndpointsResponse,

    -- ** DescribeClientVpnRoutes (Paginated)
    DescribeClientVpnRoutes (DescribeClientVpnRoutes'),
    newDescribeClientVpnRoutes,
    DescribeClientVpnRoutesResponse (DescribeClientVpnRoutesResponse'),
    newDescribeClientVpnRoutesResponse,

    -- ** DescribeClientVpnTargetNetworks (Paginated)
    DescribeClientVpnTargetNetworks (DescribeClientVpnTargetNetworks'),
    newDescribeClientVpnTargetNetworks,
    DescribeClientVpnTargetNetworksResponse (DescribeClientVpnTargetNetworksResponse'),
    newDescribeClientVpnTargetNetworksResponse,

    -- ** DescribeCoipPools (Paginated)
    DescribeCoipPools (DescribeCoipPools'),
    newDescribeCoipPools,
    DescribeCoipPoolsResponse (DescribeCoipPoolsResponse'),
    newDescribeCoipPoolsResponse,

    -- ** DescribeConversionTasks
    DescribeConversionTasks (DescribeConversionTasks'),
    newDescribeConversionTasks,
    DescribeConversionTasksResponse (DescribeConversionTasksResponse'),
    newDescribeConversionTasksResponse,

    -- ** DescribeCustomerGateways
    DescribeCustomerGateways (DescribeCustomerGateways'),
    newDescribeCustomerGateways,
    DescribeCustomerGatewaysResponse (DescribeCustomerGatewaysResponse'),
    newDescribeCustomerGatewaysResponse,

    -- ** DescribeDhcpOptions (Paginated)
    DescribeDhcpOptions (DescribeDhcpOptions'),
    newDescribeDhcpOptions,
    DescribeDhcpOptionsResponse (DescribeDhcpOptionsResponse'),
    newDescribeDhcpOptionsResponse,

    -- ** DescribeEgressOnlyInternetGateways (Paginated)
    DescribeEgressOnlyInternetGateways (DescribeEgressOnlyInternetGateways'),
    newDescribeEgressOnlyInternetGateways,
    DescribeEgressOnlyInternetGatewaysResponse (DescribeEgressOnlyInternetGatewaysResponse'),
    newDescribeEgressOnlyInternetGatewaysResponse,

    -- ** DescribeElasticGpus
    DescribeElasticGpus (DescribeElasticGpus'),
    newDescribeElasticGpus,
    DescribeElasticGpusResponse (DescribeElasticGpusResponse'),
    newDescribeElasticGpusResponse,

    -- ** DescribeExportImageTasks (Paginated)
    DescribeExportImageTasks (DescribeExportImageTasks'),
    newDescribeExportImageTasks,
    DescribeExportImageTasksResponse (DescribeExportImageTasksResponse'),
    newDescribeExportImageTasksResponse,

    -- ** DescribeExportTasks
    DescribeExportTasks (DescribeExportTasks'),
    newDescribeExportTasks,
    DescribeExportTasksResponse (DescribeExportTasksResponse'),
    newDescribeExportTasksResponse,

    -- ** DescribeFastLaunchImages (Paginated)
    DescribeFastLaunchImages (DescribeFastLaunchImages'),
    newDescribeFastLaunchImages,
    DescribeFastLaunchImagesResponse (DescribeFastLaunchImagesResponse'),
    newDescribeFastLaunchImagesResponse,

    -- ** DescribeFastSnapshotRestores (Paginated)
    DescribeFastSnapshotRestores (DescribeFastSnapshotRestores'),
    newDescribeFastSnapshotRestores,
    DescribeFastSnapshotRestoresResponse (DescribeFastSnapshotRestoresResponse'),
    newDescribeFastSnapshotRestoresResponse,

    -- ** DescribeFleetHistory
    DescribeFleetHistory (DescribeFleetHistory'),
    newDescribeFleetHistory,
    DescribeFleetHistoryResponse (DescribeFleetHistoryResponse'),
    newDescribeFleetHistoryResponse,

    -- ** DescribeFleetInstances
    DescribeFleetInstances (DescribeFleetInstances'),
    newDescribeFleetInstances,
    DescribeFleetInstancesResponse (DescribeFleetInstancesResponse'),
    newDescribeFleetInstancesResponse,

    -- ** DescribeFleets (Paginated)
    DescribeFleets (DescribeFleets'),
    newDescribeFleets,
    DescribeFleetsResponse (DescribeFleetsResponse'),
    newDescribeFleetsResponse,

    -- ** DescribeFlowLogs (Paginated)
    DescribeFlowLogs (DescribeFlowLogs'),
    newDescribeFlowLogs,
    DescribeFlowLogsResponse (DescribeFlowLogsResponse'),
    newDescribeFlowLogsResponse,

    -- ** DescribeFpgaImageAttribute
    DescribeFpgaImageAttribute (DescribeFpgaImageAttribute'),
    newDescribeFpgaImageAttribute,
    DescribeFpgaImageAttributeResponse (DescribeFpgaImageAttributeResponse'),
    newDescribeFpgaImageAttributeResponse,

    -- ** DescribeFpgaImages (Paginated)
    DescribeFpgaImages (DescribeFpgaImages'),
    newDescribeFpgaImages,
    DescribeFpgaImagesResponse (DescribeFpgaImagesResponse'),
    newDescribeFpgaImagesResponse,

    -- ** DescribeHostReservationOfferings (Paginated)
    DescribeHostReservationOfferings (DescribeHostReservationOfferings'),
    newDescribeHostReservationOfferings,
    DescribeHostReservationOfferingsResponse (DescribeHostReservationOfferingsResponse'),
    newDescribeHostReservationOfferingsResponse,

    -- ** DescribeHostReservations (Paginated)
    DescribeHostReservations (DescribeHostReservations'),
    newDescribeHostReservations,
    DescribeHostReservationsResponse (DescribeHostReservationsResponse'),
    newDescribeHostReservationsResponse,

    -- ** DescribeHosts (Paginated)
    DescribeHosts (DescribeHosts'),
    newDescribeHosts,
    DescribeHostsResponse (DescribeHostsResponse'),
    newDescribeHostsResponse,

    -- ** DescribeIamInstanceProfileAssociations (Paginated)
    DescribeIamInstanceProfileAssociations (DescribeIamInstanceProfileAssociations'),
    newDescribeIamInstanceProfileAssociations,
    DescribeIamInstanceProfileAssociationsResponse (DescribeIamInstanceProfileAssociationsResponse'),
    newDescribeIamInstanceProfileAssociationsResponse,

    -- ** DescribeIdFormat
    DescribeIdFormat (DescribeIdFormat'),
    newDescribeIdFormat,
    DescribeIdFormatResponse (DescribeIdFormatResponse'),
    newDescribeIdFormatResponse,

    -- ** DescribeIdentityIdFormat
    DescribeIdentityIdFormat (DescribeIdentityIdFormat'),
    newDescribeIdentityIdFormat,
    DescribeIdentityIdFormatResponse (DescribeIdentityIdFormatResponse'),
    newDescribeIdentityIdFormatResponse,

    -- ** DescribeImageAttribute
    DescribeImageAttribute (DescribeImageAttribute'),
    newDescribeImageAttribute,
    DescribeImageAttributeResponse (DescribeImageAttributeResponse'),
    newDescribeImageAttributeResponse,

    -- ** DescribeImages (Paginated)
    DescribeImages (DescribeImages'),
    newDescribeImages,
    DescribeImagesResponse (DescribeImagesResponse'),
    newDescribeImagesResponse,

    -- ** DescribeImportImageTasks (Paginated)
    DescribeImportImageTasks (DescribeImportImageTasks'),
    newDescribeImportImageTasks,
    DescribeImportImageTasksResponse (DescribeImportImageTasksResponse'),
    newDescribeImportImageTasksResponse,

    -- ** DescribeImportSnapshotTasks (Paginated)
    DescribeImportSnapshotTasks (DescribeImportSnapshotTasks'),
    newDescribeImportSnapshotTasks,
    DescribeImportSnapshotTasksResponse (DescribeImportSnapshotTasksResponse'),
    newDescribeImportSnapshotTasksResponse,

    -- ** DescribeInstanceAttribute
    DescribeInstanceAttribute (DescribeInstanceAttribute'),
    newDescribeInstanceAttribute,
    DescribeInstanceAttributeResponse (DescribeInstanceAttributeResponse'),
    newDescribeInstanceAttributeResponse,

    -- ** DescribeInstanceConnectEndpoints (Paginated)
    DescribeInstanceConnectEndpoints (DescribeInstanceConnectEndpoints'),
    newDescribeInstanceConnectEndpoints,
    DescribeInstanceConnectEndpointsResponse (DescribeInstanceConnectEndpointsResponse'),
    newDescribeInstanceConnectEndpointsResponse,

    -- ** DescribeInstanceCreditSpecifications (Paginated)
    DescribeInstanceCreditSpecifications (DescribeInstanceCreditSpecifications'),
    newDescribeInstanceCreditSpecifications,
    DescribeInstanceCreditSpecificationsResponse (DescribeInstanceCreditSpecificationsResponse'),
    newDescribeInstanceCreditSpecificationsResponse,

    -- ** DescribeInstanceEventNotificationAttributes
    DescribeInstanceEventNotificationAttributes (DescribeInstanceEventNotificationAttributes'),
    newDescribeInstanceEventNotificationAttributes,
    DescribeInstanceEventNotificationAttributesResponse (DescribeInstanceEventNotificationAttributesResponse'),
    newDescribeInstanceEventNotificationAttributesResponse,

    -- ** DescribeInstanceEventWindows (Paginated)
    DescribeInstanceEventWindows (DescribeInstanceEventWindows'),
    newDescribeInstanceEventWindows,
    DescribeInstanceEventWindowsResponse (DescribeInstanceEventWindowsResponse'),
    newDescribeInstanceEventWindowsResponse,

    -- ** DescribeInstanceStatus (Paginated)
    DescribeInstanceStatus (DescribeInstanceStatus'),
    newDescribeInstanceStatus,
    DescribeInstanceStatusResponse (DescribeInstanceStatusResponse'),
    newDescribeInstanceStatusResponse,

    -- ** DescribeInstanceTypeOfferings (Paginated)
    DescribeInstanceTypeOfferings (DescribeInstanceTypeOfferings'),
    newDescribeInstanceTypeOfferings,
    DescribeInstanceTypeOfferingsResponse (DescribeInstanceTypeOfferingsResponse'),
    newDescribeInstanceTypeOfferingsResponse,

    -- ** DescribeInstanceTypes (Paginated)
    DescribeInstanceTypes (DescribeInstanceTypes'),
    newDescribeInstanceTypes,
    DescribeInstanceTypesResponse (DescribeInstanceTypesResponse'),
    newDescribeInstanceTypesResponse,

    -- ** DescribeInstances (Paginated)
    DescribeInstances (DescribeInstances'),
    newDescribeInstances,
    DescribeInstancesResponse (DescribeInstancesResponse'),
    newDescribeInstancesResponse,

    -- ** DescribeInternetGateways (Paginated)
    DescribeInternetGateways (DescribeInternetGateways'),
    newDescribeInternetGateways,
    DescribeInternetGatewaysResponse (DescribeInternetGatewaysResponse'),
    newDescribeInternetGatewaysResponse,

    -- ** DescribeIpamPools (Paginated)
    DescribeIpamPools (DescribeIpamPools'),
    newDescribeIpamPools,
    DescribeIpamPoolsResponse (DescribeIpamPoolsResponse'),
    newDescribeIpamPoolsResponse,

    -- ** DescribeIpamResourceDiscoveries (Paginated)
    DescribeIpamResourceDiscoveries (DescribeIpamResourceDiscoveries'),
    newDescribeIpamResourceDiscoveries,
    DescribeIpamResourceDiscoveriesResponse (DescribeIpamResourceDiscoveriesResponse'),
    newDescribeIpamResourceDiscoveriesResponse,

    -- ** DescribeIpamResourceDiscoveryAssociations (Paginated)
    DescribeIpamResourceDiscoveryAssociations (DescribeIpamResourceDiscoveryAssociations'),
    newDescribeIpamResourceDiscoveryAssociations,
    DescribeIpamResourceDiscoveryAssociationsResponse (DescribeIpamResourceDiscoveryAssociationsResponse'),
    newDescribeIpamResourceDiscoveryAssociationsResponse,

    -- ** DescribeIpamScopes (Paginated)
    DescribeIpamScopes (DescribeIpamScopes'),
    newDescribeIpamScopes,
    DescribeIpamScopesResponse (DescribeIpamScopesResponse'),
    newDescribeIpamScopesResponse,

    -- ** DescribeIpams (Paginated)
    DescribeIpams (DescribeIpams'),
    newDescribeIpams,
    DescribeIpamsResponse (DescribeIpamsResponse'),
    newDescribeIpamsResponse,

    -- ** DescribeIpv6Pools (Paginated)
    DescribeIpv6Pools (DescribeIpv6Pools'),
    newDescribeIpv6Pools,
    DescribeIpv6PoolsResponse (DescribeIpv6PoolsResponse'),
    newDescribeIpv6PoolsResponse,

    -- ** DescribeKeyPairs
    DescribeKeyPairs (DescribeKeyPairs'),
    newDescribeKeyPairs,
    DescribeKeyPairsResponse (DescribeKeyPairsResponse'),
    newDescribeKeyPairsResponse,

    -- ** DescribeLaunchTemplateVersions (Paginated)
    DescribeLaunchTemplateVersions (DescribeLaunchTemplateVersions'),
    newDescribeLaunchTemplateVersions,
    DescribeLaunchTemplateVersionsResponse (DescribeLaunchTemplateVersionsResponse'),
    newDescribeLaunchTemplateVersionsResponse,

    -- ** DescribeLaunchTemplates (Paginated)
    DescribeLaunchTemplates (DescribeLaunchTemplates'),
    newDescribeLaunchTemplates,
    DescribeLaunchTemplatesResponse (DescribeLaunchTemplatesResponse'),
    newDescribeLaunchTemplatesResponse,

    -- ** DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations (Paginated)
    DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations (DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations'),
    newDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations,
    DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse (DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse'),
    newDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse,

    -- ** DescribeLocalGatewayRouteTableVpcAssociations (Paginated)
    DescribeLocalGatewayRouteTableVpcAssociations (DescribeLocalGatewayRouteTableVpcAssociations'),
    newDescribeLocalGatewayRouteTableVpcAssociations,
    DescribeLocalGatewayRouteTableVpcAssociationsResponse (DescribeLocalGatewayRouteTableVpcAssociationsResponse'),
    newDescribeLocalGatewayRouteTableVpcAssociationsResponse,

    -- ** DescribeLocalGatewayRouteTables (Paginated)
    DescribeLocalGatewayRouteTables (DescribeLocalGatewayRouteTables'),
    newDescribeLocalGatewayRouteTables,
    DescribeLocalGatewayRouteTablesResponse (DescribeLocalGatewayRouteTablesResponse'),
    newDescribeLocalGatewayRouteTablesResponse,

    -- ** DescribeLocalGatewayVirtualInterfaceGroups (Paginated)
    DescribeLocalGatewayVirtualInterfaceGroups (DescribeLocalGatewayVirtualInterfaceGroups'),
    newDescribeLocalGatewayVirtualInterfaceGroups,
    DescribeLocalGatewayVirtualInterfaceGroupsResponse (DescribeLocalGatewayVirtualInterfaceGroupsResponse'),
    newDescribeLocalGatewayVirtualInterfaceGroupsResponse,

    -- ** DescribeLocalGatewayVirtualInterfaces (Paginated)
    DescribeLocalGatewayVirtualInterfaces (DescribeLocalGatewayVirtualInterfaces'),
    newDescribeLocalGatewayVirtualInterfaces,
    DescribeLocalGatewayVirtualInterfacesResponse (DescribeLocalGatewayVirtualInterfacesResponse'),
    newDescribeLocalGatewayVirtualInterfacesResponse,

    -- ** DescribeLocalGateways (Paginated)
    DescribeLocalGateways (DescribeLocalGateways'),
    newDescribeLocalGateways,
    DescribeLocalGatewaysResponse (DescribeLocalGatewaysResponse'),
    newDescribeLocalGatewaysResponse,

    -- ** DescribeManagedPrefixLists (Paginated)
    DescribeManagedPrefixLists (DescribeManagedPrefixLists'),
    newDescribeManagedPrefixLists,
    DescribeManagedPrefixListsResponse (DescribeManagedPrefixListsResponse'),
    newDescribeManagedPrefixListsResponse,

    -- ** DescribeMovingAddresses (Paginated)
    DescribeMovingAddresses (DescribeMovingAddresses'),
    newDescribeMovingAddresses,
    DescribeMovingAddressesResponse (DescribeMovingAddressesResponse'),
    newDescribeMovingAddressesResponse,

    -- ** DescribeNatGateways (Paginated)
    DescribeNatGateways (DescribeNatGateways'),
    newDescribeNatGateways,
    DescribeNatGatewaysResponse (DescribeNatGatewaysResponse'),
    newDescribeNatGatewaysResponse,

    -- ** DescribeNetworkAcls (Paginated)
    DescribeNetworkAcls (DescribeNetworkAcls'),
    newDescribeNetworkAcls,
    DescribeNetworkAclsResponse (DescribeNetworkAclsResponse'),
    newDescribeNetworkAclsResponse,

    -- ** DescribeNetworkInsightsAccessScopeAnalyses (Paginated)
    DescribeNetworkInsightsAccessScopeAnalyses (DescribeNetworkInsightsAccessScopeAnalyses'),
    newDescribeNetworkInsightsAccessScopeAnalyses,
    DescribeNetworkInsightsAccessScopeAnalysesResponse (DescribeNetworkInsightsAccessScopeAnalysesResponse'),
    newDescribeNetworkInsightsAccessScopeAnalysesResponse,

    -- ** DescribeNetworkInsightsAccessScopes (Paginated)
    DescribeNetworkInsightsAccessScopes (DescribeNetworkInsightsAccessScopes'),
    newDescribeNetworkInsightsAccessScopes,
    DescribeNetworkInsightsAccessScopesResponse (DescribeNetworkInsightsAccessScopesResponse'),
    newDescribeNetworkInsightsAccessScopesResponse,

    -- ** DescribeNetworkInsightsAnalyses (Paginated)
    DescribeNetworkInsightsAnalyses (DescribeNetworkInsightsAnalyses'),
    newDescribeNetworkInsightsAnalyses,
    DescribeNetworkInsightsAnalysesResponse (DescribeNetworkInsightsAnalysesResponse'),
    newDescribeNetworkInsightsAnalysesResponse,

    -- ** DescribeNetworkInsightsPaths (Paginated)
    DescribeNetworkInsightsPaths (DescribeNetworkInsightsPaths'),
    newDescribeNetworkInsightsPaths,
    DescribeNetworkInsightsPathsResponse (DescribeNetworkInsightsPathsResponse'),
    newDescribeNetworkInsightsPathsResponse,

    -- ** DescribeNetworkInterfaceAttribute
    DescribeNetworkInterfaceAttribute (DescribeNetworkInterfaceAttribute'),
    newDescribeNetworkInterfaceAttribute,
    DescribeNetworkInterfaceAttributeResponse (DescribeNetworkInterfaceAttributeResponse'),
    newDescribeNetworkInterfaceAttributeResponse,

    -- ** DescribeNetworkInterfacePermissions (Paginated)
    DescribeNetworkInterfacePermissions (DescribeNetworkInterfacePermissions'),
    newDescribeNetworkInterfacePermissions,
    DescribeNetworkInterfacePermissionsResponse (DescribeNetworkInterfacePermissionsResponse'),
    newDescribeNetworkInterfacePermissionsResponse,

    -- ** DescribeNetworkInterfaces (Paginated)
    DescribeNetworkInterfaces (DescribeNetworkInterfaces'),
    newDescribeNetworkInterfaces,
    DescribeNetworkInterfacesResponse (DescribeNetworkInterfacesResponse'),
    newDescribeNetworkInterfacesResponse,

    -- ** DescribePlacementGroups
    DescribePlacementGroups (DescribePlacementGroups'),
    newDescribePlacementGroups,
    DescribePlacementGroupsResponse (DescribePlacementGroupsResponse'),
    newDescribePlacementGroupsResponse,

    -- ** DescribePrefixLists (Paginated)
    DescribePrefixLists (DescribePrefixLists'),
    newDescribePrefixLists,
    DescribePrefixListsResponse (DescribePrefixListsResponse'),
    newDescribePrefixListsResponse,

    -- ** DescribePrincipalIdFormat (Paginated)
    DescribePrincipalIdFormat (DescribePrincipalIdFormat'),
    newDescribePrincipalIdFormat,
    DescribePrincipalIdFormatResponse (DescribePrincipalIdFormatResponse'),
    newDescribePrincipalIdFormatResponse,

    -- ** DescribePublicIpv4Pools (Paginated)
    DescribePublicIpv4Pools (DescribePublicIpv4Pools'),
    newDescribePublicIpv4Pools,
    DescribePublicIpv4PoolsResponse (DescribePublicIpv4PoolsResponse'),
    newDescribePublicIpv4PoolsResponse,

    -- ** DescribeRegions
    DescribeRegions (DescribeRegions'),
    newDescribeRegions,
    DescribeRegionsResponse (DescribeRegionsResponse'),
    newDescribeRegionsResponse,

    -- ** DescribeReplaceRootVolumeTasks (Paginated)
    DescribeReplaceRootVolumeTasks (DescribeReplaceRootVolumeTasks'),
    newDescribeReplaceRootVolumeTasks,
    DescribeReplaceRootVolumeTasksResponse (DescribeReplaceRootVolumeTasksResponse'),
    newDescribeReplaceRootVolumeTasksResponse,

    -- ** DescribeReservedInstances
    DescribeReservedInstances (DescribeReservedInstances'),
    newDescribeReservedInstances,
    DescribeReservedInstancesResponse (DescribeReservedInstancesResponse'),
    newDescribeReservedInstancesResponse,

    -- ** DescribeReservedInstancesListings
    DescribeReservedInstancesListings (DescribeReservedInstancesListings'),
    newDescribeReservedInstancesListings,
    DescribeReservedInstancesListingsResponse (DescribeReservedInstancesListingsResponse'),
    newDescribeReservedInstancesListingsResponse,

    -- ** DescribeReservedInstancesModifications (Paginated)
    DescribeReservedInstancesModifications (DescribeReservedInstancesModifications'),
    newDescribeReservedInstancesModifications,
    DescribeReservedInstancesModificationsResponse (DescribeReservedInstancesModificationsResponse'),
    newDescribeReservedInstancesModificationsResponse,

    -- ** DescribeReservedInstancesOfferings (Paginated)
    DescribeReservedInstancesOfferings (DescribeReservedInstancesOfferings'),
    newDescribeReservedInstancesOfferings,
    DescribeReservedInstancesOfferingsResponse (DescribeReservedInstancesOfferingsResponse'),
    newDescribeReservedInstancesOfferingsResponse,

    -- ** DescribeRouteTables (Paginated)
    DescribeRouteTables (DescribeRouteTables'),
    newDescribeRouteTables,
    DescribeRouteTablesResponse (DescribeRouteTablesResponse'),
    newDescribeRouteTablesResponse,

    -- ** DescribeScheduledInstanceAvailability (Paginated)
    DescribeScheduledInstanceAvailability (DescribeScheduledInstanceAvailability'),
    newDescribeScheduledInstanceAvailability,
    DescribeScheduledInstanceAvailabilityResponse (DescribeScheduledInstanceAvailabilityResponse'),
    newDescribeScheduledInstanceAvailabilityResponse,

    -- ** DescribeScheduledInstances (Paginated)
    DescribeScheduledInstances (DescribeScheduledInstances'),
    newDescribeScheduledInstances,
    DescribeScheduledInstancesResponse (DescribeScheduledInstancesResponse'),
    newDescribeScheduledInstancesResponse,

    -- ** DescribeSecurityGroupReferences
    DescribeSecurityGroupReferences (DescribeSecurityGroupReferences'),
    newDescribeSecurityGroupReferences,
    DescribeSecurityGroupReferencesResponse (DescribeSecurityGroupReferencesResponse'),
    newDescribeSecurityGroupReferencesResponse,

    -- ** DescribeSecurityGroupRules (Paginated)
    DescribeSecurityGroupRules (DescribeSecurityGroupRules'),
    newDescribeSecurityGroupRules,
    DescribeSecurityGroupRulesResponse (DescribeSecurityGroupRulesResponse'),
    newDescribeSecurityGroupRulesResponse,

    -- ** DescribeSecurityGroups (Paginated)
    DescribeSecurityGroups (DescribeSecurityGroups'),
    newDescribeSecurityGroups,
    DescribeSecurityGroupsResponse (DescribeSecurityGroupsResponse'),
    newDescribeSecurityGroupsResponse,

    -- ** DescribeSnapshotAttribute
    DescribeSnapshotAttribute (DescribeSnapshotAttribute'),
    newDescribeSnapshotAttribute,
    DescribeSnapshotAttributeResponse (DescribeSnapshotAttributeResponse'),
    newDescribeSnapshotAttributeResponse,

    -- ** DescribeSnapshotTierStatus (Paginated)
    DescribeSnapshotTierStatus (DescribeSnapshotTierStatus'),
    newDescribeSnapshotTierStatus,
    DescribeSnapshotTierStatusResponse (DescribeSnapshotTierStatusResponse'),
    newDescribeSnapshotTierStatusResponse,

    -- ** DescribeSnapshots (Paginated)
    DescribeSnapshots (DescribeSnapshots'),
    newDescribeSnapshots,
    DescribeSnapshotsResponse (DescribeSnapshotsResponse'),
    newDescribeSnapshotsResponse,

    -- ** DescribeSpotDatafeedSubscription
    DescribeSpotDatafeedSubscription (DescribeSpotDatafeedSubscription'),
    newDescribeSpotDatafeedSubscription,
    DescribeSpotDatafeedSubscriptionResponse (DescribeSpotDatafeedSubscriptionResponse'),
    newDescribeSpotDatafeedSubscriptionResponse,

    -- ** DescribeSpotFleetInstances (Paginated)
    DescribeSpotFleetInstances (DescribeSpotFleetInstances'),
    newDescribeSpotFleetInstances,
    DescribeSpotFleetInstancesResponse (DescribeSpotFleetInstancesResponse'),
    newDescribeSpotFleetInstancesResponse,

    -- ** DescribeSpotFleetRequestHistory
    DescribeSpotFleetRequestHistory (DescribeSpotFleetRequestHistory'),
    newDescribeSpotFleetRequestHistory,
    DescribeSpotFleetRequestHistoryResponse (DescribeSpotFleetRequestHistoryResponse'),
    newDescribeSpotFleetRequestHistoryResponse,

    -- ** DescribeSpotFleetRequests (Paginated)
    DescribeSpotFleetRequests (DescribeSpotFleetRequests'),
    newDescribeSpotFleetRequests,
    DescribeSpotFleetRequestsResponse (DescribeSpotFleetRequestsResponse'),
    newDescribeSpotFleetRequestsResponse,

    -- ** DescribeSpotInstanceRequests (Paginated)
    DescribeSpotInstanceRequests (DescribeSpotInstanceRequests'),
    newDescribeSpotInstanceRequests,
    DescribeSpotInstanceRequestsResponse (DescribeSpotInstanceRequestsResponse'),
    newDescribeSpotInstanceRequestsResponse,

    -- ** DescribeSpotPriceHistory (Paginated)
    DescribeSpotPriceHistory (DescribeSpotPriceHistory'),
    newDescribeSpotPriceHistory,
    DescribeSpotPriceHistoryResponse (DescribeSpotPriceHistoryResponse'),
    newDescribeSpotPriceHistoryResponse,

    -- ** DescribeStaleSecurityGroups (Paginated)
    DescribeStaleSecurityGroups (DescribeStaleSecurityGroups'),
    newDescribeStaleSecurityGroups,
    DescribeStaleSecurityGroupsResponse (DescribeStaleSecurityGroupsResponse'),
    newDescribeStaleSecurityGroupsResponse,

    -- ** DescribeStoreImageTasks (Paginated)
    DescribeStoreImageTasks (DescribeStoreImageTasks'),
    newDescribeStoreImageTasks,
    DescribeStoreImageTasksResponse (DescribeStoreImageTasksResponse'),
    newDescribeStoreImageTasksResponse,

    -- ** DescribeSubnets (Paginated)
    DescribeSubnets (DescribeSubnets'),
    newDescribeSubnets,
    DescribeSubnetsResponse (DescribeSubnetsResponse'),
    newDescribeSubnetsResponse,

    -- ** DescribeTags (Paginated)
    DescribeTags (DescribeTags'),
    newDescribeTags,
    DescribeTagsResponse (DescribeTagsResponse'),
    newDescribeTagsResponse,

    -- ** DescribeTrafficMirrorFilters (Paginated)
    DescribeTrafficMirrorFilters (DescribeTrafficMirrorFilters'),
    newDescribeTrafficMirrorFilters,
    DescribeTrafficMirrorFiltersResponse (DescribeTrafficMirrorFiltersResponse'),
    newDescribeTrafficMirrorFiltersResponse,

    -- ** DescribeTrafficMirrorSessions (Paginated)
    DescribeTrafficMirrorSessions (DescribeTrafficMirrorSessions'),
    newDescribeTrafficMirrorSessions,
    DescribeTrafficMirrorSessionsResponse (DescribeTrafficMirrorSessionsResponse'),
    newDescribeTrafficMirrorSessionsResponse,

    -- ** DescribeTrafficMirrorTargets (Paginated)
    DescribeTrafficMirrorTargets (DescribeTrafficMirrorTargets'),
    newDescribeTrafficMirrorTargets,
    DescribeTrafficMirrorTargetsResponse (DescribeTrafficMirrorTargetsResponse'),
    newDescribeTrafficMirrorTargetsResponse,

    -- ** DescribeTransitGatewayAttachments (Paginated)
    DescribeTransitGatewayAttachments (DescribeTransitGatewayAttachments'),
    newDescribeTransitGatewayAttachments,
    DescribeTransitGatewayAttachmentsResponse (DescribeTransitGatewayAttachmentsResponse'),
    newDescribeTransitGatewayAttachmentsResponse,

    -- ** DescribeTransitGatewayConnectPeers (Paginated)
    DescribeTransitGatewayConnectPeers (DescribeTransitGatewayConnectPeers'),
    newDescribeTransitGatewayConnectPeers,
    DescribeTransitGatewayConnectPeersResponse (DescribeTransitGatewayConnectPeersResponse'),
    newDescribeTransitGatewayConnectPeersResponse,

    -- ** DescribeTransitGatewayConnects (Paginated)
    DescribeTransitGatewayConnects (DescribeTransitGatewayConnects'),
    newDescribeTransitGatewayConnects,
    DescribeTransitGatewayConnectsResponse (DescribeTransitGatewayConnectsResponse'),
    newDescribeTransitGatewayConnectsResponse,

    -- ** DescribeTransitGatewayMulticastDomains (Paginated)
    DescribeTransitGatewayMulticastDomains (DescribeTransitGatewayMulticastDomains'),
    newDescribeTransitGatewayMulticastDomains,
    DescribeTransitGatewayMulticastDomainsResponse (DescribeTransitGatewayMulticastDomainsResponse'),
    newDescribeTransitGatewayMulticastDomainsResponse,

    -- ** DescribeTransitGatewayPeeringAttachments (Paginated)
    DescribeTransitGatewayPeeringAttachments (DescribeTransitGatewayPeeringAttachments'),
    newDescribeTransitGatewayPeeringAttachments,
    DescribeTransitGatewayPeeringAttachmentsResponse (DescribeTransitGatewayPeeringAttachmentsResponse'),
    newDescribeTransitGatewayPeeringAttachmentsResponse,

    -- ** DescribeTransitGatewayPolicyTables (Paginated)
    DescribeTransitGatewayPolicyTables (DescribeTransitGatewayPolicyTables'),
    newDescribeTransitGatewayPolicyTables,
    DescribeTransitGatewayPolicyTablesResponse (DescribeTransitGatewayPolicyTablesResponse'),
    newDescribeTransitGatewayPolicyTablesResponse,

    -- ** DescribeTransitGatewayRouteTableAnnouncements (Paginated)
    DescribeTransitGatewayRouteTableAnnouncements (DescribeTransitGatewayRouteTableAnnouncements'),
    newDescribeTransitGatewayRouteTableAnnouncements,
    DescribeTransitGatewayRouteTableAnnouncementsResponse (DescribeTransitGatewayRouteTableAnnouncementsResponse'),
    newDescribeTransitGatewayRouteTableAnnouncementsResponse,

    -- ** DescribeTransitGatewayRouteTables (Paginated)
    DescribeTransitGatewayRouteTables (DescribeTransitGatewayRouteTables'),
    newDescribeTransitGatewayRouteTables,
    DescribeTransitGatewayRouteTablesResponse (DescribeTransitGatewayRouteTablesResponse'),
    newDescribeTransitGatewayRouteTablesResponse,

    -- ** DescribeTransitGatewayVpcAttachments (Paginated)
    DescribeTransitGatewayVpcAttachments (DescribeTransitGatewayVpcAttachments'),
    newDescribeTransitGatewayVpcAttachments,
    DescribeTransitGatewayVpcAttachmentsResponse (DescribeTransitGatewayVpcAttachmentsResponse'),
    newDescribeTransitGatewayVpcAttachmentsResponse,

    -- ** DescribeTransitGateways (Paginated)
    DescribeTransitGateways (DescribeTransitGateways'),
    newDescribeTransitGateways,
    DescribeTransitGatewaysResponse (DescribeTransitGatewaysResponse'),
    newDescribeTransitGatewaysResponse,

    -- ** DescribeTrunkInterfaceAssociations (Paginated)
    DescribeTrunkInterfaceAssociations (DescribeTrunkInterfaceAssociations'),
    newDescribeTrunkInterfaceAssociations,
    DescribeTrunkInterfaceAssociationsResponse (DescribeTrunkInterfaceAssociationsResponse'),
    newDescribeTrunkInterfaceAssociationsResponse,

    -- ** DescribeVerifiedAccessEndpoints (Paginated)
    DescribeVerifiedAccessEndpoints (DescribeVerifiedAccessEndpoints'),
    newDescribeVerifiedAccessEndpoints,
    DescribeVerifiedAccessEndpointsResponse (DescribeVerifiedAccessEndpointsResponse'),
    newDescribeVerifiedAccessEndpointsResponse,

    -- ** DescribeVerifiedAccessGroups (Paginated)
    DescribeVerifiedAccessGroups (DescribeVerifiedAccessGroups'),
    newDescribeVerifiedAccessGroups,
    DescribeVerifiedAccessGroupsResponse (DescribeVerifiedAccessGroupsResponse'),
    newDescribeVerifiedAccessGroupsResponse,

    -- ** DescribeVerifiedAccessInstanceLoggingConfigurations (Paginated)
    DescribeVerifiedAccessInstanceLoggingConfigurations (DescribeVerifiedAccessInstanceLoggingConfigurations'),
    newDescribeVerifiedAccessInstanceLoggingConfigurations,
    DescribeVerifiedAccessInstanceLoggingConfigurationsResponse (DescribeVerifiedAccessInstanceLoggingConfigurationsResponse'),
    newDescribeVerifiedAccessInstanceLoggingConfigurationsResponse,

    -- ** DescribeVerifiedAccessInstances (Paginated)
    DescribeVerifiedAccessInstances (DescribeVerifiedAccessInstances'),
    newDescribeVerifiedAccessInstances,
    DescribeVerifiedAccessInstancesResponse (DescribeVerifiedAccessInstancesResponse'),
    newDescribeVerifiedAccessInstancesResponse,

    -- ** DescribeVerifiedAccessTrustProviders (Paginated)
    DescribeVerifiedAccessTrustProviders (DescribeVerifiedAccessTrustProviders'),
    newDescribeVerifiedAccessTrustProviders,
    DescribeVerifiedAccessTrustProvidersResponse (DescribeVerifiedAccessTrustProvidersResponse'),
    newDescribeVerifiedAccessTrustProvidersResponse,

    -- ** DescribeVolumeAttribute
    DescribeVolumeAttribute (DescribeVolumeAttribute'),
    newDescribeVolumeAttribute,
    DescribeVolumeAttributeResponse (DescribeVolumeAttributeResponse'),
    newDescribeVolumeAttributeResponse,

    -- ** DescribeVolumeStatus (Paginated)
    DescribeVolumeStatus (DescribeVolumeStatus'),
    newDescribeVolumeStatus,
    DescribeVolumeStatusResponse (DescribeVolumeStatusResponse'),
    newDescribeVolumeStatusResponse,

    -- ** DescribeVolumes (Paginated)
    DescribeVolumes (DescribeVolumes'),
    newDescribeVolumes,
    DescribeVolumesResponse (DescribeVolumesResponse'),
    newDescribeVolumesResponse,

    -- ** DescribeVolumesModifications (Paginated)
    DescribeVolumesModifications (DescribeVolumesModifications'),
    newDescribeVolumesModifications,
    DescribeVolumesModificationsResponse (DescribeVolumesModificationsResponse'),
    newDescribeVolumesModificationsResponse,

    -- ** DescribeVpcAttribute
    DescribeVpcAttribute (DescribeVpcAttribute'),
    newDescribeVpcAttribute,
    DescribeVpcAttributeResponse (DescribeVpcAttributeResponse'),
    newDescribeVpcAttributeResponse,

    -- ** DescribeVpcClassicLink
    DescribeVpcClassicLink (DescribeVpcClassicLink'),
    newDescribeVpcClassicLink,
    DescribeVpcClassicLinkResponse (DescribeVpcClassicLinkResponse'),
    newDescribeVpcClassicLinkResponse,

    -- ** DescribeVpcClassicLinkDnsSupport (Paginated)
    DescribeVpcClassicLinkDnsSupport (DescribeVpcClassicLinkDnsSupport'),
    newDescribeVpcClassicLinkDnsSupport,
    DescribeVpcClassicLinkDnsSupportResponse (DescribeVpcClassicLinkDnsSupportResponse'),
    newDescribeVpcClassicLinkDnsSupportResponse,

    -- ** DescribeVpcEndpointConnectionNotifications (Paginated)
    DescribeVpcEndpointConnectionNotifications (DescribeVpcEndpointConnectionNotifications'),
    newDescribeVpcEndpointConnectionNotifications,
    DescribeVpcEndpointConnectionNotificationsResponse (DescribeVpcEndpointConnectionNotificationsResponse'),
    newDescribeVpcEndpointConnectionNotificationsResponse,

    -- ** DescribeVpcEndpointConnections (Paginated)
    DescribeVpcEndpointConnections (DescribeVpcEndpointConnections'),
    newDescribeVpcEndpointConnections,
    DescribeVpcEndpointConnectionsResponse (DescribeVpcEndpointConnectionsResponse'),
    newDescribeVpcEndpointConnectionsResponse,

    -- ** DescribeVpcEndpointServiceConfigurations (Paginated)
    DescribeVpcEndpointServiceConfigurations (DescribeVpcEndpointServiceConfigurations'),
    newDescribeVpcEndpointServiceConfigurations,
    DescribeVpcEndpointServiceConfigurationsResponse (DescribeVpcEndpointServiceConfigurationsResponse'),
    newDescribeVpcEndpointServiceConfigurationsResponse,

    -- ** DescribeVpcEndpointServicePermissions (Paginated)
    DescribeVpcEndpointServicePermissions (DescribeVpcEndpointServicePermissions'),
    newDescribeVpcEndpointServicePermissions,
    DescribeVpcEndpointServicePermissionsResponse (DescribeVpcEndpointServicePermissionsResponse'),
    newDescribeVpcEndpointServicePermissionsResponse,

    -- ** DescribeVpcEndpointServices (Paginated)
    DescribeVpcEndpointServices (DescribeVpcEndpointServices'),
    newDescribeVpcEndpointServices,
    DescribeVpcEndpointServicesResponse (DescribeVpcEndpointServicesResponse'),
    newDescribeVpcEndpointServicesResponse,

    -- ** DescribeVpcEndpoints (Paginated)
    DescribeVpcEndpoints (DescribeVpcEndpoints'),
    newDescribeVpcEndpoints,
    DescribeVpcEndpointsResponse (DescribeVpcEndpointsResponse'),
    newDescribeVpcEndpointsResponse,

    -- ** DescribeVpcPeeringConnections (Paginated)
    DescribeVpcPeeringConnections (DescribeVpcPeeringConnections'),
    newDescribeVpcPeeringConnections,
    DescribeVpcPeeringConnectionsResponse (DescribeVpcPeeringConnectionsResponse'),
    newDescribeVpcPeeringConnectionsResponse,

    -- ** DescribeVpcs (Paginated)
    DescribeVpcs (DescribeVpcs'),
    newDescribeVpcs,
    DescribeVpcsResponse (DescribeVpcsResponse'),
    newDescribeVpcsResponse,

    -- ** DescribeVpnConnections
    DescribeVpnConnections (DescribeVpnConnections'),
    newDescribeVpnConnections,
    DescribeVpnConnectionsResponse (DescribeVpnConnectionsResponse'),
    newDescribeVpnConnectionsResponse,

    -- ** DescribeVpnGateways
    DescribeVpnGateways (DescribeVpnGateways'),
    newDescribeVpnGateways,
    DescribeVpnGatewaysResponse (DescribeVpnGatewaysResponse'),
    newDescribeVpnGatewaysResponse,

    -- ** DetachClassicLinkVpc
    DetachClassicLinkVpc (DetachClassicLinkVpc'),
    newDetachClassicLinkVpc,
    DetachClassicLinkVpcResponse (DetachClassicLinkVpcResponse'),
    newDetachClassicLinkVpcResponse,

    -- ** DetachInternetGateway
    DetachInternetGateway (DetachInternetGateway'),
    newDetachInternetGateway,
    DetachInternetGatewayResponse (DetachInternetGatewayResponse'),
    newDetachInternetGatewayResponse,

    -- ** DetachNetworkInterface
    DetachNetworkInterface (DetachNetworkInterface'),
    newDetachNetworkInterface,
    DetachNetworkInterfaceResponse (DetachNetworkInterfaceResponse'),
    newDetachNetworkInterfaceResponse,

    -- ** DetachVerifiedAccessTrustProvider
    DetachVerifiedAccessTrustProvider (DetachVerifiedAccessTrustProvider'),
    newDetachVerifiedAccessTrustProvider,
    DetachVerifiedAccessTrustProviderResponse (DetachVerifiedAccessTrustProviderResponse'),
    newDetachVerifiedAccessTrustProviderResponse,

    -- ** DetachVolume
    DetachVolume (DetachVolume'),
    newDetachVolume,
    VolumeAttachment (VolumeAttachment'),
    newVolumeAttachment,

    -- ** DetachVpnGateway
    DetachVpnGateway (DetachVpnGateway'),
    newDetachVpnGateway,
    DetachVpnGatewayResponse (DetachVpnGatewayResponse'),
    newDetachVpnGatewayResponse,

    -- ** DisableAddressTransfer
    DisableAddressTransfer (DisableAddressTransfer'),
    newDisableAddressTransfer,
    DisableAddressTransferResponse (DisableAddressTransferResponse'),
    newDisableAddressTransferResponse,

    -- ** DisableAwsNetworkPerformanceMetricSubscription
    DisableAwsNetworkPerformanceMetricSubscription (DisableAwsNetworkPerformanceMetricSubscription'),
    newDisableAwsNetworkPerformanceMetricSubscription,
    DisableAwsNetworkPerformanceMetricSubscriptionResponse (DisableAwsNetworkPerformanceMetricSubscriptionResponse'),
    newDisableAwsNetworkPerformanceMetricSubscriptionResponse,

    -- ** DisableEbsEncryptionByDefault
    DisableEbsEncryptionByDefault (DisableEbsEncryptionByDefault'),
    newDisableEbsEncryptionByDefault,
    DisableEbsEncryptionByDefaultResponse (DisableEbsEncryptionByDefaultResponse'),
    newDisableEbsEncryptionByDefaultResponse,

    -- ** DisableFastLaunch
    DisableFastLaunch (DisableFastLaunch'),
    newDisableFastLaunch,
    DisableFastLaunchResponse (DisableFastLaunchResponse'),
    newDisableFastLaunchResponse,

    -- ** DisableFastSnapshotRestores
    DisableFastSnapshotRestores (DisableFastSnapshotRestores'),
    newDisableFastSnapshotRestores,
    DisableFastSnapshotRestoresResponse (DisableFastSnapshotRestoresResponse'),
    newDisableFastSnapshotRestoresResponse,

    -- ** DisableImageDeprecation
    DisableImageDeprecation (DisableImageDeprecation'),
    newDisableImageDeprecation,
    DisableImageDeprecationResponse (DisableImageDeprecationResponse'),
    newDisableImageDeprecationResponse,

    -- ** DisableIpamOrganizationAdminAccount
    DisableIpamOrganizationAdminAccount (DisableIpamOrganizationAdminAccount'),
    newDisableIpamOrganizationAdminAccount,
    DisableIpamOrganizationAdminAccountResponse (DisableIpamOrganizationAdminAccountResponse'),
    newDisableIpamOrganizationAdminAccountResponse,

    -- ** DisableSerialConsoleAccess
    DisableSerialConsoleAccess (DisableSerialConsoleAccess'),
    newDisableSerialConsoleAccess,
    DisableSerialConsoleAccessResponse (DisableSerialConsoleAccessResponse'),
    newDisableSerialConsoleAccessResponse,

    -- ** DisableTransitGatewayRouteTablePropagation
    DisableTransitGatewayRouteTablePropagation (DisableTransitGatewayRouteTablePropagation'),
    newDisableTransitGatewayRouteTablePropagation,
    DisableTransitGatewayRouteTablePropagationResponse (DisableTransitGatewayRouteTablePropagationResponse'),
    newDisableTransitGatewayRouteTablePropagationResponse,

    -- ** DisableVgwRoutePropagation
    DisableVgwRoutePropagation (DisableVgwRoutePropagation'),
    newDisableVgwRoutePropagation,
    DisableVgwRoutePropagationResponse (DisableVgwRoutePropagationResponse'),
    newDisableVgwRoutePropagationResponse,

    -- ** DisableVpcClassicLink
    DisableVpcClassicLink (DisableVpcClassicLink'),
    newDisableVpcClassicLink,
    DisableVpcClassicLinkResponse (DisableVpcClassicLinkResponse'),
    newDisableVpcClassicLinkResponse,

    -- ** DisableVpcClassicLinkDnsSupport
    DisableVpcClassicLinkDnsSupport (DisableVpcClassicLinkDnsSupport'),
    newDisableVpcClassicLinkDnsSupport,
    DisableVpcClassicLinkDnsSupportResponse (DisableVpcClassicLinkDnsSupportResponse'),
    newDisableVpcClassicLinkDnsSupportResponse,

    -- ** DisassociateAddress
    DisassociateAddress (DisassociateAddress'),
    newDisassociateAddress,
    DisassociateAddressResponse (DisassociateAddressResponse'),
    newDisassociateAddressResponse,

    -- ** DisassociateClientVpnTargetNetwork
    DisassociateClientVpnTargetNetwork (DisassociateClientVpnTargetNetwork'),
    newDisassociateClientVpnTargetNetwork,
    DisassociateClientVpnTargetNetworkResponse (DisassociateClientVpnTargetNetworkResponse'),
    newDisassociateClientVpnTargetNetworkResponse,

    -- ** DisassociateEnclaveCertificateIamRole
    DisassociateEnclaveCertificateIamRole (DisassociateEnclaveCertificateIamRole'),
    newDisassociateEnclaveCertificateIamRole,
    DisassociateEnclaveCertificateIamRoleResponse (DisassociateEnclaveCertificateIamRoleResponse'),
    newDisassociateEnclaveCertificateIamRoleResponse,

    -- ** DisassociateIamInstanceProfile
    DisassociateIamInstanceProfile (DisassociateIamInstanceProfile'),
    newDisassociateIamInstanceProfile,
    DisassociateIamInstanceProfileResponse (DisassociateIamInstanceProfileResponse'),
    newDisassociateIamInstanceProfileResponse,

    -- ** DisassociateInstanceEventWindow
    DisassociateInstanceEventWindow (DisassociateInstanceEventWindow'),
    newDisassociateInstanceEventWindow,
    DisassociateInstanceEventWindowResponse (DisassociateInstanceEventWindowResponse'),
    newDisassociateInstanceEventWindowResponse,

    -- ** DisassociateIpamResourceDiscovery
    DisassociateIpamResourceDiscovery (DisassociateIpamResourceDiscovery'),
    newDisassociateIpamResourceDiscovery,
    DisassociateIpamResourceDiscoveryResponse (DisassociateIpamResourceDiscoveryResponse'),
    newDisassociateIpamResourceDiscoveryResponse,

    -- ** DisassociateNatGatewayAddress
    DisassociateNatGatewayAddress (DisassociateNatGatewayAddress'),
    newDisassociateNatGatewayAddress,
    DisassociateNatGatewayAddressResponse (DisassociateNatGatewayAddressResponse'),
    newDisassociateNatGatewayAddressResponse,

    -- ** DisassociateRouteTable
    DisassociateRouteTable (DisassociateRouteTable'),
    newDisassociateRouteTable,
    DisassociateRouteTableResponse (DisassociateRouteTableResponse'),
    newDisassociateRouteTableResponse,

    -- ** DisassociateSubnetCidrBlock
    DisassociateSubnetCidrBlock (DisassociateSubnetCidrBlock'),
    newDisassociateSubnetCidrBlock,
    DisassociateSubnetCidrBlockResponse (DisassociateSubnetCidrBlockResponse'),
    newDisassociateSubnetCidrBlockResponse,

    -- ** DisassociateTransitGatewayMulticastDomain
    DisassociateTransitGatewayMulticastDomain (DisassociateTransitGatewayMulticastDomain'),
    newDisassociateTransitGatewayMulticastDomain,
    DisassociateTransitGatewayMulticastDomainResponse (DisassociateTransitGatewayMulticastDomainResponse'),
    newDisassociateTransitGatewayMulticastDomainResponse,

    -- ** DisassociateTransitGatewayPolicyTable
    DisassociateTransitGatewayPolicyTable (DisassociateTransitGatewayPolicyTable'),
    newDisassociateTransitGatewayPolicyTable,
    DisassociateTransitGatewayPolicyTableResponse (DisassociateTransitGatewayPolicyTableResponse'),
    newDisassociateTransitGatewayPolicyTableResponse,

    -- ** DisassociateTransitGatewayRouteTable
    DisassociateTransitGatewayRouteTable (DisassociateTransitGatewayRouteTable'),
    newDisassociateTransitGatewayRouteTable,
    DisassociateTransitGatewayRouteTableResponse (DisassociateTransitGatewayRouteTableResponse'),
    newDisassociateTransitGatewayRouteTableResponse,

    -- ** DisassociateTrunkInterface
    DisassociateTrunkInterface (DisassociateTrunkInterface'),
    newDisassociateTrunkInterface,
    DisassociateTrunkInterfaceResponse (DisassociateTrunkInterfaceResponse'),
    newDisassociateTrunkInterfaceResponse,

    -- ** DisassociateVpcCidrBlock
    DisassociateVpcCidrBlock (DisassociateVpcCidrBlock'),
    newDisassociateVpcCidrBlock,
    DisassociateVpcCidrBlockResponse (DisassociateVpcCidrBlockResponse'),
    newDisassociateVpcCidrBlockResponse,

    -- ** EnableAddressTransfer
    EnableAddressTransfer (EnableAddressTransfer'),
    newEnableAddressTransfer,
    EnableAddressTransferResponse (EnableAddressTransferResponse'),
    newEnableAddressTransferResponse,

    -- ** EnableAwsNetworkPerformanceMetricSubscription
    EnableAwsNetworkPerformanceMetricSubscription (EnableAwsNetworkPerformanceMetricSubscription'),
    newEnableAwsNetworkPerformanceMetricSubscription,
    EnableAwsNetworkPerformanceMetricSubscriptionResponse (EnableAwsNetworkPerformanceMetricSubscriptionResponse'),
    newEnableAwsNetworkPerformanceMetricSubscriptionResponse,

    -- ** EnableEbsEncryptionByDefault
    EnableEbsEncryptionByDefault (EnableEbsEncryptionByDefault'),
    newEnableEbsEncryptionByDefault,
    EnableEbsEncryptionByDefaultResponse (EnableEbsEncryptionByDefaultResponse'),
    newEnableEbsEncryptionByDefaultResponse,

    -- ** EnableFastLaunch
    EnableFastLaunch (EnableFastLaunch'),
    newEnableFastLaunch,
    EnableFastLaunchResponse (EnableFastLaunchResponse'),
    newEnableFastLaunchResponse,

    -- ** EnableFastSnapshotRestores
    EnableFastSnapshotRestores (EnableFastSnapshotRestores'),
    newEnableFastSnapshotRestores,
    EnableFastSnapshotRestoresResponse (EnableFastSnapshotRestoresResponse'),
    newEnableFastSnapshotRestoresResponse,

    -- ** EnableImageDeprecation
    EnableImageDeprecation (EnableImageDeprecation'),
    newEnableImageDeprecation,
    EnableImageDeprecationResponse (EnableImageDeprecationResponse'),
    newEnableImageDeprecationResponse,

    -- ** EnableIpamOrganizationAdminAccount
    EnableIpamOrganizationAdminAccount (EnableIpamOrganizationAdminAccount'),
    newEnableIpamOrganizationAdminAccount,
    EnableIpamOrganizationAdminAccountResponse (EnableIpamOrganizationAdminAccountResponse'),
    newEnableIpamOrganizationAdminAccountResponse,

    -- ** EnableReachabilityAnalyzerOrganizationSharing
    EnableReachabilityAnalyzerOrganizationSharing (EnableReachabilityAnalyzerOrganizationSharing'),
    newEnableReachabilityAnalyzerOrganizationSharing,
    EnableReachabilityAnalyzerOrganizationSharingResponse (EnableReachabilityAnalyzerOrganizationSharingResponse'),
    newEnableReachabilityAnalyzerOrganizationSharingResponse,

    -- ** EnableSerialConsoleAccess
    EnableSerialConsoleAccess (EnableSerialConsoleAccess'),
    newEnableSerialConsoleAccess,
    EnableSerialConsoleAccessResponse (EnableSerialConsoleAccessResponse'),
    newEnableSerialConsoleAccessResponse,

    -- ** EnableTransitGatewayRouteTablePropagation
    EnableTransitGatewayRouteTablePropagation (EnableTransitGatewayRouteTablePropagation'),
    newEnableTransitGatewayRouteTablePropagation,
    EnableTransitGatewayRouteTablePropagationResponse (EnableTransitGatewayRouteTablePropagationResponse'),
    newEnableTransitGatewayRouteTablePropagationResponse,

    -- ** EnableVgwRoutePropagation
    EnableVgwRoutePropagation (EnableVgwRoutePropagation'),
    newEnableVgwRoutePropagation,
    EnableVgwRoutePropagationResponse (EnableVgwRoutePropagationResponse'),
    newEnableVgwRoutePropagationResponse,

    -- ** EnableVolumeIO
    EnableVolumeIO (EnableVolumeIO'),
    newEnableVolumeIO,
    EnableVolumeIOResponse (EnableVolumeIOResponse'),
    newEnableVolumeIOResponse,

    -- ** EnableVpcClassicLink
    EnableVpcClassicLink (EnableVpcClassicLink'),
    newEnableVpcClassicLink,
    EnableVpcClassicLinkResponse (EnableVpcClassicLinkResponse'),
    newEnableVpcClassicLinkResponse,

    -- ** EnableVpcClassicLinkDnsSupport
    EnableVpcClassicLinkDnsSupport (EnableVpcClassicLinkDnsSupport'),
    newEnableVpcClassicLinkDnsSupport,
    EnableVpcClassicLinkDnsSupportResponse (EnableVpcClassicLinkDnsSupportResponse'),
    newEnableVpcClassicLinkDnsSupportResponse,

    -- ** ExportClientVpnClientCertificateRevocationList
    ExportClientVpnClientCertificateRevocationList (ExportClientVpnClientCertificateRevocationList'),
    newExportClientVpnClientCertificateRevocationList,
    ExportClientVpnClientCertificateRevocationListResponse (ExportClientVpnClientCertificateRevocationListResponse'),
    newExportClientVpnClientCertificateRevocationListResponse,

    -- ** ExportClientVpnClientConfiguration
    ExportClientVpnClientConfiguration (ExportClientVpnClientConfiguration'),
    newExportClientVpnClientConfiguration,
    ExportClientVpnClientConfigurationResponse (ExportClientVpnClientConfigurationResponse'),
    newExportClientVpnClientConfigurationResponse,

    -- ** ExportImage
    ExportImage (ExportImage'),
    newExportImage,
    ExportImageResponse (ExportImageResponse'),
    newExportImageResponse,

    -- ** ExportTransitGatewayRoutes
    ExportTransitGatewayRoutes (ExportTransitGatewayRoutes'),
    newExportTransitGatewayRoutes,
    ExportTransitGatewayRoutesResponse (ExportTransitGatewayRoutesResponse'),
    newExportTransitGatewayRoutesResponse,

    -- ** GetAssociatedEnclaveCertificateIamRoles
    GetAssociatedEnclaveCertificateIamRoles (GetAssociatedEnclaveCertificateIamRoles'),
    newGetAssociatedEnclaveCertificateIamRoles,
    GetAssociatedEnclaveCertificateIamRolesResponse (GetAssociatedEnclaveCertificateIamRolesResponse'),
    newGetAssociatedEnclaveCertificateIamRolesResponse,

    -- ** GetAssociatedIpv6PoolCidrs (Paginated)
    GetAssociatedIpv6PoolCidrs (GetAssociatedIpv6PoolCidrs'),
    newGetAssociatedIpv6PoolCidrs,
    GetAssociatedIpv6PoolCidrsResponse (GetAssociatedIpv6PoolCidrsResponse'),
    newGetAssociatedIpv6PoolCidrsResponse,

    -- ** GetAwsNetworkPerformanceData (Paginated)
    GetAwsNetworkPerformanceData (GetAwsNetworkPerformanceData'),
    newGetAwsNetworkPerformanceData,
    GetAwsNetworkPerformanceDataResponse (GetAwsNetworkPerformanceDataResponse'),
    newGetAwsNetworkPerformanceDataResponse,

    -- ** GetCapacityReservationUsage
    GetCapacityReservationUsage (GetCapacityReservationUsage'),
    newGetCapacityReservationUsage,
    GetCapacityReservationUsageResponse (GetCapacityReservationUsageResponse'),
    newGetCapacityReservationUsageResponse,

    -- ** GetCoipPoolUsage
    GetCoipPoolUsage (GetCoipPoolUsage'),
    newGetCoipPoolUsage,
    GetCoipPoolUsageResponse (GetCoipPoolUsageResponse'),
    newGetCoipPoolUsageResponse,

    -- ** GetConsoleOutput
    GetConsoleOutput (GetConsoleOutput'),
    newGetConsoleOutput,
    GetConsoleOutputResponse (GetConsoleOutputResponse'),
    newGetConsoleOutputResponse,

    -- ** GetConsoleScreenshot
    GetConsoleScreenshot (GetConsoleScreenshot'),
    newGetConsoleScreenshot,
    GetConsoleScreenshotResponse (GetConsoleScreenshotResponse'),
    newGetConsoleScreenshotResponse,

    -- ** GetDefaultCreditSpecification
    GetDefaultCreditSpecification (GetDefaultCreditSpecification'),
    newGetDefaultCreditSpecification,
    GetDefaultCreditSpecificationResponse (GetDefaultCreditSpecificationResponse'),
    newGetDefaultCreditSpecificationResponse,

    -- ** GetEbsDefaultKmsKeyId
    GetEbsDefaultKmsKeyId (GetEbsDefaultKmsKeyId'),
    newGetEbsDefaultKmsKeyId,
    GetEbsDefaultKmsKeyIdResponse (GetEbsDefaultKmsKeyIdResponse'),
    newGetEbsDefaultKmsKeyIdResponse,

    -- ** GetEbsEncryptionByDefault
    GetEbsEncryptionByDefault (GetEbsEncryptionByDefault'),
    newGetEbsEncryptionByDefault,
    GetEbsEncryptionByDefaultResponse (GetEbsEncryptionByDefaultResponse'),
    newGetEbsEncryptionByDefaultResponse,

    -- ** GetFlowLogsIntegrationTemplate
    GetFlowLogsIntegrationTemplate (GetFlowLogsIntegrationTemplate'),
    newGetFlowLogsIntegrationTemplate,
    GetFlowLogsIntegrationTemplateResponse (GetFlowLogsIntegrationTemplateResponse'),
    newGetFlowLogsIntegrationTemplateResponse,

    -- ** GetGroupsForCapacityReservation (Paginated)
    GetGroupsForCapacityReservation (GetGroupsForCapacityReservation'),
    newGetGroupsForCapacityReservation,
    GetGroupsForCapacityReservationResponse (GetGroupsForCapacityReservationResponse'),
    newGetGroupsForCapacityReservationResponse,

    -- ** GetHostReservationPurchasePreview
    GetHostReservationPurchasePreview (GetHostReservationPurchasePreview'),
    newGetHostReservationPurchasePreview,
    GetHostReservationPurchasePreviewResponse (GetHostReservationPurchasePreviewResponse'),
    newGetHostReservationPurchasePreviewResponse,

    -- ** GetInstanceTypesFromInstanceRequirements (Paginated)
    GetInstanceTypesFromInstanceRequirements (GetInstanceTypesFromInstanceRequirements'),
    newGetInstanceTypesFromInstanceRequirements,
    GetInstanceTypesFromInstanceRequirementsResponse (GetInstanceTypesFromInstanceRequirementsResponse'),
    newGetInstanceTypesFromInstanceRequirementsResponse,

    -- ** GetInstanceUefiData
    GetInstanceUefiData (GetInstanceUefiData'),
    newGetInstanceUefiData,
    GetInstanceUefiDataResponse (GetInstanceUefiDataResponse'),
    newGetInstanceUefiDataResponse,

    -- ** GetIpamAddressHistory (Paginated)
    GetIpamAddressHistory (GetIpamAddressHistory'),
    newGetIpamAddressHistory,
    GetIpamAddressHistoryResponse (GetIpamAddressHistoryResponse'),
    newGetIpamAddressHistoryResponse,

    -- ** GetIpamDiscoveredAccounts (Paginated)
    GetIpamDiscoveredAccounts (GetIpamDiscoveredAccounts'),
    newGetIpamDiscoveredAccounts,
    GetIpamDiscoveredAccountsResponse (GetIpamDiscoveredAccountsResponse'),
    newGetIpamDiscoveredAccountsResponse,

    -- ** GetIpamDiscoveredResourceCidrs (Paginated)
    GetIpamDiscoveredResourceCidrs (GetIpamDiscoveredResourceCidrs'),
    newGetIpamDiscoveredResourceCidrs,
    GetIpamDiscoveredResourceCidrsResponse (GetIpamDiscoveredResourceCidrsResponse'),
    newGetIpamDiscoveredResourceCidrsResponse,

    -- ** GetIpamPoolAllocations (Paginated)
    GetIpamPoolAllocations (GetIpamPoolAllocations'),
    newGetIpamPoolAllocations,
    GetIpamPoolAllocationsResponse (GetIpamPoolAllocationsResponse'),
    newGetIpamPoolAllocationsResponse,

    -- ** GetIpamPoolCidrs (Paginated)
    GetIpamPoolCidrs (GetIpamPoolCidrs'),
    newGetIpamPoolCidrs,
    GetIpamPoolCidrsResponse (GetIpamPoolCidrsResponse'),
    newGetIpamPoolCidrsResponse,

    -- ** GetIpamResourceCidrs (Paginated)
    GetIpamResourceCidrs (GetIpamResourceCidrs'),
    newGetIpamResourceCidrs,
    GetIpamResourceCidrsResponse (GetIpamResourceCidrsResponse'),
    newGetIpamResourceCidrsResponse,

    -- ** GetLaunchTemplateData
    GetLaunchTemplateData (GetLaunchTemplateData'),
    newGetLaunchTemplateData,
    GetLaunchTemplateDataResponse (GetLaunchTemplateDataResponse'),
    newGetLaunchTemplateDataResponse,

    -- ** GetManagedPrefixListAssociations (Paginated)
    GetManagedPrefixListAssociations (GetManagedPrefixListAssociations'),
    newGetManagedPrefixListAssociations,
    GetManagedPrefixListAssociationsResponse (GetManagedPrefixListAssociationsResponse'),
    newGetManagedPrefixListAssociationsResponse,

    -- ** GetManagedPrefixListEntries (Paginated)
    GetManagedPrefixListEntries (GetManagedPrefixListEntries'),
    newGetManagedPrefixListEntries,
    GetManagedPrefixListEntriesResponse (GetManagedPrefixListEntriesResponse'),
    newGetManagedPrefixListEntriesResponse,

    -- ** GetNetworkInsightsAccessScopeAnalysisFindings (Paginated)
    GetNetworkInsightsAccessScopeAnalysisFindings (GetNetworkInsightsAccessScopeAnalysisFindings'),
    newGetNetworkInsightsAccessScopeAnalysisFindings,
    GetNetworkInsightsAccessScopeAnalysisFindingsResponse (GetNetworkInsightsAccessScopeAnalysisFindingsResponse'),
    newGetNetworkInsightsAccessScopeAnalysisFindingsResponse,

    -- ** GetNetworkInsightsAccessScopeContent
    GetNetworkInsightsAccessScopeContent (GetNetworkInsightsAccessScopeContent'),
    newGetNetworkInsightsAccessScopeContent,
    GetNetworkInsightsAccessScopeContentResponse (GetNetworkInsightsAccessScopeContentResponse'),
    newGetNetworkInsightsAccessScopeContentResponse,

    -- ** GetPasswordData
    GetPasswordData (GetPasswordData'),
    newGetPasswordData,
    GetPasswordDataResponse (GetPasswordDataResponse'),
    newGetPasswordDataResponse,

    -- ** GetReservedInstancesExchangeQuote
    GetReservedInstancesExchangeQuote (GetReservedInstancesExchangeQuote'),
    newGetReservedInstancesExchangeQuote,
    GetReservedInstancesExchangeQuoteResponse (GetReservedInstancesExchangeQuoteResponse'),
    newGetReservedInstancesExchangeQuoteResponse,

    -- ** GetSerialConsoleAccessStatus
    GetSerialConsoleAccessStatus (GetSerialConsoleAccessStatus'),
    newGetSerialConsoleAccessStatus,
    GetSerialConsoleAccessStatusResponse (GetSerialConsoleAccessStatusResponse'),
    newGetSerialConsoleAccessStatusResponse,

    -- ** GetSpotPlacementScores (Paginated)
    GetSpotPlacementScores (GetSpotPlacementScores'),
    newGetSpotPlacementScores,
    GetSpotPlacementScoresResponse (GetSpotPlacementScoresResponse'),
    newGetSpotPlacementScoresResponse,

    -- ** GetSubnetCidrReservations
    GetSubnetCidrReservations (GetSubnetCidrReservations'),
    newGetSubnetCidrReservations,
    GetSubnetCidrReservationsResponse (GetSubnetCidrReservationsResponse'),
    newGetSubnetCidrReservationsResponse,

    -- ** GetTransitGatewayAttachmentPropagations (Paginated)
    GetTransitGatewayAttachmentPropagations (GetTransitGatewayAttachmentPropagations'),
    newGetTransitGatewayAttachmentPropagations,
    GetTransitGatewayAttachmentPropagationsResponse (GetTransitGatewayAttachmentPropagationsResponse'),
    newGetTransitGatewayAttachmentPropagationsResponse,

    -- ** GetTransitGatewayMulticastDomainAssociations (Paginated)
    GetTransitGatewayMulticastDomainAssociations (GetTransitGatewayMulticastDomainAssociations'),
    newGetTransitGatewayMulticastDomainAssociations,
    GetTransitGatewayMulticastDomainAssociationsResponse (GetTransitGatewayMulticastDomainAssociationsResponse'),
    newGetTransitGatewayMulticastDomainAssociationsResponse,

    -- ** GetTransitGatewayPolicyTableAssociations (Paginated)
    GetTransitGatewayPolicyTableAssociations (GetTransitGatewayPolicyTableAssociations'),
    newGetTransitGatewayPolicyTableAssociations,
    GetTransitGatewayPolicyTableAssociationsResponse (GetTransitGatewayPolicyTableAssociationsResponse'),
    newGetTransitGatewayPolicyTableAssociationsResponse,

    -- ** GetTransitGatewayPolicyTableEntries
    GetTransitGatewayPolicyTableEntries (GetTransitGatewayPolicyTableEntries'),
    newGetTransitGatewayPolicyTableEntries,
    GetTransitGatewayPolicyTableEntriesResponse (GetTransitGatewayPolicyTableEntriesResponse'),
    newGetTransitGatewayPolicyTableEntriesResponse,

    -- ** GetTransitGatewayPrefixListReferences (Paginated)
    GetTransitGatewayPrefixListReferences (GetTransitGatewayPrefixListReferences'),
    newGetTransitGatewayPrefixListReferences,
    GetTransitGatewayPrefixListReferencesResponse (GetTransitGatewayPrefixListReferencesResponse'),
    newGetTransitGatewayPrefixListReferencesResponse,

    -- ** GetTransitGatewayRouteTableAssociations (Paginated)
    GetTransitGatewayRouteTableAssociations (GetTransitGatewayRouteTableAssociations'),
    newGetTransitGatewayRouteTableAssociations,
    GetTransitGatewayRouteTableAssociationsResponse (GetTransitGatewayRouteTableAssociationsResponse'),
    newGetTransitGatewayRouteTableAssociationsResponse,

    -- ** GetTransitGatewayRouteTablePropagations (Paginated)
    GetTransitGatewayRouteTablePropagations (GetTransitGatewayRouteTablePropagations'),
    newGetTransitGatewayRouteTablePropagations,
    GetTransitGatewayRouteTablePropagationsResponse (GetTransitGatewayRouteTablePropagationsResponse'),
    newGetTransitGatewayRouteTablePropagationsResponse,

    -- ** GetVerifiedAccessEndpointPolicy
    GetVerifiedAccessEndpointPolicy (GetVerifiedAccessEndpointPolicy'),
    newGetVerifiedAccessEndpointPolicy,
    GetVerifiedAccessEndpointPolicyResponse (GetVerifiedAccessEndpointPolicyResponse'),
    newGetVerifiedAccessEndpointPolicyResponse,

    -- ** GetVerifiedAccessGroupPolicy
    GetVerifiedAccessGroupPolicy (GetVerifiedAccessGroupPolicy'),
    newGetVerifiedAccessGroupPolicy,
    GetVerifiedAccessGroupPolicyResponse (GetVerifiedAccessGroupPolicyResponse'),
    newGetVerifiedAccessGroupPolicyResponse,

    -- ** GetVpnConnectionDeviceSampleConfiguration
    GetVpnConnectionDeviceSampleConfiguration (GetVpnConnectionDeviceSampleConfiguration'),
    newGetVpnConnectionDeviceSampleConfiguration,
    GetVpnConnectionDeviceSampleConfigurationResponse (GetVpnConnectionDeviceSampleConfigurationResponse'),
    newGetVpnConnectionDeviceSampleConfigurationResponse,

    -- ** GetVpnConnectionDeviceTypes (Paginated)
    GetVpnConnectionDeviceTypes (GetVpnConnectionDeviceTypes'),
    newGetVpnConnectionDeviceTypes,
    GetVpnConnectionDeviceTypesResponse (GetVpnConnectionDeviceTypesResponse'),
    newGetVpnConnectionDeviceTypesResponse,

    -- ** GetVpnTunnelReplacementStatus
    GetVpnTunnelReplacementStatus (GetVpnTunnelReplacementStatus'),
    newGetVpnTunnelReplacementStatus,
    GetVpnTunnelReplacementStatusResponse (GetVpnTunnelReplacementStatusResponse'),
    newGetVpnTunnelReplacementStatusResponse,

    -- ** ImportClientVpnClientCertificateRevocationList
    ImportClientVpnClientCertificateRevocationList (ImportClientVpnClientCertificateRevocationList'),
    newImportClientVpnClientCertificateRevocationList,
    ImportClientVpnClientCertificateRevocationListResponse (ImportClientVpnClientCertificateRevocationListResponse'),
    newImportClientVpnClientCertificateRevocationListResponse,

    -- ** ImportImage
    ImportImage (ImportImage'),
    newImportImage,
    ImportImageResponse (ImportImageResponse'),
    newImportImageResponse,

    -- ** ImportInstance
    ImportInstance (ImportInstance'),
    newImportInstance,
    ImportInstanceResponse (ImportInstanceResponse'),
    newImportInstanceResponse,

    -- ** ImportKeyPair
    ImportKeyPair (ImportKeyPair'),
    newImportKeyPair,
    ImportKeyPairResponse (ImportKeyPairResponse'),
    newImportKeyPairResponse,

    -- ** ImportSnapshot
    ImportSnapshot (ImportSnapshot'),
    newImportSnapshot,
    ImportSnapshotResponse (ImportSnapshotResponse'),
    newImportSnapshotResponse,

    -- ** ImportVolume
    ImportVolume (ImportVolume'),
    newImportVolume,
    ImportVolumeResponse (ImportVolumeResponse'),
    newImportVolumeResponse,

    -- ** ListImagesInRecycleBin (Paginated)
    ListImagesInRecycleBin (ListImagesInRecycleBin'),
    newListImagesInRecycleBin,
    ListImagesInRecycleBinResponse (ListImagesInRecycleBinResponse'),
    newListImagesInRecycleBinResponse,

    -- ** ListSnapshotsInRecycleBin (Paginated)
    ListSnapshotsInRecycleBin (ListSnapshotsInRecycleBin'),
    newListSnapshotsInRecycleBin,
    ListSnapshotsInRecycleBinResponse (ListSnapshotsInRecycleBinResponse'),
    newListSnapshotsInRecycleBinResponse,

    -- ** ModifyAddressAttribute
    ModifyAddressAttribute (ModifyAddressAttribute'),
    newModifyAddressAttribute,
    ModifyAddressAttributeResponse (ModifyAddressAttributeResponse'),
    newModifyAddressAttributeResponse,

    -- ** ModifyAvailabilityZoneGroup
    ModifyAvailabilityZoneGroup (ModifyAvailabilityZoneGroup'),
    newModifyAvailabilityZoneGroup,
    ModifyAvailabilityZoneGroupResponse (ModifyAvailabilityZoneGroupResponse'),
    newModifyAvailabilityZoneGroupResponse,

    -- ** ModifyCapacityReservation
    ModifyCapacityReservation (ModifyCapacityReservation'),
    newModifyCapacityReservation,
    ModifyCapacityReservationResponse (ModifyCapacityReservationResponse'),
    newModifyCapacityReservationResponse,

    -- ** ModifyCapacityReservationFleet
    ModifyCapacityReservationFleet (ModifyCapacityReservationFleet'),
    newModifyCapacityReservationFleet,
    ModifyCapacityReservationFleetResponse (ModifyCapacityReservationFleetResponse'),
    newModifyCapacityReservationFleetResponse,

    -- ** ModifyClientVpnEndpoint
    ModifyClientVpnEndpoint (ModifyClientVpnEndpoint'),
    newModifyClientVpnEndpoint,
    ModifyClientVpnEndpointResponse (ModifyClientVpnEndpointResponse'),
    newModifyClientVpnEndpointResponse,

    -- ** ModifyDefaultCreditSpecification
    ModifyDefaultCreditSpecification (ModifyDefaultCreditSpecification'),
    newModifyDefaultCreditSpecification,
    ModifyDefaultCreditSpecificationResponse (ModifyDefaultCreditSpecificationResponse'),
    newModifyDefaultCreditSpecificationResponse,

    -- ** ModifyEbsDefaultKmsKeyId
    ModifyEbsDefaultKmsKeyId (ModifyEbsDefaultKmsKeyId'),
    newModifyEbsDefaultKmsKeyId,
    ModifyEbsDefaultKmsKeyIdResponse (ModifyEbsDefaultKmsKeyIdResponse'),
    newModifyEbsDefaultKmsKeyIdResponse,

    -- ** ModifyFleet
    ModifyFleet (ModifyFleet'),
    newModifyFleet,
    ModifyFleetResponse (ModifyFleetResponse'),
    newModifyFleetResponse,

    -- ** ModifyFpgaImageAttribute
    ModifyFpgaImageAttribute (ModifyFpgaImageAttribute'),
    newModifyFpgaImageAttribute,
    ModifyFpgaImageAttributeResponse (ModifyFpgaImageAttributeResponse'),
    newModifyFpgaImageAttributeResponse,

    -- ** ModifyHosts
    ModifyHosts (ModifyHosts'),
    newModifyHosts,
    ModifyHostsResponse (ModifyHostsResponse'),
    newModifyHostsResponse,

    -- ** ModifyIdFormat
    ModifyIdFormat (ModifyIdFormat'),
    newModifyIdFormat,
    ModifyIdFormatResponse (ModifyIdFormatResponse'),
    newModifyIdFormatResponse,

    -- ** ModifyIdentityIdFormat
    ModifyIdentityIdFormat (ModifyIdentityIdFormat'),
    newModifyIdentityIdFormat,
    ModifyIdentityIdFormatResponse (ModifyIdentityIdFormatResponse'),
    newModifyIdentityIdFormatResponse,

    -- ** ModifyImageAttribute
    ModifyImageAttribute (ModifyImageAttribute'),
    newModifyImageAttribute,
    ModifyImageAttributeResponse (ModifyImageAttributeResponse'),
    newModifyImageAttributeResponse,

    -- ** ModifyInstanceAttribute
    ModifyInstanceAttribute (ModifyInstanceAttribute'),
    newModifyInstanceAttribute,
    ModifyInstanceAttributeResponse (ModifyInstanceAttributeResponse'),
    newModifyInstanceAttributeResponse,

    -- ** ModifyInstanceCapacityReservationAttributes
    ModifyInstanceCapacityReservationAttributes (ModifyInstanceCapacityReservationAttributes'),
    newModifyInstanceCapacityReservationAttributes,
    ModifyInstanceCapacityReservationAttributesResponse (ModifyInstanceCapacityReservationAttributesResponse'),
    newModifyInstanceCapacityReservationAttributesResponse,

    -- ** ModifyInstanceCreditSpecification
    ModifyInstanceCreditSpecification (ModifyInstanceCreditSpecification'),
    newModifyInstanceCreditSpecification,
    ModifyInstanceCreditSpecificationResponse (ModifyInstanceCreditSpecificationResponse'),
    newModifyInstanceCreditSpecificationResponse,

    -- ** ModifyInstanceEventStartTime
    ModifyInstanceEventStartTime (ModifyInstanceEventStartTime'),
    newModifyInstanceEventStartTime,
    ModifyInstanceEventStartTimeResponse (ModifyInstanceEventStartTimeResponse'),
    newModifyInstanceEventStartTimeResponse,

    -- ** ModifyInstanceEventWindow
    ModifyInstanceEventWindow (ModifyInstanceEventWindow'),
    newModifyInstanceEventWindow,
    ModifyInstanceEventWindowResponse (ModifyInstanceEventWindowResponse'),
    newModifyInstanceEventWindowResponse,

    -- ** ModifyInstanceMaintenanceOptions
    ModifyInstanceMaintenanceOptions (ModifyInstanceMaintenanceOptions'),
    newModifyInstanceMaintenanceOptions,
    ModifyInstanceMaintenanceOptionsResponse (ModifyInstanceMaintenanceOptionsResponse'),
    newModifyInstanceMaintenanceOptionsResponse,

    -- ** ModifyInstanceMetadataOptions
    ModifyInstanceMetadataOptions (ModifyInstanceMetadataOptions'),
    newModifyInstanceMetadataOptions,
    ModifyInstanceMetadataOptionsResponse (ModifyInstanceMetadataOptionsResponse'),
    newModifyInstanceMetadataOptionsResponse,

    -- ** ModifyInstancePlacement
    ModifyInstancePlacement (ModifyInstancePlacement'),
    newModifyInstancePlacement,
    ModifyInstancePlacementResponse (ModifyInstancePlacementResponse'),
    newModifyInstancePlacementResponse,

    -- ** ModifyIpam
    ModifyIpam (ModifyIpam'),
    newModifyIpam,
    ModifyIpamResponse (ModifyIpamResponse'),
    newModifyIpamResponse,

    -- ** ModifyIpamPool
    ModifyIpamPool (ModifyIpamPool'),
    newModifyIpamPool,
    ModifyIpamPoolResponse (ModifyIpamPoolResponse'),
    newModifyIpamPoolResponse,

    -- ** ModifyIpamResourceCidr
    ModifyIpamResourceCidr (ModifyIpamResourceCidr'),
    newModifyIpamResourceCidr,
    ModifyIpamResourceCidrResponse (ModifyIpamResourceCidrResponse'),
    newModifyIpamResourceCidrResponse,

    -- ** ModifyIpamResourceDiscovery
    ModifyIpamResourceDiscovery (ModifyIpamResourceDiscovery'),
    newModifyIpamResourceDiscovery,
    ModifyIpamResourceDiscoveryResponse (ModifyIpamResourceDiscoveryResponse'),
    newModifyIpamResourceDiscoveryResponse,

    -- ** ModifyIpamScope
    ModifyIpamScope (ModifyIpamScope'),
    newModifyIpamScope,
    ModifyIpamScopeResponse (ModifyIpamScopeResponse'),
    newModifyIpamScopeResponse,

    -- ** ModifyLaunchTemplate
    ModifyLaunchTemplate (ModifyLaunchTemplate'),
    newModifyLaunchTemplate,
    ModifyLaunchTemplateResponse (ModifyLaunchTemplateResponse'),
    newModifyLaunchTemplateResponse,

    -- ** ModifyLocalGatewayRoute
    ModifyLocalGatewayRoute (ModifyLocalGatewayRoute'),
    newModifyLocalGatewayRoute,
    ModifyLocalGatewayRouteResponse (ModifyLocalGatewayRouteResponse'),
    newModifyLocalGatewayRouteResponse,

    -- ** ModifyManagedPrefixList
    ModifyManagedPrefixList (ModifyManagedPrefixList'),
    newModifyManagedPrefixList,
    ModifyManagedPrefixListResponse (ModifyManagedPrefixListResponse'),
    newModifyManagedPrefixListResponse,

    -- ** ModifyNetworkInterfaceAttribute
    ModifyNetworkInterfaceAttribute (ModifyNetworkInterfaceAttribute'),
    newModifyNetworkInterfaceAttribute,
    ModifyNetworkInterfaceAttributeResponse (ModifyNetworkInterfaceAttributeResponse'),
    newModifyNetworkInterfaceAttributeResponse,

    -- ** ModifyPrivateDnsNameOptions
    ModifyPrivateDnsNameOptions (ModifyPrivateDnsNameOptions'),
    newModifyPrivateDnsNameOptions,
    ModifyPrivateDnsNameOptionsResponse (ModifyPrivateDnsNameOptionsResponse'),
    newModifyPrivateDnsNameOptionsResponse,

    -- ** ModifyReservedInstances
    ModifyReservedInstances (ModifyReservedInstances'),
    newModifyReservedInstances,
    ModifyReservedInstancesResponse (ModifyReservedInstancesResponse'),
    newModifyReservedInstancesResponse,

    -- ** ModifySecurityGroupRules
    ModifySecurityGroupRules (ModifySecurityGroupRules'),
    newModifySecurityGroupRules,
    ModifySecurityGroupRulesResponse (ModifySecurityGroupRulesResponse'),
    newModifySecurityGroupRulesResponse,

    -- ** ModifySnapshotAttribute
    ModifySnapshotAttribute (ModifySnapshotAttribute'),
    newModifySnapshotAttribute,
    ModifySnapshotAttributeResponse (ModifySnapshotAttributeResponse'),
    newModifySnapshotAttributeResponse,

    -- ** ModifySnapshotTier
    ModifySnapshotTier (ModifySnapshotTier'),
    newModifySnapshotTier,
    ModifySnapshotTierResponse (ModifySnapshotTierResponse'),
    newModifySnapshotTierResponse,

    -- ** ModifySpotFleetRequest
    ModifySpotFleetRequest (ModifySpotFleetRequest'),
    newModifySpotFleetRequest,
    ModifySpotFleetRequestResponse (ModifySpotFleetRequestResponse'),
    newModifySpotFleetRequestResponse,

    -- ** ModifySubnetAttribute
    ModifySubnetAttribute (ModifySubnetAttribute'),
    newModifySubnetAttribute,
    ModifySubnetAttributeResponse (ModifySubnetAttributeResponse'),
    newModifySubnetAttributeResponse,

    -- ** ModifyTrafficMirrorFilterNetworkServices
    ModifyTrafficMirrorFilterNetworkServices (ModifyTrafficMirrorFilterNetworkServices'),
    newModifyTrafficMirrorFilterNetworkServices,
    ModifyTrafficMirrorFilterNetworkServicesResponse (ModifyTrafficMirrorFilterNetworkServicesResponse'),
    newModifyTrafficMirrorFilterNetworkServicesResponse,

    -- ** ModifyTrafficMirrorFilterRule
    ModifyTrafficMirrorFilterRule (ModifyTrafficMirrorFilterRule'),
    newModifyTrafficMirrorFilterRule,
    ModifyTrafficMirrorFilterRuleResponse (ModifyTrafficMirrorFilterRuleResponse'),
    newModifyTrafficMirrorFilterRuleResponse,

    -- ** ModifyTrafficMirrorSession
    ModifyTrafficMirrorSession (ModifyTrafficMirrorSession'),
    newModifyTrafficMirrorSession,
    ModifyTrafficMirrorSessionResponse (ModifyTrafficMirrorSessionResponse'),
    newModifyTrafficMirrorSessionResponse,

    -- ** ModifyTransitGateway
    ModifyTransitGateway (ModifyTransitGateway'),
    newModifyTransitGateway,
    ModifyTransitGatewayResponse (ModifyTransitGatewayResponse'),
    newModifyTransitGatewayResponse,

    -- ** ModifyTransitGatewayPrefixListReference
    ModifyTransitGatewayPrefixListReference (ModifyTransitGatewayPrefixListReference'),
    newModifyTransitGatewayPrefixListReference,
    ModifyTransitGatewayPrefixListReferenceResponse (ModifyTransitGatewayPrefixListReferenceResponse'),
    newModifyTransitGatewayPrefixListReferenceResponse,

    -- ** ModifyTransitGatewayVpcAttachment
    ModifyTransitGatewayVpcAttachment (ModifyTransitGatewayVpcAttachment'),
    newModifyTransitGatewayVpcAttachment,
    ModifyTransitGatewayVpcAttachmentResponse (ModifyTransitGatewayVpcAttachmentResponse'),
    newModifyTransitGatewayVpcAttachmentResponse,

    -- ** ModifyVerifiedAccessEndpoint
    ModifyVerifiedAccessEndpoint (ModifyVerifiedAccessEndpoint'),
    newModifyVerifiedAccessEndpoint,
    ModifyVerifiedAccessEndpointResponse (ModifyVerifiedAccessEndpointResponse'),
    newModifyVerifiedAccessEndpointResponse,

    -- ** ModifyVerifiedAccessEndpointPolicy
    ModifyVerifiedAccessEndpointPolicy (ModifyVerifiedAccessEndpointPolicy'),
    newModifyVerifiedAccessEndpointPolicy,
    ModifyVerifiedAccessEndpointPolicyResponse (ModifyVerifiedAccessEndpointPolicyResponse'),
    newModifyVerifiedAccessEndpointPolicyResponse,

    -- ** ModifyVerifiedAccessGroup
    ModifyVerifiedAccessGroup (ModifyVerifiedAccessGroup'),
    newModifyVerifiedAccessGroup,
    ModifyVerifiedAccessGroupResponse (ModifyVerifiedAccessGroupResponse'),
    newModifyVerifiedAccessGroupResponse,

    -- ** ModifyVerifiedAccessGroupPolicy
    ModifyVerifiedAccessGroupPolicy (ModifyVerifiedAccessGroupPolicy'),
    newModifyVerifiedAccessGroupPolicy,
    ModifyVerifiedAccessGroupPolicyResponse (ModifyVerifiedAccessGroupPolicyResponse'),
    newModifyVerifiedAccessGroupPolicyResponse,

    -- ** ModifyVerifiedAccessInstance
    ModifyVerifiedAccessInstance (ModifyVerifiedAccessInstance'),
    newModifyVerifiedAccessInstance,
    ModifyVerifiedAccessInstanceResponse (ModifyVerifiedAccessInstanceResponse'),
    newModifyVerifiedAccessInstanceResponse,

    -- ** ModifyVerifiedAccessInstanceLoggingConfiguration
    ModifyVerifiedAccessInstanceLoggingConfiguration (ModifyVerifiedAccessInstanceLoggingConfiguration'),
    newModifyVerifiedAccessInstanceLoggingConfiguration,
    ModifyVerifiedAccessInstanceLoggingConfigurationResponse (ModifyVerifiedAccessInstanceLoggingConfigurationResponse'),
    newModifyVerifiedAccessInstanceLoggingConfigurationResponse,

    -- ** ModifyVerifiedAccessTrustProvider
    ModifyVerifiedAccessTrustProvider (ModifyVerifiedAccessTrustProvider'),
    newModifyVerifiedAccessTrustProvider,
    ModifyVerifiedAccessTrustProviderResponse (ModifyVerifiedAccessTrustProviderResponse'),
    newModifyVerifiedAccessTrustProviderResponse,

    -- ** ModifyVolume
    ModifyVolume (ModifyVolume'),
    newModifyVolume,
    ModifyVolumeResponse (ModifyVolumeResponse'),
    newModifyVolumeResponse,

    -- ** ModifyVolumeAttribute
    ModifyVolumeAttribute (ModifyVolumeAttribute'),
    newModifyVolumeAttribute,
    ModifyVolumeAttributeResponse (ModifyVolumeAttributeResponse'),
    newModifyVolumeAttributeResponse,

    -- ** ModifyVpcAttribute
    ModifyVpcAttribute (ModifyVpcAttribute'),
    newModifyVpcAttribute,
    ModifyVpcAttributeResponse (ModifyVpcAttributeResponse'),
    newModifyVpcAttributeResponse,

    -- ** ModifyVpcEndpoint
    ModifyVpcEndpoint (ModifyVpcEndpoint'),
    newModifyVpcEndpoint,
    ModifyVpcEndpointResponse (ModifyVpcEndpointResponse'),
    newModifyVpcEndpointResponse,

    -- ** ModifyVpcEndpointConnectionNotification
    ModifyVpcEndpointConnectionNotification (ModifyVpcEndpointConnectionNotification'),
    newModifyVpcEndpointConnectionNotification,
    ModifyVpcEndpointConnectionNotificationResponse (ModifyVpcEndpointConnectionNotificationResponse'),
    newModifyVpcEndpointConnectionNotificationResponse,

    -- ** ModifyVpcEndpointServiceConfiguration
    ModifyVpcEndpointServiceConfiguration (ModifyVpcEndpointServiceConfiguration'),
    newModifyVpcEndpointServiceConfiguration,
    ModifyVpcEndpointServiceConfigurationResponse (ModifyVpcEndpointServiceConfigurationResponse'),
    newModifyVpcEndpointServiceConfigurationResponse,

    -- ** ModifyVpcEndpointServicePayerResponsibility
    ModifyVpcEndpointServicePayerResponsibility (ModifyVpcEndpointServicePayerResponsibility'),
    newModifyVpcEndpointServicePayerResponsibility,
    ModifyVpcEndpointServicePayerResponsibilityResponse (ModifyVpcEndpointServicePayerResponsibilityResponse'),
    newModifyVpcEndpointServicePayerResponsibilityResponse,

    -- ** ModifyVpcEndpointServicePermissions
    ModifyVpcEndpointServicePermissions (ModifyVpcEndpointServicePermissions'),
    newModifyVpcEndpointServicePermissions,
    ModifyVpcEndpointServicePermissionsResponse (ModifyVpcEndpointServicePermissionsResponse'),
    newModifyVpcEndpointServicePermissionsResponse,

    -- ** ModifyVpcPeeringConnectionOptions
    ModifyVpcPeeringConnectionOptions (ModifyVpcPeeringConnectionOptions'),
    newModifyVpcPeeringConnectionOptions,
    ModifyVpcPeeringConnectionOptionsResponse (ModifyVpcPeeringConnectionOptionsResponse'),
    newModifyVpcPeeringConnectionOptionsResponse,

    -- ** ModifyVpcTenancy
    ModifyVpcTenancy (ModifyVpcTenancy'),
    newModifyVpcTenancy,
    ModifyVpcTenancyResponse (ModifyVpcTenancyResponse'),
    newModifyVpcTenancyResponse,

    -- ** ModifyVpnConnection
    ModifyVpnConnection (ModifyVpnConnection'),
    newModifyVpnConnection,
    ModifyVpnConnectionResponse (ModifyVpnConnectionResponse'),
    newModifyVpnConnectionResponse,

    -- ** ModifyVpnConnectionOptions
    ModifyVpnConnectionOptions (ModifyVpnConnectionOptions'),
    newModifyVpnConnectionOptions,
    ModifyVpnConnectionOptionsResponse (ModifyVpnConnectionOptionsResponse'),
    newModifyVpnConnectionOptionsResponse,

    -- ** ModifyVpnTunnelCertificate
    ModifyVpnTunnelCertificate (ModifyVpnTunnelCertificate'),
    newModifyVpnTunnelCertificate,
    ModifyVpnTunnelCertificateResponse (ModifyVpnTunnelCertificateResponse'),
    newModifyVpnTunnelCertificateResponse,

    -- ** ModifyVpnTunnelOptions
    ModifyVpnTunnelOptions (ModifyVpnTunnelOptions'),
    newModifyVpnTunnelOptions,
    ModifyVpnTunnelOptionsResponse (ModifyVpnTunnelOptionsResponse'),
    newModifyVpnTunnelOptionsResponse,

    -- ** MonitorInstances
    MonitorInstances (MonitorInstances'),
    newMonitorInstances,
    MonitorInstancesResponse (MonitorInstancesResponse'),
    newMonitorInstancesResponse,

    -- ** MoveAddressToVpc
    MoveAddressToVpc (MoveAddressToVpc'),
    newMoveAddressToVpc,
    MoveAddressToVpcResponse (MoveAddressToVpcResponse'),
    newMoveAddressToVpcResponse,

    -- ** MoveByoipCidrToIpam
    MoveByoipCidrToIpam (MoveByoipCidrToIpam'),
    newMoveByoipCidrToIpam,
    MoveByoipCidrToIpamResponse (MoveByoipCidrToIpamResponse'),
    newMoveByoipCidrToIpamResponse,

    -- ** ProvisionByoipCidr
    ProvisionByoipCidr (ProvisionByoipCidr'),
    newProvisionByoipCidr,
    ProvisionByoipCidrResponse (ProvisionByoipCidrResponse'),
    newProvisionByoipCidrResponse,

    -- ** ProvisionIpamPoolCidr
    ProvisionIpamPoolCidr (ProvisionIpamPoolCidr'),
    newProvisionIpamPoolCidr,
    ProvisionIpamPoolCidrResponse (ProvisionIpamPoolCidrResponse'),
    newProvisionIpamPoolCidrResponse,

    -- ** ProvisionPublicIpv4PoolCidr
    ProvisionPublicIpv4PoolCidr (ProvisionPublicIpv4PoolCidr'),
    newProvisionPublicIpv4PoolCidr,
    ProvisionPublicIpv4PoolCidrResponse (ProvisionPublicIpv4PoolCidrResponse'),
    newProvisionPublicIpv4PoolCidrResponse,

    -- ** PurchaseHostReservation
    PurchaseHostReservation (PurchaseHostReservation'),
    newPurchaseHostReservation,
    PurchaseHostReservationResponse (PurchaseHostReservationResponse'),
    newPurchaseHostReservationResponse,

    -- ** PurchaseReservedInstancesOffering
    PurchaseReservedInstancesOffering (PurchaseReservedInstancesOffering'),
    newPurchaseReservedInstancesOffering,
    PurchaseReservedInstancesOfferingResponse (PurchaseReservedInstancesOfferingResponse'),
    newPurchaseReservedInstancesOfferingResponse,

    -- ** PurchaseScheduledInstances
    PurchaseScheduledInstances (PurchaseScheduledInstances'),
    newPurchaseScheduledInstances,
    PurchaseScheduledInstancesResponse (PurchaseScheduledInstancesResponse'),
    newPurchaseScheduledInstancesResponse,

    -- ** RebootInstances
    RebootInstances (RebootInstances'),
    newRebootInstances,
    RebootInstancesResponse (RebootInstancesResponse'),
    newRebootInstancesResponse,

    -- ** RegisterImage
    RegisterImage (RegisterImage'),
    newRegisterImage,
    RegisterImageResponse (RegisterImageResponse'),
    newRegisterImageResponse,

    -- ** RegisterInstanceEventNotificationAttributes
    RegisterInstanceEventNotificationAttributes (RegisterInstanceEventNotificationAttributes'),
    newRegisterInstanceEventNotificationAttributes,
    RegisterInstanceEventNotificationAttributesResponse (RegisterInstanceEventNotificationAttributesResponse'),
    newRegisterInstanceEventNotificationAttributesResponse,

    -- ** RegisterTransitGatewayMulticastGroupMembers
    RegisterTransitGatewayMulticastGroupMembers (RegisterTransitGatewayMulticastGroupMembers'),
    newRegisterTransitGatewayMulticastGroupMembers,
    RegisterTransitGatewayMulticastGroupMembersResponse (RegisterTransitGatewayMulticastGroupMembersResponse'),
    newRegisterTransitGatewayMulticastGroupMembersResponse,

    -- ** RegisterTransitGatewayMulticastGroupSources
    RegisterTransitGatewayMulticastGroupSources (RegisterTransitGatewayMulticastGroupSources'),
    newRegisterTransitGatewayMulticastGroupSources,
    RegisterTransitGatewayMulticastGroupSourcesResponse (RegisterTransitGatewayMulticastGroupSourcesResponse'),
    newRegisterTransitGatewayMulticastGroupSourcesResponse,

    -- ** RejectTransitGatewayMulticastDomainAssociations
    RejectTransitGatewayMulticastDomainAssociations (RejectTransitGatewayMulticastDomainAssociations'),
    newRejectTransitGatewayMulticastDomainAssociations,
    RejectTransitGatewayMulticastDomainAssociationsResponse (RejectTransitGatewayMulticastDomainAssociationsResponse'),
    newRejectTransitGatewayMulticastDomainAssociationsResponse,

    -- ** RejectTransitGatewayPeeringAttachment
    RejectTransitGatewayPeeringAttachment (RejectTransitGatewayPeeringAttachment'),
    newRejectTransitGatewayPeeringAttachment,
    RejectTransitGatewayPeeringAttachmentResponse (RejectTransitGatewayPeeringAttachmentResponse'),
    newRejectTransitGatewayPeeringAttachmentResponse,

    -- ** RejectTransitGatewayVpcAttachment
    RejectTransitGatewayVpcAttachment (RejectTransitGatewayVpcAttachment'),
    newRejectTransitGatewayVpcAttachment,
    RejectTransitGatewayVpcAttachmentResponse (RejectTransitGatewayVpcAttachmentResponse'),
    newRejectTransitGatewayVpcAttachmentResponse,

    -- ** RejectVpcEndpointConnections
    RejectVpcEndpointConnections (RejectVpcEndpointConnections'),
    newRejectVpcEndpointConnections,
    RejectVpcEndpointConnectionsResponse (RejectVpcEndpointConnectionsResponse'),
    newRejectVpcEndpointConnectionsResponse,

    -- ** RejectVpcPeeringConnection
    RejectVpcPeeringConnection (RejectVpcPeeringConnection'),
    newRejectVpcPeeringConnection,
    RejectVpcPeeringConnectionResponse (RejectVpcPeeringConnectionResponse'),
    newRejectVpcPeeringConnectionResponse,

    -- ** ReleaseAddress
    ReleaseAddress (ReleaseAddress'),
    newReleaseAddress,
    ReleaseAddressResponse (ReleaseAddressResponse'),
    newReleaseAddressResponse,

    -- ** ReleaseHosts
    ReleaseHosts (ReleaseHosts'),
    newReleaseHosts,
    ReleaseHostsResponse (ReleaseHostsResponse'),
    newReleaseHostsResponse,

    -- ** ReleaseIpamPoolAllocation
    ReleaseIpamPoolAllocation (ReleaseIpamPoolAllocation'),
    newReleaseIpamPoolAllocation,
    ReleaseIpamPoolAllocationResponse (ReleaseIpamPoolAllocationResponse'),
    newReleaseIpamPoolAllocationResponse,

    -- ** ReplaceIamInstanceProfileAssociation
    ReplaceIamInstanceProfileAssociation (ReplaceIamInstanceProfileAssociation'),
    newReplaceIamInstanceProfileAssociation,
    ReplaceIamInstanceProfileAssociationResponse (ReplaceIamInstanceProfileAssociationResponse'),
    newReplaceIamInstanceProfileAssociationResponse,

    -- ** ReplaceNetworkAclAssociation
    ReplaceNetworkAclAssociation (ReplaceNetworkAclAssociation'),
    newReplaceNetworkAclAssociation,
    ReplaceNetworkAclAssociationResponse (ReplaceNetworkAclAssociationResponse'),
    newReplaceNetworkAclAssociationResponse,

    -- ** ReplaceNetworkAclEntry
    ReplaceNetworkAclEntry (ReplaceNetworkAclEntry'),
    newReplaceNetworkAclEntry,
    ReplaceNetworkAclEntryResponse (ReplaceNetworkAclEntryResponse'),
    newReplaceNetworkAclEntryResponse,

    -- ** ReplaceRoute
    ReplaceRoute (ReplaceRoute'),
    newReplaceRoute,
    ReplaceRouteResponse (ReplaceRouteResponse'),
    newReplaceRouteResponse,

    -- ** ReplaceRouteTableAssociation
    ReplaceRouteTableAssociation (ReplaceRouteTableAssociation'),
    newReplaceRouteTableAssociation,
    ReplaceRouteTableAssociationResponse (ReplaceRouteTableAssociationResponse'),
    newReplaceRouteTableAssociationResponse,

    -- ** ReplaceTransitGatewayRoute
    ReplaceTransitGatewayRoute (ReplaceTransitGatewayRoute'),
    newReplaceTransitGatewayRoute,
    ReplaceTransitGatewayRouteResponse (ReplaceTransitGatewayRouteResponse'),
    newReplaceTransitGatewayRouteResponse,

    -- ** ReplaceVpnTunnel
    ReplaceVpnTunnel (ReplaceVpnTunnel'),
    newReplaceVpnTunnel,
    ReplaceVpnTunnelResponse (ReplaceVpnTunnelResponse'),
    newReplaceVpnTunnelResponse,

    -- ** ReportInstanceStatus
    ReportInstanceStatus (ReportInstanceStatus'),
    newReportInstanceStatus,
    ReportInstanceStatusResponse (ReportInstanceStatusResponse'),
    newReportInstanceStatusResponse,

    -- ** RequestSpotFleet
    RequestSpotFleet (RequestSpotFleet'),
    newRequestSpotFleet,
    RequestSpotFleetResponse (RequestSpotFleetResponse'),
    newRequestSpotFleetResponse,

    -- ** RequestSpotInstances
    RequestSpotInstances (RequestSpotInstances'),
    newRequestSpotInstances,
    RequestSpotInstancesResponse (RequestSpotInstancesResponse'),
    newRequestSpotInstancesResponse,

    -- ** ResetAddressAttribute
    ResetAddressAttribute (ResetAddressAttribute'),
    newResetAddressAttribute,
    ResetAddressAttributeResponse (ResetAddressAttributeResponse'),
    newResetAddressAttributeResponse,

    -- ** ResetEbsDefaultKmsKeyId
    ResetEbsDefaultKmsKeyId (ResetEbsDefaultKmsKeyId'),
    newResetEbsDefaultKmsKeyId,
    ResetEbsDefaultKmsKeyIdResponse (ResetEbsDefaultKmsKeyIdResponse'),
    newResetEbsDefaultKmsKeyIdResponse,

    -- ** ResetFpgaImageAttribute
    ResetFpgaImageAttribute (ResetFpgaImageAttribute'),
    newResetFpgaImageAttribute,
    ResetFpgaImageAttributeResponse (ResetFpgaImageAttributeResponse'),
    newResetFpgaImageAttributeResponse,

    -- ** ResetImageAttribute
    ResetImageAttribute (ResetImageAttribute'),
    newResetImageAttribute,
    ResetImageAttributeResponse (ResetImageAttributeResponse'),
    newResetImageAttributeResponse,

    -- ** ResetInstanceAttribute
    ResetInstanceAttribute (ResetInstanceAttribute'),
    newResetInstanceAttribute,
    ResetInstanceAttributeResponse (ResetInstanceAttributeResponse'),
    newResetInstanceAttributeResponse,

    -- ** ResetNetworkInterfaceAttribute
    ResetNetworkInterfaceAttribute (ResetNetworkInterfaceAttribute'),
    newResetNetworkInterfaceAttribute,
    ResetNetworkInterfaceAttributeResponse (ResetNetworkInterfaceAttributeResponse'),
    newResetNetworkInterfaceAttributeResponse,

    -- ** ResetSnapshotAttribute
    ResetSnapshotAttribute (ResetSnapshotAttribute'),
    newResetSnapshotAttribute,
    ResetSnapshotAttributeResponse (ResetSnapshotAttributeResponse'),
    newResetSnapshotAttributeResponse,

    -- ** RestoreAddressToClassic
    RestoreAddressToClassic (RestoreAddressToClassic'),
    newRestoreAddressToClassic,
    RestoreAddressToClassicResponse (RestoreAddressToClassicResponse'),
    newRestoreAddressToClassicResponse,

    -- ** RestoreImageFromRecycleBin
    RestoreImageFromRecycleBin (RestoreImageFromRecycleBin'),
    newRestoreImageFromRecycleBin,
    RestoreImageFromRecycleBinResponse (RestoreImageFromRecycleBinResponse'),
    newRestoreImageFromRecycleBinResponse,

    -- ** RestoreManagedPrefixListVersion
    RestoreManagedPrefixListVersion (RestoreManagedPrefixListVersion'),
    newRestoreManagedPrefixListVersion,
    RestoreManagedPrefixListVersionResponse (RestoreManagedPrefixListVersionResponse'),
    newRestoreManagedPrefixListVersionResponse,

    -- ** RestoreSnapshotFromRecycleBin
    RestoreSnapshotFromRecycleBin (RestoreSnapshotFromRecycleBin'),
    newRestoreSnapshotFromRecycleBin,
    RestoreSnapshotFromRecycleBinResponse (RestoreSnapshotFromRecycleBinResponse'),
    newRestoreSnapshotFromRecycleBinResponse,

    -- ** RestoreSnapshotTier
    RestoreSnapshotTier (RestoreSnapshotTier'),
    newRestoreSnapshotTier,
    RestoreSnapshotTierResponse (RestoreSnapshotTierResponse'),
    newRestoreSnapshotTierResponse,

    -- ** RevokeClientVpnIngress
    RevokeClientVpnIngress (RevokeClientVpnIngress'),
    newRevokeClientVpnIngress,
    RevokeClientVpnIngressResponse (RevokeClientVpnIngressResponse'),
    newRevokeClientVpnIngressResponse,

    -- ** RevokeSecurityGroupEgress
    RevokeSecurityGroupEgress (RevokeSecurityGroupEgress'),
    newRevokeSecurityGroupEgress,
    RevokeSecurityGroupEgressResponse (RevokeSecurityGroupEgressResponse'),
    newRevokeSecurityGroupEgressResponse,

    -- ** RevokeSecurityGroupIngress
    RevokeSecurityGroupIngress (RevokeSecurityGroupIngress'),
    newRevokeSecurityGroupIngress,
    RevokeSecurityGroupIngressResponse (RevokeSecurityGroupIngressResponse'),
    newRevokeSecurityGroupIngressResponse,

    -- ** RunInstances
    RunInstances (RunInstances'),
    newRunInstances,
    Reservation (Reservation'),
    newReservation,

    -- ** RunScheduledInstances
    RunScheduledInstances (RunScheduledInstances'),
    newRunScheduledInstances,
    RunScheduledInstancesResponse (RunScheduledInstancesResponse'),
    newRunScheduledInstancesResponse,

    -- ** SearchLocalGatewayRoutes (Paginated)
    SearchLocalGatewayRoutes (SearchLocalGatewayRoutes'),
    newSearchLocalGatewayRoutes,
    SearchLocalGatewayRoutesResponse (SearchLocalGatewayRoutesResponse'),
    newSearchLocalGatewayRoutesResponse,

    -- ** SearchTransitGatewayMulticastGroups (Paginated)
    SearchTransitGatewayMulticastGroups (SearchTransitGatewayMulticastGroups'),
    newSearchTransitGatewayMulticastGroups,
    SearchTransitGatewayMulticastGroupsResponse (SearchTransitGatewayMulticastGroupsResponse'),
    newSearchTransitGatewayMulticastGroupsResponse,

    -- ** SearchTransitGatewayRoutes
    SearchTransitGatewayRoutes (SearchTransitGatewayRoutes'),
    newSearchTransitGatewayRoutes,
    SearchTransitGatewayRoutesResponse (SearchTransitGatewayRoutesResponse'),
    newSearchTransitGatewayRoutesResponse,

    -- ** SendDiagnosticInterrupt
    SendDiagnosticInterrupt (SendDiagnosticInterrupt'),
    newSendDiagnosticInterrupt,
    SendDiagnosticInterruptResponse (SendDiagnosticInterruptResponse'),
    newSendDiagnosticInterruptResponse,

    -- ** StartInstances
    StartInstances (StartInstances'),
    newStartInstances,
    StartInstancesResponse (StartInstancesResponse'),
    newStartInstancesResponse,

    -- ** StartNetworkInsightsAccessScopeAnalysis
    StartNetworkInsightsAccessScopeAnalysis (StartNetworkInsightsAccessScopeAnalysis'),
    newStartNetworkInsightsAccessScopeAnalysis,
    StartNetworkInsightsAccessScopeAnalysisResponse (StartNetworkInsightsAccessScopeAnalysisResponse'),
    newStartNetworkInsightsAccessScopeAnalysisResponse,

    -- ** StartNetworkInsightsAnalysis
    StartNetworkInsightsAnalysis (StartNetworkInsightsAnalysis'),
    newStartNetworkInsightsAnalysis,
    StartNetworkInsightsAnalysisResponse (StartNetworkInsightsAnalysisResponse'),
    newStartNetworkInsightsAnalysisResponse,

    -- ** StartVpcEndpointServicePrivateDnsVerification
    StartVpcEndpointServicePrivateDnsVerification (StartVpcEndpointServicePrivateDnsVerification'),
    newStartVpcEndpointServicePrivateDnsVerification,
    StartVpcEndpointServicePrivateDnsVerificationResponse (StartVpcEndpointServicePrivateDnsVerificationResponse'),
    newStartVpcEndpointServicePrivateDnsVerificationResponse,

    -- ** StopInstances
    StopInstances (StopInstances'),
    newStopInstances,
    StopInstancesResponse (StopInstancesResponse'),
    newStopInstancesResponse,

    -- ** TerminateClientVpnConnections
    TerminateClientVpnConnections (TerminateClientVpnConnections'),
    newTerminateClientVpnConnections,
    TerminateClientVpnConnectionsResponse (TerminateClientVpnConnectionsResponse'),
    newTerminateClientVpnConnectionsResponse,

    -- ** TerminateInstances
    TerminateInstances (TerminateInstances'),
    newTerminateInstances,
    TerminateInstancesResponse (TerminateInstancesResponse'),
    newTerminateInstancesResponse,

    -- ** UnassignIpv6Addresses
    UnassignIpv6Addresses (UnassignIpv6Addresses'),
    newUnassignIpv6Addresses,
    UnassignIpv6AddressesResponse (UnassignIpv6AddressesResponse'),
    newUnassignIpv6AddressesResponse,

    -- ** UnassignPrivateIpAddresses
    UnassignPrivateIpAddresses (UnassignPrivateIpAddresses'),
    newUnassignPrivateIpAddresses,
    UnassignPrivateIpAddressesResponse (UnassignPrivateIpAddressesResponse'),
    newUnassignPrivateIpAddressesResponse,

    -- ** UnassignPrivateNatGatewayAddress
    UnassignPrivateNatGatewayAddress (UnassignPrivateNatGatewayAddress'),
    newUnassignPrivateNatGatewayAddress,
    UnassignPrivateNatGatewayAddressResponse (UnassignPrivateNatGatewayAddressResponse'),
    newUnassignPrivateNatGatewayAddressResponse,

    -- ** UnmonitorInstances
    UnmonitorInstances (UnmonitorInstances'),
    newUnmonitorInstances,
    UnmonitorInstancesResponse (UnmonitorInstancesResponse'),
    newUnmonitorInstancesResponse,

    -- ** UpdateSecurityGroupRuleDescriptionsEgress
    UpdateSecurityGroupRuleDescriptionsEgress (UpdateSecurityGroupRuleDescriptionsEgress'),
    newUpdateSecurityGroupRuleDescriptionsEgress,
    UpdateSecurityGroupRuleDescriptionsEgressResponse (UpdateSecurityGroupRuleDescriptionsEgressResponse'),
    newUpdateSecurityGroupRuleDescriptionsEgressResponse,

    -- ** UpdateSecurityGroupRuleDescriptionsIngress
    UpdateSecurityGroupRuleDescriptionsIngress (UpdateSecurityGroupRuleDescriptionsIngress'),
    newUpdateSecurityGroupRuleDescriptionsIngress,
    UpdateSecurityGroupRuleDescriptionsIngressResponse (UpdateSecurityGroupRuleDescriptionsIngressResponse'),
    newUpdateSecurityGroupRuleDescriptionsIngressResponse,

    -- ** WithdrawByoipCidr
    WithdrawByoipCidr (WithdrawByoipCidr'),
    newWithdrawByoipCidr,
    WithdrawByoipCidrResponse (WithdrawByoipCidrResponse'),
    newWithdrawByoipCidrResponse,

    -- * Types

    -- ** Common
    module Amazonka.EC2.Internal,

    -- ** AcceleratorManufacturer
    AcceleratorManufacturer (..),

    -- ** AcceleratorName
    AcceleratorName (..),

    -- ** AcceleratorType
    AcceleratorType (..),

    -- ** AccountAttributeName
    AccountAttributeName (..),

    -- ** ActivityStatus
    ActivityStatus (..),

    -- ** AddressAttributeName
    AddressAttributeName (..),

    -- ** AddressFamily
    AddressFamily (..),

    -- ** AddressStatus
    AddressStatus (..),

    -- ** AddressTransferStatus
    AddressTransferStatus (..),

    -- ** Affinity
    Affinity (..),

    -- ** AllocationState
    AllocationState (..),

    -- ** AllocationStrategy
    AllocationStrategy (..),

    -- ** AllocationType
    AllocationType (..),

    -- ** AllowsMultipleInstanceTypes
    AllowsMultipleInstanceTypes (..),

    -- ** AmdSevSnpSpecification
    AmdSevSnpSpecification (..),

    -- ** AnalysisStatus
    AnalysisStatus (..),

    -- ** ApplianceModeSupportValue
    ApplianceModeSupportValue (..),

    -- ** ArchitectureType
    ArchitectureType (..),

    -- ** ArchitectureValues
    ArchitectureValues (..),

    -- ** AssociatedNetworkType
    AssociatedNetworkType (..),

    -- ** AssociationStatusCode
    AssociationStatusCode (..),

    -- ** AttachmentStatus
    AttachmentStatus (..),

    -- ** AutoAcceptSharedAssociationsValue
    AutoAcceptSharedAssociationsValue (..),

    -- ** AutoAcceptSharedAttachmentsValue
    AutoAcceptSharedAttachmentsValue (..),

    -- ** AutoPlacement
    AutoPlacement (..),

    -- ** AvailabilityZoneOptInStatus
    AvailabilityZoneOptInStatus (..),

    -- ** AvailabilityZoneState
    AvailabilityZoneState (..),

    -- ** BareMetal
    BareMetal (..),

    -- ** BatchState
    BatchState (..),

    -- ** BgpStatus
    BgpStatus (..),

    -- ** BootModeType
    BootModeType (..),

    -- ** BootModeValues
    BootModeValues (..),

    -- ** BundleTaskState
    BundleTaskState (..),

    -- ** BurstablePerformance
    BurstablePerformance (..),

    -- ** ByoipCidrState
    ByoipCidrState (..),

    -- ** CancelBatchErrorCode
    CancelBatchErrorCode (..),

    -- ** CancelSpotInstanceRequestState
    CancelSpotInstanceRequestState (..),

    -- ** CapacityReservationFleetState
    CapacityReservationFleetState (..),

    -- ** CapacityReservationInstancePlatform
    CapacityReservationInstancePlatform (..),

    -- ** CapacityReservationPreference
    CapacityReservationPreference (..),

    -- ** CapacityReservationState
    CapacityReservationState (..),

    -- ** CapacityReservationTenancy
    CapacityReservationTenancy (..),

    -- ** CarrierGatewayState
    CarrierGatewayState (..),

    -- ** ClientCertificateRevocationListStatusCode
    ClientCertificateRevocationListStatusCode (..),

    -- ** ClientVpnAuthenticationType
    ClientVpnAuthenticationType (..),

    -- ** ClientVpnAuthorizationRuleStatusCode
    ClientVpnAuthorizationRuleStatusCode (..),

    -- ** ClientVpnConnectionStatusCode
    ClientVpnConnectionStatusCode (..),

    -- ** ClientVpnEndpointAttributeStatusCode
    ClientVpnEndpointAttributeStatusCode (..),

    -- ** ClientVpnEndpointStatusCode
    ClientVpnEndpointStatusCode (..),

    -- ** ClientVpnRouteStatusCode
    ClientVpnRouteStatusCode (..),

    -- ** ConnectionNotificationState
    ConnectionNotificationState (..),

    -- ** ConnectionNotificationType
    ConnectionNotificationType (..),

    -- ** ConnectivityType
    ConnectivityType (..),

    -- ** ContainerFormat
    ContainerFormat (..),

    -- ** ConversionTaskState
    ConversionTaskState (..),

    -- ** CopyTagsFromSource
    CopyTagsFromSource (..),

    -- ** CpuManufacturer
    CpuManufacturer (..),

    -- ** CurrencyCodeValues
    CurrencyCodeValues (..),

    -- ** DatafeedSubscriptionState
    DatafeedSubscriptionState (..),

    -- ** DefaultRouteTableAssociationValue
    DefaultRouteTableAssociationValue (..),

    -- ** DefaultRouteTablePropagationValue
    DefaultRouteTablePropagationValue (..),

    -- ** DefaultTargetCapacityType
    DefaultTargetCapacityType (..),

    -- ** DeleteFleetErrorCode
    DeleteFleetErrorCode (..),

    -- ** DeleteQueuedReservedInstancesErrorCode
    DeleteQueuedReservedInstancesErrorCode (..),

    -- ** DestinationFileFormat
    DestinationFileFormat (..),

    -- ** DeviceTrustProviderType
    DeviceTrustProviderType (..),

    -- ** DeviceType
    DeviceType (..),

    -- ** DiskImageFormat
    DiskImageFormat (..),

    -- ** DiskType
    DiskType (..),

    -- ** DnsNameState
    DnsNameState (..),

    -- ** DnsRecordIpType
    DnsRecordIpType (..),

    -- ** DnsSupportValue
    DnsSupportValue (..),

    -- ** DomainType
    DomainType (..),

    -- ** DynamicRoutingValue
    DynamicRoutingValue (..),

    -- ** EbsEncryptionSupport
    EbsEncryptionSupport (..),

    -- ** EbsNvmeSupport
    EbsNvmeSupport (..),

    -- ** EbsOptimizedSupport
    EbsOptimizedSupport (..),

    -- ** Ec2InstanceConnectEndpointState
    Ec2InstanceConnectEndpointState (..),

    -- ** ElasticGpuState
    ElasticGpuState (..),

    -- ** ElasticGpuStatus
    ElasticGpuStatus (..),

    -- ** EnaSupport
    EnaSupport (..),

    -- ** EndDateType
    EndDateType (..),

    -- ** EphemeralNvmeSupport
    EphemeralNvmeSupport (..),

    -- ** EventCode
    EventCode (..),

    -- ** EventType
    EventType (..),

    -- ** ExcessCapacityTerminationPolicy
    ExcessCapacityTerminationPolicy (..),

    -- ** ExportEnvironment
    ExportEnvironment (..),

    -- ** ExportTaskState
    ExportTaskState (..),

    -- ** FastLaunchResourceType
    FastLaunchResourceType (..),

    -- ** FastLaunchStateCode
    FastLaunchStateCode (..),

    -- ** FastSnapshotRestoreStateCode
    FastSnapshotRestoreStateCode (..),

    -- ** FindingsFound
    FindingsFound (..),

    -- ** FleetActivityStatus
    FleetActivityStatus (..),

    -- ** FleetCapacityReservationTenancy
    FleetCapacityReservationTenancy (..),

    -- ** FleetCapacityReservationUsageStrategy
    FleetCapacityReservationUsageStrategy (..),

    -- ** FleetEventType
    FleetEventType (..),

    -- ** FleetExcessCapacityTerminationPolicy
    FleetExcessCapacityTerminationPolicy (..),

    -- ** FleetInstanceMatchCriteria
    FleetInstanceMatchCriteria (..),

    -- ** FleetOnDemandAllocationStrategy
    FleetOnDemandAllocationStrategy (..),

    -- ** FleetReplacementStrategy
    FleetReplacementStrategy (..),

    -- ** FleetStateCode
    FleetStateCode (..),

    -- ** FleetType
    FleetType (..),

    -- ** FlowLogsResourceType
    FlowLogsResourceType (..),

    -- ** FpgaImageAttributeName
    FpgaImageAttributeName (..),

    -- ** FpgaImageStateCode
    FpgaImageStateCode (..),

    -- ** GatewayAssociationState
    GatewayAssociationState (..),

    -- ** GatewayType
    GatewayType (..),

    -- ** HostMaintenance
    HostMaintenance (..),

    -- ** HostRecovery
    HostRecovery (..),

    -- ** HostTenancy
    HostTenancy (..),

    -- ** HostnameType
    HostnameType (..),

    -- ** HttpTokensState
    HttpTokensState (..),

    -- ** HypervisorType
    HypervisorType (..),

    -- ** IamInstanceProfileAssociationState
    IamInstanceProfileAssociationState (..),

    -- ** Igmpv2SupportValue
    Igmpv2SupportValue (..),

    -- ** ImageAttributeName
    ImageAttributeName (..),

    -- ** ImageState
    ImageState (..),

    -- ** ImageTypeValues
    ImageTypeValues (..),

    -- ** ImdsSupportValues
    ImdsSupportValues (..),

    -- ** InstanceAttributeName
    InstanceAttributeName (..),

    -- ** InstanceAutoRecoveryState
    InstanceAutoRecoveryState (..),

    -- ** InstanceBootModeValues
    InstanceBootModeValues (..),

    -- ** InstanceEventWindowState
    InstanceEventWindowState (..),

    -- ** InstanceGeneration
    InstanceGeneration (..),

    -- ** InstanceHealthStatus
    InstanceHealthStatus (..),

    -- ** InstanceInterruptionBehavior
    InstanceInterruptionBehavior (..),

    -- ** InstanceLifecycle
    InstanceLifecycle (..),

    -- ** InstanceLifecycleType
    InstanceLifecycleType (..),

    -- ** InstanceMatchCriteria
    InstanceMatchCriteria (..),

    -- ** InstanceMetadataEndpointState
    InstanceMetadataEndpointState (..),

    -- ** InstanceMetadataOptionsState
    InstanceMetadataOptionsState (..),

    -- ** InstanceMetadataProtocolState
    InstanceMetadataProtocolState (..),

    -- ** InstanceMetadataTagsState
    InstanceMetadataTagsState (..),

    -- ** InstanceStateName
    InstanceStateName (..),

    -- ** InstanceStorageEncryptionSupport
    InstanceStorageEncryptionSupport (..),

    -- ** InstanceType
    InstanceType (..),

    -- ** InstanceTypeHypervisor
    InstanceTypeHypervisor (..),

    -- ** InterfacePermissionType
    InterfacePermissionType (..),

    -- ** InterfaceProtocolType
    InterfaceProtocolType (..),

    -- ** IpAddressType
    IpAddressType (..),

    -- ** IpamAddressHistoryResourceType
    IpamAddressHistoryResourceType (..),

    -- ** IpamAssociatedResourceDiscoveryStatus
    IpamAssociatedResourceDiscoveryStatus (..),

    -- ** IpamComplianceStatus
    IpamComplianceStatus (..),

    -- ** IpamDiscoveryFailureCode
    IpamDiscoveryFailureCode (..),

    -- ** IpamManagementState
    IpamManagementState (..),

    -- ** IpamOverlapStatus
    IpamOverlapStatus (..),

    -- ** IpamPoolAllocationResourceType
    IpamPoolAllocationResourceType (..),

    -- ** IpamPoolAwsService
    IpamPoolAwsService (..),

    -- ** IpamPoolCidrFailureCode
    IpamPoolCidrFailureCode (..),

    -- ** IpamPoolCidrState
    IpamPoolCidrState (..),

    -- ** IpamPoolPublicIpSource
    IpamPoolPublicIpSource (..),

    -- ** IpamPoolState
    IpamPoolState (..),

    -- ** IpamResourceDiscoveryAssociationState
    IpamResourceDiscoveryAssociationState (..),

    -- ** IpamResourceDiscoveryState
    IpamResourceDiscoveryState (..),

    -- ** IpamResourceType
    IpamResourceType (..),

    -- ** IpamScopeState
    IpamScopeState (..),

    -- ** IpamScopeType
    IpamScopeType (..),

    -- ** IpamState
    IpamState (..),

    -- ** Ipv6SupportValue
    Ipv6SupportValue (..),

    -- ** KeyFormat
    KeyFormat (..),

    -- ** KeyType
    KeyType (..),

    -- ** LaunchTemplateAutoRecoveryState
    LaunchTemplateAutoRecoveryState (..),

    -- ** LaunchTemplateErrorCode
    LaunchTemplateErrorCode (..),

    -- ** LaunchTemplateHttpTokensState
    LaunchTemplateHttpTokensState (..),

    -- ** LaunchTemplateInstanceMetadataEndpointState
    LaunchTemplateInstanceMetadataEndpointState (..),

    -- ** LaunchTemplateInstanceMetadataOptionsState
    LaunchTemplateInstanceMetadataOptionsState (..),

    -- ** LaunchTemplateInstanceMetadataProtocolIpv6
    LaunchTemplateInstanceMetadataProtocolIpv6 (..),

    -- ** LaunchTemplateInstanceMetadataTagsState
    LaunchTemplateInstanceMetadataTagsState (..),

    -- ** ListingState
    ListingState (..),

    -- ** ListingStatus
    ListingStatus (..),

    -- ** LocalGatewayRouteState
    LocalGatewayRouteState (..),

    -- ** LocalGatewayRouteTableMode
    LocalGatewayRouteTableMode (..),

    -- ** LocalGatewayRouteType
    LocalGatewayRouteType (..),

    -- ** LocalStorage
    LocalStorage (..),

    -- ** LocalStorageType
    LocalStorageType (..),

    -- ** LocationType
    LocationType (..),

    -- ** LogDestinationType
    LogDestinationType (..),

    -- ** MarketType
    MarketType (..),

    -- ** MembershipType
    MembershipType (..),

    -- ** MetricType
    MetricType (..),

    -- ** ModifyAvailabilityZoneOptInStatus
    ModifyAvailabilityZoneOptInStatus (..),

    -- ** MonitoringState
    MonitoringState (..),

    -- ** MoveStatus
    MoveStatus (..),

    -- ** MulticastSupportValue
    MulticastSupportValue (..),

    -- ** NatGatewayAddressStatus
    NatGatewayAddressStatus (..),

    -- ** NatGatewayState
    NatGatewayState (..),

    -- ** NetworkInterfaceAttribute
    NetworkInterfaceAttribute (..),

    -- ** NetworkInterfaceCreationType
    NetworkInterfaceCreationType (..),

    -- ** NetworkInterfacePermissionStateCode
    NetworkInterfacePermissionStateCode (..),

    -- ** NetworkInterfaceStatus
    NetworkInterfaceStatus (..),

    -- ** NetworkInterfaceType
    NetworkInterfaceType (..),

    -- ** OfferingClassType
    OfferingClassType (..),

    -- ** OfferingTypeValues
    OfferingTypeValues (..),

    -- ** OnDemandAllocationStrategy
    OnDemandAllocationStrategy (..),

    -- ** OperationType
    OperationType (..),

    -- ** PartitionLoadFrequency
    PartitionLoadFrequency (..),

    -- ** PayerResponsibility
    PayerResponsibility (..),

    -- ** PaymentOption
    PaymentOption (..),

    -- ** PeriodType
    PeriodType (..),

    -- ** PermissionGroup
    PermissionGroup (..),

    -- ** PlacementGroupState
    PlacementGroupState (..),

    -- ** PlacementGroupStrategy
    PlacementGroupStrategy (..),

    -- ** PlacementStrategy
    PlacementStrategy (..),

    -- ** PlatformValues
    PlatformValues (..),

    -- ** PrefixListState
    PrefixListState (..),

    -- ** PrincipalType
    PrincipalType (..),

    -- ** ProductCodeValues
    ProductCodeValues (..),

    -- ** Protocol
    Protocol (..),

    -- ** ProtocolValue
    ProtocolValue (..),

    -- ** RIProductDescription
    RIProductDescription (..),

    -- ** RecurringChargeFrequency
    RecurringChargeFrequency (..),

    -- ** ReplaceRootVolumeTaskState
    ReplaceRootVolumeTaskState (..),

    -- ** ReplacementStrategy
    ReplacementStrategy (..),

    -- ** ReportInstanceReasonCodes
    ReportInstanceReasonCodes (..),

    -- ** ReportStatusType
    ReportStatusType (..),

    -- ** ReservationState
    ReservationState (..),

    -- ** ReservedInstanceState
    ReservedInstanceState (..),

    -- ** ResetFpgaImageAttributeName
    ResetFpgaImageAttributeName (..),

    -- ** ResetImageAttributeName
    ResetImageAttributeName (..),

    -- ** ResourceType
    ResourceType (..),

    -- ** RootDeviceType
    RootDeviceType (..),

    -- ** RouteOrigin
    RouteOrigin (..),

    -- ** RouteState
    RouteState (..),

    -- ** RouteTableAssociationStateCode
    RouteTableAssociationStateCode (..),

    -- ** RuleAction
    RuleAction (..),

    -- ** Scope
    Scope (..),

    -- ** SelfServicePortal
    SelfServicePortal (..),

    -- ** ServiceConnectivityType
    ServiceConnectivityType (..),

    -- ** ServiceState
    ServiceState (..),

    -- ** ServiceType
    ServiceType (..),

    -- ** ShutdownBehavior
    ShutdownBehavior (..),

    -- ** SnapshotAttributeName
    SnapshotAttributeName (..),

    -- ** SnapshotState
    SnapshotState (..),

    -- ** SpotAllocationStrategy
    SpotAllocationStrategy (..),

    -- ** SpotInstanceInterruptionBehavior
    SpotInstanceInterruptionBehavior (..),

    -- ** SpotInstanceState
    SpotInstanceState (..),

    -- ** SpotInstanceType
    SpotInstanceType (..),

    -- ** SpreadLevel
    SpreadLevel (..),

    -- ** State
    State (..),

    -- ** StaticSourcesSupportValue
    StaticSourcesSupportValue (..),

    -- ** StatisticType
    StatisticType (..),

    -- ** StatusName
    StatusName (..),

    -- ** StatusType
    StatusType (..),

    -- ** StorageTier
    StorageTier (..),

    -- ** SubnetCidrBlockStateCode
    SubnetCidrBlockStateCode (..),

    -- ** SubnetCidrReservationType
    SubnetCidrReservationType (..),

    -- ** SubnetState
    SubnetState (..),

    -- ** SummaryStatus
    SummaryStatus (..),

    -- ** SupportedAdditionalProcessorFeature
    SupportedAdditionalProcessorFeature (..),

    -- ** TargetCapacityUnitType
    TargetCapacityUnitType (..),

    -- ** TargetStorageTier
    TargetStorageTier (..),

    -- ** TelemetryStatus
    TelemetryStatus (..),

    -- ** Tenancy
    Tenancy (..),

    -- ** TieringOperationStatus
    TieringOperationStatus (..),

    -- ** TpmSupportValues
    TpmSupportValues (..),

    -- ** TrafficDirection
    TrafficDirection (..),

    -- ** TrafficMirrorFilterRuleField
    TrafficMirrorFilterRuleField (..),

    -- ** TrafficMirrorNetworkService
    TrafficMirrorNetworkService (..),

    -- ** TrafficMirrorRuleAction
    TrafficMirrorRuleAction (..),

    -- ** TrafficMirrorSessionField
    TrafficMirrorSessionField (..),

    -- ** TrafficMirrorTargetType
    TrafficMirrorTargetType (..),

    -- ** TrafficType
    TrafficType (..),

    -- ** TransitGatewayAssociationState
    TransitGatewayAssociationState (..),

    -- ** TransitGatewayAttachmentResourceType
    TransitGatewayAttachmentResourceType (..),

    -- ** TransitGatewayAttachmentState
    TransitGatewayAttachmentState (..),

    -- ** TransitGatewayConnectPeerState
    TransitGatewayConnectPeerState (..),

    -- ** TransitGatewayMulitcastDomainAssociationState
    TransitGatewayMulitcastDomainAssociationState (..),

    -- ** TransitGatewayMulticastDomainState
    TransitGatewayMulticastDomainState (..),

    -- ** TransitGatewayPolicyTableState
    TransitGatewayPolicyTableState (..),

    -- ** TransitGatewayPrefixListReferenceState
    TransitGatewayPrefixListReferenceState (..),

    -- ** TransitGatewayPropagationState
    TransitGatewayPropagationState (..),

    -- ** TransitGatewayRouteState
    TransitGatewayRouteState (..),

    -- ** TransitGatewayRouteTableAnnouncementDirection
    TransitGatewayRouteTableAnnouncementDirection (..),

    -- ** TransitGatewayRouteTableAnnouncementState
    TransitGatewayRouteTableAnnouncementState (..),

    -- ** TransitGatewayRouteTableState
    TransitGatewayRouteTableState (..),

    -- ** TransitGatewayRouteType
    TransitGatewayRouteType (..),

    -- ** TransitGatewayState
    TransitGatewayState (..),

    -- ** TransportProtocol
    TransportProtocol (..),

    -- ** TrustProviderType
    TrustProviderType (..),

    -- ** TunnelInsideIpVersion
    TunnelInsideIpVersion (..),

    -- ** UnlimitedSupportedInstanceFamily
    UnlimitedSupportedInstanceFamily (..),

    -- ** UnsuccessfulInstanceCreditSpecificationErrorCode
    UnsuccessfulInstanceCreditSpecificationErrorCode (..),

    -- ** UsageClassType
    UsageClassType (..),

    -- ** UserTrustProviderType
    UserTrustProviderType (..),

    -- ** VerifiedAccessEndpointAttachmentType
    VerifiedAccessEndpointAttachmentType (..),

    -- ** VerifiedAccessEndpointProtocol
    VerifiedAccessEndpointProtocol (..),

    -- ** VerifiedAccessEndpointStatusCode
    VerifiedAccessEndpointStatusCode (..),

    -- ** VerifiedAccessEndpointType
    VerifiedAccessEndpointType (..),

    -- ** VerifiedAccessLogDeliveryStatusCode
    VerifiedAccessLogDeliveryStatusCode (..),

    -- ** VirtualizationType
    VirtualizationType (..),

    -- ** VolumeAttachmentState
    VolumeAttachmentState (..),

    -- ** VolumeAttributeName
    VolumeAttributeName (..),

    -- ** VolumeModificationState
    VolumeModificationState (..),

    -- ** VolumeState
    VolumeState (..),

    -- ** VolumeStatusInfoStatus
    VolumeStatusInfoStatus (..),

    -- ** VolumeStatusName
    VolumeStatusName (..),

    -- ** VolumeType
    VolumeType (..),

    -- ** VpcAttributeName
    VpcAttributeName (..),

    -- ** VpcCidrBlockStateCode
    VpcCidrBlockStateCode (..),

    -- ** VpcEndpointType
    VpcEndpointType (..),

    -- ** VpcPeeringConnectionStateReasonCode
    VpcPeeringConnectionStateReasonCode (..),

    -- ** VpcState
    VpcState (..),

    -- ** VpcTenancy
    VpcTenancy (..),

    -- ** VpnEcmpSupportValue
    VpnEcmpSupportValue (..),

    -- ** VpnProtocol
    VpnProtocol (..),

    -- ** VpnState
    VpnState (..),

    -- ** VpnStaticRouteSource
    VpnStaticRouteSource (..),

    -- ** WeekDay
    WeekDay (..),

    -- ** AcceleratorCount
    AcceleratorCount (AcceleratorCount'),
    newAcceleratorCount,

    -- ** AcceleratorCountRequest
    AcceleratorCountRequest (AcceleratorCountRequest'),
    newAcceleratorCountRequest,

    -- ** AcceleratorTotalMemoryMiB
    AcceleratorTotalMemoryMiB (AcceleratorTotalMemoryMiB'),
    newAcceleratorTotalMemoryMiB,

    -- ** AcceleratorTotalMemoryMiBRequest
    AcceleratorTotalMemoryMiBRequest (AcceleratorTotalMemoryMiBRequest'),
    newAcceleratorTotalMemoryMiBRequest,

    -- ** AccessScopeAnalysisFinding
    AccessScopeAnalysisFinding (AccessScopeAnalysisFinding'),
    newAccessScopeAnalysisFinding,

    -- ** AccessScopePath
    AccessScopePath (AccessScopePath'),
    newAccessScopePath,

    -- ** AccessScopePathRequest
    AccessScopePathRequest (AccessScopePathRequest'),
    newAccessScopePathRequest,

    -- ** AccountAttribute
    AccountAttribute (AccountAttribute'),
    newAccountAttribute,

    -- ** AccountAttributeValue
    AccountAttributeValue (AccountAttributeValue'),
    newAccountAttributeValue,

    -- ** ActiveInstance
    ActiveInstance (ActiveInstance'),
    newActiveInstance,

    -- ** AddIpamOperatingRegion
    AddIpamOperatingRegion (AddIpamOperatingRegion'),
    newAddIpamOperatingRegion,

    -- ** AddPrefixListEntry
    AddPrefixListEntry (AddPrefixListEntry'),
    newAddPrefixListEntry,

    -- ** AddedPrincipal
    AddedPrincipal (AddedPrincipal'),
    newAddedPrincipal,

    -- ** AdditionalDetail
    AdditionalDetail (AdditionalDetail'),
    newAdditionalDetail,

    -- ** Address
    Address (Address'),
    newAddress,

    -- ** AddressAttribute
    AddressAttribute (AddressAttribute'),
    newAddressAttribute,

    -- ** AddressTransfer
    AddressTransfer (AddressTransfer'),
    newAddressTransfer,

    -- ** AllowedPrincipal
    AllowedPrincipal (AllowedPrincipal'),
    newAllowedPrincipal,

    -- ** AlternatePathHint
    AlternatePathHint (AlternatePathHint'),
    newAlternatePathHint,

    -- ** AnalysisAclRule
    AnalysisAclRule (AnalysisAclRule'),
    newAnalysisAclRule,

    -- ** AnalysisComponent
    AnalysisComponent (AnalysisComponent'),
    newAnalysisComponent,

    -- ** AnalysisLoadBalancerListener
    AnalysisLoadBalancerListener (AnalysisLoadBalancerListener'),
    newAnalysisLoadBalancerListener,

    -- ** AnalysisLoadBalancerTarget
    AnalysisLoadBalancerTarget (AnalysisLoadBalancerTarget'),
    newAnalysisLoadBalancerTarget,

    -- ** AnalysisPacketHeader
    AnalysisPacketHeader (AnalysisPacketHeader'),
    newAnalysisPacketHeader,

    -- ** AnalysisRouteTableRoute
    AnalysisRouteTableRoute (AnalysisRouteTableRoute'),
    newAnalysisRouteTableRoute,

    -- ** AnalysisSecurityGroupRule
    AnalysisSecurityGroupRule (AnalysisSecurityGroupRule'),
    newAnalysisSecurityGroupRule,

    -- ** AssignedPrivateIpAddress
    AssignedPrivateIpAddress (AssignedPrivateIpAddress'),
    newAssignedPrivateIpAddress,

    -- ** AssociatedRole
    AssociatedRole (AssociatedRole'),
    newAssociatedRole,

    -- ** AssociatedTargetNetwork
    AssociatedTargetNetwork (AssociatedTargetNetwork'),
    newAssociatedTargetNetwork,

    -- ** AssociationStatus
    AssociationStatus (AssociationStatus'),
    newAssociationStatus,

    -- ** AthenaIntegration
    AthenaIntegration (AthenaIntegration'),
    newAthenaIntegration,

    -- ** AttachmentEnaSrdSpecification
    AttachmentEnaSrdSpecification (AttachmentEnaSrdSpecification'),
    newAttachmentEnaSrdSpecification,

    -- ** AttachmentEnaSrdUdpSpecification
    AttachmentEnaSrdUdpSpecification (AttachmentEnaSrdUdpSpecification'),
    newAttachmentEnaSrdUdpSpecification,

    -- ** AttributeBooleanValue
    AttributeBooleanValue (AttributeBooleanValue'),
    newAttributeBooleanValue,

    -- ** AttributeValue
    AttributeValue (AttributeValue'),
    newAttributeValue,

    -- ** AuthorizationRule
    AuthorizationRule (AuthorizationRule'),
    newAuthorizationRule,

    -- ** AvailabilityZone
    AvailabilityZone (AvailabilityZone'),
    newAvailabilityZone,

    -- ** AvailabilityZoneMessage
    AvailabilityZoneMessage (AvailabilityZoneMessage'),
    newAvailabilityZoneMessage,

    -- ** AvailableCapacity
    AvailableCapacity (AvailableCapacity'),
    newAvailableCapacity,

    -- ** BaselineEbsBandwidthMbps
    BaselineEbsBandwidthMbps (BaselineEbsBandwidthMbps'),
    newBaselineEbsBandwidthMbps,

    -- ** BaselineEbsBandwidthMbpsRequest
    BaselineEbsBandwidthMbpsRequest (BaselineEbsBandwidthMbpsRequest'),
    newBaselineEbsBandwidthMbpsRequest,

    -- ** BlobAttributeValue
    BlobAttributeValue (BlobAttributeValue'),
    newBlobAttributeValue,

    -- ** BlockDeviceMapping
    BlockDeviceMapping (BlockDeviceMapping'),
    newBlockDeviceMapping,

    -- ** BundleTask
    BundleTask (BundleTask'),
    newBundleTask,

    -- ** BundleTaskError
    BundleTaskError (BundleTaskError'),
    newBundleTaskError,

    -- ** ByoipCidr
    ByoipCidr (ByoipCidr'),
    newByoipCidr,

    -- ** CancelCapacityReservationFleetError
    CancelCapacityReservationFleetError (CancelCapacityReservationFleetError'),
    newCancelCapacityReservationFleetError,

    -- ** CancelSpotFleetRequestsError
    CancelSpotFleetRequestsError (CancelSpotFleetRequestsError'),
    newCancelSpotFleetRequestsError,

    -- ** CancelSpotFleetRequestsErrorItem
    CancelSpotFleetRequestsErrorItem (CancelSpotFleetRequestsErrorItem'),
    newCancelSpotFleetRequestsErrorItem,

    -- ** CancelSpotFleetRequestsSuccessItem
    CancelSpotFleetRequestsSuccessItem (CancelSpotFleetRequestsSuccessItem'),
    newCancelSpotFleetRequestsSuccessItem,

    -- ** CancelledSpotInstanceRequest
    CancelledSpotInstanceRequest (CancelledSpotInstanceRequest'),
    newCancelledSpotInstanceRequest,

    -- ** CapacityAllocation
    CapacityAllocation (CapacityAllocation'),
    newCapacityAllocation,

    -- ** CapacityReservation
    CapacityReservation (CapacityReservation'),
    newCapacityReservation,

    -- ** CapacityReservationFleet
    CapacityReservationFleet (CapacityReservationFleet'),
    newCapacityReservationFleet,

    -- ** CapacityReservationFleetCancellationState
    CapacityReservationFleetCancellationState (CapacityReservationFleetCancellationState'),
    newCapacityReservationFleetCancellationState,

    -- ** CapacityReservationGroup
    CapacityReservationGroup (CapacityReservationGroup'),
    newCapacityReservationGroup,

    -- ** CapacityReservationOptions
    CapacityReservationOptions (CapacityReservationOptions'),
    newCapacityReservationOptions,

    -- ** CapacityReservationOptionsRequest
    CapacityReservationOptionsRequest (CapacityReservationOptionsRequest'),
    newCapacityReservationOptionsRequest,

    -- ** CapacityReservationSpecification
    CapacityReservationSpecification (CapacityReservationSpecification'),
    newCapacityReservationSpecification,

    -- ** CapacityReservationSpecificationResponse
    CapacityReservationSpecificationResponse (CapacityReservationSpecificationResponse'),
    newCapacityReservationSpecificationResponse,

    -- ** CapacityReservationTarget
    CapacityReservationTarget (CapacityReservationTarget'),
    newCapacityReservationTarget,

    -- ** CapacityReservationTargetResponse
    CapacityReservationTargetResponse (CapacityReservationTargetResponse'),
    newCapacityReservationTargetResponse,

    -- ** CarrierGateway
    CarrierGateway (CarrierGateway'),
    newCarrierGateway,

    -- ** CertificateAuthentication
    CertificateAuthentication (CertificateAuthentication'),
    newCertificateAuthentication,

    -- ** CertificateAuthenticationRequest
    CertificateAuthenticationRequest (CertificateAuthenticationRequest'),
    newCertificateAuthenticationRequest,

    -- ** CidrAuthorizationContext
    CidrAuthorizationContext (CidrAuthorizationContext'),
    newCidrAuthorizationContext,

    -- ** CidrBlock
    CidrBlock (CidrBlock'),
    newCidrBlock,

    -- ** ClassicLinkDnsSupport
    ClassicLinkDnsSupport (ClassicLinkDnsSupport'),
    newClassicLinkDnsSupport,

    -- ** ClassicLinkInstance
    ClassicLinkInstance (ClassicLinkInstance'),
    newClassicLinkInstance,

    -- ** ClassicLoadBalancer
    ClassicLoadBalancer (ClassicLoadBalancer'),
    newClassicLoadBalancer,

    -- ** ClassicLoadBalancersConfig
    ClassicLoadBalancersConfig (ClassicLoadBalancersConfig'),
    newClassicLoadBalancersConfig,

    -- ** ClientCertificateRevocationListStatus
    ClientCertificateRevocationListStatus (ClientCertificateRevocationListStatus'),
    newClientCertificateRevocationListStatus,

    -- ** ClientConnectOptions
    ClientConnectOptions (ClientConnectOptions'),
    newClientConnectOptions,

    -- ** ClientConnectResponseOptions
    ClientConnectResponseOptions (ClientConnectResponseOptions'),
    newClientConnectResponseOptions,

    -- ** ClientData
    ClientData (ClientData'),
    newClientData,

    -- ** ClientLoginBannerOptions
    ClientLoginBannerOptions (ClientLoginBannerOptions'),
    newClientLoginBannerOptions,

    -- ** ClientLoginBannerResponseOptions
    ClientLoginBannerResponseOptions (ClientLoginBannerResponseOptions'),
    newClientLoginBannerResponseOptions,

    -- ** ClientVpnAuthentication
    ClientVpnAuthentication (ClientVpnAuthentication'),
    newClientVpnAuthentication,

    -- ** ClientVpnAuthenticationRequest
    ClientVpnAuthenticationRequest (ClientVpnAuthenticationRequest'),
    newClientVpnAuthenticationRequest,

    -- ** ClientVpnAuthorizationRuleStatus
    ClientVpnAuthorizationRuleStatus (ClientVpnAuthorizationRuleStatus'),
    newClientVpnAuthorizationRuleStatus,

    -- ** ClientVpnConnection
    ClientVpnConnection (ClientVpnConnection'),
    newClientVpnConnection,

    -- ** ClientVpnConnectionStatus
    ClientVpnConnectionStatus (ClientVpnConnectionStatus'),
    newClientVpnConnectionStatus,

    -- ** ClientVpnEndpoint
    ClientVpnEndpoint (ClientVpnEndpoint'),
    newClientVpnEndpoint,

    -- ** ClientVpnEndpointAttributeStatus
    ClientVpnEndpointAttributeStatus (ClientVpnEndpointAttributeStatus'),
    newClientVpnEndpointAttributeStatus,

    -- ** ClientVpnEndpointStatus
    ClientVpnEndpointStatus (ClientVpnEndpointStatus'),
    newClientVpnEndpointStatus,

    -- ** ClientVpnRoute
    ClientVpnRoute (ClientVpnRoute'),
    newClientVpnRoute,

    -- ** ClientVpnRouteStatus
    ClientVpnRouteStatus (ClientVpnRouteStatus'),
    newClientVpnRouteStatus,

    -- ** CloudWatchLogOptions
    CloudWatchLogOptions (CloudWatchLogOptions'),
    newCloudWatchLogOptions,

    -- ** CloudWatchLogOptionsSpecification
    CloudWatchLogOptionsSpecification (CloudWatchLogOptionsSpecification'),
    newCloudWatchLogOptionsSpecification,

    -- ** CoipAddressUsage
    CoipAddressUsage (CoipAddressUsage'),
    newCoipAddressUsage,

    -- ** CoipCidr
    CoipCidr (CoipCidr'),
    newCoipCidr,

    -- ** CoipPool
    CoipPool (CoipPool'),
    newCoipPool,

    -- ** ConnectionLogOptions
    ConnectionLogOptions (ConnectionLogOptions'),
    newConnectionLogOptions,

    -- ** ConnectionLogResponseOptions
    ConnectionLogResponseOptions (ConnectionLogResponseOptions'),
    newConnectionLogResponseOptions,

    -- ** ConnectionNotification
    ConnectionNotification (ConnectionNotification'),
    newConnectionNotification,

    -- ** ConversionTask
    ConversionTask (ConversionTask'),
    newConversionTask,

    -- ** CpuOptions
    CpuOptions (CpuOptions'),
    newCpuOptions,

    -- ** CpuOptionsRequest
    CpuOptionsRequest (CpuOptionsRequest'),
    newCpuOptionsRequest,

    -- ** CreateFleetError
    CreateFleetError (CreateFleetError'),
    newCreateFleetError,

    -- ** CreateFleetInstance
    CreateFleetInstance (CreateFleetInstance'),
    newCreateFleetInstance,

    -- ** CreateTransitGatewayConnectRequestOptions
    CreateTransitGatewayConnectRequestOptions (CreateTransitGatewayConnectRequestOptions'),
    newCreateTransitGatewayConnectRequestOptions,

    -- ** CreateTransitGatewayMulticastDomainRequestOptions
    CreateTransitGatewayMulticastDomainRequestOptions (CreateTransitGatewayMulticastDomainRequestOptions'),
    newCreateTransitGatewayMulticastDomainRequestOptions,

    -- ** CreateTransitGatewayPeeringAttachmentRequestOptions
    CreateTransitGatewayPeeringAttachmentRequestOptions (CreateTransitGatewayPeeringAttachmentRequestOptions'),
    newCreateTransitGatewayPeeringAttachmentRequestOptions,

    -- ** CreateTransitGatewayVpcAttachmentRequestOptions
    CreateTransitGatewayVpcAttachmentRequestOptions (CreateTransitGatewayVpcAttachmentRequestOptions'),
    newCreateTransitGatewayVpcAttachmentRequestOptions,

    -- ** CreateVerifiedAccessEndpointEniOptions
    CreateVerifiedAccessEndpointEniOptions (CreateVerifiedAccessEndpointEniOptions'),
    newCreateVerifiedAccessEndpointEniOptions,

    -- ** CreateVerifiedAccessEndpointLoadBalancerOptions
    CreateVerifiedAccessEndpointLoadBalancerOptions (CreateVerifiedAccessEndpointLoadBalancerOptions'),
    newCreateVerifiedAccessEndpointLoadBalancerOptions,

    -- ** CreateVerifiedAccessTrustProviderDeviceOptions
    CreateVerifiedAccessTrustProviderDeviceOptions (CreateVerifiedAccessTrustProviderDeviceOptions'),
    newCreateVerifiedAccessTrustProviderDeviceOptions,

    -- ** CreateVerifiedAccessTrustProviderOidcOptions
    CreateVerifiedAccessTrustProviderOidcOptions (CreateVerifiedAccessTrustProviderOidcOptions'),
    newCreateVerifiedAccessTrustProviderOidcOptions,

    -- ** CreateVolumePermission
    CreateVolumePermission (CreateVolumePermission'),
    newCreateVolumePermission,

    -- ** CreateVolumePermissionModifications
    CreateVolumePermissionModifications (CreateVolumePermissionModifications'),
    newCreateVolumePermissionModifications,

    -- ** CreditSpecification
    CreditSpecification (CreditSpecification'),
    newCreditSpecification,

    -- ** CreditSpecificationRequest
    CreditSpecificationRequest (CreditSpecificationRequest'),
    newCreditSpecificationRequest,

    -- ** CustomerGateway
    CustomerGateway (CustomerGateway'),
    newCustomerGateway,

    -- ** DataQuery
    DataQuery (DataQuery'),
    newDataQuery,

    -- ** DataResponse
    DataResponse (DataResponse'),
    newDataResponse,

    -- ** DeleteFleetError
    DeleteFleetError (DeleteFleetError'),
    newDeleteFleetError,

    -- ** DeleteFleetErrorItem
    DeleteFleetErrorItem (DeleteFleetErrorItem'),
    newDeleteFleetErrorItem,

    -- ** DeleteFleetSuccessItem
    DeleteFleetSuccessItem (DeleteFleetSuccessItem'),
    newDeleteFleetSuccessItem,

    -- ** DeleteLaunchTemplateVersionsResponseErrorItem
    DeleteLaunchTemplateVersionsResponseErrorItem (DeleteLaunchTemplateVersionsResponseErrorItem'),
    newDeleteLaunchTemplateVersionsResponseErrorItem,

    -- ** DeleteLaunchTemplateVersionsResponseSuccessItem
    DeleteLaunchTemplateVersionsResponseSuccessItem (DeleteLaunchTemplateVersionsResponseSuccessItem'),
    newDeleteLaunchTemplateVersionsResponseSuccessItem,

    -- ** DeleteQueuedReservedInstancesError
    DeleteQueuedReservedInstancesError (DeleteQueuedReservedInstancesError'),
    newDeleteQueuedReservedInstancesError,

    -- ** DeregisterInstanceTagAttributeRequest
    DeregisterInstanceTagAttributeRequest (DeregisterInstanceTagAttributeRequest'),
    newDeregisterInstanceTagAttributeRequest,

    -- ** DescribeFastLaunchImagesSuccessItem
    DescribeFastLaunchImagesSuccessItem (DescribeFastLaunchImagesSuccessItem'),
    newDescribeFastLaunchImagesSuccessItem,

    -- ** DescribeFastSnapshotRestoreSuccessItem
    DescribeFastSnapshotRestoreSuccessItem (DescribeFastSnapshotRestoreSuccessItem'),
    newDescribeFastSnapshotRestoreSuccessItem,

    -- ** DescribeFleetError
    DescribeFleetError (DescribeFleetError'),
    newDescribeFleetError,

    -- ** DescribeFleetsInstances
    DescribeFleetsInstances (DescribeFleetsInstances'),
    newDescribeFleetsInstances,

    -- ** DestinationOptionsRequest
    DestinationOptionsRequest (DestinationOptionsRequest'),
    newDestinationOptionsRequest,

    -- ** DestinationOptionsResponse
    DestinationOptionsResponse (DestinationOptionsResponse'),
    newDestinationOptionsResponse,

    -- ** DeviceOptions
    DeviceOptions (DeviceOptions'),
    newDeviceOptions,

    -- ** DhcpConfiguration
    DhcpConfiguration (DhcpConfiguration'),
    newDhcpConfiguration,

    -- ** DhcpOptions
    DhcpOptions (DhcpOptions'),
    newDhcpOptions,

    -- ** DirectoryServiceAuthentication
    DirectoryServiceAuthentication (DirectoryServiceAuthentication'),
    newDirectoryServiceAuthentication,

    -- ** DirectoryServiceAuthenticationRequest
    DirectoryServiceAuthenticationRequest (DirectoryServiceAuthenticationRequest'),
    newDirectoryServiceAuthenticationRequest,

    -- ** DisableFastSnapshotRestoreErrorItem
    DisableFastSnapshotRestoreErrorItem (DisableFastSnapshotRestoreErrorItem'),
    newDisableFastSnapshotRestoreErrorItem,

    -- ** DisableFastSnapshotRestoreStateError
    DisableFastSnapshotRestoreStateError (DisableFastSnapshotRestoreStateError'),
    newDisableFastSnapshotRestoreStateError,

    -- ** DisableFastSnapshotRestoreStateErrorItem
    DisableFastSnapshotRestoreStateErrorItem (DisableFastSnapshotRestoreStateErrorItem'),
    newDisableFastSnapshotRestoreStateErrorItem,

    -- ** DisableFastSnapshotRestoreSuccessItem
    DisableFastSnapshotRestoreSuccessItem (DisableFastSnapshotRestoreSuccessItem'),
    newDisableFastSnapshotRestoreSuccessItem,

    -- ** DiskImage
    DiskImage (DiskImage'),
    newDiskImage,

    -- ** DiskImageDescription
    DiskImageDescription (DiskImageDescription'),
    newDiskImageDescription,

    -- ** DiskImageDetail
    DiskImageDetail (DiskImageDetail'),
    newDiskImageDetail,

    -- ** DiskImageVolumeDescription
    DiskImageVolumeDescription (DiskImageVolumeDescription'),
    newDiskImageVolumeDescription,

    -- ** DiskInfo
    DiskInfo (DiskInfo'),
    newDiskInfo,

    -- ** DnsEntry
    DnsEntry (DnsEntry'),
    newDnsEntry,

    -- ** DnsOptions
    DnsOptions (DnsOptions'),
    newDnsOptions,

    -- ** DnsOptionsSpecification
    DnsOptionsSpecification (DnsOptionsSpecification'),
    newDnsOptionsSpecification,

    -- ** DnsServersOptionsModifyStructure
    DnsServersOptionsModifyStructure (DnsServersOptionsModifyStructure'),
    newDnsServersOptionsModifyStructure,

    -- ** EbsBlockDevice
    EbsBlockDevice (EbsBlockDevice'),
    newEbsBlockDevice,

    -- ** EbsInfo
    EbsInfo (EbsInfo'),
    newEbsInfo,

    -- ** EbsInstanceBlockDevice
    EbsInstanceBlockDevice (EbsInstanceBlockDevice'),
    newEbsInstanceBlockDevice,

    -- ** EbsInstanceBlockDeviceSpecification
    EbsInstanceBlockDeviceSpecification (EbsInstanceBlockDeviceSpecification'),
    newEbsInstanceBlockDeviceSpecification,

    -- ** EbsOptimizedInfo
    EbsOptimizedInfo (EbsOptimizedInfo'),
    newEbsOptimizedInfo,

    -- ** Ec2InstanceConnectEndpoint
    Ec2InstanceConnectEndpoint (Ec2InstanceConnectEndpoint'),
    newEc2InstanceConnectEndpoint,

    -- ** EfaInfo
    EfaInfo (EfaInfo'),
    newEfaInfo,

    -- ** EgressOnlyInternetGateway
    EgressOnlyInternetGateway (EgressOnlyInternetGateway'),
    newEgressOnlyInternetGateway,

    -- ** ElasticGpuAssociation
    ElasticGpuAssociation (ElasticGpuAssociation'),
    newElasticGpuAssociation,

    -- ** ElasticGpuHealth
    ElasticGpuHealth (ElasticGpuHealth'),
    newElasticGpuHealth,

    -- ** ElasticGpuSpecification
    ElasticGpuSpecification (ElasticGpuSpecification'),
    newElasticGpuSpecification,

    -- ** ElasticGpuSpecificationResponse
    ElasticGpuSpecificationResponse (ElasticGpuSpecificationResponse'),
    newElasticGpuSpecificationResponse,

    -- ** ElasticGpus
    ElasticGpus (ElasticGpus'),
    newElasticGpus,

    -- ** ElasticInferenceAccelerator
    ElasticInferenceAccelerator (ElasticInferenceAccelerator'),
    newElasticInferenceAccelerator,

    -- ** ElasticInferenceAcceleratorAssociation
    ElasticInferenceAcceleratorAssociation (ElasticInferenceAcceleratorAssociation'),
    newElasticInferenceAcceleratorAssociation,

    -- ** EnaSrdSpecification
    EnaSrdSpecification (EnaSrdSpecification'),
    newEnaSrdSpecification,

    -- ** EnaSrdUdpSpecification
    EnaSrdUdpSpecification (EnaSrdUdpSpecification'),
    newEnaSrdUdpSpecification,

    -- ** EnableFastSnapshotRestoreErrorItem
    EnableFastSnapshotRestoreErrorItem (EnableFastSnapshotRestoreErrorItem'),
    newEnableFastSnapshotRestoreErrorItem,

    -- ** EnableFastSnapshotRestoreStateError
    EnableFastSnapshotRestoreStateError (EnableFastSnapshotRestoreStateError'),
    newEnableFastSnapshotRestoreStateError,

    -- ** EnableFastSnapshotRestoreStateErrorItem
    EnableFastSnapshotRestoreStateErrorItem (EnableFastSnapshotRestoreStateErrorItem'),
    newEnableFastSnapshotRestoreStateErrorItem,

    -- ** EnableFastSnapshotRestoreSuccessItem
    EnableFastSnapshotRestoreSuccessItem (EnableFastSnapshotRestoreSuccessItem'),
    newEnableFastSnapshotRestoreSuccessItem,

    -- ** EnclaveOptions
    EnclaveOptions (EnclaveOptions'),
    newEnclaveOptions,

    -- ** EnclaveOptionsRequest
    EnclaveOptionsRequest (EnclaveOptionsRequest'),
    newEnclaveOptionsRequest,

    -- ** EventInformation
    EventInformation (EventInformation'),
    newEventInformation,

    -- ** Explanation
    Explanation (Explanation'),
    newExplanation,

    -- ** ExportImageTask
    ExportImageTask (ExportImageTask'),
    newExportImageTask,

    -- ** ExportTask
    ExportTask (ExportTask'),
    newExportTask,

    -- ** ExportTaskS3Location
    ExportTaskS3Location (ExportTaskS3Location'),
    newExportTaskS3Location,

    -- ** ExportTaskS3LocationRequest
    ExportTaskS3LocationRequest (ExportTaskS3LocationRequest'),
    newExportTaskS3LocationRequest,

    -- ** ExportToS3Task
    ExportToS3Task (ExportToS3Task'),
    newExportToS3Task,

    -- ** ExportToS3TaskSpecification
    ExportToS3TaskSpecification (ExportToS3TaskSpecification'),
    newExportToS3TaskSpecification,

    -- ** FailedCapacityReservationFleetCancellationResult
    FailedCapacityReservationFleetCancellationResult (FailedCapacityReservationFleetCancellationResult'),
    newFailedCapacityReservationFleetCancellationResult,

    -- ** FailedQueuedPurchaseDeletion
    FailedQueuedPurchaseDeletion (FailedQueuedPurchaseDeletion'),
    newFailedQueuedPurchaseDeletion,

    -- ** FastLaunchLaunchTemplateSpecificationRequest
    FastLaunchLaunchTemplateSpecificationRequest (FastLaunchLaunchTemplateSpecificationRequest'),
    newFastLaunchLaunchTemplateSpecificationRequest,

    -- ** FastLaunchLaunchTemplateSpecificationResponse
    FastLaunchLaunchTemplateSpecificationResponse (FastLaunchLaunchTemplateSpecificationResponse'),
    newFastLaunchLaunchTemplateSpecificationResponse,

    -- ** FastLaunchSnapshotConfigurationRequest
    FastLaunchSnapshotConfigurationRequest (FastLaunchSnapshotConfigurationRequest'),
    newFastLaunchSnapshotConfigurationRequest,

    -- ** FastLaunchSnapshotConfigurationResponse
    FastLaunchSnapshotConfigurationResponse (FastLaunchSnapshotConfigurationResponse'),
    newFastLaunchSnapshotConfigurationResponse,

    -- ** FederatedAuthentication
    FederatedAuthentication (FederatedAuthentication'),
    newFederatedAuthentication,

    -- ** FederatedAuthenticationRequest
    FederatedAuthenticationRequest (FederatedAuthenticationRequest'),
    newFederatedAuthenticationRequest,

    -- ** Filter
    Filter (Filter'),
    newFilter,

    -- ** FilterPortRange
    FilterPortRange (FilterPortRange'),
    newFilterPortRange,

    -- ** FirewallStatefulRule
    FirewallStatefulRule (FirewallStatefulRule'),
    newFirewallStatefulRule,

    -- ** FirewallStatelessRule
    FirewallStatelessRule (FirewallStatelessRule'),
    newFirewallStatelessRule,

    -- ** FleetCapacityReservation
    FleetCapacityReservation (FleetCapacityReservation'),
    newFleetCapacityReservation,

    -- ** FleetData
    FleetData (FleetData'),
    newFleetData,

    -- ** FleetLaunchTemplateConfig
    FleetLaunchTemplateConfig (FleetLaunchTemplateConfig'),
    newFleetLaunchTemplateConfig,

    -- ** FleetLaunchTemplateConfigRequest
    FleetLaunchTemplateConfigRequest (FleetLaunchTemplateConfigRequest'),
    newFleetLaunchTemplateConfigRequest,

    -- ** FleetLaunchTemplateOverrides
    FleetLaunchTemplateOverrides (FleetLaunchTemplateOverrides'),
    newFleetLaunchTemplateOverrides,

    -- ** FleetLaunchTemplateOverridesRequest
    FleetLaunchTemplateOverridesRequest (FleetLaunchTemplateOverridesRequest'),
    newFleetLaunchTemplateOverridesRequest,

    -- ** FleetLaunchTemplateSpecification
    FleetLaunchTemplateSpecification (FleetLaunchTemplateSpecification'),
    newFleetLaunchTemplateSpecification,

    -- ** FleetLaunchTemplateSpecificationRequest
    FleetLaunchTemplateSpecificationRequest (FleetLaunchTemplateSpecificationRequest'),
    newFleetLaunchTemplateSpecificationRequest,

    -- ** FleetSpotCapacityRebalance
    FleetSpotCapacityRebalance (FleetSpotCapacityRebalance'),
    newFleetSpotCapacityRebalance,

    -- ** FleetSpotCapacityRebalanceRequest
    FleetSpotCapacityRebalanceRequest (FleetSpotCapacityRebalanceRequest'),
    newFleetSpotCapacityRebalanceRequest,

    -- ** FleetSpotMaintenanceStrategies
    FleetSpotMaintenanceStrategies (FleetSpotMaintenanceStrategies'),
    newFleetSpotMaintenanceStrategies,

    -- ** FleetSpotMaintenanceStrategiesRequest
    FleetSpotMaintenanceStrategiesRequest (FleetSpotMaintenanceStrategiesRequest'),
    newFleetSpotMaintenanceStrategiesRequest,

    -- ** FlowLog
    FlowLog (FlowLog'),
    newFlowLog,

    -- ** FpgaDeviceInfo
    FpgaDeviceInfo (FpgaDeviceInfo'),
    newFpgaDeviceInfo,

    -- ** FpgaDeviceMemoryInfo
    FpgaDeviceMemoryInfo (FpgaDeviceMemoryInfo'),
    newFpgaDeviceMemoryInfo,

    -- ** FpgaImage
    FpgaImage (FpgaImage'),
    newFpgaImage,

    -- ** FpgaImageAttribute
    FpgaImageAttribute (FpgaImageAttribute'),
    newFpgaImageAttribute,

    -- ** FpgaImageState
    FpgaImageState (FpgaImageState'),
    newFpgaImageState,

    -- ** FpgaInfo
    FpgaInfo (FpgaInfo'),
    newFpgaInfo,

    -- ** GpuDeviceInfo
    GpuDeviceInfo (GpuDeviceInfo'),
    newGpuDeviceInfo,

    -- ** GpuDeviceMemoryInfo
    GpuDeviceMemoryInfo (GpuDeviceMemoryInfo'),
    newGpuDeviceMemoryInfo,

    -- ** GpuInfo
    GpuInfo (GpuInfo'),
    newGpuInfo,

    -- ** GroupIdentifier
    GroupIdentifier (GroupIdentifier'),
    newGroupIdentifier,

    -- ** HibernationOptions
    HibernationOptions (HibernationOptions'),
    newHibernationOptions,

    -- ** HibernationOptionsRequest
    HibernationOptionsRequest (HibernationOptionsRequest'),
    newHibernationOptionsRequest,

    -- ** HistoryRecord
    HistoryRecord (HistoryRecord'),
    newHistoryRecord,

    -- ** HistoryRecordEntry
    HistoryRecordEntry (HistoryRecordEntry'),
    newHistoryRecordEntry,

    -- ** Host
    Host (Host'),
    newHost,

    -- ** HostInstance
    HostInstance (HostInstance'),
    newHostInstance,

    -- ** HostOffering
    HostOffering (HostOffering'),
    newHostOffering,

    -- ** HostProperties
    HostProperties (HostProperties'),
    newHostProperties,

    -- ** HostReservation
    HostReservation (HostReservation'),
    newHostReservation,

    -- ** IKEVersionsListValue
    IKEVersionsListValue (IKEVersionsListValue'),
    newIKEVersionsListValue,

    -- ** IKEVersionsRequestListValue
    IKEVersionsRequestListValue (IKEVersionsRequestListValue'),
    newIKEVersionsRequestListValue,

    -- ** IamInstanceProfile
    IamInstanceProfile (IamInstanceProfile'),
    newIamInstanceProfile,

    -- ** IamInstanceProfileAssociation
    IamInstanceProfileAssociation (IamInstanceProfileAssociation'),
    newIamInstanceProfileAssociation,

    -- ** IamInstanceProfileSpecification
    IamInstanceProfileSpecification (IamInstanceProfileSpecification'),
    newIamInstanceProfileSpecification,

    -- ** IcmpTypeCode
    IcmpTypeCode (IcmpTypeCode'),
    newIcmpTypeCode,

    -- ** IdFormat
    IdFormat (IdFormat'),
    newIdFormat,

    -- ** Image
    Image (Image'),
    newImage,

    -- ** ImageDiskContainer
    ImageDiskContainer (ImageDiskContainer'),
    newImageDiskContainer,

    -- ** ImageRecycleBinInfo
    ImageRecycleBinInfo (ImageRecycleBinInfo'),
    newImageRecycleBinInfo,

    -- ** ImportImageLicenseConfigurationRequest
    ImportImageLicenseConfigurationRequest (ImportImageLicenseConfigurationRequest'),
    newImportImageLicenseConfigurationRequest,

    -- ** ImportImageLicenseConfigurationResponse
    ImportImageLicenseConfigurationResponse (ImportImageLicenseConfigurationResponse'),
    newImportImageLicenseConfigurationResponse,

    -- ** ImportImageTask
    ImportImageTask (ImportImageTask'),
    newImportImageTask,

    -- ** ImportInstanceLaunchSpecification
    ImportInstanceLaunchSpecification (ImportInstanceLaunchSpecification'),
    newImportInstanceLaunchSpecification,

    -- ** ImportInstanceTaskDetails
    ImportInstanceTaskDetails (ImportInstanceTaskDetails'),
    newImportInstanceTaskDetails,

    -- ** ImportInstanceVolumeDetailItem
    ImportInstanceVolumeDetailItem (ImportInstanceVolumeDetailItem'),
    newImportInstanceVolumeDetailItem,

    -- ** ImportSnapshotTask
    ImportSnapshotTask (ImportSnapshotTask'),
    newImportSnapshotTask,

    -- ** ImportVolumeTaskDetails
    ImportVolumeTaskDetails (ImportVolumeTaskDetails'),
    newImportVolumeTaskDetails,

    -- ** InferenceAcceleratorInfo
    InferenceAcceleratorInfo (InferenceAcceleratorInfo'),
    newInferenceAcceleratorInfo,

    -- ** InferenceDeviceInfo
    InferenceDeviceInfo (InferenceDeviceInfo'),
    newInferenceDeviceInfo,

    -- ** Instance
    Instance (Instance'),
    newInstance,

    -- ** InstanceBlockDeviceMapping
    InstanceBlockDeviceMapping (InstanceBlockDeviceMapping'),
    newInstanceBlockDeviceMapping,

    -- ** InstanceBlockDeviceMappingSpecification
    InstanceBlockDeviceMappingSpecification (InstanceBlockDeviceMappingSpecification'),
    newInstanceBlockDeviceMappingSpecification,

    -- ** InstanceCapacity
    InstanceCapacity (InstanceCapacity'),
    newInstanceCapacity,

    -- ** InstanceCount
    InstanceCount (InstanceCount'),
    newInstanceCount,

    -- ** InstanceCreditSpecification
    InstanceCreditSpecification (InstanceCreditSpecification'),
    newInstanceCreditSpecification,

    -- ** InstanceCreditSpecificationRequest
    InstanceCreditSpecificationRequest (InstanceCreditSpecificationRequest'),
    newInstanceCreditSpecificationRequest,

    -- ** InstanceEventWindow
    InstanceEventWindow (InstanceEventWindow'),
    newInstanceEventWindow,

    -- ** InstanceEventWindowAssociationRequest
    InstanceEventWindowAssociationRequest (InstanceEventWindowAssociationRequest'),
    newInstanceEventWindowAssociationRequest,

    -- ** InstanceEventWindowAssociationTarget
    InstanceEventWindowAssociationTarget (InstanceEventWindowAssociationTarget'),
    newInstanceEventWindowAssociationTarget,

    -- ** InstanceEventWindowDisassociationRequest
    InstanceEventWindowDisassociationRequest (InstanceEventWindowDisassociationRequest'),
    newInstanceEventWindowDisassociationRequest,

    -- ** InstanceEventWindowStateChange
    InstanceEventWindowStateChange (InstanceEventWindowStateChange'),
    newInstanceEventWindowStateChange,

    -- ** InstanceEventWindowTimeRange
    InstanceEventWindowTimeRange (InstanceEventWindowTimeRange'),
    newInstanceEventWindowTimeRange,

    -- ** InstanceEventWindowTimeRangeRequest
    InstanceEventWindowTimeRangeRequest (InstanceEventWindowTimeRangeRequest'),
    newInstanceEventWindowTimeRangeRequest,

    -- ** InstanceExportDetails
    InstanceExportDetails (InstanceExportDetails'),
    newInstanceExportDetails,

    -- ** InstanceFamilyCreditSpecification
    InstanceFamilyCreditSpecification (InstanceFamilyCreditSpecification'),
    newInstanceFamilyCreditSpecification,

    -- ** InstanceIpv4Prefix
    InstanceIpv4Prefix (InstanceIpv4Prefix'),
    newInstanceIpv4Prefix,

    -- ** InstanceIpv6Address
    InstanceIpv6Address (InstanceIpv6Address'),
    newInstanceIpv6Address,

    -- ** InstanceIpv6AddressRequest
    InstanceIpv6AddressRequest (InstanceIpv6AddressRequest'),
    newInstanceIpv6AddressRequest,

    -- ** InstanceIpv6Prefix
    InstanceIpv6Prefix (InstanceIpv6Prefix'),
    newInstanceIpv6Prefix,

    -- ** InstanceMaintenanceOptions
    InstanceMaintenanceOptions (InstanceMaintenanceOptions'),
    newInstanceMaintenanceOptions,

    -- ** InstanceMaintenanceOptionsRequest
    InstanceMaintenanceOptionsRequest (InstanceMaintenanceOptionsRequest'),
    newInstanceMaintenanceOptionsRequest,

    -- ** InstanceMarketOptionsRequest
    InstanceMarketOptionsRequest (InstanceMarketOptionsRequest'),
    newInstanceMarketOptionsRequest,

    -- ** InstanceMetadataOptionsRequest
    InstanceMetadataOptionsRequest (InstanceMetadataOptionsRequest'),
    newInstanceMetadataOptionsRequest,

    -- ** InstanceMetadataOptionsResponse
    InstanceMetadataOptionsResponse (InstanceMetadataOptionsResponse'),
    newInstanceMetadataOptionsResponse,

    -- ** InstanceMonitoring
    InstanceMonitoring (InstanceMonitoring'),
    newInstanceMonitoring,

    -- ** InstanceNetworkInterface
    InstanceNetworkInterface (InstanceNetworkInterface'),
    newInstanceNetworkInterface,

    -- ** InstanceNetworkInterfaceAssociation
    InstanceNetworkInterfaceAssociation (InstanceNetworkInterfaceAssociation'),
    newInstanceNetworkInterfaceAssociation,

    -- ** InstanceNetworkInterfaceAttachment
    InstanceNetworkInterfaceAttachment (InstanceNetworkInterfaceAttachment'),
    newInstanceNetworkInterfaceAttachment,

    -- ** InstanceNetworkInterfaceSpecification
    InstanceNetworkInterfaceSpecification (InstanceNetworkInterfaceSpecification'),
    newInstanceNetworkInterfaceSpecification,

    -- ** InstancePrivateIpAddress
    InstancePrivateIpAddress (InstancePrivateIpAddress'),
    newInstancePrivateIpAddress,

    -- ** InstanceRequirements
    InstanceRequirements (InstanceRequirements'),
    newInstanceRequirements,

    -- ** InstanceRequirementsRequest
    InstanceRequirementsRequest (InstanceRequirementsRequest'),
    newInstanceRequirementsRequest,

    -- ** InstanceRequirementsWithMetadataRequest
    InstanceRequirementsWithMetadataRequest (InstanceRequirementsWithMetadataRequest'),
    newInstanceRequirementsWithMetadataRequest,

    -- ** InstanceSpecification
    InstanceSpecification (InstanceSpecification'),
    newInstanceSpecification,

    -- ** InstanceState
    InstanceState (InstanceState'),
    newInstanceState,

    -- ** InstanceStateChange
    InstanceStateChange (InstanceStateChange'),
    newInstanceStateChange,

    -- ** InstanceStatus
    InstanceStatus (InstanceStatus'),
    newInstanceStatus,

    -- ** InstanceStatusDetails
    InstanceStatusDetails (InstanceStatusDetails'),
    newInstanceStatusDetails,

    -- ** InstanceStatusEvent
    InstanceStatusEvent (InstanceStatusEvent'),
    newInstanceStatusEvent,

    -- ** InstanceStatusSummary
    InstanceStatusSummary (InstanceStatusSummary'),
    newInstanceStatusSummary,

    -- ** InstanceStorageInfo
    InstanceStorageInfo (InstanceStorageInfo'),
    newInstanceStorageInfo,

    -- ** InstanceTagNotificationAttribute
    InstanceTagNotificationAttribute (InstanceTagNotificationAttribute'),
    newInstanceTagNotificationAttribute,

    -- ** InstanceTypeInfo
    InstanceTypeInfo (InstanceTypeInfo'),
    newInstanceTypeInfo,

    -- ** InstanceTypeInfoFromInstanceRequirements
    InstanceTypeInfoFromInstanceRequirements (InstanceTypeInfoFromInstanceRequirements'),
    newInstanceTypeInfoFromInstanceRequirements,

    -- ** InstanceTypeOffering
    InstanceTypeOffering (InstanceTypeOffering'),
    newInstanceTypeOffering,

    -- ** InstanceUsage
    InstanceUsage (InstanceUsage'),
    newInstanceUsage,

    -- ** IntegrateServices
    IntegrateServices (IntegrateServices'),
    newIntegrateServices,

    -- ** InternetGateway
    InternetGateway (InternetGateway'),
    newInternetGateway,

    -- ** InternetGatewayAttachment
    InternetGatewayAttachment (InternetGatewayAttachment'),
    newInternetGatewayAttachment,

    -- ** IpPermission
    IpPermission (IpPermission'),
    newIpPermission,

    -- ** IpRange
    IpRange (IpRange'),
    newIpRange,

    -- ** Ipam
    Ipam (Ipam'),
    newIpam,

    -- ** IpamAddressHistoryRecord
    IpamAddressHistoryRecord (IpamAddressHistoryRecord'),
    newIpamAddressHistoryRecord,

    -- ** IpamCidrAuthorizationContext
    IpamCidrAuthorizationContext (IpamCidrAuthorizationContext'),
    newIpamCidrAuthorizationContext,

    -- ** IpamDiscoveredAccount
    IpamDiscoveredAccount (IpamDiscoveredAccount'),
    newIpamDiscoveredAccount,

    -- ** IpamDiscoveredResourceCidr
    IpamDiscoveredResourceCidr (IpamDiscoveredResourceCidr'),
    newIpamDiscoveredResourceCidr,

    -- ** IpamDiscoveryFailureReason
    IpamDiscoveryFailureReason (IpamDiscoveryFailureReason'),
    newIpamDiscoveryFailureReason,

    -- ** IpamOperatingRegion
    IpamOperatingRegion (IpamOperatingRegion'),
    newIpamOperatingRegion,

    -- ** IpamPool
    IpamPool (IpamPool'),
    newIpamPool,

    -- ** IpamPoolAllocation
    IpamPoolAllocation (IpamPoolAllocation'),
    newIpamPoolAllocation,

    -- ** IpamPoolCidr
    IpamPoolCidr (IpamPoolCidr'),
    newIpamPoolCidr,

    -- ** IpamPoolCidrFailureReason
    IpamPoolCidrFailureReason (IpamPoolCidrFailureReason'),
    newIpamPoolCidrFailureReason,

    -- ** IpamResourceCidr
    IpamResourceCidr (IpamResourceCidr'),
    newIpamResourceCidr,

    -- ** IpamResourceDiscovery
    IpamResourceDiscovery (IpamResourceDiscovery'),
    newIpamResourceDiscovery,

    -- ** IpamResourceDiscoveryAssociation
    IpamResourceDiscoveryAssociation (IpamResourceDiscoveryAssociation'),
    newIpamResourceDiscoveryAssociation,

    -- ** IpamResourceTag
    IpamResourceTag (IpamResourceTag'),
    newIpamResourceTag,

    -- ** IpamScope
    IpamScope (IpamScope'),
    newIpamScope,

    -- ** Ipv4PrefixSpecification
    Ipv4PrefixSpecification (Ipv4PrefixSpecification'),
    newIpv4PrefixSpecification,

    -- ** Ipv4PrefixSpecificationRequest
    Ipv4PrefixSpecificationRequest (Ipv4PrefixSpecificationRequest'),
    newIpv4PrefixSpecificationRequest,

    -- ** Ipv4PrefixSpecificationResponse
    Ipv4PrefixSpecificationResponse (Ipv4PrefixSpecificationResponse'),
    newIpv4PrefixSpecificationResponse,

    -- ** Ipv6CidrAssociation
    Ipv6CidrAssociation (Ipv6CidrAssociation'),
    newIpv6CidrAssociation,

    -- ** Ipv6CidrBlock
    Ipv6CidrBlock (Ipv6CidrBlock'),
    newIpv6CidrBlock,

    -- ** Ipv6Pool
    Ipv6Pool (Ipv6Pool'),
    newIpv6Pool,

    -- ** Ipv6PrefixSpecification
    Ipv6PrefixSpecification (Ipv6PrefixSpecification'),
    newIpv6PrefixSpecification,

    -- ** Ipv6PrefixSpecificationRequest
    Ipv6PrefixSpecificationRequest (Ipv6PrefixSpecificationRequest'),
    newIpv6PrefixSpecificationRequest,

    -- ** Ipv6PrefixSpecificationResponse
    Ipv6PrefixSpecificationResponse (Ipv6PrefixSpecificationResponse'),
    newIpv6PrefixSpecificationResponse,

    -- ** Ipv6Range
    Ipv6Range (Ipv6Range'),
    newIpv6Range,

    -- ** KeyPairInfo
    KeyPairInfo (KeyPairInfo'),
    newKeyPairInfo,

    -- ** LastError
    LastError (LastError'),
    newLastError,

    -- ** LaunchPermission
    LaunchPermission (LaunchPermission'),
    newLaunchPermission,

    -- ** LaunchPermissionModifications
    LaunchPermissionModifications (LaunchPermissionModifications'),
    newLaunchPermissionModifications,

    -- ** LaunchSpecification
    LaunchSpecification (LaunchSpecification'),
    newLaunchSpecification,

    -- ** LaunchTemplate
    LaunchTemplate (LaunchTemplate'),
    newLaunchTemplate,

    -- ** LaunchTemplateAndOverridesResponse
    LaunchTemplateAndOverridesResponse (LaunchTemplateAndOverridesResponse'),
    newLaunchTemplateAndOverridesResponse,

    -- ** LaunchTemplateBlockDeviceMapping
    LaunchTemplateBlockDeviceMapping (LaunchTemplateBlockDeviceMapping'),
    newLaunchTemplateBlockDeviceMapping,

    -- ** LaunchTemplateBlockDeviceMappingRequest
    LaunchTemplateBlockDeviceMappingRequest (LaunchTemplateBlockDeviceMappingRequest'),
    newLaunchTemplateBlockDeviceMappingRequest,

    -- ** LaunchTemplateCapacityReservationSpecificationRequest
    LaunchTemplateCapacityReservationSpecificationRequest (LaunchTemplateCapacityReservationSpecificationRequest'),
    newLaunchTemplateCapacityReservationSpecificationRequest,

    -- ** LaunchTemplateCapacityReservationSpecificationResponse
    LaunchTemplateCapacityReservationSpecificationResponse (LaunchTemplateCapacityReservationSpecificationResponse'),
    newLaunchTemplateCapacityReservationSpecificationResponse,

    -- ** LaunchTemplateConfig
    LaunchTemplateConfig (LaunchTemplateConfig'),
    newLaunchTemplateConfig,

    -- ** LaunchTemplateCpuOptions
    LaunchTemplateCpuOptions (LaunchTemplateCpuOptions'),
    newLaunchTemplateCpuOptions,

    -- ** LaunchTemplateCpuOptionsRequest
    LaunchTemplateCpuOptionsRequest (LaunchTemplateCpuOptionsRequest'),
    newLaunchTemplateCpuOptionsRequest,

    -- ** LaunchTemplateEbsBlockDevice
    LaunchTemplateEbsBlockDevice (LaunchTemplateEbsBlockDevice'),
    newLaunchTemplateEbsBlockDevice,

    -- ** LaunchTemplateEbsBlockDeviceRequest
    LaunchTemplateEbsBlockDeviceRequest (LaunchTemplateEbsBlockDeviceRequest'),
    newLaunchTemplateEbsBlockDeviceRequest,

    -- ** LaunchTemplateElasticInferenceAccelerator
    LaunchTemplateElasticInferenceAccelerator (LaunchTemplateElasticInferenceAccelerator'),
    newLaunchTemplateElasticInferenceAccelerator,

    -- ** LaunchTemplateElasticInferenceAcceleratorResponse
    LaunchTemplateElasticInferenceAcceleratorResponse (LaunchTemplateElasticInferenceAcceleratorResponse'),
    newLaunchTemplateElasticInferenceAcceleratorResponse,

    -- ** LaunchTemplateEnclaveOptions
    LaunchTemplateEnclaveOptions (LaunchTemplateEnclaveOptions'),
    newLaunchTemplateEnclaveOptions,

    -- ** LaunchTemplateEnclaveOptionsRequest
    LaunchTemplateEnclaveOptionsRequest (LaunchTemplateEnclaveOptionsRequest'),
    newLaunchTemplateEnclaveOptionsRequest,

    -- ** LaunchTemplateHibernationOptions
    LaunchTemplateHibernationOptions (LaunchTemplateHibernationOptions'),
    newLaunchTemplateHibernationOptions,

    -- ** LaunchTemplateHibernationOptionsRequest
    LaunchTemplateHibernationOptionsRequest (LaunchTemplateHibernationOptionsRequest'),
    newLaunchTemplateHibernationOptionsRequest,

    -- ** LaunchTemplateIamInstanceProfileSpecification
    LaunchTemplateIamInstanceProfileSpecification (LaunchTemplateIamInstanceProfileSpecification'),
    newLaunchTemplateIamInstanceProfileSpecification,

    -- ** LaunchTemplateIamInstanceProfileSpecificationRequest
    LaunchTemplateIamInstanceProfileSpecificationRequest (LaunchTemplateIamInstanceProfileSpecificationRequest'),
    newLaunchTemplateIamInstanceProfileSpecificationRequest,

    -- ** LaunchTemplateInstanceMaintenanceOptions
    LaunchTemplateInstanceMaintenanceOptions (LaunchTemplateInstanceMaintenanceOptions'),
    newLaunchTemplateInstanceMaintenanceOptions,

    -- ** LaunchTemplateInstanceMaintenanceOptionsRequest
    LaunchTemplateInstanceMaintenanceOptionsRequest (LaunchTemplateInstanceMaintenanceOptionsRequest'),
    newLaunchTemplateInstanceMaintenanceOptionsRequest,

    -- ** LaunchTemplateInstanceMarketOptions
    LaunchTemplateInstanceMarketOptions (LaunchTemplateInstanceMarketOptions'),
    newLaunchTemplateInstanceMarketOptions,

    -- ** LaunchTemplateInstanceMarketOptionsRequest
    LaunchTemplateInstanceMarketOptionsRequest (LaunchTemplateInstanceMarketOptionsRequest'),
    newLaunchTemplateInstanceMarketOptionsRequest,

    -- ** LaunchTemplateInstanceMetadataOptions
    LaunchTemplateInstanceMetadataOptions (LaunchTemplateInstanceMetadataOptions'),
    newLaunchTemplateInstanceMetadataOptions,

    -- ** LaunchTemplateInstanceMetadataOptionsRequest
    LaunchTemplateInstanceMetadataOptionsRequest (LaunchTemplateInstanceMetadataOptionsRequest'),
    newLaunchTemplateInstanceMetadataOptionsRequest,

    -- ** LaunchTemplateInstanceNetworkInterfaceSpecification
    LaunchTemplateInstanceNetworkInterfaceSpecification (LaunchTemplateInstanceNetworkInterfaceSpecification'),
    newLaunchTemplateInstanceNetworkInterfaceSpecification,

    -- ** LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
    LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (LaunchTemplateInstanceNetworkInterfaceSpecificationRequest'),
    newLaunchTemplateInstanceNetworkInterfaceSpecificationRequest,

    -- ** LaunchTemplateLicenseConfiguration
    LaunchTemplateLicenseConfiguration (LaunchTemplateLicenseConfiguration'),
    newLaunchTemplateLicenseConfiguration,

    -- ** LaunchTemplateLicenseConfigurationRequest
    LaunchTemplateLicenseConfigurationRequest (LaunchTemplateLicenseConfigurationRequest'),
    newLaunchTemplateLicenseConfigurationRequest,

    -- ** LaunchTemplateOverrides
    LaunchTemplateOverrides (LaunchTemplateOverrides'),
    newLaunchTemplateOverrides,

    -- ** LaunchTemplatePlacement
    LaunchTemplatePlacement (LaunchTemplatePlacement'),
    newLaunchTemplatePlacement,

    -- ** LaunchTemplatePlacementRequest
    LaunchTemplatePlacementRequest (LaunchTemplatePlacementRequest'),
    newLaunchTemplatePlacementRequest,

    -- ** LaunchTemplatePrivateDnsNameOptions
    LaunchTemplatePrivateDnsNameOptions (LaunchTemplatePrivateDnsNameOptions'),
    newLaunchTemplatePrivateDnsNameOptions,

    -- ** LaunchTemplatePrivateDnsNameOptionsRequest
    LaunchTemplatePrivateDnsNameOptionsRequest (LaunchTemplatePrivateDnsNameOptionsRequest'),
    newLaunchTemplatePrivateDnsNameOptionsRequest,

    -- ** LaunchTemplateSpecification
    LaunchTemplateSpecification (LaunchTemplateSpecification'),
    newLaunchTemplateSpecification,

    -- ** LaunchTemplateSpotMarketOptions
    LaunchTemplateSpotMarketOptions (LaunchTemplateSpotMarketOptions'),
    newLaunchTemplateSpotMarketOptions,

    -- ** LaunchTemplateSpotMarketOptionsRequest
    LaunchTemplateSpotMarketOptionsRequest (LaunchTemplateSpotMarketOptionsRequest'),
    newLaunchTemplateSpotMarketOptionsRequest,

    -- ** LaunchTemplateTagSpecification
    LaunchTemplateTagSpecification (LaunchTemplateTagSpecification'),
    newLaunchTemplateTagSpecification,

    -- ** LaunchTemplateTagSpecificationRequest
    LaunchTemplateTagSpecificationRequest (LaunchTemplateTagSpecificationRequest'),
    newLaunchTemplateTagSpecificationRequest,

    -- ** LaunchTemplateVersion
    LaunchTemplateVersion (LaunchTemplateVersion'),
    newLaunchTemplateVersion,

    -- ** LaunchTemplatesMonitoring
    LaunchTemplatesMonitoring (LaunchTemplatesMonitoring'),
    newLaunchTemplatesMonitoring,

    -- ** LaunchTemplatesMonitoringRequest
    LaunchTemplatesMonitoringRequest (LaunchTemplatesMonitoringRequest'),
    newLaunchTemplatesMonitoringRequest,

    -- ** LicenseConfiguration
    LicenseConfiguration (LicenseConfiguration'),
    newLicenseConfiguration,

    -- ** LicenseConfigurationRequest
    LicenseConfigurationRequest (LicenseConfigurationRequest'),
    newLicenseConfigurationRequest,

    -- ** LoadBalancersConfig
    LoadBalancersConfig (LoadBalancersConfig'),
    newLoadBalancersConfig,

    -- ** LoadPermission
    LoadPermission (LoadPermission'),
    newLoadPermission,

    -- ** LoadPermissionModifications
    LoadPermissionModifications (LoadPermissionModifications'),
    newLoadPermissionModifications,

    -- ** LoadPermissionRequest
    LoadPermissionRequest (LoadPermissionRequest'),
    newLoadPermissionRequest,

    -- ** LocalGateway
    LocalGateway (LocalGateway'),
    newLocalGateway,

    -- ** LocalGatewayRoute
    LocalGatewayRoute (LocalGatewayRoute'),
    newLocalGatewayRoute,

    -- ** LocalGatewayRouteTable
    LocalGatewayRouteTable (LocalGatewayRouteTable'),
    newLocalGatewayRouteTable,

    -- ** LocalGatewayRouteTableVirtualInterfaceGroupAssociation
    LocalGatewayRouteTableVirtualInterfaceGroupAssociation (LocalGatewayRouteTableVirtualInterfaceGroupAssociation'),
    newLocalGatewayRouteTableVirtualInterfaceGroupAssociation,

    -- ** LocalGatewayRouteTableVpcAssociation
    LocalGatewayRouteTableVpcAssociation (LocalGatewayRouteTableVpcAssociation'),
    newLocalGatewayRouteTableVpcAssociation,

    -- ** LocalGatewayVirtualInterface
    LocalGatewayVirtualInterface (LocalGatewayVirtualInterface'),
    newLocalGatewayVirtualInterface,

    -- ** LocalGatewayVirtualInterfaceGroup
    LocalGatewayVirtualInterfaceGroup (LocalGatewayVirtualInterfaceGroup'),
    newLocalGatewayVirtualInterfaceGroup,

    -- ** MaintenanceDetails
    MaintenanceDetails (MaintenanceDetails'),
    newMaintenanceDetails,

    -- ** ManagedPrefixList
    ManagedPrefixList (ManagedPrefixList'),
    newManagedPrefixList,

    -- ** MemoryGiBPerVCpu
    MemoryGiBPerVCpu (MemoryGiBPerVCpu'),
    newMemoryGiBPerVCpu,

    -- ** MemoryGiBPerVCpuRequest
    MemoryGiBPerVCpuRequest (MemoryGiBPerVCpuRequest'),
    newMemoryGiBPerVCpuRequest,

    -- ** MemoryInfo
    MemoryInfo (MemoryInfo'),
    newMemoryInfo,

    -- ** MemoryMiB
    MemoryMiB (MemoryMiB'),
    newMemoryMiB,

    -- ** MemoryMiBRequest
    MemoryMiBRequest (MemoryMiBRequest'),
    newMemoryMiBRequest,

    -- ** MetricPoint
    MetricPoint (MetricPoint'),
    newMetricPoint,

    -- ** ModifyTransitGatewayOptions
    ModifyTransitGatewayOptions (ModifyTransitGatewayOptions'),
    newModifyTransitGatewayOptions,

    -- ** ModifyTransitGatewayVpcAttachmentRequestOptions
    ModifyTransitGatewayVpcAttachmentRequestOptions (ModifyTransitGatewayVpcAttachmentRequestOptions'),
    newModifyTransitGatewayVpcAttachmentRequestOptions,

    -- ** ModifyVerifiedAccessEndpointEniOptions
    ModifyVerifiedAccessEndpointEniOptions (ModifyVerifiedAccessEndpointEniOptions'),
    newModifyVerifiedAccessEndpointEniOptions,

    -- ** ModifyVerifiedAccessEndpointLoadBalancerOptions
    ModifyVerifiedAccessEndpointLoadBalancerOptions (ModifyVerifiedAccessEndpointLoadBalancerOptions'),
    newModifyVerifiedAccessEndpointLoadBalancerOptions,

    -- ** ModifyVerifiedAccessTrustProviderOidcOptions
    ModifyVerifiedAccessTrustProviderOidcOptions (ModifyVerifiedAccessTrustProviderOidcOptions'),
    newModifyVerifiedAccessTrustProviderOidcOptions,

    -- ** ModifyVpnTunnelOptionsSpecification
    ModifyVpnTunnelOptionsSpecification (ModifyVpnTunnelOptionsSpecification'),
    newModifyVpnTunnelOptionsSpecification,

    -- ** Monitoring
    Monitoring (Monitoring'),
    newMonitoring,

    -- ** MovingAddressStatus
    MovingAddressStatus (MovingAddressStatus'),
    newMovingAddressStatus,

    -- ** NatGateway
    NatGateway (NatGateway'),
    newNatGateway,

    -- ** NatGatewayAddress
    NatGatewayAddress (NatGatewayAddress'),
    newNatGatewayAddress,

    -- ** NetworkAcl
    NetworkAcl (NetworkAcl'),
    newNetworkAcl,

    -- ** NetworkAclAssociation
    NetworkAclAssociation (NetworkAclAssociation'),
    newNetworkAclAssociation,

    -- ** NetworkAclEntry
    NetworkAclEntry (NetworkAclEntry'),
    newNetworkAclEntry,

    -- ** NetworkBandwidthGbps
    NetworkBandwidthGbps (NetworkBandwidthGbps'),
    newNetworkBandwidthGbps,

    -- ** NetworkBandwidthGbpsRequest
    NetworkBandwidthGbpsRequest (NetworkBandwidthGbpsRequest'),
    newNetworkBandwidthGbpsRequest,

    -- ** NetworkCardInfo
    NetworkCardInfo (NetworkCardInfo'),
    newNetworkCardInfo,

    -- ** NetworkInfo
    NetworkInfo (NetworkInfo'),
    newNetworkInfo,

    -- ** NetworkInsightsAccessScope
    NetworkInsightsAccessScope (NetworkInsightsAccessScope'),
    newNetworkInsightsAccessScope,

    -- ** NetworkInsightsAccessScopeAnalysis
    NetworkInsightsAccessScopeAnalysis (NetworkInsightsAccessScopeAnalysis'),
    newNetworkInsightsAccessScopeAnalysis,

    -- ** NetworkInsightsAccessScopeContent
    NetworkInsightsAccessScopeContent (NetworkInsightsAccessScopeContent'),
    newNetworkInsightsAccessScopeContent,

    -- ** NetworkInsightsAnalysis
    NetworkInsightsAnalysis (NetworkInsightsAnalysis'),
    newNetworkInsightsAnalysis,

    -- ** NetworkInsightsPath
    NetworkInsightsPath (NetworkInsightsPath'),
    newNetworkInsightsPath,

    -- ** NetworkInterface
    NetworkInterface (NetworkInterface'),
    newNetworkInterface,

    -- ** NetworkInterfaceAssociation
    NetworkInterfaceAssociation (NetworkInterfaceAssociation'),
    newNetworkInterfaceAssociation,

    -- ** NetworkInterfaceAttachment
    NetworkInterfaceAttachment (NetworkInterfaceAttachment'),
    newNetworkInterfaceAttachment,

    -- ** NetworkInterfaceAttachmentChanges
    NetworkInterfaceAttachmentChanges (NetworkInterfaceAttachmentChanges'),
    newNetworkInterfaceAttachmentChanges,

    -- ** NetworkInterfaceCount
    NetworkInterfaceCount (NetworkInterfaceCount'),
    newNetworkInterfaceCount,

    -- ** NetworkInterfaceCountRequest
    NetworkInterfaceCountRequest (NetworkInterfaceCountRequest'),
    newNetworkInterfaceCountRequest,

    -- ** NetworkInterfaceIpv6Address
    NetworkInterfaceIpv6Address (NetworkInterfaceIpv6Address'),
    newNetworkInterfaceIpv6Address,

    -- ** NetworkInterfacePermission
    NetworkInterfacePermission (NetworkInterfacePermission'),
    newNetworkInterfacePermission,

    -- ** NetworkInterfacePermissionState
    NetworkInterfacePermissionState (NetworkInterfacePermissionState'),
    newNetworkInterfacePermissionState,

    -- ** NetworkInterfacePrivateIpAddress
    NetworkInterfacePrivateIpAddress (NetworkInterfacePrivateIpAddress'),
    newNetworkInterfacePrivateIpAddress,

    -- ** NewDhcpConfiguration
    NewDhcpConfiguration (NewDhcpConfiguration'),
    newNewDhcpConfiguration,

    -- ** OidcOptions
    OidcOptions (OidcOptions'),
    newOidcOptions,

    -- ** OnDemandOptions
    OnDemandOptions (OnDemandOptions'),
    newOnDemandOptions,

    -- ** OnDemandOptionsRequest
    OnDemandOptionsRequest (OnDemandOptionsRequest'),
    newOnDemandOptionsRequest,

    -- ** PacketHeaderStatement
    PacketHeaderStatement (PacketHeaderStatement'),
    newPacketHeaderStatement,

    -- ** PacketHeaderStatementRequest
    PacketHeaderStatementRequest (PacketHeaderStatementRequest'),
    newPacketHeaderStatementRequest,

    -- ** PathComponent
    PathComponent (PathComponent'),
    newPathComponent,

    -- ** PathFilter
    PathFilter (PathFilter'),
    newPathFilter,

    -- ** PathRequestFilter
    PathRequestFilter (PathRequestFilter'),
    newPathRequestFilter,

    -- ** PathStatement
    PathStatement (PathStatement'),
    newPathStatement,

    -- ** PathStatementRequest
    PathStatementRequest (PathStatementRequest'),
    newPathStatementRequest,

    -- ** PciId
    PciId (PciId'),
    newPciId,

    -- ** PeeringAttachmentStatus
    PeeringAttachmentStatus (PeeringAttachmentStatus'),
    newPeeringAttachmentStatus,

    -- ** PeeringConnectionOptions
    PeeringConnectionOptions (PeeringConnectionOptions'),
    newPeeringConnectionOptions,

    -- ** PeeringConnectionOptionsRequest
    PeeringConnectionOptionsRequest (PeeringConnectionOptionsRequest'),
    newPeeringConnectionOptionsRequest,

    -- ** PeeringTgwInfo
    PeeringTgwInfo (PeeringTgwInfo'),
    newPeeringTgwInfo,

    -- ** Phase1DHGroupNumbersListValue
    Phase1DHGroupNumbersListValue (Phase1DHGroupNumbersListValue'),
    newPhase1DHGroupNumbersListValue,

    -- ** Phase1DHGroupNumbersRequestListValue
    Phase1DHGroupNumbersRequestListValue (Phase1DHGroupNumbersRequestListValue'),
    newPhase1DHGroupNumbersRequestListValue,

    -- ** Phase1EncryptionAlgorithmsListValue
    Phase1EncryptionAlgorithmsListValue (Phase1EncryptionAlgorithmsListValue'),
    newPhase1EncryptionAlgorithmsListValue,

    -- ** Phase1EncryptionAlgorithmsRequestListValue
    Phase1EncryptionAlgorithmsRequestListValue (Phase1EncryptionAlgorithmsRequestListValue'),
    newPhase1EncryptionAlgorithmsRequestListValue,

    -- ** Phase1IntegrityAlgorithmsListValue
    Phase1IntegrityAlgorithmsListValue (Phase1IntegrityAlgorithmsListValue'),
    newPhase1IntegrityAlgorithmsListValue,

    -- ** Phase1IntegrityAlgorithmsRequestListValue
    Phase1IntegrityAlgorithmsRequestListValue (Phase1IntegrityAlgorithmsRequestListValue'),
    newPhase1IntegrityAlgorithmsRequestListValue,

    -- ** Phase2DHGroupNumbersListValue
    Phase2DHGroupNumbersListValue (Phase2DHGroupNumbersListValue'),
    newPhase2DHGroupNumbersListValue,

    -- ** Phase2DHGroupNumbersRequestListValue
    Phase2DHGroupNumbersRequestListValue (Phase2DHGroupNumbersRequestListValue'),
    newPhase2DHGroupNumbersRequestListValue,

    -- ** Phase2EncryptionAlgorithmsListValue
    Phase2EncryptionAlgorithmsListValue (Phase2EncryptionAlgorithmsListValue'),
    newPhase2EncryptionAlgorithmsListValue,

    -- ** Phase2EncryptionAlgorithmsRequestListValue
    Phase2EncryptionAlgorithmsRequestListValue (Phase2EncryptionAlgorithmsRequestListValue'),
    newPhase2EncryptionAlgorithmsRequestListValue,

    -- ** Phase2IntegrityAlgorithmsListValue
    Phase2IntegrityAlgorithmsListValue (Phase2IntegrityAlgorithmsListValue'),
    newPhase2IntegrityAlgorithmsListValue,

    -- ** Phase2IntegrityAlgorithmsRequestListValue
    Phase2IntegrityAlgorithmsRequestListValue (Phase2IntegrityAlgorithmsRequestListValue'),
    newPhase2IntegrityAlgorithmsRequestListValue,

    -- ** Placement
    Placement (Placement'),
    newPlacement,

    -- ** PlacementGroup
    PlacementGroup (PlacementGroup'),
    newPlacementGroup,

    -- ** PlacementGroupInfo
    PlacementGroupInfo (PlacementGroupInfo'),
    newPlacementGroupInfo,

    -- ** PlacementResponse
    PlacementResponse (PlacementResponse'),
    newPlacementResponse,

    -- ** PoolCidrBlock
    PoolCidrBlock (PoolCidrBlock'),
    newPoolCidrBlock,

    -- ** PortRange
    PortRange (PortRange'),
    newPortRange,

    -- ** PrefixList
    PrefixList (PrefixList'),
    newPrefixList,

    -- ** PrefixListAssociation
    PrefixListAssociation (PrefixListAssociation'),
    newPrefixListAssociation,

    -- ** PrefixListEntry
    PrefixListEntry (PrefixListEntry'),
    newPrefixListEntry,

    -- ** PrefixListId
    PrefixListId (PrefixListId'),
    newPrefixListId,

    -- ** PriceSchedule
    PriceSchedule (PriceSchedule'),
    newPriceSchedule,

    -- ** PriceScheduleSpecification
    PriceScheduleSpecification (PriceScheduleSpecification'),
    newPriceScheduleSpecification,

    -- ** PricingDetail
    PricingDetail (PricingDetail'),
    newPricingDetail,

    -- ** PrincipalIdFormat
    PrincipalIdFormat (PrincipalIdFormat'),
    newPrincipalIdFormat,

    -- ** PrivateDnsDetails
    PrivateDnsDetails (PrivateDnsDetails'),
    newPrivateDnsDetails,

    -- ** PrivateDnsNameConfiguration
    PrivateDnsNameConfiguration (PrivateDnsNameConfiguration'),
    newPrivateDnsNameConfiguration,

    -- ** PrivateDnsNameOptionsOnLaunch
    PrivateDnsNameOptionsOnLaunch (PrivateDnsNameOptionsOnLaunch'),
    newPrivateDnsNameOptionsOnLaunch,

    -- ** PrivateDnsNameOptionsRequest
    PrivateDnsNameOptionsRequest (PrivateDnsNameOptionsRequest'),
    newPrivateDnsNameOptionsRequest,

    -- ** PrivateDnsNameOptionsResponse
    PrivateDnsNameOptionsResponse (PrivateDnsNameOptionsResponse'),
    newPrivateDnsNameOptionsResponse,

    -- ** PrivateIpAddressSpecification
    PrivateIpAddressSpecification (PrivateIpAddressSpecification'),
    newPrivateIpAddressSpecification,

    -- ** ProcessorInfo
    ProcessorInfo (ProcessorInfo'),
    newProcessorInfo,

    -- ** ProductCode
    ProductCode (ProductCode'),
    newProductCode,

    -- ** PropagatingVgw
    PropagatingVgw (PropagatingVgw'),
    newPropagatingVgw,

    -- ** ProvisionedBandwidth
    ProvisionedBandwidth (ProvisionedBandwidth'),
    newProvisionedBandwidth,

    -- ** PtrUpdateStatus
    PtrUpdateStatus (PtrUpdateStatus'),
    newPtrUpdateStatus,

    -- ** PublicIpv4Pool
    PublicIpv4Pool (PublicIpv4Pool'),
    newPublicIpv4Pool,

    -- ** PublicIpv4PoolRange
    PublicIpv4PoolRange (PublicIpv4PoolRange'),
    newPublicIpv4PoolRange,

    -- ** Purchase
    Purchase (Purchase'),
    newPurchase,

    -- ** PurchaseRequest
    PurchaseRequest (PurchaseRequest'),
    newPurchaseRequest,

    -- ** RecurringCharge
    RecurringCharge (RecurringCharge'),
    newRecurringCharge,

    -- ** ReferencedSecurityGroup
    ReferencedSecurityGroup (ReferencedSecurityGroup'),
    newReferencedSecurityGroup,

    -- ** RegionInfo
    RegionInfo (RegionInfo'),
    newRegionInfo,

    -- ** RegisterInstanceTagAttributeRequest
    RegisterInstanceTagAttributeRequest (RegisterInstanceTagAttributeRequest'),
    newRegisterInstanceTagAttributeRequest,

    -- ** RemoveIpamOperatingRegion
    RemoveIpamOperatingRegion (RemoveIpamOperatingRegion'),
    newRemoveIpamOperatingRegion,

    -- ** RemovePrefixListEntry
    RemovePrefixListEntry (RemovePrefixListEntry'),
    newRemovePrefixListEntry,

    -- ** ReplaceRootVolumeTask
    ReplaceRootVolumeTask (ReplaceRootVolumeTask'),
    newReplaceRootVolumeTask,

    -- ** RequestFilterPortRange
    RequestFilterPortRange (RequestFilterPortRange'),
    newRequestFilterPortRange,

    -- ** RequestIpamResourceTag
    RequestIpamResourceTag (RequestIpamResourceTag'),
    newRequestIpamResourceTag,

    -- ** RequestLaunchTemplateData
    RequestLaunchTemplateData (RequestLaunchTemplateData'),
    newRequestLaunchTemplateData,

    -- ** RequestSpotLaunchSpecification
    RequestSpotLaunchSpecification (RequestSpotLaunchSpecification'),
    newRequestSpotLaunchSpecification,

    -- ** Reservation
    Reservation (Reservation'),
    newReservation,

    -- ** ReservationFleetInstanceSpecification
    ReservationFleetInstanceSpecification (ReservationFleetInstanceSpecification'),
    newReservationFleetInstanceSpecification,

    -- ** ReservationValue
    ReservationValue (ReservationValue'),
    newReservationValue,

    -- ** ReservedInstanceLimitPrice
    ReservedInstanceLimitPrice (ReservedInstanceLimitPrice'),
    newReservedInstanceLimitPrice,

    -- ** ReservedInstanceReservationValue
    ReservedInstanceReservationValue (ReservedInstanceReservationValue'),
    newReservedInstanceReservationValue,

    -- ** ReservedInstances
    ReservedInstances (ReservedInstances'),
    newReservedInstances,

    -- ** ReservedInstancesConfiguration
    ReservedInstancesConfiguration (ReservedInstancesConfiguration'),
    newReservedInstancesConfiguration,

    -- ** ReservedInstancesId
    ReservedInstancesId (ReservedInstancesId'),
    newReservedInstancesId,

    -- ** ReservedInstancesListing
    ReservedInstancesListing (ReservedInstancesListing'),
    newReservedInstancesListing,

    -- ** ReservedInstancesModification
    ReservedInstancesModification (ReservedInstancesModification'),
    newReservedInstancesModification,

    -- ** ReservedInstancesModificationResult
    ReservedInstancesModificationResult (ReservedInstancesModificationResult'),
    newReservedInstancesModificationResult,

    -- ** ReservedInstancesOffering
    ReservedInstancesOffering (ReservedInstancesOffering'),
    newReservedInstancesOffering,

    -- ** ResourceStatement
    ResourceStatement (ResourceStatement'),
    newResourceStatement,

    -- ** ResourceStatementRequest
    ResourceStatementRequest (ResourceStatementRequest'),
    newResourceStatementRequest,

    -- ** ResponseError
    ResponseError (ResponseError'),
    newResponseError,

    -- ** ResponseLaunchTemplateData
    ResponseLaunchTemplateData (ResponseLaunchTemplateData'),
    newResponseLaunchTemplateData,

    -- ** Route
    Route (Route'),
    newRoute,

    -- ** RouteTable
    RouteTable (RouteTable'),
    newRouteTable,

    -- ** RouteTableAssociation
    RouteTableAssociation (RouteTableAssociation'),
    newRouteTableAssociation,

    -- ** RouteTableAssociationState
    RouteTableAssociationState (RouteTableAssociationState'),
    newRouteTableAssociationState,

    -- ** RuleGroupRuleOptionsPair
    RuleGroupRuleOptionsPair (RuleGroupRuleOptionsPair'),
    newRuleGroupRuleOptionsPair,

    -- ** RuleGroupTypePair
    RuleGroupTypePair (RuleGroupTypePair'),
    newRuleGroupTypePair,

    -- ** RuleOption
    RuleOption (RuleOption'),
    newRuleOption,

    -- ** RunInstancesMonitoringEnabled
    RunInstancesMonitoringEnabled (RunInstancesMonitoringEnabled'),
    newRunInstancesMonitoringEnabled,

    -- ** S3ObjectTag
    S3ObjectTag (S3ObjectTag'),
    newS3ObjectTag,

    -- ** S3Storage
    S3Storage (S3Storage'),
    newS3Storage,

    -- ** ScheduledInstance
    ScheduledInstance (ScheduledInstance'),
    newScheduledInstance,

    -- ** ScheduledInstanceAvailability
    ScheduledInstanceAvailability (ScheduledInstanceAvailability'),
    newScheduledInstanceAvailability,

    -- ** ScheduledInstanceRecurrence
    ScheduledInstanceRecurrence (ScheduledInstanceRecurrence'),
    newScheduledInstanceRecurrence,

    -- ** ScheduledInstanceRecurrenceRequest
    ScheduledInstanceRecurrenceRequest (ScheduledInstanceRecurrenceRequest'),
    newScheduledInstanceRecurrenceRequest,

    -- ** ScheduledInstancesBlockDeviceMapping
    ScheduledInstancesBlockDeviceMapping (ScheduledInstancesBlockDeviceMapping'),
    newScheduledInstancesBlockDeviceMapping,

    -- ** ScheduledInstancesEbs
    ScheduledInstancesEbs (ScheduledInstancesEbs'),
    newScheduledInstancesEbs,

    -- ** ScheduledInstancesIamInstanceProfile
    ScheduledInstancesIamInstanceProfile (ScheduledInstancesIamInstanceProfile'),
    newScheduledInstancesIamInstanceProfile,

    -- ** ScheduledInstancesIpv6Address
    ScheduledInstancesIpv6Address (ScheduledInstancesIpv6Address'),
    newScheduledInstancesIpv6Address,

    -- ** ScheduledInstancesLaunchSpecification
    ScheduledInstancesLaunchSpecification (ScheduledInstancesLaunchSpecification'),
    newScheduledInstancesLaunchSpecification,

    -- ** ScheduledInstancesMonitoring
    ScheduledInstancesMonitoring (ScheduledInstancesMonitoring'),
    newScheduledInstancesMonitoring,

    -- ** ScheduledInstancesNetworkInterface
    ScheduledInstancesNetworkInterface (ScheduledInstancesNetworkInterface'),
    newScheduledInstancesNetworkInterface,

    -- ** ScheduledInstancesPlacement
    ScheduledInstancesPlacement (ScheduledInstancesPlacement'),
    newScheduledInstancesPlacement,

    -- ** ScheduledInstancesPrivateIpAddressConfig
    ScheduledInstancesPrivateIpAddressConfig (ScheduledInstancesPrivateIpAddressConfig'),
    newScheduledInstancesPrivateIpAddressConfig,

    -- ** SecurityGroup
    SecurityGroup (SecurityGroup'),
    newSecurityGroup,

    -- ** SecurityGroupIdentifier
    SecurityGroupIdentifier (SecurityGroupIdentifier'),
    newSecurityGroupIdentifier,

    -- ** SecurityGroupReference
    SecurityGroupReference (SecurityGroupReference'),
    newSecurityGroupReference,

    -- ** SecurityGroupRule
    SecurityGroupRule (SecurityGroupRule'),
    newSecurityGroupRule,

    -- ** SecurityGroupRuleDescription
    SecurityGroupRuleDescription (SecurityGroupRuleDescription'),
    newSecurityGroupRuleDescription,

    -- ** SecurityGroupRuleRequest
    SecurityGroupRuleRequest (SecurityGroupRuleRequest'),
    newSecurityGroupRuleRequest,

    -- ** SecurityGroupRuleUpdate
    SecurityGroupRuleUpdate (SecurityGroupRuleUpdate'),
    newSecurityGroupRuleUpdate,

    -- ** ServiceConfiguration
    ServiceConfiguration (ServiceConfiguration'),
    newServiceConfiguration,

    -- ** ServiceDetail
    ServiceDetail (ServiceDetail'),
    newServiceDetail,

    -- ** ServiceTypeDetail
    ServiceTypeDetail (ServiceTypeDetail'),
    newServiceTypeDetail,

    -- ** SlotDateTimeRangeRequest
    SlotDateTimeRangeRequest (SlotDateTimeRangeRequest'),
    newSlotDateTimeRangeRequest,

    -- ** SlotStartTimeRangeRequest
    SlotStartTimeRangeRequest (SlotStartTimeRangeRequest'),
    newSlotStartTimeRangeRequest,

    -- ** Snapshot
    Snapshot (Snapshot'),
    newSnapshot,

    -- ** SnapshotDetail
    SnapshotDetail (SnapshotDetail'),
    newSnapshotDetail,

    -- ** SnapshotDiskContainer
    SnapshotDiskContainer (SnapshotDiskContainer'),
    newSnapshotDiskContainer,

    -- ** SnapshotInfo
    SnapshotInfo (SnapshotInfo'),
    newSnapshotInfo,

    -- ** SnapshotRecycleBinInfo
    SnapshotRecycleBinInfo (SnapshotRecycleBinInfo'),
    newSnapshotRecycleBinInfo,

    -- ** SnapshotTaskDetail
    SnapshotTaskDetail (SnapshotTaskDetail'),
    newSnapshotTaskDetail,

    -- ** SnapshotTierStatus
    SnapshotTierStatus (SnapshotTierStatus'),
    newSnapshotTierStatus,

    -- ** SpotCapacityRebalance
    SpotCapacityRebalance (SpotCapacityRebalance'),
    newSpotCapacityRebalance,

    -- ** SpotDatafeedSubscription
    SpotDatafeedSubscription (SpotDatafeedSubscription'),
    newSpotDatafeedSubscription,

    -- ** SpotFleetLaunchSpecification
    SpotFleetLaunchSpecification (SpotFleetLaunchSpecification'),
    newSpotFleetLaunchSpecification,

    -- ** SpotFleetMonitoring
    SpotFleetMonitoring (SpotFleetMonitoring'),
    newSpotFleetMonitoring,

    -- ** SpotFleetRequestConfig
    SpotFleetRequestConfig (SpotFleetRequestConfig'),
    newSpotFleetRequestConfig,

    -- ** SpotFleetRequestConfigData
    SpotFleetRequestConfigData (SpotFleetRequestConfigData'),
    newSpotFleetRequestConfigData,

    -- ** SpotFleetTagSpecification
    SpotFleetTagSpecification (SpotFleetTagSpecification'),
    newSpotFleetTagSpecification,

    -- ** SpotInstanceRequest
    SpotInstanceRequest (SpotInstanceRequest'),
    newSpotInstanceRequest,

    -- ** SpotInstanceStateFault
    SpotInstanceStateFault (SpotInstanceStateFault'),
    newSpotInstanceStateFault,

    -- ** SpotInstanceStatus
    SpotInstanceStatus (SpotInstanceStatus'),
    newSpotInstanceStatus,

    -- ** SpotMaintenanceStrategies
    SpotMaintenanceStrategies (SpotMaintenanceStrategies'),
    newSpotMaintenanceStrategies,

    -- ** SpotMarketOptions
    SpotMarketOptions (SpotMarketOptions'),
    newSpotMarketOptions,

    -- ** SpotOptions
    SpotOptions (SpotOptions'),
    newSpotOptions,

    -- ** SpotOptionsRequest
    SpotOptionsRequest (SpotOptionsRequest'),
    newSpotOptionsRequest,

    -- ** SpotPlacement
    SpotPlacement (SpotPlacement'),
    newSpotPlacement,

    -- ** SpotPlacementScore
    SpotPlacementScore (SpotPlacementScore'),
    newSpotPlacementScore,

    -- ** SpotPrice
    SpotPrice (SpotPrice'),
    newSpotPrice,

    -- ** StaleIpPermission
    StaleIpPermission (StaleIpPermission'),
    newStaleIpPermission,

    -- ** StaleSecurityGroup
    StaleSecurityGroup (StaleSecurityGroup'),
    newStaleSecurityGroup,

    -- ** StateReason
    StateReason (StateReason'),
    newStateReason,

    -- ** Storage
    Storage (Storage'),
    newStorage,

    -- ** StorageLocation
    StorageLocation (StorageLocation'),
    newStorageLocation,

    -- ** StoreImageTaskResult
    StoreImageTaskResult (StoreImageTaskResult'),
    newStoreImageTaskResult,

    -- ** Subnet
    Subnet (Subnet'),
    newSubnet,

    -- ** SubnetAssociation
    SubnetAssociation (SubnetAssociation'),
    newSubnetAssociation,

    -- ** SubnetCidrBlockState
    SubnetCidrBlockState (SubnetCidrBlockState'),
    newSubnetCidrBlockState,

    -- ** SubnetCidrReservation
    SubnetCidrReservation (SubnetCidrReservation'),
    newSubnetCidrReservation,

    -- ** SubnetIpv6CidrBlockAssociation
    SubnetIpv6CidrBlockAssociation (SubnetIpv6CidrBlockAssociation'),
    newSubnetIpv6CidrBlockAssociation,

    -- ** Subscription
    Subscription (Subscription'),
    newSubscription,

    -- ** SuccessfulInstanceCreditSpecificationItem
    SuccessfulInstanceCreditSpecificationItem (SuccessfulInstanceCreditSpecificationItem'),
    newSuccessfulInstanceCreditSpecificationItem,

    -- ** SuccessfulQueuedPurchaseDeletion
    SuccessfulQueuedPurchaseDeletion (SuccessfulQueuedPurchaseDeletion'),
    newSuccessfulQueuedPurchaseDeletion,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TagDescription
    TagDescription (TagDescription'),
    newTagDescription,

    -- ** TagSpecification
    TagSpecification (TagSpecification'),
    newTagSpecification,

    -- ** TargetCapacitySpecification
    TargetCapacitySpecification (TargetCapacitySpecification'),
    newTargetCapacitySpecification,

    -- ** TargetCapacitySpecificationRequest
    TargetCapacitySpecificationRequest (TargetCapacitySpecificationRequest'),
    newTargetCapacitySpecificationRequest,

    -- ** TargetConfiguration
    TargetConfiguration (TargetConfiguration'),
    newTargetConfiguration,

    -- ** TargetConfigurationRequest
    TargetConfigurationRequest (TargetConfigurationRequest'),
    newTargetConfigurationRequest,

    -- ** TargetGroup
    TargetGroup (TargetGroup'),
    newTargetGroup,

    -- ** TargetGroupsConfig
    TargetGroupsConfig (TargetGroupsConfig'),
    newTargetGroupsConfig,

    -- ** TargetNetwork
    TargetNetwork (TargetNetwork'),
    newTargetNetwork,

    -- ** TargetReservationValue
    TargetReservationValue (TargetReservationValue'),
    newTargetReservationValue,

    -- ** TerminateConnectionStatus
    TerminateConnectionStatus (TerminateConnectionStatus'),
    newTerminateConnectionStatus,

    -- ** ThroughResourcesStatement
    ThroughResourcesStatement (ThroughResourcesStatement'),
    newThroughResourcesStatement,

    -- ** ThroughResourcesStatementRequest
    ThroughResourcesStatementRequest (ThroughResourcesStatementRequest'),
    newThroughResourcesStatementRequest,

    -- ** TotalLocalStorageGB
    TotalLocalStorageGB (TotalLocalStorageGB'),
    newTotalLocalStorageGB,

    -- ** TotalLocalStorageGBRequest
    TotalLocalStorageGBRequest (TotalLocalStorageGBRequest'),
    newTotalLocalStorageGBRequest,

    -- ** TrafficMirrorFilter
    TrafficMirrorFilter (TrafficMirrorFilter'),
    newTrafficMirrorFilter,

    -- ** TrafficMirrorFilterRule
    TrafficMirrorFilterRule (TrafficMirrorFilterRule'),
    newTrafficMirrorFilterRule,

    -- ** TrafficMirrorPortRange
    TrafficMirrorPortRange (TrafficMirrorPortRange'),
    newTrafficMirrorPortRange,

    -- ** TrafficMirrorPortRangeRequest
    TrafficMirrorPortRangeRequest (TrafficMirrorPortRangeRequest'),
    newTrafficMirrorPortRangeRequest,

    -- ** TrafficMirrorSession
    TrafficMirrorSession (TrafficMirrorSession'),
    newTrafficMirrorSession,

    -- ** TrafficMirrorTarget
    TrafficMirrorTarget (TrafficMirrorTarget'),
    newTrafficMirrorTarget,

    -- ** TransitGateway
    TransitGateway (TransitGateway'),
    newTransitGateway,

    -- ** TransitGatewayAssociation
    TransitGatewayAssociation (TransitGatewayAssociation'),
    newTransitGatewayAssociation,

    -- ** TransitGatewayAttachment
    TransitGatewayAttachment (TransitGatewayAttachment'),
    newTransitGatewayAttachment,

    -- ** TransitGatewayAttachmentAssociation
    TransitGatewayAttachmentAssociation (TransitGatewayAttachmentAssociation'),
    newTransitGatewayAttachmentAssociation,

    -- ** TransitGatewayAttachmentBgpConfiguration
    TransitGatewayAttachmentBgpConfiguration (TransitGatewayAttachmentBgpConfiguration'),
    newTransitGatewayAttachmentBgpConfiguration,

    -- ** TransitGatewayAttachmentPropagation
    TransitGatewayAttachmentPropagation (TransitGatewayAttachmentPropagation'),
    newTransitGatewayAttachmentPropagation,

    -- ** TransitGatewayConnect
    TransitGatewayConnect (TransitGatewayConnect'),
    newTransitGatewayConnect,

    -- ** TransitGatewayConnectOptions
    TransitGatewayConnectOptions (TransitGatewayConnectOptions'),
    newTransitGatewayConnectOptions,

    -- ** TransitGatewayConnectPeer
    TransitGatewayConnectPeer (TransitGatewayConnectPeer'),
    newTransitGatewayConnectPeer,

    -- ** TransitGatewayConnectPeerConfiguration
    TransitGatewayConnectPeerConfiguration (TransitGatewayConnectPeerConfiguration'),
    newTransitGatewayConnectPeerConfiguration,

    -- ** TransitGatewayConnectRequestBgpOptions
    TransitGatewayConnectRequestBgpOptions (TransitGatewayConnectRequestBgpOptions'),
    newTransitGatewayConnectRequestBgpOptions,

    -- ** TransitGatewayMulticastDeregisteredGroupMembers
    TransitGatewayMulticastDeregisteredGroupMembers (TransitGatewayMulticastDeregisteredGroupMembers'),
    newTransitGatewayMulticastDeregisteredGroupMembers,

    -- ** TransitGatewayMulticastDeregisteredGroupSources
    TransitGatewayMulticastDeregisteredGroupSources (TransitGatewayMulticastDeregisteredGroupSources'),
    newTransitGatewayMulticastDeregisteredGroupSources,

    -- ** TransitGatewayMulticastDomain
    TransitGatewayMulticastDomain (TransitGatewayMulticastDomain'),
    newTransitGatewayMulticastDomain,

    -- ** TransitGatewayMulticastDomainAssociation
    TransitGatewayMulticastDomainAssociation (TransitGatewayMulticastDomainAssociation'),
    newTransitGatewayMulticastDomainAssociation,

    -- ** TransitGatewayMulticastDomainAssociations
    TransitGatewayMulticastDomainAssociations (TransitGatewayMulticastDomainAssociations'),
    newTransitGatewayMulticastDomainAssociations,

    -- ** TransitGatewayMulticastDomainOptions
    TransitGatewayMulticastDomainOptions (TransitGatewayMulticastDomainOptions'),
    newTransitGatewayMulticastDomainOptions,

    -- ** TransitGatewayMulticastGroup
    TransitGatewayMulticastGroup (TransitGatewayMulticastGroup'),
    newTransitGatewayMulticastGroup,

    -- ** TransitGatewayMulticastRegisteredGroupMembers
    TransitGatewayMulticastRegisteredGroupMembers (TransitGatewayMulticastRegisteredGroupMembers'),
    newTransitGatewayMulticastRegisteredGroupMembers,

    -- ** TransitGatewayMulticastRegisteredGroupSources
    TransitGatewayMulticastRegisteredGroupSources (TransitGatewayMulticastRegisteredGroupSources'),
    newTransitGatewayMulticastRegisteredGroupSources,

    -- ** TransitGatewayOptions
    TransitGatewayOptions (TransitGatewayOptions'),
    newTransitGatewayOptions,

    -- ** TransitGatewayPeeringAttachment
    TransitGatewayPeeringAttachment (TransitGatewayPeeringAttachment'),
    newTransitGatewayPeeringAttachment,

    -- ** TransitGatewayPeeringAttachmentOptions
    TransitGatewayPeeringAttachmentOptions (TransitGatewayPeeringAttachmentOptions'),
    newTransitGatewayPeeringAttachmentOptions,

    -- ** TransitGatewayPolicyRule
    TransitGatewayPolicyRule (TransitGatewayPolicyRule'),
    newTransitGatewayPolicyRule,

    -- ** TransitGatewayPolicyRuleMetaData
    TransitGatewayPolicyRuleMetaData (TransitGatewayPolicyRuleMetaData'),
    newTransitGatewayPolicyRuleMetaData,

    -- ** TransitGatewayPolicyTable
    TransitGatewayPolicyTable (TransitGatewayPolicyTable'),
    newTransitGatewayPolicyTable,

    -- ** TransitGatewayPolicyTableAssociation
    TransitGatewayPolicyTableAssociation (TransitGatewayPolicyTableAssociation'),
    newTransitGatewayPolicyTableAssociation,

    -- ** TransitGatewayPolicyTableEntry
    TransitGatewayPolicyTableEntry (TransitGatewayPolicyTableEntry'),
    newTransitGatewayPolicyTableEntry,

    -- ** TransitGatewayPrefixListAttachment
    TransitGatewayPrefixListAttachment (TransitGatewayPrefixListAttachment'),
    newTransitGatewayPrefixListAttachment,

    -- ** TransitGatewayPrefixListReference
    TransitGatewayPrefixListReference (TransitGatewayPrefixListReference'),
    newTransitGatewayPrefixListReference,

    -- ** TransitGatewayPropagation
    TransitGatewayPropagation (TransitGatewayPropagation'),
    newTransitGatewayPropagation,

    -- ** TransitGatewayRequestOptions
    TransitGatewayRequestOptions (TransitGatewayRequestOptions'),
    newTransitGatewayRequestOptions,

    -- ** TransitGatewayRoute
    TransitGatewayRoute (TransitGatewayRoute'),
    newTransitGatewayRoute,

    -- ** TransitGatewayRouteAttachment
    TransitGatewayRouteAttachment (TransitGatewayRouteAttachment'),
    newTransitGatewayRouteAttachment,

    -- ** TransitGatewayRouteTable
    TransitGatewayRouteTable (TransitGatewayRouteTable'),
    newTransitGatewayRouteTable,

    -- ** TransitGatewayRouteTableAnnouncement
    TransitGatewayRouteTableAnnouncement (TransitGatewayRouteTableAnnouncement'),
    newTransitGatewayRouteTableAnnouncement,

    -- ** TransitGatewayRouteTableAssociation
    TransitGatewayRouteTableAssociation (TransitGatewayRouteTableAssociation'),
    newTransitGatewayRouteTableAssociation,

    -- ** TransitGatewayRouteTablePropagation
    TransitGatewayRouteTablePropagation (TransitGatewayRouteTablePropagation'),
    newTransitGatewayRouteTablePropagation,

    -- ** TransitGatewayRouteTableRoute
    TransitGatewayRouteTableRoute (TransitGatewayRouteTableRoute'),
    newTransitGatewayRouteTableRoute,

    -- ** TransitGatewayVpcAttachment
    TransitGatewayVpcAttachment (TransitGatewayVpcAttachment'),
    newTransitGatewayVpcAttachment,

    -- ** TransitGatewayVpcAttachmentOptions
    TransitGatewayVpcAttachmentOptions (TransitGatewayVpcAttachmentOptions'),
    newTransitGatewayVpcAttachmentOptions,

    -- ** TrunkInterfaceAssociation
    TrunkInterfaceAssociation (TrunkInterfaceAssociation'),
    newTrunkInterfaceAssociation,

    -- ** TunnelOption
    TunnelOption (TunnelOption'),
    newTunnelOption,

    -- ** UnsuccessfulInstanceCreditSpecificationItem
    UnsuccessfulInstanceCreditSpecificationItem (UnsuccessfulInstanceCreditSpecificationItem'),
    newUnsuccessfulInstanceCreditSpecificationItem,

    -- ** UnsuccessfulInstanceCreditSpecificationItemError
    UnsuccessfulInstanceCreditSpecificationItemError (UnsuccessfulInstanceCreditSpecificationItemError'),
    newUnsuccessfulInstanceCreditSpecificationItemError,

    -- ** UnsuccessfulItem
    UnsuccessfulItem (UnsuccessfulItem'),
    newUnsuccessfulItem,

    -- ** UnsuccessfulItemError
    UnsuccessfulItemError (UnsuccessfulItemError'),
    newUnsuccessfulItemError,

    -- ** UserBucket
    UserBucket (UserBucket'),
    newUserBucket,

    -- ** UserBucketDetails
    UserBucketDetails (UserBucketDetails'),
    newUserBucketDetails,

    -- ** UserData
    UserData (UserData'),
    newUserData,

    -- ** UserIdGroupPair
    UserIdGroupPair (UserIdGroupPair'),
    newUserIdGroupPair,

    -- ** VCpuCountRange
    VCpuCountRange (VCpuCountRange'),
    newVCpuCountRange,

    -- ** VCpuCountRangeRequest
    VCpuCountRangeRequest (VCpuCountRangeRequest'),
    newVCpuCountRangeRequest,

    -- ** VCpuInfo
    VCpuInfo (VCpuInfo'),
    newVCpuInfo,

    -- ** ValidationError
    ValidationError (ValidationError'),
    newValidationError,

    -- ** ValidationWarning
    ValidationWarning (ValidationWarning'),
    newValidationWarning,

    -- ** VerifiedAccessEndpoint
    VerifiedAccessEndpoint (VerifiedAccessEndpoint'),
    newVerifiedAccessEndpoint,

    -- ** VerifiedAccessEndpointEniOptions
    VerifiedAccessEndpointEniOptions (VerifiedAccessEndpointEniOptions'),
    newVerifiedAccessEndpointEniOptions,

    -- ** VerifiedAccessEndpointLoadBalancerOptions
    VerifiedAccessEndpointLoadBalancerOptions (VerifiedAccessEndpointLoadBalancerOptions'),
    newVerifiedAccessEndpointLoadBalancerOptions,

    -- ** VerifiedAccessEndpointStatus
    VerifiedAccessEndpointStatus (VerifiedAccessEndpointStatus'),
    newVerifiedAccessEndpointStatus,

    -- ** VerifiedAccessGroup
    VerifiedAccessGroup (VerifiedAccessGroup'),
    newVerifiedAccessGroup,

    -- ** VerifiedAccessInstance
    VerifiedAccessInstance (VerifiedAccessInstance'),
    newVerifiedAccessInstance,

    -- ** VerifiedAccessInstanceLoggingConfiguration
    VerifiedAccessInstanceLoggingConfiguration (VerifiedAccessInstanceLoggingConfiguration'),
    newVerifiedAccessInstanceLoggingConfiguration,

    -- ** VerifiedAccessLogCloudWatchLogsDestination
    VerifiedAccessLogCloudWatchLogsDestination (VerifiedAccessLogCloudWatchLogsDestination'),
    newVerifiedAccessLogCloudWatchLogsDestination,

    -- ** VerifiedAccessLogCloudWatchLogsDestinationOptions
    VerifiedAccessLogCloudWatchLogsDestinationOptions (VerifiedAccessLogCloudWatchLogsDestinationOptions'),
    newVerifiedAccessLogCloudWatchLogsDestinationOptions,

    -- ** VerifiedAccessLogDeliveryStatus
    VerifiedAccessLogDeliveryStatus (VerifiedAccessLogDeliveryStatus'),
    newVerifiedAccessLogDeliveryStatus,

    -- ** VerifiedAccessLogKinesisDataFirehoseDestination
    VerifiedAccessLogKinesisDataFirehoseDestination (VerifiedAccessLogKinesisDataFirehoseDestination'),
    newVerifiedAccessLogKinesisDataFirehoseDestination,

    -- ** VerifiedAccessLogKinesisDataFirehoseDestinationOptions
    VerifiedAccessLogKinesisDataFirehoseDestinationOptions (VerifiedAccessLogKinesisDataFirehoseDestinationOptions'),
    newVerifiedAccessLogKinesisDataFirehoseDestinationOptions,

    -- ** VerifiedAccessLogOptions
    VerifiedAccessLogOptions (VerifiedAccessLogOptions'),
    newVerifiedAccessLogOptions,

    -- ** VerifiedAccessLogS3Destination
    VerifiedAccessLogS3Destination (VerifiedAccessLogS3Destination'),
    newVerifiedAccessLogS3Destination,

    -- ** VerifiedAccessLogS3DestinationOptions
    VerifiedAccessLogS3DestinationOptions (VerifiedAccessLogS3DestinationOptions'),
    newVerifiedAccessLogS3DestinationOptions,

    -- ** VerifiedAccessLogs
    VerifiedAccessLogs (VerifiedAccessLogs'),
    newVerifiedAccessLogs,

    -- ** VerifiedAccessTrustProvider
    VerifiedAccessTrustProvider (VerifiedAccessTrustProvider'),
    newVerifiedAccessTrustProvider,

    -- ** VerifiedAccessTrustProviderCondensed
    VerifiedAccessTrustProviderCondensed (VerifiedAccessTrustProviderCondensed'),
    newVerifiedAccessTrustProviderCondensed,

    -- ** VgwTelemetry
    VgwTelemetry (VgwTelemetry'),
    newVgwTelemetry,

    -- ** Volume
    Volume (Volume'),
    newVolume,

    -- ** VolumeAttachment
    VolumeAttachment (VolumeAttachment'),
    newVolumeAttachment,

    -- ** VolumeDetail
    VolumeDetail (VolumeDetail'),
    newVolumeDetail,

    -- ** VolumeModification
    VolumeModification (VolumeModification'),
    newVolumeModification,

    -- ** VolumeStatusAction
    VolumeStatusAction (VolumeStatusAction'),
    newVolumeStatusAction,

    -- ** VolumeStatusAttachmentStatus
    VolumeStatusAttachmentStatus (VolumeStatusAttachmentStatus'),
    newVolumeStatusAttachmentStatus,

    -- ** VolumeStatusDetails
    VolumeStatusDetails (VolumeStatusDetails'),
    newVolumeStatusDetails,

    -- ** VolumeStatusEvent
    VolumeStatusEvent (VolumeStatusEvent'),
    newVolumeStatusEvent,

    -- ** VolumeStatusInfo
    VolumeStatusInfo (VolumeStatusInfo'),
    newVolumeStatusInfo,

    -- ** VolumeStatusItem
    VolumeStatusItem (VolumeStatusItem'),
    newVolumeStatusItem,

    -- ** Vpc
    Vpc (Vpc'),
    newVpc,

    -- ** VpcAttachment
    VpcAttachment (VpcAttachment'),
    newVpcAttachment,

    -- ** VpcCidrBlockAssociation
    VpcCidrBlockAssociation (VpcCidrBlockAssociation'),
    newVpcCidrBlockAssociation,

    -- ** VpcCidrBlockState
    VpcCidrBlockState (VpcCidrBlockState'),
    newVpcCidrBlockState,

    -- ** VpcClassicLink
    VpcClassicLink (VpcClassicLink'),
    newVpcClassicLink,

    -- ** VpcEndpoint
    VpcEndpoint (VpcEndpoint'),
    newVpcEndpoint,

    -- ** VpcEndpointConnection
    VpcEndpointConnection (VpcEndpointConnection'),
    newVpcEndpointConnection,

    -- ** VpcIpv6CidrBlockAssociation
    VpcIpv6CidrBlockAssociation (VpcIpv6CidrBlockAssociation'),
    newVpcIpv6CidrBlockAssociation,

    -- ** VpcPeeringConnection
    VpcPeeringConnection (VpcPeeringConnection'),
    newVpcPeeringConnection,

    -- ** VpcPeeringConnectionOptionsDescription
    VpcPeeringConnectionOptionsDescription (VpcPeeringConnectionOptionsDescription'),
    newVpcPeeringConnectionOptionsDescription,

    -- ** VpcPeeringConnectionStateReason
    VpcPeeringConnectionStateReason (VpcPeeringConnectionStateReason'),
    newVpcPeeringConnectionStateReason,

    -- ** VpcPeeringConnectionVpcInfo
    VpcPeeringConnectionVpcInfo (VpcPeeringConnectionVpcInfo'),
    newVpcPeeringConnectionVpcInfo,

    -- ** VpnConnection
    VpnConnection (VpnConnection'),
    newVpnConnection,

    -- ** VpnConnectionDeviceType
    VpnConnectionDeviceType (VpnConnectionDeviceType'),
    newVpnConnectionDeviceType,

    -- ** VpnConnectionOptions
    VpnConnectionOptions (VpnConnectionOptions'),
    newVpnConnectionOptions,

    -- ** VpnConnectionOptionsSpecification
    VpnConnectionOptionsSpecification (VpnConnectionOptionsSpecification'),
    newVpnConnectionOptionsSpecification,

    -- ** VpnGateway
    VpnGateway (VpnGateway'),
    newVpnGateway,

    -- ** VpnStaticRoute
    VpnStaticRoute (VpnStaticRoute'),
    newVpnStaticRoute,

    -- ** VpnTunnelLogOptions
    VpnTunnelLogOptions (VpnTunnelLogOptions'),
    newVpnTunnelLogOptions,

    -- ** VpnTunnelLogOptionsSpecification
    VpnTunnelLogOptionsSpecification (VpnTunnelLogOptionsSpecification'),
    newVpnTunnelLogOptionsSpecification,

    -- ** VpnTunnelOptionsSpecification
    VpnTunnelOptionsSpecification (VpnTunnelOptionsSpecification'),
    newVpnTunnelOptionsSpecification,
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
import Amazonka.EC2.Internal
import Amazonka.EC2.Lens
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
import Amazonka.EC2.Types
import Amazonka.EC2.UnassignIpv6Addresses
import Amazonka.EC2.UnassignPrivateIpAddresses
import Amazonka.EC2.UnassignPrivateNatGatewayAddress
import Amazonka.EC2.UnmonitorInstances
import Amazonka.EC2.UpdateSecurityGroupRuleDescriptionsEgress
import Amazonka.EC2.UpdateSecurityGroupRuleDescriptionsIngress
import Amazonka.EC2.Waiters
import Amazonka.EC2.WithdrawByoipCidr

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'EC2'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
