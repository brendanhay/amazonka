{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.EC2
-- Copyright   : (c) 2013-2021 Brendan Hay
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
-- computing capacity in the AWS Cloud. Using Amazon EC2 eliminates the
-- need to invest in hardware up front, so you can develop and deploy
-- applications faster. Amazon Virtual Private Cloud (Amazon VPC) enables
-- you to provision a logically isolated section of the AWS Cloud where you
-- can launch AWS resources in a virtual network that you\'ve defined.
-- Amazon Elastic Block Store (Amazon EBS) provides block level storage
-- volumes for use with EC2 instances. EBS volumes are highly available and
-- reliable storage volumes that can be attached to any running instance
-- and used like a hard drive.
--
-- To learn more, see the following resources:
--
-- -   Amazon EC2: <http://aws.amazon.com/ec2 AmazonEC2 product page>,
--     <http://aws.amazon.com/documentation/ec2 Amazon EC2 documentation>
--
-- -   Amazon EBS: <http://aws.amazon.com/ebs Amazon EBS product page>,
--     <http://aws.amazon.com/documentation/ebs Amazon EBS documentation>
--
-- -   Amazon VPC: <http://aws.amazon.com/vpc Amazon VPC product page>,
--     <http://aws.amazon.com/documentation/vpc Amazon VPC documentation>
--
-- -   AWS VPN: <http://aws.amazon.com/vpn AWS VPN product page>,
--     <http://aws.amazon.com/documentation/vpn AWS VPN documentation>
module Network.AWS.EC2
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- ** InstanceTerminated
    newInstanceTerminated,

    -- ** VolumeInUse
    newVolumeInUse,

    -- ** ImageExists
    newImageExists,

    -- ** NatGatewayAvailable
    newNatGatewayAvailable,

    -- ** SubnetAvailable
    newSubnetAvailable,

    -- ** NetworkInterfaceAvailable
    newNetworkInterfaceAvailable,

    -- ** KeyPairExists
    newKeyPairExists,

    -- ** SystemStatusOk
    newSystemStatusOk,

    -- ** CustomerGatewayAvailable
    newCustomerGatewayAvailable,

    -- ** ConversionTaskCompleted
    newConversionTaskCompleted,

    -- ** InstanceStopped
    newInstanceStopped,

    -- ** ConversionTaskDeleted
    newConversionTaskDeleted,

    -- ** PasswordDataAvailable
    newPasswordDataAvailable,

    -- ** InstanceRunning
    newInstanceRunning,

    -- ** SecurityGroupExists
    newSecurityGroupExists,

    -- ** SpotInstanceRequestFulfilled
    newSpotInstanceRequestFulfilled,

    -- ** VpcAvailable
    newVpcAvailable,

    -- ** ExportTaskCompleted
    newExportTaskCompleted,

    -- ** VpcPeeringConnectionDeleted
    newVpcPeeringConnectionDeleted,

    -- ** VpnConnectionAvailable
    newVpnConnectionAvailable,

    -- ** ExportTaskCancelled
    newExportTaskCancelled,

    -- ** VolumeDeleted
    newVolumeDeleted,

    -- ** VpcExists
    newVpcExists,

    -- ** BundleTaskComplete
    newBundleTaskComplete,

    -- ** VpnConnectionDeleted
    newVpnConnectionDeleted,

    -- ** ConversionTaskCancelled
    newConversionTaskCancelled,

    -- ** ImageAvailable
    newImageAvailable,

    -- ** VpcPeeringConnectionExists
    newVpcPeeringConnectionExists,

    -- ** SnapshotCompleted
    newSnapshotCompleted,

    -- ** InstanceExists
    newInstanceExists,

    -- ** InstanceStatusOk
    newInstanceStatusOk,

    -- ** VolumeAvailable
    newVolumeAvailable,

    -- * Operations
    -- $operations

    -- ** ModifyCapacityReservation
    ModifyCapacityReservation (ModifyCapacityReservation'),
    newModifyCapacityReservation,
    ModifyCapacityReservationResponse (ModifyCapacityReservationResponse'),
    newModifyCapacityReservationResponse,

    -- ** GetAssociatedIpv6PoolCidrs (Paginated)
    GetAssociatedIpv6PoolCidrs (GetAssociatedIpv6PoolCidrs'),
    newGetAssociatedIpv6PoolCidrs,
    GetAssociatedIpv6PoolCidrsResponse (GetAssociatedIpv6PoolCidrsResponse'),
    newGetAssociatedIpv6PoolCidrsResponse,

    -- ** ImportInstance
    ImportInstance (ImportInstance'),
    newImportInstance,
    ImportInstanceResponse (ImportInstanceResponse'),
    newImportInstanceResponse,

    -- ** DescribeCapacityReservationFleets (Paginated)
    DescribeCapacityReservationFleets (DescribeCapacityReservationFleets'),
    newDescribeCapacityReservationFleets,
    DescribeCapacityReservationFleetsResponse (DescribeCapacityReservationFleetsResponse'),
    newDescribeCapacityReservationFleetsResponse,

    -- ** ModifySecurityGroupRules
    ModifySecurityGroupRules (ModifySecurityGroupRules'),
    newModifySecurityGroupRules,
    ModifySecurityGroupRulesResponse (ModifySecurityGroupRulesResponse'),
    newModifySecurityGroupRulesResponse,

    -- ** RevokeSecurityGroupEgress
    RevokeSecurityGroupEgress (RevokeSecurityGroupEgress'),
    newRevokeSecurityGroupEgress,
    RevokeSecurityGroupEgressResponse (RevokeSecurityGroupEgressResponse'),
    newRevokeSecurityGroupEgressResponse,

    -- ** CreateNetworkInterfacePermission
    CreateNetworkInterfacePermission (CreateNetworkInterfacePermission'),
    newCreateNetworkInterfacePermission,
    CreateNetworkInterfacePermissionResponse (CreateNetworkInterfacePermissionResponse'),
    newCreateNetworkInterfacePermissionResponse,

    -- ** SendDiagnosticInterrupt
    SendDiagnosticInterrupt (SendDiagnosticInterrupt'),
    newSendDiagnosticInterrupt,
    SendDiagnosticInterruptResponse (SendDiagnosticInterruptResponse'),
    newSendDiagnosticInterruptResponse,

    -- ** DeleteLaunchTemplate
    DeleteLaunchTemplate (DeleteLaunchTemplate'),
    newDeleteLaunchTemplate,
    DeleteLaunchTemplateResponse (DeleteLaunchTemplateResponse'),
    newDeleteLaunchTemplateResponse,

    -- ** RejectVpcEndpointConnections
    RejectVpcEndpointConnections (RejectVpcEndpointConnections'),
    newRejectVpcEndpointConnections,
    RejectVpcEndpointConnectionsResponse (RejectVpcEndpointConnectionsResponse'),
    newRejectVpcEndpointConnectionsResponse,

    -- ** CreateVpnGateway
    CreateVpnGateway (CreateVpnGateway'),
    newCreateVpnGateway,
    CreateVpnGatewayResponse (CreateVpnGatewayResponse'),
    newCreateVpnGatewayResponse,

    -- ** CreateNetworkAcl
    CreateNetworkAcl (CreateNetworkAcl'),
    newCreateNetworkAcl,
    CreateNetworkAclResponse (CreateNetworkAclResponse'),
    newCreateNetworkAclResponse,

    -- ** DeleteKeyPair
    DeleteKeyPair (DeleteKeyPair'),
    newDeleteKeyPair,
    DeleteKeyPairResponse (DeleteKeyPairResponse'),
    newDeleteKeyPairResponse,

    -- ** DescribeSecurityGroupReferences
    DescribeSecurityGroupReferences (DescribeSecurityGroupReferences'),
    newDescribeSecurityGroupReferences,
    DescribeSecurityGroupReferencesResponse (DescribeSecurityGroupReferencesResponse'),
    newDescribeSecurityGroupReferencesResponse,

    -- ** DeleteFleets
    DeleteFleets (DeleteFleets'),
    newDeleteFleets,
    DeleteFleetsResponse (DeleteFleetsResponse'),
    newDeleteFleetsResponse,

    -- ** DescribeTags (Paginated)
    DescribeTags (DescribeTags'),
    newDescribeTags,
    DescribeTagsResponse (DescribeTagsResponse'),
    newDescribeTagsResponse,

    -- ** CreateTransitGatewayRouteTable
    CreateTransitGatewayRouteTable (CreateTransitGatewayRouteTable'),
    newCreateTransitGatewayRouteTable,
    CreateTransitGatewayRouteTableResponse (CreateTransitGatewayRouteTableResponse'),
    newCreateTransitGatewayRouteTableResponse,

    -- ** ModifyInstanceMetadataOptions
    ModifyInstanceMetadataOptions (ModifyInstanceMetadataOptions'),
    newModifyInstanceMetadataOptions,
    ModifyInstanceMetadataOptionsResponse (ModifyInstanceMetadataOptionsResponse'),
    newModifyInstanceMetadataOptionsResponse,

    -- ** UpdateSecurityGroupRuleDescriptionsIngress
    UpdateSecurityGroupRuleDescriptionsIngress (UpdateSecurityGroupRuleDescriptionsIngress'),
    newUpdateSecurityGroupRuleDescriptionsIngress,
    UpdateSecurityGroupRuleDescriptionsIngressResponse (UpdateSecurityGroupRuleDescriptionsIngressResponse'),
    newUpdateSecurityGroupRuleDescriptionsIngressResponse,

    -- ** DisassociateSubnetCidrBlock
    DisassociateSubnetCidrBlock (DisassociateSubnetCidrBlock'),
    newDisassociateSubnetCidrBlock,
    DisassociateSubnetCidrBlockResponse (DisassociateSubnetCidrBlockResponse'),
    newDisassociateSubnetCidrBlockResponse,

    -- ** DetachNetworkInterface
    DetachNetworkInterface (DetachNetworkInterface'),
    newDetachNetworkInterface,
    DetachNetworkInterfaceResponse (DetachNetworkInterfaceResponse'),
    newDetachNetworkInterfaceResponse,

    -- ** DetachInternetGateway
    DetachInternetGateway (DetachInternetGateway'),
    newDetachInternetGateway,
    DetachInternetGatewayResponse (DetachInternetGatewayResponse'),
    newDetachInternetGatewayResponse,

    -- ** DeleteVpcEndpoints
    DeleteVpcEndpoints (DeleteVpcEndpoints'),
    newDeleteVpcEndpoints,
    DeleteVpcEndpointsResponse (DeleteVpcEndpointsResponse'),
    newDeleteVpcEndpointsResponse,

    -- ** DescribeClientVpnEndpoints (Paginated)
    DescribeClientVpnEndpoints (DescribeClientVpnEndpoints'),
    newDescribeClientVpnEndpoints,
    DescribeClientVpnEndpointsResponse (DescribeClientVpnEndpointsResponse'),
    newDescribeClientVpnEndpointsResponse,

    -- ** DeleteFlowLogs
    DeleteFlowLogs (DeleteFlowLogs'),
    newDeleteFlowLogs,
    DeleteFlowLogsResponse (DeleteFlowLogsResponse'),
    newDeleteFlowLogsResponse,

    -- ** DescribeVpcClassicLink
    DescribeVpcClassicLink (DescribeVpcClassicLink'),
    newDescribeVpcClassicLink,
    DescribeVpcClassicLinkResponse (DescribeVpcClassicLinkResponse'),
    newDescribeVpcClassicLinkResponse,

    -- ** GetAssociatedEnclaveCertificateIamRoles
    GetAssociatedEnclaveCertificateIamRoles (GetAssociatedEnclaveCertificateIamRoles'),
    newGetAssociatedEnclaveCertificateIamRoles,
    GetAssociatedEnclaveCertificateIamRolesResponse (GetAssociatedEnclaveCertificateIamRolesResponse'),
    newGetAssociatedEnclaveCertificateIamRolesResponse,

    -- ** AssociateTransitGatewayMulticastDomain
    AssociateTransitGatewayMulticastDomain (AssociateTransitGatewayMulticastDomain'),
    newAssociateTransitGatewayMulticastDomain,
    AssociateTransitGatewayMulticastDomainResponse (AssociateTransitGatewayMulticastDomainResponse'),
    newAssociateTransitGatewayMulticastDomainResponse,

    -- ** ModifySubnetAttribute
    ModifySubnetAttribute (ModifySubnetAttribute'),
    newModifySubnetAttribute,
    ModifySubnetAttributeResponse (ModifySubnetAttributeResponse'),
    newModifySubnetAttributeResponse,

    -- ** DetachVolume
    DetachVolume (DetachVolume'),
    newDetachVolume,
    VolumeAttachment (VolumeAttachment'),
    newVolumeAttachment,

    -- ** DescribeInstanceCreditSpecifications (Paginated)
    DescribeInstanceCreditSpecifications (DescribeInstanceCreditSpecifications'),
    newDescribeInstanceCreditSpecifications,
    DescribeInstanceCreditSpecificationsResponse (DescribeInstanceCreditSpecificationsResponse'),
    newDescribeInstanceCreditSpecificationsResponse,

    -- ** CancelBundleTask
    CancelBundleTask (CancelBundleTask'),
    newCancelBundleTask,
    CancelBundleTaskResponse (CancelBundleTaskResponse'),
    newCancelBundleTaskResponse,

    -- ** DescribeByoipCidrs (Paginated)
    DescribeByoipCidrs (DescribeByoipCidrs'),
    newDescribeByoipCidrs,
    DescribeByoipCidrsResponse (DescribeByoipCidrsResponse'),
    newDescribeByoipCidrsResponse,

    -- ** AcceptReservedInstancesExchangeQuote
    AcceptReservedInstancesExchangeQuote (AcceptReservedInstancesExchangeQuote'),
    newAcceptReservedInstancesExchangeQuote,
    AcceptReservedInstancesExchangeQuoteResponse (AcceptReservedInstancesExchangeQuoteResponse'),
    newAcceptReservedInstancesExchangeQuoteResponse,

    -- ** ReleaseAddress
    ReleaseAddress (ReleaseAddress'),
    newReleaseAddress,
    ReleaseAddressResponse (ReleaseAddressResponse'),
    newReleaseAddressResponse,

    -- ** DescribeInstanceTypeOfferings (Paginated)
    DescribeInstanceTypeOfferings (DescribeInstanceTypeOfferings'),
    newDescribeInstanceTypeOfferings,
    DescribeInstanceTypeOfferingsResponse (DescribeInstanceTypeOfferingsResponse'),
    newDescribeInstanceTypeOfferingsResponse,

    -- ** CreateInternetGateway
    CreateInternetGateway (CreateInternetGateway'),
    newCreateInternetGateway,
    CreateInternetGatewayResponse (CreateInternetGatewayResponse'),
    newCreateInternetGatewayResponse,

    -- ** DeleteVpnConnection
    DeleteVpnConnection (DeleteVpnConnection'),
    newDeleteVpnConnection,
    DeleteVpnConnectionResponse (DeleteVpnConnectionResponse'),
    newDeleteVpnConnectionResponse,

    -- ** DescribeBundleTasks
    DescribeBundleTasks (DescribeBundleTasks'),
    newDescribeBundleTasks,
    DescribeBundleTasksResponse (DescribeBundleTasksResponse'),
    newDescribeBundleTasksResponse,

    -- ** AuthorizeSecurityGroupEgress
    AuthorizeSecurityGroupEgress (AuthorizeSecurityGroupEgress'),
    newAuthorizeSecurityGroupEgress,
    AuthorizeSecurityGroupEgressResponse (AuthorizeSecurityGroupEgressResponse'),
    newAuthorizeSecurityGroupEgressResponse,

    -- ** EnableTransitGatewayRouteTablePropagation
    EnableTransitGatewayRouteTablePropagation (EnableTransitGatewayRouteTablePropagation'),
    newEnableTransitGatewayRouteTablePropagation,
    EnableTransitGatewayRouteTablePropagationResponse (EnableTransitGatewayRouteTablePropagationResponse'),
    newEnableTransitGatewayRouteTablePropagationResponse,

    -- ** DeregisterImage
    DeregisterImage (DeregisterImage'),
    newDeregisterImage,
    DeregisterImageResponse (DeregisterImageResponse'),
    newDeregisterImageResponse,

    -- ** DeleteVpcEndpointConnectionNotifications
    DeleteVpcEndpointConnectionNotifications (DeleteVpcEndpointConnectionNotifications'),
    newDeleteVpcEndpointConnectionNotifications,
    DeleteVpcEndpointConnectionNotificationsResponse (DeleteVpcEndpointConnectionNotificationsResponse'),
    newDeleteVpcEndpointConnectionNotificationsResponse,

    -- ** DescribeCoipPools (Paginated)
    DescribeCoipPools (DescribeCoipPools'),
    newDescribeCoipPools,
    DescribeCoipPoolsResponse (DescribeCoipPoolsResponse'),
    newDescribeCoipPoolsResponse,

    -- ** ResetAddressAttribute
    ResetAddressAttribute (ResetAddressAttribute'),
    newResetAddressAttribute,
    ResetAddressAttributeResponse (ResetAddressAttributeResponse'),
    newResetAddressAttributeResponse,

    -- ** GetTransitGatewayMulticastDomainAssociations (Paginated)
    GetTransitGatewayMulticastDomainAssociations (GetTransitGatewayMulticastDomainAssociations'),
    newGetTransitGatewayMulticastDomainAssociations,
    GetTransitGatewayMulticastDomainAssociationsResponse (GetTransitGatewayMulticastDomainAssociationsResponse'),
    newGetTransitGatewayMulticastDomainAssociationsResponse,

    -- ** DeleteLocalGatewayRouteTableVpcAssociation
    DeleteLocalGatewayRouteTableVpcAssociation (DeleteLocalGatewayRouteTableVpcAssociation'),
    newDeleteLocalGatewayRouteTableVpcAssociation,
    DeleteLocalGatewayRouteTableVpcAssociationResponse (DeleteLocalGatewayRouteTableVpcAssociationResponse'),
    newDeleteLocalGatewayRouteTableVpcAssociationResponse,

    -- ** ModifyNetworkInterfaceAttribute
    ModifyNetworkInterfaceAttribute (ModifyNetworkInterfaceAttribute'),
    newModifyNetworkInterfaceAttribute,
    ModifyNetworkInterfaceAttributeResponse (ModifyNetworkInterfaceAttributeResponse'),
    newModifyNetworkInterfaceAttributeResponse,

    -- ** ModifyVpcTenancy
    ModifyVpcTenancy (ModifyVpcTenancy'),
    newModifyVpcTenancy,
    ModifyVpcTenancyResponse (ModifyVpcTenancyResponse'),
    newModifyVpcTenancyResponse,

    -- ** DescribeInstanceTypes (Paginated)
    DescribeInstanceTypes (DescribeInstanceTypes'),
    newDescribeInstanceTypes,
    DescribeInstanceTypesResponse (DescribeInstanceTypesResponse'),
    newDescribeInstanceTypesResponse,

    -- ** CancelCapacityReservationFleets
    CancelCapacityReservationFleets (CancelCapacityReservationFleets'),
    newCancelCapacityReservationFleets,
    CancelCapacityReservationFleetsResponse (CancelCapacityReservationFleetsResponse'),
    newCancelCapacityReservationFleetsResponse,

    -- ** DescribeClientVpnAuthorizationRules (Paginated)
    DescribeClientVpnAuthorizationRules (DescribeClientVpnAuthorizationRules'),
    newDescribeClientVpnAuthorizationRules,
    DescribeClientVpnAuthorizationRulesResponse (DescribeClientVpnAuthorizationRulesResponse'),
    newDescribeClientVpnAuthorizationRulesResponse,

    -- ** DeleteTransitGatewayVpcAttachment
    DeleteTransitGatewayVpcAttachment (DeleteTransitGatewayVpcAttachment'),
    newDeleteTransitGatewayVpcAttachment,
    DeleteTransitGatewayVpcAttachmentResponse (DeleteTransitGatewayVpcAttachmentResponse'),
    newDeleteTransitGatewayVpcAttachmentResponse,

    -- ** DeleteTransitGatewayMulticastDomain
    DeleteTransitGatewayMulticastDomain (DeleteTransitGatewayMulticastDomain'),
    newDeleteTransitGatewayMulticastDomain,
    DeleteTransitGatewayMulticastDomainResponse (DeleteTransitGatewayMulticastDomainResponse'),
    newDeleteTransitGatewayMulticastDomainResponse,

    -- ** CancelReservedInstancesListing
    CancelReservedInstancesListing (CancelReservedInstancesListing'),
    newCancelReservedInstancesListing,
    CancelReservedInstancesListingResponse (CancelReservedInstancesListingResponse'),
    newCancelReservedInstancesListingResponse,

    -- ** AttachClassicLinkVpc
    AttachClassicLinkVpc (AttachClassicLinkVpc'),
    newAttachClassicLinkVpc,
    AttachClassicLinkVpcResponse (AttachClassicLinkVpcResponse'),
    newAttachClassicLinkVpcResponse,

    -- ** DisableTransitGatewayRouteTablePropagation
    DisableTransitGatewayRouteTablePropagation (DisableTransitGatewayRouteTablePropagation'),
    newDisableTransitGatewayRouteTablePropagation,
    DisableTransitGatewayRouteTablePropagationResponse (DisableTransitGatewayRouteTablePropagationResponse'),
    newDisableTransitGatewayRouteTablePropagationResponse,

    -- ** DescribeVpcClassicLinkDnsSupport (Paginated)
    DescribeVpcClassicLinkDnsSupport (DescribeVpcClassicLinkDnsSupport'),
    newDescribeVpcClassicLinkDnsSupport,
    DescribeVpcClassicLinkDnsSupportResponse (DescribeVpcClassicLinkDnsSupportResponse'),
    newDescribeVpcClassicLinkDnsSupportResponse,

    -- ** AssociateSubnetCidrBlock
    AssociateSubnetCidrBlock (AssociateSubnetCidrBlock'),
    newAssociateSubnetCidrBlock,
    AssociateSubnetCidrBlockResponse (AssociateSubnetCidrBlockResponse'),
    newAssociateSubnetCidrBlockResponse,

    -- ** CreateNetworkInsightsPath
    CreateNetworkInsightsPath (CreateNetworkInsightsPath'),
    newCreateNetworkInsightsPath,
    CreateNetworkInsightsPathResponse (CreateNetworkInsightsPathResponse'),
    newCreateNetworkInsightsPathResponse,

    -- ** RunScheduledInstances
    RunScheduledInstances (RunScheduledInstances'),
    newRunScheduledInstances,
    RunScheduledInstancesResponse (RunScheduledInstancesResponse'),
    newRunScheduledInstancesResponse,

    -- ** CreateTransitGatewayRoute
    CreateTransitGatewayRoute (CreateTransitGatewayRoute'),
    newCreateTransitGatewayRoute,
    CreateTransitGatewayRouteResponse (CreateTransitGatewayRouteResponse'),
    newCreateTransitGatewayRouteResponse,

    -- ** CreateTransitGatewayPrefixListReference
    CreateTransitGatewayPrefixListReference (CreateTransitGatewayPrefixListReference'),
    newCreateTransitGatewayPrefixListReference,
    CreateTransitGatewayPrefixListReferenceResponse (CreateTransitGatewayPrefixListReferenceResponse'),
    newCreateTransitGatewayPrefixListReferenceResponse,

    -- ** CancelSpotFleetRequests
    CancelSpotFleetRequests (CancelSpotFleetRequests'),
    newCancelSpotFleetRequests,
    CancelSpotFleetRequestsResponse (CancelSpotFleetRequestsResponse'),
    newCancelSpotFleetRequestsResponse,

    -- ** ModifyCapacityReservationFleet
    ModifyCapacityReservationFleet (ModifyCapacityReservationFleet'),
    newModifyCapacityReservationFleet,
    ModifyCapacityReservationFleetResponse (ModifyCapacityReservationFleetResponse'),
    newModifyCapacityReservationFleetResponse,

    -- ** DescribeSpotPriceHistory (Paginated)
    DescribeSpotPriceHistory (DescribeSpotPriceHistory'),
    newDescribeSpotPriceHistory,
    DescribeSpotPriceHistoryResponse (DescribeSpotPriceHistoryResponse'),
    newDescribeSpotPriceHistoryResponse,

    -- ** DeleteTransitGatewayConnectPeer
    DeleteTransitGatewayConnectPeer (DeleteTransitGatewayConnectPeer'),
    newDeleteTransitGatewayConnectPeer,
    DeleteTransitGatewayConnectPeerResponse (DeleteTransitGatewayConnectPeerResponse'),
    newDeleteTransitGatewayConnectPeerResponse,

    -- ** DescribeDhcpOptions (Paginated)
    DescribeDhcpOptions (DescribeDhcpOptions'),
    newDescribeDhcpOptions,
    DescribeDhcpOptionsResponse (DescribeDhcpOptionsResponse'),
    newDescribeDhcpOptionsResponse,

    -- ** ImportImage
    ImportImage (ImportImage'),
    newImportImage,
    ImportImageResponse (ImportImageResponse'),
    newImportImageResponse,

    -- ** CreateLocalGatewayRouteTableVpcAssociation
    CreateLocalGatewayRouteTableVpcAssociation (CreateLocalGatewayRouteTableVpcAssociation'),
    newCreateLocalGatewayRouteTableVpcAssociation,
    CreateLocalGatewayRouteTableVpcAssociationResponse (CreateLocalGatewayRouteTableVpcAssociationResponse'),
    newCreateLocalGatewayRouteTableVpcAssociationResponse,

    -- ** CopyFpgaImage
    CopyFpgaImage (CopyFpgaImage'),
    newCopyFpgaImage,
    CopyFpgaImageResponse (CopyFpgaImageResponse'),
    newCopyFpgaImageResponse,

    -- ** ImportClientVpnClientCertificateRevocationList
    ImportClientVpnClientCertificateRevocationList (ImportClientVpnClientCertificateRevocationList'),
    newImportClientVpnClientCertificateRevocationList,
    ImportClientVpnClientCertificateRevocationListResponse (ImportClientVpnClientCertificateRevocationListResponse'),
    newImportClientVpnClientCertificateRevocationListResponse,

    -- ** StopInstances
    StopInstances (StopInstances'),
    newStopInstances,
    StopInstancesResponse (StopInstancesResponse'),
    newStopInstancesResponse,

    -- ** EnableEbsEncryptionByDefault
    EnableEbsEncryptionByDefault (EnableEbsEncryptionByDefault'),
    newEnableEbsEncryptionByDefault,
    EnableEbsEncryptionByDefaultResponse (EnableEbsEncryptionByDefaultResponse'),
    newEnableEbsEncryptionByDefaultResponse,

    -- ** ModifyAddressAttribute
    ModifyAddressAttribute (ModifyAddressAttribute'),
    newModifyAddressAttribute,
    ModifyAddressAttributeResponse (ModifyAddressAttributeResponse'),
    newModifyAddressAttributeResponse,

    -- ** DeregisterTransitGatewayMulticastGroupSources
    DeregisterTransitGatewayMulticastGroupSources (DeregisterTransitGatewayMulticastGroupSources'),
    newDeregisterTransitGatewayMulticastGroupSources,
    DeregisterTransitGatewayMulticastGroupSourcesResponse (DeregisterTransitGatewayMulticastGroupSourcesResponse'),
    newDeregisterTransitGatewayMulticastGroupSourcesResponse,

    -- ** ModifyLaunchTemplate
    ModifyLaunchTemplate (ModifyLaunchTemplate'),
    newModifyLaunchTemplate,
    ModifyLaunchTemplateResponse (ModifyLaunchTemplateResponse'),
    newModifyLaunchTemplateResponse,

    -- ** ModifyVpcEndpointConnectionNotification
    ModifyVpcEndpointConnectionNotification (ModifyVpcEndpointConnectionNotification'),
    newModifyVpcEndpointConnectionNotification,
    ModifyVpcEndpointConnectionNotificationResponse (ModifyVpcEndpointConnectionNotificationResponse'),
    newModifyVpcEndpointConnectionNotificationResponse,

    -- ** DescribeInternetGateways (Paginated)
    DescribeInternetGateways (DescribeInternetGateways'),
    newDescribeInternetGateways,
    DescribeInternetGatewaysResponse (DescribeInternetGatewaysResponse'),
    newDescribeInternetGatewaysResponse,

    -- ** DisableVpcClassicLink
    DisableVpcClassicLink (DisableVpcClassicLink'),
    newDisableVpcClassicLink,
    DisableVpcClassicLinkResponse (DisableVpcClassicLinkResponse'),
    newDisableVpcClassicLinkResponse,

    -- ** GetGroupsForCapacityReservation (Paginated)
    GetGroupsForCapacityReservation (GetGroupsForCapacityReservation'),
    newGetGroupsForCapacityReservation,
    GetGroupsForCapacityReservationResponse (GetGroupsForCapacityReservationResponse'),
    newGetGroupsForCapacityReservationResponse,

    -- ** DeleteLaunchTemplateVersions
    DeleteLaunchTemplateVersions (DeleteLaunchTemplateVersions'),
    newDeleteLaunchTemplateVersions,
    DeleteLaunchTemplateVersionsResponse (DeleteLaunchTemplateVersionsResponse'),
    newDeleteLaunchTemplateVersionsResponse,

    -- ** BundleInstance
    BundleInstance (BundleInstance'),
    newBundleInstance,
    BundleInstanceResponse (BundleInstanceResponse'),
    newBundleInstanceResponse,

    -- ** DescribeNetworkInterfaces (Paginated)
    DescribeNetworkInterfaces (DescribeNetworkInterfaces'),
    newDescribeNetworkInterfaces,
    DescribeNetworkInterfacesResponse (DescribeNetworkInterfacesResponse'),
    newDescribeNetworkInterfacesResponse,

    -- ** ReplaceNetworkAclAssociation
    ReplaceNetworkAclAssociation (ReplaceNetworkAclAssociation'),
    newReplaceNetworkAclAssociation,
    ReplaceNetworkAclAssociationResponse (ReplaceNetworkAclAssociationResponse'),
    newReplaceNetworkAclAssociationResponse,

    -- ** AssociateInstanceEventWindow
    AssociateInstanceEventWindow (AssociateInstanceEventWindow'),
    newAssociateInstanceEventWindow,
    AssociateInstanceEventWindowResponse (AssociateInstanceEventWindowResponse'),
    newAssociateInstanceEventWindowResponse,

    -- ** DescribeNatGateways (Paginated)
    DescribeNatGateways (DescribeNatGateways'),
    newDescribeNatGateways,
    DescribeNatGatewaysResponse (DescribeNatGatewaysResponse'),
    newDescribeNatGatewaysResponse,

    -- ** DescribeAddresses
    DescribeAddresses (DescribeAddresses'),
    newDescribeAddresses,
    DescribeAddressesResponse (DescribeAddressesResponse'),
    newDescribeAddressesResponse,

    -- ** RestoreManagedPrefixListVersion
    RestoreManagedPrefixListVersion (RestoreManagedPrefixListVersion'),
    newRestoreManagedPrefixListVersion,
    RestoreManagedPrefixListVersionResponse (RestoreManagedPrefixListVersionResponse'),
    newRestoreManagedPrefixListVersionResponse,

    -- ** DescribeSnapshotAttribute
    DescribeSnapshotAttribute (DescribeSnapshotAttribute'),
    newDescribeSnapshotAttribute,
    DescribeSnapshotAttributeResponse (DescribeSnapshotAttributeResponse'),
    newDescribeSnapshotAttributeResponse,

    -- ** DescribeIdentityIdFormat
    DescribeIdentityIdFormat (DescribeIdentityIdFormat'),
    newDescribeIdentityIdFormat,
    DescribeIdentityIdFormatResponse (DescribeIdentityIdFormatResponse'),
    newDescribeIdentityIdFormatResponse,

    -- ** ReplaceRoute
    ReplaceRoute (ReplaceRoute'),
    newReplaceRoute,
    ReplaceRouteResponse (ReplaceRouteResponse'),
    newReplaceRouteResponse,

    -- ** DescribeVpcEndpointServices (Paginated)
    DescribeVpcEndpointServices (DescribeVpcEndpointServices'),
    newDescribeVpcEndpointServices,
    DescribeVpcEndpointServicesResponse (DescribeVpcEndpointServicesResponse'),
    newDescribeVpcEndpointServicesResponse,

    -- ** DeleteLocalGatewayRoute
    DeleteLocalGatewayRoute (DeleteLocalGatewayRoute'),
    newDeleteLocalGatewayRoute,
    DeleteLocalGatewayRouteResponse (DeleteLocalGatewayRouteResponse'),
    newDeleteLocalGatewayRouteResponse,

    -- ** AuthorizeSecurityGroupIngress
    AuthorizeSecurityGroupIngress (AuthorizeSecurityGroupIngress'),
    newAuthorizeSecurityGroupIngress,
    AuthorizeSecurityGroupIngressResponse (AuthorizeSecurityGroupIngressResponse'),
    newAuthorizeSecurityGroupIngressResponse,

    -- ** CreateVpcPeeringConnection
    CreateVpcPeeringConnection (CreateVpcPeeringConnection'),
    newCreateVpcPeeringConnection,
    CreateVpcPeeringConnectionResponse (CreateVpcPeeringConnectionResponse'),
    newCreateVpcPeeringConnectionResponse,

    -- ** DescribeSubnets (Paginated)
    DescribeSubnets (DescribeSubnets'),
    newDescribeSubnets,
    DescribeSubnetsResponse (DescribeSubnetsResponse'),
    newDescribeSubnetsResponse,

    -- ** GetTransitGatewayAttachmentPropagations (Paginated)
    GetTransitGatewayAttachmentPropagations (GetTransitGatewayAttachmentPropagations'),
    newGetTransitGatewayAttachmentPropagations,
    GetTransitGatewayAttachmentPropagationsResponse (GetTransitGatewayAttachmentPropagationsResponse'),
    newGetTransitGatewayAttachmentPropagationsResponse,

    -- ** CreateTags
    CreateTags (CreateTags'),
    newCreateTags,
    CreateTagsResponse (CreateTagsResponse'),
    newCreateTagsResponse,

    -- ** PurchaseReservedInstancesOffering
    PurchaseReservedInstancesOffering (PurchaseReservedInstancesOffering'),
    newPurchaseReservedInstancesOffering,
    PurchaseReservedInstancesOfferingResponse (PurchaseReservedInstancesOfferingResponse'),
    newPurchaseReservedInstancesOfferingResponse,

    -- ** DeleteNetworkAclEntry
    DeleteNetworkAclEntry (DeleteNetworkAclEntry'),
    newDeleteNetworkAclEntry,
    DeleteNetworkAclEntryResponse (DeleteNetworkAclEntryResponse'),
    newDeleteNetworkAclEntryResponse,

    -- ** ResetSnapshotAttribute
    ResetSnapshotAttribute (ResetSnapshotAttribute'),
    newResetSnapshotAttribute,
    ResetSnapshotAttributeResponse (ResetSnapshotAttributeResponse'),
    newResetSnapshotAttributeResponse,

    -- ** DescribeVpnConnections
    DescribeVpnConnections (DescribeVpnConnections'),
    newDescribeVpnConnections,
    DescribeVpnConnectionsResponse (DescribeVpnConnectionsResponse'),
    newDescribeVpnConnectionsResponse,

    -- ** ModifyInstanceEventStartTime
    ModifyInstanceEventStartTime (ModifyInstanceEventStartTime'),
    newModifyInstanceEventStartTime,
    ModifyInstanceEventStartTimeResponse (ModifyInstanceEventStartTimeResponse'),
    newModifyInstanceEventStartTimeResponse,

    -- ** DeleteRoute
    DeleteRoute (DeleteRoute'),
    newDeleteRoute,
    DeleteRouteResponse (DeleteRouteResponse'),
    newDeleteRouteResponse,

    -- ** ReplaceNetworkAclEntry
    ReplaceNetworkAclEntry (ReplaceNetworkAclEntry'),
    newReplaceNetworkAclEntry,
    ReplaceNetworkAclEntryResponse (ReplaceNetworkAclEntryResponse'),
    newReplaceNetworkAclEntryResponse,

    -- ** DeleteInstanceEventWindow
    DeleteInstanceEventWindow (DeleteInstanceEventWindow'),
    newDeleteInstanceEventWindow,
    DeleteInstanceEventWindowResponse (DeleteInstanceEventWindowResponse'),
    newDeleteInstanceEventWindowResponse,

    -- ** DescribeVpcEndpoints (Paginated)
    DescribeVpcEndpoints (DescribeVpcEndpoints'),
    newDescribeVpcEndpoints,
    DescribeVpcEndpointsResponse (DescribeVpcEndpointsResponse'),
    newDescribeVpcEndpointsResponse,

    -- ** CreateTrafficMirrorFilter
    CreateTrafficMirrorFilter (CreateTrafficMirrorFilter'),
    newCreateTrafficMirrorFilter,
    CreateTrafficMirrorFilterResponse (CreateTrafficMirrorFilterResponse'),
    newCreateTrafficMirrorFilterResponse,

    -- ** ResetInstanceAttribute
    ResetInstanceAttribute (ResetInstanceAttribute'),
    newResetInstanceAttribute,
    ResetInstanceAttributeResponse (ResetInstanceAttributeResponse'),
    newResetInstanceAttributeResponse,

    -- ** ModifyIdentityIdFormat
    ModifyIdentityIdFormat (ModifyIdentityIdFormat'),
    newModifyIdentityIdFormat,
    ModifyIdentityIdFormatResponse (ModifyIdentityIdFormatResponse'),
    newModifyIdentityIdFormatResponse,

    -- ** AttachNetworkInterface
    AttachNetworkInterface (AttachNetworkInterface'),
    newAttachNetworkInterface,
    AttachNetworkInterfaceResponse (AttachNetworkInterfaceResponse'),
    newAttachNetworkInterfaceResponse,

    -- ** CreateCapacityReservation
    CreateCapacityReservation (CreateCapacityReservation'),
    newCreateCapacityReservation,
    CreateCapacityReservationResponse (CreateCapacityReservationResponse'),
    newCreateCapacityReservationResponse,

    -- ** DescribeInstanceStatus (Paginated)
    DescribeInstanceStatus (DescribeInstanceStatus'),
    newDescribeInstanceStatus,
    DescribeInstanceStatusResponse (DescribeInstanceStatusResponse'),
    newDescribeInstanceStatusResponse,

    -- ** ImportKeyPair
    ImportKeyPair (ImportKeyPair'),
    newImportKeyPair,
    ImportKeyPairResponse (ImportKeyPairResponse'),
    newImportKeyPairResponse,

    -- ** DeleteTags
    DeleteTags (DeleteTags'),
    newDeleteTags,
    DeleteTagsResponse (DeleteTagsResponse'),
    newDeleteTagsResponse,

    -- ** ConfirmProductInstance
    ConfirmProductInstance (ConfirmProductInstance'),
    newConfirmProductInstance,
    ConfirmProductInstanceResponse (ConfirmProductInstanceResponse'),
    newConfirmProductInstanceResponse,

    -- ** DescribeInstanceAttribute
    DescribeInstanceAttribute (DescribeInstanceAttribute'),
    newDescribeInstanceAttribute,
    DescribeInstanceAttributeResponse (DescribeInstanceAttributeResponse'),
    newDescribeInstanceAttributeResponse,

    -- ** DescribeReservedInstancesOfferings (Paginated)
    DescribeReservedInstancesOfferings (DescribeReservedInstancesOfferings'),
    newDescribeReservedInstancesOfferings,
    DescribeReservedInstancesOfferingsResponse (DescribeReservedInstancesOfferingsResponse'),
    newDescribeReservedInstancesOfferingsResponse,

    -- ** CreateCustomerGateway
    CreateCustomerGateway (CreateCustomerGateway'),
    newCreateCustomerGateway,
    CreateCustomerGatewayResponse (CreateCustomerGatewayResponse'),
    newCreateCustomerGatewayResponse,

    -- ** DescribeNetworkInsightsAnalyses (Paginated)
    DescribeNetworkInsightsAnalyses (DescribeNetworkInsightsAnalyses'),
    newDescribeNetworkInsightsAnalyses,
    DescribeNetworkInsightsAnalysesResponse (DescribeNetworkInsightsAnalysesResponse'),
    newDescribeNetworkInsightsAnalysesResponse,

    -- ** DescribeFleets (Paginated)
    DescribeFleets (DescribeFleets'),
    newDescribeFleets,
    DescribeFleetsResponse (DescribeFleetsResponse'),
    newDescribeFleetsResponse,

    -- ** DeleteNetworkInsightsAnalysis
    DeleteNetworkInsightsAnalysis (DeleteNetworkInsightsAnalysis'),
    newDeleteNetworkInsightsAnalysis,
    DeleteNetworkInsightsAnalysisResponse (DeleteNetworkInsightsAnalysisResponse'),
    newDeleteNetworkInsightsAnalysisResponse,

    -- ** CreateTransitGatewayPeeringAttachment
    CreateTransitGatewayPeeringAttachment (CreateTransitGatewayPeeringAttachment'),
    newCreateTransitGatewayPeeringAttachment,
    CreateTransitGatewayPeeringAttachmentResponse (CreateTransitGatewayPeeringAttachmentResponse'),
    newCreateTransitGatewayPeeringAttachmentResponse,

    -- ** DeleteSecurityGroup
    DeleteSecurityGroup (DeleteSecurityGroup'),
    newDeleteSecurityGroup,
    DeleteSecurityGroupResponse (DeleteSecurityGroupResponse'),
    newDeleteSecurityGroupResponse,

    -- ** DescribePublicIpv4Pools (Paginated)
    DescribePublicIpv4Pools (DescribePublicIpv4Pools'),
    newDescribePublicIpv4Pools,
    DescribePublicIpv4PoolsResponse (DescribePublicIpv4PoolsResponse'),
    newDescribePublicIpv4PoolsResponse,

    -- ** DescribeClientVpnTargetNetworks (Paginated)
    DescribeClientVpnTargetNetworks (DescribeClientVpnTargetNetworks'),
    newDescribeClientVpnTargetNetworks,
    DescribeClientVpnTargetNetworksResponse (DescribeClientVpnTargetNetworksResponse'),
    newDescribeClientVpnTargetNetworksResponse,

    -- ** DeleteVpcPeeringConnection
    DeleteVpcPeeringConnection (DeleteVpcPeeringConnection'),
    newDeleteVpcPeeringConnection,
    DeleteVpcPeeringConnectionResponse (DeleteVpcPeeringConnectionResponse'),
    newDeleteVpcPeeringConnectionResponse,

    -- ** AttachInternetGateway
    AttachInternetGateway (AttachInternetGateway'),
    newAttachInternetGateway,
    AttachInternetGatewayResponse (AttachInternetGatewayResponse'),
    newAttachInternetGatewayResponse,

    -- ** ModifyInstancePlacement
    ModifyInstancePlacement (ModifyInstancePlacement'),
    newModifyInstancePlacement,
    ModifyInstancePlacementResponse (ModifyInstancePlacementResponse'),
    newModifyInstancePlacementResponse,

    -- ** DescribeFlowLogs (Paginated)
    DescribeFlowLogs (DescribeFlowLogs'),
    newDescribeFlowLogs,
    DescribeFlowLogsResponse (DescribeFlowLogsResponse'),
    newDescribeFlowLogsResponse,

    -- ** DescribeLocalGatewayVirtualInterfaceGroups (Paginated)
    DescribeLocalGatewayVirtualInterfaceGroups (DescribeLocalGatewayVirtualInterfaceGroups'),
    newDescribeLocalGatewayVirtualInterfaceGroups,
    DescribeLocalGatewayVirtualInterfaceGroupsResponse (DescribeLocalGatewayVirtualInterfaceGroupsResponse'),
    newDescribeLocalGatewayVirtualInterfaceGroupsResponse,

    -- ** DeleteTransitGatewayConnect
    DeleteTransitGatewayConnect (DeleteTransitGatewayConnect'),
    newDeleteTransitGatewayConnect,
    DeleteTransitGatewayConnectResponse (DeleteTransitGatewayConnectResponse'),
    newDeleteTransitGatewayConnectResponse,

    -- ** DescribeLocalGatewayRouteTableVpcAssociations (Paginated)
    DescribeLocalGatewayRouteTableVpcAssociations (DescribeLocalGatewayRouteTableVpcAssociations'),
    newDescribeLocalGatewayRouteTableVpcAssociations,
    DescribeLocalGatewayRouteTableVpcAssociationsResponse (DescribeLocalGatewayRouteTableVpcAssociationsResponse'),
    newDescribeLocalGatewayRouteTableVpcAssociationsResponse,

    -- ** DescribeVpcEndpointConnectionNotifications (Paginated)
    DescribeVpcEndpointConnectionNotifications (DescribeVpcEndpointConnectionNotifications'),
    newDescribeVpcEndpointConnectionNotifications,
    DescribeVpcEndpointConnectionNotificationsResponse (DescribeVpcEndpointConnectionNotificationsResponse'),
    newDescribeVpcEndpointConnectionNotificationsResponse,

    -- ** GetManagedPrefixListEntries (Paginated)
    GetManagedPrefixListEntries (GetManagedPrefixListEntries'),
    newGetManagedPrefixListEntries,
    GetManagedPrefixListEntriesResponse (GetManagedPrefixListEntriesResponse'),
    newGetManagedPrefixListEntriesResponse,

    -- ** DisassociateInstanceEventWindow
    DisassociateInstanceEventWindow (DisassociateInstanceEventWindow'),
    newDisassociateInstanceEventWindow,
    DisassociateInstanceEventWindowResponse (DisassociateInstanceEventWindowResponse'),
    newDisassociateInstanceEventWindowResponse,

    -- ** RunInstances
    RunInstances (RunInstances'),
    newRunInstances,
    Reservation (Reservation'),
    newReservation,

    -- ** CreateSnapshots
    CreateSnapshots (CreateSnapshots'),
    newCreateSnapshots,
    CreateSnapshotsResponse (CreateSnapshotsResponse'),
    newCreateSnapshotsResponse,

    -- ** AssociateDhcpOptions
    AssociateDhcpOptions (AssociateDhcpOptions'),
    newAssociateDhcpOptions,
    AssociateDhcpOptionsResponse (AssociateDhcpOptionsResponse'),
    newAssociateDhcpOptionsResponse,

    -- ** DeleteTrafficMirrorFilterRule
    DeleteTrafficMirrorFilterRule (DeleteTrafficMirrorFilterRule'),
    newDeleteTrafficMirrorFilterRule,
    DeleteTrafficMirrorFilterRuleResponse (DeleteTrafficMirrorFilterRuleResponse'),
    newDeleteTrafficMirrorFilterRuleResponse,

    -- ** DescribeReservedInstances
    DescribeReservedInstances (DescribeReservedInstances'),
    newDescribeReservedInstances,
    DescribeReservedInstancesResponse (DescribeReservedInstancesResponse'),
    newDescribeReservedInstancesResponse,

    -- ** DescribeIdFormat
    DescribeIdFormat (DescribeIdFormat'),
    newDescribeIdFormat,
    DescribeIdFormatResponse (DescribeIdFormatResponse'),
    newDescribeIdFormatResponse,

    -- ** DescribeVpcs (Paginated)
    DescribeVpcs (DescribeVpcs'),
    newDescribeVpcs,
    DescribeVpcsResponse (DescribeVpcsResponse'),
    newDescribeVpcsResponse,

    -- ** DescribeConversionTasks
    DescribeConversionTasks (DescribeConversionTasks'),
    newDescribeConversionTasks,
    DescribeConversionTasksResponse (DescribeConversionTasksResponse'),
    newDescribeConversionTasksResponse,

    -- ** DisableImageDeprecation
    DisableImageDeprecation (DisableImageDeprecation'),
    newDisableImageDeprecation,
    DisableImageDeprecationResponse (DisableImageDeprecationResponse'),
    newDisableImageDeprecationResponse,

    -- ** CreateLaunchTemplateVersion
    CreateLaunchTemplateVersion (CreateLaunchTemplateVersion'),
    newCreateLaunchTemplateVersion,
    CreateLaunchTemplateVersionResponse (CreateLaunchTemplateVersionResponse'),
    newCreateLaunchTemplateVersionResponse,

    -- ** GetManagedPrefixListAssociations (Paginated)
    GetManagedPrefixListAssociations (GetManagedPrefixListAssociations'),
    newGetManagedPrefixListAssociations,
    GetManagedPrefixListAssociationsResponse (GetManagedPrefixListAssociationsResponse'),
    newGetManagedPrefixListAssociationsResponse,

    -- ** DisableVpcClassicLinkDnsSupport
    DisableVpcClassicLinkDnsSupport (DisableVpcClassicLinkDnsSupport'),
    newDisableVpcClassicLinkDnsSupport,
    DisableVpcClassicLinkDnsSupportResponse (DisableVpcClassicLinkDnsSupportResponse'),
    newDisableVpcClassicLinkDnsSupportResponse,

    -- ** ApplySecurityGroupsToClientVpnTargetNetwork
    ApplySecurityGroupsToClientVpnTargetNetwork (ApplySecurityGroupsToClientVpnTargetNetwork'),
    newApplySecurityGroupsToClientVpnTargetNetwork,
    ApplySecurityGroupsToClientVpnTargetNetworkResponse (ApplySecurityGroupsToClientVpnTargetNetworkResponse'),
    newApplySecurityGroupsToClientVpnTargetNetworkResponse,

    -- ** DescribeTrafficMirrorTargets (Paginated)
    DescribeTrafficMirrorTargets (DescribeTrafficMirrorTargets'),
    newDescribeTrafficMirrorTargets,
    DescribeTrafficMirrorTargetsResponse (DescribeTrafficMirrorTargetsResponse'),
    newDescribeTrafficMirrorTargetsResponse,

    -- ** DescribeVolumesModifications (Paginated)
    DescribeVolumesModifications (DescribeVolumesModifications'),
    newDescribeVolumesModifications,
    DescribeVolumesModificationsResponse (DescribeVolumesModificationsResponse'),
    newDescribeVolumesModificationsResponse,

    -- ** ExportImage
    ExportImage (ExportImage'),
    newExportImage,
    ExportImageResponse (ExportImageResponse'),
    newExportImageResponse,

    -- ** CreateFpgaImage
    CreateFpgaImage (CreateFpgaImage'),
    newCreateFpgaImage,
    CreateFpgaImageResponse (CreateFpgaImageResponse'),
    newCreateFpgaImageResponse,

    -- ** AcceptVpcEndpointConnections
    AcceptVpcEndpointConnections (AcceptVpcEndpointConnections'),
    newAcceptVpcEndpointConnections,
    AcceptVpcEndpointConnectionsResponse (AcceptVpcEndpointConnectionsResponse'),
    newAcceptVpcEndpointConnectionsResponse,

    -- ** DeleteClientVpnEndpoint
    DeleteClientVpnEndpoint (DeleteClientVpnEndpoint'),
    newDeleteClientVpnEndpoint,
    DeleteClientVpnEndpointResponse (DeleteClientVpnEndpointResponse'),
    newDeleteClientVpnEndpointResponse,

    -- ** SearchTransitGatewayRoutes
    SearchTransitGatewayRoutes (SearchTransitGatewayRoutes'),
    newSearchTransitGatewayRoutes,
    SearchTransitGatewayRoutesResponse (SearchTransitGatewayRoutesResponse'),
    newSearchTransitGatewayRoutesResponse,

    -- ** GetLaunchTemplateData
    GetLaunchTemplateData (GetLaunchTemplateData'),
    newGetLaunchTemplateData,
    GetLaunchTemplateDataResponse (GetLaunchTemplateDataResponse'),
    newGetLaunchTemplateDataResponse,

    -- ** AllocateAddress
    AllocateAddress (AllocateAddress'),
    newAllocateAddress,
    AllocateAddressResponse (AllocateAddressResponse'),
    newAllocateAddressResponse,

    -- ** AcceptTransitGatewayVpcAttachment
    AcceptTransitGatewayVpcAttachment (AcceptTransitGatewayVpcAttachment'),
    newAcceptTransitGatewayVpcAttachment,
    AcceptTransitGatewayVpcAttachmentResponse (AcceptTransitGatewayVpcAttachmentResponse'),
    newAcceptTransitGatewayVpcAttachmentResponse,

    -- ** CancelConversionTask
    CancelConversionTask (CancelConversionTask'),
    newCancelConversionTask,
    CancelConversionTaskResponse (CancelConversionTaskResponse'),
    newCancelConversionTaskResponse,

    -- ** ModifyImageAttribute
    ModifyImageAttribute (ModifyImageAttribute'),
    newModifyImageAttribute,
    ModifyImageAttributeResponse (ModifyImageAttributeResponse'),
    newModifyImageAttributeResponse,

    -- ** CreateRouteTable
    CreateRouteTable (CreateRouteTable'),
    newCreateRouteTable,
    CreateRouteTableResponse (CreateRouteTableResponse'),
    newCreateRouteTableResponse,

    -- ** RejectTransitGatewayPeeringAttachment
    RejectTransitGatewayPeeringAttachment (RejectTransitGatewayPeeringAttachment'),
    newRejectTransitGatewayPeeringAttachment,
    RejectTransitGatewayPeeringAttachmentResponse (RejectTransitGatewayPeeringAttachmentResponse'),
    newRejectTransitGatewayPeeringAttachmentResponse,

    -- ** ReportInstanceStatus
    ReportInstanceStatus (ReportInstanceStatus'),
    newReportInstanceStatus,
    ReportInstanceStatusResponse (ReportInstanceStatusResponse'),
    newReportInstanceStatusResponse,

    -- ** AttachVolume
    AttachVolume (AttachVolume'),
    newAttachVolume,
    VolumeAttachment (VolumeAttachment'),
    newVolumeAttachment,

    -- ** RequestSpotInstances
    RequestSpotInstances (RequestSpotInstances'),
    newRequestSpotInstances,
    RequestSpotInstancesResponse (RequestSpotInstancesResponse'),
    newRequestSpotInstancesResponse,

    -- ** WithdrawByoipCidr
    WithdrawByoipCidr (WithdrawByoipCidr'),
    newWithdrawByoipCidr,
    WithdrawByoipCidrResponse (WithdrawByoipCidrResponse'),
    newWithdrawByoipCidrResponse,

    -- ** DescribeHostReservationOfferings (Paginated)
    DescribeHostReservationOfferings (DescribeHostReservationOfferings'),
    newDescribeHostReservationOfferings,
    DescribeHostReservationOfferingsResponse (DescribeHostReservationOfferingsResponse'),
    newDescribeHostReservationOfferingsResponse,

    -- ** ResetFpgaImageAttribute
    ResetFpgaImageAttribute (ResetFpgaImageAttribute'),
    newResetFpgaImageAttribute,
    ResetFpgaImageAttributeResponse (ResetFpgaImageAttributeResponse'),
    newResetFpgaImageAttributeResponse,

    -- ** ModifyVpnConnection
    ModifyVpnConnection (ModifyVpnConnection'),
    newModifyVpnConnection,
    ModifyVpnConnectionResponse (ModifyVpnConnectionResponse'),
    newModifyVpnConnectionResponse,

    -- ** CreateTrafficMirrorFilterRule
    CreateTrafficMirrorFilterRule (CreateTrafficMirrorFilterRule'),
    newCreateTrafficMirrorFilterRule,
    CreateTrafficMirrorFilterRuleResponse (CreateTrafficMirrorFilterRuleResponse'),
    newCreateTrafficMirrorFilterRuleResponse,

    -- ** DeleteTransitGateway
    DeleteTransitGateway (DeleteTransitGateway'),
    newDeleteTransitGateway,
    DeleteTransitGatewayResponse (DeleteTransitGatewayResponse'),
    newDeleteTransitGatewayResponse,

    -- ** StartVpcEndpointServicePrivateDnsVerification
    StartVpcEndpointServicePrivateDnsVerification (StartVpcEndpointServicePrivateDnsVerification'),
    newStartVpcEndpointServicePrivateDnsVerification,
    StartVpcEndpointServicePrivateDnsVerificationResponse (StartVpcEndpointServicePrivateDnsVerificationResponse'),
    newStartVpcEndpointServicePrivateDnsVerificationResponse,

    -- ** DescribeVolumes (Paginated)
    DescribeVolumes (DescribeVolumes'),
    newDescribeVolumes,
    DescribeVolumesResponse (DescribeVolumesResponse'),
    newDescribeVolumesResponse,

    -- ** RejectVpcPeeringConnection
    RejectVpcPeeringConnection (RejectVpcPeeringConnection'),
    newRejectVpcPeeringConnection,
    RejectVpcPeeringConnectionResponse (RejectVpcPeeringConnectionResponse'),
    newRejectVpcPeeringConnectionResponse,

    -- ** DescribeClientVpnRoutes (Paginated)
    DescribeClientVpnRoutes (DescribeClientVpnRoutes'),
    newDescribeClientVpnRoutes,
    DescribeClientVpnRoutesResponse (DescribeClientVpnRoutesResponse'),
    newDescribeClientVpnRoutesResponse,

    -- ** DeleteVpnConnectionRoute
    DeleteVpnConnectionRoute (DeleteVpnConnectionRoute'),
    newDeleteVpnConnectionRoute,
    DeleteVpnConnectionRouteResponse (DeleteVpnConnectionRouteResponse'),
    newDeleteVpnConnectionRouteResponse,

    -- ** AssociateEnclaveCertificateIamRole
    AssociateEnclaveCertificateIamRole (AssociateEnclaveCertificateIamRole'),
    newAssociateEnclaveCertificateIamRole,
    AssociateEnclaveCertificateIamRoleResponse (AssociateEnclaveCertificateIamRoleResponse'),
    newAssociateEnclaveCertificateIamRoleResponse,

    -- ** ModifyVpcEndpoint
    ModifyVpcEndpoint (ModifyVpcEndpoint'),
    newModifyVpcEndpoint,
    ModifyVpcEndpointResponse (ModifyVpcEndpointResponse'),
    newModifyVpcEndpointResponse,

    -- ** DescribeFpgaImageAttribute
    DescribeFpgaImageAttribute (DescribeFpgaImageAttribute'),
    newDescribeFpgaImageAttribute,
    DescribeFpgaImageAttributeResponse (DescribeFpgaImageAttributeResponse'),
    newDescribeFpgaImageAttributeResponse,

    -- ** AllocateHosts
    AllocateHosts (AllocateHosts'),
    newAllocateHosts,
    AllocateHostsResponse (AllocateHostsResponse'),
    newAllocateHostsResponse,

    -- ** CreateClientVpnEndpoint
    CreateClientVpnEndpoint (CreateClientVpnEndpoint'),
    newCreateClientVpnEndpoint,
    CreateClientVpnEndpointResponse (CreateClientVpnEndpointResponse'),
    newCreateClientVpnEndpointResponse,

    -- ** CreateTrafficMirrorSession
    CreateTrafficMirrorSession (CreateTrafficMirrorSession'),
    newCreateTrafficMirrorSession,
    CreateTrafficMirrorSessionResponse (CreateTrafficMirrorSessionResponse'),
    newCreateTrafficMirrorSessionResponse,

    -- ** RegisterImage
    RegisterImage (RegisterImage'),
    newRegisterImage,
    RegisterImageResponse (RegisterImageResponse'),
    newRegisterImageResponse,

    -- ** AdvertiseByoipCidr
    AdvertiseByoipCidr (AdvertiseByoipCidr'),
    newAdvertiseByoipCidr,
    AdvertiseByoipCidrResponse (AdvertiseByoipCidrResponse'),
    newAdvertiseByoipCidrResponse,

    -- ** ModifyFleet
    ModifyFleet (ModifyFleet'),
    newModifyFleet,
    ModifyFleetResponse (ModifyFleetResponse'),
    newModifyFleetResponse,

    -- ** RevokeSecurityGroupIngress
    RevokeSecurityGroupIngress (RevokeSecurityGroupIngress'),
    newRevokeSecurityGroupIngress,
    RevokeSecurityGroupIngressResponse (RevokeSecurityGroupIngressResponse'),
    newRevokeSecurityGroupIngressResponse,

    -- ** GetEbsDefaultKmsKeyId
    GetEbsDefaultKmsKeyId (GetEbsDefaultKmsKeyId'),
    newGetEbsDefaultKmsKeyId,
    GetEbsDefaultKmsKeyIdResponse (GetEbsDefaultKmsKeyIdResponse'),
    newGetEbsDefaultKmsKeyIdResponse,

    -- ** DescribeHostReservations (Paginated)
    DescribeHostReservations (DescribeHostReservations'),
    newDescribeHostReservations,
    DescribeHostReservationsResponse (DescribeHostReservationsResponse'),
    newDescribeHostReservationsResponse,

    -- ** UpdateSecurityGroupRuleDescriptionsEgress
    UpdateSecurityGroupRuleDescriptionsEgress (UpdateSecurityGroupRuleDescriptionsEgress'),
    newUpdateSecurityGroupRuleDescriptionsEgress,
    UpdateSecurityGroupRuleDescriptionsEgressResponse (UpdateSecurityGroupRuleDescriptionsEgressResponse'),
    newUpdateSecurityGroupRuleDescriptionsEgressResponse,

    -- ** EnableVpcClassicLinkDnsSupport
    EnableVpcClassicLinkDnsSupport (EnableVpcClassicLinkDnsSupport'),
    newEnableVpcClassicLinkDnsSupport,
    EnableVpcClassicLinkDnsSupportResponse (EnableVpcClassicLinkDnsSupportResponse'),
    newEnableVpcClassicLinkDnsSupportResponse,

    -- ** DescribeVpcEndpointConnections (Paginated)
    DescribeVpcEndpointConnections (DescribeVpcEndpointConnections'),
    newDescribeVpcEndpointConnections,
    DescribeVpcEndpointConnectionsResponse (DescribeVpcEndpointConnectionsResponse'),
    newDescribeVpcEndpointConnectionsResponse,

    -- ** ModifyReservedInstances
    ModifyReservedInstances (ModifyReservedInstances'),
    newModifyReservedInstances,
    ModifyReservedInstancesResponse (ModifyReservedInstancesResponse'),
    newModifyReservedInstancesResponse,

    -- ** DeleteFpgaImage
    DeleteFpgaImage (DeleteFpgaImage'),
    newDeleteFpgaImage,
    DeleteFpgaImageResponse (DeleteFpgaImageResponse'),
    newDeleteFpgaImageResponse,

    -- ** DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations (Paginated)
    DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations (DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations'),
    newDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations,
    DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse (DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse'),
    newDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse,

    -- ** EnableImageDeprecation
    EnableImageDeprecation (EnableImageDeprecation'),
    newEnableImageDeprecation,
    EnableImageDeprecationResponse (EnableImageDeprecationResponse'),
    newEnableImageDeprecationResponse,

    -- ** DescribeScheduledInstances (Paginated)
    DescribeScheduledInstances (DescribeScheduledInstances'),
    newDescribeScheduledInstances,
    DescribeScheduledInstancesResponse (DescribeScheduledInstancesResponse'),
    newDescribeScheduledInstancesResponse,

    -- ** SearchTransitGatewayMulticastGroups (Paginated)
    SearchTransitGatewayMulticastGroups (SearchTransitGatewayMulticastGroups'),
    newSearchTransitGatewayMulticastGroups,
    SearchTransitGatewayMulticastGroupsResponse (SearchTransitGatewayMulticastGroupsResponse'),
    newSearchTransitGatewayMulticastGroupsResponse,

    -- ** CreateFlowLogs
    CreateFlowLogs (CreateFlowLogs'),
    newCreateFlowLogs,
    CreateFlowLogsResponse (CreateFlowLogsResponse'),
    newCreateFlowLogsResponse,

    -- ** DescribeSpotFleetRequests (Paginated)
    DescribeSpotFleetRequests (DescribeSpotFleetRequests'),
    newDescribeSpotFleetRequests,
    DescribeSpotFleetRequestsResponse (DescribeSpotFleetRequestsResponse'),
    newDescribeSpotFleetRequestsResponse,

    -- ** MoveAddressToVpc
    MoveAddressToVpc (MoveAddressToVpc'),
    newMoveAddressToVpc,
    MoveAddressToVpcResponse (MoveAddressToVpcResponse'),
    newMoveAddressToVpcResponse,

    -- ** DescribeFleetInstances
    DescribeFleetInstances (DescribeFleetInstances'),
    newDescribeFleetInstances,
    DescribeFleetInstancesResponse (DescribeFleetInstancesResponse'),
    newDescribeFleetInstancesResponse,

    -- ** DescribeLaunchTemplateVersions (Paginated)
    DescribeLaunchTemplateVersions (DescribeLaunchTemplateVersions'),
    newDescribeLaunchTemplateVersions,
    DescribeLaunchTemplateVersionsResponse (DescribeLaunchTemplateVersionsResponse'),
    newDescribeLaunchTemplateVersionsResponse,

    -- ** StartNetworkInsightsAnalysis
    StartNetworkInsightsAnalysis (StartNetworkInsightsAnalysis'),
    newStartNetworkInsightsAnalysis,
    StartNetworkInsightsAnalysisResponse (StartNetworkInsightsAnalysisResponse'),
    newStartNetworkInsightsAnalysisResponse,

    -- ** ModifyInstanceCreditSpecification
    ModifyInstanceCreditSpecification (ModifyInstanceCreditSpecification'),
    newModifyInstanceCreditSpecification,
    ModifyInstanceCreditSpecificationResponse (ModifyInstanceCreditSpecificationResponse'),
    newModifyInstanceCreditSpecificationResponse,

    -- ** DescribePrincipalIdFormat (Paginated)
    DescribePrincipalIdFormat (DescribePrincipalIdFormat'),
    newDescribePrincipalIdFormat,
    DescribePrincipalIdFormatResponse (DescribePrincipalIdFormatResponse'),
    newDescribePrincipalIdFormatResponse,

    -- ** DescribeTransitGateways (Paginated)
    DescribeTransitGateways (DescribeTransitGateways'),
    newDescribeTransitGateways,
    DescribeTransitGatewaysResponse (DescribeTransitGatewaysResponse'),
    newDescribeTransitGatewaysResponse,

    -- ** DeleteNetworkAcl
    DeleteNetworkAcl (DeleteNetworkAcl'),
    newDeleteNetworkAcl,
    DeleteNetworkAclResponse (DeleteNetworkAclResponse'),
    newDeleteNetworkAclResponse,

    -- ** DisassociateTransitGatewayMulticastDomain
    DisassociateTransitGatewayMulticastDomain (DisassociateTransitGatewayMulticastDomain'),
    newDisassociateTransitGatewayMulticastDomain,
    DisassociateTransitGatewayMulticastDomainResponse (DisassociateTransitGatewayMulticastDomainResponse'),
    newDisassociateTransitGatewayMulticastDomainResponse,

    -- ** DeleteTransitGatewayRouteTable
    DeleteTransitGatewayRouteTable (DeleteTransitGatewayRouteTable'),
    newDeleteTransitGatewayRouteTable,
    DeleteTransitGatewayRouteTableResponse (DeleteTransitGatewayRouteTableResponse'),
    newDeleteTransitGatewayRouteTableResponse,

    -- ** DescribeSecurityGroupRules (Paginated)
    DescribeSecurityGroupRules (DescribeSecurityGroupRules'),
    newDescribeSecurityGroupRules,
    DescribeSecurityGroupRulesResponse (DescribeSecurityGroupRulesResponse'),
    newDescribeSecurityGroupRulesResponse,

    -- ** CreateLaunchTemplate
    CreateLaunchTemplate (CreateLaunchTemplate'),
    newCreateLaunchTemplate,
    CreateLaunchTemplateResponse (CreateLaunchTemplateResponse'),
    newCreateLaunchTemplateResponse,

    -- ** CreateVpcEndpointConnectionNotification
    CreateVpcEndpointConnectionNotification (CreateVpcEndpointConnectionNotification'),
    newCreateVpcEndpointConnectionNotification,
    CreateVpcEndpointConnectionNotificationResponse (CreateVpcEndpointConnectionNotificationResponse'),
    newCreateVpcEndpointConnectionNotificationResponse,

    -- ** DeleteNetworkInterfacePermission
    DeleteNetworkInterfacePermission (DeleteNetworkInterfacePermission'),
    newDeleteNetworkInterfacePermission,
    DeleteNetworkInterfacePermissionResponse (DeleteNetworkInterfacePermissionResponse'),
    newDeleteNetworkInterfacePermissionResponse,

    -- ** DeleteVpnGateway
    DeleteVpnGateway (DeleteVpnGateway'),
    newDeleteVpnGateway,
    DeleteVpnGatewayResponse (DeleteVpnGatewayResponse'),
    newDeleteVpnGatewayResponse,

    -- ** CreateStoreImageTask
    CreateStoreImageTask (CreateStoreImageTask'),
    newCreateStoreImageTask,
    CreateStoreImageTaskResponse (CreateStoreImageTaskResponse'),
    newCreateStoreImageTaskResponse,

    -- ** CreateTrafficMirrorTarget
    CreateTrafficMirrorTarget (CreateTrafficMirrorTarget'),
    newCreateTrafficMirrorTarget,
    CreateTrafficMirrorTargetResponse (CreateTrafficMirrorTargetResponse'),
    newCreateTrafficMirrorTargetResponse,

    -- ** DescribeImportImageTasks (Paginated)
    DescribeImportImageTasks (DescribeImportImageTasks'),
    newDescribeImportImageTasks,
    DescribeImportImageTasksResponse (DescribeImportImageTasksResponse'),
    newDescribeImportImageTasksResponse,

    -- ** DescribeVolumeAttribute
    DescribeVolumeAttribute (DescribeVolumeAttribute'),
    newDescribeVolumeAttribute,
    DescribeVolumeAttributeResponse (DescribeVolumeAttributeResponse'),
    newDescribeVolumeAttributeResponse,

    -- ** DescribeMovingAddresses (Paginated)
    DescribeMovingAddresses (DescribeMovingAddresses'),
    newDescribeMovingAddresses,
    DescribeMovingAddressesResponse (DescribeMovingAddressesResponse'),
    newDescribeMovingAddressesResponse,

    -- ** ExportTransitGatewayRoutes
    ExportTransitGatewayRoutes (ExportTransitGatewayRoutes'),
    newExportTransitGatewayRoutes,
    ExportTransitGatewayRoutesResponse (ExportTransitGatewayRoutesResponse'),
    newExportTransitGatewayRoutesResponse,

    -- ** GetPasswordData
    GetPasswordData (GetPasswordData'),
    newGetPasswordData,
    GetPasswordDataResponse (GetPasswordDataResponse'),
    newGetPasswordDataResponse,

    -- ** CreateVpc
    CreateVpc (CreateVpc'),
    newCreateVpc,
    CreateVpcResponse (CreateVpcResponse'),
    newCreateVpcResponse,

    -- ** ModifyVpcPeeringConnectionOptions
    ModifyVpcPeeringConnectionOptions (ModifyVpcPeeringConnectionOptions'),
    newModifyVpcPeeringConnectionOptions,
    ModifyVpcPeeringConnectionOptionsResponse (ModifyVpcPeeringConnectionOptionsResponse'),
    newModifyVpcPeeringConnectionOptionsResponse,

    -- ** DescribeFpgaImages (Paginated)
    DescribeFpgaImages (DescribeFpgaImages'),
    newDescribeFpgaImages,
    DescribeFpgaImagesResponse (DescribeFpgaImagesResponse'),
    newDescribeFpgaImagesResponse,

    -- ** CopySnapshot
    CopySnapshot (CopySnapshot'),
    newCopySnapshot,
    CopySnapshotResponse (CopySnapshotResponse'),
    newCopySnapshotResponse,

    -- ** AcceptTransitGatewayPeeringAttachment
    AcceptTransitGatewayPeeringAttachment (AcceptTransitGatewayPeeringAttachment'),
    newAcceptTransitGatewayPeeringAttachment,
    AcceptTransitGatewayPeeringAttachmentResponse (AcceptTransitGatewayPeeringAttachmentResponse'),
    newAcceptTransitGatewayPeeringAttachmentResponse,

    -- ** DisassociateAddress
    DisassociateAddress (DisassociateAddress'),
    newDisassociateAddress,
    DisassociateAddressResponse (DisassociateAddressResponse'),
    newDisassociateAddressResponse,

    -- ** ModifyTrafficMirrorFilterNetworkServices
    ModifyTrafficMirrorFilterNetworkServices (ModifyTrafficMirrorFilterNetworkServices'),
    newModifyTrafficMirrorFilterNetworkServices,
    ModifyTrafficMirrorFilterNetworkServicesResponse (ModifyTrafficMirrorFilterNetworkServicesResponse'),
    newModifyTrafficMirrorFilterNetworkServicesResponse,

    -- ** DescribeEgressOnlyInternetGateways (Paginated)
    DescribeEgressOnlyInternetGateways (DescribeEgressOnlyInternetGateways'),
    newDescribeEgressOnlyInternetGateways,
    DescribeEgressOnlyInternetGatewaysResponse (DescribeEgressOnlyInternetGatewaysResponse'),
    newDescribeEgressOnlyInternetGatewaysResponse,

    -- ** DeleteVpc
    DeleteVpc (DeleteVpc'),
    newDeleteVpc,
    DeleteVpcResponse (DeleteVpcResponse'),
    newDeleteVpcResponse,

    -- ** CreateInstanceExportTask
    CreateInstanceExportTask (CreateInstanceExportTask'),
    newCreateInstanceExportTask,
    CreateInstanceExportTaskResponse (CreateInstanceExportTaskResponse'),
    newCreateInstanceExportTaskResponse,

    -- ** RejectTransitGatewayVpcAttachment
    RejectTransitGatewayVpcAttachment (RejectTransitGatewayVpcAttachment'),
    newRejectTransitGatewayVpcAttachment,
    RejectTransitGatewayVpcAttachmentResponse (RejectTransitGatewayVpcAttachmentResponse'),
    newRejectTransitGatewayVpcAttachmentResponse,

    -- ** DescribeTrafficMirrorSessions (Paginated)
    DescribeTrafficMirrorSessions (DescribeTrafficMirrorSessions'),
    newDescribeTrafficMirrorSessions,
    DescribeTrafficMirrorSessionsResponse (DescribeTrafficMirrorSessionsResponse'),
    newDescribeTrafficMirrorSessionsResponse,

    -- ** GetTransitGatewayRouteTableAssociations (Paginated)
    GetTransitGatewayRouteTableAssociations (GetTransitGatewayRouteTableAssociations'),
    newGetTransitGatewayRouteTableAssociations,
    GetTransitGatewayRouteTableAssociationsResponse (GetTransitGatewayRouteTableAssociationsResponse'),
    newGetTransitGatewayRouteTableAssociationsResponse,

    -- ** AssociateVpcCidrBlock
    AssociateVpcCidrBlock (AssociateVpcCidrBlock'),
    newAssociateVpcCidrBlock,
    AssociateVpcCidrBlockResponse (AssociateVpcCidrBlockResponse'),
    newAssociateVpcCidrBlockResponse,

    -- ** DescribeVpcAttribute
    DescribeVpcAttribute (DescribeVpcAttribute'),
    newDescribeVpcAttribute,
    DescribeVpcAttributeResponse (DescribeVpcAttributeResponse'),
    newDescribeVpcAttributeResponse,

    -- ** CreateVolume
    CreateVolume (CreateVolume'),
    newCreateVolume,
    Volume (Volume'),
    newVolume,

    -- ** CreateDefaultSubnet
    CreateDefaultSubnet (CreateDefaultSubnet'),
    newCreateDefaultSubnet,
    CreateDefaultSubnetResponse (CreateDefaultSubnetResponse'),
    newCreateDefaultSubnetResponse,

    -- ** DescribeScheduledInstanceAvailability (Paginated)
    DescribeScheduledInstanceAvailability (DescribeScheduledInstanceAvailability'),
    newDescribeScheduledInstanceAvailability,
    DescribeScheduledInstanceAvailabilityResponse (DescribeScheduledInstanceAvailabilityResponse'),
    newDescribeScheduledInstanceAvailabilityResponse,

    -- ** DisassociateClientVpnTargetNetwork
    DisassociateClientVpnTargetNetwork (DisassociateClientVpnTargetNetwork'),
    newDisassociateClientVpnTargetNetwork,
    DisassociateClientVpnTargetNetworkResponse (DisassociateClientVpnTargetNetworkResponse'),
    newDisassociateClientVpnTargetNetworkResponse,

    -- ** CreateClientVpnRoute
    CreateClientVpnRoute (CreateClientVpnRoute'),
    newCreateClientVpnRoute,
    CreateClientVpnRouteResponse (CreateClientVpnRouteResponse'),
    newCreateClientVpnRouteResponse,

    -- ** ModifyVolumeAttribute
    ModifyVolumeAttribute (ModifyVolumeAttribute'),
    newModifyVolumeAttribute,
    ModifyVolumeAttributeResponse (ModifyVolumeAttributeResponse'),
    newModifyVolumeAttributeResponse,

    -- ** ExportClientVpnClientConfiguration
    ExportClientVpnClientConfiguration (ExportClientVpnClientConfiguration'),
    newExportClientVpnClientConfiguration,
    ExportClientVpnClientConfigurationResponse (ExportClientVpnClientConfigurationResponse'),
    newExportClientVpnClientConfigurationResponse,

    -- ** DescribeTrunkInterfaceAssociations (Paginated)
    DescribeTrunkInterfaceAssociations (DescribeTrunkInterfaceAssociations'),
    newDescribeTrunkInterfaceAssociations,
    DescribeTrunkInterfaceAssociationsResponse (DescribeTrunkInterfaceAssociationsResponse'),
    newDescribeTrunkInterfaceAssociationsResponse,

    -- ** DeleteTrafficMirrorTarget
    DeleteTrafficMirrorTarget (DeleteTrafficMirrorTarget'),
    newDeleteTrafficMirrorTarget,
    DeleteTrafficMirrorTargetResponse (DeleteTrafficMirrorTargetResponse'),
    newDeleteTrafficMirrorTargetResponse,

    -- ** DescribeSpotDatafeedSubscription
    DescribeSpotDatafeedSubscription (DescribeSpotDatafeedSubscription'),
    newDescribeSpotDatafeedSubscription,
    DescribeSpotDatafeedSubscriptionResponse (DescribeSpotDatafeedSubscriptionResponse'),
    newDescribeSpotDatafeedSubscriptionResponse,

    -- ** DescribeLocalGatewayRouteTables (Paginated)
    DescribeLocalGatewayRouteTables (DescribeLocalGatewayRouteTables'),
    newDescribeLocalGatewayRouteTables,
    DescribeLocalGatewayRouteTablesResponse (DescribeLocalGatewayRouteTablesResponse'),
    newDescribeLocalGatewayRouteTablesResponse,

    -- ** DescribePrefixLists (Paginated)
    DescribePrefixLists (DescribePrefixLists'),
    newDescribePrefixLists,
    DescribePrefixListsResponse (DescribePrefixListsResponse'),
    newDescribePrefixListsResponse,

    -- ** AssociateTransitGatewayRouteTable
    AssociateTransitGatewayRouteTable (AssociateTransitGatewayRouteTable'),
    newAssociateTransitGatewayRouteTable,
    AssociateTransitGatewayRouteTableResponse (AssociateTransitGatewayRouteTableResponse'),
    newAssociateTransitGatewayRouteTableResponse,

    -- ** DeletePlacementGroup
    DeletePlacementGroup (DeletePlacementGroup'),
    newDeletePlacementGroup,
    DeletePlacementGroupResponse (DeletePlacementGroupResponse'),
    newDeletePlacementGroupResponse,

    -- ** ModifyTransitGateway
    ModifyTransitGateway (ModifyTransitGateway'),
    newModifyTransitGateway,
    ModifyTransitGatewayResponse (ModifyTransitGatewayResponse'),
    newModifyTransitGatewayResponse,

    -- ** DeleteTransitGatewayPrefixListReference
    DeleteTransitGatewayPrefixListReference (DeleteTransitGatewayPrefixListReference'),
    newDeleteTransitGatewayPrefixListReference,
    DeleteTransitGatewayPrefixListReferenceResponse (DeleteTransitGatewayPrefixListReferenceResponse'),
    newDeleteTransitGatewayPrefixListReferenceResponse,

    -- ** CreateTransitGatewayMulticastDomain
    CreateTransitGatewayMulticastDomain (CreateTransitGatewayMulticastDomain'),
    newCreateTransitGatewayMulticastDomain,
    CreateTransitGatewayMulticastDomainResponse (CreateTransitGatewayMulticastDomainResponse'),
    newCreateTransitGatewayMulticastDomainResponse,

    -- ** DeregisterInstanceEventNotificationAttributes
    DeregisterInstanceEventNotificationAttributes (DeregisterInstanceEventNotificationAttributes'),
    newDeregisterInstanceEventNotificationAttributes,
    DeregisterInstanceEventNotificationAttributesResponse (DeregisterInstanceEventNotificationAttributesResponse'),
    newDeregisterInstanceEventNotificationAttributesResponse,

    -- ** RequestSpotFleet
    RequestSpotFleet (RequestSpotFleet'),
    newRequestSpotFleet,
    RequestSpotFleetResponse (RequestSpotFleetResponse'),
    newRequestSpotFleetResponse,

    -- ** DeleteNetworkInsightsPath
    DeleteNetworkInsightsPath (DeleteNetworkInsightsPath'),
    newDeleteNetworkInsightsPath,
    DeleteNetworkInsightsPathResponse (DeleteNetworkInsightsPathResponse'),
    newDeleteNetworkInsightsPathResponse,

    -- ** DescribeTransitGatewayConnects (Paginated)
    DescribeTransitGatewayConnects (DescribeTransitGatewayConnects'),
    newDescribeTransitGatewayConnects,
    DescribeTransitGatewayConnectsResponse (DescribeTransitGatewayConnectsResponse'),
    newDescribeTransitGatewayConnectsResponse,

    -- ** DeleteTransitGatewayRoute
    DeleteTransitGatewayRoute (DeleteTransitGatewayRoute'),
    newDeleteTransitGatewayRoute,
    DeleteTransitGatewayRouteResponse (DeleteTransitGatewayRouteResponse'),
    newDeleteTransitGatewayRouteResponse,

    -- ** CreateTransitGatewayConnectPeer
    CreateTransitGatewayConnectPeer (CreateTransitGatewayConnectPeer'),
    newCreateTransitGatewayConnectPeer,
    CreateTransitGatewayConnectPeerResponse (CreateTransitGatewayConnectPeerResponse'),
    newCreateTransitGatewayConnectPeerResponse,

    -- ** DisableEbsEncryptionByDefault
    DisableEbsEncryptionByDefault (DisableEbsEncryptionByDefault'),
    newDisableEbsEncryptionByDefault,
    DisableEbsEncryptionByDefaultResponse (DisableEbsEncryptionByDefaultResponse'),
    newDisableEbsEncryptionByDefaultResponse,

    -- ** DeregisterTransitGatewayMulticastGroupMembers
    DeregisterTransitGatewayMulticastGroupMembers (DeregisterTransitGatewayMulticastGroupMembers'),
    newDeregisterTransitGatewayMulticastGroupMembers,
    DeregisterTransitGatewayMulticastGroupMembersResponse (DeregisterTransitGatewayMulticastGroupMembersResponse'),
    newDeregisterTransitGatewayMulticastGroupMembersResponse,

    -- ** AssociateTrunkInterface
    AssociateTrunkInterface (AssociateTrunkInterface'),
    newAssociateTrunkInterface,
    AssociateTrunkInterfaceResponse (AssociateTrunkInterfaceResponse'),
    newAssociateTrunkInterfaceResponse,

    -- ** CreateSubnet
    CreateSubnet (CreateSubnet'),
    newCreateSubnet,
    CreateSubnetResponse (CreateSubnetResponse'),
    newCreateSubnetResponse,

    -- ** CreateNetworkInterface
    CreateNetworkInterface (CreateNetworkInterface'),
    newCreateNetworkInterface,
    CreateNetworkInterfaceResponse (CreateNetworkInterfaceResponse'),
    newCreateNetworkInterfaceResponse,

    -- ** DescribeSecurityGroups (Paginated)
    DescribeSecurityGroups (DescribeSecurityGroups'),
    newDescribeSecurityGroups,
    DescribeSecurityGroupsResponse (DescribeSecurityGroupsResponse'),
    newDescribeSecurityGroupsResponse,

    -- ** GetCapacityReservationUsage
    GetCapacityReservationUsage (GetCapacityReservationUsage'),
    newGetCapacityReservationUsage,
    GetCapacityReservationUsageResponse (GetCapacityReservationUsageResponse'),
    newGetCapacityReservationUsageResponse,

    -- ** CreateTransitGatewayVpcAttachment
    CreateTransitGatewayVpcAttachment (CreateTransitGatewayVpcAttachment'),
    newCreateTransitGatewayVpcAttachment,
    CreateTransitGatewayVpcAttachmentResponse (CreateTransitGatewayVpcAttachmentResponse'),
    newCreateTransitGatewayVpcAttachmentResponse,

    -- ** DescribeExportTasks
    DescribeExportTasks (DescribeExportTasks'),
    newDescribeExportTasks,
    DescribeExportTasksResponse (DescribeExportTasksResponse'),
    newDescribeExportTasksResponse,

    -- ** ModifySpotFleetRequest
    ModifySpotFleetRequest (ModifySpotFleetRequest'),
    newModifySpotFleetRequest,
    ModifySpotFleetRequestResponse (ModifySpotFleetRequestResponse'),
    newModifySpotFleetRequestResponse,

    -- ** DetachVpnGateway
    DetachVpnGateway (DetachVpnGateway'),
    newDetachVpnGateway,
    DetachVpnGatewayResponse (DetachVpnGatewayResponse'),
    newDetachVpnGatewayResponse,

    -- ** ModifyManagedPrefixList
    ModifyManagedPrefixList (ModifyManagedPrefixList'),
    newModifyManagedPrefixList,
    ModifyManagedPrefixListResponse (ModifyManagedPrefixListResponse'),
    newModifyManagedPrefixListResponse,

    -- ** GetHostReservationPurchasePreview
    GetHostReservationPurchasePreview (GetHostReservationPurchasePreview'),
    newGetHostReservationPurchasePreview,
    GetHostReservationPurchasePreviewResponse (GetHostReservationPurchasePreviewResponse'),
    newGetHostReservationPurchasePreviewResponse,

    -- ** EnableVolumeIO
    EnableVolumeIO (EnableVolumeIO'),
    newEnableVolumeIO,
    EnableVolumeIOResponse (EnableVolumeIOResponse'),
    newEnableVolumeIOResponse,

    -- ** DescribeInstances (Paginated)
    DescribeInstances (DescribeInstances'),
    newDescribeInstances,
    DescribeInstancesResponse (DescribeInstancesResponse'),
    newDescribeInstancesResponse,

    -- ** DescribeInstanceEventWindows (Paginated)
    DescribeInstanceEventWindows (DescribeInstanceEventWindows'),
    newDescribeInstanceEventWindows,
    DescribeInstanceEventWindowsResponse (DescribeInstanceEventWindowsResponse'),
    newDescribeInstanceEventWindowsResponse,

    -- ** DisableSerialConsoleAccess
    DisableSerialConsoleAccess (DisableSerialConsoleAccess'),
    newDisableSerialConsoleAccess,
    DisableSerialConsoleAccessResponse (DisableSerialConsoleAccessResponse'),
    newDisableSerialConsoleAccessResponse,

    -- ** CreateNatGateway
    CreateNatGateway (CreateNatGateway'),
    newCreateNatGateway,
    CreateNatGatewayResponse (CreateNatGatewayResponse'),
    newCreateNatGatewayResponse,

    -- ** DescribeLocalGatewayVirtualInterfaces (Paginated)
    DescribeLocalGatewayVirtualInterfaces (DescribeLocalGatewayVirtualInterfaces'),
    newDescribeLocalGatewayVirtualInterfaces,
    DescribeLocalGatewayVirtualInterfacesResponse (DescribeLocalGatewayVirtualInterfacesResponse'),
    newDescribeLocalGatewayVirtualInterfacesResponse,

    -- ** DescribeVpcPeeringConnections (Paginated)
    DescribeVpcPeeringConnections (DescribeVpcPeeringConnections'),
    newDescribeVpcPeeringConnections,
    DescribeVpcPeeringConnectionsResponse (DescribeVpcPeeringConnectionsResponse'),
    newDescribeVpcPeeringConnectionsResponse,

    -- ** CancelExportTask
    CancelExportTask (CancelExportTask'),
    newCancelExportTask,
    CancelExportTaskResponse (CancelExportTaskResponse'),
    newCancelExportTaskResponse,

    -- ** CreateVpcEndpointServiceConfiguration
    CreateVpcEndpointServiceConfiguration (CreateVpcEndpointServiceConfiguration'),
    newCreateVpcEndpointServiceConfiguration,
    CreateVpcEndpointServiceConfigurationResponse (CreateVpcEndpointServiceConfigurationResponse'),
    newCreateVpcEndpointServiceConfigurationResponse,

    -- ** CreateDefaultVpc
    CreateDefaultVpc (CreateDefaultVpc'),
    newCreateDefaultVpc,
    CreateDefaultVpcResponse (CreateDefaultVpcResponse'),
    newCreateDefaultVpcResponse,

    -- ** DisassociateVpcCidrBlock
    DisassociateVpcCidrBlock (DisassociateVpcCidrBlock'),
    newDisassociateVpcCidrBlock,
    DisassociateVpcCidrBlockResponse (DisassociateVpcCidrBlockResponse'),
    newDisassociateVpcCidrBlockResponse,

    -- ** DescribeTrafficMirrorFilters (Paginated)
    DescribeTrafficMirrorFilters (DescribeTrafficMirrorFilters'),
    newDescribeTrafficMirrorFilters,
    DescribeTrafficMirrorFiltersResponse (DescribeTrafficMirrorFiltersResponse'),
    newDescribeTrafficMirrorFiltersResponse,

    -- ** DescribeFastSnapshotRestores (Paginated)
    DescribeFastSnapshotRestores (DescribeFastSnapshotRestores'),
    newDescribeFastSnapshotRestores,
    DescribeFastSnapshotRestoresResponse (DescribeFastSnapshotRestoresResponse'),
    newDescribeFastSnapshotRestoresResponse,

    -- ** CancelCapacityReservation
    CancelCapacityReservation (CancelCapacityReservation'),
    newCancelCapacityReservation,
    CancelCapacityReservationResponse (CancelCapacityReservationResponse'),
    newCancelCapacityReservationResponse,

    -- ** DeleteNetworkInterface
    DeleteNetworkInterface (DeleteNetworkInterface'),
    newDeleteNetworkInterface,
    DeleteNetworkInterfaceResponse (DeleteNetworkInterfaceResponse'),
    newDeleteNetworkInterfaceResponse,

    -- ** DisassociateTransitGatewayRouteTable
    DisassociateTransitGatewayRouteTable (DisassociateTransitGatewayRouteTable'),
    newDisassociateTransitGatewayRouteTable,
    DisassociateTransitGatewayRouteTableResponse (DisassociateTransitGatewayRouteTableResponse'),
    newDisassociateTransitGatewayRouteTableResponse,

    -- ** ReplaceRouteTableAssociation
    ReplaceRouteTableAssociation (ReplaceRouteTableAssociation'),
    newReplaceRouteTableAssociation,
    ReplaceRouteTableAssociationResponse (ReplaceRouteTableAssociationResponse'),
    newReplaceRouteTableAssociationResponse,

    -- ** StartInstances
    StartInstances (StartInstances'),
    newStartInstances,
    StartInstancesResponse (StartInstancesResponse'),
    newStartInstancesResponse,

    -- ** CreatePlacementGroup
    CreatePlacementGroup (CreatePlacementGroup'),
    newCreatePlacementGroup,
    CreatePlacementGroupResponse (CreatePlacementGroupResponse'),
    newCreatePlacementGroupResponse,

    -- ** DescribeInstanceEventNotificationAttributes
    DescribeInstanceEventNotificationAttributes (DescribeInstanceEventNotificationAttributes'),
    newDescribeInstanceEventNotificationAttributes,
    DescribeInstanceEventNotificationAttributesResponse (DescribeInstanceEventNotificationAttributesResponse'),
    newDescribeInstanceEventNotificationAttributesResponse,

    -- ** DescribeCapacityReservations (Paginated)
    DescribeCapacityReservations (DescribeCapacityReservations'),
    newDescribeCapacityReservations,
    DescribeCapacityReservationsResponse (DescribeCapacityReservationsResponse'),
    newDescribeCapacityReservationsResponse,

    -- ** ModifyClientVpnEndpoint
    ModifyClientVpnEndpoint (ModifyClientVpnEndpoint'),
    newModifyClientVpnEndpoint,
    ModifyClientVpnEndpointResponse (ModifyClientVpnEndpointResponse'),
    newModifyClientVpnEndpointResponse,

    -- ** ModifyInstanceCapacityReservationAttributes
    ModifyInstanceCapacityReservationAttributes (ModifyInstanceCapacityReservationAttributes'),
    newModifyInstanceCapacityReservationAttributes,
    ModifyInstanceCapacityReservationAttributesResponse (ModifyInstanceCapacityReservationAttributesResponse'),
    newModifyInstanceCapacityReservationAttributesResponse,

    -- ** DescribeAggregateIdFormat
    DescribeAggregateIdFormat (DescribeAggregateIdFormat'),
    newDescribeAggregateIdFormat,
    DescribeAggregateIdFormatResponse (DescribeAggregateIdFormatResponse'),
    newDescribeAggregateIdFormatResponse,

    -- ** DescribeSnapshots (Paginated)
    DescribeSnapshots (DescribeSnapshots'),
    newDescribeSnapshots,
    DescribeSnapshotsResponse (DescribeSnapshotsResponse'),
    newDescribeSnapshotsResponse,

    -- ** GetSubnetCidrReservations
    GetSubnetCidrReservations (GetSubnetCidrReservations'),
    newGetSubnetCidrReservations,
    GetSubnetCidrReservationsResponse (GetSubnetCidrReservationsResponse'),
    newGetSubnetCidrReservationsResponse,

    -- ** AssociateAddress
    AssociateAddress (AssociateAddress'),
    newAssociateAddress,
    AssociateAddressResponse (AssociateAddressResponse'),
    newAssociateAddressResponse,

    -- ** ModifyTrafficMirrorFilterRule
    ModifyTrafficMirrorFilterRule (ModifyTrafficMirrorFilterRule'),
    newModifyTrafficMirrorFilterRule,
    ModifyTrafficMirrorFilterRuleResponse (ModifyTrafficMirrorFilterRuleResponse'),
    newModifyTrafficMirrorFilterRuleResponse,

    -- ** DescribeNetworkInterfaceAttribute
    DescribeNetworkInterfaceAttribute (DescribeNetworkInterfaceAttribute'),
    newDescribeNetworkInterfaceAttribute,
    DescribeNetworkInterfaceAttributeResponse (DescribeNetworkInterfaceAttributeResponse'),
    newDescribeNetworkInterfaceAttributeResponse,

    -- ** ReplaceIamInstanceProfileAssociation
    ReplaceIamInstanceProfileAssociation (ReplaceIamInstanceProfileAssociation'),
    newReplaceIamInstanceProfileAssociation,
    ReplaceIamInstanceProfileAssociationResponse (ReplaceIamInstanceProfileAssociationResponse'),
    newReplaceIamInstanceProfileAssociationResponse,

    -- ** AssociateClientVpnTargetNetwork
    AssociateClientVpnTargetNetwork (AssociateClientVpnTargetNetwork'),
    newAssociateClientVpnTargetNetwork,
    AssociateClientVpnTargetNetworkResponse (AssociateClientVpnTargetNetworkResponse'),
    newAssociateClientVpnTargetNetworkResponse,

    -- ** ReleaseHosts
    ReleaseHosts (ReleaseHosts'),
    newReleaseHosts,
    ReleaseHostsResponse (ReleaseHostsResponse'),
    newReleaseHostsResponse,

    -- ** EnableSerialConsoleAccess
    EnableSerialConsoleAccess (EnableSerialConsoleAccess'),
    newEnableSerialConsoleAccess,
    EnableSerialConsoleAccessResponse (EnableSerialConsoleAccessResponse'),
    newEnableSerialConsoleAccessResponse,

    -- ** ResetNetworkInterfaceAttribute
    ResetNetworkInterfaceAttribute (ResetNetworkInterfaceAttribute'),
    newResetNetworkInterfaceAttribute,
    ResetNetworkInterfaceAttributeResponse (ResetNetworkInterfaceAttributeResponse'),
    newResetNetworkInterfaceAttributeResponse,

    -- ** DeleteInternetGateway
    DeleteInternetGateway (DeleteInternetGateway'),
    newDeleteInternetGateway,
    DeleteInternetGatewayResponse (DeleteInternetGatewayResponse'),
    newDeleteInternetGatewayResponse,

    -- ** DescribeReservedInstancesListings
    DescribeReservedInstancesListings (DescribeReservedInstancesListings'),
    newDescribeReservedInstancesListings,
    DescribeReservedInstancesListingsResponse (DescribeReservedInstancesListingsResponse'),
    newDescribeReservedInstancesListingsResponse,

    -- ** CreateVpnConnection
    CreateVpnConnection (CreateVpnConnection'),
    newCreateVpnConnection,
    CreateVpnConnectionResponse (CreateVpnConnectionResponse'),
    newCreateVpnConnectionResponse,

    -- ** ReplaceTransitGatewayRoute
    ReplaceTransitGatewayRoute (ReplaceTransitGatewayRoute'),
    newReplaceTransitGatewayRoute,
    ReplaceTransitGatewayRouteResponse (ReplaceTransitGatewayRouteResponse'),
    newReplaceTransitGatewayRouteResponse,

    -- ** CreateFleet
    CreateFleet (CreateFleet'),
    newCreateFleet,
    CreateFleetResponse (CreateFleetResponse'),
    newCreateFleetResponse,

    -- ** DeleteNatGateway
    DeleteNatGateway (DeleteNatGateway'),
    newDeleteNatGateway,
    DeleteNatGatewayResponse (DeleteNatGatewayResponse'),
    newDeleteNatGatewayResponse,

    -- ** DescribeImportSnapshotTasks (Paginated)
    DescribeImportSnapshotTasks (DescribeImportSnapshotTasks'),
    newDescribeImportSnapshotTasks,
    DescribeImportSnapshotTasksResponse (DescribeImportSnapshotTasksResponse'),
    newDescribeImportSnapshotTasksResponse,

    -- ** GetCoipPoolUsage
    GetCoipPoolUsage (GetCoipPoolUsage'),
    newGetCoipPoolUsage,
    GetCoipPoolUsageResponse (GetCoipPoolUsageResponse'),
    newGetCoipPoolUsageResponse,

    -- ** DescribeCustomerGateways
    DescribeCustomerGateways (DescribeCustomerGateways'),
    newDescribeCustomerGateways,
    DescribeCustomerGatewaysResponse (DescribeCustomerGatewaysResponse'),
    newDescribeCustomerGatewaysResponse,

    -- ** DeleteSubnet
    DeleteSubnet (DeleteSubnet'),
    newDeleteSubnet,
    DeleteSubnetResponse (DeleteSubnetResponse'),
    newDeleteSubnetResponse,

    -- ** CopyImage
    CopyImage (CopyImage'),
    newCopyImage,
    CopyImageResponse (CopyImageResponse'),
    newCopyImageResponse,

    -- ** CreateVpcEndpoint
    CreateVpcEndpoint (CreateVpcEndpoint'),
    newCreateVpcEndpoint,
    CreateVpcEndpointResponse (CreateVpcEndpointResponse'),
    newCreateVpcEndpointResponse,

    -- ** ModifyTrafficMirrorSession
    ModifyTrafficMirrorSession (ModifyTrafficMirrorSession'),
    newModifyTrafficMirrorSession,
    ModifyTrafficMirrorSessionResponse (ModifyTrafficMirrorSessionResponse'),
    newModifyTrafficMirrorSessionResponse,

    -- ** DescribeCarrierGateways (Paginated)
    DescribeCarrierGateways (DescribeCarrierGateways'),
    newDescribeCarrierGateways,
    DescribeCarrierGatewaysResponse (DescribeCarrierGatewaysResponse'),
    newDescribeCarrierGatewaysResponse,

    -- ** DescribeTransitGatewayPeeringAttachments (Paginated)
    DescribeTransitGatewayPeeringAttachments (DescribeTransitGatewayPeeringAttachments'),
    newDescribeTransitGatewayPeeringAttachments,
    DescribeTransitGatewayPeeringAttachmentsResponse (DescribeTransitGatewayPeeringAttachmentsResponse'),
    newDescribeTransitGatewayPeeringAttachmentsResponse,

    -- ** DeleteQueuedReservedInstances
    DeleteQueuedReservedInstances (DeleteQueuedReservedInstances'),
    newDeleteQueuedReservedInstances,
    DeleteQueuedReservedInstancesResponse (DeleteQueuedReservedInstancesResponse'),
    newDeleteQueuedReservedInstancesResponse,

    -- ** DescribeTransitGatewayMulticastDomains (Paginated)
    DescribeTransitGatewayMulticastDomains (DescribeTransitGatewayMulticastDomains'),
    newDescribeTransitGatewayMulticastDomains,
    DescribeTransitGatewayMulticastDomainsResponse (DescribeTransitGatewayMulticastDomainsResponse'),
    newDescribeTransitGatewayMulticastDomainsResponse,

    -- ** GetDefaultCreditSpecification
    GetDefaultCreditSpecification (GetDefaultCreditSpecification'),
    newGetDefaultCreditSpecification,
    GetDefaultCreditSpecificationResponse (GetDefaultCreditSpecificationResponse'),
    newGetDefaultCreditSpecificationResponse,

    -- ** UnmonitorInstances
    UnmonitorInstances (UnmonitorInstances'),
    newUnmonitorInstances,
    UnmonitorInstancesResponse (UnmonitorInstancesResponse'),
    newUnmonitorInstancesResponse,

    -- ** DescribeTransitGatewayVpcAttachments (Paginated)
    DescribeTransitGatewayVpcAttachments (DescribeTransitGatewayVpcAttachments'),
    newDescribeTransitGatewayVpcAttachments,
    DescribeTransitGatewayVpcAttachmentsResponse (DescribeTransitGatewayVpcAttachmentsResponse'),
    newDescribeTransitGatewayVpcAttachmentsResponse,

    -- ** DescribeTransitGatewayConnectPeers (Paginated)
    DescribeTransitGatewayConnectPeers (DescribeTransitGatewayConnectPeers'),
    newDescribeTransitGatewayConnectPeers,
    DescribeTransitGatewayConnectPeersResponse (DescribeTransitGatewayConnectPeersResponse'),
    newDescribeTransitGatewayConnectPeersResponse,

    -- ** CreateSecurityGroup
    CreateSecurityGroup (CreateSecurityGroup'),
    newCreateSecurityGroup,
    CreateSecurityGroupResponse (CreateSecurityGroupResponse'),
    newCreateSecurityGroupResponse,

    -- ** CreateInstanceEventWindow
    CreateInstanceEventWindow (CreateInstanceEventWindow'),
    newCreateInstanceEventWindow,
    CreateInstanceEventWindowResponse (CreateInstanceEventWindowResponse'),
    newCreateInstanceEventWindowResponse,

    -- ** GetEbsEncryptionByDefault
    GetEbsEncryptionByDefault (GetEbsEncryptionByDefault'),
    newGetEbsEncryptionByDefault,
    GetEbsEncryptionByDefaultResponse (GetEbsEncryptionByDefaultResponse'),
    newGetEbsEncryptionByDefaultResponse,

    -- ** ImportVolume
    ImportVolume (ImportVolume'),
    newImportVolume,
    ImportVolumeResponse (ImportVolumeResponse'),
    newImportVolumeResponse,

    -- ** DeleteCarrierGateway
    DeleteCarrierGateway (DeleteCarrierGateway'),
    newDeleteCarrierGateway,
    DeleteCarrierGatewayResponse (DeleteCarrierGatewayResponse'),
    newDeleteCarrierGatewayResponse,

    -- ** DisableVgwRoutePropagation
    DisableVgwRoutePropagation (DisableVgwRoutePropagation'),
    newDisableVgwRoutePropagation,
    DisableVgwRoutePropagationResponse (DisableVgwRoutePropagationResponse'),
    newDisableVgwRoutePropagationResponse,

    -- ** DeleteTrafficMirrorFilter
    DeleteTrafficMirrorFilter (DeleteTrafficMirrorFilter'),
    newDeleteTrafficMirrorFilter,
    DeleteTrafficMirrorFilterResponse (DeleteTrafficMirrorFilterResponse'),
    newDeleteTrafficMirrorFilterResponse,

    -- ** ModifyVpnTunnelCertificate
    ModifyVpnTunnelCertificate (ModifyVpnTunnelCertificate'),
    newModifyVpnTunnelCertificate,
    ModifyVpnTunnelCertificateResponse (ModifyVpnTunnelCertificateResponse'),
    newModifyVpnTunnelCertificateResponse,

    -- ** CreateSpotDatafeedSubscription
    CreateSpotDatafeedSubscription (CreateSpotDatafeedSubscription'),
    newCreateSpotDatafeedSubscription,
    CreateSpotDatafeedSubscriptionResponse (CreateSpotDatafeedSubscriptionResponse'),
    newCreateSpotDatafeedSubscriptionResponse,

    -- ** CancelSpotInstanceRequests
    CancelSpotInstanceRequests (CancelSpotInstanceRequests'),
    newCancelSpotInstanceRequests,
    CancelSpotInstanceRequestsResponse (CancelSpotInstanceRequestsResponse'),
    newCancelSpotInstanceRequestsResponse,

    -- ** CreateRoute
    CreateRoute (CreateRoute'),
    newCreateRoute,
    CreateRouteResponse (CreateRouteResponse'),
    newCreateRouteResponse,

    -- ** DescribeVpcEndpointServiceConfigurations (Paginated)
    DescribeVpcEndpointServiceConfigurations (DescribeVpcEndpointServiceConfigurations'),
    newDescribeVpcEndpointServiceConfigurations,
    DescribeVpcEndpointServiceConfigurationsResponse (DescribeVpcEndpointServiceConfigurationsResponse'),
    newDescribeVpcEndpointServiceConfigurationsResponse,

    -- ** DeleteSnapshot
    DeleteSnapshot (DeleteSnapshot'),
    newDeleteSnapshot,
    DeleteSnapshotResponse (DeleteSnapshotResponse'),
    newDeleteSnapshotResponse,

    -- ** AssignPrivateIpAddresses
    AssignPrivateIpAddresses (AssignPrivateIpAddresses'),
    newAssignPrivateIpAddresses,
    AssignPrivateIpAddressesResponse (AssignPrivateIpAddressesResponse'),
    newAssignPrivateIpAddressesResponse,

    -- ** AuthorizeClientVpnIngress
    AuthorizeClientVpnIngress (AuthorizeClientVpnIngress'),
    newAuthorizeClientVpnIngress,
    AuthorizeClientVpnIngressResponse (AuthorizeClientVpnIngressResponse'),
    newAuthorizeClientVpnIngressResponse,

    -- ** DeleteTransitGatewayPeeringAttachment
    DeleteTransitGatewayPeeringAttachment (DeleteTransitGatewayPeeringAttachment'),
    newDeleteTransitGatewayPeeringAttachment,
    DeleteTransitGatewayPeeringAttachmentResponse (DeleteTransitGatewayPeeringAttachmentResponse'),
    newDeleteTransitGatewayPeeringAttachmentResponse,

    -- ** ModifyInstanceAttribute
    ModifyInstanceAttribute (ModifyInstanceAttribute'),
    newModifyInstanceAttribute,
    ModifyInstanceAttributeResponse (ModifyInstanceAttributeResponse'),
    newModifyInstanceAttributeResponse,

    -- ** DeleteCustomerGateway
    DeleteCustomerGateway (DeleteCustomerGateway'),
    newDeleteCustomerGateway,
    DeleteCustomerGatewayResponse (DeleteCustomerGatewayResponse'),
    newDeleteCustomerGatewayResponse,

    -- ** DisassociateIamInstanceProfile
    DisassociateIamInstanceProfile (DisassociateIamInstanceProfile'),
    newDisassociateIamInstanceProfile,
    DisassociateIamInstanceProfileResponse (DisassociateIamInstanceProfileResponse'),
    newDisassociateIamInstanceProfileResponse,

    -- ** TerminateClientVpnConnections
    TerminateClientVpnConnections (TerminateClientVpnConnections'),
    newTerminateClientVpnConnections,
    TerminateClientVpnConnectionsResponse (TerminateClientVpnConnectionsResponse'),
    newTerminateClientVpnConnectionsResponse,

    -- ** CreateTransitGatewayConnect
    CreateTransitGatewayConnect (CreateTransitGatewayConnect'),
    newCreateTransitGatewayConnect,
    CreateTransitGatewayConnectResponse (CreateTransitGatewayConnectResponse'),
    newCreateTransitGatewayConnectResponse,

    -- ** DisassociateRouteTable
    DisassociateRouteTable (DisassociateRouteTable'),
    newDisassociateRouteTable,
    DisassociateRouteTableResponse (DisassociateRouteTableResponse'),
    newDisassociateRouteTableResponse,

    -- ** GetConsoleScreenshot
    GetConsoleScreenshot (GetConsoleScreenshot'),
    newGetConsoleScreenshot,
    GetConsoleScreenshotResponse (GetConsoleScreenshotResponse'),
    newGetConsoleScreenshotResponse,

    -- ** GetFlowLogsIntegrationTemplate
    GetFlowLogsIntegrationTemplate (GetFlowLogsIntegrationTemplate'),
    newGetFlowLogsIntegrationTemplate,
    GetFlowLogsIntegrationTemplateResponse (GetFlowLogsIntegrationTemplateResponse'),
    newGetFlowLogsIntegrationTemplateResponse,

    -- ** ResetEbsDefaultKmsKeyId
    ResetEbsDefaultKmsKeyId (ResetEbsDefaultKmsKeyId'),
    newResetEbsDefaultKmsKeyId,
    ResetEbsDefaultKmsKeyIdResponse (ResetEbsDefaultKmsKeyIdResponse'),
    newResetEbsDefaultKmsKeyIdResponse,

    -- ** AssignIpv6Addresses
    AssignIpv6Addresses (AssignIpv6Addresses'),
    newAssignIpv6Addresses,
    AssignIpv6AddressesResponse (AssignIpv6AddressesResponse'),
    newAssignIpv6AddressesResponse,

    -- ** ModifyVpnTunnelOptions
    ModifyVpnTunnelOptions (ModifyVpnTunnelOptions'),
    newModifyVpnTunnelOptions,
    ModifyVpnTunnelOptionsResponse (ModifyVpnTunnelOptionsResponse'),
    newModifyVpnTunnelOptionsResponse,

    -- ** ModifyEbsDefaultKmsKeyId
    ModifyEbsDefaultKmsKeyId (ModifyEbsDefaultKmsKeyId'),
    newModifyEbsDefaultKmsKeyId,
    ModifyEbsDefaultKmsKeyIdResponse (ModifyEbsDefaultKmsKeyIdResponse'),
    newModifyEbsDefaultKmsKeyIdResponse,

    -- ** DeleteSpotDatafeedSubscription
    DeleteSpotDatafeedSubscription (DeleteSpotDatafeedSubscription'),
    newDeleteSpotDatafeedSubscription,
    DeleteSpotDatafeedSubscriptionResponse (DeleteSpotDatafeedSubscriptionResponse'),
    newDeleteSpotDatafeedSubscriptionResponse,

    -- ** ModifyVolume
    ModifyVolume (ModifyVolume'),
    newModifyVolume,
    ModifyVolumeResponse (ModifyVolumeResponse'),
    newModifyVolumeResponse,

    -- ** EnableVpcClassicLink
    EnableVpcClassicLink (EnableVpcClassicLink'),
    newEnableVpcClassicLink,
    EnableVpcClassicLinkResponse (EnableVpcClassicLinkResponse'),
    newEnableVpcClassicLinkResponse,

    -- ** DescribePlacementGroups
    DescribePlacementGroups (DescribePlacementGroups'),
    newDescribePlacementGroups,
    DescribePlacementGroupsResponse (DescribePlacementGroupsResponse'),
    newDescribePlacementGroupsResponse,

    -- ** ProvisionByoipCidr
    ProvisionByoipCidr (ProvisionByoipCidr'),
    newProvisionByoipCidr,
    ProvisionByoipCidrResponse (ProvisionByoipCidrResponse'),
    newProvisionByoipCidrResponse,

    -- ** DisassociateEnclaveCertificateIamRole
    DisassociateEnclaveCertificateIamRole (DisassociateEnclaveCertificateIamRole'),
    newDisassociateEnclaveCertificateIamRole,
    DisassociateEnclaveCertificateIamRoleResponse (DisassociateEnclaveCertificateIamRoleResponse'),
    newDisassociateEnclaveCertificateIamRoleResponse,

    -- ** ModifyAvailabilityZoneGroup
    ModifyAvailabilityZoneGroup (ModifyAvailabilityZoneGroup'),
    newModifyAvailabilityZoneGroup,
    ModifyAvailabilityZoneGroupResponse (ModifyAvailabilityZoneGroupResponse'),
    newModifyAvailabilityZoneGroupResponse,

    -- ** DescribeStaleSecurityGroups (Paginated)
    DescribeStaleSecurityGroups (DescribeStaleSecurityGroups'),
    newDescribeStaleSecurityGroups,
    DescribeStaleSecurityGroupsResponse (DescribeStaleSecurityGroupsResponse'),
    newDescribeStaleSecurityGroupsResponse,

    -- ** CreateCarrierGateway
    CreateCarrierGateway (CreateCarrierGateway'),
    newCreateCarrierGateway,
    CreateCarrierGatewayResponse (CreateCarrierGatewayResponse'),
    newCreateCarrierGatewayResponse,

    -- ** DescribeExportImageTasks (Paginated)
    DescribeExportImageTasks (DescribeExportImageTasks'),
    newDescribeExportImageTasks,
    DescribeExportImageTasksResponse (DescribeExportImageTasksResponse'),
    newDescribeExportImageTasksResponse,

    -- ** PurchaseScheduledInstances
    PurchaseScheduledInstances (PurchaseScheduledInstances'),
    newPurchaseScheduledInstances,
    PurchaseScheduledInstancesResponse (PurchaseScheduledInstancesResponse'),
    newPurchaseScheduledInstancesResponse,

    -- ** EnableVgwRoutePropagation
    EnableVgwRoutePropagation (EnableVgwRoutePropagation'),
    newEnableVgwRoutePropagation,
    EnableVgwRoutePropagationResponse (EnableVgwRoutePropagationResponse'),
    newEnableVgwRoutePropagationResponse,

    -- ** DescribeSpotFleetRequestHistory
    DescribeSpotFleetRequestHistory (DescribeSpotFleetRequestHistory'),
    newDescribeSpotFleetRequestHistory,
    DescribeSpotFleetRequestHistoryResponse (DescribeSpotFleetRequestHistoryResponse'),
    newDescribeSpotFleetRequestHistoryResponse,

    -- ** ModifySnapshotAttribute
    ModifySnapshotAttribute (ModifySnapshotAttribute'),
    newModifySnapshotAttribute,
    ModifySnapshotAttributeResponse (ModifySnapshotAttributeResponse'),
    newModifySnapshotAttributeResponse,

    -- ** DescribeIamInstanceProfileAssociations (Paginated)
    DescribeIamInstanceProfileAssociations (DescribeIamInstanceProfileAssociations'),
    newDescribeIamInstanceProfileAssociations,
    DescribeIamInstanceProfileAssociationsResponse (DescribeIamInstanceProfileAssociationsResponse'),
    newDescribeIamInstanceProfileAssociationsResponse,

    -- ** DescribeNetworkInsightsPaths (Paginated)
    DescribeNetworkInsightsPaths (DescribeNetworkInsightsPaths'),
    newDescribeNetworkInsightsPaths,
    DescribeNetworkInsightsPathsResponse (DescribeNetworkInsightsPathsResponse'),
    newDescribeNetworkInsightsPathsResponse,

    -- ** CreateSnapshot
    CreateSnapshot (CreateSnapshot'),
    newCreateSnapshot,
    Snapshot (Snapshot'),
    newSnapshot,

    -- ** CreateLocalGatewayRoute
    CreateLocalGatewayRoute (CreateLocalGatewayRoute'),
    newCreateLocalGatewayRoute,
    CreateLocalGatewayRouteResponse (CreateLocalGatewayRouteResponse'),
    newCreateLocalGatewayRouteResponse,

    -- ** CreateNetworkAclEntry
    CreateNetworkAclEntry (CreateNetworkAclEntry'),
    newCreateNetworkAclEntry,
    CreateNetworkAclEntryResponse (CreateNetworkAclEntryResponse'),
    newCreateNetworkAclEntryResponse,

    -- ** DescribeTransitGatewayAttachments (Paginated)
    DescribeTransitGatewayAttachments (DescribeTransitGatewayAttachments'),
    newDescribeTransitGatewayAttachments,
    DescribeTransitGatewayAttachmentsResponse (DescribeTransitGatewayAttachmentsResponse'),
    newDescribeTransitGatewayAttachmentsResponse,

    -- ** CreateReservedInstancesListing
    CreateReservedInstancesListing (CreateReservedInstancesListing'),
    newCreateReservedInstancesListing,
    CreateReservedInstancesListingResponse (CreateReservedInstancesListingResponse'),
    newCreateReservedInstancesListingResponse,

    -- ** DescribeIpv6Pools (Paginated)
    DescribeIpv6Pools (DescribeIpv6Pools'),
    newDescribeIpv6Pools,
    DescribeIpv6PoolsResponse (DescribeIpv6PoolsResponse'),
    newDescribeIpv6PoolsResponse,

    -- ** AttachVpnGateway
    AttachVpnGateway (AttachVpnGateway'),
    newAttachVpnGateway,
    AttachVpnGatewayResponse (AttachVpnGatewayResponse'),
    newAttachVpnGatewayResponse,

    -- ** DescribeLocalGateways (Paginated)
    DescribeLocalGateways (DescribeLocalGateways'),
    newDescribeLocalGateways,
    DescribeLocalGatewaysResponse (DescribeLocalGatewaysResponse'),
    newDescribeLocalGatewaysResponse,

    -- ** ModifyVpcEndpointServicePermissions
    ModifyVpcEndpointServicePermissions (ModifyVpcEndpointServicePermissions'),
    newModifyVpcEndpointServicePermissions,
    ModifyVpcEndpointServicePermissionsResponse (ModifyVpcEndpointServicePermissionsResponse'),
    newModifyVpcEndpointServicePermissionsResponse,

    -- ** ExportClientVpnClientCertificateRevocationList
    ExportClientVpnClientCertificateRevocationList (ExportClientVpnClientCertificateRevocationList'),
    newExportClientVpnClientCertificateRevocationList,
    ExportClientVpnClientCertificateRevocationListResponse (ExportClientVpnClientCertificateRevocationListResponse'),
    newExportClientVpnClientCertificateRevocationListResponse,

    -- ** CreateDhcpOptions
    CreateDhcpOptions (CreateDhcpOptions'),
    newCreateDhcpOptions,
    CreateDhcpOptionsResponse (CreateDhcpOptionsResponse'),
    newCreateDhcpOptionsResponse,

    -- ** RegisterTransitGatewayMulticastGroupSources
    RegisterTransitGatewayMulticastGroupSources (RegisterTransitGatewayMulticastGroupSources'),
    newRegisterTransitGatewayMulticastGroupSources,
    RegisterTransitGatewayMulticastGroupSourcesResponse (RegisterTransitGatewayMulticastGroupSourcesResponse'),
    newRegisterTransitGatewayMulticastGroupSourcesResponse,

    -- ** DescribeAccountAttributes
    DescribeAccountAttributes (DescribeAccountAttributes'),
    newDescribeAccountAttributes,
    DescribeAccountAttributesResponse (DescribeAccountAttributesResponse'),
    newDescribeAccountAttributesResponse,

    -- ** GetTransitGatewayRouteTablePropagations (Paginated)
    GetTransitGatewayRouteTablePropagations (GetTransitGatewayRouteTablePropagations'),
    newGetTransitGatewayRouteTablePropagations,
    GetTransitGatewayRouteTablePropagationsResponse (GetTransitGatewayRouteTablePropagationsResponse'),
    newGetTransitGatewayRouteTablePropagationsResponse,

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

    -- ** RebootInstances
    RebootInstances (RebootInstances'),
    newRebootInstances,
    RebootInstancesResponse (RebootInstancesResponse'),
    newRebootInstancesResponse,

    -- ** ModifyVpcEndpointServiceConfiguration
    ModifyVpcEndpointServiceConfiguration (ModifyVpcEndpointServiceConfiguration'),
    newModifyVpcEndpointServiceConfiguration,
    ModifyVpcEndpointServiceConfigurationResponse (ModifyVpcEndpointServiceConfigurationResponse'),
    newModifyVpcEndpointServiceConfigurationResponse,

    -- ** CreateTransitGateway
    CreateTransitGateway (CreateTransitGateway'),
    newCreateTransitGateway,
    CreateTransitGatewayResponse (CreateTransitGatewayResponse'),
    newCreateTransitGatewayResponse,

    -- ** UnassignIpv6Addresses
    UnassignIpv6Addresses (UnassignIpv6Addresses'),
    newUnassignIpv6Addresses,
    UnassignIpv6AddressesResponse (UnassignIpv6AddressesResponse'),
    newUnassignIpv6AddressesResponse,

    -- ** DeleteTrafficMirrorSession
    DeleteTrafficMirrorSession (DeleteTrafficMirrorSession'),
    newDeleteTrafficMirrorSession,
    DeleteTrafficMirrorSessionResponse (DeleteTrafficMirrorSessionResponse'),
    newDeleteTrafficMirrorSessionResponse,

    -- ** CreateManagedPrefixList
    CreateManagedPrefixList (CreateManagedPrefixList'),
    newCreateManagedPrefixList,
    CreateManagedPrefixListResponse (CreateManagedPrefixListResponse'),
    newCreateManagedPrefixListResponse,

    -- ** CreateReplaceRootVolumeTask
    CreateReplaceRootVolumeTask (CreateReplaceRootVolumeTask'),
    newCreateReplaceRootVolumeTask,
    CreateReplaceRootVolumeTaskResponse (CreateReplaceRootVolumeTaskResponse'),
    newCreateReplaceRootVolumeTaskResponse,

    -- ** AssociateIamInstanceProfile
    AssociateIamInstanceProfile (AssociateIamInstanceProfile'),
    newAssociateIamInstanceProfile,
    AssociateIamInstanceProfileResponse (AssociateIamInstanceProfileResponse'),
    newAssociateIamInstanceProfileResponse,

    -- ** ModifyDefaultCreditSpecification
    ModifyDefaultCreditSpecification (ModifyDefaultCreditSpecification'),
    newModifyDefaultCreditSpecification,
    ModifyDefaultCreditSpecificationResponse (ModifyDefaultCreditSpecificationResponse'),
    newModifyDefaultCreditSpecificationResponse,

    -- ** DeleteEgressOnlyInternetGateway
    DeleteEgressOnlyInternetGateway (DeleteEgressOnlyInternetGateway'),
    newDeleteEgressOnlyInternetGateway,
    DeleteEgressOnlyInternetGatewayResponse (DeleteEgressOnlyInternetGatewayResponse'),
    newDeleteEgressOnlyInternetGatewayResponse,

    -- ** PurchaseHostReservation
    PurchaseHostReservation (PurchaseHostReservation'),
    newPurchaseHostReservation,
    PurchaseHostReservationResponse (PurchaseHostReservationResponse'),
    newPurchaseHostReservationResponse,

    -- ** ModifyTransitGatewayVpcAttachment
    ModifyTransitGatewayVpcAttachment (ModifyTransitGatewayVpcAttachment'),
    newModifyTransitGatewayVpcAttachment,
    ModifyTransitGatewayVpcAttachmentResponse (ModifyTransitGatewayVpcAttachmentResponse'),
    newModifyTransitGatewayVpcAttachmentResponse,

    -- ** CreateImage
    CreateImage (CreateImage'),
    newCreateImage,
    CreateImageResponse (CreateImageResponse'),
    newCreateImageResponse,

    -- ** DescribeClassicLinkInstances (Paginated)
    DescribeClassicLinkInstances (DescribeClassicLinkInstances'),
    newDescribeClassicLinkInstances,
    DescribeClassicLinkInstancesResponse (DescribeClassicLinkInstancesResponse'),
    newDescribeClassicLinkInstancesResponse,

    -- ** TerminateInstances
    TerminateInstances (TerminateInstances'),
    newTerminateInstances,
    TerminateInstancesResponse (TerminateInstancesResponse'),
    newTerminateInstancesResponse,

    -- ** DescribeStoreImageTasks (Paginated)
    DescribeStoreImageTasks (DescribeStoreImageTasks'),
    newDescribeStoreImageTasks,
    DescribeStoreImageTasksResponse (DescribeStoreImageTasksResponse'),
    newDescribeStoreImageTasksResponse,

    -- ** GetVpnConnectionDeviceTypes (Paginated)
    GetVpnConnectionDeviceTypes (GetVpnConnectionDeviceTypes'),
    newGetVpnConnectionDeviceTypes,
    GetVpnConnectionDeviceTypesResponse (GetVpnConnectionDeviceTypesResponse'),
    newGetVpnConnectionDeviceTypesResponse,

    -- ** GetTransitGatewayPrefixListReferences (Paginated)
    GetTransitGatewayPrefixListReferences (GetTransitGatewayPrefixListReferences'),
    newGetTransitGatewayPrefixListReferences,
    GetTransitGatewayPrefixListReferencesResponse (GetTransitGatewayPrefixListReferencesResponse'),
    newGetTransitGatewayPrefixListReferencesResponse,

    -- ** DescribeKeyPairs
    DescribeKeyPairs (DescribeKeyPairs'),
    newDescribeKeyPairs,
    DescribeKeyPairsResponse (DescribeKeyPairsResponse'),
    newDescribeKeyPairsResponse,

    -- ** DisableFastSnapshotRestores
    DisableFastSnapshotRestores (DisableFastSnapshotRestores'),
    newDisableFastSnapshotRestores,
    DisableFastSnapshotRestoresResponse (DisableFastSnapshotRestoresResponse'),
    newDisableFastSnapshotRestoresResponse,

    -- ** DescribeLaunchTemplates (Paginated)
    DescribeLaunchTemplates (DescribeLaunchTemplates'),
    newDescribeLaunchTemplates,
    DescribeLaunchTemplatesResponse (DescribeLaunchTemplatesResponse'),
    newDescribeLaunchTemplatesResponse,

    -- ** CreateVpnConnectionRoute
    CreateVpnConnectionRoute (CreateVpnConnectionRoute'),
    newCreateVpnConnectionRoute,
    CreateVpnConnectionRouteResponse (CreateVpnConnectionRouteResponse'),
    newCreateVpnConnectionRouteResponse,

    -- ** AssociateRouteTable
    AssociateRouteTable (AssociateRouteTable'),
    newAssociateRouteTable,
    AssociateRouteTableResponse (AssociateRouteTableResponse'),
    newAssociateRouteTableResponse,

    -- ** CreateSubnetCidrReservation
    CreateSubnetCidrReservation (CreateSubnetCidrReservation'),
    newCreateSubnetCidrReservation,
    CreateSubnetCidrReservationResponse (CreateSubnetCidrReservationResponse'),
    newCreateSubnetCidrReservationResponse,

    -- ** DescribeVpnGateways
    DescribeVpnGateways (DescribeVpnGateways'),
    newDescribeVpnGateways,
    DescribeVpnGatewaysResponse (DescribeVpnGatewaysResponse'),
    newDescribeVpnGatewaysResponse,

    -- ** ModifyVpnConnectionOptions
    ModifyVpnConnectionOptions (ModifyVpnConnectionOptions'),
    newModifyVpnConnectionOptions,
    ModifyVpnConnectionOptionsResponse (ModifyVpnConnectionOptionsResponse'),
    newModifyVpnConnectionOptionsResponse,

    -- ** GetConsoleOutput
    GetConsoleOutput (GetConsoleOutput'),
    newGetConsoleOutput,
    GetConsoleOutputResponse (GetConsoleOutputResponse'),
    newGetConsoleOutputResponse,

    -- ** DescribeHosts (Paginated)
    DescribeHosts (DescribeHosts'),
    newDescribeHosts,
    DescribeHostsResponse (DescribeHostsResponse'),
    newDescribeHostsResponse,

    -- ** DescribeImageAttribute
    DescribeImageAttribute (DescribeImageAttribute'),
    newDescribeImageAttribute,
    DescribeImageAttributeResponse (DescribeImageAttributeResponse'),
    newDescribeImageAttributeResponse,

    -- ** ModifyIdFormat
    ModifyIdFormat (ModifyIdFormat'),
    newModifyIdFormat,
    ModifyIdFormatResponse (ModifyIdFormatResponse'),
    newModifyIdFormatResponse,

    -- ** RegisterTransitGatewayMulticastGroupMembers
    RegisterTransitGatewayMulticastGroupMembers (RegisterTransitGatewayMulticastGroupMembers'),
    newRegisterTransitGatewayMulticastGroupMembers,
    RegisterTransitGatewayMulticastGroupMembersResponse (RegisterTransitGatewayMulticastGroupMembersResponse'),
    newRegisterTransitGatewayMulticastGroupMembersResponse,

    -- ** DeleteManagedPrefixList
    DeleteManagedPrefixList (DeleteManagedPrefixList'),
    newDeleteManagedPrefixList,
    DeleteManagedPrefixListResponse (DeleteManagedPrefixListResponse'),
    newDeleteManagedPrefixListResponse,

    -- ** DeleteRouteTable
    DeleteRouteTable (DeleteRouteTable'),
    newDeleteRouteTable,
    DeleteRouteTableResponse (DeleteRouteTableResponse'),
    newDeleteRouteTableResponse,

    -- ** ResetImageAttribute
    ResetImageAttribute (ResetImageAttribute'),
    newResetImageAttribute,
    ResetImageAttributeResponse (ResetImageAttributeResponse'),
    newResetImageAttributeResponse,

    -- ** ModifyTransitGatewayPrefixListReference
    ModifyTransitGatewayPrefixListReference (ModifyTransitGatewayPrefixListReference'),
    newModifyTransitGatewayPrefixListReference,
    ModifyTransitGatewayPrefixListReferenceResponse (ModifyTransitGatewayPrefixListReferenceResponse'),
    newModifyTransitGatewayPrefixListReferenceResponse,

    -- ** DescribeTransitGatewayRouteTables (Paginated)
    DescribeTransitGatewayRouteTables (DescribeTransitGatewayRouteTables'),
    newDescribeTransitGatewayRouteTables,
    DescribeTransitGatewayRouteTablesResponse (DescribeTransitGatewayRouteTablesResponse'),
    newDescribeTransitGatewayRouteTablesResponse,

    -- ** CreateEgressOnlyInternetGateway
    CreateEgressOnlyInternetGateway (CreateEgressOnlyInternetGateway'),
    newCreateEgressOnlyInternetGateway,
    CreateEgressOnlyInternetGatewayResponse (CreateEgressOnlyInternetGatewayResponse'),
    newCreateEgressOnlyInternetGatewayResponse,

    -- ** DescribeReservedInstancesModifications (Paginated)
    DescribeReservedInstancesModifications (DescribeReservedInstancesModifications'),
    newDescribeReservedInstancesModifications,
    DescribeReservedInstancesModificationsResponse (DescribeReservedInstancesModificationsResponse'),
    newDescribeReservedInstancesModificationsResponse,

    -- ** DescribeSpotInstanceRequests (Paginated)
    DescribeSpotInstanceRequests (DescribeSpotInstanceRequests'),
    newDescribeSpotInstanceRequests,
    DescribeSpotInstanceRequestsResponse (DescribeSpotInstanceRequestsResponse'),
    newDescribeSpotInstanceRequestsResponse,

    -- ** RevokeClientVpnIngress
    RevokeClientVpnIngress (RevokeClientVpnIngress'),
    newRevokeClientVpnIngress,
    RevokeClientVpnIngressResponse (RevokeClientVpnIngressResponse'),
    newRevokeClientVpnIngressResponse,

    -- ** UnassignPrivateIpAddresses
    UnassignPrivateIpAddresses (UnassignPrivateIpAddresses'),
    newUnassignPrivateIpAddresses,
    UnassignPrivateIpAddressesResponse (UnassignPrivateIpAddressesResponse'),
    newUnassignPrivateIpAddressesResponse,

    -- ** DescribeNetworkInterfacePermissions (Paginated)
    DescribeNetworkInterfacePermissions (DescribeNetworkInterfacePermissions'),
    newDescribeNetworkInterfacePermissions,
    DescribeNetworkInterfacePermissionsResponse (DescribeNetworkInterfacePermissionsResponse'),
    newDescribeNetworkInterfacePermissionsResponse,

    -- ** EnableFastSnapshotRestores
    EnableFastSnapshotRestores (EnableFastSnapshotRestores'),
    newEnableFastSnapshotRestores,
    EnableFastSnapshotRestoresResponse (EnableFastSnapshotRestoresResponse'),
    newEnableFastSnapshotRestoresResponse,

    -- ** DescribeVpcEndpointServicePermissions (Paginated)
    DescribeVpcEndpointServicePermissions (DescribeVpcEndpointServicePermissions'),
    newDescribeVpcEndpointServicePermissions,
    DescribeVpcEndpointServicePermissionsResponse (DescribeVpcEndpointServicePermissionsResponse'),
    newDescribeVpcEndpointServicePermissionsResponse,

    -- ** DeleteDhcpOptions
    DeleteDhcpOptions (DeleteDhcpOptions'),
    newDeleteDhcpOptions,
    DeleteDhcpOptionsResponse (DeleteDhcpOptionsResponse'),
    newDeleteDhcpOptionsResponse,

    -- ** CreateRestoreImageTask
    CreateRestoreImageTask (CreateRestoreImageTask'),
    newCreateRestoreImageTask,
    CreateRestoreImageTaskResponse (CreateRestoreImageTaskResponse'),
    newCreateRestoreImageTaskResponse,

    -- ** RegisterInstanceEventNotificationAttributes
    RegisterInstanceEventNotificationAttributes (RegisterInstanceEventNotificationAttributes'),
    newRegisterInstanceEventNotificationAttributes,
    RegisterInstanceEventNotificationAttributesResponse (RegisterInstanceEventNotificationAttributesResponse'),
    newRegisterInstanceEventNotificationAttributesResponse,

    -- ** GetVpnConnectionDeviceSampleConfiguration
    GetVpnConnectionDeviceSampleConfiguration (GetVpnConnectionDeviceSampleConfiguration'),
    newGetVpnConnectionDeviceSampleConfiguration,
    GetVpnConnectionDeviceSampleConfigurationResponse (GetVpnConnectionDeviceSampleConfigurationResponse'),
    newGetVpnConnectionDeviceSampleConfigurationResponse,

    -- ** DeleteSubnetCidrReservation
    DeleteSubnetCidrReservation (DeleteSubnetCidrReservation'),
    newDeleteSubnetCidrReservation,
    DeleteSubnetCidrReservationResponse (DeleteSubnetCidrReservationResponse'),
    newDeleteSubnetCidrReservationResponse,

    -- ** DescribeNetworkAcls (Paginated)
    DescribeNetworkAcls (DescribeNetworkAcls'),
    newDescribeNetworkAcls,
    DescribeNetworkAclsResponse (DescribeNetworkAclsResponse'),
    newDescribeNetworkAclsResponse,

    -- ** CancelImportTask
    CancelImportTask (CancelImportTask'),
    newCancelImportTask,
    CancelImportTaskResponse (CancelImportTaskResponse'),
    newCancelImportTaskResponse,

    -- ** DetachClassicLinkVpc
    DetachClassicLinkVpc (DetachClassicLinkVpc'),
    newDetachClassicLinkVpc,
    DetachClassicLinkVpcResponse (DetachClassicLinkVpcResponse'),
    newDetachClassicLinkVpcResponse,

    -- ** CreateCapacityReservationFleet
    CreateCapacityReservationFleet (CreateCapacityReservationFleet'),
    newCreateCapacityReservationFleet,
    CreateCapacityReservationFleetResponse (CreateCapacityReservationFleetResponse'),
    newCreateCapacityReservationFleetResponse,

    -- ** DescribeRegions
    DescribeRegions (DescribeRegions'),
    newDescribeRegions,
    DescribeRegionsResponse (DescribeRegionsResponse'),
    newDescribeRegionsResponse,

    -- ** MonitorInstances
    MonitorInstances (MonitorInstances'),
    newMonitorInstances,
    MonitorInstancesResponse (MonitorInstancesResponse'),
    newMonitorInstancesResponse,

    -- ** RejectTransitGatewayMulticastDomainAssociations
    RejectTransitGatewayMulticastDomainAssociations (RejectTransitGatewayMulticastDomainAssociations'),
    newRejectTransitGatewayMulticastDomainAssociations,
    RejectTransitGatewayMulticastDomainAssociationsResponse (RejectTransitGatewayMulticastDomainAssociationsResponse'),
    newRejectTransitGatewayMulticastDomainAssociationsResponse,

    -- ** AcceptTransitGatewayMulticastDomainAssociations
    AcceptTransitGatewayMulticastDomainAssociations (AcceptTransitGatewayMulticastDomainAssociations'),
    newAcceptTransitGatewayMulticastDomainAssociations,
    AcceptTransitGatewayMulticastDomainAssociationsResponse (AcceptTransitGatewayMulticastDomainAssociationsResponse'),
    newAcceptTransitGatewayMulticastDomainAssociationsResponse,

    -- ** SearchLocalGatewayRoutes (Paginated)
    SearchLocalGatewayRoutes (SearchLocalGatewayRoutes'),
    newSearchLocalGatewayRoutes,
    SearchLocalGatewayRoutesResponse (SearchLocalGatewayRoutesResponse'),
    newSearchLocalGatewayRoutesResponse,

    -- ** DeleteClientVpnRoute
    DeleteClientVpnRoute (DeleteClientVpnRoute'),
    newDeleteClientVpnRoute,
    DeleteClientVpnRouteResponse (DeleteClientVpnRouteResponse'),
    newDeleteClientVpnRouteResponse,

    -- ** AcceptVpcPeeringConnection
    AcceptVpcPeeringConnection (AcceptVpcPeeringConnection'),
    newAcceptVpcPeeringConnection,
    AcceptVpcPeeringConnectionResponse (AcceptVpcPeeringConnectionResponse'),
    newAcceptVpcPeeringConnectionResponse,

    -- ** ImportSnapshot
    ImportSnapshot (ImportSnapshot'),
    newImportSnapshot,
    ImportSnapshotResponse (ImportSnapshotResponse'),
    newImportSnapshotResponse,

    -- ** DescribeAddressesAttribute (Paginated)
    DescribeAddressesAttribute (DescribeAddressesAttribute'),
    newDescribeAddressesAttribute,
    DescribeAddressesAttributeResponse (DescribeAddressesAttributeResponse'),
    newDescribeAddressesAttributeResponse,

    -- ** DescribeVolumeStatus (Paginated)
    DescribeVolumeStatus (DescribeVolumeStatus'),
    newDescribeVolumeStatus,
    DescribeVolumeStatusResponse (DescribeVolumeStatusResponse'),
    newDescribeVolumeStatusResponse,

    -- ** DescribeReplaceRootVolumeTasks (Paginated)
    DescribeReplaceRootVolumeTasks (DescribeReplaceRootVolumeTasks'),
    newDescribeReplaceRootVolumeTasks,
    DescribeReplaceRootVolumeTasksResponse (DescribeReplaceRootVolumeTasksResponse'),
    newDescribeReplaceRootVolumeTasksResponse,

    -- ** ModifyInstanceEventWindow
    ModifyInstanceEventWindow (ModifyInstanceEventWindow'),
    newModifyInstanceEventWindow,
    ModifyInstanceEventWindowResponse (ModifyInstanceEventWindowResponse'),
    newModifyInstanceEventWindowResponse,

    -- ** DescribeRouteTables (Paginated)
    DescribeRouteTables (DescribeRouteTables'),
    newDescribeRouteTables,
    DescribeRouteTablesResponse (DescribeRouteTablesResponse'),
    newDescribeRouteTablesResponse,

    -- ** DescribeAvailabilityZones
    DescribeAvailabilityZones (DescribeAvailabilityZones'),
    newDescribeAvailabilityZones,
    DescribeAvailabilityZonesResponse (DescribeAvailabilityZonesResponse'),
    newDescribeAvailabilityZonesResponse,

    -- ** ModifyVpcAttribute
    ModifyVpcAttribute (ModifyVpcAttribute'),
    newModifyVpcAttribute,
    ModifyVpcAttributeResponse (ModifyVpcAttributeResponse'),
    newModifyVpcAttributeResponse,

    -- ** DescribeClientVpnConnections (Paginated)
    DescribeClientVpnConnections (DescribeClientVpnConnections'),
    newDescribeClientVpnConnections,
    DescribeClientVpnConnectionsResponse (DescribeClientVpnConnectionsResponse'),
    newDescribeClientVpnConnectionsResponse,

    -- ** DescribeFleetHistory
    DescribeFleetHistory (DescribeFleetHistory'),
    newDescribeFleetHistory,
    DescribeFleetHistoryResponse (DescribeFleetHistoryResponse'),
    newDescribeFleetHistoryResponse,

    -- ** DescribeImages
    DescribeImages (DescribeImages'),
    newDescribeImages,
    DescribeImagesResponse (DescribeImagesResponse'),
    newDescribeImagesResponse,

    -- ** DescribeElasticGpus
    DescribeElasticGpus (DescribeElasticGpus'),
    newDescribeElasticGpus,
    DescribeElasticGpusResponse (DescribeElasticGpusResponse'),
    newDescribeElasticGpusResponse,

    -- ** DisassociateTrunkInterface
    DisassociateTrunkInterface (DisassociateTrunkInterface'),
    newDisassociateTrunkInterface,
    DisassociateTrunkInterfaceResponse (DisassociateTrunkInterfaceResponse'),
    newDisassociateTrunkInterfaceResponse,

    -- ** RestoreAddressToClassic
    RestoreAddressToClassic (RestoreAddressToClassic'),
    newRestoreAddressToClassic,
    RestoreAddressToClassicResponse (RestoreAddressToClassicResponse'),
    newRestoreAddressToClassicResponse,

    -- ** DescribeManagedPrefixLists (Paginated)
    DescribeManagedPrefixLists (DescribeManagedPrefixLists'),
    newDescribeManagedPrefixLists,
    DescribeManagedPrefixListsResponse (DescribeManagedPrefixListsResponse'),
    newDescribeManagedPrefixListsResponse,

    -- ** CreateKeyPair
    CreateKeyPair (CreateKeyPair'),
    newCreateKeyPair,
    CreateKeyPairResponse (CreateKeyPairResponse'),
    newCreateKeyPairResponse,

    -- ** GetReservedInstancesExchangeQuote
    GetReservedInstancesExchangeQuote (GetReservedInstancesExchangeQuote'),
    newGetReservedInstancesExchangeQuote,
    GetReservedInstancesExchangeQuoteResponse (GetReservedInstancesExchangeQuoteResponse'),
    newGetReservedInstancesExchangeQuoteResponse,

    -- ** DeleteVolume
    DeleteVolume (DeleteVolume'),
    newDeleteVolume,
    DeleteVolumeResponse (DeleteVolumeResponse'),
    newDeleteVolumeResponse,

    -- ** DeprovisionByoipCidr
    DeprovisionByoipCidr (DeprovisionByoipCidr'),
    newDeprovisionByoipCidr,
    DeprovisionByoipCidrResponse (DeprovisionByoipCidrResponse'),
    newDeprovisionByoipCidrResponse,

    -- ** GetSerialConsoleAccessStatus
    GetSerialConsoleAccessStatus (GetSerialConsoleAccessStatus'),
    newGetSerialConsoleAccessStatus,
    GetSerialConsoleAccessStatusResponse (GetSerialConsoleAccessStatusResponse'),
    newGetSerialConsoleAccessStatusResponse,

    -- ** DeleteVpcEndpointServiceConfigurations
    DeleteVpcEndpointServiceConfigurations (DeleteVpcEndpointServiceConfigurations'),
    newDeleteVpcEndpointServiceConfigurations,
    DeleteVpcEndpointServiceConfigurationsResponse (DeleteVpcEndpointServiceConfigurationsResponse'),
    newDeleteVpcEndpointServiceConfigurationsResponse,

    -- ** DescribeSpotFleetInstances (Paginated)
    DescribeSpotFleetInstances (DescribeSpotFleetInstances'),
    newDescribeSpotFleetInstances,
    DescribeSpotFleetInstancesResponse (DescribeSpotFleetInstancesResponse'),
    newDescribeSpotFleetInstancesResponse,

    -- * Types

    -- ** Common
    module Network.AWS.EC2.Internal,

    -- ** AccountAttributeName
    AccountAttributeName (..),

    -- ** ActivityStatus
    ActivityStatus (..),

    -- ** AddressAttributeName
    AddressAttributeName (..),

    -- ** AddressStatus
    AddressStatus (..),

    -- ** Affinity
    Affinity (..),

    -- ** AllocationState
    AllocationState (..),

    -- ** AllocationStrategy
    AllocationStrategy (..),

    -- ** AllowsMultipleInstanceTypes
    AllowsMultipleInstanceTypes (..),

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

    -- ** DeviceType
    DeviceType (..),

    -- ** DiskImageFormat
    DiskImageFormat (..),

    -- ** DiskType
    DiskType (..),

    -- ** DnsNameState
    DnsNameState (..),

    -- ** DnsSupportValue
    DnsSupportValue (..),

    -- ** DomainType
    DomainType (..),

    -- ** EbsEncryptionSupport
    EbsEncryptionSupport (..),

    -- ** EbsNvmeSupport
    EbsNvmeSupport (..),

    -- ** EbsOptimizedSupport
    EbsOptimizedSupport (..),

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

    -- ** FastSnapshotRestoreStateCode
    FastSnapshotRestoreStateCode (..),

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

    -- ** GatewayType
    GatewayType (..),

    -- ** HostRecovery
    HostRecovery (..),

    -- ** HostTenancy
    HostTenancy (..),

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

    -- ** InstanceAttributeName
    InstanceAttributeName (..),

    -- ** InstanceEventWindowState
    InstanceEventWindowState (..),

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

    -- ** Ipv6SupportValue
    Ipv6SupportValue (..),

    -- ** KeyType
    KeyType (..),

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

    -- ** ListingState
    ListingState (..),

    -- ** ListingStatus
    ListingStatus (..),

    -- ** LocalGatewayRouteState
    LocalGatewayRouteState (..),

    -- ** LocalGatewayRouteType
    LocalGatewayRouteType (..),

    -- ** LocationType
    LocationType (..),

    -- ** LogDestinationType
    LogDestinationType (..),

    -- ** MarketType
    MarketType (..),

    -- ** MembershipType
    MembershipType (..),

    -- ** ModifyAvailabilityZoneOptInStatus
    ModifyAvailabilityZoneOptInStatus (..),

    -- ** MonitoringState
    MonitoringState (..),

    -- ** MoveStatus
    MoveStatus (..),

    -- ** MulticastSupportValue
    MulticastSupportValue (..),

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

    -- ** PaymentOption
    PaymentOption (..),

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

    -- ** State
    State (..),

    -- ** StaticSourcesSupportValue
    StaticSourcesSupportValue (..),

    -- ** StatusName
    StatusName (..),

    -- ** StatusType
    StatusType (..),

    -- ** SubnetCidrBlockStateCode
    SubnetCidrBlockStateCode (..),

    -- ** SubnetCidrReservationType
    SubnetCidrReservationType (..),

    -- ** SubnetState
    SubnetState (..),

    -- ** SummaryStatus
    SummaryStatus (..),

    -- ** TelemetryStatus
    TelemetryStatus (..),

    -- ** Tenancy
    Tenancy (..),

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

    -- ** TransitGatewayPrefixListReferenceState
    TransitGatewayPrefixListReferenceState (..),

    -- ** TransitGatewayPropagationState
    TransitGatewayPropagationState (..),

    -- ** TransitGatewayRouteState
    TransitGatewayRouteState (..),

    -- ** TransitGatewayRouteTableState
    TransitGatewayRouteTableState (..),

    -- ** TransitGatewayRouteType
    TransitGatewayRouteType (..),

    -- ** TransitGatewayState
    TransitGatewayState (..),

    -- ** TransportProtocol
    TransportProtocol (..),

    -- ** TunnelInsideIpVersion
    TunnelInsideIpVersion (..),

    -- ** UnlimitedSupportedInstanceFamily
    UnlimitedSupportedInstanceFamily (..),

    -- ** UnsuccessfulInstanceCreditSpecificationErrorCode
    UnsuccessfulInstanceCreditSpecificationErrorCode (..),

    -- ** UsageClassType
    UsageClassType (..),

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

    -- ** AccountAttribute
    AccountAttribute (AccountAttribute'),
    newAccountAttribute,

    -- ** AccountAttributeValue
    AccountAttributeValue (AccountAttributeValue'),
    newAccountAttributeValue,

    -- ** ActiveInstance
    ActiveInstance (ActiveInstance'),
    newActiveInstance,

    -- ** AddPrefixListEntry
    AddPrefixListEntry (AddPrefixListEntry'),
    newAddPrefixListEntry,

    -- ** Address
    Address (Address'),
    newAddress,

    -- ** AddressAttribute
    AddressAttribute (AddressAttribute'),
    newAddressAttribute,

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

    -- ** CoipAddressUsage
    CoipAddressUsage (CoipAddressUsage'),
    newCoipAddressUsage,

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

    -- ** CreateTransitGatewayVpcAttachmentRequestOptions
    CreateTransitGatewayVpcAttachmentRequestOptions (CreateTransitGatewayVpcAttachmentRequestOptions'),
    newCreateTransitGatewayVpcAttachmentRequestOptions,

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

    -- ** FederatedAuthentication
    FederatedAuthentication (FederatedAuthentication'),
    newFederatedAuthentication,

    -- ** FederatedAuthenticationRequest
    FederatedAuthenticationRequest (FederatedAuthenticationRequest'),
    newFederatedAuthenticationRequest,

    -- ** Filter
    Filter (Filter'),
    newFilter,

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

    -- ** ManagedPrefixList
    ManagedPrefixList (ManagedPrefixList'),
    newManagedPrefixList,

    -- ** MemoryInfo
    MemoryInfo (MemoryInfo'),
    newMemoryInfo,

    -- ** ModifyTransitGatewayOptions
    ModifyTransitGatewayOptions (ModifyTransitGatewayOptions'),
    newModifyTransitGatewayOptions,

    -- ** ModifyTransitGatewayVpcAttachmentRequestOptions
    ModifyTransitGatewayVpcAttachmentRequestOptions (ModifyTransitGatewayVpcAttachmentRequestOptions'),
    newModifyTransitGatewayVpcAttachmentRequestOptions,

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

    -- ** NetworkCardInfo
    NetworkCardInfo (NetworkCardInfo'),
    newNetworkCardInfo,

    -- ** NetworkInfo
    NetworkInfo (NetworkInfo'),
    newNetworkInfo,

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

    -- ** OnDemandOptions
    OnDemandOptions (OnDemandOptions'),
    newOnDemandOptions,

    -- ** OnDemandOptionsRequest
    OnDemandOptionsRequest (OnDemandOptionsRequest'),
    newOnDemandOptionsRequest,

    -- ** PathComponent
    PathComponent (PathComponent'),
    newPathComponent,

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

    -- ** RemovePrefixListEntry
    RemovePrefixListEntry (RemovePrefixListEntry'),
    newRemovePrefixListEntry,

    -- ** ReplaceRootVolumeTask
    ReplaceRootVolumeTask (ReplaceRootVolumeTask'),
    newReplaceRootVolumeTask,

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

    -- ** SnapshotTaskDetail
    SnapshotTaskDetail (SnapshotTaskDetail'),
    newSnapshotTaskDetail,

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

    -- ** TransitGatewayRouteTableAssociation
    TransitGatewayRouteTableAssociation (TransitGatewayRouteTableAssociation'),
    newTransitGatewayRouteTableAssociation,

    -- ** TransitGatewayRouteTablePropagation
    TransitGatewayRouteTablePropagation (TransitGatewayRouteTablePropagation'),
    newTransitGatewayRouteTablePropagation,

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

    -- ** VCpuInfo
    VCpuInfo (VCpuInfo'),
    newVCpuInfo,

    -- ** ValidationError
    ValidationError (ValidationError'),
    newValidationError,

    -- ** ValidationWarning
    ValidationWarning (ValidationWarning'),
    newValidationWarning,

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

    -- ** VpnTunnelOptionsSpecification
    VpnTunnelOptionsSpecification (VpnTunnelOptionsSpecification'),
    newVpnTunnelOptionsSpecification,
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
import Network.AWS.EC2.AssociateInstanceEventWindow
import Network.AWS.EC2.AssociateRouteTable
import Network.AWS.EC2.AssociateSubnetCidrBlock
import Network.AWS.EC2.AssociateTransitGatewayMulticastDomain
import Network.AWS.EC2.AssociateTransitGatewayRouteTable
import Network.AWS.EC2.AssociateTrunkInterface
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
import Network.AWS.EC2.CancelCapacityReservationFleets
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
import Network.AWS.EC2.CreateCapacityReservationFleet
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
import Network.AWS.EC2.CreateInstanceEventWindow
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
import Network.AWS.EC2.CreateReplaceRootVolumeTask
import Network.AWS.EC2.CreateReservedInstancesListing
import Network.AWS.EC2.CreateRestoreImageTask
import Network.AWS.EC2.CreateRoute
import Network.AWS.EC2.CreateRouteTable
import Network.AWS.EC2.CreateSecurityGroup
import Network.AWS.EC2.CreateSnapshot
import Network.AWS.EC2.CreateSnapshots
import Network.AWS.EC2.CreateSpotDatafeedSubscription
import Network.AWS.EC2.CreateStoreImageTask
import Network.AWS.EC2.CreateSubnet
import Network.AWS.EC2.CreateSubnetCidrReservation
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
import Network.AWS.EC2.DeleteInstanceEventWindow
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
import Network.AWS.EC2.DeleteSubnetCidrReservation
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
import Network.AWS.EC2.DescribeCapacityReservationFleets
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
import Network.AWS.EC2.DescribeInstanceEventWindows
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
import Network.AWS.EC2.DescribeReplaceRootVolumeTasks
import Network.AWS.EC2.DescribeReservedInstances
import Network.AWS.EC2.DescribeReservedInstancesListings
import Network.AWS.EC2.DescribeReservedInstancesModifications
import Network.AWS.EC2.DescribeReservedInstancesOfferings
import Network.AWS.EC2.DescribeRouteTables
import Network.AWS.EC2.DescribeScheduledInstanceAvailability
import Network.AWS.EC2.DescribeScheduledInstances
import Network.AWS.EC2.DescribeSecurityGroupReferences
import Network.AWS.EC2.DescribeSecurityGroupRules
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
import Network.AWS.EC2.DescribeStoreImageTasks
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
import Network.AWS.EC2.DescribeTrunkInterfaceAssociations
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
import Network.AWS.EC2.DisableImageDeprecation
import Network.AWS.EC2.DisableSerialConsoleAccess
import Network.AWS.EC2.DisableTransitGatewayRouteTablePropagation
import Network.AWS.EC2.DisableVgwRoutePropagation
import Network.AWS.EC2.DisableVpcClassicLink
import Network.AWS.EC2.DisableVpcClassicLinkDnsSupport
import Network.AWS.EC2.DisassociateAddress
import Network.AWS.EC2.DisassociateClientVpnTargetNetwork
import Network.AWS.EC2.DisassociateEnclaveCertificateIamRole
import Network.AWS.EC2.DisassociateIamInstanceProfile
import Network.AWS.EC2.DisassociateInstanceEventWindow
import Network.AWS.EC2.DisassociateRouteTable
import Network.AWS.EC2.DisassociateSubnetCidrBlock
import Network.AWS.EC2.DisassociateTransitGatewayMulticastDomain
import Network.AWS.EC2.DisassociateTransitGatewayRouteTable
import Network.AWS.EC2.DisassociateTrunkInterface
import Network.AWS.EC2.DisassociateVpcCidrBlock
import Network.AWS.EC2.EnableEbsEncryptionByDefault
import Network.AWS.EC2.EnableFastSnapshotRestores
import Network.AWS.EC2.EnableImageDeprecation
import Network.AWS.EC2.EnableSerialConsoleAccess
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
import Network.AWS.EC2.GetFlowLogsIntegrationTemplate
import Network.AWS.EC2.GetGroupsForCapacityReservation
import Network.AWS.EC2.GetHostReservationPurchasePreview
import Network.AWS.EC2.GetLaunchTemplateData
import Network.AWS.EC2.GetManagedPrefixListAssociations
import Network.AWS.EC2.GetManagedPrefixListEntries
import Network.AWS.EC2.GetPasswordData
import Network.AWS.EC2.GetReservedInstancesExchangeQuote
import Network.AWS.EC2.GetSerialConsoleAccessStatus
import Network.AWS.EC2.GetSubnetCidrReservations
import Network.AWS.EC2.GetTransitGatewayAttachmentPropagations
import Network.AWS.EC2.GetTransitGatewayMulticastDomainAssociations
import Network.AWS.EC2.GetTransitGatewayPrefixListReferences
import Network.AWS.EC2.GetTransitGatewayRouteTableAssociations
import Network.AWS.EC2.GetTransitGatewayRouteTablePropagations
import Network.AWS.EC2.GetVpnConnectionDeviceSampleConfiguration
import Network.AWS.EC2.GetVpnConnectionDeviceTypes
import Network.AWS.EC2.ImportClientVpnClientCertificateRevocationList
import Network.AWS.EC2.ImportImage
import Network.AWS.EC2.ImportInstance
import Network.AWS.EC2.ImportKeyPair
import Network.AWS.EC2.ImportSnapshot
import Network.AWS.EC2.ImportVolume
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Lens
import Network.AWS.EC2.ModifyAddressAttribute
import Network.AWS.EC2.ModifyAvailabilityZoneGroup
import Network.AWS.EC2.ModifyCapacityReservation
import Network.AWS.EC2.ModifyCapacityReservationFleet
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
import Network.AWS.EC2.ModifyInstanceEventWindow
import Network.AWS.EC2.ModifyInstanceMetadataOptions
import Network.AWS.EC2.ModifyInstancePlacement
import Network.AWS.EC2.ModifyLaunchTemplate
import Network.AWS.EC2.ModifyManagedPrefixList
import Network.AWS.EC2.ModifyNetworkInterfaceAttribute
import Network.AWS.EC2.ModifyReservedInstances
import Network.AWS.EC2.ModifySecurityGroupRules
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
import Network.AWS.EC2.Types
import Network.AWS.EC2.UnassignIpv6Addresses
import Network.AWS.EC2.UnassignPrivateIpAddresses
import Network.AWS.EC2.UnmonitorInstances
import Network.AWS.EC2.UpdateSecurityGroupRuleDescriptionsEgress
import Network.AWS.EC2.UpdateSecurityGroupRuleDescriptionsIngress
import Network.AWS.EC2.Waiters
import Network.AWS.EC2.WithdrawByoipCidr

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
