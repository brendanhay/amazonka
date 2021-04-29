{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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

    -- ** VpcPeeringConnectionDeleted
    newVpcPeeringConnectionDeleted,

    -- ** ExportTaskCompleted
    newExportTaskCompleted,

    -- ** SnapshotCompleted
    newSnapshotCompleted,

    -- ** SpotInstanceRequestFulfilled
    newSpotInstanceRequestFulfilled,

    -- ** VolumeAvailable
    newVolumeAvailable,

    -- ** ImageAvailable
    newImageAvailable,

    -- ** PasswordDataAvailable
    newPasswordDataAvailable,

    -- ** InstanceRunning
    newInstanceRunning,

    -- ** KeyPairExists
    newKeyPairExists,

    -- ** ExportTaskCancelled
    newExportTaskCancelled,

    -- ** VpnConnectionAvailable
    newVpnConnectionAvailable,

    -- ** ImageExists
    newImageExists,

    -- ** VpcAvailable
    newVpcAvailable,

    -- ** VolumeInUse
    newVolumeInUse,

    -- ** InstanceExists
    newInstanceExists,

    -- ** VpcPeeringConnectionExists
    newVpcPeeringConnectionExists,

    -- ** InstanceStatusOk
    newInstanceStatusOk,

    -- ** SecurityGroupExists
    newSecurityGroupExists,

    -- ** ConversionTaskCancelled
    newConversionTaskCancelled,

    -- ** VpnConnectionDeleted
    newVpnConnectionDeleted,

    -- ** InstanceStopped
    newInstanceStopped,

    -- ** ConversionTaskCompleted
    newConversionTaskCompleted,

    -- ** BundleTaskComplete
    newBundleTaskComplete,

    -- ** ConversionTaskDeleted
    newConversionTaskDeleted,

    -- ** VolumeDeleted
    newVolumeDeleted,

    -- ** CustomerGatewayAvailable
    newCustomerGatewayAvailable,

    -- ** VpcExists
    newVpcExists,

    -- ** SystemStatusOk
    newSystemStatusOk,

    -- ** NetworkInterfaceAvailable
    newNetworkInterfaceAvailable,

    -- ** NatGatewayAvailable
    newNatGatewayAvailable,

    -- ** SubnetAvailable
    newSubnetAvailable,

    -- * Operations
    -- $operations

    -- ** AcceptReservedInstancesExchangeQuote
    AcceptReservedInstancesExchangeQuote (AcceptReservedInstancesExchangeQuote'),
    newAcceptReservedInstancesExchangeQuote,
    AcceptReservedInstancesExchangeQuoteResponse (AcceptReservedInstancesExchangeQuoteResponse'),
    newAcceptReservedInstancesExchangeQuoteResponse,

    -- ** DescribeInstanceCreditSpecifications (Paginated)
    DescribeInstanceCreditSpecifications (DescribeInstanceCreditSpecifications'),
    newDescribeInstanceCreditSpecifications,
    DescribeInstanceCreditSpecificationsResponse (DescribeInstanceCreditSpecificationsResponse'),
    newDescribeInstanceCreditSpecificationsResponse,

    -- ** DescribeByoipCidrs (Paginated)
    DescribeByoipCidrs (DescribeByoipCidrs'),
    newDescribeByoipCidrs,
    DescribeByoipCidrsResponse (DescribeByoipCidrsResponse'),
    newDescribeByoipCidrsResponse,

    -- ** DescribeLocalGatewayVirtualInterfaces (Paginated)
    DescribeLocalGatewayVirtualInterfaces (DescribeLocalGatewayVirtualInterfaces'),
    newDescribeLocalGatewayVirtualInterfaces,
    DescribeLocalGatewayVirtualInterfacesResponse (DescribeLocalGatewayVirtualInterfacesResponse'),
    newDescribeLocalGatewayVirtualInterfacesResponse,

    -- ** DeleteLocalGatewayRouteTableVpcAssociation
    DeleteLocalGatewayRouteTableVpcAssociation (DeleteLocalGatewayRouteTableVpcAssociation'),
    newDeleteLocalGatewayRouteTableVpcAssociation,
    DeleteLocalGatewayRouteTableVpcAssociationResponse (DeleteLocalGatewayRouteTableVpcAssociationResponse'),
    newDeleteLocalGatewayRouteTableVpcAssociationResponse,

    -- ** DetachVolume
    DetachVolume (DetachVolume'),
    newDetachVolume,
    VolumeAttachment (VolumeAttachment'),
    newVolumeAttachment,

    -- ** CreateTransitGatewayVpcAttachment
    CreateTransitGatewayVpcAttachment (CreateTransitGatewayVpcAttachment'),
    newCreateTransitGatewayVpcAttachment,
    CreateTransitGatewayVpcAttachmentResponse (CreateTransitGatewayVpcAttachmentResponse'),
    newCreateTransitGatewayVpcAttachmentResponse,

    -- ** DeleteVpcEndpointConnectionNotifications
    DeleteVpcEndpointConnectionNotifications (DeleteVpcEndpointConnectionNotifications'),
    newDeleteVpcEndpointConnectionNotifications,
    DeleteVpcEndpointConnectionNotificationsResponse (DeleteVpcEndpointConnectionNotificationsResponse'),
    newDeleteVpcEndpointConnectionNotificationsResponse,

    -- ** DeleteNetworkInsightsPath
    DeleteNetworkInsightsPath (DeleteNetworkInsightsPath'),
    newDeleteNetworkInsightsPath,
    DeleteNetworkInsightsPathResponse (DeleteNetworkInsightsPathResponse'),
    newDeleteNetworkInsightsPathResponse,

    -- ** AuthorizeSecurityGroupEgress
    AuthorizeSecurityGroupEgress (AuthorizeSecurityGroupEgress'),
    newAuthorizeSecurityGroupEgress,
    AuthorizeSecurityGroupEgressResponse (AuthorizeSecurityGroupEgressResponse'),
    newAuthorizeSecurityGroupEgressResponse,

    -- ** ModifyManagedPrefixList
    ModifyManagedPrefixList (ModifyManagedPrefixList'),
    newModifyManagedPrefixList,
    ModifyManagedPrefixListResponse (ModifyManagedPrefixListResponse'),
    newModifyManagedPrefixListResponse,

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

    -- ** DescribeVpcPeeringConnections (Paginated)
    DescribeVpcPeeringConnections (DescribeVpcPeeringConnections'),
    newDescribeVpcPeeringConnections,
    DescribeVpcPeeringConnectionsResponse (DescribeVpcPeeringConnectionsResponse'),
    newDescribeVpcPeeringConnectionsResponse,

    -- ** DescribeInstances (Paginated)
    DescribeInstances (DescribeInstances'),
    newDescribeInstances,
    DescribeInstancesResponse (DescribeInstancesResponse'),
    newDescribeInstancesResponse,

    -- ** DeregisterInstanceEventNotificationAttributes
    DeregisterInstanceEventNotificationAttributes (DeregisterInstanceEventNotificationAttributes'),
    newDeregisterInstanceEventNotificationAttributes,
    DeregisterInstanceEventNotificationAttributesResponse (DeregisterInstanceEventNotificationAttributesResponse'),
    newDeregisterInstanceEventNotificationAttributesResponse,

    -- ** CreateTransitGatewayMulticastDomain
    CreateTransitGatewayMulticastDomain (CreateTransitGatewayMulticastDomain'),
    newCreateTransitGatewayMulticastDomain,
    CreateTransitGatewayMulticastDomainResponse (CreateTransitGatewayMulticastDomainResponse'),
    newCreateTransitGatewayMulticastDomainResponse,

    -- ** AssociateTransitGatewayMulticastDomain
    AssociateTransitGatewayMulticastDomain (AssociateTransitGatewayMulticastDomain'),
    newAssociateTransitGatewayMulticastDomain,
    AssociateTransitGatewayMulticastDomainResponse (AssociateTransitGatewayMulticastDomainResponse'),
    newAssociateTransitGatewayMulticastDomainResponse,

    -- ** ReleaseAddress
    ReleaseAddress (ReleaseAddress'),
    newReleaseAddress,
    ReleaseAddressResponse (ReleaseAddressResponse'),
    newReleaseAddressResponse,

    -- ** DeregisterTransitGatewayMulticastGroupMembers
    DeregisterTransitGatewayMulticastGroupMembers (DeregisterTransitGatewayMulticastGroupMembers'),
    newDeregisterTransitGatewayMulticastGroupMembers,
    DeregisterTransitGatewayMulticastGroupMembersResponse (DeregisterTransitGatewayMulticastGroupMembersResponse'),
    newDeregisterTransitGatewayMulticastGroupMembersResponse,

    -- ** GetHostReservationPurchasePreview
    GetHostReservationPurchasePreview (GetHostReservationPurchasePreview'),
    newGetHostReservationPurchasePreview,
    GetHostReservationPurchasePreviewResponse (GetHostReservationPurchasePreviewResponse'),
    newGetHostReservationPurchasePreviewResponse,

    -- ** CancelBundleTask
    CancelBundleTask (CancelBundleTask'),
    newCancelBundleTask,
    CancelBundleTaskResponse (CancelBundleTaskResponse'),
    newCancelBundleTaskResponse,

    -- ** GetCapacityReservationUsage
    GetCapacityReservationUsage (GetCapacityReservationUsage'),
    newGetCapacityReservationUsage,
    GetCapacityReservationUsageResponse (GetCapacityReservationUsageResponse'),
    newGetCapacityReservationUsageResponse,

    -- ** CreateTransitGatewayConnectPeer
    CreateTransitGatewayConnectPeer (CreateTransitGatewayConnectPeer'),
    newCreateTransitGatewayConnectPeer,
    CreateTransitGatewayConnectPeerResponse (CreateTransitGatewayConnectPeerResponse'),
    newCreateTransitGatewayConnectPeerResponse,

    -- ** ModifyVpcTenancy
    ModifyVpcTenancy (ModifyVpcTenancy'),
    newModifyVpcTenancy,
    ModifyVpcTenancyResponse (ModifyVpcTenancyResponse'),
    newModifyVpcTenancyResponse,

    -- ** CreateVpcEndpointServiceConfiguration
    CreateVpcEndpointServiceConfiguration (CreateVpcEndpointServiceConfiguration'),
    newCreateVpcEndpointServiceConfiguration,
    CreateVpcEndpointServiceConfigurationResponse (CreateVpcEndpointServiceConfigurationResponse'),
    newCreateVpcEndpointServiceConfigurationResponse,

    -- ** DescribeExportTasks
    DescribeExportTasks (DescribeExportTasks'),
    newDescribeExportTasks,
    DescribeExportTasksResponse (DescribeExportTasksResponse'),
    newDescribeExportTasksResponse,

    -- ** GetTransitGatewayMulticastDomainAssociations (Paginated)
    GetTransitGatewayMulticastDomainAssociations (GetTransitGatewayMulticastDomainAssociations'),
    newGetTransitGatewayMulticastDomainAssociations,
    GetTransitGatewayMulticastDomainAssociationsResponse (GetTransitGatewayMulticastDomainAssociationsResponse'),
    newGetTransitGatewayMulticastDomainAssociationsResponse,

    -- ** DisableEbsEncryptionByDefault
    DisableEbsEncryptionByDefault (DisableEbsEncryptionByDefault'),
    newDisableEbsEncryptionByDefault,
    DisableEbsEncryptionByDefaultResponse (DisableEbsEncryptionByDefaultResponse'),
    newDisableEbsEncryptionByDefaultResponse,

    -- ** AssociateVpcCidrBlock
    AssociateVpcCidrBlock (AssociateVpcCidrBlock'),
    newAssociateVpcCidrBlock,
    AssociateVpcCidrBlockResponse (AssociateVpcCidrBlockResponse'),
    newAssociateVpcCidrBlockResponse,

    -- ** CreateNetworkAcl
    CreateNetworkAcl (CreateNetworkAcl'),
    newCreateNetworkAcl,
    CreateNetworkAclResponse (CreateNetworkAclResponse'),
    newCreateNetworkAclResponse,

    -- ** AcceptTransitGatewayPeeringAttachment
    AcceptTransitGatewayPeeringAttachment (AcceptTransitGatewayPeeringAttachment'),
    newAcceptTransitGatewayPeeringAttachment,
    AcceptTransitGatewayPeeringAttachmentResponse (AcceptTransitGatewayPeeringAttachmentResponse'),
    newAcceptTransitGatewayPeeringAttachmentResponse,

    -- ** DeleteLaunchTemplate
    DeleteLaunchTemplate (DeleteLaunchTemplate'),
    newDeleteLaunchTemplate,
    DeleteLaunchTemplateResponse (DeleteLaunchTemplateResponse'),
    newDeleteLaunchTemplateResponse,

    -- ** DeleteVpc
    DeleteVpc (DeleteVpc'),
    newDeleteVpc,
    DeleteVpcResponse (DeleteVpcResponse'),
    newDeleteVpcResponse,

    -- ** DeleteFleets
    DeleteFleets (DeleteFleets'),
    newDeleteFleets,
    DeleteFleetsResponse (DeleteFleetsResponse'),
    newDeleteFleetsResponse,

    -- ** GetAssociatedIpv6PoolCidrs (Paginated)
    GetAssociatedIpv6PoolCidrs (GetAssociatedIpv6PoolCidrs'),
    newGetAssociatedIpv6PoolCidrs,
    GetAssociatedIpv6PoolCidrsResponse (GetAssociatedIpv6PoolCidrsResponse'),
    newGetAssociatedIpv6PoolCidrsResponse,

    -- ** DescribeTrafficMirrorSessions (Paginated)
    DescribeTrafficMirrorSessions (DescribeTrafficMirrorSessions'),
    newDescribeTrafficMirrorSessions,
    DescribeTrafficMirrorSessionsResponse (DescribeTrafficMirrorSessionsResponse'),
    newDescribeTrafficMirrorSessionsResponse,

    -- ** ImportInstance
    ImportInstance (ImportInstance'),
    newImportInstance,
    ImportInstanceResponse (ImportInstanceResponse'),
    newImportInstanceResponse,

    -- ** DescribeLocalGatewayRouteTables (Paginated)
    DescribeLocalGatewayRouteTables (DescribeLocalGatewayRouteTables'),
    newDescribeLocalGatewayRouteTables,
    DescribeLocalGatewayRouteTablesResponse (DescribeLocalGatewayRouteTablesResponse'),
    newDescribeLocalGatewayRouteTablesResponse,

    -- ** CreateNetworkInterfacePermission
    CreateNetworkInterfacePermission (CreateNetworkInterfacePermission'),
    newCreateNetworkInterfacePermission,
    CreateNetworkInterfacePermissionResponse (CreateNetworkInterfacePermissionResponse'),
    newCreateNetworkInterfacePermissionResponse,

    -- ** CreateVpnGateway
    CreateVpnGateway (CreateVpnGateway'),
    newCreateVpnGateway,
    CreateVpnGatewayResponse (CreateVpnGatewayResponse'),
    newCreateVpnGatewayResponse,

    -- ** GetTransitGatewayRouteTableAssociations (Paginated)
    GetTransitGatewayRouteTableAssociations (GetTransitGatewayRouteTableAssociations'),
    newGetTransitGatewayRouteTableAssociations,
    GetTransitGatewayRouteTableAssociationsResponse (GetTransitGatewayRouteTableAssociationsResponse'),
    newGetTransitGatewayRouteTableAssociationsResponse,

    -- ** RejectTransitGatewayVpcAttachment
    RejectTransitGatewayVpcAttachment (RejectTransitGatewayVpcAttachment'),
    newRejectTransitGatewayVpcAttachment,
    RejectTransitGatewayVpcAttachmentResponse (RejectTransitGatewayVpcAttachmentResponse'),
    newRejectTransitGatewayVpcAttachmentResponse,

    -- ** ModifyVolumeAttribute
    ModifyVolumeAttribute (ModifyVolumeAttribute'),
    newModifyVolumeAttribute,
    ModifyVolumeAttributeResponse (ModifyVolumeAttributeResponse'),
    newModifyVolumeAttributeResponse,

    -- ** DescribePrefixLists (Paginated)
    DescribePrefixLists (DescribePrefixLists'),
    newDescribePrefixLists,
    DescribePrefixListsResponse (DescribePrefixListsResponse'),
    newDescribePrefixListsResponse,

    -- ** DetachNetworkInterface
    DetachNetworkInterface (DetachNetworkInterface'),
    newDetachNetworkInterface,
    DetachNetworkInterfaceResponse (DetachNetworkInterfaceResponse'),
    newDetachNetworkInterfaceResponse,

    -- ** DeleteVpcEndpoints
    DeleteVpcEndpoints (DeleteVpcEndpoints'),
    newDeleteVpcEndpoints,
    DeleteVpcEndpointsResponse (DeleteVpcEndpointsResponse'),
    newDeleteVpcEndpointsResponse,

    -- ** DescribeVpcClassicLink
    DescribeVpcClassicLink (DescribeVpcClassicLink'),
    newDescribeVpcClassicLink,
    DescribeVpcClassicLinkResponse (DescribeVpcClassicLinkResponse'),
    newDescribeVpcClassicLinkResponse,

    -- ** UpdateSecurityGroupRuleDescriptionsIngress
    UpdateSecurityGroupRuleDescriptionsIngress (UpdateSecurityGroupRuleDescriptionsIngress'),
    newUpdateSecurityGroupRuleDescriptionsIngress,
    UpdateSecurityGroupRuleDescriptionsIngressResponse (UpdateSecurityGroupRuleDescriptionsIngressResponse'),
    newUpdateSecurityGroupRuleDescriptionsIngressResponse,

    -- ** DescribeClientVpnEndpoints (Paginated)
    DescribeClientVpnEndpoints (DescribeClientVpnEndpoints'),
    newDescribeClientVpnEndpoints,
    DescribeClientVpnEndpointsResponse (DescribeClientVpnEndpointsResponse'),
    newDescribeClientVpnEndpointsResponse,

    -- ** DisassociateAddress
    DisassociateAddress (DisassociateAddress'),
    newDisassociateAddress,
    DisassociateAddressResponse (DisassociateAddressResponse'),
    newDisassociateAddressResponse,

    -- ** DescribeScheduledInstanceAvailability (Paginated)
    DescribeScheduledInstanceAvailability (DescribeScheduledInstanceAvailability'),
    newDescribeScheduledInstanceAvailability,
    DescribeScheduledInstanceAvailabilityResponse (DescribeScheduledInstanceAvailabilityResponse'),
    newDescribeScheduledInstanceAvailabilityResponse,

    -- ** RejectVpcEndpointConnections
    RejectVpcEndpointConnections (RejectVpcEndpointConnections'),
    newRejectVpcEndpointConnections,
    RejectVpcEndpointConnectionsResponse (RejectVpcEndpointConnectionsResponse'),
    newRejectVpcEndpointConnectionsResponse,

    -- ** CreateTransitGatewayRouteTable
    CreateTransitGatewayRouteTable (CreateTransitGatewayRouteTable'),
    newCreateTransitGatewayRouteTable,
    CreateTransitGatewayRouteTableResponse (CreateTransitGatewayRouteTableResponse'),
    newCreateTransitGatewayRouteTableResponse,

    -- ** DescribeTags (Paginated)
    DescribeTags (DescribeTags'),
    newDescribeTags,
    DescribeTagsResponse (DescribeTagsResponse'),
    newDescribeTagsResponse,

    -- ** AssociateTransitGatewayRouteTable
    AssociateTransitGatewayRouteTable (AssociateTransitGatewayRouteTable'),
    newAssociateTransitGatewayRouteTable,
    AssociateTransitGatewayRouteTableResponse (AssociateTransitGatewayRouteTableResponse'),
    newAssociateTransitGatewayRouteTableResponse,

    -- ** DeleteFlowLogs
    DeleteFlowLogs (DeleteFlowLogs'),
    newDeleteFlowLogs,
    DeleteFlowLogsResponse (DeleteFlowLogsResponse'),
    newDeleteFlowLogsResponse,

    -- ** CreateDefaultSubnet
    CreateDefaultSubnet (CreateDefaultSubnet'),
    newCreateDefaultSubnet,
    CreateDefaultSubnetResponse (CreateDefaultSubnetResponse'),
    newCreateDefaultSubnetResponse,

    -- ** DeleteTrafficMirrorTarget
    DeleteTrafficMirrorTarget (DeleteTrafficMirrorTarget'),
    newDeleteTrafficMirrorTarget,
    DeleteTrafficMirrorTargetResponse (DeleteTrafficMirrorTargetResponse'),
    newDeleteTrafficMirrorTargetResponse,

    -- ** AcceptTransitGatewayMulticastDomainAssociations
    AcceptTransitGatewayMulticastDomainAssociations (AcceptTransitGatewayMulticastDomainAssociations'),
    newAcceptTransitGatewayMulticastDomainAssociations,
    AcceptTransitGatewayMulticastDomainAssociationsResponse (AcceptTransitGatewayMulticastDomainAssociationsResponse'),
    newAcceptTransitGatewayMulticastDomainAssociationsResponse,

    -- ** DescribeLaunchTemplateVersions (Paginated)
    DescribeLaunchTemplateVersions (DescribeLaunchTemplateVersions'),
    newDescribeLaunchTemplateVersions,
    DescribeLaunchTemplateVersionsResponse (DescribeLaunchTemplateVersionsResponse'),
    newDescribeLaunchTemplateVersionsResponse,

    -- ** DescribeAvailabilityZones
    DescribeAvailabilityZones (DescribeAvailabilityZones'),
    newDescribeAvailabilityZones,
    DescribeAvailabilityZonesResponse (DescribeAvailabilityZonesResponse'),
    newDescribeAvailabilityZonesResponse,

    -- ** GetReservedInstancesExchangeQuote
    GetReservedInstancesExchangeQuote (GetReservedInstancesExchangeQuote'),
    newGetReservedInstancesExchangeQuote,
    GetReservedInstancesExchangeQuoteResponse (GetReservedInstancesExchangeQuoteResponse'),
    newGetReservedInstancesExchangeQuoteResponse,

    -- ** DeleteVpnGateway
    DeleteVpnGateway (DeleteVpnGateway'),
    newDeleteVpnGateway,
    DeleteVpnGatewayResponse (DeleteVpnGatewayResponse'),
    newDeleteVpnGatewayResponse,

    -- ** CreateKeyPair
    CreateKeyPair (CreateKeyPair'),
    newCreateKeyPair,
    CreateKeyPairResponse (CreateKeyPairResponse'),
    newCreateKeyPairResponse,

    -- ** ExportTransitGatewayRoutes
    ExportTransitGatewayRoutes (ExportTransitGatewayRoutes'),
    newExportTransitGatewayRoutes,
    ExportTransitGatewayRoutesResponse (ExportTransitGatewayRoutesResponse'),
    newExportTransitGatewayRoutesResponse,

    -- ** CopySnapshot
    CopySnapshot (CopySnapshot'),
    newCopySnapshot,
    CopySnapshotResponse (CopySnapshotResponse'),
    newCopySnapshotResponse,

    -- ** DescribeElasticGpus
    DescribeElasticGpus (DescribeElasticGpus'),
    newDescribeElasticGpus,
    DescribeElasticGpusResponse (DescribeElasticGpusResponse'),
    newDescribeElasticGpusResponse,

    -- ** StartNetworkInsightsAnalysis
    StartNetworkInsightsAnalysis (StartNetworkInsightsAnalysis'),
    newStartNetworkInsightsAnalysis,
    StartNetworkInsightsAnalysisResponse (StartNetworkInsightsAnalysisResponse'),
    newStartNetworkInsightsAnalysisResponse,

    -- ** DescribeFpgaImages (Paginated)
    DescribeFpgaImages (DescribeFpgaImages'),
    newDescribeFpgaImages,
    DescribeFpgaImagesResponse (DescribeFpgaImagesResponse'),
    newDescribeFpgaImagesResponse,

    -- ** CreateFlowLogs
    CreateFlowLogs (CreateFlowLogs'),
    newCreateFlowLogs,
    CreateFlowLogsResponse (CreateFlowLogsResponse'),
    newCreateFlowLogsResponse,

    -- ** CreateLaunchTemplate
    CreateLaunchTemplate (CreateLaunchTemplate'),
    newCreateLaunchTemplate,
    CreateLaunchTemplateResponse (CreateLaunchTemplateResponse'),
    newCreateLaunchTemplateResponse,

    -- ** DescribeImportImageTasks (Paginated)
    DescribeImportImageTasks (DescribeImportImageTasks'),
    newDescribeImportImageTasks,
    DescribeImportImageTasksResponse (DescribeImportImageTasksResponse'),
    newDescribeImportImageTasksResponse,

    -- ** DeleteTransitGatewayRouteTable
    DeleteTransitGatewayRouteTable (DeleteTransitGatewayRouteTable'),
    newDeleteTransitGatewayRouteTable,
    DeleteTransitGatewayRouteTableResponse (DeleteTransitGatewayRouteTableResponse'),
    newDeleteTransitGatewayRouteTableResponse,

    -- ** DeleteNetworkAcl
    DeleteNetworkAcl (DeleteNetworkAcl'),
    newDeleteNetworkAcl,
    DeleteNetworkAclResponse (DeleteNetworkAclResponse'),
    newDeleteNetworkAclResponse,

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

    -- ** RestoreAddressToClassic
    RestoreAddressToClassic (RestoreAddressToClassic'),
    newRestoreAddressToClassic,
    RestoreAddressToClassicResponse (RestoreAddressToClassicResponse'),
    newRestoreAddressToClassicResponse,

    -- ** DeleteNetworkInterfacePermission
    DeleteNetworkInterfacePermission (DeleteNetworkInterfacePermission'),
    newDeleteNetworkInterfacePermission,
    DeleteNetworkInterfacePermissionResponse (DeleteNetworkInterfacePermissionResponse'),
    newDeleteNetworkInterfacePermissionResponse,

    -- ** DescribeRouteTables (Paginated)
    DescribeRouteTables (DescribeRouteTables'),
    newDescribeRouteTables,
    DescribeRouteTablesResponse (DescribeRouteTablesResponse'),
    newDescribeRouteTablesResponse,

    -- ** UpdateSecurityGroupRuleDescriptionsEgress
    UpdateSecurityGroupRuleDescriptionsEgress (UpdateSecurityGroupRuleDescriptionsEgress'),
    newUpdateSecurityGroupRuleDescriptionsEgress,
    UpdateSecurityGroupRuleDescriptionsEgressResponse (UpdateSecurityGroupRuleDescriptionsEgressResponse'),
    newUpdateSecurityGroupRuleDescriptionsEgressResponse,

    -- ** ResetFpgaImageAttribute
    ResetFpgaImageAttribute (ResetFpgaImageAttribute'),
    newResetFpgaImageAttribute,
    ResetFpgaImageAttributeResponse (ResetFpgaImageAttributeResponse'),
    newResetFpgaImageAttributeResponse,

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

    -- ** CreateClientVpnEndpoint
    CreateClientVpnEndpoint (CreateClientVpnEndpoint'),
    newCreateClientVpnEndpoint,
    CreateClientVpnEndpointResponse (CreateClientVpnEndpointResponse'),
    newCreateClientVpnEndpointResponse,

    -- ** RevokeClientVpnIngress
    RevokeClientVpnIngress (RevokeClientVpnIngress'),
    newRevokeClientVpnIngress,
    RevokeClientVpnIngressResponse (RevokeClientVpnIngressResponse'),
    newRevokeClientVpnIngressResponse,

    -- ** DeleteFpgaImage
    DeleteFpgaImage (DeleteFpgaImage'),
    newDeleteFpgaImage,
    DeleteFpgaImageResponse (DeleteFpgaImageResponse'),
    newDeleteFpgaImageResponse,

    -- ** ModifyVpcEndpoint
    ModifyVpcEndpoint (ModifyVpcEndpoint'),
    newModifyVpcEndpoint,
    ModifyVpcEndpointResponse (ModifyVpcEndpointResponse'),
    newModifyVpcEndpointResponse,

    -- ** DescribeReservedInstancesModifications (Paginated)
    DescribeReservedInstancesModifications (DescribeReservedInstancesModifications'),
    newDescribeReservedInstancesModifications,
    DescribeReservedInstancesModificationsResponse (DescribeReservedInstancesModificationsResponse'),
    newDescribeReservedInstancesModificationsResponse,

    -- ** DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations (Paginated)
    DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations (DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations'),
    newDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations,
    DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse (DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse'),
    newDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse,

    -- ** EnableFastSnapshotRestores
    EnableFastSnapshotRestores (EnableFastSnapshotRestores'),
    newEnableFastSnapshotRestores,
    EnableFastSnapshotRestoresResponse (EnableFastSnapshotRestoresResponse'),
    newEnableFastSnapshotRestoresResponse,

    -- ** DescribeClientVpnRoutes (Paginated)
    DescribeClientVpnRoutes (DescribeClientVpnRoutes'),
    newDescribeClientVpnRoutes,
    DescribeClientVpnRoutesResponse (DescribeClientVpnRoutesResponse'),
    newDescribeClientVpnRoutesResponse,

    -- ** GetEbsDefaultKmsKeyId
    GetEbsDefaultKmsKeyId (GetEbsDefaultKmsKeyId'),
    newGetEbsDefaultKmsKeyId,
    GetEbsDefaultKmsKeyIdResponse (GetEbsDefaultKmsKeyIdResponse'),
    newGetEbsDefaultKmsKeyIdResponse,

    -- ** ModifyIdFormat
    ModifyIdFormat (ModifyIdFormat'),
    newModifyIdFormat,
    ModifyIdFormatResponse (ModifyIdFormatResponse'),
    newModifyIdFormatResponse,

    -- ** DetachClassicLinkVpc
    DetachClassicLinkVpc (DetachClassicLinkVpc'),
    newDetachClassicLinkVpc,
    DetachClassicLinkVpcResponse (DetachClassicLinkVpcResponse'),
    newDetachClassicLinkVpcResponse,

    -- ** UnassignPrivateIpAddresses
    UnassignPrivateIpAddresses (UnassignPrivateIpAddresses'),
    newUnassignPrivateIpAddresses,
    UnassignPrivateIpAddressesResponse (UnassignPrivateIpAddressesResponse'),
    newUnassignPrivateIpAddressesResponse,

    -- ** AllocateHosts
    AllocateHosts (AllocateHosts'),
    newAllocateHosts,
    AllocateHostsResponse (AllocateHostsResponse'),
    newAllocateHostsResponse,

    -- ** GetConsoleOutput
    GetConsoleOutput (GetConsoleOutput'),
    newGetConsoleOutput,
    GetConsoleOutputResponse (GetConsoleOutputResponse'),
    newGetConsoleOutputResponse,

    -- ** ModifyVpnConnectionOptions
    ModifyVpnConnectionOptions (ModifyVpnConnectionOptions'),
    newModifyVpnConnectionOptions,
    ModifyVpnConnectionOptionsResponse (ModifyVpnConnectionOptionsResponse'),
    newModifyVpnConnectionOptionsResponse,

    -- ** CancelImportTask
    CancelImportTask (CancelImportTask'),
    newCancelImportTask,
    CancelImportTaskResponse (CancelImportTaskResponse'),
    newCancelImportTaskResponse,

    -- ** RegisterImage
    RegisterImage (RegisterImage'),
    newRegisterImage,
    RegisterImageResponse (RegisterImageResponse'),
    newRegisterImageResponse,

    -- ** ModifyFleet
    ModifyFleet (ModifyFleet'),
    newModifyFleet,
    ModifyFleetResponse (ModifyFleetResponse'),
    newModifyFleetResponse,

    -- ** DeleteRouteTable
    DeleteRouteTable (DeleteRouteTable'),
    newDeleteRouteTable,
    DeleteRouteTableResponse (DeleteRouteTableResponse'),
    newDeleteRouteTableResponse,

    -- ** ModifyReservedInstances
    ModifyReservedInstances (ModifyReservedInstances'),
    newModifyReservedInstances,
    ModifyReservedInstancesResponse (ModifyReservedInstancesResponse'),
    newModifyReservedInstancesResponse,

    -- ** DescribeImageAttribute
    DescribeImageAttribute (DescribeImageAttribute'),
    newDescribeImageAttribute,
    DescribeImageAttributeResponse (DescribeImageAttributeResponse'),
    newDescribeImageAttributeResponse,

    -- ** CreateTrafficMirrorFilterRule
    CreateTrafficMirrorFilterRule (CreateTrafficMirrorFilterRule'),
    newCreateTrafficMirrorFilterRule,
    CreateTrafficMirrorFilterRuleResponse (CreateTrafficMirrorFilterRuleResponse'),
    newCreateTrafficMirrorFilterRuleResponse,

    -- ** MonitorInstances
    MonitorInstances (MonitorInstances'),
    newMonitorInstances,
    MonitorInstancesResponse (MonitorInstancesResponse'),
    newMonitorInstancesResponse,

    -- ** ModifyVpnConnection
    ModifyVpnConnection (ModifyVpnConnection'),
    newModifyVpnConnection,
    ModifyVpnConnectionResponse (ModifyVpnConnectionResponse'),
    newModifyVpnConnectionResponse,

    -- ** DescribeSpotInstanceRequests (Paginated)
    DescribeSpotInstanceRequests (DescribeSpotInstanceRequests'),
    newDescribeSpotInstanceRequests,
    DescribeSpotInstanceRequestsResponse (DescribeSpotInstanceRequestsResponse'),
    newDescribeSpotInstanceRequestsResponse,

    -- ** CancelConversionTask
    CancelConversionTask (CancelConversionTask'),
    newCancelConversionTask,
    CancelConversionTaskResponse (CancelConversionTaskResponse'),
    newCancelConversionTaskResponse,

    -- ** ModifyVpcEndpointServiceConfiguration
    ModifyVpcEndpointServiceConfiguration (ModifyVpcEndpointServiceConfiguration'),
    newModifyVpcEndpointServiceConfiguration,
    ModifyVpcEndpointServiceConfigurationResponse (ModifyVpcEndpointServiceConfigurationResponse'),
    newModifyVpcEndpointServiceConfigurationResponse,

    -- ** ModifyTransitGatewayVpcAttachment
    ModifyTransitGatewayVpcAttachment (ModifyTransitGatewayVpcAttachment'),
    newModifyTransitGatewayVpcAttachment,
    ModifyTransitGatewayVpcAttachmentResponse (ModifyTransitGatewayVpcAttachmentResponse'),
    newModifyTransitGatewayVpcAttachmentResponse,

    -- ** AssociateRouteTable
    AssociateRouteTable (AssociateRouteTable'),
    newAssociateRouteTable,
    AssociateRouteTableResponse (AssociateRouteTableResponse'),
    newAssociateRouteTableResponse,

    -- ** DescribeAccountAttributes
    DescribeAccountAttributes (DescribeAccountAttributes'),
    newDescribeAccountAttributes,
    DescribeAccountAttributesResponse (DescribeAccountAttributesResponse'),
    newDescribeAccountAttributesResponse,

    -- ** DescribeLaunchTemplates (Paginated)
    DescribeLaunchTemplates (DescribeLaunchTemplates'),
    newDescribeLaunchTemplates,
    DescribeLaunchTemplatesResponse (DescribeLaunchTemplatesResponse'),
    newDescribeLaunchTemplatesResponse,

    -- ** DescribeIpv6Pools (Paginated)
    DescribeIpv6Pools (DescribeIpv6Pools'),
    newDescribeIpv6Pools,
    DescribeIpv6PoolsResponse (DescribeIpv6PoolsResponse'),
    newDescribeIpv6PoolsResponse,

    -- ** DescribeLocalGateways (Paginated)
    DescribeLocalGateways (DescribeLocalGateways'),
    newDescribeLocalGateways,
    DescribeLocalGatewaysResponse (DescribeLocalGatewaysResponse'),
    newDescribeLocalGatewaysResponse,

    -- ** PurchaseHostReservation
    PurchaseHostReservation (PurchaseHostReservation'),
    newPurchaseHostReservation,
    PurchaseHostReservationResponse (PurchaseHostReservationResponse'),
    newPurchaseHostReservationResponse,

    -- ** ReportInstanceStatus
    ReportInstanceStatus (ReportInstanceStatus'),
    newReportInstanceStatus,
    ReportInstanceStatusResponse (ReportInstanceStatusResponse'),
    newReportInstanceStatusResponse,

    -- ** ModifyVpcEndpointServicePermissions
    ModifyVpcEndpointServicePermissions (ModifyVpcEndpointServicePermissions'),
    newModifyVpcEndpointServicePermissions,
    ModifyVpcEndpointServicePermissionsResponse (ModifyVpcEndpointServicePermissionsResponse'),
    newModifyVpcEndpointServicePermissionsResponse,

    -- ** ModifyHosts
    ModifyHosts (ModifyHosts'),
    newModifyHosts,
    ModifyHostsResponse (ModifyHostsResponse'),
    newModifyHostsResponse,

    -- ** UnassignIpv6Addresses
    UnassignIpv6Addresses (UnassignIpv6Addresses'),
    newUnassignIpv6Addresses,
    UnassignIpv6AddressesResponse (UnassignIpv6AddressesResponse'),
    newUnassignIpv6AddressesResponse,

    -- ** GetManagedPrefixListAssociations (Paginated)
    GetManagedPrefixListAssociations (GetManagedPrefixListAssociations'),
    newGetManagedPrefixListAssociations,
    GetManagedPrefixListAssociationsResponse (GetManagedPrefixListAssociationsResponse'),
    newGetManagedPrefixListAssociationsResponse,

    -- ** DisableFastSnapshotRestores
    DisableFastSnapshotRestores (DisableFastSnapshotRestores'),
    newDisableFastSnapshotRestores,
    DisableFastSnapshotRestoresResponse (DisableFastSnapshotRestoresResponse'),
    newDisableFastSnapshotRestoresResponse,

    -- ** DeleteEgressOnlyInternetGateway
    DeleteEgressOnlyInternetGateway (DeleteEgressOnlyInternetGateway'),
    newDeleteEgressOnlyInternetGateway,
    DeleteEgressOnlyInternetGatewayResponse (DeleteEgressOnlyInternetGatewayResponse'),
    newDeleteEgressOnlyInternetGatewayResponse,

    -- ** RequestSpotInstances
    RequestSpotInstances (RequestSpotInstances'),
    newRequestSpotInstances,
    RequestSpotInstancesResponse (RequestSpotInstancesResponse'),
    newRequestSpotInstancesResponse,

    -- ** RunInstances
    RunInstances (RunInstances'),
    newRunInstances,
    Reservation (Reservation'),
    newReservation,

    -- ** GetTransitGatewayRouteTablePropagations (Paginated)
    GetTransitGatewayRouteTablePropagations (GetTransitGatewayRouteTablePropagations'),
    newGetTransitGatewayRouteTablePropagations,
    GetTransitGatewayRouteTablePropagationsResponse (GetTransitGatewayRouteTablePropagationsResponse'),
    newGetTransitGatewayRouteTablePropagationsResponse,

    -- ** AttachVolume
    AttachVolume (AttachVolume'),
    newAttachVolume,
    VolumeAttachment (VolumeAttachment'),
    newVolumeAttachment,

    -- ** AcceptVpcEndpointConnections
    AcceptVpcEndpointConnections (AcceptVpcEndpointConnections'),
    newAcceptVpcEndpointConnections,
    AcceptVpcEndpointConnectionsResponse (AcceptVpcEndpointConnectionsResponse'),
    newAcceptVpcEndpointConnectionsResponse,

    -- ** CreateDhcpOptions
    CreateDhcpOptions (CreateDhcpOptions'),
    newCreateDhcpOptions,
    CreateDhcpOptionsResponse (CreateDhcpOptionsResponse'),
    newCreateDhcpOptionsResponse,

    -- ** RebootInstances
    RebootInstances (RebootInstances'),
    newRebootInstances,
    RebootInstancesResponse (RebootInstancesResponse'),
    newRebootInstancesResponse,

    -- ** ModifyImageAttribute
    ModifyImageAttribute (ModifyImageAttribute'),
    newModifyImageAttribute,
    ModifyImageAttributeResponse (ModifyImageAttributeResponse'),
    newModifyImageAttributeResponse,

    -- ** CreateManagedPrefixList
    CreateManagedPrefixList (CreateManagedPrefixList'),
    newCreateManagedPrefixList,
    CreateManagedPrefixListResponse (CreateManagedPrefixListResponse'),
    newCreateManagedPrefixListResponse,

    -- ** SearchTransitGatewayRoutes
    SearchTransitGatewayRoutes (SearchTransitGatewayRoutes'),
    newSearchTransitGatewayRoutes,
    SearchTransitGatewayRoutesResponse (SearchTransitGatewayRoutesResponse'),
    newSearchTransitGatewayRoutesResponse,

    -- ** DescribeIdFormat
    DescribeIdFormat (DescribeIdFormat'),
    newDescribeIdFormat,
    DescribeIdFormatResponse (DescribeIdFormatResponse'),
    newDescribeIdFormatResponse,

    -- ** RegisterTransitGatewayMulticastGroupSources
    RegisterTransitGatewayMulticastGroupSources (RegisterTransitGatewayMulticastGroupSources'),
    newRegisterTransitGatewayMulticastGroupSources,
    RegisterTransitGatewayMulticastGroupSourcesResponse (RegisterTransitGatewayMulticastGroupSourcesResponse'),
    newRegisterTransitGatewayMulticastGroupSourcesResponse,

    -- ** DescribeVpcEndpointConnectionNotifications (Paginated)
    DescribeVpcEndpointConnectionNotifications (DescribeVpcEndpointConnectionNotifications'),
    newDescribeVpcEndpointConnectionNotifications,
    DescribeVpcEndpointConnectionNotificationsResponse (DescribeVpcEndpointConnectionNotificationsResponse'),
    newDescribeVpcEndpointConnectionNotificationsResponse,

    -- ** DescribeVpcs (Paginated)
    DescribeVpcs (DescribeVpcs'),
    newDescribeVpcs,
    DescribeVpcsResponse (DescribeVpcsResponse'),
    newDescribeVpcsResponse,

    -- ** GetTransitGatewayPrefixListReferences (Paginated)
    GetTransitGatewayPrefixListReferences (GetTransitGatewayPrefixListReferences'),
    newGetTransitGatewayPrefixListReferences,
    GetTransitGatewayPrefixListReferencesResponse (GetTransitGatewayPrefixListReferencesResponse'),
    newGetTransitGatewayPrefixListReferencesResponse,

    -- ** CreateRouteTable
    CreateRouteTable (CreateRouteTable'),
    newCreateRouteTable,
    CreateRouteTableResponse (CreateRouteTableResponse'),
    newCreateRouteTableResponse,

    -- ** DescribeVolumesModifications (Paginated)
    DescribeVolumesModifications (DescribeVolumesModifications'),
    newDescribeVolumesModifications,
    DescribeVolumesModificationsResponse (DescribeVolumesModificationsResponse'),
    newDescribeVolumesModificationsResponse,

    -- ** AssociateIamInstanceProfile
    AssociateIamInstanceProfile (AssociateIamInstanceProfile'),
    newAssociateIamInstanceProfile,
    AssociateIamInstanceProfileResponse (AssociateIamInstanceProfileResponse'),
    newAssociateIamInstanceProfileResponse,

    -- ** CreateImage
    CreateImage (CreateImage'),
    newCreateImage,
    CreateImageResponse (CreateImageResponse'),
    newCreateImageResponse,

    -- ** DescribeTrafficMirrorTargets (Paginated)
    DescribeTrafficMirrorTargets (DescribeTrafficMirrorTargets'),
    newDescribeTrafficMirrorTargets,
    DescribeTrafficMirrorTargetsResponse (DescribeTrafficMirrorTargetsResponse'),
    newDescribeTrafficMirrorTargetsResponse,

    -- ** AssociateDhcpOptions
    AssociateDhcpOptions (AssociateDhcpOptions'),
    newAssociateDhcpOptions,
    AssociateDhcpOptionsResponse (AssociateDhcpOptionsResponse'),
    newAssociateDhcpOptionsResponse,

    -- ** DescribeSpotFleetRequestHistory
    DescribeSpotFleetRequestHistory (DescribeSpotFleetRequestHistory'),
    newDescribeSpotFleetRequestHistory,
    DescribeSpotFleetRequestHistoryResponse (DescribeSpotFleetRequestHistoryResponse'),
    newDescribeSpotFleetRequestHistoryResponse,

    -- ** ModifyInstanceEventStartTime
    ModifyInstanceEventStartTime (ModifyInstanceEventStartTime'),
    newModifyInstanceEventStartTime,
    ModifyInstanceEventStartTimeResponse (ModifyInstanceEventStartTimeResponse'),
    newModifyInstanceEventStartTimeResponse,

    -- ** DisassociateEnclaveCertificateIamRole
    DisassociateEnclaveCertificateIamRole (DisassociateEnclaveCertificateIamRole'),
    newDisassociateEnclaveCertificateIamRole,
    DisassociateEnclaveCertificateIamRoleResponse (DisassociateEnclaveCertificateIamRoleResponse'),
    newDisassociateEnclaveCertificateIamRoleResponse,

    -- ** DeleteVpcPeeringConnection
    DeleteVpcPeeringConnection (DeleteVpcPeeringConnection'),
    newDeleteVpcPeeringConnection,
    DeleteVpcPeeringConnectionResponse (DeleteVpcPeeringConnectionResponse'),
    newDeleteVpcPeeringConnectionResponse,

    -- ** ResetInstanceAttribute
    ResetInstanceAttribute (ResetInstanceAttribute'),
    newResetInstanceAttribute,
    ResetInstanceAttributeResponse (ResetInstanceAttributeResponse'),
    newResetInstanceAttributeResponse,

    -- ** DescribeInstanceStatus (Paginated)
    DescribeInstanceStatus (DescribeInstanceStatus'),
    newDescribeInstanceStatus,
    DescribeInstanceStatusResponse (DescribeInstanceStatusResponse'),
    newDescribeInstanceStatusResponse,

    -- ** AttachNetworkInterface
    AttachNetworkInterface (AttachNetworkInterface'),
    newAttachNetworkInterface,
    AttachNetworkInterfaceResponse (AttachNetworkInterfaceResponse'),
    newAttachNetworkInterfaceResponse,

    -- ** AssignIpv6Addresses
    AssignIpv6Addresses (AssignIpv6Addresses'),
    newAssignIpv6Addresses,
    AssignIpv6AddressesResponse (AssignIpv6AddressesResponse'),
    newAssignIpv6AddressesResponse,

    -- ** CreateLocalGatewayRoute
    CreateLocalGatewayRoute (CreateLocalGatewayRoute'),
    newCreateLocalGatewayRoute,
    CreateLocalGatewayRouteResponse (CreateLocalGatewayRouteResponse'),
    newCreateLocalGatewayRouteResponse,

    -- ** EnableVgwRoutePropagation
    EnableVgwRoutePropagation (EnableVgwRoutePropagation'),
    newEnableVgwRoutePropagation,
    EnableVgwRoutePropagationResponse (EnableVgwRoutePropagationResponse'),
    newEnableVgwRoutePropagationResponse,

    -- ** DescribeVpcEndpoints (Paginated)
    DescribeVpcEndpoints (DescribeVpcEndpoints'),
    newDescribeVpcEndpoints,
    DescribeVpcEndpointsResponse (DescribeVpcEndpointsResponse'),
    newDescribeVpcEndpointsResponse,

    -- ** CreateNetworkAclEntry
    CreateNetworkAclEntry (CreateNetworkAclEntry'),
    newCreateNetworkAclEntry,
    CreateNetworkAclEntryResponse (CreateNetworkAclEntryResponse'),
    newCreateNetworkAclEntryResponse,

    -- ** DescribeStaleSecurityGroups (Paginated)
    DescribeStaleSecurityGroups (DescribeStaleSecurityGroups'),
    newDescribeStaleSecurityGroups,
    DescribeStaleSecurityGroupsResponse (DescribeStaleSecurityGroupsResponse'),
    newDescribeStaleSecurityGroupsResponse,

    -- ** DescribeFlowLogs (Paginated)
    DescribeFlowLogs (DescribeFlowLogs'),
    newDescribeFlowLogs,
    DescribeFlowLogsResponse (DescribeFlowLogsResponse'),
    newDescribeFlowLogsResponse,

    -- ** DescribePlacementGroups
    DescribePlacementGroups (DescribePlacementGroups'),
    newDescribePlacementGroups,
    DescribePlacementGroupsResponse (DescribePlacementGroupsResponse'),
    newDescribePlacementGroupsResponse,

    -- ** DescribeFleets (Paginated)
    DescribeFleets (DescribeFleets'),
    newDescribeFleets,
    DescribeFleetsResponse (DescribeFleetsResponse'),
    newDescribeFleetsResponse,

    -- ** ModifyIdentityIdFormat
    ModifyIdentityIdFormat (ModifyIdentityIdFormat'),
    newModifyIdentityIdFormat,
    ModifyIdentityIdFormatResponse (ModifyIdentityIdFormatResponse'),
    newModifyIdentityIdFormatResponse,

    -- ** DescribeLocalGatewayVirtualInterfaceGroups (Paginated)
    DescribeLocalGatewayVirtualInterfaceGroups (DescribeLocalGatewayVirtualInterfaceGroups'),
    newDescribeLocalGatewayVirtualInterfaceGroups,
    DescribeLocalGatewayVirtualInterfaceGroupsResponse (DescribeLocalGatewayVirtualInterfaceGroupsResponse'),
    newDescribeLocalGatewayVirtualInterfaceGroupsResponse,

    -- ** ReplaceNetworkAclEntry
    ReplaceNetworkAclEntry (ReplaceNetworkAclEntry'),
    newReplaceNetworkAclEntry,
    ReplaceNetworkAclEntryResponse (ReplaceNetworkAclEntryResponse'),
    newReplaceNetworkAclEntryResponse,

    -- ** DeleteTags
    DeleteTags (DeleteTags'),
    newDeleteTags,
    DeleteTagsResponse (DeleteTagsResponse'),
    newDeleteTagsResponse,

    -- ** DescribeTransitGatewayAttachments (Paginated)
    DescribeTransitGatewayAttachments (DescribeTransitGatewayAttachments'),
    newDescribeTransitGatewayAttachments,
    DescribeTransitGatewayAttachmentsResponse (DescribeTransitGatewayAttachmentsResponse'),
    newDescribeTransitGatewayAttachmentsResponse,

    -- ** DescribeReservedInstancesOfferings (Paginated)
    DescribeReservedInstancesOfferings (DescribeReservedInstancesOfferings'),
    newDescribeReservedInstancesOfferings,
    DescribeReservedInstancesOfferingsResponse (DescribeReservedInstancesOfferingsResponse'),
    newDescribeReservedInstancesOfferingsResponse,

    -- ** ModifySnapshotAttribute
    ModifySnapshotAttribute (ModifySnapshotAttribute'),
    newModifySnapshotAttribute,
    ModifySnapshotAttributeResponse (ModifySnapshotAttributeResponse'),
    newModifySnapshotAttributeResponse,

    -- ** ConfirmProductInstance
    ConfirmProductInstance (ConfirmProductInstance'),
    newConfirmProductInstance,
    ConfirmProductInstanceResponse (ConfirmProductInstanceResponse'),
    newConfirmProductInstanceResponse,

    -- ** DescribeVpnConnections
    DescribeVpnConnections (DescribeVpnConnections'),
    newDescribeVpnConnections,
    DescribeVpnConnectionsResponse (DescribeVpnConnectionsResponse'),
    newDescribeVpnConnectionsResponse,

    -- ** ModifyAvailabilityZoneGroup
    ModifyAvailabilityZoneGroup (ModifyAvailabilityZoneGroup'),
    newModifyAvailabilityZoneGroup,
    ModifyAvailabilityZoneGroupResponse (ModifyAvailabilityZoneGroupResponse'),
    newModifyAvailabilityZoneGroupResponse,

    -- ** DisassociateIamInstanceProfile
    DisassociateIamInstanceProfile (DisassociateIamInstanceProfile'),
    newDisassociateIamInstanceProfile,
    DisassociateIamInstanceProfileResponse (DisassociateIamInstanceProfileResponse'),
    newDisassociateIamInstanceProfileResponse,

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

    -- ** ImportVolume
    ImportVolume (ImportVolume'),
    newImportVolume,
    ImportVolumeResponse (ImportVolumeResponse'),
    newImportVolumeResponse,

    -- ** DescribeAddresses
    DescribeAddresses (DescribeAddresses'),
    newDescribeAddresses,
    DescribeAddressesResponse (DescribeAddressesResponse'),
    newDescribeAddressesResponse,

    -- ** DeleteLocalGatewayRoute
    DeleteLocalGatewayRoute (DeleteLocalGatewayRoute'),
    newDeleteLocalGatewayRoute,
    DeleteLocalGatewayRouteResponse (DeleteLocalGatewayRouteResponse'),
    newDeleteLocalGatewayRouteResponse,

    -- ** DescribeVpcEndpointServiceConfigurations (Paginated)
    DescribeVpcEndpointServiceConfigurations (DescribeVpcEndpointServiceConfigurations'),
    newDescribeVpcEndpointServiceConfigurations,
    DescribeVpcEndpointServiceConfigurationsResponse (DescribeVpcEndpointServiceConfigurationsResponse'),
    newDescribeVpcEndpointServiceConfigurationsResponse,

    -- ** DescribeNetworkInterfaces (Paginated)
    DescribeNetworkInterfaces (DescribeNetworkInterfaces'),
    newDescribeNetworkInterfaces,
    DescribeNetworkInterfacesResponse (DescribeNetworkInterfacesResponse'),
    newDescribeNetworkInterfacesResponse,

    -- ** DescribeVpcEndpointServices (Paginated)
    DescribeVpcEndpointServices (DescribeVpcEndpointServices'),
    newDescribeVpcEndpointServices,
    DescribeVpcEndpointServicesResponse (DescribeVpcEndpointServicesResponse'),
    newDescribeVpcEndpointServicesResponse,

    -- ** DeleteNetworkAclEntry
    DeleteNetworkAclEntry (DeleteNetworkAclEntry'),
    newDeleteNetworkAclEntry,
    DeleteNetworkAclEntryResponse (DeleteNetworkAclEntryResponse'),
    newDeleteNetworkAclEntryResponse,

    -- ** GetTransitGatewayAttachmentPropagations (Paginated)
    GetTransitGatewayAttachmentPropagations (GetTransitGatewayAttachmentPropagations'),
    newGetTransitGatewayAttachmentPropagations,
    GetTransitGatewayAttachmentPropagationsResponse (GetTransitGatewayAttachmentPropagationsResponse'),
    newGetTransitGatewayAttachmentPropagationsResponse,

    -- ** AssignPrivateIpAddresses
    AssignPrivateIpAddresses (AssignPrivateIpAddresses'),
    newAssignPrivateIpAddresses,
    AssignPrivateIpAddressesResponse (AssignPrivateIpAddressesResponse'),
    newAssignPrivateIpAddressesResponse,

    -- ** DescribeNatGateways (Paginated)
    DescribeNatGateways (DescribeNatGateways'),
    newDescribeNatGateways,
    DescribeNatGatewaysResponse (DescribeNatGatewaysResponse'),
    newDescribeNatGatewaysResponse,

    -- ** DescribeSnapshotAttribute
    DescribeSnapshotAttribute (DescribeSnapshotAttribute'),
    newDescribeSnapshotAttribute,
    DescribeSnapshotAttributeResponse (DescribeSnapshotAttributeResponse'),
    newDescribeSnapshotAttributeResponse,

    -- ** DeleteSnapshot
    DeleteSnapshot (DeleteSnapshot'),
    newDeleteSnapshot,
    DeleteSnapshotResponse (DeleteSnapshotResponse'),
    newDeleteSnapshotResponse,

    -- ** DeleteCarrierGateway
    DeleteCarrierGateway (DeleteCarrierGateway'),
    newDeleteCarrierGateway,
    DeleteCarrierGatewayResponse (DeleteCarrierGatewayResponse'),
    newDeleteCarrierGatewayResponse,

    -- ** DescribeTransitGatewayVpcAttachments (Paginated)
    DescribeTransitGatewayVpcAttachments (DescribeTransitGatewayVpcAttachments'),
    newDescribeTransitGatewayVpcAttachments,
    DescribeTransitGatewayVpcAttachmentsResponse (DescribeTransitGatewayVpcAttachmentsResponse'),
    newDescribeTransitGatewayVpcAttachmentsResponse,

    -- ** ModifyVpcEndpointConnectionNotification
    ModifyVpcEndpointConnectionNotification (ModifyVpcEndpointConnectionNotification'),
    newModifyVpcEndpointConnectionNotification,
    ModifyVpcEndpointConnectionNotificationResponse (ModifyVpcEndpointConnectionNotificationResponse'),
    newModifyVpcEndpointConnectionNotificationResponse,

    -- ** PurchaseReservedInstancesOffering
    PurchaseReservedInstancesOffering (PurchaseReservedInstancesOffering'),
    newPurchaseReservedInstancesOffering,
    PurchaseReservedInstancesOfferingResponse (PurchaseReservedInstancesOfferingResponse'),
    newPurchaseReservedInstancesOfferingResponse,

    -- ** AuthorizeSecurityGroupIngress
    AuthorizeSecurityGroupIngress (AuthorizeSecurityGroupIngress'),
    newAuthorizeSecurityGroupIngress,
    AuthorizeSecurityGroupIngressResponse (AuthorizeSecurityGroupIngressResponse'),
    newAuthorizeSecurityGroupIngressResponse,

    -- ** GetConsoleScreenshot
    GetConsoleScreenshot (GetConsoleScreenshot'),
    newGetConsoleScreenshot,
    GetConsoleScreenshotResponse (GetConsoleScreenshotResponse'),
    newGetConsoleScreenshotResponse,

    -- ** DisableVgwRoutePropagation
    DisableVgwRoutePropagation (DisableVgwRoutePropagation'),
    newDisableVgwRoutePropagation,
    DisableVgwRoutePropagationResponse (DisableVgwRoutePropagationResponse'),
    newDisableVgwRoutePropagationResponse,

    -- ** DescribeTransitGatewayMulticastDomains (Paginated)
    DescribeTransitGatewayMulticastDomains (DescribeTransitGatewayMulticastDomains'),
    newDescribeTransitGatewayMulticastDomains,
    DescribeTransitGatewayMulticastDomainsResponse (DescribeTransitGatewayMulticastDomainsResponse'),
    newDescribeTransitGatewayMulticastDomainsResponse,

    -- ** DescribeSubnets (Paginated)
    DescribeSubnets (DescribeSubnets'),
    newDescribeSubnets,
    DescribeSubnetsResponse (DescribeSubnetsResponse'),
    newDescribeSubnetsResponse,

    -- ** UnmonitorInstances
    UnmonitorInstances (UnmonitorInstances'),
    newUnmonitorInstances,
    UnmonitorInstancesResponse (UnmonitorInstancesResponse'),
    newUnmonitorInstancesResponse,

    -- ** CancelSpotInstanceRequests
    CancelSpotInstanceRequests (CancelSpotInstanceRequests'),
    newCancelSpotInstanceRequests,
    CancelSpotInstanceRequestsResponse (CancelSpotInstanceRequestsResponse'),
    newCancelSpotInstanceRequestsResponse,

    -- ** CreateSpotDatafeedSubscription
    CreateSpotDatafeedSubscription (CreateSpotDatafeedSubscription'),
    newCreateSpotDatafeedSubscription,
    CreateSpotDatafeedSubscriptionResponse (CreateSpotDatafeedSubscriptionResponse'),
    newCreateSpotDatafeedSubscriptionResponse,

    -- ** DisassociateRouteTable
    DisassociateRouteTable (DisassociateRouteTable'),
    newDisassociateRouteTable,
    DisassociateRouteTableResponse (DisassociateRouteTableResponse'),
    newDisassociateRouteTableResponse,

    -- ** DescribeTransitGatewayConnectPeers (Paginated)
    DescribeTransitGatewayConnectPeers (DescribeTransitGatewayConnectPeers'),
    newDescribeTransitGatewayConnectPeers,
    DescribeTransitGatewayConnectPeersResponse (DescribeTransitGatewayConnectPeersResponse'),
    newDescribeTransitGatewayConnectPeersResponse,

    -- ** ModifyVpnTunnelCertificate
    ModifyVpnTunnelCertificate (ModifyVpnTunnelCertificate'),
    newModifyVpnTunnelCertificate,
    ModifyVpnTunnelCertificateResponse (ModifyVpnTunnelCertificateResponse'),
    newModifyVpnTunnelCertificateResponse,

    -- ** RestoreManagedPrefixListVersion
    RestoreManagedPrefixListVersion (RestoreManagedPrefixListVersion'),
    newRestoreManagedPrefixListVersion,
    RestoreManagedPrefixListVersionResponse (RestoreManagedPrefixListVersionResponse'),
    newRestoreManagedPrefixListVersionResponse,

    -- ** ModifyAddressAttribute
    ModifyAddressAttribute (ModifyAddressAttribute'),
    newModifyAddressAttribute,
    ModifyAddressAttributeResponse (ModifyAddressAttributeResponse'),
    newModifyAddressAttributeResponse,

    -- ** CreateVpnConnection
    CreateVpnConnection (CreateVpnConnection'),
    newCreateVpnConnection,
    CreateVpnConnectionResponse (CreateVpnConnectionResponse'),
    newCreateVpnConnectionResponse,

    -- ** AssociateSubnetCidrBlock
    AssociateSubnetCidrBlock (AssociateSubnetCidrBlock'),
    newAssociateSubnetCidrBlock,
    AssociateSubnetCidrBlockResponse (AssociateSubnetCidrBlockResponse'),
    newAssociateSubnetCidrBlockResponse,

    -- ** AttachClassicLinkVpc
    AttachClassicLinkVpc (AttachClassicLinkVpc'),
    newAttachClassicLinkVpc,
    AttachClassicLinkVpcResponse (AttachClassicLinkVpcResponse'),
    newAttachClassicLinkVpcResponse,

    -- ** DescribeSpotPriceHistory (Paginated)
    DescribeSpotPriceHistory (DescribeSpotPriceHistory'),
    newDescribeSpotPriceHistory,
    DescribeSpotPriceHistoryResponse (DescribeSpotPriceHistoryResponse'),
    newDescribeSpotPriceHistoryResponse,

    -- ** DeleteQueuedReservedInstances
    DeleteQueuedReservedInstances (DeleteQueuedReservedInstances'),
    newDeleteQueuedReservedInstances,
    DeleteQueuedReservedInstancesResponse (DeleteQueuedReservedInstancesResponse'),
    newDeleteQueuedReservedInstancesResponse,

    -- ** DescribeAggregateIdFormat
    DescribeAggregateIdFormat (DescribeAggregateIdFormat'),
    newDescribeAggregateIdFormat,
    DescribeAggregateIdFormatResponse (DescribeAggregateIdFormatResponse'),
    newDescribeAggregateIdFormatResponse,

    -- ** DescribeReservedInstancesListings
    DescribeReservedInstancesListings (DescribeReservedInstancesListings'),
    newDescribeReservedInstancesListings,
    DescribeReservedInstancesListingsResponse (DescribeReservedInstancesListingsResponse'),
    newDescribeReservedInstancesListingsResponse,

    -- ** CopyImage
    CopyImage (CopyImage'),
    newCopyImage,
    CopyImageResponse (CopyImageResponse'),
    newCopyImageResponse,

    -- ** CreateLocalGatewayRouteTableVpcAssociation
    CreateLocalGatewayRouteTableVpcAssociation (CreateLocalGatewayRouteTableVpcAssociation'),
    newCreateLocalGatewayRouteTableVpcAssociation,
    CreateLocalGatewayRouteTableVpcAssociationResponse (CreateLocalGatewayRouteTableVpcAssociationResponse'),
    newCreateLocalGatewayRouteTableVpcAssociationResponse,

    -- ** DescribeCarrierGateways (Paginated)
    DescribeCarrierGateways (DescribeCarrierGateways'),
    newDescribeCarrierGateways,
    DescribeCarrierGatewaysResponse (DescribeCarrierGatewaysResponse'),
    newDescribeCarrierGatewaysResponse,

    -- ** DeleteInternetGateway
    DeleteInternetGateway (DeleteInternetGateway'),
    newDeleteInternetGateway,
    DeleteInternetGatewayResponse (DeleteInternetGatewayResponse'),
    newDeleteInternetGatewayResponse,

    -- ** CreateFleet
    CreateFleet (CreateFleet'),
    newCreateFleet,
    CreateFleetResponse (CreateFleetResponse'),
    newCreateFleetResponse,

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

    -- ** ImportClientVpnClientCertificateRevocationList
    ImportClientVpnClientCertificateRevocationList (ImportClientVpnClientCertificateRevocationList'),
    newImportClientVpnClientCertificateRevocationList,
    ImportClientVpnClientCertificateRevocationListResponse (ImportClientVpnClientCertificateRevocationListResponse'),
    newImportClientVpnClientCertificateRevocationListResponse,

    -- ** AssociateClientVpnTargetNetwork
    AssociateClientVpnTargetNetwork (AssociateClientVpnTargetNetwork'),
    newAssociateClientVpnTargetNetwork,
    AssociateClientVpnTargetNetworkResponse (AssociateClientVpnTargetNetworkResponse'),
    newAssociateClientVpnTargetNetworkResponse,

    -- ** CancelCapacityReservation
    CancelCapacityReservation (CancelCapacityReservation'),
    newCancelCapacityReservation,
    CancelCapacityReservationResponse (CancelCapacityReservationResponse'),
    newCancelCapacityReservationResponse,

    -- ** CancelReservedInstancesListing
    CancelReservedInstancesListing (CancelReservedInstancesListing'),
    newCancelReservedInstancesListing,
    CancelReservedInstancesListingResponse (CancelReservedInstancesListingResponse'),
    newCancelReservedInstancesListingResponse,

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

    -- ** CreateVpcEndpoint
    CreateVpcEndpoint (CreateVpcEndpoint'),
    newCreateVpcEndpoint,
    CreateVpcEndpointResponse (CreateVpcEndpointResponse'),
    newCreateVpcEndpointResponse,

    -- ** DescribeSnapshots (Paginated)
    DescribeSnapshots (DescribeSnapshots'),
    newDescribeSnapshots,
    DescribeSnapshotsResponse (DescribeSnapshotsResponse'),
    newDescribeSnapshotsResponse,

    -- ** DescribeImportSnapshotTasks (Paginated)
    DescribeImportSnapshotTasks (DescribeImportSnapshotTasks'),
    newDescribeImportSnapshotTasks,
    DescribeImportSnapshotTasksResponse (DescribeImportSnapshotTasksResponse'),
    newDescribeImportSnapshotTasksResponse,

    -- ** DescribeNetworkInterfaceAttribute
    DescribeNetworkInterfaceAttribute (DescribeNetworkInterfaceAttribute'),
    newDescribeNetworkInterfaceAttribute,
    DescribeNetworkInterfaceAttributeResponse (DescribeNetworkInterfaceAttributeResponse'),
    newDescribeNetworkInterfaceAttributeResponse,

    -- ** DescribeInstanceEventNotificationAttributes
    DescribeInstanceEventNotificationAttributes (DescribeInstanceEventNotificationAttributes'),
    newDescribeInstanceEventNotificationAttributes,
    DescribeInstanceEventNotificationAttributesResponse (DescribeInstanceEventNotificationAttributesResponse'),
    newDescribeInstanceEventNotificationAttributesResponse,

    -- ** EnableEbsEncryptionByDefault
    EnableEbsEncryptionByDefault (EnableEbsEncryptionByDefault'),
    newEnableEbsEncryptionByDefault,
    EnableEbsEncryptionByDefaultResponse (EnableEbsEncryptionByDefaultResponse'),
    newEnableEbsEncryptionByDefaultResponse,

    -- ** ModifyTrafficMirrorFilterRule
    ModifyTrafficMirrorFilterRule (ModifyTrafficMirrorFilterRule'),
    newModifyTrafficMirrorFilterRule,
    ModifyTrafficMirrorFilterRuleResponse (ModifyTrafficMirrorFilterRuleResponse'),
    newModifyTrafficMirrorFilterRuleResponse,

    -- ** DescribeCoipPools (Paginated)
    DescribeCoipPools (DescribeCoipPools'),
    newDescribeCoipPools,
    DescribeCoipPoolsResponse (DescribeCoipPoolsResponse'),
    newDescribeCoipPoolsResponse,

    -- ** CancelExportTask
    CancelExportTask (CancelExportTask'),
    newCancelExportTask,
    CancelExportTaskResponse (CancelExportTaskResponse'),
    newCancelExportTaskResponse,

    -- ** EnableVolumeIO
    EnableVolumeIO (EnableVolumeIO'),
    newEnableVolumeIO,
    EnableVolumeIOResponse (EnableVolumeIOResponse'),
    newEnableVolumeIOResponse,

    -- ** ModifyTransitGateway
    ModifyTransitGateway (ModifyTransitGateway'),
    newModifyTransitGateway,
    ModifyTransitGatewayResponse (ModifyTransitGatewayResponse'),
    newModifyTransitGatewayResponse,

    -- ** DescribeInstanceTypeOfferings (Paginated)
    DescribeInstanceTypeOfferings (DescribeInstanceTypeOfferings'),
    newDescribeInstanceTypeOfferings,
    DescribeInstanceTypeOfferingsResponse (DescribeInstanceTypeOfferingsResponse'),
    newDescribeInstanceTypeOfferingsResponse,

    -- ** CreateSubnet
    CreateSubnet (CreateSubnet'),
    newCreateSubnet,
    CreateSubnetResponse (CreateSubnetResponse'),
    newCreateSubnetResponse,

    -- ** RequestSpotFleet
    RequestSpotFleet (RequestSpotFleet'),
    newRequestSpotFleet,
    RequestSpotFleetResponse (RequestSpotFleetResponse'),
    newRequestSpotFleetResponse,

    -- ** DeleteVpnConnection
    DeleteVpnConnection (DeleteVpnConnection'),
    newDeleteVpnConnection,
    DeleteVpnConnectionResponse (DeleteVpnConnectionResponse'),
    newDeleteVpnConnectionResponse,

    -- ** ModifySpotFleetRequest
    ModifySpotFleetRequest (ModifySpotFleetRequest'),
    newModifySpotFleetRequest,
    ModifySpotFleetRequestResponse (ModifySpotFleetRequestResponse'),
    newModifySpotFleetRequestResponse,

    -- ** DeregisterImage
    DeregisterImage (DeregisterImage'),
    newDeregisterImage,
    DeregisterImageResponse (DeregisterImageResponse'),
    newDeregisterImageResponse,

    -- ** DetachVpnGateway
    DetachVpnGateway (DetachVpnGateway'),
    newDetachVpnGateway,
    DetachVpnGatewayResponse (DetachVpnGatewayResponse'),
    newDetachVpnGatewayResponse,

    -- ** CreateNetworkInterface
    CreateNetworkInterface (CreateNetworkInterface'),
    newCreateNetworkInterface,
    CreateNetworkInterfaceResponse (CreateNetworkInterfaceResponse'),
    newCreateNetworkInterfaceResponse,

    -- ** ModifyNetworkInterfaceAttribute
    ModifyNetworkInterfaceAttribute (ModifyNetworkInterfaceAttribute'),
    newModifyNetworkInterfaceAttribute,
    ModifyNetworkInterfaceAttributeResponse (ModifyNetworkInterfaceAttributeResponse'),
    newModifyNetworkInterfaceAttributeResponse,

    -- ** CreateNatGateway
    CreateNatGateway (CreateNatGateway'),
    newCreateNatGateway,
    CreateNatGatewayResponse (CreateNatGatewayResponse'),
    newCreateNatGatewayResponse,

    -- ** GetAssociatedEnclaveCertificateIamRoles
    GetAssociatedEnclaveCertificateIamRoles (GetAssociatedEnclaveCertificateIamRoles'),
    newGetAssociatedEnclaveCertificateIamRoles,
    GetAssociatedEnclaveCertificateIamRolesResponse (GetAssociatedEnclaveCertificateIamRolesResponse'),
    newGetAssociatedEnclaveCertificateIamRolesResponse,

    -- ** CreateInternetGateway
    CreateInternetGateway (CreateInternetGateway'),
    newCreateInternetGateway,
    CreateInternetGatewayResponse (CreateInternetGatewayResponse'),
    newCreateInternetGatewayResponse,

    -- ** EnableTransitGatewayRouteTablePropagation
    EnableTransitGatewayRouteTablePropagation (EnableTransitGatewayRouteTablePropagation'),
    newEnableTransitGatewayRouteTablePropagation,
    EnableTransitGatewayRouteTablePropagationResponse (EnableTransitGatewayRouteTablePropagationResponse'),
    newEnableTransitGatewayRouteTablePropagationResponse,

    -- ** ResetAddressAttribute
    ResetAddressAttribute (ResetAddressAttribute'),
    newResetAddressAttribute,
    ResetAddressAttributeResponse (ResetAddressAttributeResponse'),
    newResetAddressAttributeResponse,

    -- ** DescribeTransitGatewayConnects (Paginated)
    DescribeTransitGatewayConnects (DescribeTransitGatewayConnects'),
    newDescribeTransitGatewayConnects,
    DescribeTransitGatewayConnectsResponse (DescribeTransitGatewayConnectsResponse'),
    newDescribeTransitGatewayConnectsResponse,

    -- ** DeletePlacementGroup
    DeletePlacementGroup (DeletePlacementGroup'),
    newDeletePlacementGroup,
    DeletePlacementGroupResponse (DeletePlacementGroupResponse'),
    newDeletePlacementGroupResponse,

    -- ** DescribeInstanceTypes (Paginated)
    DescribeInstanceTypes (DescribeInstanceTypes'),
    newDescribeInstanceTypes,
    DescribeInstanceTypesResponse (DescribeInstanceTypesResponse'),
    newDescribeInstanceTypesResponse,

    -- ** DescribeBundleTasks
    DescribeBundleTasks (DescribeBundleTasks'),
    newDescribeBundleTasks,
    DescribeBundleTasksResponse (DescribeBundleTasksResponse'),
    newDescribeBundleTasksResponse,

    -- ** ModifySubnetAttribute
    ModifySubnetAttribute (ModifySubnetAttribute'),
    newModifySubnetAttribute,
    ModifySubnetAttributeResponse (ModifySubnetAttributeResponse'),
    newModifySubnetAttributeResponse,

    -- ** DescribeSecurityGroups (Paginated)
    DescribeSecurityGroups (DescribeSecurityGroups'),
    newDescribeSecurityGroups,
    DescribeSecurityGroupsResponse (DescribeSecurityGroupsResponse'),
    newDescribeSecurityGroupsResponse,

    -- ** CreateClientVpnRoute
    CreateClientVpnRoute (CreateClientVpnRoute'),
    newCreateClientVpnRoute,
    CreateClientVpnRouteResponse (CreateClientVpnRouteResponse'),
    newCreateClientVpnRouteResponse,

    -- ** DisassociateSubnetCidrBlock
    DisassociateSubnetCidrBlock (DisassociateSubnetCidrBlock'),
    newDisassociateSubnetCidrBlock,
    DisassociateSubnetCidrBlockResponse (DisassociateSubnetCidrBlockResponse'),
    newDisassociateSubnetCidrBlockResponse,

    -- ** DescribeSpotDatafeedSubscription
    DescribeSpotDatafeedSubscription (DescribeSpotDatafeedSubscription'),
    newDescribeSpotDatafeedSubscription,
    DescribeSpotDatafeedSubscriptionResponse (DescribeSpotDatafeedSubscriptionResponse'),
    newDescribeSpotDatafeedSubscriptionResponse,

    -- ** CreateInstanceExportTask
    CreateInstanceExportTask (CreateInstanceExportTask'),
    newCreateInstanceExportTask,
    CreateInstanceExportTaskResponse (CreateInstanceExportTaskResponse'),
    newCreateInstanceExportTaskResponse,

    -- ** DisassociateClientVpnTargetNetwork
    DisassociateClientVpnTargetNetwork (DisassociateClientVpnTargetNetwork'),
    newDisassociateClientVpnTargetNetwork,
    DisassociateClientVpnTargetNetworkResponse (DisassociateClientVpnTargetNetworkResponse'),
    newDisassociateClientVpnTargetNetworkResponse,

    -- ** SendDiagnosticInterrupt
    SendDiagnosticInterrupt (SendDiagnosticInterrupt'),
    newSendDiagnosticInterrupt,
    SendDiagnosticInterruptResponse (SendDiagnosticInterruptResponse'),
    newSendDiagnosticInterruptResponse,

    -- ** DescribeVpcAttribute
    DescribeVpcAttribute (DescribeVpcAttribute'),
    newDescribeVpcAttribute,
    DescribeVpcAttributeResponse (DescribeVpcAttributeResponse'),
    newDescribeVpcAttributeResponse,

    -- ** DescribeSecurityGroupReferences
    DescribeSecurityGroupReferences (DescribeSecurityGroupReferences'),
    newDescribeSecurityGroupReferences,
    DescribeSecurityGroupReferencesResponse (DescribeSecurityGroupReferencesResponse'),
    newDescribeSecurityGroupReferencesResponse,

    -- ** ModifyCapacityReservation
    ModifyCapacityReservation (ModifyCapacityReservation'),
    newModifyCapacityReservation,
    ModifyCapacityReservationResponse (ModifyCapacityReservationResponse'),
    newModifyCapacityReservationResponse,

    -- ** DetachInternetGateway
    DetachInternetGateway (DetachInternetGateway'),
    newDetachInternetGateway,
    DetachInternetGatewayResponse (DetachInternetGatewayResponse'),
    newDetachInternetGatewayResponse,

    -- ** CreateVolume
    CreateVolume (CreateVolume'),
    newCreateVolume,
    Volume (Volume'),
    newVolume,

    -- ** ExportClientVpnClientConfiguration
    ExportClientVpnClientConfiguration (ExportClientVpnClientConfiguration'),
    newExportClientVpnClientConfiguration,
    ExportClientVpnClientConfigurationResponse (ExportClientVpnClientConfigurationResponse'),
    newExportClientVpnClientConfigurationResponse,

    -- ** RevokeSecurityGroupEgress
    RevokeSecurityGroupEgress (RevokeSecurityGroupEgress'),
    newRevokeSecurityGroupEgress,
    RevokeSecurityGroupEgressResponse (RevokeSecurityGroupEgressResponse'),
    newRevokeSecurityGroupEgressResponse,

    -- ** DeleteKeyPair
    DeleteKeyPair (DeleteKeyPair'),
    newDeleteKeyPair,
    DeleteKeyPairResponse (DeleteKeyPairResponse'),
    newDeleteKeyPairResponse,

    -- ** ModifyInstanceMetadataOptions
    ModifyInstanceMetadataOptions (ModifyInstanceMetadataOptions'),
    newModifyInstanceMetadataOptions,
    ModifyInstanceMetadataOptionsResponse (ModifyInstanceMetadataOptionsResponse'),
    newModifyInstanceMetadataOptionsResponse,

    -- ** DescribeEgressOnlyInternetGateways (Paginated)
    DescribeEgressOnlyInternetGateways (DescribeEgressOnlyInternetGateways'),
    newDescribeEgressOnlyInternetGateways,
    DescribeEgressOnlyInternetGatewaysResponse (DescribeEgressOnlyInternetGatewaysResponse'),
    newDescribeEgressOnlyInternetGatewaysResponse,

    -- ** ModifyTrafficMirrorFilterNetworkServices
    ModifyTrafficMirrorFilterNetworkServices (ModifyTrafficMirrorFilterNetworkServices'),
    newModifyTrafficMirrorFilterNetworkServices,
    ModifyTrafficMirrorFilterNetworkServicesResponse (ModifyTrafficMirrorFilterNetworkServicesResponse'),
    newModifyTrafficMirrorFilterNetworkServicesResponse,

    -- ** ImportSnapshot
    ImportSnapshot (ImportSnapshot'),
    newImportSnapshot,
    ImportSnapshotResponse (ImportSnapshotResponse'),
    newImportSnapshotResponse,

    -- ** DescribeImages
    DescribeImages (DescribeImages'),
    newDescribeImages,
    DescribeImagesResponse (DescribeImagesResponse'),
    newDescribeImagesResponse,

    -- ** DeprovisionByoipCidr
    DeprovisionByoipCidr (DeprovisionByoipCidr'),
    newDeprovisionByoipCidr,
    DeprovisionByoipCidrResponse (DeprovisionByoipCidrResponse'),
    newDeprovisionByoipCidrResponse,

    -- ** DescribeAddressesAttribute (Paginated)
    DescribeAddressesAttribute (DescribeAddressesAttribute'),
    newDescribeAddressesAttribute,
    DescribeAddressesAttributeResponse (DescribeAddressesAttributeResponse'),
    newDescribeAddressesAttributeResponse,

    -- ** AcceptVpcPeeringConnection
    AcceptVpcPeeringConnection (AcceptVpcPeeringConnection'),
    newAcceptVpcPeeringConnection,
    AcceptVpcPeeringConnectionResponse (AcceptVpcPeeringConnectionResponse'),
    newAcceptVpcPeeringConnectionResponse,

    -- ** DescribeMovingAddresses (Paginated)
    DescribeMovingAddresses (DescribeMovingAddresses'),
    newDescribeMovingAddresses,
    DescribeMovingAddressesResponse (DescribeMovingAddressesResponse'),
    newDescribeMovingAddressesResponse,

    -- ** CreateVpcEndpointConnectionNotification
    CreateVpcEndpointConnectionNotification (CreateVpcEndpointConnectionNotification'),
    newCreateVpcEndpointConnectionNotification,
    CreateVpcEndpointConnectionNotificationResponse (CreateVpcEndpointConnectionNotificationResponse'),
    newCreateVpcEndpointConnectionNotificationResponse,

    -- ** DescribeFleetHistory
    DescribeFleetHistory (DescribeFleetHistory'),
    newDescribeFleetHistory,
    DescribeFleetHistoryResponse (DescribeFleetHistoryResponse'),
    newDescribeFleetHistoryResponse,

    -- ** DeleteVpcEndpointServiceConfigurations
    DeleteVpcEndpointServiceConfigurations (DeleteVpcEndpointServiceConfigurations'),
    newDeleteVpcEndpointServiceConfigurations,
    DeleteVpcEndpointServiceConfigurationsResponse (DeleteVpcEndpointServiceConfigurationsResponse'),
    newDeleteVpcEndpointServiceConfigurationsResponse,

    -- ** CreateVpc
    CreateVpc (CreateVpc'),
    newCreateVpc,
    CreateVpcResponse (CreateVpcResponse'),
    newCreateVpcResponse,

    -- ** SearchLocalGatewayRoutes (Paginated)
    SearchLocalGatewayRoutes (SearchLocalGatewayRoutes'),
    newSearchLocalGatewayRoutes,
    SearchLocalGatewayRoutesResponse (SearchLocalGatewayRoutesResponse'),
    newSearchLocalGatewayRoutesResponse,

    -- ** CreateTrafficMirrorTarget
    CreateTrafficMirrorTarget (CreateTrafficMirrorTarget'),
    newCreateTrafficMirrorTarget,
    CreateTrafficMirrorTargetResponse (CreateTrafficMirrorTargetResponse'),
    newCreateTrafficMirrorTargetResponse,

    -- ** DescribeVolumeStatus (Paginated)
    DescribeVolumeStatus (DescribeVolumeStatus'),
    newDescribeVolumeStatus,
    DescribeVolumeStatusResponse (DescribeVolumeStatusResponse'),
    newDescribeVolumeStatusResponse,

    -- ** DescribeVolumeAttribute
    DescribeVolumeAttribute (DescribeVolumeAttribute'),
    newDescribeVolumeAttribute,
    DescribeVolumeAttributeResponse (DescribeVolumeAttributeResponse'),
    newDescribeVolumeAttributeResponse,

    -- ** DeleteClientVpnRoute
    DeleteClientVpnRoute (DeleteClientVpnRoute'),
    newDeleteClientVpnRoute,
    DeleteClientVpnRouteResponse (DeleteClientVpnRouteResponse'),
    newDeleteClientVpnRouteResponse,

    -- ** ModifyVpcPeeringConnectionOptions
    ModifyVpcPeeringConnectionOptions (ModifyVpcPeeringConnectionOptions'),
    newModifyVpcPeeringConnectionOptions,
    ModifyVpcPeeringConnectionOptionsResponse (ModifyVpcPeeringConnectionOptionsResponse'),
    newModifyVpcPeeringConnectionOptionsResponse,

    -- ** DescribeSpotFleetInstances (Paginated)
    DescribeSpotFleetInstances (DescribeSpotFleetInstances'),
    newDescribeSpotFleetInstances,
    DescribeSpotFleetInstancesResponse (DescribeSpotFleetInstancesResponse'),
    newDescribeSpotFleetInstancesResponse,

    -- ** DescribePrincipalIdFormat (Paginated)
    DescribePrincipalIdFormat (DescribePrincipalIdFormat'),
    newDescribePrincipalIdFormat,
    DescribePrincipalIdFormatResponse (DescribePrincipalIdFormatResponse'),
    newDescribePrincipalIdFormatResponse,

    -- ** ModifyInstanceCreditSpecification
    ModifyInstanceCreditSpecification (ModifyInstanceCreditSpecification'),
    newModifyInstanceCreditSpecification,
    ModifyInstanceCreditSpecificationResponse (ModifyInstanceCreditSpecificationResponse'),
    newModifyInstanceCreditSpecificationResponse,

    -- ** DisassociateTransitGatewayMulticastDomain
    DisassociateTransitGatewayMulticastDomain (DisassociateTransitGatewayMulticastDomain'),
    newDisassociateTransitGatewayMulticastDomain,
    DisassociateTransitGatewayMulticastDomainResponse (DisassociateTransitGatewayMulticastDomainResponse'),
    newDisassociateTransitGatewayMulticastDomainResponse,

    -- ** DescribeManagedPrefixLists (Paginated)
    DescribeManagedPrefixLists (DescribeManagedPrefixLists'),
    newDescribeManagedPrefixLists,
    DescribeManagedPrefixListsResponse (DescribeManagedPrefixListsResponse'),
    newDescribeManagedPrefixListsResponse,

    -- ** GetPasswordData
    GetPasswordData (GetPasswordData'),
    newGetPasswordData,
    GetPasswordDataResponse (GetPasswordDataResponse'),
    newGetPasswordDataResponse,

    -- ** DeleteVolume
    DeleteVolume (DeleteVolume'),
    newDeleteVolume,
    DeleteVolumeResponse (DeleteVolumeResponse'),
    newDeleteVolumeResponse,

    -- ** DescribeTransitGateways (Paginated)
    DescribeTransitGateways (DescribeTransitGateways'),
    newDescribeTransitGateways,
    DescribeTransitGatewaysResponse (DescribeTransitGatewaysResponse'),
    newDescribeTransitGatewaysResponse,

    -- ** DescribeSpotFleetRequests (Paginated)
    DescribeSpotFleetRequests (DescribeSpotFleetRequests'),
    newDescribeSpotFleetRequests,
    DescribeSpotFleetRequestsResponse (DescribeSpotFleetRequestsResponse'),
    newDescribeSpotFleetRequestsResponse,

    -- ** DescribeClientVpnConnections (Paginated)
    DescribeClientVpnConnections (DescribeClientVpnConnections'),
    newDescribeClientVpnConnections,
    DescribeClientVpnConnectionsResponse (DescribeClientVpnConnectionsResponse'),
    newDescribeClientVpnConnectionsResponse,

    -- ** SearchTransitGatewayMulticastGroups (Paginated)
    SearchTransitGatewayMulticastGroups (SearchTransitGatewayMulticastGroups'),
    newSearchTransitGatewayMulticastGroups,
    SearchTransitGatewayMulticastGroupsResponse (SearchTransitGatewayMulticastGroupsResponse'),
    newSearchTransitGatewayMulticastGroupsResponse,

    -- ** ModifyVpcAttribute
    ModifyVpcAttribute (ModifyVpcAttribute'),
    newModifyVpcAttribute,
    ModifyVpcAttributeResponse (ModifyVpcAttributeResponse'),
    newModifyVpcAttributeResponse,

    -- ** RevokeSecurityGroupIngress
    RevokeSecurityGroupIngress (RevokeSecurityGroupIngress'),
    newRevokeSecurityGroupIngress,
    RevokeSecurityGroupIngressResponse (RevokeSecurityGroupIngressResponse'),
    newRevokeSecurityGroupIngressResponse,

    -- ** DescribeHostReservationOfferings (Paginated)
    DescribeHostReservationOfferings (DescribeHostReservationOfferings'),
    newDescribeHostReservationOfferings,
    DescribeHostReservationOfferingsResponse (DescribeHostReservationOfferingsResponse'),
    newDescribeHostReservationOfferingsResponse,

    -- ** DescribeTransitGatewayRouteTables (Paginated)
    DescribeTransitGatewayRouteTables (DescribeTransitGatewayRouteTables'),
    newDescribeTransitGatewayRouteTables,
    DescribeTransitGatewayRouteTablesResponse (DescribeTransitGatewayRouteTablesResponse'),
    newDescribeTransitGatewayRouteTablesResponse,

    -- ** DescribeNetworkAcls (Paginated)
    DescribeNetworkAcls (DescribeNetworkAcls'),
    newDescribeNetworkAcls,
    DescribeNetworkAclsResponse (DescribeNetworkAclsResponse'),
    newDescribeNetworkAclsResponse,

    -- ** RegisterTransitGatewayMulticastGroupMembers
    RegisterTransitGatewayMulticastGroupMembers (RegisterTransitGatewayMulticastGroupMembers'),
    newRegisterTransitGatewayMulticastGroupMembers,
    RegisterTransitGatewayMulticastGroupMembersResponse (RegisterTransitGatewayMulticastGroupMembersResponse'),
    newRegisterTransitGatewayMulticastGroupMembersResponse,

    -- ** DescribeHosts (Paginated)
    DescribeHosts (DescribeHosts'),
    newDescribeHosts,
    DescribeHostsResponse (DescribeHostsResponse'),
    newDescribeHostsResponse,

    -- ** DescribeVpnGateways
    DescribeVpnGateways (DescribeVpnGateways'),
    newDescribeVpnGateways,
    DescribeVpnGatewaysResponse (DescribeVpnGatewaysResponse'),
    newDescribeVpnGatewaysResponse,

    -- ** DescribeHostReservations (Paginated)
    DescribeHostReservations (DescribeHostReservations'),
    newDescribeHostReservations,
    DescribeHostReservationsResponse (DescribeHostReservationsResponse'),
    newDescribeHostReservationsResponse,

    -- ** DeleteManagedPrefixList
    DeleteManagedPrefixList (DeleteManagedPrefixList'),
    newDeleteManagedPrefixList,
    DeleteManagedPrefixListResponse (DeleteManagedPrefixListResponse'),
    newDeleteManagedPrefixListResponse,

    -- ** RejectVpcPeeringConnection
    RejectVpcPeeringConnection (RejectVpcPeeringConnection'),
    newRejectVpcPeeringConnection,
    RejectVpcPeeringConnectionResponse (RejectVpcPeeringConnectionResponse'),
    newRejectVpcPeeringConnectionResponse,

    -- ** ResetImageAttribute
    ResetImageAttribute (ResetImageAttribute'),
    newResetImageAttribute,
    ResetImageAttributeResponse (ResetImageAttributeResponse'),
    newResetImageAttributeResponse,

    -- ** DescribeScheduledInstances (Paginated)
    DescribeScheduledInstances (DescribeScheduledInstances'),
    newDescribeScheduledInstances,
    DescribeScheduledInstancesResponse (DescribeScheduledInstancesResponse'),
    newDescribeScheduledInstancesResponse,

    -- ** AssociateEnclaveCertificateIamRole
    AssociateEnclaveCertificateIamRole (AssociateEnclaveCertificateIamRole'),
    newAssociateEnclaveCertificateIamRole,
    AssociateEnclaveCertificateIamRoleResponse (AssociateEnclaveCertificateIamRoleResponse'),
    newAssociateEnclaveCertificateIamRoleResponse,

    -- ** ModifyTransitGatewayPrefixListReference
    ModifyTransitGatewayPrefixListReference (ModifyTransitGatewayPrefixListReference'),
    newModifyTransitGatewayPrefixListReference,
    ModifyTransitGatewayPrefixListReferenceResponse (ModifyTransitGatewayPrefixListReferenceResponse'),
    newModifyTransitGatewayPrefixListReferenceResponse,

    -- ** DescribeFpgaImageAttribute
    DescribeFpgaImageAttribute (DescribeFpgaImageAttribute'),
    newDescribeFpgaImageAttribute,
    DescribeFpgaImageAttributeResponse (DescribeFpgaImageAttributeResponse'),
    newDescribeFpgaImageAttributeResponse,

    -- ** AdvertiseByoipCidr
    AdvertiseByoipCidr (AdvertiseByoipCidr'),
    newAdvertiseByoipCidr,
    AdvertiseByoipCidrResponse (AdvertiseByoipCidrResponse'),
    newAdvertiseByoipCidrResponse,

    -- ** DeleteVpnConnectionRoute
    DeleteVpnConnectionRoute (DeleteVpnConnectionRoute'),
    newDeleteVpnConnectionRoute,
    DeleteVpnConnectionRouteResponse (DeleteVpnConnectionRouteResponse'),
    newDeleteVpnConnectionRouteResponse,

    -- ** DescribeVpcEndpointServicePermissions (Paginated)
    DescribeVpcEndpointServicePermissions (DescribeVpcEndpointServicePermissions'),
    newDescribeVpcEndpointServicePermissions,
    DescribeVpcEndpointServicePermissionsResponse (DescribeVpcEndpointServicePermissionsResponse'),
    newDescribeVpcEndpointServicePermissionsResponse,

    -- ** DescribeVpcEndpointConnections (Paginated)
    DescribeVpcEndpointConnections (DescribeVpcEndpointConnections'),
    newDescribeVpcEndpointConnections,
    DescribeVpcEndpointConnectionsResponse (DescribeVpcEndpointConnectionsResponse'),
    newDescribeVpcEndpointConnectionsResponse,

    -- ** DescribeNetworkInterfacePermissions (Paginated)
    DescribeNetworkInterfacePermissions (DescribeNetworkInterfacePermissions'),
    newDescribeNetworkInterfacePermissions,
    DescribeNetworkInterfacePermissionsResponse (DescribeNetworkInterfacePermissionsResponse'),
    newDescribeNetworkInterfacePermissionsResponse,

    -- ** CreateTrafficMirrorSession
    CreateTrafficMirrorSession (CreateTrafficMirrorSession'),
    newCreateTrafficMirrorSession,
    CreateTrafficMirrorSessionResponse (CreateTrafficMirrorSessionResponse'),
    newCreateTrafficMirrorSessionResponse,

    -- ** RegisterInstanceEventNotificationAttributes
    RegisterInstanceEventNotificationAttributes (RegisterInstanceEventNotificationAttributes'),
    newRegisterInstanceEventNotificationAttributes,
    RegisterInstanceEventNotificationAttributesResponse (RegisterInstanceEventNotificationAttributesResponse'),
    newRegisterInstanceEventNotificationAttributesResponse,

    -- ** RejectTransitGatewayMulticastDomainAssociations
    RejectTransitGatewayMulticastDomainAssociations (RejectTransitGatewayMulticastDomainAssociations'),
    newRejectTransitGatewayMulticastDomainAssociations,
    RejectTransitGatewayMulticastDomainAssociationsResponse (RejectTransitGatewayMulticastDomainAssociationsResponse'),
    newRejectTransitGatewayMulticastDomainAssociationsResponse,

    -- ** DeleteDhcpOptions
    DeleteDhcpOptions (DeleteDhcpOptions'),
    newDeleteDhcpOptions,
    DeleteDhcpOptionsResponse (DeleteDhcpOptionsResponse'),
    newDeleteDhcpOptionsResponse,

    -- ** DeleteTransitGateway
    DeleteTransitGateway (DeleteTransitGateway'),
    newDeleteTransitGateway,
    DeleteTransitGatewayResponse (DeleteTransitGatewayResponse'),
    newDeleteTransitGatewayResponse,

    -- ** EnableVpcClassicLinkDnsSupport
    EnableVpcClassicLinkDnsSupport (EnableVpcClassicLinkDnsSupport'),
    newEnableVpcClassicLinkDnsSupport,
    EnableVpcClassicLinkDnsSupportResponse (EnableVpcClassicLinkDnsSupportResponse'),
    newEnableVpcClassicLinkDnsSupportResponse,

    -- ** DescribeRegions
    DescribeRegions (DescribeRegions'),
    newDescribeRegions,
    DescribeRegionsResponse (DescribeRegionsResponse'),
    newDescribeRegionsResponse,

    -- ** CreateEgressOnlyInternetGateway
    CreateEgressOnlyInternetGateway (CreateEgressOnlyInternetGateway'),
    newCreateEgressOnlyInternetGateway,
    CreateEgressOnlyInternetGatewayResponse (CreateEgressOnlyInternetGatewayResponse'),
    newCreateEgressOnlyInternetGatewayResponse,

    -- ** CreateTransitGateway
    CreateTransitGateway (CreateTransitGateway'),
    newCreateTransitGateway,
    CreateTransitGatewayResponse (CreateTransitGatewayResponse'),
    newCreateTransitGatewayResponse,

    -- ** DeleteClientVpnEndpoint
    DeleteClientVpnEndpoint (DeleteClientVpnEndpoint'),
    newDeleteClientVpnEndpoint,
    DeleteClientVpnEndpointResponse (DeleteClientVpnEndpointResponse'),
    newDeleteClientVpnEndpointResponse,

    -- ** ExportClientVpnClientCertificateRevocationList
    ExportClientVpnClientCertificateRevocationList (ExportClientVpnClientCertificateRevocationList'),
    newExportClientVpnClientCertificateRevocationList,
    ExportClientVpnClientCertificateRevocationListResponse (ExportClientVpnClientCertificateRevocationListResponse'),
    newExportClientVpnClientCertificateRevocationListResponse,

    -- ** CreateLaunchTemplateVersion
    CreateLaunchTemplateVersion (CreateLaunchTemplateVersion'),
    newCreateLaunchTemplateVersion,
    CreateLaunchTemplateVersionResponse (CreateLaunchTemplateVersionResponse'),
    newCreateLaunchTemplateVersionResponse,

    -- ** CreateSnapshots
    CreateSnapshots (CreateSnapshots'),
    newCreateSnapshots,
    CreateSnapshotsResponse (CreateSnapshotsResponse'),
    newCreateSnapshotsResponse,

    -- ** ModifyDefaultCreditSpecification
    ModifyDefaultCreditSpecification (ModifyDefaultCreditSpecification'),
    newModifyDefaultCreditSpecification,
    ModifyDefaultCreditSpecificationResponse (ModifyDefaultCreditSpecificationResponse'),
    newModifyDefaultCreditSpecificationResponse,

    -- ** ApplySecurityGroupsToClientVpnTargetNetwork
    ApplySecurityGroupsToClientVpnTargetNetwork (ApplySecurityGroupsToClientVpnTargetNetwork'),
    newApplySecurityGroupsToClientVpnTargetNetwork,
    ApplySecurityGroupsToClientVpnTargetNetworkResponse (ApplySecurityGroupsToClientVpnTargetNetworkResponse'),
    newApplySecurityGroupsToClientVpnTargetNetworkResponse,

    -- ** AttachVpnGateway
    AttachVpnGateway (AttachVpnGateway'),
    newAttachVpnGateway,
    AttachVpnGatewayResponse (AttachVpnGatewayResponse'),
    newAttachVpnGatewayResponse,

    -- ** CreateVpnConnectionRoute
    CreateVpnConnectionRoute (CreateVpnConnectionRoute'),
    newCreateVpnConnectionRoute,
    CreateVpnConnectionRouteResponse (CreateVpnConnectionRouteResponse'),
    newCreateVpnConnectionRouteResponse,

    -- ** DescribeKeyPairs
    DescribeKeyPairs (DescribeKeyPairs'),
    newDescribeKeyPairs,
    DescribeKeyPairsResponse (DescribeKeyPairsResponse'),
    newDescribeKeyPairsResponse,

    -- ** AllocateAddress
    AllocateAddress (AllocateAddress'),
    newAllocateAddress,
    AllocateAddressResponse (AllocateAddressResponse'),
    newAllocateAddressResponse,

    -- ** DeleteTrafficMirrorSession
    DeleteTrafficMirrorSession (DeleteTrafficMirrorSession'),
    newDeleteTrafficMirrorSession,
    DeleteTrafficMirrorSessionResponse (DeleteTrafficMirrorSessionResponse'),
    newDeleteTrafficMirrorSessionResponse,

    -- ** GetManagedPrefixListEntries (Paginated)
    GetManagedPrefixListEntries (GetManagedPrefixListEntries'),
    newGetManagedPrefixListEntries,
    GetManagedPrefixListEntriesResponse (GetManagedPrefixListEntriesResponse'),
    newGetManagedPrefixListEntriesResponse,

    -- ** CreateFpgaImage
    CreateFpgaImage (CreateFpgaImage'),
    newCreateFpgaImage,
    CreateFpgaImageResponse (CreateFpgaImageResponse'),
    newCreateFpgaImageResponse,

    -- ** ExportImage
    ExportImage (ExportImage'),
    newExportImage,
    ExportImageResponse (ExportImageResponse'),
    newExportImageResponse,

    -- ** RejectTransitGatewayPeeringAttachment
    RejectTransitGatewayPeeringAttachment (RejectTransitGatewayPeeringAttachment'),
    newRejectTransitGatewayPeeringAttachment,
    RejectTransitGatewayPeeringAttachmentResponse (RejectTransitGatewayPeeringAttachmentResponse'),
    newRejectTransitGatewayPeeringAttachmentResponse,

    -- ** DescribeConversionTasks
    DescribeConversionTasks (DescribeConversionTasks'),
    newDescribeConversionTasks,
    DescribeConversionTasksResponse (DescribeConversionTasksResponse'),
    newDescribeConversionTasksResponse,

    -- ** WithdrawByoipCidr
    WithdrawByoipCidr (WithdrawByoipCidr'),
    newWithdrawByoipCidr,
    WithdrawByoipCidrResponse (WithdrawByoipCidrResponse'),
    newWithdrawByoipCidrResponse,

    -- ** DeleteTrafficMirrorFilterRule
    DeleteTrafficMirrorFilterRule (DeleteTrafficMirrorFilterRule'),
    newDeleteTrafficMirrorFilterRule,
    DeleteTrafficMirrorFilterRuleResponse (DeleteTrafficMirrorFilterRuleResponse'),
    newDeleteTrafficMirrorFilterRuleResponse,

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

    -- ** AcceptTransitGatewayVpcAttachment
    AcceptTransitGatewayVpcAttachment (AcceptTransitGatewayVpcAttachment'),
    newAcceptTransitGatewayVpcAttachment,
    AcceptTransitGatewayVpcAttachmentResponse (AcceptTransitGatewayVpcAttachmentResponse'),
    newAcceptTransitGatewayVpcAttachmentResponse,

    -- ** DisableVpcClassicLinkDnsSupport
    DisableVpcClassicLinkDnsSupport (DisableVpcClassicLinkDnsSupport'),
    newDisableVpcClassicLinkDnsSupport,
    DisableVpcClassicLinkDnsSupportResponse (DisableVpcClassicLinkDnsSupportResponse'),
    newDisableVpcClassicLinkDnsSupportResponse,

    -- ** GetLaunchTemplateData
    GetLaunchTemplateData (GetLaunchTemplateData'),
    newGetLaunchTemplateData,
    GetLaunchTemplateDataResponse (GetLaunchTemplateDataResponse'),
    newGetLaunchTemplateDataResponse,

    -- ** DescribeReservedInstances
    DescribeReservedInstances (DescribeReservedInstances'),
    newDescribeReservedInstances,
    DescribeReservedInstancesResponse (DescribeReservedInstancesResponse'),
    newDescribeReservedInstancesResponse,

    -- ** ModifyFpgaImageAttribute
    ModifyFpgaImageAttribute (ModifyFpgaImageAttribute'),
    newModifyFpgaImageAttribute,
    ModifyFpgaImageAttributeResponse (ModifyFpgaImageAttributeResponse'),
    newModifyFpgaImageAttributeResponse,

    -- ** EnableVpcClassicLink
    EnableVpcClassicLink (EnableVpcClassicLink'),
    newEnableVpcClassicLink,
    EnableVpcClassicLinkResponse (EnableVpcClassicLinkResponse'),
    newEnableVpcClassicLinkResponse,

    -- ** AttachInternetGateway
    AttachInternetGateway (AttachInternetGateway'),
    newAttachInternetGateway,
    AttachInternetGatewayResponse (AttachInternetGatewayResponse'),
    newAttachInternetGatewayResponse,

    -- ** DescribePublicIpv4Pools (Paginated)
    DescribePublicIpv4Pools (DescribePublicIpv4Pools'),
    newDescribePublicIpv4Pools,
    DescribePublicIpv4PoolsResponse (DescribePublicIpv4PoolsResponse'),
    newDescribePublicIpv4PoolsResponse,

    -- ** CreateCustomerGateway
    CreateCustomerGateway (CreateCustomerGateway'),
    newCreateCustomerGateway,
    CreateCustomerGatewayResponse (CreateCustomerGatewayResponse'),
    newCreateCustomerGatewayResponse,

    -- ** DescribeIamInstanceProfileAssociations (Paginated)
    DescribeIamInstanceProfileAssociations (DescribeIamInstanceProfileAssociations'),
    newDescribeIamInstanceProfileAssociations,
    DescribeIamInstanceProfileAssociationsResponse (DescribeIamInstanceProfileAssociationsResponse'),
    newDescribeIamInstanceProfileAssociationsResponse,

    -- ** DescribeExportImageTasks (Paginated)
    DescribeExportImageTasks (DescribeExportImageTasks'),
    newDescribeExportImageTasks,
    DescribeExportImageTasksResponse (DescribeExportImageTasksResponse'),
    newDescribeExportImageTasksResponse,

    -- ** ProvisionByoipCidr
    ProvisionByoipCidr (ProvisionByoipCidr'),
    newProvisionByoipCidr,
    ProvisionByoipCidrResponse (ProvisionByoipCidrResponse'),
    newProvisionByoipCidrResponse,

    -- ** CreateReservedInstancesListing
    CreateReservedInstancesListing (CreateReservedInstancesListing'),
    newCreateReservedInstancesListing,
    CreateReservedInstancesListingResponse (CreateReservedInstancesListingResponse'),
    newCreateReservedInstancesListingResponse,

    -- ** DescribeClientVpnTargetNetworks (Paginated)
    DescribeClientVpnTargetNetworks (DescribeClientVpnTargetNetworks'),
    newDescribeClientVpnTargetNetworks,
    DescribeClientVpnTargetNetworksResponse (DescribeClientVpnTargetNetworksResponse'),
    newDescribeClientVpnTargetNetworksResponse,

    -- ** ModifyVpnTunnelOptions
    ModifyVpnTunnelOptions (ModifyVpnTunnelOptions'),
    newModifyVpnTunnelOptions,
    ModifyVpnTunnelOptionsResponse (ModifyVpnTunnelOptionsResponse'),
    newModifyVpnTunnelOptionsResponse,

    -- ** ModifyInstancePlacement
    ModifyInstancePlacement (ModifyInstancePlacement'),
    newModifyInstancePlacement,
    ModifyInstancePlacementResponse (ModifyInstancePlacementResponse'),
    newModifyInstancePlacementResponse,

    -- ** ImportKeyPair
    ImportKeyPair (ImportKeyPair'),
    newImportKeyPair,
    ImportKeyPairResponse (ImportKeyPairResponse'),
    newImportKeyPairResponse,

    -- ** DescribeNetworkInsightsAnalyses (Paginated)
    DescribeNetworkInsightsAnalyses (DescribeNetworkInsightsAnalyses'),
    newDescribeNetworkInsightsAnalyses,
    DescribeNetworkInsightsAnalysesResponse (DescribeNetworkInsightsAnalysesResponse'),
    newDescribeNetworkInsightsAnalysesResponse,

    -- ** DeleteSecurityGroup
    DeleteSecurityGroup (DeleteSecurityGroup'),
    newDeleteSecurityGroup,
    DeleteSecurityGroupResponse (DeleteSecurityGroupResponse'),
    newDeleteSecurityGroupResponse,

    -- ** CreateCarrierGateway
    CreateCarrierGateway (CreateCarrierGateway'),
    newCreateCarrierGateway,
    CreateCarrierGatewayResponse (CreateCarrierGatewayResponse'),
    newCreateCarrierGatewayResponse,

    -- ** CreateSnapshot
    CreateSnapshot (CreateSnapshot'),
    newCreateSnapshot,
    Snapshot (Snapshot'),
    newSnapshot,

    -- ** ModifyVolume
    ModifyVolume (ModifyVolume'),
    newModifyVolume,
    ModifyVolumeResponse (ModifyVolumeResponse'),
    newModifyVolumeResponse,

    -- ** DeleteNetworkInsightsAnalysis
    DeleteNetworkInsightsAnalysis (DeleteNetworkInsightsAnalysis'),
    newDeleteNetworkInsightsAnalysis,
    DeleteNetworkInsightsAnalysisResponse (DeleteNetworkInsightsAnalysisResponse'),
    newDeleteNetworkInsightsAnalysisResponse,

    -- ** DescribeLocalGatewayRouteTableVpcAssociations (Paginated)
    DescribeLocalGatewayRouteTableVpcAssociations (DescribeLocalGatewayRouteTableVpcAssociations'),
    newDescribeLocalGatewayRouteTableVpcAssociations,
    DescribeLocalGatewayRouteTableVpcAssociationsResponse (DescribeLocalGatewayRouteTableVpcAssociationsResponse'),
    newDescribeLocalGatewayRouteTableVpcAssociationsResponse,

    -- ** CreateTrafficMirrorFilter
    CreateTrafficMirrorFilter (CreateTrafficMirrorFilter'),
    newCreateTrafficMirrorFilter,
    CreateTrafficMirrorFilterResponse (CreateTrafficMirrorFilterResponse'),
    newCreateTrafficMirrorFilterResponse,

    -- ** DeleteSpotDatafeedSubscription
    DeleteSpotDatafeedSubscription (DeleteSpotDatafeedSubscription'),
    newDeleteSpotDatafeedSubscription,
    DeleteSpotDatafeedSubscriptionResponse (DeleteSpotDatafeedSubscriptionResponse'),
    newDeleteSpotDatafeedSubscriptionResponse,

    -- ** DescribeInstanceAttribute
    DescribeInstanceAttribute (DescribeInstanceAttribute'),
    newDescribeInstanceAttribute,
    DescribeInstanceAttributeResponse (DescribeInstanceAttributeResponse'),
    newDescribeInstanceAttributeResponse,

    -- ** CreateCapacityReservation
    CreateCapacityReservation (CreateCapacityReservation'),
    newCreateCapacityReservation,
    CreateCapacityReservationResponse (CreateCapacityReservationResponse'),
    newCreateCapacityReservationResponse,

    -- ** DeleteTransitGatewayConnect
    DeleteTransitGatewayConnect (DeleteTransitGatewayConnect'),
    newDeleteTransitGatewayConnect,
    DeleteTransitGatewayConnectResponse (DeleteTransitGatewayConnectResponse'),
    newDeleteTransitGatewayConnectResponse,

    -- ** ModifyEbsDefaultKmsKeyId
    ModifyEbsDefaultKmsKeyId (ModifyEbsDefaultKmsKeyId'),
    newModifyEbsDefaultKmsKeyId,
    ModifyEbsDefaultKmsKeyIdResponse (ModifyEbsDefaultKmsKeyIdResponse'),
    newModifyEbsDefaultKmsKeyIdResponse,

    -- ** DeleteRoute
    DeleteRoute (DeleteRoute'),
    newDeleteRoute,
    DeleteRouteResponse (DeleteRouteResponse'),
    newDeleteRouteResponse,

    -- ** DescribeNetworkInsightsPaths (Paginated)
    DescribeNetworkInsightsPaths (DescribeNetworkInsightsPaths'),
    newDescribeNetworkInsightsPaths,
    DescribeNetworkInsightsPathsResponse (DescribeNetworkInsightsPathsResponse'),
    newDescribeNetworkInsightsPathsResponse,

    -- ** PurchaseScheduledInstances
    PurchaseScheduledInstances (PurchaseScheduledInstances'),
    newPurchaseScheduledInstances,
    PurchaseScheduledInstancesResponse (PurchaseScheduledInstancesResponse'),
    newPurchaseScheduledInstancesResponse,

    -- ** CreateTransitGatewayPeeringAttachment
    CreateTransitGatewayPeeringAttachment (CreateTransitGatewayPeeringAttachment'),
    newCreateTransitGatewayPeeringAttachment,
    CreateTransitGatewayPeeringAttachmentResponse (CreateTransitGatewayPeeringAttachmentResponse'),
    newCreateTransitGatewayPeeringAttachmentResponse,

    -- ** GetDefaultCreditSpecification
    GetDefaultCreditSpecification (GetDefaultCreditSpecification'),
    newGetDefaultCreditSpecification,
    GetDefaultCreditSpecificationResponse (GetDefaultCreditSpecificationResponse'),
    newGetDefaultCreditSpecificationResponse,

    -- ** DescribeInternetGateways (Paginated)
    DescribeInternetGateways (DescribeInternetGateways'),
    newDescribeInternetGateways,
    DescribeInternetGatewaysResponse (DescribeInternetGatewaysResponse'),
    newDescribeInternetGatewaysResponse,

    -- ** ModifyInstanceAttribute
    ModifyInstanceAttribute (ModifyInstanceAttribute'),
    newModifyInstanceAttribute,
    ModifyInstanceAttributeResponse (ModifyInstanceAttributeResponse'),
    newModifyInstanceAttributeResponse,

    -- ** CreateSecurityGroup
    CreateSecurityGroup (CreateSecurityGroup'),
    newCreateSecurityGroup,
    CreateSecurityGroupResponse (CreateSecurityGroupResponse'),
    newCreateSecurityGroupResponse,

    -- ** CreateTransitGatewayConnect
    CreateTransitGatewayConnect (CreateTransitGatewayConnect'),
    newCreateTransitGatewayConnect,
    CreateTransitGatewayConnectResponse (CreateTransitGatewayConnectResponse'),
    newCreateTransitGatewayConnectResponse,

    -- ** ReplaceNetworkAclAssociation
    ReplaceNetworkAclAssociation (ReplaceNetworkAclAssociation'),
    newReplaceNetworkAclAssociation,
    ReplaceNetworkAclAssociationResponse (ReplaceNetworkAclAssociationResponse'),
    newReplaceNetworkAclAssociationResponse,

    -- ** CreateRoute
    CreateRoute (CreateRoute'),
    newCreateRoute,
    CreateRouteResponse (CreateRouteResponse'),
    newCreateRouteResponse,

    -- ** DeleteLaunchTemplateVersions
    DeleteLaunchTemplateVersions (DeleteLaunchTemplateVersions'),
    newDeleteLaunchTemplateVersions,
    DeleteLaunchTemplateVersionsResponse (DeleteLaunchTemplateVersionsResponse'),
    newDeleteLaunchTemplateVersionsResponse,

    -- ** DescribeIdentityIdFormat
    DescribeIdentityIdFormat (DescribeIdentityIdFormat'),
    newDescribeIdentityIdFormat,
    DescribeIdentityIdFormatResponse (DescribeIdentityIdFormatResponse'),
    newDescribeIdentityIdFormatResponse,

    -- ** DeleteTrafficMirrorFilter
    DeleteTrafficMirrorFilter (DeleteTrafficMirrorFilter'),
    newDeleteTrafficMirrorFilter,
    DeleteTrafficMirrorFilterResponse (DeleteTrafficMirrorFilterResponse'),
    newDeleteTrafficMirrorFilterResponse,

    -- ** ReplaceRoute
    ReplaceRoute (ReplaceRoute'),
    newReplaceRoute,
    ReplaceRouteResponse (ReplaceRouteResponse'),
    newReplaceRouteResponse,

    -- ** ResetSnapshotAttribute
    ResetSnapshotAttribute (ResetSnapshotAttribute'),
    newResetSnapshotAttribute,
    ResetSnapshotAttributeResponse (ResetSnapshotAttributeResponse'),
    newResetSnapshotAttributeResponse,

    -- ** ResetEbsDefaultKmsKeyId
    ResetEbsDefaultKmsKeyId (ResetEbsDefaultKmsKeyId'),
    newResetEbsDefaultKmsKeyId,
    ResetEbsDefaultKmsKeyIdResponse (ResetEbsDefaultKmsKeyIdResponse'),
    newResetEbsDefaultKmsKeyIdResponse,

    -- ** CreateTags
    CreateTags (CreateTags'),
    newCreateTags,
    CreateTagsResponse (CreateTagsResponse'),
    newCreateTagsResponse,

    -- ** BundleInstance
    BundleInstance (BundleInstance'),
    newBundleInstance,
    BundleInstanceResponse (BundleInstanceResponse'),
    newBundleInstanceResponse,

    -- ** DeleteTransitGatewayPeeringAttachment
    DeleteTransitGatewayPeeringAttachment (DeleteTransitGatewayPeeringAttachment'),
    newDeleteTransitGatewayPeeringAttachment,
    DeleteTransitGatewayPeeringAttachmentResponse (DeleteTransitGatewayPeeringAttachmentResponse'),
    newDeleteTransitGatewayPeeringAttachmentResponse,

    -- ** AuthorizeClientVpnIngress
    AuthorizeClientVpnIngress (AuthorizeClientVpnIngress'),
    newAuthorizeClientVpnIngress,
    AuthorizeClientVpnIngressResponse (AuthorizeClientVpnIngressResponse'),
    newAuthorizeClientVpnIngressResponse,

    -- ** ModifyLaunchTemplate
    ModifyLaunchTemplate (ModifyLaunchTemplate'),
    newModifyLaunchTemplate,
    ModifyLaunchTemplateResponse (ModifyLaunchTemplateResponse'),
    newModifyLaunchTemplateResponse,

    -- ** DeleteCustomerGateway
    DeleteCustomerGateway (DeleteCustomerGateway'),
    newDeleteCustomerGateway,
    DeleteCustomerGatewayResponse (DeleteCustomerGatewayResponse'),
    newDeleteCustomerGatewayResponse,

    -- ** TerminateClientVpnConnections
    TerminateClientVpnConnections (TerminateClientVpnConnections'),
    newTerminateClientVpnConnections,
    TerminateClientVpnConnectionsResponse (TerminateClientVpnConnectionsResponse'),
    newTerminateClientVpnConnectionsResponse,

    -- ** GetEbsEncryptionByDefault
    GetEbsEncryptionByDefault (GetEbsEncryptionByDefault'),
    newGetEbsEncryptionByDefault,
    GetEbsEncryptionByDefaultResponse (GetEbsEncryptionByDefaultResponse'),
    newGetEbsEncryptionByDefaultResponse,

    -- ** CreateVpcPeeringConnection
    CreateVpcPeeringConnection (CreateVpcPeeringConnection'),
    newCreateVpcPeeringConnection,
    CreateVpcPeeringConnectionResponse (CreateVpcPeeringConnectionResponse'),
    newCreateVpcPeeringConnectionResponse,

    -- ** DeleteTransitGatewayVpcAttachment
    DeleteTransitGatewayVpcAttachment (DeleteTransitGatewayVpcAttachment'),
    newDeleteTransitGatewayVpcAttachment,
    DeleteTransitGatewayVpcAttachmentResponse (DeleteTransitGatewayVpcAttachmentResponse'),
    newDeleteTransitGatewayVpcAttachmentResponse,

    -- ** ReplaceIamInstanceProfileAssociation
    ReplaceIamInstanceProfileAssociation (ReplaceIamInstanceProfileAssociation'),
    newReplaceIamInstanceProfileAssociation,
    ReplaceIamInstanceProfileAssociationResponse (ReplaceIamInstanceProfileAssociationResponse'),
    newReplaceIamInstanceProfileAssociationResponse,

    -- ** DeleteTransitGatewayConnectPeer
    DeleteTransitGatewayConnectPeer (DeleteTransitGatewayConnectPeer'),
    newDeleteTransitGatewayConnectPeer,
    DeleteTransitGatewayConnectPeerResponse (DeleteTransitGatewayConnectPeerResponse'),
    newDeleteTransitGatewayConnectPeerResponse,

    -- ** AssociateAddress
    AssociateAddress (AssociateAddress'),
    newAssociateAddress,
    AssociateAddressResponse (AssociateAddressResponse'),
    newAssociateAddressResponse,

    -- ** CancelSpotFleetRequests
    CancelSpotFleetRequests (CancelSpotFleetRequests'),
    newCancelSpotFleetRequests,
    CancelSpotFleetRequestsResponse (CancelSpotFleetRequestsResponse'),
    newCancelSpotFleetRequestsResponse,

    -- ** ResetNetworkInterfaceAttribute
    ResetNetworkInterfaceAttribute (ResetNetworkInterfaceAttribute'),
    newResetNetworkInterfaceAttribute,
    ResetNetworkInterfaceAttributeResponse (ResetNetworkInterfaceAttributeResponse'),
    newResetNetworkInterfaceAttributeResponse,

    -- ** StartInstances
    StartInstances (StartInstances'),
    newStartInstances,
    StartInstancesResponse (StartInstancesResponse'),
    newStartInstancesResponse,

    -- ** DisassociateTransitGatewayRouteTable
    DisassociateTransitGatewayRouteTable (DisassociateTransitGatewayRouteTable'),
    newDisassociateTransitGatewayRouteTable,
    DisassociateTransitGatewayRouteTableResponse (DisassociateTransitGatewayRouteTableResponse'),
    newDisassociateTransitGatewayRouteTableResponse,

    -- ** CopyFpgaImage
    CopyFpgaImage (CopyFpgaImage'),
    newCopyFpgaImage,
    CopyFpgaImageResponse (CopyFpgaImageResponse'),
    newCopyFpgaImageResponse,

    -- ** ReleaseHosts
    ReleaseHosts (ReleaseHosts'),
    newReleaseHosts,
    ReleaseHostsResponse (ReleaseHostsResponse'),
    newReleaseHostsResponse,

    -- ** DescribeFastSnapshotRestores (Paginated)
    DescribeFastSnapshotRestores (DescribeFastSnapshotRestores'),
    newDescribeFastSnapshotRestores,
    DescribeFastSnapshotRestoresResponse (DescribeFastSnapshotRestoresResponse'),
    newDescribeFastSnapshotRestoresResponse,

    -- ** DescribeTrafficMirrorFilters (Paginated)
    DescribeTrafficMirrorFilters (DescribeTrafficMirrorFilters'),
    newDescribeTrafficMirrorFilters,
    DescribeTrafficMirrorFiltersResponse (DescribeTrafficMirrorFiltersResponse'),
    newDescribeTrafficMirrorFiltersResponse,

    -- ** CreateTransitGatewayPrefixListReference
    CreateTransitGatewayPrefixListReference (CreateTransitGatewayPrefixListReference'),
    newCreateTransitGatewayPrefixListReference,
    CreateTransitGatewayPrefixListReferenceResponse (CreateTransitGatewayPrefixListReferenceResponse'),
    newCreateTransitGatewayPrefixListReferenceResponse,

    -- ** DeleteNetworkInterface
    DeleteNetworkInterface (DeleteNetworkInterface'),
    newDeleteNetworkInterface,
    DeleteNetworkInterfaceResponse (DeleteNetworkInterfaceResponse'),
    newDeleteNetworkInterfaceResponse,

    -- ** CreateTransitGatewayRoute
    CreateTransitGatewayRoute (CreateTransitGatewayRoute'),
    newCreateTransitGatewayRoute,
    CreateTransitGatewayRouteResponse (CreateTransitGatewayRouteResponse'),
    newCreateTransitGatewayRouteResponse,

    -- ** DeregisterTransitGatewayMulticastGroupSources
    DeregisterTransitGatewayMulticastGroupSources (DeregisterTransitGatewayMulticastGroupSources'),
    newDeregisterTransitGatewayMulticastGroupSources,
    DeregisterTransitGatewayMulticastGroupSourcesResponse (DeregisterTransitGatewayMulticastGroupSourcesResponse'),
    newDeregisterTransitGatewayMulticastGroupSourcesResponse,

    -- ** DisassociateVpcCidrBlock
    DisassociateVpcCidrBlock (DisassociateVpcCidrBlock'),
    newDisassociateVpcCidrBlock,
    DisassociateVpcCidrBlockResponse (DisassociateVpcCidrBlockResponse'),
    newDisassociateVpcCidrBlockResponse,

    -- ** DescribeTransitGatewayPeeringAttachments (Paginated)
    DescribeTransitGatewayPeeringAttachments (DescribeTransitGatewayPeeringAttachments'),
    newDescribeTransitGatewayPeeringAttachments,
    DescribeTransitGatewayPeeringAttachmentsResponse (DescribeTransitGatewayPeeringAttachmentsResponse'),
    newDescribeTransitGatewayPeeringAttachmentsResponse,

    -- ** GetCoipPoolUsage
    GetCoipPoolUsage (GetCoipPoolUsage'),
    newGetCoipPoolUsage,
    GetCoipPoolUsageResponse (GetCoipPoolUsageResponse'),
    newGetCoipPoolUsageResponse,

    -- ** ImportImage
    ImportImage (ImportImage'),
    newImportImage,
    ImportImageResponse (ImportImageResponse'),
    newImportImageResponse,

    -- ** ReplaceTransitGatewayRoute
    ReplaceTransitGatewayRoute (ReplaceTransitGatewayRoute'),
    newReplaceTransitGatewayRoute,
    ReplaceTransitGatewayRouteResponse (ReplaceTransitGatewayRouteResponse'),
    newReplaceTransitGatewayRouteResponse,

    -- ** CreatePlacementGroup
    CreatePlacementGroup (CreatePlacementGroup'),
    newCreatePlacementGroup,
    CreatePlacementGroupResponse (CreatePlacementGroupResponse'),
    newCreatePlacementGroupResponse,

    -- ** CreateDefaultVpc
    CreateDefaultVpc (CreateDefaultVpc'),
    newCreateDefaultVpc,
    CreateDefaultVpcResponse (CreateDefaultVpcResponse'),
    newCreateDefaultVpcResponse,

    -- ** CreateNetworkInsightsPath
    CreateNetworkInsightsPath (CreateNetworkInsightsPath'),
    newCreateNetworkInsightsPath,
    CreateNetworkInsightsPathResponse (CreateNetworkInsightsPathResponse'),
    newCreateNetworkInsightsPathResponse,

    -- ** ModifyTrafficMirrorSession
    ModifyTrafficMirrorSession (ModifyTrafficMirrorSession'),
    newModifyTrafficMirrorSession,
    ModifyTrafficMirrorSessionResponse (ModifyTrafficMirrorSessionResponse'),
    newModifyTrafficMirrorSessionResponse,

    -- ** RunScheduledInstances
    RunScheduledInstances (RunScheduledInstances'),
    newRunScheduledInstances,
    RunScheduledInstancesResponse (RunScheduledInstancesResponse'),
    newRunScheduledInstancesResponse,

    -- ** DescribeDhcpOptions (Paginated)
    DescribeDhcpOptions (DescribeDhcpOptions'),
    newDescribeDhcpOptions,
    DescribeDhcpOptionsResponse (DescribeDhcpOptionsResponse'),
    newDescribeDhcpOptionsResponse,

    -- ** DescribeCapacityReservations (Paginated)
    DescribeCapacityReservations (DescribeCapacityReservations'),
    newDescribeCapacityReservations,
    DescribeCapacityReservationsResponse (DescribeCapacityReservationsResponse'),
    newDescribeCapacityReservationsResponse,

    -- ** DescribeCustomerGateways
    DescribeCustomerGateways (DescribeCustomerGateways'),
    newDescribeCustomerGateways,
    DescribeCustomerGatewaysResponse (DescribeCustomerGatewaysResponse'),
    newDescribeCustomerGatewaysResponse,

    -- ** DeleteNatGateway
    DeleteNatGateway (DeleteNatGateway'),
    newDeleteNatGateway,
    DeleteNatGatewayResponse (DeleteNatGatewayResponse'),
    newDeleteNatGatewayResponse,

    -- ** DescribeClientVpnAuthorizationRules (Paginated)
    DescribeClientVpnAuthorizationRules (DescribeClientVpnAuthorizationRules'),
    newDescribeClientVpnAuthorizationRules,
    DescribeClientVpnAuthorizationRulesResponse (DescribeClientVpnAuthorizationRulesResponse'),
    newDescribeClientVpnAuthorizationRulesResponse,

    -- ** StopInstances
    StopInstances (StopInstances'),
    newStopInstances,
    StopInstancesResponse (StopInstancesResponse'),
    newStopInstancesResponse,

    -- ** ReplaceRouteTableAssociation
    ReplaceRouteTableAssociation (ReplaceRouteTableAssociation'),
    newReplaceRouteTableAssociation,
    ReplaceRouteTableAssociationResponse (ReplaceRouteTableAssociationResponse'),
    newReplaceRouteTableAssociationResponse,

    -- ** DeleteTransitGatewayMulticastDomain
    DeleteTransitGatewayMulticastDomain (DeleteTransitGatewayMulticastDomain'),
    newDeleteTransitGatewayMulticastDomain,
    DeleteTransitGatewayMulticastDomainResponse (DeleteTransitGatewayMulticastDomainResponse'),
    newDeleteTransitGatewayMulticastDomainResponse,

    -- ** DeleteSubnet
    DeleteSubnet (DeleteSubnet'),
    newDeleteSubnet,
    DeleteSubnetResponse (DeleteSubnetResponse'),
    newDeleteSubnetResponse,

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

    -- ** BundleTaskState
    BundleTaskState (..),

    -- ** ByoipCidrState
    ByoipCidrState (..),

    -- ** CancelBatchErrorCode
    CancelBatchErrorCode (..),

    -- ** CancelSpotInstanceRequestState
    CancelSpotInstanceRequestState (..),

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

    -- ** FleetCapacityReservationUsageStrategy
    FleetCapacityReservationUsageStrategy (..),

    -- ** FleetEventType
    FleetEventType (..),

    -- ** FleetExcessCapacityTerminationPolicy
    FleetExcessCapacityTerminationPolicy (..),

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

    -- ** InstanceStateName
    InstanceStateName (..),

    -- ** InstanceType
    InstanceType (..),

    -- ** InstanceTypeHypervisor
    InstanceTypeHypervisor (..),

    -- ** InterfacePermissionType
    InterfacePermissionType (..),

    -- ** Ipv6SupportValue
    Ipv6SupportValue (..),

    -- ** LaunchTemplateErrorCode
    LaunchTemplateErrorCode (..),

    -- ** LaunchTemplateHttpTokensState
    LaunchTemplateHttpTokensState (..),

    -- ** LaunchTemplateInstanceMetadataEndpointState
    LaunchTemplateInstanceMetadataEndpointState (..),

    -- ** LaunchTemplateInstanceMetadataOptionsState
    LaunchTemplateInstanceMetadataOptionsState (..),

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

    -- ** InstanceExportDetails
    InstanceExportDetails (InstanceExportDetails'),
    newInstanceExportDetails,

    -- ** InstanceFamilyCreditSpecification
    InstanceFamilyCreditSpecification (InstanceFamilyCreditSpecification'),
    newInstanceFamilyCreditSpecification,

    -- ** InstanceIpv6Address
    InstanceIpv6Address (InstanceIpv6Address'),
    newInstanceIpv6Address,

    -- ** InstanceIpv6AddressRequest
    InstanceIpv6AddressRequest (InstanceIpv6AddressRequest'),
    newInstanceIpv6AddressRequest,

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

    -- ** Ipv6CidrAssociation
    Ipv6CidrAssociation (Ipv6CidrAssociation'),
    newIpv6CidrAssociation,

    -- ** Ipv6CidrBlock
    Ipv6CidrBlock (Ipv6CidrBlock'),
    newIpv6CidrBlock,

    -- ** Ipv6Pool
    Ipv6Pool (Ipv6Pool'),
    newIpv6Pool,

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

    -- ** RegionInfo
    RegionInfo (RegionInfo'),
    newRegionInfo,

    -- ** RegisterInstanceTagAttributeRequest
    RegisterInstanceTagAttributeRequest (RegisterInstanceTagAttributeRequest'),
    newRegisterInstanceTagAttributeRequest,

    -- ** RemovePrefixListEntry
    RemovePrefixListEntry (RemovePrefixListEntry'),
    newRemovePrefixListEntry,

    -- ** RequestLaunchTemplateData
    RequestLaunchTemplateData (RequestLaunchTemplateData'),
    newRequestLaunchTemplateData,

    -- ** RequestSpotLaunchSpecification
    RequestSpotLaunchSpecification (RequestSpotLaunchSpecification'),
    newRequestSpotLaunchSpecification,

    -- ** Reservation
    Reservation (Reservation'),
    newReservation,

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

    -- ** Subnet
    Subnet (Subnet'),
    newSubnet,

    -- ** SubnetAssociation
    SubnetAssociation (SubnetAssociation'),
    newSubnetAssociation,

    -- ** SubnetCidrBlockState
    SubnetCidrBlockState (SubnetCidrBlockState'),
    newSubnetCidrBlockState,

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
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Lens
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
