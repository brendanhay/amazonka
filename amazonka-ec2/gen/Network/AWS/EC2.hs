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

    -- ** ExportTaskCompleted
    newExportTaskCompleted,

    -- ** InstanceTerminated
    newInstanceTerminated,

    -- ** VpcPeeringConnectionDeleted
    newVpcPeeringConnectionDeleted,

    -- ** VolumeAvailable
    newVolumeAvailable,

    -- ** SnapshotCompleted
    newSnapshotCompleted,

    -- ** SpotInstanceRequestFulfilled
    newSpotInstanceRequestFulfilled,

    -- ** PasswordDataAvailable
    newPasswordDataAvailable,

    -- ** InstanceRunning
    newInstanceRunning,

    -- ** ImageAvailable
    newImageAvailable,

    -- ** KeyPairExists
    newKeyPairExists,

    -- ** ExportTaskCancelled
    newExportTaskCancelled,

    -- ** ImageExists
    newImageExists,

    -- ** VpnConnectionAvailable
    newVpnConnectionAvailable,

    -- ** VpcAvailable
    newVpcAvailable,

    -- ** VolumeInUse
    newVolumeInUse,

    -- ** VpcPeeringConnectionExists
    newVpcPeeringConnectionExists,

    -- ** InstanceExists
    newInstanceExists,

    -- ** SecurityGroupExists
    newSecurityGroupExists,

    -- ** InstanceStatusOk
    newInstanceStatusOk,

    -- ** ConversionTaskCancelled
    newConversionTaskCancelled,

    -- ** VpnConnectionDeleted
    newVpnConnectionDeleted,

    -- ** BundleTaskComplete
    newBundleTaskComplete,

    -- ** ConversionTaskCompleted
    newConversionTaskCompleted,

    -- ** InstanceStopped
    newInstanceStopped,

    -- ** ConversionTaskDeleted
    newConversionTaskDeleted,

    -- ** VpcExists
    newVpcExists,

    -- ** VolumeDeleted
    newVolumeDeleted,

    -- ** CustomerGatewayAvailable
    newCustomerGatewayAvailable,

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

    -- ** DescribeVpcPeeringConnections (Paginated)
    DescribeVpcPeeringConnections (DescribeVpcPeeringConnections'),
    newDescribeVpcPeeringConnections,
    DescribeVpcPeeringConnectionsResponse (DescribeVpcPeeringConnectionsResponse'),
    newDescribeVpcPeeringConnectionsResponse,

    -- ** AssociateTrunkInterface
    AssociateTrunkInterface (AssociateTrunkInterface'),
    newAssociateTrunkInterface,
    AssociateTrunkInterfaceResponse (AssociateTrunkInterfaceResponse'),
    newAssociateTrunkInterfaceResponse,

    -- ** DeleteTransitGatewayRoute
    DeleteTransitGatewayRoute (DeleteTransitGatewayRoute'),
    newDeleteTransitGatewayRoute,
    DeleteTransitGatewayRouteResponse (DeleteTransitGatewayRouteResponse'),
    newDeleteTransitGatewayRouteResponse,

    -- ** DescribeExportTasks
    DescribeExportTasks (DescribeExportTasks'),
    newDescribeExportTasks,
    DescribeExportTasksResponse (DescribeExportTasksResponse'),
    newDescribeExportTasksResponse,

    -- ** DeleteLocalGatewayRouteTableVpcAssociation
    DeleteLocalGatewayRouteTableVpcAssociation (DeleteLocalGatewayRouteTableVpcAssociation'),
    newDeleteLocalGatewayRouteTableVpcAssociation,
    DeleteLocalGatewayRouteTableVpcAssociationResponse (DeleteLocalGatewayRouteTableVpcAssociationResponse'),
    newDeleteLocalGatewayRouteTableVpcAssociationResponse,

    -- ** DeleteVpcEndpointConnectionNotifications
    DeleteVpcEndpointConnectionNotifications (DeleteVpcEndpointConnectionNotifications'),
    newDeleteVpcEndpointConnectionNotifications,
    DeleteVpcEndpointConnectionNotificationsResponse (DeleteVpcEndpointConnectionNotificationsResponse'),
    newDeleteVpcEndpointConnectionNotificationsResponse,

    -- ** CreateTransitGatewayMulticastDomain
    CreateTransitGatewayMulticastDomain (CreateTransitGatewayMulticastDomain'),
    newCreateTransitGatewayMulticastDomain,
    CreateTransitGatewayMulticastDomainResponse (CreateTransitGatewayMulticastDomainResponse'),
    newCreateTransitGatewayMulticastDomainResponse,

    -- ** CreateVpcEndpointServiceConfiguration
    CreateVpcEndpointServiceConfiguration (CreateVpcEndpointServiceConfiguration'),
    newCreateVpcEndpointServiceConfiguration,
    CreateVpcEndpointServiceConfigurationResponse (CreateVpcEndpointServiceConfigurationResponse'),
    newCreateVpcEndpointServiceConfigurationResponse,

    -- ** DescribeByoipCidrs (Paginated)
    DescribeByoipCidrs (DescribeByoipCidrs'),
    newDescribeByoipCidrs,
    DescribeByoipCidrsResponse (DescribeByoipCidrsResponse'),
    newDescribeByoipCidrsResponse,

    -- ** DetachVolume
    DetachVolume (DetachVolume'),
    newDetachVolume,
    VolumeAttachment (VolumeAttachment'),
    newVolumeAttachment,

    -- ** DeleteNetworkInsightsPath
    DeleteNetworkInsightsPath (DeleteNetworkInsightsPath'),
    newDeleteNetworkInsightsPath,
    DeleteNetworkInsightsPathResponse (DeleteNetworkInsightsPathResponse'),
    newDeleteNetworkInsightsPathResponse,

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

    -- ** AssociateTransitGatewayMulticastDomain
    AssociateTransitGatewayMulticastDomain (AssociateTransitGatewayMulticastDomain'),
    newAssociateTransitGatewayMulticastDomain,
    AssociateTransitGatewayMulticastDomainResponse (AssociateTransitGatewayMulticastDomainResponse'),
    newAssociateTransitGatewayMulticastDomainResponse,

    -- ** CreateTransitGatewayConnectPeer
    CreateTransitGatewayConnectPeer (CreateTransitGatewayConnectPeer'),
    newCreateTransitGatewayConnectPeer,
    CreateTransitGatewayConnectPeerResponse (CreateTransitGatewayConnectPeerResponse'),
    newCreateTransitGatewayConnectPeerResponse,

    -- ** ReleaseAddress
    ReleaseAddress (ReleaseAddress'),
    newReleaseAddress,
    ReleaseAddressResponse (ReleaseAddressResponse'),
    newReleaseAddressResponse,

    -- ** ModifyVpcTenancy
    ModifyVpcTenancy (ModifyVpcTenancy'),
    newModifyVpcTenancy,
    ModifyVpcTenancyResponse (ModifyVpcTenancyResponse'),
    newModifyVpcTenancyResponse,

    -- ** DescribeLocalGatewayVirtualInterfaces (Paginated)
    DescribeLocalGatewayVirtualInterfaces (DescribeLocalGatewayVirtualInterfaces'),
    newDescribeLocalGatewayVirtualInterfaces,
    DescribeLocalGatewayVirtualInterfacesResponse (DescribeLocalGatewayVirtualInterfacesResponse'),
    newDescribeLocalGatewayVirtualInterfacesResponse,

    -- ** GetHostReservationPurchasePreview
    GetHostReservationPurchasePreview (GetHostReservationPurchasePreview'),
    newGetHostReservationPurchasePreview,
    GetHostReservationPurchasePreviewResponse (GetHostReservationPurchasePreviewResponse'),
    newGetHostReservationPurchasePreviewResponse,

    -- ** AuthorizeSecurityGroupEgress
    AuthorizeSecurityGroupEgress (AuthorizeSecurityGroupEgress'),
    newAuthorizeSecurityGroupEgress,
    AuthorizeSecurityGroupEgressResponse (AuthorizeSecurityGroupEgressResponse'),
    newAuthorizeSecurityGroupEgressResponse,

    -- ** AcceptReservedInstancesExchangeQuote
    AcceptReservedInstancesExchangeQuote (AcceptReservedInstancesExchangeQuote'),
    newAcceptReservedInstancesExchangeQuote,
    AcceptReservedInstancesExchangeQuoteResponse (AcceptReservedInstancesExchangeQuoteResponse'),
    newAcceptReservedInstancesExchangeQuoteResponse,

    -- ** ModifyManagedPrefixList
    ModifyManagedPrefixList (ModifyManagedPrefixList'),
    newModifyManagedPrefixList,
    ModifyManagedPrefixListResponse (ModifyManagedPrefixListResponse'),
    newModifyManagedPrefixListResponse,

    -- ** DescribeInstanceCreditSpecifications (Paginated)
    DescribeInstanceCreditSpecifications (DescribeInstanceCreditSpecifications'),
    newDescribeInstanceCreditSpecifications,
    DescribeInstanceCreditSpecificationsResponse (DescribeInstanceCreditSpecificationsResponse'),
    newDescribeInstanceCreditSpecificationsResponse,

    -- ** GetTransitGatewayMulticastDomainAssociations (Paginated)
    GetTransitGatewayMulticastDomainAssociations (GetTransitGatewayMulticastDomainAssociations'),
    newGetTransitGatewayMulticastDomainAssociations,
    GetTransitGatewayMulticastDomainAssociationsResponse (GetTransitGatewayMulticastDomainAssociationsResponse'),
    newGetTransitGatewayMulticastDomainAssociationsResponse,

    -- ** DescribeInstances (Paginated)
    DescribeInstances (DescribeInstances'),
    newDescribeInstances,
    DescribeInstancesResponse (DescribeInstancesResponse'),
    newDescribeInstancesResponse,

    -- ** DisableEbsEncryptionByDefault
    DisableEbsEncryptionByDefault (DisableEbsEncryptionByDefault'),
    newDisableEbsEncryptionByDefault,
    DisableEbsEncryptionByDefaultResponse (DisableEbsEncryptionByDefaultResponse'),
    newDisableEbsEncryptionByDefaultResponse,

    -- ** DeregisterInstanceEventNotificationAttributes
    DeregisterInstanceEventNotificationAttributes (DeregisterInstanceEventNotificationAttributes'),
    newDeregisterInstanceEventNotificationAttributes,
    DeregisterInstanceEventNotificationAttributesResponse (DeregisterInstanceEventNotificationAttributesResponse'),
    newDeregisterInstanceEventNotificationAttributesResponse,

    -- ** CreateTransitGatewayVpcAttachment
    CreateTransitGatewayVpcAttachment (CreateTransitGatewayVpcAttachment'),
    newCreateTransitGatewayVpcAttachment,
    CreateTransitGatewayVpcAttachmentResponse (CreateTransitGatewayVpcAttachmentResponse'),
    newCreateTransitGatewayVpcAttachmentResponse,

    -- ** DeregisterTransitGatewayMulticastGroupMembers
    DeregisterTransitGatewayMulticastGroupMembers (DeregisterTransitGatewayMulticastGroupMembers'),
    newDeregisterTransitGatewayMulticastGroupMembers,
    DeregisterTransitGatewayMulticastGroupMembersResponse (DeregisterTransitGatewayMulticastGroupMembersResponse'),
    newDeregisterTransitGatewayMulticastGroupMembersResponse,

    -- ** DeleteTransitGatewayPrefixListReference
    DeleteTransitGatewayPrefixListReference (DeleteTransitGatewayPrefixListReference'),
    newDeleteTransitGatewayPrefixListReference,
    DeleteTransitGatewayPrefixListReferenceResponse (DeleteTransitGatewayPrefixListReferenceResponse'),
    newDeleteTransitGatewayPrefixListReferenceResponse,

    -- ** CreateTransitGatewayRouteTable
    CreateTransitGatewayRouteTable (CreateTransitGatewayRouteTable'),
    newCreateTransitGatewayRouteTable,
    CreateTransitGatewayRouteTableResponse (CreateTransitGatewayRouteTableResponse'),
    newCreateTransitGatewayRouteTableResponse,

    -- ** DisassociateAddress
    DisassociateAddress (DisassociateAddress'),
    newDisassociateAddress,
    DisassociateAddressResponse (DisassociateAddressResponse'),
    newDisassociateAddressResponse,

    -- ** DetachNetworkInterface
    DetachNetworkInterface (DetachNetworkInterface'),
    newDetachNetworkInterface,
    DetachNetworkInterfaceResponse (DetachNetworkInterfaceResponse'),
    newDetachNetworkInterfaceResponse,

    -- ** DeleteFleets
    DeleteFleets (DeleteFleets'),
    newDeleteFleets,
    DeleteFleetsResponse (DeleteFleetsResponse'),
    newDeleteFleetsResponse,

    -- ** DeleteVpc
    DeleteVpc (DeleteVpc'),
    newDeleteVpc,
    DeleteVpcResponse (DeleteVpcResponse'),
    newDeleteVpcResponse,

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

    -- ** DeleteTrafficMirrorTarget
    DeleteTrafficMirrorTarget (DeleteTrafficMirrorTarget'),
    newDeleteTrafficMirrorTarget,
    DeleteTrafficMirrorTargetResponse (DeleteTrafficMirrorTargetResponse'),
    newDeleteTrafficMirrorTargetResponse,

    -- ** DeleteLaunchTemplate
    DeleteLaunchTemplate (DeleteLaunchTemplate'),
    newDeleteLaunchTemplate,
    DeleteLaunchTemplateResponse (DeleteLaunchTemplateResponse'),
    newDeleteLaunchTemplateResponse,

    -- ** ModifySecurityGroupRules
    ModifySecurityGroupRules (ModifySecurityGroupRules'),
    newModifySecurityGroupRules,
    ModifySecurityGroupRulesResponse (ModifySecurityGroupRulesResponse'),
    newModifySecurityGroupRulesResponse,

    -- ** DeleteVpcEndpoints
    DeleteVpcEndpoints (DeleteVpcEndpoints'),
    newDeleteVpcEndpoints,
    DeleteVpcEndpointsResponse (DeleteVpcEndpointsResponse'),
    newDeleteVpcEndpointsResponse,

    -- ** DescribeTrafficMirrorSessions (Paginated)
    DescribeTrafficMirrorSessions (DescribeTrafficMirrorSessions'),
    newDescribeTrafficMirrorSessions,
    DescribeTrafficMirrorSessionsResponse (DescribeTrafficMirrorSessionsResponse'),
    newDescribeTrafficMirrorSessionsResponse,

    -- ** UpdateSecurityGroupRuleDescriptionsIngress
    UpdateSecurityGroupRuleDescriptionsIngress (UpdateSecurityGroupRuleDescriptionsIngress'),
    newUpdateSecurityGroupRuleDescriptionsIngress,
    UpdateSecurityGroupRuleDescriptionsIngressResponse (UpdateSecurityGroupRuleDescriptionsIngressResponse'),
    newUpdateSecurityGroupRuleDescriptionsIngressResponse,

    -- ** DescribePrefixLists (Paginated)
    DescribePrefixLists (DescribePrefixLists'),
    newDescribePrefixLists,
    DescribePrefixListsResponse (DescribePrefixListsResponse'),
    newDescribePrefixListsResponse,

    -- ** DescribeVpcClassicLink
    DescribeVpcClassicLink (DescribeVpcClassicLink'),
    newDescribeVpcClassicLink,
    DescribeVpcClassicLinkResponse (DescribeVpcClassicLinkResponse'),
    newDescribeVpcClassicLinkResponse,

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

    -- ** CreateDefaultSubnet
    CreateDefaultSubnet (CreateDefaultSubnet'),
    newCreateDefaultSubnet,
    CreateDefaultSubnetResponse (CreateDefaultSubnetResponse'),
    newCreateDefaultSubnetResponse,

    -- ** DeleteFlowLogs
    DeleteFlowLogs (DeleteFlowLogs'),
    newDeleteFlowLogs,
    DeleteFlowLogsResponse (DeleteFlowLogsResponse'),
    newDeleteFlowLogsResponse,

    -- ** ModifyVolumeAttribute
    ModifyVolumeAttribute (ModifyVolumeAttribute'),
    newModifyVolumeAttribute,
    ModifyVolumeAttributeResponse (ModifyVolumeAttributeResponse'),
    newModifyVolumeAttributeResponse,

    -- ** CreateNetworkInterfacePermission
    CreateNetworkInterfacePermission (CreateNetworkInterfacePermission'),
    newCreateNetworkInterfacePermission,
    CreateNetworkInterfacePermissionResponse (CreateNetworkInterfacePermissionResponse'),
    newCreateNetworkInterfacePermissionResponse,

    -- ** DescribeScheduledInstanceAvailability (Paginated)
    DescribeScheduledInstanceAvailability (DescribeScheduledInstanceAvailability'),
    newDescribeScheduledInstanceAvailability,
    DescribeScheduledInstanceAvailabilityResponse (DescribeScheduledInstanceAvailabilityResponse'),
    newDescribeScheduledInstanceAvailabilityResponse,

    -- ** DescribeClientVpnEndpoints (Paginated)
    DescribeClientVpnEndpoints (DescribeClientVpnEndpoints'),
    newDescribeClientVpnEndpoints,
    DescribeClientVpnEndpointsResponse (DescribeClientVpnEndpointsResponse'),
    newDescribeClientVpnEndpointsResponse,

    -- ** RejectVpcEndpointConnections
    RejectVpcEndpointConnections (RejectVpcEndpointConnections'),
    newRejectVpcEndpointConnections,
    RejectVpcEndpointConnectionsResponse (RejectVpcEndpointConnectionsResponse'),
    newRejectVpcEndpointConnectionsResponse,

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

    -- ** DescribeLocalGatewayRouteTables (Paginated)
    DescribeLocalGatewayRouteTables (DescribeLocalGatewayRouteTables'),
    newDescribeLocalGatewayRouteTables,
    DescribeLocalGatewayRouteTablesResponse (DescribeLocalGatewayRouteTablesResponse'),
    newDescribeLocalGatewayRouteTablesResponse,

    -- ** RejectTransitGatewayVpcAttachment
    RejectTransitGatewayVpcAttachment (RejectTransitGatewayVpcAttachment'),
    newRejectTransitGatewayVpcAttachment,
    RejectTransitGatewayVpcAttachmentResponse (RejectTransitGatewayVpcAttachmentResponse'),
    newRejectTransitGatewayVpcAttachmentResponse,

    -- ** CreateVpnGateway
    CreateVpnGateway (CreateVpnGateway'),
    newCreateVpnGateway,
    CreateVpnGatewayResponse (CreateVpnGatewayResponse'),
    newCreateVpnGatewayResponse,

    -- ** AcceptTransitGatewayPeeringAttachment
    AcceptTransitGatewayPeeringAttachment (AcceptTransitGatewayPeeringAttachment'),
    newAcceptTransitGatewayPeeringAttachment,
    AcceptTransitGatewayPeeringAttachmentResponse (AcceptTransitGatewayPeeringAttachmentResponse'),
    newAcceptTransitGatewayPeeringAttachmentResponse,

    -- ** GetTransitGatewayRouteTableAssociations (Paginated)
    GetTransitGatewayRouteTableAssociations (GetTransitGatewayRouteTableAssociations'),
    newGetTransitGatewayRouteTableAssociations,
    GetTransitGatewayRouteTableAssociationsResponse (GetTransitGatewayRouteTableAssociationsResponse'),
    newGetTransitGatewayRouteTableAssociationsResponse,

    -- ** DeleteVpnGateway
    DeleteVpnGateway (DeleteVpnGateway'),
    newDeleteVpnGateway,
    DeleteVpnGatewayResponse (DeleteVpnGatewayResponse'),
    newDeleteVpnGatewayResponse,

    -- ** DescribeImportImageTasks (Paginated)
    DescribeImportImageTasks (DescribeImportImageTasks'),
    newDescribeImportImageTasks,
    DescribeImportImageTasksResponse (DescribeImportImageTasksResponse'),
    newDescribeImportImageTasksResponse,

    -- ** ExportTransitGatewayRoutes
    ExportTransitGatewayRoutes (ExportTransitGatewayRoutes'),
    newExportTransitGatewayRoutes,
    ExportTransitGatewayRoutesResponse (ExportTransitGatewayRoutesResponse'),
    newExportTransitGatewayRoutesResponse,

    -- ** DescribeLaunchTemplateVersions (Paginated)
    DescribeLaunchTemplateVersions (DescribeLaunchTemplateVersions'),
    newDescribeLaunchTemplateVersions,
    DescribeLaunchTemplateVersionsResponse (DescribeLaunchTemplateVersionsResponse'),
    newDescribeLaunchTemplateVersionsResponse,

    -- ** DescribeFpgaImages (Paginated)
    DescribeFpgaImages (DescribeFpgaImages'),
    newDescribeFpgaImages,
    DescribeFpgaImagesResponse (DescribeFpgaImagesResponse'),
    newDescribeFpgaImagesResponse,

    -- ** GetReservedInstancesExchangeQuote
    GetReservedInstancesExchangeQuote (GetReservedInstancesExchangeQuote'),
    newGetReservedInstancesExchangeQuote,
    GetReservedInstancesExchangeQuoteResponse (GetReservedInstancesExchangeQuoteResponse'),
    newGetReservedInstancesExchangeQuoteResponse,

    -- ** CreateKeyPair
    CreateKeyPair (CreateKeyPair'),
    newCreateKeyPair,
    CreateKeyPairResponse (CreateKeyPairResponse'),
    newCreateKeyPairResponse,

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

    -- ** DescribeSecurityGroupRules (Paginated)
    DescribeSecurityGroupRules (DescribeSecurityGroupRules'),
    newDescribeSecurityGroupRules,
    DescribeSecurityGroupRulesResponse (DescribeSecurityGroupRulesResponse'),
    newDescribeSecurityGroupRulesResponse,

    -- ** DeleteNetworkAcl
    DeleteNetworkAcl (DeleteNetworkAcl'),
    newDeleteNetworkAcl,
    DeleteNetworkAclResponse (DeleteNetworkAclResponse'),
    newDeleteNetworkAclResponse,

    -- ** DescribeRouteTables (Paginated)
    DescribeRouteTables (DescribeRouteTables'),
    newDescribeRouteTables,
    DescribeRouteTablesResponse (DescribeRouteTablesResponse'),
    newDescribeRouteTablesResponse,

    -- ** DescribeFleetInstances
    DescribeFleetInstances (DescribeFleetInstances'),
    newDescribeFleetInstances,
    DescribeFleetInstancesResponse (DescribeFleetInstancesResponse'),
    newDescribeFleetInstancesResponse,

    -- ** DeleteTransitGatewayRouteTable
    DeleteTransitGatewayRouteTable (DeleteTransitGatewayRouteTable'),
    newDeleteTransitGatewayRouteTable,
    DeleteTransitGatewayRouteTableResponse (DeleteTransitGatewayRouteTableResponse'),
    newDeleteTransitGatewayRouteTableResponse,

    -- ** CreateLaunchTemplate
    CreateLaunchTemplate (CreateLaunchTemplate'),
    newCreateLaunchTemplate,
    CreateLaunchTemplateResponse (CreateLaunchTemplateResponse'),
    newCreateLaunchTemplateResponse,

    -- ** ModifyInstanceEventWindow
    ModifyInstanceEventWindow (ModifyInstanceEventWindow'),
    newModifyInstanceEventWindow,
    ModifyInstanceEventWindowResponse (ModifyInstanceEventWindowResponse'),
    newModifyInstanceEventWindowResponse,

    -- ** MoveAddressToVpc
    MoveAddressToVpc (MoveAddressToVpc'),
    newMoveAddressToVpc,
    MoveAddressToVpcResponse (MoveAddressToVpcResponse'),
    newMoveAddressToVpcResponse,

    -- ** AcceptTransitGatewayMulticastDomainAssociations
    AcceptTransitGatewayMulticastDomainAssociations (AcceptTransitGatewayMulticastDomainAssociations'),
    newAcceptTransitGatewayMulticastDomainAssociations,
    AcceptTransitGatewayMulticastDomainAssociationsResponse (AcceptTransitGatewayMulticastDomainAssociationsResponse'),
    newAcceptTransitGatewayMulticastDomainAssociationsResponse,

    -- ** RestoreAddressToClassic
    RestoreAddressToClassic (RestoreAddressToClassic'),
    newRestoreAddressToClassic,
    RestoreAddressToClassicResponse (RestoreAddressToClassicResponse'),
    newRestoreAddressToClassicResponse,

    -- ** DescribeAvailabilityZones
    DescribeAvailabilityZones (DescribeAvailabilityZones'),
    newDescribeAvailabilityZones,
    DescribeAvailabilityZonesResponse (DescribeAvailabilityZonesResponse'),
    newDescribeAvailabilityZonesResponse,

    -- ** CreateStoreImageTask
    CreateStoreImageTask (CreateStoreImageTask'),
    newCreateStoreImageTask,
    CreateStoreImageTaskResponse (CreateStoreImageTaskResponse'),
    newCreateStoreImageTaskResponse,

    -- ** CopySnapshot
    CopySnapshot (CopySnapshot'),
    newCopySnapshot,
    CopySnapshotResponse (CopySnapshotResponse'),
    newCopySnapshotResponse,

    -- ** DeleteNetworkInterfacePermission
    DeleteNetworkInterfacePermission (DeleteNetworkInterfacePermission'),
    newDeleteNetworkInterfacePermission,
    DeleteNetworkInterfacePermissionResponse (DeleteNetworkInterfacePermissionResponse'),
    newDeleteNetworkInterfacePermissionResponse,

    -- ** CreateFlowLogs
    CreateFlowLogs (CreateFlowLogs'),
    newCreateFlowLogs,
    CreateFlowLogsResponse (CreateFlowLogsResponse'),
    newCreateFlowLogsResponse,

    -- ** DetachClassicLinkVpc
    DetachClassicLinkVpc (DetachClassicLinkVpc'),
    newDetachClassicLinkVpc,
    DetachClassicLinkVpcResponse (DetachClassicLinkVpcResponse'),
    newDetachClassicLinkVpcResponse,

    -- ** DeleteRouteTable
    DeleteRouteTable (DeleteRouteTable'),
    newDeleteRouteTable,
    DeleteRouteTableResponse (DeleteRouteTableResponse'),
    newDeleteRouteTableResponse,

    -- ** ModifyVpnConnectionOptions
    ModifyVpnConnectionOptions (ModifyVpnConnectionOptions'),
    newModifyVpnConnectionOptions,
    ModifyVpnConnectionOptionsResponse (ModifyVpnConnectionOptionsResponse'),
    newModifyVpnConnectionOptionsResponse,

    -- ** MonitorInstances
    MonitorInstances (MonitorInstances'),
    newMonitorInstances,
    MonitorInstancesResponse (MonitorInstancesResponse'),
    newMonitorInstancesResponse,

    -- ** ModifyIdFormat
    ModifyIdFormat (ModifyIdFormat'),
    newModifyIdFormat,
    ModifyIdFormatResponse (ModifyIdFormatResponse'),
    newModifyIdFormatResponse,

    -- ** AllocateHosts
    AllocateHosts (AllocateHosts'),
    newAllocateHosts,
    AllocateHostsResponse (AllocateHostsResponse'),
    newAllocateHostsResponse,

    -- ** DescribeImageAttribute
    DescribeImageAttribute (DescribeImageAttribute'),
    newDescribeImageAttribute,
    DescribeImageAttributeResponse (DescribeImageAttributeResponse'),
    newDescribeImageAttributeResponse,

    -- ** DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations (Paginated)
    DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations (DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations'),
    newDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations,
    DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse (DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse'),
    newDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse,

    -- ** DescribeReservedInstancesModifications (Paginated)
    DescribeReservedInstancesModifications (DescribeReservedInstancesModifications'),
    newDescribeReservedInstancesModifications,
    DescribeReservedInstancesModificationsResponse (DescribeReservedInstancesModificationsResponse'),
    newDescribeReservedInstancesModificationsResponse,

    -- ** StartVpcEndpointServicePrivateDnsVerification
    StartVpcEndpointServicePrivateDnsVerification (StartVpcEndpointServicePrivateDnsVerification'),
    newStartVpcEndpointServicePrivateDnsVerification,
    StartVpcEndpointServicePrivateDnsVerificationResponse (StartVpcEndpointServicePrivateDnsVerificationResponse'),
    newStartVpcEndpointServicePrivateDnsVerificationResponse,

    -- ** CreateTrafficMirrorFilterRule
    CreateTrafficMirrorFilterRule (CreateTrafficMirrorFilterRule'),
    newCreateTrafficMirrorFilterRule,
    CreateTrafficMirrorFilterRuleResponse (CreateTrafficMirrorFilterRuleResponse'),
    newCreateTrafficMirrorFilterRuleResponse,

    -- ** GetEbsDefaultKmsKeyId
    GetEbsDefaultKmsKeyId (GetEbsDefaultKmsKeyId'),
    newGetEbsDefaultKmsKeyId,
    GetEbsDefaultKmsKeyIdResponse (GetEbsDefaultKmsKeyIdResponse'),
    newGetEbsDefaultKmsKeyIdResponse,

    -- ** DescribeClientVpnRoutes (Paginated)
    DescribeClientVpnRoutes (DescribeClientVpnRoutes'),
    newDescribeClientVpnRoutes,
    DescribeClientVpnRoutesResponse (DescribeClientVpnRoutesResponse'),
    newDescribeClientVpnRoutesResponse,

    -- ** ModifyVpnConnection
    ModifyVpnConnection (ModifyVpnConnection'),
    newModifyVpnConnection,
    ModifyVpnConnectionResponse (ModifyVpnConnectionResponse'),
    newModifyVpnConnectionResponse,

    -- ** ModifyFleet
    ModifyFleet (ModifyFleet'),
    newModifyFleet,
    ModifyFleetResponse (ModifyFleetResponse'),
    newModifyFleetResponse,

    -- ** RegisterImage
    RegisterImage (RegisterImage'),
    newRegisterImage,
    RegisterImageResponse (RegisterImageResponse'),
    newRegisterImageResponse,

    -- ** RevokeClientVpnIngress
    RevokeClientVpnIngress (RevokeClientVpnIngress'),
    newRevokeClientVpnIngress,
    RevokeClientVpnIngressResponse (RevokeClientVpnIngressResponse'),
    newRevokeClientVpnIngressResponse,

    -- ** UpdateSecurityGroupRuleDescriptionsEgress
    UpdateSecurityGroupRuleDescriptionsEgress (UpdateSecurityGroupRuleDescriptionsEgress'),
    newUpdateSecurityGroupRuleDescriptionsEgress,
    UpdateSecurityGroupRuleDescriptionsEgressResponse (UpdateSecurityGroupRuleDescriptionsEgressResponse'),
    newUpdateSecurityGroupRuleDescriptionsEgressResponse,

    -- ** ModifyVpcEndpoint
    ModifyVpcEndpoint (ModifyVpcEndpoint'),
    newModifyVpcEndpoint,
    ModifyVpcEndpointResponse (ModifyVpcEndpointResponse'),
    newModifyVpcEndpointResponse,

    -- ** UnassignPrivateIpAddresses
    UnassignPrivateIpAddresses (UnassignPrivateIpAddresses'),
    newUnassignPrivateIpAddresses,
    UnassignPrivateIpAddressesResponse (UnassignPrivateIpAddressesResponse'),
    newUnassignPrivateIpAddressesResponse,

    -- ** EnableFastSnapshotRestores
    EnableFastSnapshotRestores (EnableFastSnapshotRestores'),
    newEnableFastSnapshotRestores,
    EnableFastSnapshotRestoresResponse (EnableFastSnapshotRestoresResponse'),
    newEnableFastSnapshotRestoresResponse,

    -- ** CancelImportTask
    CancelImportTask (CancelImportTask'),
    newCancelImportTask,
    CancelImportTaskResponse (CancelImportTaskResponse'),
    newCancelImportTaskResponse,

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

    -- ** ResetFpgaImageAttribute
    ResetFpgaImageAttribute (ResetFpgaImageAttribute'),
    newResetFpgaImageAttribute,
    ResetFpgaImageAttributeResponse (ResetFpgaImageAttributeResponse'),
    newResetFpgaImageAttributeResponse,

    -- ** GetConsoleOutput
    GetConsoleOutput (GetConsoleOutput'),
    newGetConsoleOutput,
    GetConsoleOutputResponse (GetConsoleOutputResponse'),
    newGetConsoleOutputResponse,

    -- ** DeleteFpgaImage
    DeleteFpgaImage (DeleteFpgaImage'),
    newDeleteFpgaImage,
    DeleteFpgaImageResponse (DeleteFpgaImageResponse'),
    newDeleteFpgaImageResponse,

    -- ** ModifyReservedInstances
    ModifyReservedInstances (ModifyReservedInstances'),
    newModifyReservedInstances,
    ModifyReservedInstancesResponse (ModifyReservedInstancesResponse'),
    newModifyReservedInstancesResponse,

    -- ** CreateRestoreImageTask
    CreateRestoreImageTask (CreateRestoreImageTask'),
    newCreateRestoreImageTask,
    CreateRestoreImageTaskResponse (CreateRestoreImageTaskResponse'),
    newCreateRestoreImageTaskResponse,

    -- ** DescribeSpotInstanceRequests (Paginated)
    DescribeSpotInstanceRequests (DescribeSpotInstanceRequests'),
    newDescribeSpotInstanceRequests,
    DescribeSpotInstanceRequestsResponse (DescribeSpotInstanceRequestsResponse'),
    newDescribeSpotInstanceRequestsResponse,

    -- ** ModifyVpcEndpointServicePermissions
    ModifyVpcEndpointServicePermissions (ModifyVpcEndpointServicePermissions'),
    newModifyVpcEndpointServicePermissions,
    ModifyVpcEndpointServicePermissionsResponse (ModifyVpcEndpointServicePermissionsResponse'),
    newModifyVpcEndpointServicePermissionsResponse,

    -- ** UnassignIpv6Addresses
    UnassignIpv6Addresses (UnassignIpv6Addresses'),
    newUnassignIpv6Addresses,
    UnassignIpv6AddressesResponse (UnassignIpv6AddressesResponse'),
    newUnassignIpv6AddressesResponse,

    -- ** DescribeVolumesModifications (Paginated)
    DescribeVolumesModifications (DescribeVolumesModifications'),
    newDescribeVolumesModifications,
    DescribeVolumesModificationsResponse (DescribeVolumesModificationsResponse'),
    newDescribeVolumesModificationsResponse,

    -- ** DescribeIdFormat
    DescribeIdFormat (DescribeIdFormat'),
    newDescribeIdFormat,
    DescribeIdFormatResponse (DescribeIdFormatResponse'),
    newDescribeIdFormatResponse,

    -- ** ReportInstanceStatus
    ReportInstanceStatus (ReportInstanceStatus'),
    newReportInstanceStatus,
    ReportInstanceStatusResponse (ReportInstanceStatusResponse'),
    newReportInstanceStatusResponse,

    -- ** RunInstances
    RunInstances (RunInstances'),
    newRunInstances,
    Reservation (Reservation'),
    newReservation,

    -- ** ModifyHosts
    ModifyHosts (ModifyHosts'),
    newModifyHosts,
    ModifyHostsResponse (ModifyHostsResponse'),
    newModifyHostsResponse,

    -- ** AttachVolume
    AttachVolume (AttachVolume'),
    newAttachVolume,
    VolumeAttachment (VolumeAttachment'),
    newVolumeAttachment,

    -- ** DescribeStoreImageTasks (Paginated)
    DescribeStoreImageTasks (DescribeStoreImageTasks'),
    newDescribeStoreImageTasks,
    DescribeStoreImageTasksResponse (DescribeStoreImageTasksResponse'),
    newDescribeStoreImageTasksResponse,

    -- ** CreateReplaceRootVolumeTask
    CreateReplaceRootVolumeTask (CreateReplaceRootVolumeTask'),
    newCreateReplaceRootVolumeTask,
    CreateReplaceRootVolumeTaskResponse (CreateReplaceRootVolumeTaskResponse'),
    newCreateReplaceRootVolumeTaskResponse,

    -- ** DescribeAccountAttributes
    DescribeAccountAttributes (DescribeAccountAttributes'),
    newDescribeAccountAttributes,
    DescribeAccountAttributesResponse (DescribeAccountAttributesResponse'),
    newDescribeAccountAttributesResponse,

    -- ** ModifyImageAttribute
    ModifyImageAttribute (ModifyImageAttribute'),
    newModifyImageAttribute,
    ModifyImageAttributeResponse (ModifyImageAttributeResponse'),
    newModifyImageAttributeResponse,

    -- ** RegisterTransitGatewayMulticastGroupSources
    RegisterTransitGatewayMulticastGroupSources (RegisterTransitGatewayMulticastGroupSources'),
    newRegisterTransitGatewayMulticastGroupSources,
    RegisterTransitGatewayMulticastGroupSourcesResponse (RegisterTransitGatewayMulticastGroupSourcesResponse'),
    newRegisterTransitGatewayMulticastGroupSourcesResponse,

    -- ** RebootInstances
    RebootInstances (RebootInstances'),
    newRebootInstances,
    RebootInstancesResponse (RebootInstancesResponse'),
    newRebootInstancesResponse,

    -- ** AssociateRouteTable
    AssociateRouteTable (AssociateRouteTable'),
    newAssociateRouteTable,
    AssociateRouteTableResponse (AssociateRouteTableResponse'),
    newAssociateRouteTableResponse,

    -- ** AssociateIamInstanceProfile
    AssociateIamInstanceProfile (AssociateIamInstanceProfile'),
    newAssociateIamInstanceProfile,
    AssociateIamInstanceProfileResponse (AssociateIamInstanceProfileResponse'),
    newAssociateIamInstanceProfileResponse,

    -- ** PurchaseHostReservation
    PurchaseHostReservation (PurchaseHostReservation'),
    newPurchaseHostReservation,
    PurchaseHostReservationResponse (PurchaseHostReservationResponse'),
    newPurchaseHostReservationResponse,

    -- ** DescribeTrafficMirrorTargets (Paginated)
    DescribeTrafficMirrorTargets (DescribeTrafficMirrorTargets'),
    newDescribeTrafficMirrorTargets,
    DescribeTrafficMirrorTargetsResponse (DescribeTrafficMirrorTargetsResponse'),
    newDescribeTrafficMirrorTargetsResponse,

    -- ** GetManagedPrefixListAssociations (Paginated)
    GetManagedPrefixListAssociations (GetManagedPrefixListAssociations'),
    newGetManagedPrefixListAssociations,
    GetManagedPrefixListAssociationsResponse (GetManagedPrefixListAssociationsResponse'),
    newGetManagedPrefixListAssociationsResponse,

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

    -- ** CreateDhcpOptions
    CreateDhcpOptions (CreateDhcpOptions'),
    newCreateDhcpOptions,
    CreateDhcpOptionsResponse (CreateDhcpOptionsResponse'),
    newCreateDhcpOptionsResponse,

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

    -- ** DescribeVpcs (Paginated)
    DescribeVpcs (DescribeVpcs'),
    newDescribeVpcs,
    DescribeVpcsResponse (DescribeVpcsResponse'),
    newDescribeVpcsResponse,

    -- ** DescribeLocalGateways (Paginated)
    DescribeLocalGateways (DescribeLocalGateways'),
    newDescribeLocalGateways,
    DescribeLocalGatewaysResponse (DescribeLocalGatewaysResponse'),
    newDescribeLocalGatewaysResponse,

    -- ** DescribeIpv6Pools (Paginated)
    DescribeIpv6Pools (DescribeIpv6Pools'),
    newDescribeIpv6Pools,
    DescribeIpv6PoolsResponse (DescribeIpv6PoolsResponse'),
    newDescribeIpv6PoolsResponse,

    -- ** CreateRouteTable
    CreateRouteTable (CreateRouteTable'),
    newCreateRouteTable,
    CreateRouteTableResponse (CreateRouteTableResponse'),
    newCreateRouteTableResponse,

    -- ** DescribeVpcEndpointConnectionNotifications (Paginated)
    DescribeVpcEndpointConnectionNotifications (DescribeVpcEndpointConnectionNotifications'),
    newDescribeVpcEndpointConnectionNotifications,
    DescribeVpcEndpointConnectionNotificationsResponse (DescribeVpcEndpointConnectionNotificationsResponse'),
    newDescribeVpcEndpointConnectionNotificationsResponse,

    -- ** GetTransitGatewayPrefixListReferences (Paginated)
    GetTransitGatewayPrefixListReferences (GetTransitGatewayPrefixListReferences'),
    newGetTransitGatewayPrefixListReferences,
    GetTransitGatewayPrefixListReferencesResponse (GetTransitGatewayPrefixListReferencesResponse'),
    newGetTransitGatewayPrefixListReferencesResponse,

    -- ** AcceptVpcEndpointConnections
    AcceptVpcEndpointConnections (AcceptVpcEndpointConnections'),
    newAcceptVpcEndpointConnections,
    AcceptVpcEndpointConnectionsResponse (AcceptVpcEndpointConnectionsResponse'),
    newAcceptVpcEndpointConnectionsResponse,

    -- ** GetTransitGatewayRouteTablePropagations (Paginated)
    GetTransitGatewayRouteTablePropagations (GetTransitGatewayRouteTablePropagations'),
    newGetTransitGatewayRouteTablePropagations,
    GetTransitGatewayRouteTablePropagationsResponse (GetTransitGatewayRouteTablePropagationsResponse'),
    newGetTransitGatewayRouteTablePropagationsResponse,

    -- ** AssociateDhcpOptions
    AssociateDhcpOptions (AssociateDhcpOptions'),
    newAssociateDhcpOptions,
    AssociateDhcpOptionsResponse (AssociateDhcpOptionsResponse'),
    newAssociateDhcpOptionsResponse,

    -- ** DeleteEgressOnlyInternetGateway
    DeleteEgressOnlyInternetGateway (DeleteEgressOnlyInternetGateway'),
    newDeleteEgressOnlyInternetGateway,
    DeleteEgressOnlyInternetGatewayResponse (DeleteEgressOnlyInternetGatewayResponse'),
    newDeleteEgressOnlyInternetGatewayResponse,

    -- ** GetVpnConnectionDeviceTypes (Paginated)
    GetVpnConnectionDeviceTypes (GetVpnConnectionDeviceTypes'),
    newGetVpnConnectionDeviceTypes,
    GetVpnConnectionDeviceTypesResponse (GetVpnConnectionDeviceTypesResponse'),
    newGetVpnConnectionDeviceTypesResponse,

    -- ** CreateSubnetCidrReservation
    CreateSubnetCidrReservation (CreateSubnetCidrReservation'),
    newCreateSubnetCidrReservation,
    CreateSubnetCidrReservationResponse (CreateSubnetCidrReservationResponse'),
    newCreateSubnetCidrReservationResponse,

    -- ** DisableFastSnapshotRestores
    DisableFastSnapshotRestores (DisableFastSnapshotRestores'),
    newDisableFastSnapshotRestores,
    DisableFastSnapshotRestoresResponse (DisableFastSnapshotRestoresResponse'),
    newDisableFastSnapshotRestoresResponse,

    -- ** RequestSpotInstances
    RequestSpotInstances (RequestSpotInstances'),
    newRequestSpotInstances,
    RequestSpotInstancesResponse (RequestSpotInstancesResponse'),
    newRequestSpotInstancesResponse,

    -- ** DescribeLaunchTemplates (Paginated)
    DescribeLaunchTemplates (DescribeLaunchTemplates'),
    newDescribeLaunchTemplates,
    DescribeLaunchTemplatesResponse (DescribeLaunchTemplatesResponse'),
    newDescribeLaunchTemplatesResponse,

    -- ** CreateImage
    CreateImage (CreateImage'),
    newCreateImage,
    CreateImageResponse (CreateImageResponse'),
    newCreateImageResponse,

    -- ** ModifyTransitGatewayVpcAttachment
    ModifyTransitGatewayVpcAttachment (ModifyTransitGatewayVpcAttachment'),
    newModifyTransitGatewayVpcAttachment,
    ModifyTransitGatewayVpcAttachmentResponse (ModifyTransitGatewayVpcAttachmentResponse'),
    newModifyTransitGatewayVpcAttachmentResponse,

    -- ** AssignIpv6Addresses
    AssignIpv6Addresses (AssignIpv6Addresses'),
    newAssignIpv6Addresses,
    AssignIpv6AddressesResponse (AssignIpv6AddressesResponse'),
    newAssignIpv6AddressesResponse,

    -- ** DescribeLocalGatewayVirtualInterfaceGroups (Paginated)
    DescribeLocalGatewayVirtualInterfaceGroups (DescribeLocalGatewayVirtualInterfaceGroups'),
    newDescribeLocalGatewayVirtualInterfaceGroups,
    DescribeLocalGatewayVirtualInterfaceGroupsResponse (DescribeLocalGatewayVirtualInterfaceGroupsResponse'),
    newDescribeLocalGatewayVirtualInterfaceGroupsResponse,

    -- ** DescribeVpnConnections
    DescribeVpnConnections (DescribeVpnConnections'),
    newDescribeVpnConnections,
    DescribeVpnConnectionsResponse (DescribeVpnConnectionsResponse'),
    newDescribeVpnConnectionsResponse,

    -- ** CreateNetworkAclEntry
    CreateNetworkAclEntry (CreateNetworkAclEntry'),
    newCreateNetworkAclEntry,
    CreateNetworkAclEntryResponse (CreateNetworkAclEntryResponse'),
    newCreateNetworkAclEntryResponse,

    -- ** DescribePlacementGroups
    DescribePlacementGroups (DescribePlacementGroups'),
    newDescribePlacementGroups,
    DescribePlacementGroupsResponse (DescribePlacementGroupsResponse'),
    newDescribePlacementGroupsResponse,

    -- ** ModifySnapshotAttribute
    ModifySnapshotAttribute (ModifySnapshotAttribute'),
    newModifySnapshotAttribute,
    ModifySnapshotAttributeResponse (ModifySnapshotAttributeResponse'),
    newModifySnapshotAttributeResponse,

    -- ** ModifyIdentityIdFormat
    ModifyIdentityIdFormat (ModifyIdentityIdFormat'),
    newModifyIdentityIdFormat,
    ModifyIdentityIdFormatResponse (ModifyIdentityIdFormatResponse'),
    newModifyIdentityIdFormatResponse,

    -- ** EnableVgwRoutePropagation
    EnableVgwRoutePropagation (EnableVgwRoutePropagation'),
    newEnableVgwRoutePropagation,
    EnableVgwRoutePropagationResponse (EnableVgwRoutePropagationResponse'),
    newEnableVgwRoutePropagationResponse,

    -- ** DeleteTags
    DeleteTags (DeleteTags'),
    newDeleteTags,
    DeleteTagsResponse (DeleteTagsResponse'),
    newDeleteTagsResponse,

    -- ** ResetInstanceAttribute
    ResetInstanceAttribute (ResetInstanceAttribute'),
    newResetInstanceAttribute,
    ResetInstanceAttributeResponse (ResetInstanceAttributeResponse'),
    newResetInstanceAttributeResponse,

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

    -- ** DescribeFlowLogs (Paginated)
    DescribeFlowLogs (DescribeFlowLogs'),
    newDescribeFlowLogs,
    DescribeFlowLogsResponse (DescribeFlowLogsResponse'),
    newDescribeFlowLogsResponse,

    -- ** DescribeReservedInstancesOfferings (Paginated)
    DescribeReservedInstancesOfferings (DescribeReservedInstancesOfferings'),
    newDescribeReservedInstancesOfferings,
    DescribeReservedInstancesOfferingsResponse (DescribeReservedInstancesOfferingsResponse'),
    newDescribeReservedInstancesOfferingsResponse,

    -- ** DescribeFleets (Paginated)
    DescribeFleets (DescribeFleets'),
    newDescribeFleets,
    DescribeFleetsResponse (DescribeFleetsResponse'),
    newDescribeFleetsResponse,

    -- ** AttachNetworkInterface
    AttachNetworkInterface (AttachNetworkInterface'),
    newAttachNetworkInterface,
    AttachNetworkInterfaceResponse (AttachNetworkInterfaceResponse'),
    newAttachNetworkInterfaceResponse,

    -- ** ConfirmProductInstance
    ConfirmProductInstance (ConfirmProductInstance'),
    newConfirmProductInstance,
    ConfirmProductInstanceResponse (ConfirmProductInstanceResponse'),
    newConfirmProductInstanceResponse,

    -- ** DescribeTransitGatewayAttachments (Paginated)
    DescribeTransitGatewayAttachments (DescribeTransitGatewayAttachments'),
    newDescribeTransitGatewayAttachments,
    DescribeTransitGatewayAttachmentsResponse (DescribeTransitGatewayAttachmentsResponse'),
    newDescribeTransitGatewayAttachmentsResponse,

    -- ** ModifyAvailabilityZoneGroup
    ModifyAvailabilityZoneGroup (ModifyAvailabilityZoneGroup'),
    newModifyAvailabilityZoneGroup,
    ModifyAvailabilityZoneGroupResponse (ModifyAvailabilityZoneGroupResponse'),
    newModifyAvailabilityZoneGroupResponse,

    -- ** ReplaceNetworkAclEntry
    ReplaceNetworkAclEntry (ReplaceNetworkAclEntry'),
    newReplaceNetworkAclEntry,
    ReplaceNetworkAclEntryResponse (ReplaceNetworkAclEntryResponse'),
    newReplaceNetworkAclEntryResponse,

    -- ** DescribeSpotFleetRequestHistory
    DescribeSpotFleetRequestHistory (DescribeSpotFleetRequestHistory'),
    newDescribeSpotFleetRequestHistory,
    DescribeSpotFleetRequestHistoryResponse (DescribeSpotFleetRequestHistoryResponse'),
    newDescribeSpotFleetRequestHistoryResponse,

    -- ** CreateLocalGatewayRoute
    CreateLocalGatewayRoute (CreateLocalGatewayRoute'),
    newCreateLocalGatewayRoute,
    CreateLocalGatewayRouteResponse (CreateLocalGatewayRouteResponse'),
    newCreateLocalGatewayRouteResponse,

    -- ** DescribeVpcEndpoints (Paginated)
    DescribeVpcEndpoints (DescribeVpcEndpoints'),
    newDescribeVpcEndpoints,
    DescribeVpcEndpointsResponse (DescribeVpcEndpointsResponse'),
    newDescribeVpcEndpointsResponse,

    -- ** ModifyInstanceEventStartTime
    ModifyInstanceEventStartTime (ModifyInstanceEventStartTime'),
    newModifyInstanceEventStartTime,
    ModifyInstanceEventStartTimeResponse (ModifyInstanceEventStartTimeResponse'),
    newModifyInstanceEventStartTimeResponse,

    -- ** DescribeStaleSecurityGroups (Paginated)
    DescribeStaleSecurityGroups (DescribeStaleSecurityGroups'),
    newDescribeStaleSecurityGroups,
    DescribeStaleSecurityGroupsResponse (DescribeStaleSecurityGroupsResponse'),
    newDescribeStaleSecurityGroupsResponse,

    -- ** DescribeInstanceStatus (Paginated)
    DescribeInstanceStatus (DescribeInstanceStatus'),
    newDescribeInstanceStatus,
    DescribeInstanceStatusResponse (DescribeInstanceStatusResponse'),
    newDescribeInstanceStatusResponse,

    -- ** DeleteNetworkAclEntry
    DeleteNetworkAclEntry (DeleteNetworkAclEntry'),
    newDeleteNetworkAclEntry,
    DeleteNetworkAclEntryResponse (DeleteNetworkAclEntryResponse'),
    newDeleteNetworkAclEntryResponse,

    -- ** GetConsoleScreenshot
    GetConsoleScreenshot (GetConsoleScreenshot'),
    newGetConsoleScreenshot,
    GetConsoleScreenshotResponse (GetConsoleScreenshotResponse'),
    newGetConsoleScreenshotResponse,

    -- ** GetGroupsForCapacityReservation (Paginated)
    GetGroupsForCapacityReservation (GetGroupsForCapacityReservation'),
    newGetGroupsForCapacityReservation,
    GetGroupsForCapacityReservationResponse (GetGroupsForCapacityReservationResponse'),
    newGetGroupsForCapacityReservationResponse,

    -- ** DisassociateIamInstanceProfile
    DisassociateIamInstanceProfile (DisassociateIamInstanceProfile'),
    newDisassociateIamInstanceProfile,
    DisassociateIamInstanceProfileResponse (DisassociateIamInstanceProfileResponse'),
    newDisassociateIamInstanceProfileResponse,

    -- ** DescribeVpcEndpointServiceConfigurations (Paginated)
    DescribeVpcEndpointServiceConfigurations (DescribeVpcEndpointServiceConfigurations'),
    newDescribeVpcEndpointServiceConfigurations,
    DescribeVpcEndpointServiceConfigurationsResponse (DescribeVpcEndpointServiceConfigurationsResponse'),
    newDescribeVpcEndpointServiceConfigurationsResponse,

    -- ** CancelSpotInstanceRequests
    CancelSpotInstanceRequests (CancelSpotInstanceRequests'),
    newCancelSpotInstanceRequests,
    CancelSpotInstanceRequestsResponse (CancelSpotInstanceRequestsResponse'),
    newCancelSpotInstanceRequestsResponse,

    -- ** DeleteLocalGatewayRoute
    DeleteLocalGatewayRoute (DeleteLocalGatewayRoute'),
    newDeleteLocalGatewayRoute,
    DeleteLocalGatewayRouteResponse (DeleteLocalGatewayRouteResponse'),
    newDeleteLocalGatewayRouteResponse,

    -- ** DescribeVpcEndpointServices (Paginated)
    DescribeVpcEndpointServices (DescribeVpcEndpointServices'),
    newDescribeVpcEndpointServices,
    DescribeVpcEndpointServicesResponse (DescribeVpcEndpointServicesResponse'),
    newDescribeVpcEndpointServicesResponse,

    -- ** DisassociateRouteTable
    DisassociateRouteTable (DisassociateRouteTable'),
    newDisassociateRouteTable,
    DisassociateRouteTableResponse (DisassociateRouteTableResponse'),
    newDisassociateRouteTableResponse,

    -- ** AssignPrivateIpAddresses
    AssignPrivateIpAddresses (AssignPrivateIpAddresses'),
    newAssignPrivateIpAddresses,
    AssignPrivateIpAddressesResponse (AssignPrivateIpAddressesResponse'),
    newAssignPrivateIpAddressesResponse,

    -- ** GetFlowLogsIntegrationTemplate
    GetFlowLogsIntegrationTemplate (GetFlowLogsIntegrationTemplate'),
    newGetFlowLogsIntegrationTemplate,
    GetFlowLogsIntegrationTemplateResponse (GetFlowLogsIntegrationTemplateResponse'),
    newGetFlowLogsIntegrationTemplateResponse,

    -- ** ModifyVpnTunnelCertificate
    ModifyVpnTunnelCertificate (ModifyVpnTunnelCertificate'),
    newModifyVpnTunnelCertificate,
    ModifyVpnTunnelCertificateResponse (ModifyVpnTunnelCertificateResponse'),
    newModifyVpnTunnelCertificateResponse,

    -- ** DisableVgwRoutePropagation
    DisableVgwRoutePropagation (DisableVgwRoutePropagation'),
    newDisableVgwRoutePropagation,
    DisableVgwRoutePropagationResponse (DisableVgwRoutePropagationResponse'),
    newDisableVgwRoutePropagationResponse,

    -- ** DeleteSnapshot
    DeleteSnapshot (DeleteSnapshot'),
    newDeleteSnapshot,
    DeleteSnapshotResponse (DeleteSnapshotResponse'),
    newDeleteSnapshotResponse,

    -- ** DescribeSubnets (Paginated)
    DescribeSubnets (DescribeSubnets'),
    newDescribeSubnets,
    DescribeSubnetsResponse (DescribeSubnetsResponse'),
    newDescribeSubnetsResponse,

    -- ** CreateSpotDatafeedSubscription
    CreateSpotDatafeedSubscription (CreateSpotDatafeedSubscription'),
    newCreateSpotDatafeedSubscription,
    CreateSpotDatafeedSubscriptionResponse (CreateSpotDatafeedSubscriptionResponse'),
    newCreateSpotDatafeedSubscriptionResponse,

    -- ** UnmonitorInstances
    UnmonitorInstances (UnmonitorInstances'),
    newUnmonitorInstances,
    UnmonitorInstancesResponse (UnmonitorInstancesResponse'),
    newUnmonitorInstancesResponse,

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

    -- ** PurchaseReservedInstancesOffering
    PurchaseReservedInstancesOffering (PurchaseReservedInstancesOffering'),
    newPurchaseReservedInstancesOffering,
    PurchaseReservedInstancesOfferingResponse (PurchaseReservedInstancesOfferingResponse'),
    newPurchaseReservedInstancesOfferingResponse,

    -- ** DescribeSnapshotAttribute
    DescribeSnapshotAttribute (DescribeSnapshotAttribute'),
    newDescribeSnapshotAttribute,
    DescribeSnapshotAttributeResponse (DescribeSnapshotAttributeResponse'),
    newDescribeSnapshotAttributeResponse,

    -- ** AuthorizeSecurityGroupIngress
    AuthorizeSecurityGroupIngress (AuthorizeSecurityGroupIngress'),
    newAuthorizeSecurityGroupIngress,
    AuthorizeSecurityGroupIngressResponse (AuthorizeSecurityGroupIngressResponse'),
    newAuthorizeSecurityGroupIngressResponse,

    -- ** DescribeNatGateways (Paginated)
    DescribeNatGateways (DescribeNatGateways'),
    newDescribeNatGateways,
    DescribeNatGatewaysResponse (DescribeNatGatewaysResponse'),
    newDescribeNatGatewaysResponse,

    -- ** DisableVpcClassicLink
    DisableVpcClassicLink (DisableVpcClassicLink'),
    newDisableVpcClassicLink,
    DisableVpcClassicLinkResponse (DisableVpcClassicLinkResponse'),
    newDisableVpcClassicLinkResponse,

    -- ** DescribeTransitGatewayMulticastDomains (Paginated)
    DescribeTransitGatewayMulticastDomains (DescribeTransitGatewayMulticastDomains'),
    newDescribeTransitGatewayMulticastDomains,
    DescribeTransitGatewayMulticastDomainsResponse (DescribeTransitGatewayMulticastDomainsResponse'),
    newDescribeTransitGatewayMulticastDomainsResponse,

    -- ** GetTransitGatewayAttachmentPropagations (Paginated)
    GetTransitGatewayAttachmentPropagations (GetTransitGatewayAttachmentPropagations'),
    newGetTransitGatewayAttachmentPropagations,
    GetTransitGatewayAttachmentPropagationsResponse (GetTransitGatewayAttachmentPropagationsResponse'),
    newGetTransitGatewayAttachmentPropagationsResponse,

    -- ** ModifyVpcEndpointConnectionNotification
    ModifyVpcEndpointConnectionNotification (ModifyVpcEndpointConnectionNotification'),
    newModifyVpcEndpointConnectionNotification,
    ModifyVpcEndpointConnectionNotificationResponse (ModifyVpcEndpointConnectionNotificationResponse'),
    newModifyVpcEndpointConnectionNotificationResponse,

    -- ** RestoreManagedPrefixListVersion
    RestoreManagedPrefixListVersion (RestoreManagedPrefixListVersion'),
    newRestoreManagedPrefixListVersion,
    RestoreManagedPrefixListVersionResponse (RestoreManagedPrefixListVersionResponse'),
    newRestoreManagedPrefixListVersionResponse,

    -- ** DescribeTransitGatewayConnectPeers (Paginated)
    DescribeTransitGatewayConnectPeers (DescribeTransitGatewayConnectPeers'),
    newDescribeTransitGatewayConnectPeers,
    DescribeTransitGatewayConnectPeersResponse (DescribeTransitGatewayConnectPeersResponse'),
    newDescribeTransitGatewayConnectPeersResponse,

    -- ** DeleteCarrierGateway
    DeleteCarrierGateway (DeleteCarrierGateway'),
    newDeleteCarrierGateway,
    DeleteCarrierGatewayResponse (DeleteCarrierGatewayResponse'),
    newDeleteCarrierGatewayResponse,

    -- ** DescribeNetworkInterfaces (Paginated)
    DescribeNetworkInterfaces (DescribeNetworkInterfaces'),
    newDescribeNetworkInterfaces,
    DescribeNetworkInterfacesResponse (DescribeNetworkInterfacesResponse'),
    newDescribeNetworkInterfacesResponse,

    -- ** DescribeTransitGatewayVpcAttachments (Paginated)
    DescribeTransitGatewayVpcAttachments (DescribeTransitGatewayVpcAttachments'),
    newDescribeTransitGatewayVpcAttachments,
    DescribeTransitGatewayVpcAttachmentsResponse (DescribeTransitGatewayVpcAttachmentsResponse'),
    newDescribeTransitGatewayVpcAttachmentsResponse,

    -- ** ModifyAddressAttribute
    ModifyAddressAttribute (ModifyAddressAttribute'),
    newModifyAddressAttribute,
    ModifyAddressAttributeResponse (ModifyAddressAttributeResponse'),
    newModifyAddressAttributeResponse,

    -- ** DescribeImportSnapshotTasks (Paginated)
    DescribeImportSnapshotTasks (DescribeImportSnapshotTasks'),
    newDescribeImportSnapshotTasks,
    DescribeImportSnapshotTasksResponse (DescribeImportSnapshotTasksResponse'),
    newDescribeImportSnapshotTasksResponse,

    -- ** CopyImage
    CopyImage (CopyImage'),
    newCopyImage,
    CopyImageResponse (CopyImageResponse'),
    newCopyImageResponse,

    -- ** DescribeInstanceEventNotificationAttributes
    DescribeInstanceEventNotificationAttributes (DescribeInstanceEventNotificationAttributes'),
    newDescribeInstanceEventNotificationAttributes,
    DescribeInstanceEventNotificationAttributesResponse (DescribeInstanceEventNotificationAttributesResponse'),
    newDescribeInstanceEventNotificationAttributesResponse,

    -- ** EnableSerialConsoleAccess
    EnableSerialConsoleAccess (EnableSerialConsoleAccess'),
    newEnableSerialConsoleAccess,
    EnableSerialConsoleAccessResponse (EnableSerialConsoleAccessResponse'),
    newEnableSerialConsoleAccessResponse,

    -- ** ModifyTrafficMirrorFilterRule
    ModifyTrafficMirrorFilterRule (ModifyTrafficMirrorFilterRule'),
    newModifyTrafficMirrorFilterRule,
    ModifyTrafficMirrorFilterRuleResponse (ModifyTrafficMirrorFilterRuleResponse'),
    newModifyTrafficMirrorFilterRuleResponse,

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

    -- ** ModifyInstanceCapacityReservationAttributes
    ModifyInstanceCapacityReservationAttributes (ModifyInstanceCapacityReservationAttributes'),
    newModifyInstanceCapacityReservationAttributes,
    ModifyInstanceCapacityReservationAttributesResponse (ModifyInstanceCapacityReservationAttributesResponse'),
    newModifyInstanceCapacityReservationAttributesResponse,

    -- ** DescribeNetworkInterfaceAttribute
    DescribeNetworkInterfaceAttribute (DescribeNetworkInterfaceAttribute'),
    newDescribeNetworkInterfaceAttribute,
    DescribeNetworkInterfaceAttributeResponse (DescribeNetworkInterfaceAttributeResponse'),
    newDescribeNetworkInterfaceAttributeResponse,

    -- ** AttachClassicLinkVpc
    AttachClassicLinkVpc (AttachClassicLinkVpc'),
    newAttachClassicLinkVpc,
    AttachClassicLinkVpcResponse (AttachClassicLinkVpcResponse'),
    newAttachClassicLinkVpcResponse,

    -- ** GetSubnetCidrReservations
    GetSubnetCidrReservations (GetSubnetCidrReservations'),
    newGetSubnetCidrReservations,
    GetSubnetCidrReservationsResponse (GetSubnetCidrReservationsResponse'),
    newGetSubnetCidrReservationsResponse,

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

    -- ** DisableTransitGatewayRouteTablePropagation
    DisableTransitGatewayRouteTablePropagation (DisableTransitGatewayRouteTablePropagation'),
    newDisableTransitGatewayRouteTablePropagation,
    DisableTransitGatewayRouteTablePropagationResponse (DisableTransitGatewayRouteTablePropagationResponse'),
    newDisableTransitGatewayRouteTablePropagationResponse,

    -- ** CancelReservedInstancesListing
    CancelReservedInstancesListing (CancelReservedInstancesListing'),
    newCancelReservedInstancesListing,
    CancelReservedInstancesListingResponse (CancelReservedInstancesListingResponse'),
    newCancelReservedInstancesListingResponse,

    -- ** DeleteQueuedReservedInstances
    DeleteQueuedReservedInstances (DeleteQueuedReservedInstances'),
    newDeleteQueuedReservedInstances,
    DeleteQueuedReservedInstancesResponse (DeleteQueuedReservedInstancesResponse'),
    newDeleteQueuedReservedInstancesResponse,

    -- ** CreateFleet
    CreateFleet (CreateFleet'),
    newCreateFleet,
    CreateFleetResponse (CreateFleetResponse'),
    newCreateFleetResponse,

    -- ** DescribeSnapshots (Paginated)
    DescribeSnapshots (DescribeSnapshots'),
    newDescribeSnapshots,
    DescribeSnapshotsResponse (DescribeSnapshotsResponse'),
    newDescribeSnapshotsResponse,

    -- ** DescribeReservedInstancesListings
    DescribeReservedInstancesListings (DescribeReservedInstancesListings'),
    newDescribeReservedInstancesListings,
    DescribeReservedInstancesListingsResponse (DescribeReservedInstancesListingsResponse'),
    newDescribeReservedInstancesListingsResponse,

    -- ** ModifyClientVpnEndpoint
    ModifyClientVpnEndpoint (ModifyClientVpnEndpoint'),
    newModifyClientVpnEndpoint,
    ModifyClientVpnEndpointResponse (ModifyClientVpnEndpointResponse'),
    newModifyClientVpnEndpointResponse,

    -- ** CreateVpcEndpoint
    CreateVpcEndpoint (CreateVpcEndpoint'),
    newCreateVpcEndpoint,
    CreateVpcEndpointResponse (CreateVpcEndpointResponse'),
    newCreateVpcEndpointResponse,

    -- ** CreateVpnConnection
    CreateVpnConnection (CreateVpnConnection'),
    newCreateVpnConnection,
    CreateVpnConnectionResponse (CreateVpnConnectionResponse'),
    newCreateVpnConnectionResponse,

    -- ** ImportClientVpnClientCertificateRevocationList
    ImportClientVpnClientCertificateRevocationList (ImportClientVpnClientCertificateRevocationList'),
    newImportClientVpnClientCertificateRevocationList,
    ImportClientVpnClientCertificateRevocationListResponse (ImportClientVpnClientCertificateRevocationListResponse'),
    newImportClientVpnClientCertificateRevocationListResponse,

    -- ** AssociateSubnetCidrBlock
    AssociateSubnetCidrBlock (AssociateSubnetCidrBlock'),
    newAssociateSubnetCidrBlock,
    AssociateSubnetCidrBlockResponse (AssociateSubnetCidrBlockResponse'),
    newAssociateSubnetCidrBlockResponse,

    -- ** DescribeSpotPriceHistory (Paginated)
    DescribeSpotPriceHistory (DescribeSpotPriceHistory'),
    newDescribeSpotPriceHistory,
    DescribeSpotPriceHistoryResponse (DescribeSpotPriceHistoryResponse'),
    newDescribeSpotPriceHistoryResponse,

    -- ** EnableEbsEncryptionByDefault
    EnableEbsEncryptionByDefault (EnableEbsEncryptionByDefault'),
    newEnableEbsEncryptionByDefault,
    EnableEbsEncryptionByDefaultResponse (EnableEbsEncryptionByDefaultResponse'),
    newEnableEbsEncryptionByDefaultResponse,

    -- ** DescribeVpcClassicLinkDnsSupport (Paginated)
    DescribeVpcClassicLinkDnsSupport (DescribeVpcClassicLinkDnsSupport'),
    newDescribeVpcClassicLinkDnsSupport,
    DescribeVpcClassicLinkDnsSupportResponse (DescribeVpcClassicLinkDnsSupportResponse'),
    newDescribeVpcClassicLinkDnsSupportResponse,

    -- ** CreateLocalGatewayRouteTableVpcAssociation
    CreateLocalGatewayRouteTableVpcAssociation (CreateLocalGatewayRouteTableVpcAssociation'),
    newCreateLocalGatewayRouteTableVpcAssociation,
    CreateLocalGatewayRouteTableVpcAssociationResponse (CreateLocalGatewayRouteTableVpcAssociationResponse'),
    newCreateLocalGatewayRouteTableVpcAssociationResponse,

    -- ** DescribeAggregateIdFormat
    DescribeAggregateIdFormat (DescribeAggregateIdFormat'),
    newDescribeAggregateIdFormat,
    DescribeAggregateIdFormatResponse (DescribeAggregateIdFormatResponse'),
    newDescribeAggregateIdFormatResponse,

    -- ** EnableTransitGatewayRouteTablePropagation
    EnableTransitGatewayRouteTablePropagation (EnableTransitGatewayRouteTablePropagation'),
    newEnableTransitGatewayRouteTablePropagation,
    EnableTransitGatewayRouteTablePropagationResponse (EnableTransitGatewayRouteTablePropagationResponse'),
    newEnableTransitGatewayRouteTablePropagationResponse,

    -- ** RequestSpotFleet
    RequestSpotFleet (RequestSpotFleet'),
    newRequestSpotFleet,
    RequestSpotFleetResponse (RequestSpotFleetResponse'),
    newRequestSpotFleetResponse,

    -- ** DescribeBundleTasks
    DescribeBundleTasks (DescribeBundleTasks'),
    newDescribeBundleTasks,
    DescribeBundleTasksResponse (DescribeBundleTasksResponse'),
    newDescribeBundleTasksResponse,

    -- ** ModifyNetworkInterfaceAttribute
    ModifyNetworkInterfaceAttribute (ModifyNetworkInterfaceAttribute'),
    newModifyNetworkInterfaceAttribute,
    ModifyNetworkInterfaceAttributeResponse (ModifyNetworkInterfaceAttributeResponse'),
    newModifyNetworkInterfaceAttributeResponse,

    -- ** DisableSerialConsoleAccess
    DisableSerialConsoleAccess (DisableSerialConsoleAccess'),
    newDisableSerialConsoleAccess,
    DisableSerialConsoleAccessResponse (DisableSerialConsoleAccessResponse'),
    newDisableSerialConsoleAccessResponse,

    -- ** DescribeInstanceTypeOfferings (Paginated)
    DescribeInstanceTypeOfferings (DescribeInstanceTypeOfferings'),
    newDescribeInstanceTypeOfferings,
    DescribeInstanceTypeOfferingsResponse (DescribeInstanceTypeOfferingsResponse'),
    newDescribeInstanceTypeOfferingsResponse,

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

    -- ** ResetAddressAttribute
    ResetAddressAttribute (ResetAddressAttribute'),
    newResetAddressAttribute,
    ResetAddressAttributeResponse (ResetAddressAttributeResponse'),
    newResetAddressAttributeResponse,

    -- ** DescribeCoipPools (Paginated)
    DescribeCoipPools (DescribeCoipPools'),
    newDescribeCoipPools,
    DescribeCoipPoolsResponse (DescribeCoipPoolsResponse'),
    newDescribeCoipPoolsResponse,

    -- ** DescribeInstanceEventWindows (Paginated)
    DescribeInstanceEventWindows (DescribeInstanceEventWindows'),
    newDescribeInstanceEventWindows,
    DescribeInstanceEventWindowsResponse (DescribeInstanceEventWindowsResponse'),
    newDescribeInstanceEventWindowsResponse,

    -- ** CreateSubnet
    CreateSubnet (CreateSubnet'),
    newCreateSubnet,
    CreateSubnetResponse (CreateSubnetResponse'),
    newCreateSubnetResponse,

    -- ** DescribeSecurityGroups (Paginated)
    DescribeSecurityGroups (DescribeSecurityGroups'),
    newDescribeSecurityGroups,
    DescribeSecurityGroupsResponse (DescribeSecurityGroupsResponse'),
    newDescribeSecurityGroupsResponse,

    -- ** DeletePlacementGroup
    DeletePlacementGroup (DeletePlacementGroup'),
    newDeletePlacementGroup,
    DeletePlacementGroupResponse (DeletePlacementGroupResponse'),
    newDeletePlacementGroupResponse,

    -- ** EnableVolumeIO
    EnableVolumeIO (EnableVolumeIO'),
    newEnableVolumeIO,
    EnableVolumeIOResponse (EnableVolumeIOResponse'),
    newEnableVolumeIOResponse,

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

    -- ** ModifySubnetAttribute
    ModifySubnetAttribute (ModifySubnetAttribute'),
    newModifySubnetAttribute,
    ModifySubnetAttributeResponse (ModifySubnetAttributeResponse'),
    newModifySubnetAttributeResponse,

    -- ** DescribeTransitGatewayConnects (Paginated)
    DescribeTransitGatewayConnects (DescribeTransitGatewayConnects'),
    newDescribeTransitGatewayConnects,
    DescribeTransitGatewayConnectsResponse (DescribeTransitGatewayConnectsResponse'),
    newDescribeTransitGatewayConnectsResponse,

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

    -- ** DeleteVpnConnection
    DeleteVpnConnection (DeleteVpnConnection'),
    newDeleteVpnConnection,
    DeleteVpnConnectionResponse (DeleteVpnConnectionResponse'),
    newDeleteVpnConnectionResponse,

    -- ** DescribeInstanceTypes (Paginated)
    DescribeInstanceTypes (DescribeInstanceTypes'),
    newDescribeInstanceTypes,
    DescribeInstanceTypesResponse (DescribeInstanceTypesResponse'),
    newDescribeInstanceTypesResponse,

    -- ** CancelExportTask
    CancelExportTask (CancelExportTask'),
    newCancelExportTask,
    CancelExportTaskResponse (CancelExportTaskResponse'),
    newCancelExportTaskResponse,

    -- ** ModifyTransitGateway
    ModifyTransitGateway (ModifyTransitGateway'),
    newModifyTransitGateway,
    ModifyTransitGatewayResponse (ModifyTransitGatewayResponse'),
    newModifyTransitGatewayResponse,

    -- ** CreateInternetGateway
    CreateInternetGateway (CreateInternetGateway'),
    newCreateInternetGateway,
    CreateInternetGatewayResponse (CreateInternetGatewayResponse'),
    newCreateInternetGatewayResponse,

    -- ** SendDiagnosticInterrupt
    SendDiagnosticInterrupt (SendDiagnosticInterrupt'),
    newSendDiagnosticInterrupt,
    SendDiagnosticInterruptResponse (SendDiagnosticInterruptResponse'),
    newSendDiagnosticInterruptResponse,

    -- ** DisassociateClientVpnTargetNetwork
    DisassociateClientVpnTargetNetwork (DisassociateClientVpnTargetNetwork'),
    newDisassociateClientVpnTargetNetwork,
    DisassociateClientVpnTargetNetworkResponse (DisassociateClientVpnTargetNetworkResponse'),
    newDisassociateClientVpnTargetNetworkResponse,

    -- ** ModifyInstanceMetadataOptions
    ModifyInstanceMetadataOptions (ModifyInstanceMetadataOptions'),
    newModifyInstanceMetadataOptions,
    ModifyInstanceMetadataOptionsResponse (ModifyInstanceMetadataOptionsResponse'),
    newModifyInstanceMetadataOptionsResponse,

    -- ** DescribeSpotDatafeedSubscription
    DescribeSpotDatafeedSubscription (DescribeSpotDatafeedSubscription'),
    newDescribeSpotDatafeedSubscription,
    DescribeSpotDatafeedSubscriptionResponse (DescribeSpotDatafeedSubscriptionResponse'),
    newDescribeSpotDatafeedSubscriptionResponse,

    -- ** ExportClientVpnClientConfiguration
    ExportClientVpnClientConfiguration (ExportClientVpnClientConfiguration'),
    newExportClientVpnClientConfiguration,
    ExportClientVpnClientConfigurationResponse (ExportClientVpnClientConfigurationResponse'),
    newExportClientVpnClientConfigurationResponse,

    -- ** DeleteKeyPair
    DeleteKeyPair (DeleteKeyPair'),
    newDeleteKeyPair,
    DeleteKeyPairResponse (DeleteKeyPairResponse'),
    newDeleteKeyPairResponse,

    -- ** DescribeEgressOnlyInternetGateways (Paginated)
    DescribeEgressOnlyInternetGateways (DescribeEgressOnlyInternetGateways'),
    newDescribeEgressOnlyInternetGateways,
    DescribeEgressOnlyInternetGatewaysResponse (DescribeEgressOnlyInternetGatewaysResponse'),
    newDescribeEgressOnlyInternetGatewaysResponse,

    -- ** CreateVolume
    CreateVolume (CreateVolume'),
    newCreateVolume,
    Volume (Volume'),
    newVolume,

    -- ** ModifyTrafficMirrorFilterNetworkServices
    ModifyTrafficMirrorFilterNetworkServices (ModifyTrafficMirrorFilterNetworkServices'),
    newModifyTrafficMirrorFilterNetworkServices,
    ModifyTrafficMirrorFilterNetworkServicesResponse (ModifyTrafficMirrorFilterNetworkServicesResponse'),
    newModifyTrafficMirrorFilterNetworkServicesResponse,

    -- ** DescribeVpcAttribute
    DescribeVpcAttribute (DescribeVpcAttribute'),
    newDescribeVpcAttribute,
    DescribeVpcAttributeResponse (DescribeVpcAttributeResponse'),
    newDescribeVpcAttributeResponse,

    -- ** DescribeTrunkInterfaceAssociations (Paginated)
    DescribeTrunkInterfaceAssociations (DescribeTrunkInterfaceAssociations'),
    newDescribeTrunkInterfaceAssociations,
    DescribeTrunkInterfaceAssociationsResponse (DescribeTrunkInterfaceAssociationsResponse'),
    newDescribeTrunkInterfaceAssociationsResponse,

    -- ** CreateInstanceExportTask
    CreateInstanceExportTask (CreateInstanceExportTask'),
    newCreateInstanceExportTask,
    CreateInstanceExportTaskResponse (CreateInstanceExportTaskResponse'),
    newCreateInstanceExportTaskResponse,

    -- ** CreateClientVpnRoute
    CreateClientVpnRoute (CreateClientVpnRoute'),
    newCreateClientVpnRoute,
    CreateClientVpnRouteResponse (CreateClientVpnRouteResponse'),
    newCreateClientVpnRouteResponse,

    -- ** ModifyCapacityReservation
    ModifyCapacityReservation (ModifyCapacityReservation'),
    newModifyCapacityReservation,
    ModifyCapacityReservationResponse (ModifyCapacityReservationResponse'),
    newModifyCapacityReservationResponse,

    -- ** RevokeSecurityGroupEgress
    RevokeSecurityGroupEgress (RevokeSecurityGroupEgress'),
    newRevokeSecurityGroupEgress,
    RevokeSecurityGroupEgressResponse (RevokeSecurityGroupEgressResponse'),
    newRevokeSecurityGroupEgressResponse,

    -- ** DescribeSecurityGroupReferences
    DescribeSecurityGroupReferences (DescribeSecurityGroupReferences'),
    newDescribeSecurityGroupReferences,
    DescribeSecurityGroupReferencesResponse (DescribeSecurityGroupReferencesResponse'),
    newDescribeSecurityGroupReferencesResponse,

    -- ** DisassociateSubnetCidrBlock
    DisassociateSubnetCidrBlock (DisassociateSubnetCidrBlock'),
    newDisassociateSubnetCidrBlock,
    DisassociateSubnetCidrBlockResponse (DisassociateSubnetCidrBlockResponse'),
    newDisassociateSubnetCidrBlockResponse,

    -- ** DetachInternetGateway
    DetachInternetGateway (DetachInternetGateway'),
    newDetachInternetGateway,
    DetachInternetGatewayResponse (DetachInternetGatewayResponse'),
    newDetachInternetGatewayResponse,

    -- ** SearchTransitGatewayMulticastGroups (Paginated)
    SearchTransitGatewayMulticastGroups (SearchTransitGatewayMulticastGroups'),
    newSearchTransitGatewayMulticastGroups,
    SearchTransitGatewayMulticastGroupsResponse (SearchTransitGatewayMulticastGroupsResponse'),
    newSearchTransitGatewayMulticastGroupsResponse,

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

    -- ** ModifyVpcAttribute
    ModifyVpcAttribute (ModifyVpcAttribute'),
    newModifyVpcAttribute,
    ModifyVpcAttributeResponse (ModifyVpcAttributeResponse'),
    newModifyVpcAttributeResponse,

    -- ** DeleteClientVpnRoute
    DeleteClientVpnRoute (DeleteClientVpnRoute'),
    newDeleteClientVpnRoute,
    DeleteClientVpnRouteResponse (DeleteClientVpnRouteResponse'),
    newDeleteClientVpnRouteResponse,

    -- ** DeprovisionByoipCidr
    DeprovisionByoipCidr (DeprovisionByoipCidr'),
    newDeprovisionByoipCidr,
    DeprovisionByoipCidrResponse (DeprovisionByoipCidrResponse'),
    newDeprovisionByoipCidrResponse,

    -- ** DisassociateTrunkInterface
    DisassociateTrunkInterface (DisassociateTrunkInterface'),
    newDisassociateTrunkInterface,
    DisassociateTrunkInterfaceResponse (DisassociateTrunkInterfaceResponse'),
    newDisassociateTrunkInterfaceResponse,

    -- ** ImportSnapshot
    ImportSnapshot (ImportSnapshot'),
    newImportSnapshot,
    ImportSnapshotResponse (ImportSnapshotResponse'),
    newImportSnapshotResponse,

    -- ** DescribeSpotFleetInstances (Paginated)
    DescribeSpotFleetInstances (DescribeSpotFleetInstances'),
    newDescribeSpotFleetInstances,
    DescribeSpotFleetInstancesResponse (DescribeSpotFleetInstancesResponse'),
    newDescribeSpotFleetInstancesResponse,

    -- ** DescribeClientVpnConnections (Paginated)
    DescribeClientVpnConnections (DescribeClientVpnConnections'),
    newDescribeClientVpnConnections,
    DescribeClientVpnConnectionsResponse (DescribeClientVpnConnectionsResponse'),
    newDescribeClientVpnConnectionsResponse,

    -- ** CreateTrafficMirrorTarget
    CreateTrafficMirrorTarget (CreateTrafficMirrorTarget'),
    newCreateTrafficMirrorTarget,
    CreateTrafficMirrorTargetResponse (CreateTrafficMirrorTargetResponse'),
    newCreateTrafficMirrorTargetResponse,

    -- ** ModifyInstanceCreditSpecification
    ModifyInstanceCreditSpecification (ModifyInstanceCreditSpecification'),
    newModifyInstanceCreditSpecification,
    ModifyInstanceCreditSpecificationResponse (ModifyInstanceCreditSpecificationResponse'),
    newModifyInstanceCreditSpecificationResponse,

    -- ** AcceptVpcPeeringConnection
    AcceptVpcPeeringConnection (AcceptVpcPeeringConnection'),
    newAcceptVpcPeeringConnection,
    AcceptVpcPeeringConnectionResponse (AcceptVpcPeeringConnectionResponse'),
    newAcceptVpcPeeringConnectionResponse,

    -- ** DescribeVolumeAttribute
    DescribeVolumeAttribute (DescribeVolumeAttribute'),
    newDescribeVolumeAttribute,
    DescribeVolumeAttributeResponse (DescribeVolumeAttributeResponse'),
    newDescribeVolumeAttributeResponse,

    -- ** DescribeSpotFleetRequests (Paginated)
    DescribeSpotFleetRequests (DescribeSpotFleetRequests'),
    newDescribeSpotFleetRequests,
    DescribeSpotFleetRequestsResponse (DescribeSpotFleetRequestsResponse'),
    newDescribeSpotFleetRequestsResponse,

    -- ** DescribeAddressesAttribute (Paginated)
    DescribeAddressesAttribute (DescribeAddressesAttribute'),
    newDescribeAddressesAttribute,
    DescribeAddressesAttributeResponse (DescribeAddressesAttributeResponse'),
    newDescribeAddressesAttributeResponse,

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

    -- ** ModifyVpcPeeringConnectionOptions
    ModifyVpcPeeringConnectionOptions (ModifyVpcPeeringConnectionOptions'),
    newModifyVpcPeeringConnectionOptions,
    ModifyVpcPeeringConnectionOptionsResponse (ModifyVpcPeeringConnectionOptionsResponse'),
    newModifyVpcPeeringConnectionOptionsResponse,

    -- ** CreateVpcEndpointConnectionNotification
    CreateVpcEndpointConnectionNotification (CreateVpcEndpointConnectionNotification'),
    newCreateVpcEndpointConnectionNotification,
    CreateVpcEndpointConnectionNotificationResponse (CreateVpcEndpointConnectionNotificationResponse'),
    newCreateVpcEndpointConnectionNotificationResponse,

    -- ** DeleteVpcEndpointServiceConfigurations
    DeleteVpcEndpointServiceConfigurations (DeleteVpcEndpointServiceConfigurations'),
    newDeleteVpcEndpointServiceConfigurations,
    DeleteVpcEndpointServiceConfigurationsResponse (DeleteVpcEndpointServiceConfigurationsResponse'),
    newDeleteVpcEndpointServiceConfigurationsResponse,

    -- ** DescribeFleetHistory
    DescribeFleetHistory (DescribeFleetHistory'),
    newDescribeFleetHistory,
    DescribeFleetHistoryResponse (DescribeFleetHistoryResponse'),
    newDescribeFleetHistoryResponse,

    -- ** CreateVpc
    CreateVpc (CreateVpc'),
    newCreateVpc,
    CreateVpcResponse (CreateVpcResponse'),
    newCreateVpcResponse,

    -- ** DescribeVolumeStatus (Paginated)
    DescribeVolumeStatus (DescribeVolumeStatus'),
    newDescribeVolumeStatus,
    DescribeVolumeStatusResponse (DescribeVolumeStatusResponse'),
    newDescribeVolumeStatusResponse,

    -- ** GetSerialConsoleAccessStatus
    GetSerialConsoleAccessStatus (GetSerialConsoleAccessStatus'),
    newGetSerialConsoleAccessStatus,
    GetSerialConsoleAccessStatusResponse (GetSerialConsoleAccessStatusResponse'),
    newGetSerialConsoleAccessStatusResponse,

    -- ** DescribeReplaceRootVolumeTasks (Paginated)
    DescribeReplaceRootVolumeTasks (DescribeReplaceRootVolumeTasks'),
    newDescribeReplaceRootVolumeTasks,
    DescribeReplaceRootVolumeTasksResponse (DescribeReplaceRootVolumeTasksResponse'),
    newDescribeReplaceRootVolumeTasksResponse,

    -- ** DescribeImages
    DescribeImages (DescribeImages'),
    newDescribeImages,
    DescribeImagesResponse (DescribeImagesResponse'),
    newDescribeImagesResponse,

    -- ** DeleteVolume
    DeleteVolume (DeleteVolume'),
    newDeleteVolume,
    DeleteVolumeResponse (DeleteVolumeResponse'),
    newDeleteVolumeResponse,

    -- ** SearchLocalGatewayRoutes (Paginated)
    SearchLocalGatewayRoutes (SearchLocalGatewayRoutes'),
    newSearchLocalGatewayRoutes,
    SearchLocalGatewayRoutesResponse (SearchLocalGatewayRoutesResponse'),
    newSearchLocalGatewayRoutesResponse,

    -- ** DescribeMovingAddresses (Paginated)
    DescribeMovingAddresses (DescribeMovingAddresses'),
    newDescribeMovingAddresses,
    DescribeMovingAddressesResponse (DescribeMovingAddressesResponse'),
    newDescribeMovingAddressesResponse,

    -- ** CreateTrafficMirrorSession
    CreateTrafficMirrorSession (CreateTrafficMirrorSession'),
    newCreateTrafficMirrorSession,
    CreateTrafficMirrorSessionResponse (CreateTrafficMirrorSessionResponse'),
    newCreateTrafficMirrorSessionResponse,

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

    -- ** RejectTransitGatewayMulticastDomainAssociations
    RejectTransitGatewayMulticastDomainAssociations (RejectTransitGatewayMulticastDomainAssociations'),
    newRejectTransitGatewayMulticastDomainAssociations,
    RejectTransitGatewayMulticastDomainAssociationsResponse (RejectTransitGatewayMulticastDomainAssociationsResponse'),
    newRejectTransitGatewayMulticastDomainAssociationsResponse,

    -- ** DeleteTransitGateway
    DeleteTransitGateway (DeleteTransitGateway'),
    newDeleteTransitGateway,
    DeleteTransitGatewayResponse (DeleteTransitGatewayResponse'),
    newDeleteTransitGatewayResponse,

    -- ** DescribeHosts (Paginated)
    DescribeHosts (DescribeHosts'),
    newDescribeHosts,
    DescribeHostsResponse (DescribeHostsResponse'),
    newDescribeHostsResponse,

    -- ** DescribeNetworkInterfacePermissions (Paginated)
    DescribeNetworkInterfacePermissions (DescribeNetworkInterfacePermissions'),
    newDescribeNetworkInterfacePermissions,
    DescribeNetworkInterfacePermissionsResponse (DescribeNetworkInterfacePermissionsResponse'),
    newDescribeNetworkInterfacePermissionsResponse,

    -- ** GetVpnConnectionDeviceSampleConfiguration
    GetVpnConnectionDeviceSampleConfiguration (GetVpnConnectionDeviceSampleConfiguration'),
    newGetVpnConnectionDeviceSampleConfiguration,
    GetVpnConnectionDeviceSampleConfigurationResponse (GetVpnConnectionDeviceSampleConfigurationResponse'),
    newGetVpnConnectionDeviceSampleConfigurationResponse,

    -- ** DescribeVpcEndpointServicePermissions (Paginated)
    DescribeVpcEndpointServicePermissions (DescribeVpcEndpointServicePermissions'),
    newDescribeVpcEndpointServicePermissions,
    DescribeVpcEndpointServicePermissionsResponse (DescribeVpcEndpointServicePermissionsResponse'),
    newDescribeVpcEndpointServicePermissionsResponse,

    -- ** DescribeHostReservationOfferings (Paginated)
    DescribeHostReservationOfferings (DescribeHostReservationOfferings'),
    newDescribeHostReservationOfferings,
    DescribeHostReservationOfferingsResponse (DescribeHostReservationOfferingsResponse'),
    newDescribeHostReservationOfferingsResponse,

    -- ** DescribeVpcEndpointConnections (Paginated)
    DescribeVpcEndpointConnections (DescribeVpcEndpointConnections'),
    newDescribeVpcEndpointConnections,
    DescribeVpcEndpointConnectionsResponse (DescribeVpcEndpointConnectionsResponse'),
    newDescribeVpcEndpointConnectionsResponse,

    -- ** DescribeFpgaImageAttribute
    DescribeFpgaImageAttribute (DescribeFpgaImageAttribute'),
    newDescribeFpgaImageAttribute,
    DescribeFpgaImageAttributeResponse (DescribeFpgaImageAttributeResponse'),
    newDescribeFpgaImageAttributeResponse,

    -- ** EnableImageDeprecation
    EnableImageDeprecation (EnableImageDeprecation'),
    newEnableImageDeprecation,
    EnableImageDeprecationResponse (EnableImageDeprecationResponse'),
    newEnableImageDeprecationResponse,

    -- ** ResetImageAttribute
    ResetImageAttribute (ResetImageAttribute'),
    newResetImageAttribute,
    ResetImageAttributeResponse (ResetImageAttributeResponse'),
    newResetImageAttributeResponse,

    -- ** AdvertiseByoipCidr
    AdvertiseByoipCidr (AdvertiseByoipCidr'),
    newAdvertiseByoipCidr,
    AdvertiseByoipCidrResponse (AdvertiseByoipCidrResponse'),
    newAdvertiseByoipCidrResponse,

    -- ** DescribeTransitGatewayRouteTables (Paginated)
    DescribeTransitGatewayRouteTables (DescribeTransitGatewayRouteTables'),
    newDescribeTransitGatewayRouteTables,
    DescribeTransitGatewayRouteTablesResponse (DescribeTransitGatewayRouteTablesResponse'),
    newDescribeTransitGatewayRouteTablesResponse,

    -- ** ModifyTransitGatewayPrefixListReference
    ModifyTransitGatewayPrefixListReference (ModifyTransitGatewayPrefixListReference'),
    newModifyTransitGatewayPrefixListReference,
    ModifyTransitGatewayPrefixListReferenceResponse (ModifyTransitGatewayPrefixListReferenceResponse'),
    newModifyTransitGatewayPrefixListReferenceResponse,

    -- ** RegisterInstanceEventNotificationAttributes
    RegisterInstanceEventNotificationAttributes (RegisterInstanceEventNotificationAttributes'),
    newRegisterInstanceEventNotificationAttributes,
    RegisterInstanceEventNotificationAttributesResponse (RegisterInstanceEventNotificationAttributesResponse'),
    newRegisterInstanceEventNotificationAttributesResponse,

    -- ** DeleteManagedPrefixList
    DeleteManagedPrefixList (DeleteManagedPrefixList'),
    newDeleteManagedPrefixList,
    DeleteManagedPrefixListResponse (DeleteManagedPrefixListResponse'),
    newDeleteManagedPrefixListResponse,

    -- ** DescribeRegions
    DescribeRegions (DescribeRegions'),
    newDescribeRegions,
    DescribeRegionsResponse (DescribeRegionsResponse'),
    newDescribeRegionsResponse,

    -- ** RevokeSecurityGroupIngress
    RevokeSecurityGroupIngress (RevokeSecurityGroupIngress'),
    newRevokeSecurityGroupIngress,
    RevokeSecurityGroupIngressResponse (RevokeSecurityGroupIngressResponse'),
    newRevokeSecurityGroupIngressResponse,

    -- ** DeleteVpnConnectionRoute
    DeleteVpnConnectionRoute (DeleteVpnConnectionRoute'),
    newDeleteVpnConnectionRoute,
    DeleteVpnConnectionRouteResponse (DeleteVpnConnectionRouteResponse'),
    newDeleteVpnConnectionRouteResponse,

    -- ** DescribeNetworkAcls (Paginated)
    DescribeNetworkAcls (DescribeNetworkAcls'),
    newDescribeNetworkAcls,
    DescribeNetworkAclsResponse (DescribeNetworkAclsResponse'),
    newDescribeNetworkAclsResponse,

    -- ** DeleteDhcpOptions
    DeleteDhcpOptions (DeleteDhcpOptions'),
    newDeleteDhcpOptions,
    DeleteDhcpOptionsResponse (DeleteDhcpOptionsResponse'),
    newDeleteDhcpOptionsResponse,

    -- ** DescribeVpnGateways
    DescribeVpnGateways (DescribeVpnGateways'),
    newDescribeVpnGateways,
    DescribeVpnGatewaysResponse (DescribeVpnGatewaysResponse'),
    newDescribeVpnGatewaysResponse,

    -- ** RegisterTransitGatewayMulticastGroupMembers
    RegisterTransitGatewayMulticastGroupMembers (RegisterTransitGatewayMulticastGroupMembers'),
    newRegisterTransitGatewayMulticastGroupMembers,
    RegisterTransitGatewayMulticastGroupMembersResponse (RegisterTransitGatewayMulticastGroupMembersResponse'),
    newRegisterTransitGatewayMulticastGroupMembersResponse,

    -- ** DescribeHostReservations (Paginated)
    DescribeHostReservations (DescribeHostReservations'),
    newDescribeHostReservations,
    DescribeHostReservationsResponse (DescribeHostReservationsResponse'),
    newDescribeHostReservationsResponse,

    -- ** RejectVpcPeeringConnection
    RejectVpcPeeringConnection (RejectVpcPeeringConnection'),
    newRejectVpcPeeringConnection,
    RejectVpcPeeringConnectionResponse (RejectVpcPeeringConnectionResponse'),
    newRejectVpcPeeringConnectionResponse,

    -- ** CreateEgressOnlyInternetGateway
    CreateEgressOnlyInternetGateway (CreateEgressOnlyInternetGateway'),
    newCreateEgressOnlyInternetGateway,
    CreateEgressOnlyInternetGatewayResponse (CreateEgressOnlyInternetGatewayResponse'),
    newCreateEgressOnlyInternetGatewayResponse,

    -- ** DeleteSubnetCidrReservation
    DeleteSubnetCidrReservation (DeleteSubnetCidrReservation'),
    newDeleteSubnetCidrReservation,
    DeleteSubnetCidrReservationResponse (DeleteSubnetCidrReservationResponse'),
    newDeleteSubnetCidrReservationResponse,

    -- ** EnableVpcClassicLinkDnsSupport
    EnableVpcClassicLinkDnsSupport (EnableVpcClassicLinkDnsSupport'),
    newEnableVpcClassicLinkDnsSupport,
    EnableVpcClassicLinkDnsSupportResponse (EnableVpcClassicLinkDnsSupportResponse'),
    newEnableVpcClassicLinkDnsSupportResponse,

    -- ** AllocateAddress
    AllocateAddress (AllocateAddress'),
    newAllocateAddress,
    AllocateAddressResponse (AllocateAddressResponse'),
    newAllocateAddressResponse,

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

    -- ** ModifyDefaultCreditSpecification
    ModifyDefaultCreditSpecification (ModifyDefaultCreditSpecification'),
    newModifyDefaultCreditSpecification,
    ModifyDefaultCreditSpecificationResponse (ModifyDefaultCreditSpecificationResponse'),
    newModifyDefaultCreditSpecificationResponse,

    -- ** GetManagedPrefixListEntries (Paginated)
    GetManagedPrefixListEntries (GetManagedPrefixListEntries'),
    newGetManagedPrefixListEntries,
    GetManagedPrefixListEntriesResponse (GetManagedPrefixListEntriesResponse'),
    newGetManagedPrefixListEntriesResponse,

    -- ** DisableVpcClassicLinkDnsSupport
    DisableVpcClassicLinkDnsSupport (DisableVpcClassicLinkDnsSupport'),
    newDisableVpcClassicLinkDnsSupport,
    DisableVpcClassicLinkDnsSupportResponse (DisableVpcClassicLinkDnsSupportResponse'),
    newDisableVpcClassicLinkDnsSupportResponse,

    -- ** DisableImageDeprecation
    DisableImageDeprecation (DisableImageDeprecation'),
    newDisableImageDeprecation,
    DisableImageDeprecationResponse (DisableImageDeprecationResponse'),
    newDisableImageDeprecationResponse,

    -- ** ApplySecurityGroupsToClientVpnTargetNetwork
    ApplySecurityGroupsToClientVpnTargetNetwork (ApplySecurityGroupsToClientVpnTargetNetwork'),
    newApplySecurityGroupsToClientVpnTargetNetwork,
    ApplySecurityGroupsToClientVpnTargetNetworkResponse (ApplySecurityGroupsToClientVpnTargetNetworkResponse'),
    newApplySecurityGroupsToClientVpnTargetNetworkResponse,

    -- ** CreateLaunchTemplateVersion
    CreateLaunchTemplateVersion (CreateLaunchTemplateVersion'),
    newCreateLaunchTemplateVersion,
    CreateLaunchTemplateVersionResponse (CreateLaunchTemplateVersionResponse'),
    newCreateLaunchTemplateVersionResponse,

    -- ** CreateVpnConnectionRoute
    CreateVpnConnectionRoute (CreateVpnConnectionRoute'),
    newCreateVpnConnectionRoute,
    CreateVpnConnectionRouteResponse (CreateVpnConnectionRouteResponse'),
    newCreateVpnConnectionRouteResponse,

    -- ** DisassociateInstanceEventWindow
    DisassociateInstanceEventWindow (DisassociateInstanceEventWindow'),
    newDisassociateInstanceEventWindow,
    DisassociateInstanceEventWindowResponse (DisassociateInstanceEventWindowResponse'),
    newDisassociateInstanceEventWindowResponse,

    -- ** DescribeConversionTasks
    DescribeConversionTasks (DescribeConversionTasks'),
    newDescribeConversionTasks,
    DescribeConversionTasksResponse (DescribeConversionTasksResponse'),
    newDescribeConversionTasksResponse,

    -- ** DeleteTrafficMirrorSession
    DeleteTrafficMirrorSession (DeleteTrafficMirrorSession'),
    newDeleteTrafficMirrorSession,
    DeleteTrafficMirrorSessionResponse (DeleteTrafficMirrorSessionResponse'),
    newDeleteTrafficMirrorSessionResponse,

    -- ** CreateTransitGateway
    CreateTransitGateway (CreateTransitGateway'),
    newCreateTransitGateway,
    CreateTransitGatewayResponse (CreateTransitGatewayResponse'),
    newCreateTransitGatewayResponse,

    -- ** CreateSnapshots
    CreateSnapshots (CreateSnapshots'),
    newCreateSnapshots,
    CreateSnapshotsResponse (CreateSnapshotsResponse'),
    newCreateSnapshotsResponse,

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

    -- ** DescribeKeyPairs
    DescribeKeyPairs (DescribeKeyPairs'),
    newDescribeKeyPairs,
    DescribeKeyPairsResponse (DescribeKeyPairsResponse'),
    newDescribeKeyPairsResponse,

    -- ** CreateFpgaImage
    CreateFpgaImage (CreateFpgaImage'),
    newCreateFpgaImage,
    CreateFpgaImageResponse (CreateFpgaImageResponse'),
    newCreateFpgaImageResponse,

    -- ** DescribeClassicLinkInstances (Paginated)
    DescribeClassicLinkInstances (DescribeClassicLinkInstances'),
    newDescribeClassicLinkInstances,
    DescribeClassicLinkInstancesResponse (DescribeClassicLinkInstancesResponse'),
    newDescribeClassicLinkInstancesResponse,

    -- ** DeleteTrafficMirrorFilterRule
    DeleteTrafficMirrorFilterRule (DeleteTrafficMirrorFilterRule'),
    newDeleteTrafficMirrorFilterRule,
    DeleteTrafficMirrorFilterRuleResponse (DeleteTrafficMirrorFilterRuleResponse'),
    newDeleteTrafficMirrorFilterRuleResponse,

    -- ** TerminateInstances
    TerminateInstances (TerminateInstances'),
    newTerminateInstances,
    TerminateInstancesResponse (TerminateInstancesResponse'),
    newTerminateInstancesResponse,

    -- ** ModifyFpgaImageAttribute
    ModifyFpgaImageAttribute (ModifyFpgaImageAttribute'),
    newModifyFpgaImageAttribute,
    ModifyFpgaImageAttributeResponse (ModifyFpgaImageAttributeResponse'),
    newModifyFpgaImageAttributeResponse,

    -- ** WithdrawByoipCidr
    WithdrawByoipCidr (WithdrawByoipCidr'),
    newWithdrawByoipCidr,
    WithdrawByoipCidrResponse (WithdrawByoipCidrResponse'),
    newWithdrawByoipCidrResponse,

    -- ** AttachVpnGateway
    AttachVpnGateway (AttachVpnGateway'),
    newAttachVpnGateway,
    AttachVpnGatewayResponse (AttachVpnGatewayResponse'),
    newAttachVpnGatewayResponse,

    -- ** AcceptTransitGatewayVpcAttachment
    AcceptTransitGatewayVpcAttachment (AcceptTransitGatewayVpcAttachment'),
    newAcceptTransitGatewayVpcAttachment,
    AcceptTransitGatewayVpcAttachmentResponse (AcceptTransitGatewayVpcAttachmentResponse'),
    newAcceptTransitGatewayVpcAttachmentResponse,

    -- ** DeleteSpotDatafeedSubscription
    DeleteSpotDatafeedSubscription (DeleteSpotDatafeedSubscription'),
    newDeleteSpotDatafeedSubscription,
    DeleteSpotDatafeedSubscriptionResponse (DeleteSpotDatafeedSubscriptionResponse'),
    newDeleteSpotDatafeedSubscriptionResponse,

    -- ** DescribeExportImageTasks (Paginated)
    DescribeExportImageTasks (DescribeExportImageTasks'),
    newDescribeExportImageTasks,
    DescribeExportImageTasksResponse (DescribeExportImageTasksResponse'),
    newDescribeExportImageTasksResponse,

    -- ** CreateCarrierGateway
    CreateCarrierGateway (CreateCarrierGateway'),
    newCreateCarrierGateway,
    CreateCarrierGatewayResponse (CreateCarrierGatewayResponse'),
    newCreateCarrierGatewayResponse,

    -- ** AttachInternetGateway
    AttachInternetGateway (AttachInternetGateway'),
    newAttachInternetGateway,
    AttachInternetGatewayResponse (AttachInternetGatewayResponse'),
    newAttachInternetGatewayResponse,

    -- ** DescribeClientVpnTargetNetworks (Paginated)
    DescribeClientVpnTargetNetworks (DescribeClientVpnTargetNetworks'),
    newDescribeClientVpnTargetNetworks,
    DescribeClientVpnTargetNetworksResponse (DescribeClientVpnTargetNetworksResponse'),
    newDescribeClientVpnTargetNetworksResponse,

    -- ** CreateTrafficMirrorFilter
    CreateTrafficMirrorFilter (CreateTrafficMirrorFilter'),
    newCreateTrafficMirrorFilter,
    CreateTrafficMirrorFilterResponse (CreateTrafficMirrorFilterResponse'),
    newCreateTrafficMirrorFilterResponse,

    -- ** DeleteNetworkInsightsAnalysis
    DeleteNetworkInsightsAnalysis (DeleteNetworkInsightsAnalysis'),
    newDeleteNetworkInsightsAnalysis,
    DeleteNetworkInsightsAnalysisResponse (DeleteNetworkInsightsAnalysisResponse'),
    newDeleteNetworkInsightsAnalysisResponse,

    -- ** DescribeIamInstanceProfileAssociations (Paginated)
    DescribeIamInstanceProfileAssociations (DescribeIamInstanceProfileAssociations'),
    newDescribeIamInstanceProfileAssociations,
    DescribeIamInstanceProfileAssociationsResponse (DescribeIamInstanceProfileAssociationsResponse'),
    newDescribeIamInstanceProfileAssociationsResponse,

    -- ** ImportKeyPair
    ImportKeyPair (ImportKeyPair'),
    newImportKeyPair,
    ImportKeyPairResponse (ImportKeyPairResponse'),
    newImportKeyPairResponse,

    -- ** EnableVpcClassicLink
    EnableVpcClassicLink (EnableVpcClassicLink'),
    newEnableVpcClassicLink,
    EnableVpcClassicLinkResponse (EnableVpcClassicLinkResponse'),
    newEnableVpcClassicLinkResponse,

    -- ** DescribeNetworkInsightsAnalyses (Paginated)
    DescribeNetworkInsightsAnalyses (DescribeNetworkInsightsAnalyses'),
    newDescribeNetworkInsightsAnalyses,
    DescribeNetworkInsightsAnalysesResponse (DescribeNetworkInsightsAnalysesResponse'),
    newDescribeNetworkInsightsAnalysesResponse,

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

    -- ** CreateCapacityReservation
    CreateCapacityReservation (CreateCapacityReservation'),
    newCreateCapacityReservation,
    CreateCapacityReservationResponse (CreateCapacityReservationResponse'),
    newCreateCapacityReservationResponse,

    -- ** DeleteInstanceEventWindow
    DeleteInstanceEventWindow (DeleteInstanceEventWindow'),
    newDeleteInstanceEventWindow,
    DeleteInstanceEventWindowResponse (DeleteInstanceEventWindowResponse'),
    newDeleteInstanceEventWindowResponse,

    -- ** ModifyEbsDefaultKmsKeyId
    ModifyEbsDefaultKmsKeyId (ModifyEbsDefaultKmsKeyId'),
    newModifyEbsDefaultKmsKeyId,
    ModifyEbsDefaultKmsKeyIdResponse (ModifyEbsDefaultKmsKeyIdResponse'),
    newModifyEbsDefaultKmsKeyIdResponse,

    -- ** ProvisionByoipCidr
    ProvisionByoipCidr (ProvisionByoipCidr'),
    newProvisionByoipCidr,
    ProvisionByoipCidrResponse (ProvisionByoipCidrResponse'),
    newProvisionByoipCidrResponse,

    -- ** DeleteTransitGatewayConnect
    DeleteTransitGatewayConnect (DeleteTransitGatewayConnect'),
    newDeleteTransitGatewayConnect,
    DeleteTransitGatewayConnectResponse (DeleteTransitGatewayConnectResponse'),
    newDeleteTransitGatewayConnectResponse,

    -- ** ModifyVpnTunnelOptions
    ModifyVpnTunnelOptions (ModifyVpnTunnelOptions'),
    newModifyVpnTunnelOptions,
    ModifyVpnTunnelOptionsResponse (ModifyVpnTunnelOptionsResponse'),
    newModifyVpnTunnelOptionsResponse,

    -- ** CreateTransitGatewayPeeringAttachment
    CreateTransitGatewayPeeringAttachment (CreateTransitGatewayPeeringAttachment'),
    newCreateTransitGatewayPeeringAttachment,
    CreateTransitGatewayPeeringAttachmentResponse (CreateTransitGatewayPeeringAttachmentResponse'),
    newCreateTransitGatewayPeeringAttachmentResponse,

    -- ** CreateCustomerGateway
    CreateCustomerGateway (CreateCustomerGateway'),
    newCreateCustomerGateway,
    CreateCustomerGatewayResponse (CreateCustomerGatewayResponse'),
    newCreateCustomerGatewayResponse,

    -- ** ModifyVolume
    ModifyVolume (ModifyVolume'),
    newModifyVolume,
    ModifyVolumeResponse (ModifyVolumeResponse'),
    newModifyVolumeResponse,

    -- ** ModifyInstancePlacement
    ModifyInstancePlacement (ModifyInstancePlacement'),
    newModifyInstancePlacement,
    ModifyInstancePlacementResponse (ModifyInstancePlacementResponse'),
    newModifyInstancePlacementResponse,

    -- ** CreateSnapshot
    CreateSnapshot (CreateSnapshot'),
    newCreateSnapshot,
    Snapshot (Snapshot'),
    newSnapshot,

    -- ** DescribeInstanceAttribute
    DescribeInstanceAttribute (DescribeInstanceAttribute'),
    newDescribeInstanceAttribute,
    DescribeInstanceAttributeResponse (DescribeInstanceAttributeResponse'),
    newDescribeInstanceAttributeResponse,

    -- ** CreateReservedInstancesListing
    CreateReservedInstancesListing (CreateReservedInstancesListing'),
    newCreateReservedInstancesListing,
    CreateReservedInstancesListingResponse (CreateReservedInstancesListingResponse'),
    newCreateReservedInstancesListingResponse,

    -- ** DeleteSecurityGroup
    DeleteSecurityGroup (DeleteSecurityGroup'),
    newDeleteSecurityGroup,
    DeleteSecurityGroupResponse (DeleteSecurityGroupResponse'),
    newDeleteSecurityGroupResponse,

    -- ** PurchaseScheduledInstances
    PurchaseScheduledInstances (PurchaseScheduledInstances'),
    newPurchaseScheduledInstances,
    PurchaseScheduledInstancesResponse (PurchaseScheduledInstancesResponse'),
    newPurchaseScheduledInstancesResponse,

    -- ** DescribePublicIpv4Pools (Paginated)
    DescribePublicIpv4Pools (DescribePublicIpv4Pools'),
    newDescribePublicIpv4Pools,
    DescribePublicIpv4PoolsResponse (DescribePublicIpv4PoolsResponse'),
    newDescribePublicIpv4PoolsResponse,

    -- ** DescribeLocalGatewayRouteTableVpcAssociations (Paginated)
    DescribeLocalGatewayRouteTableVpcAssociations (DescribeLocalGatewayRouteTableVpcAssociations'),
    newDescribeLocalGatewayRouteTableVpcAssociations,
    DescribeLocalGatewayRouteTableVpcAssociationsResponse (DescribeLocalGatewayRouteTableVpcAssociationsResponse'),
    newDescribeLocalGatewayRouteTableVpcAssociationsResponse,

    -- ** AuthorizeClientVpnIngress
    AuthorizeClientVpnIngress (AuthorizeClientVpnIngress'),
    newAuthorizeClientVpnIngress,
    AuthorizeClientVpnIngressResponse (AuthorizeClientVpnIngressResponse'),
    newAuthorizeClientVpnIngressResponse,

    -- ** CreateVpcPeeringConnection
    CreateVpcPeeringConnection (CreateVpcPeeringConnection'),
    newCreateVpcPeeringConnection,
    CreateVpcPeeringConnectionResponse (CreateVpcPeeringConnectionResponse'),
    newCreateVpcPeeringConnectionResponse,

    -- ** CreateInstanceEventWindow
    CreateInstanceEventWindow (CreateInstanceEventWindow'),
    newCreateInstanceEventWindow,
    CreateInstanceEventWindowResponse (CreateInstanceEventWindowResponse'),
    newCreateInstanceEventWindowResponse,

    -- ** CreateSecurityGroup
    CreateSecurityGroup (CreateSecurityGroup'),
    newCreateSecurityGroup,
    CreateSecurityGroupResponse (CreateSecurityGroupResponse'),
    newCreateSecurityGroupResponse,

    -- ** DescribeInternetGateways (Paginated)
    DescribeInternetGateways (DescribeInternetGateways'),
    newDescribeInternetGateways,
    DescribeInternetGatewaysResponse (DescribeInternetGatewaysResponse'),
    newDescribeInternetGatewaysResponse,

    -- ** ModifyLaunchTemplate
    ModifyLaunchTemplate (ModifyLaunchTemplate'),
    newModifyLaunchTemplate,
    ModifyLaunchTemplateResponse (ModifyLaunchTemplateResponse'),
    newModifyLaunchTemplateResponse,

    -- ** ModifyInstanceAttribute
    ModifyInstanceAttribute (ModifyInstanceAttribute'),
    newModifyInstanceAttribute,
    ModifyInstanceAttributeResponse (ModifyInstanceAttributeResponse'),
    newModifyInstanceAttributeResponse,

    -- ** ResetEbsDefaultKmsKeyId
    ResetEbsDefaultKmsKeyId (ResetEbsDefaultKmsKeyId'),
    newResetEbsDefaultKmsKeyId,
    ResetEbsDefaultKmsKeyIdResponse (ResetEbsDefaultKmsKeyIdResponse'),
    newResetEbsDefaultKmsKeyIdResponse,

    -- ** GetEbsEncryptionByDefault
    GetEbsEncryptionByDefault (GetEbsEncryptionByDefault'),
    newGetEbsEncryptionByDefault,
    GetEbsEncryptionByDefaultResponse (GetEbsEncryptionByDefaultResponse'),
    newGetEbsEncryptionByDefaultResponse,

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

    -- ** AssociateInstanceEventWindow
    AssociateInstanceEventWindow (AssociateInstanceEventWindow'),
    newAssociateInstanceEventWindow,
    AssociateInstanceEventWindowResponse (AssociateInstanceEventWindowResponse'),
    newAssociateInstanceEventWindowResponse,

    -- ** ReplaceRoute
    ReplaceRoute (ReplaceRoute'),
    newReplaceRoute,
    ReplaceRouteResponse (ReplaceRouteResponse'),
    newReplaceRouteResponse,

    -- ** CreateTags
    CreateTags (CreateTags'),
    newCreateTags,
    CreateTagsResponse (CreateTagsResponse'),
    newCreateTagsResponse,

    -- ** CreateTransitGatewayConnect
    CreateTransitGatewayConnect (CreateTransitGatewayConnect'),
    newCreateTransitGatewayConnect,
    CreateTransitGatewayConnectResponse (CreateTransitGatewayConnectResponse'),
    newCreateTransitGatewayConnectResponse,

    -- ** DeleteLaunchTemplateVersions
    DeleteLaunchTemplateVersions (DeleteLaunchTemplateVersions'),
    newDeleteLaunchTemplateVersions,
    DeleteLaunchTemplateVersionsResponse (DeleteLaunchTemplateVersionsResponse'),
    newDeleteLaunchTemplateVersionsResponse,

    -- ** GetDefaultCreditSpecification
    GetDefaultCreditSpecification (GetDefaultCreditSpecification'),
    newGetDefaultCreditSpecification,
    GetDefaultCreditSpecificationResponse (GetDefaultCreditSpecificationResponse'),
    newGetDefaultCreditSpecificationResponse,

    -- ** ResetSnapshotAttribute
    ResetSnapshotAttribute (ResetSnapshotAttribute'),
    newResetSnapshotAttribute,
    ResetSnapshotAttributeResponse (ResetSnapshotAttributeResponse'),
    newResetSnapshotAttributeResponse,

    -- ** DeleteTransitGatewayPeeringAttachment
    DeleteTransitGatewayPeeringAttachment (DeleteTransitGatewayPeeringAttachment'),
    newDeleteTransitGatewayPeeringAttachment,
    DeleteTransitGatewayPeeringAttachmentResponse (DeleteTransitGatewayPeeringAttachmentResponse'),
    newDeleteTransitGatewayPeeringAttachmentResponse,

    -- ** CreateRoute
    CreateRoute (CreateRoute'),
    newCreateRoute,
    CreateRouteResponse (CreateRouteResponse'),
    newCreateRouteResponse,

    -- ** BundleInstance
    BundleInstance (BundleInstance'),
    newBundleInstance,
    BundleInstanceResponse (BundleInstanceResponse'),
    newBundleInstanceResponse,

    -- ** ReplaceNetworkAclAssociation
    ReplaceNetworkAclAssociation (ReplaceNetworkAclAssociation'),
    newReplaceNetworkAclAssociation,
    ReplaceNetworkAclAssociationResponse (ReplaceNetworkAclAssociationResponse'),
    newReplaceNetworkAclAssociationResponse,

    -- ** DeleteTrafficMirrorFilter
    DeleteTrafficMirrorFilter (DeleteTrafficMirrorFilter'),
    newDeleteTrafficMirrorFilter,
    DeleteTrafficMirrorFilterResponse (DeleteTrafficMirrorFilterResponse'),
    newDeleteTrafficMirrorFilterResponse,

    -- ** DescribeIdentityIdFormat
    DescribeIdentityIdFormat (DescribeIdentityIdFormat'),
    newDescribeIdentityIdFormat,
    DescribeIdentityIdFormatResponse (DescribeIdentityIdFormatResponse'),
    newDescribeIdentityIdFormatResponse,

    -- ** ReplaceTransitGatewayRoute
    ReplaceTransitGatewayRoute (ReplaceTransitGatewayRoute'),
    newReplaceTransitGatewayRoute,
    ReplaceTransitGatewayRouteResponse (ReplaceTransitGatewayRouteResponse'),
    newReplaceTransitGatewayRouteResponse,

    -- ** GetCoipPoolUsage
    GetCoipPoolUsage (GetCoipPoolUsage'),
    newGetCoipPoolUsage,
    GetCoipPoolUsageResponse (GetCoipPoolUsageResponse'),
    newGetCoipPoolUsageResponse,

    -- ** DeleteTransitGatewayVpcAttachment
    DeleteTransitGatewayVpcAttachment (DeleteTransitGatewayVpcAttachment'),
    newDeleteTransitGatewayVpcAttachment,
    DeleteTransitGatewayVpcAttachmentResponse (DeleteTransitGatewayVpcAttachmentResponse'),
    newDeleteTransitGatewayVpcAttachmentResponse,

    -- ** CreatePlacementGroup
    CreatePlacementGroup (CreatePlacementGroup'),
    newCreatePlacementGroup,
    CreatePlacementGroupResponse (CreatePlacementGroupResponse'),
    newCreatePlacementGroupResponse,

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

    -- ** CancelSpotFleetRequests
    CancelSpotFleetRequests (CancelSpotFleetRequests'),
    newCancelSpotFleetRequests,
    CancelSpotFleetRequestsResponse (CancelSpotFleetRequestsResponse'),
    newCancelSpotFleetRequestsResponse,

    -- ** DescribeClientVpnAuthorizationRules (Paginated)
    DescribeClientVpnAuthorizationRules (DescribeClientVpnAuthorizationRules'),
    newDescribeClientVpnAuthorizationRules,
    DescribeClientVpnAuthorizationRulesResponse (DescribeClientVpnAuthorizationRulesResponse'),
    newDescribeClientVpnAuthorizationRulesResponse,

    -- ** DeleteSubnet
    DeleteSubnet (DeleteSubnet'),
    newDeleteSubnet,
    DeleteSubnetResponse (DeleteSubnetResponse'),
    newDeleteSubnetResponse,

    -- ** StopInstances
    StopInstances (StopInstances'),
    newStopInstances,
    StopInstancesResponse (StopInstancesResponse'),
    newStopInstancesResponse,

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

    -- ** CreateDefaultVpc
    CreateDefaultVpc (CreateDefaultVpc'),
    newCreateDefaultVpc,
    CreateDefaultVpcResponse (CreateDefaultVpcResponse'),
    newCreateDefaultVpcResponse,

    -- ** CreateTransitGatewayPrefixListReference
    CreateTransitGatewayPrefixListReference (CreateTransitGatewayPrefixListReference'),
    newCreateTransitGatewayPrefixListReference,
    CreateTransitGatewayPrefixListReferenceResponse (CreateTransitGatewayPrefixListReferenceResponse'),
    newCreateTransitGatewayPrefixListReferenceResponse,

    -- ** DescribeDhcpOptions (Paginated)
    DescribeDhcpOptions (DescribeDhcpOptions'),
    newDescribeDhcpOptions,
    DescribeDhcpOptionsResponse (DescribeDhcpOptionsResponse'),
    newDescribeDhcpOptionsResponse,

    -- ** CreateTransitGatewayRoute
    CreateTransitGatewayRoute (CreateTransitGatewayRoute'),
    newCreateTransitGatewayRoute,
    CreateTransitGatewayRouteResponse (CreateTransitGatewayRouteResponse'),
    newCreateTransitGatewayRouteResponse,

    -- ** ResetNetworkInterfaceAttribute
    ResetNetworkInterfaceAttribute (ResetNetworkInterfaceAttribute'),
    newResetNetworkInterfaceAttribute,
    ResetNetworkInterfaceAttributeResponse (ResetNetworkInterfaceAttributeResponse'),
    newResetNetworkInterfaceAttributeResponse,

    -- ** DeleteTransitGatewayMulticastDomain
    DeleteTransitGatewayMulticastDomain (DeleteTransitGatewayMulticastDomain'),
    newDeleteTransitGatewayMulticastDomain,
    DeleteTransitGatewayMulticastDomainResponse (DeleteTransitGatewayMulticastDomainResponse'),
    newDeleteTransitGatewayMulticastDomainResponse,

    -- ** DeleteNetworkInterface
    DeleteNetworkInterface (DeleteNetworkInterface'),
    newDeleteNetworkInterface,
    DeleteNetworkInterfaceResponse (DeleteNetworkInterfaceResponse'),
    newDeleteNetworkInterfaceResponse,

    -- ** DisassociateVpcCidrBlock
    DisassociateVpcCidrBlock (DisassociateVpcCidrBlock'),
    newDisassociateVpcCidrBlock,
    DisassociateVpcCidrBlockResponse (DisassociateVpcCidrBlockResponse'),
    newDisassociateVpcCidrBlockResponse,

    -- ** ReplaceRouteTableAssociation
    ReplaceRouteTableAssociation (ReplaceRouteTableAssociation'),
    newReplaceRouteTableAssociation,
    ReplaceRouteTableAssociationResponse (ReplaceRouteTableAssociationResponse'),
    newReplaceRouteTableAssociationResponse,

    -- ** CreateNetworkInsightsPath
    CreateNetworkInsightsPath (CreateNetworkInsightsPath'),
    newCreateNetworkInsightsPath,
    CreateNetworkInsightsPathResponse (CreateNetworkInsightsPathResponse'),
    newCreateNetworkInsightsPathResponse,

    -- ** DeregisterTransitGatewayMulticastGroupSources
    DeregisterTransitGatewayMulticastGroupSources (DeregisterTransitGatewayMulticastGroupSources'),
    newDeregisterTransitGatewayMulticastGroupSources,
    DeregisterTransitGatewayMulticastGroupSourcesResponse (DeregisterTransitGatewayMulticastGroupSourcesResponse'),
    newDeregisterTransitGatewayMulticastGroupSourcesResponse,

    -- ** DescribeCustomerGateways
    DescribeCustomerGateways (DescribeCustomerGateways'),
    newDescribeCustomerGateways,
    DescribeCustomerGatewaysResponse (DescribeCustomerGatewaysResponse'),
    newDescribeCustomerGatewaysResponse,

    -- ** DescribeCapacityReservations (Paginated)
    DescribeCapacityReservations (DescribeCapacityReservations'),
    newDescribeCapacityReservations,
    DescribeCapacityReservationsResponse (DescribeCapacityReservationsResponse'),
    newDescribeCapacityReservationsResponse,

    -- ** ModifyTrafficMirrorSession
    ModifyTrafficMirrorSession (ModifyTrafficMirrorSession'),
    newModifyTrafficMirrorSession,
    ModifyTrafficMirrorSessionResponse (ModifyTrafficMirrorSessionResponse'),
    newModifyTrafficMirrorSessionResponse,

    -- ** DisassociateTransitGatewayRouteTable
    DisassociateTransitGatewayRouteTable (DisassociateTransitGatewayRouteTable'),
    newDisassociateTransitGatewayRouteTable,
    DisassociateTransitGatewayRouteTableResponse (DisassociateTransitGatewayRouteTableResponse'),
    newDisassociateTransitGatewayRouteTableResponse,

    -- ** StartInstances
    StartInstances (StartInstances'),
    newStartInstances,
    StartInstancesResponse (StartInstancesResponse'),
    newStartInstancesResponse,

    -- ** DescribeTransitGatewayPeeringAttachments (Paginated)
    DescribeTransitGatewayPeeringAttachments (DescribeTransitGatewayPeeringAttachments'),
    newDescribeTransitGatewayPeeringAttachments,
    DescribeTransitGatewayPeeringAttachmentsResponse (DescribeTransitGatewayPeeringAttachmentsResponse'),
    newDescribeTransitGatewayPeeringAttachmentsResponse,

    -- ** ImportImage
    ImportImage (ImportImage'),
    newImportImage,
    ImportImageResponse (ImportImageResponse'),
    newImportImageResponse,

    -- ** DescribeTrafficMirrorFilters (Paginated)
    DescribeTrafficMirrorFilters (DescribeTrafficMirrorFilters'),
    newDescribeTrafficMirrorFilters,
    DescribeTrafficMirrorFiltersResponse (DescribeTrafficMirrorFiltersResponse'),
    newDescribeTrafficMirrorFiltersResponse,

    -- ** AssociateAddress
    AssociateAddress (AssociateAddress'),
    newAssociateAddress,
    AssociateAddressResponse (AssociateAddressResponse'),
    newAssociateAddressResponse,

    -- ** RunScheduledInstances
    RunScheduledInstances (RunScheduledInstances'),
    newRunScheduledInstances,
    RunScheduledInstancesResponse (RunScheduledInstancesResponse'),
    newRunScheduledInstancesResponse,

    -- ** CopyFpgaImage
    CopyFpgaImage (CopyFpgaImage'),
    newCopyFpgaImage,
    CopyFpgaImageResponse (CopyFpgaImageResponse'),
    newCopyFpgaImageResponse,

    -- ** DeleteNatGateway
    DeleteNatGateway (DeleteNatGateway'),
    newDeleteNatGateway,
    DeleteNatGatewayResponse (DeleteNatGatewayResponse'),
    newDeleteNatGatewayResponse,

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
