{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Amazon Elastic Compute Cloud__
--
-- Amazon Elastic Compute Cloud (Amazon EC2) provides secure and resizable computing capacity in the AWS cloud. Using Amazon EC2 eliminates the need to invest in hardware up front, so you can develop and deploy applications faster.
-- To learn more, see the following resources:
--
--     * Amazon EC2: <http://aws.amazon.com/ec2 AmazonEC2 product page> , <http://aws.amazon.com/documentation/ec2 Amazon EC2 documentation>
--
--
--     * Amazon EBS: <http://aws.amazon.com/ebs Amazon EBS product page> , <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/AmazonEBS.html Amazon EBS documentation>
--
--
--     * Amazon VPC: <http://aws.amazon.com/vpc Amazon VPC product page> , <http://aws.amazon.com/documentation/vpc Amazon VPC documentation>
--
--
--     * AWS VPN: <http://aws.amazon.com/vpn AWS VPN product page> , <http://aws.amazon.com/documentation/vpn AWS VPN documentation>
module Network.AWS.EC2
  ( -- * Service configuration
    ec2Service,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- ** InstanceTerminated
    mkInstanceTerminated,

    -- ** VolumeInUse
    mkVolumeInUse,

    -- ** ImageExists
    mkImageExists,

    -- ** NatGatewayAvailable
    mkNatGatewayAvailable,

    -- ** SubnetAvailable
    mkSubnetAvailable,

    -- ** NetworkInterfaceAvailable
    mkNetworkInterfaceAvailable,

    -- ** KeyPairExists
    mkKeyPairExists,

    -- ** SystemStatusOK
    mkSystemStatusOK,

    -- ** CustomerGatewayAvailable
    mkCustomerGatewayAvailable,

    -- ** ConversionTaskCompleted
    mkConversionTaskCompleted,

    -- ** InstanceStopped
    mkInstanceStopped,

    -- ** ConversionTaskDeleted
    mkConversionTaskDeleted,

    -- ** PasswordDataAvailable
    mkPasswordDataAvailable,

    -- ** InstanceRunning
    mkInstanceRunning,

    -- ** SecurityGroupExists
    mkSecurityGroupExists,

    -- ** SpotInstanceRequestFulfilled
    mkSpotInstanceRequestFulfilled,

    -- ** VPCAvailable
    mkVPCAvailable,

    -- ** ExportTaskCompleted
    mkExportTaskCompleted,

    -- ** VPCPeeringConnectionDeleted
    mkVPCPeeringConnectionDeleted,

    -- ** VPNConnectionAvailable
    mkVPNConnectionAvailable,

    -- ** ExportTaskCancelled
    mkExportTaskCancelled,

    -- ** VolumeDeleted
    mkVolumeDeleted,

    -- ** VPCExists
    mkVPCExists,

    -- ** BundleTaskComplete
    mkBundleTaskComplete,

    -- ** VPNConnectionDeleted
    mkVPNConnectionDeleted,

    -- ** ConversionTaskCancelled
    mkConversionTaskCancelled,

    -- ** ImageAvailable
    mkImageAvailable,

    -- ** VPCPeeringConnectionExists
    mkVPCPeeringConnectionExists,

    -- ** SnapshotCompleted
    mkSnapshotCompleted,

    -- ** InstanceExists
    mkInstanceExists,

    -- ** InstanceStatusOK
    mkInstanceStatusOK,

    -- ** VolumeAvailable
    mkVolumeAvailable,

    -- * Operations
    -- $operations

    -- ** ModifyCapacityReservation
    module Network.AWS.EC2.ModifyCapacityReservation,

    -- ** GetAssociatedIPv6PoolCidrs (Paginated)
    module Network.AWS.EC2.GetAssociatedIPv6PoolCidrs,

    -- ** ImportInstance
    module Network.AWS.EC2.ImportInstance,

    -- ** RevokeSecurityGroupEgress
    module Network.AWS.EC2.RevokeSecurityGroupEgress,

    -- ** CreateNetworkInterfacePermission
    module Network.AWS.EC2.CreateNetworkInterfacePermission,

    -- ** SendDiagnosticInterrupt
    module Network.AWS.EC2.SendDiagnosticInterrupt,

    -- ** DeleteLaunchTemplate
    module Network.AWS.EC2.DeleteLaunchTemplate,

    -- ** RejectVPCEndpointConnections
    module Network.AWS.EC2.RejectVPCEndpointConnections,

    -- ** CreateVPNGateway
    module Network.AWS.EC2.CreateVPNGateway,

    -- ** CreateNetworkACL
    module Network.AWS.EC2.CreateNetworkACL,

    -- ** DeleteKeyPair
    module Network.AWS.EC2.DeleteKeyPair,

    -- ** DescribeSecurityGroupReferences
    module Network.AWS.EC2.DescribeSecurityGroupReferences,

    -- ** DeleteFleets
    module Network.AWS.EC2.DeleteFleets,

    -- ** DescribeTags (Paginated)
    module Network.AWS.EC2.DescribeTags,

    -- ** CreateTransitGatewayRouteTable
    module Network.AWS.EC2.CreateTransitGatewayRouteTable,

    -- ** ModifyInstanceMetadataOptions
    module Network.AWS.EC2.ModifyInstanceMetadataOptions,

    -- ** UpdateSecurityGroupRuleDescriptionsIngress
    module Network.AWS.EC2.UpdateSecurityGroupRuleDescriptionsIngress,

    -- ** DisassociateSubnetCidrBlock
    module Network.AWS.EC2.DisassociateSubnetCidrBlock,

    -- ** DetachNetworkInterface
    module Network.AWS.EC2.DetachNetworkInterface,

    -- ** DetachInternetGateway
    module Network.AWS.EC2.DetachInternetGateway,

    -- ** DeleteVPCEndpoints
    module Network.AWS.EC2.DeleteVPCEndpoints,

    -- ** DescribeClientVPNEndpoints (Paginated)
    module Network.AWS.EC2.DescribeClientVPNEndpoints,

    -- ** DeleteFlowLogs
    module Network.AWS.EC2.DeleteFlowLogs,

    -- ** DescribeVPCClassicLink
    module Network.AWS.EC2.DescribeVPCClassicLink,

    -- ** GetAssociatedEnclaveCertificateIAMRoles
    module Network.AWS.EC2.GetAssociatedEnclaveCertificateIAMRoles,

    -- ** AssociateTransitGatewayMulticastDomain
    module Network.AWS.EC2.AssociateTransitGatewayMulticastDomain,

    -- ** ModifySubnetAttribute
    module Network.AWS.EC2.ModifySubnetAttribute,

    -- ** DetachVolume
    module Network.AWS.EC2.DetachVolume,

    -- ** DescribeInstanceCreditSpecifications (Paginated)
    module Network.AWS.EC2.DescribeInstanceCreditSpecifications,

    -- ** CancelBundleTask
    module Network.AWS.EC2.CancelBundleTask,

    -- ** DescribeByoipCidrs (Paginated)
    module Network.AWS.EC2.DescribeByoipCidrs,

    -- ** AcceptReservedInstancesExchangeQuote
    module Network.AWS.EC2.AcceptReservedInstancesExchangeQuote,

    -- ** ReleaseAddress
    module Network.AWS.EC2.ReleaseAddress,

    -- ** DescribeInstanceTypeOfferings (Paginated)
    module Network.AWS.EC2.DescribeInstanceTypeOfferings,

    -- ** CreateInternetGateway
    module Network.AWS.EC2.CreateInternetGateway,

    -- ** DeleteVPNConnection
    module Network.AWS.EC2.DeleteVPNConnection,

    -- ** DescribeBundleTasks
    module Network.AWS.EC2.DescribeBundleTasks,

    -- ** AuthorizeSecurityGroupEgress
    module Network.AWS.EC2.AuthorizeSecurityGroupEgress,

    -- ** EnableTransitGatewayRouteTablePropagation
    module Network.AWS.EC2.EnableTransitGatewayRouteTablePropagation,

    -- ** DeregisterImage
    module Network.AWS.EC2.DeregisterImage,

    -- ** DeleteVPCEndpointConnectionNotifications
    module Network.AWS.EC2.DeleteVPCEndpointConnectionNotifications,

    -- ** DescribeCoipPools (Paginated)
    module Network.AWS.EC2.DescribeCoipPools,

    -- ** GetTransitGatewayMulticastDomainAssociations (Paginated)
    module Network.AWS.EC2.GetTransitGatewayMulticastDomainAssociations,

    -- ** DeleteLocalGatewayRouteTableVPCAssociation
    module Network.AWS.EC2.DeleteLocalGatewayRouteTableVPCAssociation,

    -- ** ModifyNetworkInterfaceAttribute
    module Network.AWS.EC2.ModifyNetworkInterfaceAttribute,

    -- ** ModifyVPCTenancy
    module Network.AWS.EC2.ModifyVPCTenancy,

    -- ** DescribeInstanceTypes (Paginated)
    module Network.AWS.EC2.DescribeInstanceTypes,

    -- ** DescribeClientVPNAuthorizationRules (Paginated)
    module Network.AWS.EC2.DescribeClientVPNAuthorizationRules,

    -- ** DeleteTransitGatewayVPCAttachment
    module Network.AWS.EC2.DeleteTransitGatewayVPCAttachment,

    -- ** DeleteTransitGatewayMulticastDomain
    module Network.AWS.EC2.DeleteTransitGatewayMulticastDomain,

    -- ** CancelReservedInstancesListing
    module Network.AWS.EC2.CancelReservedInstancesListing,

    -- ** AttachClassicLinkVPC
    module Network.AWS.EC2.AttachClassicLinkVPC,

    -- ** DisableTransitGatewayRouteTablePropagation
    module Network.AWS.EC2.DisableTransitGatewayRouteTablePropagation,

    -- ** DescribeVPCClassicLinkDNSSupport (Paginated)
    module Network.AWS.EC2.DescribeVPCClassicLinkDNSSupport,

    -- ** AssociateSubnetCidrBlock
    module Network.AWS.EC2.AssociateSubnetCidrBlock,

    -- ** RunScheduledInstances
    module Network.AWS.EC2.RunScheduledInstances,

    -- ** CreateTransitGatewayRoute
    module Network.AWS.EC2.CreateTransitGatewayRoute,

    -- ** CreateTransitGatewayPrefixListReference
    module Network.AWS.EC2.CreateTransitGatewayPrefixListReference,

    -- ** CancelSpotFleetRequests
    module Network.AWS.EC2.CancelSpotFleetRequests,

    -- ** DescribeSpotPriceHistory (Paginated)
    module Network.AWS.EC2.DescribeSpotPriceHistory,

    -- ** DescribeDHCPOptions (Paginated)
    module Network.AWS.EC2.DescribeDHCPOptions,

    -- ** ImportImage
    module Network.AWS.EC2.ImportImage,

    -- ** CreateLocalGatewayRouteTableVPCAssociation
    module Network.AWS.EC2.CreateLocalGatewayRouteTableVPCAssociation,

    -- ** CopyFpgaImage
    module Network.AWS.EC2.CopyFpgaImage,

    -- ** ImportClientVPNClientCertificateRevocationList
    module Network.AWS.EC2.ImportClientVPNClientCertificateRevocationList,

    -- ** StopInstances
    module Network.AWS.EC2.StopInstances,

    -- ** EnableEBSEncryptionByDefault
    module Network.AWS.EC2.EnableEBSEncryptionByDefault,

    -- ** DeregisterTransitGatewayMulticastGroupSources
    module Network.AWS.EC2.DeregisterTransitGatewayMulticastGroupSources,

    -- ** ModifyLaunchTemplate
    module Network.AWS.EC2.ModifyLaunchTemplate,

    -- ** ModifyVPCEndpointConnectionNotification
    module Network.AWS.EC2.ModifyVPCEndpointConnectionNotification,

    -- ** DescribeInternetGateways (Paginated)
    module Network.AWS.EC2.DescribeInternetGateways,

    -- ** DisableVPCClassicLink
    module Network.AWS.EC2.DisableVPCClassicLink,

    -- ** GetGroupsForCapacityReservation (Paginated)
    module Network.AWS.EC2.GetGroupsForCapacityReservation,

    -- ** DeleteLaunchTemplateVersions
    module Network.AWS.EC2.DeleteLaunchTemplateVersions,

    -- ** BundleInstance
    module Network.AWS.EC2.BundleInstance,

    -- ** DescribeNetworkInterfaces (Paginated)
    module Network.AWS.EC2.DescribeNetworkInterfaces,

    -- ** ReplaceNetworkACLAssociation
    module Network.AWS.EC2.ReplaceNetworkACLAssociation,

    -- ** DescribeNatGateways (Paginated)
    module Network.AWS.EC2.DescribeNatGateways,

    -- ** DescribeAddresses
    module Network.AWS.EC2.DescribeAddresses,

    -- ** RestoreManagedPrefixListVersion
    module Network.AWS.EC2.RestoreManagedPrefixListVersion,

    -- ** DescribeSnapshotAttribute
    module Network.AWS.EC2.DescribeSnapshotAttribute,

    -- ** DescribeIdentityIdFormat
    module Network.AWS.EC2.DescribeIdentityIdFormat,

    -- ** ReplaceRoute
    module Network.AWS.EC2.ReplaceRoute,

    -- ** DescribeVPCEndpointServices (Paginated)
    module Network.AWS.EC2.DescribeVPCEndpointServices,

    -- ** DeleteLocalGatewayRoute
    module Network.AWS.EC2.DeleteLocalGatewayRoute,

    -- ** AuthorizeSecurityGroupIngress
    module Network.AWS.EC2.AuthorizeSecurityGroupIngress,

    -- ** CreateVPCPeeringConnection
    module Network.AWS.EC2.CreateVPCPeeringConnection,

    -- ** DescribeSubnets (Paginated)
    module Network.AWS.EC2.DescribeSubnets,

    -- ** GetTransitGatewayAttachmentPropagations (Paginated)
    module Network.AWS.EC2.GetTransitGatewayAttachmentPropagations,

    -- ** CreateTags
    module Network.AWS.EC2.CreateTags,

    -- ** PurchaseReservedInstancesOffering
    module Network.AWS.EC2.PurchaseReservedInstancesOffering,

    -- ** DeleteNetworkACLEntry
    module Network.AWS.EC2.DeleteNetworkACLEntry,

    -- ** ResetSnapshotAttribute
    module Network.AWS.EC2.ResetSnapshotAttribute,

    -- ** DescribeVPNConnections
    module Network.AWS.EC2.DescribeVPNConnections,

    -- ** ModifyInstanceEventStartTime
    module Network.AWS.EC2.ModifyInstanceEventStartTime,

    -- ** DeleteRoute
    module Network.AWS.EC2.DeleteRoute,

    -- ** ReplaceNetworkACLEntry
    module Network.AWS.EC2.ReplaceNetworkACLEntry,

    -- ** DescribeVPCEndpoints (Paginated)
    module Network.AWS.EC2.DescribeVPCEndpoints,

    -- ** CreateTrafficMirrorFilter
    module Network.AWS.EC2.CreateTrafficMirrorFilter,

    -- ** ResetInstanceAttribute
    module Network.AWS.EC2.ResetInstanceAttribute,

    -- ** ModifyIdentityIdFormat
    module Network.AWS.EC2.ModifyIdentityIdFormat,

    -- ** AttachNetworkInterface
    module Network.AWS.EC2.AttachNetworkInterface,

    -- ** CreateCapacityReservation
    module Network.AWS.EC2.CreateCapacityReservation,

    -- ** DescribeInstanceStatus (Paginated)
    module Network.AWS.EC2.DescribeInstanceStatus,

    -- ** ImportKeyPair
    module Network.AWS.EC2.ImportKeyPair,

    -- ** DeleteTags
    module Network.AWS.EC2.DeleteTags,

    -- ** ConfirmProductInstance
    module Network.AWS.EC2.ConfirmProductInstance,

    -- ** DescribeInstanceAttribute
    module Network.AWS.EC2.DescribeInstanceAttribute,

    -- ** DescribeReservedInstancesOfferings (Paginated)
    module Network.AWS.EC2.DescribeReservedInstancesOfferings,

    -- ** CreateCustomerGateway
    module Network.AWS.EC2.CreateCustomerGateway,

    -- ** DescribeFleets (Paginated)
    module Network.AWS.EC2.DescribeFleets,

    -- ** CreateTransitGatewayPeeringAttachment
    module Network.AWS.EC2.CreateTransitGatewayPeeringAttachment,

    -- ** DeleteSecurityGroup
    module Network.AWS.EC2.DeleteSecurityGroup,

    -- ** DescribePublicIPv4Pools (Paginated)
    module Network.AWS.EC2.DescribePublicIPv4Pools,

    -- ** DescribeClientVPNTargetNetworks (Paginated)
    module Network.AWS.EC2.DescribeClientVPNTargetNetworks,

    -- ** DeleteVPCPeeringConnection
    module Network.AWS.EC2.DeleteVPCPeeringConnection,

    -- ** AttachInternetGateway
    module Network.AWS.EC2.AttachInternetGateway,

    -- ** ModifyInstancePlacement
    module Network.AWS.EC2.ModifyInstancePlacement,

    -- ** DescribeFlowLogs (Paginated)
    module Network.AWS.EC2.DescribeFlowLogs,

    -- ** DescribeLocalGatewayVirtualInterfaceGroups (Paginated)
    module Network.AWS.EC2.DescribeLocalGatewayVirtualInterfaceGroups,

    -- ** DescribeLocalGatewayRouteTableVPCAssociations (Paginated)
    module Network.AWS.EC2.DescribeLocalGatewayRouteTableVPCAssociations,

    -- ** DescribeVPCEndpointConnectionNotifications (Paginated)
    module Network.AWS.EC2.DescribeVPCEndpointConnectionNotifications,

    -- ** GetManagedPrefixListEntries (Paginated)
    module Network.AWS.EC2.GetManagedPrefixListEntries,

    -- ** RunInstances
    module Network.AWS.EC2.RunInstances,

    -- ** CreateSnapshots
    module Network.AWS.EC2.CreateSnapshots,

    -- ** AssociateDHCPOptions
    module Network.AWS.EC2.AssociateDHCPOptions,

    -- ** DeleteTrafficMirrorFilterRule
    module Network.AWS.EC2.DeleteTrafficMirrorFilterRule,

    -- ** DescribeReservedInstances
    module Network.AWS.EC2.DescribeReservedInstances,

    -- ** DescribeIdFormat
    module Network.AWS.EC2.DescribeIdFormat,

    -- ** DescribeVPCs (Paginated)
    module Network.AWS.EC2.DescribeVPCs,

    -- ** DescribeConversionTasks
    module Network.AWS.EC2.DescribeConversionTasks,

    -- ** CreateLaunchTemplateVersion
    module Network.AWS.EC2.CreateLaunchTemplateVersion,

    -- ** GetManagedPrefixListAssociations (Paginated)
    module Network.AWS.EC2.GetManagedPrefixListAssociations,

    -- ** DisableVPCClassicLinkDNSSupport
    module Network.AWS.EC2.DisableVPCClassicLinkDNSSupport,

    -- ** ApplySecurityGroupsToClientVPNTargetNetwork
    module Network.AWS.EC2.ApplySecurityGroupsToClientVPNTargetNetwork,

    -- ** DescribeTrafficMirrorTargets (Paginated)
    module Network.AWS.EC2.DescribeTrafficMirrorTargets,

    -- ** DescribeVolumesModifications (Paginated)
    module Network.AWS.EC2.DescribeVolumesModifications,

    -- ** ExportImage
    module Network.AWS.EC2.ExportImage,

    -- ** CreateFpgaImage
    module Network.AWS.EC2.CreateFpgaImage,

    -- ** AcceptVPCEndpointConnections
    module Network.AWS.EC2.AcceptVPCEndpointConnections,

    -- ** DeleteClientVPNEndpoint
    module Network.AWS.EC2.DeleteClientVPNEndpoint,

    -- ** SearchTransitGatewayRoutes
    module Network.AWS.EC2.SearchTransitGatewayRoutes,

    -- ** GetLaunchTemplateData
    module Network.AWS.EC2.GetLaunchTemplateData,

    -- ** AllocateAddress
    module Network.AWS.EC2.AllocateAddress,

    -- ** AcceptTransitGatewayVPCAttachment
    module Network.AWS.EC2.AcceptTransitGatewayVPCAttachment,

    -- ** CancelConversionTask
    module Network.AWS.EC2.CancelConversionTask,

    -- ** ModifyImageAttribute
    module Network.AWS.EC2.ModifyImageAttribute,

    -- ** CreateRouteTable
    module Network.AWS.EC2.CreateRouteTable,

    -- ** RejectTransitGatewayPeeringAttachment
    module Network.AWS.EC2.RejectTransitGatewayPeeringAttachment,

    -- ** ReportInstanceStatus
    module Network.AWS.EC2.ReportInstanceStatus,

    -- ** AttachVolume
    module Network.AWS.EC2.AttachVolume,

    -- ** RequestSpotInstances
    module Network.AWS.EC2.RequestSpotInstances,

    -- ** WithdrawByoipCidr
    module Network.AWS.EC2.WithdrawByoipCidr,

    -- ** DescribeHostReservationOfferings (Paginated)
    module Network.AWS.EC2.DescribeHostReservationOfferings,

    -- ** ResetFpgaImageAttribute
    module Network.AWS.EC2.ResetFpgaImageAttribute,

    -- ** ModifyVPNConnection
    module Network.AWS.EC2.ModifyVPNConnection,

    -- ** CreateTrafficMirrorFilterRule
    module Network.AWS.EC2.CreateTrafficMirrorFilterRule,

    -- ** DeleteTransitGateway
    module Network.AWS.EC2.DeleteTransitGateway,

    -- ** StartVPCEndpointServicePrivateDNSVerification
    module Network.AWS.EC2.StartVPCEndpointServicePrivateDNSVerification,

    -- ** DescribeVolumes (Paginated)
    module Network.AWS.EC2.DescribeVolumes,

    -- ** RejectVPCPeeringConnection
    module Network.AWS.EC2.RejectVPCPeeringConnection,

    -- ** DescribeClientVPNRoutes (Paginated)
    module Network.AWS.EC2.DescribeClientVPNRoutes,

    -- ** DeleteVPNConnectionRoute
    module Network.AWS.EC2.DeleteVPNConnectionRoute,

    -- ** AssociateEnclaveCertificateIAMRole
    module Network.AWS.EC2.AssociateEnclaveCertificateIAMRole,

    -- ** ModifyVPCEndpoint
    module Network.AWS.EC2.ModifyVPCEndpoint,

    -- ** DescribeFpgaImageAttribute
    module Network.AWS.EC2.DescribeFpgaImageAttribute,

    -- ** AllocateHosts
    module Network.AWS.EC2.AllocateHosts,

    -- ** CreateClientVPNEndpoint
    module Network.AWS.EC2.CreateClientVPNEndpoint,

    -- ** CreateTrafficMirrorSession
    module Network.AWS.EC2.CreateTrafficMirrorSession,

    -- ** RegisterImage
    module Network.AWS.EC2.RegisterImage,

    -- ** AdvertiseByoipCidr
    module Network.AWS.EC2.AdvertiseByoipCidr,

    -- ** ModifyFleet
    module Network.AWS.EC2.ModifyFleet,

    -- ** RevokeSecurityGroupIngress
    module Network.AWS.EC2.RevokeSecurityGroupIngress,

    -- ** GetEBSDefaultKMSKeyId
    module Network.AWS.EC2.GetEBSDefaultKMSKeyId,

    -- ** DescribeHostReservations (Paginated)
    module Network.AWS.EC2.DescribeHostReservations,

    -- ** UpdateSecurityGroupRuleDescriptionsEgress
    module Network.AWS.EC2.UpdateSecurityGroupRuleDescriptionsEgress,

    -- ** EnableVPCClassicLinkDNSSupport
    module Network.AWS.EC2.EnableVPCClassicLinkDNSSupport,

    -- ** DescribeVPCEndpointConnections (Paginated)
    module Network.AWS.EC2.DescribeVPCEndpointConnections,

    -- ** ModifyReservedInstances
    module Network.AWS.EC2.ModifyReservedInstances,

    -- ** DeleteFpgaImage
    module Network.AWS.EC2.DeleteFpgaImage,

    -- ** DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations (Paginated)
    module Network.AWS.EC2.DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations,

    -- ** DescribeScheduledInstances (Paginated)
    module Network.AWS.EC2.DescribeScheduledInstances,

    -- ** SearchTransitGatewayMulticastGroups (Paginated)
    module Network.AWS.EC2.SearchTransitGatewayMulticastGroups,

    -- ** CreateFlowLogs
    module Network.AWS.EC2.CreateFlowLogs,

    -- ** DescribeSpotFleetRequests (Paginated)
    module Network.AWS.EC2.DescribeSpotFleetRequests,

    -- ** MoveAddressToVPC
    module Network.AWS.EC2.MoveAddressToVPC,

    -- ** DescribeFleetInstances
    module Network.AWS.EC2.DescribeFleetInstances,

    -- ** DescribeLaunchTemplateVersions (Paginated)
    module Network.AWS.EC2.DescribeLaunchTemplateVersions,

    -- ** ModifyInstanceCreditSpecification
    module Network.AWS.EC2.ModifyInstanceCreditSpecification,

    -- ** DescribePrincipalIdFormat (Paginated)
    module Network.AWS.EC2.DescribePrincipalIdFormat,

    -- ** DescribeTransitGateways (Paginated)
    module Network.AWS.EC2.DescribeTransitGateways,

    -- ** DeleteNetworkACL
    module Network.AWS.EC2.DeleteNetworkACL,

    -- ** DisassociateTransitGatewayMulticastDomain
    module Network.AWS.EC2.DisassociateTransitGatewayMulticastDomain,

    -- ** DeleteTransitGatewayRouteTable
    module Network.AWS.EC2.DeleteTransitGatewayRouteTable,

    -- ** CreateLaunchTemplate
    module Network.AWS.EC2.CreateLaunchTemplate,

    -- ** CreateVPCEndpointConnectionNotification
    module Network.AWS.EC2.CreateVPCEndpointConnectionNotification,

    -- ** DeleteNetworkInterfacePermission
    module Network.AWS.EC2.DeleteNetworkInterfacePermission,

    -- ** DeleteVPNGateway
    module Network.AWS.EC2.DeleteVPNGateway,

    -- ** CreateTrafficMirrorTarget
    module Network.AWS.EC2.CreateTrafficMirrorTarget,

    -- ** DescribeImportImageTasks (Paginated)
    module Network.AWS.EC2.DescribeImportImageTasks,

    -- ** DescribeVolumeAttribute
    module Network.AWS.EC2.DescribeVolumeAttribute,

    -- ** DescribeMovingAddresses (Paginated)
    module Network.AWS.EC2.DescribeMovingAddresses,

    -- ** ExportTransitGatewayRoutes
    module Network.AWS.EC2.ExportTransitGatewayRoutes,

    -- ** GetPasswordData
    module Network.AWS.EC2.GetPasswordData,

    -- ** CreateVPC
    module Network.AWS.EC2.CreateVPC,

    -- ** ModifyVPCPeeringConnectionOptions
    module Network.AWS.EC2.ModifyVPCPeeringConnectionOptions,

    -- ** DescribeFpgaImages (Paginated)
    module Network.AWS.EC2.DescribeFpgaImages,

    -- ** CopySnapshot
    module Network.AWS.EC2.CopySnapshot,

    -- ** AcceptTransitGatewayPeeringAttachment
    module Network.AWS.EC2.AcceptTransitGatewayPeeringAttachment,

    -- ** DisassociateAddress
    module Network.AWS.EC2.DisassociateAddress,

    -- ** ModifyTrafficMirrorFilterNetworkServices
    module Network.AWS.EC2.ModifyTrafficMirrorFilterNetworkServices,

    -- ** DescribeEgressOnlyInternetGateways (Paginated)
    module Network.AWS.EC2.DescribeEgressOnlyInternetGateways,

    -- ** DeleteVPC
    module Network.AWS.EC2.DeleteVPC,

    -- ** CreateInstanceExportTask
    module Network.AWS.EC2.CreateInstanceExportTask,

    -- ** RejectTransitGatewayVPCAttachment
    module Network.AWS.EC2.RejectTransitGatewayVPCAttachment,

    -- ** DescribeTrafficMirrorSessions (Paginated)
    module Network.AWS.EC2.DescribeTrafficMirrorSessions,

    -- ** GetTransitGatewayRouteTableAssociations (Paginated)
    module Network.AWS.EC2.GetTransitGatewayRouteTableAssociations,

    -- ** AssociateVPCCidrBlock
    module Network.AWS.EC2.AssociateVPCCidrBlock,

    -- ** DescribeVPCAttribute
    module Network.AWS.EC2.DescribeVPCAttribute,

    -- ** CreateVolume
    module Network.AWS.EC2.CreateVolume,

    -- ** CreateDefaultSubnet
    module Network.AWS.EC2.CreateDefaultSubnet,

    -- ** DescribeScheduledInstanceAvailability (Paginated)
    module Network.AWS.EC2.DescribeScheduledInstanceAvailability,

    -- ** DisassociateClientVPNTargetNetwork
    module Network.AWS.EC2.DisassociateClientVPNTargetNetwork,

    -- ** CreateClientVPNRoute
    module Network.AWS.EC2.CreateClientVPNRoute,

    -- ** ModifyVolumeAttribute
    module Network.AWS.EC2.ModifyVolumeAttribute,

    -- ** ExportClientVPNClientConfiguration
    module Network.AWS.EC2.ExportClientVPNClientConfiguration,

    -- ** DeleteTrafficMirrorTarget
    module Network.AWS.EC2.DeleteTrafficMirrorTarget,

    -- ** DescribeSpotDatafeedSubscription
    module Network.AWS.EC2.DescribeSpotDatafeedSubscription,

    -- ** DescribeLocalGatewayRouteTables (Paginated)
    module Network.AWS.EC2.DescribeLocalGatewayRouteTables,

    -- ** DescribePrefixLists (Paginated)
    module Network.AWS.EC2.DescribePrefixLists,

    -- ** AssociateTransitGatewayRouteTable
    module Network.AWS.EC2.AssociateTransitGatewayRouteTable,

    -- ** DeletePlacementGroup
    module Network.AWS.EC2.DeletePlacementGroup,

    -- ** ModifyTransitGateway
    module Network.AWS.EC2.ModifyTransitGateway,

    -- ** DeleteTransitGatewayPrefixListReference
    module Network.AWS.EC2.DeleteTransitGatewayPrefixListReference,

    -- ** CreateTransitGatewayMulticastDomain
    module Network.AWS.EC2.CreateTransitGatewayMulticastDomain,

    -- ** DeregisterInstanceEventNotificationAttributes
    module Network.AWS.EC2.DeregisterInstanceEventNotificationAttributes,

    -- ** RequestSpotFleet
    module Network.AWS.EC2.RequestSpotFleet,

    -- ** DeleteTransitGatewayRoute
    module Network.AWS.EC2.DeleteTransitGatewayRoute,

    -- ** DisableEBSEncryptionByDefault
    module Network.AWS.EC2.DisableEBSEncryptionByDefault,

    -- ** DeregisterTransitGatewayMulticastGroupMembers
    module Network.AWS.EC2.DeregisterTransitGatewayMulticastGroupMembers,

    -- ** CreateSubnet
    module Network.AWS.EC2.CreateSubnet,

    -- ** CreateNetworkInterface
    module Network.AWS.EC2.CreateNetworkInterface,

    -- ** DescribeSecurityGroups (Paginated)
    module Network.AWS.EC2.DescribeSecurityGroups,

    -- ** GetCapacityReservationUsage
    module Network.AWS.EC2.GetCapacityReservationUsage,

    -- ** CreateTransitGatewayVPCAttachment
    module Network.AWS.EC2.CreateTransitGatewayVPCAttachment,

    -- ** DescribeExportTasks
    module Network.AWS.EC2.DescribeExportTasks,

    -- ** ModifySpotFleetRequest
    module Network.AWS.EC2.ModifySpotFleetRequest,

    -- ** DetachVPNGateway
    module Network.AWS.EC2.DetachVPNGateway,

    -- ** ModifyManagedPrefixList
    module Network.AWS.EC2.ModifyManagedPrefixList,

    -- ** GetHostReservationPurchasePreview
    module Network.AWS.EC2.GetHostReservationPurchasePreview,

    -- ** EnableVolumeIO
    module Network.AWS.EC2.EnableVolumeIO,

    -- ** DescribeInstances (Paginated)
    module Network.AWS.EC2.DescribeInstances,

    -- ** CreateNatGateway
    module Network.AWS.EC2.CreateNatGateway,

    -- ** DescribeLocalGatewayVirtualInterfaces (Paginated)
    module Network.AWS.EC2.DescribeLocalGatewayVirtualInterfaces,

    -- ** DescribeVPCPeeringConnections (Paginated)
    module Network.AWS.EC2.DescribeVPCPeeringConnections,

    -- ** CancelExportTask
    module Network.AWS.EC2.CancelExportTask,

    -- ** CreateVPCEndpointServiceConfiguration
    module Network.AWS.EC2.CreateVPCEndpointServiceConfiguration,

    -- ** CreateDefaultVPC
    module Network.AWS.EC2.CreateDefaultVPC,

    -- ** DisassociateVPCCidrBlock
    module Network.AWS.EC2.DisassociateVPCCidrBlock,

    -- ** DescribeTrafficMirrorFilters (Paginated)
    module Network.AWS.EC2.DescribeTrafficMirrorFilters,

    -- ** DescribeFastSnapshotRestores (Paginated)
    module Network.AWS.EC2.DescribeFastSnapshotRestores,

    -- ** CancelCapacityReservation
    module Network.AWS.EC2.CancelCapacityReservation,

    -- ** DeleteNetworkInterface
    module Network.AWS.EC2.DeleteNetworkInterface,

    -- ** DisassociateTransitGatewayRouteTable
    module Network.AWS.EC2.DisassociateTransitGatewayRouteTable,

    -- ** ReplaceRouteTableAssociation
    module Network.AWS.EC2.ReplaceRouteTableAssociation,

    -- ** StartInstances
    module Network.AWS.EC2.StartInstances,

    -- ** CreatePlacementGroup
    module Network.AWS.EC2.CreatePlacementGroup,

    -- ** DescribeInstanceEventNotificationAttributes
    module Network.AWS.EC2.DescribeInstanceEventNotificationAttributes,

    -- ** DescribeCapacityReservations (Paginated)
    module Network.AWS.EC2.DescribeCapacityReservations,

    -- ** ModifyClientVPNEndpoint
    module Network.AWS.EC2.ModifyClientVPNEndpoint,

    -- ** ModifyInstanceCapacityReservationAttributes
    module Network.AWS.EC2.ModifyInstanceCapacityReservationAttributes,

    -- ** DescribeAggregateIdFormat
    module Network.AWS.EC2.DescribeAggregateIdFormat,

    -- ** DescribeSnapshots (Paginated)
    module Network.AWS.EC2.DescribeSnapshots,

    -- ** AssociateAddress
    module Network.AWS.EC2.AssociateAddress,

    -- ** ModifyTrafficMirrorFilterRule
    module Network.AWS.EC2.ModifyTrafficMirrorFilterRule,

    -- ** DescribeNetworkInterfaceAttribute
    module Network.AWS.EC2.DescribeNetworkInterfaceAttribute,

    -- ** ReplaceIAMInstanceProfileAssociation
    module Network.AWS.EC2.ReplaceIAMInstanceProfileAssociation,

    -- ** AssociateClientVPNTargetNetwork
    module Network.AWS.EC2.AssociateClientVPNTargetNetwork,

    -- ** ReleaseHosts
    module Network.AWS.EC2.ReleaseHosts,

    -- ** ResetNetworkInterfaceAttribute
    module Network.AWS.EC2.ResetNetworkInterfaceAttribute,

    -- ** DeleteInternetGateway
    module Network.AWS.EC2.DeleteInternetGateway,

    -- ** DescribeReservedInstancesListings
    module Network.AWS.EC2.DescribeReservedInstancesListings,

    -- ** CreateVPNConnection
    module Network.AWS.EC2.CreateVPNConnection,

    -- ** ReplaceTransitGatewayRoute
    module Network.AWS.EC2.ReplaceTransitGatewayRoute,

    -- ** CreateFleet
    module Network.AWS.EC2.CreateFleet,

    -- ** DeleteNatGateway
    module Network.AWS.EC2.DeleteNatGateway,

    -- ** DescribeImportSnapshotTasks (Paginated)
    module Network.AWS.EC2.DescribeImportSnapshotTasks,

    -- ** GetCoipPoolUsage
    module Network.AWS.EC2.GetCoipPoolUsage,

    -- ** DescribeCustomerGateways
    module Network.AWS.EC2.DescribeCustomerGateways,

    -- ** DeleteSubnet
    module Network.AWS.EC2.DeleteSubnet,

    -- ** CopyImage
    module Network.AWS.EC2.CopyImage,

    -- ** CreateVPCEndpoint
    module Network.AWS.EC2.CreateVPCEndpoint,

    -- ** ModifyTrafficMirrorSession
    module Network.AWS.EC2.ModifyTrafficMirrorSession,

    -- ** DescribeCarrierGateways (Paginated)
    module Network.AWS.EC2.DescribeCarrierGateways,

    -- ** DescribeTransitGatewayPeeringAttachments (Paginated)
    module Network.AWS.EC2.DescribeTransitGatewayPeeringAttachments,

    -- ** DeleteQueuedReservedInstances
    module Network.AWS.EC2.DeleteQueuedReservedInstances,

    -- ** DescribeTransitGatewayMulticastDomains (Paginated)
    module Network.AWS.EC2.DescribeTransitGatewayMulticastDomains,

    -- ** GetDefaultCreditSpecification
    module Network.AWS.EC2.GetDefaultCreditSpecification,

    -- ** UnmonitorInstances
    module Network.AWS.EC2.UnmonitorInstances,

    -- ** DescribeTransitGatewayVPCAttachments (Paginated)
    module Network.AWS.EC2.DescribeTransitGatewayVPCAttachments,

    -- ** CreateSecurityGroup
    module Network.AWS.EC2.CreateSecurityGroup,

    -- ** GetEBSEncryptionByDefault
    module Network.AWS.EC2.GetEBSEncryptionByDefault,

    -- ** ImportVolume
    module Network.AWS.EC2.ImportVolume,

    -- ** DeleteCarrierGateway
    module Network.AWS.EC2.DeleteCarrierGateway,

    -- ** DisableVGWRoutePropagation
    module Network.AWS.EC2.DisableVGWRoutePropagation,

    -- ** DeleteTrafficMirrorFilter
    module Network.AWS.EC2.DeleteTrafficMirrorFilter,

    -- ** ModifyVPNTunnelCertificate
    module Network.AWS.EC2.ModifyVPNTunnelCertificate,

    -- ** CreateSpotDatafeedSubscription
    module Network.AWS.EC2.CreateSpotDatafeedSubscription,

    -- ** CancelSpotInstanceRequests
    module Network.AWS.EC2.CancelSpotInstanceRequests,

    -- ** CreateRoute
    module Network.AWS.EC2.CreateRoute,

    -- ** DescribeVPCEndpointServiceConfigurations (Paginated)
    module Network.AWS.EC2.DescribeVPCEndpointServiceConfigurations,

    -- ** DeleteSnapshot
    module Network.AWS.EC2.DeleteSnapshot,

    -- ** AssignPrivateIPAddresses
    module Network.AWS.EC2.AssignPrivateIPAddresses,

    -- ** AuthorizeClientVPNIngress
    module Network.AWS.EC2.AuthorizeClientVPNIngress,

    -- ** DeleteTransitGatewayPeeringAttachment
    module Network.AWS.EC2.DeleteTransitGatewayPeeringAttachment,

    -- ** ModifyInstanceAttribute
    module Network.AWS.EC2.ModifyInstanceAttribute,

    -- ** DeleteCustomerGateway
    module Network.AWS.EC2.DeleteCustomerGateway,

    -- ** DisassociateIAMInstanceProfile
    module Network.AWS.EC2.DisassociateIAMInstanceProfile,

    -- ** TerminateClientVPNConnections
    module Network.AWS.EC2.TerminateClientVPNConnections,

    -- ** DisassociateRouteTable
    module Network.AWS.EC2.DisassociateRouteTable,

    -- ** GetConsoleScreenshot
    module Network.AWS.EC2.GetConsoleScreenshot,

    -- ** ResetEBSDefaultKMSKeyId
    module Network.AWS.EC2.ResetEBSDefaultKMSKeyId,

    -- ** AssignIPv6Addresses
    module Network.AWS.EC2.AssignIPv6Addresses,

    -- ** ModifyVPNTunnelOptions
    module Network.AWS.EC2.ModifyVPNTunnelOptions,

    -- ** ModifyEBSDefaultKMSKeyId
    module Network.AWS.EC2.ModifyEBSDefaultKMSKeyId,

    -- ** DeleteSpotDatafeedSubscription
    module Network.AWS.EC2.DeleteSpotDatafeedSubscription,

    -- ** ModifyVolume
    module Network.AWS.EC2.ModifyVolume,

    -- ** EnableVPCClassicLink
    module Network.AWS.EC2.EnableVPCClassicLink,

    -- ** DescribePlacementGroups
    module Network.AWS.EC2.DescribePlacementGroups,

    -- ** ProvisionByoipCidr
    module Network.AWS.EC2.ProvisionByoipCidr,

    -- ** DisassociateEnclaveCertificateIAMRole
    module Network.AWS.EC2.DisassociateEnclaveCertificateIAMRole,

    -- ** ModifyAvailabilityZoneGroup
    module Network.AWS.EC2.ModifyAvailabilityZoneGroup,

    -- ** DescribeStaleSecurityGroups (Paginated)
    module Network.AWS.EC2.DescribeStaleSecurityGroups,

    -- ** CreateCarrierGateway
    module Network.AWS.EC2.CreateCarrierGateway,

    -- ** DescribeExportImageTasks (Paginated)
    module Network.AWS.EC2.DescribeExportImageTasks,

    -- ** PurchaseScheduledInstances
    module Network.AWS.EC2.PurchaseScheduledInstances,

    -- ** EnableVGWRoutePropagation
    module Network.AWS.EC2.EnableVGWRoutePropagation,

    -- ** DescribeSpotFleetRequestHistory
    module Network.AWS.EC2.DescribeSpotFleetRequestHistory,

    -- ** ModifySnapshotAttribute
    module Network.AWS.EC2.ModifySnapshotAttribute,

    -- ** DescribeIAMInstanceProfileAssociations (Paginated)
    module Network.AWS.EC2.DescribeIAMInstanceProfileAssociations,

    -- ** CreateSnapshot
    module Network.AWS.EC2.CreateSnapshot,

    -- ** CreateLocalGatewayRoute
    module Network.AWS.EC2.CreateLocalGatewayRoute,

    -- ** CreateNetworkACLEntry
    module Network.AWS.EC2.CreateNetworkACLEntry,

    -- ** DescribeTransitGatewayAttachments (Paginated)
    module Network.AWS.EC2.DescribeTransitGatewayAttachments,

    -- ** CreateReservedInstancesListing
    module Network.AWS.EC2.CreateReservedInstancesListing,

    -- ** DescribeIPv6Pools (Paginated)
    module Network.AWS.EC2.DescribeIPv6Pools,

    -- ** AttachVPNGateway
    module Network.AWS.EC2.AttachVPNGateway,

    -- ** DescribeLocalGateways (Paginated)
    module Network.AWS.EC2.DescribeLocalGateways,

    -- ** ModifyVPCEndpointServicePermissions
    module Network.AWS.EC2.ModifyVPCEndpointServicePermissions,

    -- ** ExportClientVPNClientCertificateRevocationList
    module Network.AWS.EC2.ExportClientVPNClientCertificateRevocationList,

    -- ** CreateDHCPOptions
    module Network.AWS.EC2.CreateDHCPOptions,

    -- ** RegisterTransitGatewayMulticastGroupSources
    module Network.AWS.EC2.RegisterTransitGatewayMulticastGroupSources,

    -- ** DescribeAccountAttributes
    module Network.AWS.EC2.DescribeAccountAttributes,

    -- ** GetTransitGatewayRouteTablePropagations (Paginated)
    module Network.AWS.EC2.GetTransitGatewayRouteTablePropagations,

    -- ** ModifyFpgaImageAttribute
    module Network.AWS.EC2.ModifyFpgaImageAttribute,

    -- ** ModifyHosts
    module Network.AWS.EC2.ModifyHosts,

    -- ** RebootInstances
    module Network.AWS.EC2.RebootInstances,

    -- ** ModifyVPCEndpointServiceConfiguration
    module Network.AWS.EC2.ModifyVPCEndpointServiceConfiguration,

    -- ** CreateTransitGateway
    module Network.AWS.EC2.CreateTransitGateway,

    -- ** UnassignIPv6Addresses
    module Network.AWS.EC2.UnassignIPv6Addresses,

    -- ** DeleteTrafficMirrorSession
    module Network.AWS.EC2.DeleteTrafficMirrorSession,

    -- ** CreateManagedPrefixList
    module Network.AWS.EC2.CreateManagedPrefixList,

    -- ** AssociateIAMInstanceProfile
    module Network.AWS.EC2.AssociateIAMInstanceProfile,

    -- ** ModifyDefaultCreditSpecification
    module Network.AWS.EC2.ModifyDefaultCreditSpecification,

    -- ** DeleteEgressOnlyInternetGateway
    module Network.AWS.EC2.DeleteEgressOnlyInternetGateway,

    -- ** PurchaseHostReservation
    module Network.AWS.EC2.PurchaseHostReservation,

    -- ** ModifyTransitGatewayVPCAttachment
    module Network.AWS.EC2.ModifyTransitGatewayVPCAttachment,

    -- ** CreateImage
    module Network.AWS.EC2.CreateImage,

    -- ** DescribeClassicLinkInstances (Paginated)
    module Network.AWS.EC2.DescribeClassicLinkInstances,

    -- ** TerminateInstances
    module Network.AWS.EC2.TerminateInstances,

    -- ** GetTransitGatewayPrefixListReferences (Paginated)
    module Network.AWS.EC2.GetTransitGatewayPrefixListReferences,

    -- ** DescribeKeyPairs
    module Network.AWS.EC2.DescribeKeyPairs,

    -- ** DisableFastSnapshotRestores
    module Network.AWS.EC2.DisableFastSnapshotRestores,

    -- ** DescribeLaunchTemplates (Paginated)
    module Network.AWS.EC2.DescribeLaunchTemplates,

    -- ** CreateVPNConnectionRoute
    module Network.AWS.EC2.CreateVPNConnectionRoute,

    -- ** AssociateRouteTable
    module Network.AWS.EC2.AssociateRouteTable,

    -- ** DescribeVPNGateways
    module Network.AWS.EC2.DescribeVPNGateways,

    -- ** ModifyVPNConnectionOptions
    module Network.AWS.EC2.ModifyVPNConnectionOptions,

    -- ** GetConsoleOutput
    module Network.AWS.EC2.GetConsoleOutput,

    -- ** DescribeHosts (Paginated)
    module Network.AWS.EC2.DescribeHosts,

    -- ** DescribeImageAttribute
    module Network.AWS.EC2.DescribeImageAttribute,

    -- ** ModifyIdFormat
    module Network.AWS.EC2.ModifyIdFormat,

    -- ** RegisterTransitGatewayMulticastGroupMembers
    module Network.AWS.EC2.RegisterTransitGatewayMulticastGroupMembers,

    -- ** DeleteManagedPrefixList
    module Network.AWS.EC2.DeleteManagedPrefixList,

    -- ** DeleteRouteTable
    module Network.AWS.EC2.DeleteRouteTable,

    -- ** ResetImageAttribute
    module Network.AWS.EC2.ResetImageAttribute,

    -- ** ModifyTransitGatewayPrefixListReference
    module Network.AWS.EC2.ModifyTransitGatewayPrefixListReference,

    -- ** DescribeTransitGatewayRouteTables (Paginated)
    module Network.AWS.EC2.DescribeTransitGatewayRouteTables,

    -- ** CreateEgressOnlyInternetGateway
    module Network.AWS.EC2.CreateEgressOnlyInternetGateway,

    -- ** DescribeReservedInstancesModifications (Paginated)
    module Network.AWS.EC2.DescribeReservedInstancesModifications,

    -- ** DescribeSpotInstanceRequests (Paginated)
    module Network.AWS.EC2.DescribeSpotInstanceRequests,

    -- ** RevokeClientVPNIngress
    module Network.AWS.EC2.RevokeClientVPNIngress,

    -- ** UnassignPrivateIPAddresses
    module Network.AWS.EC2.UnassignPrivateIPAddresses,

    -- ** DescribeNetworkInterfacePermissions (Paginated)
    module Network.AWS.EC2.DescribeNetworkInterfacePermissions,

    -- ** EnableFastSnapshotRestores
    module Network.AWS.EC2.EnableFastSnapshotRestores,

    -- ** DescribeVPCEndpointServicePermissions (Paginated)
    module Network.AWS.EC2.DescribeVPCEndpointServicePermissions,

    -- ** DeleteDHCPOptions
    module Network.AWS.EC2.DeleteDHCPOptions,

    -- ** RegisterInstanceEventNotificationAttributes
    module Network.AWS.EC2.RegisterInstanceEventNotificationAttributes,

    -- ** DescribeNetworkACLs (Paginated)
    module Network.AWS.EC2.DescribeNetworkACLs,

    -- ** CancelImportTask
    module Network.AWS.EC2.CancelImportTask,

    -- ** DetachClassicLinkVPC
    module Network.AWS.EC2.DetachClassicLinkVPC,

    -- ** DescribeRegions
    module Network.AWS.EC2.DescribeRegions,

    -- ** MonitorInstances
    module Network.AWS.EC2.MonitorInstances,

    -- ** SearchLocalGatewayRoutes (Paginated)
    module Network.AWS.EC2.SearchLocalGatewayRoutes,

    -- ** DeleteClientVPNRoute
    module Network.AWS.EC2.DeleteClientVPNRoute,

    -- ** AcceptVPCPeeringConnection
    module Network.AWS.EC2.AcceptVPCPeeringConnection,

    -- ** ImportSnapshot
    module Network.AWS.EC2.ImportSnapshot,

    -- ** DescribeVolumeStatus (Paginated)
    module Network.AWS.EC2.DescribeVolumeStatus,

    -- ** DescribeRouteTables (Paginated)
    module Network.AWS.EC2.DescribeRouteTables,

    -- ** DescribeAvailabilityZones
    module Network.AWS.EC2.DescribeAvailabilityZones,

    -- ** ModifyVPCAttribute
    module Network.AWS.EC2.ModifyVPCAttribute,

    -- ** DescribeClientVPNConnections (Paginated)
    module Network.AWS.EC2.DescribeClientVPNConnections,

    -- ** DescribeFleetHistory
    module Network.AWS.EC2.DescribeFleetHistory,

    -- ** DescribeImages
    module Network.AWS.EC2.DescribeImages,

    -- ** DescribeElasticGpus
    module Network.AWS.EC2.DescribeElasticGpus,

    -- ** RestoreAddressToClassic
    module Network.AWS.EC2.RestoreAddressToClassic,

    -- ** DescribeManagedPrefixLists (Paginated)
    module Network.AWS.EC2.DescribeManagedPrefixLists,

    -- ** CreateKeyPair
    module Network.AWS.EC2.CreateKeyPair,

    -- ** GetReservedInstancesExchangeQuote
    module Network.AWS.EC2.GetReservedInstancesExchangeQuote,

    -- ** DeleteVolume
    module Network.AWS.EC2.DeleteVolume,

    -- ** DeprovisionByoipCidr
    module Network.AWS.EC2.DeprovisionByoipCidr,

    -- ** DeleteVPCEndpointServiceConfigurations
    module Network.AWS.EC2.DeleteVPCEndpointServiceConfigurations,

    -- ** DescribeSpotFleetInstances (Paginated)
    module Network.AWS.EC2.DescribeSpotFleetInstances,

    -- * Types

    -- ** AccountAttributeName
    AccountAttributeName (..),

    -- ** ActivityStatus
    ActivityStatus (..),

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

    -- ** ClientVPNAuthenticationType
    ClientVPNAuthenticationType (..),

    -- ** ClientVPNAuthorizationRuleStatusCode
    ClientVPNAuthorizationRuleStatusCode (..),

    -- ** ClientVPNConnectionStatusCode
    ClientVPNConnectionStatusCode (..),

    -- ** ClientVPNEndpointAttributeStatusCode
    ClientVPNEndpointAttributeStatusCode (..),

    -- ** ClientVPNEndpointStatusCode
    ClientVPNEndpointStatusCode (..),

    -- ** ClientVPNRouteStatusCode
    ClientVPNRouteStatusCode (..),

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

    -- ** DNSNameState
    DNSNameState (..),

    -- ** DNSSupportValue
    DNSSupportValue (..),

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

    -- ** DomainType
    DomainType (..),

    -- ** EBSEncryptionSupport
    EBSEncryptionSupport (..),

    -- ** EBSNvmeSupport
    EBSNvmeSupport (..),

    -- ** EBSOptimizedSupport
    EBSOptimizedSupport (..),

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

    -- ** HTTPTokensState
    HTTPTokensState (..),

    -- ** HostRecovery
    HostRecovery (..),

    -- ** HostTenancy
    HostTenancy (..),

    -- ** HypervisorType
    HypervisorType (..),

    -- ** IAMInstanceProfileAssociationState
    IAMInstanceProfileAssociationState (..),

    -- ** IPv6SupportValue
    IPv6SupportValue (..),

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

    -- ** LaunchTemplateErrorCode
    LaunchTemplateErrorCode (..),

    -- ** LaunchTemplateHTTPTokensState
    LaunchTemplateHTTPTokensState (..),

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

    -- ** TunnelInsideIPVersion
    TunnelInsideIPVersion (..),

    -- ** UnlimitedSupportedInstanceFamily
    UnlimitedSupportedInstanceFamily (..),

    -- ** UnsuccessfulInstanceCreditSpecificationErrorCode
    UnsuccessfulInstanceCreditSpecificationErrorCode (..),

    -- ** UsageClassType
    UsageClassType (..),

    -- ** VPCAttributeName
    VPCAttributeName (..),

    -- ** VPCCidrBlockStateCode
    VPCCidrBlockStateCode (..),

    -- ** VPCEndpointType
    VPCEndpointType (..),

    -- ** VPCPeeringConnectionStateReasonCode
    VPCPeeringConnectionStateReasonCode (..),

    -- ** VPCState
    VPCState (..),

    -- ** VPCTenancy
    VPCTenancy (..),

    -- ** VPNEcmpSupportValue
    VPNEcmpSupportValue (..),

    -- ** VPNProtocol
    VPNProtocol (..),

    -- ** VPNState
    VPNState (..),

    -- ** VPNStaticRouteSource
    VPNStaticRouteSource (..),

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

    -- ** AccountAttribute
    AccountAttribute (..),
    mkAccountAttribute,
    aaAttributeValues,
    aaAttributeName,

    -- ** AccountAttributeValue
    AccountAttributeValue (..),
    mkAccountAttributeValue,
    aavAttributeValue,

    -- ** ActiveInstance
    ActiveInstance (..),
    mkActiveInstance,
    aiInstanceId,
    aiInstanceHealth,
    aiInstanceType,
    aiSpotInstanceRequestId,

    -- ** AddPrefixListEntry
    AddPrefixListEntry (..),
    mkAddPrefixListEntry,
    apleCidr,
    apleDescription,

    -- ** Address
    Address (..),
    mkAddress,
    aAssociationId,
    aInstanceId,
    aNetworkInterfaceOwnerId,
    aAllocationId,
    aCarrierIP,
    aNetworkBorderGroup,
    aDomain,
    aNetworkInterfaceId,
    aPublicIPv4Pool,
    aCustomerOwnedIPv4Pool,
    aCustomerOwnedIP,
    aPrivateIPAddress,
    aPublicIP,
    aTags,

    -- ** AllowedPrincipal
    AllowedPrincipal (..),
    mkAllowedPrincipal,
    apPrincipalType,
    apPrincipal,

    -- ** AssignedPrivateIPAddress
    AssignedPrivateIPAddress (..),
    mkAssignedPrivateIPAddress,
    apiaPrivateIPAddress,

    -- ** AssociatedRole
    AssociatedRole (..),
    mkAssociatedRole,
    arCertificateS3BucketName,
    arCertificateS3ObjectKey,
    arEncryptionKMSKeyId,
    arAssociatedRoleARN,

    -- ** AssociatedTargetNetwork
    AssociatedTargetNetwork (..),
    mkAssociatedTargetNetwork,
    atnNetworkId,
    atnNetworkType,

    -- ** AssociationStatus
    AssociationStatus (..),
    mkAssociationStatus,
    asCode,
    asMessage,

    -- ** AttributeBooleanValue
    AttributeBooleanValue (..),
    mkAttributeBooleanValue,
    abvValue,

    -- ** AttributeValue
    AttributeValue (..),
    mkAttributeValue,
    avValue,

    -- ** AuthorizationRule
    AuthorizationRule (..),
    mkAuthorizationRule,
    arStatus,
    arAccessAll,
    arClientVPNEndpointId,
    arGroupId,
    arDestinationCidr,
    arDescription,

    -- ** AvailabilityZone
    AvailabilityZone (..),
    mkAvailabilityZone,
    azState,
    azParentZoneId,
    azRegionName,
    azParentZoneName,
    azNetworkBorderGroup,
    azZoneId,
    azZoneName,
    azOptInStatus,
    azMessages,
    azGroupName,
    azZoneType,

    -- ** AvailabilityZoneMessage
    AvailabilityZoneMessage (..),
    mkAvailabilityZoneMessage,
    azmMessage,

    -- ** AvailableCapacity
    AvailableCapacity (..),
    mkAvailableCapacity,
    acAvailableInstanceCapacity,
    acAvailableVCPUs,

    -- ** BlobAttributeValue
    BlobAttributeValue (..),
    mkBlobAttributeValue,
    bavValue,

    -- ** BlockDeviceMapping
    BlockDeviceMapping (..),
    mkBlockDeviceMapping,
    bdmVirtualName,
    bdmNoDevice,
    bdmEBS,
    bdmDeviceName,

    -- ** BundleTask
    BundleTask (..),
    mkBundleTask,
    btInstanceId,
    btState,
    btProgress,
    btStartTime,
    btBundleId,
    btStorage,
    btUpdateTime,
    btBundleTaskError,

    -- ** BundleTaskError
    BundleTaskError (..),
    mkBundleTaskError,
    bteCode,
    bteMessage,

    -- ** ByoipCidr
    ByoipCidr (..),
    mkByoipCidr,
    bcState,
    bcCidr,
    bcStatusMessage,
    bcDescription,

    -- ** CPUOptions
    CPUOptions (..),
    mkCPUOptions,
    coCoreCount,
    coThreadsPerCore,

    -- ** CPUOptionsRequest
    CPUOptionsRequest (..),
    mkCPUOptionsRequest,
    corCoreCount,
    corThreadsPerCore,

    -- ** CancelSpotFleetRequestsError
    CancelSpotFleetRequestsError (..),
    mkCancelSpotFleetRequestsError,
    csfreCode,
    csfreMessage,

    -- ** CancelSpotFleetRequestsErrorItem
    CancelSpotFleetRequestsErrorItem (..),
    mkCancelSpotFleetRequestsErrorItem,
    csfreiError,
    csfreiSpotFleetRequestId,

    -- ** CancelSpotFleetRequestsSuccessItem
    CancelSpotFleetRequestsSuccessItem (..),
    mkCancelSpotFleetRequestsSuccessItem,
    csfrsiCurrentSpotFleetRequestState,
    csfrsiSpotFleetRequestId,
    csfrsiPreviousSpotFleetRequestState,

    -- ** CancelledSpotInstanceRequest
    CancelledSpotInstanceRequest (..),
    mkCancelledSpotInstanceRequest,
    csirState,
    csirSpotInstanceRequestId,

    -- ** CapacityReservation
    CapacityReservation (..),
    mkCapacityReservation,
    crState,
    crAvailabilityZoneId,
    crCreateDate,
    crEndDate,
    crAvailableInstanceCount,
    crEphemeralStorage,
    crInstancePlatform,
    crInstanceMatchCriteria,
    crCapacityReservationId,
    crInstanceType,
    crEBSOptimized,
    crOwnerId,
    crAvailabilityZone,
    crTenancy,
    crTotalInstanceCount,
    crEndDateType,
    crTags,
    crCapacityReservationARN,

    -- ** CapacityReservationGroup
    CapacityReservationGroup (..),
    mkCapacityReservationGroup,
    crgOwnerId,
    crgGroupARN,

    -- ** CapacityReservationOptions
    CapacityReservationOptions (..),
    mkCapacityReservationOptions,
    croUsageStrategy,

    -- ** CapacityReservationOptionsRequest
    CapacityReservationOptionsRequest (..),
    mkCapacityReservationOptionsRequest,
    crorUsageStrategy,

    -- ** CapacityReservationSpecification
    CapacityReservationSpecification (..),
    mkCapacityReservationSpecification,
    cCapacityReservationTarget,
    cCapacityReservationPreference,

    -- ** CapacityReservationSpecificationResponse
    CapacityReservationSpecificationResponse (..),
    mkCapacityReservationSpecificationResponse,
    crsCapacityReservationTarget,
    crsCapacityReservationPreference,

    -- ** CapacityReservationTarget
    CapacityReservationTarget (..),
    mkCapacityReservationTarget,
    crtCapacityReservationId,
    crtCapacityReservationResourceGroupARN,

    -- ** CapacityReservationTargetResponse
    CapacityReservationTargetResponse (..),
    mkCapacityReservationTargetResponse,
    cCapacityReservationId,
    cCapacityReservationResourceGroupARN,

    -- ** CarrierGateway
    CarrierGateway (..),
    mkCarrierGateway,
    cgState,
    cgVPCId,
    cgOwnerId,
    cgTags,
    cgCarrierGatewayId,

    -- ** CertificateAuthentication
    CertificateAuthentication (..),
    mkCertificateAuthentication,
    caClientRootCertificateChain,

    -- ** CertificateAuthenticationRequest
    CertificateAuthenticationRequest (..),
    mkCertificateAuthenticationRequest,
    carClientRootCertificateChainARN,

    -- ** CidrAuthorizationContext
    CidrAuthorizationContext (..),
    mkCidrAuthorizationContext,
    cacSignature,
    cacMessage,

    -- ** CidrBlock
    CidrBlock (..),
    mkCidrBlock,
    cbCidrBlock,

    -- ** ClassicLinkDNSSupport
    ClassicLinkDNSSupport (..),
    mkClassicLinkDNSSupport,
    cldsVPCId,
    cldsClassicLinkDNSSupported,

    -- ** ClassicLinkInstance
    ClassicLinkInstance (..),
    mkClassicLinkInstance,
    cliInstanceId,
    cliGroups,
    cliVPCId,
    cliTags,

    -- ** ClassicLoadBalancer
    ClassicLoadBalancer (..),
    mkClassicLoadBalancer,
    clbName,

    -- ** ClassicLoadBalancersConfig
    ClassicLoadBalancersConfig (..),
    mkClassicLoadBalancersConfig,
    clbcClassicLoadBalancers,

    -- ** ClientCertificateRevocationListStatus
    ClientCertificateRevocationListStatus (..),
    mkClientCertificateRevocationListStatus,
    ccrlsCode,
    ccrlsMessage,

    -- ** ClientConnectOptions
    ClientConnectOptions (..),
    mkClientConnectOptions,
    ccoEnabled,
    ccoLambdaFunctionARN,

    -- ** ClientConnectResponseOptions
    ClientConnectResponseOptions (..),
    mkClientConnectResponseOptions,
    ccroStatus,
    ccroEnabled,
    ccroLambdaFunctionARN,

    -- ** ClientData
    ClientData (..),
    mkClientData,
    cdUploadStart,
    cdUploadSize,
    cdUploadEnd,
    cdComment,

    -- ** ClientVPNAuthentication
    ClientVPNAuthentication (..),
    mkClientVPNAuthentication,
    cvaActiveDirectory,
    cvaFederatedAuthentication,
    cvaMutualAuthentication,
    cvaType,

    -- ** ClientVPNAuthenticationRequest
    ClientVPNAuthenticationRequest (..),
    mkClientVPNAuthenticationRequest,
    cvarActiveDirectory,
    cvarFederatedAuthentication,
    cvarMutualAuthentication,
    cvarType,

    -- ** ClientVPNAuthorizationRuleStatus
    ClientVPNAuthorizationRuleStatus (..),
    mkClientVPNAuthorizationRuleStatus,
    cvarsCode,
    cvarsMessage,

    -- ** ClientVPNConnection
    ClientVPNConnection (..),
    mkClientVPNConnection,
    cvcIngressPackets,
    cvcStatus,
    cvcConnectionEndTime,
    cvcCommonName,
    cvcPostureComplianceStatuses,
    cvcConnectionEstablishedTime,
    cvcConnectionId,
    cvcIngressBytes,
    cvcUsername,
    cvcEgressBytes,
    cvcClientVPNEndpointId,
    cvcClientIP,
    cvcEgressPackets,
    cvcTimestamp,

    -- ** ClientVPNConnectionStatus
    ClientVPNConnectionStatus (..),
    mkClientVPNConnectionStatus,
    cvcsCode,
    cvcsMessage,

    -- ** ClientVPNEndpoint
    ClientVPNEndpoint (..),
    mkClientVPNEndpoint,
    cveCreationTime,
    cveStatus,
    cveAssociatedTargetNetworks,
    cveSecurityGroupIds,
    cveConnectionLogOptions,
    cveSplitTunnel,
    cveTransportProtocol,
    cveVPCId,
    cveVPNPort,
    cveDeletionTime,
    cveClientCidrBlock,
    cveDNSServers,
    cveClientVPNEndpointId,
    cveClientConnectOptions,
    cveServerCertificateARN,
    cveAuthenticationOptions,
    cveSelfServicePortalURL,
    cveDescription,
    cveDNSName,
    cveVPNProtocol,
    cveTags,

    -- ** ClientVPNEndpointAttributeStatus
    ClientVPNEndpointAttributeStatus (..),
    mkClientVPNEndpointAttributeStatus,
    cveasCode,
    cveasMessage,

    -- ** ClientVPNEndpointStatus
    ClientVPNEndpointStatus (..),
    mkClientVPNEndpointStatus,
    cvesCode,
    cvesMessage,

    -- ** ClientVPNRoute
    ClientVPNRoute (..),
    mkClientVPNRoute,
    cvrStatus,
    cvrOrigin,
    cvrClientVPNEndpointId,
    cvrTargetSubnet,
    cvrDestinationCidr,
    cvrType,
    cvrDescription,

    -- ** ClientVPNRouteStatus
    ClientVPNRouteStatus (..),
    mkClientVPNRouteStatus,
    cvrsCode,
    cvrsMessage,

    -- ** CoipAddressUsage
    CoipAddressUsage (..),
    mkCoipAddressUsage,
    cauAllocationId,
    cauAWSAccountId,
    cauCoIP,
    cauAWSService,

    -- ** CoipPool
    CoipPool (..),
    mkCoipPool,
    cpPoolId,
    cpLocalGatewayRouteTableId,
    cpPoolCidrs,
    cpTags,
    cpPoolARN,

    -- ** ConnectionLogOptions
    ConnectionLogOptions (..),
    mkConnectionLogOptions,
    cloEnabled,
    cloCloudwatchLogStream,
    cloCloudwatchLogGroup,

    -- ** ConnectionLogResponseOptions
    ConnectionLogResponseOptions (..),
    mkConnectionLogResponseOptions,
    clroEnabled,
    clroCloudwatchLogStream,
    clroCloudwatchLogGroup,

    -- ** ConnectionNotification
    ConnectionNotification (..),
    mkConnectionNotification,
    cnConnectionNotificationState,
    cnConnectionNotificationType,
    cnConnectionEvents,
    cnServiceId,
    cnVPCEndpointId,
    cnConnectionNotificationId,
    cnConnectionNotificationARN,

    -- ** ConversionTask
    ConversionTask (..),
    mkConversionTask,
    cImportInstance,
    cState,
    cStatusMessage,
    cImportVolume,
    cConversionTaskId,
    cExpirationTime,
    cTags,

    -- ** CreateFleetError
    CreateFleetError (..),
    mkCreateFleetError,
    cfeLifecycle,
    cfeLaunchTemplateAndOverrides,
    cfeErrorCode,
    cfeErrorMessage,

    -- ** CreateFleetInstance
    CreateFleetInstance (..),
    mkCreateFleetInstance,
    cfiPlatform,
    cfiLifecycle,
    cfiLaunchTemplateAndOverrides,
    cfiInstanceType,
    cfiInstanceIds,

    -- ** CreateTransitGatewayVPCAttachmentRequestOptions
    CreateTransitGatewayVPCAttachmentRequestOptions (..),
    mkCreateTransitGatewayVPCAttachmentRequestOptions,
    ctgvaroIPv6Support,
    ctgvaroApplianceModeSupport,
    ctgvaroDNSSupport,

    -- ** CreateVolumePermission
    CreateVolumePermission (..),
    mkCreateVolumePermission,
    cvpGroup,
    cvpUserId,

    -- ** CreateVolumePermissionModifications
    CreateVolumePermissionModifications (..),
    mkCreateVolumePermissionModifications,
    cvpmRemove,
    cvpmAdd,

    -- ** CreditSpecification
    CreditSpecification (..),
    mkCreditSpecification,
    csCPUCredits,

    -- ** CreditSpecificationRequest
    CreditSpecificationRequest (..),
    mkCreditSpecificationRequest,
    csrCPUCredits,

    -- ** CustomerGateway
    CustomerGateway (..),
    mkCustomerGateway,
    cgfState,
    cgfIPAddress,
    cgfCertificateARN,
    cgfBGPASN,
    cgfCustomerGatewayId,
    cgfDeviceName,
    cgfType,
    cgfTags,

    -- ** DHCPConfiguration
    DHCPConfiguration (..),
    mkDHCPConfiguration,
    dcValues,
    dcKey,

    -- ** DHCPOptions
    DHCPOptions (..),
    mkDHCPOptions,
    doDHCPConfigurations,
    doOwnerId,
    doDHCPOptionsId,
    doTags,

    -- ** DNSEntry
    DNSEntry (..),
    mkDNSEntry,
    deHostedZoneId,
    deDNSName,

    -- ** DNSServersOptionsModifyStructure
    DNSServersOptionsModifyStructure (..),
    mkDNSServersOptionsModifyStructure,
    dsomsEnabled,
    dsomsCustomDNSServers,

    -- ** DeleteFleetError
    DeleteFleetError (..),
    mkDeleteFleetError,
    dfeCode,
    dfeMessage,

    -- ** DeleteFleetErrorItem
    DeleteFleetErrorItem (..),
    mkDeleteFleetErrorItem,
    dfeiError,
    dfeiFleetId,

    -- ** DeleteFleetSuccessItem
    DeleteFleetSuccessItem (..),
    mkDeleteFleetSuccessItem,
    dfsiCurrentFleetState,
    dfsiPreviousFleetState,
    dfsiFleetId,

    -- ** DeleteLaunchTemplateVersionsResponseErrorItem
    DeleteLaunchTemplateVersionsResponseErrorItem (..),
    mkDeleteLaunchTemplateVersionsResponseErrorItem,
    dltvreiLaunchTemplateName,
    dltvreiLaunchTemplateId,
    dltvreiVersionNumber,
    dltvreiResponseError,

    -- ** DeleteLaunchTemplateVersionsResponseSuccessItem
    DeleteLaunchTemplateVersionsResponseSuccessItem (..),
    mkDeleteLaunchTemplateVersionsResponseSuccessItem,
    dltvrsiLaunchTemplateName,
    dltvrsiLaunchTemplateId,
    dltvrsiVersionNumber,

    -- ** DeleteQueuedReservedInstancesError
    DeleteQueuedReservedInstancesError (..),
    mkDeleteQueuedReservedInstancesError,
    dqrieCode,
    dqrieMessage,

    -- ** DeregisterInstanceTagAttributeRequest
    DeregisterInstanceTagAttributeRequest (..),
    mkDeregisterInstanceTagAttributeRequest,
    ditarIncludeAllTagsOfInstance,
    ditarInstanceTagKeys,

    -- ** DescribeFastSnapshotRestoreSuccessItem
    DescribeFastSnapshotRestoreSuccessItem (..),
    mkDescribeFastSnapshotRestoreSuccessItem,
    dfsrsiDisablingTime,
    dfsrsiState,
    dfsrsiOwnerAlias,
    dfsrsiDisabledTime,
    dfsrsiEnabledTime,
    dfsrsiOptimizingTime,
    dfsrsiOwnerId,
    dfsrsiStateTransitionReason,
    dfsrsiAvailabilityZone,
    dfsrsiSnapshotId,
    dfsrsiEnablingTime,

    -- ** DescribeFleetError
    DescribeFleetError (..),
    mkDescribeFleetError,
    dfeLifecycle,
    dfeLaunchTemplateAndOverrides,
    dfeErrorCode,
    dfeErrorMessage,

    -- ** DescribeFleetsInstances
    DescribeFleetsInstances (..),
    mkDescribeFleetsInstances,
    dfiPlatform,
    dfiLifecycle,
    dfiLaunchTemplateAndOverrides,
    dfiInstanceType,
    dfiInstanceIds,

    -- ** DirectoryServiceAuthentication
    DirectoryServiceAuthentication (..),
    mkDirectoryServiceAuthentication,
    dsaDirectoryId,

    -- ** DirectoryServiceAuthenticationRequest
    DirectoryServiceAuthenticationRequest (..),
    mkDirectoryServiceAuthenticationRequest,
    dsarDirectoryId,

    -- ** DisableFastSnapshotRestoreErrorItem
    DisableFastSnapshotRestoreErrorItem (..),
    mkDisableFastSnapshotRestoreErrorItem,
    dfsreiFastSnapshotRestoreStateErrors,
    dfsreiSnapshotId,

    -- ** DisableFastSnapshotRestoreStateError
    DisableFastSnapshotRestoreStateError (..),
    mkDisableFastSnapshotRestoreStateError,
    dfsrseCode,
    dfsrseMessage,

    -- ** DisableFastSnapshotRestoreStateErrorItem
    DisableFastSnapshotRestoreStateErrorItem (..),
    mkDisableFastSnapshotRestoreStateErrorItem,
    dfsrseiError,
    dfsrseiAvailabilityZone,

    -- ** DisableFastSnapshotRestoreSuccessItem
    DisableFastSnapshotRestoreSuccessItem (..),
    mkDisableFastSnapshotRestoreSuccessItem,
    dDisablingTime,
    dState,
    dOwnerAlias,
    dDisabledTime,
    dEnabledTime,
    dOptimizingTime,
    dOwnerId,
    dStateTransitionReason,
    dAvailabilityZone,
    dSnapshotId,
    dEnablingTime,

    -- ** DiskImage
    DiskImage (..),
    mkDiskImage,
    diImage,
    diVolume,
    diDescription,

    -- ** DiskImageDescription
    DiskImageDescription (..),
    mkDiskImageDescription,
    didSize,
    didChecksum,
    didFormat,
    didImportManifestURL,

    -- ** DiskImageDetail
    DiskImageDetail (..),
    mkDiskImageDetail,
    dFormat,
    dImportManifestURL,
    dBytes,

    -- ** DiskImageVolumeDescription
    DiskImageVolumeDescription (..),
    mkDiskImageVolumeDescription,
    divdSize,
    divdId,

    -- ** DiskInfo
    DiskInfo (..),
    mkDiskInfo,
    diCount,
    diSizeInGB,
    diType,

    -- ** EBSBlockDevice
    EBSBlockDevice (..),
    mkEBSBlockDevice,
    ebdDeleteOnTermination,
    ebdVolumeSize,
    ebdIOPS,
    ebdEncrypted,
    ebdKMSKeyId,
    ebdVolumeType,
    ebdSnapshotId,

    -- ** EBSInfo
    EBSInfo (..),
    mkEBSInfo,
    eiEBSOptimizedInfo,
    eiEncryptionSupport,
    eiEBSOptimizedSupport,
    eiNvmeSupport,

    -- ** EBSInstanceBlockDevice
    EBSInstanceBlockDevice (..),
    mkEBSInstanceBlockDevice,
    eibdStatus,
    eibdDeleteOnTermination,
    eibdVolumeId,
    eibdAttachTime,

    -- ** EBSInstanceBlockDeviceSpecification
    EBSInstanceBlockDeviceSpecification (..),
    mkEBSInstanceBlockDeviceSpecification,
    eibdsDeleteOnTermination,
    eibdsVolumeId,

    -- ** EBSOptimizedInfo
    EBSOptimizedInfo (..),
    mkEBSOptimizedInfo,
    eoiMaximumIOPS,
    eoiBaselineIOPS,
    eoiMaximumThroughputInMBps,
    eoiMaximumBandwidthInMbps,
    eoiBaselineBandwidthInMbps,
    eoiBaselineThroughputInMBps,

    -- ** EgressOnlyInternetGateway
    EgressOnlyInternetGateway (..),
    mkEgressOnlyInternetGateway,
    eoigEgressOnlyInternetGatewayId,
    eoigAttachments,
    eoigTags,

    -- ** ElasticGpuAssociation
    ElasticGpuAssociation (..),
    mkElasticGpuAssociation,
    egaElasticGpuId,
    egaElasticGpuAssociationId,
    egaElasticGpuAssociationTime,
    egaElasticGpuAssociationState,

    -- ** ElasticGpuHealth
    ElasticGpuHealth (..),
    mkElasticGpuHealth,
    eghStatus,

    -- ** ElasticGpuSpecification
    ElasticGpuSpecification (..),
    mkElasticGpuSpecification,
    egsType,

    -- ** ElasticGpuSpecificationResponse
    ElasticGpuSpecificationResponse (..),
    mkElasticGpuSpecificationResponse,
    eType,

    -- ** ElasticGpus
    ElasticGpus (..),
    mkElasticGpus,
    egInstanceId,
    egElasticGpuType,
    egElasticGpuId,
    egElasticGpuState,
    egElasticGpuHealth,
    egAvailabilityZone,
    egTags,

    -- ** ElasticInferenceAccelerator
    ElasticInferenceAccelerator (..),
    mkElasticInferenceAccelerator,
    eiaCount,
    eiaType,

    -- ** ElasticInferenceAcceleratorAssociation
    ElasticInferenceAcceleratorAssociation (..),
    mkElasticInferenceAcceleratorAssociation,
    eiaaElasticInferenceAcceleratorAssociationState,
    eiaaElasticInferenceAcceleratorAssociationTime,
    eiaaElasticInferenceAcceleratorARN,
    eiaaElasticInferenceAcceleratorAssociationId,

    -- ** EnableFastSnapshotRestoreErrorItem
    EnableFastSnapshotRestoreErrorItem (..),
    mkEnableFastSnapshotRestoreErrorItem,
    efsreiFastSnapshotRestoreStateErrors,
    efsreiSnapshotId,

    -- ** EnableFastSnapshotRestoreStateError
    EnableFastSnapshotRestoreStateError (..),
    mkEnableFastSnapshotRestoreStateError,
    efsrseCode,
    efsrseMessage,

    -- ** EnableFastSnapshotRestoreStateErrorItem
    EnableFastSnapshotRestoreStateErrorItem (..),
    mkEnableFastSnapshotRestoreStateErrorItem,
    efsrseiError,
    efsrseiAvailabilityZone,

    -- ** EnableFastSnapshotRestoreSuccessItem
    EnableFastSnapshotRestoreSuccessItem (..),
    mkEnableFastSnapshotRestoreSuccessItem,
    efsrsiDisablingTime,
    efsrsiState,
    efsrsiOwnerAlias,
    efsrsiDisabledTime,
    efsrsiEnabledTime,
    efsrsiOptimizingTime,
    efsrsiOwnerId,
    efsrsiStateTransitionReason,
    efsrsiAvailabilityZone,
    efsrsiSnapshotId,
    efsrsiEnablingTime,

    -- ** EnclaveOptions
    EnclaveOptions (..),
    mkEnclaveOptions,
    eoEnabled,

    -- ** EnclaveOptionsRequest
    EnclaveOptionsRequest (..),
    mkEnclaveOptionsRequest,
    eorEnabled,

    -- ** EventInformation
    EventInformation (..),
    mkEventInformation,
    eiInstanceId,
    eiEventDescription,
    eiEventSubType,

    -- ** ExportImageTask
    ExportImageTask (..),
    mkExportImageTask,
    eitStatus,
    eitProgress,
    eitExportImageTaskId,
    eitStatusMessage,
    eitImageId,
    eitDescription,
    eitTags,
    eitS3ExportLocation,

    -- ** ExportTask
    ExportTask (..),
    mkExportTask,
    etExportTaskId,
    etState,
    etExportToS3Task,
    etInstanceExportDetails,
    etStatusMessage,
    etDescription,
    etTags,

    -- ** ExportTaskS3Location
    ExportTaskS3Location (..),
    mkExportTaskS3Location,
    etslS3Prefix,
    etslS3Bucket,

    -- ** ExportTaskS3LocationRequest
    ExportTaskS3LocationRequest (..),
    mkExportTaskS3LocationRequest,
    etslrS3Prefix,
    etslrS3Bucket,

    -- ** ExportToS3Task
    ExportToS3Task (..),
    mkExportToS3Task,
    etstS3Key,
    etstContainerFormat,
    etstS3Bucket,
    etstDiskImageFormat,

    -- ** ExportToS3TaskSpecification
    ExportToS3TaskSpecification (..),
    mkExportToS3TaskSpecification,
    etstsContainerFormat,
    etstsS3Prefix,
    etstsS3Bucket,
    etstsDiskImageFormat,

    -- ** FailedQueuedPurchaseDeletion
    FailedQueuedPurchaseDeletion (..),
    mkFailedQueuedPurchaseDeletion,
    fqpdError,
    fqpdReservedInstancesId,

    -- ** FederatedAuthentication
    FederatedAuthentication (..),
    mkFederatedAuthentication,
    faSamlProviderARN,
    faSelfServiceSamlProviderARN,

    -- ** FederatedAuthenticationRequest
    FederatedAuthenticationRequest (..),
    mkFederatedAuthenticationRequest,
    farSAMLProviderARN,
    farSelfServiceSAMLProviderARN,

    -- ** Filter
    Filter (..),
    mkFilter,
    fValues,
    fName,

    -- ** FleetData
    FleetData (..),
    mkFleetData,
    fdClientToken,
    fdTargetCapacitySpecification,
    fdSpotOptions,
    fdExcessCapacityTerminationPolicy,
    fdOnDemandOptions,
    fdFleetState,
    fdLaunchTemplateConfigs,
    fdValidUntil,
    fdTerminateInstancesWithExpiration,
    fdInstances,
    fdFulfilledCapacity,
    fdType,
    fdValidFrom,
    fdReplaceUnhealthyInstances,
    fdFulfilledOnDemandCapacity,
    fdFleetId,
    fdErrors,
    fdCreateTime,
    fdTags,
    fdActivityStatus,

    -- ** FleetLaunchTemplateConfig
    FleetLaunchTemplateConfig (..),
    mkFleetLaunchTemplateConfig,
    fltcOverrides,
    fltcLaunchTemplateSpecification,

    -- ** FleetLaunchTemplateConfigRequest
    FleetLaunchTemplateConfigRequest (..),
    mkFleetLaunchTemplateConfigRequest,
    fltcrOverrides,
    fltcrLaunchTemplateSpecification,

    -- ** FleetLaunchTemplateOverrides
    FleetLaunchTemplateOverrides (..),
    mkFleetLaunchTemplateOverrides,
    fltoPriority,
    fltoWeightedCapacity,
    fltoSubnetId,
    fltoInstanceType,
    fltoAvailabilityZone,
    fltoPlacement,
    fltoMaxPrice,

    -- ** FleetLaunchTemplateOverridesRequest
    FleetLaunchTemplateOverridesRequest (..),
    mkFleetLaunchTemplateOverridesRequest,
    fltorPriority,
    fltorWeightedCapacity,
    fltorSubnetId,
    fltorInstanceType,
    fltorAvailabilityZone,
    fltorPlacement,
    fltorMaxPrice,

    -- ** FleetLaunchTemplateSpecification
    FleetLaunchTemplateSpecification (..),
    mkFleetLaunchTemplateSpecification,
    fltsLaunchTemplateName,
    fltsLaunchTemplateId,
    fltsVersion,

    -- ** FleetLaunchTemplateSpecificationRequest
    FleetLaunchTemplateSpecificationRequest (..),
    mkFleetLaunchTemplateSpecificationRequest,
    fltsrLaunchTemplateName,
    fltsrLaunchTemplateId,
    fltsrVersion,

    -- ** FleetSpotCapacityRebalance
    FleetSpotCapacityRebalance (..),
    mkFleetSpotCapacityRebalance,
    fscrReplacementStrategy,

    -- ** FleetSpotCapacityRebalanceRequest
    FleetSpotCapacityRebalanceRequest (..),
    mkFleetSpotCapacityRebalanceRequest,
    fscrrReplacementStrategy,

    -- ** FleetSpotMaintenanceStrategies
    FleetSpotMaintenanceStrategies (..),
    mkFleetSpotMaintenanceStrategies,
    fsmsCapacityRebalance,

    -- ** FleetSpotMaintenanceStrategiesRequest
    FleetSpotMaintenanceStrategiesRequest (..),
    mkFleetSpotMaintenanceStrategiesRequest,
    fsmsrCapacityRebalance,

    -- ** FlowLog
    FlowLog (..),
    mkFlowLog,
    flCreationTime,
    flLogFormat,
    flMaxAggregationInterval,
    flResourceId,
    flFlowLogStatus,
    flTrafficType,
    flLogDestination,
    flDeliverLogsStatus,
    flDeliverLogsErrorMessage,
    flLogGroupName,
    flDeliverLogsPermissionARN,
    flLogDestinationType,
    flFlowLogId,
    flTags,

    -- ** FpgaDeviceInfo
    FpgaDeviceInfo (..),
    mkFpgaDeviceInfo,
    fdiMemoryInfo,
    fdiManufacturer,
    fdiCount,
    fdiName,

    -- ** FpgaDeviceMemoryInfo
    FpgaDeviceMemoryInfo (..),
    mkFpgaDeviceMemoryInfo,
    fdmiSizeInMiB,

    -- ** FpgaImage
    FpgaImage (..),
    mkFpgaImage,
    fiShellVersion,
    fiPciId,
    fiState,
    fiOwnerAlias,
    fiFpgaImageId,
    fiDataRetentionSupport,
    fiOwnerId,
    fiUpdateTime,
    fiName,
    fiProductCodes,
    fiDescription,
    fiCreateTime,
    fiTags,
    fiPublic,
    fiFpgaImageGlobalId,

    -- ** FpgaImageAttribute
    FpgaImageAttribute (..),
    mkFpgaImageAttribute,
    fiaFpgaImageId,
    fiaName,
    fiaProductCodes,
    fiaDescription,
    fiaLoadPermissions,

    -- ** FpgaImageState
    FpgaImageState (..),
    mkFpgaImageState,
    fisCode,
    fisMessage,

    -- ** FpgaInfo
    FpgaInfo (..),
    mkFpgaInfo,
    fiTotalFpgaMemoryInMiB,
    fiFpgas,

    -- ** GpuDeviceInfo
    GpuDeviceInfo (..),
    mkGpuDeviceInfo,
    gdiMemoryInfo,
    gdiManufacturer,
    gdiCount,
    gdiName,

    -- ** GpuDeviceMemoryInfo
    GpuDeviceMemoryInfo (..),
    mkGpuDeviceMemoryInfo,
    gdmiSizeInMiB,

    -- ** GpuInfo
    GpuInfo (..),
    mkGpuInfo,
    giTotalGpuMemoryInMiB,
    giGpus,

    -- ** GroupIdentifier
    GroupIdentifier (..),
    mkGroupIdentifier,
    giGroupId,
    giGroupName,

    -- ** HibernationOptions
    HibernationOptions (..),
    mkHibernationOptions,
    hoConfigured,

    -- ** HibernationOptionsRequest
    HibernationOptionsRequest (..),
    mkHibernationOptionsRequest,
    horConfigured,

    -- ** HistoryRecord
    HistoryRecord (..),
    mkHistoryRecord,
    hrEventType,
    hrEventInformation,
    hrTimestamp,

    -- ** HistoryRecordEntry
    HistoryRecordEntry (..),
    mkHistoryRecordEntry,
    hreEventType,
    hreEventInformation,
    hreTimestamp,

    -- ** Host
    Host (..),
    mkHost,
    hReleaseTime,
    hState,
    hClientToken,
    hAvailabilityZoneId,
    hHostId,
    hAvailableCapacity,
    hHostReservationId,
    hAllowsMultipleInstanceTypes,
    hHostProperties,
    hOwnerId,
    hAvailabilityZone,
    hInstances,
    hAllocationTime,
    hMemberOfServiceLinkedResourceGroup,
    hHostRecovery,
    hAutoPlacement,
    hTags,

    -- ** HostInstance
    HostInstance (..),
    mkHostInstance,
    hiInstanceId,
    hiInstanceType,
    hiOwnerId,

    -- ** HostOffering
    HostOffering (..),
    mkHostOffering,
    hoInstanceFamily,
    hoCurrencyCode,
    hoHourlyPrice,
    hoUpfrontPrice,
    hoOfferingId,
    hoDuration,
    hoPaymentOption,

    -- ** HostProperties
    HostProperties (..),
    mkHostProperties,
    hpInstanceFamily,
    hpInstanceType,
    hpTotalVCPUs,
    hpCores,
    hpSockets,

    -- ** HostReservation
    HostReservation (..),
    mkHostReservation,
    hrState,
    hrInstanceFamily,
    hrCurrencyCode,
    hrHostReservationId,
    hrStart,
    hrHourlyPrice,
    hrCount,
    hrUpfrontPrice,
    hrEnd,
    hrHostIdSet,
    hrOfferingId,
    hrDuration,
    hrTags,
    hrPaymentOption,

    -- ** IAMInstanceProfile
    IAMInstanceProfile (..),
    mkIAMInstanceProfile,
    iapARN,
    iapId,

    -- ** IAMInstanceProfileAssociation
    IAMInstanceProfileAssociation (..),
    mkIAMInstanceProfileAssociation,
    iapaAssociationId,
    iapaInstanceId,
    iapaState,
    iapaIAMInstanceProfile,
    iapaTimestamp,

    -- ** IAMInstanceProfileSpecification
    IAMInstanceProfileSpecification (..),
    mkIAMInstanceProfileSpecification,
    iapsARN,
    iapsName,

    -- ** ICMPTypeCode
    ICMPTypeCode (..),
    mkICMPTypeCode,
    itcCode,
    itcType,

    -- ** IKEVersionsListValue
    IKEVersionsListValue (..),
    mkIKEVersionsListValue,
    ikevlvValue,

    -- ** IKEVersionsRequestListValue
    IKEVersionsRequestListValue (..),
    mkIKEVersionsRequestListValue,
    ikevrlvValue,

    -- ** IPPermission
    IPPermission (..),
    mkIPPermission,
    ipFromPort,
    ipUserIdGroupPairs,
    ipPrefixListIds,
    ipIPProtocol,
    ipToPort,
    ipIPv6Ranges,
    ipIPRanges,

    -- ** IPRange
    IPRange (..),
    mkIPRange,
    iprCidrIP,
    iprDescription,

    -- ** IPv6CidrAssociation
    IPv6CidrAssociation (..),
    mkIPv6CidrAssociation,
    icaAssociatedResource,
    icaIPv6Cidr,

    -- ** IPv6CidrBlock
    IPv6CidrBlock (..),
    mkIPv6CidrBlock,
    icbIPv6CidrBlock,

    -- ** IPv6Pool
    IPv6Pool (..),
    mkIPv6Pool,
    ipPoolCidrBlocks,
    ipPoolId,
    ipDescription,
    ipTags,

    -- ** IPv6Range
    IPv6Range (..),
    mkIPv6Range,
    irCidrIPv6,
    irDescription,

    -- ** IdFormat
    IdFormat (..),
    mkIdFormat,
    ifUseLongIds,
    ifDeadline,
    ifResource,

    -- ** Image
    Image (..),
    mkImage,
    iState,
    iVirtualizationType,
    iHypervisor,
    iPlatform,
    iImageLocation,
    iPlatformDetails,
    iEnaSupport,
    iImageOwnerAlias,
    iUsageOperation,
    iRAMDiskId,
    iKernelId,
    iRootDeviceName,
    iSRIOVNetSupport,
    iOwnerId,
    iImageType,
    iName,
    iImageId,
    iArchitecture,
    iCreationDate,
    iProductCodes,
    iStateReason,
    iRootDeviceType,
    iDescription,
    iBlockDeviceMappings,
    iTags,
    iPublic,

    -- ** ImageDiskContainer
    ImageDiskContainer (..),
    mkImageDiskContainer,
    idcFormat,
    idcURL,
    idcDeviceName,
    idcUserBucket,
    idcDescription,
    idcSnapshotId,

    -- ** ImportImageLicenseConfigurationRequest
    ImportImageLicenseConfigurationRequest (..),
    mkImportImageLicenseConfigurationRequest,
    iilcrLicenseConfigurationARN,

    -- ** ImportImageLicenseConfigurationResponse
    ImportImageLicenseConfigurationResponse (..),
    mkImportImageLicenseConfigurationResponse,
    iilcLicenseConfigurationARN,

    -- ** ImportImageTask
    ImportImageTask (..),
    mkImportImageTask,
    iitStatus,
    iitHypervisor,
    iitPlatform,
    iitProgress,
    iitLicenseSpecifications,
    iitLicenseType,
    iitSnapshotDetails,
    iitEncrypted,
    iitKMSKeyId,
    iitStatusMessage,
    iitImageId,
    iitImportTaskId,
    iitArchitecture,
    iitDescription,
    iitTags,

    -- ** ImportInstanceLaunchSpecification
    ImportInstanceLaunchSpecification (..),
    mkImportInstanceLaunchSpecification,
    iilsAdditionalInfo,
    iilsGroupNames,
    iilsSubnetId,
    iilsInstanceType,
    iilsGroupIds,
    iilsUserData,
    iilsMonitoring,
    iilsPrivateIPAddress,
    iilsInstanceInitiatedShutdownBehavior,
    iilsArchitecture,
    iilsPlacement,

    -- ** ImportInstanceTaskDetails
    ImportInstanceTaskDetails (..),
    mkImportInstanceTaskDetails,
    iitdInstanceId,
    iitdPlatform,
    iitdVolumes,
    iitdDescription,

    -- ** ImportInstanceVolumeDetailItem
    ImportInstanceVolumeDetailItem (..),
    mkImportInstanceVolumeDetailItem,
    iivdiStatus,
    iivdiBytesConverted,
    iivdiImage,
    iivdiVolume,
    iivdiAvailabilityZone,
    iivdiStatusMessage,
    iivdiDescription,

    -- ** ImportSnapshotTask
    ImportSnapshotTask (..),
    mkImportSnapshotTask,
    istSnapshotTaskDetail,
    istImportTaskId,
    istDescription,
    istTags,

    -- ** ImportVolumeTaskDetails
    ImportVolumeTaskDetails (..),
    mkImportVolumeTaskDetails,
    ivtdBytesConverted,
    ivtdImage,
    ivtdVolume,
    ivtdAvailabilityZone,
    ivtdDescription,

    -- ** InferenceAcceleratorInfo
    InferenceAcceleratorInfo (..),
    mkInferenceAcceleratorInfo,
    iaiAccelerators,

    -- ** InferenceDeviceInfo
    InferenceDeviceInfo (..),
    mkInferenceDeviceInfo,
    idiManufacturer,
    idiCount,
    idiName,

    -- ** Instance
    Instance (..),
    mkInstance,
    ifInstanceId,
    ifState,
    ifVirtualizationType,
    ifPublicDNSName,
    ifHypervisor,
    ifPlatform,
    ifSecurityGroups,
    ifClientToken,
    ifEnaSupport,
    ifSourceDestCheck,
    ifElasticGpuAssociations,
    ifVPCId,
    ifKeyName,
    ifLaunchTime,
    ifNetworkInterfaces,
    ifOutpostARN,
    ifEnclaveOptions,
    ifRAMDiskId,
    ifCPUOptions,
    ifSubnetId,
    ifKernelId,
    ifRootDeviceName,
    ifCapacityReservationId,
    ifInstanceType,
    ifCapacityReservationSpecification,
    ifSRIOVNetSupport,
    ifEBSOptimized,
    ifMonitoring,
    ifStateTransitionReason,
    ifHibernationOptions,
    ifInstanceLifecycle,
    ifIAMInstanceProfile,
    ifImageId,
    ifPrivateIPAddress,
    ifMetadataOptions,
    ifArchitecture,
    ifProductCodes,
    ifSpotInstanceRequestId,
    ifLicenses,
    ifElasticInferenceAcceleratorAssociations,
    ifPrivateDNSName,
    ifStateReason,
    ifRootDeviceType,
    ifBlockDeviceMappings,
    ifAMILaunchIndex,
    ifPublicIPAddress,
    ifPlacement,
    ifTags,

    -- ** InstanceBlockDeviceMapping
    InstanceBlockDeviceMapping (..),
    mkInstanceBlockDeviceMapping,
    ibdmEBS,
    ibdmDeviceName,

    -- ** InstanceBlockDeviceMappingSpecification
    InstanceBlockDeviceMappingSpecification (..),
    mkInstanceBlockDeviceMappingSpecification,
    ibdmsVirtualName,
    ibdmsNoDevice,
    ibdmsEBS,
    ibdmsDeviceName,

    -- ** InstanceCapacity
    InstanceCapacity (..),
    mkInstanceCapacity,
    icAvailableCapacity,
    icInstanceType,
    icTotalCapacity,

    -- ** InstanceCount
    InstanceCount (..),
    mkInstanceCount,
    icState,
    icInstanceCount,

    -- ** InstanceCreditSpecification
    InstanceCreditSpecification (..),
    mkInstanceCreditSpecification,
    icsInstanceId,
    icsCPUCredits,

    -- ** InstanceCreditSpecificationRequest
    InstanceCreditSpecificationRequest (..),
    mkInstanceCreditSpecificationRequest,
    icsrInstanceId,
    icsrCPUCredits,

    -- ** InstanceExportDetails
    InstanceExportDetails (..),
    mkInstanceExportDetails,
    iedTargetEnvironment,
    iedInstanceId,

    -- ** InstanceFamilyCreditSpecification
    InstanceFamilyCreditSpecification (..),
    mkInstanceFamilyCreditSpecification,
    ifcsInstanceFamily,
    ifcsCPUCredits,

    -- ** InstanceIPv6Address
    InstanceIPv6Address (..),
    mkInstanceIPv6Address,
    iiaIPv6Address,

    -- ** InstanceIPv6AddressRequest
    InstanceIPv6AddressRequest (..),
    mkInstanceIPv6AddressRequest,
    iiarIPv6Address,

    -- ** InstanceMarketOptionsRequest
    InstanceMarketOptionsRequest (..),
    mkInstanceMarketOptionsRequest,
    imorMarketType,
    imorSpotOptions,

    -- ** InstanceMetadataOptionsRequest
    InstanceMetadataOptionsRequest (..),
    mkInstanceMetadataOptionsRequest,
    imorHTTPEndpoint,
    imorHTTPPutResponseHopLimit,
    imorHTTPTokens,

    -- ** InstanceMetadataOptionsResponse
    InstanceMetadataOptionsResponse (..),
    mkInstanceMetadataOptionsResponse,
    imoState,
    imoHTTPEndpoint,
    imoHTTPPutResponseHopLimit,
    imoHTTPTokens,

    -- ** InstanceMonitoring
    InstanceMonitoring (..),
    mkInstanceMonitoring,
    imInstanceId,
    imMonitoring,

    -- ** InstanceNetworkInterface
    InstanceNetworkInterface (..),
    mkInstanceNetworkInterface,
    iniGroups,
    iniStatus,
    iniPrivateIPAddresses,
    iniSourceDestCheck,
    iniInterfaceType,
    iniVPCId,
    iniNetworkInterfaceId,
    iniSubnetId,
    iniMACAddress,
    iniAttachment,
    iniOwnerId,
    iniPrivateIPAddress,
    iniPrivateDNSName,
    iniDescription,
    iniAssociation,
    iniIPv6Addresses,

    -- ** InstanceNetworkInterfaceAssociation
    InstanceNetworkInterfaceAssociation (..),
    mkInstanceNetworkInterfaceAssociation,
    iniaPublicDNSName,
    iniaCarrierIP,
    iniaIPOwnerId,
    iniaPublicIP,

    -- ** InstanceNetworkInterfaceAttachment
    InstanceNetworkInterfaceAttachment (..),
    mkInstanceNetworkInterfaceAttachment,
    iniaStatus,
    iniaDeleteOnTermination,
    iniaAttachmentId,
    iniaNetworkCardIndex,
    iniaAttachTime,
    iniaDeviceIndex,

    -- ** InstanceNetworkInterfaceSpecification
    InstanceNetworkInterfaceSpecification (..),
    mkInstanceNetworkInterfaceSpecification,
    inisGroups,
    inisPrivateIPAddresses,
    inisDeleteOnTermination,
    inisAssociateCarrierIPAddress,
    inisAssociatePublicIPAddress,
    inisInterfaceType,
    inisNetworkInterfaceId,
    inisSubnetId,
    inisIPv6AddressCount,
    inisNetworkCardIndex,
    inisPrivateIPAddress,
    inisSecondaryPrivateIPAddressCount,
    inisDescription,
    inisDeviceIndex,
    inisIPv6Addresses,

    -- ** InstancePrivateIPAddress
    InstancePrivateIPAddress (..),
    mkInstancePrivateIPAddress,
    ipiaPrimary,
    ipiaPrivateIPAddress,
    ipiaPrivateDNSName,
    ipiaAssociation,

    -- ** InstanceSpecification
    InstanceSpecification (..),
    mkInstanceSpecification,
    isInstanceId,
    isExcludeBootVolume,

    -- ** InstanceState
    InstanceState (..),
    mkInstanceState,
    isName,
    isCode,

    -- ** InstanceStateChange
    InstanceStateChange (..),
    mkInstanceStateChange,
    iscInstanceId,
    iscCurrentState,
    iscPreviousState,

    -- ** InstanceStatus
    InstanceStatus (..),
    mkInstanceStatus,
    iInstanceId,
    iOutpostARN,
    iSystemStatus,
    iEvents,
    iAvailabilityZone,
    iInstanceStatus,
    iInstanceState,

    -- ** InstanceStatusDetails
    InstanceStatusDetails (..),
    mkInstanceStatusDetails,
    isdStatus,
    isdImpairedSince,
    isdName,

    -- ** InstanceStatusEvent
    InstanceStatusEvent (..),
    mkInstanceStatusEvent,
    iseNotBefore,
    iseCode,
    iseInstanceEventId,
    iseDescription,
    iseNotBeforeDeadline,
    iseNotAfter,

    -- ** InstanceStatusSummary
    InstanceStatusSummary (..),
    mkInstanceStatusSummary,
    issStatus,
    issDetails,

    -- ** InstanceStorageInfo
    InstanceStorageInfo (..),
    mkInstanceStorageInfo,
    isiTotalSizeInGB,
    isiNvmeSupport,
    isiDisks,

    -- ** InstanceTagNotificationAttribute
    InstanceTagNotificationAttribute (..),
    mkInstanceTagNotificationAttribute,
    itnaIncludeAllTagsOfInstance,
    itnaInstanceTagKeys,

    -- ** InstanceTypeInfo
    InstanceTypeInfo (..),
    mkInstanceTypeInfo,
    itiHypervisor,
    itiCurrentGeneration,
    itiMemoryInfo,
    itiPlacementGroupInfo,
    itiSupportedRootDeviceTypes,
    itiSupportedUsageClasses,
    itiInstanceStorageSupported,
    itiFpgaInfo,
    itiBurstablePerformanceSupported,
    itiInstanceType,
    itiGpuInfo,
    itiSupportedVirtualizationTypes,
    itiEBSInfo,
    itiAutoRecoverySupported,
    itiInferenceAcceleratorInfo,
    itiBareMetal,
    itiNetworkInfo,
    itiProcessorInfo,
    itiFreeTierEligible,
    itiVCPUInfo,
    itiInstanceStorageInfo,
    itiDedicatedHostsSupported,
    itiHibernationSupported,

    -- ** InstanceTypeOffering
    InstanceTypeOffering (..),
    mkInstanceTypeOffering,
    itoLocation,
    itoInstanceType,
    itoLocationType,

    -- ** InstanceUsage
    InstanceUsage (..),
    mkInstanceUsage,
    iuAccountId,
    iuUsedInstanceCount,

    -- ** InternetGateway
    InternetGateway (..),
    mkInternetGateway,
    igAttachments,
    igOwnerId,
    igInternetGatewayId,
    igTags,

    -- ** InternetGatewayAttachment
    InternetGatewayAttachment (..),
    mkInternetGatewayAttachment,
    igaState,
    igaVPCId,

    -- ** KeyPairInfo
    KeyPairInfo (..),
    mkKeyPairInfo,
    kpiKeyFingerprint,
    kpiKeyName,
    kpiKeyPairId,
    kpiTags,

    -- ** LastError
    LastError (..),
    mkLastError,
    leCode,
    leMessage,

    -- ** LaunchPermission
    LaunchPermission (..),
    mkLaunchPermission,
    lGroup,
    lUserId,

    -- ** LaunchPermissionModifications
    LaunchPermissionModifications (..),
    mkLaunchPermissionModifications,
    lpmRemove,
    lpmAdd,

    -- ** LaunchSpecification
    LaunchSpecification (..),
    mkLaunchSpecification,
    lsSecurityGroups,
    lsKeyName,
    lsNetworkInterfaces,
    lsRAMDiskId,
    lsSubnetId,
    lsKernelId,
    lsInstanceType,
    lsEBSOptimized,
    lsUserData,
    lsMonitoring,
    lsIAMInstanceProfile,
    lsImageId,
    lsAddressingType,
    lsBlockDeviceMappings,
    lsPlacement,

    -- ** LaunchTemplate
    LaunchTemplate (..),
    mkLaunchTemplate,
    ltLaunchTemplateName,
    ltLatestVersionNumber,
    ltLaunchTemplateId,
    ltCreatedBy,
    ltDefaultVersionNumber,
    ltCreateTime,
    ltTags,

    -- ** LaunchTemplateAndOverridesResponse
    LaunchTemplateAndOverridesResponse (..),
    mkLaunchTemplateAndOverridesResponse,
    ltaoOverrides,
    ltaoLaunchTemplateSpecification,

    -- ** LaunchTemplateBlockDeviceMapping
    LaunchTemplateBlockDeviceMapping (..),
    mkLaunchTemplateBlockDeviceMapping,
    ltbdmVirtualName,
    ltbdmNoDevice,
    ltbdmEBS,
    ltbdmDeviceName,

    -- ** LaunchTemplateBlockDeviceMappingRequest
    LaunchTemplateBlockDeviceMappingRequest (..),
    mkLaunchTemplateBlockDeviceMappingRequest,
    ltbdmrVirtualName,
    ltbdmrNoDevice,
    ltbdmrEBS,
    ltbdmrDeviceName,

    -- ** LaunchTemplateCPUOptions
    LaunchTemplateCPUOptions (..),
    mkLaunchTemplateCPUOptions,
    ltcoCoreCount,
    ltcoThreadsPerCore,

    -- ** LaunchTemplateCPUOptionsRequest
    LaunchTemplateCPUOptionsRequest (..),
    mkLaunchTemplateCPUOptionsRequest,
    ltcorCoreCount,
    ltcorThreadsPerCore,

    -- ** LaunchTemplateCapacityReservationSpecificationRequest
    LaunchTemplateCapacityReservationSpecificationRequest (..),
    mkLaunchTemplateCapacityReservationSpecificationRequest,
    ltcrsrCapacityReservationTarget,
    ltcrsrCapacityReservationPreference,

    -- ** LaunchTemplateCapacityReservationSpecificationResponse
    LaunchTemplateCapacityReservationSpecificationResponse (..),
    mkLaunchTemplateCapacityReservationSpecificationResponse,
    ltcrsCapacityReservationTarget,
    ltcrsCapacityReservationPreference,

    -- ** LaunchTemplateConfig
    LaunchTemplateConfig (..),
    mkLaunchTemplateConfig,
    ltcOverrides,
    ltcLaunchTemplateSpecification,

    -- ** LaunchTemplateEBSBlockDevice
    LaunchTemplateEBSBlockDevice (..),
    mkLaunchTemplateEBSBlockDevice,
    ltebdDeleteOnTermination,
    ltebdVolumeSize,
    ltebdIOPS,
    ltebdEncrypted,
    ltebdKMSKeyId,
    ltebdVolumeType,
    ltebdSnapshotId,

    -- ** LaunchTemplateEBSBlockDeviceRequest
    LaunchTemplateEBSBlockDeviceRequest (..),
    mkLaunchTemplateEBSBlockDeviceRequest,
    ltebdrDeleteOnTermination,
    ltebdrVolumeSize,
    ltebdrIOPS,
    ltebdrEncrypted,
    ltebdrKMSKeyId,
    ltebdrVolumeType,
    ltebdrSnapshotId,

    -- ** LaunchTemplateElasticInferenceAccelerator
    LaunchTemplateElasticInferenceAccelerator (..),
    mkLaunchTemplateElasticInferenceAccelerator,
    lteiaCount,
    lteiaType,

    -- ** LaunchTemplateElasticInferenceAcceleratorResponse
    LaunchTemplateElasticInferenceAcceleratorResponse (..),
    mkLaunchTemplateElasticInferenceAcceleratorResponse,
    lCount,
    lType,

    -- ** LaunchTemplateEnclaveOptions
    LaunchTemplateEnclaveOptions (..),
    mkLaunchTemplateEnclaveOptions,
    lteoEnabled,

    -- ** LaunchTemplateEnclaveOptionsRequest
    LaunchTemplateEnclaveOptionsRequest (..),
    mkLaunchTemplateEnclaveOptionsRequest,
    lteorEnabled,

    -- ** LaunchTemplateHibernationOptions
    LaunchTemplateHibernationOptions (..),
    mkLaunchTemplateHibernationOptions,
    lthoConfigured,

    -- ** LaunchTemplateHibernationOptionsRequest
    LaunchTemplateHibernationOptionsRequest (..),
    mkLaunchTemplateHibernationOptionsRequest,
    lthorConfigured,

    -- ** LaunchTemplateIAMInstanceProfileSpecification
    LaunchTemplateIAMInstanceProfileSpecification (..),
    mkLaunchTemplateIAMInstanceProfileSpecification,
    ltiapsARN,
    ltiapsName,

    -- ** LaunchTemplateIAMInstanceProfileSpecificationRequest
    LaunchTemplateIAMInstanceProfileSpecificationRequest (..),
    mkLaunchTemplateIAMInstanceProfileSpecificationRequest,
    ltiapsrARN,
    ltiapsrName,

    -- ** LaunchTemplateInstanceMarketOptions
    LaunchTemplateInstanceMarketOptions (..),
    mkLaunchTemplateInstanceMarketOptions,
    ltimoMarketType,
    ltimoSpotOptions,

    -- ** LaunchTemplateInstanceMarketOptionsRequest
    LaunchTemplateInstanceMarketOptionsRequest (..),
    mkLaunchTemplateInstanceMarketOptionsRequest,
    ltimorMarketType,
    ltimorSpotOptions,

    -- ** LaunchTemplateInstanceMetadataOptions
    LaunchTemplateInstanceMetadataOptions (..),
    mkLaunchTemplateInstanceMetadataOptions,
    ltimoState,
    ltimoHTTPEndpoint,
    ltimoHTTPPutResponseHopLimit,
    ltimoHTTPTokens,

    -- ** LaunchTemplateInstanceMetadataOptionsRequest
    LaunchTemplateInstanceMetadataOptionsRequest (..),
    mkLaunchTemplateInstanceMetadataOptionsRequest,
    ltimorHTTPEndpoint,
    ltimorHTTPPutResponseHopLimit,
    ltimorHTTPTokens,

    -- ** LaunchTemplateInstanceNetworkInterfaceSpecification
    LaunchTemplateInstanceNetworkInterfaceSpecification (..),
    mkLaunchTemplateInstanceNetworkInterfaceSpecification,
    ltinisGroups,
    ltinisPrivateIPAddresses,
    ltinisDeleteOnTermination,
    ltinisAssociateCarrierIPAddress,
    ltinisAssociatePublicIPAddress,
    ltinisInterfaceType,
    ltinisNetworkInterfaceId,
    ltinisSubnetId,
    ltinisIPv6AddressCount,
    ltinisNetworkCardIndex,
    ltinisPrivateIPAddress,
    ltinisSecondaryPrivateIPAddressCount,
    ltinisDescription,
    ltinisDeviceIndex,
    ltinisIPv6Addresses,

    -- ** LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
    LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (..),
    mkLaunchTemplateInstanceNetworkInterfaceSpecificationRequest,
    ltinisrGroups,
    ltinisrPrivateIPAddresses,
    ltinisrDeleteOnTermination,
    ltinisrAssociateCarrierIPAddress,
    ltinisrAssociatePublicIPAddress,
    ltinisrInterfaceType,
    ltinisrNetworkInterfaceId,
    ltinisrSubnetId,
    ltinisrIPv6AddressCount,
    ltinisrNetworkCardIndex,
    ltinisrPrivateIPAddress,
    ltinisrSecondaryPrivateIPAddressCount,
    ltinisrDescription,
    ltinisrDeviceIndex,
    ltinisrIPv6Addresses,

    -- ** LaunchTemplateLicenseConfiguration
    LaunchTemplateLicenseConfiguration (..),
    mkLaunchTemplateLicenseConfiguration,
    ltlcLicenseConfigurationARN,

    -- ** LaunchTemplateLicenseConfigurationRequest
    LaunchTemplateLicenseConfigurationRequest (..),
    mkLaunchTemplateLicenseConfigurationRequest,
    ltlcrLicenseConfigurationARN,

    -- ** LaunchTemplateOverrides
    LaunchTemplateOverrides (..),
    mkLaunchTemplateOverrides,
    ltoPriority,
    ltoSpotPrice,
    ltoWeightedCapacity,
    ltoSubnetId,
    ltoInstanceType,
    ltoAvailabilityZone,

    -- ** LaunchTemplatePlacement
    LaunchTemplatePlacement (..),
    mkLaunchTemplatePlacement,
    ltpAffinity,
    ltpHostId,
    ltpPartitionNumber,
    ltpSpreadDomain,
    ltpAvailabilityZone,
    ltpTenancy,
    ltpGroupName,
    ltpHostResourceGroupARN,

    -- ** LaunchTemplatePlacementRequest
    LaunchTemplatePlacementRequest (..),
    mkLaunchTemplatePlacementRequest,
    ltprAffinity,
    ltprHostId,
    ltprPartitionNumber,
    ltprSpreadDomain,
    ltprAvailabilityZone,
    ltprTenancy,
    ltprGroupName,
    ltprHostResourceGroupARN,

    -- ** LaunchTemplateSpecification
    LaunchTemplateSpecification (..),
    mkLaunchTemplateSpecification,
    ltsLaunchTemplateName,
    ltsLaunchTemplateId,
    ltsVersion,

    -- ** LaunchTemplateSpotMarketOptions
    LaunchTemplateSpotMarketOptions (..),
    mkLaunchTemplateSpotMarketOptions,
    ltsmoBlockDurationMinutes,
    ltsmoInstanceInterruptionBehavior,
    ltsmoValidUntil,
    ltsmoSpotInstanceType,
    ltsmoMaxPrice,

    -- ** LaunchTemplateSpotMarketOptionsRequest
    LaunchTemplateSpotMarketOptionsRequest (..),
    mkLaunchTemplateSpotMarketOptionsRequest,
    ltsmorBlockDurationMinutes,
    ltsmorInstanceInterruptionBehavior,
    ltsmorValidUntil,
    ltsmorSpotInstanceType,
    ltsmorMaxPrice,

    -- ** LaunchTemplateTagSpecification
    LaunchTemplateTagSpecification (..),
    mkLaunchTemplateTagSpecification,
    lttsResourceType,
    lttsTags,

    -- ** LaunchTemplateTagSpecificationRequest
    LaunchTemplateTagSpecificationRequest (..),
    mkLaunchTemplateTagSpecificationRequest,
    lttsrResourceType,
    lttsrTags,

    -- ** LaunchTemplateVersion
    LaunchTemplateVersion (..),
    mkLaunchTemplateVersion,
    ltvLaunchTemplateName,
    ltvLaunchTemplateId,
    ltvCreatedBy,
    ltvDefaultVersion,
    ltvVersionNumber,
    ltvVersionDescription,
    ltvLaunchTemplateData,
    ltvCreateTime,

    -- ** LaunchTemplatesMonitoring
    LaunchTemplatesMonitoring (..),
    mkLaunchTemplatesMonitoring,
    ltmEnabled,

    -- ** LaunchTemplatesMonitoringRequest
    LaunchTemplatesMonitoringRequest (..),
    mkLaunchTemplatesMonitoringRequest,
    ltmrEnabled,

    -- ** LicenseConfiguration
    LicenseConfiguration (..),
    mkLicenseConfiguration,
    lcLicenseConfigurationARN,

    -- ** LicenseConfigurationRequest
    LicenseConfigurationRequest (..),
    mkLicenseConfigurationRequest,
    lcrLicenseConfigurationARN,

    -- ** LoadBalancersConfig
    LoadBalancersConfig (..),
    mkLoadBalancersConfig,
    lbcClassicLoadBalancersConfig,
    lbcTargetGroupsConfig,

    -- ** LoadPermission
    LoadPermission (..),
    mkLoadPermission,
    lpGroup,
    lpUserId,

    -- ** LoadPermissionModifications
    LoadPermissionModifications (..),
    mkLoadPermissionModifications,
    lRemove,
    lAdd,

    -- ** LoadPermissionRequest
    LoadPermissionRequest (..),
    mkLoadPermissionRequest,
    lprGroup,
    lprUserId,

    -- ** LocalGateway
    LocalGateway (..),
    mkLocalGateway,
    lgState,
    lgLocalGatewayId,
    lgOutpostARN,
    lgOwnerId,
    lgTags,

    -- ** LocalGatewayRoute
    LocalGatewayRoute (..),
    mkLocalGatewayRoute,
    lgrState,
    lgrLocalGatewayRouteTableARN,
    lgrOwnerId,
    lgrLocalGatewayRouteTableId,
    lgrType,
    lgrLocalGatewayVirtualInterfaceGroupId,
    lgrDestinationCidrBlock,

    -- ** LocalGatewayRouteTable
    LocalGatewayRouteTable (..),
    mkLocalGatewayRouteTable,
    lgrtState,
    lgrtLocalGatewayRouteTableARN,
    lgrtLocalGatewayId,
    lgrtOutpostARN,
    lgrtOwnerId,
    lgrtLocalGatewayRouteTableId,
    lgrtTags,

    -- ** LocalGatewayRouteTableVPCAssociation
    LocalGatewayRouteTableVPCAssociation (..),
    mkLocalGatewayRouteTableVPCAssociation,
    lgrtvaState,
    lgrtvaLocalGatewayRouteTableARN,
    lgrtvaVPCId,
    lgrtvaLocalGatewayId,
    lgrtvaLocalGatewayRouteTableVPCAssociationId,
    lgrtvaOwnerId,
    lgrtvaLocalGatewayRouteTableId,
    lgrtvaTags,

    -- ** LocalGatewayRouteTableVirtualInterfaceGroupAssociation
    LocalGatewayRouteTableVirtualInterfaceGroupAssociation (..),
    mkLocalGatewayRouteTableVirtualInterfaceGroupAssociation,
    lgrtvigaState,
    lgrtvigaLocalGatewayRouteTableARN,
    lgrtvigaLocalGatewayId,
    lgrtvigaOwnerId,
    lgrtvigaLocalGatewayRouteTableId,
    lgrtvigaLocalGatewayRouteTableVirtualInterfaceGroupAssociationId,
    lgrtvigaLocalGatewayVirtualInterfaceGroupId,
    lgrtvigaTags,

    -- ** LocalGatewayVirtualInterface
    LocalGatewayVirtualInterface (..),
    mkLocalGatewayVirtualInterface,
    lgviLocalGatewayVirtualInterfaceId,
    lgviLocalBGPASN,
    lgviVLAN,
    lgviLocalGatewayId,
    lgviLocalAddress,
    lgviPeerBGPASN,
    lgviOwnerId,
    lgviPeerAddress,
    lgviTags,

    -- ** LocalGatewayVirtualInterfaceGroup
    LocalGatewayVirtualInterfaceGroup (..),
    mkLocalGatewayVirtualInterfaceGroup,
    lgvigLocalGatewayId,
    lgvigOwnerId,
    lgvigLocalGatewayVirtualInterfaceIds,
    lgvigLocalGatewayVirtualInterfaceGroupId,
    lgvigTags,

    -- ** ManagedPrefixList
    ManagedPrefixList (..),
    mkManagedPrefixList,
    mplStateMessage,
    mplState,
    mplPrefixListARN,
    mplAddressFamily,
    mplOwnerId,
    mplPrefixListId,
    mplVersion,
    mplPrefixListName,
    mplMaxEntries,
    mplTags,

    -- ** MemoryInfo
    MemoryInfo (..),
    mkMemoryInfo,
    miSizeInMiB,

    -- ** ModifyTransitGatewayOptions
    ModifyTransitGatewayOptions (..),
    mkModifyTransitGatewayOptions,
    mtgoVPNEcmpSupport,
    mtgoAutoAcceptSharedAttachments,
    mtgoPropagationDefaultRouteTableId,
    mtgoDefaultRouteTableAssociation,
    mtgoAssociationDefaultRouteTableId,
    mtgoDefaultRouteTablePropagation,
    mtgoDNSSupport,

    -- ** ModifyTransitGatewayVPCAttachmentRequestOptions
    ModifyTransitGatewayVPCAttachmentRequestOptions (..),
    mkModifyTransitGatewayVPCAttachmentRequestOptions,
    mtgvaroIPv6Support,
    mtgvaroApplianceModeSupport,
    mtgvaroDNSSupport,

    -- ** ModifyVPNTunnelOptionsSpecification
    ModifyVPNTunnelOptionsSpecification (..),
    mkModifyVPNTunnelOptionsSpecification,
    mvtosReplayWindowSize,
    mvtosDPDTimeoutAction,
    mvtosRekeyFuzzPercentage,
    mvtosPhase1LifetimeSeconds,
    mvtosIKEVersions,
    mvtosPhase2IntegrityAlgorithms,
    mvtosPhase2LifetimeSeconds,
    mvtosPhase1EncryptionAlgorithms,
    mvtosPhase1DHGroupNumbers,
    mvtosPhase1IntegrityAlgorithms,
    mvtosRekeyMarginTimeSeconds,
    mvtosDPDTimeoutSeconds,
    mvtosTunnelInsideCidr,
    mvtosStartupAction,
    mvtosPhase2EncryptionAlgorithms,
    mvtosPhase2DHGroupNumbers,
    mvtosPreSharedKey,
    mvtosTunnelInsideIPv6Cidr,

    -- ** Monitoring
    Monitoring (..),
    mkMonitoring,
    mState,

    -- ** MovingAddressStatus
    MovingAddressStatus (..),
    mkMovingAddressStatus,
    masMoveStatus,
    masPublicIP,

    -- ** NatGateway
    NatGateway (..),
    mkNatGateway,
    ngState,
    ngFailureCode,
    ngVPCId,
    ngFailureMessage,
    ngNatGatewayId,
    ngSubnetId,
    ngDeleteTime,
    ngProvisionedBandwidth,
    ngNatGatewayAddresses,
    ngCreateTime,
    ngTags,

    -- ** NatGatewayAddress
    NatGatewayAddress (..),
    mkNatGatewayAddress,
    ngaPrivateIP,
    ngaAllocationId,
    ngaNetworkInterfaceId,
    ngaPublicIP,

    -- ** NetworkACL
    NetworkACL (..),
    mkNetworkACL,
    naEntries,
    naNetworkACLId,
    naVPCId,
    naOwnerId,
    naAssociations,
    naTags,
    naIsDefault,

    -- ** NetworkACLAssociation
    NetworkACLAssociation (..),
    mkNetworkACLAssociation,
    naaNetworkACLId,
    naaSubnetId,
    naaNetworkACLAssociationId,

    -- ** NetworkACLEntry
    NetworkACLEntry (..),
    mkNetworkACLEntry,
    naeIPv6CidrBlock,
    naeICMPTypeCode,
    naeRuleNumber,
    naeRuleAction,
    naeProtocol,
    naePortRange,
    naeCidrBlock,
    naeEgress,

    -- ** NetworkCardInfo
    NetworkCardInfo (..),
    mkNetworkCardInfo,
    nciMaximumNetworkInterfaces,
    nciNetworkPerformance,
    nciNetworkCardIndex,

    -- ** NetworkInfo
    NetworkInfo (..),
    mkNetworkInfo,
    niEfaSupported,
    niIPv6Supported,
    niEnaSupport,
    niMaximumNetworkInterfaces,
    niIPv6AddressesPerInterface,
    niNetworkPerformance,
    niMaximumNetworkCards,
    niNetworkCards,
    niDefaultNetworkCardIndex,
    niIPv4AddressesPerInterface,

    -- ** NetworkInterface
    NetworkInterface (..),
    mkNetworkInterface,
    niGroups,
    niStatus,
    niPrivateIPAddresses,
    niSourceDestCheck,
    niInterfaceType,
    niVPCId,
    niTagSet,
    niRequesterManaged,
    niOutpostARN,
    niNetworkInterfaceId,
    niSubnetId,
    niMACAddress,
    niAttachment,
    niOwnerId,
    niAvailabilityZone,
    niPrivateIPAddress,
    niPrivateDNSName,
    niRequesterId,
    niDescription,
    niAssociation,
    niIPv6Addresses,

    -- ** NetworkInterfaceAssociation
    NetworkInterfaceAssociation (..),
    mkNetworkInterfaceAssociation,
    niaAssociationId,
    niaPublicDNSName,
    niaAllocationId,
    niaCarrierIP,
    niaIPOwnerId,
    niaCustomerOwnedIP,
    niaPublicIP,

    -- ** NetworkInterfaceAttachment
    NetworkInterfaceAttachment (..),
    mkNetworkInterfaceAttachment,
    niaInstanceId,
    niaStatus,
    niaDeleteOnTermination,
    niaAttachmentId,
    niaNetworkCardIndex,
    niaInstanceOwnerId,
    niaAttachTime,
    niaDeviceIndex,

    -- ** NetworkInterfaceAttachmentChanges
    NetworkInterfaceAttachmentChanges (..),
    mkNetworkInterfaceAttachmentChanges,
    niacDeleteOnTermination,
    niacAttachmentId,

    -- ** NetworkInterfaceIPv6Address
    NetworkInterfaceIPv6Address (..),
    mkNetworkInterfaceIPv6Address,
    niiaIPv6Address,

    -- ** NetworkInterfacePermission
    NetworkInterfacePermission (..),
    mkNetworkInterfacePermission,
    nipPermissionState,
    nipNetworkInterfacePermissionId,
    nipNetworkInterfaceId,
    nipAWSAccountId,
    nipAWSService,
    nipPermission,

    -- ** NetworkInterfacePermissionState
    NetworkInterfacePermissionState (..),
    mkNetworkInterfacePermissionState,
    nipsState,
    nipsStatusMessage,

    -- ** NetworkInterfacePrivateIPAddress
    NetworkInterfacePrivateIPAddress (..),
    mkNetworkInterfacePrivateIPAddress,
    nipiaPrimary,
    nipiaPrivateIPAddress,
    nipiaPrivateDNSName,
    nipiaAssociation,

    -- ** NewDHCPConfiguration
    NewDHCPConfiguration (..),
    mkNewDHCPConfiguration,
    ndcValues,
    ndcKey,

    -- ** OnDemandOptions
    OnDemandOptions (..),
    mkOnDemandOptions,
    odoCapacityReservationOptions,
    odoSingleAvailabilityZone,
    odoMaxTotalPrice,
    odoMinTargetCapacity,
    odoSingleInstanceType,
    odoAllocationStrategy,

    -- ** OnDemandOptionsRequest
    OnDemandOptionsRequest (..),
    mkOnDemandOptionsRequest,
    odorCapacityReservationOptions,
    odorSingleAvailabilityZone,
    odorMaxTotalPrice,
    odorMinTargetCapacity,
    odorSingleInstanceType,
    odorAllocationStrategy,

    -- ** PciId
    PciId (..),
    mkPciId,
    piSubsystemId,
    piDeviceId,
    piSubsystemVendorId,
    piVendorId,

    -- ** PeeringAttachmentStatus
    PeeringAttachmentStatus (..),
    mkPeeringAttachmentStatus,
    pasCode,
    pasMessage,

    -- ** PeeringConnectionOptions
    PeeringConnectionOptions (..),
    mkPeeringConnectionOptions,
    pcoAllowEgressFromLocalVPCToRemoteClassicLink,
    pcoAllowEgressFromLocalClassicLinkToRemoteVPC,
    pcoAllowDNSResolutionFromRemoteVPC,

    -- ** PeeringConnectionOptionsRequest
    PeeringConnectionOptionsRequest (..),
    mkPeeringConnectionOptionsRequest,
    pcorAllowEgressFromLocalVPCToRemoteClassicLink,
    pcorAllowEgressFromLocalClassicLinkToRemoteVPC,
    pcorAllowDNSResolutionFromRemoteVPC,

    -- ** PeeringTgwInfo
    PeeringTgwInfo (..),
    mkPeeringTgwInfo,
    ptiOwnerId,
    ptiTransitGatewayId,
    ptiRegion,

    -- ** Phase1DHGroupNumbersListValue
    Phase1DHGroupNumbersListValue (..),
    mkPhase1DHGroupNumbersListValue,
    pdhgnlvValue,

    -- ** Phase1DHGroupNumbersRequestListValue
    Phase1DHGroupNumbersRequestListValue (..),
    mkPhase1DHGroupNumbersRequestListValue,
    pdhgnrlvValue,

    -- ** Phase1EncryptionAlgorithmsListValue
    Phase1EncryptionAlgorithmsListValue (..),
    mkPhase1EncryptionAlgorithmsListValue,
    pealvfValue,

    -- ** Phase1EncryptionAlgorithmsRequestListValue
    Phase1EncryptionAlgorithmsRequestListValue (..),
    mkPhase1EncryptionAlgorithmsRequestListValue,
    pearlvValue,

    -- ** Phase1IntegrityAlgorithmsListValue
    Phase1IntegrityAlgorithmsListValue (..),
    mkPhase1IntegrityAlgorithmsListValue,
    pialvValue,

    -- ** Phase1IntegrityAlgorithmsRequestListValue
    Phase1IntegrityAlgorithmsRequestListValue (..),
    mkPhase1IntegrityAlgorithmsRequestListValue,
    piarlvfValue,

    -- ** Phase2DHGroupNumbersListValue
    Phase2DHGroupNumbersListValue (..),
    mkPhase2DHGroupNumbersListValue,
    pValue,

    -- ** Phase2DHGroupNumbersRequestListValue
    Phase2DHGroupNumbersRequestListValue (..),
    mkPhase2DHGroupNumbersRequestListValue,
    pdhgnrlvfValue,

    -- ** Phase2EncryptionAlgorithmsListValue
    Phase2EncryptionAlgorithmsListValue (..),
    mkPhase2EncryptionAlgorithmsListValue,
    pealvValue,

    -- ** Phase2EncryptionAlgorithmsRequestListValue
    Phase2EncryptionAlgorithmsRequestListValue (..),
    mkPhase2EncryptionAlgorithmsRequestListValue,
    pearlvfValue,

    -- ** Phase2IntegrityAlgorithmsListValue
    Phase2IntegrityAlgorithmsListValue (..),
    mkPhase2IntegrityAlgorithmsListValue,
    pialvfValue,

    -- ** Phase2IntegrityAlgorithmsRequestListValue
    Phase2IntegrityAlgorithmsRequestListValue (..),
    mkPhase2IntegrityAlgorithmsRequestListValue,
    piarlvValue,

    -- ** Placement
    Placement (..),
    mkPlacement,
    pfAffinity,
    pfHostId,
    pfPartitionNumber,
    pfSpreadDomain,
    pfAvailabilityZone,
    pfTenancy,
    pfGroupName,
    pfHostResourceGroupARN,

    -- ** PlacementGroup
    PlacementGroup (..),
    mkPlacementGroup,
    pgState,
    pgStrategy,
    pgGroupId,
    pgGroupName,
    pgPartitionCount,
    pgTags,

    -- ** PlacementGroupInfo
    PlacementGroupInfo (..),
    mkPlacementGroupInfo,
    pgiSupportedStrategies,

    -- ** PlacementResponse
    PlacementResponse (..),
    mkPlacementResponse,
    pGroupName,

    -- ** PoolCidrBlock
    PoolCidrBlock (..),
    mkPoolCidrBlock,
    pcbCidr,

    -- ** PortRange
    PortRange (..),
    mkPortRange,
    prTo,
    prFrom,

    -- ** PrefixList
    PrefixList (..),
    mkPrefixList,
    plCidrs,
    plPrefixListId,
    plPrefixListName,

    -- ** PrefixListAssociation
    PrefixListAssociation (..),
    mkPrefixListAssociation,
    plaResourceId,
    plaResourceOwner,

    -- ** PrefixListEntry
    PrefixListEntry (..),
    mkPrefixListEntry,
    pleCidr,
    pleDescription,

    -- ** PrefixListId
    PrefixListId (..),
    mkPrefixListId,
    pliPrefixListId,
    pliDescription,

    -- ** PriceSchedule
    PriceSchedule (..),
    mkPriceSchedule,
    psCurrencyCode,
    psTerm,
    psActive,
    psPrice,

    -- ** PriceScheduleSpecification
    PriceScheduleSpecification (..),
    mkPriceScheduleSpecification,
    pssCurrencyCode,
    pssTerm,
    pssPrice,

    -- ** PricingDetail
    PricingDetail (..),
    mkPricingDetail,
    pdCount,
    pdPrice,

    -- ** PrincipalIdFormat
    PrincipalIdFormat (..),
    mkPrincipalIdFormat,
    pifARN,
    pifStatuses,

    -- ** PrivateDNSDetails
    PrivateDNSDetails (..),
    mkPrivateDNSDetails,
    pddPrivateDNSName,

    -- ** PrivateDNSNameConfiguration
    PrivateDNSNameConfiguration (..),
    mkPrivateDNSNameConfiguration,
    pdncState,
    pdncValue,
    pdncName,
    pdncType,

    -- ** PrivateIPAddressSpecification
    PrivateIPAddressSpecification (..),
    mkPrivateIPAddressSpecification,
    piasPrimary,
    piasPrivateIPAddress,

    -- ** ProcessorInfo
    ProcessorInfo (..),
    mkProcessorInfo,
    piSupportedArchitectures,
    piSustainedClockSpeedInGhz,

    -- ** ProductCode
    ProductCode (..),
    mkProductCode,
    pcProductCodeType,
    pcProductCodeId,

    -- ** PropagatingVGW
    PropagatingVGW (..),
    mkPropagatingVGW,
    pvGatewayId,

    -- ** ProvisionedBandwidth
    ProvisionedBandwidth (..),
    mkProvisionedBandwidth,
    pbStatus,
    pbRequested,
    pbProvisioned,
    pbRequestTime,
    pbProvisionTime,

    -- ** PublicIPv4Pool
    PublicIPv4Pool (..),
    mkPublicIPv4Pool,
    pipTotalAddressCount,
    pipNetworkBorderGroup,
    pipTotalAvailableAddressCount,
    pipPoolAddressRanges,
    pipPoolId,
    pipDescription,
    pipTags,

    -- ** PublicIPv4PoolRange
    PublicIPv4PoolRange (..),
    mkPublicIPv4PoolRange,
    piprAvailableAddressCount,
    piprLastAddress,
    piprFirstAddress,
    piprAddressCount,

    -- ** Purchase
    Purchase (..),
    mkPurchase,
    pInstanceFamily,
    pCurrencyCode,
    pHostReservationId,
    pHourlyPrice,
    pUpfrontPrice,
    pHostIdSet,
    pDuration,
    pPaymentOption,

    -- ** PurchaseRequest
    PurchaseRequest (..),
    mkPurchaseRequest,
    prInstanceCount,
    prPurchaseToken,

    -- ** RecurringCharge
    RecurringCharge (..),
    mkRecurringCharge,
    rcAmount,
    rcFrequency,

    -- ** RegionInfo
    RegionInfo (..),
    mkRegionInfo,
    riRegionName,
    riOptInStatus,
    riEndpoint,

    -- ** RegisterInstanceTagAttributeRequest
    RegisterInstanceTagAttributeRequest (..),
    mkRegisterInstanceTagAttributeRequest,
    ritarIncludeAllTagsOfInstance,
    ritarInstanceTagKeys,

    -- ** RemovePrefixListEntry
    RemovePrefixListEntry (..),
    mkRemovePrefixListEntry,
    rpleCidr,

    -- ** RequestLaunchTemplateData
    RequestLaunchTemplateData (..),
    mkRequestLaunchTemplateData,
    rltdSecurityGroupIds,
    rltdSecurityGroups,
    rltdElasticInferenceAccelerators,
    rltdInstanceMarketOptions,
    rltdLicenseSpecifications,
    rltdDisableAPITermination,
    rltdKeyName,
    rltdNetworkInterfaces,
    rltdEnclaveOptions,
    rltdCPUOptions,
    rltdRamDiskId,
    rltdKernelId,
    rltdElasticGpuSpecifications,
    rltdInstanceType,
    rltdCapacityReservationSpecification,
    rltdEBSOptimized,
    rltdUserData,
    rltdMonitoring,
    rltdTagSpecifications,
    rltdHibernationOptions,
    rltdIAMInstanceProfile,
    rltdImageId,
    rltdInstanceInitiatedShutdownBehavior,
    rltdMetadataOptions,
    rltdCreditSpecification,
    rltdBlockDeviceMappings,
    rltdPlacement,

    -- ** RequestSpotLaunchSpecification
    RequestSpotLaunchSpecification (..),
    mkRequestSpotLaunchSpecification,
    rslsSecurityGroupIds,
    rslsSecurityGroups,
    rslsKeyName,
    rslsNetworkInterfaces,
    rslsRAMDiskId,
    rslsSubnetId,
    rslsKernelId,
    rslsInstanceType,
    rslsEBSOptimized,
    rslsUserData,
    rslsMonitoring,
    rslsIAMInstanceProfile,
    rslsImageId,
    rslsAddressingType,
    rslsBlockDeviceMappings,
    rslsPlacement,

    -- ** Reservation
    Reservation (..),
    mkReservation,
    rGroups,
    rOwnerId,
    rInstances,
    rReservationId,
    rRequesterId,

    -- ** ReservationValue
    ReservationValue (..),
    mkReservationValue,
    rvHourlyPrice,
    rvRemainingTotalValue,
    rvRemainingUpfrontValue,

    -- ** ReservedInstanceLimitPrice
    ReservedInstanceLimitPrice (..),
    mkReservedInstanceLimitPrice,
    rilpAmount,
    rilpCurrencyCode,

    -- ** ReservedInstanceReservationValue
    ReservedInstanceReservationValue (..),
    mkReservedInstanceReservationValue,
    rirvReservationValue,
    rirvReservedInstanceId,

    -- ** ReservedInstances
    ReservedInstances (..),
    mkReservedInstances,
    riState,
    riCurrencyCode,
    riInstanceCount,
    riProductDescription,
    riStart,
    riInstanceType,
    riEnd,
    riAvailabilityZone,
    riScope,
    riRecurringCharges,
    riOfferingType,
    riUsagePrice,
    riFixedPrice,
    riReservedInstancesId,
    riInstanceTenancy,
    riOfferingClass,
    riDuration,
    riTags,

    -- ** ReservedInstancesConfiguration
    ReservedInstancesConfiguration (..),
    mkReservedInstancesConfiguration,
    ricPlatform,
    ricInstanceCount,
    ricInstanceType,
    ricAvailabilityZone,
    ricScope,

    -- ** ReservedInstancesId
    ReservedInstancesId (..),
    mkReservedInstancesId,
    riiReservedInstancesId,

    -- ** ReservedInstancesListing
    ReservedInstancesListing (..),
    mkReservedInstancesListing,
    rilStatus,
    rilClientToken,
    rilUpdateDate,
    rilCreateDate,
    rilPriceSchedules,
    rilStatusMessage,
    rilReservedInstancesId,
    rilTags,
    rilInstanceCounts,
    rilReservedInstancesListingId,

    -- ** ReservedInstancesModification
    ReservedInstancesModification (..),
    mkReservedInstancesModification,
    rimModificationResults,
    rimStatus,
    rimClientToken,
    rimUpdateDate,
    rimCreateDate,
    rimEffectiveDate,
    rimStatusMessage,
    rimReservedInstancesModificationId,
    rimReservedInstancesIds,

    -- ** ReservedInstancesModificationResult
    ReservedInstancesModificationResult (..),
    mkReservedInstancesModificationResult,
    rimrReservedInstancesId,
    rimrTargetConfiguration,

    -- ** ReservedInstancesOffering
    ReservedInstancesOffering (..),
    mkReservedInstancesOffering,
    rioMarketplace,
    rioCurrencyCode,
    rioProductDescription,
    rioInstanceType,
    rioAvailabilityZone,
    rioPricingDetails,
    rioScope,
    rioRecurringCharges,
    rioOfferingType,
    rioUsagePrice,
    rioFixedPrice,
    rioInstanceTenancy,
    rioReservedInstancesOfferingId,
    rioOfferingClass,
    rioDuration,

    -- ** ResponseError
    ResponseError (..),
    mkResponseError,
    reCode,
    reMessage,

    -- ** ResponseLaunchTemplateData
    ResponseLaunchTemplateData (..),
    mkResponseLaunchTemplateData,
    rSecurityGroupIds,
    rSecurityGroups,
    rElasticInferenceAccelerators,
    rInstanceMarketOptions,
    rLicenseSpecifications,
    rDisableAPITermination,
    rKeyName,
    rNetworkInterfaces,
    rEnclaveOptions,
    rCPUOptions,
    rRamDiskId,
    rKernelId,
    rElasticGpuSpecifications,
    rInstanceType,
    rCapacityReservationSpecification,
    rEBSOptimized,
    rUserData,
    rMonitoring,
    rTagSpecifications,
    rHibernationOptions,
    rIAMInstanceProfile,
    rImageId,
    rInstanceInitiatedShutdownBehavior,
    rMetadataOptions,
    rCreditSpecification,
    rBlockDeviceMappings,
    rPlacement,

    -- ** Route
    Route (..),
    mkRoute,
    rVPCPeeringConnectionId,
    rInstanceId,
    rOrigin,
    rState,
    rEgressOnlyInternetGatewayId,
    rDestinationIPv6CidrBlock,
    rLocalGatewayId,
    rNatGatewayId,
    rNetworkInterfaceId,
    rTransitGatewayId,
    rGatewayId,
    rInstanceOwnerId,
    rDestinationPrefixListId,
    rCarrierGatewayId,
    rDestinationCidrBlock,

    -- ** RouteTable
    RouteTable (..),
    mkRouteTable,
    rtRouteTableId,
    rtRoutes,
    rtVPCId,
    rtPropagatingVGWs,
    rtOwnerId,
    rtAssociations,
    rtTags,

    -- ** RouteTableAssociation
    RouteTableAssociation (..),
    mkRouteTableAssociation,
    rtaRouteTableId,
    rtaRouteTableAssociationId,
    rtaMain,
    rtaSubnetId,
    rtaGatewayId,
    rtaAssociationState,

    -- ** RouteTableAssociationState
    RouteTableAssociationState (..),
    mkRouteTableAssociationState,
    rtasState,
    rtasStatusMessage,

    -- ** RunInstancesMonitoringEnabled
    RunInstancesMonitoringEnabled (..),
    mkRunInstancesMonitoringEnabled,
    rimeEnabled,

    -- ** S3Storage
    S3Storage (..),
    mkS3Storage,
    ssPrefix,
    ssUploadPolicy,
    ssBucket,
    ssUploadPolicySignature,
    ssAWSAccessKeyId,

    -- ** ScheduledInstance
    ScheduledInstance (..),
    mkScheduledInstance,
    siPreviousSlotEndTime,
    siPlatform,
    siTermStartDate,
    siInstanceCount,
    siScheduledInstanceId,
    siHourlyPrice,
    siCreateDate,
    siSlotDurationInHours,
    siTotalScheduledInstanceHours,
    siInstanceType,
    siRecurrence,
    siAvailabilityZone,
    siTermEndDate,
    siNextSlotStartTime,
    siNetworkPlatform,

    -- ** ScheduledInstanceAvailability
    ScheduledInstanceAvailability (..),
    mkScheduledInstanceAvailability,
    siaMaxTermDurationInDays,
    siaPlatform,
    siaPurchaseToken,
    siaHourlyPrice,
    siaAvailableInstanceCount,
    siaSlotDurationInHours,
    siaTotalScheduledInstanceHours,
    siaInstanceType,
    siaRecurrence,
    siaAvailabilityZone,
    siaMinTermDurationInDays,
    siaFirstSlotStartTime,
    siaNetworkPlatform,

    -- ** ScheduledInstanceRecurrence
    ScheduledInstanceRecurrence (..),
    mkScheduledInstanceRecurrence,
    sirFrequency,
    sirOccurrenceRelativeToEnd,
    sirOccurrenceUnit,
    sirInterval,
    sirOccurrenceDaySet,

    -- ** ScheduledInstanceRecurrenceRequest
    ScheduledInstanceRecurrenceRequest (..),
    mkScheduledInstanceRecurrenceRequest,
    sirrFrequency,
    sirrOccurrenceRelativeToEnd,
    sirrOccurrenceDays,
    sirrOccurrenceUnit,
    sirrInterval,

    -- ** ScheduledInstancesBlockDeviceMapping
    ScheduledInstancesBlockDeviceMapping (..),
    mkScheduledInstancesBlockDeviceMapping,
    sibdmVirtualName,
    sibdmNoDevice,
    sibdmEBS,
    sibdmDeviceName,

    -- ** ScheduledInstancesEBS
    ScheduledInstancesEBS (..),
    mkScheduledInstancesEBS,
    sieDeleteOnTermination,
    sieVolumeSize,
    sieIOPS,
    sieEncrypted,
    sieVolumeType,
    sieSnapshotId,

    -- ** ScheduledInstancesIAMInstanceProfile
    ScheduledInstancesIAMInstanceProfile (..),
    mkScheduledInstancesIAMInstanceProfile,
    siiapARN,
    siiapName,

    -- ** ScheduledInstancesIPv6Address
    ScheduledInstancesIPv6Address (..),
    mkScheduledInstancesIPv6Address,
    siiaIPv6Address,

    -- ** ScheduledInstancesLaunchSpecification
    ScheduledInstancesLaunchSpecification (..),
    mkScheduledInstancesLaunchSpecification,
    silsSecurityGroupIds,
    silsKeyName,
    silsNetworkInterfaces,
    silsRAMDiskId,
    silsSubnetId,
    silsKernelId,
    silsInstanceType,
    silsEBSOptimized,
    silsUserData,
    silsMonitoring,
    silsIAMInstanceProfile,
    silsImageId,
    silsBlockDeviceMappings,
    silsPlacement,

    -- ** ScheduledInstancesMonitoring
    ScheduledInstancesMonitoring (..),
    mkScheduledInstancesMonitoring,
    simEnabled,

    -- ** ScheduledInstancesNetworkInterface
    ScheduledInstancesNetworkInterface (..),
    mkScheduledInstancesNetworkInterface,
    siniGroups,
    siniDeleteOnTermination,
    siniAssociatePublicIPAddress,
    siniPrivateIPAddressConfigs,
    siniNetworkInterfaceId,
    siniSubnetId,
    siniIPv6AddressCount,
    siniPrivateIPAddress,
    siniSecondaryPrivateIPAddressCount,
    siniDescription,
    siniDeviceIndex,
    siniIPv6Addresses,

    -- ** ScheduledInstancesPlacement
    ScheduledInstancesPlacement (..),
    mkScheduledInstancesPlacement,
    sipAvailabilityZone,
    sipGroupName,

    -- ** ScheduledInstancesPrivateIPAddressConfig
    ScheduledInstancesPrivateIPAddressConfig (..),
    mkScheduledInstancesPrivateIPAddressConfig,
    sipiacPrimary,
    sipiacPrivateIPAddress,

    -- ** SecurityGroup
    SecurityGroup (..),
    mkSecurityGroup,
    sgVPCId,
    sgIPPermissions,
    sgOwnerId,
    sgIPPermissionsEgress,
    sgGroupId,
    sgGroupName,
    sgDescription,
    sgTags,

    -- ** SecurityGroupIdentifier
    SecurityGroupIdentifier (..),
    mkSecurityGroupIdentifier,
    sgiGroupId,
    sgiGroupName,

    -- ** SecurityGroupReference
    SecurityGroupReference (..),
    mkSecurityGroupReference,
    sgrVPCPeeringConnectionId,
    sgrReferencingVPCId,
    sgrGroupId,

    -- ** ServiceConfiguration
    ServiceConfiguration (..),
    mkServiceConfiguration,
    scNetworkLoadBalancerARNs,
    scBaseEndpointDNSNames,
    scAvailabilityZones,
    scGatewayLoadBalancerARNs,
    scManagesVPCEndpoints,
    scServiceName,
    scServiceState,
    scServiceType,
    scAcceptanceRequired,
    scServiceId,
    scPrivateDNSName,
    scPrivateDNSNameConfiguration,
    scTags,

    -- ** ServiceDetail
    ServiceDetail (..),
    mkServiceDetail,
    sdPrivateDNSNameVerificationState,
    sdVPCEndpointPolicySupported,
    sdBaseEndpointDNSNames,
    sdOwner,
    sdAvailabilityZones,
    sdManagesVPCEndpoints,
    sdServiceName,
    sdServiceType,
    sdAcceptanceRequired,
    sdPrivateDNSNames,
    sdServiceId,
    sdPrivateDNSName,
    sdTags,

    -- ** ServiceTypeDetail
    ServiceTypeDetail (..),
    mkServiceTypeDetail,
    stdServiceType,

    -- ** SlotDateTimeRangeRequest
    SlotDateTimeRangeRequest (..),
    mkSlotDateTimeRangeRequest,
    sdtrrLatestTime,
    sdtrrEarliestTime,

    -- ** SlotStartTimeRangeRequest
    SlotStartTimeRangeRequest (..),
    mkSlotStartTimeRangeRequest,
    sstrrLatestTime,
    sstrrEarliestTime,

    -- ** Snapshot
    Snapshot (..),
    mkSnapshot,
    sfStateMessage,
    sfState,
    sfOwnerAlias,
    sfProgress,
    sfStartTime,
    sfVolumeSize,
    sfDataEncryptionKeyId,
    sfEncrypted,
    sfOwnerId,
    sfKMSKeyId,
    sfVolumeId,
    sfDescription,
    sfTags,
    sfSnapshotId,

    -- ** SnapshotDetail
    SnapshotDetail (..),
    mkSnapshotDetail,
    sdStatus,
    sdProgress,
    sdFormat,
    sdURL,
    sdDeviceName,
    sdStatusMessage,
    sdUserBucket,
    sdDiskImageSize,
    sdDescription,
    sdSnapshotId,

    -- ** SnapshotDiskContainer
    SnapshotDiskContainer (..),
    mkSnapshotDiskContainer,
    sdcFormat,
    sdcURL,
    sdcUserBucket,
    sdcDescription,

    -- ** SnapshotInfo
    SnapshotInfo (..),
    mkSnapshotInfo,
    siState,
    siProgress,
    siStartTime,
    siVolumeSize,
    siEncrypted,
    siOwnerId,
    siVolumeId,
    siDescription,
    siTags,
    siSnapshotId,

    -- ** SnapshotTaskDetail
    SnapshotTaskDetail (..),
    mkSnapshotTaskDetail,
    stdStatus,
    stdProgress,
    stdFormat,
    stdURL,
    stdEncrypted,
    stdKMSKeyId,
    stdStatusMessage,
    stdUserBucket,
    stdDiskImageSize,
    stdDescription,
    stdSnapshotId,

    -- ** SpotCapacityRebalance
    SpotCapacityRebalance (..),
    mkSpotCapacityRebalance,
    scrReplacementStrategy,

    -- ** SpotDatafeedSubscription
    SpotDatafeedSubscription (..),
    mkSpotDatafeedSubscription,
    sdsState,
    sdsPrefix,
    sdsBucket,
    sdsOwnerId,
    sdsFault,

    -- ** SpotFleetLaunchSpecification
    SpotFleetLaunchSpecification (..),
    mkSpotFleetLaunchSpecification,
    sflsSecurityGroups,
    sflsSpotPrice,
    sflsWeightedCapacity,
    sflsKeyName,
    sflsNetworkInterfaces,
    sflsRAMDiskId,
    sflsSubnetId,
    sflsKernelId,
    sflsInstanceType,
    sflsEBSOptimized,
    sflsUserData,
    sflsMonitoring,
    sflsTagSpecifications,
    sflsIAMInstanceProfile,
    sflsImageId,
    sflsAddressingType,
    sflsBlockDeviceMappings,
    sflsPlacement,

    -- ** SpotFleetMonitoring
    SpotFleetMonitoring (..),
    mkSpotFleetMonitoring,
    sfmEnabled,

    -- ** SpotFleetRequestConfig
    SpotFleetRequestConfig (..),
    mkSpotFleetRequestConfig,
    sfrcSpotFleetRequestConfig,
    sfrcSpotFleetRequestId,
    sfrcSpotFleetRequestState,
    sfrcCreateTime,
    sfrcTags,
    sfrcActivityStatus,

    -- ** SpotFleetRequestConfigData
    SpotFleetRequestConfigData (..),
    mkSpotFleetRequestConfigData,
    sfrcdIAMFleetRole,
    sfrcdClientToken,
    sfrcdInstanceInterruptionBehavior,
    sfrcdOnDemandMaxTotalPrice,
    sfrcdTargetCapacity,
    sfrcdSpotPrice,
    sfrcdSpotMaintenanceStrategies,
    sfrcdLoadBalancersConfig,
    sfrcdExcessCapacityTerminationPolicy,
    sfrcdOnDemandTargetCapacity,
    sfrcdLaunchTemplateConfigs,
    sfrcdTagSpecifications,
    sfrcdValidUntil,
    sfrcdTerminateInstancesWithExpiration,
    sfrcdOnDemandAllocationStrategy,
    sfrcdInstancePoolsToUseCount,
    sfrcdFulfilledCapacity,
    sfrcdType,
    sfrcdValidFrom,
    sfrcdReplaceUnhealthyInstances,
    sfrcdLaunchSpecifications,
    sfrcdOnDemandFulfilledCapacity,
    sfrcdSpotMaxTotalPrice,
    sfrcdAllocationStrategy,

    -- ** SpotFleetTagSpecification
    SpotFleetTagSpecification (..),
    mkSpotFleetTagSpecification,
    sftsResourceType,
    sftsTags,

    -- ** SpotInstanceRequest
    SpotInstanceRequest (..),
    mkSpotInstanceRequest,
    sirInstanceId,
    sirStatus,
    sirState,
    sirActualBlockHourlyPrice,
    sirBlockDurationMinutes,
    sirInstanceInterruptionBehavior,
    sirProductDescription,
    sirSpotPrice,
    sirLaunchSpecification,
    sirAvailabilityZoneGroup,
    sirLaunchedAvailabilityZone,
    sirValidUntil,
    sirLaunchGroup,
    sirFault,
    sirSpotInstanceRequestId,
    sirType,
    sirValidFrom,
    sirCreateTime,
    sirTags,

    -- ** SpotInstanceStateFault
    SpotInstanceStateFault (..),
    mkSpotInstanceStateFault,
    sisfCode,
    sisfMessage,

    -- ** SpotInstanceStatus
    SpotInstanceStatus (..),
    mkSpotInstanceStatus,
    sisUpdateTime,
    sisCode,
    sisMessage,

    -- ** SpotMaintenanceStrategies
    SpotMaintenanceStrategies (..),
    mkSpotMaintenanceStrategies,
    smsCapacityRebalance,

    -- ** SpotMarketOptions
    SpotMarketOptions (..),
    mkSpotMarketOptions,
    smoBlockDurationMinutes,
    smoInstanceInterruptionBehavior,
    smoValidUntil,
    smoSpotInstanceType,
    smoMaxPrice,

    -- ** SpotOptions
    SpotOptions (..),
    mkSpotOptions,
    soInstanceInterruptionBehavior,
    soSingleAvailabilityZone,
    soMaxTotalPrice,
    soMinTargetCapacity,
    soInstancePoolsToUseCount,
    soMaintenanceStrategies,
    soSingleInstanceType,
    soAllocationStrategy,

    -- ** SpotOptionsRequest
    SpotOptionsRequest (..),
    mkSpotOptionsRequest,
    sorInstanceInterruptionBehavior,
    sorSingleAvailabilityZone,
    sorMaxTotalPrice,
    sorMinTargetCapacity,
    sorInstancePoolsToUseCount,
    sorMaintenanceStrategies,
    sorSingleInstanceType,
    sorAllocationStrategy,

    -- ** SpotPlacement
    SpotPlacement (..),
    mkSpotPlacement,
    spAvailabilityZone,
    spTenancy,
    spGroupName,

    -- ** SpotPrice
    SpotPrice (..),
    mkSpotPrice,
    spfProductDescription,
    spfSpotPrice,
    spfInstanceType,
    spfAvailabilityZone,
    spfTimestamp,

    -- ** StaleIPPermission
    StaleIPPermission (..),
    mkStaleIPPermission,
    sipFromPort,
    sipUserIdGroupPairs,
    sipPrefixListIds,
    sipIPProtocol,
    sipToPort,
    sipIPRanges,

    -- ** StaleSecurityGroup
    StaleSecurityGroup (..),
    mkStaleSecurityGroup,
    ssgVPCId,
    ssgGroupId,
    ssgGroupName,
    ssgStaleIPPermissionsEgress,
    ssgStaleIPPermissions,
    ssgDescription,

    -- ** StateReason
    StateReason (..),
    mkStateReason,
    srCode,
    srMessage,

    -- ** Storage
    Storage (..),
    mkStorage,
    sS3,

    -- ** StorageLocation
    StorageLocation (..),
    mkStorageLocation,
    slBucket,
    slKey,

    -- ** Subnet
    Subnet (..),
    mkSubnet,
    sState,
    sIPv6CidrBlockAssociationSet,
    sAvailabilityZoneId,
    sAvailableIPAddressCount,
    sVPCId,
    sOutpostARN,
    sAssignIPv6AddressOnCreation,
    sSubnetId,
    sSubnetARN,
    sOwnerId,
    sCustomerOwnedIPv4Pool,
    sAvailabilityZone,
    sMapCustomerOwnedIPOnLaunch,
    sCidrBlock,
    sMapPublicIPOnLaunch,
    sDefaultForAz,
    sTags,

    -- ** SubnetAssociation
    SubnetAssociation (..),
    mkSubnetAssociation,
    saState,
    saSubnetId,

    -- ** SubnetCidrBlockState
    SubnetCidrBlockState (..),
    mkSubnetCidrBlockState,
    scbsState,
    scbsStatusMessage,

    -- ** SubnetIPv6CidrBlockAssociation
    SubnetIPv6CidrBlockAssociation (..),
    mkSubnetIPv6CidrBlockAssociation,
    sicbaAssociationId,
    sicbaIPv6CidrBlock,
    sicbaIPv6CidrBlockState,

    -- ** SuccessfulInstanceCreditSpecificationItem
    SuccessfulInstanceCreditSpecificationItem (..),
    mkSuccessfulInstanceCreditSpecificationItem,
    sicsiInstanceId,

    -- ** SuccessfulQueuedPurchaseDeletion
    SuccessfulQueuedPurchaseDeletion (..),
    mkSuccessfulQueuedPurchaseDeletion,
    sqpdReservedInstancesId,

    -- ** Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- ** TagDescription
    TagDescription (..),
    mkTagDescription,
    tdResourceId,
    tdResourceType,
    tdValue,
    tdKey,

    -- ** TagSpecification
    TagSpecification (..),
    mkTagSpecification,
    tsResourceType,
    tsTags,

    -- ** TargetCapacitySpecification
    TargetCapacitySpecification (..),
    mkTargetCapacitySpecification,
    tcsOnDemandTargetCapacity,
    tcsDefaultTargetCapacityType,
    tcsTotalTargetCapacity,
    tcsSpotTargetCapacity,

    -- ** TargetCapacitySpecificationRequest
    TargetCapacitySpecificationRequest (..),
    mkTargetCapacitySpecificationRequest,
    tcsrOnDemandTargetCapacity,
    tcsrDefaultTargetCapacityType,
    tcsrTotalTargetCapacity,
    tcsrSpotTargetCapacity,

    -- ** TargetConfiguration
    TargetConfiguration (..),
    mkTargetConfiguration,
    tcInstanceCount,
    tcOfferingId,

    -- ** TargetConfigurationRequest
    TargetConfigurationRequest (..),
    mkTargetConfigurationRequest,
    tcrInstanceCount,
    tcrOfferingId,

    -- ** TargetGroup
    TargetGroup (..),
    mkTargetGroup,
    tgARN,

    -- ** TargetGroupsConfig
    TargetGroupsConfig (..),
    mkTargetGroupsConfig,
    tgcTargetGroups,

    -- ** TargetNetwork
    TargetNetwork (..),
    mkTargetNetwork,
    tnAssociationId,
    tnStatus,
    tnSecurityGroups,
    tnTargetNetworkId,
    tnVPCId,
    tnClientVPNEndpointId,

    -- ** TargetReservationValue
    TargetReservationValue (..),
    mkTargetReservationValue,
    trvReservationValue,
    trvTargetConfiguration,

    -- ** TerminateConnectionStatus
    TerminateConnectionStatus (..),
    mkTerminateConnectionStatus,
    tcsCurrentStatus,
    tcsConnectionId,
    tcsPreviousStatus,

    -- ** TrafficMirrorFilter
    TrafficMirrorFilter (..),
    mkTrafficMirrorFilter,
    tmfTrafficMirrorFilterId,
    tmfIngressFilterRules,
    tmfNetworkServices,
    tmfEgressFilterRules,
    tmfDescription,
    tmfTags,

    -- ** TrafficMirrorFilterRule
    TrafficMirrorFilterRule (..),
    mkTrafficMirrorFilterRule,
    tmfrRuleNumber,
    tmfrTrafficDirection,
    tmfrRuleAction,
    tmfrProtocol,
    tmfrTrafficMirrorFilterId,
    tmfrTrafficMirrorFilterRuleId,
    tmfrDestinationPortRange,
    tmfrSourceCidrBlock,
    tmfrSourcePortRange,
    tmfrDescription,
    tmfrDestinationCidrBlock,

    -- ** TrafficMirrorPortRange
    TrafficMirrorPortRange (..),
    mkTrafficMirrorPortRange,
    tmprFromPort,
    tmprToPort,

    -- ** TrafficMirrorPortRangeRequest
    TrafficMirrorPortRangeRequest (..),
    mkTrafficMirrorPortRangeRequest,
    tmprrFromPort,
    tmprrToPort,

    -- ** TrafficMirrorSession
    TrafficMirrorSession (..),
    mkTrafficMirrorSession,
    tmsTrafficMirrorTargetId,
    tmsNetworkInterfaceId,
    tmsTrafficMirrorFilterId,
    tmsPacketLength,
    tmsOwnerId,
    tmsTrafficMirrorSessionId,
    tmsVirtualNetworkId,
    tmsSessionNumber,
    tmsDescription,
    tmsTags,

    -- ** TrafficMirrorTarget
    TrafficMirrorTarget (..),
    mkTrafficMirrorTarget,
    tmtTrafficMirrorTargetId,
    tmtNetworkInterfaceId,
    tmtNetworkLoadBalancerARN,
    tmtOwnerId,
    tmtType,
    tmtDescription,
    tmtTags,

    -- ** TransitGateway
    TransitGateway (..),
    mkTransitGateway,
    tgCreationTime,
    tgState,
    tgOwnerId,
    tgTransitGatewayARN,
    tgTransitGatewayId,
    tgOptions,
    tgDescription,
    tgTags,

    -- ** TransitGatewayAssociation
    TransitGatewayAssociation (..),
    mkTransitGatewayAssociation,
    tgafState,
    tgafResourceId,
    tgafResourceType,
    tgafTransitGatewayRouteTableId,
    tgafTransitGatewayAttachmentId,

    -- ** TransitGatewayAttachment
    TransitGatewayAttachment (..),
    mkTransitGatewayAttachment,
    tgaCreationTime,
    tgaState,
    tgaResourceId,
    tgaResourceType,
    tgaTransitGatewayOwnerId,
    tgaTransitGatewayId,
    tgaTransitGatewayAttachmentId,
    tgaResourceOwnerId,
    tgaTags,
    tgaAssociation,

    -- ** TransitGatewayAttachmentAssociation
    TransitGatewayAttachmentAssociation (..),
    mkTransitGatewayAttachmentAssociation,
    tgaaState,
    tgaaTransitGatewayRouteTableId,

    -- ** TransitGatewayAttachmentPropagation
    TransitGatewayAttachmentPropagation (..),
    mkTransitGatewayAttachmentPropagation,
    tgapState,
    tgapTransitGatewayRouteTableId,

    -- ** TransitGatewayMulticastDeregisteredGroupMembers
    TransitGatewayMulticastDeregisteredGroupMembers (..),
    mkTransitGatewayMulticastDeregisteredGroupMembers,
    tgmdgmDeregisteredNetworkInterfaceIds,
    tgmdgmTransitGatewayMulticastDomainId,
    tgmdgmGroupIPAddress,

    -- ** TransitGatewayMulticastDeregisteredGroupSources
    TransitGatewayMulticastDeregisteredGroupSources (..),
    mkTransitGatewayMulticastDeregisteredGroupSources,
    tgmdgsDeregisteredNetworkInterfaceIds,
    tgmdgsTransitGatewayMulticastDomainId,
    tgmdgsGroupIPAddress,

    -- ** TransitGatewayMulticastDomain
    TransitGatewayMulticastDomain (..),
    mkTransitGatewayMulticastDomain,
    tgmdCreationTime,
    tgmdState,
    tgmdTransitGatewayMulticastDomainId,
    tgmdTransitGatewayId,
    tgmdTags,

    -- ** TransitGatewayMulticastDomainAssociation
    TransitGatewayMulticastDomainAssociation (..),
    mkTransitGatewayMulticastDomainAssociation,
    tgmdaResourceId,
    tgmdaResourceType,
    tgmdaSubnet,
    tgmdaTransitGatewayAttachmentId,

    -- ** TransitGatewayMulticastDomainAssociations
    TransitGatewayMulticastDomainAssociations (..),
    mkTransitGatewayMulticastDomainAssociations,
    tResourceId,
    tResourceType,
    tSubnets,
    tTransitGatewayMulticastDomainId,
    tTransitGatewayAttachmentId,

    -- ** TransitGatewayMulticastGroup
    TransitGatewayMulticastGroup (..),
    mkTransitGatewayMulticastGroup,
    tgmgResourceId,
    tgmgResourceType,
    tgmgSourceType,
    tgmgMemberType,
    tgmgNetworkInterfaceId,
    tgmgSubnetId,
    tgmgGroupMember,
    tgmgGroupSource,
    tgmgGroupIPAddress,
    tgmgTransitGatewayAttachmentId,

    -- ** TransitGatewayMulticastRegisteredGroupMembers
    TransitGatewayMulticastRegisteredGroupMembers (..),
    mkTransitGatewayMulticastRegisteredGroupMembers,
    tgmrgmTransitGatewayMulticastDomainId,
    tgmrgmRegisteredNetworkInterfaceIds,
    tgmrgmGroupIPAddress,

    -- ** TransitGatewayMulticastRegisteredGroupSources
    TransitGatewayMulticastRegisteredGroupSources (..),
    mkTransitGatewayMulticastRegisteredGroupSources,
    tgmrgsTransitGatewayMulticastDomainId,
    tgmrgsRegisteredNetworkInterfaceIds,
    tgmrgsGroupIPAddress,

    -- ** TransitGatewayOptions
    TransitGatewayOptions (..),
    mkTransitGatewayOptions,
    tgoVPNEcmpSupport,
    tgoAutoAcceptSharedAttachments,
    tgoPropagationDefaultRouteTableId,
    tgoDefaultRouteTableAssociation,
    tgoAssociationDefaultRouteTableId,
    tgoAmazonSideASN,
    tgoDefaultRouteTablePropagation,
    tgoMulticastSupport,
    tgoDNSSupport,

    -- ** TransitGatewayPeeringAttachment
    TransitGatewayPeeringAttachment (..),
    mkTransitGatewayPeeringAttachment,
    tgpaCreationTime,
    tgpaRequesterTgwInfo,
    tgpaStatus,
    tgpaState,
    tgpaAccepterTgwInfo,
    tgpaTransitGatewayAttachmentId,
    tgpaTags,

    -- ** TransitGatewayPrefixListAttachment
    TransitGatewayPrefixListAttachment (..),
    mkTransitGatewayPrefixListAttachment,
    tgplaResourceId,
    tgplaResourceType,
    tgplaTransitGatewayAttachmentId,

    -- ** TransitGatewayPrefixListReference
    TransitGatewayPrefixListReference (..),
    mkTransitGatewayPrefixListReference,
    tgplrState,
    tgplrTransitGatewayRouteTableId,
    tgplrPrefixListOwnerId,
    tgplrBlackhole,
    tgplrPrefixListId,
    tgplrTransitGatewayAttachment,

    -- ** TransitGatewayPropagation
    TransitGatewayPropagation (..),
    mkTransitGatewayPropagation,
    tgpState,
    tgpResourceId,
    tgpResourceType,
    tgpTransitGatewayRouteTableId,
    tgpTransitGatewayAttachmentId,

    -- ** TransitGatewayRequestOptions
    TransitGatewayRequestOptions (..),
    mkTransitGatewayRequestOptions,
    tgroVPNEcmpSupport,
    tgroAutoAcceptSharedAttachments,
    tgroDefaultRouteTableAssociation,
    tgroAmazonSideASN,
    tgroDefaultRouteTablePropagation,
    tgroMulticastSupport,
    tgroDNSSupport,

    -- ** TransitGatewayRoute
    TransitGatewayRoute (..),
    mkTransitGatewayRoute,
    tgrState,
    tgrPrefixListId,
    tgrTransitGatewayAttachments,
    tgrType,
    tgrDestinationCidrBlock,

    -- ** TransitGatewayRouteAttachment
    TransitGatewayRouteAttachment (..),
    mkTransitGatewayRouteAttachment,
    tgraResourceId,
    tgraResourceType,
    tgraTransitGatewayAttachmentId,

    -- ** TransitGatewayRouteTable
    TransitGatewayRouteTable (..),
    mkTransitGatewayRouteTable,
    tgrtCreationTime,
    tgrtState,
    tgrtDefaultPropagationRouteTable,
    tgrtTransitGatewayRouteTableId,
    tgrtTransitGatewayId,
    tgrtDefaultAssociationRouteTable,
    tgrtTags,

    -- ** TransitGatewayRouteTableAssociation
    TransitGatewayRouteTableAssociation (..),
    mkTransitGatewayRouteTableAssociation,
    tgrtaState,
    tgrtaResourceId,
    tgrtaResourceType,
    tgrtaTransitGatewayAttachmentId,

    -- ** TransitGatewayRouteTablePropagation
    TransitGatewayRouteTablePropagation (..),
    mkTransitGatewayRouteTablePropagation,
    tgrtpState,
    tgrtpResourceId,
    tgrtpResourceType,
    tgrtpTransitGatewayAttachmentId,

    -- ** TransitGatewayVPCAttachment
    TransitGatewayVPCAttachment (..),
    mkTransitGatewayVPCAttachment,
    tgvaCreationTime,
    tgvaState,
    tgvaSubnetIds,
    tgvaVPCId,
    tgvaTransitGatewayId,
    tgvaOptions,
    tgvaTransitGatewayAttachmentId,
    tgvaTags,
    tgvaVPCOwnerId,

    -- ** TransitGatewayVPCAttachmentOptions
    TransitGatewayVPCAttachmentOptions (..),
    mkTransitGatewayVPCAttachmentOptions,
    tgvaoIPv6Support,
    tgvaoApplianceModeSupport,
    tgvaoDNSSupport,

    -- ** TunnelOption
    TunnelOption (..),
    mkTunnelOption,
    toOutsideIPAddress,
    toReplayWindowSize,
    toDpdTimeoutAction,
    toRekeyFuzzPercentage,
    toPhase1LifetimeSeconds,
    toIkeVersions,
    toPhase2IntegrityAlgorithms,
    toPhase2LifetimeSeconds,
    toPhase1EncryptionAlgorithms,
    toPhase1DHGroupNumbers,
    toPhase1IntegrityAlgorithms,
    toRekeyMarginTimeSeconds,
    toDpdTimeoutSeconds,
    toTunnelInsideCidr,
    toStartupAction,
    toPhase2EncryptionAlgorithms,
    toPhase2DHGroupNumbers,
    toPreSharedKey,
    toTunnelInsideIPv6Cidr,

    -- ** UnsuccessfulInstanceCreditSpecificationItem
    UnsuccessfulInstanceCreditSpecificationItem (..),
    mkUnsuccessfulInstanceCreditSpecificationItem,
    uicsiInstanceId,
    uicsiError,

    -- ** UnsuccessfulInstanceCreditSpecificationItemError
    UnsuccessfulInstanceCreditSpecificationItemError (..),
    mkUnsuccessfulInstanceCreditSpecificationItemError,
    uicsieCode,
    uicsieMessage,

    -- ** UnsuccessfulItem
    UnsuccessfulItem (..),
    mkUnsuccessfulItem,
    uiResourceId,
    uiError,

    -- ** UnsuccessfulItemError
    UnsuccessfulItemError (..),
    mkUnsuccessfulItemError,
    uieCode,
    uieMessage,

    -- ** UserBucket
    UserBucket (..),
    mkUserBucket,
    ubS3Key,
    ubS3Bucket,

    -- ** UserBucketDetails
    UserBucketDetails (..),
    mkUserBucketDetails,
    ubdS3Key,
    ubdS3Bucket,

    -- ** UserData
    UserData (..),
    mkUserData,
    udData,

    -- ** UserIdGroupPair
    UserIdGroupPair (..),
    mkUserIdGroupPair,
    uigpVPCPeeringConnectionId,
    uigpVPCId,
    uigpUserId,
    uigpGroupId,
    uigpGroupName,
    uigpDescription,
    uigpPeeringStatus,

    -- ** VCPUInfo
    VCPUInfo (..),
    mkVCPUInfo,
    vciValidThreadsPerCore,
    vciDefaultThreadsPerCore,
    vciDefaultVCPUs,
    vciDefaultCores,
    vciValidCores,

    -- ** VGWTelemetry
    VGWTelemetry (..),
    mkVGWTelemetry,
    vtStatus,
    vtOutsideIPAddress,
    vtCertificateARN,
    vtLastStatusChange,
    vtAcceptedRouteCount,
    vtStatusMessage,

    -- ** VPC
    VPC (..),
    mkVPC,
    vfState,
    vfIPv6CidrBlockAssociationSet,
    vfVPCId,
    vfCidrBlockAssociationSet,
    vfOwnerId,
    vfDHCPOptionsId,
    vfCidrBlock,
    vfInstanceTenancy,
    vfTags,
    vfIsDefault,

    -- ** VPCAttachment
    VPCAttachment (..),
    mkVPCAttachment,
    vaState,
    vaVPCId,

    -- ** VPCCidrBlockAssociation
    VPCCidrBlockAssociation (..),
    mkVPCCidrBlockAssociation,
    vcbaAssociationId,
    vcbaCidrBlockState,
    vcbaCidrBlock,

    -- ** VPCCidrBlockState
    VPCCidrBlockState (..),
    mkVPCCidrBlockState,
    vcbsState,
    vcbsStatusMessage,

    -- ** VPCClassicLink
    VPCClassicLink (..),
    mkVPCClassicLink,
    vclVPCId,
    vclTags,
    vclClassicLinkEnabled,

    -- ** VPCEndpoint
    VPCEndpoint (..),
    mkVPCEndpoint,
    veGroups,
    veState,
    vePolicyDocument,
    veSubnetIds,
    veNetworkInterfaceIds,
    veVPCId,
    veRequesterManaged,
    veDNSEntries,
    veVPCEndpointType,
    vePrivateDNSEnabled,
    veOwnerId,
    veCreationTimestamp,
    veServiceName,
    veLastError,
    veVPCEndpointId,
    veTags,
    veRouteTableIds,

    -- ** VPCEndpointConnection
    VPCEndpointConnection (..),
    mkVPCEndpointConnection,
    vecVPCEndpointOwner,
    vecNetworkLoadBalancerARNs,
    vecDNSEntries,
    vecVPCEndpointState,
    vecGatewayLoadBalancerARNs,
    vecCreationTimestamp,
    vecServiceId,
    vecVPCEndpointId,

    -- ** VPCIPv6CidrBlockAssociation
    VPCIPv6CidrBlockAssociation (..),
    mkVPCIPv6CidrBlockAssociation,
    vicbaAssociationId,
    vicbaIPv6CidrBlock,
    vicbaNetworkBorderGroup,
    vicbaIPv6CidrBlockState,
    vicbaIPv6Pool,

    -- ** VPCPeeringConnection
    VPCPeeringConnection (..),
    mkVPCPeeringConnection,
    vpcVPCPeeringConnectionId,
    vpcStatus,
    vpcAccepterVPCInfo,
    vpcRequesterVPCInfo,
    vpcExpirationTime,
    vpcTags,

    -- ** VPCPeeringConnectionOptionsDescription
    VPCPeeringConnectionOptionsDescription (..),
    mkVPCPeeringConnectionOptionsDescription,
    vpcodAllowEgressFromLocalVPCToRemoteClassicLink,
    vpcodAllowEgressFromLocalClassicLinkToRemoteVPC,
    vpcodAllowDNSResolutionFromRemoteVPC,

    -- ** VPCPeeringConnectionStateReason
    VPCPeeringConnectionStateReason (..),
    mkVPCPeeringConnectionStateReason,
    vpcsrCode,
    vpcsrMessage,

    -- ** VPCPeeringConnectionVPCInfo
    VPCPeeringConnectionVPCInfo (..),
    mkVPCPeeringConnectionVPCInfo,
    vpcviCidrBlockSet,
    vpcviVPCId,
    vpcviOwnerId,
    vpcviPeeringOptions,
    vpcviCidrBlock,
    vpcviRegion,
    vpcviIPv6CidrBlockSet,

    -- ** VPNConnection
    VPNConnection (..),
    mkVPNConnection,
    vcCustomerGatewayConfiguration,
    vcState,
    vcRoutes,
    vcVPNGatewayId,
    vcCategory,
    vcCustomerGatewayId,
    vcTransitGatewayId,
    vcType,
    vcOptions,
    vcVPNConnectionId,
    vcTags,
    vcVGWTelemetry,

    -- ** VPNConnectionOptions
    VPNConnectionOptions (..),
    mkVPNConnectionOptions,
    vcoTunnelInsideIPVersion,
    vcoRemoteIPv4NetworkCidr,
    vcoEnableAcceleration,
    vcoLocalIPv4NetworkCidr,
    vcoRemoteIPv6NetworkCidr,
    vcoTunnelOptions,
    vcoLocalIPv6NetworkCidr,
    vcoStaticRoutesOnly,

    -- ** VPNConnectionOptionsSpecification
    VPNConnectionOptionsSpecification (..),
    mkVPNConnectionOptionsSpecification,
    vcosTunnelInsideIPVersion,
    vcosRemoteIPv4NetworkCidr,
    vcosEnableAcceleration,
    vcosLocalIPv4NetworkCidr,
    vcosRemoteIPv6NetworkCidr,
    vcosTunnelOptions,
    vcosLocalIPv6NetworkCidr,
    vcosStaticRoutesOnly,

    -- ** VPNGateway
    VPNGateway (..),
    mkVPNGateway,
    vgState,
    vgVPCAttachments,
    vgVPNGatewayId,
    vgAmazonSideASN,
    vgAvailabilityZone,
    vgType,
    vgTags,

    -- ** VPNStaticRoute
    VPNStaticRoute (..),
    mkVPNStaticRoute,
    vsrState,
    vsrSource,
    vsrDestinationCidrBlock,

    -- ** VPNTunnelOptionsSpecification
    VPNTunnelOptionsSpecification (..),
    mkVPNTunnelOptionsSpecification,
    vtosReplayWindowSize,
    vtosDPDTimeoutAction,
    vtosRekeyFuzzPercentage,
    vtosPhase1LifetimeSeconds,
    vtosIKEVersions,
    vtosPhase2IntegrityAlgorithms,
    vtosPhase2LifetimeSeconds,
    vtosPhase1EncryptionAlgorithms,
    vtosPhase1DHGroupNumbers,
    vtosPhase1IntegrityAlgorithms,
    vtosRekeyMarginTimeSeconds,
    vtosDPDTimeoutSeconds,
    vtosTunnelInsideCidr,
    vtosStartupAction,
    vtosPhase2EncryptionAlgorithms,
    vtosPhase2DHGroupNumbers,
    vtosPreSharedKey,
    vtosTunnelInsideIPv6Cidr,

    -- ** ValidationError
    ValidationError (..),
    mkValidationError,
    veCode,
    veMessage,

    -- ** ValidationWarning
    ValidationWarning (..),
    mkValidationWarning,
    vwErrors,

    -- ** Volume
    Volume (..),
    mkVolume,
    vFastRestored,
    vState,
    vMultiAttachEnabled,
    vAttachments,
    vSize,
    vIOPS,
    vOutpostARN,
    vEncrypted,
    vKMSKeyId,
    vAvailabilityZone,
    vVolumeId,
    vVolumeType,
    vCreateTime,
    vTags,
    vSnapshotId,

    -- ** VolumeAttachment
    VolumeAttachment (..),
    mkVolumeAttachment,
    vafInstanceId,
    vafDeleteOnTermination,
    vafState,
    vafDevice,
    vafVolumeId,
    vafAttachTime,

    -- ** VolumeDetail
    VolumeDetail (..),
    mkVolumeDetail,
    vdSize,

    -- ** VolumeModification
    VolumeModification (..),
    mkVolumeModification,
    vmProgress,
    vmStartTime,
    vmModificationState,
    vmTargetVolumeType,
    vmOriginalVolumeType,
    vmTargetSize,
    vmTargetIOPS,
    vmOriginalSize,
    vmOriginalIOPS,
    vmStatusMessage,
    vmEndTime,
    vmVolumeId,

    -- ** VolumeStatusAction
    VolumeStatusAction (..),
    mkVolumeStatusAction,
    vsaEventType,
    vsaCode,
    vsaDescription,
    vsaEventId,

    -- ** VolumeStatusAttachmentStatus
    VolumeStatusAttachmentStatus (..),
    mkVolumeStatusAttachmentStatus,
    vsasInstanceId,
    vsasIOPerformance,

    -- ** VolumeStatusDetails
    VolumeStatusDetails (..),
    mkVolumeStatusDetails,
    vsdStatus,
    vsdName,

    -- ** VolumeStatusEvent
    VolumeStatusEvent (..),
    mkVolumeStatusEvent,
    vseInstanceId,
    vseNotBefore,
    vseEventType,
    vseDescription,
    vseNotAfter,
    vseEventId,

    -- ** VolumeStatusInfo
    VolumeStatusInfo (..),
    mkVolumeStatusInfo,
    vsiStatus,
    vsiDetails,

    -- ** VolumeStatusItem
    VolumeStatusItem (..),
    mkVolumeStatusItem,
    vsiVolumeStatus,
    vsiActions,
    vsiOutpostARN,
    vsiEvents,
    vsiAvailabilityZone,
    vsiVolumeId,
    vsiAttachmentStatuses,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.DateTime,
    Lude.Timestamp,
  )
where

import Network.AWS.EC2.AcceptReservedInstancesExchangeQuote
import Network.AWS.EC2.AcceptTransitGatewayPeeringAttachment
import Network.AWS.EC2.AcceptTransitGatewayVPCAttachment
import Network.AWS.EC2.AcceptVPCEndpointConnections
import Network.AWS.EC2.AcceptVPCPeeringConnection
import Network.AWS.EC2.AdvertiseByoipCidr
import Network.AWS.EC2.AllocateAddress
import Network.AWS.EC2.AllocateHosts
import Network.AWS.EC2.ApplySecurityGroupsToClientVPNTargetNetwork
import Network.AWS.EC2.AssignIPv6Addresses
import Network.AWS.EC2.AssignPrivateIPAddresses
import Network.AWS.EC2.AssociateAddress
import Network.AWS.EC2.AssociateClientVPNTargetNetwork
import Network.AWS.EC2.AssociateDHCPOptions
import Network.AWS.EC2.AssociateEnclaveCertificateIAMRole
import Network.AWS.EC2.AssociateIAMInstanceProfile
import Network.AWS.EC2.AssociateRouteTable
import Network.AWS.EC2.AssociateSubnetCidrBlock
import Network.AWS.EC2.AssociateTransitGatewayMulticastDomain
import Network.AWS.EC2.AssociateTransitGatewayRouteTable
import Network.AWS.EC2.AssociateVPCCidrBlock
import Network.AWS.EC2.AttachClassicLinkVPC
import Network.AWS.EC2.AttachInternetGateway
import Network.AWS.EC2.AttachNetworkInterface
import Network.AWS.EC2.AttachVPNGateway
import Network.AWS.EC2.AttachVolume
import Network.AWS.EC2.AuthorizeClientVPNIngress
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
import Network.AWS.EC2.CreateClientVPNEndpoint
import Network.AWS.EC2.CreateClientVPNRoute
import Network.AWS.EC2.CreateCustomerGateway
import Network.AWS.EC2.CreateDHCPOptions
import Network.AWS.EC2.CreateDefaultSubnet
import Network.AWS.EC2.CreateDefaultVPC
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
import Network.AWS.EC2.CreateLocalGatewayRouteTableVPCAssociation
import Network.AWS.EC2.CreateManagedPrefixList
import Network.AWS.EC2.CreateNatGateway
import Network.AWS.EC2.CreateNetworkACL
import Network.AWS.EC2.CreateNetworkACLEntry
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
import Network.AWS.EC2.CreateTransitGatewayMulticastDomain
import Network.AWS.EC2.CreateTransitGatewayPeeringAttachment
import Network.AWS.EC2.CreateTransitGatewayPrefixListReference
import Network.AWS.EC2.CreateTransitGatewayRoute
import Network.AWS.EC2.CreateTransitGatewayRouteTable
import Network.AWS.EC2.CreateTransitGatewayVPCAttachment
import Network.AWS.EC2.CreateVPC
import Network.AWS.EC2.CreateVPCEndpoint
import Network.AWS.EC2.CreateVPCEndpointConnectionNotification
import Network.AWS.EC2.CreateVPCEndpointServiceConfiguration
import Network.AWS.EC2.CreateVPCPeeringConnection
import Network.AWS.EC2.CreateVPNConnection
import Network.AWS.EC2.CreateVPNConnectionRoute
import Network.AWS.EC2.CreateVPNGateway
import Network.AWS.EC2.CreateVolume
import Network.AWS.EC2.DeleteCarrierGateway
import Network.AWS.EC2.DeleteClientVPNEndpoint
import Network.AWS.EC2.DeleteClientVPNRoute
import Network.AWS.EC2.DeleteCustomerGateway
import Network.AWS.EC2.DeleteDHCPOptions
import Network.AWS.EC2.DeleteEgressOnlyInternetGateway
import Network.AWS.EC2.DeleteFleets
import Network.AWS.EC2.DeleteFlowLogs
import Network.AWS.EC2.DeleteFpgaImage
import Network.AWS.EC2.DeleteInternetGateway
import Network.AWS.EC2.DeleteKeyPair
import Network.AWS.EC2.DeleteLaunchTemplate
import Network.AWS.EC2.DeleteLaunchTemplateVersions
import Network.AWS.EC2.DeleteLocalGatewayRoute
import Network.AWS.EC2.DeleteLocalGatewayRouteTableVPCAssociation
import Network.AWS.EC2.DeleteManagedPrefixList
import Network.AWS.EC2.DeleteNatGateway
import Network.AWS.EC2.DeleteNetworkACL
import Network.AWS.EC2.DeleteNetworkACLEntry
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
import Network.AWS.EC2.DeleteTransitGatewayMulticastDomain
import Network.AWS.EC2.DeleteTransitGatewayPeeringAttachment
import Network.AWS.EC2.DeleteTransitGatewayPrefixListReference
import Network.AWS.EC2.DeleteTransitGatewayRoute
import Network.AWS.EC2.DeleteTransitGatewayRouteTable
import Network.AWS.EC2.DeleteTransitGatewayVPCAttachment
import Network.AWS.EC2.DeleteVPC
import Network.AWS.EC2.DeleteVPCEndpointConnectionNotifications
import Network.AWS.EC2.DeleteVPCEndpointServiceConfigurations
import Network.AWS.EC2.DeleteVPCEndpoints
import Network.AWS.EC2.DeleteVPCPeeringConnection
import Network.AWS.EC2.DeleteVPNConnection
import Network.AWS.EC2.DeleteVPNConnectionRoute
import Network.AWS.EC2.DeleteVPNGateway
import Network.AWS.EC2.DeleteVolume
import Network.AWS.EC2.DeprovisionByoipCidr
import Network.AWS.EC2.DeregisterImage
import Network.AWS.EC2.DeregisterInstanceEventNotificationAttributes
import Network.AWS.EC2.DeregisterTransitGatewayMulticastGroupMembers
import Network.AWS.EC2.DeregisterTransitGatewayMulticastGroupSources
import Network.AWS.EC2.DescribeAccountAttributes
import Network.AWS.EC2.DescribeAddresses
import Network.AWS.EC2.DescribeAggregateIdFormat
import Network.AWS.EC2.DescribeAvailabilityZones
import Network.AWS.EC2.DescribeBundleTasks
import Network.AWS.EC2.DescribeByoipCidrs
import Network.AWS.EC2.DescribeCapacityReservations
import Network.AWS.EC2.DescribeCarrierGateways
import Network.AWS.EC2.DescribeClassicLinkInstances
import Network.AWS.EC2.DescribeClientVPNAuthorizationRules
import Network.AWS.EC2.DescribeClientVPNConnections
import Network.AWS.EC2.DescribeClientVPNEndpoints
import Network.AWS.EC2.DescribeClientVPNRoutes
import Network.AWS.EC2.DescribeClientVPNTargetNetworks
import Network.AWS.EC2.DescribeCoipPools
import Network.AWS.EC2.DescribeConversionTasks
import Network.AWS.EC2.DescribeCustomerGateways
import Network.AWS.EC2.DescribeDHCPOptions
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
import Network.AWS.EC2.DescribeIAMInstanceProfileAssociations
import Network.AWS.EC2.DescribeIPv6Pools
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
import Network.AWS.EC2.DescribeKeyPairs
import Network.AWS.EC2.DescribeLaunchTemplateVersions
import Network.AWS.EC2.DescribeLaunchTemplates
import Network.AWS.EC2.DescribeLocalGatewayRouteTableVPCAssociations
import Network.AWS.EC2.DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
import Network.AWS.EC2.DescribeLocalGatewayRouteTables
import Network.AWS.EC2.DescribeLocalGatewayVirtualInterfaceGroups
import Network.AWS.EC2.DescribeLocalGatewayVirtualInterfaces
import Network.AWS.EC2.DescribeLocalGateways
import Network.AWS.EC2.DescribeManagedPrefixLists
import Network.AWS.EC2.DescribeMovingAddresses
import Network.AWS.EC2.DescribeNatGateways
import Network.AWS.EC2.DescribeNetworkACLs
import Network.AWS.EC2.DescribeNetworkInterfaceAttribute
import Network.AWS.EC2.DescribeNetworkInterfacePermissions
import Network.AWS.EC2.DescribeNetworkInterfaces
import Network.AWS.EC2.DescribePlacementGroups
import Network.AWS.EC2.DescribePrefixLists
import Network.AWS.EC2.DescribePrincipalIdFormat
import Network.AWS.EC2.DescribePublicIPv4Pools
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
import Network.AWS.EC2.DescribeTransitGatewayMulticastDomains
import Network.AWS.EC2.DescribeTransitGatewayPeeringAttachments
import Network.AWS.EC2.DescribeTransitGatewayRouteTables
import Network.AWS.EC2.DescribeTransitGatewayVPCAttachments
import Network.AWS.EC2.DescribeTransitGateways
import Network.AWS.EC2.DescribeVPCAttribute
import Network.AWS.EC2.DescribeVPCClassicLink
import Network.AWS.EC2.DescribeVPCClassicLinkDNSSupport
import Network.AWS.EC2.DescribeVPCEndpointConnectionNotifications
import Network.AWS.EC2.DescribeVPCEndpointConnections
import Network.AWS.EC2.DescribeVPCEndpointServiceConfigurations
import Network.AWS.EC2.DescribeVPCEndpointServicePermissions
import Network.AWS.EC2.DescribeVPCEndpointServices
import Network.AWS.EC2.DescribeVPCEndpoints
import Network.AWS.EC2.DescribeVPCPeeringConnections
import Network.AWS.EC2.DescribeVPCs
import Network.AWS.EC2.DescribeVPNConnections
import Network.AWS.EC2.DescribeVPNGateways
import Network.AWS.EC2.DescribeVolumeAttribute
import Network.AWS.EC2.DescribeVolumeStatus
import Network.AWS.EC2.DescribeVolumes
import Network.AWS.EC2.DescribeVolumesModifications
import Network.AWS.EC2.DetachClassicLinkVPC
import Network.AWS.EC2.DetachInternetGateway
import Network.AWS.EC2.DetachNetworkInterface
import Network.AWS.EC2.DetachVPNGateway
import Network.AWS.EC2.DetachVolume
import Network.AWS.EC2.DisableEBSEncryptionByDefault
import Network.AWS.EC2.DisableFastSnapshotRestores
import Network.AWS.EC2.DisableTransitGatewayRouteTablePropagation
import Network.AWS.EC2.DisableVGWRoutePropagation
import Network.AWS.EC2.DisableVPCClassicLink
import Network.AWS.EC2.DisableVPCClassicLinkDNSSupport
import Network.AWS.EC2.DisassociateAddress
import Network.AWS.EC2.DisassociateClientVPNTargetNetwork
import Network.AWS.EC2.DisassociateEnclaveCertificateIAMRole
import Network.AWS.EC2.DisassociateIAMInstanceProfile
import Network.AWS.EC2.DisassociateRouteTable
import Network.AWS.EC2.DisassociateSubnetCidrBlock
import Network.AWS.EC2.DisassociateTransitGatewayMulticastDomain
import Network.AWS.EC2.DisassociateTransitGatewayRouteTable
import Network.AWS.EC2.DisassociateVPCCidrBlock
import Network.AWS.EC2.EnableEBSEncryptionByDefault
import Network.AWS.EC2.EnableFastSnapshotRestores
import Network.AWS.EC2.EnableTransitGatewayRouteTablePropagation
import Network.AWS.EC2.EnableVGWRoutePropagation
import Network.AWS.EC2.EnableVPCClassicLink
import Network.AWS.EC2.EnableVPCClassicLinkDNSSupport
import Network.AWS.EC2.EnableVolumeIO
import Network.AWS.EC2.ExportClientVPNClientCertificateRevocationList
import Network.AWS.EC2.ExportClientVPNClientConfiguration
import Network.AWS.EC2.ExportImage
import Network.AWS.EC2.ExportTransitGatewayRoutes
import Network.AWS.EC2.GetAssociatedEnclaveCertificateIAMRoles
import Network.AWS.EC2.GetAssociatedIPv6PoolCidrs
import Network.AWS.EC2.GetCapacityReservationUsage
import Network.AWS.EC2.GetCoipPoolUsage
import Network.AWS.EC2.GetConsoleOutput
import Network.AWS.EC2.GetConsoleScreenshot
import Network.AWS.EC2.GetDefaultCreditSpecification
import Network.AWS.EC2.GetEBSDefaultKMSKeyId
import Network.AWS.EC2.GetEBSEncryptionByDefault
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
import Network.AWS.EC2.ImportClientVPNClientCertificateRevocationList
import Network.AWS.EC2.ImportImage
import Network.AWS.EC2.ImportInstance
import Network.AWS.EC2.ImportKeyPair
import Network.AWS.EC2.ImportSnapshot
import Network.AWS.EC2.ImportVolume
import Network.AWS.EC2.ModifyAvailabilityZoneGroup
import Network.AWS.EC2.ModifyCapacityReservation
import Network.AWS.EC2.ModifyClientVPNEndpoint
import Network.AWS.EC2.ModifyDefaultCreditSpecification
import Network.AWS.EC2.ModifyEBSDefaultKMSKeyId
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
import Network.AWS.EC2.ModifyTransitGatewayVPCAttachment
import Network.AWS.EC2.ModifyVPCAttribute
import Network.AWS.EC2.ModifyVPCEndpoint
import Network.AWS.EC2.ModifyVPCEndpointConnectionNotification
import Network.AWS.EC2.ModifyVPCEndpointServiceConfiguration
import Network.AWS.EC2.ModifyVPCEndpointServicePermissions
import Network.AWS.EC2.ModifyVPCPeeringConnectionOptions
import Network.AWS.EC2.ModifyVPCTenancy
import Network.AWS.EC2.ModifyVPNConnection
import Network.AWS.EC2.ModifyVPNConnectionOptions
import Network.AWS.EC2.ModifyVPNTunnelCertificate
import Network.AWS.EC2.ModifyVPNTunnelOptions
import Network.AWS.EC2.ModifyVolume
import Network.AWS.EC2.ModifyVolumeAttribute
import Network.AWS.EC2.MonitorInstances
import Network.AWS.EC2.MoveAddressToVPC
import Network.AWS.EC2.ProvisionByoipCidr
import Network.AWS.EC2.PurchaseHostReservation
import Network.AWS.EC2.PurchaseReservedInstancesOffering
import Network.AWS.EC2.PurchaseScheduledInstances
import Network.AWS.EC2.RebootInstances
import Network.AWS.EC2.RegisterImage
import Network.AWS.EC2.RegisterInstanceEventNotificationAttributes
import Network.AWS.EC2.RegisterTransitGatewayMulticastGroupMembers
import Network.AWS.EC2.RegisterTransitGatewayMulticastGroupSources
import Network.AWS.EC2.RejectTransitGatewayPeeringAttachment
import Network.AWS.EC2.RejectTransitGatewayVPCAttachment
import Network.AWS.EC2.RejectVPCEndpointConnections
import Network.AWS.EC2.RejectVPCPeeringConnection
import Network.AWS.EC2.ReleaseAddress
import Network.AWS.EC2.ReleaseHosts
import Network.AWS.EC2.ReplaceIAMInstanceProfileAssociation
import Network.AWS.EC2.ReplaceNetworkACLAssociation
import Network.AWS.EC2.ReplaceNetworkACLEntry
import Network.AWS.EC2.ReplaceRoute
import Network.AWS.EC2.ReplaceRouteTableAssociation
import Network.AWS.EC2.ReplaceTransitGatewayRoute
import Network.AWS.EC2.ReportInstanceStatus
import Network.AWS.EC2.RequestSpotFleet
import Network.AWS.EC2.RequestSpotInstances
import Network.AWS.EC2.ResetEBSDefaultKMSKeyId
import Network.AWS.EC2.ResetFpgaImageAttribute
import Network.AWS.EC2.ResetImageAttribute
import Network.AWS.EC2.ResetInstanceAttribute
import Network.AWS.EC2.ResetNetworkInterfaceAttribute
import Network.AWS.EC2.ResetSnapshotAttribute
import Network.AWS.EC2.RestoreAddressToClassic
import Network.AWS.EC2.RestoreManagedPrefixListVersion
import Network.AWS.EC2.RevokeClientVPNIngress
import Network.AWS.EC2.RevokeSecurityGroupEgress
import Network.AWS.EC2.RevokeSecurityGroupIngress
import Network.AWS.EC2.RunInstances
import Network.AWS.EC2.RunScheduledInstances
import Network.AWS.EC2.SearchLocalGatewayRoutes
import Network.AWS.EC2.SearchTransitGatewayMulticastGroups
import Network.AWS.EC2.SearchTransitGatewayRoutes
import Network.AWS.EC2.SendDiagnosticInterrupt
import Network.AWS.EC2.StartInstances
import Network.AWS.EC2.StartVPCEndpointServicePrivateDNSVerification
import Network.AWS.EC2.StopInstances
import Network.AWS.EC2.TerminateClientVPNConnections
import Network.AWS.EC2.TerminateInstances
import Network.AWS.EC2.Types
import Network.AWS.EC2.UnassignIPv6Addresses
import Network.AWS.EC2.UnassignPrivateIPAddresses
import Network.AWS.EC2.UnmonitorInstances
import Network.AWS.EC2.UpdateSecurityGroupRuleDescriptionsEgress
import Network.AWS.EC2.UpdateSecurityGroupRuleDescriptionsIngress
import Network.AWS.EC2.Waiters
import Network.AWS.EC2.WithdrawByoipCidr
import qualified Network.AWS.Prelude as Lude

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
