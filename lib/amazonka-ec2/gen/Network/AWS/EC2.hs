{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

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
--
--
module Network.AWS.EC2
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- ** InstanceTerminated
    , mkInstanceTerminated

    -- ** VolumeInUse
    , mkVolumeInUse

    -- ** ImageExists
    , mkImageExists

    -- ** NatGatewayAvailable
    , mkNatGatewayAvailable

    -- ** SubnetAvailable
    , mkSubnetAvailable

    -- ** NetworkInterfaceAvailable
    , mkNetworkInterfaceAvailable

    -- ** KeyPairExists
    , mkKeyPairExists

    -- ** SystemStatusOk
    , mkSystemStatusOk

    -- ** CustomerGatewayAvailable
    , mkCustomerGatewayAvailable

    -- ** ConversionTaskCompleted
    , mkConversionTaskCompleted

    -- ** InstanceStopped
    , mkInstanceStopped

    -- ** ConversionTaskDeleted
    , mkConversionTaskDeleted

    -- ** PasswordDataAvailable
    , mkPasswordDataAvailable

    -- ** InstanceRunning
    , mkInstanceRunning

    -- ** SecurityGroupExists
    , mkSecurityGroupExists

    -- ** SpotInstanceRequestFulfilled
    , mkSpotInstanceRequestFulfilled

    -- ** VpcAvailable
    , mkVpcAvailable

    -- ** ExportTaskCompleted
    , mkExportTaskCompleted

    -- ** VpcPeeringConnectionDeleted
    , mkVpcPeeringConnectionDeleted

    -- ** VpnConnectionAvailable
    , mkVpnConnectionAvailable

    -- ** ExportTaskCancelled
    , mkExportTaskCancelled

    -- ** VolumeDeleted
    , mkVolumeDeleted

    -- ** VpcExists
    , mkVpcExists

    -- ** BundleTaskComplete
    , mkBundleTaskComplete

    -- ** VpnConnectionDeleted
    , mkVpnConnectionDeleted

    -- ** ConversionTaskCancelled
    , mkConversionTaskCancelled

    -- ** ImageAvailable
    , mkImageAvailable

    -- ** VpcPeeringConnectionExists
    , mkVpcPeeringConnectionExists

    -- ** SnapshotCompleted
    , mkSnapshotCompleted

    -- ** InstanceExists
    , mkInstanceExists

    -- ** InstanceStatusOk
    , mkInstanceStatusOk

    -- ** VolumeAvailable
    , mkVolumeAvailable

    -- * Operations
    -- $operations

    -- ** ModifyCapacityReservation 
    , module Network.AWS.EC2.ModifyCapacityReservation

    -- ** GetAssociatedIpv6PoolCidrs (Paginated)
    , module Network.AWS.EC2.GetAssociatedIpv6PoolCidrs

    -- ** ImportInstance 
    , module Network.AWS.EC2.ImportInstance

    -- ** RevokeSecurityGroupEgress 
    , module Network.AWS.EC2.RevokeSecurityGroupEgress

    -- ** CreateNetworkInterfacePermission 
    , module Network.AWS.EC2.CreateNetworkInterfacePermission

    -- ** SendDiagnosticInterrupt 
    , module Network.AWS.EC2.SendDiagnosticInterrupt

    -- ** DeleteLaunchTemplate 
    , module Network.AWS.EC2.DeleteLaunchTemplate

    -- ** RejectVpcEndpointConnections 
    , module Network.AWS.EC2.RejectVpcEndpointConnections

    -- ** CreateVpnGateway 
    , module Network.AWS.EC2.CreateVpnGateway

    -- ** CreateNetworkAcl 
    , module Network.AWS.EC2.CreateNetworkAcl

    -- ** DeleteKeyPair 
    , module Network.AWS.EC2.DeleteKeyPair

    -- ** DescribeSecurityGroupReferences 
    , module Network.AWS.EC2.DescribeSecurityGroupReferences

    -- ** DeleteFleets 
    , module Network.AWS.EC2.DeleteFleets

    -- ** DescribeTags (Paginated)
    , module Network.AWS.EC2.DescribeTags

    -- ** CreateTransitGatewayRouteTable 
    , module Network.AWS.EC2.CreateTransitGatewayRouteTable

    -- ** ModifyInstanceMetadataOptions 
    , module Network.AWS.EC2.ModifyInstanceMetadataOptions

    -- ** UpdateSecurityGroupRuleDescriptionsIngress 
    , module Network.AWS.EC2.UpdateSecurityGroupRuleDescriptionsIngress

    -- ** DisassociateSubnetCidrBlock 
    , module Network.AWS.EC2.DisassociateSubnetCidrBlock

    -- ** DetachNetworkInterface 
    , module Network.AWS.EC2.DetachNetworkInterface

    -- ** DetachInternetGateway 
    , module Network.AWS.EC2.DetachInternetGateway

    -- ** DeleteVpcEndpoints 
    , module Network.AWS.EC2.DeleteVpcEndpoints

    -- ** DescribeClientVpnEndpoints (Paginated)
    , module Network.AWS.EC2.DescribeClientVpnEndpoints

    -- ** DeleteFlowLogs 
    , module Network.AWS.EC2.DeleteFlowLogs

    -- ** DescribeVpcClassicLink 
    , module Network.AWS.EC2.DescribeVpcClassicLink

    -- ** GetAssociatedEnclaveCertificateIamRoles 
    , module Network.AWS.EC2.GetAssociatedEnclaveCertificateIamRoles

    -- ** AssociateTransitGatewayMulticastDomain 
    , module Network.AWS.EC2.AssociateTransitGatewayMulticastDomain

    -- ** ModifySubnetAttribute 
    , module Network.AWS.EC2.ModifySubnetAttribute

    -- ** DetachVolume 
    , module Network.AWS.EC2.DetachVolume

    -- ** DescribeInstanceCreditSpecifications (Paginated)
    , module Network.AWS.EC2.DescribeInstanceCreditSpecifications

    -- ** CancelBundleTask 
    , module Network.AWS.EC2.CancelBundleTask

    -- ** DescribeByoipCidrs (Paginated)
    , module Network.AWS.EC2.DescribeByoipCidrs

    -- ** AcceptReservedInstancesExchangeQuote 
    , module Network.AWS.EC2.AcceptReservedInstancesExchangeQuote

    -- ** ReleaseAddress 
    , module Network.AWS.EC2.ReleaseAddress

    -- ** DescribeInstanceTypeOfferings (Paginated)
    , module Network.AWS.EC2.DescribeInstanceTypeOfferings

    -- ** CreateInternetGateway 
    , module Network.AWS.EC2.CreateInternetGateway

    -- ** DeleteVpnConnection 
    , module Network.AWS.EC2.DeleteVpnConnection

    -- ** DescribeBundleTasks 
    , module Network.AWS.EC2.DescribeBundleTasks

    -- ** AuthorizeSecurityGroupEgress 
    , module Network.AWS.EC2.AuthorizeSecurityGroupEgress

    -- ** EnableTransitGatewayRouteTablePropagation 
    , module Network.AWS.EC2.EnableTransitGatewayRouteTablePropagation

    -- ** DeregisterImage 
    , module Network.AWS.EC2.DeregisterImage

    -- ** DeleteVpcEndpointConnectionNotifications 
    , module Network.AWS.EC2.DeleteVpcEndpointConnectionNotifications

    -- ** DescribeCoipPools (Paginated)
    , module Network.AWS.EC2.DescribeCoipPools

    -- ** GetTransitGatewayMulticastDomainAssociations (Paginated)
    , module Network.AWS.EC2.GetTransitGatewayMulticastDomainAssociations

    -- ** DeleteLocalGatewayRouteTableVpcAssociation 
    , module Network.AWS.EC2.DeleteLocalGatewayRouteTableVpcAssociation

    -- ** ModifyNetworkInterfaceAttribute 
    , module Network.AWS.EC2.ModifyNetworkInterfaceAttribute

    -- ** ModifyVpcTenancy 
    , module Network.AWS.EC2.ModifyVpcTenancy

    -- ** DescribeInstanceTypes (Paginated)
    , module Network.AWS.EC2.DescribeInstanceTypes

    -- ** DescribeClientVpnAuthorizationRules (Paginated)
    , module Network.AWS.EC2.DescribeClientVpnAuthorizationRules

    -- ** DeleteTransitGatewayVpcAttachment 
    , module Network.AWS.EC2.DeleteTransitGatewayVpcAttachment

    -- ** DeleteTransitGatewayMulticastDomain 
    , module Network.AWS.EC2.DeleteTransitGatewayMulticastDomain

    -- ** CancelReservedInstancesListing 
    , module Network.AWS.EC2.CancelReservedInstancesListing

    -- ** AttachClassicLinkVpc 
    , module Network.AWS.EC2.AttachClassicLinkVpc

    -- ** DisableTransitGatewayRouteTablePropagation 
    , module Network.AWS.EC2.DisableTransitGatewayRouteTablePropagation

    -- ** DescribeVpcClassicLinkDnsSupport (Paginated)
    , module Network.AWS.EC2.DescribeVpcClassicLinkDnsSupport

    -- ** AssociateSubnetCidrBlock 
    , module Network.AWS.EC2.AssociateSubnetCidrBlock

    -- ** RunScheduledInstances 
    , module Network.AWS.EC2.RunScheduledInstances

    -- ** CreateTransitGatewayRoute 
    , module Network.AWS.EC2.CreateTransitGatewayRoute

    -- ** CreateTransitGatewayPrefixListReference 
    , module Network.AWS.EC2.CreateTransitGatewayPrefixListReference

    -- ** CancelSpotFleetRequests 
    , module Network.AWS.EC2.CancelSpotFleetRequests

    -- ** DescribeSpotPriceHistory (Paginated)
    , module Network.AWS.EC2.DescribeSpotPriceHistory

    -- ** DescribeDhcpOptions (Paginated)
    , module Network.AWS.EC2.DescribeDhcpOptions

    -- ** ImportImage 
    , module Network.AWS.EC2.ImportImage

    -- ** CreateLocalGatewayRouteTableVpcAssociation 
    , module Network.AWS.EC2.CreateLocalGatewayRouteTableVpcAssociation

    -- ** CopyFpgaImage 
    , module Network.AWS.EC2.CopyFpgaImage

    -- ** ImportClientVpnClientCertificateRevocationList 
    , module Network.AWS.EC2.ImportClientVpnClientCertificateRevocationList

    -- ** StopInstances 
    , module Network.AWS.EC2.StopInstances

    -- ** EnableEbsEncryptionByDefault 
    , module Network.AWS.EC2.EnableEbsEncryptionByDefault

    -- ** DeregisterTransitGatewayMulticastGroupSources 
    , module Network.AWS.EC2.DeregisterTransitGatewayMulticastGroupSources

    -- ** ModifyLaunchTemplate 
    , module Network.AWS.EC2.ModifyLaunchTemplate

    -- ** ModifyVpcEndpointConnectionNotification 
    , module Network.AWS.EC2.ModifyVpcEndpointConnectionNotification

    -- ** DescribeInternetGateways (Paginated)
    , module Network.AWS.EC2.DescribeInternetGateways

    -- ** DisableVpcClassicLink 
    , module Network.AWS.EC2.DisableVpcClassicLink

    -- ** GetGroupsForCapacityReservation (Paginated)
    , module Network.AWS.EC2.GetGroupsForCapacityReservation

    -- ** DeleteLaunchTemplateVersions 
    , module Network.AWS.EC2.DeleteLaunchTemplateVersions

    -- ** BundleInstance 
    , module Network.AWS.EC2.BundleInstance

    -- ** DescribeNetworkInterfaces (Paginated)
    , module Network.AWS.EC2.DescribeNetworkInterfaces

    -- ** ReplaceNetworkAclAssociation 
    , module Network.AWS.EC2.ReplaceNetworkAclAssociation

    -- ** DescribeNatGateways (Paginated)
    , module Network.AWS.EC2.DescribeNatGateways

    -- ** DescribeAddresses 
    , module Network.AWS.EC2.DescribeAddresses

    -- ** RestoreManagedPrefixListVersion 
    , module Network.AWS.EC2.RestoreManagedPrefixListVersion

    -- ** DescribeSnapshotAttribute 
    , module Network.AWS.EC2.DescribeSnapshotAttribute

    -- ** DescribeIdentityIdFormat 
    , module Network.AWS.EC2.DescribeIdentityIdFormat

    -- ** ReplaceRoute 
    , module Network.AWS.EC2.ReplaceRoute

    -- ** DescribeVpcEndpointServices (Paginated)
    , module Network.AWS.EC2.DescribeVpcEndpointServices

    -- ** DeleteLocalGatewayRoute 
    , module Network.AWS.EC2.DeleteLocalGatewayRoute

    -- ** AuthorizeSecurityGroupIngress 
    , module Network.AWS.EC2.AuthorizeSecurityGroupIngress

    -- ** CreateVpcPeeringConnection 
    , module Network.AWS.EC2.CreateVpcPeeringConnection

    -- ** DescribeSubnets (Paginated)
    , module Network.AWS.EC2.DescribeSubnets

    -- ** GetTransitGatewayAttachmentPropagations (Paginated)
    , module Network.AWS.EC2.GetTransitGatewayAttachmentPropagations

    -- ** CreateTags 
    , module Network.AWS.EC2.CreateTags

    -- ** PurchaseReservedInstancesOffering 
    , module Network.AWS.EC2.PurchaseReservedInstancesOffering

    -- ** DeleteNetworkAclEntry 
    , module Network.AWS.EC2.DeleteNetworkAclEntry

    -- ** ResetSnapshotAttribute 
    , module Network.AWS.EC2.ResetSnapshotAttribute

    -- ** DescribeVpnConnections 
    , module Network.AWS.EC2.DescribeVpnConnections

    -- ** ModifyInstanceEventStartTime 
    , module Network.AWS.EC2.ModifyInstanceEventStartTime

    -- ** DeleteRoute 
    , module Network.AWS.EC2.DeleteRoute

    -- ** ReplaceNetworkAclEntry 
    , module Network.AWS.EC2.ReplaceNetworkAclEntry

    -- ** DescribeVpcEndpoints (Paginated)
    , module Network.AWS.EC2.DescribeVpcEndpoints

    -- ** CreateTrafficMirrorFilter 
    , module Network.AWS.EC2.CreateTrafficMirrorFilter

    -- ** ResetInstanceAttribute 
    , module Network.AWS.EC2.ResetInstanceAttribute

    -- ** ModifyIdentityIdFormat 
    , module Network.AWS.EC2.ModifyIdentityIdFormat

    -- ** AttachNetworkInterface 
    , module Network.AWS.EC2.AttachNetworkInterface

    -- ** CreateCapacityReservation 
    , module Network.AWS.EC2.CreateCapacityReservation

    -- ** DescribeInstanceStatus (Paginated)
    , module Network.AWS.EC2.DescribeInstanceStatus

    -- ** ImportKeyPair 
    , module Network.AWS.EC2.ImportKeyPair

    -- ** DeleteTags 
    , module Network.AWS.EC2.DeleteTags

    -- ** ConfirmProductInstance 
    , module Network.AWS.EC2.ConfirmProductInstance

    -- ** DescribeInstanceAttribute 
    , module Network.AWS.EC2.DescribeInstanceAttribute

    -- ** DescribeReservedInstancesOfferings (Paginated)
    , module Network.AWS.EC2.DescribeReservedInstancesOfferings

    -- ** CreateCustomerGateway 
    , module Network.AWS.EC2.CreateCustomerGateway

    -- ** DescribeFleets (Paginated)
    , module Network.AWS.EC2.DescribeFleets

    -- ** CreateTransitGatewayPeeringAttachment 
    , module Network.AWS.EC2.CreateTransitGatewayPeeringAttachment

    -- ** DeleteSecurityGroup 
    , module Network.AWS.EC2.DeleteSecurityGroup

    -- ** DescribePublicIpv4Pools (Paginated)
    , module Network.AWS.EC2.DescribePublicIpv4Pools

    -- ** DescribeClientVpnTargetNetworks (Paginated)
    , module Network.AWS.EC2.DescribeClientVpnTargetNetworks

    -- ** DeleteVpcPeeringConnection 
    , module Network.AWS.EC2.DeleteVpcPeeringConnection

    -- ** AttachInternetGateway 
    , module Network.AWS.EC2.AttachInternetGateway

    -- ** ModifyInstancePlacement 
    , module Network.AWS.EC2.ModifyInstancePlacement

    -- ** DescribeFlowLogs (Paginated)
    , module Network.AWS.EC2.DescribeFlowLogs

    -- ** DescribeLocalGatewayVirtualInterfaceGroups (Paginated)
    , module Network.AWS.EC2.DescribeLocalGatewayVirtualInterfaceGroups

    -- ** DescribeLocalGatewayRouteTableVpcAssociations (Paginated)
    , module Network.AWS.EC2.DescribeLocalGatewayRouteTableVpcAssociations

    -- ** DescribeVpcEndpointConnectionNotifications (Paginated)
    , module Network.AWS.EC2.DescribeVpcEndpointConnectionNotifications

    -- ** GetManagedPrefixListEntries (Paginated)
    , module Network.AWS.EC2.GetManagedPrefixListEntries

    -- ** RunInstances 
    , module Network.AWS.EC2.RunInstances

    -- ** CreateSnapshots 
    , module Network.AWS.EC2.CreateSnapshots

    -- ** AssociateDhcpOptions 
    , module Network.AWS.EC2.AssociateDhcpOptions

    -- ** DeleteTrafficMirrorFilterRule 
    , module Network.AWS.EC2.DeleteTrafficMirrorFilterRule

    -- ** DescribeReservedInstances 
    , module Network.AWS.EC2.DescribeReservedInstances

    -- ** DescribeIdFormat 
    , module Network.AWS.EC2.DescribeIdFormat

    -- ** DescribeVpcs (Paginated)
    , module Network.AWS.EC2.DescribeVpcs

    -- ** DescribeConversionTasks 
    , module Network.AWS.EC2.DescribeConversionTasks

    -- ** CreateLaunchTemplateVersion 
    , module Network.AWS.EC2.CreateLaunchTemplateVersion

    -- ** GetManagedPrefixListAssociations (Paginated)
    , module Network.AWS.EC2.GetManagedPrefixListAssociations

    -- ** DisableVpcClassicLinkDnsSupport 
    , module Network.AWS.EC2.DisableVpcClassicLinkDnsSupport

    -- ** ApplySecurityGroupsToClientVpnTargetNetwork 
    , module Network.AWS.EC2.ApplySecurityGroupsToClientVpnTargetNetwork

    -- ** DescribeTrafficMirrorTargets (Paginated)
    , module Network.AWS.EC2.DescribeTrafficMirrorTargets

    -- ** DescribeVolumesModifications (Paginated)
    , module Network.AWS.EC2.DescribeVolumesModifications

    -- ** ExportImage 
    , module Network.AWS.EC2.ExportImage

    -- ** CreateFpgaImage 
    , module Network.AWS.EC2.CreateFpgaImage

    -- ** AcceptVpcEndpointConnections 
    , module Network.AWS.EC2.AcceptVpcEndpointConnections

    -- ** DeleteClientVpnEndpoint 
    , module Network.AWS.EC2.DeleteClientVpnEndpoint

    -- ** SearchTransitGatewayRoutes 
    , module Network.AWS.EC2.SearchTransitGatewayRoutes

    -- ** GetLaunchTemplateData 
    , module Network.AWS.EC2.GetLaunchTemplateData

    -- ** AllocateAddress 
    , module Network.AWS.EC2.AllocateAddress

    -- ** AcceptTransitGatewayVpcAttachment 
    , module Network.AWS.EC2.AcceptTransitGatewayVpcAttachment

    -- ** CancelConversionTask 
    , module Network.AWS.EC2.CancelConversionTask

    -- ** ModifyImageAttribute 
    , module Network.AWS.EC2.ModifyImageAttribute

    -- ** CreateRouteTable 
    , module Network.AWS.EC2.CreateRouteTable

    -- ** RejectTransitGatewayPeeringAttachment 
    , module Network.AWS.EC2.RejectTransitGatewayPeeringAttachment

    -- ** ReportInstanceStatus 
    , module Network.AWS.EC2.ReportInstanceStatus

    -- ** AttachVolume 
    , module Network.AWS.EC2.AttachVolume

    -- ** RequestSpotInstances 
    , module Network.AWS.EC2.RequestSpotInstances

    -- ** WithdrawByoipCidr 
    , module Network.AWS.EC2.WithdrawByoipCidr

    -- ** DescribeHostReservationOfferings (Paginated)
    , module Network.AWS.EC2.DescribeHostReservationOfferings

    -- ** ResetFpgaImageAttribute 
    , module Network.AWS.EC2.ResetFpgaImageAttribute

    -- ** ModifyVpnConnection 
    , module Network.AWS.EC2.ModifyVpnConnection

    -- ** CreateTrafficMirrorFilterRule 
    , module Network.AWS.EC2.CreateTrafficMirrorFilterRule

    -- ** DeleteTransitGateway 
    , module Network.AWS.EC2.DeleteTransitGateway

    -- ** StartVpcEndpointServicePrivateDnsVerification 
    , module Network.AWS.EC2.StartVpcEndpointServicePrivateDnsVerification

    -- ** DescribeVolumes (Paginated)
    , module Network.AWS.EC2.DescribeVolumes

    -- ** RejectVpcPeeringConnection 
    , module Network.AWS.EC2.RejectVpcPeeringConnection

    -- ** DescribeClientVpnRoutes (Paginated)
    , module Network.AWS.EC2.DescribeClientVpnRoutes

    -- ** DeleteVpnConnectionRoute 
    , module Network.AWS.EC2.DeleteVpnConnectionRoute

    -- ** AssociateEnclaveCertificateIamRole 
    , module Network.AWS.EC2.AssociateEnclaveCertificateIamRole

    -- ** ModifyVpcEndpoint 
    , module Network.AWS.EC2.ModifyVpcEndpoint

    -- ** DescribeFpgaImageAttribute 
    , module Network.AWS.EC2.DescribeFpgaImageAttribute

    -- ** AllocateHosts 
    , module Network.AWS.EC2.AllocateHosts

    -- ** CreateClientVpnEndpoint 
    , module Network.AWS.EC2.CreateClientVpnEndpoint

    -- ** CreateTrafficMirrorSession 
    , module Network.AWS.EC2.CreateTrafficMirrorSession

    -- ** RegisterImage 
    , module Network.AWS.EC2.RegisterImage

    -- ** AdvertiseByoipCidr 
    , module Network.AWS.EC2.AdvertiseByoipCidr

    -- ** ModifyFleet 
    , module Network.AWS.EC2.ModifyFleet

    -- ** RevokeSecurityGroupIngress 
    , module Network.AWS.EC2.RevokeSecurityGroupIngress

    -- ** GetEbsDefaultKmsKeyId 
    , module Network.AWS.EC2.GetEbsDefaultKmsKeyId

    -- ** DescribeHostReservations (Paginated)
    , module Network.AWS.EC2.DescribeHostReservations

    -- ** UpdateSecurityGroupRuleDescriptionsEgress 
    , module Network.AWS.EC2.UpdateSecurityGroupRuleDescriptionsEgress

    -- ** EnableVpcClassicLinkDnsSupport 
    , module Network.AWS.EC2.EnableVpcClassicLinkDnsSupport

    -- ** DescribeVpcEndpointConnections (Paginated)
    , module Network.AWS.EC2.DescribeVpcEndpointConnections

    -- ** ModifyReservedInstances 
    , module Network.AWS.EC2.ModifyReservedInstances

    -- ** DeleteFpgaImage 
    , module Network.AWS.EC2.DeleteFpgaImage

    -- ** DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations (Paginated)
    , module Network.AWS.EC2.DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations

    -- ** DescribeScheduledInstances (Paginated)
    , module Network.AWS.EC2.DescribeScheduledInstances

    -- ** SearchTransitGatewayMulticastGroups (Paginated)
    , module Network.AWS.EC2.SearchTransitGatewayMulticastGroups

    -- ** CreateFlowLogs 
    , module Network.AWS.EC2.CreateFlowLogs

    -- ** DescribeSpotFleetRequests (Paginated)
    , module Network.AWS.EC2.DescribeSpotFleetRequests

    -- ** MoveAddressToVpc 
    , module Network.AWS.EC2.MoveAddressToVpc

    -- ** DescribeFleetInstances 
    , module Network.AWS.EC2.DescribeFleetInstances

    -- ** DescribeLaunchTemplateVersions (Paginated)
    , module Network.AWS.EC2.DescribeLaunchTemplateVersions

    -- ** ModifyInstanceCreditSpecification 
    , module Network.AWS.EC2.ModifyInstanceCreditSpecification

    -- ** DescribePrincipalIdFormat (Paginated)
    , module Network.AWS.EC2.DescribePrincipalIdFormat

    -- ** DescribeTransitGateways (Paginated)
    , module Network.AWS.EC2.DescribeTransitGateways

    -- ** DeleteNetworkAcl 
    , module Network.AWS.EC2.DeleteNetworkAcl

    -- ** DisassociateTransitGatewayMulticastDomain 
    , module Network.AWS.EC2.DisassociateTransitGatewayMulticastDomain

    -- ** DeleteTransitGatewayRouteTable 
    , module Network.AWS.EC2.DeleteTransitGatewayRouteTable

    -- ** CreateLaunchTemplate 
    , module Network.AWS.EC2.CreateLaunchTemplate

    -- ** CreateVpcEndpointConnectionNotification 
    , module Network.AWS.EC2.CreateVpcEndpointConnectionNotification

    -- ** DeleteNetworkInterfacePermission 
    , module Network.AWS.EC2.DeleteNetworkInterfacePermission

    -- ** DeleteVpnGateway 
    , module Network.AWS.EC2.DeleteVpnGateway

    -- ** CreateTrafficMirrorTarget 
    , module Network.AWS.EC2.CreateTrafficMirrorTarget

    -- ** DescribeImportImageTasks (Paginated)
    , module Network.AWS.EC2.DescribeImportImageTasks

    -- ** DescribeVolumeAttribute 
    , module Network.AWS.EC2.DescribeVolumeAttribute

    -- ** DescribeMovingAddresses (Paginated)
    , module Network.AWS.EC2.DescribeMovingAddresses

    -- ** ExportTransitGatewayRoutes 
    , module Network.AWS.EC2.ExportTransitGatewayRoutes

    -- ** GetPasswordData 
    , module Network.AWS.EC2.GetPasswordData

    -- ** CreateVpc 
    , module Network.AWS.EC2.CreateVpc

    -- ** ModifyVpcPeeringConnectionOptions 
    , module Network.AWS.EC2.ModifyVpcPeeringConnectionOptions

    -- ** DescribeFpgaImages (Paginated)
    , module Network.AWS.EC2.DescribeFpgaImages

    -- ** CopySnapshot 
    , module Network.AWS.EC2.CopySnapshot

    -- ** AcceptTransitGatewayPeeringAttachment 
    , module Network.AWS.EC2.AcceptTransitGatewayPeeringAttachment

    -- ** DisassociateAddress 
    , module Network.AWS.EC2.DisassociateAddress

    -- ** ModifyTrafficMirrorFilterNetworkServices 
    , module Network.AWS.EC2.ModifyTrafficMirrorFilterNetworkServices

    -- ** DescribeEgressOnlyInternetGateways (Paginated)
    , module Network.AWS.EC2.DescribeEgressOnlyInternetGateways

    -- ** DeleteVpc 
    , module Network.AWS.EC2.DeleteVpc

    -- ** CreateInstanceExportTask 
    , module Network.AWS.EC2.CreateInstanceExportTask

    -- ** RejectTransitGatewayVpcAttachment 
    , module Network.AWS.EC2.RejectTransitGatewayVpcAttachment

    -- ** DescribeTrafficMirrorSessions (Paginated)
    , module Network.AWS.EC2.DescribeTrafficMirrorSessions

    -- ** GetTransitGatewayRouteTableAssociations (Paginated)
    , module Network.AWS.EC2.GetTransitGatewayRouteTableAssociations

    -- ** AssociateVpcCidrBlock 
    , module Network.AWS.EC2.AssociateVpcCidrBlock

    -- ** DescribeVpcAttribute 
    , module Network.AWS.EC2.DescribeVpcAttribute

    -- ** CreateVolume 
    , module Network.AWS.EC2.CreateVolume

    -- ** CreateDefaultSubnet 
    , module Network.AWS.EC2.CreateDefaultSubnet

    -- ** DescribeScheduledInstanceAvailability (Paginated)
    , module Network.AWS.EC2.DescribeScheduledInstanceAvailability

    -- ** DisassociateClientVpnTargetNetwork 
    , module Network.AWS.EC2.DisassociateClientVpnTargetNetwork

    -- ** CreateClientVpnRoute 
    , module Network.AWS.EC2.CreateClientVpnRoute

    -- ** ModifyVolumeAttribute 
    , module Network.AWS.EC2.ModifyVolumeAttribute

    -- ** ExportClientVpnClientConfiguration 
    , module Network.AWS.EC2.ExportClientVpnClientConfiguration

    -- ** DeleteTrafficMirrorTarget 
    , module Network.AWS.EC2.DeleteTrafficMirrorTarget

    -- ** DescribeSpotDatafeedSubscription 
    , module Network.AWS.EC2.DescribeSpotDatafeedSubscription

    -- ** DescribeLocalGatewayRouteTables (Paginated)
    , module Network.AWS.EC2.DescribeLocalGatewayRouteTables

    -- ** DescribePrefixLists (Paginated)
    , module Network.AWS.EC2.DescribePrefixLists

    -- ** AssociateTransitGatewayRouteTable 
    , module Network.AWS.EC2.AssociateTransitGatewayRouteTable

    -- ** DeletePlacementGroup 
    , module Network.AWS.EC2.DeletePlacementGroup

    -- ** ModifyTransitGateway 
    , module Network.AWS.EC2.ModifyTransitGateway

    -- ** DeleteTransitGatewayPrefixListReference 
    , module Network.AWS.EC2.DeleteTransitGatewayPrefixListReference

    -- ** CreateTransitGatewayMulticastDomain 
    , module Network.AWS.EC2.CreateTransitGatewayMulticastDomain

    -- ** DeregisterInstanceEventNotificationAttributes 
    , module Network.AWS.EC2.DeregisterInstanceEventNotificationAttributes

    -- ** RequestSpotFleet 
    , module Network.AWS.EC2.RequestSpotFleet

    -- ** DeleteTransitGatewayRoute 
    , module Network.AWS.EC2.DeleteTransitGatewayRoute

    -- ** DisableEbsEncryptionByDefault 
    , module Network.AWS.EC2.DisableEbsEncryptionByDefault

    -- ** DeregisterTransitGatewayMulticastGroupMembers 
    , module Network.AWS.EC2.DeregisterTransitGatewayMulticastGroupMembers

    -- ** CreateSubnet 
    , module Network.AWS.EC2.CreateSubnet

    -- ** CreateNetworkInterface 
    , module Network.AWS.EC2.CreateNetworkInterface

    -- ** DescribeSecurityGroups (Paginated)
    , module Network.AWS.EC2.DescribeSecurityGroups

    -- ** GetCapacityReservationUsage 
    , module Network.AWS.EC2.GetCapacityReservationUsage

    -- ** CreateTransitGatewayVpcAttachment 
    , module Network.AWS.EC2.CreateTransitGatewayVpcAttachment

    -- ** DescribeExportTasks 
    , module Network.AWS.EC2.DescribeExportTasks

    -- ** ModifySpotFleetRequest 
    , module Network.AWS.EC2.ModifySpotFleetRequest

    -- ** DetachVpnGateway 
    , module Network.AWS.EC2.DetachVpnGateway

    -- ** ModifyManagedPrefixList 
    , module Network.AWS.EC2.ModifyManagedPrefixList

    -- ** GetHostReservationPurchasePreview 
    , module Network.AWS.EC2.GetHostReservationPurchasePreview

    -- ** EnableVolumeIO 
    , module Network.AWS.EC2.EnableVolumeIO

    -- ** DescribeInstances (Paginated)
    , module Network.AWS.EC2.DescribeInstances

    -- ** CreateNatGateway 
    , module Network.AWS.EC2.CreateNatGateway

    -- ** DescribeLocalGatewayVirtualInterfaces (Paginated)
    , module Network.AWS.EC2.DescribeLocalGatewayVirtualInterfaces

    -- ** DescribeVpcPeeringConnections (Paginated)
    , module Network.AWS.EC2.DescribeVpcPeeringConnections

    -- ** CancelExportTask 
    , module Network.AWS.EC2.CancelExportTask

    -- ** CreateVpcEndpointServiceConfiguration 
    , module Network.AWS.EC2.CreateVpcEndpointServiceConfiguration

    -- ** CreateDefaultVpc 
    , module Network.AWS.EC2.CreateDefaultVpc

    -- ** DisassociateVpcCidrBlock 
    , module Network.AWS.EC2.DisassociateVpcCidrBlock

    -- ** DescribeTrafficMirrorFilters (Paginated)
    , module Network.AWS.EC2.DescribeTrafficMirrorFilters

    -- ** DescribeFastSnapshotRestores (Paginated)
    , module Network.AWS.EC2.DescribeFastSnapshotRestores

    -- ** CancelCapacityReservation 
    , module Network.AWS.EC2.CancelCapacityReservation

    -- ** DeleteNetworkInterface 
    , module Network.AWS.EC2.DeleteNetworkInterface

    -- ** DisassociateTransitGatewayRouteTable 
    , module Network.AWS.EC2.DisassociateTransitGatewayRouteTable

    -- ** ReplaceRouteTableAssociation 
    , module Network.AWS.EC2.ReplaceRouteTableAssociation

    -- ** StartInstances 
    , module Network.AWS.EC2.StartInstances

    -- ** CreatePlacementGroup 
    , module Network.AWS.EC2.CreatePlacementGroup

    -- ** DescribeInstanceEventNotificationAttributes 
    , module Network.AWS.EC2.DescribeInstanceEventNotificationAttributes

    -- ** DescribeCapacityReservations (Paginated)
    , module Network.AWS.EC2.DescribeCapacityReservations

    -- ** ModifyClientVpnEndpoint 
    , module Network.AWS.EC2.ModifyClientVpnEndpoint

    -- ** ModifyInstanceCapacityReservationAttributes 
    , module Network.AWS.EC2.ModifyInstanceCapacityReservationAttributes

    -- ** DescribeAggregateIdFormat 
    , module Network.AWS.EC2.DescribeAggregateIdFormat

    -- ** DescribeSnapshots (Paginated)
    , module Network.AWS.EC2.DescribeSnapshots

    -- ** AssociateAddress 
    , module Network.AWS.EC2.AssociateAddress

    -- ** ModifyTrafficMirrorFilterRule 
    , module Network.AWS.EC2.ModifyTrafficMirrorFilterRule

    -- ** DescribeNetworkInterfaceAttribute 
    , module Network.AWS.EC2.DescribeNetworkInterfaceAttribute

    -- ** ReplaceIamInstanceProfileAssociation 
    , module Network.AWS.EC2.ReplaceIamInstanceProfileAssociation

    -- ** AssociateClientVpnTargetNetwork 
    , module Network.AWS.EC2.AssociateClientVpnTargetNetwork

    -- ** ReleaseHosts 
    , module Network.AWS.EC2.ReleaseHosts

    -- ** ResetNetworkInterfaceAttribute 
    , module Network.AWS.EC2.ResetNetworkInterfaceAttribute

    -- ** DeleteInternetGateway 
    , module Network.AWS.EC2.DeleteInternetGateway

    -- ** DescribeReservedInstancesListings 
    , module Network.AWS.EC2.DescribeReservedInstancesListings

    -- ** CreateVpnConnection 
    , module Network.AWS.EC2.CreateVpnConnection

    -- ** ReplaceTransitGatewayRoute 
    , module Network.AWS.EC2.ReplaceTransitGatewayRoute

    -- ** CreateFleet 
    , module Network.AWS.EC2.CreateFleet

    -- ** DeleteNatGateway 
    , module Network.AWS.EC2.DeleteNatGateway

    -- ** DescribeImportSnapshotTasks (Paginated)
    , module Network.AWS.EC2.DescribeImportSnapshotTasks

    -- ** GetCoipPoolUsage 
    , module Network.AWS.EC2.GetCoipPoolUsage

    -- ** DescribeCustomerGateways 
    , module Network.AWS.EC2.DescribeCustomerGateways

    -- ** DeleteSubnet 
    , module Network.AWS.EC2.DeleteSubnet

    -- ** CopyImage 
    , module Network.AWS.EC2.CopyImage

    -- ** CreateVpcEndpoint 
    , module Network.AWS.EC2.CreateVpcEndpoint

    -- ** ModifyTrafficMirrorSession 
    , module Network.AWS.EC2.ModifyTrafficMirrorSession

    -- ** DescribeCarrierGateways (Paginated)
    , module Network.AWS.EC2.DescribeCarrierGateways

    -- ** DescribeTransitGatewayPeeringAttachments (Paginated)
    , module Network.AWS.EC2.DescribeTransitGatewayPeeringAttachments

    -- ** DeleteQueuedReservedInstances 
    , module Network.AWS.EC2.DeleteQueuedReservedInstances

    -- ** DescribeTransitGatewayMulticastDomains (Paginated)
    , module Network.AWS.EC2.DescribeTransitGatewayMulticastDomains

    -- ** GetDefaultCreditSpecification 
    , module Network.AWS.EC2.GetDefaultCreditSpecification

    -- ** UnmonitorInstances 
    , module Network.AWS.EC2.UnmonitorInstances

    -- ** DescribeTransitGatewayVpcAttachments (Paginated)
    , module Network.AWS.EC2.DescribeTransitGatewayVpcAttachments

    -- ** CreateSecurityGroup 
    , module Network.AWS.EC2.CreateSecurityGroup

    -- ** GetEbsEncryptionByDefault 
    , module Network.AWS.EC2.GetEbsEncryptionByDefault

    -- ** ImportVolume 
    , module Network.AWS.EC2.ImportVolume

    -- ** DeleteCarrierGateway 
    , module Network.AWS.EC2.DeleteCarrierGateway

    -- ** DisableVgwRoutePropagation 
    , module Network.AWS.EC2.DisableVgwRoutePropagation

    -- ** DeleteTrafficMirrorFilter 
    , module Network.AWS.EC2.DeleteTrafficMirrorFilter

    -- ** ModifyVpnTunnelCertificate 
    , module Network.AWS.EC2.ModifyVpnTunnelCertificate

    -- ** CreateSpotDatafeedSubscription 
    , module Network.AWS.EC2.CreateSpotDatafeedSubscription

    -- ** CancelSpotInstanceRequests 
    , module Network.AWS.EC2.CancelSpotInstanceRequests

    -- ** CreateRoute 
    , module Network.AWS.EC2.CreateRoute

    -- ** DescribeVpcEndpointServiceConfigurations (Paginated)
    , module Network.AWS.EC2.DescribeVpcEndpointServiceConfigurations

    -- ** DeleteSnapshot 
    , module Network.AWS.EC2.DeleteSnapshot

    -- ** AssignPrivateIpAddresses 
    , module Network.AWS.EC2.AssignPrivateIpAddresses

    -- ** AuthorizeClientVpnIngress 
    , module Network.AWS.EC2.AuthorizeClientVpnIngress

    -- ** DeleteTransitGatewayPeeringAttachment 
    , module Network.AWS.EC2.DeleteTransitGatewayPeeringAttachment

    -- ** ModifyInstanceAttribute 
    , module Network.AWS.EC2.ModifyInstanceAttribute

    -- ** DeleteCustomerGateway 
    , module Network.AWS.EC2.DeleteCustomerGateway

    -- ** DisassociateIamInstanceProfile 
    , module Network.AWS.EC2.DisassociateIamInstanceProfile

    -- ** TerminateClientVpnConnections 
    , module Network.AWS.EC2.TerminateClientVpnConnections

    -- ** DisassociateRouteTable 
    , module Network.AWS.EC2.DisassociateRouteTable

    -- ** GetConsoleScreenshot 
    , module Network.AWS.EC2.GetConsoleScreenshot

    -- ** ResetEbsDefaultKmsKeyId 
    , module Network.AWS.EC2.ResetEbsDefaultKmsKeyId

    -- ** AssignIpv6Addresses 
    , module Network.AWS.EC2.AssignIpv6Addresses

    -- ** ModifyVpnTunnelOptions 
    , module Network.AWS.EC2.ModifyVpnTunnelOptions

    -- ** ModifyEbsDefaultKmsKeyId 
    , module Network.AWS.EC2.ModifyEbsDefaultKmsKeyId

    -- ** DeleteSpotDatafeedSubscription 
    , module Network.AWS.EC2.DeleteSpotDatafeedSubscription

    -- ** ModifyVolume 
    , module Network.AWS.EC2.ModifyVolume

    -- ** EnableVpcClassicLink 
    , module Network.AWS.EC2.EnableVpcClassicLink

    -- ** DescribePlacementGroups 
    , module Network.AWS.EC2.DescribePlacementGroups

    -- ** ProvisionByoipCidr 
    , module Network.AWS.EC2.ProvisionByoipCidr

    -- ** DisassociateEnclaveCertificateIamRole 
    , module Network.AWS.EC2.DisassociateEnclaveCertificateIamRole

    -- ** ModifyAvailabilityZoneGroup 
    , module Network.AWS.EC2.ModifyAvailabilityZoneGroup

    -- ** DescribeStaleSecurityGroups (Paginated)
    , module Network.AWS.EC2.DescribeStaleSecurityGroups

    -- ** CreateCarrierGateway 
    , module Network.AWS.EC2.CreateCarrierGateway

    -- ** DescribeExportImageTasks (Paginated)
    , module Network.AWS.EC2.DescribeExportImageTasks

    -- ** PurchaseScheduledInstances 
    , module Network.AWS.EC2.PurchaseScheduledInstances

    -- ** EnableVgwRoutePropagation 
    , module Network.AWS.EC2.EnableVgwRoutePropagation

    -- ** DescribeSpotFleetRequestHistory 
    , module Network.AWS.EC2.DescribeSpotFleetRequestHistory

    -- ** ModifySnapshotAttribute 
    , module Network.AWS.EC2.ModifySnapshotAttribute

    -- ** DescribeIamInstanceProfileAssociations (Paginated)
    , module Network.AWS.EC2.DescribeIamInstanceProfileAssociations

    -- ** CreateSnapshot 
    , module Network.AWS.EC2.CreateSnapshot

    -- ** CreateLocalGatewayRoute 
    , module Network.AWS.EC2.CreateLocalGatewayRoute

    -- ** CreateNetworkAclEntry 
    , module Network.AWS.EC2.CreateNetworkAclEntry

    -- ** DescribeTransitGatewayAttachments (Paginated)
    , module Network.AWS.EC2.DescribeTransitGatewayAttachments

    -- ** CreateReservedInstancesListing 
    , module Network.AWS.EC2.CreateReservedInstancesListing

    -- ** DescribeIpv6Pools (Paginated)
    , module Network.AWS.EC2.DescribeIpv6Pools

    -- ** AttachVpnGateway 
    , module Network.AWS.EC2.AttachVpnGateway

    -- ** DescribeLocalGateways (Paginated)
    , module Network.AWS.EC2.DescribeLocalGateways

    -- ** ModifyVpcEndpointServicePermissions 
    , module Network.AWS.EC2.ModifyVpcEndpointServicePermissions

    -- ** ExportClientVpnClientCertificateRevocationList 
    , module Network.AWS.EC2.ExportClientVpnClientCertificateRevocationList

    -- ** CreateDhcpOptions 
    , module Network.AWS.EC2.CreateDhcpOptions

    -- ** RegisterTransitGatewayMulticastGroupSources 
    , module Network.AWS.EC2.RegisterTransitGatewayMulticastGroupSources

    -- ** DescribeAccountAttributes 
    , module Network.AWS.EC2.DescribeAccountAttributes

    -- ** GetTransitGatewayRouteTablePropagations (Paginated)
    , module Network.AWS.EC2.GetTransitGatewayRouteTablePropagations

    -- ** ModifyFpgaImageAttribute 
    , module Network.AWS.EC2.ModifyFpgaImageAttribute

    -- ** ModifyHosts 
    , module Network.AWS.EC2.ModifyHosts

    -- ** RebootInstances 
    , module Network.AWS.EC2.RebootInstances

    -- ** ModifyVpcEndpointServiceConfiguration 
    , module Network.AWS.EC2.ModifyVpcEndpointServiceConfiguration

    -- ** CreateTransitGateway 
    , module Network.AWS.EC2.CreateTransitGateway

    -- ** UnassignIpv6Addresses 
    , module Network.AWS.EC2.UnassignIpv6Addresses

    -- ** DeleteTrafficMirrorSession 
    , module Network.AWS.EC2.DeleteTrafficMirrorSession

    -- ** CreateManagedPrefixList 
    , module Network.AWS.EC2.CreateManagedPrefixList

    -- ** AssociateIamInstanceProfile 
    , module Network.AWS.EC2.AssociateIamInstanceProfile

    -- ** ModifyDefaultCreditSpecification 
    , module Network.AWS.EC2.ModifyDefaultCreditSpecification

    -- ** DeleteEgressOnlyInternetGateway 
    , module Network.AWS.EC2.DeleteEgressOnlyInternetGateway

    -- ** PurchaseHostReservation 
    , module Network.AWS.EC2.PurchaseHostReservation

    -- ** ModifyTransitGatewayVpcAttachment 
    , module Network.AWS.EC2.ModifyTransitGatewayVpcAttachment

    -- ** CreateImage 
    , module Network.AWS.EC2.CreateImage

    -- ** DescribeClassicLinkInstances (Paginated)
    , module Network.AWS.EC2.DescribeClassicLinkInstances

    -- ** TerminateInstances 
    , module Network.AWS.EC2.TerminateInstances

    -- ** GetTransitGatewayPrefixListReferences (Paginated)
    , module Network.AWS.EC2.GetTransitGatewayPrefixListReferences

    -- ** DescribeKeyPairs 
    , module Network.AWS.EC2.DescribeKeyPairs

    -- ** DisableFastSnapshotRestores 
    , module Network.AWS.EC2.DisableFastSnapshotRestores

    -- ** DescribeLaunchTemplates (Paginated)
    , module Network.AWS.EC2.DescribeLaunchTemplates

    -- ** CreateVpnConnectionRoute 
    , module Network.AWS.EC2.CreateVpnConnectionRoute

    -- ** AssociateRouteTable 
    , module Network.AWS.EC2.AssociateRouteTable

    -- ** DescribeVpnGateways 
    , module Network.AWS.EC2.DescribeVpnGateways

    -- ** ModifyVpnConnectionOptions 
    , module Network.AWS.EC2.ModifyVpnConnectionOptions

    -- ** GetConsoleOutput 
    , module Network.AWS.EC2.GetConsoleOutput

    -- ** DescribeHosts (Paginated)
    , module Network.AWS.EC2.DescribeHosts

    -- ** DescribeImageAttribute 
    , module Network.AWS.EC2.DescribeImageAttribute

    -- ** ModifyIdFormat 
    , module Network.AWS.EC2.ModifyIdFormat

    -- ** RegisterTransitGatewayMulticastGroupMembers 
    , module Network.AWS.EC2.RegisterTransitGatewayMulticastGroupMembers

    -- ** DeleteManagedPrefixList 
    , module Network.AWS.EC2.DeleteManagedPrefixList

    -- ** DeleteRouteTable 
    , module Network.AWS.EC2.DeleteRouteTable

    -- ** ResetImageAttribute 
    , module Network.AWS.EC2.ResetImageAttribute

    -- ** ModifyTransitGatewayPrefixListReference 
    , module Network.AWS.EC2.ModifyTransitGatewayPrefixListReference

    -- ** DescribeTransitGatewayRouteTables (Paginated)
    , module Network.AWS.EC2.DescribeTransitGatewayRouteTables

    -- ** CreateEgressOnlyInternetGateway 
    , module Network.AWS.EC2.CreateEgressOnlyInternetGateway

    -- ** DescribeReservedInstancesModifications (Paginated)
    , module Network.AWS.EC2.DescribeReservedInstancesModifications

    -- ** DescribeSpotInstanceRequests (Paginated)
    , module Network.AWS.EC2.DescribeSpotInstanceRequests

    -- ** RevokeClientVpnIngress 
    , module Network.AWS.EC2.RevokeClientVpnIngress

    -- ** UnassignPrivateIpAddresses 
    , module Network.AWS.EC2.UnassignPrivateIpAddresses

    -- ** DescribeNetworkInterfacePermissions (Paginated)
    , module Network.AWS.EC2.DescribeNetworkInterfacePermissions

    -- ** EnableFastSnapshotRestores 
    , module Network.AWS.EC2.EnableFastSnapshotRestores

    -- ** DescribeVpcEndpointServicePermissions (Paginated)
    , module Network.AWS.EC2.DescribeVpcEndpointServicePermissions

    -- ** DeleteDhcpOptions 
    , module Network.AWS.EC2.DeleteDhcpOptions

    -- ** RegisterInstanceEventNotificationAttributes 
    , module Network.AWS.EC2.RegisterInstanceEventNotificationAttributes

    -- ** DescribeNetworkAcls (Paginated)
    , module Network.AWS.EC2.DescribeNetworkAcls

    -- ** CancelImportTask 
    , module Network.AWS.EC2.CancelImportTask

    -- ** DetachClassicLinkVpc 
    , module Network.AWS.EC2.DetachClassicLinkVpc

    -- ** DescribeRegions 
    , module Network.AWS.EC2.DescribeRegions

    -- ** MonitorInstances 
    , module Network.AWS.EC2.MonitorInstances

    -- ** SearchLocalGatewayRoutes (Paginated)
    , module Network.AWS.EC2.SearchLocalGatewayRoutes

    -- ** DeleteClientVpnRoute 
    , module Network.AWS.EC2.DeleteClientVpnRoute

    -- ** AcceptVpcPeeringConnection 
    , module Network.AWS.EC2.AcceptVpcPeeringConnection

    -- ** ImportSnapshot 
    , module Network.AWS.EC2.ImportSnapshot

    -- ** DescribeVolumeStatus (Paginated)
    , module Network.AWS.EC2.DescribeVolumeStatus

    -- ** DescribeRouteTables (Paginated)
    , module Network.AWS.EC2.DescribeRouteTables

    -- ** DescribeAvailabilityZones 
    , module Network.AWS.EC2.DescribeAvailabilityZones

    -- ** ModifyVpcAttribute 
    , module Network.AWS.EC2.ModifyVpcAttribute

    -- ** DescribeClientVpnConnections (Paginated)
    , module Network.AWS.EC2.DescribeClientVpnConnections

    -- ** DescribeFleetHistory 
    , module Network.AWS.EC2.DescribeFleetHistory

    -- ** DescribeImages 
    , module Network.AWS.EC2.DescribeImages

    -- ** DescribeElasticGpus 
    , module Network.AWS.EC2.DescribeElasticGpus

    -- ** RestoreAddressToClassic 
    , module Network.AWS.EC2.RestoreAddressToClassic

    -- ** DescribeManagedPrefixLists (Paginated)
    , module Network.AWS.EC2.DescribeManagedPrefixLists

    -- ** CreateKeyPair 
    , module Network.AWS.EC2.CreateKeyPair

    -- ** GetReservedInstancesExchangeQuote 
    , module Network.AWS.EC2.GetReservedInstancesExchangeQuote

    -- ** DeleteVolume 
    , module Network.AWS.EC2.DeleteVolume

    -- ** DeprovisionByoipCidr 
    , module Network.AWS.EC2.DeprovisionByoipCidr

    -- ** DeleteVpcEndpointServiceConfigurations 
    , module Network.AWS.EC2.DeleteVpcEndpointServiceConfigurations

    -- ** DescribeSpotFleetInstances (Paginated)
    , module Network.AWS.EC2.DescribeSpotFleetInstances

    -- * Types

    -- ** CarrierGateway
    , CarrierGateway (..)
    , mkCarrierGateway
    , cgCarrierGatewayId
    , cgOwnerId
    , cgState
    , cgTags
    , cgVpcId

    -- ** FleetCapacityReservationUsageStrategy
    , FleetCapacityReservationUsageStrategy (..)

    -- ** InstanceMarketOptionsRequest
    , InstanceMarketOptionsRequest (..)
    , mkInstanceMarketOptionsRequest
    , imorMarketType
    , imorSpotOptions

    -- ** ImageAttributeName
    , ImageAttributeName (..)

    -- ** VpcPeeringConnectionId
    , VpcPeeringConnectionId (..)

    -- ** FleetLaunchTemplateOverrides
    , FleetLaunchTemplateOverrides (..)
    , mkFleetLaunchTemplateOverrides
    , fltoAvailabilityZone
    , fltoInstanceType
    , fltoMaxPrice
    , fltoPlacement
    , fltoPriority
    , fltoSubnetId
    , fltoWeightedCapacity

    -- ** ExportTaskS3Location
    , ExportTaskS3Location (..)
    , mkExportTaskS3Location
    , etslS3Bucket
    , etslS3Prefix

    -- ** PrincipalIdFormat
    , PrincipalIdFormat (..)
    , mkPrincipalIdFormat
    , pifArn
    , pifStatuses

    -- ** Ipv6CidrBlock
    , Ipv6CidrBlock (..)
    , mkIpv6CidrBlock
    , icbIpv6CidrBlock

    -- ** PermissionGroup
    , PermissionGroup (..)

    -- ** MarketType
    , MarketType (..)

    -- ** LocalGatewayVirtualInterfaceId
    , LocalGatewayVirtualInterfaceId (..)

    -- ** TransitGatewayAttachmentResourceType
    , TransitGatewayAttachmentResourceType (..)

    -- ** StorageLocation
    , StorageLocation (..)
    , mkStorageLocation
    , slBucket
    , slKey

    -- ** InstanceId
    , InstanceId (..)

    -- ** LoadPermissionRequest
    , LoadPermissionRequest (..)
    , mkLoadPermissionRequest
    , lprGroup
    , lprUserId

    -- ** RouteTableAssociationStateCode
    , RouteTableAssociationStateCode (..)

    -- ** ClientVpnRouteStatus
    , ClientVpnRouteStatus (..)
    , mkClientVpnRouteStatus
    , cvrsCode
    , cvrsMessage

    -- ** ExportTaskId
    , ExportTaskId (..)

    -- ** AddressStatus
    , AddressStatus (..)

    -- ** NetworkAclEntry
    , NetworkAclEntry (..)
    , mkNetworkAclEntry
    , naeCidrBlock
    , naeEgress
    , naeIcmpTypeCode
    , naeIpv6CidrBlock
    , naePortRange
    , naeProtocol
    , naeRuleAction
    , naeRuleNumber

    -- ** CapacityReservationOptions
    , CapacityReservationOptions (..)
    , mkCapacityReservationOptions
    , croUsageStrategy

    -- ** Phase1DHGroupNumbersListValue
    , Phase1DHGroupNumbersListValue (..)
    , mkPhase1DHGroupNumbersListValue
    , pdhgnlvValue

    -- ** BlobAttributeValue
    , BlobAttributeValue (..)
    , mkBlobAttributeValue
    , bavValue

    -- ** Phase1IntegrityAlgorithmsListValue
    , Phase1IntegrityAlgorithmsListValue (..)
    , mkPhase1IntegrityAlgorithmsListValue
    , pialvValue

    -- ** ServiceTypeDetail
    , ServiceTypeDetail (..)
    , mkServiceTypeDetail
    , stdServiceType

    -- ** LaunchTemplateElasticInferenceAccelerator
    , LaunchTemplateElasticInferenceAccelerator (..)
    , mkLaunchTemplateElasticInferenceAccelerator
    , lteiaType
    , lteiaCount

    -- ** TransitGatewayPrefixListAttachment
    , TransitGatewayPrefixListAttachment (..)
    , mkTransitGatewayPrefixListAttachment
    , tgplaResourceId
    , tgplaResourceType
    , tgplaTransitGatewayAttachmentId

    -- ** ImportInstanceLaunchSpecification
    , ImportInstanceLaunchSpecification (..)
    , mkImportInstanceLaunchSpecification
    , iilsAdditionalInfo
    , iilsArchitecture
    , iilsGroupIds
    , iilsGroupNames
    , iilsInstanceInitiatedShutdownBehavior
    , iilsInstanceType
    , iilsMonitoring
    , iilsPlacement
    , iilsPrivateIpAddress
    , iilsSubnetId
    , iilsUserData

    -- ** RequestLaunchTemplateData
    , RequestLaunchTemplateData (..)
    , mkRequestLaunchTemplateData
    , rltdBlockDeviceMappings
    , rltdCapacityReservationSpecification
    , rltdCpuOptions
    , rltdCreditSpecification
    , rltdDisableApiTermination
    , rltdEbsOptimized
    , rltdElasticGpuSpecifications
    , rltdElasticInferenceAccelerators
    , rltdEnclaveOptions
    , rltdHibernationOptions
    , rltdIamInstanceProfile
    , rltdImageId
    , rltdInstanceInitiatedShutdownBehavior
    , rltdInstanceMarketOptions
    , rltdInstanceType
    , rltdKernelId
    , rltdKeyName
    , rltdLicenseSpecifications
    , rltdMetadataOptions
    , rltdMonitoring
    , rltdNetworkInterfaces
    , rltdPlacement
    , rltdRamDiskId
    , rltdSecurityGroupIds
    , rltdSecurityGroups
    , rltdTagSpecifications
    , rltdUserData

    -- ** Snapshot
    , Snapshot (..)
    , mkSnapshot
    , sDataEncryptionKeyId
    , sDescription
    , sEncrypted
    , sKmsKeyId
    , sOwnerAlias
    , sOwnerId
    , sProgress
    , sSnapshotId
    , sStartTime
    , sState
    , sStateMessage
    , sTags
    , sVolumeId
    , sVolumeSize

    -- ** ConnectionNotificationState
    , ConnectionNotificationState (..)

    -- ** SpotInstanceStateFault
    , SpotInstanceStateFault (..)
    , mkSpotInstanceStateFault
    , sisfCode
    , sisfMessage

    -- ** InferenceDeviceName
    , InferenceDeviceName (..)

    -- ** PeeringConnectionOptions
    , PeeringConnectionOptions (..)
    , mkPeeringConnectionOptions
    , pcoAllowDnsResolutionFromRemoteVpc
    , pcoAllowEgressFromLocalClassicLinkToRemoteVpc
    , pcoAllowEgressFromLocalVpcToRemoteClassicLink

    -- ** ClientConnectResponseOptions
    , ClientConnectResponseOptions (..)
    , mkClientConnectResponseOptions
    , ccroEnabled
    , ccroLambdaFunctionArn
    , ccroStatus

    -- ** TagDescription
    , TagDescription (..)
    , mkTagDescription
    , tdKey
    , tdResourceId
    , tdResourceType
    , tdValue

    -- ** ImportSnapshotTask
    , ImportSnapshotTask (..)
    , mkImportSnapshotTask
    , istDescription
    , istImportTaskId
    , istSnapshotTaskDetail
    , istTags

    -- ** GroupIdentifier
    , GroupIdentifier (..)
    , mkGroupIdentifier
    , giGroupId
    , giGroupName

    -- ** RouteGatewayId
    , RouteGatewayId (..)

    -- ** VpnStaticRouteSource
    , VpnStaticRouteSource (..)

    -- ** ScheduledInstanceRecurrence
    , ScheduledInstanceRecurrence (..)
    , mkScheduledInstanceRecurrence
    , sirFrequency
    , sirInterval
    , sirOccurrenceDaySet
    , sirOccurrenceRelativeToEnd
    , sirOccurrenceUnit

    -- ** LocalGatewayRoute
    , LocalGatewayRoute (..)
    , mkLocalGatewayRoute
    , lgrDestinationCidrBlock
    , lgrLocalGatewayRouteTableArn
    , lgrLocalGatewayRouteTableId
    , lgrLocalGatewayVirtualInterfaceGroupId
    , lgrOwnerId
    , lgrState
    , lgrType

    -- ** TrafficMirrorRuleAction
    , TrafficMirrorRuleAction (..)

    -- ** Phase1EncryptionAlgorithmsListValue
    , Phase1EncryptionAlgorithmsListValue (..)
    , mkPhase1EncryptionAlgorithmsListValue
    , pealvfValue

    -- ** DirectoryServiceAuthentication
    , DirectoryServiceAuthentication (..)
    , mkDirectoryServiceAuthentication
    , dsaDirectoryId

    -- ** AssociatedRole
    , AssociatedRole (..)
    , mkAssociatedRole
    , arAssociatedRoleArn
    , arCertificateS3BucketName
    , arCertificateS3ObjectKey
    , arEncryptionKmsKeyId

    -- ** ReservedInstancesListing
    , ReservedInstancesListing (..)
    , mkReservedInstancesListing
    , rilClientToken
    , rilCreateDate
    , rilInstanceCounts
    , rilPriceSchedules
    , rilReservedInstancesId
    , rilReservedInstancesListingId
    , rilStatus
    , rilStatusMessage
    , rilTags
    , rilUpdateDate

    -- ** AssociatedTargetNetwork
    , AssociatedTargetNetwork (..)
    , mkAssociatedTargetNetwork
    , atnNetworkId
    , atnNetworkType

    -- ** PciId
    , PciId (..)
    , mkPciId
    , piDeviceId
    , piSubsystemId
    , piSubsystemVendorId
    , piVendorId

    -- ** InstanceLifecycleType
    , InstanceLifecycleType (..)

    -- ** ExportVmTaskId
    , ExportVmTaskId (..)

    -- ** RegisterInstanceTagAttributeRequest
    , RegisterInstanceTagAttributeRequest (..)
    , mkRegisterInstanceTagAttributeRequest
    , ritarIncludeAllTagsOfInstance
    , ritarInstanceTagKeys

    -- ** ManagedPrefixList
    , ManagedPrefixList (..)
    , mkManagedPrefixList
    , mplAddressFamily
    , mplMaxEntries
    , mplOwnerId
    , mplPrefixListArn
    , mplPrefixListId
    , mplPrefixListName
    , mplState
    , mplStateMessage
    , mplTags
    , mplVersion

    -- ** State
    , State (..)

    -- ** EbsOptimizedInfo
    , EbsOptimizedInfo (..)
    , mkEbsOptimizedInfo
    , eoiBaselineBandwidthInMbps
    , eoiBaselineIops
    , eoiBaselineThroughputInMBps
    , eoiMaximumBandwidthInMbps
    , eoiMaximumIops
    , eoiMaximumThroughputInMBps

    -- ** VirtualizationType
    , VirtualizationType (..)

    -- ** PeeringAttachmentStatus
    , PeeringAttachmentStatus (..)
    , mkPeeringAttachmentStatus
    , pasCode
    , pasMessage

    -- ** NetworkInterfaceStatus
    , NetworkInterfaceStatus (..)

    -- ** PlatformValues
    , PlatformValues (..)

    -- ** Phase2EncryptionAlgorithmsListValue
    , Phase2EncryptionAlgorithmsListValue (..)
    , mkPhase2EncryptionAlgorithmsListValue
    , pealvValue

    -- ** VpcPeeringConnectionOptionsDescription
    , VpcPeeringConnectionOptionsDescription (..)
    , mkVpcPeeringConnectionOptionsDescription
    , vpcodAllowDnsResolutionFromRemoteVpc
    , vpcodAllowEgressFromLocalClassicLinkToRemoteVpc
    , vpcodAllowEgressFromLocalVpcToRemoteClassicLink

    -- ** EgressOnlyInternetGatewayId
    , EgressOnlyInternetGatewayId (..)

    -- ** EnclaveOptionsRequest
    , EnclaveOptionsRequest (..)
    , mkEnclaveOptionsRequest
    , eorEnabled

    -- ** TunnelInsideIpVersion
    , TunnelInsideIpVersion (..)

    -- ** AutoAcceptSharedAttachmentsValue
    , AutoAcceptSharedAttachmentsValue (..)

    -- ** VpnEcmpSupportValue
    , VpnEcmpSupportValue (..)

    -- ** TransitGatewayOptions
    , TransitGatewayOptions (..)
    , mkTransitGatewayOptions
    , tgoAmazonSideAsn
    , tgoAssociationDefaultRouteTableId
    , tgoAutoAcceptSharedAttachments
    , tgoDefaultRouteTableAssociation
    , tgoDefaultRouteTablePropagation
    , tgoDnsSupport
    , tgoMulticastSupport
    , tgoPropagationDefaultRouteTableId
    , tgoVpnEcmpSupport

    -- ** CreateVolumePermission
    , CreateVolumePermission (..)
    , mkCreateVolumePermission
    , cvpGroup
    , cvpUserId

    -- ** InstanceFamilyCreditSpecification
    , InstanceFamilyCreditSpecification (..)
    , mkInstanceFamilyCreditSpecification
    , ifcsCpuCredits
    , ifcsInstanceFamily

    -- ** EnableFastSnapshotRestoreStateErrorItem
    , EnableFastSnapshotRestoreStateErrorItem (..)
    , mkEnableFastSnapshotRestoreStateErrorItem
    , efsrseiAvailabilityZone
    , efsrseiError

    -- ** NetworkInterfaceAttachmentChanges
    , NetworkInterfaceAttachmentChanges (..)
    , mkNetworkInterfaceAttachmentChanges
    , niacAttachmentId
    , niacDeleteOnTermination

    -- ** MemoryInfo
    , MemoryInfo (..)
    , mkMemoryInfo
    , miSizeInMiB

    -- ** RecurringChargeFrequency
    , RecurringChargeFrequency (..)

    -- ** ScheduledInstancesEbs
    , ScheduledInstancesEbs (..)
    , mkScheduledInstancesEbs
    , sieDeleteOnTermination
    , sieEncrypted
    , sieIops
    , sieSnapshotId
    , sieVolumeSize
    , sieVolumeType

    -- ** ModifyAvailabilityZoneOptInStatus
    , ModifyAvailabilityZoneOptInStatus (..)

    -- ** DhcpOptions
    , DhcpOptions (..)
    , mkDhcpOptions
    , doDhcpConfigurations
    , doDhcpOptionsId
    , doOwnerId
    , doTags

    -- ** SlotStartTimeRangeRequest
    , SlotStartTimeRangeRequest (..)
    , mkSlotStartTimeRangeRequest
    , sstrrEarliestTime
    , sstrrLatestTime

    -- ** InstanceNetworkInterfaceSpecification
    , InstanceNetworkInterfaceSpecification (..)
    , mkInstanceNetworkInterfaceSpecification
    , inisAssociateCarrierIpAddress
    , inisAssociatePublicIpAddress
    , inisDeleteOnTermination
    , inisDescription
    , inisDeviceIndex
    , inisGroups
    , inisInterfaceType
    , inisIpv6AddressCount
    , inisIpv6Addresses
    , inisNetworkCardIndex
    , inisNetworkInterfaceId
    , inisPrivateIpAddress
    , inisPrivateIpAddresses
    , inisSecondaryPrivateIpAddressCount
    , inisSubnetId

    -- ** FleetLaunchTemplateSpecificationRequest
    , FleetLaunchTemplateSpecificationRequest (..)
    , mkFleetLaunchTemplateSpecificationRequest
    , fltsrLaunchTemplateId
    , fltsrLaunchTemplateName
    , fltsrVersion

    -- ** OnDemandOptionsRequest
    , OnDemandOptionsRequest (..)
    , mkOnDemandOptionsRequest
    , odorAllocationStrategy
    , odorCapacityReservationOptions
    , odorMaxTotalPrice
    , odorMinTargetCapacity
    , odorSingleAvailabilityZone
    , odorSingleInstanceType

    -- ** InstanceTypeInfo
    , InstanceTypeInfo (..)
    , mkInstanceTypeInfo
    , itiAutoRecoverySupported
    , itiBareMetal
    , itiBurstablePerformanceSupported
    , itiCurrentGeneration
    , itiDedicatedHostsSupported
    , itiEbsInfo
    , itiFpgaInfo
    , itiFreeTierEligible
    , itiGpuInfo
    , itiHibernationSupported
    , itiHypervisor
    , itiInferenceAcceleratorInfo
    , itiInstanceStorageInfo
    , itiInstanceStorageSupported
    , itiInstanceType
    , itiMemoryInfo
    , itiNetworkInfo
    , itiPlacementGroupInfo
    , itiProcessorInfo
    , itiSupportedRootDeviceTypes
    , itiSupportedUsageClasses
    , itiSupportedVirtualizationTypes
    , itiVCpuInfo

    -- ** Phase2DHGroupNumbersListValue
    , Phase2DHGroupNumbersListValue (..)
    , mkPhase2DHGroupNumbersListValue
    , pValue

    -- ** ReservationState
    , ReservationState (..)

    -- ** FpgaDeviceName
    , FpgaDeviceName (..)

    -- ** TransitGateway
    , TransitGateway (..)
    , mkTransitGateway
    , tgCreationTime
    , tgDescription
    , tgOptions
    , tgOwnerId
    , tgState
    , tgTags
    , tgTransitGatewayArn
    , tgTransitGatewayId

    -- ** VolumeState
    , VolumeState (..)

    -- ** AddPrefixListEntry
    , AddPrefixListEntry (..)
    , mkAddPrefixListEntry
    , apleCidr
    , apleDescription

    -- ** CpuOptionsRequest
    , CpuOptionsRequest (..)
    , mkCpuOptionsRequest
    , corCoreCount
    , corThreadsPerCore

    -- ** ClientCertificateRevocationListStatusCode
    , ClientCertificateRevocationListStatusCode (..)

    -- ** TransitGatewayMulticastRegisteredGroupMembers
    , TransitGatewayMulticastRegisteredGroupMembers (..)
    , mkTransitGatewayMulticastRegisteredGroupMembers
    , tgmrgmGroupIpAddress
    , tgmrgmRegisteredNetworkInterfaceIds
    , tgmrgmTransitGatewayMulticastDomainId

    -- ** VpcPeeringConnectionStateReasonCode
    , VpcPeeringConnectionStateReasonCode (..)

    -- ** AttributeValue
    , AttributeValue (..)
    , mkAttributeValue
    , avValue

    -- ** LaunchTemplateElasticInferenceAcceleratorResponse
    , LaunchTemplateElasticInferenceAcceleratorResponse (..)
    , mkLaunchTemplateElasticInferenceAcceleratorResponse
    , lteiarCount
    , lteiarType

    -- ** ClientVpnConnection
    , ClientVpnConnection (..)
    , mkClientVpnConnection
    , cvcClientIp
    , cvcClientVpnEndpointId
    , cvcCommonName
    , cvcConnectionEndTime
    , cvcConnectionEstablishedTime
    , cvcConnectionId
    , cvcEgressBytes
    , cvcEgressPackets
    , cvcIngressBytes
    , cvcIngressPackets
    , cvcPostureComplianceStatuses
    , cvcStatus
    , cvcTimestamp
    , cvcUsername

    -- ** PrivateIpAddressSpecification
    , PrivateIpAddressSpecification (..)
    , mkPrivateIpAddressSpecification
    , piasPrimary
    , piasPrivateIpAddress

    -- ** Ipv6Address
    , Ipv6Address (..)

    -- ** DeleteQueuedReservedInstancesErrorCode
    , DeleteQueuedReservedInstancesErrorCode (..)

    -- ** Image
    , Image (..)
    , mkImage
    , ifArchitecture
    , ifBlockDeviceMappings
    , ifCreationDate
    , ifDescription
    , ifEnaSupport
    , ifHypervisor
    , ifImageId
    , ifImageLocation
    , ifImageOwnerAlias
    , ifImageType
    , ifKernelId
    , ifName
    , ifOwnerId
    , ifPlatform
    , ifPlatformDetails
    , ifProductCodes
    , ifPublic
    , ifRamdiskId
    , ifRootDeviceName
    , ifRootDeviceType
    , ifSriovNetSupport
    , ifState
    , ifStateReason
    , ifTags
    , ifUsageOperation
    , ifVirtualizationType

    -- ** DhcpConfiguration
    , DhcpConfiguration (..)
    , mkDhcpConfiguration
    , dcKey
    , dcValues

    -- ** CancelSpotFleetRequestsError
    , CancelSpotFleetRequestsError (..)
    , mkCancelSpotFleetRequestsError
    , csfreCode
    , csfreMessage

    -- ** InstanceTagNotificationAttribute
    , InstanceTagNotificationAttribute (..)
    , mkInstanceTagNotificationAttribute
    , itnaIncludeAllTagsOfInstance
    , itnaInstanceTagKeys

    -- ** Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- ** CapacityReservationSpecificationResponse
    , CapacityReservationSpecificationResponse (..)
    , mkCapacityReservationSpecificationResponse
    , crsrCapacityReservationPreference
    , crsrCapacityReservationTarget

    -- ** NetworkInterfacePermissionStateCode
    , NetworkInterfacePermissionStateCode (..)

    -- ** DeleteFleetErrorItem
    , DeleteFleetErrorItem (..)
    , mkDeleteFleetErrorItem
    , dfeiError
    , dfeiFleetId

    -- ** AccountAttributeName
    , AccountAttributeName (..)

    -- ** LaunchTemplateAndOverridesResponse
    , LaunchTemplateAndOverridesResponse (..)
    , mkLaunchTemplateAndOverridesResponse
    , ltaorLaunchTemplateSpecification
    , ltaorOverrides

    -- ** TransitGatewayMulticastDomainAssociation
    , TransitGatewayMulticastDomainAssociation (..)
    , mkTransitGatewayMulticastDomainAssociation
    , tgmdaResourceId
    , tgmdaResourceType
    , tgmdaSubnet
    , tgmdaTransitGatewayAttachmentId

    -- ** LaunchTemplateHibernationOptions
    , LaunchTemplateHibernationOptions (..)
    , mkLaunchTemplateHibernationOptions
    , lthoConfigured

    -- ** NetworkInterfaceAttachment
    , NetworkInterfaceAttachment (..)
    , mkNetworkInterfaceAttachment
    , niaAttachTime
    , niaAttachmentId
    , niaDeleteOnTermination
    , niaDeviceIndex
    , niaInstanceId
    , niaInstanceOwnerId
    , niaNetworkCardIndex
    , niaStatus

    -- ** ConnectionLogOptions
    , ConnectionLogOptions (..)
    , mkConnectionLogOptions
    , cloCloudwatchLogGroup
    , cloCloudwatchLogStream
    , cloEnabled

    -- ** SpotFleetMonitoring
    , SpotFleetMonitoring (..)
    , mkSpotFleetMonitoring
    , sfmEnabled

    -- ** RunInstancesMonitoringEnabled
    , RunInstancesMonitoringEnabled (..)
    , mkRunInstancesMonitoringEnabled
    , rimeEnabled

    -- ** VolumeStatusInfo
    , VolumeStatusInfo (..)
    , mkVolumeStatusInfo
    , vsiDetails
    , vsiStatus

    -- ** RouteTableId
    , RouteTableId (..)

    -- ** CapacityReservationInstancePlatform
    , CapacityReservationInstancePlatform (..)

    -- ** NetworkInterfaceAssociation
    , NetworkInterfaceAssociation (..)
    , mkNetworkInterfaceAssociation
    , niaAllocationId
    , niaAssociationId
    , niaCarrierIp
    , niaCustomerOwnedIp
    , niaIpOwnerId
    , niaPublicDnsName
    , niaPublicIp

    -- ** LaunchTemplateOverrides
    , LaunchTemplateOverrides (..)
    , mkLaunchTemplateOverrides
    , ltoAvailabilityZone
    , ltoInstanceType
    , ltoPriority
    , ltoSpotPrice
    , ltoSubnetId
    , ltoWeightedCapacity

    -- ** SecurityGroupIdentifier
    , SecurityGroupIdentifier (..)
    , mkSecurityGroupIdentifier
    , sgiGroupId
    , sgiGroupName

    -- ** TrafficMirrorSession
    , TrafficMirrorSession (..)
    , mkTrafficMirrorSession
    , tmsDescription
    , tmsNetworkInterfaceId
    , tmsOwnerId
    , tmsPacketLength
    , tmsSessionNumber
    , tmsTags
    , tmsTrafficMirrorFilterId
    , tmsTrafficMirrorSessionId
    , tmsTrafficMirrorTargetId
    , tmsVirtualNetworkId

    -- ** DisableFastSnapshotRestoreStateErrorItem
    , DisableFastSnapshotRestoreStateErrorItem (..)
    , mkDisableFastSnapshotRestoreStateErrorItem
    , dfsrseiAvailabilityZone
    , dfsrseiError

    -- ** CreateVolumePermissionModifications
    , CreateVolumePermissionModifications (..)
    , mkCreateVolumePermissionModifications
    , cvpmAdd
    , cvpmRemove

    -- ** SubnetAssociation
    , SubnetAssociation (..)
    , mkSubnetAssociation
    , saState
    , saSubnetId

    -- ** SnapshotInfo
    , SnapshotInfo (..)
    , mkSnapshotInfo
    , siDescription
    , siEncrypted
    , siOwnerId
    , siProgress
    , siSnapshotId
    , siStartTime
    , siState
    , siTags
    , siVolumeId
    , siVolumeSize

    -- ** ScheduledInstancesPlacement
    , ScheduledInstancesPlacement (..)
    , mkScheduledInstancesPlacement
    , sipAvailabilityZone
    , sipGroupName

    -- ** LocalGatewayRouteTable
    , LocalGatewayRouteTable (..)
    , mkLocalGatewayRouteTable
    , lgrtLocalGatewayId
    , lgrtLocalGatewayRouteTableArn
    , lgrtLocalGatewayRouteTableId
    , lgrtOutpostArn
    , lgrtOwnerId
    , lgrtState
    , lgrtTags

    -- ** ModifyTransitGatewayVpcAttachmentRequestOptions
    , ModifyTransitGatewayVpcAttachmentRequestOptions (..)
    , mkModifyTransitGatewayVpcAttachmentRequestOptions
    , mtgvaroApplianceModeSupport
    , mtgvaroDnsSupport
    , mtgvaroIpv6Support

    -- ** ElasticGpuId
    , ElasticGpuId (..)

    -- ** LaunchTemplateName
    , LaunchTemplateName (..)

    -- ** VpcState
    , VpcState (..)

    -- ** ResourceType
    , ResourceType (..)

    -- ** ReportStatusType
    , ReportStatusType (..)

    -- ** ImportImageTaskId
    , ImportImageTaskId (..)

    -- ** TrafficMirrorFilterRule
    , TrafficMirrorFilterRule (..)
    , mkTrafficMirrorFilterRule
    , tmfrDescription
    , tmfrDestinationCidrBlock
    , tmfrDestinationPortRange
    , tmfrProtocol
    , tmfrRuleAction
    , tmfrRuleNumber
    , tmfrSourceCidrBlock
    , tmfrSourcePortRange
    , tmfrTrafficDirection
    , tmfrTrafficMirrorFilterId
    , tmfrTrafficMirrorFilterRuleId

    -- ** Affinity
    , Affinity (..)

    -- ** CurrencyCodeValues
    , CurrencyCodeValues (..)

    -- ** FleetSpotCapacityRebalanceRequest
    , FleetSpotCapacityRebalanceRequest (..)
    , mkFleetSpotCapacityRebalanceRequest
    , fscrrReplacementStrategy

    -- ** IcmpTypeCode
    , IcmpTypeCode (..)
    , mkIcmpTypeCode
    , itcCode
    , itcType

    -- ** LaunchTemplateCpuOptionsRequest
    , LaunchTemplateCpuOptionsRequest (..)
    , mkLaunchTemplateCpuOptionsRequest
    , ltcorCoreCount
    , ltcorThreadsPerCore

    -- ** DeleteFleetErrorCode
    , DeleteFleetErrorCode (..)

    -- ** LaunchTemplateInstanceNetworkInterfaceSpecification
    , LaunchTemplateInstanceNetworkInterfaceSpecification (..)
    , mkLaunchTemplateInstanceNetworkInterfaceSpecification
    , ltinisAssociateCarrierIpAddress
    , ltinisAssociatePublicIpAddress
    , ltinisDeleteOnTermination
    , ltinisDescription
    , ltinisDeviceIndex
    , ltinisGroups
    , ltinisInterfaceType
    , ltinisIpv6AddressCount
    , ltinisIpv6Addresses
    , ltinisNetworkCardIndex
    , ltinisNetworkInterfaceId
    , ltinisPrivateIpAddress
    , ltinisPrivateIpAddresses
    , ltinisSecondaryPrivateIpAddressCount
    , ltinisSubnetId

    -- ** AllowedPrincipal
    , AllowedPrincipal (..)
    , mkAllowedPrincipal
    , apPrincipal
    , apPrincipalType

    -- ** LaunchTemplatesMonitoring
    , LaunchTemplatesMonitoring (..)
    , mkLaunchTemplatesMonitoring
    , ltmEnabled

    -- ** PrincipalType
    , PrincipalType (..)

    -- ** LaunchTemplateTagSpecification
    , LaunchTemplateTagSpecification (..)
    , mkLaunchTemplateTagSpecification
    , lttsResourceType
    , lttsTags

    -- ** ClientVpnEndpoint
    , ClientVpnEndpoint (..)
    , mkClientVpnEndpoint
    , cveAssociatedTargetNetworks
    , cveAuthenticationOptions
    , cveClientCidrBlock
    , cveClientConnectOptions
    , cveClientVpnEndpointId
    , cveConnectionLogOptions
    , cveCreationTime
    , cveDeletionTime
    , cveDescription
    , cveDnsName
    , cveDnsServers
    , cveSecurityGroupIds
    , cveSelfServicePortalUrl
    , cveServerCertificateArn
    , cveSplitTunnel
    , cveStatus
    , cveTags
    , cveTransportProtocol
    , cveVpcId
    , cveVpnPort
    , cveVpnProtocol

    -- ** HostTenancy
    , HostTenancy (..)

    -- ** GpuDeviceManufacturerName
    , GpuDeviceManufacturerName (..)

    -- ** FpgaImageId
    , FpgaImageId (..)

    -- ** InstanceCount
    , InstanceCount (..)
    , mkInstanceCount
    , icInstanceCount
    , icState

    -- ** ClientVpnEndpointAttributeStatus
    , ClientVpnEndpointAttributeStatus (..)
    , mkClientVpnEndpointAttributeStatus
    , cveasCode
    , cveasMessage

    -- ** ExportToS3Task
    , ExportToS3Task (..)
    , mkExportToS3Task
    , etstContainerFormat
    , etstDiskImageFormat
    , etstS3Bucket
    , etstS3Key

    -- ** VpcCidrBlockStateCode
    , VpcCidrBlockStateCode (..)

    -- ** PrefixList
    , PrefixList (..)
    , mkPrefixList
    , plCidrs
    , plPrefixListId
    , plPrefixListName

    -- ** LaunchTemplateEnclaveOptionsRequest
    , LaunchTemplateEnclaveOptionsRequest (..)
    , mkLaunchTemplateEnclaveOptionsRequest
    , lteorEnabled

    -- ** InstanceInterruptionBehavior
    , InstanceInterruptionBehavior (..)

    -- ** Location
    , Location (..)

    -- ** FleetExcessCapacityTerminationPolicy
    , FleetExcessCapacityTerminationPolicy (..)

    -- ** InstanceMetadataOptionsRequest
    , InstanceMetadataOptionsRequest (..)
    , mkInstanceMetadataOptionsRequest
    , iHttpEndpoint
    , iHttpPutResponseHopLimit
    , iHttpTokens

    -- ** ClientVpnAuthorizationRuleStatusCode
    , ClientVpnAuthorizationRuleStatusCode (..)

    -- ** BlockDeviceMapping
    , BlockDeviceMapping (..)
    , mkBlockDeviceMapping
    , bdmDeviceName
    , bdmEbs
    , bdmNoDevice
    , bdmVirtualName

    -- ** InstanceCapacity
    , InstanceCapacity (..)
    , mkInstanceCapacity
    , icAvailableCapacity
    , icInstanceType
    , icTotalCapacity

    -- ** TransitGatewayRouteTableId
    , TransitGatewayRouteTableId (..)

    -- ** DnsSupportValue
    , DnsSupportValue (..)

    -- ** NetworkAclId
    , NetworkAclId (..)

    -- ** LaunchTemplateInstanceMetadataEndpointState
    , LaunchTemplateInstanceMetadataEndpointState (..)

    -- ** AllocationId
    , AllocationId (..)

    -- ** SensitiveUserData
    , SensitiveUserData (..)

    -- ** ConversionTask
    , ConversionTask (..)
    , mkConversionTask
    , ctConversionTaskId
    , ctExpirationTime
    , ctImportInstance
    , ctImportVolume
    , ctState
    , ctStatusMessage
    , ctTags

    -- ** AttachmentStatus
    , AttachmentStatus (..)

    -- ** SpotCapacityRebalance
    , SpotCapacityRebalance (..)
    , mkSpotCapacityRebalance
    , scrReplacementStrategy

    -- ** GpuDeviceName
    , GpuDeviceName (..)

    -- ** FederatedAuthenticationRequest
    , FederatedAuthenticationRequest (..)
    , mkFederatedAuthenticationRequest
    , farSAMLProviderArn
    , farSelfServiceSAMLProviderArn

    -- ** ClassicLinkInstance
    , ClassicLinkInstance (..)
    , mkClassicLinkInstance
    , cliGroups
    , cliInstanceId
    , cliTags
    , cliVpcId

    -- ** TransportProtocol
    , TransportProtocol (..)

    -- ** RouteOrigin
    , RouteOrigin (..)

    -- ** PrefixListState
    , PrefixListState (..)

    -- ** ListingState
    , ListingState (..)

    -- ** SpotPrice
    , SpotPrice (..)
    , mkSpotPrice
    , sAvailabilityZone
    , sInstanceType
    , sProductDescription
    , sSpotPrice
    , sTimestamp

    -- ** ActiveInstance
    , ActiveInstance (..)
    , mkActiveInstance
    , aiInstanceHealth
    , aiInstanceId
    , aiInstanceType
    , aiSpotInstanceRequestId

    -- ** TrafficType
    , TrafficType (..)

    -- ** LaunchTemplatePlacementRequest
    , LaunchTemplatePlacementRequest (..)
    , mkLaunchTemplatePlacementRequest
    , ltprAffinity
    , ltprAvailabilityZone
    , ltprGroupName
    , ltprHostId
    , ltprHostResourceGroupArn
    , ltprPartitionNumber
    , ltprSpreadDomain
    , ltprTenancy

    -- ** LaunchTemplateCapacityReservationSpecificationRequest
    , LaunchTemplateCapacityReservationSpecificationRequest (..)
    , mkLaunchTemplateCapacityReservationSpecificationRequest
    , ltcrsrCapacityReservationPreference
    , ltcrsrCapacityReservationTarget

    -- ** VpnGatewayId
    , VpnGatewayId (..)

    -- ** SpotFleetRequestConfigData
    , SpotFleetRequestConfigData (..)
    , mkSpotFleetRequestConfigData
    , sfrcdIamFleetRole
    , sfrcdTargetCapacity
    , sfrcdAllocationStrategy
    , sfrcdClientToken
    , sfrcdExcessCapacityTerminationPolicy
    , sfrcdFulfilledCapacity
    , sfrcdInstanceInterruptionBehavior
    , sfrcdInstancePoolsToUseCount
    , sfrcdLaunchSpecifications
    , sfrcdLaunchTemplateConfigs
    , sfrcdLoadBalancersConfig
    , sfrcdOnDemandAllocationStrategy
    , sfrcdOnDemandFulfilledCapacity
    , sfrcdOnDemandMaxTotalPrice
    , sfrcdOnDemandTargetCapacity
    , sfrcdReplaceUnhealthyInstances
    , sfrcdSpotMaintenanceStrategies
    , sfrcdSpotMaxTotalPrice
    , sfrcdSpotPrice
    , sfrcdTagSpecifications
    , sfrcdTerminateInstancesWithExpiration
    , sfrcdType
    , sfrcdValidFrom
    , sfrcdValidUntil

    -- ** ReplacementStrategy
    , ReplacementStrategy (..)

    -- ** ArchitectureType
    , ArchitectureType (..)

    -- ** EnaSupport
    , EnaSupport (..)

    -- ** AvailableCapacity
    , AvailableCapacity (..)
    , mkAvailableCapacity
    , acAvailableInstanceCapacity
    , acAvailableVCpus

    -- ** NatGatewayAddress
    , NatGatewayAddress (..)
    , mkNatGatewayAddress
    , ngaAllocationId
    , ngaNetworkInterfaceId
    , ngaPrivateIp
    , ngaPublicIp

    -- ** InstanceMonitoring
    , InstanceMonitoring (..)
    , mkInstanceMonitoring
    , imInstanceId
    , imMonitoring

    -- ** ScheduledInstanceId
    , ScheduledInstanceId (..)

    -- ** HostReservationId
    , HostReservationId (..)

    -- ** PlacementGroupStrategy
    , PlacementGroupStrategy (..)

    -- ** TrafficMirrorTargetType
    , TrafficMirrorTargetType (..)

    -- ** CapacityReservationTarget
    , CapacityReservationTarget (..)
    , mkCapacityReservationTarget
    , crtCapacityReservationId
    , crtCapacityReservationResourceGroupArn

    -- ** ModifyVpnTunnelOptionsSpecification
    , ModifyVpnTunnelOptionsSpecification (..)
    , mkModifyVpnTunnelOptionsSpecification
    , mvtosDPDTimeoutAction
    , mvtosDPDTimeoutSeconds
    , mvtosIKEVersions
    , mvtosPhase1DHGroupNumbers
    , mvtosPhase1EncryptionAlgorithms
    , mvtosPhase1IntegrityAlgorithms
    , mvtosPhase1LifetimeSeconds
    , mvtosPhase2DHGroupNumbers
    , mvtosPhase2EncryptionAlgorithms
    , mvtosPhase2IntegrityAlgorithms
    , mvtosPhase2LifetimeSeconds
    , mvtosPreSharedKey
    , mvtosRekeyFuzzPercentage
    , mvtosRekeyMarginTimeSeconds
    , mvtosReplayWindowSize
    , mvtosStartupAction
    , mvtosTunnelInsideCidr
    , mvtosTunnelInsideIpv6Cidr

    -- ** ServiceConfiguration
    , ServiceConfiguration (..)
    , mkServiceConfiguration
    , scAcceptanceRequired
    , scAvailabilityZones
    , scBaseEndpointDnsNames
    , scGatewayLoadBalancerArns
    , scManagesVpcEndpoints
    , scNetworkLoadBalancerArns
    , scPrivateDnsName
    , scPrivateDnsNameConfiguration
    , scServiceId
    , scServiceName
    , scServiceState
    , scServiceType
    , scTags

    -- ** LoadPermission
    , LoadPermission (..)
    , mkLoadPermission
    , lpGroup
    , lpUserId

    -- ** NetworkInterfacePermissionId
    , NetworkInterfacePermissionId (..)

    -- ** PriceScheduleSpecification
    , PriceScheduleSpecification (..)
    , mkPriceScheduleSpecification
    , pssCurrencyCode
    , pssPrice
    , pssTerm

    -- ** StaleIpPermission
    , StaleIpPermission (..)
    , mkStaleIpPermission
    , sipFromPort
    , sipIpProtocol
    , sipIpRanges
    , sipPrefixListIds
    , sipToPort
    , sipUserIdGroupPairs

    -- ** SpotFleetRequestConfig
    , SpotFleetRequestConfig (..)
    , mkSpotFleetRequestConfig
    , sfrcActivityStatus
    , sfrcCreateTime
    , sfrcSpotFleetRequestConfig
    , sfrcSpotFleetRequestId
    , sfrcSpotFleetRequestState
    , sfrcTags

    -- ** PrefixListResourceId
    , PrefixListResourceId (..)

    -- ** SpotInstanceStatus
    , SpotInstanceStatus (..)
    , mkSpotInstanceStatus
    , sisCode
    , sisMessage
    , sisUpdateTime

    -- ** ElasticGpuState
    , ElasticGpuState (..)

    -- ** TargetCapacitySpecification
    , TargetCapacitySpecification (..)
    , mkTargetCapacitySpecification
    , tcsDefaultTargetCapacityType
    , tcsOnDemandTargetCapacity
    , tcsSpotTargetCapacity
    , tcsTotalTargetCapacity

    -- ** SnapshotTaskDetail
    , SnapshotTaskDetail (..)
    , mkSnapshotTaskDetail
    , stdDescription
    , stdDiskImageSize
    , stdEncrypted
    , stdFormat
    , stdKmsKeyId
    , stdProgress
    , stdSnapshotId
    , stdStatus
    , stdStatusMessage
    , stdUrl
    , stdUserBucket

    -- ** VpcIpv6CidrBlockAssociation
    , VpcIpv6CidrBlockAssociation (..)
    , mkVpcIpv6CidrBlockAssociation
    , vicbaAssociationId
    , vicbaIpv6CidrBlock
    , vicbaIpv6CidrBlockState
    , vicbaIpv6Pool
    , vicbaNetworkBorderGroup

    -- ** EnableFastSnapshotRestoreErrorItem
    , EnableFastSnapshotRestoreErrorItem (..)
    , mkEnableFastSnapshotRestoreErrorItem
    , efsreiFastSnapshotRestoreStateErrors
    , efsreiSnapshotId

    -- ** TrafficMirrorSessionField
    , TrafficMirrorSessionField (..)

    -- ** SpotOptions
    , SpotOptions (..)
    , mkSpotOptions
    , soAllocationStrategy
    , soInstanceInterruptionBehavior
    , soInstancePoolsToUseCount
    , soMaintenanceStrategies
    , soMaxTotalPrice
    , soMinTargetCapacity
    , soSingleAvailabilityZone
    , soSingleInstanceType

    -- ** VpcId
    , VpcId (..)

    -- ** AvailabilityZoneState
    , AvailabilityZoneState (..)

    -- ** AllowsMultipleInstanceTypes
    , AllowsMultipleInstanceTypes (..)

    -- ** InstanceMetadataEndpointState
    , InstanceMetadataEndpointState (..)

    -- ** ElasticInferenceAcceleratorAssociation
    , ElasticInferenceAcceleratorAssociation (..)
    , mkElasticInferenceAcceleratorAssociation
    , eiaaElasticInferenceAcceleratorArn
    , eiaaElasticInferenceAcceleratorAssociationId
    , eiaaElasticInferenceAcceleratorAssociationState
    , eiaaElasticInferenceAcceleratorAssociationTime

    -- ** SpotInstanceRequest
    , SpotInstanceRequest (..)
    , mkSpotInstanceRequest
    , sirActualBlockHourlyPrice
    , sirAvailabilityZoneGroup
    , sirBlockDurationMinutes
    , sirCreateTime
    , sirFault
    , sirInstanceId
    , sirInstanceInterruptionBehavior
    , sirLaunchGroup
    , sirLaunchSpecification
    , sirLaunchedAvailabilityZone
    , sirProductDescription
    , sirSpotInstanceRequestId
    , sirSpotPrice
    , sirState
    , sirStatus
    , sirTags
    , sirType
    , sirValidFrom
    , sirValidUntil

    -- ** LaunchTemplateId
    , LaunchTemplateId (..)

    -- ** AssociationStatusCode
    , AssociationStatusCode (..)

    -- ** RemovePrefixListEntry
    , RemovePrefixListEntry (..)
    , mkRemovePrefixListEntry
    , rpleCidr

    -- ** PeeringTgwInfo
    , PeeringTgwInfo (..)
    , mkPeeringTgwInfo
    , ptiOwnerId
    , ptiRegion
    , ptiTransitGatewayId

    -- ** ScheduledInstancesNetworkInterface
    , ScheduledInstancesNetworkInterface (..)
    , mkScheduledInstancesNetworkInterface
    , siniAssociatePublicIpAddress
    , siniDeleteOnTermination
    , siniDescription
    , siniDeviceIndex
    , siniGroups
    , siniIpv6AddressCount
    , siniIpv6Addresses
    , siniNetworkInterfaceId
    , siniPrivateIpAddress
    , siniPrivateIpAddressConfigs
    , siniSecondaryPrivateIpAddressCount
    , siniSubnetId

    -- ** LaunchSpecification
    , LaunchSpecification (..)
    , mkLaunchSpecification
    , lsAddressingType
    , lsBlockDeviceMappings
    , lsEbsOptimized
    , lsIamInstanceProfile
    , lsImageId
    , lsInstanceType
    , lsKernelId
    , lsKeyName
    , lsMonitoring
    , lsNetworkInterfaces
    , lsPlacement
    , lsRamdiskId
    , lsSecurityGroups
    , lsSubnetId
    , lsUserData

    -- ** TrafficDirection
    , TrafficDirection (..)

    -- ** VolumeStatusEvent
    , VolumeStatusEvent (..)
    , mkVolumeStatusEvent
    , vseDescription
    , vseEventId
    , vseEventType
    , vseInstanceId
    , vseNotAfter
    , vseNotBefore

    -- ** SpotMaintenanceStrategies
    , SpotMaintenanceStrategies (..)
    , mkSpotMaintenanceStrategies
    , smsCapacityRebalance

    -- ** Volume
    , Volume (..)
    , mkVolume
    , vAttachments
    , vAvailabilityZone
    , vCreateTime
    , vEncrypted
    , vFastRestored
    , vIops
    , vKmsKeyId
    , vMultiAttachEnabled
    , vOutpostArn
    , vSize
    , vSnapshotId
    , vState
    , vTags
    , vVolumeId
    , vVolumeType

    -- ** Reservation
    , Reservation (..)
    , mkReservation
    , rGroups
    , rInstances
    , rOwnerId
    , rRequesterId
    , rReservationId

    -- ** TrafficMirrorTargetId
    , TrafficMirrorTargetId (..)

    -- ** PlacementGroupInfo
    , PlacementGroupInfo (..)
    , mkPlacementGroupInfo
    , pgiSupportedStrategies

    -- ** FpgaDeviceManufacturerName
    , FpgaDeviceManufacturerName (..)

    -- ** CidrAuthorizationContext
    , CidrAuthorizationContext (..)
    , mkCidrAuthorizationContext
    , cacMessage
    , cacSignature

    -- ** ClientVpnRoute
    , ClientVpnRoute (..)
    , mkClientVpnRoute
    , cvrClientVpnEndpointId
    , cvrDescription
    , cvrDestinationCidr
    , cvrOrigin
    , cvrStatus
    , cvrTargetSubnet
    , cvrType

    -- ** LaunchTemplateInstanceMarketOptions
    , LaunchTemplateInstanceMarketOptions (..)
    , mkLaunchTemplateInstanceMarketOptions
    , ltimoMarketType
    , ltimoSpotOptions

    -- ** TrafficMirrorFilterRuleField
    , TrafficMirrorFilterRuleField (..)

    -- ** LoadPermissionModifications
    , LoadPermissionModifications (..)
    , mkLoadPermissionModifications
    , lpmAdd
    , lpmRemove

    -- ** LaunchTemplateEbsBlockDevice
    , LaunchTemplateEbsBlockDevice (..)
    , mkLaunchTemplateEbsBlockDevice
    , ltebdDeleteOnTermination
    , ltebdEncrypted
    , ltebdIops
    , ltebdKmsKeyId
    , ltebdSnapshotId
    , ltebdVolumeSize
    , ltebdVolumeType

    -- ** FpgaImageState
    , FpgaImageState (..)
    , mkFpgaImageState
    , fisCode
    , fisMessage

    -- ** EphemeralNvmeSupport
    , EphemeralNvmeSupport (..)

    -- ** MulticastSupportValue
    , MulticastSupportValue (..)

    -- ** ImportInstanceVolumeDetailItem
    , ImportInstanceVolumeDetailItem (..)
    , mkImportInstanceVolumeDetailItem
    , iivdiAvailabilityZone
    , iivdiBytesConverted
    , iivdiDescription
    , iivdiImage
    , iivdiStatus
    , iivdiStatusMessage
    , iivdiVolume

    -- ** LocalGatewayRouteTableVirtualInterfaceGroupAssociation
    , LocalGatewayRouteTableVirtualInterfaceGroupAssociation (..)
    , mkLocalGatewayRouteTableVirtualInterfaceGroupAssociation
    , lgrtvigaLocalGatewayId
    , lgrtvigaLocalGatewayRouteTableArn
    , lgrtvigaLocalGatewayRouteTableId
    , lgrtvigaLocalGatewayRouteTableVirtualInterfaceGroupAssociationId
    , lgrtvigaLocalGatewayVirtualInterfaceGroupId
    , lgrtvigaOwnerId
    , lgrtvigaState
    , lgrtvigaTags

    -- ** LocalGatewayId
    , LocalGatewayId (..)

    -- ** SummaryStatus
    , SummaryStatus (..)

    -- ** ReservedInstancesModification
    , ReservedInstancesModification (..)
    , mkReservedInstancesModification
    , rimClientToken
    , rimCreateDate
    , rimEffectiveDate
    , rimModificationResults
    , rimReservedInstancesIds
    , rimReservedInstancesModificationId
    , rimStatus
    , rimStatusMessage
    , rimUpdateDate

    -- ** TransitGatewayAttachmentPropagation
    , TransitGatewayAttachmentPropagation (..)
    , mkTransitGatewayAttachmentPropagation
    , tgapState
    , tgapTransitGatewayRouteTableId

    -- ** RuleAction
    , RuleAction (..)

    -- ** CarrierGatewayState
    , CarrierGatewayState (..)

    -- ** IamInstanceProfileAssociationId
    , IamInstanceProfileAssociationId (..)

    -- ** BatchState
    , BatchState (..)

    -- ** DiskInfo
    , DiskInfo (..)
    , mkDiskInfo
    , diCount
    , diSizeInGB
    , diType

    -- ** LocalGatewayRouteTableVpcAssociationId
    , LocalGatewayRouteTableVpcAssociationId (..)

    -- ** SubnetCidrAssociationId
    , SubnetCidrAssociationId (..)

    -- ** ConnectionNotificationType
    , ConnectionNotificationType (..)

    -- ** NetworkInterface
    , NetworkInterface (..)
    , mkNetworkInterface
    , niAssociation
    , niAttachment
    , niAvailabilityZone
    , niDescription
    , niGroups
    , niInterfaceType
    , niIpv6Addresses
    , niMacAddress
    , niNetworkInterfaceId
    , niOutpostArn
    , niOwnerId
    , niPrivateDnsName
    , niPrivateIpAddress
    , niPrivateIpAddresses
    , niRequesterId
    , niRequesterManaged
    , niSourceDestCheck
    , niStatus
    , niSubnetId
    , niTagSet
    , niVpcId

    -- ** EbsNvmeSupport
    , EbsNvmeSupport (..)

    -- ** ClientVpnConnectionStatus
    , ClientVpnConnectionStatus (..)
    , mkClientVpnConnectionStatus
    , cvcsCode
    , cvcsMessage

    -- ** InferenceDeviceManufacturerName
    , InferenceDeviceManufacturerName (..)

    -- ** ExportImageTaskId
    , ExportImageTaskId (..)

    -- ** EnableFastSnapshotRestoreSuccessItem
    , EnableFastSnapshotRestoreSuccessItem (..)
    , mkEnableFastSnapshotRestoreSuccessItem
    , efsrsiAvailabilityZone
    , efsrsiDisabledTime
    , efsrsiDisablingTime
    , efsrsiEnabledTime
    , efsrsiEnablingTime
    , efsrsiOptimizingTime
    , efsrsiOwnerAlias
    , efsrsiOwnerId
    , efsrsiSnapshotId
    , efsrsiState
    , efsrsiStateTransitionReason

    -- ** TransitAssociationGatewayId
    , TransitAssociationGatewayId (..)

    -- ** TelemetryStatus
    , TelemetryStatus (..)

    -- ** LaunchTemplateCpuOptions
    , LaunchTemplateCpuOptions (..)
    , mkLaunchTemplateCpuOptions
    , ltcoCoreCount
    , ltcoThreadsPerCore

    -- ** FleetSpotCapacityRebalance
    , FleetSpotCapacityRebalance (..)
    , mkFleetSpotCapacityRebalance
    , fscrReplacementStrategy

    -- ** RouteTableAssociationId
    , RouteTableAssociationId (..)

    -- ** Subnet
    , Subnet (..)
    , mkSubnet
    , sfAssignIpv6AddressOnCreation
    , sfAvailabilityZone
    , sfAvailabilityZoneId
    , sfAvailableIpAddressCount
    , sfCidrBlock
    , sfCustomerOwnedIpv4Pool
    , sfDefaultForAz
    , sfIpv6CidrBlockAssociationSet
    , sfMapCustomerOwnedIpOnLaunch
    , sfMapPublicIpOnLaunch
    , sfOutpostArn
    , sfOwnerId
    , sfState
    , sfSubnetArn
    , sfSubnetId
    , sfTags
    , sfVpcId

    -- ** LocalGatewayRouteState
    , LocalGatewayRouteState (..)

    -- ** TerminateConnectionStatus
    , TerminateConnectionStatus (..)
    , mkTerminateConnectionStatus
    , tcsConnectionId
    , tcsCurrentStatus
    , tcsPreviousStatus

    -- ** CreateFleetInstance
    , CreateFleetInstance (..)
    , mkCreateFleetInstance
    , cfiInstanceIds
    , cfiInstanceType
    , cfiLaunchTemplateAndOverrides
    , cfiLifecycle
    , cfiPlatform

    -- ** FpgaImageAttribute
    , FpgaImageAttribute (..)
    , mkFpgaImageAttribute
    , fiaDescription
    , fiaFpgaImageId
    , fiaLoadPermissions
    , fiaName
    , fiaProductCodes

    -- ** KeyPairInfo
    , KeyPairInfo (..)
    , mkKeyPairInfo
    , kpiKeyFingerprint
    , kpiKeyName
    , kpiKeyPairId
    , kpiTags

    -- ** LaunchTemplateEnclaveOptions
    , LaunchTemplateEnclaveOptions (..)
    , mkLaunchTemplateEnclaveOptions
    , lteoEnabled

    -- ** TransitGatewayMulticastDomain
    , TransitGatewayMulticastDomain (..)
    , mkTransitGatewayMulticastDomain
    , tgmdCreationTime
    , tgmdState
    , tgmdTags
    , tgmdTransitGatewayId
    , tgmdTransitGatewayMulticastDomainId

    -- ** NatGateway
    , NatGateway (..)
    , mkNatGateway
    , ngCreateTime
    , ngDeleteTime
    , ngFailureCode
    , ngFailureMessage
    , ngNatGatewayAddresses
    , ngNatGatewayId
    , ngProvisionedBandwidth
    , ngState
    , ngSubnetId
    , ngTags
    , ngVpcId

    -- ** LaunchPermissionModifications
    , LaunchPermissionModifications (..)
    , mkLaunchPermissionModifications
    , lAdd
    , lRemove

    -- ** TrafficMirrorPortRangeRequest
    , TrafficMirrorPortRangeRequest (..)
    , mkTrafficMirrorPortRangeRequest
    , tmprrFromPort
    , tmprrToPort

    -- ** TransitGatewayVpcAttachmentOptions
    , TransitGatewayVpcAttachmentOptions (..)
    , mkTransitGatewayVpcAttachmentOptions
    , tgvaoApplianceModeSupport
    , tgvaoDnsSupport
    , tgvaoIpv6Support

    -- ** ElasticGpuHealth
    , ElasticGpuHealth (..)
    , mkElasticGpuHealth
    , eghStatus

    -- ** LoadBalancersConfig
    , LoadBalancersConfig (..)
    , mkLoadBalancersConfig
    , lbcClassicLoadBalancersConfig
    , lbcTargetGroupsConfig

    -- ** ExcessCapacityTerminationPolicy
    , ExcessCapacityTerminationPolicy (..)

    -- ** SnapshotState
    , SnapshotState (..)

    -- ** InstanceNetworkInterfaceAssociation
    , InstanceNetworkInterfaceAssociation (..)
    , mkInstanceNetworkInterfaceAssociation
    , iniaCarrierIp
    , iniaIpOwnerId
    , iniaPublicDnsName
    , iniaPublicIp

    -- ** ReservationValue
    , ReservationValue (..)
    , mkReservationValue
    , rvHourlyPrice
    , rvRemainingTotalValue
    , rvRemainingUpfrontValue

    -- ** FleetReplacementStrategy
    , FleetReplacementStrategy (..)

    -- ** DiskImageDetail
    , DiskImageDetail (..)
    , mkDiskImageDetail
    , dBytes
    , dFormat
    , dImportManifestUrl

    -- ** ValidationError
    , ValidationError (..)
    , mkValidationError
    , veCode
    , veMessage

    -- ** TransitGatewayVpcAttachment
    , TransitGatewayVpcAttachment (..)
    , mkTransitGatewayVpcAttachment
    , tgvaCreationTime
    , tgvaOptions
    , tgvaState
    , tgvaSubnetIds
    , tgvaTags
    , tgvaTransitGatewayAttachmentId
    , tgvaTransitGatewayId
    , tgvaVpcId
    , tgvaVpcOwnerId

    -- ** SpotAllocationStrategy
    , SpotAllocationStrategy (..)

    -- ** UnsuccessfulInstanceCreditSpecificationItemError
    , UnsuccessfulInstanceCreditSpecificationItemError (..)
    , mkUnsuccessfulInstanceCreditSpecificationItemError
    , uicsieCode
    , uicsieMessage

    -- ** InstancePrivateIpAddress
    , InstancePrivateIpAddress (..)
    , mkInstancePrivateIpAddress
    , ipiaAssociation
    , ipiaPrimary
    , ipiaPrivateDnsName
    , ipiaPrivateIpAddress

    -- ** CancelledSpotInstanceRequest
    , CancelledSpotInstanceRequest (..)
    , mkCancelledSpotInstanceRequest
    , csirSpotInstanceRequestId
    , csirState

    -- ** VpnConnectionOptionsSpecification
    , VpnConnectionOptionsSpecification (..)
    , mkVpnConnectionOptionsSpecification
    , vcosEnableAcceleration
    , vcosLocalIpv4NetworkCidr
    , vcosLocalIpv6NetworkCidr
    , vcosRemoteIpv4NetworkCidr
    , vcosRemoteIpv6NetworkCidr
    , vcosStaticRoutesOnly
    , vcosTunnelInsideIpVersion
    , vcosTunnelOptions

    -- ** Address
    , Address (..)
    , mkAddress
    , aAllocationId
    , aAssociationId
    , aCarrierIp
    , aCustomerOwnedIp
    , aCustomerOwnedIpv4Pool
    , aDomain
    , aInstanceId
    , aNetworkBorderGroup
    , aNetworkInterfaceId
    , aNetworkInterfaceOwnerId
    , aPrivateIpAddress
    , aPublicIp
    , aPublicIpv4Pool
    , aTags

    -- ** NatGatewayId
    , NatGatewayId (..)

    -- ** FleetType
    , FleetType (..)

    -- ** TransitGatewayPrefixListReference
    , TransitGatewayPrefixListReference (..)
    , mkTransitGatewayPrefixListReference
    , tgplrBlackhole
    , tgplrPrefixListId
    , tgplrPrefixListOwnerId
    , tgplrState
    , tgplrTransitGatewayAttachment
    , tgplrTransitGatewayRouteTableId

    -- ** VolumeAttachmentState
    , VolumeAttachmentState (..)

    -- ** TransitGatewayMulticastDomainId
    , TransitGatewayMulticastDomainId (..)

    -- ** IamInstanceProfileAssociation
    , IamInstanceProfileAssociation (..)
    , mkIamInstanceProfileAssociation
    , iipaAssociationId
    , iipaIamInstanceProfile
    , iipaInstanceId
    , iipaState
    , iipaTimestamp

    -- ** EnclaveOptions
    , EnclaveOptions (..)
    , mkEnclaveOptions
    , eoEnabled

    -- ** PrivateDnsDetails
    , PrivateDnsDetails (..)
    , mkPrivateDnsDetails
    , pddPrivateDnsName

    -- ** CoipAddressUsage
    , CoipAddressUsage (..)
    , mkCoipAddressUsage
    , cauAllocationId
    , cauAwsAccountId
    , cauAwsService
    , cauCoIp

    -- ** AssociatedNetworkType
    , AssociatedNetworkType (..)

    -- ** NetworkInterfaceId
    , NetworkInterfaceId (..)

    -- ** MovingAddressStatus
    , MovingAddressStatus (..)
    , mkMovingAddressStatus
    , masMoveStatus
    , masPublicIp

    -- ** LaunchPermission
    , LaunchPermission (..)
    , mkLaunchPermission
    , lGroup
    , lUserId

    -- ** NetworkPerformance
    , NetworkPerformance (..)

    -- ** RouteState
    , RouteState (..)

    -- ** DefaultRouteTableAssociationValue
    , DefaultRouteTableAssociationValue (..)

    -- ** OfferingClassType
    , OfferingClassType (..)

    -- ** InterfacePermissionType
    , InterfacePermissionType (..)

    -- ** SecurityGroupName
    , SecurityGroupName (..)

    -- ** ExportImageTask
    , ExportImageTask (..)
    , mkExportImageTask
    , eitDescription
    , eitExportImageTaskId
    , eitImageId
    , eitProgress
    , eitS3ExportLocation
    , eitStatus
    , eitStatusMessage
    , eitTags

    -- ** RouteTableAssociation
    , RouteTableAssociation (..)
    , mkRouteTableAssociation
    , rtaAssociationState
    , rtaGatewayId
    , rtaMain
    , rtaRouteTableAssociationId
    , rtaRouteTableId
    , rtaSubnetId

    -- ** VpcEndpointType
    , VpcEndpointType (..)

    -- ** RamdiskId
    , RamdiskId (..)

    -- ** TransitGatewayRequestOptions
    , TransitGatewayRequestOptions (..)
    , mkTransitGatewayRequestOptions
    , tgroAmazonSideAsn
    , tgroAutoAcceptSharedAttachments
    , tgroDefaultRouteTableAssociation
    , tgroDefaultRouteTablePropagation
    , tgroDnsSupport
    , tgroMulticastSupport
    , tgroVpnEcmpSupport

    -- ** LocalGatewayRouteTableVpcAssociation
    , LocalGatewayRouteTableVpcAssociation (..)
    , mkLocalGatewayRouteTableVpcAssociation
    , lgrtvaLocalGatewayId
    , lgrtvaLocalGatewayRouteTableArn
    , lgrtvaLocalGatewayRouteTableId
    , lgrtvaLocalGatewayRouteTableVpcAssociationId
    , lgrtvaOwnerId
    , lgrtvaState
    , lgrtvaTags
    , lgrtvaVpcId

    -- ** FpgaDeviceInfo
    , FpgaDeviceInfo (..)
    , mkFpgaDeviceInfo
    , fdiCount
    , fdiManufacturer
    , fdiMemoryInfo
    , fdiName

    -- ** CpuOptions
    , CpuOptions (..)
    , mkCpuOptions
    , coCoreCount
    , coThreadsPerCore

    -- ** VpcEndpointServiceId
    , VpcEndpointServiceId (..)

    -- ** BundleTaskState
    , BundleTaskState (..)

    -- ** LaunchTemplateIamInstanceProfileSpecificationRequest
    , LaunchTemplateIamInstanceProfileSpecificationRequest (..)
    , mkLaunchTemplateIamInstanceProfileSpecificationRequest
    , ltiipsrArn
    , ltiipsrName

    -- ** FleetLaunchTemplateSpecification
    , FleetLaunchTemplateSpecification (..)
    , mkFleetLaunchTemplateSpecification
    , fltsLaunchTemplateId
    , fltsLaunchTemplateName
    , fltsVersion

    -- ** OnDemandOptions
    , OnDemandOptions (..)
    , mkOnDemandOptions
    , odoAllocationStrategy
    , odoCapacityReservationOptions
    , odoMaxTotalPrice
    , odoMinTargetCapacity
    , odoSingleAvailabilityZone
    , odoSingleInstanceType

    -- ** TransitGatewayRoute
    , TransitGatewayRoute (..)
    , mkTransitGatewayRoute
    , tgrDestinationCidrBlock
    , tgrPrefixListId
    , tgrState
    , tgrTransitGatewayAttachments
    , tgrType

    -- ** DeregisterInstanceTagAttributeRequest
    , DeregisterInstanceTagAttributeRequest (..)
    , mkDeregisterInstanceTagAttributeRequest
    , ditarIncludeAllTagsOfInstance
    , ditarInstanceTagKeys

    -- ** HibernationOptionsRequest
    , HibernationOptionsRequest (..)
    , mkHibernationOptionsRequest
    , horConfigured

    -- ** PortRange
    , PortRange (..)
    , mkPortRange
    , prFrom
    , prTo

    -- ** VpcAttributeName
    , VpcAttributeName (..)

    -- ** TransitGatewayRouteTablePropagation
    , TransitGatewayRouteTablePropagation (..)
    , mkTransitGatewayRouteTablePropagation
    , tgrtpResourceId
    , tgrtpResourceType
    , tgrtpState
    , tgrtpTransitGatewayAttachmentId

    -- ** IdFormat
    , IdFormat (..)
    , mkIdFormat
    , ifDeadline
    , ifResource
    , ifUseLongIds

    -- ** SubnetIpv6CidrBlockAssociation
    , SubnetIpv6CidrBlockAssociation (..)
    , mkSubnetIpv6CidrBlockAssociation
    , sicbaAssociationId
    , sicbaIpv6CidrBlock
    , sicbaIpv6CidrBlockState

    -- ** ReservedInstancesConfiguration
    , ReservedInstancesConfiguration (..)
    , mkReservedInstancesConfiguration
    , ricAvailabilityZone
    , ricInstanceCount
    , ricInstanceType
    , ricPlatform
    , ricScope

    -- ** EbsOptimizedSupport
    , EbsOptimizedSupport (..)

    -- ** IKEVersionsRequestListValue
    , IKEVersionsRequestListValue (..)
    , mkIKEVersionsRequestListValue
    , ikevrlvValue

    -- ** FpgaInfo
    , FpgaInfo (..)
    , mkFpgaInfo
    , fiFpgas
    , fiTotalFpgaMemoryInMiB

    -- ** PublicIpv4Pool
    , PublicIpv4Pool (..)
    , mkPublicIpv4Pool
    , pipDescription
    , pipNetworkBorderGroup
    , pipPoolAddressRanges
    , pipPoolId
    , pipTags
    , pipTotalAddressCount
    , pipTotalAvailableAddressCount

    -- ** DefaultTargetCapacityType
    , DefaultTargetCapacityType (..)

    -- ** DisableFastSnapshotRestoreSuccessItem
    , DisableFastSnapshotRestoreSuccessItem (..)
    , mkDisableFastSnapshotRestoreSuccessItem
    , dAvailabilityZone
    , dDisabledTime
    , dDisablingTime
    , dEnabledTime
    , dEnablingTime
    , dOptimizingTime
    , dOwnerAlias
    , dOwnerId
    , dSnapshotId
    , dState
    , dStateTransitionReason

    -- ** ResponseLaunchTemplateData
    , ResponseLaunchTemplateData (..)
    , mkResponseLaunchTemplateData
    , rBlockDeviceMappings
    , rCapacityReservationSpecification
    , rCpuOptions
    , rCreditSpecification
    , rDisableApiTermination
    , rEbsOptimized
    , rElasticGpuSpecifications
    , rElasticInferenceAccelerators
    , rEnclaveOptions
    , rHibernationOptions
    , rIamInstanceProfile
    , rImageId
    , rInstanceInitiatedShutdownBehavior
    , rInstanceMarketOptions
    , rInstanceType
    , rKernelId
    , rKeyName
    , rLicenseSpecifications
    , rMetadataOptions
    , rMonitoring
    , rNetworkInterfaces
    , rPlacement
    , rRamDiskId
    , rSecurityGroupIds
    , rSecurityGroups
    , rTagSpecifications
    , rUserData

    -- ** ElasticGpuAssociation
    , ElasticGpuAssociation (..)
    , mkElasticGpuAssociation
    , egaElasticGpuAssociationId
    , egaElasticGpuAssociationState
    , egaElasticGpuAssociationTime
    , egaElasticGpuId

    -- ** VpcFlowLogId
    , VpcFlowLogId (..)

    -- ** VolumeStatusDetails
    , VolumeStatusDetails (..)
    , mkVolumeStatusDetails
    , vsdName
    , vsdStatus

    -- ** SubnetId
    , SubnetId (..)

    -- ** SpotInstanceState
    , SpotInstanceState (..)

    -- ** VpnConnectionOptions
    , VpnConnectionOptions (..)
    , mkVpnConnectionOptions
    , vcoEnableAcceleration
    , vcoLocalIpv4NetworkCidr
    , vcoLocalIpv6NetworkCidr
    , vcoRemoteIpv4NetworkCidr
    , vcoRemoteIpv6NetworkCidr
    , vcoStaticRoutesOnly
    , vcoTunnelInsideIpVersion
    , vcoTunnelOptions

    -- ** SpotFleetLaunchSpecification
    , SpotFleetLaunchSpecification (..)
    , mkSpotFleetLaunchSpecification
    , sflsAddressingType
    , sflsBlockDeviceMappings
    , sflsEbsOptimized
    , sflsIamInstanceProfile
    , sflsImageId
    , sflsInstanceType
    , sflsKernelId
    , sflsKeyName
    , sflsMonitoring
    , sflsNetworkInterfaces
    , sflsPlacement
    , sflsRamdiskId
    , sflsSecurityGroups
    , sflsSpotPrice
    , sflsSubnetId
    , sflsTagSpecifications
    , sflsUserData
    , sflsWeightedCapacity

    -- ** ClientVpnConnectionStatusCode
    , ClientVpnConnectionStatusCode (..)

    -- ** PrefixListEntry
    , PrefixListEntry (..)
    , mkPrefixListEntry
    , pleCidr
    , pleDescription

    -- ** Phase2IntegrityAlgorithmsRequestListValue
    , Phase2IntegrityAlgorithmsRequestListValue (..)
    , mkPhase2IntegrityAlgorithmsRequestListValue
    , piarlvValue

    -- ** KernelId
    , KernelId (..)

    -- ** ClientVpnAuthenticationType
    , ClientVpnAuthenticationType (..)

    -- ** InstanceMatchCriteria
    , InstanceMatchCriteria (..)

    -- ** VolumeModificationState
    , VolumeModificationState (..)

    -- ** UserIdGroupPair
    , UserIdGroupPair (..)
    , mkUserIdGroupPair
    , uigpDescription
    , uigpGroupId
    , uigpGroupName
    , uigpPeeringStatus
    , uigpUserId
    , uigpVpcId
    , uigpVpcPeeringConnectionId

    -- ** InstanceStatusSummary
    , InstanceStatusSummary (..)
    , mkInstanceStatusSummary
    , issDetails
    , issStatus

    -- ** InstanceHealthStatus
    , InstanceHealthStatus (..)

    -- ** SpotPlacement
    , SpotPlacement (..)
    , mkSpotPlacement
    , spAvailabilityZone
    , spGroupName
    , spTenancy

    -- ** PoolCidrBlock
    , PoolCidrBlock (..)
    , mkPoolCidrBlock
    , pcbCidr

    -- ** EbsInstanceBlockDeviceSpecification
    , EbsInstanceBlockDeviceSpecification (..)
    , mkEbsInstanceBlockDeviceSpecification
    , eibdsDeleteOnTermination
    , eibdsVolumeId

    -- ** CapacityReservationId
    , CapacityReservationId (..)

    -- ** NetworkAclAssociation
    , NetworkAclAssociation (..)
    , mkNetworkAclAssociation
    , naaNetworkAclAssociationId
    , naaNetworkAclId
    , naaSubnetId

    -- ** DeleteFleetSuccessItem
    , DeleteFleetSuccessItem (..)
    , mkDeleteFleetSuccessItem
    , dfsiCurrentFleetState
    , dfsiFleetId
    , dfsiPreviousFleetState

    -- ** InstanceTypeOffering
    , InstanceTypeOffering (..)
    , mkInstanceTypeOffering
    , itoInstanceType
    , itoLocation
    , itoLocationType

    -- ** BundleTask
    , BundleTask (..)
    , mkBundleTask
    , btBundleId
    , btBundleTaskError
    , btInstanceId
    , btProgress
    , btStartTime
    , btState
    , btStorage
    , btUpdateTime

    -- ** ElasticInferenceAccelerator
    , ElasticInferenceAccelerator (..)
    , mkElasticInferenceAccelerator
    , eiaType
    , eiaCount

    -- ** InstanceStatusEvent
    , InstanceStatusEvent (..)
    , mkInstanceStatusEvent
    , iseCode
    , iseDescription
    , iseInstanceEventId
    , iseNotAfter
    , iseNotBefore
    , iseNotBeforeDeadline

    -- ** SubnetCidrBlockState
    , SubnetCidrBlockState (..)
    , mkSubnetCidrBlockState
    , scbsState
    , scbsStatusMessage

    -- ** InstanceType
    , InstanceType (..)

    -- ** TrafficMirrorFilterId
    , TrafficMirrorFilterId (..)

    -- ** HostInstance
    , HostInstance (..)
    , mkHostInstance
    , hiInstanceId
    , hiInstanceType
    , hiOwnerId

    -- ** CapacityReservationSpecification
    , CapacityReservationSpecification (..)
    , mkCapacityReservationSpecification
    , crsCapacityReservationPreference
    , crsCapacityReservationTarget

    -- ** CoipPool
    , CoipPool (..)
    , mkCoipPool
    , cpLocalGatewayRouteTableId
    , cpPoolArn
    , cpPoolCidrs
    , cpPoolId
    , cpTags

    -- ** Route
    , Route (..)
    , mkRoute
    , rCarrierGatewayId
    , rDestinationCidrBlock
    , rDestinationIpv6CidrBlock
    , rDestinationPrefixListId
    , rEgressOnlyInternetGatewayId
    , rGatewayId
    , rInstanceId
    , rInstanceOwnerId
    , rLocalGatewayId
    , rNatGatewayId
    , rNetworkInterfaceId
    , rOrigin
    , rState
    , rTransitGatewayId
    , rVpcPeeringConnectionId

    -- ** DnsNameState
    , DnsNameState (..)

    -- ** SpotDatafeedSubscription
    , SpotDatafeedSubscription (..)
    , mkSpotDatafeedSubscription
    , sdsBucket
    , sdsFault
    , sdsOwnerId
    , sdsPrefix
    , sdsState

    -- ** CreditSpecificationRequest
    , CreditSpecificationRequest (..)
    , mkCreditSpecificationRequest
    , csrCpuCredits

    -- ** TransitGatewayRouteTableAssociation
    , TransitGatewayRouteTableAssociation (..)
    , mkTransitGatewayRouteTableAssociation
    , tgrtaResourceId
    , tgrtaResourceType
    , tgrtaState
    , tgrtaTransitGatewayAttachmentId

    -- ** HostProperties
    , HostProperties (..)
    , mkHostProperties
    , hpCores
    , hpInstanceFamily
    , hpInstanceType
    , hpSockets
    , hpTotalVCpus

    -- ** VolumeStatusAttachmentStatus
    , VolumeStatusAttachmentStatus (..)
    , mkVolumeStatusAttachmentStatus
    , vsasInstanceId
    , vsasIoPerformance

    -- ** LaunchTemplateBlockDeviceMappingRequest
    , LaunchTemplateBlockDeviceMappingRequest (..)
    , mkLaunchTemplateBlockDeviceMappingRequest
    , ltbdmrDeviceName
    , ltbdmrEbs
    , ltbdmrNoDevice
    , ltbdmrVirtualName

    -- ** SpotFleetTagSpecification
    , SpotFleetTagSpecification (..)
    , mkSpotFleetTagSpecification
    , sftsResourceType
    , sftsTags

    -- ** BundleId
    , BundleId (..)

    -- ** VpcCidrBlockAssociation
    , VpcCidrBlockAssociation (..)
    , mkVpcCidrBlockAssociation
    , vcbaAssociationId
    , vcbaCidrBlock
    , vcbaCidrBlockState

    -- ** Storage
    , Storage (..)
    , mkStorage
    , sS3

    -- ** SecurityGroup
    , SecurityGroup (..)
    , mkSecurityGroup
    , sgDescription
    , sgGroupId
    , sgGroupName
    , sgIpPermissions
    , sgIpPermissionsEgress
    , sgOwnerId
    , sgTags
    , sgVpcId

    -- ** CancelSpotInstanceRequestState
    , CancelSpotInstanceRequestState (..)

    -- ** FlowLogResourceId
    , FlowLogResourceId (..)

    -- ** TransitGatewayAttachmentState
    , TransitGatewayAttachmentState (..)

    -- ** PlacementGroupState
    , PlacementGroupState (..)

    -- ** ReservedInstancesModificationResult
    , ReservedInstancesModificationResult (..)
    , mkReservedInstancesModificationResult
    , rimrReservedInstancesId
    , rimrTargetConfiguration

    -- ** InstanceBlockDeviceMappingSpecification
    , InstanceBlockDeviceMappingSpecification (..)
    , mkInstanceBlockDeviceMappingSpecification
    , ibdmsDeviceName
    , ibdmsEbs
    , ibdmsNoDevice
    , ibdmsVirtualName

    -- ** PublicIpv4PoolRange
    , PublicIpv4PoolRange (..)
    , mkPublicIpv4PoolRange
    , piprAddressCount
    , piprAvailableAddressCount
    , piprFirstAddress
    , piprLastAddress

    -- ** ExportEnvironment
    , ExportEnvironment (..)

    -- ** CustomerGatewayId
    , CustomerGatewayId (..)

    -- ** UserData
    , UserData (..)
    , mkUserData
    , udData

    -- ** ResetFpgaImageAttributeName
    , ResetFpgaImageAttributeName (..)

    -- ** FailedQueuedPurchaseDeletion
    , FailedQueuedPurchaseDeletion (..)
    , mkFailedQueuedPurchaseDeletion
    , fqpdError
    , fqpdReservedInstancesId

    -- ** TransitGatewayAssociationState
    , TransitGatewayAssociationState (..)

    -- ** VolumeAttachment
    , VolumeAttachment (..)
    , mkVolumeAttachment
    , vaAttachTime
    , vaDeleteOnTermination
    , vaDevice
    , vaInstanceId
    , vaState
    , vaVolumeId

    -- ** ScheduledInstancesIpv6Address
    , ScheduledInstancesIpv6Address (..)
    , mkScheduledInstancesIpv6Address
    , siiaIpv6Address

    -- ** FpgaImageAttributeName
    , FpgaImageAttributeName (..)

    -- ** GpuInfo
    , GpuInfo (..)
    , mkGpuInfo
    , giGpus
    , giTotalGpuMemoryInMiB

    -- ** SecurityGroupId
    , SecurityGroupId (..)

    -- ** NatGatewayState
    , NatGatewayState (..)

    -- ** CustomerGateway
    , CustomerGateway (..)
    , mkCustomerGateway
    , cBgpAsn
    , cCertificateArn
    , cCustomerGatewayId
    , cDeviceName
    , cIpAddress
    , cState
    , cTags
    , cType

    -- ** TransitGatewayMulticastDomainState
    , TransitGatewayMulticastDomainState (..)

    -- ** TransitGatewayPeeringAttachment
    , TransitGatewayPeeringAttachment (..)
    , mkTransitGatewayPeeringAttachment
    , tgpaAccepterTgwInfo
    , tgpaCreationTime
    , tgpaRequesterTgwInfo
    , tgpaState
    , tgpaStatus
    , tgpaTags
    , tgpaTransitGatewayAttachmentId

    -- ** InstanceUsage
    , InstanceUsage (..)
    , mkInstanceUsage
    , iuAccountId
    , iuUsedInstanceCount

    -- ** EbsInstanceBlockDevice
    , EbsInstanceBlockDevice (..)
    , mkEbsInstanceBlockDevice
    , eibdAttachTime
    , eibdDeleteOnTermination
    , eibdStatus
    , eibdVolumeId

    -- ** CertificateAuthenticationRequest
    , CertificateAuthenticationRequest (..)
    , mkCertificateAuthenticationRequest
    , carClientRootCertificateChainArn

    -- ** Ipv6CidrAssociation
    , Ipv6CidrAssociation (..)
    , mkIpv6CidrAssociation
    , icaAssociatedResource
    , icaIpv6Cidr

    -- ** SubnetCidrBlockStateCode
    , SubnetCidrBlockStateCode (..)

    -- ** ShutdownBehavior
    , ShutdownBehavior (..)

    -- ** DiskImageDescription
    , DiskImageDescription (..)
    , mkDiskImageDescription
    , didChecksum
    , didFormat
    , didImportManifestUrl
    , didSize

    -- ** ElasticGpus
    , ElasticGpus (..)
    , mkElasticGpus
    , egAvailabilityZone
    , egElasticGpuHealth
    , egElasticGpuId
    , egElasticGpuState
    , egElasticGpuType
    , egInstanceId
    , egTags

    -- ** NextToken
    , NextToken (..)

    -- ** DiskImageVolumeDescription
    , DiskImageVolumeDescription (..)
    , mkDiskImageVolumeDescription
    , divdId
    , divdSize

    -- ** Monitoring
    , Monitoring (..)
    , mkMonitoring
    , mState

    -- ** CapacityReservation
    , CapacityReservation (..)
    , mkCapacityReservation
    , crAvailabilityZone
    , crAvailabilityZoneId
    , crAvailableInstanceCount
    , crCapacityReservationArn
    , crCapacityReservationId
    , crCreateDate
    , crEbsOptimized
    , crEndDate
    , crEndDateType
    , crEphemeralStorage
    , crInstanceMatchCriteria
    , crInstancePlatform
    , crInstanceType
    , crOwnerId
    , crState
    , crTags
    , crTenancy
    , crTotalInstanceCount

    -- ** SubnetState
    , SubnetState (..)

    -- ** VersionDescription
    , VersionDescription (..)

    -- ** CancelSpotFleetRequestsSuccessItem
    , CancelSpotFleetRequestsSuccessItem (..)
    , mkCancelSpotFleetRequestsSuccessItem
    , csfrsiCurrentSpotFleetRequestState
    , csfrsiPreviousSpotFleetRequestState
    , csfrsiSpotFleetRequestId

    -- ** FederatedAuthentication
    , FederatedAuthentication (..)
    , mkFederatedAuthentication
    , faSamlProviderArn
    , faSelfServiceSamlProviderArn

    -- ** NetworkCardInfo
    , NetworkCardInfo (..)
    , mkNetworkCardInfo
    , nciMaximumNetworkInterfaces
    , nciNetworkCardIndex
    , nciNetworkPerformance

    -- ** ContainerFormat
    , ContainerFormat (..)

    -- ** TrafficMirrorFilter
    , TrafficMirrorFilter (..)
    , mkTrafficMirrorFilter
    , tmfDescription
    , tmfEgressFilterRules
    , tmfIngressFilterRules
    , tmfNetworkServices
    , tmfTags
    , tmfTrafficMirrorFilterId

    -- ** AvailabilityZoneMessage
    , AvailabilityZoneMessage (..)
    , mkAvailabilityZoneMessage
    , azmMessage

    -- ** TransitGatewayMulticastGroup
    , TransitGatewayMulticastGroup (..)
    , mkTransitGatewayMulticastGroup
    , tgmgGroupIpAddress
    , tgmgGroupMember
    , tgmgGroupSource
    , tgmgMemberType
    , tgmgNetworkInterfaceId
    , tgmgResourceId
    , tgmgResourceType
    , tgmgSourceType
    , tgmgSubnetId
    , tgmgTransitGatewayAttachmentId

    -- ** VpcAttachment
    , VpcAttachment (..)
    , mkVpcAttachment
    , vafState
    , vafVpcId

    -- ** EbsEncryptionSupport
    , EbsEncryptionSupport (..)

    -- ** ScheduledInstancesIamInstanceProfile
    , ScheduledInstancesIamInstanceProfile (..)
    , mkScheduledInstancesIamInstanceProfile
    , siiipArn
    , siiipName

    -- ** EventType
    , EventType (..)

    -- ** LaunchTemplatePlacement
    , LaunchTemplatePlacement (..)
    , mkLaunchTemplatePlacement
    , ltpAffinity
    , ltpAvailabilityZone
    , ltpGroupName
    , ltpHostId
    , ltpHostResourceGroupArn
    , ltpPartitionNumber
    , ltpSpreadDomain
    , ltpTenancy

    -- ** LocalGatewayRouteType
    , LocalGatewayRouteType (..)

    -- ** GpuDeviceInfo
    , GpuDeviceInfo (..)
    , mkGpuDeviceInfo
    , gdiCount
    , gdiManufacturer
    , gdiMemoryInfo
    , gdiName

    -- ** InstanceBlockDeviceMapping
    , InstanceBlockDeviceMapping (..)
    , mkInstanceBlockDeviceMapping
    , ibdmDeviceName
    , ibdmEbs

    -- ** FleetStateCode
    , FleetStateCode (..)

    -- ** SpotMarketOptions
    , SpotMarketOptions (..)
    , mkSpotMarketOptions
    , smoBlockDurationMinutes
    , smoInstanceInterruptionBehavior
    , smoMaxPrice
    , smoSpotInstanceType
    , smoValidUntil

    -- ** CoipPoolId
    , CoipPoolId (..)

    -- ** ServiceDetail
    , ServiceDetail (..)
    , mkServiceDetail
    , sdAcceptanceRequired
    , sdAvailabilityZones
    , sdBaseEndpointDnsNames
    , sdManagesVpcEndpoints
    , sdOwner
    , sdPrivateDnsName
    , sdPrivateDnsNameVerificationState
    , sdPrivateDnsNames
    , sdServiceId
    , sdServiceName
    , sdServiceType
    , sdTags
    , sdVpcEndpointPolicySupported

    -- ** Ipv6PoolEc2Id
    , Ipv6PoolEc2Id (..)

    -- ** InstanceSpecification
    , InstanceSpecification (..)
    , mkInstanceSpecification
    , isExcludeBootVolume
    , isInstanceId

    -- ** StatusType
    , StatusType (..)

    -- ** NetworkInterfaceIpv6Address
    , NetworkInterfaceIpv6Address (..)
    , mkNetworkInterfaceIpv6Address
    , niiaIpv6Address

    -- ** ExportToS3TaskSpecification
    , ExportToS3TaskSpecification (..)
    , mkExportToS3TaskSpecification
    , etstsContainerFormat
    , etstsDiskImageFormat
    , etstsS3Bucket
    , etstsS3Prefix

    -- ** InstanceMetadataOptionsResponse
    , InstanceMetadataOptionsResponse (..)
    , mkInstanceMetadataOptionsResponse
    , imorHttpEndpoint
    , imorHttpPutResponseHopLimit
    , imorHttpTokens
    , imorState

    -- ** NetworkInterfacePermissionState
    , NetworkInterfacePermissionState (..)
    , mkNetworkInterfacePermissionState
    , nipsState
    , nipsStatusMessage

    -- ** FleetSpotMaintenanceStrategiesRequest
    , FleetSpotMaintenanceStrategiesRequest (..)
    , mkFleetSpotMaintenanceStrategiesRequest
    , fsmsrCapacityRebalance

    -- ** LaunchTemplateIamInstanceProfileSpecification
    , LaunchTemplateIamInstanceProfileSpecification (..)
    , mkLaunchTemplateIamInstanceProfileSpecification
    , ltiipsArn
    , ltiipsName

    -- ** CancelBatchErrorCode
    , CancelBatchErrorCode (..)

    -- ** FlowLogsResourceType
    , FlowLogsResourceType (..)

    -- ** VpcCidrBlockState
    , VpcCidrBlockState (..)
    , mkVpcCidrBlockState
    , vcbsState
    , vcbsStatusMessage

    -- ** HttpTokensState
    , HttpTokensState (..)

    -- ** TrafficMirrorFilterRuleId
    , TrafficMirrorFilterRuleId (..)

    -- ** TagSpecification
    , TagSpecification (..)
    , mkTagSpecification
    , tsResourceType
    , tsTags

    -- ** AvailabilityZoneOptInStatus
    , AvailabilityZoneOptInStatus (..)

    -- ** InstanceIpv6AddressRequest
    , InstanceIpv6AddressRequest (..)
    , mkInstanceIpv6AddressRequest
    , iiarIpv6Address

    -- ** PrefixListId
    , PrefixListId (..)
    , mkPrefixListId
    , pliDescription
    , pliPrefixListId

    -- ** KmsKeyId
    , KmsKeyId (..)

    -- ** CopyTagsFromSource
    , CopyTagsFromSource (..)

    -- ** ResourceArn
    , ResourceArn (..)

    -- ** NetworkInterfaceAttribute
    , NetworkInterfaceAttribute (..)

    -- ** Phase2IntegrityAlgorithmsListValue
    , Phase2IntegrityAlgorithmsListValue (..)
    , mkPhase2IntegrityAlgorithmsListValue
    , pialvfValue

    -- ** LaunchTemplateVersion
    , LaunchTemplateVersion (..)
    , mkLaunchTemplateVersion
    , ltvCreateTime
    , ltvCreatedBy
    , ltvDefaultVersion
    , ltvLaunchTemplateData
    , ltvLaunchTemplateId
    , ltvLaunchTemplateName
    , ltvVersionDescription
    , ltvVersionNumber

    -- ** ClassicLoadBalancersConfig
    , ClassicLoadBalancersConfig (..)
    , mkClassicLoadBalancersConfig
    , clbcClassicLoadBalancers

    -- ** TransitGatewayRouteAttachment
    , TransitGatewayRouteAttachment (..)
    , mkTransitGatewayRouteAttachment
    , tgraResourceId
    , tgraResourceType
    , tgraTransitGatewayAttachmentId

    -- ** ModifyTransitGatewayOptions
    , ModifyTransitGatewayOptions (..)
    , mkModifyTransitGatewayOptions
    , mtgoAssociationDefaultRouteTableId
    , mtgoAutoAcceptSharedAttachments
    , mtgoDefaultRouteTableAssociation
    , mtgoDefaultRouteTablePropagation
    , mtgoDnsSupport
    , mtgoPropagationDefaultRouteTableId
    , mtgoVpnEcmpSupport

    -- ** ScheduledInstancesMonitoring
    , ScheduledInstancesMonitoring (..)
    , mkScheduledInstancesMonitoring
    , simEnabled

    -- ** ImageTypeValues
    , ImageTypeValues (..)

    -- ** InstanceExportDetails
    , InstanceExportDetails (..)
    , mkInstanceExportDetails
    , iedInstanceId
    , iedTargetEnvironment

    -- ** SnapshotAttributeName
    , SnapshotAttributeName (..)

    -- ** HibernationOptions
    , HibernationOptions (..)
    , mkHibernationOptions
    , hoConfigured

    -- ** FpgaImage
    , FpgaImage (..)
    , mkFpgaImage
    , fiCreateTime
    , fiDataRetentionSupport
    , fiDescription
    , fiFpgaImageGlobalId
    , fiFpgaImageId
    , fiName
    , fiOwnerAlias
    , fiOwnerId
    , fiPciId
    , fiProductCodes
    , fiPublic
    , fiShellVersion
    , fiState
    , fiTags
    , fiUpdateTime

    -- ** FpgaDeviceMemoryInfo
    , FpgaDeviceMemoryInfo (..)
    , mkFpgaDeviceMemoryInfo
    , fdmiSizeInMiB

    -- ** CapacityReservationPreference
    , CapacityReservationPreference (..)

    -- ** LocalGatewayRoutetableId
    , LocalGatewayRoutetableId (..)

    -- ** NetworkInterfaceAttachmentId
    , NetworkInterfaceAttachmentId (..)

    -- ** IKEVersionsListValue
    , IKEVersionsListValue (..)
    , mkIKEVersionsListValue
    , ikevlvValue

    -- ** DescribeFastSnapshotRestoreSuccessItem
    , DescribeFastSnapshotRestoreSuccessItem (..)
    , mkDescribeFastSnapshotRestoreSuccessItem
    , dfsrsiAvailabilityZone
    , dfsrsiDisabledTime
    , dfsrsiDisablingTime
    , dfsrsiEnabledTime
    , dfsrsiEnablingTime
    , dfsrsiOptimizingTime
    , dfsrsiOwnerAlias
    , dfsrsiOwnerId
    , dfsrsiSnapshotId
    , dfsrsiState
    , dfsrsiStateTransitionReason

    -- ** AvailabilityZone
    , AvailabilityZone (..)
    , mkAvailabilityZone
    , azGroupName
    , azMessages
    , azNetworkBorderGroup
    , azOptInStatus
    , azParentZoneId
    , azParentZoneName
    , azRegionName
    , azState
    , azZoneId
    , azZoneName
    , azZoneType

    -- ** TargetNetwork
    , TargetNetwork (..)
    , mkTargetNetwork
    , tnAssociationId
    , tnClientVpnEndpointId
    , tnSecurityGroups
    , tnStatus
    , tnTargetNetworkId
    , tnVpcId

    -- ** TransitGatewayAttachmentAssociation
    , TransitGatewayAttachmentAssociation (..)
    , mkTransitGatewayAttachmentAssociation
    , tgaaState
    , tgaaTransitGatewayRouteTableId

    -- ** DisableFastSnapshotRestoreStateError
    , DisableFastSnapshotRestoreStateError (..)
    , mkDisableFastSnapshotRestoreStateError
    , dfsrseCode
    , dfsrseMessage

    -- ** InstanceLifecycle
    , InstanceLifecycle (..)

    -- ** HistoryRecord
    , HistoryRecord (..)
    , mkHistoryRecord
    , hrEventInformation
    , hrEventType
    , hrTimestamp

    -- ** ClientVpnEndpointAttributeStatusCode
    , ClientVpnEndpointAttributeStatusCode (..)

    -- ** ImportImageTask
    , ImportImageTask (..)
    , mkImportImageTask
    , iitArchitecture
    , iitDescription
    , iitEncrypted
    , iitHypervisor
    , iitImageId
    , iitImportTaskId
    , iitKmsKeyId
    , iitLicenseSpecifications
    , iitLicenseType
    , iitPlatform
    , iitProgress
    , iitSnapshotDetails
    , iitStatus
    , iitStatusMessage
    , iitTags

    -- ** LaunchTemplateLicenseConfigurationRequest
    , LaunchTemplateLicenseConfigurationRequest (..)
    , mkLaunchTemplateLicenseConfigurationRequest
    , ltlcrLicenseConfigurationArn

    -- ** VpnState
    , VpnState (..)

    -- ** DeleteFleetError
    , DeleteFleetError (..)
    , mkDeleteFleetError
    , dfeCode
    , dfeMessage

    -- ** RouteTable
    , RouteTable (..)
    , mkRouteTable
    , rtAssociations
    , rtOwnerId
    , rtPropagatingVgws
    , rtRouteTableId
    , rtRoutes
    , rtTags
    , rtVpcId

    -- ** UserBucket
    , UserBucket (..)
    , mkUserBucket
    , ubS3Bucket
    , ubS3Key

    -- ** TrafficMirrorSessionId
    , TrafficMirrorSessionId (..)

    -- ** HypervisorType
    , HypervisorType (..)

    -- ** TargetGroupsConfig
    , TargetGroupsConfig (..)
    , mkTargetGroupsConfig
    , tgcTargetGroups

    -- ** AllocationState
    , AllocationState (..)

    -- ** VpcTenancy
    , VpcTenancy (..)

    -- ** CancelSpotFleetRequestsErrorItem
    , CancelSpotFleetRequestsErrorItem (..)
    , mkCancelSpotFleetRequestsErrorItem
    , csfreiError
    , csfreiSpotFleetRequestId

    -- ** InstanceStatusDetails
    , InstanceStatusDetails (..)
    , mkInstanceStatusDetails
    , isdImpairedSince
    , isdName
    , isdStatus

    -- ** ProvisionedBandwidth
    , ProvisionedBandwidth (..)
    , mkProvisionedBandwidth
    , pbProvisionTime
    , pbProvisioned
    , pbRequestTime
    , pbRequested
    , pbStatus

    -- ** FleetActivityStatus
    , FleetActivityStatus (..)

    -- ** ClientVpnEndpointId
    , ClientVpnEndpointId (..)

    -- ** IamInstanceProfile
    , IamInstanceProfile (..)
    , mkIamInstanceProfile
    , iipArn
    , iipId

    -- ** ClientConnectOptions
    , ClientConnectOptions (..)
    , mkClientConnectOptions
    , ccoEnabled
    , ccoLambdaFunctionArn

    -- ** LaunchTemplateConfig
    , LaunchTemplateConfig (..)
    , mkLaunchTemplateConfig
    , ltcLaunchTemplateSpecification
    , ltcOverrides

    -- ** TrafficMirrorNetworkService
    , TrafficMirrorNetworkService (..)

    -- ** TransitGatewayRouteTableState
    , TransitGatewayRouteTableState (..)

    -- ** LaunchTemplateCapacityReservationSpecificationResponse
    , LaunchTemplateCapacityReservationSpecificationResponse (..)
    , mkLaunchTemplateCapacityReservationSpecificationResponse
    , lCapacityReservationPreference
    , lCapacityReservationTarget

    -- ** UnsuccessfulItem
    , UnsuccessfulItem (..)
    , mkUnsuccessfulItem
    , uiError
    , uiResourceId

    -- ** ImportImageLicenseConfigurationResponse
    , ImportImageLicenseConfigurationResponse (..)
    , mkImportImageLicenseConfigurationResponse
    , iLicenseConfigurationArn

    -- ** ElasticGpuSpecification
    , ElasticGpuSpecification (..)
    , mkElasticGpuSpecification
    , egsType

    -- ** InternetGatewayAttachment
    , InternetGatewayAttachment (..)
    , mkInternetGatewayAttachment
    , igaState
    , igaVpcId

    -- ** Scope
    , Scope (..)

    -- ** ImageId
    , ImageId (..)

    -- ** LicenseConfigurationRequest
    , LicenseConfigurationRequest (..)
    , mkLicenseConfigurationRequest
    , lcrLicenseConfigurationArn

    -- ** PurchaseRequest
    , PurchaseRequest (..)
    , mkPurchaseRequest
    , prInstanceCount
    , prPurchaseToken

    -- ** EbsInfo
    , EbsInfo (..)
    , mkEbsInfo
    , eiEbsOptimizedInfo
    , eiEbsOptimizedSupport
    , eiEncryptionSupport
    , eiNvmeSupport

    -- ** ReservedInstanceState
    , ReservedInstanceState (..)

    -- ** InstanceAttributeName
    , InstanceAttributeName (..)

    -- ** IpPermission
    , IpPermission (..)
    , mkIpPermission
    , ipFromPort
    , ipIpProtocol
    , ipIpRanges
    , ipIpv6Ranges
    , ipPrefixListIds
    , ipToPort
    , ipUserIdGroupPairs

    -- ** TrafficMirrorPortRange
    , TrafficMirrorPortRange (..)
    , mkTrafficMirrorPortRange
    , tmprFromPort
    , tmprToPort

    -- ** DeleteQueuedReservedInstancesError
    , DeleteQueuedReservedInstancesError (..)
    , mkDeleteQueuedReservedInstancesError
    , dqrieCode
    , dqrieMessage

    -- ** ConversionTaskState
    , ConversionTaskState (..)

    -- ** DiskImage
    , DiskImage (..)
    , mkDiskImage
    , diDescription
    , diImage
    , diVolume

    -- ** ApplianceModeSupportValue
    , ApplianceModeSupportValue (..)

    -- ** NetworkInterfaceCreationType
    , NetworkInterfaceCreationType (..)

    -- ** Tenancy
    , Tenancy (..)

    -- ** ClientCertificateRevocationListStatus
    , ClientCertificateRevocationListStatus (..)
    , mkClientCertificateRevocationListStatus
    , ccrlsCode
    , ccrlsMessage

    -- ** SecurityGroupReference
    , SecurityGroupReference (..)
    , mkSecurityGroupReference
    , sgrGroupId
    , sgrReferencingVpcId
    , sgrVpcPeeringConnectionId

    -- ** SpotInstanceInterruptionBehavior
    , SpotInstanceInterruptionBehavior (..)

    -- ** UnsuccessfulItemError
    , UnsuccessfulItemError (..)
    , mkUnsuccessfulItemError
    , uieCode
    , uieMessage

    -- ** InferenceAcceleratorInfo
    , InferenceAcceleratorInfo (..)
    , mkInferenceAcceleratorInfo
    , iaiAccelerators

    -- ** DhcpOptionsId
    , DhcpOptionsId (..)

    -- ** ClassicLinkDnsSupport
    , ClassicLinkDnsSupport (..)
    , mkClassicLinkDnsSupport
    , cldsClassicLinkDnsSupported
    , cldsVpcId

    -- ** KeyPairName
    , KeyPairName (..)

    -- ** EgressOnlyInternetGateway
    , EgressOnlyInternetGateway (..)
    , mkEgressOnlyInternetGateway
    , eoigAttachments
    , eoigEgressOnlyInternetGatewayId
    , eoigTags

    -- ** EnableFastSnapshotRestoreStateError
    , EnableFastSnapshotRestoreStateError (..)
    , mkEnableFastSnapshotRestoreStateError
    , efsrseCode
    , efsrseMessage

    -- ** CreateFleetError
    , CreateFleetError (..)
    , mkCreateFleetError
    , cfeErrorCode
    , cfeErrorMessage
    , cfeLaunchTemplateAndOverrides
    , cfeLifecycle

    -- ** VpcPeeringConnectionStateReason
    , VpcPeeringConnectionStateReason (..)
    , mkVpcPeeringConnectionStateReason
    , vpcsrCode
    , vpcsrMessage

    -- ** LaunchTemplateHttpTokensState
    , LaunchTemplateHttpTokensState (..)

    -- ** IamInstanceProfileSpecification
    , IamInstanceProfileSpecification (..)
    , mkIamInstanceProfileSpecification
    , iipsArn
    , iipsName

    -- ** ImportVolumeTaskDetails
    , ImportVolumeTaskDetails (..)
    , mkImportVolumeTaskDetails
    , ivtdAvailabilityZone
    , ivtdBytesConverted
    , ivtdDescription
    , ivtdImage
    , ivtdVolume

    -- ** LastError
    , LastError (..)
    , mkLastError
    , leCode
    , leMessage

    -- ** PlacementStrategy
    , PlacementStrategy (..)

    -- ** DescribeFleetsInstances
    , DescribeFleetsInstances (..)
    , mkDescribeFleetsInstances
    , dfiInstanceIds
    , dfiInstanceType
    , dfiLaunchTemplateAndOverrides
    , dfiLifecycle
    , dfiPlatform

    -- ** DnsServersOptionsModifyStructure
    , DnsServersOptionsModifyStructure (..)
    , mkDnsServersOptionsModifyStructure
    , dsomsCustomDnsServers
    , dsomsEnabled

    -- ** InstanceNetworkInterface
    , InstanceNetworkInterface (..)
    , mkInstanceNetworkInterface
    , iniAssociation
    , iniAttachment
    , iniDescription
    , iniGroups
    , iniInterfaceType
    , iniIpv6Addresses
    , iniMacAddress
    , iniNetworkInterfaceId
    , iniOwnerId
    , iniPrivateDnsName
    , iniPrivateIpAddress
    , iniPrivateIpAddresses
    , iniSourceDestCheck
    , iniStatus
    , iniSubnetId
    , iniVpcId

    -- ** AssignedPrivateIpAddress
    , AssignedPrivateIpAddress (..)
    , mkAssignedPrivateIpAddress
    , apiaPrivateIpAddress

    -- ** TargetReservationValue
    , TargetReservationValue (..)
    , mkTargetReservationValue
    , trvReservationValue
    , trvTargetConfiguration

    -- ** SpotFleetRequestId
    , SpotFleetRequestId (..)

    -- ** MembershipType
    , MembershipType (..)

    -- ** VolumeStatusAction
    , VolumeStatusAction (..)
    , mkVolumeStatusAction
    , vsaCode
    , vsaDescription
    , vsaEventId
    , vsaEventType

    -- ** PlacementResponse
    , PlacementResponse (..)
    , mkPlacementResponse
    , prGroupName

    -- ** TransitGatewayId
    , TransitGatewayId (..)

    -- ** VpcPeeringConnectionVpcInfo
    , VpcPeeringConnectionVpcInfo (..)
    , mkVpcPeeringConnectionVpcInfo
    , vpcviCidrBlock
    , vpcviCidrBlockSet
    , vpcviIpv6CidrBlockSet
    , vpcviOwnerId
    , vpcviPeeringOptions
    , vpcviRegion
    , vpcviVpcId

    -- ** ServiceState
    , ServiceState (..)

    -- ** TransitGatewayMulticastRegisteredGroupSources
    , TransitGatewayMulticastRegisteredGroupSources (..)
    , mkTransitGatewayMulticastRegisteredGroupSources
    , tgmrgsGroupIpAddress
    , tgmrgsRegisteredNetworkInterfaceIds
    , tgmrgsTransitGatewayMulticastDomainId

    -- ** FleetEventType
    , FleetEventType (..)

    -- ** Phase1IntegrityAlgorithmsRequestListValue
    , Phase1IntegrityAlgorithmsRequestListValue (..)
    , mkPhase1IntegrityAlgorithmsRequestListValue
    , piarlvfValue

    -- ** ReservationId
    , ReservationId (..)

    -- ** ServiceType
    , ServiceType (..)

    -- ** ExportTaskS3LocationRequest
    , ExportTaskS3LocationRequest (..)
    , mkExportTaskS3LocationRequest
    , etslrS3Bucket
    , etslrS3Prefix

    -- ** UserBucketDetails
    , UserBucketDetails (..)
    , mkUserBucketDetails
    , ubdS3Bucket
    , ubdS3Key

    -- ** VolumeId
    , VolumeId (..)

    -- ** OfferingId
    , OfferingId (..)

    -- ** SelfServicePortal
    , SelfServicePortal (..)

    -- ** DisableFastSnapshotRestoreErrorItem
    , DisableFastSnapshotRestoreErrorItem (..)
    , mkDisableFastSnapshotRestoreErrorItem
    , dfsreiFastSnapshotRestoreStateErrors
    , dfsreiSnapshotId

    -- ** CidrBlock
    , CidrBlock (..)
    , mkCidrBlock
    , cbCidrBlock

    -- ** ReservedInstancesModificationId
    , ReservedInstancesModificationId (..)

    -- ** LaunchTemplate
    , LaunchTemplate (..)
    , mkLaunchTemplate
    , ltCreateTime
    , ltCreatedBy
    , ltDefaultVersionNumber
    , ltLatestVersionNumber
    , ltLaunchTemplateId
    , ltLaunchTemplateName
    , ltTags

    -- ** ReservedInstanceLimitPrice
    , ReservedInstanceLimitPrice (..)
    , mkReservedInstanceLimitPrice
    , rilpAmount
    , rilpCurrencyCode

    -- ** DnsEntry
    , DnsEntry (..)
    , mkDnsEntry
    , deDnsName
    , deHostedZoneId

    -- ** AssociationStatus
    , AssociationStatus (..)
    , mkAssociationStatus
    , asCode
    , asMessage

    -- ** DiskType
    , DiskType (..)

    -- ** Vpc
    , Vpc (..)
    , mkVpc
    , vfCidrBlock
    , vfCidrBlockAssociationSet
    , vfDhcpOptionsId
    , vfInstanceTenancy
    , vfIpv6CidrBlockAssociationSet
    , vfIsDefault
    , vfOwnerId
    , vfState
    , vfTags
    , vfVpcId

    -- ** AuthorizationRule
    , AuthorizationRule (..)
    , mkAuthorizationRule
    , arAccessAll
    , arClientVpnEndpointId
    , arDescription
    , arDestinationCidr
    , arGroupId
    , arStatus

    -- ** DirectoryServiceAuthenticationRequest
    , DirectoryServiceAuthenticationRequest (..)
    , mkDirectoryServiceAuthenticationRequest
    , dsarDirectoryId

    -- ** ImageDiskContainer
    , ImageDiskContainer (..)
    , mkImageDiskContainer
    , idcDescription
    , idcDeviceName
    , idcFormat
    , idcSnapshotId
    , idcUrl
    , idcUserBucket

    -- ** Ipv6Pool
    , Ipv6Pool (..)
    , mkIpv6Pool
    , ipDescription
    , ipPoolCidrBlocks
    , ipPoolId
    , ipTags

    -- ** OperationType
    , OperationType (..)

    -- ** LocalGatewayRouteTableVirtualInterfaceGroupAssociationId
    , LocalGatewayRouteTableVirtualInterfaceGroupAssociationId (..)

    -- ** CertificateAuthentication
    , CertificateAuthentication (..)
    , mkCertificateAuthentication
    , caClientRootCertificateChain

    -- ** InferenceDeviceInfo
    , InferenceDeviceInfo (..)
    , mkInferenceDeviceInfo
    , idiCount
    , idiManufacturer
    , idiName

    -- ** ImportTaskId
    , ImportTaskId (..)

    -- ** OnDemandAllocationStrategy
    , OnDemandAllocationStrategy (..)

    -- ** TrafficMirrorTarget
    , TrafficMirrorTarget (..)
    , mkTrafficMirrorTarget
    , tmtDescription
    , tmtNetworkInterfaceId
    , tmtNetworkLoadBalancerArn
    , tmtOwnerId
    , tmtTags
    , tmtTrafficMirrorTargetId
    , tmtType

    -- ** Phase1EncryptionAlgorithmsRequestListValue
    , Phase1EncryptionAlgorithmsRequestListValue (..)
    , mkPhase1EncryptionAlgorithmsRequestListValue
    , pearlvValue

    -- ** InstanceStatus
    , InstanceStatus (..)
    , mkInstanceStatus
    , issAvailabilityZone
    , issEvents
    , issInstanceId
    , issInstanceState
    , issInstanceStatus
    , issOutpostArn
    , issSystemStatus

    -- ** TransitGatewayMulitcastDomainAssociationState
    , TransitGatewayMulitcastDomainAssociationState (..)

    -- ** CapacityReservationOptionsRequest
    , CapacityReservationOptionsRequest (..)
    , mkCapacityReservationOptionsRequest
    , crorUsageStrategy

    -- ** GpuDeviceMemoryInfo
    , GpuDeviceMemoryInfo (..)
    , mkGpuDeviceMemoryInfo
    , gdmiSizeInMiB

    -- ** ArchitectureValues
    , ArchitectureValues (..)

    -- ** ReportInstanceReasonCodes
    , ReportInstanceReasonCodes (..)

    -- ** LocationType
    , LocationType (..)

    -- ** PeeringConnectionOptionsRequest
    , PeeringConnectionOptionsRequest (..)
    , mkPeeringConnectionOptionsRequest
    , pcorAllowDnsResolutionFromRemoteVpc
    , pcorAllowEgressFromLocalClassicLinkToRemoteVpc
    , pcorAllowEgressFromLocalVpcToRemoteClassicLink

    -- ** MoveStatus
    , MoveStatus (..)

    -- ** EbsBlockDevice
    , EbsBlockDevice (..)
    , mkEbsBlockDevice
    , ebdDeleteOnTermination
    , ebdEncrypted
    , ebdIops
    , ebdKmsKeyId
    , ebdSnapshotId
    , ebdVolumeSize
    , ebdVolumeType

    -- ** VpnTunnelOptionsSpecification
    , VpnTunnelOptionsSpecification (..)
    , mkVpnTunnelOptionsSpecification
    , vtosDPDTimeoutAction
    , vtosDPDTimeoutSeconds
    , vtosIKEVersions
    , vtosPhase1DHGroupNumbers
    , vtosPhase1EncryptionAlgorithms
    , vtosPhase1IntegrityAlgorithms
    , vtosPhase1LifetimeSeconds
    , vtosPhase2DHGroupNumbers
    , vtosPhase2EncryptionAlgorithms
    , vtosPhase2IntegrityAlgorithms
    , vtosPhase2LifetimeSeconds
    , vtosPreSharedKey
    , vtosRekeyFuzzPercentage
    , vtosRekeyMarginTimeSeconds
    , vtosReplayWindowSize
    , vtosStartupAction
    , vtosTunnelInsideCidr
    , vtosTunnelInsideIpv6Cidr

    -- ** FleetLaunchTemplateOverridesRequest
    , FleetLaunchTemplateOverridesRequest (..)
    , mkFleetLaunchTemplateOverridesRequest
    , fltorAvailabilityZone
    , fltorInstanceType
    , fltorMaxPrice
    , fltorPlacement
    , fltorPriority
    , fltorSubnetId
    , fltorWeightedCapacity

    -- ** LocalGateway
    , LocalGateway (..)
    , mkLocalGateway
    , lgLocalGatewayId
    , lgOutpostArn
    , lgOwnerId
    , lgState
    , lgTags

    -- ** AccountAttribute
    , AccountAttribute (..)
    , mkAccountAttribute
    , aaAttributeName
    , aaAttributeValues

    -- ** FpgaImageStateCode
    , FpgaImageStateCode (..)

    -- ** Ipv6SupportValue
    , Ipv6SupportValue (..)

    -- ** Phase1DHGroupNumbersRequestListValue
    , Phase1DHGroupNumbersRequestListValue (..)
    , mkPhase1DHGroupNumbersRequestListValue
    , pdhgnrlvValue

    -- ** SnapshotDetail
    , SnapshotDetail (..)
    , mkSnapshotDetail
    , sdDescription
    , sdDeviceName
    , sdDiskImageSize
    , sdFormat
    , sdProgress
    , sdSnapshotId
    , sdStatus
    , sdStatusMessage
    , sdUrl
    , sdUserBucket

    -- ** SpotInstanceRequestId
    , SpotInstanceRequestId (..)

    -- ** ScheduledInstanceRecurrenceRequest
    , ScheduledInstanceRecurrenceRequest (..)
    , mkScheduledInstanceRecurrenceRequest
    , sirrFrequency
    , sirrInterval
    , sirrOccurrenceDays
    , sirrOccurrenceRelativeToEnd
    , sirrOccurrenceUnit

    -- ** VpcEndpointConnection
    , VpcEndpointConnection (..)
    , mkVpcEndpointConnection
    , vecCreationTimestamp
    , vecDnsEntries
    , vecGatewayLoadBalancerArns
    , vecNetworkLoadBalancerArns
    , vecServiceId
    , vecVpcEndpointId
    , vecVpcEndpointOwner
    , vecVpcEndpointState

    -- ** PriceSchedule
    , PriceSchedule (..)
    , mkPriceSchedule
    , psActive
    , psCurrencyCode
    , psPrice
    , psTerm

    -- ** DeviceType
    , DeviceType (..)

    -- ** DomainType
    , DomainType (..)

    -- ** RegionInfo
    , RegionInfo (..)
    , mkRegionInfo
    , riEndpoint
    , riOptInStatus
    , riRegionName

    -- ** TransitGatewayState
    , TransitGatewayState (..)

    -- ** CreditSpecification
    , CreditSpecification (..)
    , mkCreditSpecification
    , csCpuCredits

    -- ** DeleteLaunchTemplateVersionsResponseSuccessItem
    , DeleteLaunchTemplateVersionsResponseSuccessItem (..)
    , mkDeleteLaunchTemplateVersionsResponseSuccessItem
    , dltvrsiLaunchTemplateId
    , dltvrsiLaunchTemplateName
    , dltvrsiVersionNumber

    -- ** TransitGatewayRouteTable
    , TransitGatewayRouteTable (..)
    , mkTransitGatewayRouteTable
    , tgrtCreationTime
    , tgrtDefaultAssociationRouteTable
    , tgrtDefaultPropagationRouteTable
    , tgrtState
    , tgrtTags
    , tgrtTransitGatewayId
    , tgrtTransitGatewayRouteTableId

    -- ** ImportImageLicenseConfigurationRequest
    , ImportImageLicenseConfigurationRequest (..)
    , mkImportImageLicenseConfigurationRequest
    , iilcrLicenseConfigurationArn

    -- ** NetworkInfo
    , NetworkInfo (..)
    , mkNetworkInfo
    , niDefaultNetworkCardIndex
    , niEfaSupported
    , niEnaSupport
    , niIpv4AddressesPerInterface
    , niIpv6AddressesPerInterface
    , niIpv6Supported
    , niMaximumNetworkCards
    , niMaximumNetworkInterfaces
    , niNetworkCards
    , niNetworkPerformance

    -- ** LaunchTemplateSpecification
    , LaunchTemplateSpecification (..)
    , mkLaunchTemplateSpecification
    , ltsLaunchTemplateId
    , ltsLaunchTemplateName
    , ltsVersion

    -- ** LaunchTemplateBlockDeviceMapping
    , LaunchTemplateBlockDeviceMapping (..)
    , mkLaunchTemplateBlockDeviceMapping
    , ltbdmDeviceName
    , ltbdmEbs
    , ltbdmNoDevice
    , ltbdmVirtualName

    -- ** InstanceEventId
    , InstanceEventId (..)

    -- ** InstanceCreditSpecificationRequest
    , InstanceCreditSpecificationRequest (..)
    , mkInstanceCreditSpecificationRequest
    , icsrCpuCredits
    , icsrInstanceId

    -- ** Host
    , Host (..)
    , mkHost
    , hAllocationTime
    , hAllowsMultipleInstanceTypes
    , hAutoPlacement
    , hAvailabilityZone
    , hAvailabilityZoneId
    , hAvailableCapacity
    , hClientToken
    , hHostId
    , hHostProperties
    , hHostRecovery
    , hHostReservationId
    , hInstances
    , hMemberOfServiceLinkedResourceGroup
    , hOwnerId
    , hReleaseTime
    , hState
    , hTags

    -- ** PropagatingVgw
    , PropagatingVgw (..)
    , mkPropagatingVgw
    , pvGatewayId

    -- ** ProcessorInfo
    , ProcessorInfo (..)
    , mkProcessorInfo
    , piSupportedArchitectures
    , piSustainedClockSpeedInGhz

    -- ** ClientVpnAuthenticationRequest
    , ClientVpnAuthenticationRequest (..)
    , mkClientVpnAuthenticationRequest
    , cvarActiveDirectory
    , cvarFederatedAuthentication
    , cvarMutualAuthentication
    , cvarType

    -- ** OfferingTypeValues
    , OfferingTypeValues (..)

    -- ** TargetConfigurationRequest
    , TargetConfigurationRequest (..)
    , mkTargetConfigurationRequest
    , tcrOfferingId
    , tcrInstanceCount

    -- ** ScheduledInstance
    , ScheduledInstance (..)
    , mkScheduledInstance
    , siAvailabilityZone
    , siCreateDate
    , siHourlyPrice
    , siInstanceCount
    , siInstanceType
    , siNetworkPlatform
    , siNextSlotStartTime
    , siPlatform
    , siPreviousSlotEndTime
    , siRecurrence
    , siScheduledInstanceId
    , siSlotDurationInHours
    , siTermEndDate
    , siTermStartDate
    , siTotalScheduledInstanceHours

    -- ** HostReservation
    , HostReservation (..)
    , mkHostReservation
    , hrCount
    , hrCurrencyCode
    , hrDuration
    , hrEnd
    , hrHostIdSet
    , hrHostReservationId
    , hrHourlyPrice
    , hrInstanceFamily
    , hrOfferingId
    , hrPaymentOption
    , hrStart
    , hrState
    , hrTags
    , hrUpfrontPrice

    -- ** NetworkInterfacePermission
    , NetworkInterfacePermission (..)
    , mkNetworkInterfacePermission
    , nipAwsAccountId
    , nipAwsService
    , nipNetworkInterfaceId
    , nipNetworkInterfacePermissionId
    , nipPermission
    , nipPermissionState

    -- ** FleetLaunchTemplateConfigRequest
    , FleetLaunchTemplateConfigRequest (..)
    , mkFleetLaunchTemplateConfigRequest
    , fltcrLaunchTemplateSpecification
    , fltcrOverrides

    -- ** VpnGateway
    , VpnGateway (..)
    , mkVpnGateway
    , vgAmazonSideAsn
    , vgAvailabilityZone
    , vgState
    , vgTags
    , vgType
    , vgVpcAttachments
    , vgVpnGatewayId

    -- ** EventInformation
    , EventInformation (..)
    , mkEventInformation
    , eiEventDescription
    , eiEventSubType
    , eiInstanceId

    -- ** Filter
    , Filter (..)
    , mkFilter
    , fName
    , fValues

    -- ** LaunchTemplateErrorCode
    , LaunchTemplateErrorCode (..)

    -- ** LaunchTemplateSpotMarketOptionsRequest
    , LaunchTemplateSpotMarketOptionsRequest (..)
    , mkLaunchTemplateSpotMarketOptionsRequest
    , ltsmorBlockDurationMinutes
    , ltsmorInstanceInterruptionBehavior
    , ltsmorMaxPrice
    , ltsmorSpotInstanceType
    , ltsmorValidUntil

    -- ** VolumeType
    , VolumeType (..)

    -- ** ConversionTaskId
    , ConversionTaskId (..)

    -- ** InstanceStateChange
    , InstanceStateChange (..)
    , mkInstanceStateChange
    , iscCurrentState
    , iscInstanceId
    , iscPreviousState

    -- ** NetworkAcl
    , NetworkAcl (..)
    , mkNetworkAcl
    , naAssociations
    , naEntries
    , naIsDefault
    , naNetworkAclId
    , naOwnerId
    , naTags
    , naVpcId

    -- ** LogDestinationType
    , LogDestinationType (..)

    -- ** ImageState
    , ImageState (..)

    -- ** HostOffering
    , HostOffering (..)
    , mkHostOffering
    , hoCurrencyCode
    , hoDuration
    , hoHourlyPrice
    , hoInstanceFamily
    , hoOfferingId
    , hoPaymentOption
    , hoUpfrontPrice

    -- ** SuccessfulInstanceCreditSpecificationItem
    , SuccessfulInstanceCreditSpecificationItem (..)
    , mkSuccessfulInstanceCreditSpecificationItem
    , sicsiInstanceId

    -- ** TaggableResourceId
    , TaggableResourceId (..)

    -- ** ClientVpnAuthorizationRuleStatus
    , ClientVpnAuthorizationRuleStatus (..)
    , mkClientVpnAuthorizationRuleStatus
    , cvarsCode
    , cvarsMessage

    -- ** GatewayType
    , GatewayType (..)

    -- ** KeyPairId
    , KeyPairId (..)

    -- ** LaunchTemplateInstanceMetadataOptionsRequest
    , LaunchTemplateInstanceMetadataOptionsRequest (..)
    , mkLaunchTemplateInstanceMetadataOptionsRequest
    , ltimorHttpEndpoint
    , ltimorHttpPutResponseHopLimit
    , ltimorHttpTokens

    -- ** UnsuccessfulInstanceCreditSpecificationErrorCode
    , UnsuccessfulInstanceCreditSpecificationErrorCode (..)

    -- ** InstanceNetworkInterfaceAttachment
    , InstanceNetworkInterfaceAttachment (..)
    , mkInstanceNetworkInterfaceAttachment
    , iniaAttachTime
    , iniaAttachmentId
    , iniaDeleteOnTermination
    , iniaDeviceIndex
    , iniaNetworkCardIndex
    , iniaStatus

    -- ** UnsuccessfulInstanceCreditSpecificationItem
    , UnsuccessfulInstanceCreditSpecificationItem (..)
    , mkUnsuccessfulInstanceCreditSpecificationItem
    , uicsiError
    , uicsiInstanceId

    -- ** EndDateType
    , EndDateType (..)

    -- ** VpnConnectionId
    , VpnConnectionId (..)

    -- ** AttributeBooleanValue
    , AttributeBooleanValue (..)
    , mkAttributeBooleanValue
    , abvValue

    -- ** NetworkInterfaceType
    , NetworkInterfaceType (..)

    -- ** TunnelOption
    , TunnelOption (..)
    , mkTunnelOption
    , toDpdTimeoutAction
    , toDpdTimeoutSeconds
    , toIkeVersions
    , toOutsideIpAddress
    , toPhase1DHGroupNumbers
    , toPhase1EncryptionAlgorithms
    , toPhase1IntegrityAlgorithms
    , toPhase1LifetimeSeconds
    , toPhase2DHGroupNumbers
    , toPhase2EncryptionAlgorithms
    , toPhase2IntegrityAlgorithms
    , toPhase2LifetimeSeconds
    , toPreSharedKey
    , toRekeyFuzzPercentage
    , toRekeyMarginTimeSeconds
    , toReplayWindowSize
    , toStartupAction
    , toTunnelInsideCidr
    , toTunnelInsideIpv6Cidr

    -- ** DeleteLaunchTemplateVersionsResponseErrorItem
    , DeleteLaunchTemplateVersionsResponseErrorItem (..)
    , mkDeleteLaunchTemplateVersionsResponseErrorItem
    , dltvreiLaunchTemplateId
    , dltvreiLaunchTemplateName
    , dltvreiResponseError
    , dltvreiVersionNumber

    -- ** RecurringCharge
    , RecurringCharge (..)
    , mkRecurringCharge
    , rcAmount
    , rcFrequency

    -- ** ScheduledInstancesLaunchSpecification
    , ScheduledInstancesLaunchSpecification (..)
    , mkScheduledInstancesLaunchSpecification
    , silsImageId
    , silsBlockDeviceMappings
    , silsEbsOptimized
    , silsIamInstanceProfile
    , silsInstanceType
    , silsKernelId
    , silsKeyName
    , silsMonitoring
    , silsNetworkInterfaces
    , silsPlacement
    , silsRamdiskId
    , silsSecurityGroupIds
    , silsSubnetId
    , silsUserData

    -- ** ConnectionNotification
    , ConnectionNotification (..)
    , mkConnectionNotification
    , cnConnectionEvents
    , cnConnectionNotificationArn
    , cnConnectionNotificationId
    , cnConnectionNotificationState
    , cnConnectionNotificationType
    , cnServiceId
    , cnVpcEndpointId

    -- ** LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
    , LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (..)
    , mkLaunchTemplateInstanceNetworkInterfaceSpecificationRequest
    , ltinisrAssociateCarrierIpAddress
    , ltinisrAssociatePublicIpAddress
    , ltinisrDeleteOnTermination
    , ltinisrDescription
    , ltinisrDeviceIndex
    , ltinisrGroups
    , ltinisrInterfaceType
    , ltinisrIpv6AddressCount
    , ltinisrIpv6Addresses
    , ltinisrNetworkCardIndex
    , ltinisrNetworkInterfaceId
    , ltinisrPrivateIpAddress
    , ltinisrPrivateIpAddresses
    , ltinisrSecondaryPrivateIpAddressCount
    , ltinisrSubnetId

    -- ** UsageClassType
    , UsageClassType (..)

    -- ** LocalGatewayVirtualInterfaceGroupId
    , LocalGatewayVirtualInterfaceGroupId (..)

    -- ** LaunchTemplatesMonitoringRequest
    , LaunchTemplatesMonitoringRequest (..)
    , mkLaunchTemplatesMonitoringRequest
    , ltmrEnabled

    -- ** NewDhcpConfiguration
    , NewDhcpConfiguration (..)
    , mkNewDhcpConfiguration
    , ndcKey
    , ndcValues

    -- ** InstanceMetadataOptionsState
    , InstanceMetadataOptionsState (..)

    -- ** StateReason
    , StateReason (..)
    , mkStateReason
    , srCode
    , srMessage

    -- ** CapacityReservationState
    , CapacityReservationState (..)

    -- ** MonitoringState
    , MonitoringState (..)

    -- ** ElasticGpuStatus
    , ElasticGpuStatus (..)

    -- ** LicenseConfiguration
    , LicenseConfiguration (..)
    , mkLicenseConfiguration
    , lcLicenseConfigurationArn

    -- ** HostRecovery
    , HostRecovery (..)

    -- ** FleetOnDemandAllocationStrategy
    , FleetOnDemandAllocationStrategy (..)

    -- ** SlotDateTimeRangeRequest
    , SlotDateTimeRangeRequest (..)
    , mkSlotDateTimeRangeRequest
    , sdtrrEarliestTime
    , sdtrrLatestTime

    -- ** Purchase
    , Purchase (..)
    , mkPurchase
    , pCurrencyCode
    , pDuration
    , pHostIdSet
    , pHostReservationId
    , pHourlyPrice
    , pInstanceFamily
    , pPaymentOption
    , pUpfrontPrice

    -- ** ScheduledInstancesPrivateIpAddressConfig
    , ScheduledInstancesPrivateIpAddressConfig (..)
    , mkScheduledInstancesPrivateIpAddressConfig
    , sipiacPrimary
    , sipiacPrivateIpAddress

    -- ** LaunchTemplateHibernationOptionsRequest
    , LaunchTemplateHibernationOptionsRequest (..)
    , mkLaunchTemplateHibernationOptionsRequest
    , lthorConfigured

    -- ** TransitGatewayAttachmentId
    , TransitGatewayAttachmentId (..)

    -- ** PrefixListAssociation
    , PrefixListAssociation (..)
    , mkPrefixListAssociation
    , plaResourceId
    , plaResourceOwner

    -- ** FleetId
    , FleetId (..)

    -- ** ReservedInstancesId
    , ReservedInstancesId (..)
    , mkReservedInstancesId
    , riiReservedInstancesId

    -- ** StatusName
    , StatusName (..)

    -- ** InternetGateway
    , InternetGateway (..)
    , mkInternetGateway
    , igAttachments
    , igInternetGatewayId
    , igOwnerId
    , igTags

    -- ** PlacementGroupId
    , PlacementGroupId (..)

    -- ** HistoryRecordEntry
    , HistoryRecordEntry (..)
    , mkHistoryRecordEntry
    , hreEventInformation
    , hreEventType
    , hreTimestamp

    -- ** ResponseError
    , ResponseError (..)
    , mkResponseError
    , reCode
    , reMessage

    -- ** VolumeStatusName
    , VolumeStatusName (..)

    -- ** ReservedInstancesOfferingId
    , ReservedInstancesOfferingId (..)

    -- ** AllocationStrategy
    , AllocationStrategy (..)

    -- ** DescribeFleetError
    , DescribeFleetError (..)
    , mkDescribeFleetError
    , dfeErrorCode
    , dfeErrorMessage
    , dfeLaunchTemplateAndOverrides
    , dfeLifecycle

    -- ** CapacityReservationGroup
    , CapacityReservationGroup (..)
    , mkCapacityReservationGroup
    , crgGroupArn
    , crgOwnerId

    -- ** VolumeAttributeName
    , VolumeAttributeName (..)

    -- ** VpcEndpointId
    , VpcEndpointId (..)

    -- ** TransitGatewayMulticastDeregisteredGroupSources
    , TransitGatewayMulticastDeregisteredGroupSources (..)
    , mkTransitGatewayMulticastDeregisteredGroupSources
    , tgmdgsDeregisteredNetworkInterfaceIds
    , tgmdgsGroupIpAddress
    , tgmdgsTransitGatewayMulticastDomainId

    -- ** LaunchTemplateTagSpecificationRequest
    , LaunchTemplateTagSpecificationRequest (..)
    , mkLaunchTemplateTagSpecificationRequest
    , lttsrResourceType
    , lttsrTags

    -- ** PrivateDnsNameConfiguration
    , PrivateDnsNameConfiguration (..)
    , mkPrivateDnsNameConfiguration
    , pdncName
    , pdncState
    , pdncType
    , pdncValue

    -- ** ImportInstanceTaskDetails
    , ImportInstanceTaskDetails (..)
    , mkImportInstanceTaskDetails
    , iitdDescription
    , iitdInstanceId
    , iitdPlatform
    , iitdVolumes

    -- ** ClientVpnEndpointStatusCode
    , ClientVpnEndpointStatusCode (..)

    -- ** PlacementGroup
    , PlacementGroup (..)
    , mkPlacementGroup
    , pgGroupId
    , pgGroupName
    , pgPartitionCount
    , pgState
    , pgStrategy
    , pgTags

    -- ** UnlimitedSupportedInstanceFamily
    , UnlimitedSupportedInstanceFamily (..)

    -- ** AutoPlacement
    , AutoPlacement (..)

    -- ** RootDeviceType
    , RootDeviceType (..)

    -- ** ByoipCidrState
    , ByoipCidrState (..)

    -- ** VolumeModification
    , VolumeModification (..)
    , mkVolumeModification
    , vmEndTime
    , vmModificationState
    , vmOriginalIops
    , vmOriginalSize
    , vmOriginalVolumeType
    , vmProgress
    , vmStartTime
    , vmStatusMessage
    , vmTargetIops
    , vmTargetSize
    , vmTargetVolumeType
    , vmVolumeId

    -- ** TransitGatewayRouteType
    , TransitGatewayRouteType (..)

    -- ** FleetSpotMaintenanceStrategies
    , FleetSpotMaintenanceStrategies (..)
    , mkFleetSpotMaintenanceStrategies
    , fsmsCapacityRebalance

    -- ** ProductCode
    , ProductCode (..)
    , mkProductCode
    , pcProductCodeId
    , pcProductCodeType

    -- ** TransitGatewayMulticastDeregisteredGroupMembers
    , TransitGatewayMulticastDeregisteredGroupMembers (..)
    , mkTransitGatewayMulticastDeregisteredGroupMembers
    , tgmdgmDeregisteredNetworkInterfaceIds
    , tgmdgmGroupIpAddress
    , tgmdgmTransitGatewayMulticastDomainId

    -- ** FlowLog
    , FlowLog (..)
    , mkFlowLog
    , flCreationTime
    , flDeliverLogsErrorMessage
    , flDeliverLogsPermissionArn
    , flDeliverLogsStatus
    , flFlowLogId
    , flFlowLogStatus
    , flLogDestination
    , flLogDestinationType
    , flLogFormat
    , flLogGroupName
    , flMaxAggregationInterval
    , flResourceId
    , flTags
    , flTrafficType

    -- ** VCpuInfo
    , VCpuInfo (..)
    , mkVCpuInfo
    , vciDefaultCores
    , vciDefaultThreadsPerCore
    , vciDefaultVCpus
    , vciValidCores
    , vciValidThreadsPerCore

    -- ** StaleSecurityGroup
    , StaleSecurityGroup (..)
    , mkStaleSecurityGroup
    , ssgDescription
    , ssgGroupId
    , ssgGroupName
    , ssgStaleIpPermissions
    , ssgStaleIpPermissionsEgress
    , ssgVpcId

    -- ** LaunchTemplateLicenseConfiguration
    , LaunchTemplateLicenseConfiguration (..)
    , mkLaunchTemplateLicenseConfiguration
    , ltlcLicenseConfigurationArn

    -- ** ListingStatus
    , ListingStatus (..)

    -- ** CreateTransitGatewayVpcAttachmentRequestOptions
    , CreateTransitGatewayVpcAttachmentRequestOptions (..)
    , mkCreateTransitGatewayVpcAttachmentRequestOptions
    , ctgvaroApplianceModeSupport
    , ctgvaroDnsSupport
    , ctgvaroIpv6Support

    -- ** Ipv4PoolEc2Id
    , Ipv4PoolEc2Id (..)

    -- ** Phase2EncryptionAlgorithmsRequestListValue
    , Phase2EncryptionAlgorithmsRequestListValue (..)
    , mkPhase2EncryptionAlgorithmsRequestListValue
    , pearlvfValue

    -- ** IpRange
    , IpRange (..)
    , mkIpRange
    , iCidrIp
    , iDescription

    -- ** LocalGatewayVirtualInterfaceGroup
    , LocalGatewayVirtualInterfaceGroup (..)
    , mkLocalGatewayVirtualInterfaceGroup
    , lgvigLocalGatewayId
    , lgvigLocalGatewayVirtualInterfaceGroupId
    , lgvigLocalGatewayVirtualInterfaceIds
    , lgvigOwnerId
    , lgvigTags

    -- ** VolumeStatusInfoStatus
    , VolumeStatusInfoStatus (..)

    -- ** AccountAttributeValue
    , AccountAttributeValue (..)
    , mkAccountAttributeValue
    , aavAttributeValue

    -- ** TransitGatewayAttachment
    , TransitGatewayAttachment (..)
    , mkTransitGatewayAttachment
    , tgaAssociation
    , tgaCreationTime
    , tgaResourceId
    , tgaResourceOwnerId
    , tgaResourceType
    , tgaState
    , tgaTags
    , tgaTransitGatewayAttachmentId
    , tgaTransitGatewayId
    , tgaTransitGatewayOwnerId

    -- ** SnapshotDiskContainer
    , SnapshotDiskContainer (..)
    , mkSnapshotDiskContainer
    , sdcDescription
    , sdcFormat
    , sdcUrl
    , sdcUserBucket

    -- ** Ipv6Range
    , Ipv6Range (..)
    , mkIpv6Range
    , irCidrIpv6
    , irDescription

    -- ** ReservedInstanceReservationValue
    , ReservedInstanceReservationValue (..)
    , mkReservedInstanceReservationValue
    , rirvReservationValue
    , rirvReservedInstanceId

    -- ** ConnectionNotificationId
    , ConnectionNotificationId (..)

    -- ** LaunchTemplateInstanceMetadataOptionsState
    , LaunchTemplateInstanceMetadataOptionsState (..)

    -- ** InstanceStorageInfo
    , InstanceStorageInfo (..)
    , mkInstanceStorageInfo
    , isiDisks
    , isiNvmeSupport
    , isiTotalSizeInGB

    -- ** FastSnapshotRestoreStateCode
    , FastSnapshotRestoreStateCode (..)

    -- ** ClientVpnEndpointStatus
    , ClientVpnEndpointStatus (..)
    , mkClientVpnEndpointStatus
    , cvesCode
    , cvesMessage

    -- ** RIProductDescription
    , RIProductDescription (..)

    -- ** ReservedInstancesOffering
    , ReservedInstancesOffering (..)
    , mkReservedInstancesOffering
    , rioAvailabilityZone
    , rioCurrencyCode
    , rioDuration
    , rioFixedPrice
    , rioInstanceTenancy
    , rioInstanceType
    , rioMarketplace
    , rioOfferingClass
    , rioOfferingType
    , rioPricingDetails
    , rioProductDescription
    , rioRecurringCharges
    , rioReservedInstancesOfferingId
    , rioScope
    , rioUsagePrice

    -- ** TransitGatewayAssociation
    , TransitGatewayAssociation (..)
    , mkTransitGatewayAssociation
    , tResourceId
    , tResourceType
    , tState
    , tTransitGatewayAttachmentId
    , tTransitGatewayRouteTableId

    -- ** InstanceTypeHypervisor
    , InstanceTypeHypervisor (..)

    -- ** VpnProtocol
    , VpnProtocol (..)

    -- ** ClassicLoadBalancer
    , ClassicLoadBalancer (..)
    , mkClassicLoadBalancer
    , clbName

    -- ** InstanceIpv6Address
    , InstanceIpv6Address (..)
    , mkInstanceIpv6Address
    , iiaIpv6Address

    -- ** Phase2DHGroupNumbersRequestListValue
    , Phase2DHGroupNumbersRequestListValue (..)
    , mkPhase2DHGroupNumbersRequestListValue
    , pdhgnrlvfValue

    -- ** ReservedInstances
    , ReservedInstances (..)
    , mkReservedInstances
    , riAvailabilityZone
    , riCurrencyCode
    , riDuration
    , riEnd
    , riFixedPrice
    , riInstanceCount
    , riInstanceTenancy
    , riInstanceType
    , riOfferingClass
    , riOfferingType
    , riProductDescription
    , riRecurringCharges
    , riReservedInstancesId
    , riScope
    , riStart
    , riState
    , riTags
    , riUsagePrice

    -- ** FleetData
    , FleetData (..)
    , mkFleetData
    , fdActivityStatus
    , fdClientToken
    , fdCreateTime
    , fdErrors
    , fdExcessCapacityTerminationPolicy
    , fdFleetId
    , fdFleetState
    , fdFulfilledCapacity
    , fdFulfilledOnDemandCapacity
    , fdInstances
    , fdLaunchTemplateConfigs
    , fdOnDemandOptions
    , fdReplaceUnhealthyInstances
    , fdSpotOptions
    , fdTags
    , fdTargetCapacitySpecification
    , fdTerminateInstancesWithExpiration
    , fdType
    , fdValidFrom
    , fdValidUntil

    -- ** TransitGatewayPropagationState
    , TransitGatewayPropagationState (..)

    -- ** DedicatedHostId
    , DedicatedHostId (..)

    -- ** DatafeedSubscriptionState
    , DatafeedSubscriptionState (..)

    -- ** ExportTaskState
    , ExportTaskState (..)

    -- ** ProductCodeValues
    , ProductCodeValues (..)

    -- ** CapacityReservationTargetResponse
    , CapacityReservationTargetResponse (..)
    , mkCapacityReservationTargetResponse
    , crtrCapacityReservationId
    , crtrCapacityReservationResourceGroupArn

    -- ** ScheduledInstancesBlockDeviceMapping
    , ScheduledInstancesBlockDeviceMapping (..)
    , mkScheduledInstancesBlockDeviceMapping
    , sibdmDeviceName
    , sibdmEbs
    , sibdmNoDevice
    , sibdmVirtualName

    -- ** VpnConnection
    , VpnConnection (..)
    , mkVpnConnection
    , vcCategory
    , vcCustomerGatewayConfiguration
    , vcCustomerGatewayId
    , vcOptions
    , vcRoutes
    , vcState
    , vcTags
    , vcTransitGatewayId
    , vcType
    , vcVgwTelemetry
    , vcVpnConnectionId
    , vcVpnGatewayId

    -- ** CapacityReservationTenancy
    , CapacityReservationTenancy (..)

    -- ** InstanceState
    , InstanceState (..)
    , mkInstanceState
    , isCode
    , isName

    -- ** InternetGatewayId
    , InternetGatewayId (..)

    -- ** VpcEndpoint
    , VpcEndpoint (..)
    , mkVpcEndpoint
    , veCreationTimestamp
    , veDnsEntries
    , veGroups
    , veLastError
    , veNetworkInterfaceIds
    , veOwnerId
    , vePolicyDocument
    , vePrivateDnsEnabled
    , veRequesterManaged
    , veRouteTableIds
    , veServiceName
    , veState
    , veSubnetIds
    , veTags
    , veVpcEndpointId
    , veVpcEndpointType
    , veVpcId

    -- ** PublicIpAddress
    , PublicIpAddress (..)

    -- ** TargetGroup
    , TargetGroup (..)
    , mkTargetGroup
    , tgArn

    -- ** SuccessfulQueuedPurchaseDeletion
    , SuccessfulQueuedPurchaseDeletion (..)
    , mkSuccessfulQueuedPurchaseDeletion
    , sqpdReservedInstancesId

    -- ** LaunchTemplateInstanceMarketOptionsRequest
    , LaunchTemplateInstanceMarketOptionsRequest (..)
    , mkLaunchTemplateInstanceMarketOptionsRequest
    , ltimorMarketType
    , ltimorSpotOptions

    -- ** ConnectionLogResponseOptions
    , ConnectionLogResponseOptions (..)
    , mkConnectionLogResponseOptions
    , clroCloudwatchLogGroup
    , clroCloudwatchLogStream
    , clroEnabled

    -- ** TransitGatewayPropagation
    , TransitGatewayPropagation (..)
    , mkTransitGatewayPropagation
    , tgpResourceId
    , tgpResourceType
    , tgpState
    , tgpTransitGatewayAttachmentId
    , tgpTransitGatewayRouteTableId

    -- ** ClientData
    , ClientData (..)
    , mkClientData
    , cdComment
    , cdUploadEnd
    , cdUploadSize
    , cdUploadStart

    -- ** Placement
    , Placement (..)
    , mkPlacement
    , pAffinity
    , pAvailabilityZone
    , pGroupName
    , pHostId
    , pHostResourceGroupArn
    , pPartitionNumber
    , pSpreadDomain
    , pTenancy

    -- ** TransitGatewayMulticastDomainAssociations
    , TransitGatewayMulticastDomainAssociations (..)
    , mkTransitGatewayMulticastDomainAssociations
    , tgmdasResourceId
    , tgmdasResourceType
    , tgmdasSubnets
    , tgmdasTransitGatewayAttachmentId
    , tgmdasTransitGatewayMulticastDomainId

    -- ** EventCode
    , EventCode (..)

    -- ** SpotInstanceType
    , SpotInstanceType (..)

    -- ** VpcPeeringConnection
    , VpcPeeringConnection (..)
    , mkVpcPeeringConnection
    , vpcAccepterVpcInfo
    , vpcExpirationTime
    , vpcRequesterVpcInfo
    , vpcStatus
    , vpcTags
    , vpcVpcPeeringConnectionId

    -- ** LaunchTemplateInstanceMetadataOptions
    , LaunchTemplateInstanceMetadataOptions (..)
    , mkLaunchTemplateInstanceMetadataOptions
    , ltimoHttpEndpoint
    , ltimoHttpPutResponseHopLimit
    , ltimoHttpTokens
    , ltimoState

    -- ** LaunchTemplateSpotMarketOptions
    , LaunchTemplateSpotMarketOptions (..)
    , mkLaunchTemplateSpotMarketOptions
    , ltsmoBlockDurationMinutes
    , ltsmoInstanceInterruptionBehavior
    , ltsmoMaxPrice
    , ltsmoSpotInstanceType
    , ltsmoValidUntil

    -- ** ByoipCidr
    , ByoipCidr (..)
    , mkByoipCidr
    , bcCidr
    , bcDescription
    , bcState
    , bcStatusMessage

    -- ** TargetCapacitySpecificationRequest
    , TargetCapacitySpecificationRequest (..)
    , mkTargetCapacitySpecificationRequest
    , tcsrTotalTargetCapacity
    , tcsrDefaultTargetCapacityType
    , tcsrOnDemandTargetCapacity
    , tcsrSpotTargetCapacity

    -- ** S3Storage
    , S3Storage (..)
    , mkS3Storage
    , ssAWSAccessKeyId
    , ssBucket
    , ssPrefix
    , ssUploadPolicy
    , ssUploadPolicySignature

    -- ** DefaultRouteTablePropagationValue
    , DefaultRouteTablePropagationValue (..)

    -- ** PlacementGroupName
    , PlacementGroupName (..)

    -- ** VgwTelemetry
    , VgwTelemetry (..)
    , mkVgwTelemetry
    , vtAcceptedRouteCount
    , vtCertificateArn
    , vtLastStatusChange
    , vtOutsideIpAddress
    , vtStatus
    , vtStatusMessage

    -- ** VpnStaticRoute
    , VpnStaticRoute (..)
    , mkVpnStaticRoute
    , vsrDestinationCidrBlock
    , vsrSource
    , vsrState

    -- ** InstanceStateName
    , InstanceStateName (..)

    -- ** Instance
    , Instance (..)
    , mkInstance
    , iAmiLaunchIndex
    , iArchitecture
    , iBlockDeviceMappings
    , iCapacityReservationId
    , iCapacityReservationSpecification
    , iClientToken
    , iCpuOptions
    , iEbsOptimized
    , iElasticGpuAssociations
    , iElasticInferenceAcceleratorAssociations
    , iEnaSupport
    , iEnclaveOptions
    , iHibernationOptions
    , iHypervisor
    , iIamInstanceProfile
    , iImageId
    , iInstanceId
    , iInstanceLifecycle
    , iInstanceType
    , iKernelId
    , iKeyName
    , iLaunchTime
    , iLicenses
    , iMetadataOptions
    , iMonitoring
    , iNetworkInterfaces
    , iOutpostArn
    , iPlacement
    , iPlatform
    , iPrivateDnsName
    , iPrivateIpAddress
    , iProductCodes
    , iPublicDnsName
    , iPublicIpAddress
    , iRamdiskId
    , iRootDeviceName
    , iRootDeviceType
    , iSecurityGroups
    , iSourceDestCheck
    , iSpotInstanceRequestId
    , iSriovNetSupport
    , iState
    , iStateReason
    , iStateTransitionReason
    , iSubnetId
    , iTags
    , iVirtualizationType
    , iVpcId

    -- ** RouteTableAssociationState
    , RouteTableAssociationState (..)
    , mkRouteTableAssociationState
    , rtasState
    , rtasStatusMessage

    -- ** ImportSnapshotTaskId
    , ImportSnapshotTaskId (..)

    -- ** ExportTask
    , ExportTask (..)
    , mkExportTask
    , etDescription
    , etExportTaskId
    , etExportToS3Task
    , etInstanceExportDetails
    , etState
    , etStatusMessage
    , etTags

    -- ** InstanceCreditSpecification
    , InstanceCreditSpecification (..)
    , mkInstanceCreditSpecification
    , icsCpuCredits
    , icsInstanceId

    -- ** LaunchTemplateEbsBlockDeviceRequest
    , LaunchTemplateEbsBlockDeviceRequest (..)
    , mkLaunchTemplateEbsBlockDeviceRequest
    , ltebdrDeleteOnTermination
    , ltebdrEncrypted
    , ltebdrIops
    , ltebdrKmsKeyId
    , ltebdrSnapshotId
    , ltebdrVolumeSize
    , ltebdrVolumeType

    -- ** CarrierGatewayId
    , CarrierGatewayId (..)

    -- ** ResetImageAttributeName
    , ResetImageAttributeName (..)

    -- ** RequestSpotLaunchSpecification
    , RequestSpotLaunchSpecification (..)
    , mkRequestSpotLaunchSpecification
    , rslsAddressingType
    , rslsBlockDeviceMappings
    , rslsEbsOptimized
    , rslsIamInstanceProfile
    , rslsImageId
    , rslsInstanceType
    , rslsKernelId
    , rslsKeyName
    , rslsMonitoring
    , rslsNetworkInterfaces
    , rslsPlacement
    , rslsRamdiskId
    , rslsSecurityGroupIds
    , rslsSecurityGroups
    , rslsSubnetId
    , rslsUserData

    -- ** ClientVpnRouteStatusCode
    , ClientVpnRouteStatusCode (..)

    -- ** LocalGatewayVirtualInterface
    , LocalGatewayVirtualInterface (..)
    , mkLocalGatewayVirtualInterface
    , lgviLocalAddress
    , lgviLocalBgpAsn
    , lgviLocalGatewayId
    , lgviLocalGatewayVirtualInterfaceId
    , lgviOwnerId
    , lgviPeerAddress
    , lgviPeerBgpAsn
    , lgviTags
    , lgviVlan

    -- ** ValidationWarning
    , ValidationWarning (..)
    , mkValidationWarning
    , vwErrors

    -- ** SnapshotId
    , SnapshotId (..)

    -- ** VolumeDetail
    , VolumeDetail (..)
    , mkVolumeDetail
    , vdSize

    -- ** TargetConfiguration
    , TargetConfiguration (..)
    , mkTargetConfiguration
    , tcInstanceCount
    , tcOfferingId

    -- ** PaymentOption
    , PaymentOption (..)

    -- ** PricingDetail
    , PricingDetail (..)
    , mkPricingDetail
    , pdCount
    , pdPrice

    -- ** ClientVpnAuthentication
    , ClientVpnAuthentication (..)
    , mkClientVpnAuthentication
    , cvaActiveDirectory
    , cvaFederatedAuthentication
    , cvaMutualAuthentication
    , cvaType

    -- ** TransitGatewayPrefixListReferenceState
    , TransitGatewayPrefixListReferenceState (..)

    -- ** IamInstanceProfileAssociationState
    , IamInstanceProfileAssociationState (..)

    -- ** ReservedInstancesListingId
    , ReservedInstancesListingId (..)

    -- ** NetworkInterfacePrivateIpAddress
    , NetworkInterfacePrivateIpAddress (..)
    , mkNetworkInterfacePrivateIpAddress
    , nipiaAssociation
    , nipiaPrimary
    , nipiaPrivateDnsName
    , nipiaPrivateIpAddress

    -- ** DiskImageFormat
    , DiskImageFormat (..)

    -- ** ActivityStatus
    , ActivityStatus (..)

    -- ** TransitGatewayRouteState
    , TransitGatewayRouteState (..)

    -- ** FleetLaunchTemplateConfig
    , FleetLaunchTemplateConfig (..)
    , mkFleetLaunchTemplateConfig
    , fltcLaunchTemplateSpecification
    , fltcOverrides

    -- ** BundleTaskError
    , BundleTaskError (..)
    , mkBundleTaskError
    , bteCode
    , bteMessage

    -- ** VpcClassicLink
    , VpcClassicLink (..)
    , mkVpcClassicLink
    , vclClassicLinkEnabled
    , vclTags
    , vclVpcId

    -- ** SpotOptionsRequest
    , SpotOptionsRequest (..)
    , mkSpotOptionsRequest
    , sorAllocationStrategy
    , sorInstanceInterruptionBehavior
    , sorInstancePoolsToUseCount
    , sorMaintenanceStrategies
    , sorMaxTotalPrice
    , sorMinTargetCapacity
    , sorSingleAvailabilityZone
    , sorSingleInstanceType

    -- ** VolumeStatusItem
    , VolumeStatusItem (..)
    , mkVolumeStatusItem
    , vsiActions
    , vsiAttachmentStatuses
    , vsiAvailabilityZone
    , vsiEvents
    , vsiOutpostArn
    , vsiVolumeId
    , vsiVolumeStatus

    -- ** ScheduledInstanceAvailability
    , ScheduledInstanceAvailability (..)
    , mkScheduledInstanceAvailability
    , siaAvailabilityZone
    , siaAvailableInstanceCount
    , siaFirstSlotStartTime
    , siaHourlyPrice
    , siaInstanceType
    , siaMaxTermDurationInDays
    , siaMinTermDurationInDays
    , siaNetworkPlatform
    , siaPlatform
    , siaPurchaseToken
    , siaRecurrence
    , siaSlotDurationInHours
    , siaTotalScheduledInstanceHours

    -- ** ElasticGpuSpecificationResponse
    , ElasticGpuSpecificationResponse (..)
    , mkElasticGpuSpecificationResponse
    , egsrType

    -- ** DestinationPrefixListId
    , DestinationPrefixListId (..)

    -- ** GatewayId
    , GatewayId (..)

    -- ** AssociationId
    , AssociationId (..)

    -- ** KeyName
    , KeyName (..)

    -- ** RamDiskId
    , RamDiskId (..)

    -- ** GroupId
    , GroupId (..)

    -- ** GroupName
    , GroupName (..)

    -- ** LocalGatewayRouteTableArn
    , LocalGatewayRouteTableArn (..)

    -- ** LocalGatewayRouteTableId
    , LocalGatewayRouteTableId (..)

    -- ** AssociatedRoleArn
    , AssociatedRoleArn (..)

    -- ** PrefixListArn
    , PrefixListArn (..)

    -- ** AttachmentId
    , AttachmentId (..)

    -- ** ServiceId
    , ServiceId (..)

    -- ** CertificateArn
    , CertificateArn (..)

    -- ** RoleArn
    , RoleArn (..)

    -- ** TargetVpcSubnetId
    , TargetVpcSubnetId (..)

    -- ** HostId
    , HostId (..)

    -- ** PoolId
    , PoolId (..)

    -- ** CustomerOwnedIpv4Pool
    , CustomerOwnedIpv4Pool (..)

    -- ** PoolArn
    , PoolArn (..)

    -- ** ConnectionId
    , ConnectionId (..)

    -- * Serialization types
    , Lude.Base64 (..)
    , Lude._Base64
    , Lude.Sensitive (..)
    , Lude._Sensitive
    , Lude.UTCTime
    , Lude.NominalDiffTime
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Waiters
import Network.AWS.EC2.ModifyCapacityReservation
import Network.AWS.EC2.GetAssociatedIpv6PoolCidrs
import Network.AWS.EC2.ImportInstance
import Network.AWS.EC2.RevokeSecurityGroupEgress
import Network.AWS.EC2.CreateNetworkInterfacePermission
import Network.AWS.EC2.SendDiagnosticInterrupt
import Network.AWS.EC2.DeleteLaunchTemplate
import Network.AWS.EC2.RejectVpcEndpointConnections
import Network.AWS.EC2.CreateVpnGateway
import Network.AWS.EC2.CreateNetworkAcl
import Network.AWS.EC2.DeleteKeyPair
import Network.AWS.EC2.DescribeSecurityGroupReferences
import Network.AWS.EC2.DeleteFleets
import Network.AWS.EC2.DescribeTags
import Network.AWS.EC2.CreateTransitGatewayRouteTable
import Network.AWS.EC2.ModifyInstanceMetadataOptions
import Network.AWS.EC2.UpdateSecurityGroupRuleDescriptionsIngress
import Network.AWS.EC2.DisassociateSubnetCidrBlock
import Network.AWS.EC2.DetachNetworkInterface
import Network.AWS.EC2.DetachInternetGateway
import Network.AWS.EC2.DeleteVpcEndpoints
import Network.AWS.EC2.DescribeClientVpnEndpoints
import Network.AWS.EC2.DeleteFlowLogs
import Network.AWS.EC2.DescribeVpcClassicLink
import Network.AWS.EC2.GetAssociatedEnclaveCertificateIamRoles
import Network.AWS.EC2.AssociateTransitGatewayMulticastDomain
import Network.AWS.EC2.ModifySubnetAttribute
import Network.AWS.EC2.DetachVolume
import Network.AWS.EC2.DescribeInstanceCreditSpecifications
import Network.AWS.EC2.CancelBundleTask
import Network.AWS.EC2.DescribeByoipCidrs
import Network.AWS.EC2.AcceptReservedInstancesExchangeQuote
import Network.AWS.EC2.ReleaseAddress
import Network.AWS.EC2.DescribeInstanceTypeOfferings
import Network.AWS.EC2.CreateInternetGateway
import Network.AWS.EC2.DeleteVpnConnection
import Network.AWS.EC2.DescribeBundleTasks
import Network.AWS.EC2.AuthorizeSecurityGroupEgress
import Network.AWS.EC2.EnableTransitGatewayRouteTablePropagation
import Network.AWS.EC2.DeregisterImage
import Network.AWS.EC2.DeleteVpcEndpointConnectionNotifications
import Network.AWS.EC2.DescribeCoipPools
import Network.AWS.EC2.GetTransitGatewayMulticastDomainAssociations
import Network.AWS.EC2.DeleteLocalGatewayRouteTableVpcAssociation
import Network.AWS.EC2.ModifyNetworkInterfaceAttribute
import Network.AWS.EC2.ModifyVpcTenancy
import Network.AWS.EC2.DescribeInstanceTypes
import Network.AWS.EC2.DescribeClientVpnAuthorizationRules
import Network.AWS.EC2.DeleteTransitGatewayVpcAttachment
import Network.AWS.EC2.DeleteTransitGatewayMulticastDomain
import Network.AWS.EC2.CancelReservedInstancesListing
import Network.AWS.EC2.AttachClassicLinkVpc
import Network.AWS.EC2.DisableTransitGatewayRouteTablePropagation
import Network.AWS.EC2.DescribeVpcClassicLinkDnsSupport
import Network.AWS.EC2.AssociateSubnetCidrBlock
import Network.AWS.EC2.RunScheduledInstances
import Network.AWS.EC2.CreateTransitGatewayRoute
import Network.AWS.EC2.CreateTransitGatewayPrefixListReference
import Network.AWS.EC2.CancelSpotFleetRequests
import Network.AWS.EC2.DescribeSpotPriceHistory
import Network.AWS.EC2.DescribeDhcpOptions
import Network.AWS.EC2.ImportImage
import Network.AWS.EC2.CreateLocalGatewayRouteTableVpcAssociation
import Network.AWS.EC2.CopyFpgaImage
import Network.AWS.EC2.ImportClientVpnClientCertificateRevocationList
import Network.AWS.EC2.StopInstances
import Network.AWS.EC2.EnableEbsEncryptionByDefault
import Network.AWS.EC2.DeregisterTransitGatewayMulticastGroupSources
import Network.AWS.EC2.ModifyLaunchTemplate
import Network.AWS.EC2.ModifyVpcEndpointConnectionNotification
import Network.AWS.EC2.DescribeInternetGateways
import Network.AWS.EC2.DisableVpcClassicLink
import Network.AWS.EC2.GetGroupsForCapacityReservation
import Network.AWS.EC2.DeleteLaunchTemplateVersions
import Network.AWS.EC2.BundleInstance
import Network.AWS.EC2.DescribeNetworkInterfaces
import Network.AWS.EC2.ReplaceNetworkAclAssociation
import Network.AWS.EC2.DescribeNatGateways
import Network.AWS.EC2.DescribeAddresses
import Network.AWS.EC2.RestoreManagedPrefixListVersion
import Network.AWS.EC2.DescribeSnapshotAttribute
import Network.AWS.EC2.DescribeIdentityIdFormat
import Network.AWS.EC2.ReplaceRoute
import Network.AWS.EC2.DescribeVpcEndpointServices
import Network.AWS.EC2.DeleteLocalGatewayRoute
import Network.AWS.EC2.AuthorizeSecurityGroupIngress
import Network.AWS.EC2.CreateVpcPeeringConnection
import Network.AWS.EC2.DescribeSubnets
import Network.AWS.EC2.GetTransitGatewayAttachmentPropagations
import Network.AWS.EC2.CreateTags
import Network.AWS.EC2.PurchaseReservedInstancesOffering
import Network.AWS.EC2.DeleteNetworkAclEntry
import Network.AWS.EC2.ResetSnapshotAttribute
import Network.AWS.EC2.DescribeVpnConnections
import Network.AWS.EC2.ModifyInstanceEventStartTime
import Network.AWS.EC2.DeleteRoute
import Network.AWS.EC2.ReplaceNetworkAclEntry
import Network.AWS.EC2.DescribeVpcEndpoints
import Network.AWS.EC2.CreateTrafficMirrorFilter
import Network.AWS.EC2.ResetInstanceAttribute
import Network.AWS.EC2.ModifyIdentityIdFormat
import Network.AWS.EC2.AttachNetworkInterface
import Network.AWS.EC2.CreateCapacityReservation
import Network.AWS.EC2.DescribeInstanceStatus
import Network.AWS.EC2.ImportKeyPair
import Network.AWS.EC2.DeleteTags
import Network.AWS.EC2.ConfirmProductInstance
import Network.AWS.EC2.DescribeInstanceAttribute
import Network.AWS.EC2.DescribeReservedInstancesOfferings
import Network.AWS.EC2.CreateCustomerGateway
import Network.AWS.EC2.DescribeFleets
import Network.AWS.EC2.CreateTransitGatewayPeeringAttachment
import Network.AWS.EC2.DeleteSecurityGroup
import Network.AWS.EC2.DescribePublicIpv4Pools
import Network.AWS.EC2.DescribeClientVpnTargetNetworks
import Network.AWS.EC2.DeleteVpcPeeringConnection
import Network.AWS.EC2.AttachInternetGateway
import Network.AWS.EC2.ModifyInstancePlacement
import Network.AWS.EC2.DescribeFlowLogs
import Network.AWS.EC2.DescribeLocalGatewayVirtualInterfaceGroups
import Network.AWS.EC2.DescribeLocalGatewayRouteTableVpcAssociations
import Network.AWS.EC2.DescribeVpcEndpointConnectionNotifications
import Network.AWS.EC2.GetManagedPrefixListEntries
import Network.AWS.EC2.RunInstances
import Network.AWS.EC2.CreateSnapshots
import Network.AWS.EC2.AssociateDhcpOptions
import Network.AWS.EC2.DeleteTrafficMirrorFilterRule
import Network.AWS.EC2.DescribeReservedInstances
import Network.AWS.EC2.DescribeIdFormat
import Network.AWS.EC2.DescribeVpcs
import Network.AWS.EC2.DescribeConversionTasks
import Network.AWS.EC2.CreateLaunchTemplateVersion
import Network.AWS.EC2.GetManagedPrefixListAssociations
import Network.AWS.EC2.DisableVpcClassicLinkDnsSupport
import Network.AWS.EC2.ApplySecurityGroupsToClientVpnTargetNetwork
import Network.AWS.EC2.DescribeTrafficMirrorTargets
import Network.AWS.EC2.DescribeVolumesModifications
import Network.AWS.EC2.ExportImage
import Network.AWS.EC2.CreateFpgaImage
import Network.AWS.EC2.AcceptVpcEndpointConnections
import Network.AWS.EC2.DeleteClientVpnEndpoint
import Network.AWS.EC2.SearchTransitGatewayRoutes
import Network.AWS.EC2.GetLaunchTemplateData
import Network.AWS.EC2.AllocateAddress
import Network.AWS.EC2.AcceptTransitGatewayVpcAttachment
import Network.AWS.EC2.CancelConversionTask
import Network.AWS.EC2.ModifyImageAttribute
import Network.AWS.EC2.CreateRouteTable
import Network.AWS.EC2.RejectTransitGatewayPeeringAttachment
import Network.AWS.EC2.ReportInstanceStatus
import Network.AWS.EC2.AttachVolume
import Network.AWS.EC2.RequestSpotInstances
import Network.AWS.EC2.WithdrawByoipCidr
import Network.AWS.EC2.DescribeHostReservationOfferings
import Network.AWS.EC2.ResetFpgaImageAttribute
import Network.AWS.EC2.ModifyVpnConnection
import Network.AWS.EC2.CreateTrafficMirrorFilterRule
import Network.AWS.EC2.DeleteTransitGateway
import Network.AWS.EC2.StartVpcEndpointServicePrivateDnsVerification
import Network.AWS.EC2.DescribeVolumes
import Network.AWS.EC2.RejectVpcPeeringConnection
import Network.AWS.EC2.DescribeClientVpnRoutes
import Network.AWS.EC2.DeleteVpnConnectionRoute
import Network.AWS.EC2.AssociateEnclaveCertificateIamRole
import Network.AWS.EC2.ModifyVpcEndpoint
import Network.AWS.EC2.DescribeFpgaImageAttribute
import Network.AWS.EC2.AllocateHosts
import Network.AWS.EC2.CreateClientVpnEndpoint
import Network.AWS.EC2.CreateTrafficMirrorSession
import Network.AWS.EC2.RegisterImage
import Network.AWS.EC2.AdvertiseByoipCidr
import Network.AWS.EC2.ModifyFleet
import Network.AWS.EC2.RevokeSecurityGroupIngress
import Network.AWS.EC2.GetEbsDefaultKmsKeyId
import Network.AWS.EC2.DescribeHostReservations
import Network.AWS.EC2.UpdateSecurityGroupRuleDescriptionsEgress
import Network.AWS.EC2.EnableVpcClassicLinkDnsSupport
import Network.AWS.EC2.DescribeVpcEndpointConnections
import Network.AWS.EC2.ModifyReservedInstances
import Network.AWS.EC2.DeleteFpgaImage
import Network.AWS.EC2.DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
import Network.AWS.EC2.DescribeScheduledInstances
import Network.AWS.EC2.SearchTransitGatewayMulticastGroups
import Network.AWS.EC2.CreateFlowLogs
import Network.AWS.EC2.DescribeSpotFleetRequests
import Network.AWS.EC2.MoveAddressToVpc
import Network.AWS.EC2.DescribeFleetInstances
import Network.AWS.EC2.DescribeLaunchTemplateVersions
import Network.AWS.EC2.ModifyInstanceCreditSpecification
import Network.AWS.EC2.DescribePrincipalIdFormat
import Network.AWS.EC2.DescribeTransitGateways
import Network.AWS.EC2.DeleteNetworkAcl
import Network.AWS.EC2.DisassociateTransitGatewayMulticastDomain
import Network.AWS.EC2.DeleteTransitGatewayRouteTable
import Network.AWS.EC2.CreateLaunchTemplate
import Network.AWS.EC2.CreateVpcEndpointConnectionNotification
import Network.AWS.EC2.DeleteNetworkInterfacePermission
import Network.AWS.EC2.DeleteVpnGateway
import Network.AWS.EC2.CreateTrafficMirrorTarget
import Network.AWS.EC2.DescribeImportImageTasks
import Network.AWS.EC2.DescribeVolumeAttribute
import Network.AWS.EC2.DescribeMovingAddresses
import Network.AWS.EC2.ExportTransitGatewayRoutes
import Network.AWS.EC2.GetPasswordData
import Network.AWS.EC2.CreateVpc
import Network.AWS.EC2.ModifyVpcPeeringConnectionOptions
import Network.AWS.EC2.DescribeFpgaImages
import Network.AWS.EC2.CopySnapshot
import Network.AWS.EC2.AcceptTransitGatewayPeeringAttachment
import Network.AWS.EC2.DisassociateAddress
import Network.AWS.EC2.ModifyTrafficMirrorFilterNetworkServices
import Network.AWS.EC2.DescribeEgressOnlyInternetGateways
import Network.AWS.EC2.DeleteVpc
import Network.AWS.EC2.CreateInstanceExportTask
import Network.AWS.EC2.RejectTransitGatewayVpcAttachment
import Network.AWS.EC2.DescribeTrafficMirrorSessions
import Network.AWS.EC2.GetTransitGatewayRouteTableAssociations
import Network.AWS.EC2.AssociateVpcCidrBlock
import Network.AWS.EC2.DescribeVpcAttribute
import Network.AWS.EC2.CreateVolume
import Network.AWS.EC2.CreateDefaultSubnet
import Network.AWS.EC2.DescribeScheduledInstanceAvailability
import Network.AWS.EC2.DisassociateClientVpnTargetNetwork
import Network.AWS.EC2.CreateClientVpnRoute
import Network.AWS.EC2.ModifyVolumeAttribute
import Network.AWS.EC2.ExportClientVpnClientConfiguration
import Network.AWS.EC2.DeleteTrafficMirrorTarget
import Network.AWS.EC2.DescribeSpotDatafeedSubscription
import Network.AWS.EC2.DescribeLocalGatewayRouteTables
import Network.AWS.EC2.DescribePrefixLists
import Network.AWS.EC2.AssociateTransitGatewayRouteTable
import Network.AWS.EC2.DeletePlacementGroup
import Network.AWS.EC2.ModifyTransitGateway
import Network.AWS.EC2.DeleteTransitGatewayPrefixListReference
import Network.AWS.EC2.CreateTransitGatewayMulticastDomain
import Network.AWS.EC2.DeregisterInstanceEventNotificationAttributes
import Network.AWS.EC2.RequestSpotFleet
import Network.AWS.EC2.DeleteTransitGatewayRoute
import Network.AWS.EC2.DisableEbsEncryptionByDefault
import Network.AWS.EC2.DeregisterTransitGatewayMulticastGroupMembers
import Network.AWS.EC2.CreateSubnet
import Network.AWS.EC2.CreateNetworkInterface
import Network.AWS.EC2.DescribeSecurityGroups
import Network.AWS.EC2.GetCapacityReservationUsage
import Network.AWS.EC2.CreateTransitGatewayVpcAttachment
import Network.AWS.EC2.DescribeExportTasks
import Network.AWS.EC2.ModifySpotFleetRequest
import Network.AWS.EC2.DetachVpnGateway
import Network.AWS.EC2.ModifyManagedPrefixList
import Network.AWS.EC2.GetHostReservationPurchasePreview
import Network.AWS.EC2.EnableVolumeIO
import Network.AWS.EC2.DescribeInstances
import Network.AWS.EC2.CreateNatGateway
import Network.AWS.EC2.DescribeLocalGatewayVirtualInterfaces
import Network.AWS.EC2.DescribeVpcPeeringConnections
import Network.AWS.EC2.CancelExportTask
import Network.AWS.EC2.CreateVpcEndpointServiceConfiguration
import Network.AWS.EC2.CreateDefaultVpc
import Network.AWS.EC2.DisassociateVpcCidrBlock
import Network.AWS.EC2.DescribeTrafficMirrorFilters
import Network.AWS.EC2.DescribeFastSnapshotRestores
import Network.AWS.EC2.CancelCapacityReservation
import Network.AWS.EC2.DeleteNetworkInterface
import Network.AWS.EC2.DisassociateTransitGatewayRouteTable
import Network.AWS.EC2.ReplaceRouteTableAssociation
import Network.AWS.EC2.StartInstances
import Network.AWS.EC2.CreatePlacementGroup
import Network.AWS.EC2.DescribeInstanceEventNotificationAttributes
import Network.AWS.EC2.DescribeCapacityReservations
import Network.AWS.EC2.ModifyClientVpnEndpoint
import Network.AWS.EC2.ModifyInstanceCapacityReservationAttributes
import Network.AWS.EC2.DescribeAggregateIdFormat
import Network.AWS.EC2.DescribeSnapshots
import Network.AWS.EC2.AssociateAddress
import Network.AWS.EC2.ModifyTrafficMirrorFilterRule
import Network.AWS.EC2.DescribeNetworkInterfaceAttribute
import Network.AWS.EC2.ReplaceIamInstanceProfileAssociation
import Network.AWS.EC2.AssociateClientVpnTargetNetwork
import Network.AWS.EC2.ReleaseHosts
import Network.AWS.EC2.ResetNetworkInterfaceAttribute
import Network.AWS.EC2.DeleteInternetGateway
import Network.AWS.EC2.DescribeReservedInstancesListings
import Network.AWS.EC2.CreateVpnConnection
import Network.AWS.EC2.ReplaceTransitGatewayRoute
import Network.AWS.EC2.CreateFleet
import Network.AWS.EC2.DeleteNatGateway
import Network.AWS.EC2.DescribeImportSnapshotTasks
import Network.AWS.EC2.GetCoipPoolUsage
import Network.AWS.EC2.DescribeCustomerGateways
import Network.AWS.EC2.DeleteSubnet
import Network.AWS.EC2.CopyImage
import Network.AWS.EC2.CreateVpcEndpoint
import Network.AWS.EC2.ModifyTrafficMirrorSession
import Network.AWS.EC2.DescribeCarrierGateways
import Network.AWS.EC2.DescribeTransitGatewayPeeringAttachments
import Network.AWS.EC2.DeleteQueuedReservedInstances
import Network.AWS.EC2.DescribeTransitGatewayMulticastDomains
import Network.AWS.EC2.GetDefaultCreditSpecification
import Network.AWS.EC2.UnmonitorInstances
import Network.AWS.EC2.DescribeTransitGatewayVpcAttachments
import Network.AWS.EC2.CreateSecurityGroup
import Network.AWS.EC2.GetEbsEncryptionByDefault
import Network.AWS.EC2.ImportVolume
import Network.AWS.EC2.DeleteCarrierGateway
import Network.AWS.EC2.DisableVgwRoutePropagation
import Network.AWS.EC2.DeleteTrafficMirrorFilter
import Network.AWS.EC2.ModifyVpnTunnelCertificate
import Network.AWS.EC2.CreateSpotDatafeedSubscription
import Network.AWS.EC2.CancelSpotInstanceRequests
import Network.AWS.EC2.CreateRoute
import Network.AWS.EC2.DescribeVpcEndpointServiceConfigurations
import Network.AWS.EC2.DeleteSnapshot
import Network.AWS.EC2.AssignPrivateIpAddresses
import Network.AWS.EC2.AuthorizeClientVpnIngress
import Network.AWS.EC2.DeleteTransitGatewayPeeringAttachment
import Network.AWS.EC2.ModifyInstanceAttribute
import Network.AWS.EC2.DeleteCustomerGateway
import Network.AWS.EC2.DisassociateIamInstanceProfile
import Network.AWS.EC2.TerminateClientVpnConnections
import Network.AWS.EC2.DisassociateRouteTable
import Network.AWS.EC2.GetConsoleScreenshot
import Network.AWS.EC2.ResetEbsDefaultKmsKeyId
import Network.AWS.EC2.AssignIpv6Addresses
import Network.AWS.EC2.ModifyVpnTunnelOptions
import Network.AWS.EC2.ModifyEbsDefaultKmsKeyId
import Network.AWS.EC2.DeleteSpotDatafeedSubscription
import Network.AWS.EC2.ModifyVolume
import Network.AWS.EC2.EnableVpcClassicLink
import Network.AWS.EC2.DescribePlacementGroups
import Network.AWS.EC2.ProvisionByoipCidr
import Network.AWS.EC2.DisassociateEnclaveCertificateIamRole
import Network.AWS.EC2.ModifyAvailabilityZoneGroup
import Network.AWS.EC2.DescribeStaleSecurityGroups
import Network.AWS.EC2.CreateCarrierGateway
import Network.AWS.EC2.DescribeExportImageTasks
import Network.AWS.EC2.PurchaseScheduledInstances
import Network.AWS.EC2.EnableVgwRoutePropagation
import Network.AWS.EC2.DescribeSpotFleetRequestHistory
import Network.AWS.EC2.ModifySnapshotAttribute
import Network.AWS.EC2.DescribeIamInstanceProfileAssociations
import Network.AWS.EC2.CreateSnapshot
import Network.AWS.EC2.CreateLocalGatewayRoute
import Network.AWS.EC2.CreateNetworkAclEntry
import Network.AWS.EC2.DescribeTransitGatewayAttachments
import Network.AWS.EC2.CreateReservedInstancesListing
import Network.AWS.EC2.DescribeIpv6Pools
import Network.AWS.EC2.AttachVpnGateway
import Network.AWS.EC2.DescribeLocalGateways
import Network.AWS.EC2.ModifyVpcEndpointServicePermissions
import Network.AWS.EC2.ExportClientVpnClientCertificateRevocationList
import Network.AWS.EC2.CreateDhcpOptions
import Network.AWS.EC2.RegisterTransitGatewayMulticastGroupSources
import Network.AWS.EC2.DescribeAccountAttributes
import Network.AWS.EC2.GetTransitGatewayRouteTablePropagations
import Network.AWS.EC2.ModifyFpgaImageAttribute
import Network.AWS.EC2.ModifyHosts
import Network.AWS.EC2.RebootInstances
import Network.AWS.EC2.ModifyVpcEndpointServiceConfiguration
import Network.AWS.EC2.CreateTransitGateway
import Network.AWS.EC2.UnassignIpv6Addresses
import Network.AWS.EC2.DeleteTrafficMirrorSession
import Network.AWS.EC2.CreateManagedPrefixList
import Network.AWS.EC2.AssociateIamInstanceProfile
import Network.AWS.EC2.ModifyDefaultCreditSpecification
import Network.AWS.EC2.DeleteEgressOnlyInternetGateway
import Network.AWS.EC2.PurchaseHostReservation
import Network.AWS.EC2.ModifyTransitGatewayVpcAttachment
import Network.AWS.EC2.CreateImage
import Network.AWS.EC2.DescribeClassicLinkInstances
import Network.AWS.EC2.TerminateInstances
import Network.AWS.EC2.GetTransitGatewayPrefixListReferences
import Network.AWS.EC2.DescribeKeyPairs
import Network.AWS.EC2.DisableFastSnapshotRestores
import Network.AWS.EC2.DescribeLaunchTemplates
import Network.AWS.EC2.CreateVpnConnectionRoute
import Network.AWS.EC2.AssociateRouteTable
import Network.AWS.EC2.DescribeVpnGateways
import Network.AWS.EC2.ModifyVpnConnectionOptions
import Network.AWS.EC2.GetConsoleOutput
import Network.AWS.EC2.DescribeHosts
import Network.AWS.EC2.DescribeImageAttribute
import Network.AWS.EC2.ModifyIdFormat
import Network.AWS.EC2.RegisterTransitGatewayMulticastGroupMembers
import Network.AWS.EC2.DeleteManagedPrefixList
import Network.AWS.EC2.DeleteRouteTable
import Network.AWS.EC2.ResetImageAttribute
import Network.AWS.EC2.ModifyTransitGatewayPrefixListReference
import Network.AWS.EC2.DescribeTransitGatewayRouteTables
import Network.AWS.EC2.CreateEgressOnlyInternetGateway
import Network.AWS.EC2.DescribeReservedInstancesModifications
import Network.AWS.EC2.DescribeSpotInstanceRequests
import Network.AWS.EC2.RevokeClientVpnIngress
import Network.AWS.EC2.UnassignPrivateIpAddresses
import Network.AWS.EC2.DescribeNetworkInterfacePermissions
import Network.AWS.EC2.EnableFastSnapshotRestores
import Network.AWS.EC2.DescribeVpcEndpointServicePermissions
import Network.AWS.EC2.DeleteDhcpOptions
import Network.AWS.EC2.RegisterInstanceEventNotificationAttributes
import Network.AWS.EC2.DescribeNetworkAcls
import Network.AWS.EC2.CancelImportTask
import Network.AWS.EC2.DetachClassicLinkVpc
import Network.AWS.EC2.DescribeRegions
import Network.AWS.EC2.MonitorInstances
import Network.AWS.EC2.SearchLocalGatewayRoutes
import Network.AWS.EC2.DeleteClientVpnRoute
import Network.AWS.EC2.AcceptVpcPeeringConnection
import Network.AWS.EC2.ImportSnapshot
import Network.AWS.EC2.DescribeVolumeStatus
import Network.AWS.EC2.DescribeRouteTables
import Network.AWS.EC2.DescribeAvailabilityZones
import Network.AWS.EC2.ModifyVpcAttribute
import Network.AWS.EC2.DescribeClientVpnConnections
import Network.AWS.EC2.DescribeFleetHistory
import Network.AWS.EC2.DescribeImages
import Network.AWS.EC2.DescribeElasticGpus
import Network.AWS.EC2.RestoreAddressToClassic
import Network.AWS.EC2.DescribeManagedPrefixLists
import Network.AWS.EC2.CreateKeyPair
import Network.AWS.EC2.GetReservedInstancesExchangeQuote
import Network.AWS.EC2.DeleteVolume
import Network.AWS.EC2.DeprovisionByoipCidr
import Network.AWS.EC2.DeleteVpcEndpointServiceConfigurations
import Network.AWS.EC2.DescribeSpotFleetInstances
import qualified Network.AWS.Prelude as Lude

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'EC2'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
