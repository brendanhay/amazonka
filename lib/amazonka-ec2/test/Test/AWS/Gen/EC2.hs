{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.EC2
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.EC2 where

import Data.Proxy
import Network.AWS.EC2
import Test.AWS.EC2.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestModifyCapacityReservation $
--             mkModifyCapacityReservation
--
--         , requestGetAssociatedIpv6PoolCidrs $
--             mkGetAssociatedIpv6PoolCidrs
--
--         , requestImportInstance $
--             mkImportInstance
--
--         , requestRevokeSecurityGroupEgress $
--             mkRevokeSecurityGroupEgress
--
--         , requestCreateNetworkInterfacePermission $
--             mkCreateNetworkInterfacePermission
--
--         , requestSendDiagnosticInterrupt $
--             mkSendDiagnosticInterrupt
--
--         , requestDeleteLaunchTemplate $
--             mkDeleteLaunchTemplate
--
--         , requestRejectVpcEndpointConnections $
--             mkRejectVpcEndpointConnections
--
--         , requestCreateVpnGateway $
--             mkCreateVpnGateway
--
--         , requestCreateNetworkAcl $
--             mkCreateNetworkAcl
--
--         , requestDeleteKeyPair $
--             mkDeleteKeyPair
--
--         , requestDescribeSecurityGroupReferences $
--             mkDescribeSecurityGroupReferences
--
--         , requestDeleteFleets $
--             mkDeleteFleets
--
--         , requestDescribeTags $
--             mkDescribeTags
--
--         , requestCreateTransitGatewayRouteTable $
--             mkCreateTransitGatewayRouteTable
--
--         , requestModifyInstanceMetadataOptions $
--             mkModifyInstanceMetadataOptions
--
--         , requestUpdateSecurityGroupRuleDescriptionsIngress $
--             mkUpdateSecurityGroupRuleDescriptionsIngress
--
--         , requestDisassociateSubnetCidrBlock $
--             mkDisassociateSubnetCidrBlock
--
--         , requestDetachNetworkInterface $
--             mkDetachNetworkInterface
--
--         , requestDetachInternetGateway $
--             mkDetachInternetGateway
--
--         , requestDeleteVpcEndpoints $
--             mkDeleteVpcEndpoints
--
--         , requestDescribeClientVpnEndpoints $
--             mkDescribeClientVpnEndpoints
--
--         , requestDeleteFlowLogs $
--             mkDeleteFlowLogs
--
--         , requestDescribeVpcClassicLink $
--             mkDescribeVpcClassicLink
--
--         , requestGetAssociatedEnclaveCertificateIamRoles $
--             mkGetAssociatedEnclaveCertificateIamRoles
--
--         , requestAssociateTransitGatewayMulticastDomain $
--             mkAssociateTransitGatewayMulticastDomain
--
--         , requestModifySubnetAttribute $
--             mkModifySubnetAttribute
--
--         , requestDetachVolume $
--             mkDetachVolume
--
--         , requestDescribeInstanceCreditSpecifications $
--             mkDescribeInstanceCreditSpecifications
--
--         , requestCancelBundleTask $
--             mkCancelBundleTask
--
--         , requestDescribeByoipCidrs $
--             mkDescribeByoipCidrs
--
--         , requestAcceptReservedInstancesExchangeQuote $
--             mkAcceptReservedInstancesExchangeQuote
--
--         , requestReleaseAddress $
--             mkReleaseAddress
--
--         , requestDescribeInstanceTypeOfferings $
--             mkDescribeInstanceTypeOfferings
--
--         , requestCreateInternetGateway $
--             mkCreateInternetGateway
--
--         , requestDeleteVpnConnection $
--             mkDeleteVpnConnection
--
--         , requestDescribeBundleTasks $
--             mkDescribeBundleTasks
--
--         , requestAuthorizeSecurityGroupEgress $
--             mkAuthorizeSecurityGroupEgress
--
--         , requestEnableTransitGatewayRouteTablePropagation $
--             mkEnableTransitGatewayRouteTablePropagation
--
--         , requestDeregisterImage $
--             mkDeregisterImage
--
--         , requestDeleteVpcEndpointConnectionNotifications $
--             mkDeleteVpcEndpointConnectionNotifications
--
--         , requestDescribeCoipPools $
--             mkDescribeCoipPools
--
--         , requestGetTransitGatewayMulticastDomainAssociations $
--             mkGetTransitGatewayMulticastDomainAssociations
--
--         , requestDeleteLocalGatewayRouteTableVpcAssociation $
--             mkDeleteLocalGatewayRouteTableVpcAssociation
--
--         , requestModifyNetworkInterfaceAttribute $
--             mkModifyNetworkInterfaceAttribute
--
--         , requestModifyVpcTenancy $
--             mkModifyVpcTenancy
--
--         , requestDescribeInstanceTypes $
--             mkDescribeInstanceTypes
--
--         , requestDescribeClientVpnAuthorizationRules $
--             mkDescribeClientVpnAuthorizationRules
--
--         , requestDeleteTransitGatewayVpcAttachment $
--             mkDeleteTransitGatewayVpcAttachment
--
--         , requestDeleteTransitGatewayMulticastDomain $
--             mkDeleteTransitGatewayMulticastDomain
--
--         , requestCancelReservedInstancesListing $
--             mkCancelReservedInstancesListing
--
--         , requestAttachClassicLinkVpc $
--             mkAttachClassicLinkVpc
--
--         , requestDisableTransitGatewayRouteTablePropagation $
--             mkDisableTransitGatewayRouteTablePropagation
--
--         , requestDescribeVpcClassicLinkDnsSupport $
--             mkDescribeVpcClassicLinkDnsSupport
--
--         , requestAssociateSubnetCidrBlock $
--             mkAssociateSubnetCidrBlock
--
--         , requestRunScheduledInstances $
--             mkRunScheduledInstances
--
--         , requestCreateTransitGatewayRoute $
--             mkCreateTransitGatewayRoute
--
--         , requestCreateTransitGatewayPrefixListReference $
--             mkCreateTransitGatewayPrefixListReference
--
--         , requestCancelSpotFleetRequests $
--             mkCancelSpotFleetRequests
--
--         , requestDescribeSpotPriceHistory $
--             mkDescribeSpotPriceHistory
--
--         , requestDescribeDhcpOptions $
--             mkDescribeDhcpOptions
--
--         , requestImportImage $
--             mkImportImage
--
--         , requestCreateLocalGatewayRouteTableVpcAssociation $
--             mkCreateLocalGatewayRouteTableVpcAssociation
--
--         , requestCopyFpgaImage $
--             mkCopyFpgaImage
--
--         , requestImportClientVpnClientCertificateRevocationList $
--             mkImportClientVpnClientCertificateRevocationList
--
--         , requestStopInstances $
--             mkStopInstances
--
--         , requestEnableEbsEncryptionByDefault $
--             mkEnableEbsEncryptionByDefault
--
--         , requestDeregisterTransitGatewayMulticastGroupSources $
--             mkDeregisterTransitGatewayMulticastGroupSources
--
--         , requestModifyLaunchTemplate $
--             mkModifyLaunchTemplate
--
--         , requestModifyVpcEndpointConnectionNotification $
--             mkModifyVpcEndpointConnectionNotification
--
--         , requestDescribeInternetGateways $
--             mkDescribeInternetGateways
--
--         , requestDisableVpcClassicLink $
--             mkDisableVpcClassicLink
--
--         , requestGetGroupsForCapacityReservation $
--             mkGetGroupsForCapacityReservation
--
--         , requestDeleteLaunchTemplateVersions $
--             mkDeleteLaunchTemplateVersions
--
--         , requestBundleInstance $
--             mkBundleInstance
--
--         , requestDescribeNetworkInterfaces $
--             mkDescribeNetworkInterfaces
--
--         , requestReplaceNetworkAclAssociation $
--             mkReplaceNetworkAclAssociation
--
--         , requestDescribeNatGateways $
--             mkDescribeNatGateways
--
--         , requestDescribeAddresses $
--             mkDescribeAddresses
--
--         , requestRestoreManagedPrefixListVersion $
--             mkRestoreManagedPrefixListVersion
--
--         , requestDescribeSnapshotAttribute $
--             mkDescribeSnapshotAttribute
--
--         , requestDescribeIdentityIdFormat $
--             mkDescribeIdentityIdFormat
--
--         , requestReplaceRoute $
--             mkReplaceRoute
--
--         , requestDescribeVpcEndpointServices $
--             mkDescribeVpcEndpointServices
--
--         , requestDeleteLocalGatewayRoute $
--             mkDeleteLocalGatewayRoute
--
--         , requestAuthorizeSecurityGroupIngress $
--             mkAuthorizeSecurityGroupIngress
--
--         , requestCreateVpcPeeringConnection $
--             mkCreateVpcPeeringConnection
--
--         , requestDescribeSubnets $
--             mkDescribeSubnets
--
--         , requestGetTransitGatewayAttachmentPropagations $
--             mkGetTransitGatewayAttachmentPropagations
--
--         , requestCreateTags $
--             mkCreateTags
--
--         , requestPurchaseReservedInstancesOffering $
--             mkPurchaseReservedInstancesOffering
--
--         , requestDeleteNetworkAclEntry $
--             mkDeleteNetworkAclEntry
--
--         , requestResetSnapshotAttribute $
--             mkResetSnapshotAttribute
--
--         , requestDescribeVpnConnections $
--             mkDescribeVpnConnections
--
--         , requestModifyInstanceEventStartTime $
--             mkModifyInstanceEventStartTime
--
--         , requestDeleteRoute $
--             mkDeleteRoute
--
--         , requestReplaceNetworkAclEntry $
--             mkReplaceNetworkAclEntry
--
--         , requestDescribeVpcEndpoints $
--             mkDescribeVpcEndpoints
--
--         , requestCreateTrafficMirrorFilter $
--             mkCreateTrafficMirrorFilter
--
--         , requestResetInstanceAttribute $
--             mkResetInstanceAttribute
--
--         , requestModifyIdentityIdFormat $
--             mkModifyIdentityIdFormat
--
--         , requestAttachNetworkInterface $
--             mkAttachNetworkInterface
--
--         , requestCreateCapacityReservation $
--             mkCreateCapacityReservation
--
--         , requestDescribeInstanceStatus $
--             mkDescribeInstanceStatus
--
--         , requestImportKeyPair $
--             mkImportKeyPair
--
--         , requestDeleteTags $
--             mkDeleteTags
--
--         , requestConfirmProductInstance $
--             mkConfirmProductInstance
--
--         , requestDescribeInstanceAttribute $
--             mkDescribeInstanceAttribute
--
--         , requestDescribeReservedInstancesOfferings $
--             mkDescribeReservedInstancesOfferings
--
--         , requestCreateCustomerGateway $
--             mkCreateCustomerGateway
--
--         , requestDescribeFleets $
--             mkDescribeFleets
--
--         , requestCreateTransitGatewayPeeringAttachment $
--             mkCreateTransitGatewayPeeringAttachment
--
--         , requestDeleteSecurityGroup $
--             mkDeleteSecurityGroup
--
--         , requestDescribePublicIpv4Pools $
--             mkDescribePublicIpv4Pools
--
--         , requestDescribeClientVpnTargetNetworks $
--             mkDescribeClientVpnTargetNetworks
--
--         , requestDeleteVpcPeeringConnection $
--             mkDeleteVpcPeeringConnection
--
--         , requestAttachInternetGateway $
--             mkAttachInternetGateway
--
--         , requestModifyInstancePlacement $
--             mkModifyInstancePlacement
--
--         , requestDescribeFlowLogs $
--             mkDescribeFlowLogs
--
--         , requestDescribeLocalGatewayVirtualInterfaceGroups $
--             mkDescribeLocalGatewayVirtualInterfaceGroups
--
--         , requestDescribeLocalGatewayRouteTableVpcAssociations $
--             mkDescribeLocalGatewayRouteTableVpcAssociations
--
--         , requestDescribeVpcEndpointConnectionNotifications $
--             mkDescribeVpcEndpointConnectionNotifications
--
--         , requestGetManagedPrefixListEntries $
--             mkGetManagedPrefixListEntries
--
--         , requestRunInstances $
--             mkRunInstances
--
--         , requestCreateSnapshots $
--             mkCreateSnapshots
--
--         , requestAssociateDhcpOptions $
--             mkAssociateDhcpOptions
--
--         , requestDeleteTrafficMirrorFilterRule $
--             mkDeleteTrafficMirrorFilterRule
--
--         , requestDescribeReservedInstances $
--             mkDescribeReservedInstances
--
--         , requestDescribeIdFormat $
--             mkDescribeIdFormat
--
--         , requestDescribeVpcs $
--             mkDescribeVpcs
--
--         , requestDescribeConversionTasks $
--             mkDescribeConversionTasks
--
--         , requestCreateLaunchTemplateVersion $
--             mkCreateLaunchTemplateVersion
--
--         , requestGetManagedPrefixListAssociations $
--             mkGetManagedPrefixListAssociations
--
--         , requestDisableVpcClassicLinkDnsSupport $
--             mkDisableVpcClassicLinkDnsSupport
--
--         , requestApplySecurityGroupsToClientVpnTargetNetwork $
--             mkApplySecurityGroupsToClientVpnTargetNetwork
--
--         , requestDescribeTrafficMirrorTargets $
--             mkDescribeTrafficMirrorTargets
--
--         , requestDescribeVolumesModifications $
--             mkDescribeVolumesModifications
--
--         , requestExportImage $
--             mkExportImage
--
--         , requestCreateFpgaImage $
--             mkCreateFpgaImage
--
--         , requestAcceptVpcEndpointConnections $
--             mkAcceptVpcEndpointConnections
--
--         , requestDeleteClientVpnEndpoint $
--             mkDeleteClientVpnEndpoint
--
--         , requestSearchTransitGatewayRoutes $
--             mkSearchTransitGatewayRoutes
--
--         , requestGetLaunchTemplateData $
--             mkGetLaunchTemplateData
--
--         , requestAllocateAddress $
--             mkAllocateAddress
--
--         , requestAcceptTransitGatewayVpcAttachment $
--             mkAcceptTransitGatewayVpcAttachment
--
--         , requestCancelConversionTask $
--             mkCancelConversionTask
--
--         , requestModifyImageAttribute $
--             mkModifyImageAttribute
--
--         , requestCreateRouteTable $
--             mkCreateRouteTable
--
--         , requestRejectTransitGatewayPeeringAttachment $
--             mkRejectTransitGatewayPeeringAttachment
--
--         , requestReportInstanceStatus $
--             mkReportInstanceStatus
--
--         , requestAttachVolume $
--             mkAttachVolume
--
--         , requestRequestSpotInstances $
--             mkRequestSpotInstances
--
--         , requestWithdrawByoipCidr $
--             mkWithdrawByoipCidr
--
--         , requestDescribeHostReservationOfferings $
--             mkDescribeHostReservationOfferings
--
--         , requestResetFpgaImageAttribute $
--             mkResetFpgaImageAttribute
--
--         , requestModifyVpnConnection $
--             mkModifyVpnConnection
--
--         , requestCreateTrafficMirrorFilterRule $
--             mkCreateTrafficMirrorFilterRule
--
--         , requestDeleteTransitGateway $
--             mkDeleteTransitGateway
--
--         , requestStartVpcEndpointServicePrivateDnsVerification $
--             mkStartVpcEndpointServicePrivateDnsVerification
--
--         , requestDescribeVolumes $
--             mkDescribeVolumes
--
--         , requestRejectVpcPeeringConnection $
--             mkRejectVpcPeeringConnection
--
--         , requestDescribeClientVpnRoutes $
--             mkDescribeClientVpnRoutes
--
--         , requestDeleteVpnConnectionRoute $
--             mkDeleteVpnConnectionRoute
--
--         , requestAssociateEnclaveCertificateIamRole $
--             mkAssociateEnclaveCertificateIamRole
--
--         , requestModifyVpcEndpoint $
--             mkModifyVpcEndpoint
--
--         , requestDescribeFpgaImageAttribute $
--             mkDescribeFpgaImageAttribute
--
--         , requestAllocateHosts $
--             mkAllocateHosts
--
--         , requestCreateClientVpnEndpoint $
--             mkCreateClientVpnEndpoint
--
--         , requestCreateTrafficMirrorSession $
--             mkCreateTrafficMirrorSession
--
--         , requestRegisterImage $
--             mkRegisterImage
--
--         , requestAdvertiseByoipCidr $
--             mkAdvertiseByoipCidr
--
--         , requestModifyFleet $
--             mkModifyFleet
--
--         , requestRevokeSecurityGroupIngress $
--             mkRevokeSecurityGroupIngress
--
--         , requestGetEbsDefaultKmsKeyId $
--             mkGetEbsDefaultKmsKeyId
--
--         , requestDescribeHostReservations $
--             mkDescribeHostReservations
--
--         , requestUpdateSecurityGroupRuleDescriptionsEgress $
--             mkUpdateSecurityGroupRuleDescriptionsEgress
--
--         , requestEnableVpcClassicLinkDnsSupport $
--             mkEnableVpcClassicLinkDnsSupport
--
--         , requestDescribeVpcEndpointConnections $
--             mkDescribeVpcEndpointConnections
--
--         , requestModifyReservedInstances $
--             mkModifyReservedInstances
--
--         , requestDeleteFpgaImage $
--             mkDeleteFpgaImage
--
--         , requestDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations $
--             mkDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
--
--         , requestDescribeScheduledInstances $
--             mkDescribeScheduledInstances
--
--         , requestSearchTransitGatewayMulticastGroups $
--             mkSearchTransitGatewayMulticastGroups
--
--         , requestCreateFlowLogs $
--             mkCreateFlowLogs
--
--         , requestDescribeSpotFleetRequests $
--             mkDescribeSpotFleetRequests
--
--         , requestMoveAddressToVpc $
--             mkMoveAddressToVpc
--
--         , requestDescribeFleetInstances $
--             mkDescribeFleetInstances
--
--         , requestDescribeLaunchTemplateVersions $
--             mkDescribeLaunchTemplateVersions
--
--         , requestModifyInstanceCreditSpecification $
--             mkModifyInstanceCreditSpecification
--
--         , requestDescribePrincipalIdFormat $
--             mkDescribePrincipalIdFormat
--
--         , requestDescribeTransitGateways $
--             mkDescribeTransitGateways
--
--         , requestDeleteNetworkAcl $
--             mkDeleteNetworkAcl
--
--         , requestDisassociateTransitGatewayMulticastDomain $
--             mkDisassociateTransitGatewayMulticastDomain
--
--         , requestDeleteTransitGatewayRouteTable $
--             mkDeleteTransitGatewayRouteTable
--
--         , requestCreateLaunchTemplate $
--             mkCreateLaunchTemplate
--
--         , requestCreateVpcEndpointConnectionNotification $
--             mkCreateVpcEndpointConnectionNotification
--
--         , requestDeleteNetworkInterfacePermission $
--             mkDeleteNetworkInterfacePermission
--
--         , requestDeleteVpnGateway $
--             mkDeleteVpnGateway
--
--         , requestCreateTrafficMirrorTarget $
--             mkCreateTrafficMirrorTarget
--
--         , requestDescribeImportImageTasks $
--             mkDescribeImportImageTasks
--
--         , requestDescribeVolumeAttribute $
--             mkDescribeVolumeAttribute
--
--         , requestDescribeMovingAddresses $
--             mkDescribeMovingAddresses
--
--         , requestExportTransitGatewayRoutes $
--             mkExportTransitGatewayRoutes
--
--         , requestGetPasswordData $
--             mkGetPasswordData
--
--         , requestCreateVpc $
--             mkCreateVpc
--
--         , requestModifyVpcPeeringConnectionOptions $
--             mkModifyVpcPeeringConnectionOptions
--
--         , requestDescribeFpgaImages $
--             mkDescribeFpgaImages
--
--         , requestCopySnapshot $
--             mkCopySnapshot
--
--         , requestAcceptTransitGatewayPeeringAttachment $
--             mkAcceptTransitGatewayPeeringAttachment
--
--         , requestDisassociateAddress $
--             mkDisassociateAddress
--
--         , requestModifyTrafficMirrorFilterNetworkServices $
--             mkModifyTrafficMirrorFilterNetworkServices
--
--         , requestDescribeEgressOnlyInternetGateways $
--             mkDescribeEgressOnlyInternetGateways
--
--         , requestDeleteVpc $
--             mkDeleteVpc
--
--         , requestCreateInstanceExportTask $
--             mkCreateInstanceExportTask
--
--         , requestRejectTransitGatewayVpcAttachment $
--             mkRejectTransitGatewayVpcAttachment
--
--         , requestDescribeTrafficMirrorSessions $
--             mkDescribeTrafficMirrorSessions
--
--         , requestGetTransitGatewayRouteTableAssociations $
--             mkGetTransitGatewayRouteTableAssociations
--
--         , requestAssociateVpcCidrBlock $
--             mkAssociateVpcCidrBlock
--
--         , requestDescribeVpcAttribute $
--             mkDescribeVpcAttribute
--
--         , requestCreateVolume $
--             mkCreateVolume
--
--         , requestCreateDefaultSubnet $
--             mkCreateDefaultSubnet
--
--         , requestDescribeScheduledInstanceAvailability $
--             mkDescribeScheduledInstanceAvailability
--
--         , requestDisassociateClientVpnTargetNetwork $
--             mkDisassociateClientVpnTargetNetwork
--
--         , requestCreateClientVpnRoute $
--             mkCreateClientVpnRoute
--
--         , requestModifyVolumeAttribute $
--             mkModifyVolumeAttribute
--
--         , requestExportClientVpnClientConfiguration $
--             mkExportClientVpnClientConfiguration
--
--         , requestDeleteTrafficMirrorTarget $
--             mkDeleteTrafficMirrorTarget
--
--         , requestDescribeSpotDatafeedSubscription $
--             mkDescribeSpotDatafeedSubscription
--
--         , requestDescribeLocalGatewayRouteTables $
--             mkDescribeLocalGatewayRouteTables
--
--         , requestDescribePrefixLists $
--             mkDescribePrefixLists
--
--         , requestAssociateTransitGatewayRouteTable $
--             mkAssociateTransitGatewayRouteTable
--
--         , requestDeletePlacementGroup $
--             mkDeletePlacementGroup
--
--         , requestModifyTransitGateway $
--             mkModifyTransitGateway
--
--         , requestDeleteTransitGatewayPrefixListReference $
--             mkDeleteTransitGatewayPrefixListReference
--
--         , requestCreateTransitGatewayMulticastDomain $
--             mkCreateTransitGatewayMulticastDomain
--
--         , requestDeregisterInstanceEventNotificationAttributes $
--             mkDeregisterInstanceEventNotificationAttributes
--
--         , requestRequestSpotFleet $
--             mkRequestSpotFleet
--
--         , requestDeleteTransitGatewayRoute $
--             mkDeleteTransitGatewayRoute
--
--         , requestDisableEbsEncryptionByDefault $
--             mkDisableEbsEncryptionByDefault
--
--         , requestDeregisterTransitGatewayMulticastGroupMembers $
--             mkDeregisterTransitGatewayMulticastGroupMembers
--
--         , requestCreateSubnet $
--             mkCreateSubnet
--
--         , requestCreateNetworkInterface $
--             mkCreateNetworkInterface
--
--         , requestDescribeSecurityGroups $
--             mkDescribeSecurityGroups
--
--         , requestGetCapacityReservationUsage $
--             mkGetCapacityReservationUsage
--
--         , requestCreateTransitGatewayVpcAttachment $
--             mkCreateTransitGatewayVpcAttachment
--
--         , requestDescribeExportTasks $
--             mkDescribeExportTasks
--
--         , requestModifySpotFleetRequest $
--             mkModifySpotFleetRequest
--
--         , requestDetachVpnGateway $
--             mkDetachVpnGateway
--
--         , requestModifyManagedPrefixList $
--             mkModifyManagedPrefixList
--
--         , requestGetHostReservationPurchasePreview $
--             mkGetHostReservationPurchasePreview
--
--         , requestEnableVolumeIO $
--             mkEnableVolumeIO
--
--         , requestDescribeInstances $
--             mkDescribeInstances
--
--         , requestCreateNatGateway $
--             mkCreateNatGateway
--
--         , requestDescribeLocalGatewayVirtualInterfaces $
--             mkDescribeLocalGatewayVirtualInterfaces
--
--         , requestDescribeVpcPeeringConnections $
--             mkDescribeVpcPeeringConnections
--
--         , requestCancelExportTask $
--             mkCancelExportTask
--
--         , requestCreateVpcEndpointServiceConfiguration $
--             mkCreateVpcEndpointServiceConfiguration
--
--         , requestCreateDefaultVpc $
--             mkCreateDefaultVpc
--
--         , requestDisassociateVpcCidrBlock $
--             mkDisassociateVpcCidrBlock
--
--         , requestDescribeTrafficMirrorFilters $
--             mkDescribeTrafficMirrorFilters
--
--         , requestDescribeFastSnapshotRestores $
--             mkDescribeFastSnapshotRestores
--
--         , requestCancelCapacityReservation $
--             mkCancelCapacityReservation
--
--         , requestDeleteNetworkInterface $
--             mkDeleteNetworkInterface
--
--         , requestDisassociateTransitGatewayRouteTable $
--             mkDisassociateTransitGatewayRouteTable
--
--         , requestReplaceRouteTableAssociation $
--             mkReplaceRouteTableAssociation
--
--         , requestStartInstances $
--             mkStartInstances
--
--         , requestCreatePlacementGroup $
--             mkCreatePlacementGroup
--
--         , requestDescribeInstanceEventNotificationAttributes $
--             mkDescribeInstanceEventNotificationAttributes
--
--         , requestDescribeCapacityReservations $
--             mkDescribeCapacityReservations
--
--         , requestModifyClientVpnEndpoint $
--             mkModifyClientVpnEndpoint
--
--         , requestModifyInstanceCapacityReservationAttributes $
--             mkModifyInstanceCapacityReservationAttributes
--
--         , requestDescribeAggregateIdFormat $
--             mkDescribeAggregateIdFormat
--
--         , requestDescribeSnapshots $
--             mkDescribeSnapshots
--
--         , requestAssociateAddress $
--             mkAssociateAddress
--
--         , requestModifyTrafficMirrorFilterRule $
--             mkModifyTrafficMirrorFilterRule
--
--         , requestDescribeNetworkInterfaceAttribute $
--             mkDescribeNetworkInterfaceAttribute
--
--         , requestReplaceIamInstanceProfileAssociation $
--             mkReplaceIamInstanceProfileAssociation
--
--         , requestAssociateClientVpnTargetNetwork $
--             mkAssociateClientVpnTargetNetwork
--
--         , requestReleaseHosts $
--             mkReleaseHosts
--
--         , requestResetNetworkInterfaceAttribute $
--             mkResetNetworkInterfaceAttribute
--
--         , requestDeleteInternetGateway $
--             mkDeleteInternetGateway
--
--         , requestDescribeReservedInstancesListings $
--             mkDescribeReservedInstancesListings
--
--         , requestCreateVpnConnection $
--             mkCreateVpnConnection
--
--         , requestReplaceTransitGatewayRoute $
--             mkReplaceTransitGatewayRoute
--
--         , requestCreateFleet $
--             mkCreateFleet
--
--         , requestDeleteNatGateway $
--             mkDeleteNatGateway
--
--         , requestDescribeImportSnapshotTasks $
--             mkDescribeImportSnapshotTasks
--
--         , requestGetCoipPoolUsage $
--             mkGetCoipPoolUsage
--
--         , requestDescribeCustomerGateways $
--             mkDescribeCustomerGateways
--
--         , requestDeleteSubnet $
--             mkDeleteSubnet
--
--         , requestCopyImage $
--             mkCopyImage
--
--         , requestCreateVpcEndpoint $
--             mkCreateVpcEndpoint
--
--         , requestModifyTrafficMirrorSession $
--             mkModifyTrafficMirrorSession
--
--         , requestDescribeCarrierGateways $
--             mkDescribeCarrierGateways
--
--         , requestDescribeTransitGatewayPeeringAttachments $
--             mkDescribeTransitGatewayPeeringAttachments
--
--         , requestDeleteQueuedReservedInstances $
--             mkDeleteQueuedReservedInstances
--
--         , requestDescribeTransitGatewayMulticastDomains $
--             mkDescribeTransitGatewayMulticastDomains
--
--         , requestGetDefaultCreditSpecification $
--             mkGetDefaultCreditSpecification
--
--         , requestUnmonitorInstances $
--             mkUnmonitorInstances
--
--         , requestDescribeTransitGatewayVpcAttachments $
--             mkDescribeTransitGatewayVpcAttachments
--
--         , requestCreateSecurityGroup $
--             mkCreateSecurityGroup
--
--         , requestGetEbsEncryptionByDefault $
--             mkGetEbsEncryptionByDefault
--
--         , requestImportVolume $
--             mkImportVolume
--
--         , requestDeleteCarrierGateway $
--             mkDeleteCarrierGateway
--
--         , requestDisableVgwRoutePropagation $
--             mkDisableVgwRoutePropagation
--
--         , requestDeleteTrafficMirrorFilter $
--             mkDeleteTrafficMirrorFilter
--
--         , requestModifyVpnTunnelCertificate $
--             mkModifyVpnTunnelCertificate
--
--         , requestCreateSpotDatafeedSubscription $
--             mkCreateSpotDatafeedSubscription
--
--         , requestCancelSpotInstanceRequests $
--             mkCancelSpotInstanceRequests
--
--         , requestCreateRoute $
--             mkCreateRoute
--
--         , requestDescribeVpcEndpointServiceConfigurations $
--             mkDescribeVpcEndpointServiceConfigurations
--
--         , requestDeleteSnapshot $
--             mkDeleteSnapshot
--
--         , requestAssignPrivateIpAddresses $
--             mkAssignPrivateIpAddresses
--
--         , requestAuthorizeClientVpnIngress $
--             mkAuthorizeClientVpnIngress
--
--         , requestDeleteTransitGatewayPeeringAttachment $
--             mkDeleteTransitGatewayPeeringAttachment
--
--         , requestModifyInstanceAttribute $
--             mkModifyInstanceAttribute
--
--         , requestDeleteCustomerGateway $
--             mkDeleteCustomerGateway
--
--         , requestDisassociateIamInstanceProfile $
--             mkDisassociateIamInstanceProfile
--
--         , requestTerminateClientVpnConnections $
--             mkTerminateClientVpnConnections
--
--         , requestDisassociateRouteTable $
--             mkDisassociateRouteTable
--
--         , requestGetConsoleScreenshot $
--             mkGetConsoleScreenshot
--
--         , requestResetEbsDefaultKmsKeyId $
--             mkResetEbsDefaultKmsKeyId
--
--         , requestAssignIpv6Addresses $
--             mkAssignIpv6Addresses
--
--         , requestModifyVpnTunnelOptions $
--             mkModifyVpnTunnelOptions
--
--         , requestModifyEbsDefaultKmsKeyId $
--             mkModifyEbsDefaultKmsKeyId
--
--         , requestDeleteSpotDatafeedSubscription $
--             mkDeleteSpotDatafeedSubscription
--
--         , requestModifyVolume $
--             mkModifyVolume
--
--         , requestEnableVpcClassicLink $
--             mkEnableVpcClassicLink
--
--         , requestDescribePlacementGroups $
--             mkDescribePlacementGroups
--
--         , requestProvisionByoipCidr $
--             mkProvisionByoipCidr
--
--         , requestDisassociateEnclaveCertificateIamRole $
--             mkDisassociateEnclaveCertificateIamRole
--
--         , requestModifyAvailabilityZoneGroup $
--             mkModifyAvailabilityZoneGroup
--
--         , requestDescribeStaleSecurityGroups $
--             mkDescribeStaleSecurityGroups
--
--         , requestCreateCarrierGateway $
--             mkCreateCarrierGateway
--
--         , requestDescribeExportImageTasks $
--             mkDescribeExportImageTasks
--
--         , requestPurchaseScheduledInstances $
--             mkPurchaseScheduledInstances
--
--         , requestEnableVgwRoutePropagation $
--             mkEnableVgwRoutePropagation
--
--         , requestDescribeSpotFleetRequestHistory $
--             mkDescribeSpotFleetRequestHistory
--
--         , requestModifySnapshotAttribute $
--             mkModifySnapshotAttribute
--
--         , requestDescribeIamInstanceProfileAssociations $
--             mkDescribeIamInstanceProfileAssociations
--
--         , requestCreateSnapshot $
--             mkCreateSnapshot
--
--         , requestCreateLocalGatewayRoute $
--             mkCreateLocalGatewayRoute
--
--         , requestCreateNetworkAclEntry $
--             mkCreateNetworkAclEntry
--
--         , requestDescribeTransitGatewayAttachments $
--             mkDescribeTransitGatewayAttachments
--
--         , requestCreateReservedInstancesListing $
--             mkCreateReservedInstancesListing
--
--         , requestDescribeIpv6Pools $
--             mkDescribeIpv6Pools
--
--         , requestAttachVpnGateway $
--             mkAttachVpnGateway
--
--         , requestDescribeLocalGateways $
--             mkDescribeLocalGateways
--
--         , requestModifyVpcEndpointServicePermissions $
--             mkModifyVpcEndpointServicePermissions
--
--         , requestExportClientVpnClientCertificateRevocationList $
--             mkExportClientVpnClientCertificateRevocationList
--
--         , requestCreateDhcpOptions $
--             mkCreateDhcpOptions
--
--         , requestRegisterTransitGatewayMulticastGroupSources $
--             mkRegisterTransitGatewayMulticastGroupSources
--
--         , requestDescribeAccountAttributes $
--             mkDescribeAccountAttributes
--
--         , requestGetTransitGatewayRouteTablePropagations $
--             mkGetTransitGatewayRouteTablePropagations
--
--         , requestModifyFpgaImageAttribute $
--             mkModifyFpgaImageAttribute
--
--         , requestModifyHosts $
--             mkModifyHosts
--
--         , requestRebootInstances $
--             mkRebootInstances
--
--         , requestModifyVpcEndpointServiceConfiguration $
--             mkModifyVpcEndpointServiceConfiguration
--
--         , requestCreateTransitGateway $
--             mkCreateTransitGateway
--
--         , requestUnassignIpv6Addresses $
--             mkUnassignIpv6Addresses
--
--         , requestDeleteTrafficMirrorSession $
--             mkDeleteTrafficMirrorSession
--
--         , requestCreateManagedPrefixList $
--             mkCreateManagedPrefixList
--
--         , requestAssociateIamInstanceProfile $
--             mkAssociateIamInstanceProfile
--
--         , requestModifyDefaultCreditSpecification $
--             mkModifyDefaultCreditSpecification
--
--         , requestDeleteEgressOnlyInternetGateway $
--             mkDeleteEgressOnlyInternetGateway
--
--         , requestPurchaseHostReservation $
--             mkPurchaseHostReservation
--
--         , requestModifyTransitGatewayVpcAttachment $
--             mkModifyTransitGatewayVpcAttachment
--
--         , requestCreateImage $
--             mkCreateImage
--
--         , requestDescribeClassicLinkInstances $
--             mkDescribeClassicLinkInstances
--
--         , requestTerminateInstances $
--             mkTerminateInstances
--
--         , requestGetTransitGatewayPrefixListReferences $
--             mkGetTransitGatewayPrefixListReferences
--
--         , requestDescribeKeyPairs $
--             mkDescribeKeyPairs
--
--         , requestDisableFastSnapshotRestores $
--             mkDisableFastSnapshotRestores
--
--         , requestDescribeLaunchTemplates $
--             mkDescribeLaunchTemplates
--
--         , requestCreateVpnConnectionRoute $
--             mkCreateVpnConnectionRoute
--
--         , requestAssociateRouteTable $
--             mkAssociateRouteTable
--
--         , requestDescribeVpnGateways $
--             mkDescribeVpnGateways
--
--         , requestModifyVpnConnectionOptions $
--             mkModifyVpnConnectionOptions
--
--         , requestGetConsoleOutput $
--             mkGetConsoleOutput
--
--         , requestDescribeHosts $
--             mkDescribeHosts
--
--         , requestDescribeImageAttribute $
--             mkDescribeImageAttribute
--
--         , requestModifyIdFormat $
--             mkModifyIdFormat
--
--         , requestRegisterTransitGatewayMulticastGroupMembers $
--             mkRegisterTransitGatewayMulticastGroupMembers
--
--         , requestDeleteManagedPrefixList $
--             mkDeleteManagedPrefixList
--
--         , requestDeleteRouteTable $
--             mkDeleteRouteTable
--
--         , requestResetImageAttribute $
--             mkResetImageAttribute
--
--         , requestModifyTransitGatewayPrefixListReference $
--             mkModifyTransitGatewayPrefixListReference
--
--         , requestDescribeTransitGatewayRouteTables $
--             mkDescribeTransitGatewayRouteTables
--
--         , requestCreateEgressOnlyInternetGateway $
--             mkCreateEgressOnlyInternetGateway
--
--         , requestDescribeReservedInstancesModifications $
--             mkDescribeReservedInstancesModifications
--
--         , requestDescribeSpotInstanceRequests $
--             mkDescribeSpotInstanceRequests
--
--         , requestRevokeClientVpnIngress $
--             mkRevokeClientVpnIngress
--
--         , requestUnassignPrivateIpAddresses $
--             mkUnassignPrivateIpAddresses
--
--         , requestDescribeNetworkInterfacePermissions $
--             mkDescribeNetworkInterfacePermissions
--
--         , requestEnableFastSnapshotRestores $
--             mkEnableFastSnapshotRestores
--
--         , requestDescribeVpcEndpointServicePermissions $
--             mkDescribeVpcEndpointServicePermissions
--
--         , requestDeleteDhcpOptions $
--             mkDeleteDhcpOptions
--
--         , requestRegisterInstanceEventNotificationAttributes $
--             mkRegisterInstanceEventNotificationAttributes
--
--         , requestDescribeNetworkAcls $
--             mkDescribeNetworkAcls
--
--         , requestCancelImportTask $
--             mkCancelImportTask
--
--         , requestDetachClassicLinkVpc $
--             mkDetachClassicLinkVpc
--
--         , requestDescribeRegions $
--             mkDescribeRegions
--
--         , requestMonitorInstances $
--             mkMonitorInstances
--
--         , requestSearchLocalGatewayRoutes $
--             mkSearchLocalGatewayRoutes
--
--         , requestDeleteClientVpnRoute $
--             mkDeleteClientVpnRoute
--
--         , requestAcceptVpcPeeringConnection $
--             mkAcceptVpcPeeringConnection
--
--         , requestImportSnapshot $
--             mkImportSnapshot
--
--         , requestDescribeVolumeStatus $
--             mkDescribeVolumeStatus
--
--         , requestDescribeRouteTables $
--             mkDescribeRouteTables
--
--         , requestDescribeAvailabilityZones $
--             mkDescribeAvailabilityZones
--
--         , requestModifyVpcAttribute $
--             mkModifyVpcAttribute
--
--         , requestDescribeClientVpnConnections $
--             mkDescribeClientVpnConnections
--
--         , requestDescribeFleetHistory $
--             mkDescribeFleetHistory
--
--         , requestDescribeImages $
--             mkDescribeImages
--
--         , requestDescribeElasticGpus $
--             mkDescribeElasticGpus
--
--         , requestRestoreAddressToClassic $
--             mkRestoreAddressToClassic
--
--         , requestDescribeManagedPrefixLists $
--             mkDescribeManagedPrefixLists
--
--         , requestCreateKeyPair $
--             mkCreateKeyPair
--
--         , requestGetReservedInstancesExchangeQuote $
--             mkGetReservedInstancesExchangeQuote
--
--         , requestDeleteVolume $
--             mkDeleteVolume
--
--         , requestDeprovisionByoipCidr $
--             mkDeprovisionByoipCidr
--
--         , requestDeleteVpcEndpointServiceConfigurations $
--             mkDeleteVpcEndpointServiceConfigurations
--
--         , requestDescribeSpotFleetInstances $
--             mkDescribeSpotFleetInstances
--
--           ]

--     , testGroup "response"
--         [ responseModifyCapacityReservation $
--             mkModifyCapacityReservationResponse
--
--         , responseGetAssociatedIpv6PoolCidrs $
--             mkGetAssociatedIpv6PoolCidrsResponse
--
--         , responseImportInstance $
--             mkImportInstanceResponse
--
--         , responseRevokeSecurityGroupEgress $
--             mkRevokeSecurityGroupEgressResponse
--
--         , responseCreateNetworkInterfacePermission $
--             mkCreateNetworkInterfacePermissionResponse
--
--         , responseSendDiagnosticInterrupt $
--             mkSendDiagnosticInterruptResponse
--
--         , responseDeleteLaunchTemplate $
--             mkDeleteLaunchTemplateResponse
--
--         , responseRejectVpcEndpointConnections $
--             mkRejectVpcEndpointConnectionsResponse
--
--         , responseCreateVpnGateway $
--             mkCreateVpnGatewayResponse
--
--         , responseCreateNetworkAcl $
--             mkCreateNetworkAclResponse
--
--         , responseDeleteKeyPair $
--             mkDeleteKeyPairResponse
--
--         , responseDescribeSecurityGroupReferences $
--             mkDescribeSecurityGroupReferencesResponse
--
--         , responseDeleteFleets $
--             mkDeleteFleetsResponse
--
--         , responseDescribeTags $
--             mkDescribeTagsResponse
--
--         , responseCreateTransitGatewayRouteTable $
--             mkCreateTransitGatewayRouteTableResponse
--
--         , responseModifyInstanceMetadataOptions $
--             mkModifyInstanceMetadataOptionsResponse
--
--         , responseUpdateSecurityGroupRuleDescriptionsIngress $
--             mkUpdateSecurityGroupRuleDescriptionsIngressResponse
--
--         , responseDisassociateSubnetCidrBlock $
--             mkDisassociateSubnetCidrBlockResponse
--
--         , responseDetachNetworkInterface $
--             mkDetachNetworkInterfaceResponse
--
--         , responseDetachInternetGateway $
--             mkDetachInternetGatewayResponse
--
--         , responseDeleteVpcEndpoints $
--             mkDeleteVpcEndpointsResponse
--
--         , responseDescribeClientVpnEndpoints $
--             mkDescribeClientVpnEndpointsResponse
--
--         , responseDeleteFlowLogs $
--             mkDeleteFlowLogsResponse
--
--         , responseDescribeVpcClassicLink $
--             mkDescribeVpcClassicLinkResponse
--
--         , responseGetAssociatedEnclaveCertificateIamRoles $
--             mkGetAssociatedEnclaveCertificateIamRolesResponse
--
--         , responseAssociateTransitGatewayMulticastDomain $
--             mkAssociateTransitGatewayMulticastDomainResponse
--
--         , responseModifySubnetAttribute $
--             mkModifySubnetAttributeResponse
--
--         , responseDetachVolume $
--             mkVolumeAttachment
--
--         , responseDescribeInstanceCreditSpecifications $
--             mkDescribeInstanceCreditSpecificationsResponse
--
--         , responseCancelBundleTask $
--             mkCancelBundleTaskResponse
--
--         , responseDescribeByoipCidrs $
--             mkDescribeByoipCidrsResponse
--
--         , responseAcceptReservedInstancesExchangeQuote $
--             mkAcceptReservedInstancesExchangeQuoteResponse
--
--         , responseReleaseAddress $
--             mkReleaseAddressResponse
--
--         , responseDescribeInstanceTypeOfferings $
--             mkDescribeInstanceTypeOfferingsResponse
--
--         , responseCreateInternetGateway $
--             mkCreateInternetGatewayResponse
--
--         , responseDeleteVpnConnection $
--             mkDeleteVpnConnectionResponse
--
--         , responseDescribeBundleTasks $
--             mkDescribeBundleTasksResponse
--
--         , responseAuthorizeSecurityGroupEgress $
--             mkAuthorizeSecurityGroupEgressResponse
--
--         , responseEnableTransitGatewayRouteTablePropagation $
--             mkEnableTransitGatewayRouteTablePropagationResponse
--
--         , responseDeregisterImage $
--             mkDeregisterImageResponse
--
--         , responseDeleteVpcEndpointConnectionNotifications $
--             mkDeleteVpcEndpointConnectionNotificationsResponse
--
--         , responseDescribeCoipPools $
--             mkDescribeCoipPoolsResponse
--
--         , responseGetTransitGatewayMulticastDomainAssociations $
--             mkGetTransitGatewayMulticastDomainAssociationsResponse
--
--         , responseDeleteLocalGatewayRouteTableVpcAssociation $
--             mkDeleteLocalGatewayRouteTableVpcAssociationResponse
--
--         , responseModifyNetworkInterfaceAttribute $
--             mkModifyNetworkInterfaceAttributeResponse
--
--         , responseModifyVpcTenancy $
--             mkModifyVpcTenancyResponse
--
--         , responseDescribeInstanceTypes $
--             mkDescribeInstanceTypesResponse
--
--         , responseDescribeClientVpnAuthorizationRules $
--             mkDescribeClientVpnAuthorizationRulesResponse
--
--         , responseDeleteTransitGatewayVpcAttachment $
--             mkDeleteTransitGatewayVpcAttachmentResponse
--
--         , responseDeleteTransitGatewayMulticastDomain $
--             mkDeleteTransitGatewayMulticastDomainResponse
--
--         , responseCancelReservedInstancesListing $
--             mkCancelReservedInstancesListingResponse
--
--         , responseAttachClassicLinkVpc $
--             mkAttachClassicLinkVpcResponse
--
--         , responseDisableTransitGatewayRouteTablePropagation $
--             mkDisableTransitGatewayRouteTablePropagationResponse
--
--         , responseDescribeVpcClassicLinkDnsSupport $
--             mkDescribeVpcClassicLinkDnsSupportResponse
--
--         , responseAssociateSubnetCidrBlock $
--             mkAssociateSubnetCidrBlockResponse
--
--         , responseRunScheduledInstances $
--             mkRunScheduledInstancesResponse
--
--         , responseCreateTransitGatewayRoute $
--             mkCreateTransitGatewayRouteResponse
--
--         , responseCreateTransitGatewayPrefixListReference $
--             mkCreateTransitGatewayPrefixListReferenceResponse
--
--         , responseCancelSpotFleetRequests $
--             mkCancelSpotFleetRequestsResponse
--
--         , responseDescribeSpotPriceHistory $
--             mkDescribeSpotPriceHistoryResponse
--
--         , responseDescribeDhcpOptions $
--             mkDescribeDhcpOptionsResponse
--
--         , responseImportImage $
--             mkImportImageResponse
--
--         , responseCreateLocalGatewayRouteTableVpcAssociation $
--             mkCreateLocalGatewayRouteTableVpcAssociationResponse
--
--         , responseCopyFpgaImage $
--             mkCopyFpgaImageResponse
--
--         , responseImportClientVpnClientCertificateRevocationList $
--             mkImportClientVpnClientCertificateRevocationListResponse
--
--         , responseStopInstances $
--             mkStopInstancesResponse
--
--         , responseEnableEbsEncryptionByDefault $
--             mkEnableEbsEncryptionByDefaultResponse
--
--         , responseDeregisterTransitGatewayMulticastGroupSources $
--             mkDeregisterTransitGatewayMulticastGroupSourcesResponse
--
--         , responseModifyLaunchTemplate $
--             mkModifyLaunchTemplateResponse
--
--         , responseModifyVpcEndpointConnectionNotification $
--             mkModifyVpcEndpointConnectionNotificationResponse
--
--         , responseDescribeInternetGateways $
--             mkDescribeInternetGatewaysResponse
--
--         , responseDisableVpcClassicLink $
--             mkDisableVpcClassicLinkResponse
--
--         , responseGetGroupsForCapacityReservation $
--             mkGetGroupsForCapacityReservationResponse
--
--         , responseDeleteLaunchTemplateVersions $
--             mkDeleteLaunchTemplateVersionsResponse
--
--         , responseBundleInstance $
--             mkBundleInstanceResponse
--
--         , responseDescribeNetworkInterfaces $
--             mkDescribeNetworkInterfacesResponse
--
--         , responseReplaceNetworkAclAssociation $
--             mkReplaceNetworkAclAssociationResponse
--
--         , responseDescribeNatGateways $
--             mkDescribeNatGatewaysResponse
--
--         , responseDescribeAddresses $
--             mkDescribeAddressesResponse
--
--         , responseRestoreManagedPrefixListVersion $
--             mkRestoreManagedPrefixListVersionResponse
--
--         , responseDescribeSnapshotAttribute $
--             mkDescribeSnapshotAttributeResponse
--
--         , responseDescribeIdentityIdFormat $
--             mkDescribeIdentityIdFormatResponse
--
--         , responseReplaceRoute $
--             mkReplaceRouteResponse
--
--         , responseDescribeVpcEndpointServices $
--             mkDescribeVpcEndpointServicesResponse
--
--         , responseDeleteLocalGatewayRoute $
--             mkDeleteLocalGatewayRouteResponse
--
--         , responseAuthorizeSecurityGroupIngress $
--             mkAuthorizeSecurityGroupIngressResponse
--
--         , responseCreateVpcPeeringConnection $
--             mkCreateVpcPeeringConnectionResponse
--
--         , responseDescribeSubnets $
--             mkDescribeSubnetsResponse
--
--         , responseGetTransitGatewayAttachmentPropagations $
--             mkGetTransitGatewayAttachmentPropagationsResponse
--
--         , responseCreateTags $
--             mkCreateTagsResponse
--
--         , responsePurchaseReservedInstancesOffering $
--             mkPurchaseReservedInstancesOfferingResponse
--
--         , responseDeleteNetworkAclEntry $
--             mkDeleteNetworkAclEntryResponse
--
--         , responseResetSnapshotAttribute $
--             mkResetSnapshotAttributeResponse
--
--         , responseDescribeVpnConnections $
--             mkDescribeVpnConnectionsResponse
--
--         , responseModifyInstanceEventStartTime $
--             mkModifyInstanceEventStartTimeResponse
--
--         , responseDeleteRoute $
--             mkDeleteRouteResponse
--
--         , responseReplaceNetworkAclEntry $
--             mkReplaceNetworkAclEntryResponse
--
--         , responseDescribeVpcEndpoints $
--             mkDescribeVpcEndpointsResponse
--
--         , responseCreateTrafficMirrorFilter $
--             mkCreateTrafficMirrorFilterResponse
--
--         , responseResetInstanceAttribute $
--             mkResetInstanceAttributeResponse
--
--         , responseModifyIdentityIdFormat $
--             mkModifyIdentityIdFormatResponse
--
--         , responseAttachNetworkInterface $
--             mkAttachNetworkInterfaceResponse
--
--         , responseCreateCapacityReservation $
--             mkCreateCapacityReservationResponse
--
--         , responseDescribeInstanceStatus $
--             mkDescribeInstanceStatusResponse
--
--         , responseImportKeyPair $
--             mkImportKeyPairResponse
--
--         , responseDeleteTags $
--             mkDeleteTagsResponse
--
--         , responseConfirmProductInstance $
--             mkConfirmProductInstanceResponse
--
--         , responseDescribeInstanceAttribute $
--             mkDescribeInstanceAttributeResponse
--
--         , responseDescribeReservedInstancesOfferings $
--             mkDescribeReservedInstancesOfferingsResponse
--
--         , responseCreateCustomerGateway $
--             mkCreateCustomerGatewayResponse
--
--         , responseDescribeFleets $
--             mkDescribeFleetsResponse
--
--         , responseCreateTransitGatewayPeeringAttachment $
--             mkCreateTransitGatewayPeeringAttachmentResponse
--
--         , responseDeleteSecurityGroup $
--             mkDeleteSecurityGroupResponse
--
--         , responseDescribePublicIpv4Pools $
--             mkDescribePublicIpv4PoolsResponse
--
--         , responseDescribeClientVpnTargetNetworks $
--             mkDescribeClientVpnTargetNetworksResponse
--
--         , responseDeleteVpcPeeringConnection $
--             mkDeleteVpcPeeringConnectionResponse
--
--         , responseAttachInternetGateway $
--             mkAttachInternetGatewayResponse
--
--         , responseModifyInstancePlacement $
--             mkModifyInstancePlacementResponse
--
--         , responseDescribeFlowLogs $
--             mkDescribeFlowLogsResponse
--
--         , responseDescribeLocalGatewayVirtualInterfaceGroups $
--             mkDescribeLocalGatewayVirtualInterfaceGroupsResponse
--
--         , responseDescribeLocalGatewayRouteTableVpcAssociations $
--             mkDescribeLocalGatewayRouteTableVpcAssociationsResponse
--
--         , responseDescribeVpcEndpointConnectionNotifications $
--             mkDescribeVpcEndpointConnectionNotificationsResponse
--
--         , responseGetManagedPrefixListEntries $
--             mkGetManagedPrefixListEntriesResponse
--
--         , responseRunInstances $
--             mkReservation
--
--         , responseCreateSnapshots $
--             mkCreateSnapshotsResponse
--
--         , responseAssociateDhcpOptions $
--             mkAssociateDhcpOptionsResponse
--
--         , responseDeleteTrafficMirrorFilterRule $
--             mkDeleteTrafficMirrorFilterRuleResponse
--
--         , responseDescribeReservedInstances $
--             mkDescribeReservedInstancesResponse
--
--         , responseDescribeIdFormat $
--             mkDescribeIdFormatResponse
--
--         , responseDescribeVpcs $
--             mkDescribeVpcsResponse
--
--         , responseDescribeConversionTasks $
--             mkDescribeConversionTasksResponse
--
--         , responseCreateLaunchTemplateVersion $
--             mkCreateLaunchTemplateVersionResponse
--
--         , responseGetManagedPrefixListAssociations $
--             mkGetManagedPrefixListAssociationsResponse
--
--         , responseDisableVpcClassicLinkDnsSupport $
--             mkDisableVpcClassicLinkDnsSupportResponse
--
--         , responseApplySecurityGroupsToClientVpnTargetNetwork $
--             mkApplySecurityGroupsToClientVpnTargetNetworkResponse
--
--         , responseDescribeTrafficMirrorTargets $
--             mkDescribeTrafficMirrorTargetsResponse
--
--         , responseDescribeVolumesModifications $
--             mkDescribeVolumesModificationsResponse
--
--         , responseExportImage $
--             mkExportImageResponse
--
--         , responseCreateFpgaImage $
--             mkCreateFpgaImageResponse
--
--         , responseAcceptVpcEndpointConnections $
--             mkAcceptVpcEndpointConnectionsResponse
--
--         , responseDeleteClientVpnEndpoint $
--             mkDeleteClientVpnEndpointResponse
--
--         , responseSearchTransitGatewayRoutes $
--             mkSearchTransitGatewayRoutesResponse
--
--         , responseGetLaunchTemplateData $
--             mkGetLaunchTemplateDataResponse
--
--         , responseAllocateAddress $
--             mkAllocateAddressResponse
--
--         , responseAcceptTransitGatewayVpcAttachment $
--             mkAcceptTransitGatewayVpcAttachmentResponse
--
--         , responseCancelConversionTask $
--             mkCancelConversionTaskResponse
--
--         , responseModifyImageAttribute $
--             mkModifyImageAttributeResponse
--
--         , responseCreateRouteTable $
--             mkCreateRouteTableResponse
--
--         , responseRejectTransitGatewayPeeringAttachment $
--             mkRejectTransitGatewayPeeringAttachmentResponse
--
--         , responseReportInstanceStatus $
--             mkReportInstanceStatusResponse
--
--         , responseAttachVolume $
--             mkVolumeAttachment
--
--         , responseRequestSpotInstances $
--             mkRequestSpotInstancesResponse
--
--         , responseWithdrawByoipCidr $
--             mkWithdrawByoipCidrResponse
--
--         , responseDescribeHostReservationOfferings $
--             mkDescribeHostReservationOfferingsResponse
--
--         , responseResetFpgaImageAttribute $
--             mkResetFpgaImageAttributeResponse
--
--         , responseModifyVpnConnection $
--             mkModifyVpnConnectionResponse
--
--         , responseCreateTrafficMirrorFilterRule $
--             mkCreateTrafficMirrorFilterRuleResponse
--
--         , responseDeleteTransitGateway $
--             mkDeleteTransitGatewayResponse
--
--         , responseStartVpcEndpointServicePrivateDnsVerification $
--             mkStartVpcEndpointServicePrivateDnsVerificationResponse
--
--         , responseDescribeVolumes $
--             mkDescribeVolumesResponse
--
--         , responseRejectVpcPeeringConnection $
--             mkRejectVpcPeeringConnectionResponse
--
--         , responseDescribeClientVpnRoutes $
--             mkDescribeClientVpnRoutesResponse
--
--         , responseDeleteVpnConnectionRoute $
--             mkDeleteVpnConnectionRouteResponse
--
--         , responseAssociateEnclaveCertificateIamRole $
--             mkAssociateEnclaveCertificateIamRoleResponse
--
--         , responseModifyVpcEndpoint $
--             mkModifyVpcEndpointResponse
--
--         , responseDescribeFpgaImageAttribute $
--             mkDescribeFpgaImageAttributeResponse
--
--         , responseAllocateHosts $
--             mkAllocateHostsResponse
--
--         , responseCreateClientVpnEndpoint $
--             mkCreateClientVpnEndpointResponse
--
--         , responseCreateTrafficMirrorSession $
--             mkCreateTrafficMirrorSessionResponse
--
--         , responseRegisterImage $
--             mkRegisterImageResponse
--
--         , responseAdvertiseByoipCidr $
--             mkAdvertiseByoipCidrResponse
--
--         , responseModifyFleet $
--             mkModifyFleetResponse
--
--         , responseRevokeSecurityGroupIngress $
--             mkRevokeSecurityGroupIngressResponse
--
--         , responseGetEbsDefaultKmsKeyId $
--             mkGetEbsDefaultKmsKeyIdResponse
--
--         , responseDescribeHostReservations $
--             mkDescribeHostReservationsResponse
--
--         , responseUpdateSecurityGroupRuleDescriptionsEgress $
--             mkUpdateSecurityGroupRuleDescriptionsEgressResponse
--
--         , responseEnableVpcClassicLinkDnsSupport $
--             mkEnableVpcClassicLinkDnsSupportResponse
--
--         , responseDescribeVpcEndpointConnections $
--             mkDescribeVpcEndpointConnectionsResponse
--
--         , responseModifyReservedInstances $
--             mkModifyReservedInstancesResponse
--
--         , responseDeleteFpgaImage $
--             mkDeleteFpgaImageResponse
--
--         , responseDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations $
--             mkDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse
--
--         , responseDescribeScheduledInstances $
--             mkDescribeScheduledInstancesResponse
--
--         , responseSearchTransitGatewayMulticastGroups $
--             mkSearchTransitGatewayMulticastGroupsResponse
--
--         , responseCreateFlowLogs $
--             mkCreateFlowLogsResponse
--
--         , responseDescribeSpotFleetRequests $
--             mkDescribeSpotFleetRequestsResponse
--
--         , responseMoveAddressToVpc $
--             mkMoveAddressToVpcResponse
--
--         , responseDescribeFleetInstances $
--             mkDescribeFleetInstancesResponse
--
--         , responseDescribeLaunchTemplateVersions $
--             mkDescribeLaunchTemplateVersionsResponse
--
--         , responseModifyInstanceCreditSpecification $
--             mkModifyInstanceCreditSpecificationResponse
--
--         , responseDescribePrincipalIdFormat $
--             mkDescribePrincipalIdFormatResponse
--
--         , responseDescribeTransitGateways $
--             mkDescribeTransitGatewaysResponse
--
--         , responseDeleteNetworkAcl $
--             mkDeleteNetworkAclResponse
--
--         , responseDisassociateTransitGatewayMulticastDomain $
--             mkDisassociateTransitGatewayMulticastDomainResponse
--
--         , responseDeleteTransitGatewayRouteTable $
--             mkDeleteTransitGatewayRouteTableResponse
--
--         , responseCreateLaunchTemplate $
--             mkCreateLaunchTemplateResponse
--
--         , responseCreateVpcEndpointConnectionNotification $
--             mkCreateVpcEndpointConnectionNotificationResponse
--
--         , responseDeleteNetworkInterfacePermission $
--             mkDeleteNetworkInterfacePermissionResponse
--
--         , responseDeleteVpnGateway $
--             mkDeleteVpnGatewayResponse
--
--         , responseCreateTrafficMirrorTarget $
--             mkCreateTrafficMirrorTargetResponse
--
--         , responseDescribeImportImageTasks $
--             mkDescribeImportImageTasksResponse
--
--         , responseDescribeVolumeAttribute $
--             mkDescribeVolumeAttributeResponse
--
--         , responseDescribeMovingAddresses $
--             mkDescribeMovingAddressesResponse
--
--         , responseExportTransitGatewayRoutes $
--             mkExportTransitGatewayRoutesResponse
--
--         , responseGetPasswordData $
--             mkGetPasswordDataResponse
--
--         , responseCreateVpc $
--             mkCreateVpcResponse
--
--         , responseModifyVpcPeeringConnectionOptions $
--             mkModifyVpcPeeringConnectionOptionsResponse
--
--         , responseDescribeFpgaImages $
--             mkDescribeFpgaImagesResponse
--
--         , responseCopySnapshot $
--             mkCopySnapshotResponse
--
--         , responseAcceptTransitGatewayPeeringAttachment $
--             mkAcceptTransitGatewayPeeringAttachmentResponse
--
--         , responseDisassociateAddress $
--             mkDisassociateAddressResponse
--
--         , responseModifyTrafficMirrorFilterNetworkServices $
--             mkModifyTrafficMirrorFilterNetworkServicesResponse
--
--         , responseDescribeEgressOnlyInternetGateways $
--             mkDescribeEgressOnlyInternetGatewaysResponse
--
--         , responseDeleteVpc $
--             mkDeleteVpcResponse
--
--         , responseCreateInstanceExportTask $
--             mkCreateInstanceExportTaskResponse
--
--         , responseRejectTransitGatewayVpcAttachment $
--             mkRejectTransitGatewayVpcAttachmentResponse
--
--         , responseDescribeTrafficMirrorSessions $
--             mkDescribeTrafficMirrorSessionsResponse
--
--         , responseGetTransitGatewayRouteTableAssociations $
--             mkGetTransitGatewayRouteTableAssociationsResponse
--
--         , responseAssociateVpcCidrBlock $
--             mkAssociateVpcCidrBlockResponse
--
--         , responseDescribeVpcAttribute $
--             mkDescribeVpcAttributeResponse
--
--         , responseCreateVolume $
--             mkVolume
--
--         , responseCreateDefaultSubnet $
--             mkCreateDefaultSubnetResponse
--
--         , responseDescribeScheduledInstanceAvailability $
--             mkDescribeScheduledInstanceAvailabilityResponse
--
--         , responseDisassociateClientVpnTargetNetwork $
--             mkDisassociateClientVpnTargetNetworkResponse
--
--         , responseCreateClientVpnRoute $
--             mkCreateClientVpnRouteResponse
--
--         , responseModifyVolumeAttribute $
--             mkModifyVolumeAttributeResponse
--
--         , responseExportClientVpnClientConfiguration $
--             mkExportClientVpnClientConfigurationResponse
--
--         , responseDeleteTrafficMirrorTarget $
--             mkDeleteTrafficMirrorTargetResponse
--
--         , responseDescribeSpotDatafeedSubscription $
--             mkDescribeSpotDatafeedSubscriptionResponse
--
--         , responseDescribeLocalGatewayRouteTables $
--             mkDescribeLocalGatewayRouteTablesResponse
--
--         , responseDescribePrefixLists $
--             mkDescribePrefixListsResponse
--
--         , responseAssociateTransitGatewayRouteTable $
--             mkAssociateTransitGatewayRouteTableResponse
--
--         , responseDeletePlacementGroup $
--             mkDeletePlacementGroupResponse
--
--         , responseModifyTransitGateway $
--             mkModifyTransitGatewayResponse
--
--         , responseDeleteTransitGatewayPrefixListReference $
--             mkDeleteTransitGatewayPrefixListReferenceResponse
--
--         , responseCreateTransitGatewayMulticastDomain $
--             mkCreateTransitGatewayMulticastDomainResponse
--
--         , responseDeregisterInstanceEventNotificationAttributes $
--             mkDeregisterInstanceEventNotificationAttributesResponse
--
--         , responseRequestSpotFleet $
--             mkRequestSpotFleetResponse
--
--         , responseDeleteTransitGatewayRoute $
--             mkDeleteTransitGatewayRouteResponse
--
--         , responseDisableEbsEncryptionByDefault $
--             mkDisableEbsEncryptionByDefaultResponse
--
--         , responseDeregisterTransitGatewayMulticastGroupMembers $
--             mkDeregisterTransitGatewayMulticastGroupMembersResponse
--
--         , responseCreateSubnet $
--             mkCreateSubnetResponse
--
--         , responseCreateNetworkInterface $
--             mkCreateNetworkInterfaceResponse
--
--         , responseDescribeSecurityGroups $
--             mkDescribeSecurityGroupsResponse
--
--         , responseGetCapacityReservationUsage $
--             mkGetCapacityReservationUsageResponse
--
--         , responseCreateTransitGatewayVpcAttachment $
--             mkCreateTransitGatewayVpcAttachmentResponse
--
--         , responseDescribeExportTasks $
--             mkDescribeExportTasksResponse
--
--         , responseModifySpotFleetRequest $
--             mkModifySpotFleetRequestResponse
--
--         , responseDetachVpnGateway $
--             mkDetachVpnGatewayResponse
--
--         , responseModifyManagedPrefixList $
--             mkModifyManagedPrefixListResponse
--
--         , responseGetHostReservationPurchasePreview $
--             mkGetHostReservationPurchasePreviewResponse
--
--         , responseEnableVolumeIO $
--             mkEnableVolumeIOResponse
--
--         , responseDescribeInstances $
--             mkDescribeInstancesResponse
--
--         , responseCreateNatGateway $
--             mkCreateNatGatewayResponse
--
--         , responseDescribeLocalGatewayVirtualInterfaces $
--             mkDescribeLocalGatewayVirtualInterfacesResponse
--
--         , responseDescribeVpcPeeringConnections $
--             mkDescribeVpcPeeringConnectionsResponse
--
--         , responseCancelExportTask $
--             mkCancelExportTaskResponse
--
--         , responseCreateVpcEndpointServiceConfiguration $
--             mkCreateVpcEndpointServiceConfigurationResponse
--
--         , responseCreateDefaultVpc $
--             mkCreateDefaultVpcResponse
--
--         , responseDisassociateVpcCidrBlock $
--             mkDisassociateVpcCidrBlockResponse
--
--         , responseDescribeTrafficMirrorFilters $
--             mkDescribeTrafficMirrorFiltersResponse
--
--         , responseDescribeFastSnapshotRestores $
--             mkDescribeFastSnapshotRestoresResponse
--
--         , responseCancelCapacityReservation $
--             mkCancelCapacityReservationResponse
--
--         , responseDeleteNetworkInterface $
--             mkDeleteNetworkInterfaceResponse
--
--         , responseDisassociateTransitGatewayRouteTable $
--             mkDisassociateTransitGatewayRouteTableResponse
--
--         , responseReplaceRouteTableAssociation $
--             mkReplaceRouteTableAssociationResponse
--
--         , responseStartInstances $
--             mkStartInstancesResponse
--
--         , responseCreatePlacementGroup $
--             mkCreatePlacementGroupResponse
--
--         , responseDescribeInstanceEventNotificationAttributes $
--             mkDescribeInstanceEventNotificationAttributesResponse
--
--         , responseDescribeCapacityReservations $
--             mkDescribeCapacityReservationsResponse
--
--         , responseModifyClientVpnEndpoint $
--             mkModifyClientVpnEndpointResponse
--
--         , responseModifyInstanceCapacityReservationAttributes $
--             mkModifyInstanceCapacityReservationAttributesResponse
--
--         , responseDescribeAggregateIdFormat $
--             mkDescribeAggregateIdFormatResponse
--
--         , responseDescribeSnapshots $
--             mkDescribeSnapshotsResponse
--
--         , responseAssociateAddress $
--             mkAssociateAddressResponse
--
--         , responseModifyTrafficMirrorFilterRule $
--             mkModifyTrafficMirrorFilterRuleResponse
--
--         , responseDescribeNetworkInterfaceAttribute $
--             mkDescribeNetworkInterfaceAttributeResponse
--
--         , responseReplaceIamInstanceProfileAssociation $
--             mkReplaceIamInstanceProfileAssociationResponse
--
--         , responseAssociateClientVpnTargetNetwork $
--             mkAssociateClientVpnTargetNetworkResponse
--
--         , responseReleaseHosts $
--             mkReleaseHostsResponse
--
--         , responseResetNetworkInterfaceAttribute $
--             mkResetNetworkInterfaceAttributeResponse
--
--         , responseDeleteInternetGateway $
--             mkDeleteInternetGatewayResponse
--
--         , responseDescribeReservedInstancesListings $
--             mkDescribeReservedInstancesListingsResponse
--
--         , responseCreateVpnConnection $
--             mkCreateVpnConnectionResponse
--
--         , responseReplaceTransitGatewayRoute $
--             mkReplaceTransitGatewayRouteResponse
--
--         , responseCreateFleet $
--             mkCreateFleetResponse
--
--         , responseDeleteNatGateway $
--             mkDeleteNatGatewayResponse
--
--         , responseDescribeImportSnapshotTasks $
--             mkDescribeImportSnapshotTasksResponse
--
--         , responseGetCoipPoolUsage $
--             mkGetCoipPoolUsageResponse
--
--         , responseDescribeCustomerGateways $
--             mkDescribeCustomerGatewaysResponse
--
--         , responseDeleteSubnet $
--             mkDeleteSubnetResponse
--
--         , responseCopyImage $
--             mkCopyImageResponse
--
--         , responseCreateVpcEndpoint $
--             mkCreateVpcEndpointResponse
--
--         , responseModifyTrafficMirrorSession $
--             mkModifyTrafficMirrorSessionResponse
--
--         , responseDescribeCarrierGateways $
--             mkDescribeCarrierGatewaysResponse
--
--         , responseDescribeTransitGatewayPeeringAttachments $
--             mkDescribeTransitGatewayPeeringAttachmentsResponse
--
--         , responseDeleteQueuedReservedInstances $
--             mkDeleteQueuedReservedInstancesResponse
--
--         , responseDescribeTransitGatewayMulticastDomains $
--             mkDescribeTransitGatewayMulticastDomainsResponse
--
--         , responseGetDefaultCreditSpecification $
--             mkGetDefaultCreditSpecificationResponse
--
--         , responseUnmonitorInstances $
--             mkUnmonitorInstancesResponse
--
--         , responseDescribeTransitGatewayVpcAttachments $
--             mkDescribeTransitGatewayVpcAttachmentsResponse
--
--         , responseCreateSecurityGroup $
--             mkCreateSecurityGroupResponse
--
--         , responseGetEbsEncryptionByDefault $
--             mkGetEbsEncryptionByDefaultResponse
--
--         , responseImportVolume $
--             mkImportVolumeResponse
--
--         , responseDeleteCarrierGateway $
--             mkDeleteCarrierGatewayResponse
--
--         , responseDisableVgwRoutePropagation $
--             mkDisableVgwRoutePropagationResponse
--
--         , responseDeleteTrafficMirrorFilter $
--             mkDeleteTrafficMirrorFilterResponse
--
--         , responseModifyVpnTunnelCertificate $
--             mkModifyVpnTunnelCertificateResponse
--
--         , responseCreateSpotDatafeedSubscription $
--             mkCreateSpotDatafeedSubscriptionResponse
--
--         , responseCancelSpotInstanceRequests $
--             mkCancelSpotInstanceRequestsResponse
--
--         , responseCreateRoute $
--             mkCreateRouteResponse
--
--         , responseDescribeVpcEndpointServiceConfigurations $
--             mkDescribeVpcEndpointServiceConfigurationsResponse
--
--         , responseDeleteSnapshot $
--             mkDeleteSnapshotResponse
--
--         , responseAssignPrivateIpAddresses $
--             mkAssignPrivateIpAddressesResponse
--
--         , responseAuthorizeClientVpnIngress $
--             mkAuthorizeClientVpnIngressResponse
--
--         , responseDeleteTransitGatewayPeeringAttachment $
--             mkDeleteTransitGatewayPeeringAttachmentResponse
--
--         , responseModifyInstanceAttribute $
--             mkModifyInstanceAttributeResponse
--
--         , responseDeleteCustomerGateway $
--             mkDeleteCustomerGatewayResponse
--
--         , responseDisassociateIamInstanceProfile $
--             mkDisassociateIamInstanceProfileResponse
--
--         , responseTerminateClientVpnConnections $
--             mkTerminateClientVpnConnectionsResponse
--
--         , responseDisassociateRouteTable $
--             mkDisassociateRouteTableResponse
--
--         , responseGetConsoleScreenshot $
--             mkGetConsoleScreenshotResponse
--
--         , responseResetEbsDefaultKmsKeyId $
--             mkResetEbsDefaultKmsKeyIdResponse
--
--         , responseAssignIpv6Addresses $
--             mkAssignIpv6AddressesResponse
--
--         , responseModifyVpnTunnelOptions $
--             mkModifyVpnTunnelOptionsResponse
--
--         , responseModifyEbsDefaultKmsKeyId $
--             mkModifyEbsDefaultKmsKeyIdResponse
--
--         , responseDeleteSpotDatafeedSubscription $
--             mkDeleteSpotDatafeedSubscriptionResponse
--
--         , responseModifyVolume $
--             mkModifyVolumeResponse
--
--         , responseEnableVpcClassicLink $
--             mkEnableVpcClassicLinkResponse
--
--         , responseDescribePlacementGroups $
--             mkDescribePlacementGroupsResponse
--
--         , responseProvisionByoipCidr $
--             mkProvisionByoipCidrResponse
--
--         , responseDisassociateEnclaveCertificateIamRole $
--             mkDisassociateEnclaveCertificateIamRoleResponse
--
--         , responseModifyAvailabilityZoneGroup $
--             mkModifyAvailabilityZoneGroupResponse
--
--         , responseDescribeStaleSecurityGroups $
--             mkDescribeStaleSecurityGroupsResponse
--
--         , responseCreateCarrierGateway $
--             mkCreateCarrierGatewayResponse
--
--         , responseDescribeExportImageTasks $
--             mkDescribeExportImageTasksResponse
--
--         , responsePurchaseScheduledInstances $
--             mkPurchaseScheduledInstancesResponse
--
--         , responseEnableVgwRoutePropagation $
--             mkEnableVgwRoutePropagationResponse
--
--         , responseDescribeSpotFleetRequestHistory $
--             mkDescribeSpotFleetRequestHistoryResponse
--
--         , responseModifySnapshotAttribute $
--             mkModifySnapshotAttributeResponse
--
--         , responseDescribeIamInstanceProfileAssociations $
--             mkDescribeIamInstanceProfileAssociationsResponse
--
--         , responseCreateSnapshot $
--             mkSnapshot
--
--         , responseCreateLocalGatewayRoute $
--             mkCreateLocalGatewayRouteResponse
--
--         , responseCreateNetworkAclEntry $
--             mkCreateNetworkAclEntryResponse
--
--         , responseDescribeTransitGatewayAttachments $
--             mkDescribeTransitGatewayAttachmentsResponse
--
--         , responseCreateReservedInstancesListing $
--             mkCreateReservedInstancesListingResponse
--
--         , responseDescribeIpv6Pools $
--             mkDescribeIpv6PoolsResponse
--
--         , responseAttachVpnGateway $
--             mkAttachVpnGatewayResponse
--
--         , responseDescribeLocalGateways $
--             mkDescribeLocalGatewaysResponse
--
--         , responseModifyVpcEndpointServicePermissions $
--             mkModifyVpcEndpointServicePermissionsResponse
--
--         , responseExportClientVpnClientCertificateRevocationList $
--             mkExportClientVpnClientCertificateRevocationListResponse
--
--         , responseCreateDhcpOptions $
--             mkCreateDhcpOptionsResponse
--
--         , responseRegisterTransitGatewayMulticastGroupSources $
--             mkRegisterTransitGatewayMulticastGroupSourcesResponse
--
--         , responseDescribeAccountAttributes $
--             mkDescribeAccountAttributesResponse
--
--         , responseGetTransitGatewayRouteTablePropagations $
--             mkGetTransitGatewayRouteTablePropagationsResponse
--
--         , responseModifyFpgaImageAttribute $
--             mkModifyFpgaImageAttributeResponse
--
--         , responseModifyHosts $
--             mkModifyHostsResponse
--
--         , responseRebootInstances $
--             mkRebootInstancesResponse
--
--         , responseModifyVpcEndpointServiceConfiguration $
--             mkModifyVpcEndpointServiceConfigurationResponse
--
--         , responseCreateTransitGateway $
--             mkCreateTransitGatewayResponse
--
--         , responseUnassignIpv6Addresses $
--             mkUnassignIpv6AddressesResponse
--
--         , responseDeleteTrafficMirrorSession $
--             mkDeleteTrafficMirrorSessionResponse
--
--         , responseCreateManagedPrefixList $
--             mkCreateManagedPrefixListResponse
--
--         , responseAssociateIamInstanceProfile $
--             mkAssociateIamInstanceProfileResponse
--
--         , responseModifyDefaultCreditSpecification $
--             mkModifyDefaultCreditSpecificationResponse
--
--         , responseDeleteEgressOnlyInternetGateway $
--             mkDeleteEgressOnlyInternetGatewayResponse
--
--         , responsePurchaseHostReservation $
--             mkPurchaseHostReservationResponse
--
--         , responseModifyTransitGatewayVpcAttachment $
--             mkModifyTransitGatewayVpcAttachmentResponse
--
--         , responseCreateImage $
--             mkCreateImageResponse
--
--         , responseDescribeClassicLinkInstances $
--             mkDescribeClassicLinkInstancesResponse
--
--         , responseTerminateInstances $
--             mkTerminateInstancesResponse
--
--         , responseGetTransitGatewayPrefixListReferences $
--             mkGetTransitGatewayPrefixListReferencesResponse
--
--         , responseDescribeKeyPairs $
--             mkDescribeKeyPairsResponse
--
--         , responseDisableFastSnapshotRestores $
--             mkDisableFastSnapshotRestoresResponse
--
--         , responseDescribeLaunchTemplates $
--             mkDescribeLaunchTemplatesResponse
--
--         , responseCreateVpnConnectionRoute $
--             mkCreateVpnConnectionRouteResponse
--
--         , responseAssociateRouteTable $
--             mkAssociateRouteTableResponse
--
--         , responseDescribeVpnGateways $
--             mkDescribeVpnGatewaysResponse
--
--         , responseModifyVpnConnectionOptions $
--             mkModifyVpnConnectionOptionsResponse
--
--         , responseGetConsoleOutput $
--             mkGetConsoleOutputResponse
--
--         , responseDescribeHosts $
--             mkDescribeHostsResponse
--
--         , responseDescribeImageAttribute $
--             mkDescribeImageAttributeResponse
--
--         , responseModifyIdFormat $
--             mkModifyIdFormatResponse
--
--         , responseRegisterTransitGatewayMulticastGroupMembers $
--             mkRegisterTransitGatewayMulticastGroupMembersResponse
--
--         , responseDeleteManagedPrefixList $
--             mkDeleteManagedPrefixListResponse
--
--         , responseDeleteRouteTable $
--             mkDeleteRouteTableResponse
--
--         , responseResetImageAttribute $
--             mkResetImageAttributeResponse
--
--         , responseModifyTransitGatewayPrefixListReference $
--             mkModifyTransitGatewayPrefixListReferenceResponse
--
--         , responseDescribeTransitGatewayRouteTables $
--             mkDescribeTransitGatewayRouteTablesResponse
--
--         , responseCreateEgressOnlyInternetGateway $
--             mkCreateEgressOnlyInternetGatewayResponse
--
--         , responseDescribeReservedInstancesModifications $
--             mkDescribeReservedInstancesModificationsResponse
--
--         , responseDescribeSpotInstanceRequests $
--             mkDescribeSpotInstanceRequestsResponse
--
--         , responseRevokeClientVpnIngress $
--             mkRevokeClientVpnIngressResponse
--
--         , responseUnassignPrivateIpAddresses $
--             mkUnassignPrivateIpAddressesResponse
--
--         , responseDescribeNetworkInterfacePermissions $
--             mkDescribeNetworkInterfacePermissionsResponse
--
--         , responseEnableFastSnapshotRestores $
--             mkEnableFastSnapshotRestoresResponse
--
--         , responseDescribeVpcEndpointServicePermissions $
--             mkDescribeVpcEndpointServicePermissionsResponse
--
--         , responseDeleteDhcpOptions $
--             mkDeleteDhcpOptionsResponse
--
--         , responseRegisterInstanceEventNotificationAttributes $
--             mkRegisterInstanceEventNotificationAttributesResponse
--
--         , responseDescribeNetworkAcls $
--             mkDescribeNetworkAclsResponse
--
--         , responseCancelImportTask $
--             mkCancelImportTaskResponse
--
--         , responseDetachClassicLinkVpc $
--             mkDetachClassicLinkVpcResponse
--
--         , responseDescribeRegions $
--             mkDescribeRegionsResponse
--
--         , responseMonitorInstances $
--             mkMonitorInstancesResponse
--
--         , responseSearchLocalGatewayRoutes $
--             mkSearchLocalGatewayRoutesResponse
--
--         , responseDeleteClientVpnRoute $
--             mkDeleteClientVpnRouteResponse
--
--         , responseAcceptVpcPeeringConnection $
--             mkAcceptVpcPeeringConnectionResponse
--
--         , responseImportSnapshot $
--             mkImportSnapshotResponse
--
--         , responseDescribeVolumeStatus $
--             mkDescribeVolumeStatusResponse
--
--         , responseDescribeRouteTables $
--             mkDescribeRouteTablesResponse
--
--         , responseDescribeAvailabilityZones $
--             mkDescribeAvailabilityZonesResponse
--
--         , responseModifyVpcAttribute $
--             mkModifyVpcAttributeResponse
--
--         , responseDescribeClientVpnConnections $
--             mkDescribeClientVpnConnectionsResponse
--
--         , responseDescribeFleetHistory $
--             mkDescribeFleetHistoryResponse
--
--         , responseDescribeImages $
--             mkDescribeImagesResponse
--
--         , responseDescribeElasticGpus $
--             mkDescribeElasticGpusResponse
--
--         , responseRestoreAddressToClassic $
--             mkRestoreAddressToClassicResponse
--
--         , responseDescribeManagedPrefixLists $
--             mkDescribeManagedPrefixListsResponse
--
--         , responseCreateKeyPair $
--             mkCreateKeyPairResponse
--
--         , responseGetReservedInstancesExchangeQuote $
--             mkGetReservedInstancesExchangeQuoteResponse
--
--         , responseDeleteVolume $
--             mkDeleteVolumeResponse
--
--         , responseDeprovisionByoipCidr $
--             mkDeprovisionByoipCidrResponse
--
--         , responseDeleteVpcEndpointServiceConfigurations $
--             mkDeleteVpcEndpointServiceConfigurationsResponse
--
--         , responseDescribeSpotFleetInstances $
--             mkDescribeSpotFleetInstancesResponse
--
--           ]
--     ]

-- Requests

requestModifyCapacityReservation :: ModifyCapacityReservation -> TestTree
requestModifyCapacityReservation =
  req
    "ModifyCapacityReservation"
    "fixture/ModifyCapacityReservation.yaml"

requestGetAssociatedIpv6PoolCidrs :: GetAssociatedIpv6PoolCidrs -> TestTree
requestGetAssociatedIpv6PoolCidrs =
  req
    "GetAssociatedIpv6PoolCidrs"
    "fixture/GetAssociatedIpv6PoolCidrs.yaml"

requestImportInstance :: ImportInstance -> TestTree
requestImportInstance =
  req
    "ImportInstance"
    "fixture/ImportInstance.yaml"

requestRevokeSecurityGroupEgress :: RevokeSecurityGroupEgress -> TestTree
requestRevokeSecurityGroupEgress =
  req
    "RevokeSecurityGroupEgress"
    "fixture/RevokeSecurityGroupEgress.yaml"

requestCreateNetworkInterfacePermission :: CreateNetworkInterfacePermission -> TestTree
requestCreateNetworkInterfacePermission =
  req
    "CreateNetworkInterfacePermission"
    "fixture/CreateNetworkInterfacePermission.yaml"

requestSendDiagnosticInterrupt :: SendDiagnosticInterrupt -> TestTree
requestSendDiagnosticInterrupt =
  req
    "SendDiagnosticInterrupt"
    "fixture/SendDiagnosticInterrupt.yaml"

requestDeleteLaunchTemplate :: DeleteLaunchTemplate -> TestTree
requestDeleteLaunchTemplate =
  req
    "DeleteLaunchTemplate"
    "fixture/DeleteLaunchTemplate.yaml"

requestRejectVpcEndpointConnections :: RejectVpcEndpointConnections -> TestTree
requestRejectVpcEndpointConnections =
  req
    "RejectVpcEndpointConnections"
    "fixture/RejectVpcEndpointConnections.yaml"

requestCreateVpnGateway :: CreateVpnGateway -> TestTree
requestCreateVpnGateway =
  req
    "CreateVpnGateway"
    "fixture/CreateVpnGateway.yaml"

requestCreateNetworkAcl :: CreateNetworkAcl -> TestTree
requestCreateNetworkAcl =
  req
    "CreateNetworkAcl"
    "fixture/CreateNetworkAcl.yaml"

requestDeleteKeyPair :: DeleteKeyPair -> TestTree
requestDeleteKeyPair =
  req
    "DeleteKeyPair"
    "fixture/DeleteKeyPair.yaml"

requestDescribeSecurityGroupReferences :: DescribeSecurityGroupReferences -> TestTree
requestDescribeSecurityGroupReferences =
  req
    "DescribeSecurityGroupReferences"
    "fixture/DescribeSecurityGroupReferences.yaml"

requestDeleteFleets :: DeleteFleets -> TestTree
requestDeleteFleets =
  req
    "DeleteFleets"
    "fixture/DeleteFleets.yaml"

requestDescribeTags :: DescribeTags -> TestTree
requestDescribeTags =
  req
    "DescribeTags"
    "fixture/DescribeTags.yaml"

requestCreateTransitGatewayRouteTable :: CreateTransitGatewayRouteTable -> TestTree
requestCreateTransitGatewayRouteTable =
  req
    "CreateTransitGatewayRouteTable"
    "fixture/CreateTransitGatewayRouteTable.yaml"

requestModifyInstanceMetadataOptions :: ModifyInstanceMetadataOptions -> TestTree
requestModifyInstanceMetadataOptions =
  req
    "ModifyInstanceMetadataOptions"
    "fixture/ModifyInstanceMetadataOptions.yaml"

requestUpdateSecurityGroupRuleDescriptionsIngress :: UpdateSecurityGroupRuleDescriptionsIngress -> TestTree
requestUpdateSecurityGroupRuleDescriptionsIngress =
  req
    "UpdateSecurityGroupRuleDescriptionsIngress"
    "fixture/UpdateSecurityGroupRuleDescriptionsIngress.yaml"

requestDisassociateSubnetCidrBlock :: DisassociateSubnetCidrBlock -> TestTree
requestDisassociateSubnetCidrBlock =
  req
    "DisassociateSubnetCidrBlock"
    "fixture/DisassociateSubnetCidrBlock.yaml"

requestDetachNetworkInterface :: DetachNetworkInterface -> TestTree
requestDetachNetworkInterface =
  req
    "DetachNetworkInterface"
    "fixture/DetachNetworkInterface.yaml"

requestDetachInternetGateway :: DetachInternetGateway -> TestTree
requestDetachInternetGateway =
  req
    "DetachInternetGateway"
    "fixture/DetachInternetGateway.yaml"

requestDeleteVpcEndpoints :: DeleteVpcEndpoints -> TestTree
requestDeleteVpcEndpoints =
  req
    "DeleteVpcEndpoints"
    "fixture/DeleteVpcEndpoints.yaml"

requestDescribeClientVpnEndpoints :: DescribeClientVpnEndpoints -> TestTree
requestDescribeClientVpnEndpoints =
  req
    "DescribeClientVpnEndpoints"
    "fixture/DescribeClientVpnEndpoints.yaml"

requestDeleteFlowLogs :: DeleteFlowLogs -> TestTree
requestDeleteFlowLogs =
  req
    "DeleteFlowLogs"
    "fixture/DeleteFlowLogs.yaml"

requestDescribeVpcClassicLink :: DescribeVpcClassicLink -> TestTree
requestDescribeVpcClassicLink =
  req
    "DescribeVpcClassicLink"
    "fixture/DescribeVpcClassicLink.yaml"

requestGetAssociatedEnclaveCertificateIamRoles :: GetAssociatedEnclaveCertificateIamRoles -> TestTree
requestGetAssociatedEnclaveCertificateIamRoles =
  req
    "GetAssociatedEnclaveCertificateIamRoles"
    "fixture/GetAssociatedEnclaveCertificateIamRoles.yaml"

requestAssociateTransitGatewayMulticastDomain :: AssociateTransitGatewayMulticastDomain -> TestTree
requestAssociateTransitGatewayMulticastDomain =
  req
    "AssociateTransitGatewayMulticastDomain"
    "fixture/AssociateTransitGatewayMulticastDomain.yaml"

requestModifySubnetAttribute :: ModifySubnetAttribute -> TestTree
requestModifySubnetAttribute =
  req
    "ModifySubnetAttribute"
    "fixture/ModifySubnetAttribute.yaml"

requestDetachVolume :: DetachVolume -> TestTree
requestDetachVolume =
  req
    "DetachVolume"
    "fixture/DetachVolume.yaml"

requestDescribeInstanceCreditSpecifications :: DescribeInstanceCreditSpecifications -> TestTree
requestDescribeInstanceCreditSpecifications =
  req
    "DescribeInstanceCreditSpecifications"
    "fixture/DescribeInstanceCreditSpecifications.yaml"

requestCancelBundleTask :: CancelBundleTask -> TestTree
requestCancelBundleTask =
  req
    "CancelBundleTask"
    "fixture/CancelBundleTask.yaml"

requestDescribeByoipCidrs :: DescribeByoipCidrs -> TestTree
requestDescribeByoipCidrs =
  req
    "DescribeByoipCidrs"
    "fixture/DescribeByoipCidrs.yaml"

requestAcceptReservedInstancesExchangeQuote :: AcceptReservedInstancesExchangeQuote -> TestTree
requestAcceptReservedInstancesExchangeQuote =
  req
    "AcceptReservedInstancesExchangeQuote"
    "fixture/AcceptReservedInstancesExchangeQuote.yaml"

requestReleaseAddress :: ReleaseAddress -> TestTree
requestReleaseAddress =
  req
    "ReleaseAddress"
    "fixture/ReleaseAddress.yaml"

requestDescribeInstanceTypeOfferings :: DescribeInstanceTypeOfferings -> TestTree
requestDescribeInstanceTypeOfferings =
  req
    "DescribeInstanceTypeOfferings"
    "fixture/DescribeInstanceTypeOfferings.yaml"

requestCreateInternetGateway :: CreateInternetGateway -> TestTree
requestCreateInternetGateway =
  req
    "CreateInternetGateway"
    "fixture/CreateInternetGateway.yaml"

requestDeleteVpnConnection :: DeleteVpnConnection -> TestTree
requestDeleteVpnConnection =
  req
    "DeleteVpnConnection"
    "fixture/DeleteVpnConnection.yaml"

requestDescribeBundleTasks :: DescribeBundleTasks -> TestTree
requestDescribeBundleTasks =
  req
    "DescribeBundleTasks"
    "fixture/DescribeBundleTasks.yaml"

requestAuthorizeSecurityGroupEgress :: AuthorizeSecurityGroupEgress -> TestTree
requestAuthorizeSecurityGroupEgress =
  req
    "AuthorizeSecurityGroupEgress"
    "fixture/AuthorizeSecurityGroupEgress.yaml"

requestEnableTransitGatewayRouteTablePropagation :: EnableTransitGatewayRouteTablePropagation -> TestTree
requestEnableTransitGatewayRouteTablePropagation =
  req
    "EnableTransitGatewayRouteTablePropagation"
    "fixture/EnableTransitGatewayRouteTablePropagation.yaml"

requestDeregisterImage :: DeregisterImage -> TestTree
requestDeregisterImage =
  req
    "DeregisterImage"
    "fixture/DeregisterImage.yaml"

requestDeleteVpcEndpointConnectionNotifications :: DeleteVpcEndpointConnectionNotifications -> TestTree
requestDeleteVpcEndpointConnectionNotifications =
  req
    "DeleteVpcEndpointConnectionNotifications"
    "fixture/DeleteVpcEndpointConnectionNotifications.yaml"

requestDescribeCoipPools :: DescribeCoipPools -> TestTree
requestDescribeCoipPools =
  req
    "DescribeCoipPools"
    "fixture/DescribeCoipPools.yaml"

requestGetTransitGatewayMulticastDomainAssociations :: GetTransitGatewayMulticastDomainAssociations -> TestTree
requestGetTransitGatewayMulticastDomainAssociations =
  req
    "GetTransitGatewayMulticastDomainAssociations"
    "fixture/GetTransitGatewayMulticastDomainAssociations.yaml"

requestDeleteLocalGatewayRouteTableVpcAssociation :: DeleteLocalGatewayRouteTableVpcAssociation -> TestTree
requestDeleteLocalGatewayRouteTableVpcAssociation =
  req
    "DeleteLocalGatewayRouteTableVpcAssociation"
    "fixture/DeleteLocalGatewayRouteTableVpcAssociation.yaml"

requestModifyNetworkInterfaceAttribute :: ModifyNetworkInterfaceAttribute -> TestTree
requestModifyNetworkInterfaceAttribute =
  req
    "ModifyNetworkInterfaceAttribute"
    "fixture/ModifyNetworkInterfaceAttribute.yaml"

requestModifyVpcTenancy :: ModifyVpcTenancy -> TestTree
requestModifyVpcTenancy =
  req
    "ModifyVpcTenancy"
    "fixture/ModifyVpcTenancy.yaml"

requestDescribeInstanceTypes :: DescribeInstanceTypes -> TestTree
requestDescribeInstanceTypes =
  req
    "DescribeInstanceTypes"
    "fixture/DescribeInstanceTypes.yaml"

requestDescribeClientVpnAuthorizationRules :: DescribeClientVpnAuthorizationRules -> TestTree
requestDescribeClientVpnAuthorizationRules =
  req
    "DescribeClientVpnAuthorizationRules"
    "fixture/DescribeClientVpnAuthorizationRules.yaml"

requestDeleteTransitGatewayVpcAttachment :: DeleteTransitGatewayVpcAttachment -> TestTree
requestDeleteTransitGatewayVpcAttachment =
  req
    "DeleteTransitGatewayVpcAttachment"
    "fixture/DeleteTransitGatewayVpcAttachment.yaml"

requestDeleteTransitGatewayMulticastDomain :: DeleteTransitGatewayMulticastDomain -> TestTree
requestDeleteTransitGatewayMulticastDomain =
  req
    "DeleteTransitGatewayMulticastDomain"
    "fixture/DeleteTransitGatewayMulticastDomain.yaml"

requestCancelReservedInstancesListing :: CancelReservedInstancesListing -> TestTree
requestCancelReservedInstancesListing =
  req
    "CancelReservedInstancesListing"
    "fixture/CancelReservedInstancesListing.yaml"

requestAttachClassicLinkVpc :: AttachClassicLinkVpc -> TestTree
requestAttachClassicLinkVpc =
  req
    "AttachClassicLinkVpc"
    "fixture/AttachClassicLinkVpc.yaml"

requestDisableTransitGatewayRouteTablePropagation :: DisableTransitGatewayRouteTablePropagation -> TestTree
requestDisableTransitGatewayRouteTablePropagation =
  req
    "DisableTransitGatewayRouteTablePropagation"
    "fixture/DisableTransitGatewayRouteTablePropagation.yaml"

requestDescribeVpcClassicLinkDnsSupport :: DescribeVpcClassicLinkDnsSupport -> TestTree
requestDescribeVpcClassicLinkDnsSupport =
  req
    "DescribeVpcClassicLinkDnsSupport"
    "fixture/DescribeVpcClassicLinkDnsSupport.yaml"

requestAssociateSubnetCidrBlock :: AssociateSubnetCidrBlock -> TestTree
requestAssociateSubnetCidrBlock =
  req
    "AssociateSubnetCidrBlock"
    "fixture/AssociateSubnetCidrBlock.yaml"

requestRunScheduledInstances :: RunScheduledInstances -> TestTree
requestRunScheduledInstances =
  req
    "RunScheduledInstances"
    "fixture/RunScheduledInstances.yaml"

requestCreateTransitGatewayRoute :: CreateTransitGatewayRoute -> TestTree
requestCreateTransitGatewayRoute =
  req
    "CreateTransitGatewayRoute"
    "fixture/CreateTransitGatewayRoute.yaml"

requestCreateTransitGatewayPrefixListReference :: CreateTransitGatewayPrefixListReference -> TestTree
requestCreateTransitGatewayPrefixListReference =
  req
    "CreateTransitGatewayPrefixListReference"
    "fixture/CreateTransitGatewayPrefixListReference.yaml"

requestCancelSpotFleetRequests :: CancelSpotFleetRequests -> TestTree
requestCancelSpotFleetRequests =
  req
    "CancelSpotFleetRequests"
    "fixture/CancelSpotFleetRequests.yaml"

requestDescribeSpotPriceHistory :: DescribeSpotPriceHistory -> TestTree
requestDescribeSpotPriceHistory =
  req
    "DescribeSpotPriceHistory"
    "fixture/DescribeSpotPriceHistory.yaml"

requestDescribeDhcpOptions :: DescribeDhcpOptions -> TestTree
requestDescribeDhcpOptions =
  req
    "DescribeDhcpOptions"
    "fixture/DescribeDhcpOptions.yaml"

requestImportImage :: ImportImage -> TestTree
requestImportImage =
  req
    "ImportImage"
    "fixture/ImportImage.yaml"

requestCreateLocalGatewayRouteTableVpcAssociation :: CreateLocalGatewayRouteTableVpcAssociation -> TestTree
requestCreateLocalGatewayRouteTableVpcAssociation =
  req
    "CreateLocalGatewayRouteTableVpcAssociation"
    "fixture/CreateLocalGatewayRouteTableVpcAssociation.yaml"

requestCopyFpgaImage :: CopyFpgaImage -> TestTree
requestCopyFpgaImage =
  req
    "CopyFpgaImage"
    "fixture/CopyFpgaImage.yaml"

requestImportClientVpnClientCertificateRevocationList :: ImportClientVpnClientCertificateRevocationList -> TestTree
requestImportClientVpnClientCertificateRevocationList =
  req
    "ImportClientVpnClientCertificateRevocationList"
    "fixture/ImportClientVpnClientCertificateRevocationList.yaml"

requestStopInstances :: StopInstances -> TestTree
requestStopInstances =
  req
    "StopInstances"
    "fixture/StopInstances.yaml"

requestEnableEbsEncryptionByDefault :: EnableEbsEncryptionByDefault -> TestTree
requestEnableEbsEncryptionByDefault =
  req
    "EnableEbsEncryptionByDefault"
    "fixture/EnableEbsEncryptionByDefault.yaml"

requestDeregisterTransitGatewayMulticastGroupSources :: DeregisterTransitGatewayMulticastGroupSources -> TestTree
requestDeregisterTransitGatewayMulticastGroupSources =
  req
    "DeregisterTransitGatewayMulticastGroupSources"
    "fixture/DeregisterTransitGatewayMulticastGroupSources.yaml"

requestModifyLaunchTemplate :: ModifyLaunchTemplate -> TestTree
requestModifyLaunchTemplate =
  req
    "ModifyLaunchTemplate"
    "fixture/ModifyLaunchTemplate.yaml"

requestModifyVpcEndpointConnectionNotification :: ModifyVpcEndpointConnectionNotification -> TestTree
requestModifyVpcEndpointConnectionNotification =
  req
    "ModifyVpcEndpointConnectionNotification"
    "fixture/ModifyVpcEndpointConnectionNotification.yaml"

requestDescribeInternetGateways :: DescribeInternetGateways -> TestTree
requestDescribeInternetGateways =
  req
    "DescribeInternetGateways"
    "fixture/DescribeInternetGateways.yaml"

requestDisableVpcClassicLink :: DisableVpcClassicLink -> TestTree
requestDisableVpcClassicLink =
  req
    "DisableVpcClassicLink"
    "fixture/DisableVpcClassicLink.yaml"

requestGetGroupsForCapacityReservation :: GetGroupsForCapacityReservation -> TestTree
requestGetGroupsForCapacityReservation =
  req
    "GetGroupsForCapacityReservation"
    "fixture/GetGroupsForCapacityReservation.yaml"

requestDeleteLaunchTemplateVersions :: DeleteLaunchTemplateVersions -> TestTree
requestDeleteLaunchTemplateVersions =
  req
    "DeleteLaunchTemplateVersions"
    "fixture/DeleteLaunchTemplateVersions.yaml"

requestBundleInstance :: BundleInstance -> TestTree
requestBundleInstance =
  req
    "BundleInstance"
    "fixture/BundleInstance.yaml"

requestDescribeNetworkInterfaces :: DescribeNetworkInterfaces -> TestTree
requestDescribeNetworkInterfaces =
  req
    "DescribeNetworkInterfaces"
    "fixture/DescribeNetworkInterfaces.yaml"

requestReplaceNetworkAclAssociation :: ReplaceNetworkAclAssociation -> TestTree
requestReplaceNetworkAclAssociation =
  req
    "ReplaceNetworkAclAssociation"
    "fixture/ReplaceNetworkAclAssociation.yaml"

requestDescribeNatGateways :: DescribeNatGateways -> TestTree
requestDescribeNatGateways =
  req
    "DescribeNatGateways"
    "fixture/DescribeNatGateways.yaml"

requestDescribeAddresses :: DescribeAddresses -> TestTree
requestDescribeAddresses =
  req
    "DescribeAddresses"
    "fixture/DescribeAddresses.yaml"

requestRestoreManagedPrefixListVersion :: RestoreManagedPrefixListVersion -> TestTree
requestRestoreManagedPrefixListVersion =
  req
    "RestoreManagedPrefixListVersion"
    "fixture/RestoreManagedPrefixListVersion.yaml"

requestDescribeSnapshotAttribute :: DescribeSnapshotAttribute -> TestTree
requestDescribeSnapshotAttribute =
  req
    "DescribeSnapshotAttribute"
    "fixture/DescribeSnapshotAttribute.yaml"

requestDescribeIdentityIdFormat :: DescribeIdentityIdFormat -> TestTree
requestDescribeIdentityIdFormat =
  req
    "DescribeIdentityIdFormat"
    "fixture/DescribeIdentityIdFormat.yaml"

requestReplaceRoute :: ReplaceRoute -> TestTree
requestReplaceRoute =
  req
    "ReplaceRoute"
    "fixture/ReplaceRoute.yaml"

requestDescribeVpcEndpointServices :: DescribeVpcEndpointServices -> TestTree
requestDescribeVpcEndpointServices =
  req
    "DescribeVpcEndpointServices"
    "fixture/DescribeVpcEndpointServices.yaml"

requestDeleteLocalGatewayRoute :: DeleteLocalGatewayRoute -> TestTree
requestDeleteLocalGatewayRoute =
  req
    "DeleteLocalGatewayRoute"
    "fixture/DeleteLocalGatewayRoute.yaml"

requestAuthorizeSecurityGroupIngress :: AuthorizeSecurityGroupIngress -> TestTree
requestAuthorizeSecurityGroupIngress =
  req
    "AuthorizeSecurityGroupIngress"
    "fixture/AuthorizeSecurityGroupIngress.yaml"

requestCreateVpcPeeringConnection :: CreateVpcPeeringConnection -> TestTree
requestCreateVpcPeeringConnection =
  req
    "CreateVpcPeeringConnection"
    "fixture/CreateVpcPeeringConnection.yaml"

requestDescribeSubnets :: DescribeSubnets -> TestTree
requestDescribeSubnets =
  req
    "DescribeSubnets"
    "fixture/DescribeSubnets.yaml"

requestGetTransitGatewayAttachmentPropagations :: GetTransitGatewayAttachmentPropagations -> TestTree
requestGetTransitGatewayAttachmentPropagations =
  req
    "GetTransitGatewayAttachmentPropagations"
    "fixture/GetTransitGatewayAttachmentPropagations.yaml"

requestCreateTags :: CreateTags -> TestTree
requestCreateTags =
  req
    "CreateTags"
    "fixture/CreateTags.yaml"

requestPurchaseReservedInstancesOffering :: PurchaseReservedInstancesOffering -> TestTree
requestPurchaseReservedInstancesOffering =
  req
    "PurchaseReservedInstancesOffering"
    "fixture/PurchaseReservedInstancesOffering.yaml"

requestDeleteNetworkAclEntry :: DeleteNetworkAclEntry -> TestTree
requestDeleteNetworkAclEntry =
  req
    "DeleteNetworkAclEntry"
    "fixture/DeleteNetworkAclEntry.yaml"

requestResetSnapshotAttribute :: ResetSnapshotAttribute -> TestTree
requestResetSnapshotAttribute =
  req
    "ResetSnapshotAttribute"
    "fixture/ResetSnapshotAttribute.yaml"

requestDescribeVpnConnections :: DescribeVpnConnections -> TestTree
requestDescribeVpnConnections =
  req
    "DescribeVpnConnections"
    "fixture/DescribeVpnConnections.yaml"

requestModifyInstanceEventStartTime :: ModifyInstanceEventStartTime -> TestTree
requestModifyInstanceEventStartTime =
  req
    "ModifyInstanceEventStartTime"
    "fixture/ModifyInstanceEventStartTime.yaml"

requestDeleteRoute :: DeleteRoute -> TestTree
requestDeleteRoute =
  req
    "DeleteRoute"
    "fixture/DeleteRoute.yaml"

requestReplaceNetworkAclEntry :: ReplaceNetworkAclEntry -> TestTree
requestReplaceNetworkAclEntry =
  req
    "ReplaceNetworkAclEntry"
    "fixture/ReplaceNetworkAclEntry.yaml"

requestDescribeVpcEndpoints :: DescribeVpcEndpoints -> TestTree
requestDescribeVpcEndpoints =
  req
    "DescribeVpcEndpoints"
    "fixture/DescribeVpcEndpoints.yaml"

requestCreateTrafficMirrorFilter :: CreateTrafficMirrorFilter -> TestTree
requestCreateTrafficMirrorFilter =
  req
    "CreateTrafficMirrorFilter"
    "fixture/CreateTrafficMirrorFilter.yaml"

requestResetInstanceAttribute :: ResetInstanceAttribute -> TestTree
requestResetInstanceAttribute =
  req
    "ResetInstanceAttribute"
    "fixture/ResetInstanceAttribute.yaml"

requestModifyIdentityIdFormat :: ModifyIdentityIdFormat -> TestTree
requestModifyIdentityIdFormat =
  req
    "ModifyIdentityIdFormat"
    "fixture/ModifyIdentityIdFormat.yaml"

requestAttachNetworkInterface :: AttachNetworkInterface -> TestTree
requestAttachNetworkInterface =
  req
    "AttachNetworkInterface"
    "fixture/AttachNetworkInterface.yaml"

requestCreateCapacityReservation :: CreateCapacityReservation -> TestTree
requestCreateCapacityReservation =
  req
    "CreateCapacityReservation"
    "fixture/CreateCapacityReservation.yaml"

requestDescribeInstanceStatus :: DescribeInstanceStatus -> TestTree
requestDescribeInstanceStatus =
  req
    "DescribeInstanceStatus"
    "fixture/DescribeInstanceStatus.yaml"

requestImportKeyPair :: ImportKeyPair -> TestTree
requestImportKeyPair =
  req
    "ImportKeyPair"
    "fixture/ImportKeyPair.yaml"

requestDeleteTags :: DeleteTags -> TestTree
requestDeleteTags =
  req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

requestConfirmProductInstance :: ConfirmProductInstance -> TestTree
requestConfirmProductInstance =
  req
    "ConfirmProductInstance"
    "fixture/ConfirmProductInstance.yaml"

requestDescribeInstanceAttribute :: DescribeInstanceAttribute -> TestTree
requestDescribeInstanceAttribute =
  req
    "DescribeInstanceAttribute"
    "fixture/DescribeInstanceAttribute.yaml"

requestDescribeReservedInstancesOfferings :: DescribeReservedInstancesOfferings -> TestTree
requestDescribeReservedInstancesOfferings =
  req
    "DescribeReservedInstancesOfferings"
    "fixture/DescribeReservedInstancesOfferings.yaml"

requestCreateCustomerGateway :: CreateCustomerGateway -> TestTree
requestCreateCustomerGateway =
  req
    "CreateCustomerGateway"
    "fixture/CreateCustomerGateway.yaml"

requestDescribeFleets :: DescribeFleets -> TestTree
requestDescribeFleets =
  req
    "DescribeFleets"
    "fixture/DescribeFleets.yaml"

requestCreateTransitGatewayPeeringAttachment :: CreateTransitGatewayPeeringAttachment -> TestTree
requestCreateTransitGatewayPeeringAttachment =
  req
    "CreateTransitGatewayPeeringAttachment"
    "fixture/CreateTransitGatewayPeeringAttachment.yaml"

requestDeleteSecurityGroup :: DeleteSecurityGroup -> TestTree
requestDeleteSecurityGroup =
  req
    "DeleteSecurityGroup"
    "fixture/DeleteSecurityGroup.yaml"

requestDescribePublicIpv4Pools :: DescribePublicIpv4Pools -> TestTree
requestDescribePublicIpv4Pools =
  req
    "DescribePublicIpv4Pools"
    "fixture/DescribePublicIpv4Pools.yaml"

requestDescribeClientVpnTargetNetworks :: DescribeClientVpnTargetNetworks -> TestTree
requestDescribeClientVpnTargetNetworks =
  req
    "DescribeClientVpnTargetNetworks"
    "fixture/DescribeClientVpnTargetNetworks.yaml"

requestDeleteVpcPeeringConnection :: DeleteVpcPeeringConnection -> TestTree
requestDeleteVpcPeeringConnection =
  req
    "DeleteVpcPeeringConnection"
    "fixture/DeleteVpcPeeringConnection.yaml"

requestAttachInternetGateway :: AttachInternetGateway -> TestTree
requestAttachInternetGateway =
  req
    "AttachInternetGateway"
    "fixture/AttachInternetGateway.yaml"

requestModifyInstancePlacement :: ModifyInstancePlacement -> TestTree
requestModifyInstancePlacement =
  req
    "ModifyInstancePlacement"
    "fixture/ModifyInstancePlacement.yaml"

requestDescribeFlowLogs :: DescribeFlowLogs -> TestTree
requestDescribeFlowLogs =
  req
    "DescribeFlowLogs"
    "fixture/DescribeFlowLogs.yaml"

requestDescribeLocalGatewayVirtualInterfaceGroups :: DescribeLocalGatewayVirtualInterfaceGroups -> TestTree
requestDescribeLocalGatewayVirtualInterfaceGroups =
  req
    "DescribeLocalGatewayVirtualInterfaceGroups"
    "fixture/DescribeLocalGatewayVirtualInterfaceGroups.yaml"

requestDescribeLocalGatewayRouteTableVpcAssociations :: DescribeLocalGatewayRouteTableVpcAssociations -> TestTree
requestDescribeLocalGatewayRouteTableVpcAssociations =
  req
    "DescribeLocalGatewayRouteTableVpcAssociations"
    "fixture/DescribeLocalGatewayRouteTableVpcAssociations.yaml"

requestDescribeVpcEndpointConnectionNotifications :: DescribeVpcEndpointConnectionNotifications -> TestTree
requestDescribeVpcEndpointConnectionNotifications =
  req
    "DescribeVpcEndpointConnectionNotifications"
    "fixture/DescribeVpcEndpointConnectionNotifications.yaml"

requestGetManagedPrefixListEntries :: GetManagedPrefixListEntries -> TestTree
requestGetManagedPrefixListEntries =
  req
    "GetManagedPrefixListEntries"
    "fixture/GetManagedPrefixListEntries.yaml"

requestRunInstances :: RunInstances -> TestTree
requestRunInstances =
  req
    "RunInstances"
    "fixture/RunInstances.yaml"

requestCreateSnapshots :: CreateSnapshots -> TestTree
requestCreateSnapshots =
  req
    "CreateSnapshots"
    "fixture/CreateSnapshots.yaml"

requestAssociateDhcpOptions :: AssociateDhcpOptions -> TestTree
requestAssociateDhcpOptions =
  req
    "AssociateDhcpOptions"
    "fixture/AssociateDhcpOptions.yaml"

requestDeleteTrafficMirrorFilterRule :: DeleteTrafficMirrorFilterRule -> TestTree
requestDeleteTrafficMirrorFilterRule =
  req
    "DeleteTrafficMirrorFilterRule"
    "fixture/DeleteTrafficMirrorFilterRule.yaml"

requestDescribeReservedInstances :: DescribeReservedInstances -> TestTree
requestDescribeReservedInstances =
  req
    "DescribeReservedInstances"
    "fixture/DescribeReservedInstances.yaml"

requestDescribeIdFormat :: DescribeIdFormat -> TestTree
requestDescribeIdFormat =
  req
    "DescribeIdFormat"
    "fixture/DescribeIdFormat.yaml"

requestDescribeVpcs :: DescribeVpcs -> TestTree
requestDescribeVpcs =
  req
    "DescribeVpcs"
    "fixture/DescribeVpcs.yaml"

requestDescribeConversionTasks :: DescribeConversionTasks -> TestTree
requestDescribeConversionTasks =
  req
    "DescribeConversionTasks"
    "fixture/DescribeConversionTasks.yaml"

requestCreateLaunchTemplateVersion :: CreateLaunchTemplateVersion -> TestTree
requestCreateLaunchTemplateVersion =
  req
    "CreateLaunchTemplateVersion"
    "fixture/CreateLaunchTemplateVersion.yaml"

requestGetManagedPrefixListAssociations :: GetManagedPrefixListAssociations -> TestTree
requestGetManagedPrefixListAssociations =
  req
    "GetManagedPrefixListAssociations"
    "fixture/GetManagedPrefixListAssociations.yaml"

requestDisableVpcClassicLinkDnsSupport :: DisableVpcClassicLinkDnsSupport -> TestTree
requestDisableVpcClassicLinkDnsSupport =
  req
    "DisableVpcClassicLinkDnsSupport"
    "fixture/DisableVpcClassicLinkDnsSupport.yaml"

requestApplySecurityGroupsToClientVpnTargetNetwork :: ApplySecurityGroupsToClientVpnTargetNetwork -> TestTree
requestApplySecurityGroupsToClientVpnTargetNetwork =
  req
    "ApplySecurityGroupsToClientVpnTargetNetwork"
    "fixture/ApplySecurityGroupsToClientVpnTargetNetwork.yaml"

requestDescribeTrafficMirrorTargets :: DescribeTrafficMirrorTargets -> TestTree
requestDescribeTrafficMirrorTargets =
  req
    "DescribeTrafficMirrorTargets"
    "fixture/DescribeTrafficMirrorTargets.yaml"

requestDescribeVolumesModifications :: DescribeVolumesModifications -> TestTree
requestDescribeVolumesModifications =
  req
    "DescribeVolumesModifications"
    "fixture/DescribeVolumesModifications.yaml"

requestExportImage :: ExportImage -> TestTree
requestExportImage =
  req
    "ExportImage"
    "fixture/ExportImage.yaml"

requestCreateFpgaImage :: CreateFpgaImage -> TestTree
requestCreateFpgaImage =
  req
    "CreateFpgaImage"
    "fixture/CreateFpgaImage.yaml"

requestAcceptVpcEndpointConnections :: AcceptVpcEndpointConnections -> TestTree
requestAcceptVpcEndpointConnections =
  req
    "AcceptVpcEndpointConnections"
    "fixture/AcceptVpcEndpointConnections.yaml"

requestDeleteClientVpnEndpoint :: DeleteClientVpnEndpoint -> TestTree
requestDeleteClientVpnEndpoint =
  req
    "DeleteClientVpnEndpoint"
    "fixture/DeleteClientVpnEndpoint.yaml"

requestSearchTransitGatewayRoutes :: SearchTransitGatewayRoutes -> TestTree
requestSearchTransitGatewayRoutes =
  req
    "SearchTransitGatewayRoutes"
    "fixture/SearchTransitGatewayRoutes.yaml"

requestGetLaunchTemplateData :: GetLaunchTemplateData -> TestTree
requestGetLaunchTemplateData =
  req
    "GetLaunchTemplateData"
    "fixture/GetLaunchTemplateData.yaml"

requestAllocateAddress :: AllocateAddress -> TestTree
requestAllocateAddress =
  req
    "AllocateAddress"
    "fixture/AllocateAddress.yaml"

requestAcceptTransitGatewayVpcAttachment :: AcceptTransitGatewayVpcAttachment -> TestTree
requestAcceptTransitGatewayVpcAttachment =
  req
    "AcceptTransitGatewayVpcAttachment"
    "fixture/AcceptTransitGatewayVpcAttachment.yaml"

requestCancelConversionTask :: CancelConversionTask -> TestTree
requestCancelConversionTask =
  req
    "CancelConversionTask"
    "fixture/CancelConversionTask.yaml"

requestModifyImageAttribute :: ModifyImageAttribute -> TestTree
requestModifyImageAttribute =
  req
    "ModifyImageAttribute"
    "fixture/ModifyImageAttribute.yaml"

requestCreateRouteTable :: CreateRouteTable -> TestTree
requestCreateRouteTable =
  req
    "CreateRouteTable"
    "fixture/CreateRouteTable.yaml"

requestRejectTransitGatewayPeeringAttachment :: RejectTransitGatewayPeeringAttachment -> TestTree
requestRejectTransitGatewayPeeringAttachment =
  req
    "RejectTransitGatewayPeeringAttachment"
    "fixture/RejectTransitGatewayPeeringAttachment.yaml"

requestReportInstanceStatus :: ReportInstanceStatus -> TestTree
requestReportInstanceStatus =
  req
    "ReportInstanceStatus"
    "fixture/ReportInstanceStatus.yaml"

requestAttachVolume :: AttachVolume -> TestTree
requestAttachVolume =
  req
    "AttachVolume"
    "fixture/AttachVolume.yaml"

requestRequestSpotInstances :: RequestSpotInstances -> TestTree
requestRequestSpotInstances =
  req
    "RequestSpotInstances"
    "fixture/RequestSpotInstances.yaml"

requestWithdrawByoipCidr :: WithdrawByoipCidr -> TestTree
requestWithdrawByoipCidr =
  req
    "WithdrawByoipCidr"
    "fixture/WithdrawByoipCidr.yaml"

requestDescribeHostReservationOfferings :: DescribeHostReservationOfferings -> TestTree
requestDescribeHostReservationOfferings =
  req
    "DescribeHostReservationOfferings"
    "fixture/DescribeHostReservationOfferings.yaml"

requestResetFpgaImageAttribute :: ResetFpgaImageAttribute -> TestTree
requestResetFpgaImageAttribute =
  req
    "ResetFpgaImageAttribute"
    "fixture/ResetFpgaImageAttribute.yaml"

requestModifyVpnConnection :: ModifyVpnConnection -> TestTree
requestModifyVpnConnection =
  req
    "ModifyVpnConnection"
    "fixture/ModifyVpnConnection.yaml"

requestCreateTrafficMirrorFilterRule :: CreateTrafficMirrorFilterRule -> TestTree
requestCreateTrafficMirrorFilterRule =
  req
    "CreateTrafficMirrorFilterRule"
    "fixture/CreateTrafficMirrorFilterRule.yaml"

requestDeleteTransitGateway :: DeleteTransitGateway -> TestTree
requestDeleteTransitGateway =
  req
    "DeleteTransitGateway"
    "fixture/DeleteTransitGateway.yaml"

requestStartVpcEndpointServicePrivateDnsVerification :: StartVpcEndpointServicePrivateDnsVerification -> TestTree
requestStartVpcEndpointServicePrivateDnsVerification =
  req
    "StartVpcEndpointServicePrivateDnsVerification"
    "fixture/StartVpcEndpointServicePrivateDnsVerification.yaml"

requestDescribeVolumes :: DescribeVolumes -> TestTree
requestDescribeVolumes =
  req
    "DescribeVolumes"
    "fixture/DescribeVolumes.yaml"

requestRejectVpcPeeringConnection :: RejectVpcPeeringConnection -> TestTree
requestRejectVpcPeeringConnection =
  req
    "RejectVpcPeeringConnection"
    "fixture/RejectVpcPeeringConnection.yaml"

requestDescribeClientVpnRoutes :: DescribeClientVpnRoutes -> TestTree
requestDescribeClientVpnRoutes =
  req
    "DescribeClientVpnRoutes"
    "fixture/DescribeClientVpnRoutes.yaml"

requestDeleteVpnConnectionRoute :: DeleteVpnConnectionRoute -> TestTree
requestDeleteVpnConnectionRoute =
  req
    "DeleteVpnConnectionRoute"
    "fixture/DeleteVpnConnectionRoute.yaml"

requestAssociateEnclaveCertificateIamRole :: AssociateEnclaveCertificateIamRole -> TestTree
requestAssociateEnclaveCertificateIamRole =
  req
    "AssociateEnclaveCertificateIamRole"
    "fixture/AssociateEnclaveCertificateIamRole.yaml"

requestModifyVpcEndpoint :: ModifyVpcEndpoint -> TestTree
requestModifyVpcEndpoint =
  req
    "ModifyVpcEndpoint"
    "fixture/ModifyVpcEndpoint.yaml"

requestDescribeFpgaImageAttribute :: DescribeFpgaImageAttribute -> TestTree
requestDescribeFpgaImageAttribute =
  req
    "DescribeFpgaImageAttribute"
    "fixture/DescribeFpgaImageAttribute.yaml"

requestAllocateHosts :: AllocateHosts -> TestTree
requestAllocateHosts =
  req
    "AllocateHosts"
    "fixture/AllocateHosts.yaml"

requestCreateClientVpnEndpoint :: CreateClientVpnEndpoint -> TestTree
requestCreateClientVpnEndpoint =
  req
    "CreateClientVpnEndpoint"
    "fixture/CreateClientVpnEndpoint.yaml"

requestCreateTrafficMirrorSession :: CreateTrafficMirrorSession -> TestTree
requestCreateTrafficMirrorSession =
  req
    "CreateTrafficMirrorSession"
    "fixture/CreateTrafficMirrorSession.yaml"

requestRegisterImage :: RegisterImage -> TestTree
requestRegisterImage =
  req
    "RegisterImage"
    "fixture/RegisterImage.yaml"

requestAdvertiseByoipCidr :: AdvertiseByoipCidr -> TestTree
requestAdvertiseByoipCidr =
  req
    "AdvertiseByoipCidr"
    "fixture/AdvertiseByoipCidr.yaml"

requestModifyFleet :: ModifyFleet -> TestTree
requestModifyFleet =
  req
    "ModifyFleet"
    "fixture/ModifyFleet.yaml"

requestRevokeSecurityGroupIngress :: RevokeSecurityGroupIngress -> TestTree
requestRevokeSecurityGroupIngress =
  req
    "RevokeSecurityGroupIngress"
    "fixture/RevokeSecurityGroupIngress.yaml"

requestGetEbsDefaultKmsKeyId :: GetEbsDefaultKmsKeyId -> TestTree
requestGetEbsDefaultKmsKeyId =
  req
    "GetEbsDefaultKmsKeyId"
    "fixture/GetEbsDefaultKmsKeyId.yaml"

requestDescribeHostReservations :: DescribeHostReservations -> TestTree
requestDescribeHostReservations =
  req
    "DescribeHostReservations"
    "fixture/DescribeHostReservations.yaml"

requestUpdateSecurityGroupRuleDescriptionsEgress :: UpdateSecurityGroupRuleDescriptionsEgress -> TestTree
requestUpdateSecurityGroupRuleDescriptionsEgress =
  req
    "UpdateSecurityGroupRuleDescriptionsEgress"
    "fixture/UpdateSecurityGroupRuleDescriptionsEgress.yaml"

requestEnableVpcClassicLinkDnsSupport :: EnableVpcClassicLinkDnsSupport -> TestTree
requestEnableVpcClassicLinkDnsSupport =
  req
    "EnableVpcClassicLinkDnsSupport"
    "fixture/EnableVpcClassicLinkDnsSupport.yaml"

requestDescribeVpcEndpointConnections :: DescribeVpcEndpointConnections -> TestTree
requestDescribeVpcEndpointConnections =
  req
    "DescribeVpcEndpointConnections"
    "fixture/DescribeVpcEndpointConnections.yaml"

requestModifyReservedInstances :: ModifyReservedInstances -> TestTree
requestModifyReservedInstances =
  req
    "ModifyReservedInstances"
    "fixture/ModifyReservedInstances.yaml"

requestDeleteFpgaImage :: DeleteFpgaImage -> TestTree
requestDeleteFpgaImage =
  req
    "DeleteFpgaImage"
    "fixture/DeleteFpgaImage.yaml"

requestDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations :: DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations -> TestTree
requestDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations =
  req
    "DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations"
    "fixture/DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations.yaml"

requestDescribeScheduledInstances :: DescribeScheduledInstances -> TestTree
requestDescribeScheduledInstances =
  req
    "DescribeScheduledInstances"
    "fixture/DescribeScheduledInstances.yaml"

requestSearchTransitGatewayMulticastGroups :: SearchTransitGatewayMulticastGroups -> TestTree
requestSearchTransitGatewayMulticastGroups =
  req
    "SearchTransitGatewayMulticastGroups"
    "fixture/SearchTransitGatewayMulticastGroups.yaml"

requestCreateFlowLogs :: CreateFlowLogs -> TestTree
requestCreateFlowLogs =
  req
    "CreateFlowLogs"
    "fixture/CreateFlowLogs.yaml"

requestDescribeSpotFleetRequests :: DescribeSpotFleetRequests -> TestTree
requestDescribeSpotFleetRequests =
  req
    "DescribeSpotFleetRequests"
    "fixture/DescribeSpotFleetRequests.yaml"

requestMoveAddressToVpc :: MoveAddressToVpc -> TestTree
requestMoveAddressToVpc =
  req
    "MoveAddressToVpc"
    "fixture/MoveAddressToVpc.yaml"

requestDescribeFleetInstances :: DescribeFleetInstances -> TestTree
requestDescribeFleetInstances =
  req
    "DescribeFleetInstances"
    "fixture/DescribeFleetInstances.yaml"

requestDescribeLaunchTemplateVersions :: DescribeLaunchTemplateVersions -> TestTree
requestDescribeLaunchTemplateVersions =
  req
    "DescribeLaunchTemplateVersions"
    "fixture/DescribeLaunchTemplateVersions.yaml"

requestModifyInstanceCreditSpecification :: ModifyInstanceCreditSpecification -> TestTree
requestModifyInstanceCreditSpecification =
  req
    "ModifyInstanceCreditSpecification"
    "fixture/ModifyInstanceCreditSpecification.yaml"

requestDescribePrincipalIdFormat :: DescribePrincipalIdFormat -> TestTree
requestDescribePrincipalIdFormat =
  req
    "DescribePrincipalIdFormat"
    "fixture/DescribePrincipalIdFormat.yaml"

requestDescribeTransitGateways :: DescribeTransitGateways -> TestTree
requestDescribeTransitGateways =
  req
    "DescribeTransitGateways"
    "fixture/DescribeTransitGateways.yaml"

requestDeleteNetworkAcl :: DeleteNetworkAcl -> TestTree
requestDeleteNetworkAcl =
  req
    "DeleteNetworkAcl"
    "fixture/DeleteNetworkAcl.yaml"

requestDisassociateTransitGatewayMulticastDomain :: DisassociateTransitGatewayMulticastDomain -> TestTree
requestDisassociateTransitGatewayMulticastDomain =
  req
    "DisassociateTransitGatewayMulticastDomain"
    "fixture/DisassociateTransitGatewayMulticastDomain.yaml"

requestDeleteTransitGatewayRouteTable :: DeleteTransitGatewayRouteTable -> TestTree
requestDeleteTransitGatewayRouteTable =
  req
    "DeleteTransitGatewayRouteTable"
    "fixture/DeleteTransitGatewayRouteTable.yaml"

requestCreateLaunchTemplate :: CreateLaunchTemplate -> TestTree
requestCreateLaunchTemplate =
  req
    "CreateLaunchTemplate"
    "fixture/CreateLaunchTemplate.yaml"

requestCreateVpcEndpointConnectionNotification :: CreateVpcEndpointConnectionNotification -> TestTree
requestCreateVpcEndpointConnectionNotification =
  req
    "CreateVpcEndpointConnectionNotification"
    "fixture/CreateVpcEndpointConnectionNotification.yaml"

requestDeleteNetworkInterfacePermission :: DeleteNetworkInterfacePermission -> TestTree
requestDeleteNetworkInterfacePermission =
  req
    "DeleteNetworkInterfacePermission"
    "fixture/DeleteNetworkInterfacePermission.yaml"

requestDeleteVpnGateway :: DeleteVpnGateway -> TestTree
requestDeleteVpnGateway =
  req
    "DeleteVpnGateway"
    "fixture/DeleteVpnGateway.yaml"

requestCreateTrafficMirrorTarget :: CreateTrafficMirrorTarget -> TestTree
requestCreateTrafficMirrorTarget =
  req
    "CreateTrafficMirrorTarget"
    "fixture/CreateTrafficMirrorTarget.yaml"

requestDescribeImportImageTasks :: DescribeImportImageTasks -> TestTree
requestDescribeImportImageTasks =
  req
    "DescribeImportImageTasks"
    "fixture/DescribeImportImageTasks.yaml"

requestDescribeVolumeAttribute :: DescribeVolumeAttribute -> TestTree
requestDescribeVolumeAttribute =
  req
    "DescribeVolumeAttribute"
    "fixture/DescribeVolumeAttribute.yaml"

requestDescribeMovingAddresses :: DescribeMovingAddresses -> TestTree
requestDescribeMovingAddresses =
  req
    "DescribeMovingAddresses"
    "fixture/DescribeMovingAddresses.yaml"

requestExportTransitGatewayRoutes :: ExportTransitGatewayRoutes -> TestTree
requestExportTransitGatewayRoutes =
  req
    "ExportTransitGatewayRoutes"
    "fixture/ExportTransitGatewayRoutes.yaml"

requestGetPasswordData :: GetPasswordData -> TestTree
requestGetPasswordData =
  req
    "GetPasswordData"
    "fixture/GetPasswordData.yaml"

requestCreateVpc :: CreateVpc -> TestTree
requestCreateVpc =
  req
    "CreateVpc"
    "fixture/CreateVpc.yaml"

requestModifyVpcPeeringConnectionOptions :: ModifyVpcPeeringConnectionOptions -> TestTree
requestModifyVpcPeeringConnectionOptions =
  req
    "ModifyVpcPeeringConnectionOptions"
    "fixture/ModifyVpcPeeringConnectionOptions.yaml"

requestDescribeFpgaImages :: DescribeFpgaImages -> TestTree
requestDescribeFpgaImages =
  req
    "DescribeFpgaImages"
    "fixture/DescribeFpgaImages.yaml"

requestCopySnapshot :: CopySnapshot -> TestTree
requestCopySnapshot =
  req
    "CopySnapshot"
    "fixture/CopySnapshot.yaml"

requestAcceptTransitGatewayPeeringAttachment :: AcceptTransitGatewayPeeringAttachment -> TestTree
requestAcceptTransitGatewayPeeringAttachment =
  req
    "AcceptTransitGatewayPeeringAttachment"
    "fixture/AcceptTransitGatewayPeeringAttachment.yaml"

requestDisassociateAddress :: DisassociateAddress -> TestTree
requestDisassociateAddress =
  req
    "DisassociateAddress"
    "fixture/DisassociateAddress.yaml"

requestModifyTrafficMirrorFilterNetworkServices :: ModifyTrafficMirrorFilterNetworkServices -> TestTree
requestModifyTrafficMirrorFilterNetworkServices =
  req
    "ModifyTrafficMirrorFilterNetworkServices"
    "fixture/ModifyTrafficMirrorFilterNetworkServices.yaml"

requestDescribeEgressOnlyInternetGateways :: DescribeEgressOnlyInternetGateways -> TestTree
requestDescribeEgressOnlyInternetGateways =
  req
    "DescribeEgressOnlyInternetGateways"
    "fixture/DescribeEgressOnlyInternetGateways.yaml"

requestDeleteVpc :: DeleteVpc -> TestTree
requestDeleteVpc =
  req
    "DeleteVpc"
    "fixture/DeleteVpc.yaml"

requestCreateInstanceExportTask :: CreateInstanceExportTask -> TestTree
requestCreateInstanceExportTask =
  req
    "CreateInstanceExportTask"
    "fixture/CreateInstanceExportTask.yaml"

requestRejectTransitGatewayVpcAttachment :: RejectTransitGatewayVpcAttachment -> TestTree
requestRejectTransitGatewayVpcAttachment =
  req
    "RejectTransitGatewayVpcAttachment"
    "fixture/RejectTransitGatewayVpcAttachment.yaml"

requestDescribeTrafficMirrorSessions :: DescribeTrafficMirrorSessions -> TestTree
requestDescribeTrafficMirrorSessions =
  req
    "DescribeTrafficMirrorSessions"
    "fixture/DescribeTrafficMirrorSessions.yaml"

requestGetTransitGatewayRouteTableAssociations :: GetTransitGatewayRouteTableAssociations -> TestTree
requestGetTransitGatewayRouteTableAssociations =
  req
    "GetTransitGatewayRouteTableAssociations"
    "fixture/GetTransitGatewayRouteTableAssociations.yaml"

requestAssociateVpcCidrBlock :: AssociateVpcCidrBlock -> TestTree
requestAssociateVpcCidrBlock =
  req
    "AssociateVpcCidrBlock"
    "fixture/AssociateVpcCidrBlock.yaml"

requestDescribeVpcAttribute :: DescribeVpcAttribute -> TestTree
requestDescribeVpcAttribute =
  req
    "DescribeVpcAttribute"
    "fixture/DescribeVpcAttribute.yaml"

requestCreateVolume :: CreateVolume -> TestTree
requestCreateVolume =
  req
    "CreateVolume"
    "fixture/CreateVolume.yaml"

requestCreateDefaultSubnet :: CreateDefaultSubnet -> TestTree
requestCreateDefaultSubnet =
  req
    "CreateDefaultSubnet"
    "fixture/CreateDefaultSubnet.yaml"

requestDescribeScheduledInstanceAvailability :: DescribeScheduledInstanceAvailability -> TestTree
requestDescribeScheduledInstanceAvailability =
  req
    "DescribeScheduledInstanceAvailability"
    "fixture/DescribeScheduledInstanceAvailability.yaml"

requestDisassociateClientVpnTargetNetwork :: DisassociateClientVpnTargetNetwork -> TestTree
requestDisassociateClientVpnTargetNetwork =
  req
    "DisassociateClientVpnTargetNetwork"
    "fixture/DisassociateClientVpnTargetNetwork.yaml"

requestCreateClientVpnRoute :: CreateClientVpnRoute -> TestTree
requestCreateClientVpnRoute =
  req
    "CreateClientVpnRoute"
    "fixture/CreateClientVpnRoute.yaml"

requestModifyVolumeAttribute :: ModifyVolumeAttribute -> TestTree
requestModifyVolumeAttribute =
  req
    "ModifyVolumeAttribute"
    "fixture/ModifyVolumeAttribute.yaml"

requestExportClientVpnClientConfiguration :: ExportClientVpnClientConfiguration -> TestTree
requestExportClientVpnClientConfiguration =
  req
    "ExportClientVpnClientConfiguration"
    "fixture/ExportClientVpnClientConfiguration.yaml"

requestDeleteTrafficMirrorTarget :: DeleteTrafficMirrorTarget -> TestTree
requestDeleteTrafficMirrorTarget =
  req
    "DeleteTrafficMirrorTarget"
    "fixture/DeleteTrafficMirrorTarget.yaml"

requestDescribeSpotDatafeedSubscription :: DescribeSpotDatafeedSubscription -> TestTree
requestDescribeSpotDatafeedSubscription =
  req
    "DescribeSpotDatafeedSubscription"
    "fixture/DescribeSpotDatafeedSubscription.yaml"

requestDescribeLocalGatewayRouteTables :: DescribeLocalGatewayRouteTables -> TestTree
requestDescribeLocalGatewayRouteTables =
  req
    "DescribeLocalGatewayRouteTables"
    "fixture/DescribeLocalGatewayRouteTables.yaml"

requestDescribePrefixLists :: DescribePrefixLists -> TestTree
requestDescribePrefixLists =
  req
    "DescribePrefixLists"
    "fixture/DescribePrefixLists.yaml"

requestAssociateTransitGatewayRouteTable :: AssociateTransitGatewayRouteTable -> TestTree
requestAssociateTransitGatewayRouteTable =
  req
    "AssociateTransitGatewayRouteTable"
    "fixture/AssociateTransitGatewayRouteTable.yaml"

requestDeletePlacementGroup :: DeletePlacementGroup -> TestTree
requestDeletePlacementGroup =
  req
    "DeletePlacementGroup"
    "fixture/DeletePlacementGroup.yaml"

requestModifyTransitGateway :: ModifyTransitGateway -> TestTree
requestModifyTransitGateway =
  req
    "ModifyTransitGateway"
    "fixture/ModifyTransitGateway.yaml"

requestDeleteTransitGatewayPrefixListReference :: DeleteTransitGatewayPrefixListReference -> TestTree
requestDeleteTransitGatewayPrefixListReference =
  req
    "DeleteTransitGatewayPrefixListReference"
    "fixture/DeleteTransitGatewayPrefixListReference.yaml"

requestCreateTransitGatewayMulticastDomain :: CreateTransitGatewayMulticastDomain -> TestTree
requestCreateTransitGatewayMulticastDomain =
  req
    "CreateTransitGatewayMulticastDomain"
    "fixture/CreateTransitGatewayMulticastDomain.yaml"

requestDeregisterInstanceEventNotificationAttributes :: DeregisterInstanceEventNotificationAttributes -> TestTree
requestDeregisterInstanceEventNotificationAttributes =
  req
    "DeregisterInstanceEventNotificationAttributes"
    "fixture/DeregisterInstanceEventNotificationAttributes.yaml"

requestRequestSpotFleet :: RequestSpotFleet -> TestTree
requestRequestSpotFleet =
  req
    "RequestSpotFleet"
    "fixture/RequestSpotFleet.yaml"

requestDeleteTransitGatewayRoute :: DeleteTransitGatewayRoute -> TestTree
requestDeleteTransitGatewayRoute =
  req
    "DeleteTransitGatewayRoute"
    "fixture/DeleteTransitGatewayRoute.yaml"

requestDisableEbsEncryptionByDefault :: DisableEbsEncryptionByDefault -> TestTree
requestDisableEbsEncryptionByDefault =
  req
    "DisableEbsEncryptionByDefault"
    "fixture/DisableEbsEncryptionByDefault.yaml"

requestDeregisterTransitGatewayMulticastGroupMembers :: DeregisterTransitGatewayMulticastGroupMembers -> TestTree
requestDeregisterTransitGatewayMulticastGroupMembers =
  req
    "DeregisterTransitGatewayMulticastGroupMembers"
    "fixture/DeregisterTransitGatewayMulticastGroupMembers.yaml"

requestCreateSubnet :: CreateSubnet -> TestTree
requestCreateSubnet =
  req
    "CreateSubnet"
    "fixture/CreateSubnet.yaml"

requestCreateNetworkInterface :: CreateNetworkInterface -> TestTree
requestCreateNetworkInterface =
  req
    "CreateNetworkInterface"
    "fixture/CreateNetworkInterface.yaml"

requestDescribeSecurityGroups :: DescribeSecurityGroups -> TestTree
requestDescribeSecurityGroups =
  req
    "DescribeSecurityGroups"
    "fixture/DescribeSecurityGroups.yaml"

requestGetCapacityReservationUsage :: GetCapacityReservationUsage -> TestTree
requestGetCapacityReservationUsage =
  req
    "GetCapacityReservationUsage"
    "fixture/GetCapacityReservationUsage.yaml"

requestCreateTransitGatewayVpcAttachment :: CreateTransitGatewayVpcAttachment -> TestTree
requestCreateTransitGatewayVpcAttachment =
  req
    "CreateTransitGatewayVpcAttachment"
    "fixture/CreateTransitGatewayVpcAttachment.yaml"

requestDescribeExportTasks :: DescribeExportTasks -> TestTree
requestDescribeExportTasks =
  req
    "DescribeExportTasks"
    "fixture/DescribeExportTasks.yaml"

requestModifySpotFleetRequest :: ModifySpotFleetRequest -> TestTree
requestModifySpotFleetRequest =
  req
    "ModifySpotFleetRequest"
    "fixture/ModifySpotFleetRequest.yaml"

requestDetachVpnGateway :: DetachVpnGateway -> TestTree
requestDetachVpnGateway =
  req
    "DetachVpnGateway"
    "fixture/DetachVpnGateway.yaml"

requestModifyManagedPrefixList :: ModifyManagedPrefixList -> TestTree
requestModifyManagedPrefixList =
  req
    "ModifyManagedPrefixList"
    "fixture/ModifyManagedPrefixList.yaml"

requestGetHostReservationPurchasePreview :: GetHostReservationPurchasePreview -> TestTree
requestGetHostReservationPurchasePreview =
  req
    "GetHostReservationPurchasePreview"
    "fixture/GetHostReservationPurchasePreview.yaml"

requestEnableVolumeIO :: EnableVolumeIO -> TestTree
requestEnableVolumeIO =
  req
    "EnableVolumeIO"
    "fixture/EnableVolumeIO.yaml"

requestDescribeInstances :: DescribeInstances -> TestTree
requestDescribeInstances =
  req
    "DescribeInstances"
    "fixture/DescribeInstances.yaml"

requestCreateNatGateway :: CreateNatGateway -> TestTree
requestCreateNatGateway =
  req
    "CreateNatGateway"
    "fixture/CreateNatGateway.yaml"

requestDescribeLocalGatewayVirtualInterfaces :: DescribeLocalGatewayVirtualInterfaces -> TestTree
requestDescribeLocalGatewayVirtualInterfaces =
  req
    "DescribeLocalGatewayVirtualInterfaces"
    "fixture/DescribeLocalGatewayVirtualInterfaces.yaml"

requestDescribeVpcPeeringConnections :: DescribeVpcPeeringConnections -> TestTree
requestDescribeVpcPeeringConnections =
  req
    "DescribeVpcPeeringConnections"
    "fixture/DescribeVpcPeeringConnections.yaml"

requestCancelExportTask :: CancelExportTask -> TestTree
requestCancelExportTask =
  req
    "CancelExportTask"
    "fixture/CancelExportTask.yaml"

requestCreateVpcEndpointServiceConfiguration :: CreateVpcEndpointServiceConfiguration -> TestTree
requestCreateVpcEndpointServiceConfiguration =
  req
    "CreateVpcEndpointServiceConfiguration"
    "fixture/CreateVpcEndpointServiceConfiguration.yaml"

requestCreateDefaultVpc :: CreateDefaultVpc -> TestTree
requestCreateDefaultVpc =
  req
    "CreateDefaultVpc"
    "fixture/CreateDefaultVpc.yaml"

requestDisassociateVpcCidrBlock :: DisassociateVpcCidrBlock -> TestTree
requestDisassociateVpcCidrBlock =
  req
    "DisassociateVpcCidrBlock"
    "fixture/DisassociateVpcCidrBlock.yaml"

requestDescribeTrafficMirrorFilters :: DescribeTrafficMirrorFilters -> TestTree
requestDescribeTrafficMirrorFilters =
  req
    "DescribeTrafficMirrorFilters"
    "fixture/DescribeTrafficMirrorFilters.yaml"

requestDescribeFastSnapshotRestores :: DescribeFastSnapshotRestores -> TestTree
requestDescribeFastSnapshotRestores =
  req
    "DescribeFastSnapshotRestores"
    "fixture/DescribeFastSnapshotRestores.yaml"

requestCancelCapacityReservation :: CancelCapacityReservation -> TestTree
requestCancelCapacityReservation =
  req
    "CancelCapacityReservation"
    "fixture/CancelCapacityReservation.yaml"

requestDeleteNetworkInterface :: DeleteNetworkInterface -> TestTree
requestDeleteNetworkInterface =
  req
    "DeleteNetworkInterface"
    "fixture/DeleteNetworkInterface.yaml"

requestDisassociateTransitGatewayRouteTable :: DisassociateTransitGatewayRouteTable -> TestTree
requestDisassociateTransitGatewayRouteTable =
  req
    "DisassociateTransitGatewayRouteTable"
    "fixture/DisassociateTransitGatewayRouteTable.yaml"

requestReplaceRouteTableAssociation :: ReplaceRouteTableAssociation -> TestTree
requestReplaceRouteTableAssociation =
  req
    "ReplaceRouteTableAssociation"
    "fixture/ReplaceRouteTableAssociation.yaml"

requestStartInstances :: StartInstances -> TestTree
requestStartInstances =
  req
    "StartInstances"
    "fixture/StartInstances.yaml"

requestCreatePlacementGroup :: CreatePlacementGroup -> TestTree
requestCreatePlacementGroup =
  req
    "CreatePlacementGroup"
    "fixture/CreatePlacementGroup.yaml"

requestDescribeInstanceEventNotificationAttributes :: DescribeInstanceEventNotificationAttributes -> TestTree
requestDescribeInstanceEventNotificationAttributes =
  req
    "DescribeInstanceEventNotificationAttributes"
    "fixture/DescribeInstanceEventNotificationAttributes.yaml"

requestDescribeCapacityReservations :: DescribeCapacityReservations -> TestTree
requestDescribeCapacityReservations =
  req
    "DescribeCapacityReservations"
    "fixture/DescribeCapacityReservations.yaml"

requestModifyClientVpnEndpoint :: ModifyClientVpnEndpoint -> TestTree
requestModifyClientVpnEndpoint =
  req
    "ModifyClientVpnEndpoint"
    "fixture/ModifyClientVpnEndpoint.yaml"

requestModifyInstanceCapacityReservationAttributes :: ModifyInstanceCapacityReservationAttributes -> TestTree
requestModifyInstanceCapacityReservationAttributes =
  req
    "ModifyInstanceCapacityReservationAttributes"
    "fixture/ModifyInstanceCapacityReservationAttributes.yaml"

requestDescribeAggregateIdFormat :: DescribeAggregateIdFormat -> TestTree
requestDescribeAggregateIdFormat =
  req
    "DescribeAggregateIdFormat"
    "fixture/DescribeAggregateIdFormat.yaml"

requestDescribeSnapshots :: DescribeSnapshots -> TestTree
requestDescribeSnapshots =
  req
    "DescribeSnapshots"
    "fixture/DescribeSnapshots.yaml"

requestAssociateAddress :: AssociateAddress -> TestTree
requestAssociateAddress =
  req
    "AssociateAddress"
    "fixture/AssociateAddress.yaml"

requestModifyTrafficMirrorFilterRule :: ModifyTrafficMirrorFilterRule -> TestTree
requestModifyTrafficMirrorFilterRule =
  req
    "ModifyTrafficMirrorFilterRule"
    "fixture/ModifyTrafficMirrorFilterRule.yaml"

requestDescribeNetworkInterfaceAttribute :: DescribeNetworkInterfaceAttribute -> TestTree
requestDescribeNetworkInterfaceAttribute =
  req
    "DescribeNetworkInterfaceAttribute"
    "fixture/DescribeNetworkInterfaceAttribute.yaml"

requestReplaceIamInstanceProfileAssociation :: ReplaceIamInstanceProfileAssociation -> TestTree
requestReplaceIamInstanceProfileAssociation =
  req
    "ReplaceIamInstanceProfileAssociation"
    "fixture/ReplaceIamInstanceProfileAssociation.yaml"

requestAssociateClientVpnTargetNetwork :: AssociateClientVpnTargetNetwork -> TestTree
requestAssociateClientVpnTargetNetwork =
  req
    "AssociateClientVpnTargetNetwork"
    "fixture/AssociateClientVpnTargetNetwork.yaml"

requestReleaseHosts :: ReleaseHosts -> TestTree
requestReleaseHosts =
  req
    "ReleaseHosts"
    "fixture/ReleaseHosts.yaml"

requestResetNetworkInterfaceAttribute :: ResetNetworkInterfaceAttribute -> TestTree
requestResetNetworkInterfaceAttribute =
  req
    "ResetNetworkInterfaceAttribute"
    "fixture/ResetNetworkInterfaceAttribute.yaml"

requestDeleteInternetGateway :: DeleteInternetGateway -> TestTree
requestDeleteInternetGateway =
  req
    "DeleteInternetGateway"
    "fixture/DeleteInternetGateway.yaml"

requestDescribeReservedInstancesListings :: DescribeReservedInstancesListings -> TestTree
requestDescribeReservedInstancesListings =
  req
    "DescribeReservedInstancesListings"
    "fixture/DescribeReservedInstancesListings.yaml"

requestCreateVpnConnection :: CreateVpnConnection -> TestTree
requestCreateVpnConnection =
  req
    "CreateVpnConnection"
    "fixture/CreateVpnConnection.yaml"

requestReplaceTransitGatewayRoute :: ReplaceTransitGatewayRoute -> TestTree
requestReplaceTransitGatewayRoute =
  req
    "ReplaceTransitGatewayRoute"
    "fixture/ReplaceTransitGatewayRoute.yaml"

requestCreateFleet :: CreateFleet -> TestTree
requestCreateFleet =
  req
    "CreateFleet"
    "fixture/CreateFleet.yaml"

requestDeleteNatGateway :: DeleteNatGateway -> TestTree
requestDeleteNatGateway =
  req
    "DeleteNatGateway"
    "fixture/DeleteNatGateway.yaml"

requestDescribeImportSnapshotTasks :: DescribeImportSnapshotTasks -> TestTree
requestDescribeImportSnapshotTasks =
  req
    "DescribeImportSnapshotTasks"
    "fixture/DescribeImportSnapshotTasks.yaml"

requestGetCoipPoolUsage :: GetCoipPoolUsage -> TestTree
requestGetCoipPoolUsage =
  req
    "GetCoipPoolUsage"
    "fixture/GetCoipPoolUsage.yaml"

requestDescribeCustomerGateways :: DescribeCustomerGateways -> TestTree
requestDescribeCustomerGateways =
  req
    "DescribeCustomerGateways"
    "fixture/DescribeCustomerGateways.yaml"

requestDeleteSubnet :: DeleteSubnet -> TestTree
requestDeleteSubnet =
  req
    "DeleteSubnet"
    "fixture/DeleteSubnet.yaml"

requestCopyImage :: CopyImage -> TestTree
requestCopyImage =
  req
    "CopyImage"
    "fixture/CopyImage.yaml"

requestCreateVpcEndpoint :: CreateVpcEndpoint -> TestTree
requestCreateVpcEndpoint =
  req
    "CreateVpcEndpoint"
    "fixture/CreateVpcEndpoint.yaml"

requestModifyTrafficMirrorSession :: ModifyTrafficMirrorSession -> TestTree
requestModifyTrafficMirrorSession =
  req
    "ModifyTrafficMirrorSession"
    "fixture/ModifyTrafficMirrorSession.yaml"

requestDescribeCarrierGateways :: DescribeCarrierGateways -> TestTree
requestDescribeCarrierGateways =
  req
    "DescribeCarrierGateways"
    "fixture/DescribeCarrierGateways.yaml"

requestDescribeTransitGatewayPeeringAttachments :: DescribeTransitGatewayPeeringAttachments -> TestTree
requestDescribeTransitGatewayPeeringAttachments =
  req
    "DescribeTransitGatewayPeeringAttachments"
    "fixture/DescribeTransitGatewayPeeringAttachments.yaml"

requestDeleteQueuedReservedInstances :: DeleteQueuedReservedInstances -> TestTree
requestDeleteQueuedReservedInstances =
  req
    "DeleteQueuedReservedInstances"
    "fixture/DeleteQueuedReservedInstances.yaml"

requestDescribeTransitGatewayMulticastDomains :: DescribeTransitGatewayMulticastDomains -> TestTree
requestDescribeTransitGatewayMulticastDomains =
  req
    "DescribeTransitGatewayMulticastDomains"
    "fixture/DescribeTransitGatewayMulticastDomains.yaml"

requestGetDefaultCreditSpecification :: GetDefaultCreditSpecification -> TestTree
requestGetDefaultCreditSpecification =
  req
    "GetDefaultCreditSpecification"
    "fixture/GetDefaultCreditSpecification.yaml"

requestUnmonitorInstances :: UnmonitorInstances -> TestTree
requestUnmonitorInstances =
  req
    "UnmonitorInstances"
    "fixture/UnmonitorInstances.yaml"

requestDescribeTransitGatewayVpcAttachments :: DescribeTransitGatewayVpcAttachments -> TestTree
requestDescribeTransitGatewayVpcAttachments =
  req
    "DescribeTransitGatewayVpcAttachments"
    "fixture/DescribeTransitGatewayVpcAttachments.yaml"

requestCreateSecurityGroup :: CreateSecurityGroup -> TestTree
requestCreateSecurityGroup =
  req
    "CreateSecurityGroup"
    "fixture/CreateSecurityGroup.yaml"

requestGetEbsEncryptionByDefault :: GetEbsEncryptionByDefault -> TestTree
requestGetEbsEncryptionByDefault =
  req
    "GetEbsEncryptionByDefault"
    "fixture/GetEbsEncryptionByDefault.yaml"

requestImportVolume :: ImportVolume -> TestTree
requestImportVolume =
  req
    "ImportVolume"
    "fixture/ImportVolume.yaml"

requestDeleteCarrierGateway :: DeleteCarrierGateway -> TestTree
requestDeleteCarrierGateway =
  req
    "DeleteCarrierGateway"
    "fixture/DeleteCarrierGateway.yaml"

requestDisableVgwRoutePropagation :: DisableVgwRoutePropagation -> TestTree
requestDisableVgwRoutePropagation =
  req
    "DisableVgwRoutePropagation"
    "fixture/DisableVgwRoutePropagation.yaml"

requestDeleteTrafficMirrorFilter :: DeleteTrafficMirrorFilter -> TestTree
requestDeleteTrafficMirrorFilter =
  req
    "DeleteTrafficMirrorFilter"
    "fixture/DeleteTrafficMirrorFilter.yaml"

requestModifyVpnTunnelCertificate :: ModifyVpnTunnelCertificate -> TestTree
requestModifyVpnTunnelCertificate =
  req
    "ModifyVpnTunnelCertificate"
    "fixture/ModifyVpnTunnelCertificate.yaml"

requestCreateSpotDatafeedSubscription :: CreateSpotDatafeedSubscription -> TestTree
requestCreateSpotDatafeedSubscription =
  req
    "CreateSpotDatafeedSubscription"
    "fixture/CreateSpotDatafeedSubscription.yaml"

requestCancelSpotInstanceRequests :: CancelSpotInstanceRequests -> TestTree
requestCancelSpotInstanceRequests =
  req
    "CancelSpotInstanceRequests"
    "fixture/CancelSpotInstanceRequests.yaml"

requestCreateRoute :: CreateRoute -> TestTree
requestCreateRoute =
  req
    "CreateRoute"
    "fixture/CreateRoute.yaml"

requestDescribeVpcEndpointServiceConfigurations :: DescribeVpcEndpointServiceConfigurations -> TestTree
requestDescribeVpcEndpointServiceConfigurations =
  req
    "DescribeVpcEndpointServiceConfigurations"
    "fixture/DescribeVpcEndpointServiceConfigurations.yaml"

requestDeleteSnapshot :: DeleteSnapshot -> TestTree
requestDeleteSnapshot =
  req
    "DeleteSnapshot"
    "fixture/DeleteSnapshot.yaml"

requestAssignPrivateIpAddresses :: AssignPrivateIpAddresses -> TestTree
requestAssignPrivateIpAddresses =
  req
    "AssignPrivateIpAddresses"
    "fixture/AssignPrivateIpAddresses.yaml"

requestAuthorizeClientVpnIngress :: AuthorizeClientVpnIngress -> TestTree
requestAuthorizeClientVpnIngress =
  req
    "AuthorizeClientVpnIngress"
    "fixture/AuthorizeClientVpnIngress.yaml"

requestDeleteTransitGatewayPeeringAttachment :: DeleteTransitGatewayPeeringAttachment -> TestTree
requestDeleteTransitGatewayPeeringAttachment =
  req
    "DeleteTransitGatewayPeeringAttachment"
    "fixture/DeleteTransitGatewayPeeringAttachment.yaml"

requestModifyInstanceAttribute :: ModifyInstanceAttribute -> TestTree
requestModifyInstanceAttribute =
  req
    "ModifyInstanceAttribute"
    "fixture/ModifyInstanceAttribute.yaml"

requestDeleteCustomerGateway :: DeleteCustomerGateway -> TestTree
requestDeleteCustomerGateway =
  req
    "DeleteCustomerGateway"
    "fixture/DeleteCustomerGateway.yaml"

requestDisassociateIamInstanceProfile :: DisassociateIamInstanceProfile -> TestTree
requestDisassociateIamInstanceProfile =
  req
    "DisassociateIamInstanceProfile"
    "fixture/DisassociateIamInstanceProfile.yaml"

requestTerminateClientVpnConnections :: TerminateClientVpnConnections -> TestTree
requestTerminateClientVpnConnections =
  req
    "TerminateClientVpnConnections"
    "fixture/TerminateClientVpnConnections.yaml"

requestDisassociateRouteTable :: DisassociateRouteTable -> TestTree
requestDisassociateRouteTable =
  req
    "DisassociateRouteTable"
    "fixture/DisassociateRouteTable.yaml"

requestGetConsoleScreenshot :: GetConsoleScreenshot -> TestTree
requestGetConsoleScreenshot =
  req
    "GetConsoleScreenshot"
    "fixture/GetConsoleScreenshot.yaml"

requestResetEbsDefaultKmsKeyId :: ResetEbsDefaultKmsKeyId -> TestTree
requestResetEbsDefaultKmsKeyId =
  req
    "ResetEbsDefaultKmsKeyId"
    "fixture/ResetEbsDefaultKmsKeyId.yaml"

requestAssignIpv6Addresses :: AssignIpv6Addresses -> TestTree
requestAssignIpv6Addresses =
  req
    "AssignIpv6Addresses"
    "fixture/AssignIpv6Addresses.yaml"

requestModifyVpnTunnelOptions :: ModifyVpnTunnelOptions -> TestTree
requestModifyVpnTunnelOptions =
  req
    "ModifyVpnTunnelOptions"
    "fixture/ModifyVpnTunnelOptions.yaml"

requestModifyEbsDefaultKmsKeyId :: ModifyEbsDefaultKmsKeyId -> TestTree
requestModifyEbsDefaultKmsKeyId =
  req
    "ModifyEbsDefaultKmsKeyId"
    "fixture/ModifyEbsDefaultKmsKeyId.yaml"

requestDeleteSpotDatafeedSubscription :: DeleteSpotDatafeedSubscription -> TestTree
requestDeleteSpotDatafeedSubscription =
  req
    "DeleteSpotDatafeedSubscription"
    "fixture/DeleteSpotDatafeedSubscription.yaml"

requestModifyVolume :: ModifyVolume -> TestTree
requestModifyVolume =
  req
    "ModifyVolume"
    "fixture/ModifyVolume.yaml"

requestEnableVpcClassicLink :: EnableVpcClassicLink -> TestTree
requestEnableVpcClassicLink =
  req
    "EnableVpcClassicLink"
    "fixture/EnableVpcClassicLink.yaml"

requestDescribePlacementGroups :: DescribePlacementGroups -> TestTree
requestDescribePlacementGroups =
  req
    "DescribePlacementGroups"
    "fixture/DescribePlacementGroups.yaml"

requestProvisionByoipCidr :: ProvisionByoipCidr -> TestTree
requestProvisionByoipCidr =
  req
    "ProvisionByoipCidr"
    "fixture/ProvisionByoipCidr.yaml"

requestDisassociateEnclaveCertificateIamRole :: DisassociateEnclaveCertificateIamRole -> TestTree
requestDisassociateEnclaveCertificateIamRole =
  req
    "DisassociateEnclaveCertificateIamRole"
    "fixture/DisassociateEnclaveCertificateIamRole.yaml"

requestModifyAvailabilityZoneGroup :: ModifyAvailabilityZoneGroup -> TestTree
requestModifyAvailabilityZoneGroup =
  req
    "ModifyAvailabilityZoneGroup"
    "fixture/ModifyAvailabilityZoneGroup.yaml"

requestDescribeStaleSecurityGroups :: DescribeStaleSecurityGroups -> TestTree
requestDescribeStaleSecurityGroups =
  req
    "DescribeStaleSecurityGroups"
    "fixture/DescribeStaleSecurityGroups.yaml"

requestCreateCarrierGateway :: CreateCarrierGateway -> TestTree
requestCreateCarrierGateway =
  req
    "CreateCarrierGateway"
    "fixture/CreateCarrierGateway.yaml"

requestDescribeExportImageTasks :: DescribeExportImageTasks -> TestTree
requestDescribeExportImageTasks =
  req
    "DescribeExportImageTasks"
    "fixture/DescribeExportImageTasks.yaml"

requestPurchaseScheduledInstances :: PurchaseScheduledInstances -> TestTree
requestPurchaseScheduledInstances =
  req
    "PurchaseScheduledInstances"
    "fixture/PurchaseScheduledInstances.yaml"

requestEnableVgwRoutePropagation :: EnableVgwRoutePropagation -> TestTree
requestEnableVgwRoutePropagation =
  req
    "EnableVgwRoutePropagation"
    "fixture/EnableVgwRoutePropagation.yaml"

requestDescribeSpotFleetRequestHistory :: DescribeSpotFleetRequestHistory -> TestTree
requestDescribeSpotFleetRequestHistory =
  req
    "DescribeSpotFleetRequestHistory"
    "fixture/DescribeSpotFleetRequestHistory.yaml"

requestModifySnapshotAttribute :: ModifySnapshotAttribute -> TestTree
requestModifySnapshotAttribute =
  req
    "ModifySnapshotAttribute"
    "fixture/ModifySnapshotAttribute.yaml"

requestDescribeIamInstanceProfileAssociations :: DescribeIamInstanceProfileAssociations -> TestTree
requestDescribeIamInstanceProfileAssociations =
  req
    "DescribeIamInstanceProfileAssociations"
    "fixture/DescribeIamInstanceProfileAssociations.yaml"

requestCreateSnapshot :: CreateSnapshot -> TestTree
requestCreateSnapshot =
  req
    "CreateSnapshot"
    "fixture/CreateSnapshot.yaml"

requestCreateLocalGatewayRoute :: CreateLocalGatewayRoute -> TestTree
requestCreateLocalGatewayRoute =
  req
    "CreateLocalGatewayRoute"
    "fixture/CreateLocalGatewayRoute.yaml"

requestCreateNetworkAclEntry :: CreateNetworkAclEntry -> TestTree
requestCreateNetworkAclEntry =
  req
    "CreateNetworkAclEntry"
    "fixture/CreateNetworkAclEntry.yaml"

requestDescribeTransitGatewayAttachments :: DescribeTransitGatewayAttachments -> TestTree
requestDescribeTransitGatewayAttachments =
  req
    "DescribeTransitGatewayAttachments"
    "fixture/DescribeTransitGatewayAttachments.yaml"

requestCreateReservedInstancesListing :: CreateReservedInstancesListing -> TestTree
requestCreateReservedInstancesListing =
  req
    "CreateReservedInstancesListing"
    "fixture/CreateReservedInstancesListing.yaml"

requestDescribeIpv6Pools :: DescribeIpv6Pools -> TestTree
requestDescribeIpv6Pools =
  req
    "DescribeIpv6Pools"
    "fixture/DescribeIpv6Pools.yaml"

requestAttachVpnGateway :: AttachVpnGateway -> TestTree
requestAttachVpnGateway =
  req
    "AttachVpnGateway"
    "fixture/AttachVpnGateway.yaml"

requestDescribeLocalGateways :: DescribeLocalGateways -> TestTree
requestDescribeLocalGateways =
  req
    "DescribeLocalGateways"
    "fixture/DescribeLocalGateways.yaml"

requestModifyVpcEndpointServicePermissions :: ModifyVpcEndpointServicePermissions -> TestTree
requestModifyVpcEndpointServicePermissions =
  req
    "ModifyVpcEndpointServicePermissions"
    "fixture/ModifyVpcEndpointServicePermissions.yaml"

requestExportClientVpnClientCertificateRevocationList :: ExportClientVpnClientCertificateRevocationList -> TestTree
requestExportClientVpnClientCertificateRevocationList =
  req
    "ExportClientVpnClientCertificateRevocationList"
    "fixture/ExportClientVpnClientCertificateRevocationList.yaml"

requestCreateDhcpOptions :: CreateDhcpOptions -> TestTree
requestCreateDhcpOptions =
  req
    "CreateDhcpOptions"
    "fixture/CreateDhcpOptions.yaml"

requestRegisterTransitGatewayMulticastGroupSources :: RegisterTransitGatewayMulticastGroupSources -> TestTree
requestRegisterTransitGatewayMulticastGroupSources =
  req
    "RegisterTransitGatewayMulticastGroupSources"
    "fixture/RegisterTransitGatewayMulticastGroupSources.yaml"

requestDescribeAccountAttributes :: DescribeAccountAttributes -> TestTree
requestDescribeAccountAttributes =
  req
    "DescribeAccountAttributes"
    "fixture/DescribeAccountAttributes.yaml"

requestGetTransitGatewayRouteTablePropagations :: GetTransitGatewayRouteTablePropagations -> TestTree
requestGetTransitGatewayRouteTablePropagations =
  req
    "GetTransitGatewayRouteTablePropagations"
    "fixture/GetTransitGatewayRouteTablePropagations.yaml"

requestModifyFpgaImageAttribute :: ModifyFpgaImageAttribute -> TestTree
requestModifyFpgaImageAttribute =
  req
    "ModifyFpgaImageAttribute"
    "fixture/ModifyFpgaImageAttribute.yaml"

requestModifyHosts :: ModifyHosts -> TestTree
requestModifyHosts =
  req
    "ModifyHosts"
    "fixture/ModifyHosts.yaml"

requestRebootInstances :: RebootInstances -> TestTree
requestRebootInstances =
  req
    "RebootInstances"
    "fixture/RebootInstances.yaml"

requestModifyVpcEndpointServiceConfiguration :: ModifyVpcEndpointServiceConfiguration -> TestTree
requestModifyVpcEndpointServiceConfiguration =
  req
    "ModifyVpcEndpointServiceConfiguration"
    "fixture/ModifyVpcEndpointServiceConfiguration.yaml"

requestCreateTransitGateway :: CreateTransitGateway -> TestTree
requestCreateTransitGateway =
  req
    "CreateTransitGateway"
    "fixture/CreateTransitGateway.yaml"

requestUnassignIpv6Addresses :: UnassignIpv6Addresses -> TestTree
requestUnassignIpv6Addresses =
  req
    "UnassignIpv6Addresses"
    "fixture/UnassignIpv6Addresses.yaml"

requestDeleteTrafficMirrorSession :: DeleteTrafficMirrorSession -> TestTree
requestDeleteTrafficMirrorSession =
  req
    "DeleteTrafficMirrorSession"
    "fixture/DeleteTrafficMirrorSession.yaml"

requestCreateManagedPrefixList :: CreateManagedPrefixList -> TestTree
requestCreateManagedPrefixList =
  req
    "CreateManagedPrefixList"
    "fixture/CreateManagedPrefixList.yaml"

requestAssociateIamInstanceProfile :: AssociateIamInstanceProfile -> TestTree
requestAssociateIamInstanceProfile =
  req
    "AssociateIamInstanceProfile"
    "fixture/AssociateIamInstanceProfile.yaml"

requestModifyDefaultCreditSpecification :: ModifyDefaultCreditSpecification -> TestTree
requestModifyDefaultCreditSpecification =
  req
    "ModifyDefaultCreditSpecification"
    "fixture/ModifyDefaultCreditSpecification.yaml"

requestDeleteEgressOnlyInternetGateway :: DeleteEgressOnlyInternetGateway -> TestTree
requestDeleteEgressOnlyInternetGateway =
  req
    "DeleteEgressOnlyInternetGateway"
    "fixture/DeleteEgressOnlyInternetGateway.yaml"

requestPurchaseHostReservation :: PurchaseHostReservation -> TestTree
requestPurchaseHostReservation =
  req
    "PurchaseHostReservation"
    "fixture/PurchaseHostReservation.yaml"

requestModifyTransitGatewayVpcAttachment :: ModifyTransitGatewayVpcAttachment -> TestTree
requestModifyTransitGatewayVpcAttachment =
  req
    "ModifyTransitGatewayVpcAttachment"
    "fixture/ModifyTransitGatewayVpcAttachment.yaml"

requestCreateImage :: CreateImage -> TestTree
requestCreateImage =
  req
    "CreateImage"
    "fixture/CreateImage.yaml"

requestDescribeClassicLinkInstances :: DescribeClassicLinkInstances -> TestTree
requestDescribeClassicLinkInstances =
  req
    "DescribeClassicLinkInstances"
    "fixture/DescribeClassicLinkInstances.yaml"

requestTerminateInstances :: TerminateInstances -> TestTree
requestTerminateInstances =
  req
    "TerminateInstances"
    "fixture/TerminateInstances.yaml"

requestGetTransitGatewayPrefixListReferences :: GetTransitGatewayPrefixListReferences -> TestTree
requestGetTransitGatewayPrefixListReferences =
  req
    "GetTransitGatewayPrefixListReferences"
    "fixture/GetTransitGatewayPrefixListReferences.yaml"

requestDescribeKeyPairs :: DescribeKeyPairs -> TestTree
requestDescribeKeyPairs =
  req
    "DescribeKeyPairs"
    "fixture/DescribeKeyPairs.yaml"

requestDisableFastSnapshotRestores :: DisableFastSnapshotRestores -> TestTree
requestDisableFastSnapshotRestores =
  req
    "DisableFastSnapshotRestores"
    "fixture/DisableFastSnapshotRestores.yaml"

requestDescribeLaunchTemplates :: DescribeLaunchTemplates -> TestTree
requestDescribeLaunchTemplates =
  req
    "DescribeLaunchTemplates"
    "fixture/DescribeLaunchTemplates.yaml"

requestCreateVpnConnectionRoute :: CreateVpnConnectionRoute -> TestTree
requestCreateVpnConnectionRoute =
  req
    "CreateVpnConnectionRoute"
    "fixture/CreateVpnConnectionRoute.yaml"

requestAssociateRouteTable :: AssociateRouteTable -> TestTree
requestAssociateRouteTable =
  req
    "AssociateRouteTable"
    "fixture/AssociateRouteTable.yaml"

requestDescribeVpnGateways :: DescribeVpnGateways -> TestTree
requestDescribeVpnGateways =
  req
    "DescribeVpnGateways"
    "fixture/DescribeVpnGateways.yaml"

requestModifyVpnConnectionOptions :: ModifyVpnConnectionOptions -> TestTree
requestModifyVpnConnectionOptions =
  req
    "ModifyVpnConnectionOptions"
    "fixture/ModifyVpnConnectionOptions.yaml"

requestGetConsoleOutput :: GetConsoleOutput -> TestTree
requestGetConsoleOutput =
  req
    "GetConsoleOutput"
    "fixture/GetConsoleOutput.yaml"

requestDescribeHosts :: DescribeHosts -> TestTree
requestDescribeHosts =
  req
    "DescribeHosts"
    "fixture/DescribeHosts.yaml"

requestDescribeImageAttribute :: DescribeImageAttribute -> TestTree
requestDescribeImageAttribute =
  req
    "DescribeImageAttribute"
    "fixture/DescribeImageAttribute.yaml"

requestModifyIdFormat :: ModifyIdFormat -> TestTree
requestModifyIdFormat =
  req
    "ModifyIdFormat"
    "fixture/ModifyIdFormat.yaml"

requestRegisterTransitGatewayMulticastGroupMembers :: RegisterTransitGatewayMulticastGroupMembers -> TestTree
requestRegisterTransitGatewayMulticastGroupMembers =
  req
    "RegisterTransitGatewayMulticastGroupMembers"
    "fixture/RegisterTransitGatewayMulticastGroupMembers.yaml"

requestDeleteManagedPrefixList :: DeleteManagedPrefixList -> TestTree
requestDeleteManagedPrefixList =
  req
    "DeleteManagedPrefixList"
    "fixture/DeleteManagedPrefixList.yaml"

requestDeleteRouteTable :: DeleteRouteTable -> TestTree
requestDeleteRouteTable =
  req
    "DeleteRouteTable"
    "fixture/DeleteRouteTable.yaml"

requestResetImageAttribute :: ResetImageAttribute -> TestTree
requestResetImageAttribute =
  req
    "ResetImageAttribute"
    "fixture/ResetImageAttribute.yaml"

requestModifyTransitGatewayPrefixListReference :: ModifyTransitGatewayPrefixListReference -> TestTree
requestModifyTransitGatewayPrefixListReference =
  req
    "ModifyTransitGatewayPrefixListReference"
    "fixture/ModifyTransitGatewayPrefixListReference.yaml"

requestDescribeTransitGatewayRouteTables :: DescribeTransitGatewayRouteTables -> TestTree
requestDescribeTransitGatewayRouteTables =
  req
    "DescribeTransitGatewayRouteTables"
    "fixture/DescribeTransitGatewayRouteTables.yaml"

requestCreateEgressOnlyInternetGateway :: CreateEgressOnlyInternetGateway -> TestTree
requestCreateEgressOnlyInternetGateway =
  req
    "CreateEgressOnlyInternetGateway"
    "fixture/CreateEgressOnlyInternetGateway.yaml"

requestDescribeReservedInstancesModifications :: DescribeReservedInstancesModifications -> TestTree
requestDescribeReservedInstancesModifications =
  req
    "DescribeReservedInstancesModifications"
    "fixture/DescribeReservedInstancesModifications.yaml"

requestDescribeSpotInstanceRequests :: DescribeSpotInstanceRequests -> TestTree
requestDescribeSpotInstanceRequests =
  req
    "DescribeSpotInstanceRequests"
    "fixture/DescribeSpotInstanceRequests.yaml"

requestRevokeClientVpnIngress :: RevokeClientVpnIngress -> TestTree
requestRevokeClientVpnIngress =
  req
    "RevokeClientVpnIngress"
    "fixture/RevokeClientVpnIngress.yaml"

requestUnassignPrivateIpAddresses :: UnassignPrivateIpAddresses -> TestTree
requestUnassignPrivateIpAddresses =
  req
    "UnassignPrivateIpAddresses"
    "fixture/UnassignPrivateIpAddresses.yaml"

requestDescribeNetworkInterfacePermissions :: DescribeNetworkInterfacePermissions -> TestTree
requestDescribeNetworkInterfacePermissions =
  req
    "DescribeNetworkInterfacePermissions"
    "fixture/DescribeNetworkInterfacePermissions.yaml"

requestEnableFastSnapshotRestores :: EnableFastSnapshotRestores -> TestTree
requestEnableFastSnapshotRestores =
  req
    "EnableFastSnapshotRestores"
    "fixture/EnableFastSnapshotRestores.yaml"

requestDescribeVpcEndpointServicePermissions :: DescribeVpcEndpointServicePermissions -> TestTree
requestDescribeVpcEndpointServicePermissions =
  req
    "DescribeVpcEndpointServicePermissions"
    "fixture/DescribeVpcEndpointServicePermissions.yaml"

requestDeleteDhcpOptions :: DeleteDhcpOptions -> TestTree
requestDeleteDhcpOptions =
  req
    "DeleteDhcpOptions"
    "fixture/DeleteDhcpOptions.yaml"

requestRegisterInstanceEventNotificationAttributes :: RegisterInstanceEventNotificationAttributes -> TestTree
requestRegisterInstanceEventNotificationAttributes =
  req
    "RegisterInstanceEventNotificationAttributes"
    "fixture/RegisterInstanceEventNotificationAttributes.yaml"

requestDescribeNetworkAcls :: DescribeNetworkAcls -> TestTree
requestDescribeNetworkAcls =
  req
    "DescribeNetworkAcls"
    "fixture/DescribeNetworkAcls.yaml"

requestCancelImportTask :: CancelImportTask -> TestTree
requestCancelImportTask =
  req
    "CancelImportTask"
    "fixture/CancelImportTask.yaml"

requestDetachClassicLinkVpc :: DetachClassicLinkVpc -> TestTree
requestDetachClassicLinkVpc =
  req
    "DetachClassicLinkVpc"
    "fixture/DetachClassicLinkVpc.yaml"

requestDescribeRegions :: DescribeRegions -> TestTree
requestDescribeRegions =
  req
    "DescribeRegions"
    "fixture/DescribeRegions.yaml"

requestMonitorInstances :: MonitorInstances -> TestTree
requestMonitorInstances =
  req
    "MonitorInstances"
    "fixture/MonitorInstances.yaml"

requestSearchLocalGatewayRoutes :: SearchLocalGatewayRoutes -> TestTree
requestSearchLocalGatewayRoutes =
  req
    "SearchLocalGatewayRoutes"
    "fixture/SearchLocalGatewayRoutes.yaml"

requestDeleteClientVpnRoute :: DeleteClientVpnRoute -> TestTree
requestDeleteClientVpnRoute =
  req
    "DeleteClientVpnRoute"
    "fixture/DeleteClientVpnRoute.yaml"

requestAcceptVpcPeeringConnection :: AcceptVpcPeeringConnection -> TestTree
requestAcceptVpcPeeringConnection =
  req
    "AcceptVpcPeeringConnection"
    "fixture/AcceptVpcPeeringConnection.yaml"

requestImportSnapshot :: ImportSnapshot -> TestTree
requestImportSnapshot =
  req
    "ImportSnapshot"
    "fixture/ImportSnapshot.yaml"

requestDescribeVolumeStatus :: DescribeVolumeStatus -> TestTree
requestDescribeVolumeStatus =
  req
    "DescribeVolumeStatus"
    "fixture/DescribeVolumeStatus.yaml"

requestDescribeRouteTables :: DescribeRouteTables -> TestTree
requestDescribeRouteTables =
  req
    "DescribeRouteTables"
    "fixture/DescribeRouteTables.yaml"

requestDescribeAvailabilityZones :: DescribeAvailabilityZones -> TestTree
requestDescribeAvailabilityZones =
  req
    "DescribeAvailabilityZones"
    "fixture/DescribeAvailabilityZones.yaml"

requestModifyVpcAttribute :: ModifyVpcAttribute -> TestTree
requestModifyVpcAttribute =
  req
    "ModifyVpcAttribute"
    "fixture/ModifyVpcAttribute.yaml"

requestDescribeClientVpnConnections :: DescribeClientVpnConnections -> TestTree
requestDescribeClientVpnConnections =
  req
    "DescribeClientVpnConnections"
    "fixture/DescribeClientVpnConnections.yaml"

requestDescribeFleetHistory :: DescribeFleetHistory -> TestTree
requestDescribeFleetHistory =
  req
    "DescribeFleetHistory"
    "fixture/DescribeFleetHistory.yaml"

requestDescribeImages :: DescribeImages -> TestTree
requestDescribeImages =
  req
    "DescribeImages"
    "fixture/DescribeImages.yaml"

requestDescribeElasticGpus :: DescribeElasticGpus -> TestTree
requestDescribeElasticGpus =
  req
    "DescribeElasticGpus"
    "fixture/DescribeElasticGpus.yaml"

requestRestoreAddressToClassic :: RestoreAddressToClassic -> TestTree
requestRestoreAddressToClassic =
  req
    "RestoreAddressToClassic"
    "fixture/RestoreAddressToClassic.yaml"

requestDescribeManagedPrefixLists :: DescribeManagedPrefixLists -> TestTree
requestDescribeManagedPrefixLists =
  req
    "DescribeManagedPrefixLists"
    "fixture/DescribeManagedPrefixLists.yaml"

requestCreateKeyPair :: CreateKeyPair -> TestTree
requestCreateKeyPair =
  req
    "CreateKeyPair"
    "fixture/CreateKeyPair.yaml"

requestGetReservedInstancesExchangeQuote :: GetReservedInstancesExchangeQuote -> TestTree
requestGetReservedInstancesExchangeQuote =
  req
    "GetReservedInstancesExchangeQuote"
    "fixture/GetReservedInstancesExchangeQuote.yaml"

requestDeleteVolume :: DeleteVolume -> TestTree
requestDeleteVolume =
  req
    "DeleteVolume"
    "fixture/DeleteVolume.yaml"

requestDeprovisionByoipCidr :: DeprovisionByoipCidr -> TestTree
requestDeprovisionByoipCidr =
  req
    "DeprovisionByoipCidr"
    "fixture/DeprovisionByoipCidr.yaml"

requestDeleteVpcEndpointServiceConfigurations :: DeleteVpcEndpointServiceConfigurations -> TestTree
requestDeleteVpcEndpointServiceConfigurations =
  req
    "DeleteVpcEndpointServiceConfigurations"
    "fixture/DeleteVpcEndpointServiceConfigurations.yaml"

requestDescribeSpotFleetInstances :: DescribeSpotFleetInstances -> TestTree
requestDescribeSpotFleetInstances =
  req
    "DescribeSpotFleetInstances"
    "fixture/DescribeSpotFleetInstances.yaml"

-- Responses

responseModifyCapacityReservation :: ModifyCapacityReservationResponse -> TestTree
responseModifyCapacityReservation =
  res
    "ModifyCapacityReservationResponse"
    "fixture/ModifyCapacityReservationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyCapacityReservation)

responseGetAssociatedIpv6PoolCidrs :: GetAssociatedIpv6PoolCidrsResponse -> TestTree
responseGetAssociatedIpv6PoolCidrs =
  res
    "GetAssociatedIpv6PoolCidrsResponse"
    "fixture/GetAssociatedIpv6PoolCidrsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetAssociatedIpv6PoolCidrs)

responseImportInstance :: ImportInstanceResponse -> TestTree
responseImportInstance =
  res
    "ImportInstanceResponse"
    "fixture/ImportInstanceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ImportInstance)

responseRevokeSecurityGroupEgress :: RevokeSecurityGroupEgressResponse -> TestTree
responseRevokeSecurityGroupEgress =
  res
    "RevokeSecurityGroupEgressResponse"
    "fixture/RevokeSecurityGroupEgressResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RevokeSecurityGroupEgress)

responseCreateNetworkInterfacePermission :: CreateNetworkInterfacePermissionResponse -> TestTree
responseCreateNetworkInterfacePermission =
  res
    "CreateNetworkInterfacePermissionResponse"
    "fixture/CreateNetworkInterfacePermissionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateNetworkInterfacePermission)

responseSendDiagnosticInterrupt :: SendDiagnosticInterruptResponse -> TestTree
responseSendDiagnosticInterrupt =
  res
    "SendDiagnosticInterruptResponse"
    "fixture/SendDiagnosticInterruptResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SendDiagnosticInterrupt)

responseDeleteLaunchTemplate :: DeleteLaunchTemplateResponse -> TestTree
responseDeleteLaunchTemplate =
  res
    "DeleteLaunchTemplateResponse"
    "fixture/DeleteLaunchTemplateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteLaunchTemplate)

responseRejectVpcEndpointConnections :: RejectVpcEndpointConnectionsResponse -> TestTree
responseRejectVpcEndpointConnections =
  res
    "RejectVpcEndpointConnectionsResponse"
    "fixture/RejectVpcEndpointConnectionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RejectVpcEndpointConnections)

responseCreateVpnGateway :: CreateVpnGatewayResponse -> TestTree
responseCreateVpnGateway =
  res
    "CreateVpnGatewayResponse"
    "fixture/CreateVpnGatewayResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateVpnGateway)

responseCreateNetworkAcl :: CreateNetworkAclResponse -> TestTree
responseCreateNetworkAcl =
  res
    "CreateNetworkAclResponse"
    "fixture/CreateNetworkAclResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateNetworkAcl)

responseDeleteKeyPair :: DeleteKeyPairResponse -> TestTree
responseDeleteKeyPair =
  res
    "DeleteKeyPairResponse"
    "fixture/DeleteKeyPairResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteKeyPair)

responseDescribeSecurityGroupReferences :: DescribeSecurityGroupReferencesResponse -> TestTree
responseDescribeSecurityGroupReferences =
  res
    "DescribeSecurityGroupReferencesResponse"
    "fixture/DescribeSecurityGroupReferencesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeSecurityGroupReferences)

responseDeleteFleets :: DeleteFleetsResponse -> TestTree
responseDeleteFleets =
  res
    "DeleteFleetsResponse"
    "fixture/DeleteFleetsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteFleets)

responseDescribeTags :: DescribeTagsResponse -> TestTree
responseDescribeTags =
  res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeTags)

responseCreateTransitGatewayRouteTable :: CreateTransitGatewayRouteTableResponse -> TestTree
responseCreateTransitGatewayRouteTable =
  res
    "CreateTransitGatewayRouteTableResponse"
    "fixture/CreateTransitGatewayRouteTableResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateTransitGatewayRouteTable)

responseModifyInstanceMetadataOptions :: ModifyInstanceMetadataOptionsResponse -> TestTree
responseModifyInstanceMetadataOptions =
  res
    "ModifyInstanceMetadataOptionsResponse"
    "fixture/ModifyInstanceMetadataOptionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyInstanceMetadataOptions)

responseUpdateSecurityGroupRuleDescriptionsIngress :: UpdateSecurityGroupRuleDescriptionsIngressResponse -> TestTree
responseUpdateSecurityGroupRuleDescriptionsIngress =
  res
    "UpdateSecurityGroupRuleDescriptionsIngressResponse"
    "fixture/UpdateSecurityGroupRuleDescriptionsIngressResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateSecurityGroupRuleDescriptionsIngress)

responseDisassociateSubnetCidrBlock :: DisassociateSubnetCidrBlockResponse -> TestTree
responseDisassociateSubnetCidrBlock =
  res
    "DisassociateSubnetCidrBlockResponse"
    "fixture/DisassociateSubnetCidrBlockResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisassociateSubnetCidrBlock)

responseDetachNetworkInterface :: DetachNetworkInterfaceResponse -> TestTree
responseDetachNetworkInterface =
  res
    "DetachNetworkInterfaceResponse"
    "fixture/DetachNetworkInterfaceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DetachNetworkInterface)

responseDetachInternetGateway :: DetachInternetGatewayResponse -> TestTree
responseDetachInternetGateway =
  res
    "DetachInternetGatewayResponse"
    "fixture/DetachInternetGatewayResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DetachInternetGateway)

responseDeleteVpcEndpoints :: DeleteVpcEndpointsResponse -> TestTree
responseDeleteVpcEndpoints =
  res
    "DeleteVpcEndpointsResponse"
    "fixture/DeleteVpcEndpointsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteVpcEndpoints)

responseDescribeClientVpnEndpoints :: DescribeClientVpnEndpointsResponse -> TestTree
responseDescribeClientVpnEndpoints =
  res
    "DescribeClientVpnEndpointsResponse"
    "fixture/DescribeClientVpnEndpointsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeClientVpnEndpoints)

responseDeleteFlowLogs :: DeleteFlowLogsResponse -> TestTree
responseDeleteFlowLogs =
  res
    "DeleteFlowLogsResponse"
    "fixture/DeleteFlowLogsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteFlowLogs)

responseDescribeVpcClassicLink :: DescribeVpcClassicLinkResponse -> TestTree
responseDescribeVpcClassicLink =
  res
    "DescribeVpcClassicLinkResponse"
    "fixture/DescribeVpcClassicLinkResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeVpcClassicLink)

responseGetAssociatedEnclaveCertificateIamRoles :: GetAssociatedEnclaveCertificateIamRolesResponse -> TestTree
responseGetAssociatedEnclaveCertificateIamRoles =
  res
    "GetAssociatedEnclaveCertificateIamRolesResponse"
    "fixture/GetAssociatedEnclaveCertificateIamRolesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetAssociatedEnclaveCertificateIamRoles)

responseAssociateTransitGatewayMulticastDomain :: AssociateTransitGatewayMulticastDomainResponse -> TestTree
responseAssociateTransitGatewayMulticastDomain =
  res
    "AssociateTransitGatewayMulticastDomainResponse"
    "fixture/AssociateTransitGatewayMulticastDomainResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AssociateTransitGatewayMulticastDomain)

responseModifySubnetAttribute :: ModifySubnetAttributeResponse -> TestTree
responseModifySubnetAttribute =
  res
    "ModifySubnetAttributeResponse"
    "fixture/ModifySubnetAttributeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifySubnetAttribute)

responseDetachVolume :: VolumeAttachment -> TestTree
responseDetachVolume =
  res
    "DetachVolumeResponse"
    "fixture/DetachVolumeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DetachVolume)

responseDescribeInstanceCreditSpecifications :: DescribeInstanceCreditSpecificationsResponse -> TestTree
responseDescribeInstanceCreditSpecifications =
  res
    "DescribeInstanceCreditSpecificationsResponse"
    "fixture/DescribeInstanceCreditSpecificationsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeInstanceCreditSpecifications)

responseCancelBundleTask :: CancelBundleTaskResponse -> TestTree
responseCancelBundleTask =
  res
    "CancelBundleTaskResponse"
    "fixture/CancelBundleTaskResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CancelBundleTask)

responseDescribeByoipCidrs :: DescribeByoipCidrsResponse -> TestTree
responseDescribeByoipCidrs =
  res
    "DescribeByoipCidrsResponse"
    "fixture/DescribeByoipCidrsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeByoipCidrs)

responseAcceptReservedInstancesExchangeQuote :: AcceptReservedInstancesExchangeQuoteResponse -> TestTree
responseAcceptReservedInstancesExchangeQuote =
  res
    "AcceptReservedInstancesExchangeQuoteResponse"
    "fixture/AcceptReservedInstancesExchangeQuoteResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AcceptReservedInstancesExchangeQuote)

responseReleaseAddress :: ReleaseAddressResponse -> TestTree
responseReleaseAddress =
  res
    "ReleaseAddressResponse"
    "fixture/ReleaseAddressResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ReleaseAddress)

responseDescribeInstanceTypeOfferings :: DescribeInstanceTypeOfferingsResponse -> TestTree
responseDescribeInstanceTypeOfferings =
  res
    "DescribeInstanceTypeOfferingsResponse"
    "fixture/DescribeInstanceTypeOfferingsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeInstanceTypeOfferings)

responseCreateInternetGateway :: CreateInternetGatewayResponse -> TestTree
responseCreateInternetGateway =
  res
    "CreateInternetGatewayResponse"
    "fixture/CreateInternetGatewayResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateInternetGateway)

responseDeleteVpnConnection :: DeleteVpnConnectionResponse -> TestTree
responseDeleteVpnConnection =
  res
    "DeleteVpnConnectionResponse"
    "fixture/DeleteVpnConnectionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteVpnConnection)

responseDescribeBundleTasks :: DescribeBundleTasksResponse -> TestTree
responseDescribeBundleTasks =
  res
    "DescribeBundleTasksResponse"
    "fixture/DescribeBundleTasksResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeBundleTasks)

responseAuthorizeSecurityGroupEgress :: AuthorizeSecurityGroupEgressResponse -> TestTree
responseAuthorizeSecurityGroupEgress =
  res
    "AuthorizeSecurityGroupEgressResponse"
    "fixture/AuthorizeSecurityGroupEgressResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AuthorizeSecurityGroupEgress)

responseEnableTransitGatewayRouteTablePropagation :: EnableTransitGatewayRouteTablePropagationResponse -> TestTree
responseEnableTransitGatewayRouteTablePropagation =
  res
    "EnableTransitGatewayRouteTablePropagationResponse"
    "fixture/EnableTransitGatewayRouteTablePropagationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy EnableTransitGatewayRouteTablePropagation)

responseDeregisterImage :: DeregisterImageResponse -> TestTree
responseDeregisterImage =
  res
    "DeregisterImageResponse"
    "fixture/DeregisterImageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeregisterImage)

responseDeleteVpcEndpointConnectionNotifications :: DeleteVpcEndpointConnectionNotificationsResponse -> TestTree
responseDeleteVpcEndpointConnectionNotifications =
  res
    "DeleteVpcEndpointConnectionNotificationsResponse"
    "fixture/DeleteVpcEndpointConnectionNotificationsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteVpcEndpointConnectionNotifications)

responseDescribeCoipPools :: DescribeCoipPoolsResponse -> TestTree
responseDescribeCoipPools =
  res
    "DescribeCoipPoolsResponse"
    "fixture/DescribeCoipPoolsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeCoipPools)

responseGetTransitGatewayMulticastDomainAssociations :: GetTransitGatewayMulticastDomainAssociationsResponse -> TestTree
responseGetTransitGatewayMulticastDomainAssociations =
  res
    "GetTransitGatewayMulticastDomainAssociationsResponse"
    "fixture/GetTransitGatewayMulticastDomainAssociationsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetTransitGatewayMulticastDomainAssociations)

responseDeleteLocalGatewayRouteTableVpcAssociation :: DeleteLocalGatewayRouteTableVpcAssociationResponse -> TestTree
responseDeleteLocalGatewayRouteTableVpcAssociation =
  res
    "DeleteLocalGatewayRouteTableVpcAssociationResponse"
    "fixture/DeleteLocalGatewayRouteTableVpcAssociationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteLocalGatewayRouteTableVpcAssociation)

responseModifyNetworkInterfaceAttribute :: ModifyNetworkInterfaceAttributeResponse -> TestTree
responseModifyNetworkInterfaceAttribute =
  res
    "ModifyNetworkInterfaceAttributeResponse"
    "fixture/ModifyNetworkInterfaceAttributeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyNetworkInterfaceAttribute)

responseModifyVpcTenancy :: ModifyVpcTenancyResponse -> TestTree
responseModifyVpcTenancy =
  res
    "ModifyVpcTenancyResponse"
    "fixture/ModifyVpcTenancyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyVpcTenancy)

responseDescribeInstanceTypes :: DescribeInstanceTypesResponse -> TestTree
responseDescribeInstanceTypes =
  res
    "DescribeInstanceTypesResponse"
    "fixture/DescribeInstanceTypesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeInstanceTypes)

responseDescribeClientVpnAuthorizationRules :: DescribeClientVpnAuthorizationRulesResponse -> TestTree
responseDescribeClientVpnAuthorizationRules =
  res
    "DescribeClientVpnAuthorizationRulesResponse"
    "fixture/DescribeClientVpnAuthorizationRulesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeClientVpnAuthorizationRules)

responseDeleteTransitGatewayVpcAttachment :: DeleteTransitGatewayVpcAttachmentResponse -> TestTree
responseDeleteTransitGatewayVpcAttachment =
  res
    "DeleteTransitGatewayVpcAttachmentResponse"
    "fixture/DeleteTransitGatewayVpcAttachmentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteTransitGatewayVpcAttachment)

responseDeleteTransitGatewayMulticastDomain :: DeleteTransitGatewayMulticastDomainResponse -> TestTree
responseDeleteTransitGatewayMulticastDomain =
  res
    "DeleteTransitGatewayMulticastDomainResponse"
    "fixture/DeleteTransitGatewayMulticastDomainResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteTransitGatewayMulticastDomain)

responseCancelReservedInstancesListing :: CancelReservedInstancesListingResponse -> TestTree
responseCancelReservedInstancesListing =
  res
    "CancelReservedInstancesListingResponse"
    "fixture/CancelReservedInstancesListingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CancelReservedInstancesListing)

responseAttachClassicLinkVpc :: AttachClassicLinkVpcResponse -> TestTree
responseAttachClassicLinkVpc =
  res
    "AttachClassicLinkVpcResponse"
    "fixture/AttachClassicLinkVpcResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AttachClassicLinkVpc)

responseDisableTransitGatewayRouteTablePropagation :: DisableTransitGatewayRouteTablePropagationResponse -> TestTree
responseDisableTransitGatewayRouteTablePropagation =
  res
    "DisableTransitGatewayRouteTablePropagationResponse"
    "fixture/DisableTransitGatewayRouteTablePropagationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisableTransitGatewayRouteTablePropagation)

responseDescribeVpcClassicLinkDnsSupport :: DescribeVpcClassicLinkDnsSupportResponse -> TestTree
responseDescribeVpcClassicLinkDnsSupport =
  res
    "DescribeVpcClassicLinkDnsSupportResponse"
    "fixture/DescribeVpcClassicLinkDnsSupportResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeVpcClassicLinkDnsSupport)

responseAssociateSubnetCidrBlock :: AssociateSubnetCidrBlockResponse -> TestTree
responseAssociateSubnetCidrBlock =
  res
    "AssociateSubnetCidrBlockResponse"
    "fixture/AssociateSubnetCidrBlockResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AssociateSubnetCidrBlock)

responseRunScheduledInstances :: RunScheduledInstancesResponse -> TestTree
responseRunScheduledInstances =
  res
    "RunScheduledInstancesResponse"
    "fixture/RunScheduledInstancesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RunScheduledInstances)

responseCreateTransitGatewayRoute :: CreateTransitGatewayRouteResponse -> TestTree
responseCreateTransitGatewayRoute =
  res
    "CreateTransitGatewayRouteResponse"
    "fixture/CreateTransitGatewayRouteResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateTransitGatewayRoute)

responseCreateTransitGatewayPrefixListReference :: CreateTransitGatewayPrefixListReferenceResponse -> TestTree
responseCreateTransitGatewayPrefixListReference =
  res
    "CreateTransitGatewayPrefixListReferenceResponse"
    "fixture/CreateTransitGatewayPrefixListReferenceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateTransitGatewayPrefixListReference)

responseCancelSpotFleetRequests :: CancelSpotFleetRequestsResponse -> TestTree
responseCancelSpotFleetRequests =
  res
    "CancelSpotFleetRequestsResponse"
    "fixture/CancelSpotFleetRequestsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CancelSpotFleetRequests)

responseDescribeSpotPriceHistory :: DescribeSpotPriceHistoryResponse -> TestTree
responseDescribeSpotPriceHistory =
  res
    "DescribeSpotPriceHistoryResponse"
    "fixture/DescribeSpotPriceHistoryResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeSpotPriceHistory)

responseDescribeDhcpOptions :: DescribeDhcpOptionsResponse -> TestTree
responseDescribeDhcpOptions =
  res
    "DescribeDhcpOptionsResponse"
    "fixture/DescribeDhcpOptionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeDhcpOptions)

responseImportImage :: ImportImageResponse -> TestTree
responseImportImage =
  res
    "ImportImageResponse"
    "fixture/ImportImageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ImportImage)

responseCreateLocalGatewayRouteTableVpcAssociation :: CreateLocalGatewayRouteTableVpcAssociationResponse -> TestTree
responseCreateLocalGatewayRouteTableVpcAssociation =
  res
    "CreateLocalGatewayRouteTableVpcAssociationResponse"
    "fixture/CreateLocalGatewayRouteTableVpcAssociationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateLocalGatewayRouteTableVpcAssociation)

responseCopyFpgaImage :: CopyFpgaImageResponse -> TestTree
responseCopyFpgaImage =
  res
    "CopyFpgaImageResponse"
    "fixture/CopyFpgaImageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CopyFpgaImage)

responseImportClientVpnClientCertificateRevocationList :: ImportClientVpnClientCertificateRevocationListResponse -> TestTree
responseImportClientVpnClientCertificateRevocationList =
  res
    "ImportClientVpnClientCertificateRevocationListResponse"
    "fixture/ImportClientVpnClientCertificateRevocationListResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ImportClientVpnClientCertificateRevocationList)

responseStopInstances :: StopInstancesResponse -> TestTree
responseStopInstances =
  res
    "StopInstancesResponse"
    "fixture/StopInstancesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StopInstances)

responseEnableEbsEncryptionByDefault :: EnableEbsEncryptionByDefaultResponse -> TestTree
responseEnableEbsEncryptionByDefault =
  res
    "EnableEbsEncryptionByDefaultResponse"
    "fixture/EnableEbsEncryptionByDefaultResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy EnableEbsEncryptionByDefault)

responseDeregisterTransitGatewayMulticastGroupSources :: DeregisterTransitGatewayMulticastGroupSourcesResponse -> TestTree
responseDeregisterTransitGatewayMulticastGroupSources =
  res
    "DeregisterTransitGatewayMulticastGroupSourcesResponse"
    "fixture/DeregisterTransitGatewayMulticastGroupSourcesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeregisterTransitGatewayMulticastGroupSources)

responseModifyLaunchTemplate :: ModifyLaunchTemplateResponse -> TestTree
responseModifyLaunchTemplate =
  res
    "ModifyLaunchTemplateResponse"
    "fixture/ModifyLaunchTemplateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyLaunchTemplate)

responseModifyVpcEndpointConnectionNotification :: ModifyVpcEndpointConnectionNotificationResponse -> TestTree
responseModifyVpcEndpointConnectionNotification =
  res
    "ModifyVpcEndpointConnectionNotificationResponse"
    "fixture/ModifyVpcEndpointConnectionNotificationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyVpcEndpointConnectionNotification)

responseDescribeInternetGateways :: DescribeInternetGatewaysResponse -> TestTree
responseDescribeInternetGateways =
  res
    "DescribeInternetGatewaysResponse"
    "fixture/DescribeInternetGatewaysResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeInternetGateways)

responseDisableVpcClassicLink :: DisableVpcClassicLinkResponse -> TestTree
responseDisableVpcClassicLink =
  res
    "DisableVpcClassicLinkResponse"
    "fixture/DisableVpcClassicLinkResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisableVpcClassicLink)

responseGetGroupsForCapacityReservation :: GetGroupsForCapacityReservationResponse -> TestTree
responseGetGroupsForCapacityReservation =
  res
    "GetGroupsForCapacityReservationResponse"
    "fixture/GetGroupsForCapacityReservationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetGroupsForCapacityReservation)

responseDeleteLaunchTemplateVersions :: DeleteLaunchTemplateVersionsResponse -> TestTree
responseDeleteLaunchTemplateVersions =
  res
    "DeleteLaunchTemplateVersionsResponse"
    "fixture/DeleteLaunchTemplateVersionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteLaunchTemplateVersions)

responseBundleInstance :: BundleInstanceResponse -> TestTree
responseBundleInstance =
  res
    "BundleInstanceResponse"
    "fixture/BundleInstanceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy BundleInstance)

responseDescribeNetworkInterfaces :: DescribeNetworkInterfacesResponse -> TestTree
responseDescribeNetworkInterfaces =
  res
    "DescribeNetworkInterfacesResponse"
    "fixture/DescribeNetworkInterfacesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeNetworkInterfaces)

responseReplaceNetworkAclAssociation :: ReplaceNetworkAclAssociationResponse -> TestTree
responseReplaceNetworkAclAssociation =
  res
    "ReplaceNetworkAclAssociationResponse"
    "fixture/ReplaceNetworkAclAssociationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ReplaceNetworkAclAssociation)

responseDescribeNatGateways :: DescribeNatGatewaysResponse -> TestTree
responseDescribeNatGateways =
  res
    "DescribeNatGatewaysResponse"
    "fixture/DescribeNatGatewaysResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeNatGateways)

responseDescribeAddresses :: DescribeAddressesResponse -> TestTree
responseDescribeAddresses =
  res
    "DescribeAddressesResponse"
    "fixture/DescribeAddressesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeAddresses)

responseRestoreManagedPrefixListVersion :: RestoreManagedPrefixListVersionResponse -> TestTree
responseRestoreManagedPrefixListVersion =
  res
    "RestoreManagedPrefixListVersionResponse"
    "fixture/RestoreManagedPrefixListVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RestoreManagedPrefixListVersion)

responseDescribeSnapshotAttribute :: DescribeSnapshotAttributeResponse -> TestTree
responseDescribeSnapshotAttribute =
  res
    "DescribeSnapshotAttributeResponse"
    "fixture/DescribeSnapshotAttributeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeSnapshotAttribute)

responseDescribeIdentityIdFormat :: DescribeIdentityIdFormatResponse -> TestTree
responseDescribeIdentityIdFormat =
  res
    "DescribeIdentityIdFormatResponse"
    "fixture/DescribeIdentityIdFormatResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeIdentityIdFormat)

responseReplaceRoute :: ReplaceRouteResponse -> TestTree
responseReplaceRoute =
  res
    "ReplaceRouteResponse"
    "fixture/ReplaceRouteResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ReplaceRoute)

responseDescribeVpcEndpointServices :: DescribeVpcEndpointServicesResponse -> TestTree
responseDescribeVpcEndpointServices =
  res
    "DescribeVpcEndpointServicesResponse"
    "fixture/DescribeVpcEndpointServicesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeVpcEndpointServices)

responseDeleteLocalGatewayRoute :: DeleteLocalGatewayRouteResponse -> TestTree
responseDeleteLocalGatewayRoute =
  res
    "DeleteLocalGatewayRouteResponse"
    "fixture/DeleteLocalGatewayRouteResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteLocalGatewayRoute)

responseAuthorizeSecurityGroupIngress :: AuthorizeSecurityGroupIngressResponse -> TestTree
responseAuthorizeSecurityGroupIngress =
  res
    "AuthorizeSecurityGroupIngressResponse"
    "fixture/AuthorizeSecurityGroupIngressResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AuthorizeSecurityGroupIngress)

responseCreateVpcPeeringConnection :: CreateVpcPeeringConnectionResponse -> TestTree
responseCreateVpcPeeringConnection =
  res
    "CreateVpcPeeringConnectionResponse"
    "fixture/CreateVpcPeeringConnectionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateVpcPeeringConnection)

responseDescribeSubnets :: DescribeSubnetsResponse -> TestTree
responseDescribeSubnets =
  res
    "DescribeSubnetsResponse"
    "fixture/DescribeSubnetsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeSubnets)

responseGetTransitGatewayAttachmentPropagations :: GetTransitGatewayAttachmentPropagationsResponse -> TestTree
responseGetTransitGatewayAttachmentPropagations =
  res
    "GetTransitGatewayAttachmentPropagationsResponse"
    "fixture/GetTransitGatewayAttachmentPropagationsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetTransitGatewayAttachmentPropagations)

responseCreateTags :: CreateTagsResponse -> TestTree
responseCreateTags =
  res
    "CreateTagsResponse"
    "fixture/CreateTagsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateTags)

responsePurchaseReservedInstancesOffering :: PurchaseReservedInstancesOfferingResponse -> TestTree
responsePurchaseReservedInstancesOffering =
  res
    "PurchaseReservedInstancesOfferingResponse"
    "fixture/PurchaseReservedInstancesOfferingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PurchaseReservedInstancesOffering)

responseDeleteNetworkAclEntry :: DeleteNetworkAclEntryResponse -> TestTree
responseDeleteNetworkAclEntry =
  res
    "DeleteNetworkAclEntryResponse"
    "fixture/DeleteNetworkAclEntryResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteNetworkAclEntry)

responseResetSnapshotAttribute :: ResetSnapshotAttributeResponse -> TestTree
responseResetSnapshotAttribute =
  res
    "ResetSnapshotAttributeResponse"
    "fixture/ResetSnapshotAttributeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ResetSnapshotAttribute)

responseDescribeVpnConnections :: DescribeVpnConnectionsResponse -> TestTree
responseDescribeVpnConnections =
  res
    "DescribeVpnConnectionsResponse"
    "fixture/DescribeVpnConnectionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeVpnConnections)

responseModifyInstanceEventStartTime :: ModifyInstanceEventStartTimeResponse -> TestTree
responseModifyInstanceEventStartTime =
  res
    "ModifyInstanceEventStartTimeResponse"
    "fixture/ModifyInstanceEventStartTimeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyInstanceEventStartTime)

responseDeleteRoute :: DeleteRouteResponse -> TestTree
responseDeleteRoute =
  res
    "DeleteRouteResponse"
    "fixture/DeleteRouteResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteRoute)

responseReplaceNetworkAclEntry :: ReplaceNetworkAclEntryResponse -> TestTree
responseReplaceNetworkAclEntry =
  res
    "ReplaceNetworkAclEntryResponse"
    "fixture/ReplaceNetworkAclEntryResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ReplaceNetworkAclEntry)

responseDescribeVpcEndpoints :: DescribeVpcEndpointsResponse -> TestTree
responseDescribeVpcEndpoints =
  res
    "DescribeVpcEndpointsResponse"
    "fixture/DescribeVpcEndpointsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeVpcEndpoints)

responseCreateTrafficMirrorFilter :: CreateTrafficMirrorFilterResponse -> TestTree
responseCreateTrafficMirrorFilter =
  res
    "CreateTrafficMirrorFilterResponse"
    "fixture/CreateTrafficMirrorFilterResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateTrafficMirrorFilter)

responseResetInstanceAttribute :: ResetInstanceAttributeResponse -> TestTree
responseResetInstanceAttribute =
  res
    "ResetInstanceAttributeResponse"
    "fixture/ResetInstanceAttributeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ResetInstanceAttribute)

responseModifyIdentityIdFormat :: ModifyIdentityIdFormatResponse -> TestTree
responseModifyIdentityIdFormat =
  res
    "ModifyIdentityIdFormatResponse"
    "fixture/ModifyIdentityIdFormatResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyIdentityIdFormat)

responseAttachNetworkInterface :: AttachNetworkInterfaceResponse -> TestTree
responseAttachNetworkInterface =
  res
    "AttachNetworkInterfaceResponse"
    "fixture/AttachNetworkInterfaceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AttachNetworkInterface)

responseCreateCapacityReservation :: CreateCapacityReservationResponse -> TestTree
responseCreateCapacityReservation =
  res
    "CreateCapacityReservationResponse"
    "fixture/CreateCapacityReservationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateCapacityReservation)

responseDescribeInstanceStatus :: DescribeInstanceStatusResponse -> TestTree
responseDescribeInstanceStatus =
  res
    "DescribeInstanceStatusResponse"
    "fixture/DescribeInstanceStatusResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeInstanceStatus)

responseImportKeyPair :: ImportKeyPairResponse -> TestTree
responseImportKeyPair =
  res
    "ImportKeyPairResponse"
    "fixture/ImportKeyPairResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ImportKeyPair)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags =
  res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteTags)

responseConfirmProductInstance :: ConfirmProductInstanceResponse -> TestTree
responseConfirmProductInstance =
  res
    "ConfirmProductInstanceResponse"
    "fixture/ConfirmProductInstanceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ConfirmProductInstance)

responseDescribeInstanceAttribute :: DescribeInstanceAttributeResponse -> TestTree
responseDescribeInstanceAttribute =
  res
    "DescribeInstanceAttributeResponse"
    "fixture/DescribeInstanceAttributeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeInstanceAttribute)

responseDescribeReservedInstancesOfferings :: DescribeReservedInstancesOfferingsResponse -> TestTree
responseDescribeReservedInstancesOfferings =
  res
    "DescribeReservedInstancesOfferingsResponse"
    "fixture/DescribeReservedInstancesOfferingsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeReservedInstancesOfferings)

responseCreateCustomerGateway :: CreateCustomerGatewayResponse -> TestTree
responseCreateCustomerGateway =
  res
    "CreateCustomerGatewayResponse"
    "fixture/CreateCustomerGatewayResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateCustomerGateway)

responseDescribeFleets :: DescribeFleetsResponse -> TestTree
responseDescribeFleets =
  res
    "DescribeFleetsResponse"
    "fixture/DescribeFleetsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeFleets)

responseCreateTransitGatewayPeeringAttachment :: CreateTransitGatewayPeeringAttachmentResponse -> TestTree
responseCreateTransitGatewayPeeringAttachment =
  res
    "CreateTransitGatewayPeeringAttachmentResponse"
    "fixture/CreateTransitGatewayPeeringAttachmentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateTransitGatewayPeeringAttachment)

responseDeleteSecurityGroup :: DeleteSecurityGroupResponse -> TestTree
responseDeleteSecurityGroup =
  res
    "DeleteSecurityGroupResponse"
    "fixture/DeleteSecurityGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteSecurityGroup)

responseDescribePublicIpv4Pools :: DescribePublicIpv4PoolsResponse -> TestTree
responseDescribePublicIpv4Pools =
  res
    "DescribePublicIpv4PoolsResponse"
    "fixture/DescribePublicIpv4PoolsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribePublicIpv4Pools)

responseDescribeClientVpnTargetNetworks :: DescribeClientVpnTargetNetworksResponse -> TestTree
responseDescribeClientVpnTargetNetworks =
  res
    "DescribeClientVpnTargetNetworksResponse"
    "fixture/DescribeClientVpnTargetNetworksResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeClientVpnTargetNetworks)

responseDeleteVpcPeeringConnection :: DeleteVpcPeeringConnectionResponse -> TestTree
responseDeleteVpcPeeringConnection =
  res
    "DeleteVpcPeeringConnectionResponse"
    "fixture/DeleteVpcPeeringConnectionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteVpcPeeringConnection)

responseAttachInternetGateway :: AttachInternetGatewayResponse -> TestTree
responseAttachInternetGateway =
  res
    "AttachInternetGatewayResponse"
    "fixture/AttachInternetGatewayResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AttachInternetGateway)

responseModifyInstancePlacement :: ModifyInstancePlacementResponse -> TestTree
responseModifyInstancePlacement =
  res
    "ModifyInstancePlacementResponse"
    "fixture/ModifyInstancePlacementResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyInstancePlacement)

responseDescribeFlowLogs :: DescribeFlowLogsResponse -> TestTree
responseDescribeFlowLogs =
  res
    "DescribeFlowLogsResponse"
    "fixture/DescribeFlowLogsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeFlowLogs)

responseDescribeLocalGatewayVirtualInterfaceGroups :: DescribeLocalGatewayVirtualInterfaceGroupsResponse -> TestTree
responseDescribeLocalGatewayVirtualInterfaceGroups =
  res
    "DescribeLocalGatewayVirtualInterfaceGroupsResponse"
    "fixture/DescribeLocalGatewayVirtualInterfaceGroupsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeLocalGatewayVirtualInterfaceGroups)

responseDescribeLocalGatewayRouteTableVpcAssociations :: DescribeLocalGatewayRouteTableVpcAssociationsResponse -> TestTree
responseDescribeLocalGatewayRouteTableVpcAssociations =
  res
    "DescribeLocalGatewayRouteTableVpcAssociationsResponse"
    "fixture/DescribeLocalGatewayRouteTableVpcAssociationsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeLocalGatewayRouteTableVpcAssociations)

responseDescribeVpcEndpointConnectionNotifications :: DescribeVpcEndpointConnectionNotificationsResponse -> TestTree
responseDescribeVpcEndpointConnectionNotifications =
  res
    "DescribeVpcEndpointConnectionNotificationsResponse"
    "fixture/DescribeVpcEndpointConnectionNotificationsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeVpcEndpointConnectionNotifications)

responseGetManagedPrefixListEntries :: GetManagedPrefixListEntriesResponse -> TestTree
responseGetManagedPrefixListEntries =
  res
    "GetManagedPrefixListEntriesResponse"
    "fixture/GetManagedPrefixListEntriesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetManagedPrefixListEntries)

responseRunInstances :: Reservation -> TestTree
responseRunInstances =
  res
    "RunInstancesResponse"
    "fixture/RunInstancesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RunInstances)

responseCreateSnapshots :: CreateSnapshotsResponse -> TestTree
responseCreateSnapshots =
  res
    "CreateSnapshotsResponse"
    "fixture/CreateSnapshotsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateSnapshots)

responseAssociateDhcpOptions :: AssociateDhcpOptionsResponse -> TestTree
responseAssociateDhcpOptions =
  res
    "AssociateDhcpOptionsResponse"
    "fixture/AssociateDhcpOptionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AssociateDhcpOptions)

responseDeleteTrafficMirrorFilterRule :: DeleteTrafficMirrorFilterRuleResponse -> TestTree
responseDeleteTrafficMirrorFilterRule =
  res
    "DeleteTrafficMirrorFilterRuleResponse"
    "fixture/DeleteTrafficMirrorFilterRuleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteTrafficMirrorFilterRule)

responseDescribeReservedInstances :: DescribeReservedInstancesResponse -> TestTree
responseDescribeReservedInstances =
  res
    "DescribeReservedInstancesResponse"
    "fixture/DescribeReservedInstancesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeReservedInstances)

responseDescribeIdFormat :: DescribeIdFormatResponse -> TestTree
responseDescribeIdFormat =
  res
    "DescribeIdFormatResponse"
    "fixture/DescribeIdFormatResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeIdFormat)

responseDescribeVpcs :: DescribeVpcsResponse -> TestTree
responseDescribeVpcs =
  res
    "DescribeVpcsResponse"
    "fixture/DescribeVpcsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeVpcs)

responseDescribeConversionTasks :: DescribeConversionTasksResponse -> TestTree
responseDescribeConversionTasks =
  res
    "DescribeConversionTasksResponse"
    "fixture/DescribeConversionTasksResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeConversionTasks)

responseCreateLaunchTemplateVersion :: CreateLaunchTemplateVersionResponse -> TestTree
responseCreateLaunchTemplateVersion =
  res
    "CreateLaunchTemplateVersionResponse"
    "fixture/CreateLaunchTemplateVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateLaunchTemplateVersion)

responseGetManagedPrefixListAssociations :: GetManagedPrefixListAssociationsResponse -> TestTree
responseGetManagedPrefixListAssociations =
  res
    "GetManagedPrefixListAssociationsResponse"
    "fixture/GetManagedPrefixListAssociationsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetManagedPrefixListAssociations)

responseDisableVpcClassicLinkDnsSupport :: DisableVpcClassicLinkDnsSupportResponse -> TestTree
responseDisableVpcClassicLinkDnsSupport =
  res
    "DisableVpcClassicLinkDnsSupportResponse"
    "fixture/DisableVpcClassicLinkDnsSupportResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisableVpcClassicLinkDnsSupport)

responseApplySecurityGroupsToClientVpnTargetNetwork :: ApplySecurityGroupsToClientVpnTargetNetworkResponse -> TestTree
responseApplySecurityGroupsToClientVpnTargetNetwork =
  res
    "ApplySecurityGroupsToClientVpnTargetNetworkResponse"
    "fixture/ApplySecurityGroupsToClientVpnTargetNetworkResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ApplySecurityGroupsToClientVpnTargetNetwork)

responseDescribeTrafficMirrorTargets :: DescribeTrafficMirrorTargetsResponse -> TestTree
responseDescribeTrafficMirrorTargets =
  res
    "DescribeTrafficMirrorTargetsResponse"
    "fixture/DescribeTrafficMirrorTargetsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeTrafficMirrorTargets)

responseDescribeVolumesModifications :: DescribeVolumesModificationsResponse -> TestTree
responseDescribeVolumesModifications =
  res
    "DescribeVolumesModificationsResponse"
    "fixture/DescribeVolumesModificationsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeVolumesModifications)

responseExportImage :: ExportImageResponse -> TestTree
responseExportImage =
  res
    "ExportImageResponse"
    "fixture/ExportImageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ExportImage)

responseCreateFpgaImage :: CreateFpgaImageResponse -> TestTree
responseCreateFpgaImage =
  res
    "CreateFpgaImageResponse"
    "fixture/CreateFpgaImageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateFpgaImage)

responseAcceptVpcEndpointConnections :: AcceptVpcEndpointConnectionsResponse -> TestTree
responseAcceptVpcEndpointConnections =
  res
    "AcceptVpcEndpointConnectionsResponse"
    "fixture/AcceptVpcEndpointConnectionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AcceptVpcEndpointConnections)

responseDeleteClientVpnEndpoint :: DeleteClientVpnEndpointResponse -> TestTree
responseDeleteClientVpnEndpoint =
  res
    "DeleteClientVpnEndpointResponse"
    "fixture/DeleteClientVpnEndpointResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteClientVpnEndpoint)

responseSearchTransitGatewayRoutes :: SearchTransitGatewayRoutesResponse -> TestTree
responseSearchTransitGatewayRoutes =
  res
    "SearchTransitGatewayRoutesResponse"
    "fixture/SearchTransitGatewayRoutesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SearchTransitGatewayRoutes)

responseGetLaunchTemplateData :: GetLaunchTemplateDataResponse -> TestTree
responseGetLaunchTemplateData =
  res
    "GetLaunchTemplateDataResponse"
    "fixture/GetLaunchTemplateDataResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetLaunchTemplateData)

responseAllocateAddress :: AllocateAddressResponse -> TestTree
responseAllocateAddress =
  res
    "AllocateAddressResponse"
    "fixture/AllocateAddressResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AllocateAddress)

responseAcceptTransitGatewayVpcAttachment :: AcceptTransitGatewayVpcAttachmentResponse -> TestTree
responseAcceptTransitGatewayVpcAttachment =
  res
    "AcceptTransitGatewayVpcAttachmentResponse"
    "fixture/AcceptTransitGatewayVpcAttachmentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AcceptTransitGatewayVpcAttachment)

responseCancelConversionTask :: CancelConversionTaskResponse -> TestTree
responseCancelConversionTask =
  res
    "CancelConversionTaskResponse"
    "fixture/CancelConversionTaskResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CancelConversionTask)

responseModifyImageAttribute :: ModifyImageAttributeResponse -> TestTree
responseModifyImageAttribute =
  res
    "ModifyImageAttributeResponse"
    "fixture/ModifyImageAttributeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyImageAttribute)

responseCreateRouteTable :: CreateRouteTableResponse -> TestTree
responseCreateRouteTable =
  res
    "CreateRouteTableResponse"
    "fixture/CreateRouteTableResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateRouteTable)

responseRejectTransitGatewayPeeringAttachment :: RejectTransitGatewayPeeringAttachmentResponse -> TestTree
responseRejectTransitGatewayPeeringAttachment =
  res
    "RejectTransitGatewayPeeringAttachmentResponse"
    "fixture/RejectTransitGatewayPeeringAttachmentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RejectTransitGatewayPeeringAttachment)

responseReportInstanceStatus :: ReportInstanceStatusResponse -> TestTree
responseReportInstanceStatus =
  res
    "ReportInstanceStatusResponse"
    "fixture/ReportInstanceStatusResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ReportInstanceStatus)

responseAttachVolume :: VolumeAttachment -> TestTree
responseAttachVolume =
  res
    "AttachVolumeResponse"
    "fixture/AttachVolumeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AttachVolume)

responseRequestSpotInstances :: RequestSpotInstancesResponse -> TestTree
responseRequestSpotInstances =
  res
    "RequestSpotInstancesResponse"
    "fixture/RequestSpotInstancesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RequestSpotInstances)

responseWithdrawByoipCidr :: WithdrawByoipCidrResponse -> TestTree
responseWithdrawByoipCidr =
  res
    "WithdrawByoipCidrResponse"
    "fixture/WithdrawByoipCidrResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy WithdrawByoipCidr)

responseDescribeHostReservationOfferings :: DescribeHostReservationOfferingsResponse -> TestTree
responseDescribeHostReservationOfferings =
  res
    "DescribeHostReservationOfferingsResponse"
    "fixture/DescribeHostReservationOfferingsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeHostReservationOfferings)

responseResetFpgaImageAttribute :: ResetFpgaImageAttributeResponse -> TestTree
responseResetFpgaImageAttribute =
  res
    "ResetFpgaImageAttributeResponse"
    "fixture/ResetFpgaImageAttributeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ResetFpgaImageAttribute)

responseModifyVpnConnection :: ModifyVpnConnectionResponse -> TestTree
responseModifyVpnConnection =
  res
    "ModifyVpnConnectionResponse"
    "fixture/ModifyVpnConnectionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyVpnConnection)

responseCreateTrafficMirrorFilterRule :: CreateTrafficMirrorFilterRuleResponse -> TestTree
responseCreateTrafficMirrorFilterRule =
  res
    "CreateTrafficMirrorFilterRuleResponse"
    "fixture/CreateTrafficMirrorFilterRuleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateTrafficMirrorFilterRule)

responseDeleteTransitGateway :: DeleteTransitGatewayResponse -> TestTree
responseDeleteTransitGateway =
  res
    "DeleteTransitGatewayResponse"
    "fixture/DeleteTransitGatewayResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteTransitGateway)

responseStartVpcEndpointServicePrivateDnsVerification :: StartVpcEndpointServicePrivateDnsVerificationResponse -> TestTree
responseStartVpcEndpointServicePrivateDnsVerification =
  res
    "StartVpcEndpointServicePrivateDnsVerificationResponse"
    "fixture/StartVpcEndpointServicePrivateDnsVerificationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartVpcEndpointServicePrivateDnsVerification)

responseDescribeVolumes :: DescribeVolumesResponse -> TestTree
responseDescribeVolumes =
  res
    "DescribeVolumesResponse"
    "fixture/DescribeVolumesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeVolumes)

responseRejectVpcPeeringConnection :: RejectVpcPeeringConnectionResponse -> TestTree
responseRejectVpcPeeringConnection =
  res
    "RejectVpcPeeringConnectionResponse"
    "fixture/RejectVpcPeeringConnectionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RejectVpcPeeringConnection)

responseDescribeClientVpnRoutes :: DescribeClientVpnRoutesResponse -> TestTree
responseDescribeClientVpnRoutes =
  res
    "DescribeClientVpnRoutesResponse"
    "fixture/DescribeClientVpnRoutesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeClientVpnRoutes)

responseDeleteVpnConnectionRoute :: DeleteVpnConnectionRouteResponse -> TestTree
responseDeleteVpnConnectionRoute =
  res
    "DeleteVpnConnectionRouteResponse"
    "fixture/DeleteVpnConnectionRouteResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteVpnConnectionRoute)

responseAssociateEnclaveCertificateIamRole :: AssociateEnclaveCertificateIamRoleResponse -> TestTree
responseAssociateEnclaveCertificateIamRole =
  res
    "AssociateEnclaveCertificateIamRoleResponse"
    "fixture/AssociateEnclaveCertificateIamRoleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AssociateEnclaveCertificateIamRole)

responseModifyVpcEndpoint :: ModifyVpcEndpointResponse -> TestTree
responseModifyVpcEndpoint =
  res
    "ModifyVpcEndpointResponse"
    "fixture/ModifyVpcEndpointResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyVpcEndpoint)

responseDescribeFpgaImageAttribute :: DescribeFpgaImageAttributeResponse -> TestTree
responseDescribeFpgaImageAttribute =
  res
    "DescribeFpgaImageAttributeResponse"
    "fixture/DescribeFpgaImageAttributeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeFpgaImageAttribute)

responseAllocateHosts :: AllocateHostsResponse -> TestTree
responseAllocateHosts =
  res
    "AllocateHostsResponse"
    "fixture/AllocateHostsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AllocateHosts)

responseCreateClientVpnEndpoint :: CreateClientVpnEndpointResponse -> TestTree
responseCreateClientVpnEndpoint =
  res
    "CreateClientVpnEndpointResponse"
    "fixture/CreateClientVpnEndpointResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateClientVpnEndpoint)

responseCreateTrafficMirrorSession :: CreateTrafficMirrorSessionResponse -> TestTree
responseCreateTrafficMirrorSession =
  res
    "CreateTrafficMirrorSessionResponse"
    "fixture/CreateTrafficMirrorSessionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateTrafficMirrorSession)

responseRegisterImage :: RegisterImageResponse -> TestTree
responseRegisterImage =
  res
    "RegisterImageResponse"
    "fixture/RegisterImageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RegisterImage)

responseAdvertiseByoipCidr :: AdvertiseByoipCidrResponse -> TestTree
responseAdvertiseByoipCidr =
  res
    "AdvertiseByoipCidrResponse"
    "fixture/AdvertiseByoipCidrResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AdvertiseByoipCidr)

responseModifyFleet :: ModifyFleetResponse -> TestTree
responseModifyFleet =
  res
    "ModifyFleetResponse"
    "fixture/ModifyFleetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyFleet)

responseRevokeSecurityGroupIngress :: RevokeSecurityGroupIngressResponse -> TestTree
responseRevokeSecurityGroupIngress =
  res
    "RevokeSecurityGroupIngressResponse"
    "fixture/RevokeSecurityGroupIngressResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RevokeSecurityGroupIngress)

responseGetEbsDefaultKmsKeyId :: GetEbsDefaultKmsKeyIdResponse -> TestTree
responseGetEbsDefaultKmsKeyId =
  res
    "GetEbsDefaultKmsKeyIdResponse"
    "fixture/GetEbsDefaultKmsKeyIdResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetEbsDefaultKmsKeyId)

responseDescribeHostReservations :: DescribeHostReservationsResponse -> TestTree
responseDescribeHostReservations =
  res
    "DescribeHostReservationsResponse"
    "fixture/DescribeHostReservationsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeHostReservations)

responseUpdateSecurityGroupRuleDescriptionsEgress :: UpdateSecurityGroupRuleDescriptionsEgressResponse -> TestTree
responseUpdateSecurityGroupRuleDescriptionsEgress =
  res
    "UpdateSecurityGroupRuleDescriptionsEgressResponse"
    "fixture/UpdateSecurityGroupRuleDescriptionsEgressResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateSecurityGroupRuleDescriptionsEgress)

responseEnableVpcClassicLinkDnsSupport :: EnableVpcClassicLinkDnsSupportResponse -> TestTree
responseEnableVpcClassicLinkDnsSupport =
  res
    "EnableVpcClassicLinkDnsSupportResponse"
    "fixture/EnableVpcClassicLinkDnsSupportResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy EnableVpcClassicLinkDnsSupport)

responseDescribeVpcEndpointConnections :: DescribeVpcEndpointConnectionsResponse -> TestTree
responseDescribeVpcEndpointConnections =
  res
    "DescribeVpcEndpointConnectionsResponse"
    "fixture/DescribeVpcEndpointConnectionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeVpcEndpointConnections)

responseModifyReservedInstances :: ModifyReservedInstancesResponse -> TestTree
responseModifyReservedInstances =
  res
    "ModifyReservedInstancesResponse"
    "fixture/ModifyReservedInstancesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyReservedInstances)

responseDeleteFpgaImage :: DeleteFpgaImageResponse -> TestTree
responseDeleteFpgaImage =
  res
    "DeleteFpgaImageResponse"
    "fixture/DeleteFpgaImageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteFpgaImage)

responseDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations :: DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse -> TestTree
responseDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations =
  res
    "DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse"
    "fixture/DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations)

responseDescribeScheduledInstances :: DescribeScheduledInstancesResponse -> TestTree
responseDescribeScheduledInstances =
  res
    "DescribeScheduledInstancesResponse"
    "fixture/DescribeScheduledInstancesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeScheduledInstances)

responseSearchTransitGatewayMulticastGroups :: SearchTransitGatewayMulticastGroupsResponse -> TestTree
responseSearchTransitGatewayMulticastGroups =
  res
    "SearchTransitGatewayMulticastGroupsResponse"
    "fixture/SearchTransitGatewayMulticastGroupsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SearchTransitGatewayMulticastGroups)

responseCreateFlowLogs :: CreateFlowLogsResponse -> TestTree
responseCreateFlowLogs =
  res
    "CreateFlowLogsResponse"
    "fixture/CreateFlowLogsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateFlowLogs)

responseDescribeSpotFleetRequests :: DescribeSpotFleetRequestsResponse -> TestTree
responseDescribeSpotFleetRequests =
  res
    "DescribeSpotFleetRequestsResponse"
    "fixture/DescribeSpotFleetRequestsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeSpotFleetRequests)

responseMoveAddressToVpc :: MoveAddressToVpcResponse -> TestTree
responseMoveAddressToVpc =
  res
    "MoveAddressToVpcResponse"
    "fixture/MoveAddressToVpcResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy MoveAddressToVpc)

responseDescribeFleetInstances :: DescribeFleetInstancesResponse -> TestTree
responseDescribeFleetInstances =
  res
    "DescribeFleetInstancesResponse"
    "fixture/DescribeFleetInstancesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeFleetInstances)

responseDescribeLaunchTemplateVersions :: DescribeLaunchTemplateVersionsResponse -> TestTree
responseDescribeLaunchTemplateVersions =
  res
    "DescribeLaunchTemplateVersionsResponse"
    "fixture/DescribeLaunchTemplateVersionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeLaunchTemplateVersions)

responseModifyInstanceCreditSpecification :: ModifyInstanceCreditSpecificationResponse -> TestTree
responseModifyInstanceCreditSpecification =
  res
    "ModifyInstanceCreditSpecificationResponse"
    "fixture/ModifyInstanceCreditSpecificationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyInstanceCreditSpecification)

responseDescribePrincipalIdFormat :: DescribePrincipalIdFormatResponse -> TestTree
responseDescribePrincipalIdFormat =
  res
    "DescribePrincipalIdFormatResponse"
    "fixture/DescribePrincipalIdFormatResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribePrincipalIdFormat)

responseDescribeTransitGateways :: DescribeTransitGatewaysResponse -> TestTree
responseDescribeTransitGateways =
  res
    "DescribeTransitGatewaysResponse"
    "fixture/DescribeTransitGatewaysResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeTransitGateways)

responseDeleteNetworkAcl :: DeleteNetworkAclResponse -> TestTree
responseDeleteNetworkAcl =
  res
    "DeleteNetworkAclResponse"
    "fixture/DeleteNetworkAclResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteNetworkAcl)

responseDisassociateTransitGatewayMulticastDomain :: DisassociateTransitGatewayMulticastDomainResponse -> TestTree
responseDisassociateTransitGatewayMulticastDomain =
  res
    "DisassociateTransitGatewayMulticastDomainResponse"
    "fixture/DisassociateTransitGatewayMulticastDomainResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisassociateTransitGatewayMulticastDomain)

responseDeleteTransitGatewayRouteTable :: DeleteTransitGatewayRouteTableResponse -> TestTree
responseDeleteTransitGatewayRouteTable =
  res
    "DeleteTransitGatewayRouteTableResponse"
    "fixture/DeleteTransitGatewayRouteTableResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteTransitGatewayRouteTable)

responseCreateLaunchTemplate :: CreateLaunchTemplateResponse -> TestTree
responseCreateLaunchTemplate =
  res
    "CreateLaunchTemplateResponse"
    "fixture/CreateLaunchTemplateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateLaunchTemplate)

responseCreateVpcEndpointConnectionNotification :: CreateVpcEndpointConnectionNotificationResponse -> TestTree
responseCreateVpcEndpointConnectionNotification =
  res
    "CreateVpcEndpointConnectionNotificationResponse"
    "fixture/CreateVpcEndpointConnectionNotificationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateVpcEndpointConnectionNotification)

responseDeleteNetworkInterfacePermission :: DeleteNetworkInterfacePermissionResponse -> TestTree
responseDeleteNetworkInterfacePermission =
  res
    "DeleteNetworkInterfacePermissionResponse"
    "fixture/DeleteNetworkInterfacePermissionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteNetworkInterfacePermission)

responseDeleteVpnGateway :: DeleteVpnGatewayResponse -> TestTree
responseDeleteVpnGateway =
  res
    "DeleteVpnGatewayResponse"
    "fixture/DeleteVpnGatewayResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteVpnGateway)

responseCreateTrafficMirrorTarget :: CreateTrafficMirrorTargetResponse -> TestTree
responseCreateTrafficMirrorTarget =
  res
    "CreateTrafficMirrorTargetResponse"
    "fixture/CreateTrafficMirrorTargetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateTrafficMirrorTarget)

responseDescribeImportImageTasks :: DescribeImportImageTasksResponse -> TestTree
responseDescribeImportImageTasks =
  res
    "DescribeImportImageTasksResponse"
    "fixture/DescribeImportImageTasksResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeImportImageTasks)

responseDescribeVolumeAttribute :: DescribeVolumeAttributeResponse -> TestTree
responseDescribeVolumeAttribute =
  res
    "DescribeVolumeAttributeResponse"
    "fixture/DescribeVolumeAttributeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeVolumeAttribute)

responseDescribeMovingAddresses :: DescribeMovingAddressesResponse -> TestTree
responseDescribeMovingAddresses =
  res
    "DescribeMovingAddressesResponse"
    "fixture/DescribeMovingAddressesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeMovingAddresses)

responseExportTransitGatewayRoutes :: ExportTransitGatewayRoutesResponse -> TestTree
responseExportTransitGatewayRoutes =
  res
    "ExportTransitGatewayRoutesResponse"
    "fixture/ExportTransitGatewayRoutesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ExportTransitGatewayRoutes)

responseGetPasswordData :: GetPasswordDataResponse -> TestTree
responseGetPasswordData =
  res
    "GetPasswordDataResponse"
    "fixture/GetPasswordDataResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetPasswordData)

responseCreateVpc :: CreateVpcResponse -> TestTree
responseCreateVpc =
  res
    "CreateVpcResponse"
    "fixture/CreateVpcResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateVpc)

responseModifyVpcPeeringConnectionOptions :: ModifyVpcPeeringConnectionOptionsResponse -> TestTree
responseModifyVpcPeeringConnectionOptions =
  res
    "ModifyVpcPeeringConnectionOptionsResponse"
    "fixture/ModifyVpcPeeringConnectionOptionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyVpcPeeringConnectionOptions)

responseDescribeFpgaImages :: DescribeFpgaImagesResponse -> TestTree
responseDescribeFpgaImages =
  res
    "DescribeFpgaImagesResponse"
    "fixture/DescribeFpgaImagesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeFpgaImages)

responseCopySnapshot :: CopySnapshotResponse -> TestTree
responseCopySnapshot =
  res
    "CopySnapshotResponse"
    "fixture/CopySnapshotResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CopySnapshot)

responseAcceptTransitGatewayPeeringAttachment :: AcceptTransitGatewayPeeringAttachmentResponse -> TestTree
responseAcceptTransitGatewayPeeringAttachment =
  res
    "AcceptTransitGatewayPeeringAttachmentResponse"
    "fixture/AcceptTransitGatewayPeeringAttachmentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AcceptTransitGatewayPeeringAttachment)

responseDisassociateAddress :: DisassociateAddressResponse -> TestTree
responseDisassociateAddress =
  res
    "DisassociateAddressResponse"
    "fixture/DisassociateAddressResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisassociateAddress)

responseModifyTrafficMirrorFilterNetworkServices :: ModifyTrafficMirrorFilterNetworkServicesResponse -> TestTree
responseModifyTrafficMirrorFilterNetworkServices =
  res
    "ModifyTrafficMirrorFilterNetworkServicesResponse"
    "fixture/ModifyTrafficMirrorFilterNetworkServicesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyTrafficMirrorFilterNetworkServices)

responseDescribeEgressOnlyInternetGateways :: DescribeEgressOnlyInternetGatewaysResponse -> TestTree
responseDescribeEgressOnlyInternetGateways =
  res
    "DescribeEgressOnlyInternetGatewaysResponse"
    "fixture/DescribeEgressOnlyInternetGatewaysResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeEgressOnlyInternetGateways)

responseDeleteVpc :: DeleteVpcResponse -> TestTree
responseDeleteVpc =
  res
    "DeleteVpcResponse"
    "fixture/DeleteVpcResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteVpc)

responseCreateInstanceExportTask :: CreateInstanceExportTaskResponse -> TestTree
responseCreateInstanceExportTask =
  res
    "CreateInstanceExportTaskResponse"
    "fixture/CreateInstanceExportTaskResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateInstanceExportTask)

responseRejectTransitGatewayVpcAttachment :: RejectTransitGatewayVpcAttachmentResponse -> TestTree
responseRejectTransitGatewayVpcAttachment =
  res
    "RejectTransitGatewayVpcAttachmentResponse"
    "fixture/RejectTransitGatewayVpcAttachmentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RejectTransitGatewayVpcAttachment)

responseDescribeTrafficMirrorSessions :: DescribeTrafficMirrorSessionsResponse -> TestTree
responseDescribeTrafficMirrorSessions =
  res
    "DescribeTrafficMirrorSessionsResponse"
    "fixture/DescribeTrafficMirrorSessionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeTrafficMirrorSessions)

responseGetTransitGatewayRouteTableAssociations :: GetTransitGatewayRouteTableAssociationsResponse -> TestTree
responseGetTransitGatewayRouteTableAssociations =
  res
    "GetTransitGatewayRouteTableAssociationsResponse"
    "fixture/GetTransitGatewayRouteTableAssociationsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetTransitGatewayRouteTableAssociations)

responseAssociateVpcCidrBlock :: AssociateVpcCidrBlockResponse -> TestTree
responseAssociateVpcCidrBlock =
  res
    "AssociateVpcCidrBlockResponse"
    "fixture/AssociateVpcCidrBlockResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AssociateVpcCidrBlock)

responseDescribeVpcAttribute :: DescribeVpcAttributeResponse -> TestTree
responseDescribeVpcAttribute =
  res
    "DescribeVpcAttributeResponse"
    "fixture/DescribeVpcAttributeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeVpcAttribute)

responseCreateVolume :: Volume -> TestTree
responseCreateVolume =
  res
    "CreateVolumeResponse"
    "fixture/CreateVolumeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateVolume)

responseCreateDefaultSubnet :: CreateDefaultSubnetResponse -> TestTree
responseCreateDefaultSubnet =
  res
    "CreateDefaultSubnetResponse"
    "fixture/CreateDefaultSubnetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateDefaultSubnet)

responseDescribeScheduledInstanceAvailability :: DescribeScheduledInstanceAvailabilityResponse -> TestTree
responseDescribeScheduledInstanceAvailability =
  res
    "DescribeScheduledInstanceAvailabilityResponse"
    "fixture/DescribeScheduledInstanceAvailabilityResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeScheduledInstanceAvailability)

responseDisassociateClientVpnTargetNetwork :: DisassociateClientVpnTargetNetworkResponse -> TestTree
responseDisassociateClientVpnTargetNetwork =
  res
    "DisassociateClientVpnTargetNetworkResponse"
    "fixture/DisassociateClientVpnTargetNetworkResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisassociateClientVpnTargetNetwork)

responseCreateClientVpnRoute :: CreateClientVpnRouteResponse -> TestTree
responseCreateClientVpnRoute =
  res
    "CreateClientVpnRouteResponse"
    "fixture/CreateClientVpnRouteResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateClientVpnRoute)

responseModifyVolumeAttribute :: ModifyVolumeAttributeResponse -> TestTree
responseModifyVolumeAttribute =
  res
    "ModifyVolumeAttributeResponse"
    "fixture/ModifyVolumeAttributeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyVolumeAttribute)

responseExportClientVpnClientConfiguration :: ExportClientVpnClientConfigurationResponse -> TestTree
responseExportClientVpnClientConfiguration =
  res
    "ExportClientVpnClientConfigurationResponse"
    "fixture/ExportClientVpnClientConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ExportClientVpnClientConfiguration)

responseDeleteTrafficMirrorTarget :: DeleteTrafficMirrorTargetResponse -> TestTree
responseDeleteTrafficMirrorTarget =
  res
    "DeleteTrafficMirrorTargetResponse"
    "fixture/DeleteTrafficMirrorTargetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteTrafficMirrorTarget)

responseDescribeSpotDatafeedSubscription :: DescribeSpotDatafeedSubscriptionResponse -> TestTree
responseDescribeSpotDatafeedSubscription =
  res
    "DescribeSpotDatafeedSubscriptionResponse"
    "fixture/DescribeSpotDatafeedSubscriptionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeSpotDatafeedSubscription)

responseDescribeLocalGatewayRouteTables :: DescribeLocalGatewayRouteTablesResponse -> TestTree
responseDescribeLocalGatewayRouteTables =
  res
    "DescribeLocalGatewayRouteTablesResponse"
    "fixture/DescribeLocalGatewayRouteTablesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeLocalGatewayRouteTables)

responseDescribePrefixLists :: DescribePrefixListsResponse -> TestTree
responseDescribePrefixLists =
  res
    "DescribePrefixListsResponse"
    "fixture/DescribePrefixListsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribePrefixLists)

responseAssociateTransitGatewayRouteTable :: AssociateTransitGatewayRouteTableResponse -> TestTree
responseAssociateTransitGatewayRouteTable =
  res
    "AssociateTransitGatewayRouteTableResponse"
    "fixture/AssociateTransitGatewayRouteTableResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AssociateTransitGatewayRouteTable)

responseDeletePlacementGroup :: DeletePlacementGroupResponse -> TestTree
responseDeletePlacementGroup =
  res
    "DeletePlacementGroupResponse"
    "fixture/DeletePlacementGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeletePlacementGroup)

responseModifyTransitGateway :: ModifyTransitGatewayResponse -> TestTree
responseModifyTransitGateway =
  res
    "ModifyTransitGatewayResponse"
    "fixture/ModifyTransitGatewayResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyTransitGateway)

responseDeleteTransitGatewayPrefixListReference :: DeleteTransitGatewayPrefixListReferenceResponse -> TestTree
responseDeleteTransitGatewayPrefixListReference =
  res
    "DeleteTransitGatewayPrefixListReferenceResponse"
    "fixture/DeleteTransitGatewayPrefixListReferenceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteTransitGatewayPrefixListReference)

responseCreateTransitGatewayMulticastDomain :: CreateTransitGatewayMulticastDomainResponse -> TestTree
responseCreateTransitGatewayMulticastDomain =
  res
    "CreateTransitGatewayMulticastDomainResponse"
    "fixture/CreateTransitGatewayMulticastDomainResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateTransitGatewayMulticastDomain)

responseDeregisterInstanceEventNotificationAttributes :: DeregisterInstanceEventNotificationAttributesResponse -> TestTree
responseDeregisterInstanceEventNotificationAttributes =
  res
    "DeregisterInstanceEventNotificationAttributesResponse"
    "fixture/DeregisterInstanceEventNotificationAttributesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeregisterInstanceEventNotificationAttributes)

responseRequestSpotFleet :: RequestSpotFleetResponse -> TestTree
responseRequestSpotFleet =
  res
    "RequestSpotFleetResponse"
    "fixture/RequestSpotFleetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RequestSpotFleet)

responseDeleteTransitGatewayRoute :: DeleteTransitGatewayRouteResponse -> TestTree
responseDeleteTransitGatewayRoute =
  res
    "DeleteTransitGatewayRouteResponse"
    "fixture/DeleteTransitGatewayRouteResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteTransitGatewayRoute)

responseDisableEbsEncryptionByDefault :: DisableEbsEncryptionByDefaultResponse -> TestTree
responseDisableEbsEncryptionByDefault =
  res
    "DisableEbsEncryptionByDefaultResponse"
    "fixture/DisableEbsEncryptionByDefaultResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisableEbsEncryptionByDefault)

responseDeregisterTransitGatewayMulticastGroupMembers :: DeregisterTransitGatewayMulticastGroupMembersResponse -> TestTree
responseDeregisterTransitGatewayMulticastGroupMembers =
  res
    "DeregisterTransitGatewayMulticastGroupMembersResponse"
    "fixture/DeregisterTransitGatewayMulticastGroupMembersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeregisterTransitGatewayMulticastGroupMembers)

responseCreateSubnet :: CreateSubnetResponse -> TestTree
responseCreateSubnet =
  res
    "CreateSubnetResponse"
    "fixture/CreateSubnetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateSubnet)

responseCreateNetworkInterface :: CreateNetworkInterfaceResponse -> TestTree
responseCreateNetworkInterface =
  res
    "CreateNetworkInterfaceResponse"
    "fixture/CreateNetworkInterfaceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateNetworkInterface)

responseDescribeSecurityGroups :: DescribeSecurityGroupsResponse -> TestTree
responseDescribeSecurityGroups =
  res
    "DescribeSecurityGroupsResponse"
    "fixture/DescribeSecurityGroupsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeSecurityGroups)

responseGetCapacityReservationUsage :: GetCapacityReservationUsageResponse -> TestTree
responseGetCapacityReservationUsage =
  res
    "GetCapacityReservationUsageResponse"
    "fixture/GetCapacityReservationUsageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetCapacityReservationUsage)

responseCreateTransitGatewayVpcAttachment :: CreateTransitGatewayVpcAttachmentResponse -> TestTree
responseCreateTransitGatewayVpcAttachment =
  res
    "CreateTransitGatewayVpcAttachmentResponse"
    "fixture/CreateTransitGatewayVpcAttachmentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateTransitGatewayVpcAttachment)

responseDescribeExportTasks :: DescribeExportTasksResponse -> TestTree
responseDescribeExportTasks =
  res
    "DescribeExportTasksResponse"
    "fixture/DescribeExportTasksResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeExportTasks)

responseModifySpotFleetRequest :: ModifySpotFleetRequestResponse -> TestTree
responseModifySpotFleetRequest =
  res
    "ModifySpotFleetRequestResponse"
    "fixture/ModifySpotFleetRequestResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifySpotFleetRequest)

responseDetachVpnGateway :: DetachVpnGatewayResponse -> TestTree
responseDetachVpnGateway =
  res
    "DetachVpnGatewayResponse"
    "fixture/DetachVpnGatewayResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DetachVpnGateway)

responseModifyManagedPrefixList :: ModifyManagedPrefixListResponse -> TestTree
responseModifyManagedPrefixList =
  res
    "ModifyManagedPrefixListResponse"
    "fixture/ModifyManagedPrefixListResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyManagedPrefixList)

responseGetHostReservationPurchasePreview :: GetHostReservationPurchasePreviewResponse -> TestTree
responseGetHostReservationPurchasePreview =
  res
    "GetHostReservationPurchasePreviewResponse"
    "fixture/GetHostReservationPurchasePreviewResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetHostReservationPurchasePreview)

responseEnableVolumeIO :: EnableVolumeIOResponse -> TestTree
responseEnableVolumeIO =
  res
    "EnableVolumeIOResponse"
    "fixture/EnableVolumeIOResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy EnableVolumeIO)

responseDescribeInstances :: DescribeInstancesResponse -> TestTree
responseDescribeInstances =
  res
    "DescribeInstancesResponse"
    "fixture/DescribeInstancesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeInstances)

responseCreateNatGateway :: CreateNatGatewayResponse -> TestTree
responseCreateNatGateway =
  res
    "CreateNatGatewayResponse"
    "fixture/CreateNatGatewayResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateNatGateway)

responseDescribeLocalGatewayVirtualInterfaces :: DescribeLocalGatewayVirtualInterfacesResponse -> TestTree
responseDescribeLocalGatewayVirtualInterfaces =
  res
    "DescribeLocalGatewayVirtualInterfacesResponse"
    "fixture/DescribeLocalGatewayVirtualInterfacesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeLocalGatewayVirtualInterfaces)

responseDescribeVpcPeeringConnections :: DescribeVpcPeeringConnectionsResponse -> TestTree
responseDescribeVpcPeeringConnections =
  res
    "DescribeVpcPeeringConnectionsResponse"
    "fixture/DescribeVpcPeeringConnectionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeVpcPeeringConnections)

responseCancelExportTask :: CancelExportTaskResponse -> TestTree
responseCancelExportTask =
  res
    "CancelExportTaskResponse"
    "fixture/CancelExportTaskResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CancelExportTask)

responseCreateVpcEndpointServiceConfiguration :: CreateVpcEndpointServiceConfigurationResponse -> TestTree
responseCreateVpcEndpointServiceConfiguration =
  res
    "CreateVpcEndpointServiceConfigurationResponse"
    "fixture/CreateVpcEndpointServiceConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateVpcEndpointServiceConfiguration)

responseCreateDefaultVpc :: CreateDefaultVpcResponse -> TestTree
responseCreateDefaultVpc =
  res
    "CreateDefaultVpcResponse"
    "fixture/CreateDefaultVpcResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateDefaultVpc)

responseDisassociateVpcCidrBlock :: DisassociateVpcCidrBlockResponse -> TestTree
responseDisassociateVpcCidrBlock =
  res
    "DisassociateVpcCidrBlockResponse"
    "fixture/DisassociateVpcCidrBlockResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisassociateVpcCidrBlock)

responseDescribeTrafficMirrorFilters :: DescribeTrafficMirrorFiltersResponse -> TestTree
responseDescribeTrafficMirrorFilters =
  res
    "DescribeTrafficMirrorFiltersResponse"
    "fixture/DescribeTrafficMirrorFiltersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeTrafficMirrorFilters)

responseDescribeFastSnapshotRestores :: DescribeFastSnapshotRestoresResponse -> TestTree
responseDescribeFastSnapshotRestores =
  res
    "DescribeFastSnapshotRestoresResponse"
    "fixture/DescribeFastSnapshotRestoresResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeFastSnapshotRestores)

responseCancelCapacityReservation :: CancelCapacityReservationResponse -> TestTree
responseCancelCapacityReservation =
  res
    "CancelCapacityReservationResponse"
    "fixture/CancelCapacityReservationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CancelCapacityReservation)

responseDeleteNetworkInterface :: DeleteNetworkInterfaceResponse -> TestTree
responseDeleteNetworkInterface =
  res
    "DeleteNetworkInterfaceResponse"
    "fixture/DeleteNetworkInterfaceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteNetworkInterface)

responseDisassociateTransitGatewayRouteTable :: DisassociateTransitGatewayRouteTableResponse -> TestTree
responseDisassociateTransitGatewayRouteTable =
  res
    "DisassociateTransitGatewayRouteTableResponse"
    "fixture/DisassociateTransitGatewayRouteTableResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisassociateTransitGatewayRouteTable)

responseReplaceRouteTableAssociation :: ReplaceRouteTableAssociationResponse -> TestTree
responseReplaceRouteTableAssociation =
  res
    "ReplaceRouteTableAssociationResponse"
    "fixture/ReplaceRouteTableAssociationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ReplaceRouteTableAssociation)

responseStartInstances :: StartInstancesResponse -> TestTree
responseStartInstances =
  res
    "StartInstancesResponse"
    "fixture/StartInstancesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartInstances)

responseCreatePlacementGroup :: CreatePlacementGroupResponse -> TestTree
responseCreatePlacementGroup =
  res
    "CreatePlacementGroupResponse"
    "fixture/CreatePlacementGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreatePlacementGroup)

responseDescribeInstanceEventNotificationAttributes :: DescribeInstanceEventNotificationAttributesResponse -> TestTree
responseDescribeInstanceEventNotificationAttributes =
  res
    "DescribeInstanceEventNotificationAttributesResponse"
    "fixture/DescribeInstanceEventNotificationAttributesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeInstanceEventNotificationAttributes)

responseDescribeCapacityReservations :: DescribeCapacityReservationsResponse -> TestTree
responseDescribeCapacityReservations =
  res
    "DescribeCapacityReservationsResponse"
    "fixture/DescribeCapacityReservationsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeCapacityReservations)

responseModifyClientVpnEndpoint :: ModifyClientVpnEndpointResponse -> TestTree
responseModifyClientVpnEndpoint =
  res
    "ModifyClientVpnEndpointResponse"
    "fixture/ModifyClientVpnEndpointResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyClientVpnEndpoint)

responseModifyInstanceCapacityReservationAttributes :: ModifyInstanceCapacityReservationAttributesResponse -> TestTree
responseModifyInstanceCapacityReservationAttributes =
  res
    "ModifyInstanceCapacityReservationAttributesResponse"
    "fixture/ModifyInstanceCapacityReservationAttributesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyInstanceCapacityReservationAttributes)

responseDescribeAggregateIdFormat :: DescribeAggregateIdFormatResponse -> TestTree
responseDescribeAggregateIdFormat =
  res
    "DescribeAggregateIdFormatResponse"
    "fixture/DescribeAggregateIdFormatResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeAggregateIdFormat)

responseDescribeSnapshots :: DescribeSnapshotsResponse -> TestTree
responseDescribeSnapshots =
  res
    "DescribeSnapshotsResponse"
    "fixture/DescribeSnapshotsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeSnapshots)

responseAssociateAddress :: AssociateAddressResponse -> TestTree
responseAssociateAddress =
  res
    "AssociateAddressResponse"
    "fixture/AssociateAddressResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AssociateAddress)

responseModifyTrafficMirrorFilterRule :: ModifyTrafficMirrorFilterRuleResponse -> TestTree
responseModifyTrafficMirrorFilterRule =
  res
    "ModifyTrafficMirrorFilterRuleResponse"
    "fixture/ModifyTrafficMirrorFilterRuleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyTrafficMirrorFilterRule)

responseDescribeNetworkInterfaceAttribute :: DescribeNetworkInterfaceAttributeResponse -> TestTree
responseDescribeNetworkInterfaceAttribute =
  res
    "DescribeNetworkInterfaceAttributeResponse"
    "fixture/DescribeNetworkInterfaceAttributeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeNetworkInterfaceAttribute)

responseReplaceIamInstanceProfileAssociation :: ReplaceIamInstanceProfileAssociationResponse -> TestTree
responseReplaceIamInstanceProfileAssociation =
  res
    "ReplaceIamInstanceProfileAssociationResponse"
    "fixture/ReplaceIamInstanceProfileAssociationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ReplaceIamInstanceProfileAssociation)

responseAssociateClientVpnTargetNetwork :: AssociateClientVpnTargetNetworkResponse -> TestTree
responseAssociateClientVpnTargetNetwork =
  res
    "AssociateClientVpnTargetNetworkResponse"
    "fixture/AssociateClientVpnTargetNetworkResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AssociateClientVpnTargetNetwork)

responseReleaseHosts :: ReleaseHostsResponse -> TestTree
responseReleaseHosts =
  res
    "ReleaseHostsResponse"
    "fixture/ReleaseHostsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ReleaseHosts)

responseResetNetworkInterfaceAttribute :: ResetNetworkInterfaceAttributeResponse -> TestTree
responseResetNetworkInterfaceAttribute =
  res
    "ResetNetworkInterfaceAttributeResponse"
    "fixture/ResetNetworkInterfaceAttributeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ResetNetworkInterfaceAttribute)

responseDeleteInternetGateway :: DeleteInternetGatewayResponse -> TestTree
responseDeleteInternetGateway =
  res
    "DeleteInternetGatewayResponse"
    "fixture/DeleteInternetGatewayResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteInternetGateway)

responseDescribeReservedInstancesListings :: DescribeReservedInstancesListingsResponse -> TestTree
responseDescribeReservedInstancesListings =
  res
    "DescribeReservedInstancesListingsResponse"
    "fixture/DescribeReservedInstancesListingsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeReservedInstancesListings)

responseCreateVpnConnection :: CreateVpnConnectionResponse -> TestTree
responseCreateVpnConnection =
  res
    "CreateVpnConnectionResponse"
    "fixture/CreateVpnConnectionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateVpnConnection)

responseReplaceTransitGatewayRoute :: ReplaceTransitGatewayRouteResponse -> TestTree
responseReplaceTransitGatewayRoute =
  res
    "ReplaceTransitGatewayRouteResponse"
    "fixture/ReplaceTransitGatewayRouteResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ReplaceTransitGatewayRoute)

responseCreateFleet :: CreateFleetResponse -> TestTree
responseCreateFleet =
  res
    "CreateFleetResponse"
    "fixture/CreateFleetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateFleet)

responseDeleteNatGateway :: DeleteNatGatewayResponse -> TestTree
responseDeleteNatGateway =
  res
    "DeleteNatGatewayResponse"
    "fixture/DeleteNatGatewayResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteNatGateway)

responseDescribeImportSnapshotTasks :: DescribeImportSnapshotTasksResponse -> TestTree
responseDescribeImportSnapshotTasks =
  res
    "DescribeImportSnapshotTasksResponse"
    "fixture/DescribeImportSnapshotTasksResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeImportSnapshotTasks)

responseGetCoipPoolUsage :: GetCoipPoolUsageResponse -> TestTree
responseGetCoipPoolUsage =
  res
    "GetCoipPoolUsageResponse"
    "fixture/GetCoipPoolUsageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetCoipPoolUsage)

responseDescribeCustomerGateways :: DescribeCustomerGatewaysResponse -> TestTree
responseDescribeCustomerGateways =
  res
    "DescribeCustomerGatewaysResponse"
    "fixture/DescribeCustomerGatewaysResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeCustomerGateways)

responseDeleteSubnet :: DeleteSubnetResponse -> TestTree
responseDeleteSubnet =
  res
    "DeleteSubnetResponse"
    "fixture/DeleteSubnetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteSubnet)

responseCopyImage :: CopyImageResponse -> TestTree
responseCopyImage =
  res
    "CopyImageResponse"
    "fixture/CopyImageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CopyImage)

responseCreateVpcEndpoint :: CreateVpcEndpointResponse -> TestTree
responseCreateVpcEndpoint =
  res
    "CreateVpcEndpointResponse"
    "fixture/CreateVpcEndpointResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateVpcEndpoint)

responseModifyTrafficMirrorSession :: ModifyTrafficMirrorSessionResponse -> TestTree
responseModifyTrafficMirrorSession =
  res
    "ModifyTrafficMirrorSessionResponse"
    "fixture/ModifyTrafficMirrorSessionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyTrafficMirrorSession)

responseDescribeCarrierGateways :: DescribeCarrierGatewaysResponse -> TestTree
responseDescribeCarrierGateways =
  res
    "DescribeCarrierGatewaysResponse"
    "fixture/DescribeCarrierGatewaysResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeCarrierGateways)

responseDescribeTransitGatewayPeeringAttachments :: DescribeTransitGatewayPeeringAttachmentsResponse -> TestTree
responseDescribeTransitGatewayPeeringAttachments =
  res
    "DescribeTransitGatewayPeeringAttachmentsResponse"
    "fixture/DescribeTransitGatewayPeeringAttachmentsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeTransitGatewayPeeringAttachments)

responseDeleteQueuedReservedInstances :: DeleteQueuedReservedInstancesResponse -> TestTree
responseDeleteQueuedReservedInstances =
  res
    "DeleteQueuedReservedInstancesResponse"
    "fixture/DeleteQueuedReservedInstancesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteQueuedReservedInstances)

responseDescribeTransitGatewayMulticastDomains :: DescribeTransitGatewayMulticastDomainsResponse -> TestTree
responseDescribeTransitGatewayMulticastDomains =
  res
    "DescribeTransitGatewayMulticastDomainsResponse"
    "fixture/DescribeTransitGatewayMulticastDomainsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeTransitGatewayMulticastDomains)

responseGetDefaultCreditSpecification :: GetDefaultCreditSpecificationResponse -> TestTree
responseGetDefaultCreditSpecification =
  res
    "GetDefaultCreditSpecificationResponse"
    "fixture/GetDefaultCreditSpecificationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetDefaultCreditSpecification)

responseUnmonitorInstances :: UnmonitorInstancesResponse -> TestTree
responseUnmonitorInstances =
  res
    "UnmonitorInstancesResponse"
    "fixture/UnmonitorInstancesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UnmonitorInstances)

responseDescribeTransitGatewayVpcAttachments :: DescribeTransitGatewayVpcAttachmentsResponse -> TestTree
responseDescribeTransitGatewayVpcAttachments =
  res
    "DescribeTransitGatewayVpcAttachmentsResponse"
    "fixture/DescribeTransitGatewayVpcAttachmentsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeTransitGatewayVpcAttachments)

responseCreateSecurityGroup :: CreateSecurityGroupResponse -> TestTree
responseCreateSecurityGroup =
  res
    "CreateSecurityGroupResponse"
    "fixture/CreateSecurityGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateSecurityGroup)

responseGetEbsEncryptionByDefault :: GetEbsEncryptionByDefaultResponse -> TestTree
responseGetEbsEncryptionByDefault =
  res
    "GetEbsEncryptionByDefaultResponse"
    "fixture/GetEbsEncryptionByDefaultResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetEbsEncryptionByDefault)

responseImportVolume :: ImportVolumeResponse -> TestTree
responseImportVolume =
  res
    "ImportVolumeResponse"
    "fixture/ImportVolumeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ImportVolume)

responseDeleteCarrierGateway :: DeleteCarrierGatewayResponse -> TestTree
responseDeleteCarrierGateway =
  res
    "DeleteCarrierGatewayResponse"
    "fixture/DeleteCarrierGatewayResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteCarrierGateway)

responseDisableVgwRoutePropagation :: DisableVgwRoutePropagationResponse -> TestTree
responseDisableVgwRoutePropagation =
  res
    "DisableVgwRoutePropagationResponse"
    "fixture/DisableVgwRoutePropagationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisableVgwRoutePropagation)

responseDeleteTrafficMirrorFilter :: DeleteTrafficMirrorFilterResponse -> TestTree
responseDeleteTrafficMirrorFilter =
  res
    "DeleteTrafficMirrorFilterResponse"
    "fixture/DeleteTrafficMirrorFilterResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteTrafficMirrorFilter)

responseModifyVpnTunnelCertificate :: ModifyVpnTunnelCertificateResponse -> TestTree
responseModifyVpnTunnelCertificate =
  res
    "ModifyVpnTunnelCertificateResponse"
    "fixture/ModifyVpnTunnelCertificateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyVpnTunnelCertificate)

responseCreateSpotDatafeedSubscription :: CreateSpotDatafeedSubscriptionResponse -> TestTree
responseCreateSpotDatafeedSubscription =
  res
    "CreateSpotDatafeedSubscriptionResponse"
    "fixture/CreateSpotDatafeedSubscriptionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateSpotDatafeedSubscription)

responseCancelSpotInstanceRequests :: CancelSpotInstanceRequestsResponse -> TestTree
responseCancelSpotInstanceRequests =
  res
    "CancelSpotInstanceRequestsResponse"
    "fixture/CancelSpotInstanceRequestsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CancelSpotInstanceRequests)

responseCreateRoute :: CreateRouteResponse -> TestTree
responseCreateRoute =
  res
    "CreateRouteResponse"
    "fixture/CreateRouteResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateRoute)

responseDescribeVpcEndpointServiceConfigurations :: DescribeVpcEndpointServiceConfigurationsResponse -> TestTree
responseDescribeVpcEndpointServiceConfigurations =
  res
    "DescribeVpcEndpointServiceConfigurationsResponse"
    "fixture/DescribeVpcEndpointServiceConfigurationsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeVpcEndpointServiceConfigurations)

responseDeleteSnapshot :: DeleteSnapshotResponse -> TestTree
responseDeleteSnapshot =
  res
    "DeleteSnapshotResponse"
    "fixture/DeleteSnapshotResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteSnapshot)

responseAssignPrivateIpAddresses :: AssignPrivateIpAddressesResponse -> TestTree
responseAssignPrivateIpAddresses =
  res
    "AssignPrivateIpAddressesResponse"
    "fixture/AssignPrivateIpAddressesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AssignPrivateIpAddresses)

responseAuthorizeClientVpnIngress :: AuthorizeClientVpnIngressResponse -> TestTree
responseAuthorizeClientVpnIngress =
  res
    "AuthorizeClientVpnIngressResponse"
    "fixture/AuthorizeClientVpnIngressResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AuthorizeClientVpnIngress)

responseDeleteTransitGatewayPeeringAttachment :: DeleteTransitGatewayPeeringAttachmentResponse -> TestTree
responseDeleteTransitGatewayPeeringAttachment =
  res
    "DeleteTransitGatewayPeeringAttachmentResponse"
    "fixture/DeleteTransitGatewayPeeringAttachmentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteTransitGatewayPeeringAttachment)

responseModifyInstanceAttribute :: ModifyInstanceAttributeResponse -> TestTree
responseModifyInstanceAttribute =
  res
    "ModifyInstanceAttributeResponse"
    "fixture/ModifyInstanceAttributeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyInstanceAttribute)

responseDeleteCustomerGateway :: DeleteCustomerGatewayResponse -> TestTree
responseDeleteCustomerGateway =
  res
    "DeleteCustomerGatewayResponse"
    "fixture/DeleteCustomerGatewayResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteCustomerGateway)

responseDisassociateIamInstanceProfile :: DisassociateIamInstanceProfileResponse -> TestTree
responseDisassociateIamInstanceProfile =
  res
    "DisassociateIamInstanceProfileResponse"
    "fixture/DisassociateIamInstanceProfileResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisassociateIamInstanceProfile)

responseTerminateClientVpnConnections :: TerminateClientVpnConnectionsResponse -> TestTree
responseTerminateClientVpnConnections =
  res
    "TerminateClientVpnConnectionsResponse"
    "fixture/TerminateClientVpnConnectionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TerminateClientVpnConnections)

responseDisassociateRouteTable :: DisassociateRouteTableResponse -> TestTree
responseDisassociateRouteTable =
  res
    "DisassociateRouteTableResponse"
    "fixture/DisassociateRouteTableResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisassociateRouteTable)

responseGetConsoleScreenshot :: GetConsoleScreenshotResponse -> TestTree
responseGetConsoleScreenshot =
  res
    "GetConsoleScreenshotResponse"
    "fixture/GetConsoleScreenshotResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetConsoleScreenshot)

responseResetEbsDefaultKmsKeyId :: ResetEbsDefaultKmsKeyIdResponse -> TestTree
responseResetEbsDefaultKmsKeyId =
  res
    "ResetEbsDefaultKmsKeyIdResponse"
    "fixture/ResetEbsDefaultKmsKeyIdResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ResetEbsDefaultKmsKeyId)

responseAssignIpv6Addresses :: AssignIpv6AddressesResponse -> TestTree
responseAssignIpv6Addresses =
  res
    "AssignIpv6AddressesResponse"
    "fixture/AssignIpv6AddressesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AssignIpv6Addresses)

responseModifyVpnTunnelOptions :: ModifyVpnTunnelOptionsResponse -> TestTree
responseModifyVpnTunnelOptions =
  res
    "ModifyVpnTunnelOptionsResponse"
    "fixture/ModifyVpnTunnelOptionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyVpnTunnelOptions)

responseModifyEbsDefaultKmsKeyId :: ModifyEbsDefaultKmsKeyIdResponse -> TestTree
responseModifyEbsDefaultKmsKeyId =
  res
    "ModifyEbsDefaultKmsKeyIdResponse"
    "fixture/ModifyEbsDefaultKmsKeyIdResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyEbsDefaultKmsKeyId)

responseDeleteSpotDatafeedSubscription :: DeleteSpotDatafeedSubscriptionResponse -> TestTree
responseDeleteSpotDatafeedSubscription =
  res
    "DeleteSpotDatafeedSubscriptionResponse"
    "fixture/DeleteSpotDatafeedSubscriptionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteSpotDatafeedSubscription)

responseModifyVolume :: ModifyVolumeResponse -> TestTree
responseModifyVolume =
  res
    "ModifyVolumeResponse"
    "fixture/ModifyVolumeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyVolume)

responseEnableVpcClassicLink :: EnableVpcClassicLinkResponse -> TestTree
responseEnableVpcClassicLink =
  res
    "EnableVpcClassicLinkResponse"
    "fixture/EnableVpcClassicLinkResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy EnableVpcClassicLink)

responseDescribePlacementGroups :: DescribePlacementGroupsResponse -> TestTree
responseDescribePlacementGroups =
  res
    "DescribePlacementGroupsResponse"
    "fixture/DescribePlacementGroupsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribePlacementGroups)

responseProvisionByoipCidr :: ProvisionByoipCidrResponse -> TestTree
responseProvisionByoipCidr =
  res
    "ProvisionByoipCidrResponse"
    "fixture/ProvisionByoipCidrResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ProvisionByoipCidr)

responseDisassociateEnclaveCertificateIamRole :: DisassociateEnclaveCertificateIamRoleResponse -> TestTree
responseDisassociateEnclaveCertificateIamRole =
  res
    "DisassociateEnclaveCertificateIamRoleResponse"
    "fixture/DisassociateEnclaveCertificateIamRoleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisassociateEnclaveCertificateIamRole)

responseModifyAvailabilityZoneGroup :: ModifyAvailabilityZoneGroupResponse -> TestTree
responseModifyAvailabilityZoneGroup =
  res
    "ModifyAvailabilityZoneGroupResponse"
    "fixture/ModifyAvailabilityZoneGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyAvailabilityZoneGroup)

responseDescribeStaleSecurityGroups :: DescribeStaleSecurityGroupsResponse -> TestTree
responseDescribeStaleSecurityGroups =
  res
    "DescribeStaleSecurityGroupsResponse"
    "fixture/DescribeStaleSecurityGroupsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeStaleSecurityGroups)

responseCreateCarrierGateway :: CreateCarrierGatewayResponse -> TestTree
responseCreateCarrierGateway =
  res
    "CreateCarrierGatewayResponse"
    "fixture/CreateCarrierGatewayResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateCarrierGateway)

responseDescribeExportImageTasks :: DescribeExportImageTasksResponse -> TestTree
responseDescribeExportImageTasks =
  res
    "DescribeExportImageTasksResponse"
    "fixture/DescribeExportImageTasksResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeExportImageTasks)

responsePurchaseScheduledInstances :: PurchaseScheduledInstancesResponse -> TestTree
responsePurchaseScheduledInstances =
  res
    "PurchaseScheduledInstancesResponse"
    "fixture/PurchaseScheduledInstancesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PurchaseScheduledInstances)

responseEnableVgwRoutePropagation :: EnableVgwRoutePropagationResponse -> TestTree
responseEnableVgwRoutePropagation =
  res
    "EnableVgwRoutePropagationResponse"
    "fixture/EnableVgwRoutePropagationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy EnableVgwRoutePropagation)

responseDescribeSpotFleetRequestHistory :: DescribeSpotFleetRequestHistoryResponse -> TestTree
responseDescribeSpotFleetRequestHistory =
  res
    "DescribeSpotFleetRequestHistoryResponse"
    "fixture/DescribeSpotFleetRequestHistoryResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeSpotFleetRequestHistory)

responseModifySnapshotAttribute :: ModifySnapshotAttributeResponse -> TestTree
responseModifySnapshotAttribute =
  res
    "ModifySnapshotAttributeResponse"
    "fixture/ModifySnapshotAttributeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifySnapshotAttribute)

responseDescribeIamInstanceProfileAssociations :: DescribeIamInstanceProfileAssociationsResponse -> TestTree
responseDescribeIamInstanceProfileAssociations =
  res
    "DescribeIamInstanceProfileAssociationsResponse"
    "fixture/DescribeIamInstanceProfileAssociationsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeIamInstanceProfileAssociations)

responseCreateSnapshot :: Snapshot -> TestTree
responseCreateSnapshot =
  res
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateSnapshot)

responseCreateLocalGatewayRoute :: CreateLocalGatewayRouteResponse -> TestTree
responseCreateLocalGatewayRoute =
  res
    "CreateLocalGatewayRouteResponse"
    "fixture/CreateLocalGatewayRouteResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateLocalGatewayRoute)

responseCreateNetworkAclEntry :: CreateNetworkAclEntryResponse -> TestTree
responseCreateNetworkAclEntry =
  res
    "CreateNetworkAclEntryResponse"
    "fixture/CreateNetworkAclEntryResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateNetworkAclEntry)

responseDescribeTransitGatewayAttachments :: DescribeTransitGatewayAttachmentsResponse -> TestTree
responseDescribeTransitGatewayAttachments =
  res
    "DescribeTransitGatewayAttachmentsResponse"
    "fixture/DescribeTransitGatewayAttachmentsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeTransitGatewayAttachments)

responseCreateReservedInstancesListing :: CreateReservedInstancesListingResponse -> TestTree
responseCreateReservedInstancesListing =
  res
    "CreateReservedInstancesListingResponse"
    "fixture/CreateReservedInstancesListingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateReservedInstancesListing)

responseDescribeIpv6Pools :: DescribeIpv6PoolsResponse -> TestTree
responseDescribeIpv6Pools =
  res
    "DescribeIpv6PoolsResponse"
    "fixture/DescribeIpv6PoolsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeIpv6Pools)

responseAttachVpnGateway :: AttachVpnGatewayResponse -> TestTree
responseAttachVpnGateway =
  res
    "AttachVpnGatewayResponse"
    "fixture/AttachVpnGatewayResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AttachVpnGateway)

responseDescribeLocalGateways :: DescribeLocalGatewaysResponse -> TestTree
responseDescribeLocalGateways =
  res
    "DescribeLocalGatewaysResponse"
    "fixture/DescribeLocalGatewaysResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeLocalGateways)

responseModifyVpcEndpointServicePermissions :: ModifyVpcEndpointServicePermissionsResponse -> TestTree
responseModifyVpcEndpointServicePermissions =
  res
    "ModifyVpcEndpointServicePermissionsResponse"
    "fixture/ModifyVpcEndpointServicePermissionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyVpcEndpointServicePermissions)

responseExportClientVpnClientCertificateRevocationList :: ExportClientVpnClientCertificateRevocationListResponse -> TestTree
responseExportClientVpnClientCertificateRevocationList =
  res
    "ExportClientVpnClientCertificateRevocationListResponse"
    "fixture/ExportClientVpnClientCertificateRevocationListResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ExportClientVpnClientCertificateRevocationList)

responseCreateDhcpOptions :: CreateDhcpOptionsResponse -> TestTree
responseCreateDhcpOptions =
  res
    "CreateDhcpOptionsResponse"
    "fixture/CreateDhcpOptionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateDhcpOptions)

responseRegisterTransitGatewayMulticastGroupSources :: RegisterTransitGatewayMulticastGroupSourcesResponse -> TestTree
responseRegisterTransitGatewayMulticastGroupSources =
  res
    "RegisterTransitGatewayMulticastGroupSourcesResponse"
    "fixture/RegisterTransitGatewayMulticastGroupSourcesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RegisterTransitGatewayMulticastGroupSources)

responseDescribeAccountAttributes :: DescribeAccountAttributesResponse -> TestTree
responseDescribeAccountAttributes =
  res
    "DescribeAccountAttributesResponse"
    "fixture/DescribeAccountAttributesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeAccountAttributes)

responseGetTransitGatewayRouteTablePropagations :: GetTransitGatewayRouteTablePropagationsResponse -> TestTree
responseGetTransitGatewayRouteTablePropagations =
  res
    "GetTransitGatewayRouteTablePropagationsResponse"
    "fixture/GetTransitGatewayRouteTablePropagationsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetTransitGatewayRouteTablePropagations)

responseModifyFpgaImageAttribute :: ModifyFpgaImageAttributeResponse -> TestTree
responseModifyFpgaImageAttribute =
  res
    "ModifyFpgaImageAttributeResponse"
    "fixture/ModifyFpgaImageAttributeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyFpgaImageAttribute)

responseModifyHosts :: ModifyHostsResponse -> TestTree
responseModifyHosts =
  res
    "ModifyHostsResponse"
    "fixture/ModifyHostsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyHosts)

responseRebootInstances :: RebootInstancesResponse -> TestTree
responseRebootInstances =
  res
    "RebootInstancesResponse"
    "fixture/RebootInstancesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RebootInstances)

responseModifyVpcEndpointServiceConfiguration :: ModifyVpcEndpointServiceConfigurationResponse -> TestTree
responseModifyVpcEndpointServiceConfiguration =
  res
    "ModifyVpcEndpointServiceConfigurationResponse"
    "fixture/ModifyVpcEndpointServiceConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyVpcEndpointServiceConfiguration)

responseCreateTransitGateway :: CreateTransitGatewayResponse -> TestTree
responseCreateTransitGateway =
  res
    "CreateTransitGatewayResponse"
    "fixture/CreateTransitGatewayResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateTransitGateway)

responseUnassignIpv6Addresses :: UnassignIpv6AddressesResponse -> TestTree
responseUnassignIpv6Addresses =
  res
    "UnassignIpv6AddressesResponse"
    "fixture/UnassignIpv6AddressesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UnassignIpv6Addresses)

responseDeleteTrafficMirrorSession :: DeleteTrafficMirrorSessionResponse -> TestTree
responseDeleteTrafficMirrorSession =
  res
    "DeleteTrafficMirrorSessionResponse"
    "fixture/DeleteTrafficMirrorSessionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteTrafficMirrorSession)

responseCreateManagedPrefixList :: CreateManagedPrefixListResponse -> TestTree
responseCreateManagedPrefixList =
  res
    "CreateManagedPrefixListResponse"
    "fixture/CreateManagedPrefixListResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateManagedPrefixList)

responseAssociateIamInstanceProfile :: AssociateIamInstanceProfileResponse -> TestTree
responseAssociateIamInstanceProfile =
  res
    "AssociateIamInstanceProfileResponse"
    "fixture/AssociateIamInstanceProfileResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AssociateIamInstanceProfile)

responseModifyDefaultCreditSpecification :: ModifyDefaultCreditSpecificationResponse -> TestTree
responseModifyDefaultCreditSpecification =
  res
    "ModifyDefaultCreditSpecificationResponse"
    "fixture/ModifyDefaultCreditSpecificationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyDefaultCreditSpecification)

responseDeleteEgressOnlyInternetGateway :: DeleteEgressOnlyInternetGatewayResponse -> TestTree
responseDeleteEgressOnlyInternetGateway =
  res
    "DeleteEgressOnlyInternetGatewayResponse"
    "fixture/DeleteEgressOnlyInternetGatewayResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteEgressOnlyInternetGateway)

responsePurchaseHostReservation :: PurchaseHostReservationResponse -> TestTree
responsePurchaseHostReservation =
  res
    "PurchaseHostReservationResponse"
    "fixture/PurchaseHostReservationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PurchaseHostReservation)

responseModifyTransitGatewayVpcAttachment :: ModifyTransitGatewayVpcAttachmentResponse -> TestTree
responseModifyTransitGatewayVpcAttachment =
  res
    "ModifyTransitGatewayVpcAttachmentResponse"
    "fixture/ModifyTransitGatewayVpcAttachmentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyTransitGatewayVpcAttachment)

responseCreateImage :: CreateImageResponse -> TestTree
responseCreateImage =
  res
    "CreateImageResponse"
    "fixture/CreateImageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateImage)

responseDescribeClassicLinkInstances :: DescribeClassicLinkInstancesResponse -> TestTree
responseDescribeClassicLinkInstances =
  res
    "DescribeClassicLinkInstancesResponse"
    "fixture/DescribeClassicLinkInstancesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeClassicLinkInstances)

responseTerminateInstances :: TerminateInstancesResponse -> TestTree
responseTerminateInstances =
  res
    "TerminateInstancesResponse"
    "fixture/TerminateInstancesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TerminateInstances)

responseGetTransitGatewayPrefixListReferences :: GetTransitGatewayPrefixListReferencesResponse -> TestTree
responseGetTransitGatewayPrefixListReferences =
  res
    "GetTransitGatewayPrefixListReferencesResponse"
    "fixture/GetTransitGatewayPrefixListReferencesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetTransitGatewayPrefixListReferences)

responseDescribeKeyPairs :: DescribeKeyPairsResponse -> TestTree
responseDescribeKeyPairs =
  res
    "DescribeKeyPairsResponse"
    "fixture/DescribeKeyPairsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeKeyPairs)

responseDisableFastSnapshotRestores :: DisableFastSnapshotRestoresResponse -> TestTree
responseDisableFastSnapshotRestores =
  res
    "DisableFastSnapshotRestoresResponse"
    "fixture/DisableFastSnapshotRestoresResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisableFastSnapshotRestores)

responseDescribeLaunchTemplates :: DescribeLaunchTemplatesResponse -> TestTree
responseDescribeLaunchTemplates =
  res
    "DescribeLaunchTemplatesResponse"
    "fixture/DescribeLaunchTemplatesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeLaunchTemplates)

responseCreateVpnConnectionRoute :: CreateVpnConnectionRouteResponse -> TestTree
responseCreateVpnConnectionRoute =
  res
    "CreateVpnConnectionRouteResponse"
    "fixture/CreateVpnConnectionRouteResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateVpnConnectionRoute)

responseAssociateRouteTable :: AssociateRouteTableResponse -> TestTree
responseAssociateRouteTable =
  res
    "AssociateRouteTableResponse"
    "fixture/AssociateRouteTableResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AssociateRouteTable)

responseDescribeVpnGateways :: DescribeVpnGatewaysResponse -> TestTree
responseDescribeVpnGateways =
  res
    "DescribeVpnGatewaysResponse"
    "fixture/DescribeVpnGatewaysResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeVpnGateways)

responseModifyVpnConnectionOptions :: ModifyVpnConnectionOptionsResponse -> TestTree
responseModifyVpnConnectionOptions =
  res
    "ModifyVpnConnectionOptionsResponse"
    "fixture/ModifyVpnConnectionOptionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyVpnConnectionOptions)

responseGetConsoleOutput :: GetConsoleOutputResponse -> TestTree
responseGetConsoleOutput =
  res
    "GetConsoleOutputResponse"
    "fixture/GetConsoleOutputResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetConsoleOutput)

responseDescribeHosts :: DescribeHostsResponse -> TestTree
responseDescribeHosts =
  res
    "DescribeHostsResponse"
    "fixture/DescribeHostsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeHosts)

responseDescribeImageAttribute :: DescribeImageAttributeResponse -> TestTree
responseDescribeImageAttribute =
  res
    "DescribeImageAttributeResponse"
    "fixture/DescribeImageAttributeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeImageAttribute)

responseModifyIdFormat :: ModifyIdFormatResponse -> TestTree
responseModifyIdFormat =
  res
    "ModifyIdFormatResponse"
    "fixture/ModifyIdFormatResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyIdFormat)

responseRegisterTransitGatewayMulticastGroupMembers :: RegisterTransitGatewayMulticastGroupMembersResponse -> TestTree
responseRegisterTransitGatewayMulticastGroupMembers =
  res
    "RegisterTransitGatewayMulticastGroupMembersResponse"
    "fixture/RegisterTransitGatewayMulticastGroupMembersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RegisterTransitGatewayMulticastGroupMembers)

responseDeleteManagedPrefixList :: DeleteManagedPrefixListResponse -> TestTree
responseDeleteManagedPrefixList =
  res
    "DeleteManagedPrefixListResponse"
    "fixture/DeleteManagedPrefixListResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteManagedPrefixList)

responseDeleteRouteTable :: DeleteRouteTableResponse -> TestTree
responseDeleteRouteTable =
  res
    "DeleteRouteTableResponse"
    "fixture/DeleteRouteTableResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteRouteTable)

responseResetImageAttribute :: ResetImageAttributeResponse -> TestTree
responseResetImageAttribute =
  res
    "ResetImageAttributeResponse"
    "fixture/ResetImageAttributeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ResetImageAttribute)

responseModifyTransitGatewayPrefixListReference :: ModifyTransitGatewayPrefixListReferenceResponse -> TestTree
responseModifyTransitGatewayPrefixListReference =
  res
    "ModifyTransitGatewayPrefixListReferenceResponse"
    "fixture/ModifyTransitGatewayPrefixListReferenceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyTransitGatewayPrefixListReference)

responseDescribeTransitGatewayRouteTables :: DescribeTransitGatewayRouteTablesResponse -> TestTree
responseDescribeTransitGatewayRouteTables =
  res
    "DescribeTransitGatewayRouteTablesResponse"
    "fixture/DescribeTransitGatewayRouteTablesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeTransitGatewayRouteTables)

responseCreateEgressOnlyInternetGateway :: CreateEgressOnlyInternetGatewayResponse -> TestTree
responseCreateEgressOnlyInternetGateway =
  res
    "CreateEgressOnlyInternetGatewayResponse"
    "fixture/CreateEgressOnlyInternetGatewayResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateEgressOnlyInternetGateway)

responseDescribeReservedInstancesModifications :: DescribeReservedInstancesModificationsResponse -> TestTree
responseDescribeReservedInstancesModifications =
  res
    "DescribeReservedInstancesModificationsResponse"
    "fixture/DescribeReservedInstancesModificationsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeReservedInstancesModifications)

responseDescribeSpotInstanceRequests :: DescribeSpotInstanceRequestsResponse -> TestTree
responseDescribeSpotInstanceRequests =
  res
    "DescribeSpotInstanceRequestsResponse"
    "fixture/DescribeSpotInstanceRequestsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeSpotInstanceRequests)

responseRevokeClientVpnIngress :: RevokeClientVpnIngressResponse -> TestTree
responseRevokeClientVpnIngress =
  res
    "RevokeClientVpnIngressResponse"
    "fixture/RevokeClientVpnIngressResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RevokeClientVpnIngress)

responseUnassignPrivateIpAddresses :: UnassignPrivateIpAddressesResponse -> TestTree
responseUnassignPrivateIpAddresses =
  res
    "UnassignPrivateIpAddressesResponse"
    "fixture/UnassignPrivateIpAddressesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UnassignPrivateIpAddresses)

responseDescribeNetworkInterfacePermissions :: DescribeNetworkInterfacePermissionsResponse -> TestTree
responseDescribeNetworkInterfacePermissions =
  res
    "DescribeNetworkInterfacePermissionsResponse"
    "fixture/DescribeNetworkInterfacePermissionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeNetworkInterfacePermissions)

responseEnableFastSnapshotRestores :: EnableFastSnapshotRestoresResponse -> TestTree
responseEnableFastSnapshotRestores =
  res
    "EnableFastSnapshotRestoresResponse"
    "fixture/EnableFastSnapshotRestoresResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy EnableFastSnapshotRestores)

responseDescribeVpcEndpointServicePermissions :: DescribeVpcEndpointServicePermissionsResponse -> TestTree
responseDescribeVpcEndpointServicePermissions =
  res
    "DescribeVpcEndpointServicePermissionsResponse"
    "fixture/DescribeVpcEndpointServicePermissionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeVpcEndpointServicePermissions)

responseDeleteDhcpOptions :: DeleteDhcpOptionsResponse -> TestTree
responseDeleteDhcpOptions =
  res
    "DeleteDhcpOptionsResponse"
    "fixture/DeleteDhcpOptionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteDhcpOptions)

responseRegisterInstanceEventNotificationAttributes :: RegisterInstanceEventNotificationAttributesResponse -> TestTree
responseRegisterInstanceEventNotificationAttributes =
  res
    "RegisterInstanceEventNotificationAttributesResponse"
    "fixture/RegisterInstanceEventNotificationAttributesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RegisterInstanceEventNotificationAttributes)

responseDescribeNetworkAcls :: DescribeNetworkAclsResponse -> TestTree
responseDescribeNetworkAcls =
  res
    "DescribeNetworkAclsResponse"
    "fixture/DescribeNetworkAclsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeNetworkAcls)

responseCancelImportTask :: CancelImportTaskResponse -> TestTree
responseCancelImportTask =
  res
    "CancelImportTaskResponse"
    "fixture/CancelImportTaskResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CancelImportTask)

responseDetachClassicLinkVpc :: DetachClassicLinkVpcResponse -> TestTree
responseDetachClassicLinkVpc =
  res
    "DetachClassicLinkVpcResponse"
    "fixture/DetachClassicLinkVpcResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DetachClassicLinkVpc)

responseDescribeRegions :: DescribeRegionsResponse -> TestTree
responseDescribeRegions =
  res
    "DescribeRegionsResponse"
    "fixture/DescribeRegionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeRegions)

responseMonitorInstances :: MonitorInstancesResponse -> TestTree
responseMonitorInstances =
  res
    "MonitorInstancesResponse"
    "fixture/MonitorInstancesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy MonitorInstances)

responseSearchLocalGatewayRoutes :: SearchLocalGatewayRoutesResponse -> TestTree
responseSearchLocalGatewayRoutes =
  res
    "SearchLocalGatewayRoutesResponse"
    "fixture/SearchLocalGatewayRoutesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SearchLocalGatewayRoutes)

responseDeleteClientVpnRoute :: DeleteClientVpnRouteResponse -> TestTree
responseDeleteClientVpnRoute =
  res
    "DeleteClientVpnRouteResponse"
    "fixture/DeleteClientVpnRouteResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteClientVpnRoute)

responseAcceptVpcPeeringConnection :: AcceptVpcPeeringConnectionResponse -> TestTree
responseAcceptVpcPeeringConnection =
  res
    "AcceptVpcPeeringConnectionResponse"
    "fixture/AcceptVpcPeeringConnectionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AcceptVpcPeeringConnection)

responseImportSnapshot :: ImportSnapshotResponse -> TestTree
responseImportSnapshot =
  res
    "ImportSnapshotResponse"
    "fixture/ImportSnapshotResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ImportSnapshot)

responseDescribeVolumeStatus :: DescribeVolumeStatusResponse -> TestTree
responseDescribeVolumeStatus =
  res
    "DescribeVolumeStatusResponse"
    "fixture/DescribeVolumeStatusResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeVolumeStatus)

responseDescribeRouteTables :: DescribeRouteTablesResponse -> TestTree
responseDescribeRouteTables =
  res
    "DescribeRouteTablesResponse"
    "fixture/DescribeRouteTablesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeRouteTables)

responseDescribeAvailabilityZones :: DescribeAvailabilityZonesResponse -> TestTree
responseDescribeAvailabilityZones =
  res
    "DescribeAvailabilityZonesResponse"
    "fixture/DescribeAvailabilityZonesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeAvailabilityZones)

responseModifyVpcAttribute :: ModifyVpcAttributeResponse -> TestTree
responseModifyVpcAttribute =
  res
    "ModifyVpcAttributeResponse"
    "fixture/ModifyVpcAttributeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyVpcAttribute)

responseDescribeClientVpnConnections :: DescribeClientVpnConnectionsResponse -> TestTree
responseDescribeClientVpnConnections =
  res
    "DescribeClientVpnConnectionsResponse"
    "fixture/DescribeClientVpnConnectionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeClientVpnConnections)

responseDescribeFleetHistory :: DescribeFleetHistoryResponse -> TestTree
responseDescribeFleetHistory =
  res
    "DescribeFleetHistoryResponse"
    "fixture/DescribeFleetHistoryResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeFleetHistory)

responseDescribeImages :: DescribeImagesResponse -> TestTree
responseDescribeImages =
  res
    "DescribeImagesResponse"
    "fixture/DescribeImagesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeImages)

responseDescribeElasticGpus :: DescribeElasticGpusResponse -> TestTree
responseDescribeElasticGpus =
  res
    "DescribeElasticGpusResponse"
    "fixture/DescribeElasticGpusResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeElasticGpus)

responseRestoreAddressToClassic :: RestoreAddressToClassicResponse -> TestTree
responseRestoreAddressToClassic =
  res
    "RestoreAddressToClassicResponse"
    "fixture/RestoreAddressToClassicResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RestoreAddressToClassic)

responseDescribeManagedPrefixLists :: DescribeManagedPrefixListsResponse -> TestTree
responseDescribeManagedPrefixLists =
  res
    "DescribeManagedPrefixListsResponse"
    "fixture/DescribeManagedPrefixListsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeManagedPrefixLists)

responseCreateKeyPair :: CreateKeyPairResponse -> TestTree
responseCreateKeyPair =
  res
    "CreateKeyPairResponse"
    "fixture/CreateKeyPairResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateKeyPair)

responseGetReservedInstancesExchangeQuote :: GetReservedInstancesExchangeQuoteResponse -> TestTree
responseGetReservedInstancesExchangeQuote =
  res
    "GetReservedInstancesExchangeQuoteResponse"
    "fixture/GetReservedInstancesExchangeQuoteResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetReservedInstancesExchangeQuote)

responseDeleteVolume :: DeleteVolumeResponse -> TestTree
responseDeleteVolume =
  res
    "DeleteVolumeResponse"
    "fixture/DeleteVolumeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteVolume)

responseDeprovisionByoipCidr :: DeprovisionByoipCidrResponse -> TestTree
responseDeprovisionByoipCidr =
  res
    "DeprovisionByoipCidrResponse"
    "fixture/DeprovisionByoipCidrResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeprovisionByoipCidr)

responseDeleteVpcEndpointServiceConfigurations :: DeleteVpcEndpointServiceConfigurationsResponse -> TestTree
responseDeleteVpcEndpointServiceConfigurations =
  res
    "DeleteVpcEndpointServiceConfigurationsResponse"
    "fixture/DeleteVpcEndpointServiceConfigurationsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteVpcEndpointServiceConfigurations)

responseDescribeSpotFleetInstances :: DescribeSpotFleetInstancesResponse -> TestTree
responseDescribeSpotFleetInstances =
  res
    "DescribeSpotFleetInstancesResponse"
    "fixture/DescribeSpotFleetInstancesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeSpotFleetInstances)
