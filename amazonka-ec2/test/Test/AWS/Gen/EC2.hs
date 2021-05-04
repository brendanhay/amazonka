{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.EC2
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--         [ requestAcceptReservedInstancesExchangeQuote $
--             newAcceptReservedInstancesExchangeQuote
--
--         , requestDescribeInstanceCreditSpecifications $
--             newDescribeInstanceCreditSpecifications
--
--         , requestDescribeByoipCidrs $
--             newDescribeByoipCidrs
--
--         , requestDescribeLocalGatewayVirtualInterfaces $
--             newDescribeLocalGatewayVirtualInterfaces
--
--         , requestDeleteLocalGatewayRouteTableVpcAssociation $
--             newDeleteLocalGatewayRouteTableVpcAssociation
--
--         , requestDetachVolume $
--             newDetachVolume
--
--         , requestCreateTransitGatewayVpcAttachment $
--             newCreateTransitGatewayVpcAttachment
--
--         , requestDeleteVpcEndpointConnectionNotifications $
--             newDeleteVpcEndpointConnectionNotifications
--
--         , requestDeleteNetworkInsightsPath $
--             newDeleteNetworkInsightsPath
--
--         , requestAuthorizeSecurityGroupEgress $
--             newAuthorizeSecurityGroupEgress
--
--         , requestModifyManagedPrefixList $
--             newModifyManagedPrefixList
--
--         , requestDeleteTransitGatewayPrefixListReference $
--             newDeleteTransitGatewayPrefixListReference
--
--         , requestDeleteTransitGatewayRoute $
--             newDeleteTransitGatewayRoute
--
--         , requestDescribeVpcPeeringConnections $
--             newDescribeVpcPeeringConnections
--
--         , requestDescribeInstances $
--             newDescribeInstances
--
--         , requestDeregisterInstanceEventNotificationAttributes $
--             newDeregisterInstanceEventNotificationAttributes
--
--         , requestCreateTransitGatewayMulticastDomain $
--             newCreateTransitGatewayMulticastDomain
--
--         , requestAssociateTransitGatewayMulticastDomain $
--             newAssociateTransitGatewayMulticastDomain
--
--         , requestReleaseAddress $
--             newReleaseAddress
--
--         , requestDeregisterTransitGatewayMulticastGroupMembers $
--             newDeregisterTransitGatewayMulticastGroupMembers
--
--         , requestGetHostReservationPurchasePreview $
--             newGetHostReservationPurchasePreview
--
--         , requestCancelBundleTask $
--             newCancelBundleTask
--
--         , requestGetCapacityReservationUsage $
--             newGetCapacityReservationUsage
--
--         , requestCreateTransitGatewayConnectPeer $
--             newCreateTransitGatewayConnectPeer
--
--         , requestModifyVpcTenancy $
--             newModifyVpcTenancy
--
--         , requestCreateVpcEndpointServiceConfiguration $
--             newCreateVpcEndpointServiceConfiguration
--
--         , requestDescribeExportTasks $
--             newDescribeExportTasks
--
--         , requestGetTransitGatewayMulticastDomainAssociations $
--             newGetTransitGatewayMulticastDomainAssociations
--
--         , requestDisableEbsEncryptionByDefault $
--             newDisableEbsEncryptionByDefault
--
--         , requestAssociateVpcCidrBlock $
--             newAssociateVpcCidrBlock
--
--         , requestCreateNetworkAcl $
--             newCreateNetworkAcl
--
--         , requestAcceptTransitGatewayPeeringAttachment $
--             newAcceptTransitGatewayPeeringAttachment
--
--         , requestDeleteLaunchTemplate $
--             newDeleteLaunchTemplate
--
--         , requestDeleteVpc $
--             newDeleteVpc
--
--         , requestDeleteFleets $
--             newDeleteFleets
--
--         , requestGetAssociatedIpv6PoolCidrs $
--             newGetAssociatedIpv6PoolCidrs
--
--         , requestDescribeTrafficMirrorSessions $
--             newDescribeTrafficMirrorSessions
--
--         , requestImportInstance $
--             newImportInstance
--
--         , requestDescribeLocalGatewayRouteTables $
--             newDescribeLocalGatewayRouteTables
--
--         , requestCreateNetworkInterfacePermission $
--             newCreateNetworkInterfacePermission
--
--         , requestCreateVpnGateway $
--             newCreateVpnGateway
--
--         , requestGetTransitGatewayRouteTableAssociations $
--             newGetTransitGatewayRouteTableAssociations
--
--         , requestRejectTransitGatewayVpcAttachment $
--             newRejectTransitGatewayVpcAttachment
--
--         , requestModifyVolumeAttribute $
--             newModifyVolumeAttribute
--
--         , requestDescribePrefixLists $
--             newDescribePrefixLists
--
--         , requestDetachNetworkInterface $
--             newDetachNetworkInterface
--
--         , requestDeleteVpcEndpoints $
--             newDeleteVpcEndpoints
--
--         , requestDescribeVpcClassicLink $
--             newDescribeVpcClassicLink
--
--         , requestUpdateSecurityGroupRuleDescriptionsIngress $
--             newUpdateSecurityGroupRuleDescriptionsIngress
--
--         , requestDescribeClientVpnEndpoints $
--             newDescribeClientVpnEndpoints
--
--         , requestDisassociateAddress $
--             newDisassociateAddress
--
--         , requestDescribeScheduledInstanceAvailability $
--             newDescribeScheduledInstanceAvailability
--
--         , requestRejectVpcEndpointConnections $
--             newRejectVpcEndpointConnections
--
--         , requestCreateTransitGatewayRouteTable $
--             newCreateTransitGatewayRouteTable
--
--         , requestDescribeTags $
--             newDescribeTags
--
--         , requestAssociateTransitGatewayRouteTable $
--             newAssociateTransitGatewayRouteTable
--
--         , requestDeleteFlowLogs $
--             newDeleteFlowLogs
--
--         , requestCreateDefaultSubnet $
--             newCreateDefaultSubnet
--
--         , requestDeleteTrafficMirrorTarget $
--             newDeleteTrafficMirrorTarget
--
--         , requestAcceptTransitGatewayMulticastDomainAssociations $
--             newAcceptTransitGatewayMulticastDomainAssociations
--
--         , requestDescribeLaunchTemplateVersions $
--             newDescribeLaunchTemplateVersions
--
--         , requestDescribeAvailabilityZones $
--             newDescribeAvailabilityZones
--
--         , requestGetReservedInstancesExchangeQuote $
--             newGetReservedInstancesExchangeQuote
--
--         , requestDeleteVpnGateway $
--             newDeleteVpnGateway
--
--         , requestCreateKeyPair $
--             newCreateKeyPair
--
--         , requestExportTransitGatewayRoutes $
--             newExportTransitGatewayRoutes
--
--         , requestCopySnapshot $
--             newCopySnapshot
--
--         , requestDescribeElasticGpus $
--             newDescribeElasticGpus
--
--         , requestStartNetworkInsightsAnalysis $
--             newStartNetworkInsightsAnalysis
--
--         , requestDescribeFpgaImages $
--             newDescribeFpgaImages
--
--         , requestCreateFlowLogs $
--             newCreateFlowLogs
--
--         , requestCreateLaunchTemplate $
--             newCreateLaunchTemplate
--
--         , requestDescribeImportImageTasks $
--             newDescribeImportImageTasks
--
--         , requestDeleteTransitGatewayRouteTable $
--             newDeleteTransitGatewayRouteTable
--
--         , requestDeleteNetworkAcl $
--             newDeleteNetworkAcl
--
--         , requestMoveAddressToVpc $
--             newMoveAddressToVpc
--
--         , requestDescribeFleetInstances $
--             newDescribeFleetInstances
--
--         , requestRestoreAddressToClassic $
--             newRestoreAddressToClassic
--
--         , requestDeleteNetworkInterfacePermission $
--             newDeleteNetworkInterfacePermission
--
--         , requestDescribeRouteTables $
--             newDescribeRouteTables
--
--         , requestUpdateSecurityGroupRuleDescriptionsEgress $
--             newUpdateSecurityGroupRuleDescriptionsEgress
--
--         , requestResetFpgaImageAttribute $
--             newResetFpgaImageAttribute
--
--         , requestStartVpcEndpointServicePrivateDnsVerification $
--             newStartVpcEndpointServicePrivateDnsVerification
--
--         , requestDescribeVolumes $
--             newDescribeVolumes
--
--         , requestCreateClientVpnEndpoint $
--             newCreateClientVpnEndpoint
--
--         , requestRevokeClientVpnIngress $
--             newRevokeClientVpnIngress
--
--         , requestDeleteFpgaImage $
--             newDeleteFpgaImage
--
--         , requestModifyVpcEndpoint $
--             newModifyVpcEndpoint
--
--         , requestDescribeReservedInstancesModifications $
--             newDescribeReservedInstancesModifications
--
--         , requestDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations $
--             newDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
--
--         , requestEnableFastSnapshotRestores $
--             newEnableFastSnapshotRestores
--
--         , requestDescribeClientVpnRoutes $
--             newDescribeClientVpnRoutes
--
--         , requestGetEbsDefaultKmsKeyId $
--             newGetEbsDefaultKmsKeyId
--
--         , requestModifyIdFormat $
--             newModifyIdFormat
--
--         , requestDetachClassicLinkVpc $
--             newDetachClassicLinkVpc
--
--         , requestUnassignPrivateIpAddresses $
--             newUnassignPrivateIpAddresses
--
--         , requestAllocateHosts $
--             newAllocateHosts
--
--         , requestGetConsoleOutput $
--             newGetConsoleOutput
--
--         , requestModifyVpnConnectionOptions $
--             newModifyVpnConnectionOptions
--
--         , requestCancelImportTask $
--             newCancelImportTask
--
--         , requestRegisterImage $
--             newRegisterImage
--
--         , requestModifyFleet $
--             newModifyFleet
--
--         , requestDeleteRouteTable $
--             newDeleteRouteTable
--
--         , requestModifyReservedInstances $
--             newModifyReservedInstances
--
--         , requestDescribeImageAttribute $
--             newDescribeImageAttribute
--
--         , requestCreateTrafficMirrorFilterRule $
--             newCreateTrafficMirrorFilterRule
--
--         , requestMonitorInstances $
--             newMonitorInstances
--
--         , requestModifyVpnConnection $
--             newModifyVpnConnection
--
--         , requestDescribeSpotInstanceRequests $
--             newDescribeSpotInstanceRequests
--
--         , requestCancelConversionTask $
--             newCancelConversionTask
--
--         , requestModifyVpcEndpointServiceConfiguration $
--             newModifyVpcEndpointServiceConfiguration
--
--         , requestModifyTransitGatewayVpcAttachment $
--             newModifyTransitGatewayVpcAttachment
--
--         , requestAssociateRouteTable $
--             newAssociateRouteTable
--
--         , requestDescribeAccountAttributes $
--             newDescribeAccountAttributes
--
--         , requestDescribeLaunchTemplates $
--             newDescribeLaunchTemplates
--
--         , requestDescribeIpv6Pools $
--             newDescribeIpv6Pools
--
--         , requestDescribeLocalGateways $
--             newDescribeLocalGateways
--
--         , requestPurchaseHostReservation $
--             newPurchaseHostReservation
--
--         , requestReportInstanceStatus $
--             newReportInstanceStatus
--
--         , requestModifyVpcEndpointServicePermissions $
--             newModifyVpcEndpointServicePermissions
--
--         , requestModifyHosts $
--             newModifyHosts
--
--         , requestUnassignIpv6Addresses $
--             newUnassignIpv6Addresses
--
--         , requestGetManagedPrefixListAssociations $
--             newGetManagedPrefixListAssociations
--
--         , requestDisableFastSnapshotRestores $
--             newDisableFastSnapshotRestores
--
--         , requestDeleteEgressOnlyInternetGateway $
--             newDeleteEgressOnlyInternetGateway
--
--         , requestRequestSpotInstances $
--             newRequestSpotInstances
--
--         , requestRunInstances $
--             newRunInstances
--
--         , requestGetTransitGatewayRouteTablePropagations $
--             newGetTransitGatewayRouteTablePropagations
--
--         , requestAttachVolume $
--             newAttachVolume
--
--         , requestAcceptVpcEndpointConnections $
--             newAcceptVpcEndpointConnections
--
--         , requestCreateDhcpOptions $
--             newCreateDhcpOptions
--
--         , requestRebootInstances $
--             newRebootInstances
--
--         , requestModifyImageAttribute $
--             newModifyImageAttribute
--
--         , requestCreateManagedPrefixList $
--             newCreateManagedPrefixList
--
--         , requestSearchTransitGatewayRoutes $
--             newSearchTransitGatewayRoutes
--
--         , requestDescribeIdFormat $
--             newDescribeIdFormat
--
--         , requestRegisterTransitGatewayMulticastGroupSources $
--             newRegisterTransitGatewayMulticastGroupSources
--
--         , requestDescribeVpcEndpointConnectionNotifications $
--             newDescribeVpcEndpointConnectionNotifications
--
--         , requestDescribeVpcs $
--             newDescribeVpcs
--
--         , requestGetTransitGatewayPrefixListReferences $
--             newGetTransitGatewayPrefixListReferences
--
--         , requestCreateRouteTable $
--             newCreateRouteTable
--
--         , requestDescribeVolumesModifications $
--             newDescribeVolumesModifications
--
--         , requestAssociateIamInstanceProfile $
--             newAssociateIamInstanceProfile
--
--         , requestCreateImage $
--             newCreateImage
--
--         , requestDescribeTrafficMirrorTargets $
--             newDescribeTrafficMirrorTargets
--
--         , requestAssociateDhcpOptions $
--             newAssociateDhcpOptions
--
--         , requestDescribeSpotFleetRequestHistory $
--             newDescribeSpotFleetRequestHistory
--
--         , requestModifyInstanceEventStartTime $
--             newModifyInstanceEventStartTime
--
--         , requestDisassociateEnclaveCertificateIamRole $
--             newDisassociateEnclaveCertificateIamRole
--
--         , requestDeleteVpcPeeringConnection $
--             newDeleteVpcPeeringConnection
--
--         , requestResetInstanceAttribute $
--             newResetInstanceAttribute
--
--         , requestDescribeInstanceStatus $
--             newDescribeInstanceStatus
--
--         , requestAttachNetworkInterface $
--             newAttachNetworkInterface
--
--         , requestAssignIpv6Addresses $
--             newAssignIpv6Addresses
--
--         , requestCreateLocalGatewayRoute $
--             newCreateLocalGatewayRoute
--
--         , requestEnableVgwRoutePropagation $
--             newEnableVgwRoutePropagation
--
--         , requestDescribeVpcEndpoints $
--             newDescribeVpcEndpoints
--
--         , requestCreateNetworkAclEntry $
--             newCreateNetworkAclEntry
--
--         , requestDescribeStaleSecurityGroups $
--             newDescribeStaleSecurityGroups
--
--         , requestDescribeFlowLogs $
--             newDescribeFlowLogs
--
--         , requestDescribePlacementGroups $
--             newDescribePlacementGroups
--
--         , requestDescribeFleets $
--             newDescribeFleets
--
--         , requestModifyIdentityIdFormat $
--             newModifyIdentityIdFormat
--
--         , requestDescribeLocalGatewayVirtualInterfaceGroups $
--             newDescribeLocalGatewayVirtualInterfaceGroups
--
--         , requestReplaceNetworkAclEntry $
--             newReplaceNetworkAclEntry
--
--         , requestDeleteTags $
--             newDeleteTags
--
--         , requestDescribeTransitGatewayAttachments $
--             newDescribeTransitGatewayAttachments
--
--         , requestDescribeReservedInstancesOfferings $
--             newDescribeReservedInstancesOfferings
--
--         , requestModifySnapshotAttribute $
--             newModifySnapshotAttribute
--
--         , requestConfirmProductInstance $
--             newConfirmProductInstance
--
--         , requestDescribeVpnConnections $
--             newDescribeVpnConnections
--
--         , requestModifyAvailabilityZoneGroup $
--             newModifyAvailabilityZoneGroup
--
--         , requestDisassociateIamInstanceProfile $
--             newDisassociateIamInstanceProfile
--
--         , requestDisableVpcClassicLink $
--             newDisableVpcClassicLink
--
--         , requestGetGroupsForCapacityReservation $
--             newGetGroupsForCapacityReservation
--
--         , requestImportVolume $
--             newImportVolume
--
--         , requestDescribeAddresses $
--             newDescribeAddresses
--
--         , requestDeleteLocalGatewayRoute $
--             newDeleteLocalGatewayRoute
--
--         , requestDescribeVpcEndpointServiceConfigurations $
--             newDescribeVpcEndpointServiceConfigurations
--
--         , requestDescribeNetworkInterfaces $
--             newDescribeNetworkInterfaces
--
--         , requestDescribeVpcEndpointServices $
--             newDescribeVpcEndpointServices
--
--         , requestDeleteNetworkAclEntry $
--             newDeleteNetworkAclEntry
--
--         , requestGetTransitGatewayAttachmentPropagations $
--             newGetTransitGatewayAttachmentPropagations
--
--         , requestAssignPrivateIpAddresses $
--             newAssignPrivateIpAddresses
--
--         , requestDescribeNatGateways $
--             newDescribeNatGateways
--
--         , requestDescribeSnapshotAttribute $
--             newDescribeSnapshotAttribute
--
--         , requestDeleteSnapshot $
--             newDeleteSnapshot
--
--         , requestDeleteCarrierGateway $
--             newDeleteCarrierGateway
--
--         , requestDescribeTransitGatewayVpcAttachments $
--             newDescribeTransitGatewayVpcAttachments
--
--         , requestModifyVpcEndpointConnectionNotification $
--             newModifyVpcEndpointConnectionNotification
--
--         , requestPurchaseReservedInstancesOffering $
--             newPurchaseReservedInstancesOffering
--
--         , requestAuthorizeSecurityGroupIngress $
--             newAuthorizeSecurityGroupIngress
--
--         , requestGetConsoleScreenshot $
--             newGetConsoleScreenshot
--
--         , requestDisableVgwRoutePropagation $
--             newDisableVgwRoutePropagation
--
--         , requestDescribeTransitGatewayMulticastDomains $
--             newDescribeTransitGatewayMulticastDomains
--
--         , requestDescribeSubnets $
--             newDescribeSubnets
--
--         , requestUnmonitorInstances $
--             newUnmonitorInstances
--
--         , requestCancelSpotInstanceRequests $
--             newCancelSpotInstanceRequests
--
--         , requestCreateSpotDatafeedSubscription $
--             newCreateSpotDatafeedSubscription
--
--         , requestDisassociateRouteTable $
--             newDisassociateRouteTable
--
--         , requestDescribeTransitGatewayConnectPeers $
--             newDescribeTransitGatewayConnectPeers
--
--         , requestModifyVpnTunnelCertificate $
--             newModifyVpnTunnelCertificate
--
--         , requestRestoreManagedPrefixListVersion $
--             newRestoreManagedPrefixListVersion
--
--         , requestModifyAddressAttribute $
--             newModifyAddressAttribute
--
--         , requestCreateVpnConnection $
--             newCreateVpnConnection
--
--         , requestAssociateSubnetCidrBlock $
--             newAssociateSubnetCidrBlock
--
--         , requestAttachClassicLinkVpc $
--             newAttachClassicLinkVpc
--
--         , requestDescribeSpotPriceHistory $
--             newDescribeSpotPriceHistory
--
--         , requestDeleteQueuedReservedInstances $
--             newDeleteQueuedReservedInstances
--
--         , requestDescribeAggregateIdFormat $
--             newDescribeAggregateIdFormat
--
--         , requestDescribeReservedInstancesListings $
--             newDescribeReservedInstancesListings
--
--         , requestCopyImage $
--             newCopyImage
--
--         , requestCreateLocalGatewayRouteTableVpcAssociation $
--             newCreateLocalGatewayRouteTableVpcAssociation
--
--         , requestDescribeCarrierGateways $
--             newDescribeCarrierGateways
--
--         , requestDeleteInternetGateway $
--             newDeleteInternetGateway
--
--         , requestCreateFleet $
--             newCreateFleet
--
--         , requestModifyClientVpnEndpoint $
--             newModifyClientVpnEndpoint
--
--         , requestModifyInstanceCapacityReservationAttributes $
--             newModifyInstanceCapacityReservationAttributes
--
--         , requestImportClientVpnClientCertificateRevocationList $
--             newImportClientVpnClientCertificateRevocationList
--
--         , requestAssociateClientVpnTargetNetwork $
--             newAssociateClientVpnTargetNetwork
--
--         , requestCancelCapacityReservation $
--             newCancelCapacityReservation
--
--         , requestCancelReservedInstancesListing $
--             newCancelReservedInstancesListing
--
--         , requestDisableTransitGatewayRouteTablePropagation $
--             newDisableTransitGatewayRouteTablePropagation
--
--         , requestDescribeVpcClassicLinkDnsSupport $
--             newDescribeVpcClassicLinkDnsSupport
--
--         , requestCreateVpcEndpoint $
--             newCreateVpcEndpoint
--
--         , requestDescribeSnapshots $
--             newDescribeSnapshots
--
--         , requestDescribeImportSnapshotTasks $
--             newDescribeImportSnapshotTasks
--
--         , requestDescribeNetworkInterfaceAttribute $
--             newDescribeNetworkInterfaceAttribute
--
--         , requestDescribeInstanceEventNotificationAttributes $
--             newDescribeInstanceEventNotificationAttributes
--
--         , requestEnableEbsEncryptionByDefault $
--             newEnableEbsEncryptionByDefault
--
--         , requestModifyTrafficMirrorFilterRule $
--             newModifyTrafficMirrorFilterRule
--
--         , requestDescribeCoipPools $
--             newDescribeCoipPools
--
--         , requestCancelExportTask $
--             newCancelExportTask
--
--         , requestEnableVolumeIO $
--             newEnableVolumeIO
--
--         , requestModifyTransitGateway $
--             newModifyTransitGateway
--
--         , requestDescribeInstanceTypeOfferings $
--             newDescribeInstanceTypeOfferings
--
--         , requestCreateSubnet $
--             newCreateSubnet
--
--         , requestRequestSpotFleet $
--             newRequestSpotFleet
--
--         , requestDeleteVpnConnection $
--             newDeleteVpnConnection
--
--         , requestModifySpotFleetRequest $
--             newModifySpotFleetRequest
--
--         , requestDeregisterImage $
--             newDeregisterImage
--
--         , requestDetachVpnGateway $
--             newDetachVpnGateway
--
--         , requestCreateNetworkInterface $
--             newCreateNetworkInterface
--
--         , requestModifyNetworkInterfaceAttribute $
--             newModifyNetworkInterfaceAttribute
--
--         , requestCreateNatGateway $
--             newCreateNatGateway
--
--         , requestGetAssociatedEnclaveCertificateIamRoles $
--             newGetAssociatedEnclaveCertificateIamRoles
--
--         , requestCreateInternetGateway $
--             newCreateInternetGateway
--
--         , requestEnableTransitGatewayRouteTablePropagation $
--             newEnableTransitGatewayRouteTablePropagation
--
--         , requestResetAddressAttribute $
--             newResetAddressAttribute
--
--         , requestDescribeTransitGatewayConnects $
--             newDescribeTransitGatewayConnects
--
--         , requestDeletePlacementGroup $
--             newDeletePlacementGroup
--
--         , requestDescribeInstanceTypes $
--             newDescribeInstanceTypes
--
--         , requestDescribeBundleTasks $
--             newDescribeBundleTasks
--
--         , requestModifySubnetAttribute $
--             newModifySubnetAttribute
--
--         , requestDescribeSecurityGroups $
--             newDescribeSecurityGroups
--
--         , requestCreateClientVpnRoute $
--             newCreateClientVpnRoute
--
--         , requestDisassociateSubnetCidrBlock $
--             newDisassociateSubnetCidrBlock
--
--         , requestDescribeSpotDatafeedSubscription $
--             newDescribeSpotDatafeedSubscription
--
--         , requestCreateInstanceExportTask $
--             newCreateInstanceExportTask
--
--         , requestDisassociateClientVpnTargetNetwork $
--             newDisassociateClientVpnTargetNetwork
--
--         , requestSendDiagnosticInterrupt $
--             newSendDiagnosticInterrupt
--
--         , requestDescribeVpcAttribute $
--             newDescribeVpcAttribute
--
--         , requestDescribeSecurityGroupReferences $
--             newDescribeSecurityGroupReferences
--
--         , requestModifyCapacityReservation $
--             newModifyCapacityReservation
--
--         , requestDetachInternetGateway $
--             newDetachInternetGateway
--
--         , requestCreateVolume $
--             newCreateVolume
--
--         , requestExportClientVpnClientConfiguration $
--             newExportClientVpnClientConfiguration
--
--         , requestRevokeSecurityGroupEgress $
--             newRevokeSecurityGroupEgress
--
--         , requestDeleteKeyPair $
--             newDeleteKeyPair
--
--         , requestModifyInstanceMetadataOptions $
--             newModifyInstanceMetadataOptions
--
--         , requestDescribeEgressOnlyInternetGateways $
--             newDescribeEgressOnlyInternetGateways
--
--         , requestModifyTrafficMirrorFilterNetworkServices $
--             newModifyTrafficMirrorFilterNetworkServices
--
--         , requestImportSnapshot $
--             newImportSnapshot
--
--         , requestDescribeImages $
--             newDescribeImages
--
--         , requestDeprovisionByoipCidr $
--             newDeprovisionByoipCidr
--
--         , requestDescribeAddressesAttribute $
--             newDescribeAddressesAttribute
--
--         , requestAcceptVpcPeeringConnection $
--             newAcceptVpcPeeringConnection
--
--         , requestDescribeMovingAddresses $
--             newDescribeMovingAddresses
--
--         , requestCreateVpcEndpointConnectionNotification $
--             newCreateVpcEndpointConnectionNotification
--
--         , requestDescribeFleetHistory $
--             newDescribeFleetHistory
--
--         , requestDeleteVpcEndpointServiceConfigurations $
--             newDeleteVpcEndpointServiceConfigurations
--
--         , requestCreateVpc $
--             newCreateVpc
--
--         , requestSearchLocalGatewayRoutes $
--             newSearchLocalGatewayRoutes
--
--         , requestCreateTrafficMirrorTarget $
--             newCreateTrafficMirrorTarget
--
--         , requestDescribeVolumeStatus $
--             newDescribeVolumeStatus
--
--         , requestDescribeVolumeAttribute $
--             newDescribeVolumeAttribute
--
--         , requestDeleteClientVpnRoute $
--             newDeleteClientVpnRoute
--
--         , requestModifyVpcPeeringConnectionOptions $
--             newModifyVpcPeeringConnectionOptions
--
--         , requestDescribeSpotFleetInstances $
--             newDescribeSpotFleetInstances
--
--         , requestDescribePrincipalIdFormat $
--             newDescribePrincipalIdFormat
--
--         , requestModifyInstanceCreditSpecification $
--             newModifyInstanceCreditSpecification
--
--         , requestDisassociateTransitGatewayMulticastDomain $
--             newDisassociateTransitGatewayMulticastDomain
--
--         , requestDescribeManagedPrefixLists $
--             newDescribeManagedPrefixLists
--
--         , requestGetPasswordData $
--             newGetPasswordData
--
--         , requestDeleteVolume $
--             newDeleteVolume
--
--         , requestDescribeTransitGateways $
--             newDescribeTransitGateways
--
--         , requestDescribeSpotFleetRequests $
--             newDescribeSpotFleetRequests
--
--         , requestDescribeClientVpnConnections $
--             newDescribeClientVpnConnections
--
--         , requestSearchTransitGatewayMulticastGroups $
--             newSearchTransitGatewayMulticastGroups
--
--         , requestModifyVpcAttribute $
--             newModifyVpcAttribute
--
--         , requestRevokeSecurityGroupIngress $
--             newRevokeSecurityGroupIngress
--
--         , requestDescribeHostReservationOfferings $
--             newDescribeHostReservationOfferings
--
--         , requestDescribeTransitGatewayRouteTables $
--             newDescribeTransitGatewayRouteTables
--
--         , requestDescribeNetworkAcls $
--             newDescribeNetworkAcls
--
--         , requestRegisterTransitGatewayMulticastGroupMembers $
--             newRegisterTransitGatewayMulticastGroupMembers
--
--         , requestDescribeHosts $
--             newDescribeHosts
--
--         , requestDescribeVpnGateways $
--             newDescribeVpnGateways
--
--         , requestDescribeHostReservations $
--             newDescribeHostReservations
--
--         , requestDeleteManagedPrefixList $
--             newDeleteManagedPrefixList
--
--         , requestRejectVpcPeeringConnection $
--             newRejectVpcPeeringConnection
--
--         , requestResetImageAttribute $
--             newResetImageAttribute
--
--         , requestDescribeScheduledInstances $
--             newDescribeScheduledInstances
--
--         , requestAssociateEnclaveCertificateIamRole $
--             newAssociateEnclaveCertificateIamRole
--
--         , requestModifyTransitGatewayPrefixListReference $
--             newModifyTransitGatewayPrefixListReference
--
--         , requestDescribeFpgaImageAttribute $
--             newDescribeFpgaImageAttribute
--
--         , requestAdvertiseByoipCidr $
--             newAdvertiseByoipCidr
--
--         , requestDeleteVpnConnectionRoute $
--             newDeleteVpnConnectionRoute
--
--         , requestDescribeVpcEndpointServicePermissions $
--             newDescribeVpcEndpointServicePermissions
--
--         , requestDescribeVpcEndpointConnections $
--             newDescribeVpcEndpointConnections
--
--         , requestDescribeNetworkInterfacePermissions $
--             newDescribeNetworkInterfacePermissions
--
--         , requestCreateTrafficMirrorSession $
--             newCreateTrafficMirrorSession
--
--         , requestRegisterInstanceEventNotificationAttributes $
--             newRegisterInstanceEventNotificationAttributes
--
--         , requestRejectTransitGatewayMulticastDomainAssociations $
--             newRejectTransitGatewayMulticastDomainAssociations
--
--         , requestDeleteDhcpOptions $
--             newDeleteDhcpOptions
--
--         , requestDeleteTransitGateway $
--             newDeleteTransitGateway
--
--         , requestEnableVpcClassicLinkDnsSupport $
--             newEnableVpcClassicLinkDnsSupport
--
--         , requestDescribeRegions $
--             newDescribeRegions
--
--         , requestCreateEgressOnlyInternetGateway $
--             newCreateEgressOnlyInternetGateway
--
--         , requestCreateTransitGateway $
--             newCreateTransitGateway
--
--         , requestDeleteClientVpnEndpoint $
--             newDeleteClientVpnEndpoint
--
--         , requestExportClientVpnClientCertificateRevocationList $
--             newExportClientVpnClientCertificateRevocationList
--
--         , requestCreateLaunchTemplateVersion $
--             newCreateLaunchTemplateVersion
--
--         , requestCreateSnapshots $
--             newCreateSnapshots
--
--         , requestModifyDefaultCreditSpecification $
--             newModifyDefaultCreditSpecification
--
--         , requestApplySecurityGroupsToClientVpnTargetNetwork $
--             newApplySecurityGroupsToClientVpnTargetNetwork
--
--         , requestAttachVpnGateway $
--             newAttachVpnGateway
--
--         , requestCreateVpnConnectionRoute $
--             newCreateVpnConnectionRoute
--
--         , requestDescribeKeyPairs $
--             newDescribeKeyPairs
--
--         , requestAllocateAddress $
--             newAllocateAddress
--
--         , requestDeleteTrafficMirrorSession $
--             newDeleteTrafficMirrorSession
--
--         , requestGetManagedPrefixListEntries $
--             newGetManagedPrefixListEntries
--
--         , requestCreateFpgaImage $
--             newCreateFpgaImage
--
--         , requestExportImage $
--             newExportImage
--
--         , requestRejectTransitGatewayPeeringAttachment $
--             newRejectTransitGatewayPeeringAttachment
--
--         , requestDescribeConversionTasks $
--             newDescribeConversionTasks
--
--         , requestWithdrawByoipCidr $
--             newWithdrawByoipCidr
--
--         , requestDeleteTrafficMirrorFilterRule $
--             newDeleteTrafficMirrorFilterRule
--
--         , requestDescribeClassicLinkInstances $
--             newDescribeClassicLinkInstances
--
--         , requestTerminateInstances $
--             newTerminateInstances
--
--         , requestAcceptTransitGatewayVpcAttachment $
--             newAcceptTransitGatewayVpcAttachment
--
--         , requestDisableVpcClassicLinkDnsSupport $
--             newDisableVpcClassicLinkDnsSupport
--
--         , requestGetLaunchTemplateData $
--             newGetLaunchTemplateData
--
--         , requestDescribeReservedInstances $
--             newDescribeReservedInstances
--
--         , requestModifyFpgaImageAttribute $
--             newModifyFpgaImageAttribute
--
--         , requestEnableVpcClassicLink $
--             newEnableVpcClassicLink
--
--         , requestAttachInternetGateway $
--             newAttachInternetGateway
--
--         , requestDescribePublicIpv4Pools $
--             newDescribePublicIpv4Pools
--
--         , requestCreateCustomerGateway $
--             newCreateCustomerGateway
--
--         , requestDescribeIamInstanceProfileAssociations $
--             newDescribeIamInstanceProfileAssociations
--
--         , requestDescribeExportImageTasks $
--             newDescribeExportImageTasks
--
--         , requestProvisionByoipCidr $
--             newProvisionByoipCidr
--
--         , requestCreateReservedInstancesListing $
--             newCreateReservedInstancesListing
--
--         , requestDescribeClientVpnTargetNetworks $
--             newDescribeClientVpnTargetNetworks
--
--         , requestModifyVpnTunnelOptions $
--             newModifyVpnTunnelOptions
--
--         , requestModifyInstancePlacement $
--             newModifyInstancePlacement
--
--         , requestImportKeyPair $
--             newImportKeyPair
--
--         , requestDescribeNetworkInsightsAnalyses $
--             newDescribeNetworkInsightsAnalyses
--
--         , requestDeleteSecurityGroup $
--             newDeleteSecurityGroup
--
--         , requestCreateCarrierGateway $
--             newCreateCarrierGateway
--
--         , requestCreateSnapshot $
--             newCreateSnapshot
--
--         , requestModifyVolume $
--             newModifyVolume
--
--         , requestDeleteNetworkInsightsAnalysis $
--             newDeleteNetworkInsightsAnalysis
--
--         , requestDescribeLocalGatewayRouteTableVpcAssociations $
--             newDescribeLocalGatewayRouteTableVpcAssociations
--
--         , requestCreateTrafficMirrorFilter $
--             newCreateTrafficMirrorFilter
--
--         , requestDeleteSpotDatafeedSubscription $
--             newDeleteSpotDatafeedSubscription
--
--         , requestDescribeInstanceAttribute $
--             newDescribeInstanceAttribute
--
--         , requestCreateCapacityReservation $
--             newCreateCapacityReservation
--
--         , requestDeleteTransitGatewayConnect $
--             newDeleteTransitGatewayConnect
--
--         , requestModifyEbsDefaultKmsKeyId $
--             newModifyEbsDefaultKmsKeyId
--
--         , requestDeleteRoute $
--             newDeleteRoute
--
--         , requestDescribeNetworkInsightsPaths $
--             newDescribeNetworkInsightsPaths
--
--         , requestPurchaseScheduledInstances $
--             newPurchaseScheduledInstances
--
--         , requestCreateTransitGatewayPeeringAttachment $
--             newCreateTransitGatewayPeeringAttachment
--
--         , requestGetDefaultCreditSpecification $
--             newGetDefaultCreditSpecification
--
--         , requestDescribeInternetGateways $
--             newDescribeInternetGateways
--
--         , requestModifyInstanceAttribute $
--             newModifyInstanceAttribute
--
--         , requestCreateSecurityGroup $
--             newCreateSecurityGroup
--
--         , requestCreateTransitGatewayConnect $
--             newCreateTransitGatewayConnect
--
--         , requestReplaceNetworkAclAssociation $
--             newReplaceNetworkAclAssociation
--
--         , requestCreateRoute $
--             newCreateRoute
--
--         , requestDeleteLaunchTemplateVersions $
--             newDeleteLaunchTemplateVersions
--
--         , requestDescribeIdentityIdFormat $
--             newDescribeIdentityIdFormat
--
--         , requestDeleteTrafficMirrorFilter $
--             newDeleteTrafficMirrorFilter
--
--         , requestReplaceRoute $
--             newReplaceRoute
--
--         , requestResetSnapshotAttribute $
--             newResetSnapshotAttribute
--
--         , requestResetEbsDefaultKmsKeyId $
--             newResetEbsDefaultKmsKeyId
--
--         , requestCreateTags $
--             newCreateTags
--
--         , requestBundleInstance $
--             newBundleInstance
--
--         , requestDeleteTransitGatewayPeeringAttachment $
--             newDeleteTransitGatewayPeeringAttachment
--
--         , requestAuthorizeClientVpnIngress $
--             newAuthorizeClientVpnIngress
--
--         , requestModifyLaunchTemplate $
--             newModifyLaunchTemplate
--
--         , requestDeleteCustomerGateway $
--             newDeleteCustomerGateway
--
--         , requestTerminateClientVpnConnections $
--             newTerminateClientVpnConnections
--
--         , requestGetEbsEncryptionByDefault $
--             newGetEbsEncryptionByDefault
--
--         , requestCreateVpcPeeringConnection $
--             newCreateVpcPeeringConnection
--
--         , requestDeleteTransitGatewayVpcAttachment $
--             newDeleteTransitGatewayVpcAttachment
--
--         , requestReplaceIamInstanceProfileAssociation $
--             newReplaceIamInstanceProfileAssociation
--
--         , requestDeleteTransitGatewayConnectPeer $
--             newDeleteTransitGatewayConnectPeer
--
--         , requestAssociateAddress $
--             newAssociateAddress
--
--         , requestCancelSpotFleetRequests $
--             newCancelSpotFleetRequests
--
--         , requestResetNetworkInterfaceAttribute $
--             newResetNetworkInterfaceAttribute
--
--         , requestStartInstances $
--             newStartInstances
--
--         , requestDisassociateTransitGatewayRouteTable $
--             newDisassociateTransitGatewayRouteTable
--
--         , requestCopyFpgaImage $
--             newCopyFpgaImage
--
--         , requestReleaseHosts $
--             newReleaseHosts
--
--         , requestDescribeFastSnapshotRestores $
--             newDescribeFastSnapshotRestores
--
--         , requestDescribeTrafficMirrorFilters $
--             newDescribeTrafficMirrorFilters
--
--         , requestCreateTransitGatewayPrefixListReference $
--             newCreateTransitGatewayPrefixListReference
--
--         , requestDeleteNetworkInterface $
--             newDeleteNetworkInterface
--
--         , requestCreateTransitGatewayRoute $
--             newCreateTransitGatewayRoute
--
--         , requestDeregisterTransitGatewayMulticastGroupSources $
--             newDeregisterTransitGatewayMulticastGroupSources
--
--         , requestDisassociateVpcCidrBlock $
--             newDisassociateVpcCidrBlock
--
--         , requestDescribeTransitGatewayPeeringAttachments $
--             newDescribeTransitGatewayPeeringAttachments
--
--         , requestGetCoipPoolUsage $
--             newGetCoipPoolUsage
--
--         , requestImportImage $
--             newImportImage
--
--         , requestReplaceTransitGatewayRoute $
--             newReplaceTransitGatewayRoute
--
--         , requestCreatePlacementGroup $
--             newCreatePlacementGroup
--
--         , requestCreateDefaultVpc $
--             newCreateDefaultVpc
--
--         , requestCreateNetworkInsightsPath $
--             newCreateNetworkInsightsPath
--
--         , requestModifyTrafficMirrorSession $
--             newModifyTrafficMirrorSession
--
--         , requestRunScheduledInstances $
--             newRunScheduledInstances
--
--         , requestDescribeDhcpOptions $
--             newDescribeDhcpOptions
--
--         , requestDescribeCapacityReservations $
--             newDescribeCapacityReservations
--
--         , requestDescribeCustomerGateways $
--             newDescribeCustomerGateways
--
--         , requestDeleteNatGateway $
--             newDeleteNatGateway
--
--         , requestDescribeClientVpnAuthorizationRules $
--             newDescribeClientVpnAuthorizationRules
--
--         , requestStopInstances $
--             newStopInstances
--
--         , requestReplaceRouteTableAssociation $
--             newReplaceRouteTableAssociation
--
--         , requestDeleteTransitGatewayMulticastDomain $
--             newDeleteTransitGatewayMulticastDomain
--
--         , requestDeleteSubnet $
--             newDeleteSubnet
--
--           ]

--     , testGroup "response"
--         [ responseAcceptReservedInstancesExchangeQuote $
--             newAcceptReservedInstancesExchangeQuoteResponse
--
--         , responseDescribeInstanceCreditSpecifications $
--             newDescribeInstanceCreditSpecificationsResponse
--
--         , responseDescribeByoipCidrs $
--             newDescribeByoipCidrsResponse
--
--         , responseDescribeLocalGatewayVirtualInterfaces $
--             newDescribeLocalGatewayVirtualInterfacesResponse
--
--         , responseDeleteLocalGatewayRouteTableVpcAssociation $
--             newDeleteLocalGatewayRouteTableVpcAssociationResponse
--
--         , responseDetachVolume $
--             newVolumeAttachment
--
--         , responseCreateTransitGatewayVpcAttachment $
--             newCreateTransitGatewayVpcAttachmentResponse
--
--         , responseDeleteVpcEndpointConnectionNotifications $
--             newDeleteVpcEndpointConnectionNotificationsResponse
--
--         , responseDeleteNetworkInsightsPath $
--             newDeleteNetworkInsightsPathResponse
--
--         , responseAuthorizeSecurityGroupEgress $
--             newAuthorizeSecurityGroupEgressResponse
--
--         , responseModifyManagedPrefixList $
--             newModifyManagedPrefixListResponse
--
--         , responseDeleteTransitGatewayPrefixListReference $
--             newDeleteTransitGatewayPrefixListReferenceResponse
--
--         , responseDeleteTransitGatewayRoute $
--             newDeleteTransitGatewayRouteResponse
--
--         , responseDescribeVpcPeeringConnections $
--             newDescribeVpcPeeringConnectionsResponse
--
--         , responseDescribeInstances $
--             newDescribeInstancesResponse
--
--         , responseDeregisterInstanceEventNotificationAttributes $
--             newDeregisterInstanceEventNotificationAttributesResponse
--
--         , responseCreateTransitGatewayMulticastDomain $
--             newCreateTransitGatewayMulticastDomainResponse
--
--         , responseAssociateTransitGatewayMulticastDomain $
--             newAssociateTransitGatewayMulticastDomainResponse
--
--         , responseReleaseAddress $
--             newReleaseAddressResponse
--
--         , responseDeregisterTransitGatewayMulticastGroupMembers $
--             newDeregisterTransitGatewayMulticastGroupMembersResponse
--
--         , responseGetHostReservationPurchasePreview $
--             newGetHostReservationPurchasePreviewResponse
--
--         , responseCancelBundleTask $
--             newCancelBundleTaskResponse
--
--         , responseGetCapacityReservationUsage $
--             newGetCapacityReservationUsageResponse
--
--         , responseCreateTransitGatewayConnectPeer $
--             newCreateTransitGatewayConnectPeerResponse
--
--         , responseModifyVpcTenancy $
--             newModifyVpcTenancyResponse
--
--         , responseCreateVpcEndpointServiceConfiguration $
--             newCreateVpcEndpointServiceConfigurationResponse
--
--         , responseDescribeExportTasks $
--             newDescribeExportTasksResponse
--
--         , responseGetTransitGatewayMulticastDomainAssociations $
--             newGetTransitGatewayMulticastDomainAssociationsResponse
--
--         , responseDisableEbsEncryptionByDefault $
--             newDisableEbsEncryptionByDefaultResponse
--
--         , responseAssociateVpcCidrBlock $
--             newAssociateVpcCidrBlockResponse
--
--         , responseCreateNetworkAcl $
--             newCreateNetworkAclResponse
--
--         , responseAcceptTransitGatewayPeeringAttachment $
--             newAcceptTransitGatewayPeeringAttachmentResponse
--
--         , responseDeleteLaunchTemplate $
--             newDeleteLaunchTemplateResponse
--
--         , responseDeleteVpc $
--             newDeleteVpcResponse
--
--         , responseDeleteFleets $
--             newDeleteFleetsResponse
--
--         , responseGetAssociatedIpv6PoolCidrs $
--             newGetAssociatedIpv6PoolCidrsResponse
--
--         , responseDescribeTrafficMirrorSessions $
--             newDescribeTrafficMirrorSessionsResponse
--
--         , responseImportInstance $
--             newImportInstanceResponse
--
--         , responseDescribeLocalGatewayRouteTables $
--             newDescribeLocalGatewayRouteTablesResponse
--
--         , responseCreateNetworkInterfacePermission $
--             newCreateNetworkInterfacePermissionResponse
--
--         , responseCreateVpnGateway $
--             newCreateVpnGatewayResponse
--
--         , responseGetTransitGatewayRouteTableAssociations $
--             newGetTransitGatewayRouteTableAssociationsResponse
--
--         , responseRejectTransitGatewayVpcAttachment $
--             newRejectTransitGatewayVpcAttachmentResponse
--
--         , responseModifyVolumeAttribute $
--             newModifyVolumeAttributeResponse
--
--         , responseDescribePrefixLists $
--             newDescribePrefixListsResponse
--
--         , responseDetachNetworkInterface $
--             newDetachNetworkInterfaceResponse
--
--         , responseDeleteVpcEndpoints $
--             newDeleteVpcEndpointsResponse
--
--         , responseDescribeVpcClassicLink $
--             newDescribeVpcClassicLinkResponse
--
--         , responseUpdateSecurityGroupRuleDescriptionsIngress $
--             newUpdateSecurityGroupRuleDescriptionsIngressResponse
--
--         , responseDescribeClientVpnEndpoints $
--             newDescribeClientVpnEndpointsResponse
--
--         , responseDisassociateAddress $
--             newDisassociateAddressResponse
--
--         , responseDescribeScheduledInstanceAvailability $
--             newDescribeScheduledInstanceAvailabilityResponse
--
--         , responseRejectVpcEndpointConnections $
--             newRejectVpcEndpointConnectionsResponse
--
--         , responseCreateTransitGatewayRouteTable $
--             newCreateTransitGatewayRouteTableResponse
--
--         , responseDescribeTags $
--             newDescribeTagsResponse
--
--         , responseAssociateTransitGatewayRouteTable $
--             newAssociateTransitGatewayRouteTableResponse
--
--         , responseDeleteFlowLogs $
--             newDeleteFlowLogsResponse
--
--         , responseCreateDefaultSubnet $
--             newCreateDefaultSubnetResponse
--
--         , responseDeleteTrafficMirrorTarget $
--             newDeleteTrafficMirrorTargetResponse
--
--         , responseAcceptTransitGatewayMulticastDomainAssociations $
--             newAcceptTransitGatewayMulticastDomainAssociationsResponse
--
--         , responseDescribeLaunchTemplateVersions $
--             newDescribeLaunchTemplateVersionsResponse
--
--         , responseDescribeAvailabilityZones $
--             newDescribeAvailabilityZonesResponse
--
--         , responseGetReservedInstancesExchangeQuote $
--             newGetReservedInstancesExchangeQuoteResponse
--
--         , responseDeleteVpnGateway $
--             newDeleteVpnGatewayResponse
--
--         , responseCreateKeyPair $
--             newCreateKeyPairResponse
--
--         , responseExportTransitGatewayRoutes $
--             newExportTransitGatewayRoutesResponse
--
--         , responseCopySnapshot $
--             newCopySnapshotResponse
--
--         , responseDescribeElasticGpus $
--             newDescribeElasticGpusResponse
--
--         , responseStartNetworkInsightsAnalysis $
--             newStartNetworkInsightsAnalysisResponse
--
--         , responseDescribeFpgaImages $
--             newDescribeFpgaImagesResponse
--
--         , responseCreateFlowLogs $
--             newCreateFlowLogsResponse
--
--         , responseCreateLaunchTemplate $
--             newCreateLaunchTemplateResponse
--
--         , responseDescribeImportImageTasks $
--             newDescribeImportImageTasksResponse
--
--         , responseDeleteTransitGatewayRouteTable $
--             newDeleteTransitGatewayRouteTableResponse
--
--         , responseDeleteNetworkAcl $
--             newDeleteNetworkAclResponse
--
--         , responseMoveAddressToVpc $
--             newMoveAddressToVpcResponse
--
--         , responseDescribeFleetInstances $
--             newDescribeFleetInstancesResponse
--
--         , responseRestoreAddressToClassic $
--             newRestoreAddressToClassicResponse
--
--         , responseDeleteNetworkInterfacePermission $
--             newDeleteNetworkInterfacePermissionResponse
--
--         , responseDescribeRouteTables $
--             newDescribeRouteTablesResponse
--
--         , responseUpdateSecurityGroupRuleDescriptionsEgress $
--             newUpdateSecurityGroupRuleDescriptionsEgressResponse
--
--         , responseResetFpgaImageAttribute $
--             newResetFpgaImageAttributeResponse
--
--         , responseStartVpcEndpointServicePrivateDnsVerification $
--             newStartVpcEndpointServicePrivateDnsVerificationResponse
--
--         , responseDescribeVolumes $
--             newDescribeVolumesResponse
--
--         , responseCreateClientVpnEndpoint $
--             newCreateClientVpnEndpointResponse
--
--         , responseRevokeClientVpnIngress $
--             newRevokeClientVpnIngressResponse
--
--         , responseDeleteFpgaImage $
--             newDeleteFpgaImageResponse
--
--         , responseModifyVpcEndpoint $
--             newModifyVpcEndpointResponse
--
--         , responseDescribeReservedInstancesModifications $
--             newDescribeReservedInstancesModificationsResponse
--
--         , responseDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations $
--             newDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse
--
--         , responseEnableFastSnapshotRestores $
--             newEnableFastSnapshotRestoresResponse
--
--         , responseDescribeClientVpnRoutes $
--             newDescribeClientVpnRoutesResponse
--
--         , responseGetEbsDefaultKmsKeyId $
--             newGetEbsDefaultKmsKeyIdResponse
--
--         , responseModifyIdFormat $
--             newModifyIdFormatResponse
--
--         , responseDetachClassicLinkVpc $
--             newDetachClassicLinkVpcResponse
--
--         , responseUnassignPrivateIpAddresses $
--             newUnassignPrivateIpAddressesResponse
--
--         , responseAllocateHosts $
--             newAllocateHostsResponse
--
--         , responseGetConsoleOutput $
--             newGetConsoleOutputResponse
--
--         , responseModifyVpnConnectionOptions $
--             newModifyVpnConnectionOptionsResponse
--
--         , responseCancelImportTask $
--             newCancelImportTaskResponse
--
--         , responseRegisterImage $
--             newRegisterImageResponse
--
--         , responseModifyFleet $
--             newModifyFleetResponse
--
--         , responseDeleteRouteTable $
--             newDeleteRouteTableResponse
--
--         , responseModifyReservedInstances $
--             newModifyReservedInstancesResponse
--
--         , responseDescribeImageAttribute $
--             newDescribeImageAttributeResponse
--
--         , responseCreateTrafficMirrorFilterRule $
--             newCreateTrafficMirrorFilterRuleResponse
--
--         , responseMonitorInstances $
--             newMonitorInstancesResponse
--
--         , responseModifyVpnConnection $
--             newModifyVpnConnectionResponse
--
--         , responseDescribeSpotInstanceRequests $
--             newDescribeSpotInstanceRequestsResponse
--
--         , responseCancelConversionTask $
--             newCancelConversionTaskResponse
--
--         , responseModifyVpcEndpointServiceConfiguration $
--             newModifyVpcEndpointServiceConfigurationResponse
--
--         , responseModifyTransitGatewayVpcAttachment $
--             newModifyTransitGatewayVpcAttachmentResponse
--
--         , responseAssociateRouteTable $
--             newAssociateRouteTableResponse
--
--         , responseDescribeAccountAttributes $
--             newDescribeAccountAttributesResponse
--
--         , responseDescribeLaunchTemplates $
--             newDescribeLaunchTemplatesResponse
--
--         , responseDescribeIpv6Pools $
--             newDescribeIpv6PoolsResponse
--
--         , responseDescribeLocalGateways $
--             newDescribeLocalGatewaysResponse
--
--         , responsePurchaseHostReservation $
--             newPurchaseHostReservationResponse
--
--         , responseReportInstanceStatus $
--             newReportInstanceStatusResponse
--
--         , responseModifyVpcEndpointServicePermissions $
--             newModifyVpcEndpointServicePermissionsResponse
--
--         , responseModifyHosts $
--             newModifyHostsResponse
--
--         , responseUnassignIpv6Addresses $
--             newUnassignIpv6AddressesResponse
--
--         , responseGetManagedPrefixListAssociations $
--             newGetManagedPrefixListAssociationsResponse
--
--         , responseDisableFastSnapshotRestores $
--             newDisableFastSnapshotRestoresResponse
--
--         , responseDeleteEgressOnlyInternetGateway $
--             newDeleteEgressOnlyInternetGatewayResponse
--
--         , responseRequestSpotInstances $
--             newRequestSpotInstancesResponse
--
--         , responseRunInstances $
--             newReservation
--
--         , responseGetTransitGatewayRouteTablePropagations $
--             newGetTransitGatewayRouteTablePropagationsResponse
--
--         , responseAttachVolume $
--             newVolumeAttachment
--
--         , responseAcceptVpcEndpointConnections $
--             newAcceptVpcEndpointConnectionsResponse
--
--         , responseCreateDhcpOptions $
--             newCreateDhcpOptionsResponse
--
--         , responseRebootInstances $
--             newRebootInstancesResponse
--
--         , responseModifyImageAttribute $
--             newModifyImageAttributeResponse
--
--         , responseCreateManagedPrefixList $
--             newCreateManagedPrefixListResponse
--
--         , responseSearchTransitGatewayRoutes $
--             newSearchTransitGatewayRoutesResponse
--
--         , responseDescribeIdFormat $
--             newDescribeIdFormatResponse
--
--         , responseRegisterTransitGatewayMulticastGroupSources $
--             newRegisterTransitGatewayMulticastGroupSourcesResponse
--
--         , responseDescribeVpcEndpointConnectionNotifications $
--             newDescribeVpcEndpointConnectionNotificationsResponse
--
--         , responseDescribeVpcs $
--             newDescribeVpcsResponse
--
--         , responseGetTransitGatewayPrefixListReferences $
--             newGetTransitGatewayPrefixListReferencesResponse
--
--         , responseCreateRouteTable $
--             newCreateRouteTableResponse
--
--         , responseDescribeVolumesModifications $
--             newDescribeVolumesModificationsResponse
--
--         , responseAssociateIamInstanceProfile $
--             newAssociateIamInstanceProfileResponse
--
--         , responseCreateImage $
--             newCreateImageResponse
--
--         , responseDescribeTrafficMirrorTargets $
--             newDescribeTrafficMirrorTargetsResponse
--
--         , responseAssociateDhcpOptions $
--             newAssociateDhcpOptionsResponse
--
--         , responseDescribeSpotFleetRequestHistory $
--             newDescribeSpotFleetRequestHistoryResponse
--
--         , responseModifyInstanceEventStartTime $
--             newModifyInstanceEventStartTimeResponse
--
--         , responseDisassociateEnclaveCertificateIamRole $
--             newDisassociateEnclaveCertificateIamRoleResponse
--
--         , responseDeleteVpcPeeringConnection $
--             newDeleteVpcPeeringConnectionResponse
--
--         , responseResetInstanceAttribute $
--             newResetInstanceAttributeResponse
--
--         , responseDescribeInstanceStatus $
--             newDescribeInstanceStatusResponse
--
--         , responseAttachNetworkInterface $
--             newAttachNetworkInterfaceResponse
--
--         , responseAssignIpv6Addresses $
--             newAssignIpv6AddressesResponse
--
--         , responseCreateLocalGatewayRoute $
--             newCreateLocalGatewayRouteResponse
--
--         , responseEnableVgwRoutePropagation $
--             newEnableVgwRoutePropagationResponse
--
--         , responseDescribeVpcEndpoints $
--             newDescribeVpcEndpointsResponse
--
--         , responseCreateNetworkAclEntry $
--             newCreateNetworkAclEntryResponse
--
--         , responseDescribeStaleSecurityGroups $
--             newDescribeStaleSecurityGroupsResponse
--
--         , responseDescribeFlowLogs $
--             newDescribeFlowLogsResponse
--
--         , responseDescribePlacementGroups $
--             newDescribePlacementGroupsResponse
--
--         , responseDescribeFleets $
--             newDescribeFleetsResponse
--
--         , responseModifyIdentityIdFormat $
--             newModifyIdentityIdFormatResponse
--
--         , responseDescribeLocalGatewayVirtualInterfaceGroups $
--             newDescribeLocalGatewayVirtualInterfaceGroupsResponse
--
--         , responseReplaceNetworkAclEntry $
--             newReplaceNetworkAclEntryResponse
--
--         , responseDeleteTags $
--             newDeleteTagsResponse
--
--         , responseDescribeTransitGatewayAttachments $
--             newDescribeTransitGatewayAttachmentsResponse
--
--         , responseDescribeReservedInstancesOfferings $
--             newDescribeReservedInstancesOfferingsResponse
--
--         , responseModifySnapshotAttribute $
--             newModifySnapshotAttributeResponse
--
--         , responseConfirmProductInstance $
--             newConfirmProductInstanceResponse
--
--         , responseDescribeVpnConnections $
--             newDescribeVpnConnectionsResponse
--
--         , responseModifyAvailabilityZoneGroup $
--             newModifyAvailabilityZoneGroupResponse
--
--         , responseDisassociateIamInstanceProfile $
--             newDisassociateIamInstanceProfileResponse
--
--         , responseDisableVpcClassicLink $
--             newDisableVpcClassicLinkResponse
--
--         , responseGetGroupsForCapacityReservation $
--             newGetGroupsForCapacityReservationResponse
--
--         , responseImportVolume $
--             newImportVolumeResponse
--
--         , responseDescribeAddresses $
--             newDescribeAddressesResponse
--
--         , responseDeleteLocalGatewayRoute $
--             newDeleteLocalGatewayRouteResponse
--
--         , responseDescribeVpcEndpointServiceConfigurations $
--             newDescribeVpcEndpointServiceConfigurationsResponse
--
--         , responseDescribeNetworkInterfaces $
--             newDescribeNetworkInterfacesResponse
--
--         , responseDescribeVpcEndpointServices $
--             newDescribeVpcEndpointServicesResponse
--
--         , responseDeleteNetworkAclEntry $
--             newDeleteNetworkAclEntryResponse
--
--         , responseGetTransitGatewayAttachmentPropagations $
--             newGetTransitGatewayAttachmentPropagationsResponse
--
--         , responseAssignPrivateIpAddresses $
--             newAssignPrivateIpAddressesResponse
--
--         , responseDescribeNatGateways $
--             newDescribeNatGatewaysResponse
--
--         , responseDescribeSnapshotAttribute $
--             newDescribeSnapshotAttributeResponse
--
--         , responseDeleteSnapshot $
--             newDeleteSnapshotResponse
--
--         , responseDeleteCarrierGateway $
--             newDeleteCarrierGatewayResponse
--
--         , responseDescribeTransitGatewayVpcAttachments $
--             newDescribeTransitGatewayVpcAttachmentsResponse
--
--         , responseModifyVpcEndpointConnectionNotification $
--             newModifyVpcEndpointConnectionNotificationResponse
--
--         , responsePurchaseReservedInstancesOffering $
--             newPurchaseReservedInstancesOfferingResponse
--
--         , responseAuthorizeSecurityGroupIngress $
--             newAuthorizeSecurityGroupIngressResponse
--
--         , responseGetConsoleScreenshot $
--             newGetConsoleScreenshotResponse
--
--         , responseDisableVgwRoutePropagation $
--             newDisableVgwRoutePropagationResponse
--
--         , responseDescribeTransitGatewayMulticastDomains $
--             newDescribeTransitGatewayMulticastDomainsResponse
--
--         , responseDescribeSubnets $
--             newDescribeSubnetsResponse
--
--         , responseUnmonitorInstances $
--             newUnmonitorInstancesResponse
--
--         , responseCancelSpotInstanceRequests $
--             newCancelSpotInstanceRequestsResponse
--
--         , responseCreateSpotDatafeedSubscription $
--             newCreateSpotDatafeedSubscriptionResponse
--
--         , responseDisassociateRouteTable $
--             newDisassociateRouteTableResponse
--
--         , responseDescribeTransitGatewayConnectPeers $
--             newDescribeTransitGatewayConnectPeersResponse
--
--         , responseModifyVpnTunnelCertificate $
--             newModifyVpnTunnelCertificateResponse
--
--         , responseRestoreManagedPrefixListVersion $
--             newRestoreManagedPrefixListVersionResponse
--
--         , responseModifyAddressAttribute $
--             newModifyAddressAttributeResponse
--
--         , responseCreateVpnConnection $
--             newCreateVpnConnectionResponse
--
--         , responseAssociateSubnetCidrBlock $
--             newAssociateSubnetCidrBlockResponse
--
--         , responseAttachClassicLinkVpc $
--             newAttachClassicLinkVpcResponse
--
--         , responseDescribeSpotPriceHistory $
--             newDescribeSpotPriceHistoryResponse
--
--         , responseDeleteQueuedReservedInstances $
--             newDeleteQueuedReservedInstancesResponse
--
--         , responseDescribeAggregateIdFormat $
--             newDescribeAggregateIdFormatResponse
--
--         , responseDescribeReservedInstancesListings $
--             newDescribeReservedInstancesListingsResponse
--
--         , responseCopyImage $
--             newCopyImageResponse
--
--         , responseCreateLocalGatewayRouteTableVpcAssociation $
--             newCreateLocalGatewayRouteTableVpcAssociationResponse
--
--         , responseDescribeCarrierGateways $
--             newDescribeCarrierGatewaysResponse
--
--         , responseDeleteInternetGateway $
--             newDeleteInternetGatewayResponse
--
--         , responseCreateFleet $
--             newCreateFleetResponse
--
--         , responseModifyClientVpnEndpoint $
--             newModifyClientVpnEndpointResponse
--
--         , responseModifyInstanceCapacityReservationAttributes $
--             newModifyInstanceCapacityReservationAttributesResponse
--
--         , responseImportClientVpnClientCertificateRevocationList $
--             newImportClientVpnClientCertificateRevocationListResponse
--
--         , responseAssociateClientVpnTargetNetwork $
--             newAssociateClientVpnTargetNetworkResponse
--
--         , responseCancelCapacityReservation $
--             newCancelCapacityReservationResponse
--
--         , responseCancelReservedInstancesListing $
--             newCancelReservedInstancesListingResponse
--
--         , responseDisableTransitGatewayRouteTablePropagation $
--             newDisableTransitGatewayRouteTablePropagationResponse
--
--         , responseDescribeVpcClassicLinkDnsSupport $
--             newDescribeVpcClassicLinkDnsSupportResponse
--
--         , responseCreateVpcEndpoint $
--             newCreateVpcEndpointResponse
--
--         , responseDescribeSnapshots $
--             newDescribeSnapshotsResponse
--
--         , responseDescribeImportSnapshotTasks $
--             newDescribeImportSnapshotTasksResponse
--
--         , responseDescribeNetworkInterfaceAttribute $
--             newDescribeNetworkInterfaceAttributeResponse
--
--         , responseDescribeInstanceEventNotificationAttributes $
--             newDescribeInstanceEventNotificationAttributesResponse
--
--         , responseEnableEbsEncryptionByDefault $
--             newEnableEbsEncryptionByDefaultResponse
--
--         , responseModifyTrafficMirrorFilterRule $
--             newModifyTrafficMirrorFilterRuleResponse
--
--         , responseDescribeCoipPools $
--             newDescribeCoipPoolsResponse
--
--         , responseCancelExportTask $
--             newCancelExportTaskResponse
--
--         , responseEnableVolumeIO $
--             newEnableVolumeIOResponse
--
--         , responseModifyTransitGateway $
--             newModifyTransitGatewayResponse
--
--         , responseDescribeInstanceTypeOfferings $
--             newDescribeInstanceTypeOfferingsResponse
--
--         , responseCreateSubnet $
--             newCreateSubnetResponse
--
--         , responseRequestSpotFleet $
--             newRequestSpotFleetResponse
--
--         , responseDeleteVpnConnection $
--             newDeleteVpnConnectionResponse
--
--         , responseModifySpotFleetRequest $
--             newModifySpotFleetRequestResponse
--
--         , responseDeregisterImage $
--             newDeregisterImageResponse
--
--         , responseDetachVpnGateway $
--             newDetachVpnGatewayResponse
--
--         , responseCreateNetworkInterface $
--             newCreateNetworkInterfaceResponse
--
--         , responseModifyNetworkInterfaceAttribute $
--             newModifyNetworkInterfaceAttributeResponse
--
--         , responseCreateNatGateway $
--             newCreateNatGatewayResponse
--
--         , responseGetAssociatedEnclaveCertificateIamRoles $
--             newGetAssociatedEnclaveCertificateIamRolesResponse
--
--         , responseCreateInternetGateway $
--             newCreateInternetGatewayResponse
--
--         , responseEnableTransitGatewayRouteTablePropagation $
--             newEnableTransitGatewayRouteTablePropagationResponse
--
--         , responseResetAddressAttribute $
--             newResetAddressAttributeResponse
--
--         , responseDescribeTransitGatewayConnects $
--             newDescribeTransitGatewayConnectsResponse
--
--         , responseDeletePlacementGroup $
--             newDeletePlacementGroupResponse
--
--         , responseDescribeInstanceTypes $
--             newDescribeInstanceTypesResponse
--
--         , responseDescribeBundleTasks $
--             newDescribeBundleTasksResponse
--
--         , responseModifySubnetAttribute $
--             newModifySubnetAttributeResponse
--
--         , responseDescribeSecurityGroups $
--             newDescribeSecurityGroupsResponse
--
--         , responseCreateClientVpnRoute $
--             newCreateClientVpnRouteResponse
--
--         , responseDisassociateSubnetCidrBlock $
--             newDisassociateSubnetCidrBlockResponse
--
--         , responseDescribeSpotDatafeedSubscription $
--             newDescribeSpotDatafeedSubscriptionResponse
--
--         , responseCreateInstanceExportTask $
--             newCreateInstanceExportTaskResponse
--
--         , responseDisassociateClientVpnTargetNetwork $
--             newDisassociateClientVpnTargetNetworkResponse
--
--         , responseSendDiagnosticInterrupt $
--             newSendDiagnosticInterruptResponse
--
--         , responseDescribeVpcAttribute $
--             newDescribeVpcAttributeResponse
--
--         , responseDescribeSecurityGroupReferences $
--             newDescribeSecurityGroupReferencesResponse
--
--         , responseModifyCapacityReservation $
--             newModifyCapacityReservationResponse
--
--         , responseDetachInternetGateway $
--             newDetachInternetGatewayResponse
--
--         , responseCreateVolume $
--             newVolume
--
--         , responseExportClientVpnClientConfiguration $
--             newExportClientVpnClientConfigurationResponse
--
--         , responseRevokeSecurityGroupEgress $
--             newRevokeSecurityGroupEgressResponse
--
--         , responseDeleteKeyPair $
--             newDeleteKeyPairResponse
--
--         , responseModifyInstanceMetadataOptions $
--             newModifyInstanceMetadataOptionsResponse
--
--         , responseDescribeEgressOnlyInternetGateways $
--             newDescribeEgressOnlyInternetGatewaysResponse
--
--         , responseModifyTrafficMirrorFilterNetworkServices $
--             newModifyTrafficMirrorFilterNetworkServicesResponse
--
--         , responseImportSnapshot $
--             newImportSnapshotResponse
--
--         , responseDescribeImages $
--             newDescribeImagesResponse
--
--         , responseDeprovisionByoipCidr $
--             newDeprovisionByoipCidrResponse
--
--         , responseDescribeAddressesAttribute $
--             newDescribeAddressesAttributeResponse
--
--         , responseAcceptVpcPeeringConnection $
--             newAcceptVpcPeeringConnectionResponse
--
--         , responseDescribeMovingAddresses $
--             newDescribeMovingAddressesResponse
--
--         , responseCreateVpcEndpointConnectionNotification $
--             newCreateVpcEndpointConnectionNotificationResponse
--
--         , responseDescribeFleetHistory $
--             newDescribeFleetHistoryResponse
--
--         , responseDeleteVpcEndpointServiceConfigurations $
--             newDeleteVpcEndpointServiceConfigurationsResponse
--
--         , responseCreateVpc $
--             newCreateVpcResponse
--
--         , responseSearchLocalGatewayRoutes $
--             newSearchLocalGatewayRoutesResponse
--
--         , responseCreateTrafficMirrorTarget $
--             newCreateTrafficMirrorTargetResponse
--
--         , responseDescribeVolumeStatus $
--             newDescribeVolumeStatusResponse
--
--         , responseDescribeVolumeAttribute $
--             newDescribeVolumeAttributeResponse
--
--         , responseDeleteClientVpnRoute $
--             newDeleteClientVpnRouteResponse
--
--         , responseModifyVpcPeeringConnectionOptions $
--             newModifyVpcPeeringConnectionOptionsResponse
--
--         , responseDescribeSpotFleetInstances $
--             newDescribeSpotFleetInstancesResponse
--
--         , responseDescribePrincipalIdFormat $
--             newDescribePrincipalIdFormatResponse
--
--         , responseModifyInstanceCreditSpecification $
--             newModifyInstanceCreditSpecificationResponse
--
--         , responseDisassociateTransitGatewayMulticastDomain $
--             newDisassociateTransitGatewayMulticastDomainResponse
--
--         , responseDescribeManagedPrefixLists $
--             newDescribeManagedPrefixListsResponse
--
--         , responseGetPasswordData $
--             newGetPasswordDataResponse
--
--         , responseDeleteVolume $
--             newDeleteVolumeResponse
--
--         , responseDescribeTransitGateways $
--             newDescribeTransitGatewaysResponse
--
--         , responseDescribeSpotFleetRequests $
--             newDescribeSpotFleetRequestsResponse
--
--         , responseDescribeClientVpnConnections $
--             newDescribeClientVpnConnectionsResponse
--
--         , responseSearchTransitGatewayMulticastGroups $
--             newSearchTransitGatewayMulticastGroupsResponse
--
--         , responseModifyVpcAttribute $
--             newModifyVpcAttributeResponse
--
--         , responseRevokeSecurityGroupIngress $
--             newRevokeSecurityGroupIngressResponse
--
--         , responseDescribeHostReservationOfferings $
--             newDescribeHostReservationOfferingsResponse
--
--         , responseDescribeTransitGatewayRouteTables $
--             newDescribeTransitGatewayRouteTablesResponse
--
--         , responseDescribeNetworkAcls $
--             newDescribeNetworkAclsResponse
--
--         , responseRegisterTransitGatewayMulticastGroupMembers $
--             newRegisterTransitGatewayMulticastGroupMembersResponse
--
--         , responseDescribeHosts $
--             newDescribeHostsResponse
--
--         , responseDescribeVpnGateways $
--             newDescribeVpnGatewaysResponse
--
--         , responseDescribeHostReservations $
--             newDescribeHostReservationsResponse
--
--         , responseDeleteManagedPrefixList $
--             newDeleteManagedPrefixListResponse
--
--         , responseRejectVpcPeeringConnection $
--             newRejectVpcPeeringConnectionResponse
--
--         , responseResetImageAttribute $
--             newResetImageAttributeResponse
--
--         , responseDescribeScheduledInstances $
--             newDescribeScheduledInstancesResponse
--
--         , responseAssociateEnclaveCertificateIamRole $
--             newAssociateEnclaveCertificateIamRoleResponse
--
--         , responseModifyTransitGatewayPrefixListReference $
--             newModifyTransitGatewayPrefixListReferenceResponse
--
--         , responseDescribeFpgaImageAttribute $
--             newDescribeFpgaImageAttributeResponse
--
--         , responseAdvertiseByoipCidr $
--             newAdvertiseByoipCidrResponse
--
--         , responseDeleteVpnConnectionRoute $
--             newDeleteVpnConnectionRouteResponse
--
--         , responseDescribeVpcEndpointServicePermissions $
--             newDescribeVpcEndpointServicePermissionsResponse
--
--         , responseDescribeVpcEndpointConnections $
--             newDescribeVpcEndpointConnectionsResponse
--
--         , responseDescribeNetworkInterfacePermissions $
--             newDescribeNetworkInterfacePermissionsResponse
--
--         , responseCreateTrafficMirrorSession $
--             newCreateTrafficMirrorSessionResponse
--
--         , responseRegisterInstanceEventNotificationAttributes $
--             newRegisterInstanceEventNotificationAttributesResponse
--
--         , responseRejectTransitGatewayMulticastDomainAssociations $
--             newRejectTransitGatewayMulticastDomainAssociationsResponse
--
--         , responseDeleteDhcpOptions $
--             newDeleteDhcpOptionsResponse
--
--         , responseDeleteTransitGateway $
--             newDeleteTransitGatewayResponse
--
--         , responseEnableVpcClassicLinkDnsSupport $
--             newEnableVpcClassicLinkDnsSupportResponse
--
--         , responseDescribeRegions $
--             newDescribeRegionsResponse
--
--         , responseCreateEgressOnlyInternetGateway $
--             newCreateEgressOnlyInternetGatewayResponse
--
--         , responseCreateTransitGateway $
--             newCreateTransitGatewayResponse
--
--         , responseDeleteClientVpnEndpoint $
--             newDeleteClientVpnEndpointResponse
--
--         , responseExportClientVpnClientCertificateRevocationList $
--             newExportClientVpnClientCertificateRevocationListResponse
--
--         , responseCreateLaunchTemplateVersion $
--             newCreateLaunchTemplateVersionResponse
--
--         , responseCreateSnapshots $
--             newCreateSnapshotsResponse
--
--         , responseModifyDefaultCreditSpecification $
--             newModifyDefaultCreditSpecificationResponse
--
--         , responseApplySecurityGroupsToClientVpnTargetNetwork $
--             newApplySecurityGroupsToClientVpnTargetNetworkResponse
--
--         , responseAttachVpnGateway $
--             newAttachVpnGatewayResponse
--
--         , responseCreateVpnConnectionRoute $
--             newCreateVpnConnectionRouteResponse
--
--         , responseDescribeKeyPairs $
--             newDescribeKeyPairsResponse
--
--         , responseAllocateAddress $
--             newAllocateAddressResponse
--
--         , responseDeleteTrafficMirrorSession $
--             newDeleteTrafficMirrorSessionResponse
--
--         , responseGetManagedPrefixListEntries $
--             newGetManagedPrefixListEntriesResponse
--
--         , responseCreateFpgaImage $
--             newCreateFpgaImageResponse
--
--         , responseExportImage $
--             newExportImageResponse
--
--         , responseRejectTransitGatewayPeeringAttachment $
--             newRejectTransitGatewayPeeringAttachmentResponse
--
--         , responseDescribeConversionTasks $
--             newDescribeConversionTasksResponse
--
--         , responseWithdrawByoipCidr $
--             newWithdrawByoipCidrResponse
--
--         , responseDeleteTrafficMirrorFilterRule $
--             newDeleteTrafficMirrorFilterRuleResponse
--
--         , responseDescribeClassicLinkInstances $
--             newDescribeClassicLinkInstancesResponse
--
--         , responseTerminateInstances $
--             newTerminateInstancesResponse
--
--         , responseAcceptTransitGatewayVpcAttachment $
--             newAcceptTransitGatewayVpcAttachmentResponse
--
--         , responseDisableVpcClassicLinkDnsSupport $
--             newDisableVpcClassicLinkDnsSupportResponse
--
--         , responseGetLaunchTemplateData $
--             newGetLaunchTemplateDataResponse
--
--         , responseDescribeReservedInstances $
--             newDescribeReservedInstancesResponse
--
--         , responseModifyFpgaImageAttribute $
--             newModifyFpgaImageAttributeResponse
--
--         , responseEnableVpcClassicLink $
--             newEnableVpcClassicLinkResponse
--
--         , responseAttachInternetGateway $
--             newAttachInternetGatewayResponse
--
--         , responseDescribePublicIpv4Pools $
--             newDescribePublicIpv4PoolsResponse
--
--         , responseCreateCustomerGateway $
--             newCreateCustomerGatewayResponse
--
--         , responseDescribeIamInstanceProfileAssociations $
--             newDescribeIamInstanceProfileAssociationsResponse
--
--         , responseDescribeExportImageTasks $
--             newDescribeExportImageTasksResponse
--
--         , responseProvisionByoipCidr $
--             newProvisionByoipCidrResponse
--
--         , responseCreateReservedInstancesListing $
--             newCreateReservedInstancesListingResponse
--
--         , responseDescribeClientVpnTargetNetworks $
--             newDescribeClientVpnTargetNetworksResponse
--
--         , responseModifyVpnTunnelOptions $
--             newModifyVpnTunnelOptionsResponse
--
--         , responseModifyInstancePlacement $
--             newModifyInstancePlacementResponse
--
--         , responseImportKeyPair $
--             newImportKeyPairResponse
--
--         , responseDescribeNetworkInsightsAnalyses $
--             newDescribeNetworkInsightsAnalysesResponse
--
--         , responseDeleteSecurityGroup $
--             newDeleteSecurityGroupResponse
--
--         , responseCreateCarrierGateway $
--             newCreateCarrierGatewayResponse
--
--         , responseCreateSnapshot $
--             newSnapshot
--
--         , responseModifyVolume $
--             newModifyVolumeResponse
--
--         , responseDeleteNetworkInsightsAnalysis $
--             newDeleteNetworkInsightsAnalysisResponse
--
--         , responseDescribeLocalGatewayRouteTableVpcAssociations $
--             newDescribeLocalGatewayRouteTableVpcAssociationsResponse
--
--         , responseCreateTrafficMirrorFilter $
--             newCreateTrafficMirrorFilterResponse
--
--         , responseDeleteSpotDatafeedSubscription $
--             newDeleteSpotDatafeedSubscriptionResponse
--
--         , responseDescribeInstanceAttribute $
--             newDescribeInstanceAttributeResponse
--
--         , responseCreateCapacityReservation $
--             newCreateCapacityReservationResponse
--
--         , responseDeleteTransitGatewayConnect $
--             newDeleteTransitGatewayConnectResponse
--
--         , responseModifyEbsDefaultKmsKeyId $
--             newModifyEbsDefaultKmsKeyIdResponse
--
--         , responseDeleteRoute $
--             newDeleteRouteResponse
--
--         , responseDescribeNetworkInsightsPaths $
--             newDescribeNetworkInsightsPathsResponse
--
--         , responsePurchaseScheduledInstances $
--             newPurchaseScheduledInstancesResponse
--
--         , responseCreateTransitGatewayPeeringAttachment $
--             newCreateTransitGatewayPeeringAttachmentResponse
--
--         , responseGetDefaultCreditSpecification $
--             newGetDefaultCreditSpecificationResponse
--
--         , responseDescribeInternetGateways $
--             newDescribeInternetGatewaysResponse
--
--         , responseModifyInstanceAttribute $
--             newModifyInstanceAttributeResponse
--
--         , responseCreateSecurityGroup $
--             newCreateSecurityGroupResponse
--
--         , responseCreateTransitGatewayConnect $
--             newCreateTransitGatewayConnectResponse
--
--         , responseReplaceNetworkAclAssociation $
--             newReplaceNetworkAclAssociationResponse
--
--         , responseCreateRoute $
--             newCreateRouteResponse
--
--         , responseDeleteLaunchTemplateVersions $
--             newDeleteLaunchTemplateVersionsResponse
--
--         , responseDescribeIdentityIdFormat $
--             newDescribeIdentityIdFormatResponse
--
--         , responseDeleteTrafficMirrorFilter $
--             newDeleteTrafficMirrorFilterResponse
--
--         , responseReplaceRoute $
--             newReplaceRouteResponse
--
--         , responseResetSnapshotAttribute $
--             newResetSnapshotAttributeResponse
--
--         , responseResetEbsDefaultKmsKeyId $
--             newResetEbsDefaultKmsKeyIdResponse
--
--         , responseCreateTags $
--             newCreateTagsResponse
--
--         , responseBundleInstance $
--             newBundleInstanceResponse
--
--         , responseDeleteTransitGatewayPeeringAttachment $
--             newDeleteTransitGatewayPeeringAttachmentResponse
--
--         , responseAuthorizeClientVpnIngress $
--             newAuthorizeClientVpnIngressResponse
--
--         , responseModifyLaunchTemplate $
--             newModifyLaunchTemplateResponse
--
--         , responseDeleteCustomerGateway $
--             newDeleteCustomerGatewayResponse
--
--         , responseTerminateClientVpnConnections $
--             newTerminateClientVpnConnectionsResponse
--
--         , responseGetEbsEncryptionByDefault $
--             newGetEbsEncryptionByDefaultResponse
--
--         , responseCreateVpcPeeringConnection $
--             newCreateVpcPeeringConnectionResponse
--
--         , responseDeleteTransitGatewayVpcAttachment $
--             newDeleteTransitGatewayVpcAttachmentResponse
--
--         , responseReplaceIamInstanceProfileAssociation $
--             newReplaceIamInstanceProfileAssociationResponse
--
--         , responseDeleteTransitGatewayConnectPeer $
--             newDeleteTransitGatewayConnectPeerResponse
--
--         , responseAssociateAddress $
--             newAssociateAddressResponse
--
--         , responseCancelSpotFleetRequests $
--             newCancelSpotFleetRequestsResponse
--
--         , responseResetNetworkInterfaceAttribute $
--             newResetNetworkInterfaceAttributeResponse
--
--         , responseStartInstances $
--             newStartInstancesResponse
--
--         , responseDisassociateTransitGatewayRouteTable $
--             newDisassociateTransitGatewayRouteTableResponse
--
--         , responseCopyFpgaImage $
--             newCopyFpgaImageResponse
--
--         , responseReleaseHosts $
--             newReleaseHostsResponse
--
--         , responseDescribeFastSnapshotRestores $
--             newDescribeFastSnapshotRestoresResponse
--
--         , responseDescribeTrafficMirrorFilters $
--             newDescribeTrafficMirrorFiltersResponse
--
--         , responseCreateTransitGatewayPrefixListReference $
--             newCreateTransitGatewayPrefixListReferenceResponse
--
--         , responseDeleteNetworkInterface $
--             newDeleteNetworkInterfaceResponse
--
--         , responseCreateTransitGatewayRoute $
--             newCreateTransitGatewayRouteResponse
--
--         , responseDeregisterTransitGatewayMulticastGroupSources $
--             newDeregisterTransitGatewayMulticastGroupSourcesResponse
--
--         , responseDisassociateVpcCidrBlock $
--             newDisassociateVpcCidrBlockResponse
--
--         , responseDescribeTransitGatewayPeeringAttachments $
--             newDescribeTransitGatewayPeeringAttachmentsResponse
--
--         , responseGetCoipPoolUsage $
--             newGetCoipPoolUsageResponse
--
--         , responseImportImage $
--             newImportImageResponse
--
--         , responseReplaceTransitGatewayRoute $
--             newReplaceTransitGatewayRouteResponse
--
--         , responseCreatePlacementGroup $
--             newCreatePlacementGroupResponse
--
--         , responseCreateDefaultVpc $
--             newCreateDefaultVpcResponse
--
--         , responseCreateNetworkInsightsPath $
--             newCreateNetworkInsightsPathResponse
--
--         , responseModifyTrafficMirrorSession $
--             newModifyTrafficMirrorSessionResponse
--
--         , responseRunScheduledInstances $
--             newRunScheduledInstancesResponse
--
--         , responseDescribeDhcpOptions $
--             newDescribeDhcpOptionsResponse
--
--         , responseDescribeCapacityReservations $
--             newDescribeCapacityReservationsResponse
--
--         , responseDescribeCustomerGateways $
--             newDescribeCustomerGatewaysResponse
--
--         , responseDeleteNatGateway $
--             newDeleteNatGatewayResponse
--
--         , responseDescribeClientVpnAuthorizationRules $
--             newDescribeClientVpnAuthorizationRulesResponse
--
--         , responseStopInstances $
--             newStopInstancesResponse
--
--         , responseReplaceRouteTableAssociation $
--             newReplaceRouteTableAssociationResponse
--
--         , responseDeleteTransitGatewayMulticastDomain $
--             newDeleteTransitGatewayMulticastDomainResponse
--
--         , responseDeleteSubnet $
--             newDeleteSubnetResponse
--
--           ]
--     ]

-- Requests

requestAcceptReservedInstancesExchangeQuote :: AcceptReservedInstancesExchangeQuote -> TestTree
requestAcceptReservedInstancesExchangeQuote =
  req
    "AcceptReservedInstancesExchangeQuote"
    "fixture/AcceptReservedInstancesExchangeQuote.yaml"

requestDescribeInstanceCreditSpecifications :: DescribeInstanceCreditSpecifications -> TestTree
requestDescribeInstanceCreditSpecifications =
  req
    "DescribeInstanceCreditSpecifications"
    "fixture/DescribeInstanceCreditSpecifications.yaml"

requestDescribeByoipCidrs :: DescribeByoipCidrs -> TestTree
requestDescribeByoipCidrs =
  req
    "DescribeByoipCidrs"
    "fixture/DescribeByoipCidrs.yaml"

requestDescribeLocalGatewayVirtualInterfaces :: DescribeLocalGatewayVirtualInterfaces -> TestTree
requestDescribeLocalGatewayVirtualInterfaces =
  req
    "DescribeLocalGatewayVirtualInterfaces"
    "fixture/DescribeLocalGatewayVirtualInterfaces.yaml"

requestDeleteLocalGatewayRouteTableVpcAssociation :: DeleteLocalGatewayRouteTableVpcAssociation -> TestTree
requestDeleteLocalGatewayRouteTableVpcAssociation =
  req
    "DeleteLocalGatewayRouteTableVpcAssociation"
    "fixture/DeleteLocalGatewayRouteTableVpcAssociation.yaml"

requestDetachVolume :: DetachVolume -> TestTree
requestDetachVolume =
  req
    "DetachVolume"
    "fixture/DetachVolume.yaml"

requestCreateTransitGatewayVpcAttachment :: CreateTransitGatewayVpcAttachment -> TestTree
requestCreateTransitGatewayVpcAttachment =
  req
    "CreateTransitGatewayVpcAttachment"
    "fixture/CreateTransitGatewayVpcAttachment.yaml"

requestDeleteVpcEndpointConnectionNotifications :: DeleteVpcEndpointConnectionNotifications -> TestTree
requestDeleteVpcEndpointConnectionNotifications =
  req
    "DeleteVpcEndpointConnectionNotifications"
    "fixture/DeleteVpcEndpointConnectionNotifications.yaml"

requestDeleteNetworkInsightsPath :: DeleteNetworkInsightsPath -> TestTree
requestDeleteNetworkInsightsPath =
  req
    "DeleteNetworkInsightsPath"
    "fixture/DeleteNetworkInsightsPath.yaml"

requestAuthorizeSecurityGroupEgress :: AuthorizeSecurityGroupEgress -> TestTree
requestAuthorizeSecurityGroupEgress =
  req
    "AuthorizeSecurityGroupEgress"
    "fixture/AuthorizeSecurityGroupEgress.yaml"

requestModifyManagedPrefixList :: ModifyManagedPrefixList -> TestTree
requestModifyManagedPrefixList =
  req
    "ModifyManagedPrefixList"
    "fixture/ModifyManagedPrefixList.yaml"

requestDeleteTransitGatewayPrefixListReference :: DeleteTransitGatewayPrefixListReference -> TestTree
requestDeleteTransitGatewayPrefixListReference =
  req
    "DeleteTransitGatewayPrefixListReference"
    "fixture/DeleteTransitGatewayPrefixListReference.yaml"

requestDeleteTransitGatewayRoute :: DeleteTransitGatewayRoute -> TestTree
requestDeleteTransitGatewayRoute =
  req
    "DeleteTransitGatewayRoute"
    "fixture/DeleteTransitGatewayRoute.yaml"

requestDescribeVpcPeeringConnections :: DescribeVpcPeeringConnections -> TestTree
requestDescribeVpcPeeringConnections =
  req
    "DescribeVpcPeeringConnections"
    "fixture/DescribeVpcPeeringConnections.yaml"

requestDescribeInstances :: DescribeInstances -> TestTree
requestDescribeInstances =
  req
    "DescribeInstances"
    "fixture/DescribeInstances.yaml"

requestDeregisterInstanceEventNotificationAttributes :: DeregisterInstanceEventNotificationAttributes -> TestTree
requestDeregisterInstanceEventNotificationAttributes =
  req
    "DeregisterInstanceEventNotificationAttributes"
    "fixture/DeregisterInstanceEventNotificationAttributes.yaml"

requestCreateTransitGatewayMulticastDomain :: CreateTransitGatewayMulticastDomain -> TestTree
requestCreateTransitGatewayMulticastDomain =
  req
    "CreateTransitGatewayMulticastDomain"
    "fixture/CreateTransitGatewayMulticastDomain.yaml"

requestAssociateTransitGatewayMulticastDomain :: AssociateTransitGatewayMulticastDomain -> TestTree
requestAssociateTransitGatewayMulticastDomain =
  req
    "AssociateTransitGatewayMulticastDomain"
    "fixture/AssociateTransitGatewayMulticastDomain.yaml"

requestReleaseAddress :: ReleaseAddress -> TestTree
requestReleaseAddress =
  req
    "ReleaseAddress"
    "fixture/ReleaseAddress.yaml"

requestDeregisterTransitGatewayMulticastGroupMembers :: DeregisterTransitGatewayMulticastGroupMembers -> TestTree
requestDeregisterTransitGatewayMulticastGroupMembers =
  req
    "DeregisterTransitGatewayMulticastGroupMembers"
    "fixture/DeregisterTransitGatewayMulticastGroupMembers.yaml"

requestGetHostReservationPurchasePreview :: GetHostReservationPurchasePreview -> TestTree
requestGetHostReservationPurchasePreview =
  req
    "GetHostReservationPurchasePreview"
    "fixture/GetHostReservationPurchasePreview.yaml"

requestCancelBundleTask :: CancelBundleTask -> TestTree
requestCancelBundleTask =
  req
    "CancelBundleTask"
    "fixture/CancelBundleTask.yaml"

requestGetCapacityReservationUsage :: GetCapacityReservationUsage -> TestTree
requestGetCapacityReservationUsage =
  req
    "GetCapacityReservationUsage"
    "fixture/GetCapacityReservationUsage.yaml"

requestCreateTransitGatewayConnectPeer :: CreateTransitGatewayConnectPeer -> TestTree
requestCreateTransitGatewayConnectPeer =
  req
    "CreateTransitGatewayConnectPeer"
    "fixture/CreateTransitGatewayConnectPeer.yaml"

requestModifyVpcTenancy :: ModifyVpcTenancy -> TestTree
requestModifyVpcTenancy =
  req
    "ModifyVpcTenancy"
    "fixture/ModifyVpcTenancy.yaml"

requestCreateVpcEndpointServiceConfiguration :: CreateVpcEndpointServiceConfiguration -> TestTree
requestCreateVpcEndpointServiceConfiguration =
  req
    "CreateVpcEndpointServiceConfiguration"
    "fixture/CreateVpcEndpointServiceConfiguration.yaml"

requestDescribeExportTasks :: DescribeExportTasks -> TestTree
requestDescribeExportTasks =
  req
    "DescribeExportTasks"
    "fixture/DescribeExportTasks.yaml"

requestGetTransitGatewayMulticastDomainAssociations :: GetTransitGatewayMulticastDomainAssociations -> TestTree
requestGetTransitGatewayMulticastDomainAssociations =
  req
    "GetTransitGatewayMulticastDomainAssociations"
    "fixture/GetTransitGatewayMulticastDomainAssociations.yaml"

requestDisableEbsEncryptionByDefault :: DisableEbsEncryptionByDefault -> TestTree
requestDisableEbsEncryptionByDefault =
  req
    "DisableEbsEncryptionByDefault"
    "fixture/DisableEbsEncryptionByDefault.yaml"

requestAssociateVpcCidrBlock :: AssociateVpcCidrBlock -> TestTree
requestAssociateVpcCidrBlock =
  req
    "AssociateVpcCidrBlock"
    "fixture/AssociateVpcCidrBlock.yaml"

requestCreateNetworkAcl :: CreateNetworkAcl -> TestTree
requestCreateNetworkAcl =
  req
    "CreateNetworkAcl"
    "fixture/CreateNetworkAcl.yaml"

requestAcceptTransitGatewayPeeringAttachment :: AcceptTransitGatewayPeeringAttachment -> TestTree
requestAcceptTransitGatewayPeeringAttachment =
  req
    "AcceptTransitGatewayPeeringAttachment"
    "fixture/AcceptTransitGatewayPeeringAttachment.yaml"

requestDeleteLaunchTemplate :: DeleteLaunchTemplate -> TestTree
requestDeleteLaunchTemplate =
  req
    "DeleteLaunchTemplate"
    "fixture/DeleteLaunchTemplate.yaml"

requestDeleteVpc :: DeleteVpc -> TestTree
requestDeleteVpc =
  req
    "DeleteVpc"
    "fixture/DeleteVpc.yaml"

requestDeleteFleets :: DeleteFleets -> TestTree
requestDeleteFleets =
  req
    "DeleteFleets"
    "fixture/DeleteFleets.yaml"

requestGetAssociatedIpv6PoolCidrs :: GetAssociatedIpv6PoolCidrs -> TestTree
requestGetAssociatedIpv6PoolCidrs =
  req
    "GetAssociatedIpv6PoolCidrs"
    "fixture/GetAssociatedIpv6PoolCidrs.yaml"

requestDescribeTrafficMirrorSessions :: DescribeTrafficMirrorSessions -> TestTree
requestDescribeTrafficMirrorSessions =
  req
    "DescribeTrafficMirrorSessions"
    "fixture/DescribeTrafficMirrorSessions.yaml"

requestImportInstance :: ImportInstance -> TestTree
requestImportInstance =
  req
    "ImportInstance"
    "fixture/ImportInstance.yaml"

requestDescribeLocalGatewayRouteTables :: DescribeLocalGatewayRouteTables -> TestTree
requestDescribeLocalGatewayRouteTables =
  req
    "DescribeLocalGatewayRouteTables"
    "fixture/DescribeLocalGatewayRouteTables.yaml"

requestCreateNetworkInterfacePermission :: CreateNetworkInterfacePermission -> TestTree
requestCreateNetworkInterfacePermission =
  req
    "CreateNetworkInterfacePermission"
    "fixture/CreateNetworkInterfacePermission.yaml"

requestCreateVpnGateway :: CreateVpnGateway -> TestTree
requestCreateVpnGateway =
  req
    "CreateVpnGateway"
    "fixture/CreateVpnGateway.yaml"

requestGetTransitGatewayRouteTableAssociations :: GetTransitGatewayRouteTableAssociations -> TestTree
requestGetTransitGatewayRouteTableAssociations =
  req
    "GetTransitGatewayRouteTableAssociations"
    "fixture/GetTransitGatewayRouteTableAssociations.yaml"

requestRejectTransitGatewayVpcAttachment :: RejectTransitGatewayVpcAttachment -> TestTree
requestRejectTransitGatewayVpcAttachment =
  req
    "RejectTransitGatewayVpcAttachment"
    "fixture/RejectTransitGatewayVpcAttachment.yaml"

requestModifyVolumeAttribute :: ModifyVolumeAttribute -> TestTree
requestModifyVolumeAttribute =
  req
    "ModifyVolumeAttribute"
    "fixture/ModifyVolumeAttribute.yaml"

requestDescribePrefixLists :: DescribePrefixLists -> TestTree
requestDescribePrefixLists =
  req
    "DescribePrefixLists"
    "fixture/DescribePrefixLists.yaml"

requestDetachNetworkInterface :: DetachNetworkInterface -> TestTree
requestDetachNetworkInterface =
  req
    "DetachNetworkInterface"
    "fixture/DetachNetworkInterface.yaml"

requestDeleteVpcEndpoints :: DeleteVpcEndpoints -> TestTree
requestDeleteVpcEndpoints =
  req
    "DeleteVpcEndpoints"
    "fixture/DeleteVpcEndpoints.yaml"

requestDescribeVpcClassicLink :: DescribeVpcClassicLink -> TestTree
requestDescribeVpcClassicLink =
  req
    "DescribeVpcClassicLink"
    "fixture/DescribeVpcClassicLink.yaml"

requestUpdateSecurityGroupRuleDescriptionsIngress :: UpdateSecurityGroupRuleDescriptionsIngress -> TestTree
requestUpdateSecurityGroupRuleDescriptionsIngress =
  req
    "UpdateSecurityGroupRuleDescriptionsIngress"
    "fixture/UpdateSecurityGroupRuleDescriptionsIngress.yaml"

requestDescribeClientVpnEndpoints :: DescribeClientVpnEndpoints -> TestTree
requestDescribeClientVpnEndpoints =
  req
    "DescribeClientVpnEndpoints"
    "fixture/DescribeClientVpnEndpoints.yaml"

requestDisassociateAddress :: DisassociateAddress -> TestTree
requestDisassociateAddress =
  req
    "DisassociateAddress"
    "fixture/DisassociateAddress.yaml"

requestDescribeScheduledInstanceAvailability :: DescribeScheduledInstanceAvailability -> TestTree
requestDescribeScheduledInstanceAvailability =
  req
    "DescribeScheduledInstanceAvailability"
    "fixture/DescribeScheduledInstanceAvailability.yaml"

requestRejectVpcEndpointConnections :: RejectVpcEndpointConnections -> TestTree
requestRejectVpcEndpointConnections =
  req
    "RejectVpcEndpointConnections"
    "fixture/RejectVpcEndpointConnections.yaml"

requestCreateTransitGatewayRouteTable :: CreateTransitGatewayRouteTable -> TestTree
requestCreateTransitGatewayRouteTable =
  req
    "CreateTransitGatewayRouteTable"
    "fixture/CreateTransitGatewayRouteTable.yaml"

requestDescribeTags :: DescribeTags -> TestTree
requestDescribeTags =
  req
    "DescribeTags"
    "fixture/DescribeTags.yaml"

requestAssociateTransitGatewayRouteTable :: AssociateTransitGatewayRouteTable -> TestTree
requestAssociateTransitGatewayRouteTable =
  req
    "AssociateTransitGatewayRouteTable"
    "fixture/AssociateTransitGatewayRouteTable.yaml"

requestDeleteFlowLogs :: DeleteFlowLogs -> TestTree
requestDeleteFlowLogs =
  req
    "DeleteFlowLogs"
    "fixture/DeleteFlowLogs.yaml"

requestCreateDefaultSubnet :: CreateDefaultSubnet -> TestTree
requestCreateDefaultSubnet =
  req
    "CreateDefaultSubnet"
    "fixture/CreateDefaultSubnet.yaml"

requestDeleteTrafficMirrorTarget :: DeleteTrafficMirrorTarget -> TestTree
requestDeleteTrafficMirrorTarget =
  req
    "DeleteTrafficMirrorTarget"
    "fixture/DeleteTrafficMirrorTarget.yaml"

requestAcceptTransitGatewayMulticastDomainAssociations :: AcceptTransitGatewayMulticastDomainAssociations -> TestTree
requestAcceptTransitGatewayMulticastDomainAssociations =
  req
    "AcceptTransitGatewayMulticastDomainAssociations"
    "fixture/AcceptTransitGatewayMulticastDomainAssociations.yaml"

requestDescribeLaunchTemplateVersions :: DescribeLaunchTemplateVersions -> TestTree
requestDescribeLaunchTemplateVersions =
  req
    "DescribeLaunchTemplateVersions"
    "fixture/DescribeLaunchTemplateVersions.yaml"

requestDescribeAvailabilityZones :: DescribeAvailabilityZones -> TestTree
requestDescribeAvailabilityZones =
  req
    "DescribeAvailabilityZones"
    "fixture/DescribeAvailabilityZones.yaml"

requestGetReservedInstancesExchangeQuote :: GetReservedInstancesExchangeQuote -> TestTree
requestGetReservedInstancesExchangeQuote =
  req
    "GetReservedInstancesExchangeQuote"
    "fixture/GetReservedInstancesExchangeQuote.yaml"

requestDeleteVpnGateway :: DeleteVpnGateway -> TestTree
requestDeleteVpnGateway =
  req
    "DeleteVpnGateway"
    "fixture/DeleteVpnGateway.yaml"

requestCreateKeyPair :: CreateKeyPair -> TestTree
requestCreateKeyPair =
  req
    "CreateKeyPair"
    "fixture/CreateKeyPair.yaml"

requestExportTransitGatewayRoutes :: ExportTransitGatewayRoutes -> TestTree
requestExportTransitGatewayRoutes =
  req
    "ExportTransitGatewayRoutes"
    "fixture/ExportTransitGatewayRoutes.yaml"

requestCopySnapshot :: CopySnapshot -> TestTree
requestCopySnapshot =
  req
    "CopySnapshot"
    "fixture/CopySnapshot.yaml"

requestDescribeElasticGpus :: DescribeElasticGpus -> TestTree
requestDescribeElasticGpus =
  req
    "DescribeElasticGpus"
    "fixture/DescribeElasticGpus.yaml"

requestStartNetworkInsightsAnalysis :: StartNetworkInsightsAnalysis -> TestTree
requestStartNetworkInsightsAnalysis =
  req
    "StartNetworkInsightsAnalysis"
    "fixture/StartNetworkInsightsAnalysis.yaml"

requestDescribeFpgaImages :: DescribeFpgaImages -> TestTree
requestDescribeFpgaImages =
  req
    "DescribeFpgaImages"
    "fixture/DescribeFpgaImages.yaml"

requestCreateFlowLogs :: CreateFlowLogs -> TestTree
requestCreateFlowLogs =
  req
    "CreateFlowLogs"
    "fixture/CreateFlowLogs.yaml"

requestCreateLaunchTemplate :: CreateLaunchTemplate -> TestTree
requestCreateLaunchTemplate =
  req
    "CreateLaunchTemplate"
    "fixture/CreateLaunchTemplate.yaml"

requestDescribeImportImageTasks :: DescribeImportImageTasks -> TestTree
requestDescribeImportImageTasks =
  req
    "DescribeImportImageTasks"
    "fixture/DescribeImportImageTasks.yaml"

requestDeleteTransitGatewayRouteTable :: DeleteTransitGatewayRouteTable -> TestTree
requestDeleteTransitGatewayRouteTable =
  req
    "DeleteTransitGatewayRouteTable"
    "fixture/DeleteTransitGatewayRouteTable.yaml"

requestDeleteNetworkAcl :: DeleteNetworkAcl -> TestTree
requestDeleteNetworkAcl =
  req
    "DeleteNetworkAcl"
    "fixture/DeleteNetworkAcl.yaml"

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

requestRestoreAddressToClassic :: RestoreAddressToClassic -> TestTree
requestRestoreAddressToClassic =
  req
    "RestoreAddressToClassic"
    "fixture/RestoreAddressToClassic.yaml"

requestDeleteNetworkInterfacePermission :: DeleteNetworkInterfacePermission -> TestTree
requestDeleteNetworkInterfacePermission =
  req
    "DeleteNetworkInterfacePermission"
    "fixture/DeleteNetworkInterfacePermission.yaml"

requestDescribeRouteTables :: DescribeRouteTables -> TestTree
requestDescribeRouteTables =
  req
    "DescribeRouteTables"
    "fixture/DescribeRouteTables.yaml"

requestUpdateSecurityGroupRuleDescriptionsEgress :: UpdateSecurityGroupRuleDescriptionsEgress -> TestTree
requestUpdateSecurityGroupRuleDescriptionsEgress =
  req
    "UpdateSecurityGroupRuleDescriptionsEgress"
    "fixture/UpdateSecurityGroupRuleDescriptionsEgress.yaml"

requestResetFpgaImageAttribute :: ResetFpgaImageAttribute -> TestTree
requestResetFpgaImageAttribute =
  req
    "ResetFpgaImageAttribute"
    "fixture/ResetFpgaImageAttribute.yaml"

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

requestCreateClientVpnEndpoint :: CreateClientVpnEndpoint -> TestTree
requestCreateClientVpnEndpoint =
  req
    "CreateClientVpnEndpoint"
    "fixture/CreateClientVpnEndpoint.yaml"

requestRevokeClientVpnIngress :: RevokeClientVpnIngress -> TestTree
requestRevokeClientVpnIngress =
  req
    "RevokeClientVpnIngress"
    "fixture/RevokeClientVpnIngress.yaml"

requestDeleteFpgaImage :: DeleteFpgaImage -> TestTree
requestDeleteFpgaImage =
  req
    "DeleteFpgaImage"
    "fixture/DeleteFpgaImage.yaml"

requestModifyVpcEndpoint :: ModifyVpcEndpoint -> TestTree
requestModifyVpcEndpoint =
  req
    "ModifyVpcEndpoint"
    "fixture/ModifyVpcEndpoint.yaml"

requestDescribeReservedInstancesModifications :: DescribeReservedInstancesModifications -> TestTree
requestDescribeReservedInstancesModifications =
  req
    "DescribeReservedInstancesModifications"
    "fixture/DescribeReservedInstancesModifications.yaml"

requestDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations :: DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations -> TestTree
requestDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations =
  req
    "DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations"
    "fixture/DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations.yaml"

requestEnableFastSnapshotRestores :: EnableFastSnapshotRestores -> TestTree
requestEnableFastSnapshotRestores =
  req
    "EnableFastSnapshotRestores"
    "fixture/EnableFastSnapshotRestores.yaml"

requestDescribeClientVpnRoutes :: DescribeClientVpnRoutes -> TestTree
requestDescribeClientVpnRoutes =
  req
    "DescribeClientVpnRoutes"
    "fixture/DescribeClientVpnRoutes.yaml"

requestGetEbsDefaultKmsKeyId :: GetEbsDefaultKmsKeyId -> TestTree
requestGetEbsDefaultKmsKeyId =
  req
    "GetEbsDefaultKmsKeyId"
    "fixture/GetEbsDefaultKmsKeyId.yaml"

requestModifyIdFormat :: ModifyIdFormat -> TestTree
requestModifyIdFormat =
  req
    "ModifyIdFormat"
    "fixture/ModifyIdFormat.yaml"

requestDetachClassicLinkVpc :: DetachClassicLinkVpc -> TestTree
requestDetachClassicLinkVpc =
  req
    "DetachClassicLinkVpc"
    "fixture/DetachClassicLinkVpc.yaml"

requestUnassignPrivateIpAddresses :: UnassignPrivateIpAddresses -> TestTree
requestUnassignPrivateIpAddresses =
  req
    "UnassignPrivateIpAddresses"
    "fixture/UnassignPrivateIpAddresses.yaml"

requestAllocateHosts :: AllocateHosts -> TestTree
requestAllocateHosts =
  req
    "AllocateHosts"
    "fixture/AllocateHosts.yaml"

requestGetConsoleOutput :: GetConsoleOutput -> TestTree
requestGetConsoleOutput =
  req
    "GetConsoleOutput"
    "fixture/GetConsoleOutput.yaml"

requestModifyVpnConnectionOptions :: ModifyVpnConnectionOptions -> TestTree
requestModifyVpnConnectionOptions =
  req
    "ModifyVpnConnectionOptions"
    "fixture/ModifyVpnConnectionOptions.yaml"

requestCancelImportTask :: CancelImportTask -> TestTree
requestCancelImportTask =
  req
    "CancelImportTask"
    "fixture/CancelImportTask.yaml"

requestRegisterImage :: RegisterImage -> TestTree
requestRegisterImage =
  req
    "RegisterImage"
    "fixture/RegisterImage.yaml"

requestModifyFleet :: ModifyFleet -> TestTree
requestModifyFleet =
  req
    "ModifyFleet"
    "fixture/ModifyFleet.yaml"

requestDeleteRouteTable :: DeleteRouteTable -> TestTree
requestDeleteRouteTable =
  req
    "DeleteRouteTable"
    "fixture/DeleteRouteTable.yaml"

requestModifyReservedInstances :: ModifyReservedInstances -> TestTree
requestModifyReservedInstances =
  req
    "ModifyReservedInstances"
    "fixture/ModifyReservedInstances.yaml"

requestDescribeImageAttribute :: DescribeImageAttribute -> TestTree
requestDescribeImageAttribute =
  req
    "DescribeImageAttribute"
    "fixture/DescribeImageAttribute.yaml"

requestCreateTrafficMirrorFilterRule :: CreateTrafficMirrorFilterRule -> TestTree
requestCreateTrafficMirrorFilterRule =
  req
    "CreateTrafficMirrorFilterRule"
    "fixture/CreateTrafficMirrorFilterRule.yaml"

requestMonitorInstances :: MonitorInstances -> TestTree
requestMonitorInstances =
  req
    "MonitorInstances"
    "fixture/MonitorInstances.yaml"

requestModifyVpnConnection :: ModifyVpnConnection -> TestTree
requestModifyVpnConnection =
  req
    "ModifyVpnConnection"
    "fixture/ModifyVpnConnection.yaml"

requestDescribeSpotInstanceRequests :: DescribeSpotInstanceRequests -> TestTree
requestDescribeSpotInstanceRequests =
  req
    "DescribeSpotInstanceRequests"
    "fixture/DescribeSpotInstanceRequests.yaml"

requestCancelConversionTask :: CancelConversionTask -> TestTree
requestCancelConversionTask =
  req
    "CancelConversionTask"
    "fixture/CancelConversionTask.yaml"

requestModifyVpcEndpointServiceConfiguration :: ModifyVpcEndpointServiceConfiguration -> TestTree
requestModifyVpcEndpointServiceConfiguration =
  req
    "ModifyVpcEndpointServiceConfiguration"
    "fixture/ModifyVpcEndpointServiceConfiguration.yaml"

requestModifyTransitGatewayVpcAttachment :: ModifyTransitGatewayVpcAttachment -> TestTree
requestModifyTransitGatewayVpcAttachment =
  req
    "ModifyTransitGatewayVpcAttachment"
    "fixture/ModifyTransitGatewayVpcAttachment.yaml"

requestAssociateRouteTable :: AssociateRouteTable -> TestTree
requestAssociateRouteTable =
  req
    "AssociateRouteTable"
    "fixture/AssociateRouteTable.yaml"

requestDescribeAccountAttributes :: DescribeAccountAttributes -> TestTree
requestDescribeAccountAttributes =
  req
    "DescribeAccountAttributes"
    "fixture/DescribeAccountAttributes.yaml"

requestDescribeLaunchTemplates :: DescribeLaunchTemplates -> TestTree
requestDescribeLaunchTemplates =
  req
    "DescribeLaunchTemplates"
    "fixture/DescribeLaunchTemplates.yaml"

requestDescribeIpv6Pools :: DescribeIpv6Pools -> TestTree
requestDescribeIpv6Pools =
  req
    "DescribeIpv6Pools"
    "fixture/DescribeIpv6Pools.yaml"

requestDescribeLocalGateways :: DescribeLocalGateways -> TestTree
requestDescribeLocalGateways =
  req
    "DescribeLocalGateways"
    "fixture/DescribeLocalGateways.yaml"

requestPurchaseHostReservation :: PurchaseHostReservation -> TestTree
requestPurchaseHostReservation =
  req
    "PurchaseHostReservation"
    "fixture/PurchaseHostReservation.yaml"

requestReportInstanceStatus :: ReportInstanceStatus -> TestTree
requestReportInstanceStatus =
  req
    "ReportInstanceStatus"
    "fixture/ReportInstanceStatus.yaml"

requestModifyVpcEndpointServicePermissions :: ModifyVpcEndpointServicePermissions -> TestTree
requestModifyVpcEndpointServicePermissions =
  req
    "ModifyVpcEndpointServicePermissions"
    "fixture/ModifyVpcEndpointServicePermissions.yaml"

requestModifyHosts :: ModifyHosts -> TestTree
requestModifyHosts =
  req
    "ModifyHosts"
    "fixture/ModifyHosts.yaml"

requestUnassignIpv6Addresses :: UnassignIpv6Addresses -> TestTree
requestUnassignIpv6Addresses =
  req
    "UnassignIpv6Addresses"
    "fixture/UnassignIpv6Addresses.yaml"

requestGetManagedPrefixListAssociations :: GetManagedPrefixListAssociations -> TestTree
requestGetManagedPrefixListAssociations =
  req
    "GetManagedPrefixListAssociations"
    "fixture/GetManagedPrefixListAssociations.yaml"

requestDisableFastSnapshotRestores :: DisableFastSnapshotRestores -> TestTree
requestDisableFastSnapshotRestores =
  req
    "DisableFastSnapshotRestores"
    "fixture/DisableFastSnapshotRestores.yaml"

requestDeleteEgressOnlyInternetGateway :: DeleteEgressOnlyInternetGateway -> TestTree
requestDeleteEgressOnlyInternetGateway =
  req
    "DeleteEgressOnlyInternetGateway"
    "fixture/DeleteEgressOnlyInternetGateway.yaml"

requestRequestSpotInstances :: RequestSpotInstances -> TestTree
requestRequestSpotInstances =
  req
    "RequestSpotInstances"
    "fixture/RequestSpotInstances.yaml"

requestRunInstances :: RunInstances -> TestTree
requestRunInstances =
  req
    "RunInstances"
    "fixture/RunInstances.yaml"

requestGetTransitGatewayRouteTablePropagations :: GetTransitGatewayRouteTablePropagations -> TestTree
requestGetTransitGatewayRouteTablePropagations =
  req
    "GetTransitGatewayRouteTablePropagations"
    "fixture/GetTransitGatewayRouteTablePropagations.yaml"

requestAttachVolume :: AttachVolume -> TestTree
requestAttachVolume =
  req
    "AttachVolume"
    "fixture/AttachVolume.yaml"

requestAcceptVpcEndpointConnections :: AcceptVpcEndpointConnections -> TestTree
requestAcceptVpcEndpointConnections =
  req
    "AcceptVpcEndpointConnections"
    "fixture/AcceptVpcEndpointConnections.yaml"

requestCreateDhcpOptions :: CreateDhcpOptions -> TestTree
requestCreateDhcpOptions =
  req
    "CreateDhcpOptions"
    "fixture/CreateDhcpOptions.yaml"

requestRebootInstances :: RebootInstances -> TestTree
requestRebootInstances =
  req
    "RebootInstances"
    "fixture/RebootInstances.yaml"

requestModifyImageAttribute :: ModifyImageAttribute -> TestTree
requestModifyImageAttribute =
  req
    "ModifyImageAttribute"
    "fixture/ModifyImageAttribute.yaml"

requestCreateManagedPrefixList :: CreateManagedPrefixList -> TestTree
requestCreateManagedPrefixList =
  req
    "CreateManagedPrefixList"
    "fixture/CreateManagedPrefixList.yaml"

requestSearchTransitGatewayRoutes :: SearchTransitGatewayRoutes -> TestTree
requestSearchTransitGatewayRoutes =
  req
    "SearchTransitGatewayRoutes"
    "fixture/SearchTransitGatewayRoutes.yaml"

requestDescribeIdFormat :: DescribeIdFormat -> TestTree
requestDescribeIdFormat =
  req
    "DescribeIdFormat"
    "fixture/DescribeIdFormat.yaml"

requestRegisterTransitGatewayMulticastGroupSources :: RegisterTransitGatewayMulticastGroupSources -> TestTree
requestRegisterTransitGatewayMulticastGroupSources =
  req
    "RegisterTransitGatewayMulticastGroupSources"
    "fixture/RegisterTransitGatewayMulticastGroupSources.yaml"

requestDescribeVpcEndpointConnectionNotifications :: DescribeVpcEndpointConnectionNotifications -> TestTree
requestDescribeVpcEndpointConnectionNotifications =
  req
    "DescribeVpcEndpointConnectionNotifications"
    "fixture/DescribeVpcEndpointConnectionNotifications.yaml"

requestDescribeVpcs :: DescribeVpcs -> TestTree
requestDescribeVpcs =
  req
    "DescribeVpcs"
    "fixture/DescribeVpcs.yaml"

requestGetTransitGatewayPrefixListReferences :: GetTransitGatewayPrefixListReferences -> TestTree
requestGetTransitGatewayPrefixListReferences =
  req
    "GetTransitGatewayPrefixListReferences"
    "fixture/GetTransitGatewayPrefixListReferences.yaml"

requestCreateRouteTable :: CreateRouteTable -> TestTree
requestCreateRouteTable =
  req
    "CreateRouteTable"
    "fixture/CreateRouteTable.yaml"

requestDescribeVolumesModifications :: DescribeVolumesModifications -> TestTree
requestDescribeVolumesModifications =
  req
    "DescribeVolumesModifications"
    "fixture/DescribeVolumesModifications.yaml"

requestAssociateIamInstanceProfile :: AssociateIamInstanceProfile -> TestTree
requestAssociateIamInstanceProfile =
  req
    "AssociateIamInstanceProfile"
    "fixture/AssociateIamInstanceProfile.yaml"

requestCreateImage :: CreateImage -> TestTree
requestCreateImage =
  req
    "CreateImage"
    "fixture/CreateImage.yaml"

requestDescribeTrafficMirrorTargets :: DescribeTrafficMirrorTargets -> TestTree
requestDescribeTrafficMirrorTargets =
  req
    "DescribeTrafficMirrorTargets"
    "fixture/DescribeTrafficMirrorTargets.yaml"

requestAssociateDhcpOptions :: AssociateDhcpOptions -> TestTree
requestAssociateDhcpOptions =
  req
    "AssociateDhcpOptions"
    "fixture/AssociateDhcpOptions.yaml"

requestDescribeSpotFleetRequestHistory :: DescribeSpotFleetRequestHistory -> TestTree
requestDescribeSpotFleetRequestHistory =
  req
    "DescribeSpotFleetRequestHistory"
    "fixture/DescribeSpotFleetRequestHistory.yaml"

requestModifyInstanceEventStartTime :: ModifyInstanceEventStartTime -> TestTree
requestModifyInstanceEventStartTime =
  req
    "ModifyInstanceEventStartTime"
    "fixture/ModifyInstanceEventStartTime.yaml"

requestDisassociateEnclaveCertificateIamRole :: DisassociateEnclaveCertificateIamRole -> TestTree
requestDisassociateEnclaveCertificateIamRole =
  req
    "DisassociateEnclaveCertificateIamRole"
    "fixture/DisassociateEnclaveCertificateIamRole.yaml"

requestDeleteVpcPeeringConnection :: DeleteVpcPeeringConnection -> TestTree
requestDeleteVpcPeeringConnection =
  req
    "DeleteVpcPeeringConnection"
    "fixture/DeleteVpcPeeringConnection.yaml"

requestResetInstanceAttribute :: ResetInstanceAttribute -> TestTree
requestResetInstanceAttribute =
  req
    "ResetInstanceAttribute"
    "fixture/ResetInstanceAttribute.yaml"

requestDescribeInstanceStatus :: DescribeInstanceStatus -> TestTree
requestDescribeInstanceStatus =
  req
    "DescribeInstanceStatus"
    "fixture/DescribeInstanceStatus.yaml"

requestAttachNetworkInterface :: AttachNetworkInterface -> TestTree
requestAttachNetworkInterface =
  req
    "AttachNetworkInterface"
    "fixture/AttachNetworkInterface.yaml"

requestAssignIpv6Addresses :: AssignIpv6Addresses -> TestTree
requestAssignIpv6Addresses =
  req
    "AssignIpv6Addresses"
    "fixture/AssignIpv6Addresses.yaml"

requestCreateLocalGatewayRoute :: CreateLocalGatewayRoute -> TestTree
requestCreateLocalGatewayRoute =
  req
    "CreateLocalGatewayRoute"
    "fixture/CreateLocalGatewayRoute.yaml"

requestEnableVgwRoutePropagation :: EnableVgwRoutePropagation -> TestTree
requestEnableVgwRoutePropagation =
  req
    "EnableVgwRoutePropagation"
    "fixture/EnableVgwRoutePropagation.yaml"

requestDescribeVpcEndpoints :: DescribeVpcEndpoints -> TestTree
requestDescribeVpcEndpoints =
  req
    "DescribeVpcEndpoints"
    "fixture/DescribeVpcEndpoints.yaml"

requestCreateNetworkAclEntry :: CreateNetworkAclEntry -> TestTree
requestCreateNetworkAclEntry =
  req
    "CreateNetworkAclEntry"
    "fixture/CreateNetworkAclEntry.yaml"

requestDescribeStaleSecurityGroups :: DescribeStaleSecurityGroups -> TestTree
requestDescribeStaleSecurityGroups =
  req
    "DescribeStaleSecurityGroups"
    "fixture/DescribeStaleSecurityGroups.yaml"

requestDescribeFlowLogs :: DescribeFlowLogs -> TestTree
requestDescribeFlowLogs =
  req
    "DescribeFlowLogs"
    "fixture/DescribeFlowLogs.yaml"

requestDescribePlacementGroups :: DescribePlacementGroups -> TestTree
requestDescribePlacementGroups =
  req
    "DescribePlacementGroups"
    "fixture/DescribePlacementGroups.yaml"

requestDescribeFleets :: DescribeFleets -> TestTree
requestDescribeFleets =
  req
    "DescribeFleets"
    "fixture/DescribeFleets.yaml"

requestModifyIdentityIdFormat :: ModifyIdentityIdFormat -> TestTree
requestModifyIdentityIdFormat =
  req
    "ModifyIdentityIdFormat"
    "fixture/ModifyIdentityIdFormat.yaml"

requestDescribeLocalGatewayVirtualInterfaceGroups :: DescribeLocalGatewayVirtualInterfaceGroups -> TestTree
requestDescribeLocalGatewayVirtualInterfaceGroups =
  req
    "DescribeLocalGatewayVirtualInterfaceGroups"
    "fixture/DescribeLocalGatewayVirtualInterfaceGroups.yaml"

requestReplaceNetworkAclEntry :: ReplaceNetworkAclEntry -> TestTree
requestReplaceNetworkAclEntry =
  req
    "ReplaceNetworkAclEntry"
    "fixture/ReplaceNetworkAclEntry.yaml"

requestDeleteTags :: DeleteTags -> TestTree
requestDeleteTags =
  req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

requestDescribeTransitGatewayAttachments :: DescribeTransitGatewayAttachments -> TestTree
requestDescribeTransitGatewayAttachments =
  req
    "DescribeTransitGatewayAttachments"
    "fixture/DescribeTransitGatewayAttachments.yaml"

requestDescribeReservedInstancesOfferings :: DescribeReservedInstancesOfferings -> TestTree
requestDescribeReservedInstancesOfferings =
  req
    "DescribeReservedInstancesOfferings"
    "fixture/DescribeReservedInstancesOfferings.yaml"

requestModifySnapshotAttribute :: ModifySnapshotAttribute -> TestTree
requestModifySnapshotAttribute =
  req
    "ModifySnapshotAttribute"
    "fixture/ModifySnapshotAttribute.yaml"

requestConfirmProductInstance :: ConfirmProductInstance -> TestTree
requestConfirmProductInstance =
  req
    "ConfirmProductInstance"
    "fixture/ConfirmProductInstance.yaml"

requestDescribeVpnConnections :: DescribeVpnConnections -> TestTree
requestDescribeVpnConnections =
  req
    "DescribeVpnConnections"
    "fixture/DescribeVpnConnections.yaml"

requestModifyAvailabilityZoneGroup :: ModifyAvailabilityZoneGroup -> TestTree
requestModifyAvailabilityZoneGroup =
  req
    "ModifyAvailabilityZoneGroup"
    "fixture/ModifyAvailabilityZoneGroup.yaml"

requestDisassociateIamInstanceProfile :: DisassociateIamInstanceProfile -> TestTree
requestDisassociateIamInstanceProfile =
  req
    "DisassociateIamInstanceProfile"
    "fixture/DisassociateIamInstanceProfile.yaml"

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

requestImportVolume :: ImportVolume -> TestTree
requestImportVolume =
  req
    "ImportVolume"
    "fixture/ImportVolume.yaml"

requestDescribeAddresses :: DescribeAddresses -> TestTree
requestDescribeAddresses =
  req
    "DescribeAddresses"
    "fixture/DescribeAddresses.yaml"

requestDeleteLocalGatewayRoute :: DeleteLocalGatewayRoute -> TestTree
requestDeleteLocalGatewayRoute =
  req
    "DeleteLocalGatewayRoute"
    "fixture/DeleteLocalGatewayRoute.yaml"

requestDescribeVpcEndpointServiceConfigurations :: DescribeVpcEndpointServiceConfigurations -> TestTree
requestDescribeVpcEndpointServiceConfigurations =
  req
    "DescribeVpcEndpointServiceConfigurations"
    "fixture/DescribeVpcEndpointServiceConfigurations.yaml"

requestDescribeNetworkInterfaces :: DescribeNetworkInterfaces -> TestTree
requestDescribeNetworkInterfaces =
  req
    "DescribeNetworkInterfaces"
    "fixture/DescribeNetworkInterfaces.yaml"

requestDescribeVpcEndpointServices :: DescribeVpcEndpointServices -> TestTree
requestDescribeVpcEndpointServices =
  req
    "DescribeVpcEndpointServices"
    "fixture/DescribeVpcEndpointServices.yaml"

requestDeleteNetworkAclEntry :: DeleteNetworkAclEntry -> TestTree
requestDeleteNetworkAclEntry =
  req
    "DeleteNetworkAclEntry"
    "fixture/DeleteNetworkAclEntry.yaml"

requestGetTransitGatewayAttachmentPropagations :: GetTransitGatewayAttachmentPropagations -> TestTree
requestGetTransitGatewayAttachmentPropagations =
  req
    "GetTransitGatewayAttachmentPropagations"
    "fixture/GetTransitGatewayAttachmentPropagations.yaml"

requestAssignPrivateIpAddresses :: AssignPrivateIpAddresses -> TestTree
requestAssignPrivateIpAddresses =
  req
    "AssignPrivateIpAddresses"
    "fixture/AssignPrivateIpAddresses.yaml"

requestDescribeNatGateways :: DescribeNatGateways -> TestTree
requestDescribeNatGateways =
  req
    "DescribeNatGateways"
    "fixture/DescribeNatGateways.yaml"

requestDescribeSnapshotAttribute :: DescribeSnapshotAttribute -> TestTree
requestDescribeSnapshotAttribute =
  req
    "DescribeSnapshotAttribute"
    "fixture/DescribeSnapshotAttribute.yaml"

requestDeleteSnapshot :: DeleteSnapshot -> TestTree
requestDeleteSnapshot =
  req
    "DeleteSnapshot"
    "fixture/DeleteSnapshot.yaml"

requestDeleteCarrierGateway :: DeleteCarrierGateway -> TestTree
requestDeleteCarrierGateway =
  req
    "DeleteCarrierGateway"
    "fixture/DeleteCarrierGateway.yaml"

requestDescribeTransitGatewayVpcAttachments :: DescribeTransitGatewayVpcAttachments -> TestTree
requestDescribeTransitGatewayVpcAttachments =
  req
    "DescribeTransitGatewayVpcAttachments"
    "fixture/DescribeTransitGatewayVpcAttachments.yaml"

requestModifyVpcEndpointConnectionNotification :: ModifyVpcEndpointConnectionNotification -> TestTree
requestModifyVpcEndpointConnectionNotification =
  req
    "ModifyVpcEndpointConnectionNotification"
    "fixture/ModifyVpcEndpointConnectionNotification.yaml"

requestPurchaseReservedInstancesOffering :: PurchaseReservedInstancesOffering -> TestTree
requestPurchaseReservedInstancesOffering =
  req
    "PurchaseReservedInstancesOffering"
    "fixture/PurchaseReservedInstancesOffering.yaml"

requestAuthorizeSecurityGroupIngress :: AuthorizeSecurityGroupIngress -> TestTree
requestAuthorizeSecurityGroupIngress =
  req
    "AuthorizeSecurityGroupIngress"
    "fixture/AuthorizeSecurityGroupIngress.yaml"

requestGetConsoleScreenshot :: GetConsoleScreenshot -> TestTree
requestGetConsoleScreenshot =
  req
    "GetConsoleScreenshot"
    "fixture/GetConsoleScreenshot.yaml"

requestDisableVgwRoutePropagation :: DisableVgwRoutePropagation -> TestTree
requestDisableVgwRoutePropagation =
  req
    "DisableVgwRoutePropagation"
    "fixture/DisableVgwRoutePropagation.yaml"

requestDescribeTransitGatewayMulticastDomains :: DescribeTransitGatewayMulticastDomains -> TestTree
requestDescribeTransitGatewayMulticastDomains =
  req
    "DescribeTransitGatewayMulticastDomains"
    "fixture/DescribeTransitGatewayMulticastDomains.yaml"

requestDescribeSubnets :: DescribeSubnets -> TestTree
requestDescribeSubnets =
  req
    "DescribeSubnets"
    "fixture/DescribeSubnets.yaml"

requestUnmonitorInstances :: UnmonitorInstances -> TestTree
requestUnmonitorInstances =
  req
    "UnmonitorInstances"
    "fixture/UnmonitorInstances.yaml"

requestCancelSpotInstanceRequests :: CancelSpotInstanceRequests -> TestTree
requestCancelSpotInstanceRequests =
  req
    "CancelSpotInstanceRequests"
    "fixture/CancelSpotInstanceRequests.yaml"

requestCreateSpotDatafeedSubscription :: CreateSpotDatafeedSubscription -> TestTree
requestCreateSpotDatafeedSubscription =
  req
    "CreateSpotDatafeedSubscription"
    "fixture/CreateSpotDatafeedSubscription.yaml"

requestDisassociateRouteTable :: DisassociateRouteTable -> TestTree
requestDisassociateRouteTable =
  req
    "DisassociateRouteTable"
    "fixture/DisassociateRouteTable.yaml"

requestDescribeTransitGatewayConnectPeers :: DescribeTransitGatewayConnectPeers -> TestTree
requestDescribeTransitGatewayConnectPeers =
  req
    "DescribeTransitGatewayConnectPeers"
    "fixture/DescribeTransitGatewayConnectPeers.yaml"

requestModifyVpnTunnelCertificate :: ModifyVpnTunnelCertificate -> TestTree
requestModifyVpnTunnelCertificate =
  req
    "ModifyVpnTunnelCertificate"
    "fixture/ModifyVpnTunnelCertificate.yaml"

requestRestoreManagedPrefixListVersion :: RestoreManagedPrefixListVersion -> TestTree
requestRestoreManagedPrefixListVersion =
  req
    "RestoreManagedPrefixListVersion"
    "fixture/RestoreManagedPrefixListVersion.yaml"

requestModifyAddressAttribute :: ModifyAddressAttribute -> TestTree
requestModifyAddressAttribute =
  req
    "ModifyAddressAttribute"
    "fixture/ModifyAddressAttribute.yaml"

requestCreateVpnConnection :: CreateVpnConnection -> TestTree
requestCreateVpnConnection =
  req
    "CreateVpnConnection"
    "fixture/CreateVpnConnection.yaml"

requestAssociateSubnetCidrBlock :: AssociateSubnetCidrBlock -> TestTree
requestAssociateSubnetCidrBlock =
  req
    "AssociateSubnetCidrBlock"
    "fixture/AssociateSubnetCidrBlock.yaml"

requestAttachClassicLinkVpc :: AttachClassicLinkVpc -> TestTree
requestAttachClassicLinkVpc =
  req
    "AttachClassicLinkVpc"
    "fixture/AttachClassicLinkVpc.yaml"

requestDescribeSpotPriceHistory :: DescribeSpotPriceHistory -> TestTree
requestDescribeSpotPriceHistory =
  req
    "DescribeSpotPriceHistory"
    "fixture/DescribeSpotPriceHistory.yaml"

requestDeleteQueuedReservedInstances :: DeleteQueuedReservedInstances -> TestTree
requestDeleteQueuedReservedInstances =
  req
    "DeleteQueuedReservedInstances"
    "fixture/DeleteQueuedReservedInstances.yaml"

requestDescribeAggregateIdFormat :: DescribeAggregateIdFormat -> TestTree
requestDescribeAggregateIdFormat =
  req
    "DescribeAggregateIdFormat"
    "fixture/DescribeAggregateIdFormat.yaml"

requestDescribeReservedInstancesListings :: DescribeReservedInstancesListings -> TestTree
requestDescribeReservedInstancesListings =
  req
    "DescribeReservedInstancesListings"
    "fixture/DescribeReservedInstancesListings.yaml"

requestCopyImage :: CopyImage -> TestTree
requestCopyImage =
  req
    "CopyImage"
    "fixture/CopyImage.yaml"

requestCreateLocalGatewayRouteTableVpcAssociation :: CreateLocalGatewayRouteTableVpcAssociation -> TestTree
requestCreateLocalGatewayRouteTableVpcAssociation =
  req
    "CreateLocalGatewayRouteTableVpcAssociation"
    "fixture/CreateLocalGatewayRouteTableVpcAssociation.yaml"

requestDescribeCarrierGateways :: DescribeCarrierGateways -> TestTree
requestDescribeCarrierGateways =
  req
    "DescribeCarrierGateways"
    "fixture/DescribeCarrierGateways.yaml"

requestDeleteInternetGateway :: DeleteInternetGateway -> TestTree
requestDeleteInternetGateway =
  req
    "DeleteInternetGateway"
    "fixture/DeleteInternetGateway.yaml"

requestCreateFleet :: CreateFleet -> TestTree
requestCreateFleet =
  req
    "CreateFleet"
    "fixture/CreateFleet.yaml"

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

requestImportClientVpnClientCertificateRevocationList :: ImportClientVpnClientCertificateRevocationList -> TestTree
requestImportClientVpnClientCertificateRevocationList =
  req
    "ImportClientVpnClientCertificateRevocationList"
    "fixture/ImportClientVpnClientCertificateRevocationList.yaml"

requestAssociateClientVpnTargetNetwork :: AssociateClientVpnTargetNetwork -> TestTree
requestAssociateClientVpnTargetNetwork =
  req
    "AssociateClientVpnTargetNetwork"
    "fixture/AssociateClientVpnTargetNetwork.yaml"

requestCancelCapacityReservation :: CancelCapacityReservation -> TestTree
requestCancelCapacityReservation =
  req
    "CancelCapacityReservation"
    "fixture/CancelCapacityReservation.yaml"

requestCancelReservedInstancesListing :: CancelReservedInstancesListing -> TestTree
requestCancelReservedInstancesListing =
  req
    "CancelReservedInstancesListing"
    "fixture/CancelReservedInstancesListing.yaml"

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

requestCreateVpcEndpoint :: CreateVpcEndpoint -> TestTree
requestCreateVpcEndpoint =
  req
    "CreateVpcEndpoint"
    "fixture/CreateVpcEndpoint.yaml"

requestDescribeSnapshots :: DescribeSnapshots -> TestTree
requestDescribeSnapshots =
  req
    "DescribeSnapshots"
    "fixture/DescribeSnapshots.yaml"

requestDescribeImportSnapshotTasks :: DescribeImportSnapshotTasks -> TestTree
requestDescribeImportSnapshotTasks =
  req
    "DescribeImportSnapshotTasks"
    "fixture/DescribeImportSnapshotTasks.yaml"

requestDescribeNetworkInterfaceAttribute :: DescribeNetworkInterfaceAttribute -> TestTree
requestDescribeNetworkInterfaceAttribute =
  req
    "DescribeNetworkInterfaceAttribute"
    "fixture/DescribeNetworkInterfaceAttribute.yaml"

requestDescribeInstanceEventNotificationAttributes :: DescribeInstanceEventNotificationAttributes -> TestTree
requestDescribeInstanceEventNotificationAttributes =
  req
    "DescribeInstanceEventNotificationAttributes"
    "fixture/DescribeInstanceEventNotificationAttributes.yaml"

requestEnableEbsEncryptionByDefault :: EnableEbsEncryptionByDefault -> TestTree
requestEnableEbsEncryptionByDefault =
  req
    "EnableEbsEncryptionByDefault"
    "fixture/EnableEbsEncryptionByDefault.yaml"

requestModifyTrafficMirrorFilterRule :: ModifyTrafficMirrorFilterRule -> TestTree
requestModifyTrafficMirrorFilterRule =
  req
    "ModifyTrafficMirrorFilterRule"
    "fixture/ModifyTrafficMirrorFilterRule.yaml"

requestDescribeCoipPools :: DescribeCoipPools -> TestTree
requestDescribeCoipPools =
  req
    "DescribeCoipPools"
    "fixture/DescribeCoipPools.yaml"

requestCancelExportTask :: CancelExportTask -> TestTree
requestCancelExportTask =
  req
    "CancelExportTask"
    "fixture/CancelExportTask.yaml"

requestEnableVolumeIO :: EnableVolumeIO -> TestTree
requestEnableVolumeIO =
  req
    "EnableVolumeIO"
    "fixture/EnableVolumeIO.yaml"

requestModifyTransitGateway :: ModifyTransitGateway -> TestTree
requestModifyTransitGateway =
  req
    "ModifyTransitGateway"
    "fixture/ModifyTransitGateway.yaml"

requestDescribeInstanceTypeOfferings :: DescribeInstanceTypeOfferings -> TestTree
requestDescribeInstanceTypeOfferings =
  req
    "DescribeInstanceTypeOfferings"
    "fixture/DescribeInstanceTypeOfferings.yaml"

requestCreateSubnet :: CreateSubnet -> TestTree
requestCreateSubnet =
  req
    "CreateSubnet"
    "fixture/CreateSubnet.yaml"

requestRequestSpotFleet :: RequestSpotFleet -> TestTree
requestRequestSpotFleet =
  req
    "RequestSpotFleet"
    "fixture/RequestSpotFleet.yaml"

requestDeleteVpnConnection :: DeleteVpnConnection -> TestTree
requestDeleteVpnConnection =
  req
    "DeleteVpnConnection"
    "fixture/DeleteVpnConnection.yaml"

requestModifySpotFleetRequest :: ModifySpotFleetRequest -> TestTree
requestModifySpotFleetRequest =
  req
    "ModifySpotFleetRequest"
    "fixture/ModifySpotFleetRequest.yaml"

requestDeregisterImage :: DeregisterImage -> TestTree
requestDeregisterImage =
  req
    "DeregisterImage"
    "fixture/DeregisterImage.yaml"

requestDetachVpnGateway :: DetachVpnGateway -> TestTree
requestDetachVpnGateway =
  req
    "DetachVpnGateway"
    "fixture/DetachVpnGateway.yaml"

requestCreateNetworkInterface :: CreateNetworkInterface -> TestTree
requestCreateNetworkInterface =
  req
    "CreateNetworkInterface"
    "fixture/CreateNetworkInterface.yaml"

requestModifyNetworkInterfaceAttribute :: ModifyNetworkInterfaceAttribute -> TestTree
requestModifyNetworkInterfaceAttribute =
  req
    "ModifyNetworkInterfaceAttribute"
    "fixture/ModifyNetworkInterfaceAttribute.yaml"

requestCreateNatGateway :: CreateNatGateway -> TestTree
requestCreateNatGateway =
  req
    "CreateNatGateway"
    "fixture/CreateNatGateway.yaml"

requestGetAssociatedEnclaveCertificateIamRoles :: GetAssociatedEnclaveCertificateIamRoles -> TestTree
requestGetAssociatedEnclaveCertificateIamRoles =
  req
    "GetAssociatedEnclaveCertificateIamRoles"
    "fixture/GetAssociatedEnclaveCertificateIamRoles.yaml"

requestCreateInternetGateway :: CreateInternetGateway -> TestTree
requestCreateInternetGateway =
  req
    "CreateInternetGateway"
    "fixture/CreateInternetGateway.yaml"

requestEnableTransitGatewayRouteTablePropagation :: EnableTransitGatewayRouteTablePropagation -> TestTree
requestEnableTransitGatewayRouteTablePropagation =
  req
    "EnableTransitGatewayRouteTablePropagation"
    "fixture/EnableTransitGatewayRouteTablePropagation.yaml"

requestResetAddressAttribute :: ResetAddressAttribute -> TestTree
requestResetAddressAttribute =
  req
    "ResetAddressAttribute"
    "fixture/ResetAddressAttribute.yaml"

requestDescribeTransitGatewayConnects :: DescribeTransitGatewayConnects -> TestTree
requestDescribeTransitGatewayConnects =
  req
    "DescribeTransitGatewayConnects"
    "fixture/DescribeTransitGatewayConnects.yaml"

requestDeletePlacementGroup :: DeletePlacementGroup -> TestTree
requestDeletePlacementGroup =
  req
    "DeletePlacementGroup"
    "fixture/DeletePlacementGroup.yaml"

requestDescribeInstanceTypes :: DescribeInstanceTypes -> TestTree
requestDescribeInstanceTypes =
  req
    "DescribeInstanceTypes"
    "fixture/DescribeInstanceTypes.yaml"

requestDescribeBundleTasks :: DescribeBundleTasks -> TestTree
requestDescribeBundleTasks =
  req
    "DescribeBundleTasks"
    "fixture/DescribeBundleTasks.yaml"

requestModifySubnetAttribute :: ModifySubnetAttribute -> TestTree
requestModifySubnetAttribute =
  req
    "ModifySubnetAttribute"
    "fixture/ModifySubnetAttribute.yaml"

requestDescribeSecurityGroups :: DescribeSecurityGroups -> TestTree
requestDescribeSecurityGroups =
  req
    "DescribeSecurityGroups"
    "fixture/DescribeSecurityGroups.yaml"

requestCreateClientVpnRoute :: CreateClientVpnRoute -> TestTree
requestCreateClientVpnRoute =
  req
    "CreateClientVpnRoute"
    "fixture/CreateClientVpnRoute.yaml"

requestDisassociateSubnetCidrBlock :: DisassociateSubnetCidrBlock -> TestTree
requestDisassociateSubnetCidrBlock =
  req
    "DisassociateSubnetCidrBlock"
    "fixture/DisassociateSubnetCidrBlock.yaml"

requestDescribeSpotDatafeedSubscription :: DescribeSpotDatafeedSubscription -> TestTree
requestDescribeSpotDatafeedSubscription =
  req
    "DescribeSpotDatafeedSubscription"
    "fixture/DescribeSpotDatafeedSubscription.yaml"

requestCreateInstanceExportTask :: CreateInstanceExportTask -> TestTree
requestCreateInstanceExportTask =
  req
    "CreateInstanceExportTask"
    "fixture/CreateInstanceExportTask.yaml"

requestDisassociateClientVpnTargetNetwork :: DisassociateClientVpnTargetNetwork -> TestTree
requestDisassociateClientVpnTargetNetwork =
  req
    "DisassociateClientVpnTargetNetwork"
    "fixture/DisassociateClientVpnTargetNetwork.yaml"

requestSendDiagnosticInterrupt :: SendDiagnosticInterrupt -> TestTree
requestSendDiagnosticInterrupt =
  req
    "SendDiagnosticInterrupt"
    "fixture/SendDiagnosticInterrupt.yaml"

requestDescribeVpcAttribute :: DescribeVpcAttribute -> TestTree
requestDescribeVpcAttribute =
  req
    "DescribeVpcAttribute"
    "fixture/DescribeVpcAttribute.yaml"

requestDescribeSecurityGroupReferences :: DescribeSecurityGroupReferences -> TestTree
requestDescribeSecurityGroupReferences =
  req
    "DescribeSecurityGroupReferences"
    "fixture/DescribeSecurityGroupReferences.yaml"

requestModifyCapacityReservation :: ModifyCapacityReservation -> TestTree
requestModifyCapacityReservation =
  req
    "ModifyCapacityReservation"
    "fixture/ModifyCapacityReservation.yaml"

requestDetachInternetGateway :: DetachInternetGateway -> TestTree
requestDetachInternetGateway =
  req
    "DetachInternetGateway"
    "fixture/DetachInternetGateway.yaml"

requestCreateVolume :: CreateVolume -> TestTree
requestCreateVolume =
  req
    "CreateVolume"
    "fixture/CreateVolume.yaml"

requestExportClientVpnClientConfiguration :: ExportClientVpnClientConfiguration -> TestTree
requestExportClientVpnClientConfiguration =
  req
    "ExportClientVpnClientConfiguration"
    "fixture/ExportClientVpnClientConfiguration.yaml"

requestRevokeSecurityGroupEgress :: RevokeSecurityGroupEgress -> TestTree
requestRevokeSecurityGroupEgress =
  req
    "RevokeSecurityGroupEgress"
    "fixture/RevokeSecurityGroupEgress.yaml"

requestDeleteKeyPair :: DeleteKeyPair -> TestTree
requestDeleteKeyPair =
  req
    "DeleteKeyPair"
    "fixture/DeleteKeyPair.yaml"

requestModifyInstanceMetadataOptions :: ModifyInstanceMetadataOptions -> TestTree
requestModifyInstanceMetadataOptions =
  req
    "ModifyInstanceMetadataOptions"
    "fixture/ModifyInstanceMetadataOptions.yaml"

requestDescribeEgressOnlyInternetGateways :: DescribeEgressOnlyInternetGateways -> TestTree
requestDescribeEgressOnlyInternetGateways =
  req
    "DescribeEgressOnlyInternetGateways"
    "fixture/DescribeEgressOnlyInternetGateways.yaml"

requestModifyTrafficMirrorFilterNetworkServices :: ModifyTrafficMirrorFilterNetworkServices -> TestTree
requestModifyTrafficMirrorFilterNetworkServices =
  req
    "ModifyTrafficMirrorFilterNetworkServices"
    "fixture/ModifyTrafficMirrorFilterNetworkServices.yaml"

requestImportSnapshot :: ImportSnapshot -> TestTree
requestImportSnapshot =
  req
    "ImportSnapshot"
    "fixture/ImportSnapshot.yaml"

requestDescribeImages :: DescribeImages -> TestTree
requestDescribeImages =
  req
    "DescribeImages"
    "fixture/DescribeImages.yaml"

requestDeprovisionByoipCidr :: DeprovisionByoipCidr -> TestTree
requestDeprovisionByoipCidr =
  req
    "DeprovisionByoipCidr"
    "fixture/DeprovisionByoipCidr.yaml"

requestDescribeAddressesAttribute :: DescribeAddressesAttribute -> TestTree
requestDescribeAddressesAttribute =
  req
    "DescribeAddressesAttribute"
    "fixture/DescribeAddressesAttribute.yaml"

requestAcceptVpcPeeringConnection :: AcceptVpcPeeringConnection -> TestTree
requestAcceptVpcPeeringConnection =
  req
    "AcceptVpcPeeringConnection"
    "fixture/AcceptVpcPeeringConnection.yaml"

requestDescribeMovingAddresses :: DescribeMovingAddresses -> TestTree
requestDescribeMovingAddresses =
  req
    "DescribeMovingAddresses"
    "fixture/DescribeMovingAddresses.yaml"

requestCreateVpcEndpointConnectionNotification :: CreateVpcEndpointConnectionNotification -> TestTree
requestCreateVpcEndpointConnectionNotification =
  req
    "CreateVpcEndpointConnectionNotification"
    "fixture/CreateVpcEndpointConnectionNotification.yaml"

requestDescribeFleetHistory :: DescribeFleetHistory -> TestTree
requestDescribeFleetHistory =
  req
    "DescribeFleetHistory"
    "fixture/DescribeFleetHistory.yaml"

requestDeleteVpcEndpointServiceConfigurations :: DeleteVpcEndpointServiceConfigurations -> TestTree
requestDeleteVpcEndpointServiceConfigurations =
  req
    "DeleteVpcEndpointServiceConfigurations"
    "fixture/DeleteVpcEndpointServiceConfigurations.yaml"

requestCreateVpc :: CreateVpc -> TestTree
requestCreateVpc =
  req
    "CreateVpc"
    "fixture/CreateVpc.yaml"

requestSearchLocalGatewayRoutes :: SearchLocalGatewayRoutes -> TestTree
requestSearchLocalGatewayRoutes =
  req
    "SearchLocalGatewayRoutes"
    "fixture/SearchLocalGatewayRoutes.yaml"

requestCreateTrafficMirrorTarget :: CreateTrafficMirrorTarget -> TestTree
requestCreateTrafficMirrorTarget =
  req
    "CreateTrafficMirrorTarget"
    "fixture/CreateTrafficMirrorTarget.yaml"

requestDescribeVolumeStatus :: DescribeVolumeStatus -> TestTree
requestDescribeVolumeStatus =
  req
    "DescribeVolumeStatus"
    "fixture/DescribeVolumeStatus.yaml"

requestDescribeVolumeAttribute :: DescribeVolumeAttribute -> TestTree
requestDescribeVolumeAttribute =
  req
    "DescribeVolumeAttribute"
    "fixture/DescribeVolumeAttribute.yaml"

requestDeleteClientVpnRoute :: DeleteClientVpnRoute -> TestTree
requestDeleteClientVpnRoute =
  req
    "DeleteClientVpnRoute"
    "fixture/DeleteClientVpnRoute.yaml"

requestModifyVpcPeeringConnectionOptions :: ModifyVpcPeeringConnectionOptions -> TestTree
requestModifyVpcPeeringConnectionOptions =
  req
    "ModifyVpcPeeringConnectionOptions"
    "fixture/ModifyVpcPeeringConnectionOptions.yaml"

requestDescribeSpotFleetInstances :: DescribeSpotFleetInstances -> TestTree
requestDescribeSpotFleetInstances =
  req
    "DescribeSpotFleetInstances"
    "fixture/DescribeSpotFleetInstances.yaml"

requestDescribePrincipalIdFormat :: DescribePrincipalIdFormat -> TestTree
requestDescribePrincipalIdFormat =
  req
    "DescribePrincipalIdFormat"
    "fixture/DescribePrincipalIdFormat.yaml"

requestModifyInstanceCreditSpecification :: ModifyInstanceCreditSpecification -> TestTree
requestModifyInstanceCreditSpecification =
  req
    "ModifyInstanceCreditSpecification"
    "fixture/ModifyInstanceCreditSpecification.yaml"

requestDisassociateTransitGatewayMulticastDomain :: DisassociateTransitGatewayMulticastDomain -> TestTree
requestDisassociateTransitGatewayMulticastDomain =
  req
    "DisassociateTransitGatewayMulticastDomain"
    "fixture/DisassociateTransitGatewayMulticastDomain.yaml"

requestDescribeManagedPrefixLists :: DescribeManagedPrefixLists -> TestTree
requestDescribeManagedPrefixLists =
  req
    "DescribeManagedPrefixLists"
    "fixture/DescribeManagedPrefixLists.yaml"

requestGetPasswordData :: GetPasswordData -> TestTree
requestGetPasswordData =
  req
    "GetPasswordData"
    "fixture/GetPasswordData.yaml"

requestDeleteVolume :: DeleteVolume -> TestTree
requestDeleteVolume =
  req
    "DeleteVolume"
    "fixture/DeleteVolume.yaml"

requestDescribeTransitGateways :: DescribeTransitGateways -> TestTree
requestDescribeTransitGateways =
  req
    "DescribeTransitGateways"
    "fixture/DescribeTransitGateways.yaml"

requestDescribeSpotFleetRequests :: DescribeSpotFleetRequests -> TestTree
requestDescribeSpotFleetRequests =
  req
    "DescribeSpotFleetRequests"
    "fixture/DescribeSpotFleetRequests.yaml"

requestDescribeClientVpnConnections :: DescribeClientVpnConnections -> TestTree
requestDescribeClientVpnConnections =
  req
    "DescribeClientVpnConnections"
    "fixture/DescribeClientVpnConnections.yaml"

requestSearchTransitGatewayMulticastGroups :: SearchTransitGatewayMulticastGroups -> TestTree
requestSearchTransitGatewayMulticastGroups =
  req
    "SearchTransitGatewayMulticastGroups"
    "fixture/SearchTransitGatewayMulticastGroups.yaml"

requestModifyVpcAttribute :: ModifyVpcAttribute -> TestTree
requestModifyVpcAttribute =
  req
    "ModifyVpcAttribute"
    "fixture/ModifyVpcAttribute.yaml"

requestRevokeSecurityGroupIngress :: RevokeSecurityGroupIngress -> TestTree
requestRevokeSecurityGroupIngress =
  req
    "RevokeSecurityGroupIngress"
    "fixture/RevokeSecurityGroupIngress.yaml"

requestDescribeHostReservationOfferings :: DescribeHostReservationOfferings -> TestTree
requestDescribeHostReservationOfferings =
  req
    "DescribeHostReservationOfferings"
    "fixture/DescribeHostReservationOfferings.yaml"

requestDescribeTransitGatewayRouteTables :: DescribeTransitGatewayRouteTables -> TestTree
requestDescribeTransitGatewayRouteTables =
  req
    "DescribeTransitGatewayRouteTables"
    "fixture/DescribeTransitGatewayRouteTables.yaml"

requestDescribeNetworkAcls :: DescribeNetworkAcls -> TestTree
requestDescribeNetworkAcls =
  req
    "DescribeNetworkAcls"
    "fixture/DescribeNetworkAcls.yaml"

requestRegisterTransitGatewayMulticastGroupMembers :: RegisterTransitGatewayMulticastGroupMembers -> TestTree
requestRegisterTransitGatewayMulticastGroupMembers =
  req
    "RegisterTransitGatewayMulticastGroupMembers"
    "fixture/RegisterTransitGatewayMulticastGroupMembers.yaml"

requestDescribeHosts :: DescribeHosts -> TestTree
requestDescribeHosts =
  req
    "DescribeHosts"
    "fixture/DescribeHosts.yaml"

requestDescribeVpnGateways :: DescribeVpnGateways -> TestTree
requestDescribeVpnGateways =
  req
    "DescribeVpnGateways"
    "fixture/DescribeVpnGateways.yaml"

requestDescribeHostReservations :: DescribeHostReservations -> TestTree
requestDescribeHostReservations =
  req
    "DescribeHostReservations"
    "fixture/DescribeHostReservations.yaml"

requestDeleteManagedPrefixList :: DeleteManagedPrefixList -> TestTree
requestDeleteManagedPrefixList =
  req
    "DeleteManagedPrefixList"
    "fixture/DeleteManagedPrefixList.yaml"

requestRejectVpcPeeringConnection :: RejectVpcPeeringConnection -> TestTree
requestRejectVpcPeeringConnection =
  req
    "RejectVpcPeeringConnection"
    "fixture/RejectVpcPeeringConnection.yaml"

requestResetImageAttribute :: ResetImageAttribute -> TestTree
requestResetImageAttribute =
  req
    "ResetImageAttribute"
    "fixture/ResetImageAttribute.yaml"

requestDescribeScheduledInstances :: DescribeScheduledInstances -> TestTree
requestDescribeScheduledInstances =
  req
    "DescribeScheduledInstances"
    "fixture/DescribeScheduledInstances.yaml"

requestAssociateEnclaveCertificateIamRole :: AssociateEnclaveCertificateIamRole -> TestTree
requestAssociateEnclaveCertificateIamRole =
  req
    "AssociateEnclaveCertificateIamRole"
    "fixture/AssociateEnclaveCertificateIamRole.yaml"

requestModifyTransitGatewayPrefixListReference :: ModifyTransitGatewayPrefixListReference -> TestTree
requestModifyTransitGatewayPrefixListReference =
  req
    "ModifyTransitGatewayPrefixListReference"
    "fixture/ModifyTransitGatewayPrefixListReference.yaml"

requestDescribeFpgaImageAttribute :: DescribeFpgaImageAttribute -> TestTree
requestDescribeFpgaImageAttribute =
  req
    "DescribeFpgaImageAttribute"
    "fixture/DescribeFpgaImageAttribute.yaml"

requestAdvertiseByoipCidr :: AdvertiseByoipCidr -> TestTree
requestAdvertiseByoipCidr =
  req
    "AdvertiseByoipCidr"
    "fixture/AdvertiseByoipCidr.yaml"

requestDeleteVpnConnectionRoute :: DeleteVpnConnectionRoute -> TestTree
requestDeleteVpnConnectionRoute =
  req
    "DeleteVpnConnectionRoute"
    "fixture/DeleteVpnConnectionRoute.yaml"

requestDescribeVpcEndpointServicePermissions :: DescribeVpcEndpointServicePermissions -> TestTree
requestDescribeVpcEndpointServicePermissions =
  req
    "DescribeVpcEndpointServicePermissions"
    "fixture/DescribeVpcEndpointServicePermissions.yaml"

requestDescribeVpcEndpointConnections :: DescribeVpcEndpointConnections -> TestTree
requestDescribeVpcEndpointConnections =
  req
    "DescribeVpcEndpointConnections"
    "fixture/DescribeVpcEndpointConnections.yaml"

requestDescribeNetworkInterfacePermissions :: DescribeNetworkInterfacePermissions -> TestTree
requestDescribeNetworkInterfacePermissions =
  req
    "DescribeNetworkInterfacePermissions"
    "fixture/DescribeNetworkInterfacePermissions.yaml"

requestCreateTrafficMirrorSession :: CreateTrafficMirrorSession -> TestTree
requestCreateTrafficMirrorSession =
  req
    "CreateTrafficMirrorSession"
    "fixture/CreateTrafficMirrorSession.yaml"

requestRegisterInstanceEventNotificationAttributes :: RegisterInstanceEventNotificationAttributes -> TestTree
requestRegisterInstanceEventNotificationAttributes =
  req
    "RegisterInstanceEventNotificationAttributes"
    "fixture/RegisterInstanceEventNotificationAttributes.yaml"

requestRejectTransitGatewayMulticastDomainAssociations :: RejectTransitGatewayMulticastDomainAssociations -> TestTree
requestRejectTransitGatewayMulticastDomainAssociations =
  req
    "RejectTransitGatewayMulticastDomainAssociations"
    "fixture/RejectTransitGatewayMulticastDomainAssociations.yaml"

requestDeleteDhcpOptions :: DeleteDhcpOptions -> TestTree
requestDeleteDhcpOptions =
  req
    "DeleteDhcpOptions"
    "fixture/DeleteDhcpOptions.yaml"

requestDeleteTransitGateway :: DeleteTransitGateway -> TestTree
requestDeleteTransitGateway =
  req
    "DeleteTransitGateway"
    "fixture/DeleteTransitGateway.yaml"

requestEnableVpcClassicLinkDnsSupport :: EnableVpcClassicLinkDnsSupport -> TestTree
requestEnableVpcClassicLinkDnsSupport =
  req
    "EnableVpcClassicLinkDnsSupport"
    "fixture/EnableVpcClassicLinkDnsSupport.yaml"

requestDescribeRegions :: DescribeRegions -> TestTree
requestDescribeRegions =
  req
    "DescribeRegions"
    "fixture/DescribeRegions.yaml"

requestCreateEgressOnlyInternetGateway :: CreateEgressOnlyInternetGateway -> TestTree
requestCreateEgressOnlyInternetGateway =
  req
    "CreateEgressOnlyInternetGateway"
    "fixture/CreateEgressOnlyInternetGateway.yaml"

requestCreateTransitGateway :: CreateTransitGateway -> TestTree
requestCreateTransitGateway =
  req
    "CreateTransitGateway"
    "fixture/CreateTransitGateway.yaml"

requestDeleteClientVpnEndpoint :: DeleteClientVpnEndpoint -> TestTree
requestDeleteClientVpnEndpoint =
  req
    "DeleteClientVpnEndpoint"
    "fixture/DeleteClientVpnEndpoint.yaml"

requestExportClientVpnClientCertificateRevocationList :: ExportClientVpnClientCertificateRevocationList -> TestTree
requestExportClientVpnClientCertificateRevocationList =
  req
    "ExportClientVpnClientCertificateRevocationList"
    "fixture/ExportClientVpnClientCertificateRevocationList.yaml"

requestCreateLaunchTemplateVersion :: CreateLaunchTemplateVersion -> TestTree
requestCreateLaunchTemplateVersion =
  req
    "CreateLaunchTemplateVersion"
    "fixture/CreateLaunchTemplateVersion.yaml"

requestCreateSnapshots :: CreateSnapshots -> TestTree
requestCreateSnapshots =
  req
    "CreateSnapshots"
    "fixture/CreateSnapshots.yaml"

requestModifyDefaultCreditSpecification :: ModifyDefaultCreditSpecification -> TestTree
requestModifyDefaultCreditSpecification =
  req
    "ModifyDefaultCreditSpecification"
    "fixture/ModifyDefaultCreditSpecification.yaml"

requestApplySecurityGroupsToClientVpnTargetNetwork :: ApplySecurityGroupsToClientVpnTargetNetwork -> TestTree
requestApplySecurityGroupsToClientVpnTargetNetwork =
  req
    "ApplySecurityGroupsToClientVpnTargetNetwork"
    "fixture/ApplySecurityGroupsToClientVpnTargetNetwork.yaml"

requestAttachVpnGateway :: AttachVpnGateway -> TestTree
requestAttachVpnGateway =
  req
    "AttachVpnGateway"
    "fixture/AttachVpnGateway.yaml"

requestCreateVpnConnectionRoute :: CreateVpnConnectionRoute -> TestTree
requestCreateVpnConnectionRoute =
  req
    "CreateVpnConnectionRoute"
    "fixture/CreateVpnConnectionRoute.yaml"

requestDescribeKeyPairs :: DescribeKeyPairs -> TestTree
requestDescribeKeyPairs =
  req
    "DescribeKeyPairs"
    "fixture/DescribeKeyPairs.yaml"

requestAllocateAddress :: AllocateAddress -> TestTree
requestAllocateAddress =
  req
    "AllocateAddress"
    "fixture/AllocateAddress.yaml"

requestDeleteTrafficMirrorSession :: DeleteTrafficMirrorSession -> TestTree
requestDeleteTrafficMirrorSession =
  req
    "DeleteTrafficMirrorSession"
    "fixture/DeleteTrafficMirrorSession.yaml"

requestGetManagedPrefixListEntries :: GetManagedPrefixListEntries -> TestTree
requestGetManagedPrefixListEntries =
  req
    "GetManagedPrefixListEntries"
    "fixture/GetManagedPrefixListEntries.yaml"

requestCreateFpgaImage :: CreateFpgaImage -> TestTree
requestCreateFpgaImage =
  req
    "CreateFpgaImage"
    "fixture/CreateFpgaImage.yaml"

requestExportImage :: ExportImage -> TestTree
requestExportImage =
  req
    "ExportImage"
    "fixture/ExportImage.yaml"

requestRejectTransitGatewayPeeringAttachment :: RejectTransitGatewayPeeringAttachment -> TestTree
requestRejectTransitGatewayPeeringAttachment =
  req
    "RejectTransitGatewayPeeringAttachment"
    "fixture/RejectTransitGatewayPeeringAttachment.yaml"

requestDescribeConversionTasks :: DescribeConversionTasks -> TestTree
requestDescribeConversionTasks =
  req
    "DescribeConversionTasks"
    "fixture/DescribeConversionTasks.yaml"

requestWithdrawByoipCidr :: WithdrawByoipCidr -> TestTree
requestWithdrawByoipCidr =
  req
    "WithdrawByoipCidr"
    "fixture/WithdrawByoipCidr.yaml"

requestDeleteTrafficMirrorFilterRule :: DeleteTrafficMirrorFilterRule -> TestTree
requestDeleteTrafficMirrorFilterRule =
  req
    "DeleteTrafficMirrorFilterRule"
    "fixture/DeleteTrafficMirrorFilterRule.yaml"

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

requestAcceptTransitGatewayVpcAttachment :: AcceptTransitGatewayVpcAttachment -> TestTree
requestAcceptTransitGatewayVpcAttachment =
  req
    "AcceptTransitGatewayVpcAttachment"
    "fixture/AcceptTransitGatewayVpcAttachment.yaml"

requestDisableVpcClassicLinkDnsSupport :: DisableVpcClassicLinkDnsSupport -> TestTree
requestDisableVpcClassicLinkDnsSupport =
  req
    "DisableVpcClassicLinkDnsSupport"
    "fixture/DisableVpcClassicLinkDnsSupport.yaml"

requestGetLaunchTemplateData :: GetLaunchTemplateData -> TestTree
requestGetLaunchTemplateData =
  req
    "GetLaunchTemplateData"
    "fixture/GetLaunchTemplateData.yaml"

requestDescribeReservedInstances :: DescribeReservedInstances -> TestTree
requestDescribeReservedInstances =
  req
    "DescribeReservedInstances"
    "fixture/DescribeReservedInstances.yaml"

requestModifyFpgaImageAttribute :: ModifyFpgaImageAttribute -> TestTree
requestModifyFpgaImageAttribute =
  req
    "ModifyFpgaImageAttribute"
    "fixture/ModifyFpgaImageAttribute.yaml"

requestEnableVpcClassicLink :: EnableVpcClassicLink -> TestTree
requestEnableVpcClassicLink =
  req
    "EnableVpcClassicLink"
    "fixture/EnableVpcClassicLink.yaml"

requestAttachInternetGateway :: AttachInternetGateway -> TestTree
requestAttachInternetGateway =
  req
    "AttachInternetGateway"
    "fixture/AttachInternetGateway.yaml"

requestDescribePublicIpv4Pools :: DescribePublicIpv4Pools -> TestTree
requestDescribePublicIpv4Pools =
  req
    "DescribePublicIpv4Pools"
    "fixture/DescribePublicIpv4Pools.yaml"

requestCreateCustomerGateway :: CreateCustomerGateway -> TestTree
requestCreateCustomerGateway =
  req
    "CreateCustomerGateway"
    "fixture/CreateCustomerGateway.yaml"

requestDescribeIamInstanceProfileAssociations :: DescribeIamInstanceProfileAssociations -> TestTree
requestDescribeIamInstanceProfileAssociations =
  req
    "DescribeIamInstanceProfileAssociations"
    "fixture/DescribeIamInstanceProfileAssociations.yaml"

requestDescribeExportImageTasks :: DescribeExportImageTasks -> TestTree
requestDescribeExportImageTasks =
  req
    "DescribeExportImageTasks"
    "fixture/DescribeExportImageTasks.yaml"

requestProvisionByoipCidr :: ProvisionByoipCidr -> TestTree
requestProvisionByoipCidr =
  req
    "ProvisionByoipCidr"
    "fixture/ProvisionByoipCidr.yaml"

requestCreateReservedInstancesListing :: CreateReservedInstancesListing -> TestTree
requestCreateReservedInstancesListing =
  req
    "CreateReservedInstancesListing"
    "fixture/CreateReservedInstancesListing.yaml"

requestDescribeClientVpnTargetNetworks :: DescribeClientVpnTargetNetworks -> TestTree
requestDescribeClientVpnTargetNetworks =
  req
    "DescribeClientVpnTargetNetworks"
    "fixture/DescribeClientVpnTargetNetworks.yaml"

requestModifyVpnTunnelOptions :: ModifyVpnTunnelOptions -> TestTree
requestModifyVpnTunnelOptions =
  req
    "ModifyVpnTunnelOptions"
    "fixture/ModifyVpnTunnelOptions.yaml"

requestModifyInstancePlacement :: ModifyInstancePlacement -> TestTree
requestModifyInstancePlacement =
  req
    "ModifyInstancePlacement"
    "fixture/ModifyInstancePlacement.yaml"

requestImportKeyPair :: ImportKeyPair -> TestTree
requestImportKeyPair =
  req
    "ImportKeyPair"
    "fixture/ImportKeyPair.yaml"

requestDescribeNetworkInsightsAnalyses :: DescribeNetworkInsightsAnalyses -> TestTree
requestDescribeNetworkInsightsAnalyses =
  req
    "DescribeNetworkInsightsAnalyses"
    "fixture/DescribeNetworkInsightsAnalyses.yaml"

requestDeleteSecurityGroup :: DeleteSecurityGroup -> TestTree
requestDeleteSecurityGroup =
  req
    "DeleteSecurityGroup"
    "fixture/DeleteSecurityGroup.yaml"

requestCreateCarrierGateway :: CreateCarrierGateway -> TestTree
requestCreateCarrierGateway =
  req
    "CreateCarrierGateway"
    "fixture/CreateCarrierGateway.yaml"

requestCreateSnapshot :: CreateSnapshot -> TestTree
requestCreateSnapshot =
  req
    "CreateSnapshot"
    "fixture/CreateSnapshot.yaml"

requestModifyVolume :: ModifyVolume -> TestTree
requestModifyVolume =
  req
    "ModifyVolume"
    "fixture/ModifyVolume.yaml"

requestDeleteNetworkInsightsAnalysis :: DeleteNetworkInsightsAnalysis -> TestTree
requestDeleteNetworkInsightsAnalysis =
  req
    "DeleteNetworkInsightsAnalysis"
    "fixture/DeleteNetworkInsightsAnalysis.yaml"

requestDescribeLocalGatewayRouteTableVpcAssociations :: DescribeLocalGatewayRouteTableVpcAssociations -> TestTree
requestDescribeLocalGatewayRouteTableVpcAssociations =
  req
    "DescribeLocalGatewayRouteTableVpcAssociations"
    "fixture/DescribeLocalGatewayRouteTableVpcAssociations.yaml"

requestCreateTrafficMirrorFilter :: CreateTrafficMirrorFilter -> TestTree
requestCreateTrafficMirrorFilter =
  req
    "CreateTrafficMirrorFilter"
    "fixture/CreateTrafficMirrorFilter.yaml"

requestDeleteSpotDatafeedSubscription :: DeleteSpotDatafeedSubscription -> TestTree
requestDeleteSpotDatafeedSubscription =
  req
    "DeleteSpotDatafeedSubscription"
    "fixture/DeleteSpotDatafeedSubscription.yaml"

requestDescribeInstanceAttribute :: DescribeInstanceAttribute -> TestTree
requestDescribeInstanceAttribute =
  req
    "DescribeInstanceAttribute"
    "fixture/DescribeInstanceAttribute.yaml"

requestCreateCapacityReservation :: CreateCapacityReservation -> TestTree
requestCreateCapacityReservation =
  req
    "CreateCapacityReservation"
    "fixture/CreateCapacityReservation.yaml"

requestDeleteTransitGatewayConnect :: DeleteTransitGatewayConnect -> TestTree
requestDeleteTransitGatewayConnect =
  req
    "DeleteTransitGatewayConnect"
    "fixture/DeleteTransitGatewayConnect.yaml"

requestModifyEbsDefaultKmsKeyId :: ModifyEbsDefaultKmsKeyId -> TestTree
requestModifyEbsDefaultKmsKeyId =
  req
    "ModifyEbsDefaultKmsKeyId"
    "fixture/ModifyEbsDefaultKmsKeyId.yaml"

requestDeleteRoute :: DeleteRoute -> TestTree
requestDeleteRoute =
  req
    "DeleteRoute"
    "fixture/DeleteRoute.yaml"

requestDescribeNetworkInsightsPaths :: DescribeNetworkInsightsPaths -> TestTree
requestDescribeNetworkInsightsPaths =
  req
    "DescribeNetworkInsightsPaths"
    "fixture/DescribeNetworkInsightsPaths.yaml"

requestPurchaseScheduledInstances :: PurchaseScheduledInstances -> TestTree
requestPurchaseScheduledInstances =
  req
    "PurchaseScheduledInstances"
    "fixture/PurchaseScheduledInstances.yaml"

requestCreateTransitGatewayPeeringAttachment :: CreateTransitGatewayPeeringAttachment -> TestTree
requestCreateTransitGatewayPeeringAttachment =
  req
    "CreateTransitGatewayPeeringAttachment"
    "fixture/CreateTransitGatewayPeeringAttachment.yaml"

requestGetDefaultCreditSpecification :: GetDefaultCreditSpecification -> TestTree
requestGetDefaultCreditSpecification =
  req
    "GetDefaultCreditSpecification"
    "fixture/GetDefaultCreditSpecification.yaml"

requestDescribeInternetGateways :: DescribeInternetGateways -> TestTree
requestDescribeInternetGateways =
  req
    "DescribeInternetGateways"
    "fixture/DescribeInternetGateways.yaml"

requestModifyInstanceAttribute :: ModifyInstanceAttribute -> TestTree
requestModifyInstanceAttribute =
  req
    "ModifyInstanceAttribute"
    "fixture/ModifyInstanceAttribute.yaml"

requestCreateSecurityGroup :: CreateSecurityGroup -> TestTree
requestCreateSecurityGroup =
  req
    "CreateSecurityGroup"
    "fixture/CreateSecurityGroup.yaml"

requestCreateTransitGatewayConnect :: CreateTransitGatewayConnect -> TestTree
requestCreateTransitGatewayConnect =
  req
    "CreateTransitGatewayConnect"
    "fixture/CreateTransitGatewayConnect.yaml"

requestReplaceNetworkAclAssociation :: ReplaceNetworkAclAssociation -> TestTree
requestReplaceNetworkAclAssociation =
  req
    "ReplaceNetworkAclAssociation"
    "fixture/ReplaceNetworkAclAssociation.yaml"

requestCreateRoute :: CreateRoute -> TestTree
requestCreateRoute =
  req
    "CreateRoute"
    "fixture/CreateRoute.yaml"

requestDeleteLaunchTemplateVersions :: DeleteLaunchTemplateVersions -> TestTree
requestDeleteLaunchTemplateVersions =
  req
    "DeleteLaunchTemplateVersions"
    "fixture/DeleteLaunchTemplateVersions.yaml"

requestDescribeIdentityIdFormat :: DescribeIdentityIdFormat -> TestTree
requestDescribeIdentityIdFormat =
  req
    "DescribeIdentityIdFormat"
    "fixture/DescribeIdentityIdFormat.yaml"

requestDeleteTrafficMirrorFilter :: DeleteTrafficMirrorFilter -> TestTree
requestDeleteTrafficMirrorFilter =
  req
    "DeleteTrafficMirrorFilter"
    "fixture/DeleteTrafficMirrorFilter.yaml"

requestReplaceRoute :: ReplaceRoute -> TestTree
requestReplaceRoute =
  req
    "ReplaceRoute"
    "fixture/ReplaceRoute.yaml"

requestResetSnapshotAttribute :: ResetSnapshotAttribute -> TestTree
requestResetSnapshotAttribute =
  req
    "ResetSnapshotAttribute"
    "fixture/ResetSnapshotAttribute.yaml"

requestResetEbsDefaultKmsKeyId :: ResetEbsDefaultKmsKeyId -> TestTree
requestResetEbsDefaultKmsKeyId =
  req
    "ResetEbsDefaultKmsKeyId"
    "fixture/ResetEbsDefaultKmsKeyId.yaml"

requestCreateTags :: CreateTags -> TestTree
requestCreateTags =
  req
    "CreateTags"
    "fixture/CreateTags.yaml"

requestBundleInstance :: BundleInstance -> TestTree
requestBundleInstance =
  req
    "BundleInstance"
    "fixture/BundleInstance.yaml"

requestDeleteTransitGatewayPeeringAttachment :: DeleteTransitGatewayPeeringAttachment -> TestTree
requestDeleteTransitGatewayPeeringAttachment =
  req
    "DeleteTransitGatewayPeeringAttachment"
    "fixture/DeleteTransitGatewayPeeringAttachment.yaml"

requestAuthorizeClientVpnIngress :: AuthorizeClientVpnIngress -> TestTree
requestAuthorizeClientVpnIngress =
  req
    "AuthorizeClientVpnIngress"
    "fixture/AuthorizeClientVpnIngress.yaml"

requestModifyLaunchTemplate :: ModifyLaunchTemplate -> TestTree
requestModifyLaunchTemplate =
  req
    "ModifyLaunchTemplate"
    "fixture/ModifyLaunchTemplate.yaml"

requestDeleteCustomerGateway :: DeleteCustomerGateway -> TestTree
requestDeleteCustomerGateway =
  req
    "DeleteCustomerGateway"
    "fixture/DeleteCustomerGateway.yaml"

requestTerminateClientVpnConnections :: TerminateClientVpnConnections -> TestTree
requestTerminateClientVpnConnections =
  req
    "TerminateClientVpnConnections"
    "fixture/TerminateClientVpnConnections.yaml"

requestGetEbsEncryptionByDefault :: GetEbsEncryptionByDefault -> TestTree
requestGetEbsEncryptionByDefault =
  req
    "GetEbsEncryptionByDefault"
    "fixture/GetEbsEncryptionByDefault.yaml"

requestCreateVpcPeeringConnection :: CreateVpcPeeringConnection -> TestTree
requestCreateVpcPeeringConnection =
  req
    "CreateVpcPeeringConnection"
    "fixture/CreateVpcPeeringConnection.yaml"

requestDeleteTransitGatewayVpcAttachment :: DeleteTransitGatewayVpcAttachment -> TestTree
requestDeleteTransitGatewayVpcAttachment =
  req
    "DeleteTransitGatewayVpcAttachment"
    "fixture/DeleteTransitGatewayVpcAttachment.yaml"

requestReplaceIamInstanceProfileAssociation :: ReplaceIamInstanceProfileAssociation -> TestTree
requestReplaceIamInstanceProfileAssociation =
  req
    "ReplaceIamInstanceProfileAssociation"
    "fixture/ReplaceIamInstanceProfileAssociation.yaml"

requestDeleteTransitGatewayConnectPeer :: DeleteTransitGatewayConnectPeer -> TestTree
requestDeleteTransitGatewayConnectPeer =
  req
    "DeleteTransitGatewayConnectPeer"
    "fixture/DeleteTransitGatewayConnectPeer.yaml"

requestAssociateAddress :: AssociateAddress -> TestTree
requestAssociateAddress =
  req
    "AssociateAddress"
    "fixture/AssociateAddress.yaml"

requestCancelSpotFleetRequests :: CancelSpotFleetRequests -> TestTree
requestCancelSpotFleetRequests =
  req
    "CancelSpotFleetRequests"
    "fixture/CancelSpotFleetRequests.yaml"

requestResetNetworkInterfaceAttribute :: ResetNetworkInterfaceAttribute -> TestTree
requestResetNetworkInterfaceAttribute =
  req
    "ResetNetworkInterfaceAttribute"
    "fixture/ResetNetworkInterfaceAttribute.yaml"

requestStartInstances :: StartInstances -> TestTree
requestStartInstances =
  req
    "StartInstances"
    "fixture/StartInstances.yaml"

requestDisassociateTransitGatewayRouteTable :: DisassociateTransitGatewayRouteTable -> TestTree
requestDisassociateTransitGatewayRouteTable =
  req
    "DisassociateTransitGatewayRouteTable"
    "fixture/DisassociateTransitGatewayRouteTable.yaml"

requestCopyFpgaImage :: CopyFpgaImage -> TestTree
requestCopyFpgaImage =
  req
    "CopyFpgaImage"
    "fixture/CopyFpgaImage.yaml"

requestReleaseHosts :: ReleaseHosts -> TestTree
requestReleaseHosts =
  req
    "ReleaseHosts"
    "fixture/ReleaseHosts.yaml"

requestDescribeFastSnapshotRestores :: DescribeFastSnapshotRestores -> TestTree
requestDescribeFastSnapshotRestores =
  req
    "DescribeFastSnapshotRestores"
    "fixture/DescribeFastSnapshotRestores.yaml"

requestDescribeTrafficMirrorFilters :: DescribeTrafficMirrorFilters -> TestTree
requestDescribeTrafficMirrorFilters =
  req
    "DescribeTrafficMirrorFilters"
    "fixture/DescribeTrafficMirrorFilters.yaml"

requestCreateTransitGatewayPrefixListReference :: CreateTransitGatewayPrefixListReference -> TestTree
requestCreateTransitGatewayPrefixListReference =
  req
    "CreateTransitGatewayPrefixListReference"
    "fixture/CreateTransitGatewayPrefixListReference.yaml"

requestDeleteNetworkInterface :: DeleteNetworkInterface -> TestTree
requestDeleteNetworkInterface =
  req
    "DeleteNetworkInterface"
    "fixture/DeleteNetworkInterface.yaml"

requestCreateTransitGatewayRoute :: CreateTransitGatewayRoute -> TestTree
requestCreateTransitGatewayRoute =
  req
    "CreateTransitGatewayRoute"
    "fixture/CreateTransitGatewayRoute.yaml"

requestDeregisterTransitGatewayMulticastGroupSources :: DeregisterTransitGatewayMulticastGroupSources -> TestTree
requestDeregisterTransitGatewayMulticastGroupSources =
  req
    "DeregisterTransitGatewayMulticastGroupSources"
    "fixture/DeregisterTransitGatewayMulticastGroupSources.yaml"

requestDisassociateVpcCidrBlock :: DisassociateVpcCidrBlock -> TestTree
requestDisassociateVpcCidrBlock =
  req
    "DisassociateVpcCidrBlock"
    "fixture/DisassociateVpcCidrBlock.yaml"

requestDescribeTransitGatewayPeeringAttachments :: DescribeTransitGatewayPeeringAttachments -> TestTree
requestDescribeTransitGatewayPeeringAttachments =
  req
    "DescribeTransitGatewayPeeringAttachments"
    "fixture/DescribeTransitGatewayPeeringAttachments.yaml"

requestGetCoipPoolUsage :: GetCoipPoolUsage -> TestTree
requestGetCoipPoolUsage =
  req
    "GetCoipPoolUsage"
    "fixture/GetCoipPoolUsage.yaml"

requestImportImage :: ImportImage -> TestTree
requestImportImage =
  req
    "ImportImage"
    "fixture/ImportImage.yaml"

requestReplaceTransitGatewayRoute :: ReplaceTransitGatewayRoute -> TestTree
requestReplaceTransitGatewayRoute =
  req
    "ReplaceTransitGatewayRoute"
    "fixture/ReplaceTransitGatewayRoute.yaml"

requestCreatePlacementGroup :: CreatePlacementGroup -> TestTree
requestCreatePlacementGroup =
  req
    "CreatePlacementGroup"
    "fixture/CreatePlacementGroup.yaml"

requestCreateDefaultVpc :: CreateDefaultVpc -> TestTree
requestCreateDefaultVpc =
  req
    "CreateDefaultVpc"
    "fixture/CreateDefaultVpc.yaml"

requestCreateNetworkInsightsPath :: CreateNetworkInsightsPath -> TestTree
requestCreateNetworkInsightsPath =
  req
    "CreateNetworkInsightsPath"
    "fixture/CreateNetworkInsightsPath.yaml"

requestModifyTrafficMirrorSession :: ModifyTrafficMirrorSession -> TestTree
requestModifyTrafficMirrorSession =
  req
    "ModifyTrafficMirrorSession"
    "fixture/ModifyTrafficMirrorSession.yaml"

requestRunScheduledInstances :: RunScheduledInstances -> TestTree
requestRunScheduledInstances =
  req
    "RunScheduledInstances"
    "fixture/RunScheduledInstances.yaml"

requestDescribeDhcpOptions :: DescribeDhcpOptions -> TestTree
requestDescribeDhcpOptions =
  req
    "DescribeDhcpOptions"
    "fixture/DescribeDhcpOptions.yaml"

requestDescribeCapacityReservations :: DescribeCapacityReservations -> TestTree
requestDescribeCapacityReservations =
  req
    "DescribeCapacityReservations"
    "fixture/DescribeCapacityReservations.yaml"

requestDescribeCustomerGateways :: DescribeCustomerGateways -> TestTree
requestDescribeCustomerGateways =
  req
    "DescribeCustomerGateways"
    "fixture/DescribeCustomerGateways.yaml"

requestDeleteNatGateway :: DeleteNatGateway -> TestTree
requestDeleteNatGateway =
  req
    "DeleteNatGateway"
    "fixture/DeleteNatGateway.yaml"

requestDescribeClientVpnAuthorizationRules :: DescribeClientVpnAuthorizationRules -> TestTree
requestDescribeClientVpnAuthorizationRules =
  req
    "DescribeClientVpnAuthorizationRules"
    "fixture/DescribeClientVpnAuthorizationRules.yaml"

requestStopInstances :: StopInstances -> TestTree
requestStopInstances =
  req
    "StopInstances"
    "fixture/StopInstances.yaml"

requestReplaceRouteTableAssociation :: ReplaceRouteTableAssociation -> TestTree
requestReplaceRouteTableAssociation =
  req
    "ReplaceRouteTableAssociation"
    "fixture/ReplaceRouteTableAssociation.yaml"

requestDeleteTransitGatewayMulticastDomain :: DeleteTransitGatewayMulticastDomain -> TestTree
requestDeleteTransitGatewayMulticastDomain =
  req
    "DeleteTransitGatewayMulticastDomain"
    "fixture/DeleteTransitGatewayMulticastDomain.yaml"

requestDeleteSubnet :: DeleteSubnet -> TestTree
requestDeleteSubnet =
  req
    "DeleteSubnet"
    "fixture/DeleteSubnet.yaml"

-- Responses

responseAcceptReservedInstancesExchangeQuote :: AcceptReservedInstancesExchangeQuoteResponse -> TestTree
responseAcceptReservedInstancesExchangeQuote =
  res
    "AcceptReservedInstancesExchangeQuoteResponse"
    "fixture/AcceptReservedInstancesExchangeQuoteResponse.proto"
    defaultService
    (Proxy :: Proxy AcceptReservedInstancesExchangeQuote)

responseDescribeInstanceCreditSpecifications :: DescribeInstanceCreditSpecificationsResponse -> TestTree
responseDescribeInstanceCreditSpecifications =
  res
    "DescribeInstanceCreditSpecificationsResponse"
    "fixture/DescribeInstanceCreditSpecificationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInstanceCreditSpecifications)

responseDescribeByoipCidrs :: DescribeByoipCidrsResponse -> TestTree
responseDescribeByoipCidrs =
  res
    "DescribeByoipCidrsResponse"
    "fixture/DescribeByoipCidrsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeByoipCidrs)

responseDescribeLocalGatewayVirtualInterfaces :: DescribeLocalGatewayVirtualInterfacesResponse -> TestTree
responseDescribeLocalGatewayVirtualInterfaces =
  res
    "DescribeLocalGatewayVirtualInterfacesResponse"
    "fixture/DescribeLocalGatewayVirtualInterfacesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLocalGatewayVirtualInterfaces)

responseDeleteLocalGatewayRouteTableVpcAssociation :: DeleteLocalGatewayRouteTableVpcAssociationResponse -> TestTree
responseDeleteLocalGatewayRouteTableVpcAssociation =
  res
    "DeleteLocalGatewayRouteTableVpcAssociationResponse"
    "fixture/DeleteLocalGatewayRouteTableVpcAssociationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteLocalGatewayRouteTableVpcAssociation)

responseDetachVolume :: VolumeAttachment -> TestTree
responseDetachVolume =
  res
    "DetachVolumeResponse"
    "fixture/DetachVolumeResponse.proto"
    defaultService
    (Proxy :: Proxy DetachVolume)

responseCreateTransitGatewayVpcAttachment :: CreateTransitGatewayVpcAttachmentResponse -> TestTree
responseCreateTransitGatewayVpcAttachment =
  res
    "CreateTransitGatewayVpcAttachmentResponse"
    "fixture/CreateTransitGatewayVpcAttachmentResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTransitGatewayVpcAttachment)

responseDeleteVpcEndpointConnectionNotifications :: DeleteVpcEndpointConnectionNotificationsResponse -> TestTree
responseDeleteVpcEndpointConnectionNotifications =
  res
    "DeleteVpcEndpointConnectionNotificationsResponse"
    "fixture/DeleteVpcEndpointConnectionNotificationsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVpcEndpointConnectionNotifications)

responseDeleteNetworkInsightsPath :: DeleteNetworkInsightsPathResponse -> TestTree
responseDeleteNetworkInsightsPath =
  res
    "DeleteNetworkInsightsPathResponse"
    "fixture/DeleteNetworkInsightsPathResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteNetworkInsightsPath)

responseAuthorizeSecurityGroupEgress :: AuthorizeSecurityGroupEgressResponse -> TestTree
responseAuthorizeSecurityGroupEgress =
  res
    "AuthorizeSecurityGroupEgressResponse"
    "fixture/AuthorizeSecurityGroupEgressResponse.proto"
    defaultService
    (Proxy :: Proxy AuthorizeSecurityGroupEgress)

responseModifyManagedPrefixList :: ModifyManagedPrefixListResponse -> TestTree
responseModifyManagedPrefixList =
  res
    "ModifyManagedPrefixListResponse"
    "fixture/ModifyManagedPrefixListResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyManagedPrefixList)

responseDeleteTransitGatewayPrefixListReference :: DeleteTransitGatewayPrefixListReferenceResponse -> TestTree
responseDeleteTransitGatewayPrefixListReference =
  res
    "DeleteTransitGatewayPrefixListReferenceResponse"
    "fixture/DeleteTransitGatewayPrefixListReferenceResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTransitGatewayPrefixListReference)

responseDeleteTransitGatewayRoute :: DeleteTransitGatewayRouteResponse -> TestTree
responseDeleteTransitGatewayRoute =
  res
    "DeleteTransitGatewayRouteResponse"
    "fixture/DeleteTransitGatewayRouteResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTransitGatewayRoute)

responseDescribeVpcPeeringConnections :: DescribeVpcPeeringConnectionsResponse -> TestTree
responseDescribeVpcPeeringConnections =
  res
    "DescribeVpcPeeringConnectionsResponse"
    "fixture/DescribeVpcPeeringConnectionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeVpcPeeringConnections)

responseDescribeInstances :: DescribeInstancesResponse -> TestTree
responseDescribeInstances =
  res
    "DescribeInstancesResponse"
    "fixture/DescribeInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInstances)

responseDeregisterInstanceEventNotificationAttributes :: DeregisterInstanceEventNotificationAttributesResponse -> TestTree
responseDeregisterInstanceEventNotificationAttributes =
  res
    "DeregisterInstanceEventNotificationAttributesResponse"
    "fixture/DeregisterInstanceEventNotificationAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterInstanceEventNotificationAttributes)

responseCreateTransitGatewayMulticastDomain :: CreateTransitGatewayMulticastDomainResponse -> TestTree
responseCreateTransitGatewayMulticastDomain =
  res
    "CreateTransitGatewayMulticastDomainResponse"
    "fixture/CreateTransitGatewayMulticastDomainResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTransitGatewayMulticastDomain)

responseAssociateTransitGatewayMulticastDomain :: AssociateTransitGatewayMulticastDomainResponse -> TestTree
responseAssociateTransitGatewayMulticastDomain =
  res
    "AssociateTransitGatewayMulticastDomainResponse"
    "fixture/AssociateTransitGatewayMulticastDomainResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateTransitGatewayMulticastDomain)

responseReleaseAddress :: ReleaseAddressResponse -> TestTree
responseReleaseAddress =
  res
    "ReleaseAddressResponse"
    "fixture/ReleaseAddressResponse.proto"
    defaultService
    (Proxy :: Proxy ReleaseAddress)

responseDeregisterTransitGatewayMulticastGroupMembers :: DeregisterTransitGatewayMulticastGroupMembersResponse -> TestTree
responseDeregisterTransitGatewayMulticastGroupMembers =
  res
    "DeregisterTransitGatewayMulticastGroupMembersResponse"
    "fixture/DeregisterTransitGatewayMulticastGroupMembersResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterTransitGatewayMulticastGroupMembers)

responseGetHostReservationPurchasePreview :: GetHostReservationPurchasePreviewResponse -> TestTree
responseGetHostReservationPurchasePreview =
  res
    "GetHostReservationPurchasePreviewResponse"
    "fixture/GetHostReservationPurchasePreviewResponse.proto"
    defaultService
    (Proxy :: Proxy GetHostReservationPurchasePreview)

responseCancelBundleTask :: CancelBundleTaskResponse -> TestTree
responseCancelBundleTask =
  res
    "CancelBundleTaskResponse"
    "fixture/CancelBundleTaskResponse.proto"
    defaultService
    (Proxy :: Proxy CancelBundleTask)

responseGetCapacityReservationUsage :: GetCapacityReservationUsageResponse -> TestTree
responseGetCapacityReservationUsage =
  res
    "GetCapacityReservationUsageResponse"
    "fixture/GetCapacityReservationUsageResponse.proto"
    defaultService
    (Proxy :: Proxy GetCapacityReservationUsage)

responseCreateTransitGatewayConnectPeer :: CreateTransitGatewayConnectPeerResponse -> TestTree
responseCreateTransitGatewayConnectPeer =
  res
    "CreateTransitGatewayConnectPeerResponse"
    "fixture/CreateTransitGatewayConnectPeerResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTransitGatewayConnectPeer)

responseModifyVpcTenancy :: ModifyVpcTenancyResponse -> TestTree
responseModifyVpcTenancy =
  res
    "ModifyVpcTenancyResponse"
    "fixture/ModifyVpcTenancyResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyVpcTenancy)

responseCreateVpcEndpointServiceConfiguration :: CreateVpcEndpointServiceConfigurationResponse -> TestTree
responseCreateVpcEndpointServiceConfiguration =
  res
    "CreateVpcEndpointServiceConfigurationResponse"
    "fixture/CreateVpcEndpointServiceConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateVpcEndpointServiceConfiguration)

responseDescribeExportTasks :: DescribeExportTasksResponse -> TestTree
responseDescribeExportTasks =
  res
    "DescribeExportTasksResponse"
    "fixture/DescribeExportTasksResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeExportTasks)

responseGetTransitGatewayMulticastDomainAssociations :: GetTransitGatewayMulticastDomainAssociationsResponse -> TestTree
responseGetTransitGatewayMulticastDomainAssociations =
  res
    "GetTransitGatewayMulticastDomainAssociationsResponse"
    "fixture/GetTransitGatewayMulticastDomainAssociationsResponse.proto"
    defaultService
    (Proxy :: Proxy GetTransitGatewayMulticastDomainAssociations)

responseDisableEbsEncryptionByDefault :: DisableEbsEncryptionByDefaultResponse -> TestTree
responseDisableEbsEncryptionByDefault =
  res
    "DisableEbsEncryptionByDefaultResponse"
    "fixture/DisableEbsEncryptionByDefaultResponse.proto"
    defaultService
    (Proxy :: Proxy DisableEbsEncryptionByDefault)

responseAssociateVpcCidrBlock :: AssociateVpcCidrBlockResponse -> TestTree
responseAssociateVpcCidrBlock =
  res
    "AssociateVpcCidrBlockResponse"
    "fixture/AssociateVpcCidrBlockResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateVpcCidrBlock)

responseCreateNetworkAcl :: CreateNetworkAclResponse -> TestTree
responseCreateNetworkAcl =
  res
    "CreateNetworkAclResponse"
    "fixture/CreateNetworkAclResponse.proto"
    defaultService
    (Proxy :: Proxy CreateNetworkAcl)

responseAcceptTransitGatewayPeeringAttachment :: AcceptTransitGatewayPeeringAttachmentResponse -> TestTree
responseAcceptTransitGatewayPeeringAttachment =
  res
    "AcceptTransitGatewayPeeringAttachmentResponse"
    "fixture/AcceptTransitGatewayPeeringAttachmentResponse.proto"
    defaultService
    (Proxy :: Proxy AcceptTransitGatewayPeeringAttachment)

responseDeleteLaunchTemplate :: DeleteLaunchTemplateResponse -> TestTree
responseDeleteLaunchTemplate =
  res
    "DeleteLaunchTemplateResponse"
    "fixture/DeleteLaunchTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteLaunchTemplate)

responseDeleteVpc :: DeleteVpcResponse -> TestTree
responseDeleteVpc =
  res
    "DeleteVpcResponse"
    "fixture/DeleteVpcResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVpc)

responseDeleteFleets :: DeleteFleetsResponse -> TestTree
responseDeleteFleets =
  res
    "DeleteFleetsResponse"
    "fixture/DeleteFleetsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteFleets)

responseGetAssociatedIpv6PoolCidrs :: GetAssociatedIpv6PoolCidrsResponse -> TestTree
responseGetAssociatedIpv6PoolCidrs =
  res
    "GetAssociatedIpv6PoolCidrsResponse"
    "fixture/GetAssociatedIpv6PoolCidrsResponse.proto"
    defaultService
    (Proxy :: Proxy GetAssociatedIpv6PoolCidrs)

responseDescribeTrafficMirrorSessions :: DescribeTrafficMirrorSessionsResponse -> TestTree
responseDescribeTrafficMirrorSessions =
  res
    "DescribeTrafficMirrorSessionsResponse"
    "fixture/DescribeTrafficMirrorSessionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTrafficMirrorSessions)

responseImportInstance :: ImportInstanceResponse -> TestTree
responseImportInstance =
  res
    "ImportInstanceResponse"
    "fixture/ImportInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy ImportInstance)

responseDescribeLocalGatewayRouteTables :: DescribeLocalGatewayRouteTablesResponse -> TestTree
responseDescribeLocalGatewayRouteTables =
  res
    "DescribeLocalGatewayRouteTablesResponse"
    "fixture/DescribeLocalGatewayRouteTablesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLocalGatewayRouteTables)

responseCreateNetworkInterfacePermission :: CreateNetworkInterfacePermissionResponse -> TestTree
responseCreateNetworkInterfacePermission =
  res
    "CreateNetworkInterfacePermissionResponse"
    "fixture/CreateNetworkInterfacePermissionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateNetworkInterfacePermission)

responseCreateVpnGateway :: CreateVpnGatewayResponse -> TestTree
responseCreateVpnGateway =
  res
    "CreateVpnGatewayResponse"
    "fixture/CreateVpnGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy CreateVpnGateway)

responseGetTransitGatewayRouteTableAssociations :: GetTransitGatewayRouteTableAssociationsResponse -> TestTree
responseGetTransitGatewayRouteTableAssociations =
  res
    "GetTransitGatewayRouteTableAssociationsResponse"
    "fixture/GetTransitGatewayRouteTableAssociationsResponse.proto"
    defaultService
    (Proxy :: Proxy GetTransitGatewayRouteTableAssociations)

responseRejectTransitGatewayVpcAttachment :: RejectTransitGatewayVpcAttachmentResponse -> TestTree
responseRejectTransitGatewayVpcAttachment =
  res
    "RejectTransitGatewayVpcAttachmentResponse"
    "fixture/RejectTransitGatewayVpcAttachmentResponse.proto"
    defaultService
    (Proxy :: Proxy RejectTransitGatewayVpcAttachment)

responseModifyVolumeAttribute :: ModifyVolumeAttributeResponse -> TestTree
responseModifyVolumeAttribute =
  res
    "ModifyVolumeAttributeResponse"
    "fixture/ModifyVolumeAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyVolumeAttribute)

responseDescribePrefixLists :: DescribePrefixListsResponse -> TestTree
responseDescribePrefixLists =
  res
    "DescribePrefixListsResponse"
    "fixture/DescribePrefixListsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePrefixLists)

responseDetachNetworkInterface :: DetachNetworkInterfaceResponse -> TestTree
responseDetachNetworkInterface =
  res
    "DetachNetworkInterfaceResponse"
    "fixture/DetachNetworkInterfaceResponse.proto"
    defaultService
    (Proxy :: Proxy DetachNetworkInterface)

responseDeleteVpcEndpoints :: DeleteVpcEndpointsResponse -> TestTree
responseDeleteVpcEndpoints =
  res
    "DeleteVpcEndpointsResponse"
    "fixture/DeleteVpcEndpointsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVpcEndpoints)

responseDescribeVpcClassicLink :: DescribeVpcClassicLinkResponse -> TestTree
responseDescribeVpcClassicLink =
  res
    "DescribeVpcClassicLinkResponse"
    "fixture/DescribeVpcClassicLinkResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeVpcClassicLink)

responseUpdateSecurityGroupRuleDescriptionsIngress :: UpdateSecurityGroupRuleDescriptionsIngressResponse -> TestTree
responseUpdateSecurityGroupRuleDescriptionsIngress =
  res
    "UpdateSecurityGroupRuleDescriptionsIngressResponse"
    "fixture/UpdateSecurityGroupRuleDescriptionsIngressResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSecurityGroupRuleDescriptionsIngress)

responseDescribeClientVpnEndpoints :: DescribeClientVpnEndpointsResponse -> TestTree
responseDescribeClientVpnEndpoints =
  res
    "DescribeClientVpnEndpointsResponse"
    "fixture/DescribeClientVpnEndpointsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeClientVpnEndpoints)

responseDisassociateAddress :: DisassociateAddressResponse -> TestTree
responseDisassociateAddress =
  res
    "DisassociateAddressResponse"
    "fixture/DisassociateAddressResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateAddress)

responseDescribeScheduledInstanceAvailability :: DescribeScheduledInstanceAvailabilityResponse -> TestTree
responseDescribeScheduledInstanceAvailability =
  res
    "DescribeScheduledInstanceAvailabilityResponse"
    "fixture/DescribeScheduledInstanceAvailabilityResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeScheduledInstanceAvailability)

responseRejectVpcEndpointConnections :: RejectVpcEndpointConnectionsResponse -> TestTree
responseRejectVpcEndpointConnections =
  res
    "RejectVpcEndpointConnectionsResponse"
    "fixture/RejectVpcEndpointConnectionsResponse.proto"
    defaultService
    (Proxy :: Proxy RejectVpcEndpointConnections)

responseCreateTransitGatewayRouteTable :: CreateTransitGatewayRouteTableResponse -> TestTree
responseCreateTransitGatewayRouteTable =
  res
    "CreateTransitGatewayRouteTableResponse"
    "fixture/CreateTransitGatewayRouteTableResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTransitGatewayRouteTable)

responseDescribeTags :: DescribeTagsResponse -> TestTree
responseDescribeTags =
  res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTags)

responseAssociateTransitGatewayRouteTable :: AssociateTransitGatewayRouteTableResponse -> TestTree
responseAssociateTransitGatewayRouteTable =
  res
    "AssociateTransitGatewayRouteTableResponse"
    "fixture/AssociateTransitGatewayRouteTableResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateTransitGatewayRouteTable)

responseDeleteFlowLogs :: DeleteFlowLogsResponse -> TestTree
responseDeleteFlowLogs =
  res
    "DeleteFlowLogsResponse"
    "fixture/DeleteFlowLogsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteFlowLogs)

responseCreateDefaultSubnet :: CreateDefaultSubnetResponse -> TestTree
responseCreateDefaultSubnet =
  res
    "CreateDefaultSubnetResponse"
    "fixture/CreateDefaultSubnetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDefaultSubnet)

responseDeleteTrafficMirrorTarget :: DeleteTrafficMirrorTargetResponse -> TestTree
responseDeleteTrafficMirrorTarget =
  res
    "DeleteTrafficMirrorTargetResponse"
    "fixture/DeleteTrafficMirrorTargetResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTrafficMirrorTarget)

responseAcceptTransitGatewayMulticastDomainAssociations :: AcceptTransitGatewayMulticastDomainAssociationsResponse -> TestTree
responseAcceptTransitGatewayMulticastDomainAssociations =
  res
    "AcceptTransitGatewayMulticastDomainAssociationsResponse"
    "fixture/AcceptTransitGatewayMulticastDomainAssociationsResponse.proto"
    defaultService
    (Proxy :: Proxy AcceptTransitGatewayMulticastDomainAssociations)

responseDescribeLaunchTemplateVersions :: DescribeLaunchTemplateVersionsResponse -> TestTree
responseDescribeLaunchTemplateVersions =
  res
    "DescribeLaunchTemplateVersionsResponse"
    "fixture/DescribeLaunchTemplateVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLaunchTemplateVersions)

responseDescribeAvailabilityZones :: DescribeAvailabilityZonesResponse -> TestTree
responseDescribeAvailabilityZones =
  res
    "DescribeAvailabilityZonesResponse"
    "fixture/DescribeAvailabilityZonesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAvailabilityZones)

responseGetReservedInstancesExchangeQuote :: GetReservedInstancesExchangeQuoteResponse -> TestTree
responseGetReservedInstancesExchangeQuote =
  res
    "GetReservedInstancesExchangeQuoteResponse"
    "fixture/GetReservedInstancesExchangeQuoteResponse.proto"
    defaultService
    (Proxy :: Proxy GetReservedInstancesExchangeQuote)

responseDeleteVpnGateway :: DeleteVpnGatewayResponse -> TestTree
responseDeleteVpnGateway =
  res
    "DeleteVpnGatewayResponse"
    "fixture/DeleteVpnGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVpnGateway)

responseCreateKeyPair :: CreateKeyPairResponse -> TestTree
responseCreateKeyPair =
  res
    "CreateKeyPairResponse"
    "fixture/CreateKeyPairResponse.proto"
    defaultService
    (Proxy :: Proxy CreateKeyPair)

responseExportTransitGatewayRoutes :: ExportTransitGatewayRoutesResponse -> TestTree
responseExportTransitGatewayRoutes =
  res
    "ExportTransitGatewayRoutesResponse"
    "fixture/ExportTransitGatewayRoutesResponse.proto"
    defaultService
    (Proxy :: Proxy ExportTransitGatewayRoutes)

responseCopySnapshot :: CopySnapshotResponse -> TestTree
responseCopySnapshot =
  res
    "CopySnapshotResponse"
    "fixture/CopySnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy CopySnapshot)

responseDescribeElasticGpus :: DescribeElasticGpusResponse -> TestTree
responseDescribeElasticGpus =
  res
    "DescribeElasticGpusResponse"
    "fixture/DescribeElasticGpusResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeElasticGpus)

responseStartNetworkInsightsAnalysis :: StartNetworkInsightsAnalysisResponse -> TestTree
responseStartNetworkInsightsAnalysis =
  res
    "StartNetworkInsightsAnalysisResponse"
    "fixture/StartNetworkInsightsAnalysisResponse.proto"
    defaultService
    (Proxy :: Proxy StartNetworkInsightsAnalysis)

responseDescribeFpgaImages :: DescribeFpgaImagesResponse -> TestTree
responseDescribeFpgaImages =
  res
    "DescribeFpgaImagesResponse"
    "fixture/DescribeFpgaImagesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeFpgaImages)

responseCreateFlowLogs :: CreateFlowLogsResponse -> TestTree
responseCreateFlowLogs =
  res
    "CreateFlowLogsResponse"
    "fixture/CreateFlowLogsResponse.proto"
    defaultService
    (Proxy :: Proxy CreateFlowLogs)

responseCreateLaunchTemplate :: CreateLaunchTemplateResponse -> TestTree
responseCreateLaunchTemplate =
  res
    "CreateLaunchTemplateResponse"
    "fixture/CreateLaunchTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy CreateLaunchTemplate)

responseDescribeImportImageTasks :: DescribeImportImageTasksResponse -> TestTree
responseDescribeImportImageTasks =
  res
    "DescribeImportImageTasksResponse"
    "fixture/DescribeImportImageTasksResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeImportImageTasks)

responseDeleteTransitGatewayRouteTable :: DeleteTransitGatewayRouteTableResponse -> TestTree
responseDeleteTransitGatewayRouteTable =
  res
    "DeleteTransitGatewayRouteTableResponse"
    "fixture/DeleteTransitGatewayRouteTableResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTransitGatewayRouteTable)

responseDeleteNetworkAcl :: DeleteNetworkAclResponse -> TestTree
responseDeleteNetworkAcl =
  res
    "DeleteNetworkAclResponse"
    "fixture/DeleteNetworkAclResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteNetworkAcl)

responseMoveAddressToVpc :: MoveAddressToVpcResponse -> TestTree
responseMoveAddressToVpc =
  res
    "MoveAddressToVpcResponse"
    "fixture/MoveAddressToVpcResponse.proto"
    defaultService
    (Proxy :: Proxy MoveAddressToVpc)

responseDescribeFleetInstances :: DescribeFleetInstancesResponse -> TestTree
responseDescribeFleetInstances =
  res
    "DescribeFleetInstancesResponse"
    "fixture/DescribeFleetInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeFleetInstances)

responseRestoreAddressToClassic :: RestoreAddressToClassicResponse -> TestTree
responseRestoreAddressToClassic =
  res
    "RestoreAddressToClassicResponse"
    "fixture/RestoreAddressToClassicResponse.proto"
    defaultService
    (Proxy :: Proxy RestoreAddressToClassic)

responseDeleteNetworkInterfacePermission :: DeleteNetworkInterfacePermissionResponse -> TestTree
responseDeleteNetworkInterfacePermission =
  res
    "DeleteNetworkInterfacePermissionResponse"
    "fixture/DeleteNetworkInterfacePermissionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteNetworkInterfacePermission)

responseDescribeRouteTables :: DescribeRouteTablesResponse -> TestTree
responseDescribeRouteTables =
  res
    "DescribeRouteTablesResponse"
    "fixture/DescribeRouteTablesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeRouteTables)

responseUpdateSecurityGroupRuleDescriptionsEgress :: UpdateSecurityGroupRuleDescriptionsEgressResponse -> TestTree
responseUpdateSecurityGroupRuleDescriptionsEgress =
  res
    "UpdateSecurityGroupRuleDescriptionsEgressResponse"
    "fixture/UpdateSecurityGroupRuleDescriptionsEgressResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSecurityGroupRuleDescriptionsEgress)

responseResetFpgaImageAttribute :: ResetFpgaImageAttributeResponse -> TestTree
responseResetFpgaImageAttribute =
  res
    "ResetFpgaImageAttributeResponse"
    "fixture/ResetFpgaImageAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy ResetFpgaImageAttribute)

responseStartVpcEndpointServicePrivateDnsVerification :: StartVpcEndpointServicePrivateDnsVerificationResponse -> TestTree
responseStartVpcEndpointServicePrivateDnsVerification =
  res
    "StartVpcEndpointServicePrivateDnsVerificationResponse"
    "fixture/StartVpcEndpointServicePrivateDnsVerificationResponse.proto"
    defaultService
    (Proxy :: Proxy StartVpcEndpointServicePrivateDnsVerification)

responseDescribeVolumes :: DescribeVolumesResponse -> TestTree
responseDescribeVolumes =
  res
    "DescribeVolumesResponse"
    "fixture/DescribeVolumesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeVolumes)

responseCreateClientVpnEndpoint :: CreateClientVpnEndpointResponse -> TestTree
responseCreateClientVpnEndpoint =
  res
    "CreateClientVpnEndpointResponse"
    "fixture/CreateClientVpnEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy CreateClientVpnEndpoint)

responseRevokeClientVpnIngress :: RevokeClientVpnIngressResponse -> TestTree
responseRevokeClientVpnIngress =
  res
    "RevokeClientVpnIngressResponse"
    "fixture/RevokeClientVpnIngressResponse.proto"
    defaultService
    (Proxy :: Proxy RevokeClientVpnIngress)

responseDeleteFpgaImage :: DeleteFpgaImageResponse -> TestTree
responseDeleteFpgaImage =
  res
    "DeleteFpgaImageResponse"
    "fixture/DeleteFpgaImageResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteFpgaImage)

responseModifyVpcEndpoint :: ModifyVpcEndpointResponse -> TestTree
responseModifyVpcEndpoint =
  res
    "ModifyVpcEndpointResponse"
    "fixture/ModifyVpcEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyVpcEndpoint)

responseDescribeReservedInstancesModifications :: DescribeReservedInstancesModificationsResponse -> TestTree
responseDescribeReservedInstancesModifications =
  res
    "DescribeReservedInstancesModificationsResponse"
    "fixture/DescribeReservedInstancesModificationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeReservedInstancesModifications)

responseDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations :: DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse -> TestTree
responseDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations =
  res
    "DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse"
    "fixture/DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations)

responseEnableFastSnapshotRestores :: EnableFastSnapshotRestoresResponse -> TestTree
responseEnableFastSnapshotRestores =
  res
    "EnableFastSnapshotRestoresResponse"
    "fixture/EnableFastSnapshotRestoresResponse.proto"
    defaultService
    (Proxy :: Proxy EnableFastSnapshotRestores)

responseDescribeClientVpnRoutes :: DescribeClientVpnRoutesResponse -> TestTree
responseDescribeClientVpnRoutes =
  res
    "DescribeClientVpnRoutesResponse"
    "fixture/DescribeClientVpnRoutesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeClientVpnRoutes)

responseGetEbsDefaultKmsKeyId :: GetEbsDefaultKmsKeyIdResponse -> TestTree
responseGetEbsDefaultKmsKeyId =
  res
    "GetEbsDefaultKmsKeyIdResponse"
    "fixture/GetEbsDefaultKmsKeyIdResponse.proto"
    defaultService
    (Proxy :: Proxy GetEbsDefaultKmsKeyId)

responseModifyIdFormat :: ModifyIdFormatResponse -> TestTree
responseModifyIdFormat =
  res
    "ModifyIdFormatResponse"
    "fixture/ModifyIdFormatResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyIdFormat)

responseDetachClassicLinkVpc :: DetachClassicLinkVpcResponse -> TestTree
responseDetachClassicLinkVpc =
  res
    "DetachClassicLinkVpcResponse"
    "fixture/DetachClassicLinkVpcResponse.proto"
    defaultService
    (Proxy :: Proxy DetachClassicLinkVpc)

responseUnassignPrivateIpAddresses :: UnassignPrivateIpAddressesResponse -> TestTree
responseUnassignPrivateIpAddresses =
  res
    "UnassignPrivateIpAddressesResponse"
    "fixture/UnassignPrivateIpAddressesResponse.proto"
    defaultService
    (Proxy :: Proxy UnassignPrivateIpAddresses)

responseAllocateHosts :: AllocateHostsResponse -> TestTree
responseAllocateHosts =
  res
    "AllocateHostsResponse"
    "fixture/AllocateHostsResponse.proto"
    defaultService
    (Proxy :: Proxy AllocateHosts)

responseGetConsoleOutput :: GetConsoleOutputResponse -> TestTree
responseGetConsoleOutput =
  res
    "GetConsoleOutputResponse"
    "fixture/GetConsoleOutputResponse.proto"
    defaultService
    (Proxy :: Proxy GetConsoleOutput)

responseModifyVpnConnectionOptions :: ModifyVpnConnectionOptionsResponse -> TestTree
responseModifyVpnConnectionOptions =
  res
    "ModifyVpnConnectionOptionsResponse"
    "fixture/ModifyVpnConnectionOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyVpnConnectionOptions)

responseCancelImportTask :: CancelImportTaskResponse -> TestTree
responseCancelImportTask =
  res
    "CancelImportTaskResponse"
    "fixture/CancelImportTaskResponse.proto"
    defaultService
    (Proxy :: Proxy CancelImportTask)

responseRegisterImage :: RegisterImageResponse -> TestTree
responseRegisterImage =
  res
    "RegisterImageResponse"
    "fixture/RegisterImageResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterImage)

responseModifyFleet :: ModifyFleetResponse -> TestTree
responseModifyFleet =
  res
    "ModifyFleetResponse"
    "fixture/ModifyFleetResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyFleet)

responseDeleteRouteTable :: DeleteRouteTableResponse -> TestTree
responseDeleteRouteTable =
  res
    "DeleteRouteTableResponse"
    "fixture/DeleteRouteTableResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRouteTable)

responseModifyReservedInstances :: ModifyReservedInstancesResponse -> TestTree
responseModifyReservedInstances =
  res
    "ModifyReservedInstancesResponse"
    "fixture/ModifyReservedInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyReservedInstances)

responseDescribeImageAttribute :: DescribeImageAttributeResponse -> TestTree
responseDescribeImageAttribute =
  res
    "DescribeImageAttributeResponse"
    "fixture/DescribeImageAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeImageAttribute)

responseCreateTrafficMirrorFilterRule :: CreateTrafficMirrorFilterRuleResponse -> TestTree
responseCreateTrafficMirrorFilterRule =
  res
    "CreateTrafficMirrorFilterRuleResponse"
    "fixture/CreateTrafficMirrorFilterRuleResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTrafficMirrorFilterRule)

responseMonitorInstances :: MonitorInstancesResponse -> TestTree
responseMonitorInstances =
  res
    "MonitorInstancesResponse"
    "fixture/MonitorInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy MonitorInstances)

responseModifyVpnConnection :: ModifyVpnConnectionResponse -> TestTree
responseModifyVpnConnection =
  res
    "ModifyVpnConnectionResponse"
    "fixture/ModifyVpnConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyVpnConnection)

responseDescribeSpotInstanceRequests :: DescribeSpotInstanceRequestsResponse -> TestTree
responseDescribeSpotInstanceRequests =
  res
    "DescribeSpotInstanceRequestsResponse"
    "fixture/DescribeSpotInstanceRequestsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSpotInstanceRequests)

responseCancelConversionTask :: CancelConversionTaskResponse -> TestTree
responseCancelConversionTask =
  res
    "CancelConversionTaskResponse"
    "fixture/CancelConversionTaskResponse.proto"
    defaultService
    (Proxy :: Proxy CancelConversionTask)

responseModifyVpcEndpointServiceConfiguration :: ModifyVpcEndpointServiceConfigurationResponse -> TestTree
responseModifyVpcEndpointServiceConfiguration =
  res
    "ModifyVpcEndpointServiceConfigurationResponse"
    "fixture/ModifyVpcEndpointServiceConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyVpcEndpointServiceConfiguration)

responseModifyTransitGatewayVpcAttachment :: ModifyTransitGatewayVpcAttachmentResponse -> TestTree
responseModifyTransitGatewayVpcAttachment =
  res
    "ModifyTransitGatewayVpcAttachmentResponse"
    "fixture/ModifyTransitGatewayVpcAttachmentResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyTransitGatewayVpcAttachment)

responseAssociateRouteTable :: AssociateRouteTableResponse -> TestTree
responseAssociateRouteTable =
  res
    "AssociateRouteTableResponse"
    "fixture/AssociateRouteTableResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateRouteTable)

responseDescribeAccountAttributes :: DescribeAccountAttributesResponse -> TestTree
responseDescribeAccountAttributes =
  res
    "DescribeAccountAttributesResponse"
    "fixture/DescribeAccountAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAccountAttributes)

responseDescribeLaunchTemplates :: DescribeLaunchTemplatesResponse -> TestTree
responseDescribeLaunchTemplates =
  res
    "DescribeLaunchTemplatesResponse"
    "fixture/DescribeLaunchTemplatesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLaunchTemplates)

responseDescribeIpv6Pools :: DescribeIpv6PoolsResponse -> TestTree
responseDescribeIpv6Pools =
  res
    "DescribeIpv6PoolsResponse"
    "fixture/DescribeIpv6PoolsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeIpv6Pools)

responseDescribeLocalGateways :: DescribeLocalGatewaysResponse -> TestTree
responseDescribeLocalGateways =
  res
    "DescribeLocalGatewaysResponse"
    "fixture/DescribeLocalGatewaysResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLocalGateways)

responsePurchaseHostReservation :: PurchaseHostReservationResponse -> TestTree
responsePurchaseHostReservation =
  res
    "PurchaseHostReservationResponse"
    "fixture/PurchaseHostReservationResponse.proto"
    defaultService
    (Proxy :: Proxy PurchaseHostReservation)

responseReportInstanceStatus :: ReportInstanceStatusResponse -> TestTree
responseReportInstanceStatus =
  res
    "ReportInstanceStatusResponse"
    "fixture/ReportInstanceStatusResponse.proto"
    defaultService
    (Proxy :: Proxy ReportInstanceStatus)

responseModifyVpcEndpointServicePermissions :: ModifyVpcEndpointServicePermissionsResponse -> TestTree
responseModifyVpcEndpointServicePermissions =
  res
    "ModifyVpcEndpointServicePermissionsResponse"
    "fixture/ModifyVpcEndpointServicePermissionsResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyVpcEndpointServicePermissions)

responseModifyHosts :: ModifyHostsResponse -> TestTree
responseModifyHosts =
  res
    "ModifyHostsResponse"
    "fixture/ModifyHostsResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyHosts)

responseUnassignIpv6Addresses :: UnassignIpv6AddressesResponse -> TestTree
responseUnassignIpv6Addresses =
  res
    "UnassignIpv6AddressesResponse"
    "fixture/UnassignIpv6AddressesResponse.proto"
    defaultService
    (Proxy :: Proxy UnassignIpv6Addresses)

responseGetManagedPrefixListAssociations :: GetManagedPrefixListAssociationsResponse -> TestTree
responseGetManagedPrefixListAssociations =
  res
    "GetManagedPrefixListAssociationsResponse"
    "fixture/GetManagedPrefixListAssociationsResponse.proto"
    defaultService
    (Proxy :: Proxy GetManagedPrefixListAssociations)

responseDisableFastSnapshotRestores :: DisableFastSnapshotRestoresResponse -> TestTree
responseDisableFastSnapshotRestores =
  res
    "DisableFastSnapshotRestoresResponse"
    "fixture/DisableFastSnapshotRestoresResponse.proto"
    defaultService
    (Proxy :: Proxy DisableFastSnapshotRestores)

responseDeleteEgressOnlyInternetGateway :: DeleteEgressOnlyInternetGatewayResponse -> TestTree
responseDeleteEgressOnlyInternetGateway =
  res
    "DeleteEgressOnlyInternetGatewayResponse"
    "fixture/DeleteEgressOnlyInternetGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteEgressOnlyInternetGateway)

responseRequestSpotInstances :: RequestSpotInstancesResponse -> TestTree
responseRequestSpotInstances =
  res
    "RequestSpotInstancesResponse"
    "fixture/RequestSpotInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy RequestSpotInstances)

responseRunInstances :: Reservation -> TestTree
responseRunInstances =
  res
    "RunInstancesResponse"
    "fixture/RunInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy RunInstances)

responseGetTransitGatewayRouteTablePropagations :: GetTransitGatewayRouteTablePropagationsResponse -> TestTree
responseGetTransitGatewayRouteTablePropagations =
  res
    "GetTransitGatewayRouteTablePropagationsResponse"
    "fixture/GetTransitGatewayRouteTablePropagationsResponse.proto"
    defaultService
    (Proxy :: Proxy GetTransitGatewayRouteTablePropagations)

responseAttachVolume :: VolumeAttachment -> TestTree
responseAttachVolume =
  res
    "AttachVolumeResponse"
    "fixture/AttachVolumeResponse.proto"
    defaultService
    (Proxy :: Proxy AttachVolume)

responseAcceptVpcEndpointConnections :: AcceptVpcEndpointConnectionsResponse -> TestTree
responseAcceptVpcEndpointConnections =
  res
    "AcceptVpcEndpointConnectionsResponse"
    "fixture/AcceptVpcEndpointConnectionsResponse.proto"
    defaultService
    (Proxy :: Proxy AcceptVpcEndpointConnections)

responseCreateDhcpOptions :: CreateDhcpOptionsResponse -> TestTree
responseCreateDhcpOptions =
  res
    "CreateDhcpOptionsResponse"
    "fixture/CreateDhcpOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDhcpOptions)

responseRebootInstances :: RebootInstancesResponse -> TestTree
responseRebootInstances =
  res
    "RebootInstancesResponse"
    "fixture/RebootInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy RebootInstances)

responseModifyImageAttribute :: ModifyImageAttributeResponse -> TestTree
responseModifyImageAttribute =
  res
    "ModifyImageAttributeResponse"
    "fixture/ModifyImageAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyImageAttribute)

responseCreateManagedPrefixList :: CreateManagedPrefixListResponse -> TestTree
responseCreateManagedPrefixList =
  res
    "CreateManagedPrefixListResponse"
    "fixture/CreateManagedPrefixListResponse.proto"
    defaultService
    (Proxy :: Proxy CreateManagedPrefixList)

responseSearchTransitGatewayRoutes :: SearchTransitGatewayRoutesResponse -> TestTree
responseSearchTransitGatewayRoutes =
  res
    "SearchTransitGatewayRoutesResponse"
    "fixture/SearchTransitGatewayRoutesResponse.proto"
    defaultService
    (Proxy :: Proxy SearchTransitGatewayRoutes)

responseDescribeIdFormat :: DescribeIdFormatResponse -> TestTree
responseDescribeIdFormat =
  res
    "DescribeIdFormatResponse"
    "fixture/DescribeIdFormatResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeIdFormat)

responseRegisterTransitGatewayMulticastGroupSources :: RegisterTransitGatewayMulticastGroupSourcesResponse -> TestTree
responseRegisterTransitGatewayMulticastGroupSources =
  res
    "RegisterTransitGatewayMulticastGroupSourcesResponse"
    "fixture/RegisterTransitGatewayMulticastGroupSourcesResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterTransitGatewayMulticastGroupSources)

responseDescribeVpcEndpointConnectionNotifications :: DescribeVpcEndpointConnectionNotificationsResponse -> TestTree
responseDescribeVpcEndpointConnectionNotifications =
  res
    "DescribeVpcEndpointConnectionNotificationsResponse"
    "fixture/DescribeVpcEndpointConnectionNotificationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeVpcEndpointConnectionNotifications)

responseDescribeVpcs :: DescribeVpcsResponse -> TestTree
responseDescribeVpcs =
  res
    "DescribeVpcsResponse"
    "fixture/DescribeVpcsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeVpcs)

responseGetTransitGatewayPrefixListReferences :: GetTransitGatewayPrefixListReferencesResponse -> TestTree
responseGetTransitGatewayPrefixListReferences =
  res
    "GetTransitGatewayPrefixListReferencesResponse"
    "fixture/GetTransitGatewayPrefixListReferencesResponse.proto"
    defaultService
    (Proxy :: Proxy GetTransitGatewayPrefixListReferences)

responseCreateRouteTable :: CreateRouteTableResponse -> TestTree
responseCreateRouteTable =
  res
    "CreateRouteTableResponse"
    "fixture/CreateRouteTableResponse.proto"
    defaultService
    (Proxy :: Proxy CreateRouteTable)

responseDescribeVolumesModifications :: DescribeVolumesModificationsResponse -> TestTree
responseDescribeVolumesModifications =
  res
    "DescribeVolumesModificationsResponse"
    "fixture/DescribeVolumesModificationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeVolumesModifications)

responseAssociateIamInstanceProfile :: AssociateIamInstanceProfileResponse -> TestTree
responseAssociateIamInstanceProfile =
  res
    "AssociateIamInstanceProfileResponse"
    "fixture/AssociateIamInstanceProfileResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateIamInstanceProfile)

responseCreateImage :: CreateImageResponse -> TestTree
responseCreateImage =
  res
    "CreateImageResponse"
    "fixture/CreateImageResponse.proto"
    defaultService
    (Proxy :: Proxy CreateImage)

responseDescribeTrafficMirrorTargets :: DescribeTrafficMirrorTargetsResponse -> TestTree
responseDescribeTrafficMirrorTargets =
  res
    "DescribeTrafficMirrorTargetsResponse"
    "fixture/DescribeTrafficMirrorTargetsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTrafficMirrorTargets)

responseAssociateDhcpOptions :: AssociateDhcpOptionsResponse -> TestTree
responseAssociateDhcpOptions =
  res
    "AssociateDhcpOptionsResponse"
    "fixture/AssociateDhcpOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateDhcpOptions)

responseDescribeSpotFleetRequestHistory :: DescribeSpotFleetRequestHistoryResponse -> TestTree
responseDescribeSpotFleetRequestHistory =
  res
    "DescribeSpotFleetRequestHistoryResponse"
    "fixture/DescribeSpotFleetRequestHistoryResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSpotFleetRequestHistory)

responseModifyInstanceEventStartTime :: ModifyInstanceEventStartTimeResponse -> TestTree
responseModifyInstanceEventStartTime =
  res
    "ModifyInstanceEventStartTimeResponse"
    "fixture/ModifyInstanceEventStartTimeResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyInstanceEventStartTime)

responseDisassociateEnclaveCertificateIamRole :: DisassociateEnclaveCertificateIamRoleResponse -> TestTree
responseDisassociateEnclaveCertificateIamRole =
  res
    "DisassociateEnclaveCertificateIamRoleResponse"
    "fixture/DisassociateEnclaveCertificateIamRoleResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateEnclaveCertificateIamRole)

responseDeleteVpcPeeringConnection :: DeleteVpcPeeringConnectionResponse -> TestTree
responseDeleteVpcPeeringConnection =
  res
    "DeleteVpcPeeringConnectionResponse"
    "fixture/DeleteVpcPeeringConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVpcPeeringConnection)

responseResetInstanceAttribute :: ResetInstanceAttributeResponse -> TestTree
responseResetInstanceAttribute =
  res
    "ResetInstanceAttributeResponse"
    "fixture/ResetInstanceAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy ResetInstanceAttribute)

responseDescribeInstanceStatus :: DescribeInstanceStatusResponse -> TestTree
responseDescribeInstanceStatus =
  res
    "DescribeInstanceStatusResponse"
    "fixture/DescribeInstanceStatusResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInstanceStatus)

responseAttachNetworkInterface :: AttachNetworkInterfaceResponse -> TestTree
responseAttachNetworkInterface =
  res
    "AttachNetworkInterfaceResponse"
    "fixture/AttachNetworkInterfaceResponse.proto"
    defaultService
    (Proxy :: Proxy AttachNetworkInterface)

responseAssignIpv6Addresses :: AssignIpv6AddressesResponse -> TestTree
responseAssignIpv6Addresses =
  res
    "AssignIpv6AddressesResponse"
    "fixture/AssignIpv6AddressesResponse.proto"
    defaultService
    (Proxy :: Proxy AssignIpv6Addresses)

responseCreateLocalGatewayRoute :: CreateLocalGatewayRouteResponse -> TestTree
responseCreateLocalGatewayRoute =
  res
    "CreateLocalGatewayRouteResponse"
    "fixture/CreateLocalGatewayRouteResponse.proto"
    defaultService
    (Proxy :: Proxy CreateLocalGatewayRoute)

responseEnableVgwRoutePropagation :: EnableVgwRoutePropagationResponse -> TestTree
responseEnableVgwRoutePropagation =
  res
    "EnableVgwRoutePropagationResponse"
    "fixture/EnableVgwRoutePropagationResponse.proto"
    defaultService
    (Proxy :: Proxy EnableVgwRoutePropagation)

responseDescribeVpcEndpoints :: DescribeVpcEndpointsResponse -> TestTree
responseDescribeVpcEndpoints =
  res
    "DescribeVpcEndpointsResponse"
    "fixture/DescribeVpcEndpointsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeVpcEndpoints)

responseCreateNetworkAclEntry :: CreateNetworkAclEntryResponse -> TestTree
responseCreateNetworkAclEntry =
  res
    "CreateNetworkAclEntryResponse"
    "fixture/CreateNetworkAclEntryResponse.proto"
    defaultService
    (Proxy :: Proxy CreateNetworkAclEntry)

responseDescribeStaleSecurityGroups :: DescribeStaleSecurityGroupsResponse -> TestTree
responseDescribeStaleSecurityGroups =
  res
    "DescribeStaleSecurityGroupsResponse"
    "fixture/DescribeStaleSecurityGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeStaleSecurityGroups)

responseDescribeFlowLogs :: DescribeFlowLogsResponse -> TestTree
responseDescribeFlowLogs =
  res
    "DescribeFlowLogsResponse"
    "fixture/DescribeFlowLogsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeFlowLogs)

responseDescribePlacementGroups :: DescribePlacementGroupsResponse -> TestTree
responseDescribePlacementGroups =
  res
    "DescribePlacementGroupsResponse"
    "fixture/DescribePlacementGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePlacementGroups)

responseDescribeFleets :: DescribeFleetsResponse -> TestTree
responseDescribeFleets =
  res
    "DescribeFleetsResponse"
    "fixture/DescribeFleetsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeFleets)

responseModifyIdentityIdFormat :: ModifyIdentityIdFormatResponse -> TestTree
responseModifyIdentityIdFormat =
  res
    "ModifyIdentityIdFormatResponse"
    "fixture/ModifyIdentityIdFormatResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyIdentityIdFormat)

responseDescribeLocalGatewayVirtualInterfaceGroups :: DescribeLocalGatewayVirtualInterfaceGroupsResponse -> TestTree
responseDescribeLocalGatewayVirtualInterfaceGroups =
  res
    "DescribeLocalGatewayVirtualInterfaceGroupsResponse"
    "fixture/DescribeLocalGatewayVirtualInterfaceGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLocalGatewayVirtualInterfaceGroups)

responseReplaceNetworkAclEntry :: ReplaceNetworkAclEntryResponse -> TestTree
responseReplaceNetworkAclEntry =
  res
    "ReplaceNetworkAclEntryResponse"
    "fixture/ReplaceNetworkAclEntryResponse.proto"
    defaultService
    (Proxy :: Proxy ReplaceNetworkAclEntry)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags =
  res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTags)

responseDescribeTransitGatewayAttachments :: DescribeTransitGatewayAttachmentsResponse -> TestTree
responseDescribeTransitGatewayAttachments =
  res
    "DescribeTransitGatewayAttachmentsResponse"
    "fixture/DescribeTransitGatewayAttachmentsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTransitGatewayAttachments)

responseDescribeReservedInstancesOfferings :: DescribeReservedInstancesOfferingsResponse -> TestTree
responseDescribeReservedInstancesOfferings =
  res
    "DescribeReservedInstancesOfferingsResponse"
    "fixture/DescribeReservedInstancesOfferingsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeReservedInstancesOfferings)

responseModifySnapshotAttribute :: ModifySnapshotAttributeResponse -> TestTree
responseModifySnapshotAttribute =
  res
    "ModifySnapshotAttributeResponse"
    "fixture/ModifySnapshotAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy ModifySnapshotAttribute)

responseConfirmProductInstance :: ConfirmProductInstanceResponse -> TestTree
responseConfirmProductInstance =
  res
    "ConfirmProductInstanceResponse"
    "fixture/ConfirmProductInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy ConfirmProductInstance)

responseDescribeVpnConnections :: DescribeVpnConnectionsResponse -> TestTree
responseDescribeVpnConnections =
  res
    "DescribeVpnConnectionsResponse"
    "fixture/DescribeVpnConnectionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeVpnConnections)

responseModifyAvailabilityZoneGroup :: ModifyAvailabilityZoneGroupResponse -> TestTree
responseModifyAvailabilityZoneGroup =
  res
    "ModifyAvailabilityZoneGroupResponse"
    "fixture/ModifyAvailabilityZoneGroupResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyAvailabilityZoneGroup)

responseDisassociateIamInstanceProfile :: DisassociateIamInstanceProfileResponse -> TestTree
responseDisassociateIamInstanceProfile =
  res
    "DisassociateIamInstanceProfileResponse"
    "fixture/DisassociateIamInstanceProfileResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateIamInstanceProfile)

responseDisableVpcClassicLink :: DisableVpcClassicLinkResponse -> TestTree
responseDisableVpcClassicLink =
  res
    "DisableVpcClassicLinkResponse"
    "fixture/DisableVpcClassicLinkResponse.proto"
    defaultService
    (Proxy :: Proxy DisableVpcClassicLink)

responseGetGroupsForCapacityReservation :: GetGroupsForCapacityReservationResponse -> TestTree
responseGetGroupsForCapacityReservation =
  res
    "GetGroupsForCapacityReservationResponse"
    "fixture/GetGroupsForCapacityReservationResponse.proto"
    defaultService
    (Proxy :: Proxy GetGroupsForCapacityReservation)

responseImportVolume :: ImportVolumeResponse -> TestTree
responseImportVolume =
  res
    "ImportVolumeResponse"
    "fixture/ImportVolumeResponse.proto"
    defaultService
    (Proxy :: Proxy ImportVolume)

responseDescribeAddresses :: DescribeAddressesResponse -> TestTree
responseDescribeAddresses =
  res
    "DescribeAddressesResponse"
    "fixture/DescribeAddressesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAddresses)

responseDeleteLocalGatewayRoute :: DeleteLocalGatewayRouteResponse -> TestTree
responseDeleteLocalGatewayRoute =
  res
    "DeleteLocalGatewayRouteResponse"
    "fixture/DeleteLocalGatewayRouteResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteLocalGatewayRoute)

responseDescribeVpcEndpointServiceConfigurations :: DescribeVpcEndpointServiceConfigurationsResponse -> TestTree
responseDescribeVpcEndpointServiceConfigurations =
  res
    "DescribeVpcEndpointServiceConfigurationsResponse"
    "fixture/DescribeVpcEndpointServiceConfigurationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeVpcEndpointServiceConfigurations)

responseDescribeNetworkInterfaces :: DescribeNetworkInterfacesResponse -> TestTree
responseDescribeNetworkInterfaces =
  res
    "DescribeNetworkInterfacesResponse"
    "fixture/DescribeNetworkInterfacesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeNetworkInterfaces)

responseDescribeVpcEndpointServices :: DescribeVpcEndpointServicesResponse -> TestTree
responseDescribeVpcEndpointServices =
  res
    "DescribeVpcEndpointServicesResponse"
    "fixture/DescribeVpcEndpointServicesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeVpcEndpointServices)

responseDeleteNetworkAclEntry :: DeleteNetworkAclEntryResponse -> TestTree
responseDeleteNetworkAclEntry =
  res
    "DeleteNetworkAclEntryResponse"
    "fixture/DeleteNetworkAclEntryResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteNetworkAclEntry)

responseGetTransitGatewayAttachmentPropagations :: GetTransitGatewayAttachmentPropagationsResponse -> TestTree
responseGetTransitGatewayAttachmentPropagations =
  res
    "GetTransitGatewayAttachmentPropagationsResponse"
    "fixture/GetTransitGatewayAttachmentPropagationsResponse.proto"
    defaultService
    (Proxy :: Proxy GetTransitGatewayAttachmentPropagations)

responseAssignPrivateIpAddresses :: AssignPrivateIpAddressesResponse -> TestTree
responseAssignPrivateIpAddresses =
  res
    "AssignPrivateIpAddressesResponse"
    "fixture/AssignPrivateIpAddressesResponse.proto"
    defaultService
    (Proxy :: Proxy AssignPrivateIpAddresses)

responseDescribeNatGateways :: DescribeNatGatewaysResponse -> TestTree
responseDescribeNatGateways =
  res
    "DescribeNatGatewaysResponse"
    "fixture/DescribeNatGatewaysResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeNatGateways)

responseDescribeSnapshotAttribute :: DescribeSnapshotAttributeResponse -> TestTree
responseDescribeSnapshotAttribute =
  res
    "DescribeSnapshotAttributeResponse"
    "fixture/DescribeSnapshotAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSnapshotAttribute)

responseDeleteSnapshot :: DeleteSnapshotResponse -> TestTree
responseDeleteSnapshot =
  res
    "DeleteSnapshotResponse"
    "fixture/DeleteSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSnapshot)

responseDeleteCarrierGateway :: DeleteCarrierGatewayResponse -> TestTree
responseDeleteCarrierGateway =
  res
    "DeleteCarrierGatewayResponse"
    "fixture/DeleteCarrierGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCarrierGateway)

responseDescribeTransitGatewayVpcAttachments :: DescribeTransitGatewayVpcAttachmentsResponse -> TestTree
responseDescribeTransitGatewayVpcAttachments =
  res
    "DescribeTransitGatewayVpcAttachmentsResponse"
    "fixture/DescribeTransitGatewayVpcAttachmentsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTransitGatewayVpcAttachments)

responseModifyVpcEndpointConnectionNotification :: ModifyVpcEndpointConnectionNotificationResponse -> TestTree
responseModifyVpcEndpointConnectionNotification =
  res
    "ModifyVpcEndpointConnectionNotificationResponse"
    "fixture/ModifyVpcEndpointConnectionNotificationResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyVpcEndpointConnectionNotification)

responsePurchaseReservedInstancesOffering :: PurchaseReservedInstancesOfferingResponse -> TestTree
responsePurchaseReservedInstancesOffering =
  res
    "PurchaseReservedInstancesOfferingResponse"
    "fixture/PurchaseReservedInstancesOfferingResponse.proto"
    defaultService
    (Proxy :: Proxy PurchaseReservedInstancesOffering)

responseAuthorizeSecurityGroupIngress :: AuthorizeSecurityGroupIngressResponse -> TestTree
responseAuthorizeSecurityGroupIngress =
  res
    "AuthorizeSecurityGroupIngressResponse"
    "fixture/AuthorizeSecurityGroupIngressResponse.proto"
    defaultService
    (Proxy :: Proxy AuthorizeSecurityGroupIngress)

responseGetConsoleScreenshot :: GetConsoleScreenshotResponse -> TestTree
responseGetConsoleScreenshot =
  res
    "GetConsoleScreenshotResponse"
    "fixture/GetConsoleScreenshotResponse.proto"
    defaultService
    (Proxy :: Proxy GetConsoleScreenshot)

responseDisableVgwRoutePropagation :: DisableVgwRoutePropagationResponse -> TestTree
responseDisableVgwRoutePropagation =
  res
    "DisableVgwRoutePropagationResponse"
    "fixture/DisableVgwRoutePropagationResponse.proto"
    defaultService
    (Proxy :: Proxy DisableVgwRoutePropagation)

responseDescribeTransitGatewayMulticastDomains :: DescribeTransitGatewayMulticastDomainsResponse -> TestTree
responseDescribeTransitGatewayMulticastDomains =
  res
    "DescribeTransitGatewayMulticastDomainsResponse"
    "fixture/DescribeTransitGatewayMulticastDomainsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTransitGatewayMulticastDomains)

responseDescribeSubnets :: DescribeSubnetsResponse -> TestTree
responseDescribeSubnets =
  res
    "DescribeSubnetsResponse"
    "fixture/DescribeSubnetsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSubnets)

responseUnmonitorInstances :: UnmonitorInstancesResponse -> TestTree
responseUnmonitorInstances =
  res
    "UnmonitorInstancesResponse"
    "fixture/UnmonitorInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy UnmonitorInstances)

responseCancelSpotInstanceRequests :: CancelSpotInstanceRequestsResponse -> TestTree
responseCancelSpotInstanceRequests =
  res
    "CancelSpotInstanceRequestsResponse"
    "fixture/CancelSpotInstanceRequestsResponse.proto"
    defaultService
    (Proxy :: Proxy CancelSpotInstanceRequests)

responseCreateSpotDatafeedSubscription :: CreateSpotDatafeedSubscriptionResponse -> TestTree
responseCreateSpotDatafeedSubscription =
  res
    "CreateSpotDatafeedSubscriptionResponse"
    "fixture/CreateSpotDatafeedSubscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSpotDatafeedSubscription)

responseDisassociateRouteTable :: DisassociateRouteTableResponse -> TestTree
responseDisassociateRouteTable =
  res
    "DisassociateRouteTableResponse"
    "fixture/DisassociateRouteTableResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateRouteTable)

responseDescribeTransitGatewayConnectPeers :: DescribeTransitGatewayConnectPeersResponse -> TestTree
responseDescribeTransitGatewayConnectPeers =
  res
    "DescribeTransitGatewayConnectPeersResponse"
    "fixture/DescribeTransitGatewayConnectPeersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTransitGatewayConnectPeers)

responseModifyVpnTunnelCertificate :: ModifyVpnTunnelCertificateResponse -> TestTree
responseModifyVpnTunnelCertificate =
  res
    "ModifyVpnTunnelCertificateResponse"
    "fixture/ModifyVpnTunnelCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyVpnTunnelCertificate)

responseRestoreManagedPrefixListVersion :: RestoreManagedPrefixListVersionResponse -> TestTree
responseRestoreManagedPrefixListVersion =
  res
    "RestoreManagedPrefixListVersionResponse"
    "fixture/RestoreManagedPrefixListVersionResponse.proto"
    defaultService
    (Proxy :: Proxy RestoreManagedPrefixListVersion)

responseModifyAddressAttribute :: ModifyAddressAttributeResponse -> TestTree
responseModifyAddressAttribute =
  res
    "ModifyAddressAttributeResponse"
    "fixture/ModifyAddressAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyAddressAttribute)

responseCreateVpnConnection :: CreateVpnConnectionResponse -> TestTree
responseCreateVpnConnection =
  res
    "CreateVpnConnectionResponse"
    "fixture/CreateVpnConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateVpnConnection)

responseAssociateSubnetCidrBlock :: AssociateSubnetCidrBlockResponse -> TestTree
responseAssociateSubnetCidrBlock =
  res
    "AssociateSubnetCidrBlockResponse"
    "fixture/AssociateSubnetCidrBlockResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateSubnetCidrBlock)

responseAttachClassicLinkVpc :: AttachClassicLinkVpcResponse -> TestTree
responseAttachClassicLinkVpc =
  res
    "AttachClassicLinkVpcResponse"
    "fixture/AttachClassicLinkVpcResponse.proto"
    defaultService
    (Proxy :: Proxy AttachClassicLinkVpc)

responseDescribeSpotPriceHistory :: DescribeSpotPriceHistoryResponse -> TestTree
responseDescribeSpotPriceHistory =
  res
    "DescribeSpotPriceHistoryResponse"
    "fixture/DescribeSpotPriceHistoryResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSpotPriceHistory)

responseDeleteQueuedReservedInstances :: DeleteQueuedReservedInstancesResponse -> TestTree
responseDeleteQueuedReservedInstances =
  res
    "DeleteQueuedReservedInstancesResponse"
    "fixture/DeleteQueuedReservedInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteQueuedReservedInstances)

responseDescribeAggregateIdFormat :: DescribeAggregateIdFormatResponse -> TestTree
responseDescribeAggregateIdFormat =
  res
    "DescribeAggregateIdFormatResponse"
    "fixture/DescribeAggregateIdFormatResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAggregateIdFormat)

responseDescribeReservedInstancesListings :: DescribeReservedInstancesListingsResponse -> TestTree
responseDescribeReservedInstancesListings =
  res
    "DescribeReservedInstancesListingsResponse"
    "fixture/DescribeReservedInstancesListingsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeReservedInstancesListings)

responseCopyImage :: CopyImageResponse -> TestTree
responseCopyImage =
  res
    "CopyImageResponse"
    "fixture/CopyImageResponse.proto"
    defaultService
    (Proxy :: Proxy CopyImage)

responseCreateLocalGatewayRouteTableVpcAssociation :: CreateLocalGatewayRouteTableVpcAssociationResponse -> TestTree
responseCreateLocalGatewayRouteTableVpcAssociation =
  res
    "CreateLocalGatewayRouteTableVpcAssociationResponse"
    "fixture/CreateLocalGatewayRouteTableVpcAssociationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateLocalGatewayRouteTableVpcAssociation)

responseDescribeCarrierGateways :: DescribeCarrierGatewaysResponse -> TestTree
responseDescribeCarrierGateways =
  res
    "DescribeCarrierGatewaysResponse"
    "fixture/DescribeCarrierGatewaysResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCarrierGateways)

responseDeleteInternetGateway :: DeleteInternetGatewayResponse -> TestTree
responseDeleteInternetGateway =
  res
    "DeleteInternetGatewayResponse"
    "fixture/DeleteInternetGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteInternetGateway)

responseCreateFleet :: CreateFleetResponse -> TestTree
responseCreateFleet =
  res
    "CreateFleetResponse"
    "fixture/CreateFleetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateFleet)

responseModifyClientVpnEndpoint :: ModifyClientVpnEndpointResponse -> TestTree
responseModifyClientVpnEndpoint =
  res
    "ModifyClientVpnEndpointResponse"
    "fixture/ModifyClientVpnEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyClientVpnEndpoint)

responseModifyInstanceCapacityReservationAttributes :: ModifyInstanceCapacityReservationAttributesResponse -> TestTree
responseModifyInstanceCapacityReservationAttributes =
  res
    "ModifyInstanceCapacityReservationAttributesResponse"
    "fixture/ModifyInstanceCapacityReservationAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyInstanceCapacityReservationAttributes)

responseImportClientVpnClientCertificateRevocationList :: ImportClientVpnClientCertificateRevocationListResponse -> TestTree
responseImportClientVpnClientCertificateRevocationList =
  res
    "ImportClientVpnClientCertificateRevocationListResponse"
    "fixture/ImportClientVpnClientCertificateRevocationListResponse.proto"
    defaultService
    (Proxy :: Proxy ImportClientVpnClientCertificateRevocationList)

responseAssociateClientVpnTargetNetwork :: AssociateClientVpnTargetNetworkResponse -> TestTree
responseAssociateClientVpnTargetNetwork =
  res
    "AssociateClientVpnTargetNetworkResponse"
    "fixture/AssociateClientVpnTargetNetworkResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateClientVpnTargetNetwork)

responseCancelCapacityReservation :: CancelCapacityReservationResponse -> TestTree
responseCancelCapacityReservation =
  res
    "CancelCapacityReservationResponse"
    "fixture/CancelCapacityReservationResponse.proto"
    defaultService
    (Proxy :: Proxy CancelCapacityReservation)

responseCancelReservedInstancesListing :: CancelReservedInstancesListingResponse -> TestTree
responseCancelReservedInstancesListing =
  res
    "CancelReservedInstancesListingResponse"
    "fixture/CancelReservedInstancesListingResponse.proto"
    defaultService
    (Proxy :: Proxy CancelReservedInstancesListing)

responseDisableTransitGatewayRouteTablePropagation :: DisableTransitGatewayRouteTablePropagationResponse -> TestTree
responseDisableTransitGatewayRouteTablePropagation =
  res
    "DisableTransitGatewayRouteTablePropagationResponse"
    "fixture/DisableTransitGatewayRouteTablePropagationResponse.proto"
    defaultService
    (Proxy :: Proxy DisableTransitGatewayRouteTablePropagation)

responseDescribeVpcClassicLinkDnsSupport :: DescribeVpcClassicLinkDnsSupportResponse -> TestTree
responseDescribeVpcClassicLinkDnsSupport =
  res
    "DescribeVpcClassicLinkDnsSupportResponse"
    "fixture/DescribeVpcClassicLinkDnsSupportResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeVpcClassicLinkDnsSupport)

responseCreateVpcEndpoint :: CreateVpcEndpointResponse -> TestTree
responseCreateVpcEndpoint =
  res
    "CreateVpcEndpointResponse"
    "fixture/CreateVpcEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy CreateVpcEndpoint)

responseDescribeSnapshots :: DescribeSnapshotsResponse -> TestTree
responseDescribeSnapshots =
  res
    "DescribeSnapshotsResponse"
    "fixture/DescribeSnapshotsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSnapshots)

responseDescribeImportSnapshotTasks :: DescribeImportSnapshotTasksResponse -> TestTree
responseDescribeImportSnapshotTasks =
  res
    "DescribeImportSnapshotTasksResponse"
    "fixture/DescribeImportSnapshotTasksResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeImportSnapshotTasks)

responseDescribeNetworkInterfaceAttribute :: DescribeNetworkInterfaceAttributeResponse -> TestTree
responseDescribeNetworkInterfaceAttribute =
  res
    "DescribeNetworkInterfaceAttributeResponse"
    "fixture/DescribeNetworkInterfaceAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeNetworkInterfaceAttribute)

responseDescribeInstanceEventNotificationAttributes :: DescribeInstanceEventNotificationAttributesResponse -> TestTree
responseDescribeInstanceEventNotificationAttributes =
  res
    "DescribeInstanceEventNotificationAttributesResponse"
    "fixture/DescribeInstanceEventNotificationAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInstanceEventNotificationAttributes)

responseEnableEbsEncryptionByDefault :: EnableEbsEncryptionByDefaultResponse -> TestTree
responseEnableEbsEncryptionByDefault =
  res
    "EnableEbsEncryptionByDefaultResponse"
    "fixture/EnableEbsEncryptionByDefaultResponse.proto"
    defaultService
    (Proxy :: Proxy EnableEbsEncryptionByDefault)

responseModifyTrafficMirrorFilterRule :: ModifyTrafficMirrorFilterRuleResponse -> TestTree
responseModifyTrafficMirrorFilterRule =
  res
    "ModifyTrafficMirrorFilterRuleResponse"
    "fixture/ModifyTrafficMirrorFilterRuleResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyTrafficMirrorFilterRule)

responseDescribeCoipPools :: DescribeCoipPoolsResponse -> TestTree
responseDescribeCoipPools =
  res
    "DescribeCoipPoolsResponse"
    "fixture/DescribeCoipPoolsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCoipPools)

responseCancelExportTask :: CancelExportTaskResponse -> TestTree
responseCancelExportTask =
  res
    "CancelExportTaskResponse"
    "fixture/CancelExportTaskResponse.proto"
    defaultService
    (Proxy :: Proxy CancelExportTask)

responseEnableVolumeIO :: EnableVolumeIOResponse -> TestTree
responseEnableVolumeIO =
  res
    "EnableVolumeIOResponse"
    "fixture/EnableVolumeIOResponse.proto"
    defaultService
    (Proxy :: Proxy EnableVolumeIO)

responseModifyTransitGateway :: ModifyTransitGatewayResponse -> TestTree
responseModifyTransitGateway =
  res
    "ModifyTransitGatewayResponse"
    "fixture/ModifyTransitGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyTransitGateway)

responseDescribeInstanceTypeOfferings :: DescribeInstanceTypeOfferingsResponse -> TestTree
responseDescribeInstanceTypeOfferings =
  res
    "DescribeInstanceTypeOfferingsResponse"
    "fixture/DescribeInstanceTypeOfferingsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInstanceTypeOfferings)

responseCreateSubnet :: CreateSubnetResponse -> TestTree
responseCreateSubnet =
  res
    "CreateSubnetResponse"
    "fixture/CreateSubnetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSubnet)

responseRequestSpotFleet :: RequestSpotFleetResponse -> TestTree
responseRequestSpotFleet =
  res
    "RequestSpotFleetResponse"
    "fixture/RequestSpotFleetResponse.proto"
    defaultService
    (Proxy :: Proxy RequestSpotFleet)

responseDeleteVpnConnection :: DeleteVpnConnectionResponse -> TestTree
responseDeleteVpnConnection =
  res
    "DeleteVpnConnectionResponse"
    "fixture/DeleteVpnConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVpnConnection)

responseModifySpotFleetRequest :: ModifySpotFleetRequestResponse -> TestTree
responseModifySpotFleetRequest =
  res
    "ModifySpotFleetRequestResponse"
    "fixture/ModifySpotFleetRequestResponse.proto"
    defaultService
    (Proxy :: Proxy ModifySpotFleetRequest)

responseDeregisterImage :: DeregisterImageResponse -> TestTree
responseDeregisterImage =
  res
    "DeregisterImageResponse"
    "fixture/DeregisterImageResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterImage)

responseDetachVpnGateway :: DetachVpnGatewayResponse -> TestTree
responseDetachVpnGateway =
  res
    "DetachVpnGatewayResponse"
    "fixture/DetachVpnGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy DetachVpnGateway)

responseCreateNetworkInterface :: CreateNetworkInterfaceResponse -> TestTree
responseCreateNetworkInterface =
  res
    "CreateNetworkInterfaceResponse"
    "fixture/CreateNetworkInterfaceResponse.proto"
    defaultService
    (Proxy :: Proxy CreateNetworkInterface)

responseModifyNetworkInterfaceAttribute :: ModifyNetworkInterfaceAttributeResponse -> TestTree
responseModifyNetworkInterfaceAttribute =
  res
    "ModifyNetworkInterfaceAttributeResponse"
    "fixture/ModifyNetworkInterfaceAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyNetworkInterfaceAttribute)

responseCreateNatGateway :: CreateNatGatewayResponse -> TestTree
responseCreateNatGateway =
  res
    "CreateNatGatewayResponse"
    "fixture/CreateNatGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy CreateNatGateway)

responseGetAssociatedEnclaveCertificateIamRoles :: GetAssociatedEnclaveCertificateIamRolesResponse -> TestTree
responseGetAssociatedEnclaveCertificateIamRoles =
  res
    "GetAssociatedEnclaveCertificateIamRolesResponse"
    "fixture/GetAssociatedEnclaveCertificateIamRolesResponse.proto"
    defaultService
    (Proxy :: Proxy GetAssociatedEnclaveCertificateIamRoles)

responseCreateInternetGateway :: CreateInternetGatewayResponse -> TestTree
responseCreateInternetGateway =
  res
    "CreateInternetGatewayResponse"
    "fixture/CreateInternetGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy CreateInternetGateway)

responseEnableTransitGatewayRouteTablePropagation :: EnableTransitGatewayRouteTablePropagationResponse -> TestTree
responseEnableTransitGatewayRouteTablePropagation =
  res
    "EnableTransitGatewayRouteTablePropagationResponse"
    "fixture/EnableTransitGatewayRouteTablePropagationResponse.proto"
    defaultService
    (Proxy :: Proxy EnableTransitGatewayRouteTablePropagation)

responseResetAddressAttribute :: ResetAddressAttributeResponse -> TestTree
responseResetAddressAttribute =
  res
    "ResetAddressAttributeResponse"
    "fixture/ResetAddressAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy ResetAddressAttribute)

responseDescribeTransitGatewayConnects :: DescribeTransitGatewayConnectsResponse -> TestTree
responseDescribeTransitGatewayConnects =
  res
    "DescribeTransitGatewayConnectsResponse"
    "fixture/DescribeTransitGatewayConnectsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTransitGatewayConnects)

responseDeletePlacementGroup :: DeletePlacementGroupResponse -> TestTree
responseDeletePlacementGroup =
  res
    "DeletePlacementGroupResponse"
    "fixture/DeletePlacementGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePlacementGroup)

responseDescribeInstanceTypes :: DescribeInstanceTypesResponse -> TestTree
responseDescribeInstanceTypes =
  res
    "DescribeInstanceTypesResponse"
    "fixture/DescribeInstanceTypesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInstanceTypes)

responseDescribeBundleTasks :: DescribeBundleTasksResponse -> TestTree
responseDescribeBundleTasks =
  res
    "DescribeBundleTasksResponse"
    "fixture/DescribeBundleTasksResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeBundleTasks)

responseModifySubnetAttribute :: ModifySubnetAttributeResponse -> TestTree
responseModifySubnetAttribute =
  res
    "ModifySubnetAttributeResponse"
    "fixture/ModifySubnetAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy ModifySubnetAttribute)

responseDescribeSecurityGroups :: DescribeSecurityGroupsResponse -> TestTree
responseDescribeSecurityGroups =
  res
    "DescribeSecurityGroupsResponse"
    "fixture/DescribeSecurityGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSecurityGroups)

responseCreateClientVpnRoute :: CreateClientVpnRouteResponse -> TestTree
responseCreateClientVpnRoute =
  res
    "CreateClientVpnRouteResponse"
    "fixture/CreateClientVpnRouteResponse.proto"
    defaultService
    (Proxy :: Proxy CreateClientVpnRoute)

responseDisassociateSubnetCidrBlock :: DisassociateSubnetCidrBlockResponse -> TestTree
responseDisassociateSubnetCidrBlock =
  res
    "DisassociateSubnetCidrBlockResponse"
    "fixture/DisassociateSubnetCidrBlockResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateSubnetCidrBlock)

responseDescribeSpotDatafeedSubscription :: DescribeSpotDatafeedSubscriptionResponse -> TestTree
responseDescribeSpotDatafeedSubscription =
  res
    "DescribeSpotDatafeedSubscriptionResponse"
    "fixture/DescribeSpotDatafeedSubscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSpotDatafeedSubscription)

responseCreateInstanceExportTask :: CreateInstanceExportTaskResponse -> TestTree
responseCreateInstanceExportTask =
  res
    "CreateInstanceExportTaskResponse"
    "fixture/CreateInstanceExportTaskResponse.proto"
    defaultService
    (Proxy :: Proxy CreateInstanceExportTask)

responseDisassociateClientVpnTargetNetwork :: DisassociateClientVpnTargetNetworkResponse -> TestTree
responseDisassociateClientVpnTargetNetwork =
  res
    "DisassociateClientVpnTargetNetworkResponse"
    "fixture/DisassociateClientVpnTargetNetworkResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateClientVpnTargetNetwork)

responseSendDiagnosticInterrupt :: SendDiagnosticInterruptResponse -> TestTree
responseSendDiagnosticInterrupt =
  res
    "SendDiagnosticInterruptResponse"
    "fixture/SendDiagnosticInterruptResponse.proto"
    defaultService
    (Proxy :: Proxy SendDiagnosticInterrupt)

responseDescribeVpcAttribute :: DescribeVpcAttributeResponse -> TestTree
responseDescribeVpcAttribute =
  res
    "DescribeVpcAttributeResponse"
    "fixture/DescribeVpcAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeVpcAttribute)

responseDescribeSecurityGroupReferences :: DescribeSecurityGroupReferencesResponse -> TestTree
responseDescribeSecurityGroupReferences =
  res
    "DescribeSecurityGroupReferencesResponse"
    "fixture/DescribeSecurityGroupReferencesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSecurityGroupReferences)

responseModifyCapacityReservation :: ModifyCapacityReservationResponse -> TestTree
responseModifyCapacityReservation =
  res
    "ModifyCapacityReservationResponse"
    "fixture/ModifyCapacityReservationResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyCapacityReservation)

responseDetachInternetGateway :: DetachInternetGatewayResponse -> TestTree
responseDetachInternetGateway =
  res
    "DetachInternetGatewayResponse"
    "fixture/DetachInternetGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy DetachInternetGateway)

responseCreateVolume :: Volume -> TestTree
responseCreateVolume =
  res
    "CreateVolumeResponse"
    "fixture/CreateVolumeResponse.proto"
    defaultService
    (Proxy :: Proxy CreateVolume)

responseExportClientVpnClientConfiguration :: ExportClientVpnClientConfigurationResponse -> TestTree
responseExportClientVpnClientConfiguration =
  res
    "ExportClientVpnClientConfigurationResponse"
    "fixture/ExportClientVpnClientConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy ExportClientVpnClientConfiguration)

responseRevokeSecurityGroupEgress :: RevokeSecurityGroupEgressResponse -> TestTree
responseRevokeSecurityGroupEgress =
  res
    "RevokeSecurityGroupEgressResponse"
    "fixture/RevokeSecurityGroupEgressResponse.proto"
    defaultService
    (Proxy :: Proxy RevokeSecurityGroupEgress)

responseDeleteKeyPair :: DeleteKeyPairResponse -> TestTree
responseDeleteKeyPair =
  res
    "DeleteKeyPairResponse"
    "fixture/DeleteKeyPairResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteKeyPair)

responseModifyInstanceMetadataOptions :: ModifyInstanceMetadataOptionsResponse -> TestTree
responseModifyInstanceMetadataOptions =
  res
    "ModifyInstanceMetadataOptionsResponse"
    "fixture/ModifyInstanceMetadataOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyInstanceMetadataOptions)

responseDescribeEgressOnlyInternetGateways :: DescribeEgressOnlyInternetGatewaysResponse -> TestTree
responseDescribeEgressOnlyInternetGateways =
  res
    "DescribeEgressOnlyInternetGatewaysResponse"
    "fixture/DescribeEgressOnlyInternetGatewaysResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEgressOnlyInternetGateways)

responseModifyTrafficMirrorFilterNetworkServices :: ModifyTrafficMirrorFilterNetworkServicesResponse -> TestTree
responseModifyTrafficMirrorFilterNetworkServices =
  res
    "ModifyTrafficMirrorFilterNetworkServicesResponse"
    "fixture/ModifyTrafficMirrorFilterNetworkServicesResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyTrafficMirrorFilterNetworkServices)

responseImportSnapshot :: ImportSnapshotResponse -> TestTree
responseImportSnapshot =
  res
    "ImportSnapshotResponse"
    "fixture/ImportSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy ImportSnapshot)

responseDescribeImages :: DescribeImagesResponse -> TestTree
responseDescribeImages =
  res
    "DescribeImagesResponse"
    "fixture/DescribeImagesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeImages)

responseDeprovisionByoipCidr :: DeprovisionByoipCidrResponse -> TestTree
responseDeprovisionByoipCidr =
  res
    "DeprovisionByoipCidrResponse"
    "fixture/DeprovisionByoipCidrResponse.proto"
    defaultService
    (Proxy :: Proxy DeprovisionByoipCidr)

responseDescribeAddressesAttribute :: DescribeAddressesAttributeResponse -> TestTree
responseDescribeAddressesAttribute =
  res
    "DescribeAddressesAttributeResponse"
    "fixture/DescribeAddressesAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAddressesAttribute)

responseAcceptVpcPeeringConnection :: AcceptVpcPeeringConnectionResponse -> TestTree
responseAcceptVpcPeeringConnection =
  res
    "AcceptVpcPeeringConnectionResponse"
    "fixture/AcceptVpcPeeringConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy AcceptVpcPeeringConnection)

responseDescribeMovingAddresses :: DescribeMovingAddressesResponse -> TestTree
responseDescribeMovingAddresses =
  res
    "DescribeMovingAddressesResponse"
    "fixture/DescribeMovingAddressesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeMovingAddresses)

responseCreateVpcEndpointConnectionNotification :: CreateVpcEndpointConnectionNotificationResponse -> TestTree
responseCreateVpcEndpointConnectionNotification =
  res
    "CreateVpcEndpointConnectionNotificationResponse"
    "fixture/CreateVpcEndpointConnectionNotificationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateVpcEndpointConnectionNotification)

responseDescribeFleetHistory :: DescribeFleetHistoryResponse -> TestTree
responseDescribeFleetHistory =
  res
    "DescribeFleetHistoryResponse"
    "fixture/DescribeFleetHistoryResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeFleetHistory)

responseDeleteVpcEndpointServiceConfigurations :: DeleteVpcEndpointServiceConfigurationsResponse -> TestTree
responseDeleteVpcEndpointServiceConfigurations =
  res
    "DeleteVpcEndpointServiceConfigurationsResponse"
    "fixture/DeleteVpcEndpointServiceConfigurationsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVpcEndpointServiceConfigurations)

responseCreateVpc :: CreateVpcResponse -> TestTree
responseCreateVpc =
  res
    "CreateVpcResponse"
    "fixture/CreateVpcResponse.proto"
    defaultService
    (Proxy :: Proxy CreateVpc)

responseSearchLocalGatewayRoutes :: SearchLocalGatewayRoutesResponse -> TestTree
responseSearchLocalGatewayRoutes =
  res
    "SearchLocalGatewayRoutesResponse"
    "fixture/SearchLocalGatewayRoutesResponse.proto"
    defaultService
    (Proxy :: Proxy SearchLocalGatewayRoutes)

responseCreateTrafficMirrorTarget :: CreateTrafficMirrorTargetResponse -> TestTree
responseCreateTrafficMirrorTarget =
  res
    "CreateTrafficMirrorTargetResponse"
    "fixture/CreateTrafficMirrorTargetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTrafficMirrorTarget)

responseDescribeVolumeStatus :: DescribeVolumeStatusResponse -> TestTree
responseDescribeVolumeStatus =
  res
    "DescribeVolumeStatusResponse"
    "fixture/DescribeVolumeStatusResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeVolumeStatus)

responseDescribeVolumeAttribute :: DescribeVolumeAttributeResponse -> TestTree
responseDescribeVolumeAttribute =
  res
    "DescribeVolumeAttributeResponse"
    "fixture/DescribeVolumeAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeVolumeAttribute)

responseDeleteClientVpnRoute :: DeleteClientVpnRouteResponse -> TestTree
responseDeleteClientVpnRoute =
  res
    "DeleteClientVpnRouteResponse"
    "fixture/DeleteClientVpnRouteResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteClientVpnRoute)

responseModifyVpcPeeringConnectionOptions :: ModifyVpcPeeringConnectionOptionsResponse -> TestTree
responseModifyVpcPeeringConnectionOptions =
  res
    "ModifyVpcPeeringConnectionOptionsResponse"
    "fixture/ModifyVpcPeeringConnectionOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyVpcPeeringConnectionOptions)

responseDescribeSpotFleetInstances :: DescribeSpotFleetInstancesResponse -> TestTree
responseDescribeSpotFleetInstances =
  res
    "DescribeSpotFleetInstancesResponse"
    "fixture/DescribeSpotFleetInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSpotFleetInstances)

responseDescribePrincipalIdFormat :: DescribePrincipalIdFormatResponse -> TestTree
responseDescribePrincipalIdFormat =
  res
    "DescribePrincipalIdFormatResponse"
    "fixture/DescribePrincipalIdFormatResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePrincipalIdFormat)

responseModifyInstanceCreditSpecification :: ModifyInstanceCreditSpecificationResponse -> TestTree
responseModifyInstanceCreditSpecification =
  res
    "ModifyInstanceCreditSpecificationResponse"
    "fixture/ModifyInstanceCreditSpecificationResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyInstanceCreditSpecification)

responseDisassociateTransitGatewayMulticastDomain :: DisassociateTransitGatewayMulticastDomainResponse -> TestTree
responseDisassociateTransitGatewayMulticastDomain =
  res
    "DisassociateTransitGatewayMulticastDomainResponse"
    "fixture/DisassociateTransitGatewayMulticastDomainResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateTransitGatewayMulticastDomain)

responseDescribeManagedPrefixLists :: DescribeManagedPrefixListsResponse -> TestTree
responseDescribeManagedPrefixLists =
  res
    "DescribeManagedPrefixListsResponse"
    "fixture/DescribeManagedPrefixListsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeManagedPrefixLists)

responseGetPasswordData :: GetPasswordDataResponse -> TestTree
responseGetPasswordData =
  res
    "GetPasswordDataResponse"
    "fixture/GetPasswordDataResponse.proto"
    defaultService
    (Proxy :: Proxy GetPasswordData)

responseDeleteVolume :: DeleteVolumeResponse -> TestTree
responseDeleteVolume =
  res
    "DeleteVolumeResponse"
    "fixture/DeleteVolumeResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVolume)

responseDescribeTransitGateways :: DescribeTransitGatewaysResponse -> TestTree
responseDescribeTransitGateways =
  res
    "DescribeTransitGatewaysResponse"
    "fixture/DescribeTransitGatewaysResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTransitGateways)

responseDescribeSpotFleetRequests :: DescribeSpotFleetRequestsResponse -> TestTree
responseDescribeSpotFleetRequests =
  res
    "DescribeSpotFleetRequestsResponse"
    "fixture/DescribeSpotFleetRequestsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSpotFleetRequests)

responseDescribeClientVpnConnections :: DescribeClientVpnConnectionsResponse -> TestTree
responseDescribeClientVpnConnections =
  res
    "DescribeClientVpnConnectionsResponse"
    "fixture/DescribeClientVpnConnectionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeClientVpnConnections)

responseSearchTransitGatewayMulticastGroups :: SearchTransitGatewayMulticastGroupsResponse -> TestTree
responseSearchTransitGatewayMulticastGroups =
  res
    "SearchTransitGatewayMulticastGroupsResponse"
    "fixture/SearchTransitGatewayMulticastGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy SearchTransitGatewayMulticastGroups)

responseModifyVpcAttribute :: ModifyVpcAttributeResponse -> TestTree
responseModifyVpcAttribute =
  res
    "ModifyVpcAttributeResponse"
    "fixture/ModifyVpcAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyVpcAttribute)

responseRevokeSecurityGroupIngress :: RevokeSecurityGroupIngressResponse -> TestTree
responseRevokeSecurityGroupIngress =
  res
    "RevokeSecurityGroupIngressResponse"
    "fixture/RevokeSecurityGroupIngressResponse.proto"
    defaultService
    (Proxy :: Proxy RevokeSecurityGroupIngress)

responseDescribeHostReservationOfferings :: DescribeHostReservationOfferingsResponse -> TestTree
responseDescribeHostReservationOfferings =
  res
    "DescribeHostReservationOfferingsResponse"
    "fixture/DescribeHostReservationOfferingsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeHostReservationOfferings)

responseDescribeTransitGatewayRouteTables :: DescribeTransitGatewayRouteTablesResponse -> TestTree
responseDescribeTransitGatewayRouteTables =
  res
    "DescribeTransitGatewayRouteTablesResponse"
    "fixture/DescribeTransitGatewayRouteTablesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTransitGatewayRouteTables)

responseDescribeNetworkAcls :: DescribeNetworkAclsResponse -> TestTree
responseDescribeNetworkAcls =
  res
    "DescribeNetworkAclsResponse"
    "fixture/DescribeNetworkAclsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeNetworkAcls)

responseRegisterTransitGatewayMulticastGroupMembers :: RegisterTransitGatewayMulticastGroupMembersResponse -> TestTree
responseRegisterTransitGatewayMulticastGroupMembers =
  res
    "RegisterTransitGatewayMulticastGroupMembersResponse"
    "fixture/RegisterTransitGatewayMulticastGroupMembersResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterTransitGatewayMulticastGroupMembers)

responseDescribeHosts :: DescribeHostsResponse -> TestTree
responseDescribeHosts =
  res
    "DescribeHostsResponse"
    "fixture/DescribeHostsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeHosts)

responseDescribeVpnGateways :: DescribeVpnGatewaysResponse -> TestTree
responseDescribeVpnGateways =
  res
    "DescribeVpnGatewaysResponse"
    "fixture/DescribeVpnGatewaysResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeVpnGateways)

responseDescribeHostReservations :: DescribeHostReservationsResponse -> TestTree
responseDescribeHostReservations =
  res
    "DescribeHostReservationsResponse"
    "fixture/DescribeHostReservationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeHostReservations)

responseDeleteManagedPrefixList :: DeleteManagedPrefixListResponse -> TestTree
responseDeleteManagedPrefixList =
  res
    "DeleteManagedPrefixListResponse"
    "fixture/DeleteManagedPrefixListResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteManagedPrefixList)

responseRejectVpcPeeringConnection :: RejectVpcPeeringConnectionResponse -> TestTree
responseRejectVpcPeeringConnection =
  res
    "RejectVpcPeeringConnectionResponse"
    "fixture/RejectVpcPeeringConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy RejectVpcPeeringConnection)

responseResetImageAttribute :: ResetImageAttributeResponse -> TestTree
responseResetImageAttribute =
  res
    "ResetImageAttributeResponse"
    "fixture/ResetImageAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy ResetImageAttribute)

responseDescribeScheduledInstances :: DescribeScheduledInstancesResponse -> TestTree
responseDescribeScheduledInstances =
  res
    "DescribeScheduledInstancesResponse"
    "fixture/DescribeScheduledInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeScheduledInstances)

responseAssociateEnclaveCertificateIamRole :: AssociateEnclaveCertificateIamRoleResponse -> TestTree
responseAssociateEnclaveCertificateIamRole =
  res
    "AssociateEnclaveCertificateIamRoleResponse"
    "fixture/AssociateEnclaveCertificateIamRoleResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateEnclaveCertificateIamRole)

responseModifyTransitGatewayPrefixListReference :: ModifyTransitGatewayPrefixListReferenceResponse -> TestTree
responseModifyTransitGatewayPrefixListReference =
  res
    "ModifyTransitGatewayPrefixListReferenceResponse"
    "fixture/ModifyTransitGatewayPrefixListReferenceResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyTransitGatewayPrefixListReference)

responseDescribeFpgaImageAttribute :: DescribeFpgaImageAttributeResponse -> TestTree
responseDescribeFpgaImageAttribute =
  res
    "DescribeFpgaImageAttributeResponse"
    "fixture/DescribeFpgaImageAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeFpgaImageAttribute)

responseAdvertiseByoipCidr :: AdvertiseByoipCidrResponse -> TestTree
responseAdvertiseByoipCidr =
  res
    "AdvertiseByoipCidrResponse"
    "fixture/AdvertiseByoipCidrResponse.proto"
    defaultService
    (Proxy :: Proxy AdvertiseByoipCidr)

responseDeleteVpnConnectionRoute :: DeleteVpnConnectionRouteResponse -> TestTree
responseDeleteVpnConnectionRoute =
  res
    "DeleteVpnConnectionRouteResponse"
    "fixture/DeleteVpnConnectionRouteResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVpnConnectionRoute)

responseDescribeVpcEndpointServicePermissions :: DescribeVpcEndpointServicePermissionsResponse -> TestTree
responseDescribeVpcEndpointServicePermissions =
  res
    "DescribeVpcEndpointServicePermissionsResponse"
    "fixture/DescribeVpcEndpointServicePermissionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeVpcEndpointServicePermissions)

responseDescribeVpcEndpointConnections :: DescribeVpcEndpointConnectionsResponse -> TestTree
responseDescribeVpcEndpointConnections =
  res
    "DescribeVpcEndpointConnectionsResponse"
    "fixture/DescribeVpcEndpointConnectionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeVpcEndpointConnections)

responseDescribeNetworkInterfacePermissions :: DescribeNetworkInterfacePermissionsResponse -> TestTree
responseDescribeNetworkInterfacePermissions =
  res
    "DescribeNetworkInterfacePermissionsResponse"
    "fixture/DescribeNetworkInterfacePermissionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeNetworkInterfacePermissions)

responseCreateTrafficMirrorSession :: CreateTrafficMirrorSessionResponse -> TestTree
responseCreateTrafficMirrorSession =
  res
    "CreateTrafficMirrorSessionResponse"
    "fixture/CreateTrafficMirrorSessionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTrafficMirrorSession)

responseRegisterInstanceEventNotificationAttributes :: RegisterInstanceEventNotificationAttributesResponse -> TestTree
responseRegisterInstanceEventNotificationAttributes =
  res
    "RegisterInstanceEventNotificationAttributesResponse"
    "fixture/RegisterInstanceEventNotificationAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterInstanceEventNotificationAttributes)

responseRejectTransitGatewayMulticastDomainAssociations :: RejectTransitGatewayMulticastDomainAssociationsResponse -> TestTree
responseRejectTransitGatewayMulticastDomainAssociations =
  res
    "RejectTransitGatewayMulticastDomainAssociationsResponse"
    "fixture/RejectTransitGatewayMulticastDomainAssociationsResponse.proto"
    defaultService
    (Proxy :: Proxy RejectTransitGatewayMulticastDomainAssociations)

responseDeleteDhcpOptions :: DeleteDhcpOptionsResponse -> TestTree
responseDeleteDhcpOptions =
  res
    "DeleteDhcpOptionsResponse"
    "fixture/DeleteDhcpOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDhcpOptions)

responseDeleteTransitGateway :: DeleteTransitGatewayResponse -> TestTree
responseDeleteTransitGateway =
  res
    "DeleteTransitGatewayResponse"
    "fixture/DeleteTransitGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTransitGateway)

responseEnableVpcClassicLinkDnsSupport :: EnableVpcClassicLinkDnsSupportResponse -> TestTree
responseEnableVpcClassicLinkDnsSupport =
  res
    "EnableVpcClassicLinkDnsSupportResponse"
    "fixture/EnableVpcClassicLinkDnsSupportResponse.proto"
    defaultService
    (Proxy :: Proxy EnableVpcClassicLinkDnsSupport)

responseDescribeRegions :: DescribeRegionsResponse -> TestTree
responseDescribeRegions =
  res
    "DescribeRegionsResponse"
    "fixture/DescribeRegionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeRegions)

responseCreateEgressOnlyInternetGateway :: CreateEgressOnlyInternetGatewayResponse -> TestTree
responseCreateEgressOnlyInternetGateway =
  res
    "CreateEgressOnlyInternetGatewayResponse"
    "fixture/CreateEgressOnlyInternetGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy CreateEgressOnlyInternetGateway)

responseCreateTransitGateway :: CreateTransitGatewayResponse -> TestTree
responseCreateTransitGateway =
  res
    "CreateTransitGatewayResponse"
    "fixture/CreateTransitGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTransitGateway)

responseDeleteClientVpnEndpoint :: DeleteClientVpnEndpointResponse -> TestTree
responseDeleteClientVpnEndpoint =
  res
    "DeleteClientVpnEndpointResponse"
    "fixture/DeleteClientVpnEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteClientVpnEndpoint)

responseExportClientVpnClientCertificateRevocationList :: ExportClientVpnClientCertificateRevocationListResponse -> TestTree
responseExportClientVpnClientCertificateRevocationList =
  res
    "ExportClientVpnClientCertificateRevocationListResponse"
    "fixture/ExportClientVpnClientCertificateRevocationListResponse.proto"
    defaultService
    (Proxy :: Proxy ExportClientVpnClientCertificateRevocationList)

responseCreateLaunchTemplateVersion :: CreateLaunchTemplateVersionResponse -> TestTree
responseCreateLaunchTemplateVersion =
  res
    "CreateLaunchTemplateVersionResponse"
    "fixture/CreateLaunchTemplateVersionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateLaunchTemplateVersion)

responseCreateSnapshots :: CreateSnapshotsResponse -> TestTree
responseCreateSnapshots =
  res
    "CreateSnapshotsResponse"
    "fixture/CreateSnapshotsResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSnapshots)

responseModifyDefaultCreditSpecification :: ModifyDefaultCreditSpecificationResponse -> TestTree
responseModifyDefaultCreditSpecification =
  res
    "ModifyDefaultCreditSpecificationResponse"
    "fixture/ModifyDefaultCreditSpecificationResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyDefaultCreditSpecification)

responseApplySecurityGroupsToClientVpnTargetNetwork :: ApplySecurityGroupsToClientVpnTargetNetworkResponse -> TestTree
responseApplySecurityGroupsToClientVpnTargetNetwork =
  res
    "ApplySecurityGroupsToClientVpnTargetNetworkResponse"
    "fixture/ApplySecurityGroupsToClientVpnTargetNetworkResponse.proto"
    defaultService
    (Proxy :: Proxy ApplySecurityGroupsToClientVpnTargetNetwork)

responseAttachVpnGateway :: AttachVpnGatewayResponse -> TestTree
responseAttachVpnGateway =
  res
    "AttachVpnGatewayResponse"
    "fixture/AttachVpnGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy AttachVpnGateway)

responseCreateVpnConnectionRoute :: CreateVpnConnectionRouteResponse -> TestTree
responseCreateVpnConnectionRoute =
  res
    "CreateVpnConnectionRouteResponse"
    "fixture/CreateVpnConnectionRouteResponse.proto"
    defaultService
    (Proxy :: Proxy CreateVpnConnectionRoute)

responseDescribeKeyPairs :: DescribeKeyPairsResponse -> TestTree
responseDescribeKeyPairs =
  res
    "DescribeKeyPairsResponse"
    "fixture/DescribeKeyPairsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeKeyPairs)

responseAllocateAddress :: AllocateAddressResponse -> TestTree
responseAllocateAddress =
  res
    "AllocateAddressResponse"
    "fixture/AllocateAddressResponse.proto"
    defaultService
    (Proxy :: Proxy AllocateAddress)

responseDeleteTrafficMirrorSession :: DeleteTrafficMirrorSessionResponse -> TestTree
responseDeleteTrafficMirrorSession =
  res
    "DeleteTrafficMirrorSessionResponse"
    "fixture/DeleteTrafficMirrorSessionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTrafficMirrorSession)

responseGetManagedPrefixListEntries :: GetManagedPrefixListEntriesResponse -> TestTree
responseGetManagedPrefixListEntries =
  res
    "GetManagedPrefixListEntriesResponse"
    "fixture/GetManagedPrefixListEntriesResponse.proto"
    defaultService
    (Proxy :: Proxy GetManagedPrefixListEntries)

responseCreateFpgaImage :: CreateFpgaImageResponse -> TestTree
responseCreateFpgaImage =
  res
    "CreateFpgaImageResponse"
    "fixture/CreateFpgaImageResponse.proto"
    defaultService
    (Proxy :: Proxy CreateFpgaImage)

responseExportImage :: ExportImageResponse -> TestTree
responseExportImage =
  res
    "ExportImageResponse"
    "fixture/ExportImageResponse.proto"
    defaultService
    (Proxy :: Proxy ExportImage)

responseRejectTransitGatewayPeeringAttachment :: RejectTransitGatewayPeeringAttachmentResponse -> TestTree
responseRejectTransitGatewayPeeringAttachment =
  res
    "RejectTransitGatewayPeeringAttachmentResponse"
    "fixture/RejectTransitGatewayPeeringAttachmentResponse.proto"
    defaultService
    (Proxy :: Proxy RejectTransitGatewayPeeringAttachment)

responseDescribeConversionTasks :: DescribeConversionTasksResponse -> TestTree
responseDescribeConversionTasks =
  res
    "DescribeConversionTasksResponse"
    "fixture/DescribeConversionTasksResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeConversionTasks)

responseWithdrawByoipCidr :: WithdrawByoipCidrResponse -> TestTree
responseWithdrawByoipCidr =
  res
    "WithdrawByoipCidrResponse"
    "fixture/WithdrawByoipCidrResponse.proto"
    defaultService
    (Proxy :: Proxy WithdrawByoipCidr)

responseDeleteTrafficMirrorFilterRule :: DeleteTrafficMirrorFilterRuleResponse -> TestTree
responseDeleteTrafficMirrorFilterRule =
  res
    "DeleteTrafficMirrorFilterRuleResponse"
    "fixture/DeleteTrafficMirrorFilterRuleResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTrafficMirrorFilterRule)

responseDescribeClassicLinkInstances :: DescribeClassicLinkInstancesResponse -> TestTree
responseDescribeClassicLinkInstances =
  res
    "DescribeClassicLinkInstancesResponse"
    "fixture/DescribeClassicLinkInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeClassicLinkInstances)

responseTerminateInstances :: TerminateInstancesResponse -> TestTree
responseTerminateInstances =
  res
    "TerminateInstancesResponse"
    "fixture/TerminateInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy TerminateInstances)

responseAcceptTransitGatewayVpcAttachment :: AcceptTransitGatewayVpcAttachmentResponse -> TestTree
responseAcceptTransitGatewayVpcAttachment =
  res
    "AcceptTransitGatewayVpcAttachmentResponse"
    "fixture/AcceptTransitGatewayVpcAttachmentResponse.proto"
    defaultService
    (Proxy :: Proxy AcceptTransitGatewayVpcAttachment)

responseDisableVpcClassicLinkDnsSupport :: DisableVpcClassicLinkDnsSupportResponse -> TestTree
responseDisableVpcClassicLinkDnsSupport =
  res
    "DisableVpcClassicLinkDnsSupportResponse"
    "fixture/DisableVpcClassicLinkDnsSupportResponse.proto"
    defaultService
    (Proxy :: Proxy DisableVpcClassicLinkDnsSupport)

responseGetLaunchTemplateData :: GetLaunchTemplateDataResponse -> TestTree
responseGetLaunchTemplateData =
  res
    "GetLaunchTemplateDataResponse"
    "fixture/GetLaunchTemplateDataResponse.proto"
    defaultService
    (Proxy :: Proxy GetLaunchTemplateData)

responseDescribeReservedInstances :: DescribeReservedInstancesResponse -> TestTree
responseDescribeReservedInstances =
  res
    "DescribeReservedInstancesResponse"
    "fixture/DescribeReservedInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeReservedInstances)

responseModifyFpgaImageAttribute :: ModifyFpgaImageAttributeResponse -> TestTree
responseModifyFpgaImageAttribute =
  res
    "ModifyFpgaImageAttributeResponse"
    "fixture/ModifyFpgaImageAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyFpgaImageAttribute)

responseEnableVpcClassicLink :: EnableVpcClassicLinkResponse -> TestTree
responseEnableVpcClassicLink =
  res
    "EnableVpcClassicLinkResponse"
    "fixture/EnableVpcClassicLinkResponse.proto"
    defaultService
    (Proxy :: Proxy EnableVpcClassicLink)

responseAttachInternetGateway :: AttachInternetGatewayResponse -> TestTree
responseAttachInternetGateway =
  res
    "AttachInternetGatewayResponse"
    "fixture/AttachInternetGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy AttachInternetGateway)

responseDescribePublicIpv4Pools :: DescribePublicIpv4PoolsResponse -> TestTree
responseDescribePublicIpv4Pools =
  res
    "DescribePublicIpv4PoolsResponse"
    "fixture/DescribePublicIpv4PoolsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePublicIpv4Pools)

responseCreateCustomerGateway :: CreateCustomerGatewayResponse -> TestTree
responseCreateCustomerGateway =
  res
    "CreateCustomerGatewayResponse"
    "fixture/CreateCustomerGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCustomerGateway)

responseDescribeIamInstanceProfileAssociations :: DescribeIamInstanceProfileAssociationsResponse -> TestTree
responseDescribeIamInstanceProfileAssociations =
  res
    "DescribeIamInstanceProfileAssociationsResponse"
    "fixture/DescribeIamInstanceProfileAssociationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeIamInstanceProfileAssociations)

responseDescribeExportImageTasks :: DescribeExportImageTasksResponse -> TestTree
responseDescribeExportImageTasks =
  res
    "DescribeExportImageTasksResponse"
    "fixture/DescribeExportImageTasksResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeExportImageTasks)

responseProvisionByoipCidr :: ProvisionByoipCidrResponse -> TestTree
responseProvisionByoipCidr =
  res
    "ProvisionByoipCidrResponse"
    "fixture/ProvisionByoipCidrResponse.proto"
    defaultService
    (Proxy :: Proxy ProvisionByoipCidr)

responseCreateReservedInstancesListing :: CreateReservedInstancesListingResponse -> TestTree
responseCreateReservedInstancesListing =
  res
    "CreateReservedInstancesListingResponse"
    "fixture/CreateReservedInstancesListingResponse.proto"
    defaultService
    (Proxy :: Proxy CreateReservedInstancesListing)

responseDescribeClientVpnTargetNetworks :: DescribeClientVpnTargetNetworksResponse -> TestTree
responseDescribeClientVpnTargetNetworks =
  res
    "DescribeClientVpnTargetNetworksResponse"
    "fixture/DescribeClientVpnTargetNetworksResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeClientVpnTargetNetworks)

responseModifyVpnTunnelOptions :: ModifyVpnTunnelOptionsResponse -> TestTree
responseModifyVpnTunnelOptions =
  res
    "ModifyVpnTunnelOptionsResponse"
    "fixture/ModifyVpnTunnelOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyVpnTunnelOptions)

responseModifyInstancePlacement :: ModifyInstancePlacementResponse -> TestTree
responseModifyInstancePlacement =
  res
    "ModifyInstancePlacementResponse"
    "fixture/ModifyInstancePlacementResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyInstancePlacement)

responseImportKeyPair :: ImportKeyPairResponse -> TestTree
responseImportKeyPair =
  res
    "ImportKeyPairResponse"
    "fixture/ImportKeyPairResponse.proto"
    defaultService
    (Proxy :: Proxy ImportKeyPair)

responseDescribeNetworkInsightsAnalyses :: DescribeNetworkInsightsAnalysesResponse -> TestTree
responseDescribeNetworkInsightsAnalyses =
  res
    "DescribeNetworkInsightsAnalysesResponse"
    "fixture/DescribeNetworkInsightsAnalysesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeNetworkInsightsAnalyses)

responseDeleteSecurityGroup :: DeleteSecurityGroupResponse -> TestTree
responseDeleteSecurityGroup =
  res
    "DeleteSecurityGroupResponse"
    "fixture/DeleteSecurityGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSecurityGroup)

responseCreateCarrierGateway :: CreateCarrierGatewayResponse -> TestTree
responseCreateCarrierGateway =
  res
    "CreateCarrierGatewayResponse"
    "fixture/CreateCarrierGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCarrierGateway)

responseCreateSnapshot :: Snapshot -> TestTree
responseCreateSnapshot =
  res
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSnapshot)

responseModifyVolume :: ModifyVolumeResponse -> TestTree
responseModifyVolume =
  res
    "ModifyVolumeResponse"
    "fixture/ModifyVolumeResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyVolume)

responseDeleteNetworkInsightsAnalysis :: DeleteNetworkInsightsAnalysisResponse -> TestTree
responseDeleteNetworkInsightsAnalysis =
  res
    "DeleteNetworkInsightsAnalysisResponse"
    "fixture/DeleteNetworkInsightsAnalysisResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteNetworkInsightsAnalysis)

responseDescribeLocalGatewayRouteTableVpcAssociations :: DescribeLocalGatewayRouteTableVpcAssociationsResponse -> TestTree
responseDescribeLocalGatewayRouteTableVpcAssociations =
  res
    "DescribeLocalGatewayRouteTableVpcAssociationsResponse"
    "fixture/DescribeLocalGatewayRouteTableVpcAssociationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLocalGatewayRouteTableVpcAssociations)

responseCreateTrafficMirrorFilter :: CreateTrafficMirrorFilterResponse -> TestTree
responseCreateTrafficMirrorFilter =
  res
    "CreateTrafficMirrorFilterResponse"
    "fixture/CreateTrafficMirrorFilterResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTrafficMirrorFilter)

responseDeleteSpotDatafeedSubscription :: DeleteSpotDatafeedSubscriptionResponse -> TestTree
responseDeleteSpotDatafeedSubscription =
  res
    "DeleteSpotDatafeedSubscriptionResponse"
    "fixture/DeleteSpotDatafeedSubscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSpotDatafeedSubscription)

responseDescribeInstanceAttribute :: DescribeInstanceAttributeResponse -> TestTree
responseDescribeInstanceAttribute =
  res
    "DescribeInstanceAttributeResponse"
    "fixture/DescribeInstanceAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInstanceAttribute)

responseCreateCapacityReservation :: CreateCapacityReservationResponse -> TestTree
responseCreateCapacityReservation =
  res
    "CreateCapacityReservationResponse"
    "fixture/CreateCapacityReservationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCapacityReservation)

responseDeleteTransitGatewayConnect :: DeleteTransitGatewayConnectResponse -> TestTree
responseDeleteTransitGatewayConnect =
  res
    "DeleteTransitGatewayConnectResponse"
    "fixture/DeleteTransitGatewayConnectResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTransitGatewayConnect)

responseModifyEbsDefaultKmsKeyId :: ModifyEbsDefaultKmsKeyIdResponse -> TestTree
responseModifyEbsDefaultKmsKeyId =
  res
    "ModifyEbsDefaultKmsKeyIdResponse"
    "fixture/ModifyEbsDefaultKmsKeyIdResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyEbsDefaultKmsKeyId)

responseDeleteRoute :: DeleteRouteResponse -> TestTree
responseDeleteRoute =
  res
    "DeleteRouteResponse"
    "fixture/DeleteRouteResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRoute)

responseDescribeNetworkInsightsPaths :: DescribeNetworkInsightsPathsResponse -> TestTree
responseDescribeNetworkInsightsPaths =
  res
    "DescribeNetworkInsightsPathsResponse"
    "fixture/DescribeNetworkInsightsPathsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeNetworkInsightsPaths)

responsePurchaseScheduledInstances :: PurchaseScheduledInstancesResponse -> TestTree
responsePurchaseScheduledInstances =
  res
    "PurchaseScheduledInstancesResponse"
    "fixture/PurchaseScheduledInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy PurchaseScheduledInstances)

responseCreateTransitGatewayPeeringAttachment :: CreateTransitGatewayPeeringAttachmentResponse -> TestTree
responseCreateTransitGatewayPeeringAttachment =
  res
    "CreateTransitGatewayPeeringAttachmentResponse"
    "fixture/CreateTransitGatewayPeeringAttachmentResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTransitGatewayPeeringAttachment)

responseGetDefaultCreditSpecification :: GetDefaultCreditSpecificationResponse -> TestTree
responseGetDefaultCreditSpecification =
  res
    "GetDefaultCreditSpecificationResponse"
    "fixture/GetDefaultCreditSpecificationResponse.proto"
    defaultService
    (Proxy :: Proxy GetDefaultCreditSpecification)

responseDescribeInternetGateways :: DescribeInternetGatewaysResponse -> TestTree
responseDescribeInternetGateways =
  res
    "DescribeInternetGatewaysResponse"
    "fixture/DescribeInternetGatewaysResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInternetGateways)

responseModifyInstanceAttribute :: ModifyInstanceAttributeResponse -> TestTree
responseModifyInstanceAttribute =
  res
    "ModifyInstanceAttributeResponse"
    "fixture/ModifyInstanceAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyInstanceAttribute)

responseCreateSecurityGroup :: CreateSecurityGroupResponse -> TestTree
responseCreateSecurityGroup =
  res
    "CreateSecurityGroupResponse"
    "fixture/CreateSecurityGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSecurityGroup)

responseCreateTransitGatewayConnect :: CreateTransitGatewayConnectResponse -> TestTree
responseCreateTransitGatewayConnect =
  res
    "CreateTransitGatewayConnectResponse"
    "fixture/CreateTransitGatewayConnectResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTransitGatewayConnect)

responseReplaceNetworkAclAssociation :: ReplaceNetworkAclAssociationResponse -> TestTree
responseReplaceNetworkAclAssociation =
  res
    "ReplaceNetworkAclAssociationResponse"
    "fixture/ReplaceNetworkAclAssociationResponse.proto"
    defaultService
    (Proxy :: Proxy ReplaceNetworkAclAssociation)

responseCreateRoute :: CreateRouteResponse -> TestTree
responseCreateRoute =
  res
    "CreateRouteResponse"
    "fixture/CreateRouteResponse.proto"
    defaultService
    (Proxy :: Proxy CreateRoute)

responseDeleteLaunchTemplateVersions :: DeleteLaunchTemplateVersionsResponse -> TestTree
responseDeleteLaunchTemplateVersions =
  res
    "DeleteLaunchTemplateVersionsResponse"
    "fixture/DeleteLaunchTemplateVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteLaunchTemplateVersions)

responseDescribeIdentityIdFormat :: DescribeIdentityIdFormatResponse -> TestTree
responseDescribeIdentityIdFormat =
  res
    "DescribeIdentityIdFormatResponse"
    "fixture/DescribeIdentityIdFormatResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeIdentityIdFormat)

responseDeleteTrafficMirrorFilter :: DeleteTrafficMirrorFilterResponse -> TestTree
responseDeleteTrafficMirrorFilter =
  res
    "DeleteTrafficMirrorFilterResponse"
    "fixture/DeleteTrafficMirrorFilterResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTrafficMirrorFilter)

responseReplaceRoute :: ReplaceRouteResponse -> TestTree
responseReplaceRoute =
  res
    "ReplaceRouteResponse"
    "fixture/ReplaceRouteResponse.proto"
    defaultService
    (Proxy :: Proxy ReplaceRoute)

responseResetSnapshotAttribute :: ResetSnapshotAttributeResponse -> TestTree
responseResetSnapshotAttribute =
  res
    "ResetSnapshotAttributeResponse"
    "fixture/ResetSnapshotAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy ResetSnapshotAttribute)

responseResetEbsDefaultKmsKeyId :: ResetEbsDefaultKmsKeyIdResponse -> TestTree
responseResetEbsDefaultKmsKeyId =
  res
    "ResetEbsDefaultKmsKeyIdResponse"
    "fixture/ResetEbsDefaultKmsKeyIdResponse.proto"
    defaultService
    (Proxy :: Proxy ResetEbsDefaultKmsKeyId)

responseCreateTags :: CreateTagsResponse -> TestTree
responseCreateTags =
  res
    "CreateTagsResponse"
    "fixture/CreateTagsResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTags)

responseBundleInstance :: BundleInstanceResponse -> TestTree
responseBundleInstance =
  res
    "BundleInstanceResponse"
    "fixture/BundleInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy BundleInstance)

responseDeleteTransitGatewayPeeringAttachment :: DeleteTransitGatewayPeeringAttachmentResponse -> TestTree
responseDeleteTransitGatewayPeeringAttachment =
  res
    "DeleteTransitGatewayPeeringAttachmentResponse"
    "fixture/DeleteTransitGatewayPeeringAttachmentResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTransitGatewayPeeringAttachment)

responseAuthorizeClientVpnIngress :: AuthorizeClientVpnIngressResponse -> TestTree
responseAuthorizeClientVpnIngress =
  res
    "AuthorizeClientVpnIngressResponse"
    "fixture/AuthorizeClientVpnIngressResponse.proto"
    defaultService
    (Proxy :: Proxy AuthorizeClientVpnIngress)

responseModifyLaunchTemplate :: ModifyLaunchTemplateResponse -> TestTree
responseModifyLaunchTemplate =
  res
    "ModifyLaunchTemplateResponse"
    "fixture/ModifyLaunchTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyLaunchTemplate)

responseDeleteCustomerGateway :: DeleteCustomerGatewayResponse -> TestTree
responseDeleteCustomerGateway =
  res
    "DeleteCustomerGatewayResponse"
    "fixture/DeleteCustomerGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCustomerGateway)

responseTerminateClientVpnConnections :: TerminateClientVpnConnectionsResponse -> TestTree
responseTerminateClientVpnConnections =
  res
    "TerminateClientVpnConnectionsResponse"
    "fixture/TerminateClientVpnConnectionsResponse.proto"
    defaultService
    (Proxy :: Proxy TerminateClientVpnConnections)

responseGetEbsEncryptionByDefault :: GetEbsEncryptionByDefaultResponse -> TestTree
responseGetEbsEncryptionByDefault =
  res
    "GetEbsEncryptionByDefaultResponse"
    "fixture/GetEbsEncryptionByDefaultResponse.proto"
    defaultService
    (Proxy :: Proxy GetEbsEncryptionByDefault)

responseCreateVpcPeeringConnection :: CreateVpcPeeringConnectionResponse -> TestTree
responseCreateVpcPeeringConnection =
  res
    "CreateVpcPeeringConnectionResponse"
    "fixture/CreateVpcPeeringConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateVpcPeeringConnection)

responseDeleteTransitGatewayVpcAttachment :: DeleteTransitGatewayVpcAttachmentResponse -> TestTree
responseDeleteTransitGatewayVpcAttachment =
  res
    "DeleteTransitGatewayVpcAttachmentResponse"
    "fixture/DeleteTransitGatewayVpcAttachmentResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTransitGatewayVpcAttachment)

responseReplaceIamInstanceProfileAssociation :: ReplaceIamInstanceProfileAssociationResponse -> TestTree
responseReplaceIamInstanceProfileAssociation =
  res
    "ReplaceIamInstanceProfileAssociationResponse"
    "fixture/ReplaceIamInstanceProfileAssociationResponse.proto"
    defaultService
    (Proxy :: Proxy ReplaceIamInstanceProfileAssociation)

responseDeleteTransitGatewayConnectPeer :: DeleteTransitGatewayConnectPeerResponse -> TestTree
responseDeleteTransitGatewayConnectPeer =
  res
    "DeleteTransitGatewayConnectPeerResponse"
    "fixture/DeleteTransitGatewayConnectPeerResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTransitGatewayConnectPeer)

responseAssociateAddress :: AssociateAddressResponse -> TestTree
responseAssociateAddress =
  res
    "AssociateAddressResponse"
    "fixture/AssociateAddressResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateAddress)

responseCancelSpotFleetRequests :: CancelSpotFleetRequestsResponse -> TestTree
responseCancelSpotFleetRequests =
  res
    "CancelSpotFleetRequestsResponse"
    "fixture/CancelSpotFleetRequestsResponse.proto"
    defaultService
    (Proxy :: Proxy CancelSpotFleetRequests)

responseResetNetworkInterfaceAttribute :: ResetNetworkInterfaceAttributeResponse -> TestTree
responseResetNetworkInterfaceAttribute =
  res
    "ResetNetworkInterfaceAttributeResponse"
    "fixture/ResetNetworkInterfaceAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy ResetNetworkInterfaceAttribute)

responseStartInstances :: StartInstancesResponse -> TestTree
responseStartInstances =
  res
    "StartInstancesResponse"
    "fixture/StartInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy StartInstances)

responseDisassociateTransitGatewayRouteTable :: DisassociateTransitGatewayRouteTableResponse -> TestTree
responseDisassociateTransitGatewayRouteTable =
  res
    "DisassociateTransitGatewayRouteTableResponse"
    "fixture/DisassociateTransitGatewayRouteTableResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateTransitGatewayRouteTable)

responseCopyFpgaImage :: CopyFpgaImageResponse -> TestTree
responseCopyFpgaImage =
  res
    "CopyFpgaImageResponse"
    "fixture/CopyFpgaImageResponse.proto"
    defaultService
    (Proxy :: Proxy CopyFpgaImage)

responseReleaseHosts :: ReleaseHostsResponse -> TestTree
responseReleaseHosts =
  res
    "ReleaseHostsResponse"
    "fixture/ReleaseHostsResponse.proto"
    defaultService
    (Proxy :: Proxy ReleaseHosts)

responseDescribeFastSnapshotRestores :: DescribeFastSnapshotRestoresResponse -> TestTree
responseDescribeFastSnapshotRestores =
  res
    "DescribeFastSnapshotRestoresResponse"
    "fixture/DescribeFastSnapshotRestoresResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeFastSnapshotRestores)

responseDescribeTrafficMirrorFilters :: DescribeTrafficMirrorFiltersResponse -> TestTree
responseDescribeTrafficMirrorFilters =
  res
    "DescribeTrafficMirrorFiltersResponse"
    "fixture/DescribeTrafficMirrorFiltersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTrafficMirrorFilters)

responseCreateTransitGatewayPrefixListReference :: CreateTransitGatewayPrefixListReferenceResponse -> TestTree
responseCreateTransitGatewayPrefixListReference =
  res
    "CreateTransitGatewayPrefixListReferenceResponse"
    "fixture/CreateTransitGatewayPrefixListReferenceResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTransitGatewayPrefixListReference)

responseDeleteNetworkInterface :: DeleteNetworkInterfaceResponse -> TestTree
responseDeleteNetworkInterface =
  res
    "DeleteNetworkInterfaceResponse"
    "fixture/DeleteNetworkInterfaceResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteNetworkInterface)

responseCreateTransitGatewayRoute :: CreateTransitGatewayRouteResponse -> TestTree
responseCreateTransitGatewayRoute =
  res
    "CreateTransitGatewayRouteResponse"
    "fixture/CreateTransitGatewayRouteResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTransitGatewayRoute)

responseDeregisterTransitGatewayMulticastGroupSources :: DeregisterTransitGatewayMulticastGroupSourcesResponse -> TestTree
responseDeregisterTransitGatewayMulticastGroupSources =
  res
    "DeregisterTransitGatewayMulticastGroupSourcesResponse"
    "fixture/DeregisterTransitGatewayMulticastGroupSourcesResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterTransitGatewayMulticastGroupSources)

responseDisassociateVpcCidrBlock :: DisassociateVpcCidrBlockResponse -> TestTree
responseDisassociateVpcCidrBlock =
  res
    "DisassociateVpcCidrBlockResponse"
    "fixture/DisassociateVpcCidrBlockResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateVpcCidrBlock)

responseDescribeTransitGatewayPeeringAttachments :: DescribeTransitGatewayPeeringAttachmentsResponse -> TestTree
responseDescribeTransitGatewayPeeringAttachments =
  res
    "DescribeTransitGatewayPeeringAttachmentsResponse"
    "fixture/DescribeTransitGatewayPeeringAttachmentsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTransitGatewayPeeringAttachments)

responseGetCoipPoolUsage :: GetCoipPoolUsageResponse -> TestTree
responseGetCoipPoolUsage =
  res
    "GetCoipPoolUsageResponse"
    "fixture/GetCoipPoolUsageResponse.proto"
    defaultService
    (Proxy :: Proxy GetCoipPoolUsage)

responseImportImage :: ImportImageResponse -> TestTree
responseImportImage =
  res
    "ImportImageResponse"
    "fixture/ImportImageResponse.proto"
    defaultService
    (Proxy :: Proxy ImportImage)

responseReplaceTransitGatewayRoute :: ReplaceTransitGatewayRouteResponse -> TestTree
responseReplaceTransitGatewayRoute =
  res
    "ReplaceTransitGatewayRouteResponse"
    "fixture/ReplaceTransitGatewayRouteResponse.proto"
    defaultService
    (Proxy :: Proxy ReplaceTransitGatewayRoute)

responseCreatePlacementGroup :: CreatePlacementGroupResponse -> TestTree
responseCreatePlacementGroup =
  res
    "CreatePlacementGroupResponse"
    "fixture/CreatePlacementGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePlacementGroup)

responseCreateDefaultVpc :: CreateDefaultVpcResponse -> TestTree
responseCreateDefaultVpc =
  res
    "CreateDefaultVpcResponse"
    "fixture/CreateDefaultVpcResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDefaultVpc)

responseCreateNetworkInsightsPath :: CreateNetworkInsightsPathResponse -> TestTree
responseCreateNetworkInsightsPath =
  res
    "CreateNetworkInsightsPathResponse"
    "fixture/CreateNetworkInsightsPathResponse.proto"
    defaultService
    (Proxy :: Proxy CreateNetworkInsightsPath)

responseModifyTrafficMirrorSession :: ModifyTrafficMirrorSessionResponse -> TestTree
responseModifyTrafficMirrorSession =
  res
    "ModifyTrafficMirrorSessionResponse"
    "fixture/ModifyTrafficMirrorSessionResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyTrafficMirrorSession)

responseRunScheduledInstances :: RunScheduledInstancesResponse -> TestTree
responseRunScheduledInstances =
  res
    "RunScheduledInstancesResponse"
    "fixture/RunScheduledInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy RunScheduledInstances)

responseDescribeDhcpOptions :: DescribeDhcpOptionsResponse -> TestTree
responseDescribeDhcpOptions =
  res
    "DescribeDhcpOptionsResponse"
    "fixture/DescribeDhcpOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDhcpOptions)

responseDescribeCapacityReservations :: DescribeCapacityReservationsResponse -> TestTree
responseDescribeCapacityReservations =
  res
    "DescribeCapacityReservationsResponse"
    "fixture/DescribeCapacityReservationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCapacityReservations)

responseDescribeCustomerGateways :: DescribeCustomerGatewaysResponse -> TestTree
responseDescribeCustomerGateways =
  res
    "DescribeCustomerGatewaysResponse"
    "fixture/DescribeCustomerGatewaysResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCustomerGateways)

responseDeleteNatGateway :: DeleteNatGatewayResponse -> TestTree
responseDeleteNatGateway =
  res
    "DeleteNatGatewayResponse"
    "fixture/DeleteNatGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteNatGateway)

responseDescribeClientVpnAuthorizationRules :: DescribeClientVpnAuthorizationRulesResponse -> TestTree
responseDescribeClientVpnAuthorizationRules =
  res
    "DescribeClientVpnAuthorizationRulesResponse"
    "fixture/DescribeClientVpnAuthorizationRulesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeClientVpnAuthorizationRules)

responseStopInstances :: StopInstancesResponse -> TestTree
responseStopInstances =
  res
    "StopInstancesResponse"
    "fixture/StopInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy StopInstances)

responseReplaceRouteTableAssociation :: ReplaceRouteTableAssociationResponse -> TestTree
responseReplaceRouteTableAssociation =
  res
    "ReplaceRouteTableAssociationResponse"
    "fixture/ReplaceRouteTableAssociationResponse.proto"
    defaultService
    (Proxy :: Proxy ReplaceRouteTableAssociation)

responseDeleteTransitGatewayMulticastDomain :: DeleteTransitGatewayMulticastDomainResponse -> TestTree
responseDeleteTransitGatewayMulticastDomain =
  res
    "DeleteTransitGatewayMulticastDomainResponse"
    "fixture/DeleteTransitGatewayMulticastDomainResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTransitGatewayMulticastDomain)

responseDeleteSubnet :: DeleteSubnetResponse -> TestTree
responseDeleteSubnet =
  res
    "DeleteSubnetResponse"
    "fixture/DeleteSubnetResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSubnet)
