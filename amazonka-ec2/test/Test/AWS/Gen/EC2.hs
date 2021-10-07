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
--         [ requestDescribeVpcPeeringConnections $
--             newDescribeVpcPeeringConnections
--
--         , requestAssociateTrunkInterface $
--             newAssociateTrunkInterface
--
--         , requestDeleteTransitGatewayRoute $
--             newDeleteTransitGatewayRoute
--
--         , requestDescribeExportTasks $
--             newDescribeExportTasks
--
--         , requestDeleteLocalGatewayRouteTableVpcAssociation $
--             newDeleteLocalGatewayRouteTableVpcAssociation
--
--         , requestDeleteVpcEndpointConnectionNotifications $
--             newDeleteVpcEndpointConnectionNotifications
--
--         , requestCreateTransitGatewayMulticastDomain $
--             newCreateTransitGatewayMulticastDomain
--
--         , requestCreateVpcEndpointServiceConfiguration $
--             newCreateVpcEndpointServiceConfiguration
--
--         , requestDescribeByoipCidrs $
--             newDescribeByoipCidrs
--
--         , requestDetachVolume $
--             newDetachVolume
--
--         , requestDeleteNetworkInsightsPath $
--             newDeleteNetworkInsightsPath
--
--         , requestCancelBundleTask $
--             newCancelBundleTask
--
--         , requestGetCapacityReservationUsage $
--             newGetCapacityReservationUsage
--
--         , requestAssociateTransitGatewayMulticastDomain $
--             newAssociateTransitGatewayMulticastDomain
--
--         , requestCreateTransitGatewayConnectPeer $
--             newCreateTransitGatewayConnectPeer
--
--         , requestReleaseAddress $
--             newReleaseAddress
--
--         , requestModifyVpcTenancy $
--             newModifyVpcTenancy
--
--         , requestDescribeLocalGatewayVirtualInterfaces $
--             newDescribeLocalGatewayVirtualInterfaces
--
--         , requestGetHostReservationPurchasePreview $
--             newGetHostReservationPurchasePreview
--
--         , requestAuthorizeSecurityGroupEgress $
--             newAuthorizeSecurityGroupEgress
--
--         , requestAcceptReservedInstancesExchangeQuote $
--             newAcceptReservedInstancesExchangeQuote
--
--         , requestModifyManagedPrefixList $
--             newModifyManagedPrefixList
--
--         , requestDescribeInstanceCreditSpecifications $
--             newDescribeInstanceCreditSpecifications
--
--         , requestGetTransitGatewayMulticastDomainAssociations $
--             newGetTransitGatewayMulticastDomainAssociations
--
--         , requestDescribeInstances $
--             newDescribeInstances
--
--         , requestDisableEbsEncryptionByDefault $
--             newDisableEbsEncryptionByDefault
--
--         , requestDeregisterInstanceEventNotificationAttributes $
--             newDeregisterInstanceEventNotificationAttributes
--
--         , requestCreateTransitGatewayVpcAttachment $
--             newCreateTransitGatewayVpcAttachment
--
--         , requestDeregisterTransitGatewayMulticastGroupMembers $
--             newDeregisterTransitGatewayMulticastGroupMembers
--
--         , requestDeleteTransitGatewayPrefixListReference $
--             newDeleteTransitGatewayPrefixListReference
--
--         , requestCreateTransitGatewayRouteTable $
--             newCreateTransitGatewayRouteTable
--
--         , requestDisassociateAddress $
--             newDisassociateAddress
--
--         , requestDetachNetworkInterface $
--             newDetachNetworkInterface
--
--         , requestDeleteFleets $
--             newDeleteFleets
--
--         , requestDeleteVpc $
--             newDeleteVpc
--
--         , requestAssociateVpcCidrBlock $
--             newAssociateVpcCidrBlock
--
--         , requestCreateNetworkAcl $
--             newCreateNetworkAcl
--
--         , requestDeleteTrafficMirrorTarget $
--             newDeleteTrafficMirrorTarget
--
--         , requestDeleteLaunchTemplate $
--             newDeleteLaunchTemplate
--
--         , requestModifySecurityGroupRules $
--             newModifySecurityGroupRules
--
--         , requestDeleteVpcEndpoints $
--             newDeleteVpcEndpoints
--
--         , requestDescribeTrafficMirrorSessions $
--             newDescribeTrafficMirrorSessions
--
--         , requestUpdateSecurityGroupRuleDescriptionsIngress $
--             newUpdateSecurityGroupRuleDescriptionsIngress
--
--         , requestDescribePrefixLists $
--             newDescribePrefixLists
--
--         , requestDescribeVpcClassicLink $
--             newDescribeVpcClassicLink
--
--         , requestGetAssociatedIpv6PoolCidrs $
--             newGetAssociatedIpv6PoolCidrs
--
--         , requestImportInstance $
--             newImportInstance
--
--         , requestCreateDefaultSubnet $
--             newCreateDefaultSubnet
--
--         , requestDeleteFlowLogs $
--             newDeleteFlowLogs
--
--         , requestModifyVolumeAttribute $
--             newModifyVolumeAttribute
--
--         , requestCreateNetworkInterfacePermission $
--             newCreateNetworkInterfacePermission
--
--         , requestDescribeScheduledInstanceAvailability $
--             newDescribeScheduledInstanceAvailability
--
--         , requestDescribeClientVpnEndpoints $
--             newDescribeClientVpnEndpoints
--
--         , requestRejectVpcEndpointConnections $
--             newRejectVpcEndpointConnections
--
--         , requestDescribeTags $
--             newDescribeTags
--
--         , requestAssociateTransitGatewayRouteTable $
--             newAssociateTransitGatewayRouteTable
--
--         , requestDescribeLocalGatewayRouteTables $
--             newDescribeLocalGatewayRouteTables
--
--         , requestRejectTransitGatewayVpcAttachment $
--             newRejectTransitGatewayVpcAttachment
--
--         , requestCreateVpnGateway $
--             newCreateVpnGateway
--
--         , requestAcceptTransitGatewayPeeringAttachment $
--             newAcceptTransitGatewayPeeringAttachment
--
--         , requestGetTransitGatewayRouteTableAssociations $
--             newGetTransitGatewayRouteTableAssociations
--
--         , requestDeleteVpnGateway $
--             newDeleteVpnGateway
--
--         , requestDescribeImportImageTasks $
--             newDescribeImportImageTasks
--
--         , requestExportTransitGatewayRoutes $
--             newExportTransitGatewayRoutes
--
--         , requestDescribeLaunchTemplateVersions $
--             newDescribeLaunchTemplateVersions
--
--         , requestDescribeFpgaImages $
--             newDescribeFpgaImages
--
--         , requestGetReservedInstancesExchangeQuote $
--             newGetReservedInstancesExchangeQuote
--
--         , requestCreateKeyPair $
--             newCreateKeyPair
--
--         , requestDescribeElasticGpus $
--             newDescribeElasticGpus
--
--         , requestStartNetworkInsightsAnalysis $
--             newStartNetworkInsightsAnalysis
--
--         , requestDescribeSecurityGroupRules $
--             newDescribeSecurityGroupRules
--
--         , requestDeleteNetworkAcl $
--             newDeleteNetworkAcl
--
--         , requestDescribeRouteTables $
--             newDescribeRouteTables
--
--         , requestDescribeFleetInstances $
--             newDescribeFleetInstances
--
--         , requestDeleteTransitGatewayRouteTable $
--             newDeleteTransitGatewayRouteTable
--
--         , requestCreateLaunchTemplate $
--             newCreateLaunchTemplate
--
--         , requestModifyInstanceEventWindow $
--             newModifyInstanceEventWindow
--
--         , requestMoveAddressToVpc $
--             newMoveAddressToVpc
--
--         , requestAcceptTransitGatewayMulticastDomainAssociations $
--             newAcceptTransitGatewayMulticastDomainAssociations
--
--         , requestRestoreAddressToClassic $
--             newRestoreAddressToClassic
--
--         , requestDescribeAvailabilityZones $
--             newDescribeAvailabilityZones
--
--         , requestCreateStoreImageTask $
--             newCreateStoreImageTask
--
--         , requestCopySnapshot $
--             newCopySnapshot
--
--         , requestDeleteNetworkInterfacePermission $
--             newDeleteNetworkInterfacePermission
--
--         , requestCreateFlowLogs $
--             newCreateFlowLogs
--
--         , requestDetachClassicLinkVpc $
--             newDetachClassicLinkVpc
--
--         , requestDeleteRouteTable $
--             newDeleteRouteTable
--
--         , requestModifyVpnConnectionOptions $
--             newModifyVpnConnectionOptions
--
--         , requestMonitorInstances $
--             newMonitorInstances
--
--         , requestModifyIdFormat $
--             newModifyIdFormat
--
--         , requestAllocateHosts $
--             newAllocateHosts
--
--         , requestDescribeImageAttribute $
--             newDescribeImageAttribute
--
--         , requestDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations $
--             newDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
--
--         , requestDescribeReservedInstancesModifications $
--             newDescribeReservedInstancesModifications
--
--         , requestStartVpcEndpointServicePrivateDnsVerification $
--             newStartVpcEndpointServicePrivateDnsVerification
--
--         , requestCreateTrafficMirrorFilterRule $
--             newCreateTrafficMirrorFilterRule
--
--         , requestGetEbsDefaultKmsKeyId $
--             newGetEbsDefaultKmsKeyId
--
--         , requestDescribeClientVpnRoutes $
--             newDescribeClientVpnRoutes
--
--         , requestModifyVpnConnection $
--             newModifyVpnConnection
--
--         , requestModifyFleet $
--             newModifyFleet
--
--         , requestRegisterImage $
--             newRegisterImage
--
--         , requestRevokeClientVpnIngress $
--             newRevokeClientVpnIngress
--
--         , requestUpdateSecurityGroupRuleDescriptionsEgress $
--             newUpdateSecurityGroupRuleDescriptionsEgress
--
--         , requestModifyVpcEndpoint $
--             newModifyVpcEndpoint
--
--         , requestUnassignPrivateIpAddresses $
--             newUnassignPrivateIpAddresses
--
--         , requestEnableFastSnapshotRestores $
--             newEnableFastSnapshotRestores
--
--         , requestCancelImportTask $
--             newCancelImportTask
--
--         , requestDescribeVolumes $
--             newDescribeVolumes
--
--         , requestCreateClientVpnEndpoint $
--             newCreateClientVpnEndpoint
--
--         , requestResetFpgaImageAttribute $
--             newResetFpgaImageAttribute
--
--         , requestGetConsoleOutput $
--             newGetConsoleOutput
--
--         , requestDeleteFpgaImage $
--             newDeleteFpgaImage
--
--         , requestModifyReservedInstances $
--             newModifyReservedInstances
--
--         , requestCreateRestoreImageTask $
--             newCreateRestoreImageTask
--
--         , requestDescribeSpotInstanceRequests $
--             newDescribeSpotInstanceRequests
--
--         , requestModifyVpcEndpointServicePermissions $
--             newModifyVpcEndpointServicePermissions
--
--         , requestUnassignIpv6Addresses $
--             newUnassignIpv6Addresses
--
--         , requestDescribeVolumesModifications $
--             newDescribeVolumesModifications
--
--         , requestDescribeIdFormat $
--             newDescribeIdFormat
--
--         , requestReportInstanceStatus $
--             newReportInstanceStatus
--
--         , requestRunInstances $
--             newRunInstances
--
--         , requestModifyHosts $
--             newModifyHosts
--
--         , requestAttachVolume $
--             newAttachVolume
--
--         , requestDescribeStoreImageTasks $
--             newDescribeStoreImageTasks
--
--         , requestCreateReplaceRootVolumeTask $
--             newCreateReplaceRootVolumeTask
--
--         , requestDescribeAccountAttributes $
--             newDescribeAccountAttributes
--
--         , requestModifyImageAttribute $
--             newModifyImageAttribute
--
--         , requestRegisterTransitGatewayMulticastGroupSources $
--             newRegisterTransitGatewayMulticastGroupSources
--
--         , requestRebootInstances $
--             newRebootInstances
--
--         , requestAssociateRouteTable $
--             newAssociateRouteTable
--
--         , requestAssociateIamInstanceProfile $
--             newAssociateIamInstanceProfile
--
--         , requestPurchaseHostReservation $
--             newPurchaseHostReservation
--
--         , requestDescribeTrafficMirrorTargets $
--             newDescribeTrafficMirrorTargets
--
--         , requestGetManagedPrefixListAssociations $
--             newGetManagedPrefixListAssociations
--
--         , requestCancelConversionTask $
--             newCancelConversionTask
--
--         , requestModifyVpcEndpointServiceConfiguration $
--             newModifyVpcEndpointServiceConfiguration
--
--         , requestCreateDhcpOptions $
--             newCreateDhcpOptions
--
--         , requestCreateManagedPrefixList $
--             newCreateManagedPrefixList
--
--         , requestSearchTransitGatewayRoutes $
--             newSearchTransitGatewayRoutes
--
--         , requestDescribeVpcs $
--             newDescribeVpcs
--
--         , requestDescribeLocalGateways $
--             newDescribeLocalGateways
--
--         , requestDescribeIpv6Pools $
--             newDescribeIpv6Pools
--
--         , requestCreateRouteTable $
--             newCreateRouteTable
--
--         , requestDescribeVpcEndpointConnectionNotifications $
--             newDescribeVpcEndpointConnectionNotifications
--
--         , requestGetTransitGatewayPrefixListReferences $
--             newGetTransitGatewayPrefixListReferences
--
--         , requestAcceptVpcEndpointConnections $
--             newAcceptVpcEndpointConnections
--
--         , requestGetTransitGatewayRouteTablePropagations $
--             newGetTransitGatewayRouteTablePropagations
--
--         , requestAssociateDhcpOptions $
--             newAssociateDhcpOptions
--
--         , requestDeleteEgressOnlyInternetGateway $
--             newDeleteEgressOnlyInternetGateway
--
--         , requestGetVpnConnectionDeviceTypes $
--             newGetVpnConnectionDeviceTypes
--
--         , requestCreateSubnetCidrReservation $
--             newCreateSubnetCidrReservation
--
--         , requestDisableFastSnapshotRestores $
--             newDisableFastSnapshotRestores
--
--         , requestRequestSpotInstances $
--             newRequestSpotInstances
--
--         , requestDescribeLaunchTemplates $
--             newDescribeLaunchTemplates
--
--         , requestCreateImage $
--             newCreateImage
--
--         , requestModifyTransitGatewayVpcAttachment $
--             newModifyTransitGatewayVpcAttachment
--
--         , requestAssignIpv6Addresses $
--             newAssignIpv6Addresses
--
--         , requestDescribeLocalGatewayVirtualInterfaceGroups $
--             newDescribeLocalGatewayVirtualInterfaceGroups
--
--         , requestDescribeVpnConnections $
--             newDescribeVpnConnections
--
--         , requestCreateNetworkAclEntry $
--             newCreateNetworkAclEntry
--
--         , requestDescribePlacementGroups $
--             newDescribePlacementGroups
--
--         , requestModifySnapshotAttribute $
--             newModifySnapshotAttribute
--
--         , requestModifyIdentityIdFormat $
--             newModifyIdentityIdFormat
--
--         , requestEnableVgwRoutePropagation $
--             newEnableVgwRoutePropagation
--
--         , requestDeleteTags $
--             newDeleteTags
--
--         , requestResetInstanceAttribute $
--             newResetInstanceAttribute
--
--         , requestDisassociateEnclaveCertificateIamRole $
--             newDisassociateEnclaveCertificateIamRole
--
--         , requestDeleteVpcPeeringConnection $
--             newDeleteVpcPeeringConnection
--
--         , requestDescribeFlowLogs $
--             newDescribeFlowLogs
--
--         , requestDescribeReservedInstancesOfferings $
--             newDescribeReservedInstancesOfferings
--
--         , requestDescribeFleets $
--             newDescribeFleets
--
--         , requestAttachNetworkInterface $
--             newAttachNetworkInterface
--
--         , requestConfirmProductInstance $
--             newConfirmProductInstance
--
--         , requestDescribeTransitGatewayAttachments $
--             newDescribeTransitGatewayAttachments
--
--         , requestModifyAvailabilityZoneGroup $
--             newModifyAvailabilityZoneGroup
--
--         , requestReplaceNetworkAclEntry $
--             newReplaceNetworkAclEntry
--
--         , requestDescribeSpotFleetRequestHistory $
--             newDescribeSpotFleetRequestHistory
--
--         , requestCreateLocalGatewayRoute $
--             newCreateLocalGatewayRoute
--
--         , requestDescribeVpcEndpoints $
--             newDescribeVpcEndpoints
--
--         , requestModifyInstanceEventStartTime $
--             newModifyInstanceEventStartTime
--
--         , requestDescribeStaleSecurityGroups $
--             newDescribeStaleSecurityGroups
--
--         , requestDescribeInstanceStatus $
--             newDescribeInstanceStatus
--
--         , requestDeleteNetworkAclEntry $
--             newDeleteNetworkAclEntry
--
--         , requestGetConsoleScreenshot $
--             newGetConsoleScreenshot
--
--         , requestGetGroupsForCapacityReservation $
--             newGetGroupsForCapacityReservation
--
--         , requestDisassociateIamInstanceProfile $
--             newDisassociateIamInstanceProfile
--
--         , requestDescribeVpcEndpointServiceConfigurations $
--             newDescribeVpcEndpointServiceConfigurations
--
--         , requestCancelSpotInstanceRequests $
--             newCancelSpotInstanceRequests
--
--         , requestDeleteLocalGatewayRoute $
--             newDeleteLocalGatewayRoute
--
--         , requestDescribeVpcEndpointServices $
--             newDescribeVpcEndpointServices
--
--         , requestDisassociateRouteTable $
--             newDisassociateRouteTable
--
--         , requestAssignPrivateIpAddresses $
--             newAssignPrivateIpAddresses
--
--         , requestGetFlowLogsIntegrationTemplate $
--             newGetFlowLogsIntegrationTemplate
--
--         , requestModifyVpnTunnelCertificate $
--             newModifyVpnTunnelCertificate
--
--         , requestDisableVgwRoutePropagation $
--             newDisableVgwRoutePropagation
--
--         , requestDeleteSnapshot $
--             newDeleteSnapshot
--
--         , requestDescribeSubnets $
--             newDescribeSubnets
--
--         , requestCreateSpotDatafeedSubscription $
--             newCreateSpotDatafeedSubscription
--
--         , requestUnmonitorInstances $
--             newUnmonitorInstances
--
--         , requestImportVolume $
--             newImportVolume
--
--         , requestDescribeAddresses $
--             newDescribeAddresses
--
--         , requestPurchaseReservedInstancesOffering $
--             newPurchaseReservedInstancesOffering
--
--         , requestDescribeSnapshotAttribute $
--             newDescribeSnapshotAttribute
--
--         , requestAuthorizeSecurityGroupIngress $
--             newAuthorizeSecurityGroupIngress
--
--         , requestDescribeNatGateways $
--             newDescribeNatGateways
--
--         , requestDisableVpcClassicLink $
--             newDisableVpcClassicLink
--
--         , requestDescribeTransitGatewayMulticastDomains $
--             newDescribeTransitGatewayMulticastDomains
--
--         , requestGetTransitGatewayAttachmentPropagations $
--             newGetTransitGatewayAttachmentPropagations
--
--         , requestModifyVpcEndpointConnectionNotification $
--             newModifyVpcEndpointConnectionNotification
--
--         , requestRestoreManagedPrefixListVersion $
--             newRestoreManagedPrefixListVersion
--
--         , requestDescribeTransitGatewayConnectPeers $
--             newDescribeTransitGatewayConnectPeers
--
--         , requestDeleteCarrierGateway $
--             newDeleteCarrierGateway
--
--         , requestDescribeNetworkInterfaces $
--             newDescribeNetworkInterfaces
--
--         , requestDescribeTransitGatewayVpcAttachments $
--             newDescribeTransitGatewayVpcAttachments
--
--         , requestModifyAddressAttribute $
--             newModifyAddressAttribute
--
--         , requestDescribeImportSnapshotTasks $
--             newDescribeImportSnapshotTasks
--
--         , requestCopyImage $
--             newCopyImage
--
--         , requestDescribeInstanceEventNotificationAttributes $
--             newDescribeInstanceEventNotificationAttributes
--
--         , requestEnableSerialConsoleAccess $
--             newEnableSerialConsoleAccess
--
--         , requestModifyTrafficMirrorFilterRule $
--             newModifyTrafficMirrorFilterRule
--
--         , requestDescribeCarrierGateways $
--             newDescribeCarrierGateways
--
--         , requestDeleteInternetGateway $
--             newDeleteInternetGateway
--
--         , requestModifyInstanceCapacityReservationAttributes $
--             newModifyInstanceCapacityReservationAttributes
--
--         , requestDescribeNetworkInterfaceAttribute $
--             newDescribeNetworkInterfaceAttribute
--
--         , requestAttachClassicLinkVpc $
--             newAttachClassicLinkVpc
--
--         , requestGetSubnetCidrReservations $
--             newGetSubnetCidrReservations
--
--         , requestAssociateClientVpnTargetNetwork $
--             newAssociateClientVpnTargetNetwork
--
--         , requestCancelCapacityReservation $
--             newCancelCapacityReservation
--
--         , requestDisableTransitGatewayRouteTablePropagation $
--             newDisableTransitGatewayRouteTablePropagation
--
--         , requestCancelReservedInstancesListing $
--             newCancelReservedInstancesListing
--
--         , requestDeleteQueuedReservedInstances $
--             newDeleteQueuedReservedInstances
--
--         , requestCreateFleet $
--             newCreateFleet
--
--         , requestDescribeSnapshots $
--             newDescribeSnapshots
--
--         , requestDescribeReservedInstancesListings $
--             newDescribeReservedInstancesListings
--
--         , requestModifyClientVpnEndpoint $
--             newModifyClientVpnEndpoint
--
--         , requestCreateVpcEndpoint $
--             newCreateVpcEndpoint
--
--         , requestCreateVpnConnection $
--             newCreateVpnConnection
--
--         , requestImportClientVpnClientCertificateRevocationList $
--             newImportClientVpnClientCertificateRevocationList
--
--         , requestAssociateSubnetCidrBlock $
--             newAssociateSubnetCidrBlock
--
--         , requestDescribeSpotPriceHistory $
--             newDescribeSpotPriceHistory
--
--         , requestEnableEbsEncryptionByDefault $
--             newEnableEbsEncryptionByDefault
--
--         , requestDescribeVpcClassicLinkDnsSupport $
--             newDescribeVpcClassicLinkDnsSupport
--
--         , requestCreateLocalGatewayRouteTableVpcAssociation $
--             newCreateLocalGatewayRouteTableVpcAssociation
--
--         , requestDescribeAggregateIdFormat $
--             newDescribeAggregateIdFormat
--
--         , requestEnableTransitGatewayRouteTablePropagation $
--             newEnableTransitGatewayRouteTablePropagation
--
--         , requestRequestSpotFleet $
--             newRequestSpotFleet
--
--         , requestDescribeBundleTasks $
--             newDescribeBundleTasks
--
--         , requestModifyNetworkInterfaceAttribute $
--             newModifyNetworkInterfaceAttribute
--
--         , requestDisableSerialConsoleAccess $
--             newDisableSerialConsoleAccess
--
--         , requestDescribeInstanceTypeOfferings $
--             newDescribeInstanceTypeOfferings
--
--         , requestModifySpotFleetRequest $
--             newModifySpotFleetRequest
--
--         , requestDeregisterImage $
--             newDeregisterImage
--
--         , requestResetAddressAttribute $
--             newResetAddressAttribute
--
--         , requestDescribeCoipPools $
--             newDescribeCoipPools
--
--         , requestDescribeInstanceEventWindows $
--             newDescribeInstanceEventWindows
--
--         , requestCreateSubnet $
--             newCreateSubnet
--
--         , requestDescribeSecurityGroups $
--             newDescribeSecurityGroups
--
--         , requestDeletePlacementGroup $
--             newDeletePlacementGroup
--
--         , requestEnableVolumeIO $
--             newEnableVolumeIO
--
--         , requestCreateNatGateway $
--             newCreateNatGateway
--
--         , requestGetAssociatedEnclaveCertificateIamRoles $
--             newGetAssociatedEnclaveCertificateIamRoles
--
--         , requestModifySubnetAttribute $
--             newModifySubnetAttribute
--
--         , requestDescribeTransitGatewayConnects $
--             newDescribeTransitGatewayConnects
--
--         , requestDetachVpnGateway $
--             newDetachVpnGateway
--
--         , requestCreateNetworkInterface $
--             newCreateNetworkInterface
--
--         , requestDeleteVpnConnection $
--             newDeleteVpnConnection
--
--         , requestDescribeInstanceTypes $
--             newDescribeInstanceTypes
--
--         , requestCancelExportTask $
--             newCancelExportTask
--
--         , requestModifyTransitGateway $
--             newModifyTransitGateway
--
--         , requestCreateInternetGateway $
--             newCreateInternetGateway
--
--         , requestSendDiagnosticInterrupt $
--             newSendDiagnosticInterrupt
--
--         , requestDisassociateClientVpnTargetNetwork $
--             newDisassociateClientVpnTargetNetwork
--
--         , requestModifyInstanceMetadataOptions $
--             newModifyInstanceMetadataOptions
--
--         , requestDescribeSpotDatafeedSubscription $
--             newDescribeSpotDatafeedSubscription
--
--         , requestExportClientVpnClientConfiguration $
--             newExportClientVpnClientConfiguration
--
--         , requestDeleteKeyPair $
--             newDeleteKeyPair
--
--         , requestDescribeEgressOnlyInternetGateways $
--             newDescribeEgressOnlyInternetGateways
--
--         , requestCreateVolume $
--             newCreateVolume
--
--         , requestModifyTrafficMirrorFilterNetworkServices $
--             newModifyTrafficMirrorFilterNetworkServices
--
--         , requestDescribeVpcAttribute $
--             newDescribeVpcAttribute
--
--         , requestDescribeTrunkInterfaceAssociations $
--             newDescribeTrunkInterfaceAssociations
--
--         , requestCreateInstanceExportTask $
--             newCreateInstanceExportTask
--
--         , requestCreateClientVpnRoute $
--             newCreateClientVpnRoute
--
--         , requestModifyCapacityReservation $
--             newModifyCapacityReservation
--
--         , requestRevokeSecurityGroupEgress $
--             newRevokeSecurityGroupEgress
--
--         , requestDescribeSecurityGroupReferences $
--             newDescribeSecurityGroupReferences
--
--         , requestDisassociateSubnetCidrBlock $
--             newDisassociateSubnetCidrBlock
--
--         , requestDetachInternetGateway $
--             newDetachInternetGateway
--
--         , requestSearchTransitGatewayMulticastGroups $
--             newSearchTransitGatewayMulticastGroups
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
--         , requestModifyVpcAttribute $
--             newModifyVpcAttribute
--
--         , requestDeleteClientVpnRoute $
--             newDeleteClientVpnRoute
--
--         , requestDeprovisionByoipCidr $
--             newDeprovisionByoipCidr
--
--         , requestDisassociateTrunkInterface $
--             newDisassociateTrunkInterface
--
--         , requestImportSnapshot $
--             newImportSnapshot
--
--         , requestDescribeSpotFleetInstances $
--             newDescribeSpotFleetInstances
--
--         , requestDescribeClientVpnConnections $
--             newDescribeClientVpnConnections
--
--         , requestCreateTrafficMirrorTarget $
--             newCreateTrafficMirrorTarget
--
--         , requestModifyInstanceCreditSpecification $
--             newModifyInstanceCreditSpecification
--
--         , requestAcceptVpcPeeringConnection $
--             newAcceptVpcPeeringConnection
--
--         , requestDescribeVolumeAttribute $
--             newDescribeVolumeAttribute
--
--         , requestDescribeSpotFleetRequests $
--             newDescribeSpotFleetRequests
--
--         , requestDescribeAddressesAttribute $
--             newDescribeAddressesAttribute
--
--         , requestDescribePrincipalIdFormat $
--             newDescribePrincipalIdFormat
--
--         , requestDescribeTransitGateways $
--             newDescribeTransitGateways
--
--         , requestModifyVpcPeeringConnectionOptions $
--             newModifyVpcPeeringConnectionOptions
--
--         , requestCreateVpcEndpointConnectionNotification $
--             newCreateVpcEndpointConnectionNotification
--
--         , requestDeleteVpcEndpointServiceConfigurations $
--             newDeleteVpcEndpointServiceConfigurations
--
--         , requestDescribeFleetHistory $
--             newDescribeFleetHistory
--
--         , requestCreateVpc $
--             newCreateVpc
--
--         , requestDescribeVolumeStatus $
--             newDescribeVolumeStatus
--
--         , requestGetSerialConsoleAccessStatus $
--             newGetSerialConsoleAccessStatus
--
--         , requestDescribeReplaceRootVolumeTasks $
--             newDescribeReplaceRootVolumeTasks
--
--         , requestDescribeImages $
--             newDescribeImages
--
--         , requestDeleteVolume $
--             newDeleteVolume
--
--         , requestSearchLocalGatewayRoutes $
--             newSearchLocalGatewayRoutes
--
--         , requestDescribeMovingAddresses $
--             newDescribeMovingAddresses
--
--         , requestCreateTrafficMirrorSession $
--             newCreateTrafficMirrorSession
--
--         , requestDescribeScheduledInstances $
--             newDescribeScheduledInstances
--
--         , requestAssociateEnclaveCertificateIamRole $
--             newAssociateEnclaveCertificateIamRole
--
--         , requestRejectTransitGatewayMulticastDomainAssociations $
--             newRejectTransitGatewayMulticastDomainAssociations
--
--         , requestDeleteTransitGateway $
--             newDeleteTransitGateway
--
--         , requestDescribeHosts $
--             newDescribeHosts
--
--         , requestDescribeNetworkInterfacePermissions $
--             newDescribeNetworkInterfacePermissions
--
--         , requestGetVpnConnectionDeviceSampleConfiguration $
--             newGetVpnConnectionDeviceSampleConfiguration
--
--         , requestDescribeVpcEndpointServicePermissions $
--             newDescribeVpcEndpointServicePermissions
--
--         , requestDescribeHostReservationOfferings $
--             newDescribeHostReservationOfferings
--
--         , requestDescribeVpcEndpointConnections $
--             newDescribeVpcEndpointConnections
--
--         , requestDescribeFpgaImageAttribute $
--             newDescribeFpgaImageAttribute
--
--         , requestEnableImageDeprecation $
--             newEnableImageDeprecation
--
--         , requestResetImageAttribute $
--             newResetImageAttribute
--
--         , requestAdvertiseByoipCidr $
--             newAdvertiseByoipCidr
--
--         , requestDescribeTransitGatewayRouteTables $
--             newDescribeTransitGatewayRouteTables
--
--         , requestModifyTransitGatewayPrefixListReference $
--             newModifyTransitGatewayPrefixListReference
--
--         , requestRegisterInstanceEventNotificationAttributes $
--             newRegisterInstanceEventNotificationAttributes
--
--         , requestDeleteManagedPrefixList $
--             newDeleteManagedPrefixList
--
--         , requestDescribeRegions $
--             newDescribeRegions
--
--         , requestRevokeSecurityGroupIngress $
--             newRevokeSecurityGroupIngress
--
--         , requestDeleteVpnConnectionRoute $
--             newDeleteVpnConnectionRoute
--
--         , requestDescribeNetworkAcls $
--             newDescribeNetworkAcls
--
--         , requestDeleteDhcpOptions $
--             newDeleteDhcpOptions
--
--         , requestDescribeVpnGateways $
--             newDescribeVpnGateways
--
--         , requestRegisterTransitGatewayMulticastGroupMembers $
--             newRegisterTransitGatewayMulticastGroupMembers
--
--         , requestDescribeHostReservations $
--             newDescribeHostReservations
--
--         , requestRejectVpcPeeringConnection $
--             newRejectVpcPeeringConnection
--
--         , requestCreateEgressOnlyInternetGateway $
--             newCreateEgressOnlyInternetGateway
--
--         , requestDeleteSubnetCidrReservation $
--             newDeleteSubnetCidrReservation
--
--         , requestEnableVpcClassicLinkDnsSupport $
--             newEnableVpcClassicLinkDnsSupport
--
--         , requestAllocateAddress $
--             newAllocateAddress
--
--         , requestExportImage $
--             newExportImage
--
--         , requestRejectTransitGatewayPeeringAttachment $
--             newRejectTransitGatewayPeeringAttachment
--
--         , requestGetLaunchTemplateData $
--             newGetLaunchTemplateData
--
--         , requestDescribeReservedInstances $
--             newDescribeReservedInstances
--
--         , requestModifyDefaultCreditSpecification $
--             newModifyDefaultCreditSpecification
--
--         , requestGetManagedPrefixListEntries $
--             newGetManagedPrefixListEntries
--
--         , requestDisableVpcClassicLinkDnsSupport $
--             newDisableVpcClassicLinkDnsSupport
--
--         , requestDisableImageDeprecation $
--             newDisableImageDeprecation
--
--         , requestApplySecurityGroupsToClientVpnTargetNetwork $
--             newApplySecurityGroupsToClientVpnTargetNetwork
--
--         , requestCreateLaunchTemplateVersion $
--             newCreateLaunchTemplateVersion
--
--         , requestCreateVpnConnectionRoute $
--             newCreateVpnConnectionRoute
--
--         , requestDisassociateInstanceEventWindow $
--             newDisassociateInstanceEventWindow
--
--         , requestDescribeConversionTasks $
--             newDescribeConversionTasks
--
--         , requestDeleteTrafficMirrorSession $
--             newDeleteTrafficMirrorSession
--
--         , requestCreateTransitGateway $
--             newCreateTransitGateway
--
--         , requestCreateSnapshots $
--             newCreateSnapshots
--
--         , requestDeleteClientVpnEndpoint $
--             newDeleteClientVpnEndpoint
--
--         , requestExportClientVpnClientCertificateRevocationList $
--             newExportClientVpnClientCertificateRevocationList
--
--         , requestDescribeKeyPairs $
--             newDescribeKeyPairs
--
--         , requestCreateFpgaImage $
--             newCreateFpgaImage
--
--         , requestDescribeClassicLinkInstances $
--             newDescribeClassicLinkInstances
--
--         , requestDeleteTrafficMirrorFilterRule $
--             newDeleteTrafficMirrorFilterRule
--
--         , requestTerminateInstances $
--             newTerminateInstances
--
--         , requestModifyFpgaImageAttribute $
--             newModifyFpgaImageAttribute
--
--         , requestWithdrawByoipCidr $
--             newWithdrawByoipCidr
--
--         , requestAttachVpnGateway $
--             newAttachVpnGateway
--
--         , requestAcceptTransitGatewayVpcAttachment $
--             newAcceptTransitGatewayVpcAttachment
--
--         , requestDeleteSpotDatafeedSubscription $
--             newDeleteSpotDatafeedSubscription
--
--         , requestDescribeExportImageTasks $
--             newDescribeExportImageTasks
--
--         , requestCreateCarrierGateway $
--             newCreateCarrierGateway
--
--         , requestAttachInternetGateway $
--             newAttachInternetGateway
--
--         , requestDescribeClientVpnTargetNetworks $
--             newDescribeClientVpnTargetNetworks
--
--         , requestCreateTrafficMirrorFilter $
--             newCreateTrafficMirrorFilter
--
--         , requestDeleteNetworkInsightsAnalysis $
--             newDeleteNetworkInsightsAnalysis
--
--         , requestDescribeIamInstanceProfileAssociations $
--             newDescribeIamInstanceProfileAssociations
--
--         , requestImportKeyPair $
--             newImportKeyPair
--
--         , requestEnableVpcClassicLink $
--             newEnableVpcClassicLink
--
--         , requestDescribeNetworkInsightsAnalyses $
--             newDescribeNetworkInsightsAnalyses
--
--         , requestDeleteRoute $
--             newDeleteRoute
--
--         , requestDescribeNetworkInsightsPaths $
--             newDescribeNetworkInsightsPaths
--
--         , requestCreateCapacityReservation $
--             newCreateCapacityReservation
--
--         , requestDeleteInstanceEventWindow $
--             newDeleteInstanceEventWindow
--
--         , requestModifyEbsDefaultKmsKeyId $
--             newModifyEbsDefaultKmsKeyId
--
--         , requestProvisionByoipCidr $
--             newProvisionByoipCidr
--
--         , requestDeleteTransitGatewayConnect $
--             newDeleteTransitGatewayConnect
--
--         , requestModifyVpnTunnelOptions $
--             newModifyVpnTunnelOptions
--
--         , requestCreateTransitGatewayPeeringAttachment $
--             newCreateTransitGatewayPeeringAttachment
--
--         , requestCreateCustomerGateway $
--             newCreateCustomerGateway
--
--         , requestModifyVolume $
--             newModifyVolume
--
--         , requestModifyInstancePlacement $
--             newModifyInstancePlacement
--
--         , requestCreateSnapshot $
--             newCreateSnapshot
--
--         , requestDescribeInstanceAttribute $
--             newDescribeInstanceAttribute
--
--         , requestCreateReservedInstancesListing $
--             newCreateReservedInstancesListing
--
--         , requestDeleteSecurityGroup $
--             newDeleteSecurityGroup
--
--         , requestPurchaseScheduledInstances $
--             newPurchaseScheduledInstances
--
--         , requestDescribePublicIpv4Pools $
--             newDescribePublicIpv4Pools
--
--         , requestDescribeLocalGatewayRouteTableVpcAssociations $
--             newDescribeLocalGatewayRouteTableVpcAssociations
--
--         , requestAuthorizeClientVpnIngress $
--             newAuthorizeClientVpnIngress
--
--         , requestCreateVpcPeeringConnection $
--             newCreateVpcPeeringConnection
--
--         , requestCreateInstanceEventWindow $
--             newCreateInstanceEventWindow
--
--         , requestCreateSecurityGroup $
--             newCreateSecurityGroup
--
--         , requestDescribeInternetGateways $
--             newDescribeInternetGateways
--
--         , requestModifyLaunchTemplate $
--             newModifyLaunchTemplate
--
--         , requestModifyInstanceAttribute $
--             newModifyInstanceAttribute
--
--         , requestResetEbsDefaultKmsKeyId $
--             newResetEbsDefaultKmsKeyId
--
--         , requestGetEbsEncryptionByDefault $
--             newGetEbsEncryptionByDefault
--
--         , requestDeleteCustomerGateway $
--             newDeleteCustomerGateway
--
--         , requestTerminateClientVpnConnections $
--             newTerminateClientVpnConnections
--
--         , requestAssociateInstanceEventWindow $
--             newAssociateInstanceEventWindow
--
--         , requestReplaceRoute $
--             newReplaceRoute
--
--         , requestCreateTags $
--             newCreateTags
--
--         , requestCreateTransitGatewayConnect $
--             newCreateTransitGatewayConnect
--
--         , requestDeleteLaunchTemplateVersions $
--             newDeleteLaunchTemplateVersions
--
--         , requestGetDefaultCreditSpecification $
--             newGetDefaultCreditSpecification
--
--         , requestResetSnapshotAttribute $
--             newResetSnapshotAttribute
--
--         , requestDeleteTransitGatewayPeeringAttachment $
--             newDeleteTransitGatewayPeeringAttachment
--
--         , requestCreateRoute $
--             newCreateRoute
--
--         , requestBundleInstance $
--             newBundleInstance
--
--         , requestReplaceNetworkAclAssociation $
--             newReplaceNetworkAclAssociation
--
--         , requestDeleteTrafficMirrorFilter $
--             newDeleteTrafficMirrorFilter
--
--         , requestDescribeIdentityIdFormat $
--             newDescribeIdentityIdFormat
--
--         , requestReplaceTransitGatewayRoute $
--             newReplaceTransitGatewayRoute
--
--         , requestGetCoipPoolUsage $
--             newGetCoipPoolUsage
--
--         , requestDeleteTransitGatewayVpcAttachment $
--             newDeleteTransitGatewayVpcAttachment
--
--         , requestCreatePlacementGroup $
--             newCreatePlacementGroup
--
--         , requestReplaceIamInstanceProfileAssociation $
--             newReplaceIamInstanceProfileAssociation
--
--         , requestDeleteTransitGatewayConnectPeer $
--             newDeleteTransitGatewayConnectPeer
--
--         , requestCancelSpotFleetRequests $
--             newCancelSpotFleetRequests
--
--         , requestDescribeClientVpnAuthorizationRules $
--             newDescribeClientVpnAuthorizationRules
--
--         , requestDeleteSubnet $
--             newDeleteSubnet
--
--         , requestStopInstances $
--             newStopInstances
--
--         , requestReleaseHosts $
--             newReleaseHosts
--
--         , requestDescribeFastSnapshotRestores $
--             newDescribeFastSnapshotRestores
--
--         , requestCreateDefaultVpc $
--             newCreateDefaultVpc
--
--         , requestCreateTransitGatewayPrefixListReference $
--             newCreateTransitGatewayPrefixListReference
--
--         , requestDescribeDhcpOptions $
--             newDescribeDhcpOptions
--
--         , requestCreateTransitGatewayRoute $
--             newCreateTransitGatewayRoute
--
--         , requestResetNetworkInterfaceAttribute $
--             newResetNetworkInterfaceAttribute
--
--         , requestDeleteTransitGatewayMulticastDomain $
--             newDeleteTransitGatewayMulticastDomain
--
--         , requestDeleteNetworkInterface $
--             newDeleteNetworkInterface
--
--         , requestDisassociateVpcCidrBlock $
--             newDisassociateVpcCidrBlock
--
--         , requestReplaceRouteTableAssociation $
--             newReplaceRouteTableAssociation
--
--         , requestCreateNetworkInsightsPath $
--             newCreateNetworkInsightsPath
--
--         , requestDeregisterTransitGatewayMulticastGroupSources $
--             newDeregisterTransitGatewayMulticastGroupSources
--
--         , requestDescribeCustomerGateways $
--             newDescribeCustomerGateways
--
--         , requestDescribeCapacityReservations $
--             newDescribeCapacityReservations
--
--         , requestModifyTrafficMirrorSession $
--             newModifyTrafficMirrorSession
--
--         , requestDisassociateTransitGatewayRouteTable $
--             newDisassociateTransitGatewayRouteTable
--
--         , requestStartInstances $
--             newStartInstances
--
--         , requestDescribeTransitGatewayPeeringAttachments $
--             newDescribeTransitGatewayPeeringAttachments
--
--         , requestImportImage $
--             newImportImage
--
--         , requestDescribeTrafficMirrorFilters $
--             newDescribeTrafficMirrorFilters
--
--         , requestAssociateAddress $
--             newAssociateAddress
--
--         , requestRunScheduledInstances $
--             newRunScheduledInstances
--
--         , requestCopyFpgaImage $
--             newCopyFpgaImage
--
--         , requestDeleteNatGateway $
--             newDeleteNatGateway
--
--           ]

--     , testGroup "response"
--         [ responseDescribeVpcPeeringConnections $
--             newDescribeVpcPeeringConnectionsResponse
--
--         , responseAssociateTrunkInterface $
--             newAssociateTrunkInterfaceResponse
--
--         , responseDeleteTransitGatewayRoute $
--             newDeleteTransitGatewayRouteResponse
--
--         , responseDescribeExportTasks $
--             newDescribeExportTasksResponse
--
--         , responseDeleteLocalGatewayRouteTableVpcAssociation $
--             newDeleteLocalGatewayRouteTableVpcAssociationResponse
--
--         , responseDeleteVpcEndpointConnectionNotifications $
--             newDeleteVpcEndpointConnectionNotificationsResponse
--
--         , responseCreateTransitGatewayMulticastDomain $
--             newCreateTransitGatewayMulticastDomainResponse
--
--         , responseCreateVpcEndpointServiceConfiguration $
--             newCreateVpcEndpointServiceConfigurationResponse
--
--         , responseDescribeByoipCidrs $
--             newDescribeByoipCidrsResponse
--
--         , responseDetachVolume $
--             newVolumeAttachment
--
--         , responseDeleteNetworkInsightsPath $
--             newDeleteNetworkInsightsPathResponse
--
--         , responseCancelBundleTask $
--             newCancelBundleTaskResponse
--
--         , responseGetCapacityReservationUsage $
--             newGetCapacityReservationUsageResponse
--
--         , responseAssociateTransitGatewayMulticastDomain $
--             newAssociateTransitGatewayMulticastDomainResponse
--
--         , responseCreateTransitGatewayConnectPeer $
--             newCreateTransitGatewayConnectPeerResponse
--
--         , responseReleaseAddress $
--             newReleaseAddressResponse
--
--         , responseModifyVpcTenancy $
--             newModifyVpcTenancyResponse
--
--         , responseDescribeLocalGatewayVirtualInterfaces $
--             newDescribeLocalGatewayVirtualInterfacesResponse
--
--         , responseGetHostReservationPurchasePreview $
--             newGetHostReservationPurchasePreviewResponse
--
--         , responseAuthorizeSecurityGroupEgress $
--             newAuthorizeSecurityGroupEgressResponse
--
--         , responseAcceptReservedInstancesExchangeQuote $
--             newAcceptReservedInstancesExchangeQuoteResponse
--
--         , responseModifyManagedPrefixList $
--             newModifyManagedPrefixListResponse
--
--         , responseDescribeInstanceCreditSpecifications $
--             newDescribeInstanceCreditSpecificationsResponse
--
--         , responseGetTransitGatewayMulticastDomainAssociations $
--             newGetTransitGatewayMulticastDomainAssociationsResponse
--
--         , responseDescribeInstances $
--             newDescribeInstancesResponse
--
--         , responseDisableEbsEncryptionByDefault $
--             newDisableEbsEncryptionByDefaultResponse
--
--         , responseDeregisterInstanceEventNotificationAttributes $
--             newDeregisterInstanceEventNotificationAttributesResponse
--
--         , responseCreateTransitGatewayVpcAttachment $
--             newCreateTransitGatewayVpcAttachmentResponse
--
--         , responseDeregisterTransitGatewayMulticastGroupMembers $
--             newDeregisterTransitGatewayMulticastGroupMembersResponse
--
--         , responseDeleteTransitGatewayPrefixListReference $
--             newDeleteTransitGatewayPrefixListReferenceResponse
--
--         , responseCreateTransitGatewayRouteTable $
--             newCreateTransitGatewayRouteTableResponse
--
--         , responseDisassociateAddress $
--             newDisassociateAddressResponse
--
--         , responseDetachNetworkInterface $
--             newDetachNetworkInterfaceResponse
--
--         , responseDeleteFleets $
--             newDeleteFleetsResponse
--
--         , responseDeleteVpc $
--             newDeleteVpcResponse
--
--         , responseAssociateVpcCidrBlock $
--             newAssociateVpcCidrBlockResponse
--
--         , responseCreateNetworkAcl $
--             newCreateNetworkAclResponse
--
--         , responseDeleteTrafficMirrorTarget $
--             newDeleteTrafficMirrorTargetResponse
--
--         , responseDeleteLaunchTemplate $
--             newDeleteLaunchTemplateResponse
--
--         , responseModifySecurityGroupRules $
--             newModifySecurityGroupRulesResponse
--
--         , responseDeleteVpcEndpoints $
--             newDeleteVpcEndpointsResponse
--
--         , responseDescribeTrafficMirrorSessions $
--             newDescribeTrafficMirrorSessionsResponse
--
--         , responseUpdateSecurityGroupRuleDescriptionsIngress $
--             newUpdateSecurityGroupRuleDescriptionsIngressResponse
--
--         , responseDescribePrefixLists $
--             newDescribePrefixListsResponse
--
--         , responseDescribeVpcClassicLink $
--             newDescribeVpcClassicLinkResponse
--
--         , responseGetAssociatedIpv6PoolCidrs $
--             newGetAssociatedIpv6PoolCidrsResponse
--
--         , responseImportInstance $
--             newImportInstanceResponse
--
--         , responseCreateDefaultSubnet $
--             newCreateDefaultSubnetResponse
--
--         , responseDeleteFlowLogs $
--             newDeleteFlowLogsResponse
--
--         , responseModifyVolumeAttribute $
--             newModifyVolumeAttributeResponse
--
--         , responseCreateNetworkInterfacePermission $
--             newCreateNetworkInterfacePermissionResponse
--
--         , responseDescribeScheduledInstanceAvailability $
--             newDescribeScheduledInstanceAvailabilityResponse
--
--         , responseDescribeClientVpnEndpoints $
--             newDescribeClientVpnEndpointsResponse
--
--         , responseRejectVpcEndpointConnections $
--             newRejectVpcEndpointConnectionsResponse
--
--         , responseDescribeTags $
--             newDescribeTagsResponse
--
--         , responseAssociateTransitGatewayRouteTable $
--             newAssociateTransitGatewayRouteTableResponse
--
--         , responseDescribeLocalGatewayRouteTables $
--             newDescribeLocalGatewayRouteTablesResponse
--
--         , responseRejectTransitGatewayVpcAttachment $
--             newRejectTransitGatewayVpcAttachmentResponse
--
--         , responseCreateVpnGateway $
--             newCreateVpnGatewayResponse
--
--         , responseAcceptTransitGatewayPeeringAttachment $
--             newAcceptTransitGatewayPeeringAttachmentResponse
--
--         , responseGetTransitGatewayRouteTableAssociations $
--             newGetTransitGatewayRouteTableAssociationsResponse
--
--         , responseDeleteVpnGateway $
--             newDeleteVpnGatewayResponse
--
--         , responseDescribeImportImageTasks $
--             newDescribeImportImageTasksResponse
--
--         , responseExportTransitGatewayRoutes $
--             newExportTransitGatewayRoutesResponse
--
--         , responseDescribeLaunchTemplateVersions $
--             newDescribeLaunchTemplateVersionsResponse
--
--         , responseDescribeFpgaImages $
--             newDescribeFpgaImagesResponse
--
--         , responseGetReservedInstancesExchangeQuote $
--             newGetReservedInstancesExchangeQuoteResponse
--
--         , responseCreateKeyPair $
--             newCreateKeyPairResponse
--
--         , responseDescribeElasticGpus $
--             newDescribeElasticGpusResponse
--
--         , responseStartNetworkInsightsAnalysis $
--             newStartNetworkInsightsAnalysisResponse
--
--         , responseDescribeSecurityGroupRules $
--             newDescribeSecurityGroupRulesResponse
--
--         , responseDeleteNetworkAcl $
--             newDeleteNetworkAclResponse
--
--         , responseDescribeRouteTables $
--             newDescribeRouteTablesResponse
--
--         , responseDescribeFleetInstances $
--             newDescribeFleetInstancesResponse
--
--         , responseDeleteTransitGatewayRouteTable $
--             newDeleteTransitGatewayRouteTableResponse
--
--         , responseCreateLaunchTemplate $
--             newCreateLaunchTemplateResponse
--
--         , responseModifyInstanceEventWindow $
--             newModifyInstanceEventWindowResponse
--
--         , responseMoveAddressToVpc $
--             newMoveAddressToVpcResponse
--
--         , responseAcceptTransitGatewayMulticastDomainAssociations $
--             newAcceptTransitGatewayMulticastDomainAssociationsResponse
--
--         , responseRestoreAddressToClassic $
--             newRestoreAddressToClassicResponse
--
--         , responseDescribeAvailabilityZones $
--             newDescribeAvailabilityZonesResponse
--
--         , responseCreateStoreImageTask $
--             newCreateStoreImageTaskResponse
--
--         , responseCopySnapshot $
--             newCopySnapshotResponse
--
--         , responseDeleteNetworkInterfacePermission $
--             newDeleteNetworkInterfacePermissionResponse
--
--         , responseCreateFlowLogs $
--             newCreateFlowLogsResponse
--
--         , responseDetachClassicLinkVpc $
--             newDetachClassicLinkVpcResponse
--
--         , responseDeleteRouteTable $
--             newDeleteRouteTableResponse
--
--         , responseModifyVpnConnectionOptions $
--             newModifyVpnConnectionOptionsResponse
--
--         , responseMonitorInstances $
--             newMonitorInstancesResponse
--
--         , responseModifyIdFormat $
--             newModifyIdFormatResponse
--
--         , responseAllocateHosts $
--             newAllocateHostsResponse
--
--         , responseDescribeImageAttribute $
--             newDescribeImageAttributeResponse
--
--         , responseDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations $
--             newDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse
--
--         , responseDescribeReservedInstancesModifications $
--             newDescribeReservedInstancesModificationsResponse
--
--         , responseStartVpcEndpointServicePrivateDnsVerification $
--             newStartVpcEndpointServicePrivateDnsVerificationResponse
--
--         , responseCreateTrafficMirrorFilterRule $
--             newCreateTrafficMirrorFilterRuleResponse
--
--         , responseGetEbsDefaultKmsKeyId $
--             newGetEbsDefaultKmsKeyIdResponse
--
--         , responseDescribeClientVpnRoutes $
--             newDescribeClientVpnRoutesResponse
--
--         , responseModifyVpnConnection $
--             newModifyVpnConnectionResponse
--
--         , responseModifyFleet $
--             newModifyFleetResponse
--
--         , responseRegisterImage $
--             newRegisterImageResponse
--
--         , responseRevokeClientVpnIngress $
--             newRevokeClientVpnIngressResponse
--
--         , responseUpdateSecurityGroupRuleDescriptionsEgress $
--             newUpdateSecurityGroupRuleDescriptionsEgressResponse
--
--         , responseModifyVpcEndpoint $
--             newModifyVpcEndpointResponse
--
--         , responseUnassignPrivateIpAddresses $
--             newUnassignPrivateIpAddressesResponse
--
--         , responseEnableFastSnapshotRestores $
--             newEnableFastSnapshotRestoresResponse
--
--         , responseCancelImportTask $
--             newCancelImportTaskResponse
--
--         , responseDescribeVolumes $
--             newDescribeVolumesResponse
--
--         , responseCreateClientVpnEndpoint $
--             newCreateClientVpnEndpointResponse
--
--         , responseResetFpgaImageAttribute $
--             newResetFpgaImageAttributeResponse
--
--         , responseGetConsoleOutput $
--             newGetConsoleOutputResponse
--
--         , responseDeleteFpgaImage $
--             newDeleteFpgaImageResponse
--
--         , responseModifyReservedInstances $
--             newModifyReservedInstancesResponse
--
--         , responseCreateRestoreImageTask $
--             newCreateRestoreImageTaskResponse
--
--         , responseDescribeSpotInstanceRequests $
--             newDescribeSpotInstanceRequestsResponse
--
--         , responseModifyVpcEndpointServicePermissions $
--             newModifyVpcEndpointServicePermissionsResponse
--
--         , responseUnassignIpv6Addresses $
--             newUnassignIpv6AddressesResponse
--
--         , responseDescribeVolumesModifications $
--             newDescribeVolumesModificationsResponse
--
--         , responseDescribeIdFormat $
--             newDescribeIdFormatResponse
--
--         , responseReportInstanceStatus $
--             newReportInstanceStatusResponse
--
--         , responseRunInstances $
--             newReservation
--
--         , responseModifyHosts $
--             newModifyHostsResponse
--
--         , responseAttachVolume $
--             newVolumeAttachment
--
--         , responseDescribeStoreImageTasks $
--             newDescribeStoreImageTasksResponse
--
--         , responseCreateReplaceRootVolumeTask $
--             newCreateReplaceRootVolumeTaskResponse
--
--         , responseDescribeAccountAttributes $
--             newDescribeAccountAttributesResponse
--
--         , responseModifyImageAttribute $
--             newModifyImageAttributeResponse
--
--         , responseRegisterTransitGatewayMulticastGroupSources $
--             newRegisterTransitGatewayMulticastGroupSourcesResponse
--
--         , responseRebootInstances $
--             newRebootInstancesResponse
--
--         , responseAssociateRouteTable $
--             newAssociateRouteTableResponse
--
--         , responseAssociateIamInstanceProfile $
--             newAssociateIamInstanceProfileResponse
--
--         , responsePurchaseHostReservation $
--             newPurchaseHostReservationResponse
--
--         , responseDescribeTrafficMirrorTargets $
--             newDescribeTrafficMirrorTargetsResponse
--
--         , responseGetManagedPrefixListAssociations $
--             newGetManagedPrefixListAssociationsResponse
--
--         , responseCancelConversionTask $
--             newCancelConversionTaskResponse
--
--         , responseModifyVpcEndpointServiceConfiguration $
--             newModifyVpcEndpointServiceConfigurationResponse
--
--         , responseCreateDhcpOptions $
--             newCreateDhcpOptionsResponse
--
--         , responseCreateManagedPrefixList $
--             newCreateManagedPrefixListResponse
--
--         , responseSearchTransitGatewayRoutes $
--             newSearchTransitGatewayRoutesResponse
--
--         , responseDescribeVpcs $
--             newDescribeVpcsResponse
--
--         , responseDescribeLocalGateways $
--             newDescribeLocalGatewaysResponse
--
--         , responseDescribeIpv6Pools $
--             newDescribeIpv6PoolsResponse
--
--         , responseCreateRouteTable $
--             newCreateRouteTableResponse
--
--         , responseDescribeVpcEndpointConnectionNotifications $
--             newDescribeVpcEndpointConnectionNotificationsResponse
--
--         , responseGetTransitGatewayPrefixListReferences $
--             newGetTransitGatewayPrefixListReferencesResponse
--
--         , responseAcceptVpcEndpointConnections $
--             newAcceptVpcEndpointConnectionsResponse
--
--         , responseGetTransitGatewayRouteTablePropagations $
--             newGetTransitGatewayRouteTablePropagationsResponse
--
--         , responseAssociateDhcpOptions $
--             newAssociateDhcpOptionsResponse
--
--         , responseDeleteEgressOnlyInternetGateway $
--             newDeleteEgressOnlyInternetGatewayResponse
--
--         , responseGetVpnConnectionDeviceTypes $
--             newGetVpnConnectionDeviceTypesResponse
--
--         , responseCreateSubnetCidrReservation $
--             newCreateSubnetCidrReservationResponse
--
--         , responseDisableFastSnapshotRestores $
--             newDisableFastSnapshotRestoresResponse
--
--         , responseRequestSpotInstances $
--             newRequestSpotInstancesResponse
--
--         , responseDescribeLaunchTemplates $
--             newDescribeLaunchTemplatesResponse
--
--         , responseCreateImage $
--             newCreateImageResponse
--
--         , responseModifyTransitGatewayVpcAttachment $
--             newModifyTransitGatewayVpcAttachmentResponse
--
--         , responseAssignIpv6Addresses $
--             newAssignIpv6AddressesResponse
--
--         , responseDescribeLocalGatewayVirtualInterfaceGroups $
--             newDescribeLocalGatewayVirtualInterfaceGroupsResponse
--
--         , responseDescribeVpnConnections $
--             newDescribeVpnConnectionsResponse
--
--         , responseCreateNetworkAclEntry $
--             newCreateNetworkAclEntryResponse
--
--         , responseDescribePlacementGroups $
--             newDescribePlacementGroupsResponse
--
--         , responseModifySnapshotAttribute $
--             newModifySnapshotAttributeResponse
--
--         , responseModifyIdentityIdFormat $
--             newModifyIdentityIdFormatResponse
--
--         , responseEnableVgwRoutePropagation $
--             newEnableVgwRoutePropagationResponse
--
--         , responseDeleteTags $
--             newDeleteTagsResponse
--
--         , responseResetInstanceAttribute $
--             newResetInstanceAttributeResponse
--
--         , responseDisassociateEnclaveCertificateIamRole $
--             newDisassociateEnclaveCertificateIamRoleResponse
--
--         , responseDeleteVpcPeeringConnection $
--             newDeleteVpcPeeringConnectionResponse
--
--         , responseDescribeFlowLogs $
--             newDescribeFlowLogsResponse
--
--         , responseDescribeReservedInstancesOfferings $
--             newDescribeReservedInstancesOfferingsResponse
--
--         , responseDescribeFleets $
--             newDescribeFleetsResponse
--
--         , responseAttachNetworkInterface $
--             newAttachNetworkInterfaceResponse
--
--         , responseConfirmProductInstance $
--             newConfirmProductInstanceResponse
--
--         , responseDescribeTransitGatewayAttachments $
--             newDescribeTransitGatewayAttachmentsResponse
--
--         , responseModifyAvailabilityZoneGroup $
--             newModifyAvailabilityZoneGroupResponse
--
--         , responseReplaceNetworkAclEntry $
--             newReplaceNetworkAclEntryResponse
--
--         , responseDescribeSpotFleetRequestHistory $
--             newDescribeSpotFleetRequestHistoryResponse
--
--         , responseCreateLocalGatewayRoute $
--             newCreateLocalGatewayRouteResponse
--
--         , responseDescribeVpcEndpoints $
--             newDescribeVpcEndpointsResponse
--
--         , responseModifyInstanceEventStartTime $
--             newModifyInstanceEventStartTimeResponse
--
--         , responseDescribeStaleSecurityGroups $
--             newDescribeStaleSecurityGroupsResponse
--
--         , responseDescribeInstanceStatus $
--             newDescribeInstanceStatusResponse
--
--         , responseDeleteNetworkAclEntry $
--             newDeleteNetworkAclEntryResponse
--
--         , responseGetConsoleScreenshot $
--             newGetConsoleScreenshotResponse
--
--         , responseGetGroupsForCapacityReservation $
--             newGetGroupsForCapacityReservationResponse
--
--         , responseDisassociateIamInstanceProfile $
--             newDisassociateIamInstanceProfileResponse
--
--         , responseDescribeVpcEndpointServiceConfigurations $
--             newDescribeVpcEndpointServiceConfigurationsResponse
--
--         , responseCancelSpotInstanceRequests $
--             newCancelSpotInstanceRequestsResponse
--
--         , responseDeleteLocalGatewayRoute $
--             newDeleteLocalGatewayRouteResponse
--
--         , responseDescribeVpcEndpointServices $
--             newDescribeVpcEndpointServicesResponse
--
--         , responseDisassociateRouteTable $
--             newDisassociateRouteTableResponse
--
--         , responseAssignPrivateIpAddresses $
--             newAssignPrivateIpAddressesResponse
--
--         , responseGetFlowLogsIntegrationTemplate $
--             newGetFlowLogsIntegrationTemplateResponse
--
--         , responseModifyVpnTunnelCertificate $
--             newModifyVpnTunnelCertificateResponse
--
--         , responseDisableVgwRoutePropagation $
--             newDisableVgwRoutePropagationResponse
--
--         , responseDeleteSnapshot $
--             newDeleteSnapshotResponse
--
--         , responseDescribeSubnets $
--             newDescribeSubnetsResponse
--
--         , responseCreateSpotDatafeedSubscription $
--             newCreateSpotDatafeedSubscriptionResponse
--
--         , responseUnmonitorInstances $
--             newUnmonitorInstancesResponse
--
--         , responseImportVolume $
--             newImportVolumeResponse
--
--         , responseDescribeAddresses $
--             newDescribeAddressesResponse
--
--         , responsePurchaseReservedInstancesOffering $
--             newPurchaseReservedInstancesOfferingResponse
--
--         , responseDescribeSnapshotAttribute $
--             newDescribeSnapshotAttributeResponse
--
--         , responseAuthorizeSecurityGroupIngress $
--             newAuthorizeSecurityGroupIngressResponse
--
--         , responseDescribeNatGateways $
--             newDescribeNatGatewaysResponse
--
--         , responseDisableVpcClassicLink $
--             newDisableVpcClassicLinkResponse
--
--         , responseDescribeTransitGatewayMulticastDomains $
--             newDescribeTransitGatewayMulticastDomainsResponse
--
--         , responseGetTransitGatewayAttachmentPropagations $
--             newGetTransitGatewayAttachmentPropagationsResponse
--
--         , responseModifyVpcEndpointConnectionNotification $
--             newModifyVpcEndpointConnectionNotificationResponse
--
--         , responseRestoreManagedPrefixListVersion $
--             newRestoreManagedPrefixListVersionResponse
--
--         , responseDescribeTransitGatewayConnectPeers $
--             newDescribeTransitGatewayConnectPeersResponse
--
--         , responseDeleteCarrierGateway $
--             newDeleteCarrierGatewayResponse
--
--         , responseDescribeNetworkInterfaces $
--             newDescribeNetworkInterfacesResponse
--
--         , responseDescribeTransitGatewayVpcAttachments $
--             newDescribeTransitGatewayVpcAttachmentsResponse
--
--         , responseModifyAddressAttribute $
--             newModifyAddressAttributeResponse
--
--         , responseDescribeImportSnapshotTasks $
--             newDescribeImportSnapshotTasksResponse
--
--         , responseCopyImage $
--             newCopyImageResponse
--
--         , responseDescribeInstanceEventNotificationAttributes $
--             newDescribeInstanceEventNotificationAttributesResponse
--
--         , responseEnableSerialConsoleAccess $
--             newEnableSerialConsoleAccessResponse
--
--         , responseModifyTrafficMirrorFilterRule $
--             newModifyTrafficMirrorFilterRuleResponse
--
--         , responseDescribeCarrierGateways $
--             newDescribeCarrierGatewaysResponse
--
--         , responseDeleteInternetGateway $
--             newDeleteInternetGatewayResponse
--
--         , responseModifyInstanceCapacityReservationAttributes $
--             newModifyInstanceCapacityReservationAttributesResponse
--
--         , responseDescribeNetworkInterfaceAttribute $
--             newDescribeNetworkInterfaceAttributeResponse
--
--         , responseAttachClassicLinkVpc $
--             newAttachClassicLinkVpcResponse
--
--         , responseGetSubnetCidrReservations $
--             newGetSubnetCidrReservationsResponse
--
--         , responseAssociateClientVpnTargetNetwork $
--             newAssociateClientVpnTargetNetworkResponse
--
--         , responseCancelCapacityReservation $
--             newCancelCapacityReservationResponse
--
--         , responseDisableTransitGatewayRouteTablePropagation $
--             newDisableTransitGatewayRouteTablePropagationResponse
--
--         , responseCancelReservedInstancesListing $
--             newCancelReservedInstancesListingResponse
--
--         , responseDeleteQueuedReservedInstances $
--             newDeleteQueuedReservedInstancesResponse
--
--         , responseCreateFleet $
--             newCreateFleetResponse
--
--         , responseDescribeSnapshots $
--             newDescribeSnapshotsResponse
--
--         , responseDescribeReservedInstancesListings $
--             newDescribeReservedInstancesListingsResponse
--
--         , responseModifyClientVpnEndpoint $
--             newModifyClientVpnEndpointResponse
--
--         , responseCreateVpcEndpoint $
--             newCreateVpcEndpointResponse
--
--         , responseCreateVpnConnection $
--             newCreateVpnConnectionResponse
--
--         , responseImportClientVpnClientCertificateRevocationList $
--             newImportClientVpnClientCertificateRevocationListResponse
--
--         , responseAssociateSubnetCidrBlock $
--             newAssociateSubnetCidrBlockResponse
--
--         , responseDescribeSpotPriceHistory $
--             newDescribeSpotPriceHistoryResponse
--
--         , responseEnableEbsEncryptionByDefault $
--             newEnableEbsEncryptionByDefaultResponse
--
--         , responseDescribeVpcClassicLinkDnsSupport $
--             newDescribeVpcClassicLinkDnsSupportResponse
--
--         , responseCreateLocalGatewayRouteTableVpcAssociation $
--             newCreateLocalGatewayRouteTableVpcAssociationResponse
--
--         , responseDescribeAggregateIdFormat $
--             newDescribeAggregateIdFormatResponse
--
--         , responseEnableTransitGatewayRouteTablePropagation $
--             newEnableTransitGatewayRouteTablePropagationResponse
--
--         , responseRequestSpotFleet $
--             newRequestSpotFleetResponse
--
--         , responseDescribeBundleTasks $
--             newDescribeBundleTasksResponse
--
--         , responseModifyNetworkInterfaceAttribute $
--             newModifyNetworkInterfaceAttributeResponse
--
--         , responseDisableSerialConsoleAccess $
--             newDisableSerialConsoleAccessResponse
--
--         , responseDescribeInstanceTypeOfferings $
--             newDescribeInstanceTypeOfferingsResponse
--
--         , responseModifySpotFleetRequest $
--             newModifySpotFleetRequestResponse
--
--         , responseDeregisterImage $
--             newDeregisterImageResponse
--
--         , responseResetAddressAttribute $
--             newResetAddressAttributeResponse
--
--         , responseDescribeCoipPools $
--             newDescribeCoipPoolsResponse
--
--         , responseDescribeInstanceEventWindows $
--             newDescribeInstanceEventWindowsResponse
--
--         , responseCreateSubnet $
--             newCreateSubnetResponse
--
--         , responseDescribeSecurityGroups $
--             newDescribeSecurityGroupsResponse
--
--         , responseDeletePlacementGroup $
--             newDeletePlacementGroupResponse
--
--         , responseEnableVolumeIO $
--             newEnableVolumeIOResponse
--
--         , responseCreateNatGateway $
--             newCreateNatGatewayResponse
--
--         , responseGetAssociatedEnclaveCertificateIamRoles $
--             newGetAssociatedEnclaveCertificateIamRolesResponse
--
--         , responseModifySubnetAttribute $
--             newModifySubnetAttributeResponse
--
--         , responseDescribeTransitGatewayConnects $
--             newDescribeTransitGatewayConnectsResponse
--
--         , responseDetachVpnGateway $
--             newDetachVpnGatewayResponse
--
--         , responseCreateNetworkInterface $
--             newCreateNetworkInterfaceResponse
--
--         , responseDeleteVpnConnection $
--             newDeleteVpnConnectionResponse
--
--         , responseDescribeInstanceTypes $
--             newDescribeInstanceTypesResponse
--
--         , responseCancelExportTask $
--             newCancelExportTaskResponse
--
--         , responseModifyTransitGateway $
--             newModifyTransitGatewayResponse
--
--         , responseCreateInternetGateway $
--             newCreateInternetGatewayResponse
--
--         , responseSendDiagnosticInterrupt $
--             newSendDiagnosticInterruptResponse
--
--         , responseDisassociateClientVpnTargetNetwork $
--             newDisassociateClientVpnTargetNetworkResponse
--
--         , responseModifyInstanceMetadataOptions $
--             newModifyInstanceMetadataOptionsResponse
--
--         , responseDescribeSpotDatafeedSubscription $
--             newDescribeSpotDatafeedSubscriptionResponse
--
--         , responseExportClientVpnClientConfiguration $
--             newExportClientVpnClientConfigurationResponse
--
--         , responseDeleteKeyPair $
--             newDeleteKeyPairResponse
--
--         , responseDescribeEgressOnlyInternetGateways $
--             newDescribeEgressOnlyInternetGatewaysResponse
--
--         , responseCreateVolume $
--             newVolume
--
--         , responseModifyTrafficMirrorFilterNetworkServices $
--             newModifyTrafficMirrorFilterNetworkServicesResponse
--
--         , responseDescribeVpcAttribute $
--             newDescribeVpcAttributeResponse
--
--         , responseDescribeTrunkInterfaceAssociations $
--             newDescribeTrunkInterfaceAssociationsResponse
--
--         , responseCreateInstanceExportTask $
--             newCreateInstanceExportTaskResponse
--
--         , responseCreateClientVpnRoute $
--             newCreateClientVpnRouteResponse
--
--         , responseModifyCapacityReservation $
--             newModifyCapacityReservationResponse
--
--         , responseRevokeSecurityGroupEgress $
--             newRevokeSecurityGroupEgressResponse
--
--         , responseDescribeSecurityGroupReferences $
--             newDescribeSecurityGroupReferencesResponse
--
--         , responseDisassociateSubnetCidrBlock $
--             newDisassociateSubnetCidrBlockResponse
--
--         , responseDetachInternetGateway $
--             newDetachInternetGatewayResponse
--
--         , responseSearchTransitGatewayMulticastGroups $
--             newSearchTransitGatewayMulticastGroupsResponse
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
--         , responseModifyVpcAttribute $
--             newModifyVpcAttributeResponse
--
--         , responseDeleteClientVpnRoute $
--             newDeleteClientVpnRouteResponse
--
--         , responseDeprovisionByoipCidr $
--             newDeprovisionByoipCidrResponse
--
--         , responseDisassociateTrunkInterface $
--             newDisassociateTrunkInterfaceResponse
--
--         , responseImportSnapshot $
--             newImportSnapshotResponse
--
--         , responseDescribeSpotFleetInstances $
--             newDescribeSpotFleetInstancesResponse
--
--         , responseDescribeClientVpnConnections $
--             newDescribeClientVpnConnectionsResponse
--
--         , responseCreateTrafficMirrorTarget $
--             newCreateTrafficMirrorTargetResponse
--
--         , responseModifyInstanceCreditSpecification $
--             newModifyInstanceCreditSpecificationResponse
--
--         , responseAcceptVpcPeeringConnection $
--             newAcceptVpcPeeringConnectionResponse
--
--         , responseDescribeVolumeAttribute $
--             newDescribeVolumeAttributeResponse
--
--         , responseDescribeSpotFleetRequests $
--             newDescribeSpotFleetRequestsResponse
--
--         , responseDescribeAddressesAttribute $
--             newDescribeAddressesAttributeResponse
--
--         , responseDescribePrincipalIdFormat $
--             newDescribePrincipalIdFormatResponse
--
--         , responseDescribeTransitGateways $
--             newDescribeTransitGatewaysResponse
--
--         , responseModifyVpcPeeringConnectionOptions $
--             newModifyVpcPeeringConnectionOptionsResponse
--
--         , responseCreateVpcEndpointConnectionNotification $
--             newCreateVpcEndpointConnectionNotificationResponse
--
--         , responseDeleteVpcEndpointServiceConfigurations $
--             newDeleteVpcEndpointServiceConfigurationsResponse
--
--         , responseDescribeFleetHistory $
--             newDescribeFleetHistoryResponse
--
--         , responseCreateVpc $
--             newCreateVpcResponse
--
--         , responseDescribeVolumeStatus $
--             newDescribeVolumeStatusResponse
--
--         , responseGetSerialConsoleAccessStatus $
--             newGetSerialConsoleAccessStatusResponse
--
--         , responseDescribeReplaceRootVolumeTasks $
--             newDescribeReplaceRootVolumeTasksResponse
--
--         , responseDescribeImages $
--             newDescribeImagesResponse
--
--         , responseDeleteVolume $
--             newDeleteVolumeResponse
--
--         , responseSearchLocalGatewayRoutes $
--             newSearchLocalGatewayRoutesResponse
--
--         , responseDescribeMovingAddresses $
--             newDescribeMovingAddressesResponse
--
--         , responseCreateTrafficMirrorSession $
--             newCreateTrafficMirrorSessionResponse
--
--         , responseDescribeScheduledInstances $
--             newDescribeScheduledInstancesResponse
--
--         , responseAssociateEnclaveCertificateIamRole $
--             newAssociateEnclaveCertificateIamRoleResponse
--
--         , responseRejectTransitGatewayMulticastDomainAssociations $
--             newRejectTransitGatewayMulticastDomainAssociationsResponse
--
--         , responseDeleteTransitGateway $
--             newDeleteTransitGatewayResponse
--
--         , responseDescribeHosts $
--             newDescribeHostsResponse
--
--         , responseDescribeNetworkInterfacePermissions $
--             newDescribeNetworkInterfacePermissionsResponse
--
--         , responseGetVpnConnectionDeviceSampleConfiguration $
--             newGetVpnConnectionDeviceSampleConfigurationResponse
--
--         , responseDescribeVpcEndpointServicePermissions $
--             newDescribeVpcEndpointServicePermissionsResponse
--
--         , responseDescribeHostReservationOfferings $
--             newDescribeHostReservationOfferingsResponse
--
--         , responseDescribeVpcEndpointConnections $
--             newDescribeVpcEndpointConnectionsResponse
--
--         , responseDescribeFpgaImageAttribute $
--             newDescribeFpgaImageAttributeResponse
--
--         , responseEnableImageDeprecation $
--             newEnableImageDeprecationResponse
--
--         , responseResetImageAttribute $
--             newResetImageAttributeResponse
--
--         , responseAdvertiseByoipCidr $
--             newAdvertiseByoipCidrResponse
--
--         , responseDescribeTransitGatewayRouteTables $
--             newDescribeTransitGatewayRouteTablesResponse
--
--         , responseModifyTransitGatewayPrefixListReference $
--             newModifyTransitGatewayPrefixListReferenceResponse
--
--         , responseRegisterInstanceEventNotificationAttributes $
--             newRegisterInstanceEventNotificationAttributesResponse
--
--         , responseDeleteManagedPrefixList $
--             newDeleteManagedPrefixListResponse
--
--         , responseDescribeRegions $
--             newDescribeRegionsResponse
--
--         , responseRevokeSecurityGroupIngress $
--             newRevokeSecurityGroupIngressResponse
--
--         , responseDeleteVpnConnectionRoute $
--             newDeleteVpnConnectionRouteResponse
--
--         , responseDescribeNetworkAcls $
--             newDescribeNetworkAclsResponse
--
--         , responseDeleteDhcpOptions $
--             newDeleteDhcpOptionsResponse
--
--         , responseDescribeVpnGateways $
--             newDescribeVpnGatewaysResponse
--
--         , responseRegisterTransitGatewayMulticastGroupMembers $
--             newRegisterTransitGatewayMulticastGroupMembersResponse
--
--         , responseDescribeHostReservations $
--             newDescribeHostReservationsResponse
--
--         , responseRejectVpcPeeringConnection $
--             newRejectVpcPeeringConnectionResponse
--
--         , responseCreateEgressOnlyInternetGateway $
--             newCreateEgressOnlyInternetGatewayResponse
--
--         , responseDeleteSubnetCidrReservation $
--             newDeleteSubnetCidrReservationResponse
--
--         , responseEnableVpcClassicLinkDnsSupport $
--             newEnableVpcClassicLinkDnsSupportResponse
--
--         , responseAllocateAddress $
--             newAllocateAddressResponse
--
--         , responseExportImage $
--             newExportImageResponse
--
--         , responseRejectTransitGatewayPeeringAttachment $
--             newRejectTransitGatewayPeeringAttachmentResponse
--
--         , responseGetLaunchTemplateData $
--             newGetLaunchTemplateDataResponse
--
--         , responseDescribeReservedInstances $
--             newDescribeReservedInstancesResponse
--
--         , responseModifyDefaultCreditSpecification $
--             newModifyDefaultCreditSpecificationResponse
--
--         , responseGetManagedPrefixListEntries $
--             newGetManagedPrefixListEntriesResponse
--
--         , responseDisableVpcClassicLinkDnsSupport $
--             newDisableVpcClassicLinkDnsSupportResponse
--
--         , responseDisableImageDeprecation $
--             newDisableImageDeprecationResponse
--
--         , responseApplySecurityGroupsToClientVpnTargetNetwork $
--             newApplySecurityGroupsToClientVpnTargetNetworkResponse
--
--         , responseCreateLaunchTemplateVersion $
--             newCreateLaunchTemplateVersionResponse
--
--         , responseCreateVpnConnectionRoute $
--             newCreateVpnConnectionRouteResponse
--
--         , responseDisassociateInstanceEventWindow $
--             newDisassociateInstanceEventWindowResponse
--
--         , responseDescribeConversionTasks $
--             newDescribeConversionTasksResponse
--
--         , responseDeleteTrafficMirrorSession $
--             newDeleteTrafficMirrorSessionResponse
--
--         , responseCreateTransitGateway $
--             newCreateTransitGatewayResponse
--
--         , responseCreateSnapshots $
--             newCreateSnapshotsResponse
--
--         , responseDeleteClientVpnEndpoint $
--             newDeleteClientVpnEndpointResponse
--
--         , responseExportClientVpnClientCertificateRevocationList $
--             newExportClientVpnClientCertificateRevocationListResponse
--
--         , responseDescribeKeyPairs $
--             newDescribeKeyPairsResponse
--
--         , responseCreateFpgaImage $
--             newCreateFpgaImageResponse
--
--         , responseDescribeClassicLinkInstances $
--             newDescribeClassicLinkInstancesResponse
--
--         , responseDeleteTrafficMirrorFilterRule $
--             newDeleteTrafficMirrorFilterRuleResponse
--
--         , responseTerminateInstances $
--             newTerminateInstancesResponse
--
--         , responseModifyFpgaImageAttribute $
--             newModifyFpgaImageAttributeResponse
--
--         , responseWithdrawByoipCidr $
--             newWithdrawByoipCidrResponse
--
--         , responseAttachVpnGateway $
--             newAttachVpnGatewayResponse
--
--         , responseAcceptTransitGatewayVpcAttachment $
--             newAcceptTransitGatewayVpcAttachmentResponse
--
--         , responseDeleteSpotDatafeedSubscription $
--             newDeleteSpotDatafeedSubscriptionResponse
--
--         , responseDescribeExportImageTasks $
--             newDescribeExportImageTasksResponse
--
--         , responseCreateCarrierGateway $
--             newCreateCarrierGatewayResponse
--
--         , responseAttachInternetGateway $
--             newAttachInternetGatewayResponse
--
--         , responseDescribeClientVpnTargetNetworks $
--             newDescribeClientVpnTargetNetworksResponse
--
--         , responseCreateTrafficMirrorFilter $
--             newCreateTrafficMirrorFilterResponse
--
--         , responseDeleteNetworkInsightsAnalysis $
--             newDeleteNetworkInsightsAnalysisResponse
--
--         , responseDescribeIamInstanceProfileAssociations $
--             newDescribeIamInstanceProfileAssociationsResponse
--
--         , responseImportKeyPair $
--             newImportKeyPairResponse
--
--         , responseEnableVpcClassicLink $
--             newEnableVpcClassicLinkResponse
--
--         , responseDescribeNetworkInsightsAnalyses $
--             newDescribeNetworkInsightsAnalysesResponse
--
--         , responseDeleteRoute $
--             newDeleteRouteResponse
--
--         , responseDescribeNetworkInsightsPaths $
--             newDescribeNetworkInsightsPathsResponse
--
--         , responseCreateCapacityReservation $
--             newCreateCapacityReservationResponse
--
--         , responseDeleteInstanceEventWindow $
--             newDeleteInstanceEventWindowResponse
--
--         , responseModifyEbsDefaultKmsKeyId $
--             newModifyEbsDefaultKmsKeyIdResponse
--
--         , responseProvisionByoipCidr $
--             newProvisionByoipCidrResponse
--
--         , responseDeleteTransitGatewayConnect $
--             newDeleteTransitGatewayConnectResponse
--
--         , responseModifyVpnTunnelOptions $
--             newModifyVpnTunnelOptionsResponse
--
--         , responseCreateTransitGatewayPeeringAttachment $
--             newCreateTransitGatewayPeeringAttachmentResponse
--
--         , responseCreateCustomerGateway $
--             newCreateCustomerGatewayResponse
--
--         , responseModifyVolume $
--             newModifyVolumeResponse
--
--         , responseModifyInstancePlacement $
--             newModifyInstancePlacementResponse
--
--         , responseCreateSnapshot $
--             newSnapshot
--
--         , responseDescribeInstanceAttribute $
--             newDescribeInstanceAttributeResponse
--
--         , responseCreateReservedInstancesListing $
--             newCreateReservedInstancesListingResponse
--
--         , responseDeleteSecurityGroup $
--             newDeleteSecurityGroupResponse
--
--         , responsePurchaseScheduledInstances $
--             newPurchaseScheduledInstancesResponse
--
--         , responseDescribePublicIpv4Pools $
--             newDescribePublicIpv4PoolsResponse
--
--         , responseDescribeLocalGatewayRouteTableVpcAssociations $
--             newDescribeLocalGatewayRouteTableVpcAssociationsResponse
--
--         , responseAuthorizeClientVpnIngress $
--             newAuthorizeClientVpnIngressResponse
--
--         , responseCreateVpcPeeringConnection $
--             newCreateVpcPeeringConnectionResponse
--
--         , responseCreateInstanceEventWindow $
--             newCreateInstanceEventWindowResponse
--
--         , responseCreateSecurityGroup $
--             newCreateSecurityGroupResponse
--
--         , responseDescribeInternetGateways $
--             newDescribeInternetGatewaysResponse
--
--         , responseModifyLaunchTemplate $
--             newModifyLaunchTemplateResponse
--
--         , responseModifyInstanceAttribute $
--             newModifyInstanceAttributeResponse
--
--         , responseResetEbsDefaultKmsKeyId $
--             newResetEbsDefaultKmsKeyIdResponse
--
--         , responseGetEbsEncryptionByDefault $
--             newGetEbsEncryptionByDefaultResponse
--
--         , responseDeleteCustomerGateway $
--             newDeleteCustomerGatewayResponse
--
--         , responseTerminateClientVpnConnections $
--             newTerminateClientVpnConnectionsResponse
--
--         , responseAssociateInstanceEventWindow $
--             newAssociateInstanceEventWindowResponse
--
--         , responseReplaceRoute $
--             newReplaceRouteResponse
--
--         , responseCreateTags $
--             newCreateTagsResponse
--
--         , responseCreateTransitGatewayConnect $
--             newCreateTransitGatewayConnectResponse
--
--         , responseDeleteLaunchTemplateVersions $
--             newDeleteLaunchTemplateVersionsResponse
--
--         , responseGetDefaultCreditSpecification $
--             newGetDefaultCreditSpecificationResponse
--
--         , responseResetSnapshotAttribute $
--             newResetSnapshotAttributeResponse
--
--         , responseDeleteTransitGatewayPeeringAttachment $
--             newDeleteTransitGatewayPeeringAttachmentResponse
--
--         , responseCreateRoute $
--             newCreateRouteResponse
--
--         , responseBundleInstance $
--             newBundleInstanceResponse
--
--         , responseReplaceNetworkAclAssociation $
--             newReplaceNetworkAclAssociationResponse
--
--         , responseDeleteTrafficMirrorFilter $
--             newDeleteTrafficMirrorFilterResponse
--
--         , responseDescribeIdentityIdFormat $
--             newDescribeIdentityIdFormatResponse
--
--         , responseReplaceTransitGatewayRoute $
--             newReplaceTransitGatewayRouteResponse
--
--         , responseGetCoipPoolUsage $
--             newGetCoipPoolUsageResponse
--
--         , responseDeleteTransitGatewayVpcAttachment $
--             newDeleteTransitGatewayVpcAttachmentResponse
--
--         , responseCreatePlacementGroup $
--             newCreatePlacementGroupResponse
--
--         , responseReplaceIamInstanceProfileAssociation $
--             newReplaceIamInstanceProfileAssociationResponse
--
--         , responseDeleteTransitGatewayConnectPeer $
--             newDeleteTransitGatewayConnectPeerResponse
--
--         , responseCancelSpotFleetRequests $
--             newCancelSpotFleetRequestsResponse
--
--         , responseDescribeClientVpnAuthorizationRules $
--             newDescribeClientVpnAuthorizationRulesResponse
--
--         , responseDeleteSubnet $
--             newDeleteSubnetResponse
--
--         , responseStopInstances $
--             newStopInstancesResponse
--
--         , responseReleaseHosts $
--             newReleaseHostsResponse
--
--         , responseDescribeFastSnapshotRestores $
--             newDescribeFastSnapshotRestoresResponse
--
--         , responseCreateDefaultVpc $
--             newCreateDefaultVpcResponse
--
--         , responseCreateTransitGatewayPrefixListReference $
--             newCreateTransitGatewayPrefixListReferenceResponse
--
--         , responseDescribeDhcpOptions $
--             newDescribeDhcpOptionsResponse
--
--         , responseCreateTransitGatewayRoute $
--             newCreateTransitGatewayRouteResponse
--
--         , responseResetNetworkInterfaceAttribute $
--             newResetNetworkInterfaceAttributeResponse
--
--         , responseDeleteTransitGatewayMulticastDomain $
--             newDeleteTransitGatewayMulticastDomainResponse
--
--         , responseDeleteNetworkInterface $
--             newDeleteNetworkInterfaceResponse
--
--         , responseDisassociateVpcCidrBlock $
--             newDisassociateVpcCidrBlockResponse
--
--         , responseReplaceRouteTableAssociation $
--             newReplaceRouteTableAssociationResponse
--
--         , responseCreateNetworkInsightsPath $
--             newCreateNetworkInsightsPathResponse
--
--         , responseDeregisterTransitGatewayMulticastGroupSources $
--             newDeregisterTransitGatewayMulticastGroupSourcesResponse
--
--         , responseDescribeCustomerGateways $
--             newDescribeCustomerGatewaysResponse
--
--         , responseDescribeCapacityReservations $
--             newDescribeCapacityReservationsResponse
--
--         , responseModifyTrafficMirrorSession $
--             newModifyTrafficMirrorSessionResponse
--
--         , responseDisassociateTransitGatewayRouteTable $
--             newDisassociateTransitGatewayRouteTableResponse
--
--         , responseStartInstances $
--             newStartInstancesResponse
--
--         , responseDescribeTransitGatewayPeeringAttachments $
--             newDescribeTransitGatewayPeeringAttachmentsResponse
--
--         , responseImportImage $
--             newImportImageResponse
--
--         , responseDescribeTrafficMirrorFilters $
--             newDescribeTrafficMirrorFiltersResponse
--
--         , responseAssociateAddress $
--             newAssociateAddressResponse
--
--         , responseRunScheduledInstances $
--             newRunScheduledInstancesResponse
--
--         , responseCopyFpgaImage $
--             newCopyFpgaImageResponse
--
--         , responseDeleteNatGateway $
--             newDeleteNatGatewayResponse
--
--           ]
--     ]

-- Requests

requestDescribeVpcPeeringConnections :: DescribeVpcPeeringConnections -> TestTree
requestDescribeVpcPeeringConnections =
  req
    "DescribeVpcPeeringConnections"
    "fixture/DescribeVpcPeeringConnections.yaml"

requestAssociateTrunkInterface :: AssociateTrunkInterface -> TestTree
requestAssociateTrunkInterface =
  req
    "AssociateTrunkInterface"
    "fixture/AssociateTrunkInterface.yaml"

requestDeleteTransitGatewayRoute :: DeleteTransitGatewayRoute -> TestTree
requestDeleteTransitGatewayRoute =
  req
    "DeleteTransitGatewayRoute"
    "fixture/DeleteTransitGatewayRoute.yaml"

requestDescribeExportTasks :: DescribeExportTasks -> TestTree
requestDescribeExportTasks =
  req
    "DescribeExportTasks"
    "fixture/DescribeExportTasks.yaml"

requestDeleteLocalGatewayRouteTableVpcAssociation :: DeleteLocalGatewayRouteTableVpcAssociation -> TestTree
requestDeleteLocalGatewayRouteTableVpcAssociation =
  req
    "DeleteLocalGatewayRouteTableVpcAssociation"
    "fixture/DeleteLocalGatewayRouteTableVpcAssociation.yaml"

requestDeleteVpcEndpointConnectionNotifications :: DeleteVpcEndpointConnectionNotifications -> TestTree
requestDeleteVpcEndpointConnectionNotifications =
  req
    "DeleteVpcEndpointConnectionNotifications"
    "fixture/DeleteVpcEndpointConnectionNotifications.yaml"

requestCreateTransitGatewayMulticastDomain :: CreateTransitGatewayMulticastDomain -> TestTree
requestCreateTransitGatewayMulticastDomain =
  req
    "CreateTransitGatewayMulticastDomain"
    "fixture/CreateTransitGatewayMulticastDomain.yaml"

requestCreateVpcEndpointServiceConfiguration :: CreateVpcEndpointServiceConfiguration -> TestTree
requestCreateVpcEndpointServiceConfiguration =
  req
    "CreateVpcEndpointServiceConfiguration"
    "fixture/CreateVpcEndpointServiceConfiguration.yaml"

requestDescribeByoipCidrs :: DescribeByoipCidrs -> TestTree
requestDescribeByoipCidrs =
  req
    "DescribeByoipCidrs"
    "fixture/DescribeByoipCidrs.yaml"

requestDetachVolume :: DetachVolume -> TestTree
requestDetachVolume =
  req
    "DetachVolume"
    "fixture/DetachVolume.yaml"

requestDeleteNetworkInsightsPath :: DeleteNetworkInsightsPath -> TestTree
requestDeleteNetworkInsightsPath =
  req
    "DeleteNetworkInsightsPath"
    "fixture/DeleteNetworkInsightsPath.yaml"

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

requestAssociateTransitGatewayMulticastDomain :: AssociateTransitGatewayMulticastDomain -> TestTree
requestAssociateTransitGatewayMulticastDomain =
  req
    "AssociateTransitGatewayMulticastDomain"
    "fixture/AssociateTransitGatewayMulticastDomain.yaml"

requestCreateTransitGatewayConnectPeer :: CreateTransitGatewayConnectPeer -> TestTree
requestCreateTransitGatewayConnectPeer =
  req
    "CreateTransitGatewayConnectPeer"
    "fixture/CreateTransitGatewayConnectPeer.yaml"

requestReleaseAddress :: ReleaseAddress -> TestTree
requestReleaseAddress =
  req
    "ReleaseAddress"
    "fixture/ReleaseAddress.yaml"

requestModifyVpcTenancy :: ModifyVpcTenancy -> TestTree
requestModifyVpcTenancy =
  req
    "ModifyVpcTenancy"
    "fixture/ModifyVpcTenancy.yaml"

requestDescribeLocalGatewayVirtualInterfaces :: DescribeLocalGatewayVirtualInterfaces -> TestTree
requestDescribeLocalGatewayVirtualInterfaces =
  req
    "DescribeLocalGatewayVirtualInterfaces"
    "fixture/DescribeLocalGatewayVirtualInterfaces.yaml"

requestGetHostReservationPurchasePreview :: GetHostReservationPurchasePreview -> TestTree
requestGetHostReservationPurchasePreview =
  req
    "GetHostReservationPurchasePreview"
    "fixture/GetHostReservationPurchasePreview.yaml"

requestAuthorizeSecurityGroupEgress :: AuthorizeSecurityGroupEgress -> TestTree
requestAuthorizeSecurityGroupEgress =
  req
    "AuthorizeSecurityGroupEgress"
    "fixture/AuthorizeSecurityGroupEgress.yaml"

requestAcceptReservedInstancesExchangeQuote :: AcceptReservedInstancesExchangeQuote -> TestTree
requestAcceptReservedInstancesExchangeQuote =
  req
    "AcceptReservedInstancesExchangeQuote"
    "fixture/AcceptReservedInstancesExchangeQuote.yaml"

requestModifyManagedPrefixList :: ModifyManagedPrefixList -> TestTree
requestModifyManagedPrefixList =
  req
    "ModifyManagedPrefixList"
    "fixture/ModifyManagedPrefixList.yaml"

requestDescribeInstanceCreditSpecifications :: DescribeInstanceCreditSpecifications -> TestTree
requestDescribeInstanceCreditSpecifications =
  req
    "DescribeInstanceCreditSpecifications"
    "fixture/DescribeInstanceCreditSpecifications.yaml"

requestGetTransitGatewayMulticastDomainAssociations :: GetTransitGatewayMulticastDomainAssociations -> TestTree
requestGetTransitGatewayMulticastDomainAssociations =
  req
    "GetTransitGatewayMulticastDomainAssociations"
    "fixture/GetTransitGatewayMulticastDomainAssociations.yaml"

requestDescribeInstances :: DescribeInstances -> TestTree
requestDescribeInstances =
  req
    "DescribeInstances"
    "fixture/DescribeInstances.yaml"

requestDisableEbsEncryptionByDefault :: DisableEbsEncryptionByDefault -> TestTree
requestDisableEbsEncryptionByDefault =
  req
    "DisableEbsEncryptionByDefault"
    "fixture/DisableEbsEncryptionByDefault.yaml"

requestDeregisterInstanceEventNotificationAttributes :: DeregisterInstanceEventNotificationAttributes -> TestTree
requestDeregisterInstanceEventNotificationAttributes =
  req
    "DeregisterInstanceEventNotificationAttributes"
    "fixture/DeregisterInstanceEventNotificationAttributes.yaml"

requestCreateTransitGatewayVpcAttachment :: CreateTransitGatewayVpcAttachment -> TestTree
requestCreateTransitGatewayVpcAttachment =
  req
    "CreateTransitGatewayVpcAttachment"
    "fixture/CreateTransitGatewayVpcAttachment.yaml"

requestDeregisterTransitGatewayMulticastGroupMembers :: DeregisterTransitGatewayMulticastGroupMembers -> TestTree
requestDeregisterTransitGatewayMulticastGroupMembers =
  req
    "DeregisterTransitGatewayMulticastGroupMembers"
    "fixture/DeregisterTransitGatewayMulticastGroupMembers.yaml"

requestDeleteTransitGatewayPrefixListReference :: DeleteTransitGatewayPrefixListReference -> TestTree
requestDeleteTransitGatewayPrefixListReference =
  req
    "DeleteTransitGatewayPrefixListReference"
    "fixture/DeleteTransitGatewayPrefixListReference.yaml"

requestCreateTransitGatewayRouteTable :: CreateTransitGatewayRouteTable -> TestTree
requestCreateTransitGatewayRouteTable =
  req
    "CreateTransitGatewayRouteTable"
    "fixture/CreateTransitGatewayRouteTable.yaml"

requestDisassociateAddress :: DisassociateAddress -> TestTree
requestDisassociateAddress =
  req
    "DisassociateAddress"
    "fixture/DisassociateAddress.yaml"

requestDetachNetworkInterface :: DetachNetworkInterface -> TestTree
requestDetachNetworkInterface =
  req
    "DetachNetworkInterface"
    "fixture/DetachNetworkInterface.yaml"

requestDeleteFleets :: DeleteFleets -> TestTree
requestDeleteFleets =
  req
    "DeleteFleets"
    "fixture/DeleteFleets.yaml"

requestDeleteVpc :: DeleteVpc -> TestTree
requestDeleteVpc =
  req
    "DeleteVpc"
    "fixture/DeleteVpc.yaml"

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

requestDeleteTrafficMirrorTarget :: DeleteTrafficMirrorTarget -> TestTree
requestDeleteTrafficMirrorTarget =
  req
    "DeleteTrafficMirrorTarget"
    "fixture/DeleteTrafficMirrorTarget.yaml"

requestDeleteLaunchTemplate :: DeleteLaunchTemplate -> TestTree
requestDeleteLaunchTemplate =
  req
    "DeleteLaunchTemplate"
    "fixture/DeleteLaunchTemplate.yaml"

requestModifySecurityGroupRules :: ModifySecurityGroupRules -> TestTree
requestModifySecurityGroupRules =
  req
    "ModifySecurityGroupRules"
    "fixture/ModifySecurityGroupRules.yaml"

requestDeleteVpcEndpoints :: DeleteVpcEndpoints -> TestTree
requestDeleteVpcEndpoints =
  req
    "DeleteVpcEndpoints"
    "fixture/DeleteVpcEndpoints.yaml"

requestDescribeTrafficMirrorSessions :: DescribeTrafficMirrorSessions -> TestTree
requestDescribeTrafficMirrorSessions =
  req
    "DescribeTrafficMirrorSessions"
    "fixture/DescribeTrafficMirrorSessions.yaml"

requestUpdateSecurityGroupRuleDescriptionsIngress :: UpdateSecurityGroupRuleDescriptionsIngress -> TestTree
requestUpdateSecurityGroupRuleDescriptionsIngress =
  req
    "UpdateSecurityGroupRuleDescriptionsIngress"
    "fixture/UpdateSecurityGroupRuleDescriptionsIngress.yaml"

requestDescribePrefixLists :: DescribePrefixLists -> TestTree
requestDescribePrefixLists =
  req
    "DescribePrefixLists"
    "fixture/DescribePrefixLists.yaml"

requestDescribeVpcClassicLink :: DescribeVpcClassicLink -> TestTree
requestDescribeVpcClassicLink =
  req
    "DescribeVpcClassicLink"
    "fixture/DescribeVpcClassicLink.yaml"

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

requestCreateDefaultSubnet :: CreateDefaultSubnet -> TestTree
requestCreateDefaultSubnet =
  req
    "CreateDefaultSubnet"
    "fixture/CreateDefaultSubnet.yaml"

requestDeleteFlowLogs :: DeleteFlowLogs -> TestTree
requestDeleteFlowLogs =
  req
    "DeleteFlowLogs"
    "fixture/DeleteFlowLogs.yaml"

requestModifyVolumeAttribute :: ModifyVolumeAttribute -> TestTree
requestModifyVolumeAttribute =
  req
    "ModifyVolumeAttribute"
    "fixture/ModifyVolumeAttribute.yaml"

requestCreateNetworkInterfacePermission :: CreateNetworkInterfacePermission -> TestTree
requestCreateNetworkInterfacePermission =
  req
    "CreateNetworkInterfacePermission"
    "fixture/CreateNetworkInterfacePermission.yaml"

requestDescribeScheduledInstanceAvailability :: DescribeScheduledInstanceAvailability -> TestTree
requestDescribeScheduledInstanceAvailability =
  req
    "DescribeScheduledInstanceAvailability"
    "fixture/DescribeScheduledInstanceAvailability.yaml"

requestDescribeClientVpnEndpoints :: DescribeClientVpnEndpoints -> TestTree
requestDescribeClientVpnEndpoints =
  req
    "DescribeClientVpnEndpoints"
    "fixture/DescribeClientVpnEndpoints.yaml"

requestRejectVpcEndpointConnections :: RejectVpcEndpointConnections -> TestTree
requestRejectVpcEndpointConnections =
  req
    "RejectVpcEndpointConnections"
    "fixture/RejectVpcEndpointConnections.yaml"

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

requestDescribeLocalGatewayRouteTables :: DescribeLocalGatewayRouteTables -> TestTree
requestDescribeLocalGatewayRouteTables =
  req
    "DescribeLocalGatewayRouteTables"
    "fixture/DescribeLocalGatewayRouteTables.yaml"

requestRejectTransitGatewayVpcAttachment :: RejectTransitGatewayVpcAttachment -> TestTree
requestRejectTransitGatewayVpcAttachment =
  req
    "RejectTransitGatewayVpcAttachment"
    "fixture/RejectTransitGatewayVpcAttachment.yaml"

requestCreateVpnGateway :: CreateVpnGateway -> TestTree
requestCreateVpnGateway =
  req
    "CreateVpnGateway"
    "fixture/CreateVpnGateway.yaml"

requestAcceptTransitGatewayPeeringAttachment :: AcceptTransitGatewayPeeringAttachment -> TestTree
requestAcceptTransitGatewayPeeringAttachment =
  req
    "AcceptTransitGatewayPeeringAttachment"
    "fixture/AcceptTransitGatewayPeeringAttachment.yaml"

requestGetTransitGatewayRouteTableAssociations :: GetTransitGatewayRouteTableAssociations -> TestTree
requestGetTransitGatewayRouteTableAssociations =
  req
    "GetTransitGatewayRouteTableAssociations"
    "fixture/GetTransitGatewayRouteTableAssociations.yaml"

requestDeleteVpnGateway :: DeleteVpnGateway -> TestTree
requestDeleteVpnGateway =
  req
    "DeleteVpnGateway"
    "fixture/DeleteVpnGateway.yaml"

requestDescribeImportImageTasks :: DescribeImportImageTasks -> TestTree
requestDescribeImportImageTasks =
  req
    "DescribeImportImageTasks"
    "fixture/DescribeImportImageTasks.yaml"

requestExportTransitGatewayRoutes :: ExportTransitGatewayRoutes -> TestTree
requestExportTransitGatewayRoutes =
  req
    "ExportTransitGatewayRoutes"
    "fixture/ExportTransitGatewayRoutes.yaml"

requestDescribeLaunchTemplateVersions :: DescribeLaunchTemplateVersions -> TestTree
requestDescribeLaunchTemplateVersions =
  req
    "DescribeLaunchTemplateVersions"
    "fixture/DescribeLaunchTemplateVersions.yaml"

requestDescribeFpgaImages :: DescribeFpgaImages -> TestTree
requestDescribeFpgaImages =
  req
    "DescribeFpgaImages"
    "fixture/DescribeFpgaImages.yaml"

requestGetReservedInstancesExchangeQuote :: GetReservedInstancesExchangeQuote -> TestTree
requestGetReservedInstancesExchangeQuote =
  req
    "GetReservedInstancesExchangeQuote"
    "fixture/GetReservedInstancesExchangeQuote.yaml"

requestCreateKeyPair :: CreateKeyPair -> TestTree
requestCreateKeyPair =
  req
    "CreateKeyPair"
    "fixture/CreateKeyPair.yaml"

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

requestDescribeSecurityGroupRules :: DescribeSecurityGroupRules -> TestTree
requestDescribeSecurityGroupRules =
  req
    "DescribeSecurityGroupRules"
    "fixture/DescribeSecurityGroupRules.yaml"

requestDeleteNetworkAcl :: DeleteNetworkAcl -> TestTree
requestDeleteNetworkAcl =
  req
    "DeleteNetworkAcl"
    "fixture/DeleteNetworkAcl.yaml"

requestDescribeRouteTables :: DescribeRouteTables -> TestTree
requestDescribeRouteTables =
  req
    "DescribeRouteTables"
    "fixture/DescribeRouteTables.yaml"

requestDescribeFleetInstances :: DescribeFleetInstances -> TestTree
requestDescribeFleetInstances =
  req
    "DescribeFleetInstances"
    "fixture/DescribeFleetInstances.yaml"

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

requestModifyInstanceEventWindow :: ModifyInstanceEventWindow -> TestTree
requestModifyInstanceEventWindow =
  req
    "ModifyInstanceEventWindow"
    "fixture/ModifyInstanceEventWindow.yaml"

requestMoveAddressToVpc :: MoveAddressToVpc -> TestTree
requestMoveAddressToVpc =
  req
    "MoveAddressToVpc"
    "fixture/MoveAddressToVpc.yaml"

requestAcceptTransitGatewayMulticastDomainAssociations :: AcceptTransitGatewayMulticastDomainAssociations -> TestTree
requestAcceptTransitGatewayMulticastDomainAssociations =
  req
    "AcceptTransitGatewayMulticastDomainAssociations"
    "fixture/AcceptTransitGatewayMulticastDomainAssociations.yaml"

requestRestoreAddressToClassic :: RestoreAddressToClassic -> TestTree
requestRestoreAddressToClassic =
  req
    "RestoreAddressToClassic"
    "fixture/RestoreAddressToClassic.yaml"

requestDescribeAvailabilityZones :: DescribeAvailabilityZones -> TestTree
requestDescribeAvailabilityZones =
  req
    "DescribeAvailabilityZones"
    "fixture/DescribeAvailabilityZones.yaml"

requestCreateStoreImageTask :: CreateStoreImageTask -> TestTree
requestCreateStoreImageTask =
  req
    "CreateStoreImageTask"
    "fixture/CreateStoreImageTask.yaml"

requestCopySnapshot :: CopySnapshot -> TestTree
requestCopySnapshot =
  req
    "CopySnapshot"
    "fixture/CopySnapshot.yaml"

requestDeleteNetworkInterfacePermission :: DeleteNetworkInterfacePermission -> TestTree
requestDeleteNetworkInterfacePermission =
  req
    "DeleteNetworkInterfacePermission"
    "fixture/DeleteNetworkInterfacePermission.yaml"

requestCreateFlowLogs :: CreateFlowLogs -> TestTree
requestCreateFlowLogs =
  req
    "CreateFlowLogs"
    "fixture/CreateFlowLogs.yaml"

requestDetachClassicLinkVpc :: DetachClassicLinkVpc -> TestTree
requestDetachClassicLinkVpc =
  req
    "DetachClassicLinkVpc"
    "fixture/DetachClassicLinkVpc.yaml"

requestDeleteRouteTable :: DeleteRouteTable -> TestTree
requestDeleteRouteTable =
  req
    "DeleteRouteTable"
    "fixture/DeleteRouteTable.yaml"

requestModifyVpnConnectionOptions :: ModifyVpnConnectionOptions -> TestTree
requestModifyVpnConnectionOptions =
  req
    "ModifyVpnConnectionOptions"
    "fixture/ModifyVpnConnectionOptions.yaml"

requestMonitorInstances :: MonitorInstances -> TestTree
requestMonitorInstances =
  req
    "MonitorInstances"
    "fixture/MonitorInstances.yaml"

requestModifyIdFormat :: ModifyIdFormat -> TestTree
requestModifyIdFormat =
  req
    "ModifyIdFormat"
    "fixture/ModifyIdFormat.yaml"

requestAllocateHosts :: AllocateHosts -> TestTree
requestAllocateHosts =
  req
    "AllocateHosts"
    "fixture/AllocateHosts.yaml"

requestDescribeImageAttribute :: DescribeImageAttribute -> TestTree
requestDescribeImageAttribute =
  req
    "DescribeImageAttribute"
    "fixture/DescribeImageAttribute.yaml"

requestDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations :: DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations -> TestTree
requestDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations =
  req
    "DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations"
    "fixture/DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations.yaml"

requestDescribeReservedInstancesModifications :: DescribeReservedInstancesModifications -> TestTree
requestDescribeReservedInstancesModifications =
  req
    "DescribeReservedInstancesModifications"
    "fixture/DescribeReservedInstancesModifications.yaml"

requestStartVpcEndpointServicePrivateDnsVerification :: StartVpcEndpointServicePrivateDnsVerification -> TestTree
requestStartVpcEndpointServicePrivateDnsVerification =
  req
    "StartVpcEndpointServicePrivateDnsVerification"
    "fixture/StartVpcEndpointServicePrivateDnsVerification.yaml"

requestCreateTrafficMirrorFilterRule :: CreateTrafficMirrorFilterRule -> TestTree
requestCreateTrafficMirrorFilterRule =
  req
    "CreateTrafficMirrorFilterRule"
    "fixture/CreateTrafficMirrorFilterRule.yaml"

requestGetEbsDefaultKmsKeyId :: GetEbsDefaultKmsKeyId -> TestTree
requestGetEbsDefaultKmsKeyId =
  req
    "GetEbsDefaultKmsKeyId"
    "fixture/GetEbsDefaultKmsKeyId.yaml"

requestDescribeClientVpnRoutes :: DescribeClientVpnRoutes -> TestTree
requestDescribeClientVpnRoutes =
  req
    "DescribeClientVpnRoutes"
    "fixture/DescribeClientVpnRoutes.yaml"

requestModifyVpnConnection :: ModifyVpnConnection -> TestTree
requestModifyVpnConnection =
  req
    "ModifyVpnConnection"
    "fixture/ModifyVpnConnection.yaml"

requestModifyFleet :: ModifyFleet -> TestTree
requestModifyFleet =
  req
    "ModifyFleet"
    "fixture/ModifyFleet.yaml"

requestRegisterImage :: RegisterImage -> TestTree
requestRegisterImage =
  req
    "RegisterImage"
    "fixture/RegisterImage.yaml"

requestRevokeClientVpnIngress :: RevokeClientVpnIngress -> TestTree
requestRevokeClientVpnIngress =
  req
    "RevokeClientVpnIngress"
    "fixture/RevokeClientVpnIngress.yaml"

requestUpdateSecurityGroupRuleDescriptionsEgress :: UpdateSecurityGroupRuleDescriptionsEgress -> TestTree
requestUpdateSecurityGroupRuleDescriptionsEgress =
  req
    "UpdateSecurityGroupRuleDescriptionsEgress"
    "fixture/UpdateSecurityGroupRuleDescriptionsEgress.yaml"

requestModifyVpcEndpoint :: ModifyVpcEndpoint -> TestTree
requestModifyVpcEndpoint =
  req
    "ModifyVpcEndpoint"
    "fixture/ModifyVpcEndpoint.yaml"

requestUnassignPrivateIpAddresses :: UnassignPrivateIpAddresses -> TestTree
requestUnassignPrivateIpAddresses =
  req
    "UnassignPrivateIpAddresses"
    "fixture/UnassignPrivateIpAddresses.yaml"

requestEnableFastSnapshotRestores :: EnableFastSnapshotRestores -> TestTree
requestEnableFastSnapshotRestores =
  req
    "EnableFastSnapshotRestores"
    "fixture/EnableFastSnapshotRestores.yaml"

requestCancelImportTask :: CancelImportTask -> TestTree
requestCancelImportTask =
  req
    "CancelImportTask"
    "fixture/CancelImportTask.yaml"

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

requestResetFpgaImageAttribute :: ResetFpgaImageAttribute -> TestTree
requestResetFpgaImageAttribute =
  req
    "ResetFpgaImageAttribute"
    "fixture/ResetFpgaImageAttribute.yaml"

requestGetConsoleOutput :: GetConsoleOutput -> TestTree
requestGetConsoleOutput =
  req
    "GetConsoleOutput"
    "fixture/GetConsoleOutput.yaml"

requestDeleteFpgaImage :: DeleteFpgaImage -> TestTree
requestDeleteFpgaImage =
  req
    "DeleteFpgaImage"
    "fixture/DeleteFpgaImage.yaml"

requestModifyReservedInstances :: ModifyReservedInstances -> TestTree
requestModifyReservedInstances =
  req
    "ModifyReservedInstances"
    "fixture/ModifyReservedInstances.yaml"

requestCreateRestoreImageTask :: CreateRestoreImageTask -> TestTree
requestCreateRestoreImageTask =
  req
    "CreateRestoreImageTask"
    "fixture/CreateRestoreImageTask.yaml"

requestDescribeSpotInstanceRequests :: DescribeSpotInstanceRequests -> TestTree
requestDescribeSpotInstanceRequests =
  req
    "DescribeSpotInstanceRequests"
    "fixture/DescribeSpotInstanceRequests.yaml"

requestModifyVpcEndpointServicePermissions :: ModifyVpcEndpointServicePermissions -> TestTree
requestModifyVpcEndpointServicePermissions =
  req
    "ModifyVpcEndpointServicePermissions"
    "fixture/ModifyVpcEndpointServicePermissions.yaml"

requestUnassignIpv6Addresses :: UnassignIpv6Addresses -> TestTree
requestUnassignIpv6Addresses =
  req
    "UnassignIpv6Addresses"
    "fixture/UnassignIpv6Addresses.yaml"

requestDescribeVolumesModifications :: DescribeVolumesModifications -> TestTree
requestDescribeVolumesModifications =
  req
    "DescribeVolumesModifications"
    "fixture/DescribeVolumesModifications.yaml"

requestDescribeIdFormat :: DescribeIdFormat -> TestTree
requestDescribeIdFormat =
  req
    "DescribeIdFormat"
    "fixture/DescribeIdFormat.yaml"

requestReportInstanceStatus :: ReportInstanceStatus -> TestTree
requestReportInstanceStatus =
  req
    "ReportInstanceStatus"
    "fixture/ReportInstanceStatus.yaml"

requestRunInstances :: RunInstances -> TestTree
requestRunInstances =
  req
    "RunInstances"
    "fixture/RunInstances.yaml"

requestModifyHosts :: ModifyHosts -> TestTree
requestModifyHosts =
  req
    "ModifyHosts"
    "fixture/ModifyHosts.yaml"

requestAttachVolume :: AttachVolume -> TestTree
requestAttachVolume =
  req
    "AttachVolume"
    "fixture/AttachVolume.yaml"

requestDescribeStoreImageTasks :: DescribeStoreImageTasks -> TestTree
requestDescribeStoreImageTasks =
  req
    "DescribeStoreImageTasks"
    "fixture/DescribeStoreImageTasks.yaml"

requestCreateReplaceRootVolumeTask :: CreateReplaceRootVolumeTask -> TestTree
requestCreateReplaceRootVolumeTask =
  req
    "CreateReplaceRootVolumeTask"
    "fixture/CreateReplaceRootVolumeTask.yaml"

requestDescribeAccountAttributes :: DescribeAccountAttributes -> TestTree
requestDescribeAccountAttributes =
  req
    "DescribeAccountAttributes"
    "fixture/DescribeAccountAttributes.yaml"

requestModifyImageAttribute :: ModifyImageAttribute -> TestTree
requestModifyImageAttribute =
  req
    "ModifyImageAttribute"
    "fixture/ModifyImageAttribute.yaml"

requestRegisterTransitGatewayMulticastGroupSources :: RegisterTransitGatewayMulticastGroupSources -> TestTree
requestRegisterTransitGatewayMulticastGroupSources =
  req
    "RegisterTransitGatewayMulticastGroupSources"
    "fixture/RegisterTransitGatewayMulticastGroupSources.yaml"

requestRebootInstances :: RebootInstances -> TestTree
requestRebootInstances =
  req
    "RebootInstances"
    "fixture/RebootInstances.yaml"

requestAssociateRouteTable :: AssociateRouteTable -> TestTree
requestAssociateRouteTable =
  req
    "AssociateRouteTable"
    "fixture/AssociateRouteTable.yaml"

requestAssociateIamInstanceProfile :: AssociateIamInstanceProfile -> TestTree
requestAssociateIamInstanceProfile =
  req
    "AssociateIamInstanceProfile"
    "fixture/AssociateIamInstanceProfile.yaml"

requestPurchaseHostReservation :: PurchaseHostReservation -> TestTree
requestPurchaseHostReservation =
  req
    "PurchaseHostReservation"
    "fixture/PurchaseHostReservation.yaml"

requestDescribeTrafficMirrorTargets :: DescribeTrafficMirrorTargets -> TestTree
requestDescribeTrafficMirrorTargets =
  req
    "DescribeTrafficMirrorTargets"
    "fixture/DescribeTrafficMirrorTargets.yaml"

requestGetManagedPrefixListAssociations :: GetManagedPrefixListAssociations -> TestTree
requestGetManagedPrefixListAssociations =
  req
    "GetManagedPrefixListAssociations"
    "fixture/GetManagedPrefixListAssociations.yaml"

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

requestCreateDhcpOptions :: CreateDhcpOptions -> TestTree
requestCreateDhcpOptions =
  req
    "CreateDhcpOptions"
    "fixture/CreateDhcpOptions.yaml"

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

requestDescribeVpcs :: DescribeVpcs -> TestTree
requestDescribeVpcs =
  req
    "DescribeVpcs"
    "fixture/DescribeVpcs.yaml"

requestDescribeLocalGateways :: DescribeLocalGateways -> TestTree
requestDescribeLocalGateways =
  req
    "DescribeLocalGateways"
    "fixture/DescribeLocalGateways.yaml"

requestDescribeIpv6Pools :: DescribeIpv6Pools -> TestTree
requestDescribeIpv6Pools =
  req
    "DescribeIpv6Pools"
    "fixture/DescribeIpv6Pools.yaml"

requestCreateRouteTable :: CreateRouteTable -> TestTree
requestCreateRouteTable =
  req
    "CreateRouteTable"
    "fixture/CreateRouteTable.yaml"

requestDescribeVpcEndpointConnectionNotifications :: DescribeVpcEndpointConnectionNotifications -> TestTree
requestDescribeVpcEndpointConnectionNotifications =
  req
    "DescribeVpcEndpointConnectionNotifications"
    "fixture/DescribeVpcEndpointConnectionNotifications.yaml"

requestGetTransitGatewayPrefixListReferences :: GetTransitGatewayPrefixListReferences -> TestTree
requestGetTransitGatewayPrefixListReferences =
  req
    "GetTransitGatewayPrefixListReferences"
    "fixture/GetTransitGatewayPrefixListReferences.yaml"

requestAcceptVpcEndpointConnections :: AcceptVpcEndpointConnections -> TestTree
requestAcceptVpcEndpointConnections =
  req
    "AcceptVpcEndpointConnections"
    "fixture/AcceptVpcEndpointConnections.yaml"

requestGetTransitGatewayRouteTablePropagations :: GetTransitGatewayRouteTablePropagations -> TestTree
requestGetTransitGatewayRouteTablePropagations =
  req
    "GetTransitGatewayRouteTablePropagations"
    "fixture/GetTransitGatewayRouteTablePropagations.yaml"

requestAssociateDhcpOptions :: AssociateDhcpOptions -> TestTree
requestAssociateDhcpOptions =
  req
    "AssociateDhcpOptions"
    "fixture/AssociateDhcpOptions.yaml"

requestDeleteEgressOnlyInternetGateway :: DeleteEgressOnlyInternetGateway -> TestTree
requestDeleteEgressOnlyInternetGateway =
  req
    "DeleteEgressOnlyInternetGateway"
    "fixture/DeleteEgressOnlyInternetGateway.yaml"

requestGetVpnConnectionDeviceTypes :: GetVpnConnectionDeviceTypes -> TestTree
requestGetVpnConnectionDeviceTypes =
  req
    "GetVpnConnectionDeviceTypes"
    "fixture/GetVpnConnectionDeviceTypes.yaml"

requestCreateSubnetCidrReservation :: CreateSubnetCidrReservation -> TestTree
requestCreateSubnetCidrReservation =
  req
    "CreateSubnetCidrReservation"
    "fixture/CreateSubnetCidrReservation.yaml"

requestDisableFastSnapshotRestores :: DisableFastSnapshotRestores -> TestTree
requestDisableFastSnapshotRestores =
  req
    "DisableFastSnapshotRestores"
    "fixture/DisableFastSnapshotRestores.yaml"

requestRequestSpotInstances :: RequestSpotInstances -> TestTree
requestRequestSpotInstances =
  req
    "RequestSpotInstances"
    "fixture/RequestSpotInstances.yaml"

requestDescribeLaunchTemplates :: DescribeLaunchTemplates -> TestTree
requestDescribeLaunchTemplates =
  req
    "DescribeLaunchTemplates"
    "fixture/DescribeLaunchTemplates.yaml"

requestCreateImage :: CreateImage -> TestTree
requestCreateImage =
  req
    "CreateImage"
    "fixture/CreateImage.yaml"

requestModifyTransitGatewayVpcAttachment :: ModifyTransitGatewayVpcAttachment -> TestTree
requestModifyTransitGatewayVpcAttachment =
  req
    "ModifyTransitGatewayVpcAttachment"
    "fixture/ModifyTransitGatewayVpcAttachment.yaml"

requestAssignIpv6Addresses :: AssignIpv6Addresses -> TestTree
requestAssignIpv6Addresses =
  req
    "AssignIpv6Addresses"
    "fixture/AssignIpv6Addresses.yaml"

requestDescribeLocalGatewayVirtualInterfaceGroups :: DescribeLocalGatewayVirtualInterfaceGroups -> TestTree
requestDescribeLocalGatewayVirtualInterfaceGroups =
  req
    "DescribeLocalGatewayVirtualInterfaceGroups"
    "fixture/DescribeLocalGatewayVirtualInterfaceGroups.yaml"

requestDescribeVpnConnections :: DescribeVpnConnections -> TestTree
requestDescribeVpnConnections =
  req
    "DescribeVpnConnections"
    "fixture/DescribeVpnConnections.yaml"

requestCreateNetworkAclEntry :: CreateNetworkAclEntry -> TestTree
requestCreateNetworkAclEntry =
  req
    "CreateNetworkAclEntry"
    "fixture/CreateNetworkAclEntry.yaml"

requestDescribePlacementGroups :: DescribePlacementGroups -> TestTree
requestDescribePlacementGroups =
  req
    "DescribePlacementGroups"
    "fixture/DescribePlacementGroups.yaml"

requestModifySnapshotAttribute :: ModifySnapshotAttribute -> TestTree
requestModifySnapshotAttribute =
  req
    "ModifySnapshotAttribute"
    "fixture/ModifySnapshotAttribute.yaml"

requestModifyIdentityIdFormat :: ModifyIdentityIdFormat -> TestTree
requestModifyIdentityIdFormat =
  req
    "ModifyIdentityIdFormat"
    "fixture/ModifyIdentityIdFormat.yaml"

requestEnableVgwRoutePropagation :: EnableVgwRoutePropagation -> TestTree
requestEnableVgwRoutePropagation =
  req
    "EnableVgwRoutePropagation"
    "fixture/EnableVgwRoutePropagation.yaml"

requestDeleteTags :: DeleteTags -> TestTree
requestDeleteTags =
  req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

requestResetInstanceAttribute :: ResetInstanceAttribute -> TestTree
requestResetInstanceAttribute =
  req
    "ResetInstanceAttribute"
    "fixture/ResetInstanceAttribute.yaml"

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

requestDescribeFlowLogs :: DescribeFlowLogs -> TestTree
requestDescribeFlowLogs =
  req
    "DescribeFlowLogs"
    "fixture/DescribeFlowLogs.yaml"

requestDescribeReservedInstancesOfferings :: DescribeReservedInstancesOfferings -> TestTree
requestDescribeReservedInstancesOfferings =
  req
    "DescribeReservedInstancesOfferings"
    "fixture/DescribeReservedInstancesOfferings.yaml"

requestDescribeFleets :: DescribeFleets -> TestTree
requestDescribeFleets =
  req
    "DescribeFleets"
    "fixture/DescribeFleets.yaml"

requestAttachNetworkInterface :: AttachNetworkInterface -> TestTree
requestAttachNetworkInterface =
  req
    "AttachNetworkInterface"
    "fixture/AttachNetworkInterface.yaml"

requestConfirmProductInstance :: ConfirmProductInstance -> TestTree
requestConfirmProductInstance =
  req
    "ConfirmProductInstance"
    "fixture/ConfirmProductInstance.yaml"

requestDescribeTransitGatewayAttachments :: DescribeTransitGatewayAttachments -> TestTree
requestDescribeTransitGatewayAttachments =
  req
    "DescribeTransitGatewayAttachments"
    "fixture/DescribeTransitGatewayAttachments.yaml"

requestModifyAvailabilityZoneGroup :: ModifyAvailabilityZoneGroup -> TestTree
requestModifyAvailabilityZoneGroup =
  req
    "ModifyAvailabilityZoneGroup"
    "fixture/ModifyAvailabilityZoneGroup.yaml"

requestReplaceNetworkAclEntry :: ReplaceNetworkAclEntry -> TestTree
requestReplaceNetworkAclEntry =
  req
    "ReplaceNetworkAclEntry"
    "fixture/ReplaceNetworkAclEntry.yaml"

requestDescribeSpotFleetRequestHistory :: DescribeSpotFleetRequestHistory -> TestTree
requestDescribeSpotFleetRequestHistory =
  req
    "DescribeSpotFleetRequestHistory"
    "fixture/DescribeSpotFleetRequestHistory.yaml"

requestCreateLocalGatewayRoute :: CreateLocalGatewayRoute -> TestTree
requestCreateLocalGatewayRoute =
  req
    "CreateLocalGatewayRoute"
    "fixture/CreateLocalGatewayRoute.yaml"

requestDescribeVpcEndpoints :: DescribeVpcEndpoints -> TestTree
requestDescribeVpcEndpoints =
  req
    "DescribeVpcEndpoints"
    "fixture/DescribeVpcEndpoints.yaml"

requestModifyInstanceEventStartTime :: ModifyInstanceEventStartTime -> TestTree
requestModifyInstanceEventStartTime =
  req
    "ModifyInstanceEventStartTime"
    "fixture/ModifyInstanceEventStartTime.yaml"

requestDescribeStaleSecurityGroups :: DescribeStaleSecurityGroups -> TestTree
requestDescribeStaleSecurityGroups =
  req
    "DescribeStaleSecurityGroups"
    "fixture/DescribeStaleSecurityGroups.yaml"

requestDescribeInstanceStatus :: DescribeInstanceStatus -> TestTree
requestDescribeInstanceStatus =
  req
    "DescribeInstanceStatus"
    "fixture/DescribeInstanceStatus.yaml"

requestDeleteNetworkAclEntry :: DeleteNetworkAclEntry -> TestTree
requestDeleteNetworkAclEntry =
  req
    "DeleteNetworkAclEntry"
    "fixture/DeleteNetworkAclEntry.yaml"

requestGetConsoleScreenshot :: GetConsoleScreenshot -> TestTree
requestGetConsoleScreenshot =
  req
    "GetConsoleScreenshot"
    "fixture/GetConsoleScreenshot.yaml"

requestGetGroupsForCapacityReservation :: GetGroupsForCapacityReservation -> TestTree
requestGetGroupsForCapacityReservation =
  req
    "GetGroupsForCapacityReservation"
    "fixture/GetGroupsForCapacityReservation.yaml"

requestDisassociateIamInstanceProfile :: DisassociateIamInstanceProfile -> TestTree
requestDisassociateIamInstanceProfile =
  req
    "DisassociateIamInstanceProfile"
    "fixture/DisassociateIamInstanceProfile.yaml"

requestDescribeVpcEndpointServiceConfigurations :: DescribeVpcEndpointServiceConfigurations -> TestTree
requestDescribeVpcEndpointServiceConfigurations =
  req
    "DescribeVpcEndpointServiceConfigurations"
    "fixture/DescribeVpcEndpointServiceConfigurations.yaml"

requestCancelSpotInstanceRequests :: CancelSpotInstanceRequests -> TestTree
requestCancelSpotInstanceRequests =
  req
    "CancelSpotInstanceRequests"
    "fixture/CancelSpotInstanceRequests.yaml"

requestDeleteLocalGatewayRoute :: DeleteLocalGatewayRoute -> TestTree
requestDeleteLocalGatewayRoute =
  req
    "DeleteLocalGatewayRoute"
    "fixture/DeleteLocalGatewayRoute.yaml"

requestDescribeVpcEndpointServices :: DescribeVpcEndpointServices -> TestTree
requestDescribeVpcEndpointServices =
  req
    "DescribeVpcEndpointServices"
    "fixture/DescribeVpcEndpointServices.yaml"

requestDisassociateRouteTable :: DisassociateRouteTable -> TestTree
requestDisassociateRouteTable =
  req
    "DisassociateRouteTable"
    "fixture/DisassociateRouteTable.yaml"

requestAssignPrivateIpAddresses :: AssignPrivateIpAddresses -> TestTree
requestAssignPrivateIpAddresses =
  req
    "AssignPrivateIpAddresses"
    "fixture/AssignPrivateIpAddresses.yaml"

requestGetFlowLogsIntegrationTemplate :: GetFlowLogsIntegrationTemplate -> TestTree
requestGetFlowLogsIntegrationTemplate =
  req
    "GetFlowLogsIntegrationTemplate"
    "fixture/GetFlowLogsIntegrationTemplate.yaml"

requestModifyVpnTunnelCertificate :: ModifyVpnTunnelCertificate -> TestTree
requestModifyVpnTunnelCertificate =
  req
    "ModifyVpnTunnelCertificate"
    "fixture/ModifyVpnTunnelCertificate.yaml"

requestDisableVgwRoutePropagation :: DisableVgwRoutePropagation -> TestTree
requestDisableVgwRoutePropagation =
  req
    "DisableVgwRoutePropagation"
    "fixture/DisableVgwRoutePropagation.yaml"

requestDeleteSnapshot :: DeleteSnapshot -> TestTree
requestDeleteSnapshot =
  req
    "DeleteSnapshot"
    "fixture/DeleteSnapshot.yaml"

requestDescribeSubnets :: DescribeSubnets -> TestTree
requestDescribeSubnets =
  req
    "DescribeSubnets"
    "fixture/DescribeSubnets.yaml"

requestCreateSpotDatafeedSubscription :: CreateSpotDatafeedSubscription -> TestTree
requestCreateSpotDatafeedSubscription =
  req
    "CreateSpotDatafeedSubscription"
    "fixture/CreateSpotDatafeedSubscription.yaml"

requestUnmonitorInstances :: UnmonitorInstances -> TestTree
requestUnmonitorInstances =
  req
    "UnmonitorInstances"
    "fixture/UnmonitorInstances.yaml"

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

requestPurchaseReservedInstancesOffering :: PurchaseReservedInstancesOffering -> TestTree
requestPurchaseReservedInstancesOffering =
  req
    "PurchaseReservedInstancesOffering"
    "fixture/PurchaseReservedInstancesOffering.yaml"

requestDescribeSnapshotAttribute :: DescribeSnapshotAttribute -> TestTree
requestDescribeSnapshotAttribute =
  req
    "DescribeSnapshotAttribute"
    "fixture/DescribeSnapshotAttribute.yaml"

requestAuthorizeSecurityGroupIngress :: AuthorizeSecurityGroupIngress -> TestTree
requestAuthorizeSecurityGroupIngress =
  req
    "AuthorizeSecurityGroupIngress"
    "fixture/AuthorizeSecurityGroupIngress.yaml"

requestDescribeNatGateways :: DescribeNatGateways -> TestTree
requestDescribeNatGateways =
  req
    "DescribeNatGateways"
    "fixture/DescribeNatGateways.yaml"

requestDisableVpcClassicLink :: DisableVpcClassicLink -> TestTree
requestDisableVpcClassicLink =
  req
    "DisableVpcClassicLink"
    "fixture/DisableVpcClassicLink.yaml"

requestDescribeTransitGatewayMulticastDomains :: DescribeTransitGatewayMulticastDomains -> TestTree
requestDescribeTransitGatewayMulticastDomains =
  req
    "DescribeTransitGatewayMulticastDomains"
    "fixture/DescribeTransitGatewayMulticastDomains.yaml"

requestGetTransitGatewayAttachmentPropagations :: GetTransitGatewayAttachmentPropagations -> TestTree
requestGetTransitGatewayAttachmentPropagations =
  req
    "GetTransitGatewayAttachmentPropagations"
    "fixture/GetTransitGatewayAttachmentPropagations.yaml"

requestModifyVpcEndpointConnectionNotification :: ModifyVpcEndpointConnectionNotification -> TestTree
requestModifyVpcEndpointConnectionNotification =
  req
    "ModifyVpcEndpointConnectionNotification"
    "fixture/ModifyVpcEndpointConnectionNotification.yaml"

requestRestoreManagedPrefixListVersion :: RestoreManagedPrefixListVersion -> TestTree
requestRestoreManagedPrefixListVersion =
  req
    "RestoreManagedPrefixListVersion"
    "fixture/RestoreManagedPrefixListVersion.yaml"

requestDescribeTransitGatewayConnectPeers :: DescribeTransitGatewayConnectPeers -> TestTree
requestDescribeTransitGatewayConnectPeers =
  req
    "DescribeTransitGatewayConnectPeers"
    "fixture/DescribeTransitGatewayConnectPeers.yaml"

requestDeleteCarrierGateway :: DeleteCarrierGateway -> TestTree
requestDeleteCarrierGateway =
  req
    "DeleteCarrierGateway"
    "fixture/DeleteCarrierGateway.yaml"

requestDescribeNetworkInterfaces :: DescribeNetworkInterfaces -> TestTree
requestDescribeNetworkInterfaces =
  req
    "DescribeNetworkInterfaces"
    "fixture/DescribeNetworkInterfaces.yaml"

requestDescribeTransitGatewayVpcAttachments :: DescribeTransitGatewayVpcAttachments -> TestTree
requestDescribeTransitGatewayVpcAttachments =
  req
    "DescribeTransitGatewayVpcAttachments"
    "fixture/DescribeTransitGatewayVpcAttachments.yaml"

requestModifyAddressAttribute :: ModifyAddressAttribute -> TestTree
requestModifyAddressAttribute =
  req
    "ModifyAddressAttribute"
    "fixture/ModifyAddressAttribute.yaml"

requestDescribeImportSnapshotTasks :: DescribeImportSnapshotTasks -> TestTree
requestDescribeImportSnapshotTasks =
  req
    "DescribeImportSnapshotTasks"
    "fixture/DescribeImportSnapshotTasks.yaml"

requestCopyImage :: CopyImage -> TestTree
requestCopyImage =
  req
    "CopyImage"
    "fixture/CopyImage.yaml"

requestDescribeInstanceEventNotificationAttributes :: DescribeInstanceEventNotificationAttributes -> TestTree
requestDescribeInstanceEventNotificationAttributes =
  req
    "DescribeInstanceEventNotificationAttributes"
    "fixture/DescribeInstanceEventNotificationAttributes.yaml"

requestEnableSerialConsoleAccess :: EnableSerialConsoleAccess -> TestTree
requestEnableSerialConsoleAccess =
  req
    "EnableSerialConsoleAccess"
    "fixture/EnableSerialConsoleAccess.yaml"

requestModifyTrafficMirrorFilterRule :: ModifyTrafficMirrorFilterRule -> TestTree
requestModifyTrafficMirrorFilterRule =
  req
    "ModifyTrafficMirrorFilterRule"
    "fixture/ModifyTrafficMirrorFilterRule.yaml"

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

requestModifyInstanceCapacityReservationAttributes :: ModifyInstanceCapacityReservationAttributes -> TestTree
requestModifyInstanceCapacityReservationAttributes =
  req
    "ModifyInstanceCapacityReservationAttributes"
    "fixture/ModifyInstanceCapacityReservationAttributes.yaml"

requestDescribeNetworkInterfaceAttribute :: DescribeNetworkInterfaceAttribute -> TestTree
requestDescribeNetworkInterfaceAttribute =
  req
    "DescribeNetworkInterfaceAttribute"
    "fixture/DescribeNetworkInterfaceAttribute.yaml"

requestAttachClassicLinkVpc :: AttachClassicLinkVpc -> TestTree
requestAttachClassicLinkVpc =
  req
    "AttachClassicLinkVpc"
    "fixture/AttachClassicLinkVpc.yaml"

requestGetSubnetCidrReservations :: GetSubnetCidrReservations -> TestTree
requestGetSubnetCidrReservations =
  req
    "GetSubnetCidrReservations"
    "fixture/GetSubnetCidrReservations.yaml"

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

requestDisableTransitGatewayRouteTablePropagation :: DisableTransitGatewayRouteTablePropagation -> TestTree
requestDisableTransitGatewayRouteTablePropagation =
  req
    "DisableTransitGatewayRouteTablePropagation"
    "fixture/DisableTransitGatewayRouteTablePropagation.yaml"

requestCancelReservedInstancesListing :: CancelReservedInstancesListing -> TestTree
requestCancelReservedInstancesListing =
  req
    "CancelReservedInstancesListing"
    "fixture/CancelReservedInstancesListing.yaml"

requestDeleteQueuedReservedInstances :: DeleteQueuedReservedInstances -> TestTree
requestDeleteQueuedReservedInstances =
  req
    "DeleteQueuedReservedInstances"
    "fixture/DeleteQueuedReservedInstances.yaml"

requestCreateFleet :: CreateFleet -> TestTree
requestCreateFleet =
  req
    "CreateFleet"
    "fixture/CreateFleet.yaml"

requestDescribeSnapshots :: DescribeSnapshots -> TestTree
requestDescribeSnapshots =
  req
    "DescribeSnapshots"
    "fixture/DescribeSnapshots.yaml"

requestDescribeReservedInstancesListings :: DescribeReservedInstancesListings -> TestTree
requestDescribeReservedInstancesListings =
  req
    "DescribeReservedInstancesListings"
    "fixture/DescribeReservedInstancesListings.yaml"

requestModifyClientVpnEndpoint :: ModifyClientVpnEndpoint -> TestTree
requestModifyClientVpnEndpoint =
  req
    "ModifyClientVpnEndpoint"
    "fixture/ModifyClientVpnEndpoint.yaml"

requestCreateVpcEndpoint :: CreateVpcEndpoint -> TestTree
requestCreateVpcEndpoint =
  req
    "CreateVpcEndpoint"
    "fixture/CreateVpcEndpoint.yaml"

requestCreateVpnConnection :: CreateVpnConnection -> TestTree
requestCreateVpnConnection =
  req
    "CreateVpnConnection"
    "fixture/CreateVpnConnection.yaml"

requestImportClientVpnClientCertificateRevocationList :: ImportClientVpnClientCertificateRevocationList -> TestTree
requestImportClientVpnClientCertificateRevocationList =
  req
    "ImportClientVpnClientCertificateRevocationList"
    "fixture/ImportClientVpnClientCertificateRevocationList.yaml"

requestAssociateSubnetCidrBlock :: AssociateSubnetCidrBlock -> TestTree
requestAssociateSubnetCidrBlock =
  req
    "AssociateSubnetCidrBlock"
    "fixture/AssociateSubnetCidrBlock.yaml"

requestDescribeSpotPriceHistory :: DescribeSpotPriceHistory -> TestTree
requestDescribeSpotPriceHistory =
  req
    "DescribeSpotPriceHistory"
    "fixture/DescribeSpotPriceHistory.yaml"

requestEnableEbsEncryptionByDefault :: EnableEbsEncryptionByDefault -> TestTree
requestEnableEbsEncryptionByDefault =
  req
    "EnableEbsEncryptionByDefault"
    "fixture/EnableEbsEncryptionByDefault.yaml"

requestDescribeVpcClassicLinkDnsSupport :: DescribeVpcClassicLinkDnsSupport -> TestTree
requestDescribeVpcClassicLinkDnsSupport =
  req
    "DescribeVpcClassicLinkDnsSupport"
    "fixture/DescribeVpcClassicLinkDnsSupport.yaml"

requestCreateLocalGatewayRouteTableVpcAssociation :: CreateLocalGatewayRouteTableVpcAssociation -> TestTree
requestCreateLocalGatewayRouteTableVpcAssociation =
  req
    "CreateLocalGatewayRouteTableVpcAssociation"
    "fixture/CreateLocalGatewayRouteTableVpcAssociation.yaml"

requestDescribeAggregateIdFormat :: DescribeAggregateIdFormat -> TestTree
requestDescribeAggregateIdFormat =
  req
    "DescribeAggregateIdFormat"
    "fixture/DescribeAggregateIdFormat.yaml"

requestEnableTransitGatewayRouteTablePropagation :: EnableTransitGatewayRouteTablePropagation -> TestTree
requestEnableTransitGatewayRouteTablePropagation =
  req
    "EnableTransitGatewayRouteTablePropagation"
    "fixture/EnableTransitGatewayRouteTablePropagation.yaml"

requestRequestSpotFleet :: RequestSpotFleet -> TestTree
requestRequestSpotFleet =
  req
    "RequestSpotFleet"
    "fixture/RequestSpotFleet.yaml"

requestDescribeBundleTasks :: DescribeBundleTasks -> TestTree
requestDescribeBundleTasks =
  req
    "DescribeBundleTasks"
    "fixture/DescribeBundleTasks.yaml"

requestModifyNetworkInterfaceAttribute :: ModifyNetworkInterfaceAttribute -> TestTree
requestModifyNetworkInterfaceAttribute =
  req
    "ModifyNetworkInterfaceAttribute"
    "fixture/ModifyNetworkInterfaceAttribute.yaml"

requestDisableSerialConsoleAccess :: DisableSerialConsoleAccess -> TestTree
requestDisableSerialConsoleAccess =
  req
    "DisableSerialConsoleAccess"
    "fixture/DisableSerialConsoleAccess.yaml"

requestDescribeInstanceTypeOfferings :: DescribeInstanceTypeOfferings -> TestTree
requestDescribeInstanceTypeOfferings =
  req
    "DescribeInstanceTypeOfferings"
    "fixture/DescribeInstanceTypeOfferings.yaml"

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

requestResetAddressAttribute :: ResetAddressAttribute -> TestTree
requestResetAddressAttribute =
  req
    "ResetAddressAttribute"
    "fixture/ResetAddressAttribute.yaml"

requestDescribeCoipPools :: DescribeCoipPools -> TestTree
requestDescribeCoipPools =
  req
    "DescribeCoipPools"
    "fixture/DescribeCoipPools.yaml"

requestDescribeInstanceEventWindows :: DescribeInstanceEventWindows -> TestTree
requestDescribeInstanceEventWindows =
  req
    "DescribeInstanceEventWindows"
    "fixture/DescribeInstanceEventWindows.yaml"

requestCreateSubnet :: CreateSubnet -> TestTree
requestCreateSubnet =
  req
    "CreateSubnet"
    "fixture/CreateSubnet.yaml"

requestDescribeSecurityGroups :: DescribeSecurityGroups -> TestTree
requestDescribeSecurityGroups =
  req
    "DescribeSecurityGroups"
    "fixture/DescribeSecurityGroups.yaml"

requestDeletePlacementGroup :: DeletePlacementGroup -> TestTree
requestDeletePlacementGroup =
  req
    "DeletePlacementGroup"
    "fixture/DeletePlacementGroup.yaml"

requestEnableVolumeIO :: EnableVolumeIO -> TestTree
requestEnableVolumeIO =
  req
    "EnableVolumeIO"
    "fixture/EnableVolumeIO.yaml"

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

requestModifySubnetAttribute :: ModifySubnetAttribute -> TestTree
requestModifySubnetAttribute =
  req
    "ModifySubnetAttribute"
    "fixture/ModifySubnetAttribute.yaml"

requestDescribeTransitGatewayConnects :: DescribeTransitGatewayConnects -> TestTree
requestDescribeTransitGatewayConnects =
  req
    "DescribeTransitGatewayConnects"
    "fixture/DescribeTransitGatewayConnects.yaml"

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

requestDeleteVpnConnection :: DeleteVpnConnection -> TestTree
requestDeleteVpnConnection =
  req
    "DeleteVpnConnection"
    "fixture/DeleteVpnConnection.yaml"

requestDescribeInstanceTypes :: DescribeInstanceTypes -> TestTree
requestDescribeInstanceTypes =
  req
    "DescribeInstanceTypes"
    "fixture/DescribeInstanceTypes.yaml"

requestCancelExportTask :: CancelExportTask -> TestTree
requestCancelExportTask =
  req
    "CancelExportTask"
    "fixture/CancelExportTask.yaml"

requestModifyTransitGateway :: ModifyTransitGateway -> TestTree
requestModifyTransitGateway =
  req
    "ModifyTransitGateway"
    "fixture/ModifyTransitGateway.yaml"

requestCreateInternetGateway :: CreateInternetGateway -> TestTree
requestCreateInternetGateway =
  req
    "CreateInternetGateway"
    "fixture/CreateInternetGateway.yaml"

requestSendDiagnosticInterrupt :: SendDiagnosticInterrupt -> TestTree
requestSendDiagnosticInterrupt =
  req
    "SendDiagnosticInterrupt"
    "fixture/SendDiagnosticInterrupt.yaml"

requestDisassociateClientVpnTargetNetwork :: DisassociateClientVpnTargetNetwork -> TestTree
requestDisassociateClientVpnTargetNetwork =
  req
    "DisassociateClientVpnTargetNetwork"
    "fixture/DisassociateClientVpnTargetNetwork.yaml"

requestModifyInstanceMetadataOptions :: ModifyInstanceMetadataOptions -> TestTree
requestModifyInstanceMetadataOptions =
  req
    "ModifyInstanceMetadataOptions"
    "fixture/ModifyInstanceMetadataOptions.yaml"

requestDescribeSpotDatafeedSubscription :: DescribeSpotDatafeedSubscription -> TestTree
requestDescribeSpotDatafeedSubscription =
  req
    "DescribeSpotDatafeedSubscription"
    "fixture/DescribeSpotDatafeedSubscription.yaml"

requestExportClientVpnClientConfiguration :: ExportClientVpnClientConfiguration -> TestTree
requestExportClientVpnClientConfiguration =
  req
    "ExportClientVpnClientConfiguration"
    "fixture/ExportClientVpnClientConfiguration.yaml"

requestDeleteKeyPair :: DeleteKeyPair -> TestTree
requestDeleteKeyPair =
  req
    "DeleteKeyPair"
    "fixture/DeleteKeyPair.yaml"

requestDescribeEgressOnlyInternetGateways :: DescribeEgressOnlyInternetGateways -> TestTree
requestDescribeEgressOnlyInternetGateways =
  req
    "DescribeEgressOnlyInternetGateways"
    "fixture/DescribeEgressOnlyInternetGateways.yaml"

requestCreateVolume :: CreateVolume -> TestTree
requestCreateVolume =
  req
    "CreateVolume"
    "fixture/CreateVolume.yaml"

requestModifyTrafficMirrorFilterNetworkServices :: ModifyTrafficMirrorFilterNetworkServices -> TestTree
requestModifyTrafficMirrorFilterNetworkServices =
  req
    "ModifyTrafficMirrorFilterNetworkServices"
    "fixture/ModifyTrafficMirrorFilterNetworkServices.yaml"

requestDescribeVpcAttribute :: DescribeVpcAttribute -> TestTree
requestDescribeVpcAttribute =
  req
    "DescribeVpcAttribute"
    "fixture/DescribeVpcAttribute.yaml"

requestDescribeTrunkInterfaceAssociations :: DescribeTrunkInterfaceAssociations -> TestTree
requestDescribeTrunkInterfaceAssociations =
  req
    "DescribeTrunkInterfaceAssociations"
    "fixture/DescribeTrunkInterfaceAssociations.yaml"

requestCreateInstanceExportTask :: CreateInstanceExportTask -> TestTree
requestCreateInstanceExportTask =
  req
    "CreateInstanceExportTask"
    "fixture/CreateInstanceExportTask.yaml"

requestCreateClientVpnRoute :: CreateClientVpnRoute -> TestTree
requestCreateClientVpnRoute =
  req
    "CreateClientVpnRoute"
    "fixture/CreateClientVpnRoute.yaml"

requestModifyCapacityReservation :: ModifyCapacityReservation -> TestTree
requestModifyCapacityReservation =
  req
    "ModifyCapacityReservation"
    "fixture/ModifyCapacityReservation.yaml"

requestRevokeSecurityGroupEgress :: RevokeSecurityGroupEgress -> TestTree
requestRevokeSecurityGroupEgress =
  req
    "RevokeSecurityGroupEgress"
    "fixture/RevokeSecurityGroupEgress.yaml"

requestDescribeSecurityGroupReferences :: DescribeSecurityGroupReferences -> TestTree
requestDescribeSecurityGroupReferences =
  req
    "DescribeSecurityGroupReferences"
    "fixture/DescribeSecurityGroupReferences.yaml"

requestDisassociateSubnetCidrBlock :: DisassociateSubnetCidrBlock -> TestTree
requestDisassociateSubnetCidrBlock =
  req
    "DisassociateSubnetCidrBlock"
    "fixture/DisassociateSubnetCidrBlock.yaml"

requestDetachInternetGateway :: DetachInternetGateway -> TestTree
requestDetachInternetGateway =
  req
    "DetachInternetGateway"
    "fixture/DetachInternetGateway.yaml"

requestSearchTransitGatewayMulticastGroups :: SearchTransitGatewayMulticastGroups -> TestTree
requestSearchTransitGatewayMulticastGroups =
  req
    "SearchTransitGatewayMulticastGroups"
    "fixture/SearchTransitGatewayMulticastGroups.yaml"

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

requestModifyVpcAttribute :: ModifyVpcAttribute -> TestTree
requestModifyVpcAttribute =
  req
    "ModifyVpcAttribute"
    "fixture/ModifyVpcAttribute.yaml"

requestDeleteClientVpnRoute :: DeleteClientVpnRoute -> TestTree
requestDeleteClientVpnRoute =
  req
    "DeleteClientVpnRoute"
    "fixture/DeleteClientVpnRoute.yaml"

requestDeprovisionByoipCidr :: DeprovisionByoipCidr -> TestTree
requestDeprovisionByoipCidr =
  req
    "DeprovisionByoipCidr"
    "fixture/DeprovisionByoipCidr.yaml"

requestDisassociateTrunkInterface :: DisassociateTrunkInterface -> TestTree
requestDisassociateTrunkInterface =
  req
    "DisassociateTrunkInterface"
    "fixture/DisassociateTrunkInterface.yaml"

requestImportSnapshot :: ImportSnapshot -> TestTree
requestImportSnapshot =
  req
    "ImportSnapshot"
    "fixture/ImportSnapshot.yaml"

requestDescribeSpotFleetInstances :: DescribeSpotFleetInstances -> TestTree
requestDescribeSpotFleetInstances =
  req
    "DescribeSpotFleetInstances"
    "fixture/DescribeSpotFleetInstances.yaml"

requestDescribeClientVpnConnections :: DescribeClientVpnConnections -> TestTree
requestDescribeClientVpnConnections =
  req
    "DescribeClientVpnConnections"
    "fixture/DescribeClientVpnConnections.yaml"

requestCreateTrafficMirrorTarget :: CreateTrafficMirrorTarget -> TestTree
requestCreateTrafficMirrorTarget =
  req
    "CreateTrafficMirrorTarget"
    "fixture/CreateTrafficMirrorTarget.yaml"

requestModifyInstanceCreditSpecification :: ModifyInstanceCreditSpecification -> TestTree
requestModifyInstanceCreditSpecification =
  req
    "ModifyInstanceCreditSpecification"
    "fixture/ModifyInstanceCreditSpecification.yaml"

requestAcceptVpcPeeringConnection :: AcceptVpcPeeringConnection -> TestTree
requestAcceptVpcPeeringConnection =
  req
    "AcceptVpcPeeringConnection"
    "fixture/AcceptVpcPeeringConnection.yaml"

requestDescribeVolumeAttribute :: DescribeVolumeAttribute -> TestTree
requestDescribeVolumeAttribute =
  req
    "DescribeVolumeAttribute"
    "fixture/DescribeVolumeAttribute.yaml"

requestDescribeSpotFleetRequests :: DescribeSpotFleetRequests -> TestTree
requestDescribeSpotFleetRequests =
  req
    "DescribeSpotFleetRequests"
    "fixture/DescribeSpotFleetRequests.yaml"

requestDescribeAddressesAttribute :: DescribeAddressesAttribute -> TestTree
requestDescribeAddressesAttribute =
  req
    "DescribeAddressesAttribute"
    "fixture/DescribeAddressesAttribute.yaml"

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

requestModifyVpcPeeringConnectionOptions :: ModifyVpcPeeringConnectionOptions -> TestTree
requestModifyVpcPeeringConnectionOptions =
  req
    "ModifyVpcPeeringConnectionOptions"
    "fixture/ModifyVpcPeeringConnectionOptions.yaml"

requestCreateVpcEndpointConnectionNotification :: CreateVpcEndpointConnectionNotification -> TestTree
requestCreateVpcEndpointConnectionNotification =
  req
    "CreateVpcEndpointConnectionNotification"
    "fixture/CreateVpcEndpointConnectionNotification.yaml"

requestDeleteVpcEndpointServiceConfigurations :: DeleteVpcEndpointServiceConfigurations -> TestTree
requestDeleteVpcEndpointServiceConfigurations =
  req
    "DeleteVpcEndpointServiceConfigurations"
    "fixture/DeleteVpcEndpointServiceConfigurations.yaml"

requestDescribeFleetHistory :: DescribeFleetHistory -> TestTree
requestDescribeFleetHistory =
  req
    "DescribeFleetHistory"
    "fixture/DescribeFleetHistory.yaml"

requestCreateVpc :: CreateVpc -> TestTree
requestCreateVpc =
  req
    "CreateVpc"
    "fixture/CreateVpc.yaml"

requestDescribeVolumeStatus :: DescribeVolumeStatus -> TestTree
requestDescribeVolumeStatus =
  req
    "DescribeVolumeStatus"
    "fixture/DescribeVolumeStatus.yaml"

requestGetSerialConsoleAccessStatus :: GetSerialConsoleAccessStatus -> TestTree
requestGetSerialConsoleAccessStatus =
  req
    "GetSerialConsoleAccessStatus"
    "fixture/GetSerialConsoleAccessStatus.yaml"

requestDescribeReplaceRootVolumeTasks :: DescribeReplaceRootVolumeTasks -> TestTree
requestDescribeReplaceRootVolumeTasks =
  req
    "DescribeReplaceRootVolumeTasks"
    "fixture/DescribeReplaceRootVolumeTasks.yaml"

requestDescribeImages :: DescribeImages -> TestTree
requestDescribeImages =
  req
    "DescribeImages"
    "fixture/DescribeImages.yaml"

requestDeleteVolume :: DeleteVolume -> TestTree
requestDeleteVolume =
  req
    "DeleteVolume"
    "fixture/DeleteVolume.yaml"

requestSearchLocalGatewayRoutes :: SearchLocalGatewayRoutes -> TestTree
requestSearchLocalGatewayRoutes =
  req
    "SearchLocalGatewayRoutes"
    "fixture/SearchLocalGatewayRoutes.yaml"

requestDescribeMovingAddresses :: DescribeMovingAddresses -> TestTree
requestDescribeMovingAddresses =
  req
    "DescribeMovingAddresses"
    "fixture/DescribeMovingAddresses.yaml"

requestCreateTrafficMirrorSession :: CreateTrafficMirrorSession -> TestTree
requestCreateTrafficMirrorSession =
  req
    "CreateTrafficMirrorSession"
    "fixture/CreateTrafficMirrorSession.yaml"

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

requestRejectTransitGatewayMulticastDomainAssociations :: RejectTransitGatewayMulticastDomainAssociations -> TestTree
requestRejectTransitGatewayMulticastDomainAssociations =
  req
    "RejectTransitGatewayMulticastDomainAssociations"
    "fixture/RejectTransitGatewayMulticastDomainAssociations.yaml"

requestDeleteTransitGateway :: DeleteTransitGateway -> TestTree
requestDeleteTransitGateway =
  req
    "DeleteTransitGateway"
    "fixture/DeleteTransitGateway.yaml"

requestDescribeHosts :: DescribeHosts -> TestTree
requestDescribeHosts =
  req
    "DescribeHosts"
    "fixture/DescribeHosts.yaml"

requestDescribeNetworkInterfacePermissions :: DescribeNetworkInterfacePermissions -> TestTree
requestDescribeNetworkInterfacePermissions =
  req
    "DescribeNetworkInterfacePermissions"
    "fixture/DescribeNetworkInterfacePermissions.yaml"

requestGetVpnConnectionDeviceSampleConfiguration :: GetVpnConnectionDeviceSampleConfiguration -> TestTree
requestGetVpnConnectionDeviceSampleConfiguration =
  req
    "GetVpnConnectionDeviceSampleConfiguration"
    "fixture/GetVpnConnectionDeviceSampleConfiguration.yaml"

requestDescribeVpcEndpointServicePermissions :: DescribeVpcEndpointServicePermissions -> TestTree
requestDescribeVpcEndpointServicePermissions =
  req
    "DescribeVpcEndpointServicePermissions"
    "fixture/DescribeVpcEndpointServicePermissions.yaml"

requestDescribeHostReservationOfferings :: DescribeHostReservationOfferings -> TestTree
requestDescribeHostReservationOfferings =
  req
    "DescribeHostReservationOfferings"
    "fixture/DescribeHostReservationOfferings.yaml"

requestDescribeVpcEndpointConnections :: DescribeVpcEndpointConnections -> TestTree
requestDescribeVpcEndpointConnections =
  req
    "DescribeVpcEndpointConnections"
    "fixture/DescribeVpcEndpointConnections.yaml"

requestDescribeFpgaImageAttribute :: DescribeFpgaImageAttribute -> TestTree
requestDescribeFpgaImageAttribute =
  req
    "DescribeFpgaImageAttribute"
    "fixture/DescribeFpgaImageAttribute.yaml"

requestEnableImageDeprecation :: EnableImageDeprecation -> TestTree
requestEnableImageDeprecation =
  req
    "EnableImageDeprecation"
    "fixture/EnableImageDeprecation.yaml"

requestResetImageAttribute :: ResetImageAttribute -> TestTree
requestResetImageAttribute =
  req
    "ResetImageAttribute"
    "fixture/ResetImageAttribute.yaml"

requestAdvertiseByoipCidr :: AdvertiseByoipCidr -> TestTree
requestAdvertiseByoipCidr =
  req
    "AdvertiseByoipCidr"
    "fixture/AdvertiseByoipCidr.yaml"

requestDescribeTransitGatewayRouteTables :: DescribeTransitGatewayRouteTables -> TestTree
requestDescribeTransitGatewayRouteTables =
  req
    "DescribeTransitGatewayRouteTables"
    "fixture/DescribeTransitGatewayRouteTables.yaml"

requestModifyTransitGatewayPrefixListReference :: ModifyTransitGatewayPrefixListReference -> TestTree
requestModifyTransitGatewayPrefixListReference =
  req
    "ModifyTransitGatewayPrefixListReference"
    "fixture/ModifyTransitGatewayPrefixListReference.yaml"

requestRegisterInstanceEventNotificationAttributes :: RegisterInstanceEventNotificationAttributes -> TestTree
requestRegisterInstanceEventNotificationAttributes =
  req
    "RegisterInstanceEventNotificationAttributes"
    "fixture/RegisterInstanceEventNotificationAttributes.yaml"

requestDeleteManagedPrefixList :: DeleteManagedPrefixList -> TestTree
requestDeleteManagedPrefixList =
  req
    "DeleteManagedPrefixList"
    "fixture/DeleteManagedPrefixList.yaml"

requestDescribeRegions :: DescribeRegions -> TestTree
requestDescribeRegions =
  req
    "DescribeRegions"
    "fixture/DescribeRegions.yaml"

requestRevokeSecurityGroupIngress :: RevokeSecurityGroupIngress -> TestTree
requestRevokeSecurityGroupIngress =
  req
    "RevokeSecurityGroupIngress"
    "fixture/RevokeSecurityGroupIngress.yaml"

requestDeleteVpnConnectionRoute :: DeleteVpnConnectionRoute -> TestTree
requestDeleteVpnConnectionRoute =
  req
    "DeleteVpnConnectionRoute"
    "fixture/DeleteVpnConnectionRoute.yaml"

requestDescribeNetworkAcls :: DescribeNetworkAcls -> TestTree
requestDescribeNetworkAcls =
  req
    "DescribeNetworkAcls"
    "fixture/DescribeNetworkAcls.yaml"

requestDeleteDhcpOptions :: DeleteDhcpOptions -> TestTree
requestDeleteDhcpOptions =
  req
    "DeleteDhcpOptions"
    "fixture/DeleteDhcpOptions.yaml"

requestDescribeVpnGateways :: DescribeVpnGateways -> TestTree
requestDescribeVpnGateways =
  req
    "DescribeVpnGateways"
    "fixture/DescribeVpnGateways.yaml"

requestRegisterTransitGatewayMulticastGroupMembers :: RegisterTransitGatewayMulticastGroupMembers -> TestTree
requestRegisterTransitGatewayMulticastGroupMembers =
  req
    "RegisterTransitGatewayMulticastGroupMembers"
    "fixture/RegisterTransitGatewayMulticastGroupMembers.yaml"

requestDescribeHostReservations :: DescribeHostReservations -> TestTree
requestDescribeHostReservations =
  req
    "DescribeHostReservations"
    "fixture/DescribeHostReservations.yaml"

requestRejectVpcPeeringConnection :: RejectVpcPeeringConnection -> TestTree
requestRejectVpcPeeringConnection =
  req
    "RejectVpcPeeringConnection"
    "fixture/RejectVpcPeeringConnection.yaml"

requestCreateEgressOnlyInternetGateway :: CreateEgressOnlyInternetGateway -> TestTree
requestCreateEgressOnlyInternetGateway =
  req
    "CreateEgressOnlyInternetGateway"
    "fixture/CreateEgressOnlyInternetGateway.yaml"

requestDeleteSubnetCidrReservation :: DeleteSubnetCidrReservation -> TestTree
requestDeleteSubnetCidrReservation =
  req
    "DeleteSubnetCidrReservation"
    "fixture/DeleteSubnetCidrReservation.yaml"

requestEnableVpcClassicLinkDnsSupport :: EnableVpcClassicLinkDnsSupport -> TestTree
requestEnableVpcClassicLinkDnsSupport =
  req
    "EnableVpcClassicLinkDnsSupport"
    "fixture/EnableVpcClassicLinkDnsSupport.yaml"

requestAllocateAddress :: AllocateAddress -> TestTree
requestAllocateAddress =
  req
    "AllocateAddress"
    "fixture/AllocateAddress.yaml"

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

requestModifyDefaultCreditSpecification :: ModifyDefaultCreditSpecification -> TestTree
requestModifyDefaultCreditSpecification =
  req
    "ModifyDefaultCreditSpecification"
    "fixture/ModifyDefaultCreditSpecification.yaml"

requestGetManagedPrefixListEntries :: GetManagedPrefixListEntries -> TestTree
requestGetManagedPrefixListEntries =
  req
    "GetManagedPrefixListEntries"
    "fixture/GetManagedPrefixListEntries.yaml"

requestDisableVpcClassicLinkDnsSupport :: DisableVpcClassicLinkDnsSupport -> TestTree
requestDisableVpcClassicLinkDnsSupport =
  req
    "DisableVpcClassicLinkDnsSupport"
    "fixture/DisableVpcClassicLinkDnsSupport.yaml"

requestDisableImageDeprecation :: DisableImageDeprecation -> TestTree
requestDisableImageDeprecation =
  req
    "DisableImageDeprecation"
    "fixture/DisableImageDeprecation.yaml"

requestApplySecurityGroupsToClientVpnTargetNetwork :: ApplySecurityGroupsToClientVpnTargetNetwork -> TestTree
requestApplySecurityGroupsToClientVpnTargetNetwork =
  req
    "ApplySecurityGroupsToClientVpnTargetNetwork"
    "fixture/ApplySecurityGroupsToClientVpnTargetNetwork.yaml"

requestCreateLaunchTemplateVersion :: CreateLaunchTemplateVersion -> TestTree
requestCreateLaunchTemplateVersion =
  req
    "CreateLaunchTemplateVersion"
    "fixture/CreateLaunchTemplateVersion.yaml"

requestCreateVpnConnectionRoute :: CreateVpnConnectionRoute -> TestTree
requestCreateVpnConnectionRoute =
  req
    "CreateVpnConnectionRoute"
    "fixture/CreateVpnConnectionRoute.yaml"

requestDisassociateInstanceEventWindow :: DisassociateInstanceEventWindow -> TestTree
requestDisassociateInstanceEventWindow =
  req
    "DisassociateInstanceEventWindow"
    "fixture/DisassociateInstanceEventWindow.yaml"

requestDescribeConversionTasks :: DescribeConversionTasks -> TestTree
requestDescribeConversionTasks =
  req
    "DescribeConversionTasks"
    "fixture/DescribeConversionTasks.yaml"

requestDeleteTrafficMirrorSession :: DeleteTrafficMirrorSession -> TestTree
requestDeleteTrafficMirrorSession =
  req
    "DeleteTrafficMirrorSession"
    "fixture/DeleteTrafficMirrorSession.yaml"

requestCreateTransitGateway :: CreateTransitGateway -> TestTree
requestCreateTransitGateway =
  req
    "CreateTransitGateway"
    "fixture/CreateTransitGateway.yaml"

requestCreateSnapshots :: CreateSnapshots -> TestTree
requestCreateSnapshots =
  req
    "CreateSnapshots"
    "fixture/CreateSnapshots.yaml"

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

requestDescribeKeyPairs :: DescribeKeyPairs -> TestTree
requestDescribeKeyPairs =
  req
    "DescribeKeyPairs"
    "fixture/DescribeKeyPairs.yaml"

requestCreateFpgaImage :: CreateFpgaImage -> TestTree
requestCreateFpgaImage =
  req
    "CreateFpgaImage"
    "fixture/CreateFpgaImage.yaml"

requestDescribeClassicLinkInstances :: DescribeClassicLinkInstances -> TestTree
requestDescribeClassicLinkInstances =
  req
    "DescribeClassicLinkInstances"
    "fixture/DescribeClassicLinkInstances.yaml"

requestDeleteTrafficMirrorFilterRule :: DeleteTrafficMirrorFilterRule -> TestTree
requestDeleteTrafficMirrorFilterRule =
  req
    "DeleteTrafficMirrorFilterRule"
    "fixture/DeleteTrafficMirrorFilterRule.yaml"

requestTerminateInstances :: TerminateInstances -> TestTree
requestTerminateInstances =
  req
    "TerminateInstances"
    "fixture/TerminateInstances.yaml"

requestModifyFpgaImageAttribute :: ModifyFpgaImageAttribute -> TestTree
requestModifyFpgaImageAttribute =
  req
    "ModifyFpgaImageAttribute"
    "fixture/ModifyFpgaImageAttribute.yaml"

requestWithdrawByoipCidr :: WithdrawByoipCidr -> TestTree
requestWithdrawByoipCidr =
  req
    "WithdrawByoipCidr"
    "fixture/WithdrawByoipCidr.yaml"

requestAttachVpnGateway :: AttachVpnGateway -> TestTree
requestAttachVpnGateway =
  req
    "AttachVpnGateway"
    "fixture/AttachVpnGateway.yaml"

requestAcceptTransitGatewayVpcAttachment :: AcceptTransitGatewayVpcAttachment -> TestTree
requestAcceptTransitGatewayVpcAttachment =
  req
    "AcceptTransitGatewayVpcAttachment"
    "fixture/AcceptTransitGatewayVpcAttachment.yaml"

requestDeleteSpotDatafeedSubscription :: DeleteSpotDatafeedSubscription -> TestTree
requestDeleteSpotDatafeedSubscription =
  req
    "DeleteSpotDatafeedSubscription"
    "fixture/DeleteSpotDatafeedSubscription.yaml"

requestDescribeExportImageTasks :: DescribeExportImageTasks -> TestTree
requestDescribeExportImageTasks =
  req
    "DescribeExportImageTasks"
    "fixture/DescribeExportImageTasks.yaml"

requestCreateCarrierGateway :: CreateCarrierGateway -> TestTree
requestCreateCarrierGateway =
  req
    "CreateCarrierGateway"
    "fixture/CreateCarrierGateway.yaml"

requestAttachInternetGateway :: AttachInternetGateway -> TestTree
requestAttachInternetGateway =
  req
    "AttachInternetGateway"
    "fixture/AttachInternetGateway.yaml"

requestDescribeClientVpnTargetNetworks :: DescribeClientVpnTargetNetworks -> TestTree
requestDescribeClientVpnTargetNetworks =
  req
    "DescribeClientVpnTargetNetworks"
    "fixture/DescribeClientVpnTargetNetworks.yaml"

requestCreateTrafficMirrorFilter :: CreateTrafficMirrorFilter -> TestTree
requestCreateTrafficMirrorFilter =
  req
    "CreateTrafficMirrorFilter"
    "fixture/CreateTrafficMirrorFilter.yaml"

requestDeleteNetworkInsightsAnalysis :: DeleteNetworkInsightsAnalysis -> TestTree
requestDeleteNetworkInsightsAnalysis =
  req
    "DeleteNetworkInsightsAnalysis"
    "fixture/DeleteNetworkInsightsAnalysis.yaml"

requestDescribeIamInstanceProfileAssociations :: DescribeIamInstanceProfileAssociations -> TestTree
requestDescribeIamInstanceProfileAssociations =
  req
    "DescribeIamInstanceProfileAssociations"
    "fixture/DescribeIamInstanceProfileAssociations.yaml"

requestImportKeyPair :: ImportKeyPair -> TestTree
requestImportKeyPair =
  req
    "ImportKeyPair"
    "fixture/ImportKeyPair.yaml"

requestEnableVpcClassicLink :: EnableVpcClassicLink -> TestTree
requestEnableVpcClassicLink =
  req
    "EnableVpcClassicLink"
    "fixture/EnableVpcClassicLink.yaml"

requestDescribeNetworkInsightsAnalyses :: DescribeNetworkInsightsAnalyses -> TestTree
requestDescribeNetworkInsightsAnalyses =
  req
    "DescribeNetworkInsightsAnalyses"
    "fixture/DescribeNetworkInsightsAnalyses.yaml"

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

requestCreateCapacityReservation :: CreateCapacityReservation -> TestTree
requestCreateCapacityReservation =
  req
    "CreateCapacityReservation"
    "fixture/CreateCapacityReservation.yaml"

requestDeleteInstanceEventWindow :: DeleteInstanceEventWindow -> TestTree
requestDeleteInstanceEventWindow =
  req
    "DeleteInstanceEventWindow"
    "fixture/DeleteInstanceEventWindow.yaml"

requestModifyEbsDefaultKmsKeyId :: ModifyEbsDefaultKmsKeyId -> TestTree
requestModifyEbsDefaultKmsKeyId =
  req
    "ModifyEbsDefaultKmsKeyId"
    "fixture/ModifyEbsDefaultKmsKeyId.yaml"

requestProvisionByoipCidr :: ProvisionByoipCidr -> TestTree
requestProvisionByoipCidr =
  req
    "ProvisionByoipCidr"
    "fixture/ProvisionByoipCidr.yaml"

requestDeleteTransitGatewayConnect :: DeleteTransitGatewayConnect -> TestTree
requestDeleteTransitGatewayConnect =
  req
    "DeleteTransitGatewayConnect"
    "fixture/DeleteTransitGatewayConnect.yaml"

requestModifyVpnTunnelOptions :: ModifyVpnTunnelOptions -> TestTree
requestModifyVpnTunnelOptions =
  req
    "ModifyVpnTunnelOptions"
    "fixture/ModifyVpnTunnelOptions.yaml"

requestCreateTransitGatewayPeeringAttachment :: CreateTransitGatewayPeeringAttachment -> TestTree
requestCreateTransitGatewayPeeringAttachment =
  req
    "CreateTransitGatewayPeeringAttachment"
    "fixture/CreateTransitGatewayPeeringAttachment.yaml"

requestCreateCustomerGateway :: CreateCustomerGateway -> TestTree
requestCreateCustomerGateway =
  req
    "CreateCustomerGateway"
    "fixture/CreateCustomerGateway.yaml"

requestModifyVolume :: ModifyVolume -> TestTree
requestModifyVolume =
  req
    "ModifyVolume"
    "fixture/ModifyVolume.yaml"

requestModifyInstancePlacement :: ModifyInstancePlacement -> TestTree
requestModifyInstancePlacement =
  req
    "ModifyInstancePlacement"
    "fixture/ModifyInstancePlacement.yaml"

requestCreateSnapshot :: CreateSnapshot -> TestTree
requestCreateSnapshot =
  req
    "CreateSnapshot"
    "fixture/CreateSnapshot.yaml"

requestDescribeInstanceAttribute :: DescribeInstanceAttribute -> TestTree
requestDescribeInstanceAttribute =
  req
    "DescribeInstanceAttribute"
    "fixture/DescribeInstanceAttribute.yaml"

requestCreateReservedInstancesListing :: CreateReservedInstancesListing -> TestTree
requestCreateReservedInstancesListing =
  req
    "CreateReservedInstancesListing"
    "fixture/CreateReservedInstancesListing.yaml"

requestDeleteSecurityGroup :: DeleteSecurityGroup -> TestTree
requestDeleteSecurityGroup =
  req
    "DeleteSecurityGroup"
    "fixture/DeleteSecurityGroup.yaml"

requestPurchaseScheduledInstances :: PurchaseScheduledInstances -> TestTree
requestPurchaseScheduledInstances =
  req
    "PurchaseScheduledInstances"
    "fixture/PurchaseScheduledInstances.yaml"

requestDescribePublicIpv4Pools :: DescribePublicIpv4Pools -> TestTree
requestDescribePublicIpv4Pools =
  req
    "DescribePublicIpv4Pools"
    "fixture/DescribePublicIpv4Pools.yaml"

requestDescribeLocalGatewayRouteTableVpcAssociations :: DescribeLocalGatewayRouteTableVpcAssociations -> TestTree
requestDescribeLocalGatewayRouteTableVpcAssociations =
  req
    "DescribeLocalGatewayRouteTableVpcAssociations"
    "fixture/DescribeLocalGatewayRouteTableVpcAssociations.yaml"

requestAuthorizeClientVpnIngress :: AuthorizeClientVpnIngress -> TestTree
requestAuthorizeClientVpnIngress =
  req
    "AuthorizeClientVpnIngress"
    "fixture/AuthorizeClientVpnIngress.yaml"

requestCreateVpcPeeringConnection :: CreateVpcPeeringConnection -> TestTree
requestCreateVpcPeeringConnection =
  req
    "CreateVpcPeeringConnection"
    "fixture/CreateVpcPeeringConnection.yaml"

requestCreateInstanceEventWindow :: CreateInstanceEventWindow -> TestTree
requestCreateInstanceEventWindow =
  req
    "CreateInstanceEventWindow"
    "fixture/CreateInstanceEventWindow.yaml"

requestCreateSecurityGroup :: CreateSecurityGroup -> TestTree
requestCreateSecurityGroup =
  req
    "CreateSecurityGroup"
    "fixture/CreateSecurityGroup.yaml"

requestDescribeInternetGateways :: DescribeInternetGateways -> TestTree
requestDescribeInternetGateways =
  req
    "DescribeInternetGateways"
    "fixture/DescribeInternetGateways.yaml"

requestModifyLaunchTemplate :: ModifyLaunchTemplate -> TestTree
requestModifyLaunchTemplate =
  req
    "ModifyLaunchTemplate"
    "fixture/ModifyLaunchTemplate.yaml"

requestModifyInstanceAttribute :: ModifyInstanceAttribute -> TestTree
requestModifyInstanceAttribute =
  req
    "ModifyInstanceAttribute"
    "fixture/ModifyInstanceAttribute.yaml"

requestResetEbsDefaultKmsKeyId :: ResetEbsDefaultKmsKeyId -> TestTree
requestResetEbsDefaultKmsKeyId =
  req
    "ResetEbsDefaultKmsKeyId"
    "fixture/ResetEbsDefaultKmsKeyId.yaml"

requestGetEbsEncryptionByDefault :: GetEbsEncryptionByDefault -> TestTree
requestGetEbsEncryptionByDefault =
  req
    "GetEbsEncryptionByDefault"
    "fixture/GetEbsEncryptionByDefault.yaml"

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

requestAssociateInstanceEventWindow :: AssociateInstanceEventWindow -> TestTree
requestAssociateInstanceEventWindow =
  req
    "AssociateInstanceEventWindow"
    "fixture/AssociateInstanceEventWindow.yaml"

requestReplaceRoute :: ReplaceRoute -> TestTree
requestReplaceRoute =
  req
    "ReplaceRoute"
    "fixture/ReplaceRoute.yaml"

requestCreateTags :: CreateTags -> TestTree
requestCreateTags =
  req
    "CreateTags"
    "fixture/CreateTags.yaml"

requestCreateTransitGatewayConnect :: CreateTransitGatewayConnect -> TestTree
requestCreateTransitGatewayConnect =
  req
    "CreateTransitGatewayConnect"
    "fixture/CreateTransitGatewayConnect.yaml"

requestDeleteLaunchTemplateVersions :: DeleteLaunchTemplateVersions -> TestTree
requestDeleteLaunchTemplateVersions =
  req
    "DeleteLaunchTemplateVersions"
    "fixture/DeleteLaunchTemplateVersions.yaml"

requestGetDefaultCreditSpecification :: GetDefaultCreditSpecification -> TestTree
requestGetDefaultCreditSpecification =
  req
    "GetDefaultCreditSpecification"
    "fixture/GetDefaultCreditSpecification.yaml"

requestResetSnapshotAttribute :: ResetSnapshotAttribute -> TestTree
requestResetSnapshotAttribute =
  req
    "ResetSnapshotAttribute"
    "fixture/ResetSnapshotAttribute.yaml"

requestDeleteTransitGatewayPeeringAttachment :: DeleteTransitGatewayPeeringAttachment -> TestTree
requestDeleteTransitGatewayPeeringAttachment =
  req
    "DeleteTransitGatewayPeeringAttachment"
    "fixture/DeleteTransitGatewayPeeringAttachment.yaml"

requestCreateRoute :: CreateRoute -> TestTree
requestCreateRoute =
  req
    "CreateRoute"
    "fixture/CreateRoute.yaml"

requestBundleInstance :: BundleInstance -> TestTree
requestBundleInstance =
  req
    "BundleInstance"
    "fixture/BundleInstance.yaml"

requestReplaceNetworkAclAssociation :: ReplaceNetworkAclAssociation -> TestTree
requestReplaceNetworkAclAssociation =
  req
    "ReplaceNetworkAclAssociation"
    "fixture/ReplaceNetworkAclAssociation.yaml"

requestDeleteTrafficMirrorFilter :: DeleteTrafficMirrorFilter -> TestTree
requestDeleteTrafficMirrorFilter =
  req
    "DeleteTrafficMirrorFilter"
    "fixture/DeleteTrafficMirrorFilter.yaml"

requestDescribeIdentityIdFormat :: DescribeIdentityIdFormat -> TestTree
requestDescribeIdentityIdFormat =
  req
    "DescribeIdentityIdFormat"
    "fixture/DescribeIdentityIdFormat.yaml"

requestReplaceTransitGatewayRoute :: ReplaceTransitGatewayRoute -> TestTree
requestReplaceTransitGatewayRoute =
  req
    "ReplaceTransitGatewayRoute"
    "fixture/ReplaceTransitGatewayRoute.yaml"

requestGetCoipPoolUsage :: GetCoipPoolUsage -> TestTree
requestGetCoipPoolUsage =
  req
    "GetCoipPoolUsage"
    "fixture/GetCoipPoolUsage.yaml"

requestDeleteTransitGatewayVpcAttachment :: DeleteTransitGatewayVpcAttachment -> TestTree
requestDeleteTransitGatewayVpcAttachment =
  req
    "DeleteTransitGatewayVpcAttachment"
    "fixture/DeleteTransitGatewayVpcAttachment.yaml"

requestCreatePlacementGroup :: CreatePlacementGroup -> TestTree
requestCreatePlacementGroup =
  req
    "CreatePlacementGroup"
    "fixture/CreatePlacementGroup.yaml"

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

requestCancelSpotFleetRequests :: CancelSpotFleetRequests -> TestTree
requestCancelSpotFleetRequests =
  req
    "CancelSpotFleetRequests"
    "fixture/CancelSpotFleetRequests.yaml"

requestDescribeClientVpnAuthorizationRules :: DescribeClientVpnAuthorizationRules -> TestTree
requestDescribeClientVpnAuthorizationRules =
  req
    "DescribeClientVpnAuthorizationRules"
    "fixture/DescribeClientVpnAuthorizationRules.yaml"

requestDeleteSubnet :: DeleteSubnet -> TestTree
requestDeleteSubnet =
  req
    "DeleteSubnet"
    "fixture/DeleteSubnet.yaml"

requestStopInstances :: StopInstances -> TestTree
requestStopInstances =
  req
    "StopInstances"
    "fixture/StopInstances.yaml"

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

requestCreateDefaultVpc :: CreateDefaultVpc -> TestTree
requestCreateDefaultVpc =
  req
    "CreateDefaultVpc"
    "fixture/CreateDefaultVpc.yaml"

requestCreateTransitGatewayPrefixListReference :: CreateTransitGatewayPrefixListReference -> TestTree
requestCreateTransitGatewayPrefixListReference =
  req
    "CreateTransitGatewayPrefixListReference"
    "fixture/CreateTransitGatewayPrefixListReference.yaml"

requestDescribeDhcpOptions :: DescribeDhcpOptions -> TestTree
requestDescribeDhcpOptions =
  req
    "DescribeDhcpOptions"
    "fixture/DescribeDhcpOptions.yaml"

requestCreateTransitGatewayRoute :: CreateTransitGatewayRoute -> TestTree
requestCreateTransitGatewayRoute =
  req
    "CreateTransitGatewayRoute"
    "fixture/CreateTransitGatewayRoute.yaml"

requestResetNetworkInterfaceAttribute :: ResetNetworkInterfaceAttribute -> TestTree
requestResetNetworkInterfaceAttribute =
  req
    "ResetNetworkInterfaceAttribute"
    "fixture/ResetNetworkInterfaceAttribute.yaml"

requestDeleteTransitGatewayMulticastDomain :: DeleteTransitGatewayMulticastDomain -> TestTree
requestDeleteTransitGatewayMulticastDomain =
  req
    "DeleteTransitGatewayMulticastDomain"
    "fixture/DeleteTransitGatewayMulticastDomain.yaml"

requestDeleteNetworkInterface :: DeleteNetworkInterface -> TestTree
requestDeleteNetworkInterface =
  req
    "DeleteNetworkInterface"
    "fixture/DeleteNetworkInterface.yaml"

requestDisassociateVpcCidrBlock :: DisassociateVpcCidrBlock -> TestTree
requestDisassociateVpcCidrBlock =
  req
    "DisassociateVpcCidrBlock"
    "fixture/DisassociateVpcCidrBlock.yaml"

requestReplaceRouteTableAssociation :: ReplaceRouteTableAssociation -> TestTree
requestReplaceRouteTableAssociation =
  req
    "ReplaceRouteTableAssociation"
    "fixture/ReplaceRouteTableAssociation.yaml"

requestCreateNetworkInsightsPath :: CreateNetworkInsightsPath -> TestTree
requestCreateNetworkInsightsPath =
  req
    "CreateNetworkInsightsPath"
    "fixture/CreateNetworkInsightsPath.yaml"

requestDeregisterTransitGatewayMulticastGroupSources :: DeregisterTransitGatewayMulticastGroupSources -> TestTree
requestDeregisterTransitGatewayMulticastGroupSources =
  req
    "DeregisterTransitGatewayMulticastGroupSources"
    "fixture/DeregisterTransitGatewayMulticastGroupSources.yaml"

requestDescribeCustomerGateways :: DescribeCustomerGateways -> TestTree
requestDescribeCustomerGateways =
  req
    "DescribeCustomerGateways"
    "fixture/DescribeCustomerGateways.yaml"

requestDescribeCapacityReservations :: DescribeCapacityReservations -> TestTree
requestDescribeCapacityReservations =
  req
    "DescribeCapacityReservations"
    "fixture/DescribeCapacityReservations.yaml"

requestModifyTrafficMirrorSession :: ModifyTrafficMirrorSession -> TestTree
requestModifyTrafficMirrorSession =
  req
    "ModifyTrafficMirrorSession"
    "fixture/ModifyTrafficMirrorSession.yaml"

requestDisassociateTransitGatewayRouteTable :: DisassociateTransitGatewayRouteTable -> TestTree
requestDisassociateTransitGatewayRouteTable =
  req
    "DisassociateTransitGatewayRouteTable"
    "fixture/DisassociateTransitGatewayRouteTable.yaml"

requestStartInstances :: StartInstances -> TestTree
requestStartInstances =
  req
    "StartInstances"
    "fixture/StartInstances.yaml"

requestDescribeTransitGatewayPeeringAttachments :: DescribeTransitGatewayPeeringAttachments -> TestTree
requestDescribeTransitGatewayPeeringAttachments =
  req
    "DescribeTransitGatewayPeeringAttachments"
    "fixture/DescribeTransitGatewayPeeringAttachments.yaml"

requestImportImage :: ImportImage -> TestTree
requestImportImage =
  req
    "ImportImage"
    "fixture/ImportImage.yaml"

requestDescribeTrafficMirrorFilters :: DescribeTrafficMirrorFilters -> TestTree
requestDescribeTrafficMirrorFilters =
  req
    "DescribeTrafficMirrorFilters"
    "fixture/DescribeTrafficMirrorFilters.yaml"

requestAssociateAddress :: AssociateAddress -> TestTree
requestAssociateAddress =
  req
    "AssociateAddress"
    "fixture/AssociateAddress.yaml"

requestRunScheduledInstances :: RunScheduledInstances -> TestTree
requestRunScheduledInstances =
  req
    "RunScheduledInstances"
    "fixture/RunScheduledInstances.yaml"

requestCopyFpgaImage :: CopyFpgaImage -> TestTree
requestCopyFpgaImage =
  req
    "CopyFpgaImage"
    "fixture/CopyFpgaImage.yaml"

requestDeleteNatGateway :: DeleteNatGateway -> TestTree
requestDeleteNatGateway =
  req
    "DeleteNatGateway"
    "fixture/DeleteNatGateway.yaml"

-- Responses

responseDescribeVpcPeeringConnections :: DescribeVpcPeeringConnectionsResponse -> TestTree
responseDescribeVpcPeeringConnections =
  res
    "DescribeVpcPeeringConnectionsResponse"
    "fixture/DescribeVpcPeeringConnectionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeVpcPeeringConnections)

responseAssociateTrunkInterface :: AssociateTrunkInterfaceResponse -> TestTree
responseAssociateTrunkInterface =
  res
    "AssociateTrunkInterfaceResponse"
    "fixture/AssociateTrunkInterfaceResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateTrunkInterface)

responseDeleteTransitGatewayRoute :: DeleteTransitGatewayRouteResponse -> TestTree
responseDeleteTransitGatewayRoute =
  res
    "DeleteTransitGatewayRouteResponse"
    "fixture/DeleteTransitGatewayRouteResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTransitGatewayRoute)

responseDescribeExportTasks :: DescribeExportTasksResponse -> TestTree
responseDescribeExportTasks =
  res
    "DescribeExportTasksResponse"
    "fixture/DescribeExportTasksResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeExportTasks)

responseDeleteLocalGatewayRouteTableVpcAssociation :: DeleteLocalGatewayRouteTableVpcAssociationResponse -> TestTree
responseDeleteLocalGatewayRouteTableVpcAssociation =
  res
    "DeleteLocalGatewayRouteTableVpcAssociationResponse"
    "fixture/DeleteLocalGatewayRouteTableVpcAssociationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteLocalGatewayRouteTableVpcAssociation)

responseDeleteVpcEndpointConnectionNotifications :: DeleteVpcEndpointConnectionNotificationsResponse -> TestTree
responseDeleteVpcEndpointConnectionNotifications =
  res
    "DeleteVpcEndpointConnectionNotificationsResponse"
    "fixture/DeleteVpcEndpointConnectionNotificationsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVpcEndpointConnectionNotifications)

responseCreateTransitGatewayMulticastDomain :: CreateTransitGatewayMulticastDomainResponse -> TestTree
responseCreateTransitGatewayMulticastDomain =
  res
    "CreateTransitGatewayMulticastDomainResponse"
    "fixture/CreateTransitGatewayMulticastDomainResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTransitGatewayMulticastDomain)

responseCreateVpcEndpointServiceConfiguration :: CreateVpcEndpointServiceConfigurationResponse -> TestTree
responseCreateVpcEndpointServiceConfiguration =
  res
    "CreateVpcEndpointServiceConfigurationResponse"
    "fixture/CreateVpcEndpointServiceConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateVpcEndpointServiceConfiguration)

responseDescribeByoipCidrs :: DescribeByoipCidrsResponse -> TestTree
responseDescribeByoipCidrs =
  res
    "DescribeByoipCidrsResponse"
    "fixture/DescribeByoipCidrsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeByoipCidrs)

responseDetachVolume :: VolumeAttachment -> TestTree
responseDetachVolume =
  res
    "DetachVolumeResponse"
    "fixture/DetachVolumeResponse.proto"
    defaultService
    (Proxy :: Proxy DetachVolume)

responseDeleteNetworkInsightsPath :: DeleteNetworkInsightsPathResponse -> TestTree
responseDeleteNetworkInsightsPath =
  res
    "DeleteNetworkInsightsPathResponse"
    "fixture/DeleteNetworkInsightsPathResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteNetworkInsightsPath)

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

responseAssociateTransitGatewayMulticastDomain :: AssociateTransitGatewayMulticastDomainResponse -> TestTree
responseAssociateTransitGatewayMulticastDomain =
  res
    "AssociateTransitGatewayMulticastDomainResponse"
    "fixture/AssociateTransitGatewayMulticastDomainResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateTransitGatewayMulticastDomain)

responseCreateTransitGatewayConnectPeer :: CreateTransitGatewayConnectPeerResponse -> TestTree
responseCreateTransitGatewayConnectPeer =
  res
    "CreateTransitGatewayConnectPeerResponse"
    "fixture/CreateTransitGatewayConnectPeerResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTransitGatewayConnectPeer)

responseReleaseAddress :: ReleaseAddressResponse -> TestTree
responseReleaseAddress =
  res
    "ReleaseAddressResponse"
    "fixture/ReleaseAddressResponse.proto"
    defaultService
    (Proxy :: Proxy ReleaseAddress)

responseModifyVpcTenancy :: ModifyVpcTenancyResponse -> TestTree
responseModifyVpcTenancy =
  res
    "ModifyVpcTenancyResponse"
    "fixture/ModifyVpcTenancyResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyVpcTenancy)

responseDescribeLocalGatewayVirtualInterfaces :: DescribeLocalGatewayVirtualInterfacesResponse -> TestTree
responseDescribeLocalGatewayVirtualInterfaces =
  res
    "DescribeLocalGatewayVirtualInterfacesResponse"
    "fixture/DescribeLocalGatewayVirtualInterfacesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLocalGatewayVirtualInterfaces)

responseGetHostReservationPurchasePreview :: GetHostReservationPurchasePreviewResponse -> TestTree
responseGetHostReservationPurchasePreview =
  res
    "GetHostReservationPurchasePreviewResponse"
    "fixture/GetHostReservationPurchasePreviewResponse.proto"
    defaultService
    (Proxy :: Proxy GetHostReservationPurchasePreview)

responseAuthorizeSecurityGroupEgress :: AuthorizeSecurityGroupEgressResponse -> TestTree
responseAuthorizeSecurityGroupEgress =
  res
    "AuthorizeSecurityGroupEgressResponse"
    "fixture/AuthorizeSecurityGroupEgressResponse.proto"
    defaultService
    (Proxy :: Proxy AuthorizeSecurityGroupEgress)

responseAcceptReservedInstancesExchangeQuote :: AcceptReservedInstancesExchangeQuoteResponse -> TestTree
responseAcceptReservedInstancesExchangeQuote =
  res
    "AcceptReservedInstancesExchangeQuoteResponse"
    "fixture/AcceptReservedInstancesExchangeQuoteResponse.proto"
    defaultService
    (Proxy :: Proxy AcceptReservedInstancesExchangeQuote)

responseModifyManagedPrefixList :: ModifyManagedPrefixListResponse -> TestTree
responseModifyManagedPrefixList =
  res
    "ModifyManagedPrefixListResponse"
    "fixture/ModifyManagedPrefixListResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyManagedPrefixList)

responseDescribeInstanceCreditSpecifications :: DescribeInstanceCreditSpecificationsResponse -> TestTree
responseDescribeInstanceCreditSpecifications =
  res
    "DescribeInstanceCreditSpecificationsResponse"
    "fixture/DescribeInstanceCreditSpecificationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInstanceCreditSpecifications)

responseGetTransitGatewayMulticastDomainAssociations :: GetTransitGatewayMulticastDomainAssociationsResponse -> TestTree
responseGetTransitGatewayMulticastDomainAssociations =
  res
    "GetTransitGatewayMulticastDomainAssociationsResponse"
    "fixture/GetTransitGatewayMulticastDomainAssociationsResponse.proto"
    defaultService
    (Proxy :: Proxy GetTransitGatewayMulticastDomainAssociations)

responseDescribeInstances :: DescribeInstancesResponse -> TestTree
responseDescribeInstances =
  res
    "DescribeInstancesResponse"
    "fixture/DescribeInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInstances)

responseDisableEbsEncryptionByDefault :: DisableEbsEncryptionByDefaultResponse -> TestTree
responseDisableEbsEncryptionByDefault =
  res
    "DisableEbsEncryptionByDefaultResponse"
    "fixture/DisableEbsEncryptionByDefaultResponse.proto"
    defaultService
    (Proxy :: Proxy DisableEbsEncryptionByDefault)

responseDeregisterInstanceEventNotificationAttributes :: DeregisterInstanceEventNotificationAttributesResponse -> TestTree
responseDeregisterInstanceEventNotificationAttributes =
  res
    "DeregisterInstanceEventNotificationAttributesResponse"
    "fixture/DeregisterInstanceEventNotificationAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterInstanceEventNotificationAttributes)

responseCreateTransitGatewayVpcAttachment :: CreateTransitGatewayVpcAttachmentResponse -> TestTree
responseCreateTransitGatewayVpcAttachment =
  res
    "CreateTransitGatewayVpcAttachmentResponse"
    "fixture/CreateTransitGatewayVpcAttachmentResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTransitGatewayVpcAttachment)

responseDeregisterTransitGatewayMulticastGroupMembers :: DeregisterTransitGatewayMulticastGroupMembersResponse -> TestTree
responseDeregisterTransitGatewayMulticastGroupMembers =
  res
    "DeregisterTransitGatewayMulticastGroupMembersResponse"
    "fixture/DeregisterTransitGatewayMulticastGroupMembersResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterTransitGatewayMulticastGroupMembers)

responseDeleteTransitGatewayPrefixListReference :: DeleteTransitGatewayPrefixListReferenceResponse -> TestTree
responseDeleteTransitGatewayPrefixListReference =
  res
    "DeleteTransitGatewayPrefixListReferenceResponse"
    "fixture/DeleteTransitGatewayPrefixListReferenceResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTransitGatewayPrefixListReference)

responseCreateTransitGatewayRouteTable :: CreateTransitGatewayRouteTableResponse -> TestTree
responseCreateTransitGatewayRouteTable =
  res
    "CreateTransitGatewayRouteTableResponse"
    "fixture/CreateTransitGatewayRouteTableResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTransitGatewayRouteTable)

responseDisassociateAddress :: DisassociateAddressResponse -> TestTree
responseDisassociateAddress =
  res
    "DisassociateAddressResponse"
    "fixture/DisassociateAddressResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateAddress)

responseDetachNetworkInterface :: DetachNetworkInterfaceResponse -> TestTree
responseDetachNetworkInterface =
  res
    "DetachNetworkInterfaceResponse"
    "fixture/DetachNetworkInterfaceResponse.proto"
    defaultService
    (Proxy :: Proxy DetachNetworkInterface)

responseDeleteFleets :: DeleteFleetsResponse -> TestTree
responseDeleteFleets =
  res
    "DeleteFleetsResponse"
    "fixture/DeleteFleetsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteFleets)

responseDeleteVpc :: DeleteVpcResponse -> TestTree
responseDeleteVpc =
  res
    "DeleteVpcResponse"
    "fixture/DeleteVpcResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVpc)

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

responseDeleteTrafficMirrorTarget :: DeleteTrafficMirrorTargetResponse -> TestTree
responseDeleteTrafficMirrorTarget =
  res
    "DeleteTrafficMirrorTargetResponse"
    "fixture/DeleteTrafficMirrorTargetResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTrafficMirrorTarget)

responseDeleteLaunchTemplate :: DeleteLaunchTemplateResponse -> TestTree
responseDeleteLaunchTemplate =
  res
    "DeleteLaunchTemplateResponse"
    "fixture/DeleteLaunchTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteLaunchTemplate)

responseModifySecurityGroupRules :: ModifySecurityGroupRulesResponse -> TestTree
responseModifySecurityGroupRules =
  res
    "ModifySecurityGroupRulesResponse"
    "fixture/ModifySecurityGroupRulesResponse.proto"
    defaultService
    (Proxy :: Proxy ModifySecurityGroupRules)

responseDeleteVpcEndpoints :: DeleteVpcEndpointsResponse -> TestTree
responseDeleteVpcEndpoints =
  res
    "DeleteVpcEndpointsResponse"
    "fixture/DeleteVpcEndpointsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVpcEndpoints)

responseDescribeTrafficMirrorSessions :: DescribeTrafficMirrorSessionsResponse -> TestTree
responseDescribeTrafficMirrorSessions =
  res
    "DescribeTrafficMirrorSessionsResponse"
    "fixture/DescribeTrafficMirrorSessionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTrafficMirrorSessions)

responseUpdateSecurityGroupRuleDescriptionsIngress :: UpdateSecurityGroupRuleDescriptionsIngressResponse -> TestTree
responseUpdateSecurityGroupRuleDescriptionsIngress =
  res
    "UpdateSecurityGroupRuleDescriptionsIngressResponse"
    "fixture/UpdateSecurityGroupRuleDescriptionsIngressResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSecurityGroupRuleDescriptionsIngress)

responseDescribePrefixLists :: DescribePrefixListsResponse -> TestTree
responseDescribePrefixLists =
  res
    "DescribePrefixListsResponse"
    "fixture/DescribePrefixListsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePrefixLists)

responseDescribeVpcClassicLink :: DescribeVpcClassicLinkResponse -> TestTree
responseDescribeVpcClassicLink =
  res
    "DescribeVpcClassicLinkResponse"
    "fixture/DescribeVpcClassicLinkResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeVpcClassicLink)

responseGetAssociatedIpv6PoolCidrs :: GetAssociatedIpv6PoolCidrsResponse -> TestTree
responseGetAssociatedIpv6PoolCidrs =
  res
    "GetAssociatedIpv6PoolCidrsResponse"
    "fixture/GetAssociatedIpv6PoolCidrsResponse.proto"
    defaultService
    (Proxy :: Proxy GetAssociatedIpv6PoolCidrs)

responseImportInstance :: ImportInstanceResponse -> TestTree
responseImportInstance =
  res
    "ImportInstanceResponse"
    "fixture/ImportInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy ImportInstance)

responseCreateDefaultSubnet :: CreateDefaultSubnetResponse -> TestTree
responseCreateDefaultSubnet =
  res
    "CreateDefaultSubnetResponse"
    "fixture/CreateDefaultSubnetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDefaultSubnet)

responseDeleteFlowLogs :: DeleteFlowLogsResponse -> TestTree
responseDeleteFlowLogs =
  res
    "DeleteFlowLogsResponse"
    "fixture/DeleteFlowLogsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteFlowLogs)

responseModifyVolumeAttribute :: ModifyVolumeAttributeResponse -> TestTree
responseModifyVolumeAttribute =
  res
    "ModifyVolumeAttributeResponse"
    "fixture/ModifyVolumeAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyVolumeAttribute)

responseCreateNetworkInterfacePermission :: CreateNetworkInterfacePermissionResponse -> TestTree
responseCreateNetworkInterfacePermission =
  res
    "CreateNetworkInterfacePermissionResponse"
    "fixture/CreateNetworkInterfacePermissionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateNetworkInterfacePermission)

responseDescribeScheduledInstanceAvailability :: DescribeScheduledInstanceAvailabilityResponse -> TestTree
responseDescribeScheduledInstanceAvailability =
  res
    "DescribeScheduledInstanceAvailabilityResponse"
    "fixture/DescribeScheduledInstanceAvailabilityResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeScheduledInstanceAvailability)

responseDescribeClientVpnEndpoints :: DescribeClientVpnEndpointsResponse -> TestTree
responseDescribeClientVpnEndpoints =
  res
    "DescribeClientVpnEndpointsResponse"
    "fixture/DescribeClientVpnEndpointsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeClientVpnEndpoints)

responseRejectVpcEndpointConnections :: RejectVpcEndpointConnectionsResponse -> TestTree
responseRejectVpcEndpointConnections =
  res
    "RejectVpcEndpointConnectionsResponse"
    "fixture/RejectVpcEndpointConnectionsResponse.proto"
    defaultService
    (Proxy :: Proxy RejectVpcEndpointConnections)

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

responseDescribeLocalGatewayRouteTables :: DescribeLocalGatewayRouteTablesResponse -> TestTree
responseDescribeLocalGatewayRouteTables =
  res
    "DescribeLocalGatewayRouteTablesResponse"
    "fixture/DescribeLocalGatewayRouteTablesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLocalGatewayRouteTables)

responseRejectTransitGatewayVpcAttachment :: RejectTransitGatewayVpcAttachmentResponse -> TestTree
responseRejectTransitGatewayVpcAttachment =
  res
    "RejectTransitGatewayVpcAttachmentResponse"
    "fixture/RejectTransitGatewayVpcAttachmentResponse.proto"
    defaultService
    (Proxy :: Proxy RejectTransitGatewayVpcAttachment)

responseCreateVpnGateway :: CreateVpnGatewayResponse -> TestTree
responseCreateVpnGateway =
  res
    "CreateVpnGatewayResponse"
    "fixture/CreateVpnGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy CreateVpnGateway)

responseAcceptTransitGatewayPeeringAttachment :: AcceptTransitGatewayPeeringAttachmentResponse -> TestTree
responseAcceptTransitGatewayPeeringAttachment =
  res
    "AcceptTransitGatewayPeeringAttachmentResponse"
    "fixture/AcceptTransitGatewayPeeringAttachmentResponse.proto"
    defaultService
    (Proxy :: Proxy AcceptTransitGatewayPeeringAttachment)

responseGetTransitGatewayRouteTableAssociations :: GetTransitGatewayRouteTableAssociationsResponse -> TestTree
responseGetTransitGatewayRouteTableAssociations =
  res
    "GetTransitGatewayRouteTableAssociationsResponse"
    "fixture/GetTransitGatewayRouteTableAssociationsResponse.proto"
    defaultService
    (Proxy :: Proxy GetTransitGatewayRouteTableAssociations)

responseDeleteVpnGateway :: DeleteVpnGatewayResponse -> TestTree
responseDeleteVpnGateway =
  res
    "DeleteVpnGatewayResponse"
    "fixture/DeleteVpnGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVpnGateway)

responseDescribeImportImageTasks :: DescribeImportImageTasksResponse -> TestTree
responseDescribeImportImageTasks =
  res
    "DescribeImportImageTasksResponse"
    "fixture/DescribeImportImageTasksResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeImportImageTasks)

responseExportTransitGatewayRoutes :: ExportTransitGatewayRoutesResponse -> TestTree
responseExportTransitGatewayRoutes =
  res
    "ExportTransitGatewayRoutesResponse"
    "fixture/ExportTransitGatewayRoutesResponse.proto"
    defaultService
    (Proxy :: Proxy ExportTransitGatewayRoutes)

responseDescribeLaunchTemplateVersions :: DescribeLaunchTemplateVersionsResponse -> TestTree
responseDescribeLaunchTemplateVersions =
  res
    "DescribeLaunchTemplateVersionsResponse"
    "fixture/DescribeLaunchTemplateVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLaunchTemplateVersions)

responseDescribeFpgaImages :: DescribeFpgaImagesResponse -> TestTree
responseDescribeFpgaImages =
  res
    "DescribeFpgaImagesResponse"
    "fixture/DescribeFpgaImagesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeFpgaImages)

responseGetReservedInstancesExchangeQuote :: GetReservedInstancesExchangeQuoteResponse -> TestTree
responseGetReservedInstancesExchangeQuote =
  res
    "GetReservedInstancesExchangeQuoteResponse"
    "fixture/GetReservedInstancesExchangeQuoteResponse.proto"
    defaultService
    (Proxy :: Proxy GetReservedInstancesExchangeQuote)

responseCreateKeyPair :: CreateKeyPairResponse -> TestTree
responseCreateKeyPair =
  res
    "CreateKeyPairResponse"
    "fixture/CreateKeyPairResponse.proto"
    defaultService
    (Proxy :: Proxy CreateKeyPair)

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

responseDescribeSecurityGroupRules :: DescribeSecurityGroupRulesResponse -> TestTree
responseDescribeSecurityGroupRules =
  res
    "DescribeSecurityGroupRulesResponse"
    "fixture/DescribeSecurityGroupRulesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSecurityGroupRules)

responseDeleteNetworkAcl :: DeleteNetworkAclResponse -> TestTree
responseDeleteNetworkAcl =
  res
    "DeleteNetworkAclResponse"
    "fixture/DeleteNetworkAclResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteNetworkAcl)

responseDescribeRouteTables :: DescribeRouteTablesResponse -> TestTree
responseDescribeRouteTables =
  res
    "DescribeRouteTablesResponse"
    "fixture/DescribeRouteTablesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeRouteTables)

responseDescribeFleetInstances :: DescribeFleetInstancesResponse -> TestTree
responseDescribeFleetInstances =
  res
    "DescribeFleetInstancesResponse"
    "fixture/DescribeFleetInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeFleetInstances)

responseDeleteTransitGatewayRouteTable :: DeleteTransitGatewayRouteTableResponse -> TestTree
responseDeleteTransitGatewayRouteTable =
  res
    "DeleteTransitGatewayRouteTableResponse"
    "fixture/DeleteTransitGatewayRouteTableResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTransitGatewayRouteTable)

responseCreateLaunchTemplate :: CreateLaunchTemplateResponse -> TestTree
responseCreateLaunchTemplate =
  res
    "CreateLaunchTemplateResponse"
    "fixture/CreateLaunchTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy CreateLaunchTemplate)

responseModifyInstanceEventWindow :: ModifyInstanceEventWindowResponse -> TestTree
responseModifyInstanceEventWindow =
  res
    "ModifyInstanceEventWindowResponse"
    "fixture/ModifyInstanceEventWindowResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyInstanceEventWindow)

responseMoveAddressToVpc :: MoveAddressToVpcResponse -> TestTree
responseMoveAddressToVpc =
  res
    "MoveAddressToVpcResponse"
    "fixture/MoveAddressToVpcResponse.proto"
    defaultService
    (Proxy :: Proxy MoveAddressToVpc)

responseAcceptTransitGatewayMulticastDomainAssociations :: AcceptTransitGatewayMulticastDomainAssociationsResponse -> TestTree
responseAcceptTransitGatewayMulticastDomainAssociations =
  res
    "AcceptTransitGatewayMulticastDomainAssociationsResponse"
    "fixture/AcceptTransitGatewayMulticastDomainAssociationsResponse.proto"
    defaultService
    (Proxy :: Proxy AcceptTransitGatewayMulticastDomainAssociations)

responseRestoreAddressToClassic :: RestoreAddressToClassicResponse -> TestTree
responseRestoreAddressToClassic =
  res
    "RestoreAddressToClassicResponse"
    "fixture/RestoreAddressToClassicResponse.proto"
    defaultService
    (Proxy :: Proxy RestoreAddressToClassic)

responseDescribeAvailabilityZones :: DescribeAvailabilityZonesResponse -> TestTree
responseDescribeAvailabilityZones =
  res
    "DescribeAvailabilityZonesResponse"
    "fixture/DescribeAvailabilityZonesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAvailabilityZones)

responseCreateStoreImageTask :: CreateStoreImageTaskResponse -> TestTree
responseCreateStoreImageTask =
  res
    "CreateStoreImageTaskResponse"
    "fixture/CreateStoreImageTaskResponse.proto"
    defaultService
    (Proxy :: Proxy CreateStoreImageTask)

responseCopySnapshot :: CopySnapshotResponse -> TestTree
responseCopySnapshot =
  res
    "CopySnapshotResponse"
    "fixture/CopySnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy CopySnapshot)

responseDeleteNetworkInterfacePermission :: DeleteNetworkInterfacePermissionResponse -> TestTree
responseDeleteNetworkInterfacePermission =
  res
    "DeleteNetworkInterfacePermissionResponse"
    "fixture/DeleteNetworkInterfacePermissionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteNetworkInterfacePermission)

responseCreateFlowLogs :: CreateFlowLogsResponse -> TestTree
responseCreateFlowLogs =
  res
    "CreateFlowLogsResponse"
    "fixture/CreateFlowLogsResponse.proto"
    defaultService
    (Proxy :: Proxy CreateFlowLogs)

responseDetachClassicLinkVpc :: DetachClassicLinkVpcResponse -> TestTree
responseDetachClassicLinkVpc =
  res
    "DetachClassicLinkVpcResponse"
    "fixture/DetachClassicLinkVpcResponse.proto"
    defaultService
    (Proxy :: Proxy DetachClassicLinkVpc)

responseDeleteRouteTable :: DeleteRouteTableResponse -> TestTree
responseDeleteRouteTable =
  res
    "DeleteRouteTableResponse"
    "fixture/DeleteRouteTableResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRouteTable)

responseModifyVpnConnectionOptions :: ModifyVpnConnectionOptionsResponse -> TestTree
responseModifyVpnConnectionOptions =
  res
    "ModifyVpnConnectionOptionsResponse"
    "fixture/ModifyVpnConnectionOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyVpnConnectionOptions)

responseMonitorInstances :: MonitorInstancesResponse -> TestTree
responseMonitorInstances =
  res
    "MonitorInstancesResponse"
    "fixture/MonitorInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy MonitorInstances)

responseModifyIdFormat :: ModifyIdFormatResponse -> TestTree
responseModifyIdFormat =
  res
    "ModifyIdFormatResponse"
    "fixture/ModifyIdFormatResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyIdFormat)

responseAllocateHosts :: AllocateHostsResponse -> TestTree
responseAllocateHosts =
  res
    "AllocateHostsResponse"
    "fixture/AllocateHostsResponse.proto"
    defaultService
    (Proxy :: Proxy AllocateHosts)

responseDescribeImageAttribute :: DescribeImageAttributeResponse -> TestTree
responseDescribeImageAttribute =
  res
    "DescribeImageAttributeResponse"
    "fixture/DescribeImageAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeImageAttribute)

responseDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations :: DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse -> TestTree
responseDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations =
  res
    "DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse"
    "fixture/DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations)

responseDescribeReservedInstancesModifications :: DescribeReservedInstancesModificationsResponse -> TestTree
responseDescribeReservedInstancesModifications =
  res
    "DescribeReservedInstancesModificationsResponse"
    "fixture/DescribeReservedInstancesModificationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeReservedInstancesModifications)

responseStartVpcEndpointServicePrivateDnsVerification :: StartVpcEndpointServicePrivateDnsVerificationResponse -> TestTree
responseStartVpcEndpointServicePrivateDnsVerification =
  res
    "StartVpcEndpointServicePrivateDnsVerificationResponse"
    "fixture/StartVpcEndpointServicePrivateDnsVerificationResponse.proto"
    defaultService
    (Proxy :: Proxy StartVpcEndpointServicePrivateDnsVerification)

responseCreateTrafficMirrorFilterRule :: CreateTrafficMirrorFilterRuleResponse -> TestTree
responseCreateTrafficMirrorFilterRule =
  res
    "CreateTrafficMirrorFilterRuleResponse"
    "fixture/CreateTrafficMirrorFilterRuleResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTrafficMirrorFilterRule)

responseGetEbsDefaultKmsKeyId :: GetEbsDefaultKmsKeyIdResponse -> TestTree
responseGetEbsDefaultKmsKeyId =
  res
    "GetEbsDefaultKmsKeyIdResponse"
    "fixture/GetEbsDefaultKmsKeyIdResponse.proto"
    defaultService
    (Proxy :: Proxy GetEbsDefaultKmsKeyId)

responseDescribeClientVpnRoutes :: DescribeClientVpnRoutesResponse -> TestTree
responseDescribeClientVpnRoutes =
  res
    "DescribeClientVpnRoutesResponse"
    "fixture/DescribeClientVpnRoutesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeClientVpnRoutes)

responseModifyVpnConnection :: ModifyVpnConnectionResponse -> TestTree
responseModifyVpnConnection =
  res
    "ModifyVpnConnectionResponse"
    "fixture/ModifyVpnConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyVpnConnection)

responseModifyFleet :: ModifyFleetResponse -> TestTree
responseModifyFleet =
  res
    "ModifyFleetResponse"
    "fixture/ModifyFleetResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyFleet)

responseRegisterImage :: RegisterImageResponse -> TestTree
responseRegisterImage =
  res
    "RegisterImageResponse"
    "fixture/RegisterImageResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterImage)

responseRevokeClientVpnIngress :: RevokeClientVpnIngressResponse -> TestTree
responseRevokeClientVpnIngress =
  res
    "RevokeClientVpnIngressResponse"
    "fixture/RevokeClientVpnIngressResponse.proto"
    defaultService
    (Proxy :: Proxy RevokeClientVpnIngress)

responseUpdateSecurityGroupRuleDescriptionsEgress :: UpdateSecurityGroupRuleDescriptionsEgressResponse -> TestTree
responseUpdateSecurityGroupRuleDescriptionsEgress =
  res
    "UpdateSecurityGroupRuleDescriptionsEgressResponse"
    "fixture/UpdateSecurityGroupRuleDescriptionsEgressResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSecurityGroupRuleDescriptionsEgress)

responseModifyVpcEndpoint :: ModifyVpcEndpointResponse -> TestTree
responseModifyVpcEndpoint =
  res
    "ModifyVpcEndpointResponse"
    "fixture/ModifyVpcEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyVpcEndpoint)

responseUnassignPrivateIpAddresses :: UnassignPrivateIpAddressesResponse -> TestTree
responseUnassignPrivateIpAddresses =
  res
    "UnassignPrivateIpAddressesResponse"
    "fixture/UnassignPrivateIpAddressesResponse.proto"
    defaultService
    (Proxy :: Proxy UnassignPrivateIpAddresses)

responseEnableFastSnapshotRestores :: EnableFastSnapshotRestoresResponse -> TestTree
responseEnableFastSnapshotRestores =
  res
    "EnableFastSnapshotRestoresResponse"
    "fixture/EnableFastSnapshotRestoresResponse.proto"
    defaultService
    (Proxy :: Proxy EnableFastSnapshotRestores)

responseCancelImportTask :: CancelImportTaskResponse -> TestTree
responseCancelImportTask =
  res
    "CancelImportTaskResponse"
    "fixture/CancelImportTaskResponse.proto"
    defaultService
    (Proxy :: Proxy CancelImportTask)

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

responseResetFpgaImageAttribute :: ResetFpgaImageAttributeResponse -> TestTree
responseResetFpgaImageAttribute =
  res
    "ResetFpgaImageAttributeResponse"
    "fixture/ResetFpgaImageAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy ResetFpgaImageAttribute)

responseGetConsoleOutput :: GetConsoleOutputResponse -> TestTree
responseGetConsoleOutput =
  res
    "GetConsoleOutputResponse"
    "fixture/GetConsoleOutputResponse.proto"
    defaultService
    (Proxy :: Proxy GetConsoleOutput)

responseDeleteFpgaImage :: DeleteFpgaImageResponse -> TestTree
responseDeleteFpgaImage =
  res
    "DeleteFpgaImageResponse"
    "fixture/DeleteFpgaImageResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteFpgaImage)

responseModifyReservedInstances :: ModifyReservedInstancesResponse -> TestTree
responseModifyReservedInstances =
  res
    "ModifyReservedInstancesResponse"
    "fixture/ModifyReservedInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyReservedInstances)

responseCreateRestoreImageTask :: CreateRestoreImageTaskResponse -> TestTree
responseCreateRestoreImageTask =
  res
    "CreateRestoreImageTaskResponse"
    "fixture/CreateRestoreImageTaskResponse.proto"
    defaultService
    (Proxy :: Proxy CreateRestoreImageTask)

responseDescribeSpotInstanceRequests :: DescribeSpotInstanceRequestsResponse -> TestTree
responseDescribeSpotInstanceRequests =
  res
    "DescribeSpotInstanceRequestsResponse"
    "fixture/DescribeSpotInstanceRequestsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSpotInstanceRequests)

responseModifyVpcEndpointServicePermissions :: ModifyVpcEndpointServicePermissionsResponse -> TestTree
responseModifyVpcEndpointServicePermissions =
  res
    "ModifyVpcEndpointServicePermissionsResponse"
    "fixture/ModifyVpcEndpointServicePermissionsResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyVpcEndpointServicePermissions)

responseUnassignIpv6Addresses :: UnassignIpv6AddressesResponse -> TestTree
responseUnassignIpv6Addresses =
  res
    "UnassignIpv6AddressesResponse"
    "fixture/UnassignIpv6AddressesResponse.proto"
    defaultService
    (Proxy :: Proxy UnassignIpv6Addresses)

responseDescribeVolumesModifications :: DescribeVolumesModificationsResponse -> TestTree
responseDescribeVolumesModifications =
  res
    "DescribeVolumesModificationsResponse"
    "fixture/DescribeVolumesModificationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeVolumesModifications)

responseDescribeIdFormat :: DescribeIdFormatResponse -> TestTree
responseDescribeIdFormat =
  res
    "DescribeIdFormatResponse"
    "fixture/DescribeIdFormatResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeIdFormat)

responseReportInstanceStatus :: ReportInstanceStatusResponse -> TestTree
responseReportInstanceStatus =
  res
    "ReportInstanceStatusResponse"
    "fixture/ReportInstanceStatusResponse.proto"
    defaultService
    (Proxy :: Proxy ReportInstanceStatus)

responseRunInstances :: Reservation -> TestTree
responseRunInstances =
  res
    "RunInstancesResponse"
    "fixture/RunInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy RunInstances)

responseModifyHosts :: ModifyHostsResponse -> TestTree
responseModifyHosts =
  res
    "ModifyHostsResponse"
    "fixture/ModifyHostsResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyHosts)

responseAttachVolume :: VolumeAttachment -> TestTree
responseAttachVolume =
  res
    "AttachVolumeResponse"
    "fixture/AttachVolumeResponse.proto"
    defaultService
    (Proxy :: Proxy AttachVolume)

responseDescribeStoreImageTasks :: DescribeStoreImageTasksResponse -> TestTree
responseDescribeStoreImageTasks =
  res
    "DescribeStoreImageTasksResponse"
    "fixture/DescribeStoreImageTasksResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeStoreImageTasks)

responseCreateReplaceRootVolumeTask :: CreateReplaceRootVolumeTaskResponse -> TestTree
responseCreateReplaceRootVolumeTask =
  res
    "CreateReplaceRootVolumeTaskResponse"
    "fixture/CreateReplaceRootVolumeTaskResponse.proto"
    defaultService
    (Proxy :: Proxy CreateReplaceRootVolumeTask)

responseDescribeAccountAttributes :: DescribeAccountAttributesResponse -> TestTree
responseDescribeAccountAttributes =
  res
    "DescribeAccountAttributesResponse"
    "fixture/DescribeAccountAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAccountAttributes)

responseModifyImageAttribute :: ModifyImageAttributeResponse -> TestTree
responseModifyImageAttribute =
  res
    "ModifyImageAttributeResponse"
    "fixture/ModifyImageAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyImageAttribute)

responseRegisterTransitGatewayMulticastGroupSources :: RegisterTransitGatewayMulticastGroupSourcesResponse -> TestTree
responseRegisterTransitGatewayMulticastGroupSources =
  res
    "RegisterTransitGatewayMulticastGroupSourcesResponse"
    "fixture/RegisterTransitGatewayMulticastGroupSourcesResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterTransitGatewayMulticastGroupSources)

responseRebootInstances :: RebootInstancesResponse -> TestTree
responseRebootInstances =
  res
    "RebootInstancesResponse"
    "fixture/RebootInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy RebootInstances)

responseAssociateRouteTable :: AssociateRouteTableResponse -> TestTree
responseAssociateRouteTable =
  res
    "AssociateRouteTableResponse"
    "fixture/AssociateRouteTableResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateRouteTable)

responseAssociateIamInstanceProfile :: AssociateIamInstanceProfileResponse -> TestTree
responseAssociateIamInstanceProfile =
  res
    "AssociateIamInstanceProfileResponse"
    "fixture/AssociateIamInstanceProfileResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateIamInstanceProfile)

responsePurchaseHostReservation :: PurchaseHostReservationResponse -> TestTree
responsePurchaseHostReservation =
  res
    "PurchaseHostReservationResponse"
    "fixture/PurchaseHostReservationResponse.proto"
    defaultService
    (Proxy :: Proxy PurchaseHostReservation)

responseDescribeTrafficMirrorTargets :: DescribeTrafficMirrorTargetsResponse -> TestTree
responseDescribeTrafficMirrorTargets =
  res
    "DescribeTrafficMirrorTargetsResponse"
    "fixture/DescribeTrafficMirrorTargetsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTrafficMirrorTargets)

responseGetManagedPrefixListAssociations :: GetManagedPrefixListAssociationsResponse -> TestTree
responseGetManagedPrefixListAssociations =
  res
    "GetManagedPrefixListAssociationsResponse"
    "fixture/GetManagedPrefixListAssociationsResponse.proto"
    defaultService
    (Proxy :: Proxy GetManagedPrefixListAssociations)

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

responseCreateDhcpOptions :: CreateDhcpOptionsResponse -> TestTree
responseCreateDhcpOptions =
  res
    "CreateDhcpOptionsResponse"
    "fixture/CreateDhcpOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDhcpOptions)

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

responseDescribeVpcs :: DescribeVpcsResponse -> TestTree
responseDescribeVpcs =
  res
    "DescribeVpcsResponse"
    "fixture/DescribeVpcsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeVpcs)

responseDescribeLocalGateways :: DescribeLocalGatewaysResponse -> TestTree
responseDescribeLocalGateways =
  res
    "DescribeLocalGatewaysResponse"
    "fixture/DescribeLocalGatewaysResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLocalGateways)

responseDescribeIpv6Pools :: DescribeIpv6PoolsResponse -> TestTree
responseDescribeIpv6Pools =
  res
    "DescribeIpv6PoolsResponse"
    "fixture/DescribeIpv6PoolsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeIpv6Pools)

responseCreateRouteTable :: CreateRouteTableResponse -> TestTree
responseCreateRouteTable =
  res
    "CreateRouteTableResponse"
    "fixture/CreateRouteTableResponse.proto"
    defaultService
    (Proxy :: Proxy CreateRouteTable)

responseDescribeVpcEndpointConnectionNotifications :: DescribeVpcEndpointConnectionNotificationsResponse -> TestTree
responseDescribeVpcEndpointConnectionNotifications =
  res
    "DescribeVpcEndpointConnectionNotificationsResponse"
    "fixture/DescribeVpcEndpointConnectionNotificationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeVpcEndpointConnectionNotifications)

responseGetTransitGatewayPrefixListReferences :: GetTransitGatewayPrefixListReferencesResponse -> TestTree
responseGetTransitGatewayPrefixListReferences =
  res
    "GetTransitGatewayPrefixListReferencesResponse"
    "fixture/GetTransitGatewayPrefixListReferencesResponse.proto"
    defaultService
    (Proxy :: Proxy GetTransitGatewayPrefixListReferences)

responseAcceptVpcEndpointConnections :: AcceptVpcEndpointConnectionsResponse -> TestTree
responseAcceptVpcEndpointConnections =
  res
    "AcceptVpcEndpointConnectionsResponse"
    "fixture/AcceptVpcEndpointConnectionsResponse.proto"
    defaultService
    (Proxy :: Proxy AcceptVpcEndpointConnections)

responseGetTransitGatewayRouteTablePropagations :: GetTransitGatewayRouteTablePropagationsResponse -> TestTree
responseGetTransitGatewayRouteTablePropagations =
  res
    "GetTransitGatewayRouteTablePropagationsResponse"
    "fixture/GetTransitGatewayRouteTablePropagationsResponse.proto"
    defaultService
    (Proxy :: Proxy GetTransitGatewayRouteTablePropagations)

responseAssociateDhcpOptions :: AssociateDhcpOptionsResponse -> TestTree
responseAssociateDhcpOptions =
  res
    "AssociateDhcpOptionsResponse"
    "fixture/AssociateDhcpOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateDhcpOptions)

responseDeleteEgressOnlyInternetGateway :: DeleteEgressOnlyInternetGatewayResponse -> TestTree
responseDeleteEgressOnlyInternetGateway =
  res
    "DeleteEgressOnlyInternetGatewayResponse"
    "fixture/DeleteEgressOnlyInternetGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteEgressOnlyInternetGateway)

responseGetVpnConnectionDeviceTypes :: GetVpnConnectionDeviceTypesResponse -> TestTree
responseGetVpnConnectionDeviceTypes =
  res
    "GetVpnConnectionDeviceTypesResponse"
    "fixture/GetVpnConnectionDeviceTypesResponse.proto"
    defaultService
    (Proxy :: Proxy GetVpnConnectionDeviceTypes)

responseCreateSubnetCidrReservation :: CreateSubnetCidrReservationResponse -> TestTree
responseCreateSubnetCidrReservation =
  res
    "CreateSubnetCidrReservationResponse"
    "fixture/CreateSubnetCidrReservationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSubnetCidrReservation)

responseDisableFastSnapshotRestores :: DisableFastSnapshotRestoresResponse -> TestTree
responseDisableFastSnapshotRestores =
  res
    "DisableFastSnapshotRestoresResponse"
    "fixture/DisableFastSnapshotRestoresResponse.proto"
    defaultService
    (Proxy :: Proxy DisableFastSnapshotRestores)

responseRequestSpotInstances :: RequestSpotInstancesResponse -> TestTree
responseRequestSpotInstances =
  res
    "RequestSpotInstancesResponse"
    "fixture/RequestSpotInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy RequestSpotInstances)

responseDescribeLaunchTemplates :: DescribeLaunchTemplatesResponse -> TestTree
responseDescribeLaunchTemplates =
  res
    "DescribeLaunchTemplatesResponse"
    "fixture/DescribeLaunchTemplatesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLaunchTemplates)

responseCreateImage :: CreateImageResponse -> TestTree
responseCreateImage =
  res
    "CreateImageResponse"
    "fixture/CreateImageResponse.proto"
    defaultService
    (Proxy :: Proxy CreateImage)

responseModifyTransitGatewayVpcAttachment :: ModifyTransitGatewayVpcAttachmentResponse -> TestTree
responseModifyTransitGatewayVpcAttachment =
  res
    "ModifyTransitGatewayVpcAttachmentResponse"
    "fixture/ModifyTransitGatewayVpcAttachmentResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyTransitGatewayVpcAttachment)

responseAssignIpv6Addresses :: AssignIpv6AddressesResponse -> TestTree
responseAssignIpv6Addresses =
  res
    "AssignIpv6AddressesResponse"
    "fixture/AssignIpv6AddressesResponse.proto"
    defaultService
    (Proxy :: Proxy AssignIpv6Addresses)

responseDescribeLocalGatewayVirtualInterfaceGroups :: DescribeLocalGatewayVirtualInterfaceGroupsResponse -> TestTree
responseDescribeLocalGatewayVirtualInterfaceGroups =
  res
    "DescribeLocalGatewayVirtualInterfaceGroupsResponse"
    "fixture/DescribeLocalGatewayVirtualInterfaceGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLocalGatewayVirtualInterfaceGroups)

responseDescribeVpnConnections :: DescribeVpnConnectionsResponse -> TestTree
responseDescribeVpnConnections =
  res
    "DescribeVpnConnectionsResponse"
    "fixture/DescribeVpnConnectionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeVpnConnections)

responseCreateNetworkAclEntry :: CreateNetworkAclEntryResponse -> TestTree
responseCreateNetworkAclEntry =
  res
    "CreateNetworkAclEntryResponse"
    "fixture/CreateNetworkAclEntryResponse.proto"
    defaultService
    (Proxy :: Proxy CreateNetworkAclEntry)

responseDescribePlacementGroups :: DescribePlacementGroupsResponse -> TestTree
responseDescribePlacementGroups =
  res
    "DescribePlacementGroupsResponse"
    "fixture/DescribePlacementGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePlacementGroups)

responseModifySnapshotAttribute :: ModifySnapshotAttributeResponse -> TestTree
responseModifySnapshotAttribute =
  res
    "ModifySnapshotAttributeResponse"
    "fixture/ModifySnapshotAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy ModifySnapshotAttribute)

responseModifyIdentityIdFormat :: ModifyIdentityIdFormatResponse -> TestTree
responseModifyIdentityIdFormat =
  res
    "ModifyIdentityIdFormatResponse"
    "fixture/ModifyIdentityIdFormatResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyIdentityIdFormat)

responseEnableVgwRoutePropagation :: EnableVgwRoutePropagationResponse -> TestTree
responseEnableVgwRoutePropagation =
  res
    "EnableVgwRoutePropagationResponse"
    "fixture/EnableVgwRoutePropagationResponse.proto"
    defaultService
    (Proxy :: Proxy EnableVgwRoutePropagation)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags =
  res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTags)

responseResetInstanceAttribute :: ResetInstanceAttributeResponse -> TestTree
responseResetInstanceAttribute =
  res
    "ResetInstanceAttributeResponse"
    "fixture/ResetInstanceAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy ResetInstanceAttribute)

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

responseDescribeFlowLogs :: DescribeFlowLogsResponse -> TestTree
responseDescribeFlowLogs =
  res
    "DescribeFlowLogsResponse"
    "fixture/DescribeFlowLogsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeFlowLogs)

responseDescribeReservedInstancesOfferings :: DescribeReservedInstancesOfferingsResponse -> TestTree
responseDescribeReservedInstancesOfferings =
  res
    "DescribeReservedInstancesOfferingsResponse"
    "fixture/DescribeReservedInstancesOfferingsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeReservedInstancesOfferings)

responseDescribeFleets :: DescribeFleetsResponse -> TestTree
responseDescribeFleets =
  res
    "DescribeFleetsResponse"
    "fixture/DescribeFleetsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeFleets)

responseAttachNetworkInterface :: AttachNetworkInterfaceResponse -> TestTree
responseAttachNetworkInterface =
  res
    "AttachNetworkInterfaceResponse"
    "fixture/AttachNetworkInterfaceResponse.proto"
    defaultService
    (Proxy :: Proxy AttachNetworkInterface)

responseConfirmProductInstance :: ConfirmProductInstanceResponse -> TestTree
responseConfirmProductInstance =
  res
    "ConfirmProductInstanceResponse"
    "fixture/ConfirmProductInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy ConfirmProductInstance)

responseDescribeTransitGatewayAttachments :: DescribeTransitGatewayAttachmentsResponse -> TestTree
responseDescribeTransitGatewayAttachments =
  res
    "DescribeTransitGatewayAttachmentsResponse"
    "fixture/DescribeTransitGatewayAttachmentsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTransitGatewayAttachments)

responseModifyAvailabilityZoneGroup :: ModifyAvailabilityZoneGroupResponse -> TestTree
responseModifyAvailabilityZoneGroup =
  res
    "ModifyAvailabilityZoneGroupResponse"
    "fixture/ModifyAvailabilityZoneGroupResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyAvailabilityZoneGroup)

responseReplaceNetworkAclEntry :: ReplaceNetworkAclEntryResponse -> TestTree
responseReplaceNetworkAclEntry =
  res
    "ReplaceNetworkAclEntryResponse"
    "fixture/ReplaceNetworkAclEntryResponse.proto"
    defaultService
    (Proxy :: Proxy ReplaceNetworkAclEntry)

responseDescribeSpotFleetRequestHistory :: DescribeSpotFleetRequestHistoryResponse -> TestTree
responseDescribeSpotFleetRequestHistory =
  res
    "DescribeSpotFleetRequestHistoryResponse"
    "fixture/DescribeSpotFleetRequestHistoryResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSpotFleetRequestHistory)

responseCreateLocalGatewayRoute :: CreateLocalGatewayRouteResponse -> TestTree
responseCreateLocalGatewayRoute =
  res
    "CreateLocalGatewayRouteResponse"
    "fixture/CreateLocalGatewayRouteResponse.proto"
    defaultService
    (Proxy :: Proxy CreateLocalGatewayRoute)

responseDescribeVpcEndpoints :: DescribeVpcEndpointsResponse -> TestTree
responseDescribeVpcEndpoints =
  res
    "DescribeVpcEndpointsResponse"
    "fixture/DescribeVpcEndpointsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeVpcEndpoints)

responseModifyInstanceEventStartTime :: ModifyInstanceEventStartTimeResponse -> TestTree
responseModifyInstanceEventStartTime =
  res
    "ModifyInstanceEventStartTimeResponse"
    "fixture/ModifyInstanceEventStartTimeResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyInstanceEventStartTime)

responseDescribeStaleSecurityGroups :: DescribeStaleSecurityGroupsResponse -> TestTree
responseDescribeStaleSecurityGroups =
  res
    "DescribeStaleSecurityGroupsResponse"
    "fixture/DescribeStaleSecurityGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeStaleSecurityGroups)

responseDescribeInstanceStatus :: DescribeInstanceStatusResponse -> TestTree
responseDescribeInstanceStatus =
  res
    "DescribeInstanceStatusResponse"
    "fixture/DescribeInstanceStatusResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInstanceStatus)

responseDeleteNetworkAclEntry :: DeleteNetworkAclEntryResponse -> TestTree
responseDeleteNetworkAclEntry =
  res
    "DeleteNetworkAclEntryResponse"
    "fixture/DeleteNetworkAclEntryResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteNetworkAclEntry)

responseGetConsoleScreenshot :: GetConsoleScreenshotResponse -> TestTree
responseGetConsoleScreenshot =
  res
    "GetConsoleScreenshotResponse"
    "fixture/GetConsoleScreenshotResponse.proto"
    defaultService
    (Proxy :: Proxy GetConsoleScreenshot)

responseGetGroupsForCapacityReservation :: GetGroupsForCapacityReservationResponse -> TestTree
responseGetGroupsForCapacityReservation =
  res
    "GetGroupsForCapacityReservationResponse"
    "fixture/GetGroupsForCapacityReservationResponse.proto"
    defaultService
    (Proxy :: Proxy GetGroupsForCapacityReservation)

responseDisassociateIamInstanceProfile :: DisassociateIamInstanceProfileResponse -> TestTree
responseDisassociateIamInstanceProfile =
  res
    "DisassociateIamInstanceProfileResponse"
    "fixture/DisassociateIamInstanceProfileResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateIamInstanceProfile)

responseDescribeVpcEndpointServiceConfigurations :: DescribeVpcEndpointServiceConfigurationsResponse -> TestTree
responseDescribeVpcEndpointServiceConfigurations =
  res
    "DescribeVpcEndpointServiceConfigurationsResponse"
    "fixture/DescribeVpcEndpointServiceConfigurationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeVpcEndpointServiceConfigurations)

responseCancelSpotInstanceRequests :: CancelSpotInstanceRequestsResponse -> TestTree
responseCancelSpotInstanceRequests =
  res
    "CancelSpotInstanceRequestsResponse"
    "fixture/CancelSpotInstanceRequestsResponse.proto"
    defaultService
    (Proxy :: Proxy CancelSpotInstanceRequests)

responseDeleteLocalGatewayRoute :: DeleteLocalGatewayRouteResponse -> TestTree
responseDeleteLocalGatewayRoute =
  res
    "DeleteLocalGatewayRouteResponse"
    "fixture/DeleteLocalGatewayRouteResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteLocalGatewayRoute)

responseDescribeVpcEndpointServices :: DescribeVpcEndpointServicesResponse -> TestTree
responseDescribeVpcEndpointServices =
  res
    "DescribeVpcEndpointServicesResponse"
    "fixture/DescribeVpcEndpointServicesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeVpcEndpointServices)

responseDisassociateRouteTable :: DisassociateRouteTableResponse -> TestTree
responseDisassociateRouteTable =
  res
    "DisassociateRouteTableResponse"
    "fixture/DisassociateRouteTableResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateRouteTable)

responseAssignPrivateIpAddresses :: AssignPrivateIpAddressesResponse -> TestTree
responseAssignPrivateIpAddresses =
  res
    "AssignPrivateIpAddressesResponse"
    "fixture/AssignPrivateIpAddressesResponse.proto"
    defaultService
    (Proxy :: Proxy AssignPrivateIpAddresses)

responseGetFlowLogsIntegrationTemplate :: GetFlowLogsIntegrationTemplateResponse -> TestTree
responseGetFlowLogsIntegrationTemplate =
  res
    "GetFlowLogsIntegrationTemplateResponse"
    "fixture/GetFlowLogsIntegrationTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy GetFlowLogsIntegrationTemplate)

responseModifyVpnTunnelCertificate :: ModifyVpnTunnelCertificateResponse -> TestTree
responseModifyVpnTunnelCertificate =
  res
    "ModifyVpnTunnelCertificateResponse"
    "fixture/ModifyVpnTunnelCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyVpnTunnelCertificate)

responseDisableVgwRoutePropagation :: DisableVgwRoutePropagationResponse -> TestTree
responseDisableVgwRoutePropagation =
  res
    "DisableVgwRoutePropagationResponse"
    "fixture/DisableVgwRoutePropagationResponse.proto"
    defaultService
    (Proxy :: Proxy DisableVgwRoutePropagation)

responseDeleteSnapshot :: DeleteSnapshotResponse -> TestTree
responseDeleteSnapshot =
  res
    "DeleteSnapshotResponse"
    "fixture/DeleteSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSnapshot)

responseDescribeSubnets :: DescribeSubnetsResponse -> TestTree
responseDescribeSubnets =
  res
    "DescribeSubnetsResponse"
    "fixture/DescribeSubnetsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSubnets)

responseCreateSpotDatafeedSubscription :: CreateSpotDatafeedSubscriptionResponse -> TestTree
responseCreateSpotDatafeedSubscription =
  res
    "CreateSpotDatafeedSubscriptionResponse"
    "fixture/CreateSpotDatafeedSubscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSpotDatafeedSubscription)

responseUnmonitorInstances :: UnmonitorInstancesResponse -> TestTree
responseUnmonitorInstances =
  res
    "UnmonitorInstancesResponse"
    "fixture/UnmonitorInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy UnmonitorInstances)

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

responsePurchaseReservedInstancesOffering :: PurchaseReservedInstancesOfferingResponse -> TestTree
responsePurchaseReservedInstancesOffering =
  res
    "PurchaseReservedInstancesOfferingResponse"
    "fixture/PurchaseReservedInstancesOfferingResponse.proto"
    defaultService
    (Proxy :: Proxy PurchaseReservedInstancesOffering)

responseDescribeSnapshotAttribute :: DescribeSnapshotAttributeResponse -> TestTree
responseDescribeSnapshotAttribute =
  res
    "DescribeSnapshotAttributeResponse"
    "fixture/DescribeSnapshotAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSnapshotAttribute)

responseAuthorizeSecurityGroupIngress :: AuthorizeSecurityGroupIngressResponse -> TestTree
responseAuthorizeSecurityGroupIngress =
  res
    "AuthorizeSecurityGroupIngressResponse"
    "fixture/AuthorizeSecurityGroupIngressResponse.proto"
    defaultService
    (Proxy :: Proxy AuthorizeSecurityGroupIngress)

responseDescribeNatGateways :: DescribeNatGatewaysResponse -> TestTree
responseDescribeNatGateways =
  res
    "DescribeNatGatewaysResponse"
    "fixture/DescribeNatGatewaysResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeNatGateways)

responseDisableVpcClassicLink :: DisableVpcClassicLinkResponse -> TestTree
responseDisableVpcClassicLink =
  res
    "DisableVpcClassicLinkResponse"
    "fixture/DisableVpcClassicLinkResponse.proto"
    defaultService
    (Proxy :: Proxy DisableVpcClassicLink)

responseDescribeTransitGatewayMulticastDomains :: DescribeTransitGatewayMulticastDomainsResponse -> TestTree
responseDescribeTransitGatewayMulticastDomains =
  res
    "DescribeTransitGatewayMulticastDomainsResponse"
    "fixture/DescribeTransitGatewayMulticastDomainsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTransitGatewayMulticastDomains)

responseGetTransitGatewayAttachmentPropagations :: GetTransitGatewayAttachmentPropagationsResponse -> TestTree
responseGetTransitGatewayAttachmentPropagations =
  res
    "GetTransitGatewayAttachmentPropagationsResponse"
    "fixture/GetTransitGatewayAttachmentPropagationsResponse.proto"
    defaultService
    (Proxy :: Proxy GetTransitGatewayAttachmentPropagations)

responseModifyVpcEndpointConnectionNotification :: ModifyVpcEndpointConnectionNotificationResponse -> TestTree
responseModifyVpcEndpointConnectionNotification =
  res
    "ModifyVpcEndpointConnectionNotificationResponse"
    "fixture/ModifyVpcEndpointConnectionNotificationResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyVpcEndpointConnectionNotification)

responseRestoreManagedPrefixListVersion :: RestoreManagedPrefixListVersionResponse -> TestTree
responseRestoreManagedPrefixListVersion =
  res
    "RestoreManagedPrefixListVersionResponse"
    "fixture/RestoreManagedPrefixListVersionResponse.proto"
    defaultService
    (Proxy :: Proxy RestoreManagedPrefixListVersion)

responseDescribeTransitGatewayConnectPeers :: DescribeTransitGatewayConnectPeersResponse -> TestTree
responseDescribeTransitGatewayConnectPeers =
  res
    "DescribeTransitGatewayConnectPeersResponse"
    "fixture/DescribeTransitGatewayConnectPeersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTransitGatewayConnectPeers)

responseDeleteCarrierGateway :: DeleteCarrierGatewayResponse -> TestTree
responseDeleteCarrierGateway =
  res
    "DeleteCarrierGatewayResponse"
    "fixture/DeleteCarrierGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCarrierGateway)

responseDescribeNetworkInterfaces :: DescribeNetworkInterfacesResponse -> TestTree
responseDescribeNetworkInterfaces =
  res
    "DescribeNetworkInterfacesResponse"
    "fixture/DescribeNetworkInterfacesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeNetworkInterfaces)

responseDescribeTransitGatewayVpcAttachments :: DescribeTransitGatewayVpcAttachmentsResponse -> TestTree
responseDescribeTransitGatewayVpcAttachments =
  res
    "DescribeTransitGatewayVpcAttachmentsResponse"
    "fixture/DescribeTransitGatewayVpcAttachmentsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTransitGatewayVpcAttachments)

responseModifyAddressAttribute :: ModifyAddressAttributeResponse -> TestTree
responseModifyAddressAttribute =
  res
    "ModifyAddressAttributeResponse"
    "fixture/ModifyAddressAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyAddressAttribute)

responseDescribeImportSnapshotTasks :: DescribeImportSnapshotTasksResponse -> TestTree
responseDescribeImportSnapshotTasks =
  res
    "DescribeImportSnapshotTasksResponse"
    "fixture/DescribeImportSnapshotTasksResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeImportSnapshotTasks)

responseCopyImage :: CopyImageResponse -> TestTree
responseCopyImage =
  res
    "CopyImageResponse"
    "fixture/CopyImageResponse.proto"
    defaultService
    (Proxy :: Proxy CopyImage)

responseDescribeInstanceEventNotificationAttributes :: DescribeInstanceEventNotificationAttributesResponse -> TestTree
responseDescribeInstanceEventNotificationAttributes =
  res
    "DescribeInstanceEventNotificationAttributesResponse"
    "fixture/DescribeInstanceEventNotificationAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInstanceEventNotificationAttributes)

responseEnableSerialConsoleAccess :: EnableSerialConsoleAccessResponse -> TestTree
responseEnableSerialConsoleAccess =
  res
    "EnableSerialConsoleAccessResponse"
    "fixture/EnableSerialConsoleAccessResponse.proto"
    defaultService
    (Proxy :: Proxy EnableSerialConsoleAccess)

responseModifyTrafficMirrorFilterRule :: ModifyTrafficMirrorFilterRuleResponse -> TestTree
responseModifyTrafficMirrorFilterRule =
  res
    "ModifyTrafficMirrorFilterRuleResponse"
    "fixture/ModifyTrafficMirrorFilterRuleResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyTrafficMirrorFilterRule)

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

responseModifyInstanceCapacityReservationAttributes :: ModifyInstanceCapacityReservationAttributesResponse -> TestTree
responseModifyInstanceCapacityReservationAttributes =
  res
    "ModifyInstanceCapacityReservationAttributesResponse"
    "fixture/ModifyInstanceCapacityReservationAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyInstanceCapacityReservationAttributes)

responseDescribeNetworkInterfaceAttribute :: DescribeNetworkInterfaceAttributeResponse -> TestTree
responseDescribeNetworkInterfaceAttribute =
  res
    "DescribeNetworkInterfaceAttributeResponse"
    "fixture/DescribeNetworkInterfaceAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeNetworkInterfaceAttribute)

responseAttachClassicLinkVpc :: AttachClassicLinkVpcResponse -> TestTree
responseAttachClassicLinkVpc =
  res
    "AttachClassicLinkVpcResponse"
    "fixture/AttachClassicLinkVpcResponse.proto"
    defaultService
    (Proxy :: Proxy AttachClassicLinkVpc)

responseGetSubnetCidrReservations :: GetSubnetCidrReservationsResponse -> TestTree
responseGetSubnetCidrReservations =
  res
    "GetSubnetCidrReservationsResponse"
    "fixture/GetSubnetCidrReservationsResponse.proto"
    defaultService
    (Proxy :: Proxy GetSubnetCidrReservations)

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

responseDisableTransitGatewayRouteTablePropagation :: DisableTransitGatewayRouteTablePropagationResponse -> TestTree
responseDisableTransitGatewayRouteTablePropagation =
  res
    "DisableTransitGatewayRouteTablePropagationResponse"
    "fixture/DisableTransitGatewayRouteTablePropagationResponse.proto"
    defaultService
    (Proxy :: Proxy DisableTransitGatewayRouteTablePropagation)

responseCancelReservedInstancesListing :: CancelReservedInstancesListingResponse -> TestTree
responseCancelReservedInstancesListing =
  res
    "CancelReservedInstancesListingResponse"
    "fixture/CancelReservedInstancesListingResponse.proto"
    defaultService
    (Proxy :: Proxy CancelReservedInstancesListing)

responseDeleteQueuedReservedInstances :: DeleteQueuedReservedInstancesResponse -> TestTree
responseDeleteQueuedReservedInstances =
  res
    "DeleteQueuedReservedInstancesResponse"
    "fixture/DeleteQueuedReservedInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteQueuedReservedInstances)

responseCreateFleet :: CreateFleetResponse -> TestTree
responseCreateFleet =
  res
    "CreateFleetResponse"
    "fixture/CreateFleetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateFleet)

responseDescribeSnapshots :: DescribeSnapshotsResponse -> TestTree
responseDescribeSnapshots =
  res
    "DescribeSnapshotsResponse"
    "fixture/DescribeSnapshotsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSnapshots)

responseDescribeReservedInstancesListings :: DescribeReservedInstancesListingsResponse -> TestTree
responseDescribeReservedInstancesListings =
  res
    "DescribeReservedInstancesListingsResponse"
    "fixture/DescribeReservedInstancesListingsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeReservedInstancesListings)

responseModifyClientVpnEndpoint :: ModifyClientVpnEndpointResponse -> TestTree
responseModifyClientVpnEndpoint =
  res
    "ModifyClientVpnEndpointResponse"
    "fixture/ModifyClientVpnEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyClientVpnEndpoint)

responseCreateVpcEndpoint :: CreateVpcEndpointResponse -> TestTree
responseCreateVpcEndpoint =
  res
    "CreateVpcEndpointResponse"
    "fixture/CreateVpcEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy CreateVpcEndpoint)

responseCreateVpnConnection :: CreateVpnConnectionResponse -> TestTree
responseCreateVpnConnection =
  res
    "CreateVpnConnectionResponse"
    "fixture/CreateVpnConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateVpnConnection)

responseImportClientVpnClientCertificateRevocationList :: ImportClientVpnClientCertificateRevocationListResponse -> TestTree
responseImportClientVpnClientCertificateRevocationList =
  res
    "ImportClientVpnClientCertificateRevocationListResponse"
    "fixture/ImportClientVpnClientCertificateRevocationListResponse.proto"
    defaultService
    (Proxy :: Proxy ImportClientVpnClientCertificateRevocationList)

responseAssociateSubnetCidrBlock :: AssociateSubnetCidrBlockResponse -> TestTree
responseAssociateSubnetCidrBlock =
  res
    "AssociateSubnetCidrBlockResponse"
    "fixture/AssociateSubnetCidrBlockResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateSubnetCidrBlock)

responseDescribeSpotPriceHistory :: DescribeSpotPriceHistoryResponse -> TestTree
responseDescribeSpotPriceHistory =
  res
    "DescribeSpotPriceHistoryResponse"
    "fixture/DescribeSpotPriceHistoryResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSpotPriceHistory)

responseEnableEbsEncryptionByDefault :: EnableEbsEncryptionByDefaultResponse -> TestTree
responseEnableEbsEncryptionByDefault =
  res
    "EnableEbsEncryptionByDefaultResponse"
    "fixture/EnableEbsEncryptionByDefaultResponse.proto"
    defaultService
    (Proxy :: Proxy EnableEbsEncryptionByDefault)

responseDescribeVpcClassicLinkDnsSupport :: DescribeVpcClassicLinkDnsSupportResponse -> TestTree
responseDescribeVpcClassicLinkDnsSupport =
  res
    "DescribeVpcClassicLinkDnsSupportResponse"
    "fixture/DescribeVpcClassicLinkDnsSupportResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeVpcClassicLinkDnsSupport)

responseCreateLocalGatewayRouteTableVpcAssociation :: CreateLocalGatewayRouteTableVpcAssociationResponse -> TestTree
responseCreateLocalGatewayRouteTableVpcAssociation =
  res
    "CreateLocalGatewayRouteTableVpcAssociationResponse"
    "fixture/CreateLocalGatewayRouteTableVpcAssociationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateLocalGatewayRouteTableVpcAssociation)

responseDescribeAggregateIdFormat :: DescribeAggregateIdFormatResponse -> TestTree
responseDescribeAggregateIdFormat =
  res
    "DescribeAggregateIdFormatResponse"
    "fixture/DescribeAggregateIdFormatResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAggregateIdFormat)

responseEnableTransitGatewayRouteTablePropagation :: EnableTransitGatewayRouteTablePropagationResponse -> TestTree
responseEnableTransitGatewayRouteTablePropagation =
  res
    "EnableTransitGatewayRouteTablePropagationResponse"
    "fixture/EnableTransitGatewayRouteTablePropagationResponse.proto"
    defaultService
    (Proxy :: Proxy EnableTransitGatewayRouteTablePropagation)

responseRequestSpotFleet :: RequestSpotFleetResponse -> TestTree
responseRequestSpotFleet =
  res
    "RequestSpotFleetResponse"
    "fixture/RequestSpotFleetResponse.proto"
    defaultService
    (Proxy :: Proxy RequestSpotFleet)

responseDescribeBundleTasks :: DescribeBundleTasksResponse -> TestTree
responseDescribeBundleTasks =
  res
    "DescribeBundleTasksResponse"
    "fixture/DescribeBundleTasksResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeBundleTasks)

responseModifyNetworkInterfaceAttribute :: ModifyNetworkInterfaceAttributeResponse -> TestTree
responseModifyNetworkInterfaceAttribute =
  res
    "ModifyNetworkInterfaceAttributeResponse"
    "fixture/ModifyNetworkInterfaceAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyNetworkInterfaceAttribute)

responseDisableSerialConsoleAccess :: DisableSerialConsoleAccessResponse -> TestTree
responseDisableSerialConsoleAccess =
  res
    "DisableSerialConsoleAccessResponse"
    "fixture/DisableSerialConsoleAccessResponse.proto"
    defaultService
    (Proxy :: Proxy DisableSerialConsoleAccess)

responseDescribeInstanceTypeOfferings :: DescribeInstanceTypeOfferingsResponse -> TestTree
responseDescribeInstanceTypeOfferings =
  res
    "DescribeInstanceTypeOfferingsResponse"
    "fixture/DescribeInstanceTypeOfferingsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInstanceTypeOfferings)

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

responseResetAddressAttribute :: ResetAddressAttributeResponse -> TestTree
responseResetAddressAttribute =
  res
    "ResetAddressAttributeResponse"
    "fixture/ResetAddressAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy ResetAddressAttribute)

responseDescribeCoipPools :: DescribeCoipPoolsResponse -> TestTree
responseDescribeCoipPools =
  res
    "DescribeCoipPoolsResponse"
    "fixture/DescribeCoipPoolsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCoipPools)

responseDescribeInstanceEventWindows :: DescribeInstanceEventWindowsResponse -> TestTree
responseDescribeInstanceEventWindows =
  res
    "DescribeInstanceEventWindowsResponse"
    "fixture/DescribeInstanceEventWindowsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInstanceEventWindows)

responseCreateSubnet :: CreateSubnetResponse -> TestTree
responseCreateSubnet =
  res
    "CreateSubnetResponse"
    "fixture/CreateSubnetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSubnet)

responseDescribeSecurityGroups :: DescribeSecurityGroupsResponse -> TestTree
responseDescribeSecurityGroups =
  res
    "DescribeSecurityGroupsResponse"
    "fixture/DescribeSecurityGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSecurityGroups)

responseDeletePlacementGroup :: DeletePlacementGroupResponse -> TestTree
responseDeletePlacementGroup =
  res
    "DeletePlacementGroupResponse"
    "fixture/DeletePlacementGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePlacementGroup)

responseEnableVolumeIO :: EnableVolumeIOResponse -> TestTree
responseEnableVolumeIO =
  res
    "EnableVolumeIOResponse"
    "fixture/EnableVolumeIOResponse.proto"
    defaultService
    (Proxy :: Proxy EnableVolumeIO)

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

responseModifySubnetAttribute :: ModifySubnetAttributeResponse -> TestTree
responseModifySubnetAttribute =
  res
    "ModifySubnetAttributeResponse"
    "fixture/ModifySubnetAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy ModifySubnetAttribute)

responseDescribeTransitGatewayConnects :: DescribeTransitGatewayConnectsResponse -> TestTree
responseDescribeTransitGatewayConnects =
  res
    "DescribeTransitGatewayConnectsResponse"
    "fixture/DescribeTransitGatewayConnectsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTransitGatewayConnects)

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

responseDeleteVpnConnection :: DeleteVpnConnectionResponse -> TestTree
responseDeleteVpnConnection =
  res
    "DeleteVpnConnectionResponse"
    "fixture/DeleteVpnConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVpnConnection)

responseDescribeInstanceTypes :: DescribeInstanceTypesResponse -> TestTree
responseDescribeInstanceTypes =
  res
    "DescribeInstanceTypesResponse"
    "fixture/DescribeInstanceTypesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInstanceTypes)

responseCancelExportTask :: CancelExportTaskResponse -> TestTree
responseCancelExportTask =
  res
    "CancelExportTaskResponse"
    "fixture/CancelExportTaskResponse.proto"
    defaultService
    (Proxy :: Proxy CancelExportTask)

responseModifyTransitGateway :: ModifyTransitGatewayResponse -> TestTree
responseModifyTransitGateway =
  res
    "ModifyTransitGatewayResponse"
    "fixture/ModifyTransitGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyTransitGateway)

responseCreateInternetGateway :: CreateInternetGatewayResponse -> TestTree
responseCreateInternetGateway =
  res
    "CreateInternetGatewayResponse"
    "fixture/CreateInternetGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy CreateInternetGateway)

responseSendDiagnosticInterrupt :: SendDiagnosticInterruptResponse -> TestTree
responseSendDiagnosticInterrupt =
  res
    "SendDiagnosticInterruptResponse"
    "fixture/SendDiagnosticInterruptResponse.proto"
    defaultService
    (Proxy :: Proxy SendDiagnosticInterrupt)

responseDisassociateClientVpnTargetNetwork :: DisassociateClientVpnTargetNetworkResponse -> TestTree
responseDisassociateClientVpnTargetNetwork =
  res
    "DisassociateClientVpnTargetNetworkResponse"
    "fixture/DisassociateClientVpnTargetNetworkResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateClientVpnTargetNetwork)

responseModifyInstanceMetadataOptions :: ModifyInstanceMetadataOptionsResponse -> TestTree
responseModifyInstanceMetadataOptions =
  res
    "ModifyInstanceMetadataOptionsResponse"
    "fixture/ModifyInstanceMetadataOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyInstanceMetadataOptions)

responseDescribeSpotDatafeedSubscription :: DescribeSpotDatafeedSubscriptionResponse -> TestTree
responseDescribeSpotDatafeedSubscription =
  res
    "DescribeSpotDatafeedSubscriptionResponse"
    "fixture/DescribeSpotDatafeedSubscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSpotDatafeedSubscription)

responseExportClientVpnClientConfiguration :: ExportClientVpnClientConfigurationResponse -> TestTree
responseExportClientVpnClientConfiguration =
  res
    "ExportClientVpnClientConfigurationResponse"
    "fixture/ExportClientVpnClientConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy ExportClientVpnClientConfiguration)

responseDeleteKeyPair :: DeleteKeyPairResponse -> TestTree
responseDeleteKeyPair =
  res
    "DeleteKeyPairResponse"
    "fixture/DeleteKeyPairResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteKeyPair)

responseDescribeEgressOnlyInternetGateways :: DescribeEgressOnlyInternetGatewaysResponse -> TestTree
responseDescribeEgressOnlyInternetGateways =
  res
    "DescribeEgressOnlyInternetGatewaysResponse"
    "fixture/DescribeEgressOnlyInternetGatewaysResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEgressOnlyInternetGateways)

responseCreateVolume :: Volume -> TestTree
responseCreateVolume =
  res
    "CreateVolumeResponse"
    "fixture/CreateVolumeResponse.proto"
    defaultService
    (Proxy :: Proxy CreateVolume)

responseModifyTrafficMirrorFilterNetworkServices :: ModifyTrafficMirrorFilterNetworkServicesResponse -> TestTree
responseModifyTrafficMirrorFilterNetworkServices =
  res
    "ModifyTrafficMirrorFilterNetworkServicesResponse"
    "fixture/ModifyTrafficMirrorFilterNetworkServicesResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyTrafficMirrorFilterNetworkServices)

responseDescribeVpcAttribute :: DescribeVpcAttributeResponse -> TestTree
responseDescribeVpcAttribute =
  res
    "DescribeVpcAttributeResponse"
    "fixture/DescribeVpcAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeVpcAttribute)

responseDescribeTrunkInterfaceAssociations :: DescribeTrunkInterfaceAssociationsResponse -> TestTree
responseDescribeTrunkInterfaceAssociations =
  res
    "DescribeTrunkInterfaceAssociationsResponse"
    "fixture/DescribeTrunkInterfaceAssociationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTrunkInterfaceAssociations)

responseCreateInstanceExportTask :: CreateInstanceExportTaskResponse -> TestTree
responseCreateInstanceExportTask =
  res
    "CreateInstanceExportTaskResponse"
    "fixture/CreateInstanceExportTaskResponse.proto"
    defaultService
    (Proxy :: Proxy CreateInstanceExportTask)

responseCreateClientVpnRoute :: CreateClientVpnRouteResponse -> TestTree
responseCreateClientVpnRoute =
  res
    "CreateClientVpnRouteResponse"
    "fixture/CreateClientVpnRouteResponse.proto"
    defaultService
    (Proxy :: Proxy CreateClientVpnRoute)

responseModifyCapacityReservation :: ModifyCapacityReservationResponse -> TestTree
responseModifyCapacityReservation =
  res
    "ModifyCapacityReservationResponse"
    "fixture/ModifyCapacityReservationResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyCapacityReservation)

responseRevokeSecurityGroupEgress :: RevokeSecurityGroupEgressResponse -> TestTree
responseRevokeSecurityGroupEgress =
  res
    "RevokeSecurityGroupEgressResponse"
    "fixture/RevokeSecurityGroupEgressResponse.proto"
    defaultService
    (Proxy :: Proxy RevokeSecurityGroupEgress)

responseDescribeSecurityGroupReferences :: DescribeSecurityGroupReferencesResponse -> TestTree
responseDescribeSecurityGroupReferences =
  res
    "DescribeSecurityGroupReferencesResponse"
    "fixture/DescribeSecurityGroupReferencesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSecurityGroupReferences)

responseDisassociateSubnetCidrBlock :: DisassociateSubnetCidrBlockResponse -> TestTree
responseDisassociateSubnetCidrBlock =
  res
    "DisassociateSubnetCidrBlockResponse"
    "fixture/DisassociateSubnetCidrBlockResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateSubnetCidrBlock)

responseDetachInternetGateway :: DetachInternetGatewayResponse -> TestTree
responseDetachInternetGateway =
  res
    "DetachInternetGatewayResponse"
    "fixture/DetachInternetGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy DetachInternetGateway)

responseSearchTransitGatewayMulticastGroups :: SearchTransitGatewayMulticastGroupsResponse -> TestTree
responseSearchTransitGatewayMulticastGroups =
  res
    "SearchTransitGatewayMulticastGroupsResponse"
    "fixture/SearchTransitGatewayMulticastGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy SearchTransitGatewayMulticastGroups)

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

responseModifyVpcAttribute :: ModifyVpcAttributeResponse -> TestTree
responseModifyVpcAttribute =
  res
    "ModifyVpcAttributeResponse"
    "fixture/ModifyVpcAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyVpcAttribute)

responseDeleteClientVpnRoute :: DeleteClientVpnRouteResponse -> TestTree
responseDeleteClientVpnRoute =
  res
    "DeleteClientVpnRouteResponse"
    "fixture/DeleteClientVpnRouteResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteClientVpnRoute)

responseDeprovisionByoipCidr :: DeprovisionByoipCidrResponse -> TestTree
responseDeprovisionByoipCidr =
  res
    "DeprovisionByoipCidrResponse"
    "fixture/DeprovisionByoipCidrResponse.proto"
    defaultService
    (Proxy :: Proxy DeprovisionByoipCidr)

responseDisassociateTrunkInterface :: DisassociateTrunkInterfaceResponse -> TestTree
responseDisassociateTrunkInterface =
  res
    "DisassociateTrunkInterfaceResponse"
    "fixture/DisassociateTrunkInterfaceResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateTrunkInterface)

responseImportSnapshot :: ImportSnapshotResponse -> TestTree
responseImportSnapshot =
  res
    "ImportSnapshotResponse"
    "fixture/ImportSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy ImportSnapshot)

responseDescribeSpotFleetInstances :: DescribeSpotFleetInstancesResponse -> TestTree
responseDescribeSpotFleetInstances =
  res
    "DescribeSpotFleetInstancesResponse"
    "fixture/DescribeSpotFleetInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSpotFleetInstances)

responseDescribeClientVpnConnections :: DescribeClientVpnConnectionsResponse -> TestTree
responseDescribeClientVpnConnections =
  res
    "DescribeClientVpnConnectionsResponse"
    "fixture/DescribeClientVpnConnectionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeClientVpnConnections)

responseCreateTrafficMirrorTarget :: CreateTrafficMirrorTargetResponse -> TestTree
responseCreateTrafficMirrorTarget =
  res
    "CreateTrafficMirrorTargetResponse"
    "fixture/CreateTrafficMirrorTargetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTrafficMirrorTarget)

responseModifyInstanceCreditSpecification :: ModifyInstanceCreditSpecificationResponse -> TestTree
responseModifyInstanceCreditSpecification =
  res
    "ModifyInstanceCreditSpecificationResponse"
    "fixture/ModifyInstanceCreditSpecificationResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyInstanceCreditSpecification)

responseAcceptVpcPeeringConnection :: AcceptVpcPeeringConnectionResponse -> TestTree
responseAcceptVpcPeeringConnection =
  res
    "AcceptVpcPeeringConnectionResponse"
    "fixture/AcceptVpcPeeringConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy AcceptVpcPeeringConnection)

responseDescribeVolumeAttribute :: DescribeVolumeAttributeResponse -> TestTree
responseDescribeVolumeAttribute =
  res
    "DescribeVolumeAttributeResponse"
    "fixture/DescribeVolumeAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeVolumeAttribute)

responseDescribeSpotFleetRequests :: DescribeSpotFleetRequestsResponse -> TestTree
responseDescribeSpotFleetRequests =
  res
    "DescribeSpotFleetRequestsResponse"
    "fixture/DescribeSpotFleetRequestsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSpotFleetRequests)

responseDescribeAddressesAttribute :: DescribeAddressesAttributeResponse -> TestTree
responseDescribeAddressesAttribute =
  res
    "DescribeAddressesAttributeResponse"
    "fixture/DescribeAddressesAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAddressesAttribute)

responseDescribePrincipalIdFormat :: DescribePrincipalIdFormatResponse -> TestTree
responseDescribePrincipalIdFormat =
  res
    "DescribePrincipalIdFormatResponse"
    "fixture/DescribePrincipalIdFormatResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePrincipalIdFormat)

responseDescribeTransitGateways :: DescribeTransitGatewaysResponse -> TestTree
responseDescribeTransitGateways =
  res
    "DescribeTransitGatewaysResponse"
    "fixture/DescribeTransitGatewaysResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTransitGateways)

responseModifyVpcPeeringConnectionOptions :: ModifyVpcPeeringConnectionOptionsResponse -> TestTree
responseModifyVpcPeeringConnectionOptions =
  res
    "ModifyVpcPeeringConnectionOptionsResponse"
    "fixture/ModifyVpcPeeringConnectionOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyVpcPeeringConnectionOptions)

responseCreateVpcEndpointConnectionNotification :: CreateVpcEndpointConnectionNotificationResponse -> TestTree
responseCreateVpcEndpointConnectionNotification =
  res
    "CreateVpcEndpointConnectionNotificationResponse"
    "fixture/CreateVpcEndpointConnectionNotificationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateVpcEndpointConnectionNotification)

responseDeleteVpcEndpointServiceConfigurations :: DeleteVpcEndpointServiceConfigurationsResponse -> TestTree
responseDeleteVpcEndpointServiceConfigurations =
  res
    "DeleteVpcEndpointServiceConfigurationsResponse"
    "fixture/DeleteVpcEndpointServiceConfigurationsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVpcEndpointServiceConfigurations)

responseDescribeFleetHistory :: DescribeFleetHistoryResponse -> TestTree
responseDescribeFleetHistory =
  res
    "DescribeFleetHistoryResponse"
    "fixture/DescribeFleetHistoryResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeFleetHistory)

responseCreateVpc :: CreateVpcResponse -> TestTree
responseCreateVpc =
  res
    "CreateVpcResponse"
    "fixture/CreateVpcResponse.proto"
    defaultService
    (Proxy :: Proxy CreateVpc)

responseDescribeVolumeStatus :: DescribeVolumeStatusResponse -> TestTree
responseDescribeVolumeStatus =
  res
    "DescribeVolumeStatusResponse"
    "fixture/DescribeVolumeStatusResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeVolumeStatus)

responseGetSerialConsoleAccessStatus :: GetSerialConsoleAccessStatusResponse -> TestTree
responseGetSerialConsoleAccessStatus =
  res
    "GetSerialConsoleAccessStatusResponse"
    "fixture/GetSerialConsoleAccessStatusResponse.proto"
    defaultService
    (Proxy :: Proxy GetSerialConsoleAccessStatus)

responseDescribeReplaceRootVolumeTasks :: DescribeReplaceRootVolumeTasksResponse -> TestTree
responseDescribeReplaceRootVolumeTasks =
  res
    "DescribeReplaceRootVolumeTasksResponse"
    "fixture/DescribeReplaceRootVolumeTasksResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeReplaceRootVolumeTasks)

responseDescribeImages :: DescribeImagesResponse -> TestTree
responseDescribeImages =
  res
    "DescribeImagesResponse"
    "fixture/DescribeImagesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeImages)

responseDeleteVolume :: DeleteVolumeResponse -> TestTree
responseDeleteVolume =
  res
    "DeleteVolumeResponse"
    "fixture/DeleteVolumeResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVolume)

responseSearchLocalGatewayRoutes :: SearchLocalGatewayRoutesResponse -> TestTree
responseSearchLocalGatewayRoutes =
  res
    "SearchLocalGatewayRoutesResponse"
    "fixture/SearchLocalGatewayRoutesResponse.proto"
    defaultService
    (Proxy :: Proxy SearchLocalGatewayRoutes)

responseDescribeMovingAddresses :: DescribeMovingAddressesResponse -> TestTree
responseDescribeMovingAddresses =
  res
    "DescribeMovingAddressesResponse"
    "fixture/DescribeMovingAddressesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeMovingAddresses)

responseCreateTrafficMirrorSession :: CreateTrafficMirrorSessionResponse -> TestTree
responseCreateTrafficMirrorSession =
  res
    "CreateTrafficMirrorSessionResponse"
    "fixture/CreateTrafficMirrorSessionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTrafficMirrorSession)

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

responseRejectTransitGatewayMulticastDomainAssociations :: RejectTransitGatewayMulticastDomainAssociationsResponse -> TestTree
responseRejectTransitGatewayMulticastDomainAssociations =
  res
    "RejectTransitGatewayMulticastDomainAssociationsResponse"
    "fixture/RejectTransitGatewayMulticastDomainAssociationsResponse.proto"
    defaultService
    (Proxy :: Proxy RejectTransitGatewayMulticastDomainAssociations)

responseDeleteTransitGateway :: DeleteTransitGatewayResponse -> TestTree
responseDeleteTransitGateway =
  res
    "DeleteTransitGatewayResponse"
    "fixture/DeleteTransitGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTransitGateway)

responseDescribeHosts :: DescribeHostsResponse -> TestTree
responseDescribeHosts =
  res
    "DescribeHostsResponse"
    "fixture/DescribeHostsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeHosts)

responseDescribeNetworkInterfacePermissions :: DescribeNetworkInterfacePermissionsResponse -> TestTree
responseDescribeNetworkInterfacePermissions =
  res
    "DescribeNetworkInterfacePermissionsResponse"
    "fixture/DescribeNetworkInterfacePermissionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeNetworkInterfacePermissions)

responseGetVpnConnectionDeviceSampleConfiguration :: GetVpnConnectionDeviceSampleConfigurationResponse -> TestTree
responseGetVpnConnectionDeviceSampleConfiguration =
  res
    "GetVpnConnectionDeviceSampleConfigurationResponse"
    "fixture/GetVpnConnectionDeviceSampleConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy GetVpnConnectionDeviceSampleConfiguration)

responseDescribeVpcEndpointServicePermissions :: DescribeVpcEndpointServicePermissionsResponse -> TestTree
responseDescribeVpcEndpointServicePermissions =
  res
    "DescribeVpcEndpointServicePermissionsResponse"
    "fixture/DescribeVpcEndpointServicePermissionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeVpcEndpointServicePermissions)

responseDescribeHostReservationOfferings :: DescribeHostReservationOfferingsResponse -> TestTree
responseDescribeHostReservationOfferings =
  res
    "DescribeHostReservationOfferingsResponse"
    "fixture/DescribeHostReservationOfferingsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeHostReservationOfferings)

responseDescribeVpcEndpointConnections :: DescribeVpcEndpointConnectionsResponse -> TestTree
responseDescribeVpcEndpointConnections =
  res
    "DescribeVpcEndpointConnectionsResponse"
    "fixture/DescribeVpcEndpointConnectionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeVpcEndpointConnections)

responseDescribeFpgaImageAttribute :: DescribeFpgaImageAttributeResponse -> TestTree
responseDescribeFpgaImageAttribute =
  res
    "DescribeFpgaImageAttributeResponse"
    "fixture/DescribeFpgaImageAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeFpgaImageAttribute)

responseEnableImageDeprecation :: EnableImageDeprecationResponse -> TestTree
responseEnableImageDeprecation =
  res
    "EnableImageDeprecationResponse"
    "fixture/EnableImageDeprecationResponse.proto"
    defaultService
    (Proxy :: Proxy EnableImageDeprecation)

responseResetImageAttribute :: ResetImageAttributeResponse -> TestTree
responseResetImageAttribute =
  res
    "ResetImageAttributeResponse"
    "fixture/ResetImageAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy ResetImageAttribute)

responseAdvertiseByoipCidr :: AdvertiseByoipCidrResponse -> TestTree
responseAdvertiseByoipCidr =
  res
    "AdvertiseByoipCidrResponse"
    "fixture/AdvertiseByoipCidrResponse.proto"
    defaultService
    (Proxy :: Proxy AdvertiseByoipCidr)

responseDescribeTransitGatewayRouteTables :: DescribeTransitGatewayRouteTablesResponse -> TestTree
responseDescribeTransitGatewayRouteTables =
  res
    "DescribeTransitGatewayRouteTablesResponse"
    "fixture/DescribeTransitGatewayRouteTablesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTransitGatewayRouteTables)

responseModifyTransitGatewayPrefixListReference :: ModifyTransitGatewayPrefixListReferenceResponse -> TestTree
responseModifyTransitGatewayPrefixListReference =
  res
    "ModifyTransitGatewayPrefixListReferenceResponse"
    "fixture/ModifyTransitGatewayPrefixListReferenceResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyTransitGatewayPrefixListReference)

responseRegisterInstanceEventNotificationAttributes :: RegisterInstanceEventNotificationAttributesResponse -> TestTree
responseRegisterInstanceEventNotificationAttributes =
  res
    "RegisterInstanceEventNotificationAttributesResponse"
    "fixture/RegisterInstanceEventNotificationAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterInstanceEventNotificationAttributes)

responseDeleteManagedPrefixList :: DeleteManagedPrefixListResponse -> TestTree
responseDeleteManagedPrefixList =
  res
    "DeleteManagedPrefixListResponse"
    "fixture/DeleteManagedPrefixListResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteManagedPrefixList)

responseDescribeRegions :: DescribeRegionsResponse -> TestTree
responseDescribeRegions =
  res
    "DescribeRegionsResponse"
    "fixture/DescribeRegionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeRegions)

responseRevokeSecurityGroupIngress :: RevokeSecurityGroupIngressResponse -> TestTree
responseRevokeSecurityGroupIngress =
  res
    "RevokeSecurityGroupIngressResponse"
    "fixture/RevokeSecurityGroupIngressResponse.proto"
    defaultService
    (Proxy :: Proxy RevokeSecurityGroupIngress)

responseDeleteVpnConnectionRoute :: DeleteVpnConnectionRouteResponse -> TestTree
responseDeleteVpnConnectionRoute =
  res
    "DeleteVpnConnectionRouteResponse"
    "fixture/DeleteVpnConnectionRouteResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVpnConnectionRoute)

responseDescribeNetworkAcls :: DescribeNetworkAclsResponse -> TestTree
responseDescribeNetworkAcls =
  res
    "DescribeNetworkAclsResponse"
    "fixture/DescribeNetworkAclsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeNetworkAcls)

responseDeleteDhcpOptions :: DeleteDhcpOptionsResponse -> TestTree
responseDeleteDhcpOptions =
  res
    "DeleteDhcpOptionsResponse"
    "fixture/DeleteDhcpOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDhcpOptions)

responseDescribeVpnGateways :: DescribeVpnGatewaysResponse -> TestTree
responseDescribeVpnGateways =
  res
    "DescribeVpnGatewaysResponse"
    "fixture/DescribeVpnGatewaysResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeVpnGateways)

responseRegisterTransitGatewayMulticastGroupMembers :: RegisterTransitGatewayMulticastGroupMembersResponse -> TestTree
responseRegisterTransitGatewayMulticastGroupMembers =
  res
    "RegisterTransitGatewayMulticastGroupMembersResponse"
    "fixture/RegisterTransitGatewayMulticastGroupMembersResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterTransitGatewayMulticastGroupMembers)

responseDescribeHostReservations :: DescribeHostReservationsResponse -> TestTree
responseDescribeHostReservations =
  res
    "DescribeHostReservationsResponse"
    "fixture/DescribeHostReservationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeHostReservations)

responseRejectVpcPeeringConnection :: RejectVpcPeeringConnectionResponse -> TestTree
responseRejectVpcPeeringConnection =
  res
    "RejectVpcPeeringConnectionResponse"
    "fixture/RejectVpcPeeringConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy RejectVpcPeeringConnection)

responseCreateEgressOnlyInternetGateway :: CreateEgressOnlyInternetGatewayResponse -> TestTree
responseCreateEgressOnlyInternetGateway =
  res
    "CreateEgressOnlyInternetGatewayResponse"
    "fixture/CreateEgressOnlyInternetGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy CreateEgressOnlyInternetGateway)

responseDeleteSubnetCidrReservation :: DeleteSubnetCidrReservationResponse -> TestTree
responseDeleteSubnetCidrReservation =
  res
    "DeleteSubnetCidrReservationResponse"
    "fixture/DeleteSubnetCidrReservationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSubnetCidrReservation)

responseEnableVpcClassicLinkDnsSupport :: EnableVpcClassicLinkDnsSupportResponse -> TestTree
responseEnableVpcClassicLinkDnsSupport =
  res
    "EnableVpcClassicLinkDnsSupportResponse"
    "fixture/EnableVpcClassicLinkDnsSupportResponse.proto"
    defaultService
    (Proxy :: Proxy EnableVpcClassicLinkDnsSupport)

responseAllocateAddress :: AllocateAddressResponse -> TestTree
responseAllocateAddress =
  res
    "AllocateAddressResponse"
    "fixture/AllocateAddressResponse.proto"
    defaultService
    (Proxy :: Proxy AllocateAddress)

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

responseModifyDefaultCreditSpecification :: ModifyDefaultCreditSpecificationResponse -> TestTree
responseModifyDefaultCreditSpecification =
  res
    "ModifyDefaultCreditSpecificationResponse"
    "fixture/ModifyDefaultCreditSpecificationResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyDefaultCreditSpecification)

responseGetManagedPrefixListEntries :: GetManagedPrefixListEntriesResponse -> TestTree
responseGetManagedPrefixListEntries =
  res
    "GetManagedPrefixListEntriesResponse"
    "fixture/GetManagedPrefixListEntriesResponse.proto"
    defaultService
    (Proxy :: Proxy GetManagedPrefixListEntries)

responseDisableVpcClassicLinkDnsSupport :: DisableVpcClassicLinkDnsSupportResponse -> TestTree
responseDisableVpcClassicLinkDnsSupport =
  res
    "DisableVpcClassicLinkDnsSupportResponse"
    "fixture/DisableVpcClassicLinkDnsSupportResponse.proto"
    defaultService
    (Proxy :: Proxy DisableVpcClassicLinkDnsSupport)

responseDisableImageDeprecation :: DisableImageDeprecationResponse -> TestTree
responseDisableImageDeprecation =
  res
    "DisableImageDeprecationResponse"
    "fixture/DisableImageDeprecationResponse.proto"
    defaultService
    (Proxy :: Proxy DisableImageDeprecation)

responseApplySecurityGroupsToClientVpnTargetNetwork :: ApplySecurityGroupsToClientVpnTargetNetworkResponse -> TestTree
responseApplySecurityGroupsToClientVpnTargetNetwork =
  res
    "ApplySecurityGroupsToClientVpnTargetNetworkResponse"
    "fixture/ApplySecurityGroupsToClientVpnTargetNetworkResponse.proto"
    defaultService
    (Proxy :: Proxy ApplySecurityGroupsToClientVpnTargetNetwork)

responseCreateLaunchTemplateVersion :: CreateLaunchTemplateVersionResponse -> TestTree
responseCreateLaunchTemplateVersion =
  res
    "CreateLaunchTemplateVersionResponse"
    "fixture/CreateLaunchTemplateVersionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateLaunchTemplateVersion)

responseCreateVpnConnectionRoute :: CreateVpnConnectionRouteResponse -> TestTree
responseCreateVpnConnectionRoute =
  res
    "CreateVpnConnectionRouteResponse"
    "fixture/CreateVpnConnectionRouteResponse.proto"
    defaultService
    (Proxy :: Proxy CreateVpnConnectionRoute)

responseDisassociateInstanceEventWindow :: DisassociateInstanceEventWindowResponse -> TestTree
responseDisassociateInstanceEventWindow =
  res
    "DisassociateInstanceEventWindowResponse"
    "fixture/DisassociateInstanceEventWindowResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateInstanceEventWindow)

responseDescribeConversionTasks :: DescribeConversionTasksResponse -> TestTree
responseDescribeConversionTasks =
  res
    "DescribeConversionTasksResponse"
    "fixture/DescribeConversionTasksResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeConversionTasks)

responseDeleteTrafficMirrorSession :: DeleteTrafficMirrorSessionResponse -> TestTree
responseDeleteTrafficMirrorSession =
  res
    "DeleteTrafficMirrorSessionResponse"
    "fixture/DeleteTrafficMirrorSessionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTrafficMirrorSession)

responseCreateTransitGateway :: CreateTransitGatewayResponse -> TestTree
responseCreateTransitGateway =
  res
    "CreateTransitGatewayResponse"
    "fixture/CreateTransitGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTransitGateway)

responseCreateSnapshots :: CreateSnapshotsResponse -> TestTree
responseCreateSnapshots =
  res
    "CreateSnapshotsResponse"
    "fixture/CreateSnapshotsResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSnapshots)

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

responseDescribeKeyPairs :: DescribeKeyPairsResponse -> TestTree
responseDescribeKeyPairs =
  res
    "DescribeKeyPairsResponse"
    "fixture/DescribeKeyPairsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeKeyPairs)

responseCreateFpgaImage :: CreateFpgaImageResponse -> TestTree
responseCreateFpgaImage =
  res
    "CreateFpgaImageResponse"
    "fixture/CreateFpgaImageResponse.proto"
    defaultService
    (Proxy :: Proxy CreateFpgaImage)

responseDescribeClassicLinkInstances :: DescribeClassicLinkInstancesResponse -> TestTree
responseDescribeClassicLinkInstances =
  res
    "DescribeClassicLinkInstancesResponse"
    "fixture/DescribeClassicLinkInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeClassicLinkInstances)

responseDeleteTrafficMirrorFilterRule :: DeleteTrafficMirrorFilterRuleResponse -> TestTree
responseDeleteTrafficMirrorFilterRule =
  res
    "DeleteTrafficMirrorFilterRuleResponse"
    "fixture/DeleteTrafficMirrorFilterRuleResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTrafficMirrorFilterRule)

responseTerminateInstances :: TerminateInstancesResponse -> TestTree
responseTerminateInstances =
  res
    "TerminateInstancesResponse"
    "fixture/TerminateInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy TerminateInstances)

responseModifyFpgaImageAttribute :: ModifyFpgaImageAttributeResponse -> TestTree
responseModifyFpgaImageAttribute =
  res
    "ModifyFpgaImageAttributeResponse"
    "fixture/ModifyFpgaImageAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyFpgaImageAttribute)

responseWithdrawByoipCidr :: WithdrawByoipCidrResponse -> TestTree
responseWithdrawByoipCidr =
  res
    "WithdrawByoipCidrResponse"
    "fixture/WithdrawByoipCidrResponse.proto"
    defaultService
    (Proxy :: Proxy WithdrawByoipCidr)

responseAttachVpnGateway :: AttachVpnGatewayResponse -> TestTree
responseAttachVpnGateway =
  res
    "AttachVpnGatewayResponse"
    "fixture/AttachVpnGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy AttachVpnGateway)

responseAcceptTransitGatewayVpcAttachment :: AcceptTransitGatewayVpcAttachmentResponse -> TestTree
responseAcceptTransitGatewayVpcAttachment =
  res
    "AcceptTransitGatewayVpcAttachmentResponse"
    "fixture/AcceptTransitGatewayVpcAttachmentResponse.proto"
    defaultService
    (Proxy :: Proxy AcceptTransitGatewayVpcAttachment)

responseDeleteSpotDatafeedSubscription :: DeleteSpotDatafeedSubscriptionResponse -> TestTree
responseDeleteSpotDatafeedSubscription =
  res
    "DeleteSpotDatafeedSubscriptionResponse"
    "fixture/DeleteSpotDatafeedSubscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSpotDatafeedSubscription)

responseDescribeExportImageTasks :: DescribeExportImageTasksResponse -> TestTree
responseDescribeExportImageTasks =
  res
    "DescribeExportImageTasksResponse"
    "fixture/DescribeExportImageTasksResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeExportImageTasks)

responseCreateCarrierGateway :: CreateCarrierGatewayResponse -> TestTree
responseCreateCarrierGateway =
  res
    "CreateCarrierGatewayResponse"
    "fixture/CreateCarrierGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCarrierGateway)

responseAttachInternetGateway :: AttachInternetGatewayResponse -> TestTree
responseAttachInternetGateway =
  res
    "AttachInternetGatewayResponse"
    "fixture/AttachInternetGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy AttachInternetGateway)

responseDescribeClientVpnTargetNetworks :: DescribeClientVpnTargetNetworksResponse -> TestTree
responseDescribeClientVpnTargetNetworks =
  res
    "DescribeClientVpnTargetNetworksResponse"
    "fixture/DescribeClientVpnTargetNetworksResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeClientVpnTargetNetworks)

responseCreateTrafficMirrorFilter :: CreateTrafficMirrorFilterResponse -> TestTree
responseCreateTrafficMirrorFilter =
  res
    "CreateTrafficMirrorFilterResponse"
    "fixture/CreateTrafficMirrorFilterResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTrafficMirrorFilter)

responseDeleteNetworkInsightsAnalysis :: DeleteNetworkInsightsAnalysisResponse -> TestTree
responseDeleteNetworkInsightsAnalysis =
  res
    "DeleteNetworkInsightsAnalysisResponse"
    "fixture/DeleteNetworkInsightsAnalysisResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteNetworkInsightsAnalysis)

responseDescribeIamInstanceProfileAssociations :: DescribeIamInstanceProfileAssociationsResponse -> TestTree
responseDescribeIamInstanceProfileAssociations =
  res
    "DescribeIamInstanceProfileAssociationsResponse"
    "fixture/DescribeIamInstanceProfileAssociationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeIamInstanceProfileAssociations)

responseImportKeyPair :: ImportKeyPairResponse -> TestTree
responseImportKeyPair =
  res
    "ImportKeyPairResponse"
    "fixture/ImportKeyPairResponse.proto"
    defaultService
    (Proxy :: Proxy ImportKeyPair)

responseEnableVpcClassicLink :: EnableVpcClassicLinkResponse -> TestTree
responseEnableVpcClassicLink =
  res
    "EnableVpcClassicLinkResponse"
    "fixture/EnableVpcClassicLinkResponse.proto"
    defaultService
    (Proxy :: Proxy EnableVpcClassicLink)

responseDescribeNetworkInsightsAnalyses :: DescribeNetworkInsightsAnalysesResponse -> TestTree
responseDescribeNetworkInsightsAnalyses =
  res
    "DescribeNetworkInsightsAnalysesResponse"
    "fixture/DescribeNetworkInsightsAnalysesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeNetworkInsightsAnalyses)

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

responseCreateCapacityReservation :: CreateCapacityReservationResponse -> TestTree
responseCreateCapacityReservation =
  res
    "CreateCapacityReservationResponse"
    "fixture/CreateCapacityReservationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCapacityReservation)

responseDeleteInstanceEventWindow :: DeleteInstanceEventWindowResponse -> TestTree
responseDeleteInstanceEventWindow =
  res
    "DeleteInstanceEventWindowResponse"
    "fixture/DeleteInstanceEventWindowResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteInstanceEventWindow)

responseModifyEbsDefaultKmsKeyId :: ModifyEbsDefaultKmsKeyIdResponse -> TestTree
responseModifyEbsDefaultKmsKeyId =
  res
    "ModifyEbsDefaultKmsKeyIdResponse"
    "fixture/ModifyEbsDefaultKmsKeyIdResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyEbsDefaultKmsKeyId)

responseProvisionByoipCidr :: ProvisionByoipCidrResponse -> TestTree
responseProvisionByoipCidr =
  res
    "ProvisionByoipCidrResponse"
    "fixture/ProvisionByoipCidrResponse.proto"
    defaultService
    (Proxy :: Proxy ProvisionByoipCidr)

responseDeleteTransitGatewayConnect :: DeleteTransitGatewayConnectResponse -> TestTree
responseDeleteTransitGatewayConnect =
  res
    "DeleteTransitGatewayConnectResponse"
    "fixture/DeleteTransitGatewayConnectResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTransitGatewayConnect)

responseModifyVpnTunnelOptions :: ModifyVpnTunnelOptionsResponse -> TestTree
responseModifyVpnTunnelOptions =
  res
    "ModifyVpnTunnelOptionsResponse"
    "fixture/ModifyVpnTunnelOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyVpnTunnelOptions)

responseCreateTransitGatewayPeeringAttachment :: CreateTransitGatewayPeeringAttachmentResponse -> TestTree
responseCreateTransitGatewayPeeringAttachment =
  res
    "CreateTransitGatewayPeeringAttachmentResponse"
    "fixture/CreateTransitGatewayPeeringAttachmentResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTransitGatewayPeeringAttachment)

responseCreateCustomerGateway :: CreateCustomerGatewayResponse -> TestTree
responseCreateCustomerGateway =
  res
    "CreateCustomerGatewayResponse"
    "fixture/CreateCustomerGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCustomerGateway)

responseModifyVolume :: ModifyVolumeResponse -> TestTree
responseModifyVolume =
  res
    "ModifyVolumeResponse"
    "fixture/ModifyVolumeResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyVolume)

responseModifyInstancePlacement :: ModifyInstancePlacementResponse -> TestTree
responseModifyInstancePlacement =
  res
    "ModifyInstancePlacementResponse"
    "fixture/ModifyInstancePlacementResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyInstancePlacement)

responseCreateSnapshot :: Snapshot -> TestTree
responseCreateSnapshot =
  res
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSnapshot)

responseDescribeInstanceAttribute :: DescribeInstanceAttributeResponse -> TestTree
responseDescribeInstanceAttribute =
  res
    "DescribeInstanceAttributeResponse"
    "fixture/DescribeInstanceAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInstanceAttribute)

responseCreateReservedInstancesListing :: CreateReservedInstancesListingResponse -> TestTree
responseCreateReservedInstancesListing =
  res
    "CreateReservedInstancesListingResponse"
    "fixture/CreateReservedInstancesListingResponse.proto"
    defaultService
    (Proxy :: Proxy CreateReservedInstancesListing)

responseDeleteSecurityGroup :: DeleteSecurityGroupResponse -> TestTree
responseDeleteSecurityGroup =
  res
    "DeleteSecurityGroupResponse"
    "fixture/DeleteSecurityGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSecurityGroup)

responsePurchaseScheduledInstances :: PurchaseScheduledInstancesResponse -> TestTree
responsePurchaseScheduledInstances =
  res
    "PurchaseScheduledInstancesResponse"
    "fixture/PurchaseScheduledInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy PurchaseScheduledInstances)

responseDescribePublicIpv4Pools :: DescribePublicIpv4PoolsResponse -> TestTree
responseDescribePublicIpv4Pools =
  res
    "DescribePublicIpv4PoolsResponse"
    "fixture/DescribePublicIpv4PoolsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePublicIpv4Pools)

responseDescribeLocalGatewayRouteTableVpcAssociations :: DescribeLocalGatewayRouteTableVpcAssociationsResponse -> TestTree
responseDescribeLocalGatewayRouteTableVpcAssociations =
  res
    "DescribeLocalGatewayRouteTableVpcAssociationsResponse"
    "fixture/DescribeLocalGatewayRouteTableVpcAssociationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLocalGatewayRouteTableVpcAssociations)

responseAuthorizeClientVpnIngress :: AuthorizeClientVpnIngressResponse -> TestTree
responseAuthorizeClientVpnIngress =
  res
    "AuthorizeClientVpnIngressResponse"
    "fixture/AuthorizeClientVpnIngressResponse.proto"
    defaultService
    (Proxy :: Proxy AuthorizeClientVpnIngress)

responseCreateVpcPeeringConnection :: CreateVpcPeeringConnectionResponse -> TestTree
responseCreateVpcPeeringConnection =
  res
    "CreateVpcPeeringConnectionResponse"
    "fixture/CreateVpcPeeringConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateVpcPeeringConnection)

responseCreateInstanceEventWindow :: CreateInstanceEventWindowResponse -> TestTree
responseCreateInstanceEventWindow =
  res
    "CreateInstanceEventWindowResponse"
    "fixture/CreateInstanceEventWindowResponse.proto"
    defaultService
    (Proxy :: Proxy CreateInstanceEventWindow)

responseCreateSecurityGroup :: CreateSecurityGroupResponse -> TestTree
responseCreateSecurityGroup =
  res
    "CreateSecurityGroupResponse"
    "fixture/CreateSecurityGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSecurityGroup)

responseDescribeInternetGateways :: DescribeInternetGatewaysResponse -> TestTree
responseDescribeInternetGateways =
  res
    "DescribeInternetGatewaysResponse"
    "fixture/DescribeInternetGatewaysResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInternetGateways)

responseModifyLaunchTemplate :: ModifyLaunchTemplateResponse -> TestTree
responseModifyLaunchTemplate =
  res
    "ModifyLaunchTemplateResponse"
    "fixture/ModifyLaunchTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyLaunchTemplate)

responseModifyInstanceAttribute :: ModifyInstanceAttributeResponse -> TestTree
responseModifyInstanceAttribute =
  res
    "ModifyInstanceAttributeResponse"
    "fixture/ModifyInstanceAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyInstanceAttribute)

responseResetEbsDefaultKmsKeyId :: ResetEbsDefaultKmsKeyIdResponse -> TestTree
responseResetEbsDefaultKmsKeyId =
  res
    "ResetEbsDefaultKmsKeyIdResponse"
    "fixture/ResetEbsDefaultKmsKeyIdResponse.proto"
    defaultService
    (Proxy :: Proxy ResetEbsDefaultKmsKeyId)

responseGetEbsEncryptionByDefault :: GetEbsEncryptionByDefaultResponse -> TestTree
responseGetEbsEncryptionByDefault =
  res
    "GetEbsEncryptionByDefaultResponse"
    "fixture/GetEbsEncryptionByDefaultResponse.proto"
    defaultService
    (Proxy :: Proxy GetEbsEncryptionByDefault)

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

responseAssociateInstanceEventWindow :: AssociateInstanceEventWindowResponse -> TestTree
responseAssociateInstanceEventWindow =
  res
    "AssociateInstanceEventWindowResponse"
    "fixture/AssociateInstanceEventWindowResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateInstanceEventWindow)

responseReplaceRoute :: ReplaceRouteResponse -> TestTree
responseReplaceRoute =
  res
    "ReplaceRouteResponse"
    "fixture/ReplaceRouteResponse.proto"
    defaultService
    (Proxy :: Proxy ReplaceRoute)

responseCreateTags :: CreateTagsResponse -> TestTree
responseCreateTags =
  res
    "CreateTagsResponse"
    "fixture/CreateTagsResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTags)

responseCreateTransitGatewayConnect :: CreateTransitGatewayConnectResponse -> TestTree
responseCreateTransitGatewayConnect =
  res
    "CreateTransitGatewayConnectResponse"
    "fixture/CreateTransitGatewayConnectResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTransitGatewayConnect)

responseDeleteLaunchTemplateVersions :: DeleteLaunchTemplateVersionsResponse -> TestTree
responseDeleteLaunchTemplateVersions =
  res
    "DeleteLaunchTemplateVersionsResponse"
    "fixture/DeleteLaunchTemplateVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteLaunchTemplateVersions)

responseGetDefaultCreditSpecification :: GetDefaultCreditSpecificationResponse -> TestTree
responseGetDefaultCreditSpecification =
  res
    "GetDefaultCreditSpecificationResponse"
    "fixture/GetDefaultCreditSpecificationResponse.proto"
    defaultService
    (Proxy :: Proxy GetDefaultCreditSpecification)

responseResetSnapshotAttribute :: ResetSnapshotAttributeResponse -> TestTree
responseResetSnapshotAttribute =
  res
    "ResetSnapshotAttributeResponse"
    "fixture/ResetSnapshotAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy ResetSnapshotAttribute)

responseDeleteTransitGatewayPeeringAttachment :: DeleteTransitGatewayPeeringAttachmentResponse -> TestTree
responseDeleteTransitGatewayPeeringAttachment =
  res
    "DeleteTransitGatewayPeeringAttachmentResponse"
    "fixture/DeleteTransitGatewayPeeringAttachmentResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTransitGatewayPeeringAttachment)

responseCreateRoute :: CreateRouteResponse -> TestTree
responseCreateRoute =
  res
    "CreateRouteResponse"
    "fixture/CreateRouteResponse.proto"
    defaultService
    (Proxy :: Proxy CreateRoute)

responseBundleInstance :: BundleInstanceResponse -> TestTree
responseBundleInstance =
  res
    "BundleInstanceResponse"
    "fixture/BundleInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy BundleInstance)

responseReplaceNetworkAclAssociation :: ReplaceNetworkAclAssociationResponse -> TestTree
responseReplaceNetworkAclAssociation =
  res
    "ReplaceNetworkAclAssociationResponse"
    "fixture/ReplaceNetworkAclAssociationResponse.proto"
    defaultService
    (Proxy :: Proxy ReplaceNetworkAclAssociation)

responseDeleteTrafficMirrorFilter :: DeleteTrafficMirrorFilterResponse -> TestTree
responseDeleteTrafficMirrorFilter =
  res
    "DeleteTrafficMirrorFilterResponse"
    "fixture/DeleteTrafficMirrorFilterResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTrafficMirrorFilter)

responseDescribeIdentityIdFormat :: DescribeIdentityIdFormatResponse -> TestTree
responseDescribeIdentityIdFormat =
  res
    "DescribeIdentityIdFormatResponse"
    "fixture/DescribeIdentityIdFormatResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeIdentityIdFormat)

responseReplaceTransitGatewayRoute :: ReplaceTransitGatewayRouteResponse -> TestTree
responseReplaceTransitGatewayRoute =
  res
    "ReplaceTransitGatewayRouteResponse"
    "fixture/ReplaceTransitGatewayRouteResponse.proto"
    defaultService
    (Proxy :: Proxy ReplaceTransitGatewayRoute)

responseGetCoipPoolUsage :: GetCoipPoolUsageResponse -> TestTree
responseGetCoipPoolUsage =
  res
    "GetCoipPoolUsageResponse"
    "fixture/GetCoipPoolUsageResponse.proto"
    defaultService
    (Proxy :: Proxy GetCoipPoolUsage)

responseDeleteTransitGatewayVpcAttachment :: DeleteTransitGatewayVpcAttachmentResponse -> TestTree
responseDeleteTransitGatewayVpcAttachment =
  res
    "DeleteTransitGatewayVpcAttachmentResponse"
    "fixture/DeleteTransitGatewayVpcAttachmentResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTransitGatewayVpcAttachment)

responseCreatePlacementGroup :: CreatePlacementGroupResponse -> TestTree
responseCreatePlacementGroup =
  res
    "CreatePlacementGroupResponse"
    "fixture/CreatePlacementGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePlacementGroup)

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

responseCancelSpotFleetRequests :: CancelSpotFleetRequestsResponse -> TestTree
responseCancelSpotFleetRequests =
  res
    "CancelSpotFleetRequestsResponse"
    "fixture/CancelSpotFleetRequestsResponse.proto"
    defaultService
    (Proxy :: Proxy CancelSpotFleetRequests)

responseDescribeClientVpnAuthorizationRules :: DescribeClientVpnAuthorizationRulesResponse -> TestTree
responseDescribeClientVpnAuthorizationRules =
  res
    "DescribeClientVpnAuthorizationRulesResponse"
    "fixture/DescribeClientVpnAuthorizationRulesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeClientVpnAuthorizationRules)

responseDeleteSubnet :: DeleteSubnetResponse -> TestTree
responseDeleteSubnet =
  res
    "DeleteSubnetResponse"
    "fixture/DeleteSubnetResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSubnet)

responseStopInstances :: StopInstancesResponse -> TestTree
responseStopInstances =
  res
    "StopInstancesResponse"
    "fixture/StopInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy StopInstances)

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

responseCreateDefaultVpc :: CreateDefaultVpcResponse -> TestTree
responseCreateDefaultVpc =
  res
    "CreateDefaultVpcResponse"
    "fixture/CreateDefaultVpcResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDefaultVpc)

responseCreateTransitGatewayPrefixListReference :: CreateTransitGatewayPrefixListReferenceResponse -> TestTree
responseCreateTransitGatewayPrefixListReference =
  res
    "CreateTransitGatewayPrefixListReferenceResponse"
    "fixture/CreateTransitGatewayPrefixListReferenceResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTransitGatewayPrefixListReference)

responseDescribeDhcpOptions :: DescribeDhcpOptionsResponse -> TestTree
responseDescribeDhcpOptions =
  res
    "DescribeDhcpOptionsResponse"
    "fixture/DescribeDhcpOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDhcpOptions)

responseCreateTransitGatewayRoute :: CreateTransitGatewayRouteResponse -> TestTree
responseCreateTransitGatewayRoute =
  res
    "CreateTransitGatewayRouteResponse"
    "fixture/CreateTransitGatewayRouteResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTransitGatewayRoute)

responseResetNetworkInterfaceAttribute :: ResetNetworkInterfaceAttributeResponse -> TestTree
responseResetNetworkInterfaceAttribute =
  res
    "ResetNetworkInterfaceAttributeResponse"
    "fixture/ResetNetworkInterfaceAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy ResetNetworkInterfaceAttribute)

responseDeleteTransitGatewayMulticastDomain :: DeleteTransitGatewayMulticastDomainResponse -> TestTree
responseDeleteTransitGatewayMulticastDomain =
  res
    "DeleteTransitGatewayMulticastDomainResponse"
    "fixture/DeleteTransitGatewayMulticastDomainResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTransitGatewayMulticastDomain)

responseDeleteNetworkInterface :: DeleteNetworkInterfaceResponse -> TestTree
responseDeleteNetworkInterface =
  res
    "DeleteNetworkInterfaceResponse"
    "fixture/DeleteNetworkInterfaceResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteNetworkInterface)

responseDisassociateVpcCidrBlock :: DisassociateVpcCidrBlockResponse -> TestTree
responseDisassociateVpcCidrBlock =
  res
    "DisassociateVpcCidrBlockResponse"
    "fixture/DisassociateVpcCidrBlockResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateVpcCidrBlock)

responseReplaceRouteTableAssociation :: ReplaceRouteTableAssociationResponse -> TestTree
responseReplaceRouteTableAssociation =
  res
    "ReplaceRouteTableAssociationResponse"
    "fixture/ReplaceRouteTableAssociationResponse.proto"
    defaultService
    (Proxy :: Proxy ReplaceRouteTableAssociation)

responseCreateNetworkInsightsPath :: CreateNetworkInsightsPathResponse -> TestTree
responseCreateNetworkInsightsPath =
  res
    "CreateNetworkInsightsPathResponse"
    "fixture/CreateNetworkInsightsPathResponse.proto"
    defaultService
    (Proxy :: Proxy CreateNetworkInsightsPath)

responseDeregisterTransitGatewayMulticastGroupSources :: DeregisterTransitGatewayMulticastGroupSourcesResponse -> TestTree
responseDeregisterTransitGatewayMulticastGroupSources =
  res
    "DeregisterTransitGatewayMulticastGroupSourcesResponse"
    "fixture/DeregisterTransitGatewayMulticastGroupSourcesResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterTransitGatewayMulticastGroupSources)

responseDescribeCustomerGateways :: DescribeCustomerGatewaysResponse -> TestTree
responseDescribeCustomerGateways =
  res
    "DescribeCustomerGatewaysResponse"
    "fixture/DescribeCustomerGatewaysResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCustomerGateways)

responseDescribeCapacityReservations :: DescribeCapacityReservationsResponse -> TestTree
responseDescribeCapacityReservations =
  res
    "DescribeCapacityReservationsResponse"
    "fixture/DescribeCapacityReservationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCapacityReservations)

responseModifyTrafficMirrorSession :: ModifyTrafficMirrorSessionResponse -> TestTree
responseModifyTrafficMirrorSession =
  res
    "ModifyTrafficMirrorSessionResponse"
    "fixture/ModifyTrafficMirrorSessionResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyTrafficMirrorSession)

responseDisassociateTransitGatewayRouteTable :: DisassociateTransitGatewayRouteTableResponse -> TestTree
responseDisassociateTransitGatewayRouteTable =
  res
    "DisassociateTransitGatewayRouteTableResponse"
    "fixture/DisassociateTransitGatewayRouteTableResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateTransitGatewayRouteTable)

responseStartInstances :: StartInstancesResponse -> TestTree
responseStartInstances =
  res
    "StartInstancesResponse"
    "fixture/StartInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy StartInstances)

responseDescribeTransitGatewayPeeringAttachments :: DescribeTransitGatewayPeeringAttachmentsResponse -> TestTree
responseDescribeTransitGatewayPeeringAttachments =
  res
    "DescribeTransitGatewayPeeringAttachmentsResponse"
    "fixture/DescribeTransitGatewayPeeringAttachmentsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTransitGatewayPeeringAttachments)

responseImportImage :: ImportImageResponse -> TestTree
responseImportImage =
  res
    "ImportImageResponse"
    "fixture/ImportImageResponse.proto"
    defaultService
    (Proxy :: Proxy ImportImage)

responseDescribeTrafficMirrorFilters :: DescribeTrafficMirrorFiltersResponse -> TestTree
responseDescribeTrafficMirrorFilters =
  res
    "DescribeTrafficMirrorFiltersResponse"
    "fixture/DescribeTrafficMirrorFiltersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTrafficMirrorFilters)

responseAssociateAddress :: AssociateAddressResponse -> TestTree
responseAssociateAddress =
  res
    "AssociateAddressResponse"
    "fixture/AssociateAddressResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateAddress)

responseRunScheduledInstances :: RunScheduledInstancesResponse -> TestTree
responseRunScheduledInstances =
  res
    "RunScheduledInstancesResponse"
    "fixture/RunScheduledInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy RunScheduledInstances)

responseCopyFpgaImage :: CopyFpgaImageResponse -> TestTree
responseCopyFpgaImage =
  res
    "CopyFpgaImageResponse"
    "fixture/CopyFpgaImageResponse.proto"
    defaultService
    (Proxy :: Proxy CopyFpgaImage)

responseDeleteNatGateway :: DeleteNatGatewayResponse -> TestTree
responseDeleteNatGateway =
  res
    "DeleteNatGatewayResponse"
    "fixture/DeleteNatGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteNatGateway)
