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
--             modifyCapacityReservation
--
--         , requestGetAssociatedIPv6PoolCidrs $
--             getAssociatedIPv6PoolCidrs
--
--         , requestImportInstance $
--             importInstance
--
--         , requestRevokeSecurityGroupEgress $
--             revokeSecurityGroupEgress
--
--         , requestCreateNetworkInterfacePermission $
--             createNetworkInterfacePermission
--
--         , requestSendDiagnosticInterrupt $
--             sendDiagnosticInterrupt
--
--         , requestDeleteLaunchTemplate $
--             deleteLaunchTemplate
--
--         , requestRejectVPCEndpointConnections $
--             rejectVPCEndpointConnections
--
--         , requestCreateVPNGateway $
--             createVPNGateway
--
--         , requestCreateNetworkACL $
--             createNetworkACL
--
--         , requestDeleteKeyPair $
--             deleteKeyPair
--
--         , requestDescribeSecurityGroupReferences $
--             describeSecurityGroupReferences
--
--         , requestDeleteFleets $
--             deleteFleets
--
--         , requestDescribeTags $
--             describeTags
--
--         , requestCreateTransitGatewayRouteTable $
--             createTransitGatewayRouteTable
--
--         , requestModifyInstanceMetadataOptions $
--             modifyInstanceMetadataOptions
--
--         , requestUpdateSecurityGroupRuleDescriptionsIngress $
--             updateSecurityGroupRuleDescriptionsIngress
--
--         , requestDisassociateSubnetCidrBlock $
--             disassociateSubnetCidrBlock
--
--         , requestDetachNetworkInterface $
--             detachNetworkInterface
--
--         , requestDetachInternetGateway $
--             detachInternetGateway
--
--         , requestDeleteVPCEndpoints $
--             deleteVPCEndpoints
--
--         , requestDescribeClientVPNEndpoints $
--             describeClientVPNEndpoints
--
--         , requestDeleteFlowLogs $
--             deleteFlowLogs
--
--         , requestDescribeVPCClassicLink $
--             describeVPCClassicLink
--
--         , requestGetAssociatedEnclaveCertificateIAMRoles $
--             getAssociatedEnclaveCertificateIAMRoles
--
--         , requestAssociateTransitGatewayMulticastDomain $
--             associateTransitGatewayMulticastDomain
--
--         , requestModifySubnetAttribute $
--             modifySubnetAttribute
--
--         , requestDetachVolume $
--             detachVolume
--
--         , requestDescribeInstanceCreditSpecifications $
--             describeInstanceCreditSpecifications
--
--         , requestCancelBundleTask $
--             cancelBundleTask
--
--         , requestDescribeByoipCidrs $
--             describeByoipCidrs
--
--         , requestAcceptReservedInstancesExchangeQuote $
--             acceptReservedInstancesExchangeQuote
--
--         , requestReleaseAddress $
--             releaseAddress
--
--         , requestDescribeInstanceTypeOfferings $
--             describeInstanceTypeOfferings
--
--         , requestCreateInternetGateway $
--             createInternetGateway
--
--         , requestDeleteVPNConnection $
--             deleteVPNConnection
--
--         , requestDescribeBundleTasks $
--             describeBundleTasks
--
--         , requestAuthorizeSecurityGroupEgress $
--             authorizeSecurityGroupEgress
--
--         , requestEnableTransitGatewayRouteTablePropagation $
--             enableTransitGatewayRouteTablePropagation
--
--         , requestDeregisterImage $
--             deregisterImage
--
--         , requestDeleteVPCEndpointConnectionNotifications $
--             deleteVPCEndpointConnectionNotifications
--
--         , requestDescribeCoipPools $
--             describeCoipPools
--
--         , requestGetTransitGatewayMulticastDomainAssociations $
--             getTransitGatewayMulticastDomainAssociations
--
--         , requestDeleteLocalGatewayRouteTableVPCAssociation $
--             deleteLocalGatewayRouteTableVPCAssociation
--
--         , requestModifyNetworkInterfaceAttribute $
--             modifyNetworkInterfaceAttribute
--
--         , requestModifyVPCTenancy $
--             modifyVPCTenancy
--
--         , requestDescribeInstanceTypes $
--             describeInstanceTypes
--
--         , requestDescribeClientVPNAuthorizationRules $
--             describeClientVPNAuthorizationRules
--
--         , requestDeleteTransitGatewayVPCAttachment $
--             deleteTransitGatewayVPCAttachment
--
--         , requestDeleteTransitGatewayMulticastDomain $
--             deleteTransitGatewayMulticastDomain
--
--         , requestCancelReservedInstancesListing $
--             cancelReservedInstancesListing
--
--         , requestAttachClassicLinkVPC $
--             attachClassicLinkVPC
--
--         , requestDisableTransitGatewayRouteTablePropagation $
--             disableTransitGatewayRouteTablePropagation
--
--         , requestDescribeVPCClassicLinkDNSSupport $
--             describeVPCClassicLinkDNSSupport
--
--         , requestAssociateSubnetCidrBlock $
--             associateSubnetCidrBlock
--
--         , requestRunScheduledInstances $
--             runScheduledInstances
--
--         , requestCreateTransitGatewayRoute $
--             createTransitGatewayRoute
--
--         , requestCreateTransitGatewayPrefixListReference $
--             createTransitGatewayPrefixListReference
--
--         , requestCancelSpotFleetRequests $
--             cancelSpotFleetRequests
--
--         , requestDescribeSpotPriceHistory $
--             describeSpotPriceHistory
--
--         , requestDescribeDHCPOptions $
--             describeDHCPOptions
--
--         , requestImportImage $
--             importImage
--
--         , requestCreateLocalGatewayRouteTableVPCAssociation $
--             createLocalGatewayRouteTableVPCAssociation
--
--         , requestCopyFpgaImage $
--             copyFpgaImage
--
--         , requestImportClientVPNClientCertificateRevocationList $
--             importClientVPNClientCertificateRevocationList
--
--         , requestStopInstances $
--             stopInstances
--
--         , requestEnableEBSEncryptionByDefault $
--             enableEBSEncryptionByDefault
--
--         , requestDeregisterTransitGatewayMulticastGroupSources $
--             deregisterTransitGatewayMulticastGroupSources
--
--         , requestModifyLaunchTemplate $
--             modifyLaunchTemplate
--
--         , requestModifyVPCEndpointConnectionNotification $
--             modifyVPCEndpointConnectionNotification
--
--         , requestDescribeInternetGateways $
--             describeInternetGateways
--
--         , requestDisableVPCClassicLink $
--             disableVPCClassicLink
--
--         , requestGetGroupsForCapacityReservation $
--             getGroupsForCapacityReservation
--
--         , requestDeleteLaunchTemplateVersions $
--             deleteLaunchTemplateVersions
--
--         , requestBundleInstance $
--             bundleInstance
--
--         , requestDescribeNetworkInterfaces $
--             describeNetworkInterfaces
--
--         , requestReplaceNetworkACLAssociation $
--             replaceNetworkACLAssociation
--
--         , requestDescribeNatGateways $
--             describeNatGateways
--
--         , requestDescribeAddresses $
--             describeAddresses
--
--         , requestRestoreManagedPrefixListVersion $
--             restoreManagedPrefixListVersion
--
--         , requestDescribeSnapshotAttribute $
--             describeSnapshotAttribute
--
--         , requestDescribeIdentityIdFormat $
--             describeIdentityIdFormat
--
--         , requestReplaceRoute $
--             replaceRoute
--
--         , requestDescribeVPCEndpointServices $
--             describeVPCEndpointServices
--
--         , requestDeleteLocalGatewayRoute $
--             deleteLocalGatewayRoute
--
--         , requestAuthorizeSecurityGroupIngress $
--             authorizeSecurityGroupIngress
--
--         , requestCreateVPCPeeringConnection $
--             createVPCPeeringConnection
--
--         , requestDescribeSubnets $
--             describeSubnets
--
--         , requestGetTransitGatewayAttachmentPropagations $
--             getTransitGatewayAttachmentPropagations
--
--         , requestCreateTags $
--             createTags
--
--         , requestPurchaseReservedInstancesOffering $
--             purchaseReservedInstancesOffering
--
--         , requestDeleteNetworkACLEntry $
--             deleteNetworkACLEntry
--
--         , requestResetSnapshotAttribute $
--             resetSnapshotAttribute
--
--         , requestDescribeVPNConnections $
--             describeVPNConnections
--
--         , requestModifyInstanceEventStartTime $
--             modifyInstanceEventStartTime
--
--         , requestDeleteRoute $
--             deleteRoute
--
--         , requestReplaceNetworkACLEntry $
--             replaceNetworkACLEntry
--
--         , requestDescribeVPCEndpoints $
--             describeVPCEndpoints
--
--         , requestCreateTrafficMirrorFilter $
--             createTrafficMirrorFilter
--
--         , requestResetInstanceAttribute $
--             resetInstanceAttribute
--
--         , requestModifyIdentityIdFormat $
--             modifyIdentityIdFormat
--
--         , requestAttachNetworkInterface $
--             attachNetworkInterface
--
--         , requestCreateCapacityReservation $
--             createCapacityReservation
--
--         , requestDescribeInstanceStatus $
--             describeInstanceStatus
--
--         , requestImportKeyPair $
--             importKeyPair
--
--         , requestDeleteTags $
--             deleteTags
--
--         , requestConfirmProductInstance $
--             confirmProductInstance
--
--         , requestDescribeInstanceAttribute $
--             describeInstanceAttribute
--
--         , requestDescribeReservedInstancesOfferings $
--             describeReservedInstancesOfferings
--
--         , requestCreateCustomerGateway $
--             createCustomerGateway
--
--         , requestDescribeFleets $
--             describeFleets
--
--         , requestCreateTransitGatewayPeeringAttachment $
--             createTransitGatewayPeeringAttachment
--
--         , requestDeleteSecurityGroup $
--             deleteSecurityGroup
--
--         , requestDescribePublicIPv4Pools $
--             describePublicIPv4Pools
--
--         , requestDescribeClientVPNTargetNetworks $
--             describeClientVPNTargetNetworks
--
--         , requestDeleteVPCPeeringConnection $
--             deleteVPCPeeringConnection
--
--         , requestAttachInternetGateway $
--             attachInternetGateway
--
--         , requestModifyInstancePlacement $
--             modifyInstancePlacement
--
--         , requestDescribeFlowLogs $
--             describeFlowLogs
--
--         , requestDescribeLocalGatewayVirtualInterfaceGroups $
--             describeLocalGatewayVirtualInterfaceGroups
--
--         , requestDescribeLocalGatewayRouteTableVPCAssociations $
--             describeLocalGatewayRouteTableVPCAssociations
--
--         , requestDescribeVPCEndpointConnectionNotifications $
--             describeVPCEndpointConnectionNotifications
--
--         , requestGetManagedPrefixListEntries $
--             getManagedPrefixListEntries
--
--         , requestRunInstances $
--             runInstances
--
--         , requestCreateSnapshots $
--             createSnapshots
--
--         , requestAssociateDHCPOptions $
--             associateDHCPOptions
--
--         , requestDeleteTrafficMirrorFilterRule $
--             deleteTrafficMirrorFilterRule
--
--         , requestDescribeReservedInstances $
--             describeReservedInstances
--
--         , requestDescribeIdFormat $
--             describeIdFormat
--
--         , requestDescribeVPCs $
--             describeVPCs
--
--         , requestDescribeConversionTasks $
--             describeConversionTasks
--
--         , requestCreateLaunchTemplateVersion $
--             createLaunchTemplateVersion
--
--         , requestGetManagedPrefixListAssociations $
--             getManagedPrefixListAssociations
--
--         , requestDisableVPCClassicLinkDNSSupport $
--             disableVPCClassicLinkDNSSupport
--
--         , requestApplySecurityGroupsToClientVPNTargetNetwork $
--             applySecurityGroupsToClientVPNTargetNetwork
--
--         , requestDescribeTrafficMirrorTargets $
--             describeTrafficMirrorTargets
--
--         , requestDescribeVolumesModifications $
--             describeVolumesModifications
--
--         , requestExportImage $
--             exportImage
--
--         , requestCreateFpgaImage $
--             createFpgaImage
--
--         , requestAcceptVPCEndpointConnections $
--             acceptVPCEndpointConnections
--
--         , requestDeleteClientVPNEndpoint $
--             deleteClientVPNEndpoint
--
--         , requestSearchTransitGatewayRoutes $
--             searchTransitGatewayRoutes
--
--         , requestGetLaunchTemplateData $
--             getLaunchTemplateData
--
--         , requestAllocateAddress $
--             allocateAddress
--
--         , requestAcceptTransitGatewayVPCAttachment $
--             acceptTransitGatewayVPCAttachment
--
--         , requestCancelConversionTask $
--             cancelConversionTask
--
--         , requestModifyImageAttribute $
--             modifyImageAttribute
--
--         , requestCreateRouteTable $
--             createRouteTable
--
--         , requestRejectTransitGatewayPeeringAttachment $
--             rejectTransitGatewayPeeringAttachment
--
--         , requestReportInstanceStatus $
--             reportInstanceStatus
--
--         , requestAttachVolume $
--             attachVolume
--
--         , requestRequestSpotInstances $
--             requestSpotInstances
--
--         , requestWithdrawByoipCidr $
--             withdrawByoipCidr
--
--         , requestDescribeHostReservationOfferings $
--             describeHostReservationOfferings
--
--         , requestResetFpgaImageAttribute $
--             resetFpgaImageAttribute
--
--         , requestModifyVPNConnection $
--             modifyVPNConnection
--
--         , requestCreateTrafficMirrorFilterRule $
--             createTrafficMirrorFilterRule
--
--         , requestDeleteTransitGateway $
--             deleteTransitGateway
--
--         , requestStartVPCEndpointServicePrivateDNSVerification $
--             startVPCEndpointServicePrivateDNSVerification
--
--         , requestDescribeVolumes $
--             describeVolumes
--
--         , requestRejectVPCPeeringConnection $
--             rejectVPCPeeringConnection
--
--         , requestDescribeClientVPNRoutes $
--             describeClientVPNRoutes
--
--         , requestDeleteVPNConnectionRoute $
--             deleteVPNConnectionRoute
--
--         , requestAssociateEnclaveCertificateIAMRole $
--             associateEnclaveCertificateIAMRole
--
--         , requestModifyVPCEndpoint $
--             modifyVPCEndpoint
--
--         , requestDescribeFpgaImageAttribute $
--             describeFpgaImageAttribute
--
--         , requestAllocateHosts $
--             allocateHosts
--
--         , requestCreateClientVPNEndpoint $
--             createClientVPNEndpoint
--
--         , requestCreateTrafficMirrorSession $
--             createTrafficMirrorSession
--
--         , requestRegisterImage $
--             registerImage
--
--         , requestAdvertiseByoipCidr $
--             advertiseByoipCidr
--
--         , requestModifyFleet $
--             modifyFleet
--
--         , requestRevokeSecurityGroupIngress $
--             revokeSecurityGroupIngress
--
--         , requestGetEBSDefaultKMSKeyId $
--             getEBSDefaultKMSKeyId
--
--         , requestDescribeHostReservations $
--             describeHostReservations
--
--         , requestUpdateSecurityGroupRuleDescriptionsEgress $
--             updateSecurityGroupRuleDescriptionsEgress
--
--         , requestEnableVPCClassicLinkDNSSupport $
--             enableVPCClassicLinkDNSSupport
--
--         , requestDescribeVPCEndpointConnections $
--             describeVPCEndpointConnections
--
--         , requestModifyReservedInstances $
--             modifyReservedInstances
--
--         , requestDeleteFpgaImage $
--             deleteFpgaImage
--
--         , requestDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations $
--             describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
--
--         , requestDescribeScheduledInstances $
--             describeScheduledInstances
--
--         , requestSearchTransitGatewayMulticastGroups $
--             searchTransitGatewayMulticastGroups
--
--         , requestCreateFlowLogs $
--             createFlowLogs
--
--         , requestDescribeSpotFleetRequests $
--             describeSpotFleetRequests
--
--         , requestMoveAddressToVPC $
--             moveAddressToVPC
--
--         , requestDescribeFleetInstances $
--             describeFleetInstances
--
--         , requestDescribeLaunchTemplateVersions $
--             describeLaunchTemplateVersions
--
--         , requestModifyInstanceCreditSpecification $
--             modifyInstanceCreditSpecification
--
--         , requestDescribePrincipalIdFormat $
--             describePrincipalIdFormat
--
--         , requestDescribeTransitGateways $
--             describeTransitGateways
--
--         , requestDeleteNetworkACL $
--             deleteNetworkACL
--
--         , requestDisassociateTransitGatewayMulticastDomain $
--             disassociateTransitGatewayMulticastDomain
--
--         , requestDeleteTransitGatewayRouteTable $
--             deleteTransitGatewayRouteTable
--
--         , requestCreateLaunchTemplate $
--             createLaunchTemplate
--
--         , requestCreateVPCEndpointConnectionNotification $
--             createVPCEndpointConnectionNotification
--
--         , requestDeleteNetworkInterfacePermission $
--             deleteNetworkInterfacePermission
--
--         , requestDeleteVPNGateway $
--             deleteVPNGateway
--
--         , requestCreateTrafficMirrorTarget $
--             createTrafficMirrorTarget
--
--         , requestDescribeImportImageTasks $
--             describeImportImageTasks
--
--         , requestDescribeVolumeAttribute $
--             describeVolumeAttribute
--
--         , requestDescribeMovingAddresses $
--             describeMovingAddresses
--
--         , requestExportTransitGatewayRoutes $
--             exportTransitGatewayRoutes
--
--         , requestGetPasswordData $
--             getPasswordData
--
--         , requestCreateVPC $
--             createVPC
--
--         , requestModifyVPCPeeringConnectionOptions $
--             modifyVPCPeeringConnectionOptions
--
--         , requestDescribeFpgaImages $
--             describeFpgaImages
--
--         , requestCopySnapshot $
--             copySnapshot
--
--         , requestAcceptTransitGatewayPeeringAttachment $
--             acceptTransitGatewayPeeringAttachment
--
--         , requestDisassociateAddress $
--             disassociateAddress
--
--         , requestModifyTrafficMirrorFilterNetworkServices $
--             modifyTrafficMirrorFilterNetworkServices
--
--         , requestDescribeEgressOnlyInternetGateways $
--             describeEgressOnlyInternetGateways
--
--         , requestDeleteVPC $
--             deleteVPC
--
--         , requestCreateInstanceExportTask $
--             createInstanceExportTask
--
--         , requestRejectTransitGatewayVPCAttachment $
--             rejectTransitGatewayVPCAttachment
--
--         , requestDescribeTrafficMirrorSessions $
--             describeTrafficMirrorSessions
--
--         , requestGetTransitGatewayRouteTableAssociations $
--             getTransitGatewayRouteTableAssociations
--
--         , requestAssociateVPCCidrBlock $
--             associateVPCCidrBlock
--
--         , requestDescribeVPCAttribute $
--             describeVPCAttribute
--
--         , requestCreateVolume $
--             createVolume
--
--         , requestCreateDefaultSubnet $
--             createDefaultSubnet
--
--         , requestDescribeScheduledInstanceAvailability $
--             describeScheduledInstanceAvailability
--
--         , requestDisassociateClientVPNTargetNetwork $
--             disassociateClientVPNTargetNetwork
--
--         , requestCreateClientVPNRoute $
--             createClientVPNRoute
--
--         , requestModifyVolumeAttribute $
--             modifyVolumeAttribute
--
--         , requestExportClientVPNClientConfiguration $
--             exportClientVPNClientConfiguration
--
--         , requestDeleteTrafficMirrorTarget $
--             deleteTrafficMirrorTarget
--
--         , requestDescribeSpotDatafeedSubscription $
--             describeSpotDatafeedSubscription
--
--         , requestDescribeLocalGatewayRouteTables $
--             describeLocalGatewayRouteTables
--
--         , requestDescribePrefixLists $
--             describePrefixLists
--
--         , requestAssociateTransitGatewayRouteTable $
--             associateTransitGatewayRouteTable
--
--         , requestDeletePlacementGroup $
--             deletePlacementGroup
--
--         , requestModifyTransitGateway $
--             modifyTransitGateway
--
--         , requestDeleteTransitGatewayPrefixListReference $
--             deleteTransitGatewayPrefixListReference
--
--         , requestCreateTransitGatewayMulticastDomain $
--             createTransitGatewayMulticastDomain
--
--         , requestDeregisterInstanceEventNotificationAttributes $
--             deregisterInstanceEventNotificationAttributes
--
--         , requestRequestSpotFleet $
--             requestSpotFleet
--
--         , requestDeleteTransitGatewayRoute $
--             deleteTransitGatewayRoute
--
--         , requestDisableEBSEncryptionByDefault $
--             disableEBSEncryptionByDefault
--
--         , requestDeregisterTransitGatewayMulticastGroupMembers $
--             deregisterTransitGatewayMulticastGroupMembers
--
--         , requestCreateSubnet $
--             createSubnet
--
--         , requestCreateNetworkInterface $
--             createNetworkInterface
--
--         , requestDescribeSecurityGroups $
--             describeSecurityGroups
--
--         , requestGetCapacityReservationUsage $
--             getCapacityReservationUsage
--
--         , requestCreateTransitGatewayVPCAttachment $
--             createTransitGatewayVPCAttachment
--
--         , requestDescribeExportTasks $
--             describeExportTasks
--
--         , requestModifySpotFleetRequest $
--             modifySpotFleetRequest
--
--         , requestDetachVPNGateway $
--             detachVPNGateway
--
--         , requestModifyManagedPrefixList $
--             modifyManagedPrefixList
--
--         , requestGetHostReservationPurchasePreview $
--             getHostReservationPurchasePreview
--
--         , requestEnableVolumeIO $
--             enableVolumeIO
--
--         , requestDescribeInstances $
--             describeInstances
--
--         , requestCreateNatGateway $
--             createNatGateway
--
--         , requestDescribeLocalGatewayVirtualInterfaces $
--             describeLocalGatewayVirtualInterfaces
--
--         , requestDescribeVPCPeeringConnections $
--             describeVPCPeeringConnections
--
--         , requestCancelExportTask $
--             cancelExportTask
--
--         , requestCreateVPCEndpointServiceConfiguration $
--             createVPCEndpointServiceConfiguration
--
--         , requestCreateDefaultVPC $
--             createDefaultVPC
--
--         , requestDisassociateVPCCidrBlock $
--             disassociateVPCCidrBlock
--
--         , requestDescribeTrafficMirrorFilters $
--             describeTrafficMirrorFilters
--
--         , requestDescribeFastSnapshotRestores $
--             describeFastSnapshotRestores
--
--         , requestCancelCapacityReservation $
--             cancelCapacityReservation
--
--         , requestDeleteNetworkInterface $
--             deleteNetworkInterface
--
--         , requestDisassociateTransitGatewayRouteTable $
--             disassociateTransitGatewayRouteTable
--
--         , requestReplaceRouteTableAssociation $
--             replaceRouteTableAssociation
--
--         , requestStartInstances $
--             startInstances
--
--         , requestCreatePlacementGroup $
--             createPlacementGroup
--
--         , requestDescribeInstanceEventNotificationAttributes $
--             describeInstanceEventNotificationAttributes
--
--         , requestDescribeCapacityReservations $
--             describeCapacityReservations
--
--         , requestModifyClientVPNEndpoint $
--             modifyClientVPNEndpoint
--
--         , requestModifyInstanceCapacityReservationAttributes $
--             modifyInstanceCapacityReservationAttributes
--
--         , requestDescribeAggregateIdFormat $
--             describeAggregateIdFormat
--
--         , requestDescribeSnapshots $
--             describeSnapshots
--
--         , requestAssociateAddress $
--             associateAddress
--
--         , requestModifyTrafficMirrorFilterRule $
--             modifyTrafficMirrorFilterRule
--
--         , requestDescribeNetworkInterfaceAttribute $
--             describeNetworkInterfaceAttribute
--
--         , requestReplaceIAMInstanceProfileAssociation $
--             replaceIAMInstanceProfileAssociation
--
--         , requestAssociateClientVPNTargetNetwork $
--             associateClientVPNTargetNetwork
--
--         , requestReleaseHosts $
--             releaseHosts
--
--         , requestResetNetworkInterfaceAttribute $
--             resetNetworkInterfaceAttribute
--
--         , requestDeleteInternetGateway $
--             deleteInternetGateway
--
--         , requestDescribeReservedInstancesListings $
--             describeReservedInstancesListings
--
--         , requestCreateVPNConnection $
--             createVPNConnection
--
--         , requestReplaceTransitGatewayRoute $
--             replaceTransitGatewayRoute
--
--         , requestCreateFleet $
--             createFleet
--
--         , requestDeleteNatGateway $
--             deleteNatGateway
--
--         , requestDescribeImportSnapshotTasks $
--             describeImportSnapshotTasks
--
--         , requestGetCoipPoolUsage $
--             getCoipPoolUsage
--
--         , requestDescribeCustomerGateways $
--             describeCustomerGateways
--
--         , requestDeleteSubnet $
--             deleteSubnet
--
--         , requestCopyImage $
--             copyImage
--
--         , requestCreateVPCEndpoint $
--             createVPCEndpoint
--
--         , requestModifyTrafficMirrorSession $
--             modifyTrafficMirrorSession
--
--         , requestDescribeCarrierGateways $
--             describeCarrierGateways
--
--         , requestDescribeTransitGatewayPeeringAttachments $
--             describeTransitGatewayPeeringAttachments
--
--         , requestDeleteQueuedReservedInstances $
--             deleteQueuedReservedInstances
--
--         , requestDescribeTransitGatewayMulticastDomains $
--             describeTransitGatewayMulticastDomains
--
--         , requestGetDefaultCreditSpecification $
--             getDefaultCreditSpecification
--
--         , requestUnmonitorInstances $
--             unmonitorInstances
--
--         , requestDescribeTransitGatewayVPCAttachments $
--             describeTransitGatewayVPCAttachments
--
--         , requestCreateSecurityGroup $
--             createSecurityGroup
--
--         , requestGetEBSEncryptionByDefault $
--             getEBSEncryptionByDefault
--
--         , requestImportVolume $
--             importVolume
--
--         , requestDeleteCarrierGateway $
--             deleteCarrierGateway
--
--         , requestDisableVGWRoutePropagation $
--             disableVGWRoutePropagation
--
--         , requestDeleteTrafficMirrorFilter $
--             deleteTrafficMirrorFilter
--
--         , requestModifyVPNTunnelCertificate $
--             modifyVPNTunnelCertificate
--
--         , requestCreateSpotDatafeedSubscription $
--             createSpotDatafeedSubscription
--
--         , requestCancelSpotInstanceRequests $
--             cancelSpotInstanceRequests
--
--         , requestCreateRoute $
--             createRoute
--
--         , requestDescribeVPCEndpointServiceConfigurations $
--             describeVPCEndpointServiceConfigurations
--
--         , requestDeleteSnapshot $
--             deleteSnapshot
--
--         , requestAssignPrivateIPAddresses $
--             assignPrivateIPAddresses
--
--         , requestAuthorizeClientVPNIngress $
--             authorizeClientVPNIngress
--
--         , requestDeleteTransitGatewayPeeringAttachment $
--             deleteTransitGatewayPeeringAttachment
--
--         , requestModifyInstanceAttribute $
--             modifyInstanceAttribute
--
--         , requestDeleteCustomerGateway $
--             deleteCustomerGateway
--
--         , requestDisassociateIAMInstanceProfile $
--             disassociateIAMInstanceProfile
--
--         , requestTerminateClientVPNConnections $
--             terminateClientVPNConnections
--
--         , requestDisassociateRouteTable $
--             disassociateRouteTable
--
--         , requestGetConsoleScreenshot $
--             getConsoleScreenshot
--
--         , requestResetEBSDefaultKMSKeyId $
--             resetEBSDefaultKMSKeyId
--
--         , requestAssignIPv6Addresses $
--             assignIPv6Addresses
--
--         , requestModifyVPNTunnelOptions $
--             modifyVPNTunnelOptions
--
--         , requestModifyEBSDefaultKMSKeyId $
--             modifyEBSDefaultKMSKeyId
--
--         , requestDeleteSpotDatafeedSubscription $
--             deleteSpotDatafeedSubscription
--
--         , requestModifyVolume $
--             modifyVolume
--
--         , requestEnableVPCClassicLink $
--             enableVPCClassicLink
--
--         , requestDescribePlacementGroups $
--             describePlacementGroups
--
--         , requestProvisionByoipCidr $
--             provisionByoipCidr
--
--         , requestDisassociateEnclaveCertificateIAMRole $
--             disassociateEnclaveCertificateIAMRole
--
--         , requestModifyAvailabilityZoneGroup $
--             modifyAvailabilityZoneGroup
--
--         , requestDescribeStaleSecurityGroups $
--             describeStaleSecurityGroups
--
--         , requestCreateCarrierGateway $
--             createCarrierGateway
--
--         , requestDescribeExportImageTasks $
--             describeExportImageTasks
--
--         , requestPurchaseScheduledInstances $
--             purchaseScheduledInstances
--
--         , requestEnableVGWRoutePropagation $
--             enableVGWRoutePropagation
--
--         , requestDescribeSpotFleetRequestHistory $
--             describeSpotFleetRequestHistory
--
--         , requestModifySnapshotAttribute $
--             modifySnapshotAttribute
--
--         , requestDescribeIAMInstanceProfileAssociations $
--             describeIAMInstanceProfileAssociations
--
--         , requestCreateSnapshot $
--             createSnapshot
--
--         , requestCreateLocalGatewayRoute $
--             createLocalGatewayRoute
--
--         , requestCreateNetworkACLEntry $
--             createNetworkACLEntry
--
--         , requestDescribeTransitGatewayAttachments $
--             describeTransitGatewayAttachments
--
--         , requestCreateReservedInstancesListing $
--             createReservedInstancesListing
--
--         , requestDescribeIPv6Pools $
--             describeIPv6Pools
--
--         , requestAttachVPNGateway $
--             attachVPNGateway
--
--         , requestDescribeLocalGateways $
--             describeLocalGateways
--
--         , requestModifyVPCEndpointServicePermissions $
--             modifyVPCEndpointServicePermissions
--
--         , requestExportClientVPNClientCertificateRevocationList $
--             exportClientVPNClientCertificateRevocationList
--
--         , requestCreateDHCPOptions $
--             createDHCPOptions
--
--         , requestRegisterTransitGatewayMulticastGroupSources $
--             registerTransitGatewayMulticastGroupSources
--
--         , requestDescribeAccountAttributes $
--             describeAccountAttributes
--
--         , requestGetTransitGatewayRouteTablePropagations $
--             getTransitGatewayRouteTablePropagations
--
--         , requestModifyFpgaImageAttribute $
--             modifyFpgaImageAttribute
--
--         , requestModifyHosts $
--             modifyHosts
--
--         , requestRebootInstances $
--             rebootInstances
--
--         , requestModifyVPCEndpointServiceConfiguration $
--             modifyVPCEndpointServiceConfiguration
--
--         , requestCreateTransitGateway $
--             createTransitGateway
--
--         , requestUnassignIPv6Addresses $
--             unassignIPv6Addresses
--
--         , requestDeleteTrafficMirrorSession $
--             deleteTrafficMirrorSession
--
--         , requestCreateManagedPrefixList $
--             createManagedPrefixList
--
--         , requestAssociateIAMInstanceProfile $
--             associateIAMInstanceProfile
--
--         , requestModifyDefaultCreditSpecification $
--             modifyDefaultCreditSpecification
--
--         , requestDeleteEgressOnlyInternetGateway $
--             deleteEgressOnlyInternetGateway
--
--         , requestPurchaseHostReservation $
--             purchaseHostReservation
--
--         , requestModifyTransitGatewayVPCAttachment $
--             modifyTransitGatewayVPCAttachment
--
--         , requestCreateImage $
--             createImage
--
--         , requestDescribeClassicLinkInstances $
--             describeClassicLinkInstances
--
--         , requestTerminateInstances $
--             terminateInstances
--
--         , requestGetTransitGatewayPrefixListReferences $
--             getTransitGatewayPrefixListReferences
--
--         , requestDescribeKeyPairs $
--             describeKeyPairs
--
--         , requestDisableFastSnapshotRestores $
--             disableFastSnapshotRestores
--
--         , requestDescribeLaunchTemplates $
--             describeLaunchTemplates
--
--         , requestCreateVPNConnectionRoute $
--             createVPNConnectionRoute
--
--         , requestAssociateRouteTable $
--             associateRouteTable
--
--         , requestDescribeVPNGateways $
--             describeVPNGateways
--
--         , requestModifyVPNConnectionOptions $
--             modifyVPNConnectionOptions
--
--         , requestGetConsoleOutput $
--             getConsoleOutput
--
--         , requestDescribeHosts $
--             describeHosts
--
--         , requestDescribeImageAttribute $
--             describeImageAttribute
--
--         , requestModifyIdFormat $
--             modifyIdFormat
--
--         , requestRegisterTransitGatewayMulticastGroupMembers $
--             registerTransitGatewayMulticastGroupMembers
--
--         , requestDeleteManagedPrefixList $
--             deleteManagedPrefixList
--
--         , requestDeleteRouteTable $
--             deleteRouteTable
--
--         , requestResetImageAttribute $
--             resetImageAttribute
--
--         , requestModifyTransitGatewayPrefixListReference $
--             modifyTransitGatewayPrefixListReference
--
--         , requestDescribeTransitGatewayRouteTables $
--             describeTransitGatewayRouteTables
--
--         , requestCreateEgressOnlyInternetGateway $
--             createEgressOnlyInternetGateway
--
--         , requestDescribeReservedInstancesModifications $
--             describeReservedInstancesModifications
--
--         , requestDescribeSpotInstanceRequests $
--             describeSpotInstanceRequests
--
--         , requestRevokeClientVPNIngress $
--             revokeClientVPNIngress
--
--         , requestUnassignPrivateIPAddresses $
--             unassignPrivateIPAddresses
--
--         , requestDescribeNetworkInterfacePermissions $
--             describeNetworkInterfacePermissions
--
--         , requestEnableFastSnapshotRestores $
--             enableFastSnapshotRestores
--
--         , requestDescribeVPCEndpointServicePermissions $
--             describeVPCEndpointServicePermissions
--
--         , requestDeleteDHCPOptions $
--             deleteDHCPOptions
--
--         , requestRegisterInstanceEventNotificationAttributes $
--             registerInstanceEventNotificationAttributes
--
--         , requestDescribeNetworkACLs $
--             describeNetworkACLs
--
--         , requestCancelImportTask $
--             cancelImportTask
--
--         , requestDetachClassicLinkVPC $
--             detachClassicLinkVPC
--
--         , requestDescribeRegions $
--             describeRegions
--
--         , requestMonitorInstances $
--             monitorInstances
--
--         , requestSearchLocalGatewayRoutes $
--             searchLocalGatewayRoutes
--
--         , requestDeleteClientVPNRoute $
--             deleteClientVPNRoute
--
--         , requestAcceptVPCPeeringConnection $
--             acceptVPCPeeringConnection
--
--         , requestImportSnapshot $
--             importSnapshot
--
--         , requestDescribeVolumeStatus $
--             describeVolumeStatus
--
--         , requestDescribeRouteTables $
--             describeRouteTables
--
--         , requestDescribeAvailabilityZones $
--             describeAvailabilityZones
--
--         , requestModifyVPCAttribute $
--             modifyVPCAttribute
--
--         , requestDescribeClientVPNConnections $
--             describeClientVPNConnections
--
--         , requestDescribeFleetHistory $
--             describeFleetHistory
--
--         , requestDescribeImages $
--             describeImages
--
--         , requestDescribeElasticGpus $
--             describeElasticGpus
--
--         , requestRestoreAddressToClassic $
--             restoreAddressToClassic
--
--         , requestDescribeManagedPrefixLists $
--             describeManagedPrefixLists
--
--         , requestCreateKeyPair $
--             createKeyPair
--
--         , requestGetReservedInstancesExchangeQuote $
--             getReservedInstancesExchangeQuote
--
--         , requestDeleteVolume $
--             deleteVolume
--
--         , requestDeprovisionByoipCidr $
--             deprovisionByoipCidr
--
--         , requestDeleteVPCEndpointServiceConfigurations $
--             deleteVPCEndpointServiceConfigurations
--
--         , requestDescribeSpotFleetInstances $
--             describeSpotFleetInstances
--
--           ]

--     , testGroup "response"
--         [ responseModifyCapacityReservation $
--             modifyCapacityReservationResponse
--
--         , responseGetAssociatedIPv6PoolCidrs $
--             getAssociatedIPv6PoolCidrsResponse
--
--         , responseImportInstance $
--             importInstanceResponse
--
--         , responseRevokeSecurityGroupEgress $
--             revokeSecurityGroupEgressResponse
--
--         , responseCreateNetworkInterfacePermission $
--             createNetworkInterfacePermissionResponse
--
--         , responseSendDiagnosticInterrupt $
--             sendDiagnosticInterruptResponse
--
--         , responseDeleteLaunchTemplate $
--             deleteLaunchTemplateResponse
--
--         , responseRejectVPCEndpointConnections $
--             rejectVPCEndpointConnectionsResponse
--
--         , responseCreateVPNGateway $
--             createVPNGatewayResponse
--
--         , responseCreateNetworkACL $
--             createNetworkACLResponse
--
--         , responseDeleteKeyPair $
--             deleteKeyPairResponse
--
--         , responseDescribeSecurityGroupReferences $
--             describeSecurityGroupReferencesResponse
--
--         , responseDeleteFleets $
--             deleteFleetsResponse
--
--         , responseDescribeTags $
--             describeTagsResponse
--
--         , responseCreateTransitGatewayRouteTable $
--             createTransitGatewayRouteTableResponse
--
--         , responseModifyInstanceMetadataOptions $
--             modifyInstanceMetadataOptionsResponse
--
--         , responseUpdateSecurityGroupRuleDescriptionsIngress $
--             updateSecurityGroupRuleDescriptionsIngressResponse
--
--         , responseDisassociateSubnetCidrBlock $
--             disassociateSubnetCidrBlockResponse
--
--         , responseDetachNetworkInterface $
--             detachNetworkInterfaceResponse
--
--         , responseDetachInternetGateway $
--             detachInternetGatewayResponse
--
--         , responseDeleteVPCEndpoints $
--             deleteVPCEndpointsResponse
--
--         , responseDescribeClientVPNEndpoints $
--             describeClientVPNEndpointsResponse
--
--         , responseDeleteFlowLogs $
--             deleteFlowLogsResponse
--
--         , responseDescribeVPCClassicLink $
--             describeVPCClassicLinkResponse
--
--         , responseGetAssociatedEnclaveCertificateIAMRoles $
--             getAssociatedEnclaveCertificateIAMRolesResponse
--
--         , responseAssociateTransitGatewayMulticastDomain $
--             associateTransitGatewayMulticastDomainResponse
--
--         , responseModifySubnetAttribute $
--             modifySubnetAttributeResponse
--
--         , responseDetachVolume $
--             volumeAttachment
--
--         , responseDescribeInstanceCreditSpecifications $
--             describeInstanceCreditSpecificationsResponse
--
--         , responseCancelBundleTask $
--             cancelBundleTaskResponse
--
--         , responseDescribeByoipCidrs $
--             describeByoipCidrsResponse
--
--         , responseAcceptReservedInstancesExchangeQuote $
--             acceptReservedInstancesExchangeQuoteResponse
--
--         , responseReleaseAddress $
--             releaseAddressResponse
--
--         , responseDescribeInstanceTypeOfferings $
--             describeInstanceTypeOfferingsResponse
--
--         , responseCreateInternetGateway $
--             createInternetGatewayResponse
--
--         , responseDeleteVPNConnection $
--             deleteVPNConnectionResponse
--
--         , responseDescribeBundleTasks $
--             describeBundleTasksResponse
--
--         , responseAuthorizeSecurityGroupEgress $
--             authorizeSecurityGroupEgressResponse
--
--         , responseEnableTransitGatewayRouteTablePropagation $
--             enableTransitGatewayRouteTablePropagationResponse
--
--         , responseDeregisterImage $
--             deregisterImageResponse
--
--         , responseDeleteVPCEndpointConnectionNotifications $
--             deleteVPCEndpointConnectionNotificationsResponse
--
--         , responseDescribeCoipPools $
--             describeCoipPoolsResponse
--
--         , responseGetTransitGatewayMulticastDomainAssociations $
--             getTransitGatewayMulticastDomainAssociationsResponse
--
--         , responseDeleteLocalGatewayRouteTableVPCAssociation $
--             deleteLocalGatewayRouteTableVPCAssociationResponse
--
--         , responseModifyNetworkInterfaceAttribute $
--             modifyNetworkInterfaceAttributeResponse
--
--         , responseModifyVPCTenancy $
--             modifyVPCTenancyResponse
--
--         , responseDescribeInstanceTypes $
--             describeInstanceTypesResponse
--
--         , responseDescribeClientVPNAuthorizationRules $
--             describeClientVPNAuthorizationRulesResponse
--
--         , responseDeleteTransitGatewayVPCAttachment $
--             deleteTransitGatewayVPCAttachmentResponse
--
--         , responseDeleteTransitGatewayMulticastDomain $
--             deleteTransitGatewayMulticastDomainResponse
--
--         , responseCancelReservedInstancesListing $
--             cancelReservedInstancesListingResponse
--
--         , responseAttachClassicLinkVPC $
--             attachClassicLinkVPCResponse
--
--         , responseDisableTransitGatewayRouteTablePropagation $
--             disableTransitGatewayRouteTablePropagationResponse
--
--         , responseDescribeVPCClassicLinkDNSSupport $
--             describeVPCClassicLinkDNSSupportResponse
--
--         , responseAssociateSubnetCidrBlock $
--             associateSubnetCidrBlockResponse
--
--         , responseRunScheduledInstances $
--             runScheduledInstancesResponse
--
--         , responseCreateTransitGatewayRoute $
--             createTransitGatewayRouteResponse
--
--         , responseCreateTransitGatewayPrefixListReference $
--             createTransitGatewayPrefixListReferenceResponse
--
--         , responseCancelSpotFleetRequests $
--             cancelSpotFleetRequestsResponse
--
--         , responseDescribeSpotPriceHistory $
--             describeSpotPriceHistoryResponse
--
--         , responseDescribeDHCPOptions $
--             describeDHCPOptionsResponse
--
--         , responseImportImage $
--             importImageResponse
--
--         , responseCreateLocalGatewayRouteTableVPCAssociation $
--             createLocalGatewayRouteTableVPCAssociationResponse
--
--         , responseCopyFpgaImage $
--             copyFpgaImageResponse
--
--         , responseImportClientVPNClientCertificateRevocationList $
--             importClientVPNClientCertificateRevocationListResponse
--
--         , responseStopInstances $
--             stopInstancesResponse
--
--         , responseEnableEBSEncryptionByDefault $
--             enableEBSEncryptionByDefaultResponse
--
--         , responseDeregisterTransitGatewayMulticastGroupSources $
--             deregisterTransitGatewayMulticastGroupSourcesResponse
--
--         , responseModifyLaunchTemplate $
--             modifyLaunchTemplateResponse
--
--         , responseModifyVPCEndpointConnectionNotification $
--             modifyVPCEndpointConnectionNotificationResponse
--
--         , responseDescribeInternetGateways $
--             describeInternetGatewaysResponse
--
--         , responseDisableVPCClassicLink $
--             disableVPCClassicLinkResponse
--
--         , responseGetGroupsForCapacityReservation $
--             getGroupsForCapacityReservationResponse
--
--         , responseDeleteLaunchTemplateVersions $
--             deleteLaunchTemplateVersionsResponse
--
--         , responseBundleInstance $
--             bundleInstanceResponse
--
--         , responseDescribeNetworkInterfaces $
--             describeNetworkInterfacesResponse
--
--         , responseReplaceNetworkACLAssociation $
--             replaceNetworkACLAssociationResponse
--
--         , responseDescribeNatGateways $
--             describeNatGatewaysResponse
--
--         , responseDescribeAddresses $
--             describeAddressesResponse
--
--         , responseRestoreManagedPrefixListVersion $
--             restoreManagedPrefixListVersionResponse
--
--         , responseDescribeSnapshotAttribute $
--             describeSnapshotAttributeResponse
--
--         , responseDescribeIdentityIdFormat $
--             describeIdentityIdFormatResponse
--
--         , responseReplaceRoute $
--             replaceRouteResponse
--
--         , responseDescribeVPCEndpointServices $
--             describeVPCEndpointServicesResponse
--
--         , responseDeleteLocalGatewayRoute $
--             deleteLocalGatewayRouteResponse
--
--         , responseAuthorizeSecurityGroupIngress $
--             authorizeSecurityGroupIngressResponse
--
--         , responseCreateVPCPeeringConnection $
--             createVPCPeeringConnectionResponse
--
--         , responseDescribeSubnets $
--             describeSubnetsResponse
--
--         , responseGetTransitGatewayAttachmentPropagations $
--             getTransitGatewayAttachmentPropagationsResponse
--
--         , responseCreateTags $
--             createTagsResponse
--
--         , responsePurchaseReservedInstancesOffering $
--             purchaseReservedInstancesOfferingResponse
--
--         , responseDeleteNetworkACLEntry $
--             deleteNetworkACLEntryResponse
--
--         , responseResetSnapshotAttribute $
--             resetSnapshotAttributeResponse
--
--         , responseDescribeVPNConnections $
--             describeVPNConnectionsResponse
--
--         , responseModifyInstanceEventStartTime $
--             modifyInstanceEventStartTimeResponse
--
--         , responseDeleteRoute $
--             deleteRouteResponse
--
--         , responseReplaceNetworkACLEntry $
--             replaceNetworkACLEntryResponse
--
--         , responseDescribeVPCEndpoints $
--             describeVPCEndpointsResponse
--
--         , responseCreateTrafficMirrorFilter $
--             createTrafficMirrorFilterResponse
--
--         , responseResetInstanceAttribute $
--             resetInstanceAttributeResponse
--
--         , responseModifyIdentityIdFormat $
--             modifyIdentityIdFormatResponse
--
--         , responseAttachNetworkInterface $
--             attachNetworkInterfaceResponse
--
--         , responseCreateCapacityReservation $
--             createCapacityReservationResponse
--
--         , responseDescribeInstanceStatus $
--             describeInstanceStatusResponse
--
--         , responseImportKeyPair $
--             importKeyPairResponse
--
--         , responseDeleteTags $
--             deleteTagsResponse
--
--         , responseConfirmProductInstance $
--             confirmProductInstanceResponse
--
--         , responseDescribeInstanceAttribute $
--             describeInstanceAttributeResponse
--
--         , responseDescribeReservedInstancesOfferings $
--             describeReservedInstancesOfferingsResponse
--
--         , responseCreateCustomerGateway $
--             createCustomerGatewayResponse
--
--         , responseDescribeFleets $
--             describeFleetsResponse
--
--         , responseCreateTransitGatewayPeeringAttachment $
--             createTransitGatewayPeeringAttachmentResponse
--
--         , responseDeleteSecurityGroup $
--             deleteSecurityGroupResponse
--
--         , responseDescribePublicIPv4Pools $
--             describePublicIPv4PoolsResponse
--
--         , responseDescribeClientVPNTargetNetworks $
--             describeClientVPNTargetNetworksResponse
--
--         , responseDeleteVPCPeeringConnection $
--             deleteVPCPeeringConnectionResponse
--
--         , responseAttachInternetGateway $
--             attachInternetGatewayResponse
--
--         , responseModifyInstancePlacement $
--             modifyInstancePlacementResponse
--
--         , responseDescribeFlowLogs $
--             describeFlowLogsResponse
--
--         , responseDescribeLocalGatewayVirtualInterfaceGroups $
--             describeLocalGatewayVirtualInterfaceGroupsResponse
--
--         , responseDescribeLocalGatewayRouteTableVPCAssociations $
--             describeLocalGatewayRouteTableVPCAssociationsResponse
--
--         , responseDescribeVPCEndpointConnectionNotifications $
--             describeVPCEndpointConnectionNotificationsResponse
--
--         , responseGetManagedPrefixListEntries $
--             getManagedPrefixListEntriesResponse
--
--         , responseRunInstances $
--             reservation
--
--         , responseCreateSnapshots $
--             createSnapshotsResponse
--
--         , responseAssociateDHCPOptions $
--             associateDHCPOptionsResponse
--
--         , responseDeleteTrafficMirrorFilterRule $
--             deleteTrafficMirrorFilterRuleResponse
--
--         , responseDescribeReservedInstances $
--             describeReservedInstancesResponse
--
--         , responseDescribeIdFormat $
--             describeIdFormatResponse
--
--         , responseDescribeVPCs $
--             describeVPCsResponse
--
--         , responseDescribeConversionTasks $
--             describeConversionTasksResponse
--
--         , responseCreateLaunchTemplateVersion $
--             createLaunchTemplateVersionResponse
--
--         , responseGetManagedPrefixListAssociations $
--             getManagedPrefixListAssociationsResponse
--
--         , responseDisableVPCClassicLinkDNSSupport $
--             disableVPCClassicLinkDNSSupportResponse
--
--         , responseApplySecurityGroupsToClientVPNTargetNetwork $
--             applySecurityGroupsToClientVPNTargetNetworkResponse
--
--         , responseDescribeTrafficMirrorTargets $
--             describeTrafficMirrorTargetsResponse
--
--         , responseDescribeVolumesModifications $
--             describeVolumesModificationsResponse
--
--         , responseExportImage $
--             exportImageResponse
--
--         , responseCreateFpgaImage $
--             createFpgaImageResponse
--
--         , responseAcceptVPCEndpointConnections $
--             acceptVPCEndpointConnectionsResponse
--
--         , responseDeleteClientVPNEndpoint $
--             deleteClientVPNEndpointResponse
--
--         , responseSearchTransitGatewayRoutes $
--             searchTransitGatewayRoutesResponse
--
--         , responseGetLaunchTemplateData $
--             getLaunchTemplateDataResponse
--
--         , responseAllocateAddress $
--             allocateAddressResponse
--
--         , responseAcceptTransitGatewayVPCAttachment $
--             acceptTransitGatewayVPCAttachmentResponse
--
--         , responseCancelConversionTask $
--             cancelConversionTaskResponse
--
--         , responseModifyImageAttribute $
--             modifyImageAttributeResponse
--
--         , responseCreateRouteTable $
--             createRouteTableResponse
--
--         , responseRejectTransitGatewayPeeringAttachment $
--             rejectTransitGatewayPeeringAttachmentResponse
--
--         , responseReportInstanceStatus $
--             reportInstanceStatusResponse
--
--         , responseAttachVolume $
--             volumeAttachment
--
--         , responseRequestSpotInstances $
--             requestSpotInstancesResponse
--
--         , responseWithdrawByoipCidr $
--             withdrawByoipCidrResponse
--
--         , responseDescribeHostReservationOfferings $
--             describeHostReservationOfferingsResponse
--
--         , responseResetFpgaImageAttribute $
--             resetFpgaImageAttributeResponse
--
--         , responseModifyVPNConnection $
--             modifyVPNConnectionResponse
--
--         , responseCreateTrafficMirrorFilterRule $
--             createTrafficMirrorFilterRuleResponse
--
--         , responseDeleteTransitGateway $
--             deleteTransitGatewayResponse
--
--         , responseStartVPCEndpointServicePrivateDNSVerification $
--             startVPCEndpointServicePrivateDNSVerificationResponse
--
--         , responseDescribeVolumes $
--             describeVolumesResponse
--
--         , responseRejectVPCPeeringConnection $
--             rejectVPCPeeringConnectionResponse
--
--         , responseDescribeClientVPNRoutes $
--             describeClientVPNRoutesResponse
--
--         , responseDeleteVPNConnectionRoute $
--             deleteVPNConnectionRouteResponse
--
--         , responseAssociateEnclaveCertificateIAMRole $
--             associateEnclaveCertificateIAMRoleResponse
--
--         , responseModifyVPCEndpoint $
--             modifyVPCEndpointResponse
--
--         , responseDescribeFpgaImageAttribute $
--             describeFpgaImageAttributeResponse
--
--         , responseAllocateHosts $
--             allocateHostsResponse
--
--         , responseCreateClientVPNEndpoint $
--             createClientVPNEndpointResponse
--
--         , responseCreateTrafficMirrorSession $
--             createTrafficMirrorSessionResponse
--
--         , responseRegisterImage $
--             registerImageResponse
--
--         , responseAdvertiseByoipCidr $
--             advertiseByoipCidrResponse
--
--         , responseModifyFleet $
--             modifyFleetResponse
--
--         , responseRevokeSecurityGroupIngress $
--             revokeSecurityGroupIngressResponse
--
--         , responseGetEBSDefaultKMSKeyId $
--             getEBSDefaultKMSKeyIdResponse
--
--         , responseDescribeHostReservations $
--             describeHostReservationsResponse
--
--         , responseUpdateSecurityGroupRuleDescriptionsEgress $
--             updateSecurityGroupRuleDescriptionsEgressResponse
--
--         , responseEnableVPCClassicLinkDNSSupport $
--             enableVPCClassicLinkDNSSupportResponse
--
--         , responseDescribeVPCEndpointConnections $
--             describeVPCEndpointConnectionsResponse
--
--         , responseModifyReservedInstances $
--             modifyReservedInstancesResponse
--
--         , responseDeleteFpgaImage $
--             deleteFpgaImageResponse
--
--         , responseDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations $
--             describeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse
--
--         , responseDescribeScheduledInstances $
--             describeScheduledInstancesResponse
--
--         , responseSearchTransitGatewayMulticastGroups $
--             searchTransitGatewayMulticastGroupsResponse
--
--         , responseCreateFlowLogs $
--             createFlowLogsResponse
--
--         , responseDescribeSpotFleetRequests $
--             describeSpotFleetRequestsResponse
--
--         , responseMoveAddressToVPC $
--             moveAddressToVPCResponse
--
--         , responseDescribeFleetInstances $
--             describeFleetInstancesResponse
--
--         , responseDescribeLaunchTemplateVersions $
--             describeLaunchTemplateVersionsResponse
--
--         , responseModifyInstanceCreditSpecification $
--             modifyInstanceCreditSpecificationResponse
--
--         , responseDescribePrincipalIdFormat $
--             describePrincipalIdFormatResponse
--
--         , responseDescribeTransitGateways $
--             describeTransitGatewaysResponse
--
--         , responseDeleteNetworkACL $
--             deleteNetworkACLResponse
--
--         , responseDisassociateTransitGatewayMulticastDomain $
--             disassociateTransitGatewayMulticastDomainResponse
--
--         , responseDeleteTransitGatewayRouteTable $
--             deleteTransitGatewayRouteTableResponse
--
--         , responseCreateLaunchTemplate $
--             createLaunchTemplateResponse
--
--         , responseCreateVPCEndpointConnectionNotification $
--             createVPCEndpointConnectionNotificationResponse
--
--         , responseDeleteNetworkInterfacePermission $
--             deleteNetworkInterfacePermissionResponse
--
--         , responseDeleteVPNGateway $
--             deleteVPNGatewayResponse
--
--         , responseCreateTrafficMirrorTarget $
--             createTrafficMirrorTargetResponse
--
--         , responseDescribeImportImageTasks $
--             describeImportImageTasksResponse
--
--         , responseDescribeVolumeAttribute $
--             describeVolumeAttributeResponse
--
--         , responseDescribeMovingAddresses $
--             describeMovingAddressesResponse
--
--         , responseExportTransitGatewayRoutes $
--             exportTransitGatewayRoutesResponse
--
--         , responseGetPasswordData $
--             getPasswordDataResponse
--
--         , responseCreateVPC $
--             createVPCResponse
--
--         , responseModifyVPCPeeringConnectionOptions $
--             modifyVPCPeeringConnectionOptionsResponse
--
--         , responseDescribeFpgaImages $
--             describeFpgaImagesResponse
--
--         , responseCopySnapshot $
--             copySnapshotResponse
--
--         , responseAcceptTransitGatewayPeeringAttachment $
--             acceptTransitGatewayPeeringAttachmentResponse
--
--         , responseDisassociateAddress $
--             disassociateAddressResponse
--
--         , responseModifyTrafficMirrorFilterNetworkServices $
--             modifyTrafficMirrorFilterNetworkServicesResponse
--
--         , responseDescribeEgressOnlyInternetGateways $
--             describeEgressOnlyInternetGatewaysResponse
--
--         , responseDeleteVPC $
--             deleteVPCResponse
--
--         , responseCreateInstanceExportTask $
--             createInstanceExportTaskResponse
--
--         , responseRejectTransitGatewayVPCAttachment $
--             rejectTransitGatewayVPCAttachmentResponse
--
--         , responseDescribeTrafficMirrorSessions $
--             describeTrafficMirrorSessionsResponse
--
--         , responseGetTransitGatewayRouteTableAssociations $
--             getTransitGatewayRouteTableAssociationsResponse
--
--         , responseAssociateVPCCidrBlock $
--             associateVPCCidrBlockResponse
--
--         , responseDescribeVPCAttribute $
--             describeVPCAttributeResponse
--
--         , responseCreateVolume $
--             volume
--
--         , responseCreateDefaultSubnet $
--             createDefaultSubnetResponse
--
--         , responseDescribeScheduledInstanceAvailability $
--             describeScheduledInstanceAvailabilityResponse
--
--         , responseDisassociateClientVPNTargetNetwork $
--             disassociateClientVPNTargetNetworkResponse
--
--         , responseCreateClientVPNRoute $
--             createClientVPNRouteResponse
--
--         , responseModifyVolumeAttribute $
--             modifyVolumeAttributeResponse
--
--         , responseExportClientVPNClientConfiguration $
--             exportClientVPNClientConfigurationResponse
--
--         , responseDeleteTrafficMirrorTarget $
--             deleteTrafficMirrorTargetResponse
--
--         , responseDescribeSpotDatafeedSubscription $
--             describeSpotDatafeedSubscriptionResponse
--
--         , responseDescribeLocalGatewayRouteTables $
--             describeLocalGatewayRouteTablesResponse
--
--         , responseDescribePrefixLists $
--             describePrefixListsResponse
--
--         , responseAssociateTransitGatewayRouteTable $
--             associateTransitGatewayRouteTableResponse
--
--         , responseDeletePlacementGroup $
--             deletePlacementGroupResponse
--
--         , responseModifyTransitGateway $
--             modifyTransitGatewayResponse
--
--         , responseDeleteTransitGatewayPrefixListReference $
--             deleteTransitGatewayPrefixListReferenceResponse
--
--         , responseCreateTransitGatewayMulticastDomain $
--             createTransitGatewayMulticastDomainResponse
--
--         , responseDeregisterInstanceEventNotificationAttributes $
--             deregisterInstanceEventNotificationAttributesResponse
--
--         , responseRequestSpotFleet $
--             requestSpotFleetResponse
--
--         , responseDeleteTransitGatewayRoute $
--             deleteTransitGatewayRouteResponse
--
--         , responseDisableEBSEncryptionByDefault $
--             disableEBSEncryptionByDefaultResponse
--
--         , responseDeregisterTransitGatewayMulticastGroupMembers $
--             deregisterTransitGatewayMulticastGroupMembersResponse
--
--         , responseCreateSubnet $
--             createSubnetResponse
--
--         , responseCreateNetworkInterface $
--             createNetworkInterfaceResponse
--
--         , responseDescribeSecurityGroups $
--             describeSecurityGroupsResponse
--
--         , responseGetCapacityReservationUsage $
--             getCapacityReservationUsageResponse
--
--         , responseCreateTransitGatewayVPCAttachment $
--             createTransitGatewayVPCAttachmentResponse
--
--         , responseDescribeExportTasks $
--             describeExportTasksResponse
--
--         , responseModifySpotFleetRequest $
--             modifySpotFleetRequestResponse
--
--         , responseDetachVPNGateway $
--             detachVPNGatewayResponse
--
--         , responseModifyManagedPrefixList $
--             modifyManagedPrefixListResponse
--
--         , responseGetHostReservationPurchasePreview $
--             getHostReservationPurchasePreviewResponse
--
--         , responseEnableVolumeIO $
--             enableVolumeIOResponse
--
--         , responseDescribeInstances $
--             describeInstancesResponse
--
--         , responseCreateNatGateway $
--             createNatGatewayResponse
--
--         , responseDescribeLocalGatewayVirtualInterfaces $
--             describeLocalGatewayVirtualInterfacesResponse
--
--         , responseDescribeVPCPeeringConnections $
--             describeVPCPeeringConnectionsResponse
--
--         , responseCancelExportTask $
--             cancelExportTaskResponse
--
--         , responseCreateVPCEndpointServiceConfiguration $
--             createVPCEndpointServiceConfigurationResponse
--
--         , responseCreateDefaultVPC $
--             createDefaultVPCResponse
--
--         , responseDisassociateVPCCidrBlock $
--             disassociateVPCCidrBlockResponse
--
--         , responseDescribeTrafficMirrorFilters $
--             describeTrafficMirrorFiltersResponse
--
--         , responseDescribeFastSnapshotRestores $
--             describeFastSnapshotRestoresResponse
--
--         , responseCancelCapacityReservation $
--             cancelCapacityReservationResponse
--
--         , responseDeleteNetworkInterface $
--             deleteNetworkInterfaceResponse
--
--         , responseDisassociateTransitGatewayRouteTable $
--             disassociateTransitGatewayRouteTableResponse
--
--         , responseReplaceRouteTableAssociation $
--             replaceRouteTableAssociationResponse
--
--         , responseStartInstances $
--             startInstancesResponse
--
--         , responseCreatePlacementGroup $
--             createPlacementGroupResponse
--
--         , responseDescribeInstanceEventNotificationAttributes $
--             describeInstanceEventNotificationAttributesResponse
--
--         , responseDescribeCapacityReservations $
--             describeCapacityReservationsResponse
--
--         , responseModifyClientVPNEndpoint $
--             modifyClientVPNEndpointResponse
--
--         , responseModifyInstanceCapacityReservationAttributes $
--             modifyInstanceCapacityReservationAttributesResponse
--
--         , responseDescribeAggregateIdFormat $
--             describeAggregateIdFormatResponse
--
--         , responseDescribeSnapshots $
--             describeSnapshotsResponse
--
--         , responseAssociateAddress $
--             associateAddressResponse
--
--         , responseModifyTrafficMirrorFilterRule $
--             modifyTrafficMirrorFilterRuleResponse
--
--         , responseDescribeNetworkInterfaceAttribute $
--             describeNetworkInterfaceAttributeResponse
--
--         , responseReplaceIAMInstanceProfileAssociation $
--             replaceIAMInstanceProfileAssociationResponse
--
--         , responseAssociateClientVPNTargetNetwork $
--             associateClientVPNTargetNetworkResponse
--
--         , responseReleaseHosts $
--             releaseHostsResponse
--
--         , responseResetNetworkInterfaceAttribute $
--             resetNetworkInterfaceAttributeResponse
--
--         , responseDeleteInternetGateway $
--             deleteInternetGatewayResponse
--
--         , responseDescribeReservedInstancesListings $
--             describeReservedInstancesListingsResponse
--
--         , responseCreateVPNConnection $
--             createVPNConnectionResponse
--
--         , responseReplaceTransitGatewayRoute $
--             replaceTransitGatewayRouteResponse
--
--         , responseCreateFleet $
--             createFleetResponse
--
--         , responseDeleteNatGateway $
--             deleteNatGatewayResponse
--
--         , responseDescribeImportSnapshotTasks $
--             describeImportSnapshotTasksResponse
--
--         , responseGetCoipPoolUsage $
--             getCoipPoolUsageResponse
--
--         , responseDescribeCustomerGateways $
--             describeCustomerGatewaysResponse
--
--         , responseDeleteSubnet $
--             deleteSubnetResponse
--
--         , responseCopyImage $
--             copyImageResponse
--
--         , responseCreateVPCEndpoint $
--             createVPCEndpointResponse
--
--         , responseModifyTrafficMirrorSession $
--             modifyTrafficMirrorSessionResponse
--
--         , responseDescribeCarrierGateways $
--             describeCarrierGatewaysResponse
--
--         , responseDescribeTransitGatewayPeeringAttachments $
--             describeTransitGatewayPeeringAttachmentsResponse
--
--         , responseDeleteQueuedReservedInstances $
--             deleteQueuedReservedInstancesResponse
--
--         , responseDescribeTransitGatewayMulticastDomains $
--             describeTransitGatewayMulticastDomainsResponse
--
--         , responseGetDefaultCreditSpecification $
--             getDefaultCreditSpecificationResponse
--
--         , responseUnmonitorInstances $
--             unmonitorInstancesResponse
--
--         , responseDescribeTransitGatewayVPCAttachments $
--             describeTransitGatewayVPCAttachmentsResponse
--
--         , responseCreateSecurityGroup $
--             createSecurityGroupResponse
--
--         , responseGetEBSEncryptionByDefault $
--             getEBSEncryptionByDefaultResponse
--
--         , responseImportVolume $
--             importVolumeResponse
--
--         , responseDeleteCarrierGateway $
--             deleteCarrierGatewayResponse
--
--         , responseDisableVGWRoutePropagation $
--             disableVGWRoutePropagationResponse
--
--         , responseDeleteTrafficMirrorFilter $
--             deleteTrafficMirrorFilterResponse
--
--         , responseModifyVPNTunnelCertificate $
--             modifyVPNTunnelCertificateResponse
--
--         , responseCreateSpotDatafeedSubscription $
--             createSpotDatafeedSubscriptionResponse
--
--         , responseCancelSpotInstanceRequests $
--             cancelSpotInstanceRequestsResponse
--
--         , responseCreateRoute $
--             createRouteResponse
--
--         , responseDescribeVPCEndpointServiceConfigurations $
--             describeVPCEndpointServiceConfigurationsResponse
--
--         , responseDeleteSnapshot $
--             deleteSnapshotResponse
--
--         , responseAssignPrivateIPAddresses $
--             assignPrivateIPAddressesResponse
--
--         , responseAuthorizeClientVPNIngress $
--             authorizeClientVPNIngressResponse
--
--         , responseDeleteTransitGatewayPeeringAttachment $
--             deleteTransitGatewayPeeringAttachmentResponse
--
--         , responseModifyInstanceAttribute $
--             modifyInstanceAttributeResponse
--
--         , responseDeleteCustomerGateway $
--             deleteCustomerGatewayResponse
--
--         , responseDisassociateIAMInstanceProfile $
--             disassociateIAMInstanceProfileResponse
--
--         , responseTerminateClientVPNConnections $
--             terminateClientVPNConnectionsResponse
--
--         , responseDisassociateRouteTable $
--             disassociateRouteTableResponse
--
--         , responseGetConsoleScreenshot $
--             getConsoleScreenshotResponse
--
--         , responseResetEBSDefaultKMSKeyId $
--             resetEBSDefaultKMSKeyIdResponse
--
--         , responseAssignIPv6Addresses $
--             assignIPv6AddressesResponse
--
--         , responseModifyVPNTunnelOptions $
--             modifyVPNTunnelOptionsResponse
--
--         , responseModifyEBSDefaultKMSKeyId $
--             modifyEBSDefaultKMSKeyIdResponse
--
--         , responseDeleteSpotDatafeedSubscription $
--             deleteSpotDatafeedSubscriptionResponse
--
--         , responseModifyVolume $
--             modifyVolumeResponse
--
--         , responseEnableVPCClassicLink $
--             enableVPCClassicLinkResponse
--
--         , responseDescribePlacementGroups $
--             describePlacementGroupsResponse
--
--         , responseProvisionByoipCidr $
--             provisionByoipCidrResponse
--
--         , responseDisassociateEnclaveCertificateIAMRole $
--             disassociateEnclaveCertificateIAMRoleResponse
--
--         , responseModifyAvailabilityZoneGroup $
--             modifyAvailabilityZoneGroupResponse
--
--         , responseDescribeStaleSecurityGroups $
--             describeStaleSecurityGroupsResponse
--
--         , responseCreateCarrierGateway $
--             createCarrierGatewayResponse
--
--         , responseDescribeExportImageTasks $
--             describeExportImageTasksResponse
--
--         , responsePurchaseScheduledInstances $
--             purchaseScheduledInstancesResponse
--
--         , responseEnableVGWRoutePropagation $
--             enableVGWRoutePropagationResponse
--
--         , responseDescribeSpotFleetRequestHistory $
--             describeSpotFleetRequestHistoryResponse
--
--         , responseModifySnapshotAttribute $
--             modifySnapshotAttributeResponse
--
--         , responseDescribeIAMInstanceProfileAssociations $
--             describeIAMInstanceProfileAssociationsResponse
--
--         , responseCreateSnapshot $
--             snapshot
--
--         , responseCreateLocalGatewayRoute $
--             createLocalGatewayRouteResponse
--
--         , responseCreateNetworkACLEntry $
--             createNetworkACLEntryResponse
--
--         , responseDescribeTransitGatewayAttachments $
--             describeTransitGatewayAttachmentsResponse
--
--         , responseCreateReservedInstancesListing $
--             createReservedInstancesListingResponse
--
--         , responseDescribeIPv6Pools $
--             describeIPv6PoolsResponse
--
--         , responseAttachVPNGateway $
--             attachVPNGatewayResponse
--
--         , responseDescribeLocalGateways $
--             describeLocalGatewaysResponse
--
--         , responseModifyVPCEndpointServicePermissions $
--             modifyVPCEndpointServicePermissionsResponse
--
--         , responseExportClientVPNClientCertificateRevocationList $
--             exportClientVPNClientCertificateRevocationListResponse
--
--         , responseCreateDHCPOptions $
--             createDHCPOptionsResponse
--
--         , responseRegisterTransitGatewayMulticastGroupSources $
--             registerTransitGatewayMulticastGroupSourcesResponse
--
--         , responseDescribeAccountAttributes $
--             describeAccountAttributesResponse
--
--         , responseGetTransitGatewayRouteTablePropagations $
--             getTransitGatewayRouteTablePropagationsResponse
--
--         , responseModifyFpgaImageAttribute $
--             modifyFpgaImageAttributeResponse
--
--         , responseModifyHosts $
--             modifyHostsResponse
--
--         , responseRebootInstances $
--             rebootInstancesResponse
--
--         , responseModifyVPCEndpointServiceConfiguration $
--             modifyVPCEndpointServiceConfigurationResponse
--
--         , responseCreateTransitGateway $
--             createTransitGatewayResponse
--
--         , responseUnassignIPv6Addresses $
--             unassignIPv6AddressesResponse
--
--         , responseDeleteTrafficMirrorSession $
--             deleteTrafficMirrorSessionResponse
--
--         , responseCreateManagedPrefixList $
--             createManagedPrefixListResponse
--
--         , responseAssociateIAMInstanceProfile $
--             associateIAMInstanceProfileResponse
--
--         , responseModifyDefaultCreditSpecification $
--             modifyDefaultCreditSpecificationResponse
--
--         , responseDeleteEgressOnlyInternetGateway $
--             deleteEgressOnlyInternetGatewayResponse
--
--         , responsePurchaseHostReservation $
--             purchaseHostReservationResponse
--
--         , responseModifyTransitGatewayVPCAttachment $
--             modifyTransitGatewayVPCAttachmentResponse
--
--         , responseCreateImage $
--             createImageResponse
--
--         , responseDescribeClassicLinkInstances $
--             describeClassicLinkInstancesResponse
--
--         , responseTerminateInstances $
--             terminateInstancesResponse
--
--         , responseGetTransitGatewayPrefixListReferences $
--             getTransitGatewayPrefixListReferencesResponse
--
--         , responseDescribeKeyPairs $
--             describeKeyPairsResponse
--
--         , responseDisableFastSnapshotRestores $
--             disableFastSnapshotRestoresResponse
--
--         , responseDescribeLaunchTemplates $
--             describeLaunchTemplatesResponse
--
--         , responseCreateVPNConnectionRoute $
--             createVPNConnectionRouteResponse
--
--         , responseAssociateRouteTable $
--             associateRouteTableResponse
--
--         , responseDescribeVPNGateways $
--             describeVPNGatewaysResponse
--
--         , responseModifyVPNConnectionOptions $
--             modifyVPNConnectionOptionsResponse
--
--         , responseGetConsoleOutput $
--             getConsoleOutputResponse
--
--         , responseDescribeHosts $
--             describeHostsResponse
--
--         , responseDescribeImageAttribute $
--             describeImageAttributeResponse
--
--         , responseModifyIdFormat $
--             modifyIdFormatResponse
--
--         , responseRegisterTransitGatewayMulticastGroupMembers $
--             registerTransitGatewayMulticastGroupMembersResponse
--
--         , responseDeleteManagedPrefixList $
--             deleteManagedPrefixListResponse
--
--         , responseDeleteRouteTable $
--             deleteRouteTableResponse
--
--         , responseResetImageAttribute $
--             resetImageAttributeResponse
--
--         , responseModifyTransitGatewayPrefixListReference $
--             modifyTransitGatewayPrefixListReferenceResponse
--
--         , responseDescribeTransitGatewayRouteTables $
--             describeTransitGatewayRouteTablesResponse
--
--         , responseCreateEgressOnlyInternetGateway $
--             createEgressOnlyInternetGatewayResponse
--
--         , responseDescribeReservedInstancesModifications $
--             describeReservedInstancesModificationsResponse
--
--         , responseDescribeSpotInstanceRequests $
--             describeSpotInstanceRequestsResponse
--
--         , responseRevokeClientVPNIngress $
--             revokeClientVPNIngressResponse
--
--         , responseUnassignPrivateIPAddresses $
--             unassignPrivateIPAddressesResponse
--
--         , responseDescribeNetworkInterfacePermissions $
--             describeNetworkInterfacePermissionsResponse
--
--         , responseEnableFastSnapshotRestores $
--             enableFastSnapshotRestoresResponse
--
--         , responseDescribeVPCEndpointServicePermissions $
--             describeVPCEndpointServicePermissionsResponse
--
--         , responseDeleteDHCPOptions $
--             deleteDHCPOptionsResponse
--
--         , responseRegisterInstanceEventNotificationAttributes $
--             registerInstanceEventNotificationAttributesResponse
--
--         , responseDescribeNetworkACLs $
--             describeNetworkACLsResponse
--
--         , responseCancelImportTask $
--             cancelImportTaskResponse
--
--         , responseDetachClassicLinkVPC $
--             detachClassicLinkVPCResponse
--
--         , responseDescribeRegions $
--             describeRegionsResponse
--
--         , responseMonitorInstances $
--             monitorInstancesResponse
--
--         , responseSearchLocalGatewayRoutes $
--             searchLocalGatewayRoutesResponse
--
--         , responseDeleteClientVPNRoute $
--             deleteClientVPNRouteResponse
--
--         , responseAcceptVPCPeeringConnection $
--             acceptVPCPeeringConnectionResponse
--
--         , responseImportSnapshot $
--             importSnapshotResponse
--
--         , responseDescribeVolumeStatus $
--             describeVolumeStatusResponse
--
--         , responseDescribeRouteTables $
--             describeRouteTablesResponse
--
--         , responseDescribeAvailabilityZones $
--             describeAvailabilityZonesResponse
--
--         , responseModifyVPCAttribute $
--             modifyVPCAttributeResponse
--
--         , responseDescribeClientVPNConnections $
--             describeClientVPNConnectionsResponse
--
--         , responseDescribeFleetHistory $
--             describeFleetHistoryResponse
--
--         , responseDescribeImages $
--             describeImagesResponse
--
--         , responseDescribeElasticGpus $
--             describeElasticGpusResponse
--
--         , responseRestoreAddressToClassic $
--             restoreAddressToClassicResponse
--
--         , responseDescribeManagedPrefixLists $
--             describeManagedPrefixListsResponse
--
--         , responseCreateKeyPair $
--             createKeyPairResponse
--
--         , responseGetReservedInstancesExchangeQuote $
--             getReservedInstancesExchangeQuoteResponse
--
--         , responseDeleteVolume $
--             deleteVolumeResponse
--
--         , responseDeprovisionByoipCidr $
--             deprovisionByoipCidrResponse
--
--         , responseDeleteVPCEndpointServiceConfigurations $
--             deleteVPCEndpointServiceConfigurationsResponse
--
--         , responseDescribeSpotFleetInstances $
--             describeSpotFleetInstancesResponse
--
--           ]
--     ]

-- Requests

requestModifyCapacityReservation :: ModifyCapacityReservation -> TestTree
requestModifyCapacityReservation =
  req
    "ModifyCapacityReservation"
    "fixture/ModifyCapacityReservation.yaml"

requestGetAssociatedIPv6PoolCidrs :: GetAssociatedIPv6PoolCidrs -> TestTree
requestGetAssociatedIPv6PoolCidrs =
  req
    "GetAssociatedIPv6PoolCidrs"
    "fixture/GetAssociatedIPv6PoolCidrs.yaml"

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

requestRejectVPCEndpointConnections :: RejectVPCEndpointConnections -> TestTree
requestRejectVPCEndpointConnections =
  req
    "RejectVPCEndpointConnections"
    "fixture/RejectVPCEndpointConnections.yaml"

requestCreateVPNGateway :: CreateVPNGateway -> TestTree
requestCreateVPNGateway =
  req
    "CreateVPNGateway"
    "fixture/CreateVPNGateway.yaml"

requestCreateNetworkACL :: CreateNetworkACL -> TestTree
requestCreateNetworkACL =
  req
    "CreateNetworkACL"
    "fixture/CreateNetworkACL.yaml"

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

requestDeleteVPCEndpoints :: DeleteVPCEndpoints -> TestTree
requestDeleteVPCEndpoints =
  req
    "DeleteVPCEndpoints"
    "fixture/DeleteVPCEndpoints.yaml"

requestDescribeClientVPNEndpoints :: DescribeClientVPNEndpoints -> TestTree
requestDescribeClientVPNEndpoints =
  req
    "DescribeClientVPNEndpoints"
    "fixture/DescribeClientVPNEndpoints.yaml"

requestDeleteFlowLogs :: DeleteFlowLogs -> TestTree
requestDeleteFlowLogs =
  req
    "DeleteFlowLogs"
    "fixture/DeleteFlowLogs.yaml"

requestDescribeVPCClassicLink :: DescribeVPCClassicLink -> TestTree
requestDescribeVPCClassicLink =
  req
    "DescribeVPCClassicLink"
    "fixture/DescribeVPCClassicLink.yaml"

requestGetAssociatedEnclaveCertificateIAMRoles :: GetAssociatedEnclaveCertificateIAMRoles -> TestTree
requestGetAssociatedEnclaveCertificateIAMRoles =
  req
    "GetAssociatedEnclaveCertificateIAMRoles"
    "fixture/GetAssociatedEnclaveCertificateIAMRoles.yaml"

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

requestDeleteVPNConnection :: DeleteVPNConnection -> TestTree
requestDeleteVPNConnection =
  req
    "DeleteVPNConnection"
    "fixture/DeleteVPNConnection.yaml"

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

requestDeleteVPCEndpointConnectionNotifications :: DeleteVPCEndpointConnectionNotifications -> TestTree
requestDeleteVPCEndpointConnectionNotifications =
  req
    "DeleteVPCEndpointConnectionNotifications"
    "fixture/DeleteVPCEndpointConnectionNotifications.yaml"

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

requestDeleteLocalGatewayRouteTableVPCAssociation :: DeleteLocalGatewayRouteTableVPCAssociation -> TestTree
requestDeleteLocalGatewayRouteTableVPCAssociation =
  req
    "DeleteLocalGatewayRouteTableVPCAssociation"
    "fixture/DeleteLocalGatewayRouteTableVPCAssociation.yaml"

requestModifyNetworkInterfaceAttribute :: ModifyNetworkInterfaceAttribute -> TestTree
requestModifyNetworkInterfaceAttribute =
  req
    "ModifyNetworkInterfaceAttribute"
    "fixture/ModifyNetworkInterfaceAttribute.yaml"

requestModifyVPCTenancy :: ModifyVPCTenancy -> TestTree
requestModifyVPCTenancy =
  req
    "ModifyVPCTenancy"
    "fixture/ModifyVPCTenancy.yaml"

requestDescribeInstanceTypes :: DescribeInstanceTypes -> TestTree
requestDescribeInstanceTypes =
  req
    "DescribeInstanceTypes"
    "fixture/DescribeInstanceTypes.yaml"

requestDescribeClientVPNAuthorizationRules :: DescribeClientVPNAuthorizationRules -> TestTree
requestDescribeClientVPNAuthorizationRules =
  req
    "DescribeClientVPNAuthorizationRules"
    "fixture/DescribeClientVPNAuthorizationRules.yaml"

requestDeleteTransitGatewayVPCAttachment :: DeleteTransitGatewayVPCAttachment -> TestTree
requestDeleteTransitGatewayVPCAttachment =
  req
    "DeleteTransitGatewayVPCAttachment"
    "fixture/DeleteTransitGatewayVPCAttachment.yaml"

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

requestAttachClassicLinkVPC :: AttachClassicLinkVPC -> TestTree
requestAttachClassicLinkVPC =
  req
    "AttachClassicLinkVPC"
    "fixture/AttachClassicLinkVPC.yaml"

requestDisableTransitGatewayRouteTablePropagation :: DisableTransitGatewayRouteTablePropagation -> TestTree
requestDisableTransitGatewayRouteTablePropagation =
  req
    "DisableTransitGatewayRouteTablePropagation"
    "fixture/DisableTransitGatewayRouteTablePropagation.yaml"

requestDescribeVPCClassicLinkDNSSupport :: DescribeVPCClassicLinkDNSSupport -> TestTree
requestDescribeVPCClassicLinkDNSSupport =
  req
    "DescribeVPCClassicLinkDNSSupport"
    "fixture/DescribeVPCClassicLinkDNSSupport.yaml"

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

requestDescribeDHCPOptions :: DescribeDHCPOptions -> TestTree
requestDescribeDHCPOptions =
  req
    "DescribeDHCPOptions"
    "fixture/DescribeDHCPOptions.yaml"

requestImportImage :: ImportImage -> TestTree
requestImportImage =
  req
    "ImportImage"
    "fixture/ImportImage.yaml"

requestCreateLocalGatewayRouteTableVPCAssociation :: CreateLocalGatewayRouteTableVPCAssociation -> TestTree
requestCreateLocalGatewayRouteTableVPCAssociation =
  req
    "CreateLocalGatewayRouteTableVPCAssociation"
    "fixture/CreateLocalGatewayRouteTableVPCAssociation.yaml"

requestCopyFpgaImage :: CopyFpgaImage -> TestTree
requestCopyFpgaImage =
  req
    "CopyFpgaImage"
    "fixture/CopyFpgaImage.yaml"

requestImportClientVPNClientCertificateRevocationList :: ImportClientVPNClientCertificateRevocationList -> TestTree
requestImportClientVPNClientCertificateRevocationList =
  req
    "ImportClientVPNClientCertificateRevocationList"
    "fixture/ImportClientVPNClientCertificateRevocationList.yaml"

requestStopInstances :: StopInstances -> TestTree
requestStopInstances =
  req
    "StopInstances"
    "fixture/StopInstances.yaml"

requestEnableEBSEncryptionByDefault :: EnableEBSEncryptionByDefault -> TestTree
requestEnableEBSEncryptionByDefault =
  req
    "EnableEBSEncryptionByDefault"
    "fixture/EnableEBSEncryptionByDefault.yaml"

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

requestModifyVPCEndpointConnectionNotification :: ModifyVPCEndpointConnectionNotification -> TestTree
requestModifyVPCEndpointConnectionNotification =
  req
    "ModifyVPCEndpointConnectionNotification"
    "fixture/ModifyVPCEndpointConnectionNotification.yaml"

requestDescribeInternetGateways :: DescribeInternetGateways -> TestTree
requestDescribeInternetGateways =
  req
    "DescribeInternetGateways"
    "fixture/DescribeInternetGateways.yaml"

requestDisableVPCClassicLink :: DisableVPCClassicLink -> TestTree
requestDisableVPCClassicLink =
  req
    "DisableVPCClassicLink"
    "fixture/DisableVPCClassicLink.yaml"

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

requestReplaceNetworkACLAssociation :: ReplaceNetworkACLAssociation -> TestTree
requestReplaceNetworkACLAssociation =
  req
    "ReplaceNetworkACLAssociation"
    "fixture/ReplaceNetworkACLAssociation.yaml"

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

requestDescribeVPCEndpointServices :: DescribeVPCEndpointServices -> TestTree
requestDescribeVPCEndpointServices =
  req
    "DescribeVPCEndpointServices"
    "fixture/DescribeVPCEndpointServices.yaml"

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

requestCreateVPCPeeringConnection :: CreateVPCPeeringConnection -> TestTree
requestCreateVPCPeeringConnection =
  req
    "CreateVPCPeeringConnection"
    "fixture/CreateVPCPeeringConnection.yaml"

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

requestDeleteNetworkACLEntry :: DeleteNetworkACLEntry -> TestTree
requestDeleteNetworkACLEntry =
  req
    "DeleteNetworkACLEntry"
    "fixture/DeleteNetworkACLEntry.yaml"

requestResetSnapshotAttribute :: ResetSnapshotAttribute -> TestTree
requestResetSnapshotAttribute =
  req
    "ResetSnapshotAttribute"
    "fixture/ResetSnapshotAttribute.yaml"

requestDescribeVPNConnections :: DescribeVPNConnections -> TestTree
requestDescribeVPNConnections =
  req
    "DescribeVPNConnections"
    "fixture/DescribeVPNConnections.yaml"

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

requestReplaceNetworkACLEntry :: ReplaceNetworkACLEntry -> TestTree
requestReplaceNetworkACLEntry =
  req
    "ReplaceNetworkACLEntry"
    "fixture/ReplaceNetworkACLEntry.yaml"

requestDescribeVPCEndpoints :: DescribeVPCEndpoints -> TestTree
requestDescribeVPCEndpoints =
  req
    "DescribeVPCEndpoints"
    "fixture/DescribeVPCEndpoints.yaml"

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

requestDescribePublicIPv4Pools :: DescribePublicIPv4Pools -> TestTree
requestDescribePublicIPv4Pools =
  req
    "DescribePublicIPv4Pools"
    "fixture/DescribePublicIPv4Pools.yaml"

requestDescribeClientVPNTargetNetworks :: DescribeClientVPNTargetNetworks -> TestTree
requestDescribeClientVPNTargetNetworks =
  req
    "DescribeClientVPNTargetNetworks"
    "fixture/DescribeClientVPNTargetNetworks.yaml"

requestDeleteVPCPeeringConnection :: DeleteVPCPeeringConnection -> TestTree
requestDeleteVPCPeeringConnection =
  req
    "DeleteVPCPeeringConnection"
    "fixture/DeleteVPCPeeringConnection.yaml"

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

requestDescribeLocalGatewayRouteTableVPCAssociations :: DescribeLocalGatewayRouteTableVPCAssociations -> TestTree
requestDescribeLocalGatewayRouteTableVPCAssociations =
  req
    "DescribeLocalGatewayRouteTableVPCAssociations"
    "fixture/DescribeLocalGatewayRouteTableVPCAssociations.yaml"

requestDescribeVPCEndpointConnectionNotifications :: DescribeVPCEndpointConnectionNotifications -> TestTree
requestDescribeVPCEndpointConnectionNotifications =
  req
    "DescribeVPCEndpointConnectionNotifications"
    "fixture/DescribeVPCEndpointConnectionNotifications.yaml"

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

requestAssociateDHCPOptions :: AssociateDHCPOptions -> TestTree
requestAssociateDHCPOptions =
  req
    "AssociateDHCPOptions"
    "fixture/AssociateDHCPOptions.yaml"

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

requestDescribeVPCs :: DescribeVPCs -> TestTree
requestDescribeVPCs =
  req
    "DescribeVPCs"
    "fixture/DescribeVPCs.yaml"

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

requestDisableVPCClassicLinkDNSSupport :: DisableVPCClassicLinkDNSSupport -> TestTree
requestDisableVPCClassicLinkDNSSupport =
  req
    "DisableVPCClassicLinkDNSSupport"
    "fixture/DisableVPCClassicLinkDNSSupport.yaml"

requestApplySecurityGroupsToClientVPNTargetNetwork :: ApplySecurityGroupsToClientVPNTargetNetwork -> TestTree
requestApplySecurityGroupsToClientVPNTargetNetwork =
  req
    "ApplySecurityGroupsToClientVPNTargetNetwork"
    "fixture/ApplySecurityGroupsToClientVPNTargetNetwork.yaml"

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

requestAcceptVPCEndpointConnections :: AcceptVPCEndpointConnections -> TestTree
requestAcceptVPCEndpointConnections =
  req
    "AcceptVPCEndpointConnections"
    "fixture/AcceptVPCEndpointConnections.yaml"

requestDeleteClientVPNEndpoint :: DeleteClientVPNEndpoint -> TestTree
requestDeleteClientVPNEndpoint =
  req
    "DeleteClientVPNEndpoint"
    "fixture/DeleteClientVPNEndpoint.yaml"

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

requestAcceptTransitGatewayVPCAttachment :: AcceptTransitGatewayVPCAttachment -> TestTree
requestAcceptTransitGatewayVPCAttachment =
  req
    "AcceptTransitGatewayVPCAttachment"
    "fixture/AcceptTransitGatewayVPCAttachment.yaml"

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

requestModifyVPNConnection :: ModifyVPNConnection -> TestTree
requestModifyVPNConnection =
  req
    "ModifyVPNConnection"
    "fixture/ModifyVPNConnection.yaml"

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

requestStartVPCEndpointServicePrivateDNSVerification :: StartVPCEndpointServicePrivateDNSVerification -> TestTree
requestStartVPCEndpointServicePrivateDNSVerification =
  req
    "StartVPCEndpointServicePrivateDNSVerification"
    "fixture/StartVPCEndpointServicePrivateDNSVerification.yaml"

requestDescribeVolumes :: DescribeVolumes -> TestTree
requestDescribeVolumes =
  req
    "DescribeVolumes"
    "fixture/DescribeVolumes.yaml"

requestRejectVPCPeeringConnection :: RejectVPCPeeringConnection -> TestTree
requestRejectVPCPeeringConnection =
  req
    "RejectVPCPeeringConnection"
    "fixture/RejectVPCPeeringConnection.yaml"

requestDescribeClientVPNRoutes :: DescribeClientVPNRoutes -> TestTree
requestDescribeClientVPNRoutes =
  req
    "DescribeClientVPNRoutes"
    "fixture/DescribeClientVPNRoutes.yaml"

requestDeleteVPNConnectionRoute :: DeleteVPNConnectionRoute -> TestTree
requestDeleteVPNConnectionRoute =
  req
    "DeleteVPNConnectionRoute"
    "fixture/DeleteVPNConnectionRoute.yaml"

requestAssociateEnclaveCertificateIAMRole :: AssociateEnclaveCertificateIAMRole -> TestTree
requestAssociateEnclaveCertificateIAMRole =
  req
    "AssociateEnclaveCertificateIAMRole"
    "fixture/AssociateEnclaveCertificateIAMRole.yaml"

requestModifyVPCEndpoint :: ModifyVPCEndpoint -> TestTree
requestModifyVPCEndpoint =
  req
    "ModifyVPCEndpoint"
    "fixture/ModifyVPCEndpoint.yaml"

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

requestCreateClientVPNEndpoint :: CreateClientVPNEndpoint -> TestTree
requestCreateClientVPNEndpoint =
  req
    "CreateClientVPNEndpoint"
    "fixture/CreateClientVPNEndpoint.yaml"

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

requestGetEBSDefaultKMSKeyId :: GetEBSDefaultKMSKeyId -> TestTree
requestGetEBSDefaultKMSKeyId =
  req
    "GetEBSDefaultKMSKeyId"
    "fixture/GetEBSDefaultKMSKeyId.yaml"

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

requestEnableVPCClassicLinkDNSSupport :: EnableVPCClassicLinkDNSSupport -> TestTree
requestEnableVPCClassicLinkDNSSupport =
  req
    "EnableVPCClassicLinkDNSSupport"
    "fixture/EnableVPCClassicLinkDNSSupport.yaml"

requestDescribeVPCEndpointConnections :: DescribeVPCEndpointConnections -> TestTree
requestDescribeVPCEndpointConnections =
  req
    "DescribeVPCEndpointConnections"
    "fixture/DescribeVPCEndpointConnections.yaml"

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

requestMoveAddressToVPC :: MoveAddressToVPC -> TestTree
requestMoveAddressToVPC =
  req
    "MoveAddressToVPC"
    "fixture/MoveAddressToVPC.yaml"

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

requestDeleteNetworkACL :: DeleteNetworkACL -> TestTree
requestDeleteNetworkACL =
  req
    "DeleteNetworkACL"
    "fixture/DeleteNetworkACL.yaml"

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

requestCreateVPCEndpointConnectionNotification :: CreateVPCEndpointConnectionNotification -> TestTree
requestCreateVPCEndpointConnectionNotification =
  req
    "CreateVPCEndpointConnectionNotification"
    "fixture/CreateVPCEndpointConnectionNotification.yaml"

requestDeleteNetworkInterfacePermission :: DeleteNetworkInterfacePermission -> TestTree
requestDeleteNetworkInterfacePermission =
  req
    "DeleteNetworkInterfacePermission"
    "fixture/DeleteNetworkInterfacePermission.yaml"

requestDeleteVPNGateway :: DeleteVPNGateway -> TestTree
requestDeleteVPNGateway =
  req
    "DeleteVPNGateway"
    "fixture/DeleteVPNGateway.yaml"

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

requestCreateVPC :: CreateVPC -> TestTree
requestCreateVPC =
  req
    "CreateVPC"
    "fixture/CreateVPC.yaml"

requestModifyVPCPeeringConnectionOptions :: ModifyVPCPeeringConnectionOptions -> TestTree
requestModifyVPCPeeringConnectionOptions =
  req
    "ModifyVPCPeeringConnectionOptions"
    "fixture/ModifyVPCPeeringConnectionOptions.yaml"

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

requestDeleteVPC :: DeleteVPC -> TestTree
requestDeleteVPC =
  req
    "DeleteVPC"
    "fixture/DeleteVPC.yaml"

requestCreateInstanceExportTask :: CreateInstanceExportTask -> TestTree
requestCreateInstanceExportTask =
  req
    "CreateInstanceExportTask"
    "fixture/CreateInstanceExportTask.yaml"

requestRejectTransitGatewayVPCAttachment :: RejectTransitGatewayVPCAttachment -> TestTree
requestRejectTransitGatewayVPCAttachment =
  req
    "RejectTransitGatewayVPCAttachment"
    "fixture/RejectTransitGatewayVPCAttachment.yaml"

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

requestAssociateVPCCidrBlock :: AssociateVPCCidrBlock -> TestTree
requestAssociateVPCCidrBlock =
  req
    "AssociateVPCCidrBlock"
    "fixture/AssociateVPCCidrBlock.yaml"

requestDescribeVPCAttribute :: DescribeVPCAttribute -> TestTree
requestDescribeVPCAttribute =
  req
    "DescribeVPCAttribute"
    "fixture/DescribeVPCAttribute.yaml"

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

requestDisassociateClientVPNTargetNetwork :: DisassociateClientVPNTargetNetwork -> TestTree
requestDisassociateClientVPNTargetNetwork =
  req
    "DisassociateClientVPNTargetNetwork"
    "fixture/DisassociateClientVPNTargetNetwork.yaml"

requestCreateClientVPNRoute :: CreateClientVPNRoute -> TestTree
requestCreateClientVPNRoute =
  req
    "CreateClientVPNRoute"
    "fixture/CreateClientVPNRoute.yaml"

requestModifyVolumeAttribute :: ModifyVolumeAttribute -> TestTree
requestModifyVolumeAttribute =
  req
    "ModifyVolumeAttribute"
    "fixture/ModifyVolumeAttribute.yaml"

requestExportClientVPNClientConfiguration :: ExportClientVPNClientConfiguration -> TestTree
requestExportClientVPNClientConfiguration =
  req
    "ExportClientVPNClientConfiguration"
    "fixture/ExportClientVPNClientConfiguration.yaml"

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

requestDisableEBSEncryptionByDefault :: DisableEBSEncryptionByDefault -> TestTree
requestDisableEBSEncryptionByDefault =
  req
    "DisableEBSEncryptionByDefault"
    "fixture/DisableEBSEncryptionByDefault.yaml"

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

requestCreateTransitGatewayVPCAttachment :: CreateTransitGatewayVPCAttachment -> TestTree
requestCreateTransitGatewayVPCAttachment =
  req
    "CreateTransitGatewayVPCAttachment"
    "fixture/CreateTransitGatewayVPCAttachment.yaml"

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

requestDetachVPNGateway :: DetachVPNGateway -> TestTree
requestDetachVPNGateway =
  req
    "DetachVPNGateway"
    "fixture/DetachVPNGateway.yaml"

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

requestDescribeVPCPeeringConnections :: DescribeVPCPeeringConnections -> TestTree
requestDescribeVPCPeeringConnections =
  req
    "DescribeVPCPeeringConnections"
    "fixture/DescribeVPCPeeringConnections.yaml"

requestCancelExportTask :: CancelExportTask -> TestTree
requestCancelExportTask =
  req
    "CancelExportTask"
    "fixture/CancelExportTask.yaml"

requestCreateVPCEndpointServiceConfiguration :: CreateVPCEndpointServiceConfiguration -> TestTree
requestCreateVPCEndpointServiceConfiguration =
  req
    "CreateVPCEndpointServiceConfiguration"
    "fixture/CreateVPCEndpointServiceConfiguration.yaml"

requestCreateDefaultVPC :: CreateDefaultVPC -> TestTree
requestCreateDefaultVPC =
  req
    "CreateDefaultVPC"
    "fixture/CreateDefaultVPC.yaml"

requestDisassociateVPCCidrBlock :: DisassociateVPCCidrBlock -> TestTree
requestDisassociateVPCCidrBlock =
  req
    "DisassociateVPCCidrBlock"
    "fixture/DisassociateVPCCidrBlock.yaml"

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

requestModifyClientVPNEndpoint :: ModifyClientVPNEndpoint -> TestTree
requestModifyClientVPNEndpoint =
  req
    "ModifyClientVPNEndpoint"
    "fixture/ModifyClientVPNEndpoint.yaml"

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

requestReplaceIAMInstanceProfileAssociation :: ReplaceIAMInstanceProfileAssociation -> TestTree
requestReplaceIAMInstanceProfileAssociation =
  req
    "ReplaceIAMInstanceProfileAssociation"
    "fixture/ReplaceIAMInstanceProfileAssociation.yaml"

requestAssociateClientVPNTargetNetwork :: AssociateClientVPNTargetNetwork -> TestTree
requestAssociateClientVPNTargetNetwork =
  req
    "AssociateClientVPNTargetNetwork"
    "fixture/AssociateClientVPNTargetNetwork.yaml"

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

requestCreateVPNConnection :: CreateVPNConnection -> TestTree
requestCreateVPNConnection =
  req
    "CreateVPNConnection"
    "fixture/CreateVPNConnection.yaml"

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

requestCreateVPCEndpoint :: CreateVPCEndpoint -> TestTree
requestCreateVPCEndpoint =
  req
    "CreateVPCEndpoint"
    "fixture/CreateVPCEndpoint.yaml"

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

requestDescribeTransitGatewayVPCAttachments :: DescribeTransitGatewayVPCAttachments -> TestTree
requestDescribeTransitGatewayVPCAttachments =
  req
    "DescribeTransitGatewayVPCAttachments"
    "fixture/DescribeTransitGatewayVPCAttachments.yaml"

requestCreateSecurityGroup :: CreateSecurityGroup -> TestTree
requestCreateSecurityGroup =
  req
    "CreateSecurityGroup"
    "fixture/CreateSecurityGroup.yaml"

requestGetEBSEncryptionByDefault :: GetEBSEncryptionByDefault -> TestTree
requestGetEBSEncryptionByDefault =
  req
    "GetEBSEncryptionByDefault"
    "fixture/GetEBSEncryptionByDefault.yaml"

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

requestDisableVGWRoutePropagation :: DisableVGWRoutePropagation -> TestTree
requestDisableVGWRoutePropagation =
  req
    "DisableVGWRoutePropagation"
    "fixture/DisableVGWRoutePropagation.yaml"

requestDeleteTrafficMirrorFilter :: DeleteTrafficMirrorFilter -> TestTree
requestDeleteTrafficMirrorFilter =
  req
    "DeleteTrafficMirrorFilter"
    "fixture/DeleteTrafficMirrorFilter.yaml"

requestModifyVPNTunnelCertificate :: ModifyVPNTunnelCertificate -> TestTree
requestModifyVPNTunnelCertificate =
  req
    "ModifyVPNTunnelCertificate"
    "fixture/ModifyVPNTunnelCertificate.yaml"

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

requestDescribeVPCEndpointServiceConfigurations :: DescribeVPCEndpointServiceConfigurations -> TestTree
requestDescribeVPCEndpointServiceConfigurations =
  req
    "DescribeVPCEndpointServiceConfigurations"
    "fixture/DescribeVPCEndpointServiceConfigurations.yaml"

requestDeleteSnapshot :: DeleteSnapshot -> TestTree
requestDeleteSnapshot =
  req
    "DeleteSnapshot"
    "fixture/DeleteSnapshot.yaml"

requestAssignPrivateIPAddresses :: AssignPrivateIPAddresses -> TestTree
requestAssignPrivateIPAddresses =
  req
    "AssignPrivateIPAddresses"
    "fixture/AssignPrivateIPAddresses.yaml"

requestAuthorizeClientVPNIngress :: AuthorizeClientVPNIngress -> TestTree
requestAuthorizeClientVPNIngress =
  req
    "AuthorizeClientVPNIngress"
    "fixture/AuthorizeClientVPNIngress.yaml"

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

requestDisassociateIAMInstanceProfile :: DisassociateIAMInstanceProfile -> TestTree
requestDisassociateIAMInstanceProfile =
  req
    "DisassociateIAMInstanceProfile"
    "fixture/DisassociateIAMInstanceProfile.yaml"

requestTerminateClientVPNConnections :: TerminateClientVPNConnections -> TestTree
requestTerminateClientVPNConnections =
  req
    "TerminateClientVPNConnections"
    "fixture/TerminateClientVPNConnections.yaml"

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

requestResetEBSDefaultKMSKeyId :: ResetEBSDefaultKMSKeyId -> TestTree
requestResetEBSDefaultKMSKeyId =
  req
    "ResetEBSDefaultKMSKeyId"
    "fixture/ResetEBSDefaultKMSKeyId.yaml"

requestAssignIPv6Addresses :: AssignIPv6Addresses -> TestTree
requestAssignIPv6Addresses =
  req
    "AssignIPv6Addresses"
    "fixture/AssignIPv6Addresses.yaml"

requestModifyVPNTunnelOptions :: ModifyVPNTunnelOptions -> TestTree
requestModifyVPNTunnelOptions =
  req
    "ModifyVPNTunnelOptions"
    "fixture/ModifyVPNTunnelOptions.yaml"

requestModifyEBSDefaultKMSKeyId :: ModifyEBSDefaultKMSKeyId -> TestTree
requestModifyEBSDefaultKMSKeyId =
  req
    "ModifyEBSDefaultKMSKeyId"
    "fixture/ModifyEBSDefaultKMSKeyId.yaml"

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

requestEnableVPCClassicLink :: EnableVPCClassicLink -> TestTree
requestEnableVPCClassicLink =
  req
    "EnableVPCClassicLink"
    "fixture/EnableVPCClassicLink.yaml"

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

requestDisassociateEnclaveCertificateIAMRole :: DisassociateEnclaveCertificateIAMRole -> TestTree
requestDisassociateEnclaveCertificateIAMRole =
  req
    "DisassociateEnclaveCertificateIAMRole"
    "fixture/DisassociateEnclaveCertificateIAMRole.yaml"

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

requestEnableVGWRoutePropagation :: EnableVGWRoutePropagation -> TestTree
requestEnableVGWRoutePropagation =
  req
    "EnableVGWRoutePropagation"
    "fixture/EnableVGWRoutePropagation.yaml"

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

requestDescribeIAMInstanceProfileAssociations :: DescribeIAMInstanceProfileAssociations -> TestTree
requestDescribeIAMInstanceProfileAssociations =
  req
    "DescribeIAMInstanceProfileAssociations"
    "fixture/DescribeIAMInstanceProfileAssociations.yaml"

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

requestCreateNetworkACLEntry :: CreateNetworkACLEntry -> TestTree
requestCreateNetworkACLEntry =
  req
    "CreateNetworkACLEntry"
    "fixture/CreateNetworkACLEntry.yaml"

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

requestDescribeIPv6Pools :: DescribeIPv6Pools -> TestTree
requestDescribeIPv6Pools =
  req
    "DescribeIPv6Pools"
    "fixture/DescribeIPv6Pools.yaml"

requestAttachVPNGateway :: AttachVPNGateway -> TestTree
requestAttachVPNGateway =
  req
    "AttachVPNGateway"
    "fixture/AttachVPNGateway.yaml"

requestDescribeLocalGateways :: DescribeLocalGateways -> TestTree
requestDescribeLocalGateways =
  req
    "DescribeLocalGateways"
    "fixture/DescribeLocalGateways.yaml"

requestModifyVPCEndpointServicePermissions :: ModifyVPCEndpointServicePermissions -> TestTree
requestModifyVPCEndpointServicePermissions =
  req
    "ModifyVPCEndpointServicePermissions"
    "fixture/ModifyVPCEndpointServicePermissions.yaml"

requestExportClientVPNClientCertificateRevocationList :: ExportClientVPNClientCertificateRevocationList -> TestTree
requestExportClientVPNClientCertificateRevocationList =
  req
    "ExportClientVPNClientCertificateRevocationList"
    "fixture/ExportClientVPNClientCertificateRevocationList.yaml"

requestCreateDHCPOptions :: CreateDHCPOptions -> TestTree
requestCreateDHCPOptions =
  req
    "CreateDHCPOptions"
    "fixture/CreateDHCPOptions.yaml"

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

requestModifyVPCEndpointServiceConfiguration :: ModifyVPCEndpointServiceConfiguration -> TestTree
requestModifyVPCEndpointServiceConfiguration =
  req
    "ModifyVPCEndpointServiceConfiguration"
    "fixture/ModifyVPCEndpointServiceConfiguration.yaml"

requestCreateTransitGateway :: CreateTransitGateway -> TestTree
requestCreateTransitGateway =
  req
    "CreateTransitGateway"
    "fixture/CreateTransitGateway.yaml"

requestUnassignIPv6Addresses :: UnassignIPv6Addresses -> TestTree
requestUnassignIPv6Addresses =
  req
    "UnassignIPv6Addresses"
    "fixture/UnassignIPv6Addresses.yaml"

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

requestAssociateIAMInstanceProfile :: AssociateIAMInstanceProfile -> TestTree
requestAssociateIAMInstanceProfile =
  req
    "AssociateIAMInstanceProfile"
    "fixture/AssociateIAMInstanceProfile.yaml"

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

requestModifyTransitGatewayVPCAttachment :: ModifyTransitGatewayVPCAttachment -> TestTree
requestModifyTransitGatewayVPCAttachment =
  req
    "ModifyTransitGatewayVPCAttachment"
    "fixture/ModifyTransitGatewayVPCAttachment.yaml"

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

requestCreateVPNConnectionRoute :: CreateVPNConnectionRoute -> TestTree
requestCreateVPNConnectionRoute =
  req
    "CreateVPNConnectionRoute"
    "fixture/CreateVPNConnectionRoute.yaml"

requestAssociateRouteTable :: AssociateRouteTable -> TestTree
requestAssociateRouteTable =
  req
    "AssociateRouteTable"
    "fixture/AssociateRouteTable.yaml"

requestDescribeVPNGateways :: DescribeVPNGateways -> TestTree
requestDescribeVPNGateways =
  req
    "DescribeVPNGateways"
    "fixture/DescribeVPNGateways.yaml"

requestModifyVPNConnectionOptions :: ModifyVPNConnectionOptions -> TestTree
requestModifyVPNConnectionOptions =
  req
    "ModifyVPNConnectionOptions"
    "fixture/ModifyVPNConnectionOptions.yaml"

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

requestRevokeClientVPNIngress :: RevokeClientVPNIngress -> TestTree
requestRevokeClientVPNIngress =
  req
    "RevokeClientVPNIngress"
    "fixture/RevokeClientVPNIngress.yaml"

requestUnassignPrivateIPAddresses :: UnassignPrivateIPAddresses -> TestTree
requestUnassignPrivateIPAddresses =
  req
    "UnassignPrivateIPAddresses"
    "fixture/UnassignPrivateIPAddresses.yaml"

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

requestDescribeVPCEndpointServicePermissions :: DescribeVPCEndpointServicePermissions -> TestTree
requestDescribeVPCEndpointServicePermissions =
  req
    "DescribeVPCEndpointServicePermissions"
    "fixture/DescribeVPCEndpointServicePermissions.yaml"

requestDeleteDHCPOptions :: DeleteDHCPOptions -> TestTree
requestDeleteDHCPOptions =
  req
    "DeleteDHCPOptions"
    "fixture/DeleteDHCPOptions.yaml"

requestRegisterInstanceEventNotificationAttributes :: RegisterInstanceEventNotificationAttributes -> TestTree
requestRegisterInstanceEventNotificationAttributes =
  req
    "RegisterInstanceEventNotificationAttributes"
    "fixture/RegisterInstanceEventNotificationAttributes.yaml"

requestDescribeNetworkACLs :: DescribeNetworkACLs -> TestTree
requestDescribeNetworkACLs =
  req
    "DescribeNetworkACLs"
    "fixture/DescribeNetworkACLs.yaml"

requestCancelImportTask :: CancelImportTask -> TestTree
requestCancelImportTask =
  req
    "CancelImportTask"
    "fixture/CancelImportTask.yaml"

requestDetachClassicLinkVPC :: DetachClassicLinkVPC -> TestTree
requestDetachClassicLinkVPC =
  req
    "DetachClassicLinkVPC"
    "fixture/DetachClassicLinkVPC.yaml"

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

requestDeleteClientVPNRoute :: DeleteClientVPNRoute -> TestTree
requestDeleteClientVPNRoute =
  req
    "DeleteClientVPNRoute"
    "fixture/DeleteClientVPNRoute.yaml"

requestAcceptVPCPeeringConnection :: AcceptVPCPeeringConnection -> TestTree
requestAcceptVPCPeeringConnection =
  req
    "AcceptVPCPeeringConnection"
    "fixture/AcceptVPCPeeringConnection.yaml"

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

requestModifyVPCAttribute :: ModifyVPCAttribute -> TestTree
requestModifyVPCAttribute =
  req
    "ModifyVPCAttribute"
    "fixture/ModifyVPCAttribute.yaml"

requestDescribeClientVPNConnections :: DescribeClientVPNConnections -> TestTree
requestDescribeClientVPNConnections =
  req
    "DescribeClientVPNConnections"
    "fixture/DescribeClientVPNConnections.yaml"

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

requestDeleteVPCEndpointServiceConfigurations :: DeleteVPCEndpointServiceConfigurations -> TestTree
requestDeleteVPCEndpointServiceConfigurations =
  req
    "DeleteVPCEndpointServiceConfigurations"
    "fixture/DeleteVPCEndpointServiceConfigurations.yaml"

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
    ec2
    (Proxy :: Proxy ModifyCapacityReservation)

responseGetAssociatedIPv6PoolCidrs :: GetAssociatedIPv6PoolCidrsResponse -> TestTree
responseGetAssociatedIPv6PoolCidrs =
  res
    "GetAssociatedIPv6PoolCidrsResponse"
    "fixture/GetAssociatedIPv6PoolCidrsResponse.proto"
    ec2
    (Proxy :: Proxy GetAssociatedIPv6PoolCidrs)

responseImportInstance :: ImportInstanceResponse -> TestTree
responseImportInstance =
  res
    "ImportInstanceResponse"
    "fixture/ImportInstanceResponse.proto"
    ec2
    (Proxy :: Proxy ImportInstance)

responseRevokeSecurityGroupEgress :: RevokeSecurityGroupEgressResponse -> TestTree
responseRevokeSecurityGroupEgress =
  res
    "RevokeSecurityGroupEgressResponse"
    "fixture/RevokeSecurityGroupEgressResponse.proto"
    ec2
    (Proxy :: Proxy RevokeSecurityGroupEgress)

responseCreateNetworkInterfacePermission :: CreateNetworkInterfacePermissionResponse -> TestTree
responseCreateNetworkInterfacePermission =
  res
    "CreateNetworkInterfacePermissionResponse"
    "fixture/CreateNetworkInterfacePermissionResponse.proto"
    ec2
    (Proxy :: Proxy CreateNetworkInterfacePermission)

responseSendDiagnosticInterrupt :: SendDiagnosticInterruptResponse -> TestTree
responseSendDiagnosticInterrupt =
  res
    "SendDiagnosticInterruptResponse"
    "fixture/SendDiagnosticInterruptResponse.proto"
    ec2
    (Proxy :: Proxy SendDiagnosticInterrupt)

responseDeleteLaunchTemplate :: DeleteLaunchTemplateResponse -> TestTree
responseDeleteLaunchTemplate =
  res
    "DeleteLaunchTemplateResponse"
    "fixture/DeleteLaunchTemplateResponse.proto"
    ec2
    (Proxy :: Proxy DeleteLaunchTemplate)

responseRejectVPCEndpointConnections :: RejectVPCEndpointConnectionsResponse -> TestTree
responseRejectVPCEndpointConnections =
  res
    "RejectVPCEndpointConnectionsResponse"
    "fixture/RejectVPCEndpointConnectionsResponse.proto"
    ec2
    (Proxy :: Proxy RejectVPCEndpointConnections)

responseCreateVPNGateway :: CreateVPNGatewayResponse -> TestTree
responseCreateVPNGateway =
  res
    "CreateVPNGatewayResponse"
    "fixture/CreateVPNGatewayResponse.proto"
    ec2
    (Proxy :: Proxy CreateVPNGateway)

responseCreateNetworkACL :: CreateNetworkACLResponse -> TestTree
responseCreateNetworkACL =
  res
    "CreateNetworkACLResponse"
    "fixture/CreateNetworkACLResponse.proto"
    ec2
    (Proxy :: Proxy CreateNetworkACL)

responseDeleteKeyPair :: DeleteKeyPairResponse -> TestTree
responseDeleteKeyPair =
  res
    "DeleteKeyPairResponse"
    "fixture/DeleteKeyPairResponse.proto"
    ec2
    (Proxy :: Proxy DeleteKeyPair)

responseDescribeSecurityGroupReferences :: DescribeSecurityGroupReferencesResponse -> TestTree
responseDescribeSecurityGroupReferences =
  res
    "DescribeSecurityGroupReferencesResponse"
    "fixture/DescribeSecurityGroupReferencesResponse.proto"
    ec2
    (Proxy :: Proxy DescribeSecurityGroupReferences)

responseDeleteFleets :: DeleteFleetsResponse -> TestTree
responseDeleteFleets =
  res
    "DeleteFleetsResponse"
    "fixture/DeleteFleetsResponse.proto"
    ec2
    (Proxy :: Proxy DeleteFleets)

responseDescribeTags :: DescribeTagsResponse -> TestTree
responseDescribeTags =
  res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeTags)

responseCreateTransitGatewayRouteTable :: CreateTransitGatewayRouteTableResponse -> TestTree
responseCreateTransitGatewayRouteTable =
  res
    "CreateTransitGatewayRouteTableResponse"
    "fixture/CreateTransitGatewayRouteTableResponse.proto"
    ec2
    (Proxy :: Proxy CreateTransitGatewayRouteTable)

responseModifyInstanceMetadataOptions :: ModifyInstanceMetadataOptionsResponse -> TestTree
responseModifyInstanceMetadataOptions =
  res
    "ModifyInstanceMetadataOptionsResponse"
    "fixture/ModifyInstanceMetadataOptionsResponse.proto"
    ec2
    (Proxy :: Proxy ModifyInstanceMetadataOptions)

responseUpdateSecurityGroupRuleDescriptionsIngress :: UpdateSecurityGroupRuleDescriptionsIngressResponse -> TestTree
responseUpdateSecurityGroupRuleDescriptionsIngress =
  res
    "UpdateSecurityGroupRuleDescriptionsIngressResponse"
    "fixture/UpdateSecurityGroupRuleDescriptionsIngressResponse.proto"
    ec2
    (Proxy :: Proxy UpdateSecurityGroupRuleDescriptionsIngress)

responseDisassociateSubnetCidrBlock :: DisassociateSubnetCidrBlockResponse -> TestTree
responseDisassociateSubnetCidrBlock =
  res
    "DisassociateSubnetCidrBlockResponse"
    "fixture/DisassociateSubnetCidrBlockResponse.proto"
    ec2
    (Proxy :: Proxy DisassociateSubnetCidrBlock)

responseDetachNetworkInterface :: DetachNetworkInterfaceResponse -> TestTree
responseDetachNetworkInterface =
  res
    "DetachNetworkInterfaceResponse"
    "fixture/DetachNetworkInterfaceResponse.proto"
    ec2
    (Proxy :: Proxy DetachNetworkInterface)

responseDetachInternetGateway :: DetachInternetGatewayResponse -> TestTree
responseDetachInternetGateway =
  res
    "DetachInternetGatewayResponse"
    "fixture/DetachInternetGatewayResponse.proto"
    ec2
    (Proxy :: Proxy DetachInternetGateway)

responseDeleteVPCEndpoints :: DeleteVPCEndpointsResponse -> TestTree
responseDeleteVPCEndpoints =
  res
    "DeleteVPCEndpointsResponse"
    "fixture/DeleteVPCEndpointsResponse.proto"
    ec2
    (Proxy :: Proxy DeleteVPCEndpoints)

responseDescribeClientVPNEndpoints :: DescribeClientVPNEndpointsResponse -> TestTree
responseDescribeClientVPNEndpoints =
  res
    "DescribeClientVPNEndpointsResponse"
    "fixture/DescribeClientVPNEndpointsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeClientVPNEndpoints)

responseDeleteFlowLogs :: DeleteFlowLogsResponse -> TestTree
responseDeleteFlowLogs =
  res
    "DeleteFlowLogsResponse"
    "fixture/DeleteFlowLogsResponse.proto"
    ec2
    (Proxy :: Proxy DeleteFlowLogs)

responseDescribeVPCClassicLink :: DescribeVPCClassicLinkResponse -> TestTree
responseDescribeVPCClassicLink =
  res
    "DescribeVPCClassicLinkResponse"
    "fixture/DescribeVPCClassicLinkResponse.proto"
    ec2
    (Proxy :: Proxy DescribeVPCClassicLink)

responseGetAssociatedEnclaveCertificateIAMRoles :: GetAssociatedEnclaveCertificateIAMRolesResponse -> TestTree
responseGetAssociatedEnclaveCertificateIAMRoles =
  res
    "GetAssociatedEnclaveCertificateIAMRolesResponse"
    "fixture/GetAssociatedEnclaveCertificateIAMRolesResponse.proto"
    ec2
    (Proxy :: Proxy GetAssociatedEnclaveCertificateIAMRoles)

responseAssociateTransitGatewayMulticastDomain :: AssociateTransitGatewayMulticastDomainResponse -> TestTree
responseAssociateTransitGatewayMulticastDomain =
  res
    "AssociateTransitGatewayMulticastDomainResponse"
    "fixture/AssociateTransitGatewayMulticastDomainResponse.proto"
    ec2
    (Proxy :: Proxy AssociateTransitGatewayMulticastDomain)

responseModifySubnetAttribute :: ModifySubnetAttributeResponse -> TestTree
responseModifySubnetAttribute =
  res
    "ModifySubnetAttributeResponse"
    "fixture/ModifySubnetAttributeResponse.proto"
    ec2
    (Proxy :: Proxy ModifySubnetAttribute)

responseDetachVolume :: VolumeAttachment -> TestTree
responseDetachVolume =
  res
    "DetachVolumeResponse"
    "fixture/DetachVolumeResponse.proto"
    ec2
    (Proxy :: Proxy DetachVolume)

responseDescribeInstanceCreditSpecifications :: DescribeInstanceCreditSpecificationsResponse -> TestTree
responseDescribeInstanceCreditSpecifications =
  res
    "DescribeInstanceCreditSpecificationsResponse"
    "fixture/DescribeInstanceCreditSpecificationsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeInstanceCreditSpecifications)

responseCancelBundleTask :: CancelBundleTaskResponse -> TestTree
responseCancelBundleTask =
  res
    "CancelBundleTaskResponse"
    "fixture/CancelBundleTaskResponse.proto"
    ec2
    (Proxy :: Proxy CancelBundleTask)

responseDescribeByoipCidrs :: DescribeByoipCidrsResponse -> TestTree
responseDescribeByoipCidrs =
  res
    "DescribeByoipCidrsResponse"
    "fixture/DescribeByoipCidrsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeByoipCidrs)

responseAcceptReservedInstancesExchangeQuote :: AcceptReservedInstancesExchangeQuoteResponse -> TestTree
responseAcceptReservedInstancesExchangeQuote =
  res
    "AcceptReservedInstancesExchangeQuoteResponse"
    "fixture/AcceptReservedInstancesExchangeQuoteResponse.proto"
    ec2
    (Proxy :: Proxy AcceptReservedInstancesExchangeQuote)

responseReleaseAddress :: ReleaseAddressResponse -> TestTree
responseReleaseAddress =
  res
    "ReleaseAddressResponse"
    "fixture/ReleaseAddressResponse.proto"
    ec2
    (Proxy :: Proxy ReleaseAddress)

responseDescribeInstanceTypeOfferings :: DescribeInstanceTypeOfferingsResponse -> TestTree
responseDescribeInstanceTypeOfferings =
  res
    "DescribeInstanceTypeOfferingsResponse"
    "fixture/DescribeInstanceTypeOfferingsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeInstanceTypeOfferings)

responseCreateInternetGateway :: CreateInternetGatewayResponse -> TestTree
responseCreateInternetGateway =
  res
    "CreateInternetGatewayResponse"
    "fixture/CreateInternetGatewayResponse.proto"
    ec2
    (Proxy :: Proxy CreateInternetGateway)

responseDeleteVPNConnection :: DeleteVPNConnectionResponse -> TestTree
responseDeleteVPNConnection =
  res
    "DeleteVPNConnectionResponse"
    "fixture/DeleteVPNConnectionResponse.proto"
    ec2
    (Proxy :: Proxy DeleteVPNConnection)

responseDescribeBundleTasks :: DescribeBundleTasksResponse -> TestTree
responseDescribeBundleTasks =
  res
    "DescribeBundleTasksResponse"
    "fixture/DescribeBundleTasksResponse.proto"
    ec2
    (Proxy :: Proxy DescribeBundleTasks)

responseAuthorizeSecurityGroupEgress :: AuthorizeSecurityGroupEgressResponse -> TestTree
responseAuthorizeSecurityGroupEgress =
  res
    "AuthorizeSecurityGroupEgressResponse"
    "fixture/AuthorizeSecurityGroupEgressResponse.proto"
    ec2
    (Proxy :: Proxy AuthorizeSecurityGroupEgress)

responseEnableTransitGatewayRouteTablePropagation :: EnableTransitGatewayRouteTablePropagationResponse -> TestTree
responseEnableTransitGatewayRouteTablePropagation =
  res
    "EnableTransitGatewayRouteTablePropagationResponse"
    "fixture/EnableTransitGatewayRouteTablePropagationResponse.proto"
    ec2
    (Proxy :: Proxy EnableTransitGatewayRouteTablePropagation)

responseDeregisterImage :: DeregisterImageResponse -> TestTree
responseDeregisterImage =
  res
    "DeregisterImageResponse"
    "fixture/DeregisterImageResponse.proto"
    ec2
    (Proxy :: Proxy DeregisterImage)

responseDeleteVPCEndpointConnectionNotifications :: DeleteVPCEndpointConnectionNotificationsResponse -> TestTree
responseDeleteVPCEndpointConnectionNotifications =
  res
    "DeleteVPCEndpointConnectionNotificationsResponse"
    "fixture/DeleteVPCEndpointConnectionNotificationsResponse.proto"
    ec2
    (Proxy :: Proxy DeleteVPCEndpointConnectionNotifications)

responseDescribeCoipPools :: DescribeCoipPoolsResponse -> TestTree
responseDescribeCoipPools =
  res
    "DescribeCoipPoolsResponse"
    "fixture/DescribeCoipPoolsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeCoipPools)

responseGetTransitGatewayMulticastDomainAssociations :: GetTransitGatewayMulticastDomainAssociationsResponse -> TestTree
responseGetTransitGatewayMulticastDomainAssociations =
  res
    "GetTransitGatewayMulticastDomainAssociationsResponse"
    "fixture/GetTransitGatewayMulticastDomainAssociationsResponse.proto"
    ec2
    (Proxy :: Proxy GetTransitGatewayMulticastDomainAssociations)

responseDeleteLocalGatewayRouteTableVPCAssociation :: DeleteLocalGatewayRouteTableVPCAssociationResponse -> TestTree
responseDeleteLocalGatewayRouteTableVPCAssociation =
  res
    "DeleteLocalGatewayRouteTableVPCAssociationResponse"
    "fixture/DeleteLocalGatewayRouteTableVPCAssociationResponse.proto"
    ec2
    (Proxy :: Proxy DeleteLocalGatewayRouteTableVPCAssociation)

responseModifyNetworkInterfaceAttribute :: ModifyNetworkInterfaceAttributeResponse -> TestTree
responseModifyNetworkInterfaceAttribute =
  res
    "ModifyNetworkInterfaceAttributeResponse"
    "fixture/ModifyNetworkInterfaceAttributeResponse.proto"
    ec2
    (Proxy :: Proxy ModifyNetworkInterfaceAttribute)

responseModifyVPCTenancy :: ModifyVPCTenancyResponse -> TestTree
responseModifyVPCTenancy =
  res
    "ModifyVPCTenancyResponse"
    "fixture/ModifyVPCTenancyResponse.proto"
    ec2
    (Proxy :: Proxy ModifyVPCTenancy)

responseDescribeInstanceTypes :: DescribeInstanceTypesResponse -> TestTree
responseDescribeInstanceTypes =
  res
    "DescribeInstanceTypesResponse"
    "fixture/DescribeInstanceTypesResponse.proto"
    ec2
    (Proxy :: Proxy DescribeInstanceTypes)

responseDescribeClientVPNAuthorizationRules :: DescribeClientVPNAuthorizationRulesResponse -> TestTree
responseDescribeClientVPNAuthorizationRules =
  res
    "DescribeClientVPNAuthorizationRulesResponse"
    "fixture/DescribeClientVPNAuthorizationRulesResponse.proto"
    ec2
    (Proxy :: Proxy DescribeClientVPNAuthorizationRules)

responseDeleteTransitGatewayVPCAttachment :: DeleteTransitGatewayVPCAttachmentResponse -> TestTree
responseDeleteTransitGatewayVPCAttachment =
  res
    "DeleteTransitGatewayVPCAttachmentResponse"
    "fixture/DeleteTransitGatewayVPCAttachmentResponse.proto"
    ec2
    (Proxy :: Proxy DeleteTransitGatewayVPCAttachment)

responseDeleteTransitGatewayMulticastDomain :: DeleteTransitGatewayMulticastDomainResponse -> TestTree
responseDeleteTransitGatewayMulticastDomain =
  res
    "DeleteTransitGatewayMulticastDomainResponse"
    "fixture/DeleteTransitGatewayMulticastDomainResponse.proto"
    ec2
    (Proxy :: Proxy DeleteTransitGatewayMulticastDomain)

responseCancelReservedInstancesListing :: CancelReservedInstancesListingResponse -> TestTree
responseCancelReservedInstancesListing =
  res
    "CancelReservedInstancesListingResponse"
    "fixture/CancelReservedInstancesListingResponse.proto"
    ec2
    (Proxy :: Proxy CancelReservedInstancesListing)

responseAttachClassicLinkVPC :: AttachClassicLinkVPCResponse -> TestTree
responseAttachClassicLinkVPC =
  res
    "AttachClassicLinkVPCResponse"
    "fixture/AttachClassicLinkVPCResponse.proto"
    ec2
    (Proxy :: Proxy AttachClassicLinkVPC)

responseDisableTransitGatewayRouteTablePropagation :: DisableTransitGatewayRouteTablePropagationResponse -> TestTree
responseDisableTransitGatewayRouteTablePropagation =
  res
    "DisableTransitGatewayRouteTablePropagationResponse"
    "fixture/DisableTransitGatewayRouteTablePropagationResponse.proto"
    ec2
    (Proxy :: Proxy DisableTransitGatewayRouteTablePropagation)

responseDescribeVPCClassicLinkDNSSupport :: DescribeVPCClassicLinkDNSSupportResponse -> TestTree
responseDescribeVPCClassicLinkDNSSupport =
  res
    "DescribeVPCClassicLinkDNSSupportResponse"
    "fixture/DescribeVPCClassicLinkDNSSupportResponse.proto"
    ec2
    (Proxy :: Proxy DescribeVPCClassicLinkDNSSupport)

responseAssociateSubnetCidrBlock :: AssociateSubnetCidrBlockResponse -> TestTree
responseAssociateSubnetCidrBlock =
  res
    "AssociateSubnetCidrBlockResponse"
    "fixture/AssociateSubnetCidrBlockResponse.proto"
    ec2
    (Proxy :: Proxy AssociateSubnetCidrBlock)

responseRunScheduledInstances :: RunScheduledInstancesResponse -> TestTree
responseRunScheduledInstances =
  res
    "RunScheduledInstancesResponse"
    "fixture/RunScheduledInstancesResponse.proto"
    ec2
    (Proxy :: Proxy RunScheduledInstances)

responseCreateTransitGatewayRoute :: CreateTransitGatewayRouteResponse -> TestTree
responseCreateTransitGatewayRoute =
  res
    "CreateTransitGatewayRouteResponse"
    "fixture/CreateTransitGatewayRouteResponse.proto"
    ec2
    (Proxy :: Proxy CreateTransitGatewayRoute)

responseCreateTransitGatewayPrefixListReference :: CreateTransitGatewayPrefixListReferenceResponse -> TestTree
responseCreateTransitGatewayPrefixListReference =
  res
    "CreateTransitGatewayPrefixListReferenceResponse"
    "fixture/CreateTransitGatewayPrefixListReferenceResponse.proto"
    ec2
    (Proxy :: Proxy CreateTransitGatewayPrefixListReference)

responseCancelSpotFleetRequests :: CancelSpotFleetRequestsResponse -> TestTree
responseCancelSpotFleetRequests =
  res
    "CancelSpotFleetRequestsResponse"
    "fixture/CancelSpotFleetRequestsResponse.proto"
    ec2
    (Proxy :: Proxy CancelSpotFleetRequests)

responseDescribeSpotPriceHistory :: DescribeSpotPriceHistoryResponse -> TestTree
responseDescribeSpotPriceHistory =
  res
    "DescribeSpotPriceHistoryResponse"
    "fixture/DescribeSpotPriceHistoryResponse.proto"
    ec2
    (Proxy :: Proxy DescribeSpotPriceHistory)

responseDescribeDHCPOptions :: DescribeDHCPOptionsResponse -> TestTree
responseDescribeDHCPOptions =
  res
    "DescribeDHCPOptionsResponse"
    "fixture/DescribeDHCPOptionsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeDHCPOptions)

responseImportImage :: ImportImageResponse -> TestTree
responseImportImage =
  res
    "ImportImageResponse"
    "fixture/ImportImageResponse.proto"
    ec2
    (Proxy :: Proxy ImportImage)

responseCreateLocalGatewayRouteTableVPCAssociation :: CreateLocalGatewayRouteTableVPCAssociationResponse -> TestTree
responseCreateLocalGatewayRouteTableVPCAssociation =
  res
    "CreateLocalGatewayRouteTableVPCAssociationResponse"
    "fixture/CreateLocalGatewayRouteTableVPCAssociationResponse.proto"
    ec2
    (Proxy :: Proxy CreateLocalGatewayRouteTableVPCAssociation)

responseCopyFpgaImage :: CopyFpgaImageResponse -> TestTree
responseCopyFpgaImage =
  res
    "CopyFpgaImageResponse"
    "fixture/CopyFpgaImageResponse.proto"
    ec2
    (Proxy :: Proxy CopyFpgaImage)

responseImportClientVPNClientCertificateRevocationList :: ImportClientVPNClientCertificateRevocationListResponse -> TestTree
responseImportClientVPNClientCertificateRevocationList =
  res
    "ImportClientVPNClientCertificateRevocationListResponse"
    "fixture/ImportClientVPNClientCertificateRevocationListResponse.proto"
    ec2
    (Proxy :: Proxy ImportClientVPNClientCertificateRevocationList)

responseStopInstances :: StopInstancesResponse -> TestTree
responseStopInstances =
  res
    "StopInstancesResponse"
    "fixture/StopInstancesResponse.proto"
    ec2
    (Proxy :: Proxy StopInstances)

responseEnableEBSEncryptionByDefault :: EnableEBSEncryptionByDefaultResponse -> TestTree
responseEnableEBSEncryptionByDefault =
  res
    "EnableEBSEncryptionByDefaultResponse"
    "fixture/EnableEBSEncryptionByDefaultResponse.proto"
    ec2
    (Proxy :: Proxy EnableEBSEncryptionByDefault)

responseDeregisterTransitGatewayMulticastGroupSources :: DeregisterTransitGatewayMulticastGroupSourcesResponse -> TestTree
responseDeregisterTransitGatewayMulticastGroupSources =
  res
    "DeregisterTransitGatewayMulticastGroupSourcesResponse"
    "fixture/DeregisterTransitGatewayMulticastGroupSourcesResponse.proto"
    ec2
    (Proxy :: Proxy DeregisterTransitGatewayMulticastGroupSources)

responseModifyLaunchTemplate :: ModifyLaunchTemplateResponse -> TestTree
responseModifyLaunchTemplate =
  res
    "ModifyLaunchTemplateResponse"
    "fixture/ModifyLaunchTemplateResponse.proto"
    ec2
    (Proxy :: Proxy ModifyLaunchTemplate)

responseModifyVPCEndpointConnectionNotification :: ModifyVPCEndpointConnectionNotificationResponse -> TestTree
responseModifyVPCEndpointConnectionNotification =
  res
    "ModifyVPCEndpointConnectionNotificationResponse"
    "fixture/ModifyVPCEndpointConnectionNotificationResponse.proto"
    ec2
    (Proxy :: Proxy ModifyVPCEndpointConnectionNotification)

responseDescribeInternetGateways :: DescribeInternetGatewaysResponse -> TestTree
responseDescribeInternetGateways =
  res
    "DescribeInternetGatewaysResponse"
    "fixture/DescribeInternetGatewaysResponse.proto"
    ec2
    (Proxy :: Proxy DescribeInternetGateways)

responseDisableVPCClassicLink :: DisableVPCClassicLinkResponse -> TestTree
responseDisableVPCClassicLink =
  res
    "DisableVPCClassicLinkResponse"
    "fixture/DisableVPCClassicLinkResponse.proto"
    ec2
    (Proxy :: Proxy DisableVPCClassicLink)

responseGetGroupsForCapacityReservation :: GetGroupsForCapacityReservationResponse -> TestTree
responseGetGroupsForCapacityReservation =
  res
    "GetGroupsForCapacityReservationResponse"
    "fixture/GetGroupsForCapacityReservationResponse.proto"
    ec2
    (Proxy :: Proxy GetGroupsForCapacityReservation)

responseDeleteLaunchTemplateVersions :: DeleteLaunchTemplateVersionsResponse -> TestTree
responseDeleteLaunchTemplateVersions =
  res
    "DeleteLaunchTemplateVersionsResponse"
    "fixture/DeleteLaunchTemplateVersionsResponse.proto"
    ec2
    (Proxy :: Proxy DeleteLaunchTemplateVersions)

responseBundleInstance :: BundleInstanceResponse -> TestTree
responseBundleInstance =
  res
    "BundleInstanceResponse"
    "fixture/BundleInstanceResponse.proto"
    ec2
    (Proxy :: Proxy BundleInstance)

responseDescribeNetworkInterfaces :: DescribeNetworkInterfacesResponse -> TestTree
responseDescribeNetworkInterfaces =
  res
    "DescribeNetworkInterfacesResponse"
    "fixture/DescribeNetworkInterfacesResponse.proto"
    ec2
    (Proxy :: Proxy DescribeNetworkInterfaces)

responseReplaceNetworkACLAssociation :: ReplaceNetworkACLAssociationResponse -> TestTree
responseReplaceNetworkACLAssociation =
  res
    "ReplaceNetworkACLAssociationResponse"
    "fixture/ReplaceNetworkACLAssociationResponse.proto"
    ec2
    (Proxy :: Proxy ReplaceNetworkACLAssociation)

responseDescribeNatGateways :: DescribeNatGatewaysResponse -> TestTree
responseDescribeNatGateways =
  res
    "DescribeNatGatewaysResponse"
    "fixture/DescribeNatGatewaysResponse.proto"
    ec2
    (Proxy :: Proxy DescribeNatGateways)

responseDescribeAddresses :: DescribeAddressesResponse -> TestTree
responseDescribeAddresses =
  res
    "DescribeAddressesResponse"
    "fixture/DescribeAddressesResponse.proto"
    ec2
    (Proxy :: Proxy DescribeAddresses)

responseRestoreManagedPrefixListVersion :: RestoreManagedPrefixListVersionResponse -> TestTree
responseRestoreManagedPrefixListVersion =
  res
    "RestoreManagedPrefixListVersionResponse"
    "fixture/RestoreManagedPrefixListVersionResponse.proto"
    ec2
    (Proxy :: Proxy RestoreManagedPrefixListVersion)

responseDescribeSnapshotAttribute :: DescribeSnapshotAttributeResponse -> TestTree
responseDescribeSnapshotAttribute =
  res
    "DescribeSnapshotAttributeResponse"
    "fixture/DescribeSnapshotAttributeResponse.proto"
    ec2
    (Proxy :: Proxy DescribeSnapshotAttribute)

responseDescribeIdentityIdFormat :: DescribeIdentityIdFormatResponse -> TestTree
responseDescribeIdentityIdFormat =
  res
    "DescribeIdentityIdFormatResponse"
    "fixture/DescribeIdentityIdFormatResponse.proto"
    ec2
    (Proxy :: Proxy DescribeIdentityIdFormat)

responseReplaceRoute :: ReplaceRouteResponse -> TestTree
responseReplaceRoute =
  res
    "ReplaceRouteResponse"
    "fixture/ReplaceRouteResponse.proto"
    ec2
    (Proxy :: Proxy ReplaceRoute)

responseDescribeVPCEndpointServices :: DescribeVPCEndpointServicesResponse -> TestTree
responseDescribeVPCEndpointServices =
  res
    "DescribeVPCEndpointServicesResponse"
    "fixture/DescribeVPCEndpointServicesResponse.proto"
    ec2
    (Proxy :: Proxy DescribeVPCEndpointServices)

responseDeleteLocalGatewayRoute :: DeleteLocalGatewayRouteResponse -> TestTree
responseDeleteLocalGatewayRoute =
  res
    "DeleteLocalGatewayRouteResponse"
    "fixture/DeleteLocalGatewayRouteResponse.proto"
    ec2
    (Proxy :: Proxy DeleteLocalGatewayRoute)

responseAuthorizeSecurityGroupIngress :: AuthorizeSecurityGroupIngressResponse -> TestTree
responseAuthorizeSecurityGroupIngress =
  res
    "AuthorizeSecurityGroupIngressResponse"
    "fixture/AuthorizeSecurityGroupIngressResponse.proto"
    ec2
    (Proxy :: Proxy AuthorizeSecurityGroupIngress)

responseCreateVPCPeeringConnection :: CreateVPCPeeringConnectionResponse -> TestTree
responseCreateVPCPeeringConnection =
  res
    "CreateVPCPeeringConnectionResponse"
    "fixture/CreateVPCPeeringConnectionResponse.proto"
    ec2
    (Proxy :: Proxy CreateVPCPeeringConnection)

responseDescribeSubnets :: DescribeSubnetsResponse -> TestTree
responseDescribeSubnets =
  res
    "DescribeSubnetsResponse"
    "fixture/DescribeSubnetsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeSubnets)

responseGetTransitGatewayAttachmentPropagations :: GetTransitGatewayAttachmentPropagationsResponse -> TestTree
responseGetTransitGatewayAttachmentPropagations =
  res
    "GetTransitGatewayAttachmentPropagationsResponse"
    "fixture/GetTransitGatewayAttachmentPropagationsResponse.proto"
    ec2
    (Proxy :: Proxy GetTransitGatewayAttachmentPropagations)

responseCreateTags :: CreateTagsResponse -> TestTree
responseCreateTags =
  res
    "CreateTagsResponse"
    "fixture/CreateTagsResponse.proto"
    ec2
    (Proxy :: Proxy CreateTags)

responsePurchaseReservedInstancesOffering :: PurchaseReservedInstancesOfferingResponse -> TestTree
responsePurchaseReservedInstancesOffering =
  res
    "PurchaseReservedInstancesOfferingResponse"
    "fixture/PurchaseReservedInstancesOfferingResponse.proto"
    ec2
    (Proxy :: Proxy PurchaseReservedInstancesOffering)

responseDeleteNetworkACLEntry :: DeleteNetworkACLEntryResponse -> TestTree
responseDeleteNetworkACLEntry =
  res
    "DeleteNetworkACLEntryResponse"
    "fixture/DeleteNetworkACLEntryResponse.proto"
    ec2
    (Proxy :: Proxy DeleteNetworkACLEntry)

responseResetSnapshotAttribute :: ResetSnapshotAttributeResponse -> TestTree
responseResetSnapshotAttribute =
  res
    "ResetSnapshotAttributeResponse"
    "fixture/ResetSnapshotAttributeResponse.proto"
    ec2
    (Proxy :: Proxy ResetSnapshotAttribute)

responseDescribeVPNConnections :: DescribeVPNConnectionsResponse -> TestTree
responseDescribeVPNConnections =
  res
    "DescribeVPNConnectionsResponse"
    "fixture/DescribeVPNConnectionsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeVPNConnections)

responseModifyInstanceEventStartTime :: ModifyInstanceEventStartTimeResponse -> TestTree
responseModifyInstanceEventStartTime =
  res
    "ModifyInstanceEventStartTimeResponse"
    "fixture/ModifyInstanceEventStartTimeResponse.proto"
    ec2
    (Proxy :: Proxy ModifyInstanceEventStartTime)

responseDeleteRoute :: DeleteRouteResponse -> TestTree
responseDeleteRoute =
  res
    "DeleteRouteResponse"
    "fixture/DeleteRouteResponse.proto"
    ec2
    (Proxy :: Proxy DeleteRoute)

responseReplaceNetworkACLEntry :: ReplaceNetworkACLEntryResponse -> TestTree
responseReplaceNetworkACLEntry =
  res
    "ReplaceNetworkACLEntryResponse"
    "fixture/ReplaceNetworkACLEntryResponse.proto"
    ec2
    (Proxy :: Proxy ReplaceNetworkACLEntry)

responseDescribeVPCEndpoints :: DescribeVPCEndpointsResponse -> TestTree
responseDescribeVPCEndpoints =
  res
    "DescribeVPCEndpointsResponse"
    "fixture/DescribeVPCEndpointsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeVPCEndpoints)

responseCreateTrafficMirrorFilter :: CreateTrafficMirrorFilterResponse -> TestTree
responseCreateTrafficMirrorFilter =
  res
    "CreateTrafficMirrorFilterResponse"
    "fixture/CreateTrafficMirrorFilterResponse.proto"
    ec2
    (Proxy :: Proxy CreateTrafficMirrorFilter)

responseResetInstanceAttribute :: ResetInstanceAttributeResponse -> TestTree
responseResetInstanceAttribute =
  res
    "ResetInstanceAttributeResponse"
    "fixture/ResetInstanceAttributeResponse.proto"
    ec2
    (Proxy :: Proxy ResetInstanceAttribute)

responseModifyIdentityIdFormat :: ModifyIdentityIdFormatResponse -> TestTree
responseModifyIdentityIdFormat =
  res
    "ModifyIdentityIdFormatResponse"
    "fixture/ModifyIdentityIdFormatResponse.proto"
    ec2
    (Proxy :: Proxy ModifyIdentityIdFormat)

responseAttachNetworkInterface :: AttachNetworkInterfaceResponse -> TestTree
responseAttachNetworkInterface =
  res
    "AttachNetworkInterfaceResponse"
    "fixture/AttachNetworkInterfaceResponse.proto"
    ec2
    (Proxy :: Proxy AttachNetworkInterface)

responseCreateCapacityReservation :: CreateCapacityReservationResponse -> TestTree
responseCreateCapacityReservation =
  res
    "CreateCapacityReservationResponse"
    "fixture/CreateCapacityReservationResponse.proto"
    ec2
    (Proxy :: Proxy CreateCapacityReservation)

responseDescribeInstanceStatus :: DescribeInstanceStatusResponse -> TestTree
responseDescribeInstanceStatus =
  res
    "DescribeInstanceStatusResponse"
    "fixture/DescribeInstanceStatusResponse.proto"
    ec2
    (Proxy :: Proxy DescribeInstanceStatus)

responseImportKeyPair :: ImportKeyPairResponse -> TestTree
responseImportKeyPair =
  res
    "ImportKeyPairResponse"
    "fixture/ImportKeyPairResponse.proto"
    ec2
    (Proxy :: Proxy ImportKeyPair)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags =
  res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    ec2
    (Proxy :: Proxy DeleteTags)

responseConfirmProductInstance :: ConfirmProductInstanceResponse -> TestTree
responseConfirmProductInstance =
  res
    "ConfirmProductInstanceResponse"
    "fixture/ConfirmProductInstanceResponse.proto"
    ec2
    (Proxy :: Proxy ConfirmProductInstance)

responseDescribeInstanceAttribute :: DescribeInstanceAttributeResponse -> TestTree
responseDescribeInstanceAttribute =
  res
    "DescribeInstanceAttributeResponse"
    "fixture/DescribeInstanceAttributeResponse.proto"
    ec2
    (Proxy :: Proxy DescribeInstanceAttribute)

responseDescribeReservedInstancesOfferings :: DescribeReservedInstancesOfferingsResponse -> TestTree
responseDescribeReservedInstancesOfferings =
  res
    "DescribeReservedInstancesOfferingsResponse"
    "fixture/DescribeReservedInstancesOfferingsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeReservedInstancesOfferings)

responseCreateCustomerGateway :: CreateCustomerGatewayResponse -> TestTree
responseCreateCustomerGateway =
  res
    "CreateCustomerGatewayResponse"
    "fixture/CreateCustomerGatewayResponse.proto"
    ec2
    (Proxy :: Proxy CreateCustomerGateway)

responseDescribeFleets :: DescribeFleetsResponse -> TestTree
responseDescribeFleets =
  res
    "DescribeFleetsResponse"
    "fixture/DescribeFleetsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeFleets)

responseCreateTransitGatewayPeeringAttachment :: CreateTransitGatewayPeeringAttachmentResponse -> TestTree
responseCreateTransitGatewayPeeringAttachment =
  res
    "CreateTransitGatewayPeeringAttachmentResponse"
    "fixture/CreateTransitGatewayPeeringAttachmentResponse.proto"
    ec2
    (Proxy :: Proxy CreateTransitGatewayPeeringAttachment)

responseDeleteSecurityGroup :: DeleteSecurityGroupResponse -> TestTree
responseDeleteSecurityGroup =
  res
    "DeleteSecurityGroupResponse"
    "fixture/DeleteSecurityGroupResponse.proto"
    ec2
    (Proxy :: Proxy DeleteSecurityGroup)

responseDescribePublicIPv4Pools :: DescribePublicIPv4PoolsResponse -> TestTree
responseDescribePublicIPv4Pools =
  res
    "DescribePublicIPv4PoolsResponse"
    "fixture/DescribePublicIPv4PoolsResponse.proto"
    ec2
    (Proxy :: Proxy DescribePublicIPv4Pools)

responseDescribeClientVPNTargetNetworks :: DescribeClientVPNTargetNetworksResponse -> TestTree
responseDescribeClientVPNTargetNetworks =
  res
    "DescribeClientVPNTargetNetworksResponse"
    "fixture/DescribeClientVPNTargetNetworksResponse.proto"
    ec2
    (Proxy :: Proxy DescribeClientVPNTargetNetworks)

responseDeleteVPCPeeringConnection :: DeleteVPCPeeringConnectionResponse -> TestTree
responseDeleteVPCPeeringConnection =
  res
    "DeleteVPCPeeringConnectionResponse"
    "fixture/DeleteVPCPeeringConnectionResponse.proto"
    ec2
    (Proxy :: Proxy DeleteVPCPeeringConnection)

responseAttachInternetGateway :: AttachInternetGatewayResponse -> TestTree
responseAttachInternetGateway =
  res
    "AttachInternetGatewayResponse"
    "fixture/AttachInternetGatewayResponse.proto"
    ec2
    (Proxy :: Proxy AttachInternetGateway)

responseModifyInstancePlacement :: ModifyInstancePlacementResponse -> TestTree
responseModifyInstancePlacement =
  res
    "ModifyInstancePlacementResponse"
    "fixture/ModifyInstancePlacementResponse.proto"
    ec2
    (Proxy :: Proxy ModifyInstancePlacement)

responseDescribeFlowLogs :: DescribeFlowLogsResponse -> TestTree
responseDescribeFlowLogs =
  res
    "DescribeFlowLogsResponse"
    "fixture/DescribeFlowLogsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeFlowLogs)

responseDescribeLocalGatewayVirtualInterfaceGroups :: DescribeLocalGatewayVirtualInterfaceGroupsResponse -> TestTree
responseDescribeLocalGatewayVirtualInterfaceGroups =
  res
    "DescribeLocalGatewayVirtualInterfaceGroupsResponse"
    "fixture/DescribeLocalGatewayVirtualInterfaceGroupsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeLocalGatewayVirtualInterfaceGroups)

responseDescribeLocalGatewayRouteTableVPCAssociations :: DescribeLocalGatewayRouteTableVPCAssociationsResponse -> TestTree
responseDescribeLocalGatewayRouteTableVPCAssociations =
  res
    "DescribeLocalGatewayRouteTableVPCAssociationsResponse"
    "fixture/DescribeLocalGatewayRouteTableVPCAssociationsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeLocalGatewayRouteTableVPCAssociations)

responseDescribeVPCEndpointConnectionNotifications :: DescribeVPCEndpointConnectionNotificationsResponse -> TestTree
responseDescribeVPCEndpointConnectionNotifications =
  res
    "DescribeVPCEndpointConnectionNotificationsResponse"
    "fixture/DescribeVPCEndpointConnectionNotificationsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeVPCEndpointConnectionNotifications)

responseGetManagedPrefixListEntries :: GetManagedPrefixListEntriesResponse -> TestTree
responseGetManagedPrefixListEntries =
  res
    "GetManagedPrefixListEntriesResponse"
    "fixture/GetManagedPrefixListEntriesResponse.proto"
    ec2
    (Proxy :: Proxy GetManagedPrefixListEntries)

responseRunInstances :: Reservation -> TestTree
responseRunInstances =
  res
    "RunInstancesResponse"
    "fixture/RunInstancesResponse.proto"
    ec2
    (Proxy :: Proxy RunInstances)

responseCreateSnapshots :: CreateSnapshotsResponse -> TestTree
responseCreateSnapshots =
  res
    "CreateSnapshotsResponse"
    "fixture/CreateSnapshotsResponse.proto"
    ec2
    (Proxy :: Proxy CreateSnapshots)

responseAssociateDHCPOptions :: AssociateDHCPOptionsResponse -> TestTree
responseAssociateDHCPOptions =
  res
    "AssociateDHCPOptionsResponse"
    "fixture/AssociateDHCPOptionsResponse.proto"
    ec2
    (Proxy :: Proxy AssociateDHCPOptions)

responseDeleteTrafficMirrorFilterRule :: DeleteTrafficMirrorFilterRuleResponse -> TestTree
responseDeleteTrafficMirrorFilterRule =
  res
    "DeleteTrafficMirrorFilterRuleResponse"
    "fixture/DeleteTrafficMirrorFilterRuleResponse.proto"
    ec2
    (Proxy :: Proxy DeleteTrafficMirrorFilterRule)

responseDescribeReservedInstances :: DescribeReservedInstancesResponse -> TestTree
responseDescribeReservedInstances =
  res
    "DescribeReservedInstancesResponse"
    "fixture/DescribeReservedInstancesResponse.proto"
    ec2
    (Proxy :: Proxy DescribeReservedInstances)

responseDescribeIdFormat :: DescribeIdFormatResponse -> TestTree
responseDescribeIdFormat =
  res
    "DescribeIdFormatResponse"
    "fixture/DescribeIdFormatResponse.proto"
    ec2
    (Proxy :: Proxy DescribeIdFormat)

responseDescribeVPCs :: DescribeVPCsResponse -> TestTree
responseDescribeVPCs =
  res
    "DescribeVPCsResponse"
    "fixture/DescribeVPCsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeVPCs)

responseDescribeConversionTasks :: DescribeConversionTasksResponse -> TestTree
responseDescribeConversionTasks =
  res
    "DescribeConversionTasksResponse"
    "fixture/DescribeConversionTasksResponse.proto"
    ec2
    (Proxy :: Proxy DescribeConversionTasks)

responseCreateLaunchTemplateVersion :: CreateLaunchTemplateVersionResponse -> TestTree
responseCreateLaunchTemplateVersion =
  res
    "CreateLaunchTemplateVersionResponse"
    "fixture/CreateLaunchTemplateVersionResponse.proto"
    ec2
    (Proxy :: Proxy CreateLaunchTemplateVersion)

responseGetManagedPrefixListAssociations :: GetManagedPrefixListAssociationsResponse -> TestTree
responseGetManagedPrefixListAssociations =
  res
    "GetManagedPrefixListAssociationsResponse"
    "fixture/GetManagedPrefixListAssociationsResponse.proto"
    ec2
    (Proxy :: Proxy GetManagedPrefixListAssociations)

responseDisableVPCClassicLinkDNSSupport :: DisableVPCClassicLinkDNSSupportResponse -> TestTree
responseDisableVPCClassicLinkDNSSupport =
  res
    "DisableVPCClassicLinkDNSSupportResponse"
    "fixture/DisableVPCClassicLinkDNSSupportResponse.proto"
    ec2
    (Proxy :: Proxy DisableVPCClassicLinkDNSSupport)

responseApplySecurityGroupsToClientVPNTargetNetwork :: ApplySecurityGroupsToClientVPNTargetNetworkResponse -> TestTree
responseApplySecurityGroupsToClientVPNTargetNetwork =
  res
    "ApplySecurityGroupsToClientVPNTargetNetworkResponse"
    "fixture/ApplySecurityGroupsToClientVPNTargetNetworkResponse.proto"
    ec2
    (Proxy :: Proxy ApplySecurityGroupsToClientVPNTargetNetwork)

responseDescribeTrafficMirrorTargets :: DescribeTrafficMirrorTargetsResponse -> TestTree
responseDescribeTrafficMirrorTargets =
  res
    "DescribeTrafficMirrorTargetsResponse"
    "fixture/DescribeTrafficMirrorTargetsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeTrafficMirrorTargets)

responseDescribeVolumesModifications :: DescribeVolumesModificationsResponse -> TestTree
responseDescribeVolumesModifications =
  res
    "DescribeVolumesModificationsResponse"
    "fixture/DescribeVolumesModificationsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeVolumesModifications)

responseExportImage :: ExportImageResponse -> TestTree
responseExportImage =
  res
    "ExportImageResponse"
    "fixture/ExportImageResponse.proto"
    ec2
    (Proxy :: Proxy ExportImage)

responseCreateFpgaImage :: CreateFpgaImageResponse -> TestTree
responseCreateFpgaImage =
  res
    "CreateFpgaImageResponse"
    "fixture/CreateFpgaImageResponse.proto"
    ec2
    (Proxy :: Proxy CreateFpgaImage)

responseAcceptVPCEndpointConnections :: AcceptVPCEndpointConnectionsResponse -> TestTree
responseAcceptVPCEndpointConnections =
  res
    "AcceptVPCEndpointConnectionsResponse"
    "fixture/AcceptVPCEndpointConnectionsResponse.proto"
    ec2
    (Proxy :: Proxy AcceptVPCEndpointConnections)

responseDeleteClientVPNEndpoint :: DeleteClientVPNEndpointResponse -> TestTree
responseDeleteClientVPNEndpoint =
  res
    "DeleteClientVPNEndpointResponse"
    "fixture/DeleteClientVPNEndpointResponse.proto"
    ec2
    (Proxy :: Proxy DeleteClientVPNEndpoint)

responseSearchTransitGatewayRoutes :: SearchTransitGatewayRoutesResponse -> TestTree
responseSearchTransitGatewayRoutes =
  res
    "SearchTransitGatewayRoutesResponse"
    "fixture/SearchTransitGatewayRoutesResponse.proto"
    ec2
    (Proxy :: Proxy SearchTransitGatewayRoutes)

responseGetLaunchTemplateData :: GetLaunchTemplateDataResponse -> TestTree
responseGetLaunchTemplateData =
  res
    "GetLaunchTemplateDataResponse"
    "fixture/GetLaunchTemplateDataResponse.proto"
    ec2
    (Proxy :: Proxy GetLaunchTemplateData)

responseAllocateAddress :: AllocateAddressResponse -> TestTree
responseAllocateAddress =
  res
    "AllocateAddressResponse"
    "fixture/AllocateAddressResponse.proto"
    ec2
    (Proxy :: Proxy AllocateAddress)

responseAcceptTransitGatewayVPCAttachment :: AcceptTransitGatewayVPCAttachmentResponse -> TestTree
responseAcceptTransitGatewayVPCAttachment =
  res
    "AcceptTransitGatewayVPCAttachmentResponse"
    "fixture/AcceptTransitGatewayVPCAttachmentResponse.proto"
    ec2
    (Proxy :: Proxy AcceptTransitGatewayVPCAttachment)

responseCancelConversionTask :: CancelConversionTaskResponse -> TestTree
responseCancelConversionTask =
  res
    "CancelConversionTaskResponse"
    "fixture/CancelConversionTaskResponse.proto"
    ec2
    (Proxy :: Proxy CancelConversionTask)

responseModifyImageAttribute :: ModifyImageAttributeResponse -> TestTree
responseModifyImageAttribute =
  res
    "ModifyImageAttributeResponse"
    "fixture/ModifyImageAttributeResponse.proto"
    ec2
    (Proxy :: Proxy ModifyImageAttribute)

responseCreateRouteTable :: CreateRouteTableResponse -> TestTree
responseCreateRouteTable =
  res
    "CreateRouteTableResponse"
    "fixture/CreateRouteTableResponse.proto"
    ec2
    (Proxy :: Proxy CreateRouteTable)

responseRejectTransitGatewayPeeringAttachment :: RejectTransitGatewayPeeringAttachmentResponse -> TestTree
responseRejectTransitGatewayPeeringAttachment =
  res
    "RejectTransitGatewayPeeringAttachmentResponse"
    "fixture/RejectTransitGatewayPeeringAttachmentResponse.proto"
    ec2
    (Proxy :: Proxy RejectTransitGatewayPeeringAttachment)

responseReportInstanceStatus :: ReportInstanceStatusResponse -> TestTree
responseReportInstanceStatus =
  res
    "ReportInstanceStatusResponse"
    "fixture/ReportInstanceStatusResponse.proto"
    ec2
    (Proxy :: Proxy ReportInstanceStatus)

responseAttachVolume :: VolumeAttachment -> TestTree
responseAttachVolume =
  res
    "AttachVolumeResponse"
    "fixture/AttachVolumeResponse.proto"
    ec2
    (Proxy :: Proxy AttachVolume)

responseRequestSpotInstances :: RequestSpotInstancesResponse -> TestTree
responseRequestSpotInstances =
  res
    "RequestSpotInstancesResponse"
    "fixture/RequestSpotInstancesResponse.proto"
    ec2
    (Proxy :: Proxy RequestSpotInstances)

responseWithdrawByoipCidr :: WithdrawByoipCidrResponse -> TestTree
responseWithdrawByoipCidr =
  res
    "WithdrawByoipCidrResponse"
    "fixture/WithdrawByoipCidrResponse.proto"
    ec2
    (Proxy :: Proxy WithdrawByoipCidr)

responseDescribeHostReservationOfferings :: DescribeHostReservationOfferingsResponse -> TestTree
responseDescribeHostReservationOfferings =
  res
    "DescribeHostReservationOfferingsResponse"
    "fixture/DescribeHostReservationOfferingsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeHostReservationOfferings)

responseResetFpgaImageAttribute :: ResetFpgaImageAttributeResponse -> TestTree
responseResetFpgaImageAttribute =
  res
    "ResetFpgaImageAttributeResponse"
    "fixture/ResetFpgaImageAttributeResponse.proto"
    ec2
    (Proxy :: Proxy ResetFpgaImageAttribute)

responseModifyVPNConnection :: ModifyVPNConnectionResponse -> TestTree
responseModifyVPNConnection =
  res
    "ModifyVPNConnectionResponse"
    "fixture/ModifyVPNConnectionResponse.proto"
    ec2
    (Proxy :: Proxy ModifyVPNConnection)

responseCreateTrafficMirrorFilterRule :: CreateTrafficMirrorFilterRuleResponse -> TestTree
responseCreateTrafficMirrorFilterRule =
  res
    "CreateTrafficMirrorFilterRuleResponse"
    "fixture/CreateTrafficMirrorFilterRuleResponse.proto"
    ec2
    (Proxy :: Proxy CreateTrafficMirrorFilterRule)

responseDeleteTransitGateway :: DeleteTransitGatewayResponse -> TestTree
responseDeleteTransitGateway =
  res
    "DeleteTransitGatewayResponse"
    "fixture/DeleteTransitGatewayResponse.proto"
    ec2
    (Proxy :: Proxy DeleteTransitGateway)

responseStartVPCEndpointServicePrivateDNSVerification :: StartVPCEndpointServicePrivateDNSVerificationResponse -> TestTree
responseStartVPCEndpointServicePrivateDNSVerification =
  res
    "StartVPCEndpointServicePrivateDNSVerificationResponse"
    "fixture/StartVPCEndpointServicePrivateDNSVerificationResponse.proto"
    ec2
    (Proxy :: Proxy StartVPCEndpointServicePrivateDNSVerification)

responseDescribeVolumes :: DescribeVolumesResponse -> TestTree
responseDescribeVolumes =
  res
    "DescribeVolumesResponse"
    "fixture/DescribeVolumesResponse.proto"
    ec2
    (Proxy :: Proxy DescribeVolumes)

responseRejectVPCPeeringConnection :: RejectVPCPeeringConnectionResponse -> TestTree
responseRejectVPCPeeringConnection =
  res
    "RejectVPCPeeringConnectionResponse"
    "fixture/RejectVPCPeeringConnectionResponse.proto"
    ec2
    (Proxy :: Proxy RejectVPCPeeringConnection)

responseDescribeClientVPNRoutes :: DescribeClientVPNRoutesResponse -> TestTree
responseDescribeClientVPNRoutes =
  res
    "DescribeClientVPNRoutesResponse"
    "fixture/DescribeClientVPNRoutesResponse.proto"
    ec2
    (Proxy :: Proxy DescribeClientVPNRoutes)

responseDeleteVPNConnectionRoute :: DeleteVPNConnectionRouteResponse -> TestTree
responseDeleteVPNConnectionRoute =
  res
    "DeleteVPNConnectionRouteResponse"
    "fixture/DeleteVPNConnectionRouteResponse.proto"
    ec2
    (Proxy :: Proxy DeleteVPNConnectionRoute)

responseAssociateEnclaveCertificateIAMRole :: AssociateEnclaveCertificateIAMRoleResponse -> TestTree
responseAssociateEnclaveCertificateIAMRole =
  res
    "AssociateEnclaveCertificateIAMRoleResponse"
    "fixture/AssociateEnclaveCertificateIAMRoleResponse.proto"
    ec2
    (Proxy :: Proxy AssociateEnclaveCertificateIAMRole)

responseModifyVPCEndpoint :: ModifyVPCEndpointResponse -> TestTree
responseModifyVPCEndpoint =
  res
    "ModifyVPCEndpointResponse"
    "fixture/ModifyVPCEndpointResponse.proto"
    ec2
    (Proxy :: Proxy ModifyVPCEndpoint)

responseDescribeFpgaImageAttribute :: DescribeFpgaImageAttributeResponse -> TestTree
responseDescribeFpgaImageAttribute =
  res
    "DescribeFpgaImageAttributeResponse"
    "fixture/DescribeFpgaImageAttributeResponse.proto"
    ec2
    (Proxy :: Proxy DescribeFpgaImageAttribute)

responseAllocateHosts :: AllocateHostsResponse -> TestTree
responseAllocateHosts =
  res
    "AllocateHostsResponse"
    "fixture/AllocateHostsResponse.proto"
    ec2
    (Proxy :: Proxy AllocateHosts)

responseCreateClientVPNEndpoint :: CreateClientVPNEndpointResponse -> TestTree
responseCreateClientVPNEndpoint =
  res
    "CreateClientVPNEndpointResponse"
    "fixture/CreateClientVPNEndpointResponse.proto"
    ec2
    (Proxy :: Proxy CreateClientVPNEndpoint)

responseCreateTrafficMirrorSession :: CreateTrafficMirrorSessionResponse -> TestTree
responseCreateTrafficMirrorSession =
  res
    "CreateTrafficMirrorSessionResponse"
    "fixture/CreateTrafficMirrorSessionResponse.proto"
    ec2
    (Proxy :: Proxy CreateTrafficMirrorSession)

responseRegisterImage :: RegisterImageResponse -> TestTree
responseRegisterImage =
  res
    "RegisterImageResponse"
    "fixture/RegisterImageResponse.proto"
    ec2
    (Proxy :: Proxy RegisterImage)

responseAdvertiseByoipCidr :: AdvertiseByoipCidrResponse -> TestTree
responseAdvertiseByoipCidr =
  res
    "AdvertiseByoipCidrResponse"
    "fixture/AdvertiseByoipCidrResponse.proto"
    ec2
    (Proxy :: Proxy AdvertiseByoipCidr)

responseModifyFleet :: ModifyFleetResponse -> TestTree
responseModifyFleet =
  res
    "ModifyFleetResponse"
    "fixture/ModifyFleetResponse.proto"
    ec2
    (Proxy :: Proxy ModifyFleet)

responseRevokeSecurityGroupIngress :: RevokeSecurityGroupIngressResponse -> TestTree
responseRevokeSecurityGroupIngress =
  res
    "RevokeSecurityGroupIngressResponse"
    "fixture/RevokeSecurityGroupIngressResponse.proto"
    ec2
    (Proxy :: Proxy RevokeSecurityGroupIngress)

responseGetEBSDefaultKMSKeyId :: GetEBSDefaultKMSKeyIdResponse -> TestTree
responseGetEBSDefaultKMSKeyId =
  res
    "GetEBSDefaultKMSKeyIdResponse"
    "fixture/GetEBSDefaultKMSKeyIdResponse.proto"
    ec2
    (Proxy :: Proxy GetEBSDefaultKMSKeyId)

responseDescribeHostReservations :: DescribeHostReservationsResponse -> TestTree
responseDescribeHostReservations =
  res
    "DescribeHostReservationsResponse"
    "fixture/DescribeHostReservationsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeHostReservations)

responseUpdateSecurityGroupRuleDescriptionsEgress :: UpdateSecurityGroupRuleDescriptionsEgressResponse -> TestTree
responseUpdateSecurityGroupRuleDescriptionsEgress =
  res
    "UpdateSecurityGroupRuleDescriptionsEgressResponse"
    "fixture/UpdateSecurityGroupRuleDescriptionsEgressResponse.proto"
    ec2
    (Proxy :: Proxy UpdateSecurityGroupRuleDescriptionsEgress)

responseEnableVPCClassicLinkDNSSupport :: EnableVPCClassicLinkDNSSupportResponse -> TestTree
responseEnableVPCClassicLinkDNSSupport =
  res
    "EnableVPCClassicLinkDNSSupportResponse"
    "fixture/EnableVPCClassicLinkDNSSupportResponse.proto"
    ec2
    (Proxy :: Proxy EnableVPCClassicLinkDNSSupport)

responseDescribeVPCEndpointConnections :: DescribeVPCEndpointConnectionsResponse -> TestTree
responseDescribeVPCEndpointConnections =
  res
    "DescribeVPCEndpointConnectionsResponse"
    "fixture/DescribeVPCEndpointConnectionsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeVPCEndpointConnections)

responseModifyReservedInstances :: ModifyReservedInstancesResponse -> TestTree
responseModifyReservedInstances =
  res
    "ModifyReservedInstancesResponse"
    "fixture/ModifyReservedInstancesResponse.proto"
    ec2
    (Proxy :: Proxy ModifyReservedInstances)

responseDeleteFpgaImage :: DeleteFpgaImageResponse -> TestTree
responseDeleteFpgaImage =
  res
    "DeleteFpgaImageResponse"
    "fixture/DeleteFpgaImageResponse.proto"
    ec2
    (Proxy :: Proxy DeleteFpgaImage)

responseDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations :: DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse -> TestTree
responseDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations =
  res
    "DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse"
    "fixture/DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations)

responseDescribeScheduledInstances :: DescribeScheduledInstancesResponse -> TestTree
responseDescribeScheduledInstances =
  res
    "DescribeScheduledInstancesResponse"
    "fixture/DescribeScheduledInstancesResponse.proto"
    ec2
    (Proxy :: Proxy DescribeScheduledInstances)

responseSearchTransitGatewayMulticastGroups :: SearchTransitGatewayMulticastGroupsResponse -> TestTree
responseSearchTransitGatewayMulticastGroups =
  res
    "SearchTransitGatewayMulticastGroupsResponse"
    "fixture/SearchTransitGatewayMulticastGroupsResponse.proto"
    ec2
    (Proxy :: Proxy SearchTransitGatewayMulticastGroups)

responseCreateFlowLogs :: CreateFlowLogsResponse -> TestTree
responseCreateFlowLogs =
  res
    "CreateFlowLogsResponse"
    "fixture/CreateFlowLogsResponse.proto"
    ec2
    (Proxy :: Proxy CreateFlowLogs)

responseDescribeSpotFleetRequests :: DescribeSpotFleetRequestsResponse -> TestTree
responseDescribeSpotFleetRequests =
  res
    "DescribeSpotFleetRequestsResponse"
    "fixture/DescribeSpotFleetRequestsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeSpotFleetRequests)

responseMoveAddressToVPC :: MoveAddressToVPCResponse -> TestTree
responseMoveAddressToVPC =
  res
    "MoveAddressToVPCResponse"
    "fixture/MoveAddressToVPCResponse.proto"
    ec2
    (Proxy :: Proxy MoveAddressToVPC)

responseDescribeFleetInstances :: DescribeFleetInstancesResponse -> TestTree
responseDescribeFleetInstances =
  res
    "DescribeFleetInstancesResponse"
    "fixture/DescribeFleetInstancesResponse.proto"
    ec2
    (Proxy :: Proxy DescribeFleetInstances)

responseDescribeLaunchTemplateVersions :: DescribeLaunchTemplateVersionsResponse -> TestTree
responseDescribeLaunchTemplateVersions =
  res
    "DescribeLaunchTemplateVersionsResponse"
    "fixture/DescribeLaunchTemplateVersionsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeLaunchTemplateVersions)

responseModifyInstanceCreditSpecification :: ModifyInstanceCreditSpecificationResponse -> TestTree
responseModifyInstanceCreditSpecification =
  res
    "ModifyInstanceCreditSpecificationResponse"
    "fixture/ModifyInstanceCreditSpecificationResponse.proto"
    ec2
    (Proxy :: Proxy ModifyInstanceCreditSpecification)

responseDescribePrincipalIdFormat :: DescribePrincipalIdFormatResponse -> TestTree
responseDescribePrincipalIdFormat =
  res
    "DescribePrincipalIdFormatResponse"
    "fixture/DescribePrincipalIdFormatResponse.proto"
    ec2
    (Proxy :: Proxy DescribePrincipalIdFormat)

responseDescribeTransitGateways :: DescribeTransitGatewaysResponse -> TestTree
responseDescribeTransitGateways =
  res
    "DescribeTransitGatewaysResponse"
    "fixture/DescribeTransitGatewaysResponse.proto"
    ec2
    (Proxy :: Proxy DescribeTransitGateways)

responseDeleteNetworkACL :: DeleteNetworkACLResponse -> TestTree
responseDeleteNetworkACL =
  res
    "DeleteNetworkACLResponse"
    "fixture/DeleteNetworkACLResponse.proto"
    ec2
    (Proxy :: Proxy DeleteNetworkACL)

responseDisassociateTransitGatewayMulticastDomain :: DisassociateTransitGatewayMulticastDomainResponse -> TestTree
responseDisassociateTransitGatewayMulticastDomain =
  res
    "DisassociateTransitGatewayMulticastDomainResponse"
    "fixture/DisassociateTransitGatewayMulticastDomainResponse.proto"
    ec2
    (Proxy :: Proxy DisassociateTransitGatewayMulticastDomain)

responseDeleteTransitGatewayRouteTable :: DeleteTransitGatewayRouteTableResponse -> TestTree
responseDeleteTransitGatewayRouteTable =
  res
    "DeleteTransitGatewayRouteTableResponse"
    "fixture/DeleteTransitGatewayRouteTableResponse.proto"
    ec2
    (Proxy :: Proxy DeleteTransitGatewayRouteTable)

responseCreateLaunchTemplate :: CreateLaunchTemplateResponse -> TestTree
responseCreateLaunchTemplate =
  res
    "CreateLaunchTemplateResponse"
    "fixture/CreateLaunchTemplateResponse.proto"
    ec2
    (Proxy :: Proxy CreateLaunchTemplate)

responseCreateVPCEndpointConnectionNotification :: CreateVPCEndpointConnectionNotificationResponse -> TestTree
responseCreateVPCEndpointConnectionNotification =
  res
    "CreateVPCEndpointConnectionNotificationResponse"
    "fixture/CreateVPCEndpointConnectionNotificationResponse.proto"
    ec2
    (Proxy :: Proxy CreateVPCEndpointConnectionNotification)

responseDeleteNetworkInterfacePermission :: DeleteNetworkInterfacePermissionResponse -> TestTree
responseDeleteNetworkInterfacePermission =
  res
    "DeleteNetworkInterfacePermissionResponse"
    "fixture/DeleteNetworkInterfacePermissionResponse.proto"
    ec2
    (Proxy :: Proxy DeleteNetworkInterfacePermission)

responseDeleteVPNGateway :: DeleteVPNGatewayResponse -> TestTree
responseDeleteVPNGateway =
  res
    "DeleteVPNGatewayResponse"
    "fixture/DeleteVPNGatewayResponse.proto"
    ec2
    (Proxy :: Proxy DeleteVPNGateway)

responseCreateTrafficMirrorTarget :: CreateTrafficMirrorTargetResponse -> TestTree
responseCreateTrafficMirrorTarget =
  res
    "CreateTrafficMirrorTargetResponse"
    "fixture/CreateTrafficMirrorTargetResponse.proto"
    ec2
    (Proxy :: Proxy CreateTrafficMirrorTarget)

responseDescribeImportImageTasks :: DescribeImportImageTasksResponse -> TestTree
responseDescribeImportImageTasks =
  res
    "DescribeImportImageTasksResponse"
    "fixture/DescribeImportImageTasksResponse.proto"
    ec2
    (Proxy :: Proxy DescribeImportImageTasks)

responseDescribeVolumeAttribute :: DescribeVolumeAttributeResponse -> TestTree
responseDescribeVolumeAttribute =
  res
    "DescribeVolumeAttributeResponse"
    "fixture/DescribeVolumeAttributeResponse.proto"
    ec2
    (Proxy :: Proxy DescribeVolumeAttribute)

responseDescribeMovingAddresses :: DescribeMovingAddressesResponse -> TestTree
responseDescribeMovingAddresses =
  res
    "DescribeMovingAddressesResponse"
    "fixture/DescribeMovingAddressesResponse.proto"
    ec2
    (Proxy :: Proxy DescribeMovingAddresses)

responseExportTransitGatewayRoutes :: ExportTransitGatewayRoutesResponse -> TestTree
responseExportTransitGatewayRoutes =
  res
    "ExportTransitGatewayRoutesResponse"
    "fixture/ExportTransitGatewayRoutesResponse.proto"
    ec2
    (Proxy :: Proxy ExportTransitGatewayRoutes)

responseGetPasswordData :: GetPasswordDataResponse -> TestTree
responseGetPasswordData =
  res
    "GetPasswordDataResponse"
    "fixture/GetPasswordDataResponse.proto"
    ec2
    (Proxy :: Proxy GetPasswordData)

responseCreateVPC :: CreateVPCResponse -> TestTree
responseCreateVPC =
  res
    "CreateVPCResponse"
    "fixture/CreateVPCResponse.proto"
    ec2
    (Proxy :: Proxy CreateVPC)

responseModifyVPCPeeringConnectionOptions :: ModifyVPCPeeringConnectionOptionsResponse -> TestTree
responseModifyVPCPeeringConnectionOptions =
  res
    "ModifyVPCPeeringConnectionOptionsResponse"
    "fixture/ModifyVPCPeeringConnectionOptionsResponse.proto"
    ec2
    (Proxy :: Proxy ModifyVPCPeeringConnectionOptions)

responseDescribeFpgaImages :: DescribeFpgaImagesResponse -> TestTree
responseDescribeFpgaImages =
  res
    "DescribeFpgaImagesResponse"
    "fixture/DescribeFpgaImagesResponse.proto"
    ec2
    (Proxy :: Proxy DescribeFpgaImages)

responseCopySnapshot :: CopySnapshotResponse -> TestTree
responseCopySnapshot =
  res
    "CopySnapshotResponse"
    "fixture/CopySnapshotResponse.proto"
    ec2
    (Proxy :: Proxy CopySnapshot)

responseAcceptTransitGatewayPeeringAttachment :: AcceptTransitGatewayPeeringAttachmentResponse -> TestTree
responseAcceptTransitGatewayPeeringAttachment =
  res
    "AcceptTransitGatewayPeeringAttachmentResponse"
    "fixture/AcceptTransitGatewayPeeringAttachmentResponse.proto"
    ec2
    (Proxy :: Proxy AcceptTransitGatewayPeeringAttachment)

responseDisassociateAddress :: DisassociateAddressResponse -> TestTree
responseDisassociateAddress =
  res
    "DisassociateAddressResponse"
    "fixture/DisassociateAddressResponse.proto"
    ec2
    (Proxy :: Proxy DisassociateAddress)

responseModifyTrafficMirrorFilterNetworkServices :: ModifyTrafficMirrorFilterNetworkServicesResponse -> TestTree
responseModifyTrafficMirrorFilterNetworkServices =
  res
    "ModifyTrafficMirrorFilterNetworkServicesResponse"
    "fixture/ModifyTrafficMirrorFilterNetworkServicesResponse.proto"
    ec2
    (Proxy :: Proxy ModifyTrafficMirrorFilterNetworkServices)

responseDescribeEgressOnlyInternetGateways :: DescribeEgressOnlyInternetGatewaysResponse -> TestTree
responseDescribeEgressOnlyInternetGateways =
  res
    "DescribeEgressOnlyInternetGatewaysResponse"
    "fixture/DescribeEgressOnlyInternetGatewaysResponse.proto"
    ec2
    (Proxy :: Proxy DescribeEgressOnlyInternetGateways)

responseDeleteVPC :: DeleteVPCResponse -> TestTree
responseDeleteVPC =
  res
    "DeleteVPCResponse"
    "fixture/DeleteVPCResponse.proto"
    ec2
    (Proxy :: Proxy DeleteVPC)

responseCreateInstanceExportTask :: CreateInstanceExportTaskResponse -> TestTree
responseCreateInstanceExportTask =
  res
    "CreateInstanceExportTaskResponse"
    "fixture/CreateInstanceExportTaskResponse.proto"
    ec2
    (Proxy :: Proxy CreateInstanceExportTask)

responseRejectTransitGatewayVPCAttachment :: RejectTransitGatewayVPCAttachmentResponse -> TestTree
responseRejectTransitGatewayVPCAttachment =
  res
    "RejectTransitGatewayVPCAttachmentResponse"
    "fixture/RejectTransitGatewayVPCAttachmentResponse.proto"
    ec2
    (Proxy :: Proxy RejectTransitGatewayVPCAttachment)

responseDescribeTrafficMirrorSessions :: DescribeTrafficMirrorSessionsResponse -> TestTree
responseDescribeTrafficMirrorSessions =
  res
    "DescribeTrafficMirrorSessionsResponse"
    "fixture/DescribeTrafficMirrorSessionsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeTrafficMirrorSessions)

responseGetTransitGatewayRouteTableAssociations :: GetTransitGatewayRouteTableAssociationsResponse -> TestTree
responseGetTransitGatewayRouteTableAssociations =
  res
    "GetTransitGatewayRouteTableAssociationsResponse"
    "fixture/GetTransitGatewayRouteTableAssociationsResponse.proto"
    ec2
    (Proxy :: Proxy GetTransitGatewayRouteTableAssociations)

responseAssociateVPCCidrBlock :: AssociateVPCCidrBlockResponse -> TestTree
responseAssociateVPCCidrBlock =
  res
    "AssociateVPCCidrBlockResponse"
    "fixture/AssociateVPCCidrBlockResponse.proto"
    ec2
    (Proxy :: Proxy AssociateVPCCidrBlock)

responseDescribeVPCAttribute :: DescribeVPCAttributeResponse -> TestTree
responseDescribeVPCAttribute =
  res
    "DescribeVPCAttributeResponse"
    "fixture/DescribeVPCAttributeResponse.proto"
    ec2
    (Proxy :: Proxy DescribeVPCAttribute)

responseCreateVolume :: Volume -> TestTree
responseCreateVolume =
  res
    "CreateVolumeResponse"
    "fixture/CreateVolumeResponse.proto"
    ec2
    (Proxy :: Proxy CreateVolume)

responseCreateDefaultSubnet :: CreateDefaultSubnetResponse -> TestTree
responseCreateDefaultSubnet =
  res
    "CreateDefaultSubnetResponse"
    "fixture/CreateDefaultSubnetResponse.proto"
    ec2
    (Proxy :: Proxy CreateDefaultSubnet)

responseDescribeScheduledInstanceAvailability :: DescribeScheduledInstanceAvailabilityResponse -> TestTree
responseDescribeScheduledInstanceAvailability =
  res
    "DescribeScheduledInstanceAvailabilityResponse"
    "fixture/DescribeScheduledInstanceAvailabilityResponse.proto"
    ec2
    (Proxy :: Proxy DescribeScheduledInstanceAvailability)

responseDisassociateClientVPNTargetNetwork :: DisassociateClientVPNTargetNetworkResponse -> TestTree
responseDisassociateClientVPNTargetNetwork =
  res
    "DisassociateClientVPNTargetNetworkResponse"
    "fixture/DisassociateClientVPNTargetNetworkResponse.proto"
    ec2
    (Proxy :: Proxy DisassociateClientVPNTargetNetwork)

responseCreateClientVPNRoute :: CreateClientVPNRouteResponse -> TestTree
responseCreateClientVPNRoute =
  res
    "CreateClientVPNRouteResponse"
    "fixture/CreateClientVPNRouteResponse.proto"
    ec2
    (Proxy :: Proxy CreateClientVPNRoute)

responseModifyVolumeAttribute :: ModifyVolumeAttributeResponse -> TestTree
responseModifyVolumeAttribute =
  res
    "ModifyVolumeAttributeResponse"
    "fixture/ModifyVolumeAttributeResponse.proto"
    ec2
    (Proxy :: Proxy ModifyVolumeAttribute)

responseExportClientVPNClientConfiguration :: ExportClientVPNClientConfigurationResponse -> TestTree
responseExportClientVPNClientConfiguration =
  res
    "ExportClientVPNClientConfigurationResponse"
    "fixture/ExportClientVPNClientConfigurationResponse.proto"
    ec2
    (Proxy :: Proxy ExportClientVPNClientConfiguration)

responseDeleteTrafficMirrorTarget :: DeleteTrafficMirrorTargetResponse -> TestTree
responseDeleteTrafficMirrorTarget =
  res
    "DeleteTrafficMirrorTargetResponse"
    "fixture/DeleteTrafficMirrorTargetResponse.proto"
    ec2
    (Proxy :: Proxy DeleteTrafficMirrorTarget)

responseDescribeSpotDatafeedSubscription :: DescribeSpotDatafeedSubscriptionResponse -> TestTree
responseDescribeSpotDatafeedSubscription =
  res
    "DescribeSpotDatafeedSubscriptionResponse"
    "fixture/DescribeSpotDatafeedSubscriptionResponse.proto"
    ec2
    (Proxy :: Proxy DescribeSpotDatafeedSubscription)

responseDescribeLocalGatewayRouteTables :: DescribeLocalGatewayRouteTablesResponse -> TestTree
responseDescribeLocalGatewayRouteTables =
  res
    "DescribeLocalGatewayRouteTablesResponse"
    "fixture/DescribeLocalGatewayRouteTablesResponse.proto"
    ec2
    (Proxy :: Proxy DescribeLocalGatewayRouteTables)

responseDescribePrefixLists :: DescribePrefixListsResponse -> TestTree
responseDescribePrefixLists =
  res
    "DescribePrefixListsResponse"
    "fixture/DescribePrefixListsResponse.proto"
    ec2
    (Proxy :: Proxy DescribePrefixLists)

responseAssociateTransitGatewayRouteTable :: AssociateTransitGatewayRouteTableResponse -> TestTree
responseAssociateTransitGatewayRouteTable =
  res
    "AssociateTransitGatewayRouteTableResponse"
    "fixture/AssociateTransitGatewayRouteTableResponse.proto"
    ec2
    (Proxy :: Proxy AssociateTransitGatewayRouteTable)

responseDeletePlacementGroup :: DeletePlacementGroupResponse -> TestTree
responseDeletePlacementGroup =
  res
    "DeletePlacementGroupResponse"
    "fixture/DeletePlacementGroupResponse.proto"
    ec2
    (Proxy :: Proxy DeletePlacementGroup)

responseModifyTransitGateway :: ModifyTransitGatewayResponse -> TestTree
responseModifyTransitGateway =
  res
    "ModifyTransitGatewayResponse"
    "fixture/ModifyTransitGatewayResponse.proto"
    ec2
    (Proxy :: Proxy ModifyTransitGateway)

responseDeleteTransitGatewayPrefixListReference :: DeleteTransitGatewayPrefixListReferenceResponse -> TestTree
responseDeleteTransitGatewayPrefixListReference =
  res
    "DeleteTransitGatewayPrefixListReferenceResponse"
    "fixture/DeleteTransitGatewayPrefixListReferenceResponse.proto"
    ec2
    (Proxy :: Proxy DeleteTransitGatewayPrefixListReference)

responseCreateTransitGatewayMulticastDomain :: CreateTransitGatewayMulticastDomainResponse -> TestTree
responseCreateTransitGatewayMulticastDomain =
  res
    "CreateTransitGatewayMulticastDomainResponse"
    "fixture/CreateTransitGatewayMulticastDomainResponse.proto"
    ec2
    (Proxy :: Proxy CreateTransitGatewayMulticastDomain)

responseDeregisterInstanceEventNotificationAttributes :: DeregisterInstanceEventNotificationAttributesResponse -> TestTree
responseDeregisterInstanceEventNotificationAttributes =
  res
    "DeregisterInstanceEventNotificationAttributesResponse"
    "fixture/DeregisterInstanceEventNotificationAttributesResponse.proto"
    ec2
    (Proxy :: Proxy DeregisterInstanceEventNotificationAttributes)

responseRequestSpotFleet :: RequestSpotFleetResponse -> TestTree
responseRequestSpotFleet =
  res
    "RequestSpotFleetResponse"
    "fixture/RequestSpotFleetResponse.proto"
    ec2
    (Proxy :: Proxy RequestSpotFleet)

responseDeleteTransitGatewayRoute :: DeleteTransitGatewayRouteResponse -> TestTree
responseDeleteTransitGatewayRoute =
  res
    "DeleteTransitGatewayRouteResponse"
    "fixture/DeleteTransitGatewayRouteResponse.proto"
    ec2
    (Proxy :: Proxy DeleteTransitGatewayRoute)

responseDisableEBSEncryptionByDefault :: DisableEBSEncryptionByDefaultResponse -> TestTree
responseDisableEBSEncryptionByDefault =
  res
    "DisableEBSEncryptionByDefaultResponse"
    "fixture/DisableEBSEncryptionByDefaultResponse.proto"
    ec2
    (Proxy :: Proxy DisableEBSEncryptionByDefault)

responseDeregisterTransitGatewayMulticastGroupMembers :: DeregisterTransitGatewayMulticastGroupMembersResponse -> TestTree
responseDeregisterTransitGatewayMulticastGroupMembers =
  res
    "DeregisterTransitGatewayMulticastGroupMembersResponse"
    "fixture/DeregisterTransitGatewayMulticastGroupMembersResponse.proto"
    ec2
    (Proxy :: Proxy DeregisterTransitGatewayMulticastGroupMembers)

responseCreateSubnet :: CreateSubnetResponse -> TestTree
responseCreateSubnet =
  res
    "CreateSubnetResponse"
    "fixture/CreateSubnetResponse.proto"
    ec2
    (Proxy :: Proxy CreateSubnet)

responseCreateNetworkInterface :: CreateNetworkInterfaceResponse -> TestTree
responseCreateNetworkInterface =
  res
    "CreateNetworkInterfaceResponse"
    "fixture/CreateNetworkInterfaceResponse.proto"
    ec2
    (Proxy :: Proxy CreateNetworkInterface)

responseDescribeSecurityGroups :: DescribeSecurityGroupsResponse -> TestTree
responseDescribeSecurityGroups =
  res
    "DescribeSecurityGroupsResponse"
    "fixture/DescribeSecurityGroupsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeSecurityGroups)

responseGetCapacityReservationUsage :: GetCapacityReservationUsageResponse -> TestTree
responseGetCapacityReservationUsage =
  res
    "GetCapacityReservationUsageResponse"
    "fixture/GetCapacityReservationUsageResponse.proto"
    ec2
    (Proxy :: Proxy GetCapacityReservationUsage)

responseCreateTransitGatewayVPCAttachment :: CreateTransitGatewayVPCAttachmentResponse -> TestTree
responseCreateTransitGatewayVPCAttachment =
  res
    "CreateTransitGatewayVPCAttachmentResponse"
    "fixture/CreateTransitGatewayVPCAttachmentResponse.proto"
    ec2
    (Proxy :: Proxy CreateTransitGatewayVPCAttachment)

responseDescribeExportTasks :: DescribeExportTasksResponse -> TestTree
responseDescribeExportTasks =
  res
    "DescribeExportTasksResponse"
    "fixture/DescribeExportTasksResponse.proto"
    ec2
    (Proxy :: Proxy DescribeExportTasks)

responseModifySpotFleetRequest :: ModifySpotFleetRequestResponse -> TestTree
responseModifySpotFleetRequest =
  res
    "ModifySpotFleetRequestResponse"
    "fixture/ModifySpotFleetRequestResponse.proto"
    ec2
    (Proxy :: Proxy ModifySpotFleetRequest)

responseDetachVPNGateway :: DetachVPNGatewayResponse -> TestTree
responseDetachVPNGateway =
  res
    "DetachVPNGatewayResponse"
    "fixture/DetachVPNGatewayResponse.proto"
    ec2
    (Proxy :: Proxy DetachVPNGateway)

responseModifyManagedPrefixList :: ModifyManagedPrefixListResponse -> TestTree
responseModifyManagedPrefixList =
  res
    "ModifyManagedPrefixListResponse"
    "fixture/ModifyManagedPrefixListResponse.proto"
    ec2
    (Proxy :: Proxy ModifyManagedPrefixList)

responseGetHostReservationPurchasePreview :: GetHostReservationPurchasePreviewResponse -> TestTree
responseGetHostReservationPurchasePreview =
  res
    "GetHostReservationPurchasePreviewResponse"
    "fixture/GetHostReservationPurchasePreviewResponse.proto"
    ec2
    (Proxy :: Proxy GetHostReservationPurchasePreview)

responseEnableVolumeIO :: EnableVolumeIOResponse -> TestTree
responseEnableVolumeIO =
  res
    "EnableVolumeIOResponse"
    "fixture/EnableVolumeIOResponse.proto"
    ec2
    (Proxy :: Proxy EnableVolumeIO)

responseDescribeInstances :: DescribeInstancesResponse -> TestTree
responseDescribeInstances =
  res
    "DescribeInstancesResponse"
    "fixture/DescribeInstancesResponse.proto"
    ec2
    (Proxy :: Proxy DescribeInstances)

responseCreateNatGateway :: CreateNatGatewayResponse -> TestTree
responseCreateNatGateway =
  res
    "CreateNatGatewayResponse"
    "fixture/CreateNatGatewayResponse.proto"
    ec2
    (Proxy :: Proxy CreateNatGateway)

responseDescribeLocalGatewayVirtualInterfaces :: DescribeLocalGatewayVirtualInterfacesResponse -> TestTree
responseDescribeLocalGatewayVirtualInterfaces =
  res
    "DescribeLocalGatewayVirtualInterfacesResponse"
    "fixture/DescribeLocalGatewayVirtualInterfacesResponse.proto"
    ec2
    (Proxy :: Proxy DescribeLocalGatewayVirtualInterfaces)

responseDescribeVPCPeeringConnections :: DescribeVPCPeeringConnectionsResponse -> TestTree
responseDescribeVPCPeeringConnections =
  res
    "DescribeVPCPeeringConnectionsResponse"
    "fixture/DescribeVPCPeeringConnectionsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeVPCPeeringConnections)

responseCancelExportTask :: CancelExportTaskResponse -> TestTree
responseCancelExportTask =
  res
    "CancelExportTaskResponse"
    "fixture/CancelExportTaskResponse.proto"
    ec2
    (Proxy :: Proxy CancelExportTask)

responseCreateVPCEndpointServiceConfiguration :: CreateVPCEndpointServiceConfigurationResponse -> TestTree
responseCreateVPCEndpointServiceConfiguration =
  res
    "CreateVPCEndpointServiceConfigurationResponse"
    "fixture/CreateVPCEndpointServiceConfigurationResponse.proto"
    ec2
    (Proxy :: Proxy CreateVPCEndpointServiceConfiguration)

responseCreateDefaultVPC :: CreateDefaultVPCResponse -> TestTree
responseCreateDefaultVPC =
  res
    "CreateDefaultVPCResponse"
    "fixture/CreateDefaultVPCResponse.proto"
    ec2
    (Proxy :: Proxy CreateDefaultVPC)

responseDisassociateVPCCidrBlock :: DisassociateVPCCidrBlockResponse -> TestTree
responseDisassociateVPCCidrBlock =
  res
    "DisassociateVPCCidrBlockResponse"
    "fixture/DisassociateVPCCidrBlockResponse.proto"
    ec2
    (Proxy :: Proxy DisassociateVPCCidrBlock)

responseDescribeTrafficMirrorFilters :: DescribeTrafficMirrorFiltersResponse -> TestTree
responseDescribeTrafficMirrorFilters =
  res
    "DescribeTrafficMirrorFiltersResponse"
    "fixture/DescribeTrafficMirrorFiltersResponse.proto"
    ec2
    (Proxy :: Proxy DescribeTrafficMirrorFilters)

responseDescribeFastSnapshotRestores :: DescribeFastSnapshotRestoresResponse -> TestTree
responseDescribeFastSnapshotRestores =
  res
    "DescribeFastSnapshotRestoresResponse"
    "fixture/DescribeFastSnapshotRestoresResponse.proto"
    ec2
    (Proxy :: Proxy DescribeFastSnapshotRestores)

responseCancelCapacityReservation :: CancelCapacityReservationResponse -> TestTree
responseCancelCapacityReservation =
  res
    "CancelCapacityReservationResponse"
    "fixture/CancelCapacityReservationResponse.proto"
    ec2
    (Proxy :: Proxy CancelCapacityReservation)

responseDeleteNetworkInterface :: DeleteNetworkInterfaceResponse -> TestTree
responseDeleteNetworkInterface =
  res
    "DeleteNetworkInterfaceResponse"
    "fixture/DeleteNetworkInterfaceResponse.proto"
    ec2
    (Proxy :: Proxy DeleteNetworkInterface)

responseDisassociateTransitGatewayRouteTable :: DisassociateTransitGatewayRouteTableResponse -> TestTree
responseDisassociateTransitGatewayRouteTable =
  res
    "DisassociateTransitGatewayRouteTableResponse"
    "fixture/DisassociateTransitGatewayRouteTableResponse.proto"
    ec2
    (Proxy :: Proxy DisassociateTransitGatewayRouteTable)

responseReplaceRouteTableAssociation :: ReplaceRouteTableAssociationResponse -> TestTree
responseReplaceRouteTableAssociation =
  res
    "ReplaceRouteTableAssociationResponse"
    "fixture/ReplaceRouteTableAssociationResponse.proto"
    ec2
    (Proxy :: Proxy ReplaceRouteTableAssociation)

responseStartInstances :: StartInstancesResponse -> TestTree
responseStartInstances =
  res
    "StartInstancesResponse"
    "fixture/StartInstancesResponse.proto"
    ec2
    (Proxy :: Proxy StartInstances)

responseCreatePlacementGroup :: CreatePlacementGroupResponse -> TestTree
responseCreatePlacementGroup =
  res
    "CreatePlacementGroupResponse"
    "fixture/CreatePlacementGroupResponse.proto"
    ec2
    (Proxy :: Proxy CreatePlacementGroup)

responseDescribeInstanceEventNotificationAttributes :: DescribeInstanceEventNotificationAttributesResponse -> TestTree
responseDescribeInstanceEventNotificationAttributes =
  res
    "DescribeInstanceEventNotificationAttributesResponse"
    "fixture/DescribeInstanceEventNotificationAttributesResponse.proto"
    ec2
    (Proxy :: Proxy DescribeInstanceEventNotificationAttributes)

responseDescribeCapacityReservations :: DescribeCapacityReservationsResponse -> TestTree
responseDescribeCapacityReservations =
  res
    "DescribeCapacityReservationsResponse"
    "fixture/DescribeCapacityReservationsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeCapacityReservations)

responseModifyClientVPNEndpoint :: ModifyClientVPNEndpointResponse -> TestTree
responseModifyClientVPNEndpoint =
  res
    "ModifyClientVPNEndpointResponse"
    "fixture/ModifyClientVPNEndpointResponse.proto"
    ec2
    (Proxy :: Proxy ModifyClientVPNEndpoint)

responseModifyInstanceCapacityReservationAttributes :: ModifyInstanceCapacityReservationAttributesResponse -> TestTree
responseModifyInstanceCapacityReservationAttributes =
  res
    "ModifyInstanceCapacityReservationAttributesResponse"
    "fixture/ModifyInstanceCapacityReservationAttributesResponse.proto"
    ec2
    (Proxy :: Proxy ModifyInstanceCapacityReservationAttributes)

responseDescribeAggregateIdFormat :: DescribeAggregateIdFormatResponse -> TestTree
responseDescribeAggregateIdFormat =
  res
    "DescribeAggregateIdFormatResponse"
    "fixture/DescribeAggregateIdFormatResponse.proto"
    ec2
    (Proxy :: Proxy DescribeAggregateIdFormat)

responseDescribeSnapshots :: DescribeSnapshotsResponse -> TestTree
responseDescribeSnapshots =
  res
    "DescribeSnapshotsResponse"
    "fixture/DescribeSnapshotsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeSnapshots)

responseAssociateAddress :: AssociateAddressResponse -> TestTree
responseAssociateAddress =
  res
    "AssociateAddressResponse"
    "fixture/AssociateAddressResponse.proto"
    ec2
    (Proxy :: Proxy AssociateAddress)

responseModifyTrafficMirrorFilterRule :: ModifyTrafficMirrorFilterRuleResponse -> TestTree
responseModifyTrafficMirrorFilterRule =
  res
    "ModifyTrafficMirrorFilterRuleResponse"
    "fixture/ModifyTrafficMirrorFilterRuleResponse.proto"
    ec2
    (Proxy :: Proxy ModifyTrafficMirrorFilterRule)

responseDescribeNetworkInterfaceAttribute :: DescribeNetworkInterfaceAttributeResponse -> TestTree
responseDescribeNetworkInterfaceAttribute =
  res
    "DescribeNetworkInterfaceAttributeResponse"
    "fixture/DescribeNetworkInterfaceAttributeResponse.proto"
    ec2
    (Proxy :: Proxy DescribeNetworkInterfaceAttribute)

responseReplaceIAMInstanceProfileAssociation :: ReplaceIAMInstanceProfileAssociationResponse -> TestTree
responseReplaceIAMInstanceProfileAssociation =
  res
    "ReplaceIAMInstanceProfileAssociationResponse"
    "fixture/ReplaceIAMInstanceProfileAssociationResponse.proto"
    ec2
    (Proxy :: Proxy ReplaceIAMInstanceProfileAssociation)

responseAssociateClientVPNTargetNetwork :: AssociateClientVPNTargetNetworkResponse -> TestTree
responseAssociateClientVPNTargetNetwork =
  res
    "AssociateClientVPNTargetNetworkResponse"
    "fixture/AssociateClientVPNTargetNetworkResponse.proto"
    ec2
    (Proxy :: Proxy AssociateClientVPNTargetNetwork)

responseReleaseHosts :: ReleaseHostsResponse -> TestTree
responseReleaseHosts =
  res
    "ReleaseHostsResponse"
    "fixture/ReleaseHostsResponse.proto"
    ec2
    (Proxy :: Proxy ReleaseHosts)

responseResetNetworkInterfaceAttribute :: ResetNetworkInterfaceAttributeResponse -> TestTree
responseResetNetworkInterfaceAttribute =
  res
    "ResetNetworkInterfaceAttributeResponse"
    "fixture/ResetNetworkInterfaceAttributeResponse.proto"
    ec2
    (Proxy :: Proxy ResetNetworkInterfaceAttribute)

responseDeleteInternetGateway :: DeleteInternetGatewayResponse -> TestTree
responseDeleteInternetGateway =
  res
    "DeleteInternetGatewayResponse"
    "fixture/DeleteInternetGatewayResponse.proto"
    ec2
    (Proxy :: Proxy DeleteInternetGateway)

responseDescribeReservedInstancesListings :: DescribeReservedInstancesListingsResponse -> TestTree
responseDescribeReservedInstancesListings =
  res
    "DescribeReservedInstancesListingsResponse"
    "fixture/DescribeReservedInstancesListingsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeReservedInstancesListings)

responseCreateVPNConnection :: CreateVPNConnectionResponse -> TestTree
responseCreateVPNConnection =
  res
    "CreateVPNConnectionResponse"
    "fixture/CreateVPNConnectionResponse.proto"
    ec2
    (Proxy :: Proxy CreateVPNConnection)

responseReplaceTransitGatewayRoute :: ReplaceTransitGatewayRouteResponse -> TestTree
responseReplaceTransitGatewayRoute =
  res
    "ReplaceTransitGatewayRouteResponse"
    "fixture/ReplaceTransitGatewayRouteResponse.proto"
    ec2
    (Proxy :: Proxy ReplaceTransitGatewayRoute)

responseCreateFleet :: CreateFleetResponse -> TestTree
responseCreateFleet =
  res
    "CreateFleetResponse"
    "fixture/CreateFleetResponse.proto"
    ec2
    (Proxy :: Proxy CreateFleet)

responseDeleteNatGateway :: DeleteNatGatewayResponse -> TestTree
responseDeleteNatGateway =
  res
    "DeleteNatGatewayResponse"
    "fixture/DeleteNatGatewayResponse.proto"
    ec2
    (Proxy :: Proxy DeleteNatGateway)

responseDescribeImportSnapshotTasks :: DescribeImportSnapshotTasksResponse -> TestTree
responseDescribeImportSnapshotTasks =
  res
    "DescribeImportSnapshotTasksResponse"
    "fixture/DescribeImportSnapshotTasksResponse.proto"
    ec2
    (Proxy :: Proxy DescribeImportSnapshotTasks)

responseGetCoipPoolUsage :: GetCoipPoolUsageResponse -> TestTree
responseGetCoipPoolUsage =
  res
    "GetCoipPoolUsageResponse"
    "fixture/GetCoipPoolUsageResponse.proto"
    ec2
    (Proxy :: Proxy GetCoipPoolUsage)

responseDescribeCustomerGateways :: DescribeCustomerGatewaysResponse -> TestTree
responseDescribeCustomerGateways =
  res
    "DescribeCustomerGatewaysResponse"
    "fixture/DescribeCustomerGatewaysResponse.proto"
    ec2
    (Proxy :: Proxy DescribeCustomerGateways)

responseDeleteSubnet :: DeleteSubnetResponse -> TestTree
responseDeleteSubnet =
  res
    "DeleteSubnetResponse"
    "fixture/DeleteSubnetResponse.proto"
    ec2
    (Proxy :: Proxy DeleteSubnet)

responseCopyImage :: CopyImageResponse -> TestTree
responseCopyImage =
  res
    "CopyImageResponse"
    "fixture/CopyImageResponse.proto"
    ec2
    (Proxy :: Proxy CopyImage)

responseCreateVPCEndpoint :: CreateVPCEndpointResponse -> TestTree
responseCreateVPCEndpoint =
  res
    "CreateVPCEndpointResponse"
    "fixture/CreateVPCEndpointResponse.proto"
    ec2
    (Proxy :: Proxy CreateVPCEndpoint)

responseModifyTrafficMirrorSession :: ModifyTrafficMirrorSessionResponse -> TestTree
responseModifyTrafficMirrorSession =
  res
    "ModifyTrafficMirrorSessionResponse"
    "fixture/ModifyTrafficMirrorSessionResponse.proto"
    ec2
    (Proxy :: Proxy ModifyTrafficMirrorSession)

responseDescribeCarrierGateways :: DescribeCarrierGatewaysResponse -> TestTree
responseDescribeCarrierGateways =
  res
    "DescribeCarrierGatewaysResponse"
    "fixture/DescribeCarrierGatewaysResponse.proto"
    ec2
    (Proxy :: Proxy DescribeCarrierGateways)

responseDescribeTransitGatewayPeeringAttachments :: DescribeTransitGatewayPeeringAttachmentsResponse -> TestTree
responseDescribeTransitGatewayPeeringAttachments =
  res
    "DescribeTransitGatewayPeeringAttachmentsResponse"
    "fixture/DescribeTransitGatewayPeeringAttachmentsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeTransitGatewayPeeringAttachments)

responseDeleteQueuedReservedInstances :: DeleteQueuedReservedInstancesResponse -> TestTree
responseDeleteQueuedReservedInstances =
  res
    "DeleteQueuedReservedInstancesResponse"
    "fixture/DeleteQueuedReservedInstancesResponse.proto"
    ec2
    (Proxy :: Proxy DeleteQueuedReservedInstances)

responseDescribeTransitGatewayMulticastDomains :: DescribeTransitGatewayMulticastDomainsResponse -> TestTree
responseDescribeTransitGatewayMulticastDomains =
  res
    "DescribeTransitGatewayMulticastDomainsResponse"
    "fixture/DescribeTransitGatewayMulticastDomainsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeTransitGatewayMulticastDomains)

responseGetDefaultCreditSpecification :: GetDefaultCreditSpecificationResponse -> TestTree
responseGetDefaultCreditSpecification =
  res
    "GetDefaultCreditSpecificationResponse"
    "fixture/GetDefaultCreditSpecificationResponse.proto"
    ec2
    (Proxy :: Proxy GetDefaultCreditSpecification)

responseUnmonitorInstances :: UnmonitorInstancesResponse -> TestTree
responseUnmonitorInstances =
  res
    "UnmonitorInstancesResponse"
    "fixture/UnmonitorInstancesResponse.proto"
    ec2
    (Proxy :: Proxy UnmonitorInstances)

responseDescribeTransitGatewayVPCAttachments :: DescribeTransitGatewayVPCAttachmentsResponse -> TestTree
responseDescribeTransitGatewayVPCAttachments =
  res
    "DescribeTransitGatewayVPCAttachmentsResponse"
    "fixture/DescribeTransitGatewayVPCAttachmentsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeTransitGatewayVPCAttachments)

responseCreateSecurityGroup :: CreateSecurityGroupResponse -> TestTree
responseCreateSecurityGroup =
  res
    "CreateSecurityGroupResponse"
    "fixture/CreateSecurityGroupResponse.proto"
    ec2
    (Proxy :: Proxy CreateSecurityGroup)

responseGetEBSEncryptionByDefault :: GetEBSEncryptionByDefaultResponse -> TestTree
responseGetEBSEncryptionByDefault =
  res
    "GetEBSEncryptionByDefaultResponse"
    "fixture/GetEBSEncryptionByDefaultResponse.proto"
    ec2
    (Proxy :: Proxy GetEBSEncryptionByDefault)

responseImportVolume :: ImportVolumeResponse -> TestTree
responseImportVolume =
  res
    "ImportVolumeResponse"
    "fixture/ImportVolumeResponse.proto"
    ec2
    (Proxy :: Proxy ImportVolume)

responseDeleteCarrierGateway :: DeleteCarrierGatewayResponse -> TestTree
responseDeleteCarrierGateway =
  res
    "DeleteCarrierGatewayResponse"
    "fixture/DeleteCarrierGatewayResponse.proto"
    ec2
    (Proxy :: Proxy DeleteCarrierGateway)

responseDisableVGWRoutePropagation :: DisableVGWRoutePropagationResponse -> TestTree
responseDisableVGWRoutePropagation =
  res
    "DisableVGWRoutePropagationResponse"
    "fixture/DisableVGWRoutePropagationResponse.proto"
    ec2
    (Proxy :: Proxy DisableVGWRoutePropagation)

responseDeleteTrafficMirrorFilter :: DeleteTrafficMirrorFilterResponse -> TestTree
responseDeleteTrafficMirrorFilter =
  res
    "DeleteTrafficMirrorFilterResponse"
    "fixture/DeleteTrafficMirrorFilterResponse.proto"
    ec2
    (Proxy :: Proxy DeleteTrafficMirrorFilter)

responseModifyVPNTunnelCertificate :: ModifyVPNTunnelCertificateResponse -> TestTree
responseModifyVPNTunnelCertificate =
  res
    "ModifyVPNTunnelCertificateResponse"
    "fixture/ModifyVPNTunnelCertificateResponse.proto"
    ec2
    (Proxy :: Proxy ModifyVPNTunnelCertificate)

responseCreateSpotDatafeedSubscription :: CreateSpotDatafeedSubscriptionResponse -> TestTree
responseCreateSpotDatafeedSubscription =
  res
    "CreateSpotDatafeedSubscriptionResponse"
    "fixture/CreateSpotDatafeedSubscriptionResponse.proto"
    ec2
    (Proxy :: Proxy CreateSpotDatafeedSubscription)

responseCancelSpotInstanceRequests :: CancelSpotInstanceRequestsResponse -> TestTree
responseCancelSpotInstanceRequests =
  res
    "CancelSpotInstanceRequestsResponse"
    "fixture/CancelSpotInstanceRequestsResponse.proto"
    ec2
    (Proxy :: Proxy CancelSpotInstanceRequests)

responseCreateRoute :: CreateRouteResponse -> TestTree
responseCreateRoute =
  res
    "CreateRouteResponse"
    "fixture/CreateRouteResponse.proto"
    ec2
    (Proxy :: Proxy CreateRoute)

responseDescribeVPCEndpointServiceConfigurations :: DescribeVPCEndpointServiceConfigurationsResponse -> TestTree
responseDescribeVPCEndpointServiceConfigurations =
  res
    "DescribeVPCEndpointServiceConfigurationsResponse"
    "fixture/DescribeVPCEndpointServiceConfigurationsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeVPCEndpointServiceConfigurations)

responseDeleteSnapshot :: DeleteSnapshotResponse -> TestTree
responseDeleteSnapshot =
  res
    "DeleteSnapshotResponse"
    "fixture/DeleteSnapshotResponse.proto"
    ec2
    (Proxy :: Proxy DeleteSnapshot)

responseAssignPrivateIPAddresses :: AssignPrivateIPAddressesResponse -> TestTree
responseAssignPrivateIPAddresses =
  res
    "AssignPrivateIPAddressesResponse"
    "fixture/AssignPrivateIPAddressesResponse.proto"
    ec2
    (Proxy :: Proxy AssignPrivateIPAddresses)

responseAuthorizeClientVPNIngress :: AuthorizeClientVPNIngressResponse -> TestTree
responseAuthorizeClientVPNIngress =
  res
    "AuthorizeClientVPNIngressResponse"
    "fixture/AuthorizeClientVPNIngressResponse.proto"
    ec2
    (Proxy :: Proxy AuthorizeClientVPNIngress)

responseDeleteTransitGatewayPeeringAttachment :: DeleteTransitGatewayPeeringAttachmentResponse -> TestTree
responseDeleteTransitGatewayPeeringAttachment =
  res
    "DeleteTransitGatewayPeeringAttachmentResponse"
    "fixture/DeleteTransitGatewayPeeringAttachmentResponse.proto"
    ec2
    (Proxy :: Proxy DeleteTransitGatewayPeeringAttachment)

responseModifyInstanceAttribute :: ModifyInstanceAttributeResponse -> TestTree
responseModifyInstanceAttribute =
  res
    "ModifyInstanceAttributeResponse"
    "fixture/ModifyInstanceAttributeResponse.proto"
    ec2
    (Proxy :: Proxy ModifyInstanceAttribute)

responseDeleteCustomerGateway :: DeleteCustomerGatewayResponse -> TestTree
responseDeleteCustomerGateway =
  res
    "DeleteCustomerGatewayResponse"
    "fixture/DeleteCustomerGatewayResponse.proto"
    ec2
    (Proxy :: Proxy DeleteCustomerGateway)

responseDisassociateIAMInstanceProfile :: DisassociateIAMInstanceProfileResponse -> TestTree
responseDisassociateIAMInstanceProfile =
  res
    "DisassociateIAMInstanceProfileResponse"
    "fixture/DisassociateIAMInstanceProfileResponse.proto"
    ec2
    (Proxy :: Proxy DisassociateIAMInstanceProfile)

responseTerminateClientVPNConnections :: TerminateClientVPNConnectionsResponse -> TestTree
responseTerminateClientVPNConnections =
  res
    "TerminateClientVPNConnectionsResponse"
    "fixture/TerminateClientVPNConnectionsResponse.proto"
    ec2
    (Proxy :: Proxy TerminateClientVPNConnections)

responseDisassociateRouteTable :: DisassociateRouteTableResponse -> TestTree
responseDisassociateRouteTable =
  res
    "DisassociateRouteTableResponse"
    "fixture/DisassociateRouteTableResponse.proto"
    ec2
    (Proxy :: Proxy DisassociateRouteTable)

responseGetConsoleScreenshot :: GetConsoleScreenshotResponse -> TestTree
responseGetConsoleScreenshot =
  res
    "GetConsoleScreenshotResponse"
    "fixture/GetConsoleScreenshotResponse.proto"
    ec2
    (Proxy :: Proxy GetConsoleScreenshot)

responseResetEBSDefaultKMSKeyId :: ResetEBSDefaultKMSKeyIdResponse -> TestTree
responseResetEBSDefaultKMSKeyId =
  res
    "ResetEBSDefaultKMSKeyIdResponse"
    "fixture/ResetEBSDefaultKMSKeyIdResponse.proto"
    ec2
    (Proxy :: Proxy ResetEBSDefaultKMSKeyId)

responseAssignIPv6Addresses :: AssignIPv6AddressesResponse -> TestTree
responseAssignIPv6Addresses =
  res
    "AssignIPv6AddressesResponse"
    "fixture/AssignIPv6AddressesResponse.proto"
    ec2
    (Proxy :: Proxy AssignIPv6Addresses)

responseModifyVPNTunnelOptions :: ModifyVPNTunnelOptionsResponse -> TestTree
responseModifyVPNTunnelOptions =
  res
    "ModifyVPNTunnelOptionsResponse"
    "fixture/ModifyVPNTunnelOptionsResponse.proto"
    ec2
    (Proxy :: Proxy ModifyVPNTunnelOptions)

responseModifyEBSDefaultKMSKeyId :: ModifyEBSDefaultKMSKeyIdResponse -> TestTree
responseModifyEBSDefaultKMSKeyId =
  res
    "ModifyEBSDefaultKMSKeyIdResponse"
    "fixture/ModifyEBSDefaultKMSKeyIdResponse.proto"
    ec2
    (Proxy :: Proxy ModifyEBSDefaultKMSKeyId)

responseDeleteSpotDatafeedSubscription :: DeleteSpotDatafeedSubscriptionResponse -> TestTree
responseDeleteSpotDatafeedSubscription =
  res
    "DeleteSpotDatafeedSubscriptionResponse"
    "fixture/DeleteSpotDatafeedSubscriptionResponse.proto"
    ec2
    (Proxy :: Proxy DeleteSpotDatafeedSubscription)

responseModifyVolume :: ModifyVolumeResponse -> TestTree
responseModifyVolume =
  res
    "ModifyVolumeResponse"
    "fixture/ModifyVolumeResponse.proto"
    ec2
    (Proxy :: Proxy ModifyVolume)

responseEnableVPCClassicLink :: EnableVPCClassicLinkResponse -> TestTree
responseEnableVPCClassicLink =
  res
    "EnableVPCClassicLinkResponse"
    "fixture/EnableVPCClassicLinkResponse.proto"
    ec2
    (Proxy :: Proxy EnableVPCClassicLink)

responseDescribePlacementGroups :: DescribePlacementGroupsResponse -> TestTree
responseDescribePlacementGroups =
  res
    "DescribePlacementGroupsResponse"
    "fixture/DescribePlacementGroupsResponse.proto"
    ec2
    (Proxy :: Proxy DescribePlacementGroups)

responseProvisionByoipCidr :: ProvisionByoipCidrResponse -> TestTree
responseProvisionByoipCidr =
  res
    "ProvisionByoipCidrResponse"
    "fixture/ProvisionByoipCidrResponse.proto"
    ec2
    (Proxy :: Proxy ProvisionByoipCidr)

responseDisassociateEnclaveCertificateIAMRole :: DisassociateEnclaveCertificateIAMRoleResponse -> TestTree
responseDisassociateEnclaveCertificateIAMRole =
  res
    "DisassociateEnclaveCertificateIAMRoleResponse"
    "fixture/DisassociateEnclaveCertificateIAMRoleResponse.proto"
    ec2
    (Proxy :: Proxy DisassociateEnclaveCertificateIAMRole)

responseModifyAvailabilityZoneGroup :: ModifyAvailabilityZoneGroupResponse -> TestTree
responseModifyAvailabilityZoneGroup =
  res
    "ModifyAvailabilityZoneGroupResponse"
    "fixture/ModifyAvailabilityZoneGroupResponse.proto"
    ec2
    (Proxy :: Proxy ModifyAvailabilityZoneGroup)

responseDescribeStaleSecurityGroups :: DescribeStaleSecurityGroupsResponse -> TestTree
responseDescribeStaleSecurityGroups =
  res
    "DescribeStaleSecurityGroupsResponse"
    "fixture/DescribeStaleSecurityGroupsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeStaleSecurityGroups)

responseCreateCarrierGateway :: CreateCarrierGatewayResponse -> TestTree
responseCreateCarrierGateway =
  res
    "CreateCarrierGatewayResponse"
    "fixture/CreateCarrierGatewayResponse.proto"
    ec2
    (Proxy :: Proxy CreateCarrierGateway)

responseDescribeExportImageTasks :: DescribeExportImageTasksResponse -> TestTree
responseDescribeExportImageTasks =
  res
    "DescribeExportImageTasksResponse"
    "fixture/DescribeExportImageTasksResponse.proto"
    ec2
    (Proxy :: Proxy DescribeExportImageTasks)

responsePurchaseScheduledInstances :: PurchaseScheduledInstancesResponse -> TestTree
responsePurchaseScheduledInstances =
  res
    "PurchaseScheduledInstancesResponse"
    "fixture/PurchaseScheduledInstancesResponse.proto"
    ec2
    (Proxy :: Proxy PurchaseScheduledInstances)

responseEnableVGWRoutePropagation :: EnableVGWRoutePropagationResponse -> TestTree
responseEnableVGWRoutePropagation =
  res
    "EnableVGWRoutePropagationResponse"
    "fixture/EnableVGWRoutePropagationResponse.proto"
    ec2
    (Proxy :: Proxy EnableVGWRoutePropagation)

responseDescribeSpotFleetRequestHistory :: DescribeSpotFleetRequestHistoryResponse -> TestTree
responseDescribeSpotFleetRequestHistory =
  res
    "DescribeSpotFleetRequestHistoryResponse"
    "fixture/DescribeSpotFleetRequestHistoryResponse.proto"
    ec2
    (Proxy :: Proxy DescribeSpotFleetRequestHistory)

responseModifySnapshotAttribute :: ModifySnapshotAttributeResponse -> TestTree
responseModifySnapshotAttribute =
  res
    "ModifySnapshotAttributeResponse"
    "fixture/ModifySnapshotAttributeResponse.proto"
    ec2
    (Proxy :: Proxy ModifySnapshotAttribute)

responseDescribeIAMInstanceProfileAssociations :: DescribeIAMInstanceProfileAssociationsResponse -> TestTree
responseDescribeIAMInstanceProfileAssociations =
  res
    "DescribeIAMInstanceProfileAssociationsResponse"
    "fixture/DescribeIAMInstanceProfileAssociationsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeIAMInstanceProfileAssociations)

responseCreateSnapshot :: Snapshot -> TestTree
responseCreateSnapshot =
  res
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse.proto"
    ec2
    (Proxy :: Proxy CreateSnapshot)

responseCreateLocalGatewayRoute :: CreateLocalGatewayRouteResponse -> TestTree
responseCreateLocalGatewayRoute =
  res
    "CreateLocalGatewayRouteResponse"
    "fixture/CreateLocalGatewayRouteResponse.proto"
    ec2
    (Proxy :: Proxy CreateLocalGatewayRoute)

responseCreateNetworkACLEntry :: CreateNetworkACLEntryResponse -> TestTree
responseCreateNetworkACLEntry =
  res
    "CreateNetworkACLEntryResponse"
    "fixture/CreateNetworkACLEntryResponse.proto"
    ec2
    (Proxy :: Proxy CreateNetworkACLEntry)

responseDescribeTransitGatewayAttachments :: DescribeTransitGatewayAttachmentsResponse -> TestTree
responseDescribeTransitGatewayAttachments =
  res
    "DescribeTransitGatewayAttachmentsResponse"
    "fixture/DescribeTransitGatewayAttachmentsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeTransitGatewayAttachments)

responseCreateReservedInstancesListing :: CreateReservedInstancesListingResponse -> TestTree
responseCreateReservedInstancesListing =
  res
    "CreateReservedInstancesListingResponse"
    "fixture/CreateReservedInstancesListingResponse.proto"
    ec2
    (Proxy :: Proxy CreateReservedInstancesListing)

responseDescribeIPv6Pools :: DescribeIPv6PoolsResponse -> TestTree
responseDescribeIPv6Pools =
  res
    "DescribeIPv6PoolsResponse"
    "fixture/DescribeIPv6PoolsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeIPv6Pools)

responseAttachVPNGateway :: AttachVPNGatewayResponse -> TestTree
responseAttachVPNGateway =
  res
    "AttachVPNGatewayResponse"
    "fixture/AttachVPNGatewayResponse.proto"
    ec2
    (Proxy :: Proxy AttachVPNGateway)

responseDescribeLocalGateways :: DescribeLocalGatewaysResponse -> TestTree
responseDescribeLocalGateways =
  res
    "DescribeLocalGatewaysResponse"
    "fixture/DescribeLocalGatewaysResponse.proto"
    ec2
    (Proxy :: Proxy DescribeLocalGateways)

responseModifyVPCEndpointServicePermissions :: ModifyVPCEndpointServicePermissionsResponse -> TestTree
responseModifyVPCEndpointServicePermissions =
  res
    "ModifyVPCEndpointServicePermissionsResponse"
    "fixture/ModifyVPCEndpointServicePermissionsResponse.proto"
    ec2
    (Proxy :: Proxy ModifyVPCEndpointServicePermissions)

responseExportClientVPNClientCertificateRevocationList :: ExportClientVPNClientCertificateRevocationListResponse -> TestTree
responseExportClientVPNClientCertificateRevocationList =
  res
    "ExportClientVPNClientCertificateRevocationListResponse"
    "fixture/ExportClientVPNClientCertificateRevocationListResponse.proto"
    ec2
    (Proxy :: Proxy ExportClientVPNClientCertificateRevocationList)

responseCreateDHCPOptions :: CreateDHCPOptionsResponse -> TestTree
responseCreateDHCPOptions =
  res
    "CreateDHCPOptionsResponse"
    "fixture/CreateDHCPOptionsResponse.proto"
    ec2
    (Proxy :: Proxy CreateDHCPOptions)

responseRegisterTransitGatewayMulticastGroupSources :: RegisterTransitGatewayMulticastGroupSourcesResponse -> TestTree
responseRegisterTransitGatewayMulticastGroupSources =
  res
    "RegisterTransitGatewayMulticastGroupSourcesResponse"
    "fixture/RegisterTransitGatewayMulticastGroupSourcesResponse.proto"
    ec2
    (Proxy :: Proxy RegisterTransitGatewayMulticastGroupSources)

responseDescribeAccountAttributes :: DescribeAccountAttributesResponse -> TestTree
responseDescribeAccountAttributes =
  res
    "DescribeAccountAttributesResponse"
    "fixture/DescribeAccountAttributesResponse.proto"
    ec2
    (Proxy :: Proxy DescribeAccountAttributes)

responseGetTransitGatewayRouteTablePropagations :: GetTransitGatewayRouteTablePropagationsResponse -> TestTree
responseGetTransitGatewayRouteTablePropagations =
  res
    "GetTransitGatewayRouteTablePropagationsResponse"
    "fixture/GetTransitGatewayRouteTablePropagationsResponse.proto"
    ec2
    (Proxy :: Proxy GetTransitGatewayRouteTablePropagations)

responseModifyFpgaImageAttribute :: ModifyFpgaImageAttributeResponse -> TestTree
responseModifyFpgaImageAttribute =
  res
    "ModifyFpgaImageAttributeResponse"
    "fixture/ModifyFpgaImageAttributeResponse.proto"
    ec2
    (Proxy :: Proxy ModifyFpgaImageAttribute)

responseModifyHosts :: ModifyHostsResponse -> TestTree
responseModifyHosts =
  res
    "ModifyHostsResponse"
    "fixture/ModifyHostsResponse.proto"
    ec2
    (Proxy :: Proxy ModifyHosts)

responseRebootInstances :: RebootInstancesResponse -> TestTree
responseRebootInstances =
  res
    "RebootInstancesResponse"
    "fixture/RebootInstancesResponse.proto"
    ec2
    (Proxy :: Proxy RebootInstances)

responseModifyVPCEndpointServiceConfiguration :: ModifyVPCEndpointServiceConfigurationResponse -> TestTree
responseModifyVPCEndpointServiceConfiguration =
  res
    "ModifyVPCEndpointServiceConfigurationResponse"
    "fixture/ModifyVPCEndpointServiceConfigurationResponse.proto"
    ec2
    (Proxy :: Proxy ModifyVPCEndpointServiceConfiguration)

responseCreateTransitGateway :: CreateTransitGatewayResponse -> TestTree
responseCreateTransitGateway =
  res
    "CreateTransitGatewayResponse"
    "fixture/CreateTransitGatewayResponse.proto"
    ec2
    (Proxy :: Proxy CreateTransitGateway)

responseUnassignIPv6Addresses :: UnassignIPv6AddressesResponse -> TestTree
responseUnassignIPv6Addresses =
  res
    "UnassignIPv6AddressesResponse"
    "fixture/UnassignIPv6AddressesResponse.proto"
    ec2
    (Proxy :: Proxy UnassignIPv6Addresses)

responseDeleteTrafficMirrorSession :: DeleteTrafficMirrorSessionResponse -> TestTree
responseDeleteTrafficMirrorSession =
  res
    "DeleteTrafficMirrorSessionResponse"
    "fixture/DeleteTrafficMirrorSessionResponse.proto"
    ec2
    (Proxy :: Proxy DeleteTrafficMirrorSession)

responseCreateManagedPrefixList :: CreateManagedPrefixListResponse -> TestTree
responseCreateManagedPrefixList =
  res
    "CreateManagedPrefixListResponse"
    "fixture/CreateManagedPrefixListResponse.proto"
    ec2
    (Proxy :: Proxy CreateManagedPrefixList)

responseAssociateIAMInstanceProfile :: AssociateIAMInstanceProfileResponse -> TestTree
responseAssociateIAMInstanceProfile =
  res
    "AssociateIAMInstanceProfileResponse"
    "fixture/AssociateIAMInstanceProfileResponse.proto"
    ec2
    (Proxy :: Proxy AssociateIAMInstanceProfile)

responseModifyDefaultCreditSpecification :: ModifyDefaultCreditSpecificationResponse -> TestTree
responseModifyDefaultCreditSpecification =
  res
    "ModifyDefaultCreditSpecificationResponse"
    "fixture/ModifyDefaultCreditSpecificationResponse.proto"
    ec2
    (Proxy :: Proxy ModifyDefaultCreditSpecification)

responseDeleteEgressOnlyInternetGateway :: DeleteEgressOnlyInternetGatewayResponse -> TestTree
responseDeleteEgressOnlyInternetGateway =
  res
    "DeleteEgressOnlyInternetGatewayResponse"
    "fixture/DeleteEgressOnlyInternetGatewayResponse.proto"
    ec2
    (Proxy :: Proxy DeleteEgressOnlyInternetGateway)

responsePurchaseHostReservation :: PurchaseHostReservationResponse -> TestTree
responsePurchaseHostReservation =
  res
    "PurchaseHostReservationResponse"
    "fixture/PurchaseHostReservationResponse.proto"
    ec2
    (Proxy :: Proxy PurchaseHostReservation)

responseModifyTransitGatewayVPCAttachment :: ModifyTransitGatewayVPCAttachmentResponse -> TestTree
responseModifyTransitGatewayVPCAttachment =
  res
    "ModifyTransitGatewayVPCAttachmentResponse"
    "fixture/ModifyTransitGatewayVPCAttachmentResponse.proto"
    ec2
    (Proxy :: Proxy ModifyTransitGatewayVPCAttachment)

responseCreateImage :: CreateImageResponse -> TestTree
responseCreateImage =
  res
    "CreateImageResponse"
    "fixture/CreateImageResponse.proto"
    ec2
    (Proxy :: Proxy CreateImage)

responseDescribeClassicLinkInstances :: DescribeClassicLinkInstancesResponse -> TestTree
responseDescribeClassicLinkInstances =
  res
    "DescribeClassicLinkInstancesResponse"
    "fixture/DescribeClassicLinkInstancesResponse.proto"
    ec2
    (Proxy :: Proxy DescribeClassicLinkInstances)

responseTerminateInstances :: TerminateInstancesResponse -> TestTree
responseTerminateInstances =
  res
    "TerminateInstancesResponse"
    "fixture/TerminateInstancesResponse.proto"
    ec2
    (Proxy :: Proxy TerminateInstances)

responseGetTransitGatewayPrefixListReferences :: GetTransitGatewayPrefixListReferencesResponse -> TestTree
responseGetTransitGatewayPrefixListReferences =
  res
    "GetTransitGatewayPrefixListReferencesResponse"
    "fixture/GetTransitGatewayPrefixListReferencesResponse.proto"
    ec2
    (Proxy :: Proxy GetTransitGatewayPrefixListReferences)

responseDescribeKeyPairs :: DescribeKeyPairsResponse -> TestTree
responseDescribeKeyPairs =
  res
    "DescribeKeyPairsResponse"
    "fixture/DescribeKeyPairsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeKeyPairs)

responseDisableFastSnapshotRestores :: DisableFastSnapshotRestoresResponse -> TestTree
responseDisableFastSnapshotRestores =
  res
    "DisableFastSnapshotRestoresResponse"
    "fixture/DisableFastSnapshotRestoresResponse.proto"
    ec2
    (Proxy :: Proxy DisableFastSnapshotRestores)

responseDescribeLaunchTemplates :: DescribeLaunchTemplatesResponse -> TestTree
responseDescribeLaunchTemplates =
  res
    "DescribeLaunchTemplatesResponse"
    "fixture/DescribeLaunchTemplatesResponse.proto"
    ec2
    (Proxy :: Proxy DescribeLaunchTemplates)

responseCreateVPNConnectionRoute :: CreateVPNConnectionRouteResponse -> TestTree
responseCreateVPNConnectionRoute =
  res
    "CreateVPNConnectionRouteResponse"
    "fixture/CreateVPNConnectionRouteResponse.proto"
    ec2
    (Proxy :: Proxy CreateVPNConnectionRoute)

responseAssociateRouteTable :: AssociateRouteTableResponse -> TestTree
responseAssociateRouteTable =
  res
    "AssociateRouteTableResponse"
    "fixture/AssociateRouteTableResponse.proto"
    ec2
    (Proxy :: Proxy AssociateRouteTable)

responseDescribeVPNGateways :: DescribeVPNGatewaysResponse -> TestTree
responseDescribeVPNGateways =
  res
    "DescribeVPNGatewaysResponse"
    "fixture/DescribeVPNGatewaysResponse.proto"
    ec2
    (Proxy :: Proxy DescribeVPNGateways)

responseModifyVPNConnectionOptions :: ModifyVPNConnectionOptionsResponse -> TestTree
responseModifyVPNConnectionOptions =
  res
    "ModifyVPNConnectionOptionsResponse"
    "fixture/ModifyVPNConnectionOptionsResponse.proto"
    ec2
    (Proxy :: Proxy ModifyVPNConnectionOptions)

responseGetConsoleOutput :: GetConsoleOutputResponse -> TestTree
responseGetConsoleOutput =
  res
    "GetConsoleOutputResponse"
    "fixture/GetConsoleOutputResponse.proto"
    ec2
    (Proxy :: Proxy GetConsoleOutput)

responseDescribeHosts :: DescribeHostsResponse -> TestTree
responseDescribeHosts =
  res
    "DescribeHostsResponse"
    "fixture/DescribeHostsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeHosts)

responseDescribeImageAttribute :: DescribeImageAttributeResponse -> TestTree
responseDescribeImageAttribute =
  res
    "DescribeImageAttributeResponse"
    "fixture/DescribeImageAttributeResponse.proto"
    ec2
    (Proxy :: Proxy DescribeImageAttribute)

responseModifyIdFormat :: ModifyIdFormatResponse -> TestTree
responseModifyIdFormat =
  res
    "ModifyIdFormatResponse"
    "fixture/ModifyIdFormatResponse.proto"
    ec2
    (Proxy :: Proxy ModifyIdFormat)

responseRegisterTransitGatewayMulticastGroupMembers :: RegisterTransitGatewayMulticastGroupMembersResponse -> TestTree
responseRegisterTransitGatewayMulticastGroupMembers =
  res
    "RegisterTransitGatewayMulticastGroupMembersResponse"
    "fixture/RegisterTransitGatewayMulticastGroupMembersResponse.proto"
    ec2
    (Proxy :: Proxy RegisterTransitGatewayMulticastGroupMembers)

responseDeleteManagedPrefixList :: DeleteManagedPrefixListResponse -> TestTree
responseDeleteManagedPrefixList =
  res
    "DeleteManagedPrefixListResponse"
    "fixture/DeleteManagedPrefixListResponse.proto"
    ec2
    (Proxy :: Proxy DeleteManagedPrefixList)

responseDeleteRouteTable :: DeleteRouteTableResponse -> TestTree
responseDeleteRouteTable =
  res
    "DeleteRouteTableResponse"
    "fixture/DeleteRouteTableResponse.proto"
    ec2
    (Proxy :: Proxy DeleteRouteTable)

responseResetImageAttribute :: ResetImageAttributeResponse -> TestTree
responseResetImageAttribute =
  res
    "ResetImageAttributeResponse"
    "fixture/ResetImageAttributeResponse.proto"
    ec2
    (Proxy :: Proxy ResetImageAttribute)

responseModifyTransitGatewayPrefixListReference :: ModifyTransitGatewayPrefixListReferenceResponse -> TestTree
responseModifyTransitGatewayPrefixListReference =
  res
    "ModifyTransitGatewayPrefixListReferenceResponse"
    "fixture/ModifyTransitGatewayPrefixListReferenceResponse.proto"
    ec2
    (Proxy :: Proxy ModifyTransitGatewayPrefixListReference)

responseDescribeTransitGatewayRouteTables :: DescribeTransitGatewayRouteTablesResponse -> TestTree
responseDescribeTransitGatewayRouteTables =
  res
    "DescribeTransitGatewayRouteTablesResponse"
    "fixture/DescribeTransitGatewayRouteTablesResponse.proto"
    ec2
    (Proxy :: Proxy DescribeTransitGatewayRouteTables)

responseCreateEgressOnlyInternetGateway :: CreateEgressOnlyInternetGatewayResponse -> TestTree
responseCreateEgressOnlyInternetGateway =
  res
    "CreateEgressOnlyInternetGatewayResponse"
    "fixture/CreateEgressOnlyInternetGatewayResponse.proto"
    ec2
    (Proxy :: Proxy CreateEgressOnlyInternetGateway)

responseDescribeReservedInstancesModifications :: DescribeReservedInstancesModificationsResponse -> TestTree
responseDescribeReservedInstancesModifications =
  res
    "DescribeReservedInstancesModificationsResponse"
    "fixture/DescribeReservedInstancesModificationsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeReservedInstancesModifications)

responseDescribeSpotInstanceRequests :: DescribeSpotInstanceRequestsResponse -> TestTree
responseDescribeSpotInstanceRequests =
  res
    "DescribeSpotInstanceRequestsResponse"
    "fixture/DescribeSpotInstanceRequestsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeSpotInstanceRequests)

responseRevokeClientVPNIngress :: RevokeClientVPNIngressResponse -> TestTree
responseRevokeClientVPNIngress =
  res
    "RevokeClientVPNIngressResponse"
    "fixture/RevokeClientVPNIngressResponse.proto"
    ec2
    (Proxy :: Proxy RevokeClientVPNIngress)

responseUnassignPrivateIPAddresses :: UnassignPrivateIPAddressesResponse -> TestTree
responseUnassignPrivateIPAddresses =
  res
    "UnassignPrivateIPAddressesResponse"
    "fixture/UnassignPrivateIPAddressesResponse.proto"
    ec2
    (Proxy :: Proxy UnassignPrivateIPAddresses)

responseDescribeNetworkInterfacePermissions :: DescribeNetworkInterfacePermissionsResponse -> TestTree
responseDescribeNetworkInterfacePermissions =
  res
    "DescribeNetworkInterfacePermissionsResponse"
    "fixture/DescribeNetworkInterfacePermissionsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeNetworkInterfacePermissions)

responseEnableFastSnapshotRestores :: EnableFastSnapshotRestoresResponse -> TestTree
responseEnableFastSnapshotRestores =
  res
    "EnableFastSnapshotRestoresResponse"
    "fixture/EnableFastSnapshotRestoresResponse.proto"
    ec2
    (Proxy :: Proxy EnableFastSnapshotRestores)

responseDescribeVPCEndpointServicePermissions :: DescribeVPCEndpointServicePermissionsResponse -> TestTree
responseDescribeVPCEndpointServicePermissions =
  res
    "DescribeVPCEndpointServicePermissionsResponse"
    "fixture/DescribeVPCEndpointServicePermissionsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeVPCEndpointServicePermissions)

responseDeleteDHCPOptions :: DeleteDHCPOptionsResponse -> TestTree
responseDeleteDHCPOptions =
  res
    "DeleteDHCPOptionsResponse"
    "fixture/DeleteDHCPOptionsResponse.proto"
    ec2
    (Proxy :: Proxy DeleteDHCPOptions)

responseRegisterInstanceEventNotificationAttributes :: RegisterInstanceEventNotificationAttributesResponse -> TestTree
responseRegisterInstanceEventNotificationAttributes =
  res
    "RegisterInstanceEventNotificationAttributesResponse"
    "fixture/RegisterInstanceEventNotificationAttributesResponse.proto"
    ec2
    (Proxy :: Proxy RegisterInstanceEventNotificationAttributes)

responseDescribeNetworkACLs :: DescribeNetworkACLsResponse -> TestTree
responseDescribeNetworkACLs =
  res
    "DescribeNetworkACLsResponse"
    "fixture/DescribeNetworkACLsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeNetworkACLs)

responseCancelImportTask :: CancelImportTaskResponse -> TestTree
responseCancelImportTask =
  res
    "CancelImportTaskResponse"
    "fixture/CancelImportTaskResponse.proto"
    ec2
    (Proxy :: Proxy CancelImportTask)

responseDetachClassicLinkVPC :: DetachClassicLinkVPCResponse -> TestTree
responseDetachClassicLinkVPC =
  res
    "DetachClassicLinkVPCResponse"
    "fixture/DetachClassicLinkVPCResponse.proto"
    ec2
    (Proxy :: Proxy DetachClassicLinkVPC)

responseDescribeRegions :: DescribeRegionsResponse -> TestTree
responseDescribeRegions =
  res
    "DescribeRegionsResponse"
    "fixture/DescribeRegionsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeRegions)

responseMonitorInstances :: MonitorInstancesResponse -> TestTree
responseMonitorInstances =
  res
    "MonitorInstancesResponse"
    "fixture/MonitorInstancesResponse.proto"
    ec2
    (Proxy :: Proxy MonitorInstances)

responseSearchLocalGatewayRoutes :: SearchLocalGatewayRoutesResponse -> TestTree
responseSearchLocalGatewayRoutes =
  res
    "SearchLocalGatewayRoutesResponse"
    "fixture/SearchLocalGatewayRoutesResponse.proto"
    ec2
    (Proxy :: Proxy SearchLocalGatewayRoutes)

responseDeleteClientVPNRoute :: DeleteClientVPNRouteResponse -> TestTree
responseDeleteClientVPNRoute =
  res
    "DeleteClientVPNRouteResponse"
    "fixture/DeleteClientVPNRouteResponse.proto"
    ec2
    (Proxy :: Proxy DeleteClientVPNRoute)

responseAcceptVPCPeeringConnection :: AcceptVPCPeeringConnectionResponse -> TestTree
responseAcceptVPCPeeringConnection =
  res
    "AcceptVPCPeeringConnectionResponse"
    "fixture/AcceptVPCPeeringConnectionResponse.proto"
    ec2
    (Proxy :: Proxy AcceptVPCPeeringConnection)

responseImportSnapshot :: ImportSnapshotResponse -> TestTree
responseImportSnapshot =
  res
    "ImportSnapshotResponse"
    "fixture/ImportSnapshotResponse.proto"
    ec2
    (Proxy :: Proxy ImportSnapshot)

responseDescribeVolumeStatus :: DescribeVolumeStatusResponse -> TestTree
responseDescribeVolumeStatus =
  res
    "DescribeVolumeStatusResponse"
    "fixture/DescribeVolumeStatusResponse.proto"
    ec2
    (Proxy :: Proxy DescribeVolumeStatus)

responseDescribeRouteTables :: DescribeRouteTablesResponse -> TestTree
responseDescribeRouteTables =
  res
    "DescribeRouteTablesResponse"
    "fixture/DescribeRouteTablesResponse.proto"
    ec2
    (Proxy :: Proxy DescribeRouteTables)

responseDescribeAvailabilityZones :: DescribeAvailabilityZonesResponse -> TestTree
responseDescribeAvailabilityZones =
  res
    "DescribeAvailabilityZonesResponse"
    "fixture/DescribeAvailabilityZonesResponse.proto"
    ec2
    (Proxy :: Proxy DescribeAvailabilityZones)

responseModifyVPCAttribute :: ModifyVPCAttributeResponse -> TestTree
responseModifyVPCAttribute =
  res
    "ModifyVPCAttributeResponse"
    "fixture/ModifyVPCAttributeResponse.proto"
    ec2
    (Proxy :: Proxy ModifyVPCAttribute)

responseDescribeClientVPNConnections :: DescribeClientVPNConnectionsResponse -> TestTree
responseDescribeClientVPNConnections =
  res
    "DescribeClientVPNConnectionsResponse"
    "fixture/DescribeClientVPNConnectionsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeClientVPNConnections)

responseDescribeFleetHistory :: DescribeFleetHistoryResponse -> TestTree
responseDescribeFleetHistory =
  res
    "DescribeFleetHistoryResponse"
    "fixture/DescribeFleetHistoryResponse.proto"
    ec2
    (Proxy :: Proxy DescribeFleetHistory)

responseDescribeImages :: DescribeImagesResponse -> TestTree
responseDescribeImages =
  res
    "DescribeImagesResponse"
    "fixture/DescribeImagesResponse.proto"
    ec2
    (Proxy :: Proxy DescribeImages)

responseDescribeElasticGpus :: DescribeElasticGpusResponse -> TestTree
responseDescribeElasticGpus =
  res
    "DescribeElasticGpusResponse"
    "fixture/DescribeElasticGpusResponse.proto"
    ec2
    (Proxy :: Proxy DescribeElasticGpus)

responseRestoreAddressToClassic :: RestoreAddressToClassicResponse -> TestTree
responseRestoreAddressToClassic =
  res
    "RestoreAddressToClassicResponse"
    "fixture/RestoreAddressToClassicResponse.proto"
    ec2
    (Proxy :: Proxy RestoreAddressToClassic)

responseDescribeManagedPrefixLists :: DescribeManagedPrefixListsResponse -> TestTree
responseDescribeManagedPrefixLists =
  res
    "DescribeManagedPrefixListsResponse"
    "fixture/DescribeManagedPrefixListsResponse.proto"
    ec2
    (Proxy :: Proxy DescribeManagedPrefixLists)

responseCreateKeyPair :: CreateKeyPairResponse -> TestTree
responseCreateKeyPair =
  res
    "CreateKeyPairResponse"
    "fixture/CreateKeyPairResponse.proto"
    ec2
    (Proxy :: Proxy CreateKeyPair)

responseGetReservedInstancesExchangeQuote :: GetReservedInstancesExchangeQuoteResponse -> TestTree
responseGetReservedInstancesExchangeQuote =
  res
    "GetReservedInstancesExchangeQuoteResponse"
    "fixture/GetReservedInstancesExchangeQuoteResponse.proto"
    ec2
    (Proxy :: Proxy GetReservedInstancesExchangeQuote)

responseDeleteVolume :: DeleteVolumeResponse -> TestTree
responseDeleteVolume =
  res
    "DeleteVolumeResponse"
    "fixture/DeleteVolumeResponse.proto"
    ec2
    (Proxy :: Proxy DeleteVolume)

responseDeprovisionByoipCidr :: DeprovisionByoipCidrResponse -> TestTree
responseDeprovisionByoipCidr =
  res
    "DeprovisionByoipCidrResponse"
    "fixture/DeprovisionByoipCidrResponse.proto"
    ec2
    (Proxy :: Proxy DeprovisionByoipCidr)

responseDeleteVPCEndpointServiceConfigurations :: DeleteVPCEndpointServiceConfigurationsResponse -> TestTree
responseDeleteVPCEndpointServiceConfigurations =
  res
    "DeleteVPCEndpointServiceConfigurationsResponse"
    "fixture/DeleteVPCEndpointServiceConfigurationsResponse.proto"
    ec2
    (Proxy :: Proxy DeleteVPCEndpointServiceConfigurations)

responseDescribeSpotFleetInstances :: DescribeSpotFleetInstancesResponse -> TestTree
responseDescribeSpotFleetInstances =
  res
    "DescribeSpotFleetInstancesResponse"
    "fixture/DescribeSpotFleetInstancesResponse.proto"
    ec2
    (Proxy :: Proxy DescribeSpotFleetInstances)
